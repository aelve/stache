{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.Plus.RenderSpec
  ( main
  , spec )
where

import Data.Aeson (object, KeyValue (..), Value (..))
import Data.Text (Text)
import Test.Hspec
import Text.Megaparsec
import Text.Mustache.Plus.Render
import Text.Mustache.Plus.Type
import qualified Data.Map as M

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "renderMustache" $ do
  let r ns value =
        let template = Template "test" (M.singleton "test" ns)
        in renderMustache mempty template value
      key = Key . pure
  it "leaves text block “as is”" $
    r [TextBlock "a text block"] Null
      `shouldBe` ("a text block", [])
  it "renders escaped variables correctly" $
    r [EscapedVar (key "foo")]
      (object ["foo" .= ("<html>&\"something\"</html>" :: Text)])
      `shouldBe` ("&lt;html&gt;&amp;&quot;something&quot;&lt;/html&gt;", [])
  it "renders unescaped variables “as is”" $
    r [UnescapedVar (key "foo")]
      (object ["foo" .= ("<html>&\"something\"</html>" :: Text)])
      `shouldBe` ("<html>&\"something\"</html>", [])
  context "when rendering a section" $ do
    let nodes = [Section (key "foo") [UnescapedVar (key "bar"), TextBlock "*"]]
    context "when the key is not present" $
      it "renders nothing" $
        r nodes (object []) `shouldBe` ("", [])
    context "when the key is present" $ do
      context "when the key is a “false” value" $ do
        it "skips the Null value" $
          r nodes (object ["foo" .= Null]) `shouldBe` ("", [])
        it "skips false Boolean" $
          r nodes (object ["foo" .= False]) `shouldBe` ("", [])
        it "skips empty list" $
          r nodes (object ["foo" .= ([] :: [Text])]) `shouldBe` ("", [])
        it "skips empty object" $
          r nodes (object ["foo" .= object []]) `shouldBe` ("", [])
        it "skips empty string" $
          r nodes (object ["foo" .= ("" :: Text)]) `shouldBe` ("", [])
      context "when the key is a Boolean true" $
        it "renders the section without interpolation" $
          r [Section (key "foo") [TextBlock "brr"]]
            (object ["foo" .= object ["bar" .= True]])
            `shouldBe` ("brr", [])
      context "when the key is an object" $
        it "uses it to render section once" $
          r nodes (object ["foo" .= object ["bar" .= ("huh?" :: Text)]])
            `shouldBe` ("huh?*", [])
      context "when the key is a singleton list" $
        it "uses it to render section once" $
          r nodes (object ["foo" .= object ["bar" .= ("huh!" :: Text)]])
            `shouldBe` ("huh!*", [])
      context "when the key is a list of Boolean trues" $
        it "renders the section as many times as there are elements" $
          r [Section (key "foo") [TextBlock "brr"]]
            (object ["foo" .= [True, True]])
            `shouldBe` ("brrbrr", [])
      context "when the key is a list of objects" $
        it "renders the section many times changing context" $
          r nodes (object ["foo" .= [object ["bar" .= x] | x <- [1..4] :: [Int]]])
            `shouldBe` ("1*2*3*4*", [])
      context "when the key is a number" $ do
        it "renders the section" $
          r [Section (key "foo") [TextBlock "brr"]]
            (object ["foo" .= (5 :: Int)])
            `shouldBe` ("brr", [])
        it "uses the key as context" $
          r [Section (key "foo") [EscapedVar (Key [])]]
            (object ["foo" .= (5 :: Int)])
            `shouldBe` ("5", [])
      context "when the key is a non-empty string" $ do
        it "renders the section" $
          r [Section (key "foo") [TextBlock "brr"]]
            (object ["foo" .= ("x" :: Text)])
            `shouldBe` ("brr", [])
        it "uses the key as context" $
          r [Section (key "foo") [EscapedVar (Key [])]]
            (object ["foo" .= ("x" :: Text)])
            `shouldBe` ("x", [])
    context "when the key is composite" $ do
      it "looks up the same key when used in the variable" $
        r [Section (Key ["foo", "bar"]) [EscapedVar (Key ["foo", "bar"])]]
          (object ["foo" .= object [
                      "bar" .= ("x" :: Text)]])
          `shouldBe` ("x", [])
      it "uses the key as context for an empty key" $
        r [Section (Key ["foo", "bar"]) [EscapedVar (Key [])]]
          (object ["foo" .= object [
                      "bar" .= ("x" :: Text)]])
          `shouldBe` ("x", [])
      it "uses the key as context for a non-empty key" $
        r [Section (Key ["foo", "bar"]) [EscapedVar (key "baz")]]
          (object ["foo" .= object [
                      "bar" .= object [
                          "baz" .= ("x" :: Text)]]])
          `shouldBe` ("x", [])
      it "renders a nested section using a composite key" $
        r [Section (Key ["foo", "bar"]) [
              Section (Key ["baz", "quix"]) [
                  EscapedVar (Key ["foo", "bar", "baz", "quix", "blah"])]]]
          (object ["foo" .= object [
                      "bar" .= object [
                          "baz" .= object [
                              "quix" .= object [
                                  "blah" .= ("x" :: Text)]]]]])
          `shouldBe` ("x", [])
      it "doesn't find foo.bar when looking up bar in a foo.bar section" $
        r [Section (Key ["foo", "bar"]) [
              EscapedVar (key "bar") ]]
          (object ["foo" .= object [
                      "bar" .= ("x" :: Text)]])
          `shouldBe` ("", ["missing key: bar"])
  context "when rendering an inverted section" $ do
    let nodes = [InvertedSection (key "foo") [TextBlock "Here!"]]
    context "when the key is not present" $
      it "renders the inverse section" $
        r nodes (object []) `shouldBe` ("Here!", [])
    context "when the key is present" $ do
      context "when the key is a “false” value" $ do
        it "renders with Null value" $
          r nodes (object ["foo" .= Null])
            `shouldBe` ("Here!", [])
        it "renders with false Boolean" $
          r nodes (object ["foo" .= False])
            `shouldBe` ("Here!", [])
        it "renders with empty list" $
          r nodes (object ["foo" .= ([] :: [Text])])
            `shouldBe` ("Here!", [])
        it "renders with empty object" $
          r nodes (object ["foo" .= object []])
            `shouldBe` ("Here!", [])
      context "when the key is a “true” value" $ do
        it "skips true Boolean" $
          r nodes (object ["foo" .= True])
            `shouldBe` ("", [])
        it "skips non-empty object" $
          r nodes (object ["foo" .= object ["bar" .= True]])
            `shouldBe` ("", [])
        it "skips non-empty list" $
          r nodes (object ["foo" .= [True]])
            `shouldBe` ("", [])
  context "when rendering a partial" $ do
    let nodes = [ Partial "partial" [] (Just $ unsafePos 4)
                , TextBlock "*" ]
    it "skips missing partial" $
      fst (r nodes Null) `shouldBe` "   *"
    it "renders partial correctly" $
      let template = Template "test" $
            M.fromList [ ("test", nodes)
                       , ("partial", [TextBlock "one\ntwo\nthree"]) ]
      in renderMustache mempty template Null `shouldBe`
           ("   one\n   two\n   three*", [])
    context "(+) which has arguments" $ do
      let outer = [Partial "partial"
                           [("foo", ArgValue (String "text"))]
                           Nothing]
          inner = [TextBlock ">> ", EscapedVar (key "foo")]
      let template = Template "test" $
            M.fromList [ ("test", outer)
                       , ("partial", inner) ]
      it "passes arguments to partials" $
        renderMustache mempty template Null `shouldBe`
          (">> text", [])
      it "gives arguments precedence over context" $ do
        let env = object ["foo" .= ("bar" :: Text)]
        renderMustache mempty template env `shouldBe`
          (">> text", [])
  context "(+) when assigning a variable" $ do
    let funcs = M.fromList [("head", head . (++ [Null]))]
    it "works with functions" $ do
      let nodes = [ Assign "foo" (Right ("head", [ArgValue (String "foo!")]))
                  , EscapedVar (key "foo")
                  , Assign "foo" (Right ("head", [ArgValue (String "bar!")]))
                  , EscapedVar (key "foo") ]
          template = Template "test" $ M.fromList [("test", nodes)]
      renderMustache funcs template Null `shouldBe`
        ("foo!bar!", [])
    it "works with arguments" $ do
      let nodes = [ Assign "foo" (Left (ArgValue (String "foo!")))
                  , EscapedVar (key "foo")
                  , Assign "foo" (Left (ArgValue (String "bar!")))
                  , EscapedVar (key "foo") ]
          template = Template "test" $ M.fromList [("test", nodes)]
      renderMustache mempty template Null `shouldBe`
        ("foo!bar!", [])
    it "doesn't leak variables from partials out" $ do
      let partial = [ Assign "foo" (Left (ArgValue (String "bar!")))
                    , EscapedVar (key "foo")
                    , TextBlock "</partial>" ]
      let nodes = [ Assign "foo" (Left (ArgValue (String "foo!")))
                  , Partial "partial" [] Nothing
                  , EscapedVar (key "foo")
                  , TextBlock "</test>"]
          template = Template "test" $
            M.fromList [ ("test", nodes)
                       , ("partial", partial) ]
      renderMustache mempty template Null `shouldBe`
        ("bar!</partial>foo!</test>", [])
    it "passes variables to partials" $ do
      let partial = [ TextBlock "<partial>"
                    , EscapedVar (key "foo")
                    , Assign "foo" (Left (ArgValue (String "bar!")))
                    , EscapedVar (key "foo")
                    , TextBlock "</partial>" ]
      let nodes = [ TextBlock "<test>"
                  , Assign "foo" (Left (ArgValue (String "foo!")))
                  , Partial "partial" [] Nothing
                  , EscapedVar (key "foo")
                  , TextBlock "</test>"]
          template = Template "test" $
            M.fromList [ ("test", nodes)
                       , ("partial", partial) ]
      renderMustache mempty template Null `shouldBe`
        ("<test><partial>foo!bar!</partial>foo!</test>", [])
    it "gives arguments precedence over variables" $ do
      let partial = [ EscapedVar (key "foo") ]
      let nodes = [
            Assign "foo" (Left (ArgValue (String "var"))),
            Partial "partial" [("foo", ArgValue (String "arg"))] Nothing ]
          template = Template "test" $
            M.fromList [ ("test", nodes)
                       , ("partial", partial) ]
      renderMustache mempty template Null `shouldBe`
        ("arg", [])
  context "(+) when rendering an interpolated argument" $ do
    let partial = [TextBlock "foo:", EscapedVar (key "foo")]
        mkTemplate ns = Template "test" $
          M.fromList [("test", ns), ("partial", partial)]
    it "works" $ do
      let inter = [TextBlock "hi", EscapedVar (key "x")]
      let template = mkTemplate [
            Partial "partial" [("foo", ArgInterpolated inter)] Nothing ]
      renderMustache mempty template (object ["x" .= (5 :: Int)]) `shouldBe`
        ("foo:hi5", [])
    it "keeps variables inside the interpolated argument" $ do
      let inter = [Assign "x" (Left (ArgValue (Number 5)))]
      let template = mkTemplate [
            Partial "partial" [("foo", ArgInterpolated inter)] Nothing,
            EscapedVar (key "x") ]
      renderMustache mempty template Null `shouldBe`
        ("foo:", ["missing key: x"])
  context "(+) warnings" $ do
    it "warns about missing partials" $ do
      let nodes = [ Partial "foo" [] Nothing
                  , Partial "bar" [] Nothing ]
      let template = Template "test" $ M.fromList [("test", nodes)]
      renderMustache mempty template Null `shouldBe`
        ("", ["missing partial: foo", "missing partial: bar"])
    it "warns about missing keys in vars" $ do
      let nodes = [ EscapedVar (key "foo")
                  , UnescapedVar (key "bar.baz") ]
      let template = Template "test" $ M.fromList [("test", nodes)]
      renderMustache mempty template Null `shouldBe`
        ("", ["missing key: foo", "missing key: bar.baz"])
    it "warns about missing keys in vars" $ do
      let nodes = [ EscapedVar (key "foo")
                  , UnescapedVar (key "bar.baz") ]
      let template = Template "test" $ M.fromList [("test", nodes)]
      renderMustache mempty template Null `shouldBe`
        ("", ["missing key: foo", "missing key: bar.baz"])
    it "warns about missing keys in arguments" $ do
      let nodes = [Partial "part" [("foo", ArgVariable (key "bar"))] Nothing]
      let template = Template "test" $
            M.fromList [ ("test", nodes)
                       , ("part", []) ]
      renderMustache mempty template Null `shouldBe`
        ("", ["missing key: bar"])
    it "warns about unknown functions" $ do
      let nodes = [Assign "foo" (Right ("blah", []))]
      let template = Template "test" $ M.fromList [("test", nodes)]
      renderMustache mempty template Null `shouldBe`
        ("", ["unknown function: blah"])
    it "doesn't warn about missing keys in sections" $ do
      let nodes = [Section (key "foo") []]
      let template = Template "test" $ M.fromList [("test", nodes)]
      renderMustache mempty template Null `shouldBe`
        ("", [])
