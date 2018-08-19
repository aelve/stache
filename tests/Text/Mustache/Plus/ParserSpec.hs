{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.Plus.ParserSpec
  ( main
  , spec )
where

import Data.Aeson (Value(..))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Mustache.Plus.Parser
import Text.Mustache.Plus.Type
import qualified Data.Text as T
import Data.Text (Text)

main :: IO ()
main = hspec spec

key :: Text -> Key
key "." = Key []
key s   = Key (T.splitOn "." s)

spec :: Spec
spec = describe "parseMustache" $ do
  let p = parseMustache ""
  it "parses text" $
    p "test12356p0--=-34{}jnv,\n" `shouldParse`
      [TextBlock "test12356p0--=-34{}jnv,\n"]
  context "when parsing a variable" $ do
    context "with white space" $ do
      it "parses escaped {{ variable }}" $
        p "{{ name }}" `shouldParse`
          [EscapedExpr (Variable (key "name"))]
      it "parses unescaped {{{ variable }}}" $
        p "{{{ name }}}" `shouldParse`
          [UnescapedExpr (Variable (key "name"))]
      it "parses unescaped {{& variable }}" $
        p "{{& name }}" `shouldParse`
          [UnescapedExpr (Variable (key "name"))]
    context "without white space" $ do
      it "parses escaped {{variable}}" $
        p "{{name}}" `shouldParse`
          [EscapedExpr (Variable (key "name"))]
      it "parses unescaped {{{variable}}}" $
        p "{{{name}}}" `shouldParse`
          [UnescapedExpr (Variable (key "name"))]
      it "parses unescaped {{& variable }}" $
        p "{{&name}}" `shouldParse`
          [UnescapedExpr (Variable (key "name"))]
    it "allows '-' in variable names" $
      p "{{ var-name }}" `shouldParse`
        [EscapedExpr (Variable (key "var-name"))]
    it "allows '_' in variable names" $
      p "{{ var_name }}" `shouldParse`
        [EscapedExpr (Variable (key "var_name"))]
  context "when parsing a section" $ do
    it "parses empty section" $
      p "{{#section}}{{/section}}" `shouldParse`
        [Section (key "section") []]
    it "parses non-empty section" $
      p "{{# section }}Hi, {{name}}!\n{{/section}}" `shouldParse`
        [Section (key "section")
         [ TextBlock "Hi, "
         , EscapedExpr (Variable (key "name"))
         , TextBlock "!\n"]]
  context "when parsing an inverted section" $ do
    it "parses empty inverted section" $
      p "{{^section}}{{/section}}" `shouldParse`
        [InvertedSection (key "section") []]
    it "parses non-empty inverted section" $
      p "{{^ section }}No one here?!\n{{/section}}" `shouldParse`
        [InvertedSection (key "section") [TextBlock "No one here?!\n"]]
  context "when parsing a partial" $ do
    it "parses a partial with white space" $
      p "{{> that-s_my-partial }}" `shouldParse`
        [Partial "that-s_my-partial" [] (Just pos1)]
    it "parses a partial without white space" $
      p "{{>that-s_my-partial}}" `shouldParse`
        [Partial "that-s_my-partial" [] (Just pos1)]
    it "handles indented partial correctly" $
      p "   {{> next_one }}" `shouldParse`
        [Partial "next_one" [] (Just $ mkPos 4)]
    it "(+) parses a partial with variables" $
      p "{{> partial arg1=foo.bar arg2=\"test\\n\"}}" `shouldParse`
        [Partial "partial"
                 [("arg1", Variable (key "foo.bar")),
                  ("arg2", Literal (String "test\n"))]
                 (Just pos1)]
  context "when running into delimiter change" $ do
    it "has effect" $
      p "{{=<< >>=}}<<var>>{{var}}" `shouldParse`
        [EscapedExpr (Variable (key "var")), TextBlock "{{var}}"]
    it "handles whitespace just as well" $
      p "{{=<<   >>=}}<<  var >>{{ var  }}" `shouldParse`
        [EscapedExpr (Variable (key "var")), TextBlock "{{ var  }}"]
    it "affects {{{s" $
      p "{{=<< >>=}}<<{var}>>" `shouldParse`
        [UnescapedExpr (Variable (key "var"))]
    it "parses two subsequent delimiter changes" $
      p "{{=(( ))=}}(( var ))((=-- $-=))--#section$---/section$-" `shouldParse`
        [EscapedExpr (Variable (key "var")), Section (key "section") []]
    it "propagates delimiter change from a nested scope" $
      p "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldParse`
        [Section (key "section") [], EscapedExpr (Variable (key "var"))]
  context "(+) when parsing an assignment" $ do
    it "parses assignment with no functions" $
      p "{{foo = \"test\"}}" `shouldParse`
        [Assign "foo" (Literal (String "test"))]
    it "parses assignment with no arguments" $
      p "{{foo = %some-func}}" `shouldParse`
        [Assign "foo" (Call "some-func" [])]
    it "parses assignment with two arguments" $
      p "{{foo = %some-func foo \"bar\"}}" `shouldParse`
        [Assign "foo" (Call "some-func" [ Variable (key "foo")
                                        , Literal (String "bar")])]
    it "parses numeric arguments" $
      p "{{foo = %some-func 1 -2 3.5e6}}" `shouldParse`
        [Assign "foo" (Call "some-func" [ Literal (Number 1)
                                        , Literal (Number (-2))
                                        , Literal (Number 3.5e6)])]
  context "(+) when parsing an expression" $ do
    it "parses a literal" $
      p "{{ \"foo\" }}" `shouldParse`
        [EscapedExpr (Literal (String "foo"))]
    it "parses a function call with no arguments" $
      p "{{%some-func}}" `shouldParse`
        [EscapedExpr (Call "some-func" [])]
    it "parses a function call with two arguments" $
      p "{{ %some-func foo \"bar\" }}" `shouldParse`
        [EscapedExpr (Call "some-func" [ Variable (key "foo")
                                       , Literal (String "bar")])]
    it "parses a function call in parentheses" $
      p "{{ (%some-func foo (%other-func \"bar\") ) }}" `shouldParse`
        [EscapedExpr (Call "some-func" [
                         Variable (key "foo"),
                         Call "other-func" [Literal (String "bar")]])]
  context "(+) when parsing interpolated arguments" $ do
    it "parses an empty template" $
      p "{{> p x=[||]}}" `shouldParse`
        [Partial "p" [("x", Interpolated [])] (Just pos1)]
    it "parses a non-empty template" $ do
      let nodes = [ TextBlock " test "
                  , EscapedExpr (Variable (key "foo"))
                  , TextBlock " " ]
      p "{{> p x = [| test {{foo}} |] }}" `shouldParse`
        [Partial "p" [("x", Interpolated nodes)] (Just pos1)]
    it "parses a template with a “|]” in it" $ do
      let nodes = [Partial "p"
                     [("x", Literal (String "|]"))]
                     Nothing]
      p "{{> p x=[|{{> p x=\"|]\"}}|]}}" `shouldParse`
        [Partial "p" [("x", Interpolated nodes)] (Just pos1)]
    it "parses a template with interpolation in it" $ do
      let nodes = [Partial "p"
                     [("x", Interpolated [])]
                     Nothing]
      p "{{> p x=[|{{> p x=[||]}}|]}}" `shouldParse`
        [Partial "p" [("x", Interpolated nodes)] (Just pos1)]
    it "doesn't leak delimiter switches out" $ do
      let nodes = [ TextBlock " "
                  , TextBlock " "
                  , EscapedExpr (Variable (key "x"))
                  , TextBlock " " ]
      p "{{> p x=[| {{=( )=}} (x) |] }} {{y}}" `shouldParse`
        [ Partial "p" [("x", Interpolated nodes)] Nothing
        , TextBlock " "
        , EscapedExpr (Variable (key "y")) ]
{-
  context "when given malformed input" $ do
    let pos l c  = SourcePos "" (unsafePos l) (unsafePos c) :| []
        ne       = NE.fromList
    it "rejects unclosed tags" $
      p "{{ name " `shouldFailWith` ParseError
        { errorPos        = pos 1 9
        , errorUnexpected = S.singleton EndOfInput
        , errorExpected   = S.fromList [ Tokens (ne "}}")
                                       , Tokens (ne "=") ]
        , errorCustom     = S.empty }
    it "rejects unknown tags" $
      p "{{? boo }}" `shouldFailWith` ParseError
        { errorPos        = pos 1 3
        , errorUnexpected = S.singleton (Tokens $ ne "?")
        , errorExpected   = S.fromList $
            map (Label . ne) ["JSON value", "key", "variable"] ++
            map (Tokens . ne) ["[|", "%", "("]
        , errorCustom     = S.empty }
    it "(+) reports errors inside interpolated arguments" $
      p "{{> foo x=[| {{? x }} |] }}" `shouldFailWith` ParseError
        { errorPos        = pos 1 16
        , errorUnexpected = S.singleton (Tokens $ ne "?")
        , errorExpected   = S.fromList $
            map (Label . ne) ["JSON value", "key", "variable"] ++
            map (Tokens . ne) ["[|", "%", "("]
        , errorCustom     = S.empty }
    it "(+) doesn't allow a function call without parens in partial call" $
      p "{{> foo x = %bar }}" `shouldFailWith` ParseError
        { errorPos        = pos 1 13
        , errorUnexpected = S.singleton (Tokens $ ne "%")
        , errorExpected   = S.fromList $
            map (Label . ne) ["JSON value", "key"] ++
            map (Tokens . ne) ["[|", "("]
        , errorCustom     = S.empty }
    it "doesn't allow expressions in sections" $
      p "{{# %foo }}{{/ %foo }}" `shouldFailWith` ParseError
        { errorPos        = pos 1 5
        , errorUnexpected = S.singleton (Tokens $ ne "%")
        , errorExpected   = S.fromList $ [Label (ne "key")]
        , errorCustom     = S.empty }
-}
