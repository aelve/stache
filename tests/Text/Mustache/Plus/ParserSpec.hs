{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.Plus.ParserSpec
  ( main
  , spec )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Aeson (Value(..))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Mustache.Plus.Parser
import Text.Mustache.Plus.Type
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as S

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parseMustache" $ do
  let p = parseMustache ""
      key = Key . pure
  it "parses text" $
    p "test12356p0--=-34{}jnv,\n"
      `shouldParse` [TextBlock "test12356p0--=-34{}jnv,\n"]
  context "when parsing a variable" $ do
    context "with white space" $ do
      it "parses escaped {{ variable }}" $
        p "{{ name }}" `shouldParse` [EscapedVar (key "name")]
      it "parses unescaped {{{ variable }}}" $
        p "{{{ name }}}" `shouldParse` [UnescapedVar (key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{& name }}" `shouldParse` [UnescapedVar (key "name")]
    context "without white space" $ do
      it "parses escaped {{variable}}" $
        p "{{name}}" `shouldParse` [EscapedVar (key "name")]
      it "parses unescaped {{{variable}}}" $
        p "{{{name}}}" `shouldParse` [UnescapedVar (key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{&name}}" `shouldParse` [UnescapedVar (key "name")]
    it "allows '-' in variable names" $
      p "{{ var-name }}" `shouldParse` [EscapedVar (key "var-name")]
    it "allows '_' in variable names" $
      p "{{ var_name }}" `shouldParse` [EscapedVar (key "var_name")]
  context "when parsing a section" $ do
    it "parses empty section" $
      p "{{#section}}{{/section}}" `shouldParse` [Section (key "section") []]
    it "parses non-empty section" $
      p "{{# section }}Hi, {{name}}!\n{{/section}}" `shouldParse`
        [Section (key "section")
         [ TextBlock "Hi, "
         , EscapedVar (key "name")
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
        [Partial "that-s_my-partial" [] (Just $ unsafePos 1)]
    it "parses a partial without white space" $
      p "{{>that-s_my-partial}}" `shouldParse`
        [Partial "that-s_my-partial" [] (Just $ unsafePos 1)]
    it "handles indented partial correctly" $
      p "   {{> next_one }}" `shouldParse`
        [Partial "next_one" [] (Just $ unsafePos 4)]
    it "(+) parses a partial with variables" $
      p "{{> partial arg1=foo.bar arg2=\"test\\n\"}}" `shouldParse`
        [Partial "partial"
                 [("arg1", Left (Key ["foo","bar"])),
                  ("arg2", Right (String "test\n"))]
                 (Just $ unsafePos 1)]
  context "when running into delimiter change" $ do
    it "has effect" $
      p "{{=<< >>=}}<<var>>{{var}}" `shouldParse`
        [EscapedVar (key "var"), TextBlock "{{var}}"]
    it "handles whitespace just as well" $
      p "{{=<<   >>=}}<<  var >>{{ var  }}" `shouldParse`
        [EscapedVar (key "var"), TextBlock "{{ var  }}"]
    it "affects {{{s" $
      p "{{=<< >>=}}<<{var}>>" `shouldParse`
        [UnescapedVar (key "var")]
    it "parses two subsequent delimiter changes" $
      p "{{=(( ))=}}(( var ))((=-- $-=))--#section$---/section$-" `shouldParse`
        [EscapedVar (key "var"), Section (key "section") []]
    it "propagates delimiter change from a nested scope" $
      p "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldParse`
        [Section (key "section") [], EscapedVar (key "var")]
  context "(+) when parsing an assignment" $ do
    it "parses assignment with no arguments" $
      p "{{foo = %some-func}}" `shouldParse`
        [Assign "foo" ("some-func", [])]
    it "parses assignment with two arguments" $
      p "{{foo = %some-func foo \"bar\"}}" `shouldParse`
        [Assign "foo" ("some-func", [ Left (key "foo")
                                    , Right (String "bar")])]
    it "parses numeric arguments" $
      p "{{foo = %some-func 1 -2 3.5e6}}" `shouldParse`
        [Assign "foo" ("some-func", [ Right (Number 1)
                                    , Right (Number (-2))
                                    , Right (Number 3.5e6) ] )]
  context "when given malformed input" $ do
    let pos l c = SourcePos "" (unsafePos l) (unsafePos c) :| []
        ne      = NE.fromList
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
        , errorExpected   = S.fromList [ Label (ne "key")
                                       , Label (ne "variable") ]
        , errorCustom     = S.empty }
