-- |
-- Module      :  Text.Mustache.Plus.Parser
-- Copyright   :  © 2016 Stack Builders
-- License     :  BSD 3 clause
--
-- Megaparsec parser for Mustache templates. You don't usually need to
-- import the module, because "Text.Mustache.Plus" re-exports everything you may
-- need, import that module instead.

module Text.Mustache.Plus.Parser
  ( parseMustache )
where

import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as TL
import Text.Megaparsec
import Data.Aeson (Value(..))
import Text.Mustache.Plus.Type
import qualified Data.Text             as T
import qualified Text.Megaparsec.Lexer as L

----------------------------------------------------------------------------
-- Parser

-- | Parse given Mustache template.

parseMustache
  :: FilePath
     -- ^ Location of file to parse
  -> TL.Text
     -- ^ File contents (Mustache template)
  -> Either (ParseError Char Dec) [Node]
     -- ^ Parsed nodes or parse error
parseMustache = parse $
  evalStateT (pMustache eof) (Delimiters "{{" "}}")

pMustache :: Parser () -> Parser [Node]
pMustache = fmap catMaybes . manyTill (choice alts)
  where
    alts =
      [ Just    <$> pAssign
      , Nothing <$  withStandalone pComment
      , Just    <$> pSection "#" Section
      , Just    <$> pSection "^" InvertedSection
      , Just    <$> pStandalone (pPartial Just)
      , Just    <$> pPartial (const Nothing)
      , Nothing <$  withStandalone pSetDelimiters
      , Just    <$> pUnescapedVariable
      , Just    <$> pUnescapedSpecial
      , Just    <$> pEscapedVariable
      , Just    <$> pTextBlock ]
{-# INLINE pMustache #-}

pTextBlock :: Parser Node
pTextBlock = do
  start <- gets openingDel
  (void . notFollowedBy . string) start
  let terminator = choice
        [ (void . lookAhead . string) start
        , pBol
        , eof ]
  TextBlock . T.pack <$> someTill anyChar terminator
{-# INLINE pTextBlock #-}

pUnescapedVariable :: Parser Node
pUnescapedVariable = UnescapedVar <$> pTag "&"
{-# INLINE pUnescapedVariable #-}

pUnescapedSpecial :: Parser Node
pUnescapedSpecial =
  UnescapedVar <$> between (symbol "{{{") (string "}}}") pKey
{-# INLINE pUnescapedSpecial #-}

pAssign :: Parser Node
pAssign = do
  start <- gets openingDel
  end   <- gets closingDel
  vname <- try $ do
    symbol start
    vname <- lexeme (label "variable" pVarName)
    symbol "="
    return vname
  fname <- lexeme (char '%' *> label "function" pVarName)
  args <- many pArg
  string end
  return (Assign vname (fname, args))
{-# INLINE pAssign #-}

pSection :: String -> (Key -> [Node] -> Node) -> Parser Node
pSection suffix f = do
  key   <- withStandalone (pTag suffix)
  nodes <- (pMustache . withStandalone . pClosingTag) key
  return (f key nodes)
{-# INLINE pSection #-}

pPartial :: (Pos -> Maybe Pos) -> Parser Node
pPartial f = do
  pos <- f <$> L.indentLevel
  start <- gets openingDel
  end   <- gets closingDel
  between (symbol $ start ++ ">") (string end) $ do
    key <- pKey
    let pname = PName $ T.intercalate (T.pack ".") (unKey key)
    args <- many $ do
      argName <- pVarName
      char '='
      -- TODO: allow more things than just strings
      argVal <- pArg
      return (argName, argVal)
    return (Partial pname args pos)
{-# INLINE pPartial #-}

pJsonString :: Parser T.Text
pJsonString = T.pack <$> between (char '"') (char '"') (many pChar)
  where
    pChar = raw <|> (char '\\' *> quoted)
    raw = satisfy (\c -> c /= '"' && c /= '\\')
    quoted = choice [
      char '"'  $> '"',
      char '/'  $> '/',
      char '\\' $> '\\',
      char 'b'  $> '\b',
      char 't'  $> '\t',
      char 'f'  $> '\f',
      char 'n'  $> '\n',
      char 'r'  $> '\r',
      char 'u' *> (decodeUtf <$> count 4 hexDigitChar) ]
    decodeUtf x = toEnum (read ('0':'x':x) :: Int)
{-# INLINE pJsonString #-}

pComment :: Parser ()
pComment = void $ do
  start <- gets openingDel
  end   <- gets closingDel
  (void . symbol) (start ++ "!")
  manyTill anyChar (string end)
{-# INLINE pComment #-}

pSetDelimiters :: Parser ()
pSetDelimiters = void $ do
  start <- gets openingDel
  end   <- gets closingDel
  (void . symbol) (start ++ "=")
  start' <- pDelimiter <* scn
  end'   <- pDelimiter <* scn
  (void . string) ("=" ++ end)
  put (Delimiters start' end')
{-# INLINE pSetDelimiters #-}

pEscapedVariable :: Parser Node
pEscapedVariable = EscapedVar <$> pTag ""
{-# INLINE pEscapedVariable #-}

withStandalone :: Parser a -> Parser a
withStandalone p = pStandalone p <|> p
{-# INLINE withStandalone #-}

pStandalone :: Parser a -> Parser a
pStandalone p = pBol *> try (between sc (sc <* (void eol <|> eof)) p)
{-# INLINE pStandalone #-}

pTag :: String -> Parser Key
pTag suffix = do
  start <- gets openingDel
  end   <- gets closingDel
  between (symbol $ start ++ suffix) (string end) pKey
{-# INLINE pTag #-}

pClosingTag :: Key -> Parser ()
pClosingTag key = do
  start <- gets openingDel
  end   <- gets closingDel
  let str = keyToString key
  void $ between (symbol $ start ++ "/") (string end) (symbol str)
{-# INLINE pClosingTag #-}

pVarName :: Parser T.Text
pVarName = T.pack <$> some (alphaNumChar <|> oneOf "-_")

pKey :: Parser Key
pKey = (fmap Key . lexeme . label "key") (implicit <|> other)
  where
    implicit = [] <$ char '.'
    other    = sepBy1 pVarName (char '.')
{-# INLINE pKey #-}

pArg :: Parser Arg
pArg = do
  let pValue = lexeme (label "JSON string" (String <$> pJsonString))
  (Left <$> pKey) <|> (Right <$> pValue)

pDelimiter :: Parser String
pDelimiter = some (satisfy delChar) <?> "delimiter"
  where delChar x = not (isSpace x) && x /= '='
{-# INLINE pDelimiter #-}

pBol :: Parser ()
pBol = do
  level <- L.indentLevel
  unless (level == unsafePos 1) empty
{-# INLINE pBol #-}

----------------------------------------------------------------------------
-- Auxiliary types

-- | Type of Mustache parser monad stack.

type Parser = StateT Delimiters (Parsec Dec TL.Text)

-- | State used in Mustache parser. It includes currently set opening and
-- closing delimiters.

data Delimiters = Delimiters
  { openingDel :: String
  , closingDel :: String }

----------------------------------------------------------------------------
-- Lexer helpers and other

scn :: Parser ()
scn = L.space (void spaceChar) empty empty
{-# INLINE scn #-}

sc :: Parser ()
sc = L.space (void $ oneOf " \t") empty empty
{-# INLINE sc #-}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn
{-# INLINE lexeme #-}

symbol :: String -> Parser String
symbol = L.symbol scn
{-# INLINE symbol #-}

keyToString :: Key -> String
keyToString (Key []) = "."
keyToString (Key ks) = intercalate "." (T.unpack <$> ks)
{-# INLINE keyToString #-}
