-- |
-- Module      :  Text.Mustache.Plus.Compile.TH
-- Copyright   :  © 2016 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Template Haskell helpers to compile Mustache templates at compile time.
-- This module is not imported as part of "Text.Mustache.Plus", so you need to
-- import it yourself. Qualified import is recommended, but not necessary.
--
-- At the moment, functions in this module only work with GHC 8 (they
-- require at least @template-haskell-2.11@).

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Mustache.Plus.Compile.TH
  ( compileMustacheDir
  , compileMustacheFile
  , compileMustacheText )
where

import Control.Monad.Catch (try)
import Data.Text.Lazy (Text)
import Data.Typeable (cast)
import Language.Haskell.TH hiding (Dec)
import Language.Haskell.TH.Syntax (lift)
import Text.Megaparsec hiding (try)
import Text.Mustache.Plus.Type
import qualified Data.Text             as T
import qualified Text.Mustache.Plus.Compile as C

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

#if MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH.Syntax (dataToExpQ)
#else
import Data.Data (Data)
dataToExpQ :: Data a => (forall b. Data b => b -> Maybe (Q Exp)) -> a -> Q Exp
dataToExpQ _ _ = fail "The feature requires at least GHC 8 to work"
#endif

-- | Compile all templates in specified directory and select one. Template
-- files should have extension @mustache@, (e.g. @foo.mustache@) to be
-- recognized. This function /does not/ scan the directory recursively.
--
-- This version compiles the templates at compile time.

compileMustacheDir
  :: PName             -- ^ Which template to select after compiling
  -> FilePath          -- ^ Directory with templates
  -> Q Exp             -- ^ The resulting template
compileMustacheDir pname path =
  (runIO . try) (C.compileMustacheDir pname path) >>= handleEither

-- | Compile single Mustache template and select it.
--
-- This version compiles the template at compile time.

compileMustacheFile
  :: FilePath          -- ^ Location of the file
  -> Q Exp
compileMustacheFile path =
  (runIO . try) (C.compileMustacheFile path) >>= handleEither

-- | Compile Mustache template from 'Text' value. The cache will contain
-- only this template named according to given 'Key'.
--
-- This version compiles the template at compile time.

compileMustacheText
  :: PName             -- ^ How to name the template?
  -> Text              -- ^ The template to compile
  -> Q Exp
compileMustacheText pname text =
  handleEither (C.compileMustacheText pname text)

-- | Given an 'Either' result return 'Right' and signal pretty-printed error
-- if we have a 'Left'.

handleEither :: Either (ParseError Char Dec) Template -> Q Exp
handleEither val =
  case val of
    Left err -> fail (parseErrorPretty err)
    Right template -> dataToExpQ (fmap liftText . cast) template

-- | Lift strict 'T.Text' to 'Q' 'Exp'.

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)
