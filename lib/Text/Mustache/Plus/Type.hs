-- |
-- Module      :  Text.Mustache.Plus.Type
-- Copyright   :  © 2016 Stack Buliders
-- License     :  BSD 3 clause
--
-- Types used by the package. You don't usually need to import the module,
-- because "Text.Mustache.Plus" re-exports everything you may need, import that
-- module instead.

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Mustache.Plus.Type
  ( Template (..)
  , Expr (..)
  , Node (..)
  , Key (..)
  , keyToString
  , PName (..)
  , Function
  , FunctionM
  , MustacheException (..) )
where

import Control.DeepSeq
import Control.Exception (Exception(..))
import Data.Aeson (Value)
import Data.Data (Data)
import Data.Map (Map)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString (..))
import Data.List (intercalate)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void
import GHC.Generics
import Text.Megaparsec
import qualified Data.Map  as M
import qualified Data.Text as T

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

-- | Mustache template as name of “top-level” template and a collection of
-- all available templates (partials).
--
-- 'Template' is a 'Semigroup'. This means that you can combine 'Template's
-- (and their caches) using the ('<>') operator, the resulting 'Template'
-- will have the same currently selected template as the left one. Union of
-- caches is also left-biased.

data Template = Template
  { templateActual :: PName
    -- ^ Name of currently “selected” template (top-level one).
  , templateCache  :: Map PName [Node]
    -- ^ Collection of all templates that are available for interpolation
    -- (as partials). The top-level one is also contained here and the
    -- “focus” can be switched easily by modifying 'templateActual'.
  } deriving (Eq, Show, Data, Typeable, Generic)

instance Semigroup Template where
  (Template pname x) <> (Template _ y) = Template pname (M.union x y)

data Expr
  = Variable Key         -- ^ A key which will be looked up
  | Literal Value        -- ^ A JSON value
  | Call Text [Expr]     -- ^ A function call
  | Interpolated [Node]  -- ^ A template which will be turned into string
                         --   during evaluation (variables set inside the
                         --   template won't spill outside)
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Structural element of template.

data Node
  = TextBlock       Text        -- ^ Plain text contained between tags
  | EscapedExpr     Expr        -- ^ HTML-escaped expression
  | UnescapedExpr   Expr        -- ^ Unescaped expression
  | Assign          Text Expr   -- ^ Assign an expression to a variable
  | Section         Key [Node]  -- ^ Mustache section
  | InvertedSection Key [Node]  -- ^ Inverted section
  | Partial         PName [(Text, Expr)] (Maybe Pos)
    -- ^ Partial with (optional) arguments and indentation level ('Nothing'
    --   means it was inlined)
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Identifier for values to interpolate.
--
-- The representation is the following:
--
--     * @[]@ — empty list means implicit iterators;
--     * @[text]@ — single key is a normal identifier;
--     * @[text1, text2]@ — multiple keys represent dotted names.

newtype Key = Key { unKey :: [Text] }
  deriving (Eq, Ord, Show, Semigroup, Monoid, Data, Typeable, Generic)

instance NFData Key

keyToString :: Key -> String
keyToString (Key []) = "."
keyToString (Key ks) = intercalate "." (map T.unpack ks)
{-# INLINE keyToString #-}

-- | Identifier for partials. Note that with the @OverloadedStrings@
-- extension you can use just string literals to create values of this type.

newtype PName = PName { unPName :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance IsString PName where
  fromString = PName . T.pack

instance NFData PName

-- | A pure function that can be called from the template.
type Function = [Value] -> Value

-- | A function (in some monad) that can be called from the template.
type FunctionM m = [Value] -> m Value

-- | Exception that is thrown when parsing of a template has failed.

data MustacheException = MustacheException (ParseError Char Void)
  deriving (Eq, Show, Typeable, Generic)

#if MIN_VERSION_base(4,8,0)
instance Exception MustacheException where
  displayException (MustacheException e) = parseErrorPretty e
#else
instance Exception MustacheException
#endif
