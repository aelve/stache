-- |
-- Module      :  Text.Mustache.Plus.Render
-- Copyright   :  © 2016 Stack Builders
-- License     :  BSD 3 clause
--
-- Functions for rendering Mustache templates. You don't usually need to
-- import the module, because "Text.Mustache.Plus" re-exports everything you may
-- need, import that module instead.

{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.Plus.Render
  ( renderMustache
  , renderMustacheM )
where

import Data.Functor.Identity
import Control.Monad.RWS.Lazy
import Data.Aeson
import Data.Foldable (asum)
import Control.Applicative
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Megaparsec.Pos (Pos, unPos)
import Text.Mustache.Plus.Type
import qualified Data.ByteString.Lazy   as B
import qualified Data.HashMap.Strict    as H
import qualified Data.List.NonEmpty     as NE
import qualified Data.Map               as M
import qualified Data.Semigroup         as S
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Vector            as V

----------------------------------------------------------------------------
-- The rendering monad

-- | Synonym for the monad we use for rendering.
--
-- * Reader ('RenderContext'): rendering context
-- * Writer ('B.Builder'): rendered result
-- * State ('Object'): variables

type Render m a = RWST (RenderContext m) B.Builder Object m a

-- | The render monad context.

data RenderContext m = RenderContext
  { -- | Actual indentation level
    rcIndent    :: Maybe Pos
    -- | The context stack
  , rcContext   :: NonEmpty Value
    -- | Prefix accumulated by entering sections
  , rcPrefix    :: Key
    -- | The template to render
  , rcTemplate  :: Template
    -- | Functions that can be called
  , rcFunctions :: M.Map Text (FunctionM m)
    -- | Is this last node in this partial?
  , rcLastNode  :: Bool
  }

----------------------------------------------------------------------------
-- High-level interface

-- | Render a Mustache 'Template' using Aeson's 'Value' to get actual values
-- for interpolation.

renderMustache :: M.Map Text Function -> Template -> Value -> TL.Text
renderMustache fs t v =
  runIdentity $ renderMustacheM (fmap (fmap Identity) fs) t v

renderMustacheM
  :: Monad m
  => M.Map Text (FunctionM m) -> Template -> Value -> m TL.Text
renderMustacheM fs t v =
  runRender (renderPartial (templateActual t) Nothing renderNode) fs t v

-- | Render a single 'Node'.

renderNode :: Monad m => Node -> Render m ()
renderNode (TextBlock txt) = outputIndented txt
renderNode (EscapedVar k) =
  lookupKey k >>= outputRaw . escapeHtml . renderValue
renderNode (UnescapedVar k) =
  lookupKey k >>= outputRaw . renderValue
renderNode (Assign k (fname, args)) = do
  mbFunc <- M.lookup fname <$> asks rcFunctions
  forM_ mbFunc $ \func -> do
    resolvedArgs <- mapM resolveArg args
    val <- lift (func resolvedArgs)
    modify (H.insert k val)
renderNode (Section k ns) = do
  val <- lookupKey k
  enterSection k $
    unless (isBlank val) $
      case val of
        Object _ ->
          addToLocalContext val (renderMany renderNode ns)
        Array xs ->
          forM_ (V.toList xs) $ \x ->
            addToLocalContext x (renderMany renderNode ns)
        _ ->
          renderMany renderNode ns
renderNode (InvertedSection k ns) = do
  val <- lookupKey k
  when (isBlank val) $
    renderMany renderNode ns
renderNode (Partial pname args indent) = do
  resolvedArgs <- forM args $ \(argName, arg) ->
    (,) <$> pure argName <*> resolveArg arg
  vars <- get; put mempty
  -- Note that 'addToLocalContext' works in such a way that the resulting
  -- context will be 'resolvedArgs : vars : context'. Here's a relevant
  -- demonstration using 'local' (which 'addToLocalContext' uses):
  --
  -- >>> runReader (local (1:) $ local (2:) $ ask) []
  -- [2,1]
  addToLocalContext (Object vars) $
    addToLocalContext (Object (H.fromList resolvedArgs)) $
      renderPartial pname indent renderNode
  put vars

----------------------------------------------------------------------------
-- The rendering monad vocabulary

-- | Run 'Render' monad given template to render and a 'Value' to take
-- values from.

runRender
  :: Monad m
  => Render m a -> M.Map Text (FunctionM m) -> Template -> Value -> m TL.Text
runRender m f t v = B.toLazyText . snd <$> evalRWST m rc mempty
  where
    rc = RenderContext
      { rcIndent    = Nothing
      , rcContext   = v :| []
      , rcPrefix    = mempty
      , rcTemplate  = t
      , rcFunctions = f
      , rcLastNode  = True }
{-# INLINE runRender #-}

-- | Output a piece of strict 'Text'.

outputRaw :: Monad m => Text -> Render m ()
outputRaw = tell . B.fromText
{-# INLINE outputRaw #-}

-- | Output indentation consisting of appropriate number of spaces.

outputIndent :: Monad m => Render m ()
outputIndent = asks rcIndent >>= outputRaw . buildIndent
{-# INLINE outputIndent #-}

-- | Output piece of strict 'Text' with added indentation.

outputIndented :: Monad m => Text -> Render m ()
outputIndented txt = do
  level <- asks rcIndent
  lnode <- asks rcLastNode
  let f x = outputRaw (T.replace "\n" ("\n" <> buildIndent level) x)
  if lnode && T.isSuffixOf "\n" txt
    then f (T.init txt) >> outputRaw "\n"
    else f txt
{-# INLINE outputIndented #-}

-- | Render a partial.

renderPartial
  :: Monad m
  => PName                 -- ^ Name of partial to render
  -> Maybe Pos             -- ^ Indentation level to use
  -> (Node -> Render m ()) -- ^ How to render nodes in that partial
  -> Render m ()
renderPartial pname i f =
  local u (outputIndent >> getNodes >>= renderMany f)
  where
    u rc = rc
      { rcIndent   = addIndents i (rcIndent rc)
      , rcPrefix   = mempty
      , rcTemplate = (rcTemplate rc) { templateActual = pname }
      , rcLastNode = True }
{-# INLINE renderPartial #-}

-- | Get collection of 'Node's for actual template.

getNodes :: Monad m => Render m [Node]
getNodes = do
  Template actual cache <- asks rcTemplate
  return (M.findWithDefault [] actual cache)
{-# INLINE getNodes #-}

-- | Render many nodes.

renderMany
  :: Monad m
  => (Node -> Render m ()) -- ^ How to render a node
  -> [Node]                -- ^ The collection of nodes to render
  -> Render m ()
renderMany _ [] = return ()
renderMany f [n] = do
  ln <- asks rcLastNode
  local (\rc -> rc { rcLastNode = ln && rcLastNode rc }) (f n)
renderMany f (n:ns) = do
  local (\rc -> rc { rcLastNode = False }) (f n)
  renderMany f ns

resolveArg :: Monad m => Arg -> Render m Value
resolveArg (Left key) = lookupKey key
resolveArg (Right val) = return val

-- | Lookup a 'Value' by its 'Key'.

lookupKey :: Monad m => Key -> Render m Value
lookupKey (Key []) = NE.head <$> asks rcContext
lookupKey k = do
  v <- asks rcContext
  p <- asks rcPrefix
  vars <- get
  return . fromMaybe Null $
    case k of
      Key [x] -> H.lookup x vars <|>
                 asum [ asum (simpleLookup (Key prefix <> k) <$> v)
                      | prefix <- reverse (tails (unKey p)) ]
      Key _   -> asum (simpleLookup (p <> k) <$> v)

-- | Lookup a 'Value' by traversing another 'Value' using given 'Key' as
-- “path”.

simpleLookup :: Key -> Value -> Maybe Value
simpleLookup (Key [])     obj        = return obj
simpleLookup (Key (k:ks)) (Object m) = H.lookup k m >>= simpleLookup (Key ks)
simpleLookup _            _          = Nothing
{-# INLINE simpleLookup #-}

-- | Enter the section by adding given 'Key' prefix to current prefix.

enterSection :: Monad m => Key -> Render m a -> Render m a
enterSection p =
  local (\rc -> rc { rcPrefix = p <> rcPrefix rc })
{-# INLINE enterSection #-}

-- | Add new value on the top of context. The new value has the highest
-- priority when lookup takes place.

addToLocalContext :: Monad m => Value -> Render m a -> Render m a
addToLocalContext (Object v) | null v = id
addToLocalContext v =
  local (\rc -> rc { rcContext = NE.cons v (rcContext rc) })
{-# INLINE addToLocalContext #-}

----------------------------------------------------------------------------
-- Helpers

-- | Add two 'Maybe' 'Pos' values together.

addIndents :: Maybe Pos -> Maybe Pos -> Maybe Pos
addIndents Nothing  Nothing  = Nothing
addIndents Nothing  (Just x) = Just x
addIndents (Just x) Nothing  = Just x
addIndents (Just x) (Just y) = Just (x S.<> y)
{-# INLINE addIndents #-}

-- | Build intentation of specified length by repeating the space character.

buildIndent :: Maybe Pos -> Text
buildIndent Nothing = ""
buildIndent (Just p) = let n = fromIntegral (unPos p) - 1 in T.replicate n " "
{-# INLINE buildIndent #-}

-- | Select invisible values.

isBlank :: Value -> Bool
isBlank Null         = True
isBlank (Bool False) = True
isBlank (Object   m) = H.null m
isBlank (Array    a) = V.null a
isBlank (String   s) = T.null s
isBlank _            = False
{-# INLINE isBlank #-}

-- | Render Aeson's 'Value' /without/ HTML escaping.

renderValue :: Value -> Text
renderValue Null         = ""
renderValue (String str) = str
renderValue value        = (T.decodeUtf8 . B.toStrict . encode) value
{-# INLINE renderValue #-}

-- | Escape HTML represented as strict 'Text'.

escapeHtml :: Text -> Text
escapeHtml txt = foldr (uncurry T.replace) txt
  [ ("\"", "&quot;")
  , ("<",  "&lt;")
  , (">",  "&gt;")
  , ("&",  "&amp;") ]
{-# INLINE escapeHtml #-}
