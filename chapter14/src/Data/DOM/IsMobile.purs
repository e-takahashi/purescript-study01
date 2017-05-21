module Data.DOM.IsMobile
  ( Element
  , Attribute
  , Name
  , Content
  , AttributeKey
  , class IsValue
  , toValue
  , Href(..)

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height
  , name

  , attribute, (:=)
  , text
  , newName
  , isMobile

  , render
  ) where

import Prelude

import Control.Monad.Free (Free, runFreeM, liftF)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (put, get)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Control.Monad.Reader.Trans

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Content Unit)
  }

newtype Name = Name String

data ContentF a
  = TextContent String a
  | ElementContent Element a
  | NewName (Name -> a)
  | IsMobile (Boolean -> a)

instance functorContentF :: Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)
  map f (NewName k) = NewName (f <<< k)
  map f (IsMobile k) = IsMobile (f <<< k)

newtype Content a = Content (Free ContentF a)

instance contentFunctor :: Functor Content where
  map f (Content c) = Content (map f c)

instance contentApply :: Apply Content where
  apply (Content f) (Content c) = Content (apply f c)

instance contentApplicative :: Applicative Content where
  pure a = Content (pure a)

instance contentBind :: Bind Content where
  bind (Content fr) fn =
    Content (fr >>= (\a -> case fn a of Content fr' -> fr'))
    
instance monadContent :: Monad Content

newtype Attribute = Attribute
  { key          :: String
  , value        :: String
  }

newtype AttributeKey a = AttributeKey String

element :: String -> Array Attribute -> Maybe (Content Unit) -> Element
element name_ attribs content = Element { name: name_, attribs, content }

text :: String -> Content Unit
text s = Content $ liftF $ TextContent s unit

elem :: Element -> Content Unit
elem e = Content $ liftF $ ElementContent e unit

newName :: Content Name
newName = Content $ liftF $ NewName id

{--}
isMobile :: Content Boolean
isMobile = Content $ liftF $ IsMobile id
--}

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = id

instance intIsValue :: IsValue Int where
  toValue = show

instance nameIsValue :: IsValue Name where
  toValue (Name n) = n

attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

infix 4 attribute as :=

a ::  Array Attribute -> Content Unit -> Content Unit
a attribs content = elem $ element "a" attribs (Just content)

p :: Array Attribute -> Content Unit -> Content Unit
p attribs content = elem $ element "p" attribs (Just content)

img :: Array Attribute -> Content Unit
img attribs = elem $ element "img" attribs Nothing

data Href
  = URLHref String
  | AnchorHref Name

instance hrefIsValue :: IsValue Href where
  toValue (URLHref url) = url
  toValue (AnchorHref (Name nm)) = "#" <> nm

href :: AttributeKey Href
href = AttributeKey "href"

name :: AttributeKey Name
name = AttributeKey "name"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Int
width = AttributeKey "width"

height :: AttributeKey Int
height = AttributeKey "height"

--type Interp = WriterT String (State Int)
type Interp = ReaderT Boolean (WriterT String (State Int))

render :: Content Unit -> String
--render = \c -> evalState (execWriterT (renderContentTop c)) 0
render = \c -> evalState (execWriterT (runReaderT (renderContentTop c) false)) 0
  where
    renderContentTop :: Content Unit -> Interp Unit
    renderContentTop (Content c') = do
      runFreeM renderContentItem c'
    renderElement :: Element -> Interp Unit
    renderElement (Element e) = do
        tell "<"
        tell e.name
        for_ e.attribs $ \x -> do
          tell " "
          renderAttribute x
        renderContent e.content
      where
        renderAttribute :: Attribute -> Interp Unit
        renderAttribute (Attribute x) = do
          tell x.key
          tell "=\""
          tell x.value
          tell "\""
        renderContent :: Maybe (Content Unit) -> Interp Unit
        renderContent Nothing = tell " />"
        renderContent (Just (Content content)) = do
          tell ">"
          runFreeM renderContentItem content
          tell "</"
          tell e.name
          tell ">"

    renderContentItem :: forall a. ContentF (Free ContentF a) -> Interp (Free ContentF a)
    renderContentItem (TextContent s rest) = do
      tell s
      pure rest
    renderContentItem (ElementContent e' rest) = do
      renderElement e'
      pure rest
    renderContentItem (NewName k) = do
      n <- get
      let fresh = Name $ "name" <> show n
      put $ n + 1
      pure (k fresh)
    renderContentItem (IsMobile k) = do
      mbl <- ask
      pure (k mbl)
      
