module Data.DOM.Name3
{--}
  ( Element
  , Attribute
  , Name
  , Content
  , ContentF
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
  , elem
  , newName

  , render
  )
--}
 where

import Prelude

import Control.Monad.Free (Free, runFreeM, liftF, runFree)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (put, get)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

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

instance functorContentF :: Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)
  map f (NewName k) = NewName (f <<< k)

newtype Content a = Content (Free ContentF a)

--derive instance functorContent :: Functor Content

{--}
instance functorContent :: Functor Content where
  map f (Content c) = Content f (runFree k c) where
    k :: forall a. ContentF (Free ContentF a) -> (Free ContentF a)
    k (TextContent s a) = liftF $ TextContent s a
    k (ElementContent e a) = liftF $ ElementContent e a
    k (NewName l) = liftF $ NewName l
--}

instance applyContent :: Apply Content where
  apply (Content f) (Content c) = Content (f c)

instance applicativeContent :: Applicative Content where
  pure a = Content a

instance bindContent :: Bind Content where
  bind (Content c) f = Content (map f (runFree k c)) where
    k :: forall f a. Functor f => ContentF (Free ContentF a) -> Free f a
    k = liftF

instance monadContent :: Monad Content
--}

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

a :: Array Attribute -> Free ContentF Unit -> Element
a attribs content = element "a" attribs (Just (Content content))

p :: Array Attribute -> Free ContentF Unit -> Element
p attribs content = element "p" attribs (Just (Content content))

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

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

type Interp = WriterT String (State Int)

render :: Element -> String
render = \e -> evalState (execWriterT (renderElement e)) 0
  where
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
