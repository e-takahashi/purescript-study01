module Data.DOM.Name2
{--}
  ( Element
  , Attribute
  , Name
--  , Content
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

--  , render
  , NewContent

  )
--}
 where

import Prelude

import Control.Monad.Free (Free, runFreeM, liftF)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (put, get)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
--  , content      :: Maybe (Content Unit)
  , content      :: Maybe (NewContent Unit)
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

--type Content = Free ContentF
newtype NewContent a = NewContent (Free ContentF a)

newtype Attribute = Attribute
  { key          :: String
  , value        :: String
  }

newtype AttributeKey a = AttributeKey String

element :: String -> Array Attribute -> Maybe (NewContent Unit) -> Element
element name_ attribs content = Element { name: name_, attribs, content }

text :: String -> NewContent Unit
text s = NewContent $ liftF $ TextContent s unit

elem :: Element -> NewContent Unit
elem e = NewContent $ liftF $ ElementContent e unit

newName :: NewContent Name
newName = NewContent $ liftF $ NewName id

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

a ::  Array Attribute -> NewContent Unit -> NewContent Unit
a attribs content = elem $ element "a" attribs (Just content)

p :: Array Attribute -> NewContent Unit -> NewContent Unit
p attribs content = elem $ element "p" attribs (Just content)

img :: Array Attribute -> NewContent Unit
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

type Interp = WriterT String (State Int)

{--
}
renderContentItem :: forall a. (NewContent a) -> Interp a
renderContentItem (NewContent c) = runFreeM renderContentItem' c

renderContentItem' :: forall a. ContentF (Content a) -> Interp (Content a)
renderContentItem' (TextContent s rest) = do
  pure rest
renderContentItem' (ElementContent e' rest) = do
  pure rest
renderContentItem' (NewName k) = do
  let fresh = Name $ "name"
  pure (k fresh)
--}

{--
}
render :: NewContent Unit -> String
render = \c -> evalState (execWriterT (renderContentTop c)) 0
  where
    renderContentTop :: NewContent Unit -> Interp Unit
    renderContentTop (NewContent c') = renderContentItem c'

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
        renderContent :: Maybe (NewContent Unit) -> Interp Unit
        renderContent Nothing = tell " />"
        renderContent (Just (NewContent content)) = do
          tell ">"
          renderContentItem content
          tell "</"
          tell e.name
          tell ">"

    renderContentItem :: forall a b. (Free ContentF b) -> Interp (Free ContentF a)
    renderContentItem c = do
      runFreeM renderContentItem' c
        where
          renderContentItem' :: forall a. ContentF (Free ContentF a) -> Interp (Free ContentF a)
          renderContentItem' (TextContent s rest) = do
            tell s
            pure rest
          renderContentItem' (ElementContent e' rest) = do
            renderElement e'
            pure rest
          renderContentItem' (NewName k) = do
            n <- get
            let fresh = Name $ "name" <> show n
            put $ n + 1
            pure (k fresh)
--}
