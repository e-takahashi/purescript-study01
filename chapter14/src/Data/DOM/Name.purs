module Data.DOM.Name
{--
}
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
  , NewContent(..)

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

type Content = Free ContentF
newtype NewContent a = NewContent (Free ContentF a)
--type NewContent a = Free ContentF a

newtype Attribute = Attribute
  { key          :: String
  , value        :: String
  }

newtype AttributeKey a = AttributeKey String

--element :: String -> Array Attribute -> Maybe (Content Unit) -> Element
element :: String -> Array Attribute -> Maybe (NewContent Unit) -> Element
element name_ attribs content = Element { name: name_, attribs, content }

--text :: String -> Content Unit
--text s = liftF $ TextContent s unit
--text s = Content $ liftF $ TextContent s unit
text :: String -> NewContent Unit
text s = NewContent $ liftF $ TextContent s unit
--text s = liftF $ TextContent s unit

--elem :: Element -> Content Unit
--elem e = liftF $ ElementContent e unit
--elem e = Content $ liftF $ ElementContent e unit
elem :: Element -> NewContent Unit
elem e = NewContent $ liftF $ ElementContent e unit
--elem e = liftF $ ElementContent e unit

--newName :: Content Name
--newName = liftF $ NewName id
--newName = Content $ liftF $ NewName id
--newName :: NewContent Name
newName :: Free ContentF Name
--newName = NewContent $ liftF $ NewName id
newName = liftF $ NewName id

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

a' :: Array Attribute -> NewContent Unit -> Element
a' attribs content = element "a" attribs (Just content)
--a ::  Array Attribute -> Content Unit -> Content Unit
a ::  Array Attribute -> NewContent Unit -> NewContent Unit
a attribs content = elem $ a' attribs content

p' :: Array Attribute -> NewContent Unit -> Element
p' attribs content = element "p" attribs (Just content)
--p :: Array Attribute -> Content Unit -> Content Unit
p :: Array Attribute -> NewContent Unit -> NewContent Unit
p attribs content = elem $ p' attribs content

img' :: Array Attribute -> Element
img' attribs = element "img" attribs Nothing
--img :: Array Attribute -> Content Unit
img :: Array Attribute -> NewContent Unit
img attribs = elem $ img' attribs

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

{--}
render :: NewContent Unit -> String
render = \e -> evalState (execWriterT (renderContentItem e)) 0
  where
    renderContentItem :: forall a. (NewContent a) -> Interp a
    renderContentItem (NewContent c) = runFreeM renderContentItem' c
    renderContentItem' :: forall a. ContentF (Free ContentF a) -> Interp (Free ContentF a)
    renderContentItem' (TextContent s rest) = do
      tell s
      pure rest
    renderContentItem' (ElementContent e' rest) = do
      renderElement e'
      pure rest
    renderContentItem' (NewName k) = do
{--}
      n <- get
      let fresh = Name $ "name" <> show n
      put $ n + 1
      pure (k fresh)
--}
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
        renderContent (Just content) = do
          tell ">"
          renderContentItem content
          tell "</"
          tell e.name
          tell ">"
--}

{--
render :: Content Unit -> String
render = \e -> evalState (execWriterT (runFreeM renderContentItem e)) 0
  where
    renderContentItem :: forall a. ContentF (Content a) -> Interp (Content a)
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
        renderContent (Just content) = do
          tell ">"
          runFreeM renderContentItem content
          tell "</"
          tell e.name
          tell ">"

--}

{--
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
        renderContent (Just content) = do
          tell ">"
          runFreeM renderContentItem content
          tell "</"
          tell e.name
          tell ">"

        renderContentItem :: forall a. ContentF (Content a) -> Interp (Content a)
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
--}
