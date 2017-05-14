module Data.DOM.Phantom
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , Length(..)
  , True(..)
  , False(..)
  , NoSpec(..)
  , class IsValue
  , toValue
  , class MyBool

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height
  , disabled

  , attribute, (:=)
  , text
  , elem

  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Array Content)
  }

data Content
  = TextContent String
  | ElementContent Element

-- 14.5 2
class MyBool a
data True  = True
data False = False
data NoSpec = NoSpec
instance myboolTrue :: MyBool True
instance myboolFalse :: MyBool False
instance myboolNoSpec :: MyBool NoSpec
--

{--14.5 2
newtype Attribute = Attribute
  { key          :: String
--14.5 2  , value        :: String
    , value        :: Maybe String
  }
--}
data Attribute = Attribute
  { key          :: String
    , value        :: String
  } | AttEmpty { key :: String }

newtype AttributeKey a = AttributeKey String

element :: String -> Array Attribute -> Maybe (Array Content) -> Element
element name attribs content = Element
  { name:      name
  , attribs:   attribs
  , content:   content
  }

text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = id

instance intIsValue :: IsValue Int where
  toValue = show

-- 14.5 2
instance trueIsValue :: IsValue True where
  toValue True = "true"
  
instance falseIsValue :: IsValue False where
  toValue False = "false"

instance nospecIsValue :: IsValue NoSpec where
  toValue NoSpec = ""
--

attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
--attribute (AttributeKey key) NoSpec = AttEmpty { key: key }
attribute (AttributeKey key) value = Attribute
  { key: key
--14.5 2  , value: toValue value
--14.5 2  , value: Just $ toValue value
  , value: toValue value
  }

infix 4 attribute as :=

{--14.5 2
attributeEx :: forall a. AttributeKey a -> Attribute
attributeEx (AttributeKey key) = AttEmpty
  { key: key }
--}

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey String
href = AttributeKey "href"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

--14.5 1 width :: AttributeKey Int
width :: AttributeKey Length
width = AttributeKey "width"

--14.5 1 height :: AttributeKey Int
height :: AttributeKey Length
height = AttributeKey "height"

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute x) = x.key <> "=\"" <> x.value <> "\""
--14.5 2
    renderAttribute (AttEmpty x) = x.key
--}
{--14.5 2
    renderAttribute (Attribute {key:key, value:(Just value)}) = key <> "=\"" <> value <> "\""
    renderAttribute (Attribute {key:key, value:Nothing}) = key
--}
    renderContent :: Maybe (Array Content) -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'

-- 14.5 1
data Length = Pixcel Int | Percentage Int
instance pxIsValue :: IsValue Length where
  toValue (Pixcel p) = show p <> "px"
  toValue (Percentage p) = show p <> "%"
--}
-- 14.5 2
--disabled :: Attribute
--disabled = attributeEx (AttributeKey "disabled")
disabled :: forall a. MyBool a => AttributeKey a
disabled = AttributeKey "disabled"
--}
