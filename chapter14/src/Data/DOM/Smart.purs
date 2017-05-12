module Data.DOM.Smart
  ( Element
  , Attribute
  , Content
  , AttributeKey

  , a
  , p
  , img
  , input

  , href
  , _class
  , src
  , width
  , height
  , disabled
  , checked

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

newtype Attribute = Attribute
  { key          :: String
--  , value        :: String
  , value        :: Maybe String
  }

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

newtype AttributeKey = AttributeKey String

attribute :: AttributeKey -> String -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
--  , value: value
  , value: Just value
  }

infix 4 attribute as :=

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey
href = AttributeKey "href"

_class :: AttributeKey
_class = AttributeKey "class"

src :: AttributeKey
src = AttributeKey "src"

width :: AttributeKey
width = AttributeKey "width"

height :: AttributeKey
height = AttributeKey "height"

-- 14.4 2
checked :: Attribute
checked = Attribute { key: "checked", value: Nothing }

disabled :: Attribute
disabled = Attribute { key: "disabled", value: Nothing }

input :: Array Attribute -> Element
input attribs = element "input" attribs Nothing
--}

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
--    renderAttribute (Attribute x) = x.key <> "=\"" <> x.value <> "\""
    renderAttribute (Attribute { key:k, value: Just v}) = k <> "=\"" <> v <> "\""
    renderAttribute (Attribute { key:k, value: Nothing}) = k

    renderContent :: Maybe (Array Content) -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'

{--
14.4 1

log $ render $ p [_class:="top", width:="100%"][text "hello", elem (p [_class:
="inner"][text "world"])]

log $ render $ p [_class:="top", width:="100%"][ text "hello", elem (img [src:
="hoge.jpg", width:="50%", height:="50%"])]

--}
