-- 14.5 2
module Data.DOM.Phantom2
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , class IsValue
  , toValue

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height

  , attribute, (:=)
  , text
  , elem

  , render

  , True
  , False
  , disabled

{--
}
  , Foo(..)
  , class KnownFlag
  , flagVal
  , testFoo
  , On
  , Off
--}
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
  , value        :: String
  }

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

attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

infix 4 attribute as :=

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

width :: AttributeKey Int
width = AttributeKey "width"

height :: AttributeKey Int
height = AttributeKey "height"

--
data True
data False  
instance trueIsValue :: IsValue True where
  toValue _ = "true"
instance falseIsValue :: IsValue False where
  toValue _ = "false"

disabled :: AttributeKey True
disabled = AttributeKey "disabled"

--

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute x) = x.key <> "=\"" <> x.value <> "\""

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
data Foo a = Foo { getFoo :: Int }
data On
data Off
class KnownFlag a where
  flagVal :: Foo a -> Boolean
instance onKnownFlag :: KnownFlag On where
  flagVal _ = true
instance offKnownFlag :: KnownFlag Off where
  flagVal _ = false
testFoo :: forall a. KnownFlag a => Foo a -> String
testFoo foo =
  (if flagVal foo then "On:" else "Off:") -- <> show (foo.getFoo)


http://philopon.github.io/kansaifp-3-3/#/6/1

Phantom type

-- aは値に出てこない
-- ここに保持したい情報を入れればよい

data Foo a = Foo { getFoo :: Int }

-- 型が違う必要があるので別々に定義

data On
data Off

-- フラグを値にするための型クラス

class KnownFlag a where
  flagVal :: Foo a -> Bool

instance KnownFlag On where
  flagVal _ = True

instance KnownFlag Off where
  flagVal _ = False

-- 型からフラグを読み取って使用する

testFoo :: KnownFlag a => Foo a -> String
testFoo foo =
  (if flagVal foo then "On:" else "Off:") ++ show (getFoo foo)

-- つかってみる

testFoo (Foo 12 :: Foo On)
"On:12"
testFoo (Foo 12 :: Foo Off)
"Off:12"

--}
