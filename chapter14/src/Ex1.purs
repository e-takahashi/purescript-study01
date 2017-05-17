module Ex1 where

import Prelude
import Data.Maybe (Maybe(..))

type Tabun = Maybe

newtype NewTabun a = NewTabun (Maybe a)

mkInt :: Int -> Tabun Int
mkInt n = Just n
mkInt' :: Int -> NewTabun Int
mkInt' n = NewTabun (mkInt n)

mkString :: String -> Tabun String
mkString s = Just s
mkString' :: String -> NewTabun String
mkString' s = NewTabun (mkString s)

mkNon :: forall a. a -> Tabun a
mkNon _ = Nothing
mkNon' :: forall a. a -> NewTabun Unit
mkNon' _ = NewTabun (mkNon unit)

render :: forall a. Show a => Tabun a -> String
render Nothing = "Tabun non"
render (Just x) = "Tabun " <> show x

render' :: forall a. Show a => NewTabun a -> String
render' (NewTabun x) = render x

