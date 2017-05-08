module Exercise.S3 where

import Prelude
import Data.Foldable (foldl)

data Point = Point
  { x :: Number
  , y :: Number
  }

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture

type Picture = Array Shape

instance showPoint :: Show Point where
  show (Point {x,y}) = "(" <> show x <> "," <> show y <> ")"

instance showShape :: Show Shape where
  show (Circle c r) = "Circle c:" <> show c <> ", r:" <> show r
  show (Rectangle c w h) = "Rectangle c:" <> show c <>
                           ", w:" <> show w <> ", h:" <> show h
  show (Line s e) = "Line start:" <> show s <> "," <> show e
  show (Text c txt) = "Text c:" <> show c <> ", txt:" <> txt
  show (Clipped ss) = "Picture " <>
                      foldl (\b s -> "[" <> show s <> "]") "" ss


newtype Complex = Complex
   { real :: Number
   , imaginary :: Number
   }

instance showComplex :: Show Complex where
  show (Complex {real:r, imaginary:i}) = show r <> " + " <> show i <> "i"

instance eqComprex :: Eq Complex where
  eq (Complex {real:ra, imaginary:ia}) (Complex {real:rb,imaginary:ib}) =
    (ra == rb) && (ia == ib)

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: (Eq a, Eq (Array a))  => Eq (NonEmpty a (Array a)) where
  eq (NonEmpty xa xsa) (NonEmpty xb xsb) =
    eq xa xb && eq xsa xsb
    
