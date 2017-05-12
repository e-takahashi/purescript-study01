module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (sortBy, intersect, nub, union, (\\), sort)
import Data.Foldable (foldr, foldl)
import Data.Function (on)
import Data.List (List(..), fromFoldable)
import Merge (mergeWith, mergePoly, merge)
import Sorted (sorted)
import Test.QuickCheck (quickCheck, (<?>))
import Tree (Tree, member, insert, toArray, anywhere)

import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, class Coarbitrary, coarbitrary)
import Data.NonEmpty ((:|))
import Data.String (fromCharArray, toCharArray)
import Test.QuickCheck.Gen (arrayOf, elements, Gen, sample, oneOf)
import Test.QuickCheck.LCG (mkSeed)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

isSubarrayOf :: forall a. (Eq a) => Array a -> Array a -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

ints :: Array Int -> Array Int
ints = id

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = id


--treeOfInt :: Tree Number -> Tree Number
treeOfInt :: Tree Int -> Tree Int
treeOfInt = id

-- 13.5
bools :: Array Boolean -> Array Boolean
bools = id

isUnion :: forall a. (Ord a) => Array a -> Array a -> Array a -> Boolean
isUnion xs ys zs =
  let
    ys' = nub ys
    ws  = ys' \\ xs
    zs' = xs <> ws
  in (sort zs) == (sort zs')
--

-- 13.6
newtype Byte = Byte Int

instance arbitraryByte :: Arbitrary Byte where
  arbitrary = map intToByte arbitrary
    where
    intToByte n | n >= 0 = Byte (n `mod` 256)
                | otherwise = intToByte (-n)
--
-- 13.9 1
instance coarbByte :: Coarbitrary Byte where
  coarbitrary (Byte n) = coarbitrary n

instance showByte :: Show Byte where
  show (Byte a) = "Byte(" <> show a <> ")"
--    

-- 13.6 2
insertMany :: forall a. Ord a => Array a -> Tree a -> Tree a
insertMany ns t = foldl (\b a -> insert a b) t ns
--


-- 13.6
newtype RndString = RndString String

instance arbitraryRndString :: Arbitrary RndString where
  arbitrary = map (RndString <<< fromCharArray) g where
    g = arrayOf $ elements ('a' :| (toCharArray "bcdefghijklmnopqrstuvwxyz"))


instance showRndString :: Show RndString where
  show (RndString s) = s

mycheck :: Int -> Int -> Array RndString
mycheck s n = sample (mkSeed s) n (arbitrary :: Gen RndString)
--

-- 13.9 4
data OneTwoThree a = One a | Two a a | Three a a a
instance arbOneTwoThree :: (Arbitrary a) =>  Arbitrary (OneTwoThree a) where
  arbitrary = oneOf $ arb1 :| [arb2, arb3] where
    arb1 = One <$> arbitrary
    arb2 = Two <$> arbitrary <*> arbitrary
    arb3 = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance showOneTowThree :: (Show a) => Show (OneTwoThree a) where
  show (One a) = "{" <> show a <> "}"
  show (Two a b) = "{" <> show a <> "," <> show b <> "}"
  show (Three a b c) = "{" <> show a <> "," <> show b <> "," <> show c <> "}"
--}


main :: Eff ( console :: CONSOLE
            , random :: RANDOM
            , exception :: EXCEPTION
            ) Unit
main = do
  -- Tests for module 'Merge'

  quickCheck $ \xs ys -> isSorted $ merge (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> xs `isSubarrayOf` merge xs ys

  quickCheck $ \xs ys -> isSorted $ ints $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> ints xs `isSubarrayOf` mergePoly xs ys

  quickCheck $ \xs ys f -> isSorted $ map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck $ \xs ys f -> xs `isSubarrayOf` mergeWith (intToBool f) xs ys

  -- Tests for module 'Tree'

  quickCheck $ \t a -> member a $ insert a $ treeOfInt t
  quickCheck $ \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck $ \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfInt t) || anywhere g t

  -- Exercises
  -- 13.4
  quickCheck $ \xs ->
    let result = merge xs []
--    let result = merge xs [1,2]
    in (xs == result) <?> "merge x []: " <> show xs <> " was changed. " <> show result

  quickCheck $ \xs ->
    let result = merge [] xs
--    let result = merge [1,2] xs
    in (xs == result) <?> "merge [] x: " <> show xs <> " was changed. " <> show result
  
  quickCheck $ \xs ys ->
    let
      result = merge (sorted xs) (sorted ys)
    in
     isSorted result <?> show xs <> "," <> show ys <> " not sorted " <> show result
  
  quickCheck $ \xs ys ->
    let
      result = merge xs ys
    in
     xs `isSubarrayOf` result <?> show xs <> " not a subarray of " <> show result

  -- 13.5
  quickCheck $ \xs ys -> isSorted $ bools $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> bools xs `isSubarrayOf` mergePoly xs ys
  
  quickCheck $ \xs ys ->
    let
      result = ints $ union xs ys
    in
     isUnion xs ys result <?> show result <> " is not the union of " <> show xs <> " and " <> show ys <> "."

  quickCheck $ \xs ys ->
    let
      result = bools $ union xs ys
    in
     isUnion xs ys result <?> show result <> " is not the union of " <> show xs <> " and " <> show ys <> "."
  --
  -- 13.6
  quickCheck $ \t a as -> member a $ insertMany as $ insert a $ treeOfInt t
  --
