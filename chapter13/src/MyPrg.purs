module MyPrg where

import Prelude

import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf, elements, Gen)
import Data.String
import Data.NonEmpty (NonEmpty, singleton, (:|))
import Control.Plus (empty)
import Data.Show

newtype Byte = Byte Int

instance arbitraryByte :: Arbitrary Byte where
  arbitrary = map intToByte arbitrary
    where
      intToByte :: Int -> Byte
      intToByte n | n >= 0 = Byte (n `mod` 256)
                  | otherwise = intToByte (-n)

{-
arbitrary
Gen Byte
Gen (State GenState Byte)
Gen (StateT GenState Identity Byte)
Gen (StateT { newSeed::Seed, size::Size } Identity Byte)

map f (Gen (StateT { newSeed::Seed, size::Size } Identity Byte))
多分
map f (StateT { newSeed::Seed, size::Size } Identity Byte)
map f (StateT a)
StateT (\s -> map (\(Tuple b s') -> Tuple (f b) s') (a s))
  a :: GenState -> Identity (Tuple Byte GenState)
StateT (\s -> map (\(Tuple b s') -> Tuple (f b) s') (a s))
StateT (map (\(Touple b s') -> Touple (f b) s') identity (Touple byte genState))
StateT (Identity (Touple (f byte) genState))
f byte
IntToByte byte
なぜかbyteはByteでなくInt

-}

newtype RndString = RndString String

instance showRndString :: Show RndString where
  show (RndString s) = s

instance arbitraryRndString :: Arbitrary RndString where
  arbitrary =
    let
      alps = 'a' :| (toCharArray "bcdefghijklmnopqrstuvwxyz")
      ga   = elements alps
      gaa  = arrayOf ga
    in map (RndString <<< fromCharArray) gaa
       

