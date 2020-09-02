{-# LANGUAGE FlexibleInstances #-}

module Utils.DList (
  DList (..),
  toDList,
  fromDList,
) where

import Data.String
import Text.Printf

newtype DList a = DList ([a] -> [a])

toDList :: [a] -> DList a
toDList xs = DList (xs ++)

fromDList :: DList a -> [a]
fromDList (DList f) = f []

instance Semigroup (DList a) where
  (DList f1) <> (DList f2) = DList (f1 . f2) 

instance Monoid (DList a) where
  mempty = DList id

instance IsString (DList Char) where
  fromString = toDList
