module Utils.DList (
  DList (..)
) where

newtype DList a = DList ([a] -> [a])

toDList :: [a] -> DList a
toDList xs = DList (xs ++)

fromDList :: DList a -> [a]
fromDList (DList f) = f []

instance Semigroup (DList a) where
  (DList f1) <> (DList f2) = DList (f1 . f2) 

instance Monoid (DList a) where
  mempty = DList id
