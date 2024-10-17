{-# LANGUAGE LambdaCase #-}
module BinaryTree (go) where

import Test.QuickCheck

import Data.Maybe (fromMaybe)
import Control.Monad (forM_)

data BinTree a = Leaf | Node a (BinTree a) (BinTree a) deriving Show

isWithin :: (Ord a) => (Maybe a, Maybe a) -> a -> Bool
isWithin (lo, hi) x = fitsLo x && fitsHi x
    where fitsLo = maybe (const True) (<=) lo
          fitsHi = maybe (const True) (>) hi 

genBinTree :: (Ord a, Arbitrary a) => (Maybe a, Maybe a) -> Gen (BinTree a)
genBinTree bounds@(lo, hi) = frequency [(1, return Leaf), (4, node)]
    where node = suchThatMaybe arbitrary (isWithin bounds) >>= (\case {
        Just x -> Node x <$> genBinTree (lo, Just x) <*> genBinTree (Just x, hi);
        _ -> return Leaf
    })

instance (Ord a, Arbitrary a) => Arbitrary (BinTree a) where
  arbitrary = genBinTree (Nothing, Nothing)

treeFold :: (a -> b -> b -> b) -> b -> BinTree a -> b
treeFold _ base Leaf = base
treeFold f base (Node x left right) = f x (treeFold f base left) (treeFold f base right)

count :: BinTree a -> Integer
count = treeFold (\_ l r -> 1 + l + r) 0

height :: BinTree a -> Integer
height = treeFold (\_ l r -> 1 + max l r) 0

insert :: Ord a => BinTree a -> a -> BinTree a
insert Leaf x = Node x Leaf Leaf
insert (Node y left right) x
    | x < y = Node y (insert left x) right
    | otherwise = Node y left (insert right x)

isOrderedBounds :: Ord a => (Maybe a, Maybe a) -> BinTree a -> Bool
isOrderedBounds _ Leaf = True
isOrderedBounds bounds@(lo, hi) (Node x left right) =
    isWithin bounds x && isOrderedBounds (lo, Just x) left && isOrderedBounds (Just x, hi) right

isOrdered :: Ord a => BinTree a -> Bool
isOrdered = isOrderedBounds (Nothing, Nothing)

prop_insert :: Ord a => BinTree a -> a -> Bool
prop_insert tree x = 1 + count tree == count inserted && isOrdered inserted
    where inserted = insert tree x

go :: IO ()
go = do
    putStrLn "Starting:"
    quickCheck (withMaxSuccess 1000 (prop_insert :: BinTree Int -> Int -> Bool))

