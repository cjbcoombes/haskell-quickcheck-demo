module FullExample (go) where

import Test.QuickCheck
import Data.Maybe (isNothing, fromMaybe)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized gen
        where gen count = frequency [(1, return Leaf), (count, node (count `quot` 2))]
              node count = Node <$> arbitrary <*> gen count <*> gen count
    shrink Leaf = [Leaf]
    shrink (Node x l r) = [Node xs ls rs | xs <- shrink x, ls <- shrink l, rs <- shrink r]

treeMax :: Ord a => Tree a -> Maybe a
treeMax Leaf = Nothing
treeMax (Node x l r) = Just $ maybe x (max x) (max (treeMax l) (treeMax l))

prop_treeMax :: Ord a => Tree a -> a -> Bool
prop_treeMax Leaf _ = True
prop_treeMax tree new = fromMaybe False $
    do m <- treeMax tree
       nml <- treeMax (Node new tree Leaf)
       nmr <- treeMax (Node new Leaf tree)
       return $ max m new == nml && max m new == nmr

go :: IO ()
go = do
    quickCheck (withMaxSuccess 10000 (prop_treeMax :: Tree Int -> Int -> Bool))