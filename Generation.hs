module Generation (go) where

import Test.QuickCheck
import Data.List (sort, singleton)



newtype OrderedList1 a = OrderedList1 [a] deriving Show
newtype OrderedList2 a = OrderedList2 [a] deriving Show
newtype OrderedList3 a = OrderedList3 [a] deriving Show

-- OrderedList1

instance (Arbitrary a, Ord a) => Arbitrary (OrderedList1 a) where
    arbitrary = fmap (OrderedList1 . sort) arbitrary

-- OrderedList2

insertOrdered' :: Ord a => a -> [a] -> [a]
insertOrdered' x [] = [x]
insertOrdered' x (y:ys) | x < y = x:y:ys
                        | otherwise = y:insertOrdered' x ys

insertOrdered :: Ord a => a -> OrderedList2 a -> OrderedList2 a
insertOrdered x (OrderedList2 l) = OrderedList2 $ insertOrdered' x l

instance (Arbitrary a, Ord a) => Arbitrary (OrderedList2 a) where
    arbitrary = sized make
        where make 0 = return (OrderedList2 [])
              make size = insertOrdered <$> arbitrary <*> resize (size - 1) arbitrary

-- OrderedList3

instance (Arbitrary a, Ord a) => Arbitrary (OrderedList3 a) where
    arbitrary = sized make
        where make 0 = return (OrderedList3 [])
              make size = do
                rest <- resize (size - 1) arbitrary
                case rest of
                    (OrderedList3 []) -> fmap (OrderedList3 . singleton) arbitrary
                    (OrderedList3 (x:xs)) -> fmap (OrderedList3 . maybe [] (:x:xs)) (suchThatMaybe arbitrary (< x))


go :: IO ()
go = do
    generate (arbitrary :: Gen (OrderedList1 Int)) >>= print
    generate (resize 100 (arbitrary :: Gen (OrderedList1 Int))) >>= print
    -- generate (arbitrary :: Gen (OrderedList2 Int)) >>= print
    -- generate (arbitrary :: Gen (OrderedList3 Int)) >>= print
    -- generate (arbitrary :: Gen (OrderedList1 String)) >>= print
    -- generate (arbitrary :: Gen (OrderedList2 String)) >>= print
    -- generate (arbitrary :: Gen (OrderedList3 String)) >>= print
    -- generate (arbitrary :: Gen (OrderedList Int)) >>= print