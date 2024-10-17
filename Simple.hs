module Simple (go) where

import Test.QuickCheck

badReverse1 :: [a] -> [a]
badReverse1 = reverse . drop 1

badReverse2 :: [a] -> [a]
badReverse2 = id

prop_reverse_of :: Eq a => ([a] -> [a]) -> [a] -> Bool
prop_reverse_of revFunc l = id l == (revFunc . revFunc) l

go :: IO ()
go = do
    quickCheck (prop_reverse_of reverse :: [Int] -> Bool)
    quickCheck (prop_reverse_of reverse :: [String] -> Bool)
    quickCheck (prop_reverse_of badReverse1 :: [Int] -> Bool)
    quickCheck (prop_reverse_of badReverse1 :: [String] -> Bool)
    quickCheck (prop_reverse_of badReverse2 :: [Int] -> Bool)
    quickCheck (prop_reverse_of badReverse2 :: [String] -> Bool)