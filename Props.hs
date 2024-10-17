{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Props (go) where

import Test.QuickCheck (Arbitrary (arbitrary), quickCheck, chooseInt, elements, oneof, Gen)
import Data.List (sort)
import Data.Maybe (maybe)
import Text.JSON (encode, decode, toJSObject, JSON, JSValue (JSArray, JSString, JSObject), JSObject, fromJSObject, Result (Ok, Error), toJSString, readJSValue)
import Data.Map (Map, fromList, assocs, empty)

-- Problem 1: Properties for lists

prop_sortPreservesLength :: Ord a => [a] -> Bool
prop_sortPreservesLength l = length l == (length . sort) l

prop_addMaintainsSize :: a -> [a] -> Bool
prop_addMaintainsSize x xs = length xs <= length (x:xs)



-- Problem 2: Property for students to JSON and back again

data Student = Student { name :: String, interests :: [String], address :: String } deriving Show

instance Arbitrary Student where
    arbitrary = Student <$> arbitrary <*> arbitrary <*> arbitrary

objToJson :: [(String, JSValue)] -> String
objToJson = encode . toJSObject

jsonToObj :: String -> [(String, JSValue)]
jsonToObj str = case fromJSObject <$> decode str of
    Ok m -> m
    Error e -> []

prop_jsonRoundtrip :: [(String, JSValue)] -> Bool
prop_jsonRoundtrip m = m == (jsonToObj . objToJson) m

prop_jsonRoundtripStudent :: [(String, Student)] -> Bool
prop_jsonRoundtripStudent students = prop_jsonRoundtrip (map (\(id, info) -> (id, JSObject $ studentObj info)) students)
    where mkStr = JSString . toJSString
          studentObj s = 
            toJSObject [ ("name", mkStr $ name s), ("interests", JSArray . map mkStr $ interests s), ("address",  mkStr $ address s)] :: JSObject JSValue



-- Problem 3: Properties for color packing and unpacking

data Color = Red | Green | Blue | RGB Int Int Int | CMYK Int Int Int Int deriving (Show, Eq)
newtype PackedColor = PackedColor [Int] deriving (Show, Eq)

instance Arbitrary Color where
    arbitrary = oneof [genCol, genRGB, genCMYK]
        where genCol = elements [Red, Green, Blue]
              gen255 = chooseInt (0, 255)
              genRGB = RGB <$> gen255 <*> gen255 <*> gen255
              genCMYK = CMYK <$> gen255 <*> gen255 <*> gen255 <*> gen255

instance Arbitrary PackedColor where
    arbitrary = PackedColor <$> oneof [genCol, genRGB, genCMYK, genJunk]
        where genCol = elements [[0], [1], [2]]
              gen255 = chooseInt (0, 255)
              genRGB = do r <- gen255
                          g <- gen255
                          b <- gen255
                          return [3,r,g,b]
              genCMYK = do c <- gen255
                           m <- gen255
                           y <- gen255
                           k <- gen255
                           return [4,c,m,y,k]
              genJunk = arbitrary :: Gen [Int]

packColor :: Color -> PackedColor
packColor = PackedColor . \case
    Red -> [0]
    Green -> [1]
    Blue -> [2]
    RGB r g b -> [3,r,g,b]
    CMYK c m y k -> [4,c,m,y,k]

unpackColor :: PackedColor -> Maybe Color
unpackColor (PackedColor l) = case l of
    [0] -> Just Red
    [1] -> Just Green
    [2] -> Just Blue
    [3,r,g,b] -> Just $ RGB r g b
    [4,c,m,y,k] -> Just $ CMYK c m y k
    _ -> Nothing

prop_colorPackUnpack :: Color -> Bool
prop_colorPackUnpack c = Just c == (unpackColor . packColor) c

prop_colorUnpackPack :: PackedColor -> Bool
prop_colorUnpackPack c = maybe True ((c ==) . packColor) $ unpackColor c



-- Main method

go :: IO  ()
go = do
    quickCheck (prop_sortPreservesLength :: [Int] -> Bool)
    quickCheck (prop_addMaintainsSize :: Int -> [Int] -> Bool)
    quickCheck prop_colorPackUnpack
    quickCheck prop_colorUnpackPack
    quickCheck prop_jsonRoundtripStudent