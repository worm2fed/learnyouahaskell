module Modules where

import           Data.Char (chr, ord)
import           Data.List (nub, tails)
import qualified Data.Map  as Map
import qualified Data.Set  as Set


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> take nlen x == needle || acc) False $ tails haystack

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)


encode :: Int -> String -> String
encode shift msg =
    let ords    = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

decode :: Int -> String -> String
decode shift = encode $ negate shift


phoneBook :: [(String, String)]
phoneBook =
    [ ("betty","555-2938")
    , ("bonnie","452-2928")
    , ("patsy","493-2928")
    , ("lucille","205-2928")
    , ("wendy","939-8282")
    , ("penny","853-2492")
    ]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
-- findKey _   []          = Nothing
-- findKey key ((k, v):xs) = if key == k then Just v else findKey key xs
findKey key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

phoneBook' :: [(String, String)]
phoneBook' =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap =
    Map.fromListWith $ \number1 number2 -> number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs


text1 :: String
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 :: String
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

