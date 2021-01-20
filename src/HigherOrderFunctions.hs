module HigherOrderFunctions where


multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: Floating a => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f $ f x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []     _      = []
zipWith' _ _      []     = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y
-- flip' f = \x y -> f y x

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) =
  let smallerSorted = quicksort $ filter (<= x) xs
      biggerSorted  = quicksort $ filter (> x ) xs
  in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head $ filter p [100000, 99999 ..]
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length $ filter (\xs -> length xs > 15) $ map chain [1 .. 100]

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z
-- addThree x y z = x + y + z

sum', sum'' :: (Num a) => [a] -> a
sum' = foldl (+) 0
sum'' = foldl1 (+)

elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (\acc x -> x == y || acc) False

map'', map''' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []
map''' f = foldl (\acc x -> acc ++ [f x]) []

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 $ \x acc -> if x > acc then x else acc

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head', last' :: [a] -> a
head' = foldr1 $ \x _ -> x
last' = foldl1 $ \_ x -> x

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) $ scanl1 (+) $ map sqrt [1 .. ]) + 1

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f $ g x

oddSquareSum, oddSquareSum' :: Integer
oddSquareSum  = sum . takeWhile (< 10000) . filter odd . map (^2) $ [1 ..]
oddSquareSum' =
    let oddSquares  = filter odd $ map (^2) [1 ..]
        bellowLimit = takeWhile (< 10000) oddSquares
    in  sum bellowLimit
