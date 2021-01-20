module AFirstfulOfMonads where

import           Data.List (intersect, nub)
import           GHC.Base  (MonadPlus (..))


-- fmap :: (Functor f) => (a -> b) -> f a -> f b
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _  = Nothing
applyMaybe (Just x) f = f x


-- class (Applicative m) => Monad m where
--     return :: a -> m a

--     (>>=) :: m a -> (a -> m b) -> m b

--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y

--     fail :: String -> m a
--     fail msg = error msg

-- instance Monad Maybe where
--     Nothing >>= _ = Nothing
--     Just x >>= f  = f x


type Birds = Int
type Pole = (Birds, Birds)

landLeft', landRight' :: Birds -> Pole -> Pole
landLeft'  n (left, right) = (left + n, right)
landRight' n (left, right) = (left, right + n)

x -: f = f x

landLeft, landRight :: Birds -> Pole -> Maybe Pole
landLeft  n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

routine :: Maybe Pole
routine = case landLeft 1 (0, 0) of
    Nothing    -> Nothing
    Just start -> case landRight 4 start of
        Nothing    -> Nothing
        Just first -> case landLeft 2 first of
            Nothing     -> Nothing
            Just second -> landLeft 1 second


foo, foo' :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just $ show x ++ y))
foo' = do
    x <- Just 3
    y <- Just "!"
    Just $ show x ++ y

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just $ x > 8

routine', routine'' :: Maybe Pole
-- return (0, 0) >>= landLeft 2 >>= landRight 2 >>= landLeft 1
routine' = do
    start <- return (0, 0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second
-- return (0, 0) >>= landLeft 2 >> Nothing >>= landRight 2 >>= landLeft 1
routine'' = do
    start <- return (0, 0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second

justH :: Maybe Char
justH = do
    (x:_) <- Just "hello"
    return x

-- class Monad m => MonadFail (m :: * -> *) where
--     fail :: String -> m a

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x


-- instance Monad [] where
--      xs >>= f = concat $ map f xs

listOfTuples :: [(Int, Char)]
-- [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)
listOfTuples = do
    n <- [1, 2]
    ch <- ['a', 'b']
    return (n, ch)
-- [ (n, ch) | n <- [1, 2], ch <- ['a', 'b'] ]

-- class (Alternative m, Monad m) => MonadPlus m where
--     mzero :: m a
--     mplus :: m a -> m a -> m a

-- instance MonadPlus [] where
--     mzero = []
--     mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True  = return ()
guard False = mzero

sevensOnly :: [Int]
-- [ x | x <- [1..50], '7' `elem` show x ]
-- [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
sevensOnly = do
    x <- [1..50]
    guard $ '7' `elem` show x
    return x

type KnightPos = (Int, Int)

moveKnight, moveKnight' :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [ (c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1)
                , (c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2)
                ]
    guard $ c' `elem` [1..8] && r' `elem` [1..8]
    return (c', r')
moveKnight' (c, r) = filter onBoard
    [ (c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1)
    , (c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2)
    ]
    where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]

in3, in3' :: KnightPos -> [KnightPos]
in3 start = do
    first  <- moveKnight start
    second <- moveKnight first
    moveKnight second
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

--  As an exercise, you can change this function so that when you can reach one position from the other, it tells you which moves to take
-- movesToReachIn3 :: KnightPos -> KnightPos -> [KnightPos]
-- movesToReachIn3 start end =
--     if canReachIn3 start end
--         then nub $ (moveKnight start >>= moveKnight) `intersect` moveKnight end
--         else []


-- (<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
-- f <=< g = \x -> g x >>= f
