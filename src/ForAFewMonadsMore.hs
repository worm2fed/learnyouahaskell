module ForAFewMonadsMore where

import           AFirstfulOfMonads    (Birds, KnightPos, Pole, moveKnight)
import           Control.Monad        (ap, liftM)
import           Control.Monad.State  (MonadState (get, put, state), State, ap,
                                       filterM, foldM, liftM, when, (<=<))
import           Control.Monad.Writer (MonadWriter (tell, writer), Sum (Sum),
                                       Writer)
import           Data.Monoid          (Sum (Sum))
import           Data.Ratio           ((%))
import           System.Random        (Random (random), RandomGen, StdGen)


isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f =
    let (y, newLog) = f x
    in  (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- instance (Monoid w) => Functor (Writer w)  where
--     fmap = liftM

-- instance (Monoid w) => Applicative (Writer w)  where
--     pure x = Writer (x, mempty)
--     (<*>) = ap

-- instance (Monoid w) => Monad (Writer w) where
--     Writer (x, v) >>= f =
--         let Writer (y, v') = f x
--         in  Writer (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return $ a * b

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0    = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b $ a `mod` b

-- it's slow because every time it wants to add the right part to the left
-- part, it has to construct the left part all the way from the beginning!
-- ((((a ++ b) ++ c) ++ d) ++ e) ++ f
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0    = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b $ a `mod` b
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    DiffList f <> DiffList g = DiffList $ \xs -> f $ g xs

instance Monoid (DiffList a) where
    mempty = DiffList $ \xs -> [] ++ xs

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
    | b == 0    = do
        tell $ toDiffList ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse' b $ a `mod` b
        tell $ toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do
    tell ["0"]
finalCountDown x = do
    finalCountDown $ x - 1
    tell [show x]

finalCountDown' :: Int -> Writer (DiffList String) ()
finalCountDown' 0 = do
    tell $ toDiffList ["0"]
finalCountDown' x = do
    finalCountDown' $ x - 1
    tell $ toDiffList [show x]


-- instance Functor ((->) r) where
--     fmap = (.)

-- instance Applicative ((->) r) where
--     pure = const
--     (<*>) f g x = f x (g x)

-- instance Monad ((->) r) where
--     f >>= k = \r -> k (f r) r

addStuff, addStuff' :: Int -> Int
addStuff = do
    a <- (* 2)
    b <- (+ 10)
    return $ a + b
addStuff' x =
    let a = (* 2) x
        b = (+ 10) x
    in  a + b


threeCoins' :: StdGen -> (Bool, Bool, Bool )
threeCoins' gen =
    let (firstCoin, newGen)   = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

type Stack = [Int]

pop' :: Stack -> (Int, Stack)
pop' (x:xs) = (x, xs)

push' :: Int -> Stack -> ((), Stack)
push' a xs = ((), a:xs)

stackManip' :: Stack -> (Int, Stack)
stackManip' stack =
    let ((), newStack1) = push' 3 stack
        (a,  newStack2) = pop' newStack1
    in  pop' newStack2

-- newtype State s a = State { runState :: s -> (a, s) }

-- instance Functor (State s)  where
--     fmap = liftM

-- instance Applicative (State s)  where
--     pure x = State $ \s -> (x, s)
--     (<*>) = ap

-- instance Monad (State s) where
--     State h >>= f = State $ \s ->
--         let (a, newState) = h s
--             (State g) = f a
--         in  g newState

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    when (a == 100)
        stackStuff

-- get = State $ \s -> (s, s)
-- put newState = State $ \s -> ((), newState)

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1, 2, 3]
        then put [8, 3, 1]
        else put [9, 2, 1]

-- random :: (RandomGen g, Random a) => g -> (a, g)
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)


-- instance Applicative (Either e) where
--     pure          = Right
--     Left  e <*> _ = Left e
--     Right f <*> r = fmap f r

-- instance Monad (Either e) where
--     Left err >>= _ = Left err
--     Right  r >>= k = k r

landLeftE, landRightE :: Birds -> Pole -> Either String Pole
landLeftE  n (left, right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                    = ops (left + n, right)
landRightE n (left, right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise                    = ops (left, right + n)

ops :: Pole -> Either String Pole
ops (left, right) = Left $ "there are " ++ show left ++
             " birds at left side and " ++ show right ++
             " at the right one"

routineE :: Either String Pole
-- return (0, 0) >>= landLeft 2 >>= landRight 2 >>= landLeft 1
routineE = do
    let start = (0, 0)
    first <- landLeftE 2 start
    second <- landRightE 2 first
    landLeftE 1 second


-- fmap :: (Functor f) => (a -> b) -> f a -> f b

-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f m = m >>= \x -> return $ f x
-- liftM f m = do
--     x <- m
--     return $ f x

-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

-- ap :: (Monad m) => m (a -> b) -> m a -> m b
-- ap mf m = do
--     f <- mf
--     x <- m
--     return $ f x

-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f x y = f <$> x <*> y

-- liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
-- liftM2 f x y = f `liftM` x `ap` y

-- join :: (Monad m) => m (m a) -> m a
-- join mm = do
--     m <- mm
--     m

joinedMaybes :: Maybe Int
joinedMaybes = do
    m <- Just (Just 8)
    m

-- m >>= f = join $ fmap f m

-- filter :: (a -> Bool) -> [a] -> [a]

-- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

powerset :: [a] -> [[a]]
powerset = filterM (\_ -> [True, False])

-- foldl :: Foldable t => (a -> b -> a) -> a -> t b -> a

-- foldM :: (Foldable t, Monad m) => (a -> b -> m a) -> a -> t b -> m a

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9     = Nothing
    | otherwise = Just $ acc + x

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
    [ (x, "") ] -> Just x
    _           -> Nothing

foldRPN :: [Double] -> String -> Maybe [Double]
foldRPN (x:y:ys) "*"    = return $ (x * y) : ys
foldRPN (x:y:ys) "+"    = return $ (x + y) : ys
foldRPN (x:y:ys) "-"    = return $ (y - x) : ys
foldRPN xs numberString = liftM (: xs) $ readMaybe numberString

solveRPN' :: String -> Maybe Double
solveRPN' st = do
    [result] <- foldM foldRPN [] $ words st
    return result

inMany :: Int -> KnightPos -> [KnightPos]
-- inMany n start = return start >>= foldr (<=<) return (replicate n moveKnight)
inMany n = foldr (<=<) return $ replicate n moveKnight

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn n start end = end `elem` inMany n start


newtype Prob a = Prob { getProb :: [ (a, Rational) ] }
    deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [ (Prob [ ('a', 1%2), ('b', 1%2) ], 1%4)
    , (Prob [ ('c', 1%2), ('d', 1%2) ], 3%4)
    ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concatMap multAll xs
    where multAll (Prob inner, p) = map (\(x, r) -> (x, p * r)) inner

instance Monad Prob where
    return x = Prob [ (x, 1%1) ]
    m >>= f = flatten $ fmap f m

instance Applicative Prob where
    pure = return
    (<*>) = ap

data Coin = Heads | Tails
    deriving (Show, Eq)

coin, loadedCoin :: Prob Coin
coin       = Prob [ (Heads, 1%2),  (Tails, 1%2) ]
loadedCoin = Prob [ (Heads, 1%10), (Tails, 9%10) ]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return $ all (== Tails) [a, b, c]


joinSame :: [(Bool, Rational)] -> (Rational, Rational)
joinSame = foldl (
        \(t, f) (x, prob) -> if x
            then (t + prob, f)
            else (t, f + prob)
    ) (0, 0)
