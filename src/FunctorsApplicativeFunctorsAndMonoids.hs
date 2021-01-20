module FunctorsApplicativeFunctorsAndMonoids where

import           Control.Applicative             (ZipList (ZipList))
import           Data.Char                       (toUpper)
import           Data.List                       (intersperse)
import           MakingOurOwnTypesAndTypeclasses (Tree (..))


-- instance Functor IO where
--     fmap f action = do
--         result <- action
--         return $ f result

main1 = do
    line <- getLine
    let line' = reverse line
    putStrLn $ "You said " ++ line' ++ " backwards!"
    putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"

main2 = do
    line <- fmap reverse getLine
    putStrLn $ "You said " ++ line ++ " backwards!"
    putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

main3 = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line

-- instance Functor ((->) r) where
    -- fmap f g = \x -> f $ g x
    -- fmap = (.)

-- instance Functor Maybe where
--     fmap f (Just x) = Just $ f x
--     fmap _ Nothing  = Nothing

data CMaybe a = CNothing | CJust Int a
    deriving (Show)

instance Functor CMaybe where
    fmap _ CNothing          = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) $ f x


-- class (Functor f) => Applicative f where
--     pure  :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative Maybe where
--     pure = Just
--     Nothing <*> _         = Nothing
--     Just f  <*> something = fmap f something

-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- f <$> x = fmap f x

-- instance Applicative [] where
--     pure x = [x]
--     fs <*> xs = [f x | f <- fs, x <- xs]

-- instance Applicative IO where
--     pure = return
--     a <*> b = do
--         f <- a
--         x <- b
--         return $ f x

myAction, myAction' :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b
myAction' = (++) <$> getLine <*> getLine

main4 = do
    a <- myAction'
    putStrLn $ "The two lines concatenated turn out to be: " ++ a

-- instance Applicative ((->) r) where
--     pure x = \_ -> x
--     f <*> g = \x -> f x $ g x

-- instance Applicative ZipList where
--     pure x = ZipList $ repeat x
--     ZipList fs <*> ZipList xs = ZipList $ zipWith (\f x -> f x) fs xs

-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f a b = f <$> a <*> b

-- sequenceA :: (Applicative f) => [f a] -> f [a]
-- sequenceA []     = pure []
-- sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- sequenceA = foldr (liftA2 (:)) $ pure []


-- data ZipList a = ZipList [a]
-- data ZipList a = ZipList { getZipList :: [a] }
-- newtype ZipList a = ZipList { getZipList :: [a] }

data Profession = Fighter | Archer | Accountant

data Race = Human | Elf | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession

newtype CharList = CharList { getCharList :: [Char] }
    deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

newtype CoolBool' = CoolBool' { getCoolBool' :: Bool }

helloMe' :: CoolBool' -> [Char]
helloMe' (CoolBool' _) = "hello"


-- class Monoid m where
--     mempty :: m
--     mappend :: m -> m -> m
--     mconcat :: [m] -> m
--     mconcat = foldr mappend mempty

-- instance Monoid [a] where
--     mempty = []
--     mappend = (++)

newtype Product' a = Product' { getProduct' :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Semigroup (Product' a) where
    Product' x <> Product' y = Product' $ x * y

instance Num a => Monoid (Product' a) where
    mempty = Product' 1

newtype Sum' a = Sum' { getSum' :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Semigroup (Sum' a) where
    Sum' x <> Sum' y = Sum' $ x + y

instance Num a => Monoid (Sum' a) where
    mempty = Sum' 0

newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Any where
    Any x <> Any y = Any $ x || y

instance Monoid Any where
    mempty = Any False

newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup All where
    All x <> All y = All $ x && y

instance Monoid All where
    mempty = All True

-- instance Semigroup Ordering where
--     LT <> _ = LT
--     EQ <> y = y
--     GT <> _ = GT

-- instance Monoid Ordering where
--     mempty = EQ

lengthCompare, lengthCompare', lengthCompare'' :: String -> String -> Ordering
lengthCompare x y =
    let a = length x `compare` length y
        b = x `compare` y
    in  if a == EQ then b else a

lengthCompare' x y = (length x `compare` length y) `mappend` (x `compare` y)

lengthCompare'' x y = (length x `compare` length y) `mappend`
                      (vowels x `compare` vowels y) `mappend`
                      (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")

-- instance Semigroup a => Semigroup (Maybe a) where
--     Nothing <> m       = m
--     m <> Nothing       = m
--     Just m1 <> Just m2 = Just $ m1 <> m2

-- instance Monoid a => Monoid (Maybe a) where
--     mempty = Nothing

newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Semigroup (First a) where
    First (Just x) <> _ = First $ Just x
    First Nothing <> x  = x

instance Monoid (First a) where
    mempty = First Nothing

newtype Last a = Last { getLast :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Semigroup (Last a) where
    x <> Last Nothing = x
    _ <> y            = y

instance Monoid (Last a) where
    mempty = Last Nothing

instance Foldable Tree where
    -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = foldMap f l `mappend`
                             f x         `mappend`
                             foldMap f r

testTree = Node 5
                (Node 3
                    (Node 1 EmptyTree EmptyTree)
                    (Node 6 EmptyTree EmptyTree)
                )
                (Node 9
                    (Node 8 EmptyTree EmptyTree)
                    (Node 10 EmptyTree EmptyTree)
                )
