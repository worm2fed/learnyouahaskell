module MakingOurOwnTypesAndTypeclasses where

import qualified Data.Map as Map


data Person = Person
    { firstName   :: String
    , lastName    :: String
    , age         :: Int
    , height      :: Float
    , phoneNumber :: String
    , flavor      :: String
    } deriving (Show)


data Car a b c = Car
    { company :: a
    , model   :: b
    , year    :: c
    } deriving (Show)

tellCar :: (Show a) => Car String String a -> String
tellCar Car { company = c, model = m, year = y } =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


data Vector a = Vector a a a
    deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n


data Person' = Person'
    { firstName' :: String
    , lastName'  :: String
    , age'       :: Int
    } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)


type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name, number) `elem` book

type AssocList k v = [(k, v)]

type IntMap = Map.Map Int


data LockerState = Taken | Free
    deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number map =
    case Map.lookup number map of
        Nothing -> Left $ "Locker number " ++ show number ++ " doesn't exist"
        Just (state, code) -> if state /= Taken
                              then Right code
                              else Left $ "Locker " ++ show number ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]


infixr 5 :-:
data List a = Empty | a :-: List a
    deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys      = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right


-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool

--     x == y = not $ x /= y
--     x /= y = not $ x == y

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red    == Red    = True
    Green  == Green  = True
    Yellow == Yellow = True
    _      == _      = False

instance Show TrafficLight where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"

-- instance (Eq m) => Eq (Maybe m) where
--     Just x  == Just y  = x == y
--     Nothing == Nothing = True
--     _       == _       = False


class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing  = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _         = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _   = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal then yesResult else noResult


-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--     fmap = map

-- instance Functor Maybe where
--     fmap f (Just x) = Just $ f x
--     fmap _ Nothing  = Nothing

instance Functor Tree where
    fmap _ EmptyTree           = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- instance Functor (Either a) where
--     fmap f (Right x) = Right $ f x
--     fmap _ left      = left


class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank { frankField :: b a }
    deriving (Show)

instance Tofu Frank where
    tofu x = Frank x

data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance Functor (Barry a b) where
    fmap f (Barry x y) = Barry (f x) y
