module Zippers where

import           AFirstfulOfMonads               ((-:))
import           MakingOurOwnTypesAndTypeclasses (Tree (..))


-- data Tree a = EmptyTree | Node a (Tree a) (Tree a)
--     deriving (Show)

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' EmptyTree EmptyTree)
                (Node 'T' EmptyTree EmptyTree)
            )
            (Node 'Y'
                (Node 'S' EmptyTree EmptyTree)
                (Node 'A' EmptyTree EmptyTree)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' EmptyTree EmptyTree)
                (Node 'R' EmptyTree EmptyTree)
            )
            (Node 'A'
                (Node 'A' EmptyTree EmptyTree)
                (Node 'C' EmptyTree EmptyTree)
            )
        )

changeToP' :: Tree Char -> Tree Char
changeToP' (Node x l (Node y (Node _ m n) r)) =
    Node x l (Node y (Node 'P' m n) r)

data Direction = L | R
    deriving Show
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r)     = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _)     = x


-- type Breadcrumbs = [Direction]

-- goLeft, goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
-- goLeft  (Node _ l _, bs) = (l, L:bs)
-- goRight (Node _ _ r, bs) = (r, R:bs)

-- x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a)
    deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft, goRight, goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft  (Node x l r, bs) = (l, LeftCrumb  x r:bs)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp (t, LeftCrumb x r:bs)  = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs)     = (Node (f x) l r, bs)
modify f empty@(EmptyTree, _) = empty

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost t@(_, []) = t
topMost z         = topMost $ goUp z


-- data List a = Empty | Cons a (List a)
--     deriving (Show, Read, Eq, Ord)

type ListZipper a = ([a], [a])

goForward, goBack :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)
goBack (xs, b:bs) = (b:xs, bs)


type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem]
    deriving (Show)

myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem]
    deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name before after:bs) =
    (Folder name (before ++ [item] ++ after), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (before, item:after) = break (nameIs name) items
    in  (item, FSCrumb folderName before after:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _)     = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs)     = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)


goLeft', goRight', goUp' :: Zipper a -> Maybe (Zipper a)
goLeft' (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft' (EmptyTree, _)   = Nothing

goRight' (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight' (EmptyTree, _)   = Nothing

goUp' (t, LeftCrumb  x r:bs) = Just (Node x t r, bs)
goUp' (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp' (_, [])                = Nothing

newFocus  = (freeTree, []) -: goLeft -: goRight
newFocus' = return (freeTree, []) >>= goLeft' >>= goRight'

fsUp' :: FSZipper -> Maybe FSZipper
fsUp' (item, FSCrumb name before after:bs) =
    Just (Folder name (before ++ [item] ++ after), bs)
fsUp' (_, []) = Nothing

fsTo' :: Name -> FSZipper -> Maybe FSZipper
fsTo' name (Folder folderName items, bs) =
    let (before, after) = break (nameIs name) items
    in  if not $ null after
        then Just (head after, FSCrumb folderName before after:bs)
        else Nothing

fsRename' :: Name -> FSZipper -> Maybe FSZipper
fsRename' newName (Folder name items, bs) = Just (Folder newName items, bs)
fsRename' newName (File name dat, bs)     = Just (File newName dat, bs)

fsNewFile' :: FSItem -> FSZipper -> Maybe FSZipper
fsNewFile' item (Folder folderName items, bs) =
    Just (Folder folderName (item:items), bs)
fsNewFile' _ _ = Nothing
