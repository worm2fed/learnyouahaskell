module InputAndOutput where

import           Control.Monad        (forM, forever, unless, when)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as B
import           Data.Char            (toUpper)
import           Data.List            (delete)
import           System.Directory     (doesFileExist, removeFile, renameFile)
import           System.Environment   (getArgs, getProgName)
import           System.IO            (BufferMode (BlockBuffering), Handle,
                                       IOMode (ReadMode), hClose, hGetContents,
                                       hPutStr, hSetBuffering, openFile,
                                       openTempFile, withFile)
import           System.IO.Error
import           System.Random        (Random (random, randomR, randomRs),
                                       RandomGen, StdGen, getStdGen, newStdGen)


main0 = putStrLn "hello world"

main1 = do
    putStrLn "Hey, what's your name?"
    name <- getLine
    putStrLn $ "hey " ++ name ++ ", you rock!"

main2 = do
    putStrLn "What's your name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", you rock!"

main3 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main3

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main4 = do
    return ()
    return "haha"
    line <- getLine
    return "blah blah"
    return 4
    putStrLn line

main5 = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

main6 =
    do
        let a = "hell"
            b = "yeah!"
        putStrLn $ a ++ " " ++ b

main7 = do
    putStr "Hey, "
    putStr "I'm "
    putStrLn "Andy!"

main8 = do
    putChar 't'
    putChar 'e'
    putChar 'h'

-- putStr :: String -> IO ()
-- putStr []     = return ()
-- putStr (x:xs) = do
--     putChar x
--     putStr xs

main9 = do
    print True
    print 2
    print "haha"
    print 3.2
    print [3, 4, 3]

main10 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main10
        else return ()

main11 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main11

main12 = do
    -- a <- getLine
    -- b <- getLine
    -- c <- getLine
    -- print [a, b, c]
    rs <- sequence [getLine, getLine, getLine]
    print rs

main13 = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

main14 = do
    colors <- forM [1 .. 4] (\a -> do
        putStrLn $
            "Which color do you associate with the number " ++ show a ++ "?"
        getLine)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors


main15 = do
    contents <- getContents
    putStr $ map toUpper contents

main16 = do
    contents <- getContents
    putStr $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
    in  unlines shortLines

main17 = interact shortLinesOnly
-- main17 = interact $ unlines . filter ((< 10) . length) . lines

main18 = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map (
        \xs -> if isPalindrome xs then "palindrome" else "not palindrome"
    ) . lines
    where isPalindrome xs = xs == reverse xs

main19 = do
    handle <- openFile "assets/girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

main20 = withFile "assets/girlfriend.txt" ReadMode (
    \handle -> do
        contents <- hGetContents handle
        putStr contents
    )

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

main21 = do
    contents <- readFile "assets/girlfriend.txt"
    putStr contents

main22 = do
    contents <- readFile "assets/girlfriend.txt"
    writeFile "assets/girlfriendcaps.txt" $ map toUpper contents

main23 = do
    todoItem <- getLine
    appendFile "assets/todo.txt" $ todoItem ++ "\n"

main24 = do
    withFile "assets/girlfriend.txt" ReadMode (
        \handle -> do
            hSetBuffering handle $ BlockBuffering $ Just 2048
            contents <- hGetContents handle
            putStr contents
        )

main25 = do
    handle <- openFile "assets/todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "assets/" "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (
                \n line -> show n ++ " - " ++ line
            ) [0 ..] todoTasks
    putStrLn "These are your TODD items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "assets/todo.txt"
    renameFile tempName "assets/todo.txt"


main26 = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM_ putStrLn args
    putStrLn "The program name is:"
    putStrLn progName

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)
           ]

add, view, remove, bump :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _                    = errorExit

view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (
                \n line -> show n ++ " - " ++ line
            ) [0 ..] todoTasks
    putStr $ unlines numberedTasks
view _ = errorExit

remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
remove _ = errorExit

bump [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        bumpedTask = todoTasks !! number
        newTodoItems = bumpedTask : delete bumpedTask todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

errorExit :: IO ()
errorExit = putStrLn "Ops... check your arguments"

main27 = do
    (command:args) <- getArgs
    case lookup command dispatch of
        (Just action) -> action args
        Nothing       -> errorExit


threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, _) = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen
    in (value:restOfList, finalGen)

main28 = do
    gen <- getStdGen
    putStr $ take 20 $ randomRs ('a', 'z') gen

main29 = do
    gen <- getStdGen
    putStrLn $ take 20 $ randomRs ('a', 'z') gen
    gen2 <- getStdGen
    putStr $ take 20 $ randomRs ('a', 'z') gen2

main30 = do
    gen <- getStdGen
    let randomChars = randomRs ('a', 'z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStrLn first20
    putStr second20

main31 = do
    gen <- getStdGen
    putStrLn $ take 20 $ randomRs ('a', 'z') gen
    gen' <- newStdGen
    putStr $ take 20 $ randomRs ('a', 'z') gen'

main32 = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    unless (null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        askForNumber newGen

main33 = do
    gen <- getStdGen
    let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    unless (null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        newStdGen
        main33


main34 = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- B.readFile source
    B.writeFile dest contents


main35 = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The file has " ++ show (length $ lines contents) ++ " lines!"

main36 = do
    (fileName:_) <- getArgs
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            contents <- readFile fileName
            putStrLn $
                "The file has " ++ show (length $ lines contents) ++ " lines!"
        else do
            putStrLn "The file doesn't exist!"

main37 = main35 `catchIOError` handler

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = case ioeGetFileName e of
        Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
        Nothing   -> putStrLn "Whoops! File does not exist at unknown location"
    | otherwise = ioError e

main = main37
