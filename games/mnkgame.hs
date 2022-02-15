{-# LANGUAGE LambdaCase #-}
import Data.List (transpose)
-- m,n,k-game
-- A generalised version of tic-tac-toe (which is a 3,3,3-game).
-- m*n board, k in a row

data Tile = Empty | X | O deriving (Show, Eq)
type Board = [[Tile]]

main :: IO ()
main = initGame 3 3 3 >>= \case 
        (-1) -> putStrLn "Tie!"
        0    -> putStrLn "Player 1 wins!"
        1    -> putStrLn "Player 2 wins!"
        _    -> putStrLn "Error, a player that does not exist wins!"

initGame :: Int -> Int -> Int -> IO Int
initGame m n k = do
    let b = replicate m $ replicate n Empty
    initGame' b k 0

    where
        initGame' :: Board -> Int -> Int -> IO Int
        initGame' b k player = do
            move <- makeTurn b player
            if checkWinner move k then return player else 
                if checkTie move then return (-1) else
                initGame' move k $ (player + 1) `mod` 2

makeTurn :: Board -> Int -> IO Board 
makeTurn board player = undefined 

checkWinner :: Board -> Int -> Bool
checkWinner = undefined  
    -- where

getRows :: Board -> Int -> [[Tile]]
getRows b len = concat [[take len (drop n r) | n <- [0..(length r - len)]] | r <- b]

getColumns :: Board -> Int -> [[Tile]]
getColumns b = getRows (transpose b)  

getDiagonals1 :: Board -> Int -> [[Tile]]
getDiagonals1 b len = getRows [
    [(b !! r) !! c | k <- [0..(length.head) b - len], let c = r + k] 
    | r <- [0..length b - len]] len

getDiagonals2 :: Board -> Int -> [[Tile]]
getDiagonals2 b = getDiagonals1 (transpose b) 

checkTie :: Board -> Bool
checkTie = all (notElem Empty)


-------------------

prettyPrint :: Board -> IO ()
prettyPrint b = do
    putStrLn $ replicate (1 + 4 * length (head b)) '-'
    prettyPrint' $ map (map f) b

    where
        f :: Tile -> String
        f t = case t of
            Empty -> " "
            s     -> show s

        prettyPrint' :: [[String]] -> IO ()
        prettyPrint' [] = return ()
        prettyPrint' (b:bs)  = do
            putStrLn $ foldl (\s t -> s ++ t ++ " | ") "| " b
            putStrLn $ replicate (1 + 4 * length b) '-'
            prettyPrint' bs
                

------------------------------------------

testBoard :: Board
testBoard = replicate 10 $ replicate 10 Empty

testBoard2 :: Board
testBoard2 = [[Empty, X, O, O], [X, X, O, X], [Empty, Empty, O, X]]

testBoard3 :: Board
testBoard3 = [[X, X, X], [O, X, O], [X, O, O]]

testBoard4 :: Board
testBoard4 = [[X, O], [Empty, Empty], [O,X], [X,X]]

test :: [[Tile]]
test = getRows testBoard2 3

test2 :: Bool
test2 = checkTie testBoard3

test3 :: [[Tile]]
test3 = getColumns testBoard2 2

test4 :: [[Tile]]
test4 = getDiagonals2 testBoard2 2