{-# LANGUAGE LambdaCase #-}

import Data.List (transpose)

-- m,n,k-game
-- A generalised version of tic-tac-toe (which is a 3,3,3-game).
-- m*n board, k in a row

{-
General patterns: Data types, board, functions for changing/initiating board,
function for determining victory, function for displaying game state
-}

data Tile = Empty | X | O deriving (Show, Eq)
type Board = [[Tile]]

main :: IO ()
main = initGame 10 6 2 >>= \case 
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
            prettyPrint b
            newBoard <- makeTurn b player
            if checkWinner newBoard k then prettyPrint newBoard >> return player else 
                if checkTie newBoard then prettyPrint newBoard >> return (-1) else
                initGame' newBoard k $ (player + 1) `mod` 2

makeTurn :: Board -> Int -> IO Board 
makeTurn board player = do
    putStrLn "Enter desired position (format: x,y)"
    pos <- getLine
    let (x,y) = mapTup read $ splitOn ',' pos
    if x > length (head board) - 1 || y > length board - 1 then do
        putStrLn "Position does not exist"
        makeTurn board player
    else if (board !! y) !! x /= Empty then do
        putStrLn " Position is taken"
        makeTurn board player
    else do
        putStrLn "test"
        let p = if player == 0 then X else O

        return [[if x == x' && y == y' then p else t 
                | x' <- [0..(length . head) board - 1],  let t = (board !! y') !! x']
                | y' <- [0..length board - 1]]


mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (a,b) = (f a, f b)

checkWinner :: Board -> Int -> Bool
checkWinner b k = do
    let everything = getRows b k ++ getColumns b k ++ getDiagonals b k
    any (\a -> allEQ a && Empty `notElem` a) everything

getRows :: Board -> Int -> [[Tile]]
getRows b len = concat [[take len (drop n r) | n <- [0..(length r - len)]] | r <- b]

getColumns :: Board -> Int -> [[Tile]]
getColumns b = getRows (transpose b)  

getDiagonals :: Board -> Int -> [[Tile]]
getDiagonals b k = getDiagonals' b k ++ getDiagonals' (map reverse b) k
    where
        getDiagonals' b k = concat [[[(b !! (y + k')) !! (x + k') | k' <- [0..k-1]]
                            | x <- [0..length (head b) - k]]
                            | y <- [0..length b - k]] 

checkTie :: Board -> Bool
checkTie = all (notElem Empty)

allEQ :: Eq a => [a] -> Bool
allEQ (a:as) = all (== a) as
allEQ _      = True


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
                
splitOn :: Char -> String -> (String,String)
splitOn c str = (takeWhile (/=c) str, tail $ dropWhile (/=c) str)
