{-# LANGUAGE LambdaCase #-}

import Data.List (transpose)

data Tile = Empty | X | O deriving (Show, Eq)
type Board = [[Tile]]


main :: IO ()
main = initGame >>= \case 
        (-1) -> putStrLn "Tie!"
        0    -> putStrLn "Player 1 wins!"
        1    -> putStrLn "Player 2 wins!"
        _    -> putStrLn "Error, a player that does not exist wins!"

initGame :: IO Int
initGame = do
    let b1 = replicate 4 $ replicate 4 Empty
    let b2 = insertPiece b1 (2,2) X
    let b3 = insertPiece b2 (2,3) O
    let b4 = insertPiece b3 (3,2) O
    let b = insertPiece b4 (3,3) X
    prettyPrint b
    initGame' b 0

    where
        initGame' :: Board -> Int -> IO Int
        initGame' b player = do
            let p1 = if player == 0 then X else O
            let p2 = if player == 0 then O else X
            if not (checkAnyMoves b p2) then do
                if checkAnyMoves b p1 then do 
                    putStrLn "Turn passes due to no legal moves"
                    initGame' b $ (player + 1) `mod` 2
                else return (checkWinner b)
            else do
                move <- makeTurn b player
                if checkFull move then return (checkWinner move) 
                else initGame' move $ (player + 1) `mod` 2
            

makeTurn :: Board -> Int -> IO Board 
makeTurn board player = do
    putStrLn "Enter desired position (format: x,y)"
    pos <- getLine
    let p1 = if player == 0 then X else O
    let p2 = if player == 0 then O else X
    let (x,y) = mapTup read $ splitOn ',' pos
    if x > length (head board) || y > length board || x < 1 || y < 1 then do
        putStrLn "Position does not exist"
        makeTurn board player
    else if (board !! (y-1)) !! (x-1) /= Empty then do
        putStrLn " Position is taken"
        makeTurn board player
    else if not (or(checkLines board p2 (x,y))) then do
        putStrLn " Position does not flip any enemy tiles"
        makeTurn board player
    else do
        let b = changeLines board p1 p2 (x,y)
        prettyPrint b
        return b


checkWinner :: Board -> Int
checkWinner board 
    | sum (map count (map (map (\x -> x == O)) board)) > sum (map count (map (map (\x -> x == X)) board)) = 1
    | sum (map count (map (map (\x -> x == X)) board)) > sum (map count (map (map (\x -> x == O)) board)) = 0
    | otherwise                                                                                           = (-1)

checkMove:: Board -> Tile -> (Int, Int) -> Bool
checkMove b t (x,y) = not (x > length (head b) || y > length b || x < 1 || y < 1 || (b !! (y-1)) !! (x-1) /= Empty || not (or(checkLines b t (x,y))))

checkAnyMoves:: Board -> Tile -> Bool
checkAnyMoves b t = checkAnyMoves' b t (1,1)
    where
        checkAnyMoves':: Board -> Tile -> (Int, Int) -> Bool
        checkAnyMoves' board tile (x,y)
            | (x,y) == ((length (board!!0)), (length board)) =      checkMove board tile (x,y)
            | (x,y) == ((length (board!!0)), y)                = or [(checkMove board tile (x,y)), (checkAnyMoves' board tile (0  , y+1))]
            | otherwise                                          = or [(checkMove board tile (x,y)), (checkAnyMoves' board tile (x+1, y  ))]

count:: [Bool] -> Int
count list = count' list 0
    where 
        count'::[Bool] -> Int -> Int
        count' [] c = c
        count' (True:xs) c = count' xs (c+1)
        count' (x:xs) c = count' xs c

checkFull :: Board -> Bool
checkFull = all (notElem Empty)

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (a,b) = (f a, f b)

splitOn :: Char -> String -> (String,String)
splitOn c str = (takeWhile (/=c) str, tail $ dropWhile (/=c) str)

changeLines :: Board -> Tile -> Tile -> (Int, Int) -> Board
changeLines board p1tile p2tile pos = insertPiece (changeLines' board p1tile p2tile pos (-1,-1) (checkLines board p2tile pos)) pos p1tile
    where
        changeLines':: Board -> Tile -> Tile -> (Int, Int) -> (Int, Int) -> [Bool] -> Board
        changeLines' board p1tile p2tile pos (0,0) bs     = changeLines' board p1tile p2tile pos (1,0) bs
        changeLines' board p1tile p2tile pos (1,1) (b:[]) = if b == True then changeLine board p1tile p2tile pos (1,1) else board
        changeLines' board p1tile p2tile pos (1,y) (b:bs) = if b == True then changeLines' (changeLine board p1tile p2tile pos (1,y)) p1tile p2tile pos ((-1) ,(y+1)) bs else changeLines' board p1tile p2tile pos ((-1) ,(y+1)) bs
        changeLines' board p1tile p2tile pos (x,y) (b:bs) = if b == True then changeLines' (changeLine board p1tile p2tile pos (x,y)) p1tile p2tile pos ((x+1), y) bs else changeLines' board p1tile p2tile pos ((x+1), y) bs

checkLines :: Board -> Tile -> (Int, Int) -> [Bool]
checkLines board tile (x,y) = checkLines' board tile ((x-1) ,(y-1)) (-1,-1)
    where
        checkLines':: Board -> Tile -> (Int, Int) -> (Int, Int) -> [Bool]
        checkLines' board tile pos (0,0) = checkLines' board tile pos (1,0) 
        checkLines' board tile pos (1,1) = [checkLine board tile pos (1,1) False]
        checkLines' board tile pos (1,y) = [checkLine board tile pos (1,y) False] ++ checkLines' board tile pos ((-1) ,(y+1))
        checkLines' board tile pos (x,y) = [checkLine board tile pos (x,y) False] ++ checkLines' board tile pos ((x+1),y)

checkLine :: Board -> Tile -> (Int, Int) -> (Int, Int) -> Bool -> Bool
checkLine board tile (posX, posY) (dirX, dirY) check 
    | 0 > (posY+dirY) || 0 > (posX+dirX) || (posX+dirX) >= (length (board!!0)) || (posY+dirY) >= (length board) = False
    | board !! (posY+dirY) !! (posX+dirX) == tile  = checkLine board tile ((posX + dirX), (posY + dirY)) (dirX, dirY) True
    | board !! (posY+dirY) !! (posX+dirX) == Empty = False
    | otherwise = check

changeLine :: Board -> Tile -> Tile -> (Int, Int) -> (Int, Int) -> Board
changeLine board p1tile p2tile (posX, posY) (dirX, dirY) 
    | 0 >= (posY+dirY) || 0 >= (posX+dirX) || (posX+dirX) > (length (board!!0)) || (posY+dirY) > (length board) = board
    | board !! (posY-1) !! (posX-1) == p2tile = changeLine (insertPiece board (posX, posY) p1tile) p1tile p2tile ((posX + dirX), (posY + dirY)) (dirX, dirY)
    | board !! (posY-1) !! (posX-1) == Empty = changeLine board p1tile p2tile ((posX + dirX), (posY + dirY)) (dirX, dirY)
    | otherwise = board

insertPiece:: Board -> (Int, Int) -> Tile -> Board
insertPiece b (x,y) t = (take (y-1) b) ++ [(take (x-1) (b!!(y-1))) ++ [t] ++ (drop x (b!!(y-1)))] ++ (drop y b)

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
                
