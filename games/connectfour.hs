module ConnectFour where

import Data.List (transpose, elemIndex, group)
import Data.List.Index


data Tile = Empty | Blue | Red deriving (Eq)

instance Show Tile where
    show Empty = " "
    show Red = "R"
    show Blue = "B"

type Col = [Tile]
type Board = [Col]


emptyBoard :: Board
emptyBoard = replicate 7 $ replicate 6 Empty

main :: IO()
main = do
    player <- play emptyBoard 0
    case player of
        -1 -> putStrLn "Tie"
        0 -> putStrLn "Blue wins"
        1 -> putStrLn "Red wins"
        _ -> error "???"

play :: Board -> Int -> IO Int
play b p = do
    prettyPrint b
    if tie b
        then return (-1)
    else if win b
        then return ((p + 1) `mod` 2)
    else
        do b' <- place b p
           play b' $ (p + 1) `mod` 2
    
    

place :: Board -> Int -> IO Board
place b p = do
    putStrLn "Enter desired column (1-7)"
    inp <- getLine
    let col = read inp - 1
    if validPlacement b col then do
        putStrLn "Invalid position, try again"
        place b p
    else do
        let i = firstEmpty $ b !! col
            newCol = setAt i (clr !! p) (b !! col) 
            newBoard = setAt col newCol b
        return newBoard

    where clr = [Blue, Red]


firstEmpty :: Col -> Int
firstEmpty col = case elemIndex Empty col of
    Just a  -> a
    Nothing -> error "Something went wrong :/"


validPlacement :: Board -> Int -> Bool 
validPlacement b col = col > 6 || col < 0 || 6 < length (b!!col)

getRow :: Board -> Int -> [Tile]
getRow b n = getRows b !! n 

-- | Return the board as a list of rows
getRows :: Board -> Board
getRows b = getColumns (transpose b)

getColumn :: Board -> Int -> [Tile]
getColumn b n = getColumns b !! n

getColumns :: Board -> Board
getColumns b = b


win :: Board -> Bool
win b = winningRow b || winningColumn b || winningDiagonal b

tie :: Board -> Bool 
tie = all (notElem Empty)

winningRow :: Board -> Bool
winningRow = winningColumn . transpose

winningColumn :: Board -> Bool
winningColumn b = or [fourSame col | col<-b]

winningDiagonal :: Board -> Bool
winningDiagonal b =
   any fourSame (getDiagonal1 b) || any fourSame (getDiagonal2 b)

-- stulen från devin
getDiagonal1 :: Board -> Board
getDiagonal1 b = concat [[[(b !! (y + k')) !! (x + k') | k' <- [0..3]]
                            | x <- [0..length (head b) - 4]]
                            | y <- [0..length b - 4]] 


getDiagonal2 :: Board -> Board
getDiagonal2 = getDiagonal1 . reverse

fourSame :: Col -> Bool
fourSame col = any (\x -> length x >= 4 && Empty `notElem` x) (group col)


-- också stulen från Edvin
prettyPrint :: Board -> IO ()
prettyPrint b = do
    putStrLn $ replicate (1 + 4 * length b) '-'
    prettyPrint' $ map (map f) ((reverse . transpose) b)

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


