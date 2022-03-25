{-|
Module      : Rules
Description : A Haskell module containing some pre-existing rules that can be used in games

This module contains some rules that can be used in different board games and also helper functions to these rules
-}

module DSL.Lib (
    emptyGame,
    -- * Boards
    rectBoard,
    initRectBoard,
    -- * Rules
    currentPlayer,
    draw,
    tileIsEmpty,
    tileBelowIsNotEmpty,
    boardIsFull,
    playerWithMostPieces,
    playersWithMostPieces,
    checkSurrPieces,
    getDiagonalTiles,
    inARow,
    checkSurrLine,
    getDiagonals,
    getRows,
    getColumns
) where

import DSL.Types
import DSL.Utility
import Data.List
import Data.Ord
import Data.Function
import Data.Maybe (Maybe(Nothing))

-- | An empty `Game` 
emptyGame :: Game
emptyGame = Game {
    board = undefined,
    pieces = [],
    dice = [],
    players = [],
    rules = [],
    endConditions = undefined
}

-- * Boards

-- | Creates a rectangular board
rectBoard :: Int -> Int -> Board
rectBoard w h = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]]

initRectBoard :: Int -> Int -> [((Int, Int), Piece)] -> Board
initRectBoard w h []            = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]]
initRectBoard w h (((x,y), pi):ps) = placePiece pi (Pos (x-1) (y-1)) $ initRectBoard w h ps


-- * Rules

currentPlayer :: Game -> Maybe Player
currentPlayer g = Just $ head (players g)

draw :: Game -> Maybe Player
draw _ = Nothing

-- | Checks if a `Tile` at a given position is empty
tileIsEmpty :: Piece -> Pos -> Board -> Bool
tileIsEmpty _ pos board = empty' $ getTile board pos

-- | A rule for checking if the tile below a tile is empty
tileBelowIsNotEmpty :: Piece -> Pos -> Board -> Bool
tileBelowIsNotEmpty p (Pos x y) board = do 
    let maxY = length board - 1 -- Bottom row
    y >= maxY || not (tileIsEmpty p (Pos x (y+1)) board)

-- | Checks if the board is full
boardIsFull :: Game -> Bool
boardIsFull g = " " `notElem` concatMap (map show) (board g)

-- | Checks if the board contains a given number of pieces in a row in any 
--   orientation. (Vertical, horizontal, diagonal)
inARow :: Int -> Game -> Bool
inARow k g = do
    let everything = getRows b k ++ getColumns b k ++ getDiagonals b k
    any allEQ everything
    where
        b = board g

-- | NOT WORKING PROPERLY FIX THIS
checkSurrPieces :: Piece -> Pos -> Board -> Bool
checkSurrPieces p (Pos x y) b = a || c || d || e
    where
        col = transpose b !! x
        row = b !! y
        a = checkSurrLine p (reverse (take (y+1) col)) || checkSurrLine p (drop y col)
        c = checkSurrLine p (reverse (take (x+1) row)) || checkSurrLine p (drop x row)
        d = checkSurrLine p (getDiagonalTiles (reverse b) (Pos x (length b - 1 - y))) || checkSurrLine p (getDiagonalTiles b (Pos x y))
        e = checkSurrLine p (getDiagonalTiles (transpose (map reverse b)) (Pos (length b - 1 - x) (length b - 1 - y))) || checkSurrLine p (getDiagonalTiles (map reverse b) (Pos (length b - 1 - x) y))


checkSurrLine :: Piece -> [Tile] -> Bool
checkSurrLine pie [] = False 
checkSurrLine pie (_:ts) 
    | null ts = False
    | otherwise = noEmptyTiles && (pie == la) && pie /= ot
    where
        arr = concat $ take 2 (group ts)
        noEmptyTiles = " " `notElem` map show arr
        (PieceTile la _) = last arr
        (PieceTile ot _) = head arr

-- | Gets a list of all diagonals of a certain length on the board
getDiagonalTiles :: Board -> Pos -> [Tile]
getDiagonalTiles b (Pos x y) = [getTile b (Pos (x+n) (y+n)) | n <- take (min(length b - y) (length (head b) - x)) [0..]]


-- checkSurrLine (Piece "O" (Player "A")) [Empty (1,1), PieceTile (Piece "X" (Player "B")) (Pos 1 2), PieceTile (Piece "O" (Player "A")) (Pos 1 3)]

-- * Helper functions

-- | Counts how many pieces of one type there are on the board
countPiece :: Piece -> Board -> Int
countPiece p b = length $ filter (samePiece p) (concat b)


playerWithMostPieces :: Game -> Maybe Player
playerWithMostPieces game | length ps == 1 = Just $ head ps
                          | otherwise      = Nothing
    where
        ps = playersWithMostPieces (players game) (board game)

-- | Returns a list containing all pieces on the board belonging to a player        
playerPieces :: Player -> Board -> [Piece]
playerPieces p b = filter (\x -> getPlayer x == p) as
    where
        as = [pie | PieceTile pie pos <- concat b]
 

-- | Returns a list containing all players with the most pieces on the board
playersWithMostPieces :: [Player] -> Board -> [Player]
playersWithMostPieces ps b = players
         where amounts = map length $ playerPieces <$> ps <*> [b]
               amounts' = zip ps amounts
               players = [player | (player, n) <- amounts', n >= maximum amounts]

 

-- | Gets a list of all diagonals of a certain length on the board
getDiagonals :: Board -> Int -> [[Tile]]
getDiagonals b k = getDiagonals' b k ++ getDiagonals' (map reverse b) k
    where
        getDiagonals' b k = concat [[[(b !! (y + k')) !! (x + k') | k' <- [0..k-1]]
                            | x <- [0..length (head b) - k]]
                            | y <- [0..length b - k]] 

-- | Gets a list of all rows of a given length on the board
getRows :: Board -> Int -> [[Tile]]
getRows b len = concat [[take len (drop n r) | n <- [0..(length r - len)]] | r <- b]

-- | Gets a list of all columns of a given length on the board
getColumns :: Board -> Int -> [[Tile]]
getColumns b = getRows (transpose b)

-- | Checks if all tiles in a list of tiles are non-empty and contain the same `Piece`
allEQ :: [Tile] -> Bool
allEQ ((Empty _):as) = False -- all empty' as
allEQ ((PieceTile p _):as) = all (samePiece p) as
allEQ _      = True

-- | Checks if a tile is empty
empty' :: Tile -> Bool
empty' (Empty _) = True
empty'  _        = False

-- | Checks if a `Piece` is the same as another `Piece` on a `Tile`
samePiece :: Piece -> Tile -> Bool
samePiece _ (Empty _) = False
samePiece p (PieceTile p2 _) = p == p2