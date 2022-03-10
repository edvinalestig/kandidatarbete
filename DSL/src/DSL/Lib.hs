{-|
Module      : Rules
Description : A Haskell module containing some pre-existing rules that can be used in games

This module contains some rules that can be used in different board games and also helper functions to these rules
-}

module DSL.Lib (
    emptyGame,
    -- * Boards
    rectBoard,
    -- * Rules
    tileIsEmpty,
    tileBelowIsNotEmpty,
    boardIsFull,
    inARow
) where

import DSL.Types
import DSL.Utility
import Data.List

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

-- * Rules

-- | Checks if a `Tile` at a given position is empty
tileIsEmpty :: Pos -> Board -> Bool
tileIsEmpty pos board = empty' $ getTile board pos

-- | A rule for checking if the tile below a tile is empty
tileBelowIsNotEmpty :: Pos -> Board -> Bool -- Only used in connect four atm, might not work
tileBelowIsNotEmpty (Pos x y) board = do 
    let maxY = length board - 1 -- Bottom row
    y >= maxY || not (tileIsEmpty (Pos x (y+1)) board)

-- | Checks if the board is full
boardIsFull :: Board -> Bool
boardIsFull b = " " `notElem` concatMap (map show) b

-- | Checks if the board contains a given number of pieces in a row in any 
--   orientation. (Vertical, horizontal, diagonal)
inARow :: Int -> Board -> Bool
inARow k b = do
    let everything = getRows b k ++ getColumns b k ++ getDiagonals b k
    any allEQ everything

-- * Helper functions

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
empty'  _         = False

-- | Checks if a `Piece` is the same as another `Piece` on a `Tile`
samePiece :: Piece -> Tile -> Bool
samePiece _ (Empty _) = False
samePiece p (PieceTile p2 _) = p == p2