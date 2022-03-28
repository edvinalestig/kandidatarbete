{-|
Module      : Utility
Description : A Haskell module containing various utility functions
-}
module DSL.Utility (
    placePiece,
    getTile,
    getPlayer,
    getPos,
    filterNothing,
    replaceAtIndex
) where

import DSL.Types

-- | Places a piece in a certain position on the board
placePiece :: Piece -> Pos -> Board -> Board
placePiece p (Pos x y) b = replaceAtIndex y newRow b
    where tile = PieceTile p (Pos x y)
          newRow = replaceAtIndex x tile (b !! y)

getTile :: Board -> Pos -> Tile
getTile b (Pos x y) = (b !! y) !! x

getPlayer :: Piece -> Player
getPlayer (Piece _ p) = p

getPos :: Tile -> Pos
getPos (PieceTile _ pos) = pos
getPos (Empty pos) = pos

-- | Filters out `Nothing` from a list of `Maybe`
filterNothing :: [Maybe a] -> [a]
filterNothing []     = []
filterNothing (x:xs) = case x of
    Nothing -> filterNothing xs
    Just a  -> a : filterNothing xs

-- | Replaces an elemenent with the input at a given index
replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs