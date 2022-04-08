{-|
Module      : Utility
Description : A Haskell module containing various utility functions
-}
module DSL.Utility (
    placePiece,
    _placePiece,
    getTile,
    getPlayer,
    getPos,
    turnToPos,
    filterNothing,
    replaceAtIndex
) where

import DSL.Types

-- | Places a piece in a certain position on the board
placePiece :: NewRule
placePiece = Rule $ Update _placePiece

-- | Places a piece in a certain position on the board
_placePiece :: Turn -> Game -> Game
_placePiece t@(Turn p _) g = g {board = replaceAtIndex y newRow (board g)}
    where (Pos x y) = turnToPos t g
          tile = PieceTile p (Pos x y)
          newRow = replaceAtIndex x tile (board g !! y)


getTile :: Board -> Pos -> Tile
getTile b (Pos x y) = (b !! y) !! x

getPlayer :: Piece -> Player
getPlayer (Piece _ p) = p

getPos :: Tile -> Pos
getPos (PieceTile _ pos) = pos
getPos (Empty pos) = pos

turnToPos :: Turn -> Game -> Pos
turnToPos (Turn _ (Place pos)) _  = pos
turnToPos (Turn _ (Move _ pos)) _ = pos
turnToPos (Turn _ (Step p n)) g   = pos
    where 
        pos = path g p n

-- | Filters out `Nothing` from a list of `Maybe`
filterNothing :: [Maybe a] -> [a]
filterNothing []     = []
filterNothing (x:xs) = case x of
    Nothing -> filterNothing xs
    Just a  -> a : filterNothing xs

-- | Replaces an elemenent with the input at a given index
replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs