{-|
Module      : Utility
Description : A Haskell module containing various utility functions
-}
module DSL.Utility (
    getTile,
    filterNothing,
    replaceAtIndex
) where

import DSL.Types

-- | Returns a `Tile` given a position and a board
getTile :: Board -> Pos -> Tile
getTile b (Pos x y) = (b !! x) !! y

-- | Filters out `Nothing` from a list of `Maybe`
filterNothing :: [Maybe a] -> [a]
filterNothing []     = []
filterNothing (x:xs) = case x of
    Nothing -> filterNothing xs
    Just a  -> a : filterNothing xs

-- | Replaces an elemenent with the input at a given index
replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs