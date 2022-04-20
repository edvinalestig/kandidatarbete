{-|
Module      : Utility
Description : A Haskell module containing various utility functions
-}
module DSL.Utility where

import DSL.Types



-- * Turn

-- | Create a `Turn`, with the action `Place`, on the specified coordinates
placeTurn :: Piece -> Int -> Int -> Turn
placeTurn p x y = Turn p (Place (Pos x y))

-- | This does the same as @'placeTurn'@, but it takes in @Pos@ instead of two `Int`.
placeTurn' :: Piece -> Pos -> Turn
placeTurn' p pos = Turn p (Place pos)

-- | "Combines" two turns by adding the coordinates of the their `Action`, maybe poorly named
combineTurn :: Turn -> Turn -> Turn
combineTurn t1 t2@(Turn p _) = placeTurn' p (turnToPos t1 + turnToPos t2)


-- * Tile

-- | Extract the `Tile` from a `Board` and `Pos`
getTile :: Board -> Pos -> Tile
getTile b (Pos x y) = (b !! y) !! x

-- | Extract the `Tile` from a `Board` and `Pos`. It calls `getTile`.
turnGameToTile :: Turn -> Game -> Tile
turnGameToTile t g = getTile (board g) (turnToPos t)

-- | Extract the `Tile` from a `Board` and `Pos`. It calls `getTile`.
turnGameToTile' :: Turn -> Game -> Tile
turnGameToTile' t g = getTile (board g) (turnToPos' t)


-- * Bool

-- | Checks if a tile is empty
empty' :: Tile -> Bool
empty' (Empty _) = True
empty'  _        = False

-- | Check if two tiles has the same piece on it, or if both tiles are empty 
eqTile :: Tile -> Tile -> Bool
eqTile (PieceTile p1 _) (PieceTile p2 _) = p1 == p2
eqTile (Empty _) (Empty _) = True
eqTile _ _ = False

-- | Checks if all tiles in a list of tiles are non-empty and contain the same `Piece`
allEQ :: [Tile] -> Bool
allEQ ((Empty _):as) = False
allEQ ((PieceTile p _):as) = all (samePiece p) as
allEQ _      = True

-- | Checks if a `Piece` is the same as another `Piece` on a `Tile`
samePiece :: Piece -> Tile -> Bool
samePiece _ (Empty _) = False
samePiece p (PieceTile p2 _) = p == p2

-- | Extract the @Player@ of a @Piece@
getPlayer :: Piece -> Player
getPlayer (Piece _ p) = p

-- | Helper function to check if a 'Turn' results in a position within the board's boundaries.
isWithinBoard :: Turn -> Game -> Bool
isWithinBoard t g = isWithinBoard' (turnToPos' t) g && isWithinBoard' (turnToPos t) g
    where
        isWithinBoard' (Pos x y) g = x >= 0 && x < (length . head . board) g && y >= 0 && y < (length . board) g

-- * Pos

-- | Extract the `Pos` of a `Tile`
getPos :: Tile -> Pos
getPos (PieceTile _ pos) = pos
getPos (Empty pos) = pos

-- | Extract resulting `Pos` of a @Turn@.
turnToPos :: Turn -> Pos
turnToPos (Turn _ (Place pos))  = pos
turnToPos (Turn _ (Move pos _)) = pos

-- | Extract resulting `Pos` of a @Turn@.
turnToPos' :: Turn -> Pos
turnToPos' (Turn _ (Place pos))  = pos
turnToPos' (Turn _ (Move _ pos)) = pos


-- * Other

-- | Replaces an elemenent with the input at a given index
replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs