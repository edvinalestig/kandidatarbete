{-|
Module      : Utility
Description : A Haskell module containing various utility functions
-}
module DSL.Utility where

import DSL.Types


-- | Current player is put last in the player list
cyclePlayers :: [Player] -> [Player]
-- cyclePlayers ps = tail ps ++ [head ps]
cyclePlayers = tail <> take 1 -- MONOID (Semigroup)! :D

-- * Turn

-- | A turn with no values.
nullTurn :: Turn
nullTurn = Turn nullPiece (Move nullPos nullPos)

-- | Create a `Turn`, with the action `Place`, on the specified coordinates
placeTurn :: Piece -> Int -> Int -> Turn
placeTurn p x y = Turn p (Place (Pos x y))

-- | This does the same as @'placeTurn'@, but it takes in @Pos@ instead of two `Int`.
placeTurn' :: Piece -> Pos -> Turn
placeTurn' p pos = Turn p (Place pos)

-- | "Combines" two turns by adding the coordinates of the their `Action`, maybe poorly named
combineTurn :: Turn -> Turn -> Turn
combineTurn t1 t2@(Turn p _) = placeTurn' p (origin t1 + origin t2)

-- | Gets the turns respresenting the positions between
-- the start and end position of the input turn.
-- Does not include the start end end position turns.
tilesBetweenTwoCoords :: Turn -> Game -> [Turn]
tilesBetweenTwoCoords t = init . tail . tilesFromTo t

-- | Get the all the turns between the start and end positions,
-- including the start and end positions.
tilesFromTo :: Turn -> Game -> [Turn]
tilesFromTo t@(Turn p _) g =
    if zeroPos (directionPos t)
        then [t']
        else t : tilesFromTo t' g
    where
        t' = Turn p (Move (movePosInDir t) (destination t))

-- * Tile

-- | Extract the `Tile` from a `Board` and `Pos`
getTile :: Board -> Pos -> Tile
getTile b (Pos x y) = (b !! y) !! x

-- | Extract the `Tile` from a `Board` and `Pos`. It calls `getTile`.
originTile :: Turn -> Game -> Tile
originTile t g = getTile (board g) (origin t)

-- | Extract the `Tile` from a `Board` and `Pos`. It calls `getTile`.
destinationTile :: Turn -> Game -> Tile
destinationTile t g = getTile (board g) (destination t)



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
allEQ ((Empty _):_) = False
allEQ ((PieceTile p _):as) = all (samePiece p) as
allEQ _      = True

-- | Checks if a `Piece` is the same as another `Piece` on a `Tile`
samePiece :: Piece -> Tile -> Bool
samePiece _ (Empty _) = False
samePiece p (PieceTile p2 _) = p == p2

-- | Helper function to check if a 'Turn' results in a position within the board's boundaries.
isWithinBoard :: Turn -> Game -> Bool
isWithinBoard t g = isWithinBoard' (destination t) && isWithinBoard' (origin t)
    where
        isWithinBoard' (Pos x y) = x >= 0 && x < (length . head . board) g && y >= 0 && y < (length . board) g

-- | Checks if the input pos' coords both are zero
zeroPos :: Pos -> Bool
zeroPos = (==) (Pos 0 0)

-- * Pos

-- | Represent an empty 'Pos'.
-- Should only be used where a pos is needed as an argument,
-- but isn't used.
nullPos :: Pos
nullPos = Pos 0 0

-- | Extract the `Pos` of a `Tile`
getPos :: Tile -> Pos
getPos (PieceTile _ pos) = pos
getPos (Empty pos) = pos

-- | Extract resulting `Pos` of a @Turn@.
origin :: Turn -> Pos
origin (Turn _ (Place pos))  = pos
origin (Turn _ (Move pos _)) = pos

-- | Extract resulting `Pos` of a @Turn@.
destination :: Turn -> Pos
destination (Turn _ (Place pos))  = pos
destination (Turn _ (Move _ pos)) = pos

-- | The direction a move turn
directionPos :: Turn -> Pos
directionPos t = signum $ destination t - origin t

-- | moves the origin position in the turns direction
movePosInDir :: Turn -> Pos
movePosInDir t = origin t + directionPos t


-- * Piece

-- | Get a Piece from a position on the board.
-- If the tile is empty this function throws and error
getPiece :: Board -> Pos -> Piece
getPiece b pos = case getTile b pos of
                    Empty _ -> error "tried to check for a piece on an Empty tile. (this should never occur)"
                    PieceTile p _ -> p

-- | Given a board and a position, returns the piece at given position if one exists.
getPiece' :: Board -> Pos -> Maybe Piece
getPiece' b pos = case getTile b pos of
                    Empty _ -> Nothing
                    PieceTile p _ -> Just p

-- | Represent an empty 'Piece'.
-- Should only be used where a pos is needed as an argument,
-- but isn't used.
nullPiece :: Piece
nullPiece = Piece "" nullPlayer

-- * Player

-- | Extract the @Player@ of a @Piece@
getPlayer :: Piece -> Player
getPlayer (Piece _ p) = p

-- | Represent an empty 'Player'.
-- Should only be used where a pos is needed as an argument,
-- but isn't used.
nullPlayer :: Player
nullPlayer = Player ""


-- * Other

-- | Replaces an elemenent with the input at a given index
replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs