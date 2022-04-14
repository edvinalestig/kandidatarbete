{-|
Module      : Utility
Description : A Haskell module containing various utility functions
-}
module DSL.Utility (
    placePiece,
    _placePiece,
    placeTurn,
    placeTurn',
    getTile,
    turnGameToTile,
    getPlayer,
    getPos,
    turnToPos,
    filterNothing,
    replaceAtIndex
) where

import DSL.Types

-- | Places a piece in a certain position on the board
placePiece :: Rule
placePiece = Rule $ Update _placePiece

-- | Places a piece in a certain position on the board
_placePiece :: Turn -> Game -> Game
_placePiece t@(Turn p _) g = g {board = replaceAtIndex y newRow (board g)}
    where (Pos x y) = turnToPos t
          tile = PieceTile p (Pos x y)
          newRow = replaceAtIndex x tile (board g !! y)

-- | Create a `Turn`, with the action `Place`, on the specified coordinates
placeTurn :: Piece -> Int -> Int -> Turn
placeTurn p x y = Turn p (Place (Pos x y))

-- | This does the same as @'placeTurn'@, but it takes in @Pos@ instead of two `Int`.
placeTurn' :: Piece -> Pos -> Turn
placeTurn' p pos = Turn p (Place pos)


getTile :: Board -> Pos -> Tile
getTile b (Pos x y) = (b !! y) !! x

-- | Check if two tiles has the same piece on it, or if both tiles are empty 
eqTile :: Tile -> Tile -> Bool
eqTile (PieceTile p1 _) (PieceTile p2 _) = p1 == p2
eqTile (Empty _) (Empty _) = True
eqTile _ _ = False

turnGameToTile :: Turn -> Game -> Tile
turnGameToTile t g = getTile (board g) (turnToPos t)

getPlayer :: Piece -> Player
getPlayer (Piece _ p) = p

getPos :: Tile -> Pos
getPos (PieceTile _ pos) = pos
getPos (Empty pos) = pos

turnToPos :: Turn -> Pos
turnToPos (Turn _ (Place pos))  = pos
turnToPos (Turn _ (Move _ pos)) = pos


-- | Replaces an elemenent with the input at a given index
replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs