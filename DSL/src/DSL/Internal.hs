{-|
Module      : Rules
Description : A haskell module containing internal functions that helps descibe the rules used by the users

These functions are only available internally and are mainly
used by Lib.hs in order to create more complex rules.
-}

module DSL.Internal (
    -- * Rules
    -- $rule
    _placePiece,
    _makeDraw,
    _currentPlayerWins,
    _playerWithMostPiecesWins,

    -- * Conditions
    -- $condition
    _isWithinBoard,
    _boardIsFull,
    _changedState,
    _comparePieceOnTile,
    _emptyTile,
    _inARow,
    _tileBelowIsNotEmpty,

    -- * Updates
    -- $update
    _turnDirection,

    -- * Helper functions
    -- $helper
    playerWithMostPieces,
    playersWithMostPieces,
    playerPieces,
    getDiagonals,
    getRows,
    getColumns,
    countPiece
) where

import DSL.Types
import DSL.Utility
import Data.List
import Data.Ord
import Data.Function
import Data.Maybe (Maybe(Nothing), fromMaybe, fromJust, isJust)
import DSL.Run (runRule)


-- * Rules
{- $rule -}


-- | Places a piece in a certain position on the board
_placePiece :: Turn -> Game -> Game
_placePiece t@(Turn p _) g = g {board = replaceAtIndex y newRow (board g)}
    where (Pos x y) = turnToPos t
          tile = PieceTile p (Pos x y)
          newRow = replaceAtIndex x tile (board g !! y)

-- | Set the game state to a draw
_makeDraw :: Turn -> Game -> Game
_makeDraw _ g | gameEnded g = g
              | otherwise   = g {gameEnded = True, winner = Nothing}

_currentPlayerWins :: Turn -> Game -> Game
_currentPlayerWins _ g | gameEnded g = g
                       | otherwise   = g {gameEnded = True, winner = currentPlayer g}
    where
        currentPlayer g = Just $ head (players g)

_playerWithMostPiecesWins :: Turn -> Game -> Game
_playerWithMostPiecesWins _ g = g {winner = playerWithMostPieces g}


-- *  Conditions
{- $condition -}


_isWithinBoard :: Turn -> Game -> Bool
_isWithinBoard t g = x >= 0 && x < (length . head . board) g && y >= 0 && y < (length . board) g
    where
        (Pos x y) = turnToPos t

_boardIsFull :: a -> Game -> Bool
_boardIsFull _ g = " " `notElem` concatMap (map show) (board g)

_changedState :: Rule -> Turn -> Game -> Bool
_changedState r t g = board g /= maybe [] board mg
    where mg = runRule r t g

_comparePieceOnTile :: (Piece -> Piece -> Bool) -> Turn -> Game -> Bool
_comparePieceOnTile f t@(Turn p _) g =
    case turnGameToTile t g of
        (PieceTile p' _) -> p `f` p'
        _                -> False

_emptyTile :: Turn -> Game -> Bool
_emptyTile t g = empty' (turnGameToTile t g)

_inARow :: Int -> Turn -> Game -> Bool
_inARow k _ g = any allEQ everything
    where
        everything = getRows b k ++ getColumns b k ++ getDiagonals b k
        b = board g

_tileBelowIsNotEmpty :: Turn -> Game -> Bool
_tileBelowIsNotEmpty t@(Turn p _) g =
    y >= maxY || not (_emptyTile (placeTurn p x (y+1)) g)
    where
        maxY = length (board g) - 1 -- Bottom row
        (Pos x y) = turnToPos t


-- * Updates
{- $update -}


_turnDirection :: (Int, Int) -> Turn -> Turn -> Turn
_turnDirection (dx, dy) (Turn p _) = combineTurn $ placeTurn p dx dy


-- * Helper functions
{- helper -}


playerWithMostPieces :: Game -> Maybe Player
playerWithMostPieces game | length ps == 1 = Just $ head ps
                          | otherwise      = Nothing
    where
        ps = playersWithMostPieces (players game) (board game)

-- | Returns a list containing all players with the most pieces on the board
playersWithMostPieces :: [Player] -> Board -> [Player]
playersWithMostPieces ps b = players
         where amounts = map length $ playerPieces <$> ps <*> [b]
               amounts' = zip ps amounts
               players = [player | (player, n) <- amounts', n >= maximum amounts]

-- | Returns a list containing all pieces on the board belonging to a player        
playerPieces :: Player -> Board -> [Piece]
playerPieces p b = filter (\x -> getPlayer x == p) as
    where
        as = [pie | PieceTile pie pos <- concat b]


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


-- | Counts how many pieces of one type there are on the board
countPiece :: Piece -> Board -> Int
countPiece p b = length $ filter (samePiece p) (concat b)