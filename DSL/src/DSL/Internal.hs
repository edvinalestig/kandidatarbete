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
    _movePiece,
    _makeDraw,
    _currentPlayerWins,
    _playerWithMostPiecesWins,
    _convertToPiece,

    -- * Conditions
    -- $condition
    _pieceIsAtPos,
    _pieceOriginBelongsToRow,
    _pieceDestinationBelongsToRow,
    _boardIsFull,
    _changedState,
    _comparePieceOnTile,
    _comparePlayerOnTile,
    _comparePlayerOnDestination,
    _pieceEqualTo,
    _pieceOnBoard,
    _emptyTile,
    _emptyDestination,
    _destinationIsRelativeTo,
    _isDiagonalMove,
    _isStraightMove,
    _noPlayerHasMoves,
    _playerCanPlace,
    _inARow,
    _tileBelowIsNotEmpty,

    -- * Updates
    -- $update
    _skipTurn,
    _turnDirection,

    -- * Helper functions
    -- $helper
    playerWithMostPieces,
    playersWithMostPieces,
    playerPieces,
    playerHasMoves,
    pieceHasMoves,
    getDiagonals,
    getRows,
    getColumns,
    countPiece,
    isValidInput,
    filterPieces
) where

import DSL.Types
import DSL.Utility
import Data.List
import Data.Maybe (isJust)
import DSL.Run (runRule)


-- * Rules
{- $rule -}


-- | Places a piece in a certain position on the board
_placePiece :: Turn -> Game -> Game
_placePiece t@(Turn p (Place _)) = replacePiece tile
    where tile = PieceTile p $ origin t
_placePiece _ = id

-- | Moves a pieces to a absolute position on the board
_movePiece :: Turn -> Game -> Game
_movePiece (Turn p (Move pos1 pos2)) = replacePiece tile1 . replacePiece tile2
    where tile1 = Empty pos1
          tile2 = PieceTile p pos2
_movePiece _ = id

-- | Places a tile in a certain position on the board
replacePiece :: Tile -> Game -> Game
replacePiece tile g = g {board = replaceAtIndex y newRow (board g)}
    where (Pos x y) = getPos tile
          newRow = replaceAtIndex x tile (board g !! y)

-- | Set the game state to a draw
_makeDraw :: Turn -> Game -> Game
_makeDraw _ = updateWinner drawGame

-- | Update the game state such that the current player wins
_currentPlayerWins :: Turn -> Game -> Game
_currentPlayerWins _ = updateWinner $ Just . head . players

-- | Updates the game state so that the player with most pieces wins
_playerWithMostPiecesWins :: Turn -> Game -> Game
_playerWithMostPiecesWins _ = updateWinner playerWithMostPieces

_convertToPiece :: String -> Turn -> Turn -> Turn
_convertToPiece s (Turn (Piece _ player) (Move _ pos2)) _ = placeTurn' (Piece s player) pos2
_convertToPiece _ _ _ = error "undefined"


-- *  Conditions
{- $condition -}

-- | Checks if the Turn Piece is at a certain position, only relevant for moving pieces
_pieceIsAtPos :: Pos -> Turn -> Game -> Bool
_pieceIsAtPos pos' (Turn _ (Move pos _)) _ = pos == pos'
_pieceIsAtPos _ _ _ = False

-- Moving from a tile
_pieceOriginBelongsToRow :: Int -> Turn -> Game -> Bool
_pieceOriginBelongsToRow = _pieceBelongsToRow origin

-- Moving to a tile
_pieceDestinationBelongsToRow :: Int -> Turn -> Game -> Bool
_pieceDestinationBelongsToRow = _pieceBelongsToRow destination

_pieceBelongsToRow :: (Turn -> Pos) -> Int -> Turn -> Game -> Bool
_pieceBelongsToRow f i t@(Turn p _) g = tile `elem` row
    where
        row  = board g !! (i - 1)
        tile = PieceTile p (f t)

-- | Return whether or not the board is full, such that no tiles are empty.
_boardIsFull :: a -> Game -> Bool
_boardIsFull _ g = " " `notElem` concatMap (map show) (board g)

-- | Return whether or not a 'Rule' changes the board at all. 
_changedState :: Rule -> Turn -> Game -> Bool
_changedState r t g = board g /= maybe [] board mg
    where mg = runRule r t g

-- | Takes in a function that compares two pieces.
-- The first piece is given as input, the second piece is located on the board.
_comparePieceOnTile :: (Piece -> Piece -> Bool) -> Turn -> Game -> Bool
_comparePieceOnTile f t@(Turn p _) g =
    case originTile t g of
        (PieceTile p' _) -> p `f` p'
        _                -> False

-- | Make a comparision of the current 'Player' with the piece
-- specified on the board
_comparePlayerOnTile :: (Player -> Player -> Bool) -> Turn -> Game -> Bool
_comparePlayerOnTile f t g =
    case originTile t g of
        (PieceTile p' _) -> getPlayer p' `f` head (players g)
        _               -> False

_comparePlayerOnDestination :: (Player -> Player -> Bool) -> Turn -> Game -> Bool
_comparePlayerOnDestination f t g =
    case destinationTile t g of
        (PieceTile p' _) -> getPlayer p' `f` head (players g)
        _               -> False

_pieceEqualTo :: String -> Turn -> Game -> Bool
_pieceEqualTo s (Turn (Piece s' _) _) _ = s == s'

_pieceOnBoard :: String -> Turn -> Game -> Bool
_pieceOnBoard s _ g = s `elem` strings
    where
        b = board g
        filteredBoard = filter (not . empty') $ concat b
        ps = [p | (PieceTile p _) <- filteredBoard]
        strings = [str | (Piece str _) <- ps]

-- | Return whether or not the current tile the turn is referring to is empty.
_emptyTile :: Turn -> Game -> Bool
_emptyTile t = empty' . originTile t

-- | Return whether or not the destination tile the turn is referring to is empty.
_emptyDestination :: Turn -> Game -> Bool
_emptyDestination t = empty' . destinationTile t

-- | Returns `True` if no player has any valid moves, `False` otherwise
_noPlayerHasMoves :: Turn -> Game -> Bool
_noPlayerHasMoves _ g = not $ any (playerHasMoves g) (players g)

_playerCanPlace :: Turn -> Game -> Bool
_playerCanPlace _ g = playerHasMoves g (head $ players g)

-- | Checks if the destination is x steps up/down and y steps left/right compared to original position
_destinationIsRelativeTo :: (Int, Int) -> Turn -> Game -> Bool
_destinationIsRelativeTo (x,y) t _ = Pos x y == destination t - origin t

-- | Checks if the destination is diagonal from the original position.
_isDiagonalMove :: Turn -> Game -> Bool
_isDiagonalMove t _ = abs x == abs y
    where
        (Pos x y) = origin t - destination t

-- | Checks if the destination is horizontal or vertical from the original position.
_isStraightMove :: Turn -> Game -> Bool
_isStraightMove t _ = x == 0 || y == 0
    where
        (Pos x y) = origin t - destination t

-- checks if all tiles between the current to new position are empty recursively.
-- WIP
_recursive :: (Int, Int) -> Condition Turn -> Turn -> Game -> Bool
_recursive (_, _) = undefined

-- | Return whether or not 'k' in a row pieces, in all directions,
-- can be found anywhere on the board.
_inARow :: Int -> Turn -> Game -> Bool
_inARow k _ g = any allEQ everything
    where
        everything = getRows b k ++ getColumns b k ++ getDiagonals b k
        b = board g

-- | Return whether or not the tile below is empty.
_tileBelowIsNotEmpty :: Turn -> Game -> Bool
_tileBelowIsNotEmpty t@(Turn p _) g =
    y >= maxY || not (_emptyTile (placeTurn p x (y+1)) g)
    where
        maxY = length (board g) - 1 -- Bottom row
        (Pos x y) = origin t


-- * Updates
{- $update -}

_skipTurn :: Turn -> Game -> Game
_skipTurn _ g = g {players = cyclePlayers $ players g}

-- | Return the resulting 'Turn' after shifting the input 'Turn' by a given amount
_turnDirection :: (Int, Int) -> Turn -> Turn -> Turn
_turnDirection (dx, dy) (Turn p _) = combineTurn $ placeTurn p dx dy


-- * Helper functions
{- helper -}


drawGame :: Game -> Maybe a
drawGame _ = Nothing

-- | Update the game state with the winner
updateWinner :: (Game -> Maybe Player) -> Game -> Game
updateWinner f g = if gameEnded g then g else g {winner = f g, gameEnded = True}

-- | Return a 'Maybe' containing the player with most pieces on the board
-- currently. It returns 'Nothing' if multiple 'Player' has the most pieces.
playerWithMostPieces :: Game -> Maybe Player
playerWithMostPieces g | length ps == 1 = Just $ head ps
                       | otherwise      = Nothing
    where
        ps = playersWithMostPieces (players g) (board g)

-- | Returns a list containing all players with the most pieces on the board
playersWithMostPieces :: [Player] -> Board -> [Player]
playersWithMostPieces ps b = ps'
    where amounts = map length $ playerPieces <$> ps <*> [b]
          amounts' = zip ps amounts
          ps' = [player | (player, n) <- amounts', n >= maximum amounts]

-- | Returns a list containing all pieces on the board belonging to a player        
playerPieces :: Player -> Board -> [Piece]
playerPieces p b = filter (\x -> getPlayer x == p) as
    where
        as = [pie | PieceTile pie _ <- concat b]



-- | Determines if a given player has any legal moves with regards to the rules and a board state
playerHasMoves :: Game -> Player -> Bool
playerHasMoves g p = anyPiecesHasMoves (filterPieces p (pieces g)) g

-- | Determines if a any given pieces has any legal moves 
anyPiecesHasMoves :: [Piece] -> Game -> Bool
anyPiecesHasMoves ps g = foldr
    (\ p -> (||) (pieceHasMoves p g (concat (board g)))) False ps

-- | Determines if a given piece has any legal moves with regards to the rules and a board state
pieceHasMoves :: Piece -> Game -> [Tile] -> Bool
pieceHasMoves _ _ [] = False
pieceHasMoves p g (t:ts) | null (rules g) = False
                         | otherwise = validInput || pieceHasMoves p g ts
    where
        validInput = isValidInput turn g
        turn = placeTurn' p (getPos t)



-- | Gets a list of all diagonals of a certain length on the board
getDiagonals :: Board -> Int -> [[Tile]]
getDiagonals b k = getDiagonals' b ++ getDiagonals' (map reverse b)
    where
        getDiagonals' b' = concat [[[(b' !! (y + k')) !! (x + k') | k' <- [0..k-1]]
                            | x <- [0..length (head b') - k]]
                            | y <- [0..length b' - k]]

-- | Gets a list of all rows of a given length on the board
getRows :: Board -> Int -> [[Tile]]
getRows b len = concat [[take len (drop n r) | n <- [0..(length r - len)]] | r <- b]

-- | Gets a list of all columns of a given length on the board
getColumns :: Board -> Int -> [[Tile]]
getColumns b = getRows (transpose b)



-- | Counts how many pieces of one type there are on the board
countPiece :: Piece -> Board -> Int
countPiece p b = length $ filter (samePiece p) (concat b)

-- | Checks whether or not you can place a piece at a specific location
isValidInput :: Turn -> Game -> Bool
isValidInput t g = any (\f -> isJust $ runRule f t g) (rules g)

-- | Returns a list containing all pieces that the given player can place
filterPieces :: Player -> [Piece] -> [Piece]
filterPieces _ [] = []
filterPieces player ((Piece s p):ps) =
    if player == p then
        Piece s p : filterPieces player ps
    else
        filterPieces player ps