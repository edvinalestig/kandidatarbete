{-|
Module      : Rules
Description : A haskell module containing functions that users uses to create board games.

A Haskell module containing every available function that the users should have access to.
This module contains some rules, conditions and updates that can be used in different
board games and also helper functions to these rules
-}

module DSL.Lib (
    -- * Game
    -- $game
    game,

    -- * Boards
    -- $board
    rectBoard,
    initRectBoard,

    -- * Rules
    -- $rule
    placePiece,
    movePiece,
    gameDraw,
    currentPlayerWins,
    playerWithMostPiecesWins,
    doUntil,
    replaceUntil,
    skipTurn,
    convertToPiece,

    -- * Conditions
    -- $condition
    trueCond,
    falseCond,
    pieceIsAtPos,
    pieceOriginBelongsToRow,
    pieceDestinationBelongsToRow,
    boardIsFull,
    changedState,
    emptyTile,
    allyTile,
    enemyTile,
    pieceEqualTo,
    pieceEqualToEither,
    emptyDestination,
    allyDestination,
    enemyDestination,
    destinationIsRelativeTo,
    isDiagonalMove,
    isStraightMove,
    pieceOnBoard,
    pieceNotOnBoard,
    noPlayerHasMoves,
    playerCanPlace,
    inARow,
    tileBelowIsNotEmpty,
    tilesBetweenAre,
    isKnightMove,
    isKingMove,
    isRookMove,
    isBishopMove,
    isQueenMove,

    -- * Updates
    -- $update
    allDirections,
    straightDirections,
    diagonalDirections,
    turnUp,
    turnLeft,
    turnRight,
    turnDown,
    turnUpLeft,
    turnUpRight,
    turnDownLeft,
    turnDownRight,

    -- * Display functions
    -- $display
    prettyPrint
) where

import DSL.Types
import DSL.Utility
import DSL.Internal


-- * Game
{- $game -}

-- | An empty `Game` used to base games on
game :: Game
game = Game {
    board = error "No board provided, please provide a board",
    pieces = [],
    players = [],
    preTurnRules = [],
    rules = [],
    endConditions = [],
    winner = Nothing,
    gameEnded = False,
    dispFunction = prettyPrint
}

-- * Boards
{- $board -}

-- | Creates an empty rectangular board
rectBoard :: Int -> Int -> Board
rectBoard w h = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]]

-- | Creates an rectangular board with pieces in certain locations
initRectBoard :: Int -> Int -> [((Int, Int), Piece)] -> Board
initRectBoard w h []              = rectBoard w h
initRectBoard w h (((x,y), p):as) = board $ _placePiece (placeTurn p (x-1) (y-1)) $ game {board = initRectBoard w h as}


-- * Rules
{- $rule -}

-- | Places a piece in a certain position on the board
placePiece :: Rule
placePiece = Rule $ Update _placePiece

-- | Move a piece to a absolute position on the board
movePiece :: Rule
movePiece = Rule $ Update _movePiece

-- | A `Rule` for a draw
gameDraw :: Rule
gameDraw = Rule $ Update _makeDraw

-- | A `Rule` for the turn player winning
currentPlayerWins :: Rule
currentPlayerWins = Rule $ Update _currentPlayerWins

-- | A `Rule` for when the player with the most pieces out on the board wins
playerWithMostPiecesWins :: Rule
playerWithMostPiecesWins = Rule $ Update _playerWithMostPiecesWins

-- | Iterate a 'Rule' until a 'Condition' is met over an @'Update' 'Turn'@
--
-- Example usage:
--
-- > doUntil (If emptyTile placePiece) enemyTile
--
-- The example replaces the next tile, if empty, with an ally tile.
-- If the condition fails before reaching an enemyTile, the result is ignored.
doUntil :: Rule -> Condition Turn -> Update Turn -> Rule
doUntil r c f = IterateUntil (TurnRule f r) c

-- | Replace a tile until another tile in the specified direction 
-- 
-- Example usage:
--
-- > replaceUntil enemyTile allyTile
--
-- The example replaces every enemyTile until the first allyTile in the specified direction.
-- If another tile is reached before the end condition is met, the result is ignored.
replaceUntil :: Condition Turn -> Condition Turn -> Update Turn -> Rule
replaceUntil c = doUntil (If c placePiece)

-- | Passes the turn according to the order described in the gamestate.
skipTurn :: Rule
skipTurn = Rule $ Update _skipTurn

-- | A rule that places a specified piece at a tile specified by the turn inupt.
convertToPiece :: String -> Rule
convertToPiece s = TurnRule (Update $ _convertToPiece s) placePiece


-- * Conditions
{- $condition -}


-- | A condition that is always 'True'
trueCond :: Condition Turn
trueCond = Condition (\_ _ -> True)

-- | A condition that is always 'False'
falseCond :: Condition Turn
falseCond = Condition (\_ _-> False)

-- | A condition which is true if a given piece is at a given location.
pieceIsAtPos :: Pos -> Condition Turn 
pieceIsAtPos = Condition . _pieceIsAtPos

-- | A condition which is true if a piece at the first position of a move turn 
--   exists in a given row.
pieceOriginBelongsToRow :: Int -> Condition Turn
pieceOriginBelongsToRow = Condition . _pieceOriginBelongsToRow

-- | A condition which is true if a piece at the first position of a move turn 
--   exists in a given row.
pieceDestinationBelongsToRow :: Int -> Condition Turn
pieceDestinationBelongsToRow = Condition . _pieceDestinationBelongsToRow


-- | Checks if the board is full
boardIsFull :: Condition a
boardIsFull = Condition _boardIsFull

-- | A condition for checking if a 'Rule' would change the state of the board. 
changedState :: Rule -> Condition Turn
changedState r = Condition $ _changedState r

-- | A `Condition` for checking if the current tile is empty
emptyTile :: Condition Turn
emptyTile = Condition _emptyTile

-- | A `Condition` for checking if the piece (on given tile) belongs to the current player
allyTile :: Condition Turn
allyTile = Condition $ _comparePlayerOnTile (==)

-- | A `Condition` for checking if the piece (on given tile) doesn't belong to the current player.
enemyTile :: Condition Turn
enemyTile = Condition $ _comparePlayerOnTile (/=)

-- | A `Condition` for checking if a predescribed piece is equal to a given piece on the board.
pieceEqualTo :: String -> Condition Turn
pieceEqualTo = Condition . _pieceEqualTo

-- | A `Condition` for
pieceEqualToEither :: [String] -> Condition Turn
pieceEqualToEither = foldl (\a b -> a `OR` pieceEqualTo b) falseCond

-- | A `Condition` for checking if the destination tile is empty
emptyDestination :: Condition Turn 
emptyDestination = Condition _emptyDestination

-- | A `Condition` for checking if the destination belongs to the player
allyDestination :: Condition Turn
allyDestination = Condition $ _comparePlayerOnDestination (==)

-- | A `Condition` for checking if the destination belongs to the enemy
enemyDestination :: Condition Turn
enemyDestination = Condition (_comparePlayerOnDestination (/=)) `AND` NOT emptyDestination

-- | Check if the destination tile is move relative a certain amount
destinationIsRelativeTo :: (Int, Int) -> Condition Turn
destinationIsRelativeTo = Condition . _destinationIsRelativeTo

-- | A `Condition` for checking if a given move follows a diagonal path.
isDiagonalMove :: Condition Turn
isDiagonalMove = Condition _isDiagonalMove

-- | A `Condition` for checking if a given move follows a straight path.
isStraightMove :: Condition Turn
isStraightMove = Condition _isStraightMove

-- | A `Condition` for checking if the piece is on the board.
pieceOnBoard :: String -> Condition Turn
pieceOnBoard = Condition . _pieceOnBoard

-- | A `Condition` for checking if the piece is not on the board. Inverse of `pieceOnBoard`.
pieceNotOnBoard :: String -> Condition Turn
pieceNotOnBoard = NOT . pieceOnBoard

-- | Returns `True` if no player has any valid moves, `False` otherwise
noPlayerHasMoves :: Condition Turn
noPlayerHasMoves = Condition _noPlayerHasMoves

-- | A `Condition` for checking if a given player can place a piece anywhere on the board.
playerCanPlace :: Condition Turn
playerCanPlace = Condition _playerCanPlace

-- | Checks if the board contains a given number of pieces in a row in any 
--   orientation. (Vertical, horizontal, diagonal)
inARow :: Int -> Condition Turn
inARow = Condition . _inARow

-- | A condition for checking if the tile below another tile is empty
tileBelowIsNotEmpty :: Condition Turn
tileBelowIsNotEmpty = Condition _tileBelowIsNotEmpty

-- | A condition for checking if another condition applies to all tiles between
-- the origin and destination pos 
tilesBetweenAre :: Condition Turn -> Condition Turn
tilesBetweenAre c = All c tilesBetweenTwoCoords

-- | A condition for determining if the turn is a knight move in the game chess.
--   Note that this condition is not exclusive to the game chess, but is just a name for the condition
isKnightMove :: Condition Turn
isKnightMove = destinationIsRelativeTo (1,2) `OR` destinationIsRelativeTo (1,-2)
    `OR` destinationIsRelativeTo (-1,2) `OR` destinationIsRelativeTo (-1,-2)
    `OR` destinationIsRelativeTo (2,1) `OR` destinationIsRelativeTo (2,-1)
    `OR` destinationIsRelativeTo (-2,1)`OR` destinationIsRelativeTo (-2,-1)

-- | A condition for determining if the turn is a king move in the game chess.
--   Note that this condition is not exclusive to the game chess, but is just a name for the condition
isKingMove :: Condition Turn
isKingMove = foldl OR falseCond
    [destinationIsRelativeTo (i,j) | i <- [-1..1], j <- [-1..1], not (i==0 && j==0)]

-- | A condition for determining if the turn is a rook move in the game chess.
--   Note that this condition is not exclusive to the game chess, but is just a name for the condition
isRookMove :: Condition Turn
isRookMove = isStraightMove `AND` tilesBetweenAre emptyTile

-- | A condition for determining if the turn is a bishop move in the game chess.
--   Note that this condition is not exclusive to the game chess, but is just a name for the condition
isBishopMove :: Condition Turn
isBishopMove = isDiagonalMove `AND` tilesBetweenAre emptyTile

-- | A condition for determining if the turn is a queen move in the game chess.
--   Note that this condition is not exclusive to the game chess, but is just a name for the condition
isQueenMove :: Condition Turn 
isQueenMove = isRookMove `OR` isBishopMove

-- * Updates
{- $update -}

-- | A list containing all directions
allDirections :: [Update Turn]
allDirections = straightDirections ++ diagonalDirections

-- | A list containing all straight directions
straightDirections :: [Update Turn]
straightDirections = [turnDown, turnUp, turnLeft, turnRight]

-- | A list containing all diagonal directions
diagonalDirections :: [Update Turn]
diagonalDirections = [turnDownLeft, turnUpLeft, turnUpRight, turnDownRight]

-- | An `Update` for moving one step up, mainly for use with `IterateUntil`
turnUp :: Update Turn
turnUp = Update $ _turnDirection (0, -1)

-- | An `Update` for moving one step left, mainly for use with `IterateUntil`
turnLeft :: Update Turn
turnLeft = Update $ _turnDirection (-1, 0)

-- | An `Update` for moving one step right, mainly for use with `IterateUntil`
turnRight :: Update Turn
turnRight = Update $ _turnDirection (1, 0)

-- | An `Update` for moving one step down, mainly for use with `IterateUntil`
turnDown :: Update Turn
turnDown = Update $ _turnDirection (0, 1)

-- | An `Update` for moving one step up and to the left, mainly for use with `IterateUntil`
turnUpLeft :: Update Turn
turnUpLeft = turnUp `COMBINE` turnLeft

-- | An `Update` for moving one step up and to the right, mainly for use with `IterateUntil`
turnUpRight :: Update Turn
turnUpRight = turnUp `COMBINE` turnRight

-- | An `Update` for moving one step down and to the left, mainly for use with `IterateUntil`
turnDownLeft :: Update Turn
turnDownLeft = turnDown `COMBINE` turnLeft

-- | An `Update` for moving one step down and to the right, mainly for use with `IterateUntil`
turnDownRight :: Update Turn
turnDownRight = turnDown `COMBINE` turnRight


-- * Display functions
{- $display -}


-- | Prints a board in the terminal. It's pretty.
prettyPrint :: Game -> IO ()
prettyPrint g = do
    let b = board g
        n = length . show $ length b
    putStr   $ '\n' : replicate (n+1) ' '
    putStrLn $ foldl (\s t -> s ++ show t ++ replicate (max 1 (n - length (show t))) ' ' ++ "| ") 
                     "| " $ map (`mod` 10) [1..(length (head b))] -- mod 10 because of width limit
    putStrLn $ replicate (2 + n + 4 * length (head b)) '-'
    prettyPrint' n [1..] $ map (map show) b
    where

        prettyPrint' :: Int -> [Int] -> [[String]] -> IO ()
        prettyPrint' _ _ [] = return ()
        prettyPrint' n (i:is) (b:bs)  = do
            let i' = show i
            putStr $ i' ++ replicate (n + 1 - length i') ' '
            putStrLn $ foldl (\s t -> s ++ t ++ " | ") "| " b
            putStrLn $ replicate (2 + n + 4 * length b) '-'
            prettyPrint' n is bs
        prettyPrint' _ [] _ = error "Congrats, you've emptied an infinite list"
