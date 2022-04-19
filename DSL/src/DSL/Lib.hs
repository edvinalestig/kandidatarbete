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
    emptyGame,

    -- * Boards
    -- $board
    rectBoard,
    initRectBoard,

    -- * Rules
    -- $rule
    placePiece,
    gameDraw,
    currentPlayerWins,
    playerWithMostPiecesWins,
    iteratorThen,
    iteratorSEQ,

    -- * Conditions
    -- $condition
    trueCond,
    falseCond,
    isWithinBoard,
    boardIsFull,
    changedState,
    allyTile,
    enemyTile,
    emptyTile,
    noPlayerHasMoves,
    inARow,
    tileBelowIsNotEmpty,

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
import Data.List
import Data.Ord
import Data.Function
import Data.Maybe (Maybe(Nothing), fromMaybe, fromJust, isJust)
import DSL.Run (runRule)
import DSL.Internal


-- * Game
{- $game -}

-- | An empty `Game` used to base games on
emptyGame :: Game
emptyGame = Game {
    board = undefined,
    pieces = [],
    players = [],
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
initRectBoard w h []            = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]]
initRectBoard w h (((x,y), p):as) = board $ _placePiece (placeTurn p (x-1) (y-1)) $ emptyGame {board = initRectBoard w h as}


-- * Rules
{- $rule -}

-- | Places a piece in a certain position on the board
placePiece :: Rule
placePiece = Rule $ Update _placePiece

-- | A `Rule` for a draw
gameDraw :: Rule
gameDraw = Rule $ Update _makeDraw

-- | A `Rule` for the turn player winning
currentPlayerWins :: Rule
currentPlayerWins = Rule $ Update _currentPlayerWins

-- | A `Rule` for when the player with the most pieces out on the board wins
playerWithMostPiecesWins :: Rule
playerWithMostPiecesWins = Rule $ Update _playerWithMostPiecesWins

-- | A `Rule` that applies the lambda function to the `Game`
-- state for each `Turn` that is sent in. When a rule fails
-- the last successful result then given as a result.
iteratorThen :: (Update Turn -> Rule) -> [Update Turn] -> Rule
iteratorThen f [] = error "no input is found"
iteratorThen f [t] = f t
iteratorThen f (t:ts) = f t >>> iteratorThen f ts

-- | A `Rule` that applies the lambda function to the `Game`
-- state for each `Turn` that is sent in. If any rule fail
-- the result is ignored.
iteratorSEQ :: (Update Turn -> Rule) -> [Update Turn] -> Rule
iteratorSEQ f [] = error "no input is found"
iteratorSEQ f [t] = f t
iteratorSEQ f (t:ts) = f t >=> iteratorSEQ f ts


-- * Conditions
{- $condition -}

-- | A condition that is always 'True'
trueCond :: Condition Turn
trueCond = Condition (\t g -> True)

-- | A condition that is always 'False'
falseCond :: Condition Turn
falseCond = Condition (\t g -> False)

-- | Checks if a `Tile` is within the boundaries of the board
isWithinBoard :: Condition Turn
isWithinBoard = Condition _isWithinBoard

-- | Checks if the board is full
boardIsFull :: Condition a
boardIsFull = Condition _boardIsFull

-- | A condition for checking if a 'Rule' would change the state of the board. 
changedState :: Rule -> Condition Turn
changedState r = Condition $ _changedState r

-- | A `Condition` for checking if the piece (on given tile) belongs to the current player
allyTile :: Condition Turn
allyTile = Condition $ _comparePieceOnTile (==)

-- | A `Condition` for checking if the piece (on given tile) doesn't belong to the current player
enemyTile :: Condition Turn
enemyTile = Condition $ _comparePieceOnTile (/=)

-- | A `Condition` for checking if a tile is empty
emptyTile :: Condition Turn
emptyTile = Condition _emptyTile

-- | Returns `True` if no player has any valid moves, `False` otherwise
noPlayerHasMoves :: Condition Turn
noPlayerHasMoves = Condition _noPlayerHasMoves

-- | Checks if the board contains a given number of pieces in a row in any 
--   orientation. (Vertical, horizontal, diagonal)
inARow :: Int -> Condition Turn
inARow k = Condition $ _inARow k

-- | A rule for checking if the tile below another tile is empty
tileBelowIsNotEmpty :: Condition Turn
tileBelowIsNotEmpty = Condition _tileBelowIsNotEmpty


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
turnUpLeft = Update $ _turnDirection (-1, -1)

-- | An `Update` for moving one step up and to the right, mainly for use with `IterateUntil`
turnUpRight :: Update Turn
turnUpRight = Update $ _turnDirection (1, -1)

-- | An `Update` for moving one step down and to the left, mainly for use with `IterateUntil`
turnDownLeft :: Update Turn
turnDownLeft = Update $ _turnDirection (-1, 1)

-- | An `Update` for moving one step down and to the right, mainly for use with `IterateUntil`
turnDownRight :: Update Turn
turnDownRight = Update $ _turnDirection (1, 1)


-- * Display functions
{- $display -}


-- | Prints a board in the terminal. It's pretty.
prettyPrint :: Game -> IO ()
prettyPrint game = do
    let b = board game
    putStrLn $ replicate (1 + 4 * length (head b)) '-'
    prettyPrint' $ map (map f) b

    where
        f :: Tile -> String
        f t = case t of
            Empty _ -> " "
            s       -> show s

        prettyPrint' :: [[String]] -> IO ()
        prettyPrint' [] = return ()
        prettyPrint' (b:bs)  = do
            putStrLn $ foldl (\s t -> s ++ t ++ " | ") "| " b
            putStrLn $ replicate (1 + 4 * length b) '-'
            prettyPrint' bs
