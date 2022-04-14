{-|
Module      : Rules
Description : A Haskell module containing some pre-existing rules that can be used in games

This module contains some rules that can be used in different board games and also helper functions to these rules
-}

module DSL.Lib (
    emptyGame,

    -- * Boards
    rectBoard,
    initRectBoard,

    -- * Rules
    currentPlayerWins,
    currentPlayer,
    gameDraw,
    _boardIsFull,
    combineTurn,
    turnDown,
    turnLeft,
    turnUp,
    turnRight,
    turnUpLeft,
    turnUpRight,
    turnDownLeft,
    turnDownRight,
    allDirections,
    iteratorThen,
    changedState,
    tileIsEmpty,
    placeLine,
    allyTile,
    enemyTile,
    tileBelowIsNotEmpty,
    boardIsFull,
    playerWithMostPieces,
    playersWithMostPieces,
    playerWithMostPiecesWins,
    trueCond,
    inARow,
    getDiagonals,
    getRows,
    getColumns,
    prettyPrint,
    getTile
) where

import DSL.Types
import DSL.Utility
import Data.List
import Data.Ord
import Data.Function
import Data.Maybe (Maybe(Nothing), fromMaybe, fromJust, isJust)
import DSL.Run (runRule)

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

-- | Creates an empty rectangular board
rectBoard :: Int -> Int -> Board
rectBoard w h = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]]

-- | Creates an rectangular board with pieces in certain locations
initRectBoard :: Int -> Int -> [((Int, Int), Piece)] -> Board
initRectBoard w h []            = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]]
initRectBoard w h (((x,y), pi):ps) = board $ _placePiece (Turn pi (Place (Pos (x-1) (y-1)))) $ emptyGame {board = initRectBoard w h ps}


-- * Rules
-- | ???
placeLine :: (Int, Int) -> Rule
placeLine i = Rule $ Update $ _placeLine i

-- * this is expected to only take in Place currently.
_placeLine :: (Int, Int) -> Turn -> Game -> Game
_placeLine (dx, dy) t@(Turn a (Place (Pos x y))) g = do
    if x+dx >= 0 && x+dx < (length . head . board) g && y+dy >= 0 && y+dy < (length . board) g then do
        let g' = _placePiece (t {action = Place (Pos (x+dx) (y+dy))}) g
        _placeLine (dx, dy) (Turn a (Place (Pos (x+dx) (y+dy)))) g'
    else do
        g
_placeLine _ _ g = g


-- | "Combines" two turns by adding the coordinates of the their `Action`, maybe poorly named
combineTurn :: Rule -> Rule
combineTurn = TurnRule $ Update _combineTurn

_combineTurn :: Turn -> Turn -> Turn
_combineTurn (Turn _ (Place (Pos x' y'))) (Turn p (Place (Pos x y))) = Turn p (Place (Pos (x+x') (y+y')))
_combineTurn _ _ = error "Cannot combine turns that are not Place"

-- | An `Update` for moving one step down, mainly for use with `IterateUntil`
turnDown :: Update Turn
turnDown = Update $ _turnDirection (0, 1)

-- | An `Update` for moving one step up, mainly for use with `IterateUntil`
turnUp :: Update Turn
turnUp = Update $ _turnDirection (0, -1)

-- | An `Update` for moving one step left, mainly for use with `IterateUntil`
turnLeft :: Update Turn
turnLeft = Update $ _turnDirection (-1, 0)

-- | An `Update` for moving one step right, mainly for use with `IterateUntil`
turnRight :: Update Turn
turnRight = Update $ _turnDirection (1, 0)

-- | An `Update` for moving one step down and to the left, mainly for use with `IterateUntil`
turnDownLeft :: Update Turn
turnDownLeft = Update $ _turnDirection (-1, 1)

-- | An `Update` for moving one step up and to the left, mainly for use with `IterateUntil`
turnUpLeft :: Update Turn
turnUpLeft = Update $ _turnDirection (-1, -1)

-- | An `Update` for moving one step up and to the right, mainly for use with `IterateUntil`
turnUpRight :: Update Turn
turnUpRight = Update $ _turnDirection (1, -1)

-- | An `Update` for moving one step down and to the right, mainly for use with `IterateUntil`
turnDownRight :: Update Turn
turnDownRight = Update $ _turnDirection (1, 1)

-- | A list containing all directions
allDirections :: [Update Turn]
allDirections = [turnDown, turnUp, turnLeft,
                    turnRight, turnDownLeft, turnUpLeft, turnUpRight, turnDownRight]

-- | 
iteratorThen :: (Update Turn -> Rule) -> [Update Turn] -> Rule
iteratorThen f [] = error "no input is found"
iteratorThen f [t] = f t
iteratorThen f (t:ts) = f t >>> iteratorThen f ts

-- | 
iteratorSEQ :: (Update Turn -> Rule) -> [Update Turn] -> Rule
iteratorSEQ f [] = error "no input is found"
iteratorSEQ f [t] = f t
iteratorSEQ f (t:ts) = f t >=> iteratorSEQ f ts

-- | A condition for checking if the turn players action will apply a `Rule`
changedState :: Rule -> Condition Turn
changedState r = Condition $ _changedState r

_changedState :: Rule -> Turn -> Game -> Bool
_changedState r t g = board g /= maybe [] board mg
    where mg = runRule r t g

_turnDirection :: (Int, Int) -> Turn -> Turn -> Turn
_turnDirection (dx, dy) _ = _combineTurn $ Turn (Piece "" (Player "")) (Place (Pos dx dy))

-- | A `Rule` for a draw
gameDraw :: Rule
gameDraw = Rule $ Update makeDraw

makeDraw :: Turn -> Game -> Game
makeDraw _ g | gameEnded g = g
             | otherwise   = g {gameEnded = True, winner = Nothing}

-- | A `Rule` for the turn player winning
currentPlayerWins :: Rule
currentPlayerWins = Rule $ Update _currentPlayerWins

_currentPlayerWins :: Turn -> Game -> Game
_currentPlayerWins _ g | gameEnded g = g
                       | otherwise   = g {gameEnded = True, winner = currentPlayer g}

currentPlayer :: Game -> Maybe Player
currentPlayer g = Just $ head (players g)

-- | A `Condition` for checking if a tile belongs to the turn player
allyTile :: Condition Turn
allyTile = Condition _allyTile

_allyTile :: Turn -> Game -> Bool
_allyTile t@(Turn p _) g = do
    case getTile (board g) pos of
        (PieceTile p' _) -> p == p'
        _                -> False
    where
        pos = turnToPos t g

-- | A `Condition` for checking if a tile does not belong to the turn player
enemyTile :: Condition Turn
enemyTile = Condition _enemyTile

_enemyTile :: Turn -> Game -> Bool
_enemyTile t@(Turn p _) g = do
    case getTile (board g) pos of
        (PieceTile p' _) -> p /= p'
        _                -> False
    where
        pos = turnToPos t g


-- | A condition that is always true
trueCond :: Condition Turn
trueCond = Condition (\t g -> True)

-- | A condition that is always false
falseCond :: Condition Turn
falseCond = Condition (\t g -> True)


-- | Checks if a `Tile` at a given position is empty
isWithinBoard :: Condition Turn
isWithinBoard = Condition _isWithinBoard

_isWithinBoard :: Turn -> Game -> Bool
_isWithinBoard t g = x >= 0 && x < (length . head . board) g && y >= 0 && y < (length . board) g
    where
        (Pos x y) = turnToPos t g


-- | A `Condition` for checking if a tile is empty
tileIsEmpty :: Condition Turn
tileIsEmpty = Condition _tileIsEmpty

_tileIsEmpty :: Turn -> Game -> Bool
_tileIsEmpty t g = empty' (turnGameToTile t g)


turnGameToTile :: Turn -> Game -> Tile
turnGameToTile t g = getTile (board g) (turnToPos t g)

-- | A rule for checking if the tile below a tile is empty
tileBelowIsNotEmpty :: Condition Turn
tileBelowIsNotEmpty = Condition _tileBelowIsNotEmpty

_tileBelowIsNotEmpty :: Turn -> Game -> Bool
_tileBelowIsNotEmpty t@(Turn p _) g = do
    let maxY = length (board g) - 1 -- Bottom row
        (Pos x y) = turnToPos t g
    y >= maxY || not (_tileIsEmpty (Turn p (Place (Pos x (y+1)))) g)

-- | Checks if the board is full
boardIsFull :: Condition a
boardIsFull = Condition _boardIsFull

_boardIsFull :: a -> Game -> Bool
_boardIsFull _ g = " " `notElem` concatMap (map show) (board g)

-- | Checks if the board contains a given number of pieces in a row in any 
--   orientation. (Vertical, horizontal, diagonal)
inARow :: Int -> Condition Turn
inARow k = Condition $ _inARow k

_inARow :: Int -> Turn -> Game -> Bool
_inARow k _ g = do
    let everything = getRows b k ++ getColumns b k ++ getDiagonals b k
    any allEQ everything
    where
        b = board g

-- | Check if two tiles has the same piece on it, or if both tiles are empty 
eqTile :: Tile -> Tile -> Bool
eqTile (PieceTile p1 _) (PieceTile p2 _) = p1 == p2
eqTile (Empty _) (Empty _) = True
eqTile _ _ = False

-- * Helper functions

-- | Counts how many pieces of one type there are on the board
countPiece :: Piece -> Board -> Int
countPiece p b = length $ filter (samePiece p) (concat b)

playerWithMostPiecesWins :: Rule
playerWithMostPiecesWins = Rule $ Update _playerWithMostPiecesWins

_playerWithMostPiecesWins :: Turn -> Game -> Game
_playerWithMostPiecesWins _ g = g {winner = playerWithMostPieces g}

playerWithMostPieces :: Game -> Maybe Player
playerWithMostPieces game | length ps == 1 = Just $ head ps
                          | otherwise      = Nothing
    where
        ps = playersWithMostPieces (players game) (board game)

-- | Returns a list containing all pieces on the board belonging to a player        
playerPieces :: Player -> Board -> [Piece]
playerPieces p b = filter (\x -> getPlayer x == p) as
    where
        as = [pie | PieceTile pie pos <- concat b]


-- | Returns a list containing all players with the most pieces on the board
playersWithMostPieces :: [Player] -> Board -> [Player]
playersWithMostPieces ps b = players
         where amounts = map length $ playerPieces <$> ps <*> [b]
               amounts' = zip ps amounts
               players = [player | (player, n) <- amounts', n >= maximum amounts]


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

-- | Checks if all tiles in a list of tiles are non-empty and contain the same `Piece`
allEQ :: [Tile] -> Bool
allEQ ((Empty _):as) = False -- all empty' as
allEQ ((PieceTile p _):as) = all (samePiece p) as
allEQ _      = True

-- | Checks if a tile is empty
empty' :: Tile -> Bool
empty' (Empty _) = True
empty'  _        = False

-- | Checks if a `Piece` is the same as another `Piece` on a `Tile`
samePiece :: Piece -> Tile -> Bool
samePiece _ (Empty _) = False
samePiece p (PieceTile p2 _) = p == p2

-- * Display functions

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
