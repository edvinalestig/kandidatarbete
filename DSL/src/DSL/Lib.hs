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
    tileIsEmpty,
    placeLine,
    allyTile,
    tileBelowIsNotEmpty,
    boardIsFull,
    playerWithMostPieces,
    playersWithMostPieces,
    playerWithMostPiecesWins,
    checkSurrPieces,
    trueCond,
    changeSurrLines,
    getDiagonalTiles,
    inARow,
    checkSurrLine,
    pieceAtPos,
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
import Data.Maybe (Maybe(Nothing))

-- | An empty `Game` 
emptyGame :: Game
emptyGame = Game {
    board = undefined,
    pieces = [],
    dice = [],
    path = const,
    players = [],
    rules = [],
    endConditions = [],
    winner = Nothing,
    gameEnded = False,
    dispFunction = prettyPrint
}

-- * Boards

-- | Creates a rectangular board
rectBoard :: Int -> Int -> Board
rectBoard w h = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]]

initRectBoard :: Int -> Int -> [((Int, Int), Piece)] -> Board
initRectBoard w h []            = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]]
initRectBoard w h (((x,y), pi):ps) = board $ _placePiece (Turn pi (Place (Pos (x-1) (y-1)))) $ emptyGame {board = initRectBoard w h ps}


-- * Rules

placeLine :: (Int, Int) -> NewRule
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


gameDraw :: NewRule
gameDraw = Rule $ Update makeDraw

makeDraw :: Turn -> Game -> Game
makeDraw _ g | gameEnded g = g
             | otherwise   = g {gameEnded = True, winner = Nothing}

currentPlayerWins :: NewRule
currentPlayerWins = Rule $ Update _currentPlayerWins

_currentPlayerWins :: Turn -> Game -> Game
_currentPlayerWins _ g | gameEnded g = g
                       | otherwise   = g {gameEnded = True, winner = currentPlayer g}

currentPlayer :: Game -> Maybe Player
currentPlayer g = Just $ head (players g)

allyTile :: Condition Turn 
allyTile = Condition _allyTile

_allyTile :: Turn -> Game -> Bool
_allyTile t@(Turn p _) g = do
    case getTile (board g) pos of
        (PieceTile p' _) -> p == p'
        _                -> False
    where
        pos = turnToPos t g

trueCond :: Condition Turn
trueCond = Condition (\t g -> True)


-- | Checks if a `Tile` at a given position is empty
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
        -- this will be buggy with Step

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

checkSurrPieces :: Condition Turn
checkSurrPieces = Condition _checkSurrPieces

_checkSurrPieces :: Turn -> Game -> Bool
_checkSurrPieces t@(Turn p _) g = a || c || d || e
    where
        (Pos x y) = turnToPos t g
        b = board g
        col = transpose b !! x
        row = b !! y
        a = checkSurrLine p (reverse (take y col)) || checkSurrLine p (drop (y+1) col)
        c = checkSurrLine p (reverse (take x row)) || checkSurrLine p (drop (x+1) row)
        d = checkSurrLine p (getDiagonalTiles (reverse b) (Pos x (length b - 1 - y))) || checkSurrLine p (getDiagonalTiles b (Pos x y))
        e = checkSurrLine p (getDiagonalTiles (reverse (map reverse b)) (Pos (length b - 1 - x) (length b - 1 - y))) || checkSurrLine p (getDiagonalTiles (map reverse b) (Pos (length b - 1 - x) y))


changeSurrLines :: NewRule
changeSurrLines = Rule $ Update _changeSurrLines

_changeSurrLines :: Turn -> Game -> Game
_changeSurrLines t@(Turn p _) g = foldl (changeSurrLine t) g ts
    where
        (Pos x y) = turnToPos t g
        b = board g
        col = transpose b !! x
        row = b !! y
        ts = [reverse (take y col),
              drop (y+1) col,
              reverse (take x row),
              drop (x+1) row,
              getDiagonalTiles (reverse b) (Pos x (length b - 1 - y)),
              getDiagonalTiles b (Pos x y),
              getDiagonalTiles (reverse (map reverse b)) (Pos (length b - 1 - x) (length b - 1 - y)),
              getDiagonalTiles (map reverse b) (Pos (length b - 1 - x) y)]


checkSurrLine :: Piece -> [Tile] -> Bool
checkSurrLine pie [] = False
checkSurrLine pie ts
    | length ts < 2 = False
    | otherwise = noEmptyTiles && (pie == la) && pie /= ot
    where
        arr = concat $ take 2 (groupBy eqTile ts)
        noEmptyTiles = " " `notElem` map show arr
        (PieceTile la _) = last arr
        (PieceTile ot _) = head arr

changeSurrLine :: Turn -> Game -> [Tile] -> Game
changeSurrLine t@(Turn p _) g tss@((PieceTile p2 pos):ts)
    | p /= p2 && checkSurrLine p tss = changeSurrLine t (_placePiece (t {action = Place pos}) g) ts
changeSurrLine _ g _ = g --error "changeSurrLine: Empty tile reached."

pieceAtPos :: Pos -> Game -> Bool
pieceAtPos pos g = case getTile (board g) pos of
        (Empty _) -> False
        _         -> True

-- | Gets a list of all diagonals of a certain length on the board
getDiagonalTiles :: Board -> Pos -> [Tile]
getDiagonalTiles b (Pos x y) = tail [getTile b (Pos (x+n) (y+n)) | n <- take (min(length b - y) (length (head b) - x)) [0..]]

-- | Check if two tiles has the same piece on it, or if both tiles are empty 
eqTile :: Tile -> Tile -> Bool
eqTile (PieceTile p1 _) (PieceTile p2 _) = p1 == p2
eqTile (Empty _) (Empty _) = True
eqTile _ _ = False

-- checkSurrLine (Piece "O" (Player "A")) [Empty (1,1), PieceTile (Piece "X" (Player "B")) (Pos 1 2), PieceTile (Piece "O" (Player "A")) (Pos 1 3)]

-- * Helper functions

-- | Counts how many pieces of one type there are on the board
countPiece :: Piece -> Board -> Int
countPiece p b = length $ filter (samePiece p) (concat b)

playerWithMostPiecesWins :: NewRule
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

--------- Display functions ---------

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
