module SnakesAndLadders where

import           Data.List
import           Control.Monad.Random


instance Show Player where
  show (Player n) = show n

instance Show Piece where
  show (Piece p _) = show p


newtype Player = Player Int
 deriving Eq

data Board = Rect (Int, Int) [Piece]
  deriving Show

type Pos = (Int, Int)

data Piece = Piece Player Pos
 deriving Eq -- Might have to change in the future (used in move function)

data GameState = GameState [Player] Board
  deriving Show


-- Rules and moves

data Move = Increment Piece Int | Absolute Piece Pos

data Rule = AutomaticMove (GameState -> GameState) | Other

rules :: [Rule]
rules =
  -- Ladders
  [ AutomaticMove $ automaticMove (1, 9) (2, 6)
  , AutomaticMove $ automaticMove (3, 9) (6, 8)
  , AutomaticMove $ automaticMove (7, 9) (9, 7)
  , AutomaticMove $ automaticMove (0, 7) (1, 5)
  , AutomaticMove $ automaticMove (7, 7) (4, 2)
  , AutomaticMove $ automaticMove (9, 5) (6, 3)
  , AutomaticMove $ automaticMove (0, 2) (1, 0)
  , AutomaticMove $ automaticMove (9, 2) (8, 0)
  -- Snakes
  , AutomaticMove $ automaticMove (3, 0) (2, 2)
  , AutomaticMove $ automaticMove (5, 0) (4, 4)
  , AutomaticMove $ automaticMove (7, 1) (3, 7)
  , AutomaticMove $ automaticMove (1, 3) (2, 8)
  , AutomaticMove $ automaticMove (7, 5) (5, 7)
  , AutomaticMove $ automaticMove (4, 6) (5, 9)
  , AutomaticMove $ automaticMove (8, 6) (9, 9)
  ]

apply :: Rule -> GameState -> GameState
apply (AutomaticMove f) = f
apply Other             = error "N/A"

applyRules :: GameState -> GameState
applyRules g = foldr apply g rules

automaticMove :: Pos -> Pos -> GameState -> GameState
automaticMove from to gs = foldr (move . (`Absolute` to)) gs pieces
  where pieces = piecesAt from (board gs)

move :: Move -> GameState -> GameState
move (Increment (Piece player oldPos) n) (GameState players (Rect s pieces)) =
  GameState players (Rect s newPs)
 where
  newPs = Piece player (incMove oldPos n) : delete (Piece player oldPos) pieces
move (Absolute (Piece player oldPos) newPos) (GameState players (Rect s pieces))
  = GameState players (Rect s newPs)
  where newPs = Piece player newPos : delete (Piece player oldPos) pieces


checkWin :: GameState -> Maybe Player
checkWin (GameState _ (Rect _ ps)) = case atEnd of
  []          -> Nothing
  [Piece p _] -> Just p
  _           -> error "Multiple winners"
  where atEnd = filter (\(Piece _ pos) -> isWinningPos pos) ps

isWinningPos :: Pos -> Bool
isWinningPos p = winningPos == p

----------------------------------------
-- Snakes and ladders specific functions

winningPos :: Pos
winningPos = (0, 0)

incMove :: Pos -> Int -> Pos
incMove pos 0 = pos
incMove (x, y) n | isWinningPos (x, y)             = (x, y)
                 | even y && x == 0                = incMove (x, y - 1) n'
                 | odd y && x == fst boardSize - 1 = incMove (x, y - 1) n'
                 | odd y                           = incMove (x + 1, y) n'
                 | otherwise                       = incMove (x - 1, y) n'
  where n' = n - 1

boardSize :: (Int, Int)
boardSize = (10, 10)

cellSize :: (Int, Int)
cellSize = (2, 2)

startPiece :: Player -> Piece
startPiece p = Piece p (0, snd boardSize - 1)

die :: (RandomGen g) => Rand g Int
die = getRandomR (1, 6)

main :: IO ()
main = do
  let game = newGame 4
  winner' <- gameLoop game
  putStrLn $ "Player " ++ show winner' ++ " won"
  return ()

gameLoop :: GameState -> IO Player
gameLoop g = do
  let pl = currentPlayer g
  putStrLn $ "Player " ++ show pl ++ "'s turn"
  printGame g

  putStrLn "Throw dice"
  l     <- getLine
  moves <- evalRandIO die
  putStrLn $ "You rolled a " ++ show moves
  let piece      = pieceOfPlayer pl g
      newGs      = move (Increment piece moves) g
      afterRules = applyRules newGs
  case checkWin afterRules of
    Just p -> do
      printGame afterRules
      return p
    Nothing -> gameLoop $ cyclePlayers afterRules

-- Each player has only one piece, find it
pieceOfPlayer :: Player -> GameState -> Piece
pieceOfPlayer pl (GameState _ (Rect _ ps)) =
  head $ filter (\(Piece pl' _) -> pl == pl') ps

-----------------------
-- Utilityish functions

maxPlayers :: Int
maxPlayers = uncurry (+) cellSize

newGame :: Int -> GameState
newGame n = GameState players (newBoard players) where players = newPlayers n

newBoard :: [Player] -> Board
newBoard pls = Rect boardSize (map startPiece pls)

currentPlayer :: GameState -> Player
currentPlayer (GameState (p : _) _) = p

cyclePlayers :: GameState -> GameState
cyclePlayers (GameState (p : ps) b) = GameState (ps ++ [p]) b

board :: GameState -> Board
board (GameState _ b) = b

newPlayers :: Int -> [Player]
newPlayers n | n > maxPlayers = newPlayers maxPlayers
             | otherwise      = map Player [1 .. n]

piecesAt :: Pos -> Board -> [Piece]
piecesAt pos (Rect _ pieces) = filter (\(Piece _ pos') -> pos == pos') pieces

-------------------------------------------------------
-- Functions below this point are for printing the game

printGame :: GameState -> IO ()
printGame = prettyPrint . board

partPiecesAt :: Pos -> Int -> Board -> [Piece]
partPiecesAt pos part b = filter (isCorrectPart part) pieces
  where pieces = piecesAt pos b

isCorrectPart :: Int -> Piece -> Bool
isCorrectPart part (Piece (Player n) _) =
  (n - 1) `div` fst cellSize + 1 == part

printDivider :: Board -> IO ()
printDivider (Rect (w, _) _) = printDiv w
 where
  printDiv 0 = putStrLn "+"
  printDiv n = do
    putStr $ "+--" ++ line (fst cellSize)
    printDiv (n - 1)
  line :: Int -> String
  line n = replicate n '-'

prettyPrint :: Board -> IO ()
prettyPrint b = printLines 0
 where
  printLines :: Int -> IO ()
  printLines y
    | y == snd boardSize = printDivider b
    | otherwise = do
      printDivider b
      printCells 0 y 1
      printLines (y + 1)
  printCells :: Int -> Int -> Int -> IO ()
  printCells x y part
    | part > snd cellSize = return ()
    | otherwise = do
      printCellsPart x y part
      putStrLn ""
      printCells x y (part + 1)
  printCellsPart :: Int -> Int -> Int -> IO ()
  printCellsPart x y part
    | x == fst boardSize = putStr "|"
    | otherwise = do
      putStr "| "
      showPartPiecesAt (x, y) part b
      putStr " "
      printCellsPart (x + 1) y part

showPartPiecesAt :: Pos -> Int -> Board -> IO ()
showPartPiecesAt pos p b = putStr $ concatMap show pieces ++ replicate
  (fst cellSize - length pieces)
  ' '
  where pieces = partPiecesAt pos p b


