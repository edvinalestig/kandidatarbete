{-# LANGUAGE GADTs #-}
module DSL where
import Data.List (transpose, group)
import Control.Monad.Random (evalRandIO, MonadRandom (getRandomR))

-- data Game = Game [Equipment] [Rule]

-- tic-tac-toe
tictactoe :: Game
tictactoe = Game {
    equipment = [
        BoardSize 3 3,
        Piece 'X' (Player 'X'),
        Piece 'O' (Player 'O')
    ],
    rules = [
        PlaceRules [const False]
    ],
    winCondition = [
        Con (const True)
    ]
}

data Game = Game
    { 
        equipment :: [Equipment],
        rules :: [Rule],
        winCondition :: [WinCondition]
    }

data Equipment where
    BoardSize :: Int -> Int -> Equipment
    Piece :: t -> Player t -> Equipment
    Dice :: [Die] -> Equipment

data Rule where
    PlaceRules :: [Tile t -> Bool] -> Rule
    LegalMoves :: [Tile t -> Tile t -> Bool] -> Rule

data WinCondition where
    Con :: (Board t -> Bool) -> WinCondition

boardSize :: (Int, Int)
boardSize = (3, 3)

newtype Player t = Player t deriving (Eq)

data Tile t = Tile (Player t) | Empty deriving (Eq)-- MultiTile [Player t]

type Board t = [[Tile t]]

newtype Die = Die Int

throwDie :: Die -> IO Int
throwDie (Die n) = evalRandIO $ getRandomR (1,n)

newBoard :: Board t
newBoard = replicate (fst boardSize) $ replicate (snd boardSize) Empty


getRow :: Board t -> Int -> [Tile t]
getRow b n = getRows b !! n

-- | Return the board as a list of rows
getRows :: Board t -> Board t
getRows = getColumns . transpose

getColumn :: Board t -> Int -> [Tile t]
getColumn b n = getColumns b !! n

getColumns :: Board t -> Board t
getColumns = id


win :: (Eq t) => Board t -> Bool
win b = winningRow b || winningColumn b || winningDiagonal b

tie :: (Eq t) => Board t -> Bool 
tie = all (notElem Empty)

winningRow :: (Eq t) => Board t -> Bool
winningRow = winningColumn . transpose

winningColumn :: (Eq t) => Board t -> Bool
winningColumn b = or [fourSame col | col<-b]

winningDiagonal :: (Eq t) => Board t -> Bool
winningDiagonal b =
   any fourSame (getDiagonal1 b) || any fourSame (getDiagonal2 b)

-- stulen från devin
getDiagonal1 :: Board t -> Board t
getDiagonal1 b = concat [[[(b !! (y + k')) !! (x + k') | k' <- [0..3]]
                            | x <- [0..length (head b) - 4]]
                            | y <- [0..length b - 4]] 


getDiagonal2 :: Board t -> Board t
getDiagonal2 = getDiagonal1 . reverse

-- * behöver ändras
fourSame :: (Eq t) => [Tile t] -> Bool
fourSame col = any (\x -> length x >= 4 && Empty `notElem` x) (group col)

someFunc :: IO ()
someFunc = putStrLn "someFunc"




