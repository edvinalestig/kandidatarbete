{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module DSL where
import Data.List (transpose, group)
import Control.Monad.Random (evalRandIO, MonadRandom (getRandomR))
import Data.Bifunctor (Bifunctor(bimap))
import Data.List.Split (chunksOf)
-- data Game = Game [Equipment] [Rule]

-- tic-tac-toe
tictactoe :: Game
tictactoe = Game {
    board = newBoard 3 3,
    equipment = [
        Piece "X" (Player "X"),
        Piece "O" (Player "O")
    ],
    players = [
        Player "X",
        Player "O"
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
        board :: Board String,
        equipment :: [Equipment],
        players :: [Player String],
        rules :: [Rule],
        winCondition :: [WinCondition]
    }

data Equipment = Piece String (Player String) | Dice Int
--data Equipment where
    --Board :: Int -> Int -> Equipment
    --Piece :: t -> Player t -> Equipment
    --Dice :: [Die] -> Equipment

data Rule where
    PlaceRules :: [Tile t -> Bool] -> Rule
    LegalMoves :: [Tile t -> Tile t -> Bool] -> Rule
    WinConditions :: [Board t -> Maybe (Player t)] -> Rule

data RuleInternal where
    PlaceRule :: TilePos t -> Board t -> (TilePos t -> Bool) -> RuleInternal
    LegalMove :: TilePos t -> (TilePos t -> TilePos t -> Bool) -> RuleInternal
    WinCondition :: TilePos t -> (Board t -> Maybe (Player t)) -> RuleInternal


-- runEquipment :: Equipment -> Equipment 
-- runEquipment (Board a b) = undefined 
-- runEquipment (Piece t p) = undefined 
-- runEquipment (Dice ds) = undefined 

runRules :: RuleInternal -> RuleInternal
runRules (PlaceRule t b f) = undefined
runRules (LegalMove t f) = undefined
runRules (WinCondition t f) = undefined


data WinCondition where
    Con :: (Board t -> Bool) -> WinCondition

boardSize :: Board t -> (Int, Int)
boardSize b = (length b, (length . chunksOf 3) b)

newtype Player t = Player t deriving (Eq)

data Tile t = PlayerTile (Player t) | Empty deriving (Eq)-- MultiTile [Player t]
data TilePos t = Pos Int Int (Tile t)
type Board t = [TilePos t]

newtype Die = Die Int

play :: IO ()
play = undefined

gameLoop :: Game -> IO (Player t)
gameLoop g = do
  let pl = head $ players g
  putStrLn $ "Player " ++ show pl ++ "'s turn"
  displayGame $ board g

  --putStrLn "Throw dice"
  --moves <- evalRandIO die
  --putStrLn $ "You rolled a " ++ show moves
  --let piece      = pieceOfPlayer pl g
      --newGs      = move (Increment piece moves) g
      --afterRules = applyRules newGs
  
--   let placeRules = findPlaceRules g
  let placeRules = filter isPlaceRule $ rules g
  input <- getValidInput placeRules
  -- l <- getLine
  -- check if input matches place rules
  -- place piece on board
  
  --case checkWin afterRules of
    --Just p -> do
      --printGame afterRules
      --return p
    --Nothing -> gameLoop $ cyclePlayers afterRules
  undefined

getValidInput :: [Rule] -> IO (Int, Int)
getValidInput r = do
    putStrLn "Write valid input"
    input <- getLine
    let (x, y) = bimap read read $ splitOn ',' input

    undefined

splitOn :: Char -> String -> (String,String)
splitOn c str = (takeWhile (/=c) str, tail $ dropWhile (/=c) str)

currentPlayer :: [Player t] -> Player t
currentPlayer = head

isPlaceRule :: Rule -> Bool
isPlaceRule = \case (PlaceRules _) -> True ; _ -> False

cyclePlayers :: [Player t] -> [Player t]
cyclePlayers ps = tail ps ++ [head ps]


displayGame :: Board t -> IO ()
displayGame = undefined



-- prettyPrint :: Board t -> IO ()
-- prettyPrint b = do
--     putStrLn $ replicate (1 + 4 * length (head b)) '-'
--     prettyPrint' $ (map f) b

--     where
--         f :: TilePos t -> String
--         f t = case t of
--             Pos _ _ Empty -> " "
--             Pos _ _ s     -> show s

--         prettyPrint' :: [String] -> IO ()
--         prettyPrint' [] = return ()
--         prettyPrint' (b:bs)  = do
--             putStrLn $ foldl (\s t -> s ++ t ++ " | ") "| " b
--             putStrLn $ replicate (1 + 4 * length b) '-'
--             prettyPrint' bs


-- * Utility functions

throwDie :: Die -> IO Int
throwDie (Die n) = evalRandIO $ getRandomR (1,n)

newBoard :: Int -> Int -> Board String
newBoard w h = [Pos x y Empty | y <- [0..h-1], x <- [0..w-1]]


--getRow :: Board t -> Int -> [Tile t]
--getRow b n = getRows b !! n

-- | Return the board as a list of rows
--getRows :: Board t -> Board t
--getRows = getColumns . transpose

--getColumn :: Board t -> Int -> [Tile t]
--getColumn b n = getColumns b !! n

getColumns :: Board t -> Board t
getColumns = id


--checkWin :: (Eq t) => Board t -> Bool
--checkWin b = winningRow b || winningColumn b || winningDiagonal b

--tie :: (Eq t) => Board t -> Bool 
--tie = all (notElem Empty)

--winningRow :: (Eq t) => Board t -> Bool
--winningRow = winningColumn . transpose

--winningColumn :: (Eq t) => Board t -> Bool
--winningColumn b = or [fourSame col | col<-b]

--winningDiagonal :: (Eq t) => Board t -> Bool
--winningDiagonal b =
   --any fourSame (getDiagonal1 b) || any fourSame (getDiagonal2 b)

-- stulen från devin
--getDiagonal1 :: Board t -> Board t
--getDiagonal1 b = concat [[[(b !! (y + k')) !! (x + k') | k' <- [0..3]]
                           -- | x <- [0..length (head b) - 4]]
                           -- | y <- [0..length b - 4]] 


--getDiagonal2 :: Board t -> Board t
--getDiagonal2 = getDiagonal1 . reverse

-- * behöver ändras
fourSame :: (Eq t) => [Tile t] -> Bool
fourSame col = any (\x -> length x >= 4 && Empty `notElem` x) (group col)

someFunc :: IO ()
someFunc = putStrLn "someFunc"




