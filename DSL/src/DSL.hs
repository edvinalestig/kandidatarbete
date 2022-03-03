{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments #-}

module DSL where
import Data.List (transpose, group)
import Control.Monad.Random (evalRandIO, MonadRandom (getRandomR))
import Data.Bifunctor (Bifunctor(bimap))
import Data.List.Split (chunksOf, splitOn)
import Text.Read (readMaybe)
-- data Game = Game [Equipment] [Rule]

-- tic-tac-toe
tictactoe :: Game
tictactoe = Game {
    board = newBoard 3 3,
    pieces = [
        Piece "X" (Player "X"),
        Piece "O" (Player "O")
    ],
    dice = [],
    players = [
        Player "X",
        Player "O"
    ],
    rules = [
        PlaceRule undefined
    ],
    endConditions = EndCondition {
        drawCondition = undefined,
        winCondition = undefined
    }
}

data Game = Game
    {
        board         :: Board,
        pieces        :: [Piece],
        dice          :: [Die],
        -- equipment  :: (Equipment t) => [t],
        players       :: [Player],
        rules         :: [Rule],
        endConditions :: EndCondition
    }


-- data Equipment where
--     Piece' :: Piece g t -> Equipment -- (Show g, Show t) => g -> Player t -> Equipment -- Piece g (Player t) :: Equipment
--     Dice   :: [Die] -> Equipment                              -- Dice [Die] :: Equipment


newtype Rule = PlaceRule (Pos -> Board -> Bool)
    --LegalMoves    :: [Tile t -> Tile t -> Board t -> Bool] -> Rule -- Needs changing?
    --winConditions :: [Board -> Maybe Player]

data Pos = Pos Int Int 

data EndCondition = EndCondition {
    drawCondition :: ([Bool] -> Bool) -> [Board -> Bool],
    winCondition  :: ([Bool] -> Bool) -> [Board -> Bool]
}

boardSize :: Board -> (Int, Int)
boardSize b = (length b, (length . chunksOf 3) b)

newtype Player = Player String
    deriving (Show, Eq)

data Piece = Piece String Player
    deriving (Eq)
data Tile = PieceTile Piece Int Int | Empty Int Int
    deriving (Eq)

type Board = [Tile]

newtype Die = Die Int

play :: IO ()
play = runGame tictactoe

runGame :: Game -> IO ()
runGame g = do
    winner <- runGame' g
    print winner
    where
        runGame' :: Game -> IO Player
        runGame' game = do
            let currPlayer = head $ players game
            putStrLn $ "Player " ++ show currPlayer ++ "'s turn"
            displayGame game

            let placeRules' = rules game
            input <- getValidInput placeRules' (board game)

            let piece = undefined
            --placePiece input undefined b


            runGame' $ game {players = cyclePlayers $ players game}



-- gameLoop :: Game -> IO Player
-- gameLoop g = do
--   let pl = head $ players g
--   putStrLn $ "Player " ++ show pl ++ "'s turn"
--   --displayGame $ board g

--   --putStrLn "Throw dice"
--   --moves <- evalRandIO die
--   --putStrLn $ "You rolled a " ++ show moves
--   --let piece      = pieceOfPlayer pl g
--       --newGs      = move (Increment piece moves) g
--       --afterRules = applyRules newGs

-- --   let placeRules = findPlaceRules g
--   let placeRules = placeRules $ rules g
--   input <- getValidInput placeRules
--   -- l <- getLine
--   -- check if input matches place rules
--   -- place piece on board

--   --case checkWin afterRules of
--     --Just p -> do
--       --printGame afterRules
--       --return p
--     --Nothing -> gameLoop $ cyclePlayers afterRules
--   undefined

getValidInput :: [Rule] -> Board -> IO (Int, Int)
getValidInput r b = do
    putStrLn "Write valid input"
    input <- getLine
    let xs = filterNothing (map readMaybe $ splitOn "," input :: [Maybe Int])
    if length xs /= 2 then
        getValidInput r b
    else do
        let [x, y] = xs
            valid = any (\(PlaceRule f) -> f (Pos x y) b) r

        if valid then return (x, y) else getValidInput r b
        -- case readMaybe (show (x,y)) :: Maybe (Int, Int) of
        --     Just a 

placePiece :: Piece -> Pos -> Board -> Board
placePiece p pos b = undefined

-- splitOn :: Char -> String -> (String,String)
-- splitOn c str = (takeWhile (/=c) str, tail $ dropWhile (/=c) str)

-- isPlaceRule :: Rule -> Bool
-- isPlaceRule = \case (PlaceRules _) -> True ; _ -> False

filterNothing :: [Maybe a] -> [a]
filterNothing []     = []
filterNothing (x:xs) = case x of
    Nothing -> filterNothing xs
    Just a  -> a : filterNothing xs

cyclePlayers :: [Player] -> [Player]
cyclePlayers ps = tail ps ++ [head ps]


displayGame :: Game -> IO ()
displayGame = undefined


-- * Utility functions

throwDie :: Die -> IO Int
throwDie (Die n) = evalRandIO $ getRandomR (1,n)

newBoard :: Int -> Int -> Board
newBoard w h = [Empty x y | y <- [0..h-1], x <- [0..w-1]]

-- * behöver ändras
--fourSame :: (Eq t) => [Tile t] -> Bool
--fourSame col = any (\x -> length x >= 4 && Empty `notElem` x) (group col)

