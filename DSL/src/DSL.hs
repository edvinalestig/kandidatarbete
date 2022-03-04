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
        Piece "X" (Player "A"),
        Piece "O" (Player "B")
    ],
    dice = [],
    players = [
        Player "A",
        Player "B"
    ],
    rules = [
        PlaceRule tileIsEmpty
    ],
    endConditions = EndCondition {
        drawCondition = [boardIsFull],
        winCondition = [inARow 3]
    }
}

connectFour :: Game -- WIP, doesn't work right now
connectFour = Game {
    board = newBoard 7 6,
    pieces = [
        Piece "R" (Player "A"),
        Piece "B" (Player "B")
    ],
    dice = [],
    players = [
        Player "A",
        Player "B"
    ],
    rules = [
        PlaceRule tileIsEmpty,
        PlaceRule tileBelowIsNotEmpty
    ],
    endConditions = EndCondition {
        drawCondition = [boardIsFull],
        winCondition = [inARow 4]
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
    deriving (Eq, Show)

data EndCondition = EndCondition {
    drawCondition :: [Board -> Bool],
    winCondition  :: [Board -> Bool]
}

boardSize :: Board -> (Int, Int)
boardSize b = (length b, (length . chunksOf 3) b)

newtype Player = Player String
    deriving (Eq)

data Piece = Piece String Player
    deriving (Eq)
data Tile = PieceTile Piece Pos | Empty Pos -- Can Pos be removed?
    deriving (Eq)

type Board = [[Tile]] -- List of rows

newtype Die = Die Int
instance Show Player where
    show (Player p) = p
instance Show Tile where
    show (PieceTile p _) = show p
    show (Empty _) = " "

instance Show Piece where
    show (Piece s _) = s
    

play :: IO ()
play = runGame tictactoe

playC4 :: IO ()
playC4 = runGame connectFour

runGame :: Game -> IO ()
runGame g = do
    winner <- runGame' g
    case winner of
        Nothing -> putStrLn "Draw!"
        Just p -> putStrLn $ "Player " ++ show p ++ " has won!"
    where
        runGame' :: Game -> IO (Maybe Player)
        runGame' game = do
            prettyPrint $ board game
            let currPlayer = head $ players game
                placeRules' = rules game
            putStrLn $ "Player " ++ show currPlayer ++ "'s turn"

            input <- getValidInput placeRules' (board game)

            piece <- getValidPiece currPlayer (pieces game)
            let newBoard = placePiece piece input (board game)
                winCon = or $ winCondition (endConditions game) <*> [newBoard]
                drawCon = or $ drawCondition (endConditions game) <*> [newBoard]
            
            -- Check win or draw
            if winCon then do
                prettyPrint newBoard
                return $ Just currPlayer
            else if drawCon then do
                prettyPrint newBoard
                return Nothing
            else
                runGame' $ game {players = cyclePlayers $ players game, board = newBoard}



getValidInput :: [Rule] -> Board -> IO Pos
getValidInput r b = do
    putStrLn "Enter desired location (format: x,y)"
    input <- getLine
    let xs = filterNothing (map readMaybe $ splitOn "," input :: [Maybe Int])
    if length xs /= 2 then
        getValidInput r b
    else do
        let [x, y] = xs
            valid = all (\(PlaceRule f) -> f (Pos x y) b) r

        if valid then return (Pos x y) else getValidInput r b
        -- case readMaybe (show (x,y)) :: Maybe (Int, Int) of
        --     Just a 

getValidPiece :: Player -> [Piece] -> IO Piece
getValidPiece player ps = do
    putStrLn $ "Enter a desired piece among the following [0-" ++ show (length filteredPieces - 1) ++ "]: "
    mapM_ (putStrLn . helper) filteredPieces
    input <- getLine
    case readMaybe input :: Maybe Int of
        Just a -> return $ filteredPieces !! a
        Nothing -> getValidPiece player ps

    where
        helper p = "Piece: " ++ show p
        filteredPieces = filterPieces player ps
        

filterPieces :: Player -> [Piece] -> [Piece]
filterPieces _ [] = []
filterPieces player ((Piece s p):ps) = 
    if player == p then
        Piece s p : filterPieces player ps
    else
        filterPieces player ps


placePiece :: Piece -> Pos -> Board -> Board
placePiece p (Pos x y) b = replaceAtIndex x newRow b
    where tile = PieceTile p (Pos x y)
          newRow = replaceAtIndex y tile (b !! x)


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
newBoard w h = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]]

replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs


prettyPrint :: Board -> IO ()
prettyPrint b = do
    putStrLn $ replicate (1 + 4 * length b) '-'
    prettyPrint' $ map (map f) (transpose b)

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


-- * behöver ändras


-- * BASIC CONDITIONS

-- VERY UNORGANIZED WORK IN PROGRESS

tileIsEmpty :: Pos -> Board -> Bool
tileIsEmpty (Pos x y) board = empty' $ (board !! x) !! y

tileBelowIsNotEmpty :: Pos -> Board -> Bool -- Only used in connect four atm, might not work
tileBelowIsNotEmpty (Pos x y) board = do 
    let maxY = length board - 1 -- Bottom row
    y >= maxY || not (tileIsEmpty (Pos x (y+1)) board)

boardIsFull :: Board -> Bool
boardIsFull b = " " `notElem` concatMap (map show) b

inARow :: Int -> Board -> Bool -- any orientation (vertical, horizontal or diagonal)
-- inARow n b = checkWinner b n
inARow = flip checkWinner

-- from mnk, works
getDiagonals :: Board -> Int -> [[Tile]]
getDiagonals b k = getDiagonals' b k ++ getDiagonals' (map reverse b) k
    where
        getDiagonals' b k = concat [[[(b !! (y + k')) !! (x + k') | k' <- [0..k-1]]
                            | x <- [0..length (head b) - k]]
                            | y <- [0..length b - k]] 

getRows :: Board -> Int -> [[Tile]]
getRows b len = concat [[take len (drop n r) | n <- [0..(length r - len)]] | r <- b]

getColumns :: Board -> Int -> [[Tile]]
getColumns b = getRows (transpose b)  

checkWinner :: Board -> Int -> Bool
checkWinner b k = do
    let everything = getRows b k ++ getColumns b k ++ getDiagonals b k
    any allEQ everything

allEQ :: [Tile] -> Bool
allEQ ((Empty _):as) = False -- all empty' as
allEQ ((PieceTile p _):as) = all (samePiece p) as
allEQ _      = True

empty' :: Tile -> Bool
empty' (Empty _) = True
empty'  _         = False

samePiece :: Piece -> Tile -> Bool
samePiece _ (Empty _) = False
samePiece p (PieceTile p2 _) = p == p2

--UGLY

