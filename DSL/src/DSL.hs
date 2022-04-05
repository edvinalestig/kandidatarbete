{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-|
Module      : DSL
Description : A Haskell module containing the majority of the code in this library

This module contains the functions that are required for actually playing a board game
-}
module DSL (
    -- play,
    noPlayerHasMoves,
    playGame,
    prettyPrint
) where

import DSL.Lib
import DSL.Types
import DSL.Utility
import Data.List (transpose, group)
import Control.Monad.Random (evalRandIO, MonadRandom (getRandomR))
import Data.Bifunctor (Bifunctor(bimap))
import Data.List.Split (chunksOf, splitOn)
import Text.Read (readMaybe)
import GHC.Base (undefined)

-- | Plays a game
playGame :: Game -> IO ()
playGame game = do
    (dispFunction game) game

    let currPlayer = head $ players game
    if not (playerHasMoves game currPlayer) then do
        putStrLn $ "Player " ++ show currPlayer ++ "'s turn is skipped - no valid moves"
        playGame $ game {players = cyclePlayers $ players game}
    else do
        putStrLn $ "Player " ++ show currPlayer ++ "'s turn"
        piece <- getValidPiece currPlayer (pieces game)
        input <- getValidInput piece game

        let (newGame, winner) = playTurn game piece input

        -- Check if nothing happened on the board to give feedback to the user
        -- Todo: Determine if this is the way to do it, now it assumes that all moves include changes to the board
        if board game == board newGame then 
            putStrLn "Inputted move does not follow the rules" >>
            playGame newGame

        else if gameEnded newGame then
            (dispFunction game) newGame >>
            case winner of
                Nothing -> putStrLn "Draw!"
                Just p -> putStrLn $ "Player " ++ show p ++ " has won!"
        else
            playGame newGame

-- | Plays one turn
playTurn :: Game -> Piece -> Pos -> (Game, Maybe Player)
playTurn game piece position = do
    if not $ isValidInput piece game position then
        (game, Nothing)
    else do
        let r'       = [UpdateRule f | (UpdateRule f) <- rules game]
            newBoard = foldl (\b (UpdateRule x) -> x piece position b) (board game) r'
            endCon   = filter ((== True) . snd) [(p, f (game {board = newBoard})) | (p,f) <- endConditions game]
            newState = game {players = cyclePlayers $ players game, board = newBoard}

        if not (null endCon) then do
            (newState {gameEnded = True}, (fst . head) endCon game {board = newBoard})
        else
            (newState {gameEnded = False}, Nothing)


-- | Returns `True` if no player has any valid moves, `False` otherwise
noPlayerHasMoves :: Game -> Bool
noPlayerHasMoves g = not $ any (playerHasMoves g) (players g)

-- | Determines if a given player has any legal moves with regards to the rules and a board state
playerHasMoves :: Game -> Player -> Bool
playerHasMoves g p = playerHasMoves' (filterPieces p (pieces g)) (rules g) (board g)
    where
        playerHasMoves' :: [Piece] -> [Rule] -> Board -> Bool
        playerHasMoves' []     rs b = False
        playerHasMoves' (p:ps) rs b = pieceHasMoves p rs b (concat b) || playerHasMoves' ps rs b

-- | Determines if a given piece has any legal moves with regards to the rules and a board state
pieceHasMoves :: Piece -> [Rule] -> Board -> [Tile] -> Bool
pieceHasMoves _ _ _ [] = False
pieceHasMoves _ [] _ _ = False
pieceHasMoves p rs b (t:ts) = inputs rs p t || pieceHasMoves p rs b ts
    where
        inputs :: [Rule] -> Piece -> Tile -> Bool
        inputs rs p t = all (\f -> f p (getPos t) b) r'
        r' = [f | (PlaceRule f) <- rs]

-- | Given a string, check if it is equal "q" and interupt the game by throwing an error based on that.
--   If the string is not equal to "q", this function does nothing
checkInterupt :: String -> IO ()
checkInterupt s | s == "q" = error "Game interupted" 
                | otherwise = return ()

-- | Gets an input from the user and determines whether or not it is valid
getValidInput :: Piece -> Game -> IO Pos
getValidInput p g = do
    let r = rules g
        b = board g
    putStrLn "Enter desired location (format: x,y)"
    input <- getLine

    checkInterupt input

    let xs = filterNothing (map readMaybe $ splitOn "," input :: [Maybe Int])
    if length xs /= 2 then
        putStrLn "You must write exactly two integer coordinates separated by one comma" >>
        getValidInput p g
    else do
        let [x, y] = xs
        if x `notElem` [1..length $ head b] || y `notElem` [1..length b] then
            putStrLn (show x ++ ',' : show y ++ " is not within the bounds of the board") >> 
            getValidInput p g
        else return $ Pos (x - 1) (y - 1)

-- | Checks whether or not you can place a piece at a specific location
isValidInput :: Piece -> Game -> Pos -> Bool
isValidInput piece game pos = all (\(PlaceRule f)  -> f piece pos (board game))
                                  [PlaceRule f | (PlaceRule f) <- (rules game)]


-- | Asks the user for which piece they want to place
getValidPiece :: Player -> [Piece] -> IO Piece
getValidPiece player ps = do
    if length filteredPieces == 1 then
        return $ head filteredPieces
    else do
        putStrLn $ "Enter a desired piece among the following [0-" ++ show (length filteredPieces - 1) ++ "]: "
        mapM_ (putStrLn . helper) filteredPieces
        input <- getLine
        case readMaybe input :: Maybe Int of
            Just a -> return $ filteredPieces !! a
            Nothing -> getValidPiece player ps

    where
        helper p = "Piece: " ++ show p
        filteredPieces = filterPieces player ps


-- | Returns a list containing all pieces that the given player can place
filterPieces :: Player -> [Piece] -> [Piece]
filterPieces _ [] = []
filterPieces player ((Piece s p):ps) =
    if player == p then
        Piece s p : filterPieces player ps
    else
        filterPieces player ps

-- | Current player is put last in the player list
cyclePlayers :: [Player] -> [Player]
cyclePlayers ps = tail ps ++ [head ps]


-- * Utility functions

-- | Throws a die
throwDie :: Die -> IO Int
throwDie (Die n) = evalRandIO $ getRandomR (1,n)






