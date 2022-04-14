{-# LANGUAGE GADTs #-}

{-|
Module      : DSL
Description : A Haskell module containing the majority of the code in this library

This module contains the functions that are required for actually playing a board game
-}
module DSL (
    playGame,
    prettyPrint,
    playTurn
) where

import DSL.Lib
import DSL.Types
import DSL.Utility
import DSL.Run
import DSL.Internal (isValidInput, playerHasMoves, filterPieces)
import Data.List (transpose, group)
import Control.Monad.Random (evalRandIO, MonadRandom (getRandomR))
import Data.Bifunctor (Bifunctor(bimap))
import Data.List.Split (chunksOf, splitOn)
import Text.Read (readMaybe)
import Data.Maybe ( isJust, fromMaybe, catMaybes )

-- | Plays a game
playGame :: Game -> IO ()
playGame g = do
    dispFunction g g

    let currPlayer = head $ players g
    if not (playerHasMoves g currPlayer) then do
        putStrLn $ "Player " ++ show currPlayer ++ "'s turn is skipped - no valid moves"
        playGame $ g {players = cyclePlayers $ players g}
    else do
        putStrLn $ "Player " ++ show currPlayer ++ "'s turn"
        piece <- getValidPiece currPlayer (pieces g)
        input <- getValidInput piece g

        let newGame = playTurn g piece input

        -- Check if nothing happened on the board to give feedback to the user
        -- Todo: Determine if this is the way to do it, now it assumes that all moves include changes to the board
        if board g == board newGame && players g == players newGame then 
            putStrLn "Inputted move does not follow the rules" >>
            playGame newGame

        else if gameEnded newGame then
            dispFunction g newGame >>
            case winner newGame of
                Nothing -> putStrLn "Draw!"
                Just p -> putStrLn $ "Player " ++ show p ++ " has won!"
        else
            playGame newGame

-- | Plays one turn
playTurn :: Game -> Piece -> Pos -> Game
playTurn g p pos | not $ isValidInput turn g = g
                 | otherwise = postPlayTurn turn newGame
    where
        newGame = applyRules turn g rules
        turn = placeTurn' p pos

postPlayTurn :: Turn -> Game -> Game
postPlayTurn t g = newGame {players = cyclePlayers $ players newGame}
    where
        newGame = applyRules t g endConditions

-- | Given a string, check if it is equal "q" and interupt the game by throwing an error based on that.
--   If the string is not equal to "q", this function does nothing
checkInterupt :: String -> IO ()
checkInterupt s | s == "q" = error "Game interrupted" 
                | otherwise = return ()

-- | Gets an input from the user and determines whether or not it is valid
getValidInput :: Piece -> Game -> IO Pos
getValidInput p g = do
    let r = rules g
        b = board g
    putStrLn "Enter desired location (format: x,y)"
    input <- getLine

    checkInterupt input

    let xs = catMaybes (map readMaybe $ splitOn "," input :: [Maybe Int])
    if length xs /= 2 then
        putStrLn "You must write exactly two integer coordinates separated by one comma" >>
        getValidInput p g
    else do
        let [x, y] = xs
        if x `notElem` [1..length $ head b] || y `notElem` [1..length b] then
            putStrLn (show x ++ ',' : show y ++ " is not within the bounds of the board") >> 
            getValidInput p g
        else return $ Pos (x - 1) (y - 1)

-- | Apply a series of rules on the game and return the final result
applyRules :: Turn -> Game -> (Game -> [Rule]) -> Game
applyRules t g f = foldl (\g' r -> fromMaybe g' $ runRule r t g') g (f g)


-- | Asks the user for which piece they want to place
getValidPiece :: Player -> [Piece] -> IO Piece
getValidPiece player ps =
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


-- | Current player is put last in the player list
cyclePlayers :: [Player] -> [Player]
cyclePlayers ps = tail ps ++ [head ps]



