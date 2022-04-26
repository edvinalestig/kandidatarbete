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
    let g' = applyRules (Turn (Piece "" (Player "")) (Place (Pos 0 0))) g preTurnRules
    if board g' /= board g || players g' /= players g then
        playGame g'
    --if not (playerHasMoves g currPlayer) then do
        --putStrLn $ "Player " ++ show currPlayer ++ "'s turn is skipped - no valid moves"
        --playGame $ g {players = cyclePlayers $ players g}
    --else do
    else do
        putStrLn $ "Player " ++ show currPlayer ++ "'s turn"
        input <- getValidInput g
        piece <- case input of
                        Move pos _ -> return $ getPiece (board g) pos
                        Place _ -> getValidPiece currPlayer (pieces g)

        let newGame = playTurn (Turn piece input) g

        -- Check if nothing happened on the board to give feedback to the user
        -- Todo: Determine if this is the way to do it, now it assumes that all moves include changes to the board
        if board g == board newGame && players g == players newGame then 
            putStrLn "Input move does not follow the rules" >>
            playGame newGame

        else if gameEnded newGame then
            dispFunction g newGame >>
            case winner newGame of
                Nothing -> putStrLn "Draw!"
                Just p -> putStrLn $ "Player " ++ show p ++ " has won!"
        else
            playGame newGame

-- | Plays one turn and apply each rule.
playTurn :: Turn -> Game -> Game
playTurn t g | not $ isValidInput t g = g
             | otherwise = postPlayTurn t newGame
    where
        newGame = applyRules t g rules

-- | After a turn is done, apply the end conditions and cycle the players.
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
getValidInput :: Game -> IO Action
getValidInput g = do
    let r = rules g
        b = board g
    putStrLn "Enter your action (Either 'place x,y' or 'move x1,y1 x2,y2')"
    input <- getLine

    checkInterupt input

    let (func:coords) = splitOn " " input

    let action = case func of
            "place"  -> if length coords /= 1 then do
                            putStrLn "Input does not follow specification"
                            getValidInput g
                        else
                            return $ handlePlace coords
            "move"  -> if length coords /= 2 then do
                            putStrLn "Input does not follow specification"
                            getValidInput g
                        else
                            return $ handleMove coords
            _ -> do putStrLn "Input does not follow specification"
                    getValidInput g
    a <- action
    case a of
        Move pos _ -> if empty' $ getTile (board g) pos then getValidInput g else action
        Place pos -> action 



handlePlace :: [String] -> Action
handlePlace input = Place $ Pos (x-1) (y-1)
    where [x,y] = getCoordsFromInput $ head input

handleMove :: [String] -> Action
handleMove input = Move (Pos (x1-1) (y1-1)) (Pos (x2-1) (y2-1))
    where [x1,y1] = getCoordsFromInput $ head input
          [x2,y2] = getCoordsFromInput $ head (tail input)

getCoordsFromInput :: String -> [Int]
getCoordsFromInput s = catMaybes (map readMaybe $ splitOn "," s :: [Maybe Int])


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





