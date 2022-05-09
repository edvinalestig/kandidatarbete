module Main where

import DSL
import ExampleGames

import Text.Read ( readMaybe )

main = do
    putStrLn "Select a game (write the number between 1 and 5):"
    putStrLn "1. Chess"
    putStrLn "2. Connect Four"
    putStrLn "3. mnk"
    putStrLn "4. othello"
    putStrLn "5. tic-tac-toe"
    
    gameId <- getInput

    case gameId of
        1 -> playGame chess
        2 -> playGame connectFour
        3 -> playGame (tictactoeVariant 12 10 4)
        4 -> playGame othello
        5 -> playGame tictactoe
        _ -> do putStrLn "input was not recognized"
                main

getInput :: IO Int
getInput = do
    input <- getLine

    case readMaybe input :: Maybe Int of
                    Just a -> if a > 0 && a < 6 then return a else putStrLn "invalid input" >> getInput
                    Nothing -> putStrLn "invalid input" >> getInput