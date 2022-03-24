{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-|
Module      : DSL
Description : A Haskell module containing the majority of the code in this library

This module contains the functions that are required for actually playing a board game
-}
module DSL (
    play
) where
    
import DSL.Lib
import DSL.Types
import DSL.Utility
import Data.List (transpose, group)
import Control.Monad.Random (evalRandIO, MonadRandom (getRandomR))
import Data.Bifunctor (Bifunctor(bimap))
import Data.List.Split (chunksOf, splitOn)
import Text.Read (readMaybe)


-- | Runs a `Game`
play :: Game -> IO ()
play g = do
    winner <- play' g
    case winner of
        Nothing -> putStrLn "Draw!"
        Just p -> putStrLn $ "Player " ++ show p ++ " has won!"
    where
        play' :: Game -> IO (Maybe Player)
        play' game = do
            prettyPrint $ board game
            let currPlayer = head $ players game
                placeRules' = rules game
            putStrLn $ "Player " ++ show currPlayer ++ "'s turn"

            input <- getValidInput placeRules' (board game)

            piece <- getValidPiece currPlayer (pieces game)
            let newBoard = placePiece piece input (board game)
                endCon = filter ((== True) . snd) [(p, f newBoard) | (p,f) <- endConditions game] 
                            
            if not (null endCon) then do
                prettyPrint newBoard
                return $ (fst . head) endCon game {board = newBoard}
            else
                play' $ game {players = cyclePlayers $ players game, board = newBoard}



-- | Gets an input from the user and determines whether or not it is valid
getValidInput :: [Rule] -> Board -> IO Pos
getValidInput r b = do
    putStrLn "Enter desired location (format: x,y)"
    input <- getLine
    let xs = filterNothing (map readMaybe $ splitOn "," input :: [Maybe Int])
    if length xs /= 2 then
        getValidInput r b
    else do
        let [x, y] = xs
            valid = all (\(PlaceRule f) -> f (Pos (x-1) (y-1)) b) r

        if valid then return (Pos (x-1) (y-1)) else getValidInput r b
        -- case readMaybe (show (x,y)) :: Maybe (Int, Int) of
        --     Just a 

-- | Asks the user for which piece they want to place
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

-- | Prints a board in the terminal. It's pretty.
prettyPrint :: Board -> IO ()
prettyPrint b = do
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




