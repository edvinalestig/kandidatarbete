{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-|
Module      : DSL
Description : A Haskell module containing the majority of the code in this library

This module contains the functions that are required for actually playing a board game
-}
module DSL (
    play,
    noPlayerHasMoves
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
            if not (playerHasMoves game currPlayer) then do
                putStrLn $ "Player " ++ show currPlayer ++ "'s turn is skipped — no valid moves"
                play' $ game {players = cyclePlayers $ players game}
            else do
                putStrLn $ "Player " ++ show currPlayer ++ "'s turn"

                piece <- getValidPiece currPlayer (pieces game)
                input <- getValidInput piece (rules game) (board game)

                let r' = [UpdateRule f | (UpdateRule f) <- rules game]
                    newBoard = foldl (\b (UpdateRule x) -> x piece input b) (board game) r'
                    endCon = filter ((== True) . snd) [(p, f (game {board = newBoard})) | (p,f) <- endConditions game]

                if not (null endCon) then do
                    prettyPrint newBoard
                    return $ (fst . head) endCon game {board = newBoard}
                else
                    play' $ game {players = cyclePlayers $ players game, board = newBoard}


-- | Returns `True` if no player has any valid moves, `False` otherwise
noPlayerHasMoves :: Game -> Bool
noPlayerHasMoves g = not $ any (playerHasMoves g) (players g)

-- | Determines if a given player has any legal moves with regards to the rules and a board state
playerHasMoves :: Game -> Player -> Bool
playerHasMoves g p = playerHasMoves' (filterPieces p (pieces g)) (rules g) (board g)
    where
        playerHasMoves' :: [Piece] -> [Rule] -> Board -> Bool
        playerHasMoves' []     rs b = False
        playerHasMoves' (p:ps) rs b = pieceHasMoves p rs b || playerHasMoves' ps rs b

-- | Determines if a given piece has any legal moves with regards to the rules and a board state
pieceHasMoves :: Piece -> [Rule] -> Board -> Bool
pieceHasMoves p [] b = False
pieceHasMoves p (r:rs) b = inputs r p b || pieceHasMoves p rs b
    where
        inputs :: Rule -> Piece -> Board -> Bool
        inputs (PlaceRule x) p b = any ((\pos -> x p pos b) . getPos) (concat b)
        inputs _ _ _ = False


-- | Gets an input from the user and determines whether or not it is valid
getValidInput :: Piece -> [Rule] -> Board -> IO Pos
getValidInput p r b = do
    putStrLn "Enter desired location (format: x,y)"
    input <- getLine
    let xs = filterNothing (map readMaybe $ splitOn "," input :: [Maybe Int])
    if length xs /= 2 then
        getValidInput p r b
    else do
        let [x, y] = xs
            r' = [PlaceRule f | (PlaceRule f) <- r]
            valid = all (\(PlaceRule f) -> f p (Pos (x-1) (y-1)) b) r'

        if valid then return (Pos (x-1) (y-1)) else getValidInput p r b
        -- case re  adMaybe (show (x,y)) :: Maybe (Int, Int) of
        --     Just a 

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




