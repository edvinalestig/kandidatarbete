module TicTacToeTests (
    prop_tictactoe_gameNotEndedAfter4Moves,
    prop_tictactoe_player1canWin,
    prop_tictactoe_player2canWin,
    prop_tictactoe_noWinnerWhenDraw,
    prop_tictactoe_gameEndsWhenDraw,
    prop_tictactoe_correctPlacement,
    arbitraryTicTacToePos
) where

import Test.QuickCheck
import DSL.Types
import DSL.Utility
import DSL
import TestGames
import Data.Maybe (isNothing, fromJust)
import Data.List (nub)


prop_tictactoe_gameNotEndedAfter4Moves :: Pos -> Pos -> Pos -> Pos -> Property 
prop_tictactoe_gameNotEndedAfter4Moves t1 t2 t3 t4 = 
    length (nub [t1,t2,t3,t4]) == 4 ==> do
        let g1 = tictactoe 
            g2  = playTurn (placeTurn' p1 t1) g1
            g3  = playTurn (placeTurn' p2 t2) g2
            g4  = playTurn (placeTurn' p1 t3) g3
            g5  = playTurn (placeTurn' p2 t4) g4
        gameEnded g5 === False .&&. isNothing (winner g5)
        where
            p1 = head $ pieces tictactoe  
            p2 = last $ pieces tictactoe  

prop_tictactoe_player1canWin :: Property
prop_tictactoe_player1canWin = do
    let g1  = tictactoe 
        g2  = playTurn (placeTurn' p1 (Pos 0 0)) g1
        g3  = playTurn (placeTurn' p2 (Pos 0 1)) g2
        g4  = playTurn (placeTurn' p1 (Pos 1 1)) g3
        g5  = playTurn (placeTurn' p2 (Pos 1 2)) g4
        g6  = playTurn (placeTurn' p1 (Pos 2 2)) g5
    fromJust (winner g6) === player1 .&&. gameEnded g6
    where
        p1 = head $ pieces tictactoe  
        p2 = last $ pieces tictactoe  
        player1 = head $ players tictactoe

prop_tictactoe_player2canWin :: Property
prop_tictactoe_player2canWin = do
    let g1  = tictactoe 
        g2  = playTurn (placeTurn' p1 (Pos 0 0)) g1
        g3  = playTurn (placeTurn' p2 (Pos 1 0)) g2
        g4  = playTurn (placeTurn' p1 (Pos 2 2)) g3
        g5  = playTurn (placeTurn' p2 (Pos 1 1)) g4
        g6  = playTurn (placeTurn' p1 (Pos 2 0)) g5
        g7  = playTurn (placeTurn' p2 (Pos 1 2)) g6
    fromJust (winner g7) === player2 .&&. gameEnded g7
    where
        p1 = head $ pieces tictactoe  
        p2 = last $ pieces tictactoe  
        player2 = last $ players tictactoe

prop_tictactoe_noWinnerWhenDraw :: Property
prop_tictactoe_noWinnerWhenDraw = do
    let g1  = tictactoe 
        g2  = playTurn (placeTurn' p1 (Pos 0 0)) g1
        g3  = playTurn (placeTurn' p2 (Pos 0 1)) g2
        g4  = playTurn (placeTurn' p1 (Pos 1 1)) g3
        g5  = playTurn (placeTurn' p2 (Pos 2 2)) g4
        g6  = playTurn (placeTurn' p1 (Pos 2 0)) g5
        g7  = playTurn (placeTurn' p2 (Pos 0 2)) g6
        g8  = playTurn (placeTurn' p1 (Pos 1 2)) g7
        g9  = playTurn (placeTurn' p2 (Pos 1 0)) g8
        g10 = playTurn (placeTurn' p1 (Pos 2 1)) g9
    isNothing (winner g10) === True
    where
        p1 = head $ pieces tictactoe  
        p2 = last $ pieces tictactoe  

prop_tictactoe_gameEndsWhenDraw :: Property 
prop_tictactoe_gameEndsWhenDraw = do
    let g1  = tictactoe 
        g2  = playTurn (placeTurn' p1 (Pos 0 0)) g1
        g3  = playTurn (placeTurn' p2 (Pos 0 1)) g2
        g4  = playTurn (placeTurn' p1 (Pos 1 1)) g3
        g5  = playTurn (placeTurn' p2 (Pos 2 2)) g4
        g6  = playTurn (placeTurn' p1 (Pos 2 0)) g5
        g7  = playTurn (placeTurn' p2 (Pos 0 2)) g6
        g8  = playTurn (placeTurn' p1 (Pos 1 2)) g7
        g9  = playTurn (placeTurn' p2 (Pos 1 0)) g8
        g10 = playTurn (placeTurn' p1 (Pos 2 1)) g9
    gameEnded g10 === True
    where
        p1 = head $ pieces tictactoe  
        p2 = last $ pieces tictactoe       

-- | Tries to place a piece on a random tile on a random board
--   It checks whether it changes the game state according to
--   the rules of tictactoe.
prop_tictactoe_correctPlacement:: Property
prop_tictactoe_correctPlacement= ioProperty $ do
    b <- generate arbitraryTicTacToeBoard
    let game = tictactoe {
        board = b
    }
    x <- generate $ elements [0..2]
    y <- generate $ elements [0..2]
    let pos = Pos x y
    piece <- generate . elements $ pieces game
    let g = playTurn (placeTurn' piece pos) game

    if empty' (getTile (board game) pos) then
        return $ getTile (board g) pos === PieceTile piece pos 
                 .&&. players g =/= players game
    else
        return $ board g === board game .&&. players g === players game

-- | A generator for a random Tic-Tac-Toe board
arbitraryTicTacToeBoard :: Gen Board
arbitraryTicTacToeBoard = vectorOf 3 $ vectorOf 3 arbitrary

-- | A generator for a random position on a Tic-Tac-Toe board
arbitraryTicTacToePos :: Gen Pos
arbitraryTicTacToePos = Pos <$> elements [0..2] <*> elements [0..2]
