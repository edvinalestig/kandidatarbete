module Main where

import DSL
import DSL.Types
import DSL.Lib

main :: IO ()
main = play tictactoe

-- tic-tac-toe
tictactoe :: Game
tictactoe = emptyGame {
    board = rectBoard 3 3,
    pieces = [
        Piece "X" (Player "A"),
        Piece "O" (Player "B")
    ],
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


tictactoeVariant :: Int -> Int -> Int -> Game
tictactoeVariant x y z = emptyGame {
    board = rectBoard x y,
    pieces = [
        Piece "X" (Player "A"),
        Piece "O" (Player "B"),
        Piece "Z" (Player "C")
    ],
    players = [
        Player "A",
        Player "B",
        Player "C"
    ],
    rules = [
        PlaceRule tileIsEmpty
    ],
    endConditions = EndCondition {
        drawCondition = [boardIsFull],
        winCondition = [inARow z]
    }
}

connectFour :: Game
connectFour = emptyGame {
    board = rectBoard 7 6,
    pieces = [
        Piece "R" (Player "A"),
        Piece "B" (Player "B")
    ],
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
