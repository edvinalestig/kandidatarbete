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

connectFour :: Game -- WIP, doesn't work right now
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
