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
    endConditions = [
        (currentPlayer, inARow 3),
        (draw, boardIsFull)
    ]
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
    endConditions = [
        (currentPlayer, inARow z),
        (draw, boardIsFull)
    ]
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
    endConditions = [
        (currentPlayer, inARow 4),
        (draw, boardIsFull)
    ]
}

othello :: Game
othello = emptyGame {
    board = initRectBoard 8 8 [
        ((4,4), Piece "O" (Player "A")),
        ((5,5), Piece "O" (Player "A")),
        ((4,5), Piece "X" (Player "B")),
        ((5,4), Piece "X" (Player "B"))
    ],
    pieces = [
        Piece "O" (Player "A"),
        Piece "X" (Player "B")
    ],
    players = [
        Player "A",
        Player "B"
    ],
    rules = [
        PlaceRule tileIsEmpty
    ],
    endConditions = [
        (currentPlayer, inARow 4),
        (draw, boardIsFull)
--      (playerWithMostPieces, noPlayerHasMoves)
    ]
}
