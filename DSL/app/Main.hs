module Main where

import DSL
import DSL.Types
import DSL.Lib
import DSL.Utility

main :: IO ()
main = playGame tictactoe

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
        PlaceRule tileIsEmpty,
        UpdateRule placePiece
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
        PlaceRule tileIsEmpty,
        UpdateRule placePiece
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
        PlaceRule tileBelowIsNotEmpty,
        UpdateRule placePiece
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
        ((5,4), Piece "X" (Player "B")),
        ((3,4), Piece "X" (Player "B")),
        ((3,3), Piece "X" (Player "B")),
        ((6,3), Piece "O" (Player "A")),
        ((2,4), Piece "O" (Player "A")),
        ((4,2), Piece "O" (Player "A"))
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
        PlaceRule tileIsEmpty,
        PlaceRule checkSurrPieces,
        UpdateRule placePiece--,
        --UpdateRule flipSurrPieces
    ],
    endConditions = [
        (playerWithMostPieces, noPlayerHasMoves)
    ]
}
