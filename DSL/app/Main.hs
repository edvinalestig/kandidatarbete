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
        TurnRule tileIsEmpty,
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
        TurnRule tileIsEmpty,
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
        TurnRule tileIsEmpty,
        TurnRule tileBelowIsNotEmpty,
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
        TurnRule tileIsEmpty,
        TurnRule checkSurrPieces,
        UpdateRule placePiece,
        UpdateRule changeSurrLines
    ],
    endConditions = [
        (playerWithMostPieces, noPlayerHasMoves)
    ]
}

othello2 :: Game
othello2 = emptyGame {
    board = initRectBoard 3 3 [
        ((1,2), Piece "O" (Player "A")),
        ((1,1), Piece "O" (Player "A")),
        ((2,1), Piece "O" (Player "A")),
        ((3,1), Piece "O" (Player "A")),
        ((3,2), Piece "O" (Player "A")),
        ((1,3), Piece "X" (Player "B")),
        ((2,3), Piece "X" (Player "B")),
        ((3,3), Piece "X" (Player "B"))
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
        TurnRule tileIsEmpty,
        TurnRule checkSurrPieces,
        UpdateRule placePiece,
        UpdateRule changeSurrLines
    ],
    endConditions = [
        (playerWithMostPieces, noPlayerHasMoves)
    ]
}


snakesAndLadders :: Int -> Game
snakesAndLadders x = emptyGame {
    board = initRectBoard 10 10 $ take x [
        ((1, 10), Piece "A" (Player "A")),
        ((1, 10), Piece "B" (Player "B")),
        ((1, 10), Piece "C" (Player "C")),
        ((1, 10), Piece "D" (Player "D"))
    ],
    pieces = take x [
        Piece "A" (Player "A"),
        Piece "B" (Player "B"),
        Piece "C" (Player "C"),
        Piece "D" (Player "D")
    ],
    players = take x [
        Player "A",
        Player "B",
        Player "C",
        Player "D"
    ],
    rules = [
        TurnRule tileIsEmpty
        -- MovePathRule (dice 6) path,
        -- -- Ladders
        -- AutomaticMove (1, 9) (2, 6),
        -- AutomaticMove (3, 9) (6, 8),
        -- AutomaticMove (7, 9) (9, 7),
        -- AutomaticMove (0, 7) (1, 5),
        -- AutomaticMove (7, 7) (4, 2),
        -- AutomaticMove (9, 5) (6, 3),
        -- AutomaticMove (0, 2) (1, 0),
        -- AutomaticMove (9, 2) (8, 0),
        -- -- Snakes
        -- AutomaticMove (3, 0) (2, 2),
        -- AutomaticMove (5, 0) (4, 4),
        -- AutomaticMove (7, 1) (3, 7),
        -- AutomaticMove (1, 3) (2, 8),
        -- AutomaticMove (7, 5) (5, 7),
        -- AutomaticMove (4, 6) (5, 9),
        -- AutomaticMove (8, 6) (9, 9)
    ],
    endConditions = [
        (currentPlayer, pieceAtPos (Pos 1 1))
    ]
}