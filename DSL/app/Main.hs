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
        If (tileIsEmpty) placePiece
    ],
    endConditions = [
        If (inARow 3) currentPlayerWins,
        If (boardIsFull) gameDraw
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
        If (tileIsEmpty) placePiece
    ],
    endConditions = [
        If (inARow z) currentPlayerWins,
        If (boardIsFull) gameDraw
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
        If (tileIsEmpty `AND` tileBelowIsNotEmpty) placePiece
    ],
    endConditions = [
        If (inARow 4) currentPlayerWins,
        If (boardIsFull) gameDraw
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
        If (tileIsEmpty `AND` checkSurrPieces) (placePiece >=> changeSurrLines)
    ],
    endConditions = [
        If (noPlayerHasMoves) playerWithMostPiecesWins
    ]
}

othello2 :: Game
othello2 = emptyGame {
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
        If (tileIsEmpty `AND` checkSurrPieces) (placePiece 
                      >>> IterateUntil (TurnRule turnDown placePiece) (tileIsEmpty)
                      >>> IterateUntil (TurnRule turnLeft placePiece) (tileIsEmpty)
                      >>> IterateUntil (TurnRule turnUp placePiece) (tileIsEmpty)
                      >>> IterateUntil (TurnRule turnRight placePiece) (tileIsEmpty)
                      >>> IterateUntil (TurnRule turnUpLeft placePiece) (tileIsEmpty)
                      >>> IterateUntil (TurnRule turnUpRight placePiece) (tileIsEmpty)
                      >>> IterateUntil (TurnRule turnDownLeft placePiece) (tileIsEmpty)
                      >>> IterateUntil (TurnRule turnDownRight placePiece) (tileIsEmpty)
        )
    ],
    endConditions = [
        If (noPlayerHasMoves) playerWithMostPiecesWins
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
        -- (currentPlayer, pieceAtPos (Pos 1 1))
    ]
}