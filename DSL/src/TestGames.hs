module TestGames where

import DSL.Types
import DSL.Lib
import DSL.Utility
import DSL

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
        If tileIsEmpty placePiece
    ],
    endConditions = [
        If (inARow 3) currentPlayerWins,
        If boardIsFull gameDraw
    ]
}

-- | m,n,k-game
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
        If tileIsEmpty placePiece
    ],
    endConditions = [
        If (inARow z) currentPlayerWins,
        If boardIsFull gameDraw
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
        If boardIsFull gameDraw
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
        If (tileIsEmpty `AND` changedState othelloRule)
            (placePiece >>> othelloRule)
    ],
    endConditions = [
        If noPlayerHasMoves playerWithMostPiecesWins
    ]
}

-- Currently contains a lamba function, change for later by making less general?
othelloRule :: Rule
othelloRule = iteratorThen (\t -> 
    IterateUntil (TurnRule t (If enemyTile placePiece)) allyTile) allDirections

othello2 :: Game
othello2 = emptyGame {
    board = initRectBoard 8 8 [
        ((4,4), Piece "O" (Player "A")),
        ((5,5), Piece "X" (Player "B")),
        ((3,3), Piece "O" (Player "A")),
        ((3,2), Piece "O" (Player "A")),
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
        If (tileIsEmpty `AND` changedState othelloRule)
            (placePiece >>> othelloRule)
    ],
    endConditions = [
        If noPlayerHasMoves playerWithMostPiecesWins
    ]
}
