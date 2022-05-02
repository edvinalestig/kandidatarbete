module ExampleGames where

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
        If emptyTile placePiece
    ],
    endConditions = [
        If (inARow 3) currentPlayerWins,
        If boardIsFull gameDraw
    ]
}

-- Testing the move functionality
movetest :: Game
movetest = emptyGame {
    board = initRectBoard 8 8 [
        ((2,2), Piece "O" (Player "B"))
    ],
    pieces = [
        Piece "X" (Player "A"),
        Piece "O" (Player "B"),
        Piece "Y" (Player "A"),
        Piece "Z" (Player "B")

    ],
    players = [
        Player "A",
        Player "B"
    ],
    rules = [
        If emptyTile placePiece,
        If (allyTile `AND` emptyDestination)  (If (destinationIsRelativeTo (1,2)) movePiece),
        If (allyTile `AND` emptyDestination)  (If (destinationIsRelativeTo (1,-2)) movePiece),
        If (allyTile `AND` emptyDestination)  (If (destinationIsRelativeTo (-1,2)) movePiece),
        If (allyTile `AND` emptyDestination)  (If (destinationIsRelativeTo (-1,-2)) movePiece),
        If (allyTile `AND` emptyDestination)  (If (destinationIsRelativeTo (2,1)) movePiece),
        If (allyTile `AND` emptyDestination)  (If (destinationIsRelativeTo (2,-1)) movePiece),
        If (allyTile `AND` emptyDestination)  (If (destinationIsRelativeTo (-2,1)) movePiece),
        If (allyTile `AND` emptyDestination)  (If (destinationIsRelativeTo (-2,-1)) movePiece)
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
        If emptyTile placePiece
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
        If (emptyTile `AND` tileBelowIsNotEmpty) placePiece
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
    preTurnRules = [
        If (NOT playerCanPlace) skipTurn
    ],
    rules = [
        If (emptyTile `AND` changedState othelloRule)
            (placePiece >>> othelloRule)
    ],
    endConditions = [
        If noPlayerHasMoves playerWithMostPiecesWins
    ]
}

othelloRule = forEachDir allDirections (replaceUntil enemyTile allyTile)


chess :: Game
chess = emptyGame {
    board = initRectBoard 8 8 [
        ((1,7), Piece "p" (Player "White")),
        ((2,7), Piece "p" (Player "White")),
        ((3,7), Piece "p" (Player "White")),
        ((4,7), Piece "p" (Player "White")),
        ((5,7), Piece "p" (Player "White")),
        ((6,7), Piece "p" (Player "White")),
        ((7,7), Piece "p" (Player "White")),
        ((8,7), Piece "p" (Player "White")),
        ((1,8), Piece "r" (Player "White")),
        ((2,8), Piece "h" (Player "White")),
        ((3,8), Piece "b" (Player "White")),
        ((4,8), Piece "q" (Player "White")),
        ((5,8), Piece "k" (Player "White")),
        ((6,8), Piece "b" (Player "White")),
        ((7,8), Piece "h" (Player "White")),
        ((8,8), Piece "r" (Player "White")),

        ((1,2), Piece "P" (Player "Black")),
        ((2,2), Piece "P" (Player "Black")),
        ((3,2), Piece "P" (Player "Black")),
        ((4,2), Piece "P" (Player "Black")),
        ((5,2), Piece "P" (Player "Black")),
        ((6,2), Piece "P" (Player "Black")),
        ((7,2), Piece "P" (Player "Black")),
        ((8,2), Piece "P" (Player "Black")),
        ((1,1), Piece "R" (Player "Black")),
        ((2,1), Piece "H" (Player "Black")),
        ((3,1), Piece "B" (Player "Black")),
        ((4,1), Piece "Q" (Player "Black")),
        ((5,1), Piece "K" (Player "Black")),
        ((6,1), Piece "B" (Player "Black")),
        ((7,1), Piece "H" (Player "Black")),
        ((8,1), Piece "R" (Player "Black"))
    ],
    pieces = [
        Piece "P" (Player "Black"),       
        Piece "R" (Player "Black"),
        Piece "H" (Player "Black"),
        Piece "B" (Player "Black"),
        Piece "Q" (Player "Black"),
        Piece "K" (Player "Black"),
        Piece "p" (Player "White"),
        Piece "r" (Player "White"),
        Piece "h" (Player "White"),
        Piece "b" (Player "White"),
        Piece "q" (Player "White"),
        Piece "k" (Player "White")
    ],
    players = [
        Player "White",
        Player "Black"
    ],
    rules = [
        If (allyTile `AND` NOT allyDestination) $
            IfElse (isKnightMove `AND` pieceEqualToEither ["H", "h"]) movePiece $
            IfElse (isKingMove   `AND` pieceEqualToEither ["k", "K"]) movePiece $
            IfElse (isQueenMove  `AND` pieceEqualToEither ["q", "Q"]) movePiece $
            IfElse (isBishopMove `AND` pieceEqualToEither ["b", "B"]) movePiece $
            IfElse (isRookMove   `AND` pieceEqualToEither ["r", "R"]) movePiece $
            IfElse (pieceEqualTo "p") 
                (If ((destinationIsRelativeTo (0,-1) `AND` emptyDestination) `OR`
                    ((destinationIsRelativeTo (1,-1) `OR` destinationIsRelativeTo (-1,-1)) `AND` enemyDestination)
                        `OR` (pieceBelongsToRow 7 `AND` destinationIsRelativeTo (0,-2) `AND` emptyDestination))
                            movePiece)
            (If (pieceEqualTo "P") $
                If ((destinationIsRelativeTo (0,1) `AND` emptyDestination) `OR`
                    ((destinationIsRelativeTo (1,1) `OR` destinationIsRelativeTo (-1,1)) `AND` enemyDestination)
                        `OR` (pieceBelongsToRow 2 `AND` destinationIsRelativeTo (0,2) `AND` emptyDestination)) $
                            movePiece)
    ],
    endConditions = [
        If (pieceNotOnBoard "k" `OR` pieceNotOnBoard "K") currentPlayerWins
    ]
}
