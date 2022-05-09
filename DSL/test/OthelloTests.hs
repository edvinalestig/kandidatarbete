module OthelloTests (
    prop_othello_correctChangesStraightLines,
    prop_othello_correctChangesDiagonalLines,
    prop_othello_gameEndedWhenFullBoard,
    prop_othello_gameEndedWhenNoPlayerHasLegalMove,
    prop_othello_playerWithTheMostPiecesWins,
    prop_othello_drawOnEqualNumberOfPieces,
    prop_othello_invalidMoveChangesNothing
) where

import Test.QuickCheck
import DSL.Types
import DSL
import DSL.Lib
import ExampleGames
import Data.Maybe (isNothing)

prop_othello_correctChangesStraightLines :: Property
prop_othello_correctChangesStraightLines = do
    let g1 = othello { board = parseBoard [
            [m, m, m, o, m, m, m],
            [m, x, m, x, m, x, m],
            [m, m, x, x, x, m, m],
            [o, x, x, m, x, x, o],
            [m, m, x, x, x, m, m],
            [m, x, m, x, m, x, m],
            [m, m, m, o, m, m, m]
        ]
    }
    let g2 = playTurn' o (Pos 3 3) g1
    let expected = parseBoard [
            [m, m, m, o, m, m, m],
            [m, x, m, o, m, x, m],
            [m, m, x, o, x, m, m],
            [o, o, o, o, o, o, o],
            [m, m, x, o, x, m, m],
            [m, x, m, o, m, x, m],
            [m, m, m, o, m, m, m]]
    board g2 === expected

prop_othello_correctChangesDiagonalLines :: Property
prop_othello_correctChangesDiagonalLines = do
    let g1 = othello { board = parseBoard [
            [o, m, m, m, m, m, o],
            [m, x, m, x, m, x, m],
            [m, m, x, x, x, m, m],
            [m, x, x, m, x, x, m],
            [m, m, x, x, x, m, m],
            [m, x, m, x, m, x, m],
            [o, m, m, m, m, m, o]
        ]
    }
    let g2 = playTurn' o (Pos 3 3) g1
    let expected = parseBoard [
            [o, m, m, m, m, m, o],
            [m, o, m, x, m, o, m],
            [m, m, o, x, o, m, m],
            [m, x, x, o, x, x, m],
            [m, m, o, x, o, m, m],
            [m, o, m, x, m, o, m],
            [o, m, m, m, m, m, o]]
    board g2 === expected

prop_othello_gameEndedWhenFullBoard :: Property
prop_othello_gameEndedWhenFullBoard = do
    let g1 = othello { board = parseBoard [
            [x, o, o],
            [o, x, x],
            [x, m, o]
        ]
    }
    let g2 = playTurn' o (Pos 1 2) g1
    gameEnded g2 === True .&&. gameEnded g1 === False

prop_othello_gameEndedWhenNoPlayerHasLegalMove :: Property
prop_othello_gameEndedWhenNoPlayerHasLegalMove = do
    let g1 = othello { board = parseBoard [
            [m, m, x, o, o, o, o, o],
            [m, x, x, x, x, x, m, o],
            [x, x, x, x, x, x, x, o],
            [x, x, x, x, x, x, x, o],
            [x, x, x, x, x, o, x, o],
            [x, x, x, x, x, m, x, o],
            [x, x, x, x, x, x, x, o],
            [m, x, x, x, x, x, m, m]
        ]
    }
    let g2 = playTurn' o (Pos 1 0) g1
    let g3 = playTurn' x (Pos 5 5) g2
    gameEnded g3 === True .&&. gameEnded g2 === False

prop_othello_playerWithTheMostPiecesWins :: Property 
prop_othello_playerWithTheMostPiecesWins = do
    -- O should win
    let g1 = othello { board = parseBoard [
            [m, o, x],
            [x, o, x],
            [o, x, x]
        ]
    }
    let g2 = playTurn' o (Pos 0 0) g1
    -- X should win
    let h1 = othello { board = parseBoard [
            [m, o, x],
            [x, x, x],
            [m, x, o]
        ]
    }
    let h2 = playTurn' o (Pos 0 2) h1
    let h3 = playTurn' x (Pos 0 0) h2
    winner g2 === playerO .&&. winner h3 == playerX
        where
            playerO = Just (head $ players othello)
            playerX = Just (last $ players othello)

prop_othello_drawOnEqualNumberOfPieces :: Property 
prop_othello_drawOnEqualNumberOfPieces = do
    let g1 = othello { board = parseBoard [
            [m, o, x, x],
            [x, o, x, x],
            [x, o, x, x],
            [o, o, x, x]
        ]
    }
    let g2 = playTurn' o (Pos 0 0) g1
    gameEnded g2 === True .&&. isNothing (winner g2) === True

prop_othello_invalidMoveChangesNothing :: Property 
prop_othello_invalidMoveChangesNothing = do
    let g1 = othello { board = parseBoard [
            [m, o, m, o, m],
            [o, x, x, x, o],
            [m, x, m, x, m],
            [o, x, x, x, o],
            [m, o, m, o, m]
        ]
    }
    let g2 = playTurn' o (Pos 2 2) g1
    -- Board and who's turn it is should be the same
    board g1 === board g2 .&&. players g1 === players g2



-- | A fake piece used for creating othello boards with empty pieces in.
--   Only used to more easily create testing scenarios.
--   Use together with `o` and `x` in a matrix and call `parseBoard` to convert it to a `Board`.
m :: Piece
m = Piece "empty" $ Player "fake"

-- | Convenience function for getting the O-piece in the Othello game
o :: Piece
o = head $ pieces othello  

-- | Convenience function for getting the X-piece in the Othello game
x :: Piece
x = last $ pieces othello

-- | Convenience function for playing a turn by placing a piece at the given position
playTurn' :: Piece -> Pos -> Game -> Game
playTurn' pi po = playTurn $ Turn pi (Place po)

-- | Converts a matrix of pieces `o`, `x`, and the special `m`, into a board with the specific layout provided.
parseBoard :: [[Piece]] -> Board
parseBoard pss = initRectBoard w h layout
    where
        w = length $ head pss
        h = length pss
        layout = parseRows pss 1
        parseRows :: [[Piece]] -> Int -> [((Int, Int), Piece)]
        parseRows []     _ = []
        parseRows (r:rs) y = parseRow r 1 y ++ parseRows rs (y + 1)
        parseRow :: [Piece] -> Int -> Int -> [((Int, Int), Piece)]
        parseRow []     _ _ = []
                            -- Check if the piece is `m` (empty), if yes, skip it
        parseRow (p:ps) x y | p == m    = parseRow ps (x + 1) y
                            | otherwise = ((x, y), p) : parseRow ps (x + 1) y

