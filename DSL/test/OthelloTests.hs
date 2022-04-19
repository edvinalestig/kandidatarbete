module OthelloTests (
    prop_othello_correctChangesStraightLines,
    prop_othello_correctChangesDiagonalLines
) where

import Test.QuickCheck
import DSL.Types
import DSL
import DSL.Lib
import TestGames

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
    let g2 = playTurn g1 o (Pos 3 3)
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
    let g2 = playTurn g1 o (Pos 3 3)
    let expected = parseBoard [
            [o, m, m, m, m, m, o],
            [m, o, m, x, m, o, m],
            [m, m, o, x, o, m, m],
            [m, x, x, o, x, x, m],
            [m, m, o, x, o, m, m],
            [m, o, m, x, m, o, m],
            [o, m, m, m, m, m, o]]
    board g2 === expected

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

