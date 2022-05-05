module ChessTests where

import Test.QuickCheck
import DSL.Types
import DSL
import DSL.Lib
import ExampleGames
import Data.Maybe (isNothing)




prop_chess_queen_takeEnemyPiece :: Property
prop_chess_queen_takeEnemyPiece = do
    let g1 = chess { board = parseBoard [
            [_q, _k, mm, _h, _r],
            [mm, mm, _b, mm, mm],
            [mm, mm, wq, mm, mm],
            [wp, wp, wp, wp, wp],
            [mm, wk, wb, wh, wr]
        ]
    }
    let g2 = playTurn' wq (Pos 2 2) (Pos 0 3) g1
    let g3 = playTurn' wq (Pos 2 2) (Pos 0 0) g1
    let g4 = playTurn' wq (Pos 2 2) (Pos 4 2) g1
    
    let e3 = parseBoard [
                [wq, _k, mm, _h, _r],
                [mm, mm, _b, mm, mm],
                [mm, mm, mm, mm, mm],
                [wp, wp, wp, wp, wp],
                [mm, wk, wb, wh, wr]
            ]
    let e4 = parseBoard [
                [_q, _k, mm, _h, _r],
                [mm, mm, _b, mm, mm],
                [mm, mm, mm, mm, wq],
                [wp, wp, wp, wp, wp],
                [mm, wk, wb, wh, wr]
            ]
    board g1 === board g2 .&&. e3 === board g3 .&&. e4 === board g4

prop_chess_queen_canNotJumpOverPieces :: Property
prop_chess_queen_canNotJumpOverPieces = do
    let g1 = chess { board = parseBoard [
            [_q, _k, mm, _h, _r],
            [mm, mm, _b, mm, mm],
            [mm, mm, wq, mm, mm],
            [wp, wp, wp, wp, wp],
            [mm, wk, wb, wh, wr]
        ]
    }
    let g2 = playTurn' wq (Pos 2 2) (Pos 0 4) g1
    let g3 = playTurn' wq (Pos 2 2) (Pos 2 0) g1
    board g1 === board g2 .&&. board g1 === board g3

prop_chess_pawn_takeEnemyDiagonally :: Property
prop_chess_pawn_takeEnemyDiagonally = do
    let g1 = chess { board = parseBoard [
            [mm, mm, mm, mm],
            [mm, mm, wp, mm],
            [wp, mm, mm, mm],
            [mm, mm, mm, _k],
            [_b, mm, mm, mm],
            [mm, mm, _b, mm],
            [wp, wp, wp, wp],
            [wk, wb, wh, wr]
        ]
    }
    let e2 = parseBoard [
                [mm, mm, mm, mm],
                [mm, mm, wp, mm],
                [wp, mm, mm, mm],
                [mm, mm, mm, _k],
                [_b, mm, mm, mm],
                [mm, mm, wp, mm],
                [wp, wp, wp, mm],
                [wk, wb, wh, wr]
            ]
    let e3 = parseBoard [
                [mm, mm, mm, mm],
                [mm, mm, wp, mm],
                [wp, mm, mm, mm],
                [mm, mm, mm, _k],
                [_b, mm, mm, mm],
                [mm, mm, wp, mm],
                [wp, mm, wp, wp],
                [wk, wb, wh, wr]
            ]
    let g2 = playTurn' wp (Pos 3 6) (Pos 2 5) g1
        g3 = playTurn' wp (Pos 1 6) (Pos 2 5) g1
        g4 = playTurn' wp (Pos 0 6) (Pos 0 4) g1
        g5 = playTurn' wp (Pos 2 6) (Pos 2 5) g1
    e2 === board g2  .&&. e3 === board g3 .&&. board g1 === board g4 .&&. board g1 === board g5

prop_chess_pawn_moveTwiceFirstTime :: Property
prop_chess_pawn_moveTwiceFirstTime = do
    let g1 = chess { board = parseBoard [
            [mm, mm, mm, mm],
            [mm, mm, wp, mm],
            [wp, mm, mm, mm],
            [mm, mm, mm, _k],
            [_b, mm, mm, mm],
            [mm, mm, _b, mm],
            [wp, wp, wp, wp],
            [wk, wb, wh, wr]
        ]
    }
    let e2 = parseBoard [
                [mm, mm, mm, mm],
                [mm, mm, wp, mm],
                [wp, mm, mm, mm],
                [mm, mm, mm, _k],
                [_b, wp, mm, mm],
                [mm, mm, _b, mm],
                [wp, mm, wp, wp],
                [wk, wb, wh, wr]
            ]
    let e3 = parseBoard [
                [mm, mm, mm, mm],
                [mm, mm, wp, mm],
                [wp, mm, mm, mm],
                [mm, mm, mm, _k],
                [_b, mm, mm, mm],
                [mm, wp, _b, mm],
                [wp, mm, wp, wp],
                [wk, wb, wh, wr]
            ]
    let g2 = playTurn' wp (Pos 1 6) (Pos 1 4) g1
        g3 = playTurn' wp (Pos 1 6) (Pos 1 5) g1
    e2 === board g2 .&&. e3 === board g3

prop_chess_pawn_invalidJumpingOverPiece :: Property
prop_chess_pawn_invalidJumpingOverPiece = do
    let g1 = chess { board = parseBoard [
            [mm, mm, mm, mm],
            [mm, mm, wp, mm],
            [wp, mm, mm, mm],
            [mm, mm, mm, _k],
            [_b, mm, mm, mm],
            [mm, mm, _b, mm],
            [wp, wp, wp, wp],
            [wk, wb, wh, wr]
        ]
    }
    let g2 = playTurn' wp (Pos 2 6) (Pos 2 4) g1
    board g1 === board g2
    
prop_chess_pawn_canOnlyMoveForwardOnce :: Property
prop_chess_pawn_canOnlyMoveForwardOnce = do
    let g1 = chess { board = parseBoard [
            [mm, mm, mm, mm],
            [mm, mm, wp, mm],
            [wp, mm, mm, mm],
            [mm, mm, mm, _k],
            [_b, mm, mm, mm],
            [mm, mm, _b, mm],
            [wp, wp, wp, wp],
            [wk, wb, wh, wr]
        ]
    }
    let g2 = playTurn' wp (Pos 0 2) (Pos 0 0) g1
        g3 = playTurn' wp (Pos 2 1) (Pos 3 0) g1
        g4 = playTurn' wp (Pos 2 1) (Pos 1 0) g1
        g5 = playTurn' wp (Pos 2 1) (Pos 2 2) g1
    board g1 === board g2 .&&. board g1 === board g3 .&&. board g1 === board g4 .&&. board g1 === board g5

prop_chess_pawn_promotion :: Property
prop_chess_pawn_promotion = do
    let g1 = chess { board = parseBoard [
            [mm, mm, mm, mm],
            [mm, mm, wp, mm],
            [wp, mm, mm, mm],
            [mm, mm, mm, _k],
            [_b, mm, mm, mm],
            [mm, mm, _b, mm],
            [wp, wp, _p, wp],
            [wk, wb, mm, wr]
        ]
    }
    let e2 = parseBoard [
                [mm, mm, wq, mm],
                [mm, mm, mm, mm],
                [wp, mm, mm, mm],
                [mm, mm, mm, _k],
                [_b, mm, mm, mm],
                [mm, mm, _b, mm],
                [wp, wp, _p, wp],
                [wk, wb, mm, wr]
            ]
    let e3 = parseBoard [
                [mm, mm, wq, mm],
                [mm, mm, mm, mm],
                [wp, mm, mm, mm],
                [mm, mm, mm, _k],
                [_b, mm, mm, mm],
                [mm, mm, _b, mm],
                [wp, wp, mm, wp],
                [wk, wb, _q, wr]
            ]
    let g2 = playTurn' wp (Pos 2 1) (Pos 2 0) g1
        g3 = playTurn' _p (Pos 2 6) (Pos 2 7) g2

    board g2 === e2 .&&. board g3 === e3

prop_chess_knight_canOnlyMoveInLShape :: Property
prop_chess_knight_canOnlyMoveInLShape = do
    let g1 = chess { board = parseBoard [
            [mm, mm, _k, mm, mm],
            [mm, mm, mm, mm, mm],
            [mm, mm, wh, mm, mm],
            [mm, mm, mm, mm, mm],
            [mm, mm, wk, mm, mm]
        ]
    }
    
    let g2 = playTurn' wh (Pos 2 2) (Pos 0 1) g1
    let g3 = playTurn' wh (Pos 2 2) (Pos 3 0) g1
    let g4 = playTurn' wh (Pos 2 2) (Pos 3 2) g1
    let g5 = playTurn' wh (Pos 2 2) (Pos 4 4) g1

    let e2 = parseBoard [
            [mm, mm, _k, mm, mm],
            [wh, mm, mm, mm, mm],
            [mm, mm, mm, mm, mm],
            [mm, mm, mm, mm, mm],
            [mm, mm, wk, mm, mm]
         ]
    let e3 = parseBoard [
            [mm, mm, _k, wh, mm],
            [mm, mm, mm, mm, mm],
            [mm, mm, mm, mm, mm],
            [mm, mm, mm, mm, mm],
            [mm, mm, wk, mm, mm]
         ]

    e2 === board g2 .&&. e3 === board g3 .&&. board g1 === board g4 .&&. board g1 === board g5

prop_chess_knight_canJumpOverPieces :: Property
prop_chess_knight_canJumpOverPieces = do
    let g1 = chess { board = parseBoard [
            [mm, mm, mm, mm, mm],
            [mm, wp, wp, wp, mm],
            [mm, wp, wh, _k, mm],
            [mm, wp, wk, wp, mm],
            [mm, mm, mm, mm, mm]
        ]
    }
    
    let g2 = playTurn' wh (Pos 2 2) (Pos 3 4) g1

    let e2 = parseBoard [
            [mm, mm, mm, mm, mm],
            [mm, wp, wp, wp, mm],
            [mm, wp, mm, _k, mm],
            [mm, wp, wk, wp, mm],
            [mm, mm, mm, wh, mm]
         ]

    e2 === board g2


prop_chess_knight_canNotJumpToAllyTile :: Property
prop_chess_knight_canNotJumpToAllyTile = do
    let g1 = chess { board = parseBoard [
            [_q, _k, mm, _h, _r],
            [mm, mm, _b, mm, mm],
            [mm, mm, wh, mm, mm],
            [wp, wp, wp, wp, wp],
            [mm, wk, wb, wh, wr]
        ]
    }
    let g2 = playTurn' wh (Pos 2 2) (Pos 0 3) g1
        g3 = playTurn' wh (Pos 2 2) (Pos 1 4) g1
        g4 = playTurn' wh (Pos 2 2) (Pos 3 4) g1
        g5 = playTurn' wh (Pos 2 2) (Pos 4 3) g1
    board g1 === board g2 .&&. board g1 === board g3 .&&. board g1 === board g4 .&&. board g1 === board g5

prop_chess_king_moveOneStepInAnyDir :: Property
prop_chess_king_moveOneStepInAnyDir = do
    let g1 = chess { board = parseBoard [
            [mm, mm, _k, mm],
            [mm, mm, mm, _p],
            [wp, mm, wk, mm],
            [mm, mm, mm, wp]
        ] }
    let g2 = playTurn' wk (Pos 2 2) (Pos 3 3) g1
        g3 = playTurn' wk (Pos 2 2) (Pos 3 1) g1
        g4 = playTurn' wk (Pos 2 2) (Pos 1 2) g1
        g5 = playTurn' wk (Pos 2 2) (Pos 0 0) g1
        
    let e3 = parseBoard [
            [mm, mm, _k, mm],
            [mm, mm, mm, wk],
            [wp, mm, mm, mm],
            [mm, mm, mm, wp]
            ]
    let e4 = parseBoard [
            [mm, mm, _k, mm],
            [mm, mm, mm, _p],
            [wp, wk, mm, mm],
            [mm, mm, mm, wp]
            ]
    board g1 === board g2 .&&. board g1 === board g5 .&&. e3 === board g3 .&&. e4 === board g4

prop_chess_king_otherKingDeadLeadsToWin :: Property
prop_chess_king_otherKingDeadLeadsToWin = do
    let g1 = chess { board = parseBoard [
            [_r, _h, _b, _q, _k, _b, _h, _r],
            [_p, _p, _p, _p, wq, _p, _p, _p],
            [mm, mm, mm, mm, mm, mm, mm, mm],
            [mm, mm, mm, mm, mm, mm, mm, mm],
            [mm, mm, mm, mm, mm, mm, mm, mm],
            [mm, mm, mm, mm, mm, mm, mm, mm],
            [wp, wp, wp, wp, wp, wp, wp, wp],
            [wr, wh, wb, wq, wk, wb, wh, wr]
        ]
    }
    let g2 = playTurn' wq (Pos 4 1) (Pos 4 0) g1
    gameEnded g2 === True .&&. winner g2 === Just (Player "White")

-- | A fake piece used for creating chess boards with empty pieces in.
--   Only used to more easily create testing scenarios.
--   Use together with `o` and `x` in a matrix and call `parseBoard` to convert it to a `Board`.
mm :: Piece
mm = Piece "empty" $ Player "fake"

wp, wr, wh, wb, wq, wk, _p, _r, _h, _b, _q, _k :: Piece

wp = Piece "p" (Player "White")
wr = Piece "r" (Player "White")
wh = Piece "h" (Player "White")
wb = Piece "b" (Player "White")
wq = Piece "q" (Player "White")
wk = Piece "k" (Player "White")
_p = Piece "P" (Player "Black")
_r = Piece "R" (Player "Black")
_h = Piece "H" (Player "Black")
_b = Piece "B" (Player "Black")
_q = Piece "Q" (Player "Black")
_k = Piece "K" (Player "Black")

-- | Convenience function for playing a turn by placing a piece at the given position
playTurn' :: Piece -> Pos -> Pos -> Game -> Game
playTurn' pi pos1 pos2 = playTurn $ Turn pi (Move pos1 pos2)

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
        parseRow (p:ps) x y | p == mm   = parseRow ps (x + 1) y
                            | otherwise = ((x, y), p) : parseRow ps (x + 1) y