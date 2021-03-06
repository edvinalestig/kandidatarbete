import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import DSL.Lib (rectBoard)
import DSL.Types
import DSL.Utility
import DSL.Internal
import DSL
import ExampleGames
import TicTacToeTests
import OthelloTests
import ChessTests


main :: IO ()
main = hspec $ do
    describe "Lib/Internal" $ 
        describe "General function testing within Lib.hs and Internal.hs" $ do
        prop "verify that rectBoard creates a rectangular board of correct width and height" $
            \w h -> prop_rectBoard_isCorrectSize w h
        prop "verify that the amount of diagonals is equal to what it should be" $
            \w h k -> prop_getDiagonals_correctAmount w h k
        prop "verify that the amount of columns is equal to the amount of rows of a certain size on a square board" $
            \s k -> prop_getRows_equalsOnSquareBoards_getColumns s k
    describe "Tic-tac-toe" $ 
        describe "Verifies various functionality for the game" $ do
        prop "verify correct placement of pieces" $
            again prop_tictactoe_correctPlacement
        prop "verify that when the board is full and no one has three in a row, no one is a winner"
            prop_tictactoe_noWinnerWhenDraw
        prop "verify that the game has ended when there is a draw"
            prop_tictactoe_gameEndsWhenDraw
        prop "verify that player 1 can win"
            prop_tictactoe_player1canWin
        prop "verify that player 2 can win"
            prop_tictactoe_player2canWin
        prop "verify that the game has not ended after 4 random moves" $
            forAll arbitraryTicTacToePos prop_tictactoe_gameNotEndedAfter4Moves
    describe "Othello" $ 
        describe "Verifies various functionality for the game" $ do
        prop "verify that it flips pieces in straight lines with multiple for each line" 
            prop_othello_correctChangesStraightLines
        prop "verify that it flips pieces in diagonal lines with multiple for each line" 
            prop_othello_correctChangesDiagonalLines
        prop "verify that the game is ended when the board is full" 
            prop_othello_gameEndedWhenFullBoard 
        prop "verify that the game is ended when no player has a legal move" 
            prop_othello_gameEndedWhenNoPlayerHasLegalMove  
        prop "verify that the one with the most pieces wins" 
            prop_othello_playerWithTheMostPiecesWins 
        prop "verify that the game results in a draw when players has an equal number of pieces" 
            prop_othello_drawOnEqualNumberOfPieces  
        prop "verify that an invalid move does not change the game state" 
            prop_othello_invalidMoveChangesNothing
    describe "Chess" $ 
        describe "Verifies various functionality for the game" $ do
        context "Piece: Pawn" $ do
            prop "verify that it can only take pieces diagonally" 
                prop_chess_pawn_takeEnemyDiagonally
            prop "verify that it can move twice the first time it moves" 
                prop_chess_pawn_moveTwiceFirstTime
            prop "verify that it can not cross another piece"
                prop_chess_pawn_invalidJumpingOverPiece 
            prop "verify that it can be promoted to a queen"
                prop_chess_pawn_promotion
            prop "verify that it can only move forward once, except the first time it moves"
                prop_chess_pawn_canOnlyMoveForwardOnce
        context "Piece: Queen" $ do
            prop "verify that it can take enemy pieces, diagonally and straight"
                prop_chess_queen_takeEnemyPiece
            prop "verify that it can not move over other pieces"
                prop_chess_queen_canNotJumpOverPieces
        context "Piece: Knight" $ do
            prop "verify that it can only move in an L shape"
                prop_chess_knight_canOnlyMoveInLShape
            prop "verify that it can move over other pieces"
                prop_chess_knight_canJumpOverPieces
            prop "verify that it can move to either an empty piece or enemy tile"
                prop_chess_knight_canNotJumpToAllyTile
        context "Piece King" $ do
            prop "verify that it can only move one step in any direction"
                prop_chess_king_moveOneStepInAnyDir
            prop "verify that if it dies, the player loses"
                prop_chess_king_otherKingDeadLeadsToWin


-- | Verify that the board is a rectangle and of correct width and height
prop_rectBoard_isCorrectSize :: Int -> Int -> Bool
prop_rectBoard_isCorrectSize w' h' = all ((w ==) . length) board && length board == h
  where 
    w = abs w'
    h = abs h'
    board = rectBoard w h

-- | Verify that the amount of diagonals is correct given the formula 2(w-k+1)(h-k+1)
--   where w and h are larger than zero and k is smaller than or equal to the smallest of w and h
prop_getDiagonals_correctAmount :: Int -> Int -> Int -> Property
prop_getDiagonals_correctAmount w' h' k' = k <= min w h ==> 2 * (w - k + 1) * (h - k + 1) == length diagonals
  where
    w = abs w' + 1
    h = abs h' + 1
    k = abs k'
    board = rectBoard w h
    diagonals = getDiagonals board k

-- | Verify that, on a square board, there are equal amounts of rows and columns of size k
prop_getRows_equalsOnSquareBoards_getColumns :: Int -> Int -> Property
prop_getRows_equalsOnSquareBoards_getColumns s' k' = k <= s ==> length rows == length cols
  where
    s = abs s'
    k = abs k'
    board = rectBoard s s
    rows = getRows board k
    cols = getColumns board k



