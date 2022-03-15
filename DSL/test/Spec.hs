import Test.QuickCheck
import Test.Hspec
import DSL.Lib
import DSL.Types

main :: IO ()
main = hspec $ do
    describe "Common" $ do
        it "reversing a reversed list returns the input list" $ property $ \xs -> test_prop xs
    describe "Lib" $ do
        it "rectBoard creates a rectangular board of correct width and height"
            $ property $ \w h -> prop_rectBoard_isCorrectSize w h
        it "checks that boardIsFull actualy determines that there are no empty tiles"
            $ property $ \b -> prop_boardIsFull_noEmpty b
        it "verify that the amount of diagonals is equal to what it should be"
            $ property $ \w h k -> prop_getDiagonals_correctAmount w h k
        it "check that the amount of columns is equal to the amount of rows of a certain size on a square board"
            $ property $ \s k -> prop_getRows_equalsOnSquareBoards_getColumns s k


test_prop :: [Int] -> Bool
test_prop xs = reverse (reverse xs) == xs

-- | Verify that the board is a rectangle and of correct width and height
prop_rectBoard_isCorrectSize :: Int -> Int -> Bool
prop_rectBoard_isCorrectSize w' h' = all ((w ==) . length) board && length board == h
  where 
    w = abs w'
    h = abs h'
    board = rectBoard w h

-- Verify that when the board is full it is equivalent to no empty tiles on the board
prop_boardIsFull_noEmpty :: Board -> Bool
prop_boardIsFull_noEmpty b = boardIsFull b == noEmpty b
  where
    noEmpty :: Board -> Bool
    noEmpty = not . any (any empty')

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



empty' :: Tile -> Bool
empty' (Empty _) = True
empty'  _        = False



