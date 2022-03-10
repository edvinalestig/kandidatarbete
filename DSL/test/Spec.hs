import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Common" $ do
        it "reversing a reversed list returns the input list" $ property $ \xs -> test_prop xs

    -- describe

test_prop :: [Int] -> Bool
test_prop xs = reverse (reverse xs) == xs

{- 
Function naming convension: prop_[thing you test]

DSL:
cyclePlayers
placePiece
filterPieces

Lib:
rectBoard
tileIsEmpty
tileBelowIsNotEmpty
boardIsFull
getDiagonals
getRows
getColumns
allEQ
samePiece

Arbitrary:
Pos
Board
Tile
Piece
Player

Utility:
getTile
filterNothing
replaceAtIndex
-}


