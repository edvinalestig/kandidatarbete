module Othello where

import Data.Traversable
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

--GameLoop / IO

--main :: IO ()
--main = do {
--let l = placeFirstPieces initilizeBoard 4

--}

--Board handling
  
updateBoard :: Int -> Int -> Map Int Int -> Map Int Int
updateBoard player tile board = Map.insert tile player board

flipGamePieces :: Map Int Int -> [Int] -> Int -> Map Int Int
flipGamePieces board ([]) player = board
flipGamePieces board ([x]) player = Map.insert x player board
flipGamePieces board (x:xs) player = (Map.union (Map.singleton x player) (flipGamePieces board xs player))

flipGamePiecesAlt :: Map Int Int -> Map Int Int -> Map Int Int
flipGamePiecesAlt update board = Map.union update board

placeFirstPieces :: Int -> Map Int Int -> Map Int Int
placeFirstPieces size board = (Map.union
                              (Map.fromList [((middleboard - middlerow), 2),
                              (((middleboard + 1) - middlerow), 1),
                              ((middleboard + (middlerow)), 1),
                              ((middleboard + (middlerow + 1)), 2)]) board)

                              where
                              middleboard = ((Map.size board) `div` 2)
                              middlerow = (size `div` 2)

initilizeBoard :: Int -> Map Int Int
initilizeBoard size = Map.fromList $ Data.List.zip ([1..(size * size)]) (Data.List.replicate (size * size) 0)

--Checks

-- (-) = down, (+) = up

checkIfClosedInVertical :: Map Int Int -> Int -> Int -> Int -> [Int]
checkIfClosedInVertical board position = undefined

checkIfClosedInHorizontal :: Map Int Int -> Int -> Int -> Int -> [Int]
checkIfClosedInHorizontal board position player size = undefined

checkValidMove :: Int -> Int -> Int -> Int -> Map Int Int -> Bool
checkValidMove player tile size edge board = ((Just 0 == (Map.lookup tile board)) &&
  (((((Just 0 /= left)) && (Just player /= left)) && ((edge /= 3) && (edge /= 5) && (edge /= 7))) ||
  (((Just 0 /= right) && (Just player /= right)) && ((edge /= 4) && (edge /= 6) && (edge /= 8))) ||
  (((Just 0 /= top) && (Just player /= top)) && ((edge /= 1) && (edge /= 5) && (edge /= 6))) ||
  (((Just 0 /= bottom) && (Just player /= bottom) && ((edge /= 2) && (edge /= 7) && (edge /= 8))))))

  where
  left = (Map.lookup (tile - 1) board)
  right = (Map.lookup (tile + 1) board)
  top = (Map.lookup (tile - size) board)
  bottom = (Map.lookup (tile + size) board)

-- 1 = top, 2 = bottom, 3 = left, 4 = right, 5 = top left corner,
-- 6 = top right corner, 7 = bottom left corner, 8 = bottom right corner, 0 = not edge
  
checkEdge :: Map Int Int -> Int -> Int -> Int
checkEdge board position size
                  | (position <= size) && (1 == (position `mod` size)) = 5
                  | (position <= size) && (0 == (position `mod` size)) = 6
                  | ((position) > ((Map.size board) - (size))) && (1 == (position `mod` size)) = 7
                  | ((position) > ((Map.size board) - (size))) && (0 == (position `mod` size)) = 8
                  | (position < size) = 1
                  | ((position) > ((Map.size board) - (size))) = 2
                  | (1 == (position `mod` size)) = 3
                  | (0 == (position `mod` size)) = 4
                  | otherwise = 0

checkGameState :: Map Int Int -> Bool
checkGameState board = (Map.size (Map.filter (==0) board)) == 0

--Utility

calculateScore :: Int -> Map Int Int -> Int
calculateScore player board = Map.size $ Map.filter (==player) board

printBoard :: Int -> [Int] -> IO ()
printBoard size [] = print ""
printBoard size board = do {(print (Data.List.take size board)) ; (printBoard size (Data.List.drop size board))}

getRow :: Map Int Int -> Int -> Int -> [(Int,Int)]
getRow board size position = Data.List.take size (Data.List.drop start (Map.toList board))

  where
  start = (position - (position - size))

getColumn :: Map Int Int -> Int -> Int -> [(Int,Int)]
getColumn board size position = undefined  