import Text.Read ( readMaybe )

data Tile = Empty | X | O | Tie deriving (Show, Eq)
type Board = [Tile]

main :: IO ()
-- main = do
--     putStrLn "X starts:"
--     winner <- tictactoe (replicate 9 Empty) X
--     putStr "The winner is: "
--     print winner
main = putStrLn "X starts:"
    >> tictactoe (replicate 9 Empty) X 
    >>= \winner -> putStr "The winner is: "
    >> print winner

tictactoe :: Board -> Tile -> IO Tile
tictactoe board turn = do
    prettyPrint board
    putStr "Current turn: "
    print turn
    putStr "Please enter the desired position: "
    l <- getLine

    let n = readMaybe l :: Maybe Int
    case n of
        Nothing -> tictactoe board turn
        Just n -> 
            if n < 1 || n > 9 || board !! (n-1) /= Empty then
                tictactoe board turn
            else do
                let b = take (n-1) board ++ [turn] ++ drop n board
                if Empty `notElem` b then
                    return Tie
                else 
                    if checkWin b then
                        prettyPrint b >>
                        return turn
                    else
                        tictactoe b (notTile turn)


-- YUCK
checkWin :: Board -> Bool
checkWin b = (b !! 4 /= Empty && 
    ((head b == b !! 4 && head b == b !! 8) 
    || (b !! 2 == b !! 4 && b !! 4 == b !! 6)))
    || head b /= Empty && (head b == b !! 1 && head b == b !! 2)
    || b !! 3 /= Empty && (b !! 3 == b !! 4 && b !! 3 == b !! 5)
    || b !! 6 /= Empty && (b !! 6 == b !! 7 && b !! 6 == b !! 8)
    || head b /= Empty && (head b == b !! 3 && head b == b !! 6)
    || b !! 1 /= Empty && (b !! 1 == b !! 4 && b !! 1 == b !! 7)
    || b !! 2 /= Empty && (b !! 2 == b !! 5 && b !! 2 == b !! 8)


notTile :: Tile -> Tile
notTile X = O
notTile O = X
notTile t = t


prettyPrint :: Board -> IO ()
prettyPrint b = do
    putStrLn "-------------"
    prettyPrint' $ map f b
    where
        f :: Tile -> String
        f t = case t of
            Empty -> " "
            s     -> show s

        prettyPrint' :: [String] -> IO ()
        prettyPrint' [] = return ()
        prettyPrint' b  = do
            putStrLn $ "| " ++ head b ++ " | " ++ b !! 1 ++ " | " ++ b !! 2 ++ " |"
            putStrLn "-------------"
            prettyPrint' $ drop 3 b
                