import Test.QuickCheck

test_prop :: [Int] -> Bool
test_prop xs = reverse (reverse xs) == xs

main :: IO ()
main = quickCheck test_prop
