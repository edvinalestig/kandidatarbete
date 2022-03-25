{-|
Module      : Types
Description : A Haskell module containing all the types used in the DSL
-}

module DSL.Types (
    Game (..),
    Rule (..),
    Pos (..),
    EndCondition (..),
    Player (..),
    Piece (..),
    Tile (..),
    Board,
    Die (..)
) where

import Test.QuickCheck

-- | The main game object where all game info is contained. 
data Game = Game
    {
        board         :: Board,
        pieces        :: [Piece],
        dice          :: [Die],
        players       :: [Player],
        rules         :: [Rule],
        endConditions :: [EndCondition]
    }


-- | A rule object with a function which has to be fulfilled in
--   order to be able to place a piece on the board.
data Rule = PlaceRule (Pos -> Board -> Bool)
          | UpdateRule (Piece -> Pos -> Board -> Board)

-- data Action = Place Pos
--             | Move Pos Pos Piece
-- tileIsEmpty >>= tileBelowIsNotEmpty >>= placeTile
-- newType Rule = PlaceRule ([Restriction], [Move])
-- newType Rule = PlaceRule (Board -> [Restriction] -> [Action] -> Maybe Board)

-- newType Restriction = Restriction -> Board -> Bool
-- newType Move = Action -> Board -> Board

-- | A simple vector object containing a x and a y value
data Pos = Pos Int Int 
    deriving (Eq, Show)

-- | A record containing conditions to be met for the game to end.
--   It can have multiple functions for draws and wins. 
type EndCondition = (Game -> Maybe Player, Board -> Bool)

-- | A player object with a name
newtype Player = Player String
    deriving (Eq)

-- | A piece object with a name/identifier and the player owning it
data Piece = Piece String Player
    deriving (Eq)
    
-- | A tile object which can either be empty or it can contain a piece.
--   `pos` might be removed.
data Tile = PieceTile Piece Pos | Empty Pos -- Can Pos be removed?
    deriving (Eq)

-- | The board contains a list of rows of tiles. 
type Board = [[Tile]]

-- | A die with a given number of sides.
newtype Die = Die Int

instance Show Player where
    show (Player p) = p

instance Show Tile where
    show (PieceTile p _) = show p
    show (Empty _) = " "

instance Show Piece where
    show (Piece s _) = s


-- * Testing

instance Arbitrary Pos where
    arbitrary = Pos <$> (arbitrary :: Gen Int) <*> (arbitrary :: Gen Int)
instance Arbitrary Player where
    arbitrary = Player <$> printableStringGen

-- | A Generator for printable strings
printableStringGen :: Gen String 
printableStringGen = getASCIIString <$> (arbitrary :: Gen ASCIIString)
-- printableString = arbitraryASCIIChar   use if Char

instance Arbitrary Piece where
    arbitrary = Piece <$> printableStringGen <*> (arbitrary :: Gen Player)

instance Arbitrary Tile where
    arbitrary = frequency [
        (7, PieceTile <$> arbitrary <*> arbitrary),
        (3, Empty <$> arbitrary)
        ]

-- instance Arbitrary Board where
--     arbitrary = vectorOf 10 $ vectorOf 10 (arbitrary :: Gen Tile)

{- rectBoard :: Int -> Int -> Board
rectBoard w h = [[Empty (Pos x y) | x <- [0..w-1]] | y <- [0..h-1]] -}



{- nonNegative :: Gen Integer
nonNegative = do n <- arbitrary
                 return (abs n) -}


