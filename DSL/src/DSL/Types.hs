{-|
Module      : Types
Description : A Haskell module containing all the types used in the DSL
-}

module DSL.Types (
    Game (..),
    (>=>),
    (>>>),
    Rule (..),
    Update (..),
    Condition (..),
    Turn (..),
    Action (..),
    Pos (..),
    EndCondition (..),
    Player (..),
    Piece (..),
    Tile (..),
    Board,
    Die (..)
) where

import Test.QuickCheck

-- This is to support a sequence of these operators after each other
-- without needing paranthesis. 
infixr 9 `SEQ`
infixr 9 `THEN`


-- | The main game object where all game info is contained. 
data Game = Game
    {
        board         :: Board,
        pieces        :: [Piece],
        players       :: [Player],
        rules         :: [Rule],
        endConditions :: [Rule],
        winner        :: Maybe Player,
        gameEnded     :: Bool,
        dispFunction  :: Game -> IO ()
    }


-- | Represent the input a user can provide,
-- what piece they act on and what they'll do with it
data Turn = Turn
    {
        piece  :: Piece,
        action :: Action
    }

-- | Represent a move that a piece can make
data Action = Place Pos | Move Pos Pos

-- | Update
data Update t = Update (Turn -> t -> t)
              | (Update t) `COMBINE` (Update t)


-- | Sequences two rules, 
--   if one results in `Nothing` then the result will be `Nothing`
(>=>) :: Rule -> Rule -> Rule
(>=>) = SEQ

-- | Sequences two rules,
--   if one results in `Nothing` it will take the previous `Just` and continue
(>>>) :: Rule -> Rule -> Rule
(>>>) = THEN

-- | Rule
data Rule = Rule      (Update Game)
          | TurnRule  (Update Turn) Rule
          | If     (Condition Turn) Rule
          | IfElse (Condition Turn) Rule Rule
          | Rule `SEQ`   Rule
          | Rule `THEN`  Rule
          | IterateUntil Rule (Condition Turn)

-- | Condition
data Condition a = Condition (a -> Game -> Bool)
                 | (Condition a) `AND` (Condition a)
                 | (Condition a) `OR`  (Condition a)
                 | NOT (Condition a)


-- | A simple vector object containing a x and a y value
data Pos = Pos Int Int
    deriving (Eq, Show)

-- | A record containing conditions to be met for the game to end.
--   It can have multiple functions for draws and wins. 
type EndCondition = (Game -> Maybe Player, Rule) -- Game -> Bool

-- | A player object with a name
newtype Player = Player String
    deriving (Eq)

-- | A piece object with a name/identifier and the player owning it
data Piece = Piece String Player
    deriving (Eq)

-- | A tile object which can either be empty or it can contain a piece.
--   `pos` might be removed.
data Tile = PieceTile Piece Pos | Empty Pos
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

instance Num Pos where
    (Pos x y) + (Pos x' y') = Pos (x+x') (y+y')


-- * Testing

-- | A Generator for printable strings
printableStringGen :: Gen String
printableStringGen = getASCIIString <$> (arbitrary :: Gen ASCIIString)

instance Arbitrary Pos where
    arbitrary = Pos <$> (arbitrary :: Gen Int) <*> (arbitrary :: Gen Int)

instance Arbitrary Player where
    arbitrary = Player <$> printableStringGen

instance Arbitrary Piece where
    arbitrary = Piece <$> printableStringGen <*> (arbitrary :: Gen Player)

instance Arbitrary Tile where
    arbitrary = frequency [
            (7, PieceTile <$> arbitrary <*> arbitrary),
            (3, Empty <$> arbitrary)
        ]

