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
    Player (..),
    Piece (..),
    Tile (..),
    Board
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
data Action = Place Pos | Move Pos Pos deriving (Show)

-- | Update
data Update t = Update (Turn -> t -> t)         -- ^ Represent an update to a type `t`
              | (Update t) `COMBINE` (Update t) -- ^ Combines two updates


-- | Runs two 'Rule' sequentially, 
-- if any of them fail the resulting rule is ignored
(>=>) :: Rule -> Rule -> Rule
(>=>) = SEQ

-- | Runs two 'Rule' sequentially, 
-- if a 'Rule' fail to apply the last successful one is used instead.
(>>>) :: Rule -> Rule -> Rule
(>>>) = THEN

-- | Rule
data Rule = Rule      (Update Game)            -- ^ Updates the game 
          | TurnRule  (Update Turn) Rule       -- ^ Runs a 'Rule' at the at the given 'Turn'
          | If     (Condition Turn) Rule       -- ^ Runs the rule only if a condition applies
          | IfElse (Condition Turn) Rule Rule  -- ^ A condition with two outcomes
          | Rule `SEQ`   Rule                  -- ^ Runs two 'Rule' sequentially, if any of 
                                               --   them fail the resulting rule is ignored
          | Rule `THEN`  Rule                  -- ^ Runs two 'Rule' sequentially, if a 'Rule'
                                               --   fails only the rules that follow are ignored
          | IterateUntil Rule (Condition Turn) -- ^ Run a 'Rule' until the end condition is met.
                                               --   If the rule fails before the end condition
                                               --   is met the result is ignored

-- | Condition
data Condition a = Condition (a -> Game -> Bool)     -- ^ Represent a simple condition
                 | (Condition a) `AND` (Condition a) -- ^ If both conditions are 'True'
                 | (Condition a) `OR`  (Condition a) -- ^ If either conditions are 'True'
                 | NOT (Condition a)                 -- ^ Negates a 'Condition'


-- | A simple two-dimensional vector object containing a 'x' and a 'y' value
data Pos = Pos Int Int
    deriving (Eq, Show)

-- | A player object with a name
newtype Player = Player String
    deriving (Eq)

-- | A piece object with a name/identifier and the player owning it
data Piece = Piece String Player
    deriving (Eq)

-- | A tile object which can either be empty or it can contain a piece.
--   `pos` might be removed.
data Tile = PieceTile Piece Pos -- ^ Used if the tile has a piece
          | Empty Pos           -- ^ A tile with no piece on it
    deriving (Eq)

-- | The board contains a list of rows of tiles. 
type Board = [[Tile]]

instance Show Player where
    show (Player p) = p

instance Show Tile where
    show (PieceTile p _) = show p
    show (Empty _) = " "

instance Show Piece where
    show (Piece s _) = s

instance Num Pos where
    (Pos x y) + (Pos x' y') = Pos (x+x') (y+y')
    (Pos x y) - (Pos x' y') = Pos (x-x') (y-y')
    (Pos x y) * (Pos x' y') = Pos (x*x') (y*y')


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

