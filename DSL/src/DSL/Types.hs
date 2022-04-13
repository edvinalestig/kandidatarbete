{-|
Module      : Types
Description : A Haskell module containing all the types used in the DSL
-}

module DSL.Types (
    Game (..),
    (>=>),
    (>>>),
    NewRule (..),
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
-- import Data.Map (Map)

-- | The main game object where all game info is contained. 
data Game = Game
    {
        board         :: Board,
        pieces        :: [Piece],
        dice          :: [Die],
        path          :: Pos -> Int -> Pos,
        players       :: [Player],
        rules         :: [NewRule],
        endConditions :: [NewRule],
        winner        :: Maybe Player,
        gameEnded     :: Bool,
        dispFunction  :: Game -> IO ()
    }

-- data NewGame = NewGame
--     {
--         board         :: Board,
--         pieces        :: [Piece],
--         dice          :: [Die],
--         players       :: [Player],
--         rules         :: [(String, [NewRule])]
--         pieceRules    :: Map Piece String,
--         endConditions :: [EndCondition],
--         gameEnded     :: GameState,
--         dispFunction  :: Game -> IO ()
--     }


-- | Represent the input a user can provide,
-- what piece they act on and what they'll do with it
data Turn = Turn
    {
        piece  :: Piece,
        action :: Action
    }

-- | Represent a move that a piece can make
data Action = Place Pos
            | Move Pos Pos
            | Step Pos Int

-- | A rule object with a function which has to be fulfilled in
--   order to be able to place a piece on the board.
-- data Rule = TurnRule      (Turn -> Game -> Bool)
--           | UpdateRule    (Turn -> Game -> Board)


data Update t = Update (Turn -> t -> t)
              | (Update t) `COMBINE` (Update t)
              --  | TileUpdate (Turn -> [[Tile]] -> [[Tile]])


-- | Sequences two rules, 
--   if one results in `Nothing` then the result will be `Nothing`
(>=>) :: NewRule -> NewRule -> NewRule
(>=>) = SEQ

-- | Sequences two rules,
--   if one results in `Nothing` it will take the previous `Just` and continue
(>>>) :: NewRule -> NewRule -> NewRule
(>>>) = THEN

data NewRule = Rule (Update Game)
             | TurnRule (Update Turn) NewRule
             | If (Condition Turn) NewRule
             | IfElse (Condition Turn) NewRule NewRule
             | NewRule `SEQ` NewRule
             | NewRule `THEN` NewRule
             | IterateUntil NewRule (Condition Turn)



data Condition a = Condition (a -> Game -> Bool)
                 | (Condition a) `AND` (Condition a)
                 | (Condition a) `OR`  (Condition a)
                 | NOT (Condition a)



tileIsEmpty :: Pos -> Board -> Bool
tileIsEmpty = undefined
pattern :: [[Tile] -> Bool] -> [[Tile]] -> [Bool]
pattern = undefined
oneOrMore :: [Bool] -> Bool
oneOrMore = undefined
allDirections :: Pos -> Board -> [[Tile]]
allDirections = undefined
placePiece :: Piece -> Pos -> Board -> Board
placePiece = undefined
change :: (Piece -> Piece) -> [Pos] -> Board -> Board
change = undefined
enemyPiece :: [Tile] -> [Bool]
enemyPiece = undefined
alliedPiece :: [Tile] -> [Bool]
alliedPiece = undefined
to :: Piece -> Piece
to = undefined
lineBetween :: Turn -> Game -> [Tile]
lineBetween = undefined

-- instance Functor NewRule where
--     fmap = undefined

-- instance Applicative NewRule where
--     pure  = return
--     (<*>) = undefined

-- instance Monad NewRule where
--     return = undefined
--     (>>=)  = undefined

-- SCHACK BONDE : IF ((FORWARD 1 AND EMPTYTILE) OR ((FORWARD 1 AND (LEFT 1 OR RIGHT 1)) AND ENEMYPIECE)) >>= (MOVEPIECE)
-- bonde:   IFELSE (FORWARD 1 AND EMPTYTILE) 
    --          (MOVEPIECE) $

    --          IF (DIAGONAL 1 AND ENEMYPIECE) 
    --              (REMOVEENEMYPIECE AND MOVEPIECE) 

{-
rules = [ If (tileIsEmpty AND or . allDirections . (Pattern [oneOrMore enemyPiece, oneOrMore alliedPiece]))
             (placePiece AND change (enemyPiece to alliedPiece) (allDirections (Pattern [oneOrMore enemyPiece, oneOrMore alliedPiece])))
        ]

rules = [ If (tileIsEmpty AND (or . (Pattern [oneOrMore enemyPiece, oneOrMore alliedPiece]) . allDirections))
             (placePiece AND change (enemyPiece to alliedPiece) (allDirections (Pattern [oneOrMore enemyPiece, oneOrMore alliedPiece])))
        ]

rules = [ If (tileIsEmpty AND (secondIsAlly (group (allDirections board)))) AND (firstIsEnemy (group (allDirections board))))) (placePiece AND ) ]

-}


-- | Data type containing information if someone won/draw or if the game is in progress
data GameState = Maybe Player | InProgress


-- | A simple vector object containing a x and a y value
data Pos = Pos Int Int
    deriving (Eq, Show)

type Path = [Pos]

-- | A record containing conditions to be met for the game to end.
--   It can have multiple functions for draws and wins. 
type EndCondition = (Game -> Maybe Player, NewRule) -- Game -> Bool

-- | A player object with a name
newtype Player = Player String
    deriving (Eq)

-- | A piece object with a name/identifier and the player owning it
data Piece = Piece String Player
    deriving (Eq)

-- | A tile object which can either be empty or it can contain a piece.
--   `pos` might be removed.
data Tile = PieceTile Piece Pos | Empty Pos | MultiPieceTile [Piece] Pos -- Can Pos be removed?
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


