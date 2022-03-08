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

-- | The main game object where all game info is contained. 
data Game = Game
    {
        board         :: Board,
        pieces        :: [Piece],
        dice          :: [Die],
        -- equipment  :: (Equipment t) => [t],
        players       :: [Player],
        rules         :: [Rule],
        endConditions :: EndCondition
    }


-- | A rule object with a function which has to be fulfilled in
--   order to be able to place a piece on the board.
newtype Rule = PlaceRule (Pos -> Board -> Bool)

-- | A simple vector object containing a x and a y value
data Pos = Pos Int Int 
    deriving (Eq, Show)

-- | A record containing conditions to be met for the game to end.
--   It can have multiple functions for draws and wins. 
data EndCondition = EndCondition {
    drawCondition :: [Board -> Bool],
    winCondition  :: [Board -> Bool]
}

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