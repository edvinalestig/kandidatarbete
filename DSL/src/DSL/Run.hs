module DSL.Run (
    runUpdate,
    runRule,
    runCondition
) where
import DSL.Types
import DSL.Utility (_placePiece, turnToPos)
import Control.Monad.Loops
import Data.Maybe


runUpdate :: Update t -> Turn -> t -> t
runUpdate (Update f)        t g = f t g
runUpdate (u1 `COMBINE` u2) t g = runUpdate u2 t (runUpdate u1 t g)


runRule :: Rule -> Turn -> Game -> Maybe Game
runRule (Rule f)         t g = Just (runUpdate f t g)
runRule (TurnRule f r)   t g = runRule r (runUpdate f t t) g
runRule (If c r)         t g = if runCondition c t g then runRule r t g else Nothing
runRule (IfElse c r1 r2) t g = if runCondition c t g then runRule r1 t g else runRule r2 t g
runRule (r1 `SEQ` r2)    t g = runRule r1 t g >>= runRule r2 t -- equal to: if isJust (runRule r1 t g) then runRule r2 t (fromJust iter1) else Nothing
runRule (r1 `THEN` r2)   t g = case runRule r1 t g of
                                    Just c  -> runRule r2 t c
                                    Nothing -> runRule r2 t g
runRule (IterateUntil r c) t g = runUntilMain c r t g

-- | Uses `runUntil`, if the result is Left then that result of `runUntil` is returned.
-- If the result is Right then the input `Game` is returned.
runUntilMain :: Condition Turn -> Rule -> Turn -> Game -> Maybe Game
runUntilMain c r@(TurnRule u r') t g = case runUntil c r t g of
                                        Left a -> Just a
                                        Right a -> Just g
runUntilMain _ _ _ g = error "runUntilMain: Cannot have an IterateUntil without TurnRule"


runUntil :: Condition Turn -> Rule -> Turn -> Game -> Either Game Game
runUntil c r@(TurnRule u r') t g = 
    if runCondition (NOT c) t' g then
        case runRule r' t' g of
            Just game -> runUntil c r t' game
            Nothing   -> Right g
    else
        Left g
    where
        t' = runUpdate u t t
runUntil _ _ _ g = error "Cannot have an IterateUntil without TurnRule"


replaceTiles :: Game -> [Tile] -> Game
replaceTiles g []     = g
replaceTiles g (t:ts) = case t of 
    (PieceTile p pos) -> replaceTiles (g' p pos) ts
    _                 -> replaceTiles g  ts
    where
        g' p pos = _placePiece (Turn p (Place pos)) g


runCondition :: Condition Turn -> Turn -> Game -> Bool
runCondition (Condition c) t g = _isWithinBoard t g && c t g
runCondition (c1 `AND` c2) t g = runCondition c1 t g && runCondition c2 t g
runCondition (c1 `OR` c2)  t g = runCondition c1 t g || runCondition c2 t g
runCondition (NOT c)       t g = not $ runCondition c t g

_isWithinBoard :: Turn -> Game -> Bool
_isWithinBoard t g = x >= 0 && x < (length . head . board) g && y >= 0 && y < (length . board) g
    where
        (Pos x y) = turnToPos t