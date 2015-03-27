module Explorer where

import Language.Haskell.Syntax

data EO = E HsExp | O HsQOp

exploreExprs f exprs = exploreEO f $ map (E) exprs

exploreEO f es = any f es || any (exploreEO' f) es

exploreEO' :: (EO -> Bool) -> EO -> Bool
exploreEO' f (E (HsInfixApp a b c)) = exploreEO f [E a, O b, E c]
exploreEO' f (E (HsApp a b))  = exploreEO f [E a, E b]
exploreEO' f (E (HsNegApp a))  = exploreEO f [E a]
exploreEO' f (E (HsLambda _ _ a)) = exploreEO f [E a]
exploreEO' f (E (HsList a)) = exploreEO f $ map (E) a
exploreEO' _ _ = False

topExprs (HsUnGuardedRhs e) = [e]
topExprs (HsGuardedRhss rhss) = concatMap (\(HsGuardedRhs _ es1 es2) -> [es1, es2]) rhss
