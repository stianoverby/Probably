{-# LANGUAGE GADTs #-}
module Probability
    ( equals
    , run
    , infer
    , Outcome
    ) where

import qualified Data.Map as Map
import Control.Monad.Reader (Reader, ask, runReader, forM, MonadReader (local), )
import Data.Ratio (Ratio, (%))

import Syntax
    ( Type(Num)
    , Term(Number, Variable, Let, Add, Leq, Conditional)
    , Distribution(Uniform)
    )
import Control.Monad (liftM, ap)

-- * Export
equals :: Term Type -> Outcome -> Probability
term `equals` outcome =
    let annotation = infer term
        (k, _)     = annotation
    in case event annotation outcome of
        Just occ -> occ % k
        _        -> 0

-- * Implementation
type Probability      = Ratio Int
type Total            = Int
type Occurrences      = Int
type OccurrenceMap a  = Map.Map a Occurrences

type Outcome = Int

type Annotation = (Total, Observed Outcome)

type Name          = String
type Environment   = Name -> Outcome

data Observed a where
    Certainly  :: a -> Observed a
    Impossible :: Observed a
    Bind       :: Observed a -> (a -> Observed b) -> Observed b
    Lift       :: (Ord a, Num a) => OccurrenceMap a -> Observed a
    Kleisli    :: (a -> Observed b) -> (b -> Observed c) -> (a -> Observed c)

instance Functor Observed where
    fmap = liftM

instance Applicative Observed where
    pure = Certainly
    (<*>) = ap

instance Monad Observed where
    return = pure
    (>>=)  = Bind
    --(>>=) a mf = Kleisli id mf a

each :: OccurrenceMap a -> [(a, Occurrences)]
each = Map.assocs

apply :: (Ord b) => OccurrenceMap a -> (a -> Observed b) -> OccurrenceMap b
m `apply` f =
    foldr (uncurry (Map.insertWith (+))) Map.empty $
      do (a, occ0) <- each m
         (b, occ1) <- each $ (run . f) a
         return (b, occ0 * occ1)

run :: (Ord a) => Observed a -> OccurrenceMap a
run (Lift    m) = m
run (Certainly a) = Map.singleton a 1
run (Impossible ) = Map.empty
run (Bind ma f) = case ma of
    Lift    a     ->  a `apply` f
    Certainly a     ->  run $ f a
    Impossible      ->  run $ Impossible
    Bind mb g       ->  run $ Bind mb (\a -> Bind (g a) f)
    Kleisli mf mg b ->  run $ Bind (Bind (mf b) mg) f
run (Kleisli f g a) =
    -- Semantics of Kleisli (unused)
    -- (>=>) : (a -> m a) -> (b -> m b) -> (a -> m b)
    case f a of
        Lift     b       -> b `apply` g
        Certainly  b       -> run $ g b
        Impossible         -> run Impossible
        (Kleisli h i b)    -> run $ Kleisli (const (h b)) (Kleisli i g) b
        (Bind a' f'      ) -> run $ Kleisli (Kleisli (const a') f') g a


count :: Annotation -> Total
count    = fst

observed :: Annotation -> Observed Outcome
observed = snd

event :: Annotation -> Outcome -> Maybe Occurrences
event (_, m) outcome = Map.lookup outcome (run m)

chanceOf :: Annotation -> Outcome -> Probability
chanceOf a outcome = case event a outcome of
  Just k -> k % count a
  _      -> 0

bind :: Name -> Outcome -> (Environment  -> Environment)
bind name outcome env x = if name == x then outcome else env x

domain :: Type -> [Outcome]
domain (Num m n) = [m .. n]

uniform :: Ord a => [a] -> OccurrenceMap a
uniform values = Map.fromList [(v, 1) | v <- values]

interpret :: Term Type -> Reader Environment Annotation
interpret (Number  n _) =
    let k = 1
        m = return n
    in return (k, m)
interpret (Variable x _) = do
    env <- ask
    let k = 1
        m = return (env x)
    return (k, m)
interpret (Add t0 t1 _) = do
    a0 <- interpret t0
    a1 <- interpret t1
    let
        k = count a0 * count a1
        m = (+) <$> observed a0 <*> observed a1
    return (k, m)
interpret (Leq t0 t1 _) = do
    a0 <- interpret t0
    a1 <- interpret t1
    let k = count a0 * count a1
        m = (\a b  -> if a <= b then 1 else 0) <$> observed a0 <*> observed a1
    return (k, m)
interpret (Conditional t0 t1 t2 _) = do
    a0 <- interpret t0
    a1 <- interpret t1
    a2 <- interpret t2
    return $ case a0 `chanceOf` 0 of
        0 -> a1
        1 -> a2
        _ -> error "Internal error: A branch has to be picked with probability 1"
interpret (Let x (Uniform tau) t2 _) = do
    let outcomes = domain tau
    results <- forM outcomes $ \outcome -> do
        (k', m') <- local (bind x outcome) (interpret t2)
        return (outcome, k', m')
    let k = sum [k' | (_, k', _) <- results]
        m = Bind (Lift $ uniform $ domain tau) $ \val ->
            case lookup val [(outcome, m') | (outcome, _, m') <- results] of
                Just m'  -> m'
                Nothing  -> error "unexpected value in Let binding"
    return (k, m)

infer :: Term Type -> Annotation
infer t = runReader (interpret t) gamma
    where gamma x = error $ x ++ " is not bound to any outcome"
