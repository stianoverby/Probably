{-# LANGUAGE GADTs #-}
module Probability
    ( equals
    , Outcome(NumericValue, TruthValue)
    ) where

import qualified Data.Map as Map
import Control.Monad.Reader (Reader, ask, runReader, MonadPlus (..))
import Data.Ratio (Ratio, (%))

import Syntax
    ( Type(Num, Bool)
    , Term(Number, Boolean, Variable, Let, Add, Leq, Conditional)
    , Distribution(Uniform)
    )
import Control.Monad (liftM, ap)
import Control.Applicative (Alternative (empty, (<|>)))

-- * Export
equals :: Term Type -> Outcome -> Probability
term `equals` outcome =
    let annotation = infer term
        (k, _)     = annotation
    in case event annotation outcome of
        Just occ -> occ % k
        _        -> 0

-- * Implementation
type Probability = Ratio Int
type Total       = Int
type Occurrences = Int
type OccurrenceMap a  = Map.Map a Occurrences

data Outcome = NumericValue Int | TruthValue Bool
    deriving (Show, Eq, Ord, Read)

type Annotation = (Total, Observed Outcome)

type Name          = String
type Environment   = Name -> Outcome

data Observed a where
    Lift   :: (Ord a, Show a) => OccurrenceMap a ->  Observed a
    Return :: a -> Observed a
    Bind   :: Observed a -> (a -> Observed b) -> Observed b
    Zero   :: Observed a
    Plus   :: Observed a -> Observed a -> Observed a

instance Functor Observed where
    fmap = liftM

instance Applicative Observed where
    pure = Return
    (<*>) = ap

instance Monad Observed where
    return = pure
    (>>=)  = Bind

instance Alternative Observed where
    empty = Zero
    (<|>) = Plus

instance MonadPlus Observed where
  mzero = Zero
  mplus = Plus

each :: OccurrenceMap a -> [(a, Occurrences)]
each = Map.assocs

union :: (Ord a) => OccurrenceMap a -> OccurrenceMap a -> OccurrenceMap a
m0 `union`m1 = Map.unionWith (+) m0 m1

apply :: ( Ord b) => OccurrenceMap a -> (a -> Observed b) -> OccurrenceMap b
m `apply` f =
    foldr (uncurry (Map.insertWith (+))) Map.empty $
      do (a, occ0) <- each m
         (b, occ1) <- each $ (run . f) a
         return (b, occ0 * occ1)

run :: (Ord a) => Observed a -> OccurrenceMap a
run (Lift     m)            = m
run (Return   a)            = Map.singleton a 1
run Zero                    = Map.empty
run (Plus ma mb)            = run ma `union` run mb
run (Bind (Lift   m) f)     =     m  `apply`       f
run (Bind (Return a) f)     = run (f a)
run (Bind Zero _)           = run Zero
run (Bind (Plus (Lift   m)   mb        ) f) = run (Bind (Lift (m      `union` run mb)) f)
run (Bind (Plus ma           (Lift m)  ) f) = run (Bind (Lift (run ma `union`     m )) f)
run (Bind (Plus (Return a)   mb        ) f) = run (Plus (f a)       (Bind mb f))
run (Bind (Plus ma           (Return b)) f) = run (Plus (Bind ma f) (f b      ))
run (Bind (Plus Zero         mb        ) f) = run (Bind mb f)
run (Bind (Plus ma           Zero      ) f) = run (Bind ma f)
run (Bind (Plus (Plus ma mb) mc        ) f) = run (Bind (Plus ma (Plus mb mc)) f)
run (Bind (Plus ma           mb        ) f) = run (Plus (Bind ma f) (Bind mb f) )
run (Bind (Bind ma f) g)                    = run (Bind ma (\a -> Bind (f a) g))

count :: Annotation -> Total
count    = fst

observed :: Annotation -> Observed Outcome
observed = snd

event :: Annotation -> Outcome -> Maybe Occurrences
event (_, m) outcome =
    let m' = run m
    in Map.lookup outcome m'

identical :: Annotation -> Outcome -> Probability
identical a outcome = case event a outcome of
  Just k -> k % count a
  _       -> 0

plus :: Outcome -> Outcome -> Outcome
(NumericValue m) `plus` (NumericValue n) = NumericValue $ m + n
v1 `plus` v2                             = error $
    "Internal error: 'pluss' not supported between " ++ show v1 ++ " and " ++ show v2

leq :: Outcome -> Outcome -> Outcome
(NumericValue m) `leq` (NumericValue n) = TruthValue $ m <= n
v1 `leq` v2                             = error $
    "Internal error: 'leq' not supported between " ++ show v1 ++ " and " ++ show v2

bind :: Name -> Outcome -> (Environment  -> Environment)
bind name outcome env x = if name == x then outcome else env x

domain :: Type -> [Outcome]
domain (Num m n) = NumericValue <$> [m .. n     ]
domain Bool      = TruthValue   <$> [True, False]

interpret :: Term Type -> Reader Environment Annotation
interpret (Number  n _) = return (k, m)
    where k = 1
          m = return (NumericValue n)
interpret (Boolean b _) = return (k, m)
    where k = 1
          m = return (TruthValue   b)
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
        m = plus <$> observed a0 <*> observed a1
    return (k, m)
interpret (Leq t0 t1 _) = do
    a0 <- interpret t0
    a1 <- interpret t1
    let k = count a0 * count a1
        m = leq <$> observed a0 <*> observed a1
    return (k, m)
interpret (Conditional t0 t1 t2 _) = do
    a0 <- interpret t0
    a1 <- interpret t1
    a2 <- interpret t2
    return $ case a0 `identical` TruthValue True of
        1 -> a1
        0 -> a2
        _ -> error "Internal error: A branch has to be picked with probability 1"
interpret (Let x (Uniform tau) t2 _) = do
    env <- ask
    let annotations   = [runReader (interpret t2) (bind x val env) | val <- domain tau]
        k             = sum $ map count annotations
        m             = Lift $ foldr (union . run . observed) mempty annotations
    return (k, m)

infer :: Term Type -> Annotation
infer t = runReader (interpret t) gamma
    where gamma x = error $ x ++ " is not bound to any outcome"
