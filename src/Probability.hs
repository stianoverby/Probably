module Probability
    ( equals
    , Outcome(NumericValue, TruthValue)
    ) where

import qualified Data.Map as Map
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Ratio (Ratio, (%))

import Syntax
    ( Type(Num, Bool)
    , Term(Number, Boolean, Variable, Let, Add, Leq, Conditional)
    , Distribution(Uniform)
    )

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
type Observed a  = Map.Map a Occurrences

data Outcome = NumericValue Int | TruthValue Bool
    deriving (Show, Eq, Ord, Read)


type Annotation = (Total, Observed Outcome)

type Name          = String
type Environment   = Name -> Outcome

event :: Annotation -> Outcome -> Maybe Occurrences
event (_, m) outcome =  Map.lookup outcome m

count :: Annotation -> Total
count = fst

observed :: Annotation -> Observed Outcome
observed = snd

identical :: Annotation -> Outcome -> Probability
identical a outcome = case event a outcome of
  Just k -> k % count a
  _       -> 0

each :: Annotation -> [(Outcome, Occurrences)]
each (_, m) = Map.assocs m

binop :: (Outcome -> Outcome -> Outcome) -> Annotation -> Annotation -> Observed Outcome
binop f a0 a1 =
    foldr (uncurry (Map.insertWith (+))) Map.empty $
      do (v0, occ0) <- each a0
         (v1, occ1) <- each a1
         return (f v0 v1, occ0 * occ1)

pluss :: Outcome -> Outcome -> Outcome
(NumericValue m) `pluss` (NumericValue n) = NumericValue $ m + n
v1 `pluss` v2                             = error $
    "Internal error: 'pluss' not supported between " ++ show v1 ++ " and " ++ show v2

leq :: Outcome -> Outcome -> Outcome
(NumericValue m) `leq` (NumericValue n) = TruthValue $ m <= n
v1 `leq` v2                             = error $
    "Internal error: 'leq' not supported between " ++ show v1 ++ " and " ++ show v2

domain :: Type -> [Outcome]
domain (Num m n) = NumericValue <$> [m .. n     ]
domain Bool      = TruthValue   <$> [True, False]

bind :: Name -> Outcome -> (Environment  -> Environment)
bind name outcome env x = if name == x then outcome else env x

interpret :: Term Type -> Reader Environment Annotation
interpret (Number n _) =
    let k = 1
        m = Map.singleton (NumericValue n) 1
    in return (k, m)
interpret (Boolean b _) =
    let k = 1
        m = Map.singleton (TruthValue   b) 1
    in return (k, m)
interpret (Variable x _) = do
    env <- ask
    let k = 1
        m = Map.singleton (env x) 1
    return (k, m)
interpret (Add t0 t1 _) = do
    a0 <- interpret t0
    a1 <- interpret t1
    let k = count a0 * count a1
        m = binop pluss a0 a1
    return (k, m)
interpret (Leq t0 t1 _) = do
    a0 <- interpret t0
    a1 <- interpret t1
    let k = count a0 * count a1
        m = binop leq a0 a1
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
        m             = foldr (Map.unionWith (+) . observed) Map.empty annotations
    return (k, m)

infer :: Term Type -> Annotation
infer t = runReader (interpret t) gamma
    where gamma x = error $ x ++ " is not bound to any outcome"
