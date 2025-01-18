module Interpreter (evaluate) where

import System.Random (StdGen, Random (randomR), mkStdGen)
import Control.Monad.State.Lazy (State, MonadState (get), put, lift, evalState)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Time.Clock.POSIX (getPOSIXTime)

import Syntax
  ( Type(Num, Bool)
  , Term(Number, Boolean, Variable, Let, Add, Leq, Conditional)
  , Distribution(Uniform)
  )

type RandomState = State StdGen
type Name        = String

data Value = NumericValue Int | TruthValue Bool
  deriving (Eq)

instance Show Value where
  show (NumericValue m) = show m
  show (TruthValue   b) = show b

-- * export
evaluate :: Term Type -> IO Value
evaluate t = do
  time <- round `fmap` getPOSIXTime
  let initialState = mkStdGen time
  case evalState (runMaybeT (eval t)) initialState of
    Nothing      -> error "[Internal error]: Evaluation failed due to unhandled case or invalid input"
    Just outcome -> return outcome

-- * implementation
truthy :: Int -> Bool
truthy n
  | n == 0    = False
  | otherwise = True

substitute :: Name -> Term Type -> Term Type -> Term Type
substitute _ _ (Number  n tau) = Number  n tau
substitute _ _ (Boolean b tau) = Boolean b tau
substitute x c (Variable y tau)
  | x == y    = c
  | otherwise = Variable y tau
substitute x c (Add t1 t2 tau) =
  let t1' = substitute x c t1
      t2' = substitute x c t2
  in Add t1' t2' tau
substitute x c (Leq t1 t2 tau) =
  let t1' = substitute x c t1
      t2' = substitute x c t2
  in Leq t1' t2' tau
substitute x c (Conditional t0 t1 t2 tau) =
  let t0' = substitute x c t0
      t1' = substitute x c t1
      t2' = substitute x c t2
  in Conditional t0' t1' t2' tau
substitute x c (Let y t1 t2 tau) =
  let t2' = substitute x c t2
  in Let y t1 t2' tau

sample :: Distribution Type -> RandomState (Term Type)
sample (Uniform (Num l u)) = do
  gen <- get
  let (n, nextGen) = randomR (l, u) gen
  put nextGen
  return $ Number n (Num l u)
sample (Uniform Bool) = do
  gen <- get
  let (l, u      ) = (0,1) :: (Int, Int)
  let (v, nextGen) = randomR (l, u) gen
  let b            = truthy v
  put nextGen
  return $ Boolean b Bool

eval :: Term Type -> MaybeT RandomState Value
eval (Number   n _) = return $ NumericValue  n
eval (Boolean  b _) = return $ TruthValue    b
eval (Variable x _) = error  $ "Variable " ++ x ++ " is not bound"
eval (Add  t0 t1 _) = do
  NumericValue m <- eval t0
  NumericValue n <- eval t1
  return $ NumericValue (m + n)
eval (Leq  t0 t1 _) = do
  NumericValue m <- eval t0
  NumericValue n <- eval t1
  return $ TruthValue (m <= n)
eval (Conditional t0 t1 t2 _) = do
  TruthValue b <- eval t0
  if b then eval t1 else eval t2
eval (Let x dist t2 _) = do
  value <- lift $ sample dist
  eval $ substitute x value t2
  