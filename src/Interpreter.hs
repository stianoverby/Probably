module Interpreter (evaluate) where

import System.Random (StdGen, Random (randomR), mkStdGen)
import Control.Monad.State.Lazy (State, MonadState (get), put, lift, evalState)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Time.Clock.POSIX (getPOSIXTime)

import Syntax
  ( Type(Num)
  , Term(Number, Variable, Let, Add, Leq, Conditional)
  , Distribution(Uniform)
  )

type RandomState = State StdGen
type Name        = String
type Value       = Int

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

leq :: Int -> Int -> Int
leq m n
  | m <= n    = 1
  | otherwise = 0

substitute :: Name -> Term Type -> Term Type -> Term Type
substitute _ _ (Number  n tau) = Number  n tau
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

eval :: Term Type -> MaybeT RandomState Value
eval (Number   n _) = return n
eval (Variable x _) = error  $ "Variable " ++ x ++ " is not bound"
eval (Add  t0 t1 _) = (+)  <$> eval t0 <*> eval t1
eval (Leq  t0 t1 _) = leq  <$> eval t0 <*> eval t1
eval (Conditional t0 t1 t2 _) = do
  b <- eval t0
  if truthy b
    then eval t1
    else eval t2
eval (Let x dist t2 _) = do
  value <- lift $ sample dist
  eval $ substitute x value t2
