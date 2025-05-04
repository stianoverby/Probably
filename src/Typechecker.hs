module Typechecker (typecheck) where

import Control.Monad.Reader ( MonadReader(ask, local), Reader, runReader )

import Syntax
    ( Type(Num)
    , Term(Number, Variable, Not, Let, Add, Leq, Conditional)
    , Annotated(annotation)
    )

-- * Export
typecheck :: Term Unannotated -> Term Type
typecheck term = let gamma x = error $ x ++ "is not bound to any type!"
    in runReader (annotate term) gamma


-- * Implementation
type Name         = String
type Unannotated  = ()
type Environment  = Name -> Type
type Inference    = Reader Environment

bind :: Name -> Type -> (Environment -> Environment)
bind name tau env y = if name == y then tau else env y

annotate :: Term Unannotated -> Inference (Term Type)
annotate (Number  m  _) =                 return $ Number   m (Num m m)
annotate (Variable x _) = ask >>= \env -> return $ Variable x (env x)
annotate (Not     t  _) = do
    t' <- annotate t
    let tau = Num 0 1
    return $ Not t' tau
annotate (Add t0 t1 _)  = do
    t0' <- annotate t0
    t1' <- annotate t1
    let (Num l0 u0) = annotation t0'
        (Num l1 u1) = annotation t1'
        tau = Num (l0 + l1) (u0 + u1)
    return $ Add t0' t1' tau
annotate (Leq t0 t1 _) = do
    t0' <- annotate t0
    t1' <- annotate t1
    let tau = Num 0 1
    return $ Leq t0' t1' tau
annotate (Conditional t0 t1 t2 _) = do
  t0' <- annotate t0
  t1' <- annotate t1
  t2' <- annotate t2
  return $ Conditional t0' t1' t2' (annotation t1')
annotate (Let x dist t2 _) = do
  let tau = annotation dist
  t2' <- local (bind x tau) (annotate t2)
  return $ Let x dist t2' (annotation t2')
