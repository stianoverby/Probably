module Typechecker (typecheck) where

import Control.Monad.Reader ( MonadReader(ask) )
import Control.Monad.RWS.Lazy
    ( RWS
    , runRWS
    , MonadWriter(tell), MonadReader (local)
    )

import Syntax
    ( Type(Num, Bool)
    , Term(Number, Boolean, Variable, Let, Add, Leq, Conditional)
    , Annotated(annotation)
    )

-- * Export
typecheck :: Term Unannotated -> Either TypingError (Term Type)
typecheck term =
  let gamma x = error $ x ++ " is not bound any type"
      (term', _, constraints) = runRWS (annotate term) gamma 0
  in case solve constraints of
    Nothing       -> Left  $ ErrorConstraint "Unable to solve typing constraints"
    Just solution -> case refine solution (annotation term') of
        Nothing     -> Left  $ ErrorPolymorphism "Probably does not support polymorphism"
        Just tau    -> Right $ fmap (const tau) term


-- * Implementation
type Name        = String
type Index       = Int
type Unannotated = ()

data TypingError = ErrorUnexpectedType  String
                 | ErrorConstraint      String
                 | ErrorPolymorphism    String
    deriving (Show, Eq)

data Candidate = Hole Index
               | Num' Int Int
               | Bool'
    deriving (Show, Eq)

data Constraint   = Candidate :=: Candidate deriving (Show)
type Substitution = [(Index, Candidate)]
type Environment  = Name -> Candidate
type Inference    = RWS Environment [Constraint] Index


class Substitutable thing where
    substitute :: Index -> Candidate -> thing -> thing

-- * When can a candidate be substituted by another 
instance Substitutable Candidate where
    substitute i c (Hole j) | i == j = c
    substitute _ _ c                 = c

-- * When can a constraint be substituted by another
instance Substitutable Constraint where
    substitute i c0 (c1 :=: c2) =
        let c1' = substitute i c0 c1
            c2' = substitute i c0 c2
        in c1' :=: c2'

sameType :: Term Candidate -> Term Candidate -> Inference ()
t0 `sameType` t1 = tell [annotation t0 :=: annotation t1]

hasType :: Term Candidate -> Candidate -> Inference ()
t `hasType` tau = tell [annotation t :=: tau]

extractNum :: Term Candidate -> Term Candidate -> Either TypingError Candidate
extractNum t0 t1 = 
    case (annotation t0, annotation t1) of
        (Num' l0 u0, Num' l1 u1) -> Right $ Num' (l0 + l1) (u0 + u1)
        (Num' _  _ , tau       ) -> Left $ typeError t1 "Int m n" tau
        (tau       , _         ) -> Left $ typeError t0 "Int m n" tau
  where
    typeError term expected actual = 
        ErrorUnexpectedType 
        $ show term ++ " has expected type '" ++ expected ++ "', but has type " ++ show actual

extractBool :: Term Candidate -> Term Candidate -> Either TypingError Candidate
extractBool t0 t1 = 
    case (annotation t0, annotation t1) of
        (Num' _ _, Num' _ _) -> Right $ Bool'
        (Num' _ _, tau     ) -> Left $ typeError t1 "Bool" tau
        (tau     , _       ) -> Left $ typeError t0 "Bool" tau
  where
    typeError term expected actual = 
        ErrorUnexpectedType 
        $ show term ++ " has expected type '" ++ expected ++ "', but has type " ++ show actual

type2candidate :: Type -> Candidate
type2candidate (Num m n) = Num' m n
type2candidate Bool      = Bool'

bind :: Name -> Candidate -> (Environment -> Environment)
bind name tau env y = if name == y then tau else env y


annotate :: Term Unannotated -> Inference (Term Candidate)
annotate (Number  m  _) =                 return $ Number   m (Num' m m)
annotate (Boolean b  _) =                 return $ Boolean  b  Bool'
annotate (Variable x _) = ask >>= \env -> return $ Variable x (env x)
annotate (Add t0 t1 _) = do
    t0' <- annotate t0
    t1' <- annotate t1
    case extractNum t0' t1' of
        Left  err -> error  $ show err
        Right tau -> return $ Add t0' t1' tau
annotate (Leq t0 t1 _) = do
    t0' <- annotate t0
    t1' <- annotate t1
    case extractBool t0' t1' of
        Left  err -> error  $ show err
        Right tau -> return $ Add t0' t1' tau
annotate (Conditional t0 t1 t2 _) = do
  t0' <- annotate t0
  t1' <- annotate t1
  t2' <- annotate t2
  t0' `hasType` Bool'
  t1' `sameType` t2'
  return $ Conditional t0' t1' t2' (annotation t1')
annotate (Let x dist t2 _) = do
  let tau = type2candidate $ annotation dist
  t2' <- local (bind x tau) (annotate t2)
  return $ Let x dist t2' (annotation t2')


indexes :: Candidate -> [Index]
indexes (Hole i) = [i]
indexes _        = [ ]

solve :: [Constraint] -> Maybe Substitution
solve [] = Just []
solve (c : cs) =
    let t0 :=: t1 = c
    in case c of
        Num' _ _ :=: Num' _ _ -> solve cs
        Bool'    :=: Bool'    -> solve cs
        Hole i   :=: tau
            | i `elem` indexes tau -> if t0 /= t1 then Nothing else solve cs
            | otherwise            -> do
                constraint <- solve (substitute i tau <$> cs)
                return $ (i, tau) : constraint
        tau      :=: Hole i
            | i `elem` indexes tau -> if t0 /= t1 then Nothing else solve cs
            | otherwise            -> do
                constraint <- solve (substitute i tau <$> cs)
                return $ (i, tau) : constraint
        _ -> Nothing

refine :: Substitution -> Candidate -> Maybe Type
refine _  (Num' m n) = Just (Num m n)
refine _  Bool'      = Just Bool
refine [] (Hole _)   = Nothing
refine subs@((i, tau) : _) (Hole j)
  | i == j    = refine subs tau
  | otherwise = refine subs (Hole j)
