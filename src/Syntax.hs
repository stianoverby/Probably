{-#LANGUAGE DeriveFunctor #-}

module Syntax
    ( Type(Num)
    , Term(Number, Variable, Let, Add, Leq, Conditional)
    , Distribution(Uniform)
    , Annotated(annotation)
    )
where

type Name = String

data Type = Num Int Int
    deriving (Show, Eq, Ord, Read)

data Distribution a = Uniform a
    deriving (Show, Eq, Ord, Read)

-- * Abbreviations
type T0 a = Term a
type T1 a = Term a
type T2 a = Term a
type D    = Distribution Type

-- * Annotated terms
data Term a = Number      Int                      a
            | Variable    Name                     a
            | Let         Name       D      (T2 a) a
            | Add         (T0    a ) (T1 a)        a
            | Leq         (T0    a ) (T1 a)        a
            | Conditional (T0    a ) (T1 a) (T2 a) a
            deriving (Show, Eq, Functor, Ord, Read)


class Annotated thing where
  annotation  :: thing a -> a
  annotations :: thing a -> [a]

instance Annotated Term where
  annotations (Number       _        a) = return a
  annotations (Variable     _        a) = return a
  annotations (Let          _  _  t2 a) = a : ([t2               ] >>= annotations)
  annotations (Add          t0 t1    a) = a : ([t0, t1           ] >>= annotations)
  annotations (Leq          t0 t1    a) = a : ([t0, t1           ] >>= annotations)
  annotations (Conditional  t0 t1 t2 a) = a : ([t0, t1, t2       ] >>= annotations)
  annotation  t                         = head $ annotations t

instance Annotated Distribution where
    annotations (Uniform a) = return a
    annotation  dist        = head $ annotations dist
