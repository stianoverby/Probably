-- * We know that a term will wrap a Type
{-# LANGUAGE FlexibleInstances #-}

-- * We do want to keep the testing stuff in here
{-# OPTIONS_GHC -Wno-orphans #-}

import qualified Data.Map  as Map
import Test.QuickCheck
    ( Gen
    , Arbitrary(arbitrary)
    , choose
    , sized
    , oneof
    , verboseCheck
    , quickCheck, elements
    )
import Syntax
    ( Type(Num)
    , Distribution(Uniform)
    , Term(Number, Let, Add, Leq, Conditional)
    , Annotated (annotation)
    )
import Probability
    ( infer
    , run
    )
main :: IO ()
main = do
    print "[INFO]: Running tests..."
    quickCheck prop_probability_sums_to_1
    quickCheck prop_outcome_in_type
    quickCheck prop_no_negative_occurrences
    print "[INFO]: Success"

-- * Properties

prop_probability_sums_to_1 :: Term Type -> Bool
prop_probability_sums_to_1 t = k == occurrences
  where (k, m)       = infer t
        occurrences  = sum $ Map.elems $ run m

prop_outcome_in_type :: Term Type -> Bool
prop_outcome_in_type t = all (\x -> l <= x && x <= u) outcomes
  where (Num l u)     = annotation t
        (_, m)        = infer t
        outcomes      = Map.keys $ run m

prop_no_negative_occurrences :: Term Type -> Bool
prop_no_negative_occurrences t = all (0 <=) outcomes
  where (_, m)        = infer t
        outcomes      = Map.keys $ run m

-- * Type and Term Type are instances of Arbitrary
instance Arbitrary Type where
    arbitrary = do
        m <- choose (0, 100)
        n <- choose (m, 100)
        return $ Num m n

instance Arbitrary (Term Type) where
  arbitrary = sized generateTerm

-- * Generating functions
genName :: Gen String
genName = do
  c <- elements (['a'..'z'] ++ ['A'..'Z'])
  return [c]

generateTerm :: Int -> Gen (Term Type)
generateTerm 0 = do
  tau@(Num l u) <- arbitrary
  m             <- choose (l, u)
  return $ Number m tau
generateTerm x = oneof
  [ generateNum   x
  , generateAdd  (x `div` 2)
  , generateLeq  (x `div` 2)
  , generateCond (x `div` 3)
  , generateLet  (x `div` 8)
  ]
generateNum :: Int -> Gen (Term Type)
generateNum _ = do
  tau@(Num l u) <- arbitrary
  m <- choose (l, u)
  return $ Number m tau

generateAdd :: Int -> Gen (Term Type)
generateAdd _ = do
    tau1@(Num l1 u1) <- arbitrary
    tau2@(Num l2 u2) <- arbitrary
    m <- choose (l1, u1)
    n <- choose (l2, u2)
    return $ Add
      (Number m tau1)
      (Number n tau2)
      (Num (l1 + l2) (u1 + u2))

generateLeq :: Int -> Gen (Term Type)
generateLeq m = do
    t1 <- generateNum m
    t2 <- generateNum m
    return $ Leq t1 t2 (Num 0 1)

generateCond :: Int -> Gen (Term Type)
generateCond m = do
  t0 <- generateTerm m
  t1 <- generateTerm m
  t2 <- generateTerm m
  let (Num l1 u1) = annotation t1
      (Num l2 u2) = annotation t2
  return $ Conditional t0 t1 t2 (Num (min l1 l2) (max u1 u2))

generateLet :: Int -> Gen (Term Type)
generateLet m = do
  x   <- genName
  tau <- arbitrary
  t   <- generateTerm m
  return $ Let x (Uniform tau) t (annotation t)