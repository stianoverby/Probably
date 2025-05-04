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
    , quickCheck
    , verboseCheck
    , elements
    )
import System.Environment (getArgs)
import Syntax
    ( Type(Num)
    , Distribution(Uniform)
    , Term(Number, Variable, Not, Let, Add, Leq, Conditional)
    , Annotated(annotation)
    )
import Probability
    ( infer
    , run
    )
main :: IO ()
main = do
    args <- getArgs
    let check = if "--verbose" `elem` args then verboseCheck else quickCheck
    print "[INFO]: Running tests..."
    check prop_probability_sums_to_1
    check prop_outcome_in_type
    check prop_no_negative_occurrences
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
        m <- choose (0, 50)
        n <- choose (m, 50)
        return $ Num m n

instance Arbitrary (Term Type) where
  arbitrary = sized generateTerm

-- * Generating functions
genName :: Gen String
genName = do
  c <- elements (['a'..'z'] ++ ['A'..'Z'])
  return [c]

generateTerm :: Int -> Gen (Term Type)
generateTerm 0    = generateNum 0
generateTerm seed = oneof
  [ generateNum   seed
  , generateAdd  (seed `div` 2)
  , generateNot  (seed `div` 2)
  , generateLeq  (seed `div` 2)
  , generateCond (seed `div` 3)
  , generateLet  (seed `div` 8)
  ]
generateNum :: Int -> Gen (Term Type)
generateNum seed = do
  m <- choose (0, max seed 0)
  return $ Number m (Num m m)

generateAdd :: Int -> Gen (Term Type)
generateAdd seed = do
    m <- generateNum seed
    n <- generateNum seed
    let tau = combineTypes (annotation m) (annotation n)
    return $ Add m n tau

generateNot :: Int -> Gen (Term Type)
generateNot seed = do
  t <- generateTerm seed
  return $ Not t (Num 0 1)

generateLeq :: Int -> Gen (Term Type)
generateLeq seed = do
    t1 <- generateNum seed
    t2 <- generateNum seed
    return $ Leq t1 t2 (Num 0 1)

generateCond :: Int -> Gen (Term Type)
generateCond seed = do
  t0 <- generateTerm seed
  t1 <- generateTerm seed
  t2 <- generateTerm seed
  let (Num l1 u1) = annotation t1
      (Num l2 u2) = annotation t2
  return $ Conditional t0 t1 t2 (Num (min l1 l2) (max u1 u2))

generateLet :: Int -> Gen (Term Type)
generateLet seed = do
  x    <- genName
  tau  <- arbitrary
  t    <- generateTerm seed
  let var = Variable x tau
  body <- oneof
    [ return t
    , return $ Add var t   (combineTypes tau            (annotation t))
    , return $ Add t   var (combineTypes (annotation t) tau           )
    , return $ Leq var t   (Num 0 1)
    , return $ Leq t   var (Num 0 1)
    , return $ Not var     (Num 0 1)
    ]
  return $ Let x (Uniform tau) body (annotation body)

combineTypes :: Type -> Type -> Type
combineTypes (Num l1 u1) (Num l2 u2) = Num (l1 + l2) (u1 + u2)