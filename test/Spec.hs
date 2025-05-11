-- * We know that a term will only wrap a Type
{-# LANGUAGE FlexibleInstances #-}

-- * We want to keep the testing stuff in here
{-# OPTIONS_GHC -Wno-orphans #-}

import qualified Data.Map as Map
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
import Control.Monad      (when              )
import System.Environment (getArgs           )
import Data.Ratio         (Ratio, (%)        )

import Parser             (parseString             )
import Typechecker        (typecheck               )
import Probability        (infer, run, equals, less)
import Syntax
  ( Type        (Num                                              )
  , Distribution(Uniform                                          )
  , Annotated   (annotation                                       )
  , Term        (Number, Variable, Not, Let, Add, Leq, Conditional)
  )

type Assertion = IO ()
type TestCase  = (Int, Ratio Int, Ratio Int)
type Path      = String

main :: IO ()
main = do
    args <- getArgs
    let check = if "--verbose" `elem` args
                then verboseCheck
                else quickCheck
    putStrLn ""
    putStrLn "[INFO]: Running property based tests..."
    check prop_probability_sums_to_1
    check prop_outcome_in_type
    check prop_no_negative_occurrences


    putStrLn "[INFO]: Running regression tests..."
    mapM_ (uncurry runRegressionTest) $ Map.toList testInput
    putStrLn "[INFO]: Regression tests successful..."

-- * Unit Testing

testInput :: Map.Map Path TestCase
testInput = Map.fromList
  [ ("example-experiments/simple-01.prob"  , (5,  1 % 10  , 4    % 10  ))
  , ("example-experiments/simple-02.prob"  , (5,  1 % 100 , 4    % 100 ))
  , ("example-experiments/slotMachine.prob", (1,  1 % 8000, 7999 % 8000))
  , ("example-experiments/throw3dice.prob" , (14, 5 % 72  , 181  %  216))
  , ("example-experiments/throw5dice.prob" , (14, 5 % 72  , 197  % 1296))
  ]

test :: String -> IO () -> IO ()
test label action = do
  putStrLn $ "[TEST]: " ++ label
  action

assertFailure :: String -> Assertion
assertFailure msg = ioError (userError ("Assertion failed: " ++ msg))

testEquals :: String -> Int -> Ratio Int -> Assertion
testEquals program outcome expected = do
  case parseString program of
    Left err -> fail $ "Parse error: " ++ show err
    Right term ->
      let actual = typecheck term `equals` outcome
      in  when (actual /= expected) $
           assertFailure $ "Expected: " ++ show expected ++ ", got: " ++ show actual

testLess :: String -> Int -> Ratio Int -> Assertion
testLess program outcome expected = do
  case parseString program of
    Left err -> fail $ "Parse error: " ++ show err
    Right term ->
      let actual = typecheck term `less` outcome
      in  when (actual /= expected) $
           assertFailure $ "Expected: " ++ show expected ++ ", got: " ++ show actual

runRegressionTest :: Path -> TestCase -> Assertion
runRegressionTest path (input, exEq, exLess) = do
  program <- readFile path
  let msgEq   = path ++ " P(" ++ show input ++ ")" ++  " == " ++ show exEq
  let msgLess = path ++ " P(" ++ show input ++ ")" ++  " == " ++ show exLess
  test msgEq   $ testEquals program input exEq
  test msgLess $ testLess program input exLess

-- * Property Based Testing

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
  [ generateNum  (seed `div` 2)
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