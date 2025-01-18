module Main (main) where

import Parser             (parseString, parseQuery)
import Typechecker        (typecheck              )
import Probability        (equals                 )
import Interpreter        (evaluate               )
import System.Exit        (die                    )
import System.Environment (getArgs                )

printParsingError :: String -> IO ()
printParsingError s = putStrLn $ "[Parse error]: " ++ s

printTypingError :: String -> IO ()
printTypingError s = putStrLn $ "[Typing error]: " ++ s

usage :: IO ()
usage = die "Usage:\n\
        \  probably --help             PROGRAM.prob (print this message                              )\n\
        \  probably --parse            PROGRAM.prob (parse only                                      )\n\
        \  probably --typecheck        PROGRAM.ast  (parse && typecheck                              )\n\
        \  probably --equals <outcome> PROGRAM.prob (probability of experiment resulting in outcome  )\n\
        \  probably PROGRAM.prob                    (run experiment                                  )"

main :: IO ()
main = do args <- getArgs
          if "--help" `elem` args
            then usage
            else case args of
            ["--abs-typecheck", file] -> do
              s <- readFile file
              case typecheck (read s) of
                  Left e  -> printTypingError $ show e
                  Right _ -> return mempty
            ["--parse", file] -> do
              s <- readFile file
              case parseString s of
                Left  e -> printParsingError $ show e
                Right p -> print p
            ["--typecheck", file] -> do
              s <- readFile file
              case parseString s of
                Left  e -> printParsingError $ show e
                Right p -> case typecheck p of
                  Left e  -> printTypingError $ show e
                  Right _ -> return mempty
            ["--equals", query, file] -> do
              s <- readFile file
              case parseString s of
                Left  e -> printParsingError $ show e
                Right p -> case parseQuery query of
                  Left  e       -> printParsingError $ show e
                  Right outcome -> case typecheck p of
                    Left  e -> printTypingError $ show e
                    Right t -> print $ show $ t `equals` outcome
            [file] -> do
              s <- readFile file
              case parseString s of
                Left  e -> printParsingError $ show e
                Right p -> case typecheck p of
                  Left  e -> printTypingError $ show e
                  Right t -> evaluate t >>= \val -> print val
            _ -> usage
