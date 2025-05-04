module Main (main) where

import Parser             (parseString, parseQuery )
import Typechecker        (typecheck               )
import Probability        (equals, less, present   )
import Interpreter        (evaluate                )
import Print              (pp, ppAst               )
import System.Exit        (die                     )
import System.Environment (getArgs                 )

printParsingError :: String -> IO ()
printParsingError s = putStrLn $ "[Parse error]: " ++ s

usage :: IO ()
usage = die "Usage:\n\
        \  probably --help             PROGRAM.prob (print this message                          )\n\
        \  probably --parse            PROGRAM.prob (parse only                                  )\n\
        \  probably --typecheck        PROGRAM.ast  (parse && typecheck                          )\n\
        \  probably --equals <outcome> PROGRAM.prob (probability of particular outcome           )\n\
        \  probably --less   <outcome> PROGRAM.prob (probability of less than particular outcome )\n\
        \  probably --show             PROGRAM.ast  (infer and print out distribution            )\n\
        \  probably                    PROGRAM.prob (run experiment                              )"

main :: IO ()
main = do args <- getArgs
          if "--help" `elem` args
            then usage
            else case args of
            ["--abs-typecheck", file] -> do
              s <- readFile file
              _ <- return $ typecheck (read s)
              return ()
            ["--parse", file] -> do
              s <- readFile file
              case parseString s of
                Left  e -> printParsingError $ show e
                Right p -> putStrLn          $ ppAst p
            ["--typecheck", file] -> do
              s <- readFile file
              case parseString s of
                Left err -> printParsingError $ show err
                Right p  -> putStrLn          $ pp $ typecheck p
            ["--less", query, file] -> do
              s <- readFile file
              case parseString s of
                Left  e -> printParsingError $ show e
                Right p -> case parseQuery query of
                  Left  e       -> printParsingError $ show e
                  Right outcome -> print (typecheck p `less` outcome)
            ["--equals", query, file] -> do
              s <- readFile file
              case parseString s of
                Left  e -> printParsingError $ show e
                Right p -> case parseQuery query of
                  Left  e       -> printParsingError $ show e
                  Right outcome -> print (typecheck p `equals` outcome)
            ["--show", file] -> do
              s <- readFile file
              case parseString s of
                Left e  -> printParsingError $ show e
                Right p -> mapM_ print $ present $ typecheck p
            [file] -> do
              s <- readFile file
              case parseString s of
                Left  e -> printParsingError $ show e
                Right p -> evaluate (typecheck p) >>= \val -> print val
            _ -> usage
