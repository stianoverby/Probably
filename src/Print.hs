module Print
    ( pp
    , ppAst
    )
where

import Syntax

-- * Exports
pp :: Show a => Term a -> String
pp = prettyPrint 0

ppAst :: Show a => Term a -> String
ppAst = prettyPrintAST 0

-- * Implementation
prettyPrint :: Show a => Int -> Term a -> String
prettyPrint level term =
  case term of
  Number n a -> prettify $
    [ show n
    , " : "
    , show a
    ]
  Variable x a -> prettify $
    [ x
    , " : "
    , show a
    ]
  Not t a -> prettify $
    [ "not "
    , prettyPrint level t
    , " : "
    , show a
    ]
  Add t1 t2 a -> prettify $
    [ prettyPrint level t1
    , " + "
    , prettyPrint level t2
    , " : "
    , show a
    ]
  Leq t1 t2 a -> prettify $
    [ prettyPrint level t1
    , " <= "
    , prettyPrint level t2
    , " : "
    , show a
    ]
  Conditional c t1 t2 a -> prettify $
    [ "if "
    , prettyPrint (level + 4) c ++ "\n"
    , indent level
        $  "then "
        ++ prettyPrint (level + 6) t1
        ++ "\n"
    , indent level
        $  "else "
        ++ prettyPrint (level + 6) t2
        ++ "\n"
    , indent level
        $  ": "
        ++ show a
    ]
  Let x d t a -> prettify $
    [ "let "
    , x
    , " ~ "
    , prettyDist d ++ "\n"
    , indent level $ "in " ++ prettyPrint (level + 4) t  ++ "\n"
    , indent level $ ": " ++ show a
    ]
  where parens s   = if level == 0 then s else "(" ++ s ++ ")"
        indent n s = concat (replicate n " ") ++ s
        prettify   = parens . concat

prettyDist :: Distribution Type -> String
prettyDist (Uniform (Num l u)) = "uniform Int " ++ show l ++ " " ++ show u

prettyPrintAST :: Show a => Int -> Term a -> String
prettyPrintAST level term = case term of
  Number   i _ -> prettify
    [ "Number "
    , show i
    ]
  Variable x _ -> prettify
    [ "Variable "
    , show x
    ]
  Not      t _ -> prettify
    [ "Not"                         ++ "\n"
    , prettyPrintAST (level + 1) t
    ]
  Add t1 t2  _ -> prettify
    [ "Add"                         ++ "\n"
    , prettyPrintAST (level + 1) t1 ++ "\n"
    , prettyPrintAST (level + 1) t2
    ]
  Leq t1 t2  _ -> prettify
    [ "Leq"                         ++ "\n"
    , prettyPrintAST (level + 1) t1 ++ "\n"
    , prettyPrintAST (level + 1) t2
    ]
  Conditional c t1 t2 _ -> prettify
    [ "Conditional"                 ++ "\n"
    , prettyPrintAST (level + 1) c  ++ "\n"
    , prettyPrintAST (level + 1) t1 ++ "\n"
    , prettyPrintAST (level + 1) t2
    ]
  Let x d t _ -> prettify
    [ "Let "
    , show x                        ++ "\n"
    , prettyPrintASTDist (level + 1) d
    , prettyPrintAST     (level + 1) t
    ]
  where indent  s = replicate (2 * level) ' ' ++ s
        parens  s = if level == 0
                    then s
                    else "(" ++ s ++ ")"
        prettify  = indent . parens . concat

prettyPrintASTDist :: Int -> Distribution Type -> String
prettyPrintASTDist level (Uniform (Num l u)) = concat
    [ indent level
    , "Uniform Int "
    , show l
    , " "
    , show u
    , "\n"
    ]
  where indent n = replicate (2 * n) ' '