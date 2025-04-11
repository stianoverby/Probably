module Parser(parseString, parseQuery) where

import Text.ParserCombinators.Parsec
import Control.Monad (void)

import Syntax
    ( Type(Num)
    , Term(Number, Variable, Add, Leq, Conditional, Let)
    , Distribution(Uniform)
    )
import Probability(Outcome)

-- * exports
parseString :: String -> Either ParseError UnannotatedTerm
parseString = parse (whitespace *> program <* eof) ""

parseQuery :: String -> Either ParseError Outcome
parseQuery = parse (whitespace *> query <* eof) ""

-- * implementation
type UnannotatedTerm = Term ()

reserved :: [String]
reserved = ["True", "False", "let", "in", "if", "then", "else", "uniform"]

-- * Helpers
whitespace :: Parser ()
whitespace = void $ many $ void space

lexeme :: Parser a -> Parser a
lexeme = (<* whitespace)

symbol :: String -> Parser ()
symbol = lexeme . void . try . string

identifierHead :: Parser Char
identifierHead = letter <|> char '_'

identifierTail :: Parser Char
identifierTail = identifierHead <|> digit

keyword :: String -> Parser ()
keyword s = lexeme $ try $ string s >> notFollowedBy identifierTail


number :: Parser Int
number = lexeme $ fmap read $ (++) <$> prefix <*> digits
    where prefix = option "" $ string "-"
          digits = string "0" <|> many1 digit

identifier :: Parser String
identifier =
    lexeme $ (:) <$> identifierHead <*> many identifierTail >>= unreserved
    where unreserved name =
            if name `elem` reserved
                then fail $ "the name " ++ name ++ " is a reserved keyword!"
                else return name

atom :: Parser UnannotatedTerm
atom =
    choice [ number          >>= \m -> return (Number   m     ())
           , keyword "True"  >>        return (Number   1     ())
           , keyword "False" >>        return (Number   0     ())
           , identifier      >>= \x -> return (Variable x     ())
           , parens expression
           ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operator :: Monoid a => (Term a -> Term a -> a -> Term a) -> Term a -> Term a -> Term a
operator op m n = op m n mempty

-- Transform == to if-stmt using <=
equals :: Monoid a => Term a -> Term a -> Term a
equals t0 t1 =
    Conditional (Leq t0 t1 mempty)
        (Leq t1 t0 mempty)
        (Number 0 mempty)
        mempty

desugarAnd :: Monoid a => Term a -> Term a -> Term a
t0 `desugarAnd` t1 =
    Conditional (t0)
        (t1)
        (Number 0 mempty)
        mempty

desugarOr :: Monoid a => Term a -> Term a -> Term a
t0 `desugarOr` t1 =
    Conditional (t0)
        (Number 1 mempty)
        t1
        mempty

arithmetic :: Parser UnannotatedTerm
arithmetic =
    atom `chainl1`
    choice [ symbol "+" >> return (operator Add)]

nonassoc :: Parser a -> Parser (a -> a -> a) -> Parser a
nonassoc p po = p >>= \v1 -> option v1 (po >>= \f -> p >>= \v2 -> return $ f v1 v2)

relation :: Parser UnannotatedTerm
relation =
    arithmetic `nonassoc`
    choice
        [ symbol "<=" >> return (operator Leq)
        , symbol "==" >> return equals
        ]

distribution :: Parser (Distribution Type)
distribution = keyword "uniform" >>
    choice
        [ keyword "Int"  >> Num <$> number <*> number >>= \tau -> return (Uniform tau)
        , parens distribution
        ]

ifClause :: Parser UnannotatedTerm
ifClause = do
    keyword "if"
    t0 <- expression
    keyword "then"
    t1 <- expression
    keyword "else"
    t2 <- expression
    return (Conditional t0 t1 t2 ())

letClause :: Parser UnannotatedTerm
letClause = do
    keyword "let"
    x <- identifier
    symbol "~"
    dist <- distribution
    keyword "in"
    t2 <- expression
    return (Let x dist t2 ())

andTest :: Parser UnannotatedTerm
andTest =
    relation `chainl1`
    choice [symbol "&&" >> return desugarAnd]

orTest :: Parser UnannotatedTerm
orTest = andTest `chainl1` choice [ symbol "||" >> return desugarOr]

expression :: Parser UnannotatedTerm
expression =
    choice
        [ ifClause
        , letClause
        , orTest
        , parens expression
        ]

program :: Parser UnannotatedTerm
program = expression

query :: Parser Outcome
query =
    choice
        [ number          >>= \m -> return m
        , keyword "True"  >>        return 1
        , keyword "False" >>        return 0
        ]
