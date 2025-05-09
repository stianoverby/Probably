module Parser(parseString, parseQuery) where

import Control.Monad (void)
import Text.ParserCombinators.Parsec
    ( Parser
    , ParseError
    , char
    , digit
    , letter
    , space
    , string
    , between
    , chainl1
    , choice
    , eof
    , many1
    , notFollowedBy
    , option
    , (<|>)
    , many
    , parse
    , try
    , noneOf
    )

import Probability(Outcome)
import Syntax
    ( Type        (Num                                              )
    , Term        (Number, Variable, Not, Add, Leq, Conditional, Let)
    , Distribution(Uniform                                          )
    )

-- * exports
parseString :: String -> Either ParseError UnannotatedTerm
parseString = parse (whitespace *> program <* eof) ""

parseQuery :: String -> Either ParseError Outcome
parseQuery = parse (whitespace *> query <* eof) ""

-- * implementation
type UnannotatedTerm = Term ()

reserved :: [String]
reserved = ["True", "False", "let", "in", "if", "then", "else", "uniform", "not"]

-- * Helpers

comment :: Parser ()
comment = void $ symbol "--" >> many (noneOf "\n")

whitespace :: Parser ()
whitespace = void $ many $ void space <|> comment

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
atom = choice
    [ number          >>= \m -> return (Number   m     ())
    , keyword "True"  >>        return (Number   1     ())
    , keyword "False" >>        return (Number   0     ())
    , identifier      >>= \x -> return (Variable x     ())
    , parens expression
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operator :: Monoid a => (Term a -> Term a -> a -> Term a) -> Term a -> Term a -> Term a
operator op m n = op m n mempty

-- (a == b) = (if a then b else False)
sugarEquals :: Monoid a => Term a -> Term a -> Term a
sugarEquals t0 t1 =
    Conditional (Leq t0 t1 mempty)
        (Leq t1 t0 mempty)
        (Number 0 mempty)
        mempty

-- (a < b) = (a <= b && a /= b)
sugarLess :: Monoid a => Term a -> Term a -> Term a
sugarLess t0 t1 = Leq t0 t1 mempty `sugarAnd` Not (Leq t1 t0 mempty) mempty

sugarAnd :: Monoid a => Term a -> Term a -> Term a
t0 `sugarAnd` t1 =
    Conditional t0
        t1
        (Number 0 mempty)
        mempty

sugarOr :: Monoid a => Term a -> Term a -> Term a
t0 `sugarOr` t1 =
    Conditional t0
        (Number 1 mempty)
        t1
        mempty

notTest :: Parser UnannotatedTerm
notTest =
    choice
    [ keyword "not" >> (orTest >>= \t -> return (Not t ()))
    , atom
    ]

arithmetic :: Parser UnannotatedTerm
arithmetic = notTest `chainl1` (symbol "+" >> return (operator Add))

nonassoc :: Parser a -> Parser (a -> a -> a) -> Parser a
nonassoc p po = p >>= \v1 -> option v1 (po >>= \f -> p >>= \v2 -> return $ f v1 v2)

relation :: Parser UnannotatedTerm
relation =
    arithmetic `nonassoc`
    choice
    [ try $ symbol "<=" >> return (operator Leq)
    ,       symbol "<"  >> return sugarLess
    ,       symbol "==" >> return sugarEquals
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
andTest = relation `chainl1` (symbol "&&" >> return sugarAnd)

orTest :: Parser UnannotatedTerm
orTest = andTest `chainl1` (symbol "||" >> return sugarOr)


expression :: Parser UnannotatedTerm
expression = choice
    [ ifClause
    , letClause
    , orTest
    , parens expression
    ]

program :: Parser UnannotatedTerm
program = expression

query :: Parser Outcome
query = choice
    [ number          >>= \m -> return m
    , keyword "True"  >>        return 1
    , keyword "False" >>        return 0
    ]
