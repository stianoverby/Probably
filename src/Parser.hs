module Parser(parseString, parseQuery) where

import Text.ParserCombinators.Parsec
import Control.Monad (void)

import Syntax
    ( Type(Num, Bool)
    , Term(Number, Boolean, Variable, Add, Leq, Conditional, Let)
    , Distribution(Uniform)
    )
import Probability(Outcome(NumericValue, TruthValue))

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
           , keyword "True"  >>        return (Boolean  True  ())
           , keyword "False" >>        return (Boolean  False ())
           , identifier      >>= \x -> return (Variable x     ())
           , parens expression
           ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operator :: Monoid a => (Term a -> Term a -> a -> Term a) -> Term a -> Term a -> Term a
operator op m n = op m n mempty

arithmetic :: Parser UnannotatedTerm
arithmetic =
    atom `chainl1`
    choice [ symbol "+" >> return (operator Add)]

nonassoc :: Parser a -> Parser (a -> a -> a) -> Parser a
nonassoc p po = p >>= \v1 -> option v1 (po >>= \f -> p >>= \v2 -> return $ f v1 v2)

relation :: Parser UnannotatedTerm
relation =
    arithmetic `nonassoc`
    choice [ symbol "<=" >> return (operator Leq)]

distribution :: Parser (Distribution Type)
distribution = keyword "uniform" >>
    choice
        [ keyword "Int"  >> Num <$> number <*> number >>= \tau -> return (Uniform tau)
        , keyword "Bool" >>                                       return (Uniform Bool)
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

expression :: Parser UnannotatedTerm
expression =
    choice
        [ ifClause
        , letClause
        , relation
        , parens expression
        ]

program :: Parser UnannotatedTerm
program = expression

query :: Parser Outcome
query =
    choice
        [ number          >>= \m -> return (NumericValue m    )
        , keyword "True"  >>        return (TruthValue   True )
        , keyword "False" >>        return (TruthValue   False)
        ]
