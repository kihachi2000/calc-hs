module Lib
  ( digit',
    add',
    sub',
    mul',
    div',
    lb,
    rb,
    rep,
    map',
    cat2,
    cat3,
    or',
    num,
    calc,
  )
where

import           Data.Char
import           GHC.Base  (minInt)

type Parser a = String -> Maybe (a, String)

is :: (Char -> Bool) -> Parser Char
is f (x : xs) = if f x then Just (x, xs) else Nothing
is _ []       = Nothing

digit' :: Parser Char
digit' = is isDigit

add' :: Parser Char
add' = is ('+' ==)

sub' :: Parser Char
sub' = is ('-' ==)

mul' :: Parser Char
mul' = is ('*' ==)

div' :: Parser Char
div' = is ('/' ==)

lb :: Parser Char
lb = is ('(' ==)

rb :: Parser Char
rb = is (')' ==)

map' :: (a -> b) -> Parser a -> Parser b
map' f p str = case p str of
  Just (x, rest) -> Just (f x, rest)
  Nothing        -> Nothing

rep :: Parser a -> Parser [a]
rep p str = case p str of
  Just (x, rest) -> appendFst x <$> rep p rest
  Nothing        -> Just ([], str)

appendFst :: a -> ([a], String) -> ([a], String)
appendFst x (y1, y2) = (x : y1, y2)

cat2 :: Parser a -> Parser b -> Parser (a, b)
cat2 p1 p2 str = do
  (x1, rest1) <- p1 str
  (x2, rest2) <- p2 rest1
  return ((x1, x2), rest2)

cat3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
cat3 p1 p2 p3 str = do
  (x1, rest1) <- p1 str
  (x2, rest2) <- p2 rest1
  (x3, rest3) <- p3 rest2
  return ((x1, x2, x3), rest3)

or' :: Parser a -> Parser a -> Parser a
or' p1 p2 str = case (p1 str, p2 str) of
  (Just (x, rest), _)       -> Just (x, rest)
  (Nothing, Just (x, rest)) -> Just (x, rest)
  (Nothing, Nothing)        -> Nothing

calc :: Parser Int
calc = expr

expr :: Parser Int
expr = map' convert $ cat2 term $ rep $ cat2 (or' add' sub') term
  where
    convert (f1, fs) = case fs of
      (('+', f2) : fs') -> convert (f1 + f2, fs')
      (('-', f2) : fs') -> convert (f1 - f2, fs')
      (_ : _)           -> minInt
      []                -> f1

term :: Parser Int
term = map' convert $ cat2 fact $ rep $ cat2 (or' mul' div') fact
  where
    convert (f1, fs) = case fs of
      (('*', f2) : fs') -> convert (f1 * f2, fs')
      (('/', f2) : fs') -> convert (f1 `div` f2, fs')
      (_ : _)           -> minInt
      []                -> f1

fact :: Parser Int
fact = or' num $ map' convert $ cat3 lb expr rb
  where
    convert (_, x, _) = x

num :: Parser Int
num = map' convert (rep digit')
  where
    convert str = read str :: Int
