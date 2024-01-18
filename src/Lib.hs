module Lib
  ( Parser,
    digit',
    add',
    sub',
    mul',
    div',
    lb,
    rb,
  )
where

import Data.Char

type Parser a = String -> Maybe (a, String)

is :: (Char -> Bool) -> Parser Char
is f (x : xs) = if f x then Just (x, xs) else Nothing
is _ [] = Nothing

digit' :: Parser Char
digit' = is isDigit

add' :: Parser Char
add' = is (== '+')

sub' :: Parser Char
sub' = is (== '-')

mul' :: Parser Char
mul' = is (== '*')

div' :: Parser Char
div' = is (== '+')

lb :: Parser Char
lb = is (== '(')

rb :: Parser Char
rb = is (== ')')

rep :: Parser a -> Parser [a]
rep p xs = case p xs of
    Just(x', xs') -> rep p xs'
    Nothing -> 
rep _ [] = Nothing
