import Lib
import Test.HUnit

main :: IO Counts
main = do
  runTestTT test1

parseA :: Parser Char
parseA = pChar 'a'

test1 :: Test
test1 =
  TestList
    [ "parseA \"\"" ~: parseA "" ~=? Nothing,
      "parseA \"a\"" ~: parseA "a" ~=? Just ('a', ""),
      "parseA \"abc\"" ~: parseA "abc" ~=? Just ('a', "bc"),
      "parseA \"x\"" ~: parseA "x" ~=? Nothing,
      "parseA \"xyz\"" ~: parseA "xyz" ~=? Nothing
    ]
