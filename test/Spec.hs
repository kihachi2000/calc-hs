import Lib
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT test1
  _ <- runTestTT test2
  _ <- runTestTT test3
  _ <- runTestTT test4
  _ <- runTestTT test5
  _ <- runTestTT test6
  putStrLn "complete"

test1 :: Test
test1 =
  TestList
    [ "digit \"0\"" ~: digit' "0" ~=? Just ('0', ""),
      "digit \"1\"" ~: digit' "1" ~=? Just ('1', ""),
      "digit \"2\"" ~: digit' "2" ~=? Just ('2', ""),
      "digit \"3\"" ~: digit' "3" ~=? Just ('3', ""),
      "digit \"4\"" ~: digit' "4" ~=? Just ('4', ""),
      "digit \"5\"" ~: digit' "5" ~=? Just ('5', ""),
      "digit \"6\"" ~: digit' "6" ~=? Just ('6', ""),
      "digit \"7\"" ~: digit' "7" ~=? Just ('7', ""),
      "digit \"8\"" ~: digit' "8" ~=? Just ('8', ""),
      "digit \"9\"" ~: digit' "9" ~=? Just ('9', ""),
      "add \"+\"" ~: add' "+" ~=? Just ('+', ""),
      "sub \"-\"" ~: sub' "-" ~=? Just ('-', ""),
      "mul \"*\"" ~: mul' "*" ~=? Just ('*', ""),
      "div \"/\"" ~: div' "/" ~=? Just ('/', ""),
      "lb \"(\"" ~: lb "(" ~=? Just ('(', ""),
      "rb \")\"" ~: rb ")" ~=? Just (')', "")
    ]

test2 :: Test
test2 =
  TestList
    [ "rep digit \"\"" ~: rep digit' "" ~=? Just ("", ""),
      "rep digit \"123\"" ~: rep digit' "123" ~=? Just ("123", ""),
      "rep digit \"123abc\"" ~: rep digit' "123abc" ~=? Just ("123", "abc")
    ]

test3 :: Test
test3 =
  TestList
    [ "or add sub \"abc\"" ~: or' add' sub' "abc" ~=? Nothing,
      "or add sub \"+abc\"" ~: or' add' sub' "+abc" ~=? Just ('+', "abc"),
      "or add sub \"-abc\"" ~: or' add' sub' "-abc" ~=? Just ('-', "abc")
    ]

test4 :: Test
test4 =
  TestList
    [ "cat2 add sub \"+-\"" ~: cat2 add' sub' "+-abc" ~=? Just (('+', '-'), "abc"),
      "cat3 add sub mul \"+-*\"" ~: cat3 add' sub' mul' "+-*abc" ~=? Just (('+', '-', '*'), "abc")
    ]

test5 :: Test
test5 =
  TestList
    [ "num \"123\"" ~: num "123abc" ~=? Just (123, "abc"),
      "num \"456\"" ~: num "456abc" ~=? Just (456, "abc"),
      "num \"789\"" ~: num "789abc" ~=? Just (789, "abc")
    ]

test6 :: Test
test6 =
    TestList
    [ "\"100\"" ~: calc "100" ~=? Just (100, ""),
      "\"1+1\"" ~: calc "1+1" ~=? Just (2, ""),
      "\"1-1\"" ~: calc "1-1" ~=? Just (0, ""),
      "\"2*3\"" ~: calc "2*3" ~=? Just (6, ""),
      "\"100/25\"" ~: calc "100/25" ~=? Just (4, "")
    ]
