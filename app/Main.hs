module Main (main) where

import System.Environment
import Lib

main :: IO ()
main = do
    args <- getArgs
    let exp = head args
    let ans = getAns exp
    putStrLn $ exp ++ " = " ++ ans

getAns :: String -> String
getAns str = case calc str of
    Just (ans, _) -> show ans
    Nothing -> "failed to calc"
