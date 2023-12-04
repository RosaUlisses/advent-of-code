module Main where

import Data.Char (digitToInt, isDigit)
import GHC.Real (overflowError)

firstDigit :: String -> Int
firstDigit str = do
  let firstChar = head str
  if isDigit firstChar
    then digitToInt firstChar
    else firstDigit (tail str)

hasDigit :: String -> Bool
hasDigit str = do
  if null str
    then False
    else
      if isDigit firstChar
        then True
        else hasDigit (tail str)
  where
    firstChar = head str

lastDigit :: String -> Int
lastDigit str = do
  let firstChar = head str
  if isDigit firstChar && not (hasDigit (tail str))
    then digitToInt firstChar
    else lastDigit (tail str)

getNum :: String -> Int
getNum str = do
  let first = firstDigit str
  let last = lastDigit str
  first * 10 + last

readStrings :: IO [String]
readStrings = do
  line <- getLine
  if null line
    then return []
    else do
      rest <- readStrings
      return (line : rest)

solve :: [String] -> Int
solve strings = do
  if length strings == 0
    then 0
    else (getNum (head strings)) + solve ((tail strings))

main :: IO ()
main = do
  strings <- readStrings
  print (solve strings)
