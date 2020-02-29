module Main where

import HtmlParser
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  print $ readP_to_S (tagParser) "<asdfa attributik=xgavnox >ger<asdfa>gavno<div></asdfa>her</asdfa>"
  putStrLn ""
  putStrLn $ printTree $ showResult $ readP_to_S (tagParser) "<asdfa attributik=xgavnox >ger<asdfa>gavno<div></asdfa>her</asdfa>"
  putStrLn $ "\"gavno\""
