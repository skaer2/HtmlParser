module Main where

import HtmlParser
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  putStrLn $ printTree $ showResult $ readP_to_S (tagParser) "<asdfa>ger<asdfa>gavno<div></asdfa>her</asdfa>"
