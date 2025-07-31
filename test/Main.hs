module Main (main) where

import ComplexEvalTests
import EvalTests
import ParserTests
import Test.Hspec

main :: IO ()
main = hspec $ do
  evalTestsSpec
  parserTestsSpec
  complexEvalTestsSpec
