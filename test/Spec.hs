module Main where

import Test.HUnit
import System.Exit
import GatesTest

main :: IO ()
main = do
    putStrLn "Running Vectorial Quantum Computing Tests..."
    putStrLn "========================================"
    putStrLn "Testing quantum gates and circuits using the V monad"
    putStrLn ""
    
    -- Run all tests
    counts <- runTestTT gatesTests
    
    putStrLn ""
    putStrLn "========================================"
    putStrLn $ "Tests run: " ++ show (tried counts)
    putStrLn $ "Failures: " ++ show (failures counts)
    putStrLn $ "Errors: " ++ show (errors counts)
    
    -- Exit with appropriate code
    if failures counts + errors counts == 0
        then do
            putStrLn "All quantum computing tests passed! ✓"
            putStrLn "The V monad successfully executes quantum computations!"
            exitSuccess
        else do
            putStrLn "Some tests failed! ✗"
            exitFailure
