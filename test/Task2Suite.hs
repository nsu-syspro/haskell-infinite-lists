module Task2Suite where

import TestUtils
import Test.Tasty
import Test.Tasty.HUnit

import Task2 (fibs, nats, primes)

import Control.Monad

task2Tests :: TestTree
task2Tests = testGroup "Task2"
  [ localOption (mkTimeout (seconds 5)) $ testCase "take 1000 nats" $
      forM_ (takeS 1000 $ zipS indices (zipS nats natsRefS)) $ \(idx, (actual, expected)) -> do
        assertEqual ("nats #" ++ show idx) expected actual

  , localOption (mkTimeout (seconds 5)) $ testCase "take 1000 fibs" $
      forM_ (takeS 1000 $ zipS indices (zipS fibs fibsRefS)) $ \(idx, (actual, expected)) -> do
        assertEqual ("fibs #" ++ show idx) expected actual

  , localOption (mkTimeout (seconds 5)) $ testCase "take 1000 primes" $
      forM_ (takeS 1000 $ zipS indices primes) $ \(idx, actual) -> do
        assertBool ("primes #" ++ show idx ++ " = " ++ show actual ++ " is not prime") $ isPrime actual
  ]

