module Tests where

import Regex
    ( Regex(..), fout, problematic )

import VirtualMachine
    ( Instr(..), VMState(VMState), VMOutput(..), run, match )

import Test.HUnit
    ( assertBool, assertEqual, Test(..) )

-- Some regex examples

ex1 :: Regex -- ab*
ex1 = Concat Lambda (Concat (Const 'a') (Kleene (Const 'b')))

ex2 :: Regex -- a | b*
ex2 = Sum (Const 'a') (Kleene (Sum Lambda (Const 'b')))

ex3 :: Regex -- aa*bb*
ex3 = Concat (Const 'a') (Concat (Kleene (Const 'a')) (Concat (Const 'b') (Kleene (Const 'b'))))

-- Some VM Examples

exVM1 :: VMState 
exVM1 = VMState 0 "aab" [IChar 'a', ISplit 0 2, IChar 'b', ISplit 2 4, IMatch]

-- Regex Tests

test1 :: Test
test1 = TestCase (assertBool "λab* must be non problematic" ((not . problematic) ex1))

test2 :: Test
test2 = TestCase (assertBool "a | (λ | b)* must be problematic" (problematic ex2))

test3 :: Test 
test3 = TestCase (assertEqual "fout should change λab*" ex1 (fout ex1))

test4 :: Test 
test4 = TestCase (assertBool "fout should fix a | (λ | b)*" (((not . problematic) . fout) ex2))

regexTests :: Test
regexTests = TestList [test1, test2, test3, test4]

-- VM Tests

test5 :: Test 
test5 = TestCase (assertEqual "aab must be accepted by exVM1" Success (run exVM1))

test6 :: Test 
test6 = TestCase (assertBool "aab must be accepted by ex3" (match "aab" ex3))

test7 :: Test 
test7 = TestCase (assertBool "aaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbb must be accepted by ex3" (match "aaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbb" ex3))

test8 :: Test 
test8 = TestCase (assertBool "b should not be accepted by ex3" (not $ match "b" ex3))

test9 :: Test 
test9 = TestCase (assertBool "abab should not be accepted by exVM1" (not $ match "abab" ex3))

test10 :: Test
test10 = TestCase (assertBool "aaaa should not match empty language" (not $ match "aaa" Empty))

test11 :: Test 
test11 = TestCase (assertBool "empty string should not match empty language" (not $ match "" Empty))

test12 :: Test
test12 = TestCase (assertBool "empty should match lambda regex" (match "" Lambda))

vmTests :: Test
vmTests = TestList [test5, test6, test7, test8, test9, test10, test11, test12]

-- Gotta group 'em all

tests :: Test 
tests = TestList [TestLabel "Regex tests" regexTests, TestLabel "VM Tests" vmTests]

-- Performance test

bigWord :: String
bigWord = replicate 1500000 'a' ++ replicate 3000000 'b'