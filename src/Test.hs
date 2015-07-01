----------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module Main(main) where

import Test.Framework

import Text.StringEngine.Lexer
import Text.StringEngine

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   htfMain htf_thisModulesTests


prop_SimpleStr :: Bool
prop_SimpleStr = lexer "hello" == [SimpleStr "hello"]

prop_Statements :: Bool
prop_Statements = lexer "<hello>" == [Var "hello"]

prop_Esc0 :: Bool
prop_Esc0 = lexer "\\<" == [SimpleStr "<"]

prop_Esc1 :: Bool
prop_Esc1 = lexer "\\>" == [SimpleStr ">"]

prop_Esc2 :: Bool
prop_Esc2 = lexer "\\\\" == [SimpleStr "\\"]

prop_Esc3 :: Bool
prop_Esc3 = lexer "\\<hello\\>" == [SimpleStr "<hello>"]

prop_lexer0 :: Bool
prop_lexer0 = lexer "hello<hello>" == [SimpleStr "hello", Var "hello"]

prop_lexer1 :: Bool
prop_lexer1 = lexer "<hello>hello" == [Var "hello", SimpleStr "hello"]

prop_parser :: Bool
prop_parser = stringEngine lookupFun "Hello <lisa> and <peter>!" == "Hello Lisa and Peter!"
   where
      vars = [("lisa", "Lisa"), ("peter", "Peter")]

      lookupFun :: String -> Maybe String
      lookupFun v = lookup v vars


prop_se0 :: Bool
prop_se0 = [se|Hello World|] == "Hello World"


prop_se1 :: Bool
prop_se1 = [se|Hello <w>|] == "Hello World"
   where
      w = "World"

----------------------------------------------------------------------------------------------------


