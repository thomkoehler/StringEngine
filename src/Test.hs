----------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module Main(main) where

import Test.Framework

import Text.StringEngine.Preprocessor
import Text.StringEngine

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

----------------------------------------------------------------------------------------------------

main :: IO ()
main = htfMain htf_thisModulesTests


test_test0 :: IO ()
test_test0 = putStrLn test0
   where
      test0 = strEngine [Var "functions" ["foo0", "foo1"]] [str|
class Test
{
<for function in functions>
   void <function>();
<end>
};
|]


prop_SimpleStr :: Bool
prop_SimpleStr = preprocessor "<Hello World>" == "Hello World"

prop_SimpleStrLiteral :: Bool
prop_SimpleStrLiteral = preprocessor "Hello World" == "\"Hello World\""

prop_Str :: Bool
prop_Str = preprocessor "<Hello> World" == "Hello\" World\""

prop_Nl :: Bool
prop_Nl = preprocessor "a\nb" == "\"a\\nb\""

prop_Esc0 :: Bool
prop_Esc0 = preprocessor "\\<" == "\"<\""

prop_Esc1 :: Bool
prop_Esc1 = preprocessor "\\>" == "\">\""

prop_StrEng_Var :: Bool
prop_StrEng_Var = strEngine [Var "v1" "hallo"] "<v1>" == "hallo"

prop_StrEng_Str :: Bool
prop_StrEng_Str = strEngine [Var "v1" "hallo"] "v1" == "v1"

prop_StrEng_VarAndStr :: Bool
prop_StrEng_VarAndStr = strEngine [Var "v1" "Hello", Var "v2" "World"] "<v1> <v2>!" == "Hello World!"

prop_Foreach0 :: Bool
prop_Foreach0 = strEngine [Var "list" ["a", "b", "c"]] "<for c in list c end>" == "abc"

prop_Foreach1 :: Bool
prop_Foreach1 = strEngine [Var "list" ["a", "b", "c"]] "<for c in list>A<end>" == "AAA"

prop_Foreach2 :: Bool
prop_Foreach2 = strEngine [Var "list" ["a", "b", "c"]] "<for c in list>A<c end>" == "AaAbAc"

prop_Foreach3 :: Bool
prop_Foreach3 = strEngine [Var "list" ["a", "b", "c"]] "<for c in list>A<c>B<end>" == "AaBAbBAcB"

prop_IfTrue :: Bool
prop_IfTrue = strEngine [] "<if True>abc<end>" == "abc"

prop_IfFalse :: Bool
prop_IfFalse = strEngine [] "<if False>abc<end>" == ""


----------------------------------------------------------------------------------------------------


