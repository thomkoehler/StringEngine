----------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main(main) where

import Test.Framework

import Text.StringEngine.Preprocessor
import Text.StringEngine.Parser
import Text.StringEngine.Engine

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

----------------------------------------------------------------------------------------------------

main :: IO ()
main = htfMain htf_thisModulesTests

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

prop_Parser :: Bool
prop_Parser = parseStr "\"Hello World\"" == [ExprStrLit "Hello World"]

prop_StrEng_Var :: Bool
prop_StrEng_Var = strEngine [Var "v1" "hallo"] "<v1>" == "hallo"

prop_StrEng_Str :: Bool
prop_StrEng_Str = strEngine [Var "v1" "hallo"] "v1" == "v1"

prop_StrEng_VarAndStr :: Bool
prop_StrEng_VarAndStr = strEngine [Var "v1" "Hello", Var "v2" "World"] "<v1> <v2>!" == "Hello World!"

----------------------------------------------------------------------------------------------------


