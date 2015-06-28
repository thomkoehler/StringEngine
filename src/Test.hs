----------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main(main) where

import Test.Framework


import Text.StringEngine.Lexer

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

----------------------------------------------------------------------------------------------------

main :: IO()
main = do
   htfMain htf_thisModulesTests


prop_hello = lexer "hello" == [SimpleStr "hello"]

----------------------------------------------------------------------------------------------------


