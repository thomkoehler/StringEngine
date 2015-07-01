---------------------------------------------------------------------------------------------------

module Text.StringEngine.Parser(stringEngine) where

import Data.List(foldl')
import Data.Maybe(fromMaybe)

import Text.StringEngine.Lexer

---------------------------------------------------------------------------------------------------

stringEngine :: (String -> Maybe String) -> String -> String
stringEngine lookupFun input = foldl' step "" tokens
   where
      tokens = lexer input
      step prefix (SimpleStr str) = prefix ++ str
      step prefix (Var name) = prefix ++ fromMaybe (error ("Var '" ++ name ++ "' is unknown.")) (lookupFun name)
      step _ token = error $ "Token " ++ show token ++ " is not supported yet."

---------------------------------------------------------------------------------------------------