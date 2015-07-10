---------------------------------------------------------------------------------------------------

module Text.StringEngine.Parser(stringEngine) where

import Data.List(foldl')
import Data.Maybe(fromMaybe)

import Text.StringEngine.Lexer

---------------------------------------------------------------------------------------------------


data Expr
   = VarExpr String
   | StrLiteralExpr String

stringEngine :: String -> Expr
stringEngine input = foldl' step "" tokens
   where
      tokens = lexer input
      step prefix (StrLiteral str) = StrLiteralExpr str
      step prefix (Var name) = VarExpr name
      step _ token = error $ "Token " ++ show token ++ " is not supported yet."

---------------------------------------------------------------------------------------------------