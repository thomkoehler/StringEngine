----------------------------------------------------------------------------------------------------

module Text.StringEngine.Parser
(
   Expression(..),
   parseStr
)
where

import Text.Parsec
import Control.Monad.Identity

import Text.StringEngine.DynAny
import Text.StringEngine.Lexer

----------------------------------------------------------------------------------------------------

data Expression
   = ExprLiteral DynAny
   | ExprVar String
   | ExprForeach String String [Expression]
   | ExprIf Expression [Expression]
   | ExprFunCall String [Expression]
   deriving Eq


expression :: Parser Expression
expression = choice
   [
      strLit,
      boolLit,
      exprIf,
      foreach,
      varOrFunCall
   ]


strLit :: Parser Expression
strLit = liftM (ExprLiteral . DynString) stringLiteral


varOrFunCall :: Parser Expression
varOrFunCall = do
   name <- identifier
   params <- option Nothing $ do
      _params <- parens $ commaSep expression
      return $ Just _params
   case params of
      Nothing -> return $ ExprVar name
      Just ps -> return $ ExprFunCall name ps


foreach :: Parser Expression
foreach = do
   reserved "for"
   sel <- identifier
   reserved "in"
   list <- identifier
   exprs <- many1 expression
   reserved "end"
   return $ ExprForeach sel list exprs


boolLit :: Parser Expression
boolLit = choice
   [
      reserved "True" >> return (ExprLiteral dynTrue),
      reserved "False" >> return (ExprLiteral dynFalse)
   ]


exprIf :: Parser Expression
exprIf = do
   reserved "if"
   b <- expression
   exprs <- many1 expression
   reserved "end"
   return $ ExprIf b exprs


parseStr :: String -> [Expression]
parseStr str = case parse (many1 expression) "" str of
   Left err -> error $ show err
   Right xs -> xs

----------------------------------------------------------------------------------------------------
