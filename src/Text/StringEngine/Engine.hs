
----------------------------------------------------------------------------------------------------

module Text.StringEngine.Engine(strEngine) where

import qualified Data.Map as Map

import Text.StringEngine.Parser
import Text.StringEngine.Preprocessor
import Text.StringEngine.DynAny
import Text.StringEngine.Functions

import Data.Maybe(fromMaybe)
import Data.List(foldl')

----------------------------------------------------------------------------------------------------


data Bindings
   = ScopeBindings (Map.Map String DynAny)
   | ContextBindings Bindings Bindings


lookupScope :: Bindings -> String -> Maybe DynAny
lookupScope (ScopeBindings m) name = Map.lookup name m
lookupScope _ _ = Nothing


createBindings :: [Var] -> Bindings
createBindings xs = ScopeBindings $ Map.fromList $ map step xs
   where
      step (Var name ts) = (name, toDynAny ts)


getBinding :: Bindings -> String -> DynAny
getBinding (ScopeBindings s) name =
   fromMaybe (error ("Var " ++ name ++ " not found.")) $ Map.lookup name s


getBinding (ContextBindings c l) name =
   fromMaybe (getBinding c name) $ lookupScope l name


strEngine :: [Var] -> String -> String
strEngine vars input =
   let
      bindings = createBindings vars
      afterPP = preprocessor input
      exprs = parseStr afterPP
   in
      concatMap (asString . evalExpr bindings) exprs


evalExpr :: Bindings -> Expression -> DynAny
evalExpr _ (ExprLiteral da) = da

evalExpr bindings (ExprVar name) = getBinding bindings name

evalExpr bindings (ExprIf boolExpr exprs) =
   if asBool (evalExpr bindings boolExpr)
      then concatDynAny $ map (evalExpr bindings) exprs
      else emptyString

evalExpr bindings (ExprForeach selectorName listName exprs) = concatDynAny $ foldl' step [] list
   where
      list = asList $ getBinding bindings listName

      step :: [DynAny] -> DynAny -> [DynAny]
      step prefix var = prefix ++ map (evalExpr localBindings) exprs
         where
            localBindings = ContextBindings bindings $ createBindings [Var selectorName var]

evalExpr bindings (ExprFunCall funName exprs) = case exprs of
   [expr] -> (lookupFunction1 funName) (evalExpr bindings expr)
   _ -> error ("Function " ++ funName ++ " is unknown.")

----------------------------------------------------------------------------------------------------
