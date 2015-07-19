
----------------------------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Text.StringEngine.Engine(strEngine, Var(..)) where

import qualified Data.Map as Map

import Text.StringEngine.Parser
import Text.StringEngine.Preprocessor
import Text.StringEngine.DynAny

import Data.Maybe(fromMaybe)
import Data.List(foldl')

----------------------------------------------------------------------------------------------------


data Bindings
   = ScopeBindings (Map.Map String DynAny)
   | ContextBindings Bindings Bindings


data Var = forall da. ToDynAny da => Var String da


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

evalExpr bindings (ExprForeach selectorName listName exprs) = foldl' step "" list
   where
      list = asList $ getBinding bindings listName

      step prefix var =
         let
            localBindings = ContextBindings bindings $ createBindings [Var selectorName var]
         in
            prefix ++ concatMap (evalExpr localBindings) exprs



{--

evalStrExpr :: Bindings -> StrExpr -> String
evalStrExpr _ (ExprStrLit str) = str

evalStrExpr bindings (ExprVar name) = asString $ getBinding bindings name


evalStrExpr bindings (ExprIf boolExpr strExprs) =
   if evalBoolExpr bindings boolExpr
      then concatMap (evalStrExpr bindings) strExprs
      else ""


evalBoolExpr :: Bindings -> BoolExpr -> Bool
evalBoolExpr _ (ExprBoolLit b) = b


--}

----------------------------------------------------------------------------------------------------
