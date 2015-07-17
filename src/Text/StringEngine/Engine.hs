
----------------------------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Text.StringEngine.Engine(strEngine, Var(..)) where

import qualified Data.Map as Map

import Text.StringEngine.Parser
import Text.StringEngine.Preprocessor
import Text.StringEngine.DynAny
import Text.StringEngine.ToString

----------------------------------------------------------------------------------------------------


data Bindings
   = ScopeBindings
      {
         scope :: Map.Map String DynAny
      }

   | ContextBindings
      {
         context :: Bindings,
         locals :: Bindings
      }


data Var = forall da. ToDynAny da => Var String da


lookupScope :: Bindings -> String -> Maybe DynAny
lookupScope (ScopeBindings m) name = Map.lookup name m
lookupScope _ _ = Nothing


createBindings :: [Var] -> Bindings
createBindings xs = ScopeBindings $ Map.fromList $ map step xs
   where
      step (Var name ts) = (name, toDynAny ts)


getBinding :: Bindings -> String -> DynAny
getBinding (ScopeBindings s) name =  case Map.lookup name s of
   Just da -> da
   Nothing -> error $ "Var " ++ name ++ " not found."

getBinding (ContextBindings c l) name = case lookupScope l name of
   Just da -> da
   Nothing -> getBinding c name


strEngine :: [Var] -> String -> String
strEngine vars input =
   let
      bindings = createBindings vars
      afterPP = preprocessor input
      exprs = parseStr afterPP
   in
      concatMap (exprToString bindings) exprs


exprToString :: Bindings -> StrExpr -> String
exprToString _ (ExprStrLit str) = str

exprToString bindings (ExprVar name) = toString $ getBinding bindings name

exprToString bindings (ExprForeach selectorName listName exprs) =
   let
      list = toList $ getBinding bindings listName
   in
      undefined


----------------------------------------------------------------------------------------------------
