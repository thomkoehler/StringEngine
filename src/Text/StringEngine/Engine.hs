
----------------------------------------------------------------------------------------------------

module Text.StringEngine.Engine(strEngine) where

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


lookupScope :: Bindings -> String -> Maybe DynAny
lookupScope (ScopeBindings m) name = Map.lookup name m
lookupScope _ _ = Nothing


createBindings :: ToString ts => [(String, ts)] -> Bindings
createBindings xs = ScopeBindings $ Map.fromList $ map step xs
   where
      step (name, ts) = (name, DynString (toString ts))


getBinding :: Bindings -> String -> DynAny
getBinding (ScopeBindings s) name = case Map.lookup name s of
   Just da -> da
   Nothing -> error $ "Var " ++ name ++ " not found."

getBinding (ContextBindings c l) name = case lookupScope l name of
   Just da -> da
   Nothing -> getBinding c name


strEngine :: ToString ts => [(String, ts)] -> String -> String
strEngine vars input =
   let
      bindings = createBindings vars
      afterPP = preprocessor input
      exprs = parseStr afterPP
   in
      concat $ map (exprToString bindings) exprs


exprToString :: Bindings -> StrExpr -> String
exprToString _ (StrLit str) = str
exprToString bindings (Var name) = toString $ getBinding bindings name


----------------------------------------------------------------------------------------------------