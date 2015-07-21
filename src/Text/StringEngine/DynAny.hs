----------------------------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, ExistentialQuantification #-}

module Text.StringEngine.DynAny
(
   DynAny(..),
   ToDynAny(..),
   Var(..),
   dynTrue,
   dynFalse,
   emptyString,
   concatDynAny,
   asBool,
   asString,
   asList
)
where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B

----------------------------------------------------------------------------------------------------

data Var = forall da. ToDynAny da => Var String da

data DynAny
   = DynString String
   | DynBool Bool
   | DynList [DynAny]
   | DynNothing
   deriving Eq


dynTrue :: DynAny
dynTrue = DynBool True

dynFalse :: DynAny
dynFalse = DynBool False


emptyString :: DynAny
emptyString = DynString ""


class ToDynAny a where
   toDynAny :: a -> DynAny


instance ToDynAny DynAny where
   toDynAny = id


instance ToDynAny B.ByteString where
   toDynAny = DynString . C.unpack


instance ToDynAny String where
   toDynAny = DynString


instance ToDynAny Bool where
   toDynAny True = dynTrue
   toDynAny False = dynFalse


instance (ToDynAny d) => ToDynAny [d] where
   toDynAny = DynList . map toDynAny


instance (ToDynAny d) => ToDynAny (Maybe d) where
   toDynAny (Just d) = toDynAny d
   toDynAny Nothing = DynNothing


appendDynAny :: DynAny -> DynAny -> DynAny
appendDynAny (DynString s0) (DynString s1) = (DynString (s0 ++ s1))
appendDynAny _ _ = error "String expected."


concatDynAny :: [DynAny]-> DynAny
concatDynAny [] = DynString ""
concatDynAny [da] = da
concatDynAny ds = foldl1 appendDynAny ds


asBool :: DynAny -> Bool
asBool (DynBool b) = b
asBool _ = error "Bool expected."


asString :: DynAny -> String
asString (DynString str) = str
asString _ = error "String expected."


asList :: DynAny -> [DynAny]
asList da = case da of
   DynList ds -> ds
   _ -> error "List expected."

----------------------------------------------------------------------------------------------------
