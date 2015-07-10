----------------------------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.StringEngine.DynAny where


import qualified Data.Map as Map

----------------------------------------------------------------------------------------------------

data DynAny
   = DynString String
   | DynList [DynAny]
   | DynNothing


class ToDynAny a where
   toDynAny :: a -> DynAny


instance ToDynAny String where
   toDynAny str = DynString str


instance (ToDynAny d) => ToDynAny [d] where
   toDynAny = DynList . map toDynAny


instance (ToDynAny d) => ToDynAny (Maybe d) where
   toDynAny (Just d) = toDynAny d
   toDynAny Nothing = DynNothing


type Bindings = Map.Map String DynAny


----------------------------------------------------------------------------------------------------