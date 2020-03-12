{-# Language QuantifiedConstraints,MultiParamTypeClasses,FunctionalDependencies,UndecidableSuperClasses,TypeApplications,ConstraintKinds,AllowAmbiguousTypes,ScopedTypeVariables,TypeFamilyDependencies,FlexibleInstances,TypeOperators ,DataKinds , PolyKinds , TypeFamilies , TypeSynonymInstances , GADTs , UndecidableInstances#-}

module HFNonempty where

import HFContainer
import To

data Nonempty a = End a | a :| Nonempty a

data HNonempty (xs :: Nonempty k) where
 HEnd :: x -> HNonempty (End x)
 HCons'  :: x -> HNonempty xs -> HNonempty (x ':| xs)

data FNonempty (f :: k -> l) (xs :: nonempty k) where
 FEnd   :: f x -> FNonempty f (End x)
 FCons'  :: f x -> FNonempty f xs -> FNonempty f (x ':| xs)

type instance To Container (H_Container Nonempty k) Nonempty = HNonempty 

type instance To Container (F_Container Nonempty k l) Nonempty = FNonempty 

