{-# Language QuantifiedConstraints,MultiParamTypeClasses,FunctionalDependencies,UndecidableSuperClasses,TypeApplications,ConstraintKinds,AllowAmbiguousTypes,ScopedTypeVariables,TypeFamilyDependencies,FlexibleInstances,TypeOperators ,DataKinds , PolyKinds , TypeFamilies , TypeSynonymInstances , GADTs , UndecidableInstances#-}

module HFList where

import HFContainer
import To

-- List

data HList (xs :: [k]) where
 HEmpty :: HList '[]
 HCons  :: x -> HList xs -> HList (x ': xs)

data FList (f :: k -> l) (xs :: [k]) where
 FEmpty :: FList f '[]
 FCons  :: f x -> FList f xs -> FList f (x ': xs)

type instance To Container (H_Container [] k) [] = HList 

type instance To Container (F_Container [] k l) [] = FList 

