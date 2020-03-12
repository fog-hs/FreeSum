{-# Language UndecidableInstances,AllowAmbiguousTypes,ScopedTypeVariables,RankNTypes,TypeApplications,PolyKinds,DataKinds,TypeFamilies,GADTs,TypeOperators #-}

module HFSum where

import Data.Proxy
import Data.Kind
import GHC.Types (Symbol)
import HFContainer
import To

----
-- Nat

type Nat = [()]

type Z = '[]

type Succ (n :: Nat) = '() ': n

----
-- Type Prelude

type family Lookup (i :: a) (xs :: [ (a,b)]) :: b where
 Lookup i ( '(i,b) ':xs) = b
 Lookup i ( '(a,b) ':xs) = Lookup i xs
-- Lookup _ '[] = error

----
-- Sum

data Sum_T k = Sum_T Symbol [(Symbol,k)]

data Sum (xs :: Sum_T *) where 
 Sum :: Proxy (name :: Symbol) -> Lookup name nts -> Sum ('Sum_T name nts)

toSum :: forall name nts. Lookup name nts -> Sum ('Sum_T name nts)
toSum = Sum Proxy

fromSum :: 
  forall name (name' :: Symbol) nts.
  Sum ('Sum_T name nts) -> Lookup name nts
fromSum (Sum _ x) = x

----
-- F machinery

-- strips the f off the input args type and stores it as xs
data FSum (f :: k -> *) (xs :: Sum_T *) where
 FSum :: Proxy (name :: Symbol) -> f (Lookup name nts) -> FSum f ('Sum_T name nts)

type instance To Container (H_Container Sum_T *) Sum_T = Sum 

type instance To Container (F_Container Sum_T * *) Sum_T = FSum 

