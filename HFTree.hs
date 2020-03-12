{-# Language QuantifiedConstraints,MultiParamTypeClasses,FunctionalDependencies,UndecidableSuperClasses,TypeApplications,ConstraintKinds,AllowAmbiguousTypes,ScopedTypeVariables,TypeFamilyDependencies,FlexibleInstances,TypeOperators ,DataKinds , PolyKinds , TypeFamilies , TypeSynonymInstances , GADTs , UndecidableInstances#-}

module HFTree where

import To
import HFContainer
import HFList
import HFNonempty

-- Tree

data Tree a = Branches [Tree a] | Leaf a

data HTree (t :: Tree *) where 
 HBranches :: FList HTree xs -> HTree (Branches xs)
 HLeaf     :: x -> HTree (Leaf x) 

-- TODO write F version
-- this is strange, because if the f is placed on the leaf
-- as with the FList version, is that it is by construction
-- an fmap implementation implicity. 

data FTree (f :: k -> *)(t :: Tree k) where 
 FBranches :: FList (FTree f) xs -> FTree f (Branches xs)
 FLeaf     :: f x -> FTree f (Leaf x) 
