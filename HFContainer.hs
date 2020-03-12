{-# Language QuantifiedConstraints,MultiParamTypeClasses,FunctionalDependencies,UndecidableSuperClasses,TypeApplications,ConstraintKinds,AllowAmbiguousTypes,ScopedTypeVariables,TypeFamilyDependencies,FlexibleInstances,TypeOperators ,DataKinds , PolyKinds , TypeFamilies , TypeSynonymInstances , GADTs , UndecidableInstances#-}

module HFContainer where
import To
import FunctionList

----
-- Container, HContainer, FContainer

type Container = * -> *

type H_Container container k = container k -> *

type F_Container (container :: Container) k l = (k -> l) -> container k -> *

--type FN_Container (container :: Container) k a xs = FunctionList a xs k -> container k -> *

-- casting H & F _Container to Container

type instance To (H_Container container k) Container hContainer = container

type instance To (F_Container container k l) Container fContainer = container

-- casting between H & F _Container's

type instance To (H_Container container k) (F_Container container k l) hcontainer 
 = To Container (F_Container container k l) (To (H_Container container k) Container hcontainer)

type instance To (F_Container container k l) (H_Container container k) fcontainer 
 = To Container (H_Container container k) (To (F_Container container k l) Container fcontainer)


