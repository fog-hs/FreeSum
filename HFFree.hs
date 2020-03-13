{-# Language QuantifiedConstraints,MultiParamTypeClasses,FunctionalDependencies,UndecidableSuperClasses,TypeApplications,ConstraintKinds,AllowAmbiguousTypes,ScopedTypeVariables,TypeFamilyDependencies,FlexibleInstances,TypeOperators ,DataKinds , PolyKinds , TypeFamilies , TypeSynonymInstances , GADTs , UndecidableInstances#-}

module HFFree where

import GHC.Types (Symbol)
import To
import HFContainer
import HFList
import HFNonempty
import HFSum
import Data.Proxy

-- Free and Free'

data Free f a where
 Free :: (f (Free f a)) -> Free f a 
 Pure :: a -> Free f a 

data Free' f a where
 Free' :: (f (Free f a)) -> Free' f a
 Pure' :: f a -> Free' f a 

-- HFree wraps the result of unmapping the HFree constructor from the HFrees passed as an argument.
data HFree (k :: z) where
 HFree :: (fList :: (k -> *) -> container k -> *) HFree xs    
       -> HFree ((To (F_Container container k *) 
                     (H_Container container k) fList) xs)
-- takes an FContainer, which is a HList mapped over by HFree wrappers
-- and returns a HContainer, having discarded these wrappers,
-- defining a tree as the parameter, where the xs are the types 
-- of fully suturated applications of eg. a HList to its parameters.
-- this is abstracted as a container containing k, where k is the 
-- polymorphic type param to HFree, eg Lists of unballenced Nonempties and Lists in an adHoc tree.
-- ie is the nesting of lots of containers all returning as kind *.
-- HFree takes k, so it can be mapped over each of these.
 HPure :: (x :: *) -> HFree x

-- this example only has one type of container to use
egHFree :: HFree (HList '[String ,HList '[Int ,Bool]])
egHFree = HFree (HPure "hello" `FCons` ((HFree (HPure 0 `FCons` (HPure True `FCons` FEmpty)) `FCons` FEmpty)))

-- 

-- same thing now works with Nonempty
egHFree2   :: HFree (HNonempty (String ':| 'End (HNonempty (Integer ':| 'End Bool))))
egHFree2 = HFree (HPure "hello" `FCons' ` (FEnd ((HFree (HPure 0 `FCons' ` (FEnd (HPure True)))))))

-- now can write a mixed layer example
egHFree3   :: HFree (HNonempty (String ':| 'End (HList '[Int ,Bool])))
egHFree3 = HFree (HPure "hello" `FCons' ` (FEnd ((HFree (HPure 0 `FCons` ((HPure True) `FCons` FEmpty))))))

-- if we dont want to allow for this verticle hetrogeneity,
-- we can use;

data HFree' (container :: * -> *) (k :: z) where
 HFree' :: (fList :: (k -> *) -> container k -> *) (HFree' container) xs    
       -> HFree' container ((To (F_Container container k *) 
                            (H_Container container k) fList) xs)
 HPure' :: (x :: *) -> HFree' container x

type HTree = HFree' Nonempty

egHFree4 :: HTree (HNonempty ([Char] ':| 'End (HNonempty (Integer ':| 'End Bool))))
egHFree4 = HFree' (HPure' "hello" `FCons' ` (FEnd (HFree' (HPure' 0 `FCons' ` (FEnd (HPure' True))))))


class Make f where
 type Make3 f a b c = m | m -> a b c f
 make3 :: (a, (b,c)) -> HFree' f (Make3 f a b c)

instance Make Nonempty where
 type Make3 Nonempty a b c = HNonempty (a ':| 'End (HNonempty (b ':| 'End c)))
 make3 (a,(b,c))= HFree' (HPure' a `FCons' ` (FEnd (HFree' (HPure' b `FCons' ` (FEnd (HPure' c))))))

egHFree5 :: HTree (Make3 Nonempty String Int Bool)
egHFree5 = make3 ("hello",(0,True))

egHFree6 :: forall f. Make f => HFree' f (Make3 f String Int Bool)
egHFree6 = make3 @f ("hello",(0,True))

instance Make [] where
 type Make3 [] a b c = HList (a ': (HList (b ': c ': '[])) ': '[])
 make3 (a,(b,c))= HFree' (HPure' a `FCons` ((HFree' (HPure' b `FCons` (HPure' c `FCons` FEmpty)) `FCons` FEmpty)))

egHFree7 = egHFree6 @[]

-- dont make module DoubleList
-- just make HFVar

data DoubleList a = DoubleList [[a]]

data HDoubleList (xs :: DoubleList k) where
 HDoubleList :: FList HList xs -> HDoubleList ('DoubleList xs)

data FDoubleList (f :: k -> l) (xs :: DoubleList k) where -- = FDoubleList -- {runFDoubleList :: [[k]]}
 FDoubleList :: FList (FList f) xs -> FDoubleList f ('DoubleList xs)

type instance To Container (H_Container DoubleList k) DoubleList = HDoubleList 

type instance To Container (F_Container DoubleList k l) DoubleList = FDoubleList 

instance Make DoubleList where
 type Make3 DoubleList a b c = HDoubleList ('DoubleList ('[a] ': ((b ': c ': '[])) ': '[]))
 make3 (a,(b,c))= HFree' (FDoubleList (((HPure' a) `FCons` FEmpty) `FCons` (((HPure' b) `FCons` ((HPure' c) `FCons` FEmpty)) `FCons` FEmpty)))

egHFree8 = make3 @DoubleList ("hello",(0,True))

data Sum' (a :: *) where
 Sum' :: Sum (xs :: Sum_T *) -> Sum' (Sum xs)

type instance To Container (H_Container Sum_T k) Sum' = Sum 

type instance To Container (F_Container Sum_T k *) Sum' = FSum 


instance Make Sum_T where
 type Make3 Sum_T a b c = Sum ('Sum_T ("name" :: Symbol) '[ '("name" :: Symbol,a) , '("nb" :: Symbol,b) , '("nc" :: Symbol,c) ]) -- , '[ '[ '("na" :: Symbol,a)], '[ '("nb"::Symbol,b) , '("nc"::Symbol,c) ]])
 make3 (a::a,(b,c)) = HFree' $ FSum (Proxy :: Proxy "name") (HPure' a)

type SumTree = HFree' Sum_T

test10 :: test10
  :: HFree'
       Sum_T
       (Sum
          ('Sum_T
             "name" '[ '("name", [Char]), '("nb", Integer), '("nc", Bool)]))
test10 = make3 @Sum_T ("hello",(0,True))
