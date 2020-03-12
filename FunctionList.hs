{-# Language TypeFamilies,DataKinds,PolyKinds,TypeOperators,GADTs #-}

module FunctionList where

-- can do the same with a HList of kinds of the types in the type list
data FunctionList (ab :: Maybe (*,*)) (xs :: [*]) where
 FunctionListEmpty :: FunctionList Nothing '[]
 FunctionListCons :: (y -> x) -> FunctionList ab xs -> FunctionList (ConstructFunctionList1 y x ab xs) (ConstructFunctionList2 y x ab xs)

type family ConstructFunctionList1 (y :: *) (x :: *)  (ab :: Maybe (*,*)) (xs :: [*]) where
 ConstructFunctionList1 y x Nothing '[] = Just '(y, x)
 ConstructFunctionList1 y _ (Just '(_,b)) _ = Just '(y, b)

type family ConstructFunctionList2 (y :: *) (x :: *) (ab :: Maybe (*,*)) (xs :: [*]) where
 ConstructFunctionList2 y x Nothing '[] = '[]
 ConstructFunctionList2 y x (Just '(x,b)) xs = (x ': xs)

-- cast nested or unnested type to FunctionList
{-
-- *
-- todo
-- use To
type family ToFunctionList x where -- :: (FunctionList ab xs,b) where
 ToFunctionList ((f :: aa -> bb) x) = RepackFunctionList f (ToFunctionList x)

-- this is
-- map over fst
-- of the FunctionListCons

type family RepackFunctionList (f :: (y -> x)) (z :: (FunctionList ab xs,b)) :: 
              (FunctionList (ConstructFunctionList1 y x ab xs)
                            (ConstructFunctionList2 y x ab xs),b) where -- :: (FunctionList y (a ': xs) b,b) where
 RepackFunctionList f '(z,b) = '( 'FunctionListCons f z,b)
-}
{-
FunctionList.hs:24:39: error:
    * Expected kind `(FunctionList ab0 xs0, b0)',
        but `RepackFunctionList f (ToFunctionList x)' has kind `(FunctionList
                                                                   (ConstructFunctionList1
                                                                      bb bb ab0 xs0)
                                                                   (ConstructFunctionList2
                                                                      bb bb ab0 xs0),
                                                                 b0)'
    * In the type `RepackFunctionList f (ToFunctionList x)'
      In the type family declaration for `ToFunctionList'
   |
24 |  ToFunctionList ((f :: aa -> bb) x) = RepackFunctionList f (ToFunctionList x)
   |                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}

{-
type family RepackFunctionList (f :: y -> x) (z :: FunctionList ab xs) :: FunctionList y (a : xs) b where
  RepackFunctionList f z = 'FunctionListCons f z
-}
