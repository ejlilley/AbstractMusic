{-# LANGUAGE FlexibleInstances,
             GADTSyntax,
             GADTs,
             OverlappingInstances,
             StandaloneDeriving,
             UndecidableInstances #-}


module TypeSilliness where

-- data Z
-- data S n

-- data Nat = Z | S Nat

-- type One = S Z
-- -- type Two = S One

-- class Add a b c | a b -> c where
--   add :: a -> b -> c
  
-- instance              Add  Z    b  b
-- instance Add a b c => Add (S a) b (S c)

-- type Two = (Add (S Z) (S Z) a) => a
-- type Four = (Add (Two) (Two) a) => a

-- type Foo = S One

-- data Ratio = Ratio Nat Nat

-- deriving instance Show Ratio

data SomeType1 = SomeType1 deriving Show
data SomeType2 = SomeType2 deriving Show

class SomeClass d where

instance SomeClass SomeType1 where
instance SomeClass SomeType2 where

data WrapperType t where
  WrapperType :: (SomeClass t, Show t) => t -> (WrapperType t)

instance Show (WrapperType SomeType1) where
  show (WrapperType d) = "correct"

instance Show (WrapperType t) where
  show (WrapperType d) = "incorrect"

data ListWrap d where
  ListWrap :: [(WrapperType d)] -> ListWrap d

-- deriving instance Show ListWrap

deriving instance Show (WrapperType d) => Show (ListWrap d)

-- ConstraintKinds,
--             DataKinds,
--             EmptyDataDecls, 
--             ExistentialQuantification,
--             FlexibleContexts,
--             FlexibleInstances,
--             FunctionalDependencies,
--             GADTSyntax,
--             GADTs,
--             GeneralizedNewtypeDeriving,
--             InstanceSigs,
--             LiberalTypeSynonyms,
--             MultiParamTypeClasses, 
--             OverlappingInstances,
--             RankNTypes,
--             ScopedTypeVariables,
--             StandaloneDeriving,
--             TypeFamilies,
--             TypeOperators,
--             TypeSynonymInstances,
--             UndecidableInstances,
--             UnicodeSyntax #-}
