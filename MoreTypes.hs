{-# LANGUAGE FlexibleInstances,
             GADTSyntax,
             GADTs,
             RankNTypes,
             UndecidableInstances,
             OverlappingInstances,
             DeriveDataTypeable,
             StandaloneDeriving #-}
 
import Data.Data
 
data SomeType1 = SomeType1 deriving (Show, Typeable)
data SomeType2 = SomeType2 deriving (Show, Typeable)
 
class SomeClass d where
 
instance SomeClass SomeType1 where
instance SomeClass SomeType2 where
 
data WrapperType t where
  WrapperType :: (SomeClass t, Show t) => t -> (WrapperType t)
 
deriving instance Typeable1 WrapperType
 
instance Show (WrapperType SomeType1) where
  show (WrapperType d) = "correct"
 
instance Show (WrapperType t) where
  show (WrapperType d) = "incorrect"
 
data ListWrap where
  ListWrap :: Typeable d => [(WrapperType d)] -> ListWrap
  
