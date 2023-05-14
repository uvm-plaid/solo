{-# LANGUAGE
    AllowAmbiguousTypes
   ,DataKinds
   ,FlexibleContexts
   ,GADTs
   ,GeneralizedNewtypeDeriving
   ,KindSignatures
   ,PartialTypeSignatures
   ,PolyKinds
   ,RankNTypes
   ,ScopedTypeVariables
   ,TypeApplications
   ,TypeFamilies
   ,TypeOperators
   ,TypeSynonymInstances
   ,TypeFamilyDependencies
   ,UndecidableInstances
   ,RebindableSyntax
   ,EmptyCase
   #-}

module Sensitivity where

import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy
import Unsafe.Coerce

type Source = TL.Symbol                               -- sensitive data sources
data Sensitivity = InfSens | NatSens TL.Nat           -- sensitivity values
type SEnv = [(Source, Sensitivity)]                   -- sensitivity environments

data NMetric = Diff | Disc                            -- distance metrics for numeric types
newtype SDouble  (m :: NMetric) (s :: SEnv) = D_UNSAFE { unSDouble :: Double }

data CMetric = L1 | L2 | LInf                         -- metrics for compound types
newtype SPair (m :: CMetric) (f1 :: SEnv -> *) (f2 :: SEnv -> *) (s :: SEnv) = P_UNSAFE { unSPair :: (f1 s, f2 s) }
type L1Pair   = SPair L1                                   -- $⊗$-pairs in Fuzz
type L2Pair   = SPair L2                                   -- Not in Fuzz
type LInfPair = SPair LInf                                 -- $\&$-pairs in Fuzz

newtype SList (m :: CMetric) (f :: SEnv -> *) (s :: SEnv) = SList_UNSAFE { unSList :: [f s] }
type L1List   = SList L1                                   -- $τ␣‹list›$ in Fuzz
type L2List   = SList L2                                   -- Not in Fuzz
type LInfList = SList LInf                                 -- $τ␣‹alist›$ in Fuzz

type family IsLT (o :: Ordering) :: Bool where
  IsLT 'LT = 'True
  IsLT _ = 'False

type family IsEQ (o :: Ordering) :: Bool where
  IsEQ 'EQ = 'True
  IsEQ _ = 'False

type family Cond (b :: Bool) (x :: a) (y :: a) where
  Cond 'True x _ = x
  Cond 'False _ y = y

type family (+++) (s1 :: SEnv) (s2 :: SEnv) :: SEnv where
  '[]            +++ s2             = s2
  s1             +++ '[]            = s1
  ('(o,NatSens n1)':s1)  +++ ('(o,NatSens n2)':s2)  = '(o,NatSens (n1 TL.+ n2)) ': (s1 +++ s2)
  ('(o1,NatSens n1)':s1) +++ ('(o2,NatSens n2)':s2) =
    Cond (IsLT (TL.CmpSymbol o1 o2)) ('(o1,NatSens n1) ': (s1 +++ ('(o2,NatSens n2)':s2)))
                                     ('(o2,NatSens n2) ': (('(o1,NatSens n1)':s1) +++ s2))

type family ScaleSens (s :: SEnv) (n :: TL.Nat) :: SEnv where
  ScaleSens '[] _ = '[]
  ScaleSens ('(o, NatSens n2) ': s) n1 = '(o, NatSens (n1 TL.* n2)) ': ScaleSens s n1

type family MaxSens (s :: SEnv) :: TL.Nat where
  MaxSens '[] = 0
  MaxSens ('(_, NatSens n)':s) = MaxNat n (MaxSens s)

type family MaxNat (n1 :: TL.Nat) (n2 :: TL.Nat) :: TL.Nat where
  MaxNat n1 n2 = Cond (n1 TL.<=? n2) n2 n1

type family TruncateNat (n1 :: TL.Nat) (n2 :: TL.Nat) :: TL.Nat where
  TruncateNat _ 0 = 0
  TruncateNat n _ = n

type family TruncateSens (n :: TL.Nat) (s :: SEnv) :: SEnv where
  TruncateSens _ '[] = '[]
  TruncateSens n1 ('(o,NatSens n2) ': s) =
    '(o,NatSens (TruncateNat n1 n2)) ': TruncateSens n1 s

type family TruncateInf (s :: SEnv) :: SEnv where
  TruncateInf '[] = '[]
  TruncateInf ('(o,_) ': s) = '(o,InfSens) ': TruncateInf s


unsafeDropSens :: forall t s. t s -> t '[]
unsafeDropSens = unsafeCoerce

unsafeLiftSens :: forall t s. t '[] -> t s
unsafeLiftSens = unsafeCoerce


-- Unused
type family JoinSens (s1 :: SEnv) (s2 :: SEnv) :: SEnv where
  JoinSens '[] s2 = s2
  JoinSens s1 '[] = s1
  JoinSens ('(o,NatSens n1)':s1)  ('(o,NatSens n2)':s2) =
    '(o,NatSens (MaxNat n1 n2)) ': JoinSens s1 s2
  JoinSens ('(o1,NatSens n1)':s1) ('(o2,NatSens n2)':s2) =
    Cond (IsLT (TL.CmpSymbol o1 o2)) ('(o1,NatSens n1) ': JoinSens s1 ('(o2,NatSens n2)':s2))
                                     ('(o2,NatSens n2) ': JoinSens ('(o1,NatSens n1)':s1) s2)
