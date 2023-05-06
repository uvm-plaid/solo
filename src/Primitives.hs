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

module Primitives where

import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy

import Sensitivity
import Privacy

--------------------------------------------------
-- Axioms about sensitivities
--------------------------------------------------

data Id a b where
  Id :: a ~ b => Id a b

eq_sym :: Id a b -> Id b a
eq_sym p = case p of
  Id -> Id

cong :: forall a b t. Id a b -> t a -> t b
cong p x = case p of
             Id -> x

cong1 :: forall a b t c. Id a b -> t a c -> t b c
cong1 p x = case p of
              Id -> x

cong2 :: forall a b t c. Id a b -> t c a -> t c b
cong2 p x = case p of
              Id -> x

-- axioms
scale_unit     :: forall s.        Id (ScaleSens s 1) s
maxnat_idemp   :: forall n.        Id (MaxNat n n) n
truncate_n_inf :: forall n s.      Id (TruncateSens n (TruncateInf s)) (TruncateSens n s)
scale_distrib  :: forall n s1 s2.  Id (ScaleSens (s1 +++ s2) n) (ScaleSens s1 n +++ ScaleSens s2 n)
trunc_distrib  :: forall n1 n2 s.  Id (TruncateSens (n1 TL.+ n2) s) (TruncateSens n1 s +++ TruncateSens n2 s)

scale_cong1    :: forall n s1 s2.  Id s1 s2 -> Id (ScaleSens s1 n) (ScaleSens s2 n)
scale_cong2    :: forall n1 n2 s.  Id n1 n2 -> Id (ScaleSens s n1) (ScaleSens s n2)

plus_cong      :: forall s1 s1' s2 s2'. Id s1 s1' -> Id s2 s2' -> Id (s1 +++ s2) (s1' +++ s2')

priv_idemp     :: forall n eps delta senv. Id (TruncatePriv eps delta (TruncateSens n senv))
                                              (TruncatePriv eps delta senv)

scale_unit = undefined
maxnat_idemp = undefined
truncate_n_inf = undefined
scale_distrib = undefined
trunc_distrib = undefined
scale_cong1 = undefined
scale_cong2 = undefined
plus_cong = undefined
priv_idemp = undefined

--------------------------------------------------
-- Primitives for Doubles
--------------------------------------------------

(<+>) :: SDouble m s1 -> SDouble m s2 -> SDouble m (s1 +++ s2)
x <+> y = D_UNSAFE $ unSDouble x + unSDouble y

(<->) :: SDouble m s1 -> SDouble m s2 -> SDouble m (s1 +++ s2)
x <-> y = D_UNSAFE $ unSDouble x - unSDouble y

sabs :: SDouble m s -> SDouble m s
sabs x = D_UNSAFE $ abs $ unSDouble x

-- TODO: can we do better than putting "Double" here?
sfilter_fn :: (forall s. Double -> Bool)
  -> (SDouble m) s1
  -> L1List (SDouble m) s2
  -> L1List (SDouble m) (ScaleSens s1 1 +++ ScaleSens s2 1)
sfilter_fn f x xs = undefined

-- Run a regular Haskell function and treat its output as infinitely sensitive
infsensL :: ([Double] -> [Double])
  -> SList cm (SDouble m) senv
  -> SList cm (SDouble m) (TruncateInf senv)
infsensL f xs = undefined

infsensD :: (Double -> Double)
  -> SDouble m senv
  -> SDouble m (TruncateInf senv)
infsensD f x = undefined

--------------------------------------------------
-- Primitives for Lists
--------------------------------------------------

source :: forall o m. Double -> SDouble Diff '[ '(o, NatSens 1) ]
source = D_UNSAFE

sReadFileL :: forall f t. L1List (SDouble Disc) '[ '(f, NatSens 1) ]
sReadFileL = undefined

sReadFile :: forall f t. t '[ '(f, NatSens 1) ]
sReadFile = undefined

sConstD :: forall s. Double -> SDouble Diff s
sConstD = D_UNSAFE

sConstL :: forall s m t. [t s] -> SList m t s
sConstL = SList_UNSAFE

mkL1ListDouble :: forall o m. [Double] -> SList L1 (SDouble m) '[ '(o, NatSens 1) ]
mkL1ListDouble xs = SList_UNSAFE $ map D_UNSAFE xs

emptySList :: SList m t '[]
emptySList = SList_UNSAFE []

scons :: t s1 -> SList m t s2 -> SList m t (s1 +++ s2)
scons x xs = undefined

sfoldr :: forall fn_sens1 fn_sens2 t1 t2 cm s3 s4 s5.
           (forall s1p s2p.
            t1 s1p -> t2 s2p -> t2 (ScaleSens s1p fn_sens1 +++ ScaleSens s2p fn_sens2))
        -> t2 s5
        -> SList cm t1 s4
        -> t2 (ScaleSens s4 (MaxNat fn_sens1 fn_sens2) +++ TruncateInf s5)
sfoldr f init xs = undefined

-- this could be defined using a truncation version of "fold"
stmap :: forall n s2 a b.
  (forall s1. a s1 -> b (TruncateSens n s1))
  -> L1List a s2
  -> L1List b (TruncateSens n s2)
stmap f as = undefined

clipDouble :: forall m senv. SDouble Disc senv -> SDouble Diff senv
clipDouble = undefined

clipL1 :: forall m senv.
  L1List (SDouble m) senv -> L1List (SDouble Diff) (TruncateSens 1 senv)
clipL1 xs = undefined

clipL2 :: forall m senv.
  L2List (SDouble m) senv -> L2List (SDouble Diff) (TruncateSens 1 senv)
clipL2 xs = undefined

szip :: SList m a s1 -> SList m b s2 -> SList m (L1Pair a b) (s1 +++ s2)
szip xs ys = undefined

p_elim :: forall fn_sens1 fn_sens2 s t1 t2 t3.
          (forall s1p s2p.
            t1 s1p -> t2 s2p -> t3 (ScaleSens s1p fn_sens1 +++ ScaleSens s2p fn_sens2))
       -> L1Pair t1 t2 s
       -> t3 (ScaleSens s (MaxNat fn_sens1 fn_sens2))
p_elim f p = undefined

sfst :: LInfPair t1 t2 s -> t1 s
sfst p = fst $ unSPair p

ssnd :: LInfPair t1 t2 s -> t2 s
ssnd p = snd $ unSPair p


--------------------------------------------------
-- Looping combinators
--------------------------------------------------

seqloop :: (TL.KnownNat k) => (Int -> a -> PM p a) -> a -> PM (ScalePriv p k) a
seqloop = undefined

advloop :: forall k delta_prime p a.
  (TL.KnownNat k) => (Int -> a -> PM p a) -> a -> PM (AdvComp k delta_prime p) a
advloop = undefined
