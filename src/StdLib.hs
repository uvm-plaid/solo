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

module StdLib where

import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy

import SensitivitySafe
import PrivacySafe
import Primitives


summationFunction :: L1List (SDouble Disc) senv -> SDouble Diff (TruncateSens 1 senv)
summationFunction xs = sum (clipL1 xs)

sumFn :: forall s1 s2. SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (ScaleSens s1 1 +++ ScaleSens s2 1)
sumFn x y = cong (eq_sym scale_unit) x <+> cong (eq_sym scale_unit) y

sum :: forall s. L1List (SDouble Diff) s -> SDouble Diff s
sum xs = cong scale_unit $ sfoldr @1 @1 sumFn (sConstD @'[] 0) xs

clipList :: L1List (SDouble Disc) s -> L1List (SDouble Diff) s
clipList xs = cong scale_unit $ smap @1 (cong (eq_sym scale_unit) . clipDouble) xs

smap :: forall fn_sens a b s2 m.
  (forall s1. a s1 -> b (ScaleSens s1 fn_sens))
  -> SList m a s2
  -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))
smap f = sfoldr @fn_sens @1 (\x xs -> scons (f x) (cong (eq_sym scale_unit) xs)) (sConstL @'[] [])

sfilter :: (forall s. Double -> Bool)
  -> L1List (SDouble m) s1
  -> L1List (SDouble m) s1
sfilter f xs = cong scale_unit $ sfoldr @1 @1 (sfilter_fn f) (sConstL @'[] []) xs

count :: SList cm t s -> SDouble Diff s
count xs = cong scale_unit $ sfoldr @1 @1 count_fn (sConstD @'[] 0) xs
  where count_fn :: forall (s1 :: SEnv) s2 t.
          t s1 -> SDouble Diff s2 -> SDouble Diff (ScaleSens s1 1 +++ ScaleSens s2 1)
        count_fn x a = cong (eq_sym scale_unit) (sConstD @s1 1) <+> cong (eq_sym scale_unit) a

pairPlus :: L1Pair (SDouble Diff) (SDouble Diff) s -> SDouble Diff (ScaleSens s 1)
pairPlus = p_elim @1 @1 (\x y -> cong (eq_sym scale_unit) x <+> cong (eq_sym scale_unit) y)

sListSum :: forall s1 s2 m. SList m (SDouble Diff) s1 -> SList m (SDouble Diff) s2
  -> SList m (SDouble Diff) (ScaleSens s1 1 +++ ScaleSens s2 1)
sListSum xs ys = cong (scale_distrib @1 @s1 @s2) $ smap @1 pairPlus $ szip xs ys

sListSum1s :: forall s1 s2 m. SList m (SDouble Diff) s1 -> SList m (SDouble Diff) s2
  -> SList m (SDouble Diff) (s1 +++ s2)
sListSum1s xs ys = cong scale_unit $ smap @1 pairPlus $ szip xs ys

sfoldr1s :: forall t1 t2 cm s3 s4 s5.
           (forall s1 s2. t1 s1 -> t2 s2 -> t2 (s1 +++ s2))
        -> t2 s5 -> SList cm t1 s4 -> t2 (s4 +++ TruncateInf s5)
sfoldr1s f init xs = cong prf $ sfoldr @1 @1 f' init xs
  where f' :: t1 s1 -> t2 s2 -> t2 (ScaleSens s1 1 +++ ScaleSens s2 1)
        f' a b = f (cong (eq_sym scale_unit) a) (cong (eq_sym scale_unit) b)
        prf = plus_cong @(ScaleSens s4 1) @s4 @(TruncateInf s5) scale_unit Id

