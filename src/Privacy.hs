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

module Privacy where

import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy

import Sensitivity
import Rats
import Reals

import System.Random
import qualified System.Random.MWC as MWC
import qualified Statistics.Distribution.Laplace as Lap
import Statistics.Distribution (ContGen(genContVar))

--------------------------------------------------
-- (epsilon, delta) privacy environments and operations
--------------------------------------------------

type EDEnv = [(TL.Symbol, TLReal, TLReal)]
type Zero = Lit (Rat_REDUCED 0 1)
type RNat n = Lit (Rat_REDUCED n 1)
type RLit n1 n2 = Lit (Rat_REDUCED n1 n2)

type family (++++) (s1 :: EDEnv) (s2 :: EDEnv) :: EDEnv where
 '[]            ++++ s2             = s2
 s1             ++++ '[]            = s1
 ('(o,e1,d1)':s1)  ++++ ('(o,e2,d2)':s2)  = '(o,Plus e1 e2,Plus d1 d2) ': (s1 ++++ s2)
 ('(o1,e1,d1)':s1) ++++ ('(o2,e2,d2)':s2) =
   Cond (IsLT (TL.CmpSymbol o1 o2)) ('(o1,e1,d1) ': (s1 ++++ ('(o2,e2,d2)':s2)))
                                    ('(o2,e2,d2) ': (('(o1,e1,d1)':s1) ++++ s2))

type family ScalePriv (penv :: EDEnv) (n :: TL.Nat) :: EDEnv where
  ScalePriv '[] _ = '[]
  ScalePriv ('(o, e1, e2) ': s) n =
    '(o, Times (Lit (Rat_REDUCED n 1)) e1, Times (Lit (Rat_REDUCED n 1)) e2) ': ScalePriv s n

type family TruncateTLReal (n1 :: TLReal) (n2 :: TL.Nat) :: TLReal where
  TruncateTLReal _ 0 = Lit (Rat_REDUCED 0 1)
  TruncateTLReal n _ = n

type family TruncatePriv (epsilon :: TLReal) (delta :: TLReal) (s :: SEnv) :: EDEnv where
  TruncatePriv _ _ '[] = '[]
  TruncatePriv epsilon delta ('(o, NatSens n2) ': s) =
    '(o, TruncateTLReal epsilon n2, TruncateTLReal delta n2) ': TruncatePriv epsilon delta s

type family AdvComp (k :: TL.Nat) (δ' :: TLReal) (penv :: EDEnv) :: EDEnv where
  AdvComp _ _ '[]                            = '[]
  AdvComp k d2 ('(o,e1,d1)':penv)  =
    '(o,
      Times (Times e1 (RNat 2)) (Root (Times (Times (RNat k) (RNat 2)) (Ln (Div (RNat 1) d2)))),
      Plus d2 (Times (RNat k) d1)) ': AdvComp k d2 penv

--------------------------------------------------
-- Privacy Monad
--------------------------------------------------

newtype PM (p :: EDEnv) a = PM_UNSAFE { unPM :: IO a }

return :: a -> PM '[] a
return x = PM_UNSAFE $ P.return x

(>>=) :: PM p1 a -> (a -> PM p2 b) -> PM (p1 ++++ p2) b
xM >>= f = PM_UNSAFE $ unPM xM P.>>= (unPM . f)


--------------------------------------------------
-- Laplace noise
--------------------------------------------------

laplace :: forall eps s. (TL.KnownNat (MaxSens s))
  => SDouble Diff s
  -> PM (TruncatePriv eps Zero s) Double
laplace x =
  let maxSens :: Double
      maxSens = fromInteger $ TL.natVal @(MaxSens s) Proxy
      eps :: Double
      eps = eps
  in
    undefined
    -- MWC.create P.>>= \gen ->
    -- (genContVar (Lap.laplace 0 (maxSens / eps)) gen) P.>>= \r ->
    -- P.return (r + unSDouble x)

laplaceL :: forall eps s. (TL.KnownNat (MaxSens s))
  => L1List (SDouble Diff) s
  -> PM (TruncatePriv eps Zero s) [Double]
laplaceL x = undefined

laplaceLN :: forall eps n s. (TL.KnownNat n)
  => L1List (SDouble Diff) (TruncateSens n s)
  -> PM (TruncatePriv eps Zero s) [Double]
laplaceLN x = undefined

gaussL :: forall eps delta n s. (TL.KnownNat (MaxSens s))
  => L2List (SDouble Diff) s
  -> PM (TruncatePriv eps delta s) [Double]
gaussL x = undefined

gaussLN :: forall eps delta n s. (TL.KnownNat n)
  => L2List (SDouble Diff) (TruncateSens n s)
  -> PM (TruncatePriv eps delta s) [Double]
gaussLN x = undefined

expMech :: forall eps s1 t1 t2.
  (forall s. t1 -> t2 s -> SDouble Diff s)
  -> [t1]
  -> t2 s1
  -> PM (TruncatePriv eps Zero s1) t1
expMech rs x f = undefined
