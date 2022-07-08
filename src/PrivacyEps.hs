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

module PrivacyEps where

import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy

import Sensitivity

--------------------------------------------------
-- Laplace noise
--------------------------------------------------

type EpsEnv = SEnv

newtype PM (p :: EpsEnv) a = PM_UNSAFE { unPM :: IO a }

return :: a -> PM '[] a
return x = PM_UNSAFE $ P.return x

(>>=) :: PM p1 a -> (a -> PM p2 b) -> PM (p1 +++ p2) b
xM >>= f = PM_UNSAFE $ unPM xM P.>>= (unPM . f)

laplace :: forall eps s. (TL.KnownNat (MaxSens s), TL.KnownNat eps)
  => SDouble Diff s
  -> PM (TruncateSens eps s) Double
laplace x = undefined
 -- PM_UNSAFE $ do
 --  let maxSens :: Integer
 --      maxSens = TL.natVal @(MaxSens s) Proxy
 --      eps :: Integer
 --      eps = TL.natVal @eps Proxy
 --  (laplaceNoise $ fromIntegral maxSens / fromIntegral eps) P.>>= (\y -> P.return $ unSDouble x + y)

laplaceL :: forall eps s. (TL.KnownNat (MaxSens s), TL.KnownNat eps)
  => L1List (SDouble Diff) s
  -> PM (TruncateSens eps s) [Double]
laplaceL x = undefined

laplaceLN :: forall eps n s. (TL.KnownNat n, TL.KnownNat eps)
  => L1List (SDouble Diff) (TruncateSens n s)
  -> PM (TruncateSens eps s) [Double]
laplaceLN x = undefined

expMech :: forall eps s1 t1 t2. (TL.KnownNat eps)
  => (forall s. t1 -> t2 s -> SDouble Diff s)
  -> [t1]
  -> t2 s1
  -> PM (TruncateSens eps s1) t1
expMech rs x f = undefined
