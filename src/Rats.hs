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

module Rats where

import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy

import SensitivitySafe

---------------
-- RATIONALS --
---------------

data Rat = Rat_REDUCED TL.Nat TL.Nat

-- euclid's algorithm
-- assumes n1 <= n2
type family GCD_DRIVER (n1 :: TL.Nat) (n2 :: TL.Nat) :: TL.Nat where
  GCD_DRIVER 0 n = n
  GCD_DRIVER m n = GCD_DRIVER (TL.Mod n m) m

type family GCD (n1 :: TL.Nat) (n2 :: TL.Nat) :: TL.Nat where
  GCD n1 n2 = Cond (n1 TL.<=? n2) (GCD_DRIVER n1 n2) (GCD_DRIVER n2 n1)

type family MkRat_HELPER (n1 :: TL.Nat) (n2 :: TL.Nat) (gcd :: TL.Nat) :: Rat where
  MkRat_HELPER n1 n2 gcd = Rat_REDUCED (TL.Div n1 gcd) (TL.Div n2 gcd)

type family MkRat (n1 :: TL.Nat) (n2 :: TL.Nat) :: Rat where
  MkRat n1 n2 = MkRat_HELPER n1 n2 (GCD n1 n2)

type family RatPlus (r1 :: Rat) (r2 :: Rat) :: Rat where
  RatPlus (Rat_REDUCED n1 d1) (Rat_REDUCED n2 d2) =
    MkRat ((n1 TL.* d2) TL.+ (n2 TL.* d1)) (d1 TL.* d2)

type family RatMinus (r1 :: Rat) (r2 :: Rat) :: Rat where
  RatMinus (Rat_REDUCED n1 d1) (Rat_REDUCED n2 d2) =
    MkRat ((n1 TL.* d2) TL.- (n2 TL.* d1)) (d1 TL.* d2)

type family RatTimes (r1 :: Rat) (r2 :: Rat) :: Rat where
  RatTimes (Rat_REDUCED n1 d1) (Rat_REDUCED n2 d2) =
    MkRat (n1 TL.* n2) (d1 TL.* d2)

type family RatDiv (r1 :: Rat) (r2 :: Rat) :: Rat where
  RatDiv (Rat_REDUCED n1 d1) (Rat_REDUCED n2 d2) =
    MkRat (d1 TL.* d2) (n1 TL.* n2)

