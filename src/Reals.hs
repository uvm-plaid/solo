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

module Reals where

import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy

import SensitivitySafe
import Rats

---------------
-- REALS --
---------------

data TLReal = Lit Rat | TLReal :+: TLReal | TLReal :*: TLReal | Ln TLReal | Root TLReal
  | TLReal :/: TLReal
  | TLReal :-: TLReal
  | Tr TLReal
  | TLReal :\/: TLReal | TLReal :/\: TLReal

type family Plus (e1 :: TLReal) (e2 :: TLReal) :: TLReal where
  Plus (Lit q1) (Lit q2) = Lit (RatPlus q1 q2)
  Plus e1 e2 = e1 :+: e2

type family Times (e1 :: TLReal) (e2 :: TLReal) :: TLReal where
  Times (Lit q1) (Lit q2) = Lit (RatTimes q1 q2)
  Times e1 e2 = e1 :*: e2

type family Div (e1 :: TLReal) (e2 :: TLReal) :: TLReal where
  Div (Lit q1) (Lit q2) = Lit (RatDiv q1 q2)
  Div e1 e2 = e1 :/: e2

type family Minus (e1 :: TLReal) (e2 :: TLReal) :: TLReal where
  Minus (Lit q1) (Lit q2) = Lit (RatMinus q1 q2)
  Minus e1 e2 = e1 :-: e2

type family Tr (e :: TLReal) :: TLReal where
  Tr e = Tr e

type family Ln (e :: TLReal) :: TLReal where
  Ln e = Ln e

type family Root (e :: TLReal) :: TLReal where
  Root e = Root e

-- need literal ratmax/ratmin for base
type family Join (e1 :: TLReal) (e2 :: TLReal) :: TLReal where
  Join e1 e2 = e1 :\/: e2

type family Meet (e1 :: TLReal) (e2 :: TLReal) :: TLReal where
  Meet e1 e2 = e1 :/\: e2
