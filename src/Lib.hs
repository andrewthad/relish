{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

module Lib
  ( Builder(..)
  , Equirep(..)
  , Sized(..)
  , optimize
  , runLifted
  ) where

import GHC.Prim
import GHC.Types
import Data.ByteString (ByteString)

type IO# (a :: TYPE (k :: RuntimeRep)) = State# RealWorld -> (# a, State# RealWorld #)
type Maybe# (a :: TYPE (k :: RuntimeRep)) = (# a | (# #) #)
-- type Either# a b = (# a | b #)

data Builder (a :: TYPE k) where
  BuilderWord :: ( Word# -> Addr# -> IO# Int# ) -> Maybe# Int# -> Builder Word#
  BuilderInt :: ( Int# -> Addr# -> IO# Int# ) -> Maybe# Int# -> Builder Int#
  BuilderType :: ( a -> Addr# -> IO# Int# ) -> Maybe# Int# -> Builder a
  BuilderConstant :: {-# UNPACK #-} !ByteString -> Builder a


data Sized (a :: TYPE k) where
  SizedCoercion :: forall (a :: TYPE w) (b :: TYPE x). Either (Equirep b a) (Equirep a b) -> Sized b -> Sized a
  SizedWord :: ( Word# -> Addr# -> IO# Int# ) -> Sized Word#
  SizedConstant :: forall (a :: TYPE w). {-# UNPACK #-} !ByteString -> Sized a
  SizedWordHex :: Sized Word#
  SizedTuple2 :: forall (a :: TYPE w) (b :: TYPE x). Sized a -> Sized b -> Sized (# a, b #)
  SizedTuple3 :: forall (a :: TYPE w) (b :: TYPE x) (c :: TYPE y). Sized a -> Sized b -> Sized c -> Sized (# a, b, c #)
  SizedTuple4 :: Sized a -> Sized b -> Sized c -> Sized d -> Sized (# a, b, c, d #)

data Equirep (a :: TYPE k) (b :: TYPE j) :: Type where
  EquirepSelf :: forall (a :: TYPE w). Equirep a a
  EquirepTuple21 :: forall (a1 :: TYPE w1) (b1 :: TYPE x1) (c1 :: TYPE y1) (a2 :: TYPE w2) (b2 :: TYPE x2) (c2 :: TYPE y2).
    Equirep a1 a2 -> Equirep b1 b2 -> Equirep c1 c2 -> Equirep (# (# a1, b1 #), c1 #) (# a2, b2, c2 #)
  EquirepTuple22 :: forall (a1 :: TYPE w1) (b1 :: TYPE x1) (c1 :: TYPE y1) (a2 :: TYPE w2) (b2 :: TYPE x2) (c2 :: TYPE y2).
    Equirep a1 a2 -> Equirep b1 b2 -> Equirep c1 c2 -> Equirep (# a1, (# b1, c1 #) #) (# a2, b2, c2 #)

-- equirepCoerce :: Equirep a b -> a -> b

optimize :: forall (a :: TYPE k). Sized a -> Sized a
optimize = go where
  go :: forall (b :: TYPE j). Sized b -> Sized b
  go SizedWordHex = SizedWordHex
  go (SizedTuple2 (SizedTuple2 x y) z) = SizedCoercion
    (Right (EquirepTuple21 EquirepSelf EquirepSelf EquirepSelf))
    (SizedTuple3 x y z)
  go a = a

runLifted :: forall (a :: TYPE 'LiftedRep). Sized a -> a -> ByteString
runLifted (SizedConstant bs) _ = bs
runLifted (SizedCoercion _ r) a = runLifted (unsafeCoerce# r) (unsafeCoerce# a)

