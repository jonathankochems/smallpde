{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, ScopedTypeVariables,TypeFamilies,MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module Data.Vector.Unboxed.SIMD.Internal where

import GHC.Prim
import GHC.Base (Int(..), Float(..))

import Data.Primitive.MachDeps

import Data.Primitive
import qualified Data.Vector.Primitive 
import qualified Data.Vector.Primitive.Mutable
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VUB
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as P
import qualified Data.Primitive.SIMD as SIMD

import Control.Monad(liftM,forM_)

import Unsafe.Coerce

data FloatX4 = FloatX4# FloatX4# 

unI# :: Int -> Int#
unI# (I# i#) = i#

mul4 :: Int# -> Int#
mul4 i# = unI# (I# i# * 4)

#define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim ty where {                                        \
  sizeOf# _ = unI# sz                                           \
; alignment# _ = unI# align                                     \
; indexByteArray# arr# i# = ctr (idx_arr arr# (mul4 i#))               \
; readByteArray#  arr# i# s# = case rd_arr arr# (mul4 i#) s# of        \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeByteArray# arr# i# (ctr x#) s# = wr_arr arr# (mul4 i#) x# s#    \
; {-setByteArray# arr# i# n# (ctr x#) s#                          \
    = case unsafeCoerce# (internal (set_arr arr# (unI# (I# i# * 4)) n# x#)) s# of \
            { (# s1#, _ #) -> s1# }                                 \
  -}                                                              \
; indexOffAddr# addr# i# = ctr (idx_addr addr# (mul4 i#))              \
; readOffAddr#  addr# i# s# = case rd_addr addr# (mul4 i#) s# of       \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# (mul4 i#) x# s#   \
; {-# INLINE sizeOf# #-}                                        \
; {-# INLINE alignment# #-}                                     \
; {-# INLINE indexByteArray# #-}                                \
; {-# INLINE readByteArray# #-}                                 \
; {-# INLINE writeByteArray# #-}                                \
; {-# INLINE indexOffAddr# #-}                                  \
; {-# INLINE readOffAddr# #-}                                   \
; {-# INLINE writeOffAddr# #-}                                  \
}

derivePrim(FloatX4, FloatX4#, (sIZEOF_FLOAT*4), (aLIGNMENT_FLOAT*4),\
           indexFloatArrayAsFloatX4#, readFloatArrayAsFloatX4#, writeFloatArrayAsFloatX4#, setFloatArray#,\
           indexFloatOffAddrAsFloatX4#, readFloatOffAddrAsFloatX4#, writeFloatOffAddrAsFloatX4#, \
           setFloatOffAddrAsFloatX4#)

#define primMVector(ty,con)                                             \
instance VGM.MVector VUM.MVector ty where {                                   \
  {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicOverlaps #-}                                          \
; {-# INLINE basicUnsafeNew #-}                                         \
; {- # INLINE basicUnsafeReplicate # -}                                   \
; {-# INLINE basicUnsafeRead #-}                                        \
; {-# INLINE basicUnsafeWrite #-}                                       \
; {-# INLINE basicClear #-}                                             \
; {- # INLINE basicSet # -}                                               \
; {-# INLINE basicUnsafeCopy #-}                                        \
; {-# INLINE basicUnsafeGrow #-}                                        \
; basicLength (con v) = VGM.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ VGM.basicUnsafeSlice i n v         \
; basicOverlaps (con v1) (con v2) = VGM.basicOverlaps v1 v2               \
; basicUnsafeNew n = con `liftM` VGM.basicUnsafeNew n                     \
; {-basicUnsafeReplicate n x = con `liftM` M.basicUnsafeReplicate n x -}    \
; basicUnsafeRead (con v) i = VGM.basicUnsafeRead v i                     \
; basicUnsafeWrite (con v) i x = VGM.basicUnsafeWrite v i x               \
; basicClear (con v) = VGM.basicClear v                                   \
; {-basicSet (con v) x = M.basicSet v x                                -}   \
; basicUnsafeCopy (con v1) (con v2) = VGM.basicUnsafeCopy v1 v2           \
; basicUnsafeMove (con v1) (con v2) = VGM.basicUnsafeMove v1 v2           \
; basicUnsafeGrow (con v) n = con `liftM` VGM.basicUnsafeGrow v n } 

#define primVector(ty,con,mcon)                                         \
instance VG.Vector VU.Vector ty where {                                     \
  {-# INLINE basicUnsafeFreeze #-}                                      \
; {-# INLINE basicUnsafeThaw #-}                                        \
; {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicUnsafeIndexM #-}                                      \
; {-# INLINE elemseq #-}                                                \
; basicUnsafeFreeze (mcon v) = con `liftM` VG.basicUnsafeFreeze v        \
; basicUnsafeThaw (con v) = mcon `liftM` VG.basicUnsafeThaw v            \
; basicLength (con v) = VG.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ VG.basicUnsafeSlice i n v         \
; basicUnsafeIndexM (con v) i = VG.basicUnsafeIndexM v i                 \
; basicUnsafeCopy (mcon mv) (con v) = VG.basicUnsafeCopy mv v            \
; elemseq _ = seq }

newtype instance VUM.MVector s (FloatX4) = MV_FloatX4 (P.MVector s (FloatX4))
newtype instance VU.Vector     (FloatX4) = V_FloatX4  (P.Vector    (FloatX4))
instance VU.Unbox (FloatX4)
primMVector((FloatX4), MV_FloatX4)
primVector((FloatX4), V_FloatX4, MV_FloatX4)


{-# INLINE coerceToInternalFloatX4 #-}
coerceToInternalFloatX4 :: SIMD.FloatX4 -> FloatX4
coerceToInternalFloatX4 !x = unsafeCoerce x

{-# INLINE coerceToFloatX4 #-}
coerceToFloatX4 :: FloatX4 -> SIMD.FloatX4
coerceToFloatX4 !x = unsafeCoerce x

{-# INLINE vectorizeUnboxedX4 #-}
vectorizeUnboxedX4 :: VU.Vector Float -> VU.Vector FloatX4
vectorizeUnboxedX4 (VUB.V_Float (v::Data.Vector.Primitive.Vector Float)) 
  | len `mod` 4 == 0 && off `mod` 4 == 0 = V_FloatX4 pv
  | otherwise = error "vectorizeUnboxedX4 vector wrong len/offset"
  where pv = Data.Vector.Primitive.Vector (len `div` 4) (off `div` 4) arr
        Data.Vector.Primitive.Vector len off arr = v

{-# INLINE vectorizeMUnboxedX4 #-}
vectorizeMUnboxedX4 :: VUM.MVector s Float -> VUM.MVector s FloatX4
vectorizeMUnboxedX4 (VUB.MV_Float (v :: Data.Vector.Primitive.Mutable.MVector s Float)) 
  | len `mod` 4 == 0 && off `mod` 4 == 0 = MV_FloatX4 pv
  | otherwise = error "vectorizeMUnboxedX4 vector wrong len/offset"
    where
        pv = Data.Vector.Primitive.Mutable.MVector (len `div` 4) (off `div` 4) arr
        Data.Vector.Primitive.Mutable.MVector len off arr = v


{-# INLINE unsafeVectorizeUnboxedX4 #-}
unsafeVectorizeUnboxedX4 :: VU.Vector Float -> VU.Vector FloatX4
unsafeVectorizeUnboxedX4 (VUB.V_Float ((!v)::Data.Vector.Primitive.Vector Float)) = V_FloatX4 pv
    where
        !pv = Data.Vector.Primitive.Vector len' off' arr
        !len' = len `div` 4
        !off' = off `div` 4
        Data.Vector.Primitive.Vector !len !off !arr = v

{-# INLINE unsafeVectorizeMUnboxedX4 #-}
unsafeVectorizeMUnboxedX4 :: VUM.MVector s Float -> VUM.MVector s FloatX4
unsafeVectorizeMUnboxedX4 (VUB.MV_Float ((!v) :: Data.Vector.Primitive.Mutable.MVector s Float)) = MV_FloatX4 pv
    where
        !pv   = Data.Vector.Primitive.Mutable.MVector len' off' arr
        !len' = len `div` 4
        !off' = off `div` 4
        Data.Vector.Primitive.Mutable.MVector !len !off !arr = v

{-# INLINE vectorisedRead #-}
vectorisedRead :: VUM.MVector RealWorld Float -> Int -> IO SIMD.FloatX4
vectorisedRead !v !i 
  | i `mod` 4 /= 0 = error $ "only reads to multiple of 4 possible (@ " ++ show i ++ ")"
  | otherwise      = do !x <- v' `VUM.read` i'
                        return $! coerceToFloatX4 x 
  where !v' = vectorizeMUnboxedX4 v
        !i' = i `div` 4 

{-# INLINE unsafeVectorisedRead #-}
unsafeVectorisedRead :: VUM.MVector RealWorld Float -> Int -> IO SIMD.FloatX4
unsafeVectorisedRead !v !i = do !x <- v' `VUM.unsafeRead` i'
                                return $! coerceToFloatX4 x 
  where !v' = unsafeVectorizeMUnboxedX4 v
        !i' = i `div` 4 

{-# INLINE vectorisedWrite #-}
vectorisedWrite :: VUM.MVector RealWorld Float -> Int -> SIMD.FloatX4 -> IO ()
vectorisedWrite v i a 
  | i `mod` 4 /= 0 = error $ "only writes to multiple of 4 possible (@ " ++ show i ++ ")"
  | otherwise      = VUM.write v' i' a' 
  where v' = vectorizeMUnboxedX4 v
        i' = i `div` 4 
        a' = coerceToInternalFloatX4 a

{-# INLINE unsafeVectorisedWrite #-}
unsafeVectorisedWrite :: VUM.MVector RealWorld Float -> Int -> SIMD.FloatX4 -> IO ()
unsafeVectorisedWrite !v !i !a = do !() <- VUM.unsafeWrite v' i' a' 
                                    return ()
  where !v' = unsafeVectorizeMUnboxedX4 v
        !i' = i `div` 4 
        !a' = coerceToInternalFloatX4 a

{-# INLINE shuffleDownVector #-}
shuffleDownVector :: SIMD.FloatX4 -> SIMD.FloatX4 -> SIMD.FloatX4
shuffleDownVector !v !v' = coerceToFloatX4 v''
  where !(FloatX4# v#)  = coerceToInternalFloatX4 v
        !(FloatX4# v'#) = coerceToInternalFloatX4 v'
        !(# v1#, v2#, v3#, v4# #)     = unpackFloatX4# v#
        !(# v'1#, v'2#, v'3#, v'4# #) = unpackFloatX4# v'#
        !v''# = packFloatX4# (# v2#, v3#, v4#, v'1# #)
        !v''  = FloatX4# v''#

{-# INLINE shuffleUpVector #-}
shuffleUpVector :: SIMD.FloatX4 -> SIMD.FloatX4 -> SIMD.FloatX4
shuffleUpVector !v !v' = coerceToFloatX4 v''
  where !(FloatX4# v#)  = coerceToInternalFloatX4 v
        !(FloatX4# v'#) = coerceToInternalFloatX4 v'
        !(# v1#, v2#, v3#, v4# #)     = unpackFloatX4# v#
        !(# v'1#, v'2#, v'3#, v'4# #) = unpackFloatX4# v'#
        !v''# = packFloatX4# (# v4#, v'1#, v'2#, v'3# #)
        !v''  = FloatX4# v''#


{-# INLINE shuffleDownFloat #-}
shuffleDownFloat :: SIMD.FloatX4 -> Float -> SIMD.FloatX4
shuffleDownFloat !v !f = coerceToFloatX4 v'
  where !(FloatX4# v#)  = coerceToInternalFloatX4 v
        !(# v1#, v2#, v3#, v4# #)     = unpackFloatX4# v#
        !(F# f#) = f
        !v'# = packFloatX4# (# v2#, v3#, v4#, f# #)
        !v'  = FloatX4# v'#

{-# INLINE shuffleUpFloat #-}
shuffleUpFloat :: Float -> SIMD.FloatX4 -> SIMD.FloatX4
shuffleUpFloat !f !v = coerceToFloatX4 v'
  where !(FloatX4# v#) = coerceToInternalFloatX4 v
        !(# v1#, v2#, v3#, v4# #) = unpackFloatX4# v#
        !(F# f#) = f
        !v'# = packFloatX4# (# f#, v1#, v2#, v3# #)
        !v'  = FloatX4# v'#


