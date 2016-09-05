{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, QuasiQuotes, ScopedTypeVariables, FlexibleContexts,GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}

module SmallpdeVector where

import Prelude hiding (read)
import Data.List (intercalate)
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Generic.Mutable(new, read, write, unsafeRead, unsafeWrite, set)
import Control.Monad (forM, forM_)
import Control.Monad.Primitive (PrimState)
import Debug.Trace(traceShow)
import Numeric (showEFloat)

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4)

import VectorInst

when True  m = m
when False _ = return ()

{-# INLINE pureStencil' #-}
--pureStencil' :: Float -> Float -> Float -> Float -> Float -> Float -> Float
pureStencil' !d !here !east !north !west !south = 
  (1-4*d) * here + d*( north+east+west+south )

{-# INLINE solve #-}
solve :: Int ->  Int -> Int -> IO (VU.MVector (PrimState IO) Float)
solve !unroll !n !iterations = 
        do let steps :: Int
               !steps = iterations `div` 2
               d :: Float 
               !d  = dt/dx/dx 
               dt :: Float
               !dt = dx*dx/4/2
               dx :: Float
               !dx = 1/(n'*n'-1)
               n' :: Float
               !n' = fromInteger $ toInteger n
               !halfn = n `div` 2
               !a' = VU.create (do !a <- VGM.new (n*n); VGM.set a 0; return a)
               !b' = VU.create (do !b <- VGM.new (n*n); VGM.set b 0; return b)
           !a <- VU.thaw a'
           !b <- VU.thaw b'
           !() <- VGM.unsafeWrite a (n*halfn + halfn) 1.0 
           !() <- loop steps n d a b
           when (iterations `mod` 2 == 1) $ do
             !() <- onePass8 n d b a
             !() <- VGM.copy a b
             return ()
           return a
  where loop steps n d a b 
         | unroll == 1 = forM_ [0..steps-1] (\_ -> do
                                !() <- onePass1 n d b a
                                !() <- onePass1 n d a b
                                return ()
                             )
         | unroll == 2 = forM_ [0..steps-1] (\_ -> do
                                !() <- onePass2 n d b a
                                !() <- onePass2 n d a b
                                return ()
                             )
         | unroll == 4 = forM_ [0..steps-1] (\_ -> do
                                !() <- onePass4 n d b a
                                !() <- onePass4 n d a b
                                return ()
                             )
         | unroll == 8 = forM_ [0..steps-1] (\_ -> do
                                !() <- onePass8 n d b a
                                !() <- onePass8 n d a b
                                return ()
                             )
         | unroll == 16 = forM_ [0..steps-1] (\_ -> do
                                !() <- onePass16 n d b a
                                !() <- onePass16 n d a b
                                return ()
                             )
         | unroll == 32 = forM_ [0..steps-1] (\_ -> do
                                !() <- onePass32 n d b a
                                !() <- onePass32 n d a b
                                return ()
                             )
         | unroll == 64 = forM_ [0..steps-1] (\_ -> do
                                !() <- onePass64 n d b a
                                !() <- onePass64 n d a b
                                return ()
                             )
        {-# INLINE onePass1 #-}    
        onePass1 :: Int -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        onePass1 !n !d !a !b = $(concretePass 1 useEpilogues)
        {-# INLINE onePass2 #-}    
        onePass2 :: Int -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        onePass2 !n !d !a !b = $(concretePass 2 useEpilogues)
        {-# INLINE onePass4 #-}    
        onePass4 :: Int -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        onePass4 !n !d !a !b = $(concretePass 4 useEpilogues)
        {-# INLINE onePass8 #-}    
        onePass8 :: Int -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        onePass8 !n !d !a !b = $(concretePass 8 useEpilogues)
        {-# INLINE onePass16 #-}    
        onePass16 :: Int -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        onePass16 !n !d !a !b = $(concretePass 16 useEpilogues)
        {-# INLINE onePass32 #-}    
        onePass32 :: Int -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        onePass32 !n !d !a !b = $(concretePass 32 useEpilogues)
        {-# INLINE onePass64 #-}    
        onePass64 :: Int -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        onePass64 !n !d !a !b = $(concretePass 64 useEpilogues)

main :: Int -> Int -> Int -> IO ()
main !unroll !n' !steps = 
       do let n :: Int
              !n = n'  -- 256
          !a <- solve unroll n steps -- (5*1024)
          forM_ [0..n-1] (\(!i) -> do
             !liness <- forM [0..n-1] (\(!j) -> do
                          !num <- VGM.unsafeRead a (n*i+j) 
                          return $! showEFloat (Just 5) num " ")
             putStrLn $! concat liness)
