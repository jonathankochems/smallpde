{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, QuasiQuotes, ScopedTypeVariables, FlexibleContexts #-}
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

import VectorInst

{-# INLINE fourIter #-} 
fourIter !n !d !a !b !i !j = $(stencilBody 4)
    where {-# INLINE pureStencil #-} 
          pureStencil = pureStencil' d 

{-# INLINE eightIter #-} 
eightIter !n !d !a !b !i !j = $(stencilBody 8)
    where {-# INLINE pureStencil #-} 
          pureStencil = pureStencil' d 

{-# INLINE sixtIter #-} 
sixtIter !n !d !a !b !i !j = $(stencilBody 16)
    where {-# INLINE pureStencil #-} 
          pureStencil = pureStencil' d 


tester :: IO ()
tester = do b <- VU.thaw $ VU.fromList [if x == 12 then 1 else 0 | x <- [0..25]]
            a <- VU.thaw $ VU.fromList [ 0 | x <- [0..25] ]
            vectorStencil a b 5 1 1
            vectorStencil a b 5 2 1
            vectorStencil a b 5 3 1
            vectorStencil b a 5 1 1
            vectorStencil b a 5 2 1
            vectorStencil b a 5 3 1
            vectorStencil a b 5 1 1
            vectorStencil a b 5 2 1
            vectorStencil a b 5 3 1
            forM_ [0..5-1] $! \(!i) -> do
             !liness <- forM [0..5-1] $! \(!j) -> do
                          !num <- VGM.unsafeRead a (5*i+j) 
                          return $! showEFloat(Just 5) num " " 
             putStrLn $ concat liness
  where vectorStencil a b n i j = $(stencilBody 3)
        d     = 0.0625
        pureStencil = pureStencil' d 



when True  m = m
when False _ = return ()

{-# INLINE pureStencil' #-}
pureStencil' :: Float -> Float -> Float -> Float -> Float -> Float -> Float
pureStencil' !d !here !east !north !west !south = 
  (1-4*d) * here + d*( north + east + west + south )

{-# INLINE solve #-}
solve :: Int -> Int -> IO (VU.MVector (PrimState IO) Float)
solve !n !iterations = 
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
           !() <- forM_ [0..steps-1] $! \_ -> do
                    !() <- onePass n d b a
                    !() <- onePass n d a b
                    return ()
           when (iterations `mod` 2 == 1) $ do
             !() <- onePass n d b a
             !() <- VGM.copy a b
             return ()
           return a
  where {-# INLINE onePass #-}
        onePass !n !d !a !b = go 1 1
           where go !i !j | i == n-1 = return ()
                          | j >= n-16 && j == n-1 = go (i+1) 1
                          | j >= n-16 && j == n-2 = do !() <- oneIter n d a b i j
                                                       go (i+1) 1
                          | j >= n-16 && j == n-3 = do !() <- twoIter n d a b i j
                                                       go (i+1) 1
                          | j >= n-16 && j == n-4 = do !() <- twoIter n d a b i j
                                                       !() <- oneIter n d a b i (j+2)
                                                       go (i+1) 1 
                          | j >= n-16 && j == n-5 = do !() <- fourIter n d a b i j
                                                       go (i+1) 1
                          | j >= n-16 && j == n-6 = do !() <- fourIter n d a b i j
                                                       !() <- oneIter n d a b i (j+4)
                                                       go (i+1) 1
                          | j >= n-16 && j == n-7 = do !() <- fourIter n d a b i j
                                                       !() <- twoIter n d a b i (j+4)
                                                       go (i+1) 1
                          | j >= n-16 && j == n-8 = do !() <- fourIter n d a b i j
                                                       !() <- twoIter n d a b i (j+4)
                                                       !() <- oneIter n d a b i (j+6)
                                                       go (i+1) 1
                          | j >= n-16 && j == n-9 = do !() <- eightIter n d a b i j
                                                       go (i+1) 1
                          | j >= n-16 && j == n-10 = do !() <- eightIter n d a b i j
                                                        !() <- oneIter n d a b i (j+8)
                                                        go (i+1) 1
                          | j >= n-16 && j == n-11 = do !() <- eightIter n d a b i j
                                                        !() <- twoIter n d a b i (j+8)
                                                        go (i+1) 1
                          | j >= n-16 && j == n-12 = do !() <- eightIter n d a b i j
                                                        !() <- twoIter n d a b i (j+8)
                                                        !() <- oneIter n d a b i (j+10)
                                                        go (i+1) 1 
                          | j >= n-16 && j == n-13 = do !() <- eightIter n d a b i j
                                                        !() <- fourIter n d a b i (j+8)
                                                        go (i+1) 1
                          | j >= n-16 && j == n-14 = do !() <- eightIter n d a b i j
                                                        !() <- fourIter n d a b i (j+8)
                                                        !() <- oneIter n d a b i (j+12)
                                                        go (i+1) 1
                          | j >= n-16 && j == n-15 = do !() <- eightIter n d a b i j
                                                        !() <- fourIter n d a b i (j+8)
                                                        !() <- twoIter n d a b i (j+12)
                                                        go (i+1) 1
                          | j >= n-16 && j == n-16 = do !() <- eightIter n d a b i j
                                                        !() <- fourIter n d a b i (j+8)
                                                        !() <- twoIter n d a b i (j+12)
                                                        !() <- oneIter n d a b i (j+14)
                                                        go (i+1) 1
                          | otherwise            = do !() <- sixtIter n d a b i j
                                                      go i (j+16)
                          {-| otherwise            = do !() <- fourIter n d a b i j
                                                      go i (j+4)-}
        {-# INLINE oneIter #-} 
        oneIter !n !d !a !b !i !j = do 
                  !north <- VGM.unsafeRead b (n*(i+1)+j)
                  !east  <- VGM.unsafeRead b (n*i+j-1)
                  !here  <- VGM.unsafeRead b (n*i+j)
                  !west  <- VGM.unsafeRead b (n*i+j+1)
                  !south <- VGM.unsafeRead b (n*(i-1)+j)
                  !()    <- VGM.unsafeWrite a (n*i+j) $! (1-4*d) * here  
                                                             + d*( north
                                                                + east
                                                                + west
                                                                + south
                                                               )
                  return()
        {-# INLINE twoIter #-} 
        twoIter !n !d !a !b !i !j = do 
                  !north00 <- VGM.unsafeRead b (n*(i+1)+j)
                  !north01 <- VGM.unsafeRead b (n*(i+1)+j+1)
                  let !north  = north00
                      !north' = north01
                  !east  <- VGM.unsafeRead b (n*i+j-1)
                  !here  <- VGM.unsafeRead b (n*i+j)
                  !west  <- VGM.unsafeRead b (n*i+j+1)
                  !west' <- VGM.unsafeRead b (n*i+j+2)                  
                  !south <- VGM.unsafeRead b (n*(i-1)+j)
                  !south'<- VGM.unsafeRead b (n*(i-1)+j+1)
                  !()    <- VGM.unsafeWrite a (n*i+j) $! (1-4*d) * here  
                                                             + d*( north
                                                                + east
                                                                + west
                                                                + south
                                                               )
                  let !east' = here
                      !here' = west
                  !()    <- VGM.unsafeWrite a (n*i+j+1) $! (1-4*d) * here'  
                                                             + d*( north'
                                                                   + east'
                                                                   + west'
                                                                   + south'
                                                                 )
                  return ()

main = do let n :: Int
              !n = 256
          !a <- solve n (5*1024)
          forM_ [0..n-1] $! \(!i) -> do
             !liness <- forM [0..n-1] $! \(!j) -> do
                          !num <- VGM.unsafeRead a (n*i+j) 
                          return $! showEFloat (Just 5) num " "
             putStrLn $! concat liness
