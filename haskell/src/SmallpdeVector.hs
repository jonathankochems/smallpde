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
import Data.Vector.Unboxed.SIMD as VUS

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4)

import VectorInst

--{-# INLINE fourIter #-} 
--fourIter !n !d !a !b !i !j  = $(stencil 4) pureStencil' n d a b i j 
--
--{-# INLINE eightIter #-} 
--eightIter !n !d !a !b !i !j = $(stencil 8) pureStencil' n d a b i j 

--{-# INLINE sixtIter #-} 
--sixtIter !n !d !a !b !i !j = $(stencil 16) pureStencil' n d a b i j 

when True  m = m
when False _ = return ()

{-# INLINE pureStencil' #-}
--pureStencil' :: Float -> Float -> Float -> Float -> Float -> Float -> Float
pureStencil' !d !here !east !north !west !south = 
  (1-4*d) * here + d*( north+east+west+south )
    --where v :: FloatX4 
          --v = packVector (north,east,west,south)
          --s :: Float
          --s = sumVector $ v + v 

{-
    The Transformation:
    ====================
    The original grid:

      |                                |
    --------------------------------------
      |                                | 
      |                                |
    - |- - - - - - - - - - - - - - - - |-
      |                                |
      |                                |
    - |- - - - - - - - - - - - - - - - |-
      |                                |
      |                                |
    - |- - - - - - - - - - - - - - - - |-
      |                                |
      |                                |
    --------------------------------------
      |                                |
                                       
    The transformed grid:
         |                                                                                                                                 |
    --------------------------------------------------------------------------------------------------------------------------------------------
         |                                                                                                                                 |
         |                                                                                                                                 |
    --------------------------------------------------------------------------------------------------------------------------------------------
         |                                                                                                                                 |

    Within the boundary
    (i,j) -> (i% (n/4),4*j+i/(n/4))
    
-}

indexTransform !n !i !j  = (width*(i-sector*slice))+4*j+sector
    where !slice = (n+3) `div` 4
          !width = 4*n
          !sector = i `div` slice

indexTransformReverse width !i !j
       = n*i' + j'
    where !n             = width `div` 4
          !(!j',!sector) = j `divMod` 4
          !i'            = i+sector*n  

{-# INLINE solve #-}
solve :: Int -> Int -> IO (VU.MVector (PrimState IO) Float)
solve !n !iterations = 
        do let steps :: Int
               !steps = iterations `div` 2
               d' :: Float 
               !d'  = dt/dx/dx
               d :: FloatX4 
               !d  = packVector (d',d',d',d')

               dt :: Float
               !dt = dx*dx/4/2
               dx :: Float
               !dx = 1/(n'*n'-1)
               n' :: Float
               !n' = fromInteger $ toInteger n
               !halfn = n `div` 2
               !nCeil = 4*((n+3) `div` 4)
               !a' = VU.create (do !a <- VGM.new (nCeil*nCeil); VGM.set a 0; return a)
               !b' = VU.create (do !b <- VGM.new (nCeil*nCeil); VGM.set b 0; return b)
           !a <- VU.thaw a'
           !b <- VU.thaw b'
           !() <- VGM.unsafeWrite a (indexTransform n halfn halfn) 1.0 
           !() <- forM_ [0..steps-1] (\_ -> do
                    !() <- onePass n d d' b a
                    --printArray n b
                    !() <- onePass n d d' a b
                    --printArray n a
                    return ()
                 )
           when (iterations `mod` 2 == 1) $ do
             !() <- onePass n d d' b a
             --printArray n b
             !() <- VGM.copy a b
             return ()
           --printArray n a
           --printArrayRaw n a
           return a
  where {-# INLINE onePass #-}
        onePass :: Int -> FloatX4 -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        onePass !n !d !d' !a !b = go'' 0 1
           where go !i !j | 4*(i+1) >= n-2 =  -- traceShow (i,j,indexTransform n i j) $ 
                                            go' i 1
                          | j >= 4*n-4  =  -- traceShow (i,j,indexTransform n i j) $ 
                                            go (i+1) 4
                          | otherwise = do -- traceShow (i,j,indexTransform n i j) $ return ()
                                           !() <- oneIter n d a b i j
                                           go i (j+4)
                 go' !i !j | 4*i >= n-2 =  -- traceShow (i,j,indexTransform n i j) $ 
                                           return ()
                           | j >= n-1  =  -- traceShow (i,j,indexTransform n i j) $ 
                                            go' (i+1) 1
                           | otherwise = do -- traceShow (i,j,indexTransform n i j) $ return ()
                                            !() <- oneIter' n d' a b i j
                                            go' i (j+1)
                 go'' !i !j | i > 0 =  -- traceShow (i,j,indexTransform n i j) $ 
                                           go i 4 
                            | j >= n-1  =  -- traceShow (i,j,indexTransform n i j) $ 
                                            go'' (i+1) 1
                            | otherwise = do -- traceShow (i,j,indexTransform n i j) $ return ()
                                            !() <- oneIter'' n d' a b i j
                                            go'' i (j+1)

        {-# INLINE oneIter #-} 
        oneIter !n !d !a !b !i !j = do 
                  !north <- VUS.unsafeVectorisedRead b  $ 4*n*(i+1)+j
                  !east  <- VUS.unsafeVectorisedRead b  $ 4*n*i+j-4
                  !here  <- VUS.unsafeVectorisedRead b  $ 4*n*i+j
                  !west  <- VUS.unsafeVectorisedRead b  $ 4*n*i+j+4
                  !south <- VUS.unsafeVectorisedRead b  $ 4*n*(i-1)+j
                  !()    <- VUS.unsafeVectorisedWrite a  (4*n*i+j) $! (1-4*d) * here  
                                                             + d*(  north
                                                                + east
                                                                + west
                                                                + south
                                                               )
                  return()
        {-# INLINE oneIter' #-} 
        oneIter' !n !d !a !b !i !j =  
            forM_ [0..2] $! \j' -> do
                  !north <- VGM.unsafeRead b $ 4*j+j'+1
                  !east  <- VGM.unsafeRead b $ indexTransform n i (j - 1) + j'
                  !here  <- VGM.unsafeRead b $ indexTransform n i j + j'
                  !west  <- VGM.unsafeRead b $ indexTransform n i (j+1) + j'
                  !south <- VGM.unsafeRead b $ indexTransform n (i-1) j + j'
                  !()    <- VGM.unsafeWrite a (indexTransform n i j + j') $! (1-4*d) * here  
                                                             + d*(  north
                                                                + east
                                                                + west
                                                                + south
                                                               )
                  return()
        {-# INLINE oneIter'' #-} 
        oneIter'' !n !d !a !b !i !j =  
            forM_ [1..3] $! \j' -> do
                  !north <- VGM.unsafeRead b $ indexTransform n (i+1) j + j'
                  !east  <- VGM.unsafeRead b $ indexTransform n i (j - 1) + j'
                  !here  <- VGM.unsafeRead b $ indexTransform n i j + j'
                  !west  <- VGM.unsafeRead b $ indexTransform n i (j+1) + j'
                  !south <- VGM.unsafeRead b $ (4*n*(n `div` 4 - 1))+ 4*j+j'-1
                  !()    <- VGM.unsafeWrite a (indexTransform n i j + j') $! (1-4*d) * here  
                                                             + d*(  north
                                                                + east
                                                                + west
                                                                + south
                                                               )
                  return()

printArray n a = do forM_ [0..n-1] (\(!i) -> do
                      --when (i `mod` slice == 0) $ putStrLn ""
                      !liness <- forM [0..n-1] (\(!j) -> do
                           !num <- VGM.unsafeRead a $ indexTransform n i j 
                           return $! showEFloat (Just 5) num " ")
                      putStrLn $! concat liness)
                    putStrLn ""
                    putStrLn ""
    where !slice = (n+3) `div` 4 
    
printArrayRaw n a = do forM_ [0..n-1] (\(!i) -> do
                         --when (i `mod` slice == 0) $ putStrLn ""
                         !liness <- forM [0..n-1] (\(!j) -> do
                              !num <- VGM.unsafeRead a $ n*i+j 
                              return $! showEFloat (Just 5) num " ")
                         putStrLn $! concat liness)
                       putStrLn ""
                       putStrLn ""

main = do let n :: Int
              !n = 16 -- 256
          !a <- solve n $ 16 -- 5*1024
          printArray n a
