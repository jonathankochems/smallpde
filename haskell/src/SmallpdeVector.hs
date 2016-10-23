{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, QuasiQuotes, ScopedTypeVariables, FlexibleContexts, GADTs, MagicHash #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}

module SmallpdeVector where

import Prelude hiding (read,mapM_,mapM,foldr)
import Data.List (intercalate)
import qualified Data.List as List
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Generic.Mutable(new, read, write, unsafeRead, unsafeWrite, set)
import Control.Monad (forM, forM_)
import Control.Monad.Primitive (PrimState)
import Debug.Trace(traceShow)
import Numeric (showEFloat)
import qualified Data.Vector.Unboxed.SIMD as VUS
import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)

import VectorInst
--import System.IO.Unsafe(unsafePerformIO)

--import Foreign
--foreign import ccall unsafe "Debug.h" perf_marker :: IO ()

--{-# INLINE unsafePurePerfMarker#-}
--unsafePurePerfMarker :: a -> a
--unsafePurePerfMarker x = (unsafePerformIO perf_marker) `seq` x


--{-# INLINE fourIter #-} 
--fourIter !n !d !a !b !i !j  = $(stencil 4) pureStencil' n d a b i j 
--
--{-# INLINE eightIter #-} 
--eightIter !n !d !a !b !i !j = $(stencil 8) pureStencil' n d a b i j 

--{-# INLINE sixtIter #-} 
--sixtIter !n !d !a !b !i !j = $(stencil 16) pureStencil' n d a b i j 

when True  m = m
when False _ = return ()


----foldr            :: (a -> b -> b) -> b -> [a] -> b
---- foldr _ z []     =  z
---- foldr f z (x:xs) =  f x (foldr f z xs)
--{-# INLINE [0] foldr #-}
---- Inline only in the final stage, after the foldr/cons rule has had a chance
---- Also note that we inline it when it has *two* parameters, which are the
---- ones we are keen about specialising!
--foldr k z = go
--          where
--            --go :: [a] -> b
--            go []     = z
--            go (y:ys) = unsafePurePerfMarker ( y `k` go ys )
--
--
--mapM_ :: (a -> IO b) -> [a] -> IO ()
--mapM_ f= foldr ((>>) . f') (return ())
--  where {-# INLINE f' #-}
--        f' x = do perf_marker
--                  f x
---- | 'forM_' is 'mapM_' with its arguments flipped. For a version that
---- doesn't ignore the results see 'Data.Traversable.forM'.
----
---- As of base 4.8.0.0, 'forM_' is just 'for_', specialized to 'Monad'.
--forM_ :: [a] -> (a -> IO b) -> IO ()
--{-# INLINE forM_ #-}
--forM_ = flip mapM_
--
---- | 'forM' is 'mapM' with its arguments flipped. For a version that
---- ignores the results see 'Data.Foldable.forM_'.
--forM ::  [a] -> (a -> IO b) -> IO [b]
--{-# INLINE forM #-}
--forM = flip mapM
--
--{-# INLINE mapM #-} -- so that traverse can fuse
--mapM ::  (a -> IO b) -> [a] -> IO [b]
--mapM f = foldr cons_f (pure [])
--  where {-# INLINE cons_f #-}
--        cons_f x ys = (:) <$> f' x <*> ys
--        {-# INLINE f' #-}
--        f' x = do perf_marker
--                  f x

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
           !() <- timeloop steps n d d' a b
           -- !() <- forM_ [0..steps-1] (\_ -> do
           --          --perf_marker
           --          !() <- onePass n d d' b a
           --          -- printArray n b
           --          !() <- onePass n d d' a b
           --          -- printArray n a
           --          return ()
           --       )
           !() <-  when (iterations `mod` 2 == 1) $ do
                    !() <- onePass n d d' b a
                    -- printArray n b
                    !() <- VGM.unsafeCopy a b
                    return ()
           -- printArray n a
           -- printArrayRaw n a
           return a
  where {-# INLINE timeloop #-}
        timeloop :: Int -> Int -> FloatX4 -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        timeloop !steps !n !d !d' !a !b = go 0 
          where go !i | i < steps = do --perf_marker
                                       -- printArray n a
                                       !() <- onePass n d d' b a
                                       -- printArray n b
                                       !() <- onePass n d d' a b
                                       -- printArray n a
                                       go (i+1)
                      | otherwise = return ()
        {-# INLINE onePass #-}
        onePass :: Int -> FloatX4 -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        onePass !n !d !d' !a !b = go'' 0 1
           where go !i !j | i+4*n >= n*n-2 = -- traceShow (1,i,j,indexTransform n i j) $ 
                                            go' (i `div` (4*n)) 1
                          | j >= 4*n-4  = -- traceShow (2,i,j,indexTransform n i j) $
                                            go (i+4*n) 4
                          | otherwise = do -- traceShow (3,i,j,indexTransform n i j) $ return ()
                                           !() <- oneIter n d a b i j
                                           go' ((n*n) `div` (4*n)-1) 1
                 go' !i !j | 4*i >= n-2 = -- traceShow (4,i,j,indexTransform n i j) $
                                           return ()
                           | j >= n-1  = -- traceShow (5,i,j,indexTransform n i j) $
                                            go' (i+1) 1
                           | otherwise = do -- traceShow (6,i,j,indexTransform n i j) $ return ()
                                            !() <- oneIter' n d a b i j
                                            --perf_marker
                                            go' i (j+1)
                 go'' !i !j | i > 0 = -- traceShow (7,i,j,indexTransform n i j) $
                                           go (4*n) 4
                            | j >= n-1  = -- traceShow (8,i,j,indexTransform n i j) $
                                            go'' (i+1) 1
                            | otherwise = do -- traceShow (9,i,j,indexTransform n i j) $ return ()
                                             !() <- oneIter'' n d a b i j
                                             --perf_marker
                                             go'' i (j+1)

        {-# INLINE oneIter #-}
        oneIter !n !d !a !b !i !j = do
                  let rawb = VUSI.convertToRawVector b
                      rawa = VUSI.convertToRawVector a
                  VUSI.rawVectorisedStencil2 n d# d'# rawa rawb i j 
                  --VUSI.rawVectorisedStencil n d# d'# rawa rawb i (j+4) 
                  -- !north  <-  VUSI.rawVectorisedRead rawb  $ i+4*n+j
                  -- !east   <-  VUSI.rawVectorisedRead rawb  $ i+j-4
                  -- !here   <-  VUSI.rawVectorisedRead rawb  $ i+j
                  -- !west   <-  VUSI.rawVectorisedRead rawb  $ i+j+4
                  -- !south  <-  VUSI.rawVectorisedRead rawb  $ i-4*n+j
                  -- !()    <- VUS.veryunsafeVectorisedWrite a  (i+j) $! (1-4*d) * here  
                  --                                            + d*(  north
                  --                                               + east
                  --                                               + west
                  --                                               + south
                  --                                              )
                  --perf_marker
                  -- !north' <- VUSI.rawVectorisedRead rawb  $ i+4*n+j+4
                  -- !here   <- VUSI.rawVectorisedRead rawb  $ i+j
                  -- !west   <- VUSI.rawVectorisedRead rawb  $ i+j+4
                  -- !west'  <- VUSI.rawVectorisedRead rawb  $ i+j+8
                  -- !south' <- VUSI.rawVectorisedRead rawb  $ i-4*n+j+4
                  -- !()    <- VUS.veryunsafeVectorisedWrite a  (i+j+4) $! (1-4*d) * west  
                  --                                            + d*(  north'
                  --                                               + here
                  --                                               + west'
                  --                                               + south'
                  --                                              )
                  -- return()
           where !(VUSI.FloatX4# d#) = VUSI.coerceToInternalFloatX4 d
                 !(VUSI.FloatX4# d'#) = VUSI.coerceToInternalFloatX4 $! (1-4*d)
        {-# INLINE oneIter' #-} 
        oneIter' !n !d !a !b !i !j = do
            !north0 <- VGM.unsafeRead b $ 4*j+1
            !north1 <- VGM.unsafeRead b $ 4*j+2
            !north2 <- VGM.unsafeRead b $ 4*j+3
            let north = packVector(north0,north1,north2,0)
            !east  <- VUS.veryunsafeVectorisedRead b  $ 4*n*i+4*j-4
            !here  <- VUS.veryunsafeVectorisedRead b  $ 4*n*i+4*j
            !west  <- VUS.veryunsafeVectorisedRead b  $ 4*n*i+4*j+4
            !south <- VUS.veryunsafeVectorisedRead b  $ 4*n*(i-1)+4*j
            let !new = (1-4*d) * here + d*(  north
                                          + east
                                          + west
                                          + south
                                           )
                !new'= unsafeInsertVector new 0 3
            !()    <- VUS.veryunsafeVectorisedWrite a  (4*n*i+4*j) $! new' 
            --perf_marker
            return()

        {-# INLINE oneIter'' #-} 
        oneIter'' !n !d !a !b !i !j = do 
            !north <- VUS.veryunsafeVectorisedRead b  $ 4*n*(i+1)+4*j
            !east  <- VUS.veryunsafeVectorisedRead b  $ 4*n*i+4*j-4
            !here  <- VUS.veryunsafeVectorisedRead b  $ 4*n*i+4*j
            !west  <- VUS.veryunsafeVectorisedRead b  $ 4*n*i+4*j+4
            !south1 <- VGM.unsafeRead b $ (4*n*(n `div` 4 - 1))+ 4*j
            !south2 <- VGM.unsafeRead b $ (4*n*(n `div` 4 - 1))+ 4*j+1
            !south3 <- VGM.unsafeRead b $ (4*n*(n `div` 4 - 1))+ 4*j+2
            let south = packVector(0,south1,south2,south3)
            let !new = (1-4*d) * here + d*(  north
                                          + east
                                          + west
                                          + south
                                           )
                !new'= unsafeInsertVector new 0 0
            !()    <- VUS.veryunsafeVectorisedWrite a  (4*n*i+4*j) $! new' 
            --perf_marker
            return()


printArray n a = go 0 0 (n-1) "" 
    where !slice = (n+3) `div` 4
          go !k !i !j !line | k >= 4 = do putStrLn ""
                                          putStrLn ""
                            | i >= n `div` 4 = go (k+1) 0 (n-1) ""
                            | j >= 0 = do !num <- VGM.unsafeRead a $ 4*n*i+4*j+k
                                          go k i (j-1) $! (showEFloat (Just 5) num " "++line)
                            | j < 0  = do putStrLn line
                                          go k (i+1) (n-1) ""
    
printArrayRaw n a = do forM_ [0..n-1] (\(!i) -> do
                         --when (i `mod` slice == 0) $ putStrLn ""
                         !liness <- forM [0..n-1] (\(!j) -> do
                                      !num <- VGM.unsafeRead a $ n*i+j 
                                      --perf_marker
                                      return $! showEFloat (Just 5) num " ")
                         putStrLn $! concat liness)
                       putStrLn ""
                       putStrLn ""

main = do let n :: Int
              !n = 256
          !a <- solve n $ 5*1024
          printArray n a
