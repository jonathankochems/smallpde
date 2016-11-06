{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, QuasiQuotes
           , ScopedTypeVariables, FlexibleContexts, GADTs, MagicHash,UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}

module SmallpdeSIMD where

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

import Control.Monad

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)

import Language.Haskell.TH
import qualified Data.Vector.AcceleratedFor.Internal as AFI
import Data.Vector.AcceleratedFor.Internal (for1D,for2D,for2DSkip,for1D',for1D'',for2D'',for2D''',printQ)
import GHC.Prim
import Control.Monad.Primitive(primitive,primToIO,internal)
import GHC.Base (Int(..))

import qualified Data.Primitive.ByteArray as ByteArray
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
               !steps = iterations
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
           -- print (d,nCeil*nCeil)
           !() <- timeloop steps n d d' a b
           return $ if steps `mod` 2 == 0 then a else b
  where {-# INLINE timeloop #-}
        timeloop :: Int -> Int -> FloatX4 -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        timeloop !steps !n !d !d' !a !b = do 
                              let rawa = VUSI.convertToRawVector a
                                  rawb = VUSI.convertToRawVector b
                              primitive $ go rawa rawb
         where !(I# n#)       = n 
               !(I# n'#)      = n `div` 4
               !(I# nborder#) = 4*n*(n `div` 4 - 1) 
               !(I# steps#) = steps
               !d#          = VUSI.coerceToFloatX4# d
               go ::MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, () #)
               go rawa rawb s = $(AFI.generate $ do 
                       let _d      = varE $ mkName "d#"
                       -- printQ [| show ( "size"
                       --                , I# n#
                       --                , I# n'#
                       --                , I# nborder#
                       --                , I# nborder#
                       --                , VUSI.coerceFromFloatX4# $(_d)
                       --                ) |]
                       for2D''' [| 0# |] [| steps# |]   [| 1# |] 
                                [| 0# |] [| n'# |]      [| 1# |]  
                                [| 4# |] [| 4# *# n# |] [| 4# |]
                                [| rawa |] [| rawb |] 
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <-        _a `AFI.readFloatArrayAsFloatSouth` [| nborder# +# $(_j) |]
                             -- printQ [| show ( "prologue"
                             --                , I# $(_i), I# $(_j), I# (4# *# n# *# $(_i) +# $(_j))
                             --                , VUSI.coerceFromFloatX4# $(north)
                             --                , VUSI.coerceFromFloatX4# $(east)
                             --                , VUSI.coerceFromFloatX4# $(here)
                             --                , VUSI.coerceFromFloatX4# $(west)
                             --                , VUSI.coerceFromFloatX4# $(south) 
                             --                , VUSI.coerceFromFloatX4# ((( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                             --                            ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                             --                        `timesFloatX4#` $(here))  
                             --                        `plusFloatX4#` 
                             --                        ($(_d)  `timesFloatX4#` (  $(north)
                             --                                  `plusFloatX4#`  $(east)
                             --                                  `plusFloatX4#`  $(west)
                             --                                  `plusFloatX4#`  $(south)
                             --                                               )
                             --                        )) 
                             --                ) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| let new# = (( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                                                                    ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                                                                `timesFloatX4#` $(here))  
                                                                `plusFloatX4#` 
                                                                ($(_d)  `timesFloatX4#` (  $(north)
                                                                          `plusFloatX4#`  $(east)
                                                                          `plusFloatX4#`  $(west)
                                                                          `plusFloatX4#`  $(south)
                                                                                       ))
                                                    in insertFloatX4# new# 0.0# 0# 
                                                 |] 
                             return [| () |])
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             -- printQ [| show ( "body"
                             --                , I# $(_i), I# $(_j), I# (4# *# n# *# $(_i) +# $(_j))
                             --                , VUSI.coerceFromFloatX4# $(north)
                             --                , VUSI.coerceFromFloatX4# $(east)
                             --                , VUSI.coerceFromFloatX4# $(here)
                             --                , VUSI.coerceFromFloatX4# $(west)
                             --                , VUSI.coerceFromFloatX4# $(south) 
                             --                , VUSI.coerceFromFloatX4# ((( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                             --                            ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                             --                        `timesFloatX4#` $(here))  
                             --                        `plusFloatX4#` 
                             --                        ($(_d)  `timesFloatX4#` (  $(north)
                             --                                  `plusFloatX4#`  $(east)
                             --                                  `plusFloatX4#`  $(west)
                             --                                  `plusFloatX4#`  $(south)
                             --                                               )
                             --                        )) 
                             --                ) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                                                        ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                                                    `timesFloatX4#` $(here))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north)
                                                              `plusFloatX4#`  $(east)
                                                              `plusFloatX4#`  $(west)
                                                              `plusFloatX4#`  $(south)
                                                                           )
                                                    ) |] 
                             return [| () |])
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <-        _a `AFI.readFloatArrayAsFloatNorth` [| 0# +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             -- printQ [| show ( "epilogue"
                             --                , I# $(_i), I# $(_j), I# (4# *# n# *# $(_i) +# $(_j))
                             --                , VUSI.coerceFromFloatX4# $(north)
                             --                , VUSI.coerceFromFloatX4# $(east)
                             --                , VUSI.coerceFromFloatX4# $(here)
                             --                , VUSI.coerceFromFloatX4# $(west)
                             --                , VUSI.coerceFromFloatX4# $(south) 
                             --                , VUSI.coerceFromFloatX4# ((( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                             --                            ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                             --                        `timesFloatX4#` $(here))  
                             --                        `plusFloatX4#` 
                             --                        ($(_d)  `timesFloatX4#` (  $(north)
                             --                                  `plusFloatX4#`  $(east)
                             --                                  `plusFloatX4#`  $(west)
                             --                                  `plusFloatX4#`  $(south)
                             --                                               )
                             --                        )) 
                             --                ) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| let new# = (( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                                                                    ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                                                                `timesFloatX4#` $(here))  
                                                                `plusFloatX4#` 
                                                                ($(_d)  `timesFloatX4#` (  $(north)
                                                                          `plusFloatX4#`  $(east)
                                                                          `plusFloatX4#`  $(west)
                                                                          `plusFloatX4#`  $(south)
                                                                                       ))
                                                     in insertFloatX4# new# 0.0# 3# 
                                                 |] 
                             return [| () |])
                            )


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
