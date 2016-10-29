-- |
-- Module      :  Data.Vector.AcceleratedFor.Internal
-- Copyright   :  Jonathan Kochems 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, ScopedTypeVariables, TemplateHaskell, FlexibleContexts #-}
module Data.Vector.AcceleratedFor.Internal where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import GHC.Prim

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Primitive


newtype Accelerate a = Accelerate { unwrap :: Integer ->  Q (Integer, ExpQ -> Cont ExpQ ExpQ, a) }


   

instance Functor Accelerate where
  fmap f x = Accelerate $ \i -> do (i',kont,val) <- unwrap x i
                                   return (i',kont, f val) 

instance Applicative Accelerate where
  pure x    = Accelerate $ \i -> return (i, \s -> cont $ \k -> k s, x) 
  (<*>) f x = Accelerate $ \i -> do (i',kont,x')   <- unwrap x i
                                    (i'',kont',f') <- unwrap f i'
                                    return (i'',\s -> kont s >>= kont',f' x')
                                                              

instance Monad Accelerate where
  return x  = Accelerate $ \i -> return (i, \s -> cont $ \k -> k s, x)
  (>>=) x f = Accelerate $ \i -> do (i',kont,x')   <- unwrap x i 
                                    (i'',kont',y') <- unwrap (f x') i'                                        
                                    return (i'',\s -> kont s >>= kont',y')

generate :: Accelerate a -> Q Exp
generate a = do (_,f,_) <- unwrap a 0
                unitExp <- [|()|]
                runCont (f . varE $ mkName "s") $ \s'' -> do 
                  s' <- s''
                  return $ UnboxedTupE [s', unitExp]

unitE = TupE []
unitP = TupP []

pair x y = do x' <- x
              y' <- y
              return (x',y')
triple x y z = do x' <- x
                  y' <- y
                  z' <- z
                  return (x',y',z')

                  
for1D'' :: ExpQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> (ExpQ -> ExpQ -> Accelerate ExpQ) -> Accelerate ExpQ
for1D'' start' end' inc'  start end inc body 
            = Accelerate $ \j -> 
                             do let i       = [ mkName $ "i" ++ show (j + k) | k <- [1..8]  ]
                                    is      = mkName $ "i" ++ show (j + 9)
                                    is'     = mkName $ "i" ++ show (j + 10)
                                    i'      = mkName $ "i'" ++ show (j + 11)
                                go  <- newName "go"
                                (j',kont',val) <- unwrap (do forM_ i $ \_i -> body (varE i') $ varE _i
                                                             goCall (varE go) (varE i') (varE is) (varE is')) $ j+11
                                let k s = cont $ \kont -> do
                                            s'  <- newName "state"
                                            s'' <- newName "state"
                                            letE [
                                                   head <$> [d| !starts    = packInt32X4# $(unboxedTupE [ [| $(start) +# $(n) *# $(inc) |] | n <- [[|0#|], [|1#|], [|2#|], [|3#|]] ]) |]
                                                 , head <$> [d| !starts'   = packInt32X4# $(unboxedTupE [ [| $(start) +# $(n) *# $(inc) |] | n <- [[|4#|], [|5#|], [|6#|], [|7#|]] ]) |]
                                                 , head <$> [d| !incs      = broadcastInt32X4# ( 8# *# $(inc) )  |]
                                                 , sigD go [t| Int# -> Int32X4# -> Int32X4# -> State# RealWorld -> (# State# RealWorld, () #) |]
                                                 , funD go [clause [varP i', varP is, varP is', varP s'] (normalB $
                                                       [| let $(unboxedTupP . map varP $ take 4 i)           = unpackInt32X4# $(varE is ) 
                                                              $(unboxedTupP . map varP $ take 4 $ drop 4 i ) = unpackInt32X4# $(varE is') 
                                                           in case $(varE $ last i) <# $(end) of
                                                                1# -> $(runCont (kont' $ varE s') $ \s''' -> [| (# $(s'''), ()#) |])
                                                                0# ->
                                                                  case $(varE i') <# $(end') of
                                                                    1# -> $(varE go) ($(varE i') +# $(inc')) starts starts' $(varE s')
                                                                    0# -> (# $(varE s'), () #)
                                                       |]) []]
                                                 ] [| case $(varE go) $(start') starts starts' $(s) of
                                                     (# $(varP s''), () #) -> $(kont $ varE s'')
                                                   |]
                                return (j',k,return unitE)
  where goCall go i' index index' = Accelerate $ \i -> return (i, kont, tupE []) 
          where kont s = cont . const $ [| $(go) $(i') ($(index) `plusInt32X4#` incs)   ($(index') `plusInt32X4#` incs) 
                                              $(s) |]

for1D' :: Q Exp -> Q Exp -> Q Exp -> (Q Exp -> Accelerate ExpQ) -> Accelerate ExpQ
for1D' start end inc body = Accelerate $ \j -> 
                             do let i       = [ mkName $ "i" ++ show (j + k) | k <- [1..8]  ]
                                    is      = mkName $ "i" ++ show (j + 9)
                                    is'     = mkName $ "i" ++ show (j + 10)
                                go  <- newName "go"
                                (j',kont',val) <- unwrap (do forM_ i $ \_i -> body $ varE _i
                                                             goCall (varE go) (varE is) (varE is')) $ j+10
                                let k s = cont $ \kont -> do
                                            s'  <- newName "state"
                                            s'' <- newName "state"
                                            letE [
                                                   head <$> [d| !starts    = packInt32X4# $(unboxedTupE [ [| $(start) +# $(n) *# $(inc) |] | n <- [[|0#|], [|1#|], [|2#|], [|3#|]] ]) |]
                                                 , head <$> [d| !starts'   = packInt32X4# $(unboxedTupE [ [| $(start) +# $(n) *# $(inc) |] | n <- [[|4#|], [|5#|], [|6#|], [|7#|]] ]) |]
                                                 , head <$> [d| !incs      = broadcastInt32X4# ( 8# *# $(inc) )  |]
                                                 , sigD go [t| Int32X4# -> Int32X4# -> State# RealWorld -> (# State# RealWorld, () #) |]
                                                 , funD go [clause [varP is, varP is', varP s'] (normalB $
                                                       [| let $(unboxedTupP . map varP $ take 4 i)           = unpackInt32X4# $(varE is ) 
                                                              $(unboxedTupP . map varP $ take 4 $ drop 4 i ) = unpackInt32X4# $(varE is') 
                                                           in case $(varE $ last i) <# $(end) of
                                                                   0# -> (# $(varE s'), () #)
                                                                   1# -> $(runCont (kont' $ varE s') $ \s''' -> [| (# $(s'''), ()#) |])
                                                       |]) []]
                                                 ] [| case $(varE go) starts starts' $(s) of
                                                     (# $(varP s''), () #) -> $(kont $ varE s'')
                                                   |]
                                return (j',k,return unitE)
  where goCall go index index' = Accelerate $ \i -> return (i, kont, tupE []) 
          where kont s = cont . const $ [| $(go) ($(index) `plusInt32X4#` incs)   ($(index') `plusInt32X4#` incs) 
                                              $(s) |]

for1D :: Q Exp -> Q Exp -> Q Exp -> (Q Exp -> Accelerate ExpQ) -> Accelerate ExpQ
for1D start end inc body = Accelerate $ \i -> 
                             do let loopVar = mkName $ "i" ++ show (i + 1)
                                go  <- newName "go"
                                (i',kont',val) <- unwrap (body (varE loopVar) >> goCall (varE go) (varE loopVar)) $ i+1
                                let k s = cont $ \kont -> do
                                            s'  <- newName "state"
                                            s'' <- newName "state"
                                            letE [
                                                   sigD go [t| Int# -> State# RealWorld -> (# State# RealWorld, () #) |]
                                                 , funD go [clause [varP loopVar, varP s'] (normalB $
                                                       [| case $(varE loopVar) <# $(end) of
                                                            0# -> (# $(varE s'), () #)
                                                            1# -> $(runCont (kont' $ varE s') $ \s''' -> [| (# $(s'''), ()#) |])
                                                       |]) []]
                                                 ] [| case $(varE go) $(start) $(s) of
                                                     (# $(varP s''), () #) -> $(kont $ varE s'')
                                                   |]
                                return (i',k,return unitE)
  where goCall go index = Accelerate $ \i -> return (i, kont, tupE []) 
          where kont s = cont . const $ do [| $(go) ($(index) +# $(inc)) $(s) |]

          
for2DSkip :: (ExpQ, ExpQ) -> ExpQ -> ExpQ -> (ExpQ, ExpQ) -> ExpQ -> (ExpQ -> Accelerate ExpQ) -> Accelerate ExpQ
for2DSkip (indexStart, indexEnd) indexInc indexSkip (colsStart,colsEnd) colsInc body = 
             Accelerate $ \i -> 
                             do let index = mkName $ "i" ++ show (i + 1)
                                    cols  = mkName $ "j" ++ show (i + 2)
                                (i',kont',val) <- unwrap  ((body $ varE index) >> goCall (varE index) (varE cols)) $ i+2
                                let k s = cont $ \kont -> do
                                            s'  <- newName "state"
                                            s'' <- newName "state"
                                            [| let go :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, () #)
                                                   go $(varP index) $(varP cols) $(varP s') = 
                                                      case $(varE cols) <# $(colsEnd) of
                                                       1# -> $(runCont (kont' $ varE s') $ \s''' -> [| (# $(s'''), ()#) |])
                                                       0# -> 
                                                         case $(varE index) <# $(indexEnd) of
                                                          1# -> go ($(varE index) +# $(indexSkip)) $(colsStart) $(varE s')
                                                          0# -> (# $(varE s'), () #) 
                                               in case go $(indexStart) $(colsStart) $(s) of
                                                     (# $(varP s''), () #) -> $(kont $ varE s'')
                                             |]
                                return (i',k,[|()|])
  where goCall index cols = Accelerate $ \i -> return (i, kont, [|()|]) 
          where kont s = cont . const $ do let go = varE $ mkName "go"
                                           [| $(go) ($(index) +# $(indexInc)) ($(cols) +# $(colsInc)) $(s) |]

for2D :: (ExpQ, ExpQ) -> ExpQ -> (ExpQ, ExpQ) -> ExpQ -> (ExpQ -> ExpQ -> Accelerate ExpQ) -> Accelerate ExpQ
for2D (xStart, xEnd) xInc (yStart,yEnd) yInc body = 
             Accelerate $ \i -> 
                             do let x = mkName $ "i" ++ show (i + 1)
                                    y = mkName $ "j" ++ show (i + 2)
                                (i',kont',val) <- unwrap (body (varE x) (varE y) >> goCall (varE x) (varE y)) $ i+2
                                let k s = cont $ \kont -> do
                                            s'  <- newName "state"
                                            s'' <- newName "state"
                                            [| let go :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, () #)
                                                   go $(varP x) $(varP y) $(varP s') = 
                                                      case (# $(varE x) <# $(xEnd), $(varE y) <# $(yEnd) #) of
                                                       (# 1#, 1# #) -> $(runCont (kont' $ varE s') $ \s''' -> [| (# $(s'''), ()#) |])
                                                       (# 0#, 1# #) -> go $(xStart) ($(varE y) +# $(yInc)) $(varE s')
                                                       (# _,  0# #) -> (# $(varE s'), () #)
                                               in case go $(xStart) $(yStart) $(s) of
                                                     (# $(varP s''), () #) -> $(kont $ varE s'')
                                             |]
                                return (i',k,[|()|])
  where goCall xIndex yIndex = Accelerate $ \i -> return (i, kont, [|()|]) 
          where kont s = cont . const $ do let go = varE $ mkName "go" 
                                           [| $(go) ($(xIndex) +# $(xInc)) $(yIndex) $(s) |]

returnAQ = Accelerate $ \i -> do return (i,\s -> cont $ \k -> k s, unitE)

readFloatArrayAsFloatQ x y = Accelerate m 
  where m i = do let i'  = i+1
                     var = mkName $ "read" ++ show i'
                     exp = VarE var
                 s' <- newName "state"
                 let k = withState (varE s') $ \s -> \e ->
                           [| case readFloatArrayAsFloatX4# $(x) $(y) $(s) of
                                (# $(varP s'), $(varP var)#) -> $(e)|]
                 return (i',k,exp)

writeFloatArrayAsFloatQ :: ExpQ -> ExpQ -> ExpQ -> Accelerate ()
writeFloatArrayAsFloatQ x y v = Accelerate $ \i -> 
  do s' <- newName "state"
     let l = withState (varE s') $ \s -> \e -> 
               [| case writeFloatArrayAsFloatX4# $(x) $(y) $(v) $(s) of
                    $(varP s') -> $(e) |]
     return (i, l, ()) 


printQ :: ExpQ  -> Accelerate ()
printQ str = Accelerate $ \i -> 
  do s' <- newName "state"
     let l = withState (varE s') $ \s -> \e -> 
               [| case (\s' -> internal $ putStrLn s') $(str) $(s) of
                    (# $(varP s'), () #) -> $(e) |]
     return (i, l, ()) 


withState :: ExpQ -> (ExpQ -> ExpQ -> ExpQ) -> ExpQ -> Cont ExpQ ExpQ
withState s' f s = cont $ \kont -> f s $ kont s'
