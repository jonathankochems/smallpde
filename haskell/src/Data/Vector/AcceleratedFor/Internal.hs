{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, ScopedTypeVariables, TemplateHaskell, FlexibleContexts #-}
module Data.Vector.AcceleratedFor.Internal where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import GHC.Prim

import Control.Monad.Cont
import Control.Monad.Reader

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

for1D :: Q Exp -> Q Exp -> Q Exp -> (Q Exp -> Accelerate ExpQ) -> Accelerate ExpQ
for1D start end inc body = Accelerate $ \i -> 
                             do let loopVar = mkName $ "i" ++ show (i + 1)
                                (i',kont',val) <- unwrap (body (varE loopVar) >> goCall (varE loopVar)) $ i+1
                                let k s = cont $ \kont -> do
                                            s'  <- newName "state"
                                            s'' <- newName "state"
                                            [| let go :: Int# -> State# RealWorld -> (# State# RealWorld, () #)
                                                   go $(varP loopVar) $(varP s') = 
                                                      case $(varE loopVar) <# $(end) of
                                                       0# -> (# $(varE s'), () #)
                                                       1# -> $(runCont (kont' $ varE s') $ \s''' -> [| (# $(s'''), ()#) |])
                                               in case go $(start) $(s) of
                                                     (# $(varP s''), () #) -> $(kont $ varE s'')
                                             |]
                                return (i',k,return unitE)
  where goCall index = Accelerate $ \i -> return (i, kont, tupE []) 
          where kont s = cont . const $ [| go ($(index) +# $(inc)) $(s) |]

          
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
          where kont s = cont . const $ [| go ($(index) +# $(indexInc)) ($(cols) +# $(colsInc)) $(s) |]

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
          where kont s = cont . const $ [| go ($(xIndex) +# $(xInc)) $(yIndex) $(s) |]

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
