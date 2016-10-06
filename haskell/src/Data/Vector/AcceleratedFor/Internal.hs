{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, ScopedTypeVariables, TemplateHaskell #-}
module Data.Vector.AcceleratedFor.Internal where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import GHC.Prim 

newtype Accelerate a = Accelerate { unwrap :: Integer ->  Q (Integer,Q Exp -> (Q Exp -> Q Exp) -> Q Exp, a) }


   

instance Functor Accelerate where
  fmap f x = Accelerate $ \i -> do (i',kont,val) <- unwrap x i
                                   return (i',kont, f val) 

instance Applicative Accelerate where
  pure x    = Accelerate $ \i -> return (i, \s -> \k -> k s, x) 
  (<*>) f x = Accelerate $ \i -> do (i',kont,x')   <- unwrap x i
                                    (i'',kont',f') <- unwrap f i'
                                    let kont'' s k = kont' s (flip kont k)                                        
                                    return (i'',kont'',f' x')
                                                              

instance Monad Accelerate where
  return x  = Accelerate $ \i -> return (i, \s -> \k -> k s, x)
  (>>=) x f = Accelerate $ \i -> do (i',kont,x')   <- unwrap x i 
                                    (i'',kont',y') <- unwrap (f x') i'                                        
                                    let kont'' s k = kont s (flip kont' k)                                        
                                    return (i'',kont'',y')

generate :: Accelerate a -> Q Exp
generate a = do (_,f,_) <- unwrap a 0
                f (varE $ mkName "s") $ \s'' -> do s' <- s''
                                                   return $ UnboxedTupE [s', TupE []]

unitE = TupE []
unitP = TupP []

pair x y = do x' <- x
              y' <- y
              return (x',y')
triple x y z = do x' <- x
                  y' <- y
                  z' <- z
                  return (x',y',z')

forAQ :: Q Exp -> Q Exp -> Q Exp -> (Q Exp -> Accelerate ExpQ) -> Accelerate ExpQ
forAQ start end inc body = Accelerate $ \i -> 
                             do let loopVar = mkName $ "i" ++ show (i + 1)
                                (i',kont',val) <- unwrap (body (varE loopVar) >> goCall (varE loopVar)) $ i+1
                                let k s kont = do
                                        s'  <- newName "state"
                                        s'' <- newName "state"
                                        [| let go :: Int# -> State# RealWorld -> (# State# RealWorld, () #)
                                               go $(varP loopVar) $(varP s') = 
                                                  case $(varE loopVar) <# $(end) of
                                                   0# -> (# $(varE s'), () #)
                                                   1# -> $(kont' (varE s') (\s''' -> [| (# $(s'''), ()#) |]))
                                           in case go $(start) $(s) of
                                                 (# $(varP s''), () #) -> $(kont $ varE s'')
                                         |]
                                return (i',k,return unitE)
  where goCall index = Accelerate $ \i -> return (i, kont, tupE []) 
          where kont s k = do s''' <- newName "state"
                              [| case go ($(index) +# $(inc)) $(s) of 
                                  (# $(varP s'''), () #) -> $(k $ varE s''')
                               |]
                    
returnAQ = Accelerate $ \i -> do return (i,\s -> \k -> k s, unitE)

readFloatArrayAsFloatQ x y = Accelerate m 
  where m i = do let i'  = i+1
                     var = mkName $ "read" ++ show i'
                     exp = VarE var
                 s' <- newName "state"
                 let k s kont = [| case readFloatArrayAsFloatX4# $(x) $(y) $(s) of
                                   (# $(varP s'), $(varP var)#) -> $(kont (varE s'))|]
                 return (i',k,exp)

writeFloatArrayAsFloatQ x y v = Accelerate $ \i -> return (i, l, ()) 
  where l s kont = do s' <- newName "state"
                      [| case writeFloatArrayAsFloatX4# $(x) $(y) $(v) $(s) of
                          $(varP s') -> $(kont $ varE s')|]
        
