{-# LANGUAGE BangPatterns, TemplateHaskell, ScopedTypeVariables #-}
module VectorInst where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.SIMD as VUS
import qualified Data.Primitive.SIMD as SIMD

(<$$>) f g = ((<$>) f) <$> g

--sIMD = True
sIMD = False

--tester width = concretePass width
--  where a = error  "lookup"
--        b = error "lookup"
--        n = error "lookup"
--        d = error  "lookup"

constValue width | sIMD && (width == 4 || width ==8) = [| \d -> SIMD.packVector (d,d,d,d) |]
                 | otherwise                         = [| \d -> d |] 

concretePass width = do goBody <- guardedB [ tupM (concretePassLhs width l)
                                                  (concretePassRhs width l) | l <- [0..width] 
                                           ]
                        Just intT   <- ConT <$$> lookupTypeName  "Int"
                        Just ioT    <- ConT <$$> lookupTypeName  "IO"
                        e0 <- [| $(goV) 1 1 |]
                        return $ LetE [ SigD goName $ (ArrowT `AppT` intT) `AppT` ((ArrowT `AppT` intT) `AppT` (AppT ioT $ TupleT 0))
                                      , FunD goName [Clause [i,j] goBody []] 
                                      ] e0 
  where goName = mkName "go"
        goP = BangP $ VarP goName 
        goV = varE goName
        i   = BangP . VarP $ mkName "i"
        j   = BangP . VarP $ mkName "j"

tupM x y = do x' <- x; y' <- y; return (x',y')  

concretePassLhs width l | l == 0     = normalG [| $(i) == $(n)-1     |]
                        | l <  width = normalG [| $(j) == $(n)-(l+1) |]
                        | l >= width = normalG [| otherwise |]
  where n = varE $ mkName "n"
        i = varE $ mkName "i"
        j = varE $ mkName "j"

concretePassRhs width l = doE $ stencils ++ recursiveCall
  where stencils = map (\e -> bindS (bangP $ tupP []) e) 
                        [ [| $(stencil w) $(pureStencil_) $(n) $(d) $(a) $(b) $(i)  ($(j)+k) |]
                          | (w,k) <- zip (widths l) $ intwidths l
                        ]
        recursiveCall | l == width = [noBindS [| $(go) ($(i))   ($(j)+width)|]]
                      | l == 0     = [noBindS $ do Just return_ <- VarE <$$> lookupValueName "return"
                                                   [| $(return return_) ()   |]]
                      | otherwise  = [noBindS [| $(go) ($(i)+1) 1            |]]
                      
        widths x    = reverse $ filter (/=0) $ map (uncurry (*)) $ binary x `zip` [2^i | i <- [0..]]
        intwidths x = [sum $ take i xs | i <- [0..(length xs)-1]]
          where xs = widths x
        n  = varE $ mkName "n"
        d  = varE $ mkName "d"        
        a  = varE $ mkName "a"
        b  = varE $ mkName "b"
        i  = varE $ mkName "i"
        j  = varE $ mkName "j"
        go = varE $ mkName "go"  
        pureStencil_ = VarE <$$> lookupValueName "pureStencil'" >>= \(Just ps) -> return ps

binary 0 = []                    
binary x = r:binary q
  where (q,r) = x `divMod` 2

stencil width = [| \(!pureStencil') (!n) (!d) (!a) (!b) (!i) (!j) -> let pureStencil = pureStencil' ($(constValue width) d) in $(stencilBody width) |]

stencilBody :: Int -> Q Exp
stencilBody width= doE $    (readRow     width 0  "north"    1    0)
                         ++ (readRow     width 2  "eastwest" 0    (-1))
                         ++ (readRow     width 0  "south"    (-1) 0 )
                         ++ (shuffleRow  width 0   "east" "eastwest")
                         ++ (shuffleRow  width 1   "here" "eastwest")
                         ++ (shuffleRow  width 2   "west" "eastwest")
{-                         ++ (packRow     width     "north" )
                         ++ (packRow     width     "east" )
                         ++ (packRow     width     "here" )
                         ++ (packRow     width     "west" )
                         ++ (packRow     width     "south")-}
                         ++ (funcVct     width     "new" "pureStencil" ["here","east","north","west","south"])
{-                         ++ (unpackRow   width     "new")-}
                         -- ++ [printRow    width     "north" ]
                         -- ++ [printRow    width     "east" ]
                         -- ++ [printRow    width     "here" ]
                         -- ++ [printRow    width     "west" ]
                         -- ++ [printRow    width     "south" ]
                         -- ++ [printRow    width     "new" ]
                         ++ (writeRow    width     "new"      0    0)
                         ++ [returnRow   0 "nothing"]

returnRow :: Int -> String -> Q Stmt
returnRow width label = do 
    Just rreturn <- VarE <$$> lookupValueName "return"
    return $ NoBindS (AppE rreturn (TupE row))
  where row = map (\k -> VarE $ mkName $ label++show k) [0..width-1]

printRow :: Int -> String -> Q Stmt
printRow width label = do 
    Just print_ <- VarE <$$> lookupValueName "print"
    labelE      <- [| label |]
    return $ NoBindS (AppE print_ (TupE (labelE:row)))
  where row = map (\k -> VarE $ mkName $ label++vct++show k) [0..w-1]
        vct | sIMD && width >= 4 = "Vct"
            | otherwise          = ""
        w   | sIMD && width >= 4 = (width+3) `div` 4
            | otherwise          = width

packRow :: Int -> String -> [Q Stmt]
packRow width label   | not sIMD || width /= 4 && width /=8 = []
                      | otherwise = [m]

  where rows     = [map (\k -> VarE $ mkName $ label++show (4*j+k)) [0..3] | j <- [0..(width `div` 4)-1]]
        vcts     = [BangP $ VarP vctName | vctName <- vctNames]
        vctNames = [mkName $ label ++ "Vct" ++ show k | k <- [0..width `div` 4]] 

        m = do Just packVector_ <- VarE <$$> lookupValueName "packVector"
               Just floatX4T    <- ConT <$$> lookupTypeName  "FloatX4"
               return . LetS $ concat [[ SigD vctName floatX4T
                                       , ValD vct (NormalB $ AppE packVector_ (TupE row)) []
                                       ] | (row,vctName,vct) <- zip3 rows vctNames vcts]

unpackRow :: Int -> String -> [Q Stmt]
unpackRow width label | not sIMD || width /= 4 && width /=8 = []
                      | otherwise = [m]
  where rows = [map (\k -> BangP . VarP $ mkName $ label++show (4*j+k)) [0..3] | j <- [0..(width `div` 4)-1]]
        vcts = [VarE vctName | vctName <- vctNames]
        vctNames = [mkName $ label ++ "Vct" ++ show k | k <- [0..width `div` 4]]
        m   = do  Just unpackVector_ <- VarE <$$> lookupValueName "unpackVector"
                  return $ LetS $ concat [ 
                                          [ValD (BangP $ TupP row) (NormalB $ AppE unpackVector_ vct) []]  
                                         | (row,vct) <- zip rows vcts]


readRow :: Int -> Int -> String -> Int -> Int -> [Q Stmt]
readRow width extra label rowOffset colOffset 
  | not sIMD || width < 4  = map (readRowk) [0..width+extra-1]
  | otherwise              = map (readVectorRowk) [ i | i <- [0..((width+extra+3) `div` 4)-1]] 
  where readRowk k = do Just b <- varE <$$> lookupValueName "b"
                        Just n <- varE <$$> lookupValueName "n"
                        let i = lookupVar "i"
                            j = lookupVar "j"
                        e <- [| VGM.unsafeRead $(b) ($(n) * ($(i) + rowOffset) + $(j) + colOffset + k) |]
                        return $ BindS l e
          where l = BangP . VarP . mkName $ label++show k
        readVectorRowk k = do Just b <- varE <$$> lookupValueName "b"
                              Just n <- varE <$$> lookupValueName "n"
                              let i = lookupVar "i"
                                  j = lookupVar "j"
                              e <- [| VUS.vectorisedRead $(b) ($(n) * ($(i) + rowOffset) + $(j) + colOffset + 4*k) |]
                              return $ BindS l e                
          where l = BangP . VarP . mkName $ label++"Vct"++show k
          

writeRow :: Int -> String -> Int -> Int -> [Q Stmt]
writeRow width label rowOffset colOffset 
  | not sIMD || width /= 4 && width /=8 = map (writeRowk) [0..width-1]
  | otherwise                           = map (writeVectorRowk) [ i | i <- [0..(width `div` 4)-1]]
  where writeRowk k = do Just a <- varE <$$> lookupValueName "a"
                         Just n <- varE <$$> lookupValueName "n"
                         let i = lookupVar "i"
                             j = lookupVar "j"
                         e <- [| VGM.unsafeWrite $(a) ($(n) * ($(i) + rowOffset) + $(j) + colOffset + k) $(l) |]
                         return $ BindS (BangP $ TupP []) e
          where l = varE . mkName $ label++show k
        writeVectorRowk k = do Just a <- varE <$$> lookupValueName "a"
                               Just n <- varE <$$> lookupValueName "n"
                               let i = lookupVar "i"
                                   j = lookupVar "j"
                               e <- [| VUS.vectorisedWrite $(a) ($(n) * ($(i) + rowOffset) + $(j) + colOffset + 4*k) $(l) |]
                               return $ BindS (BangP $ TupP []) e
          where l = varE . mkName $ label++"Vct"++show k

shuffleRow :: Int -> Int -> String -> String -> [Q Stmt]
shuffleRow    width offset destLabel srcLabel 
  | not sIMD || width /= 4 && width /=8 = map (shuffleRowk) [0..width-1]
  | offset == 0 = map (\k -> shuffleVectorRowk k 0 ) [0..(width `div` 4-1)]
  | offset == 1 = map (\k -> shuffleVectorRowk k (k+1 `max` (width `div` 4-1)) ) [0..(width `div` 4-1)]
  | offset == 2 = map (\k -> shuffleVectorRowk k (k+1 `max` (width `div` 4-1)) ) [0..(width `div` 4-1)]
  where shuffleRowk k = do incBody     <- NormalB <$>  [|$(src)|]
                           Just floatT <- ConT <$$> lookupTypeName  "Float"
                           return $ LetS [ SigD destName floatT
                                         , ValD dest incBody []
                                         ]
          where dest     = BangP $ VarP destName
                destName = mkName $ destLabel ++ show k
                src      = varE . mkName $ srcLabel ++ show (k+offset)
        shuffleVectorRowk k k' 
          = do let shuffled | offset == 0 = [| $(src) |]
                            | offset == 1 = [| VUS.shuffleVectorUp    $(src) $(src') |]
                            | offset == 2 = [| VUS.shuffleVectorUpTwo $(src) $(src') |]
               incBody <- NormalB <$> shuffled
               Just floatX4T <- ConT <$$> lookupTypeName  "FloatX4"
               return $ LetS [ SigD destName floatX4T
                             , ValD dest incBody []
                             ]
          where dest     = BangP $ VarP destName
                destName = mkName $ destLabel ++ "Vct" ++ show k
                src      = varE . mkName $ srcLabel ++ "Vct" ++ show k
                src'     = varE . mkName $ srcLabel ++ "Vct" ++ show k'


incRow :: Int -> String -> String -> [Q Stmt]
incRow width srcLabel destLabel = map (incRowk) [0..width-1]
  where incRowk k = do incBody <- NormalB <$>  [|$(src)+1|]
                       return $ LetS [ValD dest incBody []]
          where dest = BangP . VarP . mkName $ destLabel ++ show k
                src  = varE . mkName $ srcLabel ++ show k

funcVct :: Int -> String -> String -> [String] -> [Q Stmt]
funcVct width destLabel funcLabel srcLabels 
  | not sIMD || width /= 4 && width /=8 = funcRow width destLabel funcLabel srcLabels
  | otherwise  = [m]  
          where dests     = [ BangP $ VarP destName | destName <- destNames]
                destNames = [ mkName $ destLabel ++ "Vct" ++ show k | k <- [0..(width `div` 4)-1] ]
                srcss     = [ map (\srcLabel -> VarE . mkName $ srcLabel ++ "Vct" ++ show k) srcLabels | k <- [0..(width `div` 4) - 1] ]
                m :: Q Stmt
                m     = do f <- lookupVar funcLabel
                           let funBodys = [ NormalB $ foldl AppE f srcs | srcs <- srcss ]
                           return $ LetS $ concat [ [ValD dest funBody []] | (dest,funBody) <- zip dests funBodys ]

lookupVar :: String -> Q Exp
lookupVar name = do preV  <- VarE <$$> lookupValueName name
                    let !v' = VarE $ mkName name
                        !v  = maybe v' (id) preV
                    return v

funcRow :: Int -> String -> String -> [String] -> [Q Stmt]
funcRow width destLabel funcLabel srcLabels = map (incRowk) [0..width-1]
  where incRowk k = do preF  <- VarE <$$> lookupValueName funcLabel
                       let !f' = VarE $ mkName funcLabel
                           !f  = maybe f'(id) preF 
                           funBody = NormalB $ foldl AppE f srcs 
                       return $ LetS [ValD dest funBody []]
          where dest  = BangP . VarP . mkName $ destLabel ++ show k
                srcs  = map (\srcLabel -> VarE . mkName $ srcLabel ++ show k) srcLabels


