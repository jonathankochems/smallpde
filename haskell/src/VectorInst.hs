{-# LANGUAGE BangPatterns, TemplateHaskell, ScopedTypeVariables #-}
module VectorInst where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.Vector.Generic.Mutable as VGM

import Data.Maybe(fromMaybe)

(<$$>) f g = (<$>) f <$> g

constValue width  = [| id |] 


unitT = TupleT 0

infixr `arrow`
arrow t t' = (ArrowT `AppT` t) `AppT` t'

concretePass :: Int -> Q Exp
concretePass width = do goBody <- guardedB [ tupM (concretePassLhs width l)
                                                  (concretePassRhs width l) | l <- [0..width] 
                                           ]
                        Just intT       <- ConT <$$> lookupTypeName "Int"
                        Just floatT     <- ConT <$$> lookupTypeName "Float"
                        Just ioT        <- ConT <$$> lookupTypeName "IO"
                        Just pureStencil' <- varE <$$> lookupValueName "pureStencil'"
                        let dV = lookupVar "d"
                        pureStencilBody <- NormalB <$> [| $(pureStencil') ($(constValue width) $(dV)) |] 
                        pureStencilBody' <- NormalB <$> [| $(pureStencil') ($(dV)) |] 
                        e0 <- [| $(goV) 1 $(offset) |]
                        let ns = [ 2^i | i <- [0..floor $ logBase 2 (fromInteger $ toInteger width) ]] ++ 
                                  [ width | width > 2 ^ floor (logBase 2 (fromInteger $ toInteger width))] 
                            stencilNames = [ mkName $ "stencil" ++ show w | w <- ns]
                        stencilBodies <- mapM (\w -> [| $(stencilBodyName w) |]) ns

                        return $ LetE ([ SigD goName $ intT `arrow` intT `arrow` AppT ioT unitT
                                       , FunD goName [Clause [i,j] goBody []] 
                                       , FunD pureStencilName  [Clause [] pureStencilBody []] 
                                       , FunD pureStencilName' [Clause [] pureStencilBody' []] 
                                       ]
                                       ++ map (\n -> SigD n $ intT `arrow` intT `arrow` AppT ioT unitT) stencilNames
                                       ++ map (\(sn,e) -> FunD sn [Clause [i,j] (NormalB e) []]) (zip stencilNames stencilBodies)) e0 
  where goName = mkName "go"
        goP = BangP $ VarP goName 
        goV = varE goName
        i   = BangP . VarP $ mkName "i"
        j   = BangP . VarP $ mkName "j"
        pureStencilName  = mkName "pureStencil"
        pureStencilName' = mkName "pureStencil_"
        stencilBodyName w = stencilBody w
        offset  = [|1|]


tupM x y = do x' <- x; y' <- y; return (x',y')  

concretePassLhs width l | l == 0     = normalG [| $(i) == $(n)-1     |]
                        | l <  width = normalG [| $(j) == $(n)-(l+ $(offset)) |]
                        | l >= width = normalG [| otherwise |]
  where n = varE $ mkName "n"
        i = varE $ mkName "i"
        j = varE $ mkName "j"
        offset = [|1|]

concretePassRhs width l = doE $ stencils ++ recursiveCall
  where stencils = map (bindS (bangP $ tupP [])) 
                        [ [| $(stencilName w) $(i) ($(j)+k) |]
                          | (w,k) <- zip (widths l) $ intwidths l
                        ]
        recursiveCall | l == width = [noBindS [| $(go) ($(i))   ($(j)+width)|]]
                      | l == 0     = [noBindS $ do Just return_ <- VarE <$$> lookupValueName "return"
                                                   [| $(return return_) ()   |]]
                      | otherwise  = [noBindS [| $(go) ($(i)+1) ($offset)    |]]
        widths :: Int -> [Int]             
        widths x    = if l == width && width > 0 && width > 2^floor (logBase 2 (fromInteger $ toInteger width)) 
                      then [ width ] else reverse $ filter (/=0) (zipWith (*) (binary x) [2^i | i <- [0..]])
        intwidths x = [sum $ take i xs | i <- [0..length xs-1]]
          where xs = widths x
        i             = varE $ mkName "i"
        j             = varE $ mkName "j"
        stencilName w = varE . mkName $ "stencil" ++ show w
        go            = varE $ mkName "go"  
        offset        = [|1|]

binary 0 = []                    
binary x = r:binary q
  where (q,r) = x `divMod` 2


stencilBody :: Int -> Q Exp
stencilBody width= doE $    readRow     width 0  "north"    1    0
                         ++ readRow     width 2  "eastwest" 0    (-1)
                         ++ readRow     width 0  "south"    (-1) 0 
                         ++ shuffleRow  width 0   "east" "eastwest"
                         ++ shuffleRow  width 1   "here" "eastwest"
                         ++ shuffleRow  width 2   "west" "eastwest"
                         ++ funcRow     width     "new" "pureStencil" ["here","east","north","west","south"]
                         ++ writeRow    width     "new"      0    0
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
        vct  = ""
        w    = width

readSingle :: String -> Int -> Int -> Q Stmt
readSingle    label rowOffset colOffset 
  = do Just b <- varE <$$> lookupValueName "b"
       Just n <- varE <$$> lookupValueName "n"
       let i = lookupVar "i"
           j = lookupVar "j"
       e <- [| VGM.unsafeRead $(b) ($(n) * ($(i) + rowOffset) + $(j) + colOffset) |]
       return $ BindS l e
          where l = BangP . VarP . mkName $ label

readRow :: Int -> Int -> String -> Int -> Int -> [Q Stmt]
readRow width extra label rowOffset colOffset 
    = map readRowk [0..width+extra-1]
  where readRowk k = do Just b <- varE <$$> lookupValueName "b"
                        Just n <- varE <$$> lookupValueName "n"
                        let i = lookupVar "i"
                            j = lookupVar "j"
                        e <- [| VGM.unsafeRead $(b) ($(n) * ($(i) + rowOffset) + $(j) + colOffset + k) |]
                        return $ BindS l e
          where l = BangP . VarP . mkName $ label++show k
          

writeRow :: Int -> String -> Int -> Int -> [Q Stmt]
writeRow width label rowOffset colOffset 
   = map writeRowk [0..width-1]
  where writeRowk k = do Just a <- varE <$$> lookupValueName "a"
                         Just n <- varE <$$> lookupValueName "n"
                         let i = lookupVar "i"
                             j = lookupVar "j"
                         e <- [| VGM.unsafeWrite $(a) ($(n) * ($(i) + rowOffset) + $(j) + colOffset + k) $(l) |]
                         return $ BindS (BangP $ TupP []) e
          where l = varE . mkName $ label++show k

shuffleRow :: Int -> Int -> String -> String -> [Q Stmt]
shuffleRow    width offset destLabel srcLabel 
   = map shuffleRowk [0..width-1]
  where shuffleRowk k = do incBody     <- NormalB <$>  [|$(src)|]
                           Just floatT <- ConT <$$> lookupTypeName  "Float"
                           return $ LetS [ SigD destName floatT
                                         , ValD dest incBody []
                                         ]
          where dest     = BangP $ VarP destName
                destName = mkName $ destLabel ++ show k
                src      = varE . mkName $ srcLabel ++ show (k+offset)

lookupVar :: String -> Q Exp
lookupVar name = do preV  <- VarE <$$> lookupValueName name
                    let !v' = VarE $ mkName name
                        !v  = fromMaybe v' preV
                    return v

funcRow :: Int -> String -> String -> [String] -> [Q Stmt]
funcRow width destLabel funcLabel srcLabels = map incRowk [0..width-1]
  where incRowk k = do preF  <- VarE <$$> lookupValueName funcLabel
                       let !f' = VarE $ mkName funcLabel
                           !f  = fromMaybe f' preF 
                           funBody = NormalB $ foldl AppE f srcs 
                       return $ LetS [ValD dest funBody []]
          where dest  = BangP . VarP . mkName $ destLabel ++ show k
                srcs  = map (\srcLabel -> VarE . mkName $ srcLabel ++ show k) srcLabels


