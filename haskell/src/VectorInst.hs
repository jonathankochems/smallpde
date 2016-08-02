{-# LANGUAGE BangPatterns, TemplateHaskell, ScopedTypeVariables #-}
module VectorInst where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.Vector.Generic.Mutable as VGM

(<$$>) f g = ((<$>) f) <$> g

stencilBody :: Int -> Q Exp
stencilBody width= doE $    (readRow     width     "north"    1    0)
                         ++ (readRow     (width+2) "eastwest" 0    (-1)) 
                         ++ (readRow     width     "south"    (-1) 0 )
                         ++ (shuffleRow  width 0   "east" "eastwest")
                         ++ (shuffleRow  width 1   "here" "eastwest")
                         ++ (shuffleRow  width 2   "west" "eastwest")              
                         ++ (funcRow     width     "new" "pureStencil" ["here","east","north","west","south"])              
                         ++ (writeRow    width     "new"      0    0)
                         ++ [returnRow 0 "nothing"]

returnRow :: Int -> String -> Q Stmt
returnRow width label = do 
    Just rreturn <- ((<$>) VarE) <$> lookupValueName "return"
    return $ NoBindS (AppE rreturn (TupE row))
  where row = map (\k -> VarE $ mkName $ label++show k) [0..width-1]

printRow :: Int -> String -> Q Stmt
printRow width label = do 
    Just print_ <- ((<$>) VarE) <$> lookupValueName "print"
    return $ NoBindS (AppE print_ (TupE row))
  where row = map (\k -> VarE $ mkName $ label++show k) [0..width-1]


readRow :: Int -> String -> Int -> Int -> [Q Stmt]
readRow width label rowOffset colOffset = map (readRowk) [0..width-1]
  where readRowk k = do Just b <- varE <$$> lookupValueName "b"
                        Just n <- varE <$$> lookupValueName "n"
                        Just i <- varE <$$> lookupValueName "i"
                        Just j <- varE <$$> lookupValueName "j"
                        e <- [| VGM.unsafeRead $(b) ($(n) * ($(i) + rowOffset) + $(j) + colOffset + k) |]
                        return $ BindS l e
          where l = BangP . VarP . mkName $ label++show k

writeRow :: Int -> String -> Int -> Int -> [Q Stmt]
writeRow width label rowOffset colOffset = map (writeRowk) [0..width-1]
  where writeRowk k = do Just a <- varE <$$> lookupValueName "a"
                         Just n <- varE <$$> lookupValueName "n"
                         Just i <- varE <$$> lookupValueName "i"
                         Just j <- varE <$$> lookupValueName "j"
                         e <- [| VGM.unsafeWrite $(a) ($(n) * ($(i) + rowOffset) + $(j) + colOffset + k) $(l) |]
                         return $ BindS (BangP $ TupP []) e
          where l = varE . mkName $ label++show k

shuffleRow :: Int -> Int -> String -> String -> [Q Stmt]
shuffleRow    width offset destLabel srcLabel = map (shuffleRowk) [0..width-1]
  where shuffleRowk k = do incBody <- NormalB <$>  [|$(src)|]
                           return $ LetS [ValD dest incBody []]
          where dest = BangP . VarP . mkName $ destLabel ++ show k
                src  = varE . mkName $ srcLabel ++ show (k+offset)


incRow :: Int -> String -> String -> [Q Stmt]
incRow width srcLabel destLabel = map (incRowk) [0..width-1]
  where incRowk k = do incBody <- NormalB <$>  [|$(src)+1|]
                       return $ LetS [ValD dest incBody []]
          where dest = BangP . VarP . mkName $ destLabel ++ show k
                src  = varE . mkName $ srcLabel ++ show k

funcRow :: Int -> String -> String -> [String] -> [Q Stmt]
funcRow width destLabel funcLabel srcLabels = map (incRowk) [0..width-1]
  where incRowk k = do Just f <- VarE <$$> lookupValueName funcLabel
                       let funBody = NormalB $ foldl AppE f srcs 
                       return $ LetS [ValD dest funBody []]
          where dest  = BangP . VarP . mkName $ destLabel ++ show k
                srcs  = map (\srcLabel -> VarE . mkName $ srcLabel ++ show k) srcLabels


