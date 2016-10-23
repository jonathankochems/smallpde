{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, ScopedTypeVariables, TemplateHaskell #-}
module Data.Vector.AcceleratedFor.Equivalence  where

import Debug.Trace

import Data.Maybe(isNothing)

import Language.Haskell.TH
import Language.Haskell.TH.Lib

import System.IO.Unsafe

syntacticEquals :: Exp -> Exp -> Bool
syntacticEquals = (==)



unsafeShowCode :: Q Exp -> String
unsafeShowCode x = unsafePerformIO $ showCodeQ x

showCodeQ :: Q Exp -> IO String
showCodeQ x' = runQ $ do x <- x'
                         return $ pprint x

printCodeQ :: Q Exp -> IO ()
printCodeQ x' = showCodeQ x' >>= putStrLn 

syntacticEqualsQ :: Q Exp -> Q Exp -> IO Bool
syntacticEqualsQ x' y' = runQ $ do x <- x'
                                   y <- y'
                                   return $ (nonameExp x == nonameExp y)

nonameExp (VarE v)                 = VarE $ nonameName v
nonameExp (ConE c)                 = ConE c
nonameExp (LitE l)                 = LitE l
nonameExp (AppE e1 e2)             = AppE (nonameExp e1) $ nonameExp e2
nonameExp (ParensE e)              = ParensE $ nonameExp e
nonameExp (UInfixE e1 op e2)       = UInfixE (nonameExp e1) (nonameExp op) $ nonameExp e2
nonameExp (InfixE me1 op me2)      = InfixE (fmap nonameExp me1) (nonameExp op) $ fmap nonameExp me2
nonameExp (LamE ps e)              = LamE (map nonamePat ps) $ nonameExp e
nonameExp (LamCaseE ms)            = LamCaseE (map nonameMat ms)
nonameExp (TupE [])                = VarE $ mkName "---"
nonameExp (TupE es)                = TupE $ map nonameExp es
nonameExp (UnboxedTupE es)         = UnboxedTupE $ map nonameExp es
nonameExp (CondE guard true false) = CondE (nonameExp guard) (nonameExp true) $ nonameExp false
nonameExp (MultiIfE alts)          = MultiIfE $ map (\(g,e) -> (nonameG g, nonameExp e)) alts
nonameExp (LetE ds_ e)             = LetE (map nonameDecl ds_) $ nonameExp e
nonameExp (CaseE e ms)             = CaseE (nonameExp e) $ map nonameMat ms
nonameExp (DoE ss_)                = DoE $ map nonameStm ss_
nonameExp (CompE ss)               = CompE $ map nonameStm ss
nonameExp (ArithSeqE d)            = ArithSeqE $ nonameRange d
nonameExp (ListE es)               = ListE $ map nonameExp es
nonameExp (SigE e t)               = SigE (nonameExp e) $ nonameType t
nonameExp (RecConE nm fs)          = RecConE (nonameName nm) $ map nonameFieldExp fs
nonameExp (RecUpdE e fs)           = RecUpdE (nonameExp e) $ map nonameFieldExp fs
nonameExp (StaticE e)              = StaticE (nonameExp e)
nonameExp (UnboundVarE v)          = UnboundVarE $ nonameName v

nonamePat (LitP l)          = LitP l
nonamePat (VarP v)          = VarP $ nonameName v
nonamePat (TupP ps)         = TupP $ map nonamePat ps
nonamePat (UnboxedTupP ps)  = UnboxedTupP $ map nonamePat ps
nonamePat (ConP s ps)       = ConP s $ map nonamePat ps
nonamePat (ParensP p)       = ParensP $ nonamePat p
nonamePat (UInfixP p1 n p2) = UInfixP (nonamePat p1) (nonameName n) $ nonamePat p2
nonamePat (InfixP p1 n p2)  = InfixP (nonamePat p1) (nonameName n) $ nonamePat p2
nonamePat (TildeP p)        = TildeP $ nonamePat p
nonamePat (BangP p)         = BangP $ nonamePat p
nonamePat (AsP v p)         = AsP (nonameName v) $ nonamePat p
nonamePat WildP             = WildP
nonamePat (RecP nm fs)      = RecP (nonameName nm) $ map nonameFieldPat fs
nonamePat (ListP ps)        = ListP $ map nonamePat ps
nonamePat (SigP p t)        = SigP (nonamePat p) $ nonameType t
nonamePat (ViewP e p)       = ViewP (nonameExp e) $ nonamePat p

nonameBody (NormalB e)   = NormalB $ nonameExp e 
nonameBody (GuardedB gs) = GuardedB $ map (\(g,e) -> (nonameG g, nonameExp e)) gs 

nonameMat  (Match p b ds)   = Match (nonamePat p) (nonameBody b) $ map nonameDecl ds 

nonameDecl (SigD nm t) = SigD (nonameName nm) $ nonameType t
nonameDecl (FunD nm cs) = FunD (nonameName nm) $ map nonameClause cs
nonameDecl x           = error $ "not implemented yet -- nonameDecl" ++ show x

nonameClause (Clause ps b ds) = Clause (map nonamePat ps) (nonameBody b) $ map nonameDecl ds


nonameG         = error "not implemented yet -- nonameG"
nonameStm       = error "not implemented yet -- nonameStm"
nonameType      x = x -- error "not implemented yet -- nonameType"
nonameFieldExp  = error "not implemented yet -- nonameFieldExp"
nonameFieldPat  = error "not implemented yet -- nonameFieldPat"
nonameRange     = error "not implemented yet -- nonameRange"


nonameName nm | isNothing $ nameSpace nm = mkName "^|^"
              | otherwise                = nm
