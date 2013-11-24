{-# LANGUAGE DoRec #-}
module LazyRec where

import Control.Monad.Trans.Writer
import Data.Maybe

type Symbol = String
type Line = Int
type SymData = (Symbol, Line)
type Stmt = (SymData, [String])
type Prog = [Stmt]

prog :: Prog
prog =
  [ (("alma", 1), ["korte", "alma"])
  , (("korte", 2), ["szilva"])
  , (("szilva", 3), ["alma"])
  ]

type Ref = (Symbol, SymData)

refek :: Prog -> [Ref]
refek stmts =
  let (allSyms, refs) = unzip $ map (stmtRefek allSyms) stmts
  in concat refs

stmtRefek :: [SymData] -> Stmt -> (SymData, [Ref])
stmtRefek allSymData (symData, usedSyms) =
  let thisSym = fst symData
      addThisSym = map ((,) thisSym)
      refs = addThisSym $ catMaybes $ map resolveSym usedSyms
  in (symData, refs)
  where
    resolveSym s = listToMaybe $ filter (\sd -> fst sd == s) allSymData

stmtRefekM :: [SymData] -> Stmt -> Writer [Ref] SymData
stmtRefekM allSymData stmt =
  writer $ stmtRefek allSymData stmt

refekM :: Prog -> [Ref]
refekM stmts =
  let (allSyms, refs) = runWriter $ mapM (stmtRefekM allSyms) stmts
  in refs

refekM2 :: Prog -> [Ref]
refekM2 stmts = snd . runWriter $ do
  rec allSyms <- mapM (stmtRefekM allSyms) stmts
  return allSyms

main = do
  --mapM_ print $ refek prog
  mapM_ print $ refekM2 prog
