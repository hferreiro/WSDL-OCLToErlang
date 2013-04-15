module Language.SurfaceOCL.OCL
  ( module Language.SurfaceOCL.Abs
  , module Language.SurfaceOCL.ErrM
  , module Language.SurfaceOCL.Lex
  , module Language.SurfaceOCL.Par
  , constrsByMethod
  ) where

import qualified Data.Map as Map

import Language.SurfaceOCL.Abs
import Language.SurfaceOCL.ErrM
import Language.SurfaceOCL.Lex
import Language.SurfaceOCL.Par

type MCMap = Map.Map Ident [Constraint]

constrsByMethod :: OCLfile -> MCMap
constrsByMethod (OCLfile ps)
  = foldr go Map.empty (getConstrs ps)

  where
    getConstrs :: [Package] -> [Constraint]
    getConstrs ps = concatMap (\(Pack _ (Constraints cs)) -> cs) ps

    go :: Constraint -> MCMap -> MCMap
    go c m = Map.insertWith (++) (constrOp c) [c] m

constrOp :: Constraint -> Ident
constrOp (Constr (CDOper (OpCRT _ (OpName op) _ _)) _)
  = op
