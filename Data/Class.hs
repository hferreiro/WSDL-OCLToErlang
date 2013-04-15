module Data.Class
  ( Class(..)
  , Method(..)
  , Param(..)
  , addConstrs
  ) where

import qualified Data.Map as Map

import Language.SurfaceOCL.Abs ( Constraint, Ident(..) )

data Class = Class { cName :: String
                   , cMethods :: [Method]
                   }
  deriving ( Show )

data Method = Method { mName :: String
                     , mType :: String
                     , mParams :: [Param]
                     , mConstrs :: [Constraint]
                     }
  deriving ( Show )

data Param = Param { pName :: String
                   , pType :: String
                   }
  deriving ( Show )

type MCMap = Map.Map Ident [Constraint]

addConstrs :: Class -> MCMap -> Class
addConstrs (Class n ms) m = Class n (map addCs ms)
  where
    addCs (Method n t p cs)
      = Method n t p (Map.findWithDefault cs (Ident n) m)
