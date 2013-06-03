module Data.Class
  ( Class(..)
  , Attr(..)
  , Method(..)
  , Param(..)
  , TVar(..)
  , Type(..)
  , addConstrs
  ) where

import qualified Data.Map as Map

import Language.SurfaceOCL.Abs

data Class = Class { cName :: String
                   , cAttrs :: [Attr]
                   , cMethods :: [Method]
                   }
  deriving ( Show )

data Method = Method { mName :: String
                     , mReturn :: Type
                     , mParams :: [Param]
                     , mConstrs :: [Constraint]
                     }
  deriving ( Show )

newtype Attr = Attr TVar
  deriving ( Show )

newtype Param = Param TVar
  deriving ( Show )

data TVar = TVar { tvName :: String
                 , tvType :: Type
                 }
  deriving ( Show )

data Type = TSimple [String]
          | TClass String
          | TTuple [TVar]
          | TSet Type
          | TBag Type
          | TSeq Type
          | TColl Type
  deriving ( Show )

type MCMap = Map.Map Ident [Constraint]

addConstrs :: [Class] -> [Constraint] -> [Class]
addConstrs cls cs = map addC cls
  where
    cc = classifyConstraints cs

    addC (Class n as ms)
      = let (acs, mcs) = Map.findWithDefault ([], Map.empty) n cc
        in Class n (as ++ map toAttr acs) (map (addMC mcs) ms)

    addMC mcs (Method  n r p cs)
      = Method n r p (cs ++ Map.findWithDefault [] n mcs)

    toAttr :: Constraint -> Attr
    toAttr c
      = let def = getDef c
            tv = getTV def
        in Attr tv

    getDef :: Constraint -> DefExpression
    getDef (Constr _ [CBDef def]) = def
    getDef (Constr _ [CBDefNamed _ def]) = def

    getTV :: DefExpression -> TVar
    getTV (DENoParam (Ident n) _)       = TVar n (TSimple [""])
    getTV (DENoParamType (Ident n) t _) = TVar n (getType t)

    getType :: TypeSpecifier -> Type
    getType (TSsimple ts)
      = getSimpleType ts
    getType (TScoll ct)
      = getCollType ct

    getSimpleType (STSpec (PathName pn))
      = TSimple (map (\(PName (Ident n)) -> n) pn)

    getCollType (CT Tuple cb@(CB2 _))
      = getCollBody cb
    getCollType (CT Set cb)
      = TSet (getCollBody cb)
    getCollType (CT Bag cb)
      = TBag (getCollBody cb)
    getCollType (CT Sequence cb)
      = TSeq (getCollBody cb)
    getCollType (CT Collection cb)
      = TColl (getCollBody cb)

    getCollBody :: CollectionBody -> Type
    getCollBody (CB1 ts)
      = getSimpleType ts
    getCollBody (CB2 fps)
      = TTuple (map (\(FP (Ident n) ts) -> TVar n (getType ts)) fps)
    getCollBody (CB3 ct)
      = getCollType ct

type ClassConstrs = Map.Map String ([Constraint], MethodConstrs)
type MethodConstrs = Map.Map String [Constraint]

classifyConstraints
  ::[Constraint] -> ClassConstrs
classifyConstraints cs
  = foldr go Map.empty cs
  where
    go :: Constraint -> ClassConstrs -> ClassConstrs
    go c cs
      = let (cl, elem) = constrType c
        in Map.insertWith addC cl (expand c elem) cs

    addC (cs, ms) (cs', ms')
      = (cs ++ cs', Map.unionWith (++) ms ms')

    expand :: Constraint -> Maybe String -> ([Constraint], MethodConstrs)
    expand c (Just op)
      = ([], Map.singleton op [c])
    expand c Nothing
      = ([c], Map.empty)

    -- (class, maybe method)
    constrType :: Constraint -> (String, Maybe String)
    constrType (Constr (CDClassif (CC (Ident c))) _)
      = (c, Nothing)
    constrType (Constr (CDOper (OpCRT (Ident c) (OpName (Ident op)) _ _)) _)
      = (c, Just op)
    --constrType (Constr (CDOper (OpC _ (OpName op) _)) _)
    constrType c@(Constr (CDOper _) _)
      = error ("Context declaration could not be identified from '" ++ show c ++ "'")
