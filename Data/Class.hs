module Data.Class
  ( Class(..)
  , Attr(..)
  , Method(..)
  , Param(..)
  , TVar(..)
  , Type(..)
  , getType
  , getSimpleType
  , getInnerType
  , isCollectionType
  , addConstrs
  , listTypes
  ) where

import Data.List ( intercalate )
import qualified Data.Map as Map

import Language.OCL.Abs

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
  deriving Eq

instance Show TVar where
  show (TVar n t) = n ++ ":" ++ show t

data Type = TSimple [String]
          | TClass String
          | TTuple [TVar]
          | TSet Type
          | TBag Type
          | TSeq Type
          | TColl Type
  deriving Eq

instance Show Type where
  show (TSimple ss) = intercalate "::" ss
  show (TClass c) = c
  show (TTuple tvs) = "Tuple(" ++ show tvs ++ ")"
  show (TSet t) = "Set(" ++ show t ++ ")"
  show (TBag t) = "Bag(" ++ show t ++ ")"
  show (TSeq t) = "Sequence(" ++ show t ++ ")"
  show (TColl t) = "Collection(" ++ show t ++ ")"

type MCMap = Map.Map Ident [Constraint]

addConstrs :: [Class] -> [Constraint] -> [Class]
addConstrs cls cs = map addC cls
  where
    cc = classifyConstraints cs

    addC (Class n as ms)
      = let (acs, mcs) = Map.findWithDefault ([], Map.empty) n cc
        in Class n (as ++ concatMap toAttr acs) (map (addMC mcs) ms)

    addMC mcs (Method  n r p cs)
      = Method n r p (cs ++ Map.findWithDefault [] n mcs)

    toAttr :: Constraint -> [Attr]
    toAttr c
      = let defs = getDefs c
            tvs = map getTV defs
        in map Attr tvs

    getDefs :: Constraint -> [DefExpression]
    getDefs (Constr _ cbs) = map fromCB cbs
      where
        fromCB (CBDef def) = def
        fromCB (CBDefNamed _ def) = def

    getTV :: DefExpression -> TVar
    getTV (DENoParam (Ident n) _)       = TVar n (TSimple [""])
    getTV (DENoParamType (Ident n) t _) = TVar n (getType t)

getType :: TypeSpecifier -> Type
getType (TSsimple ts)
  = getSimpleType ts

getType (TScoll ct)
  = getCollType ct
  where
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

getSimpleType (STSpec (PathName pn))
  = TSimple (map (\(PName (Ident n)) -> n) pn)

getInnerType (TSet t) = t
getInnerType (TBag t) = t
getInnerType (TSeq t) = t
getInnerType (TColl t) = t
getInnerType _ = error "Cannot get inner type of a single type"

isCollectionType (TSet _) = True
isCollectionType (TBag _) = True
isCollectionType (TSeq _) = True
isCollectionType (TColl _) = True
isCollectionType _ = False

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

listTypes :: [Class] -> [(String, Type)]
listTypes = concatMap types
  where
    types (Class n as ms) = map (fromAttr n) as ++ concatMap (fromMethod n) ms

    fromAttr c (Attr (TVar a t)) = (c ++ "::" ++ a, t)
    fromMethod c (Method m r ps _) = (c ++ "::" ++ m, r):map fromParam ps
      where
        fromParam (Param (TVar p t)) = (c ++ "::" ++ m ++ "::" ++ p, t)
