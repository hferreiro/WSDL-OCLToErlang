{-# LANGUAGE ExistentialQuantification #-}
module Text.ErlangPropList ( propList ) where

import Text.PrettyPrint

import Data.Class
import Data.List ( intercalate )
import qualified Data.Map as M
import Language.OCL

propList :: [Class] -> String
propList cs = render (proplist (M.union env (M.fromList (listTypes cs))) cs)
  where
    env = M.fromList [ ("LAnd",  TSimple ["Boolean"])
                     , ("LOr",   TSimple ["Boolean"])
                     , ("LXor",  TSimple ["Boolean"])
                     , ("EEq",   TSimple ["Boolean"])
                     , ("ENEq",  TSimple ["Boolean"])
                     , ("AAdd",  TSimple ["Integer"])
                     , ("ASub",  TSimple ["Integer"])
                     , ("MMult", TSimple ["Integer"])
                     , ("MDiv",  TSimple ["Integer"])
                     ]

type Env = M.Map String Type

envType :: String -> Env -> Type
envType s env = M.findWithDefault (error (s ++ " type not found")) s env

oclType :: Expression -> Env -> Type
oclType (EExplPropCall e1 PArrow (PCall a _ _ _)) env
  = case () of
      _ | pathName a `elem` ["at"] ->
            let t = oclType e1 env in
              if isCollectionType t
              then getInnerType t
              else error (pathName a ++ " cannot be applied to an expression of type '" ++ show t ++ "'")
        | pathName a `elem` ["size"] ->
            let t = oclType e1 env in
              if isCollectionType t
              then TSimple ["Integer"]
              else error (pathName a ++ " cannot be applied to an expression of type '" ++ show t ++ "'")
        | pathName a `elem` ["includesAll", "isEmpty", "notEmpty"] ->
            let t = oclType e1 env in
              if isCollectionType t
              then TSimple ["Boolean"]
              else error (pathName a ++ " cannot be applied to an expression of type '" ++ show t ++ "'")
        | pathName a `elem` ["collect", "excluding", "including", "select"] ->
            let t = oclType e1 env in
              if isCollectionType t
              then t
              else error (pathName a ++ " cannot be applied to an expression of type '" ++ show t ++ "'")
        | otherwise ->
            error ("'" ++ pathName a ++ "' type not implemented")
oclType e env
  = envType (oclExpr e env) env

oclExpr :: Expression -> Env -> String
oclExpr (EImplPropCall (PCall c _ _ _)) _
  = pathName c
oclExpr (EExplPropCall e1 PDot (PCall a _ _ _)) env
  = show (oclType e1 env) ++ "::" ++ pathName a
oclExpr e _
  = error ("Cannot find expression in '" ++ show e ++ "'")

class ErlangPropList a where
  proplist :: Env -> a -> Doc
  proplist _ _ = text "ErlangPropList instance not implemented"

  proplistList :: Env -> [a] -> Doc
  proplistList env as = brackets $ sep (punctuate comma (map (proplist env) as))

-- Instances
data PropList = forall a. ErlangPropList a => PL a

instance ErlangPropList PropList where
  proplist env (PL a) = proplist env a

instance ErlangPropList Char where
  proplistList _ s = text s

instance ErlangPropList Integer where
  proplist _ i = integer i

instance ErlangPropList a => ErlangPropList [a] where
  proplist = proplistList

instance (ErlangPropList a, ErlangPropList b) => ErlangPropList (a,b) where
  proplist env (a,b) = braces $ hang (proplist env a <> comma) 2 (proplist env b)

-- OCL
instance ErlangPropList Ident where
  proplist env (Ident s) = doubleQuotes (proplist env s)

instance ErlangPropList PathName where
  proplist env pn
    = proplist env $ Ident (pathName pn)

pathName :: PathName -> String
pathName (PathName pn) = intercalate "::" (map (\(PName (Ident n)) -> n) pn)

instance ErlangPropList Constraint where
  proplist env (Constr _ cbs)
    = proplist env $ [ ("preconditions", pre)
                     , ("postconditions", post)
                     , ("invariants", inv)
                     ]

    where
      pre = findBy Pre cbs
      post = findBy Post cbs
      inv = findBy Inv cbs

      findBy t cs = filter (matchSt t) cs

      matchSt t (CB st _)        = t == st
      matchSt t (CBNamed st _ _) = t == st
      matchSt _ _                = False

instance ErlangPropList FormalParameter where
  proplist env (FP n ts)
    = proplist env $ ("param", [ ("name", PL n)
                               , ("type", PL ts)
                               ])

instance ErlangPropList TypeSpecifier where
  proplist env (TSsimple t) = proplist env t

instance ErlangPropList SimpleTypeSpecifier where
  proplist env (STSpec t) = proplist env t

instance ErlangPropList ConstrBody where
  proplist env (CB _ ocle) = proplist env ("constraint", [ ("name", PL (Ident ""))
                                                         , ("body", PL ocle)
                                                         ])
  proplist env (CBNamed _ n ocle) = proplist env ("constraint", [ ("name", PL n)
                                                                , ("body", PL ocle)
                                                                ])

instance ErlangPropList Stereotype where
  proplist _ Pre = text "preconditions"
  proplist _ Post = text "postconditions"
  proplist _ Inv = text "invariants"

instance ErlangPropList Expression where
  proplist env (EOpLog e1 op e2)
    = proplistBinOp env e1 op e2

  proplist env (EOpEq e1 op e2)
    = proplistBinOp env e1 op e2

  proplist env (EOpAdd e1 op e2)
    = proplistBinOp env e1 op e2

  proplist env (EOpMul e1 op e2)
    = proplistBinOp env e1 op e2

  proplist env (EImplPropCall (PCall n _ _ _))
    = proplist env $ [ ("expression", PL (Ident "VariableExp"))
                     , ("referredVariable", plVar (pathName n) (envType (pathName n) env) Nothing)
                     ]

  proplist env (EExplPropCall (ELitColl c) PArrow (PCall op _ _ (PCPs (PCPConcrete (EImplPropCall (PCall v _ _ _)) [PCPColon vt, PCPIterate i it e, PCPBar e2]))))
    = proplist env' $ [ ("expression", PL (Ident "IteratorExpImpl"))
                      , ("name", PL op)
                      , ("iterator", PL [ ("variable", plVar (pathName v) (getSimpleType vt) Nothing)
                                        ])
                      , ("result", plVar (pathName v) (getSimpleType vt) (Just e))
                      , ("source", PL c)
                      , ("body", PL e2)
                      ]
    where
      env' = M.insert (pathName v) (getSimpleType vt) $
             M.insert ((\(Ident s) -> s) i) (getType it) env

  proplist env e@(EExplPropCall e1 PArrow (PCall (PathName [PName (Ident op)]) _ _ (PCPs (PCPConcrete e2 [PCPBar e3]))))
    | op `elem` ["collect", "select"]
    = proplist env' $ [ ("expression", PL (Ident "IteratorExpImpl"))
                      , ("name", PL (Ident op))
                      , ("iterator", PL [("variable", plVar it itType Nothing)])
                      , ("source", PL e1)
                      , ("body", PL e3)
                      ]
    where
      it = oclExpr e2 env
      itType = getInnerType (oclType e1 env)
      env' = M.insert it itType env

  proplist env e@(EExplPropCall e1 PDot (PCall p AtPre q NoPCP))
    = proplist env $ [ ("expression", PL (Ident "OperationCallExp"))
                     , ("name", PL (Ident "atPre"))
                     , ("referredOperation", plTyped "atPre" (oclType e env))
                     , ("contents", PL [("content", EExplPropCall e1 PDot (PCall p NoTE q NoPCP))])
                     ]

  proplist env e@(EExplPropCall e1 PDot (PCall p _ _ NoPCP))
    = proplist env $ [ ("expression", PL (Ident "PropertyCallExp"))
                     , ("source", PL e1)
                     , ("referredProperty", plTyped (pathName p) (oclType e env))
                     ]

  proplist env e@(EExplPropCall e1 _ (PCall op _ _ ppcp))
    = proplist env $ [ ("expression", PL (Ident "OperationCallExp"))
                     , ("name", PL (Ident ""))
                     , ("referredOperation", plTyped (pathName op) (oclType e env))
                     , ("contents", PL $ ("content", PL e1):getExpr ppcp)
                     ]
    where
      getExpr NoPCP = []
      getExpr (PCPs PCPNoDeclNoParam) = []
      getExpr (PCPs (PCPConcrete e2 _))
        = [("content", PL e2)]

  proplist env (ELit (LitNum (NumInt i)))
    = proplist env $ [ ("expression", PL (Ident "IntegerLiteralExp"))
                     , ("value", PL i)
                     ]

  proplist env (ELit (LitStr s))
    = proplist env $ [ ("expression", Ident "StringLiteralExp")
                     , ("value", Ident s)
                     ]

  proplist env (ELit LitBoolTrue)
    = proplist env $ [ ("expression", Ident "BooleanLiteralExp")
                     , ("value", Ident "true")
                     ]

  proplist env (ELit LitBoolFalse)
    = proplist env $ [ ("expression", Ident "BooleanLiteralExp")
                     , ("value", Ident "false")
                     ]

  proplist env (EIfExp (IfExp c t e))
    = proplist env $ [ ("expression", PL (Ident "IfExp"))
                     , ("condition", PL c)
                     , ("then", PL t)
                     , ("else", PL e)
                     ]

  proplist env ENull
    = proplist env $ [ ("expression", PL (Ident "UndefinedLiteralExp"))
                     , ("value", PL "null")
                     ]

  --proplist env (ELet ps e)
  --  = proplist env $ [

  proplist _ e = error ("Cannot parse constraint: " ++ show e)


plVar :: String -> Type -> Maybe Expression -> PropList
plVar n t init = PL [ ("expression", PL (Ident "Variable"))
                    , ("name", PL (Ident n))
                    , ("type", PL $ [ ("name", PL t)
                    --              , ("qualifiedName", PL)
                                    ] ++ maybe [] (\e -> [("initExpression", PL e)]) init)
                    ]

plTyped :: String -> Type -> PropList
plTyped n t = PL [ ("name", PL (Ident n))
                 , ("type", PL [ ("name", PL t)
                 --              , ("qualifiedName", PL)
                               ])
                 ]

proplistBinOp env e1 op e2
  = proplist env $ [ ("expression", PL (Ident "OperationCallExp"))
                   , ("name", PL (Ident ""))
                   , ("referredOperation", PL [ ("name", PL op)
                                              , ("type", PL [ ("name", PL (envType (show op) env))
                   --                                         , ("qualifiedName", PL)
                                                            ])
                                            ])
                   , ("contents", PL [ ("content", PL e1)
                                     , ("content", PL e2)
                                     ])
                   ]

instance ErlangPropList LogicalOperator where
  proplist env LAnd = proplist env (Ident "and")
  proplist env LOr  = proplist env (Ident "or")
  proplist env LXor = proplist env (Ident "xor")

instance ErlangPropList EqualityOperator where
  proplist env EEq  = proplist env (Ident "=")
  proplist env ENEq = proplist env (Ident "<>")

instance ErlangPropList AddOperator where
  proplist env AAdd = proplist env (Ident "+")
  proplist env ASub = proplist env (Ident "-")

instance ErlangPropList MultiplyOperator where
  proplist env MMult = proplist env (Ident "*")
  proplist env MDiv  = proplist env (Ident "/")

instance ErlangPropList LiteralCollection where
  proplist env (LCollection n [CIRange e1 e2])
    = proplist env $ [ ("expression", PL (Ident "CollectionLiteralExp"))
                     , ("kind", PL n)
                     , ("range", PL [ ("first", PL e1)
                                    , ("last", PL e2)
                                    ])
                     ]

instance ErlangPropList CollectionKind where
  proplist env k = proplist env (Ident (show k))

-- Class
instance ErlangPropList Class where
  proplist env (Class n as m)
    = proplist env' $ ("class", [ ("name", PL (Ident n))
                                , ("constraints", PL [ ("invariants", [] :: [Constraint]) ])
                                , ("attributes", PL as)
                                , ("operations", PL m)
                                ])
    where
      env' = M.insert "self" (TClass n) env

instance ErlangPropList Attr where
  proplist env (Attr (TVar n t))
    = proplist env $ ("attribute", [ ("name", PL (Ident n))
                                   , ("type", PL [("name", t)])
                                   ])

instance ErlangPropList Method where
  proplist env (Method n t ps [cs])
    = proplist env'' $ ("operation", [ ("name", PL (Ident n))
                                     , ("params", PL ps)
                                     , ("constraints", PL cs)
                                     ])
    where
      env' = M.insert "result" t env
      env'' = foldr (\p env -> uncurry M.insert (fromParam p) env) env' ps
      fromParam (Param (TVar n t)) = (n, t)

  proplist env (Method n _ p cs)
    = proplist env $ ("operation", [ ("name", PL (Ident n))
                                   , ("params", PL p)
                                   , ("constraints", PL cs)
                                   ])

instance ErlangPropList Param where
  proplist env (Param (TVar n t))
    = proplist env $ ("param", [ ("name", PL (Ident n))
                               , ("type", PL [("name", t)])
                               ])

instance ErlangPropList Type where
  proplist env t
    = doubleQuotes (go t)
    where
      go (TSimple ns)
        = proplist env (intercalate "::" ns)
      go (TClass c)
        = proplist env c
      go (TTuple fps)
        = text "Tuple" <> parens (sep (punctuate comma (map (\(TVar n t) -> proplist env n <> char ':' <> go t) fps)))
      go (TSeq t)
        = text "Sequence" <> parens (go t)
      go (TSet t)
        = text "Set" <> parens (go t)
      go (TBag t)
        = text "Bag" <> parens (go t)
      go (TColl t)
        = text "Collection" <> parens (go t)
