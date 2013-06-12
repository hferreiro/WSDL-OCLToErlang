{-# LANGUAGE ExistentialQuantification #-}
module Text.ErlangPropList ( ErlangPropList(..) ) where

import Text.PrettyPrint

import Data.Class
import Data.List ( intercalate )
import Language.OCL

class ErlangPropList a where
  proplist :: a -> Doc
  proplist _ = text "ErlangPropList instance not implemented"

  proplistList :: [a] -> Doc
  proplistList as = brackets $ sep (punctuate comma (map proplist as))

-- Instances
data PropList = forall a. ErlangPropList a => PL a

instance ErlangPropList PropList where
  proplist (PL a) = proplist a

instance ErlangPropList Char where
  proplistList s = text s

instance ErlangPropList Integer where
  proplist i = integer i

instance ErlangPropList a => ErlangPropList [a] where
  proplist = proplistList

instance (ErlangPropList a, ErlangPropList b) => ErlangPropList (a,b) where
  proplist (a,b) = braces $ hang (proplist a <> comma) 2 (proplist b)

-- OCL
instance ErlangPropList Ident where
  proplist (Ident s) = doubleQuotes (proplist s)

instance ErlangPropList PathName where
  proplist (PathName pn)
    = proplist $ Ident (intercalate "::" (map (\(PName (Ident n)) -> n) pn))

instance ErlangPropList Constraint where
  proplist (Constr _ cbs)
    = proplist $ [ ("preconditions", pre)
                 , ("postconditions", post)
                 , ("invariants", inv)
                 ]

    where
      pre = findBy Pre cbs
      post = findBy Post cbs
      inv = findBy Inv cbs

      findBy t []                      = []
      findBy t (c@(CB st _):cs)
        | t == st                      = c:findBy t cs
        | otherwise                    = findBy t cs
      findBy t (c@(CBNamed st _ _):cs)
        | t == st                      = c:findBy t cs
        | otherwise                    = findBy t cs

instance ErlangPropList FormalParameter where
  proplist (FP n ts)
    = proplist $ ("param", [ ("name", PL n)
                           , ("type", PL ts)
                           ])

instance ErlangPropList TypeSpecifier where
  proplist (TSsimple t) = proplist t

instance ErlangPropList SimpleTypeSpecifier where
  proplist (STSpec t) = proplist t

instance ErlangPropList ConstrBody where
  proplist (CB _ ocle) = proplist ("constraint", [ ("name", PL (Ident ""))
                                                 , ("body", PL ocle)
                                                 ])
  proplist (CBNamed _ n ocle) = proplist ("constraint", [ ("name", PL n)
                                                        , ("body", PL ocle)
                                                        ])

instance ErlangPropList Stereotype where
  proplist Pre = text "preconditions"
  proplist Post = text "postconditions"
  proplist Inv = text "invariants"

instance ErlangPropList Expression where
  proplist (EOpLog e1 op e2)
    = proplistBinOp e1 op e2

  proplist (EOpEq e1 op e2)
    = proplistBinOp e1 op e2

  proplist (EOpAdd e1 op e2)
    = proplistBinOp e1 op e2

  proplist (EOpMul e1 op e2)
    = proplistBinOp e1 op e2

  proplist (EImplPropCall (PCall n _ _ _))
    = proplist $ [ ("expression", PL (Ident "VariableExp"))
                 , ("referredVariable", PL [ ("expression", PL (Ident "Variable"))
                                           , ("name", PL n)
                                           --, ("type", PL [ ("name", PL)
                                           --              , ("qualifiedName", PL)
                                           --              ])
                                           ])
                 ]

  proplist (EExplPropCall (ELitColl c) PArrow (PCall op _ _ (PCPs (PCPConcrete (EImplPropCall (PCall v _ _ _)) [PCPColon vt, PCPIterate i it e, PCPBar e2]))))
    = proplist $ [ ("expression", PL (Ident "IterateExpImpl"))
                 , ("name", PL op)
                 , ("iterator", PL [ ("variable", [ ("expression", PL (Ident "Variable"))
                                                  , ("name", PL v)
                                                  , ("type", PL [ ("name", PL vt)
                                                  --              , ("qualifiedName", PL)
                                                                ])
                                                  ])
                                   ])
                 , ("result", PL [ ("expression", PL (Ident "Variable"))
                                 , ("name", PL i)
                                 , ("type", PL [ ("name", PL it)
                                               --, ("qualifiedName", PL)
                                               , ("initExpression", PL e)
                                               ])
                                 ])
                 , ("source", PL c)
                 , ("body", PL e2)
                 ]

  proplist (EExplPropCall e1 _ (PCall op _ _ ppcp))
    = proplist $ [ ("expression", PL (Ident "OperationCallExp"))
                 --, ("name", PL )
                 , ("referredOperation", PL [ ("name", PL op)
                                            --, ("type", PL [ ("name", PL)
                                            --              , ("qualifiedName", PL)
                                            --              ])
                                            ])
                 , ("contents", PL $ ("content", PL e1):getExpr ppcp)
                 ]
    where
      getExpr NoPCP = []
      getExpr (PCPs PCPNoDeclNoParam) = []
      getExpr (PCPs (PCPConcrete e2 _))
        = [("content", PL e2)]

  proplist (ELit (LitNum (NumInt i)))
    = proplist $ [ ("expression", PL (Ident "IntegerLiteralExp"))
                 , ("value", PL i)
                 ]

  proplist (ELit (LitStr s))
    = proplist $ [ ("expression", Ident "StringLiteralExp")
                 , ("value", Ident s)
                 ]

  proplist (ELit LitBoolTrue)
    = proplist $ [ ("expression", Ident "BooleanLiteralExp")
                 , ("value", Ident "true")
                 ]

  proplist (ELit LitBoolFalse)
    = proplist $ [ ("expression", Ident "BooleanLiteralExp")
                 , ("value", Ident "false")
                 ]

  proplist (EIfExp (IfExp c t e))
    = proplist $ [ ("expression", PL (Ident "IfExp"))
                 , ("condition", PL c)
                 , ("then", PL t)
                 , ("else", PL e)
                 ]

  proplist ENull
    = proplist $ [ ("expression", PL (Ident "UndefinedLiteralExp"))
                 , ("value", PL "null")
                 ]

  --proplist (ELet ps e)
  --  = proplist $ [

  proplist e = error ("Cannot parse constraint: " ++ show e)


proplistBinOp e1 op e2
  = proplist $ [ ("expression", PL (Ident "OperationCallExp"))
               --, ("name", PL )
               , ("referredOperation", PL [ ("name", PL op)
                                          --, ("type", PL [ ("name", PL)
                                          --              , ("qualifiedName", PL)
                                          --              ])
                                          ])
               , ("contents", PL [ ("content", PL e1)
                                 , ("content", PL e2)
                                 ])
               ]

instance ErlangPropList LogicalOperator where
  proplist LAnd = proplist (Ident "and")
  proplist LOr  = proplist (Ident "or")
  proplist LXor = proplist (Ident "xor")

instance ErlangPropList EqualityOperator where
  proplist EEq  = proplist (Ident "=")
  proplist ENEq = proplist (Ident "<>")

instance ErlangPropList AddOperator where
  proplist AAdd = proplist (Ident "+")
  proplist ASub = proplist (Ident "-")

instance ErlangPropList MultiplyOperator where
  proplist MMult = proplist (Ident "*")
  proplist MDiv  = proplist (Ident "/")

instance ErlangPropList LiteralCollection where
  proplist (LCollection n [CIRange e1 e2])
    = proplist $ [ ("expression", PL (Ident "CollectionLiteralExp"))
                 , ("kind", PL n)
                 , ("range", PL [ ("first", PL e1)
                                , ("last", PL e2)
                                ])
                 ]

instance ErlangPropList CollectionKind where
  proplist k = proplist (Ident (show k))

-- Class
instance ErlangPropList Class where
  proplist (Class n a m)
    = proplist $ ("class", [ ("name", PL (Ident n))
                           , ("attributes", PL a)
                           , ("operations", PL m)
                           ])

instance ErlangPropList Attr where
  proplist (Attr (TVar n t))
    = proplist $ ("attribute", [ ("name", PL (Ident n))
                               , ("type", PL [("name", t)])
                               ])

instance ErlangPropList Method where
  proplist (Method n t p [cs])
    = proplist $ ("operation", [ ("name", PL (Ident n))
                               , ("params", PL p)
                               , ("constraints", PL cs)
                               ])
  proplist (Method n t p cs)
    = proplist $ ("operation", [ ("name", PL (Ident n))
                               , ("params", PL p)
                               , ("constraints", PL cs)
                               ])

instance ErlangPropList Param where
  proplist (Param (TVar n t))
    = proplist $ ("param", [ ("name", PL (Ident n))
                           , ("type", PL [("name", t)])
                           ])

instance ErlangPropList Type where
  proplist t
    = doubleQuotes (go t)
    where
      go (TSimple ns)
        = proplist (intercalate "::" ns)
      go (TClass c)
        = proplist c
      go (TTuple fps)
        = text "Tuple" <> parens (sep (punctuate comma (map (\(TVar n t) -> proplist n <> char ':' <> go t) fps)))
      go (TSeq t)
        = text "Sequence" <> parens (go t)
      go (TSet t)
        = text "Set" <> parens (go t)
      go (TBag t)
        = text "Bag" <> parens (go t)
      go (TColl t)
        = text "Collection" <> parens (go t)
