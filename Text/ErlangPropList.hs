{-# LANGUAGE ExistentialQuantification #-}
module Text.ErlangPropList ( ErlangPropList(..) ) where

import Text.PrettyPrint

import Data.Class
import Language.SurfaceOCL.OCL

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
  proplist (Ident s) = doubleQuotes (text s)

instance ErlangPropList PathName where
  proplist (PathName [PName n]) = proplist n

instance ErlangPropList OCLfile where
  proplist (OCLfile ps) = proplist ps

instance ErlangPropList Package where
  proplist (Pack (PackName c) cs)
    = proplist $ ("class", [ ("name", PL c)
                           , ("operations", PL cs)
                           ])

instance ErlangPropList OCLExpressions where
  proplist (Constraints cs) = proplist cs

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

instance ErlangPropList OCLExpression where
  proplist (OCLExp e) = proplist e

instance ErlangPropList Expression where
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

  proplist (EExplPropCall e1 PDot (PCall op _ _ (PCPs (PCPConcrete e2 _))))
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

  proplist (ELit (LitNum (NumInt i)))
    = proplist $ [ ("expression", PL (Ident "IntegerLiteralExp"))
                 , ("value", PL i)
                 ]

  proplist (EIfExp (IfExp c t e))
    = proplist $ [ ("expression", PL (Ident "IfExp"))
                 , ("condition", PL c)
                 , ("then", PL t)
                 , ("else", PL e)
                 ]

  proplist e = text (show e)


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
  proplist (Class n m)
    = proplist $ ("class", [ ("name", PL (Ident n))
                           , ("operations", PL m)
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
  proplist (Param n t)
    = proplist $ ("param", [ ("name", PL (Ident n))
                           , ("type", PL [("name", Ident t)])
                           ])
