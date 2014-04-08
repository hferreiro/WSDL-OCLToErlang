module Text.WSDL.ClassParser ( wsdl2Class ) where

import Control.Monad ( msum )
import Data.List ( partition )
import Data.Maybe ( catMaybes, fromJust, maybeToList )
import Text.XML.Light

import Data.Class as C

wsdl2Class :: String -> String -> [Class]
wsdl2Class s x
  = do (n,i) <- getIfaceNames xml
       let methods = getOperations i defs
       --    attrs = getAttributes i
       return (Class n [] methods)
    ++ xsdCls defs
  where
    defs = parseDefs xsd

    xml = fromJust (parseXMLDoc s)
    xsd = fromJust (parseXMLDoc x)

-- return class name and interface element
getIfaceNames :: Element -> [(String, Element)]
getIfaceNames e
  = do serviceT <- elementsByTag ("wsdl","service") e
       let (n,i)
             = fromJust
                 (do n <- attrByTag "name" serviceT
                     't':'n':'s':':':i <- attrByTag "interface" serviceT
                     e <- elementByTagAttr ("wsdl","interface") ("name",i) e
                     return (n,e))
       return (n,i)

getOperations :: Element -> XSDDefs -> [Method]
getOperations e defs
  = do operation <- elementsByTag ("wsdl","operation") e
       maybeToList (getOperation operation)

  where
    getOperation :: Element -> Maybe Method
    getOperation op
      = do name <- attrByTag "name" op
           ret <- getReturn op
           let params = getParams op
           return (Method name ret params [])

    getReturn :: Element -> Maybe Type
    getReturn op
      = do outputT <- childByTag ("wsdl","output") op
           ('m':'s':'g':':':msgS) <- attrByTag "element" outputT -- "msg:"
           xsdType msgS defs

    getParams :: Element -> [Param]
    getParams op
      = do let
             attrs = concat . maybeToList $ do
                       inputT <- childByTag ("wsdl","input") op
                       ('m':'s':'g':':':msgS) <- attrByTag "element" inputT -- "msg:"
                       let cl = xsdDef msgS defs
                       cAttrs `fmap` cl
           C.Attr tv <- attrs
           return (Param (TVar (tvName tv) (tvType tv)))


type XSDDefs = ([Def], [TDef])
type Def = (String, Class)
type TDef = (String, Type)

xsdType:: String -> XSDDefs -> Maybe Type
xsdType t defs = lookup t (snd defs)

xsdDef :: String -> XSDDefs -> Maybe Class
xsdDef c defs = lookup c (fst defs)

xsdCls :: XSDDefs -> [Class]
xsdCls (defs,_) = map snd defs

parseDefs :: Element -> XSDDefs
parseDefs e = replace (defs, tds)
  where
    (defs, tds) = foldr addDef ([],[]) (catMaybes maybeDefs)
    maybeDefs = map parseType elems
    elems = elChildren e

    addDef :: (Maybe Def, TDef) -> ([Def], [TDef]) -> ([Def], [TDef])
    addDef (Nothing, td) (ds, tds) = (ds, td:tds)
    addDef (Just df, td) (ds, tds) = (df:ds, td:tds)

    parseType :: Element -> Maybe (Maybe Def, TDef)
    parseType e
      = do n <- attrByTag "name" e
           complexType <- elementByTag ("xsd","complexType") e
           let attrs = getAttrs complexType
           return $ (Just (n, Class n attrs []), (n, TClass n))
      where
        getAttrs :: Element -> [C.Attr]
        getAttrs t
          = let attrs = elementsByTag ("xsd","element") t ++
                        elementsByTag ("xsd","attribute") t
            in catMaybes (map toAttr attrs)

        toAttr :: Element -> Maybe C.Attr
        toAttr e
          = do n <- attrByTag "name" e
               typeT <- attrByTag "type" e
               t <- case typeT of
                      'x':'s':'d':':':t' ->
                        return (TSimple [t'])
                      't':'n':'s':':':t' ->
                        if attrByTag "maxOccurs" e == Just "unbounded"
                        then return (TSeq (TSimple [t']))
                        else lookup t' tds
                      _ -> fail ""
               return $ C.Attr (TVar n t)

    -- Find type definitions which are just type synomyms (single-attribute
    -- classes with either a sequence type of an attribute named 'return'),
    -- remove them and propagate the changes
    replace :: XSDDefs -> XSDDefs
    replace (defs, tds)
      = (defs', tds')
      where
        (sAttr, someDefs) = partition singleAttr defs
        singleAttr (_, Class _ [C.Attr (TVar _ (TSeq _))] _) = True
        singleAttr (_, Class _ [C.Attr (TVar "return" _)] _) = True
        singleAttr _                                         = False

        (changed, tds') = foldr (clean newTds) ([],[]) tds
        newTds = map toDef sAttr
        toDef (n, Class _ [C.Attr (TVar _ t)] _) = (n,t)
        clean cls d@(n,t) (chd, tds)
          = case lookup n cls of
              Just t' -> ((t,t'):chd, (n,t'):tds)
              Nothing -> (chd, d:tds)

        defs' = map change someDefs
          where
            change (n, Class n' attrs m)
              = (n, Class n' (map newType attrs) m)
            newType a@(C.Attr (TVar n t))
              = case lookup t changed of
                  Just t' -> C.Attr (TVar n t')
                  Nothing -> a

--
childByTag :: (String, String) -> Element -> Maybe Element
childByTag (p,t) e
  = filterChildName (qNameP (p,t)) e

elementByTag :: (String, String) -> Element -> Maybe Element
elementByTag (p,t) e
  = filterElementName (qNameP (p,t)) e

elementsByTag :: (String, String) -> Element -> [Element]
elementsByTag (p,t) e
  = filterElementsName (qNameP (p,t)) e

elementByTagAttr :: (String, String) -> (String, String) -> Element -> Maybe Element
elementByTagAttr (p,t) (a,v) e
  = filterElement (qNameAttrValueP (p,t) (a,v)) e

qNameAttrValueP :: (String, String) -> (String, String) -> Element -> Bool
qNameAttrValueP (p,n) (a,v) e@(Element { elName = QName n' _ (Just p') })
  | n == n' && p == p' = findAttrBy (qNameP ("",a)) e == Just v
  | otherwise          = False

qNameP :: (String, String) -> QName -> Bool
qNameP ("",n) (QName n' _ Nothing)
  | n == n' = True
qNameP (p,n)  (QName n' _ (Just p'))
  | n == n' && p == p' = True
qNameP _ _ = False

attrByTag :: String -> Element -> Maybe String
attrByTag t e = findAttr (QName t Nothing Nothing) e
