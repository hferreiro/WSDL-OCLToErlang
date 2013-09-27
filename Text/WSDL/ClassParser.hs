module Text.WSDL.ClassParser ( wsdl2Class ) where

import Control.Monad ( msum )
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
       return (fromJust $ getOperation operation)

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
             cl = fromJust $ do
                    inputT <- childByTag ("wsdl","input") op
                    ('m':'s':'g':':':msgS) <- attrByTag "element" inputT -- "msg:"
                    xsdDef msgS defs
           C.Attr tv <- cAttrs cl
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
parseDefs e = (defs, tds)
  where
    (defs, tds) = foldr addDef ([],[]) (catMaybes maybeDefs)
    maybeDefs = map (parseType tds) elems
    elems = elChildren e

    addDef :: (Maybe Def, TDef) -> ([Def], [TDef]) -> ([Def], [TDef])
    addDef (Nothing, td) (ds, tds) = (ds, td:tds)
    addDef (Just df, td) (ds, tds) = (df:ds, td:tds)

    parseType :: [TDef] -> Element -> Maybe (Maybe Def, TDef)
    parseType tds e
      = do n <- attrByTag "name" e
           complexType <- elementByTag ("xsd","complexType") e
           let attrs = getAttrs complexType
           case attrs of
             [C.Attr (TVar _ t)] ->
               return (Nothing, (n,t))
             _ ->
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
