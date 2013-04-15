module Text.WSDL.ClassParser ( wsdl2Class ) where

import Control.Monad ( msum )
import Data.Maybe ( catMaybes, fromJust, maybeToList )
import Text.XML.Light

import Data.Class

wsdl2Class :: String -> Class
wsdl2Class s = Class name methods
  where
    name = getClassName xml
    methods = getOperations xml

    xml = fromJust (parseXMLDoc s)

getClassName :: Element -> String
getClassName e = fromJust $ findAttr (defaultAttr "name") serviceT
  where
    serviceT = fromJust $ filterElementName (qNameP ("wsdl","service")) e

getOperations :: Element -> [Method]
getOperations e = do
  portType <- maybeToList $ childByTag ("wsdl","portType") e
  operation <- filterElementsName (qNameP ("wsdl","operation")) portType
  return (fromJust $ getOperation operation)

  where
    getOperation :: Element -> Maybe Method
    getOperation op = do
      name <- findAttr (defaultAttr "name") op
      typ <- getReturnType op
      let params = getParams op
      return (Method name typ params [])

    getReturnType :: Element -> Maybe String
    getReturnType op = do
      outputT <- childByTag ("wsdl","output") op
      ('n':'s':':':msgS) <- attrByTag "message" outputT
      msgT <- filterElement (qNameAttrValueP ("wsdl","message") ("name",msgS)) e
      partT <- childByTag ("wsdl","part") msgT
      ('n':'s':':':elementS) <- attrByTag "element" partT -- "ns:"
      elementT <- elementByTagAttr ("xs","element") ("name",elementS) e
      ('x':'s':':':typeS) <- msum $ recfindAttrBy (qNameP ("","type")) elementT -- "xs:"
      return typeS

    getParams :: Element -> [Param]
    getParams op =
      let params = fromJust $ do
                     inputT <- childByTag ("wsdl","input") op
                     ('n':'s':':':msgS) <- attrByTag "message" inputT
                     msgT <- filterElement (qNameAttrValueP ("wsdl","message") ("name",msgS)) e
                     partT <- childByTag ("wsdl","part") msgT
                     ('n':'s':':':elementS) <- attrByTag "element" partT -- "ns:"
                     elementT <- elementByTagAttr ("xs","element") ("name",elementS) e
                     sequenceT <- filterElementName (qNameP ("xs","sequence")) elementT
                     return (elChildren sequenceT)
      in catMaybes (map getParam params)

    getParam :: Element -> Maybe Param
    getParam p = do
      n <- findAttrBy (qNameP ("","name")) p
      ('x':'s':':':t) <- findAttrBy (qNameP ("","type")) p
      return (Param n t)


childByTag :: (String, String) -> Element -> Maybe Element
childByTag (p,t) e
  = filterChildName (qNameP (p,t)) e

elementByTagAttr :: (String, String) -> (String, String) -> Element -> Maybe Element
elementByTagAttr (p,t) (a,v) e
  = filterElement (qNameAttrValueP (p,t) (a,v)) e

recfindAttrBy :: (QName -> Bool) -> Element -> [Maybe String]
recfindAttrBy p e
  = [findAttrBy p e] ++ concatMap (recfindAttrBy p) (elChildren e)

attrByTag :: String -> Element -> Maybe String
attrByTag t e = findAttr (defaultAttr t) e

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

defaultAttr :: String -> QName
defaultAttr sa = QName sa Nothing Nothing
