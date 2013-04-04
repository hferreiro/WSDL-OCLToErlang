module WSDLParser ( wsdl2Class ) where

import Control.Applicative
import Data.Maybe ( catMaybes, fromJust )
import Text.XML.Light

import Debug.Trace

data Class = Class { cName :: String
                   , cMethods :: [Method]
                   }
  deriving ( Show )

data Method = Method { mName :: String
                     , mType :: String
                     , mParams :: [Param]
                     --, preconditions :: [Constraint]
                     --, postconditions :: [Constraint]
                     --, invariants :: [Constraint]
                     }
  deriving ( Show )

data Param = Param { pName :: String
                   , pType :: String
                   }
  deriving ( Show )

data Operation = Operation { oName :: String
                           , oInput :: String
                           , oOutput :: String
                           }
  deriving ( Show )

wsdl2Class :: String -> Class
wsdl2Class s = Class name methods
  where
    name = getClassName xml
    methods = getOperations xml

    xml = parseXML s

getClassName :: [Content] -> String
getClassName cs = head $ catMaybes $ map (findAttr $ defaultAttr "name") quotes
  where
    quotes = concatMap (findElements (serviceTag cs)) (onlyElems cs)

getOperations :: [Content] -> [Method]
getOperations cs = catMaybes $ map getOperation quotes
  where
    quotes = concatMap (findElements (operationTag cs)) (filterPortType cs)

    getOperation :: Element -> Maybe Method
    getOperation q = do name <- findAttr (defaultAttr "name") q
                        typ <- getReturnType q
                        params <- Just []
                        return $ Method name typ params

    getReturnType :: Element -> Maybe String
    getReturnType q = return (head o)
      where
        out = head (childrenByTag uri "output" q)
        msg = fromJust $ attrByTag "message" out
        --o = filterElementName isResponse (childrenByTag uri "message" cs)
        o = filter isResponse (concatMap (filterChildrenName (qNameNameP "message")) (onlyElems cs))

        isResponse e = case attrByTag "name" e of
                         Nothing -> False
                         Just n  -> n == drop 3 msg

    uri = getSchema cs

t a = trace a a

filterPortType :: [Content] -> [Element]
filterPortType cs = concatMap (findChildren (portTypeTag cs)) (onlyElems cs)

getSchema :: [Content] -> Maybe String
getSchema cs = head $ map (findAttr $ (schemaAttr "wsdl")) quotes
  where
     quotes = concatMap (filterElementsName $ isSchemaQName) (onlyElems cs)

--Tags
childrenByTag :: Maybe String -> String -> Element -> [Element]
childrenByTag uri t e = findChildren (tag uri t) e

attrByTag :: String -> Element -> Maybe String
attrByTag t e = findAttr (defaultAttr t) e

qNameNameP :: String -> QName -> Bool
qNameNameP s (QName n _ _)
  | s == n    = True
  | otherwise = False

tag :: Maybe String -> String -> QName
tag uri t = QName t uri (Just "wsdl")

operationTag :: [Content] -> QName
operationTag cs = QName "operation" (getSchema cs) (Just "wsdl")

portTypeTag :: [Content] -> QName
portTypeTag cs = QName "portType" (getSchema cs) (Just "wsdl")

isSchemaQName :: QName -> Bool
isSchemaQName (QName "definitions" _ (Just "wsdl")) = True
isSchemaQName _ = False

schemaAttr :: String -> QName
schemaAttr sa = QName sa Nothing (Just "xmlns")

serviceTag :: [Content] -> QName
serviceTag cs = QName "service" (getSchema cs) (Just "wsdl")

defaultAttr :: String -> QName
defaultAttr sa = QName sa Nothing Nothing
