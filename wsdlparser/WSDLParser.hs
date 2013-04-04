module WSDLParser where

import Text.XML.Light

data Class = Class { cName :: String
                   , methods :: [Method]
                   }

data Method = Method { mName :: String
                     , mType :: String
                     , params :: [Param]
                     --, preconditions :: [Constraint]
                     --, postconditions :: [Constraint]
                     --, invariants :: [Constraint]
                     }

data Param = Param { pName :: String
                   , pType :: String
									 }

data Operation = Operation { oName :: String
                           , input :: String
                           , output :: String
                           }

--XML2Class :: String -> Class
--XML2Class s = (Class "n" _)

xmlFile = "<?xml version=\"1.0\"?><Contents><StockQuote Symbol=\"PETR3\" Date=\"21-12-2010\" Time=\"13:20\" Price=\"23.02\" /><StockQuote Symbol=\"PETR4\" Date=\"21-12-2011\" Time=\"14:20\" Price=\"24.02\" /></Contents>"

parseFileTest f =
	let
	    contents = parseXML f
	    quotes  = concatMap (findChildren $ simpleName "portType") (onlyElems contents)
	    simpleName p = QName p (Just "http://schemas.xmlsoap.org/wsdl/") (Just "wsdl")
	in
	    print quotes


parseFile :: FilePath -> [Content]
parseFile = parseXML

getOperations :: [Content] -> [Maybe String]
getOperations cs = map (findAttr $ (defaultAttr "name")) quotes
  where
    quotes = concatMap (findElements (operationTag cs)) (filterPortType cs)


filterPortType :: [Content] -> [Element]
filterPortType cs = concatMap (findChildren (portTypeTag cs)) (onlyElems cs)


getClassName :: [Content] -> [Maybe String]
getClassName cs = map (findAttr $ defaultAttr "name") quotes
	where
    quotes = concatMap (findElements (serviceTag cs)) (onlyElems cs)


getSchema :: [Content] -> Maybe String
getSchema cs = head $ map (findAttr $ (schemaAttr "wsdl")) quotes
  where
     quotes = concatMap (filterElementsName $ isSchemaQName) (onlyElems cs)


--Tags
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
