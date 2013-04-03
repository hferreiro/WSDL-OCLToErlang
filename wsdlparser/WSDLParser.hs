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


--XML2Class :: String -> Class
--XML2Class s = (Class "n" _)

xmlFile = "<?xml version=\"1.0\"?><Contents><StockQuote Symbol=\"PETR3\" Date=\"21-12-2010\" Time=\"13:20\" Price=\"23.02\" /><StockQuote Symbol=\"PETR4\" Date=\"21-12-2011\" Time=\"14:20\" Price=\"24.02\" /></Contents>"

--QName {qName = \"definitions\", qURI = Just \"http://schemas.xmlsoap.org/wsdl/\", qPrefix = Just \"wsdl\"}

parseFileTest f =
	let
			contents = parseXML f
			quotes  = concatMap (findElements $ simpleNameTag "definitions") (onlyElems contents)
			symbols = map (findAttr $ simpleNameAttr "wsdl") quotes
--			simpleNameTag t = QName t (Just _) (Just "wsdl")
			simpleNameAttr s = QName s Nothing (Just "xmlns")
	in print (show contents) >> print symbols


parseFile :: FilePath -> [Content]
parseFile = parseXML


getClassName :: [Content] -> [Maybe String]
getClassName cs = map (findAttr $ serviceAttr "name") quotes
	where
  	quotes = concatMap (findElements $ serviceTag "service") (onlyElems cs)


--schemaTag :: String -> QName
--schemaTag st = QName st (Just _) (Just "wsdl")

schemaAttr :: String -> QName
schemaAttr sa = QName sa Nothing (Just "xmlns")

serviceTag :: String -> QName
serviceTag st = QName st Nothing (Just "wsdl")

serviceAttr :: String -> QName
serviceAttr sa = QName sa Nothing Nothing
