import WSDLParser
import System.IO
import Control.Monad

main :: IO ()
main = do
					wsdlFile <- readFile "MathUtils.wsdl"
					parseFileTest2 wsdlFile

--          wsdlFile <- readFile "MathUtils.wsdl"
--          print $ getClassName (parseFile wsdlFile)
