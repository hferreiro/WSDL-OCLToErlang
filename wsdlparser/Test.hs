import WSDLParser
import System.IO
import Control.Monad

main :: IO ()
main = do
					wsdlFile <- readFile "MathUtils.wsdl"
					parseFileTest wsdlFile
--					print (getOperations (parseFile wsdlFile))
--					print (getClassName (parseFile wsdlFile))
