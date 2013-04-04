import WSDLParser
import System.IO
import Control.Monad

main :: IO ()
main = do
  wsdlFile <- readFile "MathUtils.wsdl"
  putStrLn (show (wsdl2Class wsdlFile))
