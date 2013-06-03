module Main where

import Control.Monad ( when )
import Text.PrettyPrint ( render )
import System.IO ( stdin, hGetContents )
import System.Directory ( doesFileExist )
import System.Environment ( getArgs, getProgName )
import System.FilePath ( (<.>), dropExtension )

import Data.Class ( addConstrs )
import Language.SurfaceOCL.OCL
import Text.ErlangPropList ( ErlangPropList(..) )
import Text.WSDL.ClassParser ( wsdl2Class )

type ParseFun a = [Token] -> Err a

runFile :: ParseFun OCLfile -> (FilePath, FilePath, FilePath) -> IO ()
runFile p (w, o, x)
  = do wc <- readFile w
       oc <- readFile o
       xc <- readFile x
       run p wc oc xc

run :: ParseFun OCLfile -> String -> String -> String -> IO ()
run p wc oc xc
  = let ts = myLexer oc
        cls = wsdl2Class wc xc
    in case p ts of
        Ok tree -> do let (OCLfile [Pack _ (Constraints cs)]) = tree
                          cls' = addConstrs cls cs
                      putStrLn $ render (proplist cls') ++ "."
        Bad s -> error ("Could not parse OCL file: " ++ s)

getFiles :: FilePath -> IO (FilePath, FilePath, FilePath)
getFiles f
  = do b <- doesFileExist w
       when (not b) (errorF w)
       b <- doesFileExist o
       when (not b) (errorF o)
       b <- doesFileExist x
       when (not b) (errorF x)
       return (w, o, x)
    where f' = dropExtension f
          (w, o, x) = (f' <.> "wsdl", f' <.> "ocl", f' <.> "xsd")
          errorF f = error $ "File '" ++ f ++ "' does not exist"

main :: IO ()
main = do args <- getArgs
          case args of
            []  -> error "Usage: Test file[.wsdl/.ocl]"
            f:_ -> getFiles f >>= runFile pOCLfile
