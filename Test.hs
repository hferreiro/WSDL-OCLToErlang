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

runFile :: ParseFun OCLfile -> (FilePath, FilePath) -> IO ()
runFile p (w, o)
  = do wc <- readFile w
       oc <- readFile o
       run p wc oc

run :: ParseFun OCLfile -> String -> String -> IO ()
run p wc oc
  = let ts = myLexer oc
        cl = wsdl2Class wc
    in case p ts of
        Ok  tree -> do let m = constrsByMethod tree
                           cl'= addConstrs cl m
                       putStrLn $ render (proplist [cl']) ++ "."

getFiles :: FilePath -> IO (FilePath, FilePath)
getFiles f
  = do b <- doesFileExist w
       when (not b) (errorF w)
       b <- doesFileExist o
       when (not b) (errorF o)
       return (w, o)
    where f' = dropExtension f
          (w, o) = (f' <.> "wsdl", f' <.> "ocl")
          errorF f = error $ "File '" ++ f ++ "' does not exist"

main :: IO ()
main = do args <- getArgs
          case args of
            []  -> error "Usage: Test file[.wsdl/.ocl]"
            f:_ -> getFiles f >>= runFile pOCLfile
