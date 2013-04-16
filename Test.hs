module Main where

import Text.PrettyPrint ( render )
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import Data.Class ( addConstrs )
import Language.SurfaceOCL.OCL
import Text.ErlangPropList ( ErlangPropList(..) )
import Text.WSDL.ClassParser ( wsdl2Class )

type ParseFun a = [Token] -> Err a

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 0 then putStrLn s else return ()

runFile :: (ErlangPropList a, {-Print a,-} Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

runFiles :: ParseFun OCLfile -> FilePath -> FilePath -> IO ()
runFiles p w o = do
  wc <- readFile w
  oc <- readFile o
  runs p wc oc

run :: (ErlangPropList a, {-Print a,-} Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLexer s in
              case p ts of
                Bad s    -> do putStrLn "Parse Failed."
                               putStrV v "Tokens:"
                               putStrV v $ show ts
                               putStrLn s
                Ok  tree -> do showTree v tree

runs :: ParseFun OCLfile -> String -> String -> IO ()
runs p wc oc
  = let ts = myLexer oc
        cl = wsdl2Class wc
    in case p ts of
        Ok  tree -> do let m = constrsByMethod tree
                           cl'= addConstrs cl m
                       putStrLn $ render (proplist [cl']) ++ "."

showTree :: (ErlangPropList a, Show a{-, Print a-}) => Verbosity -> a -> IO ()
showTree v tree =
  case v of
    1 -> do putStrV v $ "[Abstract Syntax]\n" ++ show tree
            --putStrV v $ "[Linearized tree]\n" ++ printTree tree
    2 -> do putStrLn (render $ proplist tree)
    _ -> return ()

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 0 pOCLfile
            "-v":fs -> mapM_ (runFile 1 pOCLfile) fs
            "-e":fs -> mapM_ (runFile 2 pOCLfile) fs
            ["-w",w,o] -> runFiles pOCLfile w o
            fs -> mapM_ (runFile 0 pOCLfile) fs
