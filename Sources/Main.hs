module Main where

import Idris.Core.TT
import Idris.Core.Evaluate
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL
import Idris.Main
import Idris.ModeCommon

import IRTS.Compiler
import IRTS.CodegenClean

import System.Environment
import System.Exit

import Paths_idris_clean

data Opts = Opts
    { inputs :: [FilePath]
    , output :: FilePath }

showUsage = do
    putStrLn "Usage: idris-codegen-clean <ibc-files> [-o <output-file>]"
    exitSuccess

getOpts :: IO Opts
getOpts = do
    xs <- getArgs
    return $ process (Opts [] "out.icl") xs
    where
        process opts ("-o":o:xs) = process (opts { output = o }) xs
        process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
        process opts [] = opts

run :: Opts -> Idris ()
run opts = do
    elabPrims
    loadInputs (inputs opts) Nothing
    --showContext
    mainProg <- elabMain
    ir <- compile (Via IBCFormat "clean") (output opts) (Just mainProg)
    usedpos <- getUsedpos
    runIO $ codegenClean usedpos ir

getUsedpos :: Idris [(Name,[Int])]
getUsedpos = do
    istate <- getIState
    let callgraph = toAlist $ idris_callgraph istate
    let ttdefs = definitions $ tt_ctxt istate
    pure [ (name, map fst $ usedpos info) | (name, info) <- callgraph ]--, isTyDecl $ lookupCtxtExact name ttdefs ]
    where
        isTyDecl :: Maybe TTDecl -> Bool
        isTyDecl (Just (TyDecl _ _,_,_,_,_,_)) = True
        isTyDecl _ = False

main :: IO ()
main = do
    opts <- getOpts
    if null (inputs opts)
        then showUsage
        else runMain (run opts)

showContext :: Idris ()
showContext = do
    i <- getIState
    runIO $ putStrLn "showInterfaces begin ------------------------------------"
    runIO $ print (idris_interfaces i)
    runIO $ putStrLn "showInterfaces end  -------------------------------------"
    runIO $ putStrLn "showRecords begin ------------------------------------"
    runIO $ print (idris_records i)
    runIO $ putStrLn "showRecords end  -------------------------------------"
    runIO $ putStrLn "showDsls begin ------------------------------------"
    runIO $ print (idris_dsls i)
    runIO $ putStrLn "showDsls end  -------------------------------------"
    runIO $ putStrLn "showDataTypes begin ------------------------------------"
    runIO $ print (idris_datatypes i)
    runIO $ putStrLn "showDataTypes end  -------------------------------------"
    runIO $ putStrLn "showCallGraph begin ------------------------------------"
    runIO $ print (idris_callgraph i)
    runIO $ putStrLn "showCallGraph end  -------------------------------------"
    runIO $ putStrLn "showTyinfodata begin ------------------------------------"
    runIO $ print (idris_tyinfodata i)
    runIO $ putStrLn "showTyinfodata end  -------------------------------------"
    runIO $ putStrLn "showFninfo begin ------------------------------------"
    runIO $ print (idris_fninfo i)
    runIO $ putStrLn "showFninfo end  -------------------------------------"
    runIO $ putStrLn "showName begin ------------------------------------"
    runIO $ print (idris_name i)
    runIO $ putStrLn "showName end  -------------------------------------"
    return ()
