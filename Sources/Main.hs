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
    scds <- allSourceDirs
    runIO $ print scds
    loadInputs (inputs opts) Nothing
    showContext
    mainProg <- elabMain
    runIO $ print mainProg
    ir <- compile (Via IBCFormat "clean") (output opts) (Just mainProg)
    usedpos <- getUsedpos
    runIO $ codegenClean usedpos ir

getUsedpos :: Idris [(Name,UsageInformation)]
getUsedpos = do
    istate <- getIState
    let callgraph = toAlist $ idris_callgraph istate
    let ttdefs = definitions $ tt_ctxt istate
    pure [ (name, map fst $ usedpos info) | (name, info) <- callgraph, isTyDecl $ lookupCtxtExact name ttdefs ]
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
    runIO $ print "showInterfaces begin ------------------------------------"
    runIO $ print (idris_interfaces i)
    runIO $ print "showInterfaces end  -------------------------------------"
    runIO $ print "showRecords begin ------------------------------------"
    runIO $ print (idris_records i)
    runIO $ print "showRecords end  -------------------------------------"
    runIO $ print "showDsls begin ------------------------------------"
    runIO $ print (idris_dsls i)
    runIO $ print "showDsls end  -------------------------------------"
    runIO $ print "showDataTypes begin ------------------------------------"
    runIO $ print (idris_datatypes i)
    runIO $ print "showDataTypes end  -------------------------------------"
    runIO $ print "showCallGraph begin ------------------------------------"
    runIO $ print (idris_callgraph i)
    runIO $ print "showCallGraph end  -------------------------------------"
    runIO $ print "showTyinfodata begin ------------------------------------"
    runIO $ print (idris_tyinfodata i)
    runIO $ print "showTyinfodata end  -------------------------------------"
    runIO $ print "showFninfo begin ------------------------------------"
    runIO $ print (idris_fninfo i)
    runIO $ print "showFninfo end  -------------------------------------"
    runIO $ print "showName begin ------------------------------------"
    runIO $ print (idris_name i)
    runIO $ print "showName end  -------------------------------------"
    return ()
