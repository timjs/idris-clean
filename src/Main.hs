module Main where

import Idris.Core.TT
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
    mainProg <- elabMain
    ir <- compile (Via IBCFormat "clean") (output opts) (Just mainProg)
    runIO $ codegenClean ir

main :: IO ()
main = do
    opts <- getOpts
    if null (inputs opts)
        then showUsage
        else runMain (run opts)
