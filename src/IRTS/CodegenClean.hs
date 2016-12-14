{-# LANGUAGE OverloadedStrings #-}
module IRTS.CodegenClean
    ( codegenClean
    ) where

import Prelude hiding ((<$>))

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Defunctionalise
import Idris.Core.TT

import Data.Char
import Data.Text.Lazy (pack, unpack)
import Text.PrettyPrint.Leijen.Text hiding (string)
import System.IO

-- Helpers ---------------------------------------------------------------------

indentLevel :: Int
indentLevel = 4

blank :: Doc
blank = space

string :: String -> Doc
string = text . pack

-- Code Generator --------------------------------------------------------------

codegenClean :: CodeGenerator
codegenClean info = do
    let declarations = vsep $ map cgDecl (defunDecls info)
    let output = vsep [header, imports, helpers, declarations, start]
    withFile (outputFile info) WriteMode (`hPutDoc` output)
    where
        isFun (_, DFun{}) = True
        isFun (_, DConstructor{}) = False

header, imports, helpers, start :: Doc
header = "module Main"
imports = vsep
    [ "import StdEnv" ]
helpers = vsep
    [ ":: Value = .." ]
start = vsep
    [ "Start :: *World -> *World"
    , "Start world = runMain"
    ]

cgDecl :: (Name, DDecl) -> Doc
cgDecl (_, DConstructor name tag arity) = cgCtor name tag arity
cgDecl (_, DFun name args def) = cgFun name args def

cgCtor :: Name -> Int -> Int -> Doc
cgCtor name tag arity =
    --FIXME strictness
    blank <$>
    ":: Value |" <+> cgName name <+> hsep (replicate arity "Value")

cgFun :: Name -> [Name] -> DExp -> Doc
cgFun name args def =
    blank <$>
    "///" <+> string (show name) <$>
    cgName name <+> hsep (map (cgLoc . fst) (zip [0..] args)) <+> char '=' <$>
    indent indentLevel (cgExp def)

cgExp :: DExp -> Doc
--FIXME parens!
cgExp (DV (Glob name)) =
    cgName name
cgExp (DV (Loc idx)) =
    cgLoc idx
cgExp (DApp _istail name args) =
    cgApp name args
cgExp (DLet name def rest) =
    --FIXME should be strict always?
    char '#' <+> cgName name <+> char '=' <+> cgExp def <$>
    cgExp rest <$> empty
cgExp (DUpdate var def) =
    --FIXME what is this?
    "UPDATE " <+> cgExp def <$> empty
cgExp (DProj def idx) =
    cgExp def <+> brackets (int idx)
cgExp (DC _reloc_ _type_ name args) =
    cgApp name args
cgExp _ =
    "NOT IMPLEMENTED"

cgApp :: Name -> [DExp] -> Doc
cgApp name args = cgName name <+> hsep (map cgExp args)

cgVar :: LVar -> Doc
cgVar (Loc idx) = cgLoc idx
cgVar (Glob name) = cgName name

cgLoc :: Int -> Doc
cgLoc idx = "loc" <> int idx

cgName :: Name -> Doc
cgName name = "idris_" <> string (concatMap mangle $ showCG name)
    where
        mangle c
            | isAlpha c || isDigit c = [c]
            | otherwise = "_" ++ show (fromEnum c) ++ "_"
