module IRTS.CodegenClean
    ( codegenClean
    ) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Defunctionalise
import Idris.Core.TT

import Data.Char
import Data.List

codegenClean :: CodeGenerator
codegenClean info = do
    let declarations = concatMap cgDecl (defunDecls info)
    writeFile (outputFile info) $ unlines
        [ header
        , imports
        , helpers
        , declarations
        , start
        ]
    where
        isFun (_, DFun{}) = True
        isFun (_, DConstructor{}) = False

header, imports, helpers, start :: String
header = "module Main"
imports = unlines
    [ "import StdEnv" ]
helpers = unlines
    [ ":: Value = .." ]
start = unlines
    [ "Start :: *World -> *World"
    , "Start world = runMain"
    ]

cgDecl :: (Name, DDecl) -> String
cgDecl (_, DConstructor name tag arity) = cgCtor name tag arity
cgDecl (_, DFun name args def) = cgFun name args def

cgCtor :: Name -> Int -> Int -> String
cgCtor name tag arity =
    --FIXME strictness
    ":: Value | " ++ cgName name ++ unwords (replicate arity "Value")

cgFun :: Name -> [Name] -> DExp -> String
cgFun name args def =
    "/// " ++ show name ++ "\n" ++
    cgName name ++ " " ++ unwords (map (cgLoc . fst) (zip [0..] args)) ++ " = \n" ++ indent 1 (
    cgExp def)

cgExp :: DExp -> String
cgExp exp = "(" ++ cgBody exp ++ ")"
    where
        cgBody :: DExp -> String
        --FIXME parens!
        cgBody (DV (Glob name)) =
            cgName name
        cgBody (DV (Loc idx)) =
            cgLoc idx
        cgBody (DApp _istailcall_ name args) =
            cgApp name args
        cgBody (DLet name def rest) =
            --FIXME should be strict always?
            "# " ++ cgName name ++ " = " ++ cgExp def ++ "\n" ++
                cgExp rest ++ "\n"
        cgBody (DUpdate var def) =
            --FIXME what is this?
            "UPDATE " ++ cgExp def ++ "\n"
        cgBody (DProj def idx) =
            cgExp def ++ "[" ++ show idx ++ "]"
        cgBody (DC _reloc_ _type_ name args) =
            cgApp name args
        cgBody _ =
            "NOT IMPLEMENTED"

----

cgApp :: Name -> [DExp] -> String
cgApp name args = cgName name ++ " " ++ unwords (map cgExp args)

cgVar :: LVar -> String
cgVar (Loc idx) = cgLoc idx
cgVar (Glob name) = cgName name

cgLoc :: Int -> String
cgLoc idx = "loc" ++ show idx

cgName :: Name -> String
cgName name = "idris_" ++ concatMap mangleChar (showCG name)
    where
        mangleChar c
            | isAlpha c || isDigit c = [c]
            | otherwise = "_" ++ show (fromEnum c) ++ "_"

indent :: Int -> String -> String
indent n str = replicate (4 * n) ' ' ++ str
