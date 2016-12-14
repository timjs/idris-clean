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
import Data.Text.Lazy (Text, pack, unpack)
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
    cgName name <+> hsep (map cgName args) <+> char '=' <$>
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
    cgExp rest <$>
    blank
cgExp (DUpdate var def) =
    --FIXME what is this?
    "UPDATE " <+> cgExp def <$> empty
cgExp (DProj def idx) =
    cgExp def <+> brackets (int idx)
cgExp (DC _reloc _tag name args) =
    --FIXME optimize to Int for argless ctors
    cgApp name args
cgExp (DCase _casetype exp alts) =
    cgCase exp alts
cgExp (DChkCase exp alts) =
    cgCase exp alts
cgExp (DConst const) =
    cgConst const
cgExp (DOp prim exps) =
    cgPrim prim (map cgExp exps)
cgExp DNothing =
    empty
cgExp (DError msg) =
    "error" <+> string msg
cgExp e =
    cgUnsupported "expression" e

cgCase :: DExp -> [DAlt] -> Doc
cgCase exp alts =
    "case" <+> cgExp exp <+> "of" <$>
    indent 4 (vsep (map cgAlt alts))

cgAlt :: DAlt -> Doc
cgAlt alt = cgUnsupported "casealt" alt

cgApp :: Name -> [DExp] -> Doc
cgApp name args = cgName name <+> hsep (map cgExp args)

cgConst :: Const -> Doc
cgConst (I i) = int i
cgConst (Fl d) = double d
cgConst (Ch c) = squotes $ char c
cgConst (Str s) = dquotes $ string s
cgConst c = cgUnsupported "constant" c

cgInfix, cgPrefix :: Text -> [Doc] -> Doc
cgInfix op [left, right] = parens $ left <+> text op <+> right
cgPrefix op args = parens $ text op <+> hsep args

cgPrim :: PrimFn -> [Doc] -> Doc
cgPrim (LPlus _) = cgInfix "+"
cgPrim (LMinus _) = cgInfix "-"
cgPrim (LTimes _) = cgInfix "*"
cgPrim (LUDiv _) = cgInfix "/"
cgPrim (LSDiv _) = cgInfix "/"
cgPrim (LURem _) = cgInfix "rem"
cgPrim (LSRem _) = cgInfix "rem"

cgPrim (LAnd _) = cgInfix "bitand"
cgPrim (LOr _) = cgInfix "bitor"
cgPrim (LXOr _) = cgInfix "bitxor"
cgPrim (LSHL _) = cgInfix "<<"
cgPrim (LASHR _) = cgInfix ">>"
cgPrim (LLSHR _) = cgInfix ">>"  --FIXME
--cgPrim (LCompl _) = \[x] -> text "~" <> x

cgPrim (LEq _) = cgInfix "=="
cgPrim (LLt _) = cgInfix "<"
cgPrim (LSLt _) = cgInfix "<"
cgPrim (LLe _) = cgInfix "<="
cgPrim (LSLe _) = cgInfix "<="
cgPrim (LGt _) = cgInfix ">"
cgPrim (LSGt _) = cgInfix ">"
cgPrim (LGe _) = cgInfix ">="
cgPrim (LSGe _) = cgInfix ">="

-- cgPrim (LSExt _ _) = head
-- cgPrim (LZExt _ _) = head
-- cgPrim (LTrunc _ _) = head
-- cgPrim (LBitCast _ _) = head

cgPrim (LChInt _) = cgPrefix "toInt"
cgPrim (LIntCh _) = cgPrefix "fromInt"

cgPrim (LIntStr _) = cgPrefix "toString"
cgPrim (LStrInt _) = cgPrefix "fromString"
cgPrim LStrConcat = cgInfix "+++"
cgPrim LStrLt = cgInfix "<"
cgPrim LStrEq = cgInfix "=="

--cgPrim  LStrRev    = cgPrefix "reverse"
--cgPrim  LStrCons   = cgPrefix "cons"
--cgPrim  LStrHead   = \[x] -> x ! "0"
--cgPrim  LStrTail   = \[x] -> x ! "1:"
--cgPrim  LStrIndex  = \[x,i] -> x <> brackets i
--cgPrim  LStrLen    = cgPFun "len"
--cgPrim LStrSubstr = \[ofs,len,s] -> s <> brackets (ofs <> colon <> cgInfix "+" [ofs,len])

cgPrim LWriteStr = \[_world, str] ->
    "fwrites" <+> str <+> "stdio" <$>
    "fwrites" <+> dquotes (text "\\n") <+> "stdio"
cgPrim LReadStr = \[_world] ->
    "freadline" <+> "stdio"

--cgPrim (LExternal n) = cgExtern $ show n

cgPrim (LIntFloat _) = cgPrefix "toReal"
cgPrim (LFloatInt _) = cgPrefix "fromReal"
cgPrim LFloatStr = cgPrefix "toString"
cgPrim LStrFloat = cgPrefix "fromString"

cgPrim LFExp = cgPrefix "exp"
cgPrim LFLog = cgPrefix "log"
cgPrim LFSin = cgPrefix "sin"
cgPrim LFCos = cgPrefix "cos"
cgPrim LFTan = cgPrefix "tan"
cgPrim LFASin = cgPrefix "asin"
cgPrim LFACos = cgPrefix "acos"
cgPrim LFATan = cgPrefix "atan"
cgPrim LFSqrt = cgPrefix "sqrt"
cgPrim LFFloor = cgPrefix "floor"
cgPrim LFCeil  = cgPrefix "ceil"
cgPrim LFNegate = cgPrefix "~" -- \[x] -> text "~" <> x

cgPrim f = \_args -> cgUnsupported "primitive" f

cgVar :: LVar -> Doc
cgVar (Loc idx) = cgLoc idx
cgVar (Glob name) = cgName name

cgLoc :: Int -> Doc
cgLoc idx = "loc" <> int idx

cgName :: Name -> Doc
cgName name = "idris_" <> string (concatMap mangle $ showCG name)
    where
        mangle c
            | isAlpha c || isDigit c || isUnderscore c = [c]
            | otherwise = "_" ++ show (fromEnum c) ++ "_"
        isUnderscore c = c == '_'

cgUnsupported :: Show a => String -> a -> Doc
cgUnsupported desc val =
    text "UNSUPPORTED" <+> string (show val)
