{-# LANGUAGE OverloadedStrings #-}
module IRTS.CodegenClean
    ( codegenClean
    ) where

import Prelude hiding ((<$>))

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Defunctionalise
import Idris.Core.TT

import Numeric (showHex)
import Data.Char
import Data.List
import Data.Text.Lazy (Text, pack, unpack)
import System.IO
import System.FilePath

import Text.PrettyPrint.Leijen.Text hiding (string)

-- Helpers ---------------------------------------------------------------------

indentlevel :: Int
indentlevel = 4

blank :: Doc
blank = space

string :: String -> Doc
string = text . pack

-- Main and Prelude ------------------------------------------------------------

codegenClean :: CodeGenerator
codegenClean info = do
    let (funcs, ctors) = partition isFun (defunDecls info)
    let output = vsep
            [ cgModule (takeBaseName $ outputFile info)
            , cgImports
            , cgConstructors ctors
            , cgFunctions funcs
            , cgStart
            ]
    withFile (outputFile info) WriteMode (`hPutDoc` output)
    where
        isFun (_, DFun{}) = True
        isFun (_, DConstructor{}) = False

cgModule :: String -> Doc
cgModule name = "module" <+> string name

cgImports, cgStart :: Doc
cgImports = vsep $ map ("import" <+>)
    [ "StdEnv" ]
cgStart = vsep
    [ "Start :: *World -> *World"
    , "Start world ="
    , indent 4 ("let res =" <+> cgFunName (MN 0 "runMain") <+> "in" <+> "world")
    ]

-- Declarations and Expressions ------------------------------------------------

cgConstructors :: [(Name, DDecl)] -> Doc
cgConstructors decls =
    ":: Idris_Value = Idris_Dummy_Value" <$>
    indent 4 (vsep $ map (cgCtor . snd) decls)

cgCtor :: DDecl -> Doc
cgCtor (DConstructor name _tag arity) =
    --FIXME strictness
    char '|' <+> cgConName name <+> hsep (replicate arity "Idris_Value")

cgFunctions :: [(Name, DDecl)] -> Doc
cgFunctions = vsep . map (cgFun . snd)

cgFun :: DDecl -> Doc
cgFun (DFun name args def) =
    blank <$>
    "///" <+> string (show name) <$>
    cgFunName name <+> hsep (map cgVarName args) <+> char '=' <$>
    indent indentlevel (cgExp def)

cgExp :: DExp -> Doc
cgExp (DV var) =
    cgVar var
cgExp (DApp _istail name args) =
    cgApp (cgFunName name) args
cgExp (DLet name def rest) =
    --FIXME should be strict always?
    "let" <+> cgVarName name <+> char '=' <+> cgExp def <+> "in" <$>
    indent 4 (cgExp rest) <$>
    blank
cgExp (DUpdate var def) =
    cgUnsupported "update" (var, def)
cgExp (DProj def idx) =
    cgExp def <+> brackets (int idx)
cgExp (DC _reloc _tag name args) =
    --FIXME optimize to Int for argless ctors
    cgApp (cgConName name) args
cgExp (DCase _casetype exp alts) =
    cgCase exp alts
cgExp (DChkCase exp alts) =
    cgCase exp alts
cgExp (DConst const) =
    cgConst const
cgExp (DOp prim exps) =
    cgPrim prim exps
cgExp DNothing =
    --FIXME just Unit?
    cgUnsupported "nothing" ()
cgExp (DError msg) =
    "abort" <+> dquotes (string msg)
cgExp e =
    cgUnsupported "expression" e

cgCase :: DExp -> [DAlt] -> Doc
cgCase exp alts =
    -- parens for `case` in `case`
    "case" <+> parens (cgExp exp) <+> "of" <$>
    indent 4 (vsep (map cgAlt alts))

cgAlt :: DAlt -> Doc
cgAlt (DConCase _tag name args exp) =
    cgConName name <+> hsep (map cgVarName args) <+> "->" <+> cgExp exp
cgAlt (DConstCase const exp) =
    cgConst const <+> "->" <+> cgExp exp
cgAlt (DDefaultCase exp) =
    char '_' <+> "->" <+> cgExp exp

-- Constants and Primitives ----------------------------------------------------

cgConst :: Const -> Doc
cgConst (I i) = int i
--FIXME to big...
cgConst (BI i) = integer i
cgConst (Fl d) = double d
cgConst (Ch c) = squotes . string . cgEscape False $ c
cgConst (Str s) = dquotes . string . concatMap (cgEscape True) $ s
cgConst c = cgUnsupported "constant" c

cgEscape :: Bool -> Char -> String
cgEscape True '"' = "\\\""
cgEscape False '\'' = "\\'"
cgEscape _ '\\' = "\\\\"
cgEscape isString c
    | c >= ' ' && c < '\x7F' = [c]
    | c <= '\xFF' = "\\x" ++ showHex (ord c) ""
    | otherwise = error $ "idris-codegen-clean: char " ++ show c ++ " is bigger than 255"

cgPrim :: PrimFn -> [DExp] -> Doc
cgPrim (LPlus _) = cgOp "+"
cgPrim (LMinus _) = cgOp "-"
cgPrim (LTimes _) = cgOp "*"
cgPrim (LUDiv _) = cgOp "/"
cgPrim (LSDiv _) = cgOp "/"
cgPrim (LURem _) = cgOp "rem"
cgPrim (LSRem _) = cgOp "rem"

cgPrim (LAnd _) = cgOp "bitand"
cgPrim (LOr _) = cgOp "bitor"
cgPrim (LXOr _) = cgOp "bitxor"
cgPrim (LSHL _) = cgOp "<<"
cgPrim (LASHR _) = cgOp ">>"
cgPrim (LLSHR _) = cgOp ">>"  --FIXME
--cgPrim (LCompl _) = \[x] -> text "~" <> x

cgPrim (LEq _) = cgOp "=="
cgPrim (LLt _) = cgOp "<"
cgPrim (LSLt _) = cgOp "<"
cgPrim (LLe _) = cgOp "<="
cgPrim (LSLe _) = cgOp "<="
cgPrim (LGt _) = cgOp ">"
cgPrim (LSGt _) = cgOp ">"
cgPrim (LGe _) = cgOp ">="
cgPrim (LSGe _) = cgOp ">="

-- cgPrim (LSExt _ _) = head
-- cgPrim (LZExt _ _) = head
-- cgPrim (LTrunc _ _) = head
-- cgPrim (LBitCast _ _) = head

cgPrim (LChInt _) = cgApp "toInt"
cgPrim (LIntCh _) = cgApp "fromInt"

cgPrim (LIntStr _) = cgApp "toString"
cgPrim (LStrInt _) = cgApp "fromString"
cgPrim LStrConcat = cgOp "+++"
cgPrim LStrLt = cgOp "<"
cgPrim LStrEq = cgOp "=="

--cgPrim  LStrRev    = cgApp "reverse"
--cgPrim  LStrCons   = cgApp "cons"
--cgPrim  LStrHead   = \[x] -> x ! "0"
--cgPrim  LStrTail   = \[x] -> x ! "1:"
--cgPrim  LStrIndex  = \[x,i] -> x <> brackets i
--cgPrim  LStrLen    = cgPFun "len"
--cgPrim LStrSubstr = \[ofs,len,s] -> s <> brackets (ofs <> colon <> cgOp "+" [ofs,len])

cgPrim LWriteStr = \[_world, str] ->
    "fwrites" <+> cgExp str <+> "stdio" <$>
    "fwrites" <+> dquotes (text "\\n") <+> "stdio"
cgPrim LReadStr = \[_world] ->
    "freadline" <+> "stdio"

--cgPrim (LExternal n) = cgExtern $ show n

cgPrim (LIntFloat _) = cgApp "toReal"
cgPrim (LFloatInt _) = cgApp "fromReal"
cgPrim LFloatStr = cgApp "toString"
cgPrim LStrFloat = cgApp "fromString"

cgPrim LFExp = cgApp "exp"
cgPrim LFLog = cgApp "log"
cgPrim LFSin = cgApp "sin"
cgPrim LFCos = cgApp "cos"
cgPrim LFTan = cgApp "tan"
cgPrim LFASin = cgApp "asin"
cgPrim LFACos = cgApp "acos"
cgPrim LFATan = cgApp "atan"
cgPrim LFSqrt = cgApp "sqrt"
cgPrim LFFloor = cgApp "floor"
cgPrim LFCeil  = cgApp "ceil"
cgPrim LFNegate = cgApp "~" -- \[x] -> text "~" <> x

cgPrim f = \_args -> cgUnsupported "primitive" f

-- Names -----------------------------------------------------------------------

cgApp, cgOp :: Doc -> [DExp] -> Doc
cgApp fun args = parens $ fun <+> hsep (map cgExp args)
cgOp op [left, right] = parens $ cgExp left <+> op <+> cgExp right

cgVar :: LVar -> Doc
cgVar (Loc idx) = cgLoc idx --FIXME not in ir?
cgVar (Glob name) = cgVarName name

cgLoc :: Int -> Doc
cgLoc idx = "x" <> int idx

cgFunName, cgConName, cgVarName :: Name -> Doc
cgFunName name = string . fix $ "idris_" ++ mangle name
cgConName name = string . fix $ "Idris_" ++ mangle name
cgVarName name = string . fix $ mangle name

-- Fixes mkFnCon and mkUnderCon from IRTS.Defunctionalise
fix :: String -> String
fix name@(c:cs)
    | "P_" `isPrefixOf` name = toLower c : cs
    | "idris_U_" `isPrefixOf` name = toUpper c : cs
    | otherwise = name

mangle :: Name -> String
mangle name = concatMap mangleChar (showCG name)
    where
        mangleChar c
            | isIdent c = [c]
            | isSep c = "_"
            | isBrace c = ""
            | otherwise = "_" ++ show (fromEnum c) ++ "_"
        isIdent c = isAlpha c || isDigit c || c == '_'
        isSep c = c == '.'
        isBrace c = c == '{' || c == '}'

-- Unsupported -----------------------------------------------------------------

cgUnsupported :: Show a => Text -> a -> Doc
cgUnsupported cat val =
    parens $ "abort" <+> hsep [dquotes $ "UNSUPPORTED" <+> text cat <+> string (show val)]
