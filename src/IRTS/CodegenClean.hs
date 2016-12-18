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

level :: Int
level = 4

blank :: Doc
blank = space

string :: String -> Doc
string = text . pack

deline :: String -> String
deline = map (\c -> if c == '\n' then ' ' else c)

-- Main and Prelude ------------------------------------------------------------

codegenClean :: CodeGenerator
codegenClean info = do
    let (funcs, ctors) = partition isFun (defunDecls info)
    let output = vsep $ intersperse blank
            [ cgModule (takeBaseName $ outputFile info)
            , cgImports
            , cgHelpers
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

cgImports, cgHelpers, cgStart :: Doc
cgImports = vsep $ map ("import" <+>)
    [ "StdEnv" ]
cgHelpers = vsep
    [ "unsafeCoerce :: a -> b"
    , "unsafeCoerce x = code inline {"
    , indent level "pop_a 0"
    , "}"
    ]
cgStart = vsep
    [ "Start :: *World -> *World"
    , "Start world ="
    , indent level ("let res =" <+> cgFunName (MN 0 "runMain") <+> "in" <+> "world")
    ]

-- Declarations and Expressions ------------------------------------------------

cgConstructors :: [(Name, DDecl)] -> Doc
cgConstructors decls =
    ":: Idris_Value = Idris_Dummy_Value" <$>
    indent level (vsep $ map (cgCon . snd) decls)

cgCon :: DDecl -> Doc
cgCon (DConstructor name tag arity) =
    --FIXME strictness
    "///" <+> string (show name) <+> parens (int tag) <$>
    char '|' <+> cgConName name <+> hsep (replicate arity "Idris_Value")

cgFunctions :: [(Name, DDecl)] -> Doc
cgFunctions = vsep . map (cgFun . snd)

cgFun :: DDecl -> Doc
cgFun (DFun name args def) =
    blank <$>
    "///" <+> (string . show) name <$>
    "///" <+> (string . deline . show) def <$>
    cgFunName name <+> hsep (map cgVarName args) <+> char '=' <$>
    indent level (cgExp def)

cgExp :: DExp -> Doc
cgExp (DV var) =
    cgVar var
cgExp (DApp _istail name args) =
    cgApp (cgFunName name) args
cgExp (DLet name def rest) =
    --FIXME should be strict always?
    "let" <+> cgVarName name <+> char '=' <+> cgExp def <+> "in" <$>
    indent level (cgExp rest) <$>
    blank
cgExp (DUpdate var def) =
    cgUnsupported "UPDATE" (var, def)
cgExp (DProj def idx) =
    cgExp def <+> brackets (int idx)
-- Constructors: False, True
cgExp (DC _ 0 name []) | name == falseName =
    "False"
cgExp (DC _ 1 name []) | name == trueName =
    "True"
-- Constructors: rest
cgExp (DC _reloc _tag name args) =
    --FIXME optimize to Int for argless ctors
    cgApp (cgConName name) args
-- Case: if-then-else
cgExp (DCase _ test [DConCase 0 false [] elseAlt, DConCase 1 true  [] thenAlt]) | false == falseName && true == trueName =
    cgIfThenElse test thenAlt elseAlt
cgExp (DCase _ test [DConCase 1 true  [] thenAlt, DConCase 0 false [] elseAlt]) | false == falseName && true == trueName =
    cgIfThenElse test thenAlt elseAlt
cgExp (DCase _ test [DConCase 0 false [] elseAlt, DDefaultCase thenAlt ]) | false == falseName =
    cgIfThenElse test thenAlt elseAlt
cgExp (DCase _ test [DConCase 1 true  [] thenAlt, DDefaultCase elseAlt ]) | true == trueName =
    cgIfThenElse test thenAlt elseAlt
--cgExp (DCase _ test [DConstCase _ thenAlt, DDefaultCase elseAlt]) =
    --cgIfThenElse test thenAlt elseAlt
--cgExp (DCase _ test [t@SConstCase{}, e@SDefaultCase{}, SDefaultCase{}]) = emit (SCase Shared v [t, e])
-- Case: rest
cgExp (DCase _casetype exp alts) =
    cgCase exp alts
cgExp (DChkCase exp alts) =
    cgCase exp alts
cgExp (DConst const) =
    cgConst const
cgExp (DOp prim exps) =
    cgPrim prim exps
cgExp DNothing =
    cgUnsupported "NOTHING" ()
cgExp (DError msg) =
    "abort" <+> dquotes (string msg)
cgExp e =
    cgUnsupported "EXPRESSION" e

cgIfThenElse :: DExp -> DExp -> DExp -> Doc
cgIfThenElse test thenAlt elseAlt =
    "if" <+> parens (cgExp test) <$>
    indent level (
        parens (cgExp thenAlt) <$>
        parens(cgExp elseAlt)
    )

cgCase :: DExp -> [DAlt] -> Doc
cgCase exp alts =
    -- parens for `case` in `case`
    "case" <+> parens (cgExp exp) <+> "of" <$>
    indent level (vsep (map cgAlt alts))

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
cgConst c = cgUnsupported "CONSTANT" c

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

cgPrim f = \_args -> cgUnsupported "PRIMITIVE" f

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

falseName, trueName :: Name
falseName = NS (UN "False") ["Bool", "Prelude"]
trueName  = NS (UN "True")  ["Bool", "Prelude"]

-- Unsupported -----------------------------------------------------------------

cgUnsupported :: Show a => Text -> a -> Doc
cgUnsupported msg val =
    parens $ "abort" <+> hsep [dquotes $ text msg <+> string (show val) <+> "IS UNSUPPORTED"]
