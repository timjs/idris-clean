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

import Text.PrettyPrint.Leijen.Text hiding (indent, string)
import qualified Text.PrettyPrint.Leijen.Text as Pretty

-- Helpers ---------------------------------------------------------------------

indent :: Doc -> Doc
indent = Pretty.indent 4

string :: String -> Doc
string = text . pack

blank :: Doc
blank = space

deline :: String -> String
deline = map (\c -> if c == '\n' then ' ' else c)

prefixf, infixf :: Doc -> [Doc] -> Doc
prefixf fun args = parens $
    fun <+> hsep args
infixf op [left, right] = parens $
    left <+> op <+> right

-- Main and Prelude ------------------------------------------------------------

codegenClean :: CodeGenerator
codegenClean info = do
    let (funcs, ctors) = partition isFun (defunDecls info)
    let output = vsep $ intersperse blank
            [ cgModule (takeBaseName $ outputFile info)
            , cgImports
            , cgPredefined
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

cgImports, cgPredefined, cgStart :: Doc
cgImports = vsep $ map ("import" <+>)
    [ "StdEnv" ]
cgPredefined = vsep
    [ ":: Value = Nothing"
    , indent "| Boxed_Bool !Bool"
    , indent "| Boxed_Char !Char"
    , indent "| Boxed_Int !Int"
    , indent "| Boxed_Real !Real"
    , indent "| Boxed_String !String"
    , indent "| .."
    , blank
    , "unbox_Bool (Boxed_Bool x) :== x"
    , "unbox_Char (Boxed_Char x) :== x"
    , "unbox_Int (Boxed_Int x) :== x"
    , "unbox_Real (Boxed_Real x) :== x"
    , "unbox_String (Boxed_String x) :== x"
    ]
cgStart = vsep
    [ "Start =" <+> cgFunName (MN 0 "runMain") ]

-- Declarations and Expressions ------------------------------------------------

cgConstructors :: [(Name, DDecl)] -> Doc
cgConstructors decls =
    ":: Value" <$>
    indent (vsep $ map (cgCon . snd) decls)

cgCon :: DDecl -> Doc
cgCon (DConstructor name tag arity) =
    --FIXME strictness
    "///" <+> string (show name) <+> parens (int tag) <$>
    char '|' <+> cgConName name <+> hsep (replicate arity "!Value")

cgFunctions :: [(Name, DDecl)] -> Doc
cgFunctions = vsep . map (cgFun . snd)

cgFun :: DDecl -> Doc
cgFun (DFun name args def) =
    let arity = length args in
    blank <$>
    "///" <+> (string . show) name <$>
    "///" <+> (string . deline . show) def <$>
    cgFunName name <+> "::" <+> (if arity > 0
        then hsep (replicate arity "!Value") <+> "->"
        else empty) <+> "Value" <$>
    cgFunName name <+> hsep (map cgVarName args) <+> char '=' <$>
    indent (cgExp def)

cgExp :: DExp -> Doc
cgExp (DV var) =
    cgVar var
cgExp (DApp _istail name args) =
    cgApp (cgFunName name) args
cgExp (DLet name def rest) =
    --FIXME should be strict always?
    "let" <+> cgVarName name <+> char '=' <+> cgExp def <+> "in" <$>
    indent (cgExp rest) <$>
    blank
cgExp (DUpdate var def) =
    cgUnsupported "UPDATE" (var, def)
cgExp (DProj def idx) =
    cgExp def <+> brackets (int idx)
-- Constructors: False, True
-- cgExp (DC _ 0 name []) | name == falseName =
--     cgBox BBool "False"
-- cgExp (DC _ 1 name []) | name == trueName =
--     cgBox BBool "True"
-- Constructors: rest
cgExp (DC _reloc _tag name args) =
    --FIXME optimize to Int for argless ctors
    cgApp (cgConName name) args
-- Case: if-then-else
-- cgExp (DCase _ test [DConCase 0 false [] elseAlt, DConCase 1 true  [] thenAlt]) | false == falseName && true == trueName =
--     cgIfThenElse test thenAlt elseAlt
-- cgExp (DCase _ test [DConCase 1 true  [] thenAlt, DConCase 0 false [] elseAlt]) | false == falseName && true == trueName =
--     cgIfThenElse test thenAlt elseAlt
-- cgExp (DCase _ test [DConCase 0 false [] elseAlt, DDefaultCase thenAlt ]) | false == falseName =
--     cgIfThenElse test thenAlt elseAlt
-- cgExp (DCase _ test [DConCase 1 true  [] thenAlt, DDefaultCase elseAlt ]) | true == trueName =
--     cgIfThenElse test thenAlt elseAlt
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
    "Nothing" --cgUnsupported "NOTHING" ()
cgExp (DError msg) =
    "abort" <+> dquotes (string msg)
cgExp e =
    cgUnsupported "EXPRESSION" e

cgIfThenElse :: DExp -> DExp -> DExp -> Doc
cgIfThenElse test thenAlt elseAlt =
    "if" <+> parens (cgExp test) <$>
    indent (
        parens (cgExp thenAlt) <$>
        parens (cgExp elseAlt)
    )

cgCase :: DExp -> [DAlt] -> Doc
cgCase exp alts =
    -- parens for `case` in `case`
    parens $ "case" <+> cgExp exp <+> "of" <$>
    indent (vsep (map cgAlt alts))

cgAlt :: DAlt -> Doc
cgAlt (DConCase _tag name args exp) =
    cgConName name <+> hsep (map cgVarName args) <+> "->" <+> cgExp exp
cgAlt (DConstCase const exp) =
    cgConst const <+> "->" <+> cgExp exp
cgAlt (DDefaultCase exp) =
    char '_' <+> "->" <+> cgExp exp

-- Constants and Primitives ----------------------------------------------------

cgConst :: Const -> Doc
cgConst (I i) = cgBox BInt $ int i
--FIXME to big...
cgConst (BI i) = cgBox BInt $ integer i
cgConst (Fl d) = cgBox BReal $ double d
cgConst (Ch c) = cgBox BChar $ squotes . string . cgEscape False $ c
cgConst (Str s) = cgBox BString $ dquotes . string . concatMap (cgEscape True) $ s
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
cgPrim (LPlus  ty) = cgDeboxOp (cgATy ty) "+"
cgPrim (LMinus ty) = cgDeboxOp (cgATy ty) "-"
cgPrim (LTimes ty) = cgDeboxOp (cgATy ty) "*"
cgPrim (LSDiv  ty) = cgDeboxOp (cgATy ty) "/"
cgPrim (LUDiv  ty) = cgDeboxOp (cgITy ty) "/"
cgPrim (LSRem  ty) = cgDeboxOp (cgATy ty) "rem"
cgPrim (LURem  ty) = cgDeboxOp (cgITy ty) "rem"

cgPrim (LAnd   ty) = cgDeboxOp (cgITy ty) "bitand"
cgPrim (LOr    ty) = cgDeboxOp (cgITy ty) "bitor"
cgPrim (LXOr   ty) = cgDeboxOp (cgITy ty) "bitxor"
cgPrim (LSHL   ty) = cgDeboxOp (cgITy ty) "<<"
cgPrim (LASHR  ty) = cgDeboxOp (cgITy ty) ">>"
cgPrim (LLSHR  ty) = cgDeboxOp (cgITy ty) ">>"  --FIXME
--cgPrim (LCompl _) = \[x] -> text "~" <> x

cgPrim (LEq    ty) = cgReboxOp (cgATy ty) BBool "=="
cgPrim (LLt    ty) = cgReboxOp (cgITy ty) BBool "<"
cgPrim (LSLt   ty) = cgReboxOp (cgATy ty) BBool "<"
cgPrim (LLe    ty) = cgReboxOp (cgITy ty) BBool "<="
cgPrim (LSLe   ty) = cgReboxOp (cgATy ty) BBool "<="
cgPrim (LGt    ty) = cgReboxOp (cgITy ty) BBool ">"
cgPrim (LSGt   ty) = cgReboxOp (cgATy ty) BBool ">"
cgPrim (LGe    ty) = cgReboxOp (cgITy ty) BBool ">="
cgPrim (LSGe   ty) = cgReboxOp (cgATy ty) BBool ">="

-- cgPrim (LSExt _ _) = head
-- cgPrim (LZExt _ _) = head
-- cgPrim (LTrunc _ _) = head
-- cgPrim (LBitCast _ _) = head

cgPrim LStrConcat  = cgDeboxOp BString "+++"
cgPrim LStrLt      = cgReboxOp BString BBool "<"
cgPrim LStrEq      = cgReboxOp BString BBool "=="

--cgPrim  LStrRev    = cgApp "reverse"
--cgPrim  LStrCons   = cgApp "cons"
--cgPrim  LStrHead   = \[x] -> x ! "0"
--cgPrim  LStrTail   = \[x] -> x ! "1:"
--cgPrim  LStrIndex  = \[x,i] -> x <> brackets i
--cgPrim  LStrLen    = cgPFun "len"
--cgPrim LStrSubstr = \[ofs,len,s] -> s <> brackets (ofs <> colon <> cgOp "+" [ofs,len])

cgPrim LWriteStr = \[_world, str] ->
    "let res = fwrites" <+> cgExp str <+> "stdio in Nothing"
--cgPrim LReadStr = \[_world] ->
    --"freadline" <+> "stdio"

--cgPrim (LExternal n) = cgExtern $ show n

cgPrim (LChInt ty)    = cgReboxApp BChar (cgITy ty) "toInt"
cgPrim (LIntCh ty)    = cgReboxApp (cgITy ty) BChar "fromInt"
cgPrim (LIntStr ty)   = cgReboxApp (cgITy ty) BString "toString"
cgPrim (LStrInt ty)   = cgReboxApp BString (cgITy ty) "fromString"
cgPrim (LIntFloat ty) = cgReboxApp (cgITy ty) BReal "toReal"
cgPrim (LFloatInt ty) = cgReboxApp BReal (cgITy ty) "fromReal"
cgPrim LFloatStr      = cgReboxApp BReal BString "toString"
cgPrim LStrFloat      = cgReboxApp BString BReal "fromString"

cgPrim LFExp    = cgDeboxApp BReal "exp"
cgPrim LFLog    = cgDeboxApp BReal "log"
cgPrim LFSin    = cgDeboxApp BReal "sin"
cgPrim LFCos    = cgDeboxApp BReal "cos"
cgPrim LFTan    = cgDeboxApp BReal "tan"
cgPrim LFASin   = cgDeboxApp BReal "asin"
cgPrim LFACos   = cgDeboxApp BReal "acos"
cgPrim LFATan   = cgDeboxApp BReal "atan"
cgPrim LFSqrt   = cgDeboxApp BReal "sqrt"
cgPrim LFFloor  = cgDeboxApp BReal "floor"
cgPrim LFCeil   = cgDeboxApp BReal "ceil"
cgPrim LFNegate = cgDeboxApp BReal "~" -- \[x] -> text "~" <> x

cgPrim f = \_args -> cgUnsupported "PRIMITIVE" f

cgDeboxApp, cgDeboxOp :: BoxedTy -> Doc -> [DExp] -> Doc
cgDeboxApp ty = cgReboxApp ty ty
cgDeboxOp ty = cgReboxOp ty ty
cgReboxApp, cgReboxOp :: BoxedTy -> BoxedTy -> Doc -> [DExp] -> Doc
cgReboxApp = cgRebox prefixf
cgReboxOp = cgRebox infixf

cgRebox :: (Doc -> [Doc] -> Doc) -> BoxedTy -> BoxedTy -> Doc -> [DExp] -> Doc
cgRebox app from to fun = cgBox to . app fun . map (cgUnbox from . cgExp)

data BoxedTy
    = BBool
    | BChar
    | BInt
    | BReal
    | BString

instance Pretty BoxedTy where
    pretty BBool = "Bool"
    pretty BChar = "Char"
    pretty BInt = "Int"
    pretty BReal = "Real"
    pretty BString = "String"

cgITy :: IntTy -> BoxedTy
cgITy ITNative = BInt
cgITy ITBig = BInt
cgITy ITChar = BChar
cgITy (ITFixed IT8) = BChar
cgITy (ITFixed IT16) = BInt
cgITy (ITFixed IT32) = BInt
cgITy (ITFixed IT64) = BInt

cgATy :: ArithTy -> BoxedTy
cgATy ATFloat = BReal
cgATy (ATInt ity) = cgITy ity

-- Names & Applications --------------------------------------------------------

cgBox, cgUnbox :: BoxedTy -> Doc -> Doc
cgBox ty exp = prefixf ("Boxed_" <> pretty ty) [exp]
cgUnbox ty exp = prefixf ("unbox_" <> pretty ty) [exp]

cgApp, cgOp :: Doc -> [DExp] -> Doc
cgOp op [left, right] = infixf op [cgExp left, cgExp right]
cgApp fun args = prefixf fun (map cgExp args)

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
