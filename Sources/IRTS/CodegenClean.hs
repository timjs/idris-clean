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
import qualified Text.PrettyPrint.Leijen.Text as Pretty

-- Helpers ---------------------------------------------------------------------

string :: String -> Doc
string = text . pack

blank :: Doc
blank = space

deline, dequote :: String -> String
deline = map (\c -> if c == '\n' then ' ' else c)
dequote = map (\c -> if c == '"' then '\'' else c)
fixExponent = map (\c -> if c == 'e' then 'E' else c)

appPrefix, appInfix :: Doc -> [Doc] -> Doc
appPrefix fun args = parens $
    fun <+> hsep args
appInfix op [left, right] = parens $
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
    [ "StdEnv"
    , "StdPointer"
    ]
cgPredefined = vsep
    [ ":: Value = Nothing"
    , "         | .."
    , blank
    , "clean_String_cons chr str :== toString chr +++ str"
    , "clean_String_reverse str :== { str.[i] \\\\ i <- reverse [0..size str - 1] }"
    , "clean_String_head str :== select str 0"
    , "clean_String_tail str :== str % (1, size str - 1)"
    , "clean_String_index str i :== select str i"
    , "clean_String_len str :== size str"
    , "clean_String_substring ofs len str :== str % (ofs, ofs + len - 1)"
    , blank
    , "clean_System_write_String world str | clean_Prim_toStdout str :== Nothing"
    , "clean_System_read_String world"
    , "  # (str, ok) = clean_Prim_fromStdin"
    , "  | ok :== str"
    , "clean_System_numArgs world :== fst clean_Prim_args"
    --cgForeign (FApp C_IntT [FUnknown,FCon C_IntNative]) (FStr "idris_numArgs") [] =
    , "clean_System_getArgs idx :== (snd clean_Prim_args) !! idx"
    --cgForeign (FCon C_Str) (FStr "idris_getArg") [(FApp C_IntT [FUnknown,FCon C_IntNative], exp)] =
    , blank
    , "clean_Prim_toStdout :: !String -> Bool"
    , "clean_Prim_toStdout str = code inline {"
    , "  .d 1 0"
    , "    jsr stdioF"
    , "  .o 1 2 f"
    , "  .d 1 2 f"
    , "    jsr writeFS"
    , "  .o 0 2 f"
    , "  .d 0 2 f"
    , "    jsr closeF"
    , "  .o 0 1 b"
    , "}"
    , "clean_Prim_fromStdin :: (!String,!Bool)"
    , "clean_Prim_fromStdin = code inline {"
    , "  .d 0 0"
    , "    jsr stdioF"
    , "  .o 0 2 f"
    , "  .d 0 2 f"
    , "    jsr readLineF"
    , "  .o 1 2 f"
    , "  .d 1 2 f"
    , "    jsr closeF"
    , "  .o 1 1 b"
    , "}"
    , "clean_Prim_args :: (!Int, [String])"
    , "clean_Prim_args"
    , "  # argc = readInt32Z global_argc 0"
    , "  # argv = derefInt global_argv"
    , "  = (argc, [derefString (readInt argv (i << (IF_INT_64_OR_32 3 2)) ) \\\\ i <- [0..argc - 1]])"
    , "clean_Prim_unsafeCoerce :: a -> b"
    , "clean_Prim_unsafeCoerce x = code inline {"
    , "  pop_a 0"
    , "}"
    ]
cgStart = vsep
    [ "Start :: !Value"
    , "Start =" <+> cgFunName (MN 0 "runMain") ]

-- Declarations and Expressions ------------------------------------------------

cgConstructors :: [(Name, DDecl)] -> Doc
cgConstructors decls =
    ":: Value" <+> align (vsep $ map (cgCon . snd) decls)

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
    -- cgFunName name <+> "::" <+> (if arity > 0
        -- then hsep (replicate arity "!Value") <+> "->"
        -- else empty) <+> "Value" <$>
    cgFunName name <+> hsep (map cgVarName args) <$>
    char '=' <+> align (cgExp def)

cgExp :: DExp -> Doc
cgExp (DV var) =
    cgVar var
cgExp (DApp _istail name args) =
    cgFn (cgFunName name) args
cgExp (DLet name def rest) =
    cgLet name def rest
cgExp (DUpdate var def) =
    --cgLet var def Nothing
    cgExp def
cgExp (DProj def idx) =
    cgUnsupported "PROJECT" (def, idx)
    --cgExp def <+> brackets (int idx)
-- Constructors: False, True
-- cgExp (DC _ 0 name []) | name == falseName =
--     cgBox BBool "False"
-- cgExp (DC _ 1 name []) | name == trueName =
--     cgBox BBool "True"
-- Constructors: rest
cgExp (DC _reloc _tag name args) =
    --FIXME optimize to Int for argless ctors
    cgFn (cgConName name) args
-- Case: if-then-else
-- cgExp (DCase _ test [DConCase 0 false [] elseAlt, DConCase 1 true  [] thenAlt]) | false == falseName && true == trueName =
--     cgIfThenElse test thenAlt elseAlt
-- cgExp (DCase _ test [DConCase 1 true  [] thenAlt, DConCase 0 false [] elseAlt]) | false == falseName && true == trueName =
--     cgIfThenElse test thenAlt elseAlt
-- cgExp (DCase _ test [DConCase 0 false [] elseAlt, DDefaultCase thenAlt ]) | false == falseName =
--     cgIfThenElse test thenAlt elseAlt
-- cgExp (DCase _ test [DConCase 1 true  [] thenAlt, DDefaultCase elseAlt ]) | true == trueName =
--     cgIfThenElse test thenAlt elseAlt
cgExp (DCase _ test [DConstCase (I 0) elseAlt, DDefaultCase thenAlt]) =
    cgIfThenElse test thenAlt elseAlt
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
cgExp (DForeign fun ret args) =
    cgForeign fun ret args
cgExp DNothing =
    "Nothing" --cgUnsupported "NOTHING" ()
cgExp (DError msg) =
    appPrefix "abort" [dquotes $ string msg]

cgLet :: Name -> DExp -> DExp -> Doc
cgLet name def rest =
    --FIXME should be strict always?
    "let" <+> cgVarName name <+> char '=' <+> cgExp def <$>
    "in " <+> align (
        cgExp rest
    )

cgIfThenElse :: DExp -> DExp -> DExp -> Doc
cgIfThenElse test thenAlt elseAlt =
    "if" <+> align (
        cgUnsafeCoerce (cgExp test) <$>
        parens (cgExp thenAlt) <$>
        parens (cgExp elseAlt)
    )

cgCase :: DExp -> [DAlt] -> Doc
cgCase exp alts =
    -- parens for `case` in `case`
    parens $ "case" <+> align (
        cgUnsafeCoerce (cgExp exp) <+> "of" <$>
        vsep (map cgAlt alts)
    )

cgAlt :: DAlt -> Doc
cgAlt (DConCase _tag name args exp) =
    cgConName name <+> hsep (map cgVarName args) <+> "->" <+> align (cgExp exp)
cgAlt (DConstCase const exp) =
    cgConst const <+> "->" <+> align (cgExp exp)
cgAlt (DDefaultCase exp) =
    char '_' <+> "->" <+> align (cgExp exp)

-- Foreign Calls ---------------------------------------------------------------

cgForeign :: FDesc -> FDesc -> [(FDesc, DExp)]-> Doc
cgForeign _ (FStr "idris_numArgs") [] =
    cgFn "clean_System_numArgs" [DNothing]
cgForeign _ (FStr "idris_getArg") [(_, exp)] =
    cgFn "clean_System_getArgs" [exp]
cgForeign fun ret args =
    cgUnsupported "FOREIGN CALL" (fun, ret, args)

-- Constants and Primitives ----------------------------------------------------

cgConst :: Const -> Doc
cgConst (I i)   = cgBox BInt $ int i
cgConst (BI i)  = cgBox BInt $ if validInt i then integer i else cgUnsupported "BIG INTEGER VALUE" i
-- Translate all bit types to `BInt`, Clean doesn't have different integer sizes.
cgConst (B8 i)  = cgBox BInt . string . show $ i
cgConst (B16 i) = cgBox BInt . string . show $ i
cgConst (B32 i) = cgBox BInt . string . show $ i
cgConst (B64 i) = cgBox BInt . string . show $ i
cgConst (Fl d)  = cgBox BReal . string . fixExponent . show $ d
cgConst (Ch c)  = cgBox BChar . squotes . string . cgEscape False $ c
cgConst (Str s) = cgBox BString . dquotes . string . concatMap (cgEscape True) $ s
cgConst c       = cgUnsupported "CONSTANT" c

cgEscape :: Bool -> Char -> String
cgEscape True '"' = "\\\""
cgEscape False '\'' = "\\'"
cgEscape _ '\\' = "\\\\"
cgEscape isString c
    | c >= ' ' && c < '\x7F' = [c]
    | c <= '\xFF' = "\\x" ++ showHex (ord c) ""
    | otherwise = error $ "idris-codegen-clean: char " ++ show c ++ " is bigger than 255"

cgPrim :: PrimFn -> [DExp] -> Doc
cgPrim (LPlus  ty) = cgPrimOp (cgATy ty) "+"
cgPrim (LMinus ty) = cgPrimOp (cgATy ty) "-"
cgPrim (LTimes ty) = cgPrimOp (cgATy ty) "*"
cgPrim (LSDiv  ty) = cgPrimOp (cgATy ty) "/"
cgPrim (LUDiv  ty) = cgPrimOp (cgITy ty) "/"
cgPrim (LSRem  ty) = cgPrimOp (cgATy ty) "rem"
cgPrim (LURem  ty) = cgPrimOp (cgITy ty) "rem"

cgPrim (LAnd   ty) = cgPrimOp (cgITy ty) "bitand"
cgPrim (LOr    ty) = cgPrimOp (cgITy ty) "bitor"
cgPrim (LXOr   ty) = cgPrimOp (cgITy ty) "bitxor"
cgPrim (LSHL   ty) = cgPrimOp (cgITy ty) "<<"
cgPrim (LASHR  ty) = cgPrimOp (cgITy ty) ">>"
cgPrim (LLSHR  ty) = cgPrimOp (cgITy ty) ">>"  --FIXME
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

--XXX Only Char to Int and Int to Char? Rest is 64bit on 64bit machines...
cgPrim (LSExt _ _)    = cgFn "id"
cgPrim (LZExt _ _)    = cgFn "id"
cgPrim (LBitCast _ _) = cgFn "id"
cgPrim (LTrunc _ _)   = cgFn "id"

cgPrim LStrConcat = cgPrimOp BString "+++"
cgPrim LStrLt     = cgReboxOp BString BBool "<"
cgPrim LStrEq     = cgReboxOp BString BBool "=="

cgPrim LStrRev    = cgFn "clean_String_reverse"
cgPrim LStrCons   = cgFn "clean_String_cons"
cgPrim LStrHead   = cgFn "clean_String_head"
cgPrim LStrTail   = cgFn "clean_String_tail"
cgPrim LStrIndex  = cgFn "clean_String_index"
cgPrim LStrLen    = cgFn "clean_String_len"
cgPrim LStrSubstr = cgFn "clean_String_substring"

cgPrim LWriteStr = cgFn "clean_System_write_String"
cgPrim LReadStr  = cgFn "clean_System_read_String"

--cgPrim (LExternal n) = cgExtern $ show n

cgPrim (LChInt ty)    = cgReboxFn BChar (cgITy ty) "toInt"
cgPrim (LIntCh ty)    = cgReboxFn (cgITy ty) BChar "toChar"
cgPrim (LIntStr ty)   = cgReboxFn (cgITy ty) BString "toString"
cgPrim (LStrInt ty)   = cgReboxFn BString (cgITy ty) "toInt"
cgPrim (LIntFloat ty) = cgReboxFn (cgITy ty) BReal "toReal"
cgPrim (LFloatInt ty) = cgReboxFn BReal (cgITy ty) "toInt"
cgPrim LFloatStr      = cgReboxFn BReal BString "toString"
cgPrim LStrFloat      = cgReboxFn BString BReal "toReal"

cgPrim LFExp    = cgPrimFn BReal "exp"
cgPrim LFLog    = cgPrimFn BReal "log"
cgPrim LFSin    = cgPrimFn BReal "sin"
cgPrim LFCos    = cgPrimFn BReal "cos"
cgPrim LFTan    = cgPrimFn BReal "tan"
cgPrim LFASin   = cgPrimFn BReal "asin"
cgPrim LFACos   = cgPrimFn BReal "acos"
cgPrim LFATan   = cgPrimFn BReal "atan"
cgPrim LFSqrt   = cgPrimFn BReal "sqrt"
cgPrim LFFloor  = cgReboxFn BReal BInt "entier"
--cgPrim LFCeil   = cgReboxFn BReal BInt "ceil"
cgPrim LFNegate = cgPrimFn BReal "~" -- \[x] -> text "~" <> x

cgPrim f = \args -> cgUnsupported "PRIMITIVE" (f, args)

cgPrimFn, cgPrimOp :: BoxedTy -> Doc -> [DExp] -> Doc
cgPrimFn ty = cgReboxFn ty ty
cgPrimOp ty = cgReboxOp ty ty
cgReboxFn, cgReboxOp :: BoxedTy -> BoxedTy -> Doc -> [DExp] -> Doc
cgReboxFn = cgRebox appPrefix
cgReboxOp = cgRebox appInfix

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

-- Translate all `IntTy`s to `BInt` except for characters.
-- FIXME No good for `Integer`, but it it used as an intermediate result in
-- multiple basic Idris functions like `String` lengths etc.
cgITy :: IntTy -> BoxedTy
cgITy ITNative = BInt
cgITy ITBig = BInt
cgITy ITChar = BChar
cgITy (ITFixed IT8) = BInt
cgITy (ITFixed IT16) = BInt
cgITy (ITFixed IT32) = BInt
cgITy (ITFixed IT64) = BInt

cgATy :: ArithTy -> BoxedTy
cgATy ATFloat = BReal
cgATy (ATInt ity) = cgITy ity

-- Names & Applications --------------------------------------------------------

cgUnsafeCoerce :: Doc -> Doc
cgUnsafeCoerce exp = appPrefix "clean_Prim_unsafeCoerce" [exp]

cgBox, cgUnbox :: BoxedTy -> Doc -> Doc
cgBox ty exp = exp
cgUnbox ty exp = exp

cgRebox :: (Doc -> [Doc] -> Doc) -> BoxedTy -> BoxedTy -> Doc -> [DExp] -> Doc
cgRebox app from to fun = cgBox to . app fun . map (cgUnbox from . cgExp)

cgFn :: Doc -> [DExp] -> Doc
cgFn fun args = appPrefix fun (map (cgUnsafeCoerce . cgExp) args)

cgVar :: LVar -> Doc
cgVar (Loc idx) = cgLoc idx --FIXME not in ir?
cgVar (Glob name) = cgVarName name

cgLoc :: Int -> Doc
cgLoc idx = "x" <> int idx

cgFunName, cgConName, cgVarName :: Name -> Doc
cgFunName name = string . fixMangle $ "idris_" ++ mangle name
cgConName name = string . fixMangle $ "Idris_" ++ mangle name
cgVarName name = string . fixMangle $ mangle name

-- Fixes mkFnCon and mkUnderCon from IRTS.Defunctionalise
fixMangle :: String -> String
fixMangle name@(c:cs)
    -- Parameters of underapplied functions
    | "P_" `isPrefixOf` name = toLower c : cs
    -- Calls to partial constructors (?)
    | "idris_P_" `isPrefixOf` name = toUpper c : cs
    -- Calls to underapplied functions
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

validInt :: Integer -> Bool
validInt i = i > minInt && i < maxInt
    where
        minInt = -(2^63)
        maxInt = 2^63 - 1

falseName, trueName :: Name
falseName = NS (UN "False") ["Bool", "Prelude"]
trueName  = NS (UN "True")  ["Bool", "Prelude"]

-- Unsupported -----------------------------------------------------------------

cgUnsupported :: Show a => Text -> a -> Doc
cgUnsupported msg val =
    appPrefix "abort" [dquotes $ text msg <+> (string . dequote . show) val <+> "IS UNSUPPORTED"]
