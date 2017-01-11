{-# LANGUAGE OverloadedStrings #-}
module IRTS.CodegenClean
    ( codegenClean
    ) where

import Prelude hiding ((<$>))

import IRTS.CodegenCommon
import IRTS.Lang
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
    let (funcs, ctors) = partition isFun (liftDecls info)
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
        isFun (_, LFun{}) = True
        isFun (_, LConstructor{}) = False

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
    , "clean_Prim_force :: !a -> a"
    , "clean_Prim_force a = a"--FIXME
    , "clean_Prim_unsafeCoerce :: a -> b"
    , "clean_Prim_unsafeCoerce x = code inline {"
    , "  pop_a 0"
    , "}"
    ]
cgStart = vsep
    [ "Start :: !Value"
    , "Start =" <+> cgFunName (MN 0 "runMain") ]

-- Declarations and Expressions ------------------------------------------------

cgConstructors :: [(Name, LDecl)] -> Doc
cgConstructors decls =
    ":: Value" <+> align (vsep $ map (cgCon . snd) decls)

cgCon :: LDecl -> Doc
cgCon (LConstructor name tag arity) =
    --FIXME strictness
    "///" <+> string (show name) <+> parens (int tag) <$>
    char '|' <+> cgConName name <+> hsep (replicate arity "!Value")

cgFunctions :: [(Name, LDecl)] -> Doc
cgFunctions = vsep . map (cgFun . snd)

cgFun :: LDecl -> Doc
cgFun (LFun _opt name args def) =
    let arity = length args in
    blank <$>
    "///" <+> (string . show) name <$>
    "///" <+> (string . deline . show) def <$>
    -- cgFunName name <+> "::" <+> (if arity > 0
        -- then hsep (replicate arity "!Value") <+> "->"
        -- else empty) <+> "Value" <$>
    cgFunName name <+> hsep (map cgVarName args) <$>
    char '=' <+> align (cgExp def)

cgExp :: LExp -> Doc
cgExp (LV var) =
    cgVar var
cgExp (LLazyApp fun args) =
    cgFn (cgFunName fun) args
cgExp (LApp _istail (LV (Glob fun)) args) =
    cgFn (cgFunName fun) args
cgExp (LApp _istail exp args) =
    cgApp exp args
cgExp (LLet name def rest) =
    cgLet name def rest
cgExp (LForce (LLazyApp n args)) =
    cgExp $ LApp False (LV (Glob n)) args
cgExp (LForce exp) =
    cgFn "clean_Prim_force" [exp]
cgExp (LProj def idx) =
    cgUnsupported "PROJECT" (def, idx)
-- Constructors: False, True
-- cgExp (LCon _ 0 name []) | name == falseName =
--     cgBox BBool "False"
-- cgExp (LCon _ 1 name []) | name == trueName =
--     cgBox BBool "True"
-- Constructors: rest
cgExp (LCon _reloc _tag name args) =
    --FIXME optimize to Int for argless ctors
    cgFn (cgConName name) args
-- Case: if-then-else
-- cgExp (LCase _ test [LConCase 0 false [] elseAlt, LConCase 1 true  [] thenAlt]) | false == falseName && true == trueName =
--     cgIfThenElse test thenAlt elseAlt
-- cgExp (LCase _ test [LConCase 1 true  [] thenAlt, LConCase 0 false [] elseAlt]) | false == falseName && true == trueName =
--     cgIfThenElse test thenAlt elseAlt
-- cgExp (LCase _ test [LConCase 0 false [] elseAlt, LDefaultCase thenAlt ]) | false == falseName =
--     cgIfThenElse test thenAlt elseAlt
-- cgExp (LCase _ test [LConCase 1 true  [] thenAlt, LDefaultCase elseAlt ]) | true == trueName =
--     cgIfThenElse test thenAlt elseAlt
cgExp (LCase _ test [LConstCase (I 0) elseAlt, LDefaultCase thenAlt]) =
    cgIfThenElse test thenAlt elseAlt
--cgExp (LCase _ test [t@SConstCase{}, e@SDefaultCase{}, SDefaultCase{}]) = emit (SCase Shared v [t, e])
-- Case: rest
cgExp (LCase _casetype exp alts) =
    cgCase exp alts
cgExp (LConst const) =
    cgConst const
cgExp (LOp prim exps) =
    cgPrim prim exps
cgExp (LForeign fun ret args) =
    cgForeign fun ret args
cgExp LNothing =
    "Nothing" --cgUnsupported "NOTHING" ()
cgExp (LError msg) =
    appPrefix "abort" [dquotes $ string msg]
cgExp exp =
    cgUnsupported "EXPRESSION" exp

cgApp :: LExp -> [LExp] -> Doc
cgApp exp args = appPrefix (parens (cgExp exp)) (map cgExp args)

cgLet :: Name -> LExp -> LExp -> Doc
cgLet name def rest =
    --FIXME should be strict always?
    "let" <+> cgVarName name <+> char '=' <+> cgExp def <$>
    "in " <+> align (
        cgExp rest
    )

cgIfThenElse :: LExp -> LExp -> LExp -> Doc
cgIfThenElse test thenAlt elseAlt =
    "if" <+> align (
        cgUnsafeCoerce (cgExp test) <$>
        parens (cgExp thenAlt) <$>
        parens (cgExp elseAlt)
    )

cgCase :: LExp -> [LAlt] -> Doc
cgCase exp alts =
    -- parens for `case` in `case`
    parens $ "case" <+> align (
        cgUnsafeCoerce (cgExp exp) <+> "of" <$>
        vsep (map cgAlt alts)
    )

cgAlt :: LAlt -> Doc
cgAlt (LConCase _tag name args exp) =
    cgConName name <+> hsep (map cgVarName args) <+> "->" <+> align (cgExp exp)
cgAlt (LConstCase const exp) =
    cgConst const <+> "->" <+> align (cgExp exp)
cgAlt (LDefaultCase exp) =
    char '_' <+> "->" <+> align (cgExp exp)

-- Foreign Calls ---------------------------------------------------------------

cgForeign :: FDesc -> FDesc -> [(FDesc, LExp)]-> Doc
cgForeign _ (FStr "idris_numArgs") [] =
    cgFn "clean_System_numArgs" [LNothing]
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

cgPrim :: PrimFn -> [LExp] -> Doc
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

cgPrimFn, cgPrimOp :: BoxedTy -> Doc -> [LExp] -> Doc
cgPrimFn ty = cgReboxFn ty ty
cgPrimOp ty = cgReboxOp ty ty
cgReboxFn, cgReboxOp :: BoxedTy -> BoxedTy -> Doc -> [LExp] -> Doc
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

cgRebox :: (Doc -> [Doc] -> Doc) -> BoxedTy -> BoxedTy -> Doc -> [LExp] -> Doc
cgRebox app from to fun = cgBox to . app fun . map (cgUnbox from . cgExp)

cgFn :: Doc -> [LExp] -> Doc
cgFn fun args = appPrefix fun (map (cgUnsafeCoerce . cgExp) args)

cgVar :: LVar -> Doc
cgVar (Loc idx) = cgLoc idx --FIXME not in ir?
cgVar (Glob name) = cgVarName name

cgLoc :: Int -> Doc
cgLoc idx = "x" <> int idx

cgFunName, cgConName, cgVarName :: Name -> Doc
cgFunName name = string $ "idris_" ++ mangle name
cgConName name = string $ "Idris_" ++ mangle name
cgVarName = cgFunName

-- Let's not mangle _that_ much. Especially function parameters
-- like `e0` and `e1` are nicer when readable.
-- cgName :: Name -> Doc
-- cgName (MN i n) | all (\x -> isAlpha x || x == '_') (T.unpack n)
--     = text $ T.unpack n ++ show i
-- cgName n = text (mangle n)  -- <?> show n  -- uncomment this to get a comment for *every* mangled name
--         isIdent c = isAlpha c || isDigit c || c == '_'

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

{-
data SExp = SV LVar
data LExp = LV LVar
data LExp = LV LVar

          | SApp Bool Name [LVar]
          | LApp Bool Name [LExp] -- True = tail call
          | LApp Bool LExp [LExp] -- True = tail call

          | SLet LVar SExp SExp
          | LLet Name LExp LExp -- name just for pretty printing
          | LLet Name LExp LExp -- name just for pretty printing

          | SUpdate LVar SExp
          | DUpdate Name LExp -- eval expression, then update var with it

          | SProj LVar Int
          | LProj LExp Int
          | LProj LExp Int -- projection

          | SCon (Maybe LVar) Int Name [LVar]
          | LCon (Maybe LVar) Int Name [LExp]
          | LCon (Maybe LVar) Int Name [LExp] -- Location to reallocate, if available

          | SCase CaseType LVar [SAlt]
          | LCase CaseType LExp [LAlt]
          | LCase CaseType LExp [LAlt]

          | SChkCase LVar [SAlt]
          | DChkCase LExp [LAlt] -- a case where the type is unknown (for EVAL/APPLY)

          | SConst Const
          | LConst Const
          | LConst Const

          | SForeign FDesc FDesc [(FDesc, LVar)]
          | LForeign FDesc FDesc [(FDesc, LExp)]
          | LForeign FDesc           -- Function descriptor (usually name as string)
                     FDesc           -- Return type descriptor
                     [(FDesc, LExp)] -- first LExp is the FFI type description

          | SOp PrimFn [LVar]
          | LOp PrimFn [LExp]
          | LOp PrimFn [LExp]

          | SNothing -- erased value, will never be inspected
          | LNothing -- erased value, can be compiled to anything since it'll never be inspected
          | LNothing

          | SError String
          | LError String
          | LError String

          -- EXTRA
          | LLazyApp Name [LExp]     -- True = tail call
          | LForce LExp              -- make sure Exp is evaluted

          -- LIFTED
          | LLazyExp LExp            -- lifted out before compiling
          | LLam [Name] LExp         -- lambda, lifted out before compiling

  deriving Show


data DDecl = DFun        Name [Name] LExp --          name, arg names, def
data LDecl = LFun [LOpt] Name [Name] LExp -- options, name, arg names, def

           | LConstructor Name Int Int -- constructor name, tag, arity
           | DConstructor Name Int Int -- constructor name, tag, arity
-}
