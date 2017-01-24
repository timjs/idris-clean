{-# LANGUAGE OverloadedStrings #-}
module IRTS.CodegenClean
    ( codegenClean
    ) where

import Prelude hiding ((<$>))

import IRTS.CodegenCommon
import IRTS.TypeInfo
import IRTS.Lang
import Idris.Core.TT
import Idris.Core.Evaluate

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

codegenClean :: [(Name,[Int])] -> CodegenInfo -> IO ()
codegenClean usedpos info = do
    let (funcs, ctors) = partition isFun (liftDecls info)
    let tyinfo = mkTyInfo usedpos (ttDecls info)
    let output = vsep $ intersperse blank
            [ cgModule (takeBaseName $ outputFile info)
            , cgImports
            , cgPredefined
            , cgConstructors ctors
            , cgFunctions funcs
            , cgStart
            , cgTT (ttDecls info) usedpos
            ]
    withFile (outputFile info) WriteMode (`hPutDoc` output)
    where
        isFun (_, LFun{}) = True
        isFun (_, LConstructor{}) = False

cgTT :: [(Name, TTDecl)] -> [(Name, [Int])] -> Doc
cgTT ttdecls usedpos = "/*" <$> vsep (map go ttdecls) <$> "*/"
  where
    go (name, (def, rigcount, inject, access, tot, metainf)) = vsep
        [ "---" <+> (string . show) name <+> "---"
        , "RigCount:" <+> (string . show) rigcount
        , "Injectivity:" <+> (string . show) inject
        , "Accessibility:" <+> (string . show) access
        , "Totality:" <+> (string . show) tot
        , "MetaInformation:" <+> (string . show) metainf
        , "UsageInformation:" <+> (string . show . lookup name) usedpos
        , "---"
        , (string . show) def
        , "---"
        , blank
        ]

cgModule :: String -> Doc
cgModule name = "module" <+> string name

cgImports, cgPredefined, cgStart :: Doc
cgImports = vsep $ map ("import" <+>)
    [ "_SystemArray"
    , "_SystemEnum"
    , "StdPrim"
    , "StdPointer"
    ]
cgPredefined = vsep
    [ ":: Value = Nothing"
    , "         | .."
    , blank
    , "fst (a,b) :== a"
    , "snd (a,b) :== b"
    , blank
    , "clean_System_write_String world str | clean_Prim_toStdout str :== Nothing"
    , "clean_System_read_String world"
    , "  # (str, ok) = clean_Prim_fromStdin"
    , "  | ok :== str"
    , "clean_System_numArgs world :== fst clean_Prim_args"
    --cgForeign (FApp C_IntT [FUnknown,FCon C_IntNative]) (FStr "idris_numArgs") [] =
    , "clean_System_getArgs idx :== (snd clean_Prim_args).[idx]"
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
    , "  .d 0 2 f"
    , "    jsr closeF"
    , "  .o 0 1 b"
    , "}"
    , "clean_Prim_args :: (!Int, {String})"
    , "clean_Prim_args"
    , "  # argc = readInt32Z global_argc 0"
    , "  # argv = derefInt global_argv"
    , "  = (argc, {derefString (readInt argv (clean_Int_shl i (IF_INT_64_OR_32 3 2)) ) \\\\ i <- [0 .. clean_Int_dec argc]})"
    ]
cgStart = vsep
    [ "Start =" <+> cgFunName (MN 0 "runMain") ]

-- Declarations and Expressions ------------------------------------------------

cgConstructors :: [(Name, LDecl)] -> Doc
cgConstructors decls =
    ":: Value" <+> align (vsep $ map (cgCon . snd) decls)

cgCon :: LDecl -> Doc
cgCon (LConstructor name tag arity) =
    --FIXME strictness
    "///" <+> string (show name) <+> parens (int tag) <$>
    char '|' <+> cgConName name <+> hsep (take arity cgUniversalVars)

cgParam :: BasicTy -> Doc
cgParam = pretty . cgBTy

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
    cgFn "clean_Misc_force" [exp]
cgExp (LProj def idx) =
    cgUnsupported "PROJECT" (def, idx)
-- Constructors: False, True
-- cgExp (LCon _ 0 name []) | name == falseName =
--     cgBox RTBool "False"
-- cgExp (LCon _ 1 name []) | name == trueName =
--     cgBox RTBool "True"
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
    appPrefix "clean_Misc_abort" [dquotes $ string msg]
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
        cgCoerce (cgExp test) <$>
        parens (cgExp thenAlt) <$>
        parens (cgExp elseAlt)
    )

cgCase :: LExp -> [LAlt] -> Doc
cgCase exp alts =
    -- parens for `case` in `case`
    parens $ "case" <+> align (
        cgCoerce (cgExp exp) <+> "of" <$>
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
cgConst (I i)   = int i
cgConst (BI i)  = if validInt i then integer i else cgUnsupported "BIG INTEGER VALUE" i
-- Translate all bit types to `RTInt`, Clean doesn't have different integer sizes.
cgConst (B8 i)  = string . show $ i
cgConst (B16 i) = string . show $ i
cgConst (B32 i) = string . show $ i
cgConst (B64 i) = string . show $ i
cgConst (Fl d)  = string . fixExponent . show $ d
cgConst (Ch c)  = squotes . string . cgEscape False $ c
cgConst (Str s) = dquotes . string . concatMap (cgEscape True) $ s
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
cgPrim (LPlus  ty) = cgOp (cgATy ty) "add"
cgPrim (LMinus ty) = cgOp (cgATy ty) "sub"
cgPrim (LTimes ty) = cgOp (cgATy ty) "mul"
cgPrim (LSDiv  ty) = cgOp (cgATy ty) "div"
cgPrim (LUDiv  ty) = cgOp (cgITy ty) "div"
cgPrim (LSRem  ty) = cgOp (cgATy ty) "rem"
cgPrim (LURem  ty) = cgOp (cgITy ty) "rem"

cgPrim (LAnd   ty) = cgOp (cgITy ty) "and"
cgPrim (LOr    ty) = cgOp (cgITy ty) "or"
cgPrim (LXOr   ty) = cgOp (cgITy ty) "xor"
cgPrim (LSHL   ty) = cgOp (cgITy ty) "shl"
cgPrim (LASHR  ty) = cgOp (cgITy ty) "shr"
cgPrim (LLSHR  ty) = cgOp (cgITy ty) "shr"  --FIXME
--cgPrim (LCompl _) = \[x] -> text "~" <> x

cgPrim (LEq    ty) = cgOp (cgATy ty) "eq"
cgPrim (LLt    ty) = cgOp (cgITy ty) "lt"
cgPrim (LSLt   ty) = cgOp (cgATy ty) "lt"
cgPrim (LLe    ty) = cgOp (cgITy ty) "le"
cgPrim (LSLe   ty) = cgOp (cgATy ty) "le"
cgPrim (LGt    ty) = cgOp (cgITy ty) "gt"
cgPrim (LSGt   ty) = cgOp (cgATy ty) "gt"
cgPrim (LGe    ty) = cgOp (cgITy ty) "ge"
cgPrim (LSGe   ty) = cgOp (cgATy ty) "ge"

--XXX Only Char to Int and Int to Char? Rest is 64bit on 64bit machines...
cgPrim (LSExt _ _)    = cgFn "clean_Misc_id"
cgPrim (LZExt _ _)    = cgFn "clean_Misc_id"
cgPrim (LBitCast _ _) = cgFn "clean_Misc_id"
cgPrim (LTrunc _ _)   = cgFn "clean_Misc_id"

cgPrim LStrConcat = cgOp RTString "concat"
cgPrim LStrLt     = cgOp RTString "lt"
cgPrim LStrEq     = cgOp RTString "eq"

cgPrim LStrRev    = cgOp RTString "reverse"
cgPrim LStrCons   = cgOp RTString "cons"
cgPrim LStrHead   = cgOp RTString "head"
cgPrim LStrTail   = cgOp RTString "tail"
cgPrim LStrIndex  = cgOp RTString "index"
cgPrim LStrLen    = cgOp RTString "len"
cgPrim LStrSubstr = cgOp RTString "substring"

cgPrim LWriteStr = cgFn "clean_System_write_String"
cgPrim LReadStr  = cgFn "clean_System_read_String"

--cgPrim (LExternal n) = cgExtern $ show n

cgPrim (LChInt ty)    = cgOp RTChar "toInt"
cgPrim (LIntCh ty)    = cgOp (cgITy ty) "toChar"
cgPrim (LIntStr ty)   = cgOp (cgITy ty) "toString"
cgPrim (LStrInt ty)   = cgOp RTString "toInt"
cgPrim (LIntFloat ty) = cgOp (cgITy ty) "toReal"
cgPrim (LFloatInt ty) = cgOp RTReal "toInt"
cgPrim LFloatStr      = cgOp RTReal "toString"
cgPrim LStrFloat      = cgOp RTString "toReal"

cgPrim LFExp    = cgOp RTReal "exp"
cgPrim LFLog    = cgOp RTReal "log"
cgPrim LFSin    = cgOp RTReal "sin"
cgPrim LFCos    = cgOp RTReal "cos"
cgPrim LFTan    = cgOp RTReal "tan"
cgPrim LFASin   = cgOp RTReal "asin"
cgPrim LFACos   = cgOp RTReal "acos"
cgPrim LFATan   = cgOp RTReal "atan"
cgPrim LFSqrt   = cgOp RTReal "sqrt"
cgPrim LFFloor  = cgOp RTReal "entier"
--cgPrim LFCeil   = cgReboxFn RTReal RTInt "ceil"
cgPrim LFNegate = cgOp RTReal "~" -- \[x] -> text "~" <> x

cgPrim f = \args -> cgUnsupported "PRIMITIVE" (f, args)

data RawTy
    = RTFunc RawTy RawTy
    | RTBool
    | RTChar
    | RTInt
    | RTReal
    | RTString
    | RTFile
    | RTAny

instance Pretty RawTy where
    pretty (RTFunc a b) = parens $ pretty a <+> "->" <+> pretty b
    pretty RTBool = "Bool"
    pretty RTChar = "Char"
    pretty RTInt = "Int"
    pretty RTReal = "Real"
    pretty RTString = "String"
    pretty RTFile = "File"
    pretty RTAny = "Value"

-- Translate all `IntTy`s to `RTInt` except for characters.
-- FIXME No good for `Integer`, but it it used as an intermediate result in
-- multiple basic Idris functions like `String` lengths etc.
cgITy :: IntTy -> RawTy
cgITy ITNative = RTInt
cgITy ITBig = RTInt
cgITy ITChar = RTChar
cgITy (ITFixed IT8) = RTInt
cgITy (ITFixed IT16) = RTInt
cgITy (ITFixed IT32) = RTInt
cgITy (ITFixed IT64) = RTInt

cgATy :: ArithTy -> RawTy
cgATy ATFloat = RTReal
cgATy (ATInt ity) = cgITy ity

cgBTy :: BasicTy -> RawTy
cgBTy BTAny = RTAny
cgBTy (BTFun a b) = RTFunc (cgBTy a) (cgBTy b)
cgBTy BTBool = RTBool
cgBTy BTString = RTString
cgBTy (BTArith at) = cgATy at

-- Names & Applications --------------------------------------------------------

cgCoerce :: Doc -> Doc
cgCoerce exp = appPrefix "clean_Misc_coerce" [exp]

cgFn :: Doc -> [LExp] -> Doc
cgFn fun args = appPrefix fun (map (cgCoerce . cgExp) args)

cgOp :: RawTy -> Doc -> [LExp] -> Doc
cgOp ty fun args = appPrefix ("clean_" <> pretty ty <> "_" <> fun) (map cgExp args)

cgStrict :: Doc -> Doc
cgStrict d = "!" <> d

cgUniversalVars :: [Doc]
cgUniversalVars = [ "!(A." <> var <> ": " <> var <> ")" | var <- map char ['a'..'z'] ]

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
    appPrefix "clean_Misc_abort" [dquotes $ text msg <+> (string . dequote . show) val <+> "IS UNSUPPORTED"]

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
