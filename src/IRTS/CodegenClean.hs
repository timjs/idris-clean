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
import Data.Char (isAlpha, isDigit, ord)
import Data.Text.Lazy (Text, pack, unpack, toUpper)
import System.IO (withFile, IOMode(..))

import Text.PrettyPrint.Leijen.Text hiding (string)

-- Helpers ---------------------------------------------------------------------

indentlevel :: Int
indentlevel = 4

blank :: Doc
blank = space

string :: String -> Doc
string = text . pack

operator, call :: Text -> [Doc] -> Doc
operator op [left, right] = parens $ left <+> text op <+> right
call func args = parens $ text func <+> hsep args

mangle :: Name -> Text
mangle name = pack (concatMap mangleChar $ showCG name)
    where
        mangleChar c
            | isIdent c = [c]
            | isSep c = "_"
            | isBrace c = ""
            | otherwise = "_" ++ show (fromEnum c) ++ "_"
        isIdent c = isAlpha c || isDigit c || c == '_'
        isSep c = c == '.'
        isBrace c = c == '{' || c == '}'

-- Main and Prelude ------------------------------------------------------------

codegenClean :: CodeGenerator
codegenClean info = do
    let declarations = vsep $ map cgDecl (defunDecls info)
    let output = vsep [cgPrelude, declarations]
    withFile (outputFile info) WriteMode (`hPutDoc` output)
    where
        isFun (_, DFun{}) = True
        isFun (_, DConstructor{}) = False

cgPrelude :: Doc
cgPrelude = vsep
    [ "module Main"
    , blank
    , "import StdEnv"
    , blank
    , ":: Value = .."
    , blank
    , "Start :: *World -> *World"
    , "Start world = runMain"
    , blank
    ]

-- Declarations and Expressions ------------------------------------------------

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
    indent indentlevel (cgExp def)

cgExp :: DExp -> Doc
cgExp (DV var) =
    cgVar var
cgExp (DApp _istail name args) =
    cgApp name args
cgExp (DLet name def rest) =
    --FIXME should be strict always?
    char '#' <+> cgName name <+> char '=' <+> cgExp def <$>
    cgExp rest <$>
    blank
cgExp (DUpdate var def) =
    cgUnsupported "update" (var, def)
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
    "abort" <+> dquotes (string msg)
cgExp e =
    cgUnsupported "expression" e

cgCase :: DExp -> [DAlt] -> Doc
cgCase exp alts =
    "case" <+> cgExp exp <+> "of" <$>
    indent 4 (vsep (map cgAlt alts))

cgAlt :: DAlt -> Doc
cgAlt (DConCase _tag name args exp) =
    cgName name <+> hsep (map cgName args) <+> "->" <+> cgExp exp
cgAlt (DConstCase const exp) =
    cgConst const <+> "->" <+> cgExp exp
cgAlt (DDefaultCase exp) =
    char '_' <+> "->" <+> cgExp exp

cgApp :: Name -> [DExp] -> Doc
cgApp name args = call (mangle name) (map cgExp args)

-- Constants and Primitives ----------------------------------------------------

cgConst :: Const -> Doc
cgConst (I i) = int i
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

cgPrim :: PrimFn -> [Doc] -> Doc
cgPrim (LPlus _) = operator "+"
cgPrim (LMinus _) = operator "-"
cgPrim (LTimes _) = operator "*"
cgPrim (LUDiv _) = operator "/"
cgPrim (LSDiv _) = operator "/"
cgPrim (LURem _) = operator "rem"
cgPrim (LSRem _) = operator "rem"

cgPrim (LAnd _) = operator "bitand"
cgPrim (LOr _) = operator "bitor"
cgPrim (LXOr _) = operator "bitxor"
cgPrim (LSHL _) = operator "<<"
cgPrim (LASHR _) = operator ">>"
cgPrim (LLSHR _) = operator ">>"  --FIXME
--cgPrim (LCompl _) = \[x] -> text "~" <> x

cgPrim (LEq _) = operator "=="
cgPrim (LLt _) = operator "<"
cgPrim (LSLt _) = operator "<"
cgPrim (LLe _) = operator "<="
cgPrim (LSLe _) = operator "<="
cgPrim (LGt _) = operator ">"
cgPrim (LSGt _) = operator ">"
cgPrim (LGe _) = operator ">="
cgPrim (LSGe _) = operator ">="

-- cgPrim (LSExt _ _) = head
-- cgPrim (LZExt _ _) = head
-- cgPrim (LTrunc _ _) = head
-- cgPrim (LBitCast _ _) = head

cgPrim (LChInt _) = call "toInt"
cgPrim (LIntCh _) = call "fromInt"

cgPrim (LIntStr _) = call "toString"
cgPrim (LStrInt _) = call "fromString"
cgPrim LStrConcat = operator "+++"
cgPrim LStrLt = operator "<"
cgPrim LStrEq = operator "=="

--cgPrim  LStrRev    = call "reverse"
--cgPrim  LStrCons   = call "cons"
--cgPrim  LStrHead   = \[x] -> x ! "0"
--cgPrim  LStrTail   = \[x] -> x ! "1:"
--cgPrim  LStrIndex  = \[x,i] -> x <> brackets i
--cgPrim  LStrLen    = cgPFun "len"
--cgPrim LStrSubstr = \[ofs,len,s] -> s <> brackets (ofs <> colon <> operator "+" [ofs,len])

cgPrim LWriteStr = \[_world, str] ->
    "fwrites" <+> str <+> "stdio" <$>
    "fwrites" <+> dquotes (text "\\n") <+> "stdio"
cgPrim LReadStr = \[_world] ->
    "freadline" <+> "stdio"

--cgPrim (LExternal n) = cgExtern $ show n

cgPrim (LIntFloat _) = call "toReal"
cgPrim (LFloatInt _) = call "fromReal"
cgPrim LFloatStr = call "toString"
cgPrim LStrFloat = call "fromString"

cgPrim LFExp = call "exp"
cgPrim LFLog = call "log"
cgPrim LFSin = call "sin"
cgPrim LFCos = call "cos"
cgPrim LFTan = call "tan"
cgPrim LFASin = call "asin"
cgPrim LFACos = call "acos"
cgPrim LFATan = call "atan"
cgPrim LFSqrt = call "sqrt"
cgPrim LFFloor = call "floor"
cgPrim LFCeil  = call "ceil"
cgPrim LFNegate = call "~" -- \[x] -> text "~" <> x

cgPrim f = \_args -> cgUnsupported "primitive" f

-- Names -----------------------------------------------------------------------

cgVar :: LVar -> Doc
cgVar (Loc idx) = cgLoc idx --FIXME not in ir?
cgVar (Glob name) = cgName name

cgLoc :: Int -> Doc
cgLoc idx = "loc" <> int idx

cgName :: Name -> Doc
cgName = text . mangle

-- Unsupported -----------------------------------------------------------------

cgUnsupported :: Show a => Text -> a -> Doc
cgUnsupported cat val =
    call "abort" [dquotes $ "UNSUPPORTED" <+> text (toUpper cat) <+> string (show val)]
