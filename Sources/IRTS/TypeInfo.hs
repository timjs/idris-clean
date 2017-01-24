module IRTS.TypeInfo where

import Idris.Core.TT
import Idris.Core.Evaluate

import IRTS.Lang

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

-- Basic type information propagated through IR's for optimised code generation
data BasicTy = BTFun BasicTy BasicTy
             | BTArith ArithTy
             | BTBool
             | BTString
             | BTAny
    deriving (Show, Eq)

type Idx = Int

type TyInfo = Map Name [BasicTy]

mkTyInfo :: [(Name,[Idx])] -> [(Name,TTDecl)] -> TyInfo
mkTyInfo usageinfos = foldr go Map.empty
  where
    go :: (Name,TTDecl) -> TyInfo -> TyInfo
    go (name, (TyDecl _ ty,_,_,_,_,_)) = Map.insert name $ irType (fromMaybe useAllArgs $ lookup name usageinfos) ty
    go (name, (CaseOp _ ty _ _ _ _,_,_,_,_,_)) = Map.insert name $ irType useAllArgs ty

    useAllArgs :: [Idx]
    useAllArgs = iterate (+1) 0

irType :: [Idx] -> Type -> [BasicTy]
irType is ty = select is $ decombine (convert ty) --trace (">> FROM " ++ show ty ++ "\n   TO   " ++ show res) res
  where
    convert :: Type -> BasicTy
    convert (Constant StrType) = BTString
    convert (Constant (AType at)) = BTArith at
    convert (P _ name _)
        | name == sNS (sUN "Bool") ["Bool", "Prelude"] = BTBool
    convert (Bind _nametype (Pi _rigcount _implicit ty _kind) cont) = BTFun (convert ty) (convert cont)
    convert  _ = BTAny
    --XXX WorldType and VoidType needed?

    decombine :: BasicTy -> [BasicTy]
    decombine (BTFun a b) = a : decombine b
    decombine bt = [bt]

    select :: [Int] -> [a] -> [a]
    select is xs = select' is xs 0
      where
        select' [] _ _ = []
        select' _ [] _ = []
        select' (i:is) (x:xs) n
          | i == n = x : select' is xs (n + 1)
          | otherwise = select' (i:is) xs (n + 1)


irTypeOp :: PrimFn -> [BasicTy]
irTypeOp (LPlus at) = replicate 3 (BTArith at)
irTypeOp (LMinus at) = replicate 3 (BTArith at)
irTypeOp (LTimes at) = replicate 3 (BTArith at)

irTypeOp (LUDiv it) = replicate 3 (BTArith (ATInt it))
irTypeOp (LSDiv at) = replicate 3 (BTArith at)
irTypeOp (LURem it) = replicate 3 (BTArith (ATInt it))
irTypeOp (LSRem at) = replicate 3 (BTArith at)

irTypeOp (LAnd it) = replicate 3 (BTArith (ATInt it))
irTypeOp (LOr it) = replicate 3 (BTArith (ATInt it))
irTypeOp (LXOr it) = replicate 3 (BTArith (ATInt it))
irTypeOp (LCompl it) = replicate 3 (BTArith (ATInt it))

irTypeOp (LSHL it) = replicate 3 (BTArith (ATInt it))
irTypeOp (LLSHR it) = replicate 3 (BTArith (ATInt it))
irTypeOp (LASHR it) = replicate 3 (BTArith (ATInt it))

irTypeOp (LEq at) = replicate 2 (BTArith at) ++ [BTBool]
irTypeOp (LLt it) = replicate 2 (BTArith (ATInt it)) ++ [BTBool]
irTypeOp (LLe it) = replicate 2 (BTArith (ATInt it)) ++ [BTBool]
irTypeOp (LGt it) = replicate 2 (BTArith (ATInt it)) ++ [BTBool]
irTypeOp (LGe it) = replicate 2 (BTArith (ATInt it)) ++ [BTBool]

irTypeOp (LSLt at) = replicate 2 (BTArith at) ++ [BTBool]
irTypeOp (LSLe at) = replicate 2 (BTArith at) ++ [BTBool]
irTypeOp (LSGt at) = replicate 2 (BTArith at) ++ [BTBool]
irTypeOp (LSGe at) = replicate 2 (BTArith at) ++ [BTBool]

irTypeOp (LSExt fr to) = [BTArith (ATInt fr), BTArith (ATInt to)]
irTypeOp (LZExt fr to) = [BTArith (ATInt fr), BTArith (ATInt to)]
irTypeOp (LTrunc fr to) = [BTArith (ATInt fr), BTArith (ATInt to)]

irTypeOp LStrConcat = replicate 3 BTString
irTypeOp LStrLt = replicate 2 BTString ++ [BTBool]
irTypeOp LStrEq = replicate 2 BTString ++ [BTBool]
irTypeOp LStrLen = [BTString, BTArith (ATInt ITNative)]

irTypeOp (LIntFloat it) = [BTArith (ATInt it), BTArith ATFloat]
irTypeOp (LFloatInt it) = [BTArith ATFloat, BTArith (ATInt it)]
irTypeOp (LIntStr it) = [BTArith (ATInt it), BTString]
irTypeOp (LStrInt it) = [BTString, BTArith (ATInt it)]
irTypeOp LFloatStr = [BTArith ATFloat, BTString]
irTypeOp LStrFloat = [BTString, BTArith ATFloat]
irTypeOp (LChInt it) = [BTArith (ATInt ITChar), BTArith (ATInt it)]
irTypeOp (LIntCh it) = [BTArith (ATInt it), BTArith (ATInt ITChar)]

irTypeOp (LBitCast fr to) = [BTArith fr, BTArith to]

irTypeOp LFExp = replicate 3 (BTArith ATFloat)
irTypeOp LFLog = replicate 3 (BTArith ATFloat)
irTypeOp LFSin = replicate 3 (BTArith ATFloat)
irTypeOp LFCos = replicate 3 (BTArith ATFloat)
irTypeOp LFTan = replicate 3 (BTArith ATFloat)
irTypeOp LFASin = replicate 3 (BTArith ATFloat)
irTypeOp LFACos = replicate 3 (BTArith ATFloat)
irTypeOp LFATan = replicate 3 (BTArith ATFloat)

irTypeOp LFSqrt = replicate 2 (BTArith ATFloat)
irTypeOp LFFloor = replicate 2 (BTArith ATFloat)
irTypeOp LFCeil = replicate 2 (BTArith ATFloat)
irTypeOp LFNegate = replicate 2 (BTArith ATFloat)

irTypeOp LStrHead = [BTString, BTArith (ATInt ITChar)]
irTypeOp LStrTail = [BTString, BTString]
irTypeOp LStrCons = [BTArith (ATInt ITChar), BTString, BTString]
irTypeOp LStrIndex = [BTArith (ATInt ITNative), BTString, BTArith (ATInt ITChar)]
irTypeOp LStrRev = replicate 2 BTString
irTypeOp LStrSubstr = replicate 2 (BTArith (ATInt ITNative)) ++ replicate 2 BTString

irTypeOp _ = repeat BTAny
{-
irTypeOp LReadStr
irTypeOp LWriteStr

irTypeOp LSystemInfo

irTypeOp LFork
irTypeOp LPar
irTypeOp (LExternal Name)
irTypeOp LCrash

irTypeOp LNoOp
-}
