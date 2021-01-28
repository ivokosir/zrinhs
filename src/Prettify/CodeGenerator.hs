module Prettify.CodeGenerator (prettify) where

import AST hiding (Base (..))
import CodeGenerator
import Data.Char (toLower)

prettifyConst :: Constant -> String
prettifyConst CUnit = "null"
prettifyConst (CBool True) = "1"
prettifyConst (CBool False) = "0"
prettifyConst (CInt i) = show i
prettifyConst (CString s) = "\"" ++ s ++ "\""

prettifyName :: Name -> String
prettifyName (IName i) = "%" ++ show i
prettifyName (Name s) = "%\"" ++ s ++ "\""

prettifyType :: Type -> String
prettifyType TUnit = "void"
prettifyType TBool = "i1"
prettifyType TInt = "i32"
prettifyType TString = "i8*"
prettifyType (TFunction ret arg) = prettifyType ret ++ "(" ++ prettifyType arg ++ ")"

prettifyValue :: Value -> String
prettifyValue (Reference name _) = prettifyName name
prettifyValue (Const c) = prettifyConst c

prettifyArg :: Value -> String
prettifyArg value =
  let type_ = case value of
        Reference _ type_ -> type_
        Const c -> constType c
   in prettifyType type_ ++ " " ++ prettifyValue value

prettifyInstructionBase :: InstructionBase -> Type -> String
prettifyInstructionBase (Binary op lhs rhs) type_ =
  map toLower (show op)
    ++ " "
    ++ prettifyType type_
    ++ " "
    ++ prettifyValue lhs
    ++ " "
    ++ prettifyValue rhs
prettifyInstructionBase (Call caller callee) type_ =
  "call "
    ++ prettifyType type_
    ++ " "
    ++ prettifyValue caller
    ++ "("
    ++ prettifyArg callee
    ++ ")"
prettifyInstructionBase (Phi value1 label1 value2 label2) type_ =
  "phi "
    ++ prettifyType type_
    ++ " "
    ++ prettifyValue value1
    ++ " "
    ++ prettifyName label1
    ++ " "
    ++ prettifyValue value2
    ++ " "
    ++ prettifyName label2

prettifyInstruction :: Instruction -> String
prettifyInstruction (Instruction name t i) =
  "  "
    ++ prettifyName name
    ++ " = "
    ++ prettifyInstructionBase i t
    ++ "\n"

prettifyTerminator :: Terminator -> String
prettifyTerminator (CondBr cond rhs lhs) =
  "  cond_br "
    ++ prettifyValue cond
    ++ " "
    ++ prettifyName rhs
    ++ " "
    ++ prettifyName lhs
    ++ "\n"
prettifyTerminator (Ret v) = "  ret " ++ prettifyValue v ++ "\n"
prettifyTerminator (Br br) = "  br " ++ prettifyName br ++ "\n"

prettifyBlock :: Block -> String
prettifyBlock (Block label is t) =
  prettifyName label
    ++ "\n"
    ++ concatMap prettifyInstruction is
    ++ prettifyTerminator t

prettifyFunction :: Function -> String
prettifyFunction (Function name retType param paramType bbs) =
  "declare "
    ++ prettifyType retType
    ++ " "
    ++ prettifyName name
    ++ "("
    ++ prettifyType paramType
    ++ " "
    ++ prettifyName param
    ++ ") {\n"
    ++ concatMap prettifyBlock bbs
    ++ "}\n"

prettify :: Module -> String
prettify (Module name fs) =
  "module \"" ++ name ++ "\"\n" ++ concatMap prettifyFunction fs
