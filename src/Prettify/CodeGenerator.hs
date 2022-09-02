module Prettify.CodeGenerator (prettify) where

import AST (Constant (..), Type (..))
import Code
import Data.Char (toLower)
import Data.List (intercalate)

prettifyConst :: Constant -> String
prettifyConst CUnit = "null"
prettifyConst (CBool True) = "true"
prettifyConst (CBool False) = "false"
prettifyConst (CInt i) = show i
prettifyConst (CString s) = "\"" ++ s ++ "\""

prettifyName :: Name -> String
prettifyName (IName i) = "%" ++ show i
prettifyName (Name s) = "%\"" ++ s ++ "\""

prettifyLabel :: Label -> String
prettifyLabel (Label i) = show i

prettifyType :: Type -> String
prettifyType TUnit = "void"
prettifyType TBool = "i1"
prettifyType TInt = "i32"
prettifyType TString = "i8*"
prettifyType (TFunction ret arg) = prettifyType ret ++ "(" ++ prettifyType arg ++ ")"
prettifyType (TTuple ts) = "{" ++ intercalate ", " (fmap prettifyType ts) ++ "}"

prettifyValue :: Value -> String
prettifyValue (Reference name) = prettifyName name
prettifyValue (Const c) = prettifyConst c

prettifyInstructionBase :: InstructionBase -> String
prettifyInstructionBase (Binary op lhs rhs) =
  map toLower (show op)
    ++ " "
    ++ prettifyValue lhs
    ++ " "
    ++ prettifyValue rhs
prettifyInstructionBase (Call caller callee) =
  "call "
    ++ prettifyValue caller
    ++ "("
    ++ prettifyValue callee
    ++ ")"
prettifyInstructionBase (Phi value1 label1 value2 label2) =
  "phi "
    ++ prettifyValue value1
    ++ " "
    ++ prettifyLabel label1
    ++ " "
    ++ prettifyValue value2
    ++ " "
    ++ prettifyLabel label2
prettifyInstructionBase (Struct vs) =
  "struct {" ++ intercalate ", " (fmap prettifyValue vs) ++ "}"

prettifyInstruction :: Instruction -> String
prettifyInstruction (Instruction name i) =
  "  "
    ++ prettifyName name
    ++ " = "
    ++ prettifyInstructionBase i
    ++ "\n"

prettifyTerminator :: Terminator -> String
prettifyTerminator (CondBr cond rhs lhs) =
  "  cond_br "
    ++ prettifyValue cond
    ++ " "
    ++ prettifyLabel rhs
    ++ " "
    ++ prettifyLabel lhs
    ++ "\n"
prettifyTerminator (Ret v) = "  ret " ++ prettifyValue v ++ "\n"
prettifyTerminator (Br br) = "  br " ++ prettifyLabel br ++ "\n"

prettifyBlock :: Block -> String
prettifyBlock (Block label is t) =
  prettifyLabel label
    ++ ":\n"
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
