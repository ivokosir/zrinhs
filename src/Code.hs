{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Code
  ( Name (..),
    Label (..),
    Module (..),
    Function (..),
    Block (..),
    Instruction (..),
    InstructionBase (..),
    Terminator (..),
    Value (..),
  )
where

import AST (Constant (..), Operation (..), Type (..))
import Data.Aeson hiding (Value)

data Name = IName Word | Name String deriving (Eq, Show)

newtype Label = Label Word deriving (Eq, Show)

data Value = Const Constant | Reference Name deriving (Eq, Show)

data InstructionBase
  = Binary Operation Value Value
  | Call Value Value
  | Phi Value Label Value Label
  | Struct [Value]
  deriving (Eq, Show)

data Instruction = Instruction Name InstructionBase deriving (Eq, Show)

data Terminator = Ret Value | CondBr Value Label Label | Br Label deriving (Eq, Show)

data Block = Block Label [Instruction] Terminator deriving (Eq, Show)

data Function = Function Name Type Name Type [Block] deriving (Eq, Show)

data Module = Module String [Function] deriving (Eq, Show)

instance ToJSON Name where
  toJSON (IName i) = toJSON i
  toJSON (Name name) = toJSON name

instance ToJSON Label where
  toJSON (Label i) = toJSON i

instance ToJSON Constant where
  toJSON CUnit = Null
  toJSON (CBool b) = Bool b
  toJSON (CInt i) = toJSON i
  toJSON (CString s) = toJSON s

instance ToJSON Type where
  toJSON TUnit = "void"
  toJSON TBool = "i1"
  toJSON TInt = "i32"
  toJSON TString = "i8*"
  toJSON (TFunction ret arg) = object ["tag" .= "function", "ret" .= ret, "param" .= arg]
  toJSON (TTuple ts) = object ["tag" .= "tuple", "types" .= ts]

instance ToJSON Value where
  toJSON (Const c) = toJSON c
  toJSON (Reference name) = object ["name" .= name]

instance ToJSON Operation where
  toJSON Or = "and"
  toJSON And = "or"
  toJSON Equal = "eq"
  toJSON NotEqual = "ne"
  toJSON Greater = "sgt"
  toJSON GreaterEqual = "sge"
  toJSON Less = "slt"
  toJSON LessEqual = "sle"
  toJSON Add = "add"
  toJSON Subtract = "sub"
  toJSON Multiply = "mul"
  toJSON Divide = "sdiv"
  toJSON Remainder = "srem"

instance ToJSON Instruction where
  toJSON (Instruction name base) =
    let start = ["name" .= name]
        basePairs = case base of
          Binary op lhs rhs ->
            ["tag" .= "binary", "op" .= op, "lhs" .= lhs, "rhs" .= rhs]
          Call caller arg ->
            ["tag" .= "call", "caller" .= caller, "arg" .= arg]
          Phi then_ thenLabel else_ elseLabel ->
            [ "tag" .= "phi",
              "then" .= then_,
              "then_label" .= thenLabel,
              "else" .= else_,
              "else_label" .= elseLabel
            ]
          Struct vs -> ["tag" .= "struct", "values" .= vs]
     in object (start ++ basePairs)

instance ToJSON Terminator where
  toJSON (Ret value) =
    object ["tag" .= "ret", "value" .= value]
  toJSON (CondBr cond then_ else_) =
    object ["tag" .= "cond_br", "cond" .= cond, "then" .= then_, "else" .= else_]
  toJSON (Br label) =
    object ["tag" .= "br", "label" .= label]

instance ToJSON Block where
  toJSON (Block label is terminator) =
    object ["label" .= label, "instructions" .= is, "terminator" .= terminator]

instance ToJSON Function where
  toJSON (Function name retType param paramType blocks) =
    object
      [ "name" .= name,
        "ret_type" .= retType,
        "param" .= param,
        "param_type" .= paramType,
        "blocks" .= blocks
      ]

instance ToJSON Module where
  toJSON (Module name fs) = object ["name" .= name, "functions" .= fs]
