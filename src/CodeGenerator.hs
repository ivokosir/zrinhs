module CodeGenerator
  ( Name (..),
    Module (..),
    Function (..),
    Block (..),
    Instruction (..),
    InstructionBase (..),
    Terminator (..),
    Value (..),
    generateCode,
  )
where

import AST hiding (Base (..), Expression)
import qualified AST as E
import Code
import Control.Monad.State (State, gets, modify', runState)
import Data.Map.Strict (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import TypeChecker
import Prelude hiding (lookup)

data StateData = StateData
  { unnamedCounter :: Word,
    requestedName :: Maybe String,
    scope :: Map String Value,
    functions :: [Function],
    currentBlocks :: [Block],
    currentLabel :: Name,
    currentInstructions :: [Instruction]
  }
  deriving (Eq, Show)

type S = State StateData

initialState =
  StateData
    { unnamedCounter = 1,
      scope = empty,
      requestedName = Nothing,
      functions = [],
      currentBlocks = [],
      currentLabel = IName 0,
      currentInstructions = []
    }

generateName :: Maybe String -> S Name
generateName (Just name) = return (Name name)
generateName Nothing = do
  i <- gets unnamedCounter
  modify' (\s -> s {unnamedCounter = unnamedCounter s + 1})
  return (IName i)

takeRequestedName :: S (Maybe String)
takeRequestedName = do
  name <- gets requestedName
  setRequestedName Nothing
  return name

setRequestedName :: Maybe String -> S ()
setRequestedName name = modify' (\s -> s {requestedName = name})

addInstruction :: Instruction -> S Value
addInstruction instruction@(Instruction name type_ _) = do
  modify' (\s -> s {currentInstructions = currentInstructions s ++ [instruction]})
  return (Reference name type_)

terminateBlock :: Terminator -> Name -> S ()
terminateBlock terminator nextLabel = do
  modify'
    ( \s ->
        let block = Block (currentLabel s) (currentInstructions s) terminator
         in s
              { currentBlocks = currentBlocks s ++ [block],
                currentInstructions = [],
                currentLabel = nextLabel
              }
    )

setName :: String -> Value -> S ()
setName name value =
  modify' (\s -> s {scope = insert name value (scope s)})

withScope :: S a -> S a
withScope f = do
  oldScope <- gets scope
  a <- f
  modify' (\s -> s {scope = oldScope})
  return a

generate :: Expression -> S Value
generate (E.Expression (E.Block es e) _) = do
  withScope
    ( do
        requestedName <- takeRequestedName
        mapM_ generate es
        setRequestedName requestedName
        generate e
    )
generate (E.Expression (E.Definition name e) _) = do
  parentName <- takeRequestedName
  let bestName = fromMaybe name parentName
  setRequestedName (Just bestName)

  v <- withScope (generate e)

  takeRequestedName
  setName bestName v

  return v
generate (E.Expression (E.IfThenElse cond then_ else_) type_) = do
  requestedName <- takeRequestedName

  condValue <- withScope (generate cond)
  thenLabel <- generateName Nothing
  elseLabel <- generateName Nothing
  terminateBlock (CondBr condValue thenLabel elseLabel) thenLabel

  thenValue <- withScope (generate then_)
  endLabel <- generateName Nothing
  terminateBlock (Br endLabel) elseLabel

  elseValue <- withScope (generate else_)
  terminateBlock (Br endLabel) endLabel

  phiName <- generateName requestedName
  addInstruction (Instruction phiName type_ (Phi thenValue thenLabel elseValue elseLabel))
generate (E.Expression (E.Operation op lhs rhs) type_) = do
  requestedName <- takeRequestedName

  lhsValue <- withScope (generate lhs)
  rhsValue <- withScope (generate rhs)

  name <- generateName requestedName
  addInstruction (Instruction name type_ (Binary op lhsValue rhsValue))
generate (E.Expression (E.Function param body) type_) = do
  requestedName <- takeRequestedName
  name <- generateName requestedName

  modify'
    ( \s ->
        let fs = generateFunction name type_ param body
         in s {functions = functions s ++ fs}
    )
  return (Reference name type_)
generate (E.Expression (E.Call caller arg) type_) = do
  requestedName <- takeRequestedName

  argValue <- withScope (generate arg)
  callerValue <- withScope (generate caller)

  name <- generateName requestedName
  addInstruction (Instruction name type_ (Call callerValue argValue))
generate (E.Expression (E.Identifier name) type_) =
  gets (fromMaybe (Reference (Name name) type_) . lookup name . scope)
generate (E.Expression (E.Literal c) _) = return (Const c)

generateFunction :: Name -> Type -> String -> Expression -> [Function]
generateFunction name type_ param body =
  let (TFunction retType paramType) = type_
      (bodyValue, state) = runState (generate body) initialState
      lastBlock = Block (currentLabel state) (currentInstructions state) (Ret bodyValue)
      blocks = currentBlocks state ++ [lastBlock]
      function = Function name retType (Name param) paramType blocks
   in functions state ++ [function]

generateCode :: Expression -> Module
generateCode e =
  let fs = generateFunction (Name "main") (TFunction TInt TUnit) "" e
   in Module "module" fs
