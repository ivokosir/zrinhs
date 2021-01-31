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
  { scope :: Map String Value,
    requestedName :: Maybe String,
    nextLabel :: Word,
    nextIName :: Word,
    functions :: [Function],
    currentLabel :: Label,
    currentBlocks :: [Block],
    currentInstructions :: [Instruction]
  }
  deriving (Eq, Show)

type S = State StateData

initialState =
  StateData
    { scope = empty,
      requestedName = Nothing,
      nextLabel = 1,
      nextIName = 0,
      functions = [],
      currentLabel = Label 0,
      currentBlocks = [],
      currentInstructions = []
    }

generateName :: Maybe String -> S Name
generateName (Just name) = return (Name name)
generateName Nothing = do
  i <- gets nextIName
  modify' (\s -> s {nextIName = i + 1})
  return (IName i)

generateLabel :: S Label
generateLabel = do
  label <- gets nextLabel
  modify' (\s -> s {nextLabel = label + 1})
  return (Label label)

takeRequestedName :: S (Maybe String)
takeRequestedName = do
  name <- gets requestedName
  setRequestedName Nothing
  return name

setRequestedName :: Maybe String -> S ()
setRequestedName name = modify' (\s -> s {requestedName = name})

addInstruction :: Instruction -> S Value
addInstruction instruction@(Instruction name _) = do
  modify' (\s -> s {currentInstructions = currentInstructions s ++ [instruction]})
  return (Reference name)

terminateBlock :: Terminator -> Label -> S ()
terminateBlock terminator nextLabel = do
  modify'
    ( \s ->
        let block = Block (currentLabel s) (currentInstructions s) terminator
         in s
              { currentLabel = nextLabel,
                currentBlocks = currentBlocks s ++ [block],
                currentInstructions = []
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
generate (E.Expression (E.IfThenElse cond then_ else_) _) = do
  requestedName <- takeRequestedName

  condValue <- withScope (generate cond)
  thenLabel <- generateLabel
  elseLabel <- generateLabel
  terminateBlock (CondBr condValue thenLabel elseLabel) thenLabel

  thenValue <- withScope (generate then_)
  thenLabel <- gets currentLabel
  endLabel <- generateLabel
  terminateBlock (Br endLabel) elseLabel

  elseValue <- withScope (generate else_)
  elseLabel <- gets currentLabel
  terminateBlock (Br endLabel) endLabel

  phiName <- generateName requestedName
  addInstruction (Instruction phiName (Phi thenValue thenLabel elseValue elseLabel))
generate (E.Expression (E.Operation op lhs rhs) _) = do
  requestedName <- takeRequestedName

  lhsValue <- withScope (generate lhs)
  rhsValue <- withScope (generate rhs)

  name <- generateName requestedName
  addInstruction (Instruction name (Binary op lhsValue rhsValue))
generate (E.Expression (E.Function param body) type_) = do
  requestedName <- takeRequestedName
  name <- generateName requestedName

  modify'
    ( \s ->
        let fs = generateFunction name type_ param body
         in s {functions = functions s ++ fs}
    )
  return (Reference name)
generate (E.Expression (E.Call caller arg) _) = do
  requestedName <- takeRequestedName

  argValue <- withScope (generate arg)
  callerValue <- withScope (generate caller)

  name <- generateName requestedName
  addInstruction (Instruction name (Call callerValue argValue))
generate (E.Expression (E.Identifier name) _) =
  gets (fromMaybe (Reference (Name name)) . lookup name . scope)
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
