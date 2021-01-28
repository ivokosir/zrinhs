module TypeChecker (Expression, checkTypes) where

import AST hiding (Expression)
import qualified AST as E
import Control.Monad.State (State, gets, modify', runState)
import Data.Map.Strict (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Error
import qualified Parser as P
import Text.Parsec (SourcePos)
import Prelude hiding (lookup)

data StateData = StateData
  { scope :: Map String IncompleteType,
    requested :: Map String IncompleteType,
    errors :: [Error]
  }
  deriving (Eq, Show)

initialState = StateData empty empty []

type S = State StateData

setNameType :: String -> IncompleteType -> S ()
setNameType name type_ =
  modify' (\s -> s {scope = insert name type_ (scope s)})

getNameType :: String -> S IncompleteType
getNameType name = gets (fromMaybe incompleteUnknown . lookup name . scope)

request :: String -> IncompleteType -> S ()
request name type_ = do
  oldRequested <- getRequested name
  modify' (\s -> s {requested = insert name (oldRequested <> type_) (requested s)})

getRequested :: String -> S IncompleteType
getRequested name = gets (fromMaybe incompleteUnknown . lookup name . requested)

data Extra = Extra
  { extraStart :: SourcePos,
    extraEnd :: SourcePos,
    extraType :: IncompleteType
  }
  deriving (Eq, Show)

type IncompleteExpression = E.Expression Extra

setType :: IncompleteType -> IncompleteExpression -> IncompleteExpression
setType t = updateExtra (\extra -> extra {extraType = t})

getType :: IncompleteExpression -> IncompleteType
getType = extraType . extra

addError :: IncompleteExpression -> String -> S ()
addError e error =
  let (Extra start end _) = extra e
   in modify' (\s -> s {errors = Error start end error : errors s})

expectType :: IncompleteExpression -> IncompleteType -> IncompleteType -> S IncompleteType
expectType e actual expected =
  case (complete actual, complete expected) of
    (Just actual, Just expected) | actual /= expected -> do
      addError e ("expected '" ++ show expected ++ "', got '" ++ show actual ++ "'")
      return (incomplete actual)
    _ -> return (actual <> expected)

check :: IncompleteExpression -> S IncompleteExpression
check e = do
  let expected = getType e

  (newBase, newType) <- case base e of
    (Block es e) -> do
      es <- mapM check es
      e <- check (setType expected e)

      return (Block es e, getType e)
    (Definition name e) -> do
      requested <- getRequested name
      e <- check (setType (expected <> requested) e)

      let type_ = getType e
      setNameType name type_

      return (Definition name e, type_)
    (IfThenElse cond then_ else_) -> do
      cond <- check (setType (incomplete TBool) cond)
      then_ <- check (setType expected then_)
      let type_ = getType then_
      else_ <- check (setType type_ else_)

      return (IfThenElse cond then_ else_, type_)
    (Operation op lhs rhs) -> do
      let (result, expectedLhs, expectedRhs) = operatonTypes op

      lhs <- check (setType (incomplete expectedLhs) lhs)
      rhs <- check (setType (incomplete expectedRhs) rhs)

      return (Operation op lhs rhs, incomplete result)
    (Function param body) -> do
      (retType, expectedParamType) <- case incompleteRetAndParam expected of
        Just (retType, paramType) -> return (retType, paramType)
        Nothing -> do
          addError e ("expected '" ++ show expected ++ "', got a function")
          return (incompleteUnknown, incompleteUnknown)

      requestedParamType <- getRequested param
      let paramType = expectedParamType <> requestedParamType
      setNameType param paramType

      body <- check (setType retType body)

      let type_ = incompleteFunction paramType (getType body)

      return (Function param body, type_)
    (Call caller arg) -> do
      arg <- check arg

      caller <- check (setType (incompleteFunction expected (getType arg)) caller)

      let callerType = getType caller
      case incompleteRetAndParam callerType of
        Just (retType, argType) ->
          return (Call caller (setType argType arg), retType)
        Nothing -> do
          addError caller ("expected a function, got '" ++ show callerType ++ "'")
          return (Call caller arg, incompleteUnknown)
    (Identifier name) -> do
      nameType <- getNameType name
      let type_ = nameType <> expected
      request name type_
      return (Identifier name, type_)
    (Literal c) -> return (Literal c, incomplete (constType c))

  newType <- expectType e newType expected

  return (setType newType (setBase newBase e))

type Expression = E.Expression Type

converge :: IncompleteExpression -> S IncompleteExpression
converge old = do
  new <- check old
  if old == new
    then return new
    else converge new

checkTypes :: P.Expression -> Either [Error] Expression
checkTypes pe =
  let castExtraToIncomplete (P.Extra start end) = Extra start end incompleteUnknown
      unchecked = fmap castExtraToIncomplete pe
      (checked, s) = runState (converge (setType (incomplete TInt) unchecked)) initialState
      castExtraToComplete (Extra start end incompleteType) = case complete incompleteType of
        Just t -> Right t
        Nothing -> Left (start, end)
      typed = traverse castExtraToComplete checked
   in case errors s of
        [] -> case typed of
          Right typed -> Right typed
          Left (start, end) ->
            Left [Error start end "unexpected error, could not resolve type"]
        errors -> Left errors
