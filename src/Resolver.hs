module Resolver where

import AST hiding (Expression)
import Control.Monad.State (State, gets, modify', runState)
import Data.Map.Strict (Map, empty, insert, lookup)
import Error
import Parser
import Prelude hiding (lookup)

data StateData = StateData
  { uniqueCounter :: Map String Int,
    scope :: Map String String,
    errors :: [Error]
  }

type S = State StateData

initialState =
  StateData
    { uniqueCounter = empty,
      scope = empty,
      errors = []
    }

addError :: Expression -> String -> S ()
addError e error =
  let (Extra start end) = extra e
   in modify' (\s -> s {errors = Error start end error : errors s})

generateName :: String -> S String
generateName name = do
  counter <- gets uniqueCounter
  let (newName, newId) = case lookup name counter of
        Just id -> (name ++ "." ++ show id, id + 1)
        Nothing -> (name, 0)
  modify' (\s -> s {uniqueCounter = insert name newId counter})
  return newName

addToScope :: String -> String -> S ()
addToScope name uniqueName = modify' (\s -> s {scope = insert name uniqueName (scope s)})

withScope :: S a -> S a
withScope f = do
  parentScope <- gets scope
  a <- f
  modify' (\s -> s {scope = parentScope})
  return a

resolve' :: Expression -> S Expression
resolve' e = do
  newBase <-
    case base e of
      Block es e -> do
        withScope
          ( do
              es <- mapM resolve' es
              e <- resolve' e
              return (Block es e)
          )
      Definition name e -> do
        uniqueName <- generateName name
        e <- withScope (resolve' e)
        addToScope name uniqueName
        return (Definition uniqueName e)
      IfThenElse cond then_ else_ ->
        IfThenElse <$> withScope (resolve' cond) <*> withScope (resolve' then_) <*> withScope (resolve' else_)
      Operation op lhs rhs -> Operation op <$> withScope (resolve' lhs) <*> withScope (resolve' rhs)
      Function param e -> do
        modify' (\s -> s {uniqueCounter = empty, scope = insert param param empty})
        Function param <$> withScope (resolve' e)
      Call caller arg -> Call <$> resolve' caller <*> resolve' arg
      Identifier name -> do
        unuqueName <- gets (lookup name . scope)
        case unuqueName of
          Just unuqueName -> pure (Identifier unuqueName)
          Nothing -> do
            addError e "undefined name"
            pure (Identifier name)
      Literal constant -> pure (Literal constant)
  return (setBase newBase e)

resolve :: Expression -> Either [Error] Expression
resolve e =
  let (resolved, state) = runState (resolve' e) initialState
   in case errors state of
        [] -> Right resolved
        es -> Left es
