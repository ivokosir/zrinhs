module AST
  ( Operation (..),
    Constant (..),
    ExpressionGeneric (..),
    updateExtra,
    setExtra,
    setBase,
    ExpressionBase (..),
    Type (..),
    constType,
    operatonTypes,
    IncompleteType,
    complete,
    incomplete,
    incompleteUnknown,
    incompleteTuple,
    incompleteFunction,
    incompleteRetAndParam,
  )
where

import Data.List (intercalate)

data Operation
  = Or
  | And
  | Equal
  | NotEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  deriving (Eq, Show)

data Constant
  = CUnit
  | CBool Bool
  | CInt Int
  | CString String
  deriving (Eq, Show)

data ExpressionGeneric extra = Expression {base :: ExpressionBase (ExpressionGeneric extra), extra :: extra} deriving (Eq, Show)

updateExtra :: (extra -> extra) -> ExpressionGeneric extra -> ExpressionGeneric extra
updateExtra f e = e {extra = f (extra e)}

setExtra :: extra -> ExpressionGeneric extra -> ExpressionGeneric extra
setExtra extra = updateExtra (const extra)

setBase :: ExpressionBase (ExpressionGeneric extra) -> ExpressionGeneric extra -> ExpressionGeneric extra
setBase newBase e = e {base = newBase}

data ExpressionBase e
  = Block [e] e
  | Tuple [e]
  | Definition String e
  | IfThenElse e e e
  | Operation Operation e e
  | Function String e
  | Call e e
  | Identifier String
  | Literal Constant
  deriving (Eq, Show)

instance Functor ExpressionBase where
  fmap f (Block es e) = Block (fmap f es) (f e)
  fmap f (Tuple es) = Tuple (fmap f es)
  fmap f (Definition name e) = Definition name (f e)
  fmap f (IfThenElse cond then_ else_) = IfThenElse (f cond) (f then_) (f else_)
  fmap f (Operation op lhs rhs) = Operation op (f lhs) (f rhs)
  fmap f (Function param e) = Function param (f e)
  fmap f (Call caller arg) = Call (f caller) (f arg)
  fmap _ (Identifier name) = Identifier name
  fmap _ (Literal constant) = Literal constant

instance Functor ExpressionGeneric where
  fmap f (Expression base extra) = Expression ((fmap . fmap) f base) (f extra)

instance Foldable ExpressionBase where
  foldMap f (Block es e) = foldMap f es <> f e
  foldMap f (Tuple es) = foldMap f es
  foldMap f (Definition _ e) = f e
  foldMap f (IfThenElse cond then_ else_) = f cond <> f then_ <> f else_
  foldMap f (Operation _ lhs rhs) = f lhs <> f rhs
  foldMap f (Function _ e) = f e
  foldMap f (Call caller arg) = f caller <> f arg
  foldMap _ (Identifier _) = mempty
  foldMap _ (Literal _) = mempty

instance Foldable ExpressionGeneric where
  foldMap f (Expression base extra) = f extra <> foldMap (foldMap f) base

instance Traversable ExpressionBase where
  sequenceA (Block es e) = Block <$> sequenceA es <*> e
  sequenceA (Tuple es) = Tuple <$> sequenceA es
  sequenceA (Definition name e) = Definition name <$> e
  sequenceA (IfThenElse cond then_ else_) = IfThenElse <$> cond <*> then_ <*> else_
  sequenceA (Operation op lhs rhs) = Operation op <$> lhs <*> rhs
  sequenceA (Function param e) = Function param <$> e
  sequenceA (Call caller arg) = Call <$> caller <*> arg
  sequenceA (Identifier name) = pure (Identifier name)
  sequenceA (Literal constant) = pure (Literal constant)

instance Traversable ExpressionGeneric where
  sequenceA (Expression base extra) = Expression <$> traverse sequenceA base <*> extra

data Type
  = TUnit
  | TBool
  | TInt
  | TString
  | TFunction Type Type
  | TTuple [Type]
  deriving (Eq)

instance Show Type where
  show TUnit = "()"
  show TBool = "bool"
  show TInt = "int"
  show TString = "string"
  show (TFunction ret param) = show param ++ " -> " ++ show ret
  show (TTuple ts) = "(" ++ intercalate "," (fmap show ts) ++ ")"

constType :: Constant -> Type
constType CUnit = TUnit
constType (CBool _) = TBool
constType (CInt _) = TInt
constType (CString _) = TString

operatonTypes :: Operation -> (Type, Type, Type)
operatonTypes Or = (TBool, TBool, TBool)
operatonTypes And = (TBool, TBool, TBool)
operatonTypes Equal = (TBool, TInt, TInt)
operatonTypes NotEqual = (TBool, TInt, TInt)
operatonTypes Greater = (TBool, TInt, TInt)
operatonTypes GreaterEqual = (TBool, TInt, TInt)
operatonTypes Less = (TBool, TInt, TInt)
operatonTypes LessEqual = (TBool, TInt, TInt)
operatonTypes Add = (TInt, TInt, TInt)
operatonTypes Subtract = (TInt, TInt, TInt)
operatonTypes Multiply = (TInt, TInt, TInt)
operatonTypes Divide = (TInt, TInt, TInt)
operatonTypes Remainder = (TInt, TInt, TInt)

data IncompleteType
  = ITUnknown
  | ITComplete Type
  | ITFunction IncompleteType IncompleteType
  | ITTuple [IncompleteType]
  deriving (Eq)

instance Show IncompleteType where
  show ITUnknown = "unknown"
  show (ITComplete t) = show t
  show (ITFunction ret param) = show ret ++ " -> " ++ show param
  show (ITTuple ts) = "(" ++ intercalate "," (fmap show ts) ++ ")"

complete :: IncompleteType -> Maybe Type
complete ITUnknown = Nothing
complete (ITComplete t) = Just t
complete (ITFunction ret param) = TFunction <$> complete ret <*> complete param
complete (ITTuple es) = TTuple <$> mapM complete es

incomplete :: Type -> IncompleteType
incomplete = ITComplete

incompleteUnknown :: IncompleteType
incompleteUnknown = ITUnknown

incompleteFunction :: IncompleteType -> IncompleteType -> IncompleteType
incompleteFunction (ITComplete ret) (ITComplete param) = ITComplete (TFunction ret param)
incompleteFunction ret param = ITFunction ret param

incompleteTuple :: [IncompleteType] -> IncompleteType
incompleteTuple ts = maybe (ITTuple ts) (ITComplete . TTuple) (mapM complete ts)

incompleteRetAndParam :: IncompleteType -> Maybe (IncompleteType, IncompleteType)
incompleteRetAndParam (ITFunction ret param) = Just (ret, param)
incompleteRetAndParam (ITComplete (TFunction ret param)) = Just (ITComplete ret, ITComplete param)
incompleteRetAndParam ITUnknown = Just (ITUnknown, ITUnknown)
incompleteRetAndParam _ = Nothing

instance Semigroup IncompleteType where
  (ITComplete t) <> _ = ITComplete t
  _ <> (ITComplete t) = ITComplete t
  (ITTuple es1) <> (ITTuple es2) | length es1 /= length es2 = ITTuple es1
  (ITTuple es1) <> (ITTuple es2) = incompleteTuple (fmap (uncurry (<>)) (zip es1 es2))
  (ITFunction ret1 param1) <> (ITFunction ret2 param2) =
    incompleteFunction (ret1 <> ret2) (param1 <> param2)
  ITUnknown <> t = t
  t <> _ = t

instance Monoid IncompleteType where
  mappend = (<>)
  mempty = incompleteUnknown
