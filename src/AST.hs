module AST
  ( Operation (..),
    Constant (..),
    Expression (..),
    updateExtra,
    setExtra,
    setBase,
    Base (..),
    Type (..),
    constType,
    operatonTypes,
    IncompleteType,
    complete,
    incomplete,
    incompleteUnknown,
    incompleteFunction,
    incompleteRetAndParam,
  )
where

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

data Expression extra = Expression {base :: Base extra, extra :: extra} deriving (Eq, Show)

updateExtra :: (extra -> extra) -> Expression extra -> Expression extra
updateExtra f e = e {extra = f (extra e)}

setExtra :: extra -> Expression extra -> Expression extra
setExtra extra = updateExtra (const extra)

setBase :: Base extra -> Expression extra -> Expression extra
setBase newBase e = e {base = newBase}

data Base extra
  = Block [Expression extra] (Expression extra)
  | Definition String (Expression extra)
  | IfThenElse (Expression extra) (Expression extra) (Expression extra)
  | Operation Operation (Expression extra) (Expression extra)
  | Function String (Expression extra)
  | Call (Expression extra) (Expression extra)
  | Identifier String
  | Literal Constant
  deriving (Eq, Show)

instance Functor Expression where
  fmap f (Expression base extra) = Expression (mapExpression base) (f extra)
    where
      mapExpression (Block es e) = Block ((fmap . fmap) f es) (fmap f e)
      mapExpression (Definition name e) = Definition name (fmap f e)
      mapExpression (IfThenElse cond then_ else_) = IfThenElse (fmap f cond) (fmap f then_) (fmap f else_)
      mapExpression (Operation op lhs rhs) = Operation op (fmap f lhs) (fmap f rhs)
      mapExpression (Function param e) = Function param (fmap f e)
      mapExpression (Call caller arg) = Call (fmap f caller) (fmap f arg)
      mapExpression (Identifier name) = Identifier name
      mapExpression (Literal constant) = Literal constant

instance Foldable Expression where
  foldMap f (Expression base extra) = foldMapExpression base <> f extra
    where
      foldMapExpression (Block es e) = (foldMap . foldMap) f es <> foldMap f e
      foldMapExpression (Definition _ e) = foldMap f e
      foldMapExpression (IfThenElse cond then_ else_) = foldMap f cond <> foldMap f then_ <> foldMap f else_
      foldMapExpression (Operation _ lhs rhs) = foldMap f lhs <> foldMap f rhs
      foldMapExpression (Function _ e) = foldMap f e
      foldMapExpression (Call caller arg) = foldMap f caller <> foldMap f arg
      foldMapExpression (Identifier _) = mempty
      foldMapExpression (Literal _) = mempty

instance Traversable Expression where
  sequenceA (Expression base extra) =
    Expression <$> base' <*> extra
    where
      base' =
        case base of
          Block es e -> Block <$> traverse sequenceA es <*> sequenceA e
          Definition name e -> Definition name <$> sequenceA e
          IfThenElse cond then_ else_ -> IfThenElse <$> sequenceA cond <*> sequenceA then_ <*> sequenceA else_
          Operation op lhs rhs -> Operation op <$> sequenceA lhs <*> sequenceA rhs
          Function param e -> Function param <$> sequenceA e
          Call caller arg -> Call <$> sequenceA caller <*> sequenceA arg
          Identifier name -> pure (Identifier name)
          Literal constant -> pure (Literal constant)

data Type
  = TUnit
  | TBool
  | TInt
  | TString
  | TFunction Type Type
  deriving (Eq)

instance Show Type where
  show TUnit = "()"
  show TBool = "bool"
  show TInt = "int"
  show TString = "string"
  show (TFunction ret param) = show ret ++ " -> " ++ show param

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
  deriving (Eq)

instance Show IncompleteType where
  show ITUnknown = "unknown"
  show (ITComplete t) = show t
  show (ITFunction ret param) = show ret ++ " -> " ++ show param

complete :: IncompleteType -> Maybe Type
complete ITUnknown = Nothing
complete (ITComplete t) = Just t
complete (ITFunction ret param) = TFunction <$> complete ret <*> complete param

incomplete :: Type -> IncompleteType
incomplete = ITComplete

incompleteUnknown :: IncompleteType
incompleteUnknown = ITUnknown

incompleteFunction :: IncompleteType -> IncompleteType -> IncompleteType
incompleteFunction (ITComplete ret) (ITComplete param) = ITComplete (TFunction ret param)
incompleteFunction ret param = ITFunction ret param

incompleteRetAndParam :: IncompleteType -> Maybe (IncompleteType, IncompleteType)
incompleteRetAndParam (ITFunction ret param) = Just (ret, param)
incompleteRetAndParam (ITComplete (TFunction ret param)) = Just (ITComplete ret, ITComplete param)
incompleteRetAndParam ITUnknown = Just (ITUnknown, ITUnknown)
incompleteRetAndParam _ = Nothing

instance Semigroup IncompleteType where
  (ITComplete t) <> _ = ITComplete t
  _ <> (ITComplete t) = ITComplete t
  (ITFunction ret1 param1) <> (ITFunction ret2 param2) =
    incompleteFunction (ret1 <> ret2) (param1 <> param2)
  t <> ITUnknown = t
  ITUnknown <> t = t

instance Monoid IncompleteType where
  mappend = (<>)
  mempty = incompleteUnknown
