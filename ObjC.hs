module ObjC where

data Protocol = Protocol {
  protocolName :: String,
  protocolIncorporatedProtocols :: [Protocol],
  protocolMethods :: [Method]
}
 deriving (Show, Eq)

data MethodType = InstanceMethod | ClassMethod
 deriving (Show, Eq)
data Visibility = Private | Public
 deriving (Show, Eq)

data Method = Method {
  methodType :: MethodType,
  returnType :: Type,
  methodName :: MethodName,
  methodBody :: [Stmt]
}
 deriving (Show, Eq)

data Stmt = ExprStmt Expr
          | AssignStmt LHS Expr
 deriving (Show, Eq)

data LHS = SimpleLHS Identifier
         | NestedLHS LHS Identifier
 deriving (Eq, Show)

data MethodCall = SimpleMethodCall Identifier
                | MethodCall [(Identifier, Expr)]
 deriving (Show, Eq)

data Expr = Message Expr MethodCall
          | Identifier Identifier
          | Constant Constant
 deriving (Show, Eq)

data Constant = Nil 
              | IntConst Int
              | StrConst String
 deriving (Show, Eq)
 

data MethodName = SimpleMethodName String
                | MethodName [MethodLabel]
 deriving (Show, Eq)

data MethodLabel = MethodLabel {
  methodLabel  :: String,
  argumentType :: Type,
  argumentName :: String
}
 deriving (Show, Eq)

data Type = BaseType   BaseType
          | ObjectType ObjectType
 deriving (Show, Eq)

data BaseType   = Int | Float | Char | Bool | Void
 deriving (Show, Eq)

data ObjectType = Object {
  objectName :: ObjectName,
  objectProtocols  :: [Protocol]
}
 deriving (Show, Eq)

data ObjectName = Id | ClassName String
 deriving (Show, Eq)

data Class = Class {
  className       :: String,
  classSuper      :: Class,
  classProtocols  :: [Protocol],
  classIVars      :: [IVar],
  classAnonymousCategory :: Category,
  classMethods    :: [Method]
}
 deriving (Show, Eq)


data Category = Category {
  categoryName    :: String,
  categoryMethods :: [Either Method Property]
}
 deriving (Show, Eq)

data Property = Property {
  attributes   :: PropertyAttributes,
  propertyType :: Type,
  propertyName :: Identifier
}
 deriving (Show, Eq)

data PropertyAttributes = PropertyAttributes {
  propertyWritability     :: PropertyAttributeWritability,
  propertySetterSemantics :: PropertySetterSemantics
}
 deriving (Show, Eq)

data PropertyAttributeWritability = ReadWrite | ReadOnly
 deriving (Show, Eq)

data PropertySetterSemantics = Assign | Retain | Copy
 deriving (Show, Eq)

data IVar = IVar {iVarType :: Type, iVarName :: Identifier}
 deriving (Show, Eq)

type Identifier = String
