module ObjC where

data Protocol = Protocol {
  protocolName :: String,
  protocolIncorporatedProtocols :: [Protocol],
  protocolMethods :: [Method]
}

data MethodType = InstanceMethod | ClassMethod
data Visibility = Private | Public

data Method = Method {
  methodType :: MethodType,
  returnType :: Type,
  methodName :: MethodName,
  methodBody :: [Stmt]
}

data Stmt = ExprStmt Expr

data Expr = Message Identifier Method

data MethodName = SimpleMethodName String
                | MethodName [MethodLabel]

data MethodLabel = MethodLabel {
  methodLabel  :: String,
  argumentType :: Type,
  argumentName :: String
}

data Type = BaseType   BaseType
          | ObjectType ObjectType

data BaseType   = Int | Float | Char | Bool

data ObjectType = Object {
  objectName :: ObjectName,
  objectProtocols  :: [Protocol]
}

data ObjectName = Id | ClassName String

data Class = Class {
  className       :: String,
  classSuper      :: Class,
  classProtocols  :: [Protocol],
  classIVars      :: [IVar],
  classAnonymousCategory :: Category,
  classMethods    :: [Method]
}


data Category = Category {
  categoryName    :: String,
  categoryMethods :: [Either Method Property]
}

data Property = Property {
  attributes   :: PropertyAttributes,
  propertyType :: Type,
  propertyName :: Identifier
}

data PropertyAttributes = PropertyAttributes {
  propertyWritability     :: PropertyAttributeWritability,
  propertySetterSemantics :: PropertySetterSemantics
}

data PropertyAttributeWritability = ReadWrite | ReadOnly

data PropertySetterSemantics = Assign | Retain | Copy

data IVar = IVar {iVarType :: Type, iVarName :: Identifier}


type Identifier = String
