module Smart where

import ObjC

tId, tInt, tVoid :: Type
tId       = ObjectType (Object Id [])
tInt      = BaseType Int
tVoid     = BaseType Void

protocol :: String -> [Protocol] -> [Method] -> Protocol
protocol = Protocol


clazz :: String -> Class -> [Protocol] -> Class
clazz s c p = Class s c p [] (Category "" []) []

fun :: String -> [Stmt] -> Method
fun nm body = Method InstanceMethod tVoid (SimpleMethodName nm) body

nil :: Expr
nil = Constant Nil

self :: Identifier
self = "self"

super :: Expr
super   = Identifier "super"

infixl 2 <.>
(<.>) :: Identifier -> Identifier -> LHS
l <.> r = NestedLHS (SimpleLHS l) r
