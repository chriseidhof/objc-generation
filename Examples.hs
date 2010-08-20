module Examples where

import ObjC
import Pretty
import Smart

pUITableViewDelegate  = protocol "UITableViewDelegate" [] []

cUIViewController = clazz "UIViewController" undefined undefined

tNSString, tId, tInt :: Type
tNSString = ObjectType (Object (ClassName "NSString") [])
tId       = ObjectType (Object Id [])
tInt      = BaseType Int

resultController =  clazz "UITResultController" cUIViewController 
                    [pUITableViewDelegate]
                <@> (Private, tNSString, "name")
                <@> (Public,  tInt,      "count")

(<@>) :: Class -> (Visibility, Type, String) -> Class
(Class x y z is anonCat ms) <@> (v, t,s) = 
  Class x y z ((IVar t s) : is) (addProp t s anonCat) (addDealloc t s ms)

addDealloc :: Type -> Identifier -> [Method] -> [Method]
addDealloc t s [] = error "todo"

addProp :: Type -> Identifier -> Category -> Category
addProp t s (Category nm ms) = Category nm ((property t s):ms)

property :: Type -> Identifier -> Either a Property
property t s = Right (Property (PropertyAttributes ReadWrite (propType t)) t s)
property t s = Right (Property (PropertyAttributes ReadWrite (propType t)) t s)

propType (BaseType _)   = Assign
propType (ObjectType _) = Retain
