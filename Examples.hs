module Examples where

import ObjC
import Pretty
import Smart
import Control.Monad.State

main :: IO ()
main = do
  putStrLn $ show $ pretty resultController


pUITableViewDelegate :: Protocol
pUITableViewDelegate  = protocol "UITableViewDelegate" [] []


cUIViewController :: Class
cUIViewController = clazz "UIViewController" undefined undefined

tNSString :: Type
tNSString = ObjectType (Object (ClassName "NSString") [])


dealloc :: MethodCall
dealloc = SimpleMethodCall "dealloc"


resultController :: Class
resultController =  newClass "UITResultController" cUIViewController [pUITableViewDelegate] $ do
  name    <- prop tNSString "name"
  count   <- prop tNSString "count"
  payload <- prop tNSString "payload"
  return ()

newClass :: String -> Class -> [Protocol] -> ClassCreation () -> Class
newClass nm parent prots m = let c = clazz nm parent prots
                             in  execState m c

type ClassCreation a = State Class a

prop :: Type -> String -> ClassCreation ()
prop  t s = modify (flip (<@>) (t,s))


(<@>) :: Class -> (Type, String) -> Class
(Class x y z is anonCat ms) <@> (t,s) = 
  Class x y z ((IVar t s) : is) (addProp t s anonCat) (addDealloc t s ms)

(<>) :: Expr -> MethodCall -> Expr
e <> m = Message e m


infixr 1 =:

(=:) :: LHS -> Expr -> Stmt
x =: e = AssignStmt x e

addDealloc :: Type -> Identifier -> [Method] -> [Method]
addDealloc t s [] = return $ fun "dealloc" $
  addDealloc' t s [ ExprStmt $ super <> dealloc ]
addDealloc t s (x:xs) 
  | methodName x == SimpleMethodName "dealloc" = (changeStmts (addDealloc' t s) x):xs
  | otherwise                                  = x : (addDealloc t s xs)


addDealloc' :: Type -> Identifier -> [Stmt] -> [Stmt]
addDealloc' t s d | propType t == Retain = (self <.> s =: nil):d
                  | otherwise            = d


changeStmts :: ([Stmt] -> [Stmt]) -> Method -> Method
changeStmts f (Method m r n b) = Method m r n (f b)

addProp :: Type -> Identifier -> Category -> Category
addProp t s (Category nm ms) = Category nm ((property t s):ms)

property :: Type -> Identifier -> Either a Property
property t s = Right (Property (PropertyAttributes ReadWrite (propType t)) t s)

propType :: Type -> PropertySetterSemantics
propType (BaseType _)   = Assign
propType (ObjectType _) = Retain
