{-# LANGUAGE FlexibleInstances #-}

module Pretty where

import Text.PrettyPrint
import ObjC

class Pretty a where
  pretty :: a -> Doc

instance Pretty Protocol where
  pretty (Protocol nm prots _) = 
    sym "protocol" <+> text nm <+> (protocols prots)
    $+$
    end

instance Pretty Class where
  pretty c@(Class nm sup prots ivars anoCat _) = vcat
   [ sym "class" <+> text nm  <+> colon <+> text (className sup) <+> protocols prots
   , block (vcat $ map (addSemi . pretty) ivars)
   , end
   , empty
   , pretty (c, anoCat)
   , prettyImpl c
   ]

instance Pretty (Class, Category) where
  pretty (c, Category nm mthds) = vcat
    [ text (className c) <+> parens (text nm)
    , nest 2 (vcat $ map (addSemi . pretty) mthds)
    , end
    ]

instance Pretty Method where
  pretty (Method tp ret nm _) = pretty tp <+> parens (pretty ret) <+> pretty nm

instance Pretty MethodName where
  pretty (SimpleMethodName nm) = text nm
  pretty (MethodName labels) = hsep (map pretty labels) 

instance Pretty MethodType where
  pretty InstanceMethod = minus
  pretty ClassMethod    = plus

instance Pretty IVar where
  pretty (IVar tp nm) = pretty tp <+> text nm

instance Pretty ObjectType where
  pretty (Object nm prots) = pretty nm <+> protocols prots

instance Pretty Type where
  pretty (BaseType tp)   = pretty tp
  pretty (ObjectType tp) = pretty tp

instance Pretty BaseType where
  pretty Int   = text "NSInteger"
  pretty Float = text "CGFloat"
  pretty Char  = text "char"
  pretty Bool  = text "BOOL"
  pretty Void  = text "void"

instance Pretty ObjectName where
  pretty Id             = text "id"
  pretty (ClassName nm) = text nm <> pointer

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left l) = pretty l
  pretty (Right r) = pretty r

instance Pretty MethodLabel where
  pretty (MethodLabel nm ty arg) = text nm <> colon <> parens (pretty ty) <> text arg

instance Pretty Property where
  pretty (Property atts ty nm) = sym "property" <+> pretty atts <+> pretty ty <+> text nm

instance Pretty PropertyAttributes where
  pretty (PropertyAttributes wr sem) = parens (commaList [pretty wr, pretty sem])

instance Pretty PropertyAttributeWritability where
  pretty ReadWrite = text "readwrite"
  pretty ReadOnly  = text "readonly"

instance Pretty PropertySetterSemantics where
  pretty Assign = text "assign"
  pretty Retain = text "retain"
  pretty Copy   = text "copy"


class PrettyImpl a where
  prettyImpl :: a -> Doc


instance PrettyImpl Class where
  prettyImpl (Class nm _ _ _ _ ms) = 
    sym "implementation" <+> text nm
    $+$
    text "synthesize todo"
    $+$
    vcat (map prettyImpl ms)
    $+$
    end

instance PrettyImpl Method where
  prettyImpl m@(Method _ _ _ body) =
   pretty m
   $+$
   block (vcat $ map (addSemi . prettyImpl) body)

instance PrettyImpl Stmt where
  prettyImpl (ExprStmt e)     = prettyImpl e
  prettyImpl (AssignStmt i e) = prettyImpl i <+> equals <+> prettyImpl e

instance PrettyImpl LHS where
  prettyImpl (SimpleLHS i) = text i
  prettyImpl (NestedLHS l i) = prettyImpl l <> dot <> text i

instance PrettyImpl Expr where
  prettyImpl (Message e call) = lbrack <> prettyImpl e <+> prettyImpl call <> rbrack
  prettyImpl (Identifier i)   = text i
  prettyImpl (Constant c)     = prettyImpl c

instance PrettyImpl MethodCall where
  prettyImpl (SimpleMethodCall m) = text m
  prettyImpl (MethodCall args)    = hsep $ map prettyImpl args

instance PrettyImpl (Identifier, Expr) where
  prettyImpl (i, e) = text i <> colon <> prettyImpl e

instance PrettyImpl Constant where
  prettyImpl (Nil )       = text "nil"
  prettyImpl (IntConst i) = text (show i)
  prettyImpl (StrConst s) = at <> (text (show s))

classType :: Class -> Doc
classType (Class nm _ _ _ _ _) = pretty (Object (ClassName nm) [])

addSemi :: Doc -> Doc
addSemi p = p <> semi

protocols :: [Protocol] -> Doc
protocols []  = empty
protocols ls  = angles . commaList . map (text . protocolName) $ ls

commaList :: [Doc] -> Doc
commaList = hsep . punctuate comma

block :: Doc -> Doc
block d = lbrace $+$ nest 2 d $+$ rbrace

end :: Doc
end = sym "end"

sym :: String -> Doc
sym s = at <> text s

angles :: Doc -> Doc
angles p = langle <> p <> rangle

langle, rangle, at, pointer, minus, plus, dot :: Doc

langle  = char '<'
rangle  = char '>'
at      = char '@'
pointer = char '*'
minus   = char '-'
plus    = char '+'
dot     = char '.'
