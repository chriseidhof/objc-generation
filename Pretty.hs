{-# LANGUAGE FlexibleInstances #-}

module Pretty where

import Text.PrettyPrint
import ObjC

class Pretty a where
  pretty :: a -> Doc

instance Pretty Protocol where
  pretty (Protocol nm prots ms) = 
    sym "protocol" <+> text nm <+> (protocols prots)
    $+$
    end

instance Pretty Class where
  pretty c@(Class nm sup prots ivars anoCat _) = vcat
   [ sym "class" <+> text nm <+> protocols prots
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


prettyImpl :: Class -> Doc
prettyImpl c = empty

classType :: Class -> Doc
classType (Class nm _ _ _ _ _) = pretty (Object (ClassName nm) [])

addSemi p = p <> semi

protocols []  = empty
protocols ls  = angles . commaList . map (text . protocolName) $ ls

commaList = hsep . punctuate comma

block d = lbrace $+$ nest 2 d $+$ rbrace

end = sym "end"

sym s = at <> text s

angles :: Doc -> Doc
angles p = langle <> p <> rangle

langle, rangle, at, pointer, minus, plus :: Doc

langle  = char '<'
rangle  = char '>'
at      = char '@'
pointer = char '*'
minus   = char '-'
plus    = char '+'
