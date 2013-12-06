module ClassDiagram.Transform.UML (
    classDiagram2UML, classDiaFmtUML, classDiaExtUML
) where

import Data.Maybe 
import Text.PrettyPrint.Leijen as PP

import ClassDiagram.Model
import ClassDiagram.Keywords

classDiaFmtUML = "uml"
classDiaExtUML = ".uml"

-- | Do the transformation.
classDiagram2UML :: ClassDiagram -> Doc
classDiagram2UML cd = vcat [cs, empty, rs]
    where
      cs = prettyClasses (classes cd)
      rs = prettyRelations (relations cd)

-- ----------------------------------------------------------------- [ Classes ]
prettyClasses :: Classes -> Doc
prettyClasses cs = vcat $ map prettyClass cs

prettyClass :: Class -> Doc
prettyClass (Class n t as ms) =
    prettyCType t <+>
    text n <+>
    case isJust as || isJust ms of
      True ->  prettyClassBody as ms
      False -> empty

prettyClassBody :: Maybe Attributes -> Maybe Methods -> Doc
prettyClassBody as ms = 
    lbrace <$$>
    indent 2 (vcat [prettyAttrs as, prettyMeths ms]) <$$>
    rbrace

-- ------------------------------------------------------------- [ Class Types ]
prettyCType :: TyClass -> Doc
prettyCType ctype = case ctype of
                      AbstractClass  -> text hUmlModifierAbstract <+> text hUmlClass
                      NormalClass    -> text hUmlClass
                      InterfaceClass -> text hUmlModifierInterface <+> text hUmlClass

-- ----------------------------------------------------------------- [ Methods ]
prettyMeths :: Maybe Methods -> Doc
prettyMeths Nothing = empty
prettyMeths (Just ms) = vcat $ map prettyMeth ms

prettyMeth :: Method -> Doc
prettyMeth (Method i t m v ps) =
    prettyMod m <>
    prettyVis v <>
    text i <>
    prettyParams ps <+>
    colon <+>
    text t

-- ------------------------------------------------------------------ [ Params ]
prettyParams :: Maybe Params -> Doc
prettyParams Nothing = lparen <> rparen
prettyParams (Just ps) = parens (hcat (punctuate comma prettyp))
                         where
                           prettyp = map prettyParam ps

prettyParam :: Param -> Doc
prettyParam (Param i t) = text i <+> colon <+> text t

-- -------------------------------------------------------------- [ Attributes ]
prettyAttrs :: Maybe Attributes -> Doc
prettyAttrs Nothing = empty
prettyAttrs (Just as) = vcat $ map prettyAttr as

prettyAttr :: Attribute -> Doc
prettyAttr (Attribute i t m v) =
    prettyMod m <>
    prettyVis v <>
    text i <+>
    colon <+>
    text t

-- -------------------------------------------------------------- [ Visibility ]
prettyVis :: Visibility -> Doc
prettyVis v = case v of 
                Private   -> text hUmlVisibilityPrivate
                Protected -> text hUmlVisibilityProtected
                Package   -> text hUmlVisibilityPackage
                Public    -> text hUmlVisibilityPublic

-- --------------------------------------------------------------- [ Modifiers ]
prettyMod :: Maybe Modifier -> Doc
prettyMod Nothing = empty
prettyMod (Just m) = case m of
                       Abstract -> text hUmlModifierAbstract
                       Static   -> text hUmlModifierStatic

-- --------------------------------------------------------------- [ Relations ]
prettyRelations :: Relations -> Doc
prettyRelations rs = vsep $ map prettyRelation rs

prettyRelation :: Relation -> Doc
prettyRelation (Relation ty f t d) = 
    text (classID f) <+>
    genArrow ty <+>
    text (classID t) <+>
    maybe empty (\x -> colon <+> dquotes (text x)) d 

genArrow :: TyRelation -> Doc
genArrow t = case t of
               Specialisation -> text hUmlOperatorSpecialisation
               Aggregation    -> text hUmlOperatorAggregation   
               Composition    -> text hUmlOperatorComposition   
               Realisation    -> text hUmlOperatorRealisation   
               Association    -> text hUmlOperatorAssociation   

-- --------------------------------------------------------------------- [ EOF ]
