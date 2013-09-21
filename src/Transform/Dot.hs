module Transform.Dot
    (
     toDotClassDiagram
    ) where

import Text.PrettyPrint.Leijen as PP
import Data.Maybe
import Model
import Keywords

toDotClassDiagram :: ClassDiagram -> Doc
toDotClassDiagram cd = heed <$$> cs <$$> rs <$$> foot
    where
      heed = text "digraph G{"
      foot = text "}"
      cs = prettyClasses (classes cd)
      rs = prettyRelations (relations cd)

-- ----------------------------------------------------------------- [ Classes ]
prettyClasses :: Classes -> Doc
prettyClasses cs = vsep $ map prettyClass cs

prettyClass :: Class -> Doc
prettyClass c = text (classID c) <+>
                genClassStyle c <>
                text ";"

genClassStyle :: Class -> Doc
genClassStyle c = genStyle [genStylePair "shape" "record", body]
                  where
                    body = (text "label") <> equals <> dquotes (genRecordStyle c)

genRecordStyle :: Class -> Doc
genRecordStyle c = braces (hcat (punctuate (text "|") body))
                   where
                     body = [h,as,ms]
                     h = prettyCType (ctype c) <+> text (classID c)
                     as = prettyAttrs (attrs c)
                     ms = prettyMeths (methods c)

prettyMeths :: Maybe Methods -> Doc
prettyMeths Nothing = empty
prettyMeths (Just ms) = vcat $ map prettyMeth ms

prettyMeth :: Method -> Doc
prettyMeth m = prettyMod (methMod m) <>
               prettyVis (methVis m) <>
               text (methID m) <>
               prettyParams (params m) <+>
               colon <+>
               text (rType m)

prettyParams :: Maybe Params -> Doc
prettyParams Nothing = lparen <> rparen
prettyParams (Just ps) = parens (hcat (punctuate comma prettyp))
                         where
                           prettyp = map (prettyParam) ps

prettyParam :: Param -> Doc
prettyParam p = text (parID p) <+> colon <+> text (typ p)

prettyAttrs :: Maybe Attributes -> Doc
prettyAttrs Nothing = empty
prettyAttrs (Just as) = vcat $ map prettyAttr as

prettyAttr :: Attribute -> Doc
prettyAttr a = prettyMod (attrMod a) <>
               prettyVis (attrVis a) <>
               text (attrID a) <+>
               colon <+>
               text (attrType a)

prettyVis :: Visibility -> Doc
prettyVis v = case v of 
                Private   -> text hUmlVisibilityPrivate
                Protected -> text hUmlVisibilityProtected
                Package   -> text hUmlVisibilityPackage
                Public    -> text hUmlVisibilityPublic

prettyCType :: TyClass -> Doc
prettyCType ctype = case ctype of
                      AbstractClass  -> parens $ text "A"
                      NormalClass    -> parens $ text "C"
                      InterfaceClass -> parens $ text "I"

prettyMod :: Maybe Modifier -> Doc
prettyMod Nothing = empty
prettyMod (Just m) = case m of
                       Abstract -> parens $ text "A"
                       Static   -> parens $ text "S"

-- --------------------------------------------------------------- [ Relations ]
prettyRelations :: Relations -> Doc
prettyRelations rs = vsep $ map prettyRelation rs

prettyRelation :: Relation -> Doc
prettyRelation r = text (classID (from r)) <+>
                   text "->" <+>
                   text (classID (to r)) <+>
                   getEdgeStyle (relType r) (desc r) <>
                   semi

getEdgeStyle :: TyRelation -> Maybe String -> Doc
getEdgeStyle rtype desc = genStyle styling
    where
      styling = [genStylePair "style" lstyle,
                genStylePair "dir" dir,
                genStylePair arrow astyle,
                label]
      label = if isNothing desc
              then empty
              else genStylePair "label" (fromJust desc)
      (lstyle, dir, arrow, astyle) = genEdgeStyle rtype

genEdgeStyle :: TyRelation -> (String, String, String, String)
genEdgeStyle t = case t of
                   Specialisation -> ("solid", "back", "arrowtail", "onormal")
                   Aggregation    -> ("solid", "back", "arrowtail", "odiamond")
                   Composition    -> ("solid", "back", "arrowtail", "diamond")
                   Realisation    -> ("dashed", "back", "arrowtail", "empty")
                   Association    -> ("solid", "forward", "arrowhead", "normal")


-- ------------------------------------------------------------------- [ Utils ]
genStyle :: [Doc] -> Doc
genStyle styling = brackets (hcat (punctuate comma styling))

genStylePair :: String -> String -> Doc
genStylePair key val = text key <> equals <> dquotes (text val)

-- --------------------------------------------------------------------- [ EOF ]
