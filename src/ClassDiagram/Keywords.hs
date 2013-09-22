-- | Keywords and Operators as used by the textual UML language.
module ClassDiagram.Keywords where

-- | Keywords
hUmlKeywords = [hUmlClass,
                hUmlModifierAbstract,
                hUmlModifierStatic]
 ------------------------------------------------------------------ [ Keywords ]
hUmlClass = "class"

hUmlModifierAbstract = "abstract"
hUmlModifierStatic   = "static"
hUmlModifierInterface = "interface"

-- | Operators used with the language 
hUmlOps = [hUmlOperatorSpecialisation,
           hUmlOperatorComposition,   
           hUmlOperatorAggregation,   
           hUmlOperatorRealisation,
           hUmlOperatorAssociation,
           hUmlVisibilityPrivate,
           hUmlVisibilityProtected,
           hUmlVisibilityPackage,
           hUmlVisibilityPublic]   

-- ---------------------------------------------------- [ Visibility Operators ]
hUmlVisibilityPrivate   = "-"
hUmlVisibilityProtected = "#"
hUmlVisibilityPackage   = "~"
hUmlVisibilityPublic    = "+"

-- ------------------------------------------------------ [ Relation Operators ]

hUmlOperatorSpecialisation = "<|-" 
hUmlOperatorComposition    = "*--" 
hUmlOperatorAggregation    = "o--" 
hUmlOperatorRealisation    = "<--" 
hUmlOperatorAssociation    = "-->" 

-- --------------------------------------------------------------------- [ EOF ]
