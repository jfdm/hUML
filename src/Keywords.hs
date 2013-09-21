module Keywords where

hUmlKeywords = [hUmlClass,
                hUmlModifierAbstract,
                hUmlModifierStatic]

hUmlClass = "class"

hUmlModifierAbstract = "abstract"
hUmlModifierStatic   = "static"
hUmlModifierInterface = "interface"
 
hUmlOps = [hUmlOperatorSpecialisation,
           hUmlOperatorComposition,   
           hUmlOperatorAggregation,   
           hUmlOperatorRealisation,
           hUmlOperatorAssociation,
           hUmlVisibilityPrivate,
           hUmlVisibilityProtected,
           hUmlVisibilityPackage,
           hUmlVisibilityPublic]   

hUmlVisibilityPrivate   = "-"
hUmlVisibilityProtected = "#"
hUmlVisibilityPackage   = "~"
hUmlVisibilityPublic    = "+"

hUmlOperatorSpecialisation = "<|-" 
hUmlOperatorComposition    = "*--" 
hUmlOperatorAggregation    = "o--" 
hUmlOperatorRealisation    = "<--" 
hUmlOperatorAssociation    = "-->" 


