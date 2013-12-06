module ClassDiagram.Transform (transformClassDiagram, classDiagramOuts) where

import Text.PrettyPrint.Leijen as PP

import ClassDiagram.Model
import ClassDiagram.Transform.Dot
import ClassDiagram.Transform.UML

classDiagramOuts = [classDiaFmtDot, classDiaFmtUML]

-- | Try and transform the Class Diagram into the specified format.
-- If a supported format return a tuple containing the desired file
-- extension, and the transformed specification.
transformClassDiagram :: String
                      -> ClassDiagram
                      -> Either String (String, Doc)
transformClassDiagram fmt cd
    | fmt == classDiaFmtDot = Right (classDiaFmtDot,  classDiagram2Dot cd)
    | fmt == classDiaFmtUML = Right (classDiaFmtUML,  classDiagram2UML cd)
    | otherwise             = Left $ "Unsupported Format: " ++ fmt

-- --------------------------------------------------------------------- [ EOF ]
