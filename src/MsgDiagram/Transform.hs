module MsgDiagram.Transform (transformMsgDiagram, msgDiagramOuts) where

import Text.PrettyPrint.Leijen as PP

import MsgDiagram.Model
import MsgDiagram.Transform.UML

msgDiagramOuts = [msgDiaFmtUML]

-- | Try and transform the thing into the specified format.
-- If a supported format return a tuple containing the desired file
-- extension, and the transformed specification.
transformMsgDiagram :: String
                      -> MsgDiagram
                      -> Either String (String, Doc)
transformMsgDiagram fmt md
    | fmt == msgDiaFmtUML = Right (msgDiaFmtUML,  msgDiagram2UML md)
    | otherwise           = Left $ "Unsupported Format: " ++ fmt

-- --------------------------------------------------------------------- [ EOF ]
