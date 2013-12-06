module MsgDiagram.Transform.UML (
    msgDiagram2UML, msgDiaFmtUML, msgDiaExtUML
) where


import Text.PrettyPrint.Leijen as PP
import MsgDiagram.Model

msgDiaFmtUML = "uml"
msgDiaExtUML = ".uml"

msgDiagram2UML :: MsgDiagram -> Doc
msgDiagram2UML md = vsep $ map prettyStep md

prettyStep :: MsgDiagramStep -> Doc
prettyStep (MsgDiagramStep i f t ms) = text f <+>
                                       text "->" <+>
                                       text t <+>
                                       colon <+>
                                       prettyAsciiMSGs ms

prettyAsciiMSGs :: Messages -> Doc
prettyAsciiMSGs ms = hsep $ punctuate comma $ map prettyAsciiMSG ms

prettyAsciiMSG :: Message -> Doc
prettyAsciiMSG msg = dquotes $ text msg

-- --------------------------------------------------------------------- [ EOF ]
