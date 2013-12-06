module MsgDiagram.Model where

type MsgDiagram = [MsgDiagramStep]

data MsgDiagramStep = MsgDiagramStep {
      num  :: Int,
      from :: String,
      to   :: String,
      ms   :: Messages
    } deriving (Show, Read)

type Messages = [Message]
type Message = String

-- --------------------------------------------------------------------- [ EOF ]
