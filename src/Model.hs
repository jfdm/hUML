module Model where


data ClassDiagram = ClassDiagram {
      classes :: Classes,
      relations :: Relations
    } deriving (Show)

type Classes = [Class]

data Class = Class {
      classID :: String,
      ctype :: TyClass,
      attrs :: Maybe Attributes,
      methods :: Maybe Methods
    } deriving (Show)

data TyClass = AbstractClass |
               NormalClass   |
               InterfaceClass
               deriving (Eq, Ord, Show)

type Attributes = [Attribute]

data Attribute = Attribute {
      attrID :: String,
      attrType :: String,
      attrMod :: Maybe Modifier,
      attrVis :: Visibility
    } deriving (Show)

type Methods = [Method]

data Method = Method {
      methID :: String,
      rType :: String,
      methMod :: Maybe Modifier,
      methVis :: Visibility,
      params :: Maybe Params
    } deriving (Show)

type Params = [Param]

data Param = Param {
      parID :: String,
      typ :: String
    } deriving (Show)

type Relations = [Relation]

data Relation = Relation {
      relType :: TyRelation, 
      from :: Class,
      to :: Class,
      desc :: Maybe String
    } deriving (Show)

data Modifier = Abstract
              | Static
                deriving (Eq, Ord, Show)

data Visibility = Private
                | Protected
                | Package
                | Public
                  deriving (Eq, Ord, Show)

data TyRelation = Specialisation
              | Aggregation
              | Composition
              | Association
              | Realisation
              deriving (Eq, Ord, Show)

-- --------------------------------------------------------------------- [ EOF ]
