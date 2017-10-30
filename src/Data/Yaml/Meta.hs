module Data.Yaml.Meta where

data Stream = Stream [Document] deriving (Eq, Show)

data Document = Document Node deriving (Eq, Show)

data Node
    = SeqNode [Node]
    | MapNode [KeyVal]
    | ScalarNode String
    | AliasNode String
    deriving (Eq, Show)

data KeyVal = KeyVal Node Node deriving (Eq, Show)

emptyNode = ScalarNode ""
