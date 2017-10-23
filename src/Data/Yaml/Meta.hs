module Data.Yaml.Meta where

data Stream = Stream [Document] deriving Show

data Document = Document Node deriving Show

data Node
    = SeqNode [Node]
    | MapNode [(Node, Node)]
    | ScalarNode String
    | AliasNode String
    deriving (Show)

emptyNode = ScalarNode ""
