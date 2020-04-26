module Domain.Node where

import           RIO
import qualified Text.Printf                   as Printf

newtype Hash = Hash Text deriving (Eq, Ord)

data Node = Node
    { contentHash :: {-# UNPACK #-} !Hash
    , content :: {-# UNPACK #-} !Text
    } deriving (Show)

instance Eq Node where
  a == b = contentHash a == contentHash b

instance Show Hash where
  show (Hash r) = Printf.printf "root: %s" r


