module Domain.Node where

import           ClassyPrelude
import qualified Text.Printf                   as Printf

newtype Hash = Hash Text deriving (Eq, Ord)

data Node = Node
    { contentHash :: Hash
    , content :: Text
    } deriving (Show)

instance Eq Node where
  a == b = contentHash a == contentHash b

instance Show Hash where
  show (Hash r) = Printf.printf "root: %s" r


