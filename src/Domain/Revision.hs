module Domain.Revision where

import           ClassyPrelude
import           Numeric.Natural
import           Text.Printf

data RevError
    = ErrRevisionNotFound
    deriving (Show, Eq)

newtype Hash = Hash Text deriving (Eq, Ord)

data Node = Node
    { contentHash :: Hash
    , content :: Text
    } deriving (Show)

instance Eq Node where
        a == b = contentHash a == contentHash b

data Revision = Revision Hash Change
newtype Change = Change [Edit]
data Edit = Edit Bounds Text
type Bounds = (Natural, Natural)

newRevision :: Hash -> [Edit] -> Either Text Revision
newRevision parentHash edits =
        let changes = newChange edits
        in  case changes of
                    Left  err -> Left err
                    Right c   -> Right (Revision parentHash c)

newChange :: [Edit] -> Either Text Change
newChange ee = if validateEdits ee then Right (Change ee) else Left "invalid edits"

validateEdits :: [Edit] -> Bool
validateEdits ee = case ee of
        [] -> False
        xs -> sortedWithoutOverlapsBounds $ map (\(Edit b _) -> b :: Bounds) ee

sortedWithoutOverlapsBounds :: [Bounds] -> Bool
sortedWithoutOverlapsBounds []         = True
sortedWithoutOverlapsBounds [(s0, e0)] = s0 <= e0
sortedWithoutOverlapsBounds ((s0, e0) : (s1, e1) : xs) =
        e0 < s1 && s0 <= e0 && sortedWithoutOverlapsBounds ((s1, e1) : xs)



-- Show instances
instance Show  Hash where
        show (Hash r) = printf "root: %s" r

instance Show Revision where
        show (Revision r e) = printf "- %s, changes [ %s ]" (show r) (show e)


instance Show Change where
        show (Change ee) = intercalate "," $ map show ee

instance Show Edit where
        show (Edit (start, end) content) = printf " ( %s:%s, content: %s ) " start end content

