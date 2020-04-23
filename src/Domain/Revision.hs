module Domain.Revision where

import           ClassyPrelude
import qualified Numeric.Natural               as Nat
import qualified Text.Printf                   as Printf
import qualified Domain.Node                   as Domain

data RevError
    = ErrRevisionNotFound
    | ErrInvalidEdits
    deriving (Show, Eq)
instance Exception RevError

data Revision = Revision Domain.Hash Change
newtype Change = Change [Edit]
data Edit = Edit Bounds Text
type Bounds = (Nat.Natural, Nat.Natural)

newRevision :: Domain.Hash -> [Edit] -> Either RevError Revision
newRevision parentHash edits =
        let changes = newChange edits
        in  case changes of
                    Left  err -> Left err
                    Right c   -> Right (Revision parentHash c)

newChange :: [Edit] -> Either RevError Change
newChange ee = if validateEdits ee then Right (Change ee) else Left ErrInvalidEdits

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
instance Show Revision where
        show (Revision r e) = Printf.printf "- %s, changes [ %s ]" (show r) (show e)


instance Show Change where
        show (Change ee) = intercalate "," $ map show ee

instance Show Edit where
        show (Edit (start, end) content) = Printf.printf " ( %s:%s, content: %s ) " start end content

