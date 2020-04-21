module Domain.Change where

import           ClassyPrelude
import           Numeric.Natural

data Node = Node ParentHash Change
newtype ParentHash = ParentHash String
newtype Change = Change [Edit]
data Edit = Edit Bounds String
type Bounds = (Natural, Natural)

newRevision :: String -> [Edit] -> Either String Node
newRevision parentHash edits =
        let changes = newChange edits
        in  case changes of
                    Left  err -> Left err
                    Right c   -> Right (Node (ParentHash parentHash) c)

newChange :: [Edit] -> Either String Change
newChange ee =
        if validateEdits ee then Right (Change ee) else Left "invalid edits"

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
instance Show Node where
        show (Node r e) = "-" ++ show r ++ ", changes: [" ++ showEdits e ++ "]"
                where showEdits = show

instance Show ParentHash where
        show (ParentHash r) = "root: " ++ show r

instance Show Change where
        show (Change ee) = intercalate "," $ map show ee

instance Show Edit where
        show (Edit (start, end) content) =
                " ("
                        ++ show start
                        ++ ", "
                        ++ show end
                        ++ ", content: "
                        ++ content
                        ++ ") "

