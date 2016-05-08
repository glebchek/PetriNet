module Analys where
  import Tree
  import Data.Maybe
  import Control.Monad

  kBording :: AttainTree -> Maybe Int
  kBording tree = case findK tree of
          Num i -> return i
          Omega -> Nothing
    where
      findK :: AttainTree -> Chip
      findK (AttainTree (Marking mark) _ subTr) = let
        curMax   = maximum mark
        subTrMax = case subTr of
          Degenerate -> Num 0
          Tree pairs -> let
            subTrees   = map snd pairs
            subTrMaxms = map findK subTrees
            in maximum subTrMaxms
       in max curMax subTrMax

  analys :: AttainTree -> String
  analys tree = let
    kBoard = "К-ограниченность: " ++
      fromMaybe "сеть не ограничена" (liftM (\k -> "К = " ++ show k) $ kBording tree)
    in kBoard

  growAndGetInfo :: Petri -> [Int] -> String
  growAndGetInfo transfers mark = let
            petriMark = Marking [Num i | i <- mark]
            tree = findTree transfers petriMark
            settings = analys tree
          in "Attain tree:\n" ++ show     tree ++
             "\nSettings:\n"  ++ settings
