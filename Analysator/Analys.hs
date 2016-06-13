module Analys where
  import Tree

  kLimited :: AttainTree -> Chip
  kLimited (AttainTree (Marking mark) _ subTr) = let
          curMax   = maximum mark
          subTrMax = case subTr of
                      Tree pairs -> maximum $ map (kLimited . snd) pairs
                      Degenerate -> Num 0
       in max curMax subTrMax

  -- Сеть сохраняема, если количество фишек во всех переходах одинаково
  isSelfLife :: AttainTree -> Bool
  isSelfLife tree@(AttainTree (Marking mark) _ _) = let
      withoutOmega = notElem Omega
      findCount = sum . map (\(Num i) -> i)
      isSelfLife' count (AttainTree (Marking m) _ subTr) =
        withoutOmega m       &&
        count == findCount m &&
        case subTr of
            Degenerate -> True
            Tree pairs -> all (isSelfLife' count . snd) pairs
    in withoutOmega mark && isSelfLife' (findCount mark) tree

  -- Переход потенциально жив, если его можно запустить
  isTransferPotentiallyAlife :: AttainTree -> TransferNum -> Bool
  isTransferPotentiallyAlife (AttainTree _ _ (Tree pairs)) t = let
      (curTranses, subTrees) = unzip pairs
      in elem t curTranses || any (`isTransferPotentiallyAlife` t) subTrees

  isTransferPotentiallyAlife (AttainTree _ _ Degenerate) _ = False

  -- Переход жив, если он разрешен в каждой разрешенной маркировке
  isTransferAlife :: AttainTree -> TransferNum -> Bool
  isTransferAlife (AttainTree _ _ Degenerate) _        = True
  isTransferAlife (AttainTree _ _ (Tree pairs)) t = let
      (curTranses, subTrees) = unzip pairs
    in elem t curTranses && all (`isTransferAlife` t) subTrees

  -- Переход устойчив, если из каждой маркировки,
  -- В которую мы по нему пришли,
  -- Его можно снова запустить
  -- Не будет работать, если маркировка
  -- помечена как уже существующая, и, при этом,
  -- достигли мы её не с помощью этого перехода
  -- (тогда переход может оказаться нестабильным)
  -- !!!!!!!!!! неправильно
  isTransferStable :: AttainTree -> TransferNum -> Bool
  isTransferStable (AttainTree _ _ Degenerate) _        = True
  isTransferStable (AttainTree _ _ (Tree pairs)) t      = let
      branch = filter ((== t) . fst) pairs --Найдем наш переход
      is = not $ null branch
      isExist (AttainTree _ _ (Tree ps))  = elem t $ map fst ps
      isExist (AttainTree _ _ Degenerate) = True
      isStillStable (AttainTree _ _ tree) = case tree of
                        Degenerate -> True
                        (Tree ps)  -> all (isExist . snd) ps -- all of snd ps has t
                        -- elem t $ map fst ps
   in all ((`isTransferStable` t) . snd) pairs     &&
      (not is || all (isStillStable . snd) branch)


  analys :: Petri -> AttainTree -> [String]
  analys petri tree = let
    kLimit = "Ограниченность: " ++
             case kLimited tree of
                  Omega -> "сеть не ограничена."
                  Num i -> "К = " ++ show i
    safety = "Безопасность: " ++
             case kLimited tree of
                  Num 1 -> "сеть безопасна."
                  _     -> "сеть не безопасна."
    selfLife = "Сохраняемость: " ++
               if isSelfLife tree
                 then "сеть сохраняема."
              else "сеть не сохраняема."
    transfsNum = [1..length petri]
    potVitalityTransfers = zip transfsNum
                               (map (isTransferPotentiallyAlife tree) transfsNum)
    potentVitality = "Потенциальная живость переходов: " ++ init (init
                     (concatMap (\(n, is)
                             -> "t"    ++
                                show n ++
                                " "    ++
                                if is then "потенциально жив; "
                                  else " мёртв; ")
                          potVitalityTransfers))
                                            ++ "."
    vitalityTransfers = zip transfsNum
                            (map (isTransferAlife tree) transfsNum)
    vitalityTrans = "Живость переходов: " ++
               concatMap ((\n -> "t" ++ show n ++ " жив; ") . fst)
                         (filter snd vitalityTransfers)
    webVitality = "Живость сети: " ++
                  if all snd vitalityTransfers then
                       "сеть жива."
                  else "сеть не жива."
    transferStability = zip transfsNum
                            (map (isTransferStable tree) transfsNum)
    pTransStable = "Стабильность (устойчивость) переходов: " ++
               concatMap ((\n -> "t" ++ show n ++ " стабилен; ") . fst)
                         (filter snd transferStability)
    pWebStable = "Стабильность (устойчивость) сети: " ++
                  if all snd transferStability then
                       "сеть стабильна."
                  else "сеть не стабильна."
    in [kLimit,         safety,        selfLife,
        potentVitality, vitalityTrans, webVitality,
        pTransStable,   pWebStable]

  growAndGetInfo :: Petri -> [Int] -> String
  growAndGetInfo transfers mark = let
          petriMark = Marking [Num i | i <- mark]
          tree = findTree transfers petriMark
          settings = analys transfers tree
        in "Attain tree:\n" ++ show tree ++
           "\nSettings:\n"  ++ init (unlines settings)
