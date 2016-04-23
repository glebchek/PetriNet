module Tree where
  import Control.Arrow --second
  import Data.Tuple()
  import Control.Monad
  import Data.Maybe
  import Control.Monad.State.Lazy

  type TransferNum = Int
  -- Look like maybe Monad
  data Chip     = Num Int |
                  Omega     -- Либо число либо Омега
  instance Show Chip where
    show (Num i) = show i
    show Omega   = "w"--ω"

  instance Eq Chip where
    (==) Omega   Omega   = True
    (==) (Num c) (Num o) = c == o
    (==) _       _       = False

  instance Ord Chip where
    (<=) _       Omega   = True
    (<=) Omega   _       = False
    (<=) (Num c) (Num o) = c <= o

  type Marking = [Chip]
  type Node     = Int
  type Transfer = ([Node], [Node]) -- Входные узыл и выходные узлы
  type Petri    = [Transfer]

  data MarkType = JustMark     Marking |
                  AlreadyExist Marking |
                  CoverMark    Marking deriving Show

  fromMT :: MarkType -> Marking
  fromMT mo = case mo of
      JustMark     o -> o
      AlreadyExist o -> o
      CoverMark    o -> o

  toAlreadyExist :: MarkType -> MarkType
  toAlreadyExist m = AlreadyExist (fromMT m)

  isCovering :: MarkType -> Bool
  isCovering (CoverMark _) = True
  isCovering _           = False

  instance Eq MarkType where
    (==) mt mo = fromMT mt == fromMT mo

                    -- номер перехода, текущая маркировка, сабдеревья
  data AttainTree = AttainTree Marking [(TransferNum, AttainTree)] |
                    Finish MarkType TransferNum deriving Show

  replace :: [a] -> a -> Int -> [a]
  replace xs e n = take n xs ++ [e] ++ drop (n + 1) xs

  nextMark :: Monad m => Marking -> Transfer -> m MarkType
  -- nextMark mark [] = error $ "Не ошибка, но интересно тип: " ++ show mark
  nextMark mark (input, output) = liftM (newMark mark output) $
                                  foldM ifEnough mark input where
      ifEnough :: Monad m => Marking -> Node -> m Marking
      ifEnough cmark node = case cmark!!node of
                    Omega   -> return cmark
                    (Num i) -> if i <= 0 then
                                fail "Not enough"
                               else let
                                 new = replace cmark (Num (i - 1)) node
                               in return new

      newMark :: Marking -> [Node] -> Marking -> MarkType
      newMark oldMark n currMark = let
          addChip cmark node = case cmark!!node of -- Добавить фишки
              Omega   -> cmark
              (Num i) -> replace cmark (Num (i + 1)) node
          nMark = foldl addChip currMark n
          isCover (prev, cur) = prev <= cur -- Покрывает ли в текущей ноде
          cover oldM cmark node = let -- Заомежить всё, куда добавлялось
                  old = oldM!!node
                  cur = cmark!!node
                  -- tr = "cur > old: " ++ show cur ++ " > " ++ show old
                in if cur > old then
                     -- trace tr $
                     replace cmark Omega node
                   else cmark
        in if all isCover (zip oldMark nMark) then let -- Если покрывает,
              omeged = foldl (cover oldMark) nMark n   -- добавим ω
              in if omeged > nMark then
                   CoverMark omeged
                 else JustMark omeged
           else JustMark nMark

  -- analys :: AttainTree -> String
  -- analys = show

  type FoundedNodes = State [Marking]

  fillSubTree :: Petri -> Marking -> FoundedNodes AttainTree
  fillSubTree p mark = let
      nextMarks = map (nextMark mark) p
      pairs = zip [1..length p] nextMarks -- Пара переход-маркировка
      permittedM = filter (isJust . snd) pairs -- Разрешённые
      permitted  = map (second fromJust) permittedM
            -- уже были / ещё не были
      wasnt    st = filter (flip notElem st . fromMT . snd) permitted
      whishWas st = filter (flip elem    st . fromMT . snd) permitted
            -- покрывающие / не покрывающие
      covering    = filter (      isCovering . snd) permitted
      -- notCovering = filter (not . isCovering . snd) permitted
            -- для покрывающей маркировки удалить переход,
            -- по которому она стала покрывающей
            -- !!!!!!!!!!!! не сделано, и надо разобраться
      -- Фунция с общим состоянием для каждого поддерева
      findSubTreeList lst = runState (mapM (fillSubTree p) lst)
      in do
        foundMarks <- get
        -- Возьмём состояния, которых ещё не было
        let newMarks     = wasnt     foundMarks
        -- И которые были
        let finishedMarks = whishWas foundMarks
        -- Превращаем их в уже существующие
        let existedMarks = map (second toAlreadyExist) finishedMarks
        -- Создадим конечные поддеревья для existedMarks
        let finTrees = [Finish m t | (t, m) <- existedMarks]
        --И конечные для покрывающих
        let covTrees = [Finish m t | (t, m) <- covering]
        -- сохраним текущее состояние
        let savedMarks = map (fromMT . snd) newMarks
        let currExistMarks = savedMarks ++ foundMarks
        let (newTrees, newSt) = findSubTreeList savedMarks currExistMarks
        put newSt
        let fictiveTranses = replicate (length (finTrees ++ covTrees)) (-1)
        let fictivePairs = zip fictiveTranses (finTrees ++ covTrees)
        let transfers = map fst newMarks
        let truthPairs = zip transfers newTrees
        return (AttainTree mark (fictivePairs ++ truthPairs))

  findTree :: Petri -> Marking -> AttainTree
  findTree p m = evalState (fillSubTree p m) [m]
