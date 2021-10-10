import Debug.Trace (traceShow)

data Instruction = Jmp Int | Acc Int | Nop
  deriving (Show)

data State = State {
  instrs :: [(Instruction, Int)],
  pc :: Int,
  acc :: [Int]}
  deriving (Show)


initState :: [Instruction] -> State
initState instrs = State (map (\i -> (i, 0)) instrs) 0 [0]

nextState :: State -> State
nextState state@(State instrs pc accs) =
  let (instr, _) = instrs !! pc
  in case instr  of
        Jmp offset -> State (newInstrs instrs pc) (pc+offset) $ (head accs):accs
        Acc n -> State (newInstrs instrs pc) (pc+1) $ ((head accs)+n):accs
        Nop -> State (newInstrs instrs pc) (pc+1) $  (head accs):accs
  where newInstrs :: [(Instruction, Int)] -> Int -> [(Instruction, Int)]
        newInstrs instrs addr = take addr instrs ++ [((fst (instrs !! addr)), (snd (instrs !! addr) + 1))] ++ drop (addr+1) instrs

runUntilLoop :: State -> String
runUntilLoop state@(State instrs pc accs) =
  if (any ((>=2) . snd) instrs)
    then show accs
    else runUntilLoop $ nextState state

parseInstr :: String -> Maybe Instruction
parseInstr str =
  let instr_s:(sign_s:val_s):[] = words str
      val = case sign_s of
              '+' -> read val_s
              '-' -> -(read val_s)
  in  case instr_s of
        "jmp" -> Just (Jmp val)
        "acc" -> Just (Acc val)
        "nop" -> Just Nop
        _ -> Nothing

parseData :: String -> Maybe [Instruction]
parseData dat = sequence (map parseInstr $ lines dat)

read0 name = lines <$> readFile name

main = do
  dat <- readFile "data.txt"
  case parseData dat of
    Just instrs -> putStrLn $ runUntilLoop $ initState instrs
    Nothing -> putStrLn "failed to parse"