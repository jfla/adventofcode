import Debug.Trace (traceShow)
import Text.Read (readMaybe)
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(stripExes))

data Instruction = Jmp Int | Acc Int | Nop
  deriving (Show)

data State = State {
  instrs :: [(Instruction, Int)],
  pc :: Int,
  acc :: [Int]}
  deriving (Show)


initState :: Vector (String, Int) -> (Vector ((String, Int), Int), Int, Int)
initState instrs = ((map (\i -> (i, 0)) instrs), 0, 0)
{-}
nextState :: State -> State
nextState state@(State instrs pc accs) =
  let (instr, _) = instrs !! pc
  in case instr  of
        Jmp offset -> State (newInstrs instrs pc) (pc+offset) $ (head accs):accs
        Acc n -> State (newInstrs instrs pc) (pc+1) $ ((head accs)+n):accs
        Nop -> State (newInstrs instrs pc) (pc+1) $  (head accs):accs
  where newInstrs :: [(Instruction, Int)] -> Int -> [(Instruction, Int)]
        newInstrs instrs addr = take addr instrs ++ [((fst (instrs !! addr)), (snd (instrs !! addr) + 1))] ++ drop (addr+1) instrs
-}
nextState :: (Vector ((String, Int), Int), Int, Int) -> (Vector (String, Int), Int, Int)
nextState (ins, pc, a, a') = let ((i,v),n) = ins ! pc in if i == "jmp" then (ins // (pc,((i,v),n+1)),pc+v,a,a') else if i == "acc" then _ else _

runUntilLoop :: Vector (String, Int) -> String
runUntilLoop = show . acc . head . filter (any ((>=2) . snd) . instrs) . iterate nextState . initState

parseInt :: String -> Maybe Int
parseInt (s:v) = case s of
  '+' -> readMaybe v
  '-' -> negate <$> readMaybe v
  _ -> Nothing

parseInt [] = Nothing

parseInstr :: String -> Maybe (String, Int)
parseInstr str =
  let [instr_s, val_s] = words str
  in case parseInt val_s of
        Just v -> Just (instr_s, v)
        Nothing -> Nothing

parseData :: String -> Maybe (Vector (String, Int))
parseData = V.mapM parseInstr . V.fromList . lines

read0 :: FilePath -> IO [String]
read0 name = lines <$> readFile name

main = do
  dat <- readFile "data.txt"
  case parseData dat of
    Just instrs -> putStrLn $ runUntilLoop instrs
    Nothing -> putStrLn "failed to parse"