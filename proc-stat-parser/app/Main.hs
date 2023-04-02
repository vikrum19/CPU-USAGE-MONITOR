module Main (main) where

import Text.Parsec
import Text.Parsec.String (Parser)

--total_time = sum(fields[1:]) 

--idle_time = fields[4]

--non_idle_time = total_time - idle_time 

--usage = 100.0 * (non_idle_time / total_time) 



-- Define a data type to hold CPU statistics
data CpuStats = CpuStats { user :: Integer
                         , nice :: Integer
                         , system :: Integer
                         , idle :: Integer
                         , iowait :: Integer
                         , irq :: Integer
                         , softirq :: Integer
                         , steal :: Integer
                         , guest :: Integer
                         , guest_nice :: Integer
                         } deriving (Show,Eq)

-- Parse a single integer value
intValue :: Parser Integer
intValue = read <$> many1 digit

-- Parse a single CPU statistic
cpuStat :: Parser Integer
cpuStat = spaces *> intValue

-- Parse all CPU statistics for a single CPU
cpuStats :: Parser CpuStats
cpuStats = do
  _ <- many space
  _ <- string "cpu"
  _ <- manyTill anyChar (char ' ') --allows to handle unexpected character after cpu as long as followed by space
  u <- cpuStat
  n <- cpuStat
  s <- cpuStat
  i <- cpuStat
  io <- cpuStat
  irq' <- cpuStat
  sirq <- cpuStat
  st <- cpuStat
  g <- cpuStat
  gn <- cpuStat
  endOfLine
  return $ CpuStats u n s i io irq' sirq st g gn

-- Parse all CPU statistics for all CPUs
allCpuStats :: Parser [CpuStats]
allCpuStats = many1 (try cpuStats <|> (manyTill anyChar endOfLine *> pure (CpuStats 0 0 0 0 0 0 0 0 0 0))) 

-- Calculate the percentage of CPU usage for a single CPU
cpuUsage :: CpuStats -> CpuStats -> Float
cpuUsage prev cur
  | total == 0 = 0.0
  | otherwise = 100 * fromIntegral (total - idleDiff) / fromIntegral total
  where
    idleDiff = idle cur - idle prev
    total = sum . map (\f -> f cur - f prev) $ [user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice]

-- Calculate the percentage of CPU usage for all CPUs
overallCpuUsage :: [CpuStats] -> [CpuStats] -> Float
overallCpuUsage prev cur
  | null prev || null cur = 0.0
  | prev == cur = 0.0
  | otherwise = let cpuUsages = zipWith cpuUsage (tail prev) (tail cur)
                    usageSum = sum cpuUsages
                in 100.0 * usageSum / fromIntegral (length cpuUsages)

-- Read and parse the /proc/stat file
parseProcStat :: IO [CpuStats]
parseProcStat = do
  contents <- readFile "/proc/stat"
  case parse allCpuStats "" contents of
    Left _ -> return []  -- return an empty list in case of an error
    Right stats -> return stats

-- Example usage: print CPU usage percentages for each CPU and for the overall system
main :: IO ()
main = do
  prevStats <- parseProcStat
  -- Wait for a short interval to measure CPU usage over time
  _ <- getLine
  curStats <- parseProcStat
  let cpuUsages = zipWith cpuUsage prevStats curStats
      overallCpuUsage' = overallCpuUsage prevStats curStats 
  putStrLn $ "CPU usage percentages: " ++ show cpuUsages
  putStrLn $ "Overall CPU usage percentage: " ++ show overallCpuUsage'


