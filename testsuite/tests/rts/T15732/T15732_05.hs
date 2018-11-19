import GHC.ResponseFile   ( getArgsWithResponseFiles )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args  <- getArgs
  argsR <- getArgsWithResponseFiles
  putStrLn $ "args: " ++ show args
  putStrLn $ "argsR: " ++ show argsR
