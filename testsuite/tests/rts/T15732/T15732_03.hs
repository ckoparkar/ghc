import GHC.ResponseFile   ( getArgsWithResponseFiles )
import GHC.RTS.Flags ( getGCFlags, GCFlags(..) )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args  <- getArgs
  argsR <- getArgsWithResponseFiles
  gcflags <- getGCFlags
  putStrLn $ "args: " ++ show args
  putStrLn $ "argsR: " ++ show argsR
  putStrLn $ "oldGenFactor: " ++ show (oldGenFactor gcflags)
  putStrLn $ "generations: " ++ show (generations gcflags)
