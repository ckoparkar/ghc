import GHC.ResponseFile ( getArgsWithResponseFiles )
import GHC.RTS.Flags ( getGCFlags, GCFlags(..) )

main :: IO ()
main = do
  argsR   <- getArgsWithResponseFiles
  gcflags <- getGCFlags
  putStrLn $ "argsR: " ++ show argsR
  putStrLn $ "oldGenFactor: " ++ show (oldGenFactor gcflags)
  putStrLn $ "generations: " ++ show (generations gcflags)
