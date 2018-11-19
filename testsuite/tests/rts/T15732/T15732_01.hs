import GHC.RTS.Flags ( getGCFlags, GCFlags(..) )

main :: IO ()
main = do
  gcflags <- getGCFlags
  putStrLn $ "oldGenFactor: " ++ show (oldGenFactor gcflags)
