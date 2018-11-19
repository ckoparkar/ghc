import GHC.ResponseFile   ( getArgsWithResponseFiles )

main :: IO ()
main = do
  argsR <- getArgsWithResponseFiles
  putStrLn $ "argsR: " ++ show argsR
