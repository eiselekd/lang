
whatIsYourName :: IO ()
whatIsYourName = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Your name: " ++ name)
