import Perform

whatIsYourPureName :: World -> World
whatIsYourPureName w1 = w4
  where w2   = printStr "What is your name?" w1
        (name, w3) = readStr w2
        w4         = printStr ("Hello " ++ name) w3

branch :: World -> (World, World)
branch w = (printStr "0" w,
            printStr "1" w)
