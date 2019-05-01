import System.IO.Unsafe

foo = unsafePerformIO $ putStrLn "foo"
bar = unsafePerformIO $ do
    putStrLn "bar"
    return "baz"
main = putStrLn $ foo `seq` bar
