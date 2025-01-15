
hi :: IO String
hi = return "hi"

ho :: IO String
ho = do hi

main :: IO ()
main = do x <- ho; print x


