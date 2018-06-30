main :: IO()
main = do
	     (n:name) <- getLine
	     if ((n:name) /= "" && (n == 'A' || n == 'a')) then putStrLn "Hello!" else putStrLn "Bye!"