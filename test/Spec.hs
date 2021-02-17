import Tests ( tests, ex3, bigWord )
import VirtualMachine ( match )
import Test.HUnit ( runTestTT )
import System.TimeIt ( timeIt )

main :: IO ()
main = do
        putStrLn "Starting tests" 
        _ <- runTestTT tests
        putStrLn "End of tests"
        putStrLn "Starting a simple benchmark"
        timeIt $ putStrLn $ "Match millions of ab's against aa*bb*: " ++ (show $ match bigWord ex3)
        putStrLn "End of benchmark"