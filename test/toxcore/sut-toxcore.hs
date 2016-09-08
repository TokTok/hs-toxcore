module Main (main) where


foreign import ccall test_main :: IO ()


main :: IO ()
main = test_main
