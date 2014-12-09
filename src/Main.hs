import Brainfuck

main :: IO ()
main = readFile "helloworld.bf" >>= runBrainfuck . parseBrainfuck
