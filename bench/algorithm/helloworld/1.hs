import System.Environment
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    let n = length args
    let name = if n > 0 then head args else ""
    printf "Hello world %s!\n" name
    