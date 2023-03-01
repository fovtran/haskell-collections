import Development.Shake (cmd_, phony, shake)

main :: IO ()
main = shake (phony "build" (cmd_ "cabal new-build all"))