import qualified Data.Text.IO as T
main = T.writeFile "test.txt" "∞"

module UPPrinter where
import System.IO
import Text.PrettyPrint.Leijen

upprint a = (hPutDoc stdout . pretty) a >> putStrLn ""
