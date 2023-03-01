main :: IO ()
main = print def

instance Num ()

def :: (Num a, Enum a) => a
def = toEnum 0