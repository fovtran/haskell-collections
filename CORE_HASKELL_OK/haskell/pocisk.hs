-- Jest pudełko a w środku pewna ilość kulek. Każda kulka ma unikalny numer. Dokonaliśmy N losowań, w których
-- odnotowaliśmy wybór K różnych kulek.
-- Z jaką pewnością możemy stwierdzić, że w środku pudełka było K kulek? Innymi słowy: z jakim prawdopodobieństwem
-- wylosowaliśmy każdą z nich co najmniej raz?

import Data.Ratio

choose n k       = product [k+1..n] % product [1..n-k]
entry  n k i     = (1 % (k + i) ^ n) * (choose (k+i) k)
pocisk n k max_i = recip $ sum [entry n k i | i <- [0..max_i]] * (k%1)^n

n = 100
k =  20
main = print . fromRational $ pocisk n k 1000