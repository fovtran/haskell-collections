import Data.List

type Pos = (Int,Int)
type Matrix = [[Int]]    

moviments::Pos->[Pos]
moviments (i,j)= [(i+1,j),(i+2,j),(i-1,j),(i-2,j),(i,j+1),(i,j+2),(i,j-1),(i,j-2)]

decrementsPosition:: Pos->Matrix->Matrix
decrementsPosition(1,c) (m:ms) = (decrements c m):ms
decrementsPosition(l,c) (m:ms) = m:(decrementsPosition (l-1,c) ms)

decrements:: Int->[Int]->[Int]
decrements 1 (m:ms) = (m-1):ms
decrements n (m:ms) = m:(decrements (n-1) ms)

size:: Matrix->Pos
size m = (length m,length.head $ m)

finalMatrix::Pos->Pos->Matrix
finalMatrix (m,n) p = [[if (l,c)==p then 1 else 0 | c<-[1..n]]| l<-[1..m]]

possibleMov:: Pos->Matrix->[Pos]
possibleMov p mat = checks0 ([(a,b)|a<-(dim m),b<-(dim n)]  `intersect` xs) mat
                          where xs = movements p
                               (m,n) = size mat

dim:: Int->[Int]
dim 1 = [1]
dim n = n:dim (n-1)

checks0::[Pos]->Matrix->[Pos]
checks0 [] m =[]
checks0 (p:ps) m = if ((takeValue m p) == 0) then checks0 ps m
                                               else p:checks0 ps m

takeValue:: Matrix->Pos->Int
takeValue x (i,j)= (x!!(i-1))!!(j-1)
