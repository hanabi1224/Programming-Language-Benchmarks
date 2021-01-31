-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
-- contributed by Bryan O'Sullivan
-- modified by Eugene Kirpichov: pidgits only generates
-- the result string instead of printing it. For some
-- reason, this gives a speedup.
-- modified by W. Gordon Goodsman to do two divisions
-- as per the new task requirements.

import System.Environment

pidgits n = 0 % (0 # (1,0,1)) where
 i%ds
  | i >= n = []
  | True = (concat h ++ "\t:" ++ show j ++ "\n") ++ j%t
  where k = i+10; j = min n k
        (h,t) | k > n = (take (n`mod`10) ds ++ replicate (k-n) " ",[])
              | True = splitAt 10 ds
 j # s | n>a || q/=r = k # t
       | True = show q : k # (n*10,(a-(q*d))*10,d) -- inline eliminateDigit
  where k = j+1; t@(n,a,d)=k&s; q=3$s; r=4$s -- two calls to extractDigit
 c$(n,a,d) = (c*n+a)`div`d -- extractDigit
 j&(n,a,d) = (n*j,(a+n*2)*y,d*y) where y=(j*2+1) -- nextDigit

main = putStr.pidgits.read.head =<< getArgs