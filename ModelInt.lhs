> module ModelInt ( 
>  Model, newModel,denom, 
>  encodeSym, 
>  decodeSymInt, 
>  m1
>  ) where 

> import Data.Char
 
> newtype Model  = Model (Int,Tree)   deriving Show 
> data Tree      = Null | Fork Symbol (Int,Int) Tree Tree   deriving Show 
> type MInterval = (Int,Int,Int) 
> type Symbol    = Char 
 
> m1 :: Model 
> m1 = Model (last freqs,growTree 0 128 (adjpairs freqs)) 
 
> adjpairs :: [a] -> [(a,a)] 
> adjpairs xs = zip xs (tail xs) 
 
> growTree :: Int -> Int -> [(Int,Int)] -> Tree 
> growTree m n pqs | m==n      = Null 
>                  | m+1==n    = Fork (chr m) (head pqs) Null Null 
>                  | otherwise = Fork (chr k) pq (growTree m k lpqs) (growTree (k+1) n (tail rpqs)) 
>                                where k = (m+n) `div` 2 
>                                      lpqs = take (k-m) pqs 
>                                      rpqs = drop (k-m) pqs 
>                                      pq   = head rpqs 
 
--------------------------------------------------------------------------------------------------------------- 
The following example of possible frequencies for the characters 0 .. 127 were computed by running the function 
"gather" in the file acStats.lhs: 
 
> freqs :: [Int] 
> freqs = 
>   [0,1,2,3,4,5,6,7,8,9,10,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,421,422,423,424, 
>    425,426,427,431,473,515,985,994,1041,1056,1060,1061,1064,1077,1080,1081,1082,1083,1084,1085,1086,1087,1100, 
>    1101,1108,1138,1179,1180,1181,1182,1183,1184,1185,1189,1201,1202,1203,1204,1205,1206,1207,1219,1227,1228, 
>    1229,1230,1231,1234,1242,1243,1244,1245,1246,1247,1248,1281,1283,1316,1317,1318,1319,1391,1403,1423,1443, 
>    1519,1527,1533,1544,1574,1575,1590,1624,1649,1671,1692,1725,1729,1753,1814,1858,1896,1915,1952,1970,1982, 
>    1983,1984,1995,1996,1997,1998] 
 
--------------------------------------------------------------------------------------------------------------- 
 
> denom :: Model -> Int 
> denom (Model (d,_)) = d 
 
> encodeSym :: Model -> Symbol -> MInterval 
> encodeSym (Model (d,t)) c1 = lookup t c1 
>          where lookup (Fork c2 (p,q) r s) c1 
>                   | c1<c2   = lookup r c1 
>                   | c1==c2  = (p,q,d) 
>                   | c1>c2   = lookup s c1 

!! decodeSymInt returns both the symbol and the associated interval,
to save a separate search of the tree for a subsequent encodeSym 

> decodeSymInt :: Model -> Int -> (Symbol,MInterval) 
> decodeSymInt (Model (d,t)) x = lookup t x 
>                   where lookup (Fork c (p,q) r s) x 
>                            | x<p  = lookup r x 
>                            | x>=q = lookup s x 
>                            | otherwise = (c,(p,q,d)) 
 
> newModel :: Model -> Symbol -> Model 
> newModel m c = m --- a static model 
 
