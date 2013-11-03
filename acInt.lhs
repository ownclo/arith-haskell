Arithmetic Coding
--- With limited-precision integers
--- With an imported module for models
--- With the version of encode and decode
    derived in the final version of the paper.

AFP Summer School, August 19-24th, 2002
Richard Bird & Jeremy Gibbons

***************************
Draft of January 2nd, 2003
***************************

> import Data.List
> import ModelInt (Model,newModel,denom, encodeSym, decodeSymInt, m1)

!! decodeSymInt returns both a symbol and the associated interval to
save a subsequent search for encodeSym.

> type Interval  = (Int,Int)
> type MInterval = (Int,Int,Int)
> type EInterval = (Int,Interval)
> type Symbol    = Char
> type Bit       = Int

> w1, w2, w3, w4 :: Int
> w1 = 08192 --- 2^13    = w4/4
> w2 = 16384 --- 2^14    = w4/2
> w3 = 24576 --- 3*2^13  = 3*w4/4
> w4 = 32768 --- 2^15    = w4

> e :: Int
> e = 15

> unit :: Interval
> unit = (0,w4)

> narrow :: Interval -> MInterval -> Interval
> narrow (l,r) (p,q,d) = (l + (w*p) `div` d, l + (w*q) `div` d)
>                        where w = r-l

Encoding

> encode :: Model -> EInterval -> [Symbol] -> [Bit]
> encode m ei = concat . stream nextBits enarrow ei . encodeSyms m

> nextBits :: EInterval -> Maybe ([Bit],EInterval)
> nextBits (n,(l,r))
>   | r <= w2   = Just (bits n 0,(0,(2*l,2*r)))
>   | w2 <= l   = Just (bits n 1,(0,(2*l-w4,2*r-w4)))
>   | otherwise = Nothing

> enarrow :: EInterval -> MInterval -> EInterval
> enarrow ei int2 = (n,narrow int1 int2)
>    where (n,int1) = expand ei

> expand :: EInterval -> EInterval
> expand (n,(l,r))
>   | w1 <= l && r <= w3 = expand (n+1,(2*l - w2,2*r - w2))
>   | otherwise          = (n,(l,r))

> encodeSyms :: Model -> [Symbol] -> [MInterval]
> encodeSyms m ss = unfoldr nextInt (m,ss)
>    where nextInt (m,[])   = Nothing
>          nextInt (m,s:ss) = Just (encodeSym m s, (newModel m s, ss))

> bits :: Int -> Bit -> [Bit]
> bits n b = b:replicate n (1-b)

> stream :: (state -> Maybe (output,state)) ->
>           (state -> input -> state) ->
>           state -> [input] -> [output]
> stream f g z xs = case f z of
>                     Just(y,z')  -> y:stream f g z' xs
>                     Nothing     -> case xs of
>                                      []   -> []
>                                      x:xs -> stream f g (g z x) xs

> compress :: Model -> [Symbol] -> (Int,[Bit])
> compress m ss = (length ss, encode m (0,unit) ss)

5. Decoding

> decode :: Model -> EInterval -> [Bit] -> [Symbol]
> decode m ei bs = destream nextBitsM step nextSym ominus (m,ei) (c,ds)
>                  where c = foldl (\x b -> 2*x + b) 0 cs
>                        (cs,ds) = splitAt e (bs ++ 1:replicate (e-1) 0)

> nextBitsM :: (Model,EInterval) -> Maybe ([Bit],(Model,EInterval))
> nextBitsM(m,(n,(l,r)))
>   | r <= w2   = Just (bits n 0,(m,(0,(2*l,2*r))))
>   | w2 <= l   = Just (bits n 1,(m,(0,(2*l-w4,2*r-w4))))
>   | otherwise = Nothing

> step :: (Model,EInterval) -> Symbol -> (Model,EInterval)
> step (m,ei) s = (newModel m s, enarrow ei (encodeSym m s))

> nextSym :: (Model,EInterval) -> (Int,[Bit]) -> Symbol
> nextSym (m,ei) (c,ds) = fst (decodeSymInt m t)
>   where t = ((k-l+1)* d - 1) `div` (r-l)
>         d = denom m
>         k = fscale (n,(c,ds))
>         (n,(l,r)) = expand ei

> ominus :: (Int,[Bit]) -> [Bit] -> (Int,[Bit])
> ominus (c,ds) bs = foldl op (c,ds) bs
>                    where op (c,ds) b = (2*c - w4*b + head ds,tail ds)

> fscale :: (Int,(Int,[Bit])) -> Int
> fscale (n,(x,ds)) = foldl step x (take n ds)
>   where step x b = 2*x + b - w2

> destream :: (state -> Maybe (output,state)) ->
>             (state -> input -> state) ->
>             (state -> result -> input) ->
>             (result -> output -> result) ->
>             state -> result  -> [input]
> destream f g h k z w = case f z of
>                          Just (y,z')  ->   destream f g h k z' (k w y)
>                          Nothing      -> x:destream f g h k (g z x) w
>                         where x = h z w

> decompress :: Model -> (Int,[Bit]) -> [Symbol]
> decompress m (n,bs) = take n (decode m (0,unit) bs)

--------------------------------------------------------------------

> test m ss = decompress m (compress m ss) == ss

--------------------------------------------------------------------
