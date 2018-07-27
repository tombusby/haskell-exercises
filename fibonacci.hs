
-- Time-and-Space-Efficient because it doesn't need to keep any values that are
-- older than F_n-2 and F_n-1
fibonacciAccumulator :: Integral a => a -> a
fibonacciAccumulator 0 = 0
fibonacciAccumulator 1 = 1
fibonacciAccumulator n = fibAccumulate n 0 1
    where
        fibAccumulate :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
        fibAccumulate 1 _ b = b
        fibAccumulate n a b = fibAccumulate (n-1) b (a+b)

-- Time-efficient because it has been converted to tail-recursion.
-- Generating the infinite list of values, and selecting the one we want, is an example
-- of how powerful Haskell is. fibonacci 100000 takes less than 1 second.
fibonacciInfList :: Integral a => Int -> a
fibonacciInfList n = ([0, 1] ++ fibRecurse 0 1) !! n
    where
        fibRecurse :: Integral a => a -> a -> [a]
        fibRecurse fMin2 fMin1 = let fMin0 = fMin2 + fMin1 in
            [fMin0] ++ fibRecurse fMin1 fMin0

-- Inefficient because an exponential (2^n) amount of function calls must be made,
-- and the same value calculated many times. n=35 takes 6+ seconds to calculate.
inefficientFibonacci :: Integral a => Int -> a
inefficientFibonacci 0 = 0
inefficientFibonacci 1 = 1
inefficientFibonacci n = inefficientFibonacci (n-2) + inefficientFibonacci (n-1)
