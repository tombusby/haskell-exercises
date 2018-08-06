import Control.Monad.State.Lazy

appendToState :: Int -> State [Int] ()
appendToState n = do
    state <- get
    put (n:state)

countToTen :: Int -> State [Int] ()
countToTen n =
    if n <= 10 then do
        appendToState n
        countToTen $ n + 1
     else
        return ()

main = do
    state <- return . reverse . execState (countToTen 1) $ []
    putStrLn . show $ state
