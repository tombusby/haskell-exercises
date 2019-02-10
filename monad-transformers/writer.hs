import Control.Monad.Writer


----- Classic Writer Example

writerTest :: Writer [String] Int
writerTest = do
    tell ["hello"]
    tell ["foo", "bar"]
    return 5

----- Simple WriterT Example

someIOReturnValue :: IO String
someIOReturnValue = return "foobar in an IO monad"

liftIOIntoWriterT :: WriterT [String] IO Int
liftIOIntoWriterT = do
    text <- liftIO someIOReturnValue
    tell [text]
    return 5

main = do
    (retVal, finalState) <- runWriterT liftIOIntoWriterT
    putStrLn $ "final return value: " ++ show retVal
    putStrLn $ "final state: " ++ show finalState
