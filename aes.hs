import qualified Data.ByteString as B
import Data.Word
import Data.List.Split

type Block = [Word8]
type Key = [Word8]

blockSize :: Num a => a
blockSize = 16

key :: Key
key = [0x88, 0x9d, 0x8a, 0x79,
    0x99, 0xf4, 0xe8, 0x4b,
    0xb5, 0x4f, 0xd0, 0xda,
    0xaf, 0x5f, 0xf1, 0x43]

addPadding :: [Block] -> [Block]
addPadding bs = init bs ++ (padded . last $ bs)
    where
        padded b
            | length b == blockSize = [b] ++ [0x80 : replicate (blockSize - 1) 0x00]
            | otherwise = [b ++ [0x80] ++ replicate (blockSize - length b - 1) 0x00]

encryptBlocks :: Key -> [Block] -> [Block]
encryptBlocks key = map $ encryptBlock key

encryptBlock :: Key -> Block -> Block
encryptBlock key = id

main = do
    plaintextBlocks <- chunksOf blockSize . B.unpack <$> B.readFile "inputtext.txt"
    plaintextBlocks' <- return [last plaintextBlocks] -- Remove later
    encryptedBlocks <- return . encryptBlocks key . addPadding $ plaintextBlocks'
    print encryptedBlocks
    B.writeFile "outputtext.txt" . B.pack . foldr (++) [] $ encryptedBlocks
