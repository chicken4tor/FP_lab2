module Main where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as B
import           Data.List            (foldl', insertBy, sortBy)
import qualified Data.Map             as M
import           Data.Ord             (comparing)
import           Huffman              

main :: IO ()
main = do
  -- Читання вмісту тестових файлів
  testFile1 <- readFile "resources/test1.txt"
  testFile2 <- readFile "resources/test2.txt"
  testFile3 <- readFile "resources/test3.txt"
  
  -- Об'єднання тексту з тестових файлів
  let testText = lines testFile1 ++ lines testFile2 ++ lines testFile3
  
  -- Обчислення частоти появи символів у тексті
  let frequencies = histogram (concat testText)
  
  -- Сортування частот за допомогою функції swap, щоб пари впорядковувались за значенням частоти
  let sortedFrequencies = sortBy (comparing swap) frequencies
  
  -- Виведення символів та їх частоти
  putStrLn "\nСимволи та їх частота:\n"
  mapM_ print sortedFrequencies
  
  -- Побудова дерева Хаффмана на основі відсортованих частот
  let huffmanTree = sortedHuffman sortedFrequencies
  
  -- Виведення створеного дерева Хаффмана
  putStrLn "\nДерево Хоффмана:\n"
  print huffmanTree
  
  -- Виведення кодів для символів у дереві Хаффмана
  putStrLn "\nРезультат:\n"
  let encoding = codes huffmanTree
  let showCode (s, bits) = show s ++ " -> " ++ showBits bits
  mapM_ (putStrLn . showCode) (M.toList encoding)
  
  -- Кодування тексту за допомогою створених кодів
  putStrLn "\nЗакодований текст:\n"
  let encoded = map (encode encoding) testText
  mapM_ (print . showBits) encoded
  
  -- Доповнення бітів до кратності восьми і створення файлу з бітами
  let encBits0 = padToEight (concat encoded)
  let bits = bitpack encBits0
  B.writeFile "result.bin" bits
  
  -- Розпакування бітів з файлу та виведення результуючого рядка бітів
  let Right encBits1 = bitunpack . S.pack . B.unpack $ bits
  putStrLn "\nРозкодований текст:\n"
  
  -- Розкодування закодованого тексту
  let decoded = map (decode huffmanTree) encoded
  mapM_ print decoded
