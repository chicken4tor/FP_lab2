module HuffmanEncoding where

import           Control.Concurrent        (forkIO)
import           Control.Monad
import qualified Data.Binary.BitPut        as BitPut
import qualified Data.Binary.Strict.BitGet as BitGet
import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Lazy      as LazyByteString
import           Data.Char                 (intToDigit)
import           Data.List                 (foldl', insertBy, sortBy)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust)
import           Data.Ord                  (comparing)

-- Тип для дерева Хаффмана, що може бути листком або вузлом
data HuffmanTree a
  = LeafNode a Int
  | InternalNode Int (HuffmanTree a) (HuffmanTree a)
  deriving (Eq)

-- Показуємо дерево Хаффмана у вигляді рядка
instance Show a => Show (HuffmanTree a) where
  show = generateTreeRepresentation ""
    where
      spaces = map (const ' ')
      paren s = "(" ++ s ++ ")"
      generateTreeRepresentation ss (LeafNode s o) = "--" ++ paren (show o) ++ show s ++ "\n"
      generateTreeRepresentation ss (InternalNode o l r) =
        let root = "--" ++ paren (show o) ++ "-+"
            ss' = ss ++ tail (spaces root)
            lbranch = generateTreeRepresentation (ss' ++ "|") l
            rbranch = generateTreeRepresentation (ss' ++ " ") r
         in root ++ lbranch ++ ss' ++ "|\n" ++ ss' ++ "`" ++ rbranch

-- Повертає частоту зустрічання для дерева Хаффмана
frequency :: HuffmanTree a -> Int
frequency (LeafNode _ freq) = freq
frequency (InternalNode freq _ _) = freq

-- Генерує дерево Хаффмана з відсортованого списку пар (елемент, частота)
sortedHuffman :: [(a, Int)] -> HuffmanTree a
sortedHuffman = combine . map createLeafNode
  where
    combine [t] = t
    combine (ta:tb:ts) = combine . insertBy (comparing frequency) (merge ta tb) $ ts
    merge ta tb = InternalNode (frequency ta + frequency tb) ta tb
    createLeafNode = uncurry LeafNode

-- Генерує коди для символів у дереві Хаффмана
codes :: Ord a => HuffmanTree a -> Map.Map a [Bool]
codes = Map.fromList . generateCodes []
  where
    generateCodes p (LeafNode s _) = [(s, reverse p)]
    generateCodes p (InternalNode _ l r) = generateCodes (False : p) l ++ generateCodes (True : p) r

-- Кодує список елементів згідно таблиці кодів Хаффмана
encode :: Ord a => Map.Map a [Bool] -> [a] -> [Bool]
encode tbl = concatMap getCode
  where
    getCode x = fromJust (Map.lookup x tbl)

-- Декодує список бітів у список елементів з використанням дерева Хаффмана
decode :: HuffmanTree a -> [Bool] -> [a]
decode t0 xs0 = decodeHelper t0 xs0
  where
    decodeHelper (LeafNode s _) bs = s : decodeHelper t0 bs
    decodeHelper (InternalNode _ l r) (b:bs)
      | not b = decodeHelper l bs
      | otherwise = decodeHelper r bs
    decodeHelper _ [] = []

-- Повертає список пар (елемент, кількість зустрічань) для списку елементів
histogram :: Ord a => [a] -> [(a, Int)]
histogram = Map.toList . foldl' countOccurrences Map.empty
  where
    countOccurrences a k = Map.insertWith (+) k 1 a

-- Обмін значень між двома елементами у кортежі
swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)

-- Показує список бітів у вигляді рядка символів '0' та '1'
showBits :: [Bool] -> String
showBits = map (intToDigit . fromEnum)

-- Упаковує список булевих значень у лінивий ByteString
bitpack :: [Bool] -> LazyByteString.ByteString
bitpack = BitPut.runBitPut . mapM_ BitPut.putBit

-- Розпаковує ByteString у список булевих значень
bitunpack :: ByteString.ByteString -> Either String [Bool]
bitunpack bs0 = BitGet.runBitGet bs0 $ unpackBits []
  where
    unpackBits a = do
      e <- BitGet.isEmpty
      if e
        then return (reverse a)
        else BitGet.getBit >>= unpackBits . (: a)

-- Доповнює список бітів до кратності восьми
padToEight :: [Bool] -> [Bool]
padToEight bits =
  let len = length bits
      rem = len `mod` 8
      extra = 8 - rem
      padding = replicate extra False
   in bits ++ padding
