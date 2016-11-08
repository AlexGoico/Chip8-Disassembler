module Main where

import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as BL

type Nibble = Word8
type Opcode = (Nibble, Nibble, Nibble, Nibble)

splitNibbles :: [Word8] -> [(Nibble, Nibble)]
splitNibbles xs = map (\x -> (((shiftR x 4) .&. 0x0F), (x .&. 0x0F))) xs

extractOpcode :: [(Nibble, Nibble)] -> [Opcode]
extractOpcode [] = []
extractOpcode ((a, b):(c, d):xs) = (a, b, c, d) : extractOpcode xs

sumWord8 :: [Word8] -> Integer
sumWord8 = sum . (map toInteger)

showNibbles :: [Nibble] -> String
showNibbles [] = []
showNibbles (x:[]) = show x
showNibbles (x:xs) = show x ++ ", " ++ showNibbles xs

showXKK :: [Integer] -> String
showXKK [] = []
showXKK (x:[]) = show x
showXKK (x:xs) = show x ++ ", " ++ showXKK xs

opToString :: Opcode -> String
opToString (0x0, 0x0, 0xE, 0x0) = "CLS"
opToString (0x0, 0x0, 0xE, 0xE) = "RET"
opToString (0x0,   a,   b,   c) = "SYS     " ++ (show . sumWord8) [a, b, c]
opToString (0x1,   a,   b,   c) = "JP      " ++ (show . sumWord8) [a, b, c]
opToString (0x2,   a,   b,   c) = "CALL    " ++ (show . sumWord8) [a, b, c]
opToString (0x3,   x,   a,   b) = "SE      " ++ showXKK [toInteger x,
                                                         sumWord8 [a, b]]
opToString (0x4,   x,   a,   b) = "SNE     " ++ showXKK [toInteger x,
                                                         sumWord8 [a, b]]
opToString (0x5,   x,   y, 0x0) = "SE      " ++ showNibbles [x, y]
opToString (0x6,   x,   a,   b) = "LD      " ++ showXKK [toInteger x,
                                                         sumWord8 [a, b]]
opToString (0x7,   x,   a,   b) = "ADD     " ++ showXKK [toInteger x,
                                                         sumWord8 [a, b]]
opToString (0x8,   x,   y, 0x0) = "LD      " ++ showNibbles [x, y]
opToString (0x8,   x,   y, 0x1) = "OR      " ++ showNibbles [x, y]
opToString (0x8,   x,   y, 0x2) = "AND     " ++ showNibbles [x, y]
opToString (0x8,   x,   y, 0x3) = "XOR     " ++ showNibbles [x, y]
opToString (0x8,   x,   y, 0x4) = "ADD     " ++ showNibbles [x, y]
opToString (0x8,   x,   y, 0x5) = "SUB     " ++ showNibbles [x, y]
opToString (0x8,   x,   y, 0x6) = "SHR     " ++ showNibbles [x, y]
opToString (0x8,   x,   y, 0x7) = "SUBN    " ++ showNibbles [x, y]
opToString (0x8,   x,   y, 0xE) = "SHL     " ++ showNibbles [x, y]
opToString (0x9,   x,   y, 0x0) = "SNE     " ++ showNibbles [x, y]
opToString (0xA,   a,   b,   c) = "LD      " ++ "I, "
                                             ++ (show . sumWord8) [a, b, c]
opToString (0xB,   a,   b,   c) = "JP      " ++ (show . sumWord8) [a, b, c]
opToString (0xC,   x,   a,   b) = "RND     " ++ showXKK [toInteger x,
                                                         sumWord8 [a, b]]
opToString (0xD,   x,   y,   a) = "DRW     " ++ showNibbles [x, y, a]
opToString (0xE,   x, 0x9, 0xE) = "SKP     " ++ show x
opToString (0xE,   x, 0xA, 0x1) = "SKNP    " ++ show x
opToString (0xF,   x, 0x0, 0x7) = "LD      " ++ show x  ++ " DT"
opToString (0xF,   x, 0x0, 0xA) = "LD      " ++ show x  ++ " K"
opToString (0xF,   x, 0x1, 0x5) = "LD      " ++ "DT, "  ++ show x
opToString (0xF,   x, 0x1, 0x8) = "LD      " ++ "ST, "  ++ show x
opToString (0xF,   x, 0x1, 0xE) = "ADD     " ++ "I, "   ++ show x
opToString (0xF,   x, 0x2, 0x9) = "LD      " ++ "F, "   ++ show x
opToString (0xF,   x, 0x3, 0x3) = "LD      " ++ "B, "   ++ show x
opToString (0xF,   x, 0x5, 0x5) = "LD      " ++ "[I], " ++ show x
opToString (0xF,   x, 0x6, 0x5) = "LD      " ++ show x  ++ " [I]"
opToString _ = "Opcode not implemented"

main :: IO ()
main = do
  content <- BL.readFile "data/roms/PONG"
  let opcodes = (extractOpcode . splitNibbles . BL.unpack) content
  (putStrLn . unlines) $ fmap opToString opcodes
