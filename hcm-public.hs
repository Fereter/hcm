-- ghc -O2 -no-rtsopts -threaded -feager-blackholing -with-rtsopts="-N5" hcm-public.hs Shaskell-0.21a.hs
import System.IO
import Control.Monad
import Shaskell
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Internal (packBytes, unpackBytes)
import GHC.Word (Word8, Word64)
import Data.Bits

main = do
  putStr "CrackMe by Fereter \r\nPassword: "
  hFlush stdout
  print $ secondstr' getLine
  getChar
	where
		print = (putStrLn =<<)
		secondstr' :: IO String -> IO String
		secondstr' = liftM secondstr

secondstr :: String -> String
secondstr p = ((if isPasswordRight p then getGoodMsg else getFailMsg) p) -- ++ logMsg p

isPasswordRight :: String -> Bool
isPasswordRight p = rightPasswordSHA512 == getHash p

getGoodMsg :: String -> String
getGoodMsg p = unpack8 {-show-} $ zipWith xor (cycle $ packWords $ sha512 $ pack8 $ '.' : p) {-(pack8 winMessage)-} cryptedMessage

getFailMsg :: String -> String
getFailMsg _ = "Fail. "

pack8 :: String -> [Word8]
pack8 s = unpackBytes $ pack s

unpack8 :: [Word8] -> String
unpack8 ws = unpack $ packBytes ws

logMsg :: String -> String
logMsg p = show $ sha512 $ pack8 p

getHash :: String->String
getHash p = show $ sha512 $ pack8 p

octets :: Word64 -> [Word8]
octets w = map (\x -> fromIntegral $ shiftR w x) [56, 48, 40, 32, 24, 16, 8, 0]

packWords :: [Word64] -> [Word8]
packWords ws = concat $ map octets ws

rightPasswordSHA512 = "[2244272422136196658,12488942129965888719,16491154067270348226,5886676586504112922,14913949313687902551,3384368700584802695,8013477298822107199,8037706883374716947]"
--winMessage = "?"
cryptedMessage :: [Word8]
cryptedMessage = read "[247,191,45,80,79,82,8,51,23,26,223,139,234,178,15,61,171,37,213,191,108,75,55,59,117,166,199,253,214,100,46,83,27,152,214,118,203,189,211,204,40,253,98,71,63,76,210,99,230,76,3,58,193,21,31,181,151,92,47,209,167,39,38,215,142,240,122,3,12,7,79,119,76,67,147,208,178,252,82]"
