module FirstDemo where
import Data.Char

newYear :: Int
newYear = 2015
size = 4.2

average a b = (fromIntegral a + fromIntegral b)/2

nextChar :: Char -> Char
nextChar c = chr ((ord c)+1)