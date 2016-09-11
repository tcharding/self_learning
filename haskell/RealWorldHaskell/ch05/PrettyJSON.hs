module PrettyJSON
  (
    renderJValue
  ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))
import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                compact, pretty)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
  where d = ord c

smallHex :: Int -> Doc
smallHex x = text "\\u"
             <> text (replicate (4 - length h) '0')
             <> text h
  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0x800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                         . fsep . punctuate (char ',') . map item

renderJValue :: JValue -> Doc

renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JString s) = string s
renderJValue (JNumber n) = double n

renderJValue (JArray a) = series '[' ']' renderJValue a

renderJValue (JObject o) = series '{' '}' field o
  where field (name,val) = string name
          <> text ": "
          <> renderJValue val
