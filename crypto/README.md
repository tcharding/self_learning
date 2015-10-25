Key to variable names and data representation 
=============================================

digit = One 'character' from a representation set.

data representations:
binary { 01 }
base 16 { 0123456789ABCDEF }
base 64 = {  }

Definitions
-----------
encode: translate from binary to another base
decode: translate from arbitrary base to base 2
plaintext, cleartext, or message: denoted by 'm'
cipher: algorithm for either encryption or decryption
ciphertext: plaintext after having cipher applied plus key: denoted by 'c'
Also (from Schneier)
E(M) = C
D(C) = M
However we will use lowercase.

Number of bits represented by one digit
---------------------------------------
bpd (bits per digit)

binary: 1
hex: 4
base64: 6

Variable names for arbitrary length string of characters
--------------------------------------------------------
binary: bits
hex: hex
base64: b64

Variable names for digits
-------------------------
binary: bit
hex: hexd
base64: b64d

Variable names for 'n' bits
---------------------------
4: nibble (ie hex)
6: bits6 or bits64 (ie b64)
8: byte

