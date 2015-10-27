Cryptography
============

Data Formats
------------
base 2 { 01 } <!-- binary -->
base 4 { 0123 }
base 8 { 0123456 } <!-- oct -->
base 16 { 0123456789ABCDEF } <!-- hex -->
base 64 = { A-Z a-z 0-9 + / } <!-- b64 -->

Definitions
-----------
* digit - One 'character' from a format set (see above).
* encode - Translate from binary format to another format (or ASCII).
* decode - Translate from arbitrary base (or ASCII) to binary.
* cipher - Algorithm for either encryption or decryption
* plaintext - as name suggests, also called 'message'
* ciphertext - plaintext after having cipher applied using 'key'
* key - bit string (or ASCII string 'passphrase') required by cipher.
* encrypt - Apply key to plaintext using cipher.
* decrypt - Apply key to ciphertext using cipher (not necessarily same cipher as
  used to encrypt).

Naming Conventions
------------------

### messages, ciphertext and keys #
$k: single ASCII character key
$key: ASCII string key
$bk: 8 bit key (i.e single character)
$bit_key: arbitrary length bit key

$m: plaintext, message binary format
$msg: plaintext ASCII format

$c: ciphertext

### arbitrary length string of characters (digits) #
binary: bits
hex: hex
base64: b64
ascii: str

### names for digits #
binary: bit
hex: hexd
base64: b64d

### names for 'n' bits #
4: nibble (ie hex)
6: bits6 or bits64 (ie b64)
8: byte

