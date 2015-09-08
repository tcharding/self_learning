BNF for USHell
==============

<cmd>	::== <argv>
<argv>	::== <token> | <token> <argv>
<token>	::== <word> 
<word>	::== <char> | <char> <word>
<char>  ::== <alpha> | <digit> | <underscore>
<alpha> ::== a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z
<digit> ::== |0|1|2|3|4|5|6|7|8|9
<underscore> ::== _

