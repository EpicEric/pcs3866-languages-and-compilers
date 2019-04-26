# pcs3866-languages-and-compilers

A Darthmouth BASIC compiler, written in the Pony language. Ponyc 0.28.0 is required.

Wirth notation of the language:

```
Program = BStatement { BStatement } int "END" .
BStatement = int ( Assign | Read | Data | Print | Goto | If | For | Next | Dim | Def | Gosub | Return | Remark ) .
Assign = "LET" Var "=" Exp .
Var = letter digit | letter [ "(" Exp { "," Exp } ")" ] .
Exp = { "+" | "-" } Eb { ( "+" | "-" | "*" | "/" | "^" ) Eb } .
Eb = "(" Exp ")" | Num | Var | ( "FN" letter | Predef ) "(" Exp ")" .
Predef = "SIN" | "COS" | "TAN" | "ATN" | "EXP" | "ABS" | "LOG" | "SQR" | "INT" | "RND" .
Read = "READ" Var { "," Var } .
Data = "DATA" Snum { "," Snum } .
Print = "PRINT" [ Pitem { "," Pitem } [ "," ] ].
Pitem = Exp | """ Character { Character } """ [ Exp ] .
Goto = ( "GOTO" | "GO" "TO" ) int .
If = "IF" Exp ( ">=" | ">" | "<>" | "<" | "<=" | "=" ) Exp "THEN" int .
For = "FOR" letter [ digit ] "=" Exp "TO" Exp [ "STEP" Exp ] .
Next = "NEXT" letter [ digit ] .
Dim = "DIM" letter "(" int { "," int } ")" { "," letter "(" int { "," int } ")" } .
Def = "DEF" "FN" letter "(" letter [ digit ] ")" "=" Exp .
Gosub = "GOSUB" int .
Return = "RETURN" .
Remark = "REM" { Character } .
Int = digit { digit } .
Num = ( Int [ "." { digit } ] | "." Int ) [ "E" [ "+" | "-" ] Int ] .
Snum = [ "+" | "-" ] Num .
Character = letter | digit | special .
```
