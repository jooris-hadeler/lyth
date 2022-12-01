# Operator Precedence

|Precedence|Operator|Description|
|----------|--------|-----------|
|1|`a()`, `.`|Funktions Aufrufe und Member Access|
|2|`-a`, `!a`, `~a`, `*a`, `&a`|Unary Minus, Logical Not, Bitwise Not, Dereference, Addrof|
|3|`a*b`, `a/b`, `a%b`|Mutliplication, Division, Modulo|
|4|`a+b`, `a-b`|Addition, Subtraction|
|5|`<<`, `>>`|Bitwise shift Operators|
|6|`a&b`|Bitwise And|
|7|`a\|b`|Bitwise Or|
|8|`a^b`|Bitwise Xor|
|9|`<`, `>`, `<=`, `>=`|Relational Operators|
|10|`==`, `!=`|Equality Operators|
|11|`&&`|Logical And|
|12|`\|\|`|Logical Or|
|13|`=`, `+=`, `-=`, `*=`, `%=`, `/=`, `<<=`, `>>=`, `&=`, `\|=`, `^=`|Assign and Compound Assign|
