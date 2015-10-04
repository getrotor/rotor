%% Lexer for rotor.conf

Definitions.

BeginRotation = rotation
EndRotation = /rotation
KeyVal = [A-Za-z0-9\._/]
WS = ([\s\t\n,]|#.*)

Rules.
{BeginRotation} : {token, {beginrotation, TokenLine, TokenChars}}.
{EndRotation} : {token, {endrotation, TokenLine, TokenChars}}.
{KeyVal}+ : {token, {keyval, TokenLine, TokenChars}}.
< : {token, {openangle, TokenLine, TokenChars}}.
> : {token, {closeangle, TokenLine, TokenChars}}.
\[ : {token, {opensquare, TokenLine, TokenChars}}.
\] : {token, {closesquare, TokenLine, TokenChars}}.
= : {token, {equals, TokenLine, TokenChars}}.
{WS}+ : skip_token.

Erlang code.
