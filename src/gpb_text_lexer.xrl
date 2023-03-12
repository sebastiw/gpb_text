% https://protobuf.dev/reference/protobuf/textformat-spec/

Definitions.

COMMENT    = #.*[\n]
WHITESPACE = [\f\s\t\n\r]

String             = (\\(a|b|f|n|r|t|v|\\|\'|\"|\?|0|[0-7]{1,3}|x[0-9a-fA-F]{1,2}|u[0-9a-fA-F]{4}|U000[0-9a-fA-F]{5}|U0010[0-9a-fA-F]{4})|[^\'\n\\])*
Float              = -?((\.[0-9]+|\.[0-9]+[eE][+-]?[0-9]+|[0-9]+\.[0-9]*|[0-9]+\.[0-9]*[eE][+-]?[0-9]+|[0-9]+[eE][+-]?[0-9]+)[fF]|[0-9]+[fF])
Identifier         = [a-zA-Z_][0-9a-zA-Z_]*
SignedIdentifier   = -[a-zA-Z_][0-9a-zA-Z_]*
DecSignedInteger   = -[0-9]+
OctSignedInteger   = -0[0-8]+
HexSignedInteger   = -0[xX][0-9a-fA-F]+
DecUnsignedInteger = [0-9]+
OctUnsignedInteger = 0[0-8]+
HexUnsignedInteger = 0[xX][0-9a-fA-F]+

Rules.

{COMMENT} : {token, {comment, TokenLine, TokenChars}}.
{WHITESPACE}+ : skip_token.

\"{String}\" : {token, {string, TokenLine, trim($\", TokenChars)}}.
\'{String}\' : {token, {string, TokenLine, trim($\', TokenChars)}}.
{Float} : {token, {float, TokenLine, TokenChars}}.
{Identifier} : {token, {identifier, TokenLine, TokenChars}}.
{SignedIdentifier} : {token, {signed_identifier, TokenLine, TokenChars}}.
{DecSignedInteger} : {token, {dec_signed_integer, TokenLine, TokenChars}}.
{OctSignedInteger} : {token, {oct_signed_integer, TokenLine, TokenChars}}.
{HexSignedInteger} : {token, {hex_signed_integer, TokenLine, TokenChars}}.
{DecUnsignedInteger} : {token, {dec_unsigned_integer, TokenLine, TokenChars}}.
{OctUnsignedInteger} : {token, {oct_unsigned_integer, TokenLine, TokenChars}}.
{HexUnsignedInteger} : {token, {hex_unsigned_integer, TokenLine, TokenChars}}.

\{ : {token, {'{', TokenLine, TokenChars}}.
\} : {token, {'}', TokenLine, TokenChars}}.
\< : {token, {'<', TokenLine, TokenChars}}.
\> : {token, {'>', TokenLine, TokenChars}}.
\: : {token, {':', TokenLine, TokenChars}}.
\[ : {token, {'[', TokenLine, TokenChars}}.
\] : {token, {']', TokenLine, TokenChars}}.
\, : {token, {',', TokenLine, TokenChars}}.
\; : {token, {';', TokenLine, TokenChars}}.

Erlang code.

trim(Char, String) ->
    string:strip(String, both, Char).










