% https://protobuf.dev/reference/protobuf/textformat-spec/

Definitions.

%char    = .
%newline = \n

LETTER = [A-Za-z_]
       %%   "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M"
       %% | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
       %% | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m"
       %% | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
       %% | "_" ;

OCT = [0-7]
       %% "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ;
DEC = [0-9]
       %% "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
HEX = [0-9A-Fa-f]
       %%   "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
       %% | "A" | "B" | "C" | "D" | "E" | "F"
       %% | "a" | "b" | "c" | "d" | "e" | "f" ;

COMMENT    = #[^\n]*\n?
       %% "#", { char - newline }, [ newline ] ;
WHITESPACE = [\s\n\t\v\f\r]
       %%   " "
       %% | newline
       %% | ? ASCII #9  (horizontal tab) ?
       %% | ? ASCII #11 (vertical tab) ?
       %% | ? ASCII #12 (form feed) ?
       %% | ? ASCII #13 (carriage return) ? ;

IDENT = {LETTER}({LETTER}|{DEC})*
       %% letter, { letter | dec } ;

DEC_LIT   = (0|[1-9]{DEC}*)
          %%   "0"
          %% | ( dec - "0" ), { dec } ;
FLOAT_LIT = (\.{DEC}{DEC}*({EXP})?|{DEC_LIT}\.{DEC}*({EXP})?|{DEC_LIT}({EXP}))
          %%   ".", dec, { dec }, [ exp ]
          %% | dec_lit, ".", { dec }, [ exp ]
          %% | dec_lit, exp ;
EXP       = [Ee][+-]?{DEC}{DEC}*
          %% ( "E" | "e" ), [ "+" | "-" ], dec, { dec } ;

DEC_INT   = {DEC_LIT}
OCT_INT   = 0{OCT}{OCT}*
          %% "0", oct, { oct } ;
HEX_INT   = 0[Xx]{HEX}{HEX}*
          %% "0", ( "X" | "x" ), hex, { hex } ;
FLOAT     = ((\.[0-9]+([Ee][+-]?[0-9]+)?|(0|[1-9][0-9]*)\.[0-9]*([Ee][+-]?[0-9]+)?|(0|[1-9][0-9]*)([Ee][+-]?[0-9]+))[Ff]?|(0|[1-9][0-9]*)[Ff])
          %%   float_lit, [ "F" | "f" ]
          %% | dec_lit,   ( "F" | "f" ) ;

STRING = ({SINGLE_STRING}|{DOUBLE_STRING})
          %% single_string | double_string ;

SINGLE_STRING = \'({ESCAPE}|[^\'])*\'
          %% "'", { escape | char - "'" - newline - "\" }, "'" ;
DOUBLE_STRING = \"({ESCAPE}|[^\"])*\"
          %% '"', { escape | char - '"' - newline - "\" }, '"' ;

ESCAPE = (\\[abfnrtv?\\\'\"]|\\{OCT}{1,3}|\\x{HEX}{1,2}|\\u{HEX}{4}|\\U000{HEX}{5}|\\U0010{HEX}{4})
       %%   "\a"                        (* ASCII #7  (bell)                 *)
       %% | "\b"                        (* ASCII #8  (backspace)            *)
       %% | "\f"                        (* ASCII #12 (form feed)            *)
       %% | "\n"                        (* ASCII #10 (line feed)            *)
       %% | "\r"                        (* ASCII #13 (carriage return)      *)
       %% | "\t"                        (* ASCII #9  (horizontal tab)       *)
       %% | "\v"                        (* ASCII #11 (vertical tab)         *)
       %% | "\?"                        (* ASCII #63 (question mark)        *)
       %% | "\\"                        (* ASCII #92 (backslash)            *)
       %% | "\'"                        (* ASCII #39 (apostrophe)           *)
       %% | '\"'                        (* ASCII #34 (quote)                *)
       %% | "\", oct, [ oct, [ oct ] ]  (* UTF-8 byte in octal              *)
       %% | "\x", hex, [ hex ]          (* UTF-8 byte in hexadecimal        *)
       %% | "\u", hex, hex, hex, hex    (* Unicode code point up to 0xffff  *)
       %% | "\U000",
       %%   hex, hex, hex, hex, hex     (* Unicode code point up to 0xfffff *)
       %% | "\U0010",
       %%   hex, hex, hex, hex ;        (* Unicode code point between 0x100000 and 0x10ffff *)

%% FieldName     = ExtensionName | AnyName | IDENT ;
%% ExtensionName = "[", TypeName, "]" ;
%% AnyName       = "[", Domain, "/", TypeName, "]" ;
%% TypeName      = IDENT, { ".", IDENT } ;
%% Domain        = IDENT, { ".", IDENT } ;

TYPE_NAME      = {IDENT}(\.{IDENT})*
DOMAIN         = {IDENT}(\.{IDENT})*
EXTENSION_NAME = \[{TYPE_NAME}\]
ANY_NAME       = \[{DOMAIN}/{TYPE_NAME}\]

Rules.

{DOUBLE_STRING} : {token, {string, TokenLine, trim($\", TokenChars)}}.
{SINGLE_STRING} : {token, {string, TokenLine, trim($\', TokenChars)}}.

%% String             = STRING, { STRING } ;
%% Float              = [ "-" ], FLOAT ;
%% Identifier         = IDENT ;
%% SignedIdentifier   = "-", IDENT ;   (* For example, "-inf" *)
%% DecSignedInteger   = "-", DEC_INT ;
%% OctSignedInteger   = "-", OCT_INT ;
%% HexSignedInteger   = "-", HEX_INT ;
%% DecUnsignedInteger = DEC_INT ;
%% OctUnsignedInteger = OCT_INT ;
%% HexUnsignedInteger = HEX_INT ;

-({WHITESPACE}*|{COMMENT})*{FLOAT}          : {token, {float, TokenLine, to_float(TokenChars)}}.
({WHITESPACE}*|{COMMENT})*{FLOAT}          : {token, {float, TokenLine, to_float(TokenChars)}}.
{IDENT}            : {token, {identifier, TokenLine, TokenChars}}.
-({WHITESPACE}*|{COMMENT})*{IDENT}           : {token, {signed_identifier, TokenLine, TokenChars}}.
-({WHITESPACE}*|{COMMENT})*{DEC_INT}         : {token, {dec_signed_integer, TokenLine, to_int(10, TokenChars)}}.
-({WHITESPACE}*|{COMMENT})*{OCT_INT}         : {token, {oct_signed_integer, TokenLine, to_int(8, TokenChars)}}.
-({WHITESPACE}*|{COMMENT})*{HEX_INT}         : {token, {hex_signed_integer, TokenLine, to_int(16, TokenChars)}}.
{DEC_INT}          : {token, {dec_unsigned_integer, TokenLine, to_int(10, TokenChars)}}.
{OCT_INT}          : {token, {oct_unsigned_integer, TokenLine, to_int(8, TokenChars)}}.
{HEX_INT}          : {token, {hex_unsigned_integer, TokenLine, to_int(16, TokenChars)}}.

%% FieldName     = ExtensionName | AnyName | IDENT ;
%% ExtensionName = "[", TypeName, "]" ;
%% AnyName       = "[", Domain, "/", TypeName, "]" ;
%% TypeName      = IDENT, { ".", IDENT } ;
%% Domain        = IDENT, { ".", IDENT } ;

{EXTENSION_NAME} : {token, {extension_name, TokenLine, TokenChars}}.
{ANY_NAME} : {token, {any_name, TokenLine, TokenChars}}.

%% Field        = ScalarField | MessageField ;
%% MessageField = FieldName, [ ":" ], ( MessageValue | MessageList ) [ ";" | "," ];
%% ScalarField  = FieldName, ":",     ( ScalarValue  | ScalarList  ) [ ";" | "," ];
%% MessageList  = "[", [ MessageValue, { ",", MessageValue } ], "]" ;
%% ScalarList   = "[", [ ScalarValue,  { ",", ScalarValue  } ], "]" ;
%% MessageValue = "{", Message, "}" | "<", Message, ">" ;
%% ScalarValue  = String
%%              | Float
%%              | Identifier
%%              | SignedIdentifier
%%              | DecSignedInteger
%%              | OctSignedInteger
%%              | HexSignedInteger
%%              | DecUnsignedInteger
%%              | OctUnsignedInteger
%%              | HexUnsignedInteger ;

\[ : {token, {'[', TokenLine, TokenChars}}.
\] : {token, {']', TokenLine, TokenChars}}.
\{ : {token, {'{', TokenLine, TokenChars}}.
\} : {token, {'}', TokenLine, TokenChars}}.
\< : {token, {'<', TokenLine, TokenChars}}.
\> : {token, {'>', TokenLine, TokenChars}}.
\: : {token, {':', TokenLine, TokenChars}}.
\, : {token, {',', TokenLine, TokenChars}}.

{WHITESPACE}+ : skip_token.
{COMMENT}     : skip_token.

Erlang code.

trim(Char, String) ->
    string:trim(String, both, [Char]).

remove_comments_and_whitespace(RawString) ->
    String0 = re:replace(RawString, "#[^\n]*\n", "", [global]),
    re:replace(String0, "[[:space:]]", "", [global, {return, list}]).

to_float(RawString) ->
    String0 = remove_comments_and_whitespace(RawString),
    String1 = re:replace(String0, "[fF]$", "", [{return, list}]),
    case string:find(String1, ".") of
        nomatch ->
            list_to_float(String1 ++ ".0");
        _ ->
            list_to_float(String1)
    end.

to_int(Base, RawString) ->
    String = remove_comments_and_whitespace(RawString),
    list_to_integer(String, Base).
