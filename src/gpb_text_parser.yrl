Terminals string float identifier integer  '-' ':' ';' ',' '<' '>' '{' '}' '[' ']' '/' '.'.

Nonterminals message field  scalar_list scalar_values scalar_value message_values message_value message_list field_name extension_name any_name type_name domain string_scalar float_scalar identifier_scalar integer_scalar empty_list.

Rootsymbol message.

%% Message = { Field } ;
message -> field : ['$1'].
message -> field message : ['$1'|'$2'].

%% Field        = ScalarField | MessageField ;
%% MessageField = FieldName, [ ":" ], ( MessageValue | MessageList ) [ ";" | "," ];
%% ScalarField  = FieldName, ":",     ( ScalarValue  | ScalarList  ) [ ";" | "," ];
field -> field_name ':' scalar_value ';' : {scalar, '$1', '$3'}.
field -> field_name ':' scalar_value ',' : {scalar, '$1', '$3'}.
field -> field_name ':' scalar_value : {scalar, '$1', '$3'}.
field -> field_name ':' message_value ';' : {message, '$1', '$3'}.
field -> field_name ':' message_value ',' : {message, '$1', '$3'}.
field -> field_name ':' message_value : {message, '$1', '$3'}.
field -> field_name ':' scalar_list ';' : {scalar, '$1', '$3'}.
field -> field_name ':' scalar_list ',' : {scalar, '$1', '$3'}.
field -> field_name ':' scalar_list : {scalar, '$1', '$3'}.
field -> field_name ':' message_list ';' : {message, '$1', '$3'}.
field -> field_name ':' message_list ',' : {message, '$1', '$3'}.
field -> field_name ':' message_list : {message, '$1', '$3'}.
field -> field_name ':' empty_list ';' : {empty, '$1', '$3'}.
field -> field_name ':' empty_list ',' : {empty, '$1', '$3'}.
field -> field_name ':' empty_list : {empty, '$1', '$3'}.
field -> field_name message_value ';' : {message, '$1', '$2'}.
field -> field_name message_value ',' : {message, '$1', '$2'}.
field -> field_name message_value : {message, '$1', '$2'}.
field -> field_name message_list ';' : {message, '$1', '$2'}.
field -> field_name message_list ',' : {message, '$1', '$2'}.
field -> field_name message_list : {message, '$1', '$2'}.
field -> field_name empty_list ';' : {message, '$1', '$2'}.
field -> field_name empty_list ',' : {message, '$1', '$2'}.
field -> field_name empty_list : {message, '$1', '$2'}.

%% Both message and scalar lists can reduce to empty lists
empty_list -> '[' ']' : [].

%% FieldName     = ExtensionName | AnyName | IDENT ;
field_name -> extension_name : '$1'.
field_name -> any_name : '$1'.
field_name -> identifier : to_field_name('$1').

%% ExtensionName = "[", TypeName, "]" ;
extension_name -> '[' type_name ']' : to_ext('$2').

%% AnyName       = "[", Domain, "/", TypeName, "]" ;
any_name -> '[' domain '/' type_name ']' : to_any('$2', '$4').

%% TypeName      = IDENT, { ".", IDENT } ;
type_name -> identifier : ['$1'].
type_name -> identifier '.' type_name : ['$1'|'$3'].

%% Domain        = IDENT, { ".", IDENT } ;
domain -> identifier : ['$1'].
domain -> identifier '.' domain : ['$1'|'$3'].

%% MessageList  = "[", [ MessageValue, { ",", MessageValue } ], "]" ;
% message_list -> '[' ']' : [].
message_list -> '[' message_values ']' : '$2'.
message_values -> message_value : ['$1'].
message_values -> message_value ',' message_values : ['$1'|'$3'].

%% MessageValue = "{", Message, "}" | "<", Message, ">" ;
message_value -> '{' message '}' : '$2'.
message_value -> '<' message '>' : '$2'.

%% ScalarList   = "[", [ ScalarValue,  { ",", ScalarValue  } ], "]" ;
scalar_list -> '[' scalar_values ']' : '$2'.
scalar_values -> scalar_value : ['$1'].
scalar_values -> scalar_value ',' scalar_values : ['$1'|'$3'].

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
scalar_value -> string_scalar : '$1'.
scalar_value -> float_scalar : '$1'.
scalar_value -> identifier_scalar : '$1'.
scalar_value -> integer_scalar : '$1'.

%% String             = STRING, { STRING } ;
string_scalar -> string : to_bin(['$1']).
string_scalar -> string string_scalar : to_bin(['$1'|'$2']).

%% Float              = [ "-" ], FLOAT ;
float_scalar -> '-' float : to_neg_float('$2').
float_scalar -> float : to_float('$1').

%% SignedIdentifier   = "-", IDENT ;   (* For example, "-inf" *)
identifier_scalar -> '-' identifier : to_neg_identifier('$2').
%% Identifier         = IDENT ;
identifier_scalar -> identifier : to_identifier('$1').

%% DecSignedInteger   = "-", DEC_INT ;
%% OctSignedInteger   = "-", OCT_INT ;
%% HexSignedInteger   = "-", HEX_INT ;
%% DecUnsignedInteger = DEC_INT ;
%% OctUnsignedInteger = OCT_INT ;
%% HexUnsignedInteger = HEX_INT ;
integer_scalar -> '-' integer : to_neg_int('$2').
integer_scalar -> integer : to_int('$1').

Erlang code.

to_bin(Ss) ->
    list_to_binary(lists:flatten([to_value(S) || S <- Ss])).

to_neg_int(S) ->
    -to_int(S).

to_int(I) ->
    to_value(I).

to_neg_float(S) ->
    -to_float(S).

to_float(S) ->
    to_value(S).

to_field_name(I) ->
    to_value(I).

to_identifier(I) ->
    ident_to_atom(I).

to_neg_identifier(I) ->
    signed_ident_to_atom(I).

ident_to_atom(I) ->
    case to_value(I) of
        "inf" -> pos_infinity;
        "infinity" -> pos_infinity;
        "Inf" -> pos_infinity;
        "Infinity" -> pos_infinity;
        "nan" -> undefined;
        V -> list_to_atom(V)
    end.

signed_ident_to_atom(I) ->
    case to_value(I) of
        "-inf" -> neg_infinity;
        "-infinity" -> neg_infinity;
        "-Inf" -> neg_infinity;
        "-Infinity" -> neg_infinity;
        "-nan" -> undefined;
        V -> V
    end.

to_value(V) ->
    element(3, V).

to_ext(Parts) ->
    TypeParts = [to_value(P) || P <- Parts],
    Type = string:join(TypeParts, "."),
    FullName = "[" ++ Type ++ "]",
    {extension, FullName, [list_to_atom(T) || T <- TypeParts]}.

to_any(DParts, TParts) ->
    DomainParts = [to_value(P) || P <- DParts],
    Domain = string:join(DomainParts, "."),
    TypeParts = [to_value(P) || P <- TParts],
    Type = string:join(TypeParts, "."),
    FullName = "[" ++ Domain ++ "/" ++ Type ++ "]",
    {any, FullName, [list_to_atom(D) || D <- DomainParts], [list_to_atom(T) || T <- TypeParts]}.
