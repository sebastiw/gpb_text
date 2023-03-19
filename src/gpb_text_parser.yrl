Terminals string float identifier signed_identifier dec_signed_integer oct_signed_integer hex_signed_integer dec_unsigned_integer oct_unsigned_integer hex_unsigned_integer ':' ';' ',' '<' '>' '{' '}' '[' ']' '/' extension.

Nonterminals message fields empty_field empty_list scalar_list scalar_field scalar_values scalar_value message_field message_values message_value message_list field_name extension_name any_name type_name domain.

Rootsymbol message.

message -> fields : '$1'.

fields -> message_field : ['$1'].
fields -> message_field fields : ['$1'|'$2'].
fields -> scalar_field : ['$1'].
fields -> scalar_field fields : ['$1'|'$2'].
fields -> empty_field : ['$1'].
fields -> empty_field fields : ['$1'|'$2'].

message_field -> field_name message_value ';' : {message, '$1', '$2'}.
message_field -> field_name message_value ',' : {message, '$1', '$2'}.
message_field -> field_name message_value : {message, '$1', '$2'}.
message_field -> field_name message_list ';' : {message, '$1', '$2'}.
message_field -> field_name message_list ',' : {message, '$1', '$2'}.
message_field -> field_name message_list : {message, '$1', '$2'}.
message_field -> field_name ':' message_value ';' : {message, '$1', '$3'}.
message_field -> field_name ':' message_value ',' : {message, '$1', '$3'}.
message_field -> field_name ':' message_value : {message, '$1', '$3'}.
message_field -> field_name ':' message_list ';' : {message, '$1', '$3'}.
message_field -> field_name ':' message_list ',' : {message, '$1', '$3'}.
message_field -> field_name ':' message_list : {message, '$1', '$3'}.

scalar_field -> field_name ':' scalar_value ';' : {scalar, '$1', '$3'}.
scalar_field -> field_name ':' scalar_value ',' : {scalar, '$1', '$3'}.
scalar_field -> field_name ':' scalar_value : {scalar, '$1', '$3'}.
scalar_field -> field_name ':' scalar_list ';' : {scalar, '$1', '$3'}.
scalar_field -> field_name ':' scalar_list ',' : {scalar, '$1', '$3'}.
scalar_field -> field_name ':' scalar_list  : {scalar, '$1', '$3'}.

%% Both message and scalar lists can reduce to empty lists
empty_field -> field_name ':' empty_list ';' : {empty, '$1', []}.
empty_field -> field_name ':' empty_list ',' : {empty, '$1', []}.
empty_field -> field_name ':' empty_list : {empty, '$1', []}.

%% FieldName     = ExtensionName | AnyName | IDENT ;
%% ExtensionName = "[", TypeName, "]" ;
%% AnyName       = "[", Domain, "/", TypeName, "]" ;
%% TypeName      = IDENT, { ".", IDENT } ;
%% Domain        = IDENT, { ".", IDENT } ;

field_name -> identifier : to_atom('$1').
field_name -> extension_name : '$1'.
field_name -> any_name : '$1'.

extension_name -> '[' type_name ']' : to_ext('$2').
type_name -> identifier : '$1'.
type_name -> extension : '$1'.

any_name -> '[' domain '/' type_name ']' : to_any('$2', '$4').
domain -> identifier : '$1'.
domain -> extension : '$1'.

empty_list -> '[' ']' : [].

% message_list -> empty_list : '$1'.
message_list -> '[' message_values ']' : '$2'.
message_values -> message_value : ['$1'].
message_values -> message_value ',' message_values : ['$1'|'$3'].
message_value -> '{' message '}' : '$2'.
message_value -> '<' message '>' : '$2'.

% scalar_list -> empty_list : '$1'.
scalar_list -> '[' scalar_values ']' : '$2'.
scalar_values -> scalar_value : ['$1'].
scalar_values -> scalar_value ',' scalar_values : ['$1'|'$3'].
scalar_value -> string : to_bin('$1').
scalar_value -> float : to_value('$1').
scalar_value -> identifier : ident_to_atom('$1').
scalar_value -> signed_identifier : signed_ident_to_atom('$1').
scalar_value -> dec_signed_integer : to_value('$1').
scalar_value -> oct_signed_integer : to_value('$1').
scalar_value -> hex_signed_integer : to_value('$1').
scalar_value -> dec_unsigned_integer : to_value('$1').
scalar_value -> oct_unsigned_integer : to_value('$1').
scalar_value -> hex_unsigned_integer : to_value('$1').

Erlang code.

to_bin(S) ->
    list_to_binary(to_value(S)).

to_atom(I) ->
    list_to_atom(to_value(I)).

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

to_ext(E) ->
    Type = to_value(E),
    TypeParts = string:split(Type, ".", all),
    FullName = "[" ++ Type ++ "]",
    {extension, FullName, [list_to_atom(T) || T <- TypeParts]}.

to_any(D0, T0) ->
    Domain = to_value(D0),
    Type = to_value(T0),
    DomainParts = string:split(Domain, ".", all),
    TypeParts = string:split(Type, ".", all),
    FullName = "[" ++ Domain ++ "/" ++ Type ++ "]",
    {any, FullName, [list_to_atom(D) || D <- DomainParts], [list_to_atom(T) || T <- TypeParts]}.
