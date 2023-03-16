Terminals string float identifier signed_identifier dec_signed_integer oct_signed_integer hex_signed_integer dec_unsigned_integer oct_unsigned_integer hex_unsigned_integer ':' ';' ',' '<' '>' '{' '}' '[' ']' extension_name any_name.

Nonterminals message fields empty_field empty_list scalar_list scalar_field scalar_values scalar_value message_field message_values message_value message_list field_name.

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

field_name -> identifier : to_atom('$1').
field_name -> extension_name : to_ext('$1').
field_name -> any_name : to_any('$1').

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
scalar_value -> float : to_float('$1').
scalar_value -> identifier : to_atom('$1').
scalar_value -> signed_identifier : signed_ident_to_atom('$1').
scalar_value -> dec_signed_integer : to_int('$1').
scalar_value -> oct_signed_integer : to_int('$1').
scalar_value -> hex_signed_integer : to_int('$1').
scalar_value -> dec_unsigned_integer : to_int('$1').
scalar_value -> oct_unsigned_integer : to_int('$1').
scalar_value -> hex_unsigned_integer : to_int('$1').

Erlang code.

to_bin(S) ->
    list_to_binary(element(3, S)).

to_atom(I) ->
    list_to_atom(element(3, I)).

signed_ident_to_atom(I) ->
    case element(3, I) of
        "-inf" -> neg_infinity;
        "-infinity" -> neg_infinity;
        "-Inf" -> neg_infinity;
        "-Infinity" -> neg_infinity;
        "nan" -> undefined;
        _ -> I
    end.

to_float(I) ->
    element(3, I).

to_int(I) ->
    element(3, I).

to_ext(I) ->
    FullName = element(3, I),
    TypeParts = string:tokens(FullName, "[]."),
    {extension, FullName, [list_to_atom(T) || T <- TypeParts]}.

to_any(I) ->
    FullName = element(3, I),
    N = string:trim(FullName, both, "[]"),
    [Domain, Type] = string:split(N, "/"),
    DomainParts = string:tokens(Domain, "."),
    TypeParts = string:tokens(Domain, "."),
    {any, FullName, [list_to_atom(D) || D <- DomainParts], [list_to_atom(T) || T <- TypeParts]}.
