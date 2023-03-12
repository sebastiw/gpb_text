Terminals string float identifier signed_identifier dec_signed_integer oct_signed_integer hex_signed_integer dec_unsigned_integer oct_unsigned_integer hex_unsigned_integer ':' ';' ',' '<' '>' '{' '}' '[' ']' '/' '.'.

Nonterminals message fields scalar_list scalar_field scalar_values scalar_value message_field message_values message_value message_list field_name qualified_name.

Rootsymbol message.

message -> fields : '$1'.

fields -> message_field : ['$1'].
fields -> message_field fields : ['$1'|'$2'].
fields -> scalar_field : ['$1'].
fields -> scalar_field fields : ['$1'|'$2'].

message_field -> field_name message_value ';' : {message_field, '$1', '$2'}.
message_field -> field_name message_value ',' : {message_field, '$1', '$2'}.
message_field -> field_name message_value : {message_field, '$1', '$2'}.
message_field -> field_name message_list ';' : {message_field, '$1', '$2'}.
message_field -> field_name message_list ',' : {message_field, '$1', '$2'}.
message_field -> field_name message_list : {message_field, '$1', '$2'}.
%% message_field -> field_name ':' message_value ';' : {message_field, '$1', '$2'}.
%% message_field -> field_name ':' message_value ',' : {message_field, '$1', '$2'}.
%% message_field -> field_name ':' message_value : {message_field, '$1', '$2'}.

%% message_field -> field_name ':' message_or_scalar_list ';' : {list, '$1', '$2'}.
%% message_field -> field_name ':' message_or_scalar_list ',' : {list, '$1', '$2'}.

scalar_field -> field_name ':' scalar_value ';' : {scalar_field, '$1', '$3'}.
scalar_field -> field_name ':' scalar_value ',' : {scalar_field, '$1', '$3'}.
scalar_field -> field_name ':' scalar_value : {scalar_field, '$1', '$3'}.
scalar_field -> field_name ':' scalar_list ';' : {scalar_field, '$1', '$3'}.
scalar_field -> field_name ':' scalar_list ',' : {scalar_field, '$1', '$3'}.
scalar_field -> field_name ':' scalar_list  : {scalar_field, '$1', '$3'}.

field_name -> '[' qualified_name ']' : '$1'.
field_name -> '[' qualified_name '/' qualified_name ']' : ['$1','/'|'$2'].
field_name -> identifier : '$1'.

qualified_name -> identifier : '$1'.
qualified_name -> identifier '.' qualified_name : ['$1','.'|'$2'].

%% message_or_scalar_list ->  '[' ']' : [].
%% message_or_scalar_list ->  '[' message_values ']' : '$2'.
%% message_or_scalar_list ->  '[' scalar_values ']' : '$2'.

message_list -> '[' ']' : {message_list, []}.
message_list -> '[' message_values ']' : ['$2'].
message_values -> message_value : ['$1'].
message_values -> message_value ',' message_values : ['$1'|'$3'].
message_value -> '{' message '}' : '$2'.
message_value -> '<' message '>' : '$2'.

scalar_list -> '[' ']' : {scalar_list, []}.
scalar_list -> '[' scalar_values ']' : ['$2'].
scalar_values -> scalar_value : ['$1'].
scalar_values -> scalar_value ',' scalar_values : ['$1'|'$3'].
scalar_value -> string : '$1'.
scalar_value -> float : '$1'.
scalar_value -> identifier : '$1'.
scalar_value -> signed_identifier : '$1'.
scalar_value -> dec_signed_integer : '$1'.
scalar_value -> oct_signed_integer : '$1'.
scalar_value -> hex_signed_integer : '$1'.
scalar_value -> dec_unsigned_integer : '$1'.
scalar_value -> oct_unsigned_integer : '$1'.
scalar_value -> hex_unsigned_integer : '$1'.



