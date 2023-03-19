-module(gpb_text_lexer_tests).

-include_lib("eunit/include/eunit.hrl").

%% value: -2.0   # Valid: no additional whitespace.
%% value: - 2.0  # Valid: whitespace between '-' and '2.0'.
%% value: -
%%   # comment
%%   2.0         # Valid: whitespace and comments between '-' and '2.0'.
%% value: 2 . 0  # Invalid: the floating point period is part of the lexical
%%               # element, so no additional whitespace is allowed.

signed_floating_point_no_additional_whitespace_test() ->
    Tokens = gpb_text_lexer:string("value: -2.0"),
    ?assertMatch({ok, [{identifier, _, "value"}, _, {float, _, -2.0}], _}, Tokens).

signed_floating_point_whitespace_between_test() ->
    Tokens = gpb_text_lexer:string("value: - 2.0"),
    ?assertMatch({ok, [{identifier, _, "value"}, _, {float, _, -2.0}], _}, Tokens).

%% A person who actually does this should probably be hanged.
signed_floating_point_whitespace_and_comment_between_test() ->
    Tokens = gpb_text_lexer:string("value: -\n  # comment\n 2.0"),
    ?assertMatch({ok, [{identifier, _, "value"}, _, {float, _, -2.0}], _}, Tokens).

signed_floating_point_invalid_whitespace_test() ->
    Tokens = gpb_text_lexer:string("value: 2 . 0"),
    ?assertMatch({error, {_, _, {illegal, _}}, _}, Tokens).

%% foo: 10 bar: 20           # Valid: whitespace separates '10' and 'bar'
%% foo: 10,bar: 20           # Valid: ',' separates '10' and 'bar'
%% foo: 10[com.foo.ext]: 20  # Valid: '10' is followed immediately by '[', which is
%%                           # not an identifier.
%% foo: 10bar: 20            # Invalid: no space between '10' and identifier 'bar'.

number_token_not_followed_by_identifier_test() ->
    Tokens = gpb_text_lexer:string("foo: 10 bar: 20"),
    ?assertMatch({ok, [{identifier, _, "foo"}, _,
                       {dec_unsigned_integer, _, 10},
                       {identifier, _, "bar"}, _,
                       {dec_unsigned_integer, _, 20}], _}, Tokens).

number_token_comma_separator_test() ->
    Tokens = gpb_text_lexer:string("foo: 10,bar: 20"),
    ?assertMatch({ok, [{identifier, _, "foo"}, _,
                       {dec_unsigned_integer, _, 10}, _,
                       {identifier, _, "bar"}, _,
                       {dec_unsigned_integer, _, 20}], _}, Tokens).

number_token_followed_by_non_identifier_test() ->
    Tokens = gpb_text_lexer:string("foo: 10[com.foo.ext]: 20"),
    ?assertMatch({ok, [{identifier, _, "foo"},
                       {':', _, ":"},
                       {dec_unsigned_integer, _, 10},
                       {'[', _, "["},
                       {extension, _, "com.foo.ext"},
                       {']', _, "]"},
                       {':', _, ":"},
                       {dec_unsigned_integer, _, 20}], _}, Tokens).

%% not possible to test, lexer will accept this.
%% number_token_invalid_non_space_test() ->
%%     Tokens = gpb_text_lexer:string("foo: 10bar: 20"),
%%     ?assertMatch({error, {_, _, {illegal, _}}, _}, Tokens).

%% Decimal integers can be cast as floating-point values by using the
%% F and f suffixes. Example:
%%
%% foo: 10    # This is an integer value.
%% foo: 10f   # This is a floating-point value.
%% foo: 1.0f  # Also optional for floating-point literals.

decimal_integer_cast_integer_value_test() ->
    Tokens = gpb_text_lexer:string("foo: 10"),
    ?assertMatch({ok, [{identifier, _, "foo"}, _,
                       {dec_unsigned_integer, _, 10}], _}, Tokens).

decimal_integer_cast_floating_point_value_test() ->
    Tokens = gpb_text_lexer:string("foo: 10f"),
    ?assertMatch({ok, [{identifier, _, "foo"}, _,
                       {float, _, 10.0}], _}, Tokens).

decimal_integer_cast_floating_point_literal_test() ->
    Tokens = gpb_text_lexer:string("foo: 1.0f"),
    ?assertMatch({ok, [{identifier, _, "foo"}, _,
                       {float, _, 1.0}], _}, Tokens).


%% Longer strings can be broken into several quoted strings on
%% successive lines. For example:
%%
%% quote:
%%     "When we got into office, the thing that surprised me most was to find "
%%     "that things were just as bad as we'd been saying they were.\n\n"
%%     "  -- John F. Kennedy"

string_long_quote_test() ->
    Tokens = gpb_text_lexer:string("quote: \"When we got into office, the thing that surprised me most was to find \"\n"
                                    "       \"that things were just as bad as we'd been saying they were.\n\n\"\n"
                                    "       \"  -- John F. Kennedy\""),
    ?assertMatch({ok, [{identifier, _, "quote"}, _,
                       {string, _, "When we got into office, the thing that surprised me most was to find "},
                       {string, _, "that things were just as bad as we'd been saying they were.\n\n"},
                       {string, _, "  -- John F. Kennedy"}
                      ], _}, Tokens).

%% A single string value can comprise multiple quoted parts separated
%% by optional whitespace. Example:
%%
%% a_string: "first part" 'second part'
%%           "third part"
%% no_whitespace: "first""second"'third''fourth'

single_string_multiple_parts_test() ->
    Tokens = gpb_text_lexer:string("a_string: \"first part\" 'second part'\n"
                                    "          \"third part\""),
    ?assertMatch({ok, [{identifier, _, "a_string"}, _,
                       {string, _, "first part"},
                       {string, _, "second part"},
                       {string, _, "third part"}], _}, Tokens).

single_string_multiple_parts_no_whitespace_test() ->
    Tokens = gpb_text_lexer:string("no_whitespace: \"first\"\"second\"'third'\"fourth\""),
    ?assertMatch({ok, [{identifier, _, "no_whitespace"}, _,
                       {string, _, "first"},
                       {string, _, "second"},
                       {string, _, "third"},
                       {string, _, "fourth"}], _}, Tokens).

%% Regular fields and extension fields can have scalar or message
%% values. Any fields are always messages. Example:
%%
%% reg_scalar: 10
%% reg_message { foo: "bar" }
%%
%% [com.foo.ext.scalar]​: 10
%% [com.foo.ext.message] { foo: "bar" }
%%
%% any_value {
%%   [type.googleapis.com/com.foo.any] { foo: "bar" }
%% }

regular_scalar_test() ->
    Tokens = gpb_text_lexer:string("reg_scalar: 10"),
    ?assertMatch({ok, [{identifier, _, "reg_scalar"}, _, {dec_unsigned_integer, _, 10}], _}, Tokens).

regular_message_test() ->
    Tokens = gpb_text_lexer:string("reg_message { foo: \"bar\" }"),
    ?assertMatch({ok, [{identifier, _, "reg_message"},
                       {'{', _, "{"},
                       {identifier, _, "foo"},
                       {':', _, ":"},
                       {string, _, "bar"},
                       {'}', _, "}"}], _}, Tokens).

regular_extended_scalar_test() ->
    Tokens = gpb_text_lexer:string("[com.foo.ext.scalar]: 10"),
    ?assertMatch({ok, [{'[', _, "["},
                       {extension, _, "com.foo.ext.scalar"},
                       {']', _, "]"},
                       {':', _, ":"},
                       {dec_unsigned_integer, _, 10}], _}, Tokens).

regular_extended_message_test() ->
    Tokens = gpb_text_lexer:string("[com.foo.ext.message] { foo: \"bar\" }"),
    ?assertMatch({ok, [{'[', _, "["},
                       {extension, _, "com.foo.ext.message"},
                       {']', _, "]"},
                       {'{', _, "{"},
                       {identifier, _, "foo"},
                       {':', _, ":"},
                       {string, _, "bar"},
                       {'}', _, "}"}], _}, Tokens).

regular_any_value_test() ->
    Tokens = gpb_text_lexer:string("any_value {\n"
                                    "  [type.googleapis.com/com.foo.any] { foo: \"bar\" }\n"
                                    "}"),
    ?assertMatch({ok, [{identifier, _, "any_value"},
                       {'{', _, "{"},
                       {'[', _, "["},
                       {extension, _, "type.googleapis.com"},
                       {'/', _, "/"},
                       {extension, _, "com.foo.any"},
                       {']', _, "]"},
                       {'{', _, "{"},
                       {identifier, _, "foo"},
                       {':', _, ":"},
                       {string, _, "bar"},
                       {'}', _, "}"},
                       {'}', _, "}"}], _}, Tokens).

%% The : delimiter between the field name and value is required for
%% scalar fields but optional for message fields (including
%% lists). Example:
%%
%% scalar: 10          # Valid
%% scalar  10          # Invalid
%% scalars: [1, 2, 3]  # Valid
%% scalars  [1, 2, 3]  # Invalid
%% message: {}         # Valid
%% message  {}         # Valid
%% messages: [{}, {}]  # Valid
%% messages  [{}, {}]  # Valid

colon_delimiter_scalar_test() ->
    Tokens = gpb_text_lexer:string("scalar: 10"),
    ?assertMatch({ok, [{identifier, _, "scalar"},
                       {':', _, ":"},
                       {dec_unsigned_integer, _, 10}], _}, Tokens).

colon_delimiter_non_valid_scalar_test() ->
    %% Lexically ok
    Tokens = gpb_text_lexer:string("scalar 10"),
    ?assertMatch({ok, [{identifier, _, "scalar"},
                       {dec_unsigned_integer, _, 10}], _}, Tokens).

colon_delimiter_scalar_list_test() ->
    Tokens = gpb_text_lexer:string("scalars: [1, 2, 3]"),
    ?assertMatch({ok, [{identifier, _, "scalars"}, _,
                       {'[', _, "["},
                       {dec_unsigned_integer, _, 1},
                       {',', _, ","},
                       {dec_unsigned_integer, _, 2},
                       {',', _, ","},
                       {dec_unsigned_integer, _, 3},
                       {']', _, "]"}], _}, Tokens).

colon_delimiter_non_valid_scalar_list_test() ->
    %% Lexically ok
    Tokens = gpb_text_lexer:string("scalars [1, 2, 3]"),
    ?assertMatch({ok, [{identifier, _, "scalars"},
                       {'[', _, "["},
                       {dec_unsigned_integer, _, 1},
                       {',', _, ","},
                       {dec_unsigned_integer, _, 2},
                       {',', _, ","},
                       {dec_unsigned_integer, _, 3},
                       {']', _, "]"}], _}, Tokens).

colon_delimiter_message_with_colon_test() ->
    Tokens = gpb_text_lexer:string("message: {}"),
    ?assertMatch({ok, [{identifier, _, "message"},
                       {':', _, ":"},
                       {'{', _, "{"},
                       {'}', _, "}"}], _}, Tokens).

colon_delimiter_message_without_colon_test() ->
    Tokens = gpb_text_lexer:string("message {}"),
    ?assertMatch({ok, [{identifier, _, "message"},
                       {'{', _, "{"},
                       {'}', _, "}"}], _}, Tokens).

colon_delimiter_message_list_with_colon_test() ->
    Tokens = gpb_text_lexer:string("messages: [{}, {}]"),
    ?assertMatch({ok, [{identifier, _, "messages"}, _,
                       {'[', _, "["},
                       {'{', _, "{"},
                       {'}', _, "}"},
                       {',', _, ","},
                       {'{', _, "{"},
                       {'}', _, "}"},
                       {']', _, "]"}], _}, Tokens).

colon_delimiter_message_list_without_colon_test() ->
    Tokens = gpb_text_lexer:string("messages [{}, {}]"),
    ?assertMatch({ok, [{identifier, _, "messages"},
                       {'[', _, "["},
                       {'{', _, "{"},
                       {'}', _, "}"},
                       {',', _, ","},
                       {'{', _, "{"},
                       {'}', _, "}"},
                       {']', _, "]"}], _}, Tokens).
%% Values of message fields can be surrounded by curly brackets or
%% angle brackets:
%%
%% message: { foo: "bar" }
%% message: < foo: "bar" >

message_with_curly_brackets_test() ->
    Tokens = gpb_text_lexer:string("message: { foo: \"bar\" }"),
    ?assertMatch({ok, [{identifier, _, "message"},
                       {':', _, ":"},
                       {'{', _, "{"},
                       {identifier, _, "foo"},
                       {':', _, ":"},
                       {string, _, "bar"},
                       {'}', _, "}"}], _}, Tokens).

message_with_angle_brackets_test() ->
    Tokens = gpb_text_lexer:string("message: < foo: \"bar\" >"),
    ?assertMatch({ok, [{identifier, _, "message"},
                       {':', _, ":"},
                       {'<', _, "<"},
                       {identifier, _, "foo"},
                       {':', _, ":"},
                       {string, _, "bar"},
                       {'>', _, ">"}], _}, Tokens).
