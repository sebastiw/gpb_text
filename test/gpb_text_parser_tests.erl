-module(gpb_text_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gpb_text/include/gpb_text.hrl").

signed_floating_point_invalid_whitespace_test() ->
    {ok, Symbols, _} = gpb_text_lexer:string("value: 2 . 0"),
    ExpectedMsg = {error,{1, gpb_text_parser,
                          ["syntax error before: ", ["\".\""]]}},
    ?assertMatch(ExpectedMsg, gpb_text_parser:parse(Symbols)).

colon_delimiter_non_valid_scalar_test() ->
    {ok, Symbols, _} = gpb_text_lexer:string("scalar 10"),
    ExpectedMsg = {error,{1, gpb_text_parser,
                          ["syntax error before: ", "10"]}},
    ?assertMatch(ExpectedMsg, gpb_text_parser:parse(Symbols)).

colon_delimiter_non_valid_scalar_list_test() ->
    {ok, Symbols, _} = gpb_text_lexer:string("scalars [1, 2, 3]"),
    ExpectedMsg = {error,{1, gpb_text_parser,
                          ["syntax error before: ", "1"]}},
    ?assertMatch(ExpectedMsg, gpb_text_parser:parse(Symbols)).

default_test() ->
    Symbols = get_symbols_from_file("default_example.textproto"),
    ExpectedMsg = [#scalar{key = "name", value = <<"John Smith">>},
                   #message{name = "pet",
                            fields = [#scalar{key = "kind", value = 'DOG'},
                                      #scalar{key = "name", value = <<"Fluffy">>},
                                      #scalar{key = "tail_wagginess", value = 0.65}]},
                   #message{name = "pet",
                            fields = [#scalar{key = "kind", value = 'LIZARD'},
                                      #scalar{key = "name", value = <<"Lizzy">>},
                                      #scalar{key = "legs", value = 4}]},
                   #scalar{key = "string_value_with_escape", value = <<"valid \\n escape">>},
                   #scalar{key = "repeated_values",
                           value = [<<"one">>,
                                    <<"two">>,
                                    <<"three">>]}
                  ],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).

message_with_map_test() ->
    Symbols = get_symbols_from_file("message_with_map.textproto"),
    ExpectedMsg = [#message{name = "my_map",
                            fields = [#scalar{key = "key",
                                              value = <<"entry1">>},
                                      #scalar{key = "value",
                                              value = 1}]},
                   #message{name = "my_map",
                            fields = [#scalar{key = "key",
                                              value = <<"entry2">>},
                                      #scalar{key = "value",
                                              value = 2}]},
                   #message{name = "my_map",
                            fields = [[#scalar{key = "key",
                                               value = <<"entry3">>},
                                       #scalar{key = "value",
                                               value = 3}],
                                      [#scalar{key = "key",
                                               value = <<"entry4">>},
                                       #scalar{key = "value",
                                               value = 4}]]}],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).

map_test_data_test() ->
    Symbols = get_symbols_from_file("map_test_data.textproto"),
    ExpectedMsg = [#message{name = "map_int32_int32",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_int32_int32",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_int64_int64",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_int64_int64",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_uint32_uint32",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_uint32_uint32",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_uint64_uint64",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_uint64_uint64",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_sint32_sint32",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_sint32_sint32",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_sint64_sint64",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_sint64_sint64",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_fixed32_fixed32",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_fixed32_fixed32",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_fixed64_fixed64",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_fixed64_fixed64",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_sfixed32_sfixed32",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_sfixed32_sfixed32",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_sfixed64_sfixed64",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_sfixed64_sfixed64",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_int32_float",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_int32_float",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_int32_double",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 0}]},
                   #message{name = "map_int32_double",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 1}]},
                   #message{name = "map_bool_bool",
                            fields = [#scalar{key = "key", value = false},
                                      #scalar{key = "value", value = false}]},
                   #message{name = "map_bool_bool",
                            fields = [#scalar{key = "key", value = true},
                                      #scalar{key = "value", value = true}]},
                   #message{name = "map_string_string",
                            fields = [#scalar{key = "key", value = <<"0">>},
                                      #scalar{key = "value", value = <<"0">>}]},
                   #message{name = "map_string_string",
                            fields = [#scalar{key = "key", value = <<"1">>},
                                      #scalar{key = "value", value = <<"1">>}]},
                   #message{name = "map_int32_bytes",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = <<"0">>}]},
                   #message{name = "map_int32_bytes",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = <<"1">>}]},
                   #message{name = "map_int32_enum",
                            fields = [#scalar{key = "key", value = 0},
                                      #scalar{key = "value", value = 'MAP_ENUM_BAR'}]},
                   #message{name = "map_int32_enum",
                            fields = [#scalar{key = "key", value = 1},
                                      #scalar{key = "value", value = 'MAP_ENUM_BAZ'}]},
                   #message{name = "map_int32_foreign_message",
                            fields = [#scalar{key = "key", value = 0},
                                      #message{name = "value",
                                               fields = [#scalar{key = "c", value = 0}]}]},
                   #message{name = "map_int32_foreign_message",
                            fields = [#scalar{key = "key", value = 1},
                                      #message{name = "value",
                                               fields = [#scalar{key = "c", value = 1}]}]}],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).

text_format_unittest_data_oneof_implemented_test() ->
    Symbols = get_symbols_from_file("text_format_unittest_data_oneof_implemented.textproto"),
    ExpectedMsg = [#scalar{key = "optional_int32", value = 101},
                   #scalar{key = "optional_int64", value = 102},
                   #scalar{key = "optional_uint32", value = 103},
                   #scalar{key = "optional_uint64", value = 104},
                   #scalar{key = "optional_sint32", value = 105},
                   #scalar{key = "optional_sint64", value = 106},
                   #scalar{key = "optional_fixed32", value = 107},
                   #scalar{key = "optional_fixed64", value = 108},
                   #scalar{key = "optional_sfixed32", value = 109},
                   #scalar{key = "optional_sfixed64", value = 110},
                   #scalar{key = "optional_float", value = 111},
                   #scalar{key = "optional_double", value = 112},
                   #scalar{key = "optional_bool", value = true},
                   #scalar{key = "optional_string", value = <<"115">>},
                   #scalar{key = "optional_bytes", value = <<"116">>},
                   #message{name = "OptionalGroup",
                            fields = [#scalar{key = "a", value = 117}]},
                   #message{name = "optional_nested_message",
                            fields = [#scalar{key = "bb", value = 118}]},
                   #message{name = "optional_foreign_message",
                            fields = [#scalar{key = "c", value = 119}]},
                   #message{name = "optional_import_message",
                            fields = [#scalar{key = "d", value = 120}]},
                   #scalar{key = "optional_nested_enum", value = 'BAZ'},
                   #scalar{key = "optional_foreign_enum", value = 'FOREIGN_BAZ'},
                   #scalar{key = "optional_import_enum", value = 'IMPORT_BAZ'},
                   #scalar{key = "optional_string_piece", value = <<"124">>},
                   #scalar{key = "optional_cord", value = <<"125">>},
                   #message{name = "optional_public_import_message",
                            fields = [#scalar{key = "e", value = 126}]},
                   #message{name = "optional_lazy_message",
                            fields = [#scalar{key = "bb", value = 127}]},
                   #message{name = "optional_unverified_lazy_message",
                            fields = [#scalar{key = "bb", value = 128}]},
                   #scalar{key = "repeated_int32", value = 201},
                   #scalar{key = "repeated_int32", value = 301},
                   #scalar{key = "repeated_int64", value = 202},
                   #scalar{key = "repeated_int64", value = 302},
                   #scalar{key = "repeated_uint32", value = 203},
                   #scalar{key = "repeated_uint32", value = 303},
                   #scalar{key = "repeated_uint64", value = 204},
                   #scalar{key = "repeated_uint64", value = 304},
                   #scalar{key = "repeated_sint32", value = 205},
                   #scalar{key = "repeated_sint32", value = 305},
                   #scalar{key = "repeated_sint64", value = 206},
                   #scalar{key = "repeated_sint64", value = 306},
                   #scalar{key = "repeated_fixed32", value = 207},
                   #scalar{key = "repeated_fixed32", value = 307},
                   #scalar{key = "repeated_fixed64", value = 208},
                   #scalar{key = "repeated_fixed64", value = 308},
                   #scalar{key = "repeated_sfixed32", value = 209},
                   #scalar{key = "repeated_sfixed32", value = 309},
                   #scalar{key = "repeated_sfixed64", value = 210},
                   #scalar{key = "repeated_sfixed64", value = 310},
                   #scalar{key = "repeated_float", value = 211},
                   #scalar{key = "repeated_float", value = 311},
                   #scalar{key = "repeated_double", value = 212},
                   #scalar{key = "repeated_double", value = 312},
                   #scalar{key = "repeated_bool", value = true},
                   #scalar{key = "repeated_bool", value = false},
                   #scalar{key = "repeated_string", value = <<"215">>},
                   #scalar{key = "repeated_string", value = <<"315">>},
                   #scalar{key = "repeated_bytes", value = <<"216">>},
                   #scalar{key = "repeated_bytes", value = <<"316">>},
                   #message{name = "RepeatedGroup",
                            fields = [#scalar{key = "a", value = 217}]},
                   #message{name = "RepeatedGroup",
                            fields = [#scalar{key = "a", value = 317}]},
                   #message{name = "repeated_nested_message",
                            fields = [#scalar{key = "bb", value = 218}]},
                   #message{name = "repeated_nested_message",
                            fields = [#scalar{key = "bb", value = 318}]},
                   #message{name = "repeated_foreign_message",
                            fields = [#scalar{key = "c", value = 219}]},
                   #message{name = "repeated_foreign_message",
                            fields = [#scalar{key = "c", value = 319}]},
                   #message{name = "repeated_import_message",
                            fields = [#scalar{key = "d", value = 220}]},
                   #message{name = "repeated_import_message",
                            fields = [#scalar{key = "d", value = 320}]},
                   #scalar{key = "repeated_nested_enum", value = 'BAR'},
                   #scalar{key = "repeated_nested_enum", value = 'BAZ'},
                   #scalar{key = "repeated_foreign_enum", value = 'FOREIGN_BAR'},
                   #scalar{key = "repeated_foreign_enum", value = 'FOREIGN_BAZ'},
                   #scalar{key = "repeated_import_enum", value = 'IMPORT_BAR'},
                   #scalar{key = "repeated_import_enum", value = 'IMPORT_BAZ'},
                   #scalar{key = "repeated_string_piece", value = <<"224">>},
                   #scalar{key = "repeated_string_piece", value = <<"324">>},
                   #scalar{key = "repeated_cord", value = <<"225">>},
                   #scalar{key = "repeated_cord", value = <<"325">>},
                   #message{name = "repeated_lazy_message",
                            fields = [#scalar{key = "bb", value = 227}]},
                   #message{name = "repeated_lazy_message",
                            fields = [#scalar{key = "bb", value = 327}]},
                   #scalar{key = "default_int32", value = 401},
                   #scalar{key = "default_int64", value = 402},
                   #scalar{key = "default_uint32", value = 403},
                   #scalar{key = "default_uint64", value = 404},
                   #scalar{key = "default_sint32", value = 405},
                   #scalar{key = "default_sint64", value = 406},
                   #scalar{key = "default_fixed32", value = 407},
                   #scalar{key = "default_fixed64", value = 408},
                   #scalar{key = "default_sfixed32", value = 409},
                   #scalar{key = "default_sfixed64", value = 410},
                   #scalar{key = "default_float", value = 411},
                   #scalar{key = "default_double", value = 412},
                   #scalar{key = "default_bool", value = false},
                   #scalar{key = "default_string", value = <<"415">>},
                   #scalar{key = "default_bytes", value = <<"416">>},
                   #scalar{key = "default_nested_enum", value = 'FOO'},
                   #scalar{key = "default_foreign_enum", value = 'FOREIGN_FOO'},
                   #scalar{key = "default_import_enum", value = 'IMPORT_FOO'},
                   #scalar{key = "default_string_piece", value = <<"424">>},
                   #scalar{key = "default_cord", value = <<"425">>},
                   #scalar{key = "oneof_bytes", value = <<"604">>}],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).

text_format_unittest_data_pointy_oneof_test() ->
    Symbols = get_symbols_from_file("text_format_unittest_data_pointy_oneof.textproto"),
    ExpectedMsg = [#scalar{key = "optional_int32", value = 101},
                   #scalar{key = "optional_int64", value = 102},
                   #scalar{key = "optional_uint32", value = 103},
                   #scalar{key = "optional_uint64", value = 104},
                   #scalar{key = "optional_sint32", value = 105},
                   #scalar{key = "optional_sint64", value = 106},
                   #scalar{key = "optional_fixed32", value = 107},
                   #scalar{key = "optional_fixed64", value = 108},
                   #scalar{key = "optional_sfixed32", value = 109},
                   #scalar{key = "optional_sfixed64", value = 110},
                   #scalar{key = "optional_float", value = 111},
                   #scalar{key = "optional_double", value = 112},
                   #scalar{key = "optional_bool", value = true},
                   #scalar{key = "optional_string", value = <<"115">>},
                   #scalar{key = "optional_bytes", value = <<"116">>},
                   #message{name = "OptionalGroup",
                            fields = [#scalar{key = "a", value = 117}]},
                   #message{name = "optional_nested_message",
                            fields = [#scalar{key = "bb", value = 118}]},
                   #message{name = "optional_foreign_message",
                            fields = [#scalar{key = "c", value = 119}]},
                   #message{name = "optional_import_message",
                            fields = [#scalar{key = "d", value = 120}]},
                   #scalar{key = "optional_nested_enum", value = 'BAZ'},
                   #scalar{key = "optional_foreign_enum", value = 'FOREIGN_BAZ'},
                   #scalar{key = "optional_import_enum", value = 'IMPORT_BAZ'},
                   #scalar{key = "optional_string_piece", value = <<"124">>},
                   #scalar{key = "optional_cord", value = <<"125">>},
                   #message{name = "optional_public_import_message",
                            fields = [#scalar{key = "e", value = 126}]},
                   #message{name = "optional_lazy_message",
                            fields = [#scalar{key = "bb", value = 127}]},
                   #message{name = "optional_unverified_lazy_message",
                            fields = [#scalar{key = "bb", value = 128}]},
                   #scalar{key = "repeated_int32", value = 201},
                   #scalar{key = "repeated_int32", value = 301},
                   #scalar{key = "repeated_int64", value = 202},
                   #scalar{key = "repeated_int64", value = 302},
                   #scalar{key = "repeated_uint32", value = 203},
                   #scalar{key = "repeated_uint32", value = 303},
                   #scalar{key = "repeated_uint64", value = 204},
                   #scalar{key = "repeated_uint64", value = 304},
                   #scalar{key = "repeated_sint32", value = 205},
                   #scalar{key = "repeated_sint32", value = 305},
                   #scalar{key = "repeated_sint64", value = 206},
                   #scalar{key = "repeated_sint64", value = 306},
                   #scalar{key = "repeated_fixed32", value = 207},
                   #scalar{key = "repeated_fixed32", value = 307},
                   #scalar{key = "repeated_fixed64", value = 208},
                   #scalar{key = "repeated_fixed64", value = 308},
                   #scalar{key = "repeated_sfixed32", value = 209},
                   #scalar{key = "repeated_sfixed32", value = 309},
                   #scalar{key = "repeated_sfixed64", value = 210},
                   #scalar{key = "repeated_sfixed64", value = 310},
                   #scalar{key = "repeated_float", value = 211},
                   #scalar{key = "repeated_float", value = 311},
                   #scalar{key = "repeated_double", value = 212},
                   #scalar{key = "repeated_double", value = 312},
                   #scalar{key = "repeated_bool", value = true},
                   #scalar{key = "repeated_bool", value = false},
                   #scalar{key = "repeated_string", value = <<"215">>},
                   #scalar{key = "repeated_string", value = <<"315">>},
                   #scalar{key = "repeated_bytes", value = <<"216">>},
                   #scalar{key = "repeated_bytes", value = <<"316">>},
                   #message{name = "RepeatedGroup",
                            fields = [#scalar{key = "a", value = 217}]},
                   #message{name = "RepeatedGroup",
                            fields = [#scalar{key = "a", value = 317}]},
                   #message{name = "repeated_nested_message",
                            fields = [#scalar{key = "bb", value = 218}]},
                   #message{name = "repeated_nested_message",
                            fields = [#scalar{key = "bb", value = 318}]},
                   #message{name = "repeated_foreign_message",
                            fields = [#scalar{key = "c", value = 219}]},
                   #message{name = "repeated_foreign_message",
                            fields = [#scalar{key = "c", value = 319}]},
                   #message{name = "repeated_import_message",
                            fields = [#scalar{key = "d", value = 220}]},
                   #message{name = "repeated_import_message",
                            fields = [#scalar{key = "d", value = 320}]},
                   #scalar{key = "repeated_nested_enum", value = 'BAR'},
                   #scalar{key = "repeated_nested_enum", value = 'BAZ'},
                   #scalar{key = "repeated_foreign_enum", value = 'FOREIGN_BAR'},
                   #scalar{key = "repeated_foreign_enum", value = 'FOREIGN_BAZ'},
                   #scalar{key = "repeated_import_enum", value = 'IMPORT_BAR'},
                   #scalar{key = "repeated_import_enum", value = 'IMPORT_BAZ'},
                   #scalar{key = "repeated_string_piece", value = <<"224">>},
                   #scalar{key = "repeated_string_piece", value = <<"324">>},
                   #scalar{key = "repeated_cord", value = <<"225">>},
                   #scalar{key = "repeated_cord", value = <<"325">>},
                   #message{name = "repeated_lazy_message",
                            fields = [#scalar{key = "bb", value = 227}]},
                   #message{name = "repeated_lazy_message",
                            fields = [#scalar{key = "bb", value = 327}]},
                   #scalar{key = "default_int32", value = 401},
                   #scalar{key = "default_int64", value = 402},
                   #scalar{key = "default_uint32", value = 403},
                   #scalar{key = "default_uint64", value = 404},
                   #scalar{key = "default_sint32", value = 405},
                   #scalar{key = "default_sint64", value = 406},
                   #scalar{key = "default_fixed32", value = 407},
                   #scalar{key = "default_fixed64", value = 408},
                   #scalar{key = "default_sfixed32", value = 409},
                   #scalar{key = "default_sfixed64", value = 410},
                   #scalar{key = "default_float", value = 411},
                   #scalar{key = "default_double", value = 412},
                   #scalar{key = "default_bool", value = false},
                   #scalar{key = "default_string", value = <<"415">>},
                   #scalar{key = "default_bytes", value = <<"416">>},
                   #scalar{key = "default_nested_enum", value = 'FOO'},
                   #scalar{key = "default_foreign_enum", value = 'FOREIGN_FOO'},
                   #scalar{key = "default_import_enum", value = 'IMPORT_FOO'},
                   #scalar{key = "default_string_piece", value = <<"424">>},
                   #scalar{key = "default_cord", value = <<"425">>},
                   #scalar{key = "oneof_bytes", value = <<"604">>}],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).

text_format_unittest_data_pointy_test() ->
    Symbols = get_symbols_from_file("text_format_unittest_data_pointy.textproto"),
    ExpectedMsg = [#scalar{key = "optional_int32", value = 101},
                   #scalar{key = "optional_int64", value = 102},
                   #scalar{key = "optional_uint32", value = 103},
                   #scalar{key = "optional_uint64", value = 104},
                   #scalar{key = "optional_sint32", value = 105},
                   #scalar{key = "optional_sint64", value = 106},
                   #scalar{key = "optional_fixed32", value = 107},
                   #scalar{key = "optional_fixed64", value = 108},
                   #scalar{key = "optional_sfixed32", value = 109},
                   #scalar{key = "optional_sfixed64", value = 110},
                   #scalar{key = "optional_float", value = 111},
                   #scalar{key = "optional_double", value = 112},
                   #scalar{key = "optional_bool", value = true},
                   #scalar{key = "optional_string", value = <<"115">>},
                   #scalar{key = "optional_bytes", value = <<"116">>},
                   #message{name = "OptionalGroup",
                            fields = [#scalar{key = "a", value = 117}]},
                   #message{name = "optional_nested_message",
                            fields = [#scalar{key = "bb", value = 118}]},
                   #message{name = "optional_foreign_message",
                            fields = [#scalar{key = "c", value = 119}]},
                   #message{name = "optional_import_message",
                            fields = [#scalar{key = "d", value = 120}]},
                   #scalar{key = "optional_nested_enum", value = 'BAZ'},
                   #scalar{key = "optional_foreign_enum", value = 'FOREIGN_BAZ'},
                   #scalar{key = "optional_import_enum", value = 'IMPORT_BAZ'},
                   #scalar{key = "optional_string_piece", value = <<"124">>},
                   #scalar{key = "optional_cord", value = <<"125">>},
                   #message{name = "optional_public_import_message",
                            fields = [#scalar{key = "e", value = 126}]},
                   #message{name = "optional_lazy_message",
                            fields = [#scalar{key = "bb", value = 127}]},
                   #message{name = "optional_unverified_lazy_message",
                            fields = [#scalar{key = "bb", value = 128}]},
                   #scalar{key = "repeated_int32", value = 201},
                   #scalar{key = "repeated_int32", value = 301},
                   #scalar{key = "repeated_int64", value = 202},
                   #scalar{key = "repeated_int64", value = 302},
                   #scalar{key = "repeated_uint32", value = 203},
                   #scalar{key = "repeated_uint32", value = 303},
                   #scalar{key = "repeated_uint64", value = 204},
                   #scalar{key = "repeated_uint64", value = 304},
                   #scalar{key = "repeated_sint32", value = 205},
                   #scalar{key = "repeated_sint32", value = 305},
                   #scalar{key = "repeated_sint64", value = 206},
                   #scalar{key = "repeated_sint64", value = 306},
                   #scalar{key = "repeated_fixed32", value = 207},
                   #scalar{key = "repeated_fixed32", value = 307},
                   #scalar{key = "repeated_fixed64", value = 208},
                   #scalar{key = "repeated_fixed64", value = 308},
                   #scalar{key = "repeated_sfixed32", value = 209},
                   #scalar{key = "repeated_sfixed32", value = 309},
                   #scalar{key = "repeated_sfixed64", value = 210},
                   #scalar{key = "repeated_sfixed64", value = 310},
                   #scalar{key = "repeated_float", value = 211},
                   #scalar{key = "repeated_float", value = 311},
                   #scalar{key = "repeated_double", value = 212},
                   #scalar{key = "repeated_double", value = 312},
                   #scalar{key = "repeated_bool", value = true},
                   #scalar{key = "repeated_bool", value = false},
                   #scalar{key = "repeated_string", value = <<"215">>},
                   #scalar{key = "repeated_string", value = <<"315">>},
                   #scalar{key = "repeated_bytes", value = <<"216">>},
                   #scalar{key = "repeated_bytes", value = <<"316">>},
                   #message{name = "RepeatedGroup",
                            fields = [#scalar{key = "a", value = 217}]},
                   #message{name = "RepeatedGroup",
                            fields = [#scalar{key = "a", value = 317}]},
                   #message{name = "repeated_nested_message",
                            fields = [#scalar{key = "bb", value = 218}]},
                   #message{name = "repeated_nested_message",
                            fields = [#scalar{key = "bb", value = 318}]},
                   #message{name = "repeated_foreign_message",
                            fields = [#scalar{key = "c", value = 219}]},
                   #message{name = "repeated_foreign_message",
                            fields = [#scalar{key = "c", value = 319}]},
                   #message{name = "repeated_import_message",
                            fields = [#scalar{key = "d", value = 220}]},
                   #message{name = "repeated_import_message",
                            fields = [#scalar{key = "d", value = 320}]},
                   #scalar{key = "repeated_nested_enum", value = 'BAR'},
                   #scalar{key = "repeated_nested_enum", value = 'BAZ'},
                   #scalar{key = "repeated_foreign_enum", value = 'FOREIGN_BAR'},
                   #scalar{key = "repeated_foreign_enum", value = 'FOREIGN_BAZ'},
                   #scalar{key = "repeated_import_enum", value = 'IMPORT_BAR'},
                   #scalar{key = "repeated_import_enum", value = 'IMPORT_BAZ'},
                   #scalar{key = "repeated_string_piece", value = <<"224">>},
                   #scalar{key = "repeated_string_piece", value = <<"324">>},
                   #scalar{key = "repeated_cord", value = <<"225">>},
                   #scalar{key = "repeated_cord", value = <<"325">>},
                   #message{name = "repeated_lazy_message",
                            fields = [#scalar{key = "bb", value = 227}]},
                   #message{name = "repeated_lazy_message",
                            fields = [#scalar{key = "bb", value = 327}]},
                   #scalar{key = "default_int32", value = 401},
                   #scalar{key = "default_int64", value = 402},
                   #scalar{key = "default_uint32", value = 403},
                   #scalar{key = "default_uint64", value = 404},
                   #scalar{key = "default_sint32", value = 405},
                   #scalar{key = "default_sint64", value = 406},
                   #scalar{key = "default_fixed32", value = 407},
                   #scalar{key = "default_fixed64", value = 408},
                   #scalar{key = "default_sfixed32", value = 409},
                   #scalar{key = "default_sfixed64", value = 410},
                   #scalar{key = "default_float", value = 411},
                   #scalar{key = "default_double", value = 412},
                   #scalar{key = "default_bool", value = false},
                   #scalar{key = "default_string", value = <<"415">>},
                   #scalar{key = "default_bytes", value = <<"416">>},
                   #scalar{key = "default_nested_enum", value = 'FOO'},
                   #scalar{key = "default_foreign_enum", value = 'FOREIGN_FOO'},
                   #scalar{key = "default_import_enum", value = 'IMPORT_FOO'},
                   #scalar{key = "default_string_piece", value = <<"424">>},
                   #scalar{key = "default_cord", value = <<"425">>},
                   #scalar{key = "oneof_uint32", value = 601},
                   #message{name = "oneof_nested_message",
                            fields = [#scalar{key = "bb", value = 602}]},
                   #scalar{key = "oneof_string", value = <<"603">>},
                   #scalar{key = "oneof_bytes", value = <<"604">>}],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).

text_format_unittest_data_test() ->
    Symbols = get_symbols_from_file("text_format_unittest_data.textproto"),
    ExpectedMsg = [#scalar{key = "optional_int32", value = 101},
                   #scalar{key = "optional_int64", value = 102},
                   #scalar{key = "optional_uint32", value = 103},
                   #scalar{key = "optional_uint64", value = 104},
                   #scalar{key = "optional_sint32", value = 105},
                   #scalar{key = "optional_sint64", value = 106},
                   #scalar{key = "optional_fixed32", value = 107},
                   #scalar{key = "optional_fixed64", value = 108},
                   #scalar{key = "optional_sfixed32", value = 109},
                   #scalar{key = "optional_sfixed64", value = 110},
                   #scalar{key = "optional_float", value = 111},
                   #scalar{key = "optional_double", value = 112},
                   #scalar{key = "optional_bool", value = true},
                   #scalar{key = "optional_string", value = <<"115">>},
                   #scalar{key = "optional_bytes", value = <<"116">>},
                   #message{name = "OptionalGroup",
                            fields = [#scalar{key = "a", value = 117}]},
                   #message{name = "optional_nested_message",
                            fields = [#scalar{key = "bb", value = 118}]},
                   #message{name = "optional_foreign_message",
                            fields = [#scalar{key = "c", value = 119}]},
                   #message{name = "optional_import_message",
                            fields = [#scalar{key = "d", value = 120}]},
                   #scalar{key = "optional_nested_enum", value = 'BAZ'},
                   #scalar{key = "optional_foreign_enum", value = 'FOREIGN_BAZ'},
                   #scalar{key = "optional_import_enum", value = 'IMPORT_BAZ'},
                   #scalar{key = "optional_string_piece", value = <<"124">>},
                   #scalar{key = "optional_cord", value = <<"125">>},
                   #message{name = "optional_public_import_message",
                            fields = [#scalar{key = "e", value = 126}]},
                   #message{name = "optional_lazy_message",
                            fields = [#scalar{key = "bb", value = 127}]},
                   #scalar{key = "repeated_int32", value = 201},
                   #scalar{key = "repeated_int32", value = 301},
                   #scalar{key = "repeated_int64", value = 202},
                   #scalar{key = "repeated_int64", value = 302},
                   #scalar{key = "repeated_uint32", value = 203},
                   #scalar{key = "repeated_uint32", value = 303},
                   #scalar{key = "repeated_uint64", value = 204},
                   #scalar{key = "repeated_uint64", value = 304},
                   #scalar{key = "repeated_sint32", value = 205},
                   #scalar{key = "repeated_sint32", value = 305},
                   #scalar{key = "repeated_sint64", value = 206},
                   #scalar{key = "repeated_sint64", value = 306},
                   #scalar{key = "repeated_fixed32", value = 207},
                   #scalar{key = "repeated_fixed32", value = 307},
                   #scalar{key = "repeated_fixed64", value = 208},
                   #scalar{key = "repeated_fixed64", value = 308},
                   #scalar{key = "repeated_sfixed32", value = 209},
                   #scalar{key = "repeated_sfixed32", value = 309},
                   #scalar{key = "repeated_sfixed64", value = 210},
                   #scalar{key = "repeated_sfixed64", value = 310},
                   #scalar{key = "repeated_float", value = 211},
                   #scalar{key = "repeated_float", value = 311},
                   #scalar{key = "repeated_double", value = 212},
                   #scalar{key = "repeated_double", value = 312},
                   #scalar{key = "repeated_bool", value = true},
                   #scalar{key = "repeated_bool", value = false},
                   #scalar{key = "repeated_string", value = <<"215">>},
                   #scalar{key = "repeated_string", value = <<"315">>},
                   #scalar{key = "repeated_bytes", value = <<"216">>},
                   #scalar{key = "repeated_bytes", value = <<"316">>},
                   #message{name = "RepeatedGroup",
                            fields = [#scalar{key = "a", value = 217}]},
                   #message{name = "RepeatedGroup",
                            fields = [#scalar{key = "a", value = 317}]},
                   #message{name = "repeated_nested_message",
                            fields = [#scalar{key = "bb", value = 218}]},
                   #message{name = "repeated_nested_message",
                            fields = [#scalar{key = "bb", value = 318}]},
                   #message{name = "repeated_foreign_message",
                            fields = [#scalar{key = "c", value = 219}]},
                   #message{name = "repeated_foreign_message",
                            fields = [#scalar{key = "c", value = 319}]},
                   #message{name = "repeated_import_message",
                            fields = [#scalar{key = "d", value = 220}]},
                   #message{name = "repeated_import_message",
                            fields = [#scalar{key = "d", value = 320}]},
                   #scalar{key = "repeated_nested_enum", value = 'BAR'},
                   #scalar{key = "repeated_nested_enum", value = 'BAZ'},
                   #scalar{key = "repeated_foreign_enum", value = 'FOREIGN_BAR'},
                   #scalar{key = "repeated_foreign_enum", value = 'FOREIGN_BAZ'},
                   #scalar{key = "repeated_import_enum", value = 'IMPORT_BAR'},
                   #scalar{key = "repeated_import_enum", value = 'IMPORT_BAZ'},
                   #scalar{key = "repeated_string_piece", value = <<"224">>},
                   #scalar{key = "repeated_string_piece", value = <<"324">>},
                   #scalar{key = "repeated_cord", value = <<"225">>},
                   #scalar{key = "repeated_cord", value = <<"325">>},
                   #message{name = "repeated_lazy_message",
                            fields = [#scalar{key = "bb", value = 227}]},
                   #message{name = "repeated_lazy_message",
                            fields = [#scalar{key = "bb", value = 327}]},
                   #scalar{key = "default_int32", value = 401},
                   #scalar{key = "default_int64", value = 402},
                   #scalar{key = "default_uint32", value = 403},
                   #scalar{key = "default_uint64", value = 404},
                   #scalar{key = "default_sint32", value = 405},
                   #scalar{key = "default_sint64", value = 406},
                   #scalar{key = "default_fixed32", value = 407},
                   #scalar{key = "default_fixed64", value = 408},
                   #scalar{key = "default_sfixed32", value = 409},
                   #scalar{key = "default_sfixed64", value = 410},
                   #scalar{key = "default_float", value = 411},
                   #scalar{key = "default_double", value = 412},
                   #scalar{key = "default_bool", value = false},
                   #scalar{key = "default_string", value = <<"415">>},
                   #scalar{key = "default_bytes", value = <<"416">>},
                   #scalar{key = "default_nested_enum", value = 'FOO'},
                   #scalar{key = "default_foreign_enum", value = 'FOREIGN_FOO'},
                   #scalar{key = "default_import_enum", value = 'IMPORT_FOO'},
                   #scalar{key = "default_string_piece", value = <<"424">>},
                   #scalar{key = "default_cord", value = <<"425">>},
                   #scalar{key = "oneof_uint32", value = 601},
                   #message{name = "oneof_nested_message",
                            fields = [#scalar{key = "bb", value = 602}]},
                   #scalar{key = "oneof_string", value = <<"603">>},
                   #scalar{key = "oneof_bytes", value = <<"604">>}],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).

text_format_unittest_extensions_data_pointy_test() ->
    Symbols = get_symbols_from_file("text_format_unittest_extensions_data_pointy.textproto"),
    ExpectedMsg = [#scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_int32_extension]",
                               parts = [protobuf_unittest,
                                        optional_int32_extension]},
                      value = 101},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_int64_extension]",
                               parts = [protobuf_unittest,
                                        optional_int64_extension]},
                      value = 102},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_uint32_extension]",
                               parts = [protobuf_unittest,
                                        optional_uint32_extension]},
                      value = 103},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_uint64_extension]",
                               parts = [protobuf_unittest,
                                        optional_uint64_extension]},
                      value = 104},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_sint32_extension]",
                               parts = [protobuf_unittest,
                                        optional_sint32_extension]},
                      value = 105},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_sint64_extension]",
                               parts = [protobuf_unittest,
                                        optional_sint64_extension]},
                      value = 106},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_fixed32_extension]",
                               parts = [protobuf_unittest,
                                        optional_fixed32_extension]},
                      value = 107},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_fixed64_extension]",
                               parts = [protobuf_unittest,
                                        optional_fixed64_extension]},
                      value = 108},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_sfixed32_extension]",
                               parts = [protobuf_unittest,
                                        optional_sfixed32_extension]},
                      value = 109},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_sfixed64_extension]",
                               parts = [protobuf_unittest,
                                        optional_sfixed64_extension]},
                      value = 110},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_float_extension]",
                               parts = [protobuf_unittest,
                                        optional_float_extension]},
                      value = 111},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_double_extension]",
                               parts = [protobuf_unittest,
                                        optional_double_extension]},
                      value = 112},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_bool_extension]",
                               parts = [protobuf_unittest,
                                        optional_bool_extension]},
                      value = true},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_string_extension]",
                               parts = [protobuf_unittest,
                                        optional_string_extension]},
                      value = <<"115">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_bytes_extension]",
                               parts = [protobuf_unittest,
                                        optional_bytes_extension]},
                      value = <<"116">>},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optionalgroup_extension]",
                                parts = [protobuf_unittest,
                                         optionalgroup_extension]},
                      fields = [#scalar{key = "a", value = 117}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_nested_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_nested_message_extension]},
                      fields = [#scalar{key = "bb", value = 118}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_foreign_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_foreign_message_extension]},
                      fields = [#scalar{key = "c", value = 119}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_import_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_import_message_extension]},
                      fields = [#scalar{key = "d", value = 120}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_nested_enum_extension]",
                               parts = [protobuf_unittest,
                                        optional_nested_enum_extension]},
                      value = 'BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_foreign_enum_extension]",
                               parts = [protobuf_unittest,
                                        optional_foreign_enum_extension]},
                      value = 'FOREIGN_BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_import_enum_extension]",
                               parts = [protobuf_unittest,
                                        optional_import_enum_extension]},
                      value = 'IMPORT_BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_string_piece_extension]",
                               parts = [protobuf_unittest,
                                        optional_string_piece_extension]},
                      value = <<"124">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_cord_extension]",
                               parts = [protobuf_unittest,
                                        optional_cord_extension]},
                      value = <<"125">>},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_public_import_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_public_import_message_extension]},
                      fields = [#scalar{key = "e", value = 126}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_lazy_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_lazy_message_extension]},
                      fields = [#scalar{key = "bb", value = 127}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_unverified_lazy_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_unverified_lazy_message_extension]},
                      fields = [#scalar{key = "bb", value = 128}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_int32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_int32_extension]},
                      value = 201},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_int32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_int32_extension]},
                      value = 301},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_int64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_int64_extension]},
                      value = 202},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_int64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_int64_extension]},
                      value = 302},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_uint32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_uint32_extension]},
                      value = 203},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_uint32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_uint32_extension]},
                      value = 303},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_uint64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_uint64_extension]},
                      value = 204},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_uint64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_uint64_extension]},
                      value = 304},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sint32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_sint32_extension]},
                      value = 205},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sint32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_sint32_extension]},
                      value = 305},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sint64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_sint64_extension]},
                      value = 206},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sint64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_sint64_extension]},
                      value = 306},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_fixed32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_fixed32_extension]},
                      value = 207},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_fixed32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_fixed32_extension]},
                      value = 307},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_fixed64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_fixed64_extension]},
                      value = 208},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_fixed64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_fixed64_extension]},
                      value = 308},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sfixed32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_sfixed32_extension]},
                      value = 209},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sfixed32_extension]",
                               parts = [protobuf_unittest,
                                        repeated_sfixed32_extension]},
                      value = 309},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sfixed64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_sfixed64_extension]},
                      value = 210},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sfixed64_extension]",
                               parts = [protobuf_unittest,
                                        repeated_sfixed64_extension]},
                      value = 310},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_float_extension]",
                               parts = [protobuf_unittest,
                                        repeated_float_extension]},
                      value = 211},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_float_extension]",
                               parts = [protobuf_unittest,
                                        repeated_float_extension]},
                      value = 311},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_double_extension]",
                               parts = [protobuf_unittest,
                                        repeated_double_extension]},
                      value = 212},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_double_extension]",
                               parts = [protobuf_unittest,
                                        repeated_double_extension]},
                      value = 312},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_bool_extension]",
                               parts = [protobuf_unittest,repeated_bool_extension]},
                      value = true},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_bool_extension]",
                               parts = [protobuf_unittest,
                                        repeated_bool_extension]},
                      value = false},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_string_extension]",
                               parts = [protobuf_unittest,
                                        repeated_string_extension]},
                      value = <<"215">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_string_extension]",
                               parts = [protobuf_unittest,
                                        repeated_string_extension]},
                      value = <<"315">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_bytes_extension]",
                               parts = [protobuf_unittest,
                                        repeated_bytes_extension]},
                      value = <<"216">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_bytes_extension]",
                               parts = [protobuf_unittest,
                                        repeated_bytes_extension]},
                      value = <<"316">>},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeatedgroup_extension]",
                                parts = [protobuf_unittest,
                                         repeatedgroup_extension]},
                      fields = [#scalar{key = "a", value = 217}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeatedgroup_extension]",
                                parts = [protobuf_unittest,
                                         repeatedgroup_extension]},
                      fields = [#scalar{key = "a", value = 317}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_nested_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_nested_message_extension]},
                      fields = [#scalar{key = "bb", value = 218}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_nested_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_nested_message_extension]},
                      fields = [#scalar{key = "bb", value = 318}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_foreign_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_foreign_message_extension]},
                      fields = [#scalar{key = "c", value = 219}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_foreign_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_foreign_message_extension]},
                      fields = [#scalar{key = "c", value = 319}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_import_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_import_message_extension]},
                      fields = [#scalar{key = "d", value = 220}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_import_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_import_message_extension]},
                      fields = [#scalar{key = "d", value = 320}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_nested_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_nested_enum_extension]},
                      value = 'BAR'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_nested_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_nested_enum_extension]},
                      value = 'BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_foreign_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_foreign_enum_extension]},
                      value = 'FOREIGN_BAR'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_foreign_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_foreign_enum_extension]},
                      value = 'FOREIGN_BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_import_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_import_enum_extension]},
                      value = 'IMPORT_BAR'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_import_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_import_enum_extension]},
                      value = 'IMPORT_BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_string_piece_extension]",
                               parts = [protobuf_unittest,
                                        repeated_string_piece_extension]},
                      value = <<"224">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_string_piece_extension]",
                               parts = [protobuf_unittest,
                                        repeated_string_piece_extension]},
                      value = <<"324">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_cord_extension]",
                               parts = [protobuf_unittest,
                                        repeated_cord_extension]},
                      value = <<"225">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_cord_extension]",
                               parts = [protobuf_unittest,
                                        repeated_cord_extension]},
                      value = <<"325">>},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_lazy_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_lazy_message_extension]},
                      fields = [#scalar{key = "bb", value = 227}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_lazy_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_lazy_message_extension]},
                      fields = [#scalar{key = "bb", value = 327}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_int32_extension]",
                               parts = [protobuf_unittest,
                                        default_int32_extension]},
                      value = 401},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_int64_extension]",
                               parts = [protobuf_unittest,
                                        default_int64_extension]},
                      value = 402},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_uint32_extension]",
                               parts = [protobuf_unittest,
                                        default_uint32_extension]},
                      value = 403},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_uint64_extension]",
                               parts = [protobuf_unittest,
                                        default_uint64_extension]},
                      value = 404},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_sint32_extension]",
                               parts = [protobuf_unittest,
                                        default_sint32_extension]},
                      value = 405},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_sint64_extension]",
                               parts = [protobuf_unittest,
                                        default_sint64_extension]},
                      value = 406},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_fixed32_extension]",
                               parts = [protobuf_unittest,
                                        default_fixed32_extension]},
                      value = 407},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_fixed64_extension]",
                               parts = [protobuf_unittest,
                                        default_fixed64_extension]},
                      value = 408},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_sfixed32_extension]",
                               parts = [protobuf_unittest,
                                        default_sfixed32_extension]},
                      value = 409},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_sfixed64_extension]",
                               parts = [protobuf_unittest,
                                        default_sfixed64_extension]},
                      value = 410},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_float_extension]",
                               parts = [protobuf_unittest,
                                        default_float_extension]},
                      value = 411},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_double_extension]",
                               parts = [protobuf_unittest,
                                        default_double_extension]},
                      value = 412},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_bool_extension]",
                               parts = [protobuf_unittest,
                                        default_bool_extension]},
                      value = false},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_string_extension]",
                               parts = [protobuf_unittest,
                                        default_string_extension]},
                      value = <<"415">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_bytes_extension]",
                               parts = [protobuf_unittest,
                                        default_bytes_extension]},
                      value = <<"416">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_nested_enum_extension]",
                               parts = [protobuf_unittest,
                                        default_nested_enum_extension]},
                      value = 'FOO'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_foreign_enum_extension]",
                               parts = [protobuf_unittest,
                                        default_foreign_enum_extension]},
                      value = 'FOREIGN_FOO'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_import_enum_extension]",
                               parts = [protobuf_unittest,
                                        default_import_enum_extension]},
                      value = 'IMPORT_FOO'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_string_piece_extension]",
                               parts = [protobuf_unittest,
                                        default_string_piece_extension]},
                      value = <<"424">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_cord_extension]",
                               parts = [protobuf_unittest,
                                        default_cord_extension]},
                      value = <<"425">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.oneof_uint32_extension]",
                               parts = [protobuf_unittest,
                                        oneof_uint32_extension]},
                      value = 601},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.oneof_nested_message_extension]",
                                parts = [protobuf_unittest,
                                         oneof_nested_message_extension]},
                      fields = [#scalar{key = "bb", value = 602}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.oneof_string_extension]",
                               parts = [protobuf_unittest,
                                        oneof_string_extension]},
                      value = <<"603">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.oneof_bytes_extension]",
                               parts = [protobuf_unittest,
                                        oneof_bytes_extension]},
                      value = <<"604">>}],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).

text_format_unittest_extensions_data_test() ->
    Symbols = get_symbols_from_file("text_format_unittest_extensions_data.textproto"),
    ExpectedMsg = [#scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_int32_extension]",
                               parts = [protobuf_unittest,optional_int32_extension]},
                      value = 101},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_int64_extension]",
                               parts = [protobuf_unittest,optional_int64_extension]},
                      value = 102},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_uint32_extension]",
                               parts = [protobuf_unittest,optional_uint32_extension]},
                      value = 103},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_uint64_extension]",
                               parts = [protobuf_unittest,optional_uint64_extension]},
                      value = 104},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_sint32_extension]",
                               parts = [protobuf_unittest,optional_sint32_extension]},
                      value = 105},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_sint64_extension]",
                               parts = [protobuf_unittest,optional_sint64_extension]},
                      value = 106},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_fixed32_extension]",
                               parts = [protobuf_unittest,optional_fixed32_extension]},
                      value = 107},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_fixed64_extension]",
                               parts = [protobuf_unittest,optional_fixed64_extension]},
                      value = 108},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_sfixed32_extension]",
                               parts = [protobuf_unittest,optional_sfixed32_extension]},
                      value = 109},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_sfixed64_extension]",
                               parts = [protobuf_unittest,optional_sfixed64_extension]},
                      value = 110},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_float_extension]",
                               parts = [protobuf_unittest,optional_float_extension]},
                      value = 111},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_double_extension]",
                               parts = [protobuf_unittest,optional_double_extension]},
                      value = 112},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_bool_extension]",
                               parts = [protobuf_unittest,optional_bool_extension]},
                      value = true},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_string_extension]",
                               parts = [protobuf_unittest,optional_string_extension]},
                      value = <<"115">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_bytes_extension]",
                               parts = [protobuf_unittest,optional_bytes_extension]},
                      value = <<"116">>},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optionalgroup_extension]",
                                parts = [protobuf_unittest,optionalgroup_extension]},
                      fields = [#scalar{key = "a", value =117}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_nested_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_nested_message_extension]},
                      fields = [#scalar{key = "bb", value = 118}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_foreign_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_foreign_message_extension]},
                      fields = [#scalar{key = "c", value = 119}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_import_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_import_message_extension]},
                      fields = [#scalar{key = "d", value = 120}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_nested_enum_extension]",
                               parts = [protobuf_unittest,
                                        optional_nested_enum_extension]},
                      value = 'BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_foreign_enum_extension]",
                               parts = [protobuf_unittest,
                                        optional_foreign_enum_extension]},
                      value = 'FOREIGN_BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_import_enum_extension]",
                               parts = [protobuf_unittest,
                                        optional_import_enum_extension]},
                      value = 'IMPORT_BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_string_piece_extension]",
                               parts = [protobuf_unittest,
                                        optional_string_piece_extension]},
                      value = <<"124">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.optional_cord_extension]",
                               parts = [protobuf_unittest,optional_cord_extension]},
                      value = <<"125">>},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_public_import_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_public_import_message_extension]},
                      fields = [#scalar{key = "e", value = 126}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_lazy_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_lazy_message_extension]},
                      fields = [#scalar{key = "bb", value = 127}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.optional_unverified_lazy_message_extension]",
                                parts = [protobuf_unittest,
                                         optional_unverified_lazy_message_extension]},
                      fields = [#scalar{key = "bb", value = 128}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_int32_extension]",
                               parts = [protobuf_unittest,repeated_int32_extension]},
                      value = 201},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_int32_extension]",
                               parts = [protobuf_unittest,repeated_int32_extension]},
                      value = 301},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_int64_extension]",
                               parts = [protobuf_unittest,repeated_int64_extension]},
                      value = 202},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_int64_extension]",
                               parts = [protobuf_unittest,repeated_int64_extension]},
                      value = 302},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_uint32_extension]",
                               parts = [protobuf_unittest,repeated_uint32_extension]},
                      value = 203},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_uint32_extension]",
                               parts = [protobuf_unittest,repeated_uint32_extension]},
                      value = 303},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_uint64_extension]",
                               parts = [protobuf_unittest,repeated_uint64_extension]},
                      value = 204},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_uint64_extension]",
                               parts = [protobuf_unittest,repeated_uint64_extension]},
                      value = 304},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sint32_extension]",
                               parts = [protobuf_unittest,repeated_sint32_extension]},
                      value = 205},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sint32_extension]",
                               parts = [protobuf_unittest,repeated_sint32_extension]},
                      value = 305},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sint64_extension]",
                               parts = [protobuf_unittest,repeated_sint64_extension]},
                      value = 206},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sint64_extension]",
                               parts = [protobuf_unittest,repeated_sint64_extension]},
                      value = 306},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_fixed32_extension]",
                               parts = [protobuf_unittest,repeated_fixed32_extension]},
                      value = 207},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_fixed32_extension]",
                               parts = [protobuf_unittest,repeated_fixed32_extension]},
                      value = 307},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_fixed64_extension]",
                               parts = [protobuf_unittest,repeated_fixed64_extension]},
                      value = 208},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_fixed64_extension]",
                               parts = [protobuf_unittest,repeated_fixed64_extension]},
                      value = 308},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sfixed32_extension]",
                               parts = [protobuf_unittest,repeated_sfixed32_extension]},
                      value = 209},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sfixed32_extension]",
                               parts = [protobuf_unittest,repeated_sfixed32_extension]},
                      value = 309},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sfixed64_extension]",
                               parts = [protobuf_unittest,repeated_sfixed64_extension]},
                      value = 210},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_sfixed64_extension]",
                               parts = [protobuf_unittest,repeated_sfixed64_extension]},
                      value = 310},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_float_extension]",
                               parts = [protobuf_unittest,repeated_float_extension]},
                      value = 211},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_float_extension]",
                               parts = [protobuf_unittest,repeated_float_extension]},
                      value = 311},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_double_extension]",
                               parts = [protobuf_unittest,repeated_double_extension]},
                      value = 212},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_double_extension]",
                               parts = [protobuf_unittest,repeated_double_extension]},
                      value = 312},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_bool_extension]",
                               parts = [protobuf_unittest,repeated_bool_extension]},
                      value = true},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_bool_extension]",
                               parts = [protobuf_unittest,repeated_bool_extension]},
                      value = false},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_string_extension]",
                               parts = [protobuf_unittest,repeated_string_extension]},
                      value = <<"215">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_string_extension]",
                               parts = [protobuf_unittest,repeated_string_extension]},
                      value = <<"315">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_bytes_extension]",
                               parts = [protobuf_unittest,repeated_bytes_extension]},
                      value = <<"216">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_bytes_extension]",
                               parts = [protobuf_unittest,repeated_bytes_extension]},
                      value = <<"316">>},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeatedgroup_extension]",
                                parts = [protobuf_unittest,repeatedgroup_extension]},
                      fields = [#scalar{key = "a", value = 217}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeatedgroup_extension]",
                                parts = [protobuf_unittest,repeatedgroup_extension]},
                      fields = [#scalar{key = "a", value = 317}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_nested_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_nested_message_extension]},
                      fields = [#scalar{key = "bb", value = 218}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_nested_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_nested_message_extension]},
                      fields = [#scalar{key = "bb", value = 318}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_foreign_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_foreign_message_extension]},
                      fields = [#scalar{key = "c", value = 219}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_foreign_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_foreign_message_extension]},
                      fields = [#scalar{key = "c", value = 319}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_import_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_import_message_extension]},
                      fields = [#scalar{key = "d", value = 220}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_import_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_import_message_extension]},
                      fields = [#scalar{key = "d", value = 320}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_nested_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_nested_enum_extension]},
                      value = 'BAR'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_nested_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_nested_enum_extension]},
                      value = 'BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_foreign_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_foreign_enum_extension]},
                      value = 'FOREIGN_BAR'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_foreign_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_foreign_enum_extension]},
                      value = 'FOREIGN_BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_import_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_import_enum_extension]},
                      value = 'IMPORT_BAR'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_import_enum_extension]",
                               parts = [protobuf_unittest,
                                        repeated_import_enum_extension]},
                      value = 'IMPORT_BAZ'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_string_piece_extension]",
                               parts = [protobuf_unittest,
                                        repeated_string_piece_extension]},
                      value = <<"224">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_string_piece_extension]",
                               parts = [protobuf_unittest,
                                        repeated_string_piece_extension]},
                      value = <<"324">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_cord_extension]",
                               parts = [protobuf_unittest,repeated_cord_extension]},
                      value = <<"225">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.repeated_cord_extension]",
                               parts = [protobuf_unittest,repeated_cord_extension]},
                      value = <<"325">>},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_lazy_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_lazy_message_extension]},
                      fields = [#scalar{key = "bb", value = 227}]},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.repeated_lazy_message_extension]",
                                parts = [protobuf_unittest,
                                         repeated_lazy_message_extension]},
                      fields = [#scalar{key = "bb", value = 327}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_int32_extension]",
                               parts = [protobuf_unittest,default_int32_extension]},
                      value = 401},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_int64_extension]",
                               parts = [protobuf_unittest,default_int64_extension]},
                      value = 402},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_uint32_extension]",
                               parts = [protobuf_unittest,default_uint32_extension]},
                      value = 403},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_uint64_extension]",
                               parts = [protobuf_unittest,default_uint64_extension]},
                      value = 404},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_sint32_extension]",
                               parts = [protobuf_unittest,default_sint32_extension]},
                      value = 405},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_sint64_extension]",
                               parts = [protobuf_unittest,default_sint64_extension]},
                      value = 406},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_fixed32_extension]",
                               parts = [protobuf_unittest,default_fixed32_extension]},
                      value = 407},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_fixed64_extension]",
                               parts = [protobuf_unittest,default_fixed64_extension]},
                      value = 408},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_sfixed32_extension]",
                               parts = [protobuf_unittest,default_sfixed32_extension]},
                      value = 409},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_sfixed64_extension]",
                               parts = [protobuf_unittest,default_sfixed64_extension]},
                      value = 410},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_float_extension]",
                               parts = [protobuf_unittest,default_float_extension]},
                      value = 411},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_double_extension]",
                               parts = [protobuf_unittest,default_double_extension]},
                      value = 412},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_bool_extension]",
                               parts = [protobuf_unittest,default_bool_extension]},
                      value = false},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_string_extension]",
                               parts = [protobuf_unittest,default_string_extension]},
                      value = <<"415">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_bytes_extension]",
                               parts = [protobuf_unittest,default_bytes_extension]},
                      value = <<"416">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_nested_enum_extension]",
                               parts = [protobuf_unittest,
                                        default_nested_enum_extension]},
                      value = 'FOO'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_foreign_enum_extension]",
                               parts = [protobuf_unittest,
                                        default_foreign_enum_extension]},
                      value = 'FOREIGN_FOO'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_import_enum_extension]",
                               parts = [protobuf_unittest,
                                        default_import_enum_extension]},
                      value = 'IMPORT_FOO'},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_string_piece_extension]",
                               parts = [protobuf_unittest,
                                        default_string_piece_extension]},
                      value = <<"424">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.default_cord_extension]",
                               parts = [protobuf_unittest,default_cord_extension]},
                      value = <<"425">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.oneof_uint32_extension]",
                               parts = [protobuf_unittest,oneof_uint32_extension]},
                      value = 601},
                   #message{
                      name = #extension{
                                name = "[protobuf_unittest.oneof_nested_message_extension]",
                                parts = [protobuf_unittest,
                                         oneof_nested_message_extension]},
                      fields = [#scalar{key = "bb", value = 602}]},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.oneof_string_extension]",
                               parts = [protobuf_unittest,oneof_string_extension]},
                      value = <<"603">>},
                   #scalar{
                      key = #extension{
                               name = "[protobuf_unittest.oneof_bytes_extension]",
                               parts = [protobuf_unittest,oneof_bytes_extension]},
                      value = <<"604">>}],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).

%% Helper functions

get_symbols_from_file(TextProtoFile) ->
    TestDir = code:lib_dir(gpb_text, test),
    File = filename:join([TestDir, "data", TextProtoFile]),
    {ok, Bin} = file:read_file(File),
    {ok, Symbols, _} = gpb_text_lexer:string(binary_to_list(Bin)),
    Symbols.
