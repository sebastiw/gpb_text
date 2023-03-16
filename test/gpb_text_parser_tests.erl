-module(gpb_text_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gpb_text/include/gpb_text.hrl").

default_test() ->
    Symbols = get_symbols_from_file("default_example.textproto"),
    ExpectedMsg = [#scalar{key = name, value = <<"John Smith">>},
                   #message{name = pet,
                            fields = [#scalar{key = kind, value = 'DOG'},
                                      #scalar{key = name, value = <<"Fluffy">>},
                                      #scalar{key = tail_wagginess, value = 0.65}]},
                   #message{name = pet,
                            fields = [#scalar{key = kind, value = 'LIZARD'},
                                      #scalar{key = name, value = <<"Lizzy">>},
                                      #scalar{key = legs, value = 4}]},
                   #scalar{key = string_value_with_escape, value = <<"valid \\n escape">>},
                   #scalar{key = repeated_values,
                           value = [<<"one">>,
                                    <<"two">>,
                                    <<"three">>]}
                  ],
    ?assertEqual({ok, ExpectedMsg}, gpb_text_parser:parse(Symbols)).


get_symbols_from_file(TextProtoFile) ->
    TestDir = code:lib_dir(gpb_text, test),
    File = filename:join([TestDir, "data", TextProtoFile]),
    {ok, Bin} = file:read_file(File),
    {ok, Symbols, _} = gpb_text_lexer:string(binary_to_list(Bin)),
    Symbols.
