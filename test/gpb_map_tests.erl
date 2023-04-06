-module(gpb_map_tests).

-include_lib("eunit/include/eunit.hrl").

map_test_data_test() ->
    %% No proto definition found
    FileName = get_full_file_name("map_test_data.textproto"),
    Expected = #{"map_bool_bool" =>
                     #{"key" => true, "value" => true},
                 "map_fixed32_fixed32" =>
                     #{"key" => 1, "value" => 1},
                 "map_fixed64_fixed64" =>
                     #{"key" => 1, "value" => 1},
                 "map_int32_bytes" =>
                     #{"key" => 1, "value" => <<"1">>},
                 "map_int32_double" =>
                     #{"key" => 1, "value" => 1},
                 "map_int32_enum" =>
                     #{"key" => 1, "value" => 'MAP_ENUM_BAZ'},
                 "map_int32_float" =>
                     #{"key" => 1, "value" => 1},
                 "map_int32_foreign_message" =>
                     #{"key" => 1, "value" => #{"c" => 1}},
                 "map_int32_int32" =>
                     #{"key" => 1, "value" => 1},
                 "map_int64_int64" =>
                     #{"key" => 1, "value" => 1},
                 "map_sfixed32_sfixed32" =>
                     #{"key" => 1, "value" => 1},
                 "map_sfixed64_sfixed64" =>
                     #{"key" => 1, "value" => 1},
                 "map_sint32_sint32" =>
                     #{"key" => 1, "value" => 1},
                 "map_sint64_sint64" =>
                     #{"key" => 1, "value" => 1},
                 "map_string_string" =>
                     #{"key" => <<"1">>, "value" => <<"1">>},
                 "map_uint32_uint32" =>
                     #{"key" => 1, "value" => 1},
                 "map_uint64_uint64" =>
                     #{"key" => 1, "value" => 1}},
    ?assertEqual(Expected, gpb_text:file(FileName)).

message_with_map_test() ->
    compile_and_load("message_with_map.proto"),
    FileName = get_full_file_name("message_with_map.textproto"),
    Expected = #{my_map =>
                     [#{<<"entry1">> => 1},
                      #{<<"entry2">> => 2},
                      #{<<"entry3">> => 3},
                      #{<<"entry4">> => 4}]},
    ?assertEqual(Expected, gpb_text:file(FileName)).

one_of_example_test() ->
    compile_and_load("one_of_example.proto"),
    FileName = get_full_file_name("one_of_example.textproto"),
    Expected = #{message => [#{not_part_of_oneof => <<"always valid">>,
                               first_oneof_field => <<"valid by itself">>},
                             #{not_part_of_oneof => <<"always valid">>,
                               second_oneof_field => <<"valid by itself">>}]},
    ?assertEqual(Expected, gpb_text:file(FileName)).

one_of_example_invalid_test() ->
    compile_and_load("one_of_example.proto"),
    FileName = get_full_file_name("one_of_example_invalid.textproto"),
    Expected =  [#{first_oneof_field => <<"not valid">>},
                 #{second_oneof_field => <<"not valid">>}],
    ?assertException(throw, {error, {multiple_oneof, Expected}}, gpb_text:file(FileName)).


%% Helper functions

compile_and_load(ProtoFile) ->
    ok = gpb_compile:file(get_full_file_name(ProtoFile)),
    IGPB = code:lib_dir(gpb, include),
    BaseName = filename:basename(ProtoFile, ".proto"),
    ErlFile = BaseName ++ ".erl",
    {ok, _} = compile:file(get_full_file_name(ErlFile), [{i, IGPB}]),
    {module, _} = code:load_file(list_to_atom(BaseName)).

get_full_file_name(TextProtoFile) ->
    TestDir = code:lib_dir(gpb_text, test),
    filename:join([TestDir, "data", TextProtoFile]).
