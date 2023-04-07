-module(gpb_proto_tests).

-include_lib("eunit/include/eunit.hrl").

-export([message_with_map/1,
         one_of_example/1,
         one_of_example_invalid/1,
         message_with_group/1
        ]).

test_definitions() ->
    [{message_with_map, "message_with_map.proto", "message_with_map.textproto"},
     {one_of_example, "one_of_example.proto", "one_of_example.textproto"},
     {one_of_example_invalid, "one_of_example.proto", "one_of_example_invalid.textproto"},
     {message_with_map, {"message_with_map.proto", [maps]}, "message_with_map.textproto"},
     {one_of_example, {"one_of_example.proto", [maps]}, "one_of_example.textproto"},
     {one_of_example_invalid, {"one_of_example.proto", [maps]}, "one_of_example_invalid.textproto"}
    ].

proto_files_test_() ->
    TestFun = fun (T, F) ->
                      ProtoFile = gpb_text_test_utils:get_full_file_name(T),
                      ?MODULE:F(ProtoFile)
              end,
    {foreachx,
     fun setup/1,
     fun cleanup/2,
     [{D, fun (_, _) -> TestFun(T, F) end}
      || {F, _, T} = D <- test_definitions()]}.

setup({_, {ProtoFile, Opts}, _}) ->
    compile_and_load(ProtoFile, Opts);
setup({_, ProtoFile, _}) ->
    compile_and_load(ProtoFile).

cleanup({Module, _, _}, _) ->
    delete_and_purge(Module).

message_with_map(FileName) ->
    Expected = #{my_map =>
                     [#{<<"entry1">> => 1},
                      #{<<"entry2">> => 2},
                      #{<<"entry3">> => 3},
                      #{<<"entry4">> => 4}]},
    ?_assertEqual(Expected, gpb_text:file(FileName)).

one_of_example(FileName) ->
    Expected = #{message => [#{not_part_of_oneof => <<"always valid">>,
                               first_oneof_field => <<"valid by itself">>},
                             #{not_part_of_oneof => <<"always valid">>,
                               second_oneof_field => <<"valid by itself">>}]},
    ?_assertEqual(Expected, gpb_text:file(FileName)).

one_of_example_invalid(FileName) ->
    MultiOneOfs =  [#{first_oneof_field => <<"not valid">>},
                    #{second_oneof_field => <<"not valid">>}],
    Expected = {error, {multiple_oneof, MultiOneOfs}},
    ?_assertException(throw, Expected, gpb_text:file(FileName)).

message_with_group(FileName) ->
    Expected = #{'MyGroup' => #{my_value => 1}},
    ?_assertEqual(Expected, gpb_text:file(FileName)).


%% Helper functions

compile_and_load(ProtoFile) ->
    compile_and_load(ProtoFile, []).

compile_and_load(ProtoFile, Opts) ->
    ok = gpb_compile:file(gpb_text_test_utils:get_full_file_name(ProtoFile), Opts),
    IGPB = code:lib_dir(gpb, include),
    BaseName = filename:basename(ProtoFile, ".proto"),
    ErlFile = gpb_text_test_utils:get_full_file_name(BaseName ++ ".erl"),
    Module = list_to_atom(BaseName),
    code:delete(Module),
    code:purge(Module),
    {ok, _} = compile:file(ErlFile, [{i, IGPB}]),
    {module, _} = code:load_file(Module).

delete_and_purge(Module) ->
    code:delete(Module),
    code:purge(Module).

