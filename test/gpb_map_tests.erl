-module(gpb_map_tests).
%% No proto definition tied to textproto files

-include_lib("eunit/include/eunit.hrl").

map_test_data_test() ->
    FileName = gpb_text_test_utils:get_full_file_name("map_test_data.textproto"),
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


