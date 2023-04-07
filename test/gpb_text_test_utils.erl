-module(gpb_text_test_utils).

-export([get_full_file_name/1]).

get_full_file_name(TextProtoFile) ->
    TestDir = code:lib_dir(gpb_text, test),
    filename:join([TestDir, "data", TextProtoFile]).

