gpb_text
=====

Library for parsing Google Protobuf text-format, i.e. files ending
with `*.textproto`

For information on the format, see https://protobuf.dev/reference/protobuf/textformat-spec/

Build
-----

    $ rebar3 compile

Usage
-----

    1> File = "path/to/your.textproto".
    2> {ok, FileContent} = file:read_file(File).
    3> {ok, Symbols, _} = gpb_text:scan(binary_to_list(FileContent)).
    4> {ok, Message} = gpb_text:parse(Symbols).

or shortcut

    1> File = "path/to/your.textproto".
    2> Message = gpb_text:file(File).
