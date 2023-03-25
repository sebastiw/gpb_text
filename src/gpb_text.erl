-module(gpb_text).

-export([file/1,
         scan/1,
         parse/1
        ]).

-spec file(file:filename()) -> term().
file(FileName) ->
    parse(scan(read(FileName))).

-spec read(file:filename()) -> string().
read(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    binary_to_list(Bin).

-spec scan(string()) -> [tuple()].
scan(String) ->
    {ok, Symbols, _} = gpb_text_lexer:string(String),
    Symbols.

-spec parse([tuple()]) -> term().
parse(Symbols) ->
    {ok, Message} = gpb_text_parser:parse(Symbols),
    Message.
