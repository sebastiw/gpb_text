-module(gpb_text).

-export([file/1,
         scan/1,
         parse/1,
         to_map/1,
         to_map/2,
         find_proto_filename/1,
         find_proto_message/1,
         get_proto_mod/1,
         get_proto_defs/1
        ]).

-include("gpb_text.hrl").
-include_lib("gpb/include/gpb.hrl").

-type message() :: #message{}.
-type scalar() :: #scalar{}.
-type parsed_forms() :: [message() | scalar()].
-type gpb_fields() :: [#?gpb_field{}].

-type option() :: gpb_compile:renames().

-spec file(file:filename()) -> map().
file(FileName) ->
    file(FileName, []).

file(FileName, Opts) ->
    FileContent = read(FileName),
    Parsed = parse(scan(FileContent)),
    case can_post_process(FileContent) of
        {true, {ProtoMod, StartMsg}} ->
            Renamed = rename_fields(Parsed, ProtoMod),
            BaseMsg = base_msg_type(ProtoMod, StartMsg),
            post_process_message(Renamed, ProtoMod, BaseMsg);
        false ->
            to_map(Parsed, Opts)
    end.

-spec read(file:filename()) -> string().
read(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    binary_to_list(Bin).

-spec scan(string()) -> [tuple()].
scan(String) ->
    {ok, Symbols, _} = gpb_text_lexer:string(String),
    Symbols.

-spec parse([tuple()]) -> parsed_forms().
parse(Symbols) ->
    {ok, Message} = gpb_text_parser:parse(Symbols),
    Message.

rename_fields(Message, _Opts) ->
    %% Need to know how the proto names was renamed.
    %% i.e. get options from compiled proto file.

    %% Renames = proplists:get_value(renames, Opts, []),
    Message.

-spec can_post_process(string()) -> {true, {module(), string()}} | false.
can_post_process(FileContent) ->
    case find_proto_filename(FileContent) of
        {ok, ProtoFile} ->
            case get_proto_mod(ProtoFile) of
                {ok, ProtoMod} ->
                    case find_proto_message(FileContent) of
                        {ok, StartMsg} ->
                            {true, {ProtoMod, StartMsg}};
                        not_found ->
                            false
                    end;
                {error, {not_loaded, _}} ->
                    false
            end;
        not_found ->
            false
    end.

-spec post_process_message(parsed_forms(), module(), gpb_fields()) -> map().
post_process_message(Fs, ProtoMod, BaseMsgDefs) ->
    process_fields(ProtoMod, BaseMsgDefs, Fs, #{}).

base_msg_type(ProtoMod, MessageName) ->
    case ProtoMod:get_package_name() of
        undefined ->
            MsgName = list_to_atom(MessageName),
            ProtoMod:fetch_msg_def(MsgName);
        Pkt ->
            MsgName = list_to_atom(atom_to_list(Pkt) ++ "." ++ MessageName),
            ProtoMod:fetch_msg_def(MsgName)
    end.

find_def(FieldName, Defs) ->
    FName = list_to_atom(FieldName),
    case lists:keyfind(FName, #field.name, Defs) of
        false ->
            OneOfFields = [F || #gpb_oneof{fields = Fs} <- Defs,
                                F <- Fs],
            case lists:keyfind(FName, #field.name, OneOfFields) of
                false ->
                    throw({error, {no_definition, FieldName, Defs}});
                F ->
                    {oneof, F, OneOfFields}
            end;
        Field ->
            Field
    end.


process_fields(_ProtoMod, _Fields, [], Acc) ->
    Acc;
process_fields(ProtoMod, Fields, [#scalar{key = K, value = V}|Fs], Acc) ->
    Field = find_def(K, Fields),
    Acc2 = add_to_acc(ProtoMod, Field, V, Acc),
    process_fields(ProtoMod, Fields, Fs, Acc2);
process_fields(ProtoMod, Fields, [#message{name = K, fields = Vs}|Fs], Acc) ->
    Field = find_def(K, Fields),
    case Field#field.occurrence of
        repeated ->
            Vs2 = case is_list(hd(Vs)) of
                      true ->
                          [add_to_acc(ProtoMod, Field, V, #{}) || V <- Vs];
                      false ->
                          [add_to_acc(ProtoMod, Field, Vs, #{})]
                  end,
            Old = maps:get(Field#field.name, Acc, []),
            Acc2 = Acc#{Field#field.name => Old ++ Vs2},
            process_fields(ProtoMod, Fields, Fs, Acc2);
        _ ->
            Acc2 = add_to_acc(ProtoMod, Field, Vs, Acc),
            process_fields(ProtoMod, Fields, Fs, Acc2)
    end.


add_to_acc(ProtoMod, {oneof, F, OneOfFields}, Vs, Acc) ->
    case [O || O <- OneOfFields, maps:is_key(O#field.name, Acc)] of
        [] ->
            add_to_acc(ProtoMod, F, Vs, Acc);
        Others ->
            New = [#{F#field.name => Vs}],
            Old = [#{O#field.name => maps:get(O#field.name, Acc)} || O <- Others],
            throw({error, {multiple_oneof, Old ++ New}})
    end;
add_to_acc(_ProtoMod, #field{type = {map,_,_}}, Vs, Acc) ->
    [#scalar{key = "key", value = V1}, #scalar{key = "value", value = V2}|_] = Vs,
    Acc#{V1 => V2};
add_to_acc(ProtoMod, #field{type = {msg, Msg}, occurrence = repeated}, V, Acc) ->
    D = ProtoMod:fetch_msg_def(Msg),
    process_fields(ProtoMod, D, V, Acc);
add_to_acc(ProtoMod, #field{type = {msg, Msg}} = F, V, Acc) ->
    D = ProtoMod:fetch_msg_def(Msg),
    Acc#{F#field.name => process_fields(ProtoMod, D, V, #{})};
add_to_acc(_ProtoMod, #field{occurrence = repeated} = F, Vs, Acc) when is_list(Vs) ->
    Old = maps:get(F#field.name, Acc, []),
    Acc#{F#field.name => Old ++ Vs};
add_to_acc(_ProtoMod, #field{occurrence = repeated} = F, V, Acc) ->
    Old = maps:get(F#field.name, Acc, []),
    Acc#{F#field.name => Old ++ [V]};
add_to_acc(_ProtoMod, #field{} = F, V, Acc) ->
    Acc#{F#field.name => V}.

-spec to_map(parsed_forms() | [list()]) -> map().
to_map(Message) ->
    to_map(Message, []).

to_map([M|_] = Messages, Opts) when is_list(M) ->
    to_map(Messages, [], Opts);
to_map(Messages, Opts) ->
    to_map(Messages, #{}, Opts).

to_map(Messages, StartAcc, Opts) ->
    %% Opts1 = gpb_names:mk_rename_operations(Opts),
    {A, _} = lists:foldl(fun to_map2/2, {StartAcc, Opts}, Messages),
    A.

to_map2(#message{name = Name, fields = Fields}, {Map, Opts}) ->
    {Map#{format_msg_key(Name, Opts) => to_map(Fields, Opts)}, Opts};
to_map2(#scalar{key = Key, value = Value}, {Map, Opts}) ->
    {Map#{format_scalar_key(Key, Opts) => Value}, Opts};
to_map2(Fields, {Map, Opts}) when is_list(Fields) ->
    {[to_map(Fields, Opts)|Map], Opts}.

-spec find_proto_filename(string()) -> not_found | {ok, string()}.
find_proto_filename(FileContent) ->
    case re:run(FileContent, <<".*proto-file:\s?([^\n]+).*">>, [{capture, [1], list}]) of
        nomatch ->
            not_found;
        {match, [Syntax]} ->
            {ok, Syntax}
    end.

-spec find_proto_message(string()) -> not_found | {ok, string()}.
find_proto_message(FileContent) ->
    case re:run(FileContent, <<".*proto-message:\s?([^\n]+).*">>, [{capture, [1], list}]) of
        nomatch ->
            not_found;
        {match, [Syntax]} ->
            {ok, Syntax}
    end.

-spec get_proto_mod(file:filename()) -> {ok, module()} | {error, term()}.
get_proto_mod(FileName) ->
    BName = filename:basename(FileName, ".proto"),
    ProtoFileName = list_to_atom(BName),
    case code:is_loaded(ProtoFileName) of
        {file, _} ->
            {ok, ProtoFileName};
        false ->
            {error, {not_loaded, ProtoFileName}}
    end.

-spec get_proto_defs(module()) -> gpb_defs:defs().
get_proto_defs(ProtoMod) ->
    case erlang:function_exported(ProtoMod, get_msg_defs, 0) of
        true ->
            ProtoMod:get_msg_defs();
        false ->
            not_found
    end.


-spec format_msg_key(atom(), [option()]) -> atom().
format_msg_key(Key, Opts) ->
    Fn = get_rename_opts(msg_name, Opts),
    Fn(Key).

format_scalar_key(Key, Opts) ->
    Fn = get_rename_opts(msg_typename, Opts),
    Fn(Key).

get_rename_opts(RenameKey, Opts) ->
    RenameOpts = proplists:get_all_values(rename, Opts),
    case proplists:lookup(RenameKey, RenameOpts) of
        none ->
            fun (X) -> X end;
        {RenameKey, RenameFun} ->
            RenameFun
    end.
