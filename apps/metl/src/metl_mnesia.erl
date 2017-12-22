%%%-------------------------------------------------------------------
%%% @author linzexin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 十二月 2017 9:49
%%%-------------------------------------------------------------------
-module(metl_mnesia).
-author("linzexin").
-include_lib("stdlib/include/qlc.hrl").
-define(DB_NODES, [erlang:node()]).
-record(sequence, {name, seq}).%自增索引表，维护其他表的自增id
-record(req,{id,comtent}).
%% API
-compile(export_all).

do_this_once() ->
    check_init(),
    create_schema(),
    mnesia:create_schema(?DB_NODES),
    application:start(mnesia, permanent),
    wait_for_table(mnesia:system_info(local_tables)),
    ok.
check_init() ->
    %% 检查数据库目录是否正确
    MnesiaDir = mnesia:system_info(directory) ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            erlang:throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
        ok ->
            ok
    end.
create_schema() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            case  mnesia:create_schema(?DB_NODES) of
                {error,{_,{already_exists,_}}} ->
                    ok;
                ok ->
                    ok
            end;
        _ ->
            ok
    end,
    lists:foreach(
        fun(Node) ->
            mnesia:change_table_copy_type(schema, Node, disc_copies)
        end, ?DB_NODES),
    ok.
create_req_table()->
    mnesia:create_table(req,   [
        {disc_copies, ?DB_NODES},
        {record_name, req},
        {type, set},
        {attributes, record_info(fields, req)}
    ]).
create_sequence_table()->
    mnesia:create_table(sequence,   [
        {disc_copies, ?DB_NODES},
        {record_name, sequence},
        {type, set},
        {attributes, record_info(fields, sequence)}
    ]).
wait_for_table(Tabs) when erlang:is_list(Tabs)->
    mnesia:wait_for_tables(Tabs, infinity);
wait_for_table(Tab) when erlang:is_atom(Tab)->
    wait_for_table([Tab]).
start() ->
    mnesia:start().

add_req_item(Name) ->
    Id = mnesia:dirty_update_counter(sequence, req, 1),
    Row = #req{id = Id,comtent=Name},
    F = fun() ->
        mnesia:write(Row)
        end,
    mnesia:transaction(F).
select(Tb) ->
    do(qlc:q([X || X <- mnesia:table(Tb)])).

select(Id,Tb) ->
    do(qlc:q([X#req.comtent || X <- mnesia:table(Tb),
        X#req.id < Id
    ])).
%% SQL equivalent
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
reset_tables() ->
    mnesia:clear_table(req),
    mnesia:clear_table(sequence).

get_plan(Id) ->
    F = fun() -> mnesia:read({req, Id}) end,
    mnesia:transaction(F).

remove_req_item(Id) ->
    Oid = {req, Id},
    F = fun() ->
        mnesia:delete(Oid)
        end,
    mnesia:transaction(F).
remove_req_more(Num,Key) ->
    for(Num,Key).

for(0,_) ->
    [];
for(N,Key) when N >= 0 ->
    Oid = {req, Key},
    F = fun() ->
        mnesia:delete(Oid)
        end,
    mnesia:transaction(F),
    for(N-1,Key-1).