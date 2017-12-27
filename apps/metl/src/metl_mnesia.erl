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
-record(req,{id,comtent}).
-record(sequence, {name, seq}).%自增索引表，维护其他表的自增id
%% API
-export([
    do_this_once/0,
    check_init/0,
    create_schema/0,
    create_table_req/0,
    create_table_sequence/0,
    add_req_item/1,
    select/1,
    select/3,
    reset_tables/0,
    remove_more/3,
    remove_req_item/1
]).

do_this_once() ->
    check_init(),
    create_schema(),
    mnesia:create_schema(?DB_NODES),
    application:start(mnesia, permanent),
    wait_for_table(mnesia:system_info(local_tables)),
    create_table_sequence(),
    create_table_req(),
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
create_table_req()->
    case catch mnesia:create_table(req, [
        {disc_copies, ?DB_NODES},
        {record_name, req},
        {type, set},
        {attributes, record_info(fields, req)}
    ]) of
        {atomic, _} ->
            error_logger:info_msg("mnesia:create_table atomic:~p", [req]),
            ok;
        {aborted, {already_exists, _}} ->
            error_logger:info_msg("mnesia:create_table already_exists:~p", [req]),
            ok;
        R ->
            error_logger:error_msg("~s ~p", [req, R]),
            error
    end.
create_table_sequence()->
    case catch mnesia:create_table(sequence, [
        {disc_copies, ?DB_NODES},
        {record_name, sequence},
        {type, set},
        {attributes, record_info(fields, sequence)}
    ]) of
        {atomic, _} ->
            error_logger:info_msg("mnesia:create_table atomic:~p", [sequence]),
            ok;
        {aborted, {already_exists, _}} ->
            error_logger:info_msg("mnesia:create_table already_exists:~p", [sequence]),
            ok;
        R ->
            error_logger:error_msg("~s ~p", [sequence, R]),
            error
    end.
wait_for_table(Tabs) when erlang:is_list(Tabs)->
    mnesia:wait_for_tables(Tabs, infinity);
wait_for_table(Tab) when erlang:is_atom(Tab)->
    wait_for_table([Tab]).

add_req_item(Name) ->
    Id = mnesia:dirty_update_counter(sequence, req, 1),
    Row = #req{id = Id,comtent=Name},
    mnesia:dirty_write(Row).

select(Tb) ->
    do(qlc:q([X || X <- mnesia:table(Tb)])).

select(Len,Id,Tb) ->
    do(qlc:q([X#req.comtent || X <- mnesia:table(Tb),
        X#req.id =< Id,X#req.id >= Id-Len
    ])).
%% SQL equivalent
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
reset_tables() ->
    mnesia:clear_table(req),
    mnesia:clear_table(sequence).


remove_req_item(Id) ->
    Oid = {req, Id},
    mnesia:dirty_delete(Oid).

remove_more(Tb,Num,Key) ->
    for(Tb,Num,Key).

for(_,0,_) ->
    [];
for(Tb,N,Key) when N >= 0 ->
    Oid = {Tb, Key},
    mnesia:dirty_delete(Oid),
    for(Tb,N-1,Key-1).