%%%-------------------------------------------------------------------
%%% @author linzexin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十二月 2017 10:47
%%%-------------------------------------------------------------------
-module(sub_process).
-author("linzexin").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    content/2,
    write_file/1,
    select_data/1]
).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(start_link() ->
%%    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name) ->
    gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    erlang:send_after(1000,erlang:self(),closeFiles),
    [{_,ProcessName}|_] = erlang:process_info(self()),
    [AppId|[LogType|_]] = string:tokens(atom_to_list(ProcessName),"/"),
    erlang:put(app_id,AppId),
    erlang:put(log_type,LogType),
    erlang:put(processname,ProcessName),
    erlang:send_after(1000,erlang:self(),loop),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
content([],_)  -> [<<"\t\n">>];
content([H1|L1],B) ->
    Temp = maps:get(H1,B),
    [[Temp|[<<"\t">>]]| content(L1,B)].

write_file([]) ->
    AppId = list_to_binary(erlang:get(app_id)),
    LogType = list_to_binary(erlang:get(log_type)),
    Fields = maps:get({AppId,LogType},maps:from_list(ets:lookup(app_log,{AppId,LogType}))),
    Database = maps:get(<<"database">>,Fields),
    Table = maps:get(<<"table">>,Fields),
    DbS = binary_to_list(Database),
    TbS = binary_to_list(Table),
    {_,OldS} = maps:get({DbS,TbS},maps:from_list(ets:lookup(file_tb,{DbS,TbS}))),
    file:sync(OldS),
    ok;
write_file([H|L]) ->
    [Logs|_] = maps:get(H,maps:from_list(ets:lookup(erlang:get(processname),H))),
    AppId = list_to_binary(erlang:get(app_id)),
    LogType = list_to_binary(erlang:get(log_type)),
    Fields = maps:get({AppId,LogType},maps:from_list(ets:lookup(app_log,{AppId,LogType}))),
    Database = maps:get(<<"database">>,Fields),
    Table = maps:get(<<"table">>,Fields),
    DbS = binary_to_list(Database),
    TbS = binary_to_list(Table),
    TableInfo = maps:get({Database,Table},maps:from_list(ets:lookup(table_info,{Database,Table}))),
    TableFields = maps:get(<<"fields">>,TableInfo),
    file:make_dir(DbS),
    file:make_dir(lists:concat([DbS,"/",TbS])),
    List = ets:lookup(file_tb,{DbS,TbS}),
    case List of
        [] ->
            {{Year,Month,Date},{Hour,_,_}} = calendar:local_time(),
            Filename = lists:concat([Year,"-",Month,"-",Date,"-",Hour,".csv"]),
            {ok,S} = file:open(lists:concat([DbS,"/",TbS,"/",Filename]),[append]),
            ets:insert(file_tb,{{DbS,TbS},{Filename,S}}),
            file:write(S,content(TableFields,Logs));
        _ ->
            {OldFilename,OldS} = maps:get({DbS,TbS},maps:from_list(ets:lookup(file_tb,{DbS,TbS}))),
            {{Year,Month,Date},{Hour,_,_}} = calendar:local_time(),
            NewFilename = lists:concat([Year,"-",Month,"-",Date,"-",Hour,".csv"]),
            if
                OldFilename =:= NewFilename ->
                    file:write(OldS,content(TableFields,Logs));
                true ->
                    file:close(OldS),
                    ets:delete(file_tb,{DbS,TbS}),
                    {ok,NewS} = file:open(lists:concat([DbS,"/",TbS,"/",NewFilename]),[append]),
                    ets:insert(file_tb,{{DbS,TbS},{NewFilename,NewS}}),
                    file:write(NewS,content(TableFields,Logs))
            end
    end,
    write_file(L).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

delete_datas([]) -> ok;
delete_datas([H|L]) ->
    ets:delete(erlang:get(processname),H),
    mnesia:dirty_delete(req,H),
    delete_datas(L).

select_data(Keys) ->
    case write_file(Keys) of
          ok ->delete_datas(Keys);
          _  -> write_file(Keys)
    end.


do_loop() ->
    Keys = ets:select(erlang:get(processname),[{{'$1','_'},[],['$1']}]),
    if
        erlang:length(Keys) =:= 0 -> ok;
        true -> select_data(Keys)
    end,
    erlang:send_after(5000,erlang:self(),loop).

close_files([]) -> erlang:send_after(3600000,erlang:self(),closeFiles);
close_files([H|L]) ->
    {OldFilename,OldS} = maps:get(H,maps:from_list(ets:lookup(file_tb,H))),
    {{Year,Month,Date},{Hour,_,_}} = calendar:local_time(),
    NewFilename = lists:concat([Year,"-",Month,"-",Date,"-",Hour,".tsv"]),
    if
        OldFilename =:= NewFilename ->
            ok;
        true ->
            file:close(OldS),
            ets:delete(file_tb,H)
    end,
    close_files(L),
    ok.

handle_info(loop, State) ->
     do_loop(),
    {noreply, State};
handle_info(closeFiles, State) ->
    Files = ets:select(file_tb,[{{'$1','_'},[],['$1']}]),
    close_files(Files),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
