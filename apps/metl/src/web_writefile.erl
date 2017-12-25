%%%-------------------------------------------------------------------
%%% @author linzexin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十二月 2017 16:32
%%%-------------------------------------------------------------------
-module(web_writefile).
-author("linzexin").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    content/2,
    write_file/1
    ]).

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
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok,Bin} = file:read_file("app_log.json"),
    {ok,Bin1} = file:read_file("table_info.json"),
    TableId = ets:new(test,[set]),
    put(tableid,TableId),
    ets:insert(TableId,{app_log,Bin}),
    ets:insert(TableId,{table_info,Bin1}),
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

content([],_)  -> [];
content([H1|L1],B) ->
    Temp = maps:get(H1,B),
    if
        is_integer(Temp) -> integer_to_list(Temp) ++ "\t" ++ content(L1,B);
        is_binary(Temp) -> binary_to_list(Temp) ++ "\t" ++ content(L1,B);
        is_boolean(Temp) -> atom_to_list(Temp) ++ "\t" ++ content(L1,B)
    end.

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
write_file([]) -> [];
write_file(L) ->
    [F|N] = L,
    #{<<"logs">> := Logs, <<"headers">> := Headers } = jsx:decode(F,[return_maps]),
    [H|[]] = Logs,
    App_log = jsx:decode(maps:get(app_log,maps:from_list(ets:lookup(get(tableid),app_log))),[return_maps]),
    App_log1 = maps:get(list_to_binary(integer_to_list(maps:get(<<"app_id">>,Headers))),App_log),
    [Db_b|[]] = maps:keys(maps:get(maps:get(<<"log_type">>,Headers),App_log1)),
    Db_s = binary_to_list(Db_b),
    Tb_b = maps:get(Db_b,maps:get(maps:get(<<"log_type">>,Headers),App_log1)),
    Tb_s = binary_to_list(Tb_b),
    Table_info = maps:get(<<"fields">>,maps:get(Tb_b,jsx:decode(maps:get(table_info,maps:from_list(ets:lookup(get(tableid),table_info))),[return_maps]))),
    file:make_dir(Db_s),
    file:make_dir(Db_s ++ "\\" ++ Tb_s),
    {{Year,Month,Date},{Hour,_,_}} = calendar:local_time(),
    Filename = integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Date) ++ "-" ++integer_to_list(Hour) ++ ".csv",
    {ok, S} = file:open(Db_s ++  "\\"++ Tb_s ++"\\"  ++ Filename , [append]),
    io:format(S,"~s~n",[content(Table_info,H)]),
    write_file(N).



do_loop() ->
    Key = mnesia:dirty_update_counter(sequence, req, 0),
    if
        Key >= 10 ->
            Temp = metl_mnesia:select(10,req),
            write_file(Temp),
            metl_mnesia:remove_req_more(10,Key),
            mnesia:dirty_update_counter(sequence,req,-10),
            erlang:send_after(5000,erlang:self(),loop);
        true -> erlang:send_after(5000,erlang:self(),loop)
    end,
    ok.
handle_info(_Info, State) ->
    erlang:send_after(5000,erlang:self(),loop),
    do_loop(),
%%    gen_server:call(?MODULE,[]),
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
