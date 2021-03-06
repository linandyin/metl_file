%%%-------------------------------------------------------------------
%%% @author linzexin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十二月 2017 17:23
%%%-------------------------------------------------------------------
-module(ets_config).
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
    init_app_log/0,
    tra_database/3,
    tra_table/4,
    init_table_info/0,
    init_file_tb/0,
    init_processes/0]).

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
tra_table(_,[],_,_) -> [];
tra_table(Key,List,Maps,EtsTable) ->
    [H|L] = List,
    Fileds = maps:get(H,Maps),
    ets:insert(EtsTable,{{Key,H},Fileds}),
    tra_table(Key,L,Maps,EtsTable),
    ok.

tra_database([],_,_) -> [];
tra_database(List,Maps,EtsTable) ->
    [H|L] = List,
    TablesMaps = maps:get(H,Maps),
    TableList = maps:keys(TablesMaps),
    tra_table(H,TableList,TablesMaps,EtsTable),
    tra_database(L,Maps,EtsTable),
    ok.

init_app_log() ->
    ets:new(app_log,[set,named_table,public]),
    {ok,Bin} = file:read_file("app_log.json"),
    AppLogMaps = jsx:decode(Bin,[return_maps]),
    AppLogList = maps:keys(AppLogMaps),
    tra_database(AppLogList,AppLogMaps,app_log),
    ok.

init_table_info() ->
    ets:new(table_info,[set,named_table,public]),
    {ok,Bin} = file:read_file("table_info.json"),
    TableInfoMaps = jsx:decode(Bin,[return_maps]),
    TableInfoList = maps:keys(TableInfoMaps),
    tra_database(TableInfoList,TableInfoMaps,table_info),
    ok.

init_file_tb() ->
    ets:new(file_tb,[set,named_table,public]),
    ok.

init_processes() ->
    ets:new(processes,[set,named_table,public]),
    {ok,Bin} = file:read_file("processes.json"),
    TableInfoMaps = jsx:decode(Bin,[return_maps]),
    TableInfoList = maps:keys(TableInfoMaps),
    tra_database(TableInfoList,TableInfoMaps,processes),
    ok.

init([]) ->
    init_app_log(),
    init_table_info(),
    init_file_tb(),
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
handle_info(_Request, State) ->
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
