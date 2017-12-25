%%%-------------------------------------------------------------------
%% @doc metl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(metl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children = [
        {web_writefile, {web_writefile,start_link,[]},permanent,10000,worker,[web_writefile]},
        {ets_config, {ets_config,start_link,[]},permanent,10000,worker,[ets_config]},
        {sub_process_1, {sub_process_1,start_link,[]},permanent,10000,worker,[sub_process_1]}
    ],
    {ok, { {one_for_one, 0, 1},Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
