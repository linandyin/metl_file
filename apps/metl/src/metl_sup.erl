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
        {main_process, {main_process,start_link,[]},permanent,10000,worker,[main_process]},
        {ets_config, {ets_config,start_link,[]},permanent,10000,worker,[ets_config]},
        {web_write_sup, {web_write_sup,start_link,[]},permanent,10000,worker,[web_write_sup]}
    ],
    {ok, { {one_for_one, 0, 1},Children} }.

%%====================================================================
%% Internal functions
%%====================================================================