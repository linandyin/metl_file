%%%-------------------------------------------------------------------
%%% @author linzexin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 十二月 2017 14:13
%%%-------------------------------------------------------------------
-module(metl_web).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, #state{}}.


handle(Req, State=#state{}) ->
    {ok, Bin, Req2} = cowboy_req:body(Req),
    metl_mnesia:add_req_item(Bin),
    {ok, Req3} = cowboy_req:reply(200, [], [], Req2),
    {ok, Req3, State}.




terminate(_Reason, _Req, _State) ->
    ok.
