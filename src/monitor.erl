%%%-------------------------------------------------------------------
%%% @author dogge
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. mars 2020 10:24
%%%-------------------------------------------------------------------
-module(monitor).
-author("dogge").
-behavior(supervisor).
%% API
-export([init/1, start/0]).
start() ->
  supervisor:start_link(?MODULE, []).
init(_) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 10,
    period => 5},
  ChildSpec = [#{id => double_id,
    start => {double, start, []}}],
  {ok, {SupFlags, ChildSpec}}.
