%%%-------------------------------------------------------------------
%%% @author Douglas
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. mars 2020 10:54
%%%-------------------------------------------------------------------
-module(ring).
-author("Douglas").

%% API
-export([start/2]).

start(N, M) ->
  lists:map(
    fun( Num ) ->
      register(
        list_to_atom( "pid" ++ integer_to_list( Num ) ),
        spawn( fun() -> circle(0) end ) )
    end,
    lists:seq( 1, N ) ).







circle(I) ->
  Ref = make_ref(),
  receive
    {Pid, Ref, I} ->
      whereis()
  end.