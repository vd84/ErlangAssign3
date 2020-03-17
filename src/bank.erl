%%%-------------------------------------------------------------------
%%% @author Douglas
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mars 2020 10:59
%%%-------------------------------------------------------------------
-module(bank).
-author("Douglas").
-behavior(gen_server).
-record(state, {db = #{},
  num_requests = 0}).


%% API
-export([init/1, handle_call/3, handle_cast/2, server/0, balance/2, deposit/3, handle_continue/2, handle_info/2]).

server() ->
  gen_server:start(?MODULE, [], []).


init(_Args) ->
  {ok, #state{}, 5000}.

handle_call({balance, Name}, _From, State = #state{db = Db, num_requests = NumReq}) ->
  Response = case maps:find(Name, Db) of
               error ->
                 not_found;
               {ok, Balance} ->
                 {ok, Balance}
             end,
  {reply, Response, State#state{db = Db, num_requests = NumReq + 1}, {continue, {balance, Name}}};
handle_call(num_requests, _From, State = #state{num_requests = NumReq}) ->
  {reply, NumReq, State}.
handle_cast({deposit, {Name, Amount}}, State = #state{db = Db}) ->
  NewDb = Db#{Name => Amount},
  {noreply, State#state{db = NewDb}, {continue, {deposit, Name, Amount}}}.



handle_continue({balance, Name}, State) ->
  io:format("Server got a request balance"),
  {noreply, State};
handle_continue({deposit, Name, Amount}, State) ->
  io:format("Server was requested deposit "),
  {noreply, State}.


handle_info(timeout, State=#state{db=Db}) ->
  {noreply, State#state{db=Db#{timeout => "We had a timeout"}}};
handle_info(_Info, State) ->
  {noreply, State}.




balance(Bank, Name) when is_pid(Bank) ->
  gen_server:call(Bank, {balance, Name}).

deposit(Bank, Name, Amount) when is_pid(Bank) ->
  gen_server:cast(Bank, {deposit, {Name, Amount}}).


