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
-export([init/1, handle_call/3, server/0, balance/2, deposit/3, handle_continue/2, handle_info/2, withdraw/3, lend/4]).

server() ->
  gen_server:start(?MODULE, [], []).


init(_Args) ->
  {ok, #state{}, 5000}.

handle_call({balance, Name}, _From, State = #state{db = Db, num_requests = NumReq}) ->
  Response = case maps:find(Name, Db) of
               error ->
                 no_account;
               {ok, Balance} ->
                 {ok, Balance}
             end,
  {reply, Response, State#state{db = Db, num_requests = NumReq + 1}, {continue, {balance, Name}}};
handle_call(num_requests, _From, State = #state{num_requests = NumReq}) ->
  {reply, NumReq, State}.
handle_call({deposit, {Name, Amount}}, State = #state{db = Db}) ->
  case maps:find(Name, Db) of
    error ->
      NewDb = Db#{Name => Amount},
      {ok, Amount};
    {ok, Balance} ->
      NewDb = Db#{Name => Amount + Balance},
      {ok, Amount + Balance}
  end, {noreply, State#state{db = NewDb}, {continue, {deposit, Name, Amount}}};
handle_call({withdraw, {Name, Amount}}, State = #state{db = Db}) ->
  case maps:find(Name, Db) of
    error ->
      no_account;
    {ok, Balance} ->
      case (Amount - Balance) < 0 of
        true ->
          insufficient_funds,
          {noreply, State#state{db = db}, {continue, {deposit, Name, Amount}}};
        false ->
          NewDb = Db#{Name => Balance - Amount},
          {ok, Amount + Balance},
          {noreply, State#state{db = NewDb}, {continue, {deposit, Name, Amount}}}
      end
  end;

handle_call({lend, {From, To, Amount}}, State = #state{db = Db}) ->
  case maps:find(From, Db) of
    error ->
      {no_account, From};
    {ok, FromBalance} ->
      case maps:find(To, Db) of
        error ->
          {no_account, To};
        {ok, ToBalance} ->
          case (Amount - FromBalance) < 0 of
            true ->
              insufficient_funds,
              {noreply, State#state{db = db}, {continue, {lend, From, To, Amount}}};
            false ->
              NewDb = Db#{To => ToBalance + Amount, From => FromBalance - Amount},
              ok,
              {noreply, State#state{db = NewDb}, {continue, {lend, From, To, Amount}}}
          end
      end

  end.



handle_continue({balance, Name}, State) ->
  io:format("Server got a requested get balance from ~p \n", [Name]),
  {noreply, State};
handle_continue({deposit, Name, Amount}, State) ->
  io:format("Server was requested to deposit ~p to ~p \n", [Amount, Name]),
  {noreply, State};
handle_continue({withdraw, Name, Amount}, State) ->
  io:format("Server was requested  withdraw ~p to ~p \n", [Amount, Name]),
  {noreply, State};
handle_continue({lend, From, To, Amount}, State) ->
  io:format("Server was requested  lend From ~p to ~p the amount: ~p \n", [From, To, Amount]),
  {noreply, State}.


handle_info(timeout, State = #state{db = Db}) ->
  {noreply, State#state{db = Db#{timeout => "We had a timeout"}}};
handle_info(_Info, State) ->
  {noreply, State}.




balance(Bank, Name) when is_pid(Bank) ->
  gen_server:call(Bank, {balance, Name}).

deposit(Bank, Name, Amount) when is_pid(Bank) ->
  gen_server:call(Bank, {deposit, {Name, Amount}}).
withdraw(Bank, Name, Amount) when is_pid(Bank) ->
  gen_server:call(Bank, {withdraw, {Name, Amount}}).
lend(Bank, From, To, Amount) when is_pid(Bank) ->
  gen_server:call(Bank, {lend, {From, To, Amount}}).


