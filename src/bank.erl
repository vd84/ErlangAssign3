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
-export([init/1, handle_call/3, start/0, balance/2, deposit/3, handle_continue/2, handle_info/2, withdraw/3, lend/4, handle_cast/2]).

start() ->
  gen_server:start(?MODULE, [], []).


init(_Args) ->
  {ok, #state{}, 5000}.

handle_call({balance, Name}, _From, State = #state{db = Db}) ->
  Response = case maps:find(Name, Db) of
               error ->
                 no_account;
               {ok, Balance} ->
                 {ok, Balance}
             end,
  {reply, Response, State#state{db = Db}, {continue, {balance, Name}}};
handle_call({deposit, {Name, Amount}}, _From,  State = #state{db = Db}) ->
  {Response, ReturnDb} = case maps:find(Name, Db) of
               error ->
                 NewDb = Db#{Name => Amount},
                 {{ok, Amount}, NewDb};
               {ok, Balance} ->
                 NewDb = Db#{Name => Amount + Balance},
                 {{ok, Amount + Balance}, NewDb}
             end, {reply, Response, State#state{db = ReturnDb}, {continue, {deposit, Name, Amount}}};
handle_call({withdraw, {Name, Amount}}, _From, State = #state{db = Db}) ->
  {Response, ReturnDb} = case maps:find(Name, Db) of
               error ->
                 {no_account, Db};
               {ok, Balance} ->
                 case (Amount - Balance) < 0 of
                   true ->
                     {insufficient_funds, Db};
                   false ->
                     NewDb = Db#{Name => Balance - Amount},
                     {{ok, Amount + Balance}, NewDb}
                 end
             end, {reply, Response, State#state{db = ReturnDb}, {continue, {withdraw, Name, Amount}}};

handle_call({lend, {From, To, Amount}}, _From, State = #state{db = Db}) ->
  {Response, ReturnDb} = case maps:find(From, Db) of
               error ->
                 case maps:find(To, Db) of
                   {ok, _Balance} ->
                     {{no_account, From}, Db};
                   error ->
                     {{no_account, both}, Db}
                 end;
               {ok, FromBalance} ->
                 case maps:find(To, Db) of
                   error ->
                     {{no_account, To}, Db};
                   {ok, ToBalance} ->
                     case (FromBalance - Amount) < 0 of
                       true ->
                         {insufficient_funds,Db};
                       false ->
                         NewDb = Db#{To => ToBalance + Amount},
                         NewDb2 = NewDb#{From => FromBalance - Amount},
                         {ok, NewDb2}
                     end
                 end

             end, {reply, Response, State#state{db = ReturnDb}, {continue, {lend, From, To, Amount}}}.

handle_cast(_,_) ->
  ok.




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


