%%%-------------------------------------------------------------------
%%% @author Douglas
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mars 2020 11:20
%%%-------------------------------------------------------------------
-module(nameserver_rec).
-author("Douglas").
-behavior(gen_server).
-compile(export_all).

-record(state, {db = #{},
  num_requests=0}).

%% Perhaps: register nameserver
%% but I think it is better to be explicit
server() ->
  gen_server:start(?MODULE, [], []).

init(_Args) ->
  % REMOVE: ets:new(name_server, [set,private,named_table]),
  {ok, #state{}, 5000}.

handle_call({get, Name}, _From, State=#state{db=Db, num_requests=NumReq}) ->
  Response = case maps:find(Name, Db) of
               error ->
                 not_found;
               {ok, Address} ->
                 {ok, Address}
             end,
  {reply, Response, State#state{db=Db, num_requests=NumReq + 1}, {continue, {get, Name}}};
handle_call(num_requests, _From, State=#state{num_requests=NumReq}) ->
  {reply, NumReq, State}.

%% Start as handle_call
handle_cast({set, {Name, Address}}, State = #state{db=Db}) ->
  NewDb = Db#{Name => Address},
  {noreply, State#state{db=NewDb}, {continue, {set, Name, Address}}}.

handle_continue({get, Name}, State) ->
  io:format("Server got a request for ~s~n", [Name]),
  {noreply, State};
handle_continue({set, Name, Address}, State) ->
  io:format("Server was requested to set ~s to ~s~n", [Name, Address]),
  {noreply, State}.


handle_info(timeout, State=#state{db=Db}) ->
  {noreply, State#state{db=Db#{timeout => "We had a timeout"}}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% If we are ok with no ackn
set(Pid, Name, Address) when is_pid(Pid) ->
  gen_server:cast(Pid, {set, {Name, Address}}).

get(Pid, Name) when is_pid(Pid) ->
  gen_server:call(Pid, {get, Name}).

%%% Show lecture slides on untagged messages!
%% Change get, set (client, and server)
%% make_ref(), send ref, receive ref, resend ref, receive ref
