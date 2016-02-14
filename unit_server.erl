
-module(unit_server).
-behaviour(gen_server).

% interface calls
-export([start/1, stop/1]).
    
% gen_server callbacks
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).
% gen_server func
-export([update/2]).


%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start(ServName) -> 
    io:format("Starting with ~p~n",[1]),
    gen_server:start_link({local, ServName}, ?MODULE, [1], []).

%% Stopping server asynchronously
stop(ServName) ->
    io:format("Stopping~n"),
    gen_server:cast(ServName, shutdown).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    io:format("Initializing with ~p~n",[Pars]),
    process_flag(trap_exit, true),
    ets:new(genral,[set,named_table]),
    {ok, initialized}.

%% Synchronous, possible return values  
% {reply,Reply,NewState} 
% {reply,Reply,NewState,Timeout}
% {reply,Reply,NewState,hibernate}
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,Reply,NewState} 
% {stop,Reason,NewState}
handle_call(Message, From, State) -> 
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

%% Asynchronous, possible return values
% {noreply,NewState} 
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast({unlock,Code}, State) ->
	gen_fsm:sync_send_event(gfsm, {unlock, Code}),
    io:format("Generic cast handler: '~p' while in '~p'~n",[{unlock,Code}, State]),
    {noreply, State};
handle_cast({lock,Code}, State) ->
	gen_fsm:sync_send_event(gfsm, {lock, Code}),
    io:format("Generic cast handler: '~p' while in '~p'~n",[{lock,Code}, State]),
    {noreply, State};

handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};
%% generic async handler
handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n",[Message, State]),
    {noreply, State}.

%% Informative calls
% {noreply,NewState} 
% {noreply,NewState,Timeout} 
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
handle_info(_Message, _Server) -> 
    io:format("Generic info handler: '~p' '~p'~n",[_Message, _Server]),
    {noreply, _Server}.

%% Server termination
terminate(_Reason, _Server) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.   



update(Unit_type,Unit_data) ->
    io:format("update ~p:~p~n",[Unit_type,Unit_data]).
    %%process_flag(trap_exit, true),
    %%ets:new(genral,[set,named_table]),
   %% {ok, initialized}.
