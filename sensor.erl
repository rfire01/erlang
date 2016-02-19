-module(sensor).
 
-include_lib("stdlib/include/qlc.hrl").
 
-behaviour(gen_fsm).
 
%% API
-export([start/2]).
 
%% gen_fsm callbacks
-export([init/1,idle/2,idle/3,working/2,working/3, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
-export([start_sim/1,new_alert/2]).
-include("config.hrl").
%%-define(SERVER, ?MODULE).
 
%-record(state, {code}).
 
%%%===================================================================
%%% API
%%%===================================================================
 
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(Name,Local_gen_name) ->
    gen_fsm:start({global, Name}, ?MODULE, [Name,Local_gen_name], []).

start_sim(Name) ->
  gen_fsm:send_event({global, Name}, {start_sim}).
	
new_alert(Name,FireName) ->
	%io:format("sending alert to sensor ~p~n",[Name]),
  gen_fsm:send_event({global, Name}, {new_alert,FireName}).
 
%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([SensorName,ServerName]) ->
	Ets=ets:new(active_fire,[set]),
	put(ets_id,Ets),
	ets:insert(Ets,{serverName,ServerName}),
	ets:insert(Ets,{myName,SensorName}),
	%io:format("started sensor ~p~n",[ets:tab2list(Ets)]),
    {ok, idle, {}}.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

idle({start_sim}, State) ->
	Self = self(),
	AlertPid = spawn(fun() -> loop(Self) end), 
	put(alerting,AlertPid),
	{next_state, working, State};

idle(_Event, State) ->
  {next_state, idle, State}.
  
working({new_alert,FireName}, State) ->
	Ets = get(ets_id),
	%io:format("alert receive for: ~p~n",[FireName]),
	ets:insert(Ets,{FireName,on}),
	{next_state, working, State};
	
working(_Event, State) ->
  {next_state, working, State}.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
 
idle(_Event, _From, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, idle, State}.
  
working(_Event, _From, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, working, State}.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(check_alert, StateName, State) ->

	Ets = get(ets_id),
	%io:format("current fires= ~p~n",[ets:tab2list(State)]),

	QH = qlc:q([Fire || {Fire,on} <- ets:table(Ets), Fire /=serverName, Fire /=myName]),
	
	[{_,Gen}] = ets:lookup(Ets,serverName),
	[{_,Sen}] = ets:lookup(Ets,myName),
    case qlc:eval(QH)  of
		[] -> no_alerts;
		FireList -> [ unit_server:heli_request(Gen,Sen,Name) ||Name <- FireList]
	end,
	ets:delete_all_objects(Ets),
	
	ets:insert(Ets,{serverName,Gen}),
	ets:insert(Ets,{myName,Sen}),
	
    {next_state, StateName, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
	case get(alerting) of
		undefined -> ok;
		Pid ->	exit(Pid,kill)
	end,
    ok.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
 
%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(Pid) -> receive after ?SENSOR_CHECK_TIME -> Pid ! check_alert end, loop(Pid).