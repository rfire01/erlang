-module(sensor).
 
-include_lib("stdlib/include/qlc.hrl").	%% for the qlc module
 
-behaviour(gen_fsm).
 
%% API
-export([start/2,stop/1]).
 
%% gen_fsm callbacks
-export([init/1,idle/2,idle/3,working/2,working/3, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
-export([start_sim/1,new_alert/2,crash/1,crash_recover/2]).
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

%%get from the unit_server for init the gen_fsm
start(Name,Local_gen_name) ->
    gen_fsm:start({global, Name}, ?MODULE, [Name,Local_gen_name], []).

%%start move
start_sim(Name) ->
  gen_fsm:send_event({global, Name}, {start_sim}).

%%find fire -> send alert to unit_server
new_alert(Name,FireName) ->
  gen_fsm:send_event({global, Name}, {new_alert,FireName}).
 
%%for test crash 
crash(Name) ->
	gen_fsm:send_event({global, Name}, {crash,0}).

%%for upload the sensor after crash	
crash_recover(Name,Data) ->
    gen_fsm:start({global, Name}, ?MODULE, [crash,Data], []).
%%exit	
stop(Name) ->
	gen_fsm:send_all_state_event({global, Name}, {stop}).  
 
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
%%crash message
init([crash,Data]) ->
	Ets=ets:new(active_fire,[set]), %% list of the active_fire on the sensor
	put(ets_id,Ets),
	ets:insert(Ets,Data),
	[{_,SensorName}] = ets:lookup(Ets,myName),		%%sensor name from ETS
	[{_,ServerName}] = ets:lookup(Ets,serverName),	%%sensor serverName from ETS
	
	%%Monitor hundle
	MonName = list_to_atom(atom_to_list(ServerName) ++ "mon"),
	MonPid = global:whereis_name(MonName),
	
	ets:setopts(Ets,{heir,MonPid , {sensor,SensorName}}),
	loc_monitor:add_mon(MonName,global:whereis_name(SensorName)),
	
	sensor:start_sim(SensorName),
	
    {ok, idle, {}};

	%%link monitor and save name and server to ets
init([SensorName,ServerName]) ->
	%%Monitor hundle
	MonName = list_to_atom(atom_to_list(ServerName) ++ "mon"),
	MonPid = global:whereis_name(MonName),
	
	Ets=ets:new(active_fire,[set,{heir,MonPid , {sensor,SensorName}}]),
	put(ets_id,Ets),
	ets:insert(Ets,{serverName,ServerName}),%%sensor serverName from ETS
	ets:insert(Ets,{myName,SensorName}),%%sensor name from ETS
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

%%recive state and action

%%state = idle
%%start sim
idle({start_sim}, State) ->
	Self = self(),
	AlertPid = spawn(fun() -> loop(Self) end), 
	put(alerting,AlertPid),
	{next_state, working, State};
%%test crash	
idle({crash,Num}, _State) ->
	{next_state, working, 1/Num};
%%else
idle(_Event, State) ->
  {next_state, idle, State}.
%%state = working
%% fire enter to the radios
working({new_alert,FireName}, State) ->
	Ets = get(ets_id),
	%io:format("alert receive for: ~p~n",[FireName]),
	ets:insert(Ets,{FireName,on}),
	{next_state, working, State};
%%test crash	
working({crash,Num}, _State) ->
	{next_state, working, 1/Num};
%%else	
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
 %%else
idle(_Event, _From, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, idle, State}.
 %%else 
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
handle_event({stop}, _StateName, State) ->
	{stop, normal, State};

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
handle_info({_,{stop}}, _StateName, State) ->
	{stop, normal, State};

%% in line 291+ "loop(Pid) -> receive after ?SENSOR_CHECK_TIME -> Pid ! check_alert end, loop(Pid)"
%% so the check_alert need to be in the handle_info gen_fsm func. 
%% we take qlc Query to find if fire enter the radios
handle_info(check_alert, StateName, State) ->

	Ets = get(ets_id),

	QH = qlc:q([Fire || {Fire,on} <- ets:table(Ets), Fire /=serverName, Fire /=myName]),
	
	[{_,Gen}] = ets:lookup(Ets,serverName),
	[{_,Sen}] = ets:lookup(Ets,myName),
	%%qlc Analysis of Results
    case qlc:eval(QH)  of
		[] -> no_alerts;
		FireList -> [ unit_server:heli_request(Gen,Sen,Name) ||Name <- FireList] %% fire in the radios
	end,
	ets:delete_all_objects(Ets),	%%clean all the fire from the last check becuse the sensor know only the active fire now
	
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
%%kill fire
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

%%check loop every second for fire in the radios
loop(Pid) -> receive after ?SENSOR_CHECK_TIME -> Pid ! check_alert end, loop(Pid).