-module(fire).
 
-behaviour(gen_fsm).
 
%% API
-export([start/5]).
 
%% gen_fsm callbacks
-export([init/1,idle/2,idle/3,fire_out/2,fire_out/3, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
%%-export([start_sim/1,extinguish_fire/1,merge/1,update_sens/2,update_heli/2]).
-export([start_sim/1,extinguish_fire/1,merge/1]).

-include("config.hrl").
% -define(FIRE_REFRESH_SPEED,100).
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
start(Name,ServerName,Radius,X,Y) ->
    gen_fsm:start({global, Name}, ?MODULE, [Name,ServerName,Radius,X,Y], []).
 
start_sim(Name) ->
  gen_fsm:send_event({global, Name}, {start}).
  
extinguish_fire(Name) ->
  gen_fsm:sync_send_event({global, Name}, {decrease}).
  
merge(Name) ->
  gen_fsm:send_event({global, Name}, {merge}).
  
%%update_sens(Sens,Command) ->
%%  gen_fsm:send_event({global, Name}, {update_sensor,Sens,Command}).
  
%%update_heli(Heli,Command) ->
%%  gen_fsm:send_event({global, Name}, {update_heli,Heli,Command}).
 
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
init([Name,ServerName,StartRadius,X,Y]) ->
	Ets = ets:new(firedata,[set]),
	ets:insert(Ets,{radius,StartRadius}),
	ets:insert(Ets,{x,X}),
	ets:insert(Ets,{y,Y}),
	ets:insert(Ets,{mergeValid,true}),
	ets:insert(Ets,{myName,Name}),
	ets:insert(Ets,{serverName,ServerName}),
	io:format("started fire with radius = ~p~n",[StartRadius]),
    {ok, idle, Ets}.
 
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

idle({start}, State) ->
  {next_state, idle, State,?FIRE_REFRESH_SPEED};

idle(timeout, State) ->
	[{_,Radius}] = ets:lookup(State,radius),
	NewRad = Radius + 0.3,
	ets:insert(State,{radius,NewRad}),
	[{_,MyName}] = ets:lookup(State,myName),
	[{_,ServerName}] = ets:lookup(State,serverName),
	unit_server:update(ServerName,fire,[MyName,NewRad]),
	%io:format("Fire++ ~p, new Radius = ~p~n",[MyName,NewRad]),
	{next_state, idle, State,?FIRE_REFRESH_SPEED};
	
	
idle({merge}, State) ->
	[{_,MergeValid}] = ets:lookup(State,mergeValid),
	case MergeValid of
		 true-> [{_,MyName}] = ets:lookup(State,myName),
			[{_,ServerName}] = ets:lookup(State,serverName),
			unit_server:update(ServerName,fire,[MyName,0]),
			io:format("merge ~p~n",[MyName]),
			{next_state, fire_out, State};
		  false->{next_state, idle, State,?FIRE_REFRESH_SPEED}
	end;

idle(_Event, State) ->
  {next_state, idle, State}.
  
fire_out(_Event, State) ->
	%io:format("no fire~n"),
	{next_state, fire_out, State}.
 
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
 
idle({decrease},_From,State) ->
	%ets:insert(State,{mergeValid,false}),
	[{_,Radius}] = ets:lookup(State,radius),
	case Radius -0.5 =<0 of
		true -> NewRad=0;
		false -> NewRad = Radius - 0.5
	end,
	%io:format("Fire-- new Radius = ~p~n",[NewRad]),
	ets:insert(State,{radius,NewRad}),
	[{_,MyName}] = ets:lookup(State,myName),
	[{_,ServerName}] = ets:lookup(State,serverName),
	unit_server:update(ServerName,fire,[MyName,NewRad]),
	%io:format("new Radius = ~p~n",[NewRad]),
	case NewRad == 0 of
		true -> io:format("fire extinguished~n"), {reply, {fire_dead,NewRad}, fire_out, State};
		false -> {reply, {fire_alive,NewRad}, idle, State, ?FIRE_REFRESH_SPEED}
	end; 
 
idle(_Event, _From, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, idle, State}.
  
fire_out(_Event, _From, State) ->
  {reply, {fire_dead,0}, fire_out, State}.
 
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

delete_from_ets(KeyName,Value) ->
	[{_,List}] = ets:lookup(firedata,KeyName),
	NewList = [ Val || Val <-List, Val /= Value],
	ets:insert(firedata,{KeyName,NewList}).
	
add_to_ets(KeyName,Value) ->
	[{_,List}] = ets:lookup(firedata,KeyName),
	case lists:member(Value,List) of
		false -> ets:insert(firedata,{KeyName,[Value|List]});
		true -> already_exists
	end.