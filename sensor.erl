-module(sensor).
 
-behaviour(gen_fsm).
 
%% API
-export([start/3]).
 
%% gen_fsm callbacks
-export([init/1,idle/2,idle/3, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
-export([check_alarm/4,handling_fire/1,alarm_off/4]).
 
-define(SERVER, ?MODULE).
 
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
start(Radius,X,Y) ->
    gen_fsm:start({local, ?SERVER}, ?MODULE, [Radius,X,Y], []).
 
check_alarm(Name,X,Y,R) ->
  gen_fsm:send_event(?SERVER, {check_alarm,Name,X,Y,R}).
  
handling_fire(Name) ->
  gen_fsm:send_event(?SERVER, {handling_fire,Name}).
  
alarm_off(Name,X,Y,R) ->
  gen_fsm:send_event(?SERVER, {alarm_off,Name,X,Y,R}).
 
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
init([StartRadius,X,Y]) ->
	ets:new(sensdata,[set,named_table]),
	ets:insert(sensdata,{radius,StartRadius}),
	ets:insert(sensdata,{x,X}),
	ets:insert(sensdata,{y,Y}),
	ets:insert(sensdata,{fires,[]}),
	io:format("started sensor at (~p,~p) with radius = ~p~n",[X,Y,StartRadius]),
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

idle({check_alarm,FireName,FireX,FireY,FireRad}, State) ->
	[{_,Radius}] = ets:lookup(sensdata,radius),
	[{_,X}] = ets:lookup(sensdata,x),
	[{_,Y}] = ets:lookup(sensdata,y),
	Dist = math:sqrt( (X-FireX) * (X-FireX) + (Y-FireY) * (Y-FireY)),
	[{_,FireList}] = ets:lookup(sensdata,fires),
	case (Dist < Radius + FireRad) and (lists:member(FireName,FireList)==false)  of
		true -> io:format("request heli~n");
		false -> io:format("alarm isn't activated or alarm already on~n")
	end,
	{next_state, idle, State};
	
idle({handling_fire,FireName}, State) ->
	add_to_ets(fires,FireName),
	{next_state, idle, State};
	
idle({alarm_off,FireName,FireX,FireY,FireRad}, State) ->
	[{_,Radius}] = ets:lookup(sensdata,radius),
	[{_,X}] = ets:lookup(sensdata,x),
	[{_,Y}] = ets:lookup(sensdata,y),
	Dist = math:sqrt( (X-FireX) * (X-FireX) + (Y-FireY) * (Y-FireY)),
	case (Dist > Radius + FireRad) of
		true -> delete_from_ets(fires,FireName);
		false -> io:format("fire still in sensor area~n")
	end,
	{next_state, idle, State};

idle(_Event, State) ->
  {next_state, idle, State}.
 
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
	[{_,List}] = ets:lookup(sensdata,KeyName),
	NewList = [ Val || Val <-List, Val /= Value],
	ets:insert(sensdata,{KeyName,NewList}).
	
add_to_ets(KeyName,Value) ->
	[{_,List}] = ets:lookup(sensdata,KeyName),
	case lists:member(Value,List) of
		false -> ets:insert(sensdata,{KeyName,[Value|List]});
		true -> already_exists
	end.