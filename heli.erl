-module(heli).
 
-behaviour(gen_fsm).
 
%% API
-export([start/4]).
 
%% gen_fsm callbacks
-export([init/1,idle/2,idle/3,move_destination/2,move_destination/3, handle_event/3,
		search_circle/2, search_circle/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
-export([start_sim/1,move_dst/4,move_circle/2]).
 
%%-define(SERVER, ?MODULE).
-define(MAXX, 1200).
-define(MINX, 200).
-define(MAXY, 556).
-define(MINY, 200).
 
%-record(state, {hor,ver}).
 
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
start(Name,ServerName,X,Y) ->
    gen_fsm:start({global, Name}, ?MODULE, [Name,ServerName,X,Y], []).
 
start_sim(Name) ->
  gen_fsm:send_event({global,Name}, {idle_move}).
  
move_dst(Name,X,Y,Objective) ->
  gen_fsm:send_event({global,Name}, {move_dst, X, Y,Objective}).
  
move_circle(Name,R) ->
  gen_fsm:send_event({global,Name}, {circle, R}).
 
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
init([Name,ServerName,X,Y]) ->
	%process_flag(trap_exit, true),
    Ets = ets:new(cord,[set]),
    ets:insert(Ets,{x,X}),
    ets:insert(Ets,{y,Y}),
    ets:insert(Ets,{myName,Name}),
    ets:insert(Ets,{serverName,ServerName}),
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
idle({idle_move}, Ets) ->
  random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),
  %ets:insert(cord,{movetime,Time}),	
  ets:insert(Ets,{xdif,(random:uniform() * 2 - 1)*10}),
  ets:insert(Ets,{ydif,(random:uniform() * 2 - 1)*10}),
  {next_state, idle, Ets,100};

idle(timeout, Ets) ->
  idle_move(Ets),
  [{_,MyName}] = ets:lookup(Ets,myName),
  [{_,ServerName}] = ets:lookup(Ets,serverName),
  [{_,CurrentX}] = ets:lookup(Ets,x),
  [{_,CurrentY}] = ets:lookup(Ets,y),
  unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY,idle]),
  %[{_,CurrentTime}] = ets:lookup(cord,movetime),
  %case CurrentTime =< 0 of
	%true -> io:format("TIME OVER~n"),{next_state, idle, State};
	%false -> ets:insert(cord,{movetime,CurrentTime-1}),
	%	 {next_state, idle, State,100}
  %end;
  {next_state, idle, Ets,100};

idle({move_dst,DstX,DstY,Objective},Ets) ->
	io:format("moving to dst = (~p,~p)~n",[DstX,DstY]),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	case (DstX - CurrentX)/= 0 of
		true -> M=(DstY-CurrentY) / (DstX - CurrentX),
				N = DstY - M * DstX;
		false -> M=inf, N=0
	end,
	{next_state,move_destination,{M,N,DstX,DstY,Objective,Ets},100};
	
idle({circle,R},Ets) ->
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	CX = CurrentX - R,
	CY = CurrentY,
	{next_state,search_circle,{R,CX,CY,0,Ets},100};
  
idle(_Event, Ets) ->
  {next_state, idle, Ets}.
  
move_destination(timeout,{M,N,DstX,DstY,Objective,Ets}) -> 
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY,move_destination]),
	Arrived = step_dest(CurrentX,CurrentY,M,N,DstX,DstY,Ets),
	case Arrived of
		true -> io:format("arrive to objective: ~p and starting circle~n",[Objective]), %% if only searching fire, then remove objective
				{CR,CX,CY,A} = Objective,
				{next_state,search_circle,{CR,CX,CY,A,Ets},100};
		false -> {next_state,move_destination,{M,N,DstX,DstY,Objective,Ets},100}
	end;
	
move_destination(_Event, Ets) ->
  {next_state, move_destination, Ets,100}.
  

search_circle(timeout,{R,CX,CY,Angle,Ets}) -> 
	%[{_,CurrentX}] = ets:lookup(cord,x),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY,search_circle]),
	step_circle(CX,CY,R,Angle,Ets),
	case Angle == 360 of 
		true -> io:format("finished circle~n"),
				random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),
			    ets:insert(Ets,{xdif,(random:uniform() * 2 - 1)*10}),
			    ets:insert(Ets,{ydif,(random:uniform() * 2 - 1)*10}),
				{next_state,idle,Ets,100};
		false -> {next_state,search_circle,{R,CX,CY,Angle + 1,Ets},100}
	end;	

	
search_circle(_Event, Ets) ->
  {next_state, move_destination, Ets}.
 
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
idle(_Event, _From, Ets) ->
  Reply = {error, invalid_message},
  {reply, Reply, idle, Ets}.
  
move_destination(_Event, _From, Ets) ->
  Reply = {error, invalid_message},
  {reply, Reply, move_destination, Ets}.
  
search_circle(_Event, _From, Ets) ->
  Reply = {error, invalid_message},
  {reply, Reply, search_circle, Ets}.
 
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
handle_event(_Event, StateName, Ets) ->
    {next_state, StateName, Ets}.
 
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
handle_sync_event(_Event, _From, StateName, Ets) ->
    Reply = ok,
    {reply, Reply, StateName, Ets}.
 
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
handle_info(_Info, StateName, Ets) ->
    {next_state, StateName, Ets}.
 
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
terminate(_Reason, _StateName, _Ets) ->
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
code_change(_OldVsn, StateName, Ets, _Extra) ->
    {ok, StateName, Ets}.
 
%%%===================================================================
%%% Internal functions
%%%===================================================================

idle_move(Ets) ->
  [{_,DifX}] = ets:lookup(Ets,xdif),
  [{_,DifY}] = ets:lookup(Ets,ydif),
  [{_,CurrentX}] = ets:lookup(Ets,x),
  NewX = CurrentX+DifX,
  [{_,CurrentY}] = ets:lookup(Ets,y),
  NewY = CurrentY+DifY,
  case DifX > 0 of
	true -> 
		case ?MAXX - NewX < 1 of
			true -> ets:insert(Ets,{xdif,-1*DifX});
			false -> same_dir
		end;
	false ->
		case NewX - ?MINX < 1 of
			true -> ets:insert(Ets,{xdif,-1*DifX});
			false -> same_dir
		end
  end,
  case DifY > 0 of
	true -> 
		case ?MAXY - NewY < 1 of
			true -> ets:insert(Ets,{ydif,-1*DifY});
			false -> same_dir
		end;
	false -> 
		case NewY - ?MINY < 1 of
			true -> ets:insert(Ets,{ydif,-1*DifY});
			false -> same_dir
		end
  end,
  ets:insert(Ets,{x,NewX}),
  ets:insert(Ets,{y,NewY}).
  %io:format("new (x,y) = (~p,~p)~n",[NewX,NewY]).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_dest(X,Y,M,N,DstX,DstY,Ets) -> 
	case M /= inf of	
		true ->case M =< 1 of
				true -> case abs(DstX-X) < 1 of
							true -> Step = abs(DstX-X);
							false -> Step = 1
						end,
						case DstX > X of
							true -> NewX = X + Step,
									NewY = M*(X+Step) +N;
							false -> NewX = X - Step,
									 NewY = M*(X-Step) +N
						end;
				false -> case abs(DstY-Y) < 1 of
							true -> Step = abs(DstY-Y);
							false -> Step = 1
						 end,
						 case DstY > Y of
							true -> NewX = (Y+Step-N)/M,
									NewY = Y + Step;
							false -> NewX = (Y-Step-N)/M,
									 NewY = Y - Step
						 end
			end;
		false -> case abs(DstY-Y) < 1 of
					true -> Step = abs(DstY-Y);
					false -> Step = 1
				 end,
				 case DstY > Y of
					true -> NewY = Y + Step;
					false -> NewY = Y - Step
				 end,
				 NewX = X
	end,
	ets:insert(Ets,{x,NewX}),
	ets:insert(Ets,{y,NewY}),
	io:format("new (x,y) = (~p,~p)~n",[NewX,NewY]),
	((abs(DstX - X) < 0.000001) and (abs(DstY - Y) < 0.000001)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_circle(CX,CY,R,Angle,Ets) -> 
	NewX = math:cos(Angle * math:pi() / 180) * R + CX,
	NewY = math:sin(Angle * math:pi() / 180) *R + CY,
	ets:insert(Ets,{x,NewX}),
	ets:insert(Ets,{y,NewY}),
	%Debug = math:sqrt( (NewX-CX) * (NewX-CX) + (NewY-CY) * (NewY-CY)),
	io:format("new (x,y) = (~p,~p)~n",[NewX,NewY]). %distance from circle = ~p~n",[NewX,NewY,Debug])