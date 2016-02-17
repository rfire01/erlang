-module(heli).
 
-behaviour(gen_fsm).
 
%% API
-export([start/4]).
 
%% gen_fsm callbacks
-export([init/1,idle/2,idle/3,move_destination/2,move_destination/3, handle_event/3,
		search_circle/2, search_circle/3,extinguish/2,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
-export([start_sim/1,move_dst/4,move_circle/2]).
-include("config.hrl").
%%-define(SERVER, ?MODULE).
% -define(MAXX, 1200-25).
% -define(MINX, 0-25).
% -define(MAXY, 556-25).
% -define(MINY, 0-25).
% 
% -define(MOVEMENT_SPEED,100).
% -define(REFRESH_SPEED,10).
% -define(EXTINGUISH_SPEED,40).

 
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
	put(ets_id,Ets),
    ets:insert(Ets,{x,X}),
    ets:insert(Ets,{y,Y}),
	ets:insert(Ets,{myName,Name}),
    ets:insert(Ets,{serverName,ServerName}),
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
idle({idle_move}, _State) ->
  random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),

  Xdif = ((random:uniform() * 2 - 1) * ?MOVEMENT_SPEED),
  Dir = random:uniform(2),
  case Dir of
	1 -> Ydif = -1 * math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif);
	2 -> Ydif = math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif)
  end,
  
  DifX = Xdif * (?REFRESH_SPEED / 1000),
  DifY = Ydif * (?REFRESH_SPEED / 1000),
  {next_state, idle, {DifX,DifY},?REFRESH_SPEED};



idle(timeout, {DifX,DifY}) ->
  Ets = get(ets_id),
  {NewDifX,NewDifY}=move_dif(DifX,DifY,Ets),

  [{_,MyName}] = ets:lookup(Ets,myName),
  [{_,ServerName}] = ets:lookup(Ets,serverName),
  [{_,CurrentX}] = ets:lookup(Ets,x),
  [{_,CurrentY}] = ets:lookup(Ets,y),
  unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
  %[{_,CurrentTime}] = ets:lookup(cord,movetime),
  %case CurrentTime =< 0 of
	%true -> io:format("TIME OVER~n"),{next_state, idle, State};
	%false -> ets:insert(cord,{movetime,CurrentTime-1}),
	%	 {next_state, idle, State,100}
  %end;


  {next_state, idle, {NewDifX,NewDifY},?REFRESH_SPEED};

idle({move_dst,DstX,DstY,Objective},_State) ->
	Ets = get(ets_id),
	io:format("moving to dst = (~p,~p)~n",[DstX,DstY]),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	
	io:format("(x,y) = (~p,~p) ; (dstx,dsty) = (~p,~p)~n",[CurrentX,CurrentY,DstX,DstY]),
	Angle = calc_destination_angle(CurrentX,CurrentY,DstX,DstY),
	{DifX,DifY} = calc_destination_movement_diffs(Angle),
	{next_state,move_destination,{DifX,DifY,DstX,DstY,Objective},?REFRESH_SPEED};
	
idle({circle,R},_State) ->
	Ets = get(ets_id),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	CX = CurrentX - R,
	CY = CurrentY,
	{next_state,search_circle,{R,CX,CY,0},?REFRESH_SPEED};
  
idle(_Event, State) ->
  {next_state, idle, State}.
  
move_destination(timeout,{DifX,DifY,DstX,DstY,Objective}) -> 
	Ets = get(ets_id),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	
	Arrived = step_dest(CurrentX,CurrentY,DstX,DstY,DifX,DifY,Ets),
	unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
	case Arrived of
		true -> %io:format("arrive to objective: ~p and starting circle~n",[Objective]), %% if only searching fire, then remove objective
				{CR,CX,CY,A} = Objective,
				DifAngle = calc_angle_diff(CR),
				{next_state,search_circle,{CR,CX,CY,A,DifAngle},?REFRESH_SPEED};
		false -> {next_state,move_destination,{DifX,DifY,DstX,DstY,Objective},?REFRESH_SPEED}
	end;
	
move_destination(_Event, State) ->
  {next_state, move_destination, State,?REFRESH_SPEED}.
  

search_circle(timeout,{R,CX,CY,Angle,DifAngle}) -> 
	Ets = get(ets_id),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
	step_circle(CX,CY,R,Angle,Ets),
	
	%io:format("new angle = ~p~n", [Angle]),
	
	case gen_server:call({global,ServerName},{heli_fire_check,MyName}) of
		false -> 
				case Angle > 6.29 of 
					true -> io:format("finished circle~n"),
							{DifX,DifY} = rand_idle_diff(),
							{next_state,idle,{DifX,DifY},?REFRESH_SPEED};
					false -> {next_state,search_circle,{R,CX,CY,Angle + DifAngle,DifAngle},?REFRESH_SPEED}
				end;
		    
		[NF,RF,XF,YF] -> io:format("found fire [~p,~p,~p,~p]~n",[NF,RF,XF,YF]),
						StartAngle = calc_destination_angle(CurrentX,CurrentY,XF,YF),
						FrameX = RF * math:cos(StartAngle)+ XF,
						FrameY = RF * math:sin(StartAngle)+ YF,
						case check_frame(CurrentX,CurrentY,FrameX,FrameY) of
							true -> {next_state,extinguish,{circle,NF,RF,XF,YF,StartAngle},?EXTINGUISH_SPEED};
							false -> {DifX,DifY} = calc_destination_movement_diffs(StartAngle),
									 {next_state,extinguish,{straight,-1*DifX,-1*DifY,NF,XF,YF,StartAngle},?EXTINGUISH_SPEED}
						end
	end;
		

	
search_circle(_Event, State) ->
  {next_state, move_destination, State}.

extinguish(timeout,{circle,N,R,X,Y,Angle}) -> 
	Ets = get(ets_id),
	step_circle(X,Y,R,Angle,Ets),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
	{FireState,NewR} = fire:extinguish_fire(N),
	DifAngle = calc_angle_diff(R),
	
	case FireState of
	    fire_alive-> {next_state,extinguish,{circle,N,NewR,X,Y,Angle + DifAngle},?EXTINGUISH_SPEED};
	    fire_dead->  {DifX,DifY} = rand_idle_diff(),
					  io:format("fire_dead~n"),
					  unit_server:heli_done(ServerName,MyName),
					  {next_state, idle, {DifX,DifY},?REFRESH_SPEED}
	end;
	
	
  
extinguish(timeout,{straight,DifX,DifY,NF,XF,YF,Angle}) -> 
	Ets = get(ets_id),
	move_dif(DifX,DifY,Ets),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
	{FireState,NewR} = fire:extinguish_fire(NF),
	
	FrameX = NewR * math:cos(Angle)+ XF,
	FrameY = NewR * math:sin(Angle) + YF,
	
	case FireState of
	    fire_alive-> case check_frame(CurrentX,CurrentY,FrameX,FrameY) of
						true -> {next_state,extinguish,{circle,NF,NewR,XF,YF,Angle},?EXTINGUISH_SPEED};
						false -> {next_state,extinguish,{straight,DifX,DifY,NF,XF,YF,Angle},?EXTINGUISH_SPEED}
					 end;
	    fire_dead->  {DifX,DifY} = rand_idle_diff(),
					  io:format("fire_dead~n"),
					  unit_server:heli_done(ServerName,MyName),
					  {next_state, idle, {DifX,DifY},?REFRESH_SPEED}
	end;
	
extinguish(_Event, State) ->
  {next_state, extinguish, State}.

 
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
  
move_destination(_Event, _From, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, move_destination, State}.
  
search_circle(_Event, _From, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, search_circle, State}.
 
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

move_dif(DifX,DifY,Ets) ->
  %[{_,DifX}] = ets:lookup(Ets,xdif),
  %[{_,DifY}] = ets:lookup(Ets,ydif),
  [{_,CurrentX}] = ets:lookup(Ets,x),
  NewX = CurrentX+DifX,
  [{_,CurrentY}] = ets:lookup(Ets,y),
  NewY = CurrentY+DifY,
  case DifX > 0 of
	true -> 
		case ?MAXX - NewX < 1 of
			true -> NewDifX=-1*DifX;
			false -> NewDifX=DifX
		end;
	false ->
		case NewX - ?MINX < 1 of
			true -> NewDifX=-1*DifX;
			false -> NewDifX=DifX
		end
  end,
  case DifY > 0 of
	true -> 
		case ?MAXY - NewY < 1 of
			true -> NewDifY=-1*DifY;
			false -> NewDifY=DifY
		end;
	false -> 
		case NewY - ?MINY < 1 of
			true -> NewDifY=-1*DifY;
			false -> NewDifY=DifY
		end
  end,
  ets:insert(Ets,{x,NewX}),
  ets:insert(Ets,{y,NewY}),
  {NewDifX,NewDifY}.
  %io:format("new (x,y) = (~p,~p)~n",[NewX,NewY]).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calc_destination_angle(X,Y,DstX,DstY) ->
	DeltaX = X - DstX,
	DeltaY = Y - DstY,
	S = math:sqrt(DeltaX * DeltaX + DeltaY * DeltaY),
	case X> DstX of
		true -> case Y > DstY of
					true -> Angle = math:asin((Y-DstY)/S),io:format("I~n");
					false -> Angle = (math:pi()/2 -math:asin((DstY-Y)/S)) + math:pi() * 3/2,io:format("IV~n")
				end;
		false -> case Y > DstY of
					true -> Angle = (math:pi()/2 -math:asin((Y-DstY)/S)) + math:pi() / 2,io:format("II~n");
					false -> Angle = math:asin((DstY-Y)/S) + math:pi(),io:format("III~n")
				 end
	end,
	
	Angle.
	
calc_destination_movement_diffs(Angle) -> 
	%Travel_time = S / ?MOVEMENT_SPEED,
	%Ticks_required = 1000 * Travel_time / ?REFRESH_SPEED,
	%Dif = S / Ticks_required,
	Dif = ?MOVEMENT_SPEED * ?REFRESH_SPEED / 1000,
	DifX = -1* Dif * math:cos(Angle),
	DifY = -1 * Dif * math:sin(Angle),
	{DifX,DifY}.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_dest(X,Y,DstX,DstY,DifX,DifY,Ets) -> 
	%[{_,DifX}] = ets:lookup(Ets,xdif),
	%[{_,DifY}] = ets:lookup(Ets,ydif),
	%io:format("~ndifs = ~p~n~n",[{DifX,DifY}]),
	Dist = (X-DstX) * (X-DstX) + (Y-DstY) * (Y-DstY),
	Diff = DifX * DifX + DifY * DifY,
	case Dist < Diff of
		true -> NewX = DstX,
				NewY = DstY,
				Res = true;
		false -> NewX = X+DifX,
				 NewY = Y+DifY,
				 Res = false
	end,
	ets:insert(Ets,{x,NewX}),
	ets:insert(Ets,{y,NewY}),
	Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calc_angle_diff(R) ->
	S = 2 * math:pi() * R,
	Travel_time = S / ?MOVEMENT_SPEED,
	Ticks_required = Travel_time * 1000 / ?REFRESH_SPEED,
	Dif = S / Ticks_required,
	Delta_Angle = Dif / R,
	%io:format("diff angle = ~p~n", [Delta_Angle]),
	Delta_Angle.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_circle(CX,CY,R,Angle,Ets) -> 
	NewX = math:cos(Angle) * R + CX,
	NewY = math:sin(Angle) *R + CY,
	ets:insert(Ets,{x,NewX}),
	ets:insert(Ets,{y,NewY}).
	%Debug = math:sqrt( (NewX-CX) * (NewX-CX) + (NewY-CY) * (NewY-CY)),
	%io:format("new (x,y) = (~p,~p)~n",[NewX,NewY]). %distance from circle = ~p~n",[NewX,NewY,Debug])
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rand_idle_diff()->
  random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),

  Xdif = ((random:uniform() * 2 - 1) * ?MOVEMENT_SPEED),
  Dir = random:uniform(2),
  case Dir of
	1 -> Ydif = -1 * math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif);
	2 -> Ydif = math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif)
  end,
  
  DifX = Xdif * (?REFRESH_SPEED / 1000),
  DifY = Ydif * (?REFRESH_SPEED / 1000),
  {DifX,DifY}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_frame(HX,HY,FrameX,FrameY) -> 
	%io:format("(HX,HY,FX,FY) = ~p~n",[{HX,HY,FrameX,FrameY}]),
	%io:format("distance from frame = ~p~n",[((HX-FrameX) * (HX-FrameX) + (HY-FrameY) * (HY-FrameY))]),
	((HX-FrameX) * (HX-FrameX) + (HY-FrameY) * (HY-FrameY)) <16.
