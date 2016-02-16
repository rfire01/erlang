-module(heli).
 
-behaviour(gen_fsm).
 
%% API
-export([start/4]).
 
%% gen_fsm callbacks
-export([init/1,idle/2,idle/3,move_destination/2,move_destination/3, handle_event/3,
		search_circle/2, search_circle/3,extinguish/2,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
-export([start_sim/1,move_dst/4,move_circle/2]).

-export([calc_destination_movement_diffs/4]).
 
%%-define(SERVER, ?MODULE).
-define(MAXX, 1200).
-define(MINX, 200).
-define(MAXY, 556).
-define(MINY, 200).

-define(MOVEMENT_SPEED,50).
-define(REFRESH_SPEED,10).
-define(EXTINGUISH_SPEED,90).

 
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

  Xdif = ((random:uniform() * 2 - 1) * ?MOVEMENT_SPEED),
  Dir = random:uniform(2),
  case Dir of
	1 -> Ydif = -1 * math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif);
	2 -> Ydif = math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif)
  end,
  
  DifX = Xdif * (?REFRESH_SPEED / 1000),
  DifY = Ydif * (?REFRESH_SPEED / 1000),
  %ets:insert(Ets,{xdif,Xdif * (?REFRESH_SPEED / 1000)}),
  %ets:insert(Ets,{ydif,Ydif * (?REFRESH_SPEED / 1000)}),
  {next_state, idle, {DifX,DifY,Ets},?REFRESH_SPEED};


idle(timeout, {DifX,DifY,Ets}) ->
  idle_move(DifX,DifY,Ets),
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

  {next_state, idle, {DifX,DifY,Ets},?REFRESH_SPEED};

idle({move_dst,DstX,DstY,Objective},{_,_,Ets}) ->
	io:format("moving to dst = (~p,~p)~n",[DstX,DstY]),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	%case (DstX - CurrentX)/= 0 of
	%	true -> M=(DstY-CurrentY) / (DstX - CurrentX),
	%			N = DstY - M * DstX;
	%	false -> M=inf, N=0
	%end,
	io:format("(x,y) = (~p,~p) ; (dstx,dsty) = (~p,~p)~n",[CurrentX,CurrentY,DstX,DstY]),
	{DifX,DifY} = calc_destination_movement_diffs(CurrentX,CurrentY,DstX,DstY),
	{next_state,move_destination,{DifX,DifY,DstX,DstY,Objective,Ets},?REFRESH_SPEED};

	
idle({circle,R},Ets) ->
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	CX = CurrentX - R,
	CY = CurrentY,
	{next_state,search_circle,{R,CX,CY,0,Ets},?REFRESH_SPEED};
  
idle(_Event, Ets) ->
  {next_state, idle, Ets}.
  
move_destination(timeout,{DifX,DifY,DstX,DstY,Objective,Ets}) -> 
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	
	Arrived = step_dest(CurrentX,CurrentY,DstX,DstY,DifX,DifY,Ets),
	unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY,move_destination]),
	case Arrived of
		true -> io:format("arrive to objective: ~p and starting circle~n",[Objective]), %% if only searching fire, then remove objective
				{CR,CX,CY,A} = Objective,

				{next_state,search_circle,{CR,CX,CY,A,Ets},?REFRESH_SPEED};
		false -> {next_state,move_destination,{DifX,DifY,DstX,DstY,Objective,Ets},?REFRESH_SPEED}
	end;
	
move_destination(_Event, Ets) ->
  {next_state, move_destination, Ets,?REFRESH_SPEED}.
  

search_circle(timeout,{R,CX,CY,Angle,Ets}) -> 
	%[{_,CurrentX}] = ets:lookup(cord,x),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY,search_circle]),
	step_circle(CX,CY,R,Angle,Ets),
	
	case gen_server:call({global,ServerName},{heli_fire_check,MyName}) of
		false -> 
		    case Angle == 360 of 
			true -> io:format("finished circle~n"),
				{DifX,DifY} = rand_idle_diff(),
				{next_state,idle,{DifX,DifY,Ets},?REFRESH_SPEED};
			false -> {next_state,search_circle,{R,CX,CY,Angle + 1,Ets},?REFRESH_SPEED}
		    end;
		    
		[NF,RF,XF,YF] -> io:format("found fire [~p,~p,~p,~p]~n",[NF,RF,XF,YF]),
			      {next_state,extinguish,{NF,RF,XF,YF,Ets},?EXTINGUISH_SPEED}
	end;
		

	
search_circle(_Event, Ets) ->
  {next_state, move_destination, Ets}.
  
extinguish(timeout,{N,R,X,Y,Ets}) -> 
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY,extinguish]),
	case fire:extinguish_fire(N)of
	    fire_alive-> {next_state,extinguish,{N,R,X,Y,Ets},?EXTINGUISH_SPEED};
	    fire_dead->  {DifX,DifY} = rand_idle_diff(),
			  io:format("fire_dead~n"),{next_state, idle, {DifX,DifY,Ets},?REFRESH_SPEED}
	end;
	
extinguish(_Event, Ets) ->
  {next_state, extinguish, Ets}.

 
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
  
extinguish(_Event, _From, Ets) ->
  Reply = {error, invalid_message},
  {reply, Reply, extinguish, Ets}.
 
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

idle_move(DifX,DifY,Ets) ->
  %[{_,DifX}] = ets:lookup(Ets,xdif),
  %[{_,DifY}] = ets:lookup(Ets,ydif),
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

calc_destination_movement_diffs(X,Y,DstX,DstY) ->
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
	Travel_time = S / ?MOVEMENT_SPEED,
	Ticks_required = 1000 * Travel_time / ?REFRESH_SPEED,
	Dif = S / Ticks_required,
	io:format("dif = ~p, angle = ~p~n",[Dif,Angle/math:pi()*180]),
	DifX = -1* Dif * math:cos(Angle),
	DifY = -1 * Dif * math:sin(Angle),
	io:format("distance = ~p, difs = ~p~n",[S,{DifX,DifY}]),
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
				Res = true, io:format("arrived");
		false -> NewX = X+DifX,
				 NewY = Y+DifY,
				 Res = false
	end,
	ets:insert(Ets,{x,NewX}),
	ets:insert(Ets,{y,NewY}),
	Res.

%step_dest(X,Y,M,N,DstX,DstY,Ets) -> 
%	case M /= inf of	
%		true ->case M =< 1 of
%				true -> case abs(DstX-X) < 1 of
%							true -> Step = abs(DstX-X);
%							false -> Step = 1
%						end,
%						case DstX > X of
%							true -> NewX = X + Step,
%									NewY = M*(X+Step) +N;
%							false -> NewX = X - Step,
%									 NewY = M*(X-Step) +N
%						end;
%				false -> case abs(DstY-Y) < 1 of
%							true -> Step = abs(DstY-Y);
%							false -> Step = 1
%						 end,
%						 case DstY > Y of
%							true -> NewX = (Y+Step-N)/M,
%									NewY = Y + Step;
%							false -> NewX = (Y-Step-N)/M,
%									 NewY = Y - Step
%						 end
%			end;
%		false -> case abs(DstY-Y) < 1 of
%					true -> Step = abs(DstY-Y);
%%					false -> Step = 1
	%			 end,
	%			 case DstY > Y of
	%				true -> NewY = Y + Step;
	%				false -> NewY = Y - Step
	%			 end,
	%			 NewX = X
%	end,
%	ets:insert(Ets,{x,NewX}),
%	ets:insert(Ets,{y,NewY}),
%	io:format("new (x,y) = (~p,~p)~n",[NewX,NewY]),
%	((abs(DstX - X) < 0.000001) and (abs(DstY - Y) < 0.000001)).
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_circle(CX,CY,R,Angle,Ets) -> 
	NewX = math:cos(Angle * math:pi() / 180) * R + CX,
	NewY = math:sin(Angle * math:pi() / 180) *R + CY,
	ets:insert(Ets,{x,NewX}),
	ets:insert(Ets,{y,NewY}).
	%Debug = math:sqrt( (NewX-CX) * (NewX-CX) + (NewY-CY) * (NewY-CY)),
	%io:format("new (x,y) = (~p,~p)~n",[NewX,NewY]). %distance from circle = ~p~n",[NewX,NewY,Debug])
	
	
%%%%

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
