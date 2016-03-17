-module(heli).
 
-behaviour(gen_fsm).
 
%% API
-export([start/4,stop/1]).
 
%% gen_fsm callbacks
-export([init/1,idle/2,idle/3,move_destination/2,move_destination/3, handle_event/3,
		search_circle/2, search_circle/3,extinguish/2,recover/7,found_fire/2,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
	 crash/1,crash_recover/2]).
 
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
 
 -export([statistics/1]).
 
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
start(Name,ServerName,X,Y) ->
    gen_fsm:start({global, Name}, ?MODULE, [Name,ServerName,X,Y], []).
	
%%start move  
start_sim(Name) ->
  gen_fsm:send_event({global,Name}, {idle_move}).
%% order heli to move to new x y  
move_dst(Name,X,Y,Objective) ->
  gen_fsm:send_event({global,Name}, {move_dst, X, Y,Objective}).
 
%% take radios and turn around 
move_circle(Name,R) ->
  gen_fsm:send_event({global,Name}, {circle, R}).

%%when the unit is crash then make recover  
recover(Name,ServerName,X,Y,State,Data,Stat) ->
  gen_fsm:start({global, Name}, ?MODULE, [Name,ServerName,X,Y,State,Data,Stat], []).

%%when heli scen for fire on the sensor it get found fire from unit server  
found_fire(Name,[NF,RF,XF,YF]) -> 
	gen_fsm:send_event({global,Name}, {found_fire, [NF,RF,XF,YF]}).
	
%%use for test only - kill the procc	
crash(Name) ->
	gen_fsm:send_all_state_event({global, Name}, {crash,0}).	
	
%%seq for kill->rebulid from start	
crash_recover(Name,Data) ->
    gen_fsm:start({global, Name}, ?MODULE, [crash,Data], []).
	
	%%exit and kill
stop(Name) ->
	gen_fsm:send_all_state_event({global, Name}, {stop}).  
	
	%%for stat in the new window
statistics(Name) ->
	gen_fsm:send_all_state_event({global, Name}, {stat}).  
 
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
%%standet init in all gen fsm take name and new xy - first time
init([Name,ServerName,X,Y]) ->
	%%%%Monitor hundle
	MonName = list_to_atom(atom_to_list(ServerName) ++ "mon"),
	MonPid = global:whereis_name(MonName),
	
	%%save all the main date in the DB
	%process_flag(trap_exit, true),
    Ets = ets:new(cord,[set,{heir,MonPid , {heli,Name}}]),
	put(ets_id,Ets),
    ets:insert(Ets,{x,X}),
    ets:insert(Ets,{y,Y}),
	ets:insert(Ets,{myName,Name}),
    ets:insert(Ets,{serverName,ServerName}),
	%%init statistics
	create_stat(),
	
    {ok, idle, {}};
	
%%standet init in all gen fsm take name and new xy - secend time when it have statistics	
init([Name,ServerName,X,Y,State,Data,Stat]) ->
	%%%Monitor hundle
	MonName = list_to_atom(atom_to_list(ServerName) ++ "mon"),
	MonPid = global:whereis_name(MonName),

	%%save all the main date in the DB
	%process_flag(trap_exit, true),
	%io:format("recovering heli ~p at ~p~n",[Name,ServerName]),
    Ets = ets:new(cord,[set,{heir,MonPid , {heli,Name}}]),
	put(ets_id,Ets),
    ets:insert(Ets,{x,X}),
    ets:insert(Ets,{y,Y}),
	ets:insert(Ets,{myName,Name}),
    ets:insert(Ets,{serverName,ServerName}),
	%%init statistics
	create_stat(Stat),
	
	%%choose the speed for normal move to extinguish move
	case State of
		extinguish -> {ok, State, Data,?EXTINGUISH_SPEED};
		_Any -> {ok, State, Data,?REFRESH_SPEED}
	end;
	
	%%for test onlt - kill the procc
init([crash,Data]) ->
	%%make copy of DB
	Ets=ets:new(cord,[set]),
	put(ets_id,Ets),
	ets:insert(Ets,Data),
	[{_,Name}] = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	
	%%%Monitor hundle
	MonName = list_to_atom(atom_to_list(ServerName) ++ "mon"),
	MonPid = global:whereis_name(MonName),
	%%%Monitor hundle
	ets:setopts(Ets,{heir,MonPid , {heli,Name}}),
	loc_monitor:add_mon(MonName,global:whereis_name(Name)),
	%%%recover new heli
	heli:start_sim(Name),
	unit_server:heli_done(ServerName,Name),
	%%init statistics
	create_stat(),
	
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
%%start sim move like DVD
idle({idle_move}, _State) ->
  %Tmp = erlang:system_time() / erlang:monotonic_time(),
  %Time = erlang:round((Tmp - erlang:trunc(Tmp)) * 100000000),
  %random:seed(Time,erlang:monotonic_time(),erlang:unique_integer()),
  Pid = getPid(self()),
  {A,_B,C}=erlang:now(),%%use for random in otp 17
  random:seed(C,Pid*Pid,erlang:round(C/A)),
  %%random angle to move
  Xdif = ((random:uniform() * 2 - 1) * ?MOVEMENT_SPEED),
  Dir = random:uniform(2),
  case Dir of
	1 -> Ydif = -1 * math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif);
	2 -> Ydif = math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif)
  end,
  %%calc in the next clock tic the next move length 
  DifX = Xdif * (?REFRESH_SPEED / 1000),
  DifY = Ydif * (?REFRESH_SPEED / 1000),
  {next_state, idle, {DifX,DifY},?REFRESH_SPEED};


%%jmp to the new loction when clock tik is on
idle(timeout, {DifX,DifY}) ->
  Ets = get(ets_id),
  
  {NewDifX,NewDifY}=move_dif(DifX,DifY,Ets),

  [{_,MyName}] = ets:lookup(Ets,myName),
  [{_,ServerName}] = ets:lookup(Ets,serverName),
  [{_,CurrentX}] = ets:lookup(Ets,x),
  [{_,CurrentY}] = ets:lookup(Ets,y),
  %%uptate the statistics that we move to new loction
  update_stat(travel,math:sqrt(DifX*DifX+DifY*DifY)),
 %%ck if the heli is need to pass to other server then do move seq
  case check_screen(CurrentX,CurrentY) of
	ServerName -> unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
				  {next_state, idle, {NewDifX,NewDifY},?REFRESH_SPEED};
	OtherServer -> wait_for_server_recover(ServerName,OtherServer,[MyName,CurrentX,CurrentY],idle,{NewDifX,NewDifY})
					%unit_server:change_screen(ServerName,OtherServer,[MyName,CurrentX,CurrentY],idle,{NewDifX,NewDifY}),
				   %{stop,normal,{}}
				   
  end;
%%jmp to the new loction when clock tik is on in the way to sensor
idle({move_dst,DstX,DstY,Objective},_State) ->
	Ets = get(ets_id),
	%io:format("moving to dst = (~p,~p)~n",[DstX,DstY]),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	%%calc the angle and distance to the active sensor
	%io:format("(x,y) = (~p,~p) ; (dstx,dsty) = (~p,~p)~n",[CurrentX,CurrentY,DstX,DstY]),
	Angle = calc_destination_angle(CurrentX,CurrentY,DstX,DstY),
	{DifX,DifY} = calc_destination_movement_diffs(Angle),
	%%uptate the statistics that we move to new loction
	update_stat(current_work_time,erlang:now()),
	
	{next_state,move_destination,{DifX,DifY,DstX,DstY,Objective},?REFRESH_SPEED};
	%%when you get to the active sensor move to the start point on the radios
idle({circle,R},_State) ->
	Ets = get(ets_id),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	CX = CurrentX - R,
	CY = CurrentY,
	{next_state,search_circle,{R,CX,CY,0},?REFRESH_SPEED};
  
idle(_Event, State) ->
  {next_state, idle, State,?REFRESH_SPEED}.
 
%%now the heli in stat move to active sensor 
move_destination(timeout,{DifX,DifY,DstX,DstY,Objective}) -> 
	Ets = get(ets_id),
	
	[{_,TmpX}] = ets:lookup(Ets,x),
	[{_,TmpY}] = ets:lookup(Ets,y),
	%%ck how long is the way to the target
	Arrived = step_dest(TmpX,TmpY,DstX,DstY,DifX,DifY,Ets),
	
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	%%uptate the statistics that we move to new loction
	update_stat(travel,math:sqrt((TmpX-CurrentX)*(TmpX-CurrentX)+(TmpY-CurrentY)*(TmpY-CurrentY))),
	
	 %%ck if the heli is need to pass to other server then do move seq
	%unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
	case check_screen(CurrentX,CurrentY) of
		ServerName -> unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
					  Need_to_change_screen = false;
		AnotherServer -> Need_to_change_screen = AnotherServer
	end,
	%%case if the heli got the sensor
	case Arrived of
		true -> %io:format("arrive to objective: ~p and starting circle~n",[Objective]), %% if only searching fire, then remove objective
				{CR,CX,CY,A} = Objective,
				DifAngle = calc_angle_diff(CR),
				update_stat(dest_time,erlang:now()),
				case Need_to_change_screen of 
					%%if not need to change screen then go to scen mode on the radios of thr active sensor
					false -> {next_state,search_circle,{CR,CX,CY,A,DifAngle,Objective},?REFRESH_SPEED};
					Serv -> wait_for_server_recover(ServerName,Serv,[MyName,CurrentX,CurrentY],search_circle,{CR,CX,CY,A,DifAngle,Objective})
							%unit_server:change_screen(ServerName,Serv,[MyName,CurrentX,CurrentY],search_circle,{CR,CX,CY,A,DifAngle,Objective}),
							%{stop,normal,{}}
				end;
				%%not arrive so go on..
		false -> case Need_to_change_screen of
					false -> {next_state,move_destination,{DifX,DifY,DstX,DstY,Objective},?REFRESH_SPEED};
					Serv -> wait_for_server_recover(ServerName,Serv,[MyName,CurrentX,CurrentY],move_destination,{DifX,DifY,DstX,DstY,Objective})
							%unit_server:change_screen(ServerName,Serv,[MyName,CurrentX,CurrentY],move_destination,{DifX,DifY,DstX,DstY,Objective}),
							%{stop,normal,{}}
				 end
	end;
	%%else
move_destination(_Event, State) ->
  {next_state, move_destination, State,?REFRESH_SPEED}.
  
%%go to the new loction x y on the radios move
search_circle(timeout,{R,CX,CY,Angle,DifAngle,SensorData}) -> 
	
	Ets = get(ets_id),
	step_circle(CX,CY,R,Angle,Ets),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	%%init statistics
	update_stat(travel,Angle*R),
	%%ck if need to move to other server
	%unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
	case check_screen(CurrentX,CurrentY) of
		ServerName -> unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
					  Need_to_change_screen = false;
		OtherServer -> Need_to_change_screen = OtherServer		   
	end,
	%%if the heli find fire - move to extinguish else move til heli get all around the circle
	unit_server:fire_check(ServerName,MyName),
	%%ck if heli got to the top of the radios
	case Angle > 6.29 of 
		true -> %io:format("finished circle~n"),
				update_stat(work_time,erlang:now()),
				{DifX,DifY} = rand_idle_diff(),
				unit_server:heli_done(ServerName,MyName),
				case Need_to_change_screen of 
					false -> {next_state,idle,{DifX,DifY},?REFRESH_SPEED};
					Serv -> wait_for_server_recover(ServerName,Serv,[MyName,CurrentX,CurrentY],idle,{DifX,DifY})
							%unit_server:change_screen(ServerName,Serv,[MyName,CurrentX,CurrentY],idle,{DifX,DifY}),
							%{stop,normal,{}}
				end;
		%%keep move on the circle
		false -> case Need_to_change_screen of
					false -> {next_state,search_circle,{R,CX,CY,Angle + DifAngle,DifAngle,SensorData},?REFRESH_SPEED};
					Serv -> wait_for_server_recover(ServerName,Serv,[MyName,CurrentX,CurrentY],search_circle,{R,CX,CY,Angle + DifAngle,DifAngle,SensorData})
							%unit_server:change_screen(ServerName,Serv,[MyName,CurrentX,CurrentY],search_circle,{R,CX,CY,Angle + DifAngle,DifAngle,SensorData}),
							%{stop,normal,{}}
				 end
	end;
%%when heli move on the radios and found active fire - move to extinguish state
search_circle({found_fire,[NF,RF,XF,YF]},{_R,_CX,_CY,_Angle,_DifAngle,SensorData}) -> 
	
	Ets = get(ets_id),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	
	%%calc where is the radios of the fire to know where to move for start extinguish
	StartAngle = calc_destination_angle(CurrentX,CurrentY,XF,YF),
	FrameX = RF * math:cos(StartAngle)+ XF,
	FrameY = RF * math:sin(StartAngle)+ YF,
	%%move to fire til you get the fire radios
	case check_frame(CurrentX,CurrentY,FrameX,FrameY) of
		true -> {next_state,extinguish,{circle,NF,RF,XF,YF,StartAngle,SensorData},?EXTINGUISH_SPEED};
		false -> {DifX,DifY} = calc_destination_movement_diffs(StartAngle),
				 {next_state,extinguish,{straight,-1*DifX,-1*DifY,NF,XF,YF,StartAngle,SensorData},?EXTINGUISH_SPEED}
	end;
		

	%%else
search_circle(_Event, State) ->
  {next_state, search_circle, State,?REFRESH_SPEED}.
  
%%clock tic is the time to send a message to the fire that increse the radios of the fire
extinguish(timeout,{circle,N,R,X,Y,Angle,SensorData}) -> 
	Ets = get(ets_id),
	step_circle(X,Y,R,Angle,Ets),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	
	update_stat(travel,Angle*R),
	%%if need to change server screen
	case check_screen(CurrentX,CurrentY) of
		ServerName -> unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
					  Need_to_change_screen = false;
		OtherServer -> Need_to_change_screen = OtherServer		   
	end,
	%%send message to fire to increse
	{FireState,NewR} = fire:extinguish_fire(N),
	DifAngle = calc_angle_diff(R),
	
	%%ck if the fire is stiil alive.
	case FireState of
		%% keep extinguish
	    fire_alive-> case Need_to_change_screen of
						false -> {next_state,extinguish,{circle,N,NewR,X,Y,Angle + DifAngle,SensorData},?EXTINGUISH_SPEED};
						Serv -> wait_for_server_recover(ServerName,Serv,[MyName,CurrentX,CurrentY],extinguish,{circle,N,NewR,X,Y,Angle + DifAngle,SensorData})
								%unit_server:change_screen(ServerName,Serv,[MyName,CurrentX,CurrentY],extinguish,{circle,N,NewR,X,Y,Angle + DifAngle,SensorData}),
								%{stop,normal,{}}
					 end;
			
		%%the fire is dead and the heli is done!
		%%clc the next move after done
	    fire_dead->  %{DifX,DifY} = rand_idle_diff(),
					  %io:format("fire_dead~n"),
					  %unit_server:heli_done(ServerName,MyName),
					  {CR,CX,CY,_A} = SensorData,
					  {DstX,DstY} = {CR+CX,CY},
					  NextAngle = calc_destination_angle(CurrentX,CurrentY,DstX,DstY),
					  {NextDifX,NextDifY} = calc_destination_movement_diffs(NextAngle),
					  update_stat(fires,1),
					  %update_stat(work_time,erlang:now()),
					  
					  %%if need to change server screen
					  case Need_to_change_screen of
						false -> {next_state,move_destination,{NextDifX,NextDifY,DstX,DstY,SensorData},?REFRESH_SPEED};
						Serv -> wait_for_server_recover(ServerName,Serv,[MyName,CurrentX,CurrentY],move_destination,{NextDifX,NextDifY,DstX,DstY,SensorData})
								%unit_server:change_screen(ServerName,Serv,[MyName,CurrentX,CurrentY],move_destination,{NextDifX,NextDifY,DstX,DstY,SensorData}),
								%{stop,normal,{}}
					  end
					  
					  %{next_state, idle, {DifX,DifY},?REFRESH_SPEED}
	end;
	
%%sametimes in the extinguish seq the hlie need to inside trow the center of the fire 
%%becuse we want thet the heli keep the new radios.
%%so this is clock tic move straight like move circle
extinguish(timeout,{straight,DifX,DifY,NF,XF,YF,Angle,SensorData}) -> 
	Ets = get(ets_id),
	move_dif(DifX,DifY,Ets),
	[{_,CurrentX}] = ets:lookup(Ets,x),
	[{_,CurrentY}] = ets:lookup(Ets,y),
	[{_,MyName}]   = ets:lookup(Ets,myName),
	[{_,ServerName}] = ets:lookup(Ets,serverName),
	%%for init statistics
	update_stat(travel,math:sqrt(DifX*DifX+DifY*DifY)),
	% if need to change screen
	case check_screen(CurrentX,CurrentY) of  %%if need to change server screen
		ServerName -> unit_server:update(ServerName,heli,[MyName,CurrentX,CurrentY]),
					  Need_to_change_screen = false;
		OtherServer -> Need_to_change_screen = OtherServer		   
	end,
	%%clc the new x y and frame
	{FireState,NewR} = fire:extinguish_fire(NF),
	 
	FrameX = NewR * math:cos(Angle)+ XF,
	FrameY = NewR * math:sin(Angle) + YF,
	
		%%ck if the fire is stiil alive.
	case FireState of
	    fire_alive-> %%frame order the heli to go straight or circle. 
					case check_frame(CurrentX,CurrentY,FrameX,FrameY) of
						true -> NextState = {next_state,extinguish,{circle,NF,NewR,XF,YF,Angle,SensorData},?EXTINGUISH_SPEED};
						false -> NextState = {next_state,extinguish,{straight,DifX,DifY,NF,XF,YF,Angle,SensorData},?EXTINGUISH_SPEED}
					 end,
					 {_,_,Data,_} = NextState,
					 case Need_to_change_screen of
						false -> NextState;
						Serv -> wait_for_server_recover(ServerName,Serv,[MyName,CurrentX,CurrentY],extinguish,Data)
								%unit_server:change_screen(ServerName,Serv,[MyName,CurrentX,CurrentY],extinguish,Data),
								%{stop,normal,{}}
					 end;
		%%if fire is dead move to anther circle ck.
	    fire_dead->  %{DifX,DifY} = rand_idle_diff(),
					  %io:format("fire_dead~n"),
					  %unit_server:heli_done(ServerName,MyName),
					  {CR,CX,CY,_A} = SensorData,
					  {DstX,DstY} = {CR+CX,CY},
					  NextAngle = calc_destination_angle(CurrentX,CurrentY,DstX,DstY),
					  {NextDifX,NextDifY} = calc_destination_movement_diffs(NextAngle),
					  update_stat(fires,1),
					  %update_stat(work_time,erlang:now()),
					  
					  case Need_to_change_screen of  %%if need to change server screen
						false -> {next_state,move_destination,{NextDifX,NextDifY,DstX,DstY,SensorData},?REFRESH_SPEED};
						Serv -> wait_for_server_recover(ServerName,Serv,[MyName,CurrentX,CurrentY],move_destination,{NextDifX,NextDifY,DstX,DstY,SensorData})
								%unit_server:change_screen(ServerName,Serv,[MyName,CurrentX,CurrentY],move_destination,{NextDifX,NextDifY,DstX,DstY,SensorData}),
								%{stop,normal,{}}
					 end
					  %{next_state, idle, {DifX,DifY},?REFRESH_SPEED}
	end;
	%%else
extinguish(_Event, State) ->
  {next_state, extinguish, State,?REFRESH_SPEED}.

 
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
%%we dont use sync message
idle(_Event, _From, State) ->
  Reply = {error, invalid_message},
  io:format("got to sync receive, shouldnt be here~n"),
  {reply, Reply, idle, State}.
  
move_destination(_Event, _From, State) ->
  Reply = {error, invalid_message},
  io:format("got to sync receive, shouldnt be here~n"),
  {reply, Reply, move_destination, State}.
  
search_circle(_Event, _From, State) ->
  Reply = {error, invalid_message},
  io:format("got to sync receive, shouldnt be here~n"),
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
%%else
handle_event({stop}, _StateName, State) ->
	{stop, normal, State};
	
handle_event({stat}, StateName, State) ->
	print_stat(),
	{next_state, StateName, State,?REFRESH_SPEED};

handle_event({crash,Num}, StateName, _State) ->
    {next_state, StateName, 1/Num};

handle_event(_Event, StateName, State) ->
	%io:format("got to handle event because of ~p in state ~p~n",[_Event,StateName]),
    {next_state, StateName, State,?REFRESH_SPEED}.
 
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
	io:format("got to sync handle event because of ~p in state ~p~n",[_Event,StateName]),
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
	
handle_info({_,{crash,Num}}, StateName, _State) ->
    {next_state, StateName, 1/Num};
	
handle_info({_,{stat}}, StateName, State) ->
	print_stat(),
	{next_state, StateName, State,?REFRESH_SPEED};

handle_info(_Info, StateName, State) ->
	io:format("unsupported message received ~p in state ~p~n",[_Info,StateName]),
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
	%io:format("heli termination handler: '~p' '~p' '~p'~n",[_Reason, _StateName,_State]),
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

%%this func calc the nexy length step form the ETS and the old DifX and DifY
move_dif(DifX,DifY,Ets) ->
  %[{_,DifX}] = ets:lookup(Ets,xdif),
  %[{_,DifY}] = ets:lookup(Ets,ydif),
  [{_,CurrentX}] = ets:lookup(Ets,x),
  NewX = CurrentX+DifX, %%commolator
  [{_,CurrentY}] = ets:lookup(Ets,y),
  NewY = CurrentY+DifY,%%commolator
  %%ck if the newX and newY is in the screen borders
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
  %%same for Y
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
  %%return the new DIF
  {NewDifX,NewDifY}.
  %io:format("new (x,y) = (~p,~p)~n",[NewX,NewY]).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%take the self location (x,y) and the Dst to the target and calc the right angle.
%% find if x was change then y will update else if y was change then X will update
calc_destination_angle(X,Y,DstX,DstY) ->
	DeltaX = X - DstX,
	DeltaY = Y - DstY,
	S = math:sqrt(DeltaX * DeltaX + DeltaY * DeltaY), %%calc the length to the target
	case X> DstX of
		true -> case Y > DstY of
					true -> Angle = math:asin((Y-DstY)/S);
					false -> Angle = (math:pi()/2 -math:asin((DstY-Y)/S)) + math:pi() * 3/2
				end;
		false -> case Y > DstY of
					true -> Angle = (math:pi()/2 -math:asin((Y-DstY)/S)) + math:pi() / 2;
					false -> Angle = math:asin((DstY-Y)/S) + math:pi()
				 end
	end,
	
	Angle.
	
	%%calc Angle and refresh and movement speed to get the DIF x,y
calc_destination_movement_diffs(Angle) -> 
	%Travel_time = S / ?MOVEMENT_SPEED,
	%Ticks_required = 1000 * Travel_time / ?REFRESH_SPEED,
	%Dif = S / Ticks_required,
	Dif = ?MOVEMENT_SPEED * ?REFRESH_SPEED / 1000,
	DifX = -1* Dif * math:cos(Angle),
	DifY = -1 * Dif * math:sin(Angle),
	{DifX,DifY}.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%do the move in the clock tic. make the heli actully move. 
step_dest(X,Y,DstX,DstY,DifX,DifY,Ets) -> 
	%[{_,DifX}] = ets:lookup(Ets,xdif),
	%[{_,DifY}] = ets:lookup(Ets,ydif),
	%io:format("~ndifs = ~p~n~n",[{DifX,DifY}]),
	Dist = (X-DstX) * (X-DstX) + (Y-DstY) * (Y-DstY),
	Diff = DifX * DifX + DifY * DifY,
	%%if you got the targt update it else make step++
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
%%calc the length step on circle move
calc_angle_diff(R) ->
	S = 2 * math:pi() * R,
	Travel_time = S / ?MOVEMENT_SPEED,   %%Time*Speed=S
	Ticks_required = Travel_time * 1000 / ?REFRESH_SPEED,
	Dif = S / Ticks_required,
	Delta_Angle = Dif / R,
	%io:format("diff angle = ~p~n", [Delta_Angle]),
	Delta_Angle.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%do the step move on the radios move
step_circle(CX,CY,R,Angle,Ets) -> 
	NewX = math:cos(Angle) * R + CX,
	NewY = math:sin(Angle) *R + CY,
	ets:insert(Ets,{x,NewX}),
	ets:insert(Ets,{y,NewY}).
	%Debug = math:sqrt( (NewX-CX) * (NewX-CX) + (NewY-CY) * (NewY-CY)),
	%io:format("new (x,y) = (~p,~p)~n",[NewX,NewY]). %distance from circle = ~p~n",[NewX,NewY,Debug])
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% for normal random move seek a new angle to move
rand_idle_diff()->
  %Tmp = erlang:system_time() / erlang:monotonic_time(),
  %Time = erlang:round((Tmp - erlang:trunc(Tmp)) * 100000000),
  Pid = getPid(self()),
  {A,_B,C}=erlang:now(),
  random:seed(C,Pid*Pid,erlang:round(C/A)),
 %%random func
  Xdif = ((random:uniform() * 2 - 1) * ?MOVEMENT_SPEED),
  Dir = random:uniform(2),
	%% random angle
  case Dir of
	1 -> Ydif = -1 * math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif);
	2 -> Ydif = math:sqrt(?MOVEMENT_SPEED * ?MOVEMENT_SPEED - Xdif * Xdif)
  end,
  %%Normal the dif to sim time.
  DifX = Xdif * (?REFRESH_SPEED / 1000),
  DifY = Ydif * (?REFRESH_SPEED / 1000),
  {DifX,DifY}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%ck in whice frame the heli is.
check_frame(HX,HY,FrameX,FrameY) -> 
	((HX-FrameX) * (HX-FrameX) + (HY-FrameY) * (HY-FrameY)) <16.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%ck if the heli out from the server borders
check_screen(X,Y) ->
	case X > ?Horizontal/2 of
		true -> case Y > ?Vertical/2 of
					true -> br;
					false -> tr
				end;
		false -> case Y > ?Vertical/2 of
					true -> bl;
					false -> tl
				 end
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% when the heli move to new server screen. first make conction with the new server the kill the procc in that server
wait_for_server_recover(ServerName,Serv,Data,State,StateData) ->
	case (global:whereis_name(ServerName) /= undefined) and (global:whereis_name(Serv) /= undefined) of
		true -> Stat = get(stat),
				unit_server:change_screen(ServerName,Serv,Data,State,StateData,ets:tab2list(Stat)),
				{stop,normal,{}};
		false -> timer:sleep(?REFRESH_SPEED),
				 wait_for_server_recover(ServerName,Serv,Data,State,StateData)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% for statistics take all from ETS 
create_stat()->
	Stat = ets:new(stat,[set]),
	put(stat,Stat),
	ets:insert(Stat,{fires,0}),
	ets:insert(Stat,{current_work_time,0}),
	ets:insert(Stat,{work_time,0}),
	ets:insert(Stat,{start_time,erlang:now()}),
	ets:insert(Stat,{dest_time,{0,0,ok}}),
	ets:insert(Stat,{travelled,0}).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%save the stat ETS on the DB of the heli
create_stat(Data)->
	Stat = ets:new(stat,[set]),
	put(stat,Stat),
	ets:insert(Stat,Data).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ck Type of the stat and save to the local ETS
update_stat(Type,Value) ->
	Stat = get(stat),
	case Type of
		%%how long
		travel -> [{_,Travel}] = ets:lookup(Stat,travelled),
				  ets:insert(Stat,{travelled,Travel+Value});
		%% fire that extinguish by the heli
		fires-> [{_,Fire}] = ets:lookup(Stat,fires),
				 ets:insert(Stat,{fires,Fire+1});
		%% from start sim
		current_work_time -> ets:insert(Stat,{current_work_time,Value}),
							 [{_,{Time,Count,_State}}] = ets:lookup(Stat,dest_time),
							 ets:insert(Stat,{dest_time,{Time,Count,waiting}});
		%% all extinguish time
		work_time -> [{_,Prev}] = ets:lookup(Stat,current_work_time),
					 [{_,Work_time}] = ets:lookup(Stat,work_time),
					 ets:insert(Stat,{work_time,Work_time+timer:now_diff(Value,Prev)});
		%% all move to dst time
		dest_time -> [{_,Prev}] = ets:lookup(Stat,current_work_time),
					 [{_,{Time,Count,State}}] = ets:lookup(Stat,dest_time),
					 case State == waiting of 
						true -> ets:insert(Stat,{dest_time,{Time+timer:now_diff(Value,Prev),Count+1,ok}});
						false-> do_nothing
					end
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%print to screen
print_stat()->
	Ets = get(ets_id),
	[{_,MyName}] = ets:lookup(Ets,myName),
	io:format("~p statistics:~n",[MyName]),
	Stat=get(stat),
	[ chooseStat(CurrentStat) || CurrentStat<-ets:tab2list(Stat)].
	
	%%help func to choose type to screem
chooseStat({Type,Val}) ->
	Stat=get(stat),
	case Type of
		travelled -> io:format("----distance travelled: ~p [pixels]~n",[Val]);
		fires -> io:format("----fire extinguished by this heli: ~p ~n",[Val]);
		work_time -> [{_,Start}] = ets:lookup(Stat,start_time),
					 io:format("----total work time: ~p ; % of time working: ~p~n",[Val/1000000,100*Val/timer:now_diff(erlang:now(),Start)]);
		dest_time -> {Time,Count,_} = Val,
					 case Count ==0 of
						true -> io:format("----average time to reach to destination: ~p ~n",[undefined]);
						false -> io:format("----average time to reach to destination: ~p ~n",[Time/Count/1000000])
					end;
		_Any -> do_nothing
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%get PID all time ck if change Res in all the getPid
getPid(Pid) -> getPid(erlang:pid_to_list(Pid),false,[]).	
	
%getPid(_,true,Res) -> Res;									
getPid([H|T],Start,Res) when Start==false -> case ([H|[]]==".")of
													true -> getPid(T,true,Res);
													false -> getPid(T,Start,Res)
												  end;
												  
getPid([H|T],Start,Res) when Start==true  -> case ([H|[]]==".")of
												true -> list_to_integer(Res);
												false -> getPid(T,Start,Res ++ [H])
											 end.