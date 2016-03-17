
-module(unit_server).

-include_lib("stdlib/include/qlc.hrl").

-behaviour(gen_server).

% interface calls
-export([start/1,create/2,update/3,pass_server_alert/2,heli_request/3,heli_request/5,
		 give_heli/3,choose_heli/3,heli_done/2,fire_check/2,fire_check/4,
		 wx_update/1,start_sim/1,change_screen/6,transfer_heli/5,
		 crash/1,crash_recover/2,stop/1,statistics/1,addFire/4]).
    
% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2, code_change/3]).
-include("config.hrl").		 
%-define(OVERLAP_PERC, 90).
%%====================================================================
%% Server interface
%%====================================================================
%%starting new unit server
start(GenName) -> 
    gen_server:start({global, GenName}, ?MODULE, [GenName], []).

%%starting simulation (start of units movements)	
start_sim(GenName) -> 
    gen_server:cast({global, GenName}, {start_sim}).

%%create the units according to Data
create(GenName,Data) -> 
    gen_server:cast({global, GenName}, {create,Data}).

%%update from unit about their location \ radius
update(GenName,Type,Data) -> 
    gen_server:cast({global, GenName}, {update,Type,Data}).
	
%%check if fire is in sensor area in different node than the fire node
pass_server_alert(GenName,Data) -> 
    gen_server:cast({global, GenName}, {pass_alert,Data}).
	
%%request from sensor to get helicopter to extinguish fire
heli_request(GenName,Sname,Fname) -> 
    gen_server:cast({global, GenName}, {heli_request,Sname,Fname}).
	
%%request for helicopter from another node
heli_request(GenName,FromServer,Key,BestHeli,DstCords) -> 
    gen_server:cast({global, GenName}, {server_heli_request,FromServer,Key,BestHeli,DstCords}).
	
%%allocating helicopter to the node that requested
give_heli(GenName,Key,Val) -> 
    gen_server:cast({global, GenName}, {give_heli,Key,Val}).

%%choose which helicopter to send to handle a fire	
choose_heli(GenName,Key,Destination) -> 
    gen_server:cast({global, GenName}, {choose_heli,Key,Destination}).	

%%update helicopter state to not working
heli_done(GenName,Name) -> 
    gen_server:cast({global, GenName}, {heli_done,Name}).

%%	check if helicopter got to fire in same node
fire_check(GenName,Name) -> 
	gen_server:cast({global, GenName}, {heli_fire_check,Name}).

%%	check if helicopter got to fire in different node	
fire_check(GenName,Name,X,Y) -> 
	gen_server:cast({global, GenName}, {server_fire_check,Name,X,Y}).
   
%%send ets to wx server
wx_update(GenName) ->
	try gen_server:call({global,GenName},{wx_request},infinity) catch _Error:_Reason -> server_off end.

%%request from helicopter to change node	
change_screen(GenName,NewServer,UnitInfo,UnitState,UnitStateData,Stat) -> 
    gen_server:cast({global, GenName}, {change_screen,NewServer,UnitInfo,UnitState,UnitStateData,Stat}).

%%request from node to different node to add helicopter
transfer_heli(GenName,UnitInfo,UnitState,UnitStateData,Stat) -> 
    gen_server:cast({global, GenName}, {transfer_heli,UnitInfo,UnitState,UnitStateData,Stat}).

%%crash unit server
crash(GenName) ->
	gen_server:cast({global, GenName}, {crash,0}).

%%start unit server after crash
crash_recover(GenName,Data) ->
	gen_server:start({global, GenName}, ?MODULE, [crash,Data], []).

%%stop the unit server
stop(Name) ->
	gen_server:cast({global, Name}, {stop}).

%%print statistics of Name
statistics(Name) ->
	gen_server:cast({global, Name}, {stat}). 

%%add new fire to simulation
addFire(Name,FireName,X,Y) ->
	gen_server:cast({global, Name}, {add_fire,FireName,X,Y}). 
  
%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Name]) ->
    io:format("starting local_gen: ~p~n",[Name]),
	
	%%create monitor for local node
	MonName = list_to_atom(atom_to_list(Name) ++ "mon"),
	loc_monitor:start(MonName),
	loc_monitor:add_mon(MonName,global:whereis_name(Name)),		%%add monitor for local unit server
	put(mon_name,MonName),
	MonPid = global:whereis_name(MonName),
	
	%%creating data ets, with inheritance to the monitor process
	Ets = ets:new(general_info,[set,{heir,MonPid , {server,Name}}]),
	put(ets_id,Ets),
	ets:insert(Ets,{myInfo,Name}),
	Sen_fire = ets:new(sen_fire,[bag,public]),
	put(sen_fire_id,Sen_fire),

	create_stat(),							%%create the statistics ets
	spawn_link(fun() -> watch_dog() end),	%%start watch_dog process, to make sure there is connection to other nodes
	
    {ok, initialized};
	
init([crash,Data]) ->
	
	%%creating data ets, using the inherited data
	Ets=ets:new(general_info,[set]),
	put(ets_id,Ets),
	ets:insert(Ets,Data),
	[{_,Name}] = ets:lookup(Ets,myInfo),
	
	MonName = list_to_atom(atom_to_list(Name) ++ "mon"),
	MonPid = global:whereis_name(MonName),
	
	ets:setopts(Ets,{heir,MonPid , {server,Name}}),
	loc_monitor:add_mon(MonName,global:whereis_name(Name)),		%%add monitor to unit server

	Sen_fire = ets:new(sen_fire,[bag,public]),
	put(sen_fire_id,Sen_fire),
	
	create_stat(),							%%create the statistics ets
	spawn_link(fun() -> watch_dog() end),	%%start watch_dog process, to make sure there is connection to other nodes
	
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
handle_call({wx_request}, _From, State) -> 
	Ets = get(ets_id),
	
	%%qlc to get all helicopters and fires
	QH = qlc:q([{{Type,Name},Field1,Field2,Field3}	|| {{Type,Name},Field1,Field2,Field3} <- ets:table(Ets), ((Type==fire) or (Type==heli))]),
	
	Stat = get(stat),
	
	%%in case the simulation started, update statistics
	case ets:lookup(Stat,sim_started) == [] of
		true -> do_nothing;
		false ->
				QH2 = qlc:q([1	|| {{heli,_},_,_,_} <- ets:table(Ets)]),
				Current_heli_amount = erlang:length(qlc:eval(QH2)),
				update_stat(heli_count,Current_heli_amount),				%heli amount statistics
				update_stat(message_count,{0,erlang:now()})					%average message sent statistics
	end,
	
	{reply,qlc:eval(QH),State}; 		%%send the data for helis and fires
	
%%ignore any other call
handle_call(_Message, _From, State) -> 
    {reply, ok, State}.

%% Asynchronous, possible return values
% {noreply,NewState} 
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause

%%stop the unit server
handle_cast({stop}, State) ->
    {stop,normal,State};	

%%create ets in unit server, and starts all units
handle_cast({create,DataList}, State) ->
	Ets = get(ets_id),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	
	terminate_all_units(Ets),		%%terminates all units created before
	ets:delete_all_objects(Ets),
	
	ets:insert(Ets,{myInfo,MyName}),
	
	ets:insert(Ets,DataList),
	
	QH = qlc:q([Name || {{_,Name},_,_,_} <- ets:table(Ets)]),
	wait_done(qlc:eval(QH)),									%%wait for units to terminate before starting new ones
	HeliList = ets:match(Ets,{{heli,'$1'},'$2','$3','_'}),
	[heli:start(Name,MyName,X,Y) || [Name,X,Y] <- HeliList],	%%starting helicopters
	FireList = ets:match(Ets,{{fire,'$1'},'$2','$3','$4'}),
	[fire:start(Name,MyName,R,X,Y) || [Name,R,X,Y] <- FireList],%%starting fires
	SenList = ets:match(Ets,{{sensor,'$1'},'$2','$3','$4'}),
	[sensor:start(Name,MyName) || [Name,_,_,_] <- SenList],		%%starting sensors
	
    {noreply, State};
	
%%starting simulation, send to all units in node to start simulation
handle_cast({start_sim}, State) ->
	Ets = get(ets_id),
	HeliList = ets:match(Ets,{{heli,'$1'},'_','_','_'}),
	[heli:start_sim(Name) || [Name] <- HeliList],			%%start simulation for helis
	FireList = ets:match(Ets,{{fire,'$1'},'_','_','_'}),
	[fire:start_sim(Name) || [Name] <- FireList],			%%start simulation for fires
	SenList = ets:match(Ets,{{sensor,'$1'},'_','_','_'}),
	[sensor:start_sim(Name) || [Name] <- SenList],			%%start simulation for sensors
	
	Stat = get(stat),
	update_stat(start_sim_time,erlang:now()),				%%updates starting simulation time
	ets:insert(Stat,{sim_started,true}),
	{noreply, State};
	
%%update location \ radius of heli or fire
%%if its fire update checking if fire merge happened, if happened, and its the small one, destroy it
%%also check if fire is in sensor area, if so send alert
handle_cast({update,Unit_Type,Unit_Data}, State) ->
	Ets = get(ets_id),
	case Unit_Type of					%%check unit type
		heli -> [Name,X,Y]=Unit_Data,	
				[{{_,_},_,_,Status}]= ets:lookup(Ets,{heli,Name}),
				ets:insert(Ets,{{heli,Name},X,Y,Status});			%%update in unit server ets the heli location
		fire -> [Name,RF]=Unit_Data,
				[{{fire,_},_,XF,YF}] = ets:lookup(Ets,{fire,Name}),
				ets:insert(Ets,{{fire,Name},RF,XF,YF}),
				
				%%qlc to find if there are other fires that have 90% overlap
				QH_Merge = qlc:q([FName || {{fire,FName},F2R,F2X,F2Y} <- ets:table(Ets),
							    FName/=Name,F2R>0,RF>0, overlappingFire(XF,YF,F2X,F2Y,RF,F2R)==true ]),
				
				case qlc:eval(QH_Merge) of
					[] -> dont_care;
 					_Any ->fire:merge(Name)		%%merge the fire (destroy it)
				end,
				
				%%find all sensors in current node that overlap with the fire
				QH = qlc:q([SName || {{sensor,SName},RS,XS,YS} <- ets:table(Ets), ((XF-XS)*(XF-XS) + (YF-YS)*(YF-YS))< (RS+RF)*(RS+RF)]),

				case qlc:eval(QH) of
					[] -> dont_care;
					SensorList -> [ sensor:new_alert(Sen,Name) || Sen <- SensorList]	%%if overlapped sensor found, sends alert
				end,
				[{_,MyName}] = ets:lookup(Ets,myInfo),
				
				%%send to all other nodes the fire data in order to check if they have overlapping sensors with the fire
				[ unit_server:pass_server_alert(DstServer,[Name,RF,XF,YF]) || DstServer <- [tl,tr,bl,br], DstServer/=MyName]
	end,
	update_stat(message_count,{1,erlang:now()}),		%%update statistics
	{noreply, State};	
	
%%check if there is sensors in current node, that overlap with the fire received
handle_cast({pass_alert,Fire_Data}, State) ->
	Ets = get(ets_id),
	[Name,RF,XF,YF] = Fire_Data,
	
	%%find all overlapping sensors
	QH = qlc:q([SName || {{sensor,SName},RS,XS,YS} <- ets:table(Ets), ((XF-XS)*(XF-XS) + (YF-YS)*(YF-YS))< (RS+RF)*(RS+RF)]),

	case qlc:eval(QH) of
		[] -> dont_care;
		SensorList -> [ sensor:new_alert(Sen,Name) || Sen <- SensorList]	%%send alert to all overlapping sensors with the fire
	end,
	update_stat(message_count,{1,erlang:now()}),	%%update statistics
	{noreply,State};
	
%%request to send heli to handle fire	
handle_cast({heli_request,Sen_name,Fire_Name}, State) ->
	Ets = get(ets_id),
	Sen_fire = get(sen_fire_id),
	Exists = ets:member(Sen_fire,{Sen_name,Fire_Name}),		%%check if a heli already sent to handle the fire (because of the same sensor)
	case Exists of
		true -> do_nothing;
		false ->  QH = qlc:q([{HName,HX,HY} || {{heli,HName},HX,HY,not_working} <- ets:table(Ets)]),	%%finds free helicopter in same node
				  [{{_,_},SR,SX,SY}] = ets:lookup(Ets,{sensor,Sen_name}),
				  [{_,MyName}] = ets:lookup(Ets,myInfo),
				   case qlc:eval(QH)  of
						[]-> Dist = ?Horizontal * ?Horizontal * ?Horizontal; 
						HeliList -> 
									{Name,X,Y,Dist} = closest_heli(HeliList,SX+SR,SY), 				%%finds closest heli to the sensor and its distance from it
									ets:insert(Sen_fire,{{Sen_name,Fire_Name},{Name,Dist,MyName}}),
									ets:insert(Ets,{{heli,Name},X,Y,working})						%%"lock" the helicopter
									
									
				   end,
				   ets:delete(Sen_fire,{{Sen_name,Fire_Name},done}),
				   %%send helicopter request from all other nodes
				   [ unit_server:heli_request(Serv,MyName,{Sen_name,Fire_Name},Dist,{SX+SR,SY})|| Serv <- [tl,tr,bl,br], Serv /=MyName],
				   %%timeout for the unit server (1sec) to choose helicopter from the given helicopters
				   spawn(fun() -> timer:sleep(1000), unit_server:choose_heli(MyName,{Sen_name,Fire_Name},{SR,SX,SY}) end)
	end,
	
	update_stat(message_count,{1,erlang:now()}), %% update statistics
	{noreply, State};	

%%request for helicopter from another node	
handle_cast({server_heli_request,FromServer,Key,MinDist,{DstX,DstY}}, State) ->
	Ets = get(ets_id),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	%%check if there is a free helicopter, that is closer to the sensor than the given heli
	QH = qlc:q([{HName,HX,HY} || {{heli,HName},HX,HY,not_working} <- ets:table(Ets), (DstX-HX) * (DstX-HX) + (DstY-HY) * (DstY-HY) < MinDist]),
	case qlc:eval(QH) of
		[] -> do_nothing;
		HeliList -> {Name,X,Y,Dist} = closest_heli(HeliList,DstX,DstY),			%%choose the closest heli
					ets:insert(Ets,{{heli,Name},X,Y,working}),					%%"lock" the heli
					unit_server:give_heli(FromServer,Key,{Name,Dist,MyName})	%%allocate helicopter for the node that requested heli
	end,
	update_stat(message_count,{1,erlang:now()}),	%%update statistics
	{noreply, State};

%%give helicopter to node, to handle certain {sensor,fire}	
handle_cast({give_heli,Key,Val}, State) ->
	Sen_fire = get(sen_fire_id),
	
	%%check if node already chose heli
	%%if it already chose free the given heli (change state to not working)
	case ets:member(Sen_fire,{Key,done}) of
		false -> ets:insert(Sen_fire,{Key,Val});
		true -> {Name,_,Serv}=Val,
				unit_server:heli_done(Serv,Name)
	end,
	update_stat(message_count,{1,erlang:now()}),	%%update statistics
	{noreply, State};

%%choose helicopter to send from the given helicopters (maximum of 4 helis)
handle_cast({choose_heli,Key,{R,X,Y}}, State) ->
	Sen_fire = get(sen_fire_id),
	ets:insert(Sen_fire,{{Key,done},done}),			%%update that {sensor,fire} is handled
	case ets:member(Sen_fire,Key) of
		true -> HeliList = ets:lookup(Sen_fire,Key);
		false -> HeliList=[]
	end,
	
	case servers_closest_heli(HeliList) of		%%choose closet heli to sensor
		{ChoosenHeli,OtherHeli} -> [ unit_server:heli_done(HeliServer,Heli) || {Heli,HeliServer} <- OtherHeli], %%free the helicopters that wasn't chosen
									heli:move_dst(ChoosenHeli,X+R,Y,{R,X,Y,0}),	%%send helicopter to handle the fire
									%wait 20 second and allow another heli request from sensor-fire pair
									spawn(fun() -> timer:sleep(20000), case lists:member(Sen_fire,ets:all()) == false of false -> ets:delete(Sen_fire,Key); true-> do_nothing end end);
		no_heli -> do_nothing
	end,
	update_stat(message_count,{1,erlang:now()}),	%%update statistics
	{noreply, State};
					
					
%%change helicopter state to not working
handle_cast({heli_done,Name}, State) ->
	Ets = get(ets_id),
	ets:update_element(Ets,{heli,Name},{4,not_working}),	%%change heli state
	update_stat(message_count,{1,erlang:now()}),			%%update statistics
	{noreply, State};
	
%%tell unit server that the helicopter changing node
handle_cast({change_screen,NewServer,UnitInfo,UnitState,UnitStateData,Stat}, State) ->
	Ets = get(ets_id),
	[Name,X,Y] = UnitInfo,
	[{{_,_},_,_,WorkState}] = ets:lookup(Ets,{heli,Name}),
	ets:delete(Ets,{heli,Name}),							%%delete helicopter from the unit server
	
	%%tells the next unit server to create the heli in its node
	unit_server:transfer_heli(NewServer,[Name,X,Y,WorkState],UnitState,UnitStateData,Stat),
	
	update_stat(message_count,{1,erlang:now()}),		%%update statistics
	{noreply, State};
	
%%add heli to the unit server
handle_cast({transfer_heli,UnitInfo,UnitState,UnitStateData,Stat}, State) ->
	Ets = get(ets_id),
	[Name,X,Y,WorkState] = UnitInfo,
	ets:insert(Ets,{{heli,Name},X,Y,WorkState}),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	wait_done([Name]),											%%wait to the gen_fsm in the previous node to teminate first
	heli:recover(Name,MyName,X,Y,UnitState,UnitStateData,Stat),	%%create the heli in the current unit server \ node
	MonName = get(mon_name),
	loc_monitor:add_mon(MonName,global:whereis_name(Name)),		%%tell the monitor to add the new heli
	
	update_stat(message_count,{1,erlang:now()}),		%%update statistics
	{noreply, State};
	
%%check if there is fire in heli location
handle_cast({heli_fire_check,HeliName}, State) -> 
	Ets = get(ets_id),
	[{{heli,_},HX,HY,_}] = ets:lookup(Ets,{heli,HeliName}),
	
	%%find all fires that exists in heli location
	QH = qlc:q([[FName,FR,FX,FY] || {{fire,FName},FR,FX,FY} <- ets:table(Ets), ((FX-HX)*(FX-HX) + (FY-HY)*(FY-HY))< FR*FR]),

	case qlc:eval(QH) of
		[] -> Replay = false;
		FireList -> [Replay|_] = FireList	%%chose one of the fires in heli location
	end,
	
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	case Replay == false of
		true -> check_fire_other_server(MyName,HeliName,HX,HY,[tl,tr,bl,br]);	%%if no fire found, check if there is fire in different node on heli location
		false -> heli:found_fire(HeliName,Replay)								%%tell heli that he arrived to fire
	end,
	
	update_stat(message_count,{1,erlang:now()}),	%%update statistics
	{noreply, State};
	
%%check if there is fire in heli (from different node) location 
handle_cast({server_fire_check,HeliName,HX,HY},  State) -> 
	Ets = get(ets_id),
	
	%%find all fires that exists in heli location
	QH = qlc:q([[FName,FR,FX,FY] || {{fire,FName},FR,FX,FY} <- ets:table(Ets), ((FX-HX)*(FX-HX) + (FY-HY)*(FY-HY))< FR*FR]),

	case qlc:eval(QH) of
		[] -> Replay = false;
		FireList -> [Replay|_] = FireList %%chose one of the fires in heli location
	end,
	
	case Replay == false of
		true -> do_nothing;
		false -> heli:found_fire(HeliName,Replay)	%%tell heli that he arrived to fire
	end,
	
	update_stat(message_count,{1,erlang:now()}),	%%update statistics
	{noreply, State};

%%crash the unit server
handle_cast({crash,Num}, _State) ->
    {noreply, 1/Num};
	
%%print the unit server statistics
handle_cast({stat}, State) ->
	print_stat(),
	{noreply, State};
	
%%add fire in unit server
handle_cast({add_fire,FireName,X,Y}, State) ->
	Data = {{fire,FireName},?FireDefaultRadius,X,Y},
	Ets = get(ets_id),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	fire:start(FireName,MyName,?FireDefaultRadius,X,Y),	%%starting fire
	fire:start_sim(FireName),							%%tell fire to start simulation (start receive messages)
	ets:insert(Ets,Data),
	{noreply, State};
	
%%ignore any other cast
handle_cast(_Message, State) ->
    {noreply, State}.

%% Informative calls
% {noreply,NewState} 
% {noreply,NewState,Timeout} 
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
handle_info({_,{stop}}, State) ->
    {stop,normal,State};	

handle_info(_Message, _Server) -> 
    {noreply, _Server}.

%% Server termination
terminate(Reason, _Server) ->

	case Reason == normal of				%%check reason if normal or crash
		true ->
				MonName = get(mon_name),
				loc_monitor:stop(MonName),	%%stop local monitor
				
				Ets = get(ets_id),
				terminate_all_units(Ets),	%%terminate all units in same node
				QH = qlc:q([ Name	|| {{_,Name},_,_,_} <- ets:table(Ets), global:whereis_name(Name) /= undefined]),

				wait_done(qlc:eval(QH)),	%%wait for units termination
				[{_,Name}] = ets:lookup(Ets,myInfo),
				io:format("Generic termination handler: '~p' '~p' '~p'~n",[Reason, _Server,Name]);
		false -> Sen_Fire = get(sen_fire_id),
				 QH = qlc:q([Data || {{Sen,Fire},Data} <- ets:table(Sen_Fire), ets:member(Sen_Fire,{{Sen,Fire},done}) == false]),
				 FreeList = qlc:eval(QH),
				 %%free all helicopters in same node
				 [ unit_server:heli_done(ServerName,HeliName) || {HeliName, _, ServerName} <- FreeList]
	end,
	ok.

%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%choose the closest heli from list to sensor (sx,sy)
closest_heli([{Name,HX,HY}|T],SX,SY) -> 
	DistSquare = (HX-SX) * (HX-SX) + (HY-SY) * (HY-SY),		%%calculate distance from sensor
	closest_heli(T,{Name,DistSquare,HX,HY},SX,SY).			%%recursively check next heli in list

closest_heli([],{Name,BestDist,X,Y},_SX,_SY) -> {Name,X,Y,BestDist};	%%returns closest heli and its distance from the sensor
closest_heli([{Name,HX,HY}|T],{BestName,BestDist,BestX,BestY},SX,SY) ->
	DistSquare = (HX-SX) * (HX-SX) + (HY-SY) * (HY-SY),
	case DistSquare < BestDist of
		true -> closest_heli(T,{Name,DistSquare,HX,HY},SX,SY);			%%recursively check next heli, and choose current heli is cloest
		false -> closest_heli(T,{BestName,BestDist,BestX,BestY},SX,SY)	%%recursively check next heli, and stay with the last closest heli
	end.
	
%%choose the closest heli that received from the other nodes
servers_closest_heli([]) -> no_heli;
servers_closest_heli([{{_,_},{Name,Dist,Server}}|T]) -> servers_closest_heli(T,Name,Dist,Server,[]).

servers_closest_heli([],Name,_Dist,_Server,Rest) -> {Name,Rest};
servers_closest_heli([{{_,_},{NextName,NextDist,NextServer}}|T],Name,Dist,Server,Rest) ->
	case NextDist < Dist of 
		true -> servers_closest_heli(T,NextName,NextDist,NextServer,[{Name,Server}|Rest]);		%%recursively check next heli, and choose current heli is cloest
		false -> servers_closest_heli(T,Name,Dist,Server, [{NextName,NextServer}|Rest])			%%recursively check next heli, and stay with the last closest heli
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%check for overlapping fires	
overlappingFire(X1,Y1,X2,Y2,R1,R2)->
	DisSquare= (X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2),		%%square distance of the fires centers
    case (((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)) =< (R2+R1)*(R2+R1)) and (DisSquare>0)  of	%%check if they overlap and not with the same center
	    true->
			Dis = math:sqrt(DisSquare),
		    CosA= ((Dis*Dis)+(R1*R1)-(R2*R2))/(2*Dis*R1),
		    CosB= ((Dis*Dis)+(R2*R2)-(R1*R1))/(2*Dis*R2),
			case (((CosA>-1) and (CosA<1)) and ((CosA>-1) and (CosA<1))) of
				true ->
						SinA=2*math:acos(CosA),
						SinB=2*math:acos(CosB),
						Over=(R1*R1*math:acos(CosA))-(0.5*R1*R1*math:sin(SinA))+(R2*R2*math:acos(CosB))-(0.5*R2*R2*math:sin(SinB)),
						case Over*100/(math:pi()*R1*R1)>?OVERLAP_PERC of
							true -> Ans= true;		%%the fires overlap over ?OVERLAP_PERC precent
							false -> Ans = false	%%the fires dont overlap over ?OVERLAP_PERC precent
						end;
				false -> Ans = false
			end,
		    Ans;
	    false-> false
    end,
	case (DisSquare==0) and (R1<R2) of	%%check if the fire is the small one of two fires at the same place
		true -> true;
		false -> false
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%wait for gen_fsms in list to terminate
wait_done([]) -> ok;
wait_done([H|T]) -> 
	case global:whereis_name(H) of
		undefined -> wait_done(T);	%%unit terminated, wait for next unit
		_Any -> wait_done([H|T])	%%unit didn't terminated yet, wait for it to terminate
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%check if there is fire in X,Y coordinates in all other nodes
check_fire_other_server(_CurrentServerName,_HeliName,_X,_Y,[]) -> done;						%%no fire found
check_fire_other_server(CurrentServerName,HeliName,X,Y,[H|T]) when H==CurrentServerName -> 	
	check_fire_other_server(CurrentServerName,HeliName,X,Y,T);								%%skip the current node, already checked
check_fire_other_server(CurrentServerName,HeliName,X,Y,[H|T]) when H/=CurrentServerName -> 	
	case global:whereis_name(H) of
		undefined -> check_fire_other_server(CurrentServerName,HeliName,X,Y,T);				%%unit server doesn't exists, skip
		_Pid -> unit_server:fire_check(H,HeliName,X,Y),										%%check fires in coordinates
				check_fire_other_server(CurrentServerName,HeliName,X,Y,T)					%%check fires in next node
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

%%terminate all units
terminate_all_units(Ets) -> 
	%%go through all units and activate terminate
	QH = qlc:q([ {Type,Name} || {{Type,Name},_,_,_} <- ets:table(Ets), global:whereis_name(Name) /= undefined]),
	[ terminate_unit(Unit) || Unit <- qlc:eval(QH)].

%%terminate certain unit
terminate_unit({Type,Name}) ->
	case Type of
		heli -> heli:stop(Name);	%%stop heli
		fire -> fire:stop(Name);	%%stop fire
		sensor -> sensor:stop(Name)	%%stop sensor
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%create stat ets
create_stat()->
	Stat = ets:new(stat,[set]),
	put(stat,Stat),
	ets:insert(Stat,{start_sim_time,erlang:now()}),		%%start simulation time
	ets:insert(Stat,{message_count,{0,0}}),				%%how many messages handled vs time
	ets:insert(Stat,{heli_count,{0,0,0}}).				%%how many helis in unit server on average
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%update statistics
update_stat(Type,Value) ->
	
	Stat = get(stat),
	case Type of
		heli_count -> [{_,{Total,Count,_}}] = ets:lookup(Stat,heli_count),
					  ets:insert(Stat,{heli_count,{Total+Value,Count+1,Value}});	%%add heli amount, and add 1 to update count
		start_sim_time -> ets:insert(Stat,{start_sim_time,Value});					%%update start simulation time
		message_count -> [{_,{Total,_OldTime}}] = ets:lookup(Stat,message_count),
						 case ets:lookup(Stat,sim_started) == [] of
							true -> Seconds = 0,Message_diff=0;
							false -> [{_,Prev}] = ets:lookup(Stat,start_sim_time),
									 {Message_diff,CurrentTime} = Value,
									 Seconds = timer:now_diff(CurrentTime,Prev)/1000000
						 end,
						 ets:insert(Stat,{message_count,{Total+Message_diff,Seconds}})	%%update how many messages handled so far, and the time passed
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%print to shell all statistics
print_stat()->
	Ets = get(ets_id),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	io:format("server ~p statistics:~n",[MyName]),
	Stat=get(stat),
	[ chooseStat(CurrentStat) || CurrentStat<-ets:tab2list(Stat)]. %%print specific statistic

%%print specific statistic	
chooseStat({Type,Val}) ->
	case Type of
		heli_count -> {Total,Count,Current}=Val,
					  case Count == 0 of
						%% print how many helis in unit server right now and on average
						true -> io:format("----current amount of helis in server: ~p; ~n----average amount of helis in server: ~p~n",[undefind,undefind]);
						false ->  io:format("----current amount of helis in server: ~p; ~n----average amount of helis in server: ~p~n",[Current,Total/Count])
					  end;
		message_count -> {Total,Seconds}=Val,
						  case Seconds == 0 of
							%%prints average amount of messages handled by the unit server per second
							true -> io:format("----simulation not started yet~n");
							false ->  io:format("----amount of handled messages per seconds: ~p~n",[Total/Seconds])
						  end;			  
		_Any -> do_nothing
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%checks there is ping too all nodes
watch_dog() ->
	net_adm:ping(?TLSERVER_NODE),
	net_adm:ping(?TRSERVER_NODE),
	net_adm:ping(?BLSERVER_NODE),
	net_adm:ping(?BRSERVER_NODE),
	timer:sleep(1000),
	watch_dog().