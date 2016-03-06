
-module(unit_server).

-include_lib("stdlib/include/qlc.hrl").

-behaviour(gen_server).

% interface calls
-export([start/1,create/2,update/3,pass_server_alert/2,heli_request/3,heli_request/5,
		 give_heli/3,choose_heli/3,heli_done/2,fire_check/2,fire_check/4,
		 wx_update/1,start_sim/1,change_screen/6,transfer_heli/5,
		 crash/1,crash_recover/2,stop/1]).
    
% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2, code_change/3]).
-include("config.hrl").		 
%-define(OVERLAP_PERC, 90).
%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start(GenName) -> 
    gen_server:start({global, GenName}, ?MODULE, [GenName], []).
	 
start_sim(GenName) -> 
    gen_server:cast({global, GenName}, {start_sim}).
	
create(GenName,Data) -> 
	%Data = [{{sensor,sensor1},10,10,10},{{fire,fire1},1,30,10},{{heli,heli1},7,85,not_working}],%[ {{heli,heli1},7,85,not_working},{{heli,heli2},800,45,not_working},{{heli,heli3},180,340,not_working},
			% {{fire,fire1},7,85,50},{{fire,fire2},800,123,50},{{fire,fire3},12,230,50},
			 %{{sensor,sensor1},14,47,10},{{sensor,sensor2},314,147,10},{{sensor,sensor3},140,470,10}],
    gen_server:cast({global, GenName}, {create,Data}).
	
update(GenName,Type,Data) -> 
    gen_server:cast({global, GenName}, {update,Type,Data}).
	
pass_server_alert(GenName,Data) -> 
    gen_server:cast({global, GenName}, {pass_alert,Data}).
	
heli_request(GenName,Sname,Fname) -> 
    gen_server:cast({global, GenName}, {heli_request,Sname,Fname}).
	
heli_request(GenName,FromServer,Key,BestHeli,DstCords) -> 
    gen_server:cast({global, GenName}, {server_heli_request,FromServer,Key,BestHeli,DstCords}).
	
give_heli(GenName,Key,Val) -> 
    gen_server:cast({global, GenName}, {give_heli,Key,Val}).
	
choose_heli(GenName,Key,Destination) -> 
    gen_server:cast({global, GenName}, {choose_heli,Key,Destination}).	
	
heli_done(GenName,Name) -> 
    gen_server:cast({global, GenName}, {heli_done,Name}).

fire_check(GenName,Name) -> 
	%try gen_server:call({global,GenName},{heli_fire_check,Name},3000) catch _Error:_Reason -> error_in_server end.
	gen_server:cast({global, GenName}, {heli_fire_check,Name}).
	
fire_check(GenName,Name,X,Y) -> 
	%try gen_server:call({global,GenName},{server_fire_check,X,Y},3000) catch _Error:_Reason -> error_in_server end.
	gen_server:cast({global, GenName}, {server_fire_check,Name,X,Y}).
   
wx_update(GenName) ->
	try gen_server:call({global,GenName},{wx_request},infinity) catch _Error:_Reason -> server_off end.
  
change_screen(GenName,NewServer,UnitInfo,UnitState,UnitStateData,Stat) -> 
    gen_server:cast({global, GenName}, {change_screen,NewServer,UnitInfo,UnitState,UnitStateData,Stat}).
	
transfer_heli(GenName,UnitInfo,UnitState,UnitStateData,Stat) -> 
	%[Name,_,_,_] = UnitInfo,
	%io:format("add heli ~p request to server ~p~n",[Name,GenName]),
    gen_server:cast({global, GenName}, {transfer_heli,UnitInfo,UnitState,UnitStateData,Stat}).
	
crash(GenName) ->
	gen_server:cast({global, GenName}, {crash,0}).
	
crash_recover(GenName,Data) ->
	gen_server:start({global, GenName}, ?MODULE, [crash,Data], []).
	
stop(Name) ->
	gen_server:cast({global, Name}, {stop}).
  
%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Name]) ->
    io:format("starting local_gen: ~p~n",[Name]),
	
	MonName = list_to_atom(atom_to_list(Name) ++ "mon"),
	loc_monitor:start(MonName),
	loc_monitor:add_mon(MonName,global:whereis_name(Name)),
	put(mon_name,MonName),
	MonPid = global:whereis_name(MonName),
	
	Ets = ets:new(general_info,[set,{heir,MonPid , {server,Name}}]),
	put(ets_id,Ets),
	ets:insert(Ets,{myInfo,Name}),
	Sen_fire = ets:new(sen_fire,[bag,public]),
	put(sen_fire_id,Sen_fire),
	%io:format("######end of init gen server~n",[]),
    {ok, initialized};
	
init([crash,Data]) ->

	Ets=ets:new(general_info,[set]),
	put(ets_id,Ets),
	ets:insert(Ets,Data),
	[{_,Name}] = ets:lookup(Ets,myInfo),
	
	MonName = list_to_atom(atom_to_list(Name) ++ "mon"),
	MonPid = global:whereis_name(MonName),
	
	ets:setopts(Ets,{heir,MonPid , {server,Name}}),
	loc_monitor:add_mon(MonName,global:whereis_name(Name)),

	Sen_fire = ets:new(sen_fire,[bag,public]),
	put(sen_fire_id,Sen_fire),
	
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
	
	QH = qlc:q([{{Type,Name},Field1,Field2,Field3}	|| {{Type,Name},Field1,Field2,Field3} <- ets:table(Ets), ((Type==fire) or (Type==heli))]),
	
	{reply,qlc:eval(QH),State};
	
%handle_call({heli_fire_check,HeliName}, _From, State) -> 
%	Ets = get(ets_id),
%	[{{heli,_},HX,HY,_}] = ets:lookup(Ets,{heli,HeliName}),
%	
%	QH = qlc:q([[FName,FR,FX,FY] || {{fire,FName},FR,FX,FY} <- ets:table(Ets), ((FX-HX)*(FX-HX) + (FY-HY)*(FY-HY))< FR*FR]),
%
%	case qlc:eval(QH) of
%		[] -> Replay = false;
%		FireList -> [Replay|_] = FireList
%	end,
%	
%	[{_,MyName}] = ets:lookup(Ets,myInfo),
%	case Replay == false of
%		true -> Replay2 = check_fire_other_server(MyName,HX,HY,[tl,tr,bl,br]);
%		false -> Replay2 = Replay
%	end,
%	
%	{reply,Replay2,State};
%	
%handle_call({server_fire_check,HX,HY}, _From, State) -> 
%	Ets = get(ets_id),
%	
%	QH = qlc:q([[FName,FR,FX,FY] || {{fire,FName},FR,FX,FY} <- ets:table(Ets), ((FX-HX)*(FX-HX) + (FY-HY)*(FY-HY))< FR*FR]),
%
%	case qlc:eval(QH) of
%		[] -> Replay = false;
%		FireList -> [Replay|_] = FireList
%	end,
%	{reply,Replay,State};

handle_call(Message, From, State) -> 
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

%% Asynchronous, possible return values
% {noreply,NewState} 
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast({stop}, State) ->
    {stop,normal,State};	


handle_cast({create,DataList}, State) ->
	Ets = get(ets_id),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	
	%QH2 = qlc:q([Name || {{_,Name},_,_,_} <- ets:table(Ets)]),
	%[ io:format("unit ~p is ~p~n",[TName,global:whereis_name(TName)]) || TName <- qlc:eval(QH2)],
	
	%io:format("stoping all old fsms ~n"),
	%ObjList = ets:tab2list(Ets),
	terminate_all_units(Ets),
	%[gen_fsm:stop({global,H}) || {{Type,H},_,_,_} <- ObjList, global:whereis_name(H) /=undefined],
	%AllUnits = [H || {{_,H},_,_,_} <- ObjList],%, global:whereis_name(H) /=undefined],
	%[ gen_fsm:stop({global,Unit}) || Unit <- AllUnits, global:whereis_name(Unit) /=undefined],
	%wait_done(AllUnits),
	ets:delete_all_objects(Ets),
	
	ets:insert(Ets,{myInfo,MyName}),
	
	%io:format("insert data: ~p~n",[DataList]),
	ets:insert(Ets,DataList),
	
	QH = qlc:q([Name || {{_,Name},_,_,_} <- ets:table(Ets)]),
	wait_done(qlc:eval(QH)),
	%io:format("starting helicopters ~n"),
	HeliList = ets:match(Ets,{{heli,'$1'},'$2','$3','_'}),
	[heli:start(Name,MyName,X,Y) || [Name,X,Y] <- HeliList],
	FireList = ets:match(Ets,{{fire,'$1'},'$2','$3','$4'}),
	%io:format("starting fires ~p~n",[FireList]),
	[fire:start(Name,MyName,R,X,Y) || [Name,R,X,Y] <- FireList],
	%io:format("starting sensors ~n"),
	SenList = ets:match(Ets,{{sensor,'$1'},'$2','$3','$4'}),
	[sensor:start(Name,MyName) || [Name,_,_,_] <- SenList],
	
	MonName = get(mon_name),
	[loc_monitor:add_mon(MonName,global:whereis_name(Name)) || [Name,_,_,_] <- SenList],
	[loc_monitor:add_mon(MonName,global:whereis_name(Name)) || [Name,_,_,_] <- FireList],
	[loc_monitor:add_mon(MonName,global:whereis_name(Name)) || [Name,_,_] <- HeliList],
	
    {noreply, State};
	
handle_cast({start_sim}, State) ->
	%io:format("starting simulation ~n"),
	Ets = get(ets_id),
	HeliList = ets:match(Ets,{{heli,'$1'},'_','_','_'}),
	[heli:start_sim(Name) || [Name] <- HeliList],
	FireList = ets:match(Ets,{{fire,'$1'},'_','_','_'}),
	[fire:start_sim(Name) || [Name] <- FireList],
	SenList = ets:match(Ets,{{sensor,'$1'},'_','_','_'}),
	[sensor:start_sim(Name) || [Name] <- SenList],
	{noreply, State};
	
handle_cast({update,Unit_Type,Unit_Data}, State) ->
	%io:format("enter update : Unit_Type = ~p,Unit_Data=~p ~n",[Unit_Type,Unit_Data]),
	Ets = get(ets_id),
	%io:format("1~n",[]),
	case Unit_Type of
		heli -> [Name,X,Y]=Unit_Data,	
				%io:format("updating helicopter:~p (~p,~p) ~p~n",[Name,X,Y,Status]),
				[{{_,_},_,_,Status}]= ets:lookup(Ets,{heli,Name}),
				ets:insert(Ets,{{heli,Name},X,Y,Status});
		fire -> [Name,RF]=Unit_Data,		%io:format("updating fire: ~p~n",[Name]),
				[{{fire,_},_,XF,YF}] = ets:lookup(Ets,{fire,Name}),
				ets:insert(Ets,{{fire,Name},RF,XF,YF}),
				
				%%merge fires
				QH_Merge = qlc:q([FName || {{fire,FName},F2R,F2X,F2Y} <- ets:table(Ets),
							    FName/=Name,F2R>0,RF>0, overlappingFire(XF,YF,F2X,F2Y,RF,F2R)==true ]),
				
				case qlc:eval(QH_Merge) of
					[] -> dont_care;
 					_Any ->%io:format("QH_Merge:~p ~n",[qlc:eval(QH_Merge)]),
 					fire:merge(Name)		
				end,
				
				%%send alerts to all servers
				QH = qlc:q([SName || {{sensor,SName},RS,XS,YS} <- ets:table(Ets), ((XF-XS)*(XF-XS) + (YF-YS)*(YF-YS))< (RS+RF)*(RS+RF)]),

				case qlc:eval(QH) of
					[] -> dont_care;
					SensorList -> [ sensor:new_alert(Sen,Name) || Sen <- SensorList]%, io:format("sensor activated ~p~n",[SensorList])
				end,
				[{_,MyName}] = ets:lookup(Ets,myInfo),
				[ unit_server:pass_server_alert(DstServer,[Name,RF,XF,YF]) || DstServer <- [tl,tr,bl,br], DstServer/=MyName]
	end,
	{noreply, State};	
	
handle_cast({pass_alert,Fire_Data}, State) ->
	Ets = get(ets_id),
	[Name,RF,XF,YF] = Fire_Data,
	QH = qlc:q([SName || {{sensor,SName},RS,XS,YS} <- ets:table(Ets), ((XF-XS)*(XF-XS) + (YF-YS)*(YF-YS))< (RS+RF)*(RS+RF)]),

	case qlc:eval(QH) of
		[] -> dont_care;
		SensorList -> [ sensor:new_alert(Sen,Name) || Sen <- SensorList]%, io:format("sensor activated ~p~n",[SensorList])
	end,
	{noreply,State};
	
	
handle_cast({heli_request,Sen_name,Fire_Name}, State) ->
	Ets = get(ets_id),
	Sen_fire = get(sen_fire_id),
	%io:format("requesting heli ~n"),
	Exists = ets:member(Sen_fire,{Sen_name,Fire_Name}),
	case Exists of
		true -> do_nothing;%, io:format("heli already sent ~n");
		false ->  QH = qlc:q([{HName,HX,HY} || {{heli,HName},HX,HY,not_working} <- ets:table(Ets)]),
				  [{{_,_},SR,SX,SY}] = ets:lookup(Ets,{sensor,Sen_name}),
				  [{_,MyName}] = ets:lookup(Ets,myInfo),
				   case qlc:eval(QH)  of
						[]-> Dist = ?Horizontal * ?Horizontal * ?Horizontal;%, io:format("wait_for_free_heli ~n");
						HeliList -> 
									{Name,X,Y,Dist} = closest_heli(HeliList,SX+SR,SY),
									ets:insert(Sen_fire,{{Sen_name,Fire_Name},{Name,Dist,MyName}}),
									ets:insert(Ets,{{heli,Name},X,Y,working})
									%[{{_,_},SR,SX,SY}] = ets:lookup(Ets,{sensor,Sen_name}),
									%heli:move_dst(Name,SX+SR,SY,{SR,SX,SY,0})
									
									
				   end,
				   ets:delete(Sen_fire,{{Sen_name,Fire_Name},done}),
				   [ unit_server:heli_request(Serv,MyName,{Sen_name,Fire_Name},Dist,{SX+SR,SY})|| Serv <- [tl,tr,bl,br], Serv /=MyName],
				   spawn(fun() -> timer:sleep(1000), unit_server:choose_heli(MyName,{Sen_name,Fire_Name},{SR,SX,SY}) end)
	end,
	
	{noreply, State};	
	
handle_cast({server_heli_request,FromServer,Key,MinDist,{DstX,DstY}}, State) ->
	Ets = get(ets_id),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	QH = qlc:q([{HName,HX,HY} || {{heli,HName},HX,HY,not_working} <- ets:table(Ets), (DstX-HX) * (DstX-HX) + (DstY-HY) * (DstY-HY) < MinDist]),
	case qlc:eval(QH) of
		[] -> do_nothing;
		HeliList -> {Name,X,Y,Dist} = closest_heli(HeliList,DstX,DstY),
					ets:insert(Ets,{{heli,Name},X,Y,working}),
					unit_server:give_heli(FromServer,Key,{Name,Dist,MyName})
	end,
	{noreply, State};
	
handle_cast({give_heli,Key,Val}, State) ->
	%Ets = get(ets_id),
	%[{_,MyName}] = ets:lookup(Ets,myInfo),
	
	Sen_fire = get(sen_fire_id),
	
	case ets:member(Sen_fire,{Key,done}) of
		false -> ets:insert(Sen_fire,{Key,Val});
		true -> {Name,_,Serv}=Val,
				unit_server:heli_done(Serv,Name)
	end,
	%io:format("heli added = ~p, key = ~p in server ~p~n",[Val,Key,MyName]),
	{noreply, State};
	
handle_cast({choose_heli,Key,{R,X,Y}}, State) ->
	Ets = get(ets_id),
	[{_,Name}] = ets:lookup(Ets,myInfo),
	Sen_fire = get(sen_fire_id),
	ets:insert(Sen_fire,{{Key,done},done}),
	case ets:member(Sen_fire,Key) of
		true -> HeliList = ets:lookup(Sen_fire,Key);
		false -> HeliList=[]
	end,
	%Ets = get(ets_id),
	%[{_,MyName}] = ets:lookup(Ets,myInfo),
	%io:format("heli list = ~p, Key = ~p in server ~p~n",[HeliList,Key,MyName]),
	case servers_closest_heli(HeliList) of
		{ChoosenHeli,OtherHeli} -> [ unit_server:heli_done(HeliServer,Heli) || {Heli,HeliServer} <- OtherHeli],
									heli:move_dst(ChoosenHeli,X+R,Y,{R,X,Y,0}),
									%wait 10 second and allow another heli request from sensor-fire pair
									spawn(fun() -> timer:sleep(10000), case global:whereis_name(Name) == undefined of false -> ets:delete(Sen_fire,Key); true-> do_nothing end end);
		no_heli -> do_nothing
	end,
	{noreply, State};
					
handle_cast({heli_done,Name}, State) ->
	Ets = get(ets_id),
	%io:format("helicopter: ~p is free ~n",[Name]),
	ets:update_element(Ets,{heli,Name},{4,not_working}),
	{noreply, State};
	
handle_cast({change_screen,NewServer,UnitInfo,UnitState,UnitStateData,Stat}, State) ->
	Ets = get(ets_id),
	[Name,X,Y] = UnitInfo,
	[{{_,_},_,_,WorkState}] = ets:lookup(Ets,{heli,Name}),
	ets:delete(Ets,{heli,Name}),
	%wait_done([Name]),
	
	
	%[{_,MyName}] = ets:lookup(Ets,myInfo),
	%io:format("transfer ~p from ~p to ~p~n",[Name,MyName,NewServer]),
	
	unit_server:transfer_heli(NewServer,[Name,X,Y,WorkState],UnitState,UnitStateData,Stat),
	
	{noreply, State};
	
handle_cast({transfer_heli,UnitInfo,UnitState,UnitStateData,Stat}, State) ->
	Ets = get(ets_id),
	[Name,X,Y,WorkState] = UnitInfo,
	ets:insert(Ets,{{heli,Name},X,Y,WorkState}),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	%io:format("recovering heli ~p at ~p~n",[Name,MyName]),
	wait_done([Name]),
	heli:recover(Name,MyName,X,Y,UnitState,UnitStateData,Stat),
	MonName = get(mon_name),
	loc_monitor:add_mon(MonName,global:whereis_name(Name)),
	%io:format("result of tranfer of heli ~p to server ~p, gave ~p~n",[Name,MyName,Res]),
	
	{noreply, State};
	
handle_cast({heli_fire_check,HeliName}, State) -> 
	Ets = get(ets_id),
	[{{heli,_},HX,HY,_}] = ets:lookup(Ets,{heli,HeliName}),
	
	QH = qlc:q([[FName,FR,FX,FY] || {{fire,FName},FR,FX,FY} <- ets:table(Ets), ((FX-HX)*(FX-HX) + (FY-HY)*(FY-HY))< FR*FR]),

	case qlc:eval(QH) of
		[] -> Replay = false;
		FireList -> [Replay|_] = FireList
	end,
	
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	case Replay == false of
		true -> check_fire_other_server(MyName,HeliName,HX,HY,[tl,tr,bl,br]);
		false -> heli:found_fire(HeliName,Replay)
	end,
	
	
	{noreply, State};
	
handle_cast({server_fire_check,HeliName,HX,HY},  State) -> 
	Ets = get(ets_id),
	
	QH = qlc:q([[FName,FR,FX,FY] || {{fire,FName},FR,FX,FY} <- ets:table(Ets), ((FX-HX)*(FX-HX) + (FY-HY)*(FY-HY))< FR*FR]),

	case qlc:eval(QH) of
		[] -> Replay = false;
		FireList -> [Replay|_] = FireList
	end,
	
	case Replay == false of
		true -> do_nothing;
		false -> heli:found_fire(HeliName,Replay)
	end,
	
	{noreply, State};

handle_cast({crash,Num}, _State) ->
    {noreply, 1/Num};
	
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
terminate(Reason, _Server) ->

	case Reason == normal of
		true ->
				MonName = get(mon_name),
				loc_monitor:stop(MonName),
				
				Ets = get(ets_id),
				terminate_all_units(Ets),
				QH = qlc:q([ Name	|| {{_,Name},_,_,_} <- ets:table(Ets), global:whereis_name(Name) /= undefined]),
				%io:format("qlc = ~p~n",[qlc:eval(QH)]),
				%[gen_fsm:stop({global,Name}) || Name <- qlc:eval(QH)],
				%ObjList = ets:tab2list(Ets),
				%[gen_fsm:send_all_state_event({global,H},stop) || {{_,H},_,_,_} <- ObjList, global:whereis_name(H) /=undefined],
				%timer:sleep(2000),
				wait_done(qlc:eval(QH)),
				[{_,Name}] = ets:lookup(Ets,myInfo),
				io:format("Generic termination handler: '~p' '~p' '~p'~n",[Reason, _Server,Name]);
		false -> Sen_Fire = get(sen_fire_id),
				 QH = qlc:q([Data || {{Sen,Fire},Data} <- ets:table(Sen_Fire), ets:member(Sen_Fire,{{Sen,Fire},done}) == false]),
				 FreeList = qlc:eval(QH),
				 [ unit_server:heli_done(ServerName,HeliName) || {HeliName, _, ServerName} <- FreeList]
	end,
	ok.



%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


closest_heli([{Name,HX,HY}|T],SX,SY) -> 
	DistSquare = (HX-SX) * (HX-SX) + (HY-SY) * (HY-SY),
	closest_heli(T,{Name,DistSquare,HX,HY},SX,SY).

closest_heli([],{Name,BestDist,X,Y},_SX,_SY) -> {Name,X,Y,BestDist};
closest_heli([{Name,HX,HY}|T],{BestName,BestDist,BestX,BestY},SX,SY) ->
	DistSquare = (HX-SX) * (HX-SX) + (HY-SY) * (HY-SY),
	case DistSquare < BestDist of
		true -> closest_heli(T,{Name,DistSquare,HX,HY},SX,SY);
		false -> closest_heli(T,{BestName,BestDist,BestX,BestY},SX,SY)
	end.
	
servers_closest_heli([]) -> no_heli;
servers_closest_heli([{{_,_},{Name,Dist,Server}}|T]) -> servers_closest_heli(T,Name,Dist,Server,[]).

servers_closest_heli([],Name,_Dist,_Server,Rest) -> {Name,Rest};
servers_closest_heli([{{_,_},{NextName,NextDist,NextServer}}|T],Name,Dist,Server,Rest) ->
	case NextDist < Dist of 
		true -> servers_closest_heli(T,NextName,NextDist,NextServer,[{Name,Server}|Rest]);
		false -> servers_closest_heli(T,Name,Dist,Server, [{NextName,NextServer}|Rest])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
overlappingFire(X1,Y1,X2,Y2,R1,R2)->
    case ((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)) =< (R2+R1)*(R2+R1) of
	    true->  Dis= math:sqrt((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2)),
		    %io:format("Dis: ~p ~n",[Dis]),
		    CosA= ((Dis*Dis)+(R1*R1)-(R2*R2))/(2*Dis*R1),
		    %io:format("CosA: ~p ,",[CosA]),
		    CosB= ((Dis*Dis)+(R2*R2)-(R1*R1))/(2*Dis*R2),
		    %io:format("CosB: ~p ~n",[CosB]),
			case (((CosA>-1) and (CosA<1)) and ((CosA>-1) and (CosA<1))) of
				true ->
						SinA=2*math:acos(CosA),
						SinB=2*math:acos(CosB),
						Over=(R1*R1*math:acos(CosA))-(0.5*R1*R1*math:sin(SinA))+(R2*R2*math:acos(CosB))-(0.5*R2*R2*math:sin(SinB)),
						Ans= Over*100/(math:pi()*R1*R1)>?OVERLAP_PERC;
				false -> Ans = false
			end,
		    Ans;
	    false->false
    end.
		    
    %Temp = math:acos(math:cos(Rad)).
    %Temp * 180 / math:pi()==Deg.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wait_done([]) -> ok;
wait_done([H|T]) -> 
	case global:whereis_name(H) of
		undefined -> wait_done(T);
		_Any -> wait_done([H|T])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_fire_other_server(_CurrentServerName,_HeliName,_X,_Y,[]) -> done;
check_fire_other_server(CurrentServerName,HeliName,X,Y,[H|T]) when H==CurrentServerName -> 
	check_fire_other_server(CurrentServerName,HeliName,X,Y,T);
check_fire_other_server(CurrentServerName,HeliName,X,Y,[H|T]) when H/=CurrentServerName -> 	
	case global:whereis_name(H) of
		undefined -> check_fire_other_server(CurrentServerName,HeliName,X,Y,T);
		_Pid -> unit_server:fire_check(H,HeliName,X,Y),
				check_fire_other_server(CurrentServerName,HeliName,X,Y,T)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
terminate_all_units(Ets) -> 
	QH = qlc:q([ {Type,Name} || {{Type,Name},_,_,_} <- ets:table(Ets), global:whereis_name(Name) /= undefined]),
	[ terminate_unit(Unit) || Unit <- qlc:eval(QH)].

terminate_unit({Type,Name}) ->
	case Type of
		heli -> heli:stop(Name);
		fire -> fire:stop(Name);
		sensor -> sensor:stop(Name)
	end.