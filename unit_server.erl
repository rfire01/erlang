%%====================================================================
%%
%% @author Juanse Perez Herrero <juanseph@gmail.com> [http://bytefilia.com]
%% @copyright CC Attribution - 2013
%%
%% A sample otp gen_server template
%%
%%====================================================================
-module(unit_server).

-include_lib("stdlib/include/qlc.hrl").

-behaviour(gen_server).

% interface calls
-export([start/1,create/2,update/3,pass_server_alert/2,heli_request/3,
		 heli_done/2,fire_check/2,fire_check/3,wx_update/1,start_sim/1,
		 change_screen/5,transfer_heli/4]).
    
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
	 io:format("######create~n",[]),
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
	
heli_done(GenName,Name) -> 
    gen_server:cast({global, GenName}, {heli_done,Name}).

fire_check(GenName,Name) -> 
	try gen_server:call({global,GenName},{heli_fire_check,Name},3000) catch _Error:_Reason -> error_in_server end.
	
fire_check(GenName,X,Y) -> 
	try gen_server:call({global,GenName},{server_fire_check,X,Y},3000) catch _Error:_Reason -> error_in_server end.
   
wx_update(GenName) ->
  gen_server:call({global,GenName},{wx_request}).
  
change_screen(GenName,NewServer,UnitInfo,UnitState,UnitStateData) -> 
    gen_server:cast({global, GenName}, {change_screen,NewServer,UnitInfo,UnitState,UnitStateData}).
	
transfer_heli(GenName,UnitInfo,UnitState,UnitStateData) -> 
    gen_server:cast({global, GenName}, {transfer_heli,UnitInfo,UnitState,UnitStateData}).
  
%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Name]) ->
    io:format("starting local_gen: ~p~n",[Name]),
	Ets = ets:new(general_info,[set]),
	put(ets_id,Ets),
	ets:insert(Ets,{myInfo,Name}),
	Sen_fire = ets:new(sen_fire,[set]),
	put(sen_fire_id,Sen_fire),
	%io:format("######end of init gen server~n",[]),
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
	
handle_call({heli_fire_check,HeliName}, _From, State) -> 
	Ets = get(ets_id),
	[{{heli,_},HX,HY,_}] = ets:lookup(Ets,{heli,HeliName}),
	
	QH = qlc:q([[FName,FR,FX,FY] || {{fire,FName},FR,FX,FY} <- ets:table(Ets), ((FX-HX)*(FX-HX) + (FY-HY)*(FY-HY))< FR*FR]),

	case qlc:eval(QH) of
		[] -> Replay = false;
		FireList -> [Replay|_] = FireList
	end,
	
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	case Replay == false of
		true -> Replay2 = check_fire_other_server(MyName,HX,HY,[tl,tr,bl,br]);
		false -> Replay2 = Replay
	end,
	
	{reply,Replay2,State};
	
handle_call({server_fire_check,HX,HY}, _From, State) -> 
	Ets = get(ets_id),
	
	QH = qlc:q([[FName,FR,FX,FY] || {{fire,FName},FR,FX,FY} <- ets:table(Ets), ((FX-HX)*(FX-HX) + (FY-HY)*(FY-HY))< FR*FR]),

	case qlc:eval(QH) of
		[] -> Replay = false;
		FireList -> [Replay|_] = FireList
	end,
	{reply,Replay,State};

handle_call(Message, From, State) -> 
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

%% Asynchronous, possible return values
% {noreply,NewState} 
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast({create,DataList}, State) ->
	Ets = get(ets_id),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	
	%io:format("stoping all old fsms ~n"),
	ObjList = ets:tab2list(Ets),
	[gen_fsm:stop({global,H}) || {{_,H},_,_,_} <- ObjList, global:whereis_name(H) /=undefined],
	ets:delete_all_objects(Ets),
	
	ets:insert(Ets,{myInfo,MyName}),
	
	io:format("insert data: ~p~n",[DataList]),
	ets:insert(Ets,DataList),
	%io:format("starting helicopters ~n"),
	HeliList = ets:match(Ets,{{heli,'$1'},'$2','$3','_'}),
	[heli:start(Name,MyName,X,Y) || [Name,X,Y] <- HeliList],
	FireList = ets:match(Ets,{{fire,'$1'},'$2','$3','$4'}),
	%io:format("starting fires ~p~n",[FireList]),
	[fire:start(Name,MyName,R,X,Y) || [Name,R,X,Y] <- FireList],
	%io:format("starting sensors ~n"),
	SenList = ets:match(Ets,{{sensor,'$1'},'$2','$3','$4'}),
	[sensor:start(Name,MyName) || [Name,_,_,_] <- SenList],
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
				   case qlc:eval(QH)  of
						[]-> wait_for_free_heli;%, io:format("wait_for_free_heli ~n");
						HeliList -> [{{_,_},_,SX,SY}] = ets:lookup(Ets,{sensor,Sen_name}),
									{Name,X,Y} = closest_heli(HeliList,SX,SY),
									ets:insert(Sen_fire,{{Sen_name,Fire_Name},true}),
									ets:insert(Ets,{{heli,Name},X,Y,working}),
									[{{_,_},SR,SX,SY}] = ets:lookup(Ets,{sensor,Sen_name}),
									%io:format("sending heli ~p~n",[Name]),
									heli:move_dst(Name,SX+SR,SY,{SR,SX,SY,0})
				   end
	end,
	
	{noreply, State};	
					
handle_cast({heli_done,Name}, State) ->
	Ets = get(ets_id),
	%io:format("helicopter: ~p is free ~n",[Name]),
	ets:update_element(Ets,{heli,Name},{4,not_working}),
	{noreply, State};
	
handle_cast({change_screen,NewServer,UnitInfo,UnitState,UnitStateData}, State) ->
	Ets = get(ets_id),
	[Name,X,Y] = UnitInfo,
	[{{_,_},_,_,WorkState}] = ets:lookup(Ets,{heli,Name}),
	ets:delete(Ets,{heli,Name}),
	wait_done([Name]),
	
	%[{_,MyName}] = ets:lookup(Ets,myInfo),
	%io:format("transfer from ~p to ~p~n",[MyName,NewServer]),
	
	unit_server:transfer_heli(NewServer,[Name,X,Y,WorkState],UnitState,UnitStateData),
	
	{noreply, State};
	
handle_cast({transfer_heli,UnitInfo,UnitState,UnitStateData}, State) ->
	Ets = get(ets_id),
	[Name,X,Y,WorkState] = UnitInfo,
	ets:insert(Ets,{{heli,Name},X,Y,WorkState}),
	[{_,MyName}] = ets:lookup(Ets,myInfo),
	
	heli:recover(Name,MyName,X,Y,UnitState,UnitStateData),
	
	{noreply, State};

%% generic async handler
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
	Ets = get(ets_id),
	QH = qlc:q([ Name	|| {{_,Name},_,_,_} <- ets:table(Ets), global:whereis_name(Name) /= undefined]),
	%io:format("qlc = ~p~n",[qlc:eval(QH)]),
	[gen_fsm:stop({global,Name}) || Name <- qlc:eval(QH)],
	%ObjList = ets:tab2list(Ets),
	%[gen_fsm:send_all_state_event({global,H},stop) || {{_,H},_,_,_} <- ObjList, global:whereis_name(H) /=undefined],
	%timer:sleep(2000),
	wait_done(qlc:eval(QH)),
	[{_,Name}] = ets:lookup(Ets,myInfo),
    io:format("Generic termination handler: '~p' '~p' '~p'~n",[Reason, _Server,Name]),
	ok.



%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


closest_heli([{Name,HX,HY}|T],SX,SY) -> 
	DistSquare = (HX-SX) * (HX-SX) + (HY-SY) * (HY-SY),
	closest_heli(T,{Name,DistSquare,HX,HY},SX,SY).

closest_heli([],{Name,_BestDist,X,Y},_SX,_SY) -> {Name,X,Y};
closest_heli([{Name,HX,HY}|T],{BestName,BestDist,BestX,BestY},SX,SY) ->
	DistSquare = (HX-SX) * (HX-SX) + (HY-SY) * (HY-SY),
	case DistSquare < BestDist of
		true -> closest_heli(T,{Name,DistSquare,HX,HY},SX,SY);
		false -> closest_heli(T,{BestName,BestDist,BestX,BestY},SX,SY)
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

check_fire_other_server(_CurrentServerName,_X,_Y,[]) -> false;
check_fire_other_server(CurrentServerName,X,Y,[H|T]) when H==CurrentServerName -> 
	check_fire_other_server(CurrentServerName,X,Y,T);
check_fire_other_server(CurrentServerName,X,Y,[H|T]) when H/=CurrentServerName -> 	
	case global:whereis_name(H) of
		undefined -> check_fire_other_server(CurrentServerName,X,Y,T);
		_Pid -> case unit_server:fire_check(H,X,Y) of
					false -> check_fire_other_server(CurrentServerName,X,Y,T);
					error_in_server -> check_fire_other_server(CurrentServerName,X,Y,T);
					Any -> Any
				end
	end.
	