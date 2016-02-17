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
-export([start/1,create/2,update/3,heli_request/3,heli_done/2,
		 start_sim/1]).
    
% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2, code_change/3]).
		 
-define(OVERLAP_PERC, 90).
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
	
heli_request(GenName,Sname,Fname) -> 
    gen_server:cast({global, GenName}, {heli_request,Sname,Fname}).
	
heli_done(GenName,Name) -> 
    gen_server:cast({global, GenName}, {heli_done,Name}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Name]) ->
    io:format("starting local_gen: ~p~n",[Name]),
	ets:new(general_info,[set,named_table]),
	ets:insert(general_info,{myInfo,Name}),
	ets:new(sen_fire,[set,named_table]),
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
	{reply,ets:tab2list(general_info),State};
	
handle_call({heli_fire_check,HeliName}, _From, State) -> 
	[{{heli,_},HX,HY,_}] = ets:lookup(general_info,{heli,HeliName}),
	
	QH = qlc:q([[FName,FR,FX,FY] || {{fire,FName},FR,FX,FY} <- ets:table(general_info), ((FX-HX)*(FX-HX) + (FY-HY)*(FY-HY))< FR*FR]),

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
	
	[{_,MyName}] = ets:lookup(general_info,myInfo),
	
	io:format("stoping all old fsms ~n"),
	ObjList = ets:tab2list(general_info),
	[gen_fsm:stop({global,H}) || {{_,H},_,_,_} <- ObjList, global:whereis_name(H) /=undefined],
	ets:delete_all_objects(general_info),
	
	ets:insert(general_info,{myInfo,MyName}),

	io:format("insert data: ~p~n",[DataList]),
	ets:insert(general_info,DataList),
	io:format("starting helicopters ~n"),
	HeliList = ets:match(general_info,{{heli,'$1'},'$2','$3','_'}),
	[heli:start(Name,MyName,X,Y) || [Name,X,Y] <- HeliList],
	FireList = ets:match(general_info,{{fire,'$1'},'$2','$3','$4'}),
	io:format("starting fires ~p~n",[FireList]),
	[fire:start(Name,MyName,R,X,Y) || [Name,R,X,Y] <- FireList],
	io:format("starting sensors ~n"),
	SenList = ets:match(general_info,{{sensor,'$1'},'$2','$3','$4'}),
	[sensor:start(Name,MyName) || [Name,_,_,_] <- SenList],
    {noreply, State};
	
handle_cast({start_sim}, State) ->
	io:format("starting simulation ~n"),
	HeliList = ets:match(general_info,{{heli,'$1'},'_','_','_'}),
	[heli:start_sim(Name) || [Name] <- HeliList],
	FireList = ets:match(general_info,{{fire,'$1'},'_','_','_'}),
	[fire:start_sim(Name) || [Name] <- FireList],
	SenList = ets:match(general_info,{{sensor,'$1'},'_','_','_'}),
	[sensor:start_sim(Name) || [Name] <- SenList],
	{noreply, State};
	
handle_cast({update,Unit_Type,Unit_Data}, State) ->
	case Unit_Type of
		heli -> [Name,X,Y]=Unit_Data,	
				%io:format("updating helicopter:~p (~p,~p) ~p~n",[Name,X,Y,Status]),
				[{{_,_},_,_,Status}]= ets:lookup(general_info,{heli,Name}),
				ets:insert(general_info,{{heli,Name},X,Y,Status});
		fire -> [Name,RF]=Unit_Data,		%io:format("updating fire: ~p~n",[Name]),
				[{{fire,_},_,XF,YF}] = ets:lookup(general_info,{fire,Name}),
				ets:insert(general_info,{{fire,Name},RF,XF,YF}),
				
				%%merge fires
				QH_Merge = qlc:q([FName || {{fire,FName},F2R,F2X,F2Y} <- ets:table(general_info),
							    FName/=Name,F2R>0,RF>0, overlappingFire(XF,YF,F2X,F2Y,RF,F2R) ]),
				
				case qlc:eval(QH_Merge) of
					[] -> dont_care;
 					_Any ->%io:format("QH_Merge:~p ~n",[qlc:eval(QH_Merge)]),
 					fire:merge(Name)		
				end,
				
				%%send alerts to all servers
				QH = qlc:q([SName || {{sensor,SName},RS,XS,YS} <- ets:table(general_info), ((XF-XS)*(XF-XS) + (YF-YS)*(YF-YS))< (RS+RF)*(RS+RF)]),

				case qlc:eval(QH) of
					[] -> dont_care;
					SensorList -> [ sensor:new_alert(Sen,Name) || Sen <- SensorList]%, io:format("sensor activated ~p~n",[SensorList])
				end
				
	end,
	{noreply, State};	
	
handle_cast({heli_request,Sen_name,Fire_Name}, State) ->
	
	%io:format("requesting heli ~n"),
	Exists = ets:member(sen_fire,{Sen_name,Fire_Name}),
	case Exists of
		true -> do_nothing;%, io:format("heli already sent ~n");
		false ->  QH = qlc:q([{HName,HX,HY} || {{heli,HName},HX,HY,not_working} <- ets:table(general_info)]),
				   case qlc:eval(QH)  of
						[]-> wait_for_free_heli;%, io:format("wait_for_free_heli ~n");
						[{Name,X,Y}|_] -> ets:insert(sen_fire,{{Sen_name,Fire_Name},true}),
								  ets:insert(general_info,{{heli,Name},X,Y,working}),
								  [{{_,_},SR,SX,SY}] = ets:lookup(general_info,{sensor,Sen_name}),
								  %io:format("sending heli ~p~n",[Name]),
								  heli:move_dst(Name,SX+SR,SY,{SR,SX,SY,0})
				   end
	end,
	
	{noreply, State};	
					
handle_cast({heli_done,Name}, State) ->
	%io:format("helicopter: ~p is free ~n",[Name]),
	ets:update_element(general_info,{heli,Name},{4,not_working}),
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
terminate(_Reason, _Server) -> 
	ObjList = ets:tab2list(general_info),
	[gen_fsm:stop({global,H}) || {{_,H},_,_,_} <- ObjList, global:whereis_name(H) /=undefined],
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overlappingFire(X1,Y1,X2,Y2,R1,R2)->
    case ((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)) =< (R2+R1)*(R2+R1) of
	    true->  Dis= math:sqrt((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2)),
		    %io:format("Dis: ~p ~n",[Dis]),
		    CosA= ((Dis*Dis)+(R1*R1)-(R2*R2))/(2*Dis*R1),
		    %io:format("CosA: ~p ,",[CosA]),
		    CosB= ((Dis*Dis)+(R2*R2)-(R1*R1))/(2*Dis*R2),
		    %io:format("CosB: ~p ~n",[CosB]),
		    SinA=2*math:acos(CosA),
		    SinB=2*math:acos(CosB),
		    Over=(R1*R1*math:acos(CosA))-(0.5*R1*R1*math:sin(SinA))+(R2*R2*math:acos(CosB))-(0.5*R2*R2*math:sin(SinB)),
		    Ans= Over*100/(math:pi()*R1*R1)>?OVERLAP_PERC,
		    io:format("Over: ~p ; S: ~p ; precent = ~p  ~n",[Over,math:pi()*R1*R1, Over*100/(math:pi()*R1*R1)]),
		    Ans;
	    false->false
    end.
		    
    %Temp = math:acos(math:cos(Rad)).
    %Temp * 180 / math:pi()==Deg.
	