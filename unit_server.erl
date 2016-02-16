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
		heli -> [Name,X,Y,Status]=Unit_Data,	io:format("updating helicopter:~p (~p,~p) ~p~n",[Name,X,Y,Status]),
				ets:insert(general_info,{{heli,Name},X,Y,Status});
		fire -> [Name,RF]=Unit_Data,		%io:format("updating fire: ~p~n",[Name]),
				[{{fire,_},_,XF,YF}] = ets:lookup(general_info,{fire,Name}),
				ets:insert(general_info,{{fire,Name},RF,XF,YF}),
				
				%%send alerts to all servers
				QH = qlc:q([SName || {{sensor,SName},RS,XS,YS} <- ets:table(general_info), ((XF-XS)*(XF-XS) + (YF-YS)*(YF-YS))< (RS+RF)*(RS+RF)]),

				case qlc:eval(QH) of
					[] -> dont_care;
					SensorList -> [ sensor:new_alert(Sen,Name) || Sen <- SensorList]%, io:format("sensor activated ~p~n",[SensorList])
				end
	end,
	{noreply, State};	
	
handle_cast({heli_request,Sen_name,Fire_Name}, State) ->
	
	io:format("requesting heli ~n"),
	Exists = ets:member(sen_fire,{Sen_name,Fire_Name}),
	case Exists of
		true -> do_nothing, io:format("heli already sent ~n");
		false ->  QH = qlc:q([{HName,HX,HY} || {{heli,HName},HX,HY,idle} <- ets:table(general_info)]),

				   case qlc:eval(QH)  of
						[]-> wait_for_free_heli, io:format("wait_for_free_heli ~n");
						[{Name,X,Y}|_] -> ets:insert(sen_fire,{{Sen_name,Fire_Name},true}),
										  ets:insert(general_info,{{heli,Name},X,Y,working}),
										  [{{_,_},SX,SY,SR}] = ets:lookup(general_info,{sensor,Sen_name}),
										  io:format("sending heli ~p~n",[Name]),
										  heli:move_dst(Name,SX+SR,SY,{SR,SX,SY,0})
				   end
	end,
	
	{noreply, State};	
					
handle_cast({heli_done,Name}, State) ->
	io:format("helicopter: ~p is free ~n",[Name]),
	ets:update_element(general_info,{heli,Name},{4,idle}),
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

	