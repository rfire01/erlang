
-module(unit_server).
-behaviour(gen_server).

% interface calls
-export([start/1]).
    
% gen_server callbacks

-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2, code_change/3]).
		 
-define(SERVER, ?MODULE).


%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start() -> 
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    io:format("starting local_gen: ~p~n",[?SERVER]),
	ets:new(general_info,[set,named_table]),
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

	io:format("stoping all old fsms ~n"),
	ObjList = ets:tab2list(general_info),
	[gen_fsm:stop({global,H}) || {{_,H},_,_,_} <- ObjList],
	ets:delete_all_objects(general_info),

	io:format("insert data: ~p~n",[DataList]),
	ets:insert(general_info,DataList),
	io:format("starting helicopters ~n"),
	HeliList = ets:match(general_info,{{heli,'$1'},'$2','$3','_'}),
	[heli:start([Name,X,Y]) || [Name,X,Y] <- HeliList],
	io:format("starting fires ~n"),
	FireList = ets:match(general_info,{{fire,'$1'},'$2','$3','$4'}),
	[fire:start([Name,X,Y,R]) || [Name,X,Y,R] <- FireList],
	io:format("starting sensors ~n"),
	HeliList = ets:match(general_info,{{sensor,'$1'},'$2','$3','$4'}),
	[sensor:start([Name,X,Y,R]) || [Name,X,Y,R] <- SenList],
    {noreply, State};
	
handle_cast({update,Unit_Type,Unit_Data}, State) ->
	case Unit_Type of
		heli -> [Name,X,Y,Status]=Unit_Data,	io:format("updating helicopter: ~p~n",[Name]),
				ets:insert(general_info,{{heli,Name},X,Y,Status});
		fire -> [Name,XF,YF,RF]=Unit_Data,		io:format("updating fire: ~p~n",[Name]),
				ets:insert(general_info,{{fire,Name},XF,YF,RF}),
				
				%%send alerts to all servers
				QH = qlc:q([{SName,X,Y,R} || {{sensor,SName},XS,YS,RS} <- ets:table(general_info), ((XF-XS)*(XF-XS) + (YF-YS)*(YF-YS))< (RS+RF)*(RS+RF)]),

				case qlc:eval(QH) == [] of
					true -> dont_care;
					false -> send_alerts(SensorList) %% TODO
				end
	end,
	{noreply, State};	
	
handle_cast({heli_request,Sen_name,Fire_Name}, State) ->
	
	io:format("requesting heli ~n"),
	Exists = ets:member(sen_fire,{Sen_name,Fire_Name}),
	case Exists of
		true -> do_nothing, io:format("heli already sent ~n"),;
		false ->  QH = qlc:q([{{HName,HX,HY} || {{heli,HName},HX,HY,not_working} <- ets:table(general_info)]),

				   case qlc:eval(QH)  of
						[]-> wait_for_free_heli;
						[{Name,X,Y}|_] -> ets:insert(sen_fire,{{Sen_name,Fire_Name},true}),
										  ets:insert(general_info,{{heli,Name},X,Y,working}),
										  [{{_,_},SX,SY,SR}] = ets:lookup(general_info,{sensor,Sen_name}),
										  io:format("sending heli ~p~n",[Name]),
										  heli:move_dst(SX+SR,SY,search_fire)
				   end
	end
	
	{noreply, State};	
					
handle_cast({heli_done,Name}, State) ->
	io:format("helicopter: ~p is free ~n",[Name]),
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
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.   



update(Unit_type,Unit_data) ->
    io:format("update ~p:~p~n",[Unit_type,Unit_data]).
    %%process_flag(trap_exit, true),
    %%ets:new(genral,[set,named_table]),
   %% {ok, initialized}.
