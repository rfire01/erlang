
-module(loc_monitor).
-behaviour(gen_server).

% interface calls
-export([start/1,add_mon/2,stop/1]).
    
% gen_server callbacks
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).

%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start(MonName) -> 
    gen_server:start({global, MonName}, ?MODULE, [], []).
	
add_mon(MonName,ProcPid) ->
	gen_server:cast({global, MonName}, {add,ProcPid}).
	
stop(MonName) ->
	gen_server:cast({global, MonName}, {stop}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
	Ets = ets:new(saveData,[set]),
	put(ets_id,Ets),
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
handle_call(_Message, _From, State) -> 
    {reply, ok, State}.

%% Asynchronous, possible return values
% {noreply,NewState} 
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}

handle_cast({add,Pid}, State) ->
	monitor(process,Pid),
    {noreply, State};
	
handle_cast({stop}, State) ->
    {stop,normal,State};

%% generic async handler
handle_cast(_Message, State) ->
    {noreply, State}.

%% Informative calls
% {noreply,NewState} 
% {noreply,NewState,Timeout} 
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, _Server) -> 
	Ets = get(ets_id),
	case Reason of
		normal -> ets:delete(Ets,Pid);
		wx_deleted -> ets:delete(Ets,Pid);
		_Any -> [TableId,HeirData] = get_ets(Ets,Pid,false),
				io:format("Data = ~p~n",[HeirData]),
				case HeirData of
					{heli,Name} -> heli:crash_recover(Name,ets:tab2list(TableId));
					{fire,_Name} -> do_nothing;
					{sensor,Name} -> sensor:crash_recover(Name,ets:tab2list(TableId));
					{server,Name} -> unit_server:crash_recover(Name,ets:tab2list(TableId));
					{wxServer,_Name} -> [TableId2,_HeirData2]= get_ets(Ets,Pid,true),
									  recover_wx(TableId,TableId2,HeirData);
					_Any2 -> do_nothing
				end
				
	end,
    {noreply, _Server};

handle_info({'ETS-TRANSFER', TableId, Pid, HeirData}, _Server) -> 
	Ets = get(ets_id),
	case ets:member(Ets,Pid) of
		false -> ets:insert(Ets,{Pid,TableId, HeirData});
		true -> ets:insert(Ets,{{Pid,2},TableId,HeirData})
	end,
    {noreply, _Server};
	
handle_info(_Message, _Server) -> 
    {noreply, _Server}.

%% Server termination
terminate(_Reason, _Server) -> 
    ok.


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_ets(Ets,Pid,Wx) ->
	case Wx of
		false-> case ets:lookup(Ets,Pid) of
					[{Pid,Id,Data}] -> TableId=Id, HeirData=Data;
					[] -> receive
							{'ETS-TRANSFER', Id, Pid, Data} ->	TableId=Id, HeirData=Data
						  after 3000 -> HeirData = ignore,TableId=0%,io:format("no ets found~n")
						  end
				end;
		true -> case ets:lookup(Ets,{Pid,2}) of
					[{{Pid,2},Id,Data}] -> TableId=Id, HeirData=Data;
					[] -> receive
							{'ETS-TRANSFER', Id, Pid, Data} ->	TableId=Id, HeirData=Data
						  after 3000 -> HeirData = ignore,TableId=0%,io:format("no ets found~n")
						  end
				end
	end,
	[TableId,HeirData].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recover_wx(TableId,TableId2,{_,Type}) ->
	%io:format("tab1 = ~p ; tab2 = ~p~n",[TableId,TableId2]),
	case Type == main of
		true -> Main = ets:tab2list(TableId),
				Sen = ets:tab2list(TableId2);
		false -> Main = ets:tab2list(TableId2),
				 Sen = ets:tab2list(TableId) 
	end,
	try
		ets:delete(TableId),
		ets:delete(TableId2)
	catch
		_Error:_Reason -> do_nothing
	end,
	wxTry:crash_recover(Main,Sen).
	