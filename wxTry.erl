-module(wxTry).

-include_lib("wx/include/wx.hrl").
-include_lib("stdlib/include/qlc.hrl").

-behaviour(wx_object).
-export([start/0, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
	 
-export([crash/0,crash_recover/2]).
	 
-include("config.hrl").	 

-record(state, 
		{
		 parent,
		 canvas,
		 heli_amount,
		 fire_amount,
		 sensor_amount,
		 cash_amount,
		 random_but,
		 smart_but,
		 ets_name,
		 sen_ets_name,
		 self
		}).

%%start wx server
start() ->
	Server = wx:new(),
    wx_object:start(?MODULE, Server, []).
	
%%crash wx server
crash() -> Pid = whereis(wx_server),
		   Pid ! {crash,0}.
		   
%%start wx server after crash
crash_recover(MainData,SenData) -> 
	Server = wx:new(),
    wx_object:start(?MODULE, [Server,MainData,SenData], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Server) ->
        wx:batch(fun() -> do_init(Server) end).

do_init([Server,MainData,SenData]) ->
	register(wx_server,self()),%% register wx server to allow crash message later
	
	Frame = wxFrame:new(Server, -1, "wx test sim", [{size,{?Horizontal, ?Vertical}}]),		%%create frame for the simulator
	Panel  = wxPanel:new(Frame,[{style, ?wxFULL_REPAINT_ON_RESIZE}]),						%%create panel from the frame
	wxFrame:show(Frame),																	%%show the frame on the screen	
	
	wxButton:new(Panel, 10, [{label, "&Start Game"},{pos,{3,13}},{size,{95,30}}]),			%%start game button
	Rand=wxButton:new(Panel, 11, [{label, "&Randomize"},{pos,{105,13}},{size,{95,30}}]),		%%randomize game button
	Smart_rand=wxButton:new(Panel, 12, [{label, "&smart spreading"},{pos,{207,13}},{size,{95,30}}]),		%%randomize units, with smart sensor spreading
	wxButton:new(Panel, 13, [{label, "&heli \nstatistics"},{pos,{1080,13}},{size,{95,30}}]),		%%print helicopters statistics
	wxButton:new(Panel, 14, [{label, "&gen_server \nstatistics"},{pos,{1180,13}},{size,{95,30}}]),		%%print unit servers statistics
	
	wxPanel:connect (Panel, left_down),		%%allow left click event
	
	wxWindow:connect(Panel, command_button_clicked), %%allow button click event
	
	Map1 = wxImage:new("pic/forest_3.jpg"),														%%background image
	Map_1 = wxImage:scale(Map1, ?Horizontal,?Vertical),										%%scale image
	wxImage:destroy(Map1),
	
	%static texts
	wxStaticText:new(Panel, 201,"amount of helicopters:",[{pos,{312,22}}]),
	wxStaticText:new(Panel, 202,"amount of fires",[{pos,{530,22}}]),
	wxStaticText:new(Panel, 203,"amount of sensors",[{pos,{722,22}}]),
	wxStaticText:new(Panel, 203,"amount of cash",[{pos,{922,22}}]),
	Heli=wxTextCtrl:new(Panel, 101,[{value, "1"},{pos,{442,18}},{size,{70,22}}]), %set default value
    Fire=wxTextCtrl:new(Panel, 102,[{value, "1"},{pos,{630,18}},{size,{70,22}}]),
	Sens=wxTextCtrl:new(Panel, 103,[{value, "5"},{pos,{832,18}},{size,{70,22}}]),
	Cash=wxTextCtrl:new(Panel, 103,[{value, "300"},{pos,{1022,18}},{size,{70,22}}]),
	
	MonPid = global:whereis_name(wxMon),
	
	%%create data ets
	ets:new(simData,[set,named_table,{heir,MonPid,{wxServer,main}}]),
	ets:new(senEts,[set,named_table,{heir,MonPid,{wxServer,sen}}]),
	
	%%enter the data from the input
	[ ets:insert(simData,Data) || Data <- MainData],
	[ ets:insert(senEts,Data) || Data <- SenData],
	
	%%add wx server to the monitor
	loc_monitor:add_mon(wxMon,self()),
	
	%%animation ets
	ets:new(sensAnm,[set,named_table]),
	
	ets:insert(sensAnm,{angle,0}),
	
	State= #state{parent=Panel,canvas = Frame,heli_amount=Heli,fire_amount=Fire,sensor_amount=Sens,cash_amount=Cash,ets_name=simData,sen_ets_name=senEts,random_but=Rand,smart_but=Smart_rand,self=self()},
	
	OnPaint=fun(_Evt,_Obj)->%%function to do when paint event called
					
					%%draw background
					Paint=wxBufferedPaintDC:new(Panel),
					Bitmap=wxBitmap:new(Map_1),
					wxDC:drawBitmap(Paint,Bitmap,{0,0}),
					wxBitmap:destroy(Bitmap),
					%%print all units from the ets to screen
					add_units_to_screen(simData,Paint,senEts),
					
					wxBufferedPaintDC:destroy(Paint) end,					
	wxFrame:connect(Panel, paint, [{callback,OnPaint}]),%%connect paint event to panel with callbakc function OnPaint
	
	S=self(),
	register(refresher, spawn_link(fun() -> loop(S) end)),	%%process to send timeouts to refresh screen
	spawn_link(fun() -> watch_dog() end),					%%watch_dog process to make sure there is connection with other nodes
	
	{Panel, State};
		
do_init(Server) ->
	register(wx_server,self()),	%% register wx server to allow crash message later
	%%----------- init local servers (for each part of screen)
	%% starting unit server in each node, if there is no connection and error printed to screen with
	%% the problematic node
	case connect_to_nodes([?TLSERVER_NODE,?TRSERVER_NODE,?BLSERVER_NODE,?BRSERVER_NODE]) of
		ok -> case global:whereis_name(tl) == undefined of
				true -> spawn(?TLSERVER_NODE,unit_server,start,[tl]);
				false -> ok
			  end,
			  case global:whereis_name(tr) == undefined of
				true -> spawn(?TRSERVER_NODE,unit_server,start,[tr]);
				false -> ok
			  end,
			  case global:whereis_name(bl) == undefined of
				true -> spawn(?BLSERVER_NODE,unit_server,start,[bl]);
				false -> ok
			  end,
			  case global:whereis_name(br) == undefined of
				true -> spawn(?BRSERVER_NODE,unit_server,start,[br]);
				false -> ok
			  end,
			  wait_all_on([tl,tr,bl,br]);
		error_in_nodes_connect -> io:format("check problem and reset wx~n")
	end,

    Frame = wxFrame:new(Server, -1, "wx test sim", [{size,{?Horizontal, ?Vertical}}]),		%%create frame for the simulator
	Panel  = wxPanel:new(Frame,[{style, ?wxFULL_REPAINT_ON_RESIZE}]),						%%create panel from the frame
	wxFrame:show(Frame),																	%%show the frame on the screen	
	
	wxButton:new(Panel, 10, [{label, "&Start Game"},{pos,{3,13}},{size,{95,30}}]),			%%start game button
	Rand=wxButton:new(Panel, 11, [{label, "&Randomize"},{pos,{105,13}},{size,{95,30}}]),		%%randomize game button
	Smart_rand=wxButton:new(Panel, 12, [{label, "&smart spreading"},{pos,{207,13}},{size,{95,30}}]),		%%randomize units, with smart sensor spreading
	wxButton:new(Panel, 13, [{label, "&heli \nstatistics"},{pos,{1080,13}},{size,{95,30}}]),		%%print helicopters statistics
	wxButton:new(Panel, 14, [{label, "&gen_server \nstatistics"},{pos,{1180,13}},{size,{95,30}}]),		%%print unit servers statistics
	
	wxPanel:connect (Panel, left_down), 			%%allow left click events
	
	wxWindow:connect(Panel, command_button_clicked),%%allow button click event
	
	Map1 = wxImage:new("pic/forest_3.jpg"),														%%background image
	Map_1 = wxImage:scale(Map1, ?Horizontal,?Vertical),										%%scale image
	wxImage:destroy(Map1),
	
	%static texts
	wxStaticText:new(Panel, 201,"amount of helicopters:",[{pos,{312,22}}]),
	wxStaticText:new(Panel, 202,"amount of fires",[{pos,{530,22}}]),
	wxStaticText:new(Panel, 203,"amount of sensors",[{pos,{722,22}}]),
	wxStaticText:new(Panel, 203,"amount of cash",[{pos,{912,22}}]),
	Heli=wxTextCtrl:new(Panel, 101,[{value, "1"},{pos,{442,18}},{size,{70,22}}]), %set default value
    Fire=wxTextCtrl:new(Panel, 102,[{value, "1"},{pos,{630,18}},{size,{70,22}}]),
	Sens=wxTextCtrl:new(Panel, 103,[{value, "5"},{pos,{832,18}},{size,{70,22}}]),
	Cash=wxTextCtrl:new(Panel, 103,[{value, "300"},{pos,{1002,18}},{size,{70,22}}]),
	
	%%start monitor for the wx server
	loc_monitor:start(wxMon),
	MonPid = global:whereis_name(wxMon),
	
	%%create data ets for the wx server
	ets:new(simData,[set,named_table,{heir,MonPid,{wxServer,main}}]),
	ets:new(senEts,[set,named_table,{heir,MonPid,{wxServer,sen}}]),
	ets:new(sensAnm,[set,named_table]),
	
	ets:insert(senEts,{max_fire,0}),
	
	%%add the wx server to the monitor
	loc_monitor:add_mon(wxMon,self()),
	
	ets:insert(sensAnm,{angle,0}),
	
	State= #state{parent=Panel,canvas = Frame,heli_amount=Heli,fire_amount=Fire,sensor_amount=Sens,cash_amount=Cash,ets_name=simData,sen_ets_name=senEts,random_but=Rand,smart_but=Smart_rand,self=self()},
	
	OnPaint=fun(_Evt,_Obj)->%%function to do when paint event called
					
					%%draw background
					Paint=wxBufferedPaintDC:new(Panel),
					Bitmap=wxBitmap:new(Map_1),
					wxDC:drawBitmap(Paint,Bitmap,{0,0}),
					wxBitmap:destroy(Bitmap),
					%%paint units from ets to screen
					add_units_to_screen(simData,Paint,senEts),
					
					wxBufferedPaintDC:destroy(Paint) end,					
	wxFrame:connect(Panel, paint, [{callback,OnPaint}]),%%connect paint event to panel with callback function OnPaint
	
	S=self(),
	register(refresher, spawn_link(fun() -> loop(S) end)),	%%process to send timeouts to refresh screen
	spawn_link(fun() -> watch_dog() end),					%%watch_dog process to make sure there is connection with other nodes
	
	{Panel, State}.


%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info

%add fire at (x,y)
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State = #state{}) ->
	case ets:lookup(State#state.sen_ets_name,max_fire)==[] of
		false ->
			[{_,Max}] = ets:lookup(State#state.sen_ets_name,max_fire),		%%check how many fires created to prevent 2 fires with same name
			%%send add fire to unit server according to x,y values
			%%name of fire is "fire" + number of fires+1
			case X<?Horizontal/2 of
				true -> case Y<?Vertical/2 of
							true -> unit_server:addFire(tl,list_to_atom("fire" ++ integer_to_list(Max+1)),X,Y),
									ets:insert(State#state.sen_ets_name,{max_fire,Max+1});
							false -> unit_server:addFire(bl,list_to_atom("fire" ++ integer_to_list(Max+1)),X,Y),
									ets:insert(State#state.sen_ets_name,{max_fire,Max+1})
						end;
				false -> case Y<?Vertical/2 of
							true -> unit_server:addFire(tr,list_to_atom("fire" ++ integer_to_list(Max+1)),X,Y),
									ets:insert(State#state.sen_ets_name,{max_fire,Max+1});
							false -> unit_server:addFire(br,list_to_atom("fire" ++ integer_to_list(Max+1)),X,Y),
									ets:insert(State#state.sen_ets_name,{max_fire,Max+1})
						end
			end;
		true -> ets:insert(State#state.sen_ets_name,{max_fire,1000})
	end,
	{noreply,State};


%when randomize button clicked, randomizing all units places:
handle_event(_Ev=#wx{id=11,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("randomizing units coordinates ~n"),
	
	ets:delete_all_objects(State#state.ets_name),		%%delete old units data
	ets:delete_all_objects(State#state.sen_ets_name),	%%delete old sensors data
	
	HeliTxt = wxTextCtrl:getValue(State#state.heli_amount),		%%heli amount text
	FireTxt = wxTextCtrl:getValue(State#state.fire_amount),		%%fire amount text
	SensTxt = wxTextCtrl:getValue(State#state.sensor_amount),	%sensor amount text
	
	%%check for each text if it is number.
	%%if its a number continue, otherwise print error to screen
	case list_to_number(HeliTxt) of   
                badarg ->  HeliValid = false,HeliNum=0;
                Number1 ->  HeliValid = true,HeliNum=Number1
	end,
	case list_to_number(FireTxt) of   
                badarg ->  FireValid = false,FireNum=0;
                Number2 ->  FireValid = true,FireNum=Number2
	end,
	case list_to_number(SensTxt) of   
                badarg ->  SenValid = false,SenNum=0;
                Number3 ->  SenValid = true,SenNum=Number3
	end,
	
	
	Valid = (HeliValid and FireValid) and SenValid,	%%true if all texts was numbers
	
	case Valid == true of
		true -> ets:insert(State#state.sen_ets_name,{max_fire,FireNum}),
				Pid2 = getPid(self()),
				{A,_B,C}=erlang:now(),
				random:seed(C,Pid2*Pid2,erlang:round(C/A)),	%%create different randomize for each process
				randUnit(heli,HeliNum,State#state.ets_name),		%%randomize location for helicopters
				randUnit(fire,FireNum,State#state.ets_name),		%%randomize location for fires
				randUnit(sensor,SenNum,State#state.sen_ets_name);	%%randomize location for sensors
		false -> io:format("please enter NUMBERS into amount of units~n")
	end,
	
	Pid = State#state.self,
	Pid ! refresh,				%%refresh screen
	
	%%divide the ets by coordinates to screens
	[TL_ets,TR_ets,BL_ets,BR_ets] = divide_unit_to_screens(State#state.ets_name,State#state.sen_ets_name),
	
	unit_server:create(tl,TL_ets),	%%create units in top left node
	unit_server:create(tr,TR_ets),	%%create units in top right node
	unit_server:create(bl,BL_ets),	%%create units in bottom left node
	unit_server:create(br,BR_ets),	%%create units in bottom right node
	
    {noreply,State};
	
%randomizing units, with smart spread of sensors	
handle_event(_Ev=#wx{id=12,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("smart randomizing units coordinates ~n"),
	
	ets:delete_all_objects(State#state.ets_name),		%%delete old units data
	ets:delete_all_objects(State#state.sen_ets_name),	%%delete old sensors data
	
	HeliTxt = wxTextCtrl:getValue(State#state.heli_amount),	%%heli amount text
	FireTxt = wxTextCtrl:getValue(State#state.fire_amount),	%%fire amount text
	CashTxt = wxTextCtrl:getValue(State#state.cash_amount),	%%cash amount text
	
	%%check for each text if it is number.
	%%if its a number continue, otherwise print error to screen
	case list_to_number(HeliTxt) of   
                badarg ->  HeliValid = false,HeliNum=0;
                Number1 ->  HeliValid = true,HeliNum=Number1
	end,
	case list_to_number(FireTxt) of   
                badarg ->  FireValid = false,FireNum=0;
                Number2 ->  FireValid = true,FireNum=Number2
	end,
	case list_to_number(CashTxt) of   
                badarg ->  CashValid = false,CashNum=0;
                Number3 ->  CashValid = true,CashNum=Number3
	end,
	
	Valid = (HeliValid and FireValid) and CashValid,	%%true if all texts was numbers
	
	case Valid == true of
		true -> ets:insert(State#state.sen_ets_name,{max_fire,FireNum}),
				Pid2 = getPid(self()),
				{A,_B,C}=erlang:now(),
				random:seed(C,Pid2*Pid2,erlang:round(C/A)),	%%create different randomize for each process
				randUnit(heli,HeliNum,State#state.ets_name),	%%randomize location for helis
				randUnit(fire,FireNum,State#state.ets_name),	%%randomize location for fires
				smart_sensor_randomize(CashNum,State#state.sen_ets_name);	%%randomize location for sensors, using the smart algorithm
		false -> io:format("please enter NUMBERS into amount of units~n")
	end,
	
	Pid = State#state.self,
	Pid ! refresh,			%%refresh screen
	
	%%divide the ets by coordinates to screens
	[TL_ets,TR_ets,BL_ets,BR_ets] = divide_unit_to_screens(State#state.ets_name,State#state.sen_ets_name),
	
	unit_server:create(tl,TL_ets),	%%create units in top left node
	unit_server:create(tr,TR_ets),	%%create units in top right node
	unit_server:create(bl,BL_ets),	%%create units in bottom left node
	unit_server:create(br,BR_ets),	%%create units in bottom right node
	
    {noreply,State};
	
%when start button clicked, starting simulation:
handle_event(_Ev=#wx{id=10,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("starting simulation ~n"),
	
	wxButton:disable(State#state.random_but),	%%disable option to randomize units
	wxButton:disable(State#state.smart_but),	%%disable option to smart randomize units
	unit_server:start_sim(tl),					%%Start simulation in node tl
	unit_server:start_sim(tr),					%%Start simulation in node tr
	unit_server:start_sim(bl),					%%Start simulation in node bl
	unit_server:start_sim(br),					%%Start simulation in node tr
	
    {noreply,State};

%when start button clicked, print all helicopters statistics 
handle_event(_Ev=#wx{id=13,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
    QH = qlc:q([HeliName || {{heli,HeliName},_,_,_} <- ets:table(State#state.ets_name)]),
	io:format("########## HELICOPTERS STATISTICS ##########~n~n"),
	%%print the statistics of all helicopters
	spawn(fun() -> print_stat(qlc:eval(QH),heli) end),	
    {noreply,State};
	
%when start button clicked, print all gen_servers statistics 
handle_event(_Ev=#wx{id=14,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("########## GEN_SERVERS STATISTICS ##########~n~n"),
	%%print the statistics of all units servers
	spawn(fun() -> print_stat([tl,tr,bl,br],server) end),
    {noreply,State};
	
handle_event(_Ev = #wx{}, State = #state{}) ->
    {noreply,State}.

%% Callbacks handled as normal gen_server callbacks

handle_info(refresh,State=#state{})->

	%%animation of sensor:
	[{_,Angle}] = ets:lookup(sensAnm,angle),
	ets:insert(sensAnm,{angle,Angle+ math:pi()/15}),
	
	Updated_list = get_updated_data(State#state.ets_name),	%%get data ets from all units servers
	ets:delete_all_objects(State#state.ets_name),			%%delete all units from old ets
	[ ets:insert(State#state.ets_name,Server_list) || Server_list <- Updated_list],	%%add new data to ets
	wxWindow:refresh(State#state.parent,[{eraseBackground,false}]),					%%refresh screen
	{noreply,State};

%%crash wx server
handle_info({crash,Num},_State)->
	{noreply,1/Num};

handle_info(_Msg, State) ->
    {noreply,State}.

handle_call(_Msg, _From, State) ->
    {reply,ok,State}.
	
handle_cast(_Msg, State) ->
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(Reason, State=#state{}) ->
	unregister(wx_server),					%%unregister wx server
	wxWindow:destroy(State#state.canvas),	%%destroy wx window
	wx:destroy(),							%%destroy wx object
	case whereis(refresher) of				%%kill refresher process if alive
		undefined -> ok;
		Pid -> exit(Pid,kill)
	end,
	ets:delete(sensAnm),
	case Reason == wx_deleted of
		true ->
				%%delete ets on normal exit
				ets:delete(senEts),
				ets:delete(simData),
				
				%%shut down all servers
				[shut_down_server(Server) || Server <- [tl,tr,bl,br]];
		false -> crash
	end,
	
    ok.
	  
  
%%%===================================================================
%%% Internal functions
%%%===================================================================

%%loop to send refresh screen time out every ?WX_UPDATE_SPEED ms
loop(Pid) -> receive after ?WX_UPDATE_SPEED -> Pid ! refresh end, loop(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%trys to convert list \ string to int, if possible return the nubmer, otherwise return reason
list_to_number(Number) ->
  try list_to_integer( Number) of				%%try cast to integer
    Val_integer -> Val_integer
  catch  
    error:_Why -> try list_to_float(Number) of	%%try cast to float
                    Val_float -> Val_float
                  catch 
                    error: Reason -> Reason		%%string isn't a number
                  end   
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%randomize units locations \ radius according to their type
randUnit(_,0,_) -> ok;
randUnit(heli,Amount,EtsName) ->
	%%randomize coordinates
	X = random:uniform(?Horizontal-?HELI_PIC_SIZE*2)+?HELI_PIC_SIZE,
	Y  = random:uniform(?Vertical-?HELI_PIC_SIZE*4)+?HELI_PIC_SIZE,
	%%create heli name
	HeliData = {{heli,list_to_atom("heli" ++ integer_to_list(Amount))},X,Y,not_working},
	ets:insert(EtsName,HeliData),
	randUnit(heli,Amount-1,EtsName); %%recursively run, until amount=0 

randUnit(fire,Amount,EtsName) ->
	%%randomize coordinates
	X = random:uniform(?Horizontal-?FireDefaultRadius*5)+?FireDefaultRadius*2,
	Y  = random:uniform(?Vertical-?FireDefaultRadius)+?FireDefaultRadius,
	%%create fire name
	FireData = {{fire,list_to_atom("fire" ++ integer_to_list(Amount))},?FireDefaultRadius,X,Y},
	ets:insert(EtsName,FireData),
	randUnit(fire,Amount-1,EtsName);	%%recursively run, until amount=0 

randUnit(sensor,Amount,EtsName) ->
	%%randomize coordinates
	X = random:uniform(?Horizontal-?SensorRadius*4)+?SensorRadius*2,
	Y  = random:uniform(?Vertical-?SensorRadius*3)+?SensorRadius,
	%%create heli name
	SensName=list_to_atom("sensor" ++ integer_to_list(Amount)),
	SensorData = {{sensor,SensName},?SensorRadius,X,Y},
	ets:insert(EtsName,SensorData),
	randUnit(sensor,Amount-1,EtsName).	%%recursively run, until amount=0 
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%called in paint, to draw on screen all units
add_units_to_screen(EtsName,Paint,SenEts) ->
	
	[{_,Angle}] = ets:lookup(sensAnm,angle),	%%sensor animation angle
	
	QH_fire = qlc:q([[R,X,Y] || {{fire,_},R,X,Y} <- ets:table(EtsName)]),							%%get all fires
	QH_heli = qlc:q([[X,Y] || {{heli,_},X,Y,_} <- ets:table(EtsName)]),								%%get all helis
	QH_sensor = qlc:q([[R,X,Y,SensName,Angle] || {{sensor,SensName},R,X,Y} <- ets:table(SenEts)]),	%%get all sensors

	[ add_unit_to_screen(sensor,Unit,Paint) || Unit <- qlc:eval(QH_sensor)],						%%draw sensor to screen
	[ add_unit_to_screen(fire,Unit,Paint) || Unit <- qlc:eval(QH_fire)],							%%draw fire to screen
	[ add_unit_to_screen(heli,Unit,Paint) || Unit <- qlc:eval(QH_heli)].							%%draw heli to screen

%%called in paint, to draw certian unit on screen	
add_unit_to_screen(heli,[X,Y],Paint) ->
	
	Image1 = wxImage:new("pic/heli_4.png"),
	Image2 = wxImage:scale(Image1, ?HELI_PIC_SIZE,?HELI_PIC_SIZE),
	Bmp = wxBitmap:new(Image2),	
	wxImage:destroy(Image1),
	wxImage:destroy(Image2),
	wxDC:drawBitmap(Paint, Bmp, {round(X)-?HELI_PIC_HALF,round(Y)-?HELI_PIC_HALF}),	%%draw heli image on screen
	wxBitmap:destroy(Bmp);
	
add_unit_to_screen(fire,[R,X,Y],Paint) ->
	case 2*round(R)>0 of
		%%check that the fire radius is bigger than 0, to prevet crash on image scale
		true -> 
				Image1 = wxImage:new("pic/fire_2.png"),
				Image2 = wxImage:scale(Image1, 2*round(R),2*round(R)),
				Bmp = wxBitmap:new(Image2),
				wxImage:destroy(Image1),
				wxImage:destroy(Image2),
				wxDC:drawBitmap(Paint, Bmp, {round(X-R),round(Y-R)}),	%%draw heli image on screen
				wxBitmap:destroy(Bmp);
		false -> do_nothing
	end;
	
add_unit_to_screen(sensor,[R,X,Y,_SensName,Angle],Paint) ->

	%%draw circle on screen
	wxDC:drawCircle(Paint, {X,Y}, R),
	%%draw "triangle" in the circle
	wxDC:drawLine(Paint, {X, Y}, {erlang:round(X+math:cos(Angle)*R),erlang:round(Y+math:sin(Angle)*R)}),
	wxDC:drawLine(Paint, {X, Y}, {erlang:round(X+math:cos(Angle+math:pi()/10)*R),erlang:round(Y+math:sin(Angle+math:pi()/10)*R)}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%try to connect to all nodes, return error if not succeeded
connect_to_nodes([]) -> ok;
connect_to_nodes([Node|Rest]) -> case net_adm:ping(Node) of			%%send ping to node
									pong -> 
										connect_to_nodes(Rest);		%%ping succeeded, ping next node
									pang ->
										%%ping failed print error to screen
										io:format("ERROR: Cannot connect to node ~p.~n",[Node]),
										error_in_nodes_connect
								 end.
								 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%shut down unit server
shut_down_server(Name) ->
	case global:whereis_name(Name) of
		%%check that unit server exists, if it does stop it
		undefined -> ok;
		_any -> unit_server:stop(Name)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% axis of wx goes:
%% x lowest at left, highest at right
%% y lowest at top, highest at bottom
divide_unit_to_screens(MainEts,SenEts)->
	%%qlc to get fires according to which node they belog to
	QH_tl_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X>?Horizontal/2, Y>?Vertical/2]),
	
	%%qlc to get helis according to which node they belog to
	QH_tl_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X>?Horizontal/2, Y>?Vertical/2]),
	
	%%qlc to get sensors according to which node they belog to
	QH_tl_sensor = qlc:q([ {{sensor,Name},R,X,Y}|| {{sensor,Name},R,X,Y} <- ets:table(SenEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_sensor = qlc:q([ {{sensor,Name},R,X,Y}|| {{sensor,Name},R,X,Y} <- ets:table(SenEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_sensor = qlc:q([ {{sensor,Name},R,X,Y}|| {{sensor,Name},R,X,Y} <- ets:table(SenEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_sensor = qlc:q([ {{sensor,Name},R,X,Y}|| {{sensor,Name},R,X,Y} <- ets:table(SenEts), X>?Horizontal/2, Y>?Vertical/2]),
	
	%%create lists by by nodes that holds all the units of the relevant node
	TL_list = qlc:eval(QH_tl_fire) ++ qlc:eval(QH_tl_heli) ++ qlc:eval(QH_tl_sensor),
	TR_list = qlc:eval(QH_tr_fire) ++ qlc:eval(QH_tr_heli) ++ qlc:eval(QH_tr_sensor),
	BL_list = qlc:eval(QH_bl_fire) ++ qlc:eval(QH_bl_heli) ++ qlc:eval(QH_bl_sensor),
	BR_list = qlc:eval(QH_br_fire) ++ qlc:eval(QH_br_heli) ++ qlc:eval(QH_br_sensor),
	
	[TL_list,TR_list,BL_list,BR_list].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%wait for servers to start
wait_all_on([]) -> ok;
wait_all_on([H|T]) ->
	case global:whereis_name(H) of
		%%check of unit server started or not
		undefined -> wait_all_on([H|T]);
		_Any -> wait_all_on(T)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%get new data from unit servers
get_updated_data(MainEts)->
	
	%%qlc to get fire by nodes
	QH_tl_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X>?Horizontal/2, Y>?Vertical/2]),
	
	%%qlc to get helis by nodes
	QH_tl_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X>?Horizontal/2, Y>?Vertical/2]),
	
	%%for each server try to request new data
	%%if the unit server give data save it in list
	%%if the unit server give server_off, then the unit server crashed and we save the old data in list
	case unit_server:wx_update(tl) of
		server_off -> Tl = qlc:eval(QH_tl_fire) ++ qlc:eval(QH_tl_heli);
		Any1 -> Tl = Any1
	end,
	case unit_server:wx_update(tr) of
		server_off -> Tr = qlc:eval(QH_tr_fire) ++ qlc:eval(QH_tr_heli);
		Any2 -> Tr = Any2
	end,
	case unit_server:wx_update(bl) of
		server_off -> Bl = qlc:eval(QH_bl_fire) ++ qlc:eval(QH_bl_heli);
		Any3 -> Bl = Any3
	end,
	case unit_server:wx_update(br) of
		server_off -> Br = qlc:eval(QH_br_fire) ++ qlc:eval(QH_br_heli);
		Any4 -> Br = Any4
	end,
	%%return all the data received
	[Tl,Tr,Bl,Br].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%try to random sensor location with no overlapping with other sensors, with "Tries" chances
rand_single_sensor(_Radius,_Number,Tries,_Ets) when Tries==0 -> false; %io:format("reached try=0~n"),

rand_single_sensor(Radius,Number,Tries,Ets) when Tries /= 0 -> 
	%%random location
	X = random:uniform(?Horizontal-Radius*4)+Radius*2,
	Y  = random:uniform(?Vertical-Radius*3)+Radius,
	%%check if there are overlapping sensors
	QH = qlc:q([ found || {{sensor,_Name},RS,XS,YS} <- ets:table(Ets),((X-XS)*(X-XS) + (Y-YS)*(Y-YS))< (Radius+RS)*(Radius+RS)]),
	case qlc:eval(QH) == [] of
		%%if there is no overlapping sensors create sensor in loaction with raidus of "Radius"
		true -> SensName=list_to_atom("sensor" ++ integer_to_list(Number)),
				SensorData = {{sensor,SensName},Radius,X,Y},
				ets:insert(Ets,SensorData),
				true;
		%%if there is overlapping sensors, try to randomize location again
		false -> rand_single_sensor(Radius,Number,Tries-1,Ets)
	end.

%%try to add sensor to screen with size of Type
add_sensor_to_screen(Type,Number,Ets)->
	case Type of
		large -> rand_single_sensor(?LARGE_SENSOR_SIZE,Number,20,Ets);	%%large sensor have 20 tries
		medium -> rand_single_sensor(?MEDIUM_SENSOR_SIZE,Number,40,Ets);%%medium sensor have 40 tries
		small -> rand_single_sensor(?SMALL_SENSOR_SIZE,Number,60,Ets)	%%small sensor have 60 tries
	end.

%%smart randomize of sensors, according to cash amount
smart_sensor_randomize(Cash,Ets) -> smart_sensor_randomize(Cash,1,large,Ets).
	
smart_sensor_randomize(Cash,Number,Size,Ets) when Size == large -> 
	case Cash >= ?LARGE_SENSOR_PRICE of
		%%check if enough money for large
		%%if there is enough try to find location for it
		%%if there isn't enough cash, or no location without overlapping found start putting medium sensors
		true -> case add_sensor_to_screen(large,Number,Ets) of 
					true -> smart_sensor_randomize(Cash-?LARGE_SENSOR_PRICE,Number+1,large,Ets);
					false -> smart_sensor_randomize(Cash,Number,medium,Ets)
				end;
		false -> smart_sensor_randomize(Cash,Number,medium,Ets)
	end;
	
smart_sensor_randomize(Cash,Number,Size,Ets) when Size == medium -> 
	case Cash >= ?MEDIUM_SENSOR_PRICE of
		%%check if enough money for medium
		%%if there is enough try to find location for it
		%%if there isn't enough cash, or no location without overlapping found start putting small sensors
		true -> case add_sensor_to_screen(medium,Number,Ets) of 
					true -> smart_sensor_randomize(Cash-?MEDIUM_SENSOR_PRICE,Number+1,large,Ets);
					false -> smart_sensor_randomize(Cash,Number,small,Ets)
				end;
		false -> smart_sensor_randomize(Cash,Number,small,Ets)
	end;

smart_sensor_randomize(Cash,Number,Size,Ets) when Size == small -> 
	case Cash >= ?SMALL_SENSOR_PRICE of
		%%check if enough money for small
		%%if there is enough try to find location for it
		%%if there isn't enough cash, or no location without overlapping found stop putting sensors
		true -> case add_sensor_to_screen(small,Number,Ets) of 
					true -> smart_sensor_randomize(Cash-?SMALL_SENSOR_PRICE,Number+1,large,Ets);
					false -> stop
				end;
		false -> stop
	end.	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%print statistics of all Type objects
print_stat([],_Type) -> io:format("~n");

print_stat([H|T],Type) when Type == heli ->
	heli:statistics(H),	%%print heli statistics
	timer:sleep(200),
	print_stat(T,Type);	%%recursively print next heli statistics
	
print_stat([H|T],Type) when Type == server ->
	unit_server:statistics(H),	%%print unit server statistics
	timer:sleep(200),
	print_stat(T,Type).	%%recursively print next unit server statistics
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%extract pid as intger from "<x.y.z>" (extract y)
getPid(Pid) -> getPid(erlang:pid_to_list(Pid),false,[]).	
			
getPid([H|T],Start,Res) when Start==false -> case ([H|[]]==".")of
													%%check if reached to the first "."
													true -> getPid(T,true,Res);
													false -> getPid(T,Start,Res)
												  end;
												  
getPid([H|T],Start,Res) when Start==true  -> case ([H|[]]==".")of
												%%check if reached to the second "."
												true -> list_to_integer(Res);
												false -> getPid(T,Start,Res ++ [H])
											 end.
											 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%watch dog, keep sending pings to other nodes to make sure there is connection
watch_dog() ->
	net_adm:ping(?TLSERVER_NODE),
	net_adm:ping(?TRSERVER_NODE),
	net_adm:ping(?BLSERVER_NODE),
	net_adm:ping(?BRSERVER_NODE),
	timer:sleep(1000),
	watch_dog().