-module(wxTry).

-include_lib("wx/include/wx.hrl").
-include_lib("stdlib/include/qlc.hrl").

-behaviour(wx_object).
-export([start/0, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
	 
-export([crash/0,crash_recover/2]).
	 
-include("config.hrl").	 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-define(Horizontal,				1200).
%-define(Vertical,				556).

%-define(FireDefaultRadius, 50).
%-define(SensorRadius, 50).
%-define(HELI_PIC_SIZE, 50).
%-define(HELI_PIC_HALF, round(?HELI_PIC_SIZE/2)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

start() ->
	Server = wx:new(),
    wx_object:start(?MODULE, Server, []).
	
crash() -> Pid = whereis(wx_server),
		   Pid ! {crash,0}.
		   
crash_recover(MainData,SenData) -> 
	Server = wx:new(),
    wx_object:start(?MODULE, [Server,MainData,SenData], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Server) ->
        wx:batch(fun() -> do_init(Server) end).

do_init([Server,MainData,SenData]) ->
	register(wx_server,self()),% io:format("done! 1~n"),
	
	Frame = wxFrame:new(Server, -1, "wx test sim", [{size,{?Horizontal, ?Vertical}}]),		%%create frame for the simulator
	Panel  = wxPanel:new(Frame,[{style, ?wxFULL_REPAINT_ON_RESIZE}]),						%%create panel from the frame
	wxFrame:show(Frame),																	%%show the frame on the screen	
	
	wxButton:new(Panel, 10, [{label, "&Start Game"},{pos,{3,13}},{size,{95,30}}]),			%%start game button
	Rand=wxButton:new(Panel, 11, [{label, "&Randomize"},{pos,{105,13}},{size,{95,30}}]),		%%randomize game button
	Smart_rand=wxButton:new(Panel, 12, [{label, "&smart spreading"},{pos,{207,13}},{size,{95,30}}]),		%%randomize units, with smart sensor spreading
	wxButton:new(Panel, 13, [{label, "&heli \nstatistics"},{pos,{1080,13}},{size,{95,30}}]),		%%randomize units, with smart sensor spreading
	wxButton:new(Panel, 14, [{label, "&gen_server \nstatistics"},{pos,{1180,13}},{size,{95,30}}]),		%%randomize units, with smart sensor spreading
	
	wxPanel:connect (Panel, left_down),
	
	wxWindow:connect(Panel, command_button_clicked), 
	
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
	
	ets:new(simData,[set,named_table,{heir,MonPid,{wxServer,main}}]),
	ets:new(senEts,[set,named_table,{heir,MonPid,{wxServer,sen}}]),
	
	[ ets:insert(simData,Data) || Data <- MainData],
	[ ets:insert(senEts,Data) || Data <- SenData],
	
	loc_monitor:add_mon(wxMon,self()),
	
	ets:new(sensAnm,[set,named_table]),
	
	ets:insert(sensAnm,{angle,0}),
	
	State= #state{parent=Panel,canvas = Frame,heli_amount=Heli,fire_amount=Fire,sensor_amount=Sens,cash_amount=Cash,ets_name=simData,sen_ets_name=senEts,random_but=Rand,smart_but=Smart_rand,self=self()},
	
	OnPaint=fun(_Evt,_Obj)->%%function to do when paint event called
					
					Paint=wxBufferedPaintDC:new(Panel),
					Bitmap=wxBitmap:new(Map_1),
					wxDC:drawBitmap(Paint,Bitmap,{0,0}),
					wxBitmap:destroy(Bitmap),
					add_units_to_screen(simData,Paint,senEts),
					
					wxBufferedPaintDC:destroy(Paint) end,					
	wxFrame:connect(Panel, paint, [{callback,OnPaint}]),%%connect paint event to panel with callbakc function OnPaint
	
	%io:format("########### ~p ##############3 ~n",[global:whereis_name(full)]),
	S=self(),
	register(refresher, spawn_link(fun() -> loop(S) end)),
	spawn_link(fun() -> watch_dog() end),
	
	{Panel, State};
		
do_init(Server) ->
	register(wx_server,self()),
	%%----------- init local servers (for each part of screen)
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
	%spawn('foo@rfire-PC',unit_server,start,[full]),

    Frame = wxFrame:new(Server, -1, "wx test sim", [{size,{?Horizontal, ?Vertical}}]),		%%create frame for the simulator
	Panel  = wxPanel:new(Frame,[{style, ?wxFULL_REPAINT_ON_RESIZE}]),						%%create panel from the frame
	wxFrame:show(Frame),																	%%show the frame on the screen	
	
	wxButton:new(Panel, 10, [{label, "&Start Game"},{pos,{3,13}},{size,{95,30}}]),			%%start game button
	Rand=wxButton:new(Panel, 11, [{label, "&Randomize"},{pos,{105,13}},{size,{95,30}}]),		%%randomize game button
	Smart_rand=wxButton:new(Panel, 12, [{label, "&smart spreading"},{pos,{207,13}},{size,{95,30}}]),		%%randomize units, with smart sensor spreading
	wxButton:new(Panel, 13, [{label, "&heli \nstatistics"},{pos,{1080,13}},{size,{95,30}}]),		%%randomize units, with smart sensor spreading
	wxButton:new(Panel, 14, [{label, "&gen_server \nstatistics"},{pos,{1180,13}},{size,{95,30}}]),		%%randomize units, with smart sensor spreading
	
	%wxFrame:connect(Frame, left_down),  													% Mouse left click 
	% wxFrame:connect(Frame, right_down), 			
	% Mouse right click 
	wxPanel:connect (Panel, left_down),
	
	wxWindow:connect(Panel, command_button_clicked),
	
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
	
	loc_monitor:start(wxMon),
	MonPid = global:whereis_name(wxMon),
	
	ets:new(simData,[set,named_table,{heir,MonPid,{wxServer,main}}]),
	ets:new(senEts,[set,named_table,{heir,MonPid,{wxServer,sen}}]),
	ets:new(sensAnm,[set,named_table]),
	
	ets:insert(senEts,{max_fire,0}),
	
	loc_monitor:add_mon(wxMon,self()),
	
	ets:insert(sensAnm,{angle,0}),
	
	State= #state{parent=Panel,canvas = Frame,heli_amount=Heli,fire_amount=Fire,sensor_amount=Sens,cash_amount=Cash,ets_name=simData,sen_ets_name=senEts,random_but=Rand,smart_but=Smart_rand,self=self()},
	
	OnPaint=fun(_Evt,_Obj)->%%function to do when paint event called
					
					Paint=wxBufferedPaintDC:new(Panel),
					Bitmap=wxBitmap:new(Map_1),
					wxDC:drawBitmap(Paint,Bitmap,{0,0}),
					wxBitmap:destroy(Bitmap),
					add_units_to_screen(simData,Paint,senEts),
					
					wxBufferedPaintDC:destroy(Paint) end,					
	wxFrame:connect(Panel, paint, [{callback,OnPaint}]),%%connect paint event to panel with callback function OnPaint
	
	S=self(),
	register(refresher, spawn_link(fun() -> loop(S) end)),
	spawn_link(fun() -> watch_dog() end),
	
	{Panel, State}.


%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info

%add fire at (x,y)
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State = #state{}) ->
	case ets:lookup(State#state.sen_ets_name,max_fire)==[] of
		false ->
			[{_,Max}] = ets:lookup(State#state.sen_ets_name,max_fire),
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
	
	ets:delete_all_objects(State#state.ets_name),
	ets:delete_all_objects(State#state.sen_ets_name),
	
	HeliTxt = wxTextCtrl:getValue(State#state.heli_amount),
	FireTxt = wxTextCtrl:getValue(State#state.fire_amount),
	SensTxt = wxTextCtrl:getValue(State#state.sensor_amount),
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
	
	
	Valid = (HeliValid and FireValid) and SenValid,
	
	case Valid == true of
		true -> ets:insert(State#state.sen_ets_name,{max_fire,FireNum}),
				%Tmp = erlang:system_time() / erlang:monotonic_time(),
				%Time = erlang:round((Tmp - erlang:trunc(Tmp)) * 100000000),
				%random:seed(Time,erlang:monotonic_time(),erlang:unique_integer()),
				Pid2 = getPid(self()),
				{A,_B,C}=erlang:now(),
				random:seed(C,Pid2*Pid2,erlang:round(C/A)),
				randUnit(heli,HeliNum,State#state.ets_name),
				%random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),
				randUnit(fire,FireNum,State#state.ets_name),
				%random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),
				randUnit(sensor,SenNum,State#state.sen_ets_name);
				%io:format("ets = ~p~n",[ets:tab2list(State#state.ets_name)]);
		false -> io:format("please enter NUMBERS into amount of units~n")
	end,
	
	Pid = State#state.self,
	Pid ! refresh,
	
	[TL_ets,TR_ets,BL_ets,BR_ets] = divide_unit_to_screens(State#state.ets_name,State#state.sen_ets_name),
	
	unit_server:create(tl,TL_ets),
	unit_server:create(tr,TR_ets),
	unit_server:create(bl,BL_ets),
	unit_server:create(br,BR_ets),
	
    {noreply,State};
	
%randomizing units, with smart spread of sensors	
handle_event(_Ev=#wx{id=12,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("smart randomizing units coordinates ~n"),
	
	ets:delete_all_objects(State#state.ets_name),
	ets:delete_all_objects(State#state.sen_ets_name),
	
	HeliTxt = wxTextCtrl:getValue(State#state.heli_amount),
	FireTxt = wxTextCtrl:getValue(State#state.fire_amount),
	CashTxt = wxTextCtrl:getValue(State#state.cash_amount),
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
	
	Valid = (HeliValid and FireValid) and CashValid,
	
	case Valid == true of
		true -> ets:insert(State#state.sen_ets_name,{max_fire,FireNum}),
				%Tmp = erlang:system_time() / erlang:monotonic_time(),
				%Time = erlang:round((Tmp - erlang:trunc(Tmp)) * 100000000),
				%random:seed(Time,erlang:monotonic_time(),erlang:unique_integer()),
				Pid2 = getPid(self()),
				{A,_B,C}=erlang:now(),
				random:seed(C,Pid2*Pid2,erlang:round(C/A)),
				randUnit(heli,HeliNum,State#state.ets_name),
				%random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),
				randUnit(fire,FireNum,State#state.ets_name),
				%random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),
				smart_sensor_randomize(CashNum,State#state.sen_ets_name);
				%randUnit(sensor,SenNum,State#state.sen_ets_name);
				%io:format("ets = ~p~n",[ets:tab2list(State#state.ets_name)]);
		false -> io:format("please enter NUMBERS into amount of units~n")
	end,
	
	Pid = State#state.self,
	Pid ! refresh,
	
	[TL_ets,TR_ets,BL_ets,BR_ets] = divide_unit_to_screens(State#state.ets_name,State#state.sen_ets_name),
	
	unit_server:create(tl,TL_ets),
	unit_server:create(tr,TR_ets),
	unit_server:create(bl,BL_ets),
	unit_server:create(br,BR_ets),
	
    {noreply,State};
	
%when start button clicked, starting simulation:
handle_event(_Ev=#wx{id=10,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("starting simulation ~n"),
	
	wxButton:disable(State#state.random_but),
	wxButton:disable(State#state.smart_but),
	unit_server:start_sim(tl),
	unit_server:start_sim(tr),
	unit_server:start_sim(bl),
	unit_server:start_sim(br),
	
    {noreply,State};

%when start button clicked, print all helicopters statistics 
handle_event(_Ev=#wx{id=13,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
    QH = qlc:q([HeliName || {{heli,HeliName},_,_,_} <- ets:table(State#state.ets_name)]),
	io:format("########## HELICOPTERS STATISTICS ##########~n~n"),
	spawn(fun() -> print_stat(qlc:eval(QH),heli) end),
	%[ heli:statistics(Name) || Name <- qlc:eval(QH)],
    {noreply,State};
	
%when start button clicked, print all gen_servers statistics 
handle_event(_Ev=#wx{id=14,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("########## GEN_SERVERS STATISTICS ##########~n~n"),
	spawn(fun() -> print_stat([tl,tr,bl,br],server) end),
	%[ unit_server:statistics(Name) || Name <- [tl,tr,bl,br]],
    {noreply,State};
	
handle_event(Ev = #wx{}, State = #state{}) ->
    io:format("Got Event ~p~n",[Ev]),
    {noreply,State}.

%% Callbacks handled as normal gen_server callbacks

handle_info(refresh,State=#state{})->

	%animation of sensor:
	
	[{_,Angle}] = ets:lookup(sensAnm,angle),
	ets:insert(sensAnm,{angle,Angle+ math:pi()/15}),
	
	%end animation of sensor:
	
	Updated_list = get_updated_data(State#state.ets_name), %unit_server:wx_update(tl) ++ unit_server:wx_update(tr) ++ unit_server:wx_update(bl) ++ unit_server:wx_update(br),
	ets:delete_all_objects(State#state.ets_name),
	[ ets:insert(State#state.ets_name,Server_list) || Server_list <- Updated_list],
	wxWindow:refresh(State#state.parent,[{eraseBackground,false}]),
	{noreply,State};

handle_info({crash,Num},_State)->
	{noreply,1/Num};

handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.
	
handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(Reason, State=#state{}) ->
	unregister(wx_server),
	%io:format("reason = ~p~n",[Reason]),
	wxWindow:destroy(State#state.canvas),
	wx:destroy(),
	case whereis(refresher) of
		undefined -> ok;
		Pid -> exit(Pid,kill)
	end,
	ets:delete(sensAnm),
	case Reason == wx_deleted of
		true ->
				ets:delete(senEts),
				ets:delete(simData),
				
				[shut_down_server(Server) || Server <- [tl,tr,bl,br]];
		false -> crash
	end,
	
    ok.
	  
  
%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(Pid) -> receive after ?WX_UPDATE_SPEED -> Pid ! refresh end, loop(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_to_number(Number) ->
  try list_to_integer( Number) of
    Val_integer -> Val_integer
  catch  
    error:_Why -> try list_to_float(Number) of
                    Val_float -> Val_float
                  catch 
                    error: Reason -> Reason
                  end   
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% accessing helicopter with ets:match(EtsName,{heli,HeliName,'$1','$2','$3'}). ->
%%%%%%%%%%%%%%%%%%% return a list of the information of the heli
randUnit(_,0,_) -> ok;
randUnit(heli,Amount,EtsName) ->
	X = random:uniform(?Horizontal-?HELI_PIC_SIZE*2)+?HELI_PIC_SIZE,
	Y  = random:uniform(?Vertical-?HELI_PIC_SIZE*4)+?HELI_PIC_SIZE,
	%X = random:uniform(?Horizontal),
	%Y  = random:uniform(?Vertical),
	HeliData = {{heli,list_to_atom("heli" ++ integer_to_list(Amount))},X,Y,not_working},
	%add_to_ets(heli,HeliData,EtsName),
	ets:insert(EtsName,HeliData),
	randUnit(heli,Amount-1,EtsName);

randUnit(fire,Amount,EtsName) ->
	X = random:uniform(?Horizontal-?FireDefaultRadius*5)+?FireDefaultRadius*2,
	Y  = random:uniform(?Vertical-?FireDefaultRadius)+?FireDefaultRadius,
	%X = random:uniform(?Horizontal),
	%Y  = random:uniform(?Vertical),
	FireData = {{fire,list_to_atom("fire" ++ integer_to_list(Amount))},?FireDefaultRadius,X,Y},
	%add_to_ets(fire,FireData,EtsName),
	ets:insert(EtsName,FireData),
	randUnit(fire,Amount-1,EtsName);

randUnit(sensor,Amount,EtsName) ->
	X = random:uniform(?Horizontal-?SensorRadius*4)+?SensorRadius*2,
	Y  = random:uniform(?Vertical-?SensorRadius*3)+?SensorRadius,
	SensName=list_to_atom("sensor" ++ integer_to_list(Amount)),
	SensorData = {{sensor,SensName},?SensorRadius,X,Y},
	%add_to_ets(sens,SensorData,EtsName),
	ets:insert(EtsName,SensorData),
	randUnit(sensor,Amount-1,EtsName).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_units_to_screen(EtsName,Paint,SenEts) ->
	
	[{_,Angle}] = ets:lookup(sensAnm,angle),
	
		
	QH_fire = qlc:q([[R,X,Y] || {{fire,_},R,X,Y} <- ets:table(EtsName)]),
	QH_heli = qlc:q([[X,Y] || {{heli,_},X,Y,_} <- ets:table(EtsName)]),
	QH_sensor = qlc:q([[R,X,Y,SensName,Angle] || {{sensor,SensName},R,X,Y} <- ets:table(SenEts)]),

	[ add_unit_to_screen(sensor,Unit,Paint) || Unit <- qlc:eval(QH_sensor)],
	[ add_unit_to_screen(fire,Unit,Paint) || Unit <- qlc:eval(QH_fire)],
	[ add_unit_to_screen(heli,Unit,Paint) || Unit <- qlc:eval(QH_heli)].
	
add_unit_to_screen(heli,[X,Y],Paint) ->
	
	Image1 = wxImage:new("pic/heli_4.png"),
	Image2 = wxImage:scale(Image1, ?HELI_PIC_SIZE,?HELI_PIC_SIZE),
	%%%%%%%%%%%%%%%%%%%%Image5 = wxImage:rotate(Image4, Angle, {200,200}),
	Bmp = wxBitmap:new(Image2),	
	wxImage:destroy(Image1),
	wxImage:destroy(Image2),
	%%%%%%%%%%wxImage:destroy(Image5),
	wxDC:drawBitmap(Paint, Bmp, {round(X)-?HELI_PIC_HALF,round(Y)-?HELI_PIC_HALF}),
	wxBitmap:destroy(Bmp);
	
add_unit_to_screen(fire,[R,X,Y],Paint) ->
	case 2*round(R)>0 of
		true -> 
				Image1 = wxImage:new("pic/fire_2.png"),
				Image2 = wxImage:scale(Image1, 2*round(R),2*round(R)),
				%%%%%%%%%%%Image51 = wxImage:rotate(Image4, Angle, {200,200}),
				Bmp = wxBitmap:new(Image2),
				wxImage:destroy(Image1),
				wxImage:destroy(Image2),
				%%%%%%%%%%%%wxImage:destroy(Image51),
				wxDC:drawBitmap(Paint, Bmp, {round(X-R),round(Y-R)}),
				wxBitmap:destroy(Bmp);
		false -> do_nothing
	end;
	
add_unit_to_screen(sensor,[R,X,Y,_SensName,Angle],Paint) ->

	%% ---------------old
	%Image1 = wxImage:new(ImagePath),
	%Image2 = wxImage:scale(Image1, 2*round(R),2*round(R)),
	%Bmp = wxBitmap:new(Image2),
	%wxImage:destroy(Image1),
	%wxImage:destroy(Image2),
	%wxDC:drawBitmap(Paint, Bmp, {round(X-R),round(Y-R)}),
	%wxBitmap:destroy(Bmp).
	%% ---------------old
	
	%% ---------------temp
	%io:format("angle=~p~n",[Angle]),
	wxDC:drawCircle(Paint, {X,Y}, R),
	wxDC:drawLine(Paint, {X, Y}, {erlang:round(X+math:cos(Angle)*R),erlang:round(Y+math:sin(Angle)*R)}),
	wxDC:drawLine(Paint, {X, Y}, {erlang:round(X+math:cos(Angle+math:pi()/10)*R),erlang:round(Y+math:sin(Angle+math:pi()/10)*R)}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
connect_to_nodes([]) -> ok;
connect_to_nodes([Node|Rest]) -> case net_adm:ping(Node) of
									pong -> 
										connect_to_nodes(Rest);
									pang ->
										io:format("ERROR: Cannot connect to node ~p.~n",[Node]),
										error_in_nodes_connect
								 end.
								 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shut_down_server(Name) ->
	case global:whereis_name(Name) of
		undefined -> ok;
		_any -> unit_server:stop(Name)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% axis of wx goes:
%% x lowest at left, highest at right
%% y lowest at top, highest at bottom
divide_unit_to_screens(MainEts,SenEts)->
	QH_tl_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X>?Horizontal/2, Y>?Vertical/2]),
	
	QH_tl_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X>?Horizontal/2, Y>?Vertical/2]),
	
	%io:format("SENSORS ARE = ~p~n Hor/2 =~p ; Ver/2 = ~p~n",[ets:tab2list(SenEts),?Horizontal/2,?Vertical/2]),
	QH_tl_sensor = qlc:q([ {{sensor,Name},R,X,Y}|| {{sensor,Name},R,X,Y} <- ets:table(SenEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_sensor = qlc:q([ {{sensor,Name},R,X,Y}|| {{sensor,Name},R,X,Y} <- ets:table(SenEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_sensor = qlc:q([ {{sensor,Name},R,X,Y}|| {{sensor,Name},R,X,Y} <- ets:table(SenEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_sensor = qlc:q([ {{sensor,Name},R,X,Y}|| {{sensor,Name},R,X,Y} <- ets:table(SenEts), X>?Horizontal/2, Y>?Vertical/2]),
	
	TL_list = qlc:eval(QH_tl_fire) ++ qlc:eval(QH_tl_heli) ++ qlc:eval(QH_tl_sensor),
	TR_list = qlc:eval(QH_tr_fire) ++ qlc:eval(QH_tr_heli) ++ qlc:eval(QH_tr_sensor),
	BL_list = qlc:eval(QH_bl_fire) ++ qlc:eval(QH_bl_heli) ++ qlc:eval(QH_bl_sensor),
	BR_list = qlc:eval(QH_br_fire) ++ qlc:eval(QH_br_heli) ++ qlc:eval(QH_br_sensor),
	%io:format("TL = ~p~n",[TL_list]),
	%io:format("TR = ~p~n",[TR_list]),
	%io:format("BL = ~p~n",[BL_list]),
	%io:format("BR = ~p~n",[BR_list]),
	[TL_list,TR_list,BL_list,BR_list].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wait_all_on([]) -> ok;
wait_all_on([H|T]) ->
	case global:whereis_name(H) of
		undefined -> wait_all_on([H|T]);
		_Any -> wait_all_on(T)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_updated_data(MainEts)->

	QH_tl_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_fire = qlc:q([ {{fire,Name},R,X,Y}|| {{fire,Name},R,X,Y} <- ets:table(MainEts), X>?Horizontal/2, Y>?Vertical/2]),
	
	QH_tl_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X<?Horizontal/2, Y<?Vertical/2]),
	QH_tr_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X>?Horizontal/2, Y<?Vertical/2]),
	QH_bl_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X<?Horizontal/2, Y>?Vertical/2]),
	QH_br_heli = qlc:q([ {{heli,Name},X,Y,State}|| {{heli,Name},X,Y,State} <- ets:table(MainEts), X>?Horizontal/2, Y>?Vertical/2]),
	
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
	[Tl,Tr,Bl,Br].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rand_single_sensor(_Radius,_Number,Tries,_Ets) when Tries==0 -> false; %io:format("reached try=0~n"),

rand_single_sensor(Radius,Number,Tries,Ets) when Tries /= 0 -> 
	X = random:uniform(?Horizontal-Radius*4)+Radius*2,
	Y  = random:uniform(?Vertical-Radius*3)+Radius,
	QH = qlc:q([ found || {{sensor,_Name},RS,XS,YS} <- ets:table(Ets),((X-XS)*(X-XS) + (Y-YS)*(Y-YS))< (Radius+RS)*(Radius+RS)]),
	case qlc:eval(QH) == [] of
		true -> SensName=list_to_atom("sensor" ++ integer_to_list(Number)),
				SensorData = {{sensor,SensName},Radius,X,Y},
				ets:insert(Ets,SensorData),
				true;
		false -> rand_single_sensor(Radius,Number,Tries-1,Ets)
	end.
	
add_sensor_to_screen(Type,Number,Ets)->
	case Type of
		large -> rand_single_sensor(?LARGE_SENSOR_SIZE,Number,20,Ets);
		medium -> rand_single_sensor(?MEDIUM_SENSOR_SIZE,Number,40,Ets);
		small -> rand_single_sensor(?SMALL_SENSOR_SIZE,Number,60,Ets)
	end.

smart_sensor_randomize(Cash,Ets) -> smart_sensor_randomize(Cash,1,large,Ets).
	
smart_sensor_randomize(Cash,Number,Size,Ets) when Size == large -> 
	case Cash >= ?LARGE_SENSOR_PRICE of
		true -> case add_sensor_to_screen(large,Number,Ets) of 
					true -> smart_sensor_randomize(Cash-?LARGE_SENSOR_PRICE,Number+1,large,Ets);
					false -> smart_sensor_randomize(Cash,Number,medium,Ets)
				end;
		false -> smart_sensor_randomize(Cash,Number,medium,Ets)
	end;
	
smart_sensor_randomize(Cash,Number,Size,Ets) when Size == medium -> 
	case Cash >= ?MEDIUM_SENSOR_PRICE of
		true -> case add_sensor_to_screen(medium,Number,Ets) of 
					true -> smart_sensor_randomize(Cash-?MEDIUM_SENSOR_PRICE,Number+1,large,Ets);
					false -> smart_sensor_randomize(Cash,Number,small,Ets)
				end;
		false -> smart_sensor_randomize(Cash,Number,small,Ets)
	end;

smart_sensor_randomize(Cash,Number,Size,Ets) when Size == small -> 
	case Cash >= ?SMALL_SENSOR_PRICE of
		true -> case add_sensor_to_screen(small,Number,Ets) of 
					true -> smart_sensor_randomize(Cash-?SMALL_SENSOR_PRICE,Number+1,large,Ets);
					false -> stop
				end;
		false -> stop
	end.	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_stat([],_Type) -> io:format("~n");

print_stat([H|T],Type) when Type == heli ->
	heli:statistics(H),
	timer:sleep(200),
	print_stat(T,Type);
	
print_stat([H|T],Type) when Type == server ->
	unit_server:statistics(H),
	timer:sleep(200),
	print_stat(T,Type).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
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
											 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
watch_dog() ->
	net_adm:ping(?TLSERVER_NODE),
	net_adm:ping(?TRSERVER_NODE),
	net_adm:ping(?BLSERVER_NODE),
	net_adm:ping(?BRSERVER_NODE),
	timer:sleep(1000),
	watch_dog().