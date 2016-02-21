-module(wxTry).

-include_lib("wx/include/wx.hrl").
-include_lib("stdlib/include/qlc.hrl").

-behaviour(wx_object).
-export([start/0, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
	 
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
		 random_but,
		 ets_name,
		 sen_ets_name,
		 self
		}).

start() ->
	Server = wx:new(),
    wx_object:start(?MODULE, Server, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Server) ->
        wx:batch(fun() -> do_init(Server) end).

do_init(Server) ->
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
	
	%wxFrame:connect(Frame, left_down),  													% Mouse left click 
	% wxFrame:connect(Frame, right_down), 			
	% Mouse right click 
	%wxPanel:connect (Panel, left_up),
	
	wxWindow:connect(Panel, command_button_clicked),
	
	Map1 = wxImage:new("pic/forest_3.jpg"),														%%background image
	Map_1 = wxImage:scale(Map1, ?Horizontal,?Vertical),										%%scale image
	wxImage:destroy(Map1),
	
	%static texts
	wxStaticText:new(Panel, 201,"amount of helicopters:",[{pos,{210,22}}]),
	wxStaticText:new(Panel, 202,"amount of fires",[{pos,{480,22}}]),
	wxStaticText:new(Panel, 203,"amount of sensors",[{pos,{720,22}}]),
	Heli=wxTextCtrl:new(Panel, 101,[{value, "1"},{pos,{340,18}}]), %set default value
    Fire=wxTextCtrl:new(Panel, 102,[{value, "1"},{pos,{580,18}}]),
	Sens=wxTextCtrl:new(Panel, 103,[{value, "5"},{pos,{830,18}}]),
	
	ets:new(simData,[set,public,named_table]),
	ets:new(senEts,[set,public,named_table]),
	ets:new(sensAnm,[set,public,named_table]),
	
	ets:insert(sensAnm,{picNum,1}),
	%ets:insert(simData,{heli,[]}),
	%ets:insert(simData,{fire,[]}),
	%ets:insert(simData,{sens,[]}),
	
	State= #state{parent=Panel,canvas = Frame,heli_amount=Heli,fire_amount=Fire,sensor_amount=Sens,ets_name=simData,sen_ets_name=senEts,random_but=Rand,self=self()},
	
	OnPaint=fun(_Evt,_Obj)->%%function to do when paint event called
					
					Paint=wxBufferedPaintDC:new(Panel),
					Bitmap=wxBitmap:new(Map_1),
					wxDC:drawBitmap(Paint,Bitmap,{0,0}),
					wxBitmap:destroy(Bitmap),
					add_units_to_screen(simData,Paint,senEts),
					
					%helicopter
					%[{_,X}] = ets:lookup(cord,x),
					%[{_,Y}] = ets:lookup(cord,y),
					%%%%%%%io:format("(x,y) = (~p,~p)~n",[X,Y]),
					
					%Image3 = wxImage:new("4.jpg"),
					%Image4 = wxImage:scale(Image3, 100,100),
					%%%%%%%%%%%%%%%%%%%%Image5 = wxImage:rotate(Image4, Angle, {200,200}),
					%Bmp1 = wxBitmap:new(Image4),	
					%wxImage:destroy(Image3),
					%wxImage:destroy(Image4),
					%%%%%%%%%%wxImage:destroy(Image5),
					%wxDC:drawBitmap(Paint, Bmp1, {round(X)-100,round(Y)-100}),
					%wxBitmap:destroy(Bmp1),
					
					%fire
					%[{_,FX}] = ets:lookup(firedata,x),
					%[{_,FY}] = ets:lookup(firedata,y),
					%[{_,FR}] = ets:lookup(firedata,radius),
					%%%%%%%%%io:format("(x,y) = (~p,~p,~p)~n",[FX,FY,FR]),
					
					%Image31 = wxImage:new("fire_1.png"),
					%Image41 = wxImage:scale(Image31, 2*round(FR),2*round(FR)),
					%%%%%%%%%%%Image51 = wxImage:rotate(Image4, Angle, {200,200}),
					%Bmp11 = wxBitmap:new(Image41),
					%wxImage:destroy(Image31),
					%wxImage:destroy(Image41),
					%%%%%%%%%%%%wxImage:destroy(Image51),
					%wxDC:drawBitmap(Paint, Bmp11, {round(FX-FR),round(FY-FR)}),
					%wxBitmap:destroy(Bmp11),
					
					wxBufferedPaintDC:destroy(Paint) end,					
	wxFrame:connect(Panel, paint, [{callback,OnPaint}]),%%connect paint event to panel with callbakc function OnPaint
	
	%io:format("########### ~p ##############3 ~n",[global:whereis_name(full)]),
	S=self(),
	register(refresher, spawn_link(fun() -> loop(S) end)),
	
	{Panel, State}.


%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info

%when randomize button clicked, randomizing all units places:
handle_event(Ev=#wx{id=11,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("randomizing units coordinates ~p~n",[Ev]),
	
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
		true -> Tmp = erlang:system_time() / erlang:monotonic_time(),
				Time = erlang:round((Tmp - erlang:trunc(Tmp)) * 100000000),
				random:seed(Time,erlang:monotonic_time(),erlang:unique_integer()),
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
	
%when start button clicked, starting simulation:
handle_event(Ev=#wx{id=10,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("starting simulation ~p~n",[Ev]),
	
	wxButton:disable(State#state.random_but),
	unit_server:start_sim(tl),
	unit_server:start_sim(tr),
	unit_server:start_sim(bl),
	unit_server:start_sim(br),
	
    {noreply,State};

handle_event(Ev = #wx{}, State = #state{}) ->
    io:format("Got Event ~p~n",[Ev]),
    {noreply,State}.

%% Callbacks handled as normal gen_server callbacks

handle_info(refresh,State=#state{})->

	%animation of sensor:
	
	[{_,PicNum}] = ets:lookup(sensAnm,picNum),
	case PicNum==21 of
		true -> ets:insert(sensAnm,{picNum,1});
		false -> ets:insert(sensAnm,{picNum,PicNum+1})
	end,
	
	%end animation of sensor:
	
	Updated_list = unit_server:wx_update(tl) ++ unit_server:wx_update(tr) ++ unit_server:wx_update(bl) ++ unit_server:wx_update(br),
	ets:delete_all_objects(State#state.ets_name),
	ets:insert(State#state.ets_name,Updated_list),
	wxWindow:refresh(State#state.parent,[{eraseBackground,false}]),
	{noreply,State};


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

terminate(_Reason, State=#state{}) ->
	wxWindow:destroy(State#state.canvas),
	wx:destroy(),
	case whereis(refresher) of
		undefined -> ok;
		Pid -> exit(Pid,kill)
	end,
	
	spawn( fun() -> shut_down_server(tl) end),
	spawn( fun() -> shut_down_server(tr) end),
	spawn( fun() -> shut_down_server(bl) end),
	spawn( fun() -> shut_down_server(br) end),
	
    ok.
	  
  
%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(Pid) -> receive after 30 -> Pid ! refresh end, loop(Pid).

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
	ets:insert(sensAnm,{SensName,2}),
	randUnit(sensor,Amount-1,EtsName).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_units_to_screen(EtsName,Paint,SenEts) ->
		
		
	QH_fire = qlc:q([[R,X,Y] || {{fire,_},R,X,Y} <- ets:table(EtsName)]),
	QH_heli = qlc:q([[X,Y] || {{heli,_},X,Y,_} <- ets:table(EtsName)]),
	QH_sensor = qlc:q([[R,X,Y,SensName] || {{sensor,SensName},R,X,Y} <- ets:table(SenEts)]),

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
	
add_unit_to_screen(sensor,[R,X,Y,_SensName],Paint) ->
 
	%ets:tab2list(),
	
	%% ---------------yoed code
%	[{_,PicName}] = ets:lookup(sensAnm,SensName),
%	 case PicName == 4 of
%			  true-> PicName2=1;
%			  false-> PicName2=PicName+1
%	 end,
%	SensImgName="pic/sensor/se_" ++ integer_to_list(PicName) ++ ".png",
	%% ---------------end yoed code
	
	[{_,PicNum}] = ets:lookup(sensAnm,picNum),
	SensImgName="sensorPics/sensor" ++ integer_to_list(PicNum) ++ ".png",
	Image1 = wxImage:new(SensImgName),
	Image2 = wxImage:scale(Image1, 2*round(R),2*round(R)),
	%%%%%%%%%%%Image51 = wxImage:rotate(Image4, Angle, {200,200}),
	Bmp = wxBitmap:new(Image2),
	wxImage:destroy(Image1),
	wxImage:destroy(Image2),
	%%%%%%%%%%%%wxImage:destroy(Image51),
	wxDC:drawBitmap(Paint, Bmp, {round(X-R),round(Y-R)}),
	wxBitmap:destroy(Bmp).
	%% ---------------end yoed code

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
		_any -> gen_server:stop({global,Name})
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