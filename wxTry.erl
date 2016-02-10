-module(wxTry).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([start/0, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
	 
	 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(Horizontal,				1200).
-define(Vertical,				556).

-define(FireDefaultRadius, 50).
-define(SensorRadius, 50).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, 
		{
		 frame,
		 panel,
		 parent,
		 canvas,
		 heli_amount,
		 fire_amount,
		 sensor_amount,
		 random_but,
		 ets_name,
		 self
		}).

start() ->
	Server = wx:new(),
    wx_object:start(?MODULE, Server, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Server) ->
        wx:batch(fun() -> do_init(Server) end).

do_init(Server) ->
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
	
	Map1 = wxImage:new("forest.jpg"),														%%background image
	Map_1 = wxImage:scale(Map1, ?Horizontal,?Vertical),										%%scale image
	wxImage:destroy(Map1),
	
	%static texts
	wxStaticText:new(Panel, 201,"amount of helicopters:",[{pos,{210,22}}]),
	wxStaticText:new(Panel, 202,"amount of fires",[{pos,{480,22}}]),
	wxStaticText:new(Panel, 203,"amount of sensors",[{pos,{720,22}}]),
	Heli=wxTextCtrl:new(Panel, 101,[{value, "5"},{pos,{340,18}}]), %set default value
    Fire=wxTextCtrl:new(Panel, 102,[{value, "5"},{pos,{580,18}}]),
	Sens=wxTextCtrl:new(Panel, 103,[{value, "5"},{pos,{830,18}}]),
	
	ets:new(simData,[bag,public,named_table]),
	%ets:insert(simData,{heli,[]}),
	%ets:insert(simData,{fire,[]}),
	%ets:insert(simData,{sens,[]}),
	
	State= #state{parent=Panel,canvas = Frame,heli_amount=Heli,fire_amount=Fire,sensor_amount=Sens,ets_name=simData,random_but=Rand,self=self()},
	
	OnPaint=fun(_Evt,_Obj)->%%function to do when paint event called
					
					Paint=wxBufferedPaintDC:new(Panel),
					Bitmap=wxBitmap:new(Map_1),
					wxDC:drawBitmap(Paint,Bitmap,{0,0}),
					wxBitmap:destroy(Bitmap),
					add_units_to_screen(simData,Paint),
					
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
	
	%S=self(),
	%spawn_link(fun() -> loop(S) end),
	
	{Panel, State}.


%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info

%when randomize button clicked, randomizing all units places:
handle_event(Ev=#wx{id=11,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("randomizing units coordinates ~p~n",[Ev]),
	
	try ets:delete( State#state.ets_name) of
		_ -> ets:new(State#state.ets_name,[bag,public,named_table])
			 %ets:insert(State#state.ets_name,{heli,[]}),
			 %ets:insert(State#state.ets_name,{fire,[]}),
			 %ets:insert(State#state.ets_name,{sens,[]})
	catch  
	error:_Why -> ets:new(State#state.ets_name,[bag,public,named_table])
			 %ets:insert(State#state.ets_name,{heli,[]}),
			 %ets:insert(State#state.ets_name,{fire,[]}),
			 %ets:insert(State#state.ets_name,{sens,[]}) 
	end,
	
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
		true -> random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),
				randUnit(heli,HeliNum,State#state.ets_name),
				random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),
				randUnit(fire,FireNum,State#state.ets_name),
				random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),
				randUnit(sensor,SenNum,State#state.ets_name),
				io:format("ets = ~p~n",[ets:tab2list(State#state.ets_name)]);
		false -> io:format("please enter NUMBERS into amount of units~n")
	end,
	
	Pid = State#state.self,
	Pid ! refresh,
	
    {noreply,State};
	
%when start button clicked, starting simulation:
handle_event(Ev=#wx{id=10,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
	io:format("starting simulation ~p~n",[Ev]),
	
	wxButton:disable(State#state.random_but),
	
    {noreply,State};

handle_event(Ev = #wx{}, State = #state{}) ->
    io:format("Got Event ~p~n",[Ev]),
    {noreply,State}.

%% Callbacks handled as normal gen_server callbacks

handle_info(refresh,State)->
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

terminate(_Reason, _State) ->
    ok.
	  
  
%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(Pid) -> receive after 20 -> Pid ! refresh end, loop(Pid).

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
	X = random:uniform(?Horizontal-200)+100,
	Y  = random:uniform(?Vertical-200)+100,
	HeliData = {heli,list_to_atom("heli" ++ integer_to_list(Amount)),X,Y,not_working},
	%add_to_ets(heli,HeliData,EtsName),
	ets:insert(EtsName,HeliData),
	randUnit(heli,Amount-1,EtsName);

randUnit(fire,Amount,EtsName) ->
	X = random:uniform(?Horizontal-100)+50,
	Y  = random:uniform(?Vertical-100)+50,
	FireData = {fire,list_to_atom("fire" ++ integer_to_list(Amount)),X,Y,?FireDefaultRadius},
	%add_to_ets(fire,FireData,EtsName),
	ets:insert(EtsName,FireData),
	randUnit(fire,Amount-1,EtsName);

randUnit(sensor,Amount,EtsName) ->
	X = random:uniform(?Horizontal),
	Y  = random:uniform(?Vertical),
	SensorData = {sens,list_to_atom("sensor" ++ integer_to_list(Amount)),X,Y,?SensorRadius},
	%add_to_ets(sens,SensorData,EtsName),
	ets:insert(EtsName,SensorData),
	randUnit(sensor,Amount-1,EtsName).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_units_to_screen(EtsName,Paint) ->
		
	FiresData = ets:match(EtsName,{fire,'_','$1','$2','$3'}),
	[ add_unit_to_screen(Unit,Paint) || Unit <-FiresData],
	
	HelicoptersData = ets:match(EtsName,{heli,'_','$1','$2','_'}),
	[ add_unit_to_screen(Unit,Paint) || Unit <-HelicoptersData].
	
	%SensorsData = ets:match(EtsName,{sens,'_','$1','$2','$3'}),
	%[ add_unit_to_screen(Unit) || Unit <-SensorsData].
	
add_unit_to_screen([X,Y],Paint) ->
	
	Image1 = wxImage:new("4.jpg"),
	Image2 = wxImage:scale(Image1, 100,100),
	%%%%%%%%%%%%%%%%%%%%Image5 = wxImage:rotate(Image4, Angle, {200,200}),
	Bmp = wxBitmap:new(Image2),	
	wxImage:destroy(Image1),
	wxImage:destroy(Image2),
	%%%%%%%%%%wxImage:destroy(Image5),
	wxDC:drawBitmap(Paint, Bmp, {round(X)-100,round(Y)-100}),
	wxBitmap:destroy(Bmp);
	
add_unit_to_screen([X,Y,R],Paint) ->
	Image1 = wxImage:new("fire_1.png"),
	Image2 = wxImage:scale(Image1, 2*round(R),2*round(R)),
	%%%%%%%%%%%Image51 = wxImage:rotate(Image4, Angle, {200,200}),
	Bmp = wxBitmap:new(Image2),
	wxImage:destroy(Image1),
	wxImage:destroy(Image2),
	%%%%%%%%%%%%wxImage:destroy(Image51),
	wxDC:drawBitmap(Paint, Bmp, {round(X-R),round(Y-R)}),
	wxBitmap:destroy(Bmp).