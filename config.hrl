
%erl -setcookie hi -name 'Grf_server@127.0.0.1'
%erl -setcookie hi -name 'ULserver@127.0.0.1'

-define(GRF_SERVER_NODE, 'Grf_server@127.0.0.1').
-define(ULSERVER_NODE,   'ULserver@127.0.0.1').
-define(URSERVER_NODE,   'URserver@127.0.0.1').
-define(DLSERVER_NODE,   'DLserver@127.0.0.1').
-define(DRSERVER_NODE,   'DRserver@127.0.0.1').
-define(SERVERS_CNT,4).

%-define(UPDATE_INTERVAL, 		33).  % 30 frames per second
%-define(GAME_SPEED, 			2).   % Game Speed: from 1 to 10
%-define(CLOCK,					5).
%-define(SCAN_INTERVAL, 			500).   
%-define(X_AXIS_BORDER,			(-?Horizontal/2)).
%-define(Y_AXIS_BORDER,			(-?Vertical/2)).


%fire
-define(FIRE_REFRESH_SPEED,100).


%heli
-define(MAXX, 1200-25).
-define(MINX, 0-25).
-define(MAXY, 556-25).
-define(MINY, 0-25).

-define(MOVEMENT_SPEED,100).
-define(REFRESH_SPEED,10).
-define(EXTINGUISH_SPEED,40).

%unit_server
-define(OVERLAP_PERC, 90).

%wxTry
-define(Horizontal,1200).
-define(Vertical,556).

-define(FireDefaultRadius, 50).
-define(SensorRadius, 50).
-define(HELI_PIC_SIZE, 50).
-define(HELI_PIC_HALF, round(?HELI_PIC_SIZE/2)).