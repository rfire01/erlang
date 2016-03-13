
%erl -setcookie hi -name 'Grf_server@127.0.0.1'
%erl -setcookie hi -name TLserver@127.0.0.1

-define(WX_SERVER_NODE, 'wx_server@10.42.0.21').
-define(TLSERVER_NODE,   'TLserver@10.42.0.21').
-define(TRSERVER_NODE,   'TRserver@10.42.0.21').
-define(BLSERVER_NODE,   'BLserver@10.42.0.21').
-define(BRSERVER_NODE,   'BRserver@10.42.0.21').

%-define(WX_SERVER_NODE, 'wx_server@132.73.201.153').
%-define(TLSERVER_NODE,   'LT@132.72.104.211').
%-define(TRSERVER_NODE,   'RT@132.72.104.213').
%-define(BLSERVER_NODE,   'LB@132.73.201.8').
%-define(BRSERVER_NODE,   'RB@132.72.104.212').

%sensor
-define(SENSOR_CHECK_TIME,1000).


%fire
-define(FIRE_REFRESH_SPEED,200).
-define(FIRE_INCRESE_SPEED,100).


%heli
-define(MAXX, ?Horizontal-?HELI_PIC_HALF).
-define(MINX, 0-?HELI_PIC_HALF).
-define(MAXY, ?Vertical-?HELI_PIC_HALF).
-define(MINY, 0-?HELI_PIC_HALF).

-define(MOVEMENT_SPEED,100).
-define(REFRESH_SPEED,80).
-define(EXTINGUISH_SPEED,160).

%unit_server
-define(OVERLAP_PERC, 90).

%wxTry
-define(Horizontal,1300).
-define(Vertical,556).

-define(FireDefaultRadius, 20).
 
-define(SensorRadius, 50).

-define(HELI_PIC_SIZE, 50).
-define(HELI_PIC_HALF, round(?HELI_PIC_SIZE/2)).

-define(WX_UPDATE_SPEED, 60).

-define(LARGE_SENSOR_SIZE,60).
-define(MEDIUM_SENSOR_SIZE,40).
-define(SMALL_SENSOR_SIZE,15).

-define(LARGE_SENSOR_PRICE,30).
-define(MEDIUM_SENSOR_PRICE,20).
-define(SMALL_SENSOR_PRICE,7.5).
