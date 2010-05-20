-module(date_util).
-compile(export_all).

epoch() ->
  now_to_seconds(now()).   
    
time_right_now() ->
  {MSec, Sec, _} = now(),
  MSec * 1000000 + Sec.

% Calc the elapsed
calc_elapsed({MSecStart, SecStart, MicroSecStart}, {MSecFinish, SecFinish, MicroSecFinish}) ->
  (MSecFinish * 1000000 + SecFinish + MicroSecFinish / 1000000) -
  (MSecStart * 1000000 + SecStart + MicroSecStart / 1000000).

epoch_hires() ->
  now_to_seconds_hires(now()).   
    
now_to_seconds({Mega, Sec, _}) -> (Mega * 1000000) + Sec.

seconds_to_timestamp(Seconds) ->
  % BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  % Seconds       = BaseDate + (Milliseconds div 1000),
  calendar:gregorian_seconds_to_datetime(Seconds).

now_to_seconds() -> calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now())).
time_difference_from_now(TimeInSeconds) -> now_to_seconds() - TimeInSeconds.

now_to_milliseconds({Mega, Sec, Micro}) ->
    now_to_seconds({Mega, Sec, Micro}) * 1000.
    
now_to_seconds_hires({Mega, Sec, Micro}) ->
    now_to_seconds({Mega, Sec, Micro}) + (Micro / 1000000).
    
now_to_milliseconds_hires({Mega, Sec, Micro}) ->
    now_to_seconds_hires({Mega, Sec, Micro}) * 1000.
    
epoch_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).
        
now_to_gregorian_seconds() ->
    epoch_to_gregorian_seconds(now()).
        
epoch_to_gregorian_seconds({Mega, Sec, Micro}) ->
    epoch_to_gregorian_seconds(now_to_seconds({Mega, Sec, Micro}));
epoch_to_gregorian_seconds(Now) ->
    EpochSecs = epoch_gregorian_seconds(), Now + EpochSecs.

gregorian_seconds_to_epoch(Secs) ->
    EpochSecs = epoch_gregorian_seconds(), Secs - EpochSecs.

date_to_epoch(Date) ->
    datetime_to_epoch({Date, {0,0,0} })
.

datetime_to_epoch({Date, Time}) ->
    gregorian_seconds_to_epoch(
        calendar:datetime_to_gregorian_seconds({Date, Time}))
.

is_older_by(T1, T2, {days, N}) ->
    N1 = day_difference(T1, T2)
    , case N1 of
        N2 when (-N < N2) ->
            true;
        _ ->
            false
    end
.

is_sooner_by(T1, T2, {days, N}) ->
    case day_difference(T1, T2) of
        N1 when N > N1 ->
            true;
        _ ->
            false
    end
.

is_time_older_than({Date, Time}, Mark) ->
    is_time_older_than(calendar:datetime_to_gregorian_seconds({Date, Time}), Mark);
is_time_older_than(Time, {DateMark, TimeMark}) ->
    is_time_older_than(Time, calendar:datetime_to_gregorian_seconds({DateMark, TimeMark}));
is_time_older_than(Time, Mark)  when is_integer(Time), is_integer(Mark) ->
    Time < Mark.

day_difference({D1, _}, D2) ->
    day_difference(D1, D2);
day_difference(D1, {D2, _}) ->
    day_difference(D1, D2);
day_difference(D1, D2) ->
    Days1 = calendar:date_to_gregorian_days(D1), Days2 = calendar:date_to_gregorian_days(D2), Days1 - Days2.

is_time_sooner_than({Date, Time}, Mark) ->
    is_time_sooner_than(calendar:datetime_to_gregorian_seconds({Date, Time}), Mark);
is_time_sooner_than(Time, {DateMark, TimeMark}) ->
    is_time_sooner_than(Time, calendar:datetime_to_gregorian_seconds({DateMark, TimeMark}));
is_time_sooner_than(Time, Mark)  when is_integer(Time), is_integer(Mark) ->
    Time > Mark.

subtract(Date, {days, N}) ->
    New = calendar:date_to_gregorian_days(Date) - N, calendar:gregorian_days_to_date(New).

add(Date, {days, N}) ->
    New = calendar:date_to_gregorian_days(Date) + N, calendar:gregorian_days_to_date(New).
    
pad_zero(L) when length(L) == 1 -> "0" ++ L;
pad_zero(L) -> L.

fmt_date(TimeStamp) ->
  {{Y, M, D}, {Hr, Min, Sec}} = try
    seconds_to_timestamp(TimeStamp)
  catch
    error:_Why -> 
      calendar:now_to_datetime(now())
  end,
  MinStr = pad_zero(lists:flatten(io_lib:format("~w", [Min]))),
  SecStr = pad_zero(lists:flatten(io_lib:format("~w", [Sec]))),
  io_lib:format("~w/~w/~w ~w:~s:~s", [M, D, Y, Hr, MinStr, SecStr]).