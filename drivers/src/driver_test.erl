-module(driver_test).
-compile([export_all]).

start() ->
    ads1015_driver_app:start(),
    {ok, Channel} = ads1015_driver:get_current_channel(),
    if Channel /= 1 ->
	    ads1015_driver:change_channel(1);
       true ->
	    do_nothing
    end,
    run(300, 1).

run(0, _Channel) ->
    ads1015_driver_app:stop(ok);
run(N, Channel) ->
    {ok, Value} = ads1015_driver:read_value_from_current_channel(),
    io:format("Current value on channel ~w: ~w~n", [Channel, Value]),
    timer:sleep(100),
    run(N-1, Channel).
