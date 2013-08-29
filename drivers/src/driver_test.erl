-module(driver_test).
-compile([export_all]).

start() ->
    ads1015_driver_app:start(),
    run(100, ads1015_driver:get_current_channel()).

run(0, _Channel) ->
    ads1015_driver_app:stop();
run(N, Channel) ->
    {ok, Value} = ads1015_driver:read_value_from_current_channel(),
    io:format("Current value on channel ~w: ~w~n", [Channel, Value]),
    run(N-1, Channel).
