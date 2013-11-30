-module(driver_test2).
-compile([export_all]).

start() ->
    adc_breakout_driver_app:start(),
    run(300).

run(0) ->
    adc_breakout_driver_app:stop(ok);
run(N) ->
    {ok, Value} = adc_breakout_driver:read_value(),
    io:format("Current value: ~w~n", [Value]),
    timer:sleep(100),
    run(N-1).
