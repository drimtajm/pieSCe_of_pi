-module(ads1015_driver_tests).
-include_lib("mockgyver/include/mockgyver.hrl").
-include_lib("../include/ads1015_driver.hrl").

%% ---------------------------------------------------------------------
%% Test constants
%% ---------------------------------------------------------------------

-define(I2C_ADDRESS, 78).
-define(HANDLE, 99).
-define(INPUT_CHANNEL, 2).
-define(INCORRECT_INPUT_CHANNEL, -1).
-define(MAX_VOLTAGE, 6.144).
-define(OPERATING_MODE, single_shot).
-define(DATA_RATE, 920).

%% ---------------------------------------------------------------------
%% Define test setup and teardown for Mockgyver
%% ---------------------------------------------------------------------

ads1015_driver_test_() ->
    ?WITH_MOCKED_SETUP(fun setup/0, fun teardown/1).

setup() ->
    process_flag(trap_exit, true),
    %% Provide tests with fake application parameters
    ?WHEN(application:get_env(_Application, Parameter) ->
		 case Parameter of
		     i2c_address    -> {ok, ?I2C_ADDRESS};
	             input_channel  -> {ok, ?INPUT_CHANNEL};
	             max_voltage    -> {ok, ?MAX_VOLTAGE};
	             operating_mode -> {ok, ?OPERATING_MODE};
                     data_rate      -> {ok, ?DATA_RATE}
		 end),
    %% Mock I2C interface methods
    ?WHEN(i2c_interface:open_i2c_bus(_Address) -> {ok, ?HANDLE}),
    ?WHEN(i2c_interface:close_i2c_bus(_Address) -> ok),
    ?WHEN(i2c_interface:write_i2c_word(_Handle, _Register, _Value) -> ok),
    %% Start driver
    {ok, Pid} = ads1015_driver:start_link(),
    Pid.

teardown(Pid) ->
    %% Stop driver and make sure it's stopped before test finishes
    ads1015_driver:stop(),
    wait_for_exit(Pid).

%% ---------------------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------------------

wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive
	{'DOWN', MRef, process, Pid, _Reason} ->
	    ok
    end.

restart_server(Pid) ->
    ads1015_driver:stop(),
    wait_for_exit(Pid),
    ads1015_driver:start_link().

%% ---------------------------------------------------------------------
%% Tests for driver startup and shutdown
%%
%% Note: start_link (which is called in setup) causes init to be called
%% ---------------------------------------------------------------------

init_should_open_i2c_bus_test(_) ->
    ?WAS_CALLED(i2c_interface:open_i2c_bus(?I2C_ADDRESS)).

init_should_set_config_register_test(_) ->
    %% Use previously tested method to calculate config register value
    ExpectedValue = ads1015_driver_lib:encode_config_register_value(
		      ?INPUT_CHANNEL, ?MAX_VOLTAGE, ?OPERATING_MODE,
		      ?DATA_RATE),
    ?WAS_CALLED(i2c_interface:write_i2c_word(?HANDLE, ?CONFIG_REGISTER,
					     ExpectedValue)).

init_should_stop_server_when_i2c_bus_unavailable_test(Pid) ->
    %% Overwrite previous mock
    ?WHEN(i2c_interface:open_i2c_bus(_Address) -> {error, unavailable}),
    %% Need to restart driver to cause init to be called again
    {error, {error, unavailable}} = restart_server(Pid),
    %% Wait for driver termination
    receive
	{'EXIT', _OtherPid, {error, unavailable}} ->
	    ok
    end.

terminate_should_close_i2c_bus_test(Pid) ->
    ads1015_driver:stop(),
    wait_for_exit(Pid),
    ?WAS_CALLED(i2c_interface:close_i2c_bus(?HANDLE)).

%% ---------------------------------------------------------------------
%% Tests for API methods (read conversion register, change channel asf)
%% ---------------------------------------------------------------------
