-module(ads1015_driver_tests).

-include_lib("../include/ads1015_driver.hrl").
-include_lib("mockgyver/include/mockgyver.hrl").
-include_lib("eunit/include/eunit.hrl").

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
-define(CONVERSION_REGISTER_VALUE, -15).

expected_config_register_value() ->
    %% Use previously tested method to calculate
    ads1015_driver_lib:encode_config_register_value(?INPUT_CHANNEL,
			 ?MAX_VOLTAGE, ?OPERATING_MODE, ?DATA_RATE).

%% ---------------------------------------------------------------------
%% Define test setup and teardown for Mockgyver
%% ---------------------------------------------------------------------

ads1015_driver_test_() ->
    ?WITH_MOCKED_SETUP(fun setup/0, fun teardown/1).

setup() ->
    process_flag(trap_exit, true),
    %% Provide tests with fake application parameters
    mock_parameters(default, default, default, default, default),
    %% Mock I2C interface methods
    ?WHEN(i2c_interface:open_i2c_bus(_Address) -> {ok, ?HANDLE}),
    ?WHEN(i2c_interface:close_i2c_bus(_Address) -> ok),
    ?WHEN(i2c_interface:write_i2c_word(_Handle, _Register, _Value) -> ok),
    ?WHEN(i2c_interface:read_i2c_word(_Handle, Register) ->
		 case Register of
		     ?CONFIG_REGISTER     ->
			 {ok, expected_config_register_value() + ?IDLE};
		     ?CONVERSION_REGISTER ->
			 {ok, ?CONVERSION_REGISTER_VALUE}
		 end),
    ?WHEN(i2c_interface:read_i2c_signed_word(_Handle, _Register) ->
		 {ok, ?CONVERSION_REGISTER_VALUE}),
    %% Start driver
    {ok, Pid} = ads1015_driver:start_link(),
    Pid.

mock_parameters(I2CAddress0, InputChannel0, MaxVoltage0,
		OperatingMode0, DataRate0) ->
    I2CAddress = if (I2CAddress0 =:= default) -> ?I2C_ADDRESS;
		    true -> I2CAddress0
		 end,
    InputChannel = if (InputChannel0 =:= default) -> ?INPUT_CHANNEL;
		    true -> InputChannel0
		 end,
    MaxVoltage = if (MaxVoltage0 =:= default) -> ?MAX_VOLTAGE;
		    true -> MaxVoltage0
		 end,
    OperatingMode = if (OperatingMode0 =:= default) -> ?OPERATING_MODE;
		    true -> OperatingMode0
		 end,
    DataRate = if (DataRate0 =:= default) -> ?DATA_RATE;
		    true -> DataRate0
		 end,
    ?WHEN(application:get_env(_Application, Parameter) ->
		 case Parameter of
		     i2c_address    -> {ok, I2CAddress};
	             input_channel  -> {ok, InputChannel};
	             max_voltage    -> {ok, MaxVoltage};
	             operating_mode -> {ok, OperatingMode};
                     data_rate      -> {ok, DataRate}
		 end).

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
    ExpectedValue = expected_config_register_value(),
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

get_current_channel_should_return_initial_channel_test(_) ->
    {ok, ?INPUT_CHANNEL} = ads1015_driver:get_current_channel().

get_current_channel_should_return_new_channel_after_change_channel_test(_) ->
    NewInputChannel = ?INPUT_CHANNEL - 1,
    ok = ads1015_driver:change_channel(NewInputChannel),
    {ok, NewInputChannel} = ads1015_driver:get_current_channel().

change_channel_should_throw_exception_when_given_an_invalid_channel_test(_) ->
    ?assertThrow(badarg, ads1015_driver:change_channel(invalid_channel)),
    ?assertThrow(badarg, ads1015_driver:change_channel(127)),
    ?assertThrow(badarg, ads1015_driver:change_channel(-88)),
    ?assertThrow(badarg, ads1015_driver:change_channel(3.142)).

change_channel_should_set_new_channel_on_device_test(_) ->
    NewInputChannel = ?INPUT_CHANNEL + 1,
    ok = ads1015_driver:change_channel(NewInputChannel),
    ExpectedValue =
	ads1015_driver_lib:encode_config_register_value(NewInputChannel,
                      ?MAX_VOLTAGE, ?OPERATING_MODE, ?DATA_RATE),
    ?WAS_CALLED(i2c_interface:write_i2c_word(?HANDLE, ?CONFIG_REGISTER,
					     ExpectedValue)).

read_config_register_should_read_register_value_from_device_test(_) ->
    ads1015_driver:read_config_register(),
    ?WAS_CALLED(i2c_interface:read_i2c_word(?HANDLE, ?CONFIG_REGISTER)).

read_config_register_should_return_current_values_test(_) ->
    {ok, idle, ?INPUT_CHANNEL, ?MAX_VOLTAGE, ?OPERATING_MODE, ?DATA_RATE}
	= ads1015_driver:read_config_register().

set_config_register_should_set_config_register_on_device_test(_) ->
    Channel = ?INPUT_CHANNEL - 2,
    MaxVoltage = 2.048,
    OperatingMode = continuous,
    DataRate = 3300,
    ok = ads1015_driver:set_config_register(Channel, MaxVoltage,
					    OperatingMode, DataRate),
    ExpectedValue =
	ads1015_driver_lib:encode_config_register_value(Channel,
			     MaxVoltage, OperatingMode, DataRate),
    ?WAS_CALLED(i2c_interface:write_i2c_word(?HANDLE, ?CONFIG_REGISTER,
					     ExpectedValue)).

current_channel_should_be_updated_when_set_config_register_is_called_test(_) ->
    {ok, ?INPUT_CHANNEL} = ads1015_driver:get_current_channel(),
    NewChannel = ?INPUT_CHANNEL + 1,
    ok = ads1015_driver:set_config_register(NewChannel, ?MAX_VOLTAGE,
					    ?OPERATING_MODE, ?DATA_RATE),
    {ok, NewChannel} = ads1015_driver:get_current_channel().

set_config_register_should_throw_exception_when_parameter_invalid_test(_) ->
    ?assertThrow(badarg,
		 ads1015_driver:set_config_register(invalid_channel,
			     ?MAX_VOLTAGE, ?OPERATING_MODE, ?DATA_RATE)).

read_value_from_current_channel_should_return_correct_value_test(Pid) ->
    mock_parameters(default, default, default, continuous, default),
    restart_server(Pid),
    {ok, ?CONVERSION_REGISTER_VALUE} =
	ads1015_driver:read_value_from_current_channel(),
    ?WAS_CALLED(i2c_interface:read_i2c_signed_word(?HANDLE,
						   ?CONVERSION_REGISTER)).
