-module(ads1015_driver_lib_tests).

-include_lib("../include/ads1015_driver.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

run_quickcheck_tests_test() ->
    ?assertMatch([], proper:module(?MODULE)).

%% ---------------------------------------------------------------------
%% Tests for channel functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_channel() ->
    ?FORALL(Channel, channel_value(),
	    Channel ==
		ads1015_driver_lib:decode_channel(
		  ads1015_driver_lib:encode_channel(Channel))).

prop_decodes_channel_bits_to_valid_channel_value() ->
    ?FORALL(BitPattern, channel_bits(),
	    begin
		Channel = ads1015_driver_lib:decode_channel(BitPattern),
		is_integer(Channel) and (Channel >= 0) and (Channel =< 3)
	    end).

channel_value() ->
    choose(0, 3).

channel_bits() ->
    oneof([?CHANNEL0, ?CHANNEL1, ?CHANNEL2, ?CHANNEL3]).

%% ---------------------------------------------------------------------
%% Tests for max voltage functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_max_voltage() ->
    ?FORALL(MaxVoltage, max_voltage_value(),
	    MaxVoltage ==
		ads1015_driver_lib:decode_max_voltage(
		  ads1015_driver_lib:encode_max_voltage(MaxVoltage))).

prop_decodes_max_voltage_bits_to_valid_max_voltage_value() ->
    ?FORALL(BitPattern, max_voltage_bits(),
	    begin
		MaxVoltage = ads1015_driver_lib:decode_max_voltage(BitPattern),
		is_float(MaxVoltage) and
		    lists:member(MaxVoltage,
				 [6.144, 4.096, 2.048, 1.024, 0.512, 0.256])
	    end).

max_voltage_value() ->
    oneof([6.144, 4.096, 2.048, 1.024, 0.512, 0.256]).

max_voltage_bits() ->
    oneof([?MAX_VOLTAGE_6V, ?MAX_VOLTAGE_4V, ?MAX_VOLTAGE_2V,
	   ?MAX_VOLTAGE_1V, ?MAX_VOLTAGE_05V, ?MAX_VOLTAGE_025V]).

%% ---------------------------------------------------------------------
%% Tests for operating mode functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_operating_mode() ->
    ?FORALL(OperatingMode, operating_mode_value(),
	    OperatingMode ==
		ads1015_driver_lib:decode_operating_mode(
		  ads1015_driver_lib:encode_operating_mode(OperatingMode))).

prop_decodes_operating_mode_bits_to_valid_operating_mode_value() ->
    ?FORALL(BitPattern, operating_mode_bits(),
	    begin
		OperatingMode =
		    ads1015_driver_lib:decode_operating_mode(BitPattern),
		is_atom(OperatingMode) and
					 ((OperatingMode == continuous) or
					  (OperatingMode == single_shot))
	    end).

operating_mode_value() ->
    oneof([continuous, single_shot]).

operating_mode_bits() ->
    oneof([?CONTINUOUS_MODE, ?SINGLE_SHOT_MODE]).

%% ---------------------------------------------------------------------
%% Tests for data rate functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_data_rate() ->
    ?FORALL(DataRate, data_rate_value(),
	    DataRate ==
		ads1015_driver_lib:decode_data_rate(
		  ads1015_driver_lib:encode_data_rate(DataRate))).

prop_decodes_data_rate_bits_to_valid_data_rate_value() ->
    ?FORALL(BitPattern, data_rate_bits(),
	    begin
		DataRate =
		    ads1015_driver_lib:decode_data_rate(BitPattern),
		is_integer(DataRate) and
		    lists:member(DataRate,
				 [128, 250, 490, 920, 1600, 2400, 3300])
	    end).

data_rate_value() ->
    oneof([128, 250, 490, 920, 1600, 2400, 3300]).

data_rate_bits() ->
    oneof([?DATA_RATE_128, ?DATA_RATE_250, ?DATA_RATE_490,
	   ?DATA_RATE_920, ?DATA_RATE_1600, ?DATA_RATE_2400,
	   ?DATA_RATE_3300]).

%% ---------------------------------------------------------------------
%% Tests for config register functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_config_register_value() ->
    ?FORALL({Channel, MaxVoltage, OperatingMode, DataRate},
	    {channel_value(), max_voltage_value(), operating_mode_value(),
	     data_rate_value()},
	    {ok, Channel, MaxVoltage, OperatingMode, DataRate} ==
		ads1015_driver_lib:decode_config_register_value(
		  ads1015_driver_lib:encode_config_register_value(
		    Channel, MaxVoltage, OperatingMode, DataRate))).

prop_decodes_config_register_value_to_valid_parameters() ->
    ?FORALL({ChannelBits, MaxVoltageBits,
	     OperatingModeBits, DataRateBits},
	    {channel_bits(), max_voltage_bits(),
	     operating_mode_bits(), data_rate_bits()},
	    begin
		BitPattern = ChannelBits + MaxVoltageBits
		    + OperatingModeBits + DataRateBits,
		{ok, Channel, MaxVoltage, OperatingMode, DataRate} =
		    ads1015_driver_lib:decode_config_register_value(BitPattern),

		is_integer(Channel) and (Channel >= 0) and (Channel =< 3) and
		    is_float(MaxVoltage) and
		    lists:member(MaxVoltage,
				 [6.144, 4.096, 2.048, 1.024, 0.512, 0.256]) and
		    is_atom(OperatingMode) and
		    lists:member(OperatingMode,
				 [continuous, single_shot]) and
		    is_integer(DataRate) and
		    lists:member(DataRate,
				 [128, 250, 490, 920, 1600, 2400, 3300])
	    end).
