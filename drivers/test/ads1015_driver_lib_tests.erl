-module(ads1015_driver_lib_tests).

-include_lib("../include/ads1015_driver.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

run_quickcheck_tests_test() ->
    ?assertMatch([], proper:module(?MODULE)).

word_value() ->
    choose(0, ?MAX_WORD).

%% ---------------------------------------------------------------------
%% Tests for operational status functionality
%% ---------------------------------------------------------------------

prop_set_status_bit_always_sets_status_bit() ->
    ?FORALL(BitPattern, word_value(),
	    begin
		NewBitPattern = ads1015_driver_lib:set_status_bit(BitPattern),
		is_integer(NewBitPattern)
		    and ((NewBitPattern band ?STATUS_BIT) > 0)
	    end).

prop_decodes_status_bit_to_valid_status_value() ->
    ?FORALL(BitPattern, word_value(),
	    begin
		Status = ads1015_driver_lib:decode_status(BitPattern),
		is_atom(Status) and lists:member(Status, [idle, busy])
	    end).

%% ---------------------------------------------------------------------
%% Tests for channel functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_channel() ->
    ?FORALL(Channel, channel_value(),
	    Channel ==
		ads1015_driver_lib:decode_channel(
		  ads1015_driver_lib:encode_channel(Channel))).

prop_decodes_channel_bits_to_valid_channel_value() ->
    ?FORALL(BitPattern, word_value(),
	    begin
		Channel = ads1015_driver_lib:decode_channel(BitPattern),
		(is_integer(Channel) and (Channel >= 0) and (Channel =< 3))
		    or (Channel == comparator_mode)
	    end).

channel_value() ->
    choose(0, 3).

%% ---------------------------------------------------------------------
%% Tests for max voltage functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_max_voltage() ->
    ?FORALL(MaxVoltage, max_voltage_value(),
	    MaxVoltage ==
		ads1015_driver_lib:decode_max_voltage(
		  ads1015_driver_lib:encode_max_voltage(MaxVoltage))).

prop_decodes_max_voltage_bits_to_valid_max_voltage_value() ->
    ?FORALL(BitPattern, word_value(),
	    begin
		MaxVoltage = ads1015_driver_lib:decode_max_voltage(BitPattern),
		is_float(MaxVoltage) and
		    lists:member(MaxVoltage,
				 [6.144, 4.096, 2.048, 1.024, 0.512, 0.256])
	    end).

max_voltage_value() ->
    oneof([6.144, 4.096, 2.048, 1.024, 0.512, 0.256]).

%% ---------------------------------------------------------------------
%% Tests for operating mode functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_operating_mode() ->
    ?FORALL(OperatingMode, operating_mode_value(),
	    OperatingMode ==
		ads1015_driver_lib:decode_operating_mode(
		  ads1015_driver_lib:encode_operating_mode(OperatingMode))).

prop_decodes_operating_mode_bits_to_valid_operating_mode_value() ->
    ?FORALL(BitPattern, word_value(),
	    begin
		OperatingMode =
		    ads1015_driver_lib:decode_operating_mode(BitPattern),
		is_atom(OperatingMode) and
					 ((OperatingMode == continuous) or
					  (OperatingMode == single_shot))
	    end).

operating_mode_value() ->
    oneof([continuous, single_shot]).

%% ---------------------------------------------------------------------
%% Tests for data rate functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_data_rate() ->
    ?FORALL(DataRate, data_rate_value(),
	    DataRate ==
		ads1015_driver_lib:decode_data_rate(
		  ads1015_driver_lib:encode_data_rate(DataRate))).

prop_decodes_data_rate_bits_to_valid_data_rate_value() ->
    ?FORALL(BitPattern, word_value(),
	    begin
		DataRate =
		    ads1015_driver_lib:decode_data_rate(BitPattern),
		is_integer(DataRate) and
		    lists:member(DataRate,
				 [128, 250, 490, 920, 1600, 2400, 3300])
	    end).

data_rate_value() ->
    oneof([128, 250, 490, 920, 1600, 2400, 3300]).

%% ---------------------------------------------------------------------
%% Tests for config register functionality
%% ---------------------------------------------------------------------

prop_decodes_encoded_config_register_value() ->
    ?FORALL({Channel, MaxVoltage, OperatingMode, DataRate},
	    {channel_value(), max_voltage_value(), operating_mode_value(),
	     data_rate_value()},
	    {ok, busy, Channel, MaxVoltage, OperatingMode, DataRate} ==
		ads1015_driver_lib:decode_config_register_value(
		  ads1015_driver_lib:encode_config_register_value(
		    Channel, MaxVoltage, OperatingMode, DataRate))).

prop_decodes_config_register_value_to_valid_parameters() ->
    ?FORALL(
       BitPattern, word_value(),
       begin
	   {ok, Status, Channel, MaxVoltage, OperatingMode, DataRate} =
	       ads1015_driver_lib:decode_config_register_value(BitPattern),

	   ((is_integer(Channel) and (Channel >= 0) and (Channel =< 3))
	    or (Channel == comparator_mode)) and
	       is_float(MaxVoltage) and
	       lists:member(MaxVoltage,
			    [6.144, 4.096, 2.048, 1.024, 0.512, 0.256]) and
	       is_atom(OperatingMode) and
	       lists:member(OperatingMode,
			    [continuous, single_shot]) and
	       is_integer(DataRate) and
	       lists:member(DataRate,
			    [128, 250, 490, 920, 1600, 2400, 3300]) and
	       is_atom(Status) and
	       lists:member(Status, [busy, idle])
       end).

prop_encodes_expected_config_register_values() ->
    ?FORALL({Channel, MaxVoltage, OperatingMode, DataRate, ExpectedValue},
	    example_values_for_encoding(),
	    ExpectedValue ==
		ads1015_driver_lib:encode_config_register_value(
		    Channel, MaxVoltage, OperatingMode, DataRate)).

prop_decodes_config_register_values_to_correct_values() ->
    ?FORALL({RegisterValue, ExpectedStatus, ExpectedChannel,
	     ExpectedMaxVoltage, ExpectedOperatingMode, ExpectedDataRate},
	    example_values_for_decoding(),
	    {ok, ExpectedStatus, ExpectedChannel, ExpectedMaxVoltage,
	     ExpectedOperatingMode, ExpectedDataRate} ==
		ads1015_driver_lib:decode_config_register_value(
		  RegisterValue)).

example_values_for_encoding() ->
    oneof([{1, 6.144, continuous,  3300, 16#50c3},
	   {0, 0.256, single_shot, 1600, 16#4b83},
	   {2, 2.048, single_shot,  128, 16#6503},
	   {3, 0.512, continuous,   920, 16#7863}]).

example_values_for_decoding() ->
    oneof([{16#50c3, busy, 1, 6.144, continuous,  3300},
	   {16#d0c3, idle, 1, 6.144, continuous,  3300},
	   {16#4b83, busy, 0, 0.256, single_shot, 1600},
	   {16#6503, busy, 2, 2.048, single_shot,  128},
	   {16#f863, idle, 3, 0.512, continuous,   920},
	   %% The following line represents the default values
	   %% according to the datasheet
	   {16#8583, idle, comparator_mode, 2.048, single_shot, 1600},
	   %%
	   {16#a083, idle, comparator_mode, 6.144, continuous, 1600},
	   {16#bfe3, idle, comparator_mode, 0.256, single_shot, 3300}]).
