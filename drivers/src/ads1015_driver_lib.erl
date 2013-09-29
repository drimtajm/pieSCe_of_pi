%%%-------------------------------------------------------------------
%%% @author  drimtajm@github
%%% @copyright (C) 2013, Angela Johansson
%%% @doc Library module for the ADS1015 A/D converter driver
%%%
%%% This library module facilitates coding and decoding of configuration
%%% register values.
%%%
%%% The ADS1015 A/D converter driver is free software: you can redistribute
%%% it and/or modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation, either version 3
%%% of the License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with this software.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%% @end
%%% Created : 11 Aug 2013 by drimtajm
%%%-------------------------------------------------------------------
-module(ads1015_driver_lib).

-include("../include/ads1015_driver.hrl").

-export([decode_status/1, set_status_bit/1]).
-export([encode_channel/1, decode_channel/1]).
-export([encode_max_voltage/1, decode_max_voltage/1]).
-export([encode_operating_mode/1, decode_operating_mode/1]).
-export([encode_data_rate/1, decode_data_rate/1]).
-export([encode_config_register_value/4, decode_config_register_value/1]).

%% ---------------------------------------------------------------------
%% Operational status functionality - decode -> read,
%%                                    set -> start single conversion
%% ---------------------------------------------------------------------

-spec(decode_status(word()) -> status()).
decode_status(BitPattern) when is_integer(BitPattern),
				BitPattern >= 0, BitPattern =< ?MAX_WORD ->
    case BitPattern band ?STATUS_BIT of
	?IDLE -> idle;
	?BUSY -> busy
    end;
decode_status(_BitPattern) ->
    throw(badarg).

-spec(set_status_bit(word()) -> word()).
set_status_bit(BitPattern) when is_integer(BitPattern),
				BitPattern >= 0, BitPattern =< ?MAX_WORD ->
    BitPattern bor ?STATUS_BIT;
set_status_bit(_BitPattern) ->
    throw(badarg).

%% ---------------------------------------------------------------------
%% Channel functionality
%% ---------------------------------------------------------------------

%% Not possible to choose values for the channel where the comparator
%% would be used, since this driver only supports single-ended mode
-spec(encode_channel(channel()) -> word()).
encode_channel(Channel) when is_integer(Channel),
			     Channel >= 0, Channel =< 3 ->
    case Channel of
	0 -> ?CHANNEL0;
	1 -> ?CHANNEL1;
	2 -> ?CHANNEL2;
	3 -> ?CHANNEL3
    end;
encode_channel(_Channel) ->
    throw(badarg).

-spec(decode_channel(word()) -> channel() | comparator_mode).
decode_channel(BitPattern) when is_integer(BitPattern),
				BitPattern >= 0, BitPattern =< ?MAX_WORD ->
    case BitPattern band ?CHANNEL_BITS of
	?CHANNEL0 -> 0;
	?CHANNEL1 -> 1;
	?CHANNEL2 -> 2;
	?CHANNEL3 -> 3;
	_Else     -> comparator_mode
    end;
decode_channel(_BitPattern) ->
    throw(badarg).

%% ---------------------------------------------------------------------
%% Max voltage functionality
%% ---------------------------------------------------------------------

-spec(encode_max_voltage(max_voltage()) -> word()).
encode_max_voltage(MaxVoltage) when is_float(MaxVoltage) ->
    case MaxVoltage of
	6.144 -> ?MAX_VOLTAGE_6V;
	4.096 -> ?MAX_VOLTAGE_4V;
	2.048 -> ?MAX_VOLTAGE_2V;
	1.024 -> ?MAX_VOLTAGE_1V;
	0.512 -> ?MAX_VOLTAGE_05V;
	0.256 -> ?MAX_VOLTAGE_025V;
	_Else -> throw({badarg, bad_max_voltage})
    end;
encode_max_voltage(_MaxVoltage) ->
    throw(badarg).

-spec(decode_max_voltage(word()) -> max_voltage()).
decode_max_voltage(BitPattern) when is_integer(BitPattern),
				BitPattern >= 0, BitPattern =< ?MAX_WORD ->
    case BitPattern band ?MAX_VOLTAGE_BITS of
	?MAX_VOLTAGE_6V   -> 6.144;
	?MAX_VOLTAGE_4V   -> 4.096;
	?MAX_VOLTAGE_2V   -> 2.048;
	?MAX_VOLTAGE_1V   -> 1.024;
	?MAX_VOLTAGE_05V  -> 0.512;
	_Else             -> 0.256
    end;
decode_max_voltage(_BitPattern) ->
    throw(badarg).

%% ---------------------------------------------------------------------
%% Operating mode functionality
%% ---------------------------------------------------------------------

-spec(encode_operating_mode(operating_mode()) -> word()).
encode_operating_mode(continuous = _OperatingMode) ->
    ?CONTINUOUS_MODE;
encode_operating_mode(single_shot = _OperatingMode) ->
    ?SINGLE_SHOT_MODE;
encode_operating_mode(_OperatingMode) ->
    throw(badarg).

-spec(decode_operating_mode(word()) -> operating_mode()).
decode_operating_mode(BitPattern) when is_integer(BitPattern),
				BitPattern >= 0, BitPattern =< ?MAX_WORD ->
    case BitPattern band ?OPERATING_MODE_BIT of
	?CONTINUOUS_MODE  -> continuous;
	?SINGLE_SHOT_MODE -> single_shot
    end;
decode_operating_mode(_OperatingMode) ->
    throw(badarg).

%% ---------------------------------------------------------------------
%% Data rate functionality
%% ---------------------------------------------------------------------

-spec(encode_data_rate(data_rate()) -> word()).
encode_data_rate(DataRate) when is_integer(DataRate) ->
    case DataRate of
	128   -> ?DATA_RATE_128;
	250   -> ?DATA_RATE_250;
	490   -> ?DATA_RATE_490;
	920   -> ?DATA_RATE_920;
	1600  -> ?DATA_RATE_1600;
	2400  -> ?DATA_RATE_2400;
	3300  -> ?DATA_RATE_3300;
	_Else -> throw({badarg, bad_data_rate})
    end;
encode_data_rate(_DataRate) ->
    throw(badarg).

-spec(decode_data_rate(word()) -> data_rate()).
decode_data_rate(BitPattern) when is_integer(BitPattern),
				BitPattern >= 0, BitPattern =< ?MAX_WORD ->
    case BitPattern band ?DATA_RATE_BITS of
	?DATA_RATE_128    -> 128;
	?DATA_RATE_250    -> 250;
	?DATA_RATE_490    -> 490;
	?DATA_RATE_920    -> 920;
	?DATA_RATE_1600   -> 1600;
	?DATA_RATE_2400   -> 2400;
	?DATA_RATE_3300   -> 3300;
	?DATA_RATE_3300_2 -> 3300
    end;
decode_data_rate(_BitPattern) ->
    throw(badarg).

%% ---------------------------------------------------------------------
%% Config register functionality
%% ---------------------------------------------------------------------

-spec(encode_config_register_value(channel(), max_voltage(),
				   operating_mode(), data_rate()) ->
	     word()).
encode_config_register_value(Channel, MaxVoltage, OperatingMode, DataRate) ->
    encode_channel(Channel) bor
	encode_max_voltage(MaxVoltage) bor
	encode_operating_mode(OperatingMode) bor
	encode_data_rate(DataRate) bor
	?BASE_VALUE.

-spec(decode_config_register_value(word()) ->
	     {ok, status(), channel(), max_voltage(),
	          operating_mode(), data_rate()}).
decode_config_register_value(Value) ->
    {ok, decode_status(Value), decode_channel(Value),
     decode_max_voltage(Value), decode_operating_mode(Value),
     decode_data_rate(Value)}.
