-ifndef(ADS1015_DRIVER_HRL).
-define(ADS1015_DRIVER_HRL, true).

-define(CONVERSION_REGISTER, 0). %% see datasheet
-define(CONFIG_REGISTER, 1).     %% see datasheet
-define(BASE_VALUE, 2#00011).    %% default values for comparator config bits

-define(MAX_WORD, 65535).

-type word() :: 0..?MAX_WORD.

%% ---------------------------------------------------------------------
%% Operational status functionality
%% ---------------------------------------------------------------------

-type status() :: idle | busy.

-define(IDLE,       2#1000000000000000).
-define(BUSY,       2#0000000000000000).
-define(STATUS_BIT, 2#1000000000000000).

%% ---------------------------------------------------------------------
%% Channel functionality
%% ---------------------------------------------------------------------

-type channel() :: 0..3.

-define(CHANNEL0,     2#0100000000000000).
-define(CHANNEL1,     2#0101000000000000).
-define(CHANNEL2,     2#0110000000000000).
-define(CHANNEL3,     2#0111000000000000).
-define(CHANNEL_BITS, 2#0111000000000000).

%% ---------------------------------------------------------------------
%% Max voltage functionality
%% ---------------------------------------------------------------------

-type max_voltage() :: float().

%% see datasheet for programmable gain amplifier configuration
-define(MAX_VOLTAGE_6V,     2#0000000000000000).
-define(MAX_VOLTAGE_4V,     2#0000001000000000).
-define(MAX_VOLTAGE_2V,     2#0000010000000000).
-define(MAX_VOLTAGE_1V,     2#0000011000000000).
-define(MAX_VOLTAGE_05V,    2#0000100000000000).
-define(MAX_VOLTAGE_025V,   2#0000101000000000).
-define(MAX_VOLTAGE_025V_2, 2#0000110000000000).
-define(MAX_VOLTAGE_025V_3, 2#0000111000000000).
-define(MAX_VOLTAGE_BITS,   2#0000111000000000).

%% ---------------------------------------------------------------------
%% Operating mode functionality
%% ---------------------------------------------------------------------

-type operating_mode() :: continuous | single_shot.

-define(CONTINUOUS_MODE,    2#0000000000000000).
-define(SINGLE_SHOT_MODE,   2#0000000100000000).
-define(OPERATING_MODE_BIT, 2#0000000100000000).

%% ---------------------------------------------------------------------
%% Data rate functionality
%% ---------------------------------------------------------------------

-type data_rate() :: 128 | 250 | 490 | 920 | 1600 | 2400 | 3300.

%% see datasheet for data rates
-define(DATA_RATE_128,    2#0000000000000000).
-define(DATA_RATE_250,    2#0000000000100000).
-define(DATA_RATE_490,    2#0000000001000000).
-define(DATA_RATE_920,    2#0000000001100000).
-define(DATA_RATE_1600,   2#0000000010000000).
-define(DATA_RATE_2400,   2#0000000010100000).
-define(DATA_RATE_3300,   2#0000000011000000).
-define(DATA_RATE_3300_2, 2#0000000011100000).
-define(DATA_RATE_BITS,   2#0000000011100000).

-endif.
