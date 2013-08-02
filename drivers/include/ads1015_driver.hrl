-define(CONVERSION_REGISTER, 0). %% see datasheet
-define(CONFIG_REGISTER, 1).     %% see datasheet
-define(BASE_VALUE, 2#00011).    %% default values for comparator config bits

-define(CHANNEL_BITS(Channel), 2#100+Channel). %% 0 <= Channel <= 3

-define(MAX_VOLTAGE_6V,   2#000). %% see datasheet for programmable gain
-define(MAX_VOLTAGE_4V,   2#001). %% amplifier configuration
-define(MAX_VOLTAGE_2V,   2#010).
-define(MAX_VOLTAGE_1V,   2#011).
-define(MAX_VOLTAGE_05V,  2#100).
-define(MAX_VOLTAGE_025V, 2#101).
-define(MAX_VOLTAGE_BITS(MaxVoltage), case MaxVoltage of
					  6.144 -> ?MAX_VOLTAGE_6V;
					  4.096 -> ?MAX_VOLTAGE_4V;
					  2.048 -> ?MAX_VOLTAGE_2V;
					  1.024 -> ?MAX_VOLTAGE_1V;
					  0.512 -> ?MAX_VOLTAGE_05V;
					  0.256 -> ?MAX_VOLTAGE_025V
				      end).

-define(OPERATING_MODE_BIT(OperatingMode), case OperatingMode of
					       continuous  -> 0;
					       single_shot -> 1
					   end).

-define(DATA_RATE_128,  2#000).  %% see datasheet for data rates
-define(DATA_RATE_250,  2#001).
-define(DATA_RATE_490,  2#010).
-define(DATA_RATE_920,  2#011).
-define(DATA_RATE_1600, 2#100).
-define(DATA_RATE_2400, 2#101).
-define(DATA_RATE_3300, 2#110).
-define(DATA_RATE_BITS(DataRate), case DataRate of
				      128  -> ?DATA_RATE_128;
				      250  -> ?DATA_RATE_250;
				      490  -> ?DATA_RATE_490;
				      920  -> ?DATA_RATE_920;
				      1600 -> ?DATA_RATE_1600;
				      2400 -> ?DATA_RATE_2400;
				      3300 -> ?DATA_RATE_3300
				  end).

-define(CONFIG_REGISTER_VALUE
	  (InputChannel, MaxVoltage, OperatingMode, DataRate),
	(?CHANNEL_BITS(InputChannel) bsl 12) bor
	    (?MAX_VOLTAGE_BITS(MaxVoltage) bsl 9) bor
	    (?OPERATING_MODE_BIT(OperatingMode) bsl 8) bor
	    (?DATA_RATE_BITS(DataRate) bsl 5) bor ?BASE_VALUE).
