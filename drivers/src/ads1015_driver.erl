%%%-------------------------------------------------------------------
%%% @author  drimtajm@github
%%% @copyright (C) 2013, Angela Johansson
%%% @doc Implementation module for the ADS1015 A/D converter driver
%%%
%%% Note that the driver only supports single-ended inputs.
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
%%% Created : 30 Jul 2013 by drimtajm
%%%-------------------------------------------------------------------
-module(ads1015_driver).

-behaviour(gen_server).

-include("../include/ads1015_driver.hrl").

%% API
-export([start_link/0,
	 get_current_channel/0, change_channel/1, read_value_from_current_channel/0,
	 read_config_register/0,
	 stop/0, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(APPLICATION, ad_converter_driver_ads1015).

-record(state, {i2c_interface_handle :: non_neg_integer(),
		input_channel :: non_neg_integer(),
		max_voltage :: number(),
		operating_mode :: continuous | single_shot,
		data_rate :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_current_channel() ->
    gen_server:call(?SERVER, get_current_channel, 1000).

change_channel(Channel) when Channel >= 0, Channel =< 3 ->
    %% only four channels available
    gen_server:call(?SERVER, {change_channel, Channel}, 10000);
change_channel(_Channel) ->
    throw({badarg, out_of_range}).

read_value_from_current_channel() ->
    gen_server:call(?SERVER, read_value, 1000).

read_config_register() ->
    gen_server:call(?SERVER, read_config_register, 1000).

stop() ->
    stop(normal).

stop(Reason) ->
    gen_server:cast(?SERVER, {stop, Reason}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    I2C_address = get_default_value_for(i2c_address),
    case i2c_interface:open_i2c_bus(I2C_address) of
	{ok, Handle} ->
	    InitState =
		#state{i2c_interface_handle = Handle,
		       input_channel  = get_default_value_for(input_channel),
		       max_voltage    = get_default_value_for(max_voltage),
		       operating_mode = get_default_value_for(operating_mode),
		       data_rate      = get_default_value_for(data_rate)},
	    set_config_register(InitState),
	    {ok, InitState};
	Error        -> {stop, Error}
    end.

get_default_value_for(Parameter) ->
    {ok, Value} = application:get_env(?APPLICATION, Parameter),
    Value.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    io:format("Stop due to: ~p~n", [Reason]),
    {stop, Reason, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{i2c_interface_handle = Handle}) ->
    ok = i2c_interface:close_i2c_bus(Handle).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_config_register(#state{i2c_interface_handle = Handle,
			   input_channel = InputChannel,
			   max_voltage = MaxVoltage,
			   operating_mode = OperatingMode,
			   data_rate = DataRate}) ->
    Value = ?CONFIG_REGISTER_VALUE(InputChannel, MaxVoltage,
				   OperatingMode, DataRate),
    i2c_interface:write_i2c_word(Handle, ?CONFIG_REGISTER, Value).
