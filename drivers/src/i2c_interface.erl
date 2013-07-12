%%% An Erlang NIF that interfaces the I2C driver in ioctl.
%%% Copyright (C) 2013 Angela Johansson
%%%
%%% i2c_interface is free software:  you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% i2c_interface is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with this software.  If not, see <http://www.gnu.org/licenses/>.

%%% @doc
%%% An Erlang NIF that interfaces the I2C bus on the Raspberry Pi
%%%
%%% This interface uses the I2C ioctl driver which provides access to the
%%% I2C bus with a file descriptor. A precondition to using this software
%%% is a working I2C bus and an installed i2c-tools package.

-module(i2c_interface).

-export([open_i2c_bus/1, close_i2c_bus/1]).

-define(nif_stub,
        erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-on_load(on_load/0).

-type file_descriptor() :: integer().
-type i2c_address()     :: integer().
-type error_code()      :: atom().

on_load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    Filename = filename:join(PrivDir, ?MODULE),
    ok = erlang:load_nif(Filename, 0).

-spec(open_i2c_bus(i2c_address()) -> {ok, file_descriptor()} | {error, error_code()}).
%%% @doc This opens the I2C bus, connects to the device at the specified
%%%      address and returns a file handle to the open bus.
open_i2c_bus(I2CAddress) ->
    open_i2c_bus_nif(I2CAddress).

-spec(close_i2c_bus(file_descriptor()) -> ok| {error, error_code()}).
%%% @doc This closes the I2C bus for a given file handle.
close_i2c_bus(FileHandle) ->
    close_i2c_bus_nif(FileHandle).

%%%%%%%%%%%%%%%
%% Define stubs for NIF functions

open_i2c_bus_nif(_I2CAddress)  -> ?nif_stub.
close_i2c_bus_nif(_FileHandle) -> ?nif_stub.

%%
%%%%%%%%%%%%%%%
