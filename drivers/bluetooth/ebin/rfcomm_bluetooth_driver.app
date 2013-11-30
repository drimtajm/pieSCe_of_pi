{application,rfcomm_bluetooth_driver,
             [{description,"Bluetooth driver for the RFCOMM protocol"},
              {vsn,"1"},
              {modules,[bluetooth_client_test,bluetooth_interface,
                        bluetooth_server_test]},
              {registered,[rfcomm_driver]},
              {applications,[kernel,stdlib]},
              {mod,{rfcomm_driver_app,[]}},
              {env,[{default_channel,13}]}]}.
