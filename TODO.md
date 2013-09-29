Milestone 0: Make it work

- [x] Write NIF to interface I2C driver in ioctl from Erlang
- [ ] Fill Wiki with pictures and instructions/explanations
- [ ] Make design and put it on Wiki - design ready, not put on wiki yet
- [x] Fix Makefile and rebar config structure
- [x] Start using unit tests
- [x] Write driver for A/D converter
- [ ] Implement "speedometer"
- [ ] Test/calibrate speedometer and ensure its accuracy
- [ ] Calibrate distance sensor
- [ ] Implement "range finder"
- [ ] Implement "onboard computer" that initiates readings and evaluates the result
- [ ] Run tests both indoors and outdoors with varying speeds and distances

Milestone 1: Add support for "instrument panel"

- [ ] Implement LCD display driver that uses klajo/wpi
- [ ] Write display module that uses driver to display speed, distance, sample rate and "verdict"
- [ ] Design and implement connectivity
- [ ] Push measurements from server (onboard computer) to client (instrument panel)
- [ ] Test range and robustness of connected system

Milestone 2: Add bluetooth support

- [ ] Write NIF to interface Bluez library from Erlang
- [ ] Write bluetooth behaviour: gen_bluetooth
- [ ] Write client and server that use bluetooth "sockets" as provided by behaviour
- [ ] Add a way to start "automatically" on boot (rc.local?)
- [ ] Integrate with previous code/replace previous transport layer

Milestone 3: Wrap it up

- [ ] Make sure code is modular and reusable (especially drivers)
- [ ] Make "configure" that checks for needed packages
- [ ] Finish documentation on the Wiki with pictures
- [ ] Record a "demo test run"
- [ ] Finish presentation

Milestone 4: Add "extras"

- [ ] Add support for I2C high speed mode and possibly other settings
- [ ] Add support for devices that can't use SMBus
- [ ] Measure power consumption under varying conditions
- [ ] Replace A/D converter for speed measurement if not needed earlier
- [ ] Add support for WIDCOMM in bluetooth driver if possible
- [ ] Make it possible to display values "scaled up" to life-size cars
- [ ] Add button to switch scale on instrument panel
- [ ] Add buttons to trigger program start (onboard computer and instrument panel)
- [ ] Add leds to display program status/connection status
- [ ] Test different/several distance sensors, add "collision warning"
- [ ] Ensure code is platform independent
- [ ] Add GUI for speedometer and test instrument panel on PC
