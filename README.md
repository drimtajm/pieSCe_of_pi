pieSCe of Pi
=================================================================

Project "Safer Car" using a Raspberry Pi mounted on an RC car.
The purpose of the project is to provide a proof-of-concept of a car that warns when the distance to the next car is too short.

TODO: Write more

Getting started
---------------

Start by downloading and installing the i2c-tools and libi2c-dev packages.
Also, make sure you have downloaded [rebar][1] and added it to your PATH.

Then it should be possible to build the project:

    make
    
Run the unit tests:

    make test
    
Note that the unit tests do not need to be run on target, i.e. any Linux box without I2C bus will do.
    
[1]: https://github.com/basho/rebar/

