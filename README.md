pieSCe of Pi
=================================================================

Project "Safer Car" using a Raspberry Pi mounted on an RC car.<br>
The purpose of the project is to provide a proof-of-concept of a car that warns when the driving distance to the next car is too short.

<img src="https://raw.github.com/drimtajm/pieSCe_of_pi/master/resources/images/poP1.jpg" alt="poP car" height="414" width="400">
<img src="https://raw.github.com/drimtajm/pieSCe_of_pi/master/resources/images/poP2.jpg" alt="poP car with descriptions" height="450" width="450">

In my opinion, every real life car should have a distance sensor that continuously measures the distance to the car in front.<br>
Why?<br>
It's simple: Because I can't measure the distance with my eyes!<br>
But seriously, would you drive a car without speedometer?<br>
You could theoretically calculate the speed yourself by looking at the environment flashing by and counting the seconds.<br>
But why waste concentration on that when the car can provide you with perfectly accurate speed measurements?<br>
IMHO, the same reasoning applies to distances. I simply want a car that measures for me.<br><br>
More information about the car, its design and construction can be found on the wiki for this project.<br>
Simply click the book icon on the righthand side. Enjoy!

Getting started
---------------

Start by building the car.<br>
Then, download and install the i2c-tools and libi2c-dev packages on your Raspberry Pi.<br>
Also, make sure you have downloaded [rebar][1] and added it to your PATH.

Then it should be possible to build the project:

    make
    
Run the unit tests:

    make test
    
Note that the unit tests do not need to be run on target, i.e. any Linux box without I2C bus (and without I2C software) will do.
    
[1]: https://github.com/basho/rebar/
