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
But why waste concentration on that when the car can provide you with perfectly accurate speed measurements?<br><br>
If you are interested in reading more about my thoughts on this, continue reading on the bottom of the page.<br><br>
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

Continued discussion
--------------------

With the possibilities that modern technology offers, I believe it would be only natural to have distance measurements as well
as speed measurements.<br>
In fact, it is ridiculous that it's not standard equipment!
And I am <b>not</b> talking about connecting the system to the brakes.<br>
No, in my opinion, the driver him/herself is responsible for driving safely.<br>
But how do I know what's safe when I don't know my actual distance to the next car and my actual stopping distance?<br>
Why doesn't anybody post a law about the need of a distance meter, forbid tailgating
and enforce proper driving distances between cars?<br>
The police could measure that as easily as the speed and issue fines for violations.<br>
Isn't safety sufficiently important to us? Isn't saving our lives worth it?<br><br>
I read an article in the newspaper saying that tailgating costs many lives in car accidents.<br>
The question: "Why do you think people drive too close to the next car?" was answered by:
"I don't know. I guess most people are in a hurry."<br><br>
Do you want to know my answer to that question?<br>
Three words: Lack of awareness.<br>
Sadly, I am aware that I usually drive too close, even when I double the distance of the average driver in my home town.<br>
Problem is, I still don't know what's safe.<br>
Am I really driving too close?<br>
My eyes tell me I'm not. But that can be deceiving.<br>
And I know what most people think: "I can drive a little closer, there's plenty of room to stop
if the car in front suddenly hits the brakes." But the statistics tell us most people are mistaken.<br>
Anyway, long speech. My point is: Why rely on guessing when the car could measure the actual distance and
calculate the proper distance - for our safety's sake?
