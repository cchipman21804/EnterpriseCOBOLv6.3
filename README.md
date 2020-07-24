# EnterpriseCOBOLv6.3
Utilities written in Enterprise COBOL v6.3 for z/OS & Unix IBM Z mainframes:

Please feel free to send me (Cliff Chipman) any questions or comments
about this source code via the cobol-programming-course Slack channel:
https://openmainframeproject.slack.com/

AFFINITY.cbl
"Affinity Laws Calculator"
Energy calculation utility that calculates new flow, pressure, or horsepower 
from old values and new rpm

BIGNUM.cbl
    BigNum v1.0 - a game by Dan Sanderson (aka Doc Brown)       
    featured in JetBBS Casino Games c1996
                                                                
 Ported to Enterprise COBOL v6.3 for z/OS by Cliff Chipman      
                                                                
 BigNum is a fairly simple game.  You have five places to put   
 digits to construct a five digit number.  These five digits    
 are picked randomly (from 0 to 9), one by one.  As they are    
 picked, you choose where to put it.  Once you place the digit, 
 you can't move it.  The goal is to construct the largest       
 possible number with those digits.                             
                                                                
 The board looks like this:                                     
                                                                
        a   b   c   d   e                                       
      ---------------------                                     
      |   |   |   |   |   |                                     
      ---------------------                                     
                                                                
  To place a digit, simply press the letter.                    
                                                                
 If you get the largest number possible, you get back twice     
 your bet!  If you get the first digit the largest you get 25%  
 of your bet back.                                              

COMMBLDG.cbl
"Commercial Building Annual HVAC Energy Consumption Simulator"
This program uses KSBYTMY3 Typical Meteorological Year weather data to
simulate the hour-by-hour cooling load and electricity consumption of 
a pre-programmed commercial building.

ENTHALPY.cbl
"Enthalpy Calculator"      
Energy calculation utility that calculates enthalpy of ambient air from dry 
bulb temperature (in degF) and relative humidity (in%)

ESCAPE.cbl
ESCAPE is a beta version of a two-dimensional interactive game played on the
stdout console. It is based on the BASIC source code of "CHASE" on pages 76-77
of the Jan-Feb edition of Creative Computing magazine. It can be found on the
Internet Archive at the URL below:

https://archive.org/details/Creative_Computing_v02n01_Jan-Feb1976

The player is trapped in an enclosed labyrinth running from robotic pursuers.
Fortunately, the walls and obstacles are fatal to the robots. The object of the 
game is to position the	player where the robots will crash into the obstacles or 
into each other.  The walls and obstacles are not fatal to the player because
the computer does not allow the player to make a move that brings him into
contact with them.  The computer also will not allow the player to commit
suicide by moving onto a space already occupied by a robot.  The robots blindly
determine their direction of travel based solely on the relative direction of
the player. This causes the robots to eventually stumble into the walls and 
obstacles on the playing field. Five teleports are randomly scattered throughout
the playing field.  These devices will randomly transport whatever falls into
them to another random location on the playing field.

FAGP.cbl
"Find Annual Worth Given Present Value"
Time Value of Money calculation utility that calculates the amount of money
which someone can uniformly withdraw annually from an account from the
initial investment (Present Value), the annual interest rate, and the term (in 
years)

FFGP.cbl
"Find Future Value Given Present Value"
Time Value of Money calculation utility that calculates the Future Value of an 
investment from the Present Value, the annual interest rate, and the term (in 
years)

FLIPCOIN.cbl
based on 'RANDOM' - it flips a binary virtual coin and displays "HEADS" or 
"TAILS" on the stdout console based on the results

FPGA.cbl
"Find Present Value Given Annual Worth"
Time Value of Money calculation utility that calculates the Present Value of an
investment from the Annual Worth, the annual interest rate, and the term (in 
years)

FPGF.cbl
"Find Present Value Given Future Value"
Time Value of Money calculation utility that calculates the Present Value of an
investment from the Future Value, the annual interest rate, and the term (in 
years)

FUELSAVE.cbl
"Fuel Savings Calculator"
Energy calculation utility that calculates percentage fuel savings from the 
fuel efficiency of the old equipment and the fuel efficiency of the new
equipment

HEATINDX.cbl
"Heat Index Calculator"
Calculates the heat index from the outside air temperature and the relative
humidity

HELLO.cbl
the classic "Hello World!"

LOANPYMT.cbl
"Monthly Loan Payment Calculator"
Time Value of Money calculation utility that calculates the monthly payment
of a loan with monthly compounded interest, from the principal, the annual
interest rate, and the term (in years)

RANDOM.cbl
demonstrates the COBOL intrinsic functions - RANDOM, and CURRENT-DATE

SAVINGS.cbl
"Monthly Savings Account Calculator"
Time Value of Money calculation utility that calculates the future value of a
savings account balance that is compounded monthly from the initial 
investment (Present Value), the annual interest rate, and the term (in years).
It accepts terms less than one year by entering decimal years.

WINDCHIL.cbl
"Wind Chill Calculator"
Calculates the wind chill from the outside air temperature and the wind speed.
