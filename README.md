# EnterpriseCOBOLv6.3
Utilities written in Enterprise COBOL v6.3 for z/OS & Unix IBM Z mainframes:

Each CBL source code file is accompanied by an appropriate JCL file.  Some
programs require data files, which are located in the dat folder.

AFFINITY.cbl
"Affinity Laws Calculator"
Energy calculation utility that calculates new flow, pressure, or horsepower 
from old values and new rpm.  If old rpm equals new rpm, the new quantity
is calculated using the pulley ratio.
AFFINITY runs interactively in a Unix shell or from the TSO READY prompt.

BELTLEN.cbl
Calculates the length of a belt between two pulleys of known diameter and 
distance apart.

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
 of your bet back.  BIGNUM runs interactively in a Unix shell 
 or from the TSO READY prompt.

CAPREACT.cbl
Electronics utility that calculates the reactance of a capacitor
in an AC circuit.

COLLATZ.cbl
Calculates the Collatz Conjecture sequence for a particular integer
entered by the user

COMMBLDG.cbl
"Commercial Building Annual HVAC Energy Consumption Simulator"
This program uses KSBYTMY3 (for Typical Meteorological Year weather data) and
HUMRATIO (for specific humidity ratio data) to simulate the hour-by-hour cooling
load and electricity consumption of a pre-programmed commercial building.

Concentration?.cbl
An incomplete "work-in-progress" implementation of the children's game "Concentration"
also known as "Memory"

CNWYLIFE.cbl
"Conway's Game of Life"
This program uses CONWAYIN as the seed for subsequent generations of Conway's
Game of Life algorithm.  The current contents of CONWAYIN represent a "glider"
in the lower-right-hand corner of the grid.  The output shows the glider making
its way across the screen until it collides with the boundary and stabilizes into
a block at generation #140.

DCBNTEST.CBL
"Decimal Binary Test"
From page #10 of "More BASIC Computer Games - TRS-80 Edition"
written in 1980 by David H. Ahl.
This game tests your skills in binary-to-decimal and
decimal-to-binary conversion.  You are given twenty conversion
trials.  Numbers are chosen randomly and your score is printed
at the end.  The answer to any conversion you miss is displayed:
if the next conversion is presented, you may assume you got the
previous one correct.
There are several possible modifications for this program such
as timing the response, allowing the user to specify the number
range, checking for duplicate numbers, or extending it to other
bases.
This program was written by Ted Park of Pacific Union College.
It originally appeared in the Mar/Apr 1975 issue of Creative
Computing magazine.
Ported to COBOL by, Clifford A. Chipman 5/13/2022

DHWSAVE.cbl
"Domestic Hot Water Savings Calculator"
Energy savings utility that calculates the savings from various changes to
domestic hot water usage.

ELEVATOR.cbl
This simulates the floor display and the control panel within an elevator car.

ENTHALPY.cbl
"Enthalpy Calculator"      
Energy calculation utility that calculates enthalpy of ambient air from dry 
bulb temperature (in degF) and relative humidity (in%). ENTHALPY  runs 
interactively in a Unix shell or from the TSO READY prompt.

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
them to another random location on the playing field. ESCAPE runs interactively 
in a Unix shell or from the TSO READY prompt.

FAGP.cbl
"Find Annual Worth Given Present Value"
Time Value of Money calculation utility that calculates the amount of money
which someone can uniformly withdraw annually from an account from the
initial investment (Present Value), the annual interest rate, and the term (in 
years).  FAGP runs interactively in a Unix shell or from the TSO READY prompt.

FFGP.cbl
"Find Future Value Given Present Value"
Time Value of Money calculation utility that calculates the Future Value of an 
investment from the Present Value, the annual interest rate, and the term (in 
years).  FFGP runs interactively in a Unix shell or from the TSO READY prompt.

FIBOEXTD.cbl
"Calculate the Fibonacci Sequence out to 31 digits"

FIBONACI.cbl
"Calculate the Fibonacci Sequence out to 18 digits"

FIZZBUZZ.cbl
Classic FizzBuzz sequence

FLIPCOIN.cbl
based on 'RANDOM' - it flips a binary virtual coin and displays "HEADS" or 
"TAILS" on the stdout console based on the results. FLIPCOIN runs interactively 
in a Unix shell or from the TSO READY prompt.

FLTRFREQ.cbl
Searches a sequential file (FREQ1993) of FCC licensees from 1993 for records
containing a field matching a search string. The matching records are written to
another sequential file (FILTERED). The total number of matching records written 
are displayed in SYSOUT.

FPGA.cbl
"Find Present Value Given Annual Worth"
Time Value of Money calculation utility that calculates the Present Value of an
investment from the Annual Worth, the annual interest rate, and the term (in 
years). FPGA runs interactively in a Unix shell or from the TSO READY prompt.

FPGF.cbl
"Find Present Value Given Future Value"
Time Value of Money calculation utility that calculates the Present Value of an
investment from the Future Value, the annual interest rate, and the term (in 
years).  FPGF runs interactively in a Unix shell or from the TSO READY prompt.

FRSTFIRE.cbl
"Forest Fire algorithm"
This algorithm creates a random 80x40 'orchard'.  Empty cells grow new trees
at random.  Cells with trees, catch on fire at random.  Trees adjacent to
burning trees catch on fire and turn into empty cells.

FUELSAVE.cbl
"Fuel Savings Calculator"
Energy calculation utility that calculates percentage fuel savings from the 
fuel efficiency of the old equipment and the fuel efficiency of the new
equipment. FUELSAVE runs interactively in a Unix shell or from the TSO READY 
prompt.

HACKRANK.cbl (v1.0)
My first attempt at the Hacker News Rankings for Mainframe/COBOL Posts COBOL 
Challenge under "COBOL Programming Course - Advanced Topics"
It does not meet all of the challenge requirements, but I intend to return
to it at a later date armed with additional knowledge.

HEATINDX.cbl
"Heat Index Calculator"
Calculates the heat index from the outside air temperature and the relative
humidity.  HEATINDX runs interactively in a Unix shell or from the TSO READY 
prompt.

HELLO.cbl
the classic "Hello World!"  HELLO runs interactively in a Unix shell or from
the TSO READY prompt.

INDREACT.cbl
Electronics utility that calculates the reactance of an inductor
in an AC circuit.

LOANPYMT.cbl
"Monthly Loan Payment Calculator"
Time Value of Money calculation utility that calculates the monthly payment
of a loan with monthly compounded interest, from the principal, the annual
interest rate, and the term (in years).  LOANPYMT runs interactively in a 
Unix shell or from the TSO READY prompt.

LUHN.cbl
Checks the validity of a 16-digit credit card number

MIXEDAIR.cbl
Calculates mixed air characteristics in an air handler

OHMSLAW.cbl
Performs Ohm's Law calculations

PULLYDIS.cbl Calculates the distance between two pulleys of known diameter and belt length.

RANDOM.cbl
demonstrates the COBOL intrinsic functions - RANDOM, and CURRENT-DATE

RES2TEMP.cbl
This algorithm converts the sensor resistance of a 3Kohm @77 degF NTC thermistor to an accurate temperature. The following Python3 formula was derived from the sensor
specification sheet of a 3Kohm@77F NTC thermistor using an Excel spreadsheet of its resistance vs temperature table:
 yF = 8.5557106e-23 * math.pow(x,6) - 5.68981649695e-18 * math.pow(x,5) + 1.50180247230692e-13 * math.pow(x,4) - 2.01541522576944e-9 * math.pow(x,3) + 1.47906738776888e-5 * math.pow(x,2) - 6.2591776279401e-2 * x + 1.74508163989243e2

ROLL.cbl
rolls a virtual six-sided die. Evaluated paragraphs display each hard coded
die face in the PROCEDURE DIVISION based on the result of the cast.

ROLLDICE.cbl
rolls a virtual six-sided die. Uses 1D tables to define each die face in
the DATA DIVISION. Evaluated paragraphs display each table based on the
result of the cast.

SAVINGS.cbl
"Monthly Savings Account Calculator"
Time Value of Money calculation utility that calculates the future value of a
savings account balance that is compounded monthly from the initial 
investment (Present Value), the annual interest rate, and the term (in years).
It accepts terms less than one year by entering decimal years. SAVINGS runs 
interactively in a Unix shell or from the TSO READY prompt.

THRMST3K.cbl
This is a duplicate of RES2TEMP.cbl

TOPACCTS.cbl
This is my successful submission to the MTM2020 CBL exercise.  JCL is also found in the source code comments.

WINDCHIL.cbl
"Wind Chill Calculator"
Calculates the wind chill from the outside air temperature and the wind speed.
WINDCHIL runs interactively in a Unix shell or from the TSO READY prompt.
