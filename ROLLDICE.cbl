      *****************************************************************
      * Program name:    ROLLDICE
      * Original author: CHIPMAN
      *
      * Maintenence Log
      * Date      Author        Maintenance Requirement
      * --------- ------------  ---------------------------------------
      * 08/28/20  CHIPMAN       Created for the halibut
      * 08/29/20  CHIPMAN       Uses 1-D tables to display the
      *                         die faces
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     ROLLDICE.
       AUTHOR.         CHIPMAN.
       INSTALLATION.   CASA CHIPMAN.
       DATE-WRITTEN.   8/29/2020.
       DATE-COMPILED.  8/29/2020.
       SECURITY.       NON-CONFIDENTIAL.
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 datetime.
           02 filler         pic 9(4).
           02 filler         pic 99.
           02 filler         pic 99.
           02 filler         pic 99.
           02 filler         pic 99.
           02 filler         pic 99.
           02 hund-sec       pic 99.
           02 filler         pic x.
           02 filler         pic 99.
           02 filler         pic 99.

       01 random-num-in      pic 9v99.
       01 cast               pic 9.
      *
      * rows designates how tall the ASCII art is on the stdout console
      * minimum value is 5
      * maximum value is 20
      * 01 rows               pic 99   value 5.
      * UNFORTUNATELY, THE COMPILER DOES NOT ALLOW THE "DEPENDING ON"
      * CLAUSE TO BE USED WITH A "FILLER REDEFINES" TABLE. THEREFORE,
      * EACH TABLE'S DIMENSION MUST BE DEFINED WITH A FIXED VALUE.
      *
      * counter selects each row in the designated ASCII art table to
      * be displayed in turn until the bottom row is displayed
      * last-row MUST ALWAYS equal rows + 1
       01 counter            pic 99.
           88 last-row                value 6.

      *****************************************************************
      * ASCII art dice:
      *
       01 die-face-tables.
           02 side-one.
              03 filler      pic x(9) value "#########".
              03 filler      pic x(9) value "#       #".
              03 filler      pic x(9) value "#   o   #".
              03 filler      pic x(9) value "#       #".
              03 filler      pic x(9) value "#########".
      *
      * Each table dimension must be equal to last-row - 1
           02 filler redefines side-one.
              03 die-side-one occurs 5 TIMES
                             pic x(9).

           02 side-two.
              03 filler      pic x(9) value "#########".
              03 filler      pic x(9) value "# o     #".
              03 filler      pic x(9) value "#       #".
              03 filler      pic x(9) value "#     o #".
              03 filler      pic x(9) value "#########".
      *
      * Each table dimension must be equal to last-row - 1
           02 filler redefines side-two.
              03 die-side-two occurs 5 TIMES
                             pic x(9).

           02 side-three.
              03 filler      pic x(9) value "#########".
              03 filler      pic x(9) value "# o     #".
              03 filler      pic x(9) value "#   o   #".
              03 filler      pic x(9) value "#     o #".
              03 filler      pic x(9) value "#########".
      *
      * Each table dimension must be equal to last-row - 1
           02 filler redefines side-three.
              03 die-side-three occurs 5 TIMES
                             pic x(9).

           02 side-four.
              03 filler      pic x(9) value "#########".
              03 filler      pic x(9) value "# o   o #".
              03 filler      pic x(9) value "#       #".
              03 filler      pic x(9) value "# o   o #".
              03 filler      pic x(9) value "#########".
      *
      * Each table dimension must be equal to last-row - 1
           02 filler redefines side-four.
              03 die-side-four occurs 5 TIMES
                             pic x(9).

           02 side-five.
              03 filler      pic x(9) value "#########".
              03 filler      pic x(9) value "# o   o #".
              03 filler      pic x(9) value "#   o   #".
              03 filler      pic x(9) value "# o   o #".
              03 filler      pic x(9) value "#########".
      *
      * Each table dimension must be equal to last-row - 1
           02 filler redefines side-five.
              03 die-side-five occurs 5 TIMES
                             pic x(9).

           02 side-six.
              03 filler      pic x(9) value "#########".
              03 filler      pic x(9) value "# o   o #".
              03 filler      pic x(9) value "# o   o #".
              03 filler      pic x(9) value "# o   o #".
              03 filler      pic x(9) value "#########".
      *
      * Each table dimension must be equal to last-row - 1
           02 filler redefines side-six.
              03 die-side-six occurs 5 TIMES
                             pic x(9).

       PROCEDURE DIVISION.
       100-main-para.
           perform 110-title-screen thru 130-display-die.

       999-end-pgm.
           display spaces
           display "****** ROLL A DIE ENDS ******"
           stop run.

       110-title-screen.
           display spaces
           display "****** ROLL A DIE BEGINS ******"
           display spaces
           display "written by, Cliff Chipman, EMIT"
           display "August 29, 2020"
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces.

       120-roll-the-die.
           move function current-date to datetime
           compute random-num-in = function random(hund-sec)
           multiply 6 by random-num-in giving cast
           add 1 to cast.

       130-display-die.
           perform varying counter from 1 by 1 until last-row
                 evaluate cast
                    when 1
                         display die-side-one(counter)
                    when 2
                         display die-side-two(counter)
                    when 3
                         display die-side-three(counter)
                    when 4
                         display die-side-four(counter)
                    when 5
                         display die-side-five(counter)
                    when 6
                         display die-side-six(counter)
                    when other
                       display "Oops! This wasn't supposed to happen!"
                 end-evaluate
                 end-perform.
