      *****************************************************************
      * Program name:    ROLL
      * Original author: CHIPMAN
      *
      * Maintenence Log
      * Date      Author        Maintenance Requirement
      * --------- ------------  ---------------------------------------
      * 08/28/20  CHIPMAN       Created for the halibut
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     ROLL.
       AUTHOR.         CHIPMAN.
       INSTALLATION.   CASA CHIPMAN.
       DATE-WRITTEN.   8/28/2020.
       DATE-COMPILED.  8/28/2020.
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
           display "August 28, 2020"
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces.

       120-roll-the-die.
           move function current-date to datetime
           compute random-num-in = function random(hund-sec)
           multiply 6 by random-num-in giving cast
           add 1 to cast.

       130-display-die.
           evaluate cast
              when 1
                 perform one
              when 2
                 perform two
              when 3
                 perform three
              when 4
                 perform four
              when 5
                 perform five
              when 6
                 perform six
              when other
                 display "Oops! This wasn't supposed to happen!"
           end-evaluate.
      *****************************************************************
      * ASCII art dice:
      *
       one.
           display "#########"
           display "#       #"
           display "#   o   #"
           display "#       #"
           display "#########".


       two.
           display "#########"
           display "# o     #"
           display "#       #"
           display "#     o #"
           display "#########".


       three.
           display "#########"
           display "# o     #"
           display "#   o   #"
           display "#     o #"
           display "#########".


       four.
           display "#########"
           display "# o   o #"
           display "#       #"
           display "# o   o #"
           display "#########".


       five.
           display "#########"
           display "# o   o #"
           display "#   o   #"
           display "# o   o #"
           display "#########".


       six.
           display "#########"
           display "# o   o #"
           display "# o   o #"
           display "# o   o #"
           display "#########".

