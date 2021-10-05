       identification division.
       program-id.    flipcoin.

       data division.
       working-storage section.
       01 random-num-in      pic 9v9(10).
      *01 random-num-out     pic 9.9(10).
       01 random-int         pic 9.

       01 coin-face          pic x(5).

      *01 pointer-string.
      *    02 filler         pic x(33) value spaces.
      *    02 point          pic xx value "^^".

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

       procedure division.
       title-screen.
           display spaces
           display "******* COIN FLIPPER DEMO BEGINS *******"
           display spaces
           display "written by, Cliff Chipman, EMIT"
           display "June 25, 2020"
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces.
      *    display "This program demonstrates the COBOL intristic "
      *             with no advancing
      *    display "functions RANDOM & CURRENT-DATE"
      *    display spaces
      *    display "The 1/100 second numerals from the CURRENT-DATE are"
      *             with no advancing
      *    display " used as a seed for the RANDOM function."

       flip-the-coin.
           move function current-date to datetime
           compute random-num-in = function random(hund-sec)
      *    move random-num-in to random-num-out
           multiply 10 by random-num-in
           compute random-int = function integer(random-num-in)
           if random-int is less than or equal 5
              move "HEADS" to coin-face
           else move "TAILS" to coin-face
           end-if.

       display-results.
           display spaces
      *    display "Current Date/Time: " datetime
      *    display pointer-string
      *    display "1/100 second: " hund-sec
      *    display "Random #: " random-num-out
      *    display "Integer: " random-int
           display coin-face
           display spaces
           display "******** COIN FLIPPER DEMO ENDS ********"
           display spaces
           stop run.
