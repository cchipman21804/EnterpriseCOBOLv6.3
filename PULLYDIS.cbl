       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PULLEY-DISTANCE.
       AUTHOR.        CHIPMAN.
      *
      *//* COMPILE COBOL SOURCE CODE AND LINK OBJECT
      *//PULLYDIS JOB 1,NOTIFY=&SYSUID
      *//* COMPILE & LINK ONLY
      *// SET COBPGM='PULLYDIS'
      *//***************************************************/
      *//COBRUN  EXEC IGYWCL
      *//COBOL.SYSIN  DD DSN=&SYSUID..CBL(&COBPGM),DISP=SHR
      *//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
      *//***************************************************/
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * Data-entry-fields.
       01 pulley-diameter1-in   pic x(8).
       01 pulley-diameter2-in   pic x(8).
       01 belt-length-in        pic x(8).

      * Calculated-fields.
       01 pulley-diameter1      pic 9(5)v9(5).
       01 pulley-diameter2      pic 9(5)v9(5).
       01 pulley-distance       pic 9(5)v9(5).
       01 belt-length           pic 9(5)v9(5).
       01 pulley-diameter-sum   pic s9(6)v9(5) sign is leading separate.
       01 pulley-diameter-diff  pic s9(5)v9(5) sign is leading separate.
       01 x                     pic s9(6)v9(5) sign is leading separate.
       01 numerator             pic s9(6)v9(5) sign is leading separate.
       01 denominator           pic 99 value 16.

      * Displayed-fields.
       01 pulley-distance-out   pic zzzz9.99.

       01 pulley-diameter-sum-out   pic zzzzz9.99999.
       01 pulley-diameter-diff-out  pic zzzzz9.99999.
       01 x-out                 pic zzzzz9.99999.
       01 numerator-out         pic zzzzz9.99999.
       01 description           pic x(18).

      * Constant-text.
       01 quantity-too-much     pic x(27)
                                value "Quantity must be <= 99999.9".
       01 quantity-too-small    pic x(20)
                                value "Quantity must be > 0".
       01 not-numeric           pic x(16)
                                value " is NOT numeric.".

       PROCEDURE DIVISION.
       100-main-paragraph.
           perform 200-opening-screen
           perform 300-pulley1-data-entry
           perform 400-pulley2-data-entry
           perform 500-belt-length-data-entry
           perform 600-calculate-it
           perform 700-display-results
           perform 999-end-program.

       200-opening-screen.
           display spaces
           display "*** PULLEY DISTANCE CALCULATOR UTILITY BEGINS ***"
           display "Written by, Clifford A. Chipman, EMIT"
           display "June 25, 2021"
           display spaces
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces
           display "Enter zero for any parameter to end the program."
           display spaces.

       300-pulley1-data-entry.
           move "pulley #1 diameter" to description
           display "Enter " description ": " with no advancing
           accept pulley-diameter1-in

      * Did the user enter a valid numeric value?
           if function test-numval(pulley-diameter1-in)
              IS NOT EQUAL ZERO then
              display "Previous " description not-numeric
              perform 300-pulley1-data-entry
           else
              compute pulley-diameter1 =
                      function numval(pulley-diameter1-in)
           end-if

           evaluate true
           when pulley-diameter1 IS EQUAL ZERO
              perform 999-end-program

           when pulley-diameter1 IS NEGATIVE
              display quantity-too-small
              perform 300-pulley1-data-entry

           when pulley-diameter1 > 99999.9
              display quantity-too-much
              perform 300-pulley1-data-entry

           end-evaluate.

       400-pulley2-data-entry.
           move "pulley #2 diameter" to description
           display "Enter " description ": " with no advancing
           accept pulley-diameter2-in

      * Did the user enter a valid numeric value?
           if function test-numval(pulley-diameter2-in)
              IS NOT EQUAL ZERO then
              display "Previous " description not-numeric
              perform 400-pulley2-data-entry
           else
              compute pulley-diameter2 =
                      function numval(pulley-diameter2-in)
           end-if

           evaluate true
           when pulley-diameter2 IS EQUAL ZERO
              perform 999-end-program

           when pulley-diameter2 IS NEGATIVE
              display quantity-too-small
              perform 400-pulley2-data-entry

           when pulley-diameter2 > 99999.9
              display quantity-too-much
              perform 400-pulley2-data-entry

           end-evaluate.

       500-belt-length-data-entry.
           move "belt length" to description
           display "Enter " description ": " with no advancing
           accept belt-length-in

      * Did the user enter a valid numeric value?
           if function test-numval(belt-length-in)
              IS NOT EQUAL ZERO then
              display "Previous " description not-numeric
              perform 500-belt-length-data-entry
           else
              compute belt-length =
                      function numval(belt-length-in)
           end-if

           evaluate true
           when belt-length IS EQUAL ZERO
              perform 999-end-program

           when belt-length IS NEGATIVE
              display quantity-too-small
              perform 500-belt-length-data-entry

           when belt-length > 99999.9
              display quantity-too-much
              perform 500-belt-length-data-entry

           end-evaluate.

       600-calculate-it.
      *
      * Belt Length Formula:
      *    compute belt-length rounded = function pi *
      *       (pulley-diameter1 + pulley-diameter2) * 0.5 +
      *       (pulley-distance * 2) +
      *       (function abs(pulley-diameter2 - pulley-diameter1) ** 2 /
      *       (pulley-distance * 4)).
      *
           add pulley-diameter1 to pulley-diameter2
                 giving pulley-diameter-sum

           subtract pulley-diameter2 from pulley-diameter1
                 giving pulley-diameter-diff

           compute x rounded = (4 * belt-length) -
                                (2 * function pi * pulley-diameter-sum)

           compute numerator rounded = x + function sqrt(x ** 2 - 32 *
                    (pulley-diameter-diff ** 2))

           divide numerator by denominator
                    giving pulley-distance rounded.

       700-display-results.
           move pulley-distance to pulley-distance-out
      *
      *     move pulley-diameter-sum to pulley-diameter-sum-out
      *     move pulley-diameter-diff to pulley-diameter-diff-out
      *     move x to x-out
      *     move numerator to numerator-out
      *     display spaces
      *     display "pulley diameter sum: " pulley-diameter-sum-out
      *     display "pulley diameter diff: " pulley-diameter-diff-out
      *     display "x: " x-out
      *     display "numerator: " numerator-out
      *
           display "Pulley Distance: " pulley-distance-out.

       999-end-program.
           display spaces
           display "*** PULLEY DISTANCE CALCULATOR UTILITY ENDS ***"
           display spaces
           stop run.

