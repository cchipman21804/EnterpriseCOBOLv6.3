       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BELT-LENGTH.
       AUTHOR.        CHIPMAN.
      *
      *//* COMPILE COBOL SOURCE CODE AND LINK OBJECT
      *//BELTLEN  JOB 1,NOTIFY=&SYSUID
      *//* COMPILE & LINK ONLY
      *// SET COBPGM='BELTLEN'
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
       01 pulley-distance-in    pic x(8).

      * Calculated-fields.
       01 pulley-diameter1      pic 9(5)v9(5).
       01 pulley-diameter2      pic 9(5)v9(5).
       01 pulley-distance       pic 9(5)v9(5).
       01 belt-length           pic 9(5)v9(5).

      * Displayed-fields.
       01 belt-length-out       pic zzzz9.99.
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
           perform 500-pulley-distance-data-entry
           perform 600-calculate-it
           perform 700-display-results
           perform 999-end-program.

       200-opening-screen.
           display spaces
           display "***** BELT LENGTH CALCULATOR UTILITY BEGINS *****"
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

       500-pulley-distance-data-entry.
           move "pulley distance" to description
           display "Enter " description ": " with no advancing
           accept pulley-distance-in

      * Did the user enter a valid numeric value?
           if function test-numval(pulley-distance-in)
              IS NOT EQUAL ZERO then
              display "Previous " description not-numeric
              perform 500-pulley-distance-data-entry
           else
              compute pulley-distance =
                      function numval(pulley-distance-in)
           end-if

           evaluate true
           when pulley-distance IS EQUAL ZERO
              perform 999-end-program

           when pulley-distance IS NEGATIVE
              display quantity-too-small
              perform 500-pulley-distance-data-entry

           when pulley-distance > 99999.9
              display quantity-too-much
              perform 500-pulley-distance-data-entry

           end-evaluate.

       600-calculate-it.
           compute belt-length rounded = function pi *
              (pulley-diameter1 + pulley-diameter2) * 0.5 +
              (pulley-distance * 2) +
              (function abs(pulley-diameter2 - pulley-diameter1) ** 2 /
              (pulley-distance * 4)).

       700-display-results.
           move belt-length to belt-length-out
           display "Belt Length: " belt-length-out.

       999-end-program.
           display spaces
           display "***** BELT LENGTH CALCULATOR UTILITY ENDS *****"
           display spaces
           stop run.

