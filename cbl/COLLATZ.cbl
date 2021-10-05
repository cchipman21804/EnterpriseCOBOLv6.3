       PROCESS ARITH(EXTEND)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COLLATZ.
       AUTHOR.         CHIPMAN.
      *
      * Calculate and display the Collatz Conjecture sequence for the
      * requested integer entered by the user.
      *
      * //* COMPILE COBOL SOURCE CODE AND LINK OBJECT
      * //COLLATZ  JOB 1,NOTIFY=&SYSUID
      * //* COMPILE & LINK ONLY
      * // SET COBPGM='COLLATZ'
      * //***************************************************/
      * //COBRUN  EXEC IGYWCL
      * //COBOL.SYSIN  DD DSN=&SYSUID..CBL(&COBPGM),DISP=SHR
      * //LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
      * //***************************************************/
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SEED-IN      PIC X(31).
       01 SEED         PIC 9(31).
       01 SEED-OUT     PIC Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
       01 ITERATIONS   PIC 9(31) VALUE ZERO.
       01 ITER-OUT     PIC Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
       01 MAXIMUM      PIC 9(31) VALUE 9999999999999999999999999999999.
      *
      * https://en.wikipedia.org/wiki/Collatz_conjecture
      * https://youtu.be/094y1Z2wpJg
      *
      * Pick a number
      * If number is odd, multiply by 3 and add 1
      * If number is even, divide by 2
      * If number equals 1, exit
      * Count the iterations to the exit
      *
       01 datetime-start.
           02 yyyy      pic 9(4).
           02 mo        pic 99.
           02 dd        pic 99.
           02 hh        pic 99.
           02 mm        pic 99.
           02 ss        pic 99.
           02 hund-sec  pic 99.
           02 plsormns  pic x.
           02 tzh       pic 99.
           02 tzm       pic 99.
      *
       01 datetime-end.
           02 yyyy      pic 9(4).
           02 mo        pic 99.
           02 dd        pic 99.
           02 hh        pic 99.
           02 mm        pic 99.
           02 ss        pic 99.
           02 hund-sec  pic 99.
           02 plsormns  pic x.
           02 tzh       pic 99.
           02 tzm       pic 99.
      *
      * Constant-text.
       01 quantity-too-much     pic x(27)
                                value "Quantity must be <= 99999.9".
       01 quantity-too-small    pic x(20)
                                value "Quantity must be > 0".
       01 not-numeric           pic x(16)
                                value " is NOT numeric.".
       01 TWELVE-SPACES         PIC X(12) VALUE SPACES.

       PROCEDURE DIVISION.
       100-MAIN.
           PERFORM 110-SEED-DATA-ENTRY.

           MOVE FUNCTION CURRENT-DATE TO datetime-start

           PERFORM VARYING ITERATIONS FROM 0 BY 1 UNTIL SEED = 1
              IF FUNCTION MOD(SEED, 2) IS EQUAL TO ZERO THEN
                 DIVIDE 2 INTO SEED
              ELSE
                 MULTIPLY 3 BY SEED
                 ADD 1 TO SEED
              END-IF
              MOVE SEED TO SEED-OUT
              DISPLAY TWELVE-SPACES SEED-OUT
           END-PERFORM.

           PERFORM 999-END-PROGRAM.

       110-SEED-DATA-ENTRY.
           DISPLAY "Enter seed value (up to 31 digits): "
           ACCEPT SEED-IN
      * Did the user enter a valid numeric value?
           IF FUNCTION TEST-NUMVAL(SEED-IN) IS NOT EQUAL ZERO THEN
              DISPLAY "Seed value " not-numeric
              PERFORM 110-SEED-DATA-ENTRY
           ELSE
              COMPUTE SEED = FUNCTION NUMVAL(SEED-IN)
           END-IF

           EVALUATE TRUE
           WHEN SEED IS EQUAL ZERO OR SEED = 1
              PERFORM 999-END-PROGRAM

           WHEN SEED IS NEGATIVE
              DISPLAY quantity-too-small
              PERFORM 110-SEED-DATA-ENTRY

           WHEN SEED IS GREATER THAN MAXIMUM
              DISPLAY quantity-too-much
              PERFORM 110-SEED-DATA-ENTRY

           END-EVALUATE.

       999-END-PROGRAM.
           MOVE ITERATIONS TO ITER-OUT
           DISPLAY "Iterations: " ITER-OUT
           MOVE FUNCTION CURRENT-DATE TO datetime-end
           DISPLAY "Started at: " datetime-start
           DISPLAY "Ended at  : " datetime-end
           STOP RUN.
