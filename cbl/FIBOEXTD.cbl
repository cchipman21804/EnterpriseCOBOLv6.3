       PROCESS ARITH(EXTEND)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FIBONACI.
       AUTHOR.        CHIPMAN.
      *
      * Calculate the Fibonacci Sequence out to 31 digits.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIBO1       PIC 9(31) VALUE ZERO.
       01 FIBO2       PIC 9(31) VALUE 1.
       01 FIBO3       PIC Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
       01 COUNTER     PIC 999   VALUE ZERO.
       01 HIGHEST     PIC 999   VALUE 150.
      *
      * The Fibonacci sequence is calculated by adding the last two
      * numbers in the sequence to obtain the latest number.
      *
       PROCEDURE DIVISION.
       ONLY-PARA.

           MOVE FIBO1 TO FIBO3
           DISPLAY "Fib( " COUNTER " ): " FIBO3
           ADD 1 TO COUNTER
           MOVE FIBO2 TO FIBO3
           DISPLAY "Fib( " COUNTER " ): " FIBO3

           PERFORM VARYING COUNTER FROM 2 BY 1 UNTIL COUNTER > HIGHEST

              ADD FIBO1 TO FIBO2 GIVING FIBO3
              DISPLAY "Fib( " COUNTER " ): " FIBO3
              MOVE FIBO2 TO FIBO1
              MOVE FIBO3 TO FIBO2

           END-PERFORM
           STOP RUN.
