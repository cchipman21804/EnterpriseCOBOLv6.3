       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FIZZBUZZ.
       AUTHOR.        CHIPMAN.
      *
      * Classic FizzBuzz - Count from 1 to 100
      *                  - Display FIZZ for every multiple of 3
      *                  - Display BUZZ for every multiple of 5
      *                  - Display FIZZBUZZ for every multiple of 3 & 5
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 COUNTER        PIC 999.
       01 NONSENSE       PIC X(8).

       PROCEDURE DIVISION.
       100-MAIN-PARA.
           PERFORM VARYING COUNTER FROM 1 BY 1
                   UNTIL COUNTER IS EQUAL TO 101

                   MOVE ALL SPACES TO NONSENSE

                   EVALUATE TRUE
                   WHEN (FUNCTION MOD(COUNTER, 3) IS EQUAL TO ZERO
                    AND  FUNCTION MOD(COUNTER, 5) IS EQUAL TO ZERO)
                        MOVE "FizzBuzz" TO NONSENSE

                   WHEN FUNCTION MOD(COUNTER, 3) IS EQUAL TO ZERO
                        MOVE "Fizz" TO NONSENSE

                   WHEN FUNCTION MOD(COUNTER, 5) IS EQUAL TO ZERO
                        MOVE "Buzz" TO NONSENSE

                   WHEN OTHER
                        MOVE COUNTER TO NONSENSE
                   END-EVALUATE

                   DISPLAY NONSENSE
           END-PERFORM
           STOP RUN.
