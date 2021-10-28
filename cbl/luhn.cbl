       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LUHN.
       AUTHOR.        CHIPMAN.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 CC-NUM                              PIC X(16)
                                               VALUE "8830723086640477".
      *
       01 TEST-NUM                            PIC X(16).
       01 DIGIT                               PIC X.
       01 DIGIT-TABLES.
           02 ODD-DIGIT   OCCURS 8 TIMES      PIC 9.
           02 EVEN-DIGIT  OCCURS 8 TIMES      PIC 9.
           02 DBL-DIGIT   OCCURS 8 TIMES      PIC 99.
       01 DIGIT-INDICES.
           02 ODD-IDX                         PIC 9.
           02 EVEN-IDX                        PIC 9.
       01 DIGIT-COUNTER                       PIC 99.
       01 CHECK-DIGIT                         PIC 9.
       01 CHECKSUM                            PIC 9999  VALUE ZERO.
       01 PASS-FAIL                           PIC 9.
           88 PASS                            VALUE ZERO.
      *
       PROCEDURE DIVISION.
       100-MAIN.
      *
      * EXTRACT CHECK-DIGIT FROM CREDIT CARD NUMBER
           MOVE CC-NUM (16:1) TO CHECK-DIGIT.
      *
      * REVERSE CREDIT CARD NUMBER & FEED IT INTO THE LUHN ALGORITHM
           MOVE FUNCTION REVERSE(CC-NUM) TO TEST-NUM
           PERFORM 200-LUHN
           PERFORM 300-SHOW-RESULTS
           STOP RUN.
      *
       200-LUHN.
      *
           MOVE 1 TO ODD-IDX
           MOVE 1 TO EVEN-IDX
      *
      * EXTRACT ODD AND EVEN DIGITS
           PERFORM VARYING DIGIT-COUNTER FROM 1 BY 1
                   UNTIL DIGIT-COUNTER IS GREATER THAN 16
              IF FUNCTION MOD(DIGIT-COUNTER 2) IS EQUAL TO ZERO THEN
                 MOVE TEST-NUM(DIGIT-COUNTER:1)
                    TO EVEN-DIGIT(EVEN-IDX)
                 ADD 1 TO EVEN-IDX
              ELSE
                 MOVE TEST-NUM(DIGIT-COUNTER:1)
                    TO ODD-DIGIT(ODD-IDX)
                 ADD ODD-DIGIT(ODD-IDX) TO CHECKSUM
                 ADD 1 TO ODD-IDX
              END-IF
           END-PERFORM
      *
      * DOUBLE THE EVEN-DIGITS & EXTRACT THE SUM OF THE DOUBLE'S DIGITS
           PERFORM VARYING EVEN-IDX FROM 1 BY 1
                    UNTIL EVEN-IDX IS GREATER THAN 8
              MULTIPLY EVEN-DIGIT(EVEN-IDX) BY 2
                 GIVING DBL-DIGIT(EVEN-IDX)
              IF DBL-DIGIT(EVEN-IDX) IS GREATER THAN 9 THEN
                 SUBTRACT 9 FROM DBL-DIGIT(EVEN-IDX)
              END-IF
              ADD DBL-DIGIT(EVEN-IDX) TO CHECKSUM
           END-PERFORM
      *
           COMPUTE PASS-FAIL = FUNCTION MOD(CHECKSUM 10).
      *
       300-SHOW-RESULTS.
           DISPLAY "CC-NUM: " CC-NUM WITH NO ADVANCING
           DISPLAY SPACES
      *     DISPLAY "ODD-DIGITS:"
      *     PERFORM VARYING DIGIT-COUNTER FROM 1 BY 1
      *              UNTIL DIGIT-COUNTER IS GREATER THAN 8
      *        DISPLAY DIGIT-COUNTER ": " ODD-DIGIT(DIGIT-COUNTER)
      *     END-PERFORM
      *     DISPLAY SPACES
      *     DISPLAY "EVEN-DIGITS:"
      *     PERFORM VARYING DIGIT-COUNTER FROM 1 BY 1
      *              UNTIL DIGIT-COUNTER IS GREATER THAN 8
      *        DISPLAY DIGIT-COUNTER ": " EVEN-DIGIT(DIGIT-COUNTER)
      *     END-PERFORM
      *     DISPLAY SPACES
      *     DISPLAY "DOUBLE-DIGITS SUMS:"
      *     PERFORM VARYING DIGIT-COUNTER FROM 1 BY 1
      *              UNTIL DIGIT-COUNTER IS GREATER THAN 8
      *        DISPLAY DIGIT-COUNTER ": " DBL-DIGIT(DIGIT-COUNTER)
      *     END-PERFORM
      *     DISPLAY SPACES
      *     DISPLAY "CHECK-DIGIT: " CHECK-DIGIT
      *     DISPLAY "CHECKSUM: " CHECKSUM
      *     DISPLAY "RESULT: " PASS-FAIL
           IF PASS THEN
              DISPLAY " VALID"
           ELSE
              DISPLAY " INVALID"
           END-IF.
