       IDENTIFICATION DIVISION.
      *
      * BINARY is an invalid PROGRAM-ID because BINARY is a COBOL
      * reserved word.
      *
      * Converts Decimal - Binary
      *
       PROGRAM-ID.  CONVRTDB.
       AUTHOR.      CHIPMAN.
      *
      * From page #10 of "More BASIC Computer Games - TRS-80 Edition"
      * written in 1980 by David H. Ahl.
      *
      * This game tests your skills in binary-to-decimal and
      * decimal-to-binary conversion.  You are given twenty conversion
      * trials.  Numbers are chosen randomly and your score is printed
      * at the end.  The answer to any conversion you miss is displayed:
      * if the next conversion is presented, you may assume you got the
      * previous one correct.
      *
      * There are several possible modifications for this program such
      * as timing the response, allowing the user to specify the number
      * range, checking for duplicate numbers, or extending it to other
      * bases.
      *
      * This program was written by Ted Park of Pacific Union College.
      * It originally appeared in the Mar/Apr 1975 issue of Creative
      * Computing magazine.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 datetime.
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
       01 SEED                                 PIC 9(10).
       01 LOOP-COUNTER                         PIC 99.
       01 MAX                                  PIC 99 VALUE 32.
       01 RANDOM-NUMBER                        PIC 99.
       01 DECIMAL-NUMBER                       PIC Z9.
       01 POWER-COUNTER                        PIC 9.
       01 POWER-OF-TWO                         PIC 99.
       01 CORRECT                              PIC 99 VALUE 20.
       01 SCORE                                PIC 999V99.
       01 SCORE-OUT                            PIC ZZ9.99.
      * 01 UNIQUE-COUNTER                       PIC 99.
      * 01 PREV-NUMBER                          PIC 99 VALUE ZERO.
      *
       01 TEST-QUESTIONS.
           05 DECIMAL-ANSWERS OCCURS 20 TIMES  PIC 99.
           05 BINARY-ANSWERS  OCCURS 20 TIMES  PIC X(5) VALUE "00000".
      *
       01 BITS.
           05 BINARY-RESULT   OCCURS 5 TIMES   PIC X.
      *
       01 TAB                                  PIC X(16) VALUE SPACES.
       01 GUESS                                PIC X(5).
      * 01 GUESSNUM                             PIC 9(5).
      * 01 GUESSLEN                             PIC 9.
      *
      * 01 MODE-FLAG                            PIC X(7) VALUE SPACES.
      * 01 NOT-MODE-FLAG                        PIC X(7) VALUE SPACES.
      *
      * 01 UNIQUE-FLAG                          PIC X    VALUE "F".
      *     88 UNIQUE                                    VALUE "T".
      *
      * IBM documentation on various Enterprise COBOL v6.3 Intrinsic Functions:
      * https://www.ibm.com/docs/en/cobol-zos/6.3?topic=functions-current-date
      * https://www.ibm.com/docs/en/cobol-zos/6.3?topic=functions-integer
      * https://www.ibm.com/docs/en/cobol-zos/6.3?topic=functions-length
      * https://www.ibm.com/docs/en/cobol-zos/6.3?topic=functions-numval
      * https://www.ibm.com/docs/en/cobol-zos/6.3?topic=functions-random
      * https://www.ibm.com/docs/en/cobol-zos/6.3?topic=functions-test-numval
      *
       PROCEDURE DIVISION.
       100-MAIN.
      *
      * Generate a seed for the RANDOM intrinsic function
           PERFORM 110-GENERATE-SEED
      *
      * Display this for debugging purposes then comment out:
      *     DISPLAY "Date/Time: " DATETIME
      *     DISPLAY "SEED: " SEED
      *     DISPLAY SPACES
      *                                       ---------future feature
      *                                       vvvvvv
      * Populate TEST-QUESTIONS table with 20 unique, random numbers
           PERFORM 115-POPULATE-TEST
      *
      * Display contents of tables - FOR DEBUGGING ONLY
      *     PERFORM 120-DISPLAY-TABLES
      *
      * Display each table entry in sequence & accept GUESS from user
           DISPLAY SPACES
           DISPLAY "BINARY TO DECIMAL TEST:"
           PERFORM 130-BINARY-DECIMAL-TEST
              VARYING LOOP-COUNTER FROM 1 BY 1
              UNTIL LOOP-COUNTER IS GREATER THAN 10
      *
           DISPLAY SPACES
           DISPLAY "DECIMAL TO BINARY TEST:"
           PERFORM 140-DECIMAL-BINARY-TEST
              VARYING LOOP-COUNTER FROM 11 BY 1
              UNTIL LOOP-COUNTER IS GREATER THAN 20
      *
      * Calculate score and display it for the user
           PERFORM 150-CALCULATE-SCORE
           STOP RUN.
      *
       110-GENERATE-SEED.
      *
      * Obtain system date & time to help seed RANDOM function
           MOVE FUNCTION CURRENT-DATE TO DATETIME
      *
      * Generate SEED from DATETIME
           COMPUTE SEED = MO * DD * HH * MM * SS * HUND-SEC
      *
      * Lower SEED to within maximum limit for RANDOM function
           SUBTRACT 801076519 FROM SEED
      *
      * Initialize seeded pseudo-random number sequence
           COMPUTE RANDOM-NUMBER = FUNCTION INTEGER (
                                   FUNCTION RANDOM(SEED) * MAX).
      *
       115-POPULATE-TEST.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1
              UNTIL LOOP-COUNTER IS GREATER THAN 20
              PERFORM 200-GENERATE-UNIQUE-RANDOM-NUM
              PERFORM 220-CONVERT-DEC2BIN
           END-PERFORM.
      *
      * 120-DISPLAY-TABLES.
      *
      * TEST-QUESTIONS TABLES:
      *     DISPLAY "## DEC | BINARY"
      *     PERFORM VARYING LOOP-COUNTER FROM 1 BY 1
      *        UNTIL LOOP-COUNTER IS GREATER THAN 20
      *        MOVE DECIMAL-ANSWERS(LOOP-COUNTER) TO DECIMAL-NUMBER
      *        DISPLAY LOOP-COUNTER ": " DECIMAL-NUMBER " | "
      *-               BINARY-ANSWERS(LOOP-COUNTER)
      *     END-PERFORM.
      *
       130-BINARY-DECIMAL-TEST.
           DISPLAY LOOP-COUNTER ": BINARY: "
      -            BINARY-ANSWERS(LOOP-COUNTER) " | DECIMAL? "
                   WITH NO ADVANCING
           ACCEPT GUESS
      *
      * Comment out after debugging:
      *     DISPLAY "GUESS LENGTH:"
      *     COMPUTE GUESSNUM = FUNCTION NUMVAL(GUESS)
      *     COMPUTE GUESSLEN = FUNCTION LENGTH(GUESS)
      *     DISPLAY GUESSNUM "      " GUESSLEN
      * Checks for non-numeric answers FIRST
           IF FUNCTION TEST-NUMVAL(GUESS) IS EQUAL TO ZERO
              THEN
      * Checks for answers longer than two digits
      * FUNCTION NUMVAL allows user to enter single digit answers
              IF FUNCTION NUMVAL(GUESS) IS GREATER THAN 99
              OR
      * Checks for inequality of GUESS and corresponding DECIMAL-ANSWER
              FUNCTION NUMVAL(GUESS) IS NOT EQUAL TO
                                        DECIMAL-ANSWERS(LOOP-COUNTER)
              THEN
                 SUBTRACT 1 FROM CORRECT
                 MOVE DECIMAL-ANSWERS(LOOP-COUNTER) TO DECIMAL-NUMBER
                 DISPLAY TAB "  | CORRECT ANSWER: " DECIMAL-NUMBER
              END-IF
              ELSE
                 DISPLAY TAB "  | CORRECT ANSWER MUST BE NUMERIC."
           END-IF.
      *
       140-DECIMAL-BINARY-TEST.
           MOVE DECIMAL-ANSWERS(LOOP-COUNTER) TO DECIMAL-NUMBER
           DISPLAY LOOP-COUNTER ": DECIMAL: "
      -            DECIMAL-NUMBER " | BINARY? "
                   WITH NO ADVANCING
           ACCEPT GUESS
      * Checks for inequality of GUESS and corresponding BINARY-ANSWER
           IF GUESS IS NOT EQUAL TO BINARY-ANSWERS(LOOP-COUNTER)
              THEN
                 SUBTRACT 1 FROM CORRECT
                 DISPLAY TAB "| CORRECT ANSWER: "
      -                   BINARY-ANSWERS(LOOP-COUNTER)
           END-IF.
      *
       150-CALCULATE-SCORE.
           DIVIDE CORRECT BY 20 GIVING SCORE ROUNDED
           MULTIPLY 100 BY SCORE
           MOVE SCORE TO SCORE-OUT
           DISPLAY SPACES
           DISPLAY "CORRECT: " CORRECT
           DISPLAY "You scored: " SCORE-OUT "%.".
      *
       200-GENERATE-UNIQUE-RANDOM-NUM.
           PERFORM 210-GENERATE-RANDOM-NUMBER
      *     PERFORM VARYING UNIQUE-COUNTER FROM 1 BY 1
      *        UNTIL UNIQUE OR UNIQUE-COUNTER IS EQUAL TO LOOP-COUNTER
      *     IF DECIMAL-ANSWERS(UNIQUE-COUNTER) IS EQUAL TO RANDOM-NUMBER
      *      THEN
      *        MOVE "F" TO UNIQUE-FLAG
      *      ELSE
      *        MOVE "T" TO UNIQUE-FLAG
      *     END-IF
      *     IF UNIQUE
      *      THEN
              MOVE RANDOM-NUMBER TO DECIMAL-ANSWERS(LOOP-COUNTER).
      *      ELSE
      *        PERFORM 210-GENERATE-RANDOM-NUMBER
      *     END-IF
      *     END-PERFORM.
      *
       210-GENERATE-RANDOM-NUMBER.
      *
      * This generates random numbers between 1 & 32 inclusive:
      *     COMPUTE RANDOM-NUMBER = FUNCTION INTEGER (
      *                             FUNCTION RANDOM * 32) + 1.
      *
      * This generates random numbers between 0 & 31 inclusive:
      *     COMPUTE RANDOM-NUMBER = FUNCTION INTEGER (
      *                             FUNCTION RANDOM * 32).
           COMPUTE RANDOM-NUMBER = FUNCTION INTEGER (
                                   FUNCTION RANDOM * MAX).
      *
      * Convert decimal to binary string
       220-CONVERT-DEC2BIN.
      *
      *  16  8  4  2  1
      *   1  0  1  0  1  == 21
      *
      *   0  1  0  1  1  == 11
      *
      * Initialize these variables for the paragraph
           MOVE 16 TO POWER-OF-TWO
           MOVE "00000" TO BITS
      *
      * Convert the decimal number to a string of 5 binary digits
           PERFORM VARYING POWER-COUNTER FROM 1 BY 1
              UNTIL POWER-COUNTER IS GREATER THAN 5
              IF RANDOM-NUMBER IS GREATER THAN OR EQUAL TO POWER-OF-TWO
                 THEN
                    MOVE "1" TO BINARY-RESULT(POWER-COUNTER)
                    SUBTRACT POWER-OF-TWO FROM RANDOM-NUMBER
              ELSE
                    MOVE "0" TO BINARY-RESULT(POWER-COUNTER)
              END-IF
              DIVIDE 2 INTO POWER-OF-TWO
           END-PERFORM
      *
           MOVE BITS TO BINARY-ANSWERS(LOOP-COUNTER).
      *
