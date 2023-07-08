       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CNCNTRAT.
       AUTHOR.         CHIPMAN.
      *
      * This program plays the children's game "Concentration", also
      * known as the card game "Memory".  Find matches by uncovering
      * two cells in the 2-D table.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYMBOL-TABLE-REPORT
           ASSIGN TO CHEATSHT.

           SELECT POPULATED-CARD-TABLE
           ASSIGN TO CARDTABL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SYMBOL-TABLE-REPORT RECORDING MODE F.
       01 REPORT-RECORD                           PIC X(80).

       FD POPULATED-CARD-TABLE RECORDING MODE F.
       01 CARD-TABLE-RECORD                       PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01 WS-REPORT-RECORD.
           05 CURRENT-SYMBOL                      PIC X.
           05 FILLER                              PIC X VALUE SPACE.
           05 CURRENT-X1                          PIC XX.
           05 FILLER                              PIC X VALUE ','.
           05 CURRENT-Y1                          PIC XX.
           05 FILLER                              PIC X VALUE SPACE.
           05 CURRENT-X2                          PIC XX.
           05 FILLER                              PIC X VALUE ','.
           05 CURRENT-Y2                          PIC XX.
           05 FILLER                              PIC X(67) VALUE SPACE.

       01 WS-CARD-TABLE-RECORD                    PIC X(80) VALUE SPACE.
      *
      * The difficulty is an integer from 1 to 11.  It determines the
      * dimensions of the two-dimentional table.  TABLE-SIZE is
      * computed by adding 2 to difficulty.  This produces a 2-D table
      * from 3x3 to 13x13.
      *
      * Any difficulty level from 12 - 14 results in very long execution
      * times while the CPU attempts to randomly find empty cells and
      * available symbols (both of which become more elusive as the
      * table size increases with the difficulty level and available
      * symbols are used).
      *
       01 DIFFICULTY                              PIC 99    VALUE 11.
       01 TABLE-SIZE                              PIC 99.
      *
      * Stores every printable symbol used in the game & provides a
      * convenient way to index each symbol.
       01 ALL-SYMBOLS.
           05 ALPHA-SYMBOLS                       PIC X(52) VALUE
           "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".
           05 NUMERIC-SYMBOLS                     PIC X(10) VALUE
           "0123456789".
           05 OTHER-SYMBOLS                       PIC X(33) VALUE
           "~()›<+|&!$*;^-/º,%_>?`:#@'=\[]{}Å".
      *                                      A total of 95 symbols.
      * NUMBER-OF-SYMBOLS helps generate a random index to pick a
      * particular symbol from the AVAILABLE-SYMBOLS table.
       01 NUMBER-OF-SYMBOLS                       PIC 99    VALUE 95.
      *
      * Stores the symbols which are available for population. Replace
      * used symbols with a space character to avoid duplication within
      * the CARD-TABLE.
       01 AVAILABLE-SYMBOLS.
           05 SYM  OCCURS 1 TO 99 TIMES DEPENDING ON NUMBER-OF-SYMBOLS
                                                  PIC X.

       01 IDX                                     PIC 99    VALUE 1.
       01 LOOP-COUNTER                            PIC 999.
      *
      * Stores the contents of the current random deck
       01 CARD-TABLE.
           05 r     OCCURS 3 to 13 TIMES DEPENDING ON TABLE-SIZE.
              10 c  OCCURS 3 to 13 TIMES DEPENDING ON TABLE-SIZE
                                                  PIC X     VALUE SPACE.
      *
      * Used for display
       01 BLANK-TABLE.
           05 r     OCCURS 3 to 13 TIMES DEPENDING ON TABLE-SIZE.
              10 c  OCCURS 3 to 13 TIMES DEPENDING ON TABLE-SIZE
                                                  PIC X     VALUE SPACE.
      *
      * Randomize the game
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
      *
       01 RANDOM-SYMBOL-IDX                    PIC 99.
       01 RANDOM-X                             PIC 99.
       01 RANDOM-Y                             PIC 99.
      *
       PROCEDURE DIVISION.
       MAIN.
           PERFORM 100-OPEN-FILES
           PERFORM 105-INITIALIZE
           PERFORM 110-GENERATE-SEEDS
      *
      * Populate the 2-D table CARD-TABLE with the randomly generated
      * symbols. Place the symbols in two random locations WHICH ARE
      * ALREADY EMPTY (SPACE)
      *
           PERFORM 120-POPULATE-CARD-TABLE
           PERFORM 130-DISPLAY-CARD-TABLE
           CLOSE SYMBOL-TABLE-REPORT
           CLOSE POPULATED-CARD-TABLE
           STOP RUN.
      *
       100-OPEN-FILES.
           OPEN OUTPUT SYMBOL-TABLE-REPORT
      * Write header row to report
           MOVE "  X1,Y1 X2,Y2" TO REPORT-RECORD
           WRITE REPORT-RECORD
      *
           OPEN OUTPUT POPULATED-CARD-TABLE.
      * Think about how to write header rows here by using the same
      * sub-paragraphs as 130-DISPLAY-CARD-TABLE which automatically
      * change the headers based on the difficulty level.
      *
       105-INITIALIZE.
      *
      * Set TABLE-SIZE based on the value stored in DIFFICULTY
           ADD 2 TO DIFFICULTY GIVING TABLE-SIZE
      *
      *     ******************** CAUTION ********************
      * The PERFORM UNTIL statements (DO...UNTIL LOOP) in other
      * paragraphs can create infinite loops if the LOOP-COUNTER
      * calculation within this paragraph is incorrect!
      *
      * LOOP-COUNTER needs to be less than or equal to the value
      * of TABLE-SIZE SQUARED in order for the POPULATE-CARD-TABLE
      * paragraph to work properly without errors and without causing
      * an infinite loop
      *
           COMPUTE LOOP-COUNTER = TABLE-SIZE * TABLE-SIZE * 0.5
      *
      * Populate AVAILABLE-SYMBOLS table with printable symbols --
           MOVE ALL-SYMBOLS TO AVAILABLE-SYMBOLS.

       110-GENERATE-SEEDS.
      *
      * Obtain system date & time to help seed the RANDOM function
           MOVE FUNCTION CURRENT-DATE TO DATETIME
      *
      * Generate SEED from DATETIME
           COMPUTE SEED = MO * DD * HH * MM * SS * HUND-SEC
      *
      * Lower SEED to within maximum limit for the RANDOM function
           SUBTRACT 801076519 FROM SEED
      *
      * Initialize seeded pseudo-random number sequences
           COMPUTE RANDOM-SYMBOL-IDX =
                    FUNCTION INTEGER (
                    FUNCTION RANDOM(SEED) * NUMBER-OF-SYMBOLS + 1)

           COMPUTE RANDOM-X =
                    FUNCTION INTEGER (
                    FUNCTION RANDOM(SEED) * TABLE-SIZE + 1)

           COMPUTE RANDOM-Y =
                    FUNCTION INTEGER (
                    FUNCTION RANDOM(SEED) * TABLE-SIZE + 1).
      *
       120-POPULATE-CARD-TABLE.
      * This paragraph populates 2 empty cells of CARD-TABLE with a
      * random symbol
           PERFORM LOOP-COUNTER TIMES
      *
      *     ******************** CAUTION ********************
      * These PERFORM UNTIL statements (DO...UNTIL loops) can create
      * infinite loops if the LOOP-COUNTER calculation within the
      * 100-INITIALIZE paragraph is incorrect!
      *
      * If all of the printable symbols are used up, this condition
      * will trigger an infinite loop as the CPU continues to search
      * for a non-existent printable character within the table.
      *
              PERFORM WITH TEST AFTER
                 UNTIL SYM(RANDOM-SYMBOL-IDX) IS NOT EQUAL TO SPACE
                 COMPUTE RANDOM-SYMBOL-IDX =
                          FUNCTION INTEGER (
                          FUNCTION RANDOM * NUMBER-OF-SYMBOLS + 1)
              END-PERFORM
      *
      * The value of IDX determines whether to log RANDOM-X & RANDOM-Y
      * in CURRENT-X1 & CURRENT-Y1 or in CURRENT-X2 & CURRENT-Y2 in
      * the CHEATSHT report.
              MOVE 1 TO IDX
              PERFORM 2 TIMES
      *
      *     ******************** CAUTION ********************
      * These PERFORM UNTIL statements (DO...UNTIL loops) can create
      * infinite loops if the LOOP-COUNTER calculation within the
      * 100-INITIALIZE paragraph is incorrect!
      *
      * If CARD-TABLE runs out of EMPTY cells, this condition will
      * trigger an infinite loop as the CPU continues to search for a
      * non-existent SPACE character within the table.
      *
      * If the cell in CARD-TABLE is an empty space, populate it
      * with the random symbol
                 PERFORM WITH TEST AFTER
                    UNTIL C IN CARD-TABLE(RANDOM-Y, RANDOM-X)
                          IS EQUAL TO SPACE
                    COMPUTE RANDOM-X = FUNCTION INTEGER (
                                       FUNCTION RANDOM * TABLE-SIZE + 1)

                    COMPUTE RANDOM-Y = FUNCTION INTEGER (
                                       FUNCTION RANDOM * TABLE-SIZE + 1)
                 END-PERFORM
      *
      * Log the current symbol and its positions in CHEATSHT
                 MOVE SYM(RANDOM-SYMBOL-IDX) TO CURRENT-SYMBOL
                 IF IDX IS LESS THAN 2 THEN
                    MOVE RANDOM-X TO CURRENT-X1
                    MOVE RANDOM-Y TO CURRENT-Y1
                    ADD 1 TO IDX
                 ELSE
                    MOVE RANDOM-X TO CURRENT-X2
                    MOVE RANDOM-Y TO CURRENT-Y2
                 END-IF
      *
      * Populate the CARD-TABLE with the current random symbol
                 MOVE SYM(RANDOM-SYMBOL-IDX) TO
                      C IN CARD-TABLE(RANDOM-Y, RANDOM-X)
              END-PERFORM
      *
      * Remove used symbol from 1-D table
              MOVE SPACE TO SYM(RANDOM-SYMBOL-IDX)
      *
      * and write the log to the file
              MOVE WS-REPORT-RECORD TO REPORT-RECORD
              WRITE REPORT-RECORD
           END-PERFORM.
      *
       130-DISPLAY-CARD-TABLE.
      *
      * This paragraph displays the contents of CARD-TABLE with row
      * and column headers  on the screen and writes the contents to an
      * output file
      * Think about how to dispaly header rows here by using the same
      * sub-paragraphs as 105-OPEN-FILES which automatically
      * change the headers based on the difficulty level.
           DISPLAY "DIFFICULTY: " DIFFICULTY
           STRING "DIFFICULTY: " DIFFICULTY DELIMITED BY SIZE
                    INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
           DISPLAY "TABLE-SIZE: " TABLE-SIZE
           STRING "TABLE-SIZE: " TABLE-SIZE DELIMITED BY SIZE
                    INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
           DISPLAY "LOOP-COUNTER: " LOOP-COUNTER
           STRING "LOOP-COUNTER: " LOOP-COUNTER DELIMITED BY SIZE
                    INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
           DISPLAY "CARD-TABLE CONTENTS:"
           MOVE "CARD-TABLE CONTENTS:" TO WS-CARD-TABLE-RECORD
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
           DISPLAY SPACES
           MOVE SPACES TO WS-CARD-TABLE-RECORD
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
           DISPLAY " X 0000000001111"
           MOVE " X 0000000001111" TO WS-CARD-TABLE-RECORD
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
           DISPLAY "   1234567890123"
           MOVE "   1234567890123" TO WS-CARD-TABLE-RECORD
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
           DISPLAY "Y +-------------+"
           MOVE "Y +-------------+" TO WS-CARD-TABLE-RECORD
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
      * Hyphens and trailing plus sign remain in WS-CARD-TABLE-RECORD
      * at lower difficulty levels (Level 1 shown in comments below)
      *
      * CARD-TABLE CONTENTS:
      *
      *  X 00000000011
      *    12345678901
      * Y +-----------+
      * 01|RY›|-------+
      * 02|›9 |-------+
      * 03|Y9R|-------+
      *   +-----------+
      *
      * So, clear it with SPACES first
           MOVE SPACES TO WS-CARD-TABLE-RECORD
      *
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL IDX IS GREATER THAN TABLE-SIZE
                   DISPLAY IDX "|" R IN CARD-TABLE(IDX) "|"
                   STRING IDX "|" R IN CARD-TABLE(IDX) "|"
                          DELIMITED BY SIZE
                          INTO WS-CARD-TABLE-RECORD
                   END-STRING
                   MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
                   WRITE CARD-TABLE-RECORD
           END-PERFORM
           DISPLAY "  +-------------+"
           MOVE "  +-------------+" TO WS-CARD-TABLE-RECORD
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
           DISPLAY SPACES
      * This variable should only be logged on the SYSOUT display
      * because it is too long for an 80-column data set
           DISPLAY "AVAILABLE-SYMBOLS: " AVAILABLE-SYMBOLS.
