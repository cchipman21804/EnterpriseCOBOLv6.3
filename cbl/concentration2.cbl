       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CNCNTRAT.
       AUTHOR.         CHIPMAN.
      *
      * This program plays the children's game "Concentration", also
      * known as the card game "Memory".  Find matches by uncovering
      * two cells in the 2-D table.
      *
      * These files will be removed after debugging is complete.
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
      * Initial values for table headers
       77  ELEM-ALPHA-BET-HEADER                  PIC X(13) VALUE
           "ABCDEFGHIJKLM".

       77  ELEM-NUM-HEADER-1                      PIC X(13) VALUE
           "0000000001111".

       77  ELEM-NUM-HEADER-2                      PIC X(13) VALUE
           "1234567890123".

       01 LINE-HEADER.
           05 LN    OCCURS 3 TO 13 TIMES DEPENDING ON TABLE-SIZE
                                                  PIC X     VALUE "-".

       01 ALPHA-BET-HEADER.
           05 ALPHA OCCURS 3 TO 13 TIMES DEPENDING ON TABLE-SIZE
                                                  PIC X.

       01 NUM-HEADER-1.
           05 NUM-1 OCCURS 3 TO 13 TIMES DEPENDING ON TABLE-SIZE
                                                  PIC X.

       01 NUM-HEADER-2.
           05 NUM-2 OCCURS 3 TO 13 TIMES DEPENDING ON TABLE-SIZE
                                                  PIC X.

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
       01 SEED                                    PIC 9(10).
      *
       01 RANDOM-SYMBOL-IDX                       PIC 99.
       01 RANDOM-X                                PIC 99.
       01 RANDOM-Y                                PIC 99.

       01 IDX                                     PIC 99    VALUE 1.
       01 IDX-OUT                                 PIC Z9.
       01 LOOP-COUNTER                            PIC 999.
       01 LOCATION-1-COORD                        PIC XX.
       01 LOCATION-2-COORD                        PIC XX.
       01 X-LOC-1                                 PIC 99.
       01 Y-LOC-1                                 PIC 99.
       01 X-LOC-2                                 PIC 99.
       01 Y-LOC-2                                 PIC 99.
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
           PERFORM 130-LOG-CARD-TABLE
           ACCEPT LOCATION-1-COORD
           ACCEPT LOCATION-2-COORD
           PERFORM 150-PROCESS-USER-INPUT
           PERFORM 140-DISPLAY-BLANK-TABLE
      *
      * Display result
           IF C IN BLANK-TABLE(Y-LOC-1, X-LOC-1) IS EQUAL TO
              C IN BLANK-TABLE(Y-LOC-2, X-LOC-2) THEN
                 DISPLAY "You found a match!"
           ELSE
                 DISPLAY "These do NOT match!"
           END-IF
      *
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
           MOVE ALL-SYMBOLS TO AVAILABLE-SYMBOLS
      *
      * Populate header tables with initial elementary values --
           MOVE ELEM-ALPHA-BET-HEADER TO ALPHA-BET-HEADER
           MOVE ELEM-NUM-HEADER-1 TO NUM-HEADER-1
           MOVE ELEM-NUM-HEADER-2 TO NUM-HEADER-2.

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
      * the CHEATSHT report. This function can be removed after
      * debugging is complete.
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
      * This function can be removed after debugging is complete.
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
      * This function can be removed after debugging is complete.
              MOVE WS-REPORT-RECORD TO REPORT-RECORD
              WRITE REPORT-RECORD
           END-PERFORM.
      *
       130-LOG-CARD-TABLE.
      *
      * This paragraph displays the contents of CARD-TABLE with row
      * and column headers on the screen and/OR writes the contents to
      * an output file
      *     DISPLAY "DIFFICULTY: " DIFFICULTY
           STRING "DIFFICULTY: " DIFFICULTY DELIMITED BY SIZE
                    INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
      *     DISPLAY "TABLE-SIZE: " TABLE-SIZE
           STRING "TABLE-SIZE: " TABLE-SIZE DELIMITED BY SIZE
                    INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
      *     DISPLAY "LOOP-COUNTER: " LOOP-COUNTER
           STRING "LOOP-COUNTER: " LOOP-COUNTER DELIMITED BY SIZE
                    INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
      *     DISPLAY "CARD-TABLE CONTENTS:"
           MOVE "CARD-TABLE CONTENTS:" TO WS-CARD-TABLE-RECORD
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
      *     DISPLAY SPACES
           MOVE SPACES TO WS-CARD-TABLE-RECORD
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
           PERFORM 131-CARD-TABLE-HEADER-ROWS
      *
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL IDX IS GREATER THAN TABLE-SIZE
      *             DISPLAY IDX "|" R IN CARD-TABLE(IDX) "|"
                   MOVE IDX TO IDX-OUT
                   STRING IDX-OUT "|" R IN CARD-TABLE(IDX) "|"
                          DELIMITED BY SIZE
                          INTO WS-CARD-TABLE-RECORD
                   END-STRING
                   MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
                   WRITE CARD-TABLE-RECORD
           END-PERFORM
      *     DISPLAY "Y +" LINE-HEADER "+"
           STRING "Y +" LINE-HEADER "+" DELIMITED BY SIZE
                 INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD.
      *     DISPLAY SPACES
      * This variable should only be logged on the SYSOUT display
      * because it is too long for an 80-column data set
      *     DISPLAY "AVAILABLE-SYMBOLS: " AVAILABLE-SYMBOLS.

       131-CARD-TABLE-HEADER-ROWS.
      *     DISPLAY " X " NUM-HEADER-1
           STRING " X " NUM-HEADER-1 DELIMITED BY SIZE
                 INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
      *     DISPLAY "   " NUM-HEADER-2
           STRING "   " NUM-HEADER-2 DELIMITED BY SIZE
                 INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
      *     DISPLAY "   " ALPHA-BET-HEADER
           STRING "   " ALPHA-BET-HEADER DELIMITED BY SIZE
                 INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD
      *
      *     DISPLAY "Y +" LINE-HEADER "+"
           STRING "Y +" LINE-HEADER "+" DELIMITED BY SIZE
                 INTO WS-CARD-TABLE-RECORD
           END-STRING
           MOVE WS-CARD-TABLE-RECORD TO CARD-TABLE-RECORD
           WRITE CARD-TABLE-RECORD.
      *
      * When numeric headers were hard-coded into the DISPLAY and MOVE
      * statements in earlier versions, hyphens and trailing plus signs
      * remained in WS-CARD-TABLE-RECORD at lower difficulty levels.
      * (Level 1 shown in comments below)
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
      * So, it was necessary to clear WS-CARD-TABLE-RECORD with SPACES
      * first.  The elementary (77) headers and the variable length
      * header tables in the DATA DIVISION make this step unnecessary.
      *     MOVE SPACES TO WS-CARD-TABLE-RECORD
      *
       140-DISPLAY-BLANK-TABLE.
           DISPLAY "   " ALPHA-BET-HEADER
           DISPLAY "Y +" LINE-HEADER "+"
           PERFORM VARYING IDX FROM 1 BY 1
                    UNTIL IDX IS GREATER THAN TABLE-SIZE
                    MOVE IDX TO IDX-OUT
                    DISPLAY IDX-OUT "|" R IN BLANK-TABLE(IDX) "|"
           END-PERFORM
           DISPLAY "  +" LINE-HEADER "+"
           DISPLAY SPACES.
      *
       150-PROCESS-USER-INPUT.
      *
      * This paragraph processes the user's alphanumeric coordinates
      * into X,Y numeric table coordinates.  It then uses those numeric
      * table coordinates to retrieve the two symbols stored in those
      * locations of CARD-TABLE.  Those two symbols are stored in
      * corresponding locations of BLANK-TABLE for display.  Then, the
      * two symbols are compared for equality.
      *
      * Convert user's alphanumeric coordinates into X,Y coordinates
           COMPUTE X-LOC-1 = FUNCTION ORD(LOCATION-1-COORD(1:1)) - 193
           MOVE LOCATION-1-COORD(2:1) TO Y-LOC-1
           COMPUTE X-LOC-2 = FUNCTION ORD(LOCATION-2-COORD(1:1)) - 193
           MOVE LOCATION-2-COORD(2:1) TO Y-LOC-2
      *
      * Display results for debugging purposes
           DISPLAY SPACES
           DISPLAY "                   X, Y"
           DISPLAY "LOCATION #1: " LOCATION-1-COORD " = " X-LOC-1
                    "," Y-LOC-1
           DISPLAY "LOCATION #2: " LOCATION-2-COORD " = " X-LOC-2
                    "," Y-LOC-2
           DISPLAY SPACES
      *
      * Copy the two symbols to corresponding locations in BLANK-TABLE
	  * for display to the user
           MOVE C IN CARD-TABLE(Y-LOC-1, X-LOC-1) TO
                 C IN BLANK-TABLE(Y-LOC-1, X-LOC-1)
           MOVE C IN CARD-TABLE(Y-LOC-2, X-LOC-2) TO
                 C IN BLANK-TABLE(Y-LOC-2, X-LOC-2).
