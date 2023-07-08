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
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SYMBOL-TABLE-REPORT RECORDING MODE F.
       01 REPORT-RECORD                           PIC X(80).
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
      *
      * The difficulty is an integer from 1 to 9.  It determines the
      * dimensions of the two-dimentional table.  TABLE-SIZE is
      * computed by adding 2 to difficulty.  This produces a 2-D table
      * from 3x3 to 11x11.
      *
       01 DIFFICULTY                              PIC 9     VALUE 9.
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
       01 NUMBER-OF-SYMBOLS                       PIC 99    VALUE 95.
      *
      * Stores the symbols which are available for population. Replace
      * used symbols with a space character.
       01 AVAILABLE-SYMBOLS.
           05 SYM  OCCURS 1 TO 99 TIMES DEPENDING ON NUMBER-OF-SYMBOLS
                                                  PIC X.

       01 IDX                                     PIC 99    VALUE 1.
       01 LOOP-COUNTER                            PIC 999.
      *
      * Stores the contents of the current random deck
       01 CARD-TABLE.
           05 r     OCCURS 3 to 11 TIMES DEPENDING ON TABLE-SIZE.
              10 c  OCCURS 3 to 11 TIMES DEPENDING ON TABLE-SIZE
                                                  PIC X     VALUE SPACE.
      *
      * Used for display
       01 BLANK-TABLE.
           05 r     OCCURS 3 to 11 TIMES DEPENDING ON TABLE-SIZE.
              10 c  OCCURS 3 to 11 TIMES DEPENDING ON TABLE-SIZE
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
      * MAX is the maximum random number I want to generate
       01 MAX                                  PIC 99.
       01 RANDOM-SYMBOL-IDX                    PIC 99.
       01 RANDOM-X                             PIC 99.
       01 RANDOM-Y                             PIC 99.
      *
       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT SYMBOL-TABLE-REPORT
           PERFORM 100-INITIALIZE
           PERFORM 110-GENERATE-SEEDS
      *
      * Populate the 2-D table CARD-TABLE with the randomly generated
      * symbols. Place the symbols in two random locations WHICH ARE
      * ALREADY EMPTY (SPACE)
      *
           PERFORM 121-POPULATE-CARD-TABLE
           PERFORM 130-DISPLAY-CARD-TABLE
           CLOSE SYMBOL-TABLE-REPORT
           STOP RUN.

       100-INITIALIZE.
      *
      * Set TABLE-SIZE based on the value stored in DIFFICULTY
           ADD 2 TO DIFFICULTY GIVING TABLE-SIZE
      *
      *     ******************** CAUTION ********************
      * The PERFORM UNTIL statements (DO...UNTIL LOOP) in other
      * paragraphs can create infinite loops if the LOOP-COUNTER
      * calculation within this paragraph is incorrect!
      *
      * Calculate LOOP-COUNTER based on EVEN/ODD result of:
      * 1/2 of TABLE-SIZE SQUARED
      *
      * LOOP-COUNTER needs to be an even number AND less than the value
      * of TABLE-SIZE SQUARED in order for the POPULATE-CARD-TABLE
      * paragraph to work properly without errors and without causing
      * an infinite loop
      *
           COMPUTE LOOP-COUNTER = TABLE-SIZE * TABLE-SIZE * 0.5
      * If LOOP-COUNTER is odd, use the next lowest even number
           IF FUNCTION MOD(LOOP-COUNTER 2) IS NOT EQUAL TO ZERO THEN
              SUBTRACT 1 FROM LOOP-COUNTER
           END-IF
      *
      * Set MAX to NUMBER-OF-SYMBOLS.  MAX helps generate a random index
      * to pick a particular symbol from that 1-D table.
           MOVE NUMBER-OF-SYMBOLS TO MAX
      *
      * Populate SYMBOL table with printable symbols --
           MOVE ALL-SYMBOLS TO AVAILABLE-SYMBOLS.

       110-GENERATE-SEEDS.
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
      * Initialize seeded pseudo-random number sequences
           COMPUTE RANDOM-SYMBOL-IDX = FUNCTION INTEGER (
                                       FUNCTION RANDOM(SEED) * MAX + 1)

           COMPUTE RANDOM-X = FUNCTION INTEGER (
                              FUNCTION RANDOM(SEED) * TABLE-SIZE + 1)

           COMPUTE RANDOM-Y = FUNCTION INTEGER (
                              FUNCTION RANDOM(SEED) * TABLE-SIZE + 1).
      *
      * This paragraph populates 2 empty cells of CARD-TABLE with a
      * random symbol
       121-POPULATE-CARD-TABLE.
      * 1/2 of TABLE-SIZE SQUARED NEEDS TO BE AN EVEN NUMBER FOR THIS
      * ALGORITHM TO WORK PROPERLY.
           PERFORM LOOP-COUNTER TIMES
      *
      *     ******************** CAUTION ********************
      * These PERFORM UNTIL statements (DO...UNTIL LOOP) can create
      * infinite loops if the LOOP-COUNTER calculation within the
      * 100-INITIALIZE paragraph is incorrect!
      *
      * If all of the printable symbols are used up, this condition
      * will trigger an infinite loop as the CPU continues to search
      * for a non-existent printable character within the table.
      *
              PERFORM WITH TEST AFTER
                 UNTIL SYM(RANDOM-SYMBOL-IDX) IS NOT EQUAL TO SPACE
                 COMPUTE RANDOM-SYMBOL-IDX = FUNCTION INTEGER (
                                             FUNCTION RANDOM * MAX + 1)
              END-PERFORM
      *
      * The value of IDX determines whether to log RANDOM-X & RANDOM-Y
      * in CURRENT-X1 & CURRENT-Y1 or in CURRENT-X2 & CURRENT-Y2 in
      * the CHEATSHT report.
              MOVE 1 TO IDX
              PERFORM 2 TIMES
      *
      *     ******************** CAUTION ********************
      * These PERFORM UNTIL statements (DO...UNTIL LOOP) can create
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
      * This paragraph displays the contents of CARD-TABLE with row
      * and column headers
       130-DISPLAY-CARD-TABLE.
           DISPLAY "   00000000011"
           DISPLAY "   12345678901"
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL IDX IS GREATER THAN TABLE-SIZE
                   DISPLAY IDX " " R IN CARD-TABLE(IDX)
           END-PERFORM.
