       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CNCNTRAT.
       AUTHOR.         CHIPMAN.
      *
      * This program plays the children's game "Concentration", also
      * known as the card game "Memory".  Find matches by uncovering
      * two cells in the 2-D table.
      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * Dr. Seay - This approach is still full of bugs and is
      * incomplete.  I am certain that you and your students can find
      * a much better way to build this project.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * The difficulty is an integer from 1 to 9.  It determines the
      * dimensions of the two-dimentional table.  TABLE-SIZE is
      * computed by adding 2 to difficulty.  This produces a 2-D table
      * from 3x3 to 11x11.
      *
       01 DIFFICULTY                              PIC 9 VALUE 9.
       01 TABLE-SIZE                              PIC 99.
      *
      * Stores every printable symbol used in the game & provides a
      * convenient way to index each symbol.
      *01 ALL-SYMBOLS.
      *    05 SYM  OCCURS 1 TO 98 TIMES DEPENDING ON NUMBER-OF-SYMBOLS
      *                                           PIC X.
      *
      * Stores the symbols which are available for population. Replace
      * used symbols with a space character.
       01 AVAILABLE-SYMBOLS.
           05 SYM  OCCURS 1 TO 98 TIMES DEPENDING ON NUMBER-OF-SYMBOLS
                                                  PIC X.

       01 NUMBER-OF-SYMBOLS                       PIC 99 VALUE 71.
       01 IDX                                     PIC 99 VALUE 1.
       01 LOOP-COUNTER                            PIC 99.
      *
      * Stores the contents of the current random deck
       01 CARD-TABLE.
           05 r     OCCURS 3 to 12 TIMES DEPENDING ON TABLE-SIZE.
              10 c  OCCURS 3 to 12 TIMES DEPENDING ON TABLE-SIZE
                                                  PIC X VALUE SPACE.
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
      *
      * Set TABLE-SIZE based on the value stored in DIFFICULTY
           ADD 2 TO DIFFICULTY GIVING TABLE-SIZE
      *
      * Set LOOP-COUNTER based on EVEN/ODD of TABLE-SIZE SQUARED
           COMPUTE LOOP-COUNTER = TABLE-SIZE * TABLE-SIZE
      *
	  * Display LOOP-COUNTER variable for debugging purposes
           DISPLAY LOOP-COUNTER
      *
           IF FUNCTION MOD(LOOP-COUNTER 2) IS NOT EQUAL TO ZERO THEN
              SUBTRACT 1 FROM LOOP-COUNTER
           END-IF
      *
	  * Display LOOP-COUNTER variable for debugging purposes
           DISPLAY LOOP-COUNTER
      *
      * Set MAX to NUMBER-OF-SYMBOLS.  MAX helps generate a random index
      * to pick a particular symbol from that 1-D table.
           MOVE NUMBER-OF-SYMBOLS TO MAX
      *
      * Populate SYMBOL table with printable symbols from JCL --
           ACCEPT AVAILABLE-SYMBOLS
           PERFORM 110-GENERATE-SEEDS
      *
      * DISPLAY contents of SYMBOL for debugging purposes
      *    DISPLAY SYMBOL
      *    PERFORM 120-DISPLAY-RANDOM-SYMBOL
      *
      * Populate the 2-D table CARD-TABLE with the randomly generated
      * symbol. Place the symbol in two random locations WHICH ARE
      * ALREADY EMPTY (SPACE)
      *
           PERFORM 121-POPULATE-CARD-TABLE
      *
           PERFORM 130-DISPLAY-CARD-TABLE
           STOP RUN.

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
                                       FUNCTION RANDOM(SEED) * MAX)

           COMPUTE RANDOM-X = FUNCTION INTEGER (
                              FUNCTION RANDOM(SEED) * TABLE-SIZE)

           COMPUTE RANDOM-Y = FUNCTION INTEGER (
                              FUNCTION RANDOM(SEED) * TABLE-SIZE).
      *
      * This paragraph is for debugging purposes only
       120-DISPLAY-RANDOM-SYMBOL.
           COMPUTE RANDOM-SYMBOL-IDX = FUNCTION INTEGER(
                                       FUNCTION RANDOM * MAX)
           DISPLAY SYM(RANDOM-SYMBOL-IDX).
      *
      * This paragraph populates 2 empty cells of CARD-TABLE with a
      * random symbol
       121-POPULATE-CARD-TABLE.
      * TABLE-SIZE * TABLE-SIZE NEEDS TO BE AN EVEN NUMBER FOR THIS
      * ALGORITHM TO WORK PROPERLY.
           PERFORM LOOP-COUNTER TIMES
             COMPUTE RANDOM-SYMBOL-IDX = FUNCTION INTEGER (
                                         FUNCTION RANDOM * MAX)
             PERFORM 2 TIMES
               COMPUTE RANDOM-X = FUNCTION INTEGER (
                                  FUNCTION RANDOM * TABLE-SIZE)

               COMPUTE RANDOM-Y = FUNCTION INTEGER (
                                  FUNCTION RANDOM * TABLE-SIZE)
      *
      * If the cell in CARD-TABLE is an empty space, populate it
      * with the random symbol
               IF C(RANDOM-X, RANDOM-Y) IS EQUAL TO SPACE THEN
                  MOVE SYM(RANDOM-SYMBOL-IDX) TO C(RANDOM-X, RANDOM-Y)
               END-IF
             END-PERFORM
      *
      * Remove used symbol from 1-D table
             MOVE SPACE TO SYM(RANDOM-SYMBOL-IDX)
           END-PERFORM.
      *
      * This paragraph displays the contents of CARD-TABLE
       130-DISPLAY-CARD-TABLE.
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL IDX IS GREATER THAN TABLE-SIZE
                   DISPLAY R(IDX)
           END-PERFORM.
