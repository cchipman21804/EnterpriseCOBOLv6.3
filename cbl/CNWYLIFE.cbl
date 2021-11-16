       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CNWYLIFE.
       AUTHOR.         CHIPMAN.
      *
      * This program replicateS the rules of Conway's Game of Life:
      *
      * The universe of the Game of Life is an infinite, two-dimensional
      * orthogonal grid of square cells, each of which is in one of two
      * possible states, live or dead, (or populated and unpopulated,
      * respectively). Every cell interacts with its eight neighbours,
      * which are the cells that are horizontally, vertically, or
      * diagonally adjacent. At each step in time, the following
      * transitions occur:
      *
      * 1. Any live cell with fewer than two live neighbors dies, as if
      *    by underpopulation.
      *
      * 2. Any live cell with two or three live neighbors lives on to
      *    the next generation.
      *
      * 3. Any live cell with more than three live neighbors dies, as if
      *    by overpopulation.
      *
      * 4. Any dead cell with exactly three live neighbors becomes a
      *    live cell, as if by reproduction.
      *
      * https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
      *
      * The grid is 78 columns wide by 38 rows high.  The initial
      * conditions are set by the contents of initial-condx-file.
      * The grid is surrounded by boundary cells which the algorithm
      * views as unchangable dead cells.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT initial-condx-file
           ASSIGN TO INITLIFE
           ORGANIZATION IS SEQUENTIAL.
      *
      * //CNWYLIFE JOB 1,NOTIFY=&SYSUID
      * //***************************************************/
      * //COBRUN  EXEC IGYWCL
      * //COBOL.SYSIN  DD DSN=&SYSUID..CBL(CNWYLIFE),DISP=SHR
      * //LKED.SYSLMOD DD DSN=&SYSUID..LOAD(CNWYLIFE),DISP=SHR
      * //***************************************************/
      * // IF RC = 0 THEN
      * //***************************************************/
      * //RUN     EXEC PGM=CNWYLIFE
      * //STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
      * //INITLIFE  DD DSN=&SYSUID..CONWAYIN,DISP=SHR
      * //SYSOUT    DD SYSOUT=*,OUTLIM=15000
      * //CEEDUMP   DD DUMMY
      * //SYSUDUMP  DD DUMMY
      * //***************************************************/
      * // ELSE
      * // ENDIF
      *
       DATA DIVISION.
       FILE SECTION.
       FD initial-condx-file RECORDING MODE F.
       01 initial-condx-record                      PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
      * Game constants
       01 max-rows                                  PIC 99  VALUE 40.
       01 max-columns                               PIC 99  VALUE 80.
       01 max-gen                                   PIC 999 VALUE 357.
      * 357 generations on an 80x40 grid uses 14,993 lines on SYSOUT.
       01 living-cell                               PIC X   VALUE "O".
       01 dead-cell                                 PIC X   VALUE SPACE.
       01 boundary                                  PIC X   VALUE "X".
      *
       01 previous-generation.
           02 pg-row       OCCURS 1 TO 40 TIMES
                           DEPENDING ON max-rows.
              03 pg-column OCCURS 1 TO 80 TIMES
                           DEPENDING ON max-columns PIC X.
      *
       01 next-generation.
           02 ng-row       OCCURS 1 TO 40 TIMES
                           DEPENDING ON max-rows.
              03 ng-column OCCURS 1 TO 80 TIMES
                           DEPENDING ON max-columns PIC X.
      *
       01 generation                                PIC 999 VALUE 1.
       01 row-counter                               PIC 99  VALUE 1.
       01 column-counter                            PIC 99  VALUE 1.
       01 neighbor-x                                PIC 99.
       01 neighbor-y                                PIC 99.
       01 x                                         PIC S9
                                               SIGN IS LEADING SEPARATE.
       01 y                                         PIC S9
                                               SIGN IS LEADING SEPARATE.
       01 neighbor-counter                          PIC 9   VALUE ZERO.
      *
       01 end-of-file-flag                          PIC X.
           88 EOF                                           VALUE "T".
      *
       PROCEDURE DIVISION.
       100-MAIN.
           MOVE "F" TO end-of-file-flag
           OPEN INPUT initial-condx-file
           PERFORM 110-INITIALIZE VARYING row-counter FROM 1 BY 1
                     UNTIL row-counter IS GREATER THAN max-rows
           CLOSE initial-condx-file
           PERFORM VARYING generation FROM 1 BY 1
                    UNTIL generation IS GREATER THAN max-gen
              PERFORM 120-DISP-GEN
              PERFORM 130-CONWAY-ALG
           END-PERFORM
           STOP RUN.
      *
       110-INITIALIZE.
           READ initial-condx-file
              AT END MOVE "T" TO end-of-file-flag
           END-READ
           MOVE initial-condx-record to pg-row(row-counter).
      *
       120-DISP-GEN.
      * Display each generation
           DISPLAY "Generation: " generation
           PERFORM VARYING row-counter FROM 1 BY 1
              UNTIL row-counter IS GREATER THAN max-rows
           DISPLAY pg-row(row-counter)
           END-PERFORM
           DISPLAY SPACES.
      *
       130-CONWAY-ALG.
      * Copy previous=generation to next-generation
           MOVE previous-generation to next-generation
      * Evaluate all cells in sequence, but skip boundary cells
           PERFORM VARYING row-counter FROM 2 BY 1
                 UNTIL row-counter IS EQUAL TO max-rows
           PERFORM VARYING column-counter FROM 2 BY 1
                 UNTIL column-counter IS EQUAL TO max-columns
           MOVE ZERO TO neighbor-counter
      * Begin counting living neighbors based on whether current cell
      * is living or dead
           EVALUATE pg-column (row-counter, column-counter)
              WHEN living-cell
                 PERFORM 210-LIVING-CELL-NEIGHBORS
              WHEN dead-cell
                 PERFORM 220-DEAD-CELL-NEIGHBORS
           END-EVALUATE
           END-PERFORM
           END-PERFORM
      * Copy next-generation to previous-generation for display and
      * evaluation of next generation
           MOVE next-generation TO previous-generation.
      *
       200-COUNT-LIVING-NEIGHBORS.
           PERFORM VARYING y FROM -1 BY 1 UNTIL y IS GREATER THAN 1
           PERFORM VARYING x FROM -1 BY 1 UNTIL x IS GREATER THAN 1
      * BUG: If CURRENT cell is LIVING AND
      *      x = 0 AND y = 0
      *      (neighbor coordinates point to current cell)
      *      current cell gets counted as a neighbor!!!
      * Test for x = 0 AND y = 0,
      * if true, skip it and move on
           IF x IS NOT EQUAL TO ZERO OR
              y IS NOT EQUAL TO ZERO
           THEN
              ADD y TO row-counter GIVING neighbor-y
              ADD x to column-counter GIVING neighbor-x
              IF pg-column(neighbor-y, neighbor-x)
                 IS EQUAL TO living-cell
              THEN
                 ADD 1 TO neighbor-counter
              END-IF
           END-IF
           END-PERFORM
           END-PERFORM.
      *
       210-LIVING-CELL-NEIGHBORS.
           PERFORM 200-COUNT-LIVING-NEIGHBORS
           EVALUATE TRUE
              WHEN (neighbor-counter = 2) OR (neighbor-counter = 3)
                 MOVE living-cell
                 TO ng-column(row-counter, column-counter)

              WHEN OTHER
                 MOVE dead-cell
                 TO ng-column(row-counter, column-counter)

           END-EVALUATE.
      *
       220-DEAD-CELL-NEIGHBORS.
           PERFORM 200-COUNT-LIVING-NEIGHBORS
           EVALUATE TRUE
              WHEN neighbor-counter = 3
                 MOVE living-cell
                 TO ng-column(row-counter, column-counter)

              WHEN OTHER
                 MOVE dead-cell
                 TO ng-column(row-counter, column-counter)
           END-EVALUATE.
      *
      * The universe of the Game of Life is an infinite, two-dimensional
      * orthogonal grid of square cells, each of which is in one of two
      * possible states, live or dead, (or populated and unpopulated,
      * respectively). Every cell interacts with its eight neighbours,
      * which are the cells that are horizontally, vertically, or
      * diagonally adjacent. At each step in time, the following
      * transitions occur:
      *
      * 1. Any live cell with fewer than two live neighbors dies, as if
      *    by underpopulation.
      *
      * 2. Any live cell with two or three live neighbors lives on to
      *    the next generation.
      *
      * 3. Any live cell with more than three live neighbors dies, as if
      *    by overpopulation.
      *
      * 4. Any dead cell with exactly three live neighbors becomes a
      *    live cell, as if by reproduction.
      *
      * https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
      *
