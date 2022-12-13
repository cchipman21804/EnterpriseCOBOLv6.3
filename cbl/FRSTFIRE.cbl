       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FRSTFIRE.
       AUTHOR.         CHIPMAN.
      *
      * Implement the Drossel and Schwabl definition of the forest-fire
      * model.
      *
      * It is basically a 2D cellular automaton where each cell can be
      * in three distinct states (empty, tree and burning) and evolves
      * according to the following rules (as given by Wikipedia)
      *
      * 1. A burning cell turns into an empty cell
      * 2. A tree will burn if at least one neighbor is burning
      * 3. A tree ignites with probability   f   even if no neighbor is
      *    burning
      * 4. An empty space fills with a tree with probability   p
      *
      * Neighborhood is the Moore neighborhood. Boundary conditions are
      * so that on the boundary the cells are always empty ("fixed"
      * boundary condition).
      *
      * At the beginning, populate the lattice with empty and tree cells
      * according to a specific probability (e.g. a cell has the
      * probability 0.5 to be a tree). Then, let the system evolve.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * Game constants
       01 max-rows                                  PIC 99  VALUE 40.
       01 max-columns                               PIC 999 VALUE 80.
       01 max-gen                                   PIC 999 VALUE 357.
      *
       01 tree                                      PIC X   VALUE "T".
       01 empty                                     PIC X   VALUE SPACE.
       01 burning                                   PIC X   VALUE "B".
      *
      * Game probabilities
      * --- a tree ignites with this probability even if no neighbor
      * is burning
       01 fire-prob                                 PIC 9v999
                                                            VALUE 0.05.
       01 fp-out                                    PIC Z9.9.
      *
      * --- an empty space fills with a tree with this probability
       01 grow-prob                                 PIC 9v999
                                                            VALUE 0.04.
       01 gp-out                                    PIC Z9.9.
      *
      * --- at the beginning, populate the lattice with empty and tree
      * cells according to a specific probability
       01 init-prob                                 PIC 9V999
                                                            VALUE 0.3.
      *
       01 previous-generation.
           02 pg-row       OCCURS 1 TO 60 TIMES
                           DEPENDING ON max-rows.
              03 pg-column OCCURS 1 TO 120 TIMES
                           DEPENDING ON max-columns PIC X VALUE SPACE.
      *
      * 01 next-generation.
      *     02 ng-row       OCCURS 1 TO 60 TIMES
      *                     DEPENDING ON max-rows.
      *        03 ng-column OCCURS 1 TO 120 TIMES
      *                     DEPENDING ON max-columns PIC X.
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
      * Change to PIC X for STRING?
      * 01 datetime.
      *     02 yyyy      pic 9(4).
      *     02 mo        pic 99.
      *     02 dd        pic 99.
      *     02 hh        pic 99.
      *     02 mm        pic 99.
      *     02 ss        pic 99.
      *     02 hund-sec  pic 99.
      *     02 plsormns  pic x.
      *     02 tzh       pic 99.
      *     02 tzm       pic 99.
      *
       01 datetime.
           02 yyyy      pic X(4).
           02 mo        pic XX.
           02 dd        pic XX.
           02 hh        pic XX.
           02 mm        pic XX.
           02 ss        pic XX.
           02 hund-sec  pic XX.
           02 plsormns  pic x.
           02 tzh       pic XX.
           02 tzm       pic 99.
      *
      * Create from STRING datetime variables together
       01 seed          pic x(8).
      *
      * random-num & random-int store the generated random number
       01 factor        pic 9v999.
       01 hs            pic 99.
       01 random-num    pic 9v9(10).
      * 01 random-int    pic 9.
       01 random-seed   pic 9(8).
      *
       PROCEDURE DIVISION.
       100-MAIN.
           PERFORM 110-INITIALIZE
           PERFORM 111-DISP-PARMS
           PERFORM VARYING generation FROM 1 BY 1
                    UNTIL generation IS GREATER THAN max-gen
                 PERFORM 120-DISP-GEN
                 PERFORM 130-EVOLVE-NG
           END-PERFORM
           STOP RUN.
      *
       110-INITIALIZE.
      *
      * Randomzize using system time
           MOVE FUNCTION current-date TO datetime
           STRING hund-sec ss mm hh
                    DELIMITED BY SIZE
                    INTO seed
           END-STRING
           MOVE seed TO random-seed
           MOVE hund-sec to hs
           DIVIDE hs by 1000 GIVING factor
           COMPUTE random-num = FUNCTION RANDOM(random-seed)
      *
           PERFORM VARYING row-counter FROM 2 BY 1
                    UNTIL row-counter IS EQUAL TO  max-rows
              PERFORM VARYING column-counter FROM 2 BY 1
                       UNTIL column-counter IS EQUAL TO max-columns
      *
      * Populate orchard
                 PERFORM 210-CALCULATE-PROB
                 IF random-num IS LESS THAN init-prob THEN
                    MOVE tree TO pg-column (row-counter, column-counter)
                 END-IF
              END-PERFORM
           END-PERFORM.
      *
       111-DISP-PARMS.
           MULTIPLY 100 BY fire-prob GIVING fp-out
           MULTIPLY 100 BY grow-prob GIVING gp-out
           DISPLAY "RANDOM SEED     : " random-seed
           DISPLAY "FIRE PROBABILITY: " fp-out "%"
           DISPLAY "GROW PROBABILITY: " gp-out "%".
      *     DISPLAY "INIT PROBABILITY: " init-prob.
      *
       120-DISP-GEN.
           DISPLAY SPACES
           DISPLAY "Generation: " generation
           PERFORM VARYING row-counter FROM 1 BY 1
              UNTIL row-counter IS GREATER THAN max-rows
              DISPLAY pg-row(row-counter)
           END-PERFORM.
      *
       130-EVOLVE-NG.
      *
      * Copy contents of previous generation to next generation
      *     MOVE previous-generation TO next-generation
      *
      * EVOLVE AS FOLLOWS:
      *
      * 1. A burning cell turns into an empty cell
      * 2. A tree will burn if at least one neighbor is burning
      * 3. A tree ignites with probability   f   even if no neighbor is
      *    burning
      * 4. An empty space fills with a tree with probability   p
      *
           PERFORM VARYING row-counter FROM 1 BY 1
                    UNTIL row-counter IS GREATER THAN max-rows
              PERFORM VARYING column-counter FROM 1 BY 1
                       UNTIL column-counter IS GREATER THAN max-columns
                 MOVE ZERO TO neighbor-counter
                 EVALUATE pg-column(row-counter, column-counter)
                    WHEN burning
                       PERFORM 131-EVOLVE-BURNING-CELL
                    WHEN tree
                       PERFORM 132-EVOLVE-TREE-CELL
                    WHEN empty
                       PERFORM 133-EVOLVE-EMPTY-CELL
                 END-EVALUATE
              END-PERFORM
           END-PERFORM.
      * Copy next-generation to previous-generation for display and
      * evaluation of next generation
      *     MOVE next-generation TO previous-generation.
      *
       131-EVOLVE-BURNING-CELL.
      *
      * 1. A burning cell turns into an empty cell
           MOVE empty TO pg-column(row-counter, column-counter).
      *
       132-EVOLVE-TREE-CELL.
      *
      * 2. A tree will burn if at least one neighbor is burning
      * 3. A tree ignites with probability   f   even if no neighbor is
      *    burning
      *
           PERFORM 200-COUNT-BURNING-NEIGHBORS
           PERFORM 210-CALCULATE-PROB
           IF neighbor-counter IS GREATER THAN ZERO THEN
              PERFORM 220-IGNITE-TREE
           ELSE
              EVALUATE TRUE
                 WHEN random-num IS LESS THAN fire-prob
                    PERFORM 220-IGNITE-TREE
              END-EVALUATE
           END-IF.
      *
       133-EVOLVE-EMPTY-CELL.
      *
      * 4. An empty space fills with a tree with probability   p
           PERFORM 210-CALCULATE-PROB
           EVALUATE TRUE
              WHEN random-num IS LESS THAN grow-prob
                 MOVE tree TO pg-column(row-counter, column-counter)
           END-EVALUATE.
      *
       200-COUNT-BURNING-NEIGHBORS.
           PERFORM VARYING y FROM -1 BY 1 UNTIL y IS GREATER THAN 1
              PERFORM VARYING x FROM -1 BY 1 UNTIL x IS GREATER THAN 1
      * Test for x = 0 AND y = 0 (current cell),
      * If true, skip it and move on
                 IF x IS NOT EQUAL TO ZERO OR
                    y IS NOT EQUAL TO ZERO
                 THEN
                    ADD y TO row-counter GIVING neighbor-y
                    ADD x TO column-counter GIVING neighbor-X
                    EVALUATE TRUE
                       WHEN pg-column(neighbor-y, neighbor-x)
                             IS EQUAL TO burning
                             ADD 1 TO neighbor-counter
                    END-EVALUATE
                 END-IF
              END-PERFORM
           END-PERFORM.
      *
       210-CALCULATE-PROB.
      *
           COMPUTE random-num = FUNCTION RANDOM
           ADD factor TO random-num.
      *     COMPUTE random-int = FUNCTION INTEGER (random-num * 10).
      *
       220-IGNITE-TREE.
      *
           MOVE burning TO pg-column(row-counter, column-counter).
      *
