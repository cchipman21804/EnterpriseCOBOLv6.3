       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ELEVATOR.
       AUTHOR.        CHIPMAN.
      *
      * THIS PROGRAM SIMULATES THE FLOOR DISPLAY WITHIN AN ELEVATOR
      * CAR AFTER THE OCCUPANT PRESSES A FLOOR BUTTON.
      *
      * PRESSING THE 'O'PEN DOOR BUTTON ENDS THE SIMULATION
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 LOOP-COUNTER                               PIC 9.
       01 ELEVATOR-CMD                               PIC X.
       01 TOP-FLOOR                                  PIC 9  VALUE 9.
       01 BOTTOM-FLOOR                               PIC 9  VALUE 1.
       01 CURRENT-FLOOR                              PIC 9.
       01 DESIRED-FLOOR                              PIC 9.
       01 UP-DOWN-STEP                               PIC S9 VALUE ZERO
                                     SIGN IS LEADING SEPARATE CHARACTER.
          88 GOING-UP                                       VALUE 1.
          88 GOING-DOWN                                     VALUE -1.
      *
       01 CLOSED-ELEVATOR-CAR.
          05 FILLER                                  PIC X(9)
                                                     VALUE "+-------+".

          05 FILLER                                  PIC X(9)
                                                     VALUE "|   |   |".

          05 FILLER                                  PIC X(9)
                                                     VALUE "|   |   |".

          05 FILLER                                  PIC X(9)
                                                     VALUE "|   |   |".

          05 FILLER                                  PIC X(9)
                                                     VALUE "|   |   |".

          05 FILLER                                  PIC X(9)
                                                     VALUE "|   |   |".

          05 FILLER                                  PIC X(9)
                                                     VALUE "+-------+".

       01 FILLER REDEFINES CLOSED-ELEVATOR-CAR.
          05 CLOSED-CAR OCCURS 7 TIMES                      PIC X(9).
      *
       01 FLOOR-NUMBER-GRAPHIC.
          05 FILLER                                  PIC XX VALUE "||".
          05 FILLER                                  PIC XX
                                                     VALUE SPACES.
          05 FLOOR-NUMBER                            PIC X.
          05 FILLER                                  PIC XX
                                                     VALUE SPACES.
          05 FILLER                                  PIC XX VALUE "||".
      *
       01 OPEN-ELEVATOR-CAR.
          05 FILLER                                  PIC X(9)
                                                     VALUE "+-------+".

          05 FILLER                                  PIC X(9)
                                                     VALUE "||     ||".

          05 FILLER                                  PIC X(9)
                                                     VALUE "||     ||".

          05 FLOOR-GRAPHIC                           PIC X(9)
                                                     VALUE SPACES.

          05 FILLER                                  PIC X(9)
                                                     VALUE "||     ||".

          05 FILLER                                  PIC X(9)
                                                     VALUE "||     ||".

          05 FILLER                                  PIC X(9)
                                                     VALUE "+-------+".

       01 FILLER REDEFINES OPEN-ELEVATOR-CAR.
          05 OPEN-CAR OCCURS 7 TIMES                      PIC X(9).
      *
       PROCEDURE DIVISION.
       MAIN.
      * You get in on the first floor:
           MOVE 1 TO CURRENT-FLOOR
           MOVE CURRENT-FLOOR TO ELEVATOR-CMD
           PERFORM 120-INITIALIZE-ELEVATOR-CAR
           PERFORM WITH TEST AFTER UNTIL ELEVATOR-CMD IS EQUAL TO 'O'
              DISPLAY "You are currently on floor #" CURRENT-FLOOR
              DISPLAY "Please press an elevator button to continue: (1-"
              "9 or [O]pen)"
              ACCEPT ELEVATOR-CMD
              EVALUATE TRUE
              WHEN ELEVATOR-CMD IS ALPHABETIC
                 MOVE FUNCTION UPPER-CASE(ELEVATOR-CMD) TO ELEVATOR-CMD
                 IF ELEVATOR-CMD IS EQUAL TO 'O' THEN
                      DISPLAY SPACES
                      PERFORM 110-DISPLAY-OPEN-ELEVATOR
                      DISPLAY 'Elevator car is open. Returning to z/OS.'
                 END-IF

              WHEN ELEVATOR-CMD IS NUMERIC AND
                   ELEVATOR-CMD IS GREATER THAN ZERO
                 PERFORM 120-INITIALIZE-ELEVATOR-CAR
                 PERFORM 100-DISPLAY-CLOSED-ELEVATOR
                 DISPLAY SPACES
                 EVALUATE TRUE
                 WHEN DESIRED-FLOOR IS GREATER THAN TOP-FLOOR OR
                      DESIRED-FLOOR IS LESS THAN BOTTOM-FLOOR
                    DISPLAY "Those floors are not available from this"
                             "elevator."
                 WHEN DESIRED-FLOOR IS EQUAL TO CURRENT-FLOOR
                    DISPLAY "You are currently on floor #" DESIRED-FLOOR
                    MOVE ZERO TO UP-DOWN-STEP

                 WHEN DESIRED-FLOOR IS GREATER THAN CURRENT-FLOOR
                    MOVE 1 TO UP-DOWN-STEP

                 WHEN DESIRED-FLOOR IS LESS THAN CURRENT-FLOOR
                    MOVE -1 TO UP-DOWN-STEP

                 END-EVALUATE
                 PERFORM WITH TEST AFTER
                          VARYING LOOP-COUNTER
                          FROM CURRENT-FLOOR BY UP-DOWN-STEP
                          UNTIL LOOP-COUNTER IS EQUAL TO DESIRED-FLOOR
                          DISPLAY LOOP-COUNTER
                 END-PERFORM
                 MOVE LOOP-COUNTER TO CURRENT-FLOOR
                 DISPLAY SPACES
                 PERFORM 110-DISPLAY-OPEN-ELEVATOR

              END-EVALUATE
           END-PERFORM
           STOP RUN.
      *
       100-DISPLAY-CLOSED-ELEVATOR.
              PERFORM VARYING LOOP-COUNTER FROM 1 BY 1
                      UNTIL LOOP-COUNTER IS GREATER THAN 7
                 DISPLAY CLOSED-CAR(LOOP-COUNTER)
              END-PERFORM.
      *
       110-DISPLAY-OPEN-ELEVATOR.
              PERFORM VARYING LOOP-COUNTER FROM 1 BY 1
                      UNTIL LOOP-COUNTER IS GREATER THAN 7
                 DISPLAY OPEN-CAR(LOOP-COUNTER)
              END-PERFORM.
      *
       120-INITIALIZE-ELEVATOR-CAR.
                 MOVE ELEVATOR-CMD TO DESIRED-FLOOR
                 MOVE DESIRED-FLOOR TO
                      FLOOR-NUMBER IN FLOOR-NUMBER-GRAPHIC
                 MOVE FLOOR-NUMBER-GRAPHIC TO
                      FLOOR-GRAPHIC IN OPEN-ELEVATOR-CAR.
