      *
      *****************************************************************
      * Domestic Hot Water Savings Calculator
      * Written by, Clifford A. Chipman, EMIT
      * January 30, 2021
      * in Enterprise COBOL v6.3 for z/OS
      *****************************************************************
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DHWSAVE.
       AUTHOR.        CHIPMAN.
      *
      *****************************************************************
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *****************************************************************
      *
       01 CONSTANT-DATA.
          02 FUEL-TYPE-MENU-LIMITS.
             03 MIN-MENU                        PIC 9    VALUE ZERO.
             03 MAX-MENU                        PIC 9    VALUE 3.
      *
          02 WATER-TEMPERATURES-LIMITS.
             03 LOWER-CWT                       PIC 99   VALUE 33.
             03 UPPER-CWT                       PIC 99   VALUE 79.
             03 LOWER-HWT                       PIC 99   VALUE 80.
             03 UPPER-HWT                       PIC 999  VALUE 211.
      *
          02 FLOW-RATE-LIMITS.
             03 MIN-FLOW-RATE                   PIC 9V99 VALUE 1.
             03 MAX-FLOW-RATE                   PIC 9V99 VALUE 2.5.
      *
          02 TIME-LIMITS.
             03 MIN-TIME                        PIC 9    VALUE 1.
             03 MAX-TIME                        PIC 99   VALUE 60.
      *
          02 COMBUSTION-EFF-LIMITS.
             03 MIN-EFF                         PIC 9    VALUE 1.
             03 MAX-EFF                         PIC 999  VALUE 100.
      *
          02 FUEL-UNITS.
             03 ELECTRIC-UNIT                   PIC X(3)
                VALUE "kWh".
      *
             03 NATGAS-UNIT                     PIC X(3)
                VALUE "ccf".
      *
             03 PROPANE-UNIT                    PIC X(7)
                VALUE "gallons".
      *
          02 FUEL-UNIT-PRICES.
             03 ELECTRIC-PRICE                  PIC 9V99 VALUE 0.13.
             03 NATGAS-PRICE                    PIC 9V99 VALUE 1.18.
             03 PROPANE-PRICE                   PIC 9V99 VALUE 2.66.
      *
          02 CONVERSION-FACTORS.
             03 BTU-KWH                         PIC 9(4) VALUE 3412.
      * DIVIDE BTUS BY 3412 GIVING KWH
      *
             03 BTU-CCF                         PIC 9(6) VALUE 103700.
      * DIVIDE BTUS BY 103700 GIVING CCF
      *
             03 BTU-GALLONS                     PIC 9(5) VALUE 91452.
      * DIVIDE BTUS BY 91452 GIVING GALLONS OF PROPANE
      *
          02 DATA-ENTRY-LABEL-CONSTANTS.
             03 NOT-NUMERIC                     PIC X(16)
             VALUE " is not numeric.".
      *
             03 TOO-LOW                         PIC X(12)
             VALUE " is too low.".
      *
             03 TOO-HIGH                        PIC X(13)
             VALUE " is too high.".
      *
      *****************************************************************
      *
       01 USER-INPUT                            PIC X(9).
      *
      *****************************************************************
      *
      *   NUMERIC DATA ENTRY FIELDS FOR INITIAL VALUES
      *
       01 COLD-WATER-TEMP                       PIC 99.
      *                                          in degF
       01 INITIAL-CONDITIONS.
      *
          02 INIT-FUEL                          PIC 9.
      * ELECTRICITY, NATURAL GAS, OR PROPANE
      * 1            2               3
      *
          02 INIT-FUEL-PRICE                    PIC 9V99.
      *                                          in $
      *
          02 INIT-CONV-UNIT                     PIC 9(6).
          02 INIT-FUEL-UNIT                     PIC X(7).
          02 INIT-FUEL-COST                     PIC 99V99.
          02 INIT-FUEL-CONSUMED                 PIC 99V999.
      *
          02 INIT-DHW-TEMP                      PIC 999.
      *                                          in degF
      *
          02 INIT-FLOW-RATE                     PIC 9V99.
      *                                          in GPM
      *
          02 INIT-SHOWER-TIME                   PIC 99.
      *                                          in minutes
      *
          02 INIT-COMB-EFF                      PIC 999v999.
      * APPLIANCE FUEL COMBUSTION EFFICIENCY     in %
      *
      *****************************************************************
      *
      * CALCULATED INITIAL VALUES
      *
          02 INIT-USAGE                         PIC 999.
      *      WATER CONSUMPTION                   in gallons
      *
          02 INIT-ENERGY                        PIC 9(6)V9.
      *                                          in BTUS
      *
      *****************************************************************
      *
      *   NUMERIC DATA ENTRY FIELDS FOR NEW VALUES
      *
       01 NEW-CONDITIONS.
      *
          02 NEW-FUEL                           PIC 9.
      * ELECTRICITY, NATURAL GAS, OR PROPANE
      * 1            2               3
      *
          02 NEW-FUEL-PRICE                     PIC 9V99.
      *                                          in $
      *
          02 NEW-CONV-UNIT                      PIC 9(6).
          02 NEW-FUEL-UNIT                      PIC X(7).
          02 NEW-FUEL-COST                      PIC 99V99.
          02 NEW-FUEL-CONSUMED                  PIC 99V999.
      *
          02 NEW-DHW-TEMP                       PIC 999.
      *                                          in degF
      *
          02 NEW-FLOW-RATE                      PIC 9V99.
      *                                          in GPM
      *
          02 NEW-SHOWER-TIME                    PIC 99.
      *                                          in minutes
      *
          02 NEW-COMB-EFF                       PIC 999v999.
      * APPLIANCE FUEL COMBUSTION EFFICIENCY     in %
      *
      *****************************************************************
      *
      * CALCULATED NEW VALUES
      *
          02 NEW-USAGE                          PIC 999.
      *      WATER CONSUMPTION                   IN GALLONS
      *
          02 NEW-ENERGY                         PIC 9(6)V9.
      *                                          IN BTUS
      *
      *****************************************************************
      *
      *   CALCULATED-SAVINGS
      *
       01 WATER-SAVED                           PIC S9(3)V9.
      *                                          IN GALLONS
      *
       01 ENERGY-SAVED                          PIC S9(6)V9.
      *                                          IN BTUS
      *
       01 FUEL-SAVED                            PIC S999V9.
      *
       01 COST-SAVED                            PIC S99V99.
      *
      *****************************************************************
      *
       01 DISPLAYED-REPORT.
          02 COLD-WATER-TEMP-OUT                PIC Z9.
          02 INIT-DHW-TEMP-OUT                  PIC ZZ9.
          02 INIT-FLOW-RATE-OUT                 PIC 9.9.
          02 INIT-SHOWER-TIME-OUT               PIC Z9.
          02 INIT-COMB-EFF-OUT                  PIC ZZ9.
          02 INIT-USAGE-OUT                     PIC ZZ9.9.
          02 INIT-ENERGY-OUT                    PIC ZZZ,ZZ9.9.
          02 INIT-FUEL-CONSUMED-OUT             PIC ZZ9.999.
          02 INIT-FUEL-COST-OUT                 PIC $$9.99.
          02 NEW-DHW-TEMP-OUT                   PIC ZZ9.
          02 NEW-FLOW-RATE-OUT                  PIC 9.9.
          02 NEW-SHOWER-TIME-OUT                PIC Z9.
          02 NEW-COMB-EFF-OUT                   PIC ZZ9.
          02 NEW-USAGE-OUT                      PIC ZZ9.9.
          02 NEW-ENERGY-OUT                     PIC ZZZ,ZZ9.9.
          02 NEW-FUEL-CONSUMED-OUT              PIC ZZ9.999.
          02 NEW-FUEL-COST-OUT                  PIC $$9.99.
          02 WATER-SAVED-OUT                    PIC +ZZ9.9.
          02 ENERGY-SAVED-OUT                   PIC +ZZZ,ZZ9.9.
          02 FUEL-SAVED-OUT                     PIC +ZZ9.9.
          02 COST-SAVED-OUT                     PIC +$$9.99.
          02 PRICE-OUT                          PIC $9.99.
      *
      *****************************************************************
      *
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM 1-OPENING-SCREEN
           PERFORM 2-INIT-DATA-ENTRY
           PERFORM 3-NEW-DATA-ENTRY
           PERFORM 4-DISPLAY-INIT-REPORT
           PERFORM 5-DISPLAY-NEW-REPORT
           PERFORM 6-DISPLAY-SAVINGS-REPORT
           PERFORM 999-END-PROGRAM.
      *
       1-OPENING-SCREEN.
           DISPLAY SPACES
           DISPLAY "** Domestic Hot Water Savings Calculator BEGINS **"
           DISPLAY "Written by, Clifford A. Chipman, EMIT"
           DISPLAY "January 30, 2021"
           DISPLAY SPACES
           DISPLAY "in Enterprise COBOL v6.3 for z/OS"
           DISPLAY SPACES
           DISPLAY "Enter ZERO for any parameter to end the program."
           DISPLAY SPACES.
      *
       2-INIT-DATA-ENTRY.
           PERFORM 21-CW-TEMP
           PERFORM 22-INIT-FUEL
           PERFORM 23-INIT-HW-TEMP
           PERFORM 24-INIT-FLOW-RATE
           PERFORM 25-INIT-SHOWER-TIME
           PERFORM 26-INIT-COMB-EFF.
      *
       3-NEW-DATA-ENTRY.
           PERFORM 32-NEW-FUEL
           PERFORM 33-NEW-HW-TEMP
           PERFORM 34-NEW-FLOW-RATE
           PERFORM 35-NEW-SHOWER-TIME
           PERFORM 36-NEW-COMB-EFF.
      *
       21-CW-TEMP.
           DISPLAY "Cold water temperature limits: " WITH NO ADVANCING
           DISPLAY  LOWER-CWT "-" UPPER-CWT " degF"
           DISPLAY "Enter cold water temperature in degF: "
                   WITH NO ADVANCING
           ACCEPT USER-INPUT

      * DID THE USER ENTER A VALID NUMERIC VALUE?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "Cold water temperature" NOT-NUMERIC
              GO TO 21-CW-TEMP
           ELSE
              COMPUTE COLD-WATER-TEMP = FUNCTION NUMVAL(USER-INPUT)
           END-IF

           EVALUATE TRUE
           WHEN COLD-WATER-TEMP IS EQUAL ZERO PERFORM 999-END-PROGRAM

           WHEN COLD-WATER-TEMP IS LESS THAN LOWER-CWT
                  DISPLAY "Cold water temperature" TOO-LOW
                  GO TO 21-CW-TEMP

           WHEN COLD-WATER-TEMP IS GREATER THAN UPPER-CWT
                  DISPLAY "Cold water temperature" TOO-HIGH
                  GO TO 21-CW-TEMP

           END-EVALUATE.
      *
       22-INIT-FUEL.
           PERFORM 998-FUEL-MENU
           DISPLAY "Enter initial fuel type: " WITH NO ADVANCING
           ACCEPT USER-INPUT
      *
      * DID THE USER ENTER A VALID NUMERIC VALUE?
      *
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              EVALUATE TRUE
                WHEN USER-INPUT = "E" OR USER-INPUT = "e"
                     MOVE 1 TO INIT-FUEL
      *
                WHEN USER-INPUT = "N" OR USER-INPUT = "n"
                     MOVE 2 TO INIT-FUEL
      *
                WHEN USER-INPUT = "P" OR USER-INPUT = "p"
                     MOVE 3 TO INIT-FUEL
      *
                WHEN OTHER
                     DISPLAY "Menu selection" NOT-NUMERIC
                     GO TO 22-INIT-FUEL
      *
              END-EVALUATE
           ELSE
              COMPUTE INIT-FUEL = FUNCTION NUMVAL(USER-INPUT)
           END-IF
      *
           EVALUATE TRUE
                WHEN INIT-FUEL IS EQUAL ZERO PERFORM 999-END-PROGRAM
      *
                WHEN INIT-FUEL IS LESS THAN MIN-MENU
                     DISPLAY "Menu selection" TOO-LOW
                     GO TO 22-INIT-FUEL
      *
                WHEN INIT-FUEL = 1
                     MOVE ELECTRIC-UNIT  TO INIT-FUEL-UNIT
                     MOVE ELECTRIC-PRICE TO INIT-FUEL-PRICE
                     MOVE BTU-KWH        TO INIT-CONV-UNIT
      *
                WHEN INIT-FUEL = 2
                     MOVE NATGAS-UNIT    TO INIT-FUEL-UNIT
                     MOVE NATGAS-PRICE   TO INIT-FUEL-PRICE
                     MOVE BTU-CCF        TO INIT-CONV-UNIT
      *
                WHEN INIT-FUEL = 3
                     MOVE PROPANE-UNIT   TO INIT-FUEL-UNIT
                     MOVE PROPANE-PRICE  TO INIT-FUEL-PRICE
                     MOVE BTU-GALLONS    TO INIT-CONV-UNIT
      *
                WHEN INIT-FUEL IS GREATER THAN MAX-MENU
                     DISPLAY "Menu selection" TOO-HIGH
                     GO TO 22-INIT-FUEL
      *
           END-EVALUATE.
      *
       23-INIT-HW-TEMP.
           DISPLAY SPACES
           DISPLAY "Hot water temperature limits: " WITH NO ADVANCING
           DISPLAY  LOWER-HWT "-" UPPER-HWT " degF"
           DISPLAY "Enter initial hot water temperature in degF: "
                    WITH NO ADVANCING
           ACCEPT USER-INPUT

      * DID THE USER ENTER A VALID NUMERIC VALUE?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "Initial hot water temperature" NOT-NUMERIC
              GO TO 23-INIT-HW-TEMP
           ELSE
              COMPUTE INIT-DHW-TEMP = FUNCTION NUMVAL(USER-INPUT)
           END-IF

           EVALUATE TRUE
           WHEN INIT-DHW-TEMP IS EQUAL ZERO PERFORM 999-END-PROGRAM

           WHEN INIT-DHW-TEMP IS LESS THAN LOWER-HWT
                  DISPLAY "Initial hot water temperature" TOO-LOW
                  GO TO 23-INIT-HW-TEMP

           WHEN INIT-DHW-TEMP IS GREATER THAN UPPER-HWT
                  DISPLAY "Initial hot water temperature" TOO-HIGH
                  GO TO 23-INIT-HW-TEMP

           END-EVALUATE.
      *
       24-INIT-FLOW-RATE.
      *
      * Move flow rate limit values to separate display variables
      * for proper prompt display.
      *
           MOVE MIN-FLOW-RATE TO INIT-FLOW-RATE-OUT
           MOVE MAX-FLOW-RATE TO NEW-FLOW-RATE-OUT
      *
           DISPLAY SPACES
           DISPLAY "Flow rate limits: " WITH NO ADVANCING
           DISPLAY INIT-FLOW-RATE-OUT "-" NEW-FLOW-RATE-OUT " GPM"
           DISPLAY "Enter initial flow rate in GPM: " WITH NO ADVANCING
           ACCEPT USER-INPUT

      * DID THE USER ENTER A VALID NUMERIC VALUE?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "Initial flow rate" NOT-NUMERIC
              GO TO 24-INIT-FLOW-RATE
           ELSE
              COMPUTE INIT-FLOW-RATE = FUNCTION NUMVAL(USER-INPUT)
           END-IF

           EVALUATE TRUE
           WHEN INIT-FLOW-RATE IS EQUAL ZERO PERFORM 999-END-PROGRAM

           WHEN INIT-FLOW-RATE IS LESS THAN MIN-FLOW-RATE
                  DISPLAY "Initial flow rate" TOO-LOW
                  GO TO 24-INIT-FLOW-RATE

           WHEN INIT-FLOW-RATE IS GREATER THAN MAX-FLOW-RATE
                  DISPLAY "Initial flow rate" TOO-HIGH
                  GO TO 24-INIT-FLOW-RATE

           END-EVALUATE.
      *
       25-INIT-SHOWER-TIME.
           DISPLAY SPACES
           DISPLAY "Shower time limits: " WITH NO ADVANCING
           DISPLAY MIN-TIME "-" MAX-TIME " minutes"
           DISPLAY "Enter initial shower time in minutes: "
                    WITH NO ADVANCING
           ACCEPT USER-INPUT

      * DID THE USER ENTER A VALID NUMERIC VALUE?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "Initial shower time" NOT-NUMERIC
              GO TO 25-INIT-SHOWER-TIME
           ELSE
              COMPUTE INIT-SHOWER-TIME = FUNCTION NUMVAL(USER-INPUT)
           END-IF

           EVALUATE TRUE
           WHEN INIT-SHOWER-TIME IS EQUAL ZERO PERFORM 999-END-PROGRAM

           WHEN INIT-FLOW-RATE IS LESS THAN MIN-FLOW-RATE
                  DISPLAY "Initial shower time" TOO-LOW
                  GO TO 25-INIT-SHOWER-TIME

           WHEN INIT-FLOW-RATE IS GREATER THAN MAX-FLOW-RATE
                  DISPLAY "Initial shower time" TOO-HIGH
                  GO TO 25-INIT-SHOWER-TIME

           END-EVALUATE.
      *
       26-INIT-COMB-EFF.
           DISPLAY SPACES
           DISPLAY "Efficiency limits: " WITH NO ADVANCING
           DISPLAY MIN-EFF "-" MAX-EFF "%"
           DISPLAY "Enter initial combustion efficiency in %: "
                    WITH NO ADVANCING
           ACCEPT USER-INPUT

      * DID THE USER ENTER A VALID NUMERIC VALUE?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "Initial combustion efficiency" NOT-NUMERIC
              GO TO 26-INIT-COMB-EFF
           ELSE
              COMPUTE INIT-COMB-EFF = FUNCTION NUMVAL(USER-INPUT)
           END-IF

           EVALUATE TRUE
           WHEN INIT-COMB-EFF IS EQUAL ZERO PERFORM 999-END-PROGRAM

           WHEN INIT-COMB-EFF IS LESS THAN MIN-EFF
                  DISPLAY "Initial combustion efficiency" TOO-LOW
                  GO TO 26-INIT-COMB-EFF

           WHEN INIT-COMB-EFF IS GREATER THAN MAX-EFF
                  DISPLAY "Initial combustion efficiency" TOO-HIGH
                  GO TO 26-INIT-COMB-EFF

           END-EVALUATE.
      *
       32-NEW-FUEL.
           PERFORM 998-FUEL-MENU
           DISPLAY "Enter new fuel type: " WITH NO ADVANCING
           ACCEPT USER-INPUT
      *
      * DID THE USER ENTER A VALID NUMERIC VALUE?
      *
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              EVALUATE TRUE
                WHEN USER-INPUT = "E" OR USER-INPUT = "e"
                     MOVE 1 TO NEW-FUEL
      *
                WHEN USER-INPUT = "N" OR USER-INPUT = "n"
                     MOVE 2 TO NEW-FUEL
      *
                WHEN USER-INPUT = "P" OR USER-INPUT = "p"
                     MOVE 3 TO NEW-FUEL
      *
                WHEN OTHER
                     DISPLAY "Menu selection" NOT-NUMERIC
                     GO TO 32-NEW-FUEL
      *
              END-EVALUATE
           ELSE
              COMPUTE NEW-FUEL = FUNCTION NUMVAL(USER-INPUT)
           END-IF
      *
           EVALUATE TRUE
                WHEN NEW-FUEL IS EQUAL ZERO PERFORM 999-END-PROGRAM
      *
                WHEN NEW-FUEL IS LESS THAN MIN-MENU
                     DISPLAY "Menu selection" TOO-LOW
                     GO TO 32-NEW-FUEL
      *
                WHEN NEW-FUEL = 1
                     MOVE ELECTRIC-UNIT  TO NEW-FUEL-UNIT
                     MOVE ELECTRIC-PRICE TO NEW-FUEL-PRICE
                     MOVE BTU-KWH        TO NEW-CONV-UNIT
      *
                WHEN NEW-FUEL = 2
                     MOVE NATGAS-UNIT    TO NEW-FUEL-UNIT
                     MOVE NATGAS-PRICE   TO NEW-FUEL-PRICE
                     MOVE BTU-CCF        TO NEW-CONV-UNIT
      *
                WHEN NEW-FUEL = 3
                     MOVE PROPANE-UNIT   TO NEW-FUEL-UNIT
                     MOVE PROPANE-PRICE  TO NEW-FUEL-PRICE
                     MOVE BTU-GALLONS    TO NEW-CONV-UNIT
      *
                WHEN NEW-FUEL IS GREATER THAN MAX-MENU
                     DISPLAY "Menu selection" TOO-HIGH
                     GO TO 32-NEW-FUEL
      *
           END-EVALUATE.
      *
       33-NEW-HW-TEMP.
           DISPLAY SPACES
           DISPLAY "Hot water temperature limits: " WITH NO ADVANCING
           DISPLAY  LOWER-HWT "-" UPPER-HWT " degF"
           DISPLAY "Enter new hot water temperature in degF: "
                    WITH NO ADVANCING
           ACCEPT USER-INPUT

      * DID THE USER ENTER A VALID NUMERIC VALUE?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "Hot water temperature" NOT-NUMERIC
              GO TO 33-NEW-HW-TEMP
           ELSE
              COMPUTE NEW-DHW-TEMP = FUNCTION NUMVAL(USER-INPUT)
           END-IF

           EVALUATE TRUE
           WHEN NEW-DHW-TEMP IS EQUAL ZERO PERFORM 999-END-PROGRAM

           WHEN NEW-DHW-TEMP IS LESS THAN LOWER-HWT
                  DISPLAY "New hot water temperature" TOO-LOW
                  GO TO 33-NEW-HW-TEMP

           WHEN NEW-DHW-TEMP IS GREATER THAN UPPER-HWT
                  DISPLAY "New hot water temperature" TOO-HIGH
                  GO TO 33-NEW-HW-TEMP

           END-EVALUATE.
      *
       34-NEW-FLOW-RATE.
      *
      * Move flow rate limit values should already be in separate
      * display variables for proper prompt display.
      *
      *    MOVE MIN-FLOW-RATE TO INIT-FLOW-RATE-OUT
      *    MOVE MAX-FLOW-RATE TO NEW-FLOW-RATE-OUT
      *
           DISPLAY SPACES
           DISPLAY "Flow rate limits: " WITH NO ADVANCING
           DISPLAY INIT-FLOW-RATE-OUT "-" NEW-FLOW-RATE-OUT " GPM"
           DISPLAY "Enter new flow rate in GPM: " WITH NO ADVANCING
           ACCEPT USER-INPUT

      * DID THE USER ENTER A VALID NUMERIC VALUE?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "New flow rate" NOT-NUMERIC
              GO TO 34-NEW-FLOW-RATE
           ELSE
              COMPUTE NEW-FLOW-RATE = FUNCTION NUMVAL(USER-INPUT)
           END-IF

           EVALUATE TRUE
           WHEN NEW-FLOW-RATE IS EQUAL ZERO PERFORM 999-END-PROGRAM

           WHEN NEW-FLOW-RATE IS LESS THAN MIN-FLOW-RATE
                  DISPLAY "New flow rate" TOO-LOW
                  GO TO 34-NEW-FLOW-RATE

           WHEN NEW-FLOW-RATE IS GREATER THAN MAX-FLOW-RATE
                  DISPLAY "New flow rate" TOO-HIGH
                  GO TO 34-NEW-FLOW-RATE

           END-EVALUATE.
      *
       35-NEW-SHOWER-TIME.
           DISPLAY SPACES
           DISPLAY "Shower time limits: " WITH NO ADVANCING
           DISPLAY MIN-TIME "-" MAX-TIME " minutes"
           DISPLAY "Enter new shower time in minutes: "
                    WITH NO ADVANCING
           ACCEPT USER-INPUT

      * DID THE USER ENTER A VALID NUMERIC VALUE?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "New shower time" NOT-NUMERIC
              GO TO 35-NEW-SHOWER-TIME
           ELSE
              COMPUTE NEW-SHOWER-TIME = FUNCTION NUMVAL(USER-INPUT)
           END-IF

           EVALUATE TRUE
           WHEN NEW-SHOWER-TIME IS EQUAL ZERO PERFORM 999-END-PROGRAM

           WHEN NEW-FLOW-RATE IS LESS THAN MIN-FLOW-RATE
                  DISPLAY "New shower time" TOO-LOW
                  GO TO 35-NEW-SHOWER-TIME

           WHEN INIT-FLOW-RATE IS GREATER THAN MAX-FLOW-RATE
                  DISPLAY "Initial shower time" TOO-HIGH
                  GO TO 35-NEW-SHOWER-TIME

           END-EVALUATE.
      *
       36-NEW-COMB-EFF.
           DISPLAY SPACES
           DISPLAY "Efficiency limits: " WITH NO ADVANCING
           DISPLAY MIN-EFF "-" MAX-EFF "%"
           DISPLAY "Enter new combustion efficiency in %: "
                    WITH NO ADVANCING
           ACCEPT USER-INPUT

      * DID THE USER ENTER A VALID NUMERIC VALUE?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "New combustion efficiency" NOT-NUMERIC
              GO TO 36-NEW-COMB-EFF
           ELSE
              COMPUTE NEW-COMB-EFF = FUNCTION NUMVAL(USER-INPUT)
           END-IF

           EVALUATE TRUE
           WHEN NEW-COMB-EFF IS EQUAL ZERO PERFORM 999-END-PROGRAM

           WHEN NEW-COMB-EFF IS LESS THAN MIN-EFF
                  DISPLAY "New combustion efficiency" TOO-LOW
                  GO TO 36-NEW-COMB-EFF

           WHEN NEW-COMB-EFF IS GREATER THAN MAX-EFF
                  DISPLAY "New combustion efficiency" TOO-HIGH
                  GO TO 36-NEW-COMB-EFF

           END-EVALUATE.
      *
       4-DISPLAY-INIT-REPORT.
           MOVE COLD-WATER-TEMP  TO COLD-WATER-TEMP-OUT
           MOVE INIT-DHW-TEMP    TO INIT-DHW-TEMP-OUT
           MOVE INIT-FLOW-RATE   TO INIT-FLOW-RATE-OUT
           MOVE INIT-SHOWER-TIME TO INIT-SHOWER-TIME-OUT
           MOVE INIT-COMB-EFF    TO INIT-COMB-EFF-OUT
      *
           DISPLAY "Cold Water Inlet Temperature: " WITH NO ADVANCING
           DISPLAY COLD-WATER-TEMP-OUT " degF"
      *
           DISPLAY SPACES
      *
           DISPLAY "Initial Hot Water Temperature: " WITH NO ADVANCING
           DISPLAY INIT-DHW-TEMP-OUT " degF"
      *
           DISPLAY "Initial FlowRate: " WITH NO ADVANCING
           DISPLAY INIT-FLOW-RATE-OUT " GPM"
      *
           DISPLAY "Initial Shower Time: " WITH NO ADVANCING
           DISPLAY INIT-SHOWER-TIME-OUT " minutes"
      *
           DISPLAY "Initial Combustion Efficiency: " WITH NO ADVANCING
           DISPLAY INIT-COMB-EFF-OUT "%"
      *
      * Convert Combustion Efficiency percentage to a decimal
           DIVIDE 100 INTO INIT-COMB-EFF
      *
      * Continue calculating initial conditions
      *
           MULTIPLY INIT-FLOW-RATE BY INIT-SHOWER-TIME
                    GIVING INIT-USAGE ROUNDED
      *
           COMPUTE INIT-ENERGY ROUNDED =
                   8.33 * INIT-USAGE *
                   (INIT-DHW-TEMP - COLD-WATER-TEMP) /
                   INIT-COMB-EFF
      *
           DIVIDE INIT-ENERGY BY INIT-CONV-UNIT
                  GIVING INIT-FUEL-CONSUMED ROUNDED
      *
           MULTIPLY INIT-FUEL-CONSUMED BY INIT-FUEL-PRICE
                    GIVING INIT-FUEL-COST ROUNDED
      *
           MOVE INIT-USAGE            TO INIT-USAGE-OUT
           MOVE INIT-ENERGY           TO INIT-ENERGY-OUT
           MOVE INIT-FUEL-CONSUMED    TO INIT-FUEL-CONSUMED-OUT
           MOVE INIT-FUEL-COST        TO INIT-FUEL-COST-OUT
           MOVE INIT-FUEL-PRICE       TO PRICE-OUT
      *
      * Continue displaying initial conditions
      *
           DISPLAY "Initial Water Consumption: " WITH NO ADVANCING
           DISPLAY INIT-USAGE-OUT " gallons"
      *
           DISPLAY "Initial Energy Consumption: " WITH NO ADVANCING
           DISPLAY INIT-ENERGY-OUT " BTUs"
      *
           DISPLAY "Initial Fuel Consumption: " WITH NO ADVANCING
           DISPLAY INIT-FUEL-CONSUMED-OUT " " INIT-FUEL-UNIT
      *
           DISPLAY "Initial Fuel Cost: " WITH NO ADVANCING
           DISPLAY INIT-FUEL-COST-OUT " @" PRICE-OUT WITH NO ADVANCING
           DISPLAY " per " INIT-FUEL-UNIT.
      *
       5-DISPLAY-NEW-REPORT.
           MOVE NEW-DHW-TEMP    TO NEW-DHW-TEMP-OUT
           MOVE NEW-FLOW-RATE   TO NEW-FLOW-RATE-OUT
           MOVE NEW-SHOWER-TIME TO NEW-SHOWER-TIME-OUT
           MOVE NEW-COMB-EFF    TO NEW-COMB-EFF-OUT
      *
           DISPLAY SPACES
      *
           DISPLAY "New Hot Water Temperature: " WITH NO ADVANCING
           DISPLAY NEW-DHW-TEMP-OUT " degF"
      *
           DISPLAY "New FlowRate: " WITH NO ADVANCING
           DISPLAY NEW-FLOW-RATE-OUT " GPM"
      *
           DISPLAY "New Shower Time: " WITH NO ADVANCING
           DISPLAY NEW-SHOWER-TIME-OUT " minutes"
      *
           DISPLAY "New Combustion Efficiency: " WITH NO ADVANCING
           DISPLAY NEW-COMB-EFF-OUT "%"
      *
      * Convert Combustion Efficiency percentage to a decimal
           DIVIDE 100 INTO NEW-COMB-EFF
      *
      * Continue calculating new conditions
      *
           MULTIPLY NEW-FLOW-RATE BY NEW-SHOWER-TIME
                    GIVING NEW-USAGE ROUNDED
      *
           COMPUTE NEW-ENERGY ROUNDED =
                   8.33 * NEW-USAGE *
                   (NEW-DHW-TEMP - COLD-WATER-TEMP) /
                   NEW-COMB-EFF
      *
           DIVIDE NEW-ENERGY BY NEW-CONV-UNIT
                  GIVING NEW-FUEL-CONSUMED ROUNDED
      *
           MULTIPLY NEW-FUEL-CONSUMED BY NEW-FUEL-PRICE
                    GIVING NEW-FUEL-COST ROUNDED
      *
           MOVE NEW-USAGE            TO NEW-USAGE-OUT
           MOVE NEW-ENERGY           TO NEW-ENERGY-OUT
           MOVE NEW-FUEL-CONSUMED    TO NEW-FUEL-CONSUMED-OUT
           MOVE NEW-FUEL-COST        TO NEW-FUEL-COST-OUT
           MOVE NEW-FUEL-PRICE       TO PRICE-OUT
      *
      * Continue displaying new conditions
      *
           DISPLAY "New Water Consumption: " WITH NO ADVANCING
           DISPLAY NEW-USAGE-OUT " gallons"
      *
           DISPLAY "New Energy Consumption: " WITH NO ADVANCING
           DISPLAY NEW-ENERGY-OUT " BTUs"
      *
           DISPLAY "New Fuel Consumption: " WITH NO ADVANCING
           DISPLAY NEW-FUEL-CONSUMED-OUT " " NEW-FUEL-UNIT
      *
           DISPLAY "New Fuel Cost: " WITH NO ADVANCING
           DISPLAY NEW-FUEL-COST-OUT " @" PRICE-OUT WITH NO ADVANCING
           DISPLAY " per " NEW-FUEL-UNIT.
      *
       6-DISPLAY-SAVINGS-REPORT.
      *
      * Calculate savings
      *
           SUBTRACT NEW-USAGE FROM INIT-USAGE GIVING WATER-SAVED
           SUBTRACT NEW-ENERGY FROM INIT-ENERGY GIVING ENERGY-SAVED
           SUBTRACT NEW-FUEL-COST FROM INIT-FUEL-COST GIVING COST-SAVED
      *
           MOVE WATER-SAVED  TO WATER-SAVED-OUT
           MOVE ENERGY-SAVED TO ENERGY-SAVED-OUT
      *
           DISPLAY SPACES
           DISPLAY "Water Saved: " WATER-SAVED-OUT " gallons"
           DISPLAY "Energy Saved: " ENERGY-SAVED-OUT " BTUs"
      *
      * Display fuel saved only if initial fuel and
      * new fuel are the same type
      *
           IF INIT-FUEL IS EQUAL TO NEW-FUEL THEN
              SUBTRACT NEW-FUEL-CONSUMED FROM INIT-FUEL-CONSUMED
                       GIVING FUEL-SAVED
              MOVE FUEL-SAVED TO FUEL-SAVED-OUT
              DISPLAY "Fuel Saved: " FUEL-SAVED-OUT " " INIT-FUEL-UNIT
           END-IF.
      *
           MOVE COST-SAVED TO COST-SAVED-OUT
           DISPLAY "Cost Saved: " COST-SAVED-OUT.
      *
       998-FUEL-MENU.
           DISPLAY SPACES
           MOVE ELECTRIC-PRICE TO PRICE-OUT
           DISPLAY "1...[E]lectric    @" WITH NO ADVANCING
           DISPLAY PRICE-OUT " per " ELECTRIC-UNIT
      *
           MOVE NATGAS-PRICE TO PRICE-OUT
           DISPLAY "2...[N]atural Gas @" WITH NO ADVANCING
           DISPLAY PRICE-OUT " per " NATGAS-UNIT
      *
           MOVE PROPANE-PRICE TO PRICE-OUT
           DISPLAY "3...[P]ropane     @" WITH NO ADVANCING
           DISPLAY PRICE-OUT " per " PROPANE-UNIT.
      *
       999-END-PROGRAM.
           DISPLAY SPACES
           DISPLAY "** Domestic Hot Water Savings Calculator ENDS **"
           STOP RUN.
