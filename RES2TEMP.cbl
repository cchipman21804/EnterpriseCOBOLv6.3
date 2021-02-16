       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RES2TEMP.
       AUTHOR.        CHIPMAN.
      *
      * This algorithm converts the resistance of a 3kohm@77F
      * thermistor into an accurate temperature in degF
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * The following Python3 formula was derived from the sensor
      * specification sheet of a 3Kohm@77F NTC thermistor:
      *
      * yF = 8.5557106e-23 * math.pow(x,6) -
      *      5.68981649695e-18 * math.pow(x,5) +
      *      1.50180247230692e-13 * math.pow(x,4) -
      *      2.01541522576944e-9 * math.pow(x,3) +
      *      1.47906738776888e-5 * math.pow(x,2) -
      *      6.2591776279401e-2 * x +
      *      1.74508163989243e2
      *
       01 USER-INPUT                                  PIC X(5).
       01 SENSOR-RESISTANCE                           PIC 9(5).
       01 DEG-F                                       PIC S999V99.
       01 DEG-F-OUT                                   PIC +ZZ9.99.
      *
       01 VALID-NUMBER-FLAG                           PIC X VALUE 'F'.
          88 VALID-NUMBER                                   VALUE 'T'.
      *
      * Constant-values.
       01 min-val            pic 999  value 895.
       01 max-val            pic 9(5) value 19320.
      *
      * Constant-text.
       01 not-numeric        pic x(16) value " is NOT numeric.".
       01 quantity-too-small pic x(25)
                             value "Value must be >= 895 ohms".
       01 quantity-too-much  pic x(27)
                             value "Value must be <= 19320 ohms".
      *
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM OPENING-SCREEN
           PERFORM DATA-ENTRY UNTIL VALID-NUMBER
           PERFORM CALCULATE-TEMPERATURE.
      *
       END-PROGRAM.
           STOP RUN.
      *
       OPENING-SCREEN.
           DISPLAY "THIS ALGORITHM CONVERTS THE SENSOR RESISTANCE OF"
           DISPLAY "A 3KOHM @77 DEGF NTC THERMISTOR TO AN ACCURATE"
           DISPLAY "TEMPERATURE."
           DISPLAY SPACES
           DISPLAY "Written by, Clifford A. Chipman, EMIT on"
           DISPLAY "February 11, 2021".
      *
       DATA-ENTRY.
           DISPLAY SPACES
           DISPLAY "Enter sensor resistance: " WITH NO ADVANCING
           ACCEPT USER-INPUT
      *
      * Did the user enter an invalid numeric value?
           IF FUNCTION TEST-NUMVAL(USER-INPUT) IS NOT EQUAL ZERO THEN
              DISPLAY "Sensor Resistance" not-numeric
              MOVE 'F' TO VALID-NUMBER-FLAG
           ELSE
              COMPUTE SENSOR-RESISTANCE = FUNCTION NUMVAL(USER-INPUT)
           END-IF
      *
           IF SENSOR-RESISTANCE IS EQUAL ZERO THEN
              GO TO END-PROGRAM
           END-IF
      *
           IF SENSOR-RESISTANCE > MAX-VAL THEN
              DISPLAY QUANTITY-TOO-MUCH
              MOVE 'F' TO VALID-NUMBER-FLAG
           END-IF
      *
           IF SENSOR-RESISTANCE < MIN-VAL THEN
              DISPLAY QUANTITY-TOO-SMALL
              MOVE 'F' TO VALID-NUMBER-FLAG
           ELSE
              MOVE 'T' TO VALID-NUMBER-FLAG
           END-IF.
      *
       CALCULATE-TEMPERATURE.
      *
      * The following Python3 formula was derived from the sensor
      * specification sheet of a 3Kohm@77F NTC thermistor:
      *
      * x is the sensor resistance
      * yF is the calculated temperature in degF
      *
      * yF = 8.5557106e-23 * math.pow(x,6) -
      *      5.68981649695e-18 * math.pow(x,5) +
      *      1.50180247230692e-13 * math.pow(x,4) -
      *      2.01541522576944e-9 * math.pow(x,3) +
      *      1.47906738776888e-5 * math.pow(x,2) -
      *      6.2591776279401e-2 * x +
      *      1.74508163989243e2
      *
           COMPUTE DEG-F = 8.5557106 * FUNCTION EXP10(-23) *
                           SENSOR-RESISTANCE ** 6 -
                           5.68981649695 * FUNCTION EXP10(-18) *
                           SENSOR-RESISTANCE ** 5 +
                           1.50180247230692 * FUNCTION EXP10(-13) *
                           SENSOR-RESISTANCE ** 4 -
                           2.01541522576944 * FUNCTION EXP10(-9) *
                           SENSOR-RESISTANCE ** 3 +
                           1.47906738776888 * FUNCTION EXP10(-5) *
                           SENSOR-RESISTANCE ** 2 -
                           6.2591776279401 * FUNCTION EXP10(-2) *
                           SENSOR-RESISTANCE +
                           1.74508163989243 * FUNCTION EXP10(2)
      *
           MOVE DEG-F TO DEG-F-OUT
           DISPLAY DEG-F-OUT " degF".
