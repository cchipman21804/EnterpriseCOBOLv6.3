      *****************************************************************
      * OHMS LAW CALCULATIONS                                         *
      *                                                               *
      * A simple program that calculates the relationship between     *
      * voltage, current, and resistance in an electric circuit.      *
      *                                                               *
      *****************************************************************

       identification division.
       program-id.     ohmslaw.
       author.         Chipman.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      *                          Ohm's Laws                           *
      *                                                               *
      *                               ^                               *
      *                              / \                              *
      *                             / E \                             *
      *                            /_____\                            *
      *                           /   |   \                           *
      *                          /  I | R  \                          *
      *                         /_____|_____\                         *
      *                                                               *
      *        E = I * R          I = E / R          R = E / I        *
      *                                                               *
      *****************************************************************

      * Data-entry-fields.
       01 quantity1-in          pic x(20).
       01 quantity2-in          pic x(20).
       01 law                   pic 9.
       01 law-in                pic x.
      *01 power-factor-in       pic x(8).
      *01 yes-no                pic x.
      *    88 affirm                  value "Y".
      *    88 neg                     value "N".

      *01 three-phase-flag      pic x.
      *    88 three-phase             value "Y".
      *    88 single-phase            value "N".

      * Calculated-fields.
       01 quantity1             pic 9(9)v9(9).
       01 quantity2             pic 9(9)v9(9).
       01 voltage               pic 9(9)v9(9).
       01 current               pic 9(9)v9(9).
       01 resistance            pic 9(9)v9(9).
      *01 power-factor          pic 99v99 value 1.
      *01 square-root3          pic 99v99 value 1.
      *01 old-watts             pic 9(6)v99.
      *01 old-kilowatts         pic 999v9.

      * Displayed-fields.
       01 description1          pic x(10).
       01 description2          pic x(10).
       01 description3          pic x(10).
       01 unit1                 pic x(10) value " ohms".
       01 unit2                 pic x(10) value " amps".
       01 unit3                 pic x(10) value " volts".
       01 voltage-out           pic zzz,zzz,zz9.999  usage display.
       01 current-out           pic zzz,zzz,zz9.9(6) usage display.
       01 resistance-out        pic zzz,zzz,zz9.999  usage display.
      *01 old-kw-out            pic zz,zz9.9 usage display.
      *01 new-kw-out            pic zz,zz9.9 usage display.

      * Optional-display-fields can be commented out after debugging
      *01 power-factor-out      pic 9.99 usage display.
      *01 squareroot3-out       pic 9.99 usage display.

      * Constant-text.
      * 01 quantity-too-much     pic x(26)
      *                          value "Quantity must be <= 9999.9".
       01 not-numeric           pic x(16)
                                value " is NOT numeric.".

       procedure division.
       main-paragraph.
            perform opening-screen-data-entry
            perform quantity1-data-entry
            perform quantity2-data-entry
            perform calculate-it
            perform end-program.

      * Display opening screen & commence data entry
       opening-screen-data-entry.
           display spaces
           display "***** OHM'S LAW CALCULATOR UTILITY BEGINS *****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "October 4, 2020"
           display spaces
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces
           display "Enter zero for any parameter to end the program."
           display spaces
           display "    #1 - Calculate R"
           display "    #2 - Calculate I"
           display "    #3 - Calculate E"
           display spaces
           display "Select a law (1, 2, 3, or 0 to exit): "
                    with no advancing
           accept law-in

           evaluate true
           when law-in = "0" perform end-program

           when law-in = "1" or "R" or "r"
                  move "voltage"    to description1
                  move "current"    to description2
                  move "resistance" to description3
                  move 1 to law

           when law-in = "2" or "I" or "i"
                  move "voltage"    to description1
                  move "resistance" to description2
                  move "current"    to description3
                  move 2 to law

           when law-in = "3" or "E" or "e"
                  move "current"    to description1
                  move "resistance" to description2
                  move "voltage"    to description3
                  move 3 to law

           when other
                 display spaces
                 display "Enter 0 through 3 ONLY"
                 go to opening-screen-data-entry

           end-evaluate.

       quantity1-data-entry.
      *     display spaces
           display "Enter " description1 ": " with no advancing
           accept quantity1-in

      * Did the user enter a valid numeric value?
           if function test-numval(quantity1-in) IS NOT EQUAL ZERO then
              display description1 not-numeric
              go to quantity1-data-entry
           else
              compute quantity1 = function numval(quantity1-in)
           end-if

           evaluate true
           when quantity1 IS EQUAL ZERO perform end-program

      *    when quantity1 IS GREATER THAN 9999.9
      *           display quantity-too-much
      *           go to quantity1-data-entry
           end-evaluate.

       quantity2-data-entry.
      *     display spaces
           display "Enter " description2 ": " with no advancing
           accept quantity2-in

      * Did the user enter a valid numeric value?
           if function test-numval(quantity2-in) IS NOT EQUAL ZERO then
              display description2 not-numeric
              go to quantity2-data-entry
           else
              compute quantity2 = function numval(quantity2-in)
           end-if

           evaluate true
           when quantity2 IS EQUAL ZERO perform end-program

      *    when quantity2 IS GREATER THAN 9999.9
      *           display quantity-too-much
      *           go to quantity1-data-entry
           end-evaluate.

       calculate-it.
      *****************************************************************
      *                                                               *
      *                          Ohm's Laws                           *
      *                                                               *
      *                               ^                               *
      *                              / \                              *
      *                             / E \                             *
      *                            /_____\                            *
      *                           /   |   \                           *
      *                          /  I | R  \                          *
      *                         /_____|_____\                         *
      *                                                               *
      *        E = I * R          I = E / R          R = E / I        *
      *                                                               *
      *****************************************************************

           evaluate law
           when 1 move quantity1 to voltage
                  move quantity2 to current
                  divide voltage by current giving resistance rounded
                  evaluate true
                    when resistance IS LESS THAN 1
                          multiply 1000 by resistance
                          move " milliohms" to unit1

                    when resistance IS GREATER THAN OR EQUAL TO 1000
                          divide 1000 into resistance
                          move " kilohms" to unit1

                    when resistance IS GREATER THAN OR EQUAL TO 1000000
                          divide 1000000 into resistance
                          move " megohms" to unit1
                  end-evaluate
                  move resistance to resistance-out
                  display resistance-out unit1

           when 2 move quantity1 to voltage
                  move quantity2 to resistance
                  divide voltage by resistance giving current rounded
                  evaluate true
                    when current IS LESS THAN 1
                          multiply 1000 by current
                          move " milliamps" to unit2

                    when resistance IS GREATER THAN OR EQUAL TO 1000
                          divide 1000 into resistance
                          move " kiloamps" to unit2

                    when resistance IS GREATER THAN OR EQUAL TO 1000000
                          divide 1000000 into resistance
                          move " megaamps" to unit2
                  end-evaluate
                  move current to current-out
                  display current-out unit2

           when 3 move quantity1 to current
                  move quantity2 to resistance
                  multiply current by resistance giving voltage rounded
                  evaluate true
                    when voltage IS LESS THAN 1
                          multiply 1000 by voltage
                          move " millivolts" to unit3

                    when voltage IS GREATER THAN OR EQUAL TO 1000
                          divide 1000 into voltage
                          move " kilovolts" to unit3

                    when voltage IS GREATER THAN OR EQUAL TO 1000000
                          divide 1000000 into voltage
                          move " megavolts" to unit3
                  end-evaluate
                  move voltage to voltage-out
                  display voltage-out unit3

      *    when other
      *          display spaces
      *          display "Enter 0 through 3 ONLY"
      *          go to opening-screen-data-entry

           end-evaluate.

       end-program.
           display spaces
           display "***** OHM'S LAW CALCULATOR UTILITY ENDS *****"
           display spaces
           stop run.

