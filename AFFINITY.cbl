      *****************************************************************
      * FAN / PUMP AFFINITY LAW CALCULATIONS                          *
      *                                                               *
      * A simple program that calculates the change in horsepower,    *
      * pressure, and flow rate with changes in rotational speed.     *
      *                                                               *
      *****************************************************************

       identification division.
       program-id.   affinity.
       author.         Chipman.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Affinity Laws:                                                *
      *                                                               *
      * Law #1                                                        *
      * Quantity (CFM/GPM) changes proportionally with fan/pump speed *
      *                                                               *
      *       CFM2|GPM2   [ RPM2 ]^1                                  *
      *      ---------- = [------]                                    *
      *       CFM1|GPM1   [ RPM1 ]                                    *
      *                                                               *
      * Law #2                                                        *
      * Pressure varies with the SQUARE of fan/pump speed             *
      *                                                               *
      *       P2    [ RPM2 ]^2                                        *
      *      ---- = [------]                                          *
      *       P1    [ RPM1 ]                                          *
      *                                                               *
      * Law #3                                                        *
      * Horsepower varies with the CUBE of the fan/pump speed         *
      *                                                               *
      *       HP2    [ RPM2 ]^3                                       *
      *      ----- = [------]                                         *
      *       HP1    [ RPM1 ]                                         *
      *                                                               *
      *****************************************************************

      * Data-entry-fields.
       01 rpm1-in               pic x(8).
       01 rpm2-in               pic x(8).
       01 quantity1-in          pic x(8).
       01 motor-eff-in          pic x(8).
       01 power-factor-in       pic x(8).
       01 law                   pic 9.
       01 yes-no                pic x.
           88 affirm                  value "Y".
           88 neg                     value "N".

       01 three-phase-flag      pic x.
           88 three-phase             value "Y".
           88 single-phase            value "N".

      * Calculated-fields.
       01 rpm1                  pic 9(4)v9.
       01 rpm2                  pic 9(4)v9.
       01 quantity1             pic 9(4)v9.
       01 quantity2             pic 9(4)v9.
       01 motor-eff             pic 999v999.
       01 power-factor          pic 99v99 value 1.
       01 square-root3          pic 99v99 value 1.
       01 hp-conversion-factor  pic 999 value 746.
       01 old-watts             pic 9(6)v99.
       01 old-kilowatts         pic 999v9.
       01 new-watts             pic 9(6)v99.
       01 new-kilowatts         pic 999v9.

      * Displayed-fields.
       01 description           pic x(10).
       01 rpm1-out              pic zzz9.9 usage display.
       01 rpm2-out              pic zzz9.9 usage display.
       01 quantity1-out         pic zzz9.99 usage display.
       01 quantity2-out         pic zzz9.99 usage display.
       01 old-kw-out            pic zz,zz9.9 usage display.
       01 new-kw-out            pic zz,zz9.9 usage display.
       01 motor-eff-out         pic zz9.9 usage display.

      * Optional-display-fields can be commented out after debugging
       01 power-factor-out      pic 9.99 usage display.
       01 squareroot3-out       pic 9.99 usage display.

      * Constant-text.
       01 changes-text1         pic x(9)
                                value spaces.
       01 changes-text2         pic x(22)
                                value "changes in speed (RPM)".
       01 quantity-too-much     pic x(26)
                                value "Quantity must be <= 9999.9".
       01 not-numeric           pic x(16)
                                value " is NOT numeric.".

       procedure division.
      * main-paragraph.
      *     perform opening-screen-data-entry
      *     perform quantity1-data-entry
      *     perform oldrpm-data-entry
      *     perform newrpm-data-entry
      *     perform calculate-it
      *     perform disp-result
      *     perform end-program.

      * Display opening screen & commence data entry
       opening-screen-data-entry.
           display spaces
           display "***** AFFINITY LAWS CALCULATOR UTILITY BEGINS *****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "June 16, 2020"
           display spaces
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces
           display "Enter zero for any parameter to end the program."
           display spaces
           display "Law #1 - Flow rate changes proportionally with"
           display changes-text1 changes-text2
           display spaces
           display "    #2 - Pressure changes with the SQUARE of"
           display changes-text1 changes-text2
           display spaces
           display "    #3 - Horsepower changes with the CUBE of"
           display changes-text1 changes-text2
           display spaces
           display "Select a law (1, 2, 3, or 0 to exit): "
                    with no advancing
           accept law

           evaluate law
           when 0 go to end-program

           when 1 move "quantity" to description

           when 2 move "pressure" to description

           when 3
                 move "horsepower" to description
      * Data entry of horsepower related data
      *           display spaces
                 display "Is the motor AC powered? (Y/n): "
                          with no advancing
                 accept yes-no
                 move function upper-case(yes-no) to yes-no

                 if affirm then
                    perform ac-powered-query
                 end-if

                 perform motor-efficiency-data-entry

           when other
                 display spaces
                 display "Enter 0 through 3 ONLY"
                 go to opening-screen-data-entry

           end-evaluate.

      * Continue data entry of required quantities
       quantity1-data-entry.
      *     display spaces
           display "Enter previous " description ": " with no advancing
           accept quantity1-in

      * Did the user enter a valid numeric value?
           if function test-numval(quantity1-in) IS NOT EQUAL ZERO then
              display "Previous " description not-numeric
              go to quantity1-data-entry
           else
              compute quantity1 = function numval(quantity1-in)
           end-if

           if quantity1 IS EQUAL ZERO then go to end-program

           if quantity1 > 9999.9
              display quantity-too-much
              go to quantity1-data-entry
           end-if.

       oldrpm-data-entry.
      *     display spaces
           display "Enter old RPM value: " with no advancing
           accept rpm1-in

      * Did the user enter a valid numeric value?
           if function test-numval(rpm1-in) IS NOT EQUAL ZERO then
              display "Old RPM value" not-numeric
              go to oldrpm-data-entry
           else
              compute rpm1 = function numval(rpm1-in)
           end-if

           if rpm1 IS EQUAL ZERO then go to end-program

           if rpm1 > 9999.9
              display quantity-too-much
              go to oldrpm-data-entry
           end-if.

       newrpm-data-entry.
      *     display spaces
           display "Enter new RPM value: " with no advancing
           accept rpm2-in

      * Did the user enter a valid numeric value?
           if function test-numval(rpm2-in) IS NOT EQUAL ZERO then
              display "New RPM value" not-numeric
           else
              compute rpm2 = function numval(rpm2-in)
           end-if

           if rpm2 IS EQUAL ZERO then go to end-program

           if rpm2 > 9999.9
              display quantity-too-much
              go to newrpm-data-entry
           end-if.

       calculate-it.
      *****************************************************************
      *                                                               *
      * Affinity Laws:                                                *
      *                                                               *
      * Law #1                                                        *
      * Quantity (CFM/GPM) changes proportionally with fan/pump speed *
      *                                                               *
      *       CFM2|GPM2   [ RPM2 ]^1                                  *
      *      ---------- = [------]                                    *
      *       CFM1|GPM1   [ RPM1 ]                                    *
      *                                                               *
      * Law #2                                                        *
      * Pressure varies with the SQUARE of fan/pump speed             *
      *                                                               *
      *       P2    [ RPM2 ]^2                                        *
      *      ---- = [------]                                          *
      *       P1    [ RPM1 ]                                          *
      *                                                               *
      * Law #3                                                        *
      * Horsepower varies with the CUBE of the fan/pump speed         *
      *                                                               *
      *       HP2    [ RPM2 ]^3                                       *
      *      ----- = [------]                                         *
      *       HP1    [ RPM1 ]                                         *
      *                                                               *
      *****************************************************************

      * Affinity Law computation:
           compute quantity2 = quantity1 * (rpm2 / rpm1) ** law

      * Calculating changes in horsepower?
           if law = 3

      * The commands between the asterisk lines can be commented out
      * after debugging:
      ******************************************************
      *        move power-factor to power-factor-out
      *        move square-root3 to squareroot3-out
      ******************************************************

      * Calculate electrical power
              compute old-watts = (quantity1 * hp-conversion-factor *
                      square-root3 * power-factor) / motor-eff

              divide old-watts by 1000 giving old-kilowatts rounded
              move old-kilowatts to old-kw-out

              compute new-watts = (quantity2 * hp-conversion-factor *
                      square-root3 * power-factor) / motor-eff

              divide new-watts by 1000 giving new-kilowatts rounded
              move new-kilowatts to new-kw-out

           end-if.

       disp-result.

      * Move calculated values to displayed values
           move rpm1 to rpm1-out
           move rpm2 to rpm2-out
           move quantity1 to quantity1-out
           move quantity2 to quantity2-out
           move power-factor to power-factor-out

      * Motor efficiency numeric value already moved to displayed value
      * in the motor-efficiency-data-entry paragraph because the
      * entered % data entry value needs to be divided by 100 prior to
      * using the numeric data in the calculation formula.
      *     move motor-eff to motor-eff-out

           display spaces
           display "***** RESULTS *****"
           display spaces
           display "Old RPM: " rpm1-out
           display "Old " description " : " quantity1-out
           if law = 3
              display "Old motor electrical power: " old-kw-out " KW"
           end-if

           display spaces
           display "New RPM: " rpm2-out
           display "New " description " : " quantity2-out

           if law = 3
      * The commands between the asterisk lines can be commented out
      * after debugging:
      ******************************************************
      *           display "3-phase: " squareroot3-out
      *           display "HP conversion factor: " hp-conversion-factor
      *           display "watts: " watts
      ******************************************************
              display "New motor electrical power: " new-kw-out " KW"
              display spaces
              display "Power factor: " power-factor-out
              display "Motor efficiency: " motor-eff-out "%"
           end-if.

       end-program.
           display spaces
           display "***** AFFINITY LAWS CALCULATOR UTILITY ENDS *****"
           display spaces
           stop run.

       ac-powered-query.
      *     display spaces
           display "Enter power factor: " with no advancing
           accept power-factor-in

      *     display "numeric test"
           if function test-numval(power-factor-in)
              IS NOT EQUAL ZERO then
              display "Power Factor" not-numeric
              go to ac-powered-query
           else
      *        display "Power Factor is numeric"
              compute power-factor = function numval(power-factor-in)
      *        display power-factor
           end-if

           evaluate power-factor
              when 0
      *           display "End Program from power factor"
                 go to end-program

              when other
      * Comment out after debugging:
      *************************************
      *           display "power-factor-in: " power-factor-in
      *           display "power-factor: " power-factor
      *************************************
                 if power-factor > 1
                    display "Power factor must be <= 1.00"
                    go to ac-powered-query
                 end-if
           end-evaluate

      *     display spaces
           display "Is the AC power 3-phase? (Y/n): " with no advancing
           accept three-phase-flag

           move function upper-case(three-phase-flag) to
                 three-phase-flag

           if three-phase then
              move 1.73 to square-root3
      *        display "3-phase"
           end-if

           if single-phase then
              move 1 to square-root3
      *        display "1-phase"
           end-if

      *     display square-root3
           move square-root3 to squareroot3-out.

       motor-efficiency-data-entry.
      *     display spaces
           display "Enter motor efficiency as %: " with no advancing
           accept motor-eff-in

           if function test-numval(motor-eff-in)
              IS NOT EQUAL ZERO then
              display "Motor efficiency" not-numeric
              go to motor-efficiency-data-entry
           else
              compute motor-eff = function numval(motor-eff-in)
      *        display motor-eff
           end-if

           evaluate motor-eff
              when 0 go to end-program
              when other
                 if motor-eff > 100 then
                    display spaces
                    display "Motor efficiency % must be <= 100"
                    go to motor-efficiency-data-entry
                 end-if
           end-evaluate

      *     display "Motor eff: " motor-eff
      * Motor efficiency numeric value moved to displayed value here
      * in the motor-efficiency-data-entry paragraph because the
      * entered % data entry value needs to be divided by 100 prior to
      * using the numeric data in the calculation formula.
           move motor-eff to motor-eff-out
           divide motor-eff by 100 giving motor-eff rounded.
