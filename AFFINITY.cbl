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
       01 pulley-diameter1-in   pic x(8).
       01 pulley-diameter2-in   pic x(8).
       01 motor-eff-in          pic x(8).
       01 power-factor-in       pic x(8).
       01 law                   pic 9.
       01 yes-no                pic x.
           88 affirm                  value "Y".
           88 neg                     value "N".

       01 three-phase-flag      pic x.
           88 three-phase             value "Y".
           88 single-phase            value "N".

       01 drivetrain-flag       pic x.
           88 shaft                   value "S".
           88 pulley                  value "P".

      * Calculated-fields.
       01 rpm1                  pic 9(5)v9(5).
       01 rpm2                  pic 9(5)v9(5).
       01 quantity1             pic 9(5)v9(5).
       01 quantity2             pic 9(5)v9(5).
       01 pulley-diameter1      pic 9(5)v9(5).
       01 pulley-diameter2      pic 9(5)v9(5).
       01 motor-eff             pic 999v999.
       01 power-factor          pic 99v99 value 1.
       01 square-root3          pic 99v99 value 1.
       01 hp-conversion-factor  pic 999 value 746.
       01 old-watts             pic 9(6)v99.
       01 old-kilowatts         pic 999v9.
       01 new-watts             pic 9(6)v99.
       01 new-kilowatts         pic 999v9.
       01 pulley-ratio          pic 9(5)v9(5) value 1.

      * Displayed-fields.
       01 description           pic x(10).
       01 rpm1-out              pic zzzz9.99 usage display.
       01 rpm2-out              pic zzzz9.99 usage display.
       01 quantity1-out         pic zzzz9.99 usage display.
       01 quantity2-out         pic zzzz9.99 usage display.
       01 old-kw-out            pic zz,zz9.9 usage display.
       01 new-kw-out            pic zz,zz9.9 usage display.
       01 motor-eff-out         pic zz9.9 usage display.
       01 pulley-ratio-out      pic zz9.99 usage display.

      * Optional-display-fields can be commented out after debugging
       01 power-factor-out      pic 9.99 usage display.
       01 squareroot3-out       pic 9.99 usage display.

      * Constant-text.
       01 changes-text1         pic x(9)
                                value spaces.
       01 changes-text2         pic x(22)
                                value "changes in speed (RPM)".
       01 quantity-too-much     pic x(27)
                                value "Quantity must be <= 99999.9".
       01 quantity-too-small    pic x(20)
                                value "Quantity must be > 0".
       01 not-numeric           pic x(16)
                                value " is NOT numeric.".

       procedure division.
       100-main-paragraph.
           perform 200-opening-screen-data-entry
           perform 300-quantity1-data-entry
           perform 400-oldrpm-data-entry
           perform 500-newrpm-data-entry
           perform 600-drivetrain-data-entry
           perform 700-calculate-it
           perform 800-disp-result
           perform 999-end-program.

      * Display opening screen & commence data entry
       200-opening-screen-data-entry.
           display spaces
           display "***** AFFINITY LAWS CALCULATOR UTILITY BEGINS *****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "June 16, 2020"
      * Pulleys
           display "updated June 25, 2021"
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
           when 0 perform 999-end-program

           when 1 move "flow rate" to description

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
                 perform 200-opening-screen-data-entry

           end-evaluate.

      * Continue data entry of required quantities
       300-quantity1-data-entry.
      *     display spaces
           display "Enter previous " description ": " with no advancing
           accept quantity1-in

      * Did the user enter a valid numeric value?
           if function test-numval(quantity1-in) IS NOT EQUAL ZERO then
              display "Previous " description not-numeric
              perform 300-quantity1-data-entry
           else
              compute quantity1 = function numval(quantity1-in)
           end-if

           evaluate true
           when quantity1 IS EQUAL ZERO
              perform 999-end-program

           when quantity1 IS NEGATIVE
              display quantity-too-small
              perform 300-quantity1-data-entry

           when quantity1 > 99999.9
              display quantity-too-much
              perform 300-quantity1-data-entry

           end-evaluate.

       400-oldrpm-data-entry.
      *     display spaces
           display "Enter old motor RPM value: " with no advancing
           accept rpm1-in

      * Did the user enter a valid numeric value?
           if function test-numval(rpm1-in) IS NOT EQUAL ZERO then
              display "Old RPM value" not-numeric
              perform 400-oldrpm-data-entry
           else
              compute rpm1 = function numval(rpm1-in)
           end-if

           evaluate true
           when rpm1 IS EQUAL ZERO
              perform 999-end-program

           when rpm1 IS NEGATIVE
              display quantity-too-small
              perform 400-oldrpm-data-entry

           when rpm1 > 99999.9
              display quantity-too-much
              perform 400-oldrpm-data-entry

           end-evaluate.

       500-newrpm-data-entry.
      *     display spaces
           display "Enter new motor RPM value: " with no advancing
           accept rpm2-in

      * Did the user enter a valid numeric value?
           if function test-numval(rpm2-in) IS NOT EQUAL ZERO then
              display "New RPM value" not-numeric
           else
              compute rpm2 = function numval(rpm2-in)
           end-if

           evaluate true
           when rpm2 IS EQUAL ZERO
              perform 999-end-program

           when rpm2 IS NEGATIVE
              display quantity-too-small
              perform 500-newrpm-data-entry

           when rpm2 > 99999.9
              display quantity-too-much
              perform 500-newrpm-data-entry

           end-evaluate.

       600-drivetrain-data-entry.
           display spaces
           display "Does the drivetrain use a [P]ulley or a [S]haft?"
      *     display "S ... Shaft"
      *     display "P ... Pulley"
           display "Select a drivetrain: " with no advancing
           accept drivetrain-flag
           move function upper-case(drivetrain-flag) to drivetrain-flag

           evaluate drivetrain-flag
           when "S" move 1 to pulley-ratio
      * [S]haft implies a directly driven load on the motor's shaft
      * Therefore, the pulley-ratio defaults to 1:1

           when "P" perform pulley-data-entry
      * Obtain pulley diameters and calculate the pulley-ratio
      * This algorithm will also accept sprocket teeth & gear teeth
      * and calculate the corresponding drive ratios

           when other perform 600-drivetrain-data-entry
           end-evaluate.

       700-calculate-it.
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
      * Apply pulley ratio to oldrpm & newrpm
      * to convert motor rpms to load rpms
           compute rpm1 rounded = rpm1 * pulley-ratio
           compute rpm2 rounded = rpm2 * pulley-ratio
      *
      * If rpms differ, compute quantity2 based on rpm
           if rpm1 IS NOT EQUAL TO rpm2 then
              compute quantity2 rounded =
                       quantity1 * (rpm2 / rpm1) ** law
      *
      * If rpms are same, compute quantity2 based on pulley ratio
           else
              compute quantity2 rounded =
                       quantity1 * (pulley-ratio) ** law

      * Calculating changes in horsepower?
           if law = 3

      * The commands between the asterisk lines can be commented out
      * after debugging:
      ******************************************************
      *        move power-factor to power-factor-out
      *        move square-root3 to squareroot3-out
      ******************************************************

      * Calculate electrical power
              compute old-watts rounded =
                       (quantity1 * hp-conversion-factor *
                        square-root3 * power-factor) / motor-eff

              divide old-watts by 1000 giving old-kilowatts rounded
              move old-kilowatts to old-kw-out

              compute new-watts rounded =
                       (quantity2 * hp-conversion-factor *
                        square-root3 * power-factor) / motor-eff

              divide new-watts by 1000 giving new-kilowatts rounded
              move new-kilowatts to new-kw-out

           end-if.

       800-disp-result.

      * Move calculated values to displayed values
           move rpm1 to rpm1-out
           move rpm2 to rpm2-out
           move quantity1 to quantity1-out
           move quantity2 to quantity2-out
           move power-factor to power-factor-out
           move pulley-ratio to pulley-ratio-out

      * Motor efficiency numeric value already moved to displayed value
      * in the motor-efficiency-data-entry paragraph because the
      * entered % data entry value needs to be divided by 100 prior to
      * using the numeric data in the calculation formula.
      *     move motor-eff to motor-eff-out

           display spaces
           display "***** RESULTS *****"
           display spaces

           if pulley
      *        display pulley-ratio
              display "Pulley ratio: " pulley-ratio-out ":1"
           end-if

      *    display rpm1
           display "Old load RPM: " rpm1-out
      *     display quantity1
           display "Old " description " : " quantity1-out
           if law = 3
              display "Old motor electrical power: " old-kw-out " KW"
           end-if

           display spaces
      *     display rpm2
           display "New load RPM: " rpm2-out
      *    display quantity2
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

       999-end-program.
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
              perform ac-powered-query
           else
      *        display "Power Factor is numeric"
              compute power-factor = function numval(power-factor-in)
      *        display power-factor
           end-if

           evaluate power-factor
              when 0
      *           display "End Program from power factor"
                 perform 999-end-program

              when other
      * Comment out after debugging:
      *************************************
      *           display "power-factor-in: " power-factor-in
      *           display "power-factor: " power-factor
      *************************************
                 if power-factor > 1
                    display "Power factor must be <= 1.00"
                    perform ac-powered-query
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
              perform motor-efficiency-data-entry
           else
              compute motor-eff = function numval(motor-eff-in)
      *        display motor-eff
           end-if

           evaluate motor-eff
              when 0 perform 999-end-program
              when other
                 if motor-eff > 100 then
                    display spaces
                    display "Motor efficiency % must be <= 100"
                    perform motor-efficiency-data-entry
                 end-if
           end-evaluate

      *     display "Motor eff: " motor-eff
      * Motor efficiency numeric value moved to displayed value here
      * in the motor-efficiency-data-entry paragraph because the
      * entered % data entry value needs to be divided by 100 prior to
      * using the numeric data in the calculation formula.
           move motor-eff to motor-eff-out
           divide motor-eff by 100 giving motor-eff rounded.

       pulley-data-entry.
           display spaces
           display "Enter motor pulley diameter: " with no advancing
           accept pulley-diameter1-in

      * Did the user enter a valid numeric value?
           if function test-numval(pulley-diameter1-in)
              IS NOT EQUAL ZERO then
              display "Motor pulley diameter" not-numeric
           else
              compute pulley-diameter1 =
                       function numval(pulley-diameter1-in)
           end-if

           evaluate true
           when pulley-diameter1 IS EQUAL ZERO
              perform 999-end-program

           when pulley-diameter1 IS NEGATIVE
              display quantity-too-small
              perform pulley-data-entry

           when pulley-diameter1 > 99999.9
              display quantity-too-much
              perform pulley-data-entry

           end-evaluate

           display "Enter load pulley diameter: " with no advancing
           accept pulley-diameter2-in
      * Did the user enter a valid numeric value?
           if function test-numval(pulley-diameter2-in)
              IS NOT EQUAL ZERO then
              display "Load pulley diameter" not-numeric
           else
              compute pulley-diameter2 =
                       function numval(pulley-diameter2-in)
           end-if

           evaluate true
           when pulley-diameter2 IS EQUAL ZERO
              perform 999-end-program

           when pulley-diameter2 IS NEGATIVE
              display quantity-too-small
              perform pulley-data-entry

           when pulley-diameter2 > 9999.9
              display quantity-too-much
              perform pulley-data-entry

           end-evaluate

           divide pulley-diameter1 by pulley-diameter2
                  giving pulley-ratio rounded.
