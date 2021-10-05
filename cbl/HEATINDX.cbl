       identification division.
       program-id.  heatindx.
       author.      Chipman.

       data division.
       working-storage section.

      * Data-entry-fields.
       01 outside-temp-in                      pic x(5).
       01 relative-humidity-in                 pic x(5).

      * Calculation fields.
       01 outside-temp                         pic 999v9.
       01 relative-humidity                    pic 999v9.
       01 adjustment                           pic 9v9(9) value zero.
       01 heat-index                           pic 999v99.

      * The Rothfusz regression formula:
      * HI = -42.379 + 2.04901523 * T + 10.14333127 * RH
      *      - 0.22475541 * T * RH - 0.00683783 * T^2
      *      - 0.05481717 * RH^2 + 0.00122874 * T^2 * RH
      *      + 0.00085282 * T * RH^2 - 0.00000199 * T^2 * RH^2
      *
      * where T is temperature in degrees F
      * and RH is relative humidity in percent.

       01 constant-factors.
           02 negative-one                     pic s9
                                               value -1.

           02 cf1                              pic 99v999
                                               value 42.379.

           02 cf2                              pic 9v9(8)
                                               value 2.04901523.

           02 cf3                              pic 99v9(8)
                                               value 10.14333127.

           02 cf4                              pic 9v9(8)
                                               value 0.22475541.

           02 cf5                              pic 9v9(8)
                                               value 0.00683783.

           02 cf6                              pic 9v9(8)
                                               value 0.05481717.

           02 cf7                              pic 9v9(8)
                                               value 0.00122874.

           02 cf8                              pic 9v9(8)
                                               value 0.00085282.

           02 cf9                              pic 9v9(8)
                                               value 0.00000199.

      * Constant-values.
       01 min-temp                             pic 99   value 68.
       01 max-temp                             pic 999  value 125.
       01 min-rh                               pic 9    value 1.
       01 max-rh                               pic 999  value 100.

      * Constant-text.
       01 not-numeric                          pic x(15)
                                      value "is NOT numeric.".

       01 too-small                            pic x(11)
                                      value "must be >= ".

       01 too-much                             pic x(11)
                                      value "must be <= ".

      * Display fields.
       01 outside-temp-out                     pic ZZ9.9.
       01 relative-humidity-out                pic ZZ9.9.
       01 heat-index-out                       pic ZZ9.9.

       procedure division.
       main-para.
           perform display-title-screen
           perform outside-temp-data-entry
           perform relative-humidity-data-entry
           perform calculate-it
           perform display-results
           go to end-program.

       display-title-screen.
           display spaces
           display "********** HEAT INDEX CALCULATOR BEGINS **********"
           display "Written by, Clifford A. Chipman, EMIT "
                    with no advancing
           display "July 4, 2020"
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces
           display "Enter zero for any parameter to end the program.".

       outside-temp-data-entry.
           display spaces
           display "Enter outdoor dry bulb temperature in degF "
                    with no advancing
           display "(nnn.n): " with no advancing
           accept outside-temp-in

      * Did the user enter a valid numeric value?
           if function test-numval(outside-temp-in) IS NOT EQUAL ZERO
           then
              display "Temperature " not-numeric
              go to outside-temp-data-entry
           else
              compute outside-temp = function numval(outside-temp-in)
           end-if

           if outside-temp IS EQUAL ZERO
           then
              go to end-program
           end-if

           if outside-temp > max-temp
              display "Temperature " too-much max-temp " degF."
              go to outside-temp-data-entry
           end-if

           if outside-temp < min-temp
              display "Temperature " too-small min-temp " degF."
              go to outside-temp-data-entry
           end-if

           move outside-temp to outside-temp-out.

       relative-humidity-data-entry.
           display "Enter relative humidity as % " with no advancing
           display "(nnn.n): " with no advancing
           accept relative-humidity-in

      * Did the user enter a valid numeric value?
           if function test-numval(relative-humidity-in)
           IS NOT EQUAL ZERO then
              display "Relative humidity " not-numeric
              display spaces
              go to relative-humidity-data-entry
           else
              compute relative-humidity =
                       function numval(relative-humidity-in)
           end-if

           if relative-humidity IS EQUAL ZERO
           then
              go to end-program
           end-if

           if relative-humidity > max-rh
              display "Relative humidity " too-much max-rh "%."
              display spaces
              go to relative-humidity-data-entry
           end-if

           if relative-humidity < min-rh
              display "Relative humidity " too-small min-rh "%."
              display spaces
              go to relative-humidity-data-entry
           end-if

           move relative-humidity to relative-humidity-out.

       calculate-it.
      * The Rothfusz regression is not appropriate when conditions of
      * temperature and humidity warrant a heat index value below about
      * 80 degrees F. In those cases, a simpler formula is applied to
      * calculate values consistent with Steadman's results:
      *
      * HI = 0.5 * {T + 61.0 + [(T-68.0)*1.2] + (RH*0.094)}
      *
      * In practice, the simple formula is computed first and the result
      * averaged with the temperature. If this heat index value is 80
      * degrees F or higher, the full regression equation along with any
      * adjustments as described below are applied.
      *
           compute heat-index rounded = 0.5 * (outside-temp + 61.0 +
                   ((outside-temp - 68.0) * 1.2) + relative-humidity *
                   0.094)

      *     display "Simple heat index: " heat-index

           compute heat-index rounded =
                   function mean(heat-index, outside-temp)

      *     display "Average of heat index & outside temp: " heat-index

           evaluate TRUE
           when outside-temp IS GREATER THAN OR EQUAL TO 80 AND
                outside-temp IS LESS THAN OR EQUAL TO 112 AND
                relative-humidity IS LESS THAN 13

              compute heat-index rounded =
              negative-one * cf1 + cf2 * outside-temp +
                             cf3 * relative-humidity -
                             cf4 * outside-temp * relative-humidity -
                             cf5 * outside-temp ** 2 -
                             cf6 * relative-humidity ** 2 +
                             cf7 * outside-temp ** 2 *
                                   relative-humidity +
                             cf8 * outside-temp *
                                   relative-humidity ** 2 -
                             cf9 * outside-temp ** 2 *
                                   relative-humidity ** 2

              compute adjustment rounded =
                      ((13 - relative-humidity) / 4) *
                      function sqrt((17 -
                      function abs(outside-temp - 95)) / 17)
              subtract adjustment from heat-index

      *        display spaces
      *        display "outside-temp IS GREATER THAN OR EQUAL TO 80 AND "
      *        display "outside-temp IS LESS THAN OR EQUAL TO 112 AND "
      *        display "relative-humidity IS LESS THAN 13"
      *        display "Adjustment: -" adjustment
      *        display "Adjusted heat index: " heat-index

           when relative-humidity IS GREATER THAN 85 AND
                outside-temp IS GREATER THAN OR EQUAL TO 80 AND
                outside-temp IS LESS THAN OR EQUAL TO 87

              compute heat-index rounded =
              negative-one * cf1 + cf2 * outside-temp +
                             cf3 * relative-humidity -
                             cf4 * outside-temp * relative-humidity -
                             cf5 * outside-temp ** 2 -
                             cf6 * relative-humidity ** 2 +
                             cf7 * outside-temp ** 2 *
                                   relative-humidity +
                             cf8 * outside-temp *
                                   relative-humidity ** 2 -
                             cf9 * outside-temp ** 2 *
                                   relative-humidity ** 2

              compute adjustment rounded =
                      ((relative-humidity - 85) / 10) *
                      ((87 - outside-temp) / 5)
              add adjustment to heat-index

      *        display spaces
      *        display "relative-humidity IS GREATER THAN 85 AND "
      *        display "outside-temp IS GREATER THAN OR EQUAL TO 80 AND "
      *        display "outside-temp IS LESS THAN OR EQUAL TO 87"
      *        display "Adjustment: +" adjustment
      *        display "Adjusted heat index: " heat-index

           when heat-index IS GREATER THAN 80
              compute heat-index rounded =
              negative-one * cf1 + cf2 * outside-temp +
                             cf3 * relative-humidity -
                             cf4 * outside-temp * relative-humidity -
                             cf5 * outside-temp ** 2 -
                             cf6 * relative-humidity ** 2 +
                             cf7 * outside-temp ** 2 *
                                   relative-humidity +
                             cf8 * outside-temp *
                                   relative-humidity ** 2 -
                             cf9 * outside-temp ** 2 *
                                   relative-humidity ** 2

      *        display spaces
      *        display "heat-index IS GREATER THAN 80"
      *        display "Rothfusz regression heat index: " heat-index

           end-evaluate

           move heat-index to heat-index-out.

      *     display "Heat Index: " heat-index
      *     display "Adjustment: " adjustment.

      * The Rothfusz regression formula:
      * HI = -42.379 + 2.04901523 * T + 10.14333127 * RH
      *      - 0.22475541 * T * RH - 0.00683783 * T^2
      *      - 0.05481717 * RH^2 + 0.00122874 * T^2 * RH
      *      + 0.00085282 * T * RH^2 - 0.00000199 * T^2 * RH^2
      *
      * where T is temperature in degrees F
      * and RH is relative humidity in percent.
      *
      *
      * The Rothfusz regression is not valid for extreme temperature and
      * relative humidity conditions beyond the range of data considered
      * by Steadman.

       display-results.
           display spaces
           display "Outdoor Temperature: " outside-temp-out " degF"
           display "Relative Humidity: " relative-humidity-out "%"
           display "Heat Index: " heat-index-out " degF"

           evaluate TRUE
           when heat-index > 79 AND heat-index < 90
              display spaces
              display "************************************************"
              display "** CAUTION - Fatigue possible with prolonged  **"
              display "** exposure and/or physical activity.         **"
              display "************************************************"
           when heat-index >= 90 AND heat-index < 105
              display spaces
              display "************************************************"
              display "*** EXTREME CAUTION - Sunstroke, muscle      ***"
              display "*** cramps, and/or heat exhaustion possible  ***"
              display "*** with prolonged exposure and/or physical  ***"
              display "*** activity.                                ***"
              display "************************************************"
           when heat-index >= 105 AND heat-index < 130
              display spaces
              display "************************************************"
              display "**** DANGER - Sunstroke, muscle cramps,     ****"
              display "**** and/or heat exhaustion likely.         ****"
              display "**** Heatstroke possible with prolonged     ****"
              display "**** exposure and/or physical activity.     ****"
              display "************************************************"
           when heat-index >= 130
              display spaces
              display "************************************************"
              display "***** EXTREME DANGER - Heat stroke likely. *****"
              display "************************************************"
           when other
              display spaces
           end-evaluate.

       end-program.
           display spaces
           display "*********** HEAT INDEX CALCULATOR ENDS ***********"
           display spaces
           stop run.
