       identification division.
       program-id.    mixedair.
       author.        Cliff Chipman.
      *
      ******************************************************************
      *                                                                *
      * Mixed air calculations can be performed in one of two ways:
      *
      * - CO2 concentration (in ppm)
      * - air temperature   (in deg)
      *
      *        Xr-Xm
      * %OA = ------- * 100
      *        Xr-Xo
      *
      * Where: %OA = percentage of outside air
      *        Xr  = measurement of return air characteristic
      *        Xm  = measurement of mixed air characteristic
      *        Xo  = measurement of outside air characteristic
      *
      ******************************************************************
      *                                                                *
      *            TYPICAL COMMERCIAL/INDUSTRIAL AIR HANDLER           *
      *                                                                *
      *                                     Tra sensor                 *
      *                                      |             ___________ *
      *                                      |            |           |*
      *         _____________________________v____________|           |*
      *                                                               |*
      *         /<< Exhaust Air           << *  Return Air            |*
      *         _______________       ____________________            |*
      *                        |     |                    |           |*
      * Outside                |  V  |                    |   Condx   |*
      *   Air                  |  V  |                    |   space   |*
      *         _______________|     |____________________|           |*
      *                         Mixed Air                             |*
      *         />>               *       * * * Supply Air >>         |*
      *         __________________________________________            |*
      *                           ^       ^ ^ ^           |           |*
      *                           |       | | |           |___________|*
      *                           |       | | |                        *
      *      * <--Toa            Tma      F F H                        *
      *          sensor          sensor   A I T         Tma-Tra        *
      *         (N side)                  N L G  %OA = --------- x 100 *
      *                                     T /         Toa-Tra        *
      *                                     E C                        *
      *                                     R L                        *
      *                                       G                        *
      *by,Cliff Chipman,EMIT                                 Jun18,2020*
      ******************************************************************
      *
       data division.
       working-storage section.

      * Data-entry-fields.
       01 return-air-in      pic x(4).
       01 mixed-air-in       pic x(4).
       01 outside-air-in     pic x(4).

      * Calculated-fields.
       01 return-air         pic s9(4) sign is leading separate.
       01 mixed-air          pic s9(4) sign is leading separate.
       01 outside-air        pic s9(4) sign is leading separate.
       01 numerator          pic s9(4) sign is leading separate.
       01 denominator        pic s9(4) sign is leading separate.
       01 pct-oa             pic 99v99.

      * Displayed-fields.
       01 pct-oa-out         pic +Z9.9.

      * Constant-values.
       01 min-value          pic s99  value -40.
       01 max-value          pic 9999 value 2000.

      * Constant-text.
       01 dot                pic x value ".".
       01 not-numeric        pic x(16) value " is NOT numeric.".
       01 quantity-too-small pic x(29)
                             value "Quantity must be >= -40".
       01 quantity-too-much  pic x(29)
                             value "Quantity must be <= 2000".

       procedure division.
       display-title-screen.
           display spaces
           display "***** MIXED AIR CALCULATOR UTILITY BEGINS *****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "June 18, 2020"
           display spaces
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces.

       return-air-data-entry.
           display spaces
           display "Enter return air (deg/ppm): "
                    with no advancing
           accept return-air-in

      * Did the user enter a valid numeric value?
           if function test-numval(return-air-in) IS NOT EQUAL ZERO then
              display "Return air " not-numeric
              go to return-air-data-entry
           else
              compute return-air = function numval(return-air-in)
           end-if

           if return-air IS EQUAL ZERO then go to end-program

           if return-air > max-value
              display quantity-too-much
              go to return-air-data-entry
           end-if

           if return-air < min-value
              display quantity-too-small
              go to return-air-data-entry
           end-if.

       mixed-air-data-entry.
           display "Enter mixed air (deg/ppm): " with no advancing
           accept mixed-air-in

      * Did the user enter a valid numeric value?
           if function test-numval(mixed-air-in) IS NOT EQUAL ZERO then
              display "Mixed air " not-numeric
              display spaces
              go to mixed-air-data-entry
           else
              compute mixed-air = function numval(mixed-air-in)
           end-if

           if mixed-air IS EQUAL ZERO then go to end-program

           if mixed-air > max-value
              display quantity-too-much
              display spaces
              go to mixed-air-data-entry
           end-if

           if mixed-air < min-value
              display quantity-too-small
              display spaces
              go to mixed-air-data-entry
           end-if.

       outside-air-data-entry.
           display "Enter outside air (deg/ppm): " with no advancing
           accept outside-air-in

      * Did the user enter a valid numeric value?
           if function test-numval(outside-air-in)
              IS NOT EQUAL ZERO then
              display "Outside air " not-numeric
              display spaces
              go to outside-air-data-entry
           else
              compute outside-air = function numval(outside-air-in)
           end-if

           if outside-air IS EQUAL ZERO then go to end-program

           if outside-air > max-value
              display quantity-too-much
              display spaces
              go to outside-air-data-entry
           end-if

           if outside-air < min-value
              display quantity-too-small
              display spaces
              go to outside-air-data-entry
           end-if.

       calculate-pct-oa.
           subtract mixed-air from return-air giving numerator
           subtract outside-air from return-air giving denominator
           divide numerator by denominator giving pct-oa rounded
           multiply 100 by pct-oa

           move pct-oa to pct-oa-out
           display spaces
           display "Outside Air %: " pct-oa-out.

       end-program.
           display spaces
           display "***** MIXED AIR CALCULATOR UTILITY ENDS *****"
           display spaces
           stop run.
      *
      ******************************************************************
      *                                                                *
      *            TYPICAL COMMERCIAL/INDUSTRIAL AIR HANDLER           *
      *                                                                *
      *                                     Tra sensor                 *
      *                                      |             ___________ *
      *                                      |            |           |*
      *         _____________________________v____________|           |*
      *                                                               |*
      *         /<< Exhaust Air           << *  Return Air            |*
      *         _______________       ____________________            |*
      *                        |     |                    |           |*
      * Outside                |  V  |                    |   Condx   |*
      *   Air                  |  V  |                    |   space   |*
      *         _______________|     |____________________|           |*
      *                         Mixed Air                             |*
      *         />>               *       * * * Supply Air >>         |*
      *         __________________________________________            |*
      *                           ^       ^ ^ ^           |           |*
      *                           |       | | |           |___________|*
      *                           |       | | |                        *
      *      * <--Toa            Tma      F F H                        *
      *          sensor          sensor   A I T         Tma-Tra        *
      *         (N side)                  N L G  %OA = --------- x 100 *
      *                                     T /         Toa-Tra        *
      *                                     E C                        *
      *                                     R L                        *
      *                                       G                        *
      *by,Cliff Chipman,EMIT                                 Jun18,2020*
      ******************************************************************
