      *****************************************************************
      * CALCULATE % FUEL SAVINGS BASED ON EFFICIENCY UPGRADE          *
      *                                                               *
      * A simple program that calculates the fuel savings percentage  *
      * based on a combustion efficiency upgrade,                     *
      *                                                               *
      *****************************************************************

       identification division.
       program-id.   fuelsave.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Percent Fuel Savings formula:                                 *
      *                                                               *
      * %FuelSavings = [(neweff - oldeff) / neweff] * 100             *
      *                                                               *
      *****************************************************************

      * Data-entry-fields.
       01 old-eff-in         pic x(5).
       01 new-eff-in         pic x(5).

      * Calculated-fields.
       01 OLDEFF             pic 999V99 USAGE COMP.
       01 NEWEFF             pic 999V99 USAGE COMP.
       01 PCTEFF             pic S999V9999 USAGE COMP.
       01 NUMERATOR          pic S99V99 USAGE COMP.

      * Displayed-fields.
       01 OLD-EFF-OUT        pic Z9.99 USAGE DISPLAY.
       01 NEW-EFF-OUT        pic Z9.99 USAGE DISPLAY.
       01 PCT-EFF-OUT        pic Z9.99 USAGE DISPLAY.

      * Constant-values.
       01 min-val            pic 9 value 1.
       01 max-val            pic 99v99 value 99.99.

      * Constant-text.
       01 not-numeric        pic x(16) value " is NOT numeric.".
       01 quantity-too-small pic x(18)
                             value "Value must be >= 1".
       01 quantity-too-much  pic x(22)
                             value "Value must be <= 99.99".
       01 quantities-equal1  pic x(24)
           value "Efficiencies are equal. ".
       01 quantities-equal2  pic x(30)
           value "There will be no fuel savings.".

       01 quantity-lower1    pic x(25)
           value "New efficiency is lower. ".
       01 quantity-lower2    pic x(42)
           value "This will result in negative fuel savings.".

       procedure division.
       display-title-screen.
           display spaces
           display "**** % FUEL SAVINGS CALCULATOR UTILITY BEGINS ****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "June 17, 2020"
           display spaces
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces
           display "Enter zero for any parameter to end the program."
           display spaces.

       old-eff-data-entry.
           display "Enter old efficiency %: " with no advancing
           accept old-eff-in

      * Did the user enter a valid numeric value?
           if function test-numval(old-eff-in) IS NOT EQUAL ZERO then
              display "Old Efficiency" not-numeric
              display spaces
              go to old-eff-data-entry
           else
              compute oldeff = function numval(old-eff-in)
           end-if

           if oldeff = 0
                  go to end-program
           end-if

           if oldeff < min-val
                  display quantity-too-small
                  display spaces
                  go to old-eff-data-entry
           end-if

           if oldeff > max-val
                  display quantity-too-much
                  display spaces
                  go to old-eff-data-entry
           end-if.

       new-eff-data-entry.
           display "Enter new efficiency %: " with no advancing
           accept new-eff-in

      * Did the user enter a valid numeric value?
           if function test-numval(new-eff-in) IS NOT EQUAL ZERO then
              display "New Efficiency" not-numeric
              display spaces
              go to new-eff-data-entry
           else
              compute neweff = function numval(new-eff-in)
           end-if

           if neweff = 0
                  go to end-program
           end-if

           if neweff < min-val
                  display quantity-too-small
                  go to new-eff-data-entry
           end-if

           if neweff > max-val
                  display quantity-too-much
                  go to new-eff-data-entry
           end-if

           if neweff = oldeff
                  display quantities-equal1 quantities-equal2
                  go to end-program
           end-if

           if neweff < oldeff
                  display quantity-lower1 quantity-lower2
                  go to end-program
           end-if.

       calculate-it.

      *****************************************************************
      *                                                               *
      * Percent Fuel Savings formula:                                 *
      *                                                               *
      * %FuelSavings = [(neweff - oldeff) / neweff] * 100             *
      *                                                               *
      *****************************************************************

      *     compute pcteff = (neweff - oldeff) / neweff

           subtract oldeff from neweff giving numerator
           divide numerator by neweff giving pcteff rounded

           multiply 100 by pcteff

           move oldeff to old-eff-out
           move neweff to new-eff-out
           move pcteff to pct-eff-out.

       disp-result.
           display "Old Efficiency: " old-eff-out "%"
           display "New Efficiency: " new-eff-out "%"
           display "Fuel Savings: " pct-eff-out "%".

       end-program.
           display spaces
           display "***** % FUEL SAVINGS CALCULATOR UTILITY ENDS *****"
           display spaces
           stop run.
