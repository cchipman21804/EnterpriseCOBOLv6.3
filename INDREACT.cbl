       identification division.
       program-id.    indreact.
       author.        Cliff Chipman.
      *
      * Inductive reactance is defined as the opposition created by a
      * inductor to the flow of alternating current.
      *
      * XL = EL / IL
      * where: XL = inductive reactance
      *        EL = AC voltage across the inductor
      *        IL = current through the inductor
      *
      * XL = 2 * pi * F * C
      * where: XL = inductive reactance in ohms
      *        pi = 3.14159265
      *        F  = frequency of AC voltage in volts
      *        L  = value of inductor in Henries
      *
       data division.
       working-storage section.
      *
      * Data-entry-fields.
       01 freq-in            pic x(9).
       01 ind-in             pic x(17).
      *
      * Calculated-fields.
       01 xl                 pic 9(12)v9.
       01 f                  pic 9(9).
       01 l                  pic 9(4)v9(12).
      *
      * Displayed-fields.
       01 xl-out             pic ZZZ,ZZZ,ZZZ,ZZ9.9.

      * Constant-values.
       01 min-freq           pic 9 value 1.
       01 max-freq           pic 9(9) value 999999999.
       01 min-ind            pic 9v9(12) value 0.000000000001.
       01 max-ind            pic 9(4)v9(14) value 9999.999999999999.

      * Constant-text.
       01 dot                pic x value ".".
       01 not-numeric        pic x(16) value " is NOT numeric.".
       01 freq-too-small     pic x(30)
                             value "Frequency must be >= 1 Hertz".
       01 freq-too-much      pic x(30)
                             value "Frequency must be < 1 GHz".
       01 ind-too-small      pic x(30)
                             value "Inductance must be >= 1pH".
       01 ind-too-much       pic x(30)
                             value "Inductance must be < 10,000uH".

       procedure division.
       display-title-screen.
           display spaces
           display "***** INDUCTIVE REACTANCE CALCULATOR *****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "October 4, 2020"
           display spaces
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces.

       frequency-data-entry.
           display spaces
           display "Enter frequency: " with no advancing
           accept freq-in

      * Did the user enter a valid numeric value?
           if function test-numval(freq-in) IS NOT EQUAL ZERO then
              display "Frequency " not-numeric
              go to frequency-data-entry
           else
              compute f = function numval(freq-in)
           end-if

           if f IS EQUAL ZERO then go to end-program

           if f > max-freq
              display freq-too-much
              go to frequency-data-entry
           end-if
      *
           if f < min-freq
              display freq-too-small
              go to frequency-data-entry
           end-if.
      *    move f to freq-out.

       inductance-data-entry.
           display "Enter inductance in Henries: " with no advancing
           accept ind-in

      * Did the user enter a valid numeric value?
           if function test-numval(ind-in) IS NOT EQUAL ZERO then
              display "Inductance " not-numeric
              display spaces
              go to inductance-data-entry
           else
              compute l = function numval(ind-in)
           end-if

           if l IS EQUAL ZERO then go to end-program

           if l > max-ind
              display ind-too-much
              display spaces
              go to inductance-data-entry
           end-if

           if l < min-ind
              display ind-too-small
              display spaces
              go to inductance-data-entry
           end-if.
      *    move relative-humidity to rh-out.

       calculate-reactance.
      *    divide 1000000 into l
           compute xl = 2 * function pi * f * l
      *    divide 1 into xc

           move xl to xl-out
           display spaces
           display "XL is " xl-out " ohms".

       end-program.
           display spaces
           display "***** INDUCTIVE REACTANCE CALCULATOR ENDS *****"
           display spaces
           stop run.

