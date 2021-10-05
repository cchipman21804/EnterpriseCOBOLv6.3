       identification division.
       program-id.    capreact.
       author.        Cliff Chipman.
      *
      * Capacitive reactance is defined as the opposition created by a
      * capacitor to the flow of alternating current.
      *
      * Xc = Ec / Ic
      * where: Xc = capacitive reactance
      *        Ec = AC voltage across the capacitor
      *        Ic = current through the capacitor
      *
      * Xc = 1 / (2 * pi * F * C)
      * where: Xc = capacitive reactance in ohms
      *        pi = 3.14159265
      *        F  = frequency of AC voltage in volts
      *        C  = value of capacitor in Farads
      *
       data division.
       working-storage section.
      *
      * Data-entry-fields.
       01 freq-in            pic x(9).
       01 cap-in             pic x(17).
      *
      * Calculated-fields.
       01 xc                 pic 9(12)v9.
       01 f                  pic 9(9).
       01 c                  pic 9(4)v9(12).
      *
      * Displayed-fields.
       01 xc-out             pic ZZZ,ZZZ,ZZZ,ZZ9.9.

      * Constant-values.
       01 min-freq           pic 9 value 1.
       01 max-freq           pic 9(9) value 999999999.
       01 min-cap            pic 9v9(12) value 0.000000000001.
       01 max-cap            pic 9(4)v9(14) value 9999.999999999999.

      * Constant-text.
       01 dot                pic x value ".".
       01 not-numeric        pic x(16) value " is NOT numeric.".
       01 freq-too-small     pic x(30)
                             value "Frequency must be >= 1 Hertz".
       01 freq-too-much      pic x(30)
                             value "Frequency must be < 1 GHz".
       01 cap-too-small      pic x(30)
                             value "Capacitance must be >= 1pF".
       01 cap-too-much       pic x(30)
                             value "Capacitance must be < 10,000uF".

       procedure division.
       display-title-screen.
           display spaces
           display "***** CAPACITIVE REACTANCE CALCULATOR *****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "October 3, 2020"
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

       capacitance-data-entry.
           display "Enter capacitance in uF: " with no advancing
           accept cap-in

      * Did the user enter a valid numeric value?
           if function test-numval(cap-in) IS NOT EQUAL ZERO then
              display "Capacitance " not-numeric
              display spaces
              go to capacitance-data-entry
           else
              compute c = function numval(cap-in)
           end-if

           if c IS EQUAL ZERO then go to end-program

           if c > max-cap
              display cap-too-much
              display spaces
              go to capacitance-data-entry
           end-if

           if c < min-cap
              display cap-too-small
              display spaces
              go to capacitance-data-entry
           end-if.
      *    move relative-humidity to rh-out.

       calculate-reactance.
           divide 1000000 into c
           compute xc = 2 * function pi * f * c
           divide 1 into xc

           move xc to xc-out
           display spaces
           display "Xc is " xc-out " ohms".

       end-program.
           display spaces
           display "***** CAPACITIVE REACTANCE CALCULATOR ENDS *****"
           display spaces
           stop run.

