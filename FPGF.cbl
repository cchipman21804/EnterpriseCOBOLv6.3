      *****************************************************************
      * FIND P GIVEN F                                                *
      *                                                               *
      * A simple program that calculates the Present Value of an      *
      * investment after the user enters the Future Value, the        *
      * annual interest rate, and the term of the investment.         *
      *                                                               *
      *****************************************************************

       identification division.
       program-id.   fpgf.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Present Value formula:                                        *
      *                                                               *
      *              P = F * 1/ (1+i) ** n                            *
      *                                                               *
      *       F = Future Value                                        *
      *       P = Present Value                                       *
      *       i = annual-interest (rate)                              *
      *       n = loan-term (years)                                   *
      *                                                               *
      *****************************************************************

      * Data-entry-fields.
       01 FV-IN              pic x(7).
       01 INT-IN             pic x(5).
       01 TERM-IN            pic xx.

      * Calculated-fields.
       01 PRESENT-VALUE      PIC 9(5)v99.
       01 ANNUAL-INTEREST    PIC 99V9(4).
       01 ANNUAL-TERM        PIC 99.
       01 NUMERATOR          PIC 9(9)V9(6).
       01 DENOMINATOR        PIC 9(9)V9(6).
       01 FUTURE-VALUE       PIC 9(9)V99.

      * Displayed-fields.
       01 PV-OUT             PIC $ZZ,ZZ9.99 USAGE DISPLAY.
       01 FV-OUT             PIC $ZZZ,ZZZ,ZZ9.99 USAGE DISPLAY.
       01 INTEREST-RATE      PIC Z9.99 USAGE DISPLAY.
       01 TERM-OUT           PIC Z9 USAGE DISPLAY.

      * Constant-values.
       01 min-val            pic 9(7)v99 value 0.01.
       01 max-val            pic 9(7)v99 value 9999999.
       01 min-int            pic 99v99 value 0.01.
       01 max-int            pic 99v99 value 26.
       01 min-term           pic 99 value 1.
       01 max-term           pic 99 value 30.

      * Constant-text.
       01 not-numeric        pic x(16) value " is NOT numeric.".
       01 quantity-too-small pic x(22)
                             value "Value must be >= $0.01".
       01 quantity-too-much  pic x(27)
                             value "Value must be <= $9,999,999".
       01 interest-too-much  pic x(23)
                             value "Interest must be <= 26%".
       01 interest-too-small pic x(21)
                             value "Interest must be > 0%".
       01 term-too-short     pic x(29)
                             value "Term must be at least 1 year.".
       01 term-too-long      pic x(25)
                             value "Term must be <= 30 years.".

       procedure division.
       display-title-screen.
           display spaces
           display "***** PRESENT VALUE CALCULATOR UTILITY BEGINS *****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "June 16, 2020"
           display spaces
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces
           display "Enter zero for any parameter to end the program."
           display spaces.

       future-value-data-entry.
           display "Enter future value: " with no advancing
           accept fv-in

      * Did the user enter a valid numeric value?
           if function test-numval(fv-in) IS NOT EQUAL ZERO then
              display "Future Value " not-numeric
              display spaces
              go to future-value-data-entry
           else
              compute future-value = function numval(fv-in)
           end-if

           if future-value IS EQUAL ZERO then
              go to end-program
           end-if

           if future-value > max-val
              display quantity-too-much
              display spaces
              go to future-value-data-entry
           end-if

           if future-value < min-val
              display quantity-too-small
              display spaces
              go to future-value-data-entry
           end-if

           move future-value to fv-out.

       interest-rate-data-entry.
           display "Enter annual interest rate %: " with no advancing
           accept int-in

      * Did the user enter a valid numeric value?
           if function test-numval(int-in) IS NOT EQUAL ZERO then
              display "Interest Rate " not-numeric
              display spaces
              go to interest-rate-data-entry
           else
              compute annual-interest = function numval(int-in)
           end-if

           if annual-interest > max-int
              display interest-too-much
              display spaces
              go to interest-rate-data-entry
           end-if

           if annual-interest IS LESS THAN ZERO then
              display interest-too-small
              display spaces
              go to interest-rate-data-entry
           end-if

           if annual-interest IS EQUAL ZERO then
              go to end-program
           end-if

           move annual-interest to interest-rate
           divide annual-interest by 100 giving annual-interest.

       term-data-entry.
           display "Enter term in years as:"
           accept term-in

      * Did the user enter a valid numeric value?
           if function test-numval(term-in) IS NOT EQUAL ZERO then
              display "Term " not-numeric
              display spaces
              go to term-data-entry
           else
              compute annual-term = function numval(term-in)
           end-if

           if annual-term IS EQUAL ZERO then
              go to end-program
           end-if

           if annual-term > max-term then
              display term-too-long
              display spaces
              go to term-data-entry
           end-if

           if annual-term < min-term then
              display term-too-short
              display spaces
              go to term-data-entry
           end-if

           move annual-term to term-out.

       calculate-it.

      *****************************************************************
      *                                                               *
      * Present Value formula:                                        *
      *                                                               *
      *              P = F * 1/ (1+i) ** n                            *
      *                                                               *
      *       F = Future Value                                        *
      *       P = Present Value                                       *
      *       i = annual-interest (rate)                              *
      *       n = loan-term (years)                                   *
      *                                                               *
      *****************************************************************

           compute denominator = (1 / (1 + annual-interest) **
                                   annual-term)

           multiply future-value by denominator giving present-value
                    rounded

           move present-value to pv-out

           display spaces
           display "Future Value: " fv-out
           display "Term: " term-out " years"
           display "Interest Rate: " interest-rate "%"
           display "You will need to invest: " pv-out.

       end-program.
           display spaces
           display "***** PRESENT VALUE CALCULATOR UTILITY ENDS *****"
           display spaces
           stop run.
