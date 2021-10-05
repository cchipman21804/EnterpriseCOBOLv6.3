      *****************************************************************
      * SAVINGS                                                       *
      *                                                               *
      * A simple program that calculates the future value of an       *
      * initial investment in a savings account which is compounded   *
      * on a monthly basis.                                           *
      *                                                               *
      *****************************************************************
       identification division.
       program-id.   savings.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Savings account formula:                                      *
      *                                                               *
      *              F = P (1+r)^n                                    *
      *                                                               *
      *       F = Future Value                                        *
      *       P = Present Value (initial investment)                  *
      *       r = monthly-interest (rate)                             *
      *       n = term (months)                                       *
      *                                                               *
      *****************************************************************

      * Data-entry-fields.
       01 principal-in       pic x(9).
       01 int-in             pic x(5).
       01 term-in            pic x(5).

      * Calculated-fields.
       01 PRESENT-VALUE      PIC 9(6)V99 USAGE COMP.
       01 ANNUAL-INTEREST    PIC 99V9(4) USAGE COMP.
       01 MONTHLY-INTEREST   PIC 9V9(6) USAGE COMP.
       01 ANNUAL-TERM        PIC 99V99 USAGE COMP.
       01 LOAN-TERM-MONTHS   PIC 999 USAGE COMP.
       01 NUMERATOR          PIC 9(9)V9(6) USAGE COMP.
       01 DENOMINATOR        PIC 9(9)V9(6) USAGE COMP.
       01 FUTURE-VALUE       PIC 9(9)V99 USAGE COMP.

      * Displayed-fields.
       01 FV-OUT             PIC $ZZZ,ZZZ,ZZ9.99 USAGE DISPLAY.
       01 PRINCIPAL          PIC $ZZZ,ZZ9.99 USAGE DISPLAY.
       01 INTEREST-RATE      PIC Z9.99 USAGE DISPLAY.
       01 LOAN-TERM-OUT      PIC Z9.99 USAGE DISPLAY.
       01 MONTHS-OUT         PIC ZZ9 USAGE DISPLAY.

      * Constant-values.
       01 min-val            pic 9v99 value 0.01.
       01 max-val            pic 9(6)v99 value 999999.99.
       01 min-int            pic 9v99 value 0.01.
       01 max-int            pic 99v99 value 26.
       01 min-term           pic 9v99 value 0.01.
       01 max-term           pic 99 value 30.

      * Constant-text.
       01 not-numeric        pic x(16) value " is NOT numeric.".
       01 quantity-too-small pic x(22)
                             value "Value must be >= $0.01".
       01 quantity-too-much  pic x(28)
                             value "Value must be <= $999,999.99".
       01 interest-too-much  pic x(23)
                             value "Interest must be <= 26%".
       01 interest-too-small pic x(21)
                             value "Interest must be > 0%".
       01 term-too-short     pic x(31)
                             value "Term must be at least 0.25 year".
       01 term-too-long      pic x(24)
                             value "Term must be <= 30 years".
       01 t                  pic x(6) value "Term: ".
       01 y                  pic x(8) value " years (".
       01 m                  pic x(9) value " months )".

       procedure division.
       display-title-screen.
           display spaces
           display "* MONTHLY SAVINGS ACCT CALCULATOR UTILITY BEGINS *"
           display "Written by, Clifford A. Chipman, EMIT"
           display "June 18, 2020"
           display spaces
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces
           display "Enter zero for any parameter to end the program."
           display spaces.

       present-value-data-entry.
           display "Enter initial investment: " with no advancing
           accept principal-in

      * Did the user enter a valid numeric value?
           if function test-numval(principal-in) IS NOT EQUAL ZERO then
              display "Principal" not-numeric
              display spaces
              go to present-value-data-entry
           else
              compute present-value = function numval(principal-in)
           end-if

           if present-value IS EQUAL ZERO then
              go to end-program
           end-if

           if present-value > max-val
              display quantity-too-much
              display spaces
              go to present-value-data-entry
           end-if

           if present-value < min-val
              display quantity-too-small
              display spaces
              go to present-value-data-entry
           end-if

           move present-value to principal.

       interest-rate-data-entry.
           display "Enter annual interest rate %: " with no advancing
           accept int-in

      * Did the user enter a valid numeric value?
           if function test-numval(int-in) IS NOT EQUAL ZERO then
              display "Interest Rate" not-numeric
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

           move annual-interest to interest-rate.

       term-data-entry.
           display "Enter term in years: " with no advancing
           accept term-in

      * Did the user enter a valid numeric value?
           if function test-numval(term-in) IS NOT EQUAL ZERO then
              display "Term" not-numeric
              display spaces
              go to term-data-entry
           else
              compute annual-term = function numval(term-in)
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

           if annual-term IS EQUAL ZERO then
              go to end-program
           end-if

           move annual-term to loan-term-out.

       calculate-it.

      *****************************************************************
      *                                                               *
      * Savings account formula:                                      *
      *                                                               *
      *              F = P (1+r)^n                                    *
      *                                                               *
      *       F = Future Value                                        *
      *       P = Present Value (initial investment)                  *
      *       r = monthly-interest (rate)                             *
      *       n = term (months)                                       *
      *                                                               *
      *****************************************************************

           divide annual-interest by 100 giving annual-interest rounded
           multiply 12 by annual-term giving loan-term-months
           divide annual-interest by 12 giving monthly-interest rounded

           compute denominator = (1 + monthly-interest) **
                                                       loan-term-months

           compute future-value = present-value * denominator

           move future-value to fv-out
           move loan-term-months to months-out.

       disp-result.
           display "Principal: " principal
           display "Interest Rate: " interest-rate "%"
           display t loan-term-out y months-out m
           display "Future Value: " fv-out.

       end-program.
           display spaces
           display "** MONTHLY SAVINGS ACCT CALCULATOR UTILITY ENDS **"
           display spaces
           stop run.
