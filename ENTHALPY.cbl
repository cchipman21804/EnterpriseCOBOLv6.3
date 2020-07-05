       identification division.
       program-id.    enthalpy.
       author.        Cliff Chipman.
      *
      * The specific enthalpy of moist air can be expressed as:
      *
      * h = ha + x * hw
      *
      * where: ha = specific enthalpy of dry air (BTU/lb)
      *        ha = Cpa * t
      *
      * where: Cpa = specific heat of dry air (0.24 BTU/lb F)
      *        t   = air temperature (between -150 F and +212 F)
      *
      * where: x = humidity ratio (lb/lb)
      *
      * where: hw = specific enthalpy of water vapor (BTU/lb)
      *        hw = Cpw * t + Hwe
      *
      * where: Cpw = specific heat of water vapor (0.444 BTU/lb F)
      *        t   = water vapor temperature
      *        Hwe = evaporation heat of vapor (1061 BTU/lb)
      *
      * h = 0.24 * t + x * (0.444 * t + 1061)
      *
       data division.
       working-storage section.

      * Data-entry-fields.
       01 temp-in            pic xx.
       01 rh-in              pic xxx.

      * Calculated-fields.
       01 calc-enthalpy      pic 999v99.
       01 ha                 pic 999v999.
       01 humidityratio      pic 999.
       01 hw                 pic 9(5)v99.
       01 relative-humidity  pic 999v99.
       01 table-idx          pic 99.
       01 temperature        pic 99.
       01 x                  pic 99v9(9).

      * Displayed-fields.
       01 temp-out           pic ZZ9.
       01 rh-out             pic ZZ9.
       01 enthalpy-out       pic ZZ9.99.

      * Constant-values.
      * The minimum and maximum temperatures are based on the data
      * available from the TRANE Psychrometric Chart
      * Form Number 1-43.190 Jan.1983
       01 min-temp           pic 99 value 30.
       01 max-temp           pic 99 value 87.

      * Constant-text.
       01 dot                pic x value ".".
       01 not-numeric        pic x(16) value " is NOT numeric.".
       01 quantity-too-small pic x(28)
                             value "Quantity must be >= 30 deg F".
       01 quantity-too-much  pic x(28)
                             value "Quantity must be <= 87 deg F".

      * Relative humidity input is used to calculate specific humidity
      * using the values in this table.
      * This is a specific humidity table of 100% relative humidity at
      * various temperatures ranging from 30F to 87F (58 data points).
      * The specific humidity values are based on the data available
      * from the TRANE Psychrometric Chart Form Number 1-43.190 Jan.1983
       01 specific-humidity-table.
           02 specifichumidity occurs 58 TIMES pic 999.

       procedure division.
       display-title-screen.
           display spaces
           display "***** ENTHALPY CALCULATOR UTILITY BEGINS *****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "June 16, 2020"
           display spaces
           display "in Enterprise COBOL v6.3 for z/OS"
           display spaces
           display "Initializing psychrometric chart" with no advancing.

       init-specific-humidity-table.
           display dot with no advancing
           move 24 to specifichumidity(1)
           display dot with no advancing
           move 25 to specifichumidity(2)
           display dot with no advancing
           move 26 to specifichumidity(3)
           display dot with no advancing
           move 27 to specifichumidity(4)
           display dot with no advancing
           move 28 to specifichumidity(5)
           display dot with no advancing
           move 30 to specifichumidity(6)
           display dot with no advancing
           move 31 to specifichumidity(7)
           display dot with no advancing
           move 32 to specifichumidity(8)
           display dot with no advancing
           move 33 to specifichumidity(9)
           display dot with no advancing
           move 35 to specifichumidity(10)
           display dot with no advancing
           move 36 to specifichumidity(11)
           display dot with no advancing
           move 38 to specifichumidity(12)
           display dot with no advancing
           move 39 to specifichumidity(13)
           display dot with no advancing
           move 41 to specifichumidity(14)
           display dot with no advancing
           move 42 to specifichumidity(15)
           display dot with no advancing
           move 44 to specifichumidity(16)
           display dot with no advancing
           move 46 to specifichumidity(17)
           display dot with no advancing
           move 48 to specifichumidity(18)
           display dot with no advancing
           move 50 to specifichumidity(19)
           display dot with no advancing
           move 52 to specifichumidity(20)
           display dot with no advancing
           move 53 to specifichumidity(21)
           display dot with no advancing
           move 56 to specifichumidity(22)
           display dot with no advancing
           move 58 to specifichumidity(23)
           display dot with no advancing
           move 60 to specifichumidity(24)
           display dot with no advancing
           move 62 to specifichumidity(25)
           display dot with no advancing
           move 64 to specifichumidity(26)
           display dot with no advancing
           move 67 to specifichumidity(27)
           display dot with no advancing
           move 70 to specifichumidity(28)
           display dot with no advancing
           move 72 to specifichumidity(29)
           display dot with no advancing
           move 74 to specifichumidity(30)
           display dot with no advancing
           move 77 to specifichumidity(31)
           display dot with no advancing
           move 80 to specifichumidity(32)
           display dot with no advancing
           move 83 to specifichumidity(33)
           display dot with no advancing
           move 86 to specifichumidity(34)
           display dot with no advancing
           move 89 to specifichumidity(35)
           display dot with no advancing
           move 92 to specifichumidity(36)
           display dot with no advancing
           move 96 to specifichumidity(37)
           display dot with no advancing
           move 99 to specifichumidity(38)
           display dot with no advancing
           move 103 to specifichumidity(39)
           display dot with no advancing
           move 107 to specifichumidity(40)
           display dot with no advancing
           move 110 to specifichumidity(41)
           display dot with no advancing
           move 114 to specifichumidity(42)
           display dot with no advancing
           move 118 to specifichumidity(43)
           display dot with no advancing
           move 123 to specifichumidity(44)
           display dot with no advancing
           move 127 to specifichumidity(45)
           display dot with no advancing
           move 132 to specifichumidity(46)
           display dot with no advancing
           move 136 to specifichumidity(47)
           display dot with no advancing
           move 141 to specifichumidity(48)
           display dot with no advancing
           move 146 to specifichumidity(49)
           display dot with no advancing
           move 151 to specifichumidity(50)
           display dot with no advancing
           move 156 to specifichumidity(51)
           display dot with no advancing
           move 161 to specifichumidity(52)
           display dot with no advancing
           move 167 to specifichumidity(53)
           display dot with no advancing
           move 172 to specifichumidity(54)
           display dot with no advancing
           move 178 to specifichumidity(55)
           display dot with no advancing
           move 185 to specifichumidity(56)
           display dot with no advancing
           move 191 to specifichumidity(57)
           display dot with no advancing
           move 197 to specifichumidity(58).

       temperature-data-entry.
           display spaces
           display "Enter dry bulb temperature (30 - 87 deg F): "
                    with no advancing
           accept temp-in

      * Did the user enter a valid numeric value?
           if function test-numval(temp-in) IS NOT EQUAL ZERO then
              display "Temperature " not-numeric
              go to temperature-data-entry
           else
              compute temperature = function numval(temp-in)
           end-if

           if temperature IS EQUAL ZERO then go to end-program

           if temperature > 87
              display quantity-too-much
              go to temperature-data-entry
           end-if

           if temperature < 30
              display quantity-too-small
              go to temperature-data-entry
           end-if
           move temperature to temp-out.

       relative-humidity-data-entry.
           display "Enter relative humidity as %: " with no advancing
           accept rh-in

      * Did the user enter a valid numeric value?
           if function test-numval(rh-in) IS NOT EQUAL ZERO then
              display "Relative humidity " not-numeric
              display spaces
              go to relative-humidity-data-entry
           else
              compute relative-humidity = function numval(rh-in)
           end-if

           if relative-humidity IS EQUAL ZERO then go to end-program

           if relative-humidity > 100
              display quantity-too-much
              display spaces
              go to relative-humidity-data-entry
           end-if

           if relative-humidity < 0
              display quantity-too-small
              display spaces
              go to relative-humidity-data-entry
           end-if
           move relative-humidity to rh-out.

       calculate-enthalpy.
           multiply temperature by 0.24 giving ha rounded
           subtract 29 from temperature giving table-idx
           divide 100 into relative-humidity
           multiply relative-humidity by specifichumidity(table-idx)
                    giving humidityratio rounded

      *     display "Temp: " temperature
      *     display "ha: " ha
      *     display spaces
      *     display "Enter humidity ratio (gr/LB) as:"
      *     display "nnn"
      *     accept humidityratio

           divide humidityratio by 7000 giving x rounded

      *     display "Converted to lb/lb: " x

           multiply temperature by 0.444 giving hw rounded

      *     display "hw: " hw

           add 1061 to hw

      *     display "hw: " hw

           multiply x by hw

      *     display "x: " x
      *     display "hw: " hw

           add ha to hw giving calc-enthalpy

      *     compute calc-enthalpy = (0.24 * temperature) + x *
      *                             (0.444 * temperature + 1061)

      *     display "Calc enthalpy: " calc-enthalpy

           move calc-enthalpy to enthalpy-out
           display spaces
           display "Enthalpy is " enthalpy-out " BTU/lb of dry air".

       end-program.
           display spaces
           display "***** ENTHALPY CALCULATOR UTILITY ENDS *****"
           display spaces
           stop run.

