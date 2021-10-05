       identification division.
       program-id.  windchil.
       author.      Chipman.

       data division.
       working-storage section.

      * Data-entry-fields.
       01 outside-temp-in                      pic x(6).
       01 wind-speed-in                        pic x(5).

      * Calculation fields.
       01 outside-temp                         pic s999v9
                                               sign is leading separate.
       01 wind-speed                           pic 999v9.
       01 wind-chill                           pic s999v99
                                               sign is leading separate.

      * Formulas:
      * Twc = 35.74 + 0.6215*Ta - 35.75*v^0.16 + 0.4275*Ta*v^0.16
      *
      * where:   Twc = wind chill index, degF
      *           Ta = air temperature, degF
      *            v = wind speed, mph
      *
       01 constant-factors.
           02 cf1                              pic 99v99
                                               value 35.74.

           02 cf2                              pic 9v9(4)
                                               value 0.6215.

           02 cf3                              pic 99v99
                                               value 35.75.

           02 cf4                              pic 9v9(4)
                                               value 0.4275.

      * Constant-values.
       01 min-temp                             pic s999  value -150
                                               sign is leading separate.
       01 max-temp                             pic 99   value 50.
       01 min-wind                             pic 9    value 3.
       01 max-wind                             pic 999  value 250.

      * Constant-text.
       01 not-numeric                          pic x(15)
                                      value "is NOT numeric.".

       01 too-small                            pic x(11)
                                      value "must be >= ".

       01 too-much                             pic x(11)
                                      value "must be <= ".

      * Display fields.
       01 outside-temp-out                     pic +ZZ9.9 usage display.
       01 wind-speed-out                       pic ZZ9.9.
       01 wind-chill-out                       pic +ZZ9.9 usage display.

       procedure division.
       main-para.
           perform display-title-screen
           perform outside-temp-data-entry
           perform wind-speed-data-entry
           perform calculate-it
           perform display-results
           go to end-program.

       display-title-screen.
           display spaces
           display "********** WIND CHILL CALCULATOR BEGINS **********"
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

       wind-speed-data-entry.
           display "Enter wind speed in MPH " with no advancing
           display "(nnn.n): " with no advancing
           accept wind-speed-in

      * Did the user enter a valid numeric value?
           if function test-numval(wind-speed-in)
           IS NOT EQUAL ZERO then
              display "Wind speed " not-numeric
              display spaces
              go to wind-speed-data-entry
           else
              compute wind-speed =
                       function numval(wind-speed-in)
           end-if

           if wind-speed IS EQUAL ZERO then
              go to end-program
           end-if

           if wind-speed > max-wind
              display "Wind speed " too-much max-wind " MPH."
              display spaces
              go to wind-speed-data-entry
           end-if

           if wind-speed < min-wind
              display "Wind speed " too-small min-wind " MPH."
              display spaces
              go to wind-speed-data-entry
           end-if

           move wind-speed to wind-speed-out.

       calculate-it.
      * Twc = 35.74 + 0.6215*Ta - 35.75*v^0.16 + 0.4275*Ta*v^0.16
      *
      * where:   Twc = wind chill index, degF
      *           Ta = air temperature, degF
      *            v = wind speed, mph
      *
           compute wind-chill rounded =
                               cf1 +
                               cf2 * outside-temp -
                               cf3 * wind-speed ** 0.16 +
                               cf4 * outside-temp * wind-speed ** 0.16

           move wind-chill to wind-chill-out.

       display-results.
           display spaces
      *     display outside-temp
           display "Outdoor Temperature: " outside-temp-out " degF"
      *     display wind-speed
           display "Wind Speed: " wind-speed-out " MPH"
      *     display wind-chill
           display "Wind Chill: " wind-chill-out " degF".

       end-program.
           display spaces
           display "*********** WIND CHILL CALCULATOR ENDS ***********"
           display spaces
           stop run.
      *
      * Wind Chill
      * -- https://en.wikipedia.org/wiki/Wind_chill
      *
      * Many formulas exist for wind chill because, unlike temperature,
      * wind chill has no universally agreed upon standard definition or
      * measurement. All the formulas attempt to qualitatively predict
      * the effect of wind on the temperature humans perceive. Weather
      * services in different countries use standards unique to their
      * country or region; for example, the U.S. and Canadian weather
      * services use a model accepted by the National Weather Service.
      * That model has evolved over time.
      * The first wind chill formulas and tables were developed by Paul
      * Allman Siple and Charles F. Passel working in the Antarctic
      * before the Second World War, and were made available by the
      * National Weather Service by the 1970s. They were based on the
      * cooling rate of a small plastic bottle as its contents turned to
      * ice while suspended in the wind on the expedition hut roof, at
      * the same level as the anemometer. The so-called Windchill Index
      * provided a pretty good indication of the severity of the
      * weather.
      * Charles Eagan[2] realized that people are rarely still and that
      * even when it was calm, there was some air movement. He redefined
      * the absence of wind to be an air speed of 1.8 metres per second
      * (6.5 km/h; 4.0 mph), which was about as low a wind speed as a
      * cup anemometer could measure. This led to more realistic
      * (warmer-sounding) values of equivalent temperature.
      *
      * Original model --
      * Equivalent temperature was not universally used in North America
      * until the 21st century. Until the 1970s, the coldest parts of
      * Canada reported the original Wind Chill Index, a three or four
      * digit number with units of kilocalories/hour per square metre.
      * Each individual calibrated the scale of numbers personally,
      * through experience. The chart also provided general guidance to
      * comfort and hazard through threshold values of the index, such
      * as 1400, which was the threshold for frostbite.
      *
      * The original formula for the index was:[3][4]
      * WCI = (10 * SQR(v)-v+10.5)*(33-Ta)
      *
      * where:   WCI = wind chill index, kcal/m^2/hr
      *            v = wind velocity, m/sec
      *           Ta = air temperature, degC
      *
      * North American and United Kingdom wind chill index --
      * In November 2001, Canada, the United States, and the United
      * Kingdom implemented a new wind chill index developed by
      * scientists and medical experts on the Joint Action Group for
      * Temperature Indices (JAG/TI).[5][6][7] It is determined by
      * iterating a model of skin temperature under various wind speeds
      * and temperatures using standard engineering correlations of wind
      * speed and heat transfer rate. Heat transfer was calculated for a
      * bare face in wind, facing the wind, while walking into it at 1.4
      * metres per second (5.0 km/h; 3.1 mph). The model corrects the
      * officially measured wind speed to the wind speed at face height,
      * assuming the person is in an open field.[8] The results of this
      * model may be approximated, to within one degree, from the
      * following formula:

      * The standard wind chill formula for Environment Canada is:
      * Twc = 13.12 + 0.6215*Ta - 11.37*v^0.16 + 0.3965*Ta*v^0.16
      *
      * where:   Twc = wind chill index, degC
      *           Ta = air temperature, degC
      *            v = wind speed at 10m (33ft), km/hr[9]
      *
      * The equivalent formula in US customary units is:[10]
      * Twc = 35.74 + 0.6215*Ta - 35.75*v^0.16 + 0.4275*Ta*v^0.16
      *
      * where:   Twc = wind chill index, degF
      *           Ta = air temperature, degF
      *            v = wind speed, mph
      *
      * References:
      * 2: Eagan, C. (1964). Review of research on military problems in
      * cold regions. C. Kolb and F. Holstrom eds. TDR-64-28. Arctic
      * Aeromed. Lab. p 147156.
      *
      * 3: *Woodson, Wesley E. (1981). Human Factors Design Handbook,
      * page 815. McGraw-Hill. ISBN 0-07-071765-6
      *
      * 4: https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov
      *    /19690003109_1969003109.pdf, equation 55, page 6-113
      *
      * 5: "Environment Canada - Weather and Meteorology - Canada's
      *    Wind Chill Index". Ec.gc.ca. Retrieved 2013-08-09.
      *    http://www.ec.gc.ca
      *    /meteo-weather/default.asp?lang=n&n=5FBF816A-1#wc6
      *
      * 6: "Meteorological Tables, Wind Chill. August, 2001 Press
      *    Release:". National Weather Service.
      *    Retrieved 14 January 2013.
      *    http://www.erh.noaa.gov/er/iln/tables.htm
      *
      * 7: "Wind Chill". BBC Weather, Understanding weather. BBC.
      *    Archived from the original on 11 October 2010.
      *    https://web.archive.org
      *    /web/20101011123948/http://www.bbc.co.uk/weather/features
      *    /understanding/wind_chill.shtml
      *
      * 8: Osczevski, Randall; Bluestein, Maurice (2005). "The new wind
      *    chill equivalent temperature chart". Bulletin of the American
      *    Meteorological Society. 86 (10): 14531458.
      *    Bibcode:2005BAMS...86.1453O. doi:10.1175/BAMS-86-10-1453.
      *    https://ui.adsabs.harvard.edu
      *    /abs/2005BAMS...86.1453O/abstract
      *
      * 9: "Calculation of the 1971 to 2000 Climate Normals for Canada".
      *    Climate.weatheroffice.gc.ca. 2013-07-10. Archived from the
      *    original on 2013-06-27. Retrieved 2013-08-09.
      *    https://web.archive.org
      *    /web/20130627223738/http://climate.weatheroffice.gc.ca
      *    /prods_servs/normals_documentation_e.html
      *
      *10: "NWS Wind Chill Index". Weather.gov. 2009-12-17. Archived
      *    from the original on 2011-09-18. Retrieved 2013-08-09.
      *    https://web.archive.org
      *    /web/20110918010232/http://www.weather.gov
      *    /os/windchill/index.shtml
