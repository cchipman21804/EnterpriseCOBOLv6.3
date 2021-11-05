       identification division.
       program-id.    commbldg.
       author.        Chipman.

      ******************************************************************
      *                                                                *
      *                Commercial Building Simulator                   *
      *                                                                *
      ******************************************************************
      *
      *                Job Control Language
      *
      *//COMMBLDG JOB 1,NOTIFY=&SYSUID
      *//***************************************************/
      *//COBRUN  EXEC IGYWCL
      *//COBOL.SYSIN  DD DSN=&SYSUID..CBL(COMMBLDG),DISP=SHR
      *//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(COMMBLDG),DISP=SHR
      *//***************************************************/
      *// IF RC = 0 THEN
      *//***************************************************/
      *//RUN     EXEC PGM=COMMBLDG
      *//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
      *//HRLYCOND  DD DSN=&SYSUID..KSBYTMY3,DISP=SHR
      *//HUMRATIO  DD DSN=&SYSUID..HUMRATIO,DISP=SHR
      *//SYSOUT    DD SYSOUT=*,OUTLIM=15000
      *//CEEDUMP   DD DUMMY
      *//SYSUDUMP  DD DUMMY
      *//***************************************************/
      *// ELSE
      *// ENDIF
      *
      ******************************************************************
      *
      * Add version number & date to output file beginning with v1.0
      * All versions prior to 2020/07/31 11:10:40 (CDT) are beta
      * versions (v0.xx)
      *
      ******************************************************************
      *
      *                        CHANGE LOG
      *
      *(v1.01) Added occupancy hours to report under building
      *        specifications - 07/31/2020
      *
      *(v1.02) Added humidity ratio file - 11/5/2021
      *
      ******************************************************************
      *
      *
      *                      UPCOMING IMPROVEMENTS
      *
      ******************************************************************
      *
      * Add support for full TMY3 (MTS2) & PSM3 datasets
      * https://nsrdb.nrel.gov/about/tmy.html
      *
      * NREL National Solar Radiation Database (NSRDB)
      * https://maps.nrel.gov/nsrdb-viewer
      *
      ******************************************************************
      *
      * Add support to activate HVAC economizer function when its
      * capacity exceeds the building's current cooling load
      *
      ******************************************************************
      *
      * Add support for OpenEI utility rates:
      * https://openei.org/wiki/Utility_Rate_Database
      *
      * Download all approved rates:
      * https://openei.org/apps/USURDB/download/usurdb.csv.gz
      *
      ******************************************************************
      *
      * Add support to calculate the hourly PV energy output in kWh of
      * an optimally tilted PV array
      * https://pvpmc.sandia.gov/modeling-steps/1-weather-design-inputs/
      *
      ******************************************************************
      *
      * Calculate the slope (m) and the constant (b) of the building's
      * cooling load linear equation (y = mx+b) based on the building's
      * balance point temperature and its outdoor design temperature?
      *
      ******************************************************************
      *
      * Calculate the slope (m) and the constant (b) of the chiller's
      * EER linear equation (y = mx+b)?
      *
      * EER slope formula: y = -0.107x +19.82
      *             where x = OSAT in degF
      *                   y = EER-calc
      *
      ******************************************************************
      *
      * Re-calculate the supply air temp based on the available cooling
      * capacity of the air cooling coil, the air flow, actual deltaT
      *
      * Re-calculate the chiller capacity based on chiller specification
      * limitations
      *
      ******************************************************************
      *
      * Calculate current hourly building cooling load based on the
      * number of fractional degree days (degree hour?) calculated from
      * the current TMY3 hour record?
      *
      *    see pages 400-402 of "Air Conditioning Principles and
      *    Systems" for details
      *
      *    DD = degree days
      *
      *    deltaT = difference in temperature between the average
      *             outdoor temperature & the hypothetical average
      *             indoor temperature
      *
      *    t = number of days the outdoor temperature is above the
      *        building's balance point (usually estimated as 65 degF,
      *        but varies based on an individual building's design
      *        parameters)
      *
      *    DD = deltaT * t
      *
      *    A = surface area (in sqft) of building components with
      *        opposite surfaces exposed to both conditioned space and
      *        ambient weather conditions
      *
      *    U = factor of thermal conductance of the building materials
      *        used on a particular surface
      *
      *    Qseason = U * A * DD * 24(hrs/day)
      *
      *    Q = design heating/cooling load (BTU/hr)
      *
      *    Qo = heat energy consumed during time period under
      *         consideration
      *
      *            Q
      *    Qo - -------- * DD * 24(hrs/day)
      *          deltaT
      *
      * It should be possible to granularize the calculation to an
      * hourly basis by removing the 24(hrs/day) factor from the
      * equation.
      *
      ******************************************************************

       environment division.
       input-output section.
       file-control.
           select hourly-condx-file assign to HRLYCOND
                    organization is sequential.
      *
      *     select tmy3-hourly-condx-file assign to TMY3
      *              organization is sequential.
      *
           select specific-humidity-file assign to HUMRATIO
                    organization is sequential.
      *
       data division.
       file section.
       fd  hourly-condx-file recording mode f.
      *
      * This is a VERY small subset of TMY3 dataset fields
      * Time (HH:MM),Dry-bulb (C),RHum (%)
       01  tmy3-record-in.
           02 tmy3-date-in                     pic x(10).
           02 filler                           pic x.
           02 tmy3-time-in                     pic x(5).
           02 filler                           pic x.
           02 tmy3-DBTemp-degC-in              pic x(5).
           02 filler                           pic x.
           02 tmy3-RHum-pct-in                 pic xxx.
           02 filler                           pic x(54).
      *
      *fd tmy3-hourly-condx-file recording mode v
      *    record varying from 90 to 623.
      *
      * This will accept a full TMY3 data record
      * 01  tmy3-record-in                     pic x(623).
      *
      * A specific humidity file containing the 100% relative humidity
      * moisture content of the air (in gr/LB) at various temperatures
      * between 30 degF and 87 degF:
       fd  specific-humidity-file recording mode f.
       01  psychrometric-chart.
           02 specific-humidity-record         pic x(80).

       working-storage section.

       01  software-version-release            pic x(8)
                                               value "(v01.02)".
       01  last-rec                            pic x.
           88 EOF                              value "Y".

       01  tmy3-record.
           02 tmy3-date.
              03 tmy3-month                    pic xx.
              03 filler                        pic x.
              03 tmy3-day                      pic xx.
              03 filler                        pic x.
              03 tmy3-year                     pic x(4).
           02 tmy3-time.
              03 tmy3-hour                     pic xx.
              03 filler                        pic x.
              03 tmy3-minute                   pic xx.
           02 tmy3-DBTemp-degC                 pic s99v9
                                               sign is leading separate.
           02 tmy3-RHum-pct                    pic 999.
      *
      ******************************************************************
      *
      * TMY3 dataset format:
      *
      * Record #1: Description of Record #2 field contentss
      * Record #2: Location, units of measure
      * Record #3: TMY3 record labels
      * Record #4+: TMY3 data starts here (either q 1 hr or q 1/2 hr)
      *
      *    02 fieldname4-fieldname2
      * fieldname4: description of data when read from TMY record 4+
      * fieldname2: description of data when read from TMY record 2
      *
      ******************************************************************
      *
      * 01 tmy3-record.
      *    02 year                    pic x(4).
      *    02 month                   pic x(2).
      *    02 day                     pic x(2).
      *    02 hour                    pic x(2).
      *    02 minute                  pic x(2).
      *
      * Extract decimal latitude and decimal longitude from these next
      * two fields while processing record #2
      *
      * The next three fields can be used to calculate the energy
      * production (in kWh) of an optimally tilted PV array.
      *
      * DHI: Diffuse Horizontal Irradiance is given in watts/sq meter
      *    02 DHI-latitude            pic x(7).
      *
      * DNI: Direct Normal Irradiance is given in watts/sq meter
      *    02 DNI-longitude           pic x(7).
      *
      * GHI: Global Horizontal Irradiance is given in watts/sq meter
      *    02 GHI-timezone            pic x(4).
      *
      * Extract location elevation given in meters from the next field
      * while processing record #2
      *    02 clearskyDHI-elevation   pic x(3).
      *    02 clearskyDNI-localtz     pic x(4).
      *    02 clearskyGHI             pic x(4).
      *
      * Cloud Type  0: Clear
      *             1: Probably clear
      *             2: Fog
      *             3: Water
      *             4: Super-cooled water
      *             5: Mixed
      *             6: Opaque ice
      *             7: Cirrus
      *             8: Overlapping
      *             9: Overshooting
      *            10: Unknown
      *            11: Dust
      *            12: Smoke
      *           -15: N/A
      *    02 cloud-type              pic x(3).
      *
      * specify pic s99v9 SIGN IS LEADING SEPARATE for any numeric
      * variable that may eventually receive the information in this
      * field.
      *    02 dew-point               pic x(5).
      *
      * The next field can be used to calculate the energy production
      * (in kWh) of an optimally tilted PV array
      *    02 solar-zenith-angle      pic x(6).
      *
      * Fill Flag:  0: N/A
      *             1: Missing image
      *             2: Low irradiance
      *             3: Exceeds Clearsky
      *             4: Missing cloud properties
      *             5: Rayleigh violation
      *    02 fillflag                pic x(3).
      *
      * The next field can be used to calculate the energy production
      * (in kWh) of an optimally tilted PV array
      *    02 surfacealbedo           pic x(4).
      *
      * windspeed is given in meters per second
      *    02 windspeed               pic x(4).
      *
      * precip is given in centimeters
      *    02 precip                  pic x(3).
      *
      * winddirection is given in degrees from true north
      *    02 winddirection           pic x(3).
      *
      *    02 tmy3-RHum-pct           pic x(3).
      *
      * specify pic s99v9 SIGN IS LEADING SEPARATE for any numeric
      * variable that may eventually receive the information in this
      * field.
      *    02 tmy3-DBTemp-degC        pic x(5).
      *    02 barometricpressure      pic x(4).
      *
      * specify pic 99v9999 for any numeric variable that may eventually
      * receive the information in this field.
      * Global Horizontal UV Urradiance (280-400nm)
      *    02 ghuvia                  pic x(7).
      *
      * specify pic 99v9999 for any numeric variable that may eventually
      * receive the information in this field.
      * Global Horizontal UV Urradiance (285-385nm)
      *    02 ghuvib                  pic x(7).
      *
      * outside air temp in degF, derived from tmy3 dry bulb
      * temperature (degC) using the formula
      *             compute DBTemp-degF = (9 / 5) *
      *                      function numval(tmy3-DBTemp-degC) + 32
       01  DBTemp-degF                         pic s999v9
                                               sign is leading separate.
      *
      * numeric value of outside air relative humidity, derived from
      *             compute outside-air-rh =
      *                      function numval(tmy3-RHum-pct)
      * decimal places smaller than units will not be needed
       01  outside-air-rh                      pic 999v99.
       01  outside-air-humidity-ratio          pic 999v99.

      ******************* SPECIFIC HUMIDITY TABLE **********************
      *
      * Relative humidity input is used to calculate specific humidity
      * in gr/LB using the values in this table.
      * This is a specific humidity table of 100% relative humidity at
      * various temperatures ranging from 30F to 87F (58 data points).
      * The specific humidity values are based on the data available
      * from the TRANE Psychrometric Chart Form Number 1-43.190 Jan.1983
      *
       01 ws-spec-hum-record.
           02 ws-deg-f                          pic xxx.
           02 filler                            pic x.
           02 ws-spec-hum                       pic x(6).
           02 filler                            pic x(70).
      *
      * The index for the specific-humidity-table is found by adding one
      * to the temperature in degF.
      * For example, the COBOL table index begins at 1, however, the
      * specific humidity returned by the table is for a temperature of
      * 0 degF.
       01 specific-humidity-table.
           02 specifichumidity occurs 121 TIMES pic 999.
      *
      ***************** EXISTING SYSTEM PARAMETERS *********************
      *                                                                *
      *                 Daily Occupancy time table:                    *
      *         * = occupied for that 15-minute period                 *
      *     00  01  02  03  04  05  06  07  08  09  10  11   (AM)      *
      *    |********                                    ****|          *
      *                                                                *
      *     12  13  14  15  16  17  18  19  20  21  22  23   (PM)      *
      *    |************************************************|          *
      *                                                                *
      *                Open from 11:00am to 2:00am                     *
      *                                                                *
       01 opening-hour                         pic 99 value 11.
       01 closing-hour                         pic 99 value 2.
      *
      * Maximum occupancy (# persons).
       01 max-occupancy                        pic 9(4) value 1200.

       01 current-occupancy                    pic 9(4).
      * Make occupancy random?                                         *
      * Currently, set current occupancy to max-occupancy at all times *
      * Later, make current occupancy realistically random.            *
      * Eventually, set current-occupancy from file.                   *

       01 occupancy-flag                       pic x.
           88 occupied                         value "Y".
           88 not-occupied                     value "N".

      * Building floor area (sqft).
       01 floor-area                           pic 9(5) value 8000.

      * Outside Air Ventilation volume flow rate for occupancy in
      * CFM/person.
       01 Vdot-per-person                      pic 99 value  5.

      * Outside Air ventilation volume flow rate for off-gassing in
      * CFM/sqft.
       01 Vdot-off-gas                         pic 9v99 value 0.06.

      * Maximum cooling load heat flow rate (in tons).
      * Assume it is 0 tons when the outside air temperature is below
      * 45 degrees fahrenheit and goes up linearly to the maximum value
      * when the outside air temperature equals the design temperature.
       01 cooling-load-calc                    pic 999v9.
       01 cooling-load-max                     pic 999 value 100.

      * Design temperature in degrees Fahrenheit (degF).
       01 outside-air-temp-design              pic 99 value 94.

      * Return air temperatures (thermostat setpoints in degF).
       01 Tra-occupied                         pic 99 value 75.
       01 Tra-unoccupied                       pic 99 value 85.

      * Worst case supply air temperature (degF).
       01 Tsa-design                           pic 99 value 55.

      * Bathroom exhaust fan ventilation air volume flow rate (in CFM).
       01 Vdot-eab                             pic 9(4) value 4000.

      *               Supply Air fan specifications

      * Static Pressure (in inches of water column).
       01 static-pressure                      pic 99 value 5.

      * Temperature gain across the fan (in degF).
       01 fan-deltaT                           pic 9v9 value 1.4.

      * Fan motor efficiency factor.
       01 fan-motor-eff                        pic 9v999 value 0.922.

      * Fan motor speed.
       01 fan-motor-speed                      pic 9(4) value 578.

      * Fan motor brake horsepower.
       01 fan-BHP                              pic 999v9 value 52.2.

      * Fan motor electrical power.
       01 fan-motor-KW                         pic 999v9.
      * compute fan-motor-KW = (fan-BHP * 0.746) / fan-motor-eff

      * Fan efficiency.
       01 fan-eff                              pic 9v99.
      * compute fan-eff =
      *          (static-pressure * Vdot-supply-air) /
      *          (fan-BHP * 6356)

      *             Chilled Water pump specifications

      * Pump Total Dynamic Head (in feet of water column).
       01 pump-TDH                             pic 99 value 83.

      * Pump motor efficiency factor.
       01 pump-motor-eff                       pic 9v999 value 0.9.

      * Pump motor speed.
       01 pump-motor-speed                     pic 9(4) value 1750.

      * Pump efficiency factor.
       01 pump-eff                             pic 9v999 value 0.78.

      * Impeller diameter.
       01 impeller-diameter                    pic 99v9 value 9.5.

      * Pump motor brake horsepower.
       01 pump-BHP                             pic 999v9.
      * compute pump-BHP = (Vdot-chws * pump-TDH) / (3960 * pump-eff)

      * Pump motor electrical power.
       01 pump-motor-KW                        pic 999v9.
      * compute pump-motor-KW = (pump-BHP * 0.746) / pump-motor-eff

      *                   Chiller specifications

      * Chilled water supply flow rate (GPM).
       01 Vdot-chws                            pic 999 value 284.

      * Chiller water supply temperature (degF).
       01 Tchws                                pic 99 value 45.

      * Chiller water return temperature (degF).
       01 Tchwr                                pic 99 value 55.

      * EER (max)
       01 EER-max                              pic 99 value 15.
       01 EER-calc                             pic 99v999.
      * EER slope formula: y = -0.107x +19.82
      *             where x = OSAT in degF
      *                   y = EER-calc
      * compute EER-calc = -0.107 * OSAT + 19.82

      *
      * Compressor motor efficiency.
      * Specification may be irrelevant to calculations because it may
      * be incorporated into EER specification.
      * 01 compressor-motor-eff                 pic 9v99 value 0.91.

      * Chiller power consumption (in KW)
       01 chiller-power-calc                   pic 999v9.

      * Nominal chiller power consumption formula: P = Q / EER
      * compute chiller-power-calc = (Qdotcoil / EER-calc) / 1000

      * Chiller motor BHP
       01 chiller-BHP                          pic 999v9.
      * compute chiller-BHP = chiller-power-calc * 0.746

      * Chiller capacity (in tons) at design temp
       01 chiller-capacity-nominal             pic 999 value 100.
       01 chiller-capacity-calc                pic 999v9.
      * compute chiller-capacity-calc =
      *         (EER-calc * chiller-power-calc * 1000) / 12000

      * Economizer capacity (in tons)
       01 econo-capacity-calc                  pic s999v9
                                               sign is leading separate.
       01 pounds-per-hour                      pic 9(5)v99.

      *               Energy Control Measure Costs
      *                                        in US dollars ($)
       01 economizer-cost                      pic 9(5) value 5000.
       01 fan-vfd-cost                         pic 9(5) value 17500.
       01 pump-vfd-cost                        pic 9(5) value 2625.

      * Average Energy Cost (in $/kWh):
       01 electric-rate                        pic 9v999 value 0.079.
      * Include parameters of typical commercial electric rate tariff.
       01 accum-kwh                            pic 9(7) value zero.

      ********************** DATA ENTRY FIELDS *************************

      * Occupied space environmental conditions:
      * 01 return-air-temp-in   pic x(4).
      * 01 return-air-rh-in   pic xx.

      * Use TMY3 weather data file instead of manual data entry for
      * outdoor environmental conditions.

      * Use an occupancy file instead of manual data entry to simulate
      * building occupancy during operating hours.

      * 01 outside-air-temp-in                  pic x(4).
      * 01 outside-air-rh-in                    pic xx.


      ********************** CALCULATED FIELDS *************************

      *                         Ventilation

      * Outside air flow (in CFM):
       01 Vdot-outside-air                     pic 9(5).
      * compute Vdot-outside-air =
      * Vdot-per-person * current-occupancy + Vdot-off-gas * floor-area

      * Percent Outside Air in Supply Air:
       01 pct-oa                               pic 999v999.
      * Outside Air % can be calculated using either air temperature or
      * carbon dioxide (CO2) concentration in ppm (parts per million).
      * First, check for equivalency of mixed air and outside air to
      * avoid a divide by zero error.

      * Exhaust air flow (in CFM):
       01 Vdot-exhaust-air                     pic 9(5).
      * subtract Vdot-eab from Vdot-outside-air giving Vdot-exhaust-air
      *                            OR
      * compute Vdot-exhaust-air = Vdot-outside-air - Vdot-eab

      * Supply air flow (in CFM):
       01 Vdot-supply-air                      pic 9(5).
      * compute Vdot-supply-air =
      *                     (cooling-load [-max] * 12000) /
      *                     (1.08 * (return-air-temp - Tsa-design))
      *                               ^^^^^^^^^^^^^
      *                        setpoint determined by occupancy boolean

      * Return air flow (in CFM):
       01 Vdot-return-air                      pic 9(5).
      * subtract Vdot-eab from Vdot-supply-air giving Vdot-return-air
      *                            OR
      * compute Vdot-return-air = Vdot-supply-air - Vdot-eab

      * Mixed return air [MRA] flow (in CFM):
       01 Vdot-mixed-return-air                pic 9(5).
      * subtract Vdot-exhaust-air from Vdot-return-air
      *                                  giving Vdot-mixed-return-air
      *                            OR
      * compute Vdot-mixed-return-air =
      *                            Vdot-return-air - Vdot-exhaust-air

      * Mixed air flow (in CFM):
       01 Vdot-mixed-air                       pic 9(5).
      * add Vdot-mixed-return-air to Vdot-outside-air
      *                                  giving Vdot-mixed-air
      *                            OR
      * compute Vdot-mixed-air =
      *                      Vdot-mixed-return-air + Vdot-outside-air

      *                Environmental conditions:
      *       Temperatures are in degrees Fahrenheit
      *       Relative humidities are in %
       01 mixed-air-temp                       pic 999v9.
      * compute mixed-air-temp =
      *    (Vdot-mixed-return-air * return-air-temp) +
      *    (Vdot-outside-air * outside-air-temp) / Vdot-supply-air

       01 mixed-air-rh                         pic 999v99.
      * compute mixed-air-rh =
      *    (Vdot-mixed-return-air * return-air-rh) +
      *    (Vdot-outside-air * outside-air-rh) / Vdot-supply-air

       01 mixed-air-humidity-ratio             pic 999v99.
       01 mixed-air-enthalpy                   pic 99v99.

       01 air-coil-deltaT                      pic s99v9
                                               sign is leading separate.
      * compute air-coil-deltaT =
      *                       mixed-air-temp - Tsa-design + fan-deltaT

       01 return-air-temp                      pic 999v9.
      *   ^^^^^^^^^^^^^^^
      * setpoint determined by occupancy boolean

       01 return-air-rh                        pic 999v99 value 50.
      * Return-air-rh is determined by the return-air-temp and the
      * specific humidity of the supply air, in addtion to any water
      * vapor from the occupants.  Any sensible and latent heat gain
      * from the occupants should have been accounted for when the
      * engineers calculated the building's cooling load.

       01 return-air-humidity-ratio            pic 999v99.
       01 return-air-enthalpy                  pic 99v99.

       01 supply-air-temp                      pic 999v9.
      * if OAST >= 45 then
      *    move 55.0 to supply-air-temp
      * else
      *    compute supply-air-temp = mixed-air-temp + fan-deltaT
      * end-if

       01 Qdotcoil-air                         pic s9(7)v9
                                               sign is leading separate.
       01 Qdotcoil-water                       pic 9(7)v9.

       01 numerator                            pic s9(7)v99
                                               sign is leading separate.
       01 denominator                          pic s9(7)v99
                                               sign is leading separate.
       01 table-idx                            pic 9(4).

       01 indoor-enthalpy-calc                 pic 99v99.
       01 outdoor-enthalpy-calc                pic 99v99.
       01 ha                                   pic 999v999.
       01 humidityratio                        pic 999.
       01 hw                                   pic 9(5)v99.
       01 relative-humidity                    pic 9v99.
       01 x                                    pic 99v9(9).

       01 datetime.
           02 yyyy                       pic 9(4).
           02 mo                         pic 99.
           02 dd                         pic 99.
           02 hh                         pic 99.
           02 mi                         pic 99.
           02 ss                         pic 99.
           02 hs                         pic 99.
           02 plsormns                   pic x.
           02 tzh                        pic 99.
           02 tzm                        pic 99.

      * Begin the simulation on January 1 of the current year
      * Initialize sim-year with yyyy from datetime
       01 sim-date.
           02 sim-year                   pic 9(4).
           02 sim-month                  pic 99   value 1.
           02 sim-day                    pic 99   value 1.

      * Move sim-date to sim-date-for-integer
       01 sim-date-for-integer           pic 9(8).
      *     02 sim-year                  pic 9(4).
      *     02 sim-month                 pic 99.
      *     02 sim-date                  pic 99.

      * Retrieve integer-of-date(sim-date-for-integer)
       01 integer-date                   pic 9(7).

      * Add one to integer-date when tmy3-hour recycles to "00"
      * move date-of-integer(integer-date) to sim-date-for-integer
      * move sim-date-for-integer to sim-date

      ************************ DISPLAYED FIELDS ************************
       01 section-headers.
           02 section-header-1.
              03 filler                        pic x(16)
                                  value "****************".
              03 filler                        pic xx value spaces.
              03 filler                        pic x(29)
                                  value "COMMERCIAL BUILDING SIMULATOR".
              03 filler                        pic x value space.
              03 svr-out                       pic x(8).
              03 filler                        pic x value space.
              03 filler                        pic x(6) value "BEGINS".
              03 filler                        pic xx value spaces.
              03 filler                        pic x(15)
                                               value "***************".
           02 section-header-2.
              03 filler                        pic x(28)
                                   value "----------------------------".
              03 filler                        pic x(4) value spaces.
              03 filler                        pic x(17)
                                   value "Hourly Simulation".
              03 filler                        pic x(4) value spaces.
              03 filler                        pic x(27)
                                   value "---------------------------".
           02 section-header-3.
              03 filler                        pic x(20)
                                   value "********************".
              03 filler                        pic xxx value spaces.
              03 filler                        pic x(34)
                          value "COMMERCIAL BUILDING SIMULATOR ENDS".
              03 filler                        pic xxx value spaces.
              03 filler                        pic x(20)
                                   value "********************".

       01 bldg-specs-header.
           02 bshd-1.
              03 filler                        pic x(28)
                                   value "----------------------------".
              03 filler                        pic x value space.
              03 filler                        pic x(23)
                                   value "Building Specifications".
              03 filler                        pic x value space.
              03 filler                        pic x(27)
                                   value "---------------------------".
           02 bshd-2.
              03 filler                        pic x(18)
                                   value "Maximum Occupancy:".
              03 filler                        pic x value space.
              03 max-occupancy-out             pic zzz,zz9.
              03 filler                        pic x value space.
              03 filler                        pic x(6) value "people".
              03 filler                        pic x(7) value spaces.
              03 filler                        pic x(15)
                                   value "Floor Area    :".
              03 filler                        pic x value space.
              03 floor-area-out                pic zzz,zz9.
              03 filler                        pic x value space.
              03 filler                        pic x(4) value "sqft".

           02 bshd-3.
              03 filler                        pic x(18)
                                   value "Max Cooling Load :".
              03 filler                        pic xxx value spaces.
              03 cooling-load-max-out          pic z,zz9.
              03 filler                        pic x value space.
              03 filler                        pic x(4) value "tons".
              03 filler                        pic x(9) value spaces.
              03 filler                        pic x(15)
                                   value "Occupied Hours:".
              03 filler                        pic x value space.
              03 opening-hour-out              pic 99.
              03 filler                        pic x(6) value "00 to ".
              03 closing-hour-out              pic 99.
              03 filler                        pic xx value "00".

       01 system-specs-header.
           02 sshd-1.
              03 filler                        pic x(28)
                                   value "----------------------------".
              03 filler                        pic xx value spaces.
              03 filler                        pic x(21)
                                   value "System Specifications".
              03 filler                        pic xx value spaces.
              03 filler                        pic x(27)
                                   value "---------------------------".

           02 sshd-2.
              03 filler                        pic x(11) value spaces.
              03 filler                        pic x(8)
                                      value "Air Side".
              03 filler                        pic x(37) value spaces.
              03 filler                        pic x(10)
                                      value "Water Side".

           02 sshd-3.
              03 filler                        pic x(17)
                                      value "Outside Air Temp:".
              03 filler                        pic x(9) value spaces.
              03 outside-air-temp-design-out   pic 99.
              03 filler                        pic x value space.
              03 filler                        pic x(4) value "degF".
              03 filler                        pic x(7) value spaces.
              03 filler                        pic x(31)
                                value "Chilled Water Supply Flow Rate:".
              03 filler                        pic x value space.
              03 Vdot-chws-out                 pic 999.
              03 filler                        pic x value space.
              03 filler                        pic xxx value "GPM".

           02 sshd-4.
              03 filler                        pic x(17)
                                      value "Supply Air Temp :".
              03 filler                        pic x(9) value spaces.
              03 Tsa-design-out                pic 99.
              03 filler                        pic x value space.
              03 filler                        pic x(4) value "degF".
              03 filler                        pic x(7) value spaces.
              03 filler                        pic x(31)
                                value "Chilled Water Supply Temp     :".
              03 filler                        pic xx value spaces.
              03 Tchws-out                     pic 99.
              03 filler                        pic x value space.
              03 filler                        pic x(4) value "degF".

           02 sshd-5.
              03 filler                        pic x(17)
                                      value "Return Air Temp :".
              03 filler                        pic x(9) value spaces.
              03 Tra-occupied-out              pic 99.
              03 filler                        pic x value space.
              03 filler                        pic x(4) value "degF".
              03 filler                        pic x(7) value spaces.
              03 filler                        pic x(31)
                                value "Chilled Water Return Temp     :".
              03 filler                        pic xx value spaces.
              03 Tchwr-out                     pic 99.
              03 filler                        pic x value space.
              03 filler                        pic x(4) value "degF".

           02 sshd-6.
              03 filler                        pic x(6) value spaces.
              03 filler                        pic x(18)
                                      value "Fan Specifications".
              03 filler                        pic x(28) value spaces.
              03 filler                        pic x(19)
                                      value "Pump Specifications".

           02 sshd-7.
              03 filler                        pic x(17)
                                      value "Static Pressure :".
              03 filler                        pic x(7) value spaces.
              03 static-pressure-out           pic z9.
              03 filler                        pic xxx value space.
              03 filler                        pic x(4) value "inWC".
              03 filler                        pic x(7) value spaces.
              03 filler                        pic x(21)
                                      value "Total Dynamic Head  :".
              03 filler                        pic x(10) value spaces.
              03 pump-TDH-out                  pic z9.
              03 filler                        pic xxx value space.
              03 filler                        pic x(4) value "ftWC".

           02 sshd-8.
              03 filler                        pic x(17)
                                      value "Fan Speed       :".
              03 filler                        pic x(4) value spaces.
              03 fan-motor-speed-out           pic z,zz9.
              03 filler                        pic xxx value spaces.
              03 filler                        pic x(4) value "RPM ".
              03 filler                        pic x(7) value spaces.
              03 filler                        pic x(21)
                                      value "Pump Speed          :".
              03 filler                        pic x(7) value spaces.
              03 pump-motor-speed-out          pic z,zz9.
              03 filler                        pic xxx value spaces.
              03 filler                        pic x(4) value "RPM ".

           02 sshd-9.
              03 filler                        pic x(17)
                                      value "Fan Brake HP    :".
              03 filler                        pic x(6) value spaces.
              03 fan-BHP-out                   pic zz9.9.
              03 filler                        pic x value space.
              03 filler                        pic xx value "HP".
              03 filler                        pic x(9) value spaces.
              03 filler                        pic x(21)
                                      value "Pump Brake HP       :".
              03 filler                        pic x(9) value spaces.
              03 pump-BHP-out                  pic zz9.9.
              03 filler                        pic x value space.
              03 filler                        pic xx value "HP".

           02 sshd-10.
              03 filler                        pic x(17)
                                      value "Fan Motor KW    :".
              03 filler                        pic x(6) value spaces.
              03 fan-motor-KW-out              pic zz9.9.
              03 filler                        pic x value space.
              03 filler                        pic xx value "KW".
              03 filler                        pic x(9) value spaces.
              03 filler                        pic x(21)
                                      value "Pump Motor KW       :".
              03 filler                        pic x(9) value spaces.
              03 pump-motor-KW-out             pic zz9.9.
              03 filler                        pic x value space.
              03 filler                        pic xx value "KW".

           02 sshd-11.
              03 filler                        pic x(17)
                                      value "Fan deltaT      :".
              03 filler                        pic x(8) value spaces.
              03 fan-deltaT-out                pic 9.9.
              03 filler                        pic x value space.
              03 filler                        pic x(4) value "degF".
              03 filler                        pic x(7) value spaces.
              03 filler                        pic x(21)
                                      value "Impeller Diameter   :".
              03 filler                        pic x(10) value spaces.
              03 impeller-diameter-out         pic z9.9.
              03 filler                        pic x value space.
              03 filler                        pic xx value "in".

           02 sshd-12.
              03 filler                        pic x(17)
                                      value "Fan Motor eff   :".
              03 filler                        pic x(9) value spaces.
              03 fan-motor-eff-out             pic z9.
              03 filler                        pic x value "%".
              03 filler                        pic x(11) value spaces.
              03 filler                        pic x(21)
                                      value "Pump Motor eff      :".
              03 filler                        pic x(12) value spaces.
              03 pump-motor-eff-out            pic z9.
              03 filler                        pic x value "%".

           02 sshd-13.
              03 filler                        pic x(17)
                                      value "Fan efficiency  :".
              03 filler                        pic x(9) value spaces.
              03 fan-eff-out                   pic z9.
              03 filler                        pic x value "%".
              03 filler                        pic x(11) value spaces.
              03 filler                        pic x(21)
                                      value "Pump efficiency     :".
              03 filler                        pic x(12) value spaces.
              03 pump-eff-out                  pic z9.
              03 filler                        pic x value "%".

           02 sshd-14.
              03 filler                        pic x(5) value spaces.
              03 filler                        pic x(22)
                                         value "Ventilation Parameters".
              03 filler                        pic x(23) value spaces.
              03 filler                        pic x(22)
                                         value "Chiller Specifications".

           02 sshd-15.
              03 filler                        pic x(21)
                                         value "Bathroom Exhaust Air:".
              03 filler                        pic x value space.
              03 Vdot-eab-out                  pic zz,zz9.
              03 filler                        pic x value space.
              03 filler                        pic xxx value "CFM".
              03 filler                        pic x(8) value spaces.
              03 filler                        pic x(21)
                                         value "Nominal Capacity    :".
              03 filler                        pic x(9) value spaces.
              03 chiller-capacity-nominal-out  pic z,zz9.
              03 filler                        pic x value space.
              03 filler                        pic x(4) value "tons".

           02 sshd-16.
              03 filler                        pic x(21)
                                         value "Supply Air          :".
              03 filler                        pic x value space.
              03 Vdot-supply-air-out           pic zz,zz9.
              03 filler                        pic x value space.
              03 filler                        pic xxx value "CFM".
              03 filler                        pic x(8) value spaces.
              03 filler                        pic x(21)
                                         value "Nominal eff  (EER)  :".
              03 filler                        pic x(12) value spaces.
              03 EER-max-out                   pic z9.

           02 sshd-17.
              03 filler                        pic x(21)
                                         value "Return Air          :".
              03 filler                        pic x value space.
              03 Vdot-return-air-out           pic zz,zz9.
              03 filler                        pic x value space.
              03 filler                        pic xxx value "CFM".
              03 filler                        pic x(8) value spaces.
              03 filler                        pic x(21)
                                         value "Compressor Motor KW :".
              03 filler                        pic x(9) value spaces.
              03 comp-motor-power-nom-out      pic zz9.9.
              03 filler                        pic x value space.
              03 filler                        pic xx value "KW".

           02 sshd-18.
              03 filler                        pic x(21)
                                         value "Exhaust Air         :".
              03 filler                        pic x value space.
              03 Vdot-exhaust-air-out          pic zz,zz9.
              03 filler                        pic x value space.
              03 filler                        pic xxx value "CFM".
              03 filler                        pic x(8) value spaces.
              03 filler                        pic x(21)
                                         value "Compressor Brake HP :".
              03 filler                        pic x(9) value spaces.
              03 chiller-BHP-out               pic zz9.9.
              03 filler                        pic x value space.
              03 filler                        pic xx value "HP".

           02 sshd-19.
              03 filler                        pic x(21)
                                         value "Mixed-Return Air    :".
              03 filler                        pic x value space.
              03 Vdot-mixed-return-air-out     pic zz,zz9.
              03 filler                        pic x value space.
              03 filler                        pic xxx value "CFM".

           02 sshd-20.
              03 filler                        pic x(21)
                                         value "Mixed Air           :".
              03 filler                        pic x value space.
              03 Vdot-mixed-air-out            pic zz,zz9.
              03 filler                        pic x value space.
              03 filler                        pic xxx value "CFM".

           02 sshd-21.
              03 filler                        pic x(21)
                                         value "Outside Air         :".
              03 filler                        pic x value space.
              03 Vdot-outside-air-out          pic zz,zz9.
              03 filler                        pic x value space.
              03 filler                        pic xxx value "CFM".
              03 filler                        pic x value space.
      *
      * Cannot calculate %OA from temperature parameters at this point
      * because mixed air temp has not been calculated yet in the
      * application.  However, it can be calculated from the ratio of
      * VdotOA / VdotSA.  VdotOA is a function of building floor area
      * and current occupancy.
              03 filler                        pic x value "(".
              03 pct-oa-out                    pic z9.9.
              03 filler                        pic x(5) value "% OA)".

      ******************************************************************
      *                    HOURLY REPORT HEADERS                       *
       01  rpthd-1.
           02 filler                           pic x(7) value spaces.
           02 filler                           pic x(4) value "TMY3".
           02 filler                           pic x(7) value spaces.
           02 filler                           pic x(11)
                                                    value "OUTSIDE AIR".
           02 filler                           pic x value space.
           02 filler                           pic xxx value "OCC".
           02 filler                           pic x value space.
           02 filler                           pic x(7)
                                                     value "RTN AIR".
           02 filler                           pic x value space.
           02 filler                           pic x(9)
                                                     value "MIXED AIR".
           02 filler                           pic x value spaces.
           02 filler                           pic x(8)
                                                     value "SPLY AIR".
           02 filler                           pic x(4) value space.
           02 filler                           pic x(4)
                                                     value "COIL".
           02 filler                           pic x(4) value spaces.
           02 filler                           pic x(5)
                                                     value "ECONO".
           02 filler                           pic xx value spaces.
           02 filler                           pic x(7)
                                                     value "CHILLER".
           02 filler                           pic x value space.
           02 filler                           pic x(7)
                                                     value "COOLING".
           02 filler                           pic xx value spaces.
           02 filler                           pic x(7)
                                                     value "CHILLER".
           02 filler                           pic x value space.
           02 filler                           pic x(11)
                                                    value "ACCUMULATED".

       01  rpthd-2.
      *     02 filler                           pic x(7)
      *                                               value "INTDATE".
           02 filler                           pic xx value spaces.
           02 filler                           pic x(4)
                                                     value "DATE".
           02 filler                           pic x(6) value spaces.
           02 filler                           pic x(4)
                                                     value "TIME".
           02 filler                           pic xxxx value space.
           02 filler                           pic x(4)
                                                     value "TEMP".
           02 filler                           pic xxx value spaces.
           02 filler                           pic x(2)
                                                     value "RH".
           02 filler                           pic x value space.
           02 filler                           pic xxx value "FLG".
           02 filler                           pic x(4) value spaces.
           02 filler                           pic x(4)
                                                     value "TEMP".
           02 filler                           pic x(6) value spaces.
           02 filler                           pic x(4)
                                                     value "TEMP".
           02 filler                           pic x(5) value spaces.
           02 filler                           pic x(4) value "TEMP".
           02 filler                           pic x(1) value spaces.
           02 filler                           pic x(7) value "DELTA-T".
           02 filler                           pic x value space.
           02 filler                           pic x(8)
                                                     value "CAPACITY".
           02 filler                           pic x value space.
           02 filler                           pic x(8)
                                                     value "CAPACITY".
           02 filler                           pic x(4) value spaces.
           02 filler                           pic x(4)
                                                     value "LOAD".
           02 filler                           pic x(4) value spaces.
           02 filler                           pic x(5)
                                                     value "POWER".
           02 filler                           pic x(9) value spaces.
           02 filler                           pic xxx value "KWH".

      * Debugging placeholder to avoid compilation errors
       01  placeholder.
           02 integer-date-out                 pic 9(7).

       01  energy-record-out.
      *     02 integer-date-out                 pic 9(7).
      *     02 filler                           pic x value space.
           02 date-out.
              03 month-out                     pic 99.
              03 filler                        pic x value "/".
              03 day-out                       pic 99.
              03 filler                        pic x value "/".
              03 year-out                      pic 9(4).
           02 filler                           pic x value space.
           02 time-out.
              03 hour-out                      pic 99.
              03 filler                        pic x value ":".
              03 minute-out                    pic 99.
           02 filler                           pic x value space.
           02 DBTemp-degF-out                  pic +zz9.9.
           02 filler                           pic x value "F".
           02 filler                           pic x value space.
           02 RHum-pct-out                     pic zz9.
           02 filler                           pic x value "%".
           02 filler                           pic xx value spaces.
           02 occupancy-flag-out               pic x.
           02 filler                           pic x(3) value spaces.
           02 return-air-temp-out              pic zz9.9.
           02 filler                           pic x value "F".
           02 filler                           pic x(4) value spaces.
           02 mixed-air-temp-out               pic zz9.9.
           02 filler                           pic x value "F".
           02 filler                           pic x(3) value space.
           02 supply-air-temp-out              pic zz9.9.
           02 filler                           pic x value "F".
           02 filler                           pic x(2) value spaces.
           02 air-coil-deltaT-out              pic +z9.9.
           02 filler                           pic x value "F".
           02 filler                           pic x(2) value spaces.
           02 econo-capacity-out               pic +zz9.9.
           02 filler                           pic x value "T".
           02 filler                           pic xxx value spaces.
           02 chiller-capacity-out             pic zz9.9.
           02 filler                           pic x value "T".
           02 filler                           pic x(2) value spaces.
           02 cooling-load-out                 pic zz9.9.
           02 filler                           pic x value "T".
           02 filler                           pic x(2) value spaces.
           02 chiller-power-out                pic zz9.9.
           02 filler                           pic xx value "KW".
           02 filler                           pic x value space.
           02 accum-kwh-out                    pic z,zzz,zz9.9.

      * 01 mixed-air-temp-out                   pic zzz9.
      * 01 mixed-air-rh-out                     pic z9.
      * 01 return-air-temp-out                  pic zzz9.
      * 01 return-air-rh-out                    pic z9.
      * 01 outside-air-temp-out                 pic zzz9.
      * 01 outside-air-rh-out                   pic z9.
      * 01 pct-oa-out                           pic zz9.9.

      * Constant-values.
      * 01 min-val         pic s9(4) value -40.
      * 01 max-val         pic s9(4) value 9999.

      * Constant-text.
       01 dot                                  pic x value ".".
       01 not-numeric                          pic x(16)
                             value " is NOT numeric.".
       01 quantity-too-small                   pic x(20)
                             value "Value must be >= -40".
       01 quantity-too-much                    pic x(21)
                             value "Value must be <= 9999".

       procedure division.
       100-main-paragraph.
           open input specific-humidity-file
           move 1 to table-idx
           perform 120-init-spec-hum-table
      *      varying table-idx from 1 by 1 until EOF
      * In the (hopefully) near future, the specific humidity table will
      * be loaded from a more accurate file, instead of using this
      * guess-timated hard-coded version.
      *
           close specific-humidity-file
           perform 121-calc-design-parameters
           perform 125-calc-ventilation
           perform 110-display-specifications
           perform 130-initialize-date
           perform 140-open-files
           perform 150-calculate-it until EOF.
      *     go to 9999-end-program.

       9999-end-program.
           display spaces
           display section-header-3
      *     display spaces
      *     display "******* COMMERCIAL BUILDING SIMULATOR ENDS *******"
      *     display spaces
           close hourly-condx-file
           stop run.

       110-display-specifications.
      *     display spaces
      *     display "****** COMMERCIAL BUILDING SIMULATOR BEGINS ******"
      *     display "Written by, Clifford A. Chipman, EMIT"
      *     display "June 21, 2020"
      *     display spaces
      *     display "in Enterprise COBOL v6.3 for z/OS"
      *     display spaces
      *     display "Initializing psychrometric chart" with no advancing
      *
           move software-version-release to svr-out
           move opening-hour to opening-hour-out
           move closing-hour to closing-hour-out
      *
           display section-header-1
           display spaces
           display bshd-1
           display bshd-2
           display bshd-3
           display spaces
           display sshd-1
           display sshd-2
           display sshd-3
           display sshd-4
           display sshd-5
           display spaces
           display sshd-6
           display sshd-7
           display sshd-8
           display sshd-9
           display sshd-10
           display sshd-11
           display sshd-12
           display sshd-13
           display spaces
           display sshd-14
           display sshd-15
           display sshd-16
           display sshd-17
           display sshd-18
           display sshd-19
           display sshd-20
           display sshd-21
           display spaces
           display section-header-2
           display rpthd-1
           display rpthd-2.

       120-init-spec-hum-table.
           read specific-humidity-file into ws-spec-hum-record
              at end move "Y" to last-rec
           end-read
           if function test-numval(specific-humidity-record(1:1))
              is equal to zero then
      *        move specific-humidity-record to ws-spec-hum-record
              unstring specific-humidity-record
                 delimited by ","
                 into  ws-deg-f
                       ws-spec-hum
              end-unstring
              move ws-spec-hum to specifichumidity(table-idx)
              add 1 to table-idx
           end-if.

       121-calc-design-parameters.
      *
      * Move numeric design calculation constants and results to
      * report display variables
      *
      * Building Specifications
           move max-occupancy to max-occupancy-out
           move floor-area to floor-area-out
           move cooling-load-max to cooling-load-max-out
      *
      * System Specifications
           move outside-air-temp-design to outside-air-temp-design-out
           move Tsa-design to Tsa-design-out
           move Tra-occupied to Tra-occupied-out
           move Vdot-chws to Vdot-chws-out
           move Tchws to Tchws-out
           move Tchwr to Tchwr-out

           move static-pressure to static-pressure-out
           move pump-TDH to pump-TDH-out
           move fan-motor-speed to fan-motor-speed-out
           move pump-motor-speed to pump-motor-speed-out
           move fan-BHP to fan-BHP-out
           compute pump-BHP rounded =
                    (Vdot-chws * pump-TDH) / (3960 * pump-eff)
           move pump-BHP to pump-BHP-out
           compute fan-motor-KW rounded =
                    (fan-BHP * 0.746) / fan-motor-eff
           move fan-motor-KW to fan-motor-KW-out
           compute pump-motor-KW rounded =
                    (pump-BHP * 0.746) / pump-motor-eff
           move pump-motor-KW to pump-motor-KW-out
           move fan-deltaT to fan-deltaT-out
           move impeller-diameter to impeller-diameter-out
           multiply fan-motor-eff by 100 giving fan-motor-eff-out
           multiply pump-motor-eff by 100 giving pump-motor-eff-out
      *
      * Supply air flow (in CFM):
           compute Vdot-supply-air rounded =
                   (cooling-load-max * 12000) /
                   (1.08 * (Tra-occupied - Tsa-design))
      *                     ^^^^^^^^^^^^
      *                   Design setpoint
           move Vdot-supply-air to Vdot-supply-air-out
           compute fan-eff rounded =
                   (static-pressure * Vdot-supply-air) /
                   (fan-BHP * 6356)
           multiply fan-eff by 100 giving fan-eff-out
           multiply pump-eff by 100 giving pump-eff-out
           move chiller-capacity-nominal to
                 chiller-capacity-nominal-out
           move EER-max to EER-max-out
           compute EER-calc rounded = -0.107 * outside-air-temp-design
                                      + 19.821
      *
      * The usual formula multiplies chiller capacity (in tons) by
      * 12000 to convert to BTU/hr.  Dividing by EER gives power in
      * watts.  Dividing that figure by 1000 gives power in KW
      * (kilowatts).
      * However, multiplying capacity in tons by 12 automatically gives
      * power in kilowatts by removing the need to divide the answer by
      * 1000.
           compute Qdotcoil-water rounded =
                    500 * Vdot-chws * (Tchwr - Tchws)
           divide 1000 into Qdotcoil-water
           compute chiller-power-calc rounded =
                    Qdotcoil-water / EER-calc
           move chiller-power-calc to comp-motor-power-nom-out

      *     compute chiller-power-calc rounded =
      *              (chiller-capacity-nominal * 12) / EER-calc
      *     move chiller-power-calc to chiller-power-out

           compute chiller-BHP rounded =
                    chiller-power-calc * 0.746
           move chiller-BHP to chiller-BHP-out.

       125-calc-ventilation.
      * Calculate ventilation
           move Vdot-eab to Vdot-eab-out
      *
      * Outside air flow (in CFM):
           move max-occupancy to current-occupancy
           compute Vdot-outside-air rounded =
                    Vdot-per-person * current-occupancy +
                    Vdot-off-gas * floor-area
           move Vdot-outside-air to Vdot-outside-air-out
      *
      * Calculate Outside Air %:
           divide Vdot-outside-air by Vdot-supply-air giving
                    pct-oa rounded
           multiply 100 by pct-oa
           move pct-oa to pct-oa-out
      *
      * Exhaust air flow (in CFM):
           subtract Vdot-eab from Vdot-outside-air giving
                    Vdot-exhaust-air
      *                            OR
      *    compute Vdot-exhaust-air = Vdot-outside-air - Vdot-eab
           move Vdot-exhaust-air to Vdot-exhaust-air-out

      * Return air flow (in CFM):
           subtract Vdot-eab from Vdot-supply-air giving
                    Vdot-return-air
      *                            OR
      *    compute Vdot-return-air = Vdot-supply-air - Vdot-eab
           move Vdot-return-air to Vdot-return-air-out

      * Mixed return air [MRA] flow (in CFM):
           subtract Vdot-exhaust-air from Vdot-return-air giving
                    Vdot-mixed-return-air
      *                            OR
      *    compute Vdot-mixed-return-air =
      *            Vdot-return-air - Vdot-exhaust-air
           move Vdot-mixed-return-air to Vdot-mixed-return-air-out

      * Mixed air flow (in CFM):
           add Vdot-mixed-return-air to Vdot-outside-air giving
               Vdot-mixed-air
      *                            OR
      *    compute Vdot-mixed-air =
      *            Vdot-mixed-return-air + Vdot-outside-air
           move Vdot-mixed-air to Vdot-mixed-air-out.

       130-initialize-date.
      * Begin calculated date on January 1 of current year
      *
      * Obtain current year
           move function current-date to datetime
      *
      * Initialize sim-year with yyyy from date-time
           move yyyy to sim-year
      * 01 sim-date.
      *     02 sim-year                   pic 9(4).
      *     02 sim-month                  pic 99   value 1.
      *     02 sim-day                    pic 99   value 1.
      *
      * Move sim-date to sim-date-for-integer
           move sim-date to sim-date-for-integer
      * 01 sim-date-for-integer           pic 9(8).

      * Retrieve integer-of-date(sim-date-for-integer)
           compute integer-date =
                 function integer-of-date(sim-date-for-integer).
      * 01 integer-date                   pic 9(7).

      * Calculate date for initial integer-date
           compute sim-date-for-integer =
                 function date-of-integer(integer-date)

      * move sim-date-for-integer to sim-date
           move sim-date-for-integer to sim-date.

      * Add one to integer-date when tmy3-hour recycles to "00"
      * move date-of-integer(integer-date) to sim-date-for-integer
      * move sim-date-for-integer to sim-date

       140-open-files.
           open input hourly-condx-file.

       150-calculate-it.
           read hourly-condx-file
                 at end move "Y" to last-rec
           end-read
           perform 200-mv-tmy3-to-ws
           perform 300-degC-to-degF
           perform 400-calc-date
           perform 500-calc-baseline
           perform 800-create-output-record
           perform 900-display-output-record.

       200-mv-tmy3-to-ws.
      *
      * move alphanumeric TMY3 file data to working-storage
           move tmy3-date-in to tmy3-date
           move tmy3-time-in to tmy3-time
           compute tmy3-DBTemp-degC =
                 function numval(tmy3-DBTemp-degC-in)
           compute tmy3-RHum-pct =
                 function numval(tmy3-RHum-pct-in).

       300-degC-to-degF.
      *
      * convert TMY3 outside air temp from degC to degF
           compute DBTemp-degF = (9 / 5) * tmy3-DBTemp-degC + 32
      *
      * calculate enthalpy of outside air
           multiply DBTemp-degF by 0.24 giving ha rounded
           subtract 29 from DBTemp-degF giving table-idx
           divide tmy3-RHum-pct by 100 giving relative-humidity
           multiply relative-humidity by specifichumidity(table-idx)
                    giving humidityratio rounded
           divide humidityratio by 7000 giving x rounded
           multiply DBTemp-degF by 0.444 giving hw rounded
           add 1061 to hw
           multiply x by hw
           add ha to hw giving outdoor-enthalpy-calc.
      *     move outdoor-enthalpy-calc to enthalpy-out.
      *
      * calculate economizer capacity at %OA setting
      * 1> convert CFM (Vdot-outside-air) to lb/hr
      *    lb per hr = CFM / 4.5
      * 2> calculate enthalpy of indoor air
      *
      *    (need to assign initial value of indoor relative humidity)
      *    (and track it hourly through the simulation)
      *
      * 3> compute econo-capacity-calc =
      *             (CFM / 4.5) * (IndoorEnthalpy - OutdoorEnthalpy)
      * divide by 12000 giving tons

      * *************** THIS FORMULA IS WRONG ***************
      *     compute econo-capacity-calc =
      *          (outdoor-enthalpy-calc * Vdot-outside-air * 60) / 12000
      *
      * Set display output to zero until formula is corrected
      *    move ZERO to econo-capacity-out.

       400-calc-date.
      * move date-of-integer(integer-date) to sim-date-for-integer
           compute sim-date-for-integer =
                 function date-of-integer(integer-date)
      *
      * move sim-date-for-integer to sim-date
           move sim-date-for-integer to sim-date.

       500-calc-baseline.
      *
      * Determine occupancy from operating hours & adjust
      * return-air-temp setpoint
      *                                                                *
      *                 Daily Occupancy time table:                    *
      *         * = occupied for that 15-minute period                 *
      *     00  01  02  03  04  05  06  07  08  09  10  11   (AM)      *
      *    |********                                    ****|          *
      *                                                                *
      *     12  13  14  15  16  17  18  19  20  21  22  23   (PM)      *
      *    |************************************************|          *
      *                                                                *
      *                Open from 11:00am to 2:00am                     *
      *                                                                *
      * tmy3-time is a string -- convert to numeric first
           compute hour-out =
                 function numval(tmy3-hour)
           compute minute-out =
                 function numval(tmy3-minute)
      *
      * Is the building open or closed? Occupied or not occupied?
           if hour-out IS LESS THAN opening-hour AND
              hour-out IS GREATER THAN OR EQUAL TO closing-hour then
              move "N" to occupancy-flag
           else
              move "Y" to occupancy-flag
           end-if
           move occupancy-flag to occupancy-flag-out
      *
      * Set the thermostat based on occupancy flag
           if occupied then
              move Tra-occupied to return-air-temp
           else
              move Tra-unoccupied to return-air-temp
           end-if
           move return-air-temp to return-air-temp-out
      *
      * Calculate mixed-air-temp
      *     compute mixed-air-temp rounded =
      *              Vdot-mixed-return-air * return-air-temp +
      *              Vdot-outside-air * DBTemp-degF / Vdot-supply-air
      * COMPUTE statement gives erroneous results, but individual
      * calculation steps give the correct result
           multiply Vdot-mixed-return-air by return-air-temp
                    giving numerator
           multiply Vdot-outside-air by DBTemp-degF
                    giving denominator
           add denominator to numerator
           divide numerator by Vdot-supply-air
                    giving mixed-air-temp rounded
           move mixed-air-temp to mixed-air-temp-out
      *
      * Calculate mixed air enthalpy here
      * 1> Calculate return air enthalpy using
      *    return-air-temp and return-air-relative-humidity
           multiply return-air-temp by 0.24 giving ha rounded
           subtract 29 from return-air-temp giving table-idx
           divide return-air-rh by 100 giving relative-humidity
           multiply relative-humidity by specifichumidity(table-idx)
                    giving humidityratio rounded
           move humidityratio to return-air-humidity-ratio
           divide return-air-humidity-ratio by 7000 giving x rounded
           multiply return-air-temp by 0.444 giving hw rounded
           add 1061 to hw
           multiply x by hw
           add ha to hw giving return-air-enthalpy
      *
      * 2> Outside air enthalpy is already stored in
      *    outdoor-enthalpy-calc
           subtract 29 from DBTemp-degF giving table-idx
           divide 100 into tmy3-RHum-pct giving relative-humidity
           multiply relative-humidity by specifichumidity(table-idx)
                    giving humidityratio rounded
           move humidityratio to outside-air-humidity-ratio
      *
      * 3> Apply enthalpy (h) balance mixing formula:
      *
      *    hc
      *     v
      *     - < hb = (Qa * ha + Qc * hc) / (Qa + Qc)
      *     ^                                 (Qb)
      *    ha
      *          where: Q is the flow rate
      *                 h is the enthalpy
      *
      * ---------------------------------------------------------------
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
      * Calculate mixed-air-enthalpy
           compute mixed-air-enthalpy rounded =
           (Vdot-mixed-return-air * return-air-enthalpy +
            Vdot-outside-air * outdoor-enthalpy-calc) /
            (Vdot-mixed-return-air + Vdot-outside-air)
      *
      * Compare mixed-air-enthalpy to return-air-enthalpy
      *    convert CFM to lb/hr
           compute pounds-per-hour rounded = Vdot-return-air / 4.5
      *    Heat flow rate in BTU/hr (Q) =
           compute numerator rounded = pounds-per-hour *
                    (return-air-enthalpy - mixed-air-enthalpy)
           divide numerator by 12000 giving econo-capacity-calc rounded
           move econo-capacity-calc to econo-capacity-out
      *
      * Calculate supply-air-temp
           if DBTemp-degF >= 45 then
              move 55.0 to supply-air-temp
           else
              compute supply-air-temp rounded =
                       mixed-air-temp + fan-deltaT
           end-if
           move supply-air-temp to supply-air-temp-out
      *
      * Calculate air-coil-deltaT
           compute air-coil-deltaT rounded =
                    mixed-air-temp - supply-air-temp
           move air-coil-deltaT to air-coil-deltaT-out
      *
      * Calculate cooling load
           if DBTemp-degF < 45 then
              move zero to cooling-load-calc
           else
              compute cooling-load-calc rounded =
                       2.0408 * DBTemp-degF - 91.837
           end-if
           move cooling-load-calc to cooling-load-out
      *
      * Calculate chiller capacity
           compute EER-calc rounded = -0.107 * DBTemp-degF + 19.821
           compute Qdotcoil-air rounded =
                    1.08 * Vdot-supply-air * air-coil-deltaT
           divide 12000 into Qdotcoil-air rounded
           if Qdotcoil-air IS LESS THAN OR EQUAL TO ZERO then
              move zero to chiller-capacity-out
           else
              move Qdotcoil-air to chiller-capacity-out
           end-if
      *
      * Calculate total chiller power consumption
      * Qdotcoil-water already divided by 1000 for kilowatt calculation
      * in nnn-calc-design-parameters.
           if occupied then
               divide Qdotcoil-water by EER-calc
                       giving chiller-power-calc rounded
               add pump-motor-KW to chiller-power-calc
               add fan-motor-KW to chiller-power-calc
               add chiller-power-calc to accum-kwh rounded
           else
              move zero to chiller-power-calc
           end-if
           move chiller-power-calc to chiller-power-out
           move accum-kwh to accum-kwh-out.

       800-create-output-record.
      *
      * move data to output record for display
      *
           move integer-date to integer-date-out

           move sim-month to month-out
           move sim-day to day-out
           move sim-year to year-out

      *
      * Add one to integer-date when tmy3-hour recycles to "00"
           if hour-out is equal to zero THEN
              add 1 to integer-date
           end-if

           move DBTemp-degF to DBTemp-degF-out

           move tmy3-RHum-pct to RHum-pct-out.

       900-display-output-record.
           display energy-record-out.

      * Read this from a file in the future: (v1.02)
      * 120-init-spec-hum-table.
      *     display dot with no advancing
      *     move 24 to specifichumidity(1)
      *     display dot with no advancing
      *     move 25 to specifichumidity(2)
      *     display dot with no advancing
      *     move 26 to specifichumidity(3)
      *     display dot with no advancing
      *     move 27 to specifichumidity(4)
      *     display dot with no advancing
      *     move 28 to specifichumidity(5)
      *     display dot with no advancing
      *     move 30 to specifichumidity(6)
      *     display dot with no advancing
      *     move 31 to specifichumidity(7)
      *     display dot with no advancing
      *    move 32 to specifichumidity(8)
      *     display dot with no advancing
      *    move 33 to specifichumidity(9)
      *     display dot with no advancing
      *    move 35 to specifichumidity(10)
      *     display dot with no advancing
      *    move 36 to specifichumidity(11)
      *     display dot with no advancing
      *    move 38 to specifichumidity(12)
      *     display dot with no advancing
      *    move 39 to specifichumidity(13)
      *     display dot with no advancing
      *    move 41 to specifichumidity(14)
      *     display dot with no advancing
      *    move 42 to specifichumidity(15)
      *     display dot with no advancing
      *    move 44 to specifichumidity(16)
      *     display dot with no advancing
      *    move 46 to specifichumidity(17)
      *     display dot with no advancing
      *    move 48 to specifichumidity(18)
      *     display dot with no advancing
      *    move 50 to specifichumidity(19)
      *     display dot with no advancing
      *    move 52 to specifichumidity(20)
      *     display dot with no advancing
      *    move 53 to specifichumidity(21)
      *     display dot with no advancing
      *    move 56 to specifichumidity(22)
      *     display dot with no advancing
      *    move 58 to specifichumidity(23)
      *     display dot with no advancing
      *    move 60 to specifichumidity(24)
      *     display dot with no advancing
      *    move 62 to specifichumidity(25)
      *     display dot with no advancing
      *    move 64 to specifichumidity(26)
      *     display dot with no advancing
      *    move 67 to specifichumidity(27)
      *     display dot with no advancing
      *    move 70 to specifichumidity(28)
      *     display dot with no advancing
      *    move 72 to specifichumidity(29)
      *     display dot with no advancing
      *    move 74 to specifichumidity(30)
      *     display dot with no advancing
      *    move 77 to specifichumidity(31)
      *     display dot with no advancing
      *    move 80 to specifichumidity(32)
      *     display dot with no advancing
      *    move 83 to specifichumidity(33)
      *     display dot with no advancing
      *    move 86 to specifichumidity(34)
      *     display dot with no advancing
      *    move 89 to specifichumidity(35)
      *     display dot with no advancing
      *    move 92 to specifichumidity(36)
      *     display dot with no advancing
      *    move 96 to specifichumidity(37)
      *     display dot with no advancing
      *    move 99 to specifichumidity(38)
      *     display dot with no advancing
      *    move 103 to specifichumidity(39)
      *     display dot with no advancing
      *    move 107 to specifichumidity(40)
      *     display dot with no advancing
      *    move 110 to specifichumidity(41)
      *     display dot with no advancing
      *    move 114 to specifichumidity(42)
      *     display dot with no advancing
      *    move 118 to specifichumidity(43)
      *     display dot with no advancing
      *    move 123 to specifichumidity(44)
      *     display dot with no advancing
      *    move 127 to specifichumidity(45)
      *     display dot with no advancing
      *    move 132 to specifichumidity(46)
      *     display dot with no advancing
      *    move 136 to specifichumidity(47)
      *     display dot with no advancing
      *    move 141 to specifichumidity(48)
      *     display dot with no advancing
      *    move 146 to specifichumidity(49)
      *     display dot with no advancing
      *    move 151 to specifichumidity(50)
      *     display dot with no advancing
      *    move 156 to specifichumidity(51)
      *     display dot with no advancing
      *    move 161 to specifichumidity(52)
      *     display dot with no advancing
      *    move 167 to specifichumidity(53)
      *     display dot with no advancing
      *    move 172 to specifichumidity(54)
      *     display dot with no advancing
      *    move 178 to specifichumidity(55)
      *     display dot with no advancing
      *    move 185 to specifichumidity(56)
      *     display dot with no advancing
      *    move 191 to specifichumidity(57)
      *     display dot with no advancing
      *    move 197 to specifichumidity(58).
      *
      ******************************************************************
      *                                                                *
      *            TYPICAL COMMERCIAL/INDUSTRIAL AIR HANDLER           *
      *                                                                *
      *                                     Tra sensor                 *
      *                                      |             _____||____ *
      *                                      |            |    Bath   |*
      *         _____________________________v____________|   exhaust |*
      *                                                               |*
      *         /<< Exhaust Air           << *  Return Air            |*
      *         _______________   v   ____________________            |*
      *                        | MRA |                    |           |*
      * Outside                |  v  |                    |   Condx   |*
      *   Air                  |  v  |                    |   space   |*
      *         _______________|  v  |____________________|           |*
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
      *by,Cliff Chipman,EMIT                                Jun 20,2020*
      ******************************************************************
      *                                                                *
      * "To determine the percentage of outside air introduced to a    *
      *  forced air delivery system."                                  *
      *                                                                *
      *                         Tma - Tra                              *
      *                  %OA = ----------- * 100                       *
      *                         Toa - Tra                              *
      *                                                                *
      * where:                                                         *
      *    Toa = temperature or ppm CO2 of outside air                 *
      *    Tra = temperature or ppm CO2 of return air                  *
      *    Tma = temperature or ppm CO2 of mixed air                   *
      *                                                                *
      *    --- page 96:   HANDBOOK OF FORMULAE, EQUATIONS & CONVERSION *
      *                   FACTORS FOR THE ENERGY PROFESSIONAL          *
      *                   by, Bryan Kerwin, CEM                        *
      *                                                                *
      *                   www.littleredbook.com                        *
      *                                                                *
      *    Copyright 1994                                              *
      *                                                                *
      *    JOB Publications                                            *
      *    PO Box 20121                                                *
      *    Tallahassee, FL 32316-0121                                  *
      *                                                                *
      ******************************************************************
      *                                                                *
      * NOTE: Temperature/CO2 concentration inputs must mathematically *
      *       match the air mixing process equation!                   *
      *                                                                *
      *       (m3 x DB3) = (m1 x DB1) + (m2 x DB2)                     *
      *                                                                *
      * where: m  = mass flow rate in CFM                              *
      *        DB = dry bulb temperature in degF                       *
      *                                                                *
      * NOTE: This also applies to the humidity ratio.                 *
      *                                                                *
      *       (m3 x W3) = (m1 x W1) + (m2 x W2)                        *
      *                                                                *
      *    --- page 182:  AIR CONDITIONING PRINCIPLES AND SYSTEMS      *
      *                   AN ENERGY APPROACH - 4th edition             *
      *                   by, Edward G. Pita                           *
      *                                                                *
      *    Copyright 2002                                              *
      *                                                                *
      *    Pearson Education, Inc                                      *
      *    Upper Saddle River, NJ 07458                                *
      *                                                                *
      ******************************************************************
      *                                                                *
      * Minimum Mechanical Ventilation Requirements:                   *
      *                                                                *
      * Either 15 CFM per person times the expected occupancy rate     *
      * Or the applicable ventilation rate from Table 6.17             *
      *                                                                *
      *    --- page 147:  AIR CONDITIONING PRINCIPLES AND SYSTEMS      *
      *                   AN ENERGY APPROACH - 4th edition             *
      *                   by, Edward G. Pita                           *
      *                                                                *
      *    Copyright 2002                                              *
      *                                                                *
      *    Pearson Education, Inc                                      *
      *    Upper Saddle River, NJ 07458                                *
      *                                                                *
      *    Changing the building occupancy changes outside air         *
      *    ventilation requirements. This changes the mixed air        *
      *    temperature at the cooling coil, which changes the          *
      *    available cooling capacity of the coil.                     *
      *                                                                *
      ******************************************************************

