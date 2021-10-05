       identification division.
       program-id.    fltrfreq.
       author.        Chipman.
      *
      ******************************************************************
      * This program creates a filtered list of records containing
      * the 15 character search string.  The filtered list is stored
      * in FILTERED.  The total number of records are displayed
      * in SYSOUT.
      ******************************************************************
      *
      *                    Job Control Language
      *
      *//FLTRFREQ JOB 1,NOTIFY=&SYSUID
      *//***************************************************/
      *//COBRUN  EXEC IGYWCL
      *//COBOL.SYSIN  DD DSN=&SYSUID..CBL(FLTRFREQ),DISP=SHR
      *//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(FLTRFREQ),DISP=SHR
      *//***************************************************/
      *// IF RC = 0 THEN
      *//***************************************************/
      *//RUN     EXEC PGM=FLTRFREQ
      *//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
      *//FREQLIST  DD DSN=&SYSUID..FREQ1993,DISP=SHR
      *//FILTERED  DD SYSOUT=*,OUTLIM=15000
      *//SYSOUT    DD SYSOUT=*,OUTLIM=15000
      *//CEEDUMP   DD DUMMY
      *//SYSUDUMP  DD DUMMY
      *//***************************************************/
      *// ELSE
      *// ENDIF
      *
      ******************************************************************
       environment division.
       input-output section.
       file-control.
            select freq-in-file         assign to FREQLIST
            organization is sequential.

            select freq-filtered-file   assign to FILTERED
            organization is sequential.

       data division.
       file section.
       fd freq-in-file recording mode f.

       01 freq-in                  pic x(80).

       fd freq-filtered-file recording mode f.

       01 freq-filtered            pic x(81).

       working-storage section.

      * Holds contents of the current datetime function
       01 datetime.
           02 year                 pic 9(4).
           02 mo                   pic 99.
           02 da                   pic 99.
           02 ho                   pic 99.
           02 mi                   pic 99.
           02 se                   pic 99.
           02 hs                   pic 99.
           02 plsormns             pic x.
           02 tzh                  pic 99.
           02 tzm                  pic 99.

      * Contents are extracted (MOVEd) from datetime for the
      * report header
       01 todays-date.
           02 todays-month         pic xx.
           02 filler               pic x value "/".
           02 todays-day           pic xx.
           02 filler               pic x value "/".
           02 todays-year          pic x(4).
      *
       01 report-time.
           02 hh                   pic 99.
           02 filler               pic x value ":".
           02 mm                   pic 99.
      *
      * Strings in table elements CANNOT contain variable names. The
      * contents of this data item will be MOVEd to the first element
      * of the table after receiving contents of todays-date &
      * report-time.
      *
       01 first-report-line.
           02 filler               pic x(28) value spaces.
           02 filler               pic x(24)
                             value "1993 Scanner Frequencies".
           02 filler               pic x(11) value spaces.
           02 tdys-date-out        pic x(10).
           02 filler               pic x(2) value " @".
           02 rpt-time-out         pic x(5).

       01 report-header-lines.
           02 rpt-hdr-ln1          pic x(80).

           02 rpt-hdr-ln2.
              03 filler            pic x(40)
              value "****************************************".
              03 filler            pic x(40)
              value "****************************************".

           02 rpt-hdr-ln3.
              03 filler            pic x(11) value spaces.
              03 filler            pic x(8) value "Licensee".
              03 filler            pic x(14) value spaces.
              03 filler            pic x(9) value "Frequency".
              03 filler            pic x(8) value spaces.
              03 filler            pic x(4) value "City".
              03 filler            pic x(5) value spaces.
              03 filler            pic x(3) value "SVC".
              03 filler            pic x(1) value spaces.
              03 filler            pic x(5) value "State".
              03 filler            pic x(1) value spaces.
              03 filler            pic x(9) value "CallSign".
              03 filler            pic x(2) value spaces.

       01 filler redefines report-header-lines.
           02 rpt-hdr-ln occurs 3 times
                                   pic x(80).

       01 ws-freq-in-record.
           02 ws-freq-in-licensee  pic x(31).
           02 ws-freq-in-frequency pic x(12).
           02 ws-freq-in-city      pic x(15).
           02 ws-freq-in-service   pic x(4).
           02 ws-freq-in-state     pic x(6).
           02 ws-freq-in-callsign  pic x(8).
      *
      * Keeps track of header lines & inspected records
       01 counter                  pic 9.
       01 total                    pic 9(4) value zero.

       01 search-strings.
           02 search-string-1      pic x(15) value "   salisbury   ".
      *     02 search-string-2      pic x(15) value "   seaford     ".

       01 inspected-record         pic x(15).

       01 ws-fltr-out-record.
           02 fltr-out-licensee    pic x(31).
           02 fltr-out-frequency   pic x(12).
           02 fltr-out-city        pic x(15).
           02 fltr-out-service     pic x(4).
           02 fltr-out-state       pic x(6).
           02 fltr-out-callsign    pic x(8).
           02 filler               pic x(4) value spaces.

       01 end-of-file-flag         pic x.
           88 EOF                  value "Y".

       procedure division.
       100-primary.
           perform 105-open-files
           perform 110-obtain-current-date
           perform 115-write-report-headers
                    varying counter from 2 by 1
                    until counter is equal to 4
           perform 120-extract-fields-from-record
                    until EOF
           perform 190-close-files.
      *
       9999-end-program.
           display total " records written containing "
           "'" search-string-1 "'"
           stop run.
      *
       105-open-files.
           open input freq-in-file
           open output freq-filtered-file.

       110-obtain-current-date.
      *
      * Obtain today's date for report header
      * Write first line of report
           move function current-date to datetime
           move mo to todays-month
           move da to todays-day
           move year to todays-year
           move ho to hh
           move mi to mm
           move todays-date to tdys-date-out
           move report-time to rpt-time-out
           move first-report-line to rpt-hdr-ln(1)
           move rpt-hdr-ln(1) to freq-filtered
           write freq-filtered.

       115-write-report-headers.
           move rpt-hdr-ln(counter) to freq-filtered
           write freq-filtered after advancing 1 line.

       120-extract-fields-from-record.
      *
      * Read file
           read freq-in-file
              at end move "Y" to end-of-file-flag
           end-read

           move freq-in to ws-freq-in-record
      *     display ws-freq-in-record
           perform 121-search-for-strings.

       121-search-for-strings.
      *
      *   b.Select only the records that have mention of the words
      *     stored in search-string-1 (ignoring case)
           move zero to counter
           move function lower-case(ws-freq-in-city) to
                                                     inspected-record
           inspect inspected-record tallying counter for all
                    search-string-1.

           if counter is greater than zero then
              perform 130-create-output-record thru 140-write-record
           end-if.

      *     move zero to counter
      *     inspect inspected-title tallying counter for all
      *              search-string-2

      *     if counter is greater than zero then
      *        perform 130-create-output-record thru 140-write-record
      *     end-if.

       130-create-output-record.
           move ws-freq-in-licensee   to fltr-out-licensee
           move ws-freq-in-frequency  to fltr-out-frequency
           move ws-freq-in-city       to fltr-out-city
           move ws-freq-in-service    to fltr-out-service
           move ws-freq-in-state      to fltr-out-state
           move ws-freq-in-callsign   to fltr-out-callsign.

       140-write-record.
           move ws-fltr-out-record to freq-filtered
           add 1 to total
           write freq-filtered after advancing 1 line.

       190-close-files.
           close freq-in-file
           close freq-filtered-file.

      *
      *
      *
      *       <!WHEW!>
      *
      *
      *
      *
      *
