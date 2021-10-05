       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TOPACCTS.
       AUTHOR.        CHIPMAN.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-REC ASSIGN TO CUSTOMER
           ORGANIZATION IS SEQUENTIAL.

           SELECT PRT-LINE ASSIGN TO PRTLINE
           ORGANIZATION IS SEQUENTIAL.

      * TOPACCTJ
      * //TOPACCTS  JOB 1,NOTIFY=&SYSUID
      * //***************************************************/
      * //COBRUN  EXEC IGYWCL
      * //COBOL.SYSIN  DD DSN=&SYSUID..SOURCE(TOPACCTS),DISP=SHR
      * //LKED.SYSLMOD DD DSN=&SYSUID..LOAD(TOPACCTS),DISP=SHR
      * //***************************************************/
      * // IF RC = 0 THEN
      * //***************************************************/
      * //RUN     EXEC PGM=TOPACCTS
      * //STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
      * //CUSTOMER  DD DSN=MTM2020.PUBLIC.INPUT(CUSTRECS),DISP=SHR
      * //PRTLINE   DD DSN=&SYSUID..OUTPUT(TOPACCTS),DISP=SHR
      * //SYSOUT    DD SYSOUT=*,OUTLIM=15000
      * //CEEDUMP   DD DUMMY
      * //SYSUDUMP  DD DUMMY
      * //***************************************************/
      * // ELSE
      * // ENDIF
      
       DATA DIVISION.
       FILE SECTION.
       FD  CUST-REC RECORD CONTAINS 80 CHARACTERS RECORDING MODE F.
       01  CUSTOMER-REC.
           05  CUST-NAME-IN    PIC X(33).
           05  DATE-1          PIC X(8).
           05  FILLER          PIC X(3).
           05  DATE-2          PIC X(8).
           05  FILLER          PIC X(9).
           05  CUST-AMOUNT-IN  PIC X(12).

       FD  PRT-LINE RECORDING MODE F.
       01  PRT-REC             PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-PRT-REC.
           05  CUST-NAME-OUT   PIC X(33) VALUE SPACES.
           05  COLUMNS         PIC X(34) VALUE SPACES.
           05  AMOUNT-OUT      PIC $Z,ZZZ,ZZ9.99.

       01  CUST-AMOUNT-FILTER.
           05 MILLIONS         PIC X.
           05 FILLER           PIC X.
           05 THOUSANDS        PIC XXX.
           05 FILLER           PIC X.
           05 UNITS            PIC X(6).

       01  TOP-CUST-COUNT      PIC 9(4)  VALUE ZERO.

       01  NOCOMMAS.
           05 AMT-MILLIONS     PIC X.
           05 AMT-THOUSANDS    PIC XXX.
           05 AMT-UNITS        PIC X(6).

       01  AMOUNT              PIC 9(7)V99.
       01  TOP-AMOUNT          PIC 9(7)  VALUE 8500000.

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
           02 filler               pic x VALUE "/".
           02 todays-day           pic xx.
           02 filler               pic x VALUE "/".
           02 todays-year          pic x(4).
      *
       01 time-now.
           02 hour-now             pic xx.
           02 filler               pic x value ":".
           02 minute-now           pic xx.
           02 filler               pic x value ":".
           02 second-now           pic xx.
           02 filler               pic x value ".".
           02 h-sec-now            pic xx.
      *
       01 report-header-lines.
          02 rpt-hdr-ln1.
             03 filler         pic x(37)
                 VALUE "REPORT OF TOP ACCOUNT BALANCE HOLDERS".
             03 filler         pic x(43) VALUE SPACES.

          02 rpt-hdr-ln2.
             03 filler         pic x(13) VALUE "PREPARED FOR ".
             03 BOSS-NAME-OUT  pic x(25).
             03 filler         pic x(17) VALUE SPACES.
             03 date-out.
                04 month-out   pic x(2).
                04 filler      pic x.
                04 day-out     pic x(2).
                04 filler      pic x.
                04 year-out    pic x(4).
             03 filler         pic x(4)  VALUE " at ".
             03 time-out.
                04 hour-OUT    pic x(2).
                04 filler      pic x.
                04 minute-OUT  pic x(2).
                04 filler      pic x.
                04 second-OUT  pic x(2).
                04 filler      pic x.
                04 h-sec-OUT   pic x(2).
      *       03 filler         pic x(13) VALUE SPACES.

          02 rpt-hdr-ln3.
             04 filler         pic x(40)
                value "========================================".
             04 filler         pic x(40)
                value "========================================".

          02 rpt-hdr-ln4.
             03 filler         pic x(19) VALUE "NUMBER OF RECORDS: ".

             03 NUM-REC-OUT    pic x(4)  VALUE ZERO.
             03 filler         pic x(57) VALUE spaces.

          02 rpt-hdr-ln5.
             04 filler         pic x(40)
                value "========================================".
             04 filler         pic x(40)
                value "========================================".

       01 filler redefines report-header-lines.
          02 rpt-hdr-ln occurs 4 times
                               pic x(80).

       01 COUNTER                  PIC 9 VALUE ZERO.
       01 BOSS-NAME                PIC X(25)
                                         VALUE "Terah Chipman".

       01 EOF-FLAG                 PIC X VALUE 'N'.
          88  EOF                        VALUE 'Y'.

      *
      ****************************************************************
      *                  PROCEDURE DIVISION                          *
      ****************************************************************
      *
       PROCEDURE DIVISION.
      *
       100-PRIMARY.
           PERFORM 105-OPEN-FILES
           PERFORM 110-OBTAIN-CURRENT-DATE

           PERFORM 115-WRITE-REPORT-HEADERS
                   VARYING COUNTER FROM 1 BY 1
                   UNTIL COUNTER IS EQUAL TO 4

           PERFORM 120-CREATE-REPORT UNTIL EOF

           PERFORM 115-WRITE-REPORT-HEADERS
                   VARYING COUNTER FROM 3 BY 1
                   UNTIL COUNTER IS EQUAL TO 5

           PERFORM 130-CLOSE-FILES
           STOP RUN.

       105-OPEN-FILES.
           OPEN INPUT CUST-REC
           OPEN OUTPUT PRT-LINE.

       110-OBTAIN-CURRENT-DATE.
           MOVE FUNCTION CURRENT-DATE TO datetime
           move mo to todays-month
           move da to todays-day
           move year to todays-year
           move todays-date to date-out
           move ho to hour-now
           move mi to minute-now
           move se to second-now
           move hs to h-sec-now
           move time-now to time-out
           move boss-name to boss-name-out.

       115-WRITE-REPORT-HEADERS.
           move rpt-hdr-ln(counter) to WS-PRT-REC
           move ws-prt-rec to prt-rec
           write PRT-REC.

       120-CREATE-REPORT.
           READ CUST-REC
                AT END
                MOVE "Y" TO EOF-FLAG
                MOVE TOP-CUST-COUNT TO NUM-REC-OUT
           END-READ
           move CUST-AMOUNT-IN to CUST-AMOUNT-FILTER
           move millions to amt-millions
           move thousands to amt-thousands
           move units to amt-units
           compute amount = function numval(nocommas)
           if amount is greater than top-amount then
      *        display " | I FOUND ONE!!!"
              add 1 to top-cust-count
              move cust-name-in to cust-name-out
              move spaces to columns
              move amount to amount-out
              move ws-prt-rec to prt-rec
              write prt-rec
           end-if.

       130-CLOSE-FILES.
           CLOSE CUST-REC
           CLOSE PRT-LINE.
