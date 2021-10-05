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
