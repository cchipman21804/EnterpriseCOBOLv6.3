//FLTRFREQ JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(FLTRFREQ),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(FLTRFREQ),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=FLTRFREQ
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//FREQLIST  DD DSN=&SYSUID..FREQ1993,DISP=SHR
//FILTERED  DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
