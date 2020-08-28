//HACKRANK JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(HACKRANK),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(HACKRANK),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=HACKRANK
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//HACKNEWS  DD DSN=ZOS.PUBLIC.HACKER.NEWS,DISP=SHR
//FILTERED  DD DSN=&SYSUID..FRNTPAGE,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
