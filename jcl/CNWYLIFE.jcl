//CNWYLIFE JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(CNWYLIFE),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(CNWYLIFE),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=CNWYLIFE
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//INITLIFE  DD DSN=&SYSUID..CONWAYIN,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
