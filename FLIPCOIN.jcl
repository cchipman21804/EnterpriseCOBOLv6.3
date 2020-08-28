//FLIPCOIN JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(FLIPCOIN),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(FLIPCOIN),DISP=SHR
//***************************************************/
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=FLIPCOIN
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
