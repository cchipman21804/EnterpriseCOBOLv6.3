//COMMBLDG JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(COMMBLDG),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(COMMBLDG),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=COMMBLDG
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//HRLYCOND  DD DSN=&SYSUID..KSBYTMY3,DISP=SHR
//HUMRATIO  DD DSN=&SYSUID..HUMRATIO,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
