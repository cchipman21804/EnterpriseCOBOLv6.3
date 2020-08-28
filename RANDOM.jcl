//RANDOM JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(RANDOM),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(RANDOM),DISP=SHR
//***************************************************/
