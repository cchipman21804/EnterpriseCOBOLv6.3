//FFGP JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(FFGP),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(FFGP),DISP=SHR
//***************************************************/
