//INDREACT JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(INDREACT),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(INDREACT),DISP=SHR
//***************************************************/
