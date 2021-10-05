//SAVINGS  JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(SAVINGS),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(SAVINGS),DISP=SHR
//***************************************************/
