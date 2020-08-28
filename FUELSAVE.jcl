//FUELSAVE JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(FUELSAVE),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(FUELSAVE),DISP=SHR
//***************************************************/
