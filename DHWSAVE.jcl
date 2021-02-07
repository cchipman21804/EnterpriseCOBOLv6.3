//DHWSAVE JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(DHWSAVE),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(DHWSAVE),DISP=SHR
//***************************************************/
