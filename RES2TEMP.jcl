//RES2TEMP JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(RES2TEMP),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(RES2TEMP),DISP=SHR
//***************************************************/
