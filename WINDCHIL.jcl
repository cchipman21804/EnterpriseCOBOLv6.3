//WINDCHIL JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(WINDCHIL),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(WINDCHIL),DISP=SHR
//***************************************************/
