//HEATINDX JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(HEATINDX),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(HEATINDX),DISP=SHR
//***************************************************/
