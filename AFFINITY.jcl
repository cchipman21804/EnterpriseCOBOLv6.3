//AFFINITY JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COMPLINK     EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(AFFINITY),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(AFFINITY),DISP=SHR
//***************************************************/
