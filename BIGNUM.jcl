//BIGNUM JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(BIGNUM),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(BIGNUM),DISP=SHR
//***************************************************/
