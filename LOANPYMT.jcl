//LOANPYMT JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(LOANPYMT),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(LOANPYMT),DISP=SHR
//***************************************************/
