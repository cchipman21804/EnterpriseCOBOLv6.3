//ENTHALPY JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(ENTHALPY),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(ENTHALPY),DISP=SHR
//***************************************************/
