//OHMSLAW JOB 1,NOTIFY=&SYSUID                        
//***************************************************/
//COBRUN  EXEC IGYWCL                                 
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(OHMSLAW),DISP=SHR  
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(OHMSLAW),DISP=SHR 
//***************************************************/
