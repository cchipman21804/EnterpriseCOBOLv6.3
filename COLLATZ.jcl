//* COMPILE COBOL SOURCE CODE AND LINK OBJECT
//COLLATZ  JOB 1,NOTIFY=&SYSUID
//* COMPILE & LINK ONLY
// SET COBPGM='COLLATZ'
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(&COBPGM),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
//***************************************************/
