//* COMPILE COBOL SOURCE CODE AND LINK OBJECT
//MIXEDAIR  JOB 1,NOTIFY=&SYSUID
//* COMPILE & LINK ONLY
// SET COBPGM='MIXEDAIR'
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(&COBPGM),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
//***************************************************/
