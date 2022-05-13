//COMPLINK  JOB 1,NOTIFY=&SYSUID
//* COMPILE COBOL SOURCE CODE AND LINK OBJECT
//* COMPILE & LINK ONLY
// SET COBPGM='LUHN'
//***************************************************/
//COMLNK  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(&COBPGM),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
//***************************************************/
