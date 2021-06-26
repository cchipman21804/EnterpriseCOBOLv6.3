      *//* COMPILE COBOL SOURCE CODE AND LINK OBJECT
      *//PULLYDIS JOB 1,NOTIFY=&SYSUID
      *//* COMPILE & LINK ONLY
      *// SET COBPGM='PULLYDIS'
      *//***************************************************/
      *//COBRUN  EXEC IGYWCL
      *//COBOL.SYSIN  DD DSN=&SYSUID..CBL(&COBPGM),DISP=SHR
      *//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
      *//***************************************************/
