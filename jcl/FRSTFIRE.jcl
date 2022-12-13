//Z81011FF  JOB 1,NOTIFY=&SYSUID
// SET COBPGM='FRSTFIRE'
//* COMPILE, LINK, EXECUTE COBOL SOURCE WITH NO FILES/
//***************************************************/
//* COMPILE IT
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(&COBPGM),DISP=SHR
//***************************************************/
//* LINK IT
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//* EXECUTE IT
//RUN     EXEC PGM=&COBPGM
//STEPLIB DD   DSN=&SYSUID..LOAD,DISP=SHR
//SYSOUT  DD   SYSOUT=*,OUTLIM=15000
//*SYSIN   DD *
//*0010
//*
// ELSE
// ENDIF
