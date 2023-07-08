//Z01768EX  JOB 1,NOTIFY=&SYSUID
// SET COBPGM='CNCNTRAT'
//*****************************************************/
//* COMPILE, LINK, EXECUTE COBOL SOURCE WITH NO FILES */
//*****************************************************/
//* COMPILE IT
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..ZOSGRADS.PGMCHAL.CBL(&COBPGM),
//             DISP=SHR
//*****************************************************/
//* LINK IT
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
//*****************************************************/
// IF RC = 0 THEN
//* EXECUTE IT IF NO COMPILATION ERRORS
//RUN     EXEC PGM=&COBPGM
//STEPLIB  DD  DSN=&SYSUID..LOAD,DISP=SHR
//SYSOUT   DD  SYSOUT=*,OUTLIM=15000
//CHEATSHT DD  DSN=&SYSUID..OUTPUT(CHEATSHT),DISP=SHR
//CARDTABL DD  DSN=&SYSUID..OUTPUT(CARDTABL),DISP=SHR
//* USE SYSIN DATA SET TO ENTER ANY DATA INTO COBOL ACCEPT STATEMENTS
//*SYSIN   DD *
//*(›.<+|&!$*;^-/º,%_>?`:#@'="\)[ABCDEFGHIÅJKLMNOPQR~STUVWXYZ]{0123456789}
//*
// ELSE
// ENDIF
