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
//* *****************************************************************
//* These are log files for debugging purposes before this program
//* becomes totally interactive
//CHEATSHT DD  DSN=&SYSUID..OUTPUT(CHEATSHT),DISP=SHR
//CARDTABL DD  DSN=&SYSUID..OUTPUT(CARDTABL),DISP=SHR
//* USE SYSIN DATA SET TO ENTER ANY DATA INTO COBOL ACCEPT STATEMENTS
//*******************************************************************
//* Concentration requires as input a numeric difficulty level (1-11)
//* and two alphanumeric coordinates similar to grid coordinates on a
//* spreadsheet.
//* Entering an X as part of one of the alphanumeric coordinates causes
//* the program to exit without evaluating any cells in the table.
//*******************************************************************
//SYSIN    DD *
0
15
11
c1
a2
/*
// ELSE
// ENDIF
