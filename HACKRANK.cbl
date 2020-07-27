       identification division.
       program-id.    hackrank.
       author.        Chipman.
      *
      ******************************************************************
      *
      * Update COBOL Programming Course #2 - Advanced Topics.md
      *
      * Hacker News Rankings for Mainframe/COBOL Posts
      *
      * We will explore the popular Hacker News website for this
      * challenge. Hacker News is an online community started by Paul
      * Graham for sharing "Anything that good hackers would find
      * interesting. That includes more than hacking and startups".
      *
      * A Little Background
      * The site offers a dynamic list of posts/stories, submitted by
      * users, each of which could be expanded into its own unique
      * comment thread. Readers can upvote or downvote links and
      * comments, and the top thirty links are featured on the front
      * page. Today, more than five million people read Hacker News each
      * month, and landing a blog post on the front page is a badge of
      * honor for many technologists.
      *
      * Our Goal:
      * We will be working on a Hacker News 2015-2016 dataset from
      * Kaggle with a full year's worth of stories: Our goal is to
      * extract only the Mainframe/COBOL related stories and assign
      * ranking scores to them based on (a simplified version) the
      * published Hacker News ranking algorithm. We will create a
      * front page report that reflects this ranking order. The
      * algorithm works in a way that nothing stays on the front page
      * for too long, so a story's score will eventually drop to zero
      * over time (the gravity effect). Since our posts are spread out
      * over a year and as older posts will always have a lower (or
      * zero) ranking, we will distort the data so all our stories have
      * the same date and consider only the times in the ranking score
      * calculation. This will give all our posts a fair chance of
      * landing the front page. Our front page report is published at
      * 11:59pm. Here's some additional information on the ranking:
      *
      * http://www.righto.com
      * /2013/11/how-hacker-news-ranking-really-works.html
      *
      * The Plan:
      *
      * + There are several creative ways of accomplishing this but
      *   here's our plan: We will have a COBOL program that reads the
      *   input CSV file and retrieves only the Mainframe/COBOL stories.
      *   It then calculates the ranking score for the stories by
      *   factoring in the time they were posted and the number of votes
      *   they received. Each of the records is then written to an
      *   output dataset along with the ranking score.
      *
      * + We will then use DFSORT to sort the output dataset on ranking
      *   score, highest to lowest and display the posts as a simple
      *   report mimicking the front page.
      *
      * 1>Take a look and familiarize yourself with the dataset on z/OS:
      *   ZOS.PUBLIC.HACKER.NEWS
      *
      *   This is a CSV file that serves as input to your COBOL program.
      *   The file was created by downloading this Kaggle dataset,
      *
      *   https://www.kaggle.com/hacker-news/hacker-news-posts
      *
      *   removing the lengthy URL column that is of no relevance to us,
      *   and uploading it to z/OS. You can directly reference this
      *   dataset in your JCL.  Please avoid making a copy as it is
      *   fairly large with around 300,000 records.
      *
      * 2>Create your COBOL program. This program will:
      *   a.Read in each record in the input CSV file
      *
      *   b.Select only the records that have mention of the words
      *     'mainframe' OR 'cobol' (ignoring case) in the Title field
      *
      *   c.Calculate the ranking score for each record based on the
      *     number of votes it received and the time it was posted
      *     (ignore date as we assume all posts were created on the same
      *     date)
      *
      *              (votes - 1) ** 0.8
      *     score = --------------------
      *               (age + 2) ** 1.8
      *
      *   d.Write the record to an output file along with the ranking
      *     score
      *
      * 3>Copy/Modify/Create a JCL for compiling/linking and running the
      *   program against input/output datasets.
      *
      * 4>Submit the job, debug and test to create the output dataset.
      *
      * 5>Next add a new step in the JCL member to run the DFSORT
      *   utility utility on the output dataset from the previous step.
      *   The sort should be done on the ranking score field, from
      *   highest to lowest. Use DFSORT to also print headers for our
      *   front page. As this is a new utility not covered in the
      *   course, please check out these links to explore this very
      *   powerful and versatile tool:
      *
      *   Getting started with DFSORT:
      *   https://www-01.ibm.com/servers/resourcelink/svc00100.nsf
      *   /pages/zOSV2R3sc236880/$file/iceg200_v2r3.pdf
      *
      *   Example with DFSORT:
      *   https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.1.0
      *   /com.ibm.zos.v2r1.icea100
      *   /ice2ca_Example_10._Sort_with_OUTFIL.htm
      *
      * 6>Run and debug until the front page looks ready! Which posts
      *   ranked among the highest?
      *
      ******************************************************************
      *
      *                  Input file z/OS Dataset Attributes
      *
      *zowe files ls ds "ZOS.PUBLIC.HACKER.NEWS" -a --zosmf-p LearnCOBOL
      *-
      *  dsname: ZOS.PUBLIC.HACKER.NEWS
      *  blksz:  27885
      *  catnm:  CATALOG.USERAA
      *  cdate:  2020/07/09
      *  dev:    3390
      *  dsorg:  PS
      *  edate:  ***None***
      *  extx:   4
      *  lrecl:  143
      *  migr:   NO
      *  mvol:   N
      *  ovf:    NO
      *  rdate:  2020/07/25
      *  recfm:  FB
      *  sizex:  900
      *  spacu:  CYLINDERS
      *  used:   83
      *  vol:    VPWRKZ
      *  vols:   VPWRKZ
      *
      ******************************************************************
      *
      * I created this output dataset based on the attributes of the
      * input dataset. Unfortunately, I could not get COBOL and/or JCL
      * and/or z/OS to cooperate and WRITE the records to this dataset.
      *
      *                    (I am still learning.)
      *
      * After growing a few more gray hairs, I decided to table this
      * part of the problem until I learned a little more about creating
      * datasets. Meanwhile, I told JCL to send COBOL's FILTERED records
      * records to SYSOUT.
      *
      * What is the difference between DISPLAYing a record on SYSOUT and
      * WRITEing a record to SYSOUT?
      *
      * It turns out that SYSOUT is limited to 112 columns when it
      * receives data via COBOL's DISPLAY command.
      *
      * WRITEing the data to SYSOUT allows wider reports (150 columns
      * in this particular program.)
      *
      *//FILTERED  DD SYSOUT=*,OUTLIM=15000         < alternative write
      *
      *                 Output file z/OS Dataset Attributes
      *                         (did not work)
      *
      *zowe files ls ds "z81011.frntpage" -a --zosmf-p LearnCOBOL
      *-
      *  dsname: Z81011.FRNTPAGE
      *  blksz:  15000
      *  catnm:  CATALOG.ZOS3
      *  cdate:  2020/07/26
      *  dev:    3390
      *  dsorg:  PS
      *  edate:  ***None***
      *  extx:   1
      *  lrecl:  150
      *  migr:   NO
      *  mvol:   N
      *  ovf:    NO
      *  rdate:  2020/07/26
      *  recfm:  FBA
      *  sizex:  7
      *  spacu:  BLOCKS
      *  used:   0
      *  vol:    VPWRKE
      *  vols:   VPWRKE
      *
      ******************************************************************
      *
      *                    Job Control Language
      *
      *//HACKRANK JOB 1,NOTIFY=&SYSUID
      *//***************************************************/
      *//COBRUN  EXEC IGYWCL
      *//COBOL.SYSIN  DD DSN=&SYSUID..CBL(HACKRANK),DISP=SHR
      *//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(HACKRANK),DISP=SHR
      *//***************************************************/
      *// IF RC = 0 THEN
      *//***************************************************/
      *//RUN     EXEC PGM=HACKRANK
      *//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
      *//HACKNEWS  DD DSN=ZOS.PUBLIC.HACKER.NEWS,DISP=SHR    < read
      *//FILTERED  DD DSN=&SYSUID..FRNTPAGE,DISP=SHR         < write
      *//SYSOUT    DD SYSOUT=*,OUTLIM=15000                  < display
      *//CEEDUMP   DD DUMMY
      *//SYSUDUMP  DD DUMMY
      *//***************************************************/
      *// ELSE
      *// ENDIF
      *
      ******************************************************************
       environment division.
       input-output section.
       file-control.
            select hack-in-file         assign to HACKNEWS
            organization is sequential.

            select hack-filtered-file   assign to FILTERED
            organization is sequential.

       data division.
       file section.
       fd hack-in-file recording mode f.

       01 hack-in                  pic x(143).

       fd hack-filtered-file recording mode f.

       01 hack-filtered            pic x(150).

       working-storage section.

      * Holds contents of the current datetime function
       01 datetime.
           02 year                 pic 9(4).
           02 mo                   pic 99.
           02 da                   pic 99.
           02 ho                   pic 99.
           02 mi                   pic 99.
           02 se                   pic 99.
           02 hs                   pic 99.
           02 plsormns             pic x.
           02 tzh                  pic 99.
           02 tzm                  pic 99.

      * Contents are extracted (MOVEd) from datetime for the
      * report header
       01 todays-date.
           02 todays-month         pic xx.
           02 filler               pic x value "/".
           02 todays-day           pic xx.
           02 filler               pic x value "/".
           02 todays-year          pic x(4).
      *
      * Contents can be extracted from datetime in the future
       01 report-time.
           02 hh                   pic 99 value 23.
           02 filler               pic x value ":".
           02 mm                   pic 99 value 59.
      *
      *
      * Strings in table elements CANNOT contain variable names. The
      * contents of this data item will be MOVEd to the first element
      * of the table after receiving contents of todays-date &
      * report-time.
      *
       01 first-report-line.
           02 filler               pic x(62) value spaces.
           02 filler               pic x(25)
           value "Hacker News Front Page".
           02 filler               pic x(34) value spaces.
           02 tdys-date-out        pic x(10).
           02 filler               pic x(4) value spaces.
           02 rpt-time-out         pic x(5).
      *     02 filler               pic x(35) value spaces.

       01 report-header-lines.
           02 rpt-hdr-ln1          pic x(140).

           02 rpt-hdr-ln2.
              03 filler            pic x(60) value spaces.
              03 filler            pic x(27)
              value "All Mainframe/COBOL stories".
              03 filler            pic x(53) value spaces.

           02 rpt-hdr-ln3.
              03 filler            pic x(35)
              value "***********************************".
              03 filler            pic x(35)
              value "***********************************".
              03 filler            pic x(35)
              value "***********************************".
              03 filler            pic x(35)
              value "***********************************".

           02 rpt-hdr-ln4.
              03 filler            pic x(3) value spaces.
              03 filler            pic x(3) value "ID#".
              03 filler            pic x(45) value spaces.
              03 filler            pic x(5) value "Title".
              03 filler            pic x(52) value spaces.
              03 filler            pic x(6) value "Author".
              03 filler            pic x(10) value spaces.
              03 filler            pic x(4) value "Time".
              03 filler            pic x(5) value spaces.
              03 filler            pic x(5) value "Score".
              03 filler            pic x(2) value spaces.

           02 rpt-hdr-ln5.
              03 filler            pic x(35)
              value "***********************************".
              03 filler            pic x(35)
              value "***********************************".
              03 filler            pic x(35)
              value "***********************************".
              03 filler            pic x(35)
              value "***********************************".

       01 filler redefines report-header-lines.
           02 rpt-hdr-ln occurs 5 times
                                   pic x(140).

       01 hack-in-record.
           02 hack-in-id           pic x(8).
           02 hack-in-title        pic x(96).
           02 hack-in-points       pic x(4).
           02 hack-in-comments     pic x(4).
           02 hack-in-author       pic x(15).
           02 hack-in-create-dt    pic x(16).

       01 create-date-time-stamp.
           02 create-date-stamp    pic x(10).
           02 filler               pic x.
           02 create-time-stamp    pic x(5).

       01 created-hour             pic xx.

       01 actual-time-stamp.
           02 actual-hour          pic xx.
           02 filler               pic x value ":".
           02 actual-minute        pic xx.

       01 hour                     pic 99.
       01 minute                   pic 99.

       01 points                   pic 9(3).
       01 comments                 pic 9(3).
       01 votes                    pic 9(3).
       01 age                      pic 99v9(5).
       01 score                    pic s9v9(5) sign is leading separate.

       01 counter                  pic 9.

       01 search-strings.
           02 search-string-1      pic x(15) value "mainframe".
           02 search-string-2      pic x(15) value "cobol".

       01 inspected-title          pic x(96).

       01 hack-out-record.
           02 hack-out-id          pic x(8).
           02 filler               pic xxx value spaces.
           02 hack-out-title       pic x(96).
           02 filler               pic x value space.
      *     02 hack-out-points      pic zzz9.
      *     02 filler               pic x(5) value spaces.
      *     02 hack-out-comments    pic zzz9.
      *     02 filler               pic x(5) value spaces.
           02 hack-out-author      pic x(15).
           02 filler               pic x value space.
           02 hack-out-time        pic x(5).
           02 filler               pic x(3) value spaces.
           02 hack-out-score       pic +9.9(5) usage is display.
      *    02 filler               pic xx value space.

       01 end-of-file-flag         pic x.
           88 EOF                  value "Y".

       procedure division.
       100-primary.
           perform 105-open-files
           perform 110-obtain-current-date
           perform 115-write-report-headers
                    varying counter from 2 by 1
                    until counter is equal to 6
      *
      *   a.Read in each record in the input CSV file
           perform 120-extract-from-csv
                    until EOF
      *
      *   b.Select only the records that have mention of the words
      *     'mainframe' OR 'cobol' (ignoring case) in the Title field
      *    perform 120-do-something
      *
      *   c.Calculate the ranking score for each record based on the
      *     number of votes it received and the time it was posted
      *
           perform 190-close-files.
      *
       9999-end-program.
           stop run.
      *
      *
      *
       105-open-files.
           open input hack-in-file
           open output hack-filtered-file.

       110-obtain-current-date.
      *
      * Obtain today's date for report header
      * Write first line of report
           move function current-date to datetime
           move mo to todays-month
           move da to todays-day
           move year to todays-year
           move todays-date to tdys-date-out
           move report-time to rpt-time-out
           move first-report-line to rpt-hdr-ln(1)
           move rpt-hdr-ln(1) to hack-filtered
      *     display hack-filtered
           write hack-filtered.

       115-write-report-headers.
           move rpt-hdr-ln(counter) to hack-filtered
      *     display hack-filtered
           write hack-filtered after advancing 1 line.

       120-extract-from-csv.
      *
      * Read file
           read hack-in-file
              at end move "Y" to end-of-file-flag
           end-read

           unstring hack-in delimited by ","
           into     hack-in-id
                    hack-in-title
                    hack-in-points
                    hack-in-comments
                    hack-in-author
                    hack-in-create-dt
           end-unstring
      *
      * csvdata> ,1,2,lpellegr,mm/dd/yyyy hh:mm
      *                        || ||      ||
      * Sometimes              mm (1-12) is only one digit
      *                           ||      ||
      * Sometimes                 dd (1-31) is only one digit
      *                                   ||
      * Sometimes                         hh (0-23) is only one digit
      *
      * Unstring date-time-stamp into separate date & time fields
           unstring hack-in-create-dt delimited by space
           into     create-date-stamp
                    create-time-stamp
           end-unstring
      *
      * Unstring time-stamp into separate hour & minute fields
           unstring create-time-stamp delimited by ":"
           into     created-hour
                    actual-minute
           end-unstring
      *
      * Is the hour a single digit (<10)?
           evaluate function test-numval-f(created-hour)
              when zero
                 if function numval-f(created-hour) < 10 then
                    string   "0"            delimited by x'00'
                             created-hour   delimited by x'00'
                    into     actual-hour
                    end-string
                 else
                    move created-hour to actual-hour
                 end-if
           end-evaluate
      *
      * Are all extracted alphanumeric values valid numerals?
           if    function test-numval-f(actual-hour)
                 is equal to zero
           AND   function test-numval-f(actual-minute)
                 is equal to zero
           AND   function test-numval-f(hack-in-points)
                 is equal to zero
           AND   function test-numval-f(hack-in-comments)
                 is equal to zero
           then
      *
      * convert them into computational numerals
                 compute points = function numval-f(hack-in-points)
                 compute comments = function numval-f(hack-in-comments)
                 compute hour = function numval-f(actual-hour)
                 compute minute = function numval-f(actual-minute)
      *
      * and move on with the next step in the process
                 perform 121-search-for-strings
           end-if.

       121-search-for-strings.
      *
      *   b.Select only the records that have mention of the words
      *     'mainframe' OR 'cobol' (ignoring case) in the Title field
           move zero to counter
           move function lower-case(hack-in-title) to inspected-title
           inspect inspected-title tallying counter for all
                    search-string-1

           if counter is greater than zero then
              perform 122-create-output-record
           end-if

           move zero to counter
           inspect inspected-title tallying counter for all
                    search-string-2

           if counter is greater than zero then
              perform 122-create-output-record
           end-if.

       122-create-output-record.
      *
      * Copy input fields to output fields
           move hack-in-id to hack-out-id
           move hack-in-title to hack-out-title
      *    move hack-in-points to hack-out-points
      *    move hack-in-comments to hack-out-comments
           move hack-in-author to hack-out-author
           move actual-time-stamp to hack-out-time
      *
      *   c.Calculate the ranking score for each record based on the
      *     number of votes it received and the time it was posted
           perform 130-calculate-score
           perform 140-write-record.

       130-calculate-score.
      *
      *   c.Calculate the ranking score for each record based on the
      *     number of votes it received and the time it was posted
      *     (ignore date as we assume all posts were created on the same
      *     date)
      *
      * Calculate age using current date @11:59pm as a reference point.
      *
           compute age rounded = (hh - hour) + (minute / 60)
      *
      * Instructions are vague about how to calculate votes. I will
      * assume that votes are the sum of points and comments.
      *
      *    move hack-in-points to points
      *    move hack-in-comments to comments

           add points to comments giving votes
      *
      *              (votes - 1) ** 0.8
      *     score = --------------------
      *               (age + 2) ** 1.8
      *
           compute score = (votes - 1) ** 0.8 / (age + 2) ** 1.8
      *
      *     move zero to hack-out-score

      *     display "Points: " points " | Comments: " comments
      *              with no advancing
      *     display " | Votes: " votes " | Age: " age
      *              with no advancing
      *     display " | Hour: " hour " | Minute: " minute
      *
           move score to hack-out-score.

       140-write-record.
           move hack-out-record to hack-filtered
      *     display hack-out-record
           write hack-filtered after advancing 1 line.

       190-close-files.
           close hack-in-file
           close hack-filtered-file.

      *
      *
      *
      *       <!WHEW!>
      *
      *
      *
      *
      *
