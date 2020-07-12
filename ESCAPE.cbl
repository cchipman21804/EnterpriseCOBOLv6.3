       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ESCAPE.
       AUTHOR.      CHIPMAN.
      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                      ESCAPE a.k.a. CHASE                      *
      *                                                               *
      * Original Author: unknown                                      *
      *                                                               *
      * Modified by:                                                  *
      * Bill Cotter, Pittsfield, MA in Honeywell 600/6000 BASIC       *
      *                                                               *
      * Ported to Enterprise COBOL v6.3 for z/OS by:                  *
      * Cliff Chipman, Salisbury, MD                                  *
      *                                                               *
      * Based on the BASIC source code in the Jan-Feb 1976 edition of *
      * Creative Computing magazine (pp 76-77)                        *
      *                                                               *
      * https://archive.org/details/Creative_Computing_               *
      *                                            v02n01_Jan-Feb1976 *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *         EEEEE    SSS     CCC      A     PPPP    EEEEE         *
      *         E       S   S   C   C    A A    P   P   E             *
      *         E       S       C       A   A   P   P   E             *
      *         EEEEE    SSS    C       AAAAA   PPPP    EEEEE         *
      *         E           S   C       A   A   P       E             *
      *         E       S   S   C   C   A   A   P       E             *
      *         EEEEE    SSS     CCC    A   A   P       EEEEE         *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      *                           OBJECTIVE
      *
      * The player is trapped in an enclosed labyrinth running from
      * robotic pursuers.  Fortunately, the walls and obstacles are
      * fatal to the robots. The object of the game is to position the
      * player where the robots will crash into the obstacles or into
      * each other.  The walls and obstacles are not fatal to the player
      * because the computer does not allow the player to make a move
      * that brings him into contact with them.  The computer also will
      * not allow the player to commit suicide by moving onto a space
      * already occupied by a robot.  The robots blindly determine their
      * direction of travel based solely on the relative direction of
      * the player. This causes the robots to eventually stumble into
      * the walls and obstacles on the playing field. Five teleports are
      * randomly scattered throughout the playing field.  These devices
      * will randomly transport whatever falls into them to another
      * random location on the playing field.
      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *                        LIST OF VARIABLES
      *
       01 wall         pic x value "X".
      * WALL:         Stores the ASCII/EBCDIC value of the character
      *               designated as a wall on the playing field.
      *
       01 robot        pic x value "o".
      * ROBOT:        Stores the ASCII/EBCDIC value of the character
      *               designated as a robot on the playing field.
      *
       01 player       pic x value "i".
      * PLAYER:       Stores the ASCII/EBCDIC value of the character
      *               designated as the player on the playing field.
      *
       01 teleport     pic x value "^".
      * TELEPORT:     Stores the ASCII/EBCDIC value of the character
      *               designated as a teleport on the playing field.
      *
       01 emptyspace   pic x value space.
      * EMPTYSPACE:   Stores the ASCII/EBCDIC value of the character
      *               designated as empty space on the playing field,
      *               32 (base 10) in ASCII or 64 (base 10) in EBCDIC.
      *               20 (base 16)             40 (base 16)
      *
       01 max-x        pic 99 value 20.
       01 max-y        pic 99 value 10.
       01 min-pursuers pic 99 value 5.
       01 max-pursuers pic 99 value 50.
       01 min-teleports pic 99 value 1.
       01 max-teleports pic 99 value 99.
       01 num-teleports pic 99 value 5.

       01 playing-field.
           02 r              occurs 10 to 50 times depending on max-y.
              03 c     pic x occurs 20 to 100 times depending on max-x
                       value space.
      * A( , ):       This two-dimensional array stores the ASCII values
      *               of each position on the playing field.
      *
       01 pursuers-in  pic xxxx.
       01 pursuers     pic 99.
      * P:            This variable stores the number of pursuers
      *               designated by the player at the beginning of the
      *               game.
      *
       01 numberofdeadrobots    pic 99 value zero.
      * NUMBEROF-
      * DEADROBOTS:   This variable tracks the number of dead robots on
      *               the playing field.  When this number equals [P],
      *               the player wins.
      *
       01 pursuer-coordinates.
           02 pursuerx  pic 999 occurs 5 to 50 times
                        depending on pursuers.
      * PURSUERX( ):  This one-dimensional array stores the X coordinate
      *               of each robot's position on the playing field.
      *
           02 pursuerx1 pic 999 occurs 5 to 50 times
                        depending on pursuers.
      * PURSUERX1( ): This one-dimensional array stores the X coordinate
      *               of each robot's PREVIOUS position on the playing
      *               field.
      *
           02 pursuery  pic 999 occurs 5 to 50 times
                        depending on pursuers.
      * PURSUERY( ):  This one-dimensional array stores the Y coordinate
      *               of each robot's position on the playing field.
      *
           02 pursuery1 pic 999 occurs 5 to 50 times
                        depending on pursuers.
      * PURSUERY1( ): This one-dimensional array stores the Y coordinate
      *               of each robot's PREVIOUS position on the playing
      *               field.
      *
       01 playerx       pic 999.
      * PLAYERX:      This variable stores the X coordinate of the
      *               player's position on the playing field.
      *
       01 playerx1      pic 999.
      * PLAYERX1:     This variable stores the X coordinate of the
      *               player's PREVIOUS position on the playing field.
      *
       01 playery       pic 999.
      * PLAYERY:      This variable stores the Y coordinate of the
      *               player's position on the playing field.
      *
       01 playery1      pic 999.
      * PLAYERY1:     This variable stores the Y coordinate of the
      *               player's PREVIOUS position on the playing field.
      *
       01 x             pic 999.
      * X:            This variable stores the current X coordinate of
      *               an item on the playing field.
      *
       01 y             pic 999.
      * Y:            This variable stores the current Y coordinate of
      *               an item on the playing field.
      *
       01 n             pic 9(4).
      * N:            This variable is used in loops.
      *
       01 player-in     pic x.
      * A$:           This variable accepts input from the player.
      *
       01 playerscore   pic 99 value zero.
      * PLAYERSCORE:  This variable stores the player's wins.
      *
       01 robotscore    pic 99 value zero.
      * ROBOTSCORE:   This variable stores the robot's wins.
      *
       01 pursuers-left pic 99.

      * DO NOT USE CURRENT-DATE function as a random number seed!
      * This causes the same random number to be generated repeatedly!
      * Try this algorithm instead:
      * divide ss by 100 giving factor
      * add factor to random-num
      *
       01 datetime-label.
           02 filler    pic x(10) value spaces.
           02 filler    pic x(4) value "yyyy".
           02 filler    pic xx value "mo".
           02 filler    pic xx value "dd".
           02 filler    pic xx value "hh".
           02 filler    pic xx value "mm".
           02 filler    pic xx value "ss".
           02 filler    pic xx value "hs".

       01 datetime.
           02 yyyy      pic 9(4).
           02 mo        pic 99.
           02 dd        pic 99.
           02 hh        pic 99.
           02 mm        pic 99.
           02 ss        pic 99.
           02 hund-sec  pic 99.
           02 plsormns  pic x.
           02 tzh       pic 99.
           02 tzm       pic 99.

       01 factor        pic 9v999.
       01 random-num    pic 9v9(10).
       01 random-int    pic 9.
       01 random-x      pic 999.
       01 random-y      pic 999.
      *
      * random-num & random-int store the generated random number
      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       01 tab-over      pic x(2) value spaces.

       PROCEDURE DIVISION.
       100-primary.
           perform 110-display-title-screen
           perform 120-instructions-prompt
           perform 130-pursuers-prompt.

      * 200-main SECTION.
       210-initialization-paragraph.
           perform 211-build-north-south-walls varying x from 1 by 1
                                         until x is equal to max-x + 1

           perform 212-build-east-west-walls varying y from 1 by 1
                                         until y is equal to max-y + 1

           move function current-date to datetime
           divide hund-sec by 1000 giving factor
           perform 213-place-random-obstacles varying x from 2 by 1
                                             until x is equal to max-x
                                             after y from 2 by 1
                                             until y is equal to max-y.

      * Comment out the next display line after debugging
           display "Random numbers for teleport placement:"
           perform 214-place-teleports varying n from 1 by 1
                                   until n is equal to num-teleports + 1

      * Comment out the next display line after debugging
           display "Random numbers for pursuers placement:"
           perform 215-place-pursuers varying n from 1 by 1
                                   until n is equal to pursuers + 1

      * Comment out the next display line after debugging
           display "Random numbers for player placement:"
           perform 216-place-player

           move zero to numberofdeadrobots

           perform 220-main-paragraph.
      *       until numberofdeadrobots
      *       is greater than or equal to pursuers.

       220-main-paragraph.
           perform 420-display-playing-field
           perform 440-players-move
           move zero to n
           perform 450-robots-move
                   varying n from 1 by 1
                   until n is equal to pursuers + 1
           .
      * 200-exit-main SECTION.

      * 300-terminating SECTION.
       310-end-program.
           if c(playery, playerx) = robot then
              add 1 to robotscore
              perform 420-display-playing-field
              display "G A M E   O V E R"
           else
              add 1 to playerscore
              perform 420-display-playing-field
              display "Y O U   W I N!"
           end-if.

       320-another-game.
           display "Would you like to play again? (Y/N)"
           accept player-in
           move function lower-case(player-in) to player-in
           EVALUATE true
           when player-in is equal to "y"
              go to 210-initialization-paragraph
           when player-in is not equal to "n"
              go to 320-another-game
           END-EVALUATE.

       330-return-to-operating-system.
           stop run.

      * 300-exit-termination SECTION.

       110-display-title-screen.
      *    display spaces
           display "***************************************************"
           display "                ESCAPE a.k.a. CHASE                "
      *    display spaces
           display "Original Author: unknown"
           display "Modified by: Bill Cotter, Pittsfield, MA in "
      *              with no advancing
           display "Honeywell 600/6000 BASIC"
           display spaces
           display "Ported to Enterprise COBOL v6.3 for z/OS "
      *              with no advancing
           display "by: Cliff Chipman, Salisbury, MD"
           display spaces
           display "Based on the BASIC source code in the Jan-Feb 1976 "
           display "edition of Creative Computing magazine (pp 76-77)  "
           display "https://archive.org/details/"
           display "Creative_Computing_v02n01_Jan-Feb1976"
           display "***************************************************"
           display spaces.

       120-instructions-prompt.
           display "Would you like instructions (Y/N)? "
      *             with no advancing
           accept player-in
           move function lower-case(player-in) to player-in

           EVALUATE true
               WHEN player-in = 'y'
                  perform 121-instructions
               WHEN OTHER
                  CONTINUE
           END-EVALUATE.

       121-instructions.
           display "The player is trapped in an enclosed labyrinth "
      *              with no advancing
           display "running from robotic pursuers. Fortunately, the "
           display "walls and obstacles are fatal to the robots. The "
      *              with no advancing
           display "object of the game is to position the player where "
           display "the robots will crash into the obstacles or into "
      *              with no advancing
           display "each other. The walls and obstacles are not fatal "
           display "to the player because the computer does not allow "
      *              with no advancing
           display "the player to make a move that brings him into "
           display "contact with them. The computer also will not "
      *              with no advancing
           display "allow the player to commit suicide by moving onto a"
           display "space already occupied by a robot. The robots "
      *              with no advancing
           display "blindly determine their direction of travel based "
           display "solely on the relative direction of the player. "
      *              with no advancing
           display "This causes the robots to eventually stumble into"
           display "the walls and obstacles on the playing field. Five "
      *              with no advancing
           display "teleports are randomly scattered throughout the "
           display "playing field. These devices will randomly "
      *              with no advancing
           display "transport whatever falls into them to another "
           display "random location on the playing field."
      *    display spaces
           display "The teleports are (" teleport "). The robots are "
      *              with no advancing
           display "(" robot "). The player is the (" player "). The "
           display "player can move:"
           perform 410-display-controls.

       130-pursuers-prompt.
           display "How many pursuers? (" min-pursuers "-" max-pursuers
                    with no advancing
           display "): " with no advancing
           accept pursuers-in

           if pursuers-in is equal to "zero" or
              pursuers-in is equal to "ZERO" then
              go to 330-return-to-operating-system
           end-if

           if function test-numval(pursuers-in) is not equal zero then
              display "Pursuers quantity is not numeric."
              go to 130-pursuers-prompt
           else
              compute pursuers = function numval(pursuers-in)
           end-if

           EVALUATE true
           when pursuers is less than min-pursuers
              display "Don't you want a challenge?!"
              display "At least try to evade more than four pursuers."
              display spaces
              go to 130-pursuers-prompt

           when pursuers is greater than max-pursuers
              display "You must be a glutton for punishment!"
              display "The playing field gets crowded with more than 50"
      *                 with no advancing
              display "pursuers."
              display spaces
              go to 130-pursuers-prompt

           when pursuers is not equal to function integer(pursuers)
              display "Quit goofing off and give me a whole number!"
              display spaces
              go to 130-pursuers-prompt

           END-EVALUATE.

       211-build-north-south-walls.
           move wall to c (1, x)
           move wall to c (max-y, x).

       212-build-east-west-walls.
           move wall to c (y, 1)
           move wall to c (y, max-x).

       213-place-random-obstacles.
           compute random-num = function random
           add factor to random-num
           compute random-int = function integer(random-num * 10)
      *
      * Comment out the next display lines after debugging
           display "R: " random-num " |I: " random-int " |X: " x
                   with no advancing
           display " |Y: " y

           if random-int
                          is equal to 5
      *                   is greater than or equal to 4 AND
      *        random-int is less than or equal to 5
              then
                 move wall to c (y, x)
           else
                 move emptyspace to c (y, x)
           end-if.

       214-place-teleports.
           perform 430-teleport-character
           move teleport to c (y, x).

       215-place-pursuers.
           perform 430-teleport-character
           move x to pursuerx (n)
           move y to pursuery (n)
           move robot to c (y, x).

       216-place-player.
           perform 430-teleport-character
           move x to playerx
           move y to playery
           move player to c (y, x).

      * 400-subroutines SECTION.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *               DISPLAY PLAYER CONTROLS SUBROUTINE              *
      *                                                               *
       410-display-controls.
           display spaces
           display tab-over "        N"
           display tab-over "   NW       NE"
           display tab-over "     T  Y  U"
           display tab-over "  W  G  *  H  E"
           display tab-over "     V  B  N"
           display tab-over "   SW       SE"
           display tab-over "        S"
           display spaces.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                DISPLAY PLAYING-FIELD SUBROUTINE               *
      *                                                               *
       420-display-playing-field.
           subtract numberofdeadrobots from pursuers
                    giving pursuers-left
           display spaces
           perform 421-display-row varying y from 1 by 1
                               until y is equal to max-y + 1
           perform 410-display-controls
           display "Player @" playerx ", " playery "          "
                    with no advancing
           display "Pursuers: " pursuers-left
           display "  " playerscore with no advancing
           display "                          " robotscore.
      *                                                               *
       421-display-row.
           display y ": " with no advancing
           display r (y).
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                  TELEPORT CHARACTER SUBROUTINE                *
      *                                                               *
      * Place character randomly within the playing field in a space  *
      * that is not currently occupied by a WALL, a TELEPORT, or a    *
      * ROBOT.  Store the X,Y coordinates in the variables X and Y.   *
      *                                                               *
       430-teleport-character.
           compute random-num = function random
           add factor to random-num
           compute random-x = function integer(random-num * max-x)
           move random-x to x
           add 1 to x

           compute random-num = function random
           add factor to random-num
           compute random-y = function integer(random-num * max-y)
           move random-y to y
           add 1 to y

           if c (y, x) IS NOT EQUAL TO emptyspace OR
              x < 2 OR x > max-x - 1 OR
              y < 2 OR y > max-y - 1 then
              go to 430-teleport-character
           end-if
      *
      * Comment out these display lines after debugging
            display "N: " n " X: " x " Y: " y.
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                    PLAYER'S MOVE SUBROUTINE                   *
      *                                                               *
       440-players-move.
      *
      * Store previous position as current position
           move playerx to playerx1
           move playery to playery1
      *
      * Wait for the player to make a move
           display "Enter move: "
      *            with no advancing
           accept player-in

           EVALUATE true
               WHEN player-in = "T" or
                    player-in = "t" or
                    player-in = "7"
                       perform 447-move-nw

               WHEN player-in = "Y" or
                    player-in = "y" or
                    player-in = "8"
                       perform 448-move-north

               WHEN player-in = "U" or
                    player-in = "u" or
                    player-in = "9"
                       perform 449-move-ne

               WHEN player-in = "G" or
                    player-in = "g" or
                    player-in = "4"
                       perform 444-move-west

               WHEN player-in = "H" or
                    player-in = "h" or
                    player-in = "6"
                       perform 446-move-east

               WHEN player-in = "V" or
                    player-in = "v" or
                    player-in = "1"
                       perform 441-move-sw

               WHEN player-in = "B" or
                    player-in = "b" or
                    player-in = "2"
                       perform 442-move-south

               WHEN player-in = "N" or
                    player-in = "n" or
                    player-in = "3"
                       perform 443-move-se

               WHEN player-in = "*" or
                    player-in = "5"
                       CONTINUE

               WHEN player-in = "0"
                       go to 320-another-game

               WHEN OTHER
                       go to 440-players-move
           END-EVALUATE

      * Is the player's current position occupied by a wall or a robot?
           if c (playery, playerx) = wall or
              c (playery, playerx) = robot then

      * Go back to previous position - do not let player commit suicide
                 move playerx1 to playerx
                 move playery1 to playery
                 go to 440-players-move
           end-if

      * Is the player's current position occupied by a teleport?
           if c (playery, playerx) = teleport then

      * Jump through the teleport & see where you end up
                 perform 430-teleport-character
                 move x to playerx
                 move y to playery
           end-if

      * Make previous position empty space
      * Make current position player
           move emptyspace to c (playery1, playerx1)
           move player to c (playery, playerx).

       447-move-nw.
           subtract 1 from playerx
           subtract 1 from playery.

       448-move-north.
           subtract 1 from playery.

       449-move-ne.
           subtract 1 from playery
           add 1 to playerx.

       444-move-west.
           subtract 1 from playerx.

       446-move-east.
           add 1 to playerx.

       441-move-sw.
           add 1 to playery
           subtract 1 from playerx.

       442-move-south.
           add 1 to playery.

       443-move-se.
           add 1 to playery
           add 1 to playerx.
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                    ROBOT'S MOVE SUBROUTINE                    *
      *                                                               *
       450-robots-move.
      *    add 1 to n  <<< not needed with perform varying
      *
      * Check if robot(n) has already terminated into a wall
           move pursuerx(n) to x
           move pursuery(n) to y
           if c (y, x) is equal to wall then
              exit
           end-if
      *
      * Store current position of robot(n) as previous position
           move pursuerx(n) to pursuerx1(n)
           move pursuery(n) to pursuery1(n)
      *
      * Calculate player's direction and move robot toward player
           compute pursuerx(n) = pursuerx(n) +
                                   function sign(playerx - pursuerx(n))
           compute pursuery(n) = pursuery(n) +
                                   function sign(playery - pursuery(n))
      *
      * Comment out this display line after debugging
           display "N: " n " |X: " pursuerx(n) " |Y: " pursuery(n)
      *
      * Vacate robot's previous position
           move pursuerx1(n) to x
           move pursuery1(n) to y
           move emptyspace to c (y, x)
      *
      * What is located in the robot's current position?
      * If the robot ran into a wall, it is dead.
      * If the robot ran into another robot, BOTH robots are dead.
      * If the robot ran into a teleport, it gets teleported.
      * If the robot ran into the player, the player is dead.
           move pursuerx(n) to x
           move pursuery(n) to y

           EVALUATE true
           when c (y, x) is equal to wall
              add 1 to numberofdeadrobots

           when c (y, x) is equal to robot
              add 2 to numberofdeadrobots
              move wall to c (y, x)

           when c (y, x) is equal to player
              move robot to c (y, x)
              move pursuerx1(n) to x
              move pursuery1(n) to y
              move emptyspace to c (y, x)
              perform 420-display-playing-field
              go to 310-end-program

           when c (y, x) is equal to teleport
              perform 430-teleport-character
              move x to pursuerx(n)
              move y to pursuery(n)
              move robot to c (y, x)
              move pursuerx1(n) to x
              move pursuery1(n) to y
              move emptyspace to c (y, x)

           when c (y, x) is equal to emptyspace
              move robot to c (y, x)
              move x to pursuerx(n)
              move y to pursuery(n)
           END-EVALUATE
      *    .
      * Failsafe: comment out after debugging:
           display "Press 0 to exit or any other to continue:"
           accept player-in
           if player-in = "0" then
              go to 330-return-to-operating-system
           end-if
           exit.

