      ******************************************************************
      *                                                                *
      *    BigNum v1.0 - a game by Dan Sanderson (aka Doc Brown)       *
      *                                                                *
      * Ported to Enterprise COBOL v6.3 for z/OS by Cliff Chipman      *
      *                                                                *
      * BigNum is a fairly simple game.  You have five places to put   *
      * digits to construct a five digit number.  These five digits    *
      * are picked randomly (from 0 to 9), one by one.  As they are    *
      * picked, you choose where to put it.  Once you place the digit, *
      * you can't move it.  The goal is to construct the largest       *
      * possible number with those digits.                             *
      *                                                                *
      * The board looks like this:                                     *
      *                                                                *
      *        a   b   c   d   e                                       *
      *      ---------------------                                     *
      *      |   |   |   |   |   |                                     *
      *      ---------------------                                     *
      *                                                                *
      *  To place a digit, simply press the letter.                    *
      *                                                                *
      * If you get the largest number possible, you get back twice     *
      * your bet!  If you get the first digit the largest you get 25%  *
      * of your bet back.                                              *
      *                                                                *
      ******************************************************************

       identification division.
       program-id.    bignum.
       author.        Chipman.

       data division.
       working-storage section.
      *
      * Input variables
       01 bet-in                         pic x(4).
       01 player-in                      pic x(4).

      * Calculation variables
       01 bet                            pic 9(4).
       01 minimum-bet                    pic 999        value 200.
       01 wallet                         pic 9(4)       value 1000.
       01 reward                         pic 9(4)       value zero.

       01 table-indices.
          02 board-idx                   pic 9.
          02 table-idx                   pic 9.

       01 tables.
          02 biggest-num occurs 5 times  pic 9.
          02 player-num occurs 5 times   pic x          value space.
          02 board-num occurs 5 times    pic 9.

      * minutes, seconds, and hundreths-second from datetime function
      * helps randomize the generated random number
       01 datetime.
           02 yyyy                       pic 9(4).
           02 mo                         pic 99.
           02 dd                         pic 99.
           02 hh                         pic 99.
           02 mi                         pic 99.
           02 ss                         pic 99.
           02 hs                         pic 99.
           02 plsormns                   pic x.
           02 tzh                        pic 99.
           02 tzm                        pic 99.
      *
      * Used for debugging
       01 datetime-label.
           02 filler                     pic x(4)       value "yyyy".
           02 filler                     pic xx         value "mo".
           02 filler                     pic xx         value "dd".
           02 filler                     pic xx         value "hh".
           02 filler                     pic xx         value "mm".
           02 filler                     pic xx         value "ss".
           02 filler                     pic xx         value "hs".

      * factors help further randomize the generated random number
       01 factor                         pic 9(5)v9(5).
       01 dt-factor.
           02 f-m                        pic 9.
           02 f-s                        pic 99.
           02 f-h                        pic 99.
      *
      * Used for debugging
       01 factor-label.
           02 filler                     pic x(4)       value spaces.
           02 filler                     pic x(12)
                                                  value "|  factor  |".
      *
      * random-num stores the generated random number
       01 random-num                     pic 9v9(5).
      *
      * random-int stores the individual random integers
       01 random-int                     pic 9.

       01 numbers.
          02 playernum                   pic 9(5)       value zero.
          02 biggestnum                  pic 9(5)       value zero.

      * Binary Flags
       01 continue-flag                  pic x.
           88 skip                                      value "n".

       01 play-again-flag                pic x.
           88 playagain                                 value "y".
           88 exitpgm                                   value "n".

       01 enough-money-flag              pic x.
           88 enough-money                              value "y".
           88 not-enough-money                          value "n".

       01 go-back-flag                   pic x.
           88 go-back                                   value "y".

       01 occupied-flag                  pic x.
           88 occupied                                  value "y".
           88 not-occupied                              value "n".
      *
      * Constant message strings
       01 occupied-msg                   pic x(32)
                         value "There is already a number there.".

       01 fortystars                     pic x(40)
                 value "****************************************".

       01 playing-board-lines.
          02 pbl-line1                   pic x(30)
                          value "   a     b     c     d     e  ".

          02 pbl-line2                   pic x(30)
                          value " -----------------------------".

          02 pbl-line3                   pic x(31)
                          value "|     |     |     |     |     |".

          02 pbl-line4                   pic x(30)
                          value " -----------------------------".

          02 pbl-prefix                  pic xxx        value "|  ".
          02 pbl-number                  pic x.
          02 pbl-suffix                  pic xx         value spaces.
      *
      * Display variables
       01 bet-out                        pic $ZZZ9.
       01 minimum-bet-out                pic $ZZ9.
       01 wallet-out                     pic $ZZZ9.
       01 reward-out                     pic $ZZZ9.

       procedure division.
       100-main-paragraph.
           move minimum-bet to minimum-bet-out
           perform 110-display-title-screen
      *
      * Initialize flags:
      * Set playagain to TRUE
      * Set enough-money to TRUE (not-enough-money to FALSE)
           move "y" to play-again-flag

           if wallet IS GREATER THAN OR EQUAL TO minimum-bet then
              move "y" to enough-money-flag
           end-if

           perform 120-conditional until exitpgm or not-enough-money.

       9999-end-program.
           display spaces
           display "You left the game with " wallet-out
           stop run.

       110-display-title-screen.
           display spaces
           display fortystars fortystars
           display "BigNum v1.0 -- " with no advancing
           display "a game by Dan Sanderson (aka Doc Brown)."
           display spaces
           display "Ported to Enterprise COBOL v6.3 for z/OS "
                    with no advancing
           display "by Cliff Chipman."
           display " -- July 16, 2020."
           display fortystars fortystars.

       120-conditional.
      * perform 200 through 300 while bet < minimum-bet OR bet > wallet
           perform 200-clear-tables-loop
           perform 300-place-your-bet
           perform 400-obtain-random-number
           perform 500-fill-board-tables-loop
      *
      * Initialize more flags:
      * Set occupied to FALSE
      * Set go-back to FALSE
           move "n" to occupied-flag
           move "n" to go-back-flag
      *
           display "The board currently looks like:"
           perform 600-display-board
           perform 700-player-places-numbers-loop
      *
      * Sort random number into biggest number
           sort biggest-num descending
      *
           perform 800-board-full.

       200-clear-tables-loop.
           perform 210-clear-tables varying table-idx from 1 by 1
                    until table-idx is equal to 6.

       210-clear-tables.
           move zero to biggestnum
           move zero to playernum
           move zeros to biggest-num(table-idx).
           move zeros to board-num(table-idx)
           move spaces to player-num(table-idx).

       300-place-your-bet.
           move wallet to wallet-out
           display spaces
           display "You have " wallet-out " in your wallet."
           display "Enter your bet (" minimum-bet-out " minimum); "
           display "'h' for instructions;"
           display " or 'q' to quit: " with no advancing

           accept player-in
           move function lower-case(player-in) to player-in

           if player-in IS EQUAL TO "h" then
              perform 310-instructions
              go to 300-place-your-bet
           end-if

           if player-in IS EQUAL TO "q" then
              move "n" to play-again-flag
      *       exit paragraph
      * Flag does not work at this point - have to test for flag's
      * condition at the beginning of the each succeeding paragraph.
              go to 9999-end-program
           end-if

           if function test-numval(player-in) IS NOT EQUAL ZERO then
              move ZERO to bet
           else
              compute bet = function numval(player-in)
      *
      * Player entered a numeric value for bet
      * Does he have enough money to cover the bet?
      * If not, bet no more than contents of wallet
      *
              if bet IS GREATER THAN OR EQUAL TO wallet then
                 display "Betting it all!"
                 move wallet to bet
              end-if
              subtract bet from wallet
           end-if
      *
      * Comment out these display lines after debugging
      *     display "end of 300-place-your-bet paragraph"
      *     display "bet: " bet
      *     display "wallet: " wallet
      *     display "enough-money-flag: " enough-money-flag

           move wallet to wallet-out
           display "Wallet: " wallet-out.

       310-instructions.
           display spaces
           display "BigNum is a fairly simple game.  You have five "
                    with no advancing
           display "places to put digits to construct"
           display "a five digit number. These digits are picked "
                    with no advancing
           display "randomly (from 0 to 9), one by one."
           display "As they are picked, you choose where to put it. "
                    with no advancing
           display "Once you place the digit, you"
           display "can't move it. The goal is to construct the largest"
                    with no advancing
           display " possible number with those"
           display "digits."
           display spaces
      *
      * Insert a continue prompt here to exit paragraph:
           display "Continue? (Y/N): " with no advancing
           accept continue-flag
           move function lower-case(continue-flag) to continue-flag
      *
           if skip then exit paragraph
           end-if
      *
           display "The board looks like this:"
           display spaces
           display pbl-line1
           display pbl-line2
           display pbl-line3
           display pbl-line4
           display spaces
           display "To place a digit, simply press the letter. If you "
                    with no advancing
           display "get the largest number"
           display "possible, you get back twice your bet! If you get "
                    with no advancing
           display "the first digit the largest"
           display "you get 25% of your bet back.".

       400-obtain-random-number.
           move function current-date to datetime
      *
      * Copy specific datetime fields to specific dt-factor fields
           move mi to f-m
           move ss to f-s
           move hs to f-h
           move dt-factor to factor
      *
      * Convert factor to decimal
           divide 100000 into factor
      *
      * Generate random number
           compute random-num = function random
           add factor to random-num.
      *    .
      * Comment out these display lines after debugging
      *     display spaces
      *     display datetime space factor
      *     display datetime-label space factor-label.

       500-fill-board-tables-loop.
           perform 510-fill-board-tables varying table-idx from 1 by 1
                   until table-idx is equal to 6.

       510-fill-board-tables.
           multiply 10 by random-num
           move random-num to random-int
      *
      * Comment out this display line after debugging
      *     display "R: " random-num " |I: " random-int

           move random-int to board-num(table-idx)
           move board-num(table-idx) to biggest-num(table-idx).

       600-display-board.
      *     display spaces
           display pbl-line1
           display pbl-line2

           perform 610-display-board-table-loop varying board-idx
                   from 1 by 1 until board-idx = 6

           display pbl-prefix
           display pbl-line4.
      *     display spaces.

       610-display-board-table-loop.
           display pbl-prefix with no advancing
           display player-num(board-idx) with no advancing
           display pbl-suffix with no advancing.

       700-player-places-numbers-loop.
           perform 710-place-numbers varying table-idx from 1 by 1
                   until table-idx is equal to 6.

       710-place-numbers.
           display spaces
           display "The number is: " board-num(table-idx).
           display "Specify a column (a-e): " with no advancing
           accept player-in
           move function lower-case(player-in) to player-in

           evaluate true
           when player-in is equal to "a"
                if player-num(1) IS EQUAL TO space then
                   move board-num(table-idx) to player-num(1)
                else
                   display occupied-msg
                   go to 710-place-numbers
                end-if

           when player-in is equal to "b"
                if player-num(2) IS EQUAL TO space then
                   move board-num(table-idx) to player-num(2)
                else
                   display occupied-msg
                   go to 710-place-numbers
                end-if

           when player-in is equal to "c"
                if player-num(3) IS EQUAL TO space then
                   move board-num(table-idx) to player-num(3)
                else
                   display occupied-msg
                   go to 710-place-numbers
                end-if

           when player-in is equal to "d"
                if player-num(4) IS EQUAL TO space then
                   move board-num(table-idx) to player-num(4)
                else
                   display occupied-msg
                   go to 710-place-numbers
                end-if

           when player-in is equal to "e"
                if player-num(5) IS EQUAL TO space then
                   move board-num(table-idx) to player-num(5)
                else
                   display occupied-msg
                   go to 710-place-numbers
                end-if

           when player-in is equal to "z"
                go to 9999-end-program

           when player-in is equal to "0"
                go to 9999-end-program

           when other
                go to 710-place-numbers

           end-evaluate

           perform 600-display-board.

       800-board-full.
           perform 810-move-alpha-to-numeric-loop varying table-idx
                   from 1 by 1 until table-idx = 6

           perform 820-move-table-digits-to-num

           display spaces
           display "The board is full."
           display "Your number is: " with no advancing
           display playernum
           display spaces
           display "The biggest number is: " with no advancing
           display biggestnum

           perform 830-calculate-reward

           if reward is greater than zero then
              display "You won " reward-out
           end-if
      *
      * Comment out these display lines after debugging
      *     display "reward: " reward space reward-out
      *     display "wallet: " wallet space wallet-out

           if wallet IS GREATER THAN OR EQUAL TO minimum-bet then
              move "y" to enough-money-flag
           else
              move "n" to enough-money-flag
           end-if.

       810-move-alpha-to-numeric-loop.
           move player-num(table-idx) to board-num(table-idx).

       820-move-table-digits-to-num.
           compute playernum =  board-num(1) * 10000 +
                                board-num(2) * 1000 +
                                board-num(3) * 100 +
                                board-num(4) * 10 +
                                board-num(5) * 1

           compute biggestnum = biggest-num(1) * 10000 +
                                biggest-num(2) * 1000 +
                                biggest-num(3) * 100 +
                                biggest-num(4) * 10 +
                                biggest-num(5) * 1.

       830-calculate-reward.
      * Calculate player's reward:
      * If you get the largest number possible, you get back twice
      * your bet!  If you get the first digit the largest you get 25%
      * of your bet back.

           move zero to reward
           move reward to reward-out
           evaluate true
           when playernum is equal to biggestnum
              multiply bet by 2 giving reward
              display "You made the largest number possible!"

           when board-num(1) is equal to biggest-num(1)
              multiply bet by 0.25 giving reward
              display "You got the first digit!"

           when other
              display "Sorry, no money awarded this round."
           end-evaluate

           add reward to wallet
           move reward to reward-out
           move wallet to wallet-out.
