        REM *************************************************************~
            *                                                           *~
            *  W   W  H   H  IIIII   CCC   H   H  M   M   OOO   N   N   *~
            *  W   W  H   H    I    C   C  H   H  MM MM  O   O  NN  N   *~
            *  W   W  HHHHH    I    C      HHHHH  M M M  O   O  N N N   *~
            *  W W W  H   H    I    C   C  H   H  M   M  O   O  N  NN   *~
            *   W W   H   H  IIIII   CCC   H   H  M   M   OOO   N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WHICHMON - SUBROUTINE LOOKS IN SYSTEM DEFAULT FILE AND GET*~
            *            THE LIST OF OPEN MONTHS, COMPUTES WHICH ONE IN *~
            *            THE LIST THIS ONE IS, 0 IF NOT.                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/12/80 ! ORIGINAL                                 ! BW  *~
            * 09/29/80 ! CHANGE BRANCH IF MONTH NOT IN RANGE      ! TEM *~
            * 10/11/86 ! Modified to only check G/L Period        ! ERN *~
            * 10/11/90 ! Replaced ACCEPT with call to GETPARM     ! KAB *~
            *************************************************************

            sub "WHICHMON" (#1, date$, glperiod%)

            dim date$8                   /* Module Date Coming In      */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01022
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
L01022: REM *************************************************************
            call "READ100" (#1, "MONTHS OPEN", onfile%)
            if onfile% = 1% then L10100
        /*         ACCEPT "Error in Finding System Dates Available.",    ~
                          "Press ENTER to acknowledge and EXIT Program"*/
                   call "GETPARM" addr("I ", "A",                        ~
                                       "WHICHMON", " ", "0001", "WCHMON",~
                   2%,                                                   ~
                   "Error in Finding System Dates Available.   ",    44%,~
                   "Press ENTER to acknowledge and EXIT Program",    44%)
                   end
L10100:     call "WHICHPER" (#1, date$, glperiod%) /* Get G/L Period   */
            if glperiod% = 0% then L10180
                call "READ100" (#1, "FISCAL DATES", onfile%)
                get #1 using L10140, monthopen%
L10140:              FMT XX(158), BI(2)
                glperiod% = glperiod% - monthopen% + 2%
                glperiod% = max(1%, min(glperiod%, 3%))
                /* Got to be Valid, but 13th period can mess things up */
L10180:     end                                    /* Returns 0, 1-3   */

