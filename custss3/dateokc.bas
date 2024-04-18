        REM *************************************************************~
            *                                                           *~
            *  DDDD    AAA   TTTTT  EEEEE   OOO   K   K   CCC           *~
            *  D   D  A   A    T    E      O   O  K  K   C   C          *~
            *  D   D  AAAAA    T    EEEE   O   O  KKK    C              *~
            *  D   D  A   A    T    E      O   O  K  K   C   C          *~
            *  DDDD   A   A    T    EEEEE   OOO   K   K   CCC           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATEOKC - Tests a date to make sure it is a valid date.   *~
            *           Returns date normalized in input string plus    *~
            *           date in Integer plus error message text usable  *~
            *           by calling program to detect & display errors   *~
            *           (if any).                                       *~
            *           If dimensioned length of date variable is 10    *~
            *           then it is assumed that century digits are      *~
            *           desired.                                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 01/24/80 ! ORIGINAL (SWIPED FROM 2200T PROGRAMMING) ! BCW *~
            * 06/01/81 ! UPDATE ERROR MESSAGE STYLE AND ADD MONTH ! BCW *~
            *          ! NUMBER OF DAYS RIGHT TEST.               !     *~
            * 05/02/83 ! FREE FORMAT ENTRY (E.G. 050283 OR 030502)! KAB *~
            * 11/20/85 ! Totally Rewrote This S.O.B.              ! LDJ *~
            * 09/22/87 ! Modified to accept Century in Date String! LDJ *~
            * 09/02/88 ! Modified to add current Century,         ! RJM *~
            *          !   if appropriate to do so.               !     *~
            * 09/13/88 ! Changed on goto @ 10390 pos=5            ! RJM *~
            * 09/19/88 ! Changed Format test @10500 to allow      ! RJM *~
            *          !   Entry of YYYYMMDD past year 2000.      !     *~
            * 12/08/88 ! Still Buggy! We try yet one more time to ! LDJ *~
            *          !   get it right ...                       !     *~
            * 04/09/96 ! Changes for Century System Conversion    ! LDJ *~
            * 08/18/97 ! Changes for the year 2000.               ! DXL *~
            * 11/02/20 ! CR2722 Reset pivot year                  ! RDB *~
            *************************************************************

            sub "DATEOKC" (pass$,      /* IN: date in any format       */~
                                       /* OUT: formatted date          */~
                           date%,      /* OUT: Integer date as CCYYMMDD*/~
                           error$)     /* Error message or blank       */

            dim date$10,                 /* Work Variable for Date     */~
                error$79,                /* Error Message to Pass back */~
                format$1,                /* System Date Format (A,E,S) */~
                p%(2),                   /* Search Locator Array       */~
                pass$1,                  /* Passed in Date             */~
                cc$2,                    /* Century to Append, if need */~
                test$10                  /* Work Variable for Date     */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10140
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

L10140: REM *************************************************************

            attempt% = 0%
            error$ =" "
            date$ = pass$ : date% = 0%
try_again:  if format$ = " " then call "EXTDTFMT" addr(format$)
            if len(str(pass$)) >= 10% then f% = 10% else f% = 8%

        REM *** Check for Date in Internal Format ***
            if str(date$,1%,1%) = hex(00) then call "DATFMTC" (str(date$,,f%))

        REM *************************************************************~
            *   Normalize the Date by Removing Date Separator Characters*~
            *   and shuffling the date around to format YYYYMMDD.       *~
            *************************************************************
        REM *** Remove Date Separator Characters ***
        REM *** First Normalize Separator Charactors to Slashes ***
        REM TRAN(STR(DATE$,,LEN(DATE$)),"/-/./ /,")REPLACING
            for x% = 1% to len(date$)
              if str(date$,x%,1%) < hex(30) then CHG_SEP
              if str(date$,x%,1%) < hex(3a) then NEXTX
CHG_SEP:         str(date$,x%,1%) = "/"
NEXTX:     next x%

SEARCH_AGAIN:
            search date$ = "/" to p%()    /* Check for Slashes          */
            if p%(1%) = 0% then NORMALIZE /* No more Slashes            */
            if p%(1%) > 2% then L10340    /* Check for leading zero fill*/
               date$ = "0" & date$
               goto SEARCH_AGAIN
L10340:     if p%(2%) = 0% or p%(2%) - p%(1%) > 2%                       ~
               then str(date$,p%(1%)) = str(date$,p%(1%)+1%)             ~
               else str(date$,p%(1%),1%) = "0" /* Insert Leading Zero */
               goto SEARCH_AGAIN


NORMALIZE:
        REM *** Take Care of 'Leftover' Characters if any . . .
*           if f% = 8% then str(date$,7%) = " " else str(date$,9%) = " "
            n% = len(date$)

        REM *** Do the Century Digits Need to be Added? ***
            if n% = 8% then NORMALIZECC
        REM *** Is the Date Format already Normalized? ***
            if n% <> 6%     then TEST_DATE           /* Invalid Date */
            if format$ = "S" then ADD_CENTURY_2_STD
            if str(date$,1%,2%) > "41" then ADD_CENTURY_2_STD   /* CR2722 */
            if str(date$,1%,2%) = "00" then ADD_CENTURY_2_STD
            if format$ <> "A" then ADD_CENTURY_2_NON_STD
               if str(date$,1%,2%) > "12" then ADD_CENTURY_2_STD

ADD_CENTURY_2_NON_STD:
            if str(date$,5%,2%) < "41" then cc$ = "20" else cc$ = "19" /* 2722*/
            str(date$,5%,4%) = cc$ & str(date$,5%,2%)
            goto AMERICAN
ADD_CENTURY_2_STD:
            if str(date$,1%,2%) < "41" then cc$ = "00" else cc$ = "19" /* 2722*/
            date$ = cc$ & date$
            goto TEST_DATE
NORMALIZECC:
        REM *** Is the Date Format already Normalized? ***
            if format$ = "S"           then TEST_DATE
            if str(date$,3%,2%) > "41" then TEST_DATE              /* CR2722 */
            if str(date$,3%,2%) = "00" then TEST_DATE
            if str(date$,1%,2%) > "18" and str(date$,5%,2%) < "13" then TEST_DATE
AMERICAN:   if format$ = "E" then EUROPE
        REM *** Below Assumes American Date Format - MMDDYYYY ***
            test$ = str(date$,5%,4%) & str(date$,1%,4%)
            goto STUFF_NORMALIZED_DATE
EUROPE: REM *** Below Assumes European Date Format - DDMMYYYY ***
            test$=str(date$,5%,4%) & str(date$,3%,2%) & str(date$,,2%)

STUFF_NORMALIZED_DATE: date$ = test$

        REM *************************************************************~
            *   Determine if Date is valid, convert to Integer if Yes & *~
            *   return to caller.  Otherwise jump to Error Exit.        *~
            *************************************************************
TEST_DATE:
            convert date$ to date%, data goto ERROR_EXIT
/* CR2722 */
            if date% >= 19000101% and date% <= 42000101% then GOOD_DATE 

            if len(date$)              < 6% then ERROR_EXIT   /* Invalid Date */

               attempt% = attempt% + 2%
               date$ = str(pass$,1%,len(pass$)-attempt%) & " "

                  goto try_again

GOOD_DATE:  init (hex(20)) date$
            put date$ using DTE_FMT, date%
DTE_FMT:    FMT PD(11,1)
            call "DATE" addr("GJ", str(date$,,6%), str(test$,,5%), x%)
                if x% <> 0% then ERROR_EXIT            /* Invalid Date */

            /* Normalize to User Format   */
            call "DATFMTC" (str(date$,,f%),date%)
            pass$ = date$
            end

ERROR_EXIT
            error$ = "Sorry, Date is in wrong format or has illegal characters: " & pass$
            date% = 0%
            end
