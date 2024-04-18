        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA   PPPP   EEEEE  RRRR   IIIII   OOO   DDDD    *~
            *  S      A   A  P   P  E      R   R    I    O   O  D   D   *~
            *   SSS   AAAAA  PPPP   EEEE   RRRR     I    O   O  D   D   *~
            *      S  A   A  P      E      R   R    I    O   O  D   D   *~
            *   SSS   A   A  P      EEEEE  R   R  IIIII   OOO   DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SAPERIOD - Returns the Summary History Year file key and  *~
            *            bucket for the date passed in. Routine         *~
            *            notifies user of any problems encountered --   *~
            *            caller should just exit.                       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/19/86 ! Original                                 ! ERN *~
	    * 06/25/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SAPERIOD"   (datein$,       /* Date to determine period   */~
                                         /*  and key for.              */~
                          #1,            /* SYSFILE2 channel           */~
                          periods$(),    /* Periods for SA Year        */~
                                         /*  (1st is Summary File key) */~
                          bucket%,       /* 1-13: Valid, A-OK fine.    */~
                                         /*   97: Outside of Calendar  */~
                                         /*   98: No Calendar          */~
                                         /*   99: Invalid date         */~
                          err$)          /* Error Message              */


        dim                                                              ~
            blankdate$,                  /* Blank date for comparison  */~
            cal$(39)6,                   /* SA Calendar                */~
            date$8,                      /* Date to determine period   */~
            datein$8,                    /* Date to determine period   */~
            err$79,                      /* Error message from DATEOK  */~
            periods$(13)6                /* Calendar for Year Returned */

        dim f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System General Informa *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Check critical data before we get too serious.            *~
            *************************************************************

*        First check that the date passed in is ok and unformatted
            init (" ") periods$()
            date$ = datein$
            call "DATEOK"  (date$, p%, err$)
            bucket% = 99% : if err$ <> " " then error_exit
            call "DATUNFMT" (date$)
            blankdate$ = " "
	    call "DATUFMTC" (blankdate$)

*        Now read in the Sales Analysis Calendar
            call "READ100" (#1, "SWITCHS.SA          ", f1%(1))
            bucket% = 98%
            if f1%(1) = 0% then error_exit
                get #1 using L09190, cal$()
L09190:              FMT XX(73), 39*CH(6)
                if str(cal$()) = " " then error_exit

        REM *************************************************************~
            *                M A I N   L O G I C                        *~
            * --------------------------------------------------------- *~
            * First determine the bucket number that the date passed in *~
            * belongs to, then concoct the summary file key.            *~
            * We scan a year at a time so that we don't have to worry   *~
            * about blank periods between years.                        *~
            *************************************************************

            bucket% = 97%
                p1% = 27% : gosub find_period     /* Check NEXT year   */
                p1% = 14% : gosub find_period     /* Check THIS year   */
                p1% =  1% : gosub find_period     /* Check LAST year   */

        error_exit
            if bucket% = 97% then err$ =                                 ~
                "Posting Date is outside of the Sales Analysis Calendar."
            if bucket% = 98% then err$ =                                 ~
                "Sales Analysis Calendar has not been defined."
            if bucket% = 99% then err$ =                                 ~
                "Posting Date is an invalid date: " & datein$ & "."
            end

        find_period
            for p% = p1% + 12%  to  p1%  step -1%
                if cal$(p%) = " " or cal$(p%) = blankdate$ then L10280
                     if date$ >= cal$(p%) then L10310
L10280:     next p%
            return   /* Not in the year we've looked at */

L10310
*        Test to make sure that date found isn't the last date
            return clear all
            if str(cal$(), p% * 6 + 1) = " " or p% = 39% ~
              or str(cal$(), p% * 6 + 1) = blankdate$ then error_exit

*        Found it.  Return requested arguments
            bucket% = p% - p1% + 1%
            if p1% =  1% then str(periods$()) = str(cal$(),  1, 78)
            if p1% = 14% then str(periods$()) = str(cal$(), 79, 78)
            if p1% = 27% then str(periods$()) = str(cal$(),157, 78)
            end

