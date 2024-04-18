        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   IIIII  PPPP   IIIII  N   N  DDDD   EEEEE  X   X   *~
            *  P   P    I    P   P    I    NN  N  D   D  E       X X    *~
            *  PPPP     I    PPPP     I    N N N  D   D  EEEE     X     *~
            *  P        I    P        I    N  NN  D   D  E       X X    *~
            *  P      IIIII  P      IIIII  N   N  DDDD   EEEEE  X   X   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPINDEX - Returns the Planning Calendar Index for the    *~
            *            date passed in.  If the date passed in is      *~
            *            blank, returns the index for the System Date.  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/18/85 ! ORIGINAL                                 ! ERN *~
            * 09/11/96 ! Millie date conversion                   ! DER *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "PIPINDEX"   (#1,            /* SYSFILE2 CHANNEL #.        */~
                          datein$,       /* DATE to return index for.  */~
                                         /*   format: YYMMDD.          */~
                          index%,        /* PLANNING CALENDAR INDEX    */~
                                         /*  1 - 490 regardless of     */~
                                         /*  circumstances.            */~
                          ret%)          /* RETURN STATUS FLAG--       */~
                                         /*  0  - All ok. No errors.   */~
                                         /*  1  - No 'MONTH OPEN' rec  */~
                                         /*       found in SYSFILE2.   */~
                                         /*       INDEX% = 490         */~
                                         /*  2  - Date outside of      */~
                                         /*       planning calendar    */~
                                         /*       INDEX% = 1 or 490.   */~
                                         /*  4/8- Invalid date- err    */~
                                         /*       from subrtn DATE.    */~
                                         /*       INDEX% = 1 or 490    */

        dim blankdate$8,                 /* Blank unfmt date           */~
            date$6,                      /* Date to find index for     */~
            datein$6,                    /* Date to find index for     */~
            day1$6                       /* First date of calendar     */

        REM *************************************************************~
            *        CALCULATE PLANNING CALENDAR INDEX                  *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            if str(blankdate$, 1%, 1%) = hex(00) then skip_blnkdate
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)

          skip_blnkdate
            date$ = datein$    /* So caller's value won't be changed   */

            if date$ = " " or date$ = blankdate$ then date$ = date

*        Get first date of Planning Calendar fro SYSFILE2.
            index% = 490 : ret% = 1   /* Return values if not there.   */
            call "READ100" (#1, "MONTHS OPEN", f1%)
            if f1% = 0 then end
                get #1, using L10130, day1$
L10130:              FMT XX(32), CH(6)

*        Now calculate the index in the planning calendar.
            call "DATE" addr("G-", day1$, date$, index%, ret%)
            index% = index% + 1
            if ret% = 0  and  (index% < 1 or index% > 490) then ret% = 2
            index% = min(490, max(1, index%))

            end
