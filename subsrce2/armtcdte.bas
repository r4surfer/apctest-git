        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  TTTTT   CCC   DDDD   TTTTT  EEEEE   *~
            *  A   A  R   R  MM MM    T    C   C  D   D    T    E       *~
            *  AAAAA  RRRR   M M M    T    C      D   D    T    EEEE    *~
            *  A   A  R   R  M   M    T    C   C  D   D    T    E       *~
            *  A   A  R   R  M   M    T     CCC   DDDD     T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMTCDTE - Translates a Terms Code into actual dates.     *~
            *            If code is not on file sets dates to Base Date *~
            *            supplied.                                      *~
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
            * 09/12/86 ! Original                                 ! ERN *~
            * 07/11/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARMTCDTE" (code$,           /* Terms Code                 */~
                        base$,           /* Base Date (YYMMDD)         */~
                        cash_disc,       /* Cash Discount Percent      */~
                        discdue$,        /* Discount Due Date          */~
                        netdue$,         /* Net Due Date               */~
                        grace%)          /* Allowable Grace Days       */


        dim                                                              ~
            base$6,                      /* Base Date to Calc From     */~
            code$20,                     /* Terms Code to Decipher     */~
            date$8,                      /* Work Variable              */~
            disc$8,                      /* Disc Terms                 */~
            discdue$6,                   /* Discount Due Date          */~
            net$8,                       /* Net Terms                  */~
            netdue$6,                    /* Net Due Date               */~
            newbase$10,                  /* Work Variable for EOM Calc */~
            tdate$6,                     /* Temporary work date        */~
            udate$8                      /* Unformatted text date      */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMTERMS ! Terms Code File                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARMTERMS",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 1, keylen = 20

            call "OPENCHCK" (#1, 0%, f2%(1), 0%, " ")


        REM *************************************************************~
            *        S U B R O U T I N E   L O G I C                    *~
            * --------------------------------------------------------- *~
            * Translate Terms Code supplied into actual dates.  If      *~
            * Terms are not on file set dates for terms 0% 0 Days Net 0 *~
            *************************************************************

            call "READ100" (#1, code$, f1%(1))
            if f1%(1) = 1% then L10170

*       ** Code is not on file.  Set to Base Date and Exit.
            cash_disc = 0
            discdue$  = base$
            netdue$   = base$
            grace%    = 0%
            goto exit_subroutine

L10170
*       ** Code is on File.  Get data and translate it
            get #1 using L10190, cash_disc, disc$, net$, eom%, grace%
L10190:         FMT XX(50), PD(14,4), 2*CH(8), 2*BI(1)
            if disc$ = " " then disc$ = net$

*        First Do Discount Terms
            date$ = disc$  :  gosub translate_terms
            if date$ < base$ then date$ = base$
            discdue$ = date$

*        And Now do the Net Terms
            date$ = net$  :  gosub translate_terms
            if date$ < discdue$ then date$ = discdue$
            netdue$ = date$

*        All Done
            goto exit_subroutine


        translate_terms
*        Turn encryption in DATE$ into an actual real live date.
            if len(date$) = 6 then return   /* Specific Date Given     */

            days% = 0%
            convert str(date$,,3) to days%, data goto L10420
L10420:     if str(date$, 5) = "EOM" then L10490

*        Calculate Terms based on DAYS offset
            call "DATE" addr("G+", str(base$,,6), days%, date$, ret%)
            if ret% <> 0% then date$ = " "
            return

L10490
*        Calculate EOM terms
            newbase$ = base$
            tdate$ = base$
            call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$,7%,2%) to date%
            if date% >= eom% then add% = 2% else add% = 1%
            convert str(udate$,1%,4%) to year%
            convert str(udate$,5%,2%) to month%
            month% = month% + add%
            if month% <= 12% then L10590
                year% = year% + 1%
                month% = month% - 12%
L10590:     newbase$ = "YYYYMM01" :  days% = days% - 1%
            convert year% to str(newbase$,1%,4%), pic(0000)
            convert month% to str(newbase$,5%,2%), pic(00)
            call "DATFMTC" (newbase$)
            call "DATUFMTC" (newbase$)
            call "DATE" addr("G+", newbase$, days%, date$, ret%)
            if ret% <> 0% then date$ = " "
            return


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_subroutine

            end
