        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR    SSS   TTTTT   OOO   V   V  RRRR    *~
            *  S      E      R   R  S        T    O   O  V   V  R   R   *~
            *   SSS   EEEE   RRRR    SSS     T    O   O  V   V  RRRR    *~
            *      S  E      R   R      S    T    O   O   V V   R   R   *~
            *   SSS   EEEEE  R   R   SSS     T     OOO     V    R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERSTOVR - Handles proper clearing of the Serial Master   *~
            *            file after a user decides to "Start Over"      *~
            *            after serial number entries have already been  *~
            *            made.  The serial number work file is cleared  *~
            *            by the simple method of scratching it.         *~
            *            The S/N Work File is read and each entry (S/N) *~
            *            is checked against the SERMASTR file.          *~
            *            If the Serial # Status is '6' the S/N will be  *~
            *            deleted from the SERMASTR file.                *~
            *            If the Serial # Status is '7' the S/N status   *~
            *            will be restored to it's prior status before   *~
            *            the user grabbed it.  Otherwise the S/N must   *~
            *            be in the TIF file and it's status will not    *~
            *            be resolved until Data Save time.              *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/21/87 ! Original                                 ! LDJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SERSTOVR" (index%,          /* Line Item Pointer.  If = 0 */~
                                         /* all entries in workfile    */~
                                         /* will be processed/scratched*/~
                                         /* If > 0 then only entries   */~
                                         /* for that item will be      */~
                                         /* processed, WorkFile will   */~
                                         /* NOT be scratched in that   */~
                                         /* instance.                  */~
                        status$,         /* Status to Change back from */~
                        source$,         /* Status to Restore To       */~
                        #3,              /* SERMASTR UFB               */~
                        #4)              /* SERWORK  UFB               */

        dim                                                              ~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            source$1,                    /* Status to Select/Chg from  */~
            status$1                     /* Status to Change S/N to.   */

        dim f1%(04)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.19.00 03/13/87 Serial number, Lot tracking     "
        REM *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            if index% = 0% then break% = 0% else break% = 3%
            plowkey$ = all(hex(00))
            str(plowkey$,,3%) = bin(index%,3)

        REM *************************************************************~
            *             M A I N   P R O C E S S I N G                 *~
            *-----------------------------------------------------------*~
            * Read Work File, check to see if we should delete from     *~
            * SERMASTR or restore original Status.                      *~
            *************************************************************
            call "PLOWNEXT" (#4, plowkey$, break%, f1%(4))
            if f1%(4) = 0% then exit_routine
*          CALL "SHOSTAT" ("Releasing Any Reserved Serial #'s, " &      ~
*                          "One Moment Please.")
L10080:     if break% = 0% then L10100
            if str(plowkey$,,3%) <> str(key(#4),,3%) then exit_routine
L10100:     get #4 using L10210, str(readkey$,26%), str(readkey$,,25%)
            call "READ101" (#3, readkey$, f1%(3))
            if f1%(3) = 0% then L10180
            if str(key(#3,2),,1%) = "6" then delete #3
            if str(key(#3,2),,1%) <> "7" then L10180
            REM *** Restore Original Status ***
            put #3 using L10200, source$
            rewrite #3
L10180:     call "READNEXT" (#4, f1%(4))
            if f1%(4) = 1% then L10080
L10200:     FMT CH(1)
L10210:     FMT POS(4), CH(20), CH(25)

        exit_routine
            if index% = 0% then call "FILEBGON" (#4)                     ~
            else call "DELETE" (#4, plowkey$, break%)
            end
