        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  X   X  PPPP   RRRR   N   N  TTTTT   *~
            *  A   A  R   R    I     X X   P   P  R   R  NN  N    T     *~
            *  AAAAA  RRRR     I      X    PPPP   RRRR   N N N    T     *~
            *  A   A  R   R    I     X X   P      R   R  N  NN    T     *~
            *  A   A  R   R  IIIII  X   X  P      R   R  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIXPRNT - Passes data to the subroutine 'ARIPRSUB',      *~
            *            which then prints customer export invoices.    *~
            *            Data selected are all records in 'ARIINVRF'    *~
            *            for the current user. Data passed are the      *~
            *            related customer code and invoice number.      *~
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
            * 11/08/87 ! Original  (Clone of ARIPRINT)            ! MJB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        REM *************************************************************~
            *This program calls the subroutine 'ARIXPRSB' -             *~
            * Print Customer EXPORT Invoices.                           *~
            *************************************************************

        dim                                                              ~
            cust_code$9,                 /* Customer code from ARIINVRF*/~
            hdr$40,                      /* ASKUSER constant           */~
            invoice$8,                   /* Invoice # from ARIINVRF    */~
            msg$(3)79,                   /* ASKUSER messages           */~
            plowkey$20,                  /* Current user/ARIINVRF key  */~
            rptid$6                      /* Report ID                  */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
        REM *************************************************************

            mat f2% = con : mat f1% = zer

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #3  ! ARIINVRF ! A/R Invoice Repost file                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "ARIINVRF",                                      ~
                        varc,     indexed,  recsize =   20,              ~
                        keypos =    1, keylen =  20

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            get rslt$(3 ) using L02200, comp%
L02200:         FMT POS(17), BI(4)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if comp% > 0% then L09220
L09140:         comp% = 0%
                hdr$ = "*** CANCEL REQUEST ***"
                msg$(1) = "There are no Invoices scheduled to print"
                msg$(3) = "Press RETURN to cancel program"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L09140
                goto exit_program            /* Nothing to do! */
L09220:     plowkey$ = "xxx"
            str(plowkey$,4) = all(hex(00))
            dup%, prt%, abend% = 0%
            rptid$ = "ARI006"
            select printer

        REM *************************************************************~
            *           M A I N   P R O G R A M   L O O P               *~
            *************************************************************

        main_program_loop
            call "PLOWNEXT" (#3, plowkey$, 3%, f1%(3))
            if f1%(3) = 0% then goto end_of_job
            get #3 using L10100, cust_code$, invoice$

        REM RECORD LAYOUT FOR FILE 'ARIINVRF' ***************************
L10100:         FMT  XX(3),              /* User ID                    */~
                     CH(9),              /* Customer code              */~
                     CH(8)               /* Invoice number             */

            call "ARIXPRSB" (dup%, prt%, abend%, cust_code$, invoice$,   ~
                rptid$)
            if abend% <> 0% then goto abend_program  /* Did it abort? */
            goto main_program_loop                   /* No- continue */

        end_of_job
            if prt% <> 0% then goto L10280 /* Print anything? */
L10210:         comp% = 0%                /* No- so inform the user */
                hdr$ = "*** PRINT FILE EMPTY ***"
                msg$(1) = "There were no EXPORT Invoices to be printed"
                msg$(3) = "Press RETURN to continue"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then goto L10210
L10280:     plowkey$ = "xxx"
            str(plowkey$,4) = all(hex(00))
            call "DELETE" (#3, plowkey$, 3%)
        abend_program
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto exit_program

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

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

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
