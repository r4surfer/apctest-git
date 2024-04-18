        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  EEEEE  X   X   SSS   U   U  BBBB    *~
            *  H   H  NN  N  Y   Y  E       X X   S      U   U  B   B   *~
            *  HHHHH  N N N   YYY   EEEE     X     SSS   U   U  BBBB    *~
            *  H   H  N  NN    Y    E       X X       S  U   U  B   B   *~
            *  H   H  N   N    Y    EEEEE  X   X   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYEXSUB - SUB-ROUTINE RECORDS A PIPOUT IN THE FILE OF THE*~
            *            SAME NAME, BASED ON PASSED INFORMATION.  PART  *~
            *            OF THE PASSED DATA ALLOWS THE S/R TO UN-RECORD *~
            *            THE PREVIOUS PIPOUT, IF ANY.  THUS, IT IS A    *~
            *            SELF-HEALING PIPOUT SUBROUTINE.                *~
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
            * 02/05/87 ! Original                                 ! JRH *~
            * 09/11/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYEXSUB" (lotnbr$,         /* Lot number                 */~
                        storecd$,        /* Store code                 */~
                        partnbr$,        /* Inventory part number      */~
                        pipdate$,        /* PIPOUT date                */~
                        qty)             /* Number of parts to PIPOUT  */

        dim blankdate$8,                 /* Blank unfmt date           */~
            lotnbr$16,                   /* Lot number                 */~
            partnbr$25,                  /* Inventory part number      */~
            pipdate$8,                   /* PIPOUT date                */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            storecd$3,                   /* Store code                 */~
            tagnr$19,                    /* PIPOUT file TAG number     */~
            wrkdate$8                    /* Date work area             */

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            wrkdate$ = " "
            call "DATUNFMT" (wrkdate$)

            if beenherebefore% <> 0% then goto L10000
                beenherebefore% = 1%
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
            *  #1 ! PIPOUT   ! Planned inventory use detail rec         *~
            *  #2 ! PIPMASTR ! Planned inventory Master file            *~
            *  #3 ! SFCUM2   ! Cumulative Sales Forecast                *~
            *  #4 ! HNYMASTR ! Inventory Master File                    *~
            *  #5 ! SYSFILE2 ! System Defaults File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "PIPOUT", varc, indexed, recsize = 64,            ~
                                keypos =  1, keylen = 56,                ~
                     alt key 1, keypos = 20, keylen = 37

            select #2, "PIPMASTR", varc, indexed, recsize = 2024,        ~
                                keypos =  2, keylen = 25,                ~
                     alt key 1, keypos =  1, keylen = 26

            select #3, "SFCUM2", varc, indexed, recsize = 1985,          ~
                                keypos =  1, keylen = 25

            select #5, "SYSFILE2", varc, indexed, recsize = 500,         ~
                        keypos = 1, keylen = 20

            call "OPENCHCK" (#1, fs%( 1), f2%( 1), 300%, rslt$( 1))
            call "OPENCHCK" (#2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            fs%(3) = 1%
            call "OPENCHCK" (#5, fs%( 5), f2%( 5), 0%, rslt$( 5))

L10000: REM *************************************************************~
            *    M A I N   S U B R O U T I N E   P R O C E S S I N G    *~
            *************************************************************

            if min(fs%()) < 0% then exit_program
            tagnr$ = "EX" & str(storecd$,,3) & lotnbr$

        REM First, get rid of any previous PIPOUT for this PART/LOT/STORE
            init (hex(00)) plowkey$
            str(plowkey$,1,44) = str(tagnr$) & partnbr$
            call "PLOWNXT1" (#1, plowkey$, 44%, f1%(1))
               if f1%(1) = 0% then write_new_pipout
            get #1 using L11600, index%, qtyx
L11600:         FMT  POS(45), BI(4), POS(57), PD(14,4)
            delete #1
            call "PIPFLAGS" (partnbr$, 1%, index%, qtyx, #2, #3)

        REM Then, output a PIPOUT for the new expiration date (+ ATC)
        write_new_pipout
            if pipdate$ = " " or pipdate$ = blankdate$ then exit_program
            if qty < 1 then exit_program

            atc% = 999%
            call "READ100" (#2, partnbr$, f1%(2))
                if f1%(2) = 0% then exit_program
            get #2 using L12550, atc%
L12550:         FMT  POS(2023), BI(2)
            atc% = max(0%, mod(atc%, 1000%))
            if atc% > 490% then exit_program

            call "DATE" addr ("G+", pipdate$, atc%, wrkdate$, ret%)
               if ret% <> 0% then exit_program

            call "PIPINDEX" (#5, wrkdate$, index%, ret%)
               if ret% <> 0% then exit_program

            write #1 using L60040, tagnr$, partnbr$, index%, time, qty
            call "PIPFLAGS" (partnbr$, 1%, index%, -qty, #2, #3)
            goto exit_program

        REM *************************************************************~
            *  PIPOUT file record layout.                               *~
            *************************************************************

L60040:     FMT CH(19),                  /* Tag number                 */~
                CH(25),                  /* Part number                */~
                BI(4),                   /* Date in binary             */~
                CH(8),                   /* Time of day                */~
                PD(14,4)                 /* Quantity                   */

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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            end
