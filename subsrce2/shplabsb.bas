        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP   L       AAA   BBBB    SSS   BBBB    *~
            *  S      H   H  P   P  L      A   A  B   B  S      B   B   *~
            *   SSS   HHHHH  PPPP   L      AAAAA  BBBB    SSS   BBBB    *~
            *      S  H   H  P      L      A   A  B   B      S  B   B   *~
            *   SSS   H   H  P      LLLLL  A   A  BBBB    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPLABSB - Prints Shipping Labels as per calls from       *~
            *            SHPBOLS and SHPPICKS.                          *~
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
            * 12/27/90 ! Original from SHPBOLS for Robbins Sci.   ! JIM *~
            * 10/22/92 ! Aligned COM with SHPPICKS and SHPBOLS.   ! MLJ *~
            * 10/22/92 ! Changed COMs to DIMs & added Arguments.  ! JDH *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SHPLABSB" (keychar$,        /* 'B'= BOLs; 'P'= Pick Slips */~
                        from_date%, from_stor$, low_date$, so$(),        ~
                        to_date%, to_stor$,                              ~
                        #3,              /* BCKPRIDX file UFB          */~
                        #4,              /* BCKMASTR file UFB          */~
                        #6)              /* SHPHDRS file UFB           */

        REM *************************************************************~
            * This subroutine is called by SHPBOLS and/or SHPPICKS. It  *~
            * prints Shipping Labels based on the caller's criteria.    *~
            *************************************************************

        dim        /* OLD COM VARIABLES                                */~
            bckmastr_key$25,             /* Key to BCKMASTR            */~
            bol$3,                       /* Bill of lading # (BCKPRIDX)*/~
            cust_code$9,                 /* Customer code from BCKPRIDX*/~
            from_stor$3,                 /* Low store for compare      */~
            low_date$8,                  /* Low date for compare       */~
            po$16,                       /* PO # from BCKMASTR         */~
            ship_date$8,                 /* Ship date from BCKPRIDX    */~
            ship_to$(6)31,               /* Ship to name/addr- BCKMASTR*/~
            shphdrs_key$28,              /* Key to SHPHDRS             */~
            so$16,                       /* Sales order # from BCKPRIDX*/~
            so$(2,2)16,                  /* Sales Order # Range For Pri*/~
            stor_code$3,                 /* Store code from BCKPRIDX   */~
            to_stor$3,                   /* High store for compare     */~
            type$1,                      /* 'A'=Ackn, 'B'=BOL, 'P'-Pick*/~
                   /* COMMON DATA FILE ARRAYS                          */~
            f1%(64)                      /* = 1 if READ was successful */

        dim        /* LOCAL VARIABLES                                  */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            crhold$1,                    /* Credit Hold Flag           */~
            keychar$1,                   /* Key char passed from cllr  */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            tdate$10                     /* Temporary Date             */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            rptid$ = "SHP009"

        REM *************************************************************~
            *       M A I N   S U B R O U T I N E   L O G I C           *~
            *************************************************************

            call "SHOSTAT" ("Printing Shipping Labels")
            select printer (040)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            plowkey$ = keychar$ : str(plowkey$,2) = all(hex(00))

        main_loop
            call "PLOWNXT1" (#3, plowkey$, 1%, f1%(3))
            if f1%(3) = 0% then goto print_is_done
            get #3 using L10150, stor_code$, ship_date$, type$,           ~
                cust_code$, so$, bol$    /* BCKPRIDX */

L10150:         FMT  /* BCKPRIDX file record layout                    */~
                     XX(1),              /* 'A'ck, 'B'ol, or 'P'ick    */~
                     CH(3),              /* Store code                 */~
                     CH(6),              /* Ship date                  */~
                     CH(1),              /* Type code (A,B,P) again    */~
                     CH(9),              /* Customer code              */~
                     CH(16),             /* Sales order number         */~
                     CH(3),              /* Bill of lading number      */~
                     XX(6)               /* Setup date                 */

            if stor_code$ < from_stor$ then goto main_loop
            if stor_code$ > to_stor$ then goto main_loop
            if so$ <= so$(2,1) or so$ > so$(2,2) then main_loop
            if low_date$ = "ALL" then goto L10360
            ship_date% = 0%
            tdate$ = ship_date$
            call "DATEFMT" (tdate$, ship_date%)

            if low_date$ <> " " and low_date$ <> blankdate$ then goto L10330
            if ship_date$ = " " or ship_date$ = blankdate$ then goto L10360
            goto L10340
L10330:     if ship_date% < from_date% then goto main_loop
L10340:     if ship_date% > to_date% then goto main_loop

L10360:     bckmastr_key$ = str(cust_code$,,9) & so$
            call "READ100" (#4, bckmastr_key$, f1%(4)) /* Get Address  */
            if f1%(4) = 0% then main_loop
            get #4, using L10400, so$, po$, ship_to$(), crhold$
L10400:         FMT POS(10), CH(16), CH(16), 6*CH(30), POS(875), CH(1)
            if crhold$ = "H" then goto main_loop

            shphdrs_key$ = str(cust_code$,,9) & str(so$,,16) & bol$
            cartons = 0
            call "READ100" (#6, shphdrs_key$, f1%(6)) /* Get # cartons */
            if f1%(6) = 0% then L10490  /* header not found, cartons=0  */
            get #6 using L10480, cartons
L10480:         FMT POS(210), PD(14,4)
L10490:     if cartons < 1 then cartons = 1

            gosub ship_to_zip_code
            gosub line_print
            delete #3
            goto main_loop

        line_print
            for labels% = 1% to cartons
                print skip(8)
                print using L60040, ship_to$(1)
                print using L60040, ship_to$(2)
                print using L60040, ship_to$(3)
                print using L60040, ship_to$(4)
                print using L60040, ship_to$(5)
                print using L60040, ship_to$(6)
                print skip(3)
                print using L60040, po$
            next labels%
            return

        ship_to_zip_code
            if str(ship_to$(6),17,1) <> " "                              ~
                or str(ship_to$(6),16,1) <> " "                          ~
                or pos(str(ship_to$(6),27,4) = " ") > 0% then L10770
                     temp$ = str(ship_to$(6),27,4)
                     str(ship_to$(6),28,4) = temp$
                     str(ship_to$(6),27,1) = "-"
L10770:     call "SPCESMSH" (ship_to$(6), 2%)
            call "LINSMASH" (ship_to$())
            return

        print_is_done
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto exit_program

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L60040: %     ##############################

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
            end
