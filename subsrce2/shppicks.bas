        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP   PPPP   IIIII   CCC   K   K   SSS    *~
            *  S      H   H  P   P  P   P    I    C   C  K  K   S       *~
            *   SSS   HHHHH  PPPP   PPPP     I    C      KKK     SSS    *~
            *      S  H   H  P      P        I    C   C  K  K       S   *~
            *   SSS   H   H  P      P      IIIII   CCC   K   K   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPPICKS - Prints Pick Lists Based On The Criteria        *~
            *            Passed From The Caller (SHPDOCPR).             *~
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
            * 09/24/86 ! Original                                 ! JIM *~
            * 01/28/87 ! Skip over order if on credit hold        ! ERN *~
            * 06/09/87 ! HNYMASTR, SHPLINES, BIN$ modifications.  ! JIM *~
            * 08/11/87 ! HES's SO Polymer customization           ! JIM *~
            * 10/01/87 ! Cleaned heading on selections page. PRR  ! DAW *~
            * 10/18/88 ! Dimed WEIGHT$ to be the same as caller.  ! DAW *~
            * 03/01/90 ! modified COM statement.  Changed SHIP_TO$! LAB *~
            *          ! from 30 to 31.                           ! LAB *~
            * 03/19/90 ! Added sorting and printing of Bin Locns. ! JEF *~
            * 09/26/90 ! Added shipping instructions & 'How Ship'.! JDH *~
            *          !    Above per PRRs 11088, 11673.          !     *~
            * 06/14/91 ! Added Shipping Label function (SHPLABSB).! JIM *~
            * 06/19/91 ! PRR 12024.  Qty printed from Order Entry ! JDH *~
            *          !   is Open Qty - Schld - PreInvoiced.     !     *~
            *          ! PRR 12056.  No more SHOSTAT at end.      !     *~
            * 07/25/91 ! Print Bin Location when Source of        ! SID *~
            *          !   Locations Switch is 'A' or 'M' and     !     *~
            *          !   don't print when the switch is 'N'.    !     *~
            *          ! Also print all Bins when Max # of Bins   ! SID *~
            *          !  is equal to Zero.                       !     *~
            * 08/28/91 !(QC-FIXES) Major overhaul to change from  ! RJB *~
            *          !   single page to line by line print and  !     *~
            *          !   to properly call 'HNYBINSB'.           !     *~
            * 12/18/91 !Fixed bugs related to work files being    ! LDJ *~
            *          !   closed and garbage being written to    !     *~
            *          !   BCKPRIDX.                              !     *~
            * 01/06/92 ! Additional correction to Larry's code.   ! JDH *~
            *          !   Fixed page break & shipping info.      !     *~
            * 02/21/92 ! PRR 11690  Optional print of price on PL.! JDH *~
            *          !   Added SO Order Date.                   !     *~
            *          ! PRR 12023  Added SO Text to Pick List.   !     *~
            * 06/10/92 ! PRR 12443  Added Customer PO # to header.! JDH *~
            *          ! PRR 12427  Cartons & wts print as blanks !     *~
            *          !   if 0.  Added end of document message.  !     *~
            * 10/20/92 ! Removed FACs from page zero.             ! JIM *~
            * 10/22/92 ! PRR 12645  Changed channel #'s 13 & 14 to! MLJ *~
            *          !   50 & 51 to fix error on Get or Put.    !     *~
            * 10/22/92 ! Changed COMs to DIMs & added Arguments.  ! JDH *~
            * 09/18/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SHPPICKS" (date$, from_date%, high_date$, from_stor$, i$(), ~
                        low_date$, print_labels$, print_price$, so$(),   ~
                        to_date%, to_stor$,                              ~
                        #3,  /* BCKPRIDX- SO Document Print Index File */~
                        #4,  /* BCKMASTR- Backlog Master File          */~
                        #5,  /* BCKLINES- Backlog Line Item File       */~
                        #6,  /* SHPHDRS - Shipment Scheduling / Header */~
                        #7)  /* SHPLINES- Shipment Scheduling / Lines  */

        REM *************************************************************~
            *THIS SUBROUTINE IS CALLED BY 'SHPDOCPR'. IT PRINTS PICK    *~
            *LISTS BASED ON THE CRITERIA PASSED                         *~
            *************************************************************

        dim        /* OLD COM VARIABLES                                */~
            bol$3,                       /* Bill of lading # (BCKPRIDX)*/~
            carton$7,                    /* Edited # of cartons        */~
            cust_code$9,                 /* Customer code from BCKPRIDX*/~
            date$8,                      /* Date for screen display    */~
            from_stor$3,                 /* Low store for compare      */~
            hdr_desc$30,                 /* Header description         */~
            high_date$8,                 /* High date for compare      */~
            i$(24)80,                    /* Screen image               */~
            invoice$8,                   /* Invoice # from SHPHDRS     */~
            low_date$8,                  /* Low date for compare       */~
            po$16,                       /* PO # from BCKMASTR         */~
            print_labels$1,              /* Print Shipping Labels?     */~
            print_price$1,               /* Print Price on Pick List?  */~
            qtyship$10,                  /* Edited quantity shipped    */~
            schd_date$8,                 /* Sched ship date fr SHPHDRS */~
            ship_to$(6)31,               /* Ship to name/addr- BCKMASTR*/~
            so$16,                       /* Sales order # from BCKPRIDX*/~
            so$(2,2)16,                  /* Sales Order # Range For Pri*/~
            to_stor$3,                   /* High store for compare     */~
            type$1,                      /* 'A'=Ackn, 'B'=BOL, 'P'-Pick*/~
            weight$6,                    /* Edited shipment weight     */~
                   /* COMMON DATA FILE ARRAYS                          */~
            f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        dim          /* LOCAL VARIABLES                                */~
            badflag$1,                   /* Reject Code (No-Print)     */~
            bcklkey$99,                  /* Key for BCKLINES file      */~
            bckmkey$25,                  /* Key for BCKMASTR file      */~
            bin$8,                       /* Bin Location from HNYMASTR */~
            bin_loc$(100)20,             /* Array of Bin Locations     */~
            bin_cnt$,                    /* Max # of Bins to Print     */~
            crhold$1,                    /* Credit Hold Flag           */~
            dateactl$8,                  /* Actual ship date (SHPHDRS) */~
            dateschd$8,                  /* Sched. ship date (SHPHDRS) */~
            datesetup$8,                 /* Set-Up Date     (BCKPRIDX) */~
            dateship$8,                  /* Ship date        (SHPHDRS) */~
            err$(4)21,                   /* Reject Messages (No-Print) */~
            hidate$8,                    /* End. Ship Date Range       */~
            howship$20,                  /* How Ship from BCKMASTR/SHPH*/~
            inc_loc$1,                   /* Include HNYLOCNS for Bins  */~
            instr$(2)50,                 /* Shipping instructions      */~
            lineitem$(100)114,           /* Items to ship (no bins)    */~
            lodate$8,                    /* Beg. Ship Date Range       */~
            lot$6,                       /* Lot # from xxxLINES        */~
            mode$1,                      /* Mode of Searching for Bins */~
            order_date$8,                /* SO Order Date              */~
            part$25,                     /* Part Number                */~
            partdesc$32,                 /* Part Description (BCKLINES)*/~
            picksprinted$4,              /* Number Pick Lists Printed  */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            price$10,                    /* SO Line Item Unit Price    */~
            prtbin$1,                    /* Print Bins on Pick List    */~
            prog$8,                      /* Program name               */~
            rptid$6,                     /* Report ID for headers      */~
            seqnr$3,                     /* SO sequence # from BCKLINES*/~
            shipto$(6)31,                /* Ship-to name/addr. BCKMASTR*/~
            shplkey$99,                  /* Key for SHPLINES           */~
            shphkey$99,                  /* Key for SHPHDRS            */~
            store$3,                     /* Store Code                 */~
            txtid_bckl$4,                /* Text ID for SO Lines       */~
            txtid_bckm$4,                /* Text ID for SO Header      */~
            uom$4,                       /* Unit of Measure (Stocking) */~
            workkey$64                   /* Work File key variable     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #3  ! BCKPRIDX ! SO Document Print Index File             *~
            * #4  ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #5  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #6  ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #7  ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #9  ! HNYMASTR ! Inventory Master File                    *~
            * #10 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #11 ! HNYLOCNS ! Stock location master file               *~
            * #12 ! SYSFILE2 ! System Information File                  *~
            * #15 ! TXTFILE  ! System Text File                         *~
            * #50 ! WORKONE  ! Pick Lists not Printed (WORKFILE)        *~
            * #51 ! WORKTWO  ! Pick Lists Printed (WORKFILE)            *~
            *************************************************************~

            select #9, "HNYMASTR", varc, indexed, recsize = 900,         ~
                keypos = 1, keylen = 25,                                 ~
                alt key 1, keypos = 102, keylen = 9, dup,                ~
                    key 2, keypos = 90, keylen = 4, dup

            select #10, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44         ~

            select #11, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42          ~

            select #12, "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos =      1, keylen =  20

            select #15, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #50, "WORKONE",                                       ~
                        varc, indexed, recsize =  46,                    ~
                        keypos =     12, keylen =  28

            select #51, "WORKTWO",                                       ~
                        varc, indexed, recsize =  39,                    ~
                        keypos =      1, keylen =  39


            call "SHOSTAT" ("Picking Lists being selected for print")

            call "OPENCHCK" (#09, fs%(09), f2%(09), 0%, rslt$(09))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#15, fs%(15), f2%(15), 0%, rslt$(15))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "DATEFMT" (date$)

            rptid$ = "SHP003"
            call "EXTRACT" addr("CF", prog$)
            init(hex(00)) plowkey$
            str(plowkey$,1,1) = "P"
            mode$ = "P"
            picksprinted% = 0%

            call "BCKSWTCH" ("BCK", "BINPRINT", prtbin$  , e, e%)
            call "BCKSWTCH" ("BCK", "INC_LOC ", inc_loc$ , e, e%)
            call "BCKSWTCH" ("BCK", "PRNTNUMB", bin_cnt$ , e, e%)
            qty_loc% = 0%
            convert bin_cnt$ to qty_loc%, data goto L09190
L09190:     if qty_loc% = 0% then maxbin% = 100% else maxbin% = qty_loc%

            lodate$ = low_date$
            hidate$ = high_date$

            init(" ") err$()
            err$(1) = "S/O Not on File"
            err$(2) = "S/O is on Credit Hold"
            err$(3) = "BOL Not on File"
            err$(4) = "No S/O Line on File"

        REM *************************************************************~
            *         M A I N   P R O G R A M   L O O P                 *~
            *************************************************************

        next_so      /* Plow primary File (BCKPRIDX) for eligable SO's */
            init(" ") store$, dateship$, type$, cust_code$, so$, bol$
            call "PLOWNXT1" (#3, plowkey$, 1%, f1%(3))
                if f1%(3) = 0% then end_of_report
            get #3 using L10100, store$, dateship$, type$, cust_code$,    ~
                                                    so$, bol$, datesetup$
L10100:       FMT POS(2), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3), CH(6)
            if store$ < from_stor$ or store$ > to_stor$ then next_so
            if so$ <= so$(2,1) or so$ > so$(2,2) then next_so
            if low_date$ = "ALL" then goto L10150
                if dateship$<lodate$ or dateship$>hidate$ then next_so
L10150:     call "DATEFMT" (dateship$)
            gosub bckmastr_read
            goto next_so

        bckmastr_read       /* Header/Master Data (BCKMASTR & SHPHDRS) */
            init(" ") shipto$(), howship$, instr$(), crhold$, carton$,   ~
                      weight$, lot$, qtyship$, dateschd$, dateactl$,     ~
                      hdr_desc$, bckmkey$, shphkey$, txtid_bckm$, po$
            carton, weight = 0
            bckmkey$ = str(cust_code$,,9) & str(so$,,16)
            call "READ100" (#4, bckmkey$, f1%(4))
                if f1%(4) <> 0% then L10400
                     badflag$ = "1"
                     gosub invalid_pick
                     goto L10650
L10400:     get #4 using L10410, po$, shipto$(), howship$, instr$(),      ~
                                txtid_bckm$, order_date$, crhold$
L10410:         FMT POS(26),CH(16), 6*CH(30), POS(422), CH(20), POS(462),~
                    2*CH(50), POS(799), CH(4), POS(806), CH(6), POS(875),~
                    CH(1)
            call "DATEFMT" (order_date$)
            if crhold$ <> "H" then L10440
                badflag$ = "2"
                gosub invalid_pick
                goto L10650
L10440:     call "LINSMASH" (ship_to$())
            if bol$ <> " " then L10480
                hdr_desc$ = "Order Entry"
                goto L10640
L10480:     shphkey$ = str(cust_code$,,9) & str(so$,,16) & str(bol$,,3)
            call "READ100" (#6, shphkey$, f1%(6))
                if f1%(6) <> 0% then L10530
                     badflag$ = "3"
                     gosub invalid_pick
                     goto L10650
L10530:     get #6 using L10550, dateschd$, howship$, instr$(), dateactl$,~
                                carton, weight, invoice$
L10550:         FMT POS(32), CH(6), POS(44), CH(20), POS(84), 2*CH(50),  ~
                    CH(6), POS(210), 2*PD(14,4), POS(234), CH(8)
            call "DATEFMT" (schd_date$)
            convert round(carton,0) to carton$, pic(###,###)
            if carton = 0 then carton$ = " "
            convert round(weight,0) to weight$, pic(######)
            if weight = 0 then weight$ = " "
            hdr_desc$ = "SHPMT SCHEDULING"
L10640:     gosub bcklines_read
L10650:     return

        bcklines_read     /* Shipable Line Items (BCKLINES & SHPLINES) */
            init(" ") seqnr$, poline$, part$, partdesc$, uom$, lot$,     ~
                      lineitem$(), shplkey$, price$
            init(hex(00)) bcklkey$
            qtyopen, qtyschd, qtyship, preinv, price = 0
            lcntr% = 0%
            str(bcklkey$,1,16) = str(so$,,16)
L10870:     call "PLOWNEXT" (#5, bcklkey$, 16%, f1%(5))
                if f1%(5) <> 0% then L10940
                     if lcntr% <> 0% then L10920
                          badflag$ = "4"
                          gosub invalid_pick
                          goto L11150
L10920:              gosub print_detail
                     goto L11145
L10940:     get #5 using L10960, seqnr$, poline$, part$, partdesc$,       ~
                                qtyopen, qtyschd, preinv, price, uom$,   ~
                                lot$, txtid_bckl$
L10960:         FMT POS(26), 2*CH(3), CH(25), CH(32), POS(109),          ~
                    2*PD(14,4), POS(133), 2*PD(14,4), POS(149), CH(4),   ~
                    POS(218), CH(6), POS(242), CH(4)
            call "CONVERT" (price, 2.4, price$)
            if print_price$ <> "Y" then price$ = " "
            if bol$ <> " " then L11020
                qtyship = (qtyopen - qtyschd) - preinv
                goto L11080
L11020:     shplkey$ = str(so$,,16) & str(bol$,,3) & str(seqnr$,,3)
            call "READ100" (#7, shplkey$, f1%(7))
                if f1%(7) <> 0% then L11050
                     qtyship = 0
                     goto L11070
L11050:     get #7 using L11060, qtyship
L11060:         FMT POS(32), PD(14, 4)
L11070:     if qtyship = 0 then L10870
L11080:     convert round(qtyship,2) to qtyship$, pic(###,###.##)
            lcntr% = lcntr% + 1%
            lineitem$(lcntr%) = str(poline$,,3)   & str(seqnr$,,3)     & ~
                                str(part$,,25)    & str(partdesc$,,32) & ~
                                str(price$,,10)   &                      ~
                                str(qtyship$,,10) & str(uom$,,4)       & ~
                                str(lot$,,6)      & str(txtid_bckl$,,4)
            goto L10870
L11145:     gosub pick_printed
L11150:     return

        invalid_pick
            if f2%(50) = 0% then L11330
                call "WORKOPEN" (#50, "IO", 50%, f2%(50))
L11330:     call "DATUNFMT" (dateship$)
            write #50 using L11360, "P", store$, dateship$, type$,        ~
                              cust_code$, so$, bol$, datesetup$, badflag$
L11360:           FMT CH(1), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3),  ~
                      CH(6), CH(1)
            if badflag$ = "2" then L11400
                if print_labels$ = "Y" then delete #3
L11400:     return

        pick_printed
            print : print using L60370
            if print_labels$ = "Y" then L11540
                delete #3
                goto L11600
L11540:     if f2%(51) = 0% then L11560
                call "WORKOPEN" (#51, "IO", 50%, f2%(51))
L11560:     call "DATUNFMT" (dateship$)
            write #51 using L11590, "P", store$, dateship$, type$,        ~
                                                    cust_code$, so$, bol$
L11590:           FMT CH(1), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3)
L11600:     return

        print_detail
            init(" ") bin$, poline$, seqnr$, part$, partdesc$, qtyship$, ~
                      uom$, lot$, bin_loc$(), price$, txtid_bckl$
            page%, pcntr% = 0%
            line% = 999%
            picksprinted% = picksprinted% + 1%
            call "SORT" addr(lineitem$(), lcntr%, 114%, lineitem$(), 1%, ~
                                                           31%, "A", "S")
            for pcntr%  = 1% to lcntr%
                if line% > 52% then gosub page_heading
                get lineitem$(pcntr%) using L12120, poline$, seqnr$,      ~
                                   part$, partdesc$, price$, qtyship$,   ~
                                   uom$, lot$, txtid_bckl$
L12120:             FMT 2*CH(3), CH(25), CH(32), 2*CH(10), CH(4), CH(6), ~
                        CH(4)
                if prtbin$ <> "N" then L12160
                     bin$ = " "
                     goto L12190
L12160:         call "HNYBINSB" (mode$, part$, prtbin$, inc_loc$,        ~
                              store$, qty_loc%, bin_loc$(), #9, #10, #11)
                bin$ = str(bin_loc$(1),11,8)
L12190:         print using L60500, bin$, poline$, seqnr$, part$,         ~
                                     partdesc$, price$, qtyship$,        ~
                                     uom$, lot$, " "
                print using L60140
                line% = line% + 2%
                if prtbin$ = "Y" then L12290 /* 'Y' means HNYMASTR only */
                     for bcntr%  = 2% to maxbin%
                          if bin_loc$(bcntr%) = " " then L12290
                          if str(bin_loc$(bcntr%),11,8) =                ~
                                 str(bin_loc$(bcntr%-1%),11,8) then L12280
                          print using L60500, str(bin_loc$(bcntr%),11,8), ~
                                   " ", " ", " ", " ", " ", " ", " ", " "
                          print using L60140
                          line% = line% + 2%
L12280:              next bcntr%
L12290:         gosub'200(txtid_bckl$, 2%)
            next pcntr%
            return

        page_heading
            if page% = 0% and picksprinted% = 1% then gosub page_zero
            page% = page% + 1%
            if page% > 1% then print using L60330, "* CONTINUED *"
            print page
            print using L60040
            print using L60060, "#"
            print using L60080, rptid$, date$, hdr_desc$
            print using L60100, store$, dateship$, cust_code$,            ~
                                         str(so$,,16) & "-" & bol$, page%
            print using L60120
            print using L60284, order_date$, po$
            print using L60180, shipto$(1)
            print using L60190, shipto$(2)
            print using L60210, shipto$(3)
            print using L60230, shipto$(4)
            print using L60250, shipto$(5), carton$, weight$
            print using L60270, shipto$(6)
            print using L60280, howship$
            print using L60185, instr$(1)
            print using L60188, instr$(2)
            line% = 15%
            if page% > 1% then L12600
                gosub'200(txtid_bckm$, 1%)
L12600:     print using L60140
            print using L60160, "#"  /* COLUMN HEADERS */
            print using L60310
            line% = line% + 3%
            return

        page_zero
            select printer(134)
            call "SETPRNT" ("SHP003", prog$, 0%, 0%)
            print page
            print using L60052
            print skip(1)
            print using L60092, rptid$, date$, hdr_desc$
            print skip (4)
L12802:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L12810
                str(i$(), i%, 1%) = " "
                goto L12802
L12810:     print using L60350, "   ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60350, i$(n%)
            next n%
            return

        end_of_report
            if picksprinted% > 0% then goto L13060
            if f2%(50) <> 0% then L13020
              gosub page_zero    /* No Pick Lists printed but, */
              goto L13060         /* exception report should.   */
L13020:         comp% = 2%
                call "ASKUSER" (comp%, " ", "No Pick Lists found matchin"~
           & "g the Parameters Entered", " ", "Press RETURN to continue")
                if comp% = 0% then L13510 else L13020
L13060:     convert picksprinted% to picksprinted$, pic(####)
            print page
            print skip(2)
            print using L60052
            print skip(2)
            print using L60350, "Printing Completed " & picksprinted$ &   ~
                                              " Pick Lists were Printed."
            if f2%(50) <> 0% then L13270
                print skip(2)
                print using L60350, "The following Pick Lists were not PR"~
                                   & "INTED:"
                print skip(1)
                init(hex(00)) workkey$
L13190:         call "PLOWNEXT" (#50, workkey$, 0%, f1%(50))
                     if f1%(50) = 0% then L13270
                get #50 using L13220, cust_code$, so$, bol$, badflag$
L13220:             FMT POS(12), CH(9), CH(16), CH(3), POS(46), CH(1)
                convert badflag$ to badflag%
                print using L60350, "Customer: " & str(cust_code$ )&      ~
                       "  S/O: " & str(so$) & " BOL: " & str(bol$) &     ~
                       "  Reason: " & err$(badflag%)
                goto L13190
L13270:     close printer
            call "SETPRNT" ("SHP003", prog$, 0%, 1%)
            if print_labels$ <> "Y" then L13510
                call "SHPLABSB" ("P", from_date%, from_stor$, low_date$, ~
                                 so$(), to_date%, to_stor$, #3, #4, #6)
                init(hex(00)) workkey$
L13320:         call "PLOWNEXT" (#51, workkey$, 0%, f1%(51))
                     if f1%(51) = 0% then L13400
                          call "REDALT1" (#3, workkey$, 1%, f1%(3))
                               if f1%(3) = 0% then L13320
                                    delete #3
                                    goto L13320
L13400:         if f2%(50) <> 0% then L13510
                     init(hex(00)) workkey$
L13420:              call "PLOWNEXT" (#50, workkey$, 0%, f1%(50))
                          if f1%(50) = 0% then L13510
                     get #50, using L13450, datesetup$
L13450:                  FMT POS(40), CH(6)
                     call "REDALT1" (#3, workkey$, 1%, f1%(3%))
                          if f1%(3) = 0% then L13420
                               write #3 using L13490, workkey$, datesetup$
L13490:                              FMT CH(39), CH(6)
                               goto L13420
L13510:     if f2%(50) <> 0% then L13532
                call "FILEBGON" (#50)
                f2%(50) = 1%
L13532:     if f2%(51) <> 0% then L13540
                call "FILEBGON" (#51)
                f2%(51) = 1%
L13540:     goto exit_program

        REM *************************************************************~
            *  Print Text For Either SO Lines or SO Header              *~
            *************************************************************

        deffn'200(txtid$, type%)
            if txtid$ = hex(ffffffff) then return
            if txtid$ = " " then return
                txt% = (page% * 100%) + line%
                comp% = 0%
L16290:         call "TXTPRINT" (#15, f2%(15), 134%, txtid$, "SHP003",   ~
                     9%, line%, 52%, "T", " ", comp%)
                if comp% = 0% then goto L16340
                     gosub page_heading
                     goto L16290
L16340:         if txt% = (page% * 100%) + line% then return
                if pcntr% = lcntr% then return
                     if type% = 1% then print else print using L60140
                     line% = line% + 1%
                     return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L60040: %S A L E S   O R D E R   P I C K   L I S T     +-----+-----------~
        ~+-----------+----------------------+------+
L60052: %S A L E S   O R D E R   P I C K   L I S T

L60060: %                                              !STORE! SHIP DATE ~
        ~! CUSTOMER# ! SALES ORDER     -BOL ! PAGE !
L60080: %######  PRINTED ######## PER ################ !-----!-----------~
        ~!-----------!----------------------!------!
L60092: %######  PRINTED ######## PER ################

L60100: %                                              ! ### !  ######## ~
        ~! ######### ! #################### ! #### !
L60120: %                                              +-----+-----------~
        ~+-----------+----------------------+------+
L60140: %+--------+----+----+-------------------------+------------------~
        ~--------------+----------+----------+----+------+----------+
L60160: %!BIN LOC !LINE!SEQ#!PART NUMBER              !PART DESCRIPTION  ~
        ~              !   PRICE  ! QUANTITY !UOM ! LOT  !QTY PICKED!
L60180: %SHIP-TO: ##############################

L60185: %                                                 Ship Instr: ###~
        ~###############################################
L60188: %                                                             ###~
        ~###############################################
L60190: %         ##############################          +--------------~
        ~---+-------------+---------+----------+
L60210: %         ##############################          ! ORDER PICKED ~
        ~BY ! DATE PICKED ! CARTONS !  WEIGHT  !
L60230: %         ##############################          !--------------~
        ~---!-------------!---------!----------!
L60250: %         ##############################          !              ~
        ~   !             ! ####### !  ####### !
L60270: %         ##############################          +--------------~
        ~---+-------------+---------+----------+
L60280: %                                                 How Ship: #####~
        ~###############
L60284: %                                              SO Order Date: ###~
        ~#####    PO: ################
        %################################################################~
        ~##################################################
L60310: %!--------!----!----!-------------------------!------------------~
        ~--------------!----------!----------!----!------!----------!
L60330: %                                                                ~
        ~                  #############
L60350: %               #################################################~
        ~##################################
L60370: %                                             ** END OF DOCUMENT ~
        ~**

        %!########!    !    !                         !                  ~
        ~              !          !          !    !######!          !

        %!--------!----!----!-------------------------!------------------~
        ~--------------!----------!----------!----!------!----------!
        %!                                                               ~
        ~                              !      !          !
L60500: %!########!####!####!#########################!##################~
        ~##############!##########!##########!####!######!##########!

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
