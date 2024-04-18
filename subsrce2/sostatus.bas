
        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS    OOO    SSS   TTTTT    A    TTTTT  U   U   SSS    *~
            *  S      O   O  S        T     A A     T    U   U  S       *~
            *   SSS   O   O   SSS     T    AAAAA    T    U   U   SSS    *~
            *      S  O   O      S    T    A   A    T    U   U      S   *~
            *   SSS    OOO    SSS     T    A   A    T     UUU    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SOSTATUS - Displays relevent S.O. Data For Inquiry.       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/14/87 ! ORIGINAL (Cloned from POSTATUS)          ! HES *~
            * 12/03/87 ! Hook to DEMSTAT with Flag if files are   !     *~
            *          !        sent through to use DEMSTAT or not! JDH *~
            * 12/10/87 ! Moved input for CUSCODE# & SO# to BCKSTAT! JDH *~
            * 12/15/87 ! Change screen layout on invoice display  ! JDH *~
            * 12/17/87 ! Change screen layout again!!! Lucky me   ! JDH *~
            * 12/21/87 ! Check valid demand before link to DEMSTAT! JDH *~
            * 01/15/88 ! Added History files                      ! JDH *~
            * 02/03/88 ! Added Order Status                       ! JDH *~
            * 04/19/88 ! Fixed Status display on Shipment Dtl scrn! JIM *~
            * 02/05/90 ! Took away the been-here-before logic,    ! JDH *~
            *          !  also cleaned up PF prompts.             !     *~
            * 06/06/90 ! Hid PF prompt, captured status.          ! JDH *~
            * 06/22/90 ! 'ORDERED' qty is now the 'CURrent ORDER' ! JDH *~
            *          !  qty; Literal 'OPEN' now 'REMAINING'.    !     *~
            * 02/14/91 ! Added BOL Detail information.            ! JDH *~
            * 07/03/91 ! PRR 12075.  Invoice detail not shown if  ! JDH *~
            *          !   zero shipped that part/that BOL.       !     *~
            * 09/17/91 ! Primary screen title same on all screens.! JDH *~
            * 09/24/91 ! PRR 11862. Toggle for Orig/Current Qty.  ! JDH *~
            * 03/17/92 ! PRR 12116. Status logic mod on LI Det Scr! JDH *~
            *          ! PRR 12388. Fixed call to TXTDSPLY.       !     *~
            * 07/06/92 ! Added select and open of PIPIN file and  ! WPH *~
            *          ! pass it on to DEMSTAT.                   !     *~
            * 11/23/92 ! Added wt. field.  Added shipping status. ! JDH *~
            * 03/22/93 ! PRR 12828. Linked to ARVINVSB.           ! JDH *~
            *          ! PRR 12829. Spelling correction.          !     *~
            * 11/01/93 ! Added Shipping Priority code to display  ! MLJ *~
            *          ! via PF(25) toggle -  Shipment Details    !     *~
            *          ! screen & Order Status Inquiry screen.    !     *~
            * 03/31/94 ! Added visibility into serial numbers.    ! JDH *~
            * 05/25/95 ! PRR 13362- Display store on BOL/INV Hdr  ! RJH *~
            *          !  Detail Screen.                          !     *~
            * 06/16/95 ! Added Customer Name for Marc.            ! JDH *~
            * 06/22/95 ! Enabled PF32 so that it will exit to     ! JDH *~
            *          !  calling program.                        !     *~
            * 08/14/95 ! PRR 13484. More than 9 BOLs now display. ! JDH *~
            * 11/14/95 ! HNYMASTR channel is now a variable.      ! JDH *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "SOSTATUS" (incus$,                /* Customer Code    */~
                            sono$,                 /* Order #          */~
                            sol$,                  /* Blnk for any line*/~
                            mode%,                 /* 2% = Sales units */~
                                                   /* Else Stock units */~
                            #1,                    /* BCKLINES File UFB*/~
                            #2,                    /* SHPLINES File UFB*/~
                            #3,                    /* SHPHDRS  File UFB*/~
                            #4,                    /* BCKMASTR File UFB*/~
                            #5,                    /* ARIMASTR File UFB*/~
                            #6,                    /* ARILINES File UFB*/~
                            #7,                    /* CUSTOMER File UFB*/~
                            #9,                    /* BCKHMSTR File UFB*/~
                            #10,                   /* BCKHLNES File UFB*/~
                            #20,                   /* TXTFILE  File UFB*/~
                            mode1%,                /* 1%= DEMSTAT files*/~
                                                   /*  sent; 0% = not  */~
                            hflag%,                /* 1%=search History*/~
                                                   /* 0%=search Regular*/~
                            #21,                   /* PIPCROSS File UFB*/~
                            #22,                   /* DEMMASTR File UFB*/~
                            #23,                   /* JBMASTR2 File UFB*/~
                            #24,                   /* JBSTATUS File UFB*/~
                            #25,                   /* PIPOUT   File UFB*/~
                            #26,                   /* JBCROSS2 File UFB*/~
                            #27,                   /* VBKMASTR File UFB*/~
                            #28,                   /* VBKLINES File UFB*/~
                            #29,                   /* JBCREDIT File UFB*/~
                            #30,                   /* RCVLINES File UFB*/~
                            #31,                   /* PAYMASTR File UFB*/~
                            #32,                   /* PAYLINES File UFB*/~
                            #33,                   /* HNYMASTR File UFB*/~
                            #34,                   /* WCMASTR  File UFB*/~
                            #35)                   /* RTEMASTR File UFB*/


        dim                                                              ~
            alloc$(101)1,                /* Allocation flag            */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            carrier$6,                   /* Carrier from shipping (BOL)*/~
            cartons$10,                  /* # of Cartons from BOL      */~
            checkso$16,                  /* Used in FIND_INVOICES      */~
            crflag$1,                    /* Credit Hold Flag           */~
            cursor%(2),                  /* Cursor Loacations For Edit */~
            cuscode$9,                   /* Customer Code              */~
            cusname$32,                  /* Customer Name              */~
            date$8,                      /* Date For Screen Display    */~
            dateasof$6,                  /* Date For ARQINVSB          */~
            demstring$19,                /* Demand Code and Line       */~
            descr$(101)32,               /* Part Descriptions          */~
            descrp$80,                   /* PLOWCODE Argument          */~
            descr_map(06),               /* PLOWCODE Argument          */~
            dscrn$(12,3)8,               /* Floating Screen Area       */~
            due$(101)8,                  /* Current Due Date For Line  */~
            factor(101),                 /* Unit Of Measeure Convertion*/~
            fld$(101)3,                  /* Line/Priority array        */~
            f_o_b$20,                    /* FOB from sales order       */~
            fld1$3,                      /* Line/Priority Display      */~
            fob$20,                      /* FOB from shipping (BOL)    */~
            frt_no$20,                   /* Freight/Air Bill # from BOL*/~
            hd$(3)79,                    /* Screen Header For PLOWCODE */~
            header$79,                   /* Screen Header              */~
            header$(3)80,                /* PLOWCODE Argument          */~
            hship$20,                    /* How Ship Info from SO      */~
            howship$20,                  /* How Ship Info from BOL     */~
            i$(24)80,                    /* Junk Screen Image (Not Used*/~
            incl_excl(5),                /* PLOWCODE Argument          */~
            incl_excl$(5)25,             /* PLOWCODE Argument          */~
            incus$9,                     /* Customer Code Passed In    */~
            invtext$(101)7,              /* Invoice Screen Memo        */~
            left(101),                   /* Left Quantity For Line     */~
            lfac$(12)1,                  /* Field Atribute Characters  */~
            line$(101)3,                 /* Line Numbers               */~
            mc_on$1,                     /* Is Multi-Currency Active?  */~
            message$79,                  /* Screen Info Message Line   */~
            opart$(101)25,               /* Part Numbers               */~
            ordered$8,                   /* Order Date                 */~
            ordrdate$6,                  /* Order Date                 */~
            ordr(101),                   /* Current Order Qty For Line */~
            org(101),                    /* Original Order Qty For Line*/~
            origdate$6,                  /* Order Date                 */~
            part$(12)34,                 /* Part Numbers               */~
            pfdescr$(3)79,               /* Text For Pfkeys Active     */~
            pfkeys$32,                   /* Pfkeys Active              */~
            plowkey$90,                  /* Work Variable              */~
            plowkey1$90,                 /* Work Variable              */~
            po$16,                       /* Customer PO Number         */~
            pri$(101)3,                  /* SHipping Priority          */~
            puom$(101)4,                 /* Pricing Unit Of Measure    */~
            bol$(101)3,                  /* So called BOL numbers      */~
            dates$(101,3)8,              /* Pertinant Dates            */~
            invoice$(101)8,              /* Packing List Numbers       */~
            shipcode$1,                  /* Shipping Priority Default  */~
            shp_msg$24,                  /* Schdld, shpd, or invoiced  */~
            shpqty(101,3),               /* Quantities Involved        */~
            shptitle$79,                 /* Screen Title               */~
            store$3,                     /* Store Items Shipped from   */~
            rlt$(101)3,                  /* 'Real' Lead Times          */~
            rscrn$(12)40,                /* Floating Screen Area       */~
            seq$(100)4,                  /* Screen Place Holder        */~
            slt(101),                    /* Standard Lead Times        */~
            ship(101),                   /* Total Quantity Delivered   */~
            so$16,                       /* P.O. Number                */~
            soline$3,                    /* P.O. Line Number           */~
            sono$16,                     /* P.O. Number Passed In      */~
            stat$4,                      /* Statutory Currency         */~
            status_inv$9,                /* Order Status - Invoicing   */~
            status_shp$9,                /* Order Status - Shipping    */~
            subheader$41,                /* Screen Header              */~
            suom$(101)4,                 /* Stocking Unit Of Measure   */~
            temp$50,                     /* Temperary Variable         */~
            text$(113,1)70,              /* Free Text Array            */~
            textid$4,                    /* Document Text              */~
            textid1$4,                   /* SO Line Text Id Number     */~
            textmsg$79,                  /* Document Text Dusplay Title*/~
            title$79,                    /* Screen Title               */~
            weight$10                    /* Shipment Weight            */

        dim f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            f2%(64),                     /* File open status flags     */~
            fs%(64),                     /* File open status flags     */~
            rslt$20

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *  1  ! BCKLINES ! Sales Order Master- Lines                *~
            *  2  ! SHPLINES ! Shipment Schedule- Lines                 *~
            *  3  ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            *  4  ! BCKMASTR ! Sales Order Master- Headers              *~
            *  5  ! ARIMASTR ! Invoice Master- Headers                  *~
            *  6  ! ARILINES ! Invoice Master- Line Items               *~
            *  7  ! CUSTOMER ! Customer Master File                     *~
            *  9  ! BCKHMSTR ! SO History Master - Headers              *~
            *  10 ! BCKHLNES ! SO History Lines - Detail                *~
            *  20 ! TXTFILE  ! System Text File                         *~
            *  21 ! PIPCROSS ! hard peg cross reference                 *~
            *  22 ! DEMMASTR ! Demand Master File                       *~
            *  23 ! JBMASTR2 ! Production job master file               *~
            *  24 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            *  25 ! PIPOUT   ! Planned inventory use detail rec  feeds  *~
            *  26 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            *  27 ! VBKMASTR ! Purchase Order Headers Master File       *~
            *  28 ! VBKLINES ! Purchase Orders Line Item Master file    *~
            *  29 ! JBCREDIT ! Production job credits received detail f *~
            *  30 ! RCVLINES ! Receiver Lines File                      *~
            *  31 ! PAYMASTR ! Payables header file                     *~
            *  32 ! PAYLINES ! Payables lines  file                     *~
            *  33 ! HNYMASTR ! Inventory Master File                    *~
            *  34 ! WCMASTR  ! Workcenter Master File                   *~
            *  35 ! RTEMASTR ! Routings Master File                     *~
            *  36 ! PIPIN    ! Expected Inventory Additions file        *~
            *  37 ! SYSFILE2 ! System File                              *~
            * #38 ! SERMASTR ! Serial Number Tracking Master File       *~
            *  43 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************


            select #36, "PIPIN",                                         ~
                        varc,     indexed,  recsize =    60,             ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #37, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #38, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #43, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            if already_opened% = 1% then L09000
                call "OPENCHCK" (#36, fs%(36%), f2%(36%), 0%, rslt$)
                call "OPENCHCK" (#37, fs%(37%), f2%(37%), 0%, rslt$)
                call "OPENCHCK" (#38, fs%(38%), f2%(38%), 0%, rslt$)
                if mode1% = 1% then L02400  /* Using #33 if MODE1% = 1% */
                                       /* else using #43 for HNYMASTR */
                     call "OPENCHCK" (#43, fs%(43%), f2%(43%), 0%, rslt$)
L02400:         already_opened% = 1%

*        Test for MC here rather than in 9000s to test just once
                mc_on$ = "N"
                call "READ100" (#37, "SWITCHS.CUR         ", f1%(37%))
                if f1%(37%) <> 0% then get #37 using L02460, mc_on$, stat$
L02460:             FMT POS(21), CH(1), CH(4)
                if mc_on$ <> "Y" then stat$ = " "

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date : dateasof$ = date$
            call "DATEFMT" (date$)
            cuscode$ = incus$ : so$ = sono$ : soline$ = sol$
            if mode1% = 1% then hny% = 33% else hny% = 43%
            if hflag% = 0% then L09088
                mc% = 9% : lc% = 10%
                  goto L09090
L09088:         mc% = 4% : lc% = 1%
L09090:     driver% = 1%
               hd$(3) = " "
              crflag$ = " "
            if seq$() <> " " then L09164

            for i% = 1 to 100
                convert i% to seq$(i%), pic(###)
                str(seq$(i%),4) = ")"
            next i%

L09164: REM Get default shipping priority code from CUSTOMER - if blank  ~
                set to "3"...
            shipcode$ = " "
            call "READ100" (#7, cuscode$, f1%(7%))
                if f1%(7%) = 0% then L09178               /* Just in case */
            get #7 using L09176, cusname$, shipcode$
L09176:         FMT POS(10), CH(30), POS(733), CH(1)
            call "PUTPAREN" (cusname$)
L09178:     if shipcode$ = " " then shipcode$ = "3"

            if sol$ = " " then L10000
               gosub L12000
               goto L65000

            return% = return%

L10000: REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * SO Summary Inquiry Screen...                              *~
            *************************************************************

            gosub load_so
L10070:     message$ = "Position Cursor And Press (ENTER) To See Shipment~
        ~ And Invoicing Details."
L10100:     lfac$() = all(hex(9c))
            pfdescr$(1) = "                   (8)See SO Text             ~
        ~                 (13)Instructions"
            pfdescr$(2) = "(2)First Lines   (4/6)Previous Lines          ~
        ~                 (15)Print Screen"
            pfdescr$(3) = "(3)Last Lines    (5/7)Next Lines              ~
        ~                 (16/32)Rtrn/Exit"
            pfkeys$ = hex(000102040506070dff0f10030809ff1920ff)
            if mode% = 2 then str(pfdescr$(1),41,22) = "(9)Stocking UOM"&~
                                                       "/Part"           ~
                         else str(pfdescr$(1),41,22) = "(9)Sales UOM" &  ~
                                                       "/Descrip"

            if mod2% = 2 then str(pfdescr$(3),40,22) = "(25)Line, Curr "&~
                                                       "Ord Qty"         ~
                         else str(pfdescr$(3),40,22) = "(25)Prty, Orig "&~
                                                       "Odr Qty"
            if mod2% = 2% then mat fld$ = pri$                           ~
                          else mat fld$ = line$

*        Check Appropriate Fields
            if base% > 0 then L10230
                str(pfdescr$(2),,39)     = " "  /* Shut Off Prev Stuff */
                str(pfkeys$,3,2), str(pfkeys$,6,1) = hex(ffff)
L10230:     if base% < maxlines%-12% then L10280
                str(pfdescr$(3),,39)     = " "  /* Shut Off Next Stuff */
                str(pfkeys$,5,1), str(pfkeys$,7,1) = hex(ff)
                str(pfkeys$,12,1) = hex(ff)

L10280:     gosub format_title
            if maxlines% = 1% then L10340
            for loop% = base% + 1% to min(maxlines%-1%, base%+12%)
             m% = loop% - base%
             gosub format_order_line
            next loop%
L10340:     if maxlines% <= base% + 12% then                             ~
               rscrn$(maxlines%-base%) = hex(8c) & "** End Of Order **"

L10370: accept                                                           ~
               at (01,02), "Sales Order Status Inquiry                   ~
        ~                   Today:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,03), "Customer PO#:",                              ~
               at (04,17), fac(hex(84)), po$                    , ch(16),~
               at (04,34), "-------Status-------",                       ~
               at (04,58), "Order Date:",                                ~
               at (04,70), fac(hex(84)), ordered$               , ch(08),~
                                                                         ~
               at (05,03), "How Ship:",                                  ~
               at (05,13), fac(hex(84)), hship$                 , ch(20),~
               at (05,34), "Invoicing:",                                 ~
               at (05,45), fac(hex(84)), status_inv$            , ch(09),~
               at (05,58), "Input xxxxxxxx By",                          ~
               at (05,64), fac(hex(8c)),indate$                 , ch(08),~
               at (05,76), fac(hex(8c)),inuser$                 , ch(03),~
                                                                         ~
               at (06,03), "FOB:",                                       ~
               at (06,08), fac(hex(84)), f_o_b$                 , ch(20),~
               at (06,34), "Shipping:",                                  ~
               at (06,45), fac(hex(84)), status_shp$            , ch(09),~
                                                                         ~
               at (06,77), "Expd",                                       ~
               at (07,40), fac(hex(8c)), subheader$             , ch(41),~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(lfac$(01)), fld$(base% + 01)     , ch(03),~
               at (10,02), fac(lfac$(02)), fld$(base% + 02)     , ch(03),~
               at (11,02), fac(lfac$(03)), fld$(base% + 03)     , ch(03),~
               at (12,02), fac(lfac$(04)), fld$(base% + 04)     , ch(03),~
               at (13,02), fac(lfac$(05)), fld$(base% + 05)     , ch(03),~
               at (14,02), fac(lfac$(06)), fld$(base% + 06)     , ch(03),~
               at (15,02), fac(lfac$(07)), fld$(base% + 07)     , ch(03),~
               at (16,02), fac(lfac$(08)), fld$(base% + 08)     , ch(03),~
               at (17,02), fac(lfac$(09)), fld$(base% + 09)     , ch(03),~
               at (18,02), fac(lfac$(10)), fld$(base% + 10)     , ch(03),~
               at (19,02), fac(lfac$(11)), fld$(base% + 11)     , ch(03),~
               at (20,02), fac(lfac$(12)), fld$(base% + 12)     , ch(03),~
                                                                         ~
               at (09,06), fac(hex(84)), part$(01)              , ch(34),~
               at (10,06), fac(hex(84)), part$(02)              , ch(34),~
               at (11,06), fac(hex(84)), part$(03)              , ch(34),~
               at (12,06), fac(hex(84)), part$(04)              , ch(34),~
               at (13,06), fac(hex(84)), part$(05)              , ch(34),~
               at (14,06), fac(hex(84)), part$(06)              , ch(34),~
               at (15,06), fac(hex(84)), part$(07)              , ch(34),~
               at (16,06), fac(hex(84)), part$(08)              , ch(34),~
               at (17,06), fac(hex(84)), part$(09)              , ch(34),~
               at (18,06), fac(hex(84)), part$(10)              , ch(34),~
               at (19,06), fac(hex(84)), part$(11)              , ch(34),~
               at (20,06), fac(hex(84)), part$(12)              , ch(34),~
                                                                         ~
               at (09,41), fac(hex(8c)), rscrn$(01)             , ch(40),~
               at (10,41), fac(hex(8c)), rscrn$(02)             , ch(40),~
               at (11,41), fac(hex(8c)), rscrn$(03)             , ch(40),~
               at (12,41), fac(hex(8c)), rscrn$(04)             , ch(40),~
               at (13,41), fac(hex(8c)), rscrn$(05)             , ch(40),~
               at (14,41), fac(hex(8c)), rscrn$(06)             , ch(40),~
               at (15,41), fac(hex(8c)), rscrn$(07)             , ch(40),~
               at (16,41), fac(hex(8c)), rscrn$(08)             , ch(40),~
               at (17,41), fac(hex(8c)), rscrn$(09)             , ch(40),~
               at (18,41), fac(hex(8c)), rscrn$(10)             , ch(40),~
               at (19,41), fac(hex(8c)), rscrn$(11)             , ch(40),~
               at (20,41), fac(hex(8c)), rscrn$(12)             , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)             ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

           if keyhit% <> 0 then L11170
              close crt
              call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
              cursor%(1) = cursor%(1) - 8%
              if cursor%(1) < 1% or cursor%(1) > 12% then L10370
              if lfac$(cursor%(1)) <> hex(8e) then L10370
              soline$ = line$(cursor%(1) + base%)
              gosub show_detail
              goto L10070

L11170:    if keyhit% <> 8% then L11220
                textmsg$ = "Free Text For Sales Order: " & so$
                call "TXTDSPLY" (#20, 0%, "013",textmsg$,textid$,text$())
                goto L10370

L11220:    if keyhit% <> 9% then L11250
                if mode% = 2% then mode% = 1% else mode% = 2%
                goto L10100

L11250:    if keyhit% <> 25% then L11260
                if mod2% = 2% then mod2% = 1% else mod2% = 2%
                goto L10100

L11260:    if keyhit% <> 13% then L11300
                call "MANUAL" ("SOSTATUS")
                goto L10370

L11300:    if keyhit% <> 15% then L11340
                call "PRNTSCRN"
                goto L10370

L11340:    if keyhit% = 16% then L65000
           if keyhit% = 32% then L65000
           if keyhit% = 2% then base% = 0%
           if keyhit% = 3% then base% = maxlines%-9%
           if keyhit% = 4% then base% = base%-9%
           if keyhit% = 5% then base% = base%+9%
           if keyhit% = 6% then base% = base%-1%
           if keyhit% = 7% then base% = base%+1%
           base%=max(0%,min(base%,maxlines%-12%))
           goto L10100

        format_title
            rscrn$(), part$() = " "
            title$ = "Cust: " & cuscode$ & cusname$ & " SO: " & so$
            str(title$,62) = "SOSTATUS: " & cms2v$
            header$ = "Ln  Part Number               Due Date UOM  Cur Or~
        ~der   Shipped Remaining  Days"
            subheader$ = "Stock ........ QUANTITIES .........  Lead"
            if mode% <> 2% then L11545
                str(subheader$,,5) = "Sales"
                str(header$,31,8) = " "
                str(header$,10,11) = "Description"
L11545:     if mod2% <> 2% then L11550
                str(header$,45,03) = "Org"
                str(header$,1%,3%) = "Pty"
L11550: return

        format_order_line
             tmp = ordr(loop%) : tmp1 = ship(loop%) : tmp2 = left(loop%)
             tmp3 = org(loop%)
             rscrn$(m%) = suom$(loop%)
             part$(m%) = str(opart$(loop%)) & hex(8c) & due$(loop%)
             if mode% <> 2% then L11670
                tmp = round(tmp/ factor(loop%), 4)
                tmp1 = round(tmp1/ factor(loop%), 4)
                tmp2 = round(tmp2/ factor(loop%), 4)
                tmp3 = round(tmp3/ factor(loop%), 4)
                rscrn$(m%) = puom$(loop%)
                part$(m%) = descr$(loop%)
L11670:      call "CONVERT" (tmp, 0.2, str(rscrn$(m%),6,09))
             if mod2% <> 2% then L11680
                call "CONVERT" (tmp3, 0.2, str(rscrn$(m%),6,09))
L11680:      call "CONVERT" (tmp1, 0.2, str(rscrn$(m%),16,09))
             str(rscrn$(m%),5,1), str(rscrn$(m%),25,1) = hex(84)
             call "CONVERT" (tmp2, 0.2, str(rscrn$(m%),26,9))
             if tmp2 = 0 then str(rscrn$(m%),25,1) = hex(8c)
             str(rscrn$(m%),15,1) = str(rscrn$(m%),25,1)
             str(rscrn$(m%),37,1) = hex(8c)
             call "CONVERT" (slt(loop%), 0.0, str(rscrn$(m%),38,3))
             lfac$(m%) = hex(8e)
        return

L12000: REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * SO Detail  Inquiry Screen...                              *~
            *************************************************************

        show_detail
            gosub load_detail
L12080:     message$ = "Position Cursor and Press (RETURN) to see BOL" & ~
                       "/Invoice Details."
L12100:     lfac$() = all(hex(9c))
            pfdescr$(1) = "                   (8)See Line Text    (10)Dem~
        ~and Status       (13)Instructions"
            pfdescr$(2) = "(2)First (4/6)Prev                     (11)Inv~
        ~oice Status      (15)Print Screen"
            pfdescr$(3) = "(3)Last  (5/7)Next (12)See Serial #s          ~
        ~                 (16/32)Rtrn/Exit"
            pfkeys$ = hex(00ff02040506070dff0f100308090a0b0c1920ff)
            if mode% = 2 then str(pfdescr$(2),20,18) = "(9)Stock UOM" &  ~
                                                       "/Part"           ~
                         else str(pfdescr$(2),20,18) = "(9)Sales UOM" &  ~
                                                       "/Descr"

            if mode1% = 0% or hflag%=1% then str(pfdescr$(1),40,17) = " "
            if mode1% = 0% or hflag% = 1% then str(pfkeys$,15,1) = " "
            if mod2% = 2 then str(pfdescr$(3),40,22) = "(25)Line, Curr "&~
                                                       "Ord Qty"         ~
                         else str(pfdescr$(3),40,22) = "(25)Prty, Orig "&~
                                                       "Ord Qty"
            if mod2% = 2 then fld1$ = pri$(101)                          ~
                         else fld1$ = line$(101)

*        Set Up Appropriate Fields
            if bas1% > 0 then L12230
                str(pfdescr$(2),,18)     = " "  /* Shut Off Prev Stuff */
                str(pfkeys$,3,2), str(pfkeys$,6,1) = hex(ffff)
L12230:     if bas1% < maxdetails%-9 then L12280
                str(pfdescr$(3),,18)     = " "  /* Shut Off Next Stuff */
                str(pfkeys$,5,1), str(pfkeys$,7,1) = hex(ff)
                str(pfkeys$,12,1) = hex(ff)

L12280:     gosub format_title
            m% = 1% : loop% = 101%
            gosub format_order_line
            temp$ = "N"
            call "READ100" (#hny%, part$(1%), f1%(hny%))
                if f1%(hny%) = 0% then L12312
            get #hny% using L12310, temp$
L12310:         FMT POS(131), CH(1)
L12312:     if temp$ = "Y" then L12320  /* Serial Numbered Part */
                str(pfdescr$(3%),20%,18%) = " "
                str(pfkeys$,17%,1%) = hex(ff)
L12320:     dscrn$() = " "
            shptitle$ = "Seq  BOL# Schd Date Schd Qty  Ship Date Ship Qty~
        ~  Inv Date  Invoice#       Actl"

            if maxdetails% = 1 then L12480
            for loop% = bas1% + 1 to min(maxdetails%-1, bas1%+9)
             c% = loop% - bas1%
               tmp = shpqty(loop%,1%)
               tmp1 = shpqty(loop%,2%)
               tmp2 = shpqty(loop%,3%)
               if mode% <> 2% then L12420
                tmp = round(tmp/ factor(101%), 4)
                tmp1 = round(tmp1/ factor(101%), 4)
                tmp2 = round(tmp2/ factor(101%), 4)
L12420:        call "CONVERT" (tmp, 0.2, str(dscrn$(c%,1%),,8))
               call "CONVERT" (tmp1, 0.2, str(dscrn$(c%,2%),,8))
               call "CONVERT" (tmp2, 0.2, str(dscrn$(c%,3%),,8))
             lfac$(c%) = hex(8e)
            next loop%
L12480:     if maxdetails% > bas1% + 9% then L12510
              dscrn$(maxdetails%-bas1%,1%) = hex(8c) & "* End *"

L12510: accept                                                           ~
               at (01,02), "Sales Order Status Inquiry - Line Shipment De~
        ~tails              Today:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,03), "Customer PO#:",                              ~
               at (04,17), fac(hex(84)), po$                    , ch(16),~
               at (04,34), "-------Status-------",                       ~
               at (04,58), "Order Date:",                                ~
               at (04,70), fac(hex(84)), ordered$               , ch(08),~
                                                                         ~
               at (05,03), "How Ship:",                                  ~
               at (05,13), fac(hex(84)), hship$                 , ch(20),~
               at (05,34), "Invoicing:",                                 ~
               at (05,45), fac(hex(84)), status_inv$            , ch(09),~
               at (05,58), "Input xxxxxxxx By",                          ~
               at (05,64), fac(hex(8c)),indate$                 , ch(08),~
               at (05,76), fac(hex(8c)),inuser$                 , ch(03),~
                                                                         ~
               at (06,03), "FOB:",                                       ~
               at (06,08), fac(hex(84)), f_o_b$                 , ch(20),~
               at (06,34), "Shipping:",                                  ~
               at (06,45), fac(hex(84)), status_shp$            , ch(09),~
                                                                         ~
               at (06,77), "Expd",                                       ~
               at (07,40), fac(hex(8c)), subheader$             , ch(41),~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(hex(8c)), fld1$                  , ch(03),~
               at (09,06), fac(hex(84)), part$(1%)              , ch(34),~
               at (09,41), fac(hex(8c)), rscrn$(1%)             , ch(40),~
                                                                         ~
               at (11,02), fac(hex(ac)), shptitle$              , ch(79),~
                                                                         ~
               at (12,02), fac(lfac$(01)), seq$(bas1% + 1%)     , ch(04),~
               at (13,02), fac(lfac$(02)), seq$(bas1% + 2%)     , ch(04),~
               at (14,02), fac(lfac$(03)), seq$(bas1% + 3%)     , ch(04),~
               at (15,02), fac(lfac$(04)), seq$(bas1% + 4%)     , ch(04),~
               at (16,02), fac(lfac$(05)), seq$(bas1% + 5%)     , ch(04),~
               at (17,02), fac(lfac$(06)), seq$(bas1% + 6%)     , ch(04),~
               at (18,02), fac(lfac$(07)), seq$(bas1% + 7%)     , ch(04),~
               at (19,02), fac(lfac$(08)), seq$(bas1% + 8%)     , ch(04),~
               at (20,02), fac(lfac$(09)), seq$(bas1% + 9%)     , ch(04),~
                                                                         ~
               at (12,07), fac(hex(84)), bol$(bas1% + 1%)       , ch(03),~
               at (13,07), fac(hex(84)), bol$(bas1% + 2%)       , ch(03),~
               at (14,07), fac(hex(84)), bol$(bas1% + 3%)       , ch(03),~
               at (15,07), fac(hex(84)), bol$(bas1% + 4%)       , ch(03),~
               at (16,07), fac(hex(84)), bol$(bas1% + 5%)       , ch(03),~
               at (17,07), fac(hex(84)), bol$(bas1% + 6%)       , ch(03),~
               at (18,07), fac(hex(84)), bol$(bas1% + 7%)       , ch(03),~
               at (19,07), fac(hex(84)), bol$(bas1% + 8%)       , ch(03),~
               at (20,07), fac(hex(84)), bol$(bas1% + 9%)       , ch(03),~
                                                                         ~
               at (12,13), fac(hex(8c)), dates$(bas1%+1%,1%)    , ch(08),~
               at (13,13), fac(hex(8c)), dates$(bas1%+2%,1%)    , ch(08),~
               at (14,13), fac(hex(8c)), dates$(bas1%+3%,1%)    , ch(08),~
               at (15,13), fac(hex(8c)), dates$(bas1%+4%,1%)    , ch(08),~
               at (16,13), fac(hex(8c)), dates$(bas1%+5%,1%)    , ch(08),~
               at (17,13), fac(hex(8c)), dates$(bas1%+6%,1%)    , ch(08),~
               at (18,13), fac(hex(8c)), dates$(bas1%+7%,1%)    , ch(08),~
               at (19,13), fac(hex(8c)), dates$(bas1%+8%,1%)    , ch(08),~
               at (20,13), fac(hex(8c)), dates$(bas1%+9%,1%)    , ch(08),~
                                                                         ~
               at (12,33), fac(hex(8c)), dates$(bas1%+1%,2%)    , ch(08),~
               at (13,33), fac(hex(8c)), dates$(bas1%+2%,2%)    , ch(08),~
               at (14,33), fac(hex(8c)), dates$(bas1%+3%,2%)    , ch(08),~
               at (15,33), fac(hex(8c)), dates$(bas1%+4%,2%)    , ch(08),~
               at (16,33), fac(hex(8c)), dates$(bas1%+5%,2%)    , ch(08),~
               at (17,33), fac(hex(8c)), dates$(bas1%+6%,2%)    , ch(08),~
               at (18,33), fac(hex(8c)), dates$(bas1%+7%,2%)    , ch(08),~
               at (19,33), fac(hex(8c)), dates$(bas1%+8%,2%)    , ch(08),~
               at (20,33), fac(hex(8c)), dates$(bas1%+9%,2%)    , ch(08),~
                                                                         ~
               at (12,52), fac(hex(8c)), dates$(bas1%+1%,3%)    , ch(08),~
               at (13,52), fac(hex(8c)), dates$(bas1%+2%,3%)    , ch(08),~
               at (14,52), fac(hex(8c)), dates$(bas1%+3%,3%)    , ch(08),~
               at (15,52), fac(hex(8c)), dates$(bas1%+4%,3%)    , ch(08),~
               at (16,52), fac(hex(8c)), dates$(bas1%+5%,3%)    , ch(08),~
               at (17,52), fac(hex(8c)), dates$(bas1%+6%,3%)    , ch(08),~
               at (18,52), fac(hex(8c)), dates$(bas1%+7%,3%)    , ch(08),~
               at (19,52), fac(hex(8c)), dates$(bas1%+8%,3%)    , ch(08),~
               at (20,52), fac(hex(8c)), dates$(bas1%+9%,3%)    , ch(08),~
                                                                         ~
               at (12,22), fac(hex(84)), dscrn$(1%,1%)          , ch(08),~
               at (13,22), fac(hex(84)), dscrn$(2%,1%)          , ch(08),~
               at (14,22), fac(hex(84)), dscrn$(3%,1%)          , ch(08),~
               at (15,22), fac(hex(84)), dscrn$(4%,1%)          , ch(08),~
               at (16,22), fac(hex(84)), dscrn$(5%,1%)          , ch(08),~
               at (17,22), fac(hex(84)), dscrn$(6%,1%)          , ch(08),~
               at (18,22), fac(hex(84)), dscrn$(7%,1%)          , ch(08),~
               at (19,22), fac(hex(84)), dscrn$(8%,1%)          , ch(08),~
               at (20,22), fac(hex(84)), dscrn$(9%,1%)          , ch(08),~
                                                                         ~
               at (12,42), fac(hex(84)), dscrn$(1%,2%)          , ch(08),~
               at (13,42), fac(hex(84)), dscrn$(2%,2%)          , ch(08),~
               at (14,42), fac(hex(84)), dscrn$(3%,2%)          , ch(08),~
               at (15,42), fac(hex(84)), dscrn$(4%,2%)          , ch(08),~
               at (16,42), fac(hex(84)), dscrn$(5%,2%)          , ch(08),~
               at (17,42), fac(hex(84)), dscrn$(6%,2%)          , ch(08),~
               at (18,42), fac(hex(84)), dscrn$(7%,2%)          , ch(08),~
               at (19,42), fac(hex(84)), dscrn$(8%,2%)          , ch(08),~
               at (20,42), fac(hex(84)), dscrn$(9%,2%)          , ch(08),~
                                                                         ~
               at (12,62), fac(hex(8c)), invoice$(bas1%+1%)     , ch(08),~
               at (13,62), fac(hex(8c)), invoice$(bas1%+2%)     , ch(08),~
               at (14,62), fac(hex(8c)), invoice$(bas1%+3%)     , ch(08),~
               at (15,62), fac(hex(8c)), invoice$(bas1%+4%)     , ch(08),~
               at (16,62), fac(hex(8c)), invoice$(bas1%+5%)     , ch(08),~
               at (17,62), fac(hex(8c)), invoice$(bas1%+6%)     , ch(08),~
               at (18,62), fac(hex(8c)), invoice$(bas1%+7%)     , ch(08),~
               at (19,62), fac(hex(8c)), invoice$(bas1%+8%)     , ch(08),~
               at (20,62), fac(hex(8c)), invoice$(bas1%+9%)     , ch(08),~
                                                                         ~
               at (12,78), fac(hex(8c)), rlt$(bas1%+1%)         , ch(03),~
               at (13,78), fac(hex(8c)), rlt$(bas1%+2%)         , ch(03),~
               at (14,78), fac(hex(8c)), rlt$(bas1%+3%)         , ch(03),~
               at (15,78), fac(hex(8c)), rlt$(bas1%+4%)         , ch(03),~
               at (16,78), fac(hex(8c)), rlt$(bas1%+5%)         , ch(03),~
               at (17,78), fac(hex(8c)), rlt$(bas1%+6%)         , ch(03),~
               at (18,78), fac(hex(8c)), rlt$(bas1%+7%)         , ch(03),~
               at (19,78), fac(hex(8c)), rlt$(bas1%+8%)         , ch(03),~
               at (20,78), fac(hex(8c)), rlt$(bas1%+9%)         , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)            , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)
               message$ = " "

         if keyhit% <> 0% and keyhit% <> 11% and keyhit% <> 12% then L13580
              close crt
              call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
              cursor%(1%) = cursor%(1%) - 11%
              if cursor%(1%) < 1% or cursor%(1%) > 9% then L12080
              if lfac$(cursor%(1%)) <> hex(8e) then L12080
              cur% = cursor%(1%)
              bol% = (cursor%(1%) + bas1%)
              if cur% > c% then L12080
              if dscrn$(1%,1%) = hex(8c) & "* End *" then L12080
              if keyhit% = 11% then L13564
              if keyhit% = 12% then see_serial_numbers
              gosub show_bol_detail
              goto L12080
L13564:         if invoice$(bol%) = " " then L12080
                     call "ARQINVSB" (cuscode$, dateasof$, #7, #5,       ~
                                      #6, 1%, invoice$(bol%), cuscode$,  ~
                                      mc_on$, stat$)
                     goto L12080

L13580:    if keyhit% <> 8% then L13630
                textmsg$ = "Free Text For Sales Order: " & so$
                textmsg$ = textmsg$ & "  Line: " & line$(101)
                call "TXTDSPLY" (#20, 0%,"014",textmsg$,textid1$,text$())
                goto L13920

L13630:    if keyhit% <> 9% then L13660
                if mode% = 2% then mode% = 1% else mode% = 2%
                goto L12080

L13660:    if keyhit% <> 25% then L13670
                if mod2% = 2% then mod2% = 1% else mod2% = 2%
                goto L12080

L13670:    if keyhit% <> 10% then L13730  /* PF10 not enabled if MODE1% */
              gosub test_dem                             /* equals 0%. */
                  if message$ <> " " then L12510
              str(demstring$,1,16) = str(so$,1,16)
              str(demstring$,17,3) = str(soline$,1,3)
                call "DEMSTAT" (demstring$, #21, #22, #23, #24, #25, #26,~
                                            #27, #28, #29, #30, #31, #32,~
                              #hny%, #34, #35, #20, #36, return%, driver%)
                goto L12080

L13730:    if keyhit% <> 13% then L13770
                call "MANUAL" ("SOSTATUS")
                goto L13920

L13770:    if keyhit% <> 15% then L13810
                call "PRNTSCRN"
                goto L13920

L13810:    if keyhit% = 16% then return
           if keyhit% = 32% then L65000
           if keyhit% = 2% then bas1% = 0%
           if keyhit% = 3% then bas1% = maxdetails%-9%
           if keyhit% = 4% then bas1% = bas1%-7%
           if keyhit% = 5% then bas1% = bas1%+7%
           if keyhit% = 6% then bas1% = bas1%-1%
           if keyhit% = 7% then bas1% = bas1%+1%
           bas1%=max(0%,min(bas1%,maxdetails%-9%))
           goto L12100

        see_serial_numbers
           if invoice$(bol%) = " " then L13870
            mat incl_excl = zer  : incl_excl$() = " "
            header$ (1%) = " Serial Number"
            temp$ = str(part$(1%),,25%)
            header$ (3%) = temp$ & " Shipped on Invoice " &              ~
                           invoice$(bol%) & " (Perhaps more than 1 Line)"
            brk% = 25% : kee = 0.00 : dlen = 00
            incl_excl(1%) = 01.01 : incl_excl$(1%) = "4"
            incl_excl(2%) = 02.09 : incl_excl$(2%) = cuscode$
            incl_excl(3%) = 11.08 : incl_excl$(3%) = invoice$(bol%)
            plowkey$ = str(part$(1%),,25%)
            descrp$= hex(06) & "When Done with View, Press PF16 to Return"
            call "PLOWCODE" (#38, plowkey$, descrp$, 9000%+brk%, kee,    ~
                             f1%(38%), header$(), dlen, 0, incl_excl(),  ~
                             incl_excl$(), "Y", " ", #38, descr_map())
           goto L13920
L13870:    if dates$(bol%,2%) = " " or dates$(bol%,2%) = blankdate$ then L13920
            mat incl_excl = zer  : incl_excl$() = " "
            header$ (1%) = " Serial Number"
            temp$ = str(part$(1%),,25%)
            header$ (3%) = temp$ & " Shipped on BOL " & bol$(bol%) &     ~
                                       " (Perhaps more than 1 Line Item)"
            brk% = 25% : kee = 0.00 : dlen = 00
            incl_excl(1%) = 01.01 : incl_excl$(1%) = "t"
            incl_excl(2%) =218.16 : incl_excl$(2%) = sono$
            incl_excl(3%) =234.03 : incl_excl$(3%) = bol$(bol%)
            plowkey$ = str(part$(1%),,25%)
            descrp$= hex(06) & "When Done with View, Press PF16 to Return"
            call "PLOWCODE" (#38, plowkey$, descrp$, 9000%+brk%, kee,    ~
                             f1%(38%), header$(), dlen, 0, incl_excl(),  ~
                             incl_excl$(), "Y", " ", #38, descr_map())
           goto L13920

L13920:    message$ = "Position Cursor and Press (RETURN) to see BOL" &  ~
                      " Details."
           goto L12510

        REM *************************************************************~
            *      B O L   -   D I S P L A Y   I T                      *~
            *                                                           *~
            * BOL Detail Inquiry Screen...                              *~
            *************************************************************

        show_bol_detail
            gosub load_bol_detail
            message$ = " "
            pfdescr$(1) = "                                              ~
        ~                 (13)Instructions"
            pfdescr$(2) = "                           (11)Invoice Status ~
        ~                 (15)Print Screen"
            pfdescr$(3) = "                                              ~
        ~                 (16/32)Rtrn/Exit"
            pfkeys$ = hex(00ffffffff0bff0dff0f1020ff)
            if invoice$(bol%) <> " " then L14580
                str(pfdescr$(2%), 28%, 18%) = " "
                str(pfkeys$, 6%, 1%) = hex(ff)

L14580: accept                                                           ~
               at (01,02), "Sales Order Status Inquiry - BOL/Invoice Head~
        ~er Details         Today:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,03), "Customer PO#:",                              ~
               at (04,17), fac(hex(84)), po$                    , ch(16),~
               at (04,34), "-------Status-------",                       ~
               at (04,58), "Order Date:",                                ~
               at (04,70), fac(hex(84)), ordered$               , ch(08),~
                                                                         ~
               at (05,03), "How Ship:",                                  ~
               at (05,13), fac(hex(84)), hship$                 , ch(20),~
               at (05,34), "Invoicing:",                                 ~
               at (05,45), fac(hex(84)), status_inv$            , ch(09),~
               at (05,58), "Input xxxxxxxx By",                          ~
               at (05,64), fac(hex(8c)),indate$                 , ch(08),~
               at (05,76), fac(hex(8c)),inuser$                 , ch(03),~
                                                                         ~
               at (06,03), "FOB:",                                       ~
               at (06,08), fac(hex(84)), f_o_b$                 , ch(20),~
               at (06,34), "Shipping:",                                  ~
               at (06,45), fac(hex(84)), status_shp$            , ch(09),~
                                                                         ~
               at (06,77), "Expd",                                       ~
               at (07,40), fac(hex(8c)), subheader$             , ch(41),~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(hex(8c)), line$(101%)            , ch(03),~
               at (09,06), fac(hex(84)), part$(1%)              , ch(34),~
               at (09,41), fac(hex(8c)), rscrn$(1%)             , ch(40),~
                                                                         ~
               at (11,02), fac(hex(ac)), shptitle$              , ch(79),~
                                                                         ~
               at (12,02), fac(hex(8c)),   seq$(bol%)           , ch(04),~
               at (12,07), fac(hex(84)),   bol$(bol%)           , ch(03),~
               at (12,13), fac(hex(8c)),   dates$(bol%, 1%)     , ch(08),~
               at (12,33), fac(hex(8c)),   dates$(bol%, 2%)     , ch(08),~
               at (12,52), fac(hex(8c)),   dates$(bol%, 3%)     , ch(08),~
               at (12,22), fac(hex(84)),   dscrn$(cur%, 1%)     , ch(08),~
               at (12,42), fac(hex(84)),   dscrn$(cur%, 2%)     , ch(08),~
               at (12,62), fac(hex(8c)),   invoice$(bol%)       , ch(08),~
               at (12,78), fac(hex(8c)),   rlt$(bol%)           , ch(03),~
                                                                         ~
               at (14,03), fac(hex(a4)),   shp_msg$             , ch(60),~
                                                                         ~
               at (15,10), "Carrier:",                                   ~
               at (15,28), fac(hex(84)),   carrier$             , ch(06),~
                                                                         ~
               at (16,10), "How Shipped:",                               ~
               at (16,28), fac(hex(84)),   howship$             , ch(20),~
                                                                         ~
               at (17,10), "FOB:",                                       ~
               at (17,28), fac(hex(84)),   fob$                 , ch(20),~
                                                                         ~
               at (18,10), "Freight/Air Bill:",                          ~
               at (18,28), fac(hex(84)),   frt_no$              , ch(20),~
                                                                         ~
               at (19,10), "# of Cartons:    ",                          ~
               at (19,28), fac(hex(84)),   cartons$             , ch(10),~
               at (20,10), "Weight:",                                    ~
               at (20,28), fac(hex(84)),   weight$              , ch(10),~
                                                                         ~
               at (15,51), "Store:",                                     ~
               at (15,59), fac(hex(84)),   store$               , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)            , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

           if keyhit% <> 11% then L16180
                call "ARQINVSB" (cuscode$, dateasof$, #7, #5, #6, 1%,    ~
                                 invoice$(bol%), cuscode$, mc_on$, stat$)
                goto L14580

L16180:    if keyhit% <> 13% then L16220
                call "MANUAL" ("SOSTATUS")
                goto L14580

L16220:    if keyhit% <> 15% then L16260
                call "PRNTSCRN"
                goto L14580

L16260:    if keyhit% = 16% then return
           if keyhit% = 32% then L65000
           goto L14580

        REM *************************************************************~
            *           L O A D   U P   S O   D A T A                   *~
            *                                                           *~
            * Get data, then show it...                                 *~
            *************************************************************

        load_so
*          IF SO$ = LASTSO$ AND LASTSO$ <> " " THEN RETURN
            line$(), opart$(), descr$(), due$(), lastsoline$ = " "
            suom$(), puom$(), alloc$(), fld$(), pri$() = " "
            maxlines%, base% = 0
            mat ordr = zer               /* Current Order Quantity     */
            mat ship = zer               /* TOTAL RECEIVED             */
            mat left = zer               /* STILL ON ORDER             */
            mat slt  = zer               /* Standard Lead Times        */
            mat factor = zer             /* Vendor Convertion Factor   */

            hd$(1) = hex(0684) & "Select Customer, or PF16 To Return"
            call "GETCODE" (#7, cuscode$, hd$(1), 0%, 1.30, f1%(7))
                if f1%(7) = 0 then L65000
            str(plowkey$,,9) = cuscode$
            str(plowkey$,10,16) = so$
            hd$(1) = "  Order Number       Ship To Name"
            hd$(3) = hex(84) & "Position cursor at line and press (RETURN~
        ~) to display order, PF16 To Return"
            call "PLOWCODE" (#mc%,plowkey$," ",3009%,.30,f1%(mc%),       ~
                            hd$(),0,42)
                if f1%(mc%) = 0 then L65000
            cuscode$ = str(plowkey$,,9)
            lastso$, so$ = str(plowkey$,10,16)

            call "SHOSTAT" ("Loading Sales Order For Display")
            gosub load_order_header_data

            plowkey$ = so$
            str(plowkey$,17) = all(hex(00))
                shp_openflag%, inv_openflag% = 0%
L30340:     call "PLOWNEXT" (#lc%, plowkey$, 16%, f1%(lc%))
                if f1%(lc%) = 0 then end_of_load
            maxlines%, m%  = maxlines% + 1%
            gosub load_order_line_data
            goto L30340

        end_of_load
            maxlines% = maxlines% + 1  /* Slot For 'End' Message */
          status_set
            if inv_openflag% = 0% then status_inv$ = "CLOSED"            ~
                                  else status_inv$ = "OPEN"
            if crflag$ = "H" then status_inv$ = "CR HOLD"
            if crflag$ = "C" then status_inv$ = "CANCELLED"
            if shp_openflag% = 0% then status_shp$ = "CLOSED"            ~
                                  else status_shp$ = "OPEN"
            if crflag$ = "H" then status_shp$ = "CR HOLD"
            if crflag$ = "C" then status_shp$ = "CANCELLED"
        return

        load_order_header_data
            get #mc%, using L30470, po$, hship$, f_o_b$, textid$,         ~
                                 ordered$, indate$, inuser$, crflag$
L30470:     FMT POS(26), CH(16), POS(422), 2*CH(20), POS(799), CH(4),    ~
                POS(806), CH(6), POS(830), CH(6), CH(3), POS(875), CH(1)
            ordrdate$ = ordered$
            call "DATEFMT" (ordered$)
            call "DATEFMT" (indate$)
        return

        check_for_open_lines
            if sol$ = " " then return /* No need, status set @ LOAD_SO */
                shp_openflag%, inv_openflag% = 0%
                plowkey$ = so$
                str(plowkey$,17) = all(hex(00))
L30516:         call "PLOWNEXT" (#lc%, plowkey$, 16%, f1%(lc%))
                if f1%(lc%) = 0% then L30528
                     get #lc%, using L30522, left, pre
L30522:                   FMT POS(109), PD(14,4), POS(133), PD(14,4)
                     if left <> 0 then inv_openflag% = 1%
                     if left - pre > 0 then shp_openflag% = 1%
                     goto L30516
L30528:         gosub status_set
                return

        load_order_line_data
            get #lc%, using L30680, line$(m%), opart$(m%), descr$(m%),    ~
                          org(m%), ship(m%), left(m%), pre, suom$(m%),   ~
                          puom$(m%), factor(m%), origdate$, due$(m%),    ~
                          textid1$, alloc$(m%), pri$(m%)
            if pri$(m%) = " " then pri$(m%) = shipcode$
              if left(m%) <> 0 then inv_openflag% = 1%
            ordr(m%) = left(m%) + ship(m%) /* Open+Invc'd=Current Order */
            ship(m%) = ship(m%) + pre  /* Shipped but not invoiced */
            left(m%) = left(m%) - pre
              if left(m%) > 0 then shp_openflag% = 1%
            call "DATE" addr("G-", ordrdate$, origdate$, days%, ret%)
            if ret% <> 0% then days% = 0%
            slt(m%) = days%
            if descr$(m%) = " " then descr$(m%) = opart$(m%)
            call "DATEFMT" (due$(m%))
        return

L30680:         FMT                      /* BCKLINES data file         */~
                POS( 26), CH(3),         /* Sequence Number            */~
                POS( 32), CH(25),        /* Part Number                */~
                          CH(32),        /* Part description           */~
                POS( 93), PD(14,4),      /* Order Quantity             */~
                          PD(14,4),      /* Quantity Shipped           */~
                          PD(14,4),      /* Open Quantity              */~
                  /*      PD(14,4),      /* Scheduled Quantity         */~
                  /*      PD(14,4),      /* Allocated Quantity         */~
                POS(133), PD(14,4),      /* Preinvoiced Qty            */~
                  /*      PD(14,4),      /* Unit Price- at Stocking UOM*/~
                POS(149), CH(4),          /* Stocking UOM              */~
                          CH(4),          /* Pricing Unit of Measure   */~
                          PD(14,7),       /* Conversion  (buy to sell) */~
                POS(200), CH(6),          /* Original Due Date         */~
                          CH(6),          /* Current Due Date          */~
                  /*      CH(6),          /* Ship Date                 */~
                POS(242), CH(4),          /* Internal ID to text       */~
                          CH(1),          /* Allocation Flag           */~
                POS(277), CH(1)           /* Shipping Priority Code    */
        REM *************************************************************~
            *           L O A D   U P   D E T A I L S                   *~
            *                                                           *~
            * Get data, then show it...                                 *~
            *************************************************************

        load_detail
            if so$ <> lastso$ or lastso$ = " " then L31110
               if soline$ = lastsoline$ and lastsoline$<> " " then return


L31110:     bol$(), dates$(), invoice$(), rlt$(), invtext$() = " "
            mat shpqty = zer                 /* Receiver Quantities*/
            maxdetails%, bas1% = 0

            str(plowkey$,,9) = cuscode$
            str(plowkey$,10,16) = so$
            call "PLOWCODE" (#mc%, plowkey$, " ", 0%, 0.30, f1%(mc%))
                if f1%(mc%) = 0 then L65000
            cuscode$ = str(plowkey$,,9)
            lastso$, so$ = str(plowkey$,10,16)
            gosub load_order_header_data
            gosub check_for_open_lines  /* and set status */

            plowkey$ = so$
            str(plowkey$,17) = soline$
            call "PLOWCODE" (#lc%, plowkey$, " ", 16%, 0.00, f1%(lc%))
                if f1%(lc%) = 0 then L10000
            soline$, lastsoline$ = str(plowkey$,17)
            m% = 101% : gosub load_order_line_data

            REM Look for SHPHDRS record...
            plowkey$ = cuscode$
            str(plowkey$,10,16) = so$
L31330:     call "PLOWNEXT" (#3, plowkey$, 25%, f1%(3))
                if f1%(3) = 0 then find_invoices
            maxdetails%, c% = maxdetails% + 1%
            get #3, using L31630, bol$(c%), dates$(c%,1%), dates$(c%,2%), ~
                                 invoice$(c%)
            if dates$(c%,2%) <> " " and dates$(c%,2%) <> blankdate$ then ~
                    dates$(c%,1%) = " "

            REM Go after ship lines...
            plowkey1$ = str(plowkey$,10)
            str(plowkey1$,20) = soline$
            call "READ100" (#2, plowkey1$, f1%(2))
                if f1%(2) = 0 then L31460
            get #2, using L31440, quan
L31440:     FMT POS(32), PD(14,4)
            if quan <> 0 then L31500
L31460:         bol$(c%), dates$(c%,1%), dates$(c%,2%), invoice$(c%) = " "
                maxdetails%, c% = maxdetails% - 1%
                goto L31330 /* Bug out */

L31500:     if invoice$(c%) = " " then L31600
            REM Retrieve Invoice Date From Invoice Master File...
            dates$(c%,3%) = "??????"
            plowkey1$ = cuscode$
            str(plowkey1$,10) = invoice$(c%)
            call "READ100" (#5, plowkey1$, f1%(5))
                if f1%(5) = 0 then L31600
            get #5, using L31580, dates$(c%,2%), dates$(c%,3%)
L31580:     FMT POS(413), CH(6), POS(521), CH(6)

L31600:     gosub format_detail_line
            goto L31330

L31630:     FMT                          /* RECEIVER LINE ITEM         */~
                POS( 26), CH(03),        /* BOL Number                 */~
                POS( 32), CH(6),         /* Scheduled Ship date        */~
                POS(184), CH(6),         /* Actual Ship date           */~
                POS(234), CH(9)          /* Invoice Number             */

        find_invoices
            str(plowkey$,,16) = str(so$,,16)
            call "REDALT0" (#5, plowkey$, 3%, f1%(5))
L31730:         if f1%(5) = 0 then end_of_load_two
            get #5, using L31750, plowkey1$, checkso$, inv_bol$,          ~
                                 shp_date$, inv_date$
L31750:     FMT  POS(1), CH(17), POS(34), CH(16), POS(50), CH(3),        ~
                          POS(413), CH(6), POS(521), CH(6)
                if checkso$ <> str(so$,1,16) then end_of_load_two
            goto L31800

L31770:     call "READNEXT" (#5,f1%(5))
            goto L31730

L31800:     str(plowkey1$,18) = hex(00)
L31810:     call "PLOWNEXT" (#6, plowkey1$, 17%, f1%(6))
                if f1%(6) = 0 then L31770
            get #6, using L31840, quan, inv_so_line$
L31840:     FMT POS(93), PD(14,4), POS(194), CH(3)
            if quan = 0 then L31810
            if inv_so_line$ <> soline$ then L31810

            REM Ok, Got one...
            maxdetails%, c% = maxdetails% + 1%
            dates$(c%,3%) = inv_date$
               if shp_date$ = " " or shp_date$ = blankdate$ then ~
                    shp_date$ = inv_date$
            dates$(c%,2%) = shp_date$
            invoice$(c%) = str(plowkey1$,10,8)
            gosub format_detail_line
            dates$(c%,1%) = " "
            bol$(c%) = inv_bol$
            goto L31810

        format_detail_line
            if invoice$(c%) = " " then L32040
                shpqty(c%,2) = quan   /* Invoiced */
                goto L32090
L32040:     if dates$(c%,2%) = " " or dates$(c%,2%) = blankdate$ then L32070
                shpqty(c%,2) = quan   /* Pre-Invoiced Only */
                goto L32090
L32070:     shpqty(c%,1) = quan       /* Scheduled Only */

L32090:     test$ = dates$(c%,1)
            if dates$(c%,2) <> " " and dates$(c%,2%) <> blankdate$ then ~
                       test$ = dates$(c%,2)
            call "DATE"addr("G-", ordrdate$, str(test$,,6), days%, ret%)
            if ret% <> 0 then days% = 0
            temp = days%
            call "CONVERT" (temp, 0.0, rlt$(c%))
            call "DATEFMT" (dates$(c%,1))
            call "DATEFMT" (dates$(c%,2))
            call "DATEFMT" (dates$(c%,3))
        return

        end_of_load_two
            maxdetails% = maxdetails% + 1% /* Slot For 'End' Message */
        return

        REM *************************************************************~
            *         L O A D   U P   B O L   D E T A I L S             *~
            *                                                           *~
            * Get data, then show it...                                 *~
            *************************************************************

        load_bol_detail
            init (" ") carrier$, howship$, fob$, frt_no$, cartons$,      ~
                       shp_msg$, weight$, store$
            if invoice$(bol%) <> " " then get_from_invoice else          ~
                                          get_from_bol

        get_from_invoice
            plowkey$ = str(cuscode$) & str(invoice$(bol%))
            call "READ100" (#5, plowkey$, f1%(5))
            if f1%(5) <> 1% then get_from_bol
                get #5 using L36920, howship$, fob$, carrier$, cartons,   ~
                                    weight, frt_no$, store$
L36920:              FMT POS(419), CH(20), CH(20), CH(6), 2*PD(14,4),    ~
                         POS(481), CH(20),POS(870), CH(3)
                goto end_bol_detail

        get_from_bol
            if bol$(bol%) = " " then end_bol_detail
            plowkey$ = str(cuscode$) & str(so$) & bol$(bol%)
            call "READ100" (#3, plowkey$, f1%(3))
            if f1%(3) <> 1% then end_bol_detail
                get #3 using L37020, store$, carrier$, howship$, fob$,    ~
                                    frt_no$, cartons, weight
L37020:              FMT POS(29), CH(3), POS(38), CH(6), CH(20), CH(20), ~
                         POS(190), CH(20), 2*PD(14,4)

        end_bol_detail
            if cartons <> 0 then call "CONVERT" (cartons, 2.2, cartons$)
            if weight  <> 0 then call "CONVERT" (weight , 2.2, weight$)
            shp_msg$ = "Scheduled"
            if dates$(bol%, 1%) = " " or dates$(bol%, 1%) = blankdate$ ~
                    then shp_msg$ = "Shipped but NOT Invoiced"
            if invoice$(bol%)  <> " " then shp_msg$ = "Shipped and"  &   ~
                                                      " Invoiced"
            call "STRING" addr ("CT", shp_msg$, 24%)
            return

        REM *************************************************************~
            *                T E S T      D A T A                       *~
            *************************************************************

        test_dem
            REM 1st test for non-stocked part
                if mode1% = 0% then return
              str(plowkey$,,25) = str(part$(1%),,25)
            call "READ100" (#hny%, plowkey$, f1%(hny%))
                if f1%(hny%) <> 0% then L50200
            message$ = "Non-Stocked Parts have NO demand.  Pick another o~
        ~r return."
            return

L50200:     REM 2nd test fot 'C' type allocation
                if alloc$(loop%) <> "C" then return
            message$ = "This Part has been COMPLETELY Allocated, it has N~
        ~O demand. Pick another."
            return

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY LEFT, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
