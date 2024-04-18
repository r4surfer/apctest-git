        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  RRRR    CCC   V   V  SSS  TTTTT    A    TTTTT     SSS    *~
            *  R   R  C      V   V S       T     A A     T      S       *~
            *  RRRR   C      V   V  SSS    T     AAA     T       SSS    *~
            *  R  R   C       V V      S   T    A   A    T          S   *~
            *  R   R   CCC     V    SSS    T    A   A    T       SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVSTATS - Displays relevent Receiver Data For Inquiry.   *~
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
            * 08/13/93 ! ORIGINAL (Clone of POSTATUS)             ! JDH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "RCVSTATS" (rcvr$,                 /* Receiver Number  */~
                            vendor$,               /* Vendor Number    */~
                            po$,                   /* Purchase Order # */~
                            pol$,                  /* Blnk for any line*/~
                            #01,                   /* RCVMASTR File UFB*/~
                            #02,                   /* RCVLINES File UFB*/~
                            #20)                   /* TXTFILE  File UFB*/

        dim                                                              ~
            bol$30,                      /* Bill of Lading Number      */~
            carrier_code$9,              /* Carrier Vendor Code, if one*/~
            carrier_name$30,             /* Carrier Name               */~
            comment$30,                  /* Comments                   */~
            cursor%(2),                  /* CURSOR LOACATIONS FOR EDIT */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            distr$65,                    /* Qty Distribution           */~
            distr_hdr$65,                /* Qty Distribution Header    */~
            due$(101)8,                  /* DUE DATE FOR LINE          */~
            header$79,                   /* SCREEN HEADER              */~
            i$(24)80,                    /* JUNK SCREEN IMAGE (NOT USED*/~
            job$(101)8,                  /* Job from PO Line           */~
            last_read$90,                /* Last value read            */~
            lfac$(12)1,                  /* Field Atribute Characters  */~
            line$(101)3,                 /* Line Numbers               */~
            message$79,                  /* SCREEN INFO MESSAGE LINE   */~
            part$(101,2)32,              /* PART NUMBERS & DESCR       */~
            pfdescr$(3)79,               /* TEXT FOR PFKEYS ACTIVE     */~
            pfkeys$32,                   /* PFKEYS ACTIVE              */~
            plowkey$90,                  /* WORK VARIABLE              */~
            po_ps$(101)33,               /* P.O. & Pack Slip Array     */~
            po$16,                       /* P.O. NUMBER                */~
            pol$3,                       /* P.O. Line                  */~
            qty(101,6),                  /* Misc Qtys  1 = Recvr Hold  */~
                                         /*            2 = QC          */~
                                         /*            3 = QC Hold     */~
                                         /*            4 = to Rwrk/Scrp*/~
                                         /*            5 = Returned    */~
                                         /*            6 = to On Hand  */~
            rcvr$16,                     /* Receiver Number            */~
            rcvd$(101)10,                /* TOTAL QUANTITY RECEIVED    */~
            rcv_date$8,                  /* Date of Receiver           */~
            rcv_time$8,                  /* Time of Receiver           */~
            rcvtitle$79,                 /* SCREEN TITLE               */~
            seq$(100)4,                  /* SCREEN PLACE HOLDER        */~
            text$(113,1)70,              /* Free Text Array            */~
            textid$(101,3)4,             /* Receiver/PO/QC Text IDs    */~
            textmsg$79,                  /* DOCUMENT TEXT DUSPLAY TITLE*/~
            txtmsg$(3)4,                 /* Text msg - Line/PO/QC      */~
            title$79,                    /* SCREEN TITLE               */~
            uom$(101)4,                  /* Unit of Measure - Stocking */~
            vendor$9,                    /* Vendor Code                */~
            vendor$(101)40               /* Vendor Code & Name Array   */

        dim f1%(20)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! RCVMASTR ! Receiver Master File                     *~
            * #02 ! RCVLINES ! Receiver Line Items                      *~
            * #03 ! VENDOR   ! Vendor Master File                       *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #20 ! TXTFILE  ! System Text File                         *~
            *************************************************************

            select #03, "VENDOR",                                        ~
                        varc, indexed, recsize = 600,                    ~
                        keypos = 1, keylen =  9,                         ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #04, "HNYMASTR",                                      ~
                        varc, indexed, recsize = 900,                    ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen =  9, dup, ~
                                  key 2, keypos =  90, keylen =  4, dup, ~
                                  key 3, keypos =  26, keylen = 32, dup

            if been_here_before% = 1% then L09000
                call "OPENCHCK" (#03, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#04, 0%, 0%, 0%, " ")
                been_here_before% = 1%

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            if seq$() <> " " then L09170

            for i% = 1% to 100%
                convert i% to seq$(i%), pic(###)
                str(seq$(i%),4%) = ")"
            next i%

            txtmsg$(1%) = "Line"
            txtmsg$(2%) = "  PO"
            txtmsg$(3%) = "  QC"
            title$ = "Receiver: " & rcvr$
            str(title$,62%) = "RCVSTATS: " & cms2v$
            header$ = "Seq  Vendor    Vendor Name                    Purc~
        ~hase Order   Packing Slip"
            rcvtitle$ = "Line  Part Number                      UOM    Re~
        ~ceived  Due Date Job Nmbr"

L09170:     if vendor$ = " " then gosub view_rcvr else goto L09200
            goto L65000

L09200:     gosub prime_for_detail
            gosub show_dist
            goto L65000

        REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * Top Level - Receiver Data...                              *~
            *************************************************************

        view_rcvr
            gosub load_rcvr
L10090:     message$ = "Position Cursor And Press (RETURN) To See Receive~
        ~r Line Details."
            lfac$() = all(hex(9c))
            pfdescr$(1%)= "                                              ~
        ~                 (13)Instructions"
            pfdescr$(2%)= "(2)First Lines   (4/6)Previous Lines          ~
        ~                 (15)Print Screen"
            pfdescr$(3%)= "(3)Last Lines    (5/7)Next Lines              ~
        ~                 (16)Return"
            pfkeys$ = hex(00ff02040506070dff0f1003ffffffff)

*        Check Appropriate Fields
            if base% > 0 then L10250
                str(pfdescr$(2%),,39%) = " "  /* Shut Off Prev Stuff */
                str(pfkeys$,3%,2%), str(pfkeys$,6%,1%) = hex(ffff)
L10250:     if base% < maxlines% - 12% then L10350
                str(pfdescr$(3%),,39%) = " "  /* Shut Off Next Stuff */
                str(pfkeys$,5%,1%), str(pfkeys$,7%,1%) = hex(ff)
                str(pfkeys$,12%,1%) = hex(ff)

L10350:     if maxlines% = 1% then L10580
            for loop% = base% + 1% to min(maxlines% - 1%, base% + 12%)
                m% = loop% - base%
                lfac$(m%) = hex(8e)
            next loop%

L10580: accept                                                           ~
               at (01,02), "Receiver Status                              ~
        ~                   Today:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,02), "Carrier Code:",                              ~
               at (04,16), fac(hex(84)), carrier_code$          , ch(09),~
               at (04,56), "Receiver Date:",                             ~
               at (04,71), fac(hex(84)), rcv_date$              , ch(08),~
                                                                         ~
               at (05,02), "Carrier Name:",                              ~
               at (05,16), fac(hex(84)), carrier_name$          , ch(30),~
               at (05,56), "Receiver Time:",                             ~
               at (05,71), fac(hex(84)), rcv_time$              , ch(08),~
                                                                         ~
               at (06,02), "BOL:",                                       ~
               at (06,07), fac(hex(84)), bol$                   , ch(30),~
               at (06,40), "Comments:",                                  ~
               at (06,50), fac(hex(84)), comment$               , ch(30),~
                                                                         ~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(lfac$(01)), seq$(base% + 01%)    , ch(04),~
               at (10,02), fac(lfac$(02)), seq$(base% + 02%)    , ch(04),~
               at (11,02), fac(lfac$(03)), seq$(base% + 03%)    , ch(04),~
               at (12,02), fac(lfac$(04)), seq$(base% + 04%)    , ch(04),~
               at (13,02), fac(lfac$(05)), seq$(base% + 05%)    , ch(04),~
               at (14,02), fac(lfac$(06)), seq$(base% + 06%)    , ch(04),~
               at (15,02), fac(lfac$(07)), seq$(base% + 07%)    , ch(04),~
               at (16,02), fac(lfac$(08)), seq$(base% + 08%)    , ch(04),~
               at (17,02), fac(lfac$(09)), seq$(base% + 09%)    , ch(04),~
               at (18,02), fac(lfac$(10)), seq$(base% + 10%)    , ch(04),~
               at (19,02), fac(lfac$(11)), seq$(base% + 11%)    , ch(04),~
               at (20,02), fac(lfac$(12)), seq$(base% + 12%)    , ch(04),~
                                                                         ~
               at (09,07), fac(hex(84)), vendor$(base% + 01%)   , ch(40),~
               at (10,07), fac(hex(84)), vendor$(base% + 02%)   , ch(40),~
               at (11,07), fac(hex(84)), vendor$(base% + 03%)   , ch(40),~
               at (12,07), fac(hex(84)), vendor$(base% + 04%)   , ch(40),~
               at (13,07), fac(hex(84)), vendor$(base% + 05%)   , ch(40),~
               at (14,07), fac(hex(84)), vendor$(base% + 06%)   , ch(40),~
               at (15,07), fac(hex(84)), vendor$(base% + 07%)   , ch(40),~
               at (16,07), fac(hex(84)), vendor$(base% + 08%)   , ch(40),~
               at (17,07), fac(hex(84)), vendor$(base% + 09%)   , ch(40),~
               at (18,07), fac(hex(84)), vendor$(base% + 10%)   , ch(40),~
               at (19,07), fac(hex(84)), vendor$(base% + 11%)   , ch(40),~
               at (20,07), fac(hex(84)), vendor$(base% + 12%)   , ch(40),~
                                                                         ~
               at (09,48), fac(hex(84)), po_ps$(base% + 01%)    , ch(33),~
               at (10,48), fac(hex(84)), po_ps$(base% + 02%)    , ch(33),~
               at (11,48), fac(hex(84)), po_ps$(base% + 03%)    , ch(33),~
               at (12,48), fac(hex(84)), po_ps$(base% + 04%)    , ch(33),~
               at (13,48), fac(hex(84)), po_ps$(base% + 05%)    , ch(33),~
               at (14,48), fac(hex(84)), po_ps$(base% + 06%)    , ch(33),~
               at (15,48), fac(hex(84)), po_ps$(base% + 07%)    , ch(33),~
               at (16,48), fac(hex(84)), po_ps$(base% + 08%)    , ch(33),~
               at (17,48), fac(hex(84)), po_ps$(base% + 09%)    , ch(33),~
               at (18,48), fac(hex(84)), po_ps$(base% + 10%)    , ch(33),~
               at (19,48), fac(hex(84)), po_ps$(base% + 11%)    , ch(33),~
               at (20,48), fac(hex(84)), po_ps$(base% + 12%)    , ch(33),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3%)            ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

           if keyhit% <> 0% then L11470
              close crt
              call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
              cursor%(1%) = cursor%(1%) - 8%
              if cursor%(1%) < 1% or cursor%(1%) > 12% then L10580
              if lfac$(cursor%(1%)) <> hex(8e) then L10580
              pos% = cursor%(1%) + base%
              gosub show_detail
              goto L10090

L11470:    if keyhit% <> 13% then L11510
                call "MANUAL" ("RCVSTATS")
                goto L10580

L11510:    if keyhit% <> 15% then L11640
                call "PRNTSCRN"
                goto L10580

L11640:    if keyhit% = 16% then L65000
           if keyhit% = 2% then base% = 0%
           if keyhit% = 3% then base% = maxlines% - 12%
           if keyhit% = 4% then base% = base% - 11%
           if keyhit% = 5% then base% = base% + 11%
           if keyhit% = 6% then base% = base% - 1%
           if keyhit% = 7% then base% = base% + 1%
           base% = max(0%, min(base%, maxlines% - 12%))
           goto L10090

        REM *************************************************************~
            * Bypassing 1st screen, but need to set up header info      *~
            *************************************************************
        prime_for_detail
            gosub load_rcvr
            pos% = 1% : str(header$,,3%) = " " : mode% = 1%
            seq$(1%) = " "
            str(vendor$(1%),,9%) = vendor$
            str(po_ps$(1%),,16%) = po$
            call "DESCRIBE" (#03, vendor$, str(vendor$(1%),11%,30%), 0%, ~
                                                                 f1%(3%))
            str(vendor$(1%),10%,1%), str(po_ps$(1%),17%,1%)  = hex(8c)
            plowkey$ = str(rcvr$) & str(vendor$) & str(po$) & str(pol$) &~
                                                    hex(0000000000000000)
            call "PLOWNEXT" (#02, plowkey$, 44%, f1%(2%))
            if f1%(2%) = 1% then get #02 using L11940,                    ~
                                                  str(po_ps$(1%),18%,16%)
L11940:         FMT POS(128), CH(16)
            if f1%(2%) = 0% then return

            c%, pos1% = 1%
            line$(c%) = pol$
            gosub get_line_info
            return

        REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * Receiver Line Detail Screen...                            *~
            *************************************************************

        show_detail
            gosub load_detail
            mode% = 1%
L12110:     message$ = "Position Cursor And Press (RETURN) To See Invento~
        ~ry Distribution."
L12150:     lfac$() = all(hex(9c))
            pfdescr$(1%)= "                        (9)Part Toggle   (23)R~
        ~eceiver Text     (13)Instructions"
            pfdescr$(2%)= "(2)First Trans.  (4/6)Previous           (24)P~
        ~O Text           (15)Print Screen"
            pfdescr$(3%)= "(3)Last Trans.   (5/7)Next Transactions  (25)Q~
        ~C Text           (16)Return"
            pfkeys$ = hex(000102040506070dff0f10030917181920ff)

*        Set Up Appropriate Fields
            if bas1% > 0% then L12300
                str(pfdescr$(2%),,30%) = " "  /* Shut Off Prev Stuff */
                str(pfkeys$,3%,2%), str(pfkeys$,6%,1%) = hex(ffff)
L12300:     if bas1% < max% - 9% then L12400
                str(pfdescr$(3%),,39%) = " "  /* Shut Off Next Stuff */
                str(pfkeys$,5%,1%), str(pfkeys$,7%,1%) = hex(ff)
                str(pfkeys$,12%,1%) = hex(ff)

L12400:     if mode% = 1% then rcvtitle$ = "Line  Part Number            ~
        ~          UOM    Received  Due Date Job Nmbr"                    ~
                          else rcvtitle$ = "Line  Part Description       ~
        ~          UOM    Received  Due Date Job Nmbr"

            if max% = 1% then L12770
            for loop% = bas1% + 1% to min(max% - 1%, bas1% + 9%)
                c% = loop% - bas1%
                lfac$(c%) = hex(8e)
            next loop%

L12770: accept                                                           ~
               at (01,02), "Receiver Line Detail                         ~
        ~                   Today:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,02), "Carrier Code:",                              ~
               at (04,16), fac(hex(84)), carrier_code$          , ch(09),~
               at (04,56), "Receiver Date:",                             ~
               at (04,71), fac(hex(84)), rcv_date$              , ch(08),~
                                                                         ~
               at (05,02), "Carrier Name:",                              ~
               at (05,16), fac(hex(84)), carrier_name$          , ch(30),~
               at (05,56), "Receiver Time:",                             ~
               at (05,71), fac(hex(84)), rcv_time$              , ch(08),~
                                                                         ~
               at (06,02), "BOL:",                                       ~
               at (06,07), fac(hex(84)), bol$                   , ch(30),~
               at (06,40), "Comments:",                                  ~
               at (06,50), fac(hex(84)), comment$               , ch(30),~
                                                                         ~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(hex(8c)), seq$(pos%)             , ch(04),~
               at (09,07), fac(hex(8c)), vendor$(pos%)          , ch(40),~
               at (09,48), fac(hex(8c)), po_ps$(pos%)           , ch(33),~
               at (11,08), fac(hex(ac)), rcvtitle$              , ch(73),~
                                                                         ~
               at (12,08), fac(lfac$(01)), line$(bas1% + 01%)   , ch(03),~
               at (13,08), fac(lfac$(02)), line$(bas1% + 02%)   , ch(03),~
               at (14,08), fac(lfac$(03)), line$(bas1% + 03%)   , ch(03),~
               at (15,08), fac(lfac$(04)), line$(bas1% + 04%)   , ch(03),~
               at (16,08), fac(lfac$(05)), line$(bas1% + 05%)   , ch(03),~
               at (17,08), fac(lfac$(06)), line$(bas1% + 06%)   , ch(03),~
               at (18,08), fac(lfac$(07)), line$(bas1% + 07%)   , ch(03),~
               at (19,08), fac(lfac$(08)), line$(bas1% + 08%)   , ch(03),~
               at (20,08), fac(lfac$(09)), line$(bas1% + 09%)   , ch(03),~
                                                                         ~
               at (12,14), fac(hex(84)), part$(bas1% + 1%,mode%), ch(32),~
               at (13,14), fac(hex(84)), part$(bas1% + 2%,mode%), ch(32),~
               at (14,14), fac(hex(84)), part$(bas1% + 3%,mode%), ch(32),~
               at (15,14), fac(hex(84)), part$(bas1% + 4%,mode%), ch(32),~
               at (16,14), fac(hex(84)), part$(bas1% + 5%,mode%), ch(32),~
               at (17,14), fac(hex(84)), part$(bas1% + 6%,mode%), ch(32),~
               at (18,14), fac(hex(84)), part$(bas1% + 7%,mode%), ch(32),~
               at (19,14), fac(hex(84)), part$(bas1% + 8%,mode%), ch(32),~
               at (20,14), fac(hex(84)), part$(bas1% + 9%,mode%), ch(32),~
                                                                         ~
               at (12,47), fac(hex(8c)), uom$(bas1% + 1%),        ch(04),~
               at (13,47), fac(hex(8c)), uom$(bas1% + 2%),        ch(04),~
               at (14,47), fac(hex(8c)), uom$(bas1% + 3%),        ch(04),~
               at (15,47), fac(hex(8c)), uom$(bas1% + 4%),        ch(04),~
               at (16,47), fac(hex(8c)), uom$(bas1% + 5%),        ch(04),~
               at (17,47), fac(hex(8c)), uom$(bas1% + 6%),        ch(04),~
               at (18,47), fac(hex(8c)), uom$(bas1% + 7%),        ch(04),~
               at (19,47), fac(hex(8c)), uom$(bas1% + 8%),        ch(04),~
               at (20,47), fac(hex(8c)), uom$(bas1% + 9%),        ch(04),~
                                                                         ~
               at (12,52), fac(hex(84)), rcvd$(bas1% + 1%)      , ch(10),~
               at (13,52), fac(hex(84)), rcvd$(bas1% + 2%)      , ch(10),~
               at (14,52), fac(hex(84)), rcvd$(bas1% + 3%)      , ch(10),~
               at (15,52), fac(hex(84)), rcvd$(bas1% + 4%)      , ch(10),~
               at (16,52), fac(hex(84)), rcvd$(bas1% + 5%)      , ch(10),~
               at (17,52), fac(hex(84)), rcvd$(bas1% + 6%)      , ch(10),~
               at (18,52), fac(hex(84)), rcvd$(bas1% + 7%)      , ch(10),~
               at (19,52), fac(hex(84)), rcvd$(bas1% + 8%)      , ch(10),~
               at (20,52), fac(hex(84)), rcvd$(bas1% + 9%)      , ch(10),~
                                                                         ~
               at (12,64), fac(hex(8c)), due$(bas1% + 1%),        ch(08),~
               at (13,64), fac(hex(8c)), due$(bas1% + 2%),        ch(08),~
               at (14,64), fac(hex(8c)), due$(bas1% + 3%),        ch(08),~
               at (15,64), fac(hex(8c)), due$(bas1% + 4%),        ch(08),~
               at (16,64), fac(hex(8c)), due$(bas1% + 5%),        ch(08),~
               at (17,64), fac(hex(8c)), due$(bas1% + 6%),        ch(08),~
               at (18,64), fac(hex(8c)), due$(bas1% + 7%),        ch(08),~
               at (19,64), fac(hex(8c)), due$(bas1% + 8%),        ch(08),~
               at (20,64), fac(hex(8c)), due$(bas1% + 9%),        ch(08),~
                                                                         ~
               at (12,73), fac(hex(84)), job$(bas1% + 1%)       , ch(08),~
               at (13,73), fac(hex(84)), job$(bas1% + 2%)       , ch(08),~
               at (14,73), fac(hex(84)), job$(bas1% + 3%)       , ch(08),~
               at (15,73), fac(hex(84)), job$(bas1% + 4%)       , ch(08),~
               at (16,73), fac(hex(84)), job$(bas1% + 5%)       , ch(08),~
               at (17,73), fac(hex(84)), job$(bas1% + 6%)       , ch(08),~
               at (18,73), fac(hex(84)), job$(bas1% + 7%)       , ch(08),~
               at (19,73), fac(hex(84)), job$(bas1% + 8%)       , ch(08),~
               at (20,73), fac(hex(84)), job$(bas1% + 9%)       , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3%)            ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

           if keyhit% <> 0% then L13630
L13540:       close crt
              call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
              cursor%(1%) = cursor%(1%) - 11%
              if cursor%(1%) < 1% or cursor%(1%) > 9% then L12770
              if lfac$(cursor%(1%)) <> hex(8e) then L12770
              pos1% = cursor%(1%) + bas1%
              if keyhit% <> 0% then return
              gosub show_dist
              goto L12110

L13630:    if keyhit% < 23% or keyhit% > 25% then L13680
                gosub L13540 /* Get Position */
                tt% = keyhit% - 22%
                textmsg$ = txtmsg$(tt%) & " Text for Receiver: " & rcvr$
                textmsg$ = textmsg$ & "  PO: "   & po_ps$(pos%)
                textmsg$ = textmsg$ & "  Line: " & line$(pos1%)
                call "TXTDSPLY" (#20, 0%, "005", textmsg$,               ~
                                 textid$(pos1%, tt%), text$())
                goto L12770

L13680:    if keyhit% <> 9% then L13770
                if mode% = 2% then mode% = 1% else mode% = 2%
                goto L12150

L13770:    if keyhit% <> 13% then L13810
                call "MANUAL" ("RCVSTATS")
                goto L12770

L13810:    if keyhit% <> 15% then L13850
                call "PRNTSCRN"
                goto L12770

L13850:    if keyhit% = 16% then return
           if keyhit% = 32% then L65000
           if keyhit% = 2% then bas1% = 0%
           if keyhit% = 3% then bas1% = max% - 9%
           if keyhit% = 4% then bas1% = bas1% - 8%
           if keyhit% = 5% then bas1% = bas1% + 8%
           if keyhit% = 6% then bas1% = bas1% - 1%
           if keyhit% = 7% then bas1% = bas1% + 1%
           bas1% = max(0%,  min(bas1%, max% - 9%))
           goto L12110

        REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * Receiver Line Quantities Distribution                     *~
            *************************************************************

        show_dist
            message$ = " "
            lfac$() = all(hex(9c))
            pfdescr$(1%)= "                                         (23)R~
        ~eceiver Text     (13)Instructions"
            pfdescr$(2%)= "                                         (24)P~
        ~O Text           (15)Print Screen"
            pfdescr$(3%)= "                                         (25)Q~
        ~C Text           (16)Return"
            pfkeys$ = hex(0d0f10171819)
            distr_hdr$ = "Recvr Hold         QC    QC Hold Rewrk/Scrp   R~
        ~eturned  Inventory"
            tmp1 = qty(pos1%, 1%) : tmp2 = qty(pos1%, 2%)
            tmp3 = qty(pos1%, 3%) : tmp4 = qty(pos1%, 4%)
            tmp5 = qty(pos1%, 5%) : tmp6 = qty(pos1%, 6%)
            if mode% <> 2% then L14260
L14260:     call "CONVERT" (tmp1, 0.2, str(distr$,  1%, 10%))
            call "CONVERT" (tmp2, 0.2, str(distr$, 12%, 10%))
            call "CONVERT" (tmp3, 0.2, str(distr$, 23%, 10%))
            call "CONVERT" (tmp4, 0.2, str(distr$, 34%, 10%))
            call "CONVERT" (tmp5, 0.2, str(distr$, 45%, 10%))
            call "CONVERT" (tmp6, 0.2, str(distr$, 56%, 10%))


L14520: accept                                                           ~
               at (01,02), "Receiver Line Distribution Details           ~
        ~                   Today:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,02), "Carrier Code:",                              ~
               at (04,16), fac(hex(84)), carrier_code$          , ch(09),~
               at (04,56), "Receiver Date:",                             ~
               at (04,71), fac(hex(84)), rcv_date$              , ch(08),~
                                                                         ~
               at (05,02), "Carrier Name:",                              ~
               at (05,16), fac(hex(84)), carrier_name$          , ch(30),~
               at (05,56), "Receiver Time:",                             ~
               at (05,71), fac(hex(84)), rcv_time$              , ch(08),~
                                                                         ~
               at (06,02), "BOL:",                                       ~
               at (06,07), fac(hex(84)), bol$                   , ch(30),~
               at (06,40), "Comments:",                                  ~
               at (06,50), fac(hex(84)), comment$               , ch(30),~
                                                                         ~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(hex(8c)), seq$(pos%)             , ch(04),~
               at (09,07), fac(hex(8c)), vendor$(pos%)          , ch(40),~
               at (09,48), fac(hex(8c)), po_ps$(pos%)           , ch(33),~
               at (11,08), fac(hex(ac)), rcvtitle$              , ch(73),~
                                                                         ~
               at (12,08), fac(hex(8c)), line$(pos1%)           , ch(03),~
               at (12,14), fac(hex(8c)), part$(pos1%, mode%)    , ch(32),~
               at (12,47), fac(hex(8c)), uom$(pos1%)            , ch(04),~
               at (12,52), fac(hex(8c)), rcvd$(pos1%)           , ch(10),~
               at (12,64), fac(hex(8c)), due$(pos1%)            , ch(08),~
               at (12,73), fac(hex(8c)), job$(pos1%)            , ch(08),~
                                                                         ~
               at (14,16), fac(hex(ac)), distr_hdr$             , ch(65),~
               at (15,16), fac(hex(84)), distr$                 , ch(65),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3%)            ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

           if keyhit% < 23% or keyhit% > 25% then L15540
                tt% = keyhit% - 22%
                textmsg$ = txtmsg$(tt%) & " Text for Receiver: " & rcvr$
                textmsg$ = textmsg$ & "  PO: "   & po_ps$(pos%)
                textmsg$ = textmsg$ & "  Line: " & line$(pos1%)
                call "TXTDSPLY" (#20, 0%, "005", textmsg$,               ~
                                 textid$(pos1%, tt%), text$())
                goto L14520

L15540:    if keyhit% <> 13% then L15580
                call "MANUAL" ("RCVSTATS")
                goto L14520

L15580:    if keyhit% <> 15% then L15620
                call "PRNTSCRN"
                goto L14520

L15620:    if keyhit% = 16% then return
           if keyhit% = 32% then L65000
           goto L14520

        REM *************************************************************~
            *           L O A D   U P   R C V   D A T A                 *~
            *                                                           *~
            * Get data, then show it...                                 *~
            *************************************************************

        load_rcvr
            call "READ100" (#01, rcvr$, f1%(1%))
            if f1%(1%) = 0% then L65000  /* Bad news if this happens */

            call "SHOSTAT" ("Loading Receiver For Display")
*        First get receiver master info
            get #01 using L30130, carrier_code$, carrier_name$, bol$,     ~
                                 rcv_date$, rcv_time$, comment$
L30130:         FMT POS(17), CH(9), CH(30), CH(30), CH(6), POS(97),      ~
                    CH(8), CH(30)
            call "DATEFMT" (rcv_date$)
            if vendor$ <> " " then return  /* Skipping first screen */

*        Now extract line info; these are vendor POs
            maxlines%, base% = 0
            vendor$(), po_ps$() = " "
            last_read$ = " "
            plowkey$ = str(rcvr$) & hex(00)
        rcvlines_loop
            call "PLOWNEXT" (#02, plowkey$, 16%, f1%(2%))
                if f1%(2%) = 0% then end_of_receiver
            if str(plowkey$,,41%)=str(last_read$,,41%) then rcvlines_loop
                last_read$ = str(plowkey$,,41%)
            maxlines%, m%  = maxlines% + 1%
            get #02 using L30280, str(vendor$(m%),,9%),                   ~
                                 str(po_ps$(m%),,16%),                   ~
                                 str(po_ps$(m%),18%,16%)
L30280:         FMT POS(42), CH(9), CH(16), POS(128), CH(16)
            call "DESCRIBE" (#03, str(vendor$(m%),,9%),                  ~
                                  str(vendor$(m%),11%,30%),              ~
                                  0%, f1%(3%))
            if f1%(3%) = 0% then str(vendor$(m%),11%,30%) =              ~
                                 "** Not on File **"
            str(vendor$(m%),10%,1%) = hex(8c)
            str(po_ps$(m%),17%,1%)  = hex(8c)
            goto rcvlines_loop

        end_of_receiver
            maxlines% = maxlines% + 1  /* Slot For 'End' Message */
            vendor$(maxlines%) = hex(8c) & "   ** End Of Receiver **"
            return


        REM *************************************************************~
            *           L O A D   U P   D E T A I L S                   *~
            *                                                           *~
            * Get data, then show it...                                 *~
            *************************************************************

        load_detail
            max%, bas1% = 0%
            line$(), part$(), uom$(), rcvd$(), due$(), job$() = " "

            plowkey$ = str(rcvr$) & str(vendor$(pos%),,9%) &             ~
                       str(po_ps$(pos%),,16%) & hex(00)
        line_loop
            call "PLOWNEXT" (#02, plowkey$, 41%, f1%(2%))
                if f1%(2%) = 0% then end_of_lines
            max%, c% = max% + 1%
        get_line_info
            get #02 using L31210, part$(c%,1%), line$(c%), part$(c%,2%),  ~
                                 due$(c%), textid$(c%,2%), rcvd,         ~
                                 qty(c%,1%), qty(c%,2%), qty(c%,3%),     ~
                                 qty(c%,4%), qty(c%,5%), qty(c%,6%),     ~
                                 uom$(c%), textid$(c%,1%),               ~
                                 textid$(c%,3%), job$(c%)
L31210:         FMT CH(25), POS(67), CH(3), POS(78), CH(32), POS(116),   ~
                    CH(6), POS(152), CH(4), 7*PD(14,4), POS(244), CH(4), ~
                    POS(388), 2*CH(4), POS(400), CH(8)
            call "DATEFMT" (due$(c%))
            call "CONVERT" (rcvd, 0.2, rcvd$(c%))
            call "READ100" (#04, part$(c%,1%), f1%(4%))
                if f1%(4%) = 1% then get #04 using L31280, uom$(c%)
L31280:              FMT POS(74), CH(4)
            if vendor$ <> " " then return  /* Skipping second screen */
            goto line_loop

        end_of_lines
            max% = max% + 1%  /* Slot For 'End' Message */
            part$(max%, 1%) = hex(8c) & "** End Of Detail **"
            part$(max%, 2%) = hex(8c) & "** End Of Detail **"
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
