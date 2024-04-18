        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP    OOO    SSS   TTTTT    A    TTTTT  U   U   SSS    *~
            *  P   P  O   O  S        T     A A     T    U   U  S       *~
            *  PPPP   O   O   SSS     T    AAAAA    T    U   U   SSS    *~
            *  P      O   O      S    T    A   A    T    U   U      S   *~
            *  P       OOO    SSS     T    A   A    T     UUU    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * POSTATUS - Displays relevent P.O. Data For Inquiry.       *~
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
            * 06/04/86 ! ORIGINAL                                 ! HES *~
            * 10/26/87 ! Fixed disp]ay of ven part on detail scrn ! HES *~
            * 01/04/89 ! Correct division by 0 problem (PRR 10374)! JIM *~
            * 06/07/90 ! Fixed PRR 10554.  Invoiced's Flag.       ! SID *~
            * 11/02/92 ! PRR 12409 - Now calls TXTDSPLY with '004'! MLJ *~
            *          !  instead of '003' source.                !     *~
            * 11/04/92 ! Fixed Expd Lead Days display overflow,   ! MLJ *~
            *          !   neg sign is now displayed.             !     *~
            * 08/11/93 ! Added PF10 to see qty details; QC, Etc.  ! JDH *~
            * 08/13/93 ! Added call to RCVSTATS for recvr detail. ! JDH *~
            * 05/22/95 ! Added call to GETDEM to trace demand.    ! JDH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "POSTATUS" (vendor$,               /* Vendor Number    */~
                            po$,                   /* Purchase Order # */~
                            pol$,                  /* Blnk for any line*/~
                            mode%,                 /* 2% = ven units   */~
                                                   /*   Else our units */~
                            #1,                    /* VBKLINES File UFB*/~
                            #2,                    /* RCVLINES File UFB*/~
                            #4,                    /* VBKMASTR File UFB*/~
                            #5,                    /* PAYLINES File UFB*/~
                            #6,                    /* PAYMASTR File UFB*/~
                            #20)                   /* TXTFILE  File UFB*/

        dim                                                              ~
            buyer$3,                     /* BUYER ID                   */~
            contact$20,                  /* VENDOR CONTACT             */~
            cursor%(2),                  /* CURSOR LOACATIONS FOR EDIT */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            distr$65,                    /* Qty Distribution           */~
            distr_hdr$65,                /* Qty Distribution Header    */~
            due$(101)8,                  /* CURRENT DUE DATE FOR LINE  */~
            factor(101),                 /* Unit Of Measeure Convertion*/~
            header$79,                   /* SCREEN HEADER              */~
            i$(24)80,                    /* JUNK SCREEN IMAGE (NOT USED*/~
            inv$(101)16,                 /* Invoice Numbers            */~
            invamt(101),                 /* Dollars Invoiced           */~
            invtext$(101)7,              /* Invoice Screen Memo        */~
            lastpo$16,                   /* Last PO Number             */~
            lastpoline$3,                /* Last PO Line Number        */~
            left$10,                     /* LEFT QUANTITY FOR LINE     */~
            left(101),                   /* LEFT QUANTITY FOR LINE     */~
            lfac$(12)1,                  /* Field Atribute Characters  */~
            line$(101)3,                 /* Line Numbers               */~
            max%(2),                     /* Maxlines counters          */~
            message$79,                  /* SCREEN INFO MESSAGE LINE   */~
            opart$(101)25,               /* VENDORS PART NUMBERS       */~
            ordered$8,                   /* ORDER DATE                 */~
            ordrdate$6,                  /* ORDER DATE                 */~
            orig$10,                     /* ORIG QUANTITY FOR LINE     */~
            orig(101),                   /* ORIG QUANTITY FOR LINE     */~
            origdate$6,                  /* ORDER DATE                 */~
            part$(12)25,                 /* PART NUMBERS               */~
            pfdescr$(2)79,               /* TEXT FOR PFKEYS ACTIVE     */~
            pfkeys$32,                   /* PFKEYS ACTIVE              */~
            plowkey$90,                  /* WORK VARIABLE              */~
            po$16,                       /* P.O. NUMBER                */~
            pol$3,                       /* P.O. Line                  */~
            poline$3,                    /* P.O. LINE NUMBER           */~
            qty(101,6),                  /* Misc Qtys  1 = Recvr Hold  */~
                                         /*            2 = QC          */~
                                         /*            3 = QC Hold     */~
                                         /*            4 = to OH Inv   */~
                                         /*            5 = Rejected    */~
                                         /*            6 = to Rework   */~
            rcv$(101,2)16,               /* Receiver Numbers           */~
            rcvd$10,                     /* TOTAL QUANTITY RECEIVED    */~
            rcvd(101),                   /* TOTAL QUANTITY RECEIVED    */~
            rcvdate$(101,2)8,            /* Receiver Dates             */~
            rcvpack$(101)16,             /* Packing List Numbers       */~
            rcvqty(101),                 /* Quantity Received          */~
            rcvstat$(101)1,              /* Receiver Invoicing Status  */~
            rcvtitle$79,                 /* SCREEN TITLE               */~
            rlt(101),                    /* 'Real' Lead Times          */~
            rscrn$(12)39,                /* For Actuals Display        */~
            seq$(100)4,                  /* SCREEN PLACE HOLDER        */~
            ship$30,                     /* SHIP VIA TEXT              */~
            slt(101),                    /* Standard Lead Times        */~
            subheader$37,                /* SCREEN HEADER              */~
            tagnr$19,                    /* PIPIN Tag number           */~
            text$(113,1)70,              /* Free Text Array            */~
            textid$4,                    /* DOCUMENT TEXT              */~
            textid1$4,                   /* PO LINE TEXT ID NUMBER     */~
            textmsg$79,                  /* DOCUMENT TEXT DUSPLAY TITLE*/~
            title$79,                    /* SCREEN TITLE               */~
            vendor$9,                    /* Vendor Code                */~
            vpart$(101)25                /* VENDORS PART NUMBERS       */

        dim f1%(20)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch Finalization of R6.04.01  "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *  1  ! VBKLINES ! BACKLOG LINE ITEMS FILE                  *~
            *  2  ! RCVLINES ! Receiver Line Items                      *~
            *  3  ! RCVMASTR ! Receiver Master File                     *~
            *  4  ! VBKMASTR ! VENDOR   BACKLOG MASTER FILE             *~
            *  5  ! PAYLINES ! PAYABLES LINE ITEMS FILE                 *~
            *  6  ! PAYLINES ! PAYABLES LINE ITEMS FILE                 *~
            *  7  ! DEMMASTR ! MASTER FILE FOR PLANNING DEMANDS         *~
            *  8  ! PIPCROSS ! PIP HARD PEG CROSS REFERENCE             *~
            *  9  ! PIPIN    ! PIP QUANTS DUE IN                        *~
            *  20 ! TXTFILE  ! System Text File                         *~
            *************************************************************

            select #03, "RCVMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos= 1, keylen = 16                           ~

           select # 7, "DEMMASTR", varc, indexed,                        ~
                     recsize = 123 , keypos = 2   , keylen = 27,         ~
                     alt key 1, keypos =10, keylen = 19,                 ~
                         key 2, keypos = 1, keylen = 28,                 ~
                         key 3, keypos =29, keylen = 25, dup

           select # 8, "PIPCROSS", varc, indexed,                        ~
                     recsize = 150,  keypos =  1  , keylen =  71,        ~
                     alt key 1, keypos = 20, keylen = 52,                ~
                         key 2, keypos = 39, keylen = 33

           select # 9, "PIPIN"   , varc, indexed,                        ~
                     recsize = 60,   keypos =  30 , keylen =  19,        ~
                     alt key 1, keypos = 1 , keylen = 48

            if been_here_before% = 1% then L09000
                call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#8, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#9, 0%, 0%, 0%, " ")
                been_here_before% = 1%

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            poline$ = pol$
            if seq$() <> " " then L09160

            for i% = 1 to 100
                convert i% to seq$(i%), pic(###)
                str(seq$(i%),4) = ")"
            next i%

L09160:     if pol$ = " " then L10000
            gosub L12000
            goto L65000

L10000: REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * PO Summary Inquiry Screen...                              *~
            *************************************************************

            gosub load_po
L10070:     title$ = "Vendor: " & vendor$ & "   PO Number: " & po$
            str(title$,62) = "POSTATUS: " & cms2v$
            message$ = "Position Cursor And Press (ENTER) To See Receipts~
        ~ And Invoicing Details."
            call "STRING" addr ("CT", message$, 79%)
L10120:     lfac$() = all(hex(9c))
            pfdescr$(1) = "(2)First Lines   (4/6)Previous Lines          ~
        ~                 (15)Print Screen"
            pfdescr$(2) = "(3)Last Lines    (5/7)Next Lines       (10)See~
        ~ Quantity Detail (16)Return"
            pfkeys$ = hex(000102040506070dff0f100308090aff0c)
            if mode% = 2 then str(pfdescr$(1),41,15) = "(9)Our Parts"    ~
                          else str(pfdescr$(1),41,15) = "(9)Vendor Parts"

*        Check Appropriate Fields
            if base% > 0 then L10250
                str(pfdescr$(1),,39)     = " "  /* Shut Off Prev Stuff */
                str(pfkeys$,3,2), str(pfkeys$,6,1) = hex(ffff)
L10250:     if base% < maxlines%-12 then L10300
                str(pfdescr$(2),,39)     = " "  /* Shut Off Next Stuff */
                str(pfkeys$,5,1), str(pfkeys$,7,1) = hex(ff)
                str(pfkeys$,12,1) = hex(ff)

L10300:     rscrn$(), part$() = " "
            header$ = "Line Part Number               Due Date   Original~
        ~   Received       Left   Days"
            if mode% = 2 then str(header$,6,20) = "Vendor Part Number"
            subheader$ = "......... QUANTITIES .........   Lead"
            if maxlines% = 1 then L10550
            for loop% = base% + 1 to min(maxlines%-1, base%+12)
             m% = loop% - base%
             tmp = orig(loop%) : tmp1 = rcvd(loop%) : tmp2 = left(loop%)
             if str(pfdescr$(1),41,4)="(9)V" then L10430
               if factor(loop%) = 0 then goto L10430
                tmp = round(tmp/ factor(loop%), 4)
                tmp1 = round(tmp1/ factor(loop%), 4)
                tmp2 = round(tmp2/ factor(loop%), 4)
L10430:      call "CONVERT" (tmp, 0.2, str(rscrn$(m%),1,10))
             call "CONVERT" (tmp1, 0.2, str(rscrn$(m%),12,10))
             str(rscrn$(m%),22,1) = hex(84)
             call "CONVERT" (tmp2, 0.2, str(rscrn$(m%),23,10))
             if tmp2 = 0 then str(rscrn$(m%),22,1) = hex(8c)
             str(rscrn$(m%),11,1) = str(rscrn$(m%),22,1)
             str(rscrn$(m%),34,1) = hex(8c)
             call "CONVERT" (slt(loop%), 0.0, str(rscrn$(m%),36,4))
             part$(m%) = opart$(loop%)
             if str(pfdescr$(1),41,4)="(9)O" then part$(m%)=vpart$(loop%)
             lfac$(m%) = hex(8e)
            next loop%
L10550:     if maxlines% <= base% + 12 then                              ~
               rscrn$(maxlines%-base%) = hex(8c) & "** End Of Order **"

L10580: accept                                                           ~
               at (01,02), "Purchase Order Status Inquiry                ~
        ~                   Today:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,03), "Vendors Order Contact:",                     ~
               at (04,26), fac(hex(84)), contact$               , ch(20),~
               at (04,60), "Order Date:",                                ~
               at (04,72), fac(hex(84)), ordered$               , ch(08),~
                                                                         ~
               at (05,03), "Ship Via Instructions:",                     ~
               at (05,26), fac(hex(84)), ship$                  , ch(30),~
                                                                         ~
               at (06,03), "Buyer Id:",                                  ~
               at (06,13), fac(hex(84)), buyer$                 , ch(03),~
                                                                         ~
               at (06,77), "Expd",                                       ~
               at (07,44), fac(hex(8c)), subheader$             , ch(37),~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(lfac$(01)), line$(base% + 01)    , ch(03),~
               at (10,02), fac(lfac$(02)), line$(base% + 02)    , ch(03),~
               at (11,02), fac(lfac$(03)), line$(base% + 03)    , ch(03),~
               at (12,02), fac(lfac$(04)), line$(base% + 04)    , ch(03),~
               at (13,02), fac(lfac$(05)), line$(base% + 05)    , ch(03),~
               at (14,02), fac(lfac$(06)), line$(base% + 06)    , ch(03),~
               at (15,02), fac(lfac$(07)), line$(base% + 07)    , ch(03),~
               at (16,02), fac(lfac$(08)), line$(base% + 08)    , ch(03),~
               at (17,02), fac(lfac$(09)), line$(base% + 09)    , ch(03),~
               at (18,02), fac(lfac$(10)), line$(base% + 10)    , ch(03),~
               at (19,02), fac(lfac$(11)), line$(base% + 11)    , ch(03),~
               at (20,02), fac(lfac$(12)), line$(base% + 12)    , ch(03),~
                                                                         ~
               at (09,07), fac(hex(84)), part$(01)              , ch(25),~
               at (10,07), fac(hex(84)), part$(02)              , ch(25),~
               at (11,07), fac(hex(84)), part$(03)              , ch(25),~
               at (12,07), fac(hex(84)), part$(04)              , ch(25),~
               at (13,07), fac(hex(84)), part$(05)              , ch(25),~
               at (14,07), fac(hex(84)), part$(06)              , ch(25),~
               at (15,07), fac(hex(84)), part$(07)              , ch(25),~
               at (16,07), fac(hex(84)), part$(08)              , ch(25),~
               at (17,07), fac(hex(84)), part$(09)              , ch(25),~
               at (18,07), fac(hex(84)), part$(10)              , ch(25),~
               at (19,07), fac(hex(84)), part$(11)              , ch(25),~
               at (20,07), fac(hex(84)), part$(12)              , ch(25),~
                                                                         ~
               at (09,33), fac(hex(8c)), due$(base% + 01)       , ch(08),~
               at (10,33), fac(hex(8c)), due$(base% + 02)       , ch(08),~
               at (11,33), fac(hex(8c)), due$(base% + 03)       , ch(08),~
               at (12,33), fac(hex(8c)), due$(base% + 04)       , ch(08),~
               at (13,33), fac(hex(8c)), due$(base% + 05)       , ch(08),~
               at (14,33), fac(hex(8c)), due$(base% + 06)       , ch(08),~
               at (15,33), fac(hex(8c)), due$(base% + 07)       , ch(08),~
               at (16,33), fac(hex(8c)), due$(base% + 08)       , ch(08),~
               at (17,33), fac(hex(8c)), due$(base% + 09)       , ch(08),~
               at (18,33), fac(hex(8c)), due$(base% + 10)       , ch(08),~
               at (19,33), fac(hex(8c)), due$(base% + 11)       , ch(08),~
               at (20,33), fac(hex(8c)), due$(base% + 12)       , ch(08),~
                                                                         ~
               at (09,42), fac(hex(84)), rscrn$(01)             , ch(39),~
               at (10,42), fac(hex(84)), rscrn$(02)             , ch(39),~
               at (11,42), fac(hex(84)), rscrn$(03)             , ch(39),~
               at (12,42), fac(hex(84)), rscrn$(04)             , ch(39),~
               at (13,42), fac(hex(84)), rscrn$(05)             , ch(39),~
               at (14,42), fac(hex(84)), rscrn$(06)             , ch(39),~
               at (15,42), fac(hex(84)), rscrn$(07)             , ch(39),~
               at (16,42), fac(hex(84)), rscrn$(08)             , ch(39),~
               at (17,42), fac(hex(84)), rscrn$(09)             , ch(39),~
               at (18,42), fac(hex(84)), rscrn$(10)             , ch(39),~
               at (19,42), fac(hex(84)), rscrn$(11)             , ch(39),~
               at (20,42), fac(hex(84)), rscrn$(12)             , ch(39),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,24), "(12)Trace Demand",                           ~
               at (22,42), "(8)See P.O. Text",                           ~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

           if keyhit% <> 0% and keyhit% <> 10% and keyhit% <> 12% then   ~
                                                                    L11470
              close crt
              call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
              cursor%(1%) = cursor%(1%) - 8%
              if cursor%(1%) < 1% or cursor%(1%) > 12% then L10580
              if lfac$(cursor%(1%)) <> hex(8e) then L10580
              pos% = cursor%(1%)
              poline$ = line$(cursor%(1%) + base%)
              if keyhit% = 12% then call_getdem
              if keyhit% = 10% then gosub show_dist                      ~
                               else gosub show_detail
              goto L10070

        call_getdem
              call "STRING" addr("RJ", poline$, 3%)
              tagnr$ = "PO" & str(po$,,14%) & poline$
              call "GETDEM"(1%, tagnr$, #8, #7, #9, " ", " ", 0%)
              goto L10070

L11470:    if keyhit% <> 8% then L11520
                textmsg$ = "Free Text For Purchase Order: " & po$
                call "TXTDSPLY" (#20, 0%, "004",textmsg$,textid$,text$())
                goto L10580

L11520:    if keyhit% <> 9% then L11560
                if mode% = 2% then mode% = 1% else mode% = 2%
                goto L10120

L11560:    if keyhit% <> 13% then L11600
                call "MANUAL" ("POSTATUS")
                goto L10580

L11600:    if keyhit% <> 15% then L11640
                call "PRNTSCRN"
                goto L10580

L11640:    if keyhit% = 16% then L65000
           if keyhit% = 2% then base% = 0%
           if keyhit% = 3% then base% = maxlines%-9%
           if keyhit% = 4% then base% = base%-9%
           if keyhit% = 5% then base% = base%+9%
           if keyhit% = 6% then base% = base%-1%
           if keyhit% = 7% then base% = base%+1%
           base%=max(0%,min(base%,maxlines%-12%))
           goto L10120

L12000: REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * PO Summary Inquiry Screen...                              *~
            *************************************************************

        show_detail
            gosub load_detail
            dist% = 1
            title$ = "Vendor: " & vendor$ & "   PO Number: " & po$
            str(title$,63) = "POSTATUS:" & cms2v$
L12110:     message$ = "Position Cursor and press (RETURN) to see Receive~
        ~r Details."
            if dist% = 2% then message$ = " "
L12150:     lfac$() = all(hex(9c))
            pfdescr$(1) = "(2)First Trans.  (4/6)Previous                ~
        ~                 (15)Print Screen"
            pfdescr$(2) = "(3)Last Trans.   (5/7)Next Trans.             ~
        ~                 (16)Return"
            pfkeys$ = hex(000102040506070dff0f10030809200a)
            if mode% = 2 then str(pfdescr$(1),46,15) = "(9)Our Part "    ~
                          else str(pfdescr$(1),46,15) = "(9)Vendor Part "
            if dist% = 2 then str(pfdescr$(2),45,16) = "(10)See Rec'vrs "~
                         else str(pfdescr$(2),45,16) = "(10)See Invoices"

*        Set Up Appropriate Fields
            if bas1% > 0 then L12300
                str(pfdescr$(1),,30)     = " "  /* Shut Off Prev Stuff */
                str(pfkeys$,3,2), str(pfkeys$,6,1) = hex(ffff)
L12300:     if bas1% < max%(dist%)-9 then L12350
                str(pfdescr$(2),,39)     = " "  /* Shut Off Next Stuff */
                str(pfkeys$,5,1), str(pfkeys$,7,1) = hex(ff)
                str(pfkeys$,12,1) = hex(ff)

L12350:     rscrn$() = " "
            header$ = "Line Part Number               Due Date   Original~
        ~   Received       Left   Days"
            if mode% = 2 then str(header$,6,20) = "Vendor Part Number"
            subheader$ = "......... QUANTITIES .........   Lead"
            if dist% = 2 then rcvtitle$ = "Seq   Receiver Number  Inv Dat~
        ~e Invoice Number      Amount       Paid?"  else                  ~
                              rcvtitle$ = "Seq   Receiver Number  Rcv Dat~
        ~e Packing List      Quantity Invcd? Actl"
            tmp = orig(101) : tmp1 = rcvd(101) : tmp2 = left(101)
            part$(1) = opart$(101)
            if mode% <> 2% then L12510
              if factor(101) = 0 then goto L12510
                tmp = round(tmp/ factor(101), 4)
                tmp1 = round(tmp1/ factor(101), 4)
                tmp2 = round(tmp2/ factor(101), 4)
                part$(1) = vpart$(101)
L12510:     call "CONVERT" (tmp, 0.2, orig$)
            call "CONVERT" (tmp1, 0.2, rcvd$)
            call "CONVERT" (tmp2, 0.2, left$)

            if max%(dist%) = 1 then L12740
            for loop% = bas1% + 1 to min(max%(dist%)-1, bas1%+9)
             c% = loop% - bas1%
               if dist% = 1 then L12630
                  rscrn$(c%) = inv$(loop%)
                  call "CONVERT"(invamt(loop%),2.2,str(rscrn$(c%),17,10))
                  str(rscrn$(c%), 32) = invtext$(loop%)
                  goto L12720
L12630:        rscrn$(c%) = rcvpack$(loop%)
               tmp = rcvqty(loop%)
               if mode% <> 2% then L12670
                 if factor(101) <> 0 then tmp = round(tmp/ factor(101), 4)
L12670:        call "CONVERT" (tmp, 0.2, str(rscrn$(c%),17,10))
               str(rscrn$(c%), 31) = rcvstat$(loop%)
               str(rscrn$(c%),35) = hex(8c)
               call "CONVERT" (rlt(loop%), 0.0, str(rscrn$(c%),36,3))
               if rlt(loop%) > slt(101) then str(rscrn$(c%),35) = hex(84)
L12720:      lfac$(c%) = hex(8e)
            next loop%
L12740:     if max%(dist%) <= bas1% + 9 then                             ~
              rscrn$(max%(dist%)-bas1%) = hex(8c) & "** End Of Detail **"

L12770: accept                                                           ~
               at (01,02), "Purchase Order Line Receiver Details         ~
        ~             Todays Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,03), "Vendors Order Contact:",                     ~
               at (04,26), fac(hex(84)), contact$               , ch(20),~
               at (04,60), "Order Date:",                                ~
               at (04,72), fac(hex(84)), ordered$               , ch(08),~
                                                                         ~
               at (05,03), "Ship Via Instructions:",                     ~
               at (05,26), fac(hex(84)), ship$                  , ch(30),~
                                                                         ~
               at (06,03), "Buyer Id:",                                  ~
               at (06,13), fac(hex(84)), buyer$                 , ch(03),~
                                                                         ~
               at (06,77), "Expd",                                       ~
               at (07,44), fac(hex(8c)), subheader$             , ch(37),~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(hex(8c)), line$(101)             , ch(03),~
               at (09,07), fac(hex(8c)), part$(1)               , ch(25),~
               at (09,33), fac(hex(8c)), due$(101)              , ch(08),~
               at (09,42), fac(hex(8c)), orig$                  , ch(10),~
               at (09,53), fac(hex(8c)), rcvd$                  , ch(10),~
               at (09,64), fac(hex(8c)), left$                  , ch(10),~
               at (09,77), fac(hex(8c)), slt(101)            , pic(-###),~
               at (11,11), fac(hex(ac)), rcvtitle$              , ch(70),~
                                                                         ~
               at (12,11), fac(lfac$(01)), seq$(bas1% + 01)     , ch(04),~
               at (13,11), fac(lfac$(02)), seq$(bas1% + 02)     , ch(04),~
               at (14,11), fac(lfac$(03)), seq$(bas1% + 03)     , ch(04),~
               at (15,11), fac(lfac$(04)), seq$(bas1% + 04)     , ch(04),~
               at (16,11), fac(lfac$(05)), seq$(bas1% + 05)     , ch(04),~
               at (17,11), fac(lfac$(06)), seq$(bas1% + 06)     , ch(04),~
               at (18,11), fac(lfac$(07)), seq$(bas1% + 07)     , ch(04),~
               at (19,11), fac(lfac$(08)), seq$(bas1% + 08)     , ch(04),~
               at (20,11), fac(lfac$(09)), seq$(bas1% + 09)     , ch(04),~
                                                                         ~
               at (12,17), fac(hex(84)), rcv$(bas1% + 01,dist%) , ch(16),~
               at (13,17), fac(hex(84)), rcv$(bas1% + 02,dist%) , ch(16),~
               at (14,17), fac(hex(84)), rcv$(bas1% + 03,dist%) , ch(16),~
               at (15,17), fac(hex(84)), rcv$(bas1% + 04,dist%) , ch(16),~
               at (16,17), fac(hex(84)), rcv$(bas1% + 05,dist%) , ch(16),~
               at (17,17), fac(hex(84)), rcv$(bas1% + 06,dist%) , ch(16),~
               at (18,17), fac(hex(84)), rcv$(bas1% + 07,dist%) , ch(16),~
               at (19,17), fac(hex(84)), rcv$(bas1% + 08,dist%) , ch(16),~
               at (20,17), fac(hex(84)), rcv$(bas1% + 09,dist%) , ch(16),~
                                                                         ~
               at (12,34), fac(hex(8c)), rcvdate$(bas1%+01,dist%),ch(08),~
               at (13,34), fac(hex(8c)), rcvdate$(bas1%+02,dist%),ch(08),~
               at (14,34), fac(hex(8c)), rcvdate$(bas1%+03,dist%),ch(08),~
               at (15,34), fac(hex(8c)), rcvdate$(bas1%+04,dist%),ch(08),~
               at (16,34), fac(hex(8c)), rcvdate$(bas1%+05,dist%),ch(08),~
               at (17,34), fac(hex(8c)), rcvdate$(bas1%+06,dist%),ch(08),~
               at (18,34), fac(hex(8c)), rcvdate$(bas1%+07,dist%),ch(08),~
               at (19,34), fac(hex(8c)), rcvdate$(bas1%+08,dist%),ch(08),~
               at (20,34), fac(hex(8c)), rcvdate$(bas1%+09,dist%),ch(08),~
                                                                         ~
               at (12,43), fac(hex(84)), rscrn$(01)             , ch(38),~
               at (13,43), fac(hex(84)), rscrn$(02)             , ch(38),~
               at (14,43), fac(hex(84)), rscrn$(03)             , ch(38),~
               at (15,43), fac(hex(84)), rscrn$(04)             , ch(38),~
               at (16,43), fac(hex(84)), rscrn$(05)             , ch(38),~
               at (17,43), fac(hex(84)), rscrn$(06)             , ch(38),~
               at (18,43), fac(hex(84)), rscrn$(07)             , ch(38),~
               at (19,43), fac(hex(84)), rscrn$(08)             , ch(38),~
               at (20,43), fac(hex(84)), rscrn$(09)             , ch(38),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,47), "(8)Line Text",                               ~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

           if keyhit% <> 0% or dist% = 2% then L13630
              close crt
              call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
              cursor%(1%) = cursor%(1%) - 11%
              if cursor%(1%) < 1% or cursor%(1%) > 9% then L12770
              if lfac$(cursor%(1%)) <> hex(8e) then L12770
              if rcvmastr_open% = 1% then L13590
                call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
                rcvmastr_open% = 1%
L13590:       call "RCVSTATS" (rcv$(bas1% + cursor%(1%), 1%), vendor$,   ~
                               po$, line$(101%), #3, #2, #20)
              goto L12770

L13630:    if keyhit% <> 8% then L13680
                textmsg$ = "Free Text For Purchase Order: " & po$
                textmsg$ = textmsg$ & "  Line: " & line$(101%)
                call "TXTDSPLY" (#20, 0%,"004",textmsg$,textid1$,text$())
                goto L12770

L13680:    if keyhit% <> 9% then L13720
                if mode% = 2% then mode% = 1% else mode% = 2%
                goto L12150

L13720:    if keyhit% <> 10% then L13770
                if dist% = 2% then dist% = 1% else dist% = 2%
                bas1% = 0%
                goto L12110

L13770:    if keyhit% <> 13% then L13810
                call "MANUAL" ("POSTATUS")
                goto L10580

L13810:    if keyhit% <> 15% then L13850
                call "PRNTSCRN"
                goto L12770

L13850:    if keyhit% = 16% then return
           if keyhit% = 32% then L65000
           if keyhit% = 2% then bas1% = 0%
           if keyhit% = 3% then bas1% = max%(dist%)-9%
           if keyhit% = 4% then bas1% = bas1%-7%
           if keyhit% = 5% then bas1% = bas1%+7%
           if keyhit% = 6% then bas1% = bas1%-1%
           if keyhit% = 7% then bas1% = bas1%+1%
           bas1%=max(0%,min(bas1%,max%(dist%)-9%))
           goto L12150

        REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * PO Line Quantities Distribution                           *~
            *************************************************************

        show_dist
            title$ = "Vendor: " & vendor$ & "   PO Number: " & po$
            str(title$,63) = "POSTATUS:" & cms2v$
            message$ = " "
            lfac$() = all(hex(9c))
            pfdescr$(1) = "                                              ~
        ~                 (15)Print Screen"
            pfdescr$(2) = "                                              ~
        ~                 (16)Return"
            pfkeys$ = hex(0d0f10)
            distr_hdr$ = "Recvr Hold         QC    QC Hold  Inventory   R~
        ~ejected     Rework"
            tmp1 = qty(base% + pos%, 1%) : tmp2 = qty(base% + pos%, 2%)
            tmp3 = qty(base% + pos%, 3%) : tmp4 = qty(base% + pos%, 4%)
            tmp5 = qty(base% + pos%, 5%) : tmp6 = qty(base% + pos%, 6%)
            if mode% <> 2% then L14260
              if factor(base% + pos%) = 0 then goto L14260
                tmp1 = round(tmp1/ factor(base% + pos%), 2)
                tmp2 = round(tmp2/ factor(base% + pos%), 2)
                tmp3 = round(tmp3/ factor(base% + pos%), 2)
                tmp4 = round(tmp4/ factor(base% + pos%), 2)
                tmp5 = round(tmp5/ factor(base% + pos%), 2)
                tmp6 = round(tmp6/ factor(base% + pos%), 2)
L14260:     call "CONVERT" (tmp1, 0.2, str(distr$,  1%, 10%))
            call "CONVERT" (tmp2, 0.2, str(distr$, 12%, 10%))
            call "CONVERT" (tmp3, 0.2, str(distr$, 23%, 10%))
            call "CONVERT" (tmp4, 0.2, str(distr$, 34%, 10%))
            call "CONVERT" (tmp5, 0.2, str(distr$, 45%, 10%))
            call "CONVERT" (tmp6, 0.2, str(distr$, 56%, 10%))


L14520: accept                                                           ~
               at (01,02), "Purchase Order Line Distribution Details     ~
        ~             Todays Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,03), "Vendors Order Contact:",                     ~
               at (04,26), fac(hex(84)), contact$               , ch(20),~
               at (04,60), "Order Date:",                                ~
               at (04,72), fac(hex(84)), ordered$               , ch(08),~
                                                                         ~
               at (05,03), "Ship Via Instructions:",                     ~
               at (05,26), fac(hex(84)), ship$                  , ch(30),~
                                                                         ~
               at (06,03), "Buyer Id:",                                  ~
               at (06,13), fac(hex(84)), buyer$                 , ch(03),~
                                                                         ~
               at (06,77), "Expd",                                       ~
               at (07,44), fac(hex(8c)), subheader$             , ch(37),~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(hex(8c)), line$(base% + pos%)    , ch(03),~
               at (09,07), fac(hex(8c)), part$(pos%)            , ch(25),~
               at (09,33), fac(hex(8c)), due$(base% + pos%)     , ch(08),~
               at (09,42), fac(hex(8c)), rscrn$(pos%)           , ch(39),~
               at (11,16), fac(hex(ac)), distr_hdr$             , ch(65),~
                                                                         ~
               at (12,16), fac(hex(8c)), distr$                 , ch(65),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

           if keyhit% <> 13% then L15580
                call "MANUAL" ("POSTATUS")
                goto L14520

L15580:    if keyhit% <> 15% then L15620
                call "PRNTSCRN"
                goto L14520

L15620:    if keyhit% = 16% then return
           if keyhit% = 32% then L65000
           goto L14520

        REM *************************************************************~
            *           L O A D   U P   P O   D A T A                   *~
            *                                                           *~
            * Get data, then show it...                                 *~
            *************************************************************

        load_po
            if po$ = lastpo$ and lastpo$ <> " " then return
            line$(), opart$(), vpart$(), due$(), lastpoline$ = " "
            maxlines%, base% = 0
            mat orig = zer               /* ORIGINALY ORDER QUANTITY   */
            mat rcvd = zer               /* TOTAL RECEIVED             */
            mat left = zer               /* STILL ON ORDER             */
            mat slt  = zer               /* Standard Lead Times        */
            mat factor = zer             /* Vendor Convertion Factor   */
            mat qty  = zer               /* Misc Quantities            */

            str(plowkey$,,9) = vendor$
            str(plowkey$,10,16) = po$
            call "PLOWCODE" (#4, plowkey$, " ", 0%, 0.30, f1%(4))
                if f1%(4) = 0 then L65000
            vendor$ = str(plowkey$,,9)
            lastpo$, po$ = str(plowkey$,10,16)
            call "SHOSTAT" ("Loading Purchase Order For Display")
            get #4, using L30240, contact$, ordered$, buyer$,ship$,textid$
L30240:     FMT XX(205), CH(20), XX(225), CH(6), XX(29), CH(3), XX(33),  ~
                CH(30), XX(10), CH(04)
            ordrdate$ = ordered$
            call "DATEFMT" (ordered$)

            str(plowkey$,26) = all(hex(00))
L30300:     call "PLOWNEXT" (#1, plowkey$, 25%, f1%(1))
                if f1%(1) = 0 then format_totals

            maxlines%, m%  = maxlines% + 1%
            get #1, using L30430,line$(m%),opart$(m%), orig(m%), rcvd(m%),~
                          left(m%), due$(m%), vpart$(m%), factor(m%),    ~
                          origdate$, textid1$, qty(m%,1%), qty(m%,2%),   ~
                          qty(m%,3%), qty(m%,4%), qty(m%,5%), qty(m%,6%)
            call "DATE" addr("G-", ordrdate$, origdate$, days%, ret%)
            if ret% <> 0 then days% = 0
            slt(m%) = days%
            if vpart$(m%) = " " then vpart$(m%) = opart$(m%)
            call "DATEFMT" (due$(m%))
            goto L30300

L30430:         FMT                      /* VBKLINES data file         */~
                POS( 26), CH(3),         /* SEQUENCE NUMBER            */~
                POS( 32), CH(25),        /* PART NUMBER                */~
                POS( 93), PD(14,4),      /* QUANTITY ORIGINALLY ORDERED*/~
                          PD(14,4),      /* QUANTITY RECEIVED          */~
                          PD(14,4),      /* QUANTITY ON ORDER          */~
                POS(142), CH(6),         /* DATE DUE INFORMATION       */~
                POS(197), CH(25),        /* VENDOR PART                */~
                POS(292), PD(14,4),      /* QUANTITY PER UNIT          */~
                          CH(6),         /* Original Due Date          */~
                          CH(4),         /* Line Text ID               */~
                POS(373), 6*PD(14,4)     /* Distribution Quantities    */

        format_totals
            maxlines% = maxlines% + 1  /* Slot For 'End' Message */
        return

        REM *************************************************************~
            *           L O A D   U P   D E T A I L S                   *~
            *                                                           *~
            * Get data, then show it...                                 *~
            *************************************************************

        load_detail
            if po$ <> lastpo$ or lastpo$ = " " then L31110
               if poline$ = lastpoline$ and lastpoline$<> " " then return


L31110:     rcv$(), rcvdate$(), rcvpack$(), rcvstat$(), invtext$(),      ~
            inv$() = " "
            mat rlt = zer                    /* 'Real' Lead Times  */
            mat rcvqty = zer                 /* Receiver Quantities*/
            mat invamt = zer                 /* Invoice Amounts    */
            max%(1), max%(2), bas1% = 0

            str(plowkey$,,9) = vendor$
            str(plowkey$,10,16) = po$
            call "PLOWCODE" (#4, plowkey$, " ", 0%, 0.30, f1%(4))
                if f1%(4) = 0 then L65000
            vendor$ = str(plowkey$,,9)
            lastpo$, po$ = str(plowkey$,10,16)
            get #4, using L30240, contact$, ordered$, buyer$,ship$,textid$
            ordrdate$ = ordered$
            call "DATEFMT" (ordered$)

            str(plowkey$,26) = poline$
            call "PLOWCODE" (#1, plowkey$, " ", 25%, 0.00, f1%(1))
                if f1%(1) = 0 then L10000
            poline$, lastpoline$ = str(plowkey$,26)
            get #1, using L30430, line$(101), opart$(101), orig(101),     ~
                          rcvd(101), left(101), due$(101), vpart$(101),  ~
                          factor(101), origdate$, textid1$
            call "DATE" addr("G-", ordrdate$, origdate$, days%, ret%)
            if ret% <> 0 then days% = 0
            slt(101) = days%
            if vpart$(101) = " " then vpart$(101) = opart$(101)
            call "DATEFMT" (due$(101))

            str(plowkey$,29) = all(hex(00))
L31420:     call "PLOWALTS" (#2, plowkey$, 2%, 28%, f1%(2))
                if f1%(2) = 0 then find_invoices
            max%(1), c%  = max%(1) + 1%
            get #2, using L31550, rcv$(c%,1), rcvdate$(c%,1),rcvpack$(c%),~
                                 rcvqty(c%), openamt, invoicedamt
            call "DATE"addr("G-", ordrdate$, rcvdate$(c%,1), days%, ret%)
            if ret% <> 0 then days% = 0
            rlt(c%) = days%
            call "DATEFMT" (rcvdate$(c%,1))
            rcvstat$(c%) = "Y"
            if openamt <> 0 then rcvstat$(c%) = "N"
            if invoicedamt = 0 then rcvstat$(c%) = "N"
            goto L31420

L31550:     FMT                          /* RECEIVER LINE ITEM         */~
                POS( 26), CH(16),        /* Receiver Number            */~
                POS(122), CH(6),         /* Date Received              */~
                          CH(16),        /* Vendor's Packslip          */~
                POS(156), PD(14,4),      /* Total Quantity Received    */~
                POS(256), PD(14,4),      /* Open Amount                */~
                          PD(14,4),      /* Invoiced Amount            */~
                POS(296), CH(9)          /* (Pre) Payables Liab. Acct. */

        find_invoices
            plowkey$ = po$
            str(plowkey$,17) = poline$
            str(plowkey$,20) = vendor$
L32080:     call "PLOWALTS" (#5, plowkey$, 2%, 28%, f1%(5))
                if f1%(5) = 0 then format_totals_two
            max%(2), c%  = max%(2) + 1%
            get #5, using L32220, rcv$(c%,2), inv$(c%), invamt(c%)
            call "READ100" (#6, str(plowkey$,20,25), f1%(6))
                if f1%(6)<>0 then get #6, using L32150, rcvdate$(c%,2),   ~
                                                       tmp, tmp1
L32150:     FMT XX(41), CH(6), XX(63), 2*PD(14,4)
            call "DATEFMT" (rcvdate$(c%,2))
            if tmp1 = 0 then invtext$(c%) = "Paid"
            if tmp1 <> 0 then invtext$(c%) = "Partial"
            if tmp1 = tmp then invtext$(c%) = "Un-Paid"
            goto L32080

L32220:     FMT  /* PAYLINES - Invoice Line Items    */                  ~
                          CH(16),        /* RECEIVER NUMBER            */~
                POS( 45), CH(16),        /* INVOICE NUMBER             */~
                POS(106), PD(14,4)       /* EXTENSION                  */

        format_totals_two
            max%(1) = max%(1) + 1  /* Slot For 'End' Message */
            max%(2) = max%(2) + 1  /* Slot For 'End' Message */
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
