        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  BBBB    OOO   M   M  BBBB   PPPP   DDDD    SSS   PPPP    *~
            *  B   B  O   O  MM MM  B   B  P   P  D   D  S      P   P   *~
            *  BBBB   O   O  M M M  BBBB   PPPP   D   D   SSS   PPPP    *~
            *  B   B  O   O  M   M  B   B  P      D   D      S  P       *~
            *  BBBB    OOO   M   M  BBBB   P      DDDD    SSS   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMBPDSP - SUBROUTINE TO DISPLAY BY-PRODUCTS FOR A        *~
            *            SPECIFIC PART.                                 *~
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
            * 04/06/89 ! ORIGINAL - 1st attempt, be patient please! MLJ *~
            * 10/08/92 ! PRR11741 Corrected the Batch Qty Cal.    ! SID *~
            * 11/19/92 ! Changed 'SHOWMSG' to 'SHOWSTAT'.         ! SID *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "BOMBPDSP" (part$, bomid$, qty)

        REM * If QTY = 1, Batch Qty is equal to 1 (JOB)
        REM   If QTY = 0, Batch Qty is acquired from BOMMASTR Header (BOM)

         dim                                                             ~
           bompart$(15)25,               /* DISPLAY PART NUMBER        */~
           bomseq$(15)3,                 /* DISPLAY SEQUENCE NUMBER    */~
           bomqty$(15)10,                /* DISPLAY QUANTITY           */~
           bomdescr$(15)32,              /* DISPLAY DESCRIPTION        */~
           blankline$79,                 /* UNDERSCORED BLANK LINE     */~
           colhead1$7,                   /* COLUMN HEADING 1           */~
           colhead2$25,                  /* COLUMN HEADING 2           */~
           colhead3$32,                  /* COLUMN HEADING 3           */~
           colhead4$10,                  /* COLUMN HEADING 4           */~
           date$8,                       /* DATE FOR SCREEN DISPLAY    */~
           line2$79,                     /* SECOND HEADER LINE         */~
           errormsg$79,                  /* ERROR MESSAGE              */~
           pfline$79,                    /* PF KEY LINE VARIABLE       */~
           pfline1$79,                   /* PF (2) $ (5) DISPLAY       */~
           plowkey$50,                   /* PLOW FILE KEY              */~
           readkey$50,                   /* READ FILE KEY              */~
           askhdr$40,                    /* ASKUSER MSG HEADING        */~
           askln1$80,                    /* ASKUSER MSG LINE 1         */~
           askln2$80,                    /* ASKUSER MSG LINE 2         */~
           askln3$80                     /* ASKUSER MSG LINE 3         */~

         dim                                                             ~
           f1%(64),                      /* READ RETURN CODE           */~
           f2%(64),                      /* FILE OPEN MATRIX           */~
           bp%(1)                        /* ARE THERE BY-PRODUCTS?     */~

        REM *************************************************************~
            *     Release Version ID Section                            *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! BOMMASTR ! Bill of Materials Relationship File      *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************

            select # 1, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =   150,             ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56

            select # 2, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize = 900,               ~
                        keypos= 1, keylen = 25,                          ~
                        alt key 1, keypos = 102, keylen = 9, dup,        ~
                            key 2, keypos = 90, keylen = 4, dup

            call "OPENCHCK" (#1, 0%, f2%(1), 0%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2), 0%, " ")

        REM *************************************************************~
            *     INITIALIZATION AND ENTRY                              *~
            *************************************************************

            date$ = date
            line2$, pfline$ = " "
            pfline1$ = "(2)First                         (5)Next"
            colhead1$ = "Seq No."
            colhead2$ = "Part Code"
            colhead3$ = "Part Description"
            colhead4$ = " Quantity "
            call "DATEFMT" (date$)
            str(line2$) = "PART: " & part$ & "  BOM ID: " & bomid$
            str(line2$,62) = "BOMBPDSP: "  & cms2v$

            init (" ")   bomseq$(), bompart$(), bomdescr$(), bomqty$(),  ~
                         errormsg$, plowkey$, readkey$
            bp%(1) = 0%

        REM *************************************************************~
            *     LOAD BY-PRODUCTS FROM MASTER FILE                     *~
            *************************************************************

L10061:     call "SHOSTAT" ("Loading Bill of Materials Information")
            plowkey$ = str(part$,,25) & str(bomid$,,3) & "  0"
L10080:     init(" ") bompart$(), bomqty$(), bomseq$(), bomdescr$()
            bomlines%, v% = 0%
            call "READ100" (#1, plowkey$, f1%(1%))
                 if f1%(1%) = 0% then L10310
            get  #1 using L10096, batchqty
L10096:     FMT  POS(107), PD(14,4)

L10100:     call "PLOWNEXT" (#1, plowkey$, 28%, f1%(1))
                 if f1%(1) = 0% then L10310
            v%, bomlines% = v% + 1%
            get  #1 using L10150, bompart$(v%), bomseq$(v%), bomqqty,     ~
                    bomused, fixdqty, overage
L10150:     FMT  CH(25), POS(54), CH(3), PD(14,4), PD(14,4), PD(14,4),   ~
                 PD(14,4)

        REM IF QTY = 1 use it(from job), else use the batch qty(from BOM)
            if qty = 1 then L10180 else qty = batchqty

L10180: REM TEST FOR BY-PRODUCT
            totqty = (qty * ((bomqqty * bomused) + overage)) + fixdqty
            if totqty < 0 then L10221
            init (" ") bompart$(v%), bomseq$(v%)
            v%, bomlines% = v% - 1
            goto L10100
L10221:     bp%(1) = 1%
            call "CONVERT" (totqty, -0.2, bomqty$(v%))
            readkey$ = bompart$(v%)
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1 then L10270 else L10290
L10270:     get #2 using L10280, bomdescr$(v%)
L10280:     FMT POS(26), CH(32)
L10290:     if bomlines% < 16% then L10100 else goto L10310

L10310: REM PLOW EXIT
           if bomlines% < 15% then pfline$ = " "                         ~
                else pfline$ = pfline1$
           if bomlines% > 0% then L40000 else goto L65000

L40000: accept                                                           ~
               at (01,02),                                               ~
        "BILL OF MATERIALS BY/CO PRODUCT DISPLAY",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(ac)), colhead1$              , ch( 7),~
               at (04,10), fac(hex(ac)), colhead2$              , ch(25),~
               at (04,37), fac(hex(ac)), colhead3$              , ch(32),~
               at (04,71), fac(hex(ac)), colhead4$              , ch(10),~
                                                                         ~
               at (05,04), fac(hex(8c)), bomseq$(1)             , ch( 3),~
               at (06,04), fac(hex(8c)), bomseq$(2)             , ch( 3),~
               at (07,04), fac(hex(8c)), bomseq$(3)             , ch( 3),~
               at (08,04), fac(hex(8c)), bomseq$(4)             , ch( 3),~
               at (09,04), fac(hex(8c)), bomseq$(5)             , ch( 3),~
               at (10,04), fac(hex(8c)), bomseq$(6)             , ch( 3),~
               at (11,04), fac(hex(8c)), bomseq$(7)             , ch( 3),~
               at (12,04), fac(hex(8c)), bomseq$(8)             , ch( 3),~
               at (13,04), fac(hex(8c)), bomseq$(9)             , ch( 3),~
               at (14,04), fac(hex(8c)), bomseq$(10)            , ch( 3),~
               at (15,04), fac(hex(8c)), bomseq$(11)            , ch( 3),~
               at (16,04), fac(hex(8c)), bomseq$(12)            , ch( 3),~
               at (17,04), fac(hex(8c)), bomseq$(13)            , ch( 3),~
               at (18,04), fac(hex(8c)), bomseq$(14)            , ch( 3),~
               at (19,04), fac(hex(8c)), bomseq$(15)            , ch( 3),~
                                                                         ~
               at (05,10), fac(hex(8c)), bompart$(1)            , ch(25),~
               at (06,10), fac(hex(8c)), bompart$(2)            , ch(25),~
               at (07,10), fac(hex(8c)), bompart$(3)            , ch(25),~
               at (08,10), fac(hex(8c)), bompart$(4)            , ch(25),~
               at (09,10), fac(hex(8c)), bompart$(5)            , ch(25),~
               at (10,10), fac(hex(8c)), bompart$(6)            , ch(25),~
               at (11,10), fac(hex(8c)), bompart$(7)            , ch(25),~
               at (12,10), fac(hex(8c)), bompart$(8)            , ch(25),~
               at (13,10), fac(hex(8c)), bompart$(9)            , ch(25),~
               at (14,10), fac(hex(8c)), bompart$(10)           , ch(25),~
               at (15,10), fac(hex(8c)), bompart$(11)           , ch(25),~
               at (16,10), fac(hex(8c)), bompart$(12)           , ch(25),~
               at (17,10), fac(hex(8c)), bompart$(13)           , ch(25),~
               at (18,10), fac(hex(8c)), bompart$(14)           , ch(25),~
               at (19,10), fac(hex(8c)), bompart$(15)           , ch(25),~
                                                                         ~
               at (05,37), fac(hex(8c)), bomdescr$(1)           , ch(32),~
               at (06,37), fac(hex(8c)), bomdescr$(2)           , ch(32),~
               at (07,37), fac(hex(8c)), bomdescr$(3)           , ch(32),~
               at (08,37), fac(hex(8c)), bomdescr$(4)           , ch(32),~
               at (09,37), fac(hex(8c)), bomdescr$(5)           , ch(32),~
               at (10,37), fac(hex(8c)), bomdescr$(6)           , ch(32),~
               at (11,37), fac(hex(8c)), bomdescr$(7)           , ch(32),~
               at (12,37), fac(hex(8c)), bomdescr$(8)           , ch(32),~
               at (13,37), fac(hex(8c)), bomdescr$(9)           , ch(32),~
               at (14,37), fac(hex(8c)), bomdescr$(10)          , ch(32),~
               at (15,37), fac(hex(8c)), bomdescr$(11)          , ch(32),~
               at (16,37), fac(hex(8c)), bomdescr$(12)          , ch(32),~
               at (17,37), fac(hex(8c)), bomdescr$(13)          , ch(32),~
               at (18,37), fac(hex(8c)), bomdescr$(14)          , ch(32),~
               at (19,37), fac(hex(8c)), bomdescr$(15)          , ch(32),~
                                                                         ~
               at (05,71), fac(hex(8c)), bomqty$(1)             , ch(10),~
               at (06,71), fac(hex(8c)), bomqty$(2)             , ch(10),~
               at (07,71), fac(hex(8c)), bomqty$(3)             , ch(10),~
               at (08,71), fac(hex(8c)), bomqty$(4)             , ch(10),~
               at (09,71), fac(hex(8c)), bomqty$(5)             , ch(10),~
               at (10,71), fac(hex(8c)), bomqty$(6)             , ch(10),~
               at (11,71), fac(hex(8c)), bomqty$(7)             , ch(10),~
               at (12,71), fac(hex(8c)), bomqty$(8)             , ch(10),~
               at (13,71), fac(hex(8c)), bomqty$(9)             , ch(10),~
               at (14,71), fac(hex(8c)), bomqty$(10)            , ch(10),~
               at (15,71), fac(hex(8c)), bomqty$(11)            , ch(10),~
               at (16,71), fac(hex(8c)), bomqty$(12)            , ch(10),~
               at (17,71), fac(hex(8c)), bomqty$(13)            , ch(10),~
               at (18,71), fac(hex(8c)), bomqty$(14)            , ch(10),~
               at (19,71), fac(hex(8c)), bomqty$(15)            , ch(10),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
                                                                         ~
               at (22,02), fac(hex(8c)), pfline$                , ch(79),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return",                                 ~
                                                                         ~
               keys (hex(02050f1020)),                                   ~
               key  (keyhit%)

               if keyhit% <> 15% then L40830
                  call "PRNTSCRN"
                  goto L40000
L40830:           if keyhit%  =  2% and bomlines% < 16 then L40000
                  if keyhit%  =  2% and bomlines% > 15 then L10061
                  if keyhit%  =  5% and bomlines% < 16 then L40000
                  if keyhit%  =  5% and bomlines% > 15 then L10080
                  if keyhit%  = 16% then L65000
                  if keyhit%  = 32% then L65000
                  goto L40000

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN                       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            if bp%(1) = 1% then L65180
            askhdr$ = " * * *  W A R N I N G  * * * "
            str(askln1$) = "There are no by-products for this part: "
            str(askln2$) = part$
            str(askln3$) = "Press RETURN to acknowledge and continue."
L65172:     keyhit% = 0
            call "ASKUSER" (keyhit%, askhdr$, askln1$, askln2$, askln3$)
            if keyhit% <> 0 then L65172

L65180:     end
