        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   L      N   N  RRRR    SSS   U   U  BBBB           *~
            *  P   P  L      NN  N  R   R  S      U   U  B   B          *~
            *  PPPP   L      N N N  RRRR    SSS   U   U  BBBB           *~
            *  P      L      N  NN  R   R      S  U   U  B   B          *~
            *  P      LLLLL  N   N  R   R   SSS    UUU   BBBB           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLNRSUB  - DEMAND INPUT PLANNING AND REVIEW FUNCTIONS IN  *~
            *            SUBROUTINE FORM.                               *~
            *            NOTE THAT EVERY SECTION IS (HOPEFULLY) INDEPEN *~
            *            DENT AND COULD BE EXTRACTED (WITH THE APPRO-   *~
            *            PRIATE DIMENSIONS) WITHOUT A LOT OF HASSLE.    *~
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
            * 11/21/83 ! ORIGINAL                                 ! KEN *~
            * 06/18/85 ! ADDED 'GETDEM' TO SCREENS WITH TAG NO. S ! WPH *~
            * 09/25/85 ! WCMASTR FILE FORMAT CHANGE               ! HES *~
            * 09/25/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 12/02/87 ! Corrected 'Next' logic on PIPIN & PIPOUTS! HES *~
            * 05/18/88 ! Reformat of ATC screen & added PF10 to   ! RJM *~
            *          !   switch between ATC/PIP & ATC/SHELF.    !     *~
            *          !  Added GETDEM to WC Detail Screen.       !     *~
            * 05/20/88 ! Added page breaks to demand status report! RJM *~
            * 06/06/88 ! Fixed display of Description Fields      ! RJM *~
            * 10/14/88 ! Add Generic part # option                ! JDH *~
            * 11/04/88 ! Add Alternate Part #'s                   ! KAB *~
            * 02/07/89 ! Initialized ERRORMSG$ to blank @ 32020   ! MJB *~
            * 11/30/90 ! Changed test @ ln 31600 to '> 6' was 5   ! MJB *~
            * 07/02/92 ! Added PIPIN channel in calls to GETDEM   ! WPH *~
            * 11/02/93 ! Purchase Jobs Project - Added support for! JBK *~
            *          !   'BW' and 'RW' type PIPs.               !     *~
            * 07/08/94 ! PRR 12036 - Corrected display when start ! JBK *~
            *          !   date in PIPIN is < 1.  Repaired return !     *~
            *          !   from BOM display.                      !     *~
            *          ! PRR 12500 - Corrected ATC screen when    !     *~
            *          !   showing details for wrong month.       !     *~
            *          ! PRR 12519 - Corrected problems with      !     *~
            *          !   display of Sales Forcast etc. screen.  !     *~
            *          ! PRR 13057 - Added work file to include   !     *~
            *          !   'RO' & 'RW' tags with 'BO' and 'BW'    !     *~
            *          !   tags and 'QC' tags w/'PO' tags when    !     *~
            *          !   displaying PIP details.                !     *~
            *          ! Misc. - Added some (%), calls to the     !     *~
            *          !   to the MXFL... subroutines. Replaced   !     *~
            *          !   'SHOWMSG' with 'SHOSTAT'.              !     *~
            * 07/19/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "PLNRSUB" (mode%, reviewpart$,                           ~
                                     #1, #2, #4, #7, #11, #12, #15, #23, ~
                             #24, #33, #34, #35, #40, #41, #5, #6, #16)

            REM CALLS RECORD FROM FILES-                                 ~
                      #1, DEMMASTR                                       ~
                      #2, PIPMASTR                                       ~
                      #4, HNYMASTR                                       ~
                      #5, HNYDETAL                                       ~
                      #7, RTEMASTR                                       ~
                      #11, WCMASTR                                       ~
                      #12, CALMASTR                                      ~
                      #15, BOMMASTR                                      ~
                      #23, WCOUT                                         ~
                      #24, ENGMASTR                                      ~
                      #33, PIPIN                                         ~
                      #34, PIPOUT                                        ~
                      #35, PIPCROSS                                      ~
                      #40, SFMASTR                                       ~
                      #41, SFCUM2                                        ~

          dim                                                            ~
           bom$(490)3,                   /* WHICH BOM TO USE           */~
           bom$3,                                                        ~
           bompart$(20)25,                                               ~
           bombom$(20)3,                                                 ~
           bomqty$(20)10,                                                ~
           bomused$(20)10,                                               ~
           bommrk$(20)2,                                                 ~
           date$8,                       /* DATE FOR SCREEN DISPLAY    */~
           davail$(31)7,                 /* DISPLAYED AVAILABLE        */~
           dd$(31)3,                     /* DISPLAYED DAY OF WEEK NAME */~
           demcode$16,                   /* DEMAND CODE                */~
           demcompdate$8,                /* DESIRED COMPL DATE (YYMMDD)*/~
           demcus$9,                                                     ~
           demline$3,                    /*   LINE NUM FOR THIS DEMAND */~
           dempart$25,                                                   ~
           dempartdescr$34,              /* PART CODE                  */~
           demprior$1,                   /* PRIORITY DESIRED? (A-Z)    */~
           demquant$10,                  /* QUANTITY DESIRED           */~
           demsbom$3,                    /* WHICH BOM TO USE?          */~
           demsrte$3,                    /* WHICH WC ROUTE TO USE?     */~
           demstat$1,                                                    ~
           demtype$1,                    /* DEM TYPE 1-9 (SO,SF,PRO,PM)*/~
           demwc$4,                      /* WHICH WC? (FOR PM ONLY)    */~
           demwhse$3,                    /* WHICH WAREHOUSE?           */~
           dlp$6,                        /* DATE LAST PLANNED (YYMMDD) */~
           dow$(490)3,                   /*                            */~
           dpct$(31)7,                   /* DISPLAYED PCT USED         */~
           dplowkey$28,                                                  ~
           dused$(31)7,                  /* DISPLAYED USED             */~
           enddate$(20)8,                                                ~
           er$(31)17,                                                    ~
           errormsg$79,                  /* ERROR MESSAGE              */~
           file$8,                       /* File Name                  */~
           flymsg$78,                                                    ~
           hdr$(3)79,                                                    ~
           incl$(1)1,                                                    ~
           i$(24)80,                                                     ~
           i1$(24)80,                    /* SCREEN IMAGE               */~
           inlib$8,                                                      ~
           invol$6,                                                      ~
           inpmessage$79,                                                ~
           lfac$(31)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
           lockey$32,                                                    ~
           modate$(12)9,                 /* MONTH NAMES                */~
           note$(31)7,                   /* FOR HOLIDAYS               */~
           part$25,                                                      ~
           partdescr$34,                                                 ~
           pcd$6,                        /* PLANNED COMPL DATE (YYMMDD)*/~
           pipinaltplowkey$48,                                           ~
           pipinplowkey$19,                                              ~
           pipinrec$60,                                                  ~
           pipoutaltplowkey$37,                                          ~
           pippart$25,                                                   ~
           ppt$(20)25,                   /* PART CHECKED               */~
           quantity$(20)9,                                               ~
           reviewpart$25,                                                ~
           plowkey$50,                                                   ~
           readkey$50,                                                   ~
           rte$(490)3,                   /* WHICH ROUTE TO USE         */~
           see$(20)79,                                                   ~
           seebom$(31)3,                                                 ~
           seert$(31)3,                                                  ~
           shw$(20)79,                                                   ~
           startdate$(20)8,                                              ~
           startstat$1,                                                  ~
           tagnr$(20)19,                                                 ~
           tagnr$19,                                                     ~
           thispart$25,                                                  ~
           thispartdescr$34,                                             ~
           tdate$8,                      /* Temporary Date             */~
           title$79,                     /* SCREEN DISPLAY TEXT        */~
           udate$8,                      /* Temporary Date             */~
           wc$4,                                                         ~
           wcdescr$32,                                                   ~
           wdavail$(31)6,                /* DISPLAYED AVAILABLE        */~
           wdd$(31)3,                    /* DISPLAYED DAY OF WEEK NAME */~
           wdpct$(31)6,                  /* DISPLAYED PCT USED         */~
           wdused$(31)6,                 /* DISPLAYED USED             */~
           wnote$(31)7,                  /* FOR HOLIDAYS               */~
           yymmdd$(490)6                 /* DATE REFERENCE             */~

          dim                                                            ~
           atc%(490),                                                    ~
           avail%(490),                  /* HRS AVAIL ARRAY            */~
           cumf%(490),                   /* CUM FCST                   */~
           cursor%(2),                                                   ~
           curs%(2),                                                     ~
           f1%(64),                                                      ~
           fcst%(490),                                                   ~
           incl(1),                                                      ~
           lineend%(20),                                                 ~
           mm%(490),                     /*                            */~
           totcomp(20),                                                  ~
           totnet(20),                                                   ~
           totnonnet(20),                                                ~
           totorder(20),                                                 ~
           totplnd(20),                                                  ~
           totsls(20),                                                   ~
           used%(490),                   /* HRS USED ARRAY             */~
           wavail%(490),                                                 ~
           wused%(490),                                                  ~
           yy%(490)                      /*                            */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 8 ! HNYGENER ! Generic Part # file                      *~
            * #25 ! JBCROSS2 ! JOB TO BOM/RTE CROSS - JOB TRACKING      *~
            * #50 ! WORKFILE ! Workfile to sort various PIPIN's         *~
            *************************************************************

            select # 8, "HNYGENER",                                      ~
                        varc,     indexed,  recsize =   100,             ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41

            select #25, "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 94,                                    ~
                        keypos=29, keylen = 19,                          ~
                        alt key 1, keypos = 1 , keylen = 47,             ~
                            key 2, keypos = 48, keylen = 47

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =    79,             ~
                        keypos =   61, keylen = 19

            if open% = 0 then call "OPENCHCK" (#25, open%, 0%, 0%, " ")
            if open1% = 0 then call "OPENCHCK" (#8, open1%, 0%, 0%, " ")
            if open50% <> 0% then L09000
                call "GETNAMES" addr(#33, file$, inlib$, invol$)
                call "READFDR" addr(file$, inlib$, invol$, 0%, "RC",     ~
                                    recnbr%, err%)
                if err% <> 0% then recnbr% = 0%
                recnbr% = max(100%, recnbr% / 2%)
                call "WORKOPEN" (#50, "IO   ", recnbr%, open50%)
                if open50% = 0% then open50% = 1%

L09000: REM *************************************************************~
            *                                                           *~
            *     INITIALIZATION AND ENTRY                              *~
            *                                                           *~
            *************************************************************

            date$=date
            call "DATEFMT" (date$)

            part$,thispart$=reviewpart$

            modate$(01) = "JANUARY  "
            modate$(02) = "FEBRUARY "
            modate$(03) = "MARCH    "
            modate$(04) = "APRIL    "
            modate$(05) = "MAY      "
            modate$(06) = "JUNE     "
            modate$(07) = "JULY     "
            modate$(08) = "AUGUST   "
            modate$(09) = "SEPTEMBER"
            modate$(10) = "OCTOBER  "
            modate$(11) = "NOVEMBER "
            modate$(12) = "DECEMBER "

            if mode%=0% then mode%=6%

            hit=0
            gosub loadcalendar
            if hit > 0 then gosub exit_gracefully

            search str(yymmdd$(),1)=date to cursor%() step 6
            if cursor%(1)=0% then gosub exit_gracefully
            today%=(cursor%(1)/6%) + 1%

        REM *************************************************************~
            * START THE WHOLE MESS OFF WITH THE RIGHT SECTION.          *~
            *                                                           *~
            * NOTE THE GOSUB EXIT_GRACEFULLY AT END, JUST IN CASE       *~
            *************************************************************


            on mode% gosub     see_pip,            /* PLANNED POSITION */~
                               see_inv_sa,         /* SOURCE AND APPLIC*/~
                               see_wc_rte,         /* ROUTINGS         */~
                               see_ed,             /* EFFECTIVE DATE   */~
                               see_fcst_perf,      /* FORCAST/PERFORM. */~
                               jorpt,              /* PIP ACTIVITY     */~
                               demand_status,      /* DEMAND STATUS    */~
                               see_wc_cap,         /* WC CAPACITY      */~
                               see_bom             /* BILL OF MATERIAL */~

            gosub exit_gracefully


        REM *************************************************************~
            *                                                           *~
            * SEE BILL OF MATERIALS                                     *~
            *                                                           *~
            *************************************************************

L10060: see_bom
           errormsg$ = " "
           bom$ = " "
           part$=thispart$

L10110: accept                                                           ~
           at(01,02), ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INFORMATION PLEASE ~
        ~<<<<<<<<<<<<<<<<<<<<<<<<<<<<<",                                  ~
           at(10,02), "I NEED TO KNOW WHICH PART/BOM YOU WISH TO SEE",   ~
           at(12,02), "BILL OF MATERIALS FOR PART:",                     ~
           at(12,30), fac(hex(81)),     part$, ch(25),                   ~
           at(14,02), "BILL OF MATERIALS I.D. :",                        ~
           at(14,26), fac(hex(81)), bom$,  ch(03),                       ~
           at(16,02), fac(hex(94)), errormsg$, ch(79),                   ~
           at(22,02), "PF-KEYS ACTIVE:",                                 ~
           at(23,02), "(ENTER)TO EXAMINE BILL OF MATERIALS",             ~
           at(24,60), "(16)RETURN",                                      ~
                     keys(hex(001020)) , key(khht%)

                     if khht%  = 32% then exit_gracefully
                     if khht% <> 16% then L10241
                        errormsg$ = " "
                        if mode%=9% then exit_gracefully else return

L10241:     if part$ = " " then L10250
                call "READ100" (#4, part$, f1%(4%))
                if f1%(4%) = 1% then L10265
                     call "HNYGREF" (part$, #8, #4, f1%(4%))
                     if f1%(4%) <> 0% then L10265

L10250:     call "GETCODE" (#4, part$, partdescr$, 0%, 0, f1%(4%))
                 if f1%(4%) <> 0% then L10265
            errormsg$ = "Invalid Entry For Part"  :   goto L10110

L10265:     readkey$ = str(part$,,25%) & bom$
            incl(1%) = 0
            hdr$(2%) = "  Part Assemblies             Part Descriptions"
            hdr$(1%) = "  Existing BOMs For Part.  Use PF-1 To Select Ano~
        ~ther Part."
            hdr$(3%) = hex(ac) & "Select the Assembly Part And/Or BOM To ~
        ~Display.         Use PF-16 to Cancel."
            lockey$ = hex(06) & "Select the Part Assembly"
            REM *** Get Part & BOMID To Copy ***

            call "PLOWCODE" (#15, readkey$, lockey$, 8025%, -.30,f1%(15),~
                         hdr$(), 3.32, 57, incl(), incl$(), "Y", " ", #4)
                if f1%(15%) = 0% then L10060
            part$ = str(readkey$,,25%)
            bom$  = str(readkey$,26%,3%)

        REM *************************************************************~
            *        L O A D    B O M   F R O M   F I L E               *~
            *                                                           *~
            * THIS ROUTINE LOADS THE BILL OF MATERIALS     FROM THE     *~
            * FILE, RETURNS IF THERE IS NOT ONE.                        *~
            *************************************************************

            call "GETCODE" (#4, part$, partdescr$, 0%, 0, f1%(4%))
                 if f1%(4%) <> 0% then L10385
            errormsg$="Invalid Entry For Part": goto L10110

L10385:     call "SHOSTAT" ("Loading Bill Of Materials Information")
L10386:     plowkey$ = str(part$,,25%) & str(bom$,,3%) & "  0"
            bomlnecnt% = 0%
            init(" ") bompart$(),bomqty$(),bomused$(),bommrk$(), bombom$()
            bomlines%, v% = 0%
L10450:     call "PLOWNEXT" (#15, plowkey$, 28%, f1%(15%))
                 if f1%(15%) = 0% then L10630
                 if bomlines% > 17% then L10630
L10470:     v%, bomlines% = v% + 1%  :  bomlnecnt% = bomlnecnt% + 1%
            get #15, using L10530, bompart$(v%),                          ~
                                    qty, qtyu, bommrk$(v%), bombom$(v%)
                call "NUMPRINT" (qty,2,bomqty$(v%))
                call "NUMPRINT" (qtyu,2,bomused$(v%))
            goto L10450

L10530:     FMT CH(25), XX(31), PD(14,4), PD(14,4),POS(89),CH(2),XX(1),  ~
                CH(3)

L10630: REM PLOW EXIT
           line% = 0%
           if bomlines% > 0% then L10650
           errormsg$ = "Bom Not On File: " & bom$:goto L10110

L10650:    flymsg$=                                                      ~
        "COMPONENT                BOM        QUANTITY    TIMES USED      ~
        ~MARKER"

L10720: accept                                                           ~
               at (01,03),                                               ~
        "BILL OF MATERIALS#",                                            ~
               at (01,23), fac(hex(84)), bom$                   , ch( 3),~
               at (01,28),                                               ~
        "FOR PART#",                                                     ~
               at (01,39), fac(hex(84)), part$                  , ch(25),~
               at (02,39), fac(hex(8c)), partdescr$             , ch(34),~
               at (03,03), fac(hex(ac)), flymsg$                , ch(78),~
                                                                         ~
               at (04,02), fac(hex(84)), bompart$(line%+ 1%)    , ch(25),~
               at (05,02), fac(hex(84)), bompart$(line%+ 2%)    , ch(25),~
               at (06,02), fac(hex(84)), bompart$(line%+ 3%)    , ch(25),~
               at (07,02), fac(hex(84)), bompart$(line%+ 4%)    , ch(25),~
               at (08,02), fac(hex(84)), bompart$(line%+ 5%)    , ch(25),~
               at (09,02), fac(hex(84)), bompart$(line%+ 6%)    , ch(25),~
               at (10,02), fac(hex(84)), bompart$(line%+ 7%)    , ch(25),~
               at (11,02), fac(hex(84)), bompart$(line%+ 8%)    , ch(25),~
               at (12,02), fac(hex(84)), bompart$(line%+ 9%)    , ch(25),~
               at (13,02), fac(hex(84)), bompart$(line%+10%)    , ch(25),~
               at (14,02), fac(hex(84)), bompart$(line%+11%)    , ch(25),~
               at (15,02), fac(hex(84)), bompart$(line%+12%)    , ch(25),~
               at (16,02), fac(hex(84)), bompart$(line%+13%)    , ch(25),~
               at (17,02), fac(hex(84)), bompart$(line%+14%)    , ch(25),~
               at (18,02), fac(hex(84)), bompart$(line%+15%)    , ch(25),~
               at (19,02), fac(hex(84)), bompart$(line%+16%)    , ch(25),~
               at (20,02), fac(hex(84)), bompart$(line%+17%)    , ch(25),~
               at (21,02), fac(hex(84)), bompart$(line%+18%)    , ch(25),~
                                                                         ~
               at (04,33), fac(hex(84)), bomqty$ (line%+ 1%)    , ch(10),~
               at (05,33), fac(hex(84)), bomqty$ (line%+ 2%)    , ch(10),~
               at (06,33), fac(hex(84)), bomqty$ (line%+ 3%)    , ch(10),~
               at (07,33), fac(hex(84)), bomqty$ (line%+ 4%)    , ch(10),~
               at (08,33), fac(hex(84)), bomqty$ (line%+ 5%)    , ch(10),~
               at (09,33), fac(hex(84)), bomqty$ (line%+ 6%)    , ch(10),~
               at (10,33), fac(hex(84)), bomqty$ (line%+ 7%)    , ch(10),~
               at (11,33), fac(hex(84)), bomqty$ (line%+ 8%)    , ch(10),~
               at (12,33), fac(hex(84)), bomqty$ (line%+ 9%)    , ch(10),~
               at (13,33), fac(hex(84)), bomqty$ (line%+10%)    , ch(10),~
               at (14,33), fac(hex(84)), bomqty$ (line%+11%)    , ch(10),~
               at (15,33), fac(hex(84)), bomqty$ (line%+12%)    , ch(10),~
               at (16,33), fac(hex(84)), bomqty$ (line%+13%)    , ch(10),~
               at (17,33), fac(hex(84)), bomqty$ (line%+14%)    , ch(10),~
               at (18,33), fac(hex(84)), bomqty$ (line%+15%)    , ch(10),~
               at (19,33), fac(hex(84)), bomqty$ (line%+16%)    , ch(10),~
               at (20,33), fac(hex(84)), bomqty$ (line%+17%)    , ch(10),~
               at (21,33), fac(hex(84)), bomqty$ (line%+18%)    , ch(10),~
                                                                         ~
               at (04,45), fac(hex(84)), bomused$(line%+ 1%)    , ch(10),~
               at (05,45), fac(hex(84)), bomused$(line%+ 2%)    , ch(10),~
               at (06,45), fac(hex(84)), bomused$(line%+ 3%)    , ch(10),~
               at (07,45), fac(hex(84)), bomused$(line%+ 4%)    , ch(10),~
               at (08,45), fac(hex(84)), bomused$(line%+ 5%)    , ch(10),~
               at (09,45), fac(hex(84)), bomused$(line%+ 6%)    , ch(10),~
               at (10,45), fac(hex(84)), bomused$(line%+ 7%)    , ch(10),~
               at (11,45), fac(hex(84)), bomused$(line%+ 8%)    , ch(10),~
               at (12,45), fac(hex(84)), bomused$(line%+ 9%)    , ch(10),~
               at (13,45), fac(hex(84)), bomused$(line%+10%)    , ch(10),~
               at (14,45), fac(hex(84)), bomused$(line%+11%)    , ch(10),~
               at (15,45), fac(hex(84)), bomused$(line%+12%)    , ch(10),~
               at (16,45), fac(hex(84)), bomused$(line%+13%)    , ch(10),~
               at (17,45), fac(hex(84)), bomused$(line%+14%)    , ch(10),~
               at (18,45), fac(hex(84)), bomused$(line%+15%)    , ch(10),~
               at (19,45), fac(hex(84)), bomused$(line%+16%)    , ch(10),~
               at (20,45), fac(hex(84)), bomused$(line%+17%)    , ch(10),~
               at (21,45), fac(hex(84)), bomused$(line%+18%)    , ch(10),~
                                                                         ~
               at (04,68), fac(hex(84)), bommrk$ (line%+ 1%)    , ch(02),~
               at (05,68), fac(hex(84)), bommrk$ (line%+ 2%)    , ch(02),~
               at (06,68), fac(hex(84)), bommrk$ (line%+ 3%)    , ch(02),~
               at (07,68), fac(hex(84)), bommrk$ (line%+ 4%)    , ch(02),~
               at (08,68), fac(hex(84)), bommrk$ (line%+ 5%)    , ch(02),~
               at (09,68), fac(hex(84)), bommrk$ (line%+ 6%)    , ch(02),~
               at (10,68), fac(hex(84)), bommrk$ (line%+ 7%)    , ch(02),~
               at (11,68), fac(hex(84)), bommrk$ (line%+ 8%)    , ch(02),~
               at (12,68), fac(hex(84)), bommrk$ (line%+ 9%)    , ch(02),~
               at (13,68), fac(hex(84)), bommrk$ (line%+10%)    , ch(02),~
               at (14,68), fac(hex(84)), bommrk$ (line%+11%)    , ch(02),~
               at (15,68), fac(hex(84)), bommrk$ (line%+12%)    , ch(02),~
               at (16,68), fac(hex(84)), bommrk$ (line%+13%)    , ch(02),~
               at (17,68), fac(hex(84)), bommrk$ (line%+14%)    , ch(02),~
               at (18,68), fac(hex(84)), bommrk$ (line%+15%)    , ch(02),~
               at (19,68), fac(hex(84)), bommrk$ (line%+16%)    , ch(02),~
               at (20,68), fac(hex(84)), bommrk$ (line%+17%)    , ch(02),~
               at (21,68), fac(hex(84)), bommrk$ (line%+18%)    , ch(02),~
                                                                         ~
               at (04,28), fac(hex(84)), bombom$(line%+ 1%)     , ch( 3),~
               at (05,28), fac(hex(84)), bombom$(line%+ 2%)     , ch( 3),~
               at (06,28), fac(hex(84)), bombom$(line%+ 3%)     , ch( 3),~
               at (07,28), fac(hex(84)), bombom$(line%+ 4%)     , ch( 3),~
               at (08,28), fac(hex(84)), bombom$(line%+ 5%)     , ch( 3),~
               at (09,28), fac(hex(84)), bombom$(line%+ 6%)     , ch( 3),~
               at (10,28), fac(hex(84)), bombom$(line%+ 7%)     , ch( 3),~
               at (11,28), fac(hex(84)), bombom$(line%+ 8%)     , ch( 3),~
               at (12,28), fac(hex(84)), bombom$(line%+ 9%)     , ch( 3),~
               at (13,28), fac(hex(84)), bombom$(line%+10%)     , ch( 3),~
               at (14,28), fac(hex(84)), bombom$(line%+11%)     , ch( 3),~
               at (15,28), fac(hex(84)), bombom$(line%+12%)     , ch( 3),~
               at (16,28), fac(hex(84)), bombom$(line%+13%)     , ch( 3),~
               at (17,28), fac(hex(84)), bombom$(line%+14%)     , ch( 3),~
               at (18,28), fac(hex(84)), bombom$(line%+15%)     , ch( 3),~
               at (19,28), fac(hex(84)), bombom$(line%+16%)     , ch( 3),~
               at (20,28), fac(hex(84)), bombom$(line%+17%)     , ch( 3),~
               at (21,28), fac(hex(84)), bombom$(line%+18%)     , ch( 3),~
                                                                         ~
               at (23,02),                                               ~
           "(2)FIRST                    (5)NEXT                  (15)PRNT~
        ~SCRN  (16)RETURN",                                               ~
                                                                         ~
               keys (hex(02050f1020)),                                   ~
               key  (keyhit%)

               if keyhit% <> 15% then L11820
                  call "PRNTSCRN"
                  goto L10720
L11820:           if keyhit%  <>  2% then L11825
                     if bomlnecnt% < 18% then L10720
                          goto L10386
L11825:           if keyhit% <>  5% then L11833
                     if f1%(15%) = 0% then L10720
                     init(" ")  bompart$(), bomqty$(), bomused$(),       ~
                                bommrk$(), bombom$()
                     bomlines%, v% = 0%
                     goto L10470
L11833: REM       IF KEYHIT%  =  3% THEN LINE% = MAX(0, BOMLINES% - 18)
        REM       IF KEYHIT%  =  4% THEN LINE% = MAX(0, LINE% - 15)
        REM       IF KEYHIT%  =  5% THEN LINE% = MIN(LINE% + 15,         ~
                                                  MAX(0, BOMLINES% - 18))
        REM       IF KEYHIT%  =  6 THEN LINE% = MAX(0, LINE% - 1)
        REM       IF KEYHIT%  =  7 THEN LINE% = MIN(LINE% + 1,           ~
                                                  MAX(0, BOMLINES% - 18))
                  if keyhit% <> 16% then L11875
                       errormsg$ = " "
                       if mode%=8% then exit_gracefully else return
L11875:           if keyhit%  = 32% then exit_gracefully
                  goto L10720

        REM *************************************************************~
            *                                                           *~
            * SEE WORK CENTER ROUTING                                   *~
            *                                                           *~
            *************************************************************

L15060: see_wc_rte
           errormsg$ = " "
           rte$ = " "
           part$=thispart$

L15110: accept                                                           ~
           at(01,02), ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INFORMATION PLEASE ~
        ~<<<<<<<<<<<<<<<<<<<<<<<<<<<<<",                                  ~
           at(10,02), "I NEED TO KNOW WHICH PART YOU WANT ROUTE DETAILS  ~
        ~FOR & WHICH ROUTE",                                              ~
           at(12,02), "ROUTE DETAILS FOR PART:",                         ~
           at(12,26), fac(hex(81)),     part$, ch(25),                   ~
           at(14,02), "USING WORK CENTER ROUTE:",                        ~
           at(14,26), fac(hex(81)), rte$,  ch(03),                       ~
           at(16,02), fac(hex(94)), errormsg$, ch(79),                   ~
           at(22,02), "PF-KEYS ACTIVE:",                                 ~
           at(23,02), "(ENTER)TO EXAMINE ROUTE DETAILS",                 ~
           at(24,60), "(16)RETURN",                                      ~
                     keys(hex(001020)) , key(khht%)

                     if khht%  = 32% then exit_gracefully
                     if khht% <> 16% then L15249
                     if mode%  =  3% then exit_gracefully else return

L15249:     if part$ = " " then L15300
                call "READ100" (#4, part$, f1%(4))
                if f1%(4) = 1% then L15256
                     call "HNYGREF" (part$, #8, #4, f1%(4))
                     if f1%(4) = 0% then L15300

L15256:     readkey$ = str(part$,,25) & rte$
            incl(1) = 0
            hdr$(2) = "  Part Routings               Part Descriptions"
            hdr$(1) = "  Existing RTEs For Part.  Use PF-1 To Select Anot~
        ~her Part."
            hdr$(3) = hex(ac) & "Select the Assembly Part And/Or RTE To D~
        ~isplay.         Use PF-16 to Cancel."
            lockey$ = hex(06) & "Select the Part Assembly"
            REM *** Get Part & RTEID To Copy ***

            call "PLOWCODE" (#7, readkey$, lockey$, 8025%, -.001, f1%(7),~
                         hdr$(), 3.32, 57, incl(), incl$(), " ", " ", #4)
                if f1%(7) = 0% then L15060
            part$ = str(readkey$,,25)
            rte$  = str(readkey$,26%,3%)

L15300:     call "GETCODE" (#4, part$, " ", 0%, 0, f1%(4))
                if f1%(4) <> 0% then L15340
            errormsg$ = "Part Not On File":goto L15110

L15340:     plowkey$ = str(part$,,25) & str(rte$,,3)
            call "PLOWNEXT" (#7, plowkey$, 28%, f1%(7))
                if f1%(7) <> 0% then L15380
                errormsg$ = "Route Not On File":goto L15110
L15380:     call "RTEDSPLY" (part$, rte$, #7, #4)
            if mode%=3% then exit_gracefully
            return

        REM *************************************************************~
            *                                                           *~
            * SEE EFFECTIVE DATES                                       *~
            *                                                           *~
            *************************************************************

        see_ed
            errormsg$ = " "
            part$=thispart$

L20050: accept                                                           ~
           at(01,02), ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INFORMATION PLEASE ~
        ~<<<<<<<<<<<<<<<<<<<<<<<<<<<<<",                                  ~
           at(10,02), "I NEED TO KNOW WHICH PART YOU WANT EFFECTIVITY DAT~
        ~ES FOR",                                                         ~
           at(12,02), "EFFECTIVITY DATES FOR PART:",                     ~
           at(12,30), fac(hex(81)),     part$, ch(25),                   ~
           at(14,02), fac(hex(94)), errormsg$, ch(79),                   ~
           at(22,02), "PF-KEYS ACTIVE:",                                 ~
           at(23,02), "(ENTER)TO EXAMINE EFFECTIVITY DATES",             ~
           at(24,60), "(16)RETURN",                                      ~
                     keys(hex(001020)) , key(khht%)

                     if khht%  = 32% then exit_gracefully
                     if khht% <> 16% then L20126
                           if mode% = 4% then exit_gracefully else return

L20126:     if part$ = " " then L20140
                call "READ100" (#4, part$, f1%(4))
                if f1%(4) = 1% then L20150
                     call "HNYGREF" (part$, #8, #4, f1%(4))
                     if f1%(4) = 0% then L20140
                          call "READ100" (#4, part$, f1%(4))
                          if f1%(4) = 1% then L20150

L20140:    call "GETCODE" (#4, part$, " ", 0%, 0, f1%(4))
                if f1%(4) = 0% then L20050
L20150:    call "SHOSTAT" ("Loading Engineering Data")

           rte$(), bom$() = " "
           call "READ100" (#24, str(part$,,25) & "1001" , f1%(24))
                     if f1%(24) <> 0% then L20190
           errormsg$ = "Effectivity Record Not On File":goto L20050

L20190:    get #24, using L20195, str(bom$(),,1470)
L20195:    FMT XX(29), CH(1470)
           for i%=1% to 490%
           call "READ100"(#15,str(part$,,25) &str(bom$(i%),,3) & "  0",  ~
                                                   f1%(15))
            if f1%(15)=0% then L20222
            get #15, using L20220, rte$(i%)
L20220:         FMT POS(87), CH(3)
L20222:     next i%

        REM THIS ALLOWS THE USER TO SEE EFFECTIVE DATES FOR BOM & RTE'S

L20250:     tdate$ = date$
            call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$,5%,2%) to mo%
            convert str(udate$,1%,4%) to yr%
L20260:     f% = 0%
            for i% = 1% to 490%
            if yy%(i%) < yr% then L20295
            if yy%(i%) > yr% then L20305
            if mm%(i%) < mo% then L20295
            if mm%(i%) > mo% then L20305
            if f% =  0% then f% = i%
L20295:         next i%

L20305:     l% = i% - 1%

            j% = 0%
            dd$(), seebom$(), seert$(), er$() = " "

            for i% = f% to l%
                j% = j% + 1%
            seebom$(j%) = bom$(i%)
            seert$(j%) = rte$(i%)
            dd$(j%) = dow$(i%)
                next i%


           init(hex(86)) lfac$()

L20380: accept                                                           ~
               at (01,02),                                               ~
        "BOM & WC EFFECTIVE: ",                                          ~
               at (01,22), fac(hex(84)), part$                  , ch(25),~
               at (01,48),                                               ~
        "MONTH OF:",                                                     ~
               at (01,57), fac(hex(84)), modate$(mo%)           , ch(09),~
               at (01,68), fac(hex(84)), yr%, pic(####),                 ~
               at(02,16), fac(hex(84)), dempartdescr$,        ch(34),    ~
               at (03,03),                                               ~
        "DAY       BOM    RTE                  ! DAY        BOM    RTE   ~
        ~    ",                                                          ~
               at (04,03),                                               ~
        "01",                                                            ~
               at (04,41),                                               ~
        "!  16",                                                         ~
               at (05,03),                                               ~
        "02",                                                            ~
               at (05,41),                                               ~
        "!  17",                                                         ~
               at (06,03),                                               ~
        "03",                                                            ~
               at (06,41),                                               ~
        "!  18",                                                         ~
               at (07,03),                                               ~
        "04",                                                            ~
               at (07,41),                                               ~
        "!  19",                                                         ~
               at (08,03),                                               ~
        "05",                                                            ~
               at (08,41),                                               ~
        "!  20",                                                         ~
               at (09,03),                                               ~
        "06",                                                            ~
               at (09,41),                                               ~
        "!  21",                                                         ~
               at (10,03),                                               ~
        "07",                                                            ~
               at (10,41),                                               ~
        "!  22",                                                         ~
               at (11,03),                                               ~
        "08",                                                            ~
               at (11,41),                                               ~
        "!  23",                                                         ~
               at (12,03),                                               ~
        "09",                                                            ~
               at (12,41),                                               ~
        "!  24",                                                         ~
               at (13,03),                                               ~
        "10",                                                            ~
               at (13,41),                                               ~
        "!  25",                                                         ~
               at (14,03),                                               ~
        "11",                                                            ~
               at (14,41),                                               ~
        "!  26",                                                         ~
               at (15,03),                                               ~
        "12",                                                            ~
               at (15,41),                                               ~
        "!  27",                                                         ~
               at (16,03),                                               ~
        "13",                                                            ~
               at (16,41),                                               ~
        "!  28",                                                         ~
               at (17,03),                                               ~
        "14",                                                            ~
               at (17,41),                                               ~
        "!  29",                                                         ~
               at (18,03),                                               ~
        "15",                                                            ~
               at (18,41),                                               ~
        "!  30",                                                         ~
               at (19,41),                                               ~
        "!  31",                                                         ~
               at (23,02),                                               ~
        "PF keys:            (3)FIRST MO  (4)PREV MO  (5)NEXT MO",       ~
               at (24,02),                                               ~
        "                                      (15)PRINT SCREEN     (16)R~
        ~ETURN",                                                          ~
           at(04,13), fac(lfac$(01)), seebom$(01)           ,ch(03)     ,~
           at(05,13), fac(lfac$(02)), seebom$(02)           ,ch(03)     ,~
           at(06,13), fac(lfac$(03)), seebom$(03)           ,ch(03)     ,~
           at(07,13), fac(lfac$(04)), seebom$(04)           ,ch(03)     ,~
           at(08,13), fac(lfac$(05)), seebom$(05)           ,ch(03)     ,~
           at(09,13), fac(lfac$(06)), seebom$(06)           ,ch(03)     ,~
           at(10,13), fac(lfac$(07)), seebom$(07)           ,ch(03)     ,~
           at(11,13), fac(lfac$(08)), seebom$(08)           ,ch(03)     ,~
           at(12,13), fac(lfac$(09)), seebom$(09)           ,ch(03)     ,~
           at(13,13), fac(lfac$(10)), seebom$(10)           ,ch(03)     ,~
           at(14,13), fac(lfac$(11)), seebom$(11)           ,ch(03)     ,~
           at(15,13), fac(lfac$(12)), seebom$(12)           ,ch(03)     ,~
           at(16,13), fac(lfac$(13)), seebom$(13)           ,ch(03)     ,~
           at(17,13), fac(lfac$(14)), seebom$(14)           ,ch(03)     ,~
           at(18,13), fac(lfac$(15)), seebom$(15)           ,ch(03)     ,~
           at(04,54), fac(lfac$(16)), seebom$(16)           ,ch(03)     ,~
           at(05,54), fac(lfac$(17)), seebom$(17)           ,ch(03)     ,~
           at(06,54), fac(lfac$(18)), seebom$(18)           ,ch(03)     ,~
           at(07,54), fac(lfac$(19)), seebom$(19)           ,ch(03)     ,~
           at(08,54), fac(lfac$(20)), seebom$(20)           ,ch(03)     ,~
           at(09,54), fac(lfac$(21)), seebom$(21)           ,ch(03)     ,~
           at(10,54), fac(lfac$(22)), seebom$(22)           ,ch(03)     ,~
           at(11,54), fac(lfac$(23)), seebom$(23)           ,ch(03)     ,~
           at(12,54), fac(lfac$(24)), seebom$(24)           ,ch(03)     ,~
           at(13,54), fac(lfac$(25)), seebom$(25)           ,ch(03)     ,~
           at(14,54), fac(lfac$(26)), seebom$(26)           ,ch(03)     ,~
           at(15,54), fac(lfac$(27)), seebom$(27)           ,ch(03)     ,~
           at(16,54), fac(lfac$(28)), seebom$(28)           ,ch(03)     ,~
           at(17,54), fac(lfac$(29)), seebom$(29)           ,ch(03)     ,~
           at(18,54), fac(lfac$(30)), seebom$(30)           ,ch(03)     ,~
           at(19,54), fac(lfac$(31)), seebom$(31)           ,ch(03)     ,~
           at(04,20), fac(lfac$( 1)), seert$ (01)           ,ch(03)     ,~
           at(05,20), fac(lfac$( 2)), seert$ (02)           ,ch(03)     ,~
           at(06,20), fac(lfac$( 3)), seert$ (03)           ,ch(03)     ,~
           at(07,20), fac(lfac$( 4)), seert$ (04)           ,ch(03)     ,~
           at(08,20), fac(lfac$( 5)), seert$ (05)           ,ch(03)     ,~
           at(09,20), fac(lfac$( 6)), seert$ (06)           ,ch(03)     ,~
           at(10,20), fac(lfac$( 7)), seert$ (07)           ,ch(03)     ,~
           at(11,20), fac(lfac$( 8)), seert$ (08)           ,ch(03)     ,~
           at(12,20), fac(lfac$( 9)), seert$ (09)           ,ch(03)     ,~
           at(13,20), fac(lfac$(10)), seert$ (10)           ,ch(03)     ,~
           at(14,20), fac(lfac$(11)), seert$ (11)           ,ch(03)     ,~
           at(15,20), fac(lfac$(12)), seert$ (12)           ,ch(03)     ,~
           at(16,20), fac(lfac$(13)), seert$ (13)           ,ch(03)     ,~
           at(17,20), fac(lfac$(14)), seert$ (14)           ,ch(03)     ,~
           at(18,20), fac(lfac$(15)), seert$ (15)           ,ch(03)     ,~
           at(04,61), fac(lfac$(16)), seert$ (16)           ,ch(03)     ,~
           at(05,61), fac(lfac$(17)), seert$ (17)           ,ch(03)     ,~
           at(06,61), fac(lfac$(18)), seert$ (18)           ,ch(03)     ,~
           at(07,61), fac(lfac$(19)), seert$ (19)           ,ch(03)     ,~
           at(08,61), fac(lfac$(20)), seert$ (20)           ,ch(03)     ,~
           at(09,61), fac(lfac$(21)), seert$ (21)           ,ch(03)     ,~
           at(10,61), fac(lfac$(22)), seert$ (22)           ,ch(03)     ,~
           at(11,61), fac(lfac$(23)), seert$ (23)           ,ch(03)     ,~
           at(12,61), fac(lfac$(24)), seert$ (24)           ,ch(03)     ,~
           at(13,61), fac(lfac$(25)), seert$ (25)           ,ch(03)     ,~
           at(14,61), fac(lfac$(26)), seert$ (26)           ,ch(03)     ,~
           at(15,61), fac(lfac$(27)), seert$ (27)           ,ch(03)     ,~
           at(16,61), fac(lfac$(28)), seert$ (28)           ,ch(03)     ,~
           at(17,61), fac(lfac$(29)), seert$ (29)           ,ch(03)     ,~
           at(18,61), fac(lfac$(30)), seert$ (30)           ,ch(03)     ,~
           at(19,61), fac(lfac$(31)), seert$ (31)           ,ch(03)     ,~
            at(04,06), fac(hex(84)), dd$(01), ch(3),                     ~
            at(05,06), fac(hex(84)), dd$(02), ch(3),                     ~
            at(06,06), fac(hex(84)), dd$(03), ch(3),                     ~
            at(07,06), fac(hex(84)), dd$(04), ch(3),                     ~
            at(08,06), fac(hex(84)), dd$(05), ch(3),                     ~
            at(09,06), fac(hex(84)), dd$(06), ch(3),                     ~
            at(10,06), fac(hex(84)), dd$(07), ch(3),                     ~
            at(11,06), fac(hex(84)), dd$(08), ch(3),                     ~
            at(12,06), fac(hex(84)), dd$(09), ch(3),                     ~
            at(13,06), fac(hex(84)), dd$(10), ch(3),                     ~
            at(14,06), fac(hex(84)), dd$(11), ch(3),                     ~
            at(15,06), fac(hex(84)), dd$(12), ch(3),                     ~
            at(16,06), fac(hex(84)), dd$(13), ch(3),                     ~
            at(17,06), fac(hex(84)), dd$(14), ch(3),                     ~
            at(18,06), fac(hex(84)), dd$(15), ch(3),                     ~
            at(04,47), fac(hex(84)), dd$(16), ch(3),                     ~
            at(05,47), fac(hex(84)), dd$(17), ch(3),                     ~
            at(06,47), fac(hex(84)), dd$(18), ch(3),                     ~
            at(07,47), fac(hex(84)), dd$(19), ch(3),                     ~
            at(08,47), fac(hex(84)), dd$(20), ch(3),                     ~
            at(09,47), fac(hex(84)), dd$(21), ch(3),                     ~
            at(10,47), fac(hex(84)), dd$(22), ch(3),                     ~
            at(11,47), fac(hex(84)), dd$(23), ch(3),                     ~
            at(12,47), fac(hex(84)), dd$(24), ch(3),                     ~
            at(13,47), fac(hex(84)), dd$(25), ch(3),                     ~
            at(14,47), fac(hex(84)), dd$(26), ch(3),                     ~
            at(15,47), fac(hex(84)), dd$(27), ch(3),                     ~
            at(16,47), fac(hex(84)), dd$(28), ch(3),                     ~
            at(17,47), fac(hex(84)), dd$(29), ch(3),                     ~
            at(18,47), fac(hex(84)), dd$(30), ch(3),                     ~
            at(19,47), fac(hex(84)), dd$(31), ch(3),                     ~
            at(04,24), fac(hex(84)), er$(01), ch(11),                    ~
            at(05,24), fac(hex(84)), er$(02), ch(11),                    ~
            at(06,24), fac(hex(84)), er$(03), ch(11),                    ~
            at(07,24), fac(hex(84)), er$(04), ch(11),                    ~
            at(08,24), fac(hex(84)), er$(05), ch(11),                    ~
            at(09,24), fac(hex(84)), er$(24), ch(11),                    ~
            at(10,24), fac(hex(84)), er$(07), ch(11),                    ~
            at(11,24), fac(hex(84)), er$(08), ch(11),                    ~
            at(12,24), fac(hex(84)), er$(09), ch(11),                    ~
            at(13,24), fac(hex(84)), er$(10), ch(11),                    ~
            at(14,24), fac(hex(84)), er$(11), ch(11),                    ~
            at(15,24), fac(hex(84)), er$(12), ch(11),                    ~
            at(16,24), fac(hex(84)), er$(13), ch(11),                    ~
            at(17,24), fac(hex(84)), er$(14), ch(11),                    ~
            at(18,24), fac(hex(84)), er$(15), ch(11),                    ~
            at(04,65), fac(hex(84)), er$(16), ch(11),                    ~
            at(05,65), fac(hex(84)), er$(17), ch(11),                    ~
            at(06,65), fac(hex(84)), er$(18), ch(11),                    ~
            at(07,65), fac(hex(84)), er$(19), ch(11),                    ~
            at(08,65), fac(hex(84)), er$(20), ch(11),                    ~
            at(09,65), fac(hex(84)), er$(21), ch(11),                    ~
            at(10,65), fac(hex(84)), er$(22), ch(11),                    ~
            at(11,65), fac(hex(84)), er$(23), ch(11),                    ~
            at(12,65), fac(hex(84)), er$(24), ch(11),                    ~
            at(13,65), fac(hex(84)), er$(25), ch(11),                    ~
            at(14,65), fac(hex(84)), er$(26), ch(11),                    ~
            at(15,65), fac(hex(84)), er$(27), ch(11),                    ~
            at(16,65), fac(hex(84)), er$(28), ch(11),                    ~
            at(17,65), fac(hex(84)), er$(29), ch(11),                    ~
            at(18,65), fac(hex(84)), er$(30), ch(11),                    ~
            at(19,65), fac(hex(84)), er$(31), ch(11),                    ~
                                                                         ~
                keys(hex(0304050f1020)), key(keyhit%)

            er$() = " "
            if keyhit%  = 32% then exit_gracefully
            if keyhit% <> 16% then L21440
                    if mode%=4% then exit_gracefully else return
L21440:     if keyhit%  = 3% then L20250
            if keyhit% <> 4% then L21470
                if f% = 1% then L20380
                mo% = mm%(f%-1)
                yr% = yy%(f%-1)
                goto L20260
L21470:     if keyhit% <> 5% then L21495
                if l% = 490% then L20380
                mo% = mm%(l%+1)
                yr% = yy%(l%+1)
                goto L20260
L21495:     if keyhit% <> 15% then L20380
                call "PRNTSCRN"
                goto L20380

        REM *************************************************************~
            *                                                           *~
            *   S E E    P I P   I N F O R M A T I O N                  *~
            *                                                           *~
            *************************************************************

        see_pip
           errormsg$ = " "
L24080: accept                                                           ~
           at(01,02), ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INFORMATION PLEASE ~
        ~<<<<<<<<<<<<<<<<<<<<<<<<<<<<<",                                  ~
           at(05,02), "I NEED TO KNOW WHICH PART YOU WANT PLANNED INVENTO~
        ~RY POSITION DETAILS FOR.",                                       ~
           at(06,02), "THE PART YOU WERE JUST WORKING ON IS SHOWN BELOW. ~
        ~JUST TYPE IN THE PART ",                                         ~
           at(07,02), "YOU WANT AND PRESS ENTER TO GET THE DETAILS, OR HI~
        ~T (16) TO RETURN.",                                              ~
           at(08,02), "(PRESS PF8 TO SEE/SELECT FROM ATERNATES FOR THIS P~
        ~ART)",                                                           ~
           at(10,14), "PART:",                                           ~
           at(10,20), fac(hex(81)), thispart$, ch(25),                   ~
           at(11,14), "ALLOW VARIABLE ATC HORIZON:",                     ~
           at(11,42), fac(hex(81)), atch$, ch(3),                        ~
           at(13,02), fac(hex(94)), errormsg$, ch(79),                   ~
                     keys(hex(00081020)) , key(kh%)

           if kh%  =  8% then L24271
           if kh%  = 32% then exit_gracefully
           if kh% <> 16% then L24271
               if mode%=1% then exit_gracefully  else return

L24271:     if thispart$ = " " then L24280
                call "READ100" (#4, thispart$, f1%(4))
                if f1%(4) = 1% then L24301
                     call "HNYGREF" (thispart$, #8, #4, f1%(4))
                     if f1%(4) = 0% then L24280
                          call "READ100" (#4, thispart$, f1%(4))
                          if f1%(4) = 1% then L24301

L24280:     call "GETCODE" (#4, thispart$, " ", 0%, 0, f1%(4))
            if f1%(4) = 0% then L24080

L24301:     if kh% <> 8% then L24310
            call "PLOWCODE" (#16, thispart$, part$, 25%, 0.25, f1%(16))
               if f1%(16%) <> 0% then thispart$ = part$

L24310:     if str(atch$,1,1) = "Y" then atch$ = "YES" else atch$ = "N0"

L24330: REM *************************************************************~
           *    L O A D   PIP   I N F O R M A T I O N                   *~
           **************************************************************

           part$=thispart$
           mat avail% = zer : mat used% = zer : mat atc% = zer
           mat cumf% = zer
           j% = 0%
           call "PIPFLAGS"(thispart$, today%, today%, 0, #2, #41)
           call "READ100" (#2, thispart$, f1%(2))
           if f1%(02) <> 0% then L24450
              errormsg$ = "PART NOT FOUND IN PIP":goto L24080

L24450:    call "SHOSTAT" ("Gathering PIP Data, One Moment Please")
           get #2, using L24530, str(s$,,1), oh, ss, moq, pz, type%, lt%, ~
                                atch%
                call "MXFL4GT" addr(#2, 26%, avail%(1%), 490%)
           atch% = mod(atch%, 1000%)
           if atch$ = "YES" then atch1% = atch% else atch1% = 999%

           REM WE ARE REUSING ARRAY NAMES TO SAVE SEG-2 SPACE SO WATCHIT

L24530:    FMT       CH(01),   /* STATUS OF RECORD           */          ~
                     XX(25),   /* SKIP PART                  */          ~
                     XX(1960), /* PIP ARRAY                  */          ~
                 4*PD(14,4),   /* ON-HAND, SAF-STCK, MOQ     */          ~
                    3*BI(2)    /* TYPE, LEADTIME, ATC HORIZ. */

           call "DESCRIBE" (#4, thispart$, partdescr$, 1%,   f1%(4))
           toggle% = 0%
           title$ = "DAY     ATC-1   SHELF CUMFCST     PIP  ! DAY     ATC~
        ~-1   SHELF CUMFCST     PIP"

           init (" ") str(s$,2)
           if pos("23"=str(s$,1,1))<>0% then str(s$,2)="Surplus       "
           if pos("45"=str(s$,1,1))<>0% then str(s$,2)="Safety Stock  "
           if pos("89"=str(s$,1,1))<>0% then str(s$,2)="Critical      "

           call "READ100" (#41, thispart$, f1%(41))

           if f1%(41) <> 1% then L24720
           call "MXFL4GT" addr(#41, 25%, cumf%(1%), 490%)
*         GET #41, USING 24700  , CUMF%()    /* CUM SLS FCST ARRAY  */
*         FMT XX(25), 490*BI(4)


L24720:    mat atc% = avail%
           for kkkk% = min(489%, today%+atch1%) to today% step -1%
           atc%(kkkk%) = min(avail%(kkkk%), atc%(kkkk% + 1%))
           next kkkk%

           if today% = 1% then L24760
           for kkkk% = today%-1% to 1% step -1%
                atc%(kkkk%) = min(avail%(kkkk%), atc%(today%))
           next kkkk%

L24760:    for kkkk% = 1% to 490%
                wused%(kkkk%) = avail%(kkkk%) - max(0%, cumf%(kkkk%))
           next kkkk%

           mat used% = wused%
           for kkkk% = min(489%, today%+atch1%) to today% step -1%
                used%(kkkk%) = min(wused%(kkkk%), used%(kkkk% + 1%))
           next kkkk%

           if today% = 1% then seepipdata
           for kkkk% = today%-1% to 1% step -1%
                used%(kkkk%) = min(wused%(kkkk%), used%(today%))
           next kkkk%

        REM *************************************************************~
           *    S E E   PLANNED INVENTORY     B Y   M O N T H           *~
           **************************************************************

        seepipdata
           tdate$ = date
           call "DATEFMT" (tdate$, 0%, udate$)
           convert str(udate$,5%,2%) to mt%
           convert str(udate$,1%,4%) to yr%
L24930:     j%, f% = 0%
            for i% = 1% to 490%
                 if yy%(i%) < yr% then L25000
                 if yy%(i%) > yr% then L25020
                 if mm%(i%) < mt% then L25000
                 if mm%(i%) > mt% then L25020
                 if f% =  0% then f% = i%
L25000:     next i%

L25020:     l% = i% - 1%

            dd$(), davail$(), dused$(), dpct$(), note$() = " "
            j% = 0%
            for i% = f% to l%
                j% = j% + 1%
           convert atc%  (i%) to  davail$(j%), pic(-######)
           if cumf%(i%) > 0% then                                        ~
            convert (avail%(i%) - cumf%(i%)) to dused$(j%), pic(-######) ~
                     else convert avail%(i%) to dused$(j%), pic(-######)
           if cumf%(i%) > 0% then                                        ~
                           convert  cumf%(i%) to dpct$(j%), pic(#######) ~
                      else dpct$(j%) = " "
           convert avail%(i%) to note$(j%), pic(-######)
           dd$(j%) = str(dow$(i%),1,1)
                next i%


L25210:     inpmessage$ = "Position Cursor and PF9: Daily Details.  PF14:~
        ~ See General PIP Details."
           init(hex(86)) lfac$()

        accept                                                           ~
               at (01,02),                                               ~
        "INVENTORY PLANS FOR:",                                          ~
               at (01,22), fac(hex(84)), thispart$              , ch(25),~
               at (01,48),                                               ~
        "MONTH OF:",                                                     ~
               at (01,57), fac(hex(84)), modate$(mt%)           , ch(09),~
               at (01,68), fac(hex(84)), yr%                 , pic(####),~
               at (02,22), fac(hex(84)), partdescr$             , ch(34),~
               at (03,02), fac(hex(ac)), title$                 , ch(79),~
               at (04,02), "01",                                         ~
               at (05,02), "02",                                         ~
               at (06,02), "03",                                         ~
               at (07,02), "04",                                         ~
               at (08,02), "05",                                         ~
               at (09,02), "06",                                         ~
               at (10,02), "07",                                         ~
               at (11,02), "08",                                         ~
               at (12,02), "09",                                         ~
               at (13,02), "10",                                         ~
               at (14,02), "11",                                         ~
               at (15,02), "12",                                         ~
               at (16,02), "13",                                         ~
               at (17,02), "14",                                         ~
               at (18,02), "15",                                         ~
               at (04,41), "!  16",                                      ~
               at (05,41), "!  17",                                      ~
               at (06,41), "!  18",                                      ~
               at (07,41), "!  19",                                      ~
               at (08,41), "!  20",                                      ~
               at (09,41), "!  21",                                      ~
               at (10,41), "!  22",                                      ~
               at (11,41), "!  23",                                      ~
               at (12,41), "!  24",                                      ~
               at (13,41), "!  25",                                      ~
               at (14,41), "!  26",                                      ~
               at (15,41), "!  27",                                      ~
               at (16,41), "!  28",                                      ~
               at (17,41), "!  29",                                      ~
               at (18,41), "!  30",                                      ~
               at (19,41), "!  31",                                      ~
                                                                         ~
               at (20,02), "CURRENT:",                                   ~
               at(20,11), "On Hand:",                                    ~
               at(20,20), fac(hex(84)), oh,     pic(-#######.##),        ~
               at(20,32), "Safety Stk:",                                 ~
               at(20,44), fac(hex(84)), ss,     pic(#######.##),         ~
               at(20,55), "L/T:",                                        ~
               at(20,61), fac(hex(84)), lt%,    pic(###),                ~
               at(20,65), fac(hex(84)), str(s$,2)               , ch(15),~
               at(21,11), "Plan MOQ:",                                   ~
               at(21,21), fac(hex(84)), moq,    pic(#######.##),         ~
               at(21,32), "Pansize:",                                    ~
               at(21,44), fac(hex(84)), pz,     pic(#######.##),         ~
               at(21,55), "Type:",                                       ~
               at(21,61), fac(hex(84)), type%,  pic(000),                ~
               at(21,65), "ATC Horiz:",                                  ~
               at(21,76), fac(hex(84)), atch%,  pic(###),                ~
               at(22,02), fac(hex(ac)), inpmessage$             , ch(79),~
                                                                         ~
              at(23,02),                                                 ~
        "(1)New Part     (4)Prev Month                    (11)Srces/Appls~
        ~  (15)Prnt Scr",                                                 ~
              at(24,02),                                                 ~
        "(3)First Month  (5)Next Month  (10)Switch ATC    (12)Sales/Fcsts~
        ~  (16)RETURN",                                                   ~
                                                                         ~
                                                                         ~
           at(04,07), fac(lfac$(01)), davail$(01)           ,ch(07)     ,~
           at(05,07), fac(lfac$(02)), davail$(02)           ,ch(07)     ,~
           at(06,07), fac(lfac$(03)), davail$(03)           ,ch(07)     ,~
           at(07,07), fac(lfac$(04)), davail$(04)           ,ch(07)     ,~
           at(08,07), fac(lfac$(05)), davail$(05)           ,ch(07)     ,~
           at(09,07), fac(lfac$(06)), davail$(06)           ,ch(07)     ,~
           at(10,07), fac(lfac$(07)), davail$(07)           ,ch(07)     ,~
           at(11,07), fac(lfac$(08)), davail$(08)           ,ch(07)     ,~
           at(12,07), fac(lfac$(09)), davail$(09)           ,ch(07)     ,~
           at(13,07), fac(lfac$(10)), davail$(10)           ,ch(07)     ,~
           at(14,07), fac(lfac$(11)), davail$(11)           ,ch(07)     ,~
           at(15,07), fac(lfac$(12)), davail$(12)           ,ch(07)     ,~
           at(16,07), fac(lfac$(13)), davail$(13)           ,ch(07)     ,~
           at(17,07), fac(lfac$(14)), davail$(14)           ,ch(07)     ,~
           at(18,07), fac(lfac$(15)), davail$(15)           ,ch(07)     ,~
           at(04,49), fac(lfac$(16)), davail$(16)           ,ch(07)     ,~
           at(05,49), fac(lfac$(17)), davail$(17)           ,ch(07)     ,~
           at(06,49), fac(lfac$(18)), davail$(18)           ,ch(07)     ,~
           at(07,49), fac(lfac$(19)), davail$(19)           ,ch(07)     ,~
           at(08,49), fac(lfac$(20)), davail$(20)           ,ch(07)     ,~
           at(09,49), fac(lfac$(21)), davail$(21)           ,ch(07)     ,~
           at(10,49), fac(lfac$(22)), davail$(22)           ,ch(07)     ,~
           at(11,49), fac(lfac$(23)), davail$(23)           ,ch(07)     ,~
           at(12,49), fac(lfac$(24)), davail$(24)           ,ch(07)     ,~
           at(13,49), fac(lfac$(25)), davail$(25)           ,ch(07)     ,~
           at(14,49), fac(lfac$(26)), davail$(26)           ,ch(07)     ,~
           at(15,49), fac(lfac$(27)), davail$(27)           ,ch(07)     ,~
           at(16,49), fac(lfac$(28)), davail$(28)           ,ch(07)     ,~
           at(17,49), fac(lfac$(29)), davail$(29)           ,ch(07)     ,~
           at(18,49), fac(lfac$(30)), davail$(30)           ,ch(07)     ,~
           at(19,49), fac(lfac$(31)), davail$(31)           ,ch(07)     ,~
           at(04,15), fac(hex(84))  , dused$ (01)           ,ch(07)     ,~
           at(05,15), fac(hex(84))  , dused$ (02)           ,ch(07)     ,~
           at(06,15), fac(hex(84))  , dused$ (03)           ,ch(07)     ,~
           at(07,15), fac(hex(84))  , dused$ (04)           ,ch(07)     ,~
           at(08,15), fac(hex(84))  , dused$ (05)           ,ch(07)     ,~
           at(09,15), fac(hex(84))  , dused$ (06)           ,ch(07)     ,~
           at(10,15), fac(hex(84))  , dused$ (07)           ,ch(07)     ,~
           at(11,15), fac(hex(84))  , dused$ (08)           ,ch(07)     ,~
           at(12,15), fac(hex(84))  , dused$ (09)           ,ch(07)     ,~
           at(13,15), fac(hex(84))  , dused$ (10)           ,ch(07)     ,~
           at(14,15), fac(hex(84))  , dused$ (11)           ,ch(07)     ,~
           at(15,15), fac(hex(84))  , dused$ (12)           ,ch(07)     ,~
           at(16,15), fac(hex(84))  , dused$ (13)           ,ch(07)     ,~
           at(17,15), fac(hex(84))  , dused$ (14)           ,ch(07)     ,~
           at(18,15), fac(hex(84))  , dused$ (15)           ,ch(07)     ,~
           at(04,57), fac(hex(84))  , dused$ (16)           ,ch(07)     ,~
           at(05,57), fac(hex(84))  , dused$ (17)           ,ch(07)     ,~
           at(06,57), fac(hex(84))  , dused$ (18)           ,ch(07)     ,~
           at(07,57), fac(hex(84))  , dused$ (19)           ,ch(07)     ,~
           at(08,57), fac(hex(84))  , dused$ (20)           ,ch(07)     ,~
           at(09,57), fac(hex(84))  , dused$ (21)           ,ch(07)     ,~
           at(10,57), fac(hex(84))  , dused$ (22)           ,ch(07)     ,~
           at(11,57), fac(hex(84))  , dused$ (23)           ,ch(07)     ,~
           at(12,57), fac(hex(84))  , dused$ (24)           ,ch(07)     ,~
           at(13,57), fac(hex(84))  , dused$ (25)           ,ch(07)     ,~
           at(14,57), fac(hex(84))  , dused$ (26)           ,ch(07)     ,~
           at(15,57), fac(hex(84))  , dused$ (27)           ,ch(07)     ,~
           at(16,57), fac(hex(84))  , dused$ (28)           ,ch(07)     ,~
           at(17,57), fac(hex(84))  , dused$ (29)           ,ch(07)     ,~
           at(18,57), fac(hex(84))  , dused$ (30)           ,ch(07)     ,~
           at(19,57), fac(hex(84))  , dused$ (31)           ,ch(07)     ,~
           at(04,24), fac(hex(84))  , dpct$  (01)           ,ch(07)     ,~
           at(05,24), fac(hex(84))  , dpct$  (02)           ,ch(07)     ,~
           at(06,24), fac(hex(84))  , dpct$  (03)           ,ch(07)     ,~
           at(07,24), fac(hex(84))  , dpct$  (04)           ,ch(07)     ,~
           at(08,24), fac(hex(84))  , dpct$  (05)           ,ch(07)     ,~
           at(09,24), fac(hex(84))  , dpct$  (06)           ,ch(07)     ,~
           at(10,24), fac(hex(84))  , dpct$  (07)           ,ch(07)     ,~
           at(11,24), fac(hex(84))  , dpct$  (08)           ,ch(07)     ,~
           at(12,24), fac(hex(84))  , dpct$  (09)           ,ch(07)     ,~
           at(13,24), fac(hex(84))  , dpct$  (10)           ,ch(07)     ,~
           at(14,24), fac(hex(84))  , dpct$  (11)           ,ch(07)     ,~
           at(15,24), fac(hex(84))  , dpct$  (12)           ,ch(07)     ,~
           at(16,24), fac(hex(84))  , dpct$  (13)           ,ch(07)     ,~
           at(17,24), fac(hex(84))  , dpct$  (14)           ,ch(07)     ,~
           at(18,24), fac(hex(84))  , dpct$  (15)           ,ch(07)     ,~
           at(04,65), fac(hex(84))  , dpct$  (16)           ,ch(07)     ,~
           at(05,65), fac(hex(84))  , dpct$  (17)           ,ch(07)     ,~
           at(06,65), fac(hex(84))  , dpct$  (18)           ,ch(07)     ,~
           at(07,65), fac(hex(84))  , dpct$  (19)           ,ch(07)     ,~
           at(08,65), fac(hex(84))  , dpct$  (20)           ,ch(07)     ,~
           at(09,65), fac(hex(84))  , dpct$  (21)           ,ch(07)     ,~
           at(10,65), fac(hex(84))  , dpct$  (22)           ,ch(07)     ,~
           at(11,65), fac(hex(84))  , dpct$  (23)           ,ch(07)     ,~
           at(12,65), fac(hex(84))  , dpct$  (24)           ,ch(07)     ,~
           at(13,65), fac(hex(84))  , dpct$  (25)           ,ch(07)     ,~
           at(14,65), fac(hex(84))  , dpct$  (26)           ,ch(07)     ,~
           at(15,65), fac(hex(84))  , dpct$  (27)           ,ch(07)     ,~
           at(16,65), fac(hex(84))  , dpct$  (28)           ,ch(07)     ,~
           at(17,65), fac(hex(84))  , dpct$  (29)           ,ch(07)     ,~
           at(18,65), fac(hex(84))  , dpct$  (30)           ,ch(07)     ,~
           at(19,65), fac(hex(84))  , dpct$  (31)           ,ch(07)     ,~
            at(04,05), fac(hex(84)), dd$(01), ch(1),                     ~
            at(05,05), fac(hex(84)), dd$(02), ch(1),                     ~
            at(06,05), fac(hex(84)), dd$(03), ch(1),                     ~
            at(07,05), fac(hex(84)), dd$(04), ch(1),                     ~
            at(08,05), fac(hex(84)), dd$(05), ch(1),                     ~
            at(09,05), fac(hex(84)), dd$(06), ch(1),                     ~
            at(10,05), fac(hex(84)), dd$(07), ch(1),                     ~
            at(11,05), fac(hex(84)), dd$(08), ch(1),                     ~
            at(12,05), fac(hex(84)), dd$(09), ch(1),                     ~
            at(13,05), fac(hex(84)), dd$(10), ch(1),                     ~
            at(14,05), fac(hex(84)), dd$(11), ch(1),                     ~
            at(15,05), fac(hex(84)), dd$(12), ch(1),                     ~
            at(16,05), fac(hex(84)), dd$(13), ch(1),                     ~
            at(17,05), fac(hex(84)), dd$(14), ch(1),                     ~
            at(18,05), fac(hex(84)), dd$(15), ch(1),                     ~
            at(04,47), fac(hex(84)), dd$(16), ch(1),                     ~
            at(05,47), fac(hex(84)), dd$(17), ch(1),                     ~
            at(06,47), fac(hex(84)), dd$(18), ch(1),                     ~
            at(07,47), fac(hex(84)), dd$(19), ch(1),                     ~
            at(08,47), fac(hex(84)), dd$(20), ch(1),                     ~
            at(09,47), fac(hex(84)), dd$(21), ch(1),                     ~
            at(10,47), fac(hex(84)), dd$(22), ch(1),                     ~
            at(11,47), fac(hex(84)), dd$(23), ch(1),                     ~
            at(12,47), fac(hex(84)), dd$(24), ch(1),                     ~
            at(13,47), fac(hex(84)), dd$(25), ch(1),                     ~
            at(14,47), fac(hex(84)), dd$(26), ch(1),                     ~
            at(15,47), fac(hex(84)), dd$(27), ch(1),                     ~
            at(16,47), fac(hex(84)), dd$(28), ch(1),                     ~
            at(17,47), fac(hex(84)), dd$(29), ch(1),                     ~
            at(18,47), fac(hex(84)), dd$(30), ch(1),                     ~
            at(19,47), fac(hex(84)), dd$(31), ch(1),                     ~
           at(04,32), fac(hex(84))  , note$  (01)           ,ch(07)     ,~
           at(05,32), fac(hex(84))  , note$  (02)           ,ch(07)     ,~
           at(06,32), fac(hex(84))  , note$  (03)           ,ch(07)     ,~
           at(07,32), fac(hex(84))  , note$  (04)           ,ch(07)     ,~
           at(08,32), fac(hex(84))  , note$  (05)           ,ch(07)     ,~
           at(09,32), fac(hex(84))  , note$  (06)           ,ch(07)     ,~
           at(10,32), fac(hex(84))  , note$  (07)           ,ch(07)     ,~
           at(11,32), fac(hex(84))  , note$  (08)           ,ch(07)     ,~
           at(12,32), fac(hex(84))  , note$  (09)           ,ch(07)     ,~
           at(13,32), fac(hex(84))  , note$  (10)           ,ch(07)     ,~
           at(14,32), fac(hex(84))  , note$  (11)           ,ch(07)     ,~
           at(15,32), fac(hex(84))  , note$  (12)           ,ch(07)     ,~
           at(16,32), fac(hex(84))  , note$  (13)           ,ch(07)     ,~
           at(17,32), fac(hex(84))  , note$  (14)           ,ch(07)     ,~
           at(18,32), fac(hex(84))  , note$  (15)           ,ch(07)     ,~
           at(04,73), fac(hex(84))  , note$  (16)           ,ch(07)     ,~
           at(05,73), fac(hex(84))  , note$  (17)           ,ch(07)     ,~
           at(06,73), fac(hex(84))  , note$  (18)           ,ch(07)     ,~
           at(07,73), fac(hex(84))  , note$  (19)           ,ch(07)     ,~
           at(08,73), fac(hex(84))  , note$  (20)           ,ch(07)     ,~
           at(09,73), fac(hex(84))  , note$  (21)           ,ch(07)     ,~
           at(10,73), fac(hex(84))  , note$  (22)           ,ch(07)     ,~
           at(11,73), fac(hex(84))  , note$  (23)           ,ch(07)     ,~
           at(12,73), fac(hex(84))  , note$  (24)           ,ch(07)     ,~
           at(13,73), fac(hex(84))  , note$  (25)           ,ch(07)     ,~
           at(14,73), fac(hex(84))  , note$  (26)           ,ch(07)     ,~
           at(15,73), fac(hex(84))  , note$  (27)           ,ch(07)     ,~
           at(16,73), fac(hex(84))  , note$  (28)           ,ch(07)     ,~
           at(17,73), fac(hex(84))  , note$  (29)           ,ch(07)     ,~
           at(18,73), fac(hex(84))  , note$  (30)           ,ch(07)     ,~
           at(19,73), fac(hex(84))  , note$  (31)           ,ch(07)     ,~
                                                                         ~
                keys(hex(01030405090a0b0c0e0f1020)), key(hither%)

            if hither% =  1% then see_pip
            if hither% = 32% then exit_gracefully
            if hither% <> 16% then L27600
                         if mode%=1% then exit_gracefully else return
L27600:     if hither% = 12% then gosub L35115
            if hither% = 11% then gosub L40115
            if hither% = 14% then gosub jorpt
            if hither% =  9% then gosub pipdetail
            if hither% = 10% then gosub switch_atc
            if hither% =  3% then seepipdata
            if hither% <> 4% then L27700
                if f% = 1% then L25210
                mt% = mm%(f%-1%)
                yr% = yy%(f%-1%)
                goto L24930
L27700:     if hither% <> 5% then L27750
                if l% = 490% then L25210
                mt% = mm%(l%+1%)
                yr% = yy%(l%+1%)
                goto L24930
L27750:     if hither% <> 15% then L25210
                call "PRNTSCRN"
                goto L25210


        switch_atc
            j% = 0%
            for i% = f% to l%
                j% = j% + 1%
                if toggle% = 0% then                                     ~
                     convert used% (i%) to  davail$(j%), pic(-######)    ~
                else convert atc%  (i%) to  davail$(j%), pic(-######)
            next i%
            if toggle% = 0% then L27880
            str(title$,13,1) = "1" : str(title$,54,1) = "1"
            goto L27890
L27880:     str(title$,13,1) = "2" : str(title$,54,1) = "2"
L27890:     if toggle% = 0% then toggle% = 1% else toggle% = 0%
            return

        REM *************************************************************~
            *                                                           *~
            * PIP DETAILS                                               *~
            *                                                           *~
            *************************************************************
        pipdetail

            close ws
            call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
            sub%=min(max(1%,cursor%(1)-3%),16%)
            if sub%=16% then L28130
            if sub%<1% or sub%>15% then return
            if cursor%(2) < 40% then L28140
L28130:     sub%=sub%+15%
L28140:     if dd$(sub%)=" " then return
            sub%=sub%+f%-1%
            i$() = " "
            for i% = 1% to 8% : str(i$(i%),39,1)="!" : next i%
            keyhit%=0%
            init ("-") str(i$(9),,79)
            i$(10)="STORE     LOT  CODE    QUANTITY  TEXT"

L28220:     put pipinaltplowkey$, using L28230, thispart$, sub%, " "
L28230:         FMT CH(25),BI(4),CH(19)
L28240:     cursor%(1),v%=0%
            for i% = 1% to 8% : str(i$(i%),,38)=" " : next i%
L28260:     call "PLOWALTS" (#33,pipinaltplowkey$,1%,29%,f1%(33))
            if f1%(33)=0% then L28340
            v%,cursor%(1)=cursor%(1)+1%
            get #33, using L28300,str(i$(v%),2,19),qty
L28300:         FMT XX(29),CH(19),PD(14,4)
            call "NUMPRINT" (qty,2,str(i$(v%),25,10))
            if cursor%(1)<8% then L28260

L28340:     if v%<8% then str(i$(v%+1%),05,25)="* * * END OF FILE * * *"
            if keyhit%>3% then L28700

L28370:     put pipoutaltplowkey$, using L28380, thispart$, sub%, " "
L28380:         FMT CH(25),BI(4),CH(8)
L28390:     cursor%(2),v%=0%
            for i% = 1% to 8% : str(i$(i%),40,39) = " " : next i%
L28410:     call "PLOWALTS" (#34,pipoutaltplowkey$,1%,29%,f1%(34))
            if f1%(34)=0% then L28490
            v%,cursor%(2)=cursor%(2)+1%
            get #34, using L28450,str(i$(v%),42,19),qty
L28450:         FMT CH(19),XX(37),PD(14,4)
            call "NUMPRINT" (qty,2,str(i$(v%),65,10))
            if cursor%(2)<8% then L28410

L28490:     if v%<8% then str(i$(v%+1%),45,25)="* * * END OF FILE * * *"
            if keyhit%>3% then L28700

L28520:     put plowkey$, using L28530, thispart$," "
L28530:         FMT CH(25),CH(17)
L28540:     vv%=0% : for i% = 11% to 18%: str(i$(i%),,79) = " " : next i%
L28550:     call "PLOWNEXT" (#5,plowkey$,25%,f1%(5))
            if f1%(5) = 0% then L28680
           get #5, using L28590,str(i$(vv%+11%),3,3),str(i$(vv%+11%),8,6),~
            temp$,str(i$(vv%+11%),18,2),qty,str(i$(vv%+11%),34,40)
L28590:    FMT XX(25),CH(3),CH(6),XX(8),CH(6),CH(2),PD(14,4),XX(24),CH(40)
            if str(temp$,,6)<>yymmdd$(sub%) then L28650
            call "NUMPRINT" (qty,2,str(i$(vv%+11%),22,10))
            vv%=vv%+1%
            if vv%<8% then L28550 else goto L28680

L28650:     str(i$(vv%+11%),,79) = " "
            goto L28550

L28680:    if vv%<8% then str(i$(vv%+11%),29)="* * * END OF FILE * * *"

L28700:     temp$=yymmdd$(sub%)
            call "DATEFMT" (temp$)

L28730: accept                                                           ~
               at (01,02),                                               ~
        "PLANNED INVENTORY POSITION DETAILS FOR",                        ~
               at (01,41), fac(hex(84)), thispart$              , ch(25),~
               at (01,67),                                               ~
        "AS OF",                                                         ~
               at (01,73), fac(hex(84)), temp$                  , ch(08),~
               at (02,41), fac(hex(8c)), partdescr$             , ch(34),~
               at (03,03),                                               ~
        "IN TAG NUMBER            QUANTITY       OUT TAG NUMBER          ~
        ~ QUANTITY",                                                      ~
               at (04,02), fac(hex(86)), i$( 1)                 , ch(79),~
               at (05,02), fac(hex(86)), i$( 2)                 , ch(79),~
               at (06,02), fac(hex(86)), i$( 3)                 , ch(79),~
               at (07,02), fac(hex(86)), i$( 4)                 , ch(79),~
               at (08,02), fac(hex(86)), i$( 5)                 , ch(79),~
               at (09,02), fac(hex(86)), i$( 6)                 , ch(79),~
               at (10,02), fac(hex(86)), i$( 7)                 , ch(79),~
               at (11,02), fac(hex(86)), i$( 8)                 , ch(79),~
               at (12,02), fac(hex(86)), i$( 9)                 , ch(79),~
               at (13,02), fac(hex(86)), i$(10)                 , ch(79),~
               at (14,02), fac(hex(86)), i$(11)                 , ch(79),~
               at (15,02), fac(hex(86)), i$(12)                 , ch(79),~
               at (16,02), fac(hex(86)), i$(13)                 , ch(79),~
               at (17,02), fac(hex(86)), i$(14)                 , ch(79),~
               at (18,02), fac(hex(86)), i$(15)                 , ch(79),~
               at (19,02), fac(hex(86)), i$(16)                 , ch(79),~
               at (20,02), fac(hex(86)), i$(17)                 , ch(79),~
               at (21,02), fac(hex(86)), i$(18)                 , ch(79),~
               at (22,03),                                               ~
        "PF KEYS ACTIVE:  (POSITION CURSOR & PRESS RETURN TO SEE TOP LEVE~
        ~L DEMAND)",                                                      ~
               at (23,03),                                               ~
        "(2)PREV DAY (4)FIRST PIPIN (6)FIRST PIPOUT  (8)FIRST DETAIL  (15~
        ~)PRINT SCREEN",                                                  ~
               at (24,03),                                               ~
        "(3)NEXT DAY (5)NEXT PIPIN  (7)NEXT PIPOUT   (9)NEXT DETAIL      ~
        ~   (16)RETURN",                                                  ~
                                                                         ~
            keys(hex(0002030405060708090d0f10)), key(keyhit%)

            if keyhit%=16% then return
            if keyhit%<>15% then L29180
                call "PRNTSCRN"
                goto L28730
L29180:     if keyhit%<>13% then L29210
                call "MANUAL" ("PLNRSUB")
                goto L28730
L29210:     if keyhit%<>2% then L29240
               sub%=max(1%,sub%-1%)
               goto L28220
L29240:     if keyhit%<>3% then L29270
               sub%=min(490%,sub%+1%)
               goto L28220
L29270:     if keyhit%=4% then L28220
            if keyhit%=5% and cursor%(1)= 8% then L28240
            if keyhit%=6% then L28370
            if keyhit%=7% and cursor%(2)= 8% then L28390
            if keyhit%=8% then L28520
            if keyhit%=9% and vv%=8% then L28540

            if keyhit% <> 0%  then L29520
           mat curs% = zer : ret% = 0 : tagnr$, demand$, type$ = " "
           close ws
           call "SCREEN" addr ("C", u3%, "I", i1$(), curs%())

             if curs%(1) >= 12% or curs%(1) <= 3% then L28730
             if curs%(2) < 1% or curs%(2) > 80% then L28730
             curs%(1) = curs%(1) - 3%

             if curs%(2) <= 40% then                                     ~
                            str(tagnr$,,19) = str(i$(curs%(1)), 2,19)    ~
                               else                                      ~
                            str(tagnr$,,19) = str(i$(curs%(1)),42,19)

             if tagnr$ = " " then L28730
             if tagnr$ = "   * * * END OF FIL"  then L28730
           call "GETDEM"(1%, tagnr$ ,#35, #1, #33, demand$, type$, ret%)

L29520:     goto L28730


        REM *************************************************************~
            *                                                           *~
            * SEE PIPIN DETAILS                                         *~
            *                                                           *~
            *************************************************************

        jorpt

            pippart$=thispart$

L30050: accept                                                           ~
               at (01,03),                                               ~
        ">>>>>>>>>>>>>>>>>> EXAMINE JOB AND PURCHASE ORDER ACTIONS <<<<<<~
        ~<<<<<<<<<<<<<",                                                  ~
               at (03,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "! WHERE REQUESTED, PLEASE ENTER DATA.  THEN USE THE PF KEYS TO G~
        ~ATHER DATA. !",                                                  ~
               at (05,03),                                               ~
        "+----------------------------------------------------------+----~
        ~------------+",                                                  ~
               at (06,03),                                               ~
        "! (2) SEE PURCHASE ORDER ADVICES IN START DATE ORDER       !    ~
        ~            !",                                                  ~
               at (07,03),                                               ~
        "! (3) SEE JOB ORDER ADVICES IN START DATE ORDER            !   H~
        ~IGHLIGHT    !",                                                  ~
               at (08,03),                                               ~
        "+----------------------------------------------------------+   A~
        ~CTIONS      !",                                                  ~
               at (09,03),                                               ~
        "!     SEE ALL RELEASED AND PLANNED ORDERS-                 !   T~
        ~HAT ARE     !",                                                  ~
               at (10,03),                                               ~
        "! (4)    - IN START DATE ORDER                             !   N~
        ~OW LATE     !",                                                  ~
               at (11,03),                                               ~
        "! (5)    - IN PART CODE ORDER (THEN BY COMPLETION DATE)    !    ~
        ~            !",                                                  ~
               at (12,03),                                               ~
        "+----------------------------------------------------------+----~
        ~------------+",                                                  ~
               at (13,03),                                               ~
        "! (6) SEE ALL OUTSTANDING ORDERS FOR PART:",                    ~
               at (13,47),                                               ~
        fac(hex(81)),  pippart$, ch(25),                                 ~
               at (13,79),                                               ~
        "!",                                                             ~
               at (14,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (15,03),                                               ~
        "! (7) SEE RELEASED PURCHASE ORDERS IN PURCHASE ORDER SEQUENCE   ~
        ~            !",                                                  ~
               at (16,03),                                               ~
        "! (8) SEE RELEASED JOB ORDERS IN JOB ORDER SEQUENCE             ~
        ~            !",                                                  ~
               at (17,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (18,03),                                               ~
        "! (10) SEE EFFECTIVITY DATES                                    ~
        ~            !",                                                  ~
               at (19,03),                                               ~
        "! (11) SEE WORK CENTER ROUTINGS                                 ~
        ~            !",                                                  ~
               at (20,03),                                               ~
        "! (12) SEE BILL OF MATERIALS                                    ~
        ~            !",                                                  ~
               at (21,03),                                               ~
        "! (13) SEE WORK CENTER CAPACITIES                               ~
        ~            !",                                                  ~
               at (22,03),                                               ~
        "! (14) SEE PLANNED INVENTORY POSITION                           ~
        ~            !",                                                  ~
               at (23,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (24,19),                                               ~
        "   (15) PRINT THIS SCREEN   (16) RETURN   ",                    ~
                                                                         ~
           keys(hex(020304050607080a0b0c0d0e0f1020)), key(jorptkey%)

           if jorptkey% <> 16% then L30422
              if mode%=6% then exit_gracefully else return
L30422:    if jorptkey% = 6%  then gosub L31700
           if errormsg$ <> " " then goto L30050
           if jorptkey% = 32% then exit_gracefully
           if jorptkey% = 4%  then gosub L30505
           if jorptkey% = 2%  then gosub L30540
           if jorptkey% = 3%  then gosub L30580
           if jorptkey% = 8%  then gosub L30620
           if jorptkey% = 7%  then gosub L30660
           if jorptkey% = 5%  then gosub L30805
           if jorptkey% = 6%  then gosub L30840
           if jorptkey% = 10% then gosub see_ed
           if jorptkey% = 11% then gosub see_wc_rte
           if jorptkey% = 12% then gosub see_bom
           if jorptkey% = 13% then gosub see_wc_cap
           if jorptkey% = 14% then gosub see_pip
           if jorptkey% <> 15%  then  L30050
                     call "PRNTSCRN"
                     goto L30050

L30505:    pipinplowkey$ = " "
           break% = 0%
           returnflag% = 1%  :  file% = 33%
           flymsg$ =                                                     ~
        "OUTSTANDING RELEASED AND PLANNED JOB AND PURCHASE ORDERS IN TAG ~
        ~NUMBER ORDER "
                     goto  L30695
L30540:    pipinplowkey$ = hex(00)
           call "DELETE" (#50, pipinplowkey$, 0%)
           pipinplowkey$ = "B"  :  break% = 1%
           gosub build_work_file

           pipinplowkey$ = "R"  :  break% = 1%
           gosub build_work_file

           pipinplowkey$ = " "
           break% = 0%
           file% = 50%
           returnflag% = 2%
           flymsg$ =                                                     ~
        "     PLANNED PURCHASE ORDERS (NOT YET RELEASED) SHOWN IN START D~
        ~ATE ORDER    "
                     goto L30695
L30580:    pipinplowkey$ = "WO"
           break% = 2%
           returnflag% = 3%  :  file% = 33%
           flymsg$ =                                                     ~
        "       PLANNED JOB ORDERS (NOT YET RELEASED) SHOWN IN START DATE~
        ~ ORDER       "
                     goto L30695
L30620:    pipinplowkey$ = "JOB ORDER: "
           break% = 11%
           returnflag% = 4%  :  file% = 33%
           flymsg$ =                                                     ~
        "        RELEASED ACTIVE JOB ORDERS - SHOWN IN JOB NUMBER SEQUENC~
        ~E          "
                     goto L30695
L30660:    pipinplowkey$ = hex(00)
           call "DELETE" (#50, pipinplowkey$, 0%)
           pipinplowkey$ = "PO"  :  break% = 2%
           gosub build_work_file

           pipinplowkey$ = "QC"  :  break% = 2%
           gosub build_work_file

           pipinplowkey$ = " "
           break% = 0%
           returnflag% = 5%  :  file% = 50%
           flymsg$ =                                                     ~
        "        RELEASED ACTIVE P/O'S  - SHOWN IN P/O CODE SEQUENCE"

L30695:    ppt$(), quantity$(), startdate$(), enddate$(), tagnr$() = " "
           abc% = 0%
L30705:    if abc% > 14% then  L31015
L30709:    call "PLOWNEXT" (#file%, pipinplowkey$, break%, f1%(file%))
           if f1%(file%) <> 1% then L31015
           if file% = 50% then L30723
              if str(pipinplowkey$,,2%)="BO" then L30723
              if str(pipinplowkey$,,2%)="RO" then L30723
              if str(pipinplowkey$,,2%)="PO" then L30723
              if str(pipinplowkey$,,2%)="QC" then L30723
              if str(pipinplowkey$,,2%)="JO" then L30723
              if str(pipinplowkey$,,2%)="WO" then L30723
              if str(pipinplowkey$,,2%)="BW" then L30723
              if str(pipinplowkey$,,2%)="RW" then L30723
              goto L30709
L30723:    abc% = abc% + 1%
           if abc% > 16% then  L31015

           get #file%, using  L30740, ppt$(abc%), ed%, tagnr$(abc%),      ~
                                     quant, sd%
L30740:    FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)

           if sd% < 1% then call "DATE" addr("G+", yymmdd$(1%), sd% - 1%,~
                                             startdate$(abc%), ret%)     ~
            else startdate$(abc%) = yymmdd$(sd%)
           call "DATEFMT" (startdate$(abc%))
           enddate$(abc%) = yymmdd$(ed%)
           call "DATEFMT" (enddate$(abc%))
           if str(tagnr$(abc%),,2%)  = "WO" then L30785
           if str(tagnr$(abc%),,2%)  = "BO" then L30785
           if str(tagnr$(abc%),,2%)  = "BW" then L30785
           if str(tagnr$(abc%),,2%)  = "RO" then L30785
           if str(tagnr$(abc%),,2%)  = "RW" then L30785
           goto L30790
L30785:              if sd% < today% then enddate$(abc%) = "< LATE <"
L30790:    convert quant to quantity$(abc%), pic(-######.#)
           goto L30705

L30805:    init(hex(00)) pipinaltplowkey$
           break% = 0%
           returnflag% = 6%
           flymsg$ =                                                     ~
        "         ALL ORDERS (PLANNED AND RELEASED) IN PART CODE SEQUENCE~
        ~           "
                     goto L30880
L30840:    init(hex(00)) pipinaltplowkey$
           str(pipinaltplowkey$,,25) = str(pippart$,,25)
           break% = 25%
           returnflag% = 7%
           flymsg$ =                                                     ~
        "ORDERS IN DELIVERY DATE SEQUENCE FOR PART CODE: " & pippart$
                     goto L30880

L30880:    ppt$(), quantity$(), startdate$(), enddate$(), tagnr$() = " "
           abc% = 0%
L30890:    if abc% > 14% then  L31015
L30895:    call "PLOWALTS" (#33, pipinaltplowkey$, 1%,  break%, f1%(33%))
           if f1%(33%) <> 1% then L31015
              if str(pipinaltplowkey$,30%,2%)="BO" then L30912
              if str(pipinaltplowkey$,30%,2%)="RO" then L30912
              if str(pipinaltplowkey$,30%,2%)="PO" then L30912
              if str(pipinaltplowkey$,30%,2%)="QC" then L30912
              if str(pipinaltplowkey$,30%,2%)="JO" then L30912
              if str(pipinaltplowkey$,30%,2%)="WO" then L30912
              if str(pipinaltplowkey$,30%,2%)="BW" then L30912
              if str(pipinaltplowkey$,30%,2%)="RW" then L30912
              goto L30895
L30912:    abc% = abc% + 1%
           if abc% > 16% then  L31015
           get #33, using  L30925  ,ppt$(19), ed%, tagnr$(19), quant, sd%
L30925:    FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)
           if ppt$(19) = ppt$(20) then L30945
           if abc% = 15% then L30945 : if abc% = 1% then L30945
           abc% = abc% + 1%
L30945:    ppt$(abc%), ppt$(20) = ppt$(19)
           tagnr$(abc%) = tagnr$(19)

           startdate$(abc%) = yymmdd$(sd%)
           call "DATEFMT" (startdate$(abc%))
           enddate$(abc%) = yymmdd$(ed%)
           call "DATEFMT" (enddate$(abc%))
           if str(tagnr$(abc%),,2%)  = "WO" then L30995
           if str(tagnr$(abc%),,2%)  = "BO" then L30995
           goto L31000
L30995:              if sd% < today% then enddate$(abc%) = "< LATE <"
L31000:    convert quant to quantity$(abc%), pic(-######.#)
           goto L30890

L31015:    if ppt$(1) = " " then ppt$(1) = "NO MORE TO SHOW"
L31020: accept                                                           ~
               at (01,03),   fac(hex(84)), flymsg$, ch(78),              ~
               at (03,03),                                               ~
        "----------------------------------------------------------------~
        ~-------------",                                                  ~
               at (04,03),                                               ~
        "PART                        QUANTITY  START BY  COMPL BY  INTERN~
        ~AL TAG NUMBER",                                                  ~
               at (05,03),                                               ~
        "----------------------------------------------------------------~
        ~-------------",                                                  ~
               at (22,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (23,03),                                               ~
        "! MOVE CURSOR & RETURN FOR DEMAND  (2)FIRST (5)NEXT (15)PRNT SCN~
        ~ (16)RETURN !",                                                  ~
               at (24,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
        at(06,03), fac(hex(86)), ppt$         (01), ch(25),              ~
        at(07,03), fac(hex(86)), ppt$         (02), ch(25),              ~
        at(08,03), fac(hex(86)), ppt$         (03), ch(25),              ~
        at(09,03), fac(hex(86)), ppt$         (04), ch(25),              ~
        at(10,03), fac(hex(86)), ppt$         (05), ch(25),              ~
        at(11,03), fac(hex(86)), ppt$         (06), ch(25),              ~
        at(12,03), fac(hex(86)), ppt$         (07), ch(25),              ~
        at(13,03), fac(hex(86)), ppt$         (08), ch(25),              ~
        at(14,03), fac(hex(86)), ppt$         (09), ch(25),              ~
        at(15,03), fac(hex(86)), ppt$         (10), ch(25),              ~
        at(16,03), fac(hex(86)), ppt$         (11), ch(25),              ~
        at(17,03), fac(hex(86)), ppt$         (12), ch(25),              ~
        at(18,03), fac(hex(86)), ppt$         (13), ch(25),              ~
        at(19,03), fac(hex(86)), ppt$         (14), ch(25),              ~
        at(20,03), fac(hex(86)), ppt$         (15), ch(25),              ~
                                                                         ~
        at(06,30), fac(hex(84)), quantity$    (01), ch(9),               ~
        at(07,30), fac(hex(84)), quantity$    (02), ch(9),               ~
        at(08,30), fac(hex(84)), quantity$    (03), ch(9),               ~
        at(09,30), fac(hex(84)), quantity$    (04), ch(9),               ~
        at(10,30), fac(hex(84)), quantity$    (05), ch(9),               ~
        at(11,30), fac(hex(84)), quantity$    (06), ch(9),               ~
        at(12,30), fac(hex(84)), quantity$    (07), ch(9),               ~
        at(13,30), fac(hex(84)), quantity$    (08), ch(9),               ~
        at(14,30), fac(hex(84)), quantity$    (09), ch(9),               ~
        at(15,30), fac(hex(84)), quantity$    (10), ch(9),               ~
        at(16,30), fac(hex(84)), quantity$    (11), ch(9),               ~
        at(17,30), fac(hex(84)), quantity$    (12), ch(9),               ~
        at(18,30), fac(hex(84)), quantity$    (13), ch(9),               ~
        at(19,30), fac(hex(84)), quantity$    (14), ch(9),               ~
        at(20,30), fac(hex(84)), quantity$    (15), ch(9),               ~
                                                                         ~
        at(06,41), fac(hex(84)), startdate$   (01), ch(08),              ~
        at(07,41), fac(hex(84)), startdate$   (02), ch(08),              ~
        at(08,41), fac(hex(84)), startdate$   (03), ch(08),              ~
        at(09,41), fac(hex(84)), startdate$   (04), ch(08),              ~
        at(10,41), fac(hex(84)), startdate$   (05), ch(08),              ~
        at(11,41), fac(hex(84)), startdate$   (06), ch(08),              ~
        at(12,41), fac(hex(84)), startdate$   (07), ch(08),              ~
        at(13,41), fac(hex(84)), startdate$   (08), ch(08),              ~
        at(14,41), fac(hex(84)), startdate$   (09), ch(08),              ~
        at(15,41), fac(hex(84)), startdate$   (10), ch(08),              ~
        at(16,41), fac(hex(84)), startdate$   (11), ch(08),              ~
        at(17,41), fac(hex(84)), startdate$   (12), ch(08),              ~
        at(18,41), fac(hex(84)), startdate$   (13), ch(08),              ~
        at(19,41), fac(hex(84)), startdate$   (14), ch(08),              ~
        at(20,41), fac(hex(84)), startdate$   (15), ch(08),              ~
                                                                         ~
        at(06,51), fac(hex(84)), enddate$     (01), ch(08),              ~
        at(07,51), fac(hex(84)), enddate$     (02), ch(08),              ~
        at(08,51), fac(hex(84)), enddate$     (03), ch(08),              ~
        at(09,51), fac(hex(84)), enddate$     (04), ch(08),              ~
        at(10,51), fac(hex(84)), enddate$     (05), ch(08),              ~
        at(11,51), fac(hex(84)), enddate$     (06), ch(08),              ~
        at(12,51), fac(hex(84)), enddate$     (07), ch(08),              ~
        at(13,51), fac(hex(84)), enddate$     (08), ch(08),              ~
        at(14,51), fac(hex(84)), enddate$     (09), ch(08),              ~
        at(15,51), fac(hex(84)), enddate$     (10), ch(08),              ~
        at(16,51), fac(hex(84)), enddate$     (11), ch(08),              ~
        at(17,51), fac(hex(84)), enddate$     (12), ch(08),              ~
        at(18,51), fac(hex(84)), enddate$     (13), ch(08),              ~
        at(19,51), fac(hex(84)), enddate$     (14), ch(08),              ~
        at(20,51), fac(hex(84)), enddate$     (15), ch(08),              ~
                                                                         ~
        at(06,61), fac(hex(84)), tagnr$       (01), ch(19),              ~
        at(07,61), fac(hex(84)), tagnr$       (02), ch(19),              ~
        at(08,61), fac(hex(84)), tagnr$       (03), ch(19),              ~
        at(09,61), fac(hex(84)), tagnr$       (04), ch(19),              ~
        at(10,61), fac(hex(84)), tagnr$       (05), ch(19),              ~
        at(11,61), fac(hex(84)), tagnr$       (06), ch(19),              ~
        at(12,61), fac(hex(84)), tagnr$       (07), ch(19),              ~
        at(13,61), fac(hex(84)), tagnr$       (08), ch(19),              ~
        at(14,61), fac(hex(84)), tagnr$       (09), ch(19),              ~
        at(15,61), fac(hex(84)), tagnr$       (10), ch(19),              ~
        at(16,61), fac(hex(84)), tagnr$       (11), ch(19),              ~
        at(17,61), fac(hex(84)), tagnr$       (12), ch(19),              ~
        at(18,61), fac(hex(84)), tagnr$       (13), ch(19),              ~
        at(19,61), fac(hex(84)), tagnr$       (14), ch(19),              ~
        at(20,61), fac(hex(84)), tagnr$       (15), ch(19),              ~
                                                                         ~
           keys(hex(0002050f1020)), key(scrnhit%)

           if scrnhit% = 2%  then  L31565
           if scrnhit% = 5%  then  L31600
           if scrnhit% = 15% then  L31610
           if scrnhit% = 16% then return
           if scrnhit% = 32% then exit_gracefully
           if scrnhit% <> 0% then L31555
           mat cursor% = zer
           close ws
           call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
             field% = cursor%(1) - 5%
             ret% = 0% : demand$ = " "
             if field% < 1% or field% > 20% then L31555
             if  tagnr$(field%) = " " then L31020
L31555:      tagnr$ = tagnr$(field%)

           call "GETDEM"(1%, tagnr$(field%),#35,#1,#33,demand$,type$,ret%)
           goto L31020
L31565:    if returnflag%  = 1% then  L30505
           if returnflag%  = 2% then  L30540
           if returnflag%  = 3% then  L30580
           if returnflag%  = 4% then  L30620
           if returnflag%  = 5% then  L30660
           if returnflag%  = 6% then  L30805
           if returnflag%  = 7% then  L30840

L31600:    if returnflag%  > 5% then  L30880   else goto L30695

L31610:              call "PRNTSCRN"
                     goto L31020

L31700: REM TEST PART# W/PF6

            errormsg$ = " "
            if pippart$ = " " then L32660
                call "READ100" (#4, pippart$, f1%(4))
                if f1%(4) = 1% then return
                     call "HNYGREF" (pippart$, #8, #4, f1%(4))
                     if f1%(4) = 0% then L32660
                     call "READ100" (#4, pippart$, f1%(4))
                     if f1%(4) = 1% then return

L32660:     call "GETCODE" (#4, pippart$, partdescr$, 0%, 0, f1%(4))
                 if f1%(4) <> 0 then return
             errormsg$ = "    PART # NOT VALID" : return

        build_work_file
L32920:     call "PLOWNEXT" (#33, pipinplowkey$, break%, f1%(33%))
                if f1%(33%) = 0% then return
            get #33 using L32945, pipinrec$
L32945:         FMT CH(60)
            write #50 using L32970, pipinrec$, str(pipinrec$,32%,17%),    ~
                                   str(pipinrec$,30%,2%)
L32970:         FMT CH(60), CH(17), CH(2)
            goto L32920

        REM *************************************************************~
            *                                                           *~
            *   S E E- F C S T- P E R F                                 *~
            *                                                           *~
            *************************************************************

        see_fcst_perf
            errormsg$=" "
L35040: accept                                                           ~
           at(01,02), ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INFORMATION PLEASE ~
        ~<<<<<<<<<<<<<<<<<<<<<<<<<<<<<",                                  ~
           at(05,02), "I NEED TO KNOW WHICH PART YOU WANT FORECAST PERFOR~
        ~MANCE DETAILS FOR.",                                             ~
           at(06,02), "THE PART YOU WERE JUST WORKING ON IS SHOWN BELOW. ~
        ~JUST TYPE IN THE PART ",                                         ~
           at(07,02), "YOU WANT AND PRESS ENTER TO GET THE DETAILS, OR HI~
        ~T (16) TO RETURN.",                                              ~
           at(10,20), fac(hex(81)), thispart$, ch(25),                   ~
           at(12,02), fac(hex(94)), errormsg$, ch(79),                   ~
                     keys(hex(001020)) , key(kh%)

           if kh%  = 32%  then exit_gracefully
           if kh% <> 16% then L35115
               if mode%=5% then exit_gracefully else return

L35115:    thispartdescr$, see$() = " "

            if thispart$ = " " then L35130
                call "READ100" (#4, thispart$, f1%(4%))
                if f1%(4%) = 1% then L35130
                     call "HNYGREF" (thispart$, #8, #4, f1%(4%))
*                   IF F1%(4%) = 0% THEN 35130
L35125
*                        CALL "READ100" (#4, THISPART$, F1%(4%))
*                        IF F1%(4%) = 1% THEN 35134

L35130:    call"GETCODE" (#4, thispart$, thispartdescr$, 0%, 0, f1%(4%))
            if f1%(4%) <> 0% then L35134
                errormsg$ = "Part Not On File": goto L35040

L35134:    part$=thispart$
           gosub L35435
           mat cumf% = zer : mat fcst% = zer
           call "READ100" (#40, thispart$, f1%(40%))
           if f1%(40%) <> 1% then  L35165
*         GET #40, USING 35160, FCST%()
           call "MXFL4GT" addr(#40, 25%, fcst%(1%), 490%)
*         FMT  XX(25), 490*BI(4)
L35165:    call "READ100" (#41, thispart$, f1%(41%))
           if f1%(41%) <> 1% then L35190
           call "MXFL4GT" addr(#41, 25%, cumf%(1%), 490%)
*         GET #41, USING 35180, CUMF%()
*         FMT  XX(25), 490*BI(4)

L35190:     tdate$ = date
            call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$,5%,2%) to mo%
            convert str(udate$,1%,4%) to yr%
            ff% = 0%
            for i% = 1% to 490%
               if yy%(i%) < yr%         then L35235
               if yy%(i%) > yr%         then L35245
               if mm%(i%) < mo%         then L35235
               if mm%(i%) > mo%         then L35245
               if ff% =  0% then ff% = i%
L35235:     next i%

L35245:     l% = i% - 1%

           j%, tot  = 0
           tgtmo% = mm%(ff%)

           for i% = ff% to 490%
               if mm%(i%) = tgtmo% then L35345
               tgtmo% = mm%(i%)
               j% = j% + 1%
               convert tot to str(see$(j%), 39%,9%), pic(-######.#)
               tot = 0
               str(see$( j%),5%,3%)  = str(modate$(mm%(i% - 1%)),,3%)
               str(see$( j%),8%,1%)  = "-"
               tdate$  = yymmdd$(i% - 1%)
               call "DATEFMT" (tdate$, 0%, udate$)
               str(see$( j%),09%,2%) = str(udate$,3%,2%)
               str(see$( j%), 2%,1%) = "!"
               str(see$( j%),13%,1%) = "!"
               str(see$( j%),51%,1%) = "!"
               str(see$( j%),78%,1%) = "!"
               convert cumf%(i% - 1%) to str(see$(j%), 68%,9%) ,         ~
                                         pic(-#####.#)
L35345:        tot = tot + fcst%(i%)
           next i%
           if j% = 16% then L35420
           j% = j% + 1%
           convert tot to str(see$(j%), 39%,9%), pic(-######.#)
           tot = 0
           str(see$( j%),5%,3%)  = str(modate$(mm%(490%)),,3%)
           str(see$( j%),8%,1%)  = "-"
           tdate$  = yymmdd$(490%)
           call "DATEFMT" (tdate$, 0%, udate$)
           str(see$( j%),09%,2%) = str(udate$,3%,2%)
           str(see$( j%), 2%,1%) = "!"
           str(see$( j%),13%,1%) = "!"
           str(see$( j%),51%,1%) = "!"
           str(see$( j%),78%,1%) = "!"
           convert cumf%(490%) to str(see$(j%), 68%,9%) , pic(-#####.#)

L35420:    gosub L35435
           goto L35675

L35435: display                                                          ~
               at (01,03),                                               ~
        " FOR PART:",                                                    ~
               at (01,15),                                               ~
                      thispartdescr$, ch(34),                            ~
               at (01,50),                                               ~
        "CODE:",                                                         ~
               at (01,56),                                               ~
                      thispart$, ch(25),                                 ~
               at (02,03),                                               ~
        "           +-------------------------------------+--------------~
        ~------------+",                                                  ~
               at (03,03),                                               ~
        "           !      SALES & FORECASTS BOOKED       !        PERFOR~
        ~MANCE       !",                                                  ~
               at (04,03),                                               ~
        "   MON-YY  ! NON-NETTED      NETTED  FORECASTED  !    FOR-MON   ~
        ~EOM-CUM-NET !",                                                  ~
               at (05,03),                                               ~
        "+----------+-------------------------------------+--------------~
        ~------------+",                                                  ~
               at (22,03),                                               ~
        "+----------+-------------------------------------+--------------~
        ~------------+",                                                  ~
               at (23,03),                                               ~
        " ",                                                             ~
               at (24,03),                                               ~
        "ONE MOMENT PLEASE, I'LL KEEP YOU POSTED WHILE THE DATA GATHERING~
        ~ PROGRESSES  ",                                                  ~
           at(06,02),               see$(01), ch(79),                    ~
           at(07,02),               see$(02), ch(79),                    ~
           at(08,02),               see$(03), ch(79),                    ~
           at(09,02),               see$(04), ch(79),                    ~
           at(10,02),               see$(05), ch(79),                    ~
           at(11,02),               see$(06), ch(79),                    ~
           at(12,02),               see$(07), ch(79),                    ~
           at(13,02),               see$(08), ch(79),                    ~
           at(14,02),               see$(09), ch(79),                    ~
           at(15,02),               see$(10), ch(79),                    ~
           at(16,02),               see$(11), ch(79),                    ~
           at(17,02),               see$(12), ch(79),                    ~
           at(18,02),               see$(13), ch(79),                    ~
           at(19,02),               see$(14), ch(79),                    ~
           at(20,02),               see$(15), ch(79),                    ~
           at(21,02),               see$(16), ch(79)
           return


L35675:    mat totnet = zer : mat totnonnet = zer

           j% = 0% : mat lineend% = zer : tgtmo% = mm%(ff%)
           for i% = ff% to 490%
           if mm%(i%) = tgtmo% then  L35715
           j% = j% + 1%
           lineend%(j%) = i% - 1%
           tgtmo% = mm%(i%)
L35715:    next i%

           init(hex(00))  pipoutaltplowkey$
           str(pipoutaltplowkey$,,25%) = str(thispart$,,25%)
L35735:    call "PLOWALTS" (#34, pipoutaltplowkey$, 1%, 25%, f1%(34%))
           if f1%(34%) <> 1% then  L35865
           get #34, using L35750, tagnr$ , day%, quantity
L35750:        FMT CH(19), XX(25), BI(4), XX(8), PD(14,4)

           if str(tagnr$,,2%) = "WO" then L35735
           if str(tagnr$,,2%) = "JO" then L35735
           if str(tagnr$,,2%) = "PO" then L35735
           if str(tagnr$,,2%) = "QC" then L35735
           if str(tagnr$,,2%) = "BW" then L35735
           if str(tagnr$,,2%) = "RW" then L35735

           call "REDALT0" (#1, tagnr$, 1%,  f1%(1%))
           if f1%(1%) <> 1% then L35735
           get #1, using L35790, s$,type$
L35790:    FMT CH(1), CH(1)
           if s$<"2" then L35735
           if type$ = "2" then L35835
           if type$ = "1" then L35810
           goto  L35735
L35810:    for i% = 1% to 16%
                     if day% <= lineend%(i%) then L35825
                     next i%
L35825:    totnet(i%) = totnet(i%) + quantity
           goto L35735
L35835:    for i% = 1% to 16%
                     if day% <= lineend%(i%) then L35850
                     next i%
L35850:    totnonnet(i%) = totnonnet(i%) + quantity
           goto L35735

L35865:    for i% = 1% to 16%
           if str(see$(i%),5%,3%) = " " then L35895
           convert totnet(i%) to str(see$(i%),28%,9%), pic(-######.#)
           convert totnonnet(i%) to str(see$(i%),16%,9%), pic(-######.#)
           convert str(see$(i%),39%,9%) to aaa, data goto L35895
           convert totnet(i%) -aaa to str(see$(i%),54%,9%), pic(-######.#)
L35895:    next i%


L35910: accept                                                           ~
               at (01,03),                                               ~
        " FOR PART:",                                                    ~
               at (01,15),                                               ~
        fac(hex(84)), thispartdescr$, ch(34),                            ~
               at (01,50),                                               ~
        "CODE:",                                                         ~
               at (01,56),                                               ~
        fac(hex(84)), thispart$, ch(25),                                 ~
               at (02,03),                                               ~
        "           +-------------------------------------+--------------~
        ~------------+",                                                  ~
               at (03,03),                                               ~
        "           !      SALES & FORECASTS BOOKED       !        PERFOR~
        ~MANCE       !",                                                  ~
               at (04,03),                                               ~
        "   MON-YY  ! NON-NETTED      NETTED  FORECASTED  !    FOR-MON   ~
        ~EOM-CUM-NET !",                                                  ~
               at (05,03),                                               ~
        "+----------+-------------------------------------+--------------~
        ~------------+",                                                  ~
               at (22,03),                                               ~
        "+----------+-------------------------------------+--------------~
        ~------------+",                                                  ~
               at (23,03),                                               ~
        " ",                                                             ~
               at (24,03),                                               ~
        "(10)AVAIL TO COMMIT (11)SRCE & APLIC OF INV (14)PIP DET'S (15)PR~
        ~NT (16)RETURN",                                                  ~
           at(06,02), fac(hex(8c)), see$(01%), ch(79),                   ~
           at(07,02), fac(hex(8c)), see$(02%), ch(79),                   ~
           at(08,02), fac(hex(8c)), see$(03%), ch(79),                   ~
           at(09,02), fac(hex(8c)), see$(04%), ch(79),                   ~
           at(10,02), fac(hex(8c)), see$(05%), ch(79),                   ~
           at(11,02), fac(hex(8c)), see$(06%), ch(79),                   ~
           at(12,02), fac(hex(8c)), see$(07%), ch(79),                   ~
           at(13,02), fac(hex(8c)), see$(08%), ch(79),                   ~
           at(14,02), fac(hex(8c)), see$(09%), ch(79),                   ~
           at(15,02), fac(hex(8c)), see$(10%), ch(79),                   ~
           at(16,02), fac(hex(8c)), see$(11%), ch(79),                   ~
           at(17,02), fac(hex(8c)), see$(12%), ch(79),                   ~
           at(18,02), fac(hex(8c)), see$(13%), ch(79),                   ~
           at(19,02), fac(hex(8c)), see$(14%), ch(79),                   ~
           at(20,02), fac(hex(8c)), see$(15%), ch(79),                   ~
           at(21,02), fac(hex(8c)), see$(16%), ch(79),                   ~
           keys(hex(0a0b0e0f1020)), key(keyhithere%)

           if keyhithere%  = 10% then gosub L24330
           if keyhithere%  = 11% then gosub L40125
           if keyhithere%  = 14% then gosub jorpt
           if keyhithere%  = 15% then  L36185
           if keyhithere% <> 16% then L36175
                             if mode%=5% then exit_gracefully else return
L36175:    if keyhithere%  = 32% then exit_gracefully
           goto  L35910
L36185:              call "PRNTSCRN"
                     goto L35910

        REM *************************************************************~
            *                                                           *~
            *   S E E- I N V - S A   (SOURCE AND APPLICATION OF INV)    *~
            *                                                           *~
            *************************************************************

        see_inv_sa
            errormsg$ = " "
L40040: accept                                                           ~
           at(01,02), ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INFORMATION PLEASE ~
        ~<<<<<<<<<<<<<<<<<<<<<<<<<<<<<",                                  ~
           at(05,02), "I NEED TO KNOW WHICH PART YOU WANT THE SOURCE AND ~
        ~APPLIC OF INV FOR.",                                             ~
           at(06,02), "THE PART YOU WERE JUST WORKING ON IS SHOWN BELOW. ~
        ~JUST TYPE IN THE PART ",                                         ~
           at(07,02), "YOU WANT AND PRESS ENTER TO GET THE DETAILS, OR HI~
        ~T (16) TO RETURN.",                                              ~
           at(10,20), fac(hex(81)), thispart$, ch(25),                   ~
           at(12,02), fac(hex(94)), errormsg$, ch(79),                   ~
                     keys(hex(001020)) , key(jh%)

           if jh%  = 32%  then exit_gracefully
           if jh% <> 16% then L40115
               if mode%=2% then exit_gracefully

L40115:    thispartdescr$, shw$() = " "

            if thispart$ = " " then L40130
                call "READ100" (#4, thispart$, f1%(4%))
                if f1%(4%) = 1% then L40130
                     call "HNYGREF" (thispart$, #8, #4, f1%(4%))
*                   IF F1%(4%) = 0% THEN 40130
L40125
*                        CALL "READ100" (#4, THISPART$, F1%(4%))
*                        IF F1%(4%) = 1% THEN 40134

L40130:    call"GETCODE" (#4, thispart$, thispartdescr$, 0%, 0, f1%(4%))
            if f1%(4%) <> 0% then L40134
            errormsg$ = "Part Not On File"  :  goto L40040

L40134:    part$=thispart$
           gosub  L40150
           goto  L40610

L40150: display                                                          ~
               at (01,03),                                               ~
        " FOR PART:",                                                    ~
               at (01,15),                                               ~
                             thispartdescr$, ch(34),                     ~
               at (01,50),                                               ~
        "CODE:",                                                         ~
               at (01,56),                                               ~
                             thispart$, ch(25),                          ~
               at (02,03),                                               ~
        "           +-------------------------------+--------------------~
        ~------------+",                                                  ~
               at (03,03),                                               ~
        "           !    INVENTORY ADDITIONS FROM   !        INVENTORY US~
        ~EAGE        !",                                                  ~
               at (04,03),                                               ~
        "   MON-YY  !      PLANS         ORDERS     !    FOR SALES   AS C~
        ~OMPONENTS   !",                                                  ~
               at (05,03),                                               ~
        "+----------+-------------------------------+--------------------~
        ~------------+",                                                  ~
               at (07,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (09,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (10,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (11,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (12,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (13,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (14,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (15,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (16,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (17,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (18,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (19,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (20,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (21,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (22,03),                                               ~
        "+----------+-------------------------------+--------------------~
        ~------------+",                                                  ~
               at (23,03),                                               ~
        " ",                                                             ~
               at (24,03),                                               ~
        "ONE MOMENT PLEASE, I'LL KEEP YOU POSTED WHILE THE DATA GATHERING~
        ~ PROGRESSES  " ,                                                 ~
           at(06,02),               shw$(01%), ch(79),                   ~
           at(07,02),               shw$(02%), ch(79),                   ~
           at(08,02),               shw$(03%), ch(79),                   ~
           at(09,02),               shw$(04%), ch(79),                   ~
           at(10,02),               shw$(05%), ch(79),                   ~
           at(11,02),               shw$(06%), ch(79),                   ~
           at(12,02),               shw$(07%), ch(79),                   ~
           at(13,02),               shw$(08%), ch(79),                   ~
           at(14,02),               shw$(09%), ch(79),                   ~
           at(15,02),               shw$(10%), ch(79),                   ~
           at(16,02),               shw$(11%), ch(79),                   ~
           at(17,02),               shw$(12%), ch(79),                   ~
           at(18,02),               shw$(13%), ch(79),                   ~
           at(19,02),               shw$(14%), ch(79),                   ~
           at(20,02),               shw$(15%), ch(79),                   ~
           at(21,02),               shw$(16%), ch(79)
           return

L40610:     tdate$ = date
            call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$,5%,2%) to mo%
            convert str(udate$,1%,4%) to yr%
            ff% = 0%
            for i% = 1% to 490%
               if yy%(i%) < yr%         then L40655
               if yy%(i%) > yr%         then L40665
               if mm%(i%) < mo%         then L40655
               if mm%(i%) > mo%         then L40665
               if ff% =  0% then ff% = i%
L40655:     next i%

L40665:     l% = i% - 1%

           j%,tot  = 0
           tgtmo% = mm%(ff%)

           for i% = ff% to 490%
               if mm%(i%) = tgtmo% then L40745
               tgtmo% = mm%(i%)
               j% = j% + 1%
               str(shw$( j%),5%,3%) = str(modate$(mm%(i% - 1%)),,3%)
               str(shw$( j%),8%,1%) = "-"
               tdate$ = yymmdd$(i% - 1%)
               call "DATEFMT" (tdate$, 0%, udate$)
               str(shw$( j%),09%,2%) = str(udate$,3%,2%)
               str(shw$( j%), 2%,1%) = "!"
               str(shw$( j%),13%,1%) = "!"
               str(shw$( j%),45%,1%) = "!"
               str(shw$( j%),78%,1%) = "!"
L40745:    next i%
           if j% = 16% then L40800
           j% = j% + 1%
           str(shw$( j%),5%,3%) = str(modate$(mm%(490%)),,3%)
           str(shw$( j%),8%,1%) = "-"
           tdate$ = yymmdd$( 490% )
           call "DATEFMT" (tdate$, 0%, udate$)
           str(shw$( j%),09%,2%) = str(udate$,3%,2%)
           str(shw$( j%), 2%,1%) = "!"
           str(shw$( j%),13%,1%) = "!"
           str(shw$( j%),45%,1%) = "!"
           str(shw$( j%),78%,1%) = "!"

L40800:    gosub L40150

           mat totplnd = zer : mat totorder = zer
           mat totsls = zer  : mat totcomp = zer

           j% = 0% : mat lineend% = zer : tgtmo% = mm%(ff%)
           for i% = ff% to 490%
           if mm%(i%) = tgtmo% then  L40855
           j% = j% + 1%
           lineend%(j%) = i% - 1%
           tgtmo% = mm%(i%)
L40855:    next i%

           init(hex(00))  pipoutaltplowkey$
           str(pipoutaltplowkey$,,25%) = str(thispart$,,25%)
L40875:    call "PLOWALTS" (#34, pipoutaltplowkey$, 1%, 25%, f1%(34%))
           if f1%(34%) <> 1% then  L40980
           get #34, using L40890, tagnr$ , day%, quantity
L40890:        FMT CH(19), XX(25), BI(4), XX(8), PD(14,4)

           if str(tagnr$,,2%) = "WO" then L40925
           if str(tagnr$,,2%) = "JO" then L40925
           if str(tagnr$,,2%) = "PO" then L40925
           if str(tagnr$,,2%) = "QC" then L40925
           if str(tagnr$,,2%) = "BW" then L40925
           if str(tagnr$,,2%) = "RW" then L40925
           call "READ100" (#33, tagnr$, f1%(33%))
              if f1%(33%) <> 0% then L40875
           goto L40950

L40925:    for i% = 1% to 16%
                     if day% <= lineend%(i%) then L40940
                     next i%
L40940:    totcomp(i%) = totcomp(i%) + quantity
           goto L40875
L40950:    for i% = 1% to 16%
                     if day% <= lineend%(i%) then L40965
                     next i%
L40965:    totsls(i%) = totsls(i%) + quantity
           goto L40875

L40980:    for i% = 1% to 16%
           if str(shw$(i%),5%,3%) = " " then L41000
           convert totsls(i%) to str(shw$(i%),50%,9%), pic(-######.#)
           convert totcomp(i%) to str(shw$(i%),66%,9%), pic(-######.#)
L41000:    next i%

           gosub L40150

           init(hex(00))  pipinaltplowkey$
           str(pipinaltplowkey$,,25%) = str(thispart$,,25%)
L41030:    call "PLOWALTS" (#33, pipinaltplowkey$, 1%, 25%, f1%(33%))
           if f1%(33) <> 1 then  L41135
           get #33, using L41045, day%,  tagnr$ ,  quantity
L41045:        FMT XX(25), BI(4), CH(19),  PD(14,4)

           if str(tagnr$,,2%) = "WO" then L41080
           if str(tagnr$,,2%) = "BO" then L41080
           if str(tagnr$,,2%) = "RO" then L41105
           if str(tagnr$,,2%) = "PO" then L41105
           if str(tagnr$,,2%) = "QC" then L41105
           if str(tagnr$,,2%) = "JO" then L41105
           if str(tagnr$,,2%) = "BW" then L41080
           if str(tagnr$,,2%) = "RW" then L41105
           goto L41030

L41080:    for i% = 1% to 16%
                     if day% <= lineend%(i%) then L41095
                     next i%
L41095:    totplnd(i%) = totplnd(i%) + quantity
           goto L41030
L41105:    for i% = 1% to 16%
                     if day% <= lineend%(i%) then L41120
                     next i%
L41120:    totorder(i%) = totorder(i%) + quantity
           goto L41030

L41135:    for i% = 1% to 16%
           if str(shw$(i%),5%,3%) = " " then L41155
           convert totplnd(i%) to str(shw$(i%),16%,9%), pic(-######.#)
           convert totorder(i%) to str(shw$(i%),31%,9%), pic(-######.#)
L41155:    next i%


L41170: accept                                                           ~
               at (01,03),                                               ~
        " FOR PART:",                                                    ~
               at (01,15),                                               ~
        fac(hex(84)), thispartdescr$, ch(34),                            ~
               at (01,50),                                               ~
        "CODE:",                                                         ~
               at (01,56),                                               ~
        fac(hex(84)), thispart$, ch(25),                                 ~
               at (02,03),                                               ~
        "           +-------------------------------+--------------------~
        ~------------+",                                                  ~
               at (03,03),                                               ~
        "           !    INVENTORY ADDITIONS FROM   !        INVENTORY US~
        ~EAGE        !",                                                  ~
               at (04,03),                                               ~
        "   MON-YY  !      PLANS         ORDERS     !    FOR SALES   AS C~
        ~OMPONENTS   !",                                                  ~
               at (05,03),                                               ~
        "+----------+-------------------------------+--------------------~
        ~------------+",                                                  ~
               at (07,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (09,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (10,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (11,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (12,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (13,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (14,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (15,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (16,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (17,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (18,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (19,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (20,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (21,03),                                               ~
        "!          !                               !                    ~
        ~            !",                                                  ~
               at (22,03),                                               ~
        "+----------+-------------------------------+--------------------~
        ~------------+",                                                  ~
               at (23,03),                                               ~
        " ",                                                             ~
               at (24,03),                                               ~
        "(10)PLANNED POSITION (12)FCST/PERF (14)PIP DETAIL (15)PRINT SCRE~
        ~EN (16)RETURN",                                                  ~
           at(06,02), fac(hex(8c)), shw$(01%), ch(79),                   ~
           at(07,02), fac(hex(8c)), shw$(02%), ch(79),                   ~
           at(08,02), fac(hex(8c)), shw$(03%), ch(79),                   ~
           at(09,02), fac(hex(8c)), shw$(04%), ch(79),                   ~
           at(10,02), fac(hex(8c)), shw$(05%), ch(79),                   ~
           at(11,02), fac(hex(8c)), shw$(06%), ch(79),                   ~
           at(12,02), fac(hex(8c)), shw$(07%), ch(79),                   ~
           at(13,02), fac(hex(8c)), shw$(08%), ch(79),                   ~
           at(14,02), fac(hex(8c)), shw$(09%), ch(79),                   ~
           at(15,02), fac(hex(8c)), shw$(10%), ch(79),                   ~
           at(16,02), fac(hex(8c)), shw$(11%), ch(79),                   ~
           at(17,02), fac(hex(8c)), shw$(12%), ch(79),                   ~
           at(18,02), fac(hex(8c)), shw$(13%), ch(79),                   ~
           at(19,02), fac(hex(8c)), shw$(14%), ch(79),                   ~
           at(20,02), fac(hex(8c)), shw$(15%), ch(79),                   ~
           at(21,02), fac(hex(8c)), shw$(16%), ch(79),                   ~
           keys(hex(0a0c0e0f1020)), key(gg%)
           if gg% <> 16% then L41635
                     if mode%=2% then exit_gracefully else return
L41635:    if gg% = 32% then exit_gracefully
           if gg% <> 15% then L41655
              call "PRNTSCRN"
              goto  L41170
L41655:    if gg% = 10% then gosub L24330
           if gg% = 12% then gosub L35125
           if gg% = 14% then gosub jorpt
              goto  L41170

        REM *************************************************************~
            *                                                           *~
            * SEE DEMAND STATUS                                         *~
            *                                                           *~
            *************************************************************

        demand_status
            errormsg$, startstat$ = " "
L45040:     accept                                                       ~
               at (01,02),                                               ~
                     "PRINT THE STATUS OF CURRENT DEMANDS",              ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FOR WHICH STATUS?",                                   ~
               at (06,30), fac(hex(82)), startstat$             , ch(01),~
               at (08,02),                                               ~
                  "DEMAND STATUS CODES ARE",                             ~
               at (09,02),                                               ~
                  "  BLANK = AWAITING APPROVAL",                         ~
               at (10,02),                                               ~
                  "    1   = ALL APPROVAL GIVEN, NOT NOW PLANNED",       ~
               at (11,02),                                               ~
                  "    6   = AWAITING APPROVAL, PLANNED LATE",           ~
               at (12,02),                                               ~
                  "    7   = APPROVED, PLANNED LATE",                    ~
               at (13,02),                                               ~
                  "    8   = AWAITING APPROVAL, PLANNED ON TIME",        ~
               at (14,02),                                               ~
                  "    9   = APPROVED, PLANNED ON TIME",                 ~
                                                                         ~
               at (23,02),                                               ~
        "(ENTER) PRINT DEMAND STATUS REPORT SHOWN",                      ~
               at (24,02),                                               ~
        "(8)PRINT REPORT FOR ALL DEMANDS            (15)PRINT SCREEN  (16~
        ~)RETURN      ",                                                  ~
                                                                         ~
               keys(hex(00080f1020)),                                    ~
               key (keyttt%)
           if keyttt% <> 15% then L45220
                     call "PRNTSCRN"
                     goto L45040
L45220:    if keyttt% = 32% then exit_gracefully
           if keyttt% <> 16% then L45235
                         if mode%=7% then exit_gracefully else return
L45235:    if keyttt% = 8% then L45340

           if startstat$ = " " then L45340
           if startstat$ = "1" then L45340
           if startstat$ = "6" then L45340
           if startstat$ = "7" then L45340
           if startstat$ = "8" then L45340
           if startstat$ = "9" then L45340

           errormsg$ = "NO SUCH STATUS: " & startstat$
           goto L45040

L45340:    select printer(134)
           if keyttt% = 8% then nnn% = 0% else nnn% = 1%
           dplowkey$ = str(startstat$,,1) & " "
L45355:    gosub stat_print_control
L45360:    call "PLOWALTS" (#1, dplowkey$, 2%, nnn%, f1%(1))
           if f1%(1) =  1% then L45385
           close printer
           goto L45040

L45385:     get #1, using L45520 ,  demstat$, demtype$, demprior$,        ~
                demcompdate$, demcode$, demline$, dempart$, demquant$,   ~
                demwc$, demsbom$, demsrte$, demwhse$, dlp$, pcd$,        ~
                demcus$

            call "NUMTEST" (str(demquant$,,10), 0, 9999999999, errormsg$,~
                            -0.2, 0)
            if errormsg$ <> " " then errormsg$, demquant$ = " "
            call "DATEFMT" (demcompdate$)
            call "DATEFMT" (dlp$)
            call "DATEFMT" (pcd$)

        print using L45440, demstat$,demcode$,demline$,demprior$,demtype$,~
                dempart$, demquant$, demcus$, demcompdate$, pcd$, dlp$

L45440: %   #    ################ ###     #       #  ####################~
        ~##### ########## ######### ######## ########  ########

L45455: % STATUS DEMAND CODE      LINE PRIORITY TYPE PART                ~
        ~        QUANTITY CUSTOMER  DUE DATE CMP.DATE  LAST PLAN

            call "DATUNFMT" (demcompdate$)
            call "DATUNFMT" (dlp$)
            call "DATUNFMT" (pcd$)

            lncount% = lncount% + 1%
            if lncount% > 57% then L45355
            goto L45360


        stat_print_control
           print page
           print using L45490, startstat$, date$
L45490: % STATUS OF ALL DEMANDS THAT ARE AT STATUS # AS OF ########
           print skip(1)
           print using L45455
           print skip(1)
           lncount% = 4%
           return

L45520:     FMT CH( 1),                       /* RECORD STATUS         */~
                CH( 1),                       /* DEMAND TYPE           */~
                CH( 1),                       /* PRIORITY              */~
                CH( 6),                       /* REQ COMPLETION DATE   */~
                CH(16),                       /* DEMAND CODE           */~
                CH( 3),                       /* DEMAND LINE           */~
                CH(25),                       /* PART NEEDED           */~
                CH(10),                       /* QUANTITY              */~
                CH( 4),                       /* WC (FOR PM ONLY)      */~
                CH( 3),                       /* WHICH BOM REQD?       */~
                CH( 3),                       /* WHICH WCROUTE REQD?   */~
                CH( 3),                       /* DEL TO/SHIP FROM WHSE */~
                CH( 6),                       /* DATE LAST PLANNED     */~
                CH( 6),                       /* PLANNED COMPL DATE    */~
                CH(09)                        /* CUSTOMER IF TYPE 1    */~

        REM *************************************************************~
            *                                                           *~
            * WORK CENTER CAPACITY REVIEW                               *~
            *                                                           *~
            *************************************************************

        see_wc_cap
            errormsg$ = " "
L50040: accept                                                           ~
           at(01,02), ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INFORMATION PLEASE ~
        ~<<<<<<<<<<<<<<<<<<<<<<<<<<<<<",                                  ~
           at(10,02), "FOR WHICH WORK CENTER DO YOU WISH TO REVIEW CAPACI~
        ~TY?",                                                            ~
           at(12,02), "WORK CENTER:",                                    ~
           at(12,16), fac(hex(81)),       wc$, ch(04),                   ~
           at(14,02), fac(hex(94)), errormsg$, ch(79),                   ~
           at(22,02), "PF-KEYS ACTIVE:",                                 ~
           at(23,02), "(ENTER)TO EXAMINE WORK CENTER CAPACITY",          ~
           at(24,60), "(16)RETURN",                                      ~
                     keys(hex(001020)) , key(khht%)

                     if khht% = 32 then exit_gracefully
                     if khht% <> 16 then L50130
                        if mode%=8% then exit_gracefully else return

L50130:    call "GETCODE" (#11, wc$, wcdescr$, 0%, 0, f1%(11))
                if f1%(11) <> 0 then L50139
                errormsg$ = "Work Center Not On File"
                goto L50040
L50139:    call "SHOSTAT" ("Loading Work Center Data")
*         GET #11, USING 50150, WAVAIL%(), WUSED%()
           call "MXFL2GT" addr(#11,   59%, wavail%(1%), 490%)
           call "MXFL2GT" addr(#11, 1039%, wused%(1%), 490%)
*         FMT XX(59), 490*BI(02), 490*BI(02)

        REM *************************************************************~
           *    S E E   C A P A C I T I E S   B Y   M O N T H           *~
           **************************************************************

        seecapacities
            tdate$ = date
            call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$,5%,2%) to mo%
            convert str(udate$,1%,4%) to yr%
L50295:     j%, f% = 0%
            for i% = 1% to 490%
                if yy%(i%) < yr% then L50330
                if yy%(i%) > yr% then L50340
                if mm%(i%) < mo% then L50330
                if mm%(i%) > mo% then L50340
                if f% =  0% then f% = i%
L50330:     next i%

L50340:     l% = i% - 1%

            effic,     counter, counter1, j% = 0%
            wdd$(), wdavail$(), wdused$(), wdpct$(), wnote$() = " "

            for i% = f% to l%
                j% = j% + 1%
                if wavail%(i%) = 0% then L50390
                counter1 = counter1 + wavail%(i%)
                convert wavail%(i%) to  wdavail$(j%), pic(-#####)
L50390:         if wused%(i%) = 0% then L50405
                   counter = counter + wused%(i%)
                   convert wused%(i%)  to  wdused$(j%) , pic(-#####)
L50405:            if wavail%(i%) = 0 then L50415
                pctwork=1000%*wused%(i%)/wavail%(i%):pctwork=pctwork/10
                convert round(pctwork,1%) to wdpct$(j%), pic(####.#)
L50415:            wdd$(j%) = dow$(i%)
                if wused%(i%)=wavail%(i%) and wavail%(i%)>0% then        ~
                   wnote$(j%)=" FULL"
          if wused%(i%) > wavail%(i%) then wnote$(j%)=" OVER"
            next i%

           if counter1 = 0 then L50455
              effic = 100 * counter / counter1

L50455:    init(hex(86)) lfac$()

L50465: accept                                                           ~
               at (01,02),                                               ~
        "UNITS AVAILABLE AND USED FOR WORK CENTER:",                     ~
               at (01,43), fac(hex(84)), wc$                    , ch(04),~
               at (01,48),                                               ~
        "MONTH OF:",                                                     ~
               at (01,57), fac(hex(84)), modate$(mo%)           , ch(09),~
               at (01,68), fac(hex(84)), yr%, pic(####),                 ~
               at(02,31), fac(hex(84)), wcdescr$, ch(32),                ~
               at (03,03),                                               ~
        "DAY     AVAIL   USED      %           ! DAY      AVAIL   USED   ~
        ~   %",                                                           ~
               at (04,03), "01", at(04,41), "!  16",                     ~
               at (05,03), "02", at(05,41), "!  17",                     ~
               at (06,03), "03", at(06,41), "!  18",                     ~
               at (07,03), "04", at(07,41), "!  19",                     ~
               at (08,03), "05", at(08,41), "!  20",                     ~
               at (09,03), "06", at(09,41), "!  21",                     ~
               at (10,03), "07", at(10,41), "!  22",                     ~
               at (11,03), "08", at(11,41), "!  23",                     ~
               at (12,03), "09", at(12,41), "!  24",                     ~
               at (13,03), "10", at(13,41), "!  25",                     ~
               at (14,03), "11", at(14,41), "!  26",                     ~
               at (15,03), "12", at(15,41), "!  27",                     ~
               at (16,03), "13", at(16,41), "!  28",                     ~
               at (17,03), "14", at(17,41), "!  29",                     ~
               at (18,03), "15", at(18,41), "!  30",                     ~
                                 at(19,41), "!  31",                     ~
               at (20,02),                                               ~
        "+---------------------------------------------------------------~
        ~--------------+",                                                ~
               at (21,02),                                               ~
        "WORK CENTER UTILIZATION:",                                      ~
               at(21,27), fac(hex(84)), effic , pic(###.##),             ~
               at(21,34), "% (USED ",                                    ~
               at(21,42), fac(hex(84)), counter, pic(#####),             ~
               at(21,48), "OUT OF",                                      ~
               at(21,55), fac(hex(84)), counter1, pic(#####),            ~
               at(21,61), "TOTAL UNITS AVAIL)",                          ~
                                                                         ~
           at(04,10), fac(lfac$(01)),wdavail$(01)           ,ch(06)     ,~
           at(05,10), fac(lfac$(02)),wdavail$(02)           ,ch(06)     ,~
           at(06,10), fac(lfac$(03)),wdavail$(03)           ,ch(06)     ,~
           at(07,10), fac(lfac$(04)),wdavail$(04)           ,ch(06)     ,~
           at(08,10), fac(lfac$(05)),wdavail$(05)           ,ch(06)     ,~
           at(09,10), fac(lfac$(06)),wdavail$(06)           ,ch(06)     ,~
           at(10,10), fac(lfac$(07)),wdavail$(07)           ,ch(06)     ,~
           at(11,10), fac(lfac$(08)),wdavail$(08)           ,ch(06)     ,~
           at(12,10), fac(lfac$(09)),wdavail$(09)           ,ch(06)     ,~
           at(13,10), fac(lfac$(10)),wdavail$(10)           ,ch(06)     ,~
           at(14,10), fac(lfac$(11)),wdavail$(11)           ,ch(06)     ,~
           at(15,10), fac(lfac$(12)),wdavail$(12)           ,ch(06)     ,~
           at(16,10), fac(lfac$(13)),wdavail$(13)           ,ch(06)     ,~
           at(17,10), fac(lfac$(14)),wdavail$(14)           ,ch(06)     ,~
           at(18,10), fac(lfac$(15)),wdavail$(15)           ,ch(06)     ,~
           at(04,51), fac(lfac$(16)),wdavail$(16)           ,ch(06)     ,~
           at(05,51), fac(lfac$(17)),wdavail$(17)           ,ch(06)     ,~
           at(06,51), fac(lfac$(18)),wdavail$(18)           ,ch(06)     ,~
           at(07,51), fac(lfac$(19)),wdavail$(19)           ,ch(06)     ,~
           at(08,51), fac(lfac$(20)),wdavail$(20)           ,ch(06)     ,~
           at(09,51), fac(lfac$(21)),wdavail$(21)           ,ch(06)     ,~
           at(10,51), fac(lfac$(22)),wdavail$(22)           ,ch(06)     ,~
           at(11,51), fac(lfac$(23)),wdavail$(23)           ,ch(06)     ,~
           at(12,51), fac(lfac$(24)),wdavail$(24)           ,ch(06)     ,~
           at(13,51), fac(lfac$(25)),wdavail$(25)           ,ch(06)     ,~
           at(14,51), fac(lfac$(26)),wdavail$(26)           ,ch(06)     ,~
           at(15,51), fac(lfac$(27)),wdavail$(27)           ,ch(06)     ,~
           at(16,51), fac(lfac$(28)),wdavail$(28)           ,ch(06)     ,~
           at(17,51), fac(lfac$(29)),wdavail$(29)           ,ch(06)     ,~
           at(18,51), fac(lfac$(30)),wdavail$(30)           ,ch(06)     ,~
           at(19,51), fac(lfac$(31)),wdavail$(31)           ,ch(06)     ,~
           at(04,17), fac(hex(84))  ,wdused$ (01)           ,ch(06)     ,~
           at(05,17), fac(hex(84))  ,wdused$ (02)           ,ch(06)     ,~
           at(06,17), fac(hex(84))  ,wdused$ (03)           ,ch(06)     ,~
           at(07,17), fac(hex(84))  ,wdused$ (04)           ,ch(06)     ,~
           at(08,17), fac(hex(84))  ,wdused$ (05)           ,ch(06)     ,~
           at(09,17), fac(hex(84))  ,wdused$ (06)           ,ch(06)     ,~
           at(10,17), fac(hex(84))  ,wdused$ (07)           ,ch(06)     ,~
           at(11,17), fac(hex(84))  ,wdused$ (08)           ,ch(06)     ,~
           at(12,17), fac(hex(84))  ,wdused$ (09)           ,ch(06)     ,~
           at(13,17), fac(hex(84))  ,wdused$ (10)           ,ch(06)     ,~
           at(14,17), fac(hex(84))  ,wdused$ (11)           ,ch(06)     ,~
           at(15,17), fac(hex(84))  ,wdused$ (12)           ,ch(06)     ,~
           at(16,17), fac(hex(84))  ,wdused$ (13)           ,ch(06)     ,~
           at(17,17), fac(hex(84))  ,wdused$ (14)           ,ch(06)     ,~
           at(18,17), fac(hex(84))  ,wdused$ (15)           ,ch(06)     ,~
           at(04,58), fac(hex(84))  ,wdused$ (16)           ,ch(06)     ,~
           at(05,58), fac(hex(84))  ,wdused$ (17)           ,ch(06)     ,~
           at(06,58), fac(hex(84))  ,wdused$ (18)           ,ch(06)     ,~
           at(07,58), fac(hex(84))  ,wdused$ (19)           ,ch(06)     ,~
           at(08,58), fac(hex(84))  ,wdused$ (20)           ,ch(06)     ,~
           at(09,58), fac(hex(84))  ,wdused$ (21)           ,ch(06)     ,~
           at(10,58), fac(hex(84))  ,wdused$ (22)           ,ch(06)     ,~
           at(11,58), fac(hex(84))  ,wdused$ (23)           ,ch(06)     ,~
           at(12,58), fac(hex(84))  ,wdused$ (24)           ,ch(06)     ,~
           at(13,58), fac(hex(84))  ,wdused$ (25)           ,ch(06)     ,~
           at(14,58), fac(hex(84))  ,wdused$ (26)           ,ch(06)     ,~
           at(15,58), fac(hex(84))  ,wdused$ (27)           ,ch(06)     ,~
           at(16,58), fac(hex(84))  ,wdused$ (28)           ,ch(06)     ,~
           at(17,58), fac(hex(84))  ,wdused$ (29)           ,ch(06)     ,~
           at(18,58), fac(hex(84))  ,wdused$ (30)           ,ch(06)     ,~
           at(19,58), fac(hex(84))  ,wdused$ (31)           ,ch(06)     ,~
           at(04,24), fac(hex(84))  ,wdpct$  (01)           ,ch(06)     ,~
           at(05,24), fac(hex(84))  ,wdpct$  (02)           ,ch(06)     ,~
           at(06,24), fac(hex(84))  ,wdpct$  (03)           ,ch(06)     ,~
           at(07,24), fac(hex(84))  ,wdpct$  (04)           ,ch(06)     ,~
           at(08,24), fac(hex(84))  ,wdpct$  (05)           ,ch(06)     ,~
           at(09,24), fac(hex(84))  ,wdpct$  (06)           ,ch(06)     ,~
           at(10,24), fac(hex(84))  ,wdpct$  (07)           ,ch(06)     ,~
           at(11,24), fac(hex(84))  ,wdpct$  (08)           ,ch(06)     ,~
           at(12,24), fac(hex(84))  ,wdpct$  (09)           ,ch(06)     ,~
           at(13,24), fac(hex(84))  ,wdpct$  (10)           ,ch(06)     ,~
           at(14,24), fac(hex(84))  ,wdpct$  (11)           ,ch(06)     ,~
           at(15,24), fac(hex(84))  ,wdpct$  (12)           ,ch(06)     ,~
           at(16,24), fac(hex(84))  ,wdpct$  (13)           ,ch(06)     ,~
           at(17,24), fac(hex(84))  ,wdpct$  (14)           ,ch(06)     ,~
           at(18,24), fac(hex(84))  ,wdpct$  (15)           ,ch(06)     ,~
           at(04,65), fac(hex(84))  ,wdpct$  (16)           ,ch(06)     ,~
           at(05,65), fac(hex(84))  ,wdpct$  (17)           ,ch(06)     ,~
           at(06,65), fac(hex(84))  ,wdpct$  (18)           ,ch(06)     ,~
           at(07,65), fac(hex(84))  ,wdpct$  (19)           ,ch(06)     ,~
           at(08,65), fac(hex(84))  ,wdpct$  (20)           ,ch(06)     ,~
           at(09,65), fac(hex(84))  ,wdpct$  (21)           ,ch(06)     ,~
           at(10,65), fac(hex(84))  ,wdpct$  (22)           ,ch(06)     ,~
           at(11,65), fac(hex(84))  ,wdpct$  (23)           ,ch(06)     ,~
           at(12,65), fac(hex(84))  ,wdpct$  (24)           ,ch(06)     ,~
           at(13,65), fac(hex(84))  ,wdpct$  (25)           ,ch(06)     ,~
           at(14,65), fac(hex(84))  ,wdpct$  (26)           ,ch(06)     ,~
           at(15,65), fac(hex(84))  ,wdpct$  (27)           ,ch(06)     ,~
           at(16,65), fac(hex(84))  ,wdpct$  (28)           ,ch(06)     ,~
           at(17,65), fac(hex(84))  ,wdpct$  (29)           ,ch(06)     ,~
           at(18,65), fac(hex(84))  ,wdpct$  (30)           ,ch(06)     ,~
           at(19,65), fac(hex(84))  ,wdpct$  (31)           ,ch(06)     ,~
            at(04,06), fac(hex(84)),wdd$(01), ch(3),                     ~
            at(05,06), fac(hex(84)),wdd$(02), ch(3),                     ~
            at(06,06), fac(hex(84)),wdd$(03), ch(3),                     ~
            at(07,06), fac(hex(84)),wdd$(04), ch(3),                     ~
            at(08,06), fac(hex(84)),wdd$(05), ch(3),                     ~
            at(09,06), fac(hex(84)),wdd$(06), ch(3),                     ~
            at(10,06), fac(hex(84)),wdd$(07), ch(3),                     ~
            at(11,06), fac(hex(84)),wdd$(08), ch(3),                     ~
            at(12,06), fac(hex(84)),wdd$(09), ch(3),                     ~
            at(13,06), fac(hex(84)),wdd$(10), ch(3),                     ~
            at(14,06), fac(hex(84)),wdd$(11), ch(3),                     ~
            at(15,06), fac(hex(84)),wdd$(12), ch(3),                     ~
            at(16,06), fac(hex(84)),wdd$(13), ch(3),                     ~
            at(17,06), fac(hex(84)),wdd$(14), ch(3),                     ~
            at(18,06), fac(hex(84)),wdd$(15), ch(3),                     ~
            at(04,47), fac(hex(84)),wdd$(16), ch(3),                     ~
            at(05,47), fac(hex(84)),wdd$(17), ch(3),                     ~
            at(06,47), fac(hex(84)),wdd$(18), ch(3),                     ~
            at(07,47), fac(hex(84)),wdd$(19), ch(3),                     ~
            at(08,47), fac(hex(84)),wdd$(20), ch(3),                     ~
            at(09,47), fac(hex(84)),wdd$(21), ch(3),                     ~
            at(10,47), fac(hex(84)),wdd$(22), ch(3),                     ~
            at(11,47), fac(hex(84)),wdd$(23), ch(3),                     ~
            at(12,47), fac(hex(84)),wdd$(24), ch(3),                     ~
            at(13,47), fac(hex(84)),wdd$(25), ch(3),                     ~
            at(14,47), fac(hex(84)),wdd$(26), ch(3),                     ~
            at(15,47), fac(hex(84)),wdd$(27), ch(3),                     ~
            at(16,47), fac(hex(84)),wdd$(28), ch(3),                     ~
            at(17,47), fac(hex(84)),wdd$(29), ch(3),                     ~
            at(18,47), fac(hex(84)),wdd$(30), ch(3),                     ~
            at(19,47), fac(hex(84)),wdd$(31), ch(3),                     ~
           at(04,31), fac(hex(84))  ,wnote$  (01)           ,ch(07)     ,~
           at(05,31), fac(hex(84))  ,wnote$  (02)           ,ch(07)     ,~
           at(06,31), fac(hex(84))  ,wnote$  (03)           ,ch(07)     ,~
           at(07,31), fac(hex(84))  ,wnote$  (04)           ,ch(07)     ,~
           at(08,31), fac(hex(84))  ,wnote$  (05)           ,ch(07)     ,~
           at(09,31), fac(hex(84))  ,wnote$  (06)           ,ch(07)     ,~
           at(10,31), fac(hex(84))  ,wnote$  (07)           ,ch(07)     ,~
           at(11,31), fac(hex(84))  ,wnote$  (08)           ,ch(07)     ,~
           at(12,31), fac(hex(84))  ,wnote$  (09)           ,ch(07)     ,~
           at(13,31), fac(hex(84))  ,wnote$  (10)           ,ch(07)     ,~
           at(14,31), fac(hex(84))  ,wnote$  (11)           ,ch(07)     ,~
           at(15,31), fac(hex(84))  ,wnote$  (12)           ,ch(07)     ,~
           at(16,31), fac(hex(84))  ,wnote$  (13)           ,ch(07)     ,~
           at(17,31), fac(hex(84))  ,wnote$  (14)           ,ch(07)     ,~
           at(18,31), fac(hex(84))  ,wnote$  (15)           ,ch(07)     ,~
           at(04,72), fac(hex(84))  ,wnote$  (16)           ,ch(07)     ,~
           at(05,72), fac(hex(84))  ,wnote$  (17)           ,ch(07)     ,~
           at(06,72), fac(hex(84))  ,wnote$  (18)           ,ch(07)     ,~
           at(07,72), fac(hex(84))  ,wnote$  (19)           ,ch(07)     ,~
           at(08,72), fac(hex(84))  ,wnote$  (20)           ,ch(07)     ,~
           at(09,72), fac(hex(84))  ,wnote$  (21)           ,ch(07)     ,~
           at(10,72), fac(hex(84))  ,wnote$  (22)           ,ch(07)     ,~
           at(11,72), fac(hex(84))  ,wnote$  (23)           ,ch(07)     ,~
           at(12,72), fac(hex(84))  ,wnote$  (24)           ,ch(07)     ,~
           at(13,72), fac(hex(84))  ,wnote$  (25)           ,ch(07)     ,~
           at(14,72), fac(hex(84))  ,wnote$  (26)           ,ch(07)     ,~
           at(15,72), fac(hex(84))  ,wnote$  (27)           ,ch(07)     ,~
           at(16,72), fac(hex(84))  ,wnote$  (28)           ,ch(07)     ,~
           at(17,72), fac(hex(84))  ,wnote$  (29)           ,ch(07)     ,~
           at(18,72), fac(hex(84))  ,wnote$  (30)           ,ch(07)     ,~
           at(19,72), fac(hex(84))  ,wnote$  (31)           ,ch(07)     ,~
                                                                         ~
               at (22,02),                                               ~
        "+---------------------------------------------------------------~
        ~--------------+",                                                ~
              at (23,02),                                                ~
        "(3)FIRST MO   (4)PREV MO    (5)NEXT MO      POSITION CURSOR AND ~
        ~(9)DETAILS ",                                                    ~
               at (24,02),                                               ~
        "(10)SEE AVAILABLE TO COMMIT (14)SEE PIP DETAIL  (15)PRINT SCREEN~
        ~ (16)RETURN",                                                    ~
                                                                         ~
                keys(hex(030405090a0e0f1020)), key(keyhit%)

            if keyhit%  = 32% then exit_gracefully
            if keyhit% <> 16% then L51526
                if mode%=8% then exit_gracefully else return
L51526:     if keyhit% =  9% then gosub wcdetail
            if keyhit% =  10% then gosub see_pip
            if keyhit% =  14% then gosub jorpt
            if keyhit% =  3% then seecapacities
            if keyhit% <> 4% then L51570
                if f% = 1% then L50465
                mo% = mm%(f%-1%)
                yr% = yy%(f%-1%)
                goto L50295
L51570:     if keyhit% <> 5% then L51595
                if l% = 490% then L50465
                mo% = mm%(l%+1%)
                yr% = yy%(l%+1%)
                goto L50295
L51595:     if keyhit% <> 15% then L50465
                call "PRNTSCRN"
                goto L50465


        REM *************************************************************~
            *                                                           *~
            * WORK CENTER CAPACITY DETAILS                              *~
            *                                                           *~
            *************************************************************
        wcdetail

            close ws
            call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
            sub%=min(max(1%,cursor%(1)-3%),16%)
            if sub%=16% then L52120
            if sub%<1% or sub%>15% then return
            if cursor%(2) < 40% then L52130
L52120:     sub%=sub%+15%
L52130:     if wdd$(sub%)=" " then return
            sub%=sub%+f%-1%

L52140:     put plowkey$, using L52150, wc$, sub%
L52150:         FMT CH(4), BI(2)
            init (hex(00)) str(plowkey$,7)
            see$() = " " : v%=0%

L52180:     call "PLOWALTS" (#23,plowkey$,1%,6%,f1%(23))
            if f1%(23)=0% then L52250
            v%=v%+1%
            get #23, using L52210,str(see$(v%),2,19), su%, run%
L52210:         FMT XX(8), CH(19), XX(4), 2*BI(4), CH(25)
            qty = su% + run%
            call "CONVERT" (qty, 0.2, str(see$(v%),52,10))
            call "READ100" (#25, str(see$(v%),2,19), f1%(25))
                if f1%(25) <> 0 then get #25, str(see$(v%),24,25)        ~
                                     else str(see$(v%),24,25) = " "
            if v%<18% then L52180

L52250:     if v%<18% then str(see$(v%+1%),24,25)=                       ~
                                                "* * * END OF FILE * * *"
            temp$=yymmdd$(sub%)
            call "DATEFMT" (temp$)

L52310: accept                                                           ~
               at (01,03),                                               ~
        "WORK CENTER DETAILS FOR",                                       ~
               at (01,28), fac(hex(84)), wc$                    , ch(04),~
               at (01,34),                                               ~
        "AS OF",                                                         ~
               at (01,41), fac(hex(84)), temp$                  , ch(08),~
               at (03,03),                                               ~
        "TAG NUMBER            PART CODE                   UNITS USED",  ~
               at (04,02), fac(hex(86)), see$( 1)               , ch(79),~
               at (05,02), fac(hex(86)), see$( 2)               , ch(79),~
               at (06,02), fac(hex(86)), see$( 3)               , ch(79),~
               at (07,02), fac(hex(86)), see$( 4)               , ch(79),~
               at (08,02), fac(hex(86)), see$( 5)               , ch(79),~
               at (09,02), fac(hex(86)), see$( 6)               , ch(79),~
               at (10,02), fac(hex(86)), see$( 7)               , ch(79),~
               at (11,02), fac(hex(86)), see$( 8)               , ch(79),~
               at (12,02), fac(hex(86)), see$( 9)               , ch(79),~
               at (13,02), fac(hex(86)), see$(10)               , ch(79),~
               at (14,02), fac(hex(86)), see$(11)               , ch(79),~
               at (15,02), fac(hex(86)), see$(12)               , ch(79),~
               at (16,02), fac(hex(86)), see$(13)               , ch(79),~
               at (17,02), fac(hex(86)), see$(14)               , ch(79),~
               at (18,02), fac(hex(86)), see$(15)               , ch(79),~
               at (19,02), fac(hex(86)), see$(16)               , ch(79),~
               at (20,02), fac(hex(86)), see$(17)               , ch(79),~
               at (21,02), fac(hex(86)), see$(18)               , ch(79),~
               at (22,03),                                               ~
        "PF Keys Active:  (Position Cursor & Press RETURN To See Top Leve~
        ~l Demand)",                                                      ~
               at (23,03),                                               ~
        "            (3)Next Day       (5)Next      (13)Instructions  (15~
        ~)Print Screen",                                                  ~
               at (24,03),                                               ~
        "(2)Prev Day            (4)First                                 ~
        ~   (16)RETURN",                                                  ~
                                                                         ~
            keys(hex(00020304050d0f10)), key(keyhit%)

            if keyhit%=16% then return
            if keyhit%<>15% then L52600
                call "PRNTSCRN"
                goto L52310
L52600:     if keyhit%<>13% then L52630
                call "MANUAL" ("PLNRSUB")
                goto L52310
L52630:     if keyhit%<>2% then L52660
               sub%=max(1%,sub%-1%)
               goto L52140
L52660:     if keyhit%<>3% then L52690
               sub%=min(490%,sub%+1%)
               goto L52140
L52690:     if keyhit%=4 then L52140
            if keyhit%=0% then L52730
            if keyhit%=5% and v%=18% then L52180
            goto L52310

L52730:    mat cursor% = zer
           close ws
           call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
             field% = cursor%(1) - 3%
             ret% = 0% : demand$ = " "
             if field% < 1% or field% > 18% then L52800
             if  str(see$(field%),2,19) = " " then L52310
L52800:      tagnr$ = str(see$(field%),2,19)

           call "GETDEM"(1%, str(see$(field%),2,19), #35,#1,#33, demand$,~
                                                             type$, ret%)
           goto L52310

        REM *************************************************************~
            *                                                           *~
            * LOAD CALENDAR ONE TIME.                                   *~
            *   -JUST TO TAKE THE BURDEN OFF ANY CALLING PROGRAM        *~
            *************************************************************

        loadcalendar

            call "READ100" (#12,"10", f1%(12))
                if f1%(12) = 0 then L64966
            get #12, using L64922, str(yymmdd$(),,1470)
L64922:         FMT XX(2), CH(1470)

            call "READ100" (#12,"11", f1%(12))
                if f1%(12) = 0 then L64966
            get #12, using L64922, str(yymmdd$(),1471,1470)

            call "READ100" (#12,"20", f1%(12))
                if f1%(12) = 0 then L64966
*          GET #12, USING 64940, YY%()
            call "MXFL4GT" addr(#12, 2%, yy%(1%), 490%)
*              FMT XX(2), 490*BI(4)

            call "READ100" (#12,"30", f1%(12))
                if f1%(12) = 0 then L64966
            call "MXFL4GT" addr(#12, 2%, mm%(1%), 490%)
*          GET #12, USING 64940, MM%()

            call "READ100" (#12,"50", f1%(12))
                if f1%(12) = 0 then L64966
            get #12, using L64958, dow$()
L64958:         FMT XX(2), 490*CH(3)

            return

L64966:     hit=1
            return

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_gracefully
            errormsg$ = " "
            return clear all


            end
