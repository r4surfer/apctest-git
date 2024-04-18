        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   IIIII  PPPP     A     CCC   TTTTT   SSS   BBBB    *~
            *  P   P    I    P   P   A A   C        T    S      B   B   *~
            *  PPPP     I    PPPP   A   A  C        T     SSS   BBBB    *~
            *  P        I    P      AAAAA  C        T        S  B   B   *~
            *  P      IIIII  P      A   A   CCCC    T     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPACTSB - SEE_PIP Available to Commit                    *~
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
            * 04/27/87 ! SEE_PIP Section removed from PLNRSUB     ! MJB *~
            * 11/15/87 ! Corrected File Channels, now see PIPOUTS ! KAB *~
            * 12/01/87 ! Corrected 'Next' logic on PIPIN & PIPOUTs! HES *~
            * 05/19/88 ! New PF key to show ATC for Type 2 Sales  ! RJM *~
            * 07/05/90 ! Honor ATC Horizon.                       ! JDH *~
            * 07/02/92 ! Added PIPIN channel in pass to GETDEM    ! WPH *~
            * 10/22/92 ! PRR 12643 - ATC now Mode sensitive with  ! RJH *~
            *          !  calander changes.                       !     *~
            * 07/25/94 ! PRR 12017 - Show Negative CumForcast In  ! RJH *~
            *          !  <>'s even though it is not used in Calc !     *~
            *          !  of ATC.                                 !     *~
            * 07/18/96 ! Changes for the year 2000.               ! DXL *~
            * 09/30/97 ! Changed SHOWMSG to SHOWSTAT (1call)      ! MLJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "PIPATCSB" (reviewpart$, #1, #2, #3, #4, #5, #6, #7, #8, #9)

        REM Calls records from files -                                   ~
                      #1, PIPMASTR                                       ~
                      #2, HNYMASTR                                       ~
                      #3, SFCUM2                                         ~
                      #4, CALMASTR                                       ~
                      #5, PIPIN                                          ~
                      #6, PIPOUT                                         ~
                      #7, HNYDETAL                                       ~
                      #8, DEMMASTR                                       ~
                      #9, PIPCROSS

        dim                                                              ~
           date$8,                       /* Date for screen display    */~
           davail$(31)7,                 /* Displayed available        */~
           dd$(31)3,                     /* Displayed day of week name */~
           dow$(490)3,                   /*                            */~
           dpct$(31)7,                   /* Displayed pct used         */~
           dused$(31)7,                  /* Displayed used             */~
           errormsg$79,                  /* Error message              */~
           horizonmsg$18,                /* Ignore/Honor Horizon PF(11)*/~
           horizonmsg2$19,               /* Ignore/Honor Horizon       */~
           inpmessage$79,                /* Input Message              */~
           i$(24)80,                                                     ~
           i1$(24)80,                    /* Screen image               */~
           lfac$(31)1,                   /* Field attribute characters */~
           modate$(12)9,                 /* Month names                */~
           note$(31)7,                   /* For holidays               */~
           part$25,                                                      ~
           partdescr$34,                                                 ~
           pipinaltplowkey$48,                                           ~
           pipoutaltplowkey$37,                                          ~
           reviewpart$25,                                                ~
           plowkey$50,                                                   ~
           tagnr$19,                                                     ~
           tdate$8,                      /* Temporary Date             */~
           thispart$25,                                                  ~
           title$79,                                                     ~
           udate$8,                      /* Temporary Date             */~
           yymmdd$(490)6                 /* Date reference             */~

        dim                                                              ~
           atc%(490),                                                    ~
           avail%(490),                  /* HRS avail array            */~
           cumf%(490),                                                   ~
           cursor%(2),                                                   ~
           curs%(2),                                                     ~
           f1%(64),                                                      ~
           mm%(490),                     /*                            */~
           used%(490),                   /* HRS used array             */~
           yy%(490)                      /*                            */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                                                           *~
            *     INITIALIZATION AND ENTRY                              *~
            *                                                           *~
            *************************************************************

            date$=date
            call "DATEFMT" (date$)

            horizonmsg$  = "(11)Ignore Horizon"
            horizonmsg2$ = "ATC Horizon Honored"
            part$, thispart$ = reviewpart$

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

            gosub loadcalendar
            if loaded% = 0% then exit_gracefully

            search str(yymmdd$(),1) = date to cursor%() step 6
            if cursor%(1) = 0% then goto exit_gracefully
            today% = (cursor%(1)/6%) + 1%

        REM *************************************************************~
            *     M A I N   L O G I C   S E C T I O N                   *~
            *************************************************************

           errormsg$ = " "

           part$=thispart$
           mat avail% = zer : mat used% = zer : mat atc% = zer
           mat cumf% = zer
           j% = 0%
           call "PIPFLAGS"(thispart$, today%, today%, 0, #1, #3)
           call "READ100" (#1, thispart$, f1%(1))
           if f1%(1) = 0% then exit_gracefully

           call "SHOSTAT" ("Gathering PIP Data, One Moment Please")
           get #1, using L20230, str(s$,,1), avail%(), oh, ss, moq, pz,   ~
                   type%, lt%, atch%
           atch% = mod(atch%, 1000%)
*         ATCH1% = 999%
           atch1% = atch%

           REM WE ARE REUSING ARRAY NAMES TO SAVE SEG-2 SPACE SO WATCHIT

L20230:    FMT       CH(01),   /* STATUS OF RECORD           */          ~
                     XX(25),   /* SKIP PART                  */          ~
               490 * BI(04),   /* PIP ARRAY                  */          ~
                 4*PD(14,4),   /* ON-HAND, SAF-STCK, MOQ     */          ~
                    3*BI(2)    /* TYPE, LEADTIME, ATC HORIZ. */

           call "DESCRIBE" (#2, thispart$, partdescr$, 1%,   f1%(2))
           atc_toggle%, cfcst_toggle% = 0%
           title$ = "DAY     ATC-1   SHELF CUMFCST     PIP  ! DAY     ATC~
        ~-1   SHELF CUMFCST     PIP"

           init (" ") str(s$,2)
           if pos("23"=str(s$,1,1))<>0% then str(s$,2)="Surplus       "
           if pos("45"=str(s$,1,1))<>0% then str(s$,2)="Safety Stock  "
           if pos("89"=str(s$,1,1))<>0% then str(s$,2)="Critical      "

           call "READ100" (#3, thispart$, f1%(3))

           if f1%(3) <> 1% then L20442
           get #3, using L20430  , cumf%()    /* CUM SLS FCST ARRAY  */
L20430:    FMT XX(25), 490*BI(4)

L20442:    gosub calc_data
           goto seepipdata

        calc_data
           for kkkk% = 1% to 490%
                atc%(kkkk%) = avail%(kkkk%) - max(0%, cumf%(kkkk%))
           next kkkk%

           mat used% = atc%
           for kkkk% = min(489%, today%+atch1%) to today% step -1%
                used%(kkkk%) = min(atc%(kkkk%), used%(kkkk% + 1%))
           next kkkk%

           if today% = 1% then L20590
           for kkkk% = today%-1% to 1% step -1%
                used%(kkkk%) = min(atc%(kkkk%), used%(today%))
           next kkkk%
L20590:    mat atc% = avail%
           for kkkk% = min(489%, today%+atch1%) to today% step -1%
                atc%(kkkk%) = min(avail%(kkkk%), atc%(kkkk% + 1%))
           next kkkk%

           if today% = 1% then L20675
           for kkkk% = today%-1% to 1% step -1%
                atc%(kkkk%) = min(avail%(kkkk%), atc%(today%))
           next kkkk%
L20675:    return

        REM *************************************************************~
           *    S E E   PLANNED INVENTORY     B Y   M O N T H           *~
           **************************************************************

        seepipdata
           tdate$ = date
           call "DATEFMT" (tdate$, 0%, udate$)
           convert str(udate$,5%,2%) to mt%
           convert str(udate$,1%,4%) to yr%
L20760:     j%, f% = 0%
            init(hex(8e)) lfac$()
            for i% = 1% to 490%
                 if yy%(i%) < yr% then L20830
                 if yy%(i%) > yr% then L20850
                 if mm%(i%) < mt% then L20830
                 if mm%(i%) > mt% then L20850
                 if f% =  0% then f% = i%
L20830:     next i%

L20850:     l% = i% - 1%

            dd$(), davail$(), dused$(), dpct$(), note$() = " "
            j% = 0%
            for i% = f% to l%
                j% = j% + 1%
                if atc_toggle% = 0% then    /* 0% = ATC-1, 1% = ATC-2 */ ~
                         convert atc%(i%)  to  davail$(j%), pic(-######) ~
                    else convert used%(i%) to  davail$(j%), pic(-######)

                if cumf%(i%) > 0% then L20970
                    convert avail%(i%) to dused$(j%), pic(-######)
                    if f1%(3%) = 1% and cfcst_toggle% = 0%  then L20950
                        dpct$(j%) = " "
                        goto L20980

L20950:                 convert  cumf%(i%) to dpct$(j%), pic(-####)
                        dpct$(j%) = "<" & dpct$(j%) & ">"
                        goto L20980

L20970:        convert (avail%(i%) - cumf%(i%)) to dused$(j%),pic(-######)
               convert  cumf%(i%) to dpct$(j%), pic(-######)

L20980:        convert avail%(i%) to note$(j%), pic(-######)
               dd$(j%) = str(dow$(i%),1,1)
               if i% >= today% and i% <= today% + atch1% then            ~
                                                     lfac$(j%) = hex(86)
            next i%


L21010:     inpmessage$ = "Position Cursor and Press PF9 to see Daily Det~
        ~ails"

        accept                                                           ~
               at (01,02),                                               ~
        "INVENTORY PLANS FOR:",                                          ~
               at (01,22), fac(hex(84)), thispart$              , ch(25),~
               at (01,48),                                               ~
        "MONTH OF:",                                                     ~
               at (01,57), fac(hex(84)), modate$(mt%)           , ch(09),~
               at (01,67),                                               ~
               at (01,70), fac(hex(84)), yr%                   , pic(####),~
               at (02,22), fac(hex(84)), partdescr$             , ch(32),~
               at (02,60), fac(hex(a4)), horizonmsg2$           , ch(19),~
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
               at (22,02), fac(hex(a4)), inpmessage$,             ch(79),~
                                                                         ~
               at (23,19), "(4)Prev Month",                              ~
               at (23,33), "(10/26)Switch ATC/CUMFCST",                  ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), "(3)Current Month (5)Next Month",             ~
               at (24,33), fac(hex(8c)), horizonmsg$        ,ch(18)     ,~
               at (24,65), "(16)Return",                                 ~
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
                keys(hex(030405090a0b0e0f10201a)), key(hither%)

            if hither% = 32% then exit_gracefully
            if hither% =  16% then exit_gracefully
            if hither% =  9% then gosub pipdetail
            if hither% = 10% then gosub switch_atc
            if hither% <> 26% then L23430
                if cfcst_toggle% = 0% then cfcst_toggle%  = 1%           ~
                                      else cfcst_toggle%  = 0%
                goto L20760
L23430:     if hither% =  3% then seepipdata
            if hither% <> 4% then L23490
                if f% = 1% then L21010
                mt% = mm%(f%-1%)
                yr% = yy%(f%-1%)
                goto L20760
L23490:     if hither% <> 5% then L23540
                if l% = 490% then L21010
                mt% = mm%(l%+1%)
                yr% = yy%(l%+1%)
                goto L20760
L23540:     if hither% <> 15% then L23570
                call "PRNTSCRN"
                goto L21010

L23570:     if hither% <> 11% then L21010
                if str(horizonmsg$,5,1) = "H" then L23580
                    horizonmsg$  = "(11)Honor Horizon "
                    horizonmsg2$ = "ATC Horizon Ignored"
                    atch1%       =  999%
                goto L23586
L23580:             horizonmsg$  = "(11)Ignore Horizon"
                    horizonmsg2$ = "ATC Horizon Honored"
                    atch1%       =  atch%
L23586:         gosub calc_data : goto L20760

        switch_atc
            j% = 0%
            for i% = f% to l%
                j% = j% + 1%
                if atc_toggle% = 0% then                                 ~
                     convert used% (i%) to  davail$(j%), pic(-######)    ~
                else convert atc%  (i%) to  davail$(j%), pic(-######)
            next i%
            if atc_toggle% = 0% then L23700
            str(title$,13,1) = "1" : str(title$,54,1) = "1"
            goto L23710
L23700:     str(title$,13,1) = "2" : str(title$,54,1) = "2"
L23710:     if atc_toggle% = 0% then atc_toggle% = 1%                    ~
                                else atc_toggle% = 0%
            return


        REM *************************************************************~
            * Display Planned Inventory Position Details                *~
            *************************************************************
        pipdetail

            close ws
            call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
            sub% = min(max(1%,cursor%(1)-3%),16%)
            if sub% = 16% then L27110
                if sub% < 1% or sub% > 15% then return
                if cursor%(2) < 40 then L27120
L27110:     sub% = sub% + 15%
L27120:     if dd$(sub%)=" " then return
            sub% = sub%+f%-1%
            i$() = " "
            for i=1 to 8
                str(i$(i),39,1) = "!"
            next i
            keyhit% = 0%
            init ("-") str(i$(9),,79)
            i$(10)="Store     Lot  Code    Quantity  Text"

L27220:     put pipinaltplowkey$, using L27230, thispart$, sub%, hex(00)
L27230:         FMT CH(25),BI(4),CH(19)
L27240:     cursor%(1), v% = 0%
            for i=1 to 8
                str(i$(i),,38) = " "
            next i
L27280:     call "PLOWALTS" (#5, pipinaltplowkey$, 1%, 29%, f1%(5))
            if f1%(5) = 0% then L27360
            v%, cursor%(1) = cursor%(1)+1%
            get #5, using L27320, str(i$(v%),2,19), qty
L27320:         FMT XX(29),CH(19),PD(14,4)
            call "CONVERT" (qty, 2.2, str(i$(v%),25,10))
            if cursor%(1) < 8% then L27280

L27360:     if v%<8% then str(i$(v%+1%),05,25)="* * * END OF FILE * * *"
            if keyhit% > 3% then L27740

L27390:     put pipoutaltplowkey$, using L27400, thispart$, sub%, hex(00)
L27400:         FMT CH(25),BI(4),CH(8)
L27410:     cursor%(2),v% = 0%
            for i = 1 to 8
                str(i$(i),40,39) = " "
            next i
L27450:     call "PLOWALTS" (#6, pipoutaltplowkey$, 1%, 29%, f1%(6))
            if f1%(6) = 0% then L27530
            v%,cursor%(2)=cursor%(2)+1%
            get #6, using L27490,str(i$(v%),42,19),qty
L27490:         FMT CH(19),XX(37),PD(14,4)
            call "NUMPRINT" (qty,2,str(i$(v%),65,10))
            if cursor%(2)<8% then L27450

L27530:     if v%<8% then str(i$(v%+1%),45,25)="* * * END OF FILE * * *"
            if keyhit%>3% then L27740

L27560:     put plowkey$, using L27570, thispart$," "
L27570:         FMT CH(25),CH(17)
L27580:     vv%=0%:for i=11 to 18: str(i$(i),,79) = " " :next i
L27590:     call "PLOWNEXT" (#7,plowkey$,25%,f1%(7))
            if f1%(7)=0 then L27720
           get #7, using L27630,str(i$(vv%+11%),3,3),str(i$(vv%+11%),8,6),~
            temp$,str(i$(vv%+11%),18,2),qty,str(i$(vv%+11%),34,40)
L27630:    FMT XX(25),CH(3),CH(6),XX(8),CH(6),CH(2),PD(14,4),XX(24),CH(40)
            if str(temp$,,6)<>yymmdd$(sub%) then L27690
            call "CONVERT" (qty, 2.2, str(i$(vv%+11%),22,10))
            vv% = vv%+1%
            if vv% < 8% then L27590 else goto L27720

L27690:     str(i$(vv%+11%),,79) = " "
            goto L27590

L27720:    if vv%<8% then str(i$(vv%+11%),29)="* * * END OF FILE * * *"

L27740:     temp$=yymmdd$(sub%)
            call "DATEFMT" (temp$)
            inpmessage$ = "Position Cursor and Press RETURN to see Top Le~
        ~vel Demand"

L27770: accept                                                           ~
               at (01,02), "Planned Inventory Position Details for",     ~
               at (01,41), fac(hex(84)), thispart$              , ch(25),~
               at (01,67), "as of",                                      ~
               at (01,73), fac(hex(84)), temp$                  , ch(08),~
               at (02,41), fac(hex(8c)), partdescr$             , ch(32),~
               at (03,03),                                               ~
        "IN Tag Number            Quantity       OUT Tag Number          ~
        ~ Quantity",                                                      ~
               at (04,02), fac(hex(84)), i$( 1)                 , ch(79),~
               at (05,02), fac(hex(84)), i$( 2)                 , ch(79),~
               at (06,02), fac(hex(84)), i$( 3)                 , ch(79),~
               at (07,02), fac(hex(84)), i$( 4)                 , ch(79),~
               at (08,02), fac(hex(84)), i$( 5)                 , ch(79),~
               at (09,02), fac(hex(84)), i$( 6)                 , ch(79),~
               at (10,02), fac(hex(84)), i$( 7)                 , ch(79),~
               at (11,02), fac(hex(84)), i$( 8)                 , ch(79),~
               at (12,02), fac(hex(84)), i$( 9)                 , ch(79),~
               at (13,02), fac(hex(84)), i$(10)                 , ch(79),~
               at (14,02), fac(hex(84)), i$(11)                 , ch(79),~
               at (15,02), fac(hex(84)), i$(12)                 , ch(79),~
               at (16,02), fac(hex(84)), i$(13)                 , ch(79),~
               at (17,02), fac(hex(84)), i$(14)                 , ch(79),~
               at (18,02), fac(hex(84)), i$(15)                 , ch(79),~
               at (19,02), fac(hex(84)), i$(16)                 , ch(79),~
               at (20,02), fac(hex(84)), i$(17)                 , ch(79),~
               at (21,02), fac(hex(84)), i$(18)                 , ch(79),~
               at (22,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (23,03),                                               ~
        "(2)Prev day (4)First PIPIN (6)First PIPOUT  (8)First Detail   (1~
        ~5)Print Screen",                                                 ~
               at (24,03),                                               ~
        "(3)Next day (5)Next PIPIN  (7)Next PIPOUT   (9)Next Detail    (1~
        ~6)Return",                                                       ~
                                                                         ~
            keys(hex(0002030405060708090d0f10)), key(keyhit%)

            if keyhit%=16% then return
            if keyhit%<>15% then L28180
                call "PRNTSCRN"
                goto L27770
L28180:     if keyhit%<>13% then L28210
                call "MANUAL" ("PLNRSUB")
                goto L27770
L28210:     if keyhit%<>2% then L28240
               sub%=max(1%,sub%-1%)
               goto L27220
L28240:     if keyhit%<>3% then L28270
               sub%=min(490%,sub%+1%)
               goto L27220
L28270:     if keyhit%=4% then L27220
            if keyhit%=5% and cursor%(1)= 8% then L27240
            if keyhit%=6% then L27390
            if keyhit%=7% and cursor%(2)= 8% then L27410
            if keyhit%=8% then L27560
            if keyhit%=9% and vv%=8% then L27580

            if keyhit% <> 0%  then L28520
           mat curs% = zer : ret% = 0 : tagnr$, demand$, type$ = " "
           close ws
           call "SCREEN" addr ("C", u3%, "I", i1$(), curs%())

             if curs%(1) >= 12 or curs%(1) <= 3 then L27770
             if curs%(2) < 1 or curs%(2) > 80 then L27770
             curs%(1) = curs%(1) - 3

             if curs%(2) <= 40 then                                      ~
                            str(tagnr$,,19) = str(i$(curs%(1)), 2,19)    ~
                               else                                      ~
                            str(tagnr$,,19) = str(i$(curs%(1)),42,19)

             if tagnr$ = " " then L27770
             if tagnr$ = "   * * * END OF FIL"  then L27770
           call "GETDEM"(1%, tagnr$ , #9, #8, #5, demand$, type$, ret%)

L28520:     goto L27770


        REM *************************************************************~
            * Load calendar one time.                                   *~
            *   -just to take the burden off any calling program        *~
            *************************************************************

        loadcalendar
            if loaded% = 1% then return
            call "READ100" (#4,"10", f1%(4))
                if f1%(4) = 0 then return
            get #4, using L35100, str(yymmdd$(),,1470)
L35100:         FMT XX(2), CH(1470)

            call "READ100" (#4,"11", f1%(4))
                if f1%(4) = 0 then return
            get #4, using L35100, str(yymmdd$(),1471,1470)

            call "READ100" (#4,"20", f1%(4))
                if f1%(4) = 0 then return
            get #4, using L35190, yy%()
L35190:         FMT XX(2), 490*BI(4)

            call "READ100" (#4,"30", f1%(4))
                if f1%(4) = 0 then return
            get #4, using L35190, mm%()

            call "READ100" (#4,"50", f1%(4))
                if f1%(4) = 0 then return
            get #4, using L35280, dow$()
L35280:         FMT XX(2), 490*CH(3)
            loaded% = 1%
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

            end

