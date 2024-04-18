        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   L      N   N  M   M   AAA   X   X  IIIII  M   M   *~
            *  P   P  L      NN  N  MM MM  A   A   X X     I    MM MM   *~
            *  PPPP   L      N N N  M M M  AAAAA    X      I    M M M   *~
            *  P      L      N  NN  M   M  A   A   X X     I    M   M   *~
            *  P      LLLLL  N   N  M   M  A   A  X   X  IIIII  M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLNMAXIM - ITERATIVELY, IF NECESSARY, DETERMINE MAXIMUM   *~
            *            FEASIBLE QUANTITY BY DATE ENTERED.  ALLOW FOR  *~
            *            NEGATIVE PIP OR SAFETY STOCK INTRUSION EXTERN- *~
            *            ALLY TO PLANSUB.  RETURNS QUANTITY IN ERRORMSG *~
            *            AS OUTPUT. USES TYPE 8 DEMAND                  *~
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
            * 06/11/84 ! ORIGINAL                                 ! KEN *~
            * 11/04/88 ! Pass HNYALTRS to PLBNRSUB                ! KEN *~
            * 09/16/96 ! Millie date conversion                   ! DER *~
            * 09/30/97 ! Changed SHOWMSG to SHOSTAT (1 call)      ! MLJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "PLNMAXIM"                                                   ~
           (errormsg$,                   /* ERROR MESSAGE              */~
            mbom$,                       /* BILL OF MATERIALS          */~
            mdate$,                      /* DUE DATE                   */~
            mpart$,                      /* PART                       */~
            mpriority$,                  /* PRIORITY                   */~
            mquantity$,                  /* MAXIMUM QUANTITY           */~
            today%,                                                      ~
                     #4,                 /* HNYMASTR                   */~
                     #16,                /* HNYALTRS                   */~
                     #15,                /* BOMMASTR                   */~
                     #2,                 /* PIPMASTR                   */~
                     #7,                 /* RTEMASTR                   */~
                     #8,                 /* JBCROSS2                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT   WC-CROSSREFERENCE  */~
                     #33,                /* PIPIN (QUANTS ADDED/DAY)   */~
                     #34,                /* PIPOUT (QUANTS SUBTR/DAY)  */~
                     #40,                /* SFMASTR2  SALES FORECASTS  */~
                     #41,                /* SFCUM2  CUMULATIVE FCSTS   */~
                     #35,                /* PIPCROSS                   */~
                     #24,                /* ENGMASTR                   */~
                     #14,                /* BOMSPEC                    */~
                     #36, #6,            /* JBPIPXRF, RTEALTRS         */~
                     #62, #63,                                           ~
                     #1, #5, #12)        /* NEEDED FOR PLNRSUB         */~

        com planflags$(25)20,                                            ~
            yymmdd$(490)6,                                               ~
            eff$(490)3,                  /* EFF, PLT WORK ARRAY        */~
            oldcompplowkey$(100)31,      /* FOR PHANTOM LOGIC          */~
            pip%(490),                                                   ~
            cumf%(490),                                                  ~
            awca%(490),                  /* CONCURRENT WC AVAILABILITY */~
            awcu1%(490),                 /* 1ST CONCURRENT USED        */~
            awcu2%(490),                 /* 2ND CONCURRENT USED        */~
            awcu3%(490),                 /* 3RD CONCURRENT USED        */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            phfact(101),                                                 ~
                               /* THE ABOVE ELEMENTS CANNOT BE CHANGED */~
            part$    (1000)25,                                           ~
            intagnr$ (1000)19,                                           ~
            outtagnr$(1000)19,                                           ~
            rte$     (1000)3,            /* WHICH ROUTE TO USE         */~
            bom$     (1000)3,            /* WHICH BOM TO USE           */~
            ed%      (1000),                                             ~
            sd%      (1000),                                             ~
            parline% (1000),             /* PARENT LINE NUMBER         */~
            action%  (1000),             /* ACTION TO TAKE             */~
            lt%      (1000),             /* LEADTIME ARRAY             */~
            moq%     (1000),             /* MOQ                        */~
            type%    (1000),             /* PART TYPE                  */~
            qtyu     (1000),             /* QTY USED (NEEDED)          */~
            qtyp     (1000),             /* QTY TO PROCURE             */~
                               /* THE ABOVE ELEMENTS ARE THE MATERIALS */~
            wc$(2000)4,                  /* WORK CENTER                */~
            wl$(2000)1,                  /* JUST FOR SUMMARY REPORT    */~
            wa$(2000)1,                  /* IN CASE OF ARRAY OVERFLOW  */~
            ws$(2000)5,                  /* WC STEP #                  */~
            wl%(2000),                   /* LINE STACK                 */~
            du%(2000),                   /* DATE USED ARRAY STACK      */~
            au%(2000),                   /* AMOUNT USED ARRAY STACK    */~
            su%(2000),                   /* SET UP TIME TODAY          */~
            wa%(2000),                   /* WC ALTERNATE SEQ NO.       */~
                               /* THESE ARE THE WORK CENTER ARRAYS     */~
            rtestep$(255)200,            /* THE STEP AS A STRING       */~
            step$   (255)5,              /* STEP                       */~
            yld     (255),               /* STEP YIELD                 */~
            pd%     (255),               /* WC STEP START DATE(SPL PCK)*/~
            mmx%    (255),               /* START OF RTE STEP          */~
            xsd%    (255)                /* START OF RTE STEP          */~
                               /* THESE ARE FOR ROUTE STEPS            */~

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            plnerror$79,                 /* ERROR MESSAGE              */~
            statusmsg$79,                /* ERROR MESSAGE              */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            mbom$3,                      /* BILL OF MATERIALS          */~
            mdate$8,                     /* DUE DATE                   */~
            mincr$10,                    /* INCREMENT                  */~
            mngpip$10,                   /* NEG PIP ADJUSTMENT         */~
            mpart$25,                    /* PART                       */~
            mpartdescr$32,               /* PART DESCRIPTION           */~
            mpriority$1,                 /* PRIORITY                   */~
            mquantity$10,                /* MAXIMUM QUANTITY           */~
            nquantity$10,                /* MINIMUM QUANTITY           */~
            pipdescr$32,                                                 ~
            temp$16, temp1$3


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            if mpart$<>" " then L10182

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************
        inputmode
        init (" ")                                                       ~
            mbom$,                       /* BILL OF MATERIALS          */~
            mdate$,                      /* DUE DATE                   */~
            mpart$,                      /* PART                       */~
            mpriority$,                  /* PRIORITY                   */~
            mquantity$                   /* MAXIMUM QUANTITY           */~

L10182: init (" ")                                                       ~
            mincr$,                      /* INCREMENT                  */~
            mngpip$,                     /* NEG PIP ADJUSTMENT         */~
            mpartdescr$,                 /* PART DESCRIPTION           */~
            nquantity$,                  /* MAXIMUM QUANTITY           */~
            plandet$,                                                    ~
            type$,                                                       ~
            errormsg$, inpmessage$, plnerror$, pipdescr$

            for fieldnr% = 1 to  7
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10330
L10290:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10290
L10330:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10290
                next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************
        editmode
L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       test_feasibility
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  9 then L11060
            if fieldnr% = 6 or fieldnr% = 7 then fieldnr% = 5
            if fieldnr% > 7 then fieldnr% = fieldnr% - 2%
L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        test_feasibility
            mquant=mquantity + mngpip
            gosub L30000
            if finaldate% > 490% then L19120
                base=mquant
                goto L19280
L19120:     if nquantity=mquantity then L19150
            mquant=nquantity + mngpip
            gosub L30000
            if finaldate% > 490 then L19150 else L19170
L19150:         errormsg$="MINIMUM QUANTITY CANNOT BE PROCURED"
                goto editmode

L19170:     base=nquantity
            test=(mquantity-nquantity)/mincr
            test=abs(int(-test))
            if abs(test)>=2.00000 then L19200
            if finaldate%>490% then L19150 else L19280
L19200:     test=(log(test)/log(2))
            test%=abs(int(-test))

            for i%=test%-1% to 0% step -1%
            mquant=base + ((2^i%)*mincr) + mngpip
            if mquant>=mquantity then L19270
            gosub L30000
            if finaldate%<490% then base=mquant
L19270:     next i%

L19280:     errormsg$=" "
            call "NUMSMASH" (base, 2, errormsg$)
            errormsg$= errormsg$ & " PARTS " & mpart$
            errormsg$=errormsg$ & " MAY BE BUILT BY " & mdate$
            goto editmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* PART             */~
                                    L20200,         /* PRIORITY         */~
                                    L20300,         /* BILL OF MATERIALS*/~
                                    L20400,         /* DUE DATE         */~
                                    L20500,         /* MAXIMUM QUANTITY */~
                                 /* 20600,         /* MINIMUM QUANTITY */~
                                 /* 20700,         /* INCREMENT        */~
                                    L20800,         /* NEG PIP ADJ      */~
                                    L20900          /* REPORT           */
                     return
L20100:     REM DEFAULT/ENABLE FOR PART
                return
L20200:     REM DEFAULT/ENABLE FOR PRIORITY
                return
L20300:     REM DEFAULT/ENABLE FOR BILL OF MATERIALS
                return
L20400:     REM DEFAULT/ENABLE FOR DUE DATE
                return
L20500:     REM DEFAULT/ENABLE FOR MAXIMUM QUANTITY
            if mquantity$=" " then mquantity$="1.00"
            if nquantity$=" " then nquantity$="1.00"
        REM     RETURN
            REM DEFAULT/ENABLE FOR MINIMUM QUANTITY
        REM     RETURN
            REM DEFAULT/ENABLE FOR INCREMENT
            if mincr$=" " then mincr$="1.00"
                return
L20800:     REM DEFAULT/ENABLE FOR NEG PIP ADJUSTMENT
            if mngpip$=" " then mngpip$=".00"
            if mngpip = 0 then enabled% = 0%
                return
L20900:     REM DEFAULT/ENABLE FOR REPORT
            plandet$="NO"
                return


L28000: rem**************************************************************~
           *                   c a l c i w                              *~
           *                                                            *~
           *  this routine calculates the amount that can be withdrawn  *~
           *  from planned inventory as of ed%(l%) and sets qtyp(l%)    *~
           **************************************************************

           mat cumf% = zer
           mpspip, mngpip = 0
           call "READ100"(#2, mpart$, f1%(2))
              if f1%(2%) = 0% then L28610

           atc% = 0%

           get #2, using L28230, pip%(), ss, atch%
L28230:         FMT XX(26), 490*BI(4), XX(8), PD(14,4), XX(20), BI(2)
           atch% = mod(atch%, 1000%)

           call "READ100" (#41, mpart$, f1%(41))
              if f1%(41%) <> 1% then goto L28410
           get #41, using L28390, cumf%()
L28390:       FMT XX(25), 490*BI(4)

L28410: REM CALCULATE THE AVAILABLE TO COMMIT
           atc% = 2000000000%
           for i% = cd% to min(490%, cd% + atch%)
                atc% = min(atc%, (pip%(i%) - max(0, cumf%(i%))))
           next i%
           goto L28540
        REM ONLY COMMIT THE SAFETY STOCK FOR SALES ORDER DEMANDS
                atc% = atc% - int(ss)

L28540: REM CALCULATE NEGATIVE PIP ADJUSTMENT

            mngpip=max(-atc%,0):mpspip=max(atc%,0)
L28610:     call "NUMSMASH" (mngpip, 2, mngpip$)
            temp=atc%:call "NUMSMASH" (temp, 2, temp$)
            pipdescr$="THE PIP POSITION IS " & temp$
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29918:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L29942, L29948, L29952
               return

L29942:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L29952:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L29918

L30000: REM *************************************************************~
            *                                                           *~
            *    PLAN OR TEST FEASIBILITY                               *~
            *                                                           *~
            *************************************************************

        REM NOW DECIDE WHAT TO DO

            planflag%=0%
            firstdate%,finaldate%=0%

            statusmsg$="CHECKING DEMAND:"
            if plandet$="NO" then                                        ~
               statusmsg$=statusmsg$ & " " & "NO PRINTED REPORT"
            if plandet$="YES" then                                       ~
               statusmsg$=statusmsg$ & " " & "WILL PRINT FULL REPORT"
            if str(planflags$(),1,1) <> "Y" then                         ~
             call "SHOSTAT" (statusmsg$)

            if plandet$="YES" then planflag%=10%
            plnerror$, temp$, temp1$ = " "
        call"PLANSUB" (temp$,            /* DEMAND CODE                */~
                     temp1$,             /* DEMAND LINE                */~
                     "8",                /* DEMAND TYPE                */~
                     mpriority$,         /* DEMAND PRIORITY            */~
                     mpart$,             /* PART NEEDED                */~
                     mquant,             /* QUANTITY NEEDED            */~
                     cd%,                /* REQ'D COMPL DATE           */~
                     "   ",              /* DELIVER TO WAREHOUSE       */~
                     "   ",              /* THE WC ROUTE TO USE        */~
                     mbom$,              /* WHICH BOM TO USE           */~
                     plnerror$,          /* THE RETURN MESSAGE         */~
                     firstdate%,         /* FIRST DATE PASSED BACK     */~
                     finaldate%,         /* DATE PLANNED FOR           */~
                     today%,             /* SUBSCRIPT FOR TODAY'S DATE */~
                     planflag%,          /* >0= PLAN, 0 = CHECK ONLY   */~
                     #16,                /* HNYALTRS                   */~
                     #15,                /* BOMMASTR                   */~
                     #2,                 /* PIPMASTR                   */~
                     #7,                 /* RTEMASTR                   */~
                     #8,                 /* JBCROSS2                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT   WC-CROSSREFERENCE  */~
                     #33,                /* PIPIN (QUANTS ADDED/DAY)   */~
                     #34,                /* PIPOUT (QUANTS SUBTR/DAY)  */~
                     #40,                /* SFMASTR2  SALES FORECASTS  */~
                     #41,                /* SFCUM2  CUMULATIVE FCSTS   */~
                     #35,                /* PIPCROSS                   */~
                     #24,                /* ENGMASTR                   */~
                     #14,                /* BOMSPEC                    */~
                     #36,                /* JBPIPXRF                   */~
                     #6,                 /* RTEALTRS                   */~
                     #62,#63)

        REM NOW FIND OUT HOW WE DID
            if errormsg$ = "RUN ABORTED BY OPERATOR" then L30600
            return

L30600:     return clear
            goto editmode

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40210,         /* PART             */~
                                    L40210,         /* PRIORITY         */~
                                    L40210,         /* BILL OF MATERIALS*/~
                                    L40210,         /* DUE DATE         */~
                                    L40240,         /* MAXIMUM QUANTITY */~
                                /*  40240,         /* MINIMUM QUANTITY */~
                                /*  40240,         /* INCREMENT        */~
                                    L40240,         /* NEG PIP ADJ      */~
                                    L40210          /* REPORT           */
                     goto L40280

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40210:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40240:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40280:     accept                                                       ~
               at (01,02),                                               ~
                  "DETERMINE MAXIMUM FEASIBLE QUANTITY BY DATE",         ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PART",                                                ~
               at (06,30), fac(lfac$( 1)), mpart$               , ch(25),~
               at (06,60), "PART TYPE-",                                 ~
               at (06,70), fac(hex(8c))  , type$                , ch( 3),~
               at (07,02),                                               ~
                  "PRIORITY",                                            ~
               at (07,30), fac(lfac$( 2)), mpriority$           , ch(01),~
               at (07,45), fac(hex(8c))  , mpartdescr$          , ch(32),~
               at (08,02),                                               ~
                  "BILL OF MATERIALS",                                   ~
               at (08,30), fac(lfac$( 3)), mbom$                , ch(03),~
               at (09,02),                                               ~
                  "DUE DATE",                                            ~
               at (09,30), fac(lfac$( 4)), mdate$               , ch(08),~
               at (09,49), fac(hex(8c)), pipdescr$              , ch(32),~
               at (10,02),                                               ~
                  "MAXIMUM QUANTITY",                                    ~
               at (10,30), fac(lfac$( 5)), mquantity$           , ch(10),~
               at (11,02),                                               ~
                  "MINIMUM QUANTITY",                                    ~
               at (11,30), fac(lfac$( 5)), nquantity$           , ch(10),~
               at (12,02),                                               ~
                  "INCREMENT",                                           ~
               at (12,30), fac(lfac$( 5)), mincr$               , ch(10),~
               at (13,02),                                               ~
                  "NEG PIP ADJUSTMENT",                                  ~
               at (13,30), fac(lfac$( 6)), mngpip$              , ch(10),~
               at (14,02),                                               ~
                  "PRINT FULL REPORT",                                   ~
               at (14,30), fac(lfac$( 7)), plandet$             , ch( 3),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,02),                                               ~
                  "(2)REVIEW FUNCTIONS",                                 ~
               at (24,68),                                               ~
                  "(16)RETURN",                                          ~
                                                                         ~
               keys(hex(0001020d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 2 then L40770
                  gosub L60000
                  goto L40280

L40770:        if keyhit% <> 13 then L40810
                  call "MANUAL" ("PLNMAXIM")
                  goto L40280

L40810:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40280

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41210,         /* PART             */~
                                    L41210,         /* PRIORITY         */~
                                    L41210,         /* BILL OF MATERIALS*/~
                                    L41210,         /* DUE DATE         */~
                                    L41240,         /* MAXIMUM QUANTITY */~
                                 /* 41240,         /* MINIMUM QUANTITY */~
                                 /* 41240,         /* INCREMENT        */~
                                    L41240,         /* NEG PIP ADJ      */~
                                    L41210          /* REPORT           */
                     goto L41280

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41210:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41240:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41280:     accept                                                       ~
               at (01,02),                                               ~
                  "DETERMINE MAXIMUM FEASIBLE QUANTITY BY DATE",         ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PART",                                                ~
               at (06,30), fac(lfac$( 1)), mpart$               , ch(25),~
               at (06,60), "PART TYPE-",                                 ~
               at (06,70), fac(hex(8c))  , type$                , ch( 3),~
               at (07,02),                                               ~
                  "PRIORITY",                                            ~
               at (07,30), fac(lfac$( 2)), mpriority$           , ch(01),~
               at (07,45), fac(hex(8c))  , mpartdescr$          , ch(32),~
               at (08,02),                                               ~
                  "BILL OF MATERIALS",                                   ~
               at (08,30), fac(lfac$( 3)), mbom$                , ch(03),~
               at (09,02),                                               ~
                  "DUE DATE",                                            ~
               at (09,30), fac(lfac$( 4)), mdate$               , ch(08),~
               at (09,49), fac(hex(8c)), pipdescr$              , ch(32),~
               at (10,02),                                               ~
                  "MAXIMUM QUANTITY",                                    ~
               at (10,30), fac(lfac$( 5)), mquantity$           , ch(10),~
               at (11,02),                                               ~
                  "MINIMUM QUANTITY",                                    ~
               at (11,30), fac(lfac$( 5)), nquantity$           , ch(10),~
               at (12,02),                                               ~
                  "INCREMENT",                                           ~
               at (12,30), fac(lfac$( 5)), mincr$               , ch(10),~
               at (13,02),                                               ~
                  "NEG PIP ADJUSTMENT",                                  ~
               at (13,30), fac(lfac$( 6)), mngpip$              , ch(10),~
               at (14,02),                                               ~
                  "PRINT FULL REPORT",                                   ~
               at (14,30), fac(lfac$( 7)), plandet$             , ch( 3),~
                                                                         ~
               at (19,02), fac(hex(84)),   plnerror$            , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,22),                                               ~
                  "(4)TEST FEASIBILITY",                                 ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,02),                                               ~
                  "(2)REVIEW FUNCTIONS",                                 ~
               at (24,22),                                               ~
                  "(3)PLANNING SWITCHES",                                ~
               at (24,68),                                               ~
                  "(16)RETURN",                                          ~
                                                                         ~
               keys(hex(00010203040d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 2 then L41770
                  gosub L60000
                  goto L41280

L41770:        if keyhit% <> 3 then L41801
                  gosub L61000
                  goto L41280

L41801:        if keyhit% <> 13 then L41810
                  call "MANUAL" ("PLNMAXIM")
                  goto L41280

L41810:        if keyhit% <> 15 then L41850
                  call "PRNTSCRN"
                  goto L41280

L41850:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  plnerror$, errormsg$ = " "
                  on fieldnr% gosub L50100,         /* PART             */~
                                    L50200,         /* PRIORITY         */~
                                    L50300,         /* BILL OF MATERIALS*/~
                                    L50400,         /* DUE DATE         */~
                                    L50500,         /* MAXIMUM QUANTITY */~
                                /*  50600,         /* MINIMUM QUANTITY */~
                                /*  50700,         /* INCREMENT        */~
                                    L50800,         /* NEG PIP ADJ      */~
                                    L50900          /* REPORT           */
                     return
L50100:     REM TEST DATA FOR PART CODE
            call "READ100" (#4, mpart$, f1%(4))
                  if f1%(4) = 0 then goto L50132
                  get #4, using L50108, mpartdescr$, type$, nquantity$
L50108:              FMT XX(25), CH(32), XX(122), CH(10), CH(10)
                  convert type$ to type%, data goto L50144
                  convert nquantity$ to nquantity, data goto L50144
                  call "NUMSMASH" (nquantity, 2, nquantity$)
                  if type%>0% and type%<200% then L50140
                  if type%=0% then L50136
            return
L50132:           errormsg$ = "PART NOT IN INVENTORY"
                  return
L50136:           errormsg$="BUILD TO OPTION PART, PLAN VIA SALES ORDER"
                  return
L50140:           errormsg$="UNPLANNED ITEM TYPE:" & type$
                  return
L50144:           errormsg$="PART MASTER FILE ERROR, PLEASE CHECK"
                  return
L50200:     REM TEST DATA FOR PRIORITY
                if mpriority$<"A" or mpriority$>"Z" then L50230
                return
L50230:              errormsg$="PRIORITY MUST BE A TO Z"
                     return
L50300:     REM TEST DATA FOR BILL OF MATERIALS
                return
L50400:     REM TEST DATA FOR DUE DATE
                call "DATEOK" (mdate$, u3%, errormsg$)
                if errormsg$ <> " "  then return
                convert u3% to temp$, pic(########)
                call "DATECONV" (temp$)
                search str(yymmdd$(),1)=str(temp$,1,6) to cursor%() step 6
                if cursor%(1)=0% then L50434
                cd%=1%+(cursor%(1)/6)
                if cd%>=today% then L28000
                    errormsg$="BEFORE TODAY, DEMAND NOT FEASIBLE"
                    return

L50434:         errormsg$ = "REQUESTED COMPLETION DATE IS NOT WITHIN THE ~
        ~PLANNING PERIOD, RESPECIFY"
                return
L50500:     REM TEST DATA FOR MAXIMUM QUANTITY
                convert mquantity$ to mquantity, data goto L50655
                mquantity=abs(round(mquantity,2))
                if mquantity < 1 then mquantity = 1
                call "NUMSMASH" (mquantity, 2, mquantity$)
            REM TEST DATA FOR MINIMUM QUANTITY
                convert nquantity$ to nquantity, data goto L50655
                nquantity=abs(round(nquantity,2))
                if nquantity < 1 then nquantity = 1
                call "NUMSMASH" (nquantity, 2, nquantity$)
                if nquantity<=mquantity then L50700
                     errormsg$="MINIMUM EXCEEDS MAXIMUM"
                     return
L50655:              errormsg$="INVALID NUMERIC ENTRY"
                     return
L50700:     REM TEST DATA FOR INCREMENT
                convert mincr$ to mincr, data goto L50655
                mincr=abs(round(mincr,2))
                if mincr < 1 then mincr=1
                call "NUMSMASH" (mincr, 2, mincr$)
                return
L50800:     REM TEST DATA FOR NEG PIP ADJUSTMENT
                convert mngpip$ to mngpip, data goto L50655
                mngpip=abs(round(mngpip,2))
                mngpip=max(mngpip,0)
                call "NUMSMASH" (mngpip, 2, mngpip$)
                return
L50900:     REM TEST DATA FOR REPORT
                if plandet$="YES" then return
                if plandet$="NO" then return
                   errormsg$="YES OR NO, PLEASE"
                   return

L60000: REM *************************************************************~
            *                                                           *~
            *   ENTRY TO REVIEW FUNCTIONS                               *~
            *                                                           *~
            *************************************************************

            reviewpart$=mpart$
            mode%=0%
            call "PLNRSUB" (mode%, reviewpart$,                          ~
                            #1, #2, #4, #7, #11, #12, #15, #23,          ~
                            #24, #33, #34, #35, #40, #41, #5, #6, #16)

            return

L61000: REM *************************************************************~
            * PLANNING SYSTEM SWITCHES                                  *~
            *************************************************************

              err% = today%
                 call "PLNFLSUB" (planflags$(), err%)
              return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end
