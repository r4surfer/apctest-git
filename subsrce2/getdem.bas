        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   EEEEE  TTTTT  DDDD   EEEEE  M   M                 *~
            *  G      E        T    D   D  E      MM MM                 *~
            *  G GGG  EEEE     T    D   D  EEEE   M M M                 *~
            *  G   G  E        T    D   D  E      M   M                 *~
            *   GGG   EEEEE    T    DDDD   EEEEE  M   M                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GETDEM   - SUBROUTINE TO FIND TOP LEVEL DEMAND GIVEN A TAG*~
            *            NUMBER.  WILL DISPLAY THE CHAIN OF JOBS LEADING*~
            *            UPWARD TO DEMAND (MODE = 1) OR IF MODE = 2 JUST*~
            *            FIND THE DEMAND AN PASS IT BACK TO THE CALLER. *~
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
            * 04/24/85 ! ORIGINAL                                 ! WPH *~
            * 05/10/88 ! FIXED TEST FOR BROKEN CHAIN @ 16320      ! RJM *~
            *          !   & REMOVED EXCESS AND DEAD CODE         !     *~
            * 01/10/92 ! Now clears part, etc, for cancelled job  ! WPH *~
            * 07/01/92 ! PRR 12513-Changed to get qty & date info ! WPH *~
            *          !  from  PIPIN file rather than PIPCROSS   !     *~
            * 11/08/93 ! Purchase Job Project - Added Support for ! JBK *~
            *          !  'BW' & 'RW' tags, Added code for        !     *~
            *          !  multiple demands pointing to same PIP   !     *~
            *          !  Tag.  Updated screen in several places. !     *~
            *          ! PRR 13045- Corrected display when PIPIN  !     *~
            *          !  record is gone.                         !     *~
            * 07/16/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "GETDEM" (mode%,tagnumber$, #1, #2, #4,     demstring$,  ~
                             type$,  return%)

            REM  MODE = 1 WILL FIND THE DEMAND AND DISPLAY THE PATH      ~
                 MODE = 2 WILL ONLY FIND THE DEMAND AND PASS IT BACK     ~
                 #1 IS PIPCROSS FILE                                     ~
                 #2 IS DEMMASTR FILE                                     ~
                 #4 is PIPIN                                             ~
                 RETURN% = 1 TRACE SUCCESSFUL                            ~
                           2 UPPER LEVEL JOB CANCELLED, CAN'T TRACE      ~
                           3 TRACE CANCELLED BY USER                     ~
                           4 PEGGING CHAIN BROKEN, TRACE NOT SUCCESSFULL ~
                           5 NO PEGGING LINKAGE FOUND



        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            demand$19,                   /* DEMAND FOUND IN PIPCROSS   */~
            demcust$9,                   /* CUSTOMER IF SO DEMAND      */~
            demand_count$3,              /* Number of Demands          */~
            demand_index$3,              /* Number of Demands Viewed   */~
            demand_msg$40,               /* Multiple Demands Message   */~
            demquan$10,                  /* DEMAND QUANTITY            */~
            dempart$25,                  /* DEMAND PART NUMBER         */~
            dempcd$8,                    /* DEMAND PLANNED COMPL. DATE */~
            demstatus$1,                 /* STATUS OF DEMAND           */~
            dempriority$1,               /* DEMAND PRIORITY A-Z        */~
            bline1$79,                   /* COLUMN HEADDING UNDERLINE  */~
            bline2$79,                   /* COLUMN HEADDING UNDERLINE  */~
            demstring$19,                /* TOP LEVEL DEMAND           */~
            demtype$1,                   /* DEMAND TYPE                */~
            end$(30)8,                   /* END DATE FOR JOB           */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            infomsg$59,                  /* WHY FAILED OR TYPE OF DEM  */~
            jobnumb$(30)19,              /* JOB NUMBER                 */~
            pipout$(30)19,               /* PIPOUT (USE) FROM PIPCROSS */~
            plbase$6,                    /* Base date of Plan Calendar */~
            quant$(30)10,                /* QUANTITY TO BUILD FOR JOB  */~
            partno$(30)25,               /* PART TO BUILD FOR JOB      */~
            pf$(2)79, pfk$22,            /* PF Descrs and Keys         */~
            start$(30)8,                 /* START DATE FOR JOB         */~
            tagnumber$19,                /* TAG NUMBER PASSED          */~
            type$1,                      /* DEMAND TYPE PASSED BACK    */~
            readkey$99,                                                  ~
            plowkey$99,                                                  ~
            plowkey1$99

        dim quant(30),                                                   ~
            f1%(64)


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 3 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 5 ! PIPCROSS ! PIP Cross Reference                      *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *~
            *************************************************************

            select #3 , "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #5,  "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   1, keylen =  71,                      ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            call "OPENCHCK" (#3, fs%, 0%, 0%, " ")
               if fs% < 0% then L65000
            call "OPENCHCK" (#5, fs%, 0%, 0%, " ")

L11000: REM *************************************************************~
            *              INITITALIZE VARIABLES                        *~
            *************************************************************

          init(" ") errormsg$, demstring$, demtype$, jobnumb$(), demand$,~
                      partno$(), quant$(), start$(), end$(), infomsg$,   ~
                      dempart$, demquan$, demcust$, dempcd$, demstatus$, ~
                      dempriority$, demand_count$, demand_index$,        ~
                      demand_msg$

          mat quant   = zer

          blankdate$ = " "
          call "DATUFMTC" (blankdate$)

          line%, maxline%, return%   = 0%
          demand_count%, demand_index% = 0%

            bline2$ = "PF Keys Active:                                   ~
        ~                             "
            bline1$ = "Tag Number           Part Procured              Pr~
        ~oc Quan  Start-Date End-Date "

*        Obtain Planning Base Date

            if plbase$ <> " " and plbase$ <> blankdate$ then L12000

            call "READ100" (#03, "MONTHS OPEN", f1%(03))
                if f1%(03) = 0 then L65000
            get #03 using L11250 , plbase$
L11250:         FMT XX(32), CH(6)
               close #3

L12000: REM *************************************************************~
            *   DISPLAY INFO PLEASE SCREEN IF NECESSARY                 *~
            *************************************************************

          if tagnumber$ <> " " then goto find_path
          if mode% = 2% then goto find_path


        accept                                                           ~
           at(01,02), ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INFORMATION PLEASE ~
        ~<<<<<<<<<<<<<<<<<<<<<<<<<<<<<",                                  ~
           at(10,02), "ENTER THE TAG NUMBER YOU WISH TO TRACE UPWARD",   ~
           at(12,02), "TAG NUMBER TO TRACE:",                            ~
           at(12,30), fac(hex(81)), tagnumber$, ch(19),                  ~
           at(16,02), fac(hex(94)), errormsg$, ch(79),                   ~
           at(22,02), "PF-KEYS ACTIVE:",                                 ~
           at(23,02), "(ENTER)TO BEGIN THE TRACE",                       ~
           at(24,60), "(16)RETURN",                                      ~
                     keys(hex(0010)) , key(khht%)


                     if khht% <> 16% then find_path
                       return% = 3% : end

        REM *************************************************************~
            *        CHECK THE TAG NUMBER PASSED IN                     *~
            *************************************************************
        find_path
           call "SHOSTAT" ("Tracing Upward To Find Top Level Demand")

           readkey$ = tagnumber$
           call "REDALT0" (#2, readkey$, 1%, ret%)
                 if ret% = 0% then L15000
             get #2, using L14100, demtype$
L14100:         FMT  XX(1), CH(1)
           return% = 1%  /* EASY, THEY PASSED IN THE TOP DEMAND*/
           demstring$, demand$ = readkey$

           if mode% <> 1% then end
              infomsg$ = "The tag number IS the top level demand"
              gosub display_path
              end


L15000: REM *************************************************************~
            *        Find Out How Many Demands there are                *~
            *************************************************************

            demand_count% = 0%
            demand_msg$   = " "
            plowkey$ = str(tagnumber$,1%,19%) & all(hex(00))
            ret% = 1%

L15080:     call "PLOWALTS" (#1, plowkey$, 1%, 19%, ret%)
                 if ret% = 0% then L15120
            demand_count% = demand_count% + 1%
            goto L15080
L15120:     if demand_count% = 0% then demand_count% = 1%

            plowkey$ = str(tagnumber$,1%,19%) & all(hex(00))

            for demand_index% = 1% to demand_count%

                line% = 1%  :  file% = 1%
                init (" ")  errormsg$, demtype$, jobnumb$(), partno$(),  ~
                            quant$(), start$(), end$(), infomsg$,        ~
                            dempart$, demquan$, demcust$, dempcd$,       ~
                            demstatus$, dempriority$, demand_msg$

                call "PLOWALTS" (#1, plowkey$, 1%, 19%, ret%)
                goto L16090

        REM *************************************************************~
            *        BEGIN THE TRACE OF PEGGING LINKAGE                 *~
            *************************************************************

*         LINE% = 1%

*         PLOWKEY$ = TAGNUMBER$

L16080:     call "PLOWALTS" (#5, plowkey1$, 1%, 19%, ret%)
L16090:          if ret% <> 0% then L16290   /* GOT A HIT SO LOAD UP  */

                   if line% > 2% then L16230   /* GOT A HIT BEFORE SO? */

                   if mode% <> 2% then L16160
                        return% = 5% : end

L16160:            infomsg$ = "NO PEGGING LINKAGE AVAILABLE "
                   gosub display_path  :  goto end_loop

           /* WE GOT A HIT AT LEAST ONCE BUT FAILED THE TESTS TO         ~
              RECOGNIZE THE END OF CHAIN OR TOP LEVEL REACHED.           ~
              SOMETHING IS FISHY !!                             */

L16230:     return% = 4%
            if mode% = 2% then end
            infomsg$ = "PEGGING CHAIN BROKEN"
            gosub display_path  :  goto end_loop


L16290:    get #file%, using L16360, demand$, jobnumb$(line%),            ~
                                             pipout$(line%),             ~
                                             partno$(line%)

L16360:    FMT  CH(19), CH(19), CH(19), XX(15), CH(25)


           call "READ100" (#4, jobnumb$(line%), f1%(4%))
                if f1%(4%) <> 1% then L16430
           get #4, using L16390, end%, quant(line%), start%
L16390:      FMT  POS(26), BI(4), POS(49), PD(14,4), BI(4)

           call "DATE" addr("G+", plbase$, end%-1%, end$(line%), ret%)
           call "DATE" addr("G+", plbase$, start%-1%, start$(line%),ret%)
           call "DATEFMT" (start$(line%))
           call "DATEFMT" (end$(line%))
           goto L16460

L16430
*        No Pipin, Dummy something up
            quant(line%) = 0
            start$(line%) = "Completd"
            end$(line%)   = " "

L16460:   convert quant(line%) to quant$(line%), pic (##########)

          if line% <> 1% then L16620      /*LOAD DEMAND STUFF ONLY        ~
                                             ON FIRST PASS*/

            call "REDALT0" (#2, demand$, 1%, ret%)
                 if ret% = 0% then end  /* ORPHAN DEMAND IN PIPCROSS*/
            get #2, using L16590, demstatus$, demtype$, dempriority$,     ~
                   dempart$,  demquan$, dempcd$,  demcust$
L16590:     FMT CH(1), CH(1), CH(1),XX(25), CH(25), CH(10), XX(19),      ~
                   CH(6) , CH(19)
            call "DATEFMT" (dempcd$)
L16620:   if pipout$(line%) <> " " then  L16810

          if partno$(line%) <> dempart$ then L16730

              return% = 1%
              type$ = demtype$
              demstring$ = demand$
              if mode% = 2% then end
              gosub display_path  :  goto end_loop


L16730:       return% = 4%
              if mode% = 2% then end
              infomsg$ = "PEGGING LINK BROKEN; DEMAND UNPLANNED WITH SOFT~
        ~ PEGGING"

              gosub display_path  :  goto end_loop


L16810:   if pipout$(line%) = "WO CANCELLED"    or                       ~
             pipout$(line%) = "WO RESCHEDULED"  then L16830
          if pipout$(line%) = "BO CANCELLED"    or                       ~
             pipout$(line%) = "BO RESCHEDULED"  then L16830
          if pipout$(line%) = "BW CANCELLED"    or                       ~
             pipout$(line%) = "BW RESCHEDULED"  then L16830
          if pipout$(line%) = "RO CANCELLED"    or                       ~
             pipout$(line%) = "RO RESCHEDULED"  then L16830
          if pipout$(line%) = "RW CANCELLED"    or                       ~
             pipout$(line%) = "RW RESCHEDULED"  then L16830
          goto L16870
L16830:      return% = 2%
             if mode% = 2% then end
             infomsg$ = "Higher Level Job Cancelled, Chain Broken"

L16870:   if pipout$(line%) <> demand$ then L16950
             return% = 1%
             type$ = demtype$
             demstring$ = demand$
             if mode% = 2% then end
             gosub display_path  :  goto end_loop


L16950:   plowkey1$ = pipout$(line%)
          line% = line% + 1%  :  file% = 5%
          goto  L16080  /* GO FOR THE NEXT ASSEMBLY LEVEL*/

        end_loop
            next demand_index%
            end

        display_path
           init (" ")  demand_msg$
           if return% = 1% then  L17050
             demand$, dempart$, demcust$, dempcd$, demquan$ = " "
             demstatus$, dempriority$ = " "

L17050:    if demand$ = " " then L17270
           if infomsg$ <> " " then L17270

           if demtype$ = "1" then infomsg$ = "THE DEMAND IS A TYPE 1 - NE~
        ~TTABLE SALES ORDER FOR CUSTOMER:"


           if demtype$ = "2" then infomsg$ = "THE DEMAND IS A TYPE 2 - NO~
        ~N-NETTABLE SALES ORDER FOR CUST:"

           if demtype$ = "3" then infomsg$ = "THE DEMAND IS A TYPE 3 - RE~
        ~QUISITION FOR STOCK"
           if demtype$ = "4" then infomsg$ = "THE DEMAND IS A TYPE 4 - NE~
        ~TTABLE FORECAST"
           if demtype$ = "5" then infomsg$ = "THE DEMAND IS A TYPE 5 - NO~
        ~N-NETTABLE FORECAST"
           if demtype$ = "7" then infomsg$ = "THE DEMAND IS A TYPE 7 - PR~
        ~OCUREMENT FOR STOCK"
           if demtype$ = "8" then infomsg$ = "THE DEMAND IS A TYPE 8 - PR~
        ~OCUREMENT FOR STOCK"
           if demtype$ = "9" then infomsg$ = "THE DEMAND IS A TYPE 9 - PR~
        ~EVENTIVE MAINTANANCE"

L17270:     if demand_count% < 2% then L17340
                convert demand_count% to demand_count$, pic(###)
                convert demand_index% to demand_index$, pic(###)
                demand_msg$ = "Multiple Demands:  Demand "               ~
                              & demand_index$ & " of " & demand_count$


L17340:     pf$(1%) = "(1)Start Over  (4)Prev  (9)Next Demand  "   &     ~
                      "                       (13)Instructions"
            pf$(2%) = "(2)First       (5)Next                  "   &     ~
                     "           (15)Print Screen  (16)Return"
            pfk$ = hex(0102ff0405ffffff09ffffff0dff0f1000)
            if demand_count% > 1% and demand_count% <> demand_index%     ~
                     then L18000
                str(pf$(1%),25%,15%) = " "  :  str(pfk$,9%,1%) = hex(ff)

L18000: REM *************************************************************~
            *            INVERT THE ARRAYS BEFORE DISPLAYING            *~
            * THREE STEP PROCESS  1) PUT TOP ITEM INTO TEMP SPOT, 2)PUT *~
            *  BOTTOM INTO TOP, 3) PUT TEMP INTO BOTTOM.  IF ODD NUMBER *~
            *  IN ARRAY THEN WE DON'T NEED TO SWITCH THE MIDDLE ONE.    *~
            *************************************************************
          maxline% = line%
          for i% = 1% to int(maxline%/2%)

             jobnumb$(maxline% +1%) = jobnumb$(i%)
             partno$(maxline% + 1%) = partno$(i%)
             quant$(maxline% + 1%) = quant$(i%)
             start$(maxline% + 1%) = start$(i%)
             end$(maxline% + 1%) = end$(i%)

             jobnumb$(i%) = jobnumb$(maxline% - i% + 1%)
             partno$(i%) = partno$(maxline% - i% + 1%)
             quant$(i%) = quant$(maxline% - i% + 1%)
             start$(i%) = start$(maxline% - i% + 1%)
             end$(i%) = end$(maxline% - i% + 1%)

             jobnumb$(maxline% - i% + 1%) = jobnumb$(maxline% + 1%)
             partno$(maxline% - i% + 1%) = partno$(maxline% + 1%)
             quant$(maxline% - i% + 1%) = quant$(maxline% + 1%)
             start$(maxline% - i% + 1%) = start$(maxline% + 1%)
             end$(maxline% - i% + 1%) = end$(maxline% + 1%)

          next i%

           jobnumb$(maxline% + 1%) = " "
           partno$(maxline% + 1%) = " "
           quant$(maxline% + 1%) = " "
           start$(maxline% + 1%) = " "
           end$(maxline% + 1%) = " "


          line% = 0%

           if jobnumb$(1%) = "WO RESCHEDULED" or                         ~
              jobnumb$(1%) = "WO CANCELLED" then L18397
           if jobnumb$(1%) = "BO RESCHEDULED" or                         ~
              jobnumb$(1%) = "BO CANCELLED" then L18397
           if jobnumb$(1%) = "BW RESCHEDULED" or                         ~
              jobnumb$(1%) = "BW CANCELLED" then L18397
           if jobnumb$(1%) = "RO RESCHEDULED" or                         ~
              jobnumb$(1%) = "RO CANCELLED" then L18397
           if jobnumb$(1%) = "RW RESCHEDULED" or                         ~
              jobnumb$(1%) = "RW CANCELLED" then L18397
           goto L18400

L18397:    init(" ") partno$(1%), quant$(1%), start$(1%), end$(1%)

L18400: accept                                                           ~
               at (01,02), "TAG#:",                                      ~
               at (01,08), fac(hex(84)), tagnumber$             , ch(19),~
               at (01,28), "WAS CAUSED BY DEMAND:",                      ~
               at (01,50),  fac(hex(84)), demand$               , ch(19),~
               at (01,70), "STATUS",                                     ~
               at (01,79), fac(hex(84)), demstatus$             , ch(1) ,~
               at (03,02),  fac(hex(84)), infomsg$              , ch(59),~
               at (03,64), fac(hex(84)), demcust$               , ch(9) ,~
               at (02,02), "FOR PART#:",                                 ~
               at (02,13),  fac(hex(84)), dempart$              , ch(25),~
               at (02,39), "QUAN:"       ,                               ~
               at (02,45),  fac(hex(84)), demquan$              , ch(10),~
               at (02,56), "PCD:",                                       ~
               at (02,60), fac(hex(84)), dempcd$                , ch(8) ,~
               at (02,70), "PRIORITY:",                                  ~
               at (02,79), fac(hex(84)), dempriority$           , ch(1) ,~
               at (04,40), fac(hex(84)), demand_msg$            , ch(40),~
               at (05,02), fac(hex(ac)), bline1$                , ch(79),~
                                                                         ~
                                                                         ~
               at (06,02), fac(hex(84)), jobnumb$(line%+ 1%)    , ch(19),~
               at (07,02), fac(hex(84)), jobnumb$(line%+ 2%)    , ch(19),~
               at (08,02), fac(hex(84)), jobnumb$(line%+ 3%)    , ch(19),~
               at (09,02), fac(hex(84)), jobnumb$(line%+ 4%)    , ch(19),~
               at (10,02), fac(hex(84)), jobnumb$(line%+ 5%)    , ch(19),~
               at (11,02), fac(hex(84)), jobnumb$(line%+ 6%)    , ch(19),~
               at (12,02), fac(hex(84)), jobnumb$(line%+ 7%)    , ch(19),~
               at (13,02), fac(hex(84)), jobnumb$(line%+ 8%)    , ch(19),~
               at (14,02), fac(hex(84)), jobnumb$(line%+ 9%)    , ch(19),~
               at (15,02), fac(hex(84)), jobnumb$(line%+10%)    , ch(19),~
               at (16,02), fac(hex(84)), jobnumb$(line%+11%)    , ch(19),~
               at (17,02), fac(hex(84)), jobnumb$(line%+12%)    , ch(19),~
               at (18,02), fac(hex(84)), jobnumb$(line%+13%)    , ch(19),~
               at (19,02), fac(hex(84)), jobnumb$(line%+14%)    , ch(19),~
               at (20,02), fac(hex(84)), jobnumb$(line%+15%)    , ch(19),~
                                                                         ~
               at (06,23), fac(hex(84)), partno$ (line%+ 1%)    , ch(25),~
               at (07,23), fac(hex(84)), partno$ (line%+ 2%)    , ch(25),~
               at (08,23), fac(hex(84)), partno$ (line%+ 3%)    , ch(25),~
               at (09,23), fac(hex(84)), partno$ (line%+ 4%)    , ch(25),~
               at (10,23), fac(hex(84)), partno$ (line%+ 5%)    , ch(25),~
               at (11,23), fac(hex(84)), partno$ (line%+ 6%)    , ch(25),~
               at (12,23), fac(hex(84)), partno$ (line%+ 7%)    , ch(25),~
               at (13,23), fac(hex(84)), partno$ (line%+ 8%)    , ch(25),~
               at (14,23), fac(hex(84)), partno$ (line%+ 9%)    , ch(25),~
               at (15,23), fac(hex(84)), partno$ (line%+10%)    , ch(25),~
               at (16,23), fac(hex(84)), partno$ (line%+11%)    , ch(25),~
               at (17,23), fac(hex(84)), partno$ (line%+12%)    , ch(25),~
               at (18,23), fac(hex(84)), partno$ (line%+13%)    , ch(25),~
               at (19,23), fac(hex(84)), partno$ (line%+14%)    , ch(25),~
               at (20,23), fac(hex(84)), partno$ (line%+15%)    , ch(25),~
                                                                         ~
               at (06,50), fac(hex(84)), quant$  (line%+ 1%)    , ch(10),~
               at (07,50), fac(hex(84)), quant$  (line%+ 2%)    , ch(10),~
               at (08,50), fac(hex(84)), quant$  (line%+ 3%)    , ch(10),~
               at (09,50), fac(hex(84)), quant$  (line%+ 4%)    , ch(10),~
               at (10,50), fac(hex(84)), quant$  (line%+ 5%)    , ch(10),~
               at (11,50), fac(hex(84)), quant$  (line%+ 6%)    , ch(10),~
               at (12,50), fac(hex(84)), quant$  (line%+ 7%)    , ch(10),~
               at (13,50), fac(hex(84)), quant$  (line%+ 8%)    , ch(10),~
               at (14,50), fac(hex(84)), quant$  (line%+ 9%)    , ch(10),~
               at (15,50), fac(hex(84)), quant$  (line%+10%)    , ch(10),~
               at (16,50), fac(hex(84)), quant$  (line%+11%)    , ch(10),~
               at (17,50), fac(hex(84)), quant$  (line%+12%)    , ch(10),~
               at (18,50), fac(hex(84)), quant$  (line%+13%)    , ch(10),~
               at (19,50), fac(hex(84)), quant$  (line%+14%)    , ch(10),~
               at (20,50), fac(hex(84)), quant$  (line%+15%)    , ch(10),~
                                                                         ~
               at (06,62), fac(hex(84)), start$  (line%+ 1%)    , ch(08),~
               at (07,62), fac(hex(84)), start$  (line%+ 2%)    , ch(08),~
               at (08,62), fac(hex(84)), start$  (line%+ 3%)    , ch(08),~
               at (09,62), fac(hex(84)), start$  (line%+ 4%)    , ch(08),~
               at (10,62), fac(hex(84)), start$  (line%+ 5%)    , ch(08),~
               at (11,62), fac(hex(84)), start$  (line%+ 6%)    , ch(08),~
               at (12,62), fac(hex(84)), start$  (line%+ 7%)    , ch(08),~
               at (13,62), fac(hex(84)), start$  (line%+ 8%)    , ch(08),~
               at (14,62), fac(hex(84)), start$  (line%+ 9%)    , ch(08),~
               at (15,62), fac(hex(84)), start$  (line%+10%)    , ch(08),~
               at (16,62), fac(hex(84)), start$  (line%+11%)    , ch(08),~
               at (17,62), fac(hex(84)), start$  (line%+12%)    , ch(08),~
               at (18,62), fac(hex(84)), start$  (line%+13%)    , ch(08),~
               at (19,62), fac(hex(84)), start$  (line%+14%)    , ch(08),~
               at (20,62), fac(hex(84)), start$  (line%+15%)    , ch(08),~
                                                                         ~
               at (06,72), fac(hex(84)), end$   (line%+ 1%)     , ch( 8),~
               at (07,72), fac(hex(84)), end$   (line%+ 2%)     , ch( 8),~
               at (08,72), fac(hex(84)), end$   (line%+ 3%)     , ch( 8),~
               at (09,72), fac(hex(84)), end$   (line%+ 4%)     , ch( 8),~
               at (10,72), fac(hex(84)), end$   (line%+ 5%)     , ch( 8),~
               at (11,72), fac(hex(84)), end$   (line%+ 6%)     , ch( 8),~
               at (12,72), fac(hex(84)), end$   (line%+ 7%)     , ch( 8),~
               at (13,72), fac(hex(84)), end$   (line%+ 8%)     , ch( 8),~
               at (14,72), fac(hex(84)), end$   (line%+ 9%)     , ch( 8),~
               at (15,72), fac(hex(84)), end$   (line%+10%)     , ch( 8),~
               at (16,72), fac(hex(84)), end$   (line%+11%)     , ch( 8),~
               at (17,72), fac(hex(84)), end$   (line%+12%)     , ch( 8),~
               at (18,72), fac(hex(84)), end$   (line%+13%)     , ch( 8),~
               at (19,72), fac(hex(84)), end$   (line%+14%)     , ch( 8),~
               at (20,72), fac(hex(84)), end$   (line%+15%)     , ch( 8),~
                                                                         ~
               at (22,02), fac(hex(ac)), bline2$                , ch(79),~
               at (23,02), fac(hex(8c)), pf$(1%)                , ch(79),~
               at (24,02), fac(hex(8c)), pf$(2%)                , ch(79),~
                     keys(pfk$),                                         ~
                     key (keyhit%)

               if keyhit% <> 15% then L19550
                  call "PRNTSCRN"
                  goto L18400

               if keyhit%  =  1% then startover

L19550:        if keyhit%  =  2% then line% = 0%

               if keyhit%  =  4% then line% = max(0%, line% - 15%)

               if keyhit%  =  5% then line% = min(line% + 15%,           ~
                                                max(0%, maxline% - 15%))

               if keyhit% <> 1% then L19650
                  gosub startover

L19650:        if keyhit% <> 13% then L19685
                  call "MANUAL" ("GETDEM")
                  goto L18400

L19685:        if keyhit% = 9% then return
               if keyhit% = 0% then return
               if keyhit% <> 16% then L18400
               end

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
               startover% = 2%
               call "STARTOVR" (startover%)
               if startover% = 1% then return
                   return clear
                   tagnumber$ = " "
                   goto L11000

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

            call "SHOSTAT" ("One Moment Please")
            end
