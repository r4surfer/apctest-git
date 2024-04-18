        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   L       AAA   N   N                               *~
            *  P   P  L      A   A  NN  N                               *~
            *  PPPP   L      AAAAA  N N N    D E M O N S T R A T I O N  *~
            *  P      L      A   A  N  NN     & SUB COMBINED            *~
            *  P      LLLLL  A   A  N   N                               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLANSUB  - THIS IS THE MAIN CMS2 PLANNING SUBORUTINE.     *~
            *            PLAN OR CHECK FEASIBILITY.  WITH OR WITHOUT    *~
            *            PRINTED REPORT.                                *~
            *-----------------------------------------------------------*~
            *    THE NINE DEMAND TYPES ARE -                            *~
            *    1 = SALES ORDER TO BE NETTED AGAINST FORECASTS         *~
            *    2 = SALES ORDER NOT TO BE NETTED AGAINST FORECASTS     *~
            *    3 = REQUISITION - BUILD FOR STOCK, NO PIPOUT           *~
            *    4 = REGULAR SALES FORECAST- ASSUMED TO BE PART OF      *~
            *           NETTABLE SALES ENTERED BEFORE OR AFTER THIS FCST*~
            *           WILL ONLY PLAN FOR MORE IF CUMF IS > 0          *~
            *    5 = TRUE NET NEW SALES FORECASTS- WILL ADD TO CUMF &   *~
            *           WILL PLAN FOR THE QUANTITY SPECIFIED NO MATTER  *~
            *           WHAT CUMF IS NOW.                               *~
            *    6 = USER SET - NOW DOES NOTHING                        *~
            *    7 = USER SET - NOW DOES NOTHING                        *~
            *    8 = PROCUREMENT DEMAND (NO JUMP, NO PIPOUT FOR PART)   *~
            *    9 = PREVENTITIVE MAINTENANCE DEMAND                    *~
            *  - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *~
            * PLANFLAG% DETERMINES REPORTING, PLAN OR JUST CHECK        *~
            *           FEASIBILITY & HOW MANY BY THIS DATE FEASIBILITY.*~
            * 1 -  4 = REALLY PLAN, NO PRINTED REPORT, HARD PEG ONLY IF *~
            *          OPTIONS SPECIFIED.                               *~
            * 5 - 9  = REALLY PLAN, NO PRINTED REPORT, HARD PEG.        *~
            *11 - 14 = SAME AS 1 TO 4 BUT WITH FULL REPORT.             *~
            *15 - 19 = SAME AS 5 TO 9 BUT WITH FULL REPORT.             *~
            *                                                           *~
            *      0 = CHECK FEASIBILITY, THIS QUANTITY ONLY, NO REPORT *~
            *     10 = CHECK FEASIBILITY, THIS QUANTITY ONLY, PRINT REPT*~
            * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *~
            *  THE NORMAL FLOW OF A PLAN IS -                           *~
            *    1) CHECK INCOMMING ARGUEMENT VARIABLES FOR VALIDITY    *~
            *    2) ZERO & BLANK ALL ARRAY VARIABLES                    *~
            *    3) LOAD LINE 1 (THE PART/DATE/QUANTITY DESIRED)        *~
            *    4) CALC QTY TO PROCURE (QTYR - ATC)                    *~
            *     4A)IF CONSTRAINED, CHECK ALTERNATES                   *~
            *    5) FOR PURCHASED PARTS, SET START DATE IF POSSIBLE,    *~
            *       THEN EVALUATE THE NEXT LINE (IF ANY)                *~
            *    6) FOR MANUFACUTRED PARTS -                            *~
            *       'MFGDATE'    - FIND EFFECTIVE BOM & RTE             *~
            *                      LOAD WC STEPS & REVERSE SEQUENCE     *~
            *                      CHECK CAPACITY - KEEP TRACK OF STEP  *~
            *                      END DATES                            *~
            *       'LOAD_COMPS' - WHEN ALL STEPS ARE FEASIBLE THEN     *~
            *                      LOAD THE COMPONENT PARTS, SET THE    *~
            *                      END DATES BASED ON THE SPLIT PICK    *~
            *    8) REPEAT 6 & 7 UNTIL ALL LINES OF THE PLAN ARE        *~
            *       SATISFIED.                                          *~
            *                                                           *~
            *  ARRAY DESIGNATOR 'L' IS THE PLAN LINE CURRENTLY BEING    *~
            *  WORKED ON. 'MAXLINES' = THE TOTAL NUMBER OF LINES        *~
            *  LOADED AT ANY POINT.  IN 'MFGDATE', 'X' = RTE STEP       *~
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
            * 08/08/83 ! RELEASE VERSION                          ! GLW *~
            * 01/29/84 ! ADD SPLIT PICK, SCREEN GRAPHICS, HOW MANY! GLW *~
            * 03/26/84 ! NEW SCREEN GRAPHICS, MODIFIED SPLIT PICK,! ERN *~
            *          ! QTY-REQD IN OPTIONS, BOM/RTE SELECTION,  !     *~
            *          ! ATC LOGIC FOR ALTRNATE PARTS, BOM ON RPRT!     *~
            *          ! VARIABLE LEAD TIMES.                     !     *~
            * 05/04/84 ! CHANGE FOR SPEC/BOM.  ALSO GENERAL       ! KAB *~
            *          ! CLEAN-UP AND COMBINING COMMON CODE       !     *~
            * 04/25/85 ! PATCHED FORCE INTEGER EQUATION           ! KAB *~
            * 09/03/86 ! VAR. ATC, PICK BY STEP, YIELD%           ! KAB *~
            * 03/24/87 ! PUR. PART LOOK-AHEAD SPEED UP            ! KAB *~
            * 01/21/88 ! For BOM approvals, do not paln an        ! TLJ *~
            *          ! an unapproved BOM and obtain part types  !     *~
            *          ! from the BOMMASTR file for the components!     *~
            * 06/03/88 ! BOM per unit/fixed overage inversion     ! KAB *~
            * 08/05/88 !1) Fixed Planning Narrative Page numbering! RJM *~
            *          !2) Fixed PRR 4918, W/C allocation error   !     *~
            * 08/08/88 ! Fixed so things aren't scheduled during  ! RJM *~
            *          ! Weekends or Holidays.                    !     *~
            *          ! Added GO FISH logic for MFG Parts.       !     *~
            *          ! (Looks for ATC between old ED and new    !     *~
            *          !  jump date and ajusts ED if enough ATC). !     *~
            * 08/09/88 ! Skips GO FISH for Primary part IF the    ! RJM *~
            *          ! demand type is 7,   AND                  !     *~
            *          ! Fixed Overlap Logic for REQUIRED COMPLETE!     *~
            *          ! BEFORE NEXT STEP CAN START. Now compares !     *~
            *          ! the 'Start Date of this Step' with the   !     *~
            *          ! 'Pick Date Next Step' before planning    !     *~
            *          ! the previous step (only if overlap set). !     *~
            * 08/12/88 ! Added FN'99 to Replace all PLANFLAG Tests! RJM *~
            * 08/17/88 ! Made GO FISH check ATC of Top Level too. ! RJM *~
            * 02/05/89 ! Minor Fixes to Alternate Parts Logic     ! KAB *~
            *          ! Print Minus for Byproduct Quantities     !     *~
            * 06/29/90 ! Tool Quantity MIN to MAX                 ! KAB *~
            * 04/07/92 ! PRR 11797 - Chg'd narrative line from HRS! MLJ *~
            *          !   to UNITS, omitted 'NEED' expanding fld !     *~
            *          !   size from 4 to 7.                      !     *~
            * 03/04/93 ! Formax bug. Errorneous assumption that   ! KAB *~
            *          !   top level part actually used capacity. !     *~
            *          !   BOOK_ORDER (35220).                    !     *~
            * 06/22/93 ! Try to do IW if Phantoms available.      ! KAB *~
            *          ! Minor Bug Fix. Make Sure PH's BOM exists !     *~
            * 07/09/93 ! Minor Chng @ 15160 for SCRNADDR/CDAVSCOM ! KAB *~
            * 08/19/93 ! Purchase Job Support - BW's              ! KAB *~
            * 12/31/93 ! EOD's on PIPOUT writes.  Problem with    ! KAB *~
            *          !    Move Queue if Run < 2 units           !     *~
            * 07/04/94 ! Activity Code to WCOUTs.                 ! KAB *~
            * 12/13/94 ! Better BO optimize, eliminate or decrease! KAB *~
            * 09/26/95 ! Corrected writing of activity to WCOUT   ! JDH *~
            *          !  when there are 2 consecutive steps for  !     *~
            *          !  the same WC.  Thanks Kenny.             !     *~
            * 10/11/95 ! Timing issue - 1) Transition to workfile ! KAB *~
            *          !  occurs during LOAD_COMPS for a part w/  !     *~
            *          !  options. 2) At least one option has been!     *~
            *          !  loaded.  Result - Option chain broken.  !     *~
            *          !  Solution - Save & reset OPTKEY$.        !     *~
            * 04/15/96 ! Re-arrange logic for tag assignment.     ! KAB *~
            *          !  (about 35200 - 26300). Fast machines    !     *~
            *          !  sill beating 1/100 sec.                 !     *~
            * 09/04/96 ! Millie date conversion                   ! DER *~
            * 07/18/97 ! Tag No back to YYMMDD format             ! DER *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        EJECT
        sub "PLANSUB" (dc$,              /* DEMAND CODE                */~
                     dl$,                /* DEMAND LINE                */~
                     dt$,                /* DEMAND TYPE                */~
                     dp$,                /* DEMAND PRIORITY            */~
                     part$,              /* PART NEEDED                */~
                     dqtyu,              /* QUANTITY NEEDED            */~
                     cd%,                /* REQ'D COMPL DATE           */~
                     dw$,                /* DELIVER TO WAREHOUSE       */~
                     rte$,               /* THE WC ROUTE TO USE        */~
                     bom$,               /* WHICH BOM TO USE           */~
                     errormsg$,          /* THE RETURN MESSAGE         */~
                     firstdate%,         /* FIRST DATE PASSED BACK     */~
                     finaldate%,         /* DATE PLANNED FOR           */~
                     todaya%,            /* SUBSCRIPT FOR TODAY'S DATE */~
                     planflag%,          /* 1 = PLAN, 0 = CHECK ONLY   */~
                     #16,                /* HNYALTRS                   */~
                     #15,                /* BOMMASTR                   */~
                     #2,                 /* PIPMASTR                   */~
                     #7,                 /* RTEMASTR                   */~
                     #8,                 /* JBCROSS2                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT   WC-CROSSREFERENCE  */~
                     #33,                /* PIPIN  PARTS DUE INTO PIP  */~
                     #34,                /* PIPOUT PARTS DUE OUT OF PIP*/~
                     #40,                /* SFMASTR2  SALES FORECASTS  */~
                     #41,                /* SFCUM2  CUMULATIVE FCSTS   */~
                     #35,                /* PIPCROSS                   */~
                     #24,                /* ENGMASTR                   */~
                     #14,                /* BOMSPEC                    */~
                     #36,                /* JBPIPXRF                   */~
                     #6,                 /* RTEALTRS                   */~
                     #62,                /* WORK FILES                 */~
                     #63)                                                ~

        EJECT
        rem************** explanation of variables **********************~
           *  arrays = 1000 are for each line in the plan               *~
           *  arrays = 100 are for each work center step for any line   *~
           *  *do not change com block.  it is used several places!!!!* *~
           **************************************************************

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
           act$4,                        /* ACTION PERFORMED, REPORT   */~
           approval$,                    /* APPR FLAG FROM BOM HDR REC */~
           optkey$100, optkeyhold$100,                                   ~
           optmkr$1,                                                     ~
           pbs$5,                                                        ~
           pbs%(1),                                                      ~
           pbshold%(101),                                                ~
           palevel%(101),                                                ~
           paok$(101)1,                                                  ~
           testbom$60,                                                   ~
           type$3,                       /* PART TYPE FROM BOMMASTR   */ ~
           pltkey$29,                                                    ~
           activ$40,                     /* ACTIVITY IN WORK CTR       */~
           plowkey1$33,                  /* PLOW ON HNYALTR            */~
           errormsg$79,                  /*                            */~
           altpart$25,                   /* ALTERNATE PART             */~
           ccyymmdd$(2)8,                /* ccyymmdd (need 2)          */~
           compplowkey$31,               /* PLOW BOMMASTR              */~
           date$8,                       /* Todays date                */~
           date_fmtr$8,                  /* date place holdr 4 ccyymmdd*/~
           date_mmddtst$2,               /* month and day test 0301    */~
           time$8,                       /* Run time                   */~
           mkr$2,                        /* BOM MARKER FOR PHANTOMS    */~
           holdlwc$4,                                                    ~
           scrn$78,                      /* SCREEN GRAPHICS            */~
           scrn23$78,                    /* SCREEN GRAPHICS            */~
           scrn24$78,                    /* SCREEN GRAPHICS            */~
           scrnaction$1,                 /*                            */~
           scrndate1$8,                  /*                            */~
           scrndate2$8,                  /*                            */~
           dc$16,                        /* DEMAND CODE & LINE         */~
           dl$3,                         /* DEMAND CODE & LINE         */~
           dp$1,                         /* DEMAND TYPE & PRIORITY     */~
           dt$1,                         /* DEMAND TYPE & PRIORITY     */~
           dw$3,                         /* DEMAND WAREHOUSE           */~
           part$25,                      /*                            */~
           rte$3,                        /*                            */~
           step$7,                       /* Phantomized Step, REPORT   */~
           bom$3,                        /*                            */~
           tagdate$6,                    /* Tag No date, YYMMDD used   */~
           tagdtfull$8,                  /* Tag No date, CCYYMMDD      */~
           tagdttmp$8,                   /* temp/scratch date          */~
           testbyte$6,                   /*                            */~
           testdemd$6,                   /*                            */~
           lwc$4,                        /* WC STACK                   */~
           lwc1$4,                       /* WC STACK - 1ST CONC        */~
           lwc2$4,                       /* WC STACK - 2ND CONC        */~
           lwc3$4,                       /* WC STACK - 3RD CONC        */~
           wd$(2000)4,                   /* WORK CENTER ACTIVITY (S)   */~
           wcpact$4,                     /* WORK CENTER ACTIVITY (S)   */~
           wc1act$4,                     /* WORK CENTER ACTIVITY (S)   */~
           wc2act$4,                     /* WORK CENTER ACTIVITY (S)   */~
           wc3act$4,                     /* WORK CENTER ACTIVITY (S)   */~
           wcsact$4,                     /* WORK CENTER ACTIVITY (S)   */~
           mqp$1,                        /* MQ TIME OPTION             */~
           xxsd%(255),                   /*                            */~
           cumuse(490),                  /*                            */~
           stpuse(490),                  /*                            */~
           pjflag$1                      /* Purchase Job Flag          */~




        EJECT
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************
           if str(planflags$(),1,1)<>" " then L09150
              errormsg$="PLANNING OPTIONS RECORD INCORRECT"
              gosub return_unplanned

L09150:    get str(planflags$()) using L09180, toolcons%, capmq%,          ~
                       jumpmin%, jumpmax%, jumppur%, jumpmfg%, purscan%, ~
                       endoffset%, compoffset%, mfgscan%
L09180:        FMT POS(6), 2*BI(1), 4*BI(2), POS(207), BI(2),            ~
                   POS(273), 2*BI(4), POS(425), BI(2)
            capmq=capmq%:capmq=round(capmq/100,2)
            todaye% = min(490%, max(1%, todaya% + endoffset%))
            todayc% = min(490%, max(1%, todaya% + compoffset%))

            matarray% = dim(part$(),1)
            wcarray%  = dim(wc$(),1)
        REM LWCARRAY% = DIM(RTESTEP$(),1)
            workfile% = 0%

            call "FILEBGON" (#62)
            call "FILEBGON" (#63)
            date$, time$ = " "
            date$ = date : call "DATEFMT" (date$) : call "TIME" (time$)
            page%, got_vend%, top_iw% = 0%

            tagdttmp$ = date
            call "DATEFMT" ( tagdttmp$, 0%, tagdtfull$ )
            tagdate$ = str( tagdtfull$, 3%, 6% )


        EJECT
        rem**************************************************************~
           *         m a i n   c o n t r o l   s e c t i o n            *~
           *                                                            *~
           *  this section of code controls the entire plan sequence.   *~
           *  note that it calls a series of internal subroutines       *~
           *  to do all of the planning work, including booking the     *~
           *  order.                                                    *~
           *  planflag% > 0 if you really want to plan.                 *~
           *  printflag = 1 if you want a printed report.               *~
           **************************************************************

           if dt$ = "6" then end

           if dt$ = "9" then today% = todaya% else today% = todaye%
           qtyu = dqtyu
           init (" ") scrn$, scrn23$, scrn24$, errormsg$

           gosub scrninit /* SCREEN INITIALIZATION */
           select printer(134)

           init (hex(00)) testdemd$,testbyte$
           if dt$ = "9" then L10270

           str(testdemd$,1,6)=hex(00)                                    ~
                             & bin(2^(val(dt$,1)-49%),1)                 ~
                             & bin(2^(val(dp$,1)-64%),4)

L10270:    if dt$="9" then ed%=cd% else ed%=max(cd%,today%)
           if ed% <= 490% then L10280
              errormsg$="DUE DATE OUTSIDE PLANNING CALENDAR"
              gosub return_unplanned
L10280:    if dt$="9" then L10330
           if ed%-cd% <= jumpmax% then L10330
              errormsg$="DUE DATE OUTSIDE ALLOWABLE JUMP RANGE"
              gosub return_unplanned

L10330:    type78% = int(planflag%/100000%)
           planflag% = mod(planflag%, 100000%)
           offset%, resched%  = int(planflag%/100%)
           planflag% = mod(planflag%, 100%)
           if planflag% >  9% then printflag%=1% else printflag% = 0%
           planflag% = mod(planflag%,  10%)

           date_mmddtst$ = hex(0301)
           search str(yymmdd$(),4%) = date_mmddtst$ to ed%() step 6
           if ed%(1%) <> 0% then L10440
              errormsg$="PURCHASE LEAD TIME BASE ERROR"
              gosub return_unplanned
L10440:    pltbase%=(ed%(1%)+5%)/6%

           if dt$ = "9" then L10630

           qtydd     = 0
           optflag%  = 0%
           part$(1%) = part$
           qtyu(1%)  = qtyu
           ed%(1%)   = ed%
           bom$(1%)  = bom$
           rte$(1%)  = " "

           gosub'64(1%):gosub'65(1%)

           if dt$ > "2" then L10600
           gosub check_sales_order
L10600:         errormsg$ = "CAN'T PLAN FOR A QUANTITY <= 0"
           if qtyu < .00001 then gosub return_unplanned

L10630:    if printflag% = 0% then L10880
           gosub page_head
           goto L10880
*          PRINT PAGE
*          PRINT USING 12020, PAGE%: PRINT: PRINT USING 12030: PRINT
*          PRINT USING 12040: PRINT: PRINT USING 12055: PRINT USING 12060
*          PRINT USING 12065: PRINT USING 12075: PRINT USING 12085
*          PRINT USING 12095: PRINT USING 12105: PRINT
*          PRINT USING 12115: PRINT: PRINT
*          PRINT USING 12130: PRINT USING 12140: PRINT : PRINT
*          PRINT USING 12150, QTYU , PART$
*          PRINT USING 12160, DT$
*          PRINT USING 12165, YYMMDD$(ED%), ED%
*          PRINT USING 12175, DC$
*          PRINT USING 12185, DL$
*          PRINT USING 12195, YYMMDD$(TODAY%), TODAY%
        page_head
            page% = page% + 1%
            print page : print using L45040, date$, time$, page%
            print using L45060
            print : print using L45100 : print
            print using L45120, dc$, dl$  :  print
            line% = 7%
            return

L10880:    init (" ") intagnr$(), outtagnr$()
           maxi% = 0%: x%, l%, maxlines% = 1%
           str(optkey$,1,23)=str(dc$,1,16) & str(dl$,1,3) & hex(00000000)
           optkeyhold$ = optkey$

           if dt$ = "9" then pm_only_plan

           call "PIPFLAGS" (part$, today%, 490%, 0, #2, #41)
           if dt$ = "4" or dt$ = "5" then gosub check_forecast
           gosub screenloop
           goto L12220

        REM * * * TEST FOR A REASON TO BE HERE * * *
        check_sales_order
           qtyu, qtyu(1%) = 0
           call "READ100" (#33, str(dc$,1,16) & str(dl$,1,3), f1%(33))
           if f1%(33) <> 0 then L11210
            errormsg$="SALES ORDER PIPIN NOT FOUND, ORDER ASSUMED FILLED"
             scrn$ = errormsg$
             gosub'080 (scrn$, 3%, 1%)
             if printflag%<> 0% then print errormsg$

L11090:      gosub'99(type%(1%),106%,test%)
             if test% = 0% then L11340

             return clear
             firstdate%, finaldate% = ed%(1)
             if planflag% = 0% then return_planned
             if dt$ <> "1" then return_planned
             scrn$="PERFORMING FINAL NET DOWN OF FORECAST FILES"
             gosub'080 (scrn$, 3%, 1%)
             gosub final_net_down
             goto return_planned

L11210:      get #33, using L11220, qtyu, data goto L11340
L11220:         FMT XX(48), PD(14,4)
             qtyu(1%) = qtyu
             if qtyu > .0001 then L11300
             qtyu, qtyu(1%) = 0
             call "READ101" (#33, str(dc$,1,16) & str(dl$,1,3), f1%(33))
             if f1%(33) <> 0 then delete #33
             goto L11090

L11300:      gosub'99(type%(1%),106%,test%)
             if test% = 1% then return

L11340:     qtyu, qtyu(1%) = max(dqtyu, qtyu)
            return

        pm_only_plan
           mq%, wcalters%, su%, holdsu% = 0% : pjflag$ = " "
           savesdl% = 999% : x% = 1%
           ed%(1%), sd%(1%), xxsd%(1%), xsd%(1%), pd%(1%), useday% = ed%
           lwc$ = str(part$,22,4)
           lwc1$, lwc2$, lwc3$, step$(1%), mqp$ = " "
           holdtr%, tr%, qtyu(1%), qtyp(1%) = max(1,round(qtyu,0))
           nm1, nm2, nm3, shift, nextpass, phfact = 1
           run, cumuse = 0 : yld(1%) = 100
           part$(1%)="PREVENTIVE MAINTENANCE"
           gosub screenloop
           gosub alloc_wc_time
           gosub scrnwcplot
           sd%(1%) = pd%(1%)
           if sd%(1%) > today% then book_order
            errormsg$ = "PM CANNOT BE PERFORMED BEFORE YOUR DESIRED DATE"
             gosub return_unplanned

        EJECT
L12000: REM  ************************************************************~
           *     ALL LOOP CHECKING STARTS HERE. L% IS THE CURRENT LINE  *~
           *  BEING EXAMINED.  THAT LINE IS EITHER DEALT WITH OR        *~
           *  WE MUST JUMP.  JUMP WILL RESTART HERE WITH L% = 1 AGAIN.  *~
           **************************************************************

           if l% > 1% then L12140

           action%(1%) = 0%
           if printflag% = 0% then L12220
             if line% > 56% then gosub page_head

             date_fmtr$ = yymmdd$(ed%)
             call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
             date_fmtr$ = yymmdd$(ed%(1%))
             call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

             print:print:print using L45250, str(ccyymmdd$(1%), 3%, 6%), ~
                                             str(ccyymmdd$(2%), 3%, 6%)

             line% = line% + 3%
             goto L12220

L12140:    if printflag% = 0% then L12180
             if line% > 57% then gosub page_head
             date_fmtr$ = yymmdd$(ed%(l%))
             call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))

             print:print using L45270, str(ccyymmdd$(1%), 3%, 6%), ~
                                       qtyu(l%), part$(l%)
             line% = line% + 2%
L12180:    offset% = 0%:today% = todayc%
           if type%(l%)>489% and type%(l%)<500% then loop_check
           if type%(l%)>789% and type%(l%)<800% then loop_check
           if action%(l%) = 0% then L12220
              qtyp(l%) = 0 : goto L12230

L12220:    gosub calciw      /* CALCIW ALLOCATES AS MUCH AS POSSIBLE TO */
L12230:    altflag% = 0%     /* THE DEMAND. THE REMAINDER IS PLANNED.   */
           gosub screenloop  /* ALSO CHECKS FOR PART IN PIP, ETC        */

           if abs(qtyp(l%)) > .00001 then L12510


        atc_ok
                action%(l%) = 1%              /* 1 = IW           */
                sd%(l%)     = ed%(l%)

                if printflag% = 0% then loop_check
                    if line% > 58% then gosub page_head
                    print using L45210, qtyu (l%), part$(l%)
                    line% = line% + 1%

        loop_check


                if l% = maxlines% then book_order
                   l% = l% + 1%
                     goto L12000







L12510:    if l% = 1% then L12580

           gosub'99(type%(l%),64%,test%)
           if test% = 0% then L12580

           gosub alt_part_sub
L12580:    altflag% = 1%
           if type%(l%) < 500% and type%(l%) > 0%  then pur_part

        REM ** THESE ARE ALL MANUFACTURED PARTS **

           gosub mfgdate
           if sd%(l%) <= today% then alt_part_sub
        mfg_ok
                     action%(l%) = 3%     /* 3 = MANUFACTURE            */
                     if pjflag$ <> " " then  action%(l%) = 4%  /* BW */

                     gosub load_comps
           goto loop_check

        pur_part
           if today% + lt%(l%) > ed%(l%) then alt_part_sub
        pur_ok
                     action%(l%) = 2%               /* 2 = PURCH        */
                     sd%(l%)     = ed%(l%) - lt%(l%)

           if printflag% = 0% then loop_check
               if line% > 58% then gosub page_head
               print using L45200, qtyp(l%), part$(l%)
               line% = line% + 1%
           goto loop_check

        EJECT
        alt_part_sub
          if altflag% = 0% then L13500
          if type%(l%) > 499% or type%(l%) = 0%  then L13050
                minjump% =  today% + lt%(l%) + jumppur%  - ed%(l%)
                goto L13060
L13050:   minjump% =  jumpmfg% * x% /* X% = # OF STEPS LEFT IN MGF SEQ */
L13060:   minjump% = max(minjump%, jumpmin%)
          minjump% = min(minjump%, 491%-ed%(1%), cd%+jumpmax%+1%-ed%(1%))

          gosub'059(1%)   /* Can we do IW on Top level part by Now ?  */
          if top_iw% = 1% then jump

             gosub'059(l%)                  /* GO FISH FOR COMPONENT */

L13500: REM CHECK FOR ALTERNATE PARTS
           if l% > 1% then L13540
              if altflag%=0% then return
              goto jump
L13540:    altpart$=part$(l%)
           alttype%=type%(l%)
           altmoq% =moq% (l%)
           altlt%  =lt%  (l%)
           altqtyp =qtyp (l%)
           altbom$ =bom$ (l%)
           altrte$ =rte$ (l%)
           bom$(l%), rte$(l%) = " "

           gosub L14010

           part$(l%)=altpart$
           type%(l%)=alttype%
           moq% (l%)=altmoq%
           lt%  (l%)=altlt%
           qtyp (l%)=altqtyp
           bom$ (l%)=altbom$
           rte$ (l%)=altrte$
           return

        EJECT
        REM ALTERNATE PART TEST LOOP
L14010:    if type%(l%)=0% then L14090
           if printflag% = 0% then L14060
               if line% > 58% then gosub page_head
               print using L45150, part$(l%)
               line% = line% + 1%
L14060:    plowkey1$ = str(part$(l%),1,25) & " "
L14070:    call "PLOWNEXT" (#16, plowkey1$, 25%, f1%(16))
                if f1%(16%) = 1% then L14160
L14090:            if altflag% = 0% then return
                   return clear all
                   if printflag% = 0% then jump
                   if line% > 58% then gosub page_head
                   print using L45170, altpart$
                   line% = line% + 1%
                   goto jump
L14160:    get #16, using L14170, part$(l%)
L14170:              FMT XX(33), CH(25)

           call"READ100"(#2, part$(l%), f1%(2))
                if f1%(2%) <> 1% then L14070
           get #2, using L14220, moq, type%(l%), lt%(l%)
L14220:         FMT POS(2003), PD(14,4), XX(8), 2*BI(2)

           moq%(l%) = moq
           if type%(l%) < 200% then L14070
           gosub'65(l%)

           gosub calciw
           if abs(qtyp(l%)) > .00001 then L14340

           action%(l%)=1%
           goto sub_alt_part

L14340:    if altflag% = 0% then L14070

           gosub'99(alttype%,70%,test%)
           if test% = 0% then L14070

           if type%(l%) > 499 then L14490

        REM ** CHECK PURCHASED ALTERNATE **
           if today% + lt%(l%) > ed%(l%) then L14070

           action%(l%) = 2%               /* 2 = PURCHASE              */
           goto sub_alt_part


L14490: REM ** THESE ARE ALL MANUFACTURED PARTS **

           gosub mfgdate

           if sd%(l%) <= today% then L14070
              action%(l%) = 3%            /* 3 = MANUFACTURE           */
              if pjflag$ <> " " then  action%(l%) = 4%  /* BW */
           goto sub_alt_part

        sub_alt_part

           scrn$ = "ALTERNATE PART " & part$(l%) & " SUBSTITUTED."
           gosub'080 (scrn$, 3%, 1%)

           if printflag% = 0% then L14660
               if line% > 58% then gosub page_head
               print using L45190, part$(l%), altpart$
               line% = line% + 1%
L14660:    return clear all
           on action%(l%) goto atc_ok, pur_ok, mfg_ok, mfg_ok

        EJECT
        REM *************************************************************~
            *                                                           *~
            * END OF LOOP CHECKING, EXIT FROM LOOP CHECK IS EITHER      *~
            * BOOK ORDER OR JUMP.  NEXT COME COMMON EXIT ROUTINES       *~
            *                                                           *~
            *************************************************************

        return_unplanned
           return clear all
           if workfile% <> 0% then L15110
           if str(planflags$(),20,1) = "Y" then gosub boprint
L15110:    if printflag% <> 0% then print errormsg$
           if printflag% <> 0% then line% = line% + 1%
           init(" ") scrn$
           if dt$  = "8" then put scrn$, using L62900, " "
           if dt$ <> "8" then put scrn$, using L62910, " "
           gosub'080 (scrn$, 3%, 3%)

           if printflag% = 0% then L15220
               if line% > 58% then gosub page_head
               print using L45240
               line% = line% + 1%
L15220:    finaldate% = 999%
           goto L15540

        return_planned

           if printflag% <> 0% then print errormsg$
           if printflag% <> 0% then line% = line% + 1%
L15540:    close printer
           if str(planflags$(),1,1) <> "Y" then L15570
           call "SCRNADDR" ("C", 1%, 1%, scrn$, 78%, 0%, r%)
L15570:    end

        REM *************************************************************~
            *                  TEST PLANFLAG SWITCHES                   *~
            *************************************************************
        deffn'99(typ%,p%,test%)
            test% = 1%
            str(testdemd$,1,1)  = bin(2^(int(typ%/100)-2%))
            str(testbyte$,1,6) = str(testdemd$,1,6)     and              ~
                                 str(planflags$(),p%,6)
            if str(testbyte$,1,6)=hex(000000000000) then test% = 0%
            return

        EJECT
        REM *************************************************************~
            *   CHECK LOGIC FOR ITEMS DESIGNATED AS TOOLS               *~
            *************************************************************
        tool_check
            if mkr$="TL" then L16080
               errormsg$="COMPONENT NOT MARKED AS TOOL " & part$(m%)
               goto return_unplanned

L16080:    parline%(m%) = l%
           action%(m%) = 1%
           qtyu (m%) = max(0, qtyp(l%)*phfact(ti%) * (qu*tu + fu))
           if qtyu(m%) > 0 then qtyu(m%)=max(1,round(qtyu(m%),0))
           qtyu(m%)=round(qtyu(m%) + addon, 0)
           if qtyu(m%) < .0001 then L19500
           qtyp (m%) = 0
        REM *** TOOL REQUIREMENT DATES SET HERE **************

           pbs% = min(loi%, max(pbs%, offset%+1%))

              sd%(m%) = pd%(pbs%)
              ed%(m%)=max(du%(mmx%(pbs%)+1%)+1%, sd%(m%)+1%)

           if mod(type%(m%),10%) >= toolcons% then L16320
L16230:       maxlines% = maxlines% + 1%
           if printflag% = 0% then L16280
              if line% > 58% then gosub page_head
              print using L45620, qtyu(m%), part$(m%)
              line% = line% + 1%
L16280:    init (" ") scrn$:put scrn$ using L45620, qtyu(m%), part$(m%)
           gosub'080 (scrn$, 3%, 1%)
              goto L19500

L16320: REM * * * RATS, TOOL MARKED AS CONSTRAINING, CHECK PIP * * *
           scrn$ = "CHECKING TOOL FOR CONSTRAINT " & part$(m%)
           gosub'080 (scrn$, 3%, 1%)

           altpart$=part$(m%)
           alttype%=type%(m%)
           altmoq% =moq% (m%)
           altlt%  =lt%  (m%)
           plowkey1$ = str(part$(m%),1,25) & " "

            gosub L18160

L16440:     for tl%=sd%(m%) to ed%(m%)-1%
                if pip%(tl%)<qtyu(m%) then L16490
                next tl%
                goto L16230

L16490:    gosub L18000
                if f1%(16%) <> 0% then L16440

        REM CONSTRAINED, NOW WHAT???; TRY BACKWARDS JUMP

           holdsd% = sd%(m%)
           holdrq% = ed%(m%)-sd%(m%)-1%
           holdxsd% = ed%(m%)-1%
           maxlines%=startmaxlines%



           if holdsd%-1% < today% then L17150

           if printflag% = 0% then L16680
               if line% > 58% then gosub page_head
               print using L45580, altpart$
               line% = line% + 1%

L16680:    init (" ") scrn$:put scrn$ using L45580, altpart$
           gosub'080 (scrn$, 3%, 1%)

           if str(planflags$(),16,1) <> "Y" then L16730
           for tl%=holdsd%-1% to today% step -1%
L16730:        part$(m%)=altpart$
               type%(m%)=alttype%
               moq% (m%)=altmoq%
               lt%  (m%)=altlt%
               plowkey1$ = str(part$(m%),1,25) & " "

            gosub L18160

L16810:    if str(planflags$(),16,1) = "Y" then L16830
           for tl%=holdsd%-1% to today% step -1%
L16830:              for j%=tl% to tl%+holdrq%
                         if pip%(j%) < qtyu(m%) then L16870
                     next j%
                     goto L16950
L16870:    if str(planflags$(),16,1) = "Y" then L16890
           next tl%
L16890:    gosub L18000
                if f1%(16%) <> 0% then L16810
           if str(planflags$(),16,1) <> "Y" then L17150
           next tl%
                goto L17150

L16950:          maxi%=mmx%(pbs%)
                 xx% = pbs%
                 xsd%(pbs%)=holdrq%+tl%
                 sd%(l%)=xsd%(pbs%)

           if printflag% = 0% then L17040
               if line% > 58% then gosub page_head
               print using L45600, pbs%, tl%-holdsd%
               line% = line% + 1%
L17040:    init (" ") scrn$:put scrn$ using L45600, pbs%, tl%-holdsd%
           gosub'080 (scrn$, 3%, 1%)

              nextpass = 1
                 gosub L25500  /* INTO CENTER OF MFGDATE LOGIC */
                 if today% < sd%(l%) then load_comps
                 xsd%(pbs%)=holdxsd%
           if str(planflags$(),16,1) <> "Y" then L17150
           goto L16890


L17150: REM CANT JUMP BACK, SO SET MINJUMP FOR FORWARD JUMP & REPLAN

           holdxsd% = min(490%-ed%(1%), cd%+jumpmax%-ed%(1%))
           if holdxsd% < 1% then L18400
           holdsd%=holdsd%+1%
           holdxsd%=min(490%, holdxsd%+holdsd%)

           if str(planflags$(),17,1) <> "Y" then L17250
           for tl%=holdsd% to holdxsd%

L17250:        part$(m%)=altpart$
               type%(m%)=alttype%
               moq% (m%)=altmoq%
               lt%  (m%)=altlt%
               plowkey1$ = str(part$(m%),1,25) & " "

           gosub L18160

L17330:    if str(planflags$(),17,1) = "Y" then L17350
            for tl%=holdsd% to holdxsd%
L17350:              for j%=tl% to tl%+holdrq%
                         if pip%(j%) < qtyu(m%) then L17390
                     next j%
                     goto L17470
L17390:    if str(planflags$(),17,1) = "Y" then L17410
            next tl%
L17410:    gosub L18000
                if f1%(16%) <> 0% then L17330
           if str(planflags$(),17,1) <> "Y" then L18400
           next tl%
                goto L18400

L17470:          minjump%=tl%-holdsd%
                 x% = pbs%
                 return clear all

           if printflag% = 0% then L17550
               if line% > 58% then gosub page_head
               print using L45610, minjump%
               line% = line% + 1%
L17550:    init (" ") scrn$:put scrn$ using L45610, minjump%
           gosub'080 (scrn$, 3%, 1%)
                 top_iw% = -1%   /* Force GO FISH on Line 1 Part */
                 goto jump

L18000: REM COMMON PLOW AND LOAD ROUTINE

L18020:    call "PLOWNEXT" (#16, plowkey1$, 25%, f1%(16))
                if f1%(16%) = 0% then return

           get #16, using L14170, part$(m%)
                     FMT XX(33), CH(25)
           gosub'64 (m%)
             if type%(m%)>489% and type%(m%)<500% then L18120
             if type%(m%)>789% and type%(m%)<800% then L18120
           goto L18020

L18120:    if mod(type%(m%),10%) >= toolcons% then L18160
              return clear
              goto L16230

L18160: REM COMMON LOAD PIP ROUTINE
            call "READ100" (#2, part$(m%), f1%(2))
                if f1%(2) <> 0 then L18210
                 errormsg$="TOOL NOT FOUND IN PIP" & part$(m%)
                 goto return_unplanned
L18210:     get #2, using L18220, pip%()
L18220:         FMT XX(26), 490*BI(4)

        REM AND (HO HUM) ADJUST FOR USE IN THIS PLAN



            if m% < 3%     then return

            for i%=2% to m%-1%
                if part$(i%)<>part$(m%) then L18350
                   for j%=sd%(i%) to ed%(i%)-1%
                       pip%(j%)=pip%(j%)-qtyu(i%)
                   next j%
L18350:    next i%


           return

L18400: REM * * ERROR MESSAGE AND BYE-BYE * * *

               errormsg$="TOOL AVAILABILITY CANNOT BE FOUND " & altpart$
               goto return_unplanned

        EJECT
        REM *************************************************************~
           *        L O A D   C O M P O N E N T   P A R T S             *~
           *  THIS ROUTINE LOADS THE COMPONENT PARTS FOR A MANUFACTURED *~
           *  PARENT.  IT ALSO GETS THE LT, MOQ, & TYPE.                *~
           *    THE SPLIT PICKING DATES ARE ESTABLISHED.  THE BOM & RTE *~
           *  WERE SELECTED IN 'MFGDATE'.  PHANTOM BLOW UPS ARE PICKED  *~
           *  AT THE STEP SPECIFIED FOR THE PHANTOM ITSELF.             *~
           **************************************************************

        load_comps

           ti%  = 1% : phfact(1%) = 1
           palevel%, palevel%(ti%) = 0%:paok$(ti%) = "Y"
           startmaxlines%=maxlines%
           optkeyhold$ = optkey$
           scrn$ = "LOADING PARTS FROM BOM " & bom$(l%)
           gosub'080 (scrn$, 3%, 1%)

           if printflag% = 0% then L19220
               if line% > 58% then gosub page_head
               print using L45220, bom$(l%),part$(l%)
               line% = line% + 1%

L19220:    compplowkey$ = str(part$(l%),1,25) & str(bom$(l%),1,3) & "  0"

L19500:    m% = maxlines% + 1%

           call "PLOWNEXT" (#15, compplowkey$, 28%, f1%(15))
                if f1%(15%) =  0% then return
           if m% < matarray% then L19570
              errormsg$="MAXIMUM COMPONENTS EXCEEDED FOR THIS PLAN"
              action%(l%) = 0%
              goto workfile
L19570:    bom$(m%),rte$(m%)=" "
           get #15, using L19600, part$(m%), qu, tu, addon, fu, mkr$,     ~
                    optmkr$, bom$(m%), pbs$, type$
L19600:       FMT CH(25), XX(31), 4*PD(14,4), CH(2), CH(1), CH(3),       ~
                  XX(4), CH(4), CH(3)

           search "PHPASTUUASTLNCNPBP" = str(mkr$,,2) to pbs%() step 2
              if pbs%(1%) = 0% then L19500   /* Exclude SP, RE, et. al. */

           if ti% = 1% then L19670
           if paok$(ti%) = "Y" then L19670
              pbs% = pbshold%(ti% - 1%)
              goto L19740
L19670:    if pbs$ <> " " then L19690
              pbs% = 1%:goto L19740
L19690:    put str(pbs$,5) using L19700, palevel%(ti%)
L19700:        FMT BI(1)
          search str(step$(),1%, 5%*loi%) = str(pbs$,,5) to pbs%() step 5
          pbs% = min(max(1%, int((pbs%(1) + 4%)/5%)), loi%)

L19740:    if pbs% <= offset% then L19500
           if optmkr$ <> "Y" then L20500

        rem********** option part processing ****************************

           if printflag% = 0% then L20070
               if line% > 58% then gosub page_head
               print using L45440, part$(m%)
               line% = line% + 1%

L20070:    call "PLOWALTS"(#14,optkey$,1%,19%,f1%(14))
                if f1%(14%) <> 0% then L20110
                errormsg$ = "OPTION NOT FOUND FOR PART " & part$(m%)
                     goto return_unplanned
L20110:    get #14, using L20120, testbom$, part$(m%), qu, tu,            ~
                                 bom$(m%), optmkr$
L20120:         FMT XX(25),CH(31),XX(19),XX(4),CH(25), 2*PD(14,4),       ~
                    CH(3), CH(1)
           if str(testbom$,1,31) = str(compplowkey$,1,31) then L20160
                errormsg$ = "INCORRECT OPTION ENCOUNTERED" & testbom$
                     goto return_unplanned
L20160:    if part$(m%) = " " then L19500
           if optmkr$  <> " " then L19500
*              BOM$(M%)=" "
           scrn$ = "OPTION PART " & part$(m%) & " SUBSTITUTED."
           gosub'080 (scrn$, 3%, 1%)
           if printflag% = 0% then L20250
               if line% > 58% then gosub page_head
               print using L45460, part$(m%)
               line% = line% + 1%

L20250: rem************ end of option processing ************************

L20500:    gosub'64(m%)
           if type%(m%)>489% and type%(m%)<500% then tool_check
           if type%(m%)>789% and type%(m%)<800% then tool_check
           if mkr$<>"TL" then L20570
              errormsg$="COMPONENT MARKED AS TOOL " & part$(m%)
              goto return_unplanned

L20570:    parline%(m%) = l% : action%(m%) = 0%

        REM *** SPLIT PICKING DATES ARE SET HERE **************

           pbs% = min(loi%,max(pbs%,offset%+1%))
           sd%(m%), ed%(m%) = max(pd%(pbs%), sd%(l%))

           qtyu (m%) = qtyp(l%)*phfact(ti%) * (qu*tu + fu)
           qtyu (m%) = 100 * qtyu(m%) / yld(pbs%)
           qtyu (m%) = qtyu(m%) + addon
           qtyp (m%) = 0
           if qtyu(m%) > 0 then L20740
           if qtyu(m%) = 0 then L19500
           gosub'99(type%(m%),118%,test%)
           if test% = 0% then L19500

L20740: REM TEST FORCE INTEGER
           gosub'99(type%(m%),58%,test%)
           if test% = 0% then L20820
             qtyu(m%)=sgn(qtyu(m%))*abs(int(-qtyu(m%)))
        REM  QTYU(M%)=INT(QTYU(M%)+SGN(QTYU(M%))*.99999)
               if qtyu(m%) = 0 then L19500

L20820: REM RESUME
           gosub'65(m%)
           if mkr$ <> "UU" then L20900
                if type%(m%)=0% or type%(m%)>489% then type%(m%)=489%
                lt%(m%)=999%
                moq%(m%)=0%
*              GOTO 21050

L20900: REM PRESCAN LOGIC
           if mkr$ = "NP" then L21100
           gosub'99(type%(m%),142%,test%)
           if test% = 0% then L21500

           call "PLANCIW"                                                ~
                      (dt$,              /* DEMAND TYPE                */~
                       testdemd$,        /* SWITCH MASK                */~
                       errormsg$,        /* THE RETURN MESSAGE         */~
                       qtydd,            /* QTY ON DELIVERY DAY        */~
                       cd%,              /* COMPLETION DAY OFFSET      */~
                       m%,               /* MATERIALS ARRAY LOCATION   */~
                       todaya%,          /* ACTUAL TODAY               */~
                       workfile%,        /* WORKFILE STATUS = 0        */~
                       atcf%,            /* WARNING FLAG               */~
                       1%,               /* PRESCAN, TRY HERE          */~
                       #2,               /* PIPMASTR                   */~
                       #41,              /* SFCUM2  CUMULATIVE FCSTS   */~
                       #62)              /* MATERIALS WORKFILE         */~

           if qtyp(m%) >= 0 then L21090
L21080:       qtyp(m%)  = 0 : goto L21500
L21090:    if qtyp(m%)  > 0 then L21110
L21100:       action%(m%) = 1% : goto L21550
L21110:    if qtyp(m%) >= qtyu(m%) then L21080

           if type%(m%) > 199% and type%(m%) < 500% then L21080
           if mkr$  = "PH" then L21162
           if mkr$ <> "PA" then L21080

L21162:    gosub'99(type%(m%),148%,test%)
           if test% = 0% then L21080

           if m% + 1% < matarray% then L21210
              errormsg$="MAXIMUM COMPONENTS EXCEEDED FOR THIS PLAN"
              action%(l%) = 0%
              goto workfile

L21210:    part$   (m% + 1%) = part$   (m%)
           bom$    (m% + 1%) = bom$    (m%)
           rte$    (m% + 1%) = rte$    (m%)
           type%   (m% + 1%) = type%   (m%)
           parline%(m% + 1%) = parline%(m%)
           ed%     (m% + 1%) = ed%     (m%)
           sd%     (m% + 1%) = sd%     (m%)
           lt%     (m% + 1%) = lt%     (m%)
           moq%    (m% + 1%) = moq%    (m%)
           qtyu    (m% + 1%) = qtyp(m%)
           qtyp    (m% + 1%) = 0
           action% (m% + 1%) = 0%
           qtyu    (m%)      = qtyu(m%) - qtyp(m%)
           qtyp    (m%)      = 0
           action% (m%)      = 1%
           maxlines% = maxlines% + 1%
           m% = m% + 1%
           goto L22020

L21500:    if type%(m%) > 199% and type%(m%) < 500% then L21550

           if mkr$ = "PH" then L22020
           if mkr$ = "PA" then L22020

L21550:    maxlines% = maxlines% + 1%






           goto L19500

        REM * * * PHANTOM LOOP LOGIC * * *

L22020:      if ti% < 101% then L22050
             errormsg$="TOO MANY PHANTOMS FOR THIS ASSEMBLY " & part$(l%)
             goto return_unplanned
L22050:      oldcompplowkey$(ti%) = compplowkey$
             pbshold%(ti%) = pbs%
             ti% = ti% + 1%
             if mkr$ <> "PA" then L22120
             if paok$(ti% - 1%) <> "Y" then L22120
                palevel%, palevel%(ti%) = palevel% + 1%
                paok$(ti%) = "Y"
L22120
*           PHFACT(TI%)= PHFACT(TI%-1%)*QU*TU
             phfact(ti%)= qtyu(m%) / qtyp(l%)
                  if bom$(m%) <> " " then L22230
                     testbom$ = str(part$(m%),1,25) & "1" & hex(000000)
                         call "PLOWNEXT" (#24, testbom$, 26%, f1%(24))
                         if f1%(24%) = 1% then L22200
                                   errormsg$ = "PART: " & part$(m%) &    ~
                                               " HAS NO EFFECTIVE BOM."
                                   goto return_unplanned
L22200:                   get #24, using L22210, eff$()
L22210:                       FMT XX(29), 490 * CH(3)
                     bom$(m%) = eff$(ed%(m%))
L22230:      compplowkey$ = str(part$(m%),1,25) &                        ~
                                            str(bom$(m%),1,3) & "  0"
             call "READ100" (#15, compplowkey$, f1%(15))
                  if f1%(15%) <>  0% then L22250
                     errormsg$ = "PART: " & part$(m%) & " BOM: " &       ~
                                 bom$(m%) & " NOT FOUND."
                     goto return_unplanned
L22250:      gosub L19500
             paok$(ti%) = " "
             ti% = ti% - 1%
             compplowkey$ = oldcompplowkey$(ti%)
             goto L19500

        REM *************************************************************~
            *  COMMON LOADER FOR PART/COMPONENT FROM HNY INFO           *~
            *************************************************************
            deffn'64(hny%)

              errormsg$ = "PART: " & part$(hny%) & " NOT IN MASTER FILE"
           call"READ100"(#2, part$(hny%), f1%(2))
               if f1%(2%) = 0% then return_unplanned

           get #2, using L22600, moq, type%(hny%), lt%(hny%)
L22600:         FMT POS(2003), PD(14,4), XX(8), 2*BI(2)

*        If approvals are required the part type obtained from the
*        BOMMASTR header record for the components is used for planning.

          if str(planflags$(),206,1) <> "Y" or hny% = 1% then L22690
              if type$ >= "000" and type$ <= "999" then                  ~
                                             convert type$ to type%(hny%)

L22690:         errormsg$ = "INCORRECT TYPE FOR PART: " & part$(hny%)
           if type%(hny%)>0 and type%(hny%)<200 then return_unplanned

           moq%(hny%) = moq
           if type%(hny%) > 0% then L22750
           moq%(hny%)=0%:optflag%=1%
L22750:         errormsg$ = " "

           return

        REM SET PURCHASE LEAD TIME
            deffn'65(hny%)

                pltkey$ = str(part$(hny%)) & "2001"
                call "READ100" (#24, pltkey$, f1%(24))
                    if f1%(24%) = 0% then return
                get #24 using L22860, eff$()
L22860:              FMT XX(29), 490*CH(3)
                convert eff$(mod(ed%(hny%) - pltbase%, 365%) + 1%)  to   ~
                                        lt%(hny%), data goto L22890
L22890:         return

        EJECT
        rem**************************************************************~
           *                 j u m p   l o g i c                        *~
           *                                                            *~
           *  jumping is required when we have encountered a part       *~
           *  which cannot be procurred and for which there are no      *~
           *  procurable alternates.  the distance to jump is based     *~
           *  on the constraining part and all of its parents.          *~
           **************************************************************

        jump
           errormsg$ = "PROCUREMENT DEMAND REQUEST NOT FEASIBLE"
           if dt$ = "8" then gosub return_unplanned
           errormsg$=" "

           if top_iw% = -1% then gosub'059(1%)
           top_iw% = 0%

           if type%(l%) > 499% or type%(l%) = 0%  then L23360

         REM ** NON-MANUFACTURED PARTS

           init(" ") scrn23$, scrn24$
           put scrn23$, using scrn10, l% , qtyp(l%), part$(l%)
           scrndate1$ = yymmdd$(ed%(l%)) : call "DATEFMT" (scrndate1$)
           scrn23$ = scrn23$ & " BY " & scrndate1$
           gosub'080 (scrn23$, 23%, 1%)
           gosub'080 (scrn24$, 24%, 1%)

           if printflag% = 0% then L23510
               if line% > 58% then gosub page_head
             date_fmtr$ = yymmdd$(ed%(l%))
             call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
             date_fmtr$ = yymmdd$(today%)
             call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

               print using L45410, str(ccyymmdd$(ed%(l%)), 3%, 6%), lt%(l%),  ~
                                   str(ccyymmdd$(today%),  3%, 6%),           ~
                                   lt%(l%) - ed%(l%) + today%
               line% = line% + 1%
               goto L23510

L23360:  REM ** MANUFACTURED PARTS

           init(" ") scrn23$, scrn24$
           put scrn23$, using scrn11, l% , qtyp(l%), part$(l%)
           scrndate1$ = yymmdd$(ed%(l%)) : call "DATEFMT" (scrndate1$)
           scrn23$ = scrn23$ & " BY " & scrndate1$
           gosub'080 (scrn23$, 23%, 1%)
           put scrn24$, using scrn12, x%, loi%, lwc$, minjump%
           gosub'080 (scrn24$, 24%, 1%)

           if printflag% = 0% then L23510
               if line% > 58% then gosub page_head
               print using L45390, x%, loi%, lwc$, minjump%
               line% = line% + 1%

L23510:    ed%(1%) = ed%(1%) + minjump%
           if ed%(1%) > 490% or ed%(1%) - cd% > jumpmax% then L23690
           workfile%, maxi% = 0%
           l%, maxlines% = 1%
           offset% = resched%
           today% = todaye%
           bom$(1%)=bom$:rte$(1%)=" "
           str(optkey$,1,23)=str(dc$,1,16) & str(dl$,1,3) & hex(00000000)
           optkeyhold$ = optkey$

           gosub'65(1%)

           qtyu(1%)=qtyu
           if dt$="4" or dt$ = "5" then gosub check_forecast
           gosub screenloop
           if str(planflags$(),5,1) = "Y" and (dt$="4" or dt$="5")       ~
                                                      then planflag% = 0%
           goto L12000

L23690:    errormsg$ = "THIS DEMAND CANNOT BE MET WITHIN THE PLANNING PER~
        ~IOD"
           gosub return_unplanned


*       ***************************************************************
*       **      GO FISH ON TOP LEVEL ASSEMBLY IF JUMP OCCURS        ***
*       ***************************************************************
        def fn'059(a%)

              top_iw% = 0%
              if dt$ >= "7" and a% = 1% then return
              if type%(a%) = 0% then return

*         Set Go Fish Scan Increment For MFG'd Or PURCHASED Part
              if type%(a%) > 499% then scan_step% = mfgscan%             ~
                                  else scan_step% = purscan%

              if minjump% <= scan_step% or scan_step% = 0% then return
              ll% = l%
              l% = a%
              holded% = ed%(l%)
              gosub get_vend
              for jmp% = holded%+minjump% to holded%+1% step -scan_step%
                  ed%(l%) = jmp%
                  if awcu3%(ed%(l%)) <= 0% then L23910
                  gosub calciw
                  if qtyp(l%) <> 0% then L23915
                     top_iw% = 1%
                     minjump% = ed%(l%) - holded%
L23910:       next jmp%
L23915:       ed%(l%) = holded%
              l% = ll%
              return

        EJECT
        rem**************************************************************~
           *                   c a l c i w                              *~
           *                                                            *~
           *  this routine calculates the amount that can be withdrawn  *~
           *  from planned inventory as of ed%(l%) and sets qtyp(l%)    *~
           **************************************************************

        calciw

           if printflag% = 0% then L24140
               if line% > 58% then gosub page_head
             date_fmtr$ = yymmdd$(ed%(l%))
             call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))

               print using L45290, part$(l%), str(ccyymmdd$(ed%(l%)), 3%, 6%)
               line% = line% + 1%

L24140:    call "PLANCIW"                                                ~
                      (dt$,              /* DEMAND TYPE                */~
                       testdemd$,        /* SWITCH MASK                */~
                       errormsg$,        /* THE RETURN MESSAGE         */~
                       qtydd,            /* QTY ON DELIVERY DAY        */~
                       cd%,              /* COMPLETION DAY OFFSET      */~
                       l%,               /* MATERIALS ARRAY LOCATION   */~
                       todaya%,          /* ACTUAL TODAY               */~
                       workfile%,        /* WORKFILE STATUS = 0        */~
                       atcf%,            /* WARNING FLAG               */~
                       0%,               /* NO PRESCAN, REAL THING     */~
                       #2,               /* PIPMASTR                   */~
                       #41,              /* SFCUM2  CUMULATIVE FCSTS   */~
                       #62)              /* MATERIALS WORKFILE         */~

            if errormsg$ <> " " then gosub return_unplanned

            if atcf% = 0% then return
            if printflag% = 0% then return
                if line% > 58% then gosub page_head
                print using L45480, part$(l%)
                line% = line% + 1%
                return

        EJECT
        rem**************************************************************~
           *                      m f g d a t e                         *~
           *  this routine calculates the latest date that manufactur-  *~
           *  ing can start and still make the desired ending date.     *~
           *    every time some capacity is used up the work center,    *~
           *  date, and amount used are loaded into a stack array that  *~
           *  will either get emptied in jump, or will be used to write *~
           *  the capacity useage record during bookorder.              *~
           *    the variable pd%() is used to hold the starting date    *~
           *  for each step in the work center sequence.  this date is  *~
           *  used by the split pick logic.                             *~
           *    the bom to use is obtained for the desired ending date, *~
           *  (or operator specified.) the route is determined from the *~
           *  bom.                                                      *~
           **************************************************************

        mfgdate
         REM ** GET BOM ID EITHER USE SPECIFIED ONE -OR- GET EFFECTIVE **

           sd%(l%) = ed%(l%):init (" ") step$()
           if bom$(l%) <> " " then getroute             /* BOM SUPPLIED*/

*        If Approvals are required, MFG components must be hard pegged
           if str(planflags$(),206,1) <> "Y" or l% = 1% then L25220
                 errormsg$ = "APPROVALS REQUIRED, PART: " & part$(l%)   &~
                                                 " MUST BE HARD PEGGED."
                 gosub return_unplanned

L25220:    testbom$ = str(part$(l%),1,25) & "1" & hex(000000)
           call "PLOWNEXT" (#24,testbom$,26%,f1%(24))
                if f1%(24%) = 1% then L25280
                     errormsg$ = "PART: " & part$(l%) &                  ~
                                  " HAS NO EFFECTIVE BOM."
                     goto return_unplanned
L25280:         get #24, using L25290, eff$()
L25290:              FMT XX(29), 490 * CH(3)
                bom$(l%) = eff$(ed%(l%))

         getroute  /* OBTAIN ROUTE-ID FROM BOM     */
           compplowkey$ = str(part$(l%),1,25) & str(bom$(l%),1,3) & "  0"
           call "READ100" (#15, compplowkey$, f1%(15))
                if f1%(15%) =  1% then L25390
                     errormsg$ = "PART: " & part$(l%) & " BOM: " &       ~
                                 bom$(l%) & " NOT FOUND."
                goto return_unplanned
L25390:    get #15, using L25395, rte$(l%), approval$, pjflag$
L25395:         FMT XX(86), CH(3), POS(115), CH(6), POS(145), CH(1)
           if pjflag$ <> "Y" then L25399
           gosub'99(type%(l%),154%,test%)
             if test% = 1% then L25402
L25399:         pjflag$ = " "

L25402
*        If approvals are required, the BOM must be approved to plan.
           if str(planflags$(),206,1)<>"Y" or approval$<>" " then L25440
                 errormsg$ = "PART: " & part$(l%) & " BOM: " &           ~
                                 bom$(l%) & " NOT APPROVED"
                 gosub return_unplanned

         REM ** END OBTAINING BOM & ASSOCIATED ROUTE ***

L25440:    x%,xx%,loi% = 0%:startmaxi% = maxi%:startmaxlines% = maxlines%

           scrn$ = "ANALYZING WORK CENTER ROUTING: ROUTE " & rte$(l%) &  ~
                   ", BOM " & bom$(l%)
           gosub'080 (scrn$, 3%, 1%)

L25500: REM * * * START OF ROUTE LOADING LOOP * * *
         if pjflag$ <> " " then L25820     /* Use Lead time for PJobs */
         if rte$(l%) = " " then L25750
            gosub'99(type%(l%),112%,test%)
            if test% = 1% then L25820

            call"PLANRTE"                /*                            */~
                     (part$(l%),         /* PART NEEDED                */~
                      rte$(l%),          /* THE WC ROUTE TO USE        */~
                      bom$(l%),          /* WHICH BOM TO USE           */~
                      errormsg$,         /* THE RETURN MESSAGE         */~
                      rtestep$(),        /* THE DERIVED ROUTE          */~
                      ed%(l%),           /* EFFECTIVE DATE             */~
                      x%,                /* # OF ELEMENTS              */~
                      #15,               /* BOMMASTR                   */~
                      #7,                /* RTEMASTR                   */~
                      #24)               /* ENGMASTR                   */~

            if errormsg$ <> " " then return_unplanned

        rem** this code is only for the printed report ******************
          if xx% > 0% then L26000
          loi% = x%
          if loi% >0% then L25900
          if rte$(l%)<>" " then L25780
L25750:      gosub'99(type%(l%),94%,test%)
             if test% = 1% then L25820
L25780:         errormsg$="ROUTE HAS NO STEPS, " & str(part$(l%),,25) &  ~
                                                        rte$(l%)
                goto return_unplanned

L25820:     loi%=1%
            put str(rtestep$(1%)), using L25860, " ", lt%(l%), 0%, 0, " ",~
                    1, " ", 1, " ", 1, 1, " ", 100%, 100, " ", 0%, " ",  ~
                    1, " ", 0, 100, 0, 100, " "
L25860:         FMT  CH(35), BI(4), BI(2), PD(14,4), CH(4), PD(7,4),     ~
                     CH(4), PD(7,4), CH(4), PD(7,4), PD(7,6), CH(1),     ~
                     BI(1), PD(14,7), CH(4), BI(1), CH(43), PD(14,4),    ~
                     CH(1), 4*PD(14,4), CH(16)
L25900:   if printflag% = 0% then L26000  /* IF NO REPORT, BRANCH AROUND */
              if line% > 58% then gosub page_head
              print using L45310, loi%, rte$(l%), part$(l%)
              line% = line% + 1%

L26000: REM *** END OF REPORT ONLY CODE, ALLOCATION CONTROL CODE ***

           if xx% = 0% then xx% = loi%
           overlap = 0
           nextpass = 1 : savesdl% = 999%:mat cumuse = zer
           cumuse   = 0 : xxsd%(xx%), xsd%(xx%), useday%  = sd%(l%)
           for x% = xx% to offset% + 1% step -1% /* WORK BACKWARD */
                     gosub L33000
                     mat stpuse = zer
                     mmx%(x%) = maxi%
                     ovrlp_restart% = 0%
                     savesdl% = min(sd%(l%), savesdl%)
                     if requnits% >= 0% then L26086
                        xsd%(x%) = xxsd%(x%)
                        overlap  = 0
L26086:              if overlap <= 0 then L26100
                        nextpass = max(0, 1 - cumuse(xsd%(x%)))
                        nextpass = min(1, nextpass + overlap)
L26100:              sd%(l%)  = xsd%(x%):overlap = 0
                     if x% > 1% then xxsd%(x%-1%), xsd%(x%-1%) = sd%(l%)
                     if mqf% = 0% then L26150
                        xxsd%(x%), xsd%(x%) = min(sd%(l%)+ mqf%, ed%(l%))
                        sd%(l%)  = xsd%(x%)
                        cumuse   = 0:mat cumuse = zer
                        useday%  = sd%(l%)
                        nextpass = 1
L26150:              pd%(x%)  = sd%(l%)-1%
                     if mq% + tr% > 0 then L26200
                        if x% = xx% then L26360
                        pd%(x%) = pd%(x%+1%)
                        goto L26360
L26200:              if pd%(x%) <= today% then L26380
                     init (hex(00)) testbom$
                     str(testbom$,1,31) = str(rtestep$(x%),,31)
                     holdnextpass=nextpass
                     holdtr% = tr%:holdsu% = su%
                     wcalters% = 0%



                     if lwc$ = "VEND" then L26310
                     if lwc$ <> " "   then L26340
L26310:                 gosub alloc_vend_time
                        goto L26360

L26340:                 gosub alloc_wc_time

L26360:              if pd%(x%) <= today% then L26380
                     if sd%(l%) >= today% then L26420
L26380:                        maxi%=startmaxi%
                               sd%(l%)=min(pd%(x%), sd%(l%))

                               return
L26420:              tr% = holdtr%:su% = holdsu%
                     mat cumuse = cumuse + stpuse
                     gosub scrnwcplot
           next x%                  /* FOR EACH WC STEP           */

           sd%(l%) = min(savesdl%, sd%(l%))
           if sd%(l%)<=today% then sd%(l%)=today%+1%
           sd%(l%)=min(pd%(offset% + 1%), sd%(l%))
           init(" ") scrn$
           date_fmtr$ = yymmdd$(sd%(l%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
           date_fmtr$ = yymmdd$(ed%(l%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

           put scrn$ using L45340, str(ccyymmdd$(1%), 3%, 6%),  ~
                                   str(ccyymmdd$(2%), 3%, 6%),  ~
                           ed%(l%)+1% -sd%(l%), qtyp(l%), part$(l%)
           gosub'080 (scrn$, 3%, 1%)

           if printflag% = 0% then return
                if line% > 58% then gosub page_head
           date_fmtr$ = yymmdd$(sd%(l%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
           date_fmtr$ = yymmdd$(ed%(l%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

                print using L45340, str(ccyymmdd$(1%), 3%, 6%), ~
                                    str(ccyymmdd$(2%), 3%, 6%), ~
                            ed%(l%)+1% -sd%(l%), qtyp(l%), part$(l%)
                line% = line% + 1%
                return


        EJECT
        REM * * * VENDOR STEP, ALLOCATE TIME ONLY * * *
        alloc_vend_time

                gosub get_vend
                if nextpass = 1 then L27140
                        sd%(l%)=sd%(l%)-1%:pd%(x%)=pd%(x%)-1%
                        if pd%(x%) > today% then L27130
                        gosub rte_alters
                        return
L27130:         nextpass = 1
L27140:         qwwq% = mq%+tr%
                if pd%(x%) - qwwq% + 1% > today% then L27190
                     pd%(x%)=pd%(x%)-qwwq%+1%:sd%(l%)=sd%(l%)-qwwq%+1%
                          gosub rte_alters
                          return
L27190:         if maxi% < wcarray% then L27220
                errormsg$="TOO MANY WC ALLOCATION STEPS IN THIS PLAN"
                goto workfile
                        /* NOTE: AWCU3%() is the Avail. in W/C "VEND" */
L27220:              if awcu3%(sd%(l%)) > 0% then L27270
                        sd%(l%)=sd%(l%)-1%:pd%(x%)=pd%(x%)-1%
                        if pd%(x%) > today% then L27220
                        gosub rte_alters
                        return
L27270:              if qwwq% <= 0% then return
                     maxi%      = maxi% + 1%
                     wc$(maxi%) = lwc$
                     du%(maxi%) = sd%(l%)
                     wl%(maxi%) = l%
                     au%(maxi%) = 0%
                     su%(maxi%) = 0%
                     ws$(maxi%) = str(rtestep$(x%),88,5)
                     wl$(maxi%) = bin(x%):wa$(maxi%)=hex(00)
                     wa%(maxi%) = 0%
                     wd$(maxi%) = wcpact$
                     if str(testbom$,32,3) = hex(000000) then L27400
                     convert str(testbom$,32,3) to wa%(maxi%),           ~
                                                         data goto L27400
L27400:              pd%(x%), sd%(l%)  = sd%(l%) - 1%
                     if x% > 1% then xxsd%(x%-1%), xsd%(x%-1%) = sd%(l%)
                     if pd%(x%) > today% then L27440
                        gosub rte_alters
                        return
L27440:         qwwq% = qwwq% - 1%
                if qwwq% > 0% then L27190
L27460:         if awcu3%(pd%(x%)) > 0% then return
                   pd%(x%)=pd%(x%)-1%
                   if pd%(x%) > today% then L27460
                      gosub rte_alters
                      return

        REM * * * * * LOAD VENDOR SCHEDULE FROM WCMASTR * * * * *
        get_vend
                if got_vend% = 1% then return
                mat awcu3% = con
                call"READ100"(#11, "VEND ", f1%(11))
                     if f1%(11%) <> 1% then L27670
                get #11, using L27660, awcu3%()
L27660:              FMT XX(59), 490*BI(2)
                got_vend% = 1%
L27670:         return

        EJECT
        REM * * * ALLOCATE WORK CENTER TIME AND CAPACITY * * *
        alloc_wc_time

                mat awca% = con
                got_vend% = 0%
                if lwc1$ = " " then L28190
                call"READ100"(#11, str(lwc1$,1,4)& " ",f1%(11))
                     act$ = lwc1$
                     if f1%(11%) <> 1% then L33800
                get #11, using L28380, awcu1%(), pip%()
                    mat awca% = awcu1%:mat awcu1% = awcu1% - pip%
                if lwc2$ = " " then L28190
                call"READ100"(#11, str(lwc2$,1,4)& " ",f1%(11))
                     act$ = lwc2$
                     if f1%(11%) <> 1% then L33800
                get #11, using L28380, awcu2%(), pip%()
                    mat awca% = awca% + awcu2%:mat awcu2% = awcu2% - pip%
                if lwc3$ = " " then L28190
                call"READ100"(#11, str(lwc3$,1,4)& " ",f1%(11))
                     act$ = lwc3$
                     if f1%(11%) <> 1% then L33800
                get #11, using L28380, awcu3%(), pip%()
                    mat awca% = awca% + awcu3%:mat awcu3% = awcu3% - pip%
L28190:         call"READ100"(#11, str(lwc$,1,4)& " ",f1%(11))
                     act$ = lwc$
                     if f1%(11%) <> 1% then L33800
                get #11, using L28380, cumf%(), pip%()

        REM TEST FOR DISREGARD OF WC CAPACITIES

          gosub'99(type%(l%),88%,test%)
          if test% = 1% then L28320
             if lwc1$ = " " then L28330
             if lwc2$ = " " then L28340
             if lwc3$ = " " then L28350
                goto L28400
L28320:            mat pip%   = zer
L28330:            mat awcu1% = con:mat awcu1% = (999999%)*awcu1%
L28340:            mat awcu2% = con:mat awcu2% = (999999%)*awcu2%
L28350:            mat awcu3% = con:mat awcu3% = (999999%)*awcu3%
          if test% = 1% then L29000

L28380:              FMT XX(59), 490*BI(2), 490*BI(2)

L28400: REM * * * ADJUST FOR CAPACITY USED, BUT NOT YET RECORDED * * *
            if tr% < 1% then L29000
            if maxi% = 0% then L29000
                for ttt% = 1% to maxi%

                 if wc$(ttt%) <> lwc$ then L28480
                    pip%(du%(ttt%))  =pip%(du%(ttt%))   + au%(ttt%)
                    goto L28580
L28480:          if wc$(ttt%) <> lwc1$ then L28510
                    awcu1%(du%(ttt%))=awcu1%(du%(ttt%)) - au%(ttt%)
                    goto L28580
L28510:          if wc$(ttt%) <> lwc2$ then L28540
                    awcu2%(du%(ttt%))=awcu2%(du%(ttt%)) - au%(ttt%)
                    goto L28580
L28540:          if wc$(ttt%) <> lwc3$ then L28580
                    awcu3%(du%(ttt%))=awcu3%(du%(ttt%)) - au%(ttt%)
                    goto L28580

L28580:         next ttt%

        EJECT
L29000: REM * * * START OF ACTUAL WORK CENTER TIME ALLOCATION LOOP * * *

                if mqp$ <> "Y" then L29490
                if mq%  <   1% then L29490
                if nextpass = 1 then L29130
                        sd%(l%)=sd%(l%)-1%
                        if sd%(l%) > today% then L29130
                        gosub rte_alters
                        return
L29130:         nextpass = 1:aaa% = mq%
L29140:         if aaa% < 1% then L29390
                if cumf%(sd%(l%)) <= 0% then L29330
                aaa% = aaa% - 1%
                if maxi% < wcarray% then L29200
                   errormsg$="TOO MANY WC ALLOCATION STEPS IN THIS PLAN"
                   goto workfile
L29200:         maxi% = maxi% + 1%
                     wc$(maxi%) = lwc$
                     du%(maxi%) = sd%(l%)
                     au%(maxi%) = 0%
                     su%(maxi%) = 0%
                     wl%(maxi%) = l%
                     wl$(maxi%) = bin(x%):wa$(maxi%)=hex(09)
                     ws$(maxi%) = str(rtestep$(x%),88,5)
                     wa%(maxi%) = 0%
                     wd$(maxi%) = " "
                     if str(testbom$,32,3) = hex(000000) then L29330
                     convert str(testbom$,32,3) to wa%(maxi%),           ~
                                                         data goto L29330

L29330:         sd%(l%) = sd%(l%) - 1%
                if sd%(l%) >= today% then L29140
                     gosub rte_alters
                     pd%(x%)=sd%(l%)
                     return

L29390:         if tr% > 0% then L29460
                pd%(x%) = du%(mmx%(x%)+1%) : aaa% = 0
                if x% > 1% then xxsd%(x%-1%), xsd%(x%-1%) = sd%(l%)
                if pd%(x%) > today% then L30670
L29430:            gosub rte_alters
                   return

L29460:         pd%(x%) = sd%(l%) - 1%
                if pd%(x%) <= today% then L29430

L29490:         if cumf%(sd%(l%)) <= 0 then L30610
                if awca%(sd%(l%)) <= 0 then wcrestart

                aaa%, asu% = 0%

L29540:         if requnits% <= 0% or x% = xx% then L29670
                if requnits% > tr% and sd%(l%) > pd%(x%+1%) then L29580
                goto L29670
L29580:            ovrlp_restart% = 1%
                   goto wcrestart

L29670:         if tr% < 1% then L30670

                pip%(sd%(l%))   = max(pip%(sd%(l%))  , 0%)
                awcu1%(sd%(l%)) = max(awcu1%(sd%(l%)), 0%)
                awcu2%(sd%(l%)) = max(awcu2%(sd%(l%)), 0%)
                awcu3%(sd%(l%)) = max(awcu3%(sd%(l%)), 0%)

                aaa = min(cumf%(sd%(l%)) - pip%(sd%(l%)),                ~
                        cumf%(sd%(l%))*shift, cumf%(sd%(l%))*nextpass)
                aaa = min(aaa, awcu1%(sd%(l%))/nm1)
                aaa = min(aaa, awcu2%(sd%(l%))/nm2)
                aaa = min(aaa, awcu3%(sd%(l%))/nm3)

                aaa% = max(0%, round(aaa,0))
                if aaa% < 1% then wcrestart
                aaa% = min(tr%, aaa%)
                if x% = 1% then L29880
                if padunits% < 0% then L29860
                if holdtr% - tr% > padunits% then L29870
L29860:            xsd%(x%-1%)  = sd%(l%)
L29870:            xxsd%(x%-1%) = sd%(l%)
L29880:         tr%  = tr% - aaa%
                aaa = aaa%
                aaa = 100*aaa/cumf%(sd%(l%))
                stpuse(sd%(l%)) = stpuse(sd%(l%)) + (aaa/100)
                if tr% >= su% then L29960
                asu% = max(0%, su% - tr%)
                su%  = max(0%, su% - asu%)

L29960:         if maxi% < wcarray% then L29990
                   errormsg$="TOO MANY WC ALLOCATION STEPS IN THIS PLAN"
                   goto workfile
L29990:         maxi% = maxi% + 1%
                     wc$(maxi%) = lwc$
                     du%(maxi%) = sd%(l%)
                     au%(maxi%) = aaa%
                     su%(maxi%) = asu%
                     wl%(maxi%) = l%
                     pip%(sd%(l%)) = pip%(sd%(l%)) + aaa%
                     wl$(maxi%) = bin(x%):wa$(maxi%)=hex(01)
                     ws$(maxi%) = str(rtestep$(x%),88,5)
                     wa%(maxi%) = 0%
                     wd$(maxi%) = wcpact$
                     if aaa% - asu% <= 0% then wd$(maxi%) = wcsact$
                     if str(testbom$,32,3) = hex(000000) then L30120
                     convert str(testbom$,32,3) to wa%(maxi%),           ~
                                                         data goto L30120
L30120:         if lwc1$ = " " then L30550
                if maxi% < wcarray% then L30160
                   errormsg$="TOO MANY WC ALLOCATION STEPS IN THIS PLAN"
                   goto workfile
L30160:         maxi% = maxi% + 1%
                     wc$(maxi%) = lwc1$
                     du%(maxi%) = sd%(l%)
                     au%(maxi%) = round(aaa%*nm1, 0)
                     su%(maxi%) = round(asu%*nm1, 0)
                     wl%(maxi%) = l%
                     wl$(maxi%) = bin(x%):wa$(maxi%)=hex(02)
                     awcu1%(sd%(l%)) = awcu1%(sd%(l%)) - au%(maxi%)
                     ws$(maxi%) = ws$(maxi% - 1%)
                     wa%(maxi%) = wa%(maxi% - 1%)
                     wd$(maxi%) = wc1act$
                     if au%(maxi%) - su%(maxi%) <= 0% then wd$(maxi%) =" "
                if lwc2$ = " " then L30550
                if maxi% < wcarray% then L30300
                   errormsg$="TOO MANY WC ALLOCATION STEPS IN THIS PLAN"
                   goto workfile
L30300:         maxi% = maxi% + 1%
                     wc$(maxi%) = lwc2$
                     du%(maxi%) = sd%(l%)
                     au%(maxi%) = round(aaa%*nm2, 0)
                     su%(maxi%) = round(asu%*nm2, 0)
                     wl%(maxi%) = l%
                     wl$(maxi%) = bin(x%):wa$(maxi%)=hex(03)
                     awcu2%(sd%(l%)) = awcu2%(sd%(l%)) - au%(maxi%)
                     ws$(maxi%) = ws$(maxi% - 1%)
                     wa%(maxi%) = wa%(maxi% - 1%)
                     wd$(maxi%) = wc2act$
                     if au%(maxi%) - su%(maxi%) <= 0% then wd$(maxi%) =" "
                if lwc3$ = " " then L30550
                if maxi% < wcarray% then L30440
                   errormsg$="TOO MANY WC ALLOCATION STEPS IN THIS PLAN"
                   goto workfile
L30440:         maxi% = maxi% + 1%
                     wc$(maxi%) = lwc3$
                     du%(maxi%) = sd%(l%)
                     au%(maxi%) = round(aaa%*nm3, 0)
                     au%(maxi%) = round(asu%*nm3, 0)
                     wl%(maxi%) = l%
                     wl$(maxi%) = bin(x%):wa$(maxi%)=hex(04)
                     awcu3%(sd%(l%)) = awcu3%(sd%(l%)) - au%(maxi%)
                     ws$(maxi%) = ws$(maxi% - 1%)
                     wa%(maxi%) = wa%(maxi% - 1%)
                     wd$(maxi%) = wc3act$
                     if au%(maxi%) - su%(maxi%) <= 0% then wd$(maxi%) =" "

L30550:         if padunits% < 0% then L30600
                if overlap <> 0 then L30600
                   overlap = max(0, holdtr% - (tr% + padunits%))
                   overlap = 100*overlap/cumf%(sd%(l%))
                   overlap = overlap/100
L30600:         if tr% < 1% then L29540
L30610:            sd%(l%) = sd%(l%)-1% : pd%(x%) = pd%(x%)-1%
                   nextpass = 1
                     if pd%(x%) > today% then L29490
                        gosub rte_alters
                        return

L30670: REM ALLOCATE WORK CENTER MQ TIME IF ANY, SET FACTOR FOR NEXT STEP
            if dt$ = "9" then return
L30690:         if cumf%(pd%(x%)) > 0% then L30740
                   pd%(x%) = pd%(x%) - 1%
                   if pd%(x%) > today% then L30690
                      gosub rte_alters
                      return
L30740:         if holdtr% >= 1% then L30780
                   nextpass = 1
                   return
L30780:         aaa = aaa%
                aaa = 100*aaa/cumf%(du%(maxi%))
                aaa = aaa/100
                if du%(maxi%) <> useday% then cumuse = 0
                cumuse = aaa + cumuse
                aaa% = min(cumuse + (1 - capmq), 1)
                useday% = du%(maxi%)
                if mqp$ = "Y" then L30825
                if mq% > 0% then aaa% = 1% + mq%
L30825:         if aaa% > 0% then L30840
                   nextpass = 1 - cumuse
                   return
L30840:         cumuse = 0
                nextpass = 1

L30855:         if aaa% < 1% then return
                if cumf%(sd%(l%)) <= 0% then L30955
                aaa% = aaa% - 1%
                if maxi% = mmx%(x%) then L30880
                if sd%(l%) = du%(maxi%) then L30955
L30880:         if maxi% < wcarray% then L30895
                   errormsg$="TOO MANY WC ALLOCATION STEPS IN THIS PLAN"
                   goto workfile
L30895:         maxi% = maxi% + 1%
                     wc$(maxi%) = lwc$
                     du%(maxi%) = sd%(l%)
                     au%(maxi%) = 0%
                     su%(maxi%) = 0%
                     wl%(maxi%) = l%
                     wl$(maxi%) = bin(x%):wa$(maxi%)=hex(00)
                     ws$(maxi%) = str(rtestep$(x%),88,5)
                     wa%(maxi%) = 0%
                     wd$(maxi%) = " "
                     if str(testbom$,32,3) = hex(000000) then L30955
                     convert str(testbom$,32,3) to wa%(maxi%),           ~
                                                         data goto L30955
L30955:         sd%(l%) = sd%(l%) - 1%
                if x% <= 1% then L30980
                if padunits% > 0% then L30975
                   xsd%(x%-1%)  = sd%(l%)
L30975:            xxsd%(x%-1%) = sd%(l%)
L30980:         if sd%(l%) >= today% then L30855
                gosub rte_alters
                return

        wcrestart
         if printflag% = 0% then L31120
           if line% > 58% then gosub page_head

           date_fmtr$ = yymmdd$(sd%(l%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
           date_fmtr$ = yymmdd$(pd%(x%+1%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

           if ovrlp_restart% = 0% then                                   ~
                 print using L45360, lwc$, x%, loi%, ~
                       str(ccyymmdd$(1%), 3%, 6%), tr%, holdtr% ~
           else  print using L45375, lwc$, x%, loi%, ~
                       str(ccyymmdd$(1%), 3%, 6%),   ~
                       str(ccyymmdd$(2%), 3%, 6%),   ~
                       str(rtestep$(x%+1%), 32, 4)  ~

                 line% = line% + 1%

L31120:      if dt$<>"9" then L31180
              maxi%=0% : tr%=holdtr% : nextpass = 1
              sd%(1%)=sd%(1%)-1%
              pd%(offset%+1%), xsd%(1%), xxsd%(1%) = sd%(1%)
              goto L29000

L31180:    gosub'99(type%(l%),100%,test%)
           if test% = 0% then L31230

           gosub rte_alters
             if wcalters% <> 0% then L31500
L31230:         if x% = xx% then L31270
                   if ovrlp_restart% = 1% then L31500
                   if str(rtestep$(x%),144,1) <> "Y" then L31270
                   if sd%(l%) <= pd%(x%+1%) then L31500
L31270:    nextpass = 1 : mat stpuse = zer : overlap = 0
           tr% = holdtr% : su% = holdsu%
           maxi% = mmx%(x%)

           sd%(l%) = sd%(l%) - 1%
           pd%(x%) = sd%(l%) - 1%
             if pd%(x%) > today% then L29000
                gosub rte_alters
                return

L31500: REM * * * RELOAD ORIGINAL ROUTE STEP AND GO * * *

         nx% = x%
L31503:  if str(rtestep$(nx%),144,1) <> "Y" then L31508
            if nx% = xx% then L31508
               nx% = nx% + 1%
               goto L31503

L31508:  init (hex(00)) testbom$

         for i% = x% to nx%
         call "READ100" (#7, str(rtestep$(i%),1,31), f1%(7))
         if f1%(7) <> 0 then L31550
                   errormsg$="ROUTE STEP ERROR:" & str(rtestep$(i%),1,31)
                    goto return_unplanned
L31550:         get #7 using L31570, str(rtestep$(i%),32,4),              ~
                                   str(rtestep$(i%),36,43)
L31570:               FMT CH(4), XX(31), CH(43)
          holdlwc$ = str(rtestep$(i%),32,4)
          str(testbom$,1,31) = str(rtestep$(i%),1,31)


          next i%

          x% = nx%

           holdnextpass, nextpass = 1
           wcalters% = 0%
L31625:    xsd%(x%) = xsd%(x%) - 1%
           sd%(l%) = xsd%(x%)
           if ovrlp_restart% <> 1% then xxsd%(x%) = xsd%(x%)
           pd%(x%) = sd%(l%)-1%
        if cumf%(sd%(l%)) <= 0% then L31625   /* Weekend or Holiday?    */
        if pd%(x%) > today% then gosub  L32220
        return

        REM * * *  LOAD ALTERNATE WORK CENTER STEP * * *
        rte_alters
            if dt$="9" then return
            call "PLOWNEXT" (#6, testbom$, 31%, f1%(6))
            if f1%(6) = 0% then return

            wcalters% = 1%
            holdlwc$ = lwc$
            nextpass = holdnextpass
            pd%(x%) = xsd%(x%)-1%
            sd%(l%) = xsd%(x%)
            get #6 using L32120, str(rtestep$(x%),32,47)
L32120:         FMT POS(35), CH(47)

            scrn$=" SUBSTITUTED WC ROUTE STEP:" & str(testbom$,1,34)
            if printflag% = 0% then L32200
                if line% > 58% then gosub page_head
                print scrn$
                line% = line% + 1%

L32200:     gosub'080 (scrn$, 3%, 1%)

L32220:     gosub L33000
            holdtr% = tr%:holdsu% = su%
            mat stpuse = zer:overlap = 0

            maxi% = mmx%(x%)
            return clear
            if lwc$="VEND" then alloc_vend_time
            goto alloc_wc_time

L33000: REM COMMON CONVERT ROUTINE FOR WC INFO

            get str(rtestep$(x%),32), using L33070,                       ~
                     lwc$, mq%, su%, run, lwc1$, nm1, lwc2$, nm2,        ~
                     lwc3$, nm3, shift, mqp$, yld(x%), step$(x%),        ~
                     wcpact$, phfact, padqty, padpct, reqqty, reqpct,    ~
                     wc1act$, wc2act$, wc3act$, wcsact$

L33070:         FMT  CH(4), BI(4), BI(2), PD(14,4), CH(4), PD(7,4),      ~
                     CH(4), PD(7,4), CH(4), PD(7,4), PD(7,6), CH(1),     ~
                     XX(1), PD(14,7), CH(5), XX(1), CH(4), XX(38),       ~
                     PD(14,4), POS(114), 4*PD(14,4), 4*CH(4)

           mqf% = 0%: if mq% >= 0% then L33160
           mqf% = -mq%
           mq%  = 0%

L33160:    if shift < .01 then shift = 1

           nm1 = max(.01, nm1)
           nm2 = max(.01, nm2)
           nm3 = max(.01, nm3)

           tr = max(0, su% + ((100*phfact*run*qtyp(l%))/yld(x%)))
           if lwc$  = "VEND" then L33250
           if lwc$ <> " "    then L33280
L33250:         tr% = round(tr/8,0)
                requnits% = 0%
             if reqqty >= qtyp(l%) or reqpct >= 100 then requnits% = -1
                return

L33280:        if tr > 0 then tr = max(tr, 1)
               tr% = round(tr,0)
            gosub'99(type%(l%),124%,test%)
            if test% = 0% then L33350
               mq% = 0%

L33350:     padunits% = -1%
            if tr%-su% <= 0% then L33440
            if padpct >= 100 then L33440
            if padqty >= qtyp(l%) then L33440
               padunits1  = max(0, (100*phfact*run*qtyp(l%))/yld(x%))
               padunits1% = (padunits1 * padpct)/100
               padunits2% = max(0, (100*phfact*run*padqty)/yld(x%))
               padunits%  = min(tr%-su%, max(padunits1%, padunits2%))

L33440:     requnits% = -1%
            if tr%-su% <= 0% then L33530
            if reqpct >= 100 then L33530
            if reqqty >= qtyp(l%) then L33530
               requnits1  = max(0, (100*phfact*run*qtyp(l%))/yld(x%))
               requnits1% = (requnits1 * reqpct)/100
               requnits2% = max(0, (100*phfact*run*reqqty)/yld(x%))
               requnits%  = min(tr%-su%, max(requnits1%, requnits2%))

L33530:        if mq% > 0% then L33570
                  mqp$=" "
                  return

L33570:        if tr% > 0% then return
                  mqp$="Y"
                  return

L33800: REM * * *  ERROR MESSAGES AND KICK OUT * * *
           if dt$="9" then L33860
            errormsg$="WORK CENTER NOT FOUND, " & act$ & " " & part$(l%) ~
                                                       &  rte$(l%)
            goto return_unplanned

L33860:            errormsg$ = "CANNOT FIND WORK CENTER, PM NOT PLANNED"
                   goto return_unplanned

        EJECT
        rem**************************************************************~
           *                 b o o k   o r d e r                        *~
           *                                                            *~
           *  the demand has been successfully planned for a completion *~
           *  date of ed%(1).  this routine will write the plan info    *~
           *  to the required files.  all iw's are recorded in          *~
           *  pipused and have changed pipmastr by the qtyu ().         *~
           *  jo's and po's are recorded in plnorder.  if the           *~
           *  qtyp () > qtyu () then the net addition is recorded in    *~
           *  pipmastr and the details in pipused.  simularly the       *~
           *  work center useage is recorded in wcmastr under 'U' and   *~
           *  the details in wcused.  unplanning is facilitated since   *~
           *  all three files can be plowed quickly.                    *~
           **************************************************************

        book_order
           gosub scrnsuccess
           if dt$ <> "9" then L35220
                intagnr$(1%)= "WO" & "   " & tagdate$ & time
                convert sd%(1%) to str(intagnr$(1%),3,3), pic(###)
                goto next_pip_action

L35220:    if maxi% < 1% then L35228
              if wl%(1%) <> 1% then L35228   /* Not Associated */
              if du%(1%) < ed%(1%) then ed%(1%) = du%(1%)

L35228:    gosub bo_optimize
           if planflag% < 1% then next_pip_action

        REM CREATE TAG NUMBERS
           for j% = 1% to maxlines%
                     if j% > 1% then L35320
                     if dt$ =  "1" then L35310
                     if dt$ =  "2" then L35310
                     goto L35320
L35310:              outtagnr$(j%) = str(dc$,1,16) & str(dl$,1,3)
L35320:    if action%(j%) <> 4% then L35325
                     str(intagnr$(j%),1,2)  = "BW"
                     goto L36000
L35325:    if action%(j%) <> 3% then L35410
                     str(intagnr$(j%),1,2)  = "WO"
                     goto L36000
L35410:    if action%(j%) <> 2% then L36000
                     str(intagnr$(j%),1,2)  = "BO"

L36000: REM NOW WRITE PIPIN/PIPOUT
           if intagnr$(j%) = " " then L36320
           convert sd%(j%) to str(intagnr$(j%),3,3), pic(###)
L36020:    str(intagnr$(j%),6,14) = tagdate$ & time
           write #33, using  L36090   ,                                   ~
                     part$(j%),          /* PART                       */~
                     ed%(j%),            /* DATE DUE TO COME IN        */~
                     intagnr$(j%),       /* FROM WHICH JO/PO           */~
                     qtyp (j%),          /* QUANTITY TO COME IN        */~
                     sd%(j%),            /* DAY TO START               */~
                     eod goto L36020
L36090:        FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)

           if str(intagnr$(j%),1,2) =  "BW" then L36120
           if str(intagnr$(j%),1,2) <> "WO" then L36150
L36120:    write #8, using L36130, part$(j%), rte$(j%), intagnr$(j%),     ~
                     part$(j%), bom$(j%), intagnr$(j%), eod goto L36136
L36130:        FMT CH(25), CH(3), CH(19), CH(25), CH(3), CH(19)

L36136:              for k% = j% to maxlines%
                         if parline%(k%) <> j% then L36139
                         outtagnr$(k%) = intagnr$(j%)
L36139:              next k%

L36150:    if str(planflags$(),4,1) = "P" then L36320
           mkr$ = str(planflags$(),4,1)
           if optflag% > 0% and mkr$="N" then mkr$ = "L"
           if mkr$ = "T" and j% > 1% then mkr$ = "L"
               if dt$ < "7" then L36230
               if type78% <> 0% then L36230
                  dc$ = "PC" & str(intagnr$(1),3,14)
                  dl$ = str(intagnr$(1),17,3)
L36230:              if dc$=" " then L36320
L36240:                   write #35,using L36290,str(dc$,1,16),           ~
                          str(dl$,1,3),intagnr$(j%),outtagnr$(j%), date, ~
                          time, mkr$, part$(j%), qtyp(j%), qtyu(j%),     ~
                          dt$, dp$, yymmdd$(sd%(j%)), yymmdd$(ed%(j%)),  ~
                          " ", eod goto L36240
L36290:                  FMT CH(16), CH(3), CH(19), CH(19), CH(6), CH(8),~
                              CH(1), CH(25), 2*PD(14,4), 2*CH(1), CH(6), ~
                              CH(6), CH(23)
L36320:    if outtagnr$(j%) = " " then L37180
             if j% > 1% or dt$ > "2" then L37000

               call "READ101" (#33,outtagnr$(1),f1%(33))
                   if f1%(33) = 0% then L36560
               get #33, using L37120, z%, b
               delete #33
               call "PIPFLAGS" (part$(1), 1%, z%, -b, #2, #41)

L36560:     if ed%(1%)=cd% then L37140
            gosub'99(type%(1%),22%,test%)
            if test% = 0% then qtydd = 0  else L36660

            gosub'99(type%(1%),28%,test%)
            if test% = 0% then L37140

L36660:     init (hex(00)) optkey$:str(optkey$,1,19)=outtagnr$(1%)
            qtyrem=0
L36680:     call "PLOWNXT1" (#34, optkey$, 19%, f1%(34))
                if f1%(34)=0 then L36770
            get #34, using L36710, z%, b
L36710:         FMT XX(44), BI(4), XX(8), PD(14,4)
            delete #34
            qtyrem=qtyrem+b
            call "PIPFLAGS" (part$(1), 1%, z%, b, #2, #41)
            goto L36680

L36770:     if qtyrem < .00001 then L37140
            qtydd = max(0,min(qtydd,qtyrem))
            if qtydd < .00001 then L36860
L36800:        write #34, using L37110, outtagnr$(1%), part$(1%), cd%,    ~
                          time, qtydd, eod goto L36800
               call "PIPFLAGS" (part$(1), 1%, cd%, -qtydd, #2, #41)

            qtyrem=max(0, qtyrem-qtydd)

L36860:     if qtyrem < .00001 then L37140
L36870:        write #34, using L37110, outtagnr$(1%), part$(1%), ed%(1%),~
                          time, qtyrem, eod goto L36870
               call "PIPFLAGS" (part$(1%), 1%, ed%(1%), -qtyrem, #2, #41)

               goto L37140

L37000: REM  NORMAL PARTS
           if type%(j%)>489% and type%(j%)<500% then tool_pips
           if type%(j%)>789% and type%(j%)<800% then tool_pips

L37040:      write #34, using  L37110   ,                                 ~
                     outtagnr$(j%),      /* DUE TO COME OUT FOR        */~
                     part$(j%),          /* PART TO BE WITHDRAWN       */~
                     ed%(j%),            /* DAY TO BE WITHDRAWN        */~
                     time,               /* TIME THIS SPEC REC WRITTEN */~
                     qtyu(j%),           /* QUANTITY TO BE WITHDRAWN   */~
                     eod goto L37040
L37110:         FMT CH(19), CH(25), BI(4), CH(8), PD(14,4)
L37120:         FMT XX(25),BI(4),XX(19),PD(14,4)

L37140:       if type%(j%) <> 0% then L37180
             write #36, using L37160, outtagnr$(j%),part$(j%),intagnr$(j%)
L37160:            FMT CH(19), CH(25), CH(19)

L37180: REM WRITE PIPMASTR RECORDS IF THERE IS A NET CHANGE

           temp = 0 : if j% > 1% then temp = qtyu(j%)

               if qtyp(j%) = temp then L37410
                 call "PIPFLAGS"                                         ~
                    (part$(j%),          /* THE PART TO CHECK          */~
                     today%,                                             ~
                     ed%(j%),            /* DAY QUANTITY ADDED         */~
                     qtyp(j%)- temp,     /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */

           goto L37410

        tool_pips
L37340:     write #34, using L37110, outtagnr$(j%), part$(j%), sd%(j%),   ~
                           time, qtyu(j%), eod goto L37340
L37360:     write #34, using L37110, outtagnr$(j%), part$(j%), ed%(j%),   ~
                           time, -qtyu(j%), eod goto L37360

           call "PIPFLAGS" (part$(j%), 1%, sd%(j%), -qtyu(j%), #2, #41)
           call "PIPFLAGS" (part$(j%), 1%, ed%(j%),  qtyu(j%), #2, #41)
L37410:    next j%
           if dt$ > "2" then qtyu(1) = 0

        next_pip_action
           gosub boprint          /* AND PARTS BUILT */

        EJECT
        REM PM ONLY ENTRY, WRITES WORK CENTER DETAILS
           if planflag% < 1% then L38770

        REM NOW BOOK THE WC USEAGE IN THE ARRAY STACKS
           if maxi% <= 0% then L38750

        REM FIRST WRITE THE WCOUT   RECORDS FOR EACH WC/DAY OF USE
           for i%  = 1% to maxi%
               if wc$(i%) = " " then L38510
               if du%(i%) <= 0% or du%(i%) > 490% then L38510
               mat cumf% = zer: f1%(11) = 0%
               au%(i%) = au%(i%) - su%(i%)
               if wc$(i%) = "VEND" then L38170

               call "READ100" (#11, str(wc$(i%),1,4) & " ", f1%(11))
                     if f1%(11) <> 1% then L38240

L38170:        rteseq% = 100%*val(wl$(i%)) + val(wa$(i%))
               write #23 using L38600, wc$(i%), du%(i%), rteseq%,         ~
                                intagnr$(wl%(i%)), du%(i%), rteseq%,     ~
                                su%(i%), au%(i%), ws$(i%), wa%(i%),      ~
                                wa$(i%), wd$(i%), " "
               cumf%(du%(i%)) = cumf%(du%(i%)) + au%(i%) + su%(i%)

L38240:        if i% = maxi% then L38400
                   for j% = i% + 1%  to maxi%
                     if wc$(j%) <> wc$(i%)  then L38380
                     if wc$(j%) = "VEND" then L38290
                     if f1%(11%) = 0% then L38370
L38290:              if du%(j%) < 1% or du%(j%) > 490% then L38370
                     au%(j%) = au%(j%) - su%(j%)
                     rteseq% = 100%*val(wl$(j%)) + val(wa$(j%))
                     write #23 using L38600, wc$(j%), du%(j%), rteseq%,   ~
                                intagnr$(wl%(j%)), du%(j%), rteseq%,     ~
                                su%(j%), au%(j%), ws$(j%), wa%(j%),      ~
                                wa$(j%), wd$(j%), " "
                     cumf%(du%(j%)) = cumf%(du%(j%)) + au%(j%) + su%(j%)
L38370:              wc$(j%) = " "
L38380:            next j%
               if f1%(11) = 0% then L38510
L38400:        call "READ101" (#11, str(wc$(i%),1,4) & " ", f1%(11))
                     if f1%(11) <> 1% then L38510

               get #11 using L38440, pip%()
L38440:            FMT XX(1039), 490*BI(2)

               mat pip% = pip% + cumf%

               put #11 using L38490, pip%():rewrite #11
L38490:            FMT POS(1040), 490*BI(2)

L38510:   next i%

L38600:         FMT  CH(4),              /* WHICH WORK CENTER          */~
                     BI(2),              /* WHICH DAY                  */~
                     BI(2),              /* SEQ/CONC. INDICATOR        */~
                     CH(19),             /* PARENT'S TAG NUMBER        */~
                     BI(2),              /* WHICH DAY                  */~
                     BI(2),              /* SEQ/CONC. INDICATOR        */~
                     BI(4),              /* SET UP UNITS               */~
                     BI(4),              /* RUN UNITS                  */~
                     CH(5),              /* SOFT STEP NUMBER           */~
                     BI(2),              /* ALT RTE STEP SEQ. NUMBER   */~
                     CH(1),              /* CONCURRENCY FLAG           */~
                     CH(4),              /* ACTIVITY                   */~
                     CH(17)              /* FILLER                     */~

L38750:    errormsg$ = "SUCCESSFULLY PLANNED & BOOKED"
             goto L38800
L38770:    errormsg$ = "SUCCESSFULLY CHECKED FOR FEASIBILITY AS SHOWN"
             goto L38800

L38800: REM CONTROLS THE NET UP OR DOWN PROCESS
           finaldate% = ed%(1)
           firstdate% = sd%(maxlines%)
           if planflag% < 1% then return_planned

           if dt$ = "1" then gosub final_net_down
           if dt$ = "4" or dt$ = "5" then gosub final_net_up
           if dt$ <  "7" then return_planned
           if type78% <> 0% then return_planned

           str(dc$,1,16) = str(intagnr$(1),1,16)      /* DT 7, 8, 9    */
           str(dl$,1,3)  = str( intagnr$(1),17,3)
           str(dc$,1,2)  = "PC"
           if dt$        = "9" then str(dc$,1,2)="PM"

           goto return_planned

        EJECT
        bo_optimize
            if str(planflags$(),19,1)="N" then return
            if maxlines% < 2% then return
            if maxlines% >= matarray% then return
                l% = maxlines% + 1%
            for bo% = 2% to maxlines%
                if action%(bo%) <> 2% then L39220
            boqtyu=qtyu(bo%):boqtyp=qtyp(bo%)
            qtyu(bo%), qtyp(bo%) = 0
            qtyu(l%) = boqtyu : qtyp(l%) = 0
            part$(l%)=part$(bo%)
            type%(l%)=type%(bo%)
            moq%(l%) = moq%(bo%)
            ed%(l%)  = ed% (bo%)

            gosub calciw

            qtyu(bo%)=boqtyu:qtyp(bo%)=boqtyp
            if qtyp(l%) > .0001 then L39213
            qtyp(bo%) = 0
            action%(bo%) = 1%
            sd%(bo%) = ed%(bo%)
            goto L39220

L39213:     if qtyp(l%) < qtyp(bo%) then qtyp(bo%) = qtyp(l%)

L39220:     next bo%
            return

        EJECT
        REM *************************************************************~
            *  IMAGE STATEMENTS FOR FULL PRINTED REPORT                 *~
            *************************************************************

L45040: % RUN TIME: ######## @ ########        C A E L U S   M A N A G E ~
        ~M E N T   S Y S T E M S                                  PAGE ###
L45060: %                                     D E T A I L E D   P L A N N~
        ~ I N G   N A R R A T I V E
        %

L45100: %................................................................~
        ~.................................................................
L45120: % CHECKING PLAN REQUIREMENTS FOR VALIDITY -  (DEMAND ############~
        ~####  ###)
L45140: % NETTABLE SALES ALREADY SUFFICIENT TO ABSORB THIS FORECAST
L45150: % NOT ENOUGH ATC: CHECK AVAILABLE ALTERNATES FOR PART ###########~
        ~##############
L45170: % ....... NO ALTERNATES AVAILABLE: TRY TO PROCURE PART ##########~
        ~##############
L45190: % SUBSTITUTING PART #################### FOR ####################
L45200: % ISSUING A TEMPORARY POA FOR ######### PART ####################
L45210: % ISSUING A TEMPORARY IWA FOR ######### PART ####################
L45220: % LOADING COMPONENTS USING BOM ### FOR PART #####################~
        ~####
L45240: %>>>>>>>> ENDING UNPLANNED, DEMAND NOT FEASIBLE <<<<<<<<
L45250: %>>>>>>>>>>>>>>>>>>>> RESTART - WE WANTED THIS DEMAND BY ######  ~
        ~ WE ARE NOW WORKING ON A DELIVERY BY >>>>>>>>>>>>>>>>>>>>> ######
L45270: %   HIGHER LEVEL PARTS PLANNED: NOW ATTEMPT TO OBTAIN BY ######  ~
        ~ ######.# PART #########################
L45290: % CHECKING ATC: DETERMINE QUANT TO PROCURE, FOR PART ############~
        ~############# ON ######
L45310: % CHECK WC CAP, USE THE ### STEPS OF ROUTE ### PART #############~
        ~############

L45340: % MAKE FROM ###### TO ###### (### DAYS) ########.# ##############~
        ~##############
L45360: % WC #### (###/###) USED ON ######, LOOK EARLIER (####### UNITS O~
        ~F ##### REQ'D)
L45375: % WC #### (###/###) ON ###### MUST START BY ###### TO OVERLAP WIT~
        ~H NEXT STEP WC ####

L45390: % PROBLEM: STEP ###/### WC ####: HIT TODAY, DELAY DELIVERY ### DA~
        ~YS AND REPLAN.
L45410: % PROBLEM: NEED BY ######: LT = ### TODAY = ######, ### DAYS SHOR~
        ~T: REPLAN.

L45440: % PART ######################### MARKED AS OPTION, CHECKING FOR  ~
        ~OPTIONS
L45460: % SUBSTITUTED PART #########################

L45480: % * * * WARNING * * * , NEGATIVE PIP FOR PART ###################~
        ~######

L45510: % FINAL ACTUAL PLANNED ORDERS AND INVENTORY COMMITMENTS TO PROCUR~
        ~E ######## PART ######################### (################  ###)
L45530:    %LINE  PLN *ACT PART                      BOM   QTYREQD  QTYPR~
        ~OC  START-END      MOQ TPE  LT IN-TAG-NUMBER       OUT-TAG-NUMBER
L45550:    %#### #### #### ######################### ### -######.# ######~
        ~.# ######-###### ##### ### ### ################### ##############~
        ~#####
L45580: % TOOL CONSTRAINT, CHECKING FOR RESCHEDULE, TOOL ################~
        ~#########
L45600: % TRYING BACKWARDS RESET OF STEP ####, #### DAYS
L45610: % FORCED TO JUMP #### DAYS FORWARD
L45620: % ALLOCATED ########.## UNITS TOOL #########################

L45640:   % ROUTING AND WORK CENTER USEAGE FOR PARTS NEEDED FOR YOUR DEMA~
        ~ND FOR ######### / ######################### (################  #~
        ~##)
L45660: % ROUTE ### FOR ########.## OF PART ######################### FOR~
        ~ LINE ####
L45680: %STEP     WORK CTR   MQ/VEND DAYS   S/U UNITS  PR UNITS/PT  % COM~
        ~PLETE   YIELD  ACTIVITY TO BE PERFORMED
L45700: %#######    ####     -#######         ######   #######.##      ##~
        ~###     #####  #########################################
L45720: %                        STEP      WORK CENTER  DAY         SET-U~
        ~P UNITS   RUN UNITS   ACT.CODE
L45740: %                        #######      ####      ######         ##~
        ~#######   #########   ####
L45760: %                        ----------------------------------------~
        ~--------------------------
L45780: %(MEM)                               P L A N N I N G   P R O C U ~
        ~R E M E N T   S U M M A R Y
L45800: %(MEM)                      P L A N N I N G   C A P A C I T Y   R~
        ~ E S E R V A T I O N   S U M M A R Y

        EJECT
        REM *************************************************************~
            *  THIS SECTION HANDLES PRINTED SUMMARIES, BOTH PARTS & LAB *~
            *************************************************************

        boprint
           page% = 0%
           if printflag% <> 0% then L50080
           if str(planflags$(),2,1) <> "Y" then L51000
L50080:    if maxlines% = 0% then L51000
           line% = 99% : page%, pageb% = 0%

           for i% = 1% to maxlines%

           act$ = "N/PL"
           if action%(i%) = 1% then act$ = "IW  "
           if action%(i%) = 2% then act$ = "PRCH"
           if action%(i%) = 3% then act$ = " BLD"
           if action%(i%) = 4% then act$ = "PWRK"
           gosub L50500

           date_fmtr$ = yymmdd$(sd%(i%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
           date_fmtr$ = yymmdd$(ed%(i%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

           print using L45550, i%, parline%(i%),act$, part$(i%), bom$(i%),~
                               qtyu(i%), qtyp(i%),                        ~
                               str(ccyymmdd$(1%), 3%, 6%),                ~
                               str(ccyymmdd$(2%), 3%, 6%),                ~
                               moq%(i%), type%(i%), (ed%(i%) - sd%(i%)),  ~
                               intagnr$(i%), outtagnr$(i%)
           line%=line%+1%
           next i%
           goto L51000

L50500:    if line% < 50% then return
           if mod(page%,250%) = 0% then close printer
           select printer(134)
           page%, pageb% = page% + 1%: print page
           print using L45040, date$, time$, page%
           print using L45780 : print skip(2)
           print using L45510, dqtyu , part$, dc$, dl$
           print
           print using L45530
           line% = 0%
           return

        EJECT
L51000: REM NOW PRINT PARTS BUILT IF PRINTFLAG IS ON
         if printflag% <> 0% then L51030
         if str(planflags$(),3,1) <> "Y" then return
L51030:  if maxi% <= 0% then  return
            mmaxi% = maxi%
            if page% > 25% then close printer
            line% = 99% : page% = 0%

L51100: REM PROCESS NEXT ASSEMBLY

            if line% > 46% then line% = 99%
            if line% < 47% then gosub L52800

            call"PLANRTE"                /*                            */~
                     (part$(wl%(mmaxi%)),/* PART NEEDED                */~
                      rte$(wl%(mmaxi%)), /* THE WC ROUTE TO USE        */~
                      bom$(wl%(mmaxi%)), /* WHICH BOM TO USE           */~
                      errormsg$,         /* THE RETURN MESSAGE         */~
                      rtestep$(),        /* THE DERIVED ROUTE          */~
                      ed%(wl%(mmaxi%)),  /* EFFECTIVE DATE             */~
                      x%,                /* # OF ELEMENTS              */~
                      #15,               /* BOMMASTR                   */~
                      #7,                /* RTEMASTR                   */~
                      #24)               /* ENGMASTR                   */~

          if x% = 0% then L52000

          for rte% = 1% to x%
              get str(rtestep$(rte%)) using L51510,                       ~
                  lwc$, mq%, su%, run, yld%, step$, palevel%, cmp%,      ~
                  str(activ$,1,4), str(activ$,6), phfact
L51510:       FMT XX(31), CH(4), BI(4), BI(2), PD(14,4), XX(29), BI(1),  ~
                  XX(8), CH(4), BI(1), BI(1), CH(4), CH(30), XX(8),      ~
                  PD(14,4)

          gosub L52500
          if line% = 0% then gosub L52800
          str(step$,5,1) = "-":convert palevel% to str(step$,6,2),pic(00)
          run = run * phfact
          print using L45700,step$,lwc$,mq%,su%,run,cmp%,yld%,activ$
          line% = line% + 1%
          next rte%

L52000:    if line% > 47% then line% = 99%
           if line% < 48% then gosub L52900
           ll% = wl%(mmaxi%)
           lwc$ = " ": lwc1$ = hex(ff) : lau% = -1%

           for i1% = mmaxi% to 1% step -1%
                if wl%(i1%) <> ll% then L52220
                i2% = i1%
L52052:         if i2% = 1% then L52059
                   if wa$(i2%) = hex(00) then L52059
                   if wa$(i2%) = hex(09) then L52059
                   if wa$(i2% - 1%) >= wa$(i2%) then L52059
                   if wa$(i2% - 1%) = hex(00) then L52059
                   if wa$(i2% - 1%) = hex(09) then L52059
                i2% = i2% - 1% : goto  L52052
L52059:         for i% = i2% to i1%
                if i% <> i2% then L52122
                if i1% = mmaxi% then L52100
                if wl$(i1%) = wl$(i1%+1%) then L52122
                   print using L45760
                   line% = line% + 1%
                   lwc$ = " ": lwc1$ = hex(ff) : lau% = -1%
L52100:         str(step$,1,5) = str(ws$(i%),1,4) & "-"
                convert val(str(ws$(i%),5,1)) to str(step$,6,2), pic(00)
                if wc$(i%) = " " then step$ = " "

L52122:         if wc$(i%) <> lwc1$ then lwc$ = wc$(i%)
                   if lau% = 0% then L52126
                      if au%(i%)  = 0% then lwc$ = wc$(i%)
                      goto L52130
L52126:               if au%(i%) <> 0% then lwc$ = wc$(i%)

L52130:         gosub L52500
                if line% = 0% then gosub L52900

                date_fmtr$ = yymmdd$(du%(i%))
                call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))

                print using L45740, step$, lwc$,                 ~
                               str(ccyymmdd$(1%), 3%, 6%),       ~
                               su%(i%), au%(i%) - su%(i%), wd$(i%)
                line% = line% + 1%
                lwc$, step$ = " "
                lwc1$ = wc$(i%) : lau% = au%(i%)
           next i%

           i1% = i2%
           next i1%

           return

L52220:    mmaxi% = i1%
           goto L51100


L52500:     if line% < 50% then return
            if mod(page% + pageb%,250%) = 0% then close printer
            select printer (134)
            page% = page% + 1% : print page
            print using L45040, date$, time$, page%
            print using L45800
            print skip(2) : print using L45640, qtyu, part$, dc$, dl$
            line% = 0%
            return

L52800:     print: print using L45660, rte$(wl%(mmaxi%)),                 ~
                   qtyp(wl%(mmaxi%)), part$(wl%(mmaxi%)), wl%(mmaxi%)
            print using L45680
            line% = line% + 3%
            return


L52900:    print: print using L45720
           line% = line% + 2%
           return

        EJECT
        REM *************************************************************~
            *  THIS SECTION HANDLES FORCASTS; NET UP, DOWN OR CHECKING  *~
            *************************************************************

        final_net_down

           pipadj% = -int(dqtyu)
           if pipadj% = 0% then return
        upd_sfcum

           mat cumf% = zer
L58170:    FMT CH(25), 490*BI(4)

           call "READ101" (#41, part$, f1%(41))
           if f1%(41) = 0 then L58230
           get #41, using L58170, part$, cumf%()

L58230: REM START WITH ED% AND ADJUST FCSTS BY PROPER AMOUNT
                   for i% = ed%(1%) to 490%
                       cumf%(i%) = cumf%(i%) + pipadj%
                   next i%

           put #41, using  L58170,                                        ~
                     part$,              /* PART FORECAST              */~
                     cumf%()             /* CUMULATIVE FCST LEFT       */

           if f1%(41) = 1% then rewrite #41 else write #41
           return

        final_net_up

           pipadj% = int(qtyu)
           if pipadj% = 0% then return

           gosub upd_sfcum

L58570:    FMT CH(25), 490*BI(4)
           mat cumf% = zer

           call "READ101" (#40, part$, f1%(40))
                if f1%(40) <> 1% then L58670

            get  #40, using  L58570   ,                                   ~
                     part$,              /* PART FORECAST              */~
                     cumf%()             /* ACTUAL FCSTS               */

L58670:    cumf%(ed%(1)) = cumf%(ed%(1)) + pipadj%

            put  #40, using  L58570,                                      ~
                     part$,              /* PART FORECAST              */~
                     cumf%()             /* ACTUAL FCSTS               */

           if f1%(40) = 1% then rewrite #40 else write #40
           return

        check_forecast
        REM REGULAR FORECASTS NEED TO BE CHECKED TO SEE IF CUMF IS       ~
           ALREADY NEGATIVE ON ED WHICH MEANS THAT WE ALREADY HAVE       ~
           PLANNED SALES WHICH ARE TO BE NETTED AGAINST THIS FORECAST.   ~
           THIS ALLOWS THE SALE TO BE PLANNED PRIOR TO THE FORECAST      ~
           YET STILL NET PROPERLY.

L59070:    FMT CH(25), 490*BI(4)

           call "READ100" (#41, part$, f1%(41))
           if f1%(41) = 0% then return

           get #41, using  L59070     ,                                   ~
                     part$,              /* PART FORECAST              */~
                     cumf%()             /* CUMULATIVE FCST LEFT       */

        REM START WITH ED% AND NET UP FCSTS BY QTYU IF THIS IS A PLAN
           if cumf%(ed%(1%)) >= 0% then return
             if dt$="4" then L59230
               errormsg$="CUMULATIVE FORECAST IS NEGATIVE, TYPE 5 IS INAP~
        ~PROPRIATE."
                 goto return_unplanned

L59230:        if abs(cumf%(ed%(1%))) >= qtyu(1%) then L59270
               qtyu(1%)=qtyu(1%)+cumf%(ed%(1%))  /* SET QTYU TO NET AMT */
               return                            /* GOT SOMETHING TO DO */

L59270:        if planflag% > 0% then L59310
               errormsg$="FORCAST CAN BE NETTED TO EXISTING DEMANDS"
               goto L59340

L59310:        gosub final_net_up
               errormsg$="SUCESSFULLY NETTED AGAINST DEMANDS"

L59340:   firstdate%, finaldate% = ed%(1%)
          if printflag% <> 0% then print using L45140
          return clear
          goto return_planned

        EJECT
        REM *******  SCREEN SUB-ROUTINES  ******************************

        scrninit     /* SET-UP SCREEN FOR THE FIRST TIME ONLY          */
           if str(planflags$(),1,1) <> "Y" then L60080
            close crt
            call "SCRNADDR" ("O", 1%, 2%, scrn$, 0%, 0%, r%)
            call "SCRNADDR" ("E", 1%, 2%, scrn$, 0%, 0%, r%)

L60080:     init ("-") scrn$
                       str(scrn$, 1, 1) = hex(01)
                       str(scrn$,78, 1) = hex(01)
            gosub'080 (scrn$, 4%, 0%)

            init (" ") scrn$
            scrndate1$ = yymmdd$(cd%)  :  call "DATEFMT" (scrndate1$)
            put scrn$, using scrn00, qtyu, part$, scrndate1$, scrndate1$
            gosub'080 (scrn$, 1%, 1%)

            scrn$ = "CHECKING PLAN SPECIFICATIONS FOR VALIDITY..."
            gosub'080 (scrn$, 3%, 1%)

            return


        def fn'080 (scrn$, scrnrow%, scrnintsy%) /* PUT LINE ON SCREEN */
          if str(planflags$(),1,1) <> "Y" then return
           call "SCRNADDR" ("W", scrnrow%, 2%, scrn$, 78%, scrnintsy%,r%)
           gosub checkinterupt
           return


        screenplot   /* PLOT PLAN FOR PART ON SCREEN         */
            init (" ") scrn$

            scrnaction$ = "*"
                if action%(l% -1%) = 1% then scrnaction$ = hex(03)
                if action%(l% -1%) = 2% then scrnaction$ = "*"
                if action%(l% -1%) = 3% then scrnaction$ = hex(0b)
                if action%(l% -1%) = 4% then scrnaction$ = "#"

           scrne% = min(78%, 78% - int((ed%(1%) - ed%(l%-1%))*scrnscale))
                if scrne% > 78% then scrne% = 78%
           scrns% = max( 1%, 78% - int((ed%(1%) - sd%(l%-1%))*scrnscale))
                if scrns% > scrne% then scrns% = scrne%
            scrnl% = 1% + scrne% - scrns%
            if scrns% + scrnl% > 79% then scrnl% = min(1%, 79%-scrns%)
            init (scrnaction$) str(scrn$, scrns%, scrnl%)
                convert l% - 1% to str(scrn$,1,4), pic (#000)

            if scrnline% < 23% then L60740
           if str(planflags$(),1,1) <> "Y" then L60700
                call "SCRNADDR" ("E", 5%, 2%, scrn$, 1%, 1%, r%)
L60700:         gosub'080 (scrn23$, 23%, 1%)
                gosub'080 (scrn24$, 24%, 1%)
                scrnline% = 5%

L60740:     gosub'080(scrn$, scrnline%, 0%)
                scrnline% = scrnline% + 1%
            return

        screenloop   /* START/RESTART SCREEN FOR NEW PART    */
            if l%  <> 1% then L61250

            scrnscale  = 78 / (ed%(1%)-today% + 1%)

            scrndate1$ = yymmdd$(cd%)     : call "DATEFMT" (scrndate1$)
            scrndate2$ = yymmdd$(ed%(1%)) : call "DATEFMT" (scrndate2$)
            init (" ") scrn$
            put scrn$, using scrn00, qtyu(1%), part$(1%), scrndate1$,    ~
                                                          scrndate2$
            gosub'080 (scrn$, 1%, 1%)

           init ("-") scrn$ : str(scrn$,,1) = "T" : str(scrn$,78,1) = "C"
            scrne% = min(78%, 78%- int((ed%(1%) - cd%) * scrnscale))
            if scrne% < 1% then scrne% = 1%
            str(scrn$,scrne%,1) = "D"
            gosub'080 (scrn$, 4%, 0%)

           if str(planflags$(),1,1) <> "Y" then L61210
                call "SCRNADDR" ("E", 5%, 2%, scrn$, 1%, 1%, r%)
L61210:         gosub'080  (scrn23$, 23%, 1%)
                gosub'080  (scrn24$, 24%, 1%)
                scrnline% = 5%

L61250:     if l% > 1% then gosub screenplot      /* PUT OUT LAST LINE */

            scrndate1$ = yymmdd$(ed%(l%)) : call "DATEFMT" (scrndate1$)
            init(" ") scrn$
            gosub'080 (scrn$, 3%, 0%)
            put scrn$, using scrn01, l% , qtyp(l%), part$(l%)
            scrn$ = scrn$ & " BY " & scrndate1$
            gosub'080 (scrn$, 2%, 1%)

            return

        scrnsuccess
            init (" ") scrn$
            gosub'080 (scrn$, 2%, 1%)
            gosub'080 (scrn$, 3%, 1%)
            gosub'080 (scrn$,23%, 1%)
            gosub'080 (scrn$,24%, 1%)
            scrn$ = "       ****  PLANNED SUCCESSFULLY  ****"
            gosub'080 (scrn$, 2%, 1%)
            if planflag% < 1% then return
            scrn$ = "        ---     SAVING DETAILS     ---"
            gosub'080 (scrn$, 3%, 1%)
            return

        scrnwcplot   /* PLOT PLAN FOR WC ON SCREEN         */
            init (" ") scrn$

            scrnaction$ = hex(0b)
            if pjflag$ <> " " then scrnaction$ = "#"

            scrne% = min(78%, 78% - int((ed%(1%) - ed%(l%))*scrnscale))
                if scrne% > 78% then scrne% = 78%
            scrns% = max( 1%, 78% - int((ed%(1%) - sd%(l%))*scrnscale))
                if scrns% > scrne% then scrns% = scrne%
            scrnl% = 1% + scrne% - scrns%
            if scrns% + scrnl% > 79% then scrnl% = min(1%, 79%-scrns%)
            init (scrnaction$) str(scrn$, scrns%, scrnl%)
                convert l%  to str(scrn$,1,4), pic (#000)

            if scrnline% < 23% then L62210
           if str(planflags$(),1,1) <> "Y" then L62170
                call "SCRNADDR" ("E", 5%, 2%, scrn$, 1%, 1%, r%)
L62170:         gosub'080 (scrn23$, 23%, 1%)
                gosub'080 (scrn24$, 24%, 1%)
                scrnline% = 5%

L62210:     gosub'080 (scrn$, scrnline%, 0%)

            return


        checkinterupt     /* SEE IF ABORT KEY HIT  */
            if r% <>  127% then return
            errormsg$ = "RUN ABORTED BY OPERATOR"
            goto return_unplanned


        scrn00:%######## PART ######################### BY ########.  NOW~
        ~ PLANNING BY ########
        scrn01:%CURRENT: LINE ####. ######## PART #######################~
        ~##
        scrn10:%PROBLEM PROCURING LINE ####:######## ####################~
        ~#####
        scrn11:%PROBLEM BUILDING  LINE ####:######## ####################~
        ~#####
        scrn12:%  REASON: STEP ###/### WC ####. DELAY ### DAYS AND REPLAN

L62900: % YOUR PROCUREMENT DEMAND IS NOT FEASIBLE <<<<<<<<<<<<< #
L62910: % YOUR DEMAND CANNOT BE FULFILLED WITHIN THE PLANNING PERIOD #


        EJECT
        REM *************************************************************~
            * WORK FILE FORMATS                                         *~
            *************************************************************

            FMT                /* #62, MATERIALS WORKFILE              */~
                CH(25),        /* PART         AK1                     */~
                BI(4) ,        /* REV LINE     AK1                     */~
                BI(1) ,        /* ACTION           AK3                 */~
                BI(4) ,        /* PARLINE          AK3     PK          */~
                BI(4) ,        /* LINE             AK3 AK2 PK          */~
                BI(4) ,        /* START DT                             */~
                BI(4) ,        /* END DT                               */~
                BI(4) ,        /* LEAD TIME                            */~
                BI(4) ,        /* MOQ                                  */~
                BI(4) ,        /* TYPE                                 */~
                CH(3) ,        /* BOM                                  */~
                CH(3) ,        /* RTE                                  */~
                BI(1) ,        /* ACTION (ACTUAL)                      */~
                PD(14,4),      /* QUANTITY REQUIRED                    */~
                PD(14,4),      /* QUANTITY TO PROCURE                  */~
                CH(19),        /* IN TAG NUMBER                        */~
                CH(19)         /* OUT TAG NUMBER                       */~

            FMT                /* #63, WORK CENTER WORKFILE            */~
                CH(4) ,        /* WORK CENTER   AK1                    */~
                BI(4) ,        /* REV PARLINE   AK1   PK               */~
                BI(1) ,        /* STEP          AK1   PK               */~
                BI(4) ,        /* DATE USED     AK1   PK               */~
                BI(1) ,        /* CONCURRENCY   AK1   PK               */~
                BI(4) ,        /* UNITS USED                           */~
                BI(4) ,        /* SET UP                               */~
                BI(4) ,        /* ALT SEQ NR                           */~
                CH(5) ,        /* SOFT STEP NUMBER                     */~
                CH(35)         /* FILLER/TAG NUMBER                    */~

        EJECT
        REM *************************************************************~
            * ENTRY TO WORKFILE VERSION IF ALLOWED                      *~
            * TIME FOR A COFFEE BREAK IF YOU ARE HERE                   *~
            *************************************************************

        workfile

               if l% = 1% then return_unplanned
               if dt$ = "9" then return_unplanned
               if str(planflags$(),21,1) <> "Y" then return_unplanned
               return clear all

               minjump% = 0%:workfile% = 1%
               close printer

            f2%=1%: call "WORKOPEN" (#62, "IO   ", 3000%, f2%)
            f2%=1%: call "WORKOPEN" (#63, "IO   ", 5000%, f2%)
            optkey$ = optkeyhold$

            if startmaxlines% = 0% then L64300
            for i% = 1 to startmaxlines%

            write #62, using L64260, part$(i%), 10000%-i%, action%(i%),   ~
                       parline%(i%), i%, sd%(i%), ed%(i%), lt%(i%),      ~
                       moq%(i%), type%(i%), bom$(i%), rte$(i%),          ~
                       action%(i%), qtyu(i%), qtyp(i%), " ", " "
L64260:          FMT CH(25), BI(4), BI(1), 7*BI(4), 2*CH(3), BI(1),      ~
                     2*PD(14,4), 2*CH(19)
            next i%

L64300:     if startmaxi% = 0% then L64410

            for i% = 1 to startmaxi%

            write #63, using L64370, wc$(i%), 10000%- wl%(i%),  wl$(i%),  ~
               du%(i%), wa$(i%), au%(i%), su%(i%), wa%(i%), ws$(i%),     ~
               wd$(i%), " "

L64370:           FMT CH(4), BI(4), CH(1), BI(4), CH(1),                 ~
                      BI(4), BI(4), BI(4), CH(5), CH(4), CH(31)
            next i%

L64410: call "PLANWORK" (dc$,            /* DEMAND CODE                */~
                     dl$,                /* DEMAND LINE                */~
                     dt$,                /* DEMAND TYPE                */~
                     dp$,                /* DEMAND PRIORITY            */~
                     part$,              /* PART NEEDED                */~
                     dqtyu,              /* QUANTITY NEEDED            */~
                     cd%,                /* REQ'D COMPL DATE           */~
                     dw$,                /* DELIVER TO WAREHOUSE       */~
                     rte$,               /* THE WC ROUTE TO USE        */~
                     bom$,               /* WHICH BOM TO USE           */~
                     errormsg$,          /* THE RETURN MESSAGE         */~
                     firstdate%,         /* FIRST DATE PASSED BACK     */~
                     finaldate%,         /* DATE PLANNED FOR           */~
                     todaya%,            /* SUBSCRIPT FOR TODAY'S DATE */~
                     planflag%,          /* 1 = PLAN, 0 = CHECK ONLY   */~
                     todaye%,            /* SUBSCRIPT FOR TODAY'S DATE */~
                     todayc%,            /* SUBSCRIPT FOR TODAY'S DATE */~
                     type78%,            /* 7/8 SPECIAL FLAG           */~
                     optflag%,           /*                            */~
                     qtydd,              /*                            */~
                     testdemd$,          /*                            */~
                     optkey$,                                            ~
                     l%,                                                 ~
                     scrnline%,                                          ~
                     minjump%,                                           ~
                     pltbase%,                                           ~
                     startmaxlines%,                                     ~
                     scrnscale,                                          ~
                     #16,                /* HNYALTRS                   */~
                     #15,                /* BOMMASTR                   */~
                     #2,                 /* PIPMASTR                   */~
                     #7,                 /* RTEMASTR                   */~
                     #8,                 /* JBCROSS2                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT   WC-CROSSREFERENCE  */~
                     #33,                /* PIPIN  PARTS DUE INTO PIP  */~
                     #34,                /* PIPOUT PARTS DUE OUT OF PIP*/~
                     #40,                /* SFMASTR2  SALES FORECASTS  */~
                     #41,                /* SFCUM2  CUMULATIVE FCSTS   */~
                     #35,                /* PIPCROSS                   */~
                     #24,                /* ENGMASTR                   */~
                     #14,                /* BOMSPEC                    */~
                     #36,                /* JBPIPXRF                   */~
                     #6,                 /* RTEALTRS                   */~
                     #62,                /* WORK FILES                 */~
                     #63)

            call "FILEBGON" (#62)
            call "FILEBGON" (#63)

            if finaldate% > 490% then gosub return_unplanned
            if minjump% > 0% then L64970
            maxlines% = 2%
            if planflag% < 1% then L38770
            goto L38750

L64970:     l% = 2%
            goto jump
