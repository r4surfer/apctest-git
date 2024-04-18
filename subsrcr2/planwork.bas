        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   L       AAA   N   N  W   W   OOO   RRRR   K   K   *~
            *  P   P  L      A   A  NN  N  W   W  O   O  R   R  K  K    *~
            *  PPPP   L      AAAAA  N N N  W   W  O   O  RRRR   KKK     *~
            *  P      L      A   A  N  NN  W W W  O   O  R R    K  K    *~
            *  P      LLLLL  A   A  N   N   W W    OOO   R  R   K   K   *~
            *     LINKED WORKFILE VERSION                               *~
            *-----------------------------------------------------------*~
            * PLANWORK - THIS IS THE MAIN CMS2 PLANNING SUBORUTINE.     *~
            *            PLAN OR CHECK FEASABILITY.  WITH OR WITHOUT    *~
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
            *      0 = CHECK FEASABILITY, THIS QUANTITY ONLY, NO REPORT *~
            *     10 = CHECK FEASABILITY, THIS QUANTITY ONLY, PRINT REPT*~
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
            *       'LOAD_COMPS' - WHEN ALL STEPS ARE FEASABLE THEN     *~
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
            * 03/04/93 ! Formax bug. Errorneous assumption that   ! KAB *~
            *          !   top level part actually used capacity. !     *~
            *          !   BOOK_ORDER (35170).                    !     *~
            * 06/22/93 ! Try to do IW if Phantoms available.      ! KAB *~
            *          ! Minor Bug Fix. Make Sure PH's BOM exists !     *~
            * 08/19/93 ! Purchase Job Support - BW's              ! KAB *~
            * 12/31/93 ! EOD's on PIPOUT writes.  Problem with    ! KAB *~
            *          !    Move Queue if Run < 2 units           !     *~
            * 07/04/94 ! Activity Code to WCOUTs.                 ! KAB *~
            * 12/13/94 ! Better BO optimize, eliminate or decrease! KAB *~
            * 04/15/96 ! Re-arrange logic for tag assignment.     ! KAB *~
            *          !  (about 35200 - 26300). Fast machines    !     *~
            *          !  sill beating 1/100 sec.                 !     *~
            * 08/30/96 ! Millie date conversion                   ! DER *~
            * 07/18/97 ! Tag No back to YYMMDD format             ! DER *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        EJECT
        sub "PLANWORK" (dc$,             /* DEMAND CODE                */~
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
                     maxlines%,                                          ~
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
           ccyymmdd$(2)8,                /* print date fmt yymmdd      */~
           date_fmtr$8,                  /* date format place holder   */~
           optkey$100,                                                   ~
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
           compplowkey$31,               /* PLOW BOMMASTR              */~
                                                                         ~
                                                                         ~
           mkr$2,                        /* BOM MARKER FOR PHANTOMS    */~
           holdlwc$4,                                                    ~
           scrn$78,                      /* SCREEN GRAPHICS            */~
           scrn23$78,                    /* SCREEN GRAPHICS            */~
           scrn24$78,                    /* SCREEN GRAPHICS            */~
           scrnaction$1,                 /*                            */~
           scrndate1$8,                  /*                            */~
                                         /*                            */~
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
           wd$4,                         /* WORK CENTER ACTIVITY (S)   */~
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

        dim matkey$4,matkey1$29,matkey2$8,matkey3$9,wckey$10,wckey1$14,  ~
            wwc$4

        EJECT
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************
           if str(planflags$(),1,1)<>" " then L09100
              errormsg$="PLANNING OPTIONS RECORD INCORRECT"
              gosub return_unplanned

L09100:    get str(planflags$()) using L09130, toolcons%, capmq%,          ~
                       jumpmin%, jumpmax%, jumppur%, jumpmfg%, purscan%, ~
                       mfgscan%
L09130:        FMT POS(6), 2*BI(1), 4*BI(2), POS(207), BI(2),            ~
                   POS(425), BI(2)
            capmq=capmq%:capmq=round(capmq/100,2)



        REM LWCARRAY% = DIM(RTESTEP$(),1)
            workfile% = 1%               /* JUST FOR PLANCIW */
            nodisp%   = 0%


            date$, time$ = " "
            date$ = date : call "DATEFMT" (date$) : call "TIME" (time$)
            page%, got_vend%, top_iw% = 0%

            tagdttmp$ = date
            call "DATEFMT" ( tagdttmp$, 0%, tagdtfull$ )
            tagdate$ = str( tagdtfull$, 3%, 6% )


        EJECT

            matkey$ = bin(l%-1%,4)
            goto loop_check


        page_head
            page% = page% + 1%
            print page : print using L45040, date$, time$, page%
            print using L45060
            print : print using L45100 : print
            print using L45120, dc$, dl$  :  print
            line% = 7%
            return

        EJECT
L12000: REM  ************************************************************~
           *     ALL LOOP CHECKING STARTS HERE. L% IS THE CURRENT LINE  *~
           *  BEING EXAMINED.  THAT LINE IS EITHER DEALT WITH OR        *~
           *  WE MUST JUMP.  JUMP WILL RESTART HERE WITH L% = 1 AGAIN.  *~
           **************************************************************









           if printflag% = 0% then L12180
             if line% > 57% then gosub page_head
             date_fmtr$ = yymmdd$(ed%(2%))
             call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))

             print:print using L45270, str(ccyymmdd$(1%), 3%, 6%), ~
                                       qtyu(2%),part$(2%)
             line% = line% + 2%
L12180:    offset% = 0%:today% = todayc%
           if type%(2%)>489% and type%(2%)<500% then tool_screen
           if type%(2%)>789% and type%(2%)<800% then tool_screen
           if action%(2%) = 0% then L12220
              qtyp(2%) = 0 : goto L12230

L12220:    gosub calciw      /* CALCIW ALLOCATES AS MUCH AS POSSIBLE TO */
L12230:    altflag% = 0%     /* THE DEMAND. THE REMAINDER IS PLANNED.   */
                             /* ALSO CHECKS FOR PART IN PIP, ETC        */

           if abs(qtyp(2%)) > .00001 then L12510


        atc_ok
                action%(2%) = 1%              /* 1 = IW           */
                sd%(2%)     = ed%(2%)
                gosub update_matfile
                if printflag% = 0% then loop_check
                    if line% > 58% then gosub page_head
                    print using L45210, qtyu (2%), part$(2%)
                    line% = line% + 1%

        loop_check

            call "PLOWALTS" (#62, matkey$, 2%, 0%, f1%(62))
                if f1%(62) = 0 then book_order
            get #62, using L12460, part$(2%),                             ~
                       parline%(2%), l%, sd%(2%), ed%(2%), lt%(2%),      ~
                       moq%(2%), type%(2%), bom$(2%), rte$(2%),          ~
                       action%(2%), qtyu(2%), qtyp(2%)
L12460:          FMT CH(25), XX(5), 7*BI(4), 2*CH(3), BI(1),             ~
                     2*PD(14,4)
            gosub screenpart
                     goto L12000

L12510:    if l% = 1% then L12580  /* TARGET FOR ALT PART JUMP */

           gosub'99(type%(2%),64%,test%)
           if test% = 0% then L12580

           gosub alt_part_sub
L12580:    altflag% = 1%
           if type%(2%) < 500% and type%(2%) > 0%  then pur_part

        REM ** THESE ARE ALL MANUFACTURED PARTS **

           gosub mfgdate
           if sd%(2%) <= today% then alt_part_sub
        mfg_ok
                     action%(2%) = 3%     /* 3 = MANUFACTURE            */
                     if pjflag$ <> " " then  action%(2%) = 4%  /* BW */
                     gosub update_matfile
                     gosub load_comps
           goto loop_check

        pur_part
           if today% + lt%(2%) > ed%(2%) then alt_part_sub
        pur_ok
                     action%(2%) = 2%               /* 2 = PURCH        */
                     sd%(2%)     = ed%(2%) - lt%(2%)
                     gosub update_matfile
           if printflag% = 0% then loop_check
               if line% > 58% then gosub page_head
               print using L45200, qtyp(2%), part$(2%)
               line% = line% + 1%
           goto loop_check

        EJECT
        alt_part_sub
          if altflag% = 0% then L13500
          if type%(2%) > 499% or type%(2%) = 0%  then L13050
                minjump% =  today% + lt%(2%) + jumppur%  - ed%(2%)
                goto L13060
L13050:   minjump% =  jumpmfg% * x% /* X% = # OF STEPS LEFT IN MGF SEQ */
L13060:   minjump% = max(minjump%, jumpmin%)
          minjump% = min(minjump%, 491%-ed%(1%), cd%+jumpmax%+1%-ed%(1%))

          gosub'059(1%)   /* Can we do IW on Top level part by Now ?  */
          if top_iw% = 1% then jump

             gosub'059(2%)                  /* GO FISH FOR COMPONENT */

L13500: REM CHECK FOR ALTERNATE PARTS
           if l% > 1% then L13540
              if altflag%=0% then return
              goto jump
L13540:    altpart$=part$(2%)
           alttype%=type%(2%)
           altmoq% =moq% (2%)
           altlt%  =lt%  (2%)
           altqtyp =qtyp (2%)
           altbom$ =bom$ (2%)
           altrte$ =rte$ (2%)
           bom$(2%), rte$(2%) = " "

           gosub L14010

           part$(2%)=altpart$
           type%(2%)=alttype%
           moq% (2%)=altmoq%
           lt%  (2%)=altlt%
           qtyp (2%)=altqtyp
           bom$ (2%)=altbom$
           rte$ (2%)=altrte$
           return

        EJECT
        REM ALTERNATE PART TEST LOOP
L14010:    if type%(2%)=0% then L14090
           if printflag% = 0% then L14060
               if line% > 58% then gosub page_head
               print using L45150, part$(2%)
               line% = line% + 1%
L14060:    plowkey1$ = str(part$(2%),1,25) & " "
L14070:    call "PLOWNEXT" (#16, plowkey1$, 25%, f1%(16))
                if f1%(16%) = 1% then L14160
L14090:            if altflag% = 0% then return
                   return clear all
                   if printflag% = 0% then jump
                   if line% > 58% then gosub page_head
                   print using L45170, altpart$
                   line% = line% + 1%
                   goto jump
L14160:    get #16, using L14170, part$(2%)
L14170:              FMT XX(33), CH(25)

           call"READ100"(#2, part$(2%), f1%(2))
                if f1%(2%) <> 1% then L14070
           get #2, using L14220, moq, type%(2%), lt%(2%)
L14220:         FMT POS(2003), PD(14,4), XX(8), 2*BI(2)

           moq%(2%) = moq
           if type%(2%) < 200% then L14070
           gosub'65(2%)

           gosub calciw
           if abs(qtyp(2%)) > .00001 then L14340

           action%(2%)=1%
           goto sub_alt_part

L14340:    if altflag% = 0% then L14070

           gosub'99(alttype%,70%,test%)
           if test% = 0% then L14070

           if type%(2%) > 499 then L14490

        REM ** CHECK PURCHASED ALTERNATE **
           if today% + lt%(2%) > ed%(2%) then L14070

           action%(2%) = 2%               /* 2 = PURCHASE              */
           goto sub_alt_part


L14490: REM ** THESE ARE ALL MANUFACTURED PARTS **

           gosub mfgdate

           if sd%(2%) <= today% then L14070
              action%(2%) = 3%            /* 3 = MANUFACTURE           */
              if pjflag$ <> " " then  action%(2%) = 4%  /* BW */
           goto sub_alt_part

        sub_alt_part

           scrn$ = "ALTERNATE PART " & part$(2%) & " SUBSTITUTED."
           gosub'080 (scrn$, 3%, 1%)

           if printflag% = 0% then L14660
               if line% > 58% then gosub page_head
               print using L45190, part$(2%), altpart$
               line% = line% + 1%
L14660:    return clear all
           on action%(2%) goto atc_ok, pur_ok, mfg_ok, mfg_ok

        update_matfile
            matkey$ = bin(l%,4)
            call "REDALT1" (#62, matkey$, 2%, f1%(62))
            if f1%(62) <> 0 then L14750
               errormsg$ = "COMPONENTS WORKFILE ERROR"
               gosub return_unplanned
L14750:     rewrite #62, using L14790, part$(2%), 10000%-l%, action%(2%), ~
                       parline%(2%), l%, sd%(2%), ed%(2%), lt%(2%),      ~
                       moq%(2%), type%(2%), bom$(2%), rte$(2%),          ~
                       action%(2%), qtyu(2%), qtyp(2%), " ", " "
L14790:          FMT CH(25), BI(4), BI(1), 7*BI(4), 2*CH(3), BI(1),      ~
                     2*PD(14,4), 2*CH(19)
            if nodisp% <> 0% then return
L14810:     l% = l% + 1% : gosub screenloop : l% = l% - 1%
            return

        tool_screen
            gosub L14810
            goto loop_check

        EJECT
        REM *************************************************************~
            *                                                           *~
            * END OF LOOP CHECKING, EXIT FROM LOOP CHECK IS EITHER      *~
            * BOOK ORDER OR JUMP.  NEXT COME COMMON EXIT ROUTINES       *~
            *                                                           *~
            *************************************************************

        return_unplanned
           return clear all

           if str(planflags$(),20,1) = "Y" then gosub boprint

           finaldate% = 999%
           goto L15540

        return_planned

           finaldate% = ed%(1%)
           minjump% = 0%
L15540:    close printer


           end

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
               errormsg$="COMPONENT NOT MARKED AS TOOL " & part$(3%)
               goto return_unplanned

L16080:    parline%(3%) = l%
           action%(3%) = 1%
           qtyu (3%) = max(0, qtyp(2%)*phfact(ti%) * (qu*tu + fu))
           if qtyu(3%) > 0 then qtyu(3%)=max(1,round(qtyu(3%),0))
           qtyu(3%)=round(qtyu(3%) + addon, 0)
           if qtyu(3%) < .0001 then L19500
           qtyp (3%) = 0
        REM *** TOOL REQUIREMENT DATES SET HERE **************

           pbs% = min(loi%, max(pbs%, offset%+1%))

              sd%(3%) = pd%(pbs%)
              ed%(3%)=min(490%, max(xsd%(pbs%), sd%(3%))+1%)

           if mod(type%(3%),10%) >= toolcons% then L16320
L16230:       maxlines% = maxlines% + 1%
           if printflag% = 0% then L16280
              if line% > 58% then gosub page_head
              print using L45620, qtyu(3%), part$(3%)
              line% = line% + 1%
L16280:    init (" ") scrn$:put scrn$ using L45620, qtyu(3%), part$(3%)
           gosub'080 (scrn$, 3%, 1%)
              goto L21560

L16320: REM * * * RATS, TOOL MARKED AS CONSTRAINING, CHECK PIP * * *
           scrn$ = "CHECKING TOOL FOR CONSTRAINT " & part$(3%)
           gosub'080 (scrn$, 3%, 1%)

           altpart$=part$(3%)
           alttype%=type%(3%)
           altmoq% =moq% (3%)
           altlt%  =lt%  (3%)
           plowkey1$ = str(part$(3%),1,25) & " "

            gosub L18160

L16440:     for tl%=sd%(3%) to ed%(3%)-1%
                if pip%(tl%)<qtyu(3%) then L16490
                next tl%
                goto L16230

L16490:    gosub L18000
                if f1%(16%) <> 0% then L16440

        REM CONSTRAINED, NOW WHAT???; TRY BACKWARDS JUMP

           holdsd% = sd%(3%)
           holdrq% = ed%(3%)-sd%(3%)-1%
           holdxsd% = ed%(3%)-1%
           maxlines%=startmaxlines%
              matkey2$ = bin(l%,4) & hex(00000000)
              call "DELETE" (#62, matkey2$, 4%)

           if holdsd%-1% < today% then L17150

           if printflag% = 0% then L16680
               if line% > 58% then gosub page_head
               print using L45580, altpart$
               line% = line% + 1%

L16680:    init (" ") scrn$:put scrn$ using L45580, altpart$
           gosub'080 (scrn$, 3%, 1%)

           if str(planflags$(),16,1) <> "Y" then L16730
           for tl%=holdsd%-1% to today% step -1%
L16730:        part$(3%)=altpart$
               type%(3%)=alttype%
               moq% (3%)=altmoq%
               lt%  (3%)=altlt%
               plowkey1$ = str(part$(3%),1,25) & " "

            gosub L18160

L16810:    if str(planflags$(),16,1) = "Y" then L16830
           for tl%=holdsd%-1% to today% step -1%
L16830:              for j%=tl% to tl%+holdrq%
                         if pip%(j%) < qtyu(3%) then L16870
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
                 sd%(2%)=xsd%(pbs%)

           if printflag% = 0% then L17040
               if line% > 58% then gosub page_head
               print using L45600, pbs%, tl%-holdsd%
               line% = line% + 1%
L17040:    init (" ") scrn$:put scrn$ using L45600, pbs%, tl%-holdsd%
           gosub'080 (scrn$, 3%, 1%)

              nextpass = 1
                 gosub L25500  /* INTO CENTER OF MFGDATE LOGIC */
                 if today% < sd%(2%) then load_comps
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

L17250:        part$(3%)=altpart$
               type%(3%)=alttype%
               moq% (3%)=altmoq%
               lt%  (3%)=altlt%
               plowkey1$ = str(part$(3%),1,25) & " "

           gosub L18160

L17330:    if str(planflags$(),17,1) = "Y" then L17350
            for tl%=holdsd% to holdxsd%
L17350:              for j%=tl% to tl%+holdrq%
                         if pip%(j%) < qtyu(3%) then L17390
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
                 top_iw% = -1%
                 goto jump

L18000: REM COMMON PLOW AND LOAD ROUTINE

L18020:    call "PLOWNEXT" (#16, plowkey1$, 25%, f1%(16))
                if f1%(16%) = 0% then return

           get #16, using L14170, part$(3%)
                     FMT XX(33), CH(25)
           gosub'64 (3%)
             if type%(3%)>489% and type%(3%)<500% then L18120
             if type%(3%)>789% and type%(3%)<800% then L18120
           goto L18020

L18120:    if mod(type%(3%),10%) >= toolcons% then L18160
              return clear
              goto L16230

L18160: REM COMMON LOAD PIP ROUTINE
            call "READ100" (#2, part$(3%), f1%(2))
                if f1%(2) <> 0 then L18210
                 errormsg$="TOOL NOT FOUND IN PIP" & part$(3%)
                 goto return_unplanned
L18210:     get #2, using L18220, pip%()
L18220:         FMT XX(26), 490*BI(4)

        REM AND (HO HUM) ADJUST FOR USE IN THIS PLAN BUFFER

            matkey1$=str(part$(3%),1,25) & bin(10000% - m%, 4)
L18270:     call "PLOWALTS" (#62, matkey1$, 1%, 25%, f1%(62))
            if f1%(62) = 0 then return
                get #62, using L18300, sdt%, edt%, temp
L18300:            FMT XX(25), XX(13), 2*BI(4), XX(19), PD(14,4)

                   for j% = sdt% to edt%-1%
                       pip%(j%)=pip%(j%)-temp
                   next j%

                goto L18270

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
           scrn$ = "LOADING PARTS FROM BOM " & bom$(2%)
           gosub'080 (scrn$, 3%, 1%)

           if printflag% = 0% then L19220
               if line% > 58% then gosub page_head
               print using L45220, bom$(2%),part$(2%)
               line% = line% + 1%

L19220:    compplowkey$ = str(part$(2%),1,25) & str(bom$(2%),1,3) & "  0"

L19500:    m% = maxlines% + 1%

           call "PLOWNEXT" (#15, compplowkey$, 28%, f1%(15))
                if f1%(15%) =  0% then return



           bom$(3%),rte$(3%)=" "
           get #15, using L19600, part$(3%), qu, tu, addon, fu, mkr$,     ~
                    optmkr$, bom$(3%), pbs$, type$
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
               print using L45440, part$(3%)
               line% = line% + 1%

L20070:    call "PLOWALTS"(#14,optkey$,1%,19%,f1%(14))
                if f1%(14%) <> 0% then L20110
                errormsg$ = "OPTION NOT FOUND FOR PART " & part$(3%)
                     goto return_unplanned
L20110:    get #14, using L20120, testbom$, part$(3%), qu, tu,            ~
                                 bom$(3%), optmkr$
L20120:         FMT XX(25),CH(31),XX(19),XX(4),CH(25), 2*PD(14,4),       ~
                    CH(3), CH(1)
           if str(testbom$,1,31) = str(compplowkey$,1,31) then L20160
                errormsg$ = "INCORRECT OPTION ENCOUNTERED" & testbom$
                     goto return_unplanned
L20160:    if part$(3%) = " " then L19500
           if optmkr$  <> " " then L19500
*              BOM$(3%)=" "
           scrn$ = "OPTION PART " & part$(3%) & " SUBSTITUTED."
           gosub'080 (scrn$, 3%, 1%)
           if printflag% = 0% then L20250
               if line% > 58% then gosub page_head
               print using L45460, part$(3%)
               line% = line% + 1%

L20250: rem************ end of option processing ************************

L20500:    gosub'64(3%)
           if type%(3%)>489% and type%(3%)<500% then tool_check
           if type%(3%)>789% and type%(3%)<800% then tool_check
           if mkr$<>"TL" then L20570
              errormsg$="COMPONENT MARKED AS TOOL " & part$(3%)
              goto return_unplanned

L20570:    parline%(3%) = l% : action%(3%) = 0%

        REM *** SPLIT PICKING DATES ARE SET HERE **************

           pbs% = min(loi%,max(pbs%,offset%+1%))
           sd%(3%), ed%(3%) = max(pd%(pbs%), sd%(2%))

           qtyu (3%) = qtyp(2%)*phfact(ti%) * (qu*tu + fu)
           qtyu (3%) = 100 * qtyu(3%) / yld(pbs%)
           qtyu (3%) = qtyu(3%) + addon
           qtyp (3%) = 0
           if qtyu(3%) > 0 then L20740
           if qtyu(3%) = 0 then L19500
           gosub'99(type%(3%),118%,test%)
           if test% = 0% then L19500

L20740: REM TEST FORCE INTEGER
           gosub'99(type%(3%),58%,test%)
           if test% = 0% then L20820
             qtyu(3%)=sgn(qtyu(3%))*abs(int(-qtyu(3%)))
        REM  QTYU(3%)=INT(QTYU(3%)+SGN(QTYU(3%))*.99999)
               if qtyu(3%) = 0 then L19500

L20820: REM RESUME
           gosub'65(3%)
           if mkr$ <> "UU" then L20900
                if type%(3%)=0% or type%(3%)>489% then type%(3%)=489%
                lt%(3%)=999%
                moq%(3%)=0%
*              GOTO 21550

L20900: REM PRESCAN LOGIC
           if mkr$ = "NP" then L21100
           gosub'99(type%(3%),142%,test%)
           if test% = 0% then L21500

           call "PLANCIW"                                                ~
                      (dt$,              /* DEMAND TYPE                */~
                       testdemd$,        /* SWITCH MASK                */~
                       errormsg$,        /* THE RETURN MESSAGE         */~
                       qtydd,            /* QTY ON DELIVERY DAY        */~
                       cd%,              /* COMPLETION DAY OFFSET      */~
                       m%,               /* MATERIALS ARRAY LOCATION   */~
                       todaya%,          /* ACTUAL TODAY               */~
                       workfile%,        /* WORKFILE STATUS = 1        */~
                       atcf%,            /* WARNING FLAG               */~
                       1%,               /* PRESCAN, TRY HERE          */~
                       #2,               /* PIPMASTR                   */~
                       #41,              /* SFCUM2  CUMULATIVE FCSTS   */~
                       #62)              /* MATERIALS WORKFILE         */~

           if qtyp(3%) >= 0 then L21090
L21080:       qtyp(3%)  = 0 : goto L21500
L21090:    if qtyp(3%)  > 0 then L21110
L21100:       action%(3%) = 1% : goto L21550
L21110:    if qtyp(3%) >= qtyu(3%) then L21080

           if type%(3%) > 199% and type%(3%) < 500% then L21080
           if mkr$  = "PH" then L21162
           if mkr$ <> "PA" then L21080

L21162:    gosub'99(type%(3%),148%,test%)
           if test% = 0% then L21080

           write #62, using L21600, part$(3%), 10000%-m%, 1%,             ~
                      parline%(3%), m%, sd%(3%), ed%(3%), lt%(3%),       ~
                      moq%(3%), type%(3%), bom$(3%), rte$(3%),           ~
                      1%, qtyu(3%) - qtyp(3%), 0, " ", " "

           qtyu    (3%) = qtyp(3%)
           qtyp    (3%) = 0
           action% (3%) = 0%
           maxlines% = maxlines% + 1%
           m% = m% + 1%
           goto L22020

L21500:    if type%(3%) > 199% and type%(3%) < 500% then L21550

           if mkr$ = "PH" then L22020
           if mkr$ = "PA" then L22020

L21550:    maxlines% = maxlines% + 1%
L21560:       write #62, using L21600, part$(3%), 10000%-m%, action%(3%), ~
                    parline%(3%), m%, sd%(3%), ed%(3%), lt%(3%),         ~
                    moq%(3%), type%(3%), bom$(3%), rte$(3%), action%(3%),~
                    qtyu(3%), qtyp(3%), " ", " "
L21600:          FMT CH(25), BI(4), BI(1), 7*BI(4), 2*CH(3), BI(1),      ~
                     2*PD(14,4), 2*CH(19)
           goto L19500

        REM * * * PHANTOM LOOP LOGIC * * *

L22020:      if ti% < 101% then L22050
             errormsg$="TOO MANY PHANTOMS FOR THIS ASSEMBLY " & part$(2%)
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
             phfact(ti%)= qtyu(3%) / qtyp(2%)
                  if bom$(3%) <> " " then L22230
                     testbom$ = str(part$(3%),1,25) & "1" & hex(000000)
                         call "PLOWNEXT" (#24, testbom$, 26%, f1%(24))
                         if f1%(24%) = 1% then L22200
                                   errormsg$ = "PART: " & part$(3%) &    ~
                                               " HAS NO EFFECTIVE BOM."
                                   goto return_unplanned
L22200:                   get #24, using L22210, eff$()
L22210:                       FMT XX(29), 490 * CH(3)
                     bom$(3%) = eff$(ed%(3%))
L22230:      compplowkey$ = str(part$(3%),1,25) &                        ~
                                            str(bom$(3%),1,3) & "  0"
             call "READ100" (#15, compplowkey$, f1%(15))
                if f1%(15%) <> 0% then L22250
                     errormsg$ = "PART: " & part$(3%) & " BOM: " &       ~
                                 bom$(3%) & " NOT FOUND."
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
           finaldate% = 0%
           if top_iw% = -1% then gosub'059(1%)
           end


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
               date_fmtr$ = yymmdd$(ed%(2%))
               call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))

               print using L45290, part$(2%), str(ccyymmdd$(1%), 3%, 6%)
               line% = line% + 1%

L24140:    call "PLANCIW"                                                ~
                      (dt$,              /* DEMAND TYPE                */~
                       testdemd$,        /* SWITCH MASK                */~
                       errormsg$,        /* THE RETURN MESSAGE         */~
                       qtydd,            /* QTY ON DELIVERY DAY        */~
                       cd%,              /* COMPLETION DAY OFFSET      */~
                       l%,               /* MATERIALS ARRAY LOCATION   */~
                       todaya%,          /* ACTUAL TODAY               */~
                       workfile%,        /* WORKFILE STATUS = 1        */~
                       atcf%,            /* WARNING FLAG               */~
                       0%,               /* NO PRESCAN, REAL THING     */~
                       #2,               /* PIPMASTR                   */~
                       #41,              /* SFCUM2  CUMULATIVE FCSTS   */~
                       #62)              /* MATERIALS WORKFILE         */~

            if errormsg$ <> " " then gosub return_unplanned

            if atcf% = 0% then return
            if printflag% = 0% then return
                if line% > 58% then gosub page_head
                print using L45480, part$(2%)
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

           sd%(2%) = ed%(2%):init (" ") step$()
           if bom$(2%) <> " " then getroute             /* BOM SUPPLIED*/

*        If Approvals are required, MFG components must be hard pegged
           if str(planflags$(),206,1) <> "Y" or l% = 1% then L25220
                 errormsg$ = "APPROVALS REQUIRED, PART: " & part$(2%)   &~
                                                 " MUST BE HARD PEGGED."
                 gosub return_unplanned

L25220:    testbom$ = str(part$(2%),1,25) & "1" & hex(000000)
           call "PLOWNEXT" (#24,testbom$,26%,f1%(24))
                if f1%(24%) = 1% then L25280
                     errormsg$ = "PART: " & part$(2%) &                  ~
                                  " HAS NO EFFECTIVE BOM."
                     goto return_unplanned
L25280:         get #24, using L25290, eff$()
L25290:              FMT XX(29), 490 * CH(3)
                bom$(2%) = eff$(ed%(2%))

         getroute  /* OBTAIN ROUTE-ID FROM BOM     */
           compplowkey$ = str(part$(2%),1,25) & str(bom$(2%),1,3) & "  0"
           call "READ100" (#15, compplowkey$, f1%(15))
                if f1%(15%) =  1% then L25390
                     errormsg$ = "PART: " & part$(2%) & " BOM: " &       ~
                                 bom$(2%) & " NOT FOUND."
                goto return_unplanned
L25390:    get #15, using L25395, rte$(2%), approval$, pjflag$
L25395:         FMT XX(86), CH(3), POS(115), CH(6), POS(145), CH(1)
           if pjflag$ <> "Y" then L25399
           gosub'99(type%(2%),154%,test%)
             if test% = 1% then L25402
L25399:         pjflag$ = " "

L25402
*        If approvals are required, the BOM must be approved to plan.
           if str(planflags$(),206,1)<>"Y" or approval$<>" " then L25440
                 errormsg$ = "PART: " & part$(2%) & " BOM: " &           ~
                                 bom$(2%) & " NOT APPROVED"
                 gosub return_unplanned

         REM ** END OBTAINING BOM & ASSOCIATED ROUTE ***

L25440:    x%,xx%,loi% = 0%:startmaxi% = maxi%:startmaxlines% = maxlines%

           scrn$ = "ANALYZING WORK CENTER ROUTING: ROUTE " & rte$(2%) &  ~
                   ", BOM " & bom$(2%)
           gosub'080 (scrn$, 3%, 1%)

L25500: REM * * * START OF ROUTE LOADING LOOP * * *
         if pjflag$ <> " " then L25820     /* Use Lead time for PJobs */
         if rte$(2%) = " " then L25750
            gosub'99(type%(2%),112%,test%)
            if test% = 1% then L25820

            call"PLANRTE"                /*                            */~
                     (part$(2%),         /* PART NEEDED                */~
                      rte$(2%),          /* THE WC ROUTE TO USE        */~
                      bom$(2%),          /* WHICH BOM TO USE           */~
                      errormsg$,         /* THE RETURN MESSAGE         */~
                      rtestep$(),        /* THE DERIVED ROUTE          */~
                      ed%(2%),           /* EFFECTIVE DATE             */~
                      x%,                /* # OF ELEMENTS              */~
                      #15,               /* BOMMASTR                   */~
                      #7,                /* RTEMASTR                   */~
                      #24)               /* ENGMASTR                   */~

            if errormsg$ <> " " then return_unplanned

        rem** this code is only for the printed report ******************
          if xx% > 0% then L26000
          loi% = x%
          if loi% >0% then L25900
          if rte$(2%)<>" " then L25780
L25750:      gosub'99(type%(2%),94%,test%)
             if test% = 1% then L25820
L25780:         errormsg$="ROUTE HAS NO STEPS, " & str(part$(2%),,25) &  ~
                                                        rte$(2%)
                goto return_unplanned

L25820:     loi%=1%
            put str(rtestep$(1%)), using L25860, " ", lt%(2%), 0%, 0, " ",~
                    1, " ", 1, " ", 1, 1, " ", 100%, 100, " ", 0%, " ",  ~
                    1, " ", 0, 100, 0, 100, " "
L25860:         FMT  CH(35), BI(4), BI(2), PD(14,4), CH(4), PD(7,4),     ~
                     CH(4), PD(7,4), CH(4), PD(7,4), PD(7,6), CH(1),     ~
                     BI(1), PD(14,7), CH(4), BI(1), CH(43), PD(14,4),    ~
                     CH(1), 4*PD(14,4), CH(16)
L25900:   if printflag% = 0% then L26000  /* IF NO REPORT, BRANCH AROUND */
              if line% > 58% then gosub page_head
              print using L45310, loi%, rte$(2%), part$(2%)
              line% = line% + 1%

L26000: REM *** END OF REPORT ONLY CODE, ALLOCATION CONTROL CODE ***

           if xx% = 0% then xx% = loi%
           overlap = 0
           nextpass = 1 : savesdl% = 999%:mat cumuse = zer
           cumuse   = 0 : xxsd%(xx%), xsd%(xx%), useday%  = sd%(2%)
           for x% = xx% to offset% + 1% step -1% /* WORK BACKWARD */
                     gosub L33000
                     mat stpuse = zer
                     mmx%(x%) = maxi%
                     ovrlp_restart% = 0%
                     savesdl% = min(sd%(2%), savesdl%)
                     if requnits% >= 0% then L26086
                        xsd%(x%) = xxsd%(x%)
                        overlap  = 0
L26086:              if overlap <= 0 then L26100
                        nextpass = max(0, 1 - cumuse(xsd%(x%)))
                        nextpass = min(1, nextpass + overlap)
L26100:              sd%(2%)  = xsd%(x%):overlap = 0
                     if x% > 1% then xxsd%(x%-1%), xsd%(x%-1%) = sd%(2%)
                     if mqf% = 0% then L26150
                        xxsd%(x%), xsd%(x%) = min(sd%(2%)+ mqf%, ed%(2%))
                        sd%(2%)  = xsd%(x%)
                        cumuse   = 0:mat cumuse = zer
                        useday%  = sd%(2%)
                        nextpass = 1
L26150:              pd%(x%)  = sd%(2%)-1%
                     if mq% + tr% > 0 then L26200
                        if x% = xx% then L26360
                        pd%(x%) = pd%(x%+1%)
                        goto L26360
L26200:              if pd%(x%) <= today% then L26380
                     init (hex(00)) testbom$
                     str(testbom$,1,31) = str(rtestep$(x%),,31)
                     holdnextpass=nextpass
                     holdtr% = tr%:holdsu% = su%
                     wcalters% = 0%:edt% = sd%(2%)

                     wckey$=bin(10000%-l%,4) & bin(x%,1)
                        call "DELETE" (#63, wckey$, 5%)
                     if lwc$ = "VEND" then L26310
                     if lwc$ <> " "   then L26340
L26310:                 gosub alloc_vend_time
                        goto L26360

L26340:                 gosub alloc_wc_time

L26360:              if pd%(x%) <= today% then L26380
                     if sd%(2%) >= today% then L26420
L26380:                        maxi%=startmaxi%
                               sd%(2%)=min(pd%(x%), sd%(2%))
                                  call "DELETE" (#63, wckey$, 4%)
                               return
L26420:              tr% = holdtr%:su% = holdsu%
                     mat cumuse = cumuse + stpuse
                     gosub scrnwcplot
           next x%                  /* FOR EACH WC STEP           */

           sd%(2%) = min(savesdl%, sd%(2%))
           if sd%(2%)<=today% then sd%(2%)=today%+1%
           sd%(2%)=min(pd%(offset% + 1%), sd%(2%))

           date_fmtr$ = yymmdd$(sd%(2%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
           date_fmtr$ = yymmdd$(ed%(2%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

           init(" ") scrn$
           put scrn$ using L45340, str(ccyymmdd$(1%), 3%, 6%),  ~
                                   str(ccyymmdd$(2%), 3%, 6%),  ~
                                ed%(2%)+1% -sd%(2%), qtyp(2%), part$(2%)
           gosub'080 (scrn$, 3%, 1%)

           date_fmtr$ = yymmdd$(sd%(2%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
           date_fmtr$ = yymmdd$(ed%(2%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

           if printflag% = 0% then return
                if line% > 58% then gosub page_head
                print using L45340, str(ccyymmdd$(1%), 3%, 6%),  ~
                                    str(ccyymmdd$(2%), 3%, 6%),  ~
                                 ed%(2%)+1% -sd%(2%), qtyp(2%), part$(2%)
                line% = line% + 1%
                return


        EJECT
        REM * * * VENDOR STEP, ALLOCATE TIME ONLY * * *
        alloc_vend_time

                gosub get_vend
                if nextpass = 1 then L27140
                        sd%(2%)=sd%(2%)-1%:pd%(x%)=pd%(x%)-1%
                        if pd%(x%) > today% then L27130
                        gosub rte_alters
                        return
L27130:         nextpass = 1
L27140:         qwwq% = mq%+tr%
                if pd%(x%) - qwwq% + 1% > today% then L27220
                     pd%(x%)=pd%(x%)-qwwq%+1%:sd%(2%)=sd%(2%)-qwwq%+1%
                          gosub rte_alters
                          return



L27220:              if awcu3%(sd%(2%)) > 0% then L27270
                        sd%(2%)=sd%(2%)-1%:pd%(x%)=pd%(x%)-1%
                        if pd%(x%) > today% then L27220
                        gosub rte_alters
                        return
L27270:              if qwwq% <= 0% then return
                     maxi%      = maxi% + 1%
                     wa%        = 0%
                     if str(testbom$,32,3) = hex(000000) then L27320
                     convert str(testbom$,32,3) to wa%, data goto L27320
L27320:              write #63, using L27360, "VEND", 10000%-l%, x%,      ~
                                sd%(2%), 0%, 0%, 0%, wa%,                ~
                                str(rtestep$(x%),88,5), wcpact$, " "

L27360:                   FMT CH(4), BI(4), BI(1), BI(4), BI(1),         ~
                              BI(4), BI(4), BI(4), CH(5), CH(4), CH(31)


                     pd%(x%), sd%(2%)  = sd%(2%) - 1%
                     if x% > 1% then xxsd%(x%-1%), xsd%(x%-1%) = sd%(2%)
                     if pd%(x%) > today% then L27440
                        gosub rte_alters
                        return
L27440:         qwwq% = qwwq% - 1%
                if qwwq% > 0% then L27220
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

          gosub'99(type%(2%),88%,test%)
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
            init (hex(00)) wckey1$:str(wckey1$,1,4)=str(lwc$,1,4)
            call "REDALT0" (#63, wckey1$, 1%, f1%(63))
                if f1%(63) = 0 then L28520
L28450:         get #63, using L28460, wwc$, sdt%, temp%
L28460:             FMT CH(4), XX(5), BI(4), XX(1), BI(4)
                if wwc$ <> lwc$ then L28520
                pip%(sdt%)=pip%(sdt%) + temp%
                read #63, eod goto L28520
                goto L28450

L28520:     if lwc1$ = " " then L29000
            init (hex(00)) wckey1$:str(wckey1$,1,4)=str(lwc1$,1,4)
            call "REDALT0" (#63, wckey1$, 1%, f1%(63))
                if f1%(63) = 0 then L28630
L28560:         get #63, using L28570, wwc$, sdt%, temp%
L28570:             FMT CH(4), XX(5), BI(4), XX(1), BI(4)
                if wwc$ <> lwc1$ then L28630
                awcu1%(sdt%)=awcu1%(sdt%) - temp%
                read #63, eod goto L28630
                goto L28560

L28630:     if lwc2$ = " " then L29000
            init (hex(00)) wckey1$:str(wckey1$,1,4)=str(lwc2$,1,4)
            call "REDALT0" (#63, wckey1$, 1%, f1%(63))
                if f1%(63) = 0 then L28740
L28670:         get #63, using L28680, wwc$, sdt%, temp%
L28680:             FMT CH(4), XX(5), BI(4), XX(1), BI(4)
                if wwc$ <> lwc2$ then L28740
                awcu2%(sdt%)=awcu2%(sdt%) - temp%
                read #63, eod goto L28740
                goto L28670

L28740:     if lwc3$ = " " then L29000
            init (hex(00)) wckey1$:str(wckey1$,1,4)=str(lwc3$,1,4)
            call "REDALT0" (#63, wckey1$, 1%, f1%(63))
                if f1%(63) = 0 then L29000
L28780:         get #63, using L28790, wwc$, sdt%, temp%
L28790:             FMT CH(4), XX(5), BI(4), XX(1), BI(4)
                if wwc$ <> lwc3$ then L29000
                awcu3%(sdt%)=awcu3%(sdt%) - temp%
                read #63, eod goto L29000
                goto L28780

        EJECT
L29000: REM * * * START OF ACTUAL WORK CENTER TIME ALLOCATION LOOP * * *

                if mqp$ <> "Y" then L29490
                if mq%  <   1% then L29490
                if nextpass = 1 then L29130
                        sd%(2%)=sd%(2%)-1%
                        if sd%(2%) > today% then L29130
                        gosub rte_alters
                        return
L29130:         nextpass = 1:aaa% = mq%
L29140:         if aaa% < 1% then L29390
                if cumf%(sd%(2%)) <= 0% then L29330
                if aaa% = mq% then spd% = sd%(2%)
                aaa% = aaa% - 1%

                     wa% = 0%
                     if str(testbom$,32,3) = hex(000000) then L29230
                     convert str(testbom$,32,3) to wa%, data goto L29230

L29230:              write #63, using L29270, lwc$, 10000%-l%, x%,        ~
                                sd%(2%), 9%, 0%, 0%, wa%,                ~
                                str(rtestep$(x%),88,5), " "

L29270:                   FMT CH(4), BI(4), BI(1), BI(4), BI(1),         ~
                              BI(4), BI(4), BI(4), CH(5), CH(35)

                     edt% = sd%(2%)


L29330:         sd%(2%) = sd%(2%) - 1%
                if sd%(2%) >= today% then L29140
                     gosub rte_alters
                     pd%(x%)=sd%(2%)
                     return

L29390:         if tr% > 0% then L29460
                pd%(x%) = spd% : aaa% = 0%
                if x% > 1% then xxsd%(x%-1%), xsd%(x%-1%) = sd%(2%)
                if pd%(x%) > today% then L30670
L29430:            gosub rte_alters
                   return

L29460:         pd%(x%) = sd%(2%) - 1%
                if pd%(x%) <= today% then L29430

L29490:         if cumf%(sd%(2%)) <= 0 then L30610
                if awca%(sd%(2%)) <= 0 then wcrestart

                aaa%, asu% = 0%

L29540:         if requnits% <= 0% or x% = xx% then L29670
                if requnits% > tr% and sd%(2%) > pd%(x%+1%) then L29580
                goto L29670
L29580:            ovrlp_restart% = 1%
                   goto wcrestart

L29670:         if tr% < 1% then L30670

                pip%(sd%(2%))   = max(pip%(sd%(2%))  , 0%)
                awcu1%(sd%(2%)) = max(awcu1%(sd%(2%)), 0%)
                awcu2%(sd%(2%)) = max(awcu2%(sd%(2%)), 0%)
                awcu3%(sd%(2%)) = max(awcu3%(sd%(2%)), 0%)

                aaa = min(cumf%(sd%(2%)) - pip%(sd%(2%)),                ~
                        cumf%(sd%(2%))*shift, cumf%(sd%(2%))*nextpass)
                aaa = min(aaa, awcu1%(sd%(2%))/nm1)
                aaa = min(aaa, awcu2%(sd%(2%))/nm2)
                aaa = min(aaa, awcu3%(sd%(2%))/nm3)

                aaa% = max(0%, round(aaa,0))
                if aaa% < 1% then wcrestart
                aaa% = min(tr%, aaa%)
                if x% = 1% then L29880
                if padunits% < 0% then L29860
                if holdtr% - tr% > padunits% then L29870
L29860:            xsd%(x%-1%)  = sd%(2%)
L29870:            xxsd%(x%-1%) = sd%(2%)
L29880:         tr%  = tr% - aaa%
                aaa = aaa%
                aaa = 100*aaa/cumf%(sd%(2%))
                stpuse(sd%(2%)) = stpuse(sd%(2%)) + (aaa/100)
                if tr% >= su% then L29960
                asu% = max(0%, su% - tr%)
                su%  = max(0%, su% - asu%)

L29960:
                wa% = 0%
                if str(testbom$,32,3) = hex(000000) then L30010
                convert str(testbom$,32,3) to wa%, data goto L30010

L30010:              wd$ = wcpact$
                     if aaa% - asu% <= 0% then wd$ = wcsact$
                     write #63, using L30050, lwc$, 10000%-l%, x%,        ~
                                sd%(2%), 1%, aaa%, asu%, wa%,            ~
                                str(rtestep$(x%),88,5), wd$, " "

L30050:                   FMT CH(4), BI(4), BI(1), BI(4), BI(1),         ~
                              BI(4), BI(4), BI(4), CH(5), CH(4), CH(31)

                     edt% = sd%(2%)
                     pip%(sd%(2%))=pip%(sd%(2%)) + aaa%


                if lwc1$ = " " then L30550

                     temp%  = round(aaa%*nm1, 0)
                     temps% = round(asu%*nm1, 0)

                     wd$ = wc1act$
                     if temp% - temps% <= 0% then wd$ =" "
                     write #63, using L30200, lwc1$, 10000%-l%, x%,       ~
                                sd%(2%), 2%, temp%, temps%, wa%,         ~
                                str(rtestep$(x%),88,5), wd$, " "
L30200:                   FMT CH(4), BI(4), BI(1), BI(4), BI(1),         ~
                              BI(4), BI(4), BI(4), CH(5), CH(4), CH(31)

                     awcu1%(sd%(2%)) = awcu1%(sd%(2%)) - temp%


                if lwc2$ = " " then L30550

                     temp%  = round(aaa%*nm2, 0)
                     temps% = round(asu%*nm2, 0)

                     wd$ = wc2act$
                     if temp% - temps% <= 0% then wd$ =" "
                     write #63, using L30340, lwc2$, 10000%-l%, x%,       ~
                                sd%(2%), 3%, temp%, temps%, wa%,         ~
                                str(rtestep$(x%),88,5), wd$, " "
L30340:                   FMT CH(4), BI(4), BI(1), BI(4), BI(1),         ~
                              BI(4), BI(4), BI(4), CH(5), CH(4), CH(31)

                     awcu2%(sd%(2%)) = awcu2%(sd%(2%)) - temp%


                if lwc3$ = " " then L30550

                     temp%  = round(aaa%*nm3, 0)
                     temps% = round(asu%*nm3, 0)

                     wd$ = wc3act$
                     if temp% - temps% <= 0% then wd$ =" "
                     write #63, using L30480, lwc3$, 10000%-l%, x%,       ~
                                sd%(2%), 4%, temp%, temps%, wa%,         ~
                                str(rtestep$(x%),88,5), wd$, " "
L30480:                   FMT CH(4), BI(4), BI(1), BI(4), BI(1),         ~
                              BI(4), BI(4), BI(4), CH(5), CH(4), CH(31)

                     awcu3%(sd%(2%)) = awcu3%(sd%(2%)) - temp%



L30550:         if padunits% < 0% then L30600
                if overlap <> 0 then L30600
                   overlap = max(0, holdtr% - (tr% + padunits%))
                   overlap = 100*overlap/cumf%(sd%(2%))
                   overlap = overlap/100
L30600:         if tr% < 1% then L29540
L30610:            sd%(2%) = sd%(2%)-1% : pd%(x%) = pd%(x%)-1%
                   nextpass = 1
                     if pd%(x%) > today% then L29490
                        gosub rte_alters
                        return

L30670: REM ALLOCATE WORK CENTER MQ TIME IF ANY, SET FACTOR FOR NEXT STEP

L30690:         if cumf%(pd%(x%)) > 0% then L30740
                   pd%(x%) = pd%(x%) - 1%
                   if pd%(x%) > today% then L30690
                      gosub rte_alters
                      return
L30740:         if holdtr% >= 1% then L30780
                   nextpass = 1
                   return
L30780:         aaa = aaa%
                aaa = 100*aaa/cumf%(edt%)
                aaa = aaa/100
                if edt% <> useday%       then cumuse = 0
                cumuse = aaa + cumuse
                aaa% = min(cumuse + (1 - capmq), 1)
                useday% = edt%
                if mqp$ = "Y" then L30825
                if mq% > 0% then aaa% = 1% + mq%
L30825:         if aaa% > 0% then L30840
                   nextpass = 1 - cumuse
                   return
L30840:         cumuse = 0
                nextpass = 1

L30855:         if aaa% < 1% then return
                if cumf%(sd%(2%)) <= 0% then L30955
                aaa% = aaa% - 1%

                if sd%(2%) = edt% then L30955


                     wa% = 0%
                     if str(testbom$,32,3) = hex(000000) then L30915
                     convert str(testbom$,32,3) to wa%, data goto L30915


L30915:              write #63, using L30935, lwc$, 10000%-l%, x%,        ~
                                sd%(2%), 0%, 0%, 0%, wa%,                ~
                                str(rtestep$(x%),88,5), " "

L30935:                   FMT CH(4), BI(4), BI(1), BI(4), BI(1),         ~
                              BI(4), BI(4), BI(4), CH(5), CH(35)


L30955:         sd%(2%) = sd%(2%) - 1%
                if x% <= 1% then L30980
                if padunits% > 0% then L30975
                   xsd%(x%-1%)  = sd%(2%)
L30975:            xxsd%(x%-1%) = sd%(2%)
L30980:         if sd%(2%) >= today% then L30855
                gosub rte_alters
                return

        wcrestart
         if printflag% = 0% then L31180
           if line% > 58% then gosub page_head

           date_fmtr$ = yymmdd$(sd%(2%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
           date_fmtr$ = yymmdd$(pd%(x%+1%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

           if ovrlp_restart% = 0% then                              ~
                 print using L45360, lwc$, x%, loi%,                ~
                                     str(ccyymmdd$(1%), 3%, 6%),    ~
                                     tr%, holdtr%                   ~
           else  print using L45375, lwc$, x%, loi%,                ~
                                     str(ccyymmdd$(1%), 3%, 6%),    ~
                                     str(ccyymmdd$(2%), 3%, 6%),    ~
                                     str(rtestep$(x%+1%), 32, 4)  ~

                 line% = line% + 1%







L31180:    gosub'99(type%(2%),100%,test%)
           if test% = 0% then L31230

           gosub rte_alters
             if wcalters% <> 0% then L31500
L31230:         if x% = xx% then L31270
                   if ovrlp_restart% = 1% then L31500
                   if str(rtestep$(x%),144,1) <> "Y" then L31270
                   if sd%(2%) <= pd%(x%+1%) then L31500
L31270:    nextpass = 1 : mat stpuse = zer : overlap = 0
           tr% = holdtr% : su% = holdsu%
           maxi% = mmx%(x%)
              call "DELETE" (#63, wckey$, 5%)
           sd%(2%) = sd%(2%) - 1%
           pd%(x%) = sd%(2%) - 1%
             if pd%(x%) > today% then L29000
                gosub rte_alters
                return

L31500: REM * * * RELOAD ORIGINAL ROUTE STEP(S) AND GO * * *

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
          str(wckey$,5,1) = bin(i%,1)
          call "DELETE" (#63, wckey$, 5%)
          next i%

          x% = nx%:str(wckey$,5,1) = bin(x%,1)

           holdnextpass, nextpass = 1
           wcalters% = 0%
L31625:    xsd%(x%) = xsd%(x%) - 1%
           sd%(2%) = xsd%(x%)
           if ovrlp_restart% <> 1% then xxsd%(x%) = xsd%(x%)
           pd%(x%) = sd%(2%)-1%
        if cumf%(sd%(2%)) <= 0% then L31625   /* Weekend or Holiday?    */
        if pd%(x%) > today% then gosub  L32220
        return

        REM * * *  LOAD ALTERNATE WORK CENTER STEP * * *
        rte_alters

            call "PLOWNEXT" (#6, testbom$, 31%, f1%(6))
            if f1%(6) = 0% then return

            wcalters% = 1%
            holdlwc$ = lwc$
            nextpass = holdnextpass
            pd%(x%) = xsd%(x%)-1%
            sd%(2%) = xsd%(x%)
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

            call "DELETE" (#63, wckey$, 5%)
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

           tr = max(0, su% + ((100*phfact*run*qtyp(2%))/yld(x%)))
           if lwc$  = "VEND" then L33250
           if lwc$ <> " "    then L33280
L33250:         tr% = round(tr/8,0)
                requnits% = 0%
             if reqqty >= qtyp(2%) or reqpct >= 100 then requnits% = -1
                return

L33280:        if tr > 0 then tr = max(tr, 1)
               tr% = round(tr,0)
            gosub'99(type%(2%),124%,test%)
            if test% = 0% then L33350
               mq% = 0%

L33350:     padunits% = -1%
            if tr%-su% <= 0% then L33440
            if padpct >= 100 then L33440
            if padqty >= qtyp(2%) then L33440
               padunits1  = max(0, (100*phfact*run*qtyp(2%))/yld(x%))
               padunits1% = (padunits1 * padpct)/100
               padunits2% = max(0, (100*phfact*run*padqty)/yld(x%))
               padunits%  = min(tr%-su%, max(padunits1%, padunits2%))

L33440:     requnits% = -1%
            if tr%-su% <= 0% then L33530
            if reqpct >= 100 then L33530
            if reqqty >= qtyp(2%) then L33530
               requnits1  = max(0, (100*phfact*run*qtyp(2%))/yld(x%))
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

            errormsg$="WORK CENTER NOT FOUND, " & act$ & " " & part$(2%) ~
                                                       & rte$(2%)
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

           init (hex(00)) wckey$:temp% = -1%
           put str(wckey$,,4) using L35174, 9999%
L35174:        FMT BI(4)
L35176:    call "PLOWNEXT" (#63, wckey$, 4%, f1%(63))
              if f1%(63) = 0% then L35186
           get #63 using L35182, temp%
L35182:       FMT POS(10), BI(4)
           goto L35176
L35186:    if temp% <= 0% then L35220
           init (hex(00)) wckey$:str(wckey$,8,1) = hex(01)
           call "READ101" (#62, wckey$, f1%(62))
              if f1%(62) = 0% then L35220
           get #62 using L35196, temp1%, temp2%
L35196:        FMT POS(39), 2*BI(4)
           if temp% <  temp1% then L35220
           if temp2% <= temp% then L35220
           put #62 using L35196, temp1%, temp%
           rewrite #62

L35220:    gosub bo_optimize
           if planflag% < 1% then next_pip_action
              init (hex(00)) matkey$
L35250: REM CREATE TAG NUMBERS

            call "PLOWAL1" (#62, matkey$, 2%, 0%, f1%(62))
                if f1%(62) = 0 then pm_only_entry
            get #62, using L35340, part$(2%),                             ~
                       parline%(2%), l%, sd%(2%), ed%(2%), lt%(2%),      ~
                       moq%(2%), type%(2%), bom$(2%), rte$(2%),          ~
                       action%(2%), qtyu(2%), qtyp(2%), intagnr$(2%),    ~
                       outtagnr$(2%)
L35340:          FMT CH(25), XX(5), 7*BI(4), 2*CH(3), BI(1),             ~
                     2*PD(14,4), 2*CH(19)
           if l% > 1% then L35400
              if dt$ = "1" then L35368
              if dt$ = "2" then L35368
                 goto L35370
L35368:       outtagnr$(2%) = str(dc$,1,16) & str(dl$,1,3)
L35370:       if action%(2%) > 1% then L35420
              put #62, using L35380, intagnr$(2%), outtagnr$(2%)
L35380:           FMT POS(82), 2*CH(19)
              rewrite #62
              goto L36320

L35400:    if action%(2%) < 2% then L36320
L35420:    if action%(2%) = 2% then str(intagnr$(2%),1,2)  = "BO"
           if action%(2%) = 3% then str(intagnr$(2%),1,2)  = "WO"
           if action%(2%) = 4% then str(intagnr$(2%),1,2)  = "BW"

        REM NOW WRITE PIPIN/PIPOUT
           if intagnr$(2%) = " " then L36320
           convert sd%(2%) to str(intagnr$(2%),3,3), pic(###)
L36020:    str(intagnr$(2%),6,14) = tagdate$ & time
           write #33, using  L36130   ,                                   ~
                     part$(2%),          /* PART                       */~
                     ed%(2%),            /* DATE DUE TO COME IN        */~
                     intagnr$(2%),       /* FROM WHICH JO/PO           */~
                     qtyp (2%),          /* QUANTITY TO COME IN        */~
                     sd%(2%),            /* DAY TO START               */~
                     eod goto L36020
L36130:        FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)

           call "REDALT1" (#62, matkey$, 2%, f1%(62))
           put #62, using L36146, intagnr$(2%), outtagnr$(2%)
L36146:        FMT POS(82), 2*CH(19)
           rewrite #62

           if str(intagnr$(2%),1,2)  = "BW" then L36164
           if str(intagnr$(2%),1,2) <> "WO" then L36200
L36164:    write #8, using L36168, part$(2%), rte$(2%), intagnr$(2%),     ~
                     part$(2%), bom$(2%), intagnr$(2%), eod goto L36180
L36168:        FMT CH(25), CH(3), CH(19), CH(25), CH(3), CH(19)

L36180:     init (hex(00)) matkey2$:str(matkey2$,1,4) = bin(l%,4)
L36181:     call "PLOWNXT1" (#62, matkey2$, 4%, f1%(62))
                if f1%(62) = 0 then L36188
            put #62, using L36184, intagnr$(2%)
L36184:         FMT POS(101), CH(19)
            rewrite #62
            goto L36181

L36188:     init (hex(00)) wckey$:str(wckey$,1,4)=bin(10000%-l%,4)
L36189:     call "PLOWNXT1" (#63, wckey$, 4%, f1%(63))
                if f1%(63) = 0 then L36200
            put #63, using L36192, intagnr$(2%)
L36192:         FMT POS(36), CH(19)
            rewrite #63
            goto L36189

L36200:    if str(planflags$(),4,1) = "P" then L36320
           mkr$ = str(planflags$(),4,1)
           if optflag% > 0% and mkr$="N" then mkr$ = "L"
           if mkr$ = "T" and l% > 1% then mkr$ = "L"
               if dt$ < "7" then L36230
               if type78% <> 0% then L36230
                  dc$ = "PC" & str(intagnr$(1),3,14)
                  dl$ = str(intagnr$(1),17,3)
L36230:              if dc$=" " then L36320
L36240:                   write #35,using L36290,str(dc$,1,16),           ~
                          str(dl$,1,3),intagnr$(2%),outtagnr$(2%), date, ~
                          time, mkr$, part$(2%), qtyp(2%), qtyu(2%),     ~
                          dt$, dp$, yymmdd$(sd%(2%)), yymmdd$(ed%(2%)),  ~
                          " ", eod goto L36240
L36290:                  FMT CH(16), CH(3), CH(19), CH(19), CH(6), CH(8),~
                              CH(1), CH(25), 2*PD(14,4), 2*CH(1), CH(6), ~
                              CH(6), CH(23)
L36320:    if outtagnr$(2%) = " " then L37180
             if l% > 1% or dt$ > "2" then L37000

               call "READ101" (#33,outtagnr$(2%),f1%(33))
                   if f1%(33) = 0% then L36560
               get #33, using L37120, z%, b
               delete #33
               call "PIPFLAGS" (part$(2%), 1%, z%, -b, #2, #41)

L36560:     if ed%(2%)=cd% then L37140
            gosub'99(type%(2%),22%,test%)
            if test% = 0% then qtydd = 0  else L36660

            gosub'99(type%(2%),28%,test%)
            if test% = 0% then L37140

L36660:     init (hex(00)) optkey$:str(optkey$,1,19)=outtagnr$(2%)
            qtyrem=0
L36680:     call "PLOWNXT1" (#34, optkey$, 19%, f1%(34))
                if f1%(34)=0 then L36770
            get #34, using L36710, z%, b
L36710:         FMT XX(44), BI(4), XX(8), PD(14,4)
            delete #34
            qtyrem=qtyrem+b
            call "PIPFLAGS" (part$(2), 1%, z%, b, #2, #41)
            goto L36680

L36770:     if qtyrem < .00001 then L37140
            qtydd = max(0,min(qtydd,qtyrem))
            if qtydd < .00001 then L36860
L36800:        write #34, using L37110, outtagnr$(2%), part$(2%), cd%,    ~
                          time, qtydd, eod goto L36800
               call "PIPFLAGS" (part$(2), 1%, cd%, -qtydd, #2, #41)

            qtyrem=max(0, qtyrem-qtydd)

L36860:     if qtyrem < .00001 then L37140
L36870:        write #34, using L37110, outtagnr$(2%), part$(2%), ed%(2%),~
                          time, qtyrem, eod goto L36870
               call "PIPFLAGS" (part$(2%), 1%, ed%(2%), -qtyrem, #2, #41)

               goto L37140

L37000: REM  NORMAL PARTS
           if type%(2%)>489% and type%(2%)<500% then tool_pips
           if type%(2%)>789% and type%(2%)<800% then tool_pips

L37040:      write #34, using  L37110   ,                                 ~
                     outtagnr$(2%),      /* DUE TO COME OUT FOR        */~
                     part$(2%),          /* PART TO BE WITHDRAWN       */~
                     ed%(2%),            /* DAY TO BE WITHDRAWN        */~
                     time,               /* TIME THIS SPEC REC WRITTEN */~
                     qtyu(2%),           /* QUANTITY TO BE WITHDRAWN   */~
                     eod goto L37040
L37110:         FMT CH(19), CH(25), BI(4), CH(8), PD(14,4)
L37120:         FMT XX(25),BI(4),XX(19),PD(14,4)

L37140:       if type%(2%) <> 0% then L37180
             write #36, using L37160, outtagnr$(2%),part$(2%),intagnr$(2%)
L37160:            FMT CH(19), CH(25), CH(19)

L37180: REM WRITE PIPMASTR RECORDS IF THERE IS A NET CHANGE

           if l% = 1% then qtyu(2%) = 0

               if qtyp(2%) = qtyu(2%) then L35250
                 call "PIPFLAGS"                                         ~
                    (part$(2%),          /* THE PART TO CHECK          */~
                     today%,                                             ~
                     ed%(2%),            /* DAY QUANTITY ADDED         */~
                     qtyp(2%)- qtyu(2%), /* QUANTITY TO ADD            */~
                     #2,                 /* PIPMASTR                   */~
                     #41)                /* SFCUM2                     */

           goto L35250

        tool_pips
L37340:     write #34, using L37110, outtagnr$(2%), part$(2%), sd%(2%),   ~
                           time, qtyu(2%), eod goto L37340
L37360:     write #34, using L37110, outtagnr$(2%), part$(2%), ed%(2%),   ~
                           time, -qtyu(2%), eod goto L37360

           call "PIPFLAGS" (part$(2%), 1%, sd%(2%),-qtyu(2%), #2, #41)
           call "PIPFLAGS" (part$(2%), 1%, ed%(2%), qtyu(2%), #2, #41)

           goto L35250




        EJECT
        pm_only_entry
           init (hex(00)) wckey1$

L38030: REM NOW BOOK THE WC USEAGE IN THE ARRAY STACKS

           mat cumf% = zer:init (hex(ff)) str(wckey1$,5)
           call "REDALT2" (#63, wckey1$, 1%, f1%(63))
               if f1%(63) = 0 then next_pip_action
               get #63, using L38090, wckey1$
L38090:            FMT CH(4)
               goto L38130
L38110:    call "READNEXT" (#63, f1%(63))
               if f1%(63) = 0 then L38260
L38130:    get #63, using L38150, lwc$, sdt%, edt%, temp%, aaa%, asu%,    ~
                                 wa%, pbs$, wd$, intagnr$(2%)
L38150:        FMT CH(4), XX(4), BI(1), BI(4), BI(1), BI(4), BI(4),      ~
                   BI(4), CH(5), CH(4), CH(19)
               if lwc$<>wckey1$ then L38260
               rteseq% = 100%*sdt% + temp%
               aaa% = aaa% - asu%
               write #23 using L38600, lwc$, edt%, rteseq%,               ~
                     intagnr$(2%), edt%, rteseq%, asu%, aaa%, pbs$, wa%, ~
                     temp%, wd$, " "
               cumf%(edt%) = cumf%(edt%) + aaa% + asu%
               goto L38110

L38260:        if wckey1$ = "VEND" then L38030
               call "READ101" (#11, str(wckey1$,1,4) & " ", f1%(11))
                     if f1%(11) <> 1% then L38030

               get #11 using L38310, pip%()
L38310:            FMT XX(1039), 490*BI(2)
               mat pip% = pip% + cumf%

               put #11 using L38350, pip%():rewrite #11
L38350:            FMT POS(1040), 490*BI(2)

          goto L38030

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
                     BI(1),              /* CONCURRENCY FLAG           */~
                     CH(4),              /* ACTIVITY                   */~
                     CH(17)              /* FILLER                     */~

        next_pip_action

           gosub boprint          /* AND PARTS BUILT */
           goto return_planned

        EJECT
        bo_optimize
            if str(planflags$(),19,1)="N" then return

            nodisp%   = 1%
            init (hex(00)) matkey3$
            str(matkey3$,1%,1%) = hex(02)

L39050:     call "PLOWALTS" (#62, matkey3$, 3%, 1%, f1%(62))
                if f1%(62) = 0% then return

            get #62, using L39120, part$(2%),                             ~
                       parline%(2%), l%, sd%(2%), ed%(2%), lt%(2%),      ~
                       moq%(2%), type%(2%), bom$(2%), rte$(2%),          ~
                       action%(2%), qtyu(2%), qtyp(2%)
L39120:          FMT CH(25), XX(5), 7*BI(4), 2*CH(3), BI(1),             ~
                     2*PD(14,4)
            holdqtyp = qtyp(2%) : holdqtyu = qtyu(2%)
            qtyp(2%) = 0 : qtyu(2%) = 0%
            gosub update_matfile

            holdl% = l% : l% = maxlines% + 1%
            qtyu(2%) = holdqtyu

            gosub calciw
            l% = holdl%

            if abs(qtyp(2%)) > .001 then L39203
                action%(2%) = 1%
                qtyp(2%) = 0
                sd%(2) = ed%(2%)
                goto L39210

L39203:     if qtyp(2%) <= holdqtyp then L39210
               qtyp(2%) = holdqtyp

L39210:         gosub update_matfile
                goto L39050


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
        % NETTABLE SALES ALREADY SUFFICIENT TO ABSORB THIS FORECAST
L45150: % NOT ENOUGH ATC: CHECK AVAILABLE ALTERNATES FOR PART ###########~
        ~##############
L45170: % ....... NO ALTERNATES AVAILABLE: TRY TO PROCURE PART ##########~
        ~##############
L45190: % SUBSTITUTING PART #################### FOR ####################
L45200: % ISSUING A TEMPORARY POA FOR ######### PART ####################
L45210: % ISSUING A TEMPORARY IWA FOR ######### PART ####################
L45220: % LOADING COMPONENTS USING BOM ### FOR PART #####################~
        ~####
        %>>>>>>>> ENDING UNPLANNED, DEMAND NOT FEASIBLE <<<<<<<<
        %>>>>>>>>>>>>>>>>>>>> RESTART - WE WANTED THIS DEMAND BY ######  ~
        ~ WE ARE NOW WORKING ON A DELIVERY BY >>>>>>>>>>>>>>>>>>>>> ######
L45270: %   HIGHER LEVEL PARTS PLANNED: NOW ATTEMPT TO OBTAIN BY ######  ~
        ~ ######.# PART #########################
L45290: % CHECKING ATC: DETERMINE QUANT TO PROCURE, FOR PART ############~
        ~############# ON ######
L45310: % CHECK WC CAP, USE THE ### STEPS OF ROUTE ### PART #############~
        ~############

L45340: % MAKE FROM ###### TO ###### (### DAYS) ########.# ##############~
        ~##############
L45360: % WC #### (###/###) USED ON ######, LOOK EARLIER (NEED #### HRS O~
        ~F ##### REQ'D)
L45375: % WC #### (###/###) ON ###### MUST START BY ###### TO OVERLAP WIT~
        ~H NEXT STEP WC ####

        % PROBLEM: STEP ###/### WC ####: HIT TODAY, DELAY DELIVERY ### DA~
        ~YS AND REPLAN.
        % PROBLEM: NEED BY ######: LT = ### TODAY = ######, ### DAYS SHOR~
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
L45780: %(WKF)                               P L A N N I N G   P R O C U ~
        ~R E M E N T   S U M M A R Y
L45800: %(WKF)                      P L A N N I N G   C A P A C I T Y   R~
        ~ E S E R V A T I O N   S U M M A R Y

        EJECT
        REM *************************************************************~
            *  THIS SECTION HANDLES PRINTED SUMMARIES, BOTH PARTS & LAB *~
            *************************************************************

        boprint
           page% = 0%
           if printflag% <> 0% then L50090
           if str(planflags$(),2,1) <> "Y" then L51000

L50090:    line% = 99% : page%, pageb% = 0% : init (hex(00)) matkey$

L50110:     call "PLOWALTS" (#62, matkey$, 2%, 0%, f1%(62))
                if f1%(62) = 0 then L51000
            get #62, using L50180, part$(2%),                             ~
                       parline%(2%), l%, sd%(2%), ed%(2%), lt%(2%),      ~
                       moq%(2%), type%(2%), bom$(2%), rte$(2%),          ~
                       action%(2%), qtyu(2%), qtyp(2%), intagnr$(2%),    ~
                       outtagnr$(2%)
L50180:          FMT CH(25), XX(5), 7*BI(4), 2*CH(3), BI(1),             ~
                     2*PD(14,4), 2*CH(19)
           act$ = "N/PL"
           if action%(2%) = 1% then act$ = "IW  "
           if action%(2%) = 2% then act$ = "PRCH"
           if action%(2%) = 3% then act$ = " BLD"
           if action%(2%) = 4% then act$ = "PWRK"
           gosub L50500

           date_fmtr$ = yymmdd$(sd%(2%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))
           date_fmtr$ = yymmdd$(ed%(2%))
           call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(2%))

           print using L45550, l%, parline%(2%),act$, part$(2%), bom$(2%),~
                               qtyu(2%), qtyp(2%),                       ~
                               str(ccyymmdd$(1%), 3%, 6%),    ~
                               str(ccyymmdd$(2%), 3%, 6%),    ~
                               moq%(2%), type%(2%), (ed%(2%) - sd%(2%)), ~
                               intagnr$(2%), outtagnr$(2%)
           line%=line%+1%

           goto L50110

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
L51030:
            init (hex(00)) wckey$

            line% = 99% : page% = 0%

L51100:     init (hex(ff)) str(wckey$,5)
            call "PLOWNEXT" (#63, wckey$, 0%, f1%(63))
                if f1%(63) = 0 then return
            get #63, using L51140, temp%
L51140:         FMT XX(4), BI(4)
            holdstep% = 9999999%
            matkey$ = bin (10000% - temp%, 4)
            call "REDALT0" (#62, matkey$, 2%, f1%(62))
                if f1%(62) = 0 then L51100
            get #62, using L51210, part$(2%), edt%, bom$(2%), rte$(2%),   ~
                                                                 qtyp(2%)
L51210:      FMT CH(25), XX(17), BI(4), XX(12), 2*CH(3), XX(9), PD(14,4)

            if line% > 46% then line% = 99%
            if line% < 47% then gosub L52800

            call"PLANRTE"                /*                            */~
                     (part$(2%),         /* PART NEEDED                */~
                      rte$(2%),          /* THE WC ROUTE TO USE        */~
                      bom$(2%),          /* WHICH BOM TO USE           */~
                      errormsg$,         /* THE RETURN MESSAGE         */~
                      rtestep$(),        /* THE DERIVED ROUTE          */~
                      edt%,              /* EFFECTIVE DATE             */~
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
           lwc$ = " ": lwc1$ = hex(ff) : lau% = -1%

L52030:    get #63, using L52060, lwc2$, step%, sdt%, aaa%, asu%, step$,  ~
                                 palevel%, wd$

L52060:         FMT CH(4), XX(4), BI(1), BI(4), XX(1), BI(4), BI(4),     ~
                    XX(4), CH(4), BI(1), CH(4)


                str(step$,5) = "-"
                convert palevel% to str(step$,6,2), pic(00)
                if holdstep% = step% then step$ = " "
                if lwc2$     = " "   then step$ = " "

                if lwc2$ <> lwc1$ then lwc$ = lwc2$
                   if lau% = 0% then L52126
                      if aaa%  = 0% then lwc$ = lwc2$
                      goto L52130
L52126:               if aaa% <> 0% then lwc$ = lwc2$

L52130:         gosub L52500
                if line% = 0% then gosub L52900

                date_fmtr$ = yymmdd$(sdt%)
                call "DATEFMT" (date_fmtr$, nil%, ccyymmdd$(1%))

                print using L45740, step$, lwc$,                  ~
                                    str(ccyymmdd$(1%), 3%, 6%),   ~
                                   asu%, aaa% - asu%, wd$
                line% = line% + 1%
                holdstep% = step%
                lwc1$ = lwc2$ : lau% = aaa% : lwc$ = " "
            call "PLOWNEXT" (#63, wckey$, 5%, f1%(63))
                if f1%(63) <> 0 then L52030
            call "PLOWNEXT" (#63, wckey$, 4%, f1%(63))
                if f1%(63) = 0 then L51100
                    print using  L45760
                    line% = line% + 1%
                    lwc1$ = hex(ff) : lau% = -1%
                    goto L52030

L52500:     if line% < 50% then return
            if mod(page% + pageb%,250%) = 0% then close printer
            select printer (134)
            page% = page% + 1% : print page
            print using L45040, date$, time$, page%
            print using L45800
            print skip(2) : print using L45640, qtyu(1%), part$, dc$, dl$
            line% = 0%
            return

L52800:     print: print using L45660, rte$(2%), qtyp(2%), part$(2%),     ~
                   10000%-temp%
            print using L45680
            line% = line% + 3%
            return


L52900:    print: print using L45720
           line% = line% + 2%
           return

        EJECT
        REM *******  SCREEN SUB-ROUTINES  ******************************

        def fn'080 (scrn$, scrnrow%, scrnintsy%) /* PUT LINE ON SCREEN */
          if str(planflags$(),1,1) <> "Y" then return
           call "SCRNADDR" ("W", scrnrow%, 2%, scrn$, 78%, scrnintsy%,r%)
           gosub checkinterupt
           return


        screenplot   /* PLOT PLAN FOR PART ON SCREEN         */
            init (" ") scrn$

            scrnaction$ = "*"
                if action%(2%) = 1% then scrnaction$ = hex(03)
                if action%(2%) = 2% then scrnaction$ = "*"
                if action%(2%) = 3% then scrnaction$ = hex(0b)
                if action%(2%) = 4% then scrnaction$ = "#"

            scrne% = min(78%, 78% - int((ed%(1%) - ed%(2%))*scrnscale))
                if scrne% > 78% then scrne% = 78%
            scrns% = max( 1%, 78% - int((ed%(1%) - sd%(2%))*scrnscale))
                if scrns% > scrne% then scrns% = scrne%
            scrnl% = 1% + scrne% - scrns%
            if scrns% + scrnl% > 79% then scrnl% = min(1%, 79%-scrns%)
            init (scrnaction$) str(scrn$, scrns%, scrnl%)
                convert l% - 1% to str(scrn$,1,4), pic (0000)

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

            gosub screenplot                      /* PUT OUT LAST LINE */

        screenpart
            scrndate1$ = yymmdd$(ed%(2%)) : call "DATEFMT" (scrndate1$)

            init (" ") scrn$
            gosub'080 (scrn$, 3%, 0%)
           put scrn$, using scrn01, max(1%, l% - 1%), qtyu(2%), part$(2%)
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

            scrne% = min(78%, 78% - int((ed%(1%) - ed%(2%))*scrnscale))
                if scrne% > 78% then scrne% = 78%
            scrns% = max( 1%, 78% - int((ed%(1%) - sd%(2%))*scrnscale))
                if scrns% > scrne% then scrns% = scrne%
            scrnl% = 1% + scrne% - scrns%
            if scrns% + scrnl% > 79% then scrnl% = min(1%, 79%-scrns%)
            init (scrnaction$) str(scrn$, scrns%, scrnl%)
                convert l%  to str(scrn$,1,4), pic (0000)

            if scrnline% < 23% then L62210
           if str(planflags$(),1,1) <> "Y" then L62170
                call "SCRNADDR" ("E", 5%, 2%, scrn$, 1%, 1%, r%)
L62170:         gosub'080 (scrn23$, 23%, 1%)
                gosub'080 (scrn24$, 24%, 1%)
                scrnline% = 5%

L62210:     gosub'080(scrn$, scrnline%, 0%)

            return


        checkinterupt     /* SEE IF ABORT KEY HIT  */
            if r% <>  127% then return
            errormsg$ = "RUN ABORTED BY OPERATOR"
            goto return_unplanned




        scrn01:%CURRENT: LINE ####. ######## PART #######################~
        ~##






        % YOUR PROCUREMENT DEMAND IS NOT FEASABLE <<<<<<<<<<<<< #
        % YOUR DEMAND CANNOT BE FULFILLED WITHIN THE PLANNING PERIOD #


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
                CH(4) ,        /* ACTIVITY                             */~
                CH(31)         /* FILLER/TAG NUMBER                    */~

