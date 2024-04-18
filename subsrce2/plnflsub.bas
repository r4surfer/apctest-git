        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   L      N   N  FFFFF  L       SSS   U   U  BBBB    *~
            *  P   P  L      NN  N  F      L      S      U   U  B   B   *~
            *  PPPP   L      N N N  FFFF   L       SSS   U   U  BBBB    *~
            *  P      L      N  NN  F      L          S  U   U  B   B   *~
            *  P      LLLLL  N   N  F      LLLLL   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLNFLSUB - INPUT OF USER SELECTABLE SWITCHES AND OPTIONS  *~
            *            TO CONTROL THEN ACTIONS OF PLANSUB. 3 SECTIONS *~
            *            DIVIDED INTO 1) SYSTEM WIDE DECISIONS, 2) MORE *~
            *            DETAILED DECISIONS (DT,PR, TYPE) AND 3A AND B) *~
            *            WHAT TO DO WHEN A CONSTRAINT IS HIT.           *~
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
            * 06/28/84 ! ORIGINAL                                 ! KEN *~
            * 03/25/87 ! ADDED PUR. LOOK AHEAD TO OPTIMIZE JUMP   ! KEN *~
            * 01/20/88 ! Added Approval Flag                      ! TLJ *~
            * 08/08/88 ! Added MFG Forward Jump Scan Increment    ! RJM *~
            * 06/03/91 ! Insidious Bug at 30710 in load logic.    ! KAB *~
            *          ! Only becomes active under certian cond-  !     *~
            *          ! itions but ugly when it wakes up.        !     *~
            * 06/22/93 ! Flags for Prescan for IW (Phantoms) and  ! KAB *~
            *          !   'all or nothing' if some available.    !     *~
            * 08/19/93 ! Purchase Job Support - BW's              ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "PLNFLSUB" (planflags$(), err%)

        dim planflags$(25)20,                                            ~
            holdflags$(25)20                                             ~

        dim                                                              ~
            apprvl$1,                    /* APPROVAL REQUIRED FLAG     */~
            askhdr$40,                                                   ~
            askpf1$80,                                                   ~
            askmid$80,                                                   ~
            askpf2$80,                                                   ~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fac1$(30)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            forecast$1,                  /* FORCE FORECAST TO FEAS.    */~
            pegging$1,                   /* PEGGING OPTIONS            */~
            scrndsply$1,                 /* SHOW SCREEN DISPLAY DURING */~
            summproc$1,                  /* PRINT SUMMARY OF PROCUREMEN*/~
            summwcs$1,                   /* PRINT SUMMARY OF WORK CENTE*/~
            toolback$1,                                                  ~
            toolfore$1,                                                  ~
            unplanrpt$1,                                                 ~
            boopt$1,                                                     ~
            filler$(25)20,                                               ~
            page2$(30)6,                                                 ~
            parttype$(30)8,                                              ~
            demdtype$(30)8,                                              ~
            demprior$(30)27,                                             ~
            header1$45,                                                  ~
            test$1,                                                      ~
            temp$27,                                                     ~
            usewkf$1,                                                    ~
            sumabort$1

        dim                                                              ~
            header2$79,                                                  ~
            header3$79,                                                  ~
            prior$(8)1,                                                  ~
            dts%(8),                                                     ~
            lta%(26),                    /* PRIORITY A                 */~
            pcfiw%(8),                                                   ~
            fixedfiw%(8)                                                 ~

        dim                                                              ~
            cursor%(2),                                                  ~
            i$(24)80,                                                    ~
            line2$79,                                                    ~
            line21$79,                                                   ~
            pfmsg1$79,                                                   ~
            pfmsg2$79,                                                   ~
            pfmsg3$79,                                                   ~
            pfkey$32,                                                    ~
            screentitle$(3)40

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

            init (" ") parttype$(), demdtype$(), demprior$()
            today% = abs(err%)
            if err% < 0% then create% = 1% else create% = 0%
            err%=99%
            date$ = date
            call "DATEFMT" (date$)

            init (hex(00)) filler$()

            header1$="Part     Demand   Demand Priority"
            header2$ = "Planning Priority  Planning Lead Time ! Planning ~
        ~Priority  Planning Lead Time"
            header3$ = "Part Type      Priority  Days To Supply  [Force I~
        ~W:] % Lead time   Fixed Days"
            line2$ = " "
            str(line2$,62%) = "PLNFLSUB" & ":" & str(cms2v$,,8%)

            gosub L30000

            if create% = 1% then datasave

L10000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            page% = 1%
L10070:     fieldnr%, screen% = 0%
            gosub'111(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  3 then       L15000
                  if keyhit%  =  5 then       L11000
                  if keyhit%  =  6 then       L10220
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L10070
            fieldnr% = cursor%(1) - 5%
            if fieldnr% > 0% and fieldnr% < 7% then L10220
            fieldnr% = fieldnr% - 2%
            if fieldnr% = 7% then L10220
            fieldnr% = fieldnr% - 1%
            if fieldnr% = 8% then L10220
            fieldnr% = fieldnr% - 2%
            if fieldnr% = 9% then L10220
               goto L10070

L10220:     screen% = 1%
L10230:     gosub'111(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L10230

            screen% = 2%
            gosub'151
                  if errormsg$ <> " " then L10230
            goto L10000

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * PAGE 2                                                    *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            page% = 2%
L11070:     fieldnr%, screen% = 0%
            gosub'112(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       L10000
                  if keyhit%  =  3 then       L15000
                  if keyhit%  =  4 then       L10000
                  if keyhit%  =  5 then       L12000
                  if keyhit%  =  6 then       L11150
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11070
            fieldnr% = cursor%(1) - 5%
            if fieldnr% > 0% and fieldnr% < 11% then L11150
            fieldnr% = fieldnr% - 2%
            if fieldnr% > 10% and fieldnr% < 13% then L11150
               goto L11070

L11150:     screen% = 1%
L11160:     gosub'112(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11160

            screen% = 2%
            gosub'152
                  if errormsg$ <> " " then L11160
            goto L11000

L12000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * PAGE 3                                                    *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            page% = 3%
L12070:     fieldnr%, screen% = 0%
            gosub'113(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       L10000
                  if keyhit%  =  3 then       L15000
                  if keyhit%  =  4 then       L11000
                  if keyhit%  =  5 then       L13000
                  if keyhit%  =  6 then       L12160
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L12070
            fieldnr% = cursor%(1) - 5%
            if fieldnr% > 0% and fieldnr% <  4% then L12160
            fieldnr% = fieldnr% - 2%
            if fieldnr% > 3% and fieldnr% <  9% then L12160
            fieldnr% = fieldnr% - 2%
            if fieldnr% > 8% and fieldnr% < 11% then L12160
               goto L12070

L12160:     screen% = 1%
L12170:     gosub'113(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12170

            screen% = 2%
            gosub'153
                  if errormsg$ <> " " then L12170
            goto L12000

L13000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * PAGE 4                                                    *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            page% = 4%
L13070:     fieldnr%, screen% = 0%
            gosub'114(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       L10000
                  if keyhit%  =  3 then       L15000
                  if keyhit%  =  4 then       L12000
                  if keyhit%  =  5 then       L14000
                  if keyhit%  =  6 then       L13160
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L13070
            fieldnr% = cursor%(1) - 5%
            if fieldnr% > 0% and fieldnr% < 13% then L13160
               goto L13070

L13160:     screen% = 1%
L13170:     gosub'114(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L13170

            screen% = 2%
            gosub'154
                  if errormsg$ <> " " then L13170
            goto L13000

L14000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * PAGE 5                                                    *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            page% = 5%
L14070:     fieldnr%, screen% = 0%
            gosub'115(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       L10000
                  if keyhit%  =  3 then       L15000
                  if keyhit%  =  4 then       L13000
                  if keyhit%  =  5 then       L15000
                  if keyhit%  =  6 then       L14150
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L14070
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% > 13% then L14070
            if cursor%(2) > 40% then fieldnr% = fieldnr% + 13%

L14150:     screen% = 1%
L14160:     gosub'115(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L14160

            screen% = 2%
            gosub'155
                  if errormsg$ <> " " then L14160
            goto L14000

L15000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * PAGE 6                                                    *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            page% = 6%
L15070:     fieldnr%, screen% = 0%
            gosub'116(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       L10000
                  if keyhit%  =  4 then       L14000
                  if keyhit%  =  6 then       L15140
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L15070
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% > 8% then L15070

L15140:     screen% = 1%
L15150:     gosub'116(screen%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L15150

            screen% = 2%
            gosub'156
                  if errormsg$ <> " " then L15150
            goto L15000

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000
            goto L65000

L20000: REM *************************************************************~
            * INSTEAD OF DEFAULT/ENABLES THIS IS THE INITIAL VALUE      *~
            * SECTION - PAGE 1                                          *~
            *************************************************************

            scrndsply$  = "Y"
            summproc$   = "Y"
            summwcs$    = "N"
            sumabort$   = "N"
            usewkf$     = "N"
            boopt$      = "Y"
            pegging$    = "L"
            unplanrpt$  = "N"
            capmq%      = 50%

            return

L21000: REM *************************************************************~
            * INSTEAD OF DEFAULT/ENABLES THIS IS THE INITIAL VALUE      *~
            * SECTION - PAGE 2                                          *~
            *************************************************************

            forecast$   = "N"
            minjump%    = 5%
            maxjump%    = 490%
            purjump%    = 2%
            mfgjump%    = 2%
            toolcons%   = 10%
            toolback$   ="N"
            toolfore$   ="N"
            purscan%    = 0%
            mfgscan%    = 0%
            endoff%     = 0%
            lowoff%     = 0%
                gosub L59000

            return

L22000: REM *************************************************************~
            * INSTEAD OF DEFAULT/ENABLES THIS IS THE INITIAL VALUE      *~
            * SECTION - PAGE 3                                          *~
            *************************************************************

        REM SPLIT WITHDRAWALS
            parttype$( 1)="        " : demdtype$( 1)="        "
            demprior$( 1)="                           "
        REM MOVE ENTIRE WITHDRAWAL
            parttype$( 2)="        " : demdtype$( 2)="        "
            demprior$( 2)="                           "
        REM PLAN ONLY CURRENTLY BACKORDERED QUANTITY FOR SALES ORDERS
            parttype$(15)="        " : demdtype$(15)="        "
            demprior$(15)="                           "
        REM IGNORE W/C CAPACITY
            parttype$(12)="        " : demdtype$(12)="        "
            demprior$(12)="@                          "
        REM IGNORE NO WC ROUTING (INSTALLATION TOOL??)
            parttype$(13)="        " : demdtype$(13)="        "
            demprior$(13)="                           "
        REM TEST ALTERNATE WC STEPS PRIOR TO JUMP
            parttype$(14)="        " : demdtype$(14)="        "
            demprior$(14)="                           "
        REM DISREGARD ROUTE SPECIFIED IN BOM
            parttype$(16)="        " : demdtype$(16)="        "
            demprior$(16)="                           "
        REM IGNORE MOVE/QUEUE TIME
            parttype$(18)="        " : demdtype$(18)="        "
            demprior$(18)="                           "
        REM TRY ALTERNATE WITHDRAWAL PRIOR TO PROCURING PRIMARY
            parttype$( 8)="  4     " : demdtype$( 8)="        "
            demprior$( 8)="@                          "
        REM ALLOW PROCUREMENT OF ALTERNATE
            parttype$( 9)="23456789" : demdtype$( 9)="        "
            demprior$( 9)="                           "

            return

*          PARTTYPE$( I)="23456789" : DEMDTYPE$( I)="12345678"
*          DEMPRIOR$( I)="@ABCDEFGHIJKLMNOPQRSTUVWXYZ"

L23000: REM *************************************************************~
            * INSTEAD OF DEFAULT/ENABLES THIS IS THE INITIAL VALUE      *~
            * SECTION - PAGE 4                                          *~
            *************************************************************

        REM ALLOW SAFETY STOCK INTRUSION
            parttype$( 3)="        " : demdtype$( 3)="12      "
            demprior$( 3)="@                          "
        REM IGNORE NEGATIVE PIP
            parttype$( 4)="        " : demdtype$( 4)="        "
            demprior$( 4)="@                          "
        REM FORCE INVENTORY WITHDRAWAL
            parttype$( 5)="        " : demdtype$( 5)="        "
            demprior$( 5)="                           "
        REM IGNORE MOQ/PANSIZE CALCULATION
            parttype$( 6)="        " : demdtype$( 6)="        "
            demprior$( 6)="@                          "
        REM FORCE QTY REQUIRED TO INTEGER
            parttype$( 7)="        " : demdtype$( 7)="        "
            demprior$( 7)="                           "
        REM FORCE PROCUREMENT
            parttype$(10)="        " : demdtype$(10)="        "
            demprior$(10)="                           "
        REM IGNORE ATC, USE PIP
            parttype$(11)="        " : demdtype$(11)="        "
            demprior$(11)="@                          "
        REM ALLOW BYPRODUCT YIELD
            parttype$(17)="        " : demdtype$(17)="        "
            demprior$(17)="                           "
        REM ALLOW VARIABLE ATC
            parttype$(19)="        " : demdtype$(19)="        "
            demprior$(19)="                           "
        REM ALLOW PRESCAN FOR IW
            parttype$(21)="        " : demdtype$(21)="        "
            demprior$(21)="                           "
        REM ALLOW SPLIT FOR PHANTOMS
            parttype$(22)="        " : demdtype$(22)="        "
            demprior$(22)="                           "
        REM ALLOW PURCHASE JOBS
            parttype$(23)="        " : demdtype$(23)="        "
            demprior$(23)="                           "

            return

*          PARTTYPE$( I)="23456789" : DEMDTYPE$( I)="12345678"
*          DEMPRIOR$( I)="@ABCDEFGHIJKLMNOPQRSTUVWXYZ"

L24000: REM *************************************************************~
            * INSTEAD OF DEFAULT/ENABLES THIS IS THE INITIAL VALUE      *~
            * SECTION - PAGE 5                                          *~
            *************************************************************

            mat lta% = con : mat lta% = (999%) * lta%
            return

L25000: REM *************************************************************~
            * INSTEAD OF DEFAULT/ENABLES THIS IS THE INITIAL VALUE      *~
            * SECTION - PAGE 6                                          *~
            *************************************************************

            mat dts% = con
            str(prior$()) = " "
            mat pcfiw% = zer
            mat fixedfiw% = zer
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

L29100: REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            askhdr$ = "***** START OVER COMMAND*****"
            str(askpf1$,,40) = "PF1  - Return To Display.               "
            str(askpf1$ ,41) = "                                        "
            str(askmid$,,40) = "PF10 - Reset of ALL Current Defaults.   "
            str(askmid$ ,41) = "PF12 - Reset To Orig. Deflts by Page.   "
            str(askpf2$,,40) = "                                        "
            str(askpf2$ ,41) = "PF16 - Exit Program Without Changes.    "

            askkey% = 2%

            call "ASKUSER" (askkey%, askhdr$, askpf1$, askmid$, askpf2$)

            if askkey% =  1% then return
            if askkey% = 10% then L29380
            if askkey% = 12% then L29440
            if askkey% = 16% then L65000
               goto L29100

L29380:        REM RESET CURRENT DEFAULTS (P.F. KEY 10)
                   str(planflags$()) = str(holdflags$())
                   return clear
                   gosub L30000
                   goto  L10000

L29440:        REM RETURN TO DISPLAY.    (P.F. KEY 12)
                   return clear
                   on page% gosub L20000, L21000, L22000, L23000, L24000,     ~
                                  L25000
                   on page% goto  L10000, L11000, L12000, L13000, L14000,     ~
                                  L15000
                      goto L10000 /* JUST IN CASE */

L30000: REM *************************************************************~
            *       S E T  D E F A U L T S  A N D  R E A D  D A T A     *~
            *                                                           *~
            * SETS UP, IF NO RECORD, DEFAULTS ARE SET FOR INPUT         *~
            *************************************************************

            if str(planflags$(),1,1)<>" " then L30120
                gosub L20000 : gosub L21000 : gosub L22000 : gosub L23000
                gosub L24000 : gosub L25000 : gosub L31000 : err% = 99%
                str(holdflags$()) = str(planflags$())
                return

L30120:     if str(holdflags$(),1,1) = " " then                          ~
                str(holdflags$()) = str(planflags$())

            get str(planflags$(),1), using L30770,                        ~
                    scrndsply$  ,                                        ~
                    summproc$   ,                                        ~
                    summwcs$    ,                                        ~
                    pegging$    ,                                        ~
                    forecast$   ,                                        ~
                    toolcons%   ,                                        ~
                    capmq%      ,                                        ~
                    minjump%    ,                                        ~
                    maxjump%    ,                                        ~
                    purjump%    ,                                        ~
                    mfgjump%    ,                                        ~
                    toolback$   ,                                        ~
                    toolfore$   ,                                        ~
                    unplanrpt$  ,                                        ~
                    boopt$      ,                                        ~
                    sumabort$   ,                                        ~
                    usewkf$     ,                                        ~
                    page2$()    ,                                        ~
                    apprvl$     ,                                        ~
                    purscan%    ,                                        ~
                    pcfiw%()    ,                                        ~
                    fixedfiw%() ,                                        ~
                    endoff%     ,                                        ~
                    lowoff%     ,                                        ~
                    lta%()      ,                                        ~
                    prior$()    ,                                        ~
                    dts%()      ,                                        ~
                    mfgscan%

            init (" ") parttype$(), demdtype$(), demprior$()
            for i%=1% to 30%
            for j%=1% to 3%
                on j% gosub L30540, L30610, L30680
            next j%
            next i%

            gosub L59000

            return

L30540:     for k%=1% to 8%
                test$=bin(2^(k%-1%))
                test$=test$ and str(page2$(i%),1,1)
                if test$<>hex(00) then str(parttype$(i%),k%,1)=bin(49%+k%)
                next k%
            return

L30610:     for k%=1% to 8%
                test$=bin(2^(k%-1%))
                test$=test$ and str(page2$(i%),2,1)
                if test$<>hex(00) then str(demdtype$(i%),k%,1)=bin(48%+k%)
                next k%
            return

L30680:     for k%=1% to 27%
                str(temp$,1,4)=bin(2^(k%-1%),4)
                str(temp$,1,4)=str(temp$,1,4) and str(page2$(i%),3,4)
                if str(temp$,1,4)<>hex(00000000) then                    ~
                                  str(demprior$(i%),k%,1)=bin(63%+k%)
                next k%
            return

            return
L30770:     FMT 5*CH(1),2*BI(1),4*BI(2),4*CH(1),2*CH(1),30*CH(6),        ~
                POS(206), CH(1), BI(2), 8*BI(4), 8*BI(4), 2*BI(4),       ~
                POS(281), 26*BI(4), 8*CH(1), 8*BI(4), BI(2)

L31000: REM *************************************************************~
            *       W R I T E   R E C O R D   T O   F I L E             *~
            *                                                           *~
            *                                                           *~
            *************************************************************

            init (hex(00)) page2$()
            for i%=1% to 30%
            for j%=1% to 3%
                on j% gosub L31140,L31210,L31280
            next j%
            next i%
            goto L31350

L31140:     for k%=1% to 8%
            test$=bin(k%+49%)
            if pos(parttype$(i%)=test$)<>0% then                         ~
                str(page2$(i%),1,1)=str(page2$(i%),1,1) or bin(2^(k%-1%))
            next k%
            return

L31210:     for k%=1% to 8%
            test$=bin(k%+48%)
            if pos(demdtype$(i%)=test$)<>0% then                         ~
                str(page2$(i%),2,1)=str(page2$(i%),2,1) or bin(2^(k%-1%))
            next k%
            return

L31280:     for k%=1% to 27%
            test$=bin(k%+63%)
            if pos(demprior$(i%)=test$)<>0% then                         ~
             str(page2$(i%),3,4)=str(page2$(i%),3,4) or bin(2^(k%-1%),4)
            next k%
            return

L31350:     put str(planflags$(),1), using L39060,                        ~
                    scrndsply$  ,                                        ~
                    summproc$   ,                                        ~
                    summwcs$    ,                                        ~
                    pegging$    ,                                        ~
                    forecast$   ,                                        ~
                    toolcons%   ,                                        ~
                    capmq%      ,                                        ~
                    minjump%    ,                                        ~
                    maxjump%    ,                                        ~
                    purjump%    ,                                        ~
                    mfgjump%    ,                                        ~
                    toolback$   ,                                        ~
                    toolfore$   ,                                        ~
                    unplanrpt$  ,                                        ~
                    boopt$      ,                                        ~
                    sumabort$   ,                                        ~
                    usewkf$     ,                                        ~
                    page2$()    ,                                        ~
                    str(filler$(),1,4),                                  ~
                    apprvl$     ,                                        ~
                    purscan%    ,                                        ~
                    pcfiw%()    ,                                        ~
                    fixedfiw%() ,                                        ~
                    endoff%     ,                                        ~
                    lowoff%     ,                                        ~
                    lta%()      ,                                        ~
                    prior$()    ,                                        ~
                    dts%()      ,                                        ~
                    mfgscan%    ,                                        ~
                    str(filler$(),1,54)                                  ~

            err% = 0% /* TRIGGER REWRITE IF APPROPRIATE */
            return

        REM *************************************************************~
            * BECAUSE OF THE CRYPTIC NATURE OF THIS PROGRAM AND THE     *~
            * RECORD IT PRODUCES, IT WOULD BE NICE TO HAVE A MAP OF THE *~
            * RECORD WITH DEFINITIIONS.   HERE IT IS . . . .            *~
            *************************************************************

L39060:     FMT /* CH(20),(POS)   KEY FIELD 'PLANNING SYSTEM FLAG'     */~
                CH( 1), /* 1   /* SCREEN DISPLAY (Y/N)                 */~
                CH( 1), /* 2   /* MATERIALS SUMMARY (Y/N)              */~
                CH( 1), /* 3   /* WORK CENTER SUMMARY (Y/N)            */~
                CH( 1), /* 4   /* PEGGING METHOD (Y,T,L,N)             */~
                CH( 1), /* 5   /* FORCE FORECAST TO FEASIBILITY (Y/N)  */~
                BI( 1), /* 6   /* TOOL CONST. (-1 TO 9) STORED +1      */~
                BI( 1), /* 7   /* W/C CAPACITY AUTOMATIC M&Q GENERATION*/~
                BI( 2), /* 8   /* MINIMUM JUMP                         */~
                BI( 2), /* 10  /* MAXIMUM JUMP                         */~
                BI( 2), /* 12  /* PURCHASE JUMP FUDGE FACTOR           */~
                BI( 2), /* 14  /* MFG JUMP FUDGE FACTOR                */~
                CH( 1), /* 16  /* TOOL BACKWARDS JUMP OPTION           */~
                CH( 1), /* 17  /* TOOL FOREWARDS JUMP OPTION           */~
                CH( 1), /* 18  /* PRINT UNPLANNING REP0RT              */~
                CH( 1), /* 19  /* OPTIMIZE PURCHASE ADVICES            */~
                CH( 1), /* 20  /* PRINT SUMMARY ON ABORT               */~
                CH( 1), /* 21  /* USE WORKFILE                         */~
                /* THE NEXT 30   FIELDS ARE BIT-ORIENTED, 6 CHARACTERS */~
                /*     BYTE 1 - PRODUCT CLASS                          */~
                /*        BIT 0 = 200  BIT 1 = 300 ... BIT 7 = 900     */~
                /*     BYTE 2 - DEMAND TYPE                            */~
                /*        BIT 0 = 1  BIT 1 = 2  BIT 2 = 3 ... BIT 7 = 8*/~
                /*     BYTES 3 - 6 (4 OF EM, 32 BITS)                  */~
                /*        BIT 0 = @  BIT 1 = A  BIT 2 = B ...BIT 26 = Z*/~
                /*        BITS 27 - 31 ARE UNUSED (BUT DON'T TELL)     */~
                CH( 6), /* 22  /* SPLIT LATE SO PIPOUTS                */~
                CH( 6), /* 28  /* MOVE ENTIRE LATE SO PIPOUT           */~
                CH( 6), /* 34  /* ALLOW SAFETY STOCK INTRUSION         */~
                CH( 6), /* 40  /* IGNORE NEGATIVE PIP                  */~
                CH( 6), /* 46  /* FORCE INVENTORY WITHDRAWAL           */~
                CH( 6), /* 52  /* IGNORE MOQ/PANSIZE CALCULATION       */~
                CH( 6), /* 58  /* FORCE QTY REQUIRED TO INTEGER        */~
                CH( 6), /* 64  /* TRY ALTERNATE IW PRIOR TO PROCUREMENT*/~
                CH( 6), /* 70  /* ALLOW PROCUREMENT OF ALTERNATES      */~
                CH( 6), /* 76  /* FORCE PROCUREMENT OF PART            */~
                CH( 6), /* 82  /* IGNORE ATC, USE PIP                  */~
                CH( 6), /* 88  /* DISREGARD WC CAPACITIES              */~
                CH( 6), /* 94  /* IGNORE NO ROUTING                    */~
                CH( 6), /* 100 /* TEST ALTERNATE WC STEPS PRIOR TO JUMP*/~
                CH( 6), /* 106 /* PLAN ONLY BACKORDERED QUANTITY       */~
                CH( 6), /* 112 /* DISREGARD LINKED ROUTE               */~
                CH( 6), /* 118 /* ALLOW PLANNED YIELD                  */~
                CH( 6), /* 124 /* IGNORE MOVE/QUEUE TIME               */~
                CH( 6), /* 130 /* ALLOW VARIABLE ATC                   */~
                CH( 6), /* 136 /* RESERVED - EXPEDITE LEAD TIME        */~
                CH( 6), /* 142 /* Prescan for possible IW's            */~
                CH( 6), /* 148 /* Split Phantoms if partial IW         */~
                CH( 6), /* 154 /* Purchase Jobs on or off              */~
                CH( 6), /* 160 /* RESERVED FOR FUTURE ENHANCEMENT      */~
                CH( 6), /* 166 /* RESERVED FOR FUTURE ENHANCEMENT      */~
                CH( 6), /* 172 /* RESERVED FOR FUTURE ENHANCEMENT      */~
                CH( 6), /* 178 /* RESERVED FOR FUTURE ENHANCEMENT      */~
                CH( 6), /* 184 /* RESERVED FOR FUTURE ENHANCEMENT      */~
                CH( 6), /* 190 /* RESERVED FOR FUTURE ENHANCEMENT      */~
                CH( 6), /* 196 /* RESERVED FOR FUTURE ENHANCEMENT      */~
                CH( 4), /* 202 /* FILLER                               */~
                CH(1),  /* 206 /* APPROVAL REQUIRED FLAG               */~
                BI( 2), /* 207 /* PURCH. JUMP FWD SCAN INCREMENT       */~
                8*BI(4),/* 209 /* PERCENT FORCE IW                     */~
                8*BI(4),/* 241 /* FIXED   FORCE IW                     */~
                2*BI(4),/* 273 /* START DATE OFFSETS                   */~
                26*BI(4),/*281 /* PLANNING LEAD TIME                   */~
                8*CH(1),/* 385 /* PLANNING PRIORITY DEFAULTS           */~
                8*BI(4),/* 393 /* PLANNING DAYS TO SUPPLY              */~
                BI( 2), /* 425 /* MFG JUMP FWD SCAN INCREMENT          */~
                CH(54)  /* 427 /* FILLER                               */~

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *-----------------------------------------------------------*~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(screen%)
                  gosub L49000
            screentitle$(1) = "General Planning System Options:"
            screentitle$(2) = "Unplanning/Pegging Option(s):"
            screentitle$(3) = "During Work Center Capacity Allocation:"
                  on screen%+1% goto L40065,            /* DISPLAY MODE */~
                                     L40085,            /* EDIT    MODE */~
                                     L40105             /* ERROR   MODE */
                     return

L40065:           str(pfmsg1$,20,33) = " "
                  str(pfkey$,3,1) = hex(ff)
                  str(pfkey$,5,1) = hex(ff)
                  goto L40220

L40085:           if fieldnr% > 0% then L40100
                     str(fac1$()) = all(hex(81))
                     goto L40220
L40100:           str(fac1$()) = all(hex(8c))
L40105:           on fieldnr% gosub L40185,         /* SCRNDSPLY        */~
                                    L40185,         /* SUMMARY MATERIALS*/~
                                    L40185,         /* SUMMARY WORK CENT*/~
                                    L40185,         /* SUMMARY ON ABORT */~
                                    L40185,         /* USE WORKFILE     */~
                                    L40185,         /* OPTIMIZE BO'S    */~
                                    L40185,         /* PEGGING OPTIONS  */~
                                    L40185,         /* UNPLAN REPORT    */~
                                    L40185          /* CAPACITY M&Q     */~

                   if errormsg$ <> " " then                              ~
                      fac1$(fieldnr%) = fac1$(fieldnr%) or hex(10)
                     goto L40220

                  REM Set FAC's for Upper/Lower Case Input
                      fac1$(fieldnr%) = hex(80)
                      return
L40185:           REM Set FAC's for Upper Case Only Input
                      fac1$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      fac1$(fieldnr%) = hex(82)
                      return

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Planning System Switches:  General Behavior",         ~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)),                                 ~
                  str(screentitle$(1),,len(screentitle$(1))),            ~
               at (06,02),                                               ~
        "   Show Planning System Screen Display While Planning",         ~
               at (07,02),                                               ~
        "   Print Summaries After Planning Run - Material Procurements", ~
               at (08,02),                                               ~
         "                                        Work Center Reservation~
        ~s",                                                              ~
               at (09,02),                                               ~
        "   Print Selected Summaries After Aborted Plan",                ~
               at (10,02),                                               ~
        "   Switch to Workfiles on Overflow",                            ~
               at (11,02),                                               ~
        "   Optimize Purchase Order Activity",                           ~
                                                                         ~
               at (13,02), fac(hex(ac)),                                 ~
                  str(screentitle$(2),,len(screentitle$(2))),            ~
               at (14,02),                                               ~
        "   (Y) Unplan All  (T) End Item Always (followed by L)",        ~
               at (15,02),                                               ~
        "   (L) Logical/Soft (PROC <= REQ'D) (N) Never (P) No File",     ~
               at (16,02),                                               ~
        "   Print Purge Report When Unplanning",                         ~
                                                                         ~
               at (18,02), fac(hex(ac)),                                 ~
                  str(screentitle$(3),,len(screentitle$(3))),            ~
               at (19,02),                                               ~
        "   Force Move & Queue Time at % Capacity (Last Step)",          ~
                                                                         ~
               at (21,02), fac(hex(a4)), line21$                , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               at (06,75), fac(fac1$( 1)), scrndsply$           , ch(01),~
               at (07,75), fac(fac1$( 2)), summproc$            , ch(01),~
               at (08,75), fac(fac1$( 3)), summwcs$             , ch(01),~
               at (09,75), fac(fac1$( 4)), sumabort$            , ch(01),~
               at (10,75), fac(fac1$( 5)), usewkf$              , ch(01),~
               at (11,75), fac(fac1$( 6)), boopt$               , ch(01),~
               at (14,75), fac(fac1$( 7)), pegging$             , ch(01),~
               at (16,75), fac(fac1$( 8)), unplanrpt$           , ch(01),~
               at (19,73), fac(fac1$( 9)), capmq%            , pic (###),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L40515
                  call "MANUAL" ("PLNFLAGS")
                  goto L40220

L40515:        if keyhit% <> 15 then L40535
                  call "PRNTSCRN"
                  goto L40220

L40535:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 2 OF DOCUMENT.                    *~
            *************************************************************

            deffn'112(screen%)
                  gosub L49000
            screentitle$(1) = "Jump Logic Options:"
            screentitle$(2) = "Activity Start Date Control Offsets"
                  on screen%+1% goto L41210,            /* DISPLAY MODE */~
                                     L41075,            /* EDIT    MODE */~
                                     L41095             /* ERROR   MODE */
                     return

L41075:           if fieldnr% > 0% then L41090
                     str(fac1$()) = all(hex(81))
                     goto L41210
L41090:           str(fac1$()) = all(hex(8c))
L41095:           on fieldnr% gosub L41175,         /* FORECAST JUMP OPT*/~
                                    L41175,         /* MINIMUM JUMP     */~
                                    L41175,         /* MAXIMUM JUMP     */~
                                    L41175,         /* PURCHASE JUMP ALL*/~
                                    L41175,         /* PUR. JUMP FWD SCN*/~
                                    L41175,         /* MFG JUMP ALL     */~
                                    L41175,         /* MFG JUMP FWD SCAN*/~
                                    L41175,         /* TOOLING CONST    */~
                                    L41175,         /* TOOL BACKWARD    */~
                                    L41175,         /* TOOL FOREWARD    */~
                                    L41175,         /* END OFFSET       */~
                                    L41175          /* COMP OFFSET      */~

                   if errormsg$ <> " " then                              ~
                      fac1$(fieldnr%) = fac1$(fieldnr%) or hex(10)
                     goto L41210

                  REM Set FAC's for Upper/Lower Case Input
                      fac1$(fieldnr%) = hex(80)
                      return
L41175:           REM Set FAC's for Upper Case Only Input
                      fac1$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      fac1$(fieldnr%) = hex(82)
                      return

L41210:     accept                                                       ~
               at (01,02),                                               ~
                  "Planning System Switches:  Jump Control Factors",     ~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)),                                 ~
                  str(screentitle$(1),,len(screentitle$(1))),            ~
               at (06,02),                                               ~
        "   Revert Forecast Planning to Feasibility Test on Jump",       ~
               at (07,02),                                               ~
        "   Minimum Number of Days to Jump",                             ~
               at (08,02),                                               ~
        "   Maximum Number of Days to Jump (From Due Date)",             ~
               at (09,02),                                               ~
        "   Purchasing Jump Allowance (Days)",                           ~
               at (10,02),                                               ~
        "       Purchasing Jump Forward Scan Increment (Days)",          ~
               at (11,02),                                               ~
        "   Manufacturing Jump Factor (Days)",                           ~
               at (12,02),                                               ~
        "       Manufacturing Jump Forward Scan Increment (Days)",       ~
               at (13,02),                                               ~
        "   Lowest Part Type for Tooling Checking (Last Digit)",         ~
               at (14,02),                                               ~
        "   Check by Day for Backward Jump on Tooling Const.  ",         ~
               at (15,02),                                               ~
        "   Check by Day for Forward Jump on Tooling Const.   ",         ~
                                                                         ~
               at (17,02), fac(hex(ac)),                                 ~
                  str(screentitle$(2),,len(screentitle$(2))),            ~
               at (18,02),                                               ~
        "   End Item First Activity Offset (Days from Today)",           ~
               at (19,02),                                               ~
        "   Lower Level Item Offset (Days from Today)",                  ~
                                                                         ~
               at (21,02), fac(hex(a4)), line21$                , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               at (06,75), fac(fac1$( 1)), forecast$            , ch(01),~
               at (07,73), fac(fac1$( 2)), minjump%           , pic(###),~
               at (08,73), fac(fac1$( 3)), maxjump%           , pic(###),~
               at (09,73), fac(fac1$( 4)), purjump%           , pic(###),~
               at (10,73), fac(fac1$( 5)), purscan%           , pic(###),~
               at (11,73), fac(fac1$( 6)), mfgjump%           , pic(###),~
               at (12,73), fac(fac1$( 7)), mfgscan%           , pic(###),~
               at (13,74), fac(fac1$( 8)), toolcons%          , pic (##),~
               at (14,75), fac(fac1$( 9)), toolback$            , ch(01),~
               at (15,75), fac(fac1$(10)), toolfore$            , ch(01),~
                                                                         ~
               at (18,72), fac(fac1$(11)), endoff%           , pic(-###),~
               at (19,72), fac(fac1$(12)), lowoff%           , pic(-###),~
               at (18,60), fac(hex(8c))  , enddate$             , ch(11),~
               at (19,60), fac(hex(8c))  , lowdate$             , ch(11),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L41515
                  call "MANUAL" ("PLNFLAGS")
                  goto L41210

L41515:        if keyhit% <> 15 then L41535
                  call "PRNTSCRN"
                  goto L41210

L41535:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   3       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 3 OF DOCUMENT.                    *~
            *************************************************************

            deffn'113(screen%)
                  gosub L49000
            screentitle$(1) = "When Planning Sales Orders:"
            screentitle$(2) = "Work Center Capacity Allocation:"
            screentitle$(3) = "Alternate Parts:"
                  on screen%+1% goto L42355,            /* DISPLAY MODE */~
                                     L42220,            /* EDIT    MODE */~
                                     L42240             /* ERROR   MODE */
                     return

L42220:           if fieldnr% > 0% then L42235
                     str(fac1$()) = all(hex(81))
                     goto L42355
L42235:           str(fac1$()) = all(hex(8c))
L42240:           on fieldnr% gosub L42320,         /* Split  Withdraw  */~
                                    L42320,         /* Entire Withdraw  */~
                                    L42320,         /* B.O. Qty Only    */~
                                    L42320,         /* Disregard Cap.   */~
                                    L42320,         /* Ignore Rte Error */~
                                    L42320,         /* Test Alts Prior  */~
                                    L42320,         /* Ignore RTE use LT*/~
                                    L42320,         /* Ignore Move/Queue*/~
                                    L42320,         /* Withdraw Alts    */~
                                    L42320          /* Procure Alts     */~

                   if errormsg$ <> " " then                              ~
                      fac1$(fieldnr%) = fac1$(fieldnr%) or hex(10)
                     goto L42355

                  REM Set FAC's for Upper/Lower Case Input
                      fac1$(fieldnr%) = hex(80)
                      return
L42320:           REM Set FAC's for Upper Case Only Input
                      fac1$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      fac1$(fieldnr%) = hex(82)
                      return

L42355:     accept                                                       ~
               at (01,02),                                               ~
                  "Planning System Switches (Type Selective):  S.O., W.C.~
        ~, & Alts.",                                                      ~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)),                                 ~
                  str(screentitle$(1),,len(screentitle$(1))),            ~
               at (06,02),                                               ~
        "Allow Split Withdrawals",                                       ~
               at (07,02),                                               ~
        "Entire Withdrawal on PCD",                                      ~
               at (08,02),                                               ~
        "Plan Backorder Quantity Only",                                  ~
                                                                         ~
               at (10,02), fac(hex(ac)),                                 ~
                  str(screentitle$(2),,len(screentitle$(2))),            ~
               at (11,02),                                               ~
        "Disregard Current WC Load",                                     ~
               at (12,02),                                               ~
        "Ignore No Route Error (Use LT)",                                ~
               at (13,02),                                               ~
        "Test ALT steps Prior to JUMP",                                  ~
               at (14,02),                                               ~
        "Disregard Route (Use LT)",                                      ~
               at (15,02),                                               ~
        "Disregard Move/Queue Time",                                     ~
                                                                         ~
               at (17,02), fac(hex(ac)),                                 ~
                  str(screentitle$(3),,len(screentitle$(3))),            ~
               at (18,02),                                               ~
        "W/D ALT Prior to PROC of Primary",                              ~
               at (19,02),                                               ~
        "Allow PROCUREMENT of Alternate",                                ~
                                                                         ~
               at (21,02), fac(hex(a4)), line21$                , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               at (04,36), fac(hex(ac))  , header1$             , ch(45),~
                                                                         ~
               at (06,36), fac(fac1$( 1)), parttype$( 1)        , ch( 8),~
               at (07,36), fac(fac1$( 2)), parttype$( 2)        , ch( 8),~
               at (08,36), fac(fac1$( 3)), parttype$(15)        , ch( 8),~
               at (11,36), fac(fac1$( 4)), parttype$(12)        , ch( 8),~
               at (12,36), fac(fac1$( 5)), parttype$(13)        , ch( 8),~
               at (13,36), fac(fac1$( 6)), parttype$(14)        , ch( 8),~
               at (14,36), fac(fac1$( 7)), parttype$(16)        , ch( 8),~
               at (15,36), fac(fac1$( 8)), parttype$(18)        , ch( 8),~
               at (18,36), fac(fac1$( 9)), parttype$( 8)        , ch( 8),~
               at (19,36), fac(fac1$(10)), parttype$( 9)        , ch( 8),~
                                                                         ~
               at (06,45), fac(fac1$( 1)), demdtype$( 1)        , ch( 8),~
               at (07,45), fac(fac1$( 2)), demdtype$( 2)        , ch( 8),~
               at (08,45), fac(fac1$( 3)), demdtype$(15)        , ch( 8),~
               at (11,45), fac(fac1$( 4)), demdtype$(12)        , ch( 8),~
               at (12,45), fac(fac1$( 5)), demdtype$(13)        , ch( 8),~
               at (13,45), fac(fac1$( 6)), demdtype$(14)        , ch( 8),~
               at (14,45), fac(fac1$( 7)), demdtype$(16)        , ch( 8),~
               at (15,45), fac(fac1$( 8)), demdtype$(18)        , ch( 8),~
               at (18,45), fac(fac1$( 9)), demdtype$( 8)        , ch( 8),~
               at (19,45), fac(fac1$(10)), demdtype$( 9)        , ch( 8),~
                                                                         ~
               at (06,54), fac(fac1$( 1)), demprior$( 1)        , ch(27),~
               at (07,54), fac(fac1$( 2)), demprior$( 2)        , ch(27),~
               at (08,54), fac(fac1$( 3)), demprior$(15)        , ch(27),~
               at (11,54), fac(fac1$( 4)), demprior$(12)        , ch(27),~
               at (12,54), fac(fac1$( 5)), demprior$(13)        , ch(27),~
               at (13,54), fac(fac1$( 6)), demprior$(14)        , ch(27),~
               at (14,54), fac(fac1$( 7)), demprior$(16)        , ch(27),~
               at (15,54), fac(fac1$( 8)), demprior$(18)        , ch(27),~
               at (18,54), fac(fac1$( 9)), demprior$( 8)        , ch(27),~
               at (19,54), fac(fac1$(10)), demprior$( 9)        , ch(27),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L42815
                  call "MANUAL" ("PLNFLAGS")
                  goto L42355

L42815:        if keyhit% <> 15 then L42840
                  call "PRNTSCRN"
                  goto L42355

L42840:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   4       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 4 OF DOCUMENT.                    *~
            *************************************************************

            deffn'114(screen%)
                  gosub L49000
            screentitle$(1) = "Procurement/Inventory Withdrawal:"
                  on screen%+1% goto L43345,            /* DISPLAY MODE */~
                                     L43220,            /* EDIT    MODE */~
                                     L43240             /* ERROR   MODE */
                     return

L43220:           if fieldnr% > 0% then L43235
                     str(fac1$()) = all(hex(81))
                     goto L43345
L43235:           str(fac1$()) = all(hex(8c))
L43240:           on fieldnr% gosub L43310,         /* Saftey Stock Int */~
                                    L43310,         /* Ign. Neg. PIP    */~
                                    L43310,         /* Force IW         */~
                                    L43310,         /* Ign. MOQ/Pansize */~
                                    L43310,         /* Integer Quantity */~
                                    L43310,         /* Force Procurement*/~
                                    L43310,         /* Use PIP          */~
                                    L43310,         /* Allow Yield      */~
                                    L43310,         /* Allow Var. ATC   */~
                                    L43310,         /* Prescan for IW   */~
                                    L43310,         /* Split Phantoms   */~
                                    L43310          /* Purchase Jobs    */~

                   if errormsg$ <> " " then                              ~
                      fac1$(fieldnr%) = fac1$(fieldnr%) or hex(10)
                     goto L43345

                  REM Set FAC's for Upper/Lower Case Input
                      fac1$(fieldnr%) = hex(80)
                      return
L43310:           REM Set FAC's for Upper Case Only Input
                      fac1$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      fac1$(fieldnr%) = hex(82)
                      return

L43345:     accept                                                       ~
               at (01,02),                                               ~
                  "Planning System Switches (Type Selective):  Material A~
        ~lloc.",                                                          ~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)),                                 ~
                  str(screentitle$(1),,len(screentitle$(1))),            ~
               at (06,02),                                               ~
        "Allow Safety Stock Intrusion",                                  ~
               at (07,02),                                               ~
        "Ignore Negative PIP",                                           ~
               at (08,02),                                               ~
        "Force Inventory Withdrawal",                                    ~
               at (09,02),                                               ~
        "Ignore MOQ/PANSIZE Calculation",                                ~
               at (10,02),                                               ~
        "Force QTY REQ'D to Integer",                                    ~
               at (11,02),                                               ~
        "Force Procurement of Part",                                     ~
               at (12,02),                                               ~
        "Ignore ATC, use PIP",                                           ~
               at (13,02),                                               ~
        "Allow By-Product Yield",                                        ~
               at (14,02),                                               ~
        "Allow Variable ATC Horizon",                                    ~
               at (15,02),                                               ~
        "Pre-Scan for possible IW",                                      ~
               at (16,02),                                               ~
        "Split of Phantoms if partial IW",                               ~
               at (17,02),                                               ~
        "Allow Purchase Jobs Option",                                    ~
                                                                         ~
               at (21,02), fac(hex(a4)), line21$                , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               at (04,36), fac(hex(ac))  , header1$             , ch(45),~
                                                                         ~
               at (06,36), fac(fac1$( 1)), parttype$( 3)        , ch( 8),~
               at (07,36), fac(fac1$( 2)), parttype$( 4)        , ch( 8),~
               at (08,36), fac(fac1$( 3)), parttype$( 5)        , ch( 8),~
               at (09,36), fac(fac1$( 4)), parttype$( 6)        , ch( 8),~
               at (10,36), fac(fac1$( 5)), parttype$( 7)        , ch( 8),~
               at (11,36), fac(fac1$( 6)), parttype$(10)        , ch( 8),~
               at (12,36), fac(fac1$( 7)), parttype$(11)        , ch( 8),~
               at (13,36), fac(fac1$( 8)), parttype$(17)        , ch( 8),~
               at (14,36), fac(fac1$( 9)), parttype$(19)        , ch( 8),~
               at (15,36), fac(fac1$(10)), parttype$(21)        , ch( 8),~
               at (16,36), fac(fac1$(11)), parttype$(22)        , ch( 8),~
               at (17,36), fac(fac1$(12)), parttype$(23)        , ch( 8),~
                                                                         ~
               at (06,45), fac(fac1$( 1)), demdtype$( 3)        , ch( 8),~
               at (07,45), fac(fac1$( 2)), demdtype$( 4)        , ch( 8),~
               at (08,45), fac(fac1$( 3)), demdtype$( 5)        , ch( 8),~
               at (09,45), fac(fac1$( 4)), demdtype$( 6)        , ch( 8),~
               at (10,45), fac(fac1$( 5)), demdtype$( 7)        , ch( 8),~
               at (11,45), fac(fac1$( 6)), demdtype$(10)        , ch( 8),~
               at (12,45), fac(fac1$( 7)), demdtype$(11)        , ch( 8),~
               at (13,45), fac(fac1$( 8)), demdtype$(17)        , ch( 8),~
               at (14,45), fac(fac1$( 9)), demdtype$(19)        , ch( 8),~
               at (15,45), fac(fac1$(10)), demdtype$(21)        , ch( 8),~
               at (16,45), fac(fac1$(11)), demdtype$(22)        , ch( 8),~
               at (17,45), fac(fac1$(12)), demdtype$(23)        , ch( 8),~
                                                                         ~
               at (06,54), fac(fac1$( 1)), demprior$( 3)        , ch(27),~
               at (07,54), fac(fac1$( 2)), demprior$( 4)        , ch(27),~
               at (08,54), fac(fac1$( 3)), demprior$( 5)        , ch(27),~
               at (09,54), fac(fac1$( 4)), demprior$( 6)        , ch(27),~
               at (10,54), fac(fac1$( 5)), demprior$( 7)        , ch(27),~
               at (11,54), fac(fac1$( 6)), demprior$(10)        , ch(27),~
               at (12,54), fac(fac1$( 7)), demprior$(11)        , ch(27),~
               at (13,54), fac(fac1$( 8)), demprior$(17)        , ch(27),~
               at (14,54), fac(fac1$( 9)), demprior$(19)        , ch(27),~
               at (15,54), fac(fac1$(10)), demprior$(21)        , ch(27),~
               at (16,54), fac(fac1$(11)), demprior$(22)        , ch(27),~
               at (17,54), fac(fac1$(12)), demprior$(23)        , ch(27),~
                                                                         ~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L43890
                  call "MANUAL" ("PLNFLAGS")
                  goto L43345

L43890:        if keyhit% <> 15 then L43915
                  call "PRNTSCRN"
                  goto L43345

L43915:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   5       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 5 OF DOCUMENT.                    *~
            *************************************************************

            deffn'115(screen%)
                  gosub L49000
                  on screen%+1% goto L44380,            /* DISPLAY MODE */~
                                     L44245,            /* EDIT    MODE */~
                                     L44275             /* ERROR   MODE */
                     return

L44245:           line21$ = "Note: Greater than 490 = ALWAYS plan, Less t~
        ~han -490 = NEVER plan."

                  if fieldnr% > 0% then L44270
                     str(fac1$()) = all(hex(82))
                     goto L44380
L44270:           str(fac1$()) = all(hex(8c))
L44275:           fac1$(fieldnr%) = hex(82)

                  if errormsg$ <> " " then                               ~
                      fac1$(fieldnr%) = fac1$(fieldnr%) or hex(10)
                     goto L44380

L44380:     accept                                                       ~
               at (01,02),                                               ~
                  "Planning System Switches: Priority Lead Time Control",~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)), line21$                , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), header2$               , ch(79),~
                                                                         ~
               at (06,40), "!",                                          ~
               at (07,40), "!",                                          ~
               at (08,40), "!",                                          ~
               at (09,40), "!",                                          ~
               at (10,40), "!",                                          ~
               at (11,40), "!",                                          ~
               at (12,40), "!",                                          ~
               at (13,40), "!",                                          ~
               at (14,40), "!",                                          ~
               at (15,40), "!",                                          ~
               at (16,40), "!",                                          ~
               at (17,40), "!",                                          ~
               at (18,40), "!",                                          ~
                                                                         ~
               at (06,10), "A",  at (06,50), "N",                        ~
               at (07,10), "B",  at (07,50), "O",                        ~
               at (08,10), "C",  at (08,50), "P",                        ~
               at (09,10), "D",  at (09,50), "Q",                        ~
               at (10,10), "E",  at (10,50), "R",                        ~
               at (11,10), "F",  at (11,50), "S",                        ~
               at (12,10), "G",  at (12,50), "T",                        ~
               at (13,10), "H",  at (13,50), "U",                        ~
               at (14,10), "I",  at (14,50), "V",                        ~
               at (15,10), "J",  at (15,50), "W",                        ~
               at (16,10), "K",  at (16,50), "X",                        ~
               at (17,10), "L",  at (17,50), "Y",                        ~
               at (18,10), "M",  at (18,50), "Z",                        ~
                                                                         ~
               at (06,30), fac(fac1$( 1)), lta%( 1)          , pic(-###),~
               at (07,30), fac(fac1$( 2)), lta%( 2)          , pic(-###),~
               at (08,30), fac(fac1$( 3)), lta%( 3)          , pic(-###),~
               at (09,30), fac(fac1$( 4)), lta%( 4)          , pic(-###),~
               at (10,30), fac(fac1$( 5)), lta%( 5)          , pic(-###),~
               at (11,30), fac(fac1$( 6)), lta%( 6)          , pic(-###),~
               at (12,30), fac(fac1$( 7)), lta%( 7)          , pic(-###),~
               at (13,30), fac(fac1$( 8)), lta%( 8)          , pic(-###),~
               at (14,30), fac(fac1$( 9)), lta%( 9)          , pic(-###),~
               at (15,30), fac(fac1$(10)), lta%(10)          , pic(-###),~
               at (16,30), fac(fac1$(11)), lta%(11)          , pic(-###),~
               at (17,30), fac(fac1$(12)), lta%(12)          , pic(-###),~
               at (18,30), fac(fac1$(13)), lta%(13)          , pic(-###),~
                                                                         ~
               at (06,70), fac(fac1$(14)), lta%(14)          , pic(-###),~
               at (07,70), fac(fac1$(15)), lta%(15)          , pic(-###),~
               at (08,70), fac(fac1$(16)), lta%(16)          , pic(-###),~
               at (09,70), fac(fac1$(17)), lta%(17)          , pic(-###),~
               at (10,70), fac(fac1$(18)), lta%(18)          , pic(-###),~
               at (11,70), fac(fac1$(19)), lta%(19)          , pic(-###),~
               at (12,70), fac(fac1$(20)), lta%(20)          , pic(-###),~
               at (13,70), fac(fac1$(21)), lta%(21)          , pic(-###),~
               at (14,70), fac(fac1$(22)), lta%(22)          , pic(-###),~
               at (15,70), fac(fac1$(23)), lta%(23)          , pic(-###),~
               at (16,70), fac(fac1$(24)), lta%(24)          , pic(-###),~
               at (17,70), fac(fac1$(25)), lta%(25)          , pic(-###),~
               at (18,70), fac(fac1$(26)), lta%(26)          , pic(-###),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L44920
                  call "MANUAL" ("PLNFLAGS")
                  goto L44380

L44920:        if keyhit% <> 15 then L44945
                  call "PRNTSCRN"
                  goto L44380

L44945:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   6       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 6 OF DOCUMENT.                    *~
            *************************************************************

            deffn'116(screen%)
                  gosub L49000
                  on screen%+1% goto L45070,            /* DISPLAY MODE */~
                                     L45220,            /* EDIT    MODE */~
                                     L45240             /* ERROR   MODE */
                     return

L45070:           str(pfmsg2$,20,33) = " "
                  str(pfkey$,4,1) = hex(ff)
                  str(pfkey$,6,1) = hex(ff)
                  goto L45355

L45220:           if fieldnr% > 0% then L45235
                     str(fac1$()) = all(hex(81))
                     goto L45355
L45235:           str(fac1$()) = all(hex(8c))
L45240:           on fieldnr% gosub L45320,         /* 200 - 299        */~
                                    L45320,         /* 300 - 399        */~
                                    L45320,         /* 400 - 499        */~
                                    L45320,         /* 500 - 599        */~
                                    L45320,         /* 600 - 699        */~
                                    L45320,         /* 700 - 799        */~
                                    L45320,         /* 800 - 899        */~
                                    L45320          /* 900 - 999        */~

                   if errormsg$ <> " " then                              ~
                      fac1$(fieldnr%) = fac1$(fieldnr%) or hex(10)
                     goto L45355

                  REM Set FAC's for Upper/Lower Case Input
                      fac1$(fieldnr%) = hex(80)
                      return
L45320:           REM Set FAC's for Upper Case Only Input
                      fac1$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      fac1$(fieldnr%) = hex(82)
                      return

L45355:     accept                                                       ~
               at (01,02),                                               ~
                  "Planning System Switches:  Optimization Priority/Days ~
        ~to Supply",                                                      ~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)),                                 ~
                  str(header3$,,len(header3$)),                          ~
               at (06,02), "200 - 299",                                  ~
               at (07,02), "300 - 399",                                  ~
               at (08,02), "400 - 499",                                  ~
               at (09,02), "500 - 599",                                  ~
               at (10,02), "600 - 699",                                  ~
               at (11,02), "700 - 799",                                  ~
               at (12,02), "800 - 899",                                  ~
               at (13,02), "900 - 999",                                  ~
                                                                         ~
               at (06,20), fac(fac1$( 1)), prior$( 1)           , ch( 1),~
               at (07,20), fac(fac1$( 2)), prior$( 2)           , ch( 1),~
               at (08,20), fac(fac1$( 3)), prior$( 3)           , ch( 1),~
               at (09,20), fac(fac1$( 4)), prior$( 4)           , ch( 1),~
               at (10,20), fac(fac1$( 5)), prior$( 5)           , ch( 1),~
               at (11,20), fac(fac1$( 6)), prior$( 6)           , ch( 1),~
               at (12,20), fac(fac1$( 7)), prior$( 7)           , ch( 1),~
               at (13,20), fac(fac1$( 8)), prior$( 8)           , ch( 1),~
                                                                         ~
               at (06,33), fac(fac1$( 1)), dts%( 1)           , pic(###),~
               at (07,33), fac(fac1$( 2)), dts%( 2)           , pic(###),~
               at (08,33), fac(fac1$( 3)), dts%( 3)           , pic(###),~
               at (09,33), fac(fac1$( 4)), dts%( 4)           , pic(###),~
               at (10,33), fac(fac1$( 5)), dts%( 5)           , pic(###),~
               at (11,33), fac(fac1$( 6)), dts%( 6)           , pic(###),~
               at (12,33), fac(fac1$( 7)), dts%( 7)           , pic(###),~
               at (13,33), fac(fac1$( 8)), dts%( 8)           , pic(###),~
                                                                         ~
               at (06,60), fac(fac1$( 1)), pcfiw%( 1)         , pic(###),~
               at (07,60), fac(fac1$( 2)), pcfiw%( 2)         , pic(###),~
               at (08,60), fac(fac1$( 3)), pcfiw%( 3)         , pic(###),~
               at (09,60), fac(fac1$( 4)), pcfiw%( 4)         , pic(###),~
               at (10,60), fac(fac1$( 5)), pcfiw%( 5)         , pic(###),~
               at (11,60), fac(fac1$( 6)), pcfiw%( 6)         , pic(###),~
               at (12,60), fac(fac1$( 7)), pcfiw%( 7)         , pic(###),~
               at (13,60), fac(fac1$( 8)), pcfiw%( 8)         , pic(###),~
                                                                         ~
               at (06,73), fac(fac1$( 1)), fixedfiw%( 1)      , pic(###),~
               at (07,73), fac(fac1$( 2)), fixedfiw%( 2)      , pic(###),~
               at (08,73), fac(fac1$( 3)), fixedfiw%( 3)      , pic(###),~
               at (09,73), fac(fac1$( 4)), fixedfiw%( 4)      , pic(###),~
               at (10,73), fac(fac1$( 5)), fixedfiw%( 5)      , pic(###),~
               at (11,73), fac(fac1$( 6)), fixedfiw%( 6)      , pic(###),~
               at (12,73), fac(fac1$( 7)), fixedfiw%( 7)      , pic(###),~
               at (13,73), fac(fac1$( 8)), fixedfiw%( 8)      , pic(###),~
                                                                         ~
               at (21,02), fac(hex(a4)), line21$                , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L45760
                  call "MANUAL" ("PLNFLAGS")
                  goto L45355

L45760:        if keyhit% <> 15 then L45780
                  call "PRNTSCRN"
                  goto L45355

L45780:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L49000: REM *************************************************************~
            * COMMON SET-UP FOR SCREENS                                 *~
            *************************************************************

                  pfmsg1$ = "(1)Start Over                               ~
        ~                   (13)Instructions"
                  pfmsg2$ = "                                            ~
        ~                   (15)Print Screen"
                  pfmsg3$ = " "
                  line21$ = " "
                  pfkey$  = hex(0001ffffffffff0d0fff)

                  on screen%+1% goto L49170,            /* DISPLAY MODE */~
                                     L49150,            /* EDIT    MODE */~
                                     L49250             /* ERROR   MODE */
L49150:              return

L49170:           str(pfmsg1$,20,33) = "(2)First Page    (4)Previous Page"
                  str(pfmsg2$,20,33) = "(3)Last Page     (5)Next Page    "
                  str(pfmsg3$,63) = hex(84) & "(16)EXIT PROGRAM"
                  pfkey$  = hex(000102030405060d0f10)
                  init(hex(8e)) fac1$()
                  line21$ = "Press PF6 for FULL SCREEN Edit."
                  return

L49250:           line21$ = "Correct the ERROR and press return."
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151
                  errormsg$ = " "
                  for fieldnr% = 1% to 9%
                  on fieldnr% gosub L50100,         /* SCRNDSPLY        */~
                                    L50150,         /* SUMMARY MATERIALS*/~
                                    L50200,         /* SUMMARY WORK CENT*/~
                                    L50250,         /* SUMMARY ON ABORT */~
                                    L50300,         /* USE WORKFILE     */~
                                    L50350,         /* OPTIMIZE BO'S    */~
                                    L50400,         /* PEGGING OPTIONS  */~
                                    L50500,         /* UNPLAN REPORT    */~
                                    L50550          /* CAPACITY M&Q     */~

                  if errormsg$ <> " " then return

                  str(fac1$(),fieldnr%,1)=hex(8c)
                  next fieldnr%
                  return

L50100:     REM TEST SCREEN DISPLAY
                if scrndsply$="Y" then return
                if scrndsply$="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L50150:     REM TEST MATERIALS SUMMARY
                if summproc$="Y" then return
                if summproc$="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L50200:     REM TEST WORK CENTER SUMMARY
                if summwcs$="Y" then return
                if summwcs$="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L50250:     REM TEST SUMMARY ON ABORT
                if sumabort$ ="Y" then return
                if sumabort$ ="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L50300:     REM TEST USE WORKFILE
                if usewkf$   ="Y" then return
                if usewkf$   ="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L50350:     REM TEST BO OPTIMIZATION
                if boopt$="Y" then return
                if boopt$="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L50400:     REM TEST PEGGING OPTIONS
                if pegging$="Y" then return
                if pegging$="T" then return
                if pegging$="L" then return
                if pegging$="N" then return
                if pegging$="P" then return
                   errormsg$ = "Valid Responses are 'Y', 'T', 'L', 'N', o~
        ~r 'P'"
                   return

L50500:     REM TEST UNPLAN REPORT
                if unplanrpt$="Y" then return
                if unplanrpt$="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L50550:     REM TEST CAPACITY MOVE & QUEUE PERCENT
                if capmq%>=0% and capmq%<=100% then L50590
                   errormsg$ = "Must Be '0 - 100' Percent"
                   return
L50590:         capmq%=capmq%/5% : capmq%=capmq%*5%
                return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152
                  errormsg$ = " "
                  for fieldnr% = 1% to 11%
                  on fieldnr% gosub L51300,         /* FORECAST JUMP OPT*/~
                                    L51350,         /* MINIMUM JUMP     */~
                                    L51400,         /* MAXIMUM JUMP     */~
                                    L51450,         /* PURCHASE JUMP ALL*/~
                                    L51500,         /* PUR. JMP FWD SCAN*/~
                                    L51550,         /* MFG JUMP ALL     */~
                                    L51850,         /* MFG JUMP FWD SCAN*/~
                                    L51600,         /* TOOLING CONST    */~
                                    L51650,         /* TOOL BACKWARD    */~
                                    L51700,         /* TOOL FOREWARD    */~
                                    L51750,         /* END OFFSET       */~
                                    L51800          /* LOW OFFSET       */~

                  if errormsg$ <> " " then return

                  str(fac1$(),fieldnr%,1)=hex(8c)
                  next fieldnr%
                  return

L51300:     REM TEST FORECAST FEASIBILITY ON JUMP
                if forecast$="Y" then return
                if forecast$="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L51350:     REM TEST MINIMUM JUMP
                if minjump% > 0% then return
                   errormsg$ = "Minimum Jump Must Be Greater Than Zero"
                   return

L51400:     REM TEST MAXIMUM JUMP
                if maxjump% >= 0% then return
                   errormsg$ = "Maximum Jump Cannot Be Less Than Zero"
                   return

L51450:     REM TEST PURCHASE JUMP ALLOWANCE
                if purjump% >= 0% then return
                   errormsg$ = "Purchase Jump Allowance Cannot Be Less Th~
        ~an Zero"
                   return

L51500:     REM TEST PURCHASE JUMP FORWARD SCAN
                if purscan% >= 0% then return
                   errormsg$ = "Purchase Jump Forward Scan Increment Cann~
        ~ot Be Less Than Zero"
                   return

L51550:     REM TEST MANUFACTURING JUMP ALLOWANCE
                if mfgjump% >= 0% then return
                   errormsg$ = "Manufacturing Jump Factor Cannot be Less ~
        ~than Zero"
                   return

L51600:     REM TEST CONSTRAINING TOOL SELECTION
                if toolcons% >= 0% and toolcons% <= 10% then return
                   errormsg$ = "Constraint Class Must Be '0 to 10'"
                   return

L51650:     REM TEST TOOL BACKWARDS OPTION
                if toolback$="Y" then return
                if toolback$="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L51700:     REM TEST TOOL FOREWARDS OPTION
                if toolfore$="Y" then return
                if toolfore$="N" then return
                   errormsg$ = "Valid Responses are 'Y' or 'N'"
                   return

L51750:     REM TEST END OFFSET
                gosub L59000
                return

L51800:     REM TEST END OFFSET
                gosub L59000
                return

L51850:     REM TEST MANUFACTURING JUMP FORWARD SCAN
                if mfgscan% >= 0% then return
                   errormsg$ = "Manufacturing Jump Forward Scan Increment~
        ~ Cannot Be Less Than Zero"
                   return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 3.                       *~
            *************************************************************

            deffn'153
                  errormsg$ = " "
                  for fieldnr% = 1% to 2%
                      gosub L52300
                  next fieldnr%

                  for fieldnr% = 8% to 9%
                      gosub L52300
                  next fieldnr%

                  for fieldnr% = 12% to 16%
                      gosub L52300
                  next fieldnr%

                  for fieldnr% = 18% to 18%
                      gosub L52300
                  next fieldnr%
                  return

L52300:           for column%  = 1 to 3%
                     on column% gosub L52500,  /* PART TYPE             */~
                                      L52600,  /* DEMAND TYPE           */~
                                      L52700   /* DEMAND PRIORITY       */~

                     if errormsg$ = " " then L52380
                        return clear
                        return
L52380:              next column%
                  fac1$(fieldnr%) = hex(8c)
                  return

L52500: REM CHECK PART TYPE ARRAY
            temp$=parttype$(fieldnr%):parttype$(fieldnr%)=" "
            for i%=2% to 9%
                test$=bin(i%+48%)
                if pos(temp$=test$)<>0% then                             ~
                 str(parttype$(fieldnr%),i%-1%,1%)=test$
            next i%
            return


L52600: REM CHECK DEMAND TYPE ARRAY
            temp$=demdtype$(fieldnr%):demdtype$(fieldnr%)=" "
            for i%=1% to 8%
                test$=bin(i%+48%)
                if pos(temp$=test$)<>0% then                             ~
                 str(demdtype$(fieldnr%),i%,1%)=test$
            next i%
            return

L52700: REM TEST PRIORITY ARRAY
            temp$=demprior$(fieldnr%):demprior$(fieldnr%)=" "
            for i%=1% to 27%
                test$=bin(i%+63%)
                if pos(temp$=test$)<>0% then                             ~
                    str(demprior$(fieldnr%),i%,1%)=test$
            next i%
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 4.                       *~
            *************************************************************

            deffn'154
                  errormsg$ = " "
                  for fieldnr% = 3% to 10%
                      gosub L53100
                  next fieldnr%

                  for fieldnr% = 11% to 11%
                      gosub L53100
                  next fieldnr%

                  for fieldnr% = 17% to 17%
                      gosub L53100
                  next fieldnr%

                  for fieldnr% = 19% to 19%
                      gosub L53100
                  next fieldnr%

                  for fieldnr% = 21% to 23%
                      gosub L53100
                  next fieldnr%

                  return

L53100:           for column%  = 1 to 3%
                     on column% gosub L53200,  /* PART TYPE             */~
                                      L53300,  /* DEMAND TYPE           */~
                                      L53390   /* DEMAND PRIORITY       */~

                     if errormsg$ = " " then L53140
                        return clear
                        return
L53140:           next column%
                  fac1$(fieldnr%) = hex(8c)
                  return

L53200: REM CHECK PART TYPE ARRAY
            temp$=parttype$(fieldnr%):parttype$(fieldnr%)=" "
            for i%=2% to 9%
                test$=bin(i%+48%)
                if pos(temp$=test$)<>0% then                             ~
                 str(parttype$(fieldnr%),i%-1%,1%)=test$
            next i%
            return


L53300: REM CHECK DEMAND TYPE ARRAY
            temp$=demdtype$(fieldnr%):demdtype$(fieldnr%)=" "
            for i%=1% to 8%
                test$=bin(i%+48%)
                if pos(temp$=test$)<>0% then                             ~
                 str(demdtype$(fieldnr%),i%,1%)=test$
            next i%
            return

L53390: REM TEST PRIORITY ARRAY
            temp$=demprior$(fieldnr%):demprior$(fieldnr%)=" "
            for i%=1% to 27%
                test$=bin(i%+63%)
                if pos(temp$=test$)<>0% then                             ~
                    str(demprior$(fieldnr%),i%,1%)=test$
            next i%
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'155
                  errormsg$ = " "

        REM TEST DATA FOR PRIORITIES, JUST CHECK RANGE
            for i%=1% to 13%
            lta%(i%)=min(999%, max(-999%,lta%(i%)))
            lta%(i%+13%)=min(999%, max(-999%,lta%(i%+13%)))
            next i%
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 6.                       *~
            *************************************************************

            deffn'156
                  errormsg$ = " "

        REM TEST DATA FOR PRIORITIES, DAYS TO SUPPLY
            for i%=1% to 8%
            if prior$(i%) = " " then L55150
            if prior$(i%) >= "@" and prior$(i%) <= "Z" then L55150
                errormsg$ = "Priority Must be '@, A - Z'"
                goto L55280
L55150:     if dts%(i%) > 0% and dts%(i%) < 491% then L55180
                errormsg$ = "Days to Supply Must Be '1 - 490'"
                goto L55280
L55180:     if pcfiw%(i%) >= 0% and pcfiw%(i%) < 100% then L55210
                errormsg$ = "Percent 'Force I/W' Must Be '1 - 100'"
                goto L55280
L55210:     if fixedfiw%(i%) >= 0% then L55260
                errormsg$ = "Fixed 'Force I/W' Cannot be Negative"
                goto L55280
L55260:     fac1$(i%) = hex(8c)
            next i%
L55280:         return

L59000: REM *************************************************************~
            * CODE SECTION TO FORMAT ACTIVITY START DATES               *~
            *************************************************************

            enddate$, lowdate$ = " "
            temp% = min(490%, max(1%, today% + endoff%))
            temp% = temp% - today%
            call "DATE" addr("G+", date, temp%, str(enddate$,3,6), u3%)
            call "DATEFMT" (str(enddate$,3,8))
            str(enddate$,2,1) = "(":str(enddate$,11,1) = ")"
            if temp% <> endoff% then str(enddate$,1,1) = hex(84)

            temp% = min(490%, max(1%, today% + lowoff%))
            temp% = temp% - today%
            call "DATE" addr("G+", date, temp%, str(lowdate$,3,6), u3%)
            call "DATEFMT" (str(lowdate$,3,8))
            str(lowdate$,2,1) = "(":str(lowdate$,11,1) = ")"
            if temp% <> lowoff% then str(lowdate$,1,1) = hex(84)

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

