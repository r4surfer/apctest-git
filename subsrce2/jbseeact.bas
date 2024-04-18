        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB    SSS   EEEEE  EEEEE    A     CCC   TTTTT   *~
            *    J    B   B  S      E      E       A A   C   C    T     *~
            *    J    BBBB    SSS   EEE    EEE    AAAAA  C        T     *~
            *  J J    B   B      S  E      E      A   A  C   C    T     *~
            *   J     BBBB    SSS   EEEEE  EEEEE  A   A   CCC     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBSEEACT - Displays Report Details Of Actual Job Movement.*~
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
            * 01/07/86 ! ORIGINAL                                 ! HES *~
            * 10/28/86 ! JBSTATUS & RTEMASTR File Format Changes  ! HES *~
            * 11/20/87 ! Summarize data more for display          ! HES *~
            * 09/15/97 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "JBSEEACT" (job$,                  /* Job Number       */~
                            #1,                    /* JBSTATUS File UFB*/~
                            #2,                    /* WCMASTR  File UFB*/~
                            #3,                    /* HNYMASTR File UFB*/~
                            #4,                    /* JBMASTR2 File UFB*/~
                            #5,                    /* RTEMASTR File UFB*/~
                            #6)                    /* JBCROSS2 File UFB*/

        dim                                                              ~
            actstart$8,                  /* ACTUAL START DATE          */~
            complete$10,                 /* QUANTITY COMPLETED TO DATE */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            plowkey$50,                  /* WORK VARIABLE              */~
            job$8,                       /* JOB NUMBER                 */~
            jobdescr$32,                 /* JOB NUMBER                 */~
            part$25,                     /* PART TO BE BUILT           */~
            partdescr$32,                /* PART TO BE BUILT           */~
            quantity$10,                 /* QUANTITY TO MAKE           */~
            rsetup$6,                    /* Total reported Setup HOURS */~
            vsetup$6,                    /* ---------------------------*/~
            pfdescr$(2)79,               /* TEXT FOR PFKEYS ACTIVE     */~
            pfkeys$32,                   /* PFKEYS ACTIVE              */~
            prun$6,                      /*                            */~
            psetup$6,                    /*                            */~
            rrun$6,                      /* Total reported Run HOURS   */~
            vrun$6,                      /*                            */~
            message$79,                  /* SCREEN INFO MESSAGE LINE   */~
            header$79,                   /* SCREEN HEADER              */~
            subheader$44,                /* SCREEN HEADER              */~
            title$79,                    /* SCREEN TITLE               */~
            fwc$(100)4,                  /* REPORTED WORK CENTER       */~
            ractv$(100)4,                /*                            */~
            rdate$(100)8,                /*                            */~
            rtext$(100)30,               /*                            */~
            rtime$(100)5,                /*                            */~
            rscrn$(12)45,                /* For Actuals Display        */~
            rpartsout(100),              /* REPORTED PARTS OUT         */~
            partstrashed(100),           /* REPORTED PARTS TO REWORK   */~
            partstillin(100),            /* REPORTED PARTS TO SCRAP    */~
            rrunhrs(100),                /* REPORTED RUN TIME          */~
            rsetuphrs(100),              /* REPORTED SET UP      HOURS */~
            step$(100)7,                 /* Step Numbers               */~
            stepfrom$7,                  /* Step Numbers               */~
            stepto$7,                    /* Step Numbers               */~
            ttext$30,                    /* Free Text On Movement Rcrd */~
            f1%(10)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *  1  ! JBSTATUS ! JOB STATUS TRACKING FILE                 *~
            *  2  ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            *  3  ! HNYMASTR ! INVENTORY MASTER (DESCRIPTIONS)          *~
            *  4  ! JBMASTR2 ! LEVEL 2 JOB MASTER FILE                  *~
            *  5  ! RTEMASTR ! STANDARD & ALT WORK CENTER ROUTINGS      *~
            *  6  ! JBCROSS2 ! JOB RTE/BOM USED CROSS REF.              *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

        REM *************************************************************~
            *          S E E   W C   T I M E   D E T A I L S            *~
            *                                                           *~
            * Get data, then show it...                                 *~
            *************************************************************

            temp = -1
            if job$ = " " then temp = 0
            call "GETCODE" (#4, job$, jobdescr$, 0%, temp, f1%(4))
                if f1%(4) = 0 then L65000
            get #4, using L10130, job$, jobdescr$, part$, qtymake,        ~
                                              qtycomp, actstart$, closed$
L10130:     FMT CH(8), CH(30), XX(19), CH(25), 2*PD(14,4), XX(48),2*CH(6)

            call "SHOSTAT" ("Loading Details...")
            call "DATEFMT" (actstart$)
            call "DATEFMT" (closed$)
            call "GETCODE" (#3, part$, partdescr$, 1%, 99, f1%(3))
            title$ = "Job Number: xxxxxxxx  Job Part:"
            str(title$,13,8) = job$
            title$ = title$ & " " & part$
            title$ = title$ & " " & partdescr$
            call "CONVERT" (qtymake, -0.2, str(quantity$,,10))
            call "CONVERT" (qtycomp, 0.2, str(complete$,,10))

        REM *************************************************************~
            *                  L O A D    D A T A                       *~
            *                                                           *~
            * Load 'Actual' data first.                                 *~
            *************************************************************

            ractv$(), rtime$(), rdate$(), rtext$(), fwc$(), rteused$= " "
            step$() = " "
            maxlines%, totsuhrs, totrunhrs, totprunhrs, totpsuhrs = 0
            mat rpartsout = zer          /* REPORTED PARTS OUT         */
            mat partstillin = zer        /* REPORTED PARTS TO SCRAP    */
            mat partstrashed = zer       /* REPORTED PARTS TO REWORK   */
            mat rsetuphrs = zer          /* REPORTED SET UP      HOURS */
            mat rrunhrs = zer            /* REPORTED RUN   HOURS       */
            done = 0

            plowkey$ = job$
            str(plowkey$,9) = all(hex(00))
L11180:     call "PLOWNEXT" (#1, plowkey$, 8%, f1%(1))
                if f1%(1) = 0 then loadplan

            get #1, using L11250, wcto$, actvto$, wcfrom$, tdate$, ttime$,~
                    actvfrom$, stepfrom$, stepto$, setuphrs, runhrs,     ~
                    partsout, scrapped, rework, ttext$

L11250:         FMT                      /* JBSTATUS                   */~
                    XX(08),              /* JOB NUMBER                 */~
                    XX(4),               /* SEQUENCE NUMBER            */~
                    CH(04),              /* TO WORK CENTER             */~
                    CH(04),              /* TO ACTIVITY CODE           */~
                    XX(08),              /* JOB NUMBER                 */~
                    CH(04),              /* FROM WORK CENTER           */~
                    CH(6),               /* TRANSACTION DATE           */~
                    CH(8),               /* TRANSACTION TIME           */~
                    CH(04),              /* FROM ACTIVITY CODE         */~
                    XX(14),              /* SYSTEM DATE & TIME         */~
                    CH(07),              /* FROM RTE STEP              */~
                    CH(07),              /* TO RTE STEP                */~
                    XX(12),              /* EMPLOYEE CODE              */~
                    BI(4),               /* SET UP TIME                */~
                    BI(4),               /* RUN TIME                   */~
                    PD(14,4),            /* QUANTITY MOVED             */~
                    PD(14,4),            /* QUANTITY SCRAPPED          */~
                    PD(14,4),            /* QUANTITY FOR REWORK JOB    */~
                    CH(30),              /* FREE TEXT                  */~
                    CH(03),              /* USERID WHO ENTERED TRANS   */~
                    CH(17)               /* FILLER                     */

            REM Convert WC units to hours for common base
            if wcfrom$ = " " then L11540
            call "WCUN2HRS" (#2, wcfrom$, 0, setuphrs, " ")
            call "WCUN2HRS" (#2, wcfrom$, 0, runhrs, " ")
            totsuhrs = totsuhrs + setuphrs
            totrunhrs = totrunhrs + runhrs
L11540:     if stepto$ = "DONE" then done = done + partsout
            call "DATEFMT" (tdate$)
            scrapped = scrapped + rework

            REM Add to any previously reported through this step...
            if stepfrom$ = " " then stepfrom$ = "PICKING"
            search step$() = str(stepfrom$) to f1%() step 7
            i% = (f1%(1)+6%)/7%
            if f1%(1) <> 0% then L11700
               REM Should only hit here for 'PICKING'...
               if stepfrom$ <> "PICKING" then L11810
               maxlines%, i%  = maxlines% + 1%
               step$(i%) = stepfrom$
               fwc$(i%) = wcfrom$ : ractv$(i%) = actvfrom$
               partstillin(i%) = qtymake
               setuphrs, runhrs = 0
L11700:     rdate$(i%) = tdate$
            rtime$(i%) = ttime$
            rsetuphrs(i%) = rsetuphrs(i%) + setuphrs
            rrunhrs(i%) = rrunhrs(i%) + runhrs
            rpartsout(i%) = rpartsout(i%) + partsout
            partstillin(i%) = partstillin(i%) - partsout - scrapped
            if step$(i%) = "PICKING" then partstillin(i%) =              ~
                                                 max(partstillin(i%), 0%)
            partstrashed(i%) = partstrashed(i%) + scrapped
            rtext$(i%) = ttext$

L11810:     REM Accumulate reported 'ins'...
            search step$() = str(stepto$) to f1%() step 7
            i% = (f1%(1)+6%)/7%
            if f1%(1) <> 0% then L11890
               REM Add 'TO' step to stacks...
               maxlines%, i%  = maxlines% + 1%
               step$(i%) = stepto$
               fwc$(i%) = wcto$ : ractv$(i%) = actvto$
L11890:     rdate$(i%) = tdate$
            rtime$(i%) = ttime$
            partstillin(i%) = partstillin(i%) + partsout
            rtext$(i%) = ttext$
            goto L11180

        loadplan
            call "READ100" (#6, "JOB ORDER: " & str(job$), f1%(6))
                     if f1%(6) = 0 then L12000
            get #6, using L11990, rteused$
L11990:     FMT XX(25), CH(3)
L12000:     plowkey$ = str(part$,,25) & str(rteused$,,3)

L12020:     call "PLOWNEXT" (#5, plowkey$, 28%, f1%(5))
                if f1%(5) <> 1% then format_totals
            get #5, using L12050, wc$, su, run
L12050:     FMT CH(04), XX(35), BI(2), PD(14,4)

            if wc$ = "VEND" then L12100
            call "WCUN2HRS" (#2, wc$, 0, su, " ")
            call "WCUN2HRS" (#2, wc$, 0, run, " ")
L12100:     totpsuhrs = totpsuhrs + su
            totprunhrs = totprunhrs + run * qtymake
            goto L12020

        format_totals
            call "CONVERT" (totsuhrs, -0.2, rsetup$)
            call "CONVERT" (totrunhrs, -0.2, rrun$)
            call "CONVERT" (totpsuhrs, -0.2, psetup$)
            call "CONVERT" (totprunhrs, -0.2, prun$)
            call "CONVERT" (totpsuhrs-totsuhrs, -0.2, vsetup$)
            call "CONVERT" (totprunhrs-totrunhrs, -0.2, vrun$)

            message$ = "Job Was CLOSED on " & closed$
            if closed$ <> " " then L12440
            if abs(qtymake - qtycomp) > .009 then L12280
            message$ = "Job Is Ready To Be Closed"
            goto L12440

L12280:     REM How much has been reported as done from the shop floor?
            if maxlines% > 0 then L12320
                message$ = "No Movement Details Have Been Reported"
                goto L12440
L12320:     scraped = 0
            for i% = 1 to maxlines%
                if step$(i%) = "DONE" then done = done - rpartsout(i%)
                scraped = round(scraped + partstrashed(i%), 2)
            next i%

            call "CONVERT" (done, -0.2, number$)
            message$ = "Shop Floor Reports " & number$
            call "CONVERT" (scraped, -0.2, number$)
            message$ = message$ & " Done, " & number$
            message$ = message$ & " Scrapped or sent to rework Job."

L12440:     call "STRING" addr ("CT", message$, 79%)
            maxlines% = maxlines% + 1  /* Slot For 'End' Message */
            base% = 0

        REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * Show'em what we got...                                    *~
            *************************************************************

L20080:     pfdescr$(1) = "(2)First Trans.  (4/6)Previous Trans.   (8)See~
        ~ PLANNED Routing (15)Print Screen"
            pfdescr$(2) = "(3)Last Trans.   (5/7)Next Transactions       ~
        ~         (13)Instruc.  (16)Return"
            pfkeys$ = hex(000102040506070dff0f10030809ffff)

*        Flip Off Appropriate Fields
            if base% > 0 then L20190
                str(pfdescr$(1),18,20)   = " "   /* Shut Off Prev Line */
                str(pfdescr$(1),,15)     = " "  /* Shut Off First Line */
                str(pfkeys$,3,2), str(pfkeys$,6,1) = hex(ffff)
L20190:     if base% < maxlines%-12 then L20250
                str(pfdescr$(2),18,22)   = " "   /* Shut Off Next Line */
                str(pfdescr$(2),,15)     = " "  /* Shut Off Last Line */
                str(pfkeys$,5,1), str(pfkeys$,7,1) = hex(ff)
                str(pfkeys$,12,1) = hex(ff)

L20250:     rscrn$() = " "
            header$ = "Step     WCTR ACTV   DATE & TIME   SET UP    RUN  ~
        ~FINISHED  STILL IN  SCRP/RWRK"
            subheader$ = "... HOURS ...  ........ QUANTITIES ........."
            if maxlines% = 1 then L20490
            for loop% = base% + 1 to min(maxlines%-1, base%+12)
             m% = loop% - base%
             tmp = rsetuphrs(loop%)
                call "CONVERT" (tmp, 0.1, str(rscrn$(m%),2,6))
                str(rscrn$(m%),,1) = hex(8c)
             tmp = rrunhrs(loop%)
                call "CONVERT" (tmp, 0.1, str(rscrn$(m%),9,6))
             tmp = rpartsout(loop%) :     str(rscrn$(m%),15,1) = hex(84)
                call "CONVERT" (tmp, 0.2, str(rscrn$(m%),17,8))
                if tmp = 0 then str(rscrn$(m%),15,1) = hex(8c)
             tmp = partstillin(loop%) :  str(rscrn$(m%),26,1) = hex(84)
                call "CONVERT" (tmp, 0.2, str(rscrn$(m%),27,8))
                if tmp = 0 then str(rscrn$(m%),26,1) = hex(8c)
             tmp = partstrashed(loop%) : str(rscrn$(m%),37,1) = hex(84)
                call "CONVERT" (tmp, 0.2, str(rscrn$(m%),38,8))
                if tmp = 0 then str(rscrn$(m%),37,1) = hex(8c)
            next loop%
L20490:     str(pfdescr$(2),41,14) = "(9)Memo Text"
            if maxlines% <= base% + 12 then                              ~
               rscrn$(maxlines%-base%) = hex(8c) & "** End Of Details **"

L20530: accept                                                           ~
               at (01,02), "Reported Production Time & Route Details     ~
        ~                    Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,03), "Planned Set up Hours For Job:",              ~
               at (04,33), fac(hex(84)), psetup$                , ch(06),~
               at (04,43), "Reported:",                                  ~
               at (04,53), fac(hex(84)), rsetup$                , ch(06),~
               at (04,62), "Variance:",                                  ~
               at (04,72), fac(hex(84)), vsetup$                , ch(06),~
                                                                         ~
               at (05,03), "Planned Run Hours For Job:",                 ~
               at (05,33), fac(hex(84)), prun$                  , ch(06),~
               at (05,43), "Reported:",                                  ~
               at (05,53), fac(hex(84)), rrun$                  , ch(06),~
               at (05,62), "Variance:",                                  ~
               at (05,72), fac(hex(84)), vrun$                  , ch(06),~
                                                                         ~
               at (06,03), "Job Released On:",                           ~
               at (06,20), fac(hex(84)), actstart$              , ch(08),~
               at (06,32), "Job Quantity:",                              ~
               at (06,46), fac(hex(84)), quantity$              , ch(10),~
               at (06,60), "Routing Used:",                              ~
               at (06,74), fac(hex(84)), rteused$               , ch(03),~
                                                                         ~
               at (07,37), fac(hex(8c)), subheader$             , ch(44),~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(hex(8c)), step$(base% + 01)      , ch(07),~
               at (10,02), fac(hex(8c)), step$(base% + 02)      , ch(07),~
               at (11,02), fac(hex(8c)), step$(base% + 03)      , ch(07),~
               at (12,02), fac(hex(8c)), step$(base% + 04)      , ch(07),~
               at (13,02), fac(hex(8c)), step$(base% + 05)      , ch(07),~
               at (14,02), fac(hex(8c)), step$(base% + 06)      , ch(07),~
               at (15,02), fac(hex(8c)), step$(base% + 07)      , ch(07),~
               at (16,02), fac(hex(8c)), step$(base% + 08)      , ch(07),~
               at (17,02), fac(hex(8c)), step$(base% + 09)      , ch(07),~
               at (18,02), fac(hex(8c)), step$(base% + 10)      , ch(07),~
               at (19,02), fac(hex(8c)), step$(base% + 11)      , ch(07),~
               at (20,02), fac(hex(8c)), step$(base% + 12)      , ch(07),~
                                                                         ~
               at (09,11), fac(hex(84)), fwc$(base% + 01)       , ch(04),~
               at (10,11), fac(hex(84)), fwc$(base% + 02)       , ch(04),~
               at (11,11), fac(hex(84)), fwc$(base% + 03)       , ch(04),~
               at (12,11), fac(hex(84)), fwc$(base% + 04)       , ch(04),~
               at (13,11), fac(hex(84)), fwc$(base% + 05)       , ch(04),~
               at (14,11), fac(hex(84)), fwc$(base% + 06)       , ch(04),~
               at (15,11), fac(hex(84)), fwc$(base% + 07)       , ch(04),~
               at (16,11), fac(hex(84)), fwc$(base% + 08)       , ch(04),~
               at (17,11), fac(hex(84)), fwc$(base% + 09)       , ch(04),~
               at (18,11), fac(hex(84)), fwc$(base% + 10)       , ch(04),~
               at (19,11), fac(hex(84)), fwc$(base% + 11)       , ch(04),~
               at (20,11), fac(hex(84)), fwc$(base% + 12)       , ch(04),~
                                                                         ~
               at (09,16), fac(hex(84)), ractv$(base% + 01)     , ch(04),~
               at (10,16), fac(hex(84)), ractv$(base% + 02)     , ch(04),~
               at (11,16), fac(hex(84)), ractv$(base% + 03)     , ch(04),~
               at (12,16), fac(hex(84)), ractv$(base% + 04)     , ch(04),~
               at (13,16), fac(hex(84)), ractv$(base% + 05)     , ch(04),~
               at (14,16), fac(hex(84)), ractv$(base% + 06)     , ch(04),~
               at (15,16), fac(hex(84)), ractv$(base% + 07)     , ch(04),~
               at (16,16), fac(hex(84)), ractv$(base% + 08)     , ch(04),~
               at (17,16), fac(hex(84)), ractv$(base% + 09)     , ch(04),~
               at (18,16), fac(hex(84)), ractv$(base% + 10)     , ch(04),~
               at (19,16), fac(hex(84)), ractv$(base% + 11)     , ch(04),~
               at (20,16), fac(hex(84)), ractv$(base% + 12)     , ch(04),~
                                                                         ~
               at (09,21), fac(hex(84)), rdate$(base% + 01)     , ch(08),~
               at (10,21), fac(hex(84)), rdate$(base% + 02)     , ch(08),~
               at (11,21), fac(hex(84)), rdate$(base% + 03)     , ch(08),~
               at (12,21), fac(hex(84)), rdate$(base% + 04)     , ch(08),~
               at (13,21), fac(hex(84)), rdate$(base% + 05)     , ch(08),~
               at (14,21), fac(hex(84)), rdate$(base% + 06)     , ch(08),~
               at (15,21), fac(hex(84)), rdate$(base% + 07)     , ch(08),~
               at (16,21), fac(hex(84)), rdate$(base% + 08)     , ch(08),~
               at (17,21), fac(hex(84)), rdate$(base% + 09)     , ch(08),~
               at (18,21), fac(hex(84)), rdate$(base% + 10)     , ch(08),~
               at (19,21), fac(hex(84)), rdate$(base% + 11)     , ch(08),~
               at (20,21), fac(hex(84)), rdate$(base% + 12)     , ch(08),~
                                                                         ~
               at (09,30), fac(hex(84)), rtime$(base% + 01)     , ch(05),~
               at (10,30), fac(hex(84)), rtime$(base% + 02)     , ch(05),~
               at (11,30), fac(hex(84)), rtime$(base% + 03)     , ch(05),~
               at (12,30), fac(hex(84)), rtime$(base% + 04)     , ch(05),~
               at (13,30), fac(hex(84)), rtime$(base% + 05)     , ch(05),~
               at (14,30), fac(hex(84)), rtime$(base% + 06)     , ch(05),~
               at (15,30), fac(hex(84)), rtime$(base% + 07)     , ch(05),~
               at (16,30), fac(hex(84)), rtime$(base% + 08)     , ch(05),~
               at (17,30), fac(hex(84)), rtime$(base% + 09)     , ch(05),~
               at (18,30), fac(hex(84)), rtime$(base% + 10)     , ch(05),~
               at (19,30), fac(hex(84)), rtime$(base% + 11)     , ch(05),~
               at (20,30), fac(hex(84)), rtime$(base% + 12)     , ch(05),~
                                                                         ~
               at (09,36), fac(hex(84)), rscrn$(01)             , ch(45),~
               at (10,36), fac(hex(84)), rscrn$(02)             , ch(45),~
               at (11,36), fac(hex(84)), rscrn$(03)             , ch(45),~
               at (12,36), fac(hex(84)), rscrn$(04)             , ch(45),~
               at (13,36), fac(hex(84)), rscrn$(05)             , ch(45),~
               at (14,36), fac(hex(84)), rscrn$(06)             , ch(45),~
               at (15,36), fac(hex(84)), rscrn$(07)             , ch(45),~
               at (16,36), fac(hex(84)), rscrn$(08)             , ch(45),~
               at (17,36), fac(hex(84)), rscrn$(09)             , ch(45),~
               at (18,36), fac(hex(84)), rscrn$(10)             , ch(45),~
               at (19,36), fac(hex(84)), rscrn$(11)             , ch(45),~
               at (20,36), fac(hex(84)), rscrn$(12)             , ch(45),~
                                                                         ~
               at (22,02), fac(hex(ac)), message$               , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (herehit%)

           if herehit% = 16 then L65000
           if herehit% <> 8 then L21710
                call "RTEDSPLY" (part$, rteused$, #5, #3)
                goto L20530
L21710:    if herehit% <> 9 then L21810
                if str(pfdescr$(2),41,14) <> "(9)Memo Text" then L20250
                for loop% = base% + 1 to min(maxlines%-1, base%+12)
                     m% = loop% - base%
                     rscrn$(m%) = "       " & rtext$(loop%)
                next loop%
                str(pfdescr$(2),41,14) = "(9)See Numbers"
                str(header$,34) = "               -- Memo Text --"
                subheader$ = " "
                goto L20530
L21810:    if herehit% <> 15 then L21840
                call "PRNTSCRN"
                goto L20530
L21840:    if herehit% = 2 then base% = 0%
           if herehit% = 3 then base% = maxlines%-9
           if herehit% = 4 then base% = base%-9
           if herehit% = 5 then base% = base%+9
           if herehit% = 6 then base% = base%-1
           if herehit% = 7 then base% = base%+1
           base%=max(0%,min(base%,maxlines%-12))
           goto L20080

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
