        rem**************************************************************~
            *                                                           *~
            *  rrrr   ttttt  eeeee  dddd    sss   pppp   l      y   y   *~
            *  r   r    t    e      d   d  s      p   p  l      y   y   *~
            *  rrrr     t    eeee   d   d   sss   pppp   l       yyy    *~
            *  r   r    t    e      d   d      s  p      l        y     *~
            *  r   r    t    eeeee  dddd    sss   p      lllll    y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * rtedsply - subroutine allows user to review passed in     *~
            *            production routing.                            *~
            *-----------------------------------------------------------*~
            *                  m o d i f i c a t i o n s                *~
            *---when---+----------------what----------------------+-who-*~
            * 11/27/85 ! original                                 ! hes *~
            * 09/18/86 ! rte file format change                   ! hes *~
            * 01/23/87 ! convert units to hours for display       ! hes *~
            * 10/19/95 ! corrected subtle unix problem.           ! jdh *~
            * 06/10/97 ! corrected subtle nt problem.             ! kab *~
            *************************************************************

            sub "RTEDSPLY" (part$, rteid$, #1, #2)

        dim                                                              ~
            actcode$(100)4,              /* STEP'S ACTIVITY CODE       */~
            activity$(100)30,            /* ACTIVITY CODE DESCRIPTION  */~
            cost$(100)6,                 /* ROUTE STEP'S COST          */~
            cwc$(100,3)4,                /* CONCURRENT WORK CENTERS    */~
            date$8,                      /* TODAY'S CLOCK DATE         */~
            message$79,                  /* SRCEEN MESSAGE LINE        */~
            mq$(100)3,                   /* ROUTE STEP'S MOQ TIME      */~
            mqp$(100)1,                  /*                            */~
            nf$(100,3)6,                 /* CONCURRENT NORMALIZATION FA*/~
            oldid$3,                     /* LAST ROUTE DONE            */~
            oldpart$25,                  /* LAST PART DONE             */~
            part$25,                     /* PART TO DO RTE FOR         */~
            partdescr$34,                /* ASSEMBLY PART NUMBER       */~
            pfdescr$(2)79,               /* FUNCTION KEY DESCRIPTIONS  */~
            readkey$79,                  /* KEY TO PLOW FILE WITH      */~
            rteid$3,                     /* ROUTE THIS BOM USES        */~
            run$(100)6,                  /* ROUTE STEP'S RUN TIME      */~
            shift$(100)6,                /*                            */~
            step$(100)4,                 /* STEP NUMBERS FOR ROUTE DISP*/~
            su$(100)6,                   /* ROUTE STEP'S SET UP TIME   */~
            title$79,                    /* SCREEN TITLE               */~
            title1$79,                   /* SCREEN TITLE FOR COMCURRNTS*/~
            unitype$1,                   /* DISPLAY MODE               */~
            wc$(100)4,                   /* ROUTE STEP'S WORK CENTER   */~
            yield$(100)6                 /* ROUTE STEP'S YIELD         */

        dim f1%(2)                       /* RECORD-ON-FILE FLAGS       */

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
            * # 1 ! RTEMASTR ! STANDARD ROUTING FILE W/ALTERNATE ROUTES *~
            * # 2 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 3 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            *************************************************************

            select #3,  "WCMASTR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2  , keylen = 5

            if open% = 0 then call "OPENCHCK" (#3, open%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            title$ = "Step   WC   MQD  SETUP RUN/PT  %CMPL %YIELD CODE/AC~
        ~TIVITY TO BE PERFORMED      "
            title1$= "Step   WC   MQD  SETUP RUN/PT  MQ  SHIFT    CONCURR~
        ~ENT W/C'S AND UNITS FACTORS "

L10000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA PASSED IN.                                     *~
            *************************************************************

            if part$<>" " and oldpart$=part$ and oldid$=rteid$ then L40000
            readkey$ = str(part$) & str(rteid$)
            call "PLOWNEXT" (#1, readkey$, 28%, f1%(1))
                if f1%(1) = 0% then L65000
            oldpart$ = readkey$
            oldid$ = str(readkey$,26)
            partdescr$ = " "
            call "GETCODE" (#2, oldpart$, partdescr$, 1%, 99, f1%(2))

        REM *************************************************************~
            *                L O A D   R O U T I N G                    *~
            * --------------------------------------------------------- *~
            * RETRIEVES INDICATED ROUTING FROM DISK.                    *~
            *************************************************************

            wc$(), cwc$(), mq$(), su$(), run$(), cost$(), mqp$(), nf$(), ~
            activity$(), actcode$(), yield$(), shift$(), step$() = " "

            routes%, c% = 0
            readkey$ = str(oldpart$) & str(oldid$) & hex(0000000000)
L30110:     call "PLOWNEXT" (#1, readkey$, 28%, f1%(1))
                     if f1%(1) = 0 then L40000
            routes%, c% = c% + 1
            if c% = 1 then call "SHOSTAT" ("Loading Route For Display")
            get #1, using L30460, wc$(c%), mq, su, run, cwc$(c%,1), nm1,  ~
                    cwc$(c%,2), nm2, cwc$(c%,3), nm3, shift, mqp$(c%),   ~
                    yield, step$(c%), cost, actcode$(c%), activity$(c%)

            nm1 = (nm1 * su) + (nm1 * run)
            nm2 = (nm2 * su) + (nm2 * run)
            nm3 = (nm3 * su) + (nm3 * run)
            if unitype$ = "U" then L30250
            call "WCUN2HRS" (#3, wc$(c%), 0, su, " ")
            call "WCUN2HRS" (#3, wc$(c%), 0, run, " ")
L30250:     call "CONVERT" (mq,    0.0, mq$(c%))
            call "CONVERT" (su,    0.2, su$(c%))
            call "CONVERT" (run,   0.4, run$(c%))
            call "CONVERT" (cost,  0.0, cost$(c%))
            call "CONVERT" (yield, 0.0, yield$(c%))
            call "CONVERT" (round(shift,4), 4.4, shift$(c%))
            if cwc$(c%,1) = " " then L30350
               if unitype$ = "U" then L30340
               call "WCUN2HRS" (#3, cwc$(c%,1), 0, nm1, " ")
L30340:        call "CONVERT" (nm1, -0.4, nf$(c%,1))
L30350:     if cwc$(c%,2) = " " then L30390
               if unitype$ = "U" then L30380
               call "WCUN2HRS" (#3, cwc$(c%,2), 0, nm2, " ")
L30380:        call "CONVERT" (nm2, -0.4, nf$(c%,2))
L30390:     if cwc$(c%,3) = " " then L30430
               if unitype$ = "U" then L30420
               call "WCUN2HRS" (#3, cwc$(c%,3), 0, nm3, " ")
L30420:        call "CONVERT" (nm3, -0.4, nf$(c%,3))
L30430:     if c% = 100 then L40000
            goto L30110

L30460:     FMT CH(04),                  /* WORK CENTER                */~
                XX(28),                  /*       PART+ROUTE           */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                BI(4),                   /* MOVE QUEUE TIME IN DAYS    */~
                BI(2),                   /* SET UP TIME IN HOURS       */~
                PD(14,4),                /* RUN TIME IN HOURS          */~
                CH(4),                   /* CONCURRENT WC ONE          */~
                PD(7,4),                 /* WC NORMALIZATION FACTOR    */~
                CH(4),                   /* CONCURRENT WC TWO          */~
                PD(7,4),                 /* WC NORMALIZATION FACTOR    */~
                CH(4),                   /* CONCURRENT WC THREE        */~
                PD(7,4),                 /* WC NORMALIZATION FACTOR    */~
                PD(7,6),                 /* WC SHIFT DIFERENTIAL       */~
                CH(1),                   /* MOVE QUE OPTION            */~
                BI(1),                   /* PROCESS YIELD EXPECTED %   */~
                XX(8),                   /* Effective yield (internal) */~
                CH(4),                   /* STEP CODE                  */~
                BI(1),                   /* % OF COSTS INCURRED TO HERE*/~
                CH(4),                   /* ACTIVITY CODE              */~
                CH(30),                  /* ACTIVITY PERFORMED         */~
                CH(4),                   /* Header Text ID             */~
                CH(4)                    /* Line Text ID               */~

L40000: REM *************************************************************~
            *             S H O W   T H E   R O U T I N G               *~
            * --------------------------------------------------------- *~
            * SCREEN FOR REVIEWING THE INDICATED ROUTING.               *~
            *************************************************************

            message$ = "Setup And Run Times Are Displayed In Clock Hours"
            if unitype$ = "U" then str(message$,38) = "Work Center Units"
            if routes% > 0% then L40090 else L65000 /*How subtle can it be*/
L40090:     line% = 0%
            readkey$ = hex(8c)& "ROUTING" &hex(84)& str(oldid$) &hex(8c)
            readkey$ = readkey$ & "FOR PART:" &hex(84)& oldpart$
            readkey$ = readkey$ &hex(8c)& partdescr$
            call "STRING" addr ("CT", readkey$, 79%)

        show_again
            pfdescr$(1) = "(2)First Steps  (4)Prev Steps   (6)Down One   ~
        ~(8)Concurrents   (15)Print Screen"
            pfdescr$(2) = "(3)Last Steps   (5)Next Steps   (7)Up One    (~
        ~10)Display In Units    (16)Return"

            if unitype$ = "U" then str(pfdescr$(2),61,5) = "Hours"
            if line% = 0 then str(pfdescr$(1),,45) = " "
            if line% >= routes%-17 then str(pfdescr$(2),,45) = " "

L40250:     accept                                                       ~
               at (01,20), "-- Production Routing Display And Review --",~
               at (02,02), fac(hex(84)), readkey$               , ch(79),~
               at (04,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), step$    (line%+ 1%)    , ch(04),~
               at (06,02), fac(hex(8c)), step$    (line%+ 2%)    , ch(04),~
               at (07,02), fac(hex(8c)), step$    (line%+ 3%)    , ch(04),~
               at (08,02), fac(hex(8c)), step$    (line%+ 4%)    , ch(04),~
               at (09,02), fac(hex(8c)), step$    (line%+ 5%)    , ch(04),~
               at (10,02), fac(hex(8c)), step$    (line%+ 6%)    , ch(04),~
               at (11,02), fac(hex(8c)), step$    (line%+ 7%)    , ch(04),~
               at (12,02), fac(hex(8c)), step$    (line%+ 8%)    , ch(04),~
               at (13,02), fac(hex(8c)), step$    (line%+ 9%)    , ch(04),~
               at (14,02), fac(hex(8c)), step$    (line%+10%)    , ch(04),~
               at (15,02), fac(hex(8c)), step$    (line%+11%)    , ch(04),~
               at (16,02), fac(hex(8c)), step$    (line%+12%)    , ch(04),~
               at (17,02), fac(hex(8c)), step$    (line%+13%)    , ch(04),~
               at (18,02), fac(hex(8c)), step$    (line%+14%)    , ch(04),~
               at (19,02), fac(hex(8c)), step$    (line%+15%)    , ch(04),~
               at (20,02), fac(hex(8c)), step$    (line%+16%)    , ch(04),~
               at (21,02), fac(hex(8c)), step$    (line%+17%)    , ch(04),~
                                                                         ~
               at (05,08), fac(hex(84)), wc$      (line%+ 1%)    , ch(04),~
               at (06,08), fac(hex(84)), wc$      (line%+ 2%)    , ch(04),~
               at (07,08), fac(hex(84)), wc$      (line%+ 3%)    , ch(04),~
               at (08,08), fac(hex(84)), wc$      (line%+ 4%)    , ch(04),~
               at (09,08), fac(hex(84)), wc$      (line%+ 5%)    , ch(04),~
               at (10,08), fac(hex(84)), wc$      (line%+ 6%)    , ch(04),~
               at (11,08), fac(hex(84)), wc$      (line%+ 7%)    , ch(04),~
               at (12,08), fac(hex(84)), wc$      (line%+ 8%)    , ch(04),~
               at (13,08), fac(hex(84)), wc$      (line%+ 9%)    , ch(04),~
               at (14,08), fac(hex(84)), wc$      (line%+10%)    , ch(04),~
               at (15,08), fac(hex(84)), wc$      (line%+11%)    , ch(04),~
               at (16,08), fac(hex(84)), wc$      (line%+12%)    , ch(04),~
               at (17,08), fac(hex(84)), wc$      (line%+13%)    , ch(04),~
               at (18,08), fac(hex(84)), wc$      (line%+14%)    , ch(04),~
               at (19,08), fac(hex(84)), wc$      (line%+15%)    , ch(04),~
               at (20,08), fac(hex(84)), wc$      (line%+16%)    , ch(04),~
               at (21,08), fac(hex(84)), wc$      (line%+17%)    , ch(04),~
                                                                         ~
               at (05,14), fac(hex(84)), mq$      (line%+ 1%)    , ch(03),~
               at (06,14), fac(hex(84)), mq$      (line%+ 2%)    , ch(03),~
               at (07,14), fac(hex(84)), mq$      (line%+ 3%)    , ch(03),~
               at (08,14), fac(hex(84)), mq$      (line%+ 4%)    , ch(03),~
               at (09,14), fac(hex(84)), mq$      (line%+ 5%)    , ch(03),~
               at (10,14), fac(hex(84)), mq$      (line%+ 6%)    , ch(03),~
               at (11,14), fac(hex(84)), mq$      (line%+ 7%)    , ch(03),~
               at (12,14), fac(hex(84)), mq$      (line%+ 8%)    , ch(03),~
               at (13,14), fac(hex(84)), mq$      (line%+ 9%)    , ch(03),~
               at (14,14), fac(hex(84)), mq$      (line%+10%)    , ch(03),~
               at (15,14), fac(hex(84)), mq$      (line%+11%)    , ch(03),~
               at (16,14), fac(hex(84)), mq$      (line%+12%)    , ch(03),~
               at (17,14), fac(hex(84)), mq$      (line%+13%)    , ch(03),~
               at (18,14), fac(hex(84)), mq$      (line%+14%)    , ch(03),~
               at (19,14), fac(hex(84)), mq$      (line%+15%)    , ch(03),~
               at (20,14), fac(hex(84)), mq$      (line%+16%)    , ch(03),~
               at (21,14), fac(hex(84)) ,mq$      (line%+17%)    , ch(03),~
                                                                         ~
               at (05,18), fac(hex(84)), su$      (line%+ 1%)    , ch(06),~
               at (06,18), fac(hex(84)), su$      (line%+ 2%)    , ch(06),~
               at (07,18), fac(hex(84)), su$      (line%+ 3%)    , ch(06),~
               at (08,18), fac(hex(84)), su$      (line%+ 4%)    , ch(06),~
               at (09,18), fac(hex(84)), su$      (line%+ 5%)    , ch(06),~
               at (10,18), fac(hex(84)), su$      (line%+ 6%)    , ch(06),~
               at (11,18), fac(hex(84)), su$      (line%+ 7%)    , ch(06),~
               at (12,18), fac(hex(84)), su$      (line%+ 8%)    , ch(06),~
               at (13,18), fac(hex(84)), su$      (line%+ 9%)    , ch(06),~
               at (14,18), fac(hex(84)), su$      (line%+10%)    , ch(06),~
               at (15,18), fac(hex(84)), su$      (line%+11%)    , ch(06),~
               at (16,18), fac(hex(84)), su$      (line%+12%)    , ch(06),~
               at (17,18), fac(hex(84)), su$      (line%+13%)    , ch(06),~
               at (18,18), fac(hex(84)), su$      (line%+14%)    , ch(06),~
               at (19,18), fac(hex(84)), su$      (line%+15%)    , ch(06),~
               at (20,18), fac(hex(84)), su$      (line%+16%)    , ch(06),~
               at (21,18), fac(hex(84)), su$      (line%+17%)    , ch(06),~
                                                                         ~
               at (05,25), fac(hex(84)), run$     (line%+ 1%)    , ch(06),~
               at (06,25), fac(hex(84)), run$     (line%+ 2%)    , ch(06),~
               at (07,25), fac(hex(84)), run$     (line%+ 3%)    , ch(06),~
               at (08,25), fac(hex(84)), run$     (line%+ 4%)    , ch(06),~
               at (09,25), fac(hex(84)), run$     (line%+ 5%)    , ch(06),~
               at (10,25), fac(hex(84)), run$     (line%+ 6%)    , ch(06),~
               at (11,25), fac(hex(84)), run$     (line%+ 7%)    , ch(06),~
               at (12,25), fac(hex(84)), run$     (line%+ 8%)    , ch(06),~
               at (13,25), fac(hex(84)), run$     (line%+ 9%)    , ch(06),~
               at (14,25), fac(hex(84)), run$     (line%+10%)    , ch(06),~
               at (15,25), fac(hex(84)), run$     (line%+11%)    , ch(06),~
               at (16,25), fac(hex(84)), run$     (line%+12%)    , ch(06),~
               at (17,25), fac(hex(84)), run$     (line%+13%)    , ch(06),~
               at (18,25), fac(hex(84)), run$     (line%+14%)    , ch(06),~
               at (19,25), fac(hex(84)), run$     (line%+15%)    , ch(06),~
               at (20,25), fac(hex(84)), run$     (line%+16%)    , ch(06),~
               at (21,25), fac(hex(84)), run$     (line%+17%)    , ch(06),~
                                                                         ~
               at (05,32), fac(hex(84)), cost$    (line%+ 1%)    , ch(06),~
               at (06,32), fac(hex(84)), cost$    (line%+ 2%)    , ch(06),~
               at (07,32), fac(hex(84)), cost$    (line%+ 3%)    , ch(06),~
               at (08,32), fac(hex(84)), cost$    (line%+ 4%)    , ch(06),~
               at (09,32), fac(hex(84)), cost$    (line%+ 5%)    , ch(06),~
               at (10,32), fac(hex(84)), cost$    (line%+ 6%)    , ch(06),~
               at (11,32), fac(hex(84)), cost$    (line%+ 7%)    , ch(06),~
               at (12,32), fac(hex(84)), cost$    (line%+ 8%)    , ch(06),~
               at (13,32), fac(hex(84)), cost$    (line%+ 9%)    , ch(06),~
               at (14,32), fac(hex(84)), cost$    (line%+10%)    , ch(06),~
               at (15,32), fac(hex(84)), cost$    (line%+11%)    , ch(06),~
               at (16,32), fac(hex(84)), cost$    (line%+12%)    , ch(06),~
               at (17,32), fac(hex(84)), cost$    (line%+13%)    , ch(06),~
               at (18,32), fac(hex(84)), cost$    (line%+14%)    , ch(06),~
               at (19,32), fac(hex(84)), cost$    (line%+15%)    , ch(06),~
               at (20,32), fac(hex(84)), cost$    (line%+16%)    , ch(06),~
               at (21,32), fac(hex(84)) ,cost$    (line%+17%)    , ch(06),~
                                                                         ~
               at (05,39), fac(hex(84)), yield$   (line%+ 1%)    , ch(06),~
               at (06,39), fac(hex(84)), yield$   (line%+ 2%)    , ch(06),~
               at (07,39), fac(hex(84)), yield$   (line%+ 3%)    , ch(06),~
               at (08,39), fac(hex(84)), yield$   (line%+ 4%)    , ch(06),~
               at (09,39), fac(hex(84)), yield$   (line%+ 5%)    , ch(06),~
               at (10,39), fac(hex(84)), yield$   (line%+ 6%)    , ch(06),~
               at (11,39), fac(hex(84)), yield$   (line%+ 7%)    , ch(06),~
               at (12,39), fac(hex(84)), yield$   (line%+ 8%)    , ch(06),~
               at (13,39), fac(hex(84)), yield$   (line%+ 9%)    , ch(06),~
               at (14,39), fac(hex(84)), yield$   (line%+10%)    , ch(06),~
               at (15,39), fac(hex(84)), yield$   (line%+11%)    , ch(06),~
               at (16,39), fac(hex(84)), yield$   (line%+12%)    , ch(06),~
               at (17,39), fac(hex(84)), yield$   (line%+13%)    , ch(06),~
               at (18,39), fac(hex(84)), yield$   (line%+14%)    , ch(06),~
               at (19,39), fac(hex(84)), yield$   (line%+15%)    , ch(06),~
               at (20,39), fac(hex(84)), yield$   (line%+16%)    , ch(06),~
               at (21,39), fac(hex(84)), yield$   (line%+17%)    , ch(06),~
                                                                         ~
               at (05,46), fac(hex(84)), actcode$ (line%+ 1%)    , ch(04),~
               at (06,46), fac(hex(84)), actcode$ (line%+ 2%)    , ch(04),~
               at (07,46), fac(hex(84)), actcode$ (line%+ 3%)    , ch(04),~
               at (08,46), fac(hex(84)), actcode$ (line%+ 4%)    , ch(04),~
               at (09,46), fac(hex(84)), actcode$ (line%+ 5%)    , ch(04),~
               at (10,46), fac(hex(84)), actcode$ (line%+ 6%)    , ch(04),~
               at (11,46), fac(hex(84)), actcode$ (line%+ 7%)    , ch(04),~
               at (12,46), fac(hex(84)), actcode$ (line%+ 8%)    , ch(04),~
               at (13,46), fac(hex(84)), actcode$ (line%+ 9%)    , ch(04),~
               at (14,46), fac(hex(84)), actcode$ (line%+10%)    , ch(04),~
               at (15,46), fac(hex(84)), actcode$ (line%+11%)    , ch(04),~
               at (16,46), fac(hex(84)), actcode$ (line%+12%)    , ch(04),~
               at (17,46), fac(hex(84)), actcode$ (line%+13%)    , ch(04),~
               at (18,46), fac(hex(84)), actcode$ (line%+14%)    , ch(04),~
               at (19,46), fac(hex(84)), actcode$ (line%+15%)    , ch(04),~
               at (20,46), fac(hex(84)), actcode$ (line%+16%)    , ch(04),~
               at (21,46), fac(hex(84)), actcode$ (line%+17%)    , ch(04),~
                                                                         ~
               at (05,51), fac(hex(84)), activity$(line%+ 1%)    , ch(30),~
               at (06,51), fac(hex(84)), activity$(line%+ 2%)    , ch(30),~
               at (07,51), fac(hex(84)), activity$(line%+ 3%)    , ch(30),~
               at (08,51), fac(hex(84)), activity$(line%+ 4%)    , ch(30),~
               at (09,51), fac(hex(84)), activity$(line%+ 5%)    , ch(30),~
               at (10,51), fac(hex(84)), activity$(line%+ 6%)    , ch(30),~
               at (11,51), fac(hex(84)), activity$(line%+ 7%)    , ch(30),~
               at (12,51), fac(hex(84)), activity$(line%+ 8%)    , ch(30),~
               at (13,51), fac(hex(84)), activity$(line%+ 9%)    , ch(30),~
               at (14,51), fac(hex(84)), activity$(line%+10%)    , ch(30),~
               at (15,51), fac(hex(84)), activity$(line%+11%)    , ch(30),~
               at (16,51), fac(hex(84)), activity$(line%+12%)    , ch(30),~
               at (17,51), fac(hex(84)), activity$(line%+13%)    , ch(30),~
               at (18,51), fac(hex(84)), activity$(line%+14%)    , ch(30),~
               at (19,51), fac(hex(84)), activity$(line%+15%)    , ch(30),~
               at (20,51), fac(hex(84)), activity$(line%+16%)    , ch(30),~
               at (21,51), fac(hex(84)), activity$(line%+17%)    , ch(30),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$               , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
                                                                         ~
               keys (hex(020304050607080a0d0f10)),                       ~
               key  (key%)

               if key% <> 10 then L42040
                  if unitype$=" " then unitype$="U" else unitype$=" "
                  oldpart$ = hex(ff)
                  goto L10000

L42040:        if key% <> 13 then L42080
                  call "MANUAL" ("RTEDSPLY")
                  goto L40250

L42080:        if key% <> 15 then L42120
                  call "PRNTSCRN"
                  goto L40250

L42120:     if key% = 16% then L65000
                  if key%  =  2 then line% = 0
                  if key%  =  3 then line% = max(0, routes% - 17)
                  if key%  =  4 then line% = max(0, line% - 15)
                  if key%  =  5 then line% = min(line% + 15,             ~
                                                  max(0, routes% - 17))
                  if key%  =  6 then line% = max(0, line% - 1)
                  if key%  =  7 then line% = min(line% + 1,              ~
                                                  max(0, routes% - 17))
                  if key%  =  8 then L43000
                  goto show_again

L43000: REM *************************************************************~
            *             S H O W   T H E   R O U T I N G               *~
            * --------------------------------------------------------- *~
            * SCREEN FOR REVIEWING ROUTES CONCURRENT CENTERS.           *~
            *************************************************************

        show_concurrent
            pfdescr$(1) = "(2)First Steps  (4)Prev Steps   (6)Down One   ~
        ~(8)Primary Cntrs (15)Print Screen"
            pfdescr$(2) = "(3)Last Steps   (5)Next Steps   (7)Up One     ~
        ~                       (16)Return"

            if line% = 0 then str(pfdescr$(1),,45) = " "
            if line% >= routes%-17 then str(pfdescr$(2),,45) = " "
L43200:     accept                                                       ~
               at (01,20), "-- Production Routing Display And Review --",~
               at (02,02), fac(hex(84)), readkey$               , ch(79),~
               at (04,02), fac(hex(ac)), title1$                , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), step$    (line%+ 1%)    , ch(04),~
               at (06,02), fac(hex(8c)), step$    (line%+ 2%)    , ch(04),~
               at (07,02), fac(hex(8c)), step$    (line%+ 3%)    , ch(04),~
               at (08,02), fac(hex(8c)), step$    (line%+ 4%)    , ch(04),~
               at (09,02), fac(hex(8c)), step$    (line%+ 5%)    , ch(04),~
               at (10,02), fac(hex(8c)), step$    (line%+ 6%)    , ch(04),~
               at (11,02), fac(hex(8c)), step$    (line%+ 7%)    , ch(04),~
               at (12,02), fac(hex(8c)), step$    (line%+ 8%)    , ch(04),~
               at (13,02), fac(hex(8c)), step$    (line%+ 9%)    , ch(04),~
               at (14,02), fac(hex(8c)), step$    (line%+10%)    , ch(04),~
               at (15,02), fac(hex(8c)), step$    (line%+11%)    , ch(04),~
               at (16,02), fac(hex(8c)), step$    (line%+12%)    , ch(04),~
               at (17,02), fac(hex(8c)), step$    (line%+13%)    , ch(04),~
               at (18,02), fac(hex(8c)), step$    (line%+14%)    , ch(04),~
               at (19,02), fac(hex(8c)), step$    (line%+15%)    , ch(04),~
               at (20,02), fac(hex(8c)), step$    (line%+16%)    , ch(04),~
               at (21,02), fac(hex(8c)), step$    (line%+17%)    , ch(04),~
                                                                         ~
               at (05,08), fac(hex(84)), wc$      (line%+ 1%)    , ch(04),~
               at (06,08), fac(hex(84)), wc$      (line%+ 2%)    , ch(04),~
               at (07,08), fac(hex(84)), wc$      (line%+ 3%)    , ch(04),~
               at (08,08), fac(hex(84)), wc$      (line%+ 4%)    , ch(04),~
               at (09,08), fac(hex(84)), wc$      (line%+ 5%)    , ch(04),~
               at (10,08), fac(hex(84)), wc$      (line%+ 6%)    , ch(04),~
               at (11,08), fac(hex(84)), wc$      (line%+ 7%)    , ch(04),~
               at (12,08), fac(hex(84)), wc$      (line%+ 8%)    , ch(04),~
               at (13,08), fac(hex(84)), wc$      (line%+ 9%)    , ch(04),~
               at (14,08), fac(hex(84)), wc$      (line%+10%)    , ch(04),~
               at (15,08), fac(hex(84)), wc$      (line%+11%)    , ch(04),~
               at (16,08), fac(hex(84)), wc$      (line%+12%)    , ch(04),~
               at (17,08), fac(hex(84)), wc$      (line%+13%)    , ch(04),~
               at (18,08), fac(hex(84)), wc$      (line%+14%)    , ch(04),~
               at (19,08), fac(hex(84)), wc$      (line%+15%)    , ch(04),~
               at (20,08), fac(hex(84)), wc$      (line%+16%)    , ch(04),~
               at (21,08), fac(hex(84)), wc$      (line%+17%)    , ch(04),~
                                                                         ~
               at (05,14), fac(hex(84)), mq$      (line%+ 1%)    , ch(03),~
               at (06,14), fac(hex(84)), mq$      (line%+ 2%)    , ch(03),~
               at (07,14), fac(hex(84)), mq$      (line%+ 3%)    , ch(03),~
               at (08,14), fac(hex(84)), mq$      (line%+ 4%)    , ch(03),~
               at (09,14), fac(hex(84)), mq$      (line%+ 5%)    , ch(03),~
               at (10,14), fac(hex(84)), mq$      (line%+ 6%)    , ch(03),~
               at (11,14), fac(hex(84)), mq$      (line%+ 7%)    , ch(03),~
               at (12,14), fac(hex(84)), mq$      (line%+ 8%)    , ch(03),~
               at (13,14), fac(hex(84)), mq$      (line%+ 9%)    , ch(03),~
               at (14,14), fac(hex(84)), mq$      (line%+10%)    , ch(03),~
               at (15,14), fac(hex(84)), mq$      (line%+11%)    , ch(03),~
               at (16,14), fac(hex(84)), mq$      (line%+12%)    , ch(03),~
               at (17,14), fac(hex(84)), mq$      (line%+13%)    , ch(03),~
               at (18,14), fac(hex(84)), mq$      (line%+14%)    , ch(03),~
               at (19,14), fac(hex(84)), mq$      (line%+15%)    , ch(03),~
               at (20,14), fac(hex(84)), mq$      (line%+16%)    , ch(03),~
               at (21,14), fac(hex(84)) ,mq$      (line%+17%)    , ch(03),~
                                                                         ~
               at (05,18), fac(hex(84)), su$      (line%+ 1%)    , ch(06),~
               at (06,18), fac(hex(84)), su$      (line%+ 2%)    , ch(06),~
               at (07,18), fac(hex(84)), su$      (line%+ 3%)    , ch(06),~
               at (08,18), fac(hex(84)), su$      (line%+ 4%)    , ch(06),~
               at (09,18), fac(hex(84)), su$      (line%+ 5%)    , ch(06),~
               at (10,18), fac(hex(84)), su$      (line%+ 6%)    , ch(06),~
               at (11,18), fac(hex(84)), su$      (line%+ 7%)    , ch(06),~
               at (12,18), fac(hex(84)), su$      (line%+ 8%)    , ch(06),~
               at (13,18), fac(hex(84)), su$      (line%+ 9%)    , ch(06),~
               at (14,18), fac(hex(84)), su$      (line%+10%)    , ch(06),~
               at (15,18), fac(hex(84)), su$      (line%+11%)    , ch(06),~
               at (16,18), fac(hex(84)), su$      (line%+12%)    , ch(06),~
               at (17,18), fac(hex(84)), su$      (line%+13%)    , ch(06),~
               at (18,18), fac(hex(84)), su$      (line%+14%)    , ch(06),~
               at (19,18), fac(hex(84)), su$      (line%+15%)    , ch(06),~
               at (20,18), fac(hex(84)), su$      (line%+16%)    , ch(06),~
               at (21,18), fac(hex(84)), su$      (line%+17%)    , ch(06),~
                                                                         ~
               at (05,25), fac(hex(84)), run$     (line%+ 1%)    , ch(06),~
               at (06,25), fac(hex(84)), run$     (line%+ 2%)    , ch(06),~
               at (07,25), fac(hex(84)), run$     (line%+ 3%)    , ch(06),~
               at (08,25), fac(hex(84)), run$     (line%+ 4%)    , ch(06),~
               at (09,25), fac(hex(84)), run$     (line%+ 5%)    , ch(06),~
               at (10,25), fac(hex(84)), run$     (line%+ 6%)    , ch(06),~
               at (11,25), fac(hex(84)), run$     (line%+ 7%)    , ch(06),~
               at (12,25), fac(hex(84)), run$     (line%+ 8%)    , ch(06),~
               at (13,25), fac(hex(84)), run$     (line%+ 9%)    , ch(06),~
               at (14,25), fac(hex(84)), run$     (line%+10%)    , ch(06),~
               at (15,25), fac(hex(84)), run$     (line%+11%)    , ch(06),~
               at (16,25), fac(hex(84)), run$     (line%+12%)    , ch(06),~
               at (17,25), fac(hex(84)), run$     (line%+13%)    , ch(06),~
               at (18,25), fac(hex(84)), run$     (line%+14%)    , ch(06),~
               at (19,25), fac(hex(84)), run$     (line%+15%)    , ch(06),~
               at (20,25), fac(hex(84)), run$     (line%+16%)    , ch(06),~
               at (21,25), fac(hex(84)), run$     (line%+17%)    , ch(06),~
                                                                         ~
               at (05,34), fac(hex(84)), mqp$     (line%+ 1%)    , ch(01),~
               at (06,34), fac(hex(84)), mqp$     (line%+ 2%)    , ch(01),~
               at (07,34), fac(hex(84)), mqp$     (line%+ 3%)    , ch(01),~
               at (08,34), fac(hex(84)), mqp$     (line%+ 4%)    , ch(01),~
               at (09,34), fac(hex(84)), mqp$     (line%+ 5%)    , ch(01),~
               at (10,34), fac(hex(84)), mqp$     (line%+ 6%)    , ch(01),~
               at (11,34), fac(hex(84)), mqp$     (line%+ 7%)    , ch(01),~
               at (12,34), fac(hex(84)), mqp$     (line%+ 8%)    , ch(01),~
               at (13,34), fac(hex(84)), mqp$     (line%+ 9%)    , ch(01),~
               at (14,34), fac(hex(84)), mqp$     (line%+10%)    , ch(01),~
               at (15,34), fac(hex(84)), mqp$     (line%+11%)    , ch(01),~
               at (16,34), fac(hex(84)), mqp$     (line%+12%)    , ch(01),~
               at (17,34), fac(hex(84)), mqp$     (line%+13%)    , ch(01),~
               at (18,34), fac(hex(84)), mqp$     (line%+14%)    , ch(01),~
               at (19,34), fac(hex(84)), mqp$     (line%+15%)    , ch(01),~
               at (20,34), fac(hex(84)), mqp$     (line%+16%)    , ch(01),~
               at (21,34), fac(hex(84)), mqp$     (line%+17%)    , ch(01),~
                                                                         ~
               at (05,36), fac(hex(84)), shift$   (line%+ 1%)    , ch(06),~
               at (06,36), fac(hex(84)), shift$   (line%+ 2%)    , ch(06),~
               at (07,36), fac(hex(84)), shift$   (line%+ 3%)    , ch(06),~
               at (08,36), fac(hex(84)), shift$   (line%+ 4%)    , ch(06),~
               at (09,36), fac(hex(84)), shift$   (line%+ 5%)    , ch(06),~
               at (10,36), fac(hex(84)), shift$   (line%+ 6%)    , ch(06),~
               at (11,36), fac(hex(84)), shift$   (line%+ 7%)    , ch(06),~
               at (12,36), fac(hex(84)), shift$   (line%+ 8%)    , ch(06),~
               at (13,36), fac(hex(84)), shift$   (line%+ 9%)    , ch(06),~
               at (14,36), fac(hex(84)), shift$   (line%+10%)    , ch(06),~
               at (15,36), fac(hex(84)), shift$   (line%+11%)    , ch(06),~
               at (16,36), fac(hex(84)), shift$   (line%+12%)    , ch(06),~
               at (17,36), fac(hex(84)), shift$   (line%+13%)    , ch(06),~
               at (18,36), fac(hex(84)), shift$   (line%+14%)    , ch(06),~
               at (19,36), fac(hex(84)), shift$   (line%+15%)    , ch(06),~
               at (20,36), fac(hex(84)), shift$   (line%+16%)    , ch(06),~
               at (21,36), fac(hex(84)), shift$   (line%+17%)    , ch(06),~
                                                                         ~
               at (05,46), fac(hex(84)), cwc$     (line%+ 1%, 1%) , ch(04),~
               at (06,46), fac(hex(84)), cwc$     (line%+ 2%, 1%) , ch(04),~
               at (07,46), fac(hex(84)), cwc$     (line%+ 3%, 1%) , ch(04),~
               at (08,46), fac(hex(84)), cwc$     (line%+ 4%, 1%) , ch(04),~
               at (09,46), fac(hex(84)), cwc$     (line%+ 5%, 1%) , ch(04),~
               at (10,46), fac(hex(84)), cwc$     (line%+ 6%, 1%) , ch(04),~
               at (11,46), fac(hex(84)), cwc$     (line%+ 7%, 1%) , ch(04),~
               at (12,46), fac(hex(84)), cwc$     (line%+ 8%, 1%) , ch(04),~
               at (13,46), fac(hex(84)), cwc$     (line%+ 9%, 1%) , ch(04),~
               at (14,46), fac(hex(84)), cwc$     (line%+10%, 1%) , ch(04),~
               at (15,46), fac(hex(84)), cwc$     (line%+11%, 1%) , ch(04),~
               at (16,46), fac(hex(84)), cwc$     (line%+12%, 1%) , ch(04),~
               at (17,46), fac(hex(84)), cwc$     (line%+13%, 1%) , ch(04),~
               at (18,46), fac(hex(84)), cwc$     (line%+14%, 1%) , ch(04),~
               at (19,46), fac(hex(84)), cwc$     (line%+15%, 1%) , ch(04),~
               at (20,46), fac(hex(84)), cwc$     (line%+16%, 1%) , ch(04),~
               at (21,46), fac(hex(84)), cwc$     (line%+17%, 1%) , ch(04),~
                                                                         ~
               at (05,51), fac(hex(84)), nf$      (line%+ 1%, 1%) , ch(06),~
               at (06,51), fac(hex(84)), nf$      (line%+ 2%, 1%) , ch(06),~
               at (07,51), fac(hex(84)), nf$      (line%+ 3%, 1%) , ch(06),~
               at (08,51), fac(hex(84)), nf$      (line%+ 4%, 1%) , ch(06),~
               at (09,51), fac(hex(84)), nf$      (line%+ 5%, 1%) , ch(06),~
               at (10,51), fac(hex(84)), nf$      (line%+ 6%, 1%) , ch(06),~
               at (11,51), fac(hex(84)), nf$      (line%+ 7%, 1%) , ch(06),~
               at (12,51), fac(hex(84)), nf$      (line%+ 8%, 1%) , ch(06),~
               at (13,51), fac(hex(84)), nf$      (line%+ 9%, 1%) , ch(06),~
               at (14,51), fac(hex(84)), nf$      (line%+10%, 1%) , ch(06),~
               at (15,51), fac(hex(84)), nf$      (line%+11%, 1%) , ch(06),~
               at (16,51), fac(hex(84)), nf$      (line%+12%, 1%) , ch(06),~
               at (17,51), fac(hex(84)), nf$      (line%+13%, 1%) , ch(06),~
               at (18,51), fac(hex(84)), nf$      (line%+14%, 1%) , ch(06),~
               at (19,51), fac(hex(84)), nf$      (line%+15%, 1%) , ch(06),~
               at (20,51), fac(hex(84)), nf$      (line%+16%, 1%) , ch(06),~
               at (21,51), fac(hex(84)), nf$      (line%+17%, 1%) , ch(06),~
                                                                         ~
               at (05,58), fac(hex(84)), cwc$     (line%+ 1%, 2%) , ch(04),~
               at (06,58), fac(hex(84)), cwc$     (line%+ 2%, 2%) , ch(04),~
               at (07,58), fac(hex(84)), cwc$     (line%+ 3%, 2%) , ch(04),~
               at (08,58), fac(hex(84)), cwc$     (line%+ 4%, 2%) , ch(04),~
               at (09,58), fac(hex(84)), cwc$     (line%+ 5%, 2%) , ch(04),~
               at (10,58), fac(hex(84)), cwc$     (line%+ 6%, 2%) , ch(04),~
               at (11,58), fac(hex(84)), cwc$     (line%+ 7%, 2%) , ch(04),~
               at (12,58), fac(hex(84)), cwc$     (line%+ 8%, 2%) , ch(04),~
               at (13,58), fac(hex(84)), cwc$     (line%+ 9%, 2%) , ch(04),~
               at (14,58), fac(hex(84)), cwc$     (line%+10%, 2%) , ch(04),~
               at (15,58), fac(hex(84)), cwc$     (line%+11%, 2%) , ch(04),~
               at (16,58), fac(hex(84)), cwc$     (line%+12%, 2%) , ch(04),~
               at (17,58), fac(hex(84)), cwc$     (line%+13%, 2%) , ch(04),~
               at (18,58), fac(hex(84)), cwc$     (line%+14%, 2%) , ch(04),~
               at (19,58), fac(hex(84)), cwc$     (line%+15%, 2%) , ch(04),~
               at (20,58), fac(hex(84)), cwc$     (line%+16%, 2%) , ch(04),~
               at (21,58), fac(hex(84)), cwc$     (line%+17%, 2%) , ch(04),~
                                                                         ~
               at (05,63), fac(hex(84)), nf$      (line%+ 1%, 2%) , ch(06),~
               at (06,63), fac(hex(84)), nf$      (line%+ 2%, 2%) , ch(06),~
               at (07,63), fac(hex(84)), nf$      (line%+ 3%, 2%) , ch(06),~
               at (08,63), fac(hex(84)), nf$      (line%+ 4%, 2%) , ch(06),~
               at (09,63), fac(hex(84)), nf$      (line%+ 5%, 2%) , ch(06),~
               at (10,63), fac(hex(84)), nf$      (line%+ 6%, 2%) , ch(06),~
               at (11,63), fac(hex(84)), nf$      (line%+ 7%, 2%) , ch(06),~
               at (12,63), fac(hex(84)), nf$      (line%+ 8%, 2%) , ch(06),~
               at (13,63), fac(hex(84)), nf$      (line%+ 9%, 2%) , ch(06),~
               at (14,63), fac(hex(84)), nf$      (line%+10%, 2%) , ch(06),~
               at (15,63), fac(hex(84)), nf$      (line%+11%, 2%) , ch(06),~
               at (16,63), fac(hex(84)), nf$      (line%+12%, 2%) , ch(06),~
               at (17,63), fac(hex(84)), nf$      (line%+13%, 2%) , ch(06),~
               at (18,63), fac(hex(84)), nf$      (line%+14%, 2%) , ch(06),~
               at (19,63), fac(hex(84)), nf$      (line%+15%, 2%) , ch(06),~
               at (20,63), fac(hex(84)), nf$      (line%+16%, 2%) , ch(06),~
               at (21,63), fac(hex(84)), nf$      (line%+17%, 2%) , ch(06),~
                                                                         ~
               at (05,70), fac(hex(84)), cwc$     (line%+ 1%, 3%) , ch(04),~
               at (06,70), fac(hex(84)), cwc$     (line%+ 2%, 3%) , ch(04),~
               at (07,70), fac(hex(84)), cwc$     (line%+ 3%, 3%) , ch(04),~
               at (08,70), fac(hex(84)), cwc$     (line%+ 4%, 3%) , ch(04),~
               at (09,70), fac(hex(84)), cwc$     (line%+ 5%, 3%) , ch(04),~
               at (10,70), fac(hex(84)), cwc$     (line%+ 6%, 3%) , ch(04),~
               at (11,70), fac(hex(84)), cwc$     (line%+ 7%, 3%) , ch(04),~
               at (12,70), fac(hex(84)), cwc$     (line%+ 8%, 3%) , ch(04),~
               at (13,70), fac(hex(84)), cwc$     (line%+ 9%, 3%) , ch(04),~
               at (14,70), fac(hex(84)), cwc$     (line%+10%, 3%) , ch(04),~
               at (15,70), fac(hex(84)), cwc$     (line%+11%, 3%) , ch(04),~
               at (16,70), fac(hex(84)), cwc$     (line%+12%, 3%) , ch(04),~
               at (17,70), fac(hex(84)), cwc$     (line%+13%, 3%) , ch(04),~
               at (18,70), fac(hex(84)), cwc$     (line%+14%, 3%) , ch(04),~
               at (19,70), fac(hex(84)), cwc$     (line%+15%, 3%) , ch(04),~
               at (20,70), fac(hex(84)), cwc$     (line%+16%, 3%) , ch(04),~
               at (21,70), fac(hex(84)), cwc$     (line%+17%, 3%) , ch(04),~
                                                                         ~
               at (05,75), fac(hex(84)), nf$      (line%+ 1%, 3%) , ch(06),~
               at (06,75), fac(hex(84)), nf$      (line%+ 2%, 3%) , ch(06),~
               at (07,75), fac(hex(84)), nf$      (line%+ 3%, 3%) , ch(06),~
               at (08,75), fac(hex(84)), nf$      (line%+ 4%, 3%) , ch(06),~
               at (09,75), fac(hex(84)), nf$      (line%+ 5%, 3%) , ch(06),~
               at (10,75), fac(hex(84)), nf$      (line%+ 6%, 3%) , ch(06),~
               at (11,75), fac(hex(84)), nf$      (line%+ 7%, 3%) , ch(06),~
               at (12,75), fac(hex(84)), nf$      (line%+ 8%, 3%) , ch(06),~
               at (13,75), fac(hex(84)), nf$      (line%+ 9%, 3%) , ch(06),~
               at (14,75), fac(hex(84)), nf$      (line%+10%, 3%) , ch(06),~
               at (15,75), fac(hex(84)), nf$      (line%+11%, 3%) , ch(06),~
               at (16,75), fac(hex(84)), nf$      (line%+12%, 3%) , ch(06),~
               at (17,75), fac(hex(84)), nf$      (line%+13%, 3%) , ch(06),~
               at (18,75), fac(hex(84)), nf$      (line%+14%, 3%) , ch(06),~
               at (19,75), fac(hex(84)), nf$      (line%+15%, 3%) , ch(06),~
               at (20,75), fac(hex(84)), nf$      (line%+16%, 3%) , ch(06),~
               at (21,75), fac(hex(84)), nf$      (line%+17%, 3%) , ch(06),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$               , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
                                                                         ~
               keys (hex(020304050607080d0f10)),                         ~
               key  (key%)

               if key% <> 13 then L45700
                  call "MANUAL" ("RTEDSPLY")
                  goto L43200

L45700:        if key% <> 15 then L45740
                  call "PRNTSCRN"
                  goto L43200

L45740:     if key% = 16% then L65000
                  if key%  =  2 then line% = 0
                  if key%  =  3 then line% = max(0, routes% - 17)
                  if key%  =  4 then line% = max(0, line% - 15)
                  if key%  =  5 then line% = min(line% + 15,             ~
                                                  max(0, routes% - 17))
                  if key%  =  6 then line% = max(0, line% - 1)
                  if key%  =  7 then line% = min(line% + 1,              ~
                                                  max(0, routes% - 17))
                  if key%  =  8 then show_again
                  goto show_concurrent

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            end
