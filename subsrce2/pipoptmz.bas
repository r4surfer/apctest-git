        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP    OOO   PPPP   TTTTT  M   M  ZZZZZ   *~
            *  P   P    I    P   P  O   O  P   P    T    MM MM     Z    *~
            *  PPPP     I    PPPP   O   O  PPPP     T    M M M    Z     *~
            *  P        I    P      O   O  P        T    M   M   Z      *~
            *  P      IIIII  P       OOO   P        T    M   M  ZZZZZ   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPOPTMZ - OPTIMIZE PIP SUB                               *~
            *            CALLED BY PIPSCAN                              *~
            *                                                           *~
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
            * 06/03/88 ! ORIGINAL                                 ! RJM *~
            * 11/03/93 ! Purchase Job Project - Added Support for ! JBK *~
            *          !   'BW' Type Tags.                        !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "PIPOPTMZ" (                                                 ~
                       part$,            /* PART TO BE OPTIMIZED       */~
                       startday%,        /* SUBSCRIPT FOR START DATE   */~
                       supply%,          /* DAYS TO SUPPLY             */~
                       today%,           /* SUBSCRIPT FOR TODAY'S DATE */~
                       psbprior$,        /* PLANNING PRIORITY          */~
                       ret%,             /* KEYHIT IN & OUT            */~
                       #1,               /* DEMMASTR                   */~
                       #2,               /* PIPMASTR                   */~
                       #6,               /* RTEALTRS                   */~
                       #7,               /* RTEMASTR                   */~
                       #8,               /* JBCROSS2                   */~
                       #11,              /* WCMASTR                    */~
                       #14,              /* BOMSPEC                    */~
                       #15,              /* BOMMASTR                   */~
                       #16,              /* HNYALTRS                   */~
                       #23,              /* WCOUT                      */~
                       #24,              /* ENGMASTR                   */~
                       #33,              /* PIPIN                      */~
                       #34,              /* PIPOUT                     */~
                       #35,              /* PIPCROSS                   */~
                       #36,              /* JBPIPXRF                   */~
                       #40,              /* SFMASTR2                   */~
                       #41,              /* SFCUM2                     */~
                       #62,              /* WORK1                      */~
                       #63)              /* WORK2                      */


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
            clrpart$25,                  /* CLEAR PIPIN PART           */~
            clrtag$19,                   /* CLEAR PIPIN TAG            */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            intagnr$19,                  /* TAG NUMBER                 */~
            part$25,                     /* PART NUMBER                */~
            planflagshold$(25)20,                                        ~
            plowkey$100,                 /* PLOW KEY                   */~
            workpip%(490)

        dim psbcode$16,                  /* DEMAND CODE                */~
            psbline$3,                   /* DEMAND LINE                */~
            psbtype$1,                   /* DEMAND TYPE                */~
            psbprior$1,                  /* DEMAND PRIORITY            */~
            psbpart$25,                  /* PART NEEDED                */~
            psbrte$3,                    /* THE WC ROUTE TO USE        */~
            psbbom$3                     /* WHICH BOM TO USE           */

        REM PSBQUANT,                    /* QUANTITY NEEDED            */~
            PSBCD%,                      /* REQ'D COMPL DATE           */~
            PSBTODAY%,                   /* SUBSCRIPT FOR TODAY'S DATE */~
            PLANFLAG%                    /* >0= PLAN, 0 = CHECK ONLY   */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

        REM *************************************************************~
            *  SELECT FILES                                             *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! DEMMASTR ! Demand Master File                       *~
            * #2  ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #6  ! RTEALTRS ! Rte alternates                           *~
            * #7  ! RTEMASTR ! Production routing master file           *~
            * #8  ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #11 ! WCMASTR  ! Work center master file                  *~
            * #14 ! BOMSPEC  ! options selected file                    *~
            * #15 ! BOMMASTR ! BOM relationship file                    *~
            * #16 ! HNYALTRS ! Inventory Alternate Part File            *~
            * #23 ! WCOUT    ! Planned work center use detail rec       *~
            * #24 ! ENGMASTR ! Engineering Master Filer                 *~
            * #33 ! PIPIN    ! Planned inventory additions detail       *~
            * #34 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #35 ! PIPCROSS ! hard peg cross reference                 *~
            * #36 ! JBPIPXRF ! option part harder peg                   *~
            * #40 ! SFMASTR2 ! Sales forecast master file               *~
            * #41 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #62 ! WORK1    ! PLANNING WORKFILE - MATERIALS            *~
            * #63 ! WORK2    ! PLANNING WORKFILE - WORK CENTER ALLOC    *~
            *************************************************************~


        REM *************************************************************~
            *                                                           *~
            *      OPTIMIZE THE PIPIN FILE                              *~
            *                                                           *~
            *************************************************************
            singlelevel% = 0%
            call "SHOSTAT" ("Optimization in Process")
            if ret% = 24% then singlelevel% = 1%

            init (hex(00)) plowkey$
            put str(plowkey$,1,29), using L35110, part$, startday%
L35110:         FMT CH(25), BI(4)
L35120:     call "PLOWALTS" (#33, plowkey$, 1%, 25%, f1%(33))
                if f1%(33) = 0% then start_loop
            get #33, using L35150, due%, intagnr$
L35150:         FMT XX(25), BI(4), CH(19)
            if str(intagnr$,1%,2%)  = "BO" then L35190
            if str(intagnr$,1%,2%)  = "BW" then L35190
            if str(intagnr$,1%,2%) <> "WO" then L35120

L35190:         clrpart$ = part$
                clrdate% = due%
                clrtag$  = intagnr$

                gosub call_clrpipin

            goto L35120

        start_loop

           call "READ100" (#2, part$, f1%(2))
            if f1%(2) = 0% then L65000
           get #2, using L35350, workpip%(), type%, lt%
L35350:         FMT XX(1), XX(25), 490*BI(4), XX(32), 2*BI(2)

            if type% = 0% then L65000
            if type% < 200% then L65000
            if type% > 489% and type% < 500% then L65000
            if type% > 789% and type% < 800% then L65000
            if type% > 499% then lt% = 0%

        REM START LOOP
            startday% = startday% + lt% : make = 0
L35510:     if startday% = 1% then L35560
                if workpip%(startday%-1%) >= 0% then L35560
                   for i%=startday% to 490%
                     workpip%(i%) = workpip%(i%) - workpip%(startday%-1%)
                   next i%
L35560:     if workpip%(startday%) < 0% then L35600
L35570:     startday% = startday% + 1%
            make = 0
            if startday% < 490% then L35510 else L65000

L35600:     for i% = startday% to min(490%, startday% + supply%)
                make = min(make, workpip%(i%))
            next i%
            if abs(make) < .0001 then L35570
            gosub planit
            goto L35570

        planit

            psbcode$  = "OP000" & date & time
            psbline$  = "001"
            psbtype$  = "8"
            psbprior$ = psbprior$
            psbpart$  = part$
            psbrte$   = " "
            psbbom$   = " "
            first% = 0%
            psbquant  = -make
            psbcd%    = startday%
            psbtoday% = today%
            planflag% = 5%

            if singlelevel% = 0% then                                    ~
                             str(planflags$(),46,6)=hex(ffffffffffff)

            gosub call_plansub

            str(planflags$(),46,6)=str(planflagshold$(),46,6)

            if final% < 1% or final% > 490% then return

            init (hex(00)) plowkey$
            str(plowkey$,1,19) = str(psbcode$,1,16) & str(psbline$,1,3)
            str(plowkey$,20,19) = str(intagnr$,1,19)
            call "PLOWNEXT" (#35, plowkey$, 38%, f1%(35))
               if f1%(35) = 0% then L35955
            get #35, using L35950, psbquant
L35950:         FMT XX(97),PD(14,4)
L35955:     for i%=startday% to 490%
                workpip%(i%)=workpip%(i%) + psbquant
            next i%
            call "DELETE" (#35, plowkey$, 19%)

            return

        REM *************************************************************~
            *                                                           *~
            *     COMMON CALL TO PLANSUB                                *~
            *                                                           *~
            *************************************************************

        call_plansub

            call "PLANSUB"                                               ~
                    (psbcode$,           /* DEMAND CODE                */~
                     psbline$,           /* DEMAND LINE                */~
                     psbtype$,           /* DEMAND TYPE                */~
                     psbprior$,          /* DEMAND PRIORITY            */~
                     psbpart$,           /* PART NEEDED                */~
                     psbquant,           /* QUANTITY NEEDED            */~
                     psbcd%,             /* REQ'D COMPL DATE           */~
                     "001",              /* DELIVER TO WAREHOUSE       */~
                     psbrte$,            /* THE WC ROUTE TO USE        */~
                     psbbom$,            /* WHICH BOM TO USE           */~
                     errormsg$,          /* THE RETURN MESSAGE         */~
                     first%,             /* FIRST DATE PASSED BACK     */~
                     final%,             /* DATE PLANNED FOR           */~
                     psbtoday%,          /* SUBSCRIPT FOR TODAY'S DATE */~
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
                     #62,                                                ~
                     #63)

            return

        REM *************************************************************~
            *                                                           *~
            *     COMMON CALL TO CLRPIPIN                               *~
            *                                                           *~
            *************************************************************

        call_clrpipin

             call "CLRPIPIN"                                             ~
                    (clrpart$,                                           ~
                     today%,                                             ~
                     clrdate%,           /* DATE OF PIPIN TO CLEAR     */~
                     clrtag$,            /* TAG OF PIPIN TO CLEAR      */~
                     #1,                 /* DEMMASTR                   */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #41,                /* SFCUM2                     */~
                     #35,                /* PIPCROSS                   */~
                     #36)                /* JBPIPXRF                   */

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
