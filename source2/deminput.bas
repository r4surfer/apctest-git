        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  DDDD   EEEEE  M   M  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  D   D  E      MM MM    I    NN  N  P   P  U   U    T     *~
            *  D   D  EEEE   M M M    I    N N N  PPPP   U   U    T     *~
            *  D   D  E      M   M    I    N  NN  P      U   U    T     *~
            *  DDDD   EEEEE  M   M  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DEMINPUT - THE PRIMARY PRODUCTION PLANNING TOOL FOR THE   *~
            *            CAELUS MANAGEMENT SYSTEMS LEVEL-II.  ALLOWS    *~
            *            EXAMINATION & AUTOMATIC CORRECTION OF PIP,     *~
            *            DIRECT MANAGEMENT OF DEMANDS, EXTENSIVE        *~
            *            REVIEWS, ETC.                                  *~
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
            * 12/03/82 ! ORIGINAL                                 ! GLW *~
            * 02/01/83 ! MODIFIED FILE FMTS & SUB CALLS           ! GLW *~
            * 07/13/83 ! EXPANDED MANAGEMENT ABILITY              ! GLW *~
            * 03/28/84 ! BOMSPEC  RECORD LENGTH CHANGED TO 136    ! ECR *~
            * 03/30/84 ! MISC AND ASUNDRY COMESTIC CHANGES        ! ERN *~
            * 05/13/87 ! Std Cost Changes (HNYMASTR/RTEMASTR).    ! ERN *~
            * 11/02/87 ! Fixed calls to '151 (format dates)       ! HES *~
            * 12/18/87 ! Link to DEMSTAT                          ! JDH *~
            * 06/09/88 ! Doesn't allow adding lines to SO demands ! RJM *~
            *          !  Changed arg to plansub to prevent type  !     *~
            *          !  7 & 8 demand codes from being changed   !     *~
            *          ! (HES) - rewrites BOM ID back to SO line. !     *~
            * 06/16/88 ! Added Alt key 3 to DEMMASTR              ! MJB *~
            * 11/04/88 ! Added HNYALTRS to PLNRSUB                ! KAB *~
            * 02/01/89 ! Added Premature DATASAVE for DEMSTAT Call! KAB *~
            *          ! Fixed PLANFLAG% for Demand Types 7 & 8.  ! KAB *~
            * 12/04/89 ! Removed PIPMSUB and corresponding code   ! MJB *~
            * 04/21/92 ! PRR 11450 Added Info to PF8 PLOWCODE.    ! JDH *~
            *          ! PRR 11809 Stanardize hdr & BOM PLOWCODE. !     *~
            * 06/19/92 ! Rub nose in fact that demand is unapprovd! JDH *~
            * 07/02/92 ! Changed to pass PIPIN channel to DEMSTAT ! WPH *~
            * 02/02/93 ! Changed to call BOMBRWSB to review BOM   ! WPH *~
            * 03/08/94 ! Changed record length for BOMSPEC        ! WPH *~
            * 08/05/96 ! Changes for the year 2000.               ! DXL *~
            * 07/23/97 ! Fix Plan off of calendar error for demand! DXL *~
            *          ! types 1 and 2.                           !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

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
            apprid$(5)3,                                                 ~
            apprmoday%(5),                                               ~
            apprstat$1,                                                  ~
            apprstat1$1,                                                 ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            compdate$8,                  /* PLANNED COMPLETION DATE    */~
            compdatedescr$32,            /* PLANNED COMPLETION DATE    */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dembom$3,                    /* WHICH BOM STRUCTURE?       */~
            dembomdescr$32,              /* WHICH BOM STRUCTURE?       */~
            demcode$16,                  /* DEMAND CODE                */~
            demcompdate$8,               /* DESIRED COMPLETION DATE    */~
            demcompdatedescr$32,         /* DESIRED COMPLETION DATE    */~
            demcus$9,                                                    ~
            demkey$19,                                                   ~
            demline$3,                   /* DEMAND LINE                */~
            dempart$25,                  /* PART CODE                  */~
            dempartdescr$32,             /* PART CODE                  */~
            dempriority$1,               /* PRIORITY DESIRED (A-Z)     */~
            demquantity$10,              /* QUANTITY DESIRED           */~
            demrte$3,                    /* WHICH ROUTING?             */~
            demstatus$1,                 /* DEMAND STATUS              */~
            demstatusdescr$32,           /* DEMAND STATUS              */~
            demstring$19,                /* DEMAND CODE & LINE         */~
            demtype$1,                   /* DEMAND TYPE                */~
            demtypedescr$32,             /* DEMAND TYPE                */~
            demwc$4,                     /* WHICH WC (PM ONLY)         */~
            demwcdescr$32,               /* WHICH WC (PM ONLY)         */~
            demwhse$3,                   /* WHICH WAREHOUSE            */~
            descr$35,                    /* PLOWCODE Decription (Part) */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            err$79,                      /* Error returned from sub    */~
            excost$13,                   /* Extended Standard Cost     */~
            hdr$(2)79,                   /* PLOWCODE Header            */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            junk(4),                     /* WORK VARIABLE              */~
            ldemcompdate$8,                                              ~
            ldemcode$16,                                                 ~
            ldemline$3,                                                  ~
            ldempart$25,                                                 ~
            ldemquant$10,                                                ~
            line2$79,                    /* 2nd line on screen         */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            pfkey$(2)20,                                                 ~
            pfmessage$(2)79,                                             ~
            plandate$8,                  /* DATE LAST PLANNED          */~
            plandatedescr$32,            /* DATE LAST PLANNED          */~
            plandet$3,                                                   ~
            type$3,                                                      ~
            readkey$50,                                                  ~
            reviewpart$25,                                               ~
            unplanopt$1,                                                 ~
            unplanrpt$1,                                                 ~
            uscost$10,                   /* Unit Standard Cost         */~
            statusmsg$79

        dim                                                              ~
            mbom$3,                      /* BILL OF MATERIALS          */~
            mdate$8,                     /* DUE DATE                   */~
            mpart$25,                    /* PART                       */~
            mpriority$1,                 /* PRIORITY                   */~
            mquantity$10                 /* MAXIMUM QUANTITY           */~

        dim  /* DAYS$79,                 /* CALENDAR VARIABLES         */~
            months$79,                                                   ~
            modate$(12)9,                                                ~
            yy%(490),                                                    ~
            mm%(490),                                                    ~
            dd%(490),                                                    ~
            dow$(490)3,                                                  ~
            mwoy%(490),                                                  ~
            fwoy%(490),                                                  ~
            cqoy%(490),                                                  ~
            fqoy%(490)                                                   ~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            fs%(64),                     /* = 1 IF THE FILE IS OPEN    */~
            rslt$(64)20                  /* TEXT FROM FILE OPENING     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! DEMMASTR ! MASTER FILE FOR PLANNING DEMANDS         *~
            * # 2 ! PIPMASTR ! PLANNED INVENTORY POSITION MASTER FILE   *~
            * # 4 ! HNYMASTR ! V-2 INVENTORY MASTER FILE                *~
            * # 5 ! HNYDETAL ! HNY DETAILS FOR REVIEWS                  *~
            * # 6 ! RTEALTRS ! Alternate Routes File                    *~
            * # 7 ! RTEMASTR ! STANDARD & ALT WORK CENTER ROUTINGS      *~
            * # 8 ! JBCROSS2 ! JOB TO BOM/RTE CROSS - JOB TRACKING      *~
            * #11 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * #12 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            * #13 ! PLTMASTR ! PLANT MASTER FILE, ALLOWABLE WAREHOUSES  *~
            * #14 ! BOMSPEC  ! OPTION BILL SPECIFICATIONS FILE          *~
            * #15 ! BOMMASPL ! BILL OF MATERIALS MASTER FILE            *~
            * #16 ! HNYALTRS ! ALTERNATE PARTS MASTER FILE              *~
            * #17 ! BOMSPHDR ! Header File for BOMSPEC                  *~
            * #20 ! BCKLINES ! Sales Order Line Item File               *~
            * #21 ! STORNAME ! WAREHOUSE (STORE) MASTER FILE            *~
            * #23 ! WCOUT    ! WORK CENTER CROSS REFERENCE FILE         *~
            * #24 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #33 ! PIPIN    ! PIP QUANTS DUE IN                        *~
            * #34 ! PIPOUT   ! PIP QUANTS DUE OUT                       *~
            * #35 ! PIPCROSS ! PIP HARD PEG CROSS REFERENCE             *~
            * #36 ! JBPIPXRF ! GENERIC TAG FILE                         *~
            * #40 ! SFMASTR2 ! SALES FORECAST MASTER FILE               *~
            * #41 ! SFCUM2   ! CUMULATIVE SALES FORECASTS               *~
            * #43 ! JBMASTR2 ! Production job master file               *~
            * #44 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * #45 ! VBKMASTR ! Purchase Order Headers Master File       *~
            * #46 ! VBKLINES ! Purchase Orders Line Item Master file    *~
            * #47 ! JBCREDIT ! Production job credits received detail f *~
            * #48 ! RCVLINES ! Receiver Lines File                      *~
            * #49 ! PAYMASTR ! Payables header file                     *~
            * #50 ! PAYLINES ! Payables lines  file                     *~
            * #51 ! TXTFILE  ! System Text File                         *~
            * #52 ! BCKMASTR ! Sales Order Master File                  *~
            * #60 ! SYSFILE2 ! SYSTEM OPTIONS                           *~
            * #62 ! WORK1    ! Work File                                *~
            * #63 ! WORK2    ! Work File                                *~
            *************************************************************


           select # 1, "DEMMASTR", varc, indexed,                        ~
                     recsize = 123 , keypos = 2   , keylen = 27,         ~
                     alt key 1, keypos =10, keylen = 19,                 ~
                         key 2, keypos = 1, keylen = 28,                 ~
                         key 3, keypos =29, keylen = 25, dup

           select # 2, "PIPMASTR", varc, indexed,                        ~
                     recsize = 2024, keypos = 2   , keylen = 25,         ~
                     alt key 1, keypos = 1 , keylen = 26

            select # 4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup

            select #5, "HNYDETAL",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select # 6, "RTEALTRS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =   1, keylen = 34

            select # 7, "RTEMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos =   5, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 35

            select #8,  "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 94,                                    ~
                        keypos=29, keylen = 19,                          ~
                        alt key 1, keypos = 1 , keylen = 47,             ~
                            key 2, keypos = 48, keylen = 47

           select #11,  "WCMASTR", varc, indexed,                        ~
                     recsize =2024 , keypos =  2  , keylen = 5,          ~
                     alt key 1, keypos = 1, keylen = 6

           select #12, "CALMASTR", varc, indexed,                        ~
                     recsize = 1962, keypos = 1   , keylen = 2

           select #13, "PLTMASTR", varc, indexed,                        ~
                     recsize = 2024, keypos = 1   , keylen = 9


           select #14, "BOMSPEC" , varc, indexed,                        ~
                     recsize = 150 , keypos = 26  , keylen = 54 ,        ~
                     alt key 1, keypos = 57, keylen = 23


           select #15, "BOMMASTR", varc, indexed,                        ~
                     recsize = 150 , keypos = 26  , keylen = 31 ,        ~
                     alt key 1, keypos = 1, keylen = 56

           select #16, "HNYALTRS", varc, indexed,                        ~
                     recsize =  60 , keypos = 1   , keylen = 33

            select #17, "BOMSPHDR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  53,                     ~
                        alt key  1, keypos =   35, keylen =  19

            select #20, "BCKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 10, keylen = 19

            select #21, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

           select #23, "WCOUT",                                          ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos = 9, keylen = 23,                         ~
                        alt key 1, keypos = 1, keylen = 27

           select #24, "ENGMASTR" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

           select #33, "PIPIN"   , varc, indexed,                        ~
                     recsize = 60,   keypos =  30 , keylen =  19,        ~
                     alt key 1, keypos = 1 , keylen = 48

           select #34, "PIPOUT"  , varc, indexed,                        ~
                     recsize = 64,   keypos =  1  , keylen =  56,        ~
                     alt key 1, keypos = 20, keylen = 37

            select #35, "PIPCROSS", varc, indexed,                       ~
                     recsize = 150,  keypos =  1  , keylen =  71,        ~
                     alt key 1, keypos = 20, keylen = 52,                ~
                         key 2, keypos = 39, keylen = 33

           select #36, "JBPIPXRF",varc, indexed, recsize=63,             ~
                        keypos=1, keylen=63,                             ~
                        alt key 1, keypos=45, keylen=19

           select #40, "SFMASTR2" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos = 1, keylen = 25

           select #41, "SFCUM2",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos = 1, keylen = 25

            select #43, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select #44, "JBSTATUS",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   21, keylen =  44,         ~
                            key  2, keypos =   29, keylen =  36          ~

            select #45, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16          ~

            select #46  "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #47, "JBCREDIT",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select #48, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select #49, "PAYMASTR",                                      ~
                        varc,     indexed,  recsize = 350 ,              ~
                        keypos =    1, keylen =  25


            select #50, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key 1, keypos = 1, keylen = 63,              ~
                            key 2, keypos = 17, keylen = 47

            select #51, "TXTFILE" ,                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen = 11

            select #52, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =   1, keylen = 25,                       ~
                        alt key 1, keypos = 26, keylen = 16, dup

           select #60, "SYSFILE2" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #62, "WORK1",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos = 31, keylen = 8,                         ~
                        alt key 1, keypos = 1, keylen = 29,              ~
                            key 2, keypos = 35, keylen= 4,               ~
                            key 3, keypos = 30, keylen= 9

            select #63, "WORK2",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 66,                                    ~
                        keypos = 5, keylen = 10,                         ~
                        alt key 1, keypos = 1, keylen = 4, dup


           call "SHOSTAT" ("Linking To Data Base For Production Planning ~
        ~And Analysis")

            call "WORKOPEN" (#62, "IO", 3000%, f2%(62))
            call "WORKOPEN" (#63, "IO", 3000%, f2%(63))

           call "OPENCHCK" (#12, fs%(12), f2%(12),   0%, rslt$(12))
                     if f2%(12) <> 0% then goto L65000

                hit = 0
           gosub loadcalendar
                if hit > 0 then L65000

           call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 100%, rslt$( 1))
           call "OPENCHCK" (# 2, fs%( 2), f2%( 2),   0%, rslt$( 2))
           call "OPENCHCK" (#33, fs%(33), f2%(33), 100%, rslt$(33))
           call "OPENCHCK" (#34, fs%(34), f2%(34), 100%, rslt$(34))
           call "OPENCHCK" (#35, fs%(35), f2%(35), 100%, rslt$(35))
           call "OPENCHCK" (#36, fs%(36), f2%(36), 100%, rslt$(36))
           call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 100%, rslt$( 8))
           call "OPENCHCK" (# 4, fs%( 4), f2%( 4),   0%, rslt$( 4))
           call "OPENCHCK" (# 5, fs%( 5), f2%( 5),   0%, rslt$( 5))
           call "OPENCHCK" (# 6, fs%( 6), f2%( 6),   0%, rslt$( 6))
           call "OPENCHCK" (# 7, fs%( 7), f2%( 7),   0%, rslt$( 7))
           call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))
           call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
           call "OPENCHCK" (#15, fs%(15), f2%(15),   0%, rslt$(15))
           call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
           call "OPENCHCK" (#17, fs%(17), f2%(17),   0%, rslt$(17))
           call "OPENCHCK" (#20, fs%(20), f2%(20),   0%, rslt$(20))
           call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, rslt$(21))
           call "OPENCHCK" (#23, fs%(23), f2%(23), 100%, rslt$(23))
           call "OPENCHCK" (#24, fs%(24), f2%(24),   0%, rslt$(24))
           call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
           call "OPENCHCK" (#40, fs%(40), f2%(40), 100%, rslt$(40))
           call "OPENCHCK" (#41, fs%(41), f2%(41), 100%, rslt$(41))
           call "OPENCHCK" (#52, fs%(52), f2%(52), 100%, rslt$(52))
           call "OPENCHCK" (#60, fs%(60), f2%(60),   0%, rslt$(60))

        REM **  Files #43 thru #51 are not opened until DEMSTAT is called



        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

                     for today% = 1% to 490%
                     if yymmdd$(today%) = date then goto L08100
                     next today%

L08100:     date$ = date
                months$ = "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC"
        REM     DAYS$ = "SUNMONTUEWEDTHRFRISAT"
            call "DATEFMT" (date$)
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

            pfmessage$(1)=                                               ~
                  "(2)Next/Don't Save    (4)Test Feasibility     (8)Plan ~
        ~as Shown    (16)Next/Save"
            pfmessage$(2)=                                               ~
                  "                      (7)Unplan and Edit              ~
        ~            (16)Next/Save"

            pfkey$(1)=hex(00020304050608090a0b0c0d0e0f101418ffffff)
            pfkey$(2)=hex(03050607090a0b0c0d0e0f1018ffffffffffffff)

            call "READ100" (#60, "PLANNING SYSTEM FLAG", f1%(60))
            if f1%(60)=0% then L65000
            get #60, using L08410, str(planflags$(),1,480)
L08410:         FMT XX(20), CH(480)

            unplanopt$ = str(planflags$(), 4, 1)
            unplanrpt$ = str(planflags$(),18, 1)

        REM *************************************************************~
            *       M A I N   C O N T R O L   S E C T I O N             *~
            *                                                           *~
            *************************************************************

        maincontrol

L09035: accept                                                           ~
               at (01,03),                                               ~
        ">>>>>>>>>>>> PRODUCTION PLANNING AND DEMAND MANAGEMENT FUNCTIONS~
        ~ <<<<<<<<<<<<",                                                  ~
               at (03,03),                                               ~
        "+--------+------------------------------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "! PF-KEY ! Use the PF Keys shown at left to Analyze, Plan, Revie~
        ~w or Manage !",                                                  ~
               at (05,03),                                               ~
        "+--------+------------------------------------------------------~
        ~------------+",                                                  ~
               at (06,03),                                               ~
        "! RETURN ! Enter/manage plans for fulfilling sales/forecast dema~
        ~nds         !",                                                  ~
               at (07,03),                                               ~
        "!  ( 1)  ! or other production requirements                     ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "+--------+------------------------------------------------------~
        ~------------+",                                                  ~
               at (09,03),                                               ~
        "!  ( 3)  ! Modify planning system parameters (this session only)~
        ~            !",                                                  ~
               at (10,03),                                               ~
        "+--------+------------------------------------------------------~
        ~------------+",                                                  ~
               at (11,03),                                               ~
        "!  ( 4)  ! Test feasibility for quantity by a given date        ~
        ~            !",                                                  ~
               at (12,03),                                               ~
        "!  ( 5)  ! Examine and report on the current status of demands  ~
        ~            !",                                                  ~
               at (13,03),                                               ~
        "+--------+------------------------------------------------------~
        ~------------+",                                                  ~
               at (14,03),                                               ~
        "!  ( 6)  ! Examine planned inventory position including availabl~
        ~e to commit !",                                                  ~
               at (15,03),                                               ~
        "!  ( 7)  ! Examine the sources and applications of planned inven~
        ~tory        !",                                                  ~
               at (16,03),                                               ~
        "!  ( 8)  ! Examine sales vs sales forecast performance as curren~
        ~tly planned !",                                                  ~
               at (17,03),                                               ~
        "+--------+------------------------------------------------------~
        ~------------+",                                                  ~
               at (18,03),                                               ~
        "!  ( 9)  ! Examine job and purchase order advices and orders out~
        ~standing    !",                                                  ~
               at (19,03),                                               ~
        "+--------+------------------------------------------------------~
        ~------------+",                                                  ~
               at (20,03),                                               ~
        "!  (10)  ! Examine routing structures                           ~
        ~            !",                                                  ~
               at (21,03),                                               ~
        "!  (11)  ! Examine effectivity dates                            ~
        ~            !",                                                  ~
               at (22,03),                                               ~
        "!  (12)  ! Examine work center capacity                         ~
        ~            !",                                                  ~
               at (23,03),                                               ~
        "+--------+------------------------------------------------------~
        ~------------+",                                                  ~
               at (24,03), "(15)Print Screen",                           ~
               at (24,64), "(16)Exit Program",                           ~
                                                                         ~
           keys(hex(0001ff030405060708090a0b0c0f10)), key(rr%)

           if rr% = 16% then goto L65000
           if rr%  > 1% then L09414
              goto  inputmode
              goto L09035

L09414:    if rr% > 3% then L09430
              gosub L62000
              goto L09035
L09430:    if rr% <> 15% then L09445
              call "PRNTSCRN"
              goto L09035
L09445:    if rr% <> 4% then L09475
              gosub L63000
              goto L09035
L09475:       mode%=0%
           if rr% =  6% then mode%=1%
           if rr% =  7% then mode%=2%
           if rr% = 10% then mode%=3%
           if rr% = 11% then mode%=4%
           if rr% =  5% then mode%=7%
           if rr% =  9% then mode%=6%
           if rr% =  8% then mode%=5%
           if rr% = 12% then mode%=8%
           if mode% =0% then L09035
              gosub review_functions
              goto L09035

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode

                    ldemcompdate$ =  demcompdate$
                    ldemcode$ =  demcode$
                    ldemline$ =  demline$
                    ldempart$ =  dempart$
                    ldemquant$ =  demquantity$
L10037:     demkey$=str(demcode$,1,16)&str(demline$,1,3)
            demmode%=0%

            init(" ") errormsg$, inpmessage$, statusmsg$, excost$,       ~
            demcus$, apprid$(), apprstat1$, apprstat$, type$, uscost$,   ~
            compdate$, compdatedescr$, dembom$, dembomdescr$, demcode$,  ~
            demcompdate$, demcompdatedescr$, demline$, dempart$,         ~
            dempartdescr$, dempriority$, demquantity$, demrte$,          ~
            demstatus$, demstatusdescr$, demtype$, demtypedescr$,        ~
            demwc$, demwcdescr$, demwhse$, plandate$, plandatedescr$

            mat apprmoday% = zer
            apprstat$ = hex(00)
           call "REDALT2" (#1, demkey$, 1%, f1%(1))
           if f1%(1) = 0% then goto L10090
           get #1, using L10086, demcode$, demline$
L10086:    FMT XX(9), CH(16), CH(3)

L10090:     for fieldnr% = 1% to 10%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10180
                      errormsg$ = " "
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if fieldnr% <>  1% then L10150
                         if keyhit%  = 16% then maincontrol
                         if keyhit%  =  5% then L10037
L10150:               if keyhit% <>  0% then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

            statusmsg$="This Demand is UNPLANNED, Modify or Plan This Dem~
        ~and as Desired"
            demmode%=1%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************
        editmode
            if demmode%=1% then                                          ~
            inpmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."                                ~
                                else inpmessage$=" "

L11060:     gosub'111(0%)
                  errormsg$ = " "
                  if keyhit%  = 16% then       datasave
                  if keyhit%  =  2% then       datasave
                  if keyhit%  =  4% then       planquestion
                  if keyhit%  = 20% then       planquestion
                  if keyhit%  =  7% then       unplan
                  if keyhit%  =  8% then       planquestion
                  if keyhit%  = 24% then       planquestion
                  if keyhit% <>  0% then       L11060
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 3% or fieldnr% > 10% then editmode
            if demtype$<>"9" and fieldnr%=10% then editmode
            if demtype$>"2" and demtype$<"9" then L11690  /*NOT SO OR PM */

            if demtype$<>"9" then L11240
        REM PM MODIFIABLE FIELDS
            if fieldnr%=3% then L11690
            if fieldnr%=4% then L11690
            if fieldnr%=6% then L11690
            if fieldnr%=8% then L11690
            if fieldnr%=10% then L11690
            errormsg$ = hex(84) & "Sorry, edit of this field is not allow~
        ~ed for type 9 Demands."
            goto editmode

L11240: REM SALES ORDER CHANGABLE FIELDS
            if fieldnr%<>3% then L11290
               gosub'052(fieldnr%)
               print at (21,02), inpmessage$
               ask% = 1%
               call "ASKUSER" (ask%, "* * * Sales Order Demand * * *",   ~
                             "Demand Type Can Only Be '1' or '2'.",      ~
                             "Press PF1 to Change.",                     ~
                             "Press Any Other PF Key to Return to Edit.")
               if ask% <> 1% then editmode
               if demtype$="1" then demtype$="2" else demtype$="1"
               gosub'151(3%)
               errormsg$=" "
               goto editmode
L11290:     if fieldnr%=4% then L11690
            if fieldnr%=5% then L11400
            if fieldnr%=9% then L11690
            errormsg$ = hex(84) & "Sorry, edit of this field is not allow~
        ~ed for Sales Order type Demands."
            goto editmode

L11400: REM CALL TO BOMOPSUB GOES HERE FOR OPTION PARTS

            if type%<>0% then editmode
            demkey$=str(demcode$,1,16) & str(demline$,1,3)

            call "READ100" (#20, demkey$, f1%(20))
                if f1%(20) = 0% then editmode
            get #20, using L11480, junk(3)  /* current price  */
L11480:         FMT POS(141), PD(14,4)
            k% = 2%
               call "ASKUSER" (k%, "* * * N O T I C E * * *",            ~
            "You are about to access the Option Selection Function. " &  ~
            "You can review or",                                         ~
            "change the configuration but price changes will not effect"&~
            " the sales order.",                                         ~
            "Press PF 16 to Continue or press any other key to return.")
               if k% <> 16% then editmode
            temp$=demcompdate$
            call "BOMOPSUB"(0%,dempart$,str(demkey$,1,19),str(temp$,1,8),~
                               dembom$, " ", " ", " ", " ", " ", junk(), ~
                               #4, #15, #14, #60, #24, #17, err%)
                if err% <> 0% then dembom$ = " "
            if junk(3) = junk(4) then goto editmode
            k% = 2%
               call "ASKUSER" (k%, "* * * W A R N I N G * * *",          ~
            "The selling price may have changed as a result of the ch" & ~
            "anges you have made.",                                      ~
            "The actual price can only be modified through the Order " & ~
            "Entry function.",                                           ~
            "Notify Order Entry if appropriate.  Press any key to " &    ~
            "acknowledge.")

            goto editmode
L11690:     gosub'052(fieldnr%)
                if enabled% = 0% then editmode
L11710:     gosub'111(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11710
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11710
            goto editmode


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)       /*DEFAULTS AND MESSAGES FOR INPUT*/
                  enabled% = 1%
                  on fieldnr% goto  L20100,         /* DEMAND CODE      */~
                                    L20150,         /* DEMAND LINE      */~
                                    L20200,         /* DEMAND TYPE      */~
                                    L20250,         /* DEMAND PRIORITY  */~
                                    L20300,         /* PART CODE        */~
                                    L20350,         /* QUANTITY DESIRED */~
                                    L20450,         /* Standard Costs   */~
                                    L20480,         /* REQ COMP DATE    */~
                                    L20500,         /* DEMAND BOM       */~
                                    L20600          /* WORK CENTER      */

                  return

            deffn'052(fieldnr%)           /* SET MESSAGES FOR EDITMODE */
                  enabled% = 1%
                  on fieldnr% goto  L20120,         /* DEMAND CODE      */~
                                    L20160,         /* DEMAND LINE      */~
                                    L20210,         /* DEMAND TYPE      */~
                                    L20260,         /* DEMAND PRIORITY  */~
                                    L20315,         /* PART CODE        */~
                                    L20360,         /* QUANTITY DESIRED */~
                                    L20450,         /* Standard Costs   */~
                                    L20480,         /* REQ COMP DATE    */~
                                    L20500,         /* DEMAND BOM       */~
                                    L20610          /* WORK CENTER      */
                  return

L20100:     REM DEFAULT/ENABLE FOR DEMAND CODE
L20120:     inpmessage$="For Procurement Or P/M, You May Leave Blank.  Th~
        ~e Computer Will Generate."
                return
L20150:     REM DEFAULT/ENABLE FOR DEMAND LINE
L20160:     inpmessage$="Enter Which Line Of This Demand."
                return
L20200:     REM DEFAULT/ENABLE FOR DEMAND TYPE
L20210: /*  INPMESSAGE$="1-Net Sale,2-Non Net Sale,3-Requisition,4-FCST,5~
        -Above FCST,8-PROC,9-Prev.Main."
            INPMESSAGE$="123456789012345678901234567890123456789012345678~
        9012345678901234567890123456789"
        */  inpmessage$="1-Net Sale,2-Non Net Sale,3-Req,4-FCST,5-Above F~
        ~CST,7-Prc/Jump,8-Prc,9-Pre.Mnt."
                return
L20250:     REM DEFAULT/ENABLE FOR PRIORITY DESIRED (A-Z)
L20260:     inpmessage$="Priority Must Be 'A' To 'Z'."
                return
L20300:     REM DEFAULT/ENABLE FOR PART CODE
                if demtype$="9" then enabled%=0%
L20315:     inpmessage$="Enter Valid Part Code."
                return
L20350:     REM DEFAULT/ENABLE FOR QUANTITY DESIRED
L20360:     inpmessage$="Enter Number of Parts Required, OR Number of WC ~
        ~Units For P/M."
                return
L20450: REM Default/enable for Standard Costs
            gosub L50400
            enabled% = 0%
            return
L20480: REM Default/enable for desired completion date
          inpmessage$="Enter The Desired Completion Date For This Demand."
                return
L20500: REM Default/enable for which BOM structure?
                if demtype$="9" then enabled%=0%
                if type%<500% and type%>0% then enabled%=0%
                if type% = 0% then enabled% = 0%
            inpmessage$="Leave Blank and the Effective Date Routine Will ~
        ~Select the Proper BOM."
                readkey$ = str(demcode$,,16) & str(demline$,,3)
                str(readkey$,20) = all(hex(00))
                call "PLOWALTS" (#14, readkey$, 1%, 19%, f1%(14))
                     if f1%(14) = 0% then return
                enabled% = 0%
                errormsg$ = "Sorry, BOM Can't Change Until Option Selecti~
        ~ons Are Cleared."
                return
L20600:     REM DEFAULT/ENABLE FOR WHICH WC (PM ONLY)
            if demtype$<>"9" then enabled% = 0%
L20610:     inpmessage$="Enter the Work Center for which this P/M is to b~
        ~e Performed."
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

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode


        REM *************************************************************~
            *                                                           *~
            *    LOAD EXISTING DEMAND                                   *~
            *                                                           *~
            *************************************************************

        loaddemanddata
            get #1, using L30120, demstatus$, demtype$, dempriority$,     ~
                demcompdate$, demcode$, demline$, dempart$, demquantity$,~
                demwc$, dembom$, demrte$, demwhse$, plandate$, compdate$,~
                demcus$, apprstat1$, apprid$(), apprmoday%()

             if demcompdate$ = blankdate$ then demcompdate$ = " "
             if compdate$    = blankdate$ then compdate$    = " "
             if plandate$    = blankdate$ then plandate$    = " "

L30120:     FMT CH( 1),                       /* RECORD STATUS         */~
                CH(1),                        /* DEMAND TYPE           */~
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
                CH(09),                       /* CUSTOMER IF TYPE 1    */~
                CH(01),                       /* APPROVAL STATUS       */~
                5*CH(3),                      /* APPROVAL BY           */~
                5*BI(2)                       /* APPROVAL ON MMDD (BIN)*/

                if pos("179" = demstatus$) <> 0% then apprstat$ = hex(00)~
                                                 else apprstat$ = hex(ff)

                return

L31000: REM *************************************************************~
            *                                                           *~
            *    REWRITE DEMAND RECORD                                  *~
            *                                                           *~
            *************************************************************

                if demcode$ <> " " and demline$ <> " " then L31060
                 if keyhit% = 2% then return
                 errormsg$ = "TO SAVE DEMAND CODE/LINE MUST NOT BE BLANK"
                 return clear
                 goto editmode

L31060:         demkey$ = str(demcode$,1,16) & str(demline$,1,3)
                call "REDALT1" (#1, demkey$, 1%, f1%(1))
                if keyhit% <>  2% then goto L31120
                if demtype$ > "2" then L31090
              errormsg$="Delete sales order demands via sales order input"
                return clear
                goto editmode

L31090:         if f1%(1)<>0% then delete #1
                return

L31120:     if demcompdate$<>" " then call "DATUNFMT" (demcompdate$)
            if compdate$<>" " then call "DATUNFMT" (compdate$)
            if plandate$<>" " then call "DATUNFMT" (plandate$)

            if compdate$ <> " " then L31290
            if apprstat$=hex(00) then demstatus$="1" else demstatus$=" "
               goto L31350

L31290:     if compdate$>demcompdate$ then L31330
            if apprstat$=hex(00) then demstatus$="9" else demstatus$="8"
               goto L31350

L31330:     if apprstat$=hex(00) then demstatus$="7" else demstatus$="6"

L31350:     if f1%(1)<>0% then delete #1

             if demcompdate$ = " " then demcompdate$ = blankdate$
             if compdate$    = " " then compdate$    = blankdate$
             if plandate$    = " " then plandate$    = blankdate$

            put #1, using L30120, demstatus$, demtype$, dempriority$,     ~
                demcompdate$, demcode$, demline$, dempart$, demquantity$,~
                demwc$, dembom$, demrte$, demwhse$, plandate$, compdate$,~
                demcus$, apprstat1$, apprid$(), apprmoday%()

            write #1

             if demcompdate$ = blankdate$ then demcompdate$ = " "
             if compdate$    = blankdate$ then compdate$    = " "
             if plandate$    = blankdate$ then plandate$    = " "

            call "READ101" (#20, demkey$, f1%(20))
                if f1%(20) = 0% then L31630
            put #20, using L31600, demtype$, dempriority$, dembom$
L31600:         FMT POS(240), CH(1), CH(1), POS(263), CH(3)
            rewrite #20

L31630:     if demcompdate$ <> " " then call "DATEFMT" (demcompdate$)
            if compdate$ <> " " then call "DATEFMT" (compdate$)
            if plandate$ <> " " then call "DATEFMT" (plandate$)

            return

        REM *************************************************************~
            *                                                           *~
            *    UNPLAN CURRENT DEMAND                                  *~
            *                                                           *~
            *************************************************************
        unplan
           statusmsg$ = "Unplanning Demand:" & " " & demcode$ & " "      ~
                     & str(demline$,1,3) & " One Moment Please"
           call "SHOSTAT" (statusmsg$)

           call "DATUNFMT" (compdate$)
           search str(yymmdd$(),1)=str(compdate$,1,6) to cursor%() step 6
           ddd%=(cursor%(1)/6%)+1%

        call "UNPLAN" (demcode$,         /* DEMAND CODE                */~
                     demline$,           /* DEMAND LINE                */~
                     demtype$,           /* DEMAND TYPE                */~
                     ddd%,               /* PLANNED COMPLETION DATE    */~
                     ed%,                /* DEMAND DUE DATE            */~
                     dempart$,           /* PART                       */~
                     demquant,           /* QUANTITY                   */~
                     today%,                                             ~
                     unplanopt$,         /* UNPLANNING OPTION          */~
                     unplanrpt$,         /* UNPLANNING REPORT          */~
                     #8,                                                 ~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT    REFERENCES       */~
                     #40,                /* SFMASTR2  SALES FORECASTS  */~
                     #41,                /* SFCUM2  CUMULATIVE FCSTS   */~
                     #35,                /* PIPCROSS                   */~
                     #36)                /* JBPIPXRF                   */

            compdate$,compdatedescr$,plandate$,plandatedescr$=" "
            if apprstat$=hex(00) then demstatus$="1" else demstatus$=" "
            statusmsg$="This Demand is UNPLANNED, Modify or Plan This Dem~
        ~and as Desired"
            demmode%=1%
            gosub'151(13%)
            goto editmode


        REM *************************************************************~
            *                                                           *~
            *    PLAN OR TEST FEASIBILITY                               *~
            *                                                           *~
            *************************************************************

        planquestion

            goto L33135            /* CHECKING IS ON IF THIS IS REM'D */
            if ed%>=today% then L33160
               errormsg$="Desired completion date is before today, please~
        ~ change."
               goto editmode

L33135:     cd%=ed%
L33160:     plandet$="NO"

L33180: accept                                                           ~
           at(01,02), ">>>>>>>>>>>>>>>>>>  PLEASE ENTER THE PLAN/TEST OPT~
        ~IONS YOU WISH <<<<<<<<<<<<<<<",                                  ~
           at(13,02), "Want Report?",                                    ~
           at(13,25), "(Yes/No) Answering 'YES' will cause a full report ~
        ~",                                                               ~
           at(14,25), "         of all details to be printed.",          ~
           at(13,19), fac(hex(81)), plandet$                     ,ch(03),~
           at(24,02), "(RETURN) To Continue      (15) Print Screen       ~
        ~         (16) Exit W/O Action",                                  ~
                     keys(hex(000f10)), key(rr%)
           if rr%  = 16% then goto L33340
           if rr% <> 15% then goto L33360
                     call "PRNTSCRN"
                     goto L33180

L33340:    errormsg$ = "ABORTED Plan at your request"
           goto editmode

L33360: REM NOW DECIDE WHAT TO DO
            if plandet$="NO" then L33363
            if plandet$<>"YES" then L33180
L33363: REM TEST FOR FAST PLAN SELECTION
            if keyhit% < 16% then L33370
                if demtype$="1" then demtype$="2"
                if demtype$="4" or demtype$="5" then demtype$="3"
                gosub'151(3%):errormsg$=" "
                dempriority$="@"
                keyhit% = keyhit% - 16%
L33370:     if keyhit%=4% then planflag%=0% else planflag%=5%
            firstdate%,finaldate%=0%
            if planflag%=0% then                                         ~
               statusmsg$="Checking Demand:"   else                      ~
               statusmsg$="Planning Demand:"
            statusmsg$=statusmsg$ & " " & demcode$ & str(demline$,1,3)
            if plandet$="NO" then                                        ~
               statusmsg$=statusmsg$ & " " & "No Printed Report"   else  ~
               statusmsg$=statusmsg$ & " " & "Will Print Full Report"

            if demtype$="9" then str(dempart$,22,4) = str(demwc$,1,4)
            compdate$,compdatedescr$,plandate$,plandatedescr$=" "
            if str(planflags$(),1,1) <> "Y" then                         ~
             call "SHOSTAT" (statusmsg$)

            if plandet$="YES" then planflag%=planflag%+10%
            if demcode$ <> " " and demtype$ <> " " then                  ~
                                 planflag% = planflag% + 100000%

        call "PLANSUB"  (demcode$,       /* DEMAND CODE                */~
                     demline$,           /* DEMAND LINE                */~
                     demtype$,           /* DEMAND TYPE                */~
                     dempriority$,       /* DEMAND PRIORITY            */~
                     dempart$,           /* PART NEEDED                */~
                     demquant,           /* QUANTITY NEEDED            */~
                     cd%,                /* REQ'D COMPL DATE           */~
                     demwhse$,           /* DELIVER TO WAREHOUSE       */~
                     demrte$,            /* THE WC ROUTE TO USE        */~
                     dembom$,            /* WHICH BOM TO USE           */~
                     errormsg$,          /* THE RETURN MESSAGE         */~
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
                     #62,                                                ~
                     #63)

        REM NOW FIND OUT HOW WE DID

            if demtype$="9" then str(dempart$,22,4) = "    "
            if finaldate% <= 490% and finaldate%>0% then L34500

L34240:     statusmsg$="This demand is UNPLANNED, modify or plan this dem~
        ~and as desired"
            demmode%=1%
            goto editmode

L34500: REM AHA!!   SUCCESS!!
            if plandet$<>"YES" then L34550
            firstdate%=max(firstdate%,1%)
            finaldate%=min(490%,finaldate%)
            finaldate%=max(finaldate%,firstdate%)
L34550:     if planflag%<1% then L34700
            if finaldate%>ed% then L34590
            if apprstat$=hex(00) then demstatus$="9" else demstatus$="8"
            goto L34601
L34590:     if apprstat$=hex(00) then demstatus$="7" else demstatus$="6"
            errormsg$=errormsg$ & " " & "* * NOTE PLANNED LATE * *"

L34601:     compdate$=yymmdd$(finaldate%)
            plandate$=date
            statusmsg$=errormsg$

            for fieldnr%=11% to 13%
                gosub'151(fieldnr%)
                next fieldnr%

            errormsg$=statusmsg$
            statusmsg$="This demand is now PLANNED, you must unplan prior~
        ~ to modification"
            demmode%=2%
            goto editmode

L34700:     errormsg$="Feasibility test complete.  Demand may be met on "
            temp$=yymmdd$(finaldate%)
            call "DATEFMT" (temp$)
            errormsg$=errormsg$ & " " & temp$
            goto L34240

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                line2$ = " Last:For " & ldemquant$ & " of " & ldempart$ &~
                         " on " & ldemcompdate$
                str(line2$,62) = "DEMINPUT: " & str(cms2v$,,8)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40140,         /* DEMAND CODE      */~
                                    L40140,         /* DEMAND LINE      */~
                                    L40140,         /* DEMAND TYPE      */~
                                    L40140,         /* DEMAND PRIORITY  */~
                                    L40140,         /* PART CODE        */~
                                    L40155,         /* QUANTITY DESIRED */~
                                    L40140,         /* Std Costs        */~
                                    L40140,         /* REQ COMP DATE    */~
                                    L40140,         /* DEMAND BOM       */~
                                    L40140          /* WORK CENTER      */

                     goto L40175

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      if fieldnr%=1% then lfac$(fieldnr%+1%) = hex(81)
                      return
L40155:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40175:     accept                                                       ~
               at (01,02),                                               ~
                  "Planning System Demand Maintenance   Last:",          ~
               at (01,45), fac(hex(8c)), ldemcode$              , ch(16),~
               at (01,62), fac(hex(8c)), ldemline$              , ch(03),~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(84)), statusmsg$             , ch(79),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Demand Code",                                         ~
               at (06,30), fac(lfac$( 1)), demcode$             , ch(16),~
               at (07,02),                                               ~
                  "Which line of this demand",                           ~
               at (07,30), fac(lfac$( 2)), demline$             , ch(03),~
               at (08,02),                                               ~
                  "Demand type(see list below)",                         ~
               at (08,30), fac(lfac$( 3)), demtype$             , ch(01),~
               at (08,49), fac(hex(8c)),   demtypedescr$        , ch(32),~
               at (09,02),                                               ~
                  "Priority desired (A-Z)",                              ~
               at (09,30), fac(lfac$( 4)), dempriority$         , ch(01),~
               at (10,02),                                               ~
                  "Part Code",                                           ~
               at (10,30), fac(lfac$( 5)), dempart$             , ch(25),~
               at (10,60), "PART TYPE",                                  ~
               at (10,70), fac(hex(8c)),   type$                , ch(03),~
               at (11,49), fac(hex(8c)),   dempartdescr$        , ch(32),~
               at (11,02),                                               ~
                  "Quantity desired",                                    ~
               at (11,30), fac(lfac$( 6)), demquantity$         , ch(10),~
               at (12,02), "Unit Standard Cost",                         ~
               at (12,30), fac(lfac$( 7)), uscost$              , ch(10),~
               at (12,49), "Extended Std Cost:",                         ~
               at (12,68), fac(lfac$( 7)), excost$              , ch(13),~
               at (13,02), "Desired completion date",                    ~
               at (13,30), fac(lfac$( 8)), demcompdate$         , ch(08),~
               at (13,49), fac(hex(8c)),   demcompdatedescr$    , ch(32),~
               at (14,02), "Which BOM structure?",                       ~
               at (14,30), fac(lfac$( 9)), dembom$              , ch(03),~
               at (14,49), fac(hex(8c)),   dembomdescr$         , ch(32),~
               at (15,02),                                               ~
                  "Which WC (PM only)",                                  ~
               at (15,30), fac(lfac$(10)), demwc$               , ch(04),~
               at (15,49), fac(hex(8c)),   demwcdescr$          , ch(32),~
               at (17,02),                                               ~
                  "Planned completion date",                             ~
               at (17,30), fac(hex(84)),   compdate$            , ch(08),~
               at (17,49), fac(hex(8c)),   compdatedescr$       , ch(32),~
               at (18,02),                                               ~
                  "Date last planned",                                   ~
               at (18,30), fac(hex(84)),   plandate$            , ch(08),~
               at (18,49), fac(hex(8c)),   plandatedescr$       , ch(32),~
               at (19,02),                                               ~
                  "Demand status",                                       ~
               at (19,30), fac(hex(84)),   demstatus$           , ch(01),~
               at (19,49), fac(hex(8c)),   demstatusdescr$      , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,25),                                               ~
                  "(5)Next Demand",                                      ~
               at (24,25),                                               ~
                  "(8)Find Demand",                                      ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Main Control",                                    ~
                                                                         ~
               keys(hex(000105080d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 8% then L40572
                  if fieldnr% > 2% then L40175
                  readkey$, errormsg$, statusmsg$ = " "
                  hdr$(1) = "  Demand Code    Line   Part      " &       ~
                            "                 Quantity"
                  call "PLOWCODE" (#1, readkey$, descr$, 2000%, 1.35,    ~
                                                   f1%(1), hdr$(), 0.35)
                     if f1%(1) = 0% then L40175
                  demcode$ = readkey$
                  demline$ = str(readkey$,17)
                  goto L40175

L40572:        if keyhit% <> 13% then L40590
                  call "MANUAL" ("DEMINPUT")
                  goto L40175

L40590:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40175

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41140,         /* DEMAND CODE      */~
                                    L41140,         /* DEMAND LINE      */~
                                    L41140,         /* DEMAND TYPE      */~
                                    L41140,         /* DEMAND PRIORITY  */~
                                    L41140,         /* PART CODE        */~
                                    L41155,         /* QUANTITY DESIRED */~
                                    L41140,         /* Std Costs        */~
                                    L41140,         /* REQ COMP DATE    */~
                                    L41140,         /* DEMAND BOM       */~
                                    L41140          /* WORK CENTER      */
                     goto L41175

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41155:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41175:     accept                                                       ~
               at (01,02),                                               ~
                  "Planning System Demand Maintenance   Last:",          ~
               at (01,45), fac(hex(8c)), ldemcode$              , ch(16),~
               at (01,62), fac(hex(8c)), ldemline$              , ch(03),~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(84)), statusmsg$             , ch(79),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Demand code",                                         ~
               at (06,30), fac(lfac$( 1)), demcode$             , ch(16),~
               at (07,02),                                               ~
                  "Which line of this demand",                           ~
               at (07,30), fac(lfac$( 2)), demline$             , ch(03),~
               at (08,02),                                               ~
                  "Demand type(see list below)",                         ~
               at (08,30), fac(lfac$( 3)), demtype$             , ch(01),~
               at (08,49), fac(hex(8c)),   demtypedescr$        , ch(32),~
               at (09,02),                                               ~
                  "Priority desired (A-Z)",                              ~
               at (09,30), fac(lfac$( 4)), dempriority$         , ch(01),~
               at (10,02),                                               ~
                  "Part Code",                                           ~
               at (10,30), fac(lfac$( 5)), dempart$             , ch(25),~
               at (10,60), "PART TYPE",                                  ~
               at (10,70), fac(hex(8c)),   type$                , ch(03),~
               at (11,49), fac(hex(8c)),   dempartdescr$        , ch(32),~
               at (11,02),                                               ~
                  "Quantity desired",                                    ~
               at (11,30), fac(lfac$( 6)), demquantity$         , ch(10),~
               at (12,02), "Unit Standard Cost",                         ~
               at (12,30), fac(lfac$( 7)), uscost$              , ch(10),~
               at (12,49), "Extended Std Cost:",                         ~
               at (12,68), fac(lfac$( 7)), excost$              , ch(13),~
               at (13,02),                                               ~
                  "Desired completion date",                             ~
               at (13,30), fac(lfac$( 8)), demcompdate$         , ch(08),~
               at (13,49), fac(hex(8c)),   demcompdatedescr$    , ch(32),~
               at (14,02),                                               ~
                  "Which BOM structure?",                                ~
               at (14,30), fac(lfac$( 9)), dembom$              , ch(03),~
               at (14,49), fac(hex(8c)),   dembomdescr$         , ch(32),~
               at (15,02), "Which WC (PM only)",                         ~
               at (15,30), fac(lfac$(10)), demwc$               , ch(04),~
               at (15,49), fac(hex(8c)),   demwcdescr$          , ch(32),~
               at (17,02), "Planned completion date",                    ~
               at (17,30), fac(hex(84)),   compdate$            , ch(08),~
               at (17,49), fac(hex(8c)),   compdatedescr$       , ch(32),~
               at (18,02),                                               ~
                  "Date last planned",                                   ~
               at (18,30), fac(hex(84)),   plandate$            , ch(08),~
               at (18,49), fac(hex(8c)),   plandatedescr$       , ch(32),~
               at (19,02),                                               ~
                  "Demand status",                                       ~
               at (19,30), fac(hex(84)),   demstatus$           , ch(01),~
               at (19,49), fac(hex(8c)),   demstatusdescr$      , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfmessage$(demmode%) , ch(79),~
               at (23,02),                                               ~
                  "(3)Planning Switches  (5)See BOM  (6)See PIP         (~
        ~9)Advices    (10)Routings",                                      ~
               at (24,02),                                               ~
                  "(11)Eff. Dates (12)W/C Cap (13)Instr (14)How Many (15)~
        ~Prt Scr (24)Demand Status",                                      ~
                                                                         ~
               keys(pfkey$(demmode%)),                                   ~
               key (keyhit%)

               if keyhit% <>  3% then L41566
                  gosub L62000
                  goto L41175

L41566:        if keyhit% <>  5% then L41571
                  gosub see_bom
                  goto L41175

L41571:        if keyhit% <> 13% then L41578
                  call "MANUAL" ("DEMINPUT")
                  goto L41175

L41578:        if keyhit% <> 15% then L41586
                  call "PRNTSCRN"
                  goto L41175

L41586:        if keyhit% <> 14% then L41594
                  gosub how_many
                  goto L41175

L41594:        if keyhit% <> 24% then L41610
                  gosub dem_stat
                  goto L41175

L41610:        mode%=0%
               if keyhit%=6% then mode%=1%
               if keyhit%=9% then mode%=6%
               if keyhit%=10% then mode%=3%
               if keyhit%=11% then mode%=4%
               if keyhit%=12% then mode%=8%
               if mode%=0% then L41705
               gosub review_functions
               goto L41175

L41705:        if demmode%<>1% then return
               close ws
               call "SCREEN" addr ("C", f1%(64), "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* DEMAND CODE      */~
                                    L50126,         /* DEMAND LINE      */~
                                    L50200,         /* DEMAND TYPE      */~
                                    L50250,         /* DEMAND PRIORITY  */~
                                    L50300,         /* PART CODE        */~
                                    L50350,         /* QUANTITY DESIRED */~
                                    L50400,         /* Std Costs        */~
                                    L50450,         /* REQ COMP DATE    */~
                                    L50500,         /* DEMAND BOM       */~
                                    L50600,         /* WORK CENTER      */~
                                    L50700,         /* COMPLETION DATE  */~
                                    L50750,         /* DATE LAST PLANNED*/~
                                    L50800          /* DEMAND STATUS    */
                     return

L50100:     REM TEST DATA FOR DEMAND CODE
           if demcode$ = " " then return
           if str(demcode$,1,2) = "PO" then goto L50120
           if str(demcode$,1,2) = "BO" then goto L50120
           if str(demcode$,1,2) = "WO" then goto L50120
           if str(demcode$,1,2) = "JO" then goto L50120
           if str(demcode$,1,2) = "RO" then goto L50120
           if str(demcode$,1,2) = "QC" then goto L50120
           if demline$<>" " then L50126
           return
L50120:     errormsg$="The First 2 Char of the Demand Code CANNOT be " & ~
            str(demcode$,1,2) :           return

L50126:     REM TEST DATA FOR DEMAND LINE
                if demcode$ = " " then demline$=" "
                if demcode$ = " " then return
                if demline$ = " " then demline$ = "001"
                call "STRING" addr("RJ", demline$, 3%)
                demkey$ = str(demcode$,1,16) & str(demline$,1,3)
                call "REDALT0" (#1, demkey$, 1%, f1%(1))
                if f1%(1)<> 0% then L50156
                statusmsg$="Entering new demand"
                str(demkey$,17,3) = hex(000000)
                call "PLOWNEXT" (#20, demkey$, 16%, f1%(20))
                     if f1%(20) = 0% then L50154
                errormsg$ = "Sorry, Code can't be same as existing SO#."
                fieldnr%=1%:return
L50154:         fieldnr%=2%:return
L50156:         gosub loaddemanddata
                     for fieldnr% = 3% to 13%
                     gosub'151(fieldnr%)    /* TO GET THE DESCRIPTIONS */
                     next fieldnr%
                return clear
                return clear
                if compdate$ <> " " and compdate$ <> blankdate$ then L50180
            statusmsg$="This demand is UNPLANNED, modify or plan this dem~
        ~and as desired"
            demmode%=1%
            goto editmode

L50180:     statusmsg$="This demand is now PLANNED, you must unplan prior~
        ~ to modification"
            demmode%=2%
            goto editmode

L50200:     REM TEST DATA FOR DEMAND TYPE
                demtypedescr$=" "
        if demtype$="1" then demtypedescr$="Sales order - net forecasts"
        if demtype$="2" then demtypedescr$="Sales order - don't net fcst"
        if demtype$="3" then demtypedescr$="Requisition - don't net fcst"
        if demtype$="4" then demtypedescr$="Sales forecast - auto net"
        if demtype$="5" then demtypedescr$="Sales forecast - over & above"
        if demtype$="7" then demtypedescr$="Procurement - with jump"
        if demtype$="8" then demtypedescr$="Procurement - will not jump"
        if demtype$="9" then demtypedescr$="Preventive maintenance"
        if demtypedescr$=" " then errormsg$="Invalid demand type"
        if errormsg$<>" " then return
        if demcode$ <> " "  then L50224
        if pos ("789" = demtype$) <> 0% then L50224
           errormsg$ = "Blank demand code for Procurement/PM only":return
L50224: if demtype$>"2" then L50229
           errormsg$="Sales orders must be entered via sales order input"
           return
L50229: if demtype$="9" then L50232
           if dempart$="PREVENTIVE MAINT." then dempart$,dempartdescr$=" "
           demwc$=" ":return
L50232:         dempart$="PREVENTIVE MAINT.":dembom$=" ":demrte$=" "
                dempartdescr$="PREVENTIVE MAINTENANCE":demwhse$=" "
                apprstat$=hex(00):return

L50250:     REM TEST DATA FOR PRIORITY DESIRED (A-Z)
                if dempriority$ = " " then dempriority$ = "M"
                if dempriority$ > "Z" or dempriority$ < "A" then         ~
                                errormsg$ = "Priority must be 'A' to 'Z'"
                return

L50300:     REM TEST DATA FOR PART CODE
            call "GETCODE" (#4, dempart$, " ", 0%, 0, f1%(4))
                  if f1%(4) = 0% then goto L50320
            get #4, using L50304, dempartdescr$, type$
L50304:     FMT XX(25), CH(32), XX(122), CH(3)
            convert type$ to type%, data goto L50324
            if type%>0% and type%<200% then L50328
            if type%=0% and pos("12"=demtype$)=0% then L50324
            gosub L50500
            if errormsg$<>" " then dembom$=" "
            return
L50320:           errormsg$ = "Part Not In Inventory"
                  return
L50324:           errormsg$="Build to option part, enter via sales order"
                  return
L50328:           errormsg$="Unplanned Item Type:" & type$
                  return

L50350:     REM TEST DATA FOR QUANTITY DESIRED
                call "NUMTEST" (demquantity$, .01, 9e7, errormsg$, 0.2,  ~
                                                                demquant)
*              RETURN
L50400:     REM TEST DATA FOR STANDARD COSTS
                call "STCCOSTS" (dempart$, " ", #60, 1%, stdcost)
                excost = stdcost * demquant
                call "CONVERT" (stdcost, -2.4, uscost$)
                call "CONVERT" (excost, -2.4, excost$)
                return
L50450:     REM TEST DATA FOR DESIRED COMPLETION DATE
                demcompdatedescr$=" "
                call "DATEOK" (demcompdate$, err%, errormsg$)
                if errormsg$ <> " "  then return
                temp1$ = demcompdate$
                call "DATUNFMT" (temp1$, 0%, temp$)
                call "DATE" addr ("G-", yymmdd$(1%), temp1$, ed%, u3%)
                if u3% <> 0% then ed% = 0%
                ed% = ed% + 1%
                ed% = max(0%, ed%)
                if ed% =   0% then L50486
                if ed% > 490% then L50486
                    convert str(temp$,5%,2%) to m%, data goto L50494
                    demcompdatedescr$ = str(dow$(ed%),1%,3%) & ": " ~
                     & str(months$,(m%*3%-2%),3%) & " "             ~
                     & str(temp$,7%,2%) & ", "                      ~
                     & str(temp$,1%,4%)
             if ed%<today% then                                     ~
           demcompdatedescr$=demcompdatedescr$ & hex(84) & "* * LATE * *"
                    return

L50486: errormsg$ = "Requested completion date is not within the planning~
        ~ period, respecify"
                return
L50494:         errormsg$ = "Date is NOT valid: " & demcompdate$
                return
L50500:     REM TEST DATA FOR WHICH BOM STRUCTURE?
                     if dembom$ = " " then return
                     if dembom$ = "?" then dembom$ = " "
                     if type%<500% and type%>0% then L50525
                     readkey$ = str(dempart$) & str(dembom$)
                     hdr$()="  Listed Below Are The Existing BOMs " &    ~
                                "For Part: " & dempart$
                     dembomdescr$ = hex(06) & "Select Bill Of Materials"
                     call "PLOWCODE" (#15, readkey$, dembomdescr$, 2025%,~
                                      .30, f1%(15), hdr$(), 3)
                     if f1%(15) <> 1% then goto L50516
                         dembom$ = str(readkey$,26%,3%)
                         return
L50516:              errormsg$ = "No such BOM structure on file: " &     ~
                                         dembom$
                     return
L50525:         errormsg$="This is not a manufactured part, no BOM"
                     return

L50600:     REM TEST DATA FOR WHICH WC (PM ONLY)
                demwcdescr$ = " "
                if demwc$ = " " and demtype$ <> "9" then return
                call "GETCODE" (#11, demwc$, demwcdescr$, 0%, 0, f1%(11))
                   if f1%(11) <> 0% then return
                errormsg$ = "ENTER WORK CENTER FOR PREVENTIVE MAINTENANCE"
                return

L50700:     REM TEST DATA FOR PLANNED COMPLETION DATE
                compdatedescr$=" "
                if compdate$ = " " or compdate$ = blankdate$ then return
                call "DATEOK" (compdate$, err%, errormsg$)
                if errormsg$ <> " "  then L50732
                temp1$ = compdate$
                call "DATUNFMT" (temp1$, 0%, temp$)
                search str(yymmdd$(),1)=str(temp1$,1%,6%) to cursor%() step 6
                if cursor%(1%)=0% then L50726
                d%=1%+(cursor%(1%)/6%)
                    convert str(temp$,5%,2%) to m%, data goto L50732
                    compdatedescr$         = str(dow$(d%),1%,3%) & ": "    ~
                     & str(months$,(m%*3%-2%),3%) & " "                     ~
                     & str(temp$,7%,2%) & ", "                             ~
                     & str(temp$,1%,4%)
                return
L50726:         compdatedescr$ = "Outside planning calendar"
                return
L50732:         compdatedescr$ = "Date is not valid"
                return
L50750:     REM TEST DATA FOR DATE LAST PLANNED
                plandatedescr$ = " "
                if plandate$ = " " or plandate$ = blankdate$ then return
                call "DATEOK" (plandate$, err%, errormsg$)
                if errormsg$ <> " "  then L50782
                temp1$ = plandate$
                call "DATUNFMT" (temp1$, 0%, temp$)
                search str(yymmdd$(),1)=str(temp1$,1,6) to cursor%() step 6
                if cursor%(1)=0% then L50775
                d%=1%+(cursor%(1)/6%)
                    convert str(temp$,5%,2%) to m%, data goto L50782
                     plandatedescr$        = str(dow$(d%),1%,3%) & ": "    ~
                     & str(months$,(m%*3%-2%),3%) & " "                     ~
                     & str(temp$,7%,2%) & ", "                             ~
                     & str(temp$,1%,4%)
                return
L50775:         plandatedescr$="Outside planning calendar"
                return
L50782:         plandatedescr$="Date is not valid:"
                return
L50800:     REM TEST DATA FOR DEMAND STATUS
                demstatusdescr$=" "
                if demstatus$=" " then demstatusdescr$ = hex(94) &       ~
                                          "Not PLANNED, Approvals needed"
                if demstatus$="1" then demstatusdescr$="Not PLANNED, Appr~
        ~oved"
                if demstatus$="6" then demstatusdescr$="Planned LATE, App~
        ~rovals needed"
                if demstatus$="7" then demstatusdescr$="Planned LATE, App~
        ~roved"
                if demstatus$="8" then demstatusdescr$="Planned OK, Appro~
        ~vals needed"
                if demstatus$="9" then demstatusdescr$="Planned OK, Appro~
        ~ved"
                if demstatusdescr$=" " then demstatusdescr$="Incorrect st~
        ~atus!"
                return


        rem**************************************************************~
            *      see demand status                                    *~
            *************************************************************
        dem_stat

            call "SHOSTAT" ("Linking To DEMSTAT...Just A Moment")

            call "OPENCHCK" (#43, fs%(43), f2%(43), 0%, rslt$(43))
            call "OPENCHCK" (#44, fs%(44), f2%(44), 0%, rslt$(44))
            call "OPENCHCK" (#45, fs%(45), f2%(45), 0%, rslt$(45))
            call "OPENCHCK" (#46, fs%(46), f2%(46), 0%, rslt$(46))
            call "OPENCHCK" (#47, fs%(47), f2%(47), 0%, rslt$(47))
            call "OPENCHCK" (#48, fs%(48), f2%(48), 0%, rslt$(48))
            call "OPENCHCK" (#49, fs%(49), f2%(49), 0%, rslt$(49))
            call "OPENCHCK" (#50, fs%(50), f2%(50), 0%, rslt$(50))
            call "OPENCHCK" (#51, fs%(51), f2%(51), 0%, rslt$(51))

            str(demstring$,1,16) = str(demcode$,1,16)
            str(demstring$,17,3) = str(demline$,1,3)
            return% = 0%
            gosub L31000

            call "DEMSTAT" (demstring$, #35, #1, #43, #44, #34, #8, #45, ~
                              #46, #47, #48, #49, #50, #4, #11, #7, #51, ~
                              #33, return%, 1%)

            return

L62000: rem**************************************************************~
            *      modify planning flags                                *~
            *************************************************************

              err% = today%
                 call "PLNFLSUB" (planflags$(), err%)
                    unplanopt$ = str(planflags$(), 4, 1)
                    unplanrpt$ = str(planflags$(),18, 1)
              return

L63000: rem**************************************************************~
            *      test how many by a certain date                      *~
            *************************************************************
            init (" ")                                                   ~
            errormsg$,                   /* ERROR MESSAGE              */~
            mbom$,                       /* BILL OF MATERIALS          */~
            mdate$,                      /* DUE DATE                   */~
            mpart$,                      /* PART                       */~
            mpriority$,                  /* PRIORITY                   */~
            mquantity$                   /* MAXIMUM QUANTITY           */~

            goto L63310

        how_many
            init (" ") errormsg$

            mbom$ = dembom$              /* BILL OF MATERIALS          */
            mdate$ = demcompdate$        /* DUE DATE                   */
            mpart$ = dempart$            /* PART                       */
            mpriority$ = dempriority$    /* PRIORITY                   */
            mquantity$ = demquantity$    /* MAXIMUM QUANTITY           */

L63310:     call "PLNMAXIM"                                              ~
            (errormsg$,                  /* ERROR MESSAGE              */~
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
                     #36, #6, #62, #63,  /* JBPIPXRF, RTEALTRS         */~
                     #1, #5, #12)        /* FOR PLNRSUB                */~

                    unplanopt$ = str(planflags$(), 4, 1)
                    unplanrpt$ = str(planflags$(),18, 1)

            return

        rem**************************************************************~
            *      l o a d   c a l m a s t r    d a t a                 *~
            *************************************************************

        loadcalendar

L64030:         FMT XX(2), CH(1470)
L64035:         FMT XX(2), 490*BI(4)
L64040:         FMT XX(2), 490*CH(3)

            call "READ100" (#12,"10", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64030, str(yymmdd$(),1,1470)

            call "READ100" (#12,"11", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64030, str(yymmdd$(),1471,1470)

            call "READ100" (#12,"20", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64035, yy%()

            call "READ100" (#12,"30", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64035, mm%()

            call "READ100" (#12,"40", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64035, dd%()

            call "READ100" (#12,"50", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64040, dow$()

            call "READ100" (#12,"60", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64035, mwoy%()

            call "READ100" (#12,"61", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64035, fwoy%()

            call "READ100" (#12,"70", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64035, cqoy%()

            call "READ100" (#12,"71", f1%(12))
                if f1%(12) = 0% then goto L64260
            get #12, using L64035, fqoy%()

            return

L64260:         hit = 1
                return

        REM *************************************************************~
            *  REVIEW  THE BILL OF MATERIALS                            *~
            *************************************************************

        see_bom

            call "BOMBRWSB" (#15, #24, #60, #4, dempart$, dembom$,       ~
                               1%, reviewpart$, err$)
            if err$ = " " then return

            k% = 2%
            call "ASKUSER" (k%, "* * * *  NOTE  * * * *",                ~
                   err$,                                                 ~
                   "Part Number: " & dempart$,                           ~
                   "Press any key to acknowledge.")
            return

        REM *************************************************************~
            *  REVIEW     FUNCTIONS                                     *~
            *************************************************************

        review_functions

             reviewpart$=dempart$
             REM
             call "PLNRSUB" (mode%, reviewpart$,                         ~
                             #1, #2, #4, #7, #11, #12, #15, #23,         ~
                             #24, #33, #34, #35, #40, #41, #5, #6, #16)

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

            call "SHOSTAT" ("One Moment Please.")
            end
