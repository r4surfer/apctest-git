        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   L       AAA   N   N  RRRR   N   N   GGG   EEEEE   *~
            *  P   P  L      A   A  NN  N  R   R  NN  N  G      E       *~
            *  PPPP   L      AAAAA  N N N  RRRR   N N N  G GGG  EEEE    *~
            *  P      L      A   A  N  NN  R   R  N  NN  G   G  E       *~
            *  P      LLLLL  A   A  N   N  R   R  N   N   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLANRNGE - ALLOWS PLANNING OR UNPLANNING OF A RANGE OF    *~
            *          DEMANDS. ENTER THE SELECT CRITERIA, PROGRAM PLOWS*~
            *            THROUGH DEMAND FILE AND SENDS DEMANDS THAT PASS*~
            *            THE TEST TO THE EITHER PLANSUB OR UNPLAN.      *~
            *            DEFAULT INPUT IS ALL OF EVERYTHING.            *~
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
            * 09/20/83 ! ORIGINAL                                 ! HES *~
            * 10/24/83 ! OPTION PROCESSER ADDED                   ! KAB *~
            * 11/10/83 ! UPLAN OR PLAN OPTION ADDED               ! KAB *~
            * 04/25/88 ! Now correctly sets date last planned     ! HES *~
            * 04/25/88 ! Added option to open files in shared mode! HES *~
            * 11/08/90 ! Added ln 19246/7 to stop alter of DEMCODE! MJB *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 07/15/93 ! Added Selected Demand Processing         ! MJB *~
            * 03/08/94 ! Changed record length of BOMSPEC         ! WPH *~
            * 03/18/94 ! PRR - 12724, 12886, 12965 Add more Filter! RJH *~
            *          !  Options when Planning/Unplanning. Ranges!     *~
            *          !  of Part, Part Type, MPS, Buyer/Scheduler!     *~
            *          !  Display/accept selection before process.!     *~
            *          ! Combined Input & Edit 'ACCEPT' Screens.  !     *~
            * 03/15/95 ! Corrected Unplanning functions.          ! JDH *~
            * 03/16/95 ! Added some PLOWCODEs to 50000s.          ! JDH *~
            * 01/23/96 ! Bug Fix - Reverse Buyer/Scheduler Filters! RJH *~
            * 09/06/96 ! Millie date conversion                   ! DER *~
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
            aproved$1,                   /* APROVAL STATUS (Y/N SWITCH)*/~
            aprv$1,                      /* APROVAL STATUS             */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$3,                       /* BOM FOR DEMAND             */~
            buyer$3,                     /* Buyer Code                 */~
            choice$1,                                                    ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            comp$6,                      /* COMPLETION DATE            */~
            cus$9,                       /* CUSTOMER                   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            date$(2)10,                  /* DATE RANGE                 */~
            dateaprv$10,                 /* DATE APROVED               */~
            date%(2),                    /* DATE RANGE FOR TESTING     */~
            demand$19,                   /* DEMAND CODE & LINE NUMBER  */~
            demand$(2)16,                /* DEMAND CODE RANGE          */~
            demand_scrn$(2)16,           /* DEMAND CODE RANGE (Display)*/~
            dem$(2)19,                   /* DEMAND RANGE FOR DATASAVE  */~
            descr$32,                    /* Description                */~
            disp_demand$1,               /* Display Selected Demand Flg*/~
            due$6,                       /* REQUESTED COMPLETION DATE  */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            first$6,                     /* LAST PLANNED DATE          */~
            fmbuycode$3,                 /* From Buyer Code Range      */~
            fmmps$8,                     /* From MPS Code Range        */~
            fmpart$25,                   /* From Part Number Range     */~
            fmschdcode$3,                /* From Scheduler Code Range  */~
            fmtyp$3,                     /* From Part Type Range       */~
            hdr$60,                      /* Header for ASKUSER         */~
            phdr$(2)79,                  /* PLOWCODE Header            */~
            titles$79,                   /* Select Screen Title Line   */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            last_demand_1$16,            /* Last entry of DEMAND$(1)   */~
            late$1,                      /* YES/NO FLAG                */~
            lfac$(32)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lta%(26),                    /* PRIORITY LEAD TIMES        */~
            line$(2)3,                   /* DEMAND LINE RANGE          */~
            line_scrn$(2)3,              /* DEMAND LINE RANGE(Screen)  */~
            line2$79,                    /* Screen Line #2             */~
            mode$5,                      /* File Open Mode             */~
            mpsgroup$8,                  /* MPS Group name             */~
            msg$(3)80,                   /* Messages for ASKUSER       */~
            newstat$1,                   /* STATUS OF DEMAND           */~
            paprv$1,                     /* APROVAL STATUS FOR PRINT   */~
            part$25,                     /* PART FOR DEMAND            */~
            partdescr$32,                /* PART FOR DEMAND            */~
            parttype$3,                  /* PART Type                  */~
            pdate$(2)8,                  /* DATES FOR PRINT            */~
            pf$(3)79,                    /* PF-Key Strings             */~
            pfkeys$32,                   /* PF-Keys active             */~
            plowkey$30,                  /* PLOW KEY (OBVIOUSLY!)      */~
            plowdescr$79,                /* PLOW KEY Description       */~
            plandet$3,                                                   ~
            planfast$3,                                                  ~
            prior$1,                     /* PRIORITY OF DEMAND         */~
            priority$(30)1,              /* PRIORITIES TO INCLUDE (@-Z)*/~
            printline$132,               /* PRINT LINE                 */~
            quan$10,                     /* QUANTITY TO BUILD          */~
            rtnmessage$79,               /* RETURN MSG FROM DATASAVE   */~
            readkey$19,                  /* Key for REDALT0            */~
            rte$3,                       /* RTE FOR DEMAND             */~
            schdr$3,                     /* Scheduler                  */~
            sdemand$(30)16,              /* Selected Demand Array      */~
            sline$(30)3,                 /* Selected Demand Line Array */~
            selmessage$79,               /* Input Message for Selects  */~
            stat$1,                      /* STATUS OF DEMAND           */~
            store$3,                     /* STORE NUMBER               */~
            temp$99,                     /* TEMP Variable              */~
            to$(12)2,                    /* Accept display literal     */~
            tobuycode$3,                 /* To   Buyer Code Range      */~
            tomps$8,                     /* To   MPS Code Range        */~
            topart$25,                   /* To   Part Number Range     */~
            toschdcode$3,                /* To   Scheduler Code Range  */~
            totyp$3,                     /* To   Part Type Range       */~
            typ$(10)1,                   /* TYPES TO INCLUDE (1-9)     */~
            type$1,                      /* TYPE OF DEMAND             */~
            unffmdate$8,                 /* Unformated From Date       */~
            unftodate$8,                 /* Unformated TO   Date       */~
            unpl_aprove_flag$1,          /* Unplan aproved only flag   */~
            unplanopt$1,                                                 ~
            unplanrpt$1,                                                 ~
            wc$4,                        /* WORK CENTER                */~
            whoaprv$(5)3,                /* WHO APROVED                */~
            workhits$10                  /* Demands into Work File     */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
         /* F1%(64), IN COMMON BLOCK     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

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
            * 01  ! DEMMASTR ! Demand Master File                       *~
            * 02  ! HNYMASTR ! Inventory Master File                    *~
            * 03  ! HNYALTRS ! Inventory Alternate Part File            *~
            * 04  ! BOMMASTR ! BOM relationship file                    *~
            * 05  ! PIPMASTR ! Planned Inventory Position Master File   *~
            * 06  ! RTEMASTR ! Production routing master file           *~
            * 07  ! WCMASTR  ! Work center master file                  *~
            * 08  ! WCOUT    ! Planned work center use detail rec       *~
            * 09  ! PIPIN    ! Planned inventory additions detail       *~
            * 10  ! PIPOUT   ! Planned inventory use detail rec         *~
            * 11  ! SFMASTR2 ! Sales forecast master file               *~
            * 12  ! SFCUM2   ! Cumulative sales forecast file           *~
            * 13  ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            * 14  ! JBCROSS2 ! JOB TO BOM/RTE CROSS - JOB TRACKING      *~
            * 15  ! PIPCROSS ! PIP CROSS REF (HARD PEG)                 *~
            * 16  ! ENGMASTR !                                          *~
            * 17  ! BOMSPEC  ! OPTIONS LIST                             *~
            * 18  ! JBPIPXRF !                                          *~
            * 19  ! SYSFILE2 ! System Records File                      *~
            * 20  ! RTEALTRS ! Alternate Routes File                    *~
            * 21  ! BCKLINES ! Sales Order Header File                  *~
            * 22  ! MPSGROUP ! MPS GROUPS MASTER FILE                   *~
            * 23  ! MPSITEMS ! MPS Items Master File                    *~
            * 24  ! GENCODES ! General Codes File                       *~
            * 50  ! SORTWORK ! Sort File                                *~
            * 62  ! WORK1    ! Work File                                *~
            * 63  ! WORK2    ! Work File                                *~
            * 64  ! WORK3    ! Work File (Display)                      *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select #02, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #03, "HNYALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =    1, keylen =  33                      ~

            select #04, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #05, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #06, "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #07, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #08, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #09, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #10, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #11, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #12, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

           select #13, "CALMASTR", varc, indexed,                        ~
                     recsize = 1962, keypos = 1   , keylen = 2

            select #14, "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 94,                                    ~
                        keypos=29, keylen = 19,                          ~
                        alt key 1, keypos = 1 , keylen = 47,             ~
                            key 2, keypos = 48, keylen = 47

            select #15, "PIPCROSS", varc, indexed,                       ~
                     recsize = 150,  keypos =  1  , keylen =  71,        ~
                     alt key 1, keypos = 20, keylen = 52,                ~
                         key 2, keypos = 39, keylen = 33

           select #16, "ENGMASTR" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

           select #17, "BOMSPEC" , varc, indexed,                        ~
                     recsize = 150 , keypos = 26  , keylen = 54 ,        ~
                     alt key 1, keypos = 57, keylen = 23


           select #18, "JBPIPXRF",varc, indexed, recsize=63,             ~
                        keypos=1, keylen=63,                             ~
                        alt key 1, keypos=45, keylen=19

            select #19, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize=500, keypos=1, keylen=20

            select #20, "RTEALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 34

            select #21, "BCKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 10, keylen = 19

            select #22, "MPSGROUP",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos = 1,    keylen = 8,                       ~
                        alternate key 1, keypos = 39, keylen = 8, dup,   ~
                                  key 2, keypos = 47, keylen = 8, dup

            select #23, "MPSITEMS",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen = 25,                      ~
                        alternate key 1, keypos = 26, keylen = 8, dup,   ~
                                  key 2, keypos = 34, keylen = 6, dup,   ~
                                  key 3, keypos = 40, keylen = 2, dup,   ~
                                  key 4, keypos = 42, keylen = 33

            select #24, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            select #50, "SORTWORK",                                      ~
                        consec,                                          ~
                        recsize = 132

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

            select #64, "WORK3",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 210,                                   ~
                        keypos = 1, keylen = 19,                         ~
                        alt key 1, keypos = 20, keylen = 27, dup

            call "SHOSTAT" ("Linking to Data Base for Multiple Demand Pla~
        ~nning or Unplanning")

L03660:     ask% = 2%
            call "ASKUSER" (ask%, "* * * INFORMATION REQUIRED * * *",    ~
                 "Press PF-16 to open planning files in shared mode",    ~
                 "Or, to improve performance, you may use",              ~
                 "PF-32 and attempt to open files for EXCLUSIVE use.")

            if ask% = 32% then mode$ = "IO   "
            if ask% = 16% then mode$ = "SHARE"
            if ask% <> 16% and ask% <> 32% then L03660

            call "SHOSTAT" ("Linking to Data Base for Multiple Demand Pla~
        ~nning or Unplanning")

            call "OPENFILE" (#01, mode$, f2%(01%), rslt$(01%), axd$(01%))
            call "OPENFILE" (#02, mode$, f2%(02%), rslt$(02%), axd$(02%))
            call "OPENFILE" (#03, mode$, f2%(03%), rslt$(03%), axd$(03%))
            call "OPENFILE" (#04, mode$, f2%(04%), rslt$(04%), axd$(04%))
            call "OPENFILE" (#05, mode$, f2%(05%), rslt$(05%), axd$(05%))
            call "OPENFILE" (#06, mode$, f2%(06%), rslt$(06%), axd$(06%))
            call "OPENFILE" (#07, mode$, f2%(07%), rslt$(07%), axd$(07%))
            call "OPENFILE" (#08, mode$, f2%(08%), rslt$(08%), axd$(08%))
            call "OPENFILE" (#09, mode$, f2%(09%), rslt$(09%), axd$(09%))
            call "OPENFILE" (#10, mode$, f2%(10%), rslt$(10%), axd$(10%))
            call "OPENFILE" (#11, mode$, f2%(11%), rslt$(11%), axd$(11%))
            call "OPENFILE" (#12, mode$, f2%(12%), rslt$(12%), axd$(12%))
            call "OPENFILE" (#13, mode$, f2%(13%), rslt$(13%), axd$(13%))
            call "OPENFILE" (#14, mode$, f2%(14%), rslt$(14%), axd$(14%))
            call "OPENFILE" (#15, mode$, f2%(15%), rslt$(15%), axd$(15%))
            call "OPENFILE" (#16, mode$, f2%(16%), rslt$(16%), axd$(16%))
            call "OPENFILE" (#17, mode$, f2%(17%), rslt$(17%), axd$(17%))
            call "OPENFILE" (#18, mode$, f2%(18%), rslt$(18%), axd$(18%))
            call "OPENFILE" (#19,"SHARE",f2%(19%), rslt$(19%), axd$(19%))
            call "OPENFILE" (#20, mode$, f2%(20%), rslt$(20%), axd$(20%))
            call "OPENFILE" (#21, mode$, f2%(21%), rslt$(21%), axd$(21%))
            call "OPENFILE" (#22, mode$, f2%(22%), rslt$(22%), axd$(22%))
            call "OPENFILE" (#23, mode$, f2%(23%), rslt$(23%), axd$(23%))
            call "OPENFILE" (#24, mode$, f2%(24%), rslt$(24%), axd$(24%))

            recnbr% = val(str(rslt$(1%),17%,4%),4%)
            recnbr% = max(100%, recnbr% / 2%)

            hdr$ = "***** ERROR CONDITION *****"
            if f2%(01%) = 0 then L04110
L04030:     ask% = 2%
            msg$(1) = "The DEMAND Master File is Either MISSING,"
            msg$(2) = "or is NOT AVAILABLE at this time."
            msg$(3) = "Press RETURN to EXIT this program"
            call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
            if ask% <> 0% then L04030
            goto L65000

L04110:     for u3% = 2 to 18
               if u3% = 17% then goto L04220
               if f2%(u3%) = 0 then L04220
L04140:     ask% = 2%
            msg$(1) = "One of the REQUIRED Files is Either MISSING,"
            msg$(2) = "or is NOT AVAILABLE at this time."
            msg$(3) = "Press RETURN to EXIT this program"
            call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
            if ask% <> 0% then L04140
            goto L65000

L04220:     next u3%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            gosub loadcalendar
            search yymmdd$() = date to cursor%() step 6
            if cursor%(1) = 0 then L65000
            today% = (cursor%(1) + 5)/6

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            mat lta% = con
            mat lta% = (999%) * lta%

            call "READ100" (#19, "PLANNING SYSTEM FLAG", f1%(19))
            if f1%(19)=0 then L65000
            get #19, using L09220, str(planflags$(),1,480)
L09220:         FMT XX(20), CH(480)

            unplanopt$ = str(planflags$(), 4, 1)
            unplanrpt$ = str(planflags$(),18, 1)

            get str(planflags$(), 281) using L09280, lta%()
L09280:         FMT 26*BI(4)
            str(line2$,62) = "PLANRNGE: " & str(cms2v$,1,8)

            titles$ = "Demand Number   " & hex(8c)
            str(titles$,18) = hex(ac) &  "Ln#" & hex(8c)
            str(titles$,28) = hex(ac) &  "Demand Number   " & hex(8c)
            str(titles$,46) = hex(ac) &  "Ln#" & hex(8c)
            str(titles$,56) = hex(ac) &  "Demand Number   " & hex(8c)
            str(titles$,74) = hex(ac) &  "Ln#" & hex(8c)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
        rtnmessage$, errormsg$ = " "

            init(" ") priority$(), date$(), demand$(), line$(), typ$(),  ~
                      paprv$, inpmessage$, aproved$, plandet$, planfast$,~
                      sdemand$(), sline$(), fmbuycode$, fmmps$, fmpart$, ~
                      fmschdcode$, fmtyp$, tobuycode$, tomps$, topart$,  ~
                      toschdcode$, totyp$, to$(), disp_demand$,          ~
                      unpl_aprove_flag$

            sel% = 0%

            for fieldnr% = 1% to  17%
L10230:         gosub'051(fieldnr%)
                      if enabled% = 0% then L10380
L10250:         gosub'101(fieldnr%, 0%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10330
L10280:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10250
                         if fieldnr% = 1% then L10230
                         goto L10280
L10330:               if keyhit%  =  2% then gosub L49000
                      if keyhit%  = 16% and fieldnr% = 1 then L65000
                      if keyhit% <>  0% then       L10250
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10250
L10380:         next fieldnr%
                fieldnr% = 0%
                if demand$(1%) <> "SELECT" then editpg1
                sel% = 1%
                goto L12000

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%

            gosub'101(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then       datasave
                  if keyhit%  =  9% and sel% = 1% then editsel
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11300:     fieldnr% = cursor%(1%) - 5%
            if fieldnr%    <  1% or fieldnr% >  17% then editpg1
            if fieldnr%    =  9% then editpg1
            if cursor%(1%) = 6% and cursor%(2%) > 50% then fieldnr% = 2%
            if cursor%(1%) = 7%  then fieldnr% = 3%
            if cursor%(1%) = 7% and cursor%(2%) > 50% then fieldnr% = 4%
            if cursor%(1%) > 7%  then fieldnr% = cursor%(1%) - 3%
            if cursor%(1%) > 14%  then fieldnr% = cursor%(1%) - 4%
            if cursor%(1%) = 17% and cursor%(2%) > 50% then fieldnr% = 14%
            if cursor%(1%) > 17%  then fieldnr% = cursor%(1%) - 3%

            if fieldnr% = lastfieldnr% then editpg1

            gosub'051(fieldnr%)    /* SET EDTMESSAGE$ */
                if enabled% = 0% then editpg1
L11560:     gosub'101(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11560
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11560
                  lastfieldnr% = fieldnr%
                  if demand$(1%)  = "ALL"  and fieldnr% < 5%             ~
                                           then  lastfieldnr% = 1%
            goto L11300

        demand_range_edit_loop    /* Force Modification of Demand Range */
                                  /* when 'ALL' changes to valid Demand */
            if last_demand_1$ <> "ALL"  then  return
            for fieldnr% = 2% to 4%
                gosub'051(fieldnr%)    /* SET EDTMESSAGE$ */
                    if enabled% = 0% then L11740
L11710:         gosub'101(fieldnr%, 2%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then L11710
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L11710
                      lastfieldnr% = fieldnr%
L11740:     next fieldnr%
            if demand$(1%)  = "ALL"  then  lastfieldnr% = 1%
            return

L12000: REM *************************************************************~
            *        M A N A G E   S E L E C T   S C R E E N            *~
            *                                                           *~
            * Input/Edit for Selected Demands Screen                    *~
            *************************************************************
            edit% = 0%
            selmessage$ = "Enter Selected Demands For Processing" &      ~
                          " (Enter Left to Right)"
L12140:     gosub'102(edit%)
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then gosub L49000
                if keyhit%  = 16% then L65000
                if keyhit% <>  0% then L12140
            gosub'152
                if errormsg$ <> " " then L12140
        editsel
            selmessage$ = "Press RETURN for Full Screen Edit"
            edit% = 1%
L12340:     gosub'102(edit%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then       datasave
                  if keyhit%  =  9% then       L11000
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L12340
            goto L12000

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *************************************************************
        datasave
            call "WORKOPEN" (#50, "OUTPT", 5000%, f2%(50))
            if disp_demand$ <> "Y" then L18025
                call "WORKOPEN" (#64, "IO", recnbr%, f2%(64%))
L18025:     call "SHOSTAT" ("Scanning the Demand File")
            choice$="P"
            if keyhit%=16% then L18035
                 choice$="U" : aproved$ = unpl_aprove_flag$
L18035:     hits%, hitsplnd%, hitsunplnd%, page%, process_loop%,         ~
                                                  workhits% = 0%
            pline% = 999%
            if sel% = 1% then process_selected
            mat  demand_scrn$ = demand$  :  mat line_scrn$ = line$
            if demand$(1) <> "ALL" then L18065
            init(hex(00)) demand$(1), line$(1)
            init(hex(ff)) demand$(2), line$(2)
L18065:     if line$(1) = " " then init(hex(00)) line$(1)
            if line$(2) = " " then init(hex(ff)) line$(2)
            if demand$(2) = " " then demand$(2) = demand$(1)
            dem$(1) = str(demand$(),,16) & str(line$(),,3)
            dem$(2) = str(demand$(),17,16) & str(line$(),4,3)
            call "DATUNFMT" (date$(1))
            call "DATUNFMT" (date$(2))
                                         /* LOGIC MAY BE CONFUSING, BUT */
            for j% = 1 to len(typ$())        /* IT PUTS A GOOD DENT IN */
            for k% = 1 to len(priority$())    /* THE HIT/MISS RATIO... */
            plowkey$ = str(typ$(),j%,1) &                                ~
                             str(priority$(),k%,1) & str(date$(1),,6)
L18125:     call "PLOWNEXT" (#1, plowkey$, 2%, f1%(1))
                     if f1%(1) = 0 then L18455
            gosub L35000                 /* LOAD DEMAND RECORD */
            if due$ < date$(1) or due$ > date$(2) then L18125
            if demand$ < dem$(1) or demand$ > dem$(2) then L18125
            /* Checking Part specific ranges */
            if sel% = 1% then L18152
                gosub check_part_criteria
                if ok% = 0% then L18125

L18152:     if stat$ > "1" then L18860
            if choice$="U" then goto L18125
L18160:     if aproved$ = "Y" and stat$ = " " then L18125
            search yymmdd$() = due$ to cursor%() step 6
            due% = (cursor%(1) + 5)/6
            if due% <> 0 then L18185
                if sel% = 1% then L30420 else L18125
L18185:     ltss%=val(prior$,1)-64%
            if ltss% < 1% or ltss% > 26% then L18205
            if due%-lta%(ltss%) <= today% then L18205
                if sel% = 1% then L30420 else L18125
L18205:  inpmessage$="CURRENTLY PLANNING DEMAND CODE : "&str(demand$,,16)
         inpmessage$=inpmessage$ & ",  LINE : " & str(demand$,17,3)
            pdate$(1) = due$
            call "DATEFMT" (pdate$(1))
            planflag%=5%
            if plandet$="YES" then planflag%=planflag%+10%
            if planfast$<>"YES" then L18255
            if type$="1" then type$="2"
            if type$="4" or type$="5" then type$="3"
            prior$="@"
L18255:
            if demand$ <> " " and type$ <> " " then                      ~
                              planflag% = planflag% + 100000%
            /* Should We Display First ??? */
            if disp_demand$ <> "Y" then L18273
                gosub write_temp_list_of_demands
                goto L18450   /* Get Next Demand from DEMMASTR */

L18273:     call "SHOSTAT" (inpmessage$)
L18275:     call "PLANSUB" (str(demand$,,16), str(demand$,17,3), type$,  ~
                            prior$, part$, qty, due%, store$, rte$, bom$,~
                            errormsg$, first%, final%, today%, planflag%,~
                            #3, #4, #5, #6, #14, #7, #8, #9, #10, #11,   ~
                            #12, #15, #16, #17, #18, #20, #62, #63)
            hits% = hits% + 1
            if planflag% < 1% then L18320
            if final% < 1% then L18320
            if final% <= 490% then L18355
L18320:           paprv$ = "Y" : late$ = " "  /* HERE IF UNSUCCESFUL */
                  if stat$ = " " then paprv$ = "N"
                  pdate$(2) = " "
                  gosub print_it
L18340:           if sel% = 1% then L30420      /* Selected Demand List */
                  if process_loop% = 1% then loop_disp_workfile else L18125

        REM   =====================  SET NEW STATUS  ===================
L18355:     if stat$ = " " then L18380        /* STAT$ is "1" if Aprvd */
                  if final% > due% then newstat$ = "7"        /* Late */ ~
                  else newstat$ = "9"                      /* On Time */
                  paprv$ = "Y"
                  goto L18405
L18380:     if final% > due% then newstat$ = "6"              /* Late */ ~
                  else newstat$ = "8"                      /* On Time */
                  paprv$ = "N"

        REM   ==============  SET PLANNED COMPLETION DATE  =============
L18405:     comp$, pdate$(2) = yymmdd$(final%)
            first$           = yymmdd$(first%)
            call "DATEFMT" (pdate$(2))
            late$ = "N"
            if final% > due% then late$ = "Y"  /* LATE$ IS PRINT ONLY */
            gosub print_it
            first$ = date
            gosub L36000        /* RE-SAVE THE DEMAND */
            if sel% = 1% then L30420
L18450:     if process_loop% = 1% then loop_disp_workfile else L18125
L18455:     next k%
            next j%

            if disp_demand$ = "Y" then gosub process_display_workfile    ~
                                  else L18470
            if ok% = 0% then goto L65000   /* Didn't Accept Demand List */

L18470: REM   ====== ALL DONE, TELL THEM THE SUMMARIZED RESULTS  =======
            if choice$<>"P" then L18525
            rtnmessage$ = "     UNPLANNED DEMANDS WERE FOUND THAT MEET TH~
        ~E SELECTION CRITERIA"
            if hits% = 0 then str(rtnmessage$,,4) = "  NO"  else         ~
            convert hits% to  str(rtnmessage$,,4), pic(####)
            errormsg$ = "OUT OF THOSE,      WERE SUCCESSFULLY PLANNED"
            if hitsplnd% = 0 then str(errormsg$,15,4) = "NONE"  else     ~
            convert hitsplnd% to  str(errormsg$,15,4), pic(####)
            goto L18565

L18525:     rtnmessage$ = "     PLANNED DEMANDS WERE FOUND THAT MEET THE ~
        ~SELECTION CRITERIA"
            if hits% = 0 then str(rtnmessage$,,4) = "  NO"  else         ~
            convert hits% to  str(rtnmessage$,,4), pic(####)
            errormsg$ = "OUT OF THOSE,      WERE SUCCESSFULLY UNPLANNED"
            if hitsunplnd% = 0 then str(errormsg$,15,4) = "NONE"  else   ~
            convert hitsunplnd% to  str(errormsg$,15,4), pic(####)

L18565:     select printer(134)
            close #50
            call "WORKOPN2" (#50, "INPUT", 0%, f2%(50))
L18580:     if pline% > 60% then gosub print_heading
                read #50, using L18615, printline$, eod goto L18620
                print printline$
                read #50, using L18615, printline$, eod goto L18620
                print printline$  :  print
                pline%=pline%+3%
                goto L18580
L18615:              FMT CH(132)
L18620:         print rtnmessage$
                print errormsg$

        REM Now Close The Printer, Scratch Workfile, And End The Job
               close printer
               call "GETNAMES" addr (#50, workfile$, worklib$, workvol$)
               close #50  :    f2%(50) = 1%
               call "SCRATCH" addr ("F", workfile$, worklib$, workvol$,  ~
                                    "B", " ", u3%)
               goto L65000       /* PRR 6014- Formerly GOTO INPMODE */

        print_it   /* Prints The Demand, Prints Header When Needed */
            call "DESCRIBE" (#2, part$, partdescr$, 0%, f1%(2))
            if f1%(2) = 0 then partdescr$ = " "
            init (" ") printline$
            put printline$, using L18745, str(demand$,,16),               ~
                  str(demand$,17,3), type$, prior$, paprv$, part$,       ~
                  partdescr$, qty, pdate$(1), pdate$(2), late$
            write #50, using L18730, printline$
            init (" ") printline$
            put printline$, using L18755, errormsg$
            write #50, using L18730, printline$
L18730:     FMT CH(132)
        return

L18745: %################ ###    #       #        #     #################~
        ~######## ###################### ########.## ######## ########   #
L18755: %################################################################~
        ~################

        print_heading
            if page% <> 0 then print using L18845
            print page
            page% = page% + 1
            print using L18820, page%, date$  :  print
            print using L18830
            print using L18845
            pline% = 5
        return

L18820: %PAGE :######                                MULTIPLE DEMAND PLAN~
        ~NING STATUS REPORT                             RUN DATE : ########
L18830: %DEMAND          LINE  TYPE  PRIORITY  APPROVED  PART NUMBER     ~
        ~          DESCRIPTION              QUANTITY DUE DATE  PLANNED LAT~
        ~E
L18845: %================ ===    =       =        =     =================~
        ~======== ====================== =========== ======== ========   =

L18860: REM UNPLAN LOGIC FOR THAT CHOICE
            if choice$="P" then goto L18125
L18870:     if aproved$  <> "N" then L18880
            if (stat$ = "7" or stat$ = "9") and sel% = 1% then L30420     ~
                                                          else L18125
L18880:     search yymmdd$() = comp$ to cursor%() step 6
            ed%  = (cursor%(1) + 5)/6
            if ed% <> 0% then L18900
                if sel% = 1% then L30420 else L18125
L18900:     search yymmdd$() = due$ to cursor%() step 6
            due% = (cursor%(1) + 5)/6
            if due% > 0% then L18916
                if sel% = 1% then L30420 else L18125
L18916:     /* Should We Display First ??? */
            if disp_demand$ <> "Y" then L18925
                gosub write_temp_list_of_demands
                goto L18340   /* Get Next Demand from DEMMASTR */

L18925:     call "UNPLAN" (str(demand$,,16), str(demand$,17,3), type$,   ~
                 ed%, due%, part$, qty, today%, unplanopt$, unplanrpt$,  ~
                 #14, #5, #7, #8, #9, #10, #11, #12, #15, #17)

            hits% = hits% + 1%
            errormsg$ = "UNPLANNED AS REQUESTED"
            pdate$(1)=due$:pdate$(2)=comp$
            call "DATEFMT" (pdate$(1)) : call "DATEFMT" (pdate$(2))
            if comp$>due$ then late$="Y" else late$="N"
            gosub print_it
            if stat$=" " or stat$="6" or stat$="8" then newstat$ = " "
            if stat$="1" or stat$="7" or stat$="9" then newstat$ = "1"
            first$,comp$=" "
            gosub L36000
            if sel% = 1% then L30420
            if process_loop% = 1% then loop_disp_workfile else L18125

        check_part_criteria  /* Testing Part Data against Range items */
            ok% = 0%
            if fmpart$ <> "ALL"  and                                     ~
               (part$ < fmpart$ or part$ > topart$)  then  return
            if fmtyp$ = "ALL" and fmbuycode$ = "ALL" and                 ~
                                  fmschdcode$ = "ALL" then L19230
            call "READ100" (#02, part$, f1%(2%))
            if f1%(2%) = 0% then return
            get #02 using L19080, parttype$, buyer$, schdr$
L19080: FMT POS(180), CH(3), POS(200), CH(3), POS(309), CH(3)
            if fmtyp$ <> "ALL" and                                       ~
               (parttype$ < fmtyp$ or parttype$ > totyp$)  then  return
            if parttype$ >= "001" and parttype$ <= "199" then return
            if parttype$ >= "200" and parttype$ <= "499" then L19170
                /* Test Manufactored Part against Schedular */
            if fmschdcode$ <> "ALL"  and                                 ~
               (schdr$ < fmschdcode$ or schdr$ > toschdcode$) then return
            goto L19220
L19170:         /* Test Purchased Part against Buyer     */
            if fmbuycode$ <> "ALL"  and                                  ~
               (buyer$ < fmbuycode$ or buyer$ > tobuycode$) then return
            goto L19220

L19220:     /* Check MPS Group Range */
L19230:     if fmmps$ = "ALL" then L19290
            call "READ100" (#23, part$, f1%(23%))
            if f1%(23%) = 0% then return          /* No MPS Group */
            get #23 using L19270, mpsgroup$
L19270: FMT POS(26), CH(8)
            if mpsgroup$ < fmmps$ or mpsgroup$ > tomps$ then return
L19290:     ok% = 1%
            return

        write_temp_list_of_demands
            workhits% = workhits% + 1%
            if choice$ = "U" then L19450
                /* Plan Workfile */
            put #64 using L19420, demand$,plowkey$, type$,                ~
                            prior$, part$, qty, due%, store$, rte$, bom$,~
                            errormsg$, first%, final%, today%, planflag%,~
                            pdate$(1%)
            write #64
            return

L19420: FMT CH(19), CH(27), CH(1), CH(1), CH(25), PD(14,4), BI(4), CH(3),~
            CH(3), CH(3), CH(79), BI(4), BI(4), BI(4), BI(4), CH(8)

L19450:         /* Un-Plan Workfile */
            put #64 using L19520, demand$,plowkey$, type$,                ~
                           ed%, due%, part$, qty, today%, unplanopt$,    ~
                           unplanrpt$, due$, comp$
            write #64
            return

L19520: FMT CH(19), CH(27), CH(1), BI(4), BI(4), CH(25), PD(14,4), BI(4),~
            CH(1), CH(1), CH(6), CH(6)

            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20165,         /* BEGINNING DEMAND */~
                                    L20205,         /* BEGINNING LINE   */~
                                    L20250,         /* ENDING DEMAND    */~
                                    L20295,         /* ENDING LINE      */~
                                    L20340,         /* Part Range       */~
                                    L20390,         /* Type Range       */~
                                    L20440,         /* MPS Group Range  */~
                                    L20490,         /* Buyer Code Range */~
                                    L20545,         /* Scheduler Range  */~
                                    L20600,         /* Date Range       */~
                                    L20655,         /* Types to include */~
                                    L20700,         /* PRIORITIES       */~
                                    L20755,         /* Plan Approved    */~
                                    L20770,         /* UnPlan Approved  */~
                                    L20785,         /* Display Flag     */~
                                    L20830,         /* DETAIL?          */~
                                    L20860          /* FAST PLAN        */
                     return

L20165: REM Default/Enable For Enter Beginning Demand Code
            if sel% = 1% then return
            inpmessage$ = "(ALL) To Process All Eligible Demands, 'SELECT~
        ~' To Enter Specific Demands"
                if demand$(1%) = " " then  demand$(1%) = "ALL"
                enabled% = 1%
                return

L20205: REM Default/Enable For Enter Beginning Demand Line
            if sel% = 1% then return
                if demand$(1) = "ALL" or demand$(1) = "SELECT" then return
            inpmessage$ = "Which Line Of This Demand Do You Want To Start~
        ~ With ? (Default Is First Line)"
                if line$(1%) = " "   then line$(1%) = "  1"
                enabled% = 1%
                return

L20250: REM Default/Enable For Enter Ending Demand Code
            if sel% = 1% then return
                if demand$(1) = "ALL" or demand$(1) = "SELECT" then return
            inpmessage$ = "Enter The Last Demand To Consider. (Default Is~
        ~ BEGINNING DEMAND)"
                if demand$(2%) = " " then demand$(2%) = demand$(1%)
                enabled% = 1%
                return

L20295: REM Default/Enable For Enter Ending Demand Line
            if sel% = 1% then return
                if demand$(1) = "ALL" or demand$(1) = "SELECT" then return
            inpmessage$ = "Which Line Of The Ending Demand Do You Want To~
        ~ End With ?(Default Is Last Line)"
                if line$(2%) = " " then line$(2%) = "999"
                enabled% = 1%
                return

L20340: REM Def/Enable Part Range                  PART$
            if sel% = 1% then return
            inpmessage$ = "Enter the Part Number Range To be " &         ~
                                "Included or 'ALL' for all Parts."
            to$(5%) = "TO"
            if fmpart$ = " " and topart$ = " " then fmpart$ = "ALL"
            if demand$(1%) = "SELECT" then return
                enabled% = 1%
                return

L20390: REM Def/Enable Part Type Range             FMTYP$ / TOTYP$
            if sel% = 1% then return
            inpmessage$ = "Enter the Part Type Range To be " &           ~
                           "Included or 'ALL' for all Part Types."
            to$( 6%) = "TO"
            if fmtyp$ = " " and totyp$ = " " then fmtyp$ = "ALL"
            if demand$(1%) = "SELECT" then return
                enabled% = 1%
                return

L20440: REM Def/Enable MPS Group Range             FMMSP$ / TOMPS$
            if sel% = 1% then return
            inpmessage$ = "Enter the MPS Group Range To be " &           ~
                                "Included or 'ALL' for all MPS Groups."
            to$(7%) = "TO"
            if fmmps$ = " " and tomps$ = " " then fmmps$ = "ALL"
            if demand$(1%) = "SELECT" then return
                enabled% = 1%
                return

L20490: REM Def/Enable Buyer Code Range          FMBUYCODE$ / TOBUYCODE$
            if sel% = 1% then return
            inpmessage$ = "Enter the Buyer Code Range To be " &          ~
                                "Included or 'ALL' for all Buyer Codes."
            to$(8%) = "TO"
            if fmbuycode$ = " " and tobuycode$ = " "                     ~
                                          then fmbuycode$ = "ALL"
            if demand$(1%) = "SELECT" then return
                enabled% = 1%
                return

L20545: REM Def/Enable Scheduler Code Range     FMSCHDCODE$ / TOSCHDCODE$
            if sel% = 1% then return
            inpmessage$ = "Enter the Scheduler Code Range To be " &      ~
                           "Included or 'ALL' for all Scheduler Codes."
            to$(9%) = "TO"
            if fmschdcode$ = " " and toschdcode$ = " "                   ~
                                          then fmschdcode$ = "ALL"
            if demand$(1%) = "SELECT" then return
                enabled% = 1%
                return

L20600: REM Default/Enable For Due Date Range
            if sel% = 1% then return
            inpmessage$ = "ONLY Demands That Are Due Within This Date Ran~
        ~ge Will Be Processed."
            to$(10%) = "TO"
            if (date$(1%) = " " or date$(1%) = blankdate$) and ~
               (date$(2%) = " " or date$(2%) = blankdate$) then ~
                date$(1%) = "ALL"
            if demand$(1%) = "SELECT" then return
                enabled% = 1%
                return

L20655: REM Default/Enable For Types To Include (1-9)
            if sel% = 1% then return
            if typ$() = " "  then  typ$() = "123456789"
            inpmessage$ = "Enter 'RANGE' or specific Part Types to Proces~
        ~sed."
                if demand$(1) = "SELECT" then return
                enabled% = 1
                return

L20700: REM Default/Enable For Priorities To Include (A-Z)
            if sel% = 1% then return
            inpmessage$ = "ONLY Demands With Priorities In This List Will~
        ~ Be Processed."
                for u3% = 1 to 26
                     str(priority$(),u3%,1) = bin(u3%+64,1)
                next u3%
                if demand$(1) = "SELECT" then return
                enabled% = 1%
                return

L20755: REM Default/Enable For Approved Only
                if aproved$ = " "  then aproved$ = "N"
            inpmessage$ = "Enter 'Y' to Include Only Approved Demands"
                enabled% = 1%
                return

L20770: REM Default/Enable For Approved Only
                if unpl_aprove_flag$ = " "  then unpl_aprove_flag$ = "Y"
            inpmessage$ = "Enter 'Y' to Unplan ALL, 'N' to Unplan if NOT"~
                           &  " Approved"
                enabled% = 1%
                return

L20785: REM Default/Enable For Selected Demand Display    DISP_DEMAND$
                if demand$(1%) <> "SELECT" then L20800
                      disp_demand$ = "N" :  return
L20800:         if disp_demand$ = " "  then disp_demand$ = "Y"
            inpmessage$ = "Enter 'Y' to Display Selected Demands Prior" &~
                              " to Planning/Unplanning."
                enabled% = 1%
                return

L20830: REM Default For Detail
                if plandet$=" " then plandet$="N"
            inpmessage$="YES to cause full detail to print with each PLAN"
                enabled%=1%
                return

L20860: REM Fast Plan
                if planfast$=" " then planfast$="N"
            inpmessage$="YES for automatic change to EXPEDITE priority"
                enabled%=1%
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
            call "FILEBGON" (#50)
            call "FILEBGON" (#62)
            call "FILEBGON" (#63)
            call "FILEBGON" (#64)
            goto inputmode

        REM *************************************************************~
            *     P R O C E S S   S E L E C T E D   D E M A N D S       *~
            *************************************************************
        process_selected
            if (pu% = 1% and choice$ = "P") or                           ~
               (pu% = 2% and choice$ = "U") then L30180
                errormsg$="Wrong PF-KEY Hit for Current Selected Demands"
                call "FILEBGON"(#50)
                goto editsel
L30180:     for i% = 1% to 30%
                if sdemand$(i%) = " " then L30420
                readkey$ = str(sdemand$(i%)) & str(sline$(i%))
                call "REDALT0" (#1, readkey$, 1%, f1%(1%))
                    if f1%(1%) = 0% then L30420
                gosub L35000     /* Load Demand Record */
                plowkey$ = key(#1)
                if choice$ = "U" then do_unplan
                    goto L18160

        do_unplan
                goto L18870
L30420:     next i%

            goto L18470

        REM *************************************************************~
            *        L O O P  T H R U   T H E  D E M A N D S            *~
            *                                                           *~
            * PROCESSES THE DEMANDS FROM THE DISPLAY WORK FILE          *~
            *************************************************************
        process_display_workfile
            convert workhits% to workhits$ , pic(#######)
L31070:     plowdescr$ = hex(06) & "Display of Selected Demands"
            if choice$ = "U" then temp$ = "Unplan" else temp$ = "Plan"

            call "PLOWCODE" (#64, " ", plowdescr$, 0%, 0, f1%(64%))
L31100:     ask% = 0%
            call "ASKUSER" (ask%, "***  ACCEPT SELECTED DEMANDS ***",    ~
                           workhits$ & " Selected"                  ,    ~
                           "Press PF16 to "& temp$ &" Displayed Demands",~
                           "Press PF12 to Start Over               " )
            if ask% <> 12% then L31170         /* Else Startover Stuff */
L31152:         keyhit% = 2%
                call "STARTOVR" (keyhit%)
                if keyhit% = 1% then L31070      /* Back to Plow Screen */
                if keyhit% <> 0% then L31152               /* Try Again */
              /* Startover Return to *EDIT* Screen  */
                mat  demand$ = demand_scrn$  :  mat line$ = line_scrn$
                call "FILEBGON" (#50)  :   call "FILEBGON" (#64)
                call "DATEFMT" (date$(1%)) :  call "DATEFMT" (date$(2%))
                inpmessage$ = "To Modify Displayed Values, Position " &  ~
                              "Cursor To Desired Value And Press (RETURN)"
                goto  editpg1
L31170:     if ask% <> 16% then L31100
            ok% = 1%
            process_loop% = 1%
            plowkey$ = hex(00)
            call "SHOSTAT" (temp$ & "ning Selected Demands")
            call "REDALT2" (#64, plowkey$, 1%, f1%(64%))
            goto L31250
          loop_disp_workfile
            call "READNEXT"  (#64, f1%(64%))
L31250:     if f1%(64%) = 0% then return
            if choice$ = "U" then L31340
           /* Process Planned Demands */
            get #64 using L19420, demand$, plowkey$, type$,               ~
                            prior$, part$, qty, due%, store$, rte$, bom$,~
                            errormsg$, first%, final%, today%, planflag%,~
                            pdate$(1%)
            gosub get_real_demand
            goto L18275   /* call PLANSUB */

L31340:    /* Process Planned Demands */
            get #64 using L19520, demand$, plowkey$, type$,               ~
                           ed%, due%, part$, qty, today%, unplanopt$,    ~
                           unplanrpt$, due$, comp$
            gosub get_real_demand
            goto L18925   /* call UNPLAN sub */

        get_real_demand
            call "READ100" (#1, plowkey$, f1%(1%))
                if f1%(1%) = 0% then return /* Whoops!!! */
            gosub L35000
            return

L35000: REM *************************************************************~
            *        L O A D  T H E  D E M A N D  R E C O R D           *~
            *                                                           *~
            * LOADS THE DEMAND FROM THE FILE.IGNORES PREV PLAN COMP DATE*~
            *************************************************************

            get #1, using L35130, stat$, type$, prior$, due$, demand$,    ~
                    part$, quan$, wc$, bom$, rte$, store$, first$, comp$,~
                    cus$, aprv$, whoaprv$(), dateaprv$
            qty = 0
            convert quan$ to qty, data goto L35120
            if type$="9" then str(part$,22,4)=str(wc$,1,4)
L35120:     return
L35130: FMT                      /* FILE: DEMMASTR                     */~
            CH(1),               /* Status indicator for level 2 plann */~
            CH(1),               /* Type - used generically for specia */~
            CH(1),               /* priority code                      */~
            CH(6),               /* Date delivery requested            */~
            CH(19),              /* Demand code (ie: sales order numbe */~
                                 /* Demand line (ie: SO line if SO dem */~
            CH(25),              /* Part code                          */~
            CH(10),              /* Quantity of something in packed de */~
            CH(4),               /* work-center code                   */~
            CH(3),               /* The specific BOM identifier for al */~
            CH(3),               /* The specific routing for multiple  */~
            CH(3),               /* Warehouse or Stores                */~
            CH(6),               /* Date last planned - for level 2 de */~
            CH(6),               /* Planned completion date for level  */~
            CH(9),               /* customer code                      */~
            CH(1),               /* approval status byte               */~
            5*CH(3),             /* id of user granting approval       */~
            CH(10)               /* date approval granted mmdd         */~

L36000: REM *************************************************************~
            *           S A V E  D E M A N D  R E C O R D               *~
            *                                                           *~
            * SAVE WITH NEW STATUS AND NEW DATES.                       *~
            *************************************************************

            call "READ101" (#1, plowkey$, f1%(1))  /* HOLD MODE */
                     if f1%(1) = 1 then L36130
                     print "DEMAND MASTER RECORD -" & demand$ & "- COULD ~
        ~NOT BE READ IN HOLD MODE  *PLAN ABORTED* *PLAN ABORTED*"
                     pline% = pline% + 1
                     return

L36130:     delete#1
            if type$="9" then init(" ") str(part$,22,4)
            put #1, using L36220, newstat$, type$, prior$, due$, demand$, ~
                    part$, quan$, wc$, bom$, rte$, store$, first$, comp$,~
                    cus$, aprv$, whoaprv$(), dateaprv$
            write #1
            if choice$="P" then hitsplnd% = hitsplnd% + 1%  else         ~
                hitsunplnd% = hitsunplnd% +1%
            goto L36420
L36220: FMT                      /* FILE: DEMMASTR                     */~
            CH(1),               /* Status indicator for level 2 plann */~
            CH(1),               /* Type - used generically for specia */~
            CH(1),               /* priority code                      */~
            CH(6),               /* Date delivery requested            */~
            CH(19),              /* Demand code (ie: sales order numbe */~
                                 /* Demand line (ie: SO line if SO dem */~
            CH(25),              /* Part code                          */~
            CH(10),              /* Quantity of something in packed de */~
            CH(4),               /* work-center code                   */~
            CH(3),               /* The specific BOM identifier for al */~
            CH(3),               /* The specific routing for multiple  */~
            CH(3),               /* Warehouse or Stores                */~
            CH(6),               /* Date last planned - for level 2 de */~
            CH(6),               /* Planned completion date for level  */~
            CH(9),               /* customer code                      */~
            CH(1),               /* approval status byte               */~
            5*CH(3),             /* id of user granting approval       */~
            CH(10)               /* date approval granted mmdd         */~

L36420:     call "READ101" (#21, demand$, f1%(21))
                if f1%(21) = 0 then return
            put #21, using L36450, type$, prior$
L36450:         FMT POS(240), CH(1), CH(1)
            rewrite #21
            return

        rem**************************************************************~
            *      l o a d   c a l m a s t r    d a t a                 *~
            *************************************************************

        loadcalendar

            call "READ100" (#13,"10", f1%(12))
                if f1%(12) = 1 then goto L37100
                hit = 1
                return
L37100:     get #13, using L37110, str(yymmdd$(),1,1470)
L37110:         FMT XX(2), CH(1470)

            call "READ100" (#13,"11", f1%(12))
                if f1%(12) = 1 then goto L37170
                hit = 1
                return
L37170:     get #13, using L37180, str(yymmdd$(),1471,1470)
L37180:         FMT XX(2), CH(1470)
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************
            deffn'101(fieldnr%, edit%)
            if fieldnr% > 0% then init(hex(8c)) lfac$() else             ~
                                  init(hex(84)) lfac$()
            gosub setpf_1

                  on fieldnr% gosub L40195,         /* BEGINNING DEMAND */~
                                    L40210,         /* BEGINNING LINE   */~
                                    L40195,         /* ENDING DEMAND    */~
                                    L40210,         /* ENDING LINE      */~
                                    L40195,         /* Part Range       */~
                                    L40195,         /* Part Type Range  */~
                                    L40195,         /* MPS Group Range  */~
                                    L40195,         /* Buyer Code Range */~
                                    L40195,         /* Scheduler Range  */~
                                    L40195,         /* Date Range       */~
                                    L40210,         /* Demand Types     */~
                                    L40195,         /* PRIORITIES       */~
                                    L40195,         /* Plan Approved    */~
                                    L40195,         /* Unplan approved  */~
                                    L40195,         /* Display Flag     */~
                                    L40195,         /* DETAIL?          */~
                                    L40195          /* FAST PLAN        */
                     goto L40230

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40195:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40210:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "PLAN OR UNPLAN MULTIPLE DEMANDS",                     ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), rtnmessage$            , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Enter BEGINNING Demand Code",                ~
               at (06,32), fac(lfac$( 1%)), demand$(1%)         , ch(16),~
               at (06,50), "at LINE",                                    ~
               at (06,59), fac(lfac$( 2%)), line$(1%)           , ch(03),~
               at (07,08), "ENDING Demand Code",                         ~
               at (07,32), fac(lfac$( 3%)), demand$(2%)         , ch(16),~
               at (07,50), "at LINE",                                    ~
               at (07,59), fac(lfac$( 4%)), line$(2%)           , ch(03),~
                                                                         ~
               at (08,08), "Part Number",                                ~
               at (08,20), fac(lfac$( 5%)), fmpart$             , ch(25),~
               at (08,47), fac(hex(8c)),   to$(5%)              , ch( 2),~
               at (08,50), fac(lfac$( 5%)), topart$             , ch(25),~
                                                                         ~
               at (09,08), "Part Type  ",                                ~
               at (09,37), fac(lfac$( 6%)), fmtyp$              , ch( 3),~
               at (09,47), fac(hex(8c)),   to$( 6%)             , ch( 2),~
               at (09,50), fac(lfac$( 6%)), totyp$              , ch( 3),~
                                                                         ~
               at (10,08), "MPS Group  ",                                ~
               at (10,37), fac(lfac$( 7%)), fmmps$              , ch( 8),~
               at (10,47), fac(hex(8c)),   to$(7%)              , ch( 2),~
               at (10,50), fac(lfac$( 7%)), tomps$              , ch( 8),~
                                                                         ~
               at (11,08), "Buyer Code ",                                ~
               at (11,37), fac(lfac$( 8%)), fmbuycode$          , ch( 3),~
               at (11,47), fac(hex(8c)),   to$(8%)              , ch( 2),~
               at (11,50), fac(lfac$( 8%)), tobuycode$          , ch( 3),~
                                                                         ~
               at (12,08), "Scheduler Code",                             ~
               at (12,37), fac(lfac$( 9%)), fmschdcode$         , ch( 3),~
               at (12,47), fac(hex(8c)),   to$(9%)              , ch( 2),~
               at (12,50), fac(lfac$( 9%)), toschdcode$         , ch( 3),~
                                                                         ~
               at (13,08), "Due Date   ",                                ~
               at (13,37), fac(lfac$(10%)), date$(1%)           , ch(10),~
               at (13,48), fac(hex(8c)),   to$(10%)             , ch( 2),~
               at (13,51), fac(lfac$(10%)), date$(2%)           , ch(10),~
                                                                         ~
               at (15,08), "TYPES to Include (1-9)",                     ~
               at (15,37), fac(lfac$(11%)), str(typ$())         , ch(10),~
                                                                         ~
               at (16,08), "PRIORITIES to Include (A-Z)",                ~
               at (16,37), fac(lfac$(12%)), str(priority$())    , ch(30),~
                                                                         ~
               at (17,08), "Plan Approved Demands Only",                 ~
               at (17,37), fac(lfac$(13%)), aproved$            , ch(01),~
                                                                         ~
               at (17,45), "Unplan ALL Demands ",                        ~
               at (17,70), fac(lfac$(14%)), unpl_aprove_flag$   , ch(01),~
                                                                         ~
               at (18,08), "Display Demands before Planning",            ~
               at (18,45), fac(lfac$(15%)), disp_demand$        , ch(01),~
                                                                         ~
               at (19,08), "Print Full Report With Plan",                ~
               at (19,45), fac(lfac$(16%)), plandet$            , ch(01),~
                                                                         ~
               at (20,08), "Expedite Plan When Planning",                ~
               at (20,45), fac(lfac$(17%)), planfast$           , ch(01),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <>  3% then L40620
                  gosub L60000
                  goto L40230

L40620:        if keyhit% <> 13% then L40640
                  call "MANUAL" ("PLANRNGE")
                  goto L40230

L40640:        if keyhit% <> 15% then L40660
                  call "PRNTSCRN"
                  goto L40230

L40660:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        setpf_1
            if edit% > 0% then L40770     /*  Input Mode  */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)Brief Instructions                   " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "(3)Planning System Switches             " &       ~
                     "                       (16)Exit Program"

            pfkeys$ = hex(01020304ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40750
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                return
L40750:     str(pf$(1),18,24) = " " : str(pfkeys$,04,1) = hex(ff)
            return

            if fieldnr% > 0% then L40820     /*  Edit Mode  */
L40770:     pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)Brief Instructions                   " &       ~
                     "(8)UNPLAN Demands      (15)Print Screen"
            pf$(3%) = "(3)Planning System Switches             " &       ~
                     "(9)Edit Parameters     (16)PLAN Demands"

            pfkeys$ = hex(010203ffffffff0809ffffff0dff0f1000)
            if sel% = 1% then  return
               str(pf$(3%),41%,19%) = " " : str(pfkeys$,9%,1%) = hex(ff)
               return

L40820:     pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)Brief Instructions                   " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "(3)Planning System Switches             " &       ~
                     "                                       "

            pfkeys$ = hex(010203ffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUT SELECTED DEMANDS                                    *~
            *************************************************************

        deffn'102(edit%)
            if edit% = 2% then L42120
            if edit% = 0% then init(hex(81)) lfac$() else                ~
                               init(hex(8c)) lfac$()
            str(line2$,,50) = "Enter Selected Demands"
            inpmessage$ = selmessage$
L42120:     gosub setpf

L42140:     accept                                                       ~
               at (01,02), "Plan Or Unplan Multiple Demands",            ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), rtnmessage$            , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (05,02), fac(hex(ac)), titles$                , ch(79),~
                                                                         ~
               at (06,02), fac(lfac$( 1)), sdemand$( 1%)        , ch(16),~
               at (06,20), fac(lfac$( 1)), sline$( 1%)          , ch(03),~
               at (06,30), fac(lfac$( 2)), sdemand$( 2%)        , ch(16),~
               at (06,48), fac(lfac$( 2)), sline$( 2%)          , ch(03),~
               at (06,58), fac(lfac$( 3)), sdemand$( 3%)        , ch(16),~
               at (06,76), fac(lfac$( 3)), sline$( 3%)          , ch(03),~
                                                                         ~
               at (07,02), fac(lfac$( 4)), sdemand$( 4%)        , ch(16),~
               at (07,20), fac(lfac$( 4)), sline$( 4%)          , ch(03),~
               at (07,30), fac(lfac$( 5)), sdemand$( 5%)        , ch(16),~
               at (07,48), fac(lfac$( 5)), sline$( 5%)          , ch(03),~
               at (07,58), fac(lfac$( 6)), sdemand$( 6%)        , ch(16),~
               at (07,76), fac(lfac$( 6)), sline$( 6%)          , ch(03),~
                                                                         ~
               at (08,02), fac(lfac$( 7)), sdemand$( 7%)        , ch(16),~
               at (08,20), fac(lfac$( 7)), sline$( 7%)          , ch(03),~
               at (08,30), fac(lfac$( 8)), sdemand$( 8%)        , ch(16),~
               at (08,48), fac(lfac$( 8)), sline$( 8%)          , ch(03),~
               at (08,58), fac(lfac$( 9)), sdemand$( 9%)        , ch(16),~
               at (08,76), fac(lfac$( 9)), sline$( 9%)          , ch(03),~
                                                                         ~
               at (09,02), fac(lfac$(10)), sdemand$(10%)        , ch(16),~
               at (09,20), fac(lfac$(10)), sline$(10%)          , ch(03),~
               at (09,30), fac(lfac$(11)), sdemand$(11%)        , ch(16),~
               at (09,48), fac(lfac$(11)), sline$(11%)          , ch(03),~
               at (09,58), fac(lfac$(12)), sdemand$(12%)        , ch(16),~
               at (09,76), fac(lfac$(12)), sline$(12%)          , ch(03),~
                                                                         ~
               at (10,02), fac(lfac$(13)), sdemand$(13%)        , ch(16),~
               at (10,20), fac(lfac$(13)), sline$(13%)          , ch(03),~
               at (10,30), fac(lfac$(14)), sdemand$(14%)        , ch(16),~
               at (10,48), fac(lfac$(14)), sline$(14%)          , ch(03),~
               at (10,58), fac(lfac$(15)), sdemand$(15%)        , ch(16),~
               at (10,76), fac(lfac$(15)), sline$(15%)          , ch(03),~
                                                                         ~
               at (11,02), fac(lfac$(16)), sdemand$(16%)        , ch(16),~
               at (11,20), fac(lfac$(16)), sline$(16%)          , ch(03),~
               at (11,30), fac(lfac$(17)), sdemand$(17%)        , ch(16),~
               at (11,48), fac(lfac$(17)), sline$(17%)          , ch(03),~
               at (11,58), fac(lfac$(18)), sdemand$(18%)        , ch(16),~
               at (11,76), fac(lfac$(18)), sline$(18%)          , ch(03),~
                                                                         ~
               at (12,02), fac(lfac$(19)), sdemand$(19%)        , ch(16),~
               at (12,20), fac(lfac$(19)), sline$(19%)          , ch(03),~
               at (12,30), fac(lfac$(20)), sdemand$(20%)        , ch(16),~
               at (12,48), fac(lfac$(20)), sline$(20%)          , ch(03),~
               at (12,58), fac(lfac$(21)), sdemand$(21%)        , ch(16),~
               at (12,76), fac(lfac$(21)), sline$(21%)          , ch(03),~
                                                                         ~
               at (13,02), fac(lfac$(22)), sdemand$(22%)        , ch(16),~
               at (13,20), fac(lfac$(22)), sline$(22%)          , ch(03),~
               at (13,30), fac(lfac$(23)), sdemand$(23%)        , ch(16),~
               at (13,48), fac(lfac$(23)), sline$(23%)          , ch(03),~
               at (13,58), fac(lfac$(24)), sdemand$(24%)        , ch(16),~
               at (13,76), fac(lfac$(24)), sline$(24%)          , ch(03),~
                                                                         ~
               at (14,02), fac(lfac$(25)), sdemand$(25%)        , ch(16),~
               at (14,20), fac(lfac$(25)), sline$(25%)          , ch(03),~
               at (14,30), fac(lfac$(26)), sdemand$(26%)        , ch(16),~
               at (14,48), fac(lfac$(26)), sline$(26%)          , ch(03),~
               at (14,58), fac(lfac$(27)), sdemand$(27%)        , ch(16),~
               at (14,76), fac(lfac$(27)), sline$(27%)          , ch(03),~
                                                                         ~
               at (15,02), fac(lfac$(28)), sdemand$(28%)        , ch(16),~
               at (15,20), fac(lfac$(28)), sline$(28%)          , ch(03),~
               at (15,30), fac(lfac$(29)), sdemand$(29%)        , ch(16),~
               at (15,48), fac(lfac$(29)), sline$(29%)          , ch(03),~
               at (15,58), fac(lfac$(30)), sdemand$(30%)        , ch(16),~
               at (15,76), fac(lfac$(30)), sline$(30%)          , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <>  2 then L43060
                  gosub L49000
                  goto L42140

L43060:        if keyhit% <>  3 then L43100
                  gosub L60000
                  goto L42140

L43100:        if keyhit% <> 13 then L43140
                  call "MANUAL" ("PLANRNGE")
                  goto L42140

L43140:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42140

        setpf
            if edit% > 0% then L43310     /*  Input Mode  */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)Brief Instructions                   " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "(3)Planning System Switches             " &       ~
                     "                       (16)Exit Program"

            pfkeys$ = hex(010203ffffffffffffffffff0dff0f1000)
            return

            if edit% > 1% then L43410     /*  Edit Mode  */
L43310:     pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)Brief Instructions                   " &       ~
                     "(8)UNPLAN Demands      (15)Print Screen"
            pf$(3%) = "(3)Planning System Switches             " &       ~
                     "(9)Edit Parameters     (16)PLAN Demands"

            pfkeys$ = hex(010203ffffffff0809ffffff0dff0f1000)
            return

L43410:     pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)Brief Instructions                   " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "(3)Planning System Switches             " &       ~
                     "                                       "

            pfkeys$ = hex(010203ffffffffffffffffff0dff0fff00)
            return
L49000: REM *************************************************************~
            *       ABBREVIATED INSTRUCTIONS                            *~
            *                                                           *~
            *************************************************************

            accept                                                       ~
               at (01,02),                                               ~
                  "BRIEF INSTRUCTIONS - REFER TO USERS MANUAL FOR COMPLET~
        ~E INSTRUCTIONS",                                                 ~
               at (03,06),                                               ~
                "VALID DEMAND TYPES ARE -",                              ~
               at (04,13),                                               ~
                "1 = Sales order to be NETTED against forecasts      ",  ~
               at (05,13),                                               ~
                "2 = Sales order NOT TO BE NETTED against forecasts  ",  ~
               at (06,13),                                               ~
                "3 = REQUISITION - like a type 2 sales forecast      ",  ~
               at (07,13),                                               ~
                "4 = REGULAR SALES FORECAST- assumed to be part of   ",  ~
               at (08,13),                                               ~
                "    nettable sales entered before OR after this fcst",  ~
               at (09,13),                                               ~
                "5 = TRUE NET NEW SALES FORECASTS - over and above   ",  ~
               at (10,13),                                               ~
                "    current forecast, will plan for the quantity shown",~
               at (11,13),                                               ~
                "6 = user settable - now does nothing                ",  ~
               at (12,13),                                               ~
                "7 = user settable - now does nothing                ",  ~
               at (13,13),                                               ~
                "8 = PROCUREMENT DEMAND (NO jump, NO pipout for part)",  ~
               at (14,13),                                               ~
                "9 = Preventive maintenance demand (work center only)",  ~
               at (16,06),                                               ~
                "FOR 'PRIORITIES AND TYPES TO INCLUDE' -             ",  ~
               at (17,13),                                               ~
                "Delete the ones you DON'T want or erase & enter ones you~
        ~ DO want",                                                       ~
               at (19,06),                                               ~
                "FOR 'APPROVED DEMANDS?' -                           ",  ~
               at (20,13),                                               ~
              "IF PLANNING,   N = plan ALL,   Y = plan ONLY approved",   ~
               at (21,13),                                               ~
                "IF UNPLANNING, Y = unplan ALL, N = unplan if NOT approve~
        ~d",                                                              ~
               at (24,06),                                               ~
                "PRESS (ENTER) TO RETURN"
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, rtnmessage$ = " "
                  on fieldnr% gosub L50125,         /* BEGINNING DEMAND */~
                                    L50180,         /* BEGINNING LINE   */~
                                    L50210,         /* ENDING DEMAND    */~
                                    L50245,         /* ENDING LINE      */~
                                    L50275,         /* Part Range       */~
                                    L50305,         /* Part Type Range  */~
                                    L50335,         /* MPS Group Range  */~
                                    L50365,         /* Buyer Code Range */~
                                    L50400,         /* Scheduler Range  */~
                                    L50435,         /* Date Range       */~
                                    L50525,         /* Types to include */~
                                    L50620,         /* PRIORITIES       */~
                                    L50710,         /* Plan Approved    */~
                                    L50720,         /* Unplan Approved  */~
                                    L50735,         /* Display Demands  */~
                                    L50760,         /* DETAIL?          */~
                                    L50785          /* FAST PLAN        */
                     return
L50125: REM Test Data For Enter Beginning Demand Code
                if demand$(1%) <> "?" then L50130
                    gosub get_demand : if chose_one% = 1% then return
L50130:         if demand$(1%) = "ALL" or demand$(1%) = "SELECT"         ~
                                 then demand$(2), line$() = " "
                if demand$(1%) <> " " then L50155
                    errormsg$ = "STARTING Demand Code Cannot Be Blank"
                    return
L50155:         if edit% > 0% then gosub demand_range_edit_loop/* Force */
                                        /* Modification of Demand Range */
                last_demand_1$ = demand$(1%)
                return

L50180: REM Test Data For Enter Beginning Demand Line
                if demand$(1%) = "ALL" or demand$(1%) = "SELECT"         ~
                                 then return
                if line$(1%) = " " then line$(1) = "  1"
                return

L50210: REM Test Data For Enter Ending Demand Code
                if demand$(2%) <> "?" then L50215
                    gosub get_demand : if chose_one% = 1% then return
L50215:         if demand$(1%) = "ALL" or demand$(1%) = "SELECT"         ~
                                 then return
                if demand$(2%) < demand$(1%) then errormsg$ = "ENDING Dem~
        ~and Cannot Be Less Than The BEGINNING Demand"
                return

L50245: REM Test Data For Enter Ending Demand Line
                if demand$(1%) = "ALL" or demand$(1%) = "SELECT"         ~
                                 then return
                if line$(2%) = " " then line$(2%) = "999"
                return

L50275: REM Test Data for Part Range
                call "TESTRNGE"   (fmpart$, topart$, temp$, temp$,       ~
                                   errormsg$, #02)
                if fmpart$ = "ALL" then to$(5%) = " "
                return

L50305: REM Test Part Type Range               FMTYP$
                if fmtyp$ = "?" then L50314
                if fmtyp$ <> "ALL" then L50310
                    totyp$, to$( 6%) = " "    :   return
L50310:         convert fmtyp$ to temp, data goto L50329
                if totyp$ <> " " then                                    ~
                    convert totyp$ to temp, data goto L50329
L50314:         call "TESTRNGE"   (fmtyp$, totyp$, temp$, temp$,         ~
                                   errormsg$, #24, "PARTTYPE ")
                return
L50329:         errormsg$ = "Part Type must be Numeric"
                return

L50335:     REM Test MPS Group Range
                call "TESTRNGE"   (fmmps$, tomps$, temp$, temp$,         ~
                                   errormsg$, #22)
                if fmmps$ = "ALL" then to$(7%) = " "
                return

L50365: REM Test Buyer Codes Range             FMBUYCODE$
                call "TESTRNGE"   (fmbuycode$, tobuycode$,               ~
                                   temp$, temp$, errormsg$, #24,         ~
                                   "BYCLASSES")
                if fmbuycode$ = "ALL" then to$(8%) = " "
                return

L50400: REM Test Scheduler Codes Range          FMSCHDCODE$
                call "TESTRNGE"   (fmschdcode$, toschdcode$,             ~
                                   temp$, temp$, errormsg$, #24,         ~
                                   "PSCLASSES")
                if fmschdcode$ = "ALL" then to$(9%) = " "
                return

L50435: REM Test for Due Date Range              DATE$()
            date%(1%), date%(2%) = 0%
            if date$(1%) <> "ALL" then L50455
            date$(1%) = "19010101"
            date$(2%) = "20991231"
            call "DATFMTC" (date$(1%))
            call "DATFMTC" (date$(2%))
            return

L50455:     if date$(1%) <> " " and date$(1%) <> blankdate$ and ~
               (date$(2%) = " " or date$(2%) = blankdate$) then ~
                date$(2%) = date$(1%)
            call "DATEOKC" (date$(1%), date%(1%), errormsg$)
                 if errormsg$ <> " " then return
            call "DATEOKC" (date$(2%), date%(2%), errormsg$)
                 if errormsg$ <> " " then return
            unffmdate$ = date$(1%)
            unftodate$ = date$(2%)
            call "DATUFMTC" (unffmdate$)
            call "DATUFMTC" (unftodate$)
            if (unffmdate$ >  unftodate$)                                ~
                then errormsg$ = "From may not be greater then To."
            return



L50525: REM Test Data For Types To Include (1-9)
            for u% = 1% to 10%
              if str(typ$(),u%,1%) < hex(31) or str(typ$(),u%,1%)        ~
                           > hex(39) then str(typ$(),u%,1%)=" "
            next u%
            for u% = 1% to 9%
                for i% = u%+1% to 10%
                     if str(typ$(),i%,1%)=str(typ$(),u%,1%)              ~
                          then str(typ$(),i%,1%)=" "
                next i%
            next u%

            if typ$() = " " then errormsg$ = "Valid Type Codes are the Nu~
        ~mbers '1-9'"
            if errormsg$ <> " " then return
            call "SPCSMASH" (str(typ$(),1%,10%))
            return


L50620: REM Test Data For Priorities To Include (A-Z)
            for u% = 1% to 30%
              if str(priority$(),u%,1) < hex(40) or str(priority$(),u%,1)~
                           > hex(5a) then str(priority$(),u%,1)=" "
            next u%
            for u% = 1% to 29%
                for i% = u%+1% to 30%
                     if str(priority$(),i%,1%)=str(priority$(),u%,1%)    ~
                          then str(priority$(),i%,1%)=" "
                next i%
            next u%

            if priority$() = " " then errormsg$ = "Valid Priority Codes A~
        ~re the Letters 'A-Z'"
            if errormsg$ <> " " then return
            call "SPCSMASH" (str(priority$(),1,30))
            return

L50710: REM Test Data For Aproved Only
                if pos("YN" = aproved$) = 0 then                         ~
                               errormsg$ = "Please Enter 'Y'es' or 'N'o'"
                return

L50720: REM Test Data For Unplan NOT Approved Only
                if pos("YN" = unpl_aprove_flag$) = 0 then                ~
                               errormsg$ = "Please Enter 'Y'es' or 'N'o'"
                return

L50735: REM Test Display Demands before Proccessing   DISP_DEMAND$
                if pos("YN" = aproved$) = 0 then                         ~
                               errormsg$ = "Please Enter 'Y'es' or 'N'o'"
                return

L50760: REM Planning Detail
            if plandet$ = "Y" or plandet$ = "N" then return
                errormsg$ = "Please Enter 'Y'es' or 'N'o'"
                return

L50785: REM FAST PLAN
            if planfast$ = "Y" or planfast$ = "N" then return
                errormsg$ = "Please Enter 'Y'es' or 'N'o'"
                return

        get_demand
            chose_one% = 0%
            which% = 1% : descr$ = "Choose Starting Demand & Line"
            if demand$(1%) = "?" then L50920
                which% = 2% : descr$ = "Choose Ending Demand & Line"
L50920:     readkey$ = " "
            phdr$(1%) = "  Demand Code    Line   Part      " &           ~
                            "                 Quantity"
            call "PLOWCODE" (#1, readkey$, descr$, 2000%, 1.35, f1%(1%), ~
                                                          phdr$(), 0.35)
            if f1%(1%) = 0% then return
                demand$(which%) = str(readkey$,,16%)
                line$(which%)   = str(readkey$,17%,3%)
                chose_one% = 1%
                if which% = 1% then last_demand_1$ = demand$(1%)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * Test Data for Selected Demand Input                       *~
            *************************************************************
        deffn'152
            edit% = 2%
            errormsg$ = " "
            init(hex(8c)) lfac$()
            for i% = 1% to 30%
                if sdemand$(i%) = " " then sline$(i%) = " "
                if sdemand$(i%) = " " then L51980
                call "STRING" addr("RJ", sline$(i%), 3%)
                if sdemand$(i%) <> "?" then L51460
                    readkey$, errormsg$, descr$ = " "
                    phdr$(1) = "  Demand Code    Line   Part      " &    ~
                               "                 Quantity"
                    call "PLOWCODE" (#1, readkey$, descr$, 2000%, 1.35,  ~
                                     f1%(1), phdr$(), 0.35)
                        if f1%(1) = 0% then L51520
                    sdemand$(i%) = readkey$
                    sline$(i%) = str(readkey$,17)
                    goto L51580
L51460:         readkey$ = str(sdemand$(i%)) & str(sline$(i%))
                call "REDALT0"(#1, readkey$, 1%, f1%(1%))
                if f1%(1%) = 1% then L51580
L51520:             lfac$(i%) = hex(91)
                    errormsg$ = "Please Enter a Valid Demand Code"
                    return
L51580:         if i% = 1% then L51760
                for j% = 1% to i% - 1%
                    if sdemand$(j%) = " " then L51740
                    if sdemand$(j%) <> sdemand$(i%) then L51740
                    if sline$(j%) <> sline$(i%) then L51740
                        errormsg$ = "DUPLICATE Demand Entered"
                        lfac$(i%) = hex(91)
                        return
L51740:         next j%
L51760:         get #1 using L51780, stat$
L51780:             FMT CH(01)
                if i% > 1% then L51860
                    if stat$ > "1" then pu% = 2% else pu% = 1%
                    goto L51980
L51860:         if stat$ > "1" and pu% = 2% then L51980
                if stat$ < "2" and pu% = 1% then L51980
                    lfac$(i%) = hex(91)
                    errormsg$ = "ALL Selected Demands Must be of same" & ~
                                " Status - either Planned or Unplanned"
                    return
L51980:     next i%
            return

L60000: REM *************************************************************~
            *      MODIFY PLANNING FLAGS                                *~
            *************************************************************

              err% = today%
                 call "PLNFLSUB" (planflags$(), err%)
                    unplanopt$ = str(planflags$(), 4, 1)
                    unplanrpt$ = str(planflags$(),18, 1)
                    get str(planflags$(), 281) using L60100, lta%()
L60100:                  FMT 26*BI(4)

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

            call "SHOSTAT" ("Closing Files, One Moment Please")

            call "FILEBGON" (#50)
            call "FILEBGON" (#62)
            call "FILEBGON" (#63)
            call "FILEBGON" (#64)
            close printer
            end
