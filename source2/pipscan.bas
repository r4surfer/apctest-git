        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP    SSS    CCC    AAA   N   N          *~
            *  P   P    I    P   P  S      C   C  A   A  NN  N          *~
            *  PPPP     I    PPPP    SSS   C      AAAAA  N N N          *~
            *  P        I    P          S  C   C  A   A  N  NN          *~
            *  P      IIIII  P       SSS    CCC   A   A  N   N          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPSCAN  - SCAN PIPMASTR FILES FOR CRITICAL SHORTAGES,    *~
            *            SAFETY STOCK INTRUSIONS, SURPLUSES, OR JUST    *~
            *            ENTER A PART NUMBER TO MANAGE.                 *~
            *                                                           *~
            * **NOTE**  THIS PROGRAM IS ON THE VERGE OF EXCEEDING THE   *~
            *           COMPILER WITH TOO MANY BRANCHES ! ! ! ! ! !     *~
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
            * 10/16/84 ! ORIGINAL                                 ! KEN *~
            * 08/02/85 ! ADDED CALL TO GETDEM TO FIND DEMAND      ! WPH *~
            * 08/12/86 ! Spelling Corrections                     ! MJB *~
            * 10/10/86 ! Added PSBQUANT, COMBQUANT to DIM's       ! kab *~
            * 09/25/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 10/25/86 ! WCOUT FORMAT CHANGED                     ! HES *~
            * 04/23/87 ! Added PORLSE & VBKMASTR for PIPDSUB      ! MJB *~
            * 04/27/87 ! Added Calculated Cost of Procurement     ! MJB *~
            * 05/27/87 ! File changes for standard costing        ! MJB *~
            * 06/09/87 ! JBMASTR2 record length mod to 1300 bytes.! JIM *~
            * 03/03/87 ! Changed condition calculations to use    ! RJM *~
            *          !  shelf instead of pip. Also added a front!     *~
            *          !  end screen where selection ranges can be!     *~
            *          !  entered (Part #, Type & Category).      !     *~
            *          !  A workfile is created from selections if!     *~
            *          !  not ALL. This speeds things up.         !     *~
            * 04/15/88 ! Fixed calls to GETDEM, added +L% to Tag #! RJM *~
            *          !  Added Late in's & out's to scan status  !     *~
            * 05/13/88 ! More changes; Eff. LT's, Min days for    ! RJM *~
            *          !  each condition, allows force BO before  !     *~
            *          !  today, new mini-startover, added part   !     *~
            *          !  class, Scanning date range.             !     *~
            * 05/19/88 ! Added new PIP Management function,       ! RJM *~
            *          !  Adjust Sales Forecast Quantities on any !     *~
            *          !  date if ATC's allow, ( No new advices ).!     *~
            *          !  This is a Call to a new SUB.            !     *~
            * 06/06/88 ! Added Option to restrict work file list  ! RJM *~
            *          !  to valid Buyer/Planner Parts.           !     *~
            * 06/06/88 ! Created 2 new SUBs striped out so pipscan! RJM *~
            *          !  could continue to compile. The following!     *~
            *          !  are the names of the new SUBs;          !     *~
            *          !          PIPOPTMZ  &  PIPRVWSB           !     *~
            * 06/08/88 ! Combined 3 ACCEPT Screens into 1         ! RJM *~
            *          !  & Now differentiates between Critical - !     *~
            *          !  Shortages and Forecast - Shortages.     !     *~
            * 06/13/88 ! Spelling/Terminology fixes & changed     ! RJM *~
            *          !  name of sub SFAJSTSB to SFADJSUB.       !     *~
            * 06/15/88 ! Standardized Range input testing,        ! RJM *~
            *          !  Fixed Workfile overflow msg & startover,!     *~
            *          !  User can reset PIP status if PF9 pressed!     *~
            *          !  Removed Eff LT's so PGM would compile ! !     *~
            * 06/17/88 ! Added Alt Key 3 on DEMMASTR              ! MJB *~
            * 06/20/88 ! Fixed Field edit logic, workfile overflow! RJM *~
            *          !  logic & min days test for <= 0          !     *~
            * 11/04/88 !  Added HNYALTRS to Review Functions      ! KAB *~
            * 02/01/89 !  Enabled Search Logic/ 2nd Screen Input  ! KAB *~
            *          !  Fixed PLANFLAG% for Direct Procurement  !     *~
            * 08/09/89 ! Fixed File Status 22 on DEMMASTR by      ! MLJ *~
            *          !  changing #54700 from READ100 to REDALT0.!     *~
            * 09/12/89 ! Right justified Demand Line.             ! JDH *~
            * 09/19/89 ! Validate dates prior to PF9 Proceed.     ! JDH *~
            * 12/04/89 ! Removed PIPMSUB & associated logic.      ! MJB *~
            * 05/01/90 !(PRR's 11007, 11114, 10216)               ! WPH *~
            *          ! Added 'slide' capability in conjunction  !     *~
            *          ! with reschedule, and ability to search   !     *~
            *          ! for only problems inside ATC horizon.    !     *~
            * 11/30/90 ! Changed test @ ln 312070 to CS% <> 0%    ! MJB *~
            * 07/06/90 !(PRR 10514) Added code to allow full pip  ! WPH *~
            *          !    scan from the initial Input Menu (PF9)!     *~
            * 01/17/91 ! Changed initialize of RTESTEP to INIT    ! MJB *~
            * 06/11/91 ! PRR 11705 Added ERR% = TODAY% prior to   ! SID *~
            *          !     calling "PLNFLSUB".                  !     *~
            * 08/02/91 !(QC-FIXES) 1-Corrected open error on #10  ! RJB *~
            *          !     Workfile, 2-Rearanged Screen #1 to   !     *~
            *          !     put manditior fields for PF9 function!     *~
            *          !     first, 3-Modifed Main Input Code to  !     *~
            *          !     support #2, 4-Added Input Msg. to the!     *~
            *          !     DEFFN'110 to end user confusion about!     *~
            *          !     about Entering P/N's, 5-Changed when !     *~
            *          !     PF9 on Screen 1 is active to be after!     *~
            *          !     the three Manditory fields (#2),     !     *~
            *          !     6-Changed approp. input msgs to tell !     *~
            *          !     users to enter non-blank chars., and !     *~
            *          !     lastly added call to 'ALLFREE'.      !     *~
            * 12/16/91 ! (R6.01.00 fix) Removed OPENCHCK loop on  ! MLJ *~
            *          !     #1-#16 eliminating #10 open error.   !     *~
            *          !     Also removed FILEBGON loop #61-#64.  !     *~
            * 07/02/92 ! Added PIPIN channel to the pass to GETDEM! WPH *~
            * 11/03/92 ! PRR 12347 - Initialize EXPCOST$ variale. ! RJH *~
            *          ! PRR 12169 - Set and release JobHolds in  ! RJH *~
            *          !  JBTIF File.                             !     *~
            *          ! PRR 11459 - Call HNYDDISP to display     ! RJH *~
            *          !  Inventory details w/ PF9 @ Mng. Part.   !     *~
            * 11/04/92 ! PRR 12464 - Use PSBTODAY% date for Last  ! RJH *~
            *          !  Planned date.                           !     *~
            * 01/04/93 ! Misc Spelling corrections.               ! RJH *~
            * 03/08/94 ! Changed record length for BOMSPEC        ! WPH *~
            * 05/25/94 ! Added UOM to Part Problem Screen.        ! JDH *~
            * 06/28/94 ! Vendor Service Advices - added reset     ! ERN *~
            *          !  to VBKVSA when reschedule takes place.  !     *~
            * 08/22/96 ! Changes for the year 2000.               ! DXL *~
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

        dim add$3,                                                       ~
            atcflag$1,                   /* Only Problems in ATC Horzn?*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buyer$3,                     /* BUYER CODE FROM HNYMASTR   */~
            buyer_flag$1,                /* USE BUYER/PLANNER CODES ?  */~
            bcodes$(100)3,               /* Buyer Codes                */~
            cat$(2)4,                    /* PART CATEGORY RANGE        */~
            catr$4,                      /* PART CATEGORY RANGE        */~
            class$(2)4,                  /* PART CLASS RANGE           */~
            clrpart$25,                  /* CLEAR PIPIN PART           */~
            clrtag$19,                   /* CLEAR PIPIN TAG            */~
            combdate$8,                                                  ~
            combquant$10,                                                ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            demand$19,                   /* DEMAND PASSED FROM GETDEM  */~
            dts%(8),                     /* DAYS TO SUPPLY             */~
            duedate$8,                                                   ~
            startday$8,                                                  ~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            expcost$10,                  /* Expected Extended Cost     */~
            field1$10,                   /* MUTLI-USE SCREEN FIELD     */~
            field2$8,                    /* MUTLI-USE SCREEN FIELD     */~
            field3$8,                    /* MUTLI-USE SCREEN FIELD     */~
            file$8,                                                      ~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lib$8,                                                       ~
            line2$79,                    /* Screen Line #2             */~
            lt$3,                        /* PART LEAD TIME             */~
            cmin_days$3,                 /* MIN DAYS FOR CRITICAL SHORT*/~
            imin_days$3,                 /* MIN DAYS FOR S/S INTRUSION */~
            smin_days$3,                 /* MIN DAYS FOR SURPLUS       */~
            moq$10,                                                      ~
            newtag$19,                                                   ~
            oktag$2,                                                     ~
            oldtag$19,                                                   ~
            onhand$10,                                                   ~
            pansize$10,                                                  ~
            part$25,                     /* PART NUMBER                */~
            partr$(4)25,                 /* PART NUMBER RANGE          */~
            partdescr$32,                /* PART NUMBER                */~
            partpriority$1,              /* PART PRIORITY DEFAULT      */~
            parttypedescr$32,            /* PART TYPE DESCR - SCREEN   */~
            pclass$4,                    /* PART CLASS                 */~
            pcodes$(100)3,               /* Planner/Scheduler Codes    */~
            planner$3,                   /* PLANNER CODE FROM HNYMASTR */~
            pldate$6,                                                    ~
            pfktext$(3)79,               /* PF KEY DESCRIPTION DISPLAY */~
            pfkeys$32,                   /* PF KEY ENABLE STRING       */~
            pip(4),                                                      ~
            pip_reset_flag$1,            /* RESET PIP BEFORE ANALYSIS? */~
            pipplow$100,                                                 ~
            planflagshold$(25)20,                                        ~
            plowkey$100,                 /* PLOW KEY                   */~
            pprior$(8)1,                 /* PART TYPE PRIORITIES       */~
            prfac$1,                     /* PRIORITY FAC FOR SCREEN    */~
            prompt1$(4)14,               /* SCREEN PROMPT STRINGS      */~
            prompt1$22,                  /* SCREEN PROMPT STRING       */~
            ptype$3,                     /* Part Type                  */~
            reviewpart$25,               /* PART NUMBER                */~
            rschdbom$3,                                                  ~
            rschdstep$7,                                                 ~
            rschdstart$8,                                                ~
            rschddue$8,                                                  ~
            rschdtag$19,                                                 ~
            safety$10,                                                   ~
            scan%(2),                                                    ~
            scan_pipin$1,                                                ~
            scandate$(2)10,                                              ~
            scantype$3,                                                  ~
            scanstat$1,                                                  ~
            status$(6)70,                /* STATUS                     */~
            statnm$(5)10,                /* STATUS                     */~
            statdt$(5)8,                 /* STATUS                     */~
            stat%(4),                                                    ~
            supply$3,                                                    ~
            temp2$10,                                                    ~
            title$(2)25,                                                 ~
            tmpdate$10,                                                  ~
            travel$(256)67,              /* FORMATTED WCOUT DATA       */~
            type$1,                      /* DEMAND TYPE FROM GETDEM    */~
            typer$(2)3,                  /* PART TYPE RANGE            */~
            vol$6,                                                       ~
            wstat$4,                     /* TEMP STATUS OF PIP         */~
            unplanopt$1,                                                 ~
            unplanrpt$1,                                                 ~
            uom$4,                       /* Unit of Measure            */~
            userid$3,                                                    ~
            vjob$25, vpart$25, vpiptag$19/* JBVSASUB arguments         */

        dim psbcode$16,                  /* DEMAND CODE                */~
            psbline$3,                   /* DEMAND LINE                */~
            psbtype$1,                   /* DEMAND TYPE                */~
            psbprior$1,                  /* DEMAND PRIORITY            */~
            psbpart$25,                  /* PART NEEDED                */~
            psbquant$10,                 /* QUANTITY NEEDED            */~
            psbrte$3,                    /* THE WC ROUTE TO USE        */~
            psbbom$3                     /* WHICH BOM TO USE           */

        REM PSBQUANT,                    /* QUANTITY NEEDED            */~
            PSBCD%,                      /* REQ'D COMPL DATE           */~
            PSBTODAY%,                   /* SUBSCRIPT FOR TODAY'S DATE */~
            PLANFLAG%                    /* >0= PLAN, 0 = CHECK ONLY   */

        dim f2%(64)                      /* = 0 IF THE FILE IS OPEN    */
         /* F1%(64)  IN COMMON BLOCK     /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLE F2%() SHOULD NOT BE MODIFIED.  IT */
                     /* IS AN INTRINSIC PART OF THE FILE OPENING,      */

            call "SHOSTAT" ("Opening Files, One Moment Please")

        REM *************************************************************~
            *  SELECT FILES                                             *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! DEMMASTR ! Demand Master File                       *~
            * #2  ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! HNYDETAL ! Inventory detail file                    *~
            * #6  ! RTEALTRS ! Rte alternates                           *~
            * #7  ! RTEMASTR ! Production routing master file           *~
            * #8  ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #9  ! JBMASTR2 ! JOB MASTER FILE                          *~
            * #10 ! WORKFILE ! Allows Easy Access To Route Steps        *~
            * #11 ! WCMASTR  ! Work center master file                  *~
            * #12 ! CALMASTR ! Planning Production Calendar File        *~
            * #14 ! BOMSPEC  ! options selected file                    *~
            * #15 ! BOMMASTR ! BOM relationship file                    *~
            * #16 ! HNYALTRS ! Inventory Alternate Part File            *~
            * #20 ! PORELUSR ! User Buy Order Release Cross Reference   *~
            * #21 ! JBRELUSR ! User Work Order Release Cross Reference  *~
            * #23 ! WCOUT    ! Planned work center use detail rec       *~
            * #24 ! ENGMASTR ! Engineering Master Filer                 *~
            * #33 ! PIPIN    ! Planned inventory additions detail       *~
            * #34 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #35 ! PIPCROSS ! hard peg cross reference                 *~
            * #36 ! JBPIPXRF ! option part harder peg                   *~
            * #40 ! SFMASTR2 ! Sales forecast master file               *~
            * #41 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #42 ! PORLSE   ! Purchase Directives (Requisitions) File  *~
            * #43 ! VBKMASTR ! Purchase Order Header Master File        *~
            * #50 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #51 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #52 ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #53 ! GLMAIN   ! General ledger master file               *~
            * #54 ! USERINFO ! User posting dates                       *~
            * #55 ! HNYQUAN  ! Inventory quantity  file                 *~
            * #56 ! STORNAME ! Store codes                              *~
            * #60 ! SYSFILE2 ! Caelus Management System Information     *~
            * #61 ! WORKR    ! RESCHEDULE WORKFILE                      *~
            * #62 ! WORK1    ! PLANNING WORKFILE - MATERIALS            *~
            * #63 ! WORK2    ! PLANNING WORKFILE - WORK CENTER ALLOC    *~
            * #64 ! WORKFILE ! Workfile containing valid parts in range *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25, dup

            select #2,  "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #4,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #5,  "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup     ~

            select #6,  "RTEALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  34                      ~

            select #7,  "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #8,  "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   94,                                  ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select  #9, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            select #10, "WORKFILE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 54,                                    ~
                        keypos = 1, keylen = 9,                          ~
                        alt key 1, keypos = 10, keylen = 43, dup

            select #11, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #12, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2                      ~

            select #14, "BOMSPEC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23          ~

            select #15, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #16, "HNYALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =    1, keylen =  33                      ~

            select #20, "PORELUSR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 70,                                    ~
                        keypos =    1, keylen =   6,                     ~
                        alt key 1, keypos =  4, keylen = 6

            select #21, "JBRELUSR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 70,                                    ~
                        keypos =    1, keylen =   6,                     ~
                        alt key 1, keypos =  4, keylen = 6

            select #23, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27          ~

            select #24, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29                      ~

            select #33, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #34, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #35, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #36, "JBPIPXRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   63,                                  ~
                        keypos =    1, keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19          ~

            select #40, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #41, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #42, "PORLSE",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 492,                                   ~
                        keypos =   1, keylen =  66,                      ~
                        alt key  1, keypos =   48, keylen =  19, dup,    ~
                            key  2, keypos =    5, keylen =  62, dup,    ~
                            key  3, keypos =   14, keylen =  53, dup,    ~
                            key  4, keypos =   39, keylen =  28, dup

            select #43, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            select #50, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #51, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #52, "SERWORK",                                       ~
                        varc,     indexed,  recsize =   48,              ~
                        keypos = 1, keylen = 23

            select #53, "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select #54, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #55, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select #56, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #60, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #61, "WORK1",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 34

            select #62, "WORK2",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos = 31, keylen = 8,                         ~
                        alt key 1, keypos = 1, keylen = 29,              ~
                            key 2, keypos = 35, keylen= 4,               ~
                            key 3, keypos = 30, keylen= 9

            select #63, "WORK3",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 66,                                    ~
                        keypos = 5, keylen = 10,                         ~
                        alt key 1, keypos = 1, keylen = 4, dup

            select #64, "WORKFILE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 74,                                    ~
                        keypos = 1, keylen = 25


            call "WORKOPEN" (#61, "IO", 1000%, f2%(61))

           call "OPENCHCK" (# 1, 0%, f2%( 1), 100%, " ")
           call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, " ")
           call "OPENCHCK" (# 4, 0%, f2%( 4),   0%, " ")
           call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, " ")
           call "OPENCHCK" (# 6, 0%, f2%( 6),   0%, " ")
           call "OPENCHCK" (# 7, 0%, f2%( 7),   0%, " ")
           call "OPENCHCK" (# 8, 0%, f2%( 8), 100%, " ")
           call "OPENCHCK" (# 9, 0%, f2%( 9), 200%, " ")
           call "OPENCHCK" (#11, 0%, f2%(11),   0%, " ")
           call "OPENCHCK" (#12, 0%, f2%(12),   0%, " ")
           call "OPENCHCK" (#14, 0%, f2%(14),   0%, " ")
           call "OPENCHCK" (#15, 0%, f2%(15),   0%, " ")
           call "OPENCHCK" (#16, 0%, f2%(16),   0%, " ")
           call "OPENCHCK" (#23, 0%, f2%(23), 100%, " ")
           call "OPENCHCK" (#24, 0%, f2%(24),   0%, " ")
           call "OPENCHCK" (#33, 0%, f2%(33), 100%, " ")
           call "OPENCHCK" (#34, 0%, f2%(34), 100%, " ")
           call "OPENCHCK" (#35, 0%, f2%(35), 100%, " ")
           call "OPENCHCK" (#36, 0%, f2%(36), 100%, " ")
           call "OPENCHCK" (#40, 0%, f2%(40), 100%, " ")
           call "OPENCHCK" (#41, 0%, f2%(41), 100%, " ")
           call "OPENCHCK" (#42, 0%, f2%(42),   0%, " ")
           call "OPENCHCK" (#43, 0%, f2%(43),   0%, " ")
           call "OPENCHCK" (#50, 0%, f2%(50),   0%, " ")
           call "OPENCHCK" (#51, 0%, f2%(51),   0%, " ")
           call "OPENCHCK" (#52, 0%, f2%(52),   0%, " ")
           call "OPENCHCK" (#53, 0%, f2%(53),   0%, " ")
           call "OPENCHCK" (#54, 0%, f2%(54),   0%, " ")
           call "OPENCHCK" (#55, 0%, f2%(55),   0%, " ")
           call "OPENCHCK" (#56, 0%, f2%(56),   0%, " ")
           call "OPENCHCK" (#60, 0%, f2%(60),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            str(line2$,63) = "PIPSCAN: " & str(cms2v$,1,8)
            title$(1) = "From"  :  title$(2) = "To"

            call "READ100" (#60, "MONTHS OPEN", f1%(60))
            if f1%(60)=0% then L65000
            get #60, using L09085, pldate$
L09085:         FMT XX(32), CH(6)
            call "DATE" addr("G-", pldate$, date$, today%, err%)
            if err%<>0% then L65000
            today%=today%+1%
            if today% < 1% or today% > 490% then L65000

            call "DATEFMT" (date$)

            init ("A") pprior$():mat dts% = con
            call "READ100" (#60, "PLAN RANGE PRIORITY", f1%(60))
            if f1%(60) = 0% then L09155
            get #60, using L09145 , pprior$(), dts%()
L09145:         FMT XX(20), XX(104), 8*CH(1), 8*BI(4)

L09155:     call "READ100" (#60, "PLANNING SYSTEM FLAG", f1%(60))
            if f1%(60)=0% then L65000
            get #60, using L09170, str(planflags$(),1,480)
L09170:         FMT XX(20), CH(480)
            str(planflagshold$(),1,480) = str(planflags$(),1,480)
            unplanopt$ = str(planflags$(), 4, 1)
            unplanrpt$ = str(planflags$(),18, 1)

            call "READ100" (#12,"10", f1%(12))
                if f1%(12) = 0% then L09260
            get #12, using L09230, str(yymmdd$(),1,1470)

            call "READ100" (#12,"11", f1%(12))
                if f1%(12) = 0% then L09260
            get #12, using L09230, str(yymmdd$(),1471,1470)
L09230:         FMT XX(2), CH(1470)
            goto L09295

L09260:     call "ASKUSER" (2%, "*** CALENDAR ERROR ***",                ~
                "Planning Calendar cannot be loaded from file "&         ~
                "'CALMASTR'", " ", "Press any PF key to return to menu")
            goto L65000

L09295:     call "GETNAMES" addr(#2, file$, lib$, vol$)
            call "READFDR" addr(file$,lib$,vol$,0%,"RC",rec_count%,err%)
                if err% <> 0% then rec_count% = 3000%
            rec_count% = rec_count% / 2%

            call "EXTRACT" addr("ID", userid$)
            bcodes% = 0%

        startover_entry

                scantype$ = "ALL":scantype% = 1%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * INPUT INITIAL INFORMATION                                 *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            call "ALLFREE"
            init(" ") errormsg$, partr$(), typer$(), cat$(), scandate$(),~
                         part$, pip_reset_flag$, class$(), cmin_days$,   ~
                         atcflag$,  imin_days$, smin_days$, buyer_flag$, ~
                         scan_pipin$
            gosub L29000                  /* INIT MORE VARIABLES */
            edit%, workfl_flag%, no_wf% = 0%
            for fieldnr% = 1% to 12%
L10130:         gosub'050(fieldnr%)
                      if enabled% = 0% then L10336
L10150:         gosub'100(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  9% then L10260
                           no_wf% = 1%
                           partr$(3) = " "
                           partr$(4) = all(hex(ff))
*                         IF FIELDNR% <> 1% THEN 10193
*                             GOSUB'150(1%)
                               if errormsg$ <> " " then L10150
*                         FIELDNR% = 9%
                           goto L10311
L10260:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit%  = 32% then L65000
                      if keyhit% <>  4% then L10300
                           fieldnr% = max(1%, fieldnr% -1%)
                           goto L10130
L10300:         gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10150
                           if fieldnr% <> 12% then L10336
                           if no_wf% = 0% then L10336
L10311:                    if pip_reset_flag$ = "Y" then gosub reset_pip
                           fieldnr% = 2%
                           inpmessage$ = " "
                           goto L10970

L10336:     next fieldnr%

        REM *************************************************************~
            *      A L L O W   E D I T   O F   S E L E C T I O N        *~
            *************************************************************

        editmode
            fieldnr% = 0%
            edit% = 1%
            inpmessage$ = edtmessage$

            gosub'100(fieldnr%)
                if keyhit% = 1% then  gosub startover
                if keyhit% <>  9% then L10500
                     goto L10311                    /* GO SCAN DIRECTLY */
L10500:         if keyhit% = 32% then      L65000
                if keyhit% = 16% then      setup_scan
                if keyhit% <> 0% then      editmode

            fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% > 12% then editmode
                gosub'050(fieldnr%)
                    if enabled% = 0% then editmode
L10600:         gosub'100(fieldnr%)
                    if keyhit% = 1% then  gosub startover
                    if keyhit% <> 0% then      L10600
                gosub'150(fieldnr%)
                    if errormsg$ <> " " then L10600

            goto editmode


        REM *************************************************************~
            *    D I S P L A Y   C O N D I T I O N   O F   P A R T      *~
            *                                                           *~
            *                                                           *~
            *************************************************************
        show_condition

            gosub'110(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       manage_part
                  if keyhit%  = 32% then       L65000
                      if keyhit%  = 25% then scan_parts
                      if keyhit% >= 8% and keyhit% <= 12% then scan_parts
                      if keyhit% <> 0% then show_condition
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% = 4% then fieldnr% = 3%
            if fieldnr% < 1% or fieldnr% > 3% then show_condition
L10970:     gosub'110(fieldnr%)
                  if keyhit% =  1% then gosub startover
                  if keyhit% = 32% then L65000
                      if keyhit%  = 25% then scan_parts
                      if keyhit% >= 8% and keyhit% <= 12% then scan_parts
                  if keyhit% <> 0% then L10970
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L10970
            goto show_condition

L11000: REM *************************************************************~
            *             MANAGE PART SELECTIONS                        *~
            *                                                           *~
            * NOW DECIDE WHAT YOU WANT TO DO                            *~
            *************************************************************

        manage_part

            gosub release_jobhold            /* Release Job from Hold */

            errormsg$ = " "
L11090:     gosub L41000
                errormsg$ = " "
                if keyhit% = 16% then L11340
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then clear_pipins
                if keyhit% =  3% then clear_pipouts
                if keyhit% >  4% then L11170
                     if parttype% < 200% then L11380 else goto procure
L11170:         if keyhit% > 6% then L11230
                     if parttype% < 200% then L11380
                     if parttype% > 489% and parttype% < 500% then L11430
                     if parttype% > 789% and parttype% < 800% then L11430
                     if keyhit% =  5% then optimize
                     if keyhit% =  6% then combine_wo
L11230:         if keyhit% >  7% then L11250
                     if parttype% < 200% then L11380 else goto forcepo
L11250:         if keyhit% >  8% then L11265
                    if parttype% < 200% then L11380 else goto reschedule_wo
L11265:         if keyhit% =  9% then call_hnyddisp
                if keyhit% = 10% then revert_cumf
                if keyhit% = 11% then call_jbrelsub
                if keyhit% > 12% then L11310
                     gosub planflags
                     goto manage_part
L11310:         if keyhit% = 32% then L65000
            goto manage_part

L11340
*        RESET STATUS AND RETURN TO SCAN
            gosub'151(2%)
            goto show_condition

L11380
*        UNPLANNED PART TYPE ERROR TRAPPING
            errormsg$ = "Selection NOT Valid for a NON-Planned or Generi"~
                      & "c Part."
            goto L11090

L11430
*        TOOLING PART TYPE ERROR TRAPPING
            errormsg$ = "Selection NOT Valid for Tooling"
            goto L11090

        revert_cumf
            call "SFADJSUB" (part$, #41, #40, #2, #4, #60)
            goto manage_part

        call_hnyddisp
            call "HNYDDISP" (part$, "D", " ", " ", " ", " ", " ", " ",   ~
                             " ", #4, #5)
            goto manage_part

L12000: REM *************************************************************~
            * CANCEL PIPINS                                             *~
            *                                                           *~
            * DISPLAY AND ALLOW MARKING FOR PIP CLEARING                *~
            *************************************************************

        clear_pipins
            call "SHOSTAT" ("One Moment Please")

            oktag$ = "BO"  :  gosub load_pipins
        scr_select% = 1%  /* USED TO SELECT SCREEN IN MULTI-USE SCREEN */
L12100:     screen% = 0%
            gosub'108(screen%)
                if keyhit% <> 1% then L12130
                     gosub startovr2
                     if keyhit1% = 0% then reset_pipin
L12130:         if keyhit% =  2% then l% = 0%
                if keyhit% =  3% then l% = max(0%,max%-12%)
                if keyhit% =  4% then l% = max(0%, l%-12%)
                if keyhit% =  5% then l% = max(0%,min(l%+12%,max%-12%))
                if keyhit% =  6% then l% = max(0%, l% - 1%)
                if keyhit% =  7% then l% = max(0%,min(l%+1%,max%-12%))
                if keyhit% = 16% then L11000
                if keyhit% = 32% then L65000
                if keyhit% <> 0% then L12100

                hits% = 0%:init(hex(84)) str(wa$(),1,1000)
                for i% = 1% to max%
                     if intagnr$(i%)<>" " then L12280
                         wl$(i%) = " "
                         goto L12310
L12280:              if wl$(i%) = " " then L12310
                     hits% = 1%
                     wa$(i%) = hex(94)
L12310:         next i%
                if hits% = 0% then L12100
L12330:         screen% = 1%
                gosub'108(screen%)
                  if keyhit% <> 1% then L12360
                          gosub startovr2
                          if keyhit1% = 0% then reset_pipin
L12360:           if keyhit% =  2% then l% = 0%
                  if keyhit% =  3% then l% = max(0%,max%-12%)
                  if keyhit% =  4% then l% = max(0%, l%-12%)
                  if keyhit% =  5% then l% = max(0%,min(l%+12%,max%-12%))
                  if keyhit% =  6% then l% = max(0%, l% - 1%)
                  if keyhit% =  7% then l% = max(0%,min(l%+1%,max%-12%))
                  if keyhit% <> 8% then L12330
                goto cancel_pipin

        cancel_pipin
            for i% = 1% to max%
            if wl$(i%)=" " then L12550

                clrpart$ = part$
                clrdate% = lt%     (i%)
                clrtag$  = intagnr$(i%)

                gosub call_clrpipin

L12550:     next i%
            goto L12000

        reset_pipin
                init (" ") wl$()
                goto L12100

L13000: REM *************************************************************~
            * CANCEL PIPOUTS                                            *~
            *                                                           *~
            * DISPLAY AND ALLOW MARKING FOR PIP CLEARING                *~
            *************************************************************

        clear_pipouts
            call "SHOSTAT" ("One Moment Please")

            gosub load_pipouts

L13100:     screen% = 0%
            gosub L43000
                if keyhit% <> 1% then L13130
                     gosub startovr2
                     if keyhit1% = 0% then reset_pipout
L13130:         if keyhit% =  2% then l% = 0%
                if keyhit% =  3% then l% = max(0%,max%-12%)
                if keyhit% =  4% then l% = max(0%, l%-12%)
                if keyhit% =  5% then l% = max(0%,min(l%+12%,max%-12%))
                if keyhit% =  6% then l% = max(0%, l% - 1%)
                if keyhit% =  7% then l% = max(0%,min(l%+1%,max%-12%))
                if keyhit% = 16% then L11000
                if keyhit% = 32% then L65000
                if keyhit% <> 0% then L13100

                hits% = 0%:init(hex(84)) str(wa$(),1,1000)
                for i% = 1% to max%
                     if intagnr$(i%)<>" " then L13280
                         wl$(i%) = " "
                         goto L13310
L13280:              if wl$(i%) = " " then L13310
                     hits% = 1%
                     wa$(i%)=hex(94)
L13310:         next i%
                if hits% = 0% then L13100
L13330:         screen% = 1%
                gosub L43000
                if keyhit% <> 1% then L13360
                     gosub startovr2
                     if keyhit1% = 0% then reset_pipout
L13360:           if keyhit% =  2% then l% = 0%
                  if keyhit% =  3% then l% = max(0%,max%-12%)
                  if keyhit% =  4% then l% = max(0%, l%-12%)
                  if keyhit% =  5% then l% = max(0%,min(l%+12%,max%-12%))
                  if keyhit% =  6% then l% = max(0%, l% - 1%)
                  if keyhit% =  7% then l% = max(0%,min(l%+1%,max%-12%))
                  if keyhit% <> 8% then L13330
                goto cancel_pipout

        cancel_pipout
            for i% = 1% to max%
            if wl$(i%)=" " then L13550

                clrpart$ = part$   (i%)
                clrdate% = lt%     (i%)
                clrtag$  = intagnr$(i%)

                gosub call_clrpipin

L13550:     next i%
            goto L13000

        reset_pipout
                init (" ") wl$()
                goto L13100

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * DIRECT PROCUREMENT                                        *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        procure
            init(" ") errormsg$,inpmessage$,psbquant$,psbprior$,duedate$,~
                      add$, psbtype$, psbbom$, psbrte$, expcost$,        ~
                      psbcode$, psbline$, prompt1$(), prompt1$
            combineflag% = 0%
            call "READ100" (#2, part$, f1%(2))
                if f1%(2) = 0% then L11000
            gosub set_status

            for fieldnr% = 1% to  7%
                gosub'054(fieldnr%)
                      if enabled% = 0% then L14240
L14170:         gosub'104(fieldnr%)
                      if keyhit% <> 1% then L14190
                           gosub startovr2
                           if keyhit1% = 0% then procure
L14190:               if keyhit%  = 16% then L11000
                      if keyhit%  = 32% then L65000
                      if keyhit% <>  0% then L14170
                gosub'154(fieldnr%)
                      if errormsg$ <> " " then L14170
L14240:         next fieldnr%

L14500: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * DIRECT PROCUREMENT                                        *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************
            init (" ") errormsg$
L14560:     gosub'104(0%)
                  if keyhit% <> 1% then L14580
                     gosub startovr2
                     if keyhit1% = 0% then procure
L14580:           if keyhit%  =  8% then       L34000
                  if keyhit% <> 16% then       L14620
                     if combineflag% <> 0% then restore_wo
                     goto L11000
L14620:           if keyhit%  = 32% then       L65000
                  if keyhit% <>  0% then       L14560
            fieldnr% = cursor%(1) - 7%
            if fieldnr% < 1% or fieldnr% > 7% then L14560

            if fieldnr% = 7% then gosub'054(fieldnr%)
            if enabled% = 0% then L14560
L14670:     gosub'104(fieldnr%)
                  if keyhit% <> 1% then L14700
                     gosub startovr2
                     if keyhit1% = 0% then procure
                  if keyhit% <>  0% then L14670
L14700:     gosub'154(fieldnr%)
                  if errormsg$ <> " " then L14670
            goto L14560

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * OPTIMIZE PIP POSITION                                     *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        optimize
            init(" ") errormsg$,inpmessage$,startday$,psbprior$,supply$

            call "READ100" (#2, part$, f1%(2))
                if f1%(2) = 0% then L11000

            for fieldnr% = 1% to  3%
                gosub'055(fieldnr%)
                      if enabled% =  0% then L15230
L15160:         gosub'105(fieldnr%)
                      if keyhit% <> 1% then L15180
                           gosub startovr2
                           if keyhit1% = 0% then optimize
L15180:               if keyhit%  = 16% then L11000
                      if keyhit%  = 32% then L65000
                      if keyhit% <>  0% then       L15160
                gosub'155(fieldnr%)
                      if errormsg$ <> " " then L15160
L15230:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * OPTIMIZE PIP POSITION                                     *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************
            init (" ") errormsg$
L15560:     gosub'105(0%)
                  if keyhit% <> 1% then L15580
                       gosub startovr2
                       if keyhit1% = 0% then optimize
L15580:           if keyhit%  =  8% then       call_pipoptmz
                  if keyhit%  = 24% then       call_pipoptmz
                  if keyhit%  = 16% then       L11000
                  if keyhit%  = 32% then       L65000
                  if keyhit% <>  0% then       L15560
            fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% >  3% then L15560

L15660:     gosub'105(fieldnr%)
                  if keyhit% <> 1% then L15680
                     gosub startovr2
                     if keyhit1% = 0% then optimize
L15680:           if keyhit% <>  0% then L15660
            gosub'155(fieldnr%)
                  if errormsg$ <> " " then L15660
            goto L15560

        REM *************************************************************~
            *                                                           *~
            * COMBINE WORK ORDERS INTO ONE (1) ADVICE                   *~
            *                                                           *~
            *************************************************************

        combine_wo
            call "SHOSTAT" ("One Moment Please")
            combquant=0:comb%=999%:combdate$, psbprior$,combquant$=" "
                prfac$=hex(84)
            oktag$ = hex(ffff):gosub load_pipins
        scr_select% = 2%  /* USED TO SELECT SCREEN IN MULTI-USE SCREEN */
L16110:     screen% = 0%
            gosub'108(screen%)
                if keyhit% <> 1% then L16140
                     gosub startovr2
                     if keyhit1% = 0% then combine_wo
L16140:         if keyhit% =  2% then l% = 0%
                if keyhit% =  3% then l% = max(0%,max%-12%)
                if keyhit% =  4% then l% = max(0%, l%-12%)
                if keyhit% =  5% then l% = max(0%,min(l%+12%,max%-12%))
                if keyhit% =  6% then l% = max(0%, l% - 1%)
                if keyhit% =  7% then l% = max(0%,min(l%+1%,max%-12%))
                if keyhit% = 16% then L11000
                if keyhit% = 32% then L65000
                if keyhit% <> 0% then L16110

                hits% = 0%:init(hex(84)) str(wa$(),1,1000)
                combquant=0:comb%=999%:combquant$,combdate$=" "
                for i% = 1% to max%
                     if intagnr$(i%)<>" " then L16300
                         wl$(i%) = " "
                         goto L16390
L16300:              if wl$(i%) = " " then L16390
                        convert str(part$(i%),9,10) to temp
                        combquant=combquant + temp
                        call "CONVERT" (combquant, -2.2, combquant$)
                        if comb% <= lt%(i%) then L16370
                           comb% = lt%(i%)
                           combdate$ = str(part$(i%),1,8)
L16370:              hits% = hits% + 1%
                     wa$(i%) = hex(94)
L16390:         next i%
                if hits% < 2% then L16110
                if combquant < .0001 then L16110

L16430:         screen% = 1%:prfac$=hex(81):psbprior$=partpriority$
L16440:         gosub'108(screen%)
                  if keyhit% <> 1% then L16460
                     gosub startovr2
                     if keyhit1% = 0% then reset_wo
L16460:           if keyhit% =  2% then l% = 0%
                  if keyhit% =  3% then l% = max(0%,max%-12%)
                  if keyhit% =  4% then l% = max(0%, l%-12%)
                  if keyhit% =  5% then l% = max(0%,min(l%+12%,max%-12%))
                  if keyhit% =  6% then l% = max(0%, l% - 1%)
                  if keyhit% =  7% then l% = max(0%,min(l%+1%,max%-12%))
                  if keyhit% <> 0% then L16430
                if psbprior$ >="@" and psbprior$ <="Z" then L16570
                     prfac$=hex(91)
                     goto L16440

L16570:         screen% = 2%:prfac$=hex(84)
                gosub'108(screen%)
                  if keyhit% <> 1% then L16600
                     gosub startovr2
                     if keyhit1% = 0% then reset_wo
L16600:           if keyhit% =  2% then l% = 0%
                  if keyhit% =  3% then l% = max(0%,max%-12%)
                  if keyhit% =  4% then l% = max(0%, l%-12%)
                  if keyhit% =  5% then l% = max(0%,min(l%+12%,max%-12%))
                  if keyhit% =  6% then l% = max(0%, l% - 1%)
                  if keyhit% =  7% then l% = max(0%,min(l%+1%,max%-12%))
                  if keyhit% =  8% then L16680
                  if keyhit% <> 24% then L16570
L16680:         goto cancel_wo

        cancel_wo
            clrflag% = 0%
            call "SHOSTAT" ("Saving Work Order Data")
            for i% = 1% to max%
            if wl$(i%)=" " then L16840

                clrstep% = 0%
                clrday%  = 0%
                clrclear%= clrflag%
                clrtag$  = intagnr$(i%)

                gosub call_clrrschd
                clrflag% = clrflag% + 1%

L16840:     next i%
            if keyhit% = 24% then L36000
            if str(planflags$(),1,1) <> "Y" then                         ~
                  call "SHOSTAT" ("Planning Combined Advice")
            goto L36000

        reset_wo
                combquant=0:comb%=999%:psbprior$,combquant$,combdate$=" "
                prfac$=hex(84)
                init (" ") wl$()
                goto L16110

        restore_wo
                call "SHOSTAT" ("Restoring Work Order Data")
                clrtag$ = all(hex(20))
                gosub call_rstrschd
                goto L11000

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * FORCED BO ADVICE                                          *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        forcepo
            init(" ") errormsg$, inpmessage$, psbquant$, duedate$,       ~
                      startday$, expcost$
            call "READ100" (#2, part$, f1%(2))
                if f1%(2) = 0% then L11000
            gosub set_status

            for fieldnr% = 1% to  3%
                gosub'057(fieldnr%)
                      if enabled% = 0% then L17210
L17160:         gosub'107(fieldnr%)
                      if keyhit% <> 1% then L17180
                         gosub startovr2
                         if keyhit1% = 0% then forcepo
L17180:               if keyhit%  = 16% then L11000
                      if keyhit%  = 32% then L65000
                      if keyhit% <>  0% then       L17160
L17210:         gosub'157(fieldnr%)
                      if errormsg$ <> " " then L17160
                next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * FORCED BUY ADVICE                                         *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************
            init (" ") errormsg$
L17560:     gosub'107(0%)
                  if keyhit% <> 1% then L17580
                     gosub startovr2
                     if keyhit1% = 0% then forcepo
L17580:           if keyhit%  =  8% then       L17720
                  if keyhit%  = 16% then       L11000
                  if keyhit%  = 32% then       L65000
                  if keyhit% <>  0% then       L17560
            fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% >  3% then L17560

L17650:     gosub'107(fieldnr%)
                  if keyhit% <> 1% then L17670
                     gosub startovr2
                     if keyhit1% = 0% then forcepo
L17670:           if keyhit% <>  0% then L17650
            gosub'157(fieldnr%)
                  if errormsg$ <> " " then L17650
            goto L17560

L17720: REM FORCE BO

            intagnr$(1) = "BO000" & date & time
            convert st% to str(intagnr$(1),3,3), pic(###)

            write #33, using L17790, part$, cd%, intagnr$(1), psbquant, st%
L17790:           FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)
            call "PIPFLAGS" (part$, 1%, cd%, psbquant, #2, #41)
            goto L11000

L18000: REM *************************************************************~
            *                                                           *~
            * RESCHEDULE JOB OR WORK ORDER                              *~
            *                                                           *~
            *************************************************************

        reschedule_wo
            call "SHOSTAT" ("One Moment Please")
            oktag$ = "JO" :  gosub load_pipins

            init (" ") rschdstep$, rschdstart$, rschddue$, psbprior$
            reschedule_in_process% = 0% : errormsg$ = " "
            rschdday%, rschdstep%, rschdend% = 9999%
            slide%      = 0%
            scr_select% = 3%        /* Used To Select Screen   */
L18051:                             /* In Multi-Use Screen     */
            gosub'058(1%)
L18055:     gosub'108(1%)
                if keyhit% <> 1% then L18065
                   gosub startovr2
                   if keyhit1% = 0% then reschedule_wo
L18065:         if keyhit% =  2% then l% = 0%
                if keyhit% =  3% then l% = max(0%,max%-12%)
                if keyhit% =  4% then l% = max(0%, l%-12%)
                if keyhit% =  5% then l% = max(0%,min(l%+12%,max%-12%))
                if keyhit% =  6% then l% = max(0%, l% - 1%)
                if keyhit% =  7% then l% = max(0%,min(l%+1%,max%-12%))
                if keyhit% = 16% then L11000
                if keyhit% = 32% then L65000
                if keyhit% <> 0% then L18055
            gosub'158(1%)
                if errormsg$ <> " " then L18051
*              IF ERRORMSG$ <> " " THEN 18055
            if slide% = 1% then slide_pipin
L18125:     gosub'058(2%)
            gosub'108(2%)
                    if keyhit% <> 1% then L18140
                       gosub startovr2
                       if keyhit1% <> 0% then L18140
                           gosub release_jobhold
                           goto  reset_rschd
L18140:             if keyhit% = 16% then L11000
                    if keyhit% = 32% then L65000
                    if keyhit% <> 0% then L18125
            gosub'158(2%)
                if errormsg$ <> " " then L18125

            for screen% = 3% to 5%
L18175:         gosub'058(screen%)
                gosub'108(screen%)
                      if keyhit% <> 1% then L18190
                         gosub startovr2
                         if keyhit1% <> 0% then L18190
                             gosub release_jobhold
                             goto  reset_rschd
L18190:               if keyhit% = 16% then L11000
                      if keyhit% = 32% then L65000
                      if keyhit% <> 0% then L18175
                gosub'158(screen%)
                      if errormsg$ <> " " then L18175
            next screen%

L18500: REM EDIT MODE

L18510:     gosub'058(0%)
            gosub'108(0%)
                if keyhit% <> 1% then L18525
                   gosub startovr2
                   if keyhit1% <> 0% then L18525
                       gosub release_jobhold
                       goto  reset_rschd
L18525:         if keyhit% <> 16% then L18528
                           gosub restore_rschd
                           goto L11000
L18528:         if keyhit% <> 32% then L18532
                           gosub restore_rschd
                           goto L65000
L18532:         if keyhit%  =  8% then save_rschd
                if keyhit%  = 24% then save_rschd
                if keyhit% <>  0% then L18510
                if cursor%(1%) <> 20% then L18510
                   screen% = 0%
                   if reschedule_in_process% <> 0% then L18560
                      if cursor%(2%) >  0% then screen% = 2%
L18560:               if cursor%(2%) > 27% then screen% = 3%
                      if cursor%(2%) > 46% then screen% = 4%
                      if cursor%(2%) > 68% then screen% = 5%
                if screen% = 0% then L18510

L18585:     gosub'058(screen%)
L18590:     gosub'108(screen%)
                if keyhit% <> 1% then L18600
                   gosub startovr2
                   if keyhit1% <> 0% then L18600
                       gosub release_jobhold
                       goto  reset_rschd
L18600:         if keyhit% <> 16% then L18603
                           gosub restore_rschd
                           goto L11000
L18603:         if keyhit% <> 32% then L18607
                           gosub restore_rschd
                           goto L65000
L18607:         if keyhit% <>  0% then L18590
            gosub'158(screen%)
                if errormsg$ <> " " then L18585
            goto L18510

        save_rschd
            if reschedule_in_process% <> 0% then L18750
            call "SHOSTAT" ("Saving Work Order Data")
                clrstep% = rschdstep%
                clrday%  = rschdday%
                clrclear%= reschedule_in_process%
                clrtag$  = rschdtag$
                gosub call_clrrschd
                vpiptag$ = rschdtag$ : gosub call_jbvbkvsa
                reschedule_in_process% = 1%

L18750:     if str(planflags$(),1,1) <> "Y" then                         ~
                  call "SHOSTAT" ("Planning Rescheduled Activity")
            goto L38000

        reset_rschd
                gosub restore_rschd
                init (" ") wl$(), errormsg$
                goto L18000

        restore_rschd
            if reschedule_in_process% = 0% then return
                call "SHOSTAT" ("Restoring Work Order Data")
                clrtag$ = rschdtag$
                gosub call_rstrschd
                vpiptag$ = rschdtag$ : gosub call_jbvbkvsa
                init (" ") errormsg$
                return

        slide_pipin
            call "PIPSLIDE" (rschdtag$,                                  ~
                     #60,                /* SYSFILE2                   */~
                     #41,                /* SFCUM2                     */~
                     #9,                 /* JBMASTR2                   */~
                     #11,                /* WCMASTER                   */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #35,                /* PIPCROSS                   */~
                     #36,                /* JBPIPXRF                   */~
                     #4,                 /* HNYMASTR                   */~
                     #7,                 /* RTEMASTR                   */~
                     #15,                /* BOMMASTR                   */~
                     #24)                /* ENGMASTR                   */

           goto L11000

        call_jbvbkvsa
*        Need to first get some Job data required for the call...
            if str(vpiptag$,,2) <> "JO" then return

            vjob$ = str(vpiptag$,12,8)
            call "READ100" (#9, vjob$, f1%(9%))
            if f1%(9%) = 0% then return

            get #9 using L19293, vpart$, vmake
L19293:         FMT POS(58), CH(25), PD(14,4)

            call "JBVSASUB" (#60, #23, "RES", vjob$, vpart$, vmake)

            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * INITIAL SELECTION SCREEN                                  *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'050(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20180,         /* SCAN FROM DATE   */~
                                    L20750,         /* Only in ATC Horzn*/~
                                    L20410,         /* RESET PIP ?      */~
                                    L20290,         /* PART No. RANGE   */~
                                    L20330,         /* PART TYPE RANGE  */~
                                    L20370,         /* CATEGORY RANGE   */~
                                    L20650,         /* PART CLASS RANGE */~
                                    L20470,         /* C / S Min Days   */~
                                    L20530,         /* S / S Min Days   */~
                                    L20590,         /* Surplus Min Days */~
                                    L20700,         /* Use Buyer Codes  */~
                                    L20800          /* Scan Pipin Only  */
                     return
L20180:     REM DEFAULT/ENABLE FOR FROM DATE
                scandate$(1) = date$
                call "DATUNFMT" (scandate$(1%))
                call "DATFMTC" (scandate$(1%))
                call "DATE" addr("G+",pldate$,489%,temp2$,err%)
                scandate$(2) = str(temp2$,1,6)
                call "DATFMTC" (scandate$(2%))
                scan%(1) = today%
                scan%(2) = 490%
                inpmessage$ = "Enter the date range in which a PIP Proble~
        ~m must occur."
                return
L20290:     REM DEFAULT/ENABLE FOR PART NUMBER RANGE
                if partr$(1%) = " " then partr$(1%) = "ALL"
                inpmessage$ = "Enter a range of Part Numbers or 'ALL'."
                return
L20330:     REM DEFAULT/ENABLE FOR PART TYPE RANGE
                if typer$(1%) = " " then typer$(1%) = "ALL"
                inpmessage$ = "Enter a range of Part Types or 'ALL'."
                return
L20370:     REM DEFAULT/ENABLE FOR CATEGORY RANGE
                if cat$(1%) = " " then cat$(1%) = "ALL"
                inpmessage$ = "Enter a range of Part Categories or 'ALL'"
                return
L20410:     REM DEFAULT/ENABLE FOR RESET PIP = TODAY ?
                if pip_reset_flag$ = " " then pip_reset_flag$ = "N"
                inpmessage$ = "Enter a 'Y' to update PIP status as of Tod~
        ~ay."
                return

L20470:     REM DEFAULT/ENABLE FOR C/S MIN DAYS TO BE A PROBLEM
                if cmin_days$ = " " then cmin_days$ = "1"
                inpmessage$ = "Enter Minimum number of days a Critical Sh~
        ~ortage must last to be shown."
                return

L20530:     REM DEFAULT/ENABLE FOR S/S Int MIN DAYS TO BE A PROBLEM
                if imin_days$ = " " then imin_days$ = "1"
                inpmessage$ = "Enter Minimum number of days a Safety/Stoc~
        ~k Intrusion must last to be shown."
                return

L20590:     REM DEFAULT/ENABLE FOR SURPLUS MIN DAYS TO BE A PROBLEM
                if smin_days$ = " " then smin_days$ = "1"
                inpmessage$ = "Enter Minimum number of days a Surplus Con~
        ~dition must last to be shown."
                return

L20650:     REM DEFAULT/ENABLE FOR CATEGORY RANGE
                if class$(1%) = " " then class$(1%) = "ALL"
                inpmessage$ = "Enter a range of Part Classes or 'ALL'."
                return

L20700:     REM DEFAULT/ENABLE FOR Use Buyer/Planner Codes
                if buyer_flag$ = " " then buyer_flag$ = "Y"
                inpmessage$ = "Enter 'Y' to Limit Selection to Parts that~
        ~ pass Buyer/Scheduler Restrictions"
                return

L20750:     REM DEFAULT/ENABLE FOR Only in ATC Horizon Flag
                if atcflag$ = " " then atcflag$ = "N"
                inpmessage$ = "Enter 'Y' to only search for problems insi~
        ~de each part's ATC Horizon"
                return

L20800:     REM DEFAULT/ENABLE FOR Scan Pipin Only
                if scan_pipin$ = " " then scan_pipin$ = "N"
                inpmessage$ = "Enter 'Y' to only search for Parts with ac~
        ~tive procurements"
                return
        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * FORCE PLANNED PROCUREMENT                                 *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'054(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21150,         /* BY DATE          */~
                                    L21280,         /* QUANTITY         */~
                                    L21300,         /* USING BOM        */~
                                    L21320,         /* PRIORITY         */~
                                    L21350,         /* PLAN TYPE        */~
                                    L21380,         /* ADD TO FILE      */~
                                    L21410          /* DEMAND CODE & LN */
                     return
L21150:     REM DEFAULT/ENABLE FOR BY DATE
            if stat%(1%) = 0% then L21220  /* TRY SS INTRUSION  */
                duedate$ = statdt$(1%)
                psbquant = -(pip%(stat%(1%)) - max(0%,cumf%(stat%(1%)))) ~
                           + pip(2)
                if psbquant < .0001 then psbquant = 0
                call "CONVERT" (psbquant, -2.2, psbquant$)
                return
L21220:     if stat%(2%) = 0% then return   /* WHY ARE WE HERE?? */
                duedate$ = statdt$(2%)
                psbquant = pip(2) - ( pip%(stat%(2%)) -                  ~
                                               max(0%,cumf%(stat%(2%))) )
                if psbquant < .0001 then psbquant = 0
                call "CONVERT" (psbquant, -2.2, psbquant$)
                return
L21280:     REM DEFAULT/ENABLE FOR QUANTITY
                return
L21300:     REM DEFAULT/ENABLE FOR BOM
                return
L21320:     REM DEFAULT/ENABLE FOR PRIORITY
                psbprior$ = partpriority$
                return
L21350:     REM DEFAULT/ENABLE FOR PLAN TYPE
                psbtype$ = "8"
                return
L21380:     REM DEFAULT/ENABLE FOR ADD TO FILE
                add$ = "YES"
                return
L21410:     REM DEFAULT/ENABLE FOR DEMAND CODE & LINE
                if add$ = "YES" then L21440
                     enabled% = 0% : prompt1$, prompt1$(1) = " "
L21440:         return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * OPTIMIZE LOGIC                                            *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'055(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L22130,         /* STARTDAY         */~
                                    L22160,         /* SUPPLY           */~
                                    L22190          /* PRIORITY         */~

                     return
L22130:     REM DEFAULT/ENABLE FOR BY DATE
            startday$ = scandate$(1)
                return
L22160:     REM DEFAULT/ENABLE FOR SUPPLY
            convert partsupplyday% to supply$, pic(###)
                return
L22190:     REM DEFAULT/ENABLE FOR PRIORITY
            psbprior$ = partpriority$
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * FORCED BO                                                 *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'057(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L23130,         /* BY DATE          */~
                                    L23300,         /* START DATE       */~
                                    L23260          /* QUANTITY         */~


                     return
L23130:     REM DEFAULT/ENABLE FOR BY DATE
            if stat%(1%) = 0% then L23200  /* TRY SS INTRUSION  */
                duedate$ = statdt$(1%)
                psbquant = -(pip%(stat%(1%)) - max(0%,cumf%(stat%(1%)))) ~
                           + pip(2)
                if psbquant < .0001 then psbquant = 0
                call "CONVERT" (psbquant, -2.2, psbquant$)
                return
L23200:     if stat%(2%) = 0% then return   /* WHY ARE WE HERE?? */
                duedate$ = statdt$(2%)
                psbquant = pip(2) - ( pip%(stat%(2%)) -                  ~
                                               max(0%,cumf%(stat%(2%))) )
                if psbquant < .0001 then psbquant = 0
                call "CONVERT" (psbquant, -2.2, psbquant$)
                return
L23260:     REM DEFAULT/ENABLE FOR QUANTITY
                return

L23300:     REM DEFAULT/ENABLE FOR START DATE
                if startday$ <> " " and startday$ <> blankdate$ then return
                l% = 0%
                convert lt$ to l%, data goto L23350
L23350:         st% = cd% - l%
                if st% < 1% then st% = 1%
                startday$ = yymmdd$(st%)
                call "DATEFMT" (startday$)
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * RESCHEDULE                                                *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'058(screen%)
            init (" ") inpmessage$
                 on screen% goto L24180,            /* SELECTIONS       */~
                                 L24210,            /* LAST GOOD STEP   */~
                                 L24250,            /* RESUME DATE      */~
                                 L24320,            /* ADJ. DUE DATE    */~
                                 L24390             /* PRIORITY         */

            inpmessage$ = "Press PF8 to Plan (PF24 - One Level), PF1 to R~
        ~eset, or PF16 to RETURN"
            if slide% = 1% then inpmessage$ = "Press PF8 to slide, PF1 to~
        ~ Reset, or PF16 to RETURN"
            return

L24180: REM DEFAULT/ENABLE FOR SELECTIONS
            inpmessage$ = "Mark Job or Advice to Slide with 'S', or with ~
        ~'X' to Reschedule"
            return
L24210: REM DEFAULT/ENABLE FOR LAST COMPLETE STEP
            inpmessage$ = "Enter Last Step to be Completed. Blank Default~
        ~s To First Step Of Routing."
            return
L24250: REM DEFAULT/ENABLE FOR RESUME DATE
            if rschdstart$ <> " " and rschdstart$ <> blankdate$ then L24300
               call "DATE" addr("G+",pldate$,rschdstart%,rschdstart$,err%)
                    if err% <> 0% then rschdstart$ = date
               call "DATEFMT" (rschdstart$)
L24300:     inpmessage$ = "Enter Lowest Date to Resume Activity"
            return
L24320: REM DEFAULT/ENABLE FOR ADJ. DUE DATE
            if rschddue$ <> " " and rschddue$ <> blankdate$ then L24370
               call "DATE" addr("G+",pldate$,rschdend%-1%,rschddue$,err%)
                    if err% <> 0% then rschddue$ = date
               call "DATEFMT" (rschddue$)
L24370:     inpmessage$ = "Enter Adjusted Due Date Desired"
            return
L24390: REM DEFAULT/ENABLE FOR PRIORITY
            if psbprior$ <> " " then L24420
               psbprior$ = partpriority$
L24420:     inpmessage$ = "Enter Priority for Reschedule"
            return

L27500: REM *************************************************************~
            *  L O A D   B U Y E R   A N D   P L A N N E R   C O D E S  *~
            *************************************************************

            bcodes% = 1% : b_admin%, p_admin% = 0%

            call "OPENCHCK" (#20, 0%, f2%(20),   0%, " ")
                if f2%(20) <> 0% then b_admin% = 1%
            call "OPENCHCK" (#21, 0%, f2%(21),   0%, " ")
                if f2%(21) <> 0% then p_admin% = 1%
            if p_admin% = 1% and b_admin% = 1% then return

            REM Is Buyer Codes Feature Used?
            if b_admin% = 1% then L27610
            plowkey$ = all(hex(00))
            call "READ102" (#20, plowkey$, f1%(20))
                if f1%(20) = 1% then L27610
                     b_admin% = 1%

L27610:     REM Is Planner Codes Feature Used?
            if p_admin% = 1% then L27640
            plowkey$ = all(hex(00))
            call "READ102" (#21, plowkey$, f1%(21))
                if f1%(21) = 1% then L27640
                     p_admin% = 1%

L27640:     REM See if Operator is an Administrator
            if b_admin% = 1% then L27670
            call "CMSMACHK" ("VBK", lfac$(1), lfac$(2))
                if lfac$(1) = "Y" then b_admin% = 1%
                if lfac$(2) = "Y" then b_admin%, p_admin% = 1%

L27670:     if p_admin% = 1% then L27700
            call "CMSMACHK" ("SFC", lfac$(1), lfac$(2))
                if lfac$(1) = "Y" then p_admin% = 1%
                if lfac$(2) = "Y" then b_admin%, p_admin% = 1%

L27700:     if b_admin% = 1% then L27760
                c% = 0%
                plowkey$ = str(userid$) & hex(00000000)
L27715:         call "PLOWNEXT" (#20, plowkey$, 3%, f1%(20))
                     if f1%(20) = 0% then L27760
                c% = c% + 1%
                if c% = 100% then L27760
                bcodes$(c%) = str(plowkey$,4,3)

                goto L27715

L27760:     if p_admin% = 1% then L27820
                c% = 0%
                plowkey$ = str(userid$) & hex(00000000)
L27775:         call "PLOWNEXT" (#21, plowkey$, 3%, f1%(21))
                     if f1%(21) = 0% then L27820
                c% = c% + 1%
                if c% = 100% then L27820
                pcodes$(c%) = str(plowkey$,4,3)

                goto L27775

L27820:    if bcodes$() <> "ALL" then L27835
                b_admin% = 1%

L27835:    if pcodes$() <> "ALL" then L27850
                p_admin% = 1%

L27850:     if f2%(20) = 0% then close #20
            if f2%(21) = 0% then close #21
            return

        REM *************************************************************~
            * C R E A T E   W O R K F I L E   F O R   P A R T   R N G E *~
            * CREATES A WORKFILE OF SELECTABLE PARTS TO MANAGE          *~
            *   IF 'ALL' PARTS WERE NOT SELECTED.                       *~
            *************************************************************

        setup_scan

            part$ = partr$(3%)
            workfl_flag% = 1%
            i%, recnum% = 0%
            keyhit% = 9%       /* Set so Set Status shows first part */
            if buyer_flag$ = "Y" and bcodes% = 0% then gosub L27500
            if buyer_flag$ = "Y" and b_admin% = 0% and p_admin% = 0%     ~
                                                             then L28140
               if partr$(1%) = "ALL" and cat$(1%) = "ALL" then i% = 1%
               if typelow% = 0% and typehigh% = 999% then i% = i% + 1%
               if class$(1%) = "ALL" then i% = i% + 1%
               if buyer_flag$ = "N" or b_admin% = 1% or p_admin% = 1%    ~
                                     then i% = i% + 1%
               if i% = 4% then workfl_flag% = 0% /* Go Direct to scan */

L28140:     if pip_reset_flag$ = "Y" and workfl_flag% = 0%               ~
                                     then gosub reset_pip
            part$ = partr$(3%)
            if workfl_flag% = 0% then scan_parts

            if pip_reset_flag$ <> "Y"                                    ~
                then call "SHOSTAT" ("Creating Work File . . .")         ~
                else call "SHOSTAT" ("Creating Work File   &             ~
        ~Updating PIPMASTR Status")
            call "WORKOPN2" (#64, "OUTPT", rec_count%, f1%(64%))
                if f1%(64%) = 0% then L28260
                     workfl_flag% = 0%
                     goto L28140

L28260:     plowkey$ = partr$(3%) : init (hex(00)) str(plowkey$,26%)
            if scan_pipin$ = "Y" then L28316

            call "READ102" (#2, plowkey$, f1%(2%))
                goto L28310
L28300:     call "READNEXT" (#2, f1%(2%))
L28310:         if f1%(2%) = 0% then end_build
            if key(#2) > partr$(4%) then end_build
            gosub L28330
            goto L28300

L28316:     call "PLOWALTS" (#33, plowkey$, 1%, 0%, f1%(33%))
               if f1%(33%) = 0% then end_build
            if str(plowkey$,,25%) > partr$(4%) then end_build
            call "REDALT0" (#2, str(plowkey$,,25%), 0%, f1%(2%))
               if f1%(2%) = 0% then L28322
            gosub L28330
L28322:     init (hex(ff)) str(plowkey$,26%)
            goto L28316

L28330:     get #2, using L28340, part$, parttype%
L28340:         FMT XX(1), CH(25), POS(2019), BI(2)

            if parttype% < typelow% or parttype% > typehigh% then return
            gosub L28900          /* GET HNYMASTR RECORD */
                if f1%(4%) = 0% then return

            if cat$(1%) = "ALL" then L28460
            if catr$ < cat$(1%) or catr$ > cat$(2%) then return
L28460:     if class$(1%) = "ALL" then L28480
            if pclass$ < class$(1%) or pclass$ > class$(2%) then return
L28480:     if buyer_flag$ <> "Y" then L28560

            if b_admin% = 1% then L28520
                search bcodes$() = str(buyer$,,3) to cursor%() step 3
                     c% = cursor%(1)
L28520:     if p_admin% = 1% then L28560
                search pcodes$() = str(planner$,,3) to cursor%() step 3
                     if cursor%(1) = 0% and c% = 0% then return

L28560:     if pip_reset_flag$ <> "Y" then L28600
                     fcst% = max(1%, today% - 1%)
                     call "PIPFLAGS" (part$, fcst%, today%, 0, #2, #41)

L28600:     if recnum% < rec_count% then L28740 /* Too many records? */
L28610:              keyhit1% = 3%
                call "ASKUSER" (keyhit1%, "Inefficient Range Selection", ~
                                     "The Selected Range is too large to"~
                                & " provide increased processing speed", ~
                                "Press RETURN to scan all PIP records",  ~
                                "or Press PF1 to reselect range")

                     if keyhit1%  = 1% then L29970         /* Startover */
                     if keyhit1% <> 0% then L28610
                     return clear all
                     goto L28840               /* Continue w/o Workfile */

        REM Write to workfile after passing selection tests
L28740:     write #64, using L28880, part$, partdescr$, "XXXX", parttype%,~
                                    catr$, pclass$, lt$
            recnum% = recnum% + 1%
            return

        end_build
            part$ = partr$(3%)
            call "WORKOPN2" (#64, "IO", 0%, f1%(64%))
                if f1%(64%) = 0% then scan_parts
L28840:     workfl_flag% = 0%
            part$ = partr$(3%)
            call "FILEBGON" (#64)
            goto scan_parts


L28880: FMT  CH(25), CH(32), CH(4), BI(2), CH(4), CH(4), CH(3)


L28900: REM *************************************************************~
            *    L O A D   H N Y M A S T R   D A T A   R E C O R D      *~
            *************************************************************

            call "READ100" (#4, part$, f1%(4%))
                if f1%(4%) = 0% then L28945
            get #4, using L28930, partdescr$, uom$, catr$, pclass$, lt$,  ~
                                 buyer$, planner$
L28930:         FMT XX(25), CH(32), XX(16), CH(4), XX(12), CH(4),        ~
                    POS(133), CH(4), POS(170), CH(3), POS(200), CH(3),   ~
                    POS(309), CH(3)
            return

L28945:     partdescr$ = "PART NOT ON FILE"
            uom$, catr$, pclass$, lt$, buyer$, planner$ = " "
            return


L29000: REM INITIALIZE SCREEN VARIABLES FOR STATUS RESULTS SCREEN
           init (" ") status$(), partdescr$, onhand$, safety$, lt$, uom$,~
               catr$, pansize$, pclass$, moq$, parttypedescr$, ptype$
           return

        REM  **************  MINI - START OVER  *************************~
             *  THIS VERSION OF STARTOVER IS FOR THE VARIOUS PIP        *~
             *  MANAGEMENT SCREENS SO THAT THE WORKFILE ISN'T DELETED   *~
             *  AND SO THAT A START OVER ONLY RESTARTS THAT SCREEN &    *~
             *  NOT THE ENTIRE PROGRAM.                                 *~
             ************************************************************

        startovr2

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
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

L29970:     return clear all
            if workfl_flag% = 1% then call "FILEBGON" (#64)
            goto startover_entry


        REM *************************************************************~
            *      INPUT ROUTINES, FIND PROBLEMS IN PIPMASTR            *~
            *                                                           *~
            * INITIAL SCAN                                              *~
            *************************************************************

        reset_pip

            call "SHOSTAT" ("Updating PIPMASTR Status")
            fcst% = max(1%, today%-1%)
            plowkey$ = partr$(3%)
            call "READ102" (#2, plowkey$, f1%(2))
L30060:         if f1%(2)=0% then L30100
            get #2, using L30070, part$
L30070:         FMT XX(1), CH(25)
            if part$ > partr$(4%) then L30100
            call "PIPFLAGS" (part$, fcst%, today%, 0, #2, #41)
            call "READNEXT" (#2, f1%(2))
            goto L30060

L30100:     part$ = " "
            return

        scan_parts
            if keyhit% <> 9% then call "SHOSTAT" ("Scanning . . .")
            clrpart$ = part$
            init (" ") errormsg$
            gosub set_scantype

            if workfl_flag% = 0% then L30255

                call "READ103" (#64, part$, f1%(64%))
                    if f1%(64%) = 0% then end_scan
                goto L30185
L30170:              call "READNXT1" (#64, f1%(64%))
                          if f1%(64%) = 0% then end_scan

L30185:         get #64, using L28880, part$, partdescr$, wstat$,         ~
                                   typescan%, catr$, pclass$, lt$
                if scantype% <> 2% then L30205
                    if typescan% > 0% and typescan% < 500% then L30170
L30205:         if scantype% <> 3% then L30215
                    if typescan% = 0% or typescan% > 499% then L30170
L30215:         gosub scan_validate
                if passed% = 1% then L30235
            goto L30170    /* NEXT PART */

L30235:         call "READ100" (#2, part$, f1%(2%))
                    if f1%(2%) = 0% then end_scan
                goto L30270

L30255:     call "READ102" (#2, part$, f1%(2%))
                if f1%(2%) = 0% then end_scan

L30270:     get #2, using L30275, scanstat$, part$, typescan%
L30275:         FMT CH(1), CH(25), POS(2019), BI(2)

            if scantype% <> 2% then L30295
                if typescan% > 0% and typescan% < 500% then L30365
L30295:     if scantype% <> 3% then L30310
                if typescan% = 0% or typescan% > 499% then L30365

L30310:     if keyhit% = 10% and pos("89"=scanstat$) = 0% then L30365
            if keyhit% = 11% and pos("45"=scanstat$) = 0% then L30365
            wstat$ = " "
            gosub set_status

            if workfl_flag% = 0% then L30350
                 rewrite #64, using L28880, part$, partdescr$, wstat$,    ~
                                       typescan%, catr$, pclass$, lt$
L30350:   gosub scan_validate
          if passed% = 1% then show_condition

L30365: if workfl_flag% = 1% then L30170 else L30255

        end_scan
            errormsg$="No 'NEXT PART' Found in scan"
            part$ = clrpart$
            call "READ100" (#2, part$, f1%(2))
                if f1%(2) <> 0% then L30415    /* This Shouldn't Happen */
                     gosub L29000
                     fieldnr% = 2%
                     goto L10970
L30415:     gosub L28900          /* GET HNYMASTR RECORD */
            gosub set_status
            goto show_condition

        REM *************************************************************~
            *            VALIDATE STATUS BASED ON PF KEY                *~
            *************************************************************
        scan_validate
                passed% = 1%
                if keyhit% =  9%  or wstat$ = "XXXX" then L30550
                if keyhit% = 25% and wstat$ <> " "   then L30550

                               /* NEXT CRITICAL SHORTAGE */
                if keyhit% = 10% and pos(wstat$="C") <> 0% then L30550

                               /* NEXT SAFETY STOCK INTRUSION */
                if keyhit% = 11% and pos(wstat$="I") <> 0% then L30550

                               /* NEXT SURPLUS CONDTION */
                if keyhit% = 12% and pos(wstat$="R") <> 0% then L30550

                               /* NEXT LATE ACTIVITY    */
                if keyhit% =  8% and pos(wstat$="L") <> 0% then L30550
            passed% = 0%
L30550:     return

        REM *************************************************************~
            *                  SET SCAN MODE                            *~
            *************************************************************
        set_scantype
              if str(scantype$,1,1) <> "M" then L30980
                 scantype$="MFG":scantype%=2%:return
L30980:       if str(scantype$,1,1) <> "P" then L30990
                 scantype$="PUR":scantype%=3%:return
L30990:       scantype$="ALL":scantype%=1%:return

        REM *************************************************************~
            *      SET STATUS AND MESSAGES IF ANY                       *~
            *                                                           *~
            *************************************************************

        set_status
            mat stat% = zer
            get #2, using L31045, part$, pip%(), pip(), parttype%,        ~
                     partpriority%
L31045:         FMT XX(1),CH(25),490*BI(4),4*PD(14,4),BI(2),XX(2),BI(2)

            atch% = max(0%, mod(partpriority%, 1000%))
            if atcflag$ = "N" then atch% = 999%
            atch% = today% + atch%

            if parttype% = 0% then parttypedescr$="Build To Order"
            if parttype% > 0% and parttype% < 200% then                  ~
                         parttypedescr$="Non-Planned Part"
            if parttype% > 199% and parttype% < 490% then                ~
                         parttypedescr$="Purchased Part"
            if parttype% > 489% and parttype% < 500% then                ~
                         parttypedescr$="Purchased Tool"
            if parttype% > 499% then parttypedescr$="Manufactured Part"
            if parttype% > 789% and parttype% < 800% then                ~
                         parttypedescr$="Manufactured Tool"
            call "CONVERT" (pip(1), 2.2, onhand$)
            call "CONVERT" (pip(2), 2.2, safety$)
            call "CONVERT" (pip(3), 2.2, moq$)
            call "CONVERT" (pip(4), 2.2, pansize$)
            if workfl_flag% = 1% then L31150
                gosub L28900          /* GET HNYMASTR RECORD */
L31150:     mat cumf% = zer
            call "READ100" (#41, part$, f1%(41%))
            if f1%(41%) = 1% then get #41, using L31170, cumf%()
L31170:              FMT POS(26), 490*BI(4)
            surp%, cs%, fs%, ssi% = 0%
            convert parttype% to ptype$, pic(000)

        for i% = scan%(1%) to 490%
              cumf%(i%) = max(0%, cumf%(i%))
              t_shelf% = pip%(i%) - cumf%(i%)

                 /* Critical Shortage Overrides a Forecast Shortage */
              if cs% = 2% then L31320
              if cs% = 1% then L31240
                if pip%(i%) >= 0% or i% > min(atch%,scan%(2%)) then L31270
                       stat%(1%) = i%
                       cs% = 1% : fs% = 0%
L31240:           if pip%(i%) >= 0% then L31255
                       csend% = i%
                       goto L31270
L31255:           if cmin_days% > csend% - stat%(1%) + 1% then           ~
                       cs%, csend%, stat%(1%) = 0%       else cs% = 2%

L31270:       if fs% = 1% or cs% <> 0% then L31320
              if stat%(1%) <> 0% then L31290
               if i% > min(atch%,scan%(2%)) or t_shelf% >= 0% then L31320
                       stat%(1%) = i%
L31290:           if t_shelf% >= 0% then L31305
                       csend% = i%
                       goto L31435
L31305:           if cmin_days% > csend% - stat%(1%) + 1% then           ~
                       fs%, csend%, stat%(1%) = 0%       else fs% = 1%

L31320:       if ssi% = 1% then L31380
              if abs(pip(2%)) < .0001 then L31380
              if stat%(2%) <> 0% then L31350
                  if i% > min(atch%,scan%(2%)) then L31380
                  if t_shelf% < 0% or t_shelf% >= pip(2%) then L31380
                       stat%(2%) = i%
L31350:           if t_shelf% < 0% or t_shelf% >= pip(2%) then L31365
                       ssend% = i%
                       goto L31435
L31365:           if imin_days% > ssend% - stat%(2%) + 1% then           ~
                       ssi%, ssend%, stat%(2%) = 0%      else ssi% = 1%

L31380:       if surp% = 1% then L31435
              if stat%(3%) <> 0% then L31405
                  if i% > min(atch%,scan%(2%)) then L31435
                  if t_shelf% <= pip(2%) + pip(3%) then L31435
                       stat%(3%) = i%
L31405:           if t_shelf% <= pip(2%) + pip(3%) then L31420
                       stat%(4%) = i%
                       goto L31435
L31420:           if smin_days% > stat%(4%)- stat%(3%) +1% then          ~
                       surp%, stat%(3%), stat%(4%) = 0%   else surp% = 1%

L31435:       if i% < min(atch%,scan%(2%)) then L31460  /* End Date yet? */
                  if (fs% = 0% or cs% = 1%) and stat%(1%) > 0% then L31460
                  if ssi% = 0% and stat%(2%) > 0% then L31460
                  if surp% = 0% and stat%(3%) > 0% then L31460
                     goto L31470          /* ALL DONE */
L31460: next i%

L31470:     init (" ") status$(),statnm$(),statdt$(),temp2$,tmpdate$
            statct%=1%
            if stat%(1%) = 0% then L31565
              str(wstat$,1,1) = "C"
              call "DATE" addr("G+",pldate$, stat%(1%)-1%,               ~
                                               statdt$(1%), err%)
              call "DATEFMT" (statdt$(1%))
              if cs% > 0% then L31540     /* NOT A FORECAST SHORTAGE */
              temp = pip%(stat%(1%)) - cumf%(stat%(1%))
              call "CONVERT" (temp, -2.2, statnm$(1%))
              status$(statct%) = "Shelf Quantity of " & statnm$(1%) &    ~
                        " on " & statdt$(1%) & " is a FORECAST SHORTAGE"
              statct% = statct% + 1%
              goto L31565
L31540:           temp = pip%(stat%(1%))
                  call "CONVERT" (temp, -2.2, statnm$(1%))
                  status$(statct%) = "PIP Quantity of " & statnm$(1%) &  ~
                       " on " & statdt$(1%) & " is a CRITICAL SHORTAGE"
                  statct% = statct% + 1%

L31565:     if stat%(2%) = 0% then L31615
              str(wstat$,2,1) = "I"
          call "DATE" addr("G+",pldate$, stat%(2%)-1%, statdt$(2%), err%)
              call "DATEFMT" (statdt$(2%))
              temp = pip%(stat%(2%)) - cumf%(stat%(2%))
              call "CONVERT" (temp, -2.2, statnm$(2%))
              status$(statct%) = "Shelf Quantity of " & statnm$(2%) &    ~
                          " on " & statdt$(2%) & " is BELOW SAFETY STOCK"
              statct% = statct% + 1%

L31615:     if stat%(3%) = 0% then L31705
              str(wstat$,3,1) = "R"
              call "DATE" addr("G+",pldate$, stat%(3%)-1%,               ~
                                               statdt$(3%), err%)
              call "DATEFMT" (statdt$(3%))
              temp = pip%(stat%(3%))
              call "CONVERT" (temp, -2.2, statnm$(3%))
              call "DATE" addr("G+",pldate$, stat%(4%)-1%,tmpdate$, err%)
              call "DATEFMT" (tmpdate$)
              temp2 = temp - ( cumf%(stat%(3%)) + pip(2%) + pip(3%) )
              call "CONVERT" (temp2, -2.2, temp2$)
              status$(statct%) = "SURPLUS Condition: " & temp2$ & " of " ~
                        & statnm$(3%) & " is Surplus:"
              statct% = statct% + 1%
              status$(statct%) = "                   From " & statdt$(3%)~
                        & " Until " & tmpdate$
              statct% = statct% + 1%

L31705:       init (hex(00)) pipplow$
              str(pipplow$,1,25)=part$
              call "PLOWALTS" (#33, pipplow$, 1%, 25%, f1%(33))
                if f1%(33) = 0% then L31780
              get #33, using L31730, in%, qty
L31730:           FMT XX(25), BI(4), XX(19), PD(14,4)
              if in% >= today% then L31780
              call "DATE" addr("G+",pldate$, in%-1%, statdt$(4%), err%)
              call "DATEFMT" (statdt$(4%))
              str(wstat$,4,1) = "L"
              call "CONVERT" (qty, -2.2, statnm$(4%))
              status$(statct%) = "Scheduled Addition of " & statnm$(4%) &~
                          " on " & statdt$(4%) & " is LATE"
              statct% = statct% + 1%

L31780:       init (hex(00)) pipplow$
              str(pipplow$,1,25)=part$
              call "PLOWALTS" (#34, pipplow$, 1%, 25%, f1%(34))
                if f1%(34) = 0% then L31855
              get #34, using L31805, out%, qty
L31805:           FMT XX(44), BI(4), XX(8), PD(14,4)
              if out% >= today% then L31855
              call "DATE" addr("G+",pldate$, out%-1%, statdt$(5%), err%)
              call "DATEFMT" (statdt$(5%))
              str(wstat$,4,1) = "L"
              call "CONVERT" (qty, -2.2, statnm$(5%))
              status$(statct%) = "Scheduled Withdrawal of " & statnm$(5%)~
                        & " on " & statdt$(5%) & " is LATE"
              statct% = statct% + 1%

L31855:     if status$() = " " then                                      ~
                status$(1) = "NO MANAGEMENT SITUATIONS DETECTED"

            ltss% = max(1%,(parttype%/100%) - 1%)
            partpriority% = int(partpriority%/1000%)
            if partpriority% = 0% then partpriority% = 32%               ~
                else partpriority% = partpriority% + 64%
            partpriority$ = bin(partpriority%, 1)
            if partpriority$ = " " then partpriority$ = pprior$(ltss%)
            if partpriority$ = " " then partpriority$ = "A"
            partsupplyday%=max(1%,min(dts%(ltss%),490%))
            return

        REM *************************************************************~
            *                                                           *~
            *      LOAD ARRAYS WITH PIPINS                              *~
            *                                                           *~
            *************************************************************
        load_pipins

            init (" ") wl$(),intagnr$(),outtagnr$(),part$():l%,max% = 0%

            init (hex(00)) plowkey$:str(plowkey$,1,25)=str(part$,1,25)

L32600:         if max% > 998% then L32910
            call "PLOWALTS" (#33, plowkey$, 1%, 25%, f1%(33))
                if f1%(33) = 0% then L32920
            get #33, using L32640, lt%(max%+1%), intagnr$(max%+1%), temp
L32640:         FMT XX(25), BI(4), CH(19), PD(14,4)

            if str(intagnr$(max%+1%),1,2) = "WO" then L32710
            if str(intagnr$(max%+1%),1,2) = "BW" then L32710
            if str(intagnr$(max%+1%),1,2) = str(oktag$,1,2) then L32710
L32680:     init (" ") intagnr$(max%+1%)
            goto L32600

L32710:     call "DATE" addr("G+",pldate$, lt%(max%+1%) - 1%,            ~
                                        tmpdate$, err%)
            if err%<> 0% then L32680
            call "DATEFMT" (tmpdate$)
            str(part$(max%+1%),1,8) = tmpdate$
            call "CONVERT" (temp, 2.2, str(part$(max%+1%),9,10))
            max% = max% + 1%

            call "READ100" (#8, intagnr$(max%), f1%(8))
                if f1%(8) = 0% then L32830
            get #8 using L32810,str(part$(max%),23,3),str(part$(max%),19,3)
L32810:                FMT XX(25), CH(3),XX(19),XX(25),CH(3)

L32830:     pipplow$=intagnr$(max%)
            init (hex(00)) str(pipplow$,20)
            call "PLOWALTS" (#35, pipplow$, 1%, 19%, f1%(35))
            if f1%(35) = 0% then L32600
            get #35, using L32880, outtagnr$(max%)
L32880:         FMT CH(19)
            goto L32600

L32910:     outtagnr$(max%+1%) = "(ARRAY FULL)"
L32920:     max% = max% + 1%
            part$(max%) = " END OF LIST"
            return

        REM *************************************************************~
            *                                                           *~
            *      LOAD ARRAYS WITH PIPOUTS                             *~
            * COMMON ROUTINE                                            *~
            *************************************************************
        load_pipouts

            init (" ") wl$(),intagnr$(),outtagnr$(),part$():l%,max% = 0%

            init (hex(00)) plowkey$:str(plowkey$,1,25)=str(part$,1,25)

L33100:     call "PLOWALTS" (#34, plowkey$, 1%, 25%, f1%(34))
                if f1%(34) = 0% then L33350
                if max% > 998% then L33345
            get #34, using L33130, intagnr$(max%+1%), temp%, temp
L33130:         FMT CH(19), XX(25), BI(4), XX(8), PD(14,4)
            if str(intagnr$(max%+1%),1,2) = "BO" then L33190
            if str(intagnr$(max%+1%),1,2) = "WO" then L33190
            if str(intagnr$(max%+1%),1,2) = "BW" then L33190
L33160:     init (" ") intagnr$(max%+1%)
            goto L33100

L33190:     call "DATE" addr("G+",pldate$, temp% - 1%, tmpdate$, err%)

            if err%<> 0% then L33160
            call "DATEFMT" (tmpdate$)
            str(outtagnr$(max%+1%),1,8) = tmpdate$
            call "CONVERT" (temp, 2.2, str(outtagnr$(max%+1%),10,10))
            max% = max% + 1%


            pipplow$=intagnr$(max%)
            init (hex(00)) str(pipplow$,20)
            call "READ100" (#33, pipplow$, f1%(33))
            if f1%(33) = 0% then L33100
            get #33, using L33320, part$(max%), lt%(max%)
L33320:         FMT CH(25), BI(4)
            goto L33100

L33345:     part$(max%+1%) = "ARRAY FULL:"
L33350:     max% = max% + 1%
            part$(max%) = part$(max%) & "END OF LIST"
            return

L34000: REM *************************************************************~
            *                                                           *~
            *      PROCURE SOME MORE, DIRECTLY                          *~
            *                                                           *~
            *************************************************************

*          PSBCODE$  = " "
*          PSBLINE$  = " "
            psbpart$  = part$
            psbrte$   = " "
            psbcd%    = cd%
            psbtoday% = today%              /*  OR PBSLINE$ <> " " */
            planflag% = 5% : if psbcode$ <> " "                          ~
                                then planflag% = planflag% + 100000%
            gosub call_plansub

            if final% > 0% and final% < 491% then L34270
                inpmessage$="Press RETURN to Acknowledge"
                gosub'104(0%)
                inpmessage$=" "
                goto L14500

L34270:     if add$ = "YES" then L34330
            init (hex(00)) plowkey$
            str(plowkey$,1,19)=str(psbcode$,1,16)&str(psbline$,1,3)
            call "DELETE" (#35, plowkey$, 19%)
            goto L34430

L34330:     put #1, using L34480, "9", psbtype$, psbprior$,               ~
                yymmdd$(psbcd%), psbcode$, psbline$, psbpart$, psbquant$,~
                " ",          "001", yymmdd$(psbtoday%), yymmdd$(final%),~
                " ",           hex(00000000000000000000)

            if final% <= psbcd% then L34420
               put #1, using L34400, "7"
L34400:            FMT POS(1), CH(1)

L34420:     write #1
L34430:     inpmessage$="Press RETURN to Acknowledge"
            gosub'104(0%)
            inpmessage$=" "
            goto L11000

L34480:     FMT CH( 1),                       /* RECORD STATUS         */~
                CH( 1),                       /* DEMAND TYPE           */~
                CH( 1),                       /* PRIORITY              */~
                CH( 6),                       /* REQ COMPLETION DATE   */~
                CH(16),                       /* DEMAND CODE           */~
                CH( 3),                       /* DEMAND LINE           */~
                CH(25),                       /* PART NEEDED           */~
                CH(10),                       /* QUANTITY              */~
                CH(10),                       /* WC (FOR PM ONLY)      */~
                                              /* WHICH BOM REQD?       */~
                                              /* WHICH WCROUTE REQD?   */~
                CH( 3),                       /* DEL TO/SHIP FROM WHSE */~
                CH( 6),                       /* DATE LAST PLANNED     */~
                CH( 6),                       /* PLANNED COMPL DATE    */~
                CH(25),                       /* CUSTOMER IF TYPE 1    */~
                                              /* APPROVAL STATUS       */~
                                              /* APPROVAL BY           */~
                CH(10)                        /* APPROVAL ON MMDD (BIN)*/

L36000: REM *************************************************************~
            *                                                           *~
            *      PROCURE NEW COMBINED WO QUANTITY                     *~
            *                                                           *~
            *************************************************************

            psbquant$ = combquant$:duedate$=combdate$:add$="NO"

            psbcode$  = " "
            psbline$  = " "
            psbtype$  = "8"
            psbpart$  = part$
            psbrte$   = " "
            psbbom$   = " "
            psbquant    = combquant
            psbcd%, cd% = comb%
            psbtoday%   = today%
            planflag%   = 5%

            if keyhit% = 24% then L36290

            gosub call_plansub

            if final% > 0% and final% < 491% then L36320
                inpmessage$="Press RETURN to Acknowledge"
                gosub'104(0%)
                inpmessage$=" "
L36290:         combineflag% = 1%
                goto L14500

L36320:     init (hex(00)) plowkey$
            str(plowkey$,1,19)=str(psbcode$,1,16)&str(psbline$,1,3)
            call "DELETE" (#35, plowkey$, 19%)
            goto L11000

L38000: REM *************************************************************~
            * PROCESSOR FOR RESCHEDULE ROUTINE                          *~
            *************************************************************

            psbcode$  = " "
            psbline$  = " "
            psbtype$  = "7"
            psbpart$  = part$
            psbrte$   = " "
            psbbom$   = rschdbom$

            psbquant    = rschdquant
            psbcd%, cd% = rschdcd%
            psbtoday%   = rschdtoday%
            planflag%   = 5% + (100%*rschdstep%)

            if keyhit% = 24% then str(planflags$(),46,6)=hex(ffffffffffff)

            gosub call_plansub

            str(planflags$(),46,6)=str(planflagshold$(),46,6)

               inpmessage$="Press RETURN to Acknowledge"
               oldtag$ = intagnr$(1)
               init (" ") intagnr$(),outtagnr$(),part$(),wl$()
               init (hex(84)) str(wa$(),1,1000)
               wa$(1)=hex(94):wl$(1)="X":l%=0%:max%=1%
               intagnr$(1)=rschdtag$
               part$(1)=reviewpart$
               if final% < 1% or final% > 490% then L38390
                  call "READ100" (#33, oldtag$, f1%(33))
                     if f1%(33) = 0% then L38390
                  get #33, using L38300, temp%, temp
L38300:               FMT XX(25), BI(4), XX(19), PD(14,4)
                  call "DATE" addr("G+", pldate$, temp%-1%,              ~
                                               tmpdate$, err%)
                  call "DATEFMT" (tmpdate$)
            str(part$(1),1,8) = tmpdate$
                  call "CONVERT" (temp, 2.2, str(part$(1),9,10))
                  if str(rschdtag$,1,2) = "JO" then inpmessage$ =        ~
                           inpmessage$ & ": Review kit list for accuracy"
                     goto L38410

L38390:        outtagnr$(1)=newtag$

L38410:        gosub'108(0%)
               errormsg$, inpmessage$=" "
               if final% < 1% or final% > 490% then L18500

            init (hex(00)) plowkey$
            str(plowkey$,1,19)=str(psbcode$,1,16)&str(psbline$,1,3)
            call "DELETE" (#35, plowkey$, 19%)

            if str(rschdtag$,1,2) = "JO" then L38520
               if rschdstep% < 1% then L11000

L38520:     newtag$ = rschdtag$
            call "SHOSTAT" ("Aligning Rescheduled Tags")
            gosub call_jbretag
            if str(rschdtag$,1,2) = "WO" then L11000

            call "READ100" (#33, rschdtag$, f1%(33))
                if f1%(33) = 0% then L11000
            get #33, using L38590, rschdquant
L38590:         FMT XX(25), XX(4), XX(19), PD(14,4)
            call "READ101" (#9, str(rschdtag$,12,8), f1%(9))
                if f1%(9) = 0% then L11000
            get #9, using L38612, temp
L38612:         FMT POS(91), PD(14,4)
            put #9, using L38630, round(rschdquant+temp, 4)
L38630:         FMT POS(83), PD(14,4)
            call "DATE" addr("G+", pldate$, final%-1%, temp2$, err%)
                if err% <> 0% then L38680
            put #9, using L38670, str(temp2$,1,6)
L38670:         FMT POS(174), CH(6)
L38680:     if rschdstep% > 0% then L38730
            call "DATE" addr("G+", pldate$, first%-1%, temp2$, err%)
                if err% <> 0% then L38730
            put #9, using L38720, str(temp2$,1,6)
L38720:         FMT POS(168), CH(6)
L38730:     rewrite #9
            goto L11000

        REM *************************************************************~
            * Link to JBRELSUB                                          *~
            *************************************************************
        call_jbrelsub
            call                                                         ~
              "JBRELSUB" (#02,                   /* PIPMASTR File UFB  */~
                          #04,                   /* HNYMASTR File UFB  */~
                          #09,                   /* JBMASTR2 File UFB  */~
                          #50,                   /* SERTIF   File UFB  */~
                          #51,                   /* SERMASTR File UFB  */~
                          #52,                   /* SERWORK  File UFB  */~
                          #41,                   /* SFCUM2   FILE UFB  */~
                          #07,                   /* RTEMASTR File UFB  */~
                          #11,                   /* WCMASTR  File UFB  */~
                          #53,                   /* GLMAIN   File UFB  */~
                          #54,                   /* USERINFO File UFB  */~
                          #23,                   /* WCOUT    File UFB  */~
                          #33,                   /* PIPIN    File UFB  */~
                          #34,                   /* PIPOUT   File UFB  */~
                          #08,                   /* JBCROSS2 File UFB  */~
                          #55,                   /* HNYQUAN  File UFB  */~
                          #60,                   /* SYSFILE2 File UFB  */~
                          #35,                   /* PIPCROSS File UFB  */~
                          #36,                   /* JBPIPXRF File UFB  */~
                          #56,                   /* STORNAME File UFB  */~
                          #15,                   /* BOMMASTR File UFB  */~
                          #24,                   /* ENGMASTR File UFB  */~
                          f2%())                 /* Entire F2% array   */

            goto manage_part

        REM *************************************************************~
            *      I N P U T   R A N G E S   F O R   P I P S C A N      *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *************************************************************

            deffn'100(fieldnr%)

                pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "                        (4)Previous Field ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                        (9)Proceed to Scan~
        ~ ALL                 (16)Exit Program"
                pfkeys$ = hex(000104090d0f2010)
                if fieldnr% < 2% or edit% = 1%  then                     ~
                                    str(pfktext$(2),24,20) = " "
                if fieldnr% = 0% then str(pfktext$(3),63,) = hex(84)  &  ~
                                                        "(16)Scan PIP  "
                if fieldnr% > 3% then L40105
                     pfkeys$ = hex(000104000d0f2010)
                     str(pfktext$(3),25,23) = " "

L40105:           init(hex(84)) lfac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$()
                  on fieldnr% gosub L40170,         /* PIP START DATE   */~
                                    L40170,         /* Only in ATC Horzn*/~
                                    L40170,         /* RESET PIP STAUS  */~
                                    L40170,         /* PART NUMBER RANGE*/~
                                    L40170,         /* PART TYPE RANGE  */~
                                    L40170,         /* CATAGORY RANGE   */~
                                    L40170,         /* PART CLASS       */~
                                    L40185,         /* C / S Min Days   */~
                                    L40185,         /* S / S Min Days   */~
                                    L40185,         /* Surplus Min Days */~
                                    L40170,         /* BUYER/PLANNER    */~
                                    L40170          /* Scan only PIPIN  */

                     goto L40205

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40185:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40205:     accept                                                       ~
               at (01,02), "PIP MANAGEMENT SCANNING CRITERIA",           ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,29), fac(hex(ac)), title$(1)              , ch(25),~
               at (06,55), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (07,02), "Scan from Date:",                            ~
               at (07,29), fac(lfac$( 1)), scandate$(1)         , ch(10),~
               at (07,55), fac(lfac$( 1)), scandate$(2)         , ch(10),~
                                                                         ~
               at (08,02), "Only Problems in ATC Hrzn?",                 ~
               at (08,29), fac(lfac$(02)), atcflag$             , ch(01),~
                                                                         ~
               at (09,02), "Update PIP Status?",                         ~
               at (09,29), fac(lfac$(03)), pip_reset_flag$      , ch(01),~
                                                                         ~
               at (10,02), "Part Number:",                               ~
               at (10,29), fac(lfac$(04)), partr$(1)            , ch(25),~
               at (10,55), fac(lfac$(04)), partr$(2)            , ch(25),~
                                                                         ~
               at (11,02), "Part Type:",                                 ~
               at (11,29), fac(lfac$(05)), typer$(1)            , ch(03),~
               at (11,55), fac(lfac$(05)), typer$(2)            , ch(03),~
                                                                         ~
               at (12,02), "Part Category:",                             ~
               at (12,29), fac(lfac$(06)), cat$(1)              , ch(04),~
               at (12,55), fac(lfac$(06)), cat$(2)              , ch(04),~
                                                                         ~
               at (13,02), "Part Class:",                                ~
               at (13,29), fac(lfac$(07)), class$(1)            , ch(04),~
               at (13,55), fac(lfac$(07)), class$(2)            , ch(04),~
                                                                         ~
               at (14,02), "Critical Shortage Days:",                    ~
               at (14,29), fac(lfac$(08)), cmin_days$           , ch(03),~
                                                                         ~
               at (15,02), "S / S Intrusion Days:",                      ~
               at (15,29), fac(lfac$(09)), imin_days$           , ch(03),~
                                                                         ~
               at (16,02), "Surplus Days:",                              ~
               at (16,29), fac(lfac$(10)), smin_days$           , ch(03),~
                                                                         ~
               at (17,02), "Use Buyer/Scheduler Codes?",                 ~
               at (17,29), fac(lfac$(11)), buyer_flag$          , ch(01),~
                                                                         ~
               at (18,02), "Only Parts w/Procurements?",                 ~
               at (18,29), fac(lfac$(12)), scan_pipin$          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L40425
                  call "MANUAL" ("PIPSCAN")
                  goto L40205

L40425:        if keyhit% <> 15% then L40450
                  call "PRNTSCRN"
                  goto L40205

L40450:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * EDIT CRITERIA FOR SCANS (INITIAL ENTRY)                   *~
            *************************************************************

            deffn'110(fieldnr%)
                inpmessage$ = "Enter Part Number or Use PF Keys to Scan."
                init(hex(84)) lfac$()
                if fieldnr% = 0% then init(hex(86)) lfac$()
                  on fieldnr% gosub L40590,         /* FROM DATE        */~
                                    L40590,         /* PART NUMBER      */~
                                    L40590          /* SCAN MODE        */

                     goto L40625

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40590:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40625:     accept                                                       ~
               at (01,02),                                               ~
                  "SCAN PLANNED INVENTORY POSITION FOR PROBLEMS",        ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Scan from date",                                      ~
               at (06,30), fac(lfac$( 1)), scandate$(1)         , ch(10),~
               at (06,41), "to",                                         ~
               at (06,44), fac(lfac$( 1)), scandate$(2)         , ch(10),~
               at (07,02),                                               ~
                  "Part number",                                         ~
               at (07,30), fac(lfac$( 2)), part$                , ch(25),~
               at (08,41), fac(hex(8c)), partdescr$             , ch(32),~
               at (09,02),                                               ~
                  "Part Type for Scan (ALL, MFG, PUR)",                  ~
               at (09,37), fac(lfac$( 3)), scantype$            , ch( 3),~
               at (09,41), "Stocking UOM",                               ~
               at (09,62), fac(hex(84)), uom$                   , ch(04),~
               at (10,02),                                               ~
                  "Quantity on hand",                                    ~
               at (10,25), fac(hex(84)), onhand$                , ch(10),~
               at (11,02),                                               ~
                  "Safety stock level",                                  ~
               at (11,25), fac(hex(84)), safety$                , ch(10),~
               at (10,41),                                               ~
                  "Minimum order quantity",                              ~
               at (10,56), fac(hex(84)), moq$                   , ch(10),~
               at (11,41),                                               ~
                  "Pan size",                                            ~
               at (11,56), fac(hex(84)), pansize$               , ch(10),~
               at (12,02),                                               ~
                  "Part type",                                           ~
               at (12,32), fac(hex(84)), ptype$                 , ch( 3),~
               at (12,41), fac(hex(8c)), parttypedescr$         , ch(32),~
               at (13,02),                                               ~
                  "Lead Time",                                           ~
               at (13,32), fac(hex(84)), lt$                    , ch( 3),~
               at (13,41),                                               ~
                  "Part Class",                                          ~
               at (13,62), fac(hex(84)), pclass$                , ch( 4),~
                                                                         ~
               at (14, 02),                                              ~
                  "Status:",                                             ~
               at (15,05), fac(hex(84)), status$(1)             , ch(70),~
               at (16,05), fac(hex(84)), status$(2)             , ch(70),~
               at (17,05), fac(hex(84)), status$(3)             , ch(70),~
               at (18,05), fac(hex(84)), status$(4)             , ch(70),~
               at (19,05), fac(hex(84)), status$(5)             , ch(70),~
               at (20,05), fac(hex(84)), status$(6)             , ch(70),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02),                                               ~
                  "1)Start Over    8)Next Late Activity        11)Next S/~
        ~S Int    14)Review Fncts",                                       ~
               at (23,02),                                               ~
                  "                9/25)Next Part/Problem      12)Next Su~
        ~rplus    15)Print Screen",                                       ~
               at (24,02),                                               ~
                  "                10)Next Crit/Fcst Shortage  13)Instruc~
        ~tions    16)MANAGE PART ",                                       ~
                                                                         ~
               keys(hex(000108090a0b0c0d0e0f101920)),                    ~
               key (keyhit%)

               if keyhit% <> 13% then L40940
                  call "MANUAL" ("PIPSCAN ")
                  goto L40625

L40940:        if keyhit% <> 14% then L40960
                  gosub review_functions
                       if keyhit% = 1% or keyhit% = 32% then return
                  goto L40625

L40960:        if keyhit% <> 15% then L40980
                  call "PRNTSCRN"
                  goto L40625

L40980:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L41000: REM *************************************************************~
            *                                                           *~
            * SELECT MANAGE FUNCTION                                    *~
            *                                                           *~
            *************************************************************

        accept                                                           ~
               at (01,03),                                               ~
        "MANAGE PLANNED INVENTORY POSITION FOR PART",                    ~
               at (01,48), fac(hex(84)), part$                  , ch(25),~
               at (02,48), fac(hex(8c)), partdescr$             , ch(32),~
               at (03,02), fac(hex(94)), errormsg$              , ch(72),~
               at (04,03),                                               ~
        "Select one -",                                                  ~
               at (06,11),                                               ~
        "PF1) Start Over",                                               ~
               at (07,11),                                               ~
        "PF2) Cancel unreleased procurement advices",                    ~
               at (08,11),                                               ~
        "PF3) Cancel unreleased usage advices (higher level Procurements)~
        ~",                                                               ~
               at (09,11),                                               ~
        "PF4) Procure this part directly",                               ~
               at (10,11),                                               ~
        "PF5) Optimize planned inventory position for this part",        ~
               at (11,11),                                               ~
        "PF6) Combine unreleased work order advices",                    ~
               at (12,11),                                               ~
        "PF7) Force purchase advice for this part, auto expedite",       ~
               at (13,11),                                               ~
        "PF8) Slide or Reschedule work advice or job order",             ~
               at (14,11),                                               ~
        "PF9) Display Inventory Details ",                               ~
               at (15,10),                                               ~
        "PF10) Adjust Forecast Quantities",                              ~
                                                                         ~
               at (16,10),                                               ~
        "PF11) Release Job Advices",                                     ~
               at (18,10),                                               ~
        "PF12) Change planning system parameters (this session only)",   ~
               at (19,10),                                               ~
        "PF13) Instructions",                                            ~
               at (20,10),                                               ~
        "PF14) Review functions",                                        ~
               at (21,10),                                               ~
        "PF15) Print screen",                                            ~
               at (22,10),                                               ~
        "PF16) RETURN TO SCAN",                                          ~
               at (23,10),                                               ~
        "PF32) EXIT PROGRAM IMMEDIATELY",                                ~
                                                                         ~
               keys(hex(0102030405060708090a0b0c0d0e0f1020)),            ~
               key (keyhit%)

               if keyhit% <> 13% then L41560
                  call "MANUAL" ("PIPSCAN ")
                  goto L41000

L41560:        if keyhit% <> 14% then L41600
                  gosub review_functions
                       if keyhit% = 1% or keyhit% = 32% then return
                  goto L41000

L41600:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L41000

L43000: REM *************************************************************~
            *  CANCEL PIPOUTS                                           *~
            *                                                           *~
            *  VIEW, MARK AND CANCEL UNRELEASED PIPINS FOR A PART       *~
            *************************************************************

            if screen% <> 0% then L43055
               init (hex(8c)) str(wa$(),1,1000)
               if max% < 2% then L43055
               init (hex(80)) str(wa$(),1,max%-1%)

L43055:     if screen% = 0% then                                         ~
            inpmessage$ = "Mark Advices to Cancel with a NON-Blank " &   ~
                          "Character, then Press RETURN" else            ~
            inpmessage$ = "Press PF(8) to Cancel Advices, PF1 to Reset"
            str(line2$,1,60) = "Part No. " & part$ & ", " & partdescr$

        accept                                                           ~
               at (01,02),                                               ~
        "CANCEL UNRELEASED USAGE ADVICES (HIGHER LEVEL PROCUREMENTS)",   ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (06,02),                                               ~
                  "  Source/Use Tag       Date Out   Quantity   for Part ~
        ~Number                  ",                                       ~
                                                                         ~
               at (07,02), fac(wa$(l%+ 1%)), wl$(l%+ 1%)        , ch( 1),~
               at (08,02), fac(wa$(l%+ 2%)), wl$(l%+ 2%)        , ch( 1),~
               at (09,02), fac(wa$(l%+ 3%)), wl$(l%+ 3%)        , ch( 1),~
               at (10,02), fac(wa$(l%+ 4%)), wl$(l%+ 4%)        , ch( 1),~
               at (11,02), fac(wa$(l%+ 5%)), wl$(l%+ 5%)        , ch( 1),~
               at (12,02), fac(wa$(l%+ 6%)), wl$(l%+ 6%)        , ch( 1),~
               at (13,02), fac(wa$(l%+ 7%)), wl$(l%+ 7%)        , ch( 1),~
               at (14,02), fac(wa$(l%+ 8%)), wl$(l%+ 8%)        , ch( 1),~
               at (15,02), fac(wa$(l%+ 9%)), wl$(l%+ 9%)        , ch( 1),~
               at (16,02), fac(wa$(l%+10%)), wl$(l%+10%)        , ch( 1),~
               at (17,02), fac(wa$(l%+11%)), wl$(l%+11%)        , ch( 1),~
               at (18,02), fac(wa$(l%+12%)), wl$(l%+12%)        , ch( 1),~
                                                                         ~
               at (07,04), fac(hex(84)), intagnr$(l%+ 1%)       , ch(19),~
               at (08,04), fac(hex(84)), intagnr$(l%+ 2%)       , ch(19),~
               at (09,04), fac(hex(84)), intagnr$(l%+ 3%)       , ch(19),~
               at (10,04), fac(hex(84)), intagnr$(l%+ 4%)       , ch(19),~
               at (11,04), fac(hex(84)), intagnr$(l%+ 5%)       , ch(19),~
               at (12,04), fac(hex(84)), intagnr$(l%+ 6%)       , ch(19),~
               at (13,04), fac(hex(84)), intagnr$(l%+ 7%)       , ch(19),~
               at (14,04), fac(hex(84)), intagnr$(l%+ 8%)       , ch(19),~
               at (15,04), fac(hex(84)), intagnr$(l%+ 9%)       , ch(19),~
               at (16,04), fac(hex(84)), intagnr$(l%+10%)       , ch(19),~
               at (17,04), fac(hex(84)), intagnr$(l%+11%)       , ch(19),~
               at (18,04), fac(hex(84)), intagnr$(l%+12%)       , ch(19),~
                                                                         ~
               at (07,25), fac(hex(84)), outtagnr$(l%+ 1%)      , ch(19),~
               at (08,25), fac(hex(84)), outtagnr$(l%+ 2%)      , ch(19),~
               at (09,25), fac(hex(84)), outtagnr$(l%+ 3%)      , ch(19),~
               at (10,25), fac(hex(84)), outtagnr$(l%+ 4%)      , ch(19),~
               at (11,25), fac(hex(84)), outtagnr$(l%+ 5%)      , ch(19),~
               at (12,25), fac(hex(84)), outtagnr$(l%+ 6%)      , ch(19),~
               at (13,25), fac(hex(84)), outtagnr$(l%+ 7%)      , ch(19),~
               at (14,25), fac(hex(84)), outtagnr$(l%+ 8%)      , ch(19),~
               at (15,25), fac(hex(84)), outtagnr$(l%+ 9%)      , ch(19),~
               at (16,25), fac(hex(84)), outtagnr$(l%+10%)      , ch(19),~
               at (17,25), fac(hex(84)), outtagnr$(l%+11%)      , ch(19),~
               at (18,25), fac(hex(84)), outtagnr$(l%+12%)      , ch(19),~
                                                                         ~
               at (07,47), fac(hex(84)), part$    (l%+ 1%)      , ch(25),~
               at (08,47), fac(hex(84)), part$    (l%+ 2%)      , ch(25),~
               at (09,47), fac(hex(84)), part$    (l%+ 3%)      , ch(25),~
               at (10,47), fac(hex(84)), part$    (l%+ 4%)      , ch(25),~
               at (11,47), fac(hex(84)), part$    (l%+ 5%)      , ch(25),~
               at (12,47), fac(hex(84)), part$    (l%+ 6%)      , ch(25),~
               at (13,47), fac(hex(84)), part$    (l%+ 7%)      , ch(25),~
               at (14,47), fac(hex(84)), part$    (l%+ 8%)      , ch(25),~
               at (15,47), fac(hex(84)), part$    (l%+ 9%)      , ch(25),~
               at (16,47), fac(hex(84)), part$    (l%+10%)      , ch(25),~
               at (17,47), fac(hex(84)), part$    (l%+11%)      , ch(25),~
               at (18,47), fac(hex(84)), part$    (l%+12%)      , ch(25),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "1)Start Over            2)First 4)Prev 6)Down 13)Instr~
        ~uctions  15)Print Screen",                                       ~
               at (24,02),                                               ~
                  "Cursor & (9) for Demand 3)Last  5)Next 7)Up   14)Revie~
        ~w Fncts  16)RETURN     ",                                        ~
                                                                         ~
               keys(hex(000102030405060708090d0e0f1020)),                ~
               key (keyhit%)

               inpmessage$ = " "

               if keyhit% <> 13% then L43500
                  call "MANUAL" ("PIPSCAN ")
                  goto L43000

L43500:        if keyhit% <> 14% then L43520
                  gosub review_functions
                       if keyhit% = 1% or keyhit% = 32% then return
                  goto L43000

L43520:        if keyhit% <> 15% then L43540
                  call "PRNTSCRN"
                  goto L43000

L43540:        if keyhit% <> 9% then return
                  mat cursor% = zer : ret% = 0%
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  field% = cursor%(1) - 6%
                  call"GETDEM" (1%, intagnr$(field%+l%), #35, #1, #33,   ~
                                     demand$,  type$, ret%)
                  goto L43000

        REM *************************************************************~
            *                                                           *~
            * IMMEDIATE PROCUREMENT                                     *~
            *                                                           *~
            *************************************************************

            deffn'104(fieldnr%)
                  init(hex(84)) lfac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$()
                  str(line2$,1,60) = " "
                  on fieldnr% gosub L44200,         /* BY DATE          */~
                                    L44230,         /* QUANTITY         */~
                                    L44200,         /* USING BOM        */~
                                    L44200,         /* PRIORITY         */~
                                    L44200,         /* TYPE             */~
                                    L44200,         /* ADD TO FILE      */~
                                    L44200          /* Demand Code      */
                     goto L44270

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L44200:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L44230:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L44270:     accept                                                       ~
               at (01,02),                                               ~
                  "PLAN IMMEDIATE PROCUREMENT",                          ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Part number",                                         ~
               at (06,30), fac(hex(84)),   part$                , ch(25),~
               at (07,02),                                               ~
                  "Part descr",                                          ~
               at (07,30), fac(hex(8c)),   partdescr$           , ch(32),~
               at (08,02),                                               ~
                  "By date",                                             ~
               at (08,30), fac(lfac$( 1)), duedate$             , ch(08),~
               at (09,02),                                               ~
                  "Quantity",                                            ~
               at (09,30), fac(lfac$( 2)), psbquant$            , ch(10),~
               at (09,41), "at an Expected Total Cost of ",              ~
               at (09,70), fac(hex(84)),   expcost$             , ch(10),~
               at (10,02),                                               ~
                  "Using BOM",                                           ~
               at (10,30), fac(lfac$( 3)), psbbom$              , ch( 3),~
               at (11,02),                                               ~
                  "Priority",                                            ~
               at (11,30), fac(lfac$( 4)), psbprior$            , ch(01),~
               at (12,02),                                               ~
                  "Plan type",                                           ~
               at (12,30), fac(lfac$( 5)), psbtype$             , ch(01),~
               at (13,02),                                               ~
                  "Record in master file",                               ~
               at (13,30), fac(lfac$( 6)), add$                 , ch(03),~
               at (14,02), fac(hex(8c)), prompt1$               , ch(22),~
               at (14,30), fac(lfac$( 7)), psbcode$             , ch(16),~
               at (14,47), fac(lfac$( 7)), psbline$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,18),                                               ~
                  "(12)Change Planning Flags",                           ~
               at (23,45),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(8)Plan (When Screen is Complete)",                   ~
               at (24,45),                                               ~
                  "(14)Review Fncts",                                    ~
               at (24,65),                                               ~
                  "(16)RETURN",                                          ~
                                                                         ~
               keys(hex(0001080c0d0e0f1020)),                            ~
               key (keyhit%)

               if keyhit% <> 12% then L44840
                  gosub planflags
                  goto L44270

L44840:        if keyhit% <> 13% then L44880
                  call "MANUAL" ("PIPSCAN")
                  goto L44270

L44880:        if keyhit% <> 14% then L44920
                  gosub review_functions
                       if keyhit% = 1% or keyhit% = 32% then return
                  goto L44270

L44920:        if keyhit% <> 15% then L44960
                  call "PRNTSCRN"
                  goto L44270

L44960:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                                                           *~
            * OPTIMIZE PIP POSITION                                     *~
            *                                                           *~
            *************************************************************

            deffn'105(fieldnr%)
                  init(hex(84)) lfac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$()
                  str(line2$,1,60) = " "
                  on fieldnr% gosub L45180,         /* STARTDAY         */~
                                    L45210,         /* DAYS TO SUPPLY   */~
                                    L45180          /* PRIORITY         */~

                     goto L45250

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L45180:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L45210:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L45250:     accept                                                       ~
               at (01,02),                                               ~
                  "OPTIMIZE PLANNED INVENTORY POSITION",                 ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Part number",                                         ~
               at (06,30), fac(hex(84)),   part$                , ch(25),~
               at (07,02),                                               ~
                  "Part descr",                                          ~
               at (07,30), fac(hex(8c)),   partdescr$           , ch(32),~
               at (08,02),                                               ~
                  "Start on day",                                        ~
               at (08,30), fac(lfac$( 1)), startday$            , ch(08),~
               at (09,02),                                               ~
                  "Days to supply",                                      ~
               at (09,30), fac(lfac$( 2)), supply$              , ch( 3),~
               at (10,02),                                               ~
                  "Priority",                                            ~
               at (10,30), fac(lfac$( 3)), psbprior$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,24),                                               ~
                  "(13)Instructions",                                    ~
               at (23,45),                                               ~
                  "(14)Review Fncts",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                 "When Complete: (8)Plan one level  (24)Plan all levels",~
               at (24,65),                                               ~
                  "(16)RETURN",                                          ~
                                                                         ~
               keys(hex(0001080c0d0e0f101820)),                          ~
               key (keyhit%)

               if keyhit% <> 12% then L45690
                  gosub planflags
                  goto L45250

L45690:        if keyhit% <> 13% then L45730
                  call "MANUAL" ("PIPSCAN")
                  goto L45250

L45730:        if keyhit% <> 14% then L45770
                  gosub review_functions
                       if keyhit% = 1% or keyhit% = 32% then return
                  goto L45250

L45770:        if keyhit% <> 15% then L45810
                  call "PRNTSCRN"
                  goto L45250

L45810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *  DRIVER FOR MULTI-PURPOSE SCREEN                          *~
            *   CANCEL PIPINS, or COMBINE WORK ORDERS, or RESCHEDULE    *~
            *************************************************************

        deffn'108(screen%)
            init(hex(8c)) lfac$()
            init(" ") pfktext$()
            if screen% <> 0% or scr_select% = 3% then L46045
               init (hex(8c)) str(wa$(),1,1000)
               if max% < 2% then L46045
               init (hex(80)) str(wa$(),1,max% - 1%)
L46045:     str(line2$,1,60) = "PART No. " & part$ & ", " & partdescr$
        pfktext$(2) = "  Source Tag           Date In     Quantity  for D~
        ~emand           BOM/RTE Used"
            on scr_select% gosub L46100, L46200, L46300
                                 /* ----- SCR_SELECT% -------- */
                                 /* 1 = Cancel Pipins          */
                                 /* 2 = Combine Work Orders    */
                                 /* 3 = Reschedule Work Orders */

            gosub L48000          /* DISPLAY SCREEN */

            if scr_select% = 3% then gosub L46700
            return


        REM -- SET UP FOR CANCEL PIPINS SCREEN

L46100:     init (" ") prompt1$(), field1$, field2$, field3$, psbprior$

            if screen% = 0% then                                         ~
            inpmessage$ = "Mark Advices to Cancel with a NON-Blank " &   ~
                          "Character, then press RETURN" else            ~
            inpmessage$ = "Press PF(8) to Cancel Advices, PF1 to Reset"
            pfktext$(1) = "CANCEL UNRELEASED PROCUREMENT ADVICES"
            return

        REM -- SET UP FOR COMBINE WORK ORDERS SCREEN

L46200:     init (" ") inpmessage$
            on screen% + 1% goto L46210, L46225, L46240
            goto L46250
L46210:     inpmessage$ = "Mark work orders to cancel and combine with a"~
                        & " NON-Blank Character, Press RETURN"
            goto L46250
L46225:     inpmessage$ = "Indicate priority desired for combined order p~
        ~lan"
            goto L46250
L46240:     inpmessage$ = "Press PF(8) to cancel work orders and plan com~
        ~bined order"

L46250:     field1$ = combquant$
            field2$ = combdate$
            field3$ = " "
            prompt1$(1) = "Combine Quant"
            prompt1$(2) = "Needed By"
            prompt1$(3) = " "
            prompt1$(4) = "Priority"
            lfac$(1), lfac$(2) = hex(84)
            lfac$(4) = prfac$
            str(pfktext$(2),1,16) = "  Work Order Tag"
            pfktext$(1) = "COMBINE WORK ORDER ADVICES"
            return


        REM -- SET UP FOR RESCHEDULE WORK ORDERS SCREEN

L46300:        init (hex(84)) lfac$()
               if screen% = 0% then init(hex(86)) lfac$()
               on screen% goto  L46390,             /* SELECTION        */~
                                L46440,             /* LAST GOOD STEP   */~
                                L46440,             /* RESUME DATE      */~
                                L46440,             /* ADJ DUE DATE     */~
                                L46440              /* PRIORITY         */
               goto L46470

L46390:             REM SET FAC'S FOR SELECTION
                        init (hex(8c)) str(wa$(),1,1000)
                        if max% < 2% then L46470
                        init (hex(81)) str(wa$(),1,max% - 1%)
                        goto L46470
L46440:             REM SET FAC'S FOR UPPER CASE ONLY INPUT
                        lfac$(screen% - 1%) = hex(81)

L46470:     field1$ = rschdstep$
            field2$ = rschdstart$
            field3$ = rschddue$
            prompt1$(1) = "Last Step Done"
            prompt1$(2) = "Resume On"
            prompt1$(3) = "Adj Due Date"
            prompt1$(4) = "Priority"
            str(pfktext$(2),1,16) = "  Job/W.O. Tag  "
            pfktext$(1) = "RESCHEDULE WORK/JOB ORDERS"
            return

L46700: REM -- RESTORE VARIABLES FOR RESCHEDULE WORK ORDERS SCREEN

            rschdstep$ = str(field1$,1,7)
            rschdstart$ = field2$
            rschddue$ = field3$
            return

        REM *************************************************************~
            *                                                           *~
            *  FORCE BUY ORDER                                          *~
            *                                                           *~
            *************************************************************

            deffn'107(fieldnr%)
                  init(hex(84)) lfac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$()
                  str(line2$,1,60) = " "
                  on fieldnr% gosub L47170,         /* BY DATE          */~
                                    L47170,         /* START DATE       */~
                                    L47200          /* QUANTITY         */~

                     goto L47240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L47170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L47200:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L47240:     accept                                                       ~
               at (01,02),                                               ~
                  "FORCE BUY ADVICE FOR PART",                           ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Part number",                                         ~
               at (06,30), fac(hex(84)),   part$                , ch(25),~
               at (07,02),                                               ~
                  "Part descr",                                          ~
               at (07,30), fac(hex(8c)),   partdescr$           , ch(32),~
               at (08,02),                                               ~
                  "Due Date",                                            ~
               at (08,30), fac(lfac$( 1)), duedate$             , ch(08),~
               at (09,02),                                               ~
                  "Start Date",                                          ~
               at (09,30), fac(lfac$( 2)), startday$            , ch(08),~
               at (10,02),                                               ~
                  "Quantity",                                            ~
               at (10,30), fac(lfac$( 3)), psbquant$            , ch(10),~
               at (10,41), "at an Expected Total Cost of ",              ~
               at (10,70), fac(hex(84)),   expcost$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,45),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(8)Record advice (when screen is complete)",          ~
               at (24,45),                                               ~
                  "(14)Review Fncts",                                    ~
               at (24,65),                                               ~
                  "(16)RETURN",                                          ~
                                                                         ~
               keys(hex(0001080d0e0f1020)),                              ~
               key (keyhit%)

               if keyhit% <> 12% then L47670
                  gosub planflags
                  goto L47240

L47670:        if keyhit% <> 13% then L47710
                  call "MANUAL" ("PIPSCAN")
                  goto L47240

L47710:        if keyhit% <> 14% then L47750
                  gosub review_functions
                       if keyhit% = 1% or keyhit% = 32% then return
                  goto L47240

L47750:        if keyhit% <> 15% then L47790
                  call "PRNTSCRN"
                  goto L47240

L47790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L48000: REM *************************************************************~
            *  MultiPurpose Screen;  CANCEL PIPINS, COMBINE WORK ORDERS *~
            *                        & RESCHEDULE WORK ORDERS           *~
            *  VIEW, MARK AND ANSWER SOME PERTINENT QUESTIONS           *~
            *************************************************************


L48070: accept                                                           ~
               at (01,02), fac(hex(8c)), pfktext$(1)            , ch(40),~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (06,02), fac(hex(8c)), pfktext$(2)            , ch(79),~
                                                                         ~
               at (07,02), fac(wa$(l%+ 1%)), wl$(l%+ 1%)        , ch( 1),~
               at (08,02), fac(wa$(l%+ 2%)), wl$(l%+ 2%)        , ch( 1),~
               at (09,02), fac(wa$(l%+ 3%)), wl$(l%+ 3%)        , ch( 1),~
               at (10,02), fac(wa$(l%+ 4%)), wl$(l%+ 4%)        , ch( 1),~
               at (11,02), fac(wa$(l%+ 5%)), wl$(l%+ 5%)        , ch( 1),~
               at (12,02), fac(wa$(l%+ 6%)), wl$(l%+ 6%)        , ch( 1),~
               at (13,02), fac(wa$(l%+ 7%)), wl$(l%+ 7%)        , ch( 1),~
               at (14,02), fac(wa$(l%+ 8%)), wl$(l%+ 8%)        , ch( 1),~
               at (15,02), fac(wa$(l%+ 9%)), wl$(l%+ 9%)        , ch( 1),~
               at (16,02), fac(wa$(l%+10%)), wl$(l%+10%)        , ch( 1),~
               at (17,02), fac(wa$(l%+11%)), wl$(l%+11%)        , ch( 1),~
               at (18,02), fac(wa$(l%+12%)), wl$(l%+12%)        , ch( 1),~
                                                                         ~
               at (07,04), fac(hex(84)), intagnr$(l%+ 1%)       , ch(19),~
               at (08,04), fac(hex(84)), intagnr$(l%+ 2%)       , ch(19),~
               at (09,04), fac(hex(84)), intagnr$(l%+ 3%)       , ch(19),~
               at (10,04), fac(hex(84)), intagnr$(l%+ 4%)       , ch(19),~
               at (11,04), fac(hex(84)), intagnr$(l%+ 5%)       , ch(19),~
               at (12,04), fac(hex(84)), intagnr$(l%+ 6%)       , ch(19),~
               at (13,04), fac(hex(84)), intagnr$(l%+ 7%)       , ch(19),~
               at (14,04), fac(hex(84)), intagnr$(l%+ 8%)       , ch(19),~
               at (15,04), fac(hex(84)), intagnr$(l%+ 9%)       , ch(19),~
               at (16,04), fac(hex(84)), intagnr$(l%+10%)       , ch(19),~
               at (17,04), fac(hex(84)), intagnr$(l%+11%)       , ch(19),~
               at (18,04), fac(hex(84)), intagnr$(l%+12%)       , ch(19),~
                                                                         ~
               at (07,25), fac(hex(84)), str(part$(l%+ 1%),1, 8), ch( 8),~
               at (08,25), fac(hex(84)), str(part$(l%+ 2%),1, 8), ch( 8),~
               at (09,25), fac(hex(84)), str(part$(l%+ 3%),1, 8), ch( 8),~
               at (10,25), fac(hex(84)), str(part$(l%+ 4%),1, 8), ch( 8),~
               at (11,25), fac(hex(84)), str(part$(l%+ 5%),1, 8), ch( 8),~
               at (12,25), fac(hex(84)), str(part$(l%+ 6%),1, 8), ch( 8),~
               at (13,25), fac(hex(84)), str(part$(l%+ 7%),1, 8), ch( 8),~
               at (14,25), fac(hex(84)), str(part$(l%+ 8%),1, 8), ch( 8),~
               at (15,25), fac(hex(84)), str(part$(l%+ 9%),1, 8), ch( 8),~
               at (16,25), fac(hex(84)), str(part$(l%+10%),1, 8), ch( 8),~
               at (17,25), fac(hex(84)), str(part$(l%+11%),1, 8), ch( 8),~
               at (18,25), fac(hex(84)), str(part$(l%+12%),1, 8), ch( 8),~
                                                                         ~
               at (07,35), fac(hex(84)), str(part$(l%+ 1%),9,10), ch(10),~
               at (08,35), fac(hex(84)), str(part$(l%+ 2%),9,10), ch(10),~
               at (09,35), fac(hex(84)), str(part$(l%+ 3%),9,10), ch(10),~
               at (10,35), fac(hex(84)), str(part$(l%+ 4%),9,10), ch(10),~
               at (11,35), fac(hex(84)), str(part$(l%+ 5%),9,10), ch(10),~
               at (12,35), fac(hex(84)), str(part$(l%+ 6%),9,10), ch(10),~
               at (13,35), fac(hex(84)), str(part$(l%+ 7%),9,10), ch(10),~
               at (14,35), fac(hex(84)), str(part$(l%+ 8%),9,10), ch(10),~
               at (15,35), fac(hex(84)), str(part$(l%+ 9%),9,10), ch(10),~
               at (16,35), fac(hex(84)), str(part$(l%+10%),9,10), ch(10),~
               at (17,35), fac(hex(84)), str(part$(l%+11%),9,10), ch(10),~
               at (18,35), fac(hex(84)), str(part$(l%+12%),9,10), ch(10),~
                                                                         ~
               at (07,47), fac(hex(84)), outtagnr$(l%+ 1%)      , ch(19),~
               at (08,47), fac(hex(84)), outtagnr$(l%+ 2%)      , ch(19),~
               at (09,47), fac(hex(84)), outtagnr$(l%+ 3%)      , ch(19),~
               at (10,47), fac(hex(84)), outtagnr$(l%+ 4%)      , ch(19),~
               at (11,47), fac(hex(84)), outtagnr$(l%+ 5%)      , ch(19),~
               at (12,47), fac(hex(84)), outtagnr$(l%+ 6%)      , ch(19),~
               at (13,47), fac(hex(84)), outtagnr$(l%+ 7%)      , ch(19),~
               at (14,47), fac(hex(84)), outtagnr$(l%+ 8%)      , ch(19),~
               at (15,47), fac(hex(84)), outtagnr$(l%+ 9%)      , ch(19),~
               at (16,47), fac(hex(84)), outtagnr$(l%+10%)      , ch(19),~
               at (17,47), fac(hex(84)), outtagnr$(l%+11%)      , ch(19),~
               at (18,47), fac(hex(84)), outtagnr$(l%+12%)      , ch(19),~
                                                                         ~
               at (07,68), fac(hex(84)), str(part$(l%+ 1%),19,7), ch( 7),~
               at (08,68), fac(hex(84)), str(part$(l%+ 2%),19,7), ch( 7),~
               at (09,68), fac(hex(84)), str(part$(l%+ 3%),19,7), ch( 7),~
               at (10,68), fac(hex(84)), str(part$(l%+ 4%),19,7), ch( 7),~
               at (11,68), fac(hex(84)), str(part$(l%+ 5%),19,7), ch( 7),~
               at (12,68), fac(hex(84)), str(part$(l%+ 6%),19,7), ch( 7),~
               at (13,68), fac(hex(84)), str(part$(l%+ 7%),19,7), ch( 7),~
               at (14,68), fac(hex(84)), str(part$(l%+ 8%),19,7), ch( 7),~
               at (15,68), fac(hex(84)), str(part$(l%+ 9%),19,7), ch( 7),~
               at (16,68), fac(hex(84)), str(part$(l%+10%),19,7), ch( 7),~
               at (17,68), fac(hex(84)), str(part$(l%+11%),19,7), ch( 7),~
               at (18,68), fac(hex(84)), str(part$(l%+12%),19,7), ch( 7),~
                                                                         ~
               at (19,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (20,02), fac(hex(8c)), prompt1$(1)            , ch(14),~
               at (20,17), fac(lfac$(1)), field1$               , ch(10),~
               at (20,28), fac(hex(8c)), str(prompt1$(2),1,9)   , ch(09),~
               at (20,38), fac(lfac$(2)), field2$               , ch(08),~
               at (20,47), fac(hex(8c)), str(prompt1$(3),1,12)  , ch(12),~
               at (20,60), fac(lfac$(3)), field3$               , ch(08),~
               at (20,69), fac(hex(8c)), str(prompt1$(4),1,8)   , ch(08),~
               at (20,78), fac(lfac$(4)), psbprior$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "1)Start Over            2)First 4)Prev 6)Down 13)Instr~
        ~uctions  15)Print Screen",                                       ~
               at (24,02),                                               ~
                  "Cursor & (9) for Demand 3)Last  5)Next 7)Up   14)Revie~
        ~w Fncts  16)RETURN      ",                                       ~
                                                                         ~
               keys(hex(000102030405060708090c0d0e0f101820)),            ~
               key (keyhit%)

               if keyhit% <> 12% then L49180
                  gosub planflags
                  goto L48070

L49180:        if keyhit% <> 13% then L49220
                  call "MANUAL" ("PIPSCAN ")
                  goto L48070

L49220:        if keyhit% <> 14% then L49260
                  gosub review_functions
                       if keyhit% = 1% or keyhit% = 32% then return
                  goto L48070

L49260:        if keyhit% <> 15% then L49300
                  call "PRNTSCRN"
                  goto L48070

L49300:           mat cursor% = zer : ret% = 0%
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

               if keyhit% <> 9% then return
                  field% = cursor%(1) - 6%
                  call"GETDEM" (1%, intagnr$(field%+l%), #35, #1, #33,   ~
                                     demand$,  type$, ret%)
                  goto L48070

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * INITIAL ENTRY AND SELECTION                               *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50480,         /* FROM DATE        */~
                                    L50460,         /* Only in ATC Horzn*/~
                                    L50430,         /* RESET PIP=TODAY? */~
                                    L50090,         /* PART RANGE       */~
                                    L50300,         /* PART TYPE RANGE  */~
                                    L50390,         /* CATEGORY RANGE   */~
                                    L50620,         /* PART CLASS       */~
                                    L50125,         /* C / S Min Days   */~
                                    L50165,         /* S / S Min Days   */~
                                    L50235,         /* Surplus Min Days */~
                                    L50670,         /* Buyer/Planner Cds*/~
                                    L50720          /* Scan Pipin Only  */
                     return
L50090:     REM **********  TEST DATA FOR PART NUMBER RANGE  ***********
                call "TESTRNGE"   (partr$(1%), partr$(2%), partr$(3%),   ~
                                   partr$(4%), errormsg$)
                return

L50125: REM TEST MIN DAYS A Critical Shortage MUST LAST TO BE A PROBLEM
            if cmin_days$ = " " then cmin_days$ = "1"
            convert cmin_days$ to cmin_days%, data goto L50145
            if cmin_days% + today% > 491% or cmin_days% < 1% then L50145
            return
L50145:     errormsg$="Not Valid number of Days"
            return

L50165: REM TEST MIN DAYS A S / Stock Intrusion MUST LAST TO BE A PROBLEM
            if imin_days$ = " " then imin_days$ = "1"
            convert imin_days$ to imin_days%, data goto L50145
            if imin_days% + today% > 491% or imin_days% < 1% then L50145
            return

L50235: REM TEST MIN DAYS A Surplus MUST LAST TO BE A PROBLEM
            if smin_days$ = " " then smin_days$ = "1"
            convert smin_days$ to smin_days%, data goto L50145
            if smin_days% + today% > 491% or smin_days% < 1% then L50145
            return

L50300: REM TEST PART TYPE RANGE
            typelow% = 0% : typehigh% = 999%
            if typer$(1) = "ALL" then L50345
            if typer$(2) = " " then typer$(2) = typer$(1)
            call "NUMTEST" (typer$(1), 0, 999, errormsg$, 0, temp)
            if errormsg$ <> " " then return
            typelow% = temp
            call "NUMTEST" (typer$(2), temp, 999, errormsg$, 0, temp2)
            if errormsg$ <> " " then return
            typehigh% = temp2
L50345:     convert typelow% to typer$(1), pic(000)
            convert typehigh% to typer$(2), pic(000)
            return

L50390: REM TEST PART CATEGORY RANGE
         call "TESTRNGE" (cat$(1%), cat$(2%), temp2$, temp2$, errormsg$)
           return

L50430: REM TEST PIP RESET FLAG
           if pip_reset_flag$ <> "Y" and pip_reset_flag$ <> "N"  then    ~
                     errormsg$ = "Must enter Y or N !"
           return

L50460: REM TEST ATC HORIZON FLAG
           if atcflag$ <> "Y" and atcflag$ <> "N"  then                  ~
                     errormsg$ = "Must enter Y or N !"
           return

L50480: REM TEST FROM DATE
            if scandate$(2%) = " " or scandate$(2%) = blankdate$ ~
                 then scandate$(2%) = scandate$(1%)
            for i% = 1% to 2%
               call "DATEOKC" (scandate$(i%), err%, errormsg$)
                   if errormsg$ <> " "  then return
               tmpdate$ = scandate$(i%)
               call "DATUFMTC" (tmpdate$)
               call "DATE" addr("G-", pldate$, tmpdate$, scan%(i%), err%)
                   if err% = 0% then L50530
L50520:            errormsg$="Date not valid for planning"
                   return
L50530:        scan%(i%) = scan%(i%) + 1%
               if scan%(i%) < 1% then L50520
               if scan%(i%) > 490% then L50520
            next i%
            if scan%(1) > scan%(2) then                                  ~
                     errormsg$ = "Date Range is Not Valid"
         return

L50620: REM TEST PART CLASS RANGE
           call "TESTRNGE" (class$(1%), class$(2%), temp2$, temp2$,      ~
                            errormsg$)
           return

L50670: REM TEST BUYER / PLANNER CODES RESTRICTION      BUYER_FLAG$
           if buyer_flag$ <> "Y" and buyer_flag$ <> "N"  then            ~
                     errormsg$ = "Must enter Y or N !"
           return

L50720: REM TEST SCAN PIPIN ONLY                        SCAN_PIPIN$
           if scan_pipin$ <> "Y" and scan_pipin$ <> "N"  then            ~
                     errormsg$ = "Must enter Y or N !"
           return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * INITIAL ENTRY AND SELECTION                               *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  gosub set_scantype
                  on fieldnr% gosub L51280,         /* FROM DATE        */~
                                    L51130          /* PART NUMBER      */~

                     return
L51130:     REM TEST DATA FOR PART NUMBER
                gosub L29000              /* INIT VARIABLES */
                if workfl_flag% = 1% then L51171
                call "PLOWCODE" (#4, part$, partdescr$, 0%, 0.32, f1%(4))
                   if f1%(4) <> 0% then L51180
                      errormsg$="Part not on file"
                      return
L51171:       call "PLOWCODE" (#64, part$, partdescr$, 0%, 0.32, f1%(64))
                   if f1%(64) <> 0% then L51180
                      errormsg$="Part not within the Selected Range"
                      return
L51180:         call "READ100" (#2, part$, f1%(2))
                   if f1%(2) <> 0% then L51220
                      errormsg$="Part not found in PIP"
                      return
L51220:         gosub set_status
                   return clear all
                   goto show_condition   /* Display Status */


L51280: REM TEST FROM DATE
            gosub L50480
               if errormsg$ <> " " then return
            call "READ100" (#2, part$, f1%(2))
               if f1%(2) = 0% then return
               gosub set_status
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L54150,         /* BY DATE          */~
                                    L54260,         /* QUANTITY         */~
                                    L54340,         /* BOM              */~
                                    L54430,         /* PRIORITY         */~
                                    L54490,         /* PLAN TYPE        */~
                                    L54550,         /* ADD TO FILE      */~
                                    L54630          /* DEMAND CODE      */
                     return

L54150:     REM TEST DATA FOR BY DATE
            call "DATEOK" (duedate$, err%, errormsg$)
            if errormsg$ <> " " then return
            tmpdate$=duedate$ : call"DATUNFMT" (tmpdate$)
            call "DATE" addr("G-", pldate$, tmpdate$, cd%, err%)
            if err% <> 0% then L54240
            cd% = cd% + 1%
            if cd% < 1% or cd% > 490% then L54240
            if cd% >= today% then return
L54240:         errormsg$ = "Date not within planning period"
                return
L54260:     REM TEST DATA FOR QUANTITY
                convert psbquant$ to psbquant, data goto L54320
                psbquant = round(psbquant,2)
                call "CONVERT" (psbquant, 2.2, psbquant$)
                if psbquant < .0001 then L54320
                call "STCCOSTS" (part$, " ", #60, 1%, tot_cost)
                expcost = psbquant * tot_cost
                call "CONVERT" (expcost, -2.2, expcost$)
                return
L54320:            errormsg$ = "Invalid numeric entry"
                   return
L54340:     REM TEST DATA FOR BOM
                if psbbom$ = " " then return
                init (hex(00)) plowkey$
                str(plowkey$,1,28) = str(part$,1,25) & str(psbbom$,1,3)
                call "PLOWNEXT" (#15, plowkey$, 28%, f1%(15))
                  if f1%(15) <> 0% then return
                errormsg$ = "BOM Not on file"
                return

L54430:     REM TEST DATA FOR PRIORITY
            if psbprior$ <"@" or psbprior$ > "Z" then L54460
                return
L54460:         errormsg$ = "Invalid priority, please re-enter"
                return

L54490:     REM TEST DATA FOR PLAN TYPE
                if psbtype$ = "8" then return
                if psbtype$ = "7" then return
                   errormsg$ = "Type 7 or 8 Only"
                return

L54550:     REM TEST ADD TO FILE
                if add$ = "YES" then L54600
                if add$ = "NO" then L54610
                    errormsg$ = "YES or NO, Please"
                    return
L54600:         prompt1$ = "Demand Code & Line" : return
L54610:         prompt1$ = " " : return

L54630:     REM TEST DEMAND CODE & LINE
                if psbcode$ <> " " then L54670
                     psbline$ = " "
                     return
L54670:         if psbline$ = " " then L54730
                convert psbline$ to temp%, data goto L54730
                call "STRING" addr("RJ", psbline$, 3%)
                init(hex(00)) plowkey$
                str(plowkey$,1,19) = str(psbcode$,,16) & str(psbline$,,3)
                call "REDALT0" (#1, plowkey$, 1%, f1%(1))
                     if f1%(1) = 0% then return
                errormsg$ = "This Demand Code and Line number is already ~
        ~ON FILE."
                return
L54730:         errormsg$ = "The Demand Line Number MUST be Numeric."
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * OPTIMIZE                                                  *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'155(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L55130,         /* STARTDAY         */~
                                    L55240,         /* SUPPLY           */~
                                    L55300          /* PRIORITY         */~

                     return
L55130:     REM TEST DATA FOR BY DATE
            call "DATEOK" (startday$, err%, errormsg$)
            if errormsg$ <> " " then return
            tmpdate$=startday$ : call"DATUNFMT" (tmpdate$)
            call "DATE" addr("G-", pldate$, tmpdate$, startday%, err%)
            if err% <> 0% then L55220
            startday% = startday% + 1%
            if startday% < 1% or startday% > 490% then L55220
            if startday% >= today% then return
L55220:         errormsg$ = "Date not within planning period"
                return
L55240:     REM TEST DATA FOR SUPPLY
                convert supply$ to supply%, data goto L55280
                convert supply% to supply$, pic(###)
                if supply% > 0% then return
L55280:            errormsg$ = "Invalid numeric entry"
                   return
L55300:     REM TEST DATA FOR PRIORITY
            if psbprior$ <"@" or psbprior$ > "Z" then L55330
                return
L55330:         errormsg$ = "Invalid priority, please re-enter"
                return

            REM TEST ADD TO FILE
                if add$ = "YES" then return
                if add$ = "NO" then return
                    errormsg$ = "YES or NO, Please"
                    return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * FORCE BO                                                  *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'157(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L57120,         /* BY DATE          */~
                                    L57320,         /* START DATE       */~
                                    L57230          /* QUANTITY         */~

                     return
L57120:     REM TEST DATA FOR BY DATE
            call "DATEOK" (duedate$, err%, errormsg$)
            if errormsg$ <> " " then return
            tmpdate$=duedate$ : call"DATUNFMT" (tmpdate$)
            call "DATE" addr("G-", pldate$, tmpdate$, cd%, err%)
            if err% <> 0% then L57210
            cd% = cd% + 1%
            if cd% < 1% or cd% > 490% then L57210
            return
L57210:         errormsg$ = "Date not within planning period"
                return
L57230:     REM TEST DATA FOR QUANTITY
                convert psbquant$ to psbquant, data goto L57290
                psbquant = round(psbquant,2)
                call "CONVERT" (psbquant, 2.2, psbquant$)
                if psbquant < .0001 then L57290
                call "STCCOSTS" (part$, " ", #60, 1%, tot_cost)
                expcost = psbquant * tot_cost
                call "CONVERT" (expcost, -2.2, expcost$)
                return
L57290:            errormsg$ = "Invalid numeric entry"
                   return

L57320:     REM TEST DATA FOR START DATE
            call "DATEOK" (startday$, err%, errormsg$)
            if errormsg$ <> " " then return
            tmpdate$=startday$ : call"DATUNFMT" (tmpdate$)
            call "DATE" addr("G-", pldate$, tmpdate$, st%, err%)
            if err% <> 0% then L57210
            st% = st% + 1%
            if st% < 1% or st% > 490% then L57210
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * RESCHEDULE                                                *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'158(screen%)
            init (" ") errormsg$
            on screen% goto L58075,       /* SELECTION                  */~
                            L58500,       /* LAST COMPLETE STEP         */~
                            L59030,       /* RESUME DATE                */~
                            L59170,       /* ADJ. DUE DATE              */~
                            L59310        /* PRIORITY                   */
            return

L58075: REM TEST DATA FOR SELECTION

                hits% = 0%:init(hex(84)) str(wa$(),1,1000)
                for i% = 1% to max%
                     if intagnr$(i%)<>" " then L58110
                         wl$(i%) = " "
                         goto L58140
L58110:              if wl$(i%) = " " then L58140
                        if wl$(i%) = "S" then slide% = 1%
                        hits% = i%
                        wa$(i%) = hex(94)
                        l% = max(0%, min(max%-12%, hits%-1%))
                        init (" ") str(wl$(),hits%+1%)
                        goto L58160
L58140:         next i%
                errormsg$ = "Choose job or work order to reschedule"
                return

L58160:     rschdtag$ = intagnr$(hits%)
            rschdbom$ = str(part$(hits%),19,3)
            convert str(part$(hits%),9,10) to rschdquant
            reviewpart$ = part$(hits%):newtag$=outtagnr$(hits%)

            if str(rschdtag$,,11) = "JOB ORDER: " and wl$(hits%) <> " "  ~
                 then gosub test_and_set_jobhold
                 if errormsg$ <> " " then return

            init (hex(00)) plowkey$
            str(plowkey$,1,31) = str(part$,1,25) & str(part$(hits%),19,3)~
                                 & "  0"
            call "READ100" (#15, plowkey$, f1%(15))
                if f1%(15) <> 0% then L58220
                   errormsg$ = "Indicated BOM not on file"
                   return
L58220:     get #15, using L58225, temp2$
L58225:         FMT POS(87), CH(3)
            if str(temp2$,1,3) = str(part$(hits%),23,3) then L58250
               errormsg$ = "BOM is currently linked to different route"
               return

L58250:     if str(part$(hits%),23,3) <> " " then L58270
            rschdmax% = 1%
            goto L58325

L58270:     rschdmax% = 0% : init(" ") rtestep$()
            print at(19,02,79), hex(84) & "Loading Route..."
            f% = 9%  /* file channel, saves a second call to plntrvlr */
            if str(rschdtag$,,11) <> "JOB ORDER: " then f% = 33%
            call "PLNTRVLR" (str(rschdtag$,,19), #7, #23, #8, #15, #24,  ~
                             #60, #f%, " ", " ", rtestep$(), rschdmax%,  ~
                             travel$(), cntr2%)

L58325:     if rschdmax% > 0% then L58345
               errormsg$ = "Route indicated is not on file"
               return

L58345:     mat ed% = zer
            if f2%(10) = 0% then call "DELETE" (#10, hex(00), 0%)
            if cntr2% < 1% then L58465
            if cntr2% < 2% or f2%(10) = 0% then L58370
                call "WORKOPEN" (#10, "IO", 100%, f2%(10))
L58370:     for temp% = 1% to cntr2%
                get travel$(temp%), using L58380,temp2$,step%,tmpdate$
L58380:         FMT CH(7), BI(2), XX(34), CH(6)
                call "PIPINDEX" (#60, tmpdate$, index%, 0%)
                if step% < 1% then L58455
                ed%(step%) = max(ed%(step%), index%)
                if cntr2% < 2% then L58455
                   plowkey$ = str(rtestep$(step%),98,30)
                   tmpdate$ = str(rtestep$(step%),94,4)
                   if tmpdate$ <> " " then call "PUTPAREN" (tmpdate$)
                   plowkey$ = plowkey$ & " " & tmpdate$
                   write #10, using L58450, temp2$, step%, ")",           ~
                              plowkey$, step%, eod goto L58455
L58450:            FMT CH(9), PIC(###), CH(3), CH(37), BI(2)
L58455:     next temp%

L58465:     tmpdate$ = str(part$(hits%),,8)
            call "DATUNFMT" (tmpdate$)
            call "DATE" addr("G-", pldate$, tmpdate$, rschdend%, err%)
                if err% <> 0% then rschdend% = 0%
            rschdend% = max(rschdend%+1%, ed%(rschdmax%), today%)
            return

L58500: REM TEST DATA FOR LAST COMPLETE STEP
            rschdstep% = 0%
            if rschdstep$ = " " then L58540
            call "GETCODE" (#10, rschdstep$, " ", 0%, 1.43, f1%(10))
                if f1%(10) = 0% then L59000
            get #10, using L58530, rschdstep%
L58530:     FMT POS(53), BI(2)
            if rschdstep% > 0% then L58550
L58540:         rschdday% = 0%
                goto L58555
L58550:     rschdday% = ed%(rschdstep%)
L58555:     rschdstart% = (max(rschdday%,today%) - 1%)
            return
L59000:         errormsg$ = "Invalid entry for last complete step"
                return

L59030: REM TEST DATA FOR RESUME DATE
            call "DATEOK" (rschdstart$, err%, errormsg$)
                if errormsg$ <> " " then return
            tmpdate$ = rschdstart$
            call "DATUNFMT" (tmpdate$)
            call "DATE" addr("G-", pldate$, tmpdate$, rschdtoday%, err%)
                if err% <> 0% then L59140
                rschdtoday% = rschdtoday% + 1%
                if rschdtoday% < max(rschdday%,today%) then L59140
                if rschdtoday% > 490% then L59140
                   return
L59140:               errormsg$ = "Invalid entry for restart date"
                      return

L59170: REM TEST DATA FOR ADJ DUE DATE
            call "DATEOK" (rschddue$, err%, errormsg$)
                if errormsg$ <> " " then return
            tmpdate$ = rschddue$
            call "DATUNFMT" (tmpdate$)
            call "DATE" addr("G-", pldate$, tmpdate$, rschdcd%, err%)
                if err% <> 0% then L59280
                rschdcd% = rschdcd% + 1%
                if rschdcd% < max(rschdtoday%,today%) then L59280
                if rschdcd% > 490% then L59280
                   return
L59280:               errormsg$ = "Invalid entry for adjusted due date"
                      return

L59310: REM TEST DATA FOR PRIORITY
            if psbprior$ < "@" then L59340
            if psbprior$ <= "Z" then return
L59340:        errormsg$ = "'A' - 'Z' Priority please"
               return

        test_and_set_jobhold
            in_use% = 2%
            call "JBINUSE"(str(rschdtag$,12,8), in_use%)
            if in_use% = 1% then errormsg$ = "Job " & str(rschdtag$,12,8)~
                & " in use."
            return

        release_jobhold
            if str(rschdtag$,,11) <> "JOB ORDER: " then return
            in_use% = 1%
            call "JBINUSE"(str(rschdtag$,12,8), in_use%)
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

        REM *************************************************************~
            *                                                           *~
            *     COMMON CALL TO CLRRSCHD                               *~
            *                                                           *~
            *************************************************************

        call_clrrschd

            call "CLRRSCHD" (                                            ~
                     clrtag$,            /* TAG NO. OF PIPIN TO CLEAR  */~
                     today%,             /* FOR PIPFLAGS               */~
                     clrday%,            /* CLEAR PIPOUTS >  THIS DAY  */~
                     clrstep%,           /* CLEAR WC STEPS > THIS STEP */~
                     clrclear%,          /* CLEAR WORKFILE? (ALSO RET.)*/~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #41,                /* SFCUM2                     */~
                     #35,                /* PIPCROSS                   */~
                     #36,                /* JBPIPXRF                   */~
                     #61,                /* WORKFILE TO STORE CLEARED  */~
                     ret%)               /* RECORDS FOR RESTORE        */

            return

        REM *************************************************************~
            *                                                           *~
            *     COMMON CALL TO RSTRSCHD                               *~
            *                                                           *~
            *************************************************************

        call_rstrschd

            call "RSTRSCHD" (                                            ~
                     clrtag$,            /* TAG NO. OF PIPIN TO CLEAR  */~
                     today%,             /* FOR PIPFLAGS               */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #41,                /* SFCUM2                     */~
                     #35,                /* PIPCROSS                   */~
                     #36,                /* JBPIPXRF                   */~
                     #61)                /* WORKFILE TO STORE CLEARED  */~

            return

        REM *************************************************************~
            *                                                           *~
            *     COMMON CALL TO JBRETAG                                *~
            *                                                           *~
            *************************************************************

        call_jbretag

            call "JBRETAG" (                                             ~
                     oldtag$,            /* TAG CURRENTLY ON FILE      */~
                     newtag$,            /* TAG IT WILL BECOME         */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #35,                /* PIPCROSS                   */~
                     #36)                /* JBPIPXRF                   */

            return

        REM *************************************************************~
            *                                                           *~
            *     CHANGE PLANNING SYSTEM FLAGS                          *~
            *                                                           *~
            *************************************************************

        planflags
              err% = today%
              call "PLNFLSUB" (planflags$(), err%)
              unplanopt$ = str(planflags$(), 4, 1)
              unplanrpt$ = str(planflags$(),18, 1)
              str(planflagshold$(),1,480) = str(planflags$(),1,480)
              return

        REM *************************************************************~
            *                                                           *~
            *     LINK TO PLANNING REVIEW FUNCTIONS                     *~
            *                                                           *~
            *************************************************************

        review_functions

        call "PIPRVWSB" (                                                ~
                       part$,            /* PART TO BE REVIEWED        */~
                       keyhit%,          /* KEYHIT OUT                 */~
                       #1,               /* DEMMASTR                   */~
                       #2,               /* PIPMASTR                   */~
                       #4,               /* HNYMASTR                   */~
                       #5,               /* HNYDETAL                   */~
                       #6,               /* RTEALTRS                   */~
                       #7,               /* RTEMASTR                   */~
                       #9,               /* JBMASTR2                   */~
                       #11,              /* WCMASTR                    */~
                       #12,              /* CALMASTR                   */~
                       #15,              /* BOMMASTR                   */~
                       #23,              /* WCOUT                      */~
                       #24,              /* ENGMASTR                   */~
                       #33,              /* PIPIN                      */~
                       #34,              /* PIPOUT                     */~
                       #35,              /* PIPCROSS                   */~
                       #40,              /* SFMASTR2                   */~
                       #41,              /* SFCUM2                     */~
                       #42,              /* PORLSE                     */~
                       #43,              /* VBKMASTR                   */~
                       #60,              /* SYSFILE2                   */~
                       #16)              /* HNYALTRS                   */

            return

        REM *************************************************************~
            *                                                           *~
            *     LINK TO PIP OPTIMIZATION FUNCTION                     *~
            *                                                           *~
            *************************************************************

        call_pipoptmz

        call "PIPOPTMZ" (                                                ~
                       part$,            /* PART TO BE OPTIMIZED       */~
                       startday%,        /* SUBSCRIPT FOR START DATE   */~
                       supply%,          /* DAYS TO SUPPLY             */~
                       today%,           /* SUBSCRIPT FOR TODAY'S DATE */~
                       psbprior$,        /* PLANNING PRIORITY          */~
                       keyhit%,          /* KEYHIT IN & OUT            */~
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

            call "SHOSTAT" ("Closing Files, One Moment Please")

            gosub release_jobhold        /* Release last job from hold */

            call "FILEBGON" (#10)
            call "FILEBGON" (#61)
            call "FILEBGON" (#62)
            call "FILEBGON" (#63)
            call "FILEBGON" (#64)
            end
