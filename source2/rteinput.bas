        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  RRRR   TTTTT  EEEEE  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  R   R    T    E        I    NN  N  P   P  U   U    T     *~
            *  RRRR     T    EEEE     I    N N N  PPPP   U   U    T     *~
            *  R   R    T    E        I    N  NN  P      U   U    T     *~
            *  R   R    T    EEEEE  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RTEINPUT - Management of the Work Center sequences used   *~
            *            for building one part.                         *~
            *               An infinite (sic) number of alternate seqs  *~
            *            may be entered.  A WC may be labeled 'VEND' to *~
            *            indicate the need for outside processing.      *~
            *               Up to 100 Work Centers can be in each seq   *~
            *            for each routing.  When a WC Route is entered  *~
            *            or modified, the system recalsulates the Std   *~
            *            leadtime based on the new information.  The    *~
            *            User can then reedit the routing if desired.   *~
            *               Part type, MOQ, and Buyer/Planner may also  *~
            *            be modified before saving the data.            *~
            *                                                           *~
            *    TEXT-   Three types of text are used.  Routing text as *~
            *            General or Procedural and Step text.           *~
            *                                                           *~
            *----------------------------------------------------------Q*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/07/85 ! Added Unified Activity Codes             ! KAB *~
            * 09/24/85 ! For WCMASTR Format Changes               ! HES *~
            * 11/20/85 ! Added Text Editor for Route & Step       ! MJB *~
            * 11/20/85 ! Deleted Summary Screen                   ! MJB *~
            * 11/20/85 ! Changed Delete Routine to call ASKUSER   ! MJB *~
            * 03/11/86 ! Added User definable Step Numbers        ! HES *~
            * 09/25/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 07/31/86 ! Add Fields for standard costs, overlap   !     *~
            *          ! factors, and general ergo overhaul.      ! HES *~
            * 05/21/87 ! Standard Costing Enhancements            ! ERN *~
            * 11/02/87 ! Fixed soft enables, added toggle to sumry! HES *~
            * 06/15/88 ! Enable PF(24) for Administrators only.   ! JIM *~
            * 06/15/88 ! Miscellaneous fixes.                     ! JIM *~
            * 08/23/88 ! Added Init for TEXTID$() @ COLUMNONE     ! RJM *~
            *          !   which fixes Insert problem.  ADDED     !     *~
            *          !   Askuser option to not renumber Route   !     *~
            *          !   Step No's when copying to new route.   !     *~
            * 09/14/88 ! Small Fix to Copy Renumber Askuser.      ! RJM *~
            * 11/30/88 ! Added check for BOM Module Administrator ! MJB *~
            * 01/06/89 ! Changed MAT statement @ 17090 to CON     ! MJB *~
            * 01/26/93 ! PRR 12640 Show up to 200 Routes per Part.! JIM *~
            * 01/26/93 ! Some implied integer conversions fixed.  ! JIM *~
            * 01/26/93 ! ALLFREE.                                 ! JIM *~
            * 01/26/93 ! PRR 10693 Part is described on all scrns.! JIM *~
            * 01/27/93 ! PRR 11539 PF(14)Copy- added ASKUSER to   ! JIM *~
            *          !   see if user wants the Alternates too.  !     *~
            * 01/27/93 ! PRR 10590 PF(25)Step Free Text in INPUT. ! JIM *~
            * 01/27/93 ! PF(16) @ EDITALTERS now RETURNs to point ! JIM *~
            *          !   of access instead of going straight to !     *~
            *          !   LINE_SUMMARY.                          !     *~
            * 01/27/93 ! PRR 10178 PF(17) allows ADMIN% users to  ! JIM *~
            *          !   define Step Start & Increment defaults.!     *~
            * 01/27/93 ! Enabled PF(1)Start Over at Part # entry. ! JIM *~
            * 01/28/93 ! Force numeric Steps rt just & left 0 pad.! JIM *~
            * 01/28/93 ! Disable PF(14)Copy if no room in arrays. ! JIM *~
            * 02/05/93 ! QC Rework- don't let Step # go > 9999.   ! JIM *~
            * 02/05/93 ! QC Rework- Step > 9999 PF(17) safety net ! JIM *~
            *          !   on LINE_SUMMARY (ADMIN%'s only).       !     *~
            * 02/08/93 ! PRR 10178 PF(17) Step Start & Increment  ! JIM *~
            *          !   are stored in SYSFILE2 (SWITCHS.RTE).  !     *~
            * 05/06/93 ! Added new HANDLINGFACTOR field to be used! WPH *~
            *          !   in JBACTSUB to convert qty moved to    !     *~
            *          !   units of the job part.                 !     *~
            * 09/03/93 ! PRR 13008  Concurrent WC Descr blank if  ! JDH *~
            *          !   CC blanked.  No plowcode on setup code !     *~
            *          !   if no setup amount. Same for run code. !     *~
            * 09/13/93 ! PRR 12944  Added call to BOMEFDSP.       ! JDH *~
            * 10/18/93 ! PRR 13041  Cleared Alts when 'Delete All'! JDH *~
            * 03/15/94 ! Added Work Center defaults functionality.! MLJ *~
            * 03/30/94 ! Added PFKey prompt and call to ECRINQSB. ! LDJ *~
            * 11/16/94 ! PRR 13311  Nolonger read sysfile2 on Hold! RJH *~
            *          !   until just prior to write.             !     *~
            *          ! PRR 13318  Changed IF THEN ELSE w/ RETURN! RJH *~
            *          !   CLEAR for UNIX compatiblity.           !     *~
            * 03/24/95 ! PRR 13369  Modified edit for run time so ! JBK *~
            *          !   that a change to any of three fields   !     *~
            *          !   will cause a recalculation of the other!     *~
            *          !   two fields.  Restored default Activity !     *~
            *          !   code description.                      !     *~
            *          ! Misc. - Modified edit for setup time so  !     *~
            *          !   that a change to either of two fields  !     *~
            *          !   will cause a recalculation of the other!     *~
            *          !   fields.  Tightened up defaults and     !     *~
            *          !   edits of the handling factor to default!     *~
            *          !   to 1 and not let 0 be entered.         !     *~
            * 03/31/95 ! Took away force of 01/28/93. Rte Step.   ! JDH *~
            * 08/30/95 ! PRR 13481.  Use Activity's Descr if WC   ! JDH *~
            *          !   default is blank.                      !     *~
            *          ! PRR 13119.  Display Effective BOM/RTE.   !     *~
            *************************************************************

        dim                                                              ~
            actcode$(100)4,              /* Activity Perf. In Wc List  */~
            actdescr$32,                 /* Activity Code Description  */~
            activity$(100)40,            /* Activity Perf. In Wc List  */~
            altind%(2000),               /* Internal Pointer Array     */~
            amq0$(2001)5,                /* Alternate Move/Queue Time  */~
            amqp$(2001)1,                /* Alternate Move/Queue Flag  */~
            anm1$(2001)6,                /* Alt. Capacity adj. Factor 1*/~
            anm2$(2001)6,                /* Alt. Capacity adj. Factor 2*/~
            anm3$(2001)6,                /* Alt. Capacity adj. Factor 3*/~
            arn0$(2001)6,                /* Alternate Run Time         */~
            asht$(2001)7,                /* Alternate Shift Differental*/~
            asu0$(2001)6,                /* Alternate Setup Time       */~
            awc0$(2001)4,                /* Alternate Workcenter       */~
            awc1$(2001)4,                /* Alternate First Concurrent */~
            awc2$(2001)4,                /* Alternate Second Concurrent*/~
            awc3$(2001)4,                /* Alternate Third Concurrent */~
            ca1$(100)4,                  /* Concurrent Activity Code 1 */~
            ca2$(100)4,                  /* Concurrent Activity Code 2 */~
            ca3$(100)4,                  /* Concurrent Activity Code 3 */~
            cadescr$(3)32,               /* Activity Code Descriptions */~
            cg$(100)1,                   /* Force Contiguity Flag      */~
            comp$(100)3,                 /* % Complete through this stp*/~
            copytxt$1,                   /* Copy Text Switch           */~
            cpart$25,                    /* Search Part Number         */~
            crte$3,                      /* Search Route Id.           */~
            cursor%(2),                  /* Cursor Locations For Edit  */~
            date$8,                      /* Today's Clock Date         */~
            def_mqp$1,                   /* Default Move/Queue Bef/Aft */~
            def_rucode$4,                /* Default Run Code           */~
            def_rudescr$32,              /* Default Run Description    */~
            def_sucode$4,                /* Default Setup Code         */~
            defstepincrm$4,              /* Default Step Increment     */~
            defstepstart$4,              /* Default Step Start #       */~
            dfac$(20)1,                  /* Summary Screen Fac's       */~
            ecrpfk$14,                   /* ECR Inquiry PFKey Prompt   */~
            edtalt%(1020),               /* Internal Pointer Array     */~
            eff_rte$4,                   /* Effective Route            */~
            errormsg$79,                 /* Error Message Text         */~
            f1$(100,2)8,                 /* 1st overlap Factor         */~
            f2$(100,2)8,                 /* 2nd overlap Factor         */~
            fill1$(100)2,                /* Filler                     */~
            handfactor$(100)8,           /* Factor for use in JBACTSUB */~
            header$79,                   /* Header For Alts Screen     */~
            header1$79,                  /* Screen Subtitle            */~
            header2$79,                  /* Screen Subtitle - 2        */~
            hdr$(2)8,                    /* Screen Subtitle - 3        */~
            hdr1$42,                     /* Screen Subtitle - 4        */~
            i$(24)80,                    /* Screen Image (Not Used)    */~
            incl(1),                     /* PLOWCODE Argument          */~
            incl$(1)1,                   /* PLOWCODE Argument          */~
            information$79,              /* Current Routes Information */~
            lastrte$79,                  /* Last Part & Route Managed  */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen line 2              */~
            loc%(100),                   /* Internal Pointer Array     */~
            message$79,                  /* Edit Messages              */~
            mq$(100)5,                   /* Move/Queue Time Or Vendr LT*/~
            mqp$(100)1,                  /* Move/Que Before/After Flag */~
            nm1$(100)6,                  /* Capacity adj. Factor 1     */~
            nm2$(100)6,                  /* Capacity adj. Factor 2     */~
            nm3$(100)6,                  /* Capacity adj. Factor 3     */~
            nyield(100),                 /* Expec Process Yield In %   */~
            numbers$(14,2)8,             /* For summary Screen Display */~
            part$25,                     /* Which Part Is This Rte For */~
            partdescr$34,                /* Parts Description          */~
            pfkeys$32,                   /* Function Key Enable Lists  */~
            pfktext$(3)79,               /* Function Keys Descriptions */~
            phdr$(3)79,                  /* PLOWCODE Argument          */~
            ppr$28,                      /* PART + ROUTE               */~
            pprdescr$32,                 /*                            */~
            readkey$50, altrsky$50,      /* Key To Plow File With      */~
            readkey1$50, bomkey$50,      /* Key To Plow File With      */~
            rfac$(200)1,                 /* Displayed Route Facs       */~
            route$(200)3,                /* Routes In Rtemastr         */~
            rte$3,                       /* Which Alt Route Is This    */~
            rtekey$60,                   /* Plowkey For RTEMASTR       */~
            run$(100)6,                  /* Run Times (WCunits/Part)   */~
            runh$(100)8,                 /* Run Time  (Hrs/Part)       */~
            runp$8,                      /* Run Time  (Parts/Hr)       */~
            saverun$6, saverunh$8, savehtxt$4, seq$3, seqnr$3, savesuh$8,~
            seq$(100)4,                  /* SEQUENCE NUMBERS FOR SCREEN*/~
            scr%(7,17), set%(255),       /* Soft Enables / Screen Refs */~
            shift$(100)7,                /* Shift Differentials        */~
            src$3,                       /* Field for Text Source Code */~
            step$(100)4,                 /* Operation Code             */~
            su$(100)6,                   /* Set Up Times               */~
            suh$(100)8,                  /* Set Up Times In Clock Hours*/~
            sucode$(100)4,               /* Setup Activity Code        */~
            sudescr$32,                  /* Activity Code Description  */~
            tempkey$50,                  /* Temporary Disk Key         */~
            swrtekey$20,                 /* 'SWITCHS.RTE'              */~
            texta$(196,1)65,             /* Text Matrix for TXTINSUB   */~
            textid$(101)4,               /* Text ID (101 = header)     */~
            textmsg$79,                  /* Message to TXTINSUB        */~
            tfac$(20)1,                  /* Summary Screen Fac's       */~
            wc$(100)4,                   /* Primary Work Center        */~
            wc1$(100)4,                  /* First Concurrent           */~
            wc2$(100)4,                  /* Second Concurrent          */~
            wc3$(100)4,                  /* Third Concurrent           */~
            wcdescr$(4)32,               /* Work Center Description    */~
            yield$(100)3                 /* Expec Process Yield In %   */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! RTEMASTR ! STANDARD ROUTING FILE W/ALTERNATE ROUTES *~
            * # 2 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 3 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * # 4 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * # 5 ! ENGMASTR ! Engineering Master Filer                 *~
            * # 6 ! JBCROSS2 ! JOB RTE/BOM USED CROSS REF.              *~
            * # 7 ! RTEALTRS ! ALTERNATE ROUTE STEPS                    *~
            * # 8 ! CALMASTR ! Planning Production Calendar File        *~
            * # 9 ! TXTFILE  ! SYSTEM TEXT FILE                         *~
            * #10 ! RTEMAST2 ! STANDARD ROUTING TO STEP NUMBER X REF    *~
            * #11 ! STCBOMXF ! Std Costing Usage X-Ref file             *~
            * #12 ! GENCODES ! General Codes File                       *~
            * #13 ! WCDFLTS  ! Work Center Defaults File                *~
            * #20 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************

            select  #1, "RTEMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos =   5, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 35

            select  #2, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25

            select  #3, "WCMASTR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos =  2  , keylen = 5 ,                     ~
                         alt key  1, keypos = 1, keylen = 6

             select #4, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select # 5, "ENGMASTR",                                      ~
                        varc, indexed, recsize = 2015,                   ~
                        keypos =    1, keylen =  29

            select  #6,"JBCROSS2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize =  94,                                    ~
                       keypos =29, keylen = 19,                          ~
                       alternate key 1, keypos = 1 , keylen = 47,        ~
                                 key 2, keypos = 48, keylen = 47

            select  #7, "RTEALTRS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =   1, keylen = 34

            select  #8, "CALMASTR",                                      ~
                        varc, indexed, recsize = 1962,                   ~
                        keypos =    1, keylen =   2

            select  #9, "TXTFILE",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos =  1, keylen =  11

            select #10, "RTEMAST2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 50,                                   ~
                         keypos =   1, keylen = 32

            select #11, "STCBOMXF",                                      ~
                        varc, indexed, recsize = 72,                     ~
                        keypos =  29, keylen = 33,                       ~
                        alt key 1, keypos =  1, keylen = 36,             ~
                            key 2, keypos = 37, keylen = 36

            select #12, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =   1, keylen = 24

            select #13, "WCDFLTS",                                       ~
                        varc,     indexed,  recsize =   100,             ~
                        keypos =    1, keylen =   5

            select #20, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            call "SHOSTAT" ("Linking To Data Base For Management Of Work ~
        ~Center Routing Structures")

            call "OPENCHCK" (# 1, 0%, f2%( 1%), 200%, " ")
            call "OPENCHCK" (# 2, 0%, f2%( 2%),   0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3%),   0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4%),   0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5%),   0%, " ")
            call "OPENCHCK" (# 6, 0%, f2%( 6%),   0%, " ")
            call "OPENCHCK" (# 7, 0%, f2%( 7%), 100%, " ")
            call "OPENCHCK" (# 8, 0%, f2%( 8%),   0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10%), 200%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11%),   0%, " ")
            call "OPENCHCK" (#12, 0%, f2%(12%),   0%, " ")
            call "OPENCHCK" (#13, 0%, f2%(13%),   0%, " ")
            call "OPENCHCK" (#20, 0%, f2%(20%),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            altrs_allowed% = dim(altind%(),1%)
            lines_allowed% = dim(step$(),1%)
            str(line2$,62%) = "RTEINPUT: " & str(cms2v$,,8%)

            for i% = 1% to lines_allowed%
                convert i% to seq$(i%), pic(###)
                str(seq$(i%),4%) = ")"
            next i%
            hdr$(1) = "Quantity"
            hdr$(2) = " Percent"
            hdr1$   = "STEP OVERLAP FACTORS"
            gosub init_enables

*        See if operator is an administrator or not
            call "CMSMACHK" ("BOM", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%         ~
                                                else admin% = 0%
*        Check for GENCODES 'WC ACTVTY' header
            readkey$ = all(hex(00)) : str(readkey$,10) = "WC ACTVTY"
            write #12 using L09290, str(readkey$,,24), "WC/Activity Codes",~
                                  " 4", " ", eod goto L09310
L09290:         FMT CH(24), CH(30), CH(2), CH(72)

L09310
*        Default the Step Defaults, then see if they're in SYSFILE2.
            defstepstart% = 1000%  : defstepincrm% = 50%
            swrtekey$ = "SWITCHS.RTE"
            gosub read_switches_rte_rec

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            call "ALLFREE"
            activity$(), partdescr$, part$, rte$, comp$(), amqp$(), ppr$,~
            route$(), information$, actcode$(), errormsg$, pprdescr$,    ~
            wc$(), mq$(), yield$(), run$(), fill1$(), shift$(), asht$(), ~
            mqp$(), step$(), message$, wcdescr$(), actdescr$, sucode$(), ~
            su$(), sudescr$, cg$(), f1$(), f2$(), ca1$(), ca2$(), ca3$(),~
            wc1$(), wc2$(), wc3$(), nm1$(), nm2$(), nm3$(), awc0$(),     ~
            awc1$(), awc2$(), awc3$(), amq0$(), asu0$(), arn0$(),anm1$(),~
            anm2$(), anm3$(), textmsg$, texta$(), cadescr$(), suh$(),    ~
            runh$(), handfactor$(), def_mqp$, def_sucode$, def_rucode$,  ~
            def_rudescr$, runp$, ecrpfk$, eff_rte$ = " "

            mat altind% = zer
            altmax%, altind%, line%, noedit%, editmode%, defaults% = 0%
            init(hex(00)) textid$()
            copytxt$ = "N"

            call "TXTFUTIL" (#9, f2%(9), "INTL", "    ")

L10240:     for fieldnr% = 1% to 2%
                gosub'161(fieldnr%)
                      if enabled% = 0% then L10410
L10270:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  4% then L10240
                      if keyhit% <> 14% then L10370
                          cpart$ = part$
                          gosub find_rte
                          if errormsg$ <> " " then L10270
                          information$ = " "
                          part$ = cpart$ : rte$ = crte$
                          gosub L50220  /* Load Route */
                          goto L10270   /* Shouldn't Happen */
L10370:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit%  = 17% then gosub set_step_default_values
                      if keyhit% <>  0 then       L10270
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10270
L10410:         next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUTS LINE ITEMS AND TESTS FOR VALIDITY.                 *~
            *************************************************************

L10490:     if maxlines% = lines_allowed% then L10530
                c% = maxlines% + 1%
                gosub inputlines
                if keyhit% <> 16% then L10550
L10530:              gosub columnone
                     goto line_summary
L10550:         maxlines% = maxlines% + 1%
                goto L10490

        inputlines
            linemode% = 0
            gosub columnone
            wcdescr$(), cadescr$(), actdescr$, sudescr$, runp$ = " "
            if maxlines% = 0 and editmode% = 0 then                      ~
                errormsg$ = hex(84) & "Enter The Production Steps..."
L10640:     for fieldnr% = 1% to 11%
                gosub'162(fieldnr%, 1%)
                      if enabled% = 0% then L10830
L10670:         gosub'202(fieldnr%)
                      if keyhit%  = 16% then return
                      if keyhit%  = 25% then gosub'181(c%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then       inputlines
                      if keyhit% <>  6% then L10740
                         gosub prevline
                         goto L10830
L10740:               if keyhit% <>  4% then L10820
L10750:                  fieldnr% = max(1, fieldnr% - 1%)
                         if fieldnr% > 1% then L10790
                             gosub columnone
                             goto L10640
L10790:                  gosub'162(fieldnr%, 1%)
                         if enabled% <> 0 then L10670
                         goto L10750
L10820:               if keyhit% <>  0 then       L10670
L10830:         gosub'152(fieldnr%)
                  if errormsg$ <> " " then L10670
                next fieldnr%

            REM Page Two...
L10890:     for fieldnr% = 1% to 13%
                gosub'163(fieldnr%, 1%)
                      if enabled% = 0% then L11060
L10920:         gosub'203(fieldnr%)
                      if keyhit%  = 16% then return
                      if keyhit%  = 25% then gosub'181(c%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then       inputlines
                      if keyhit% <>  6% then L10990
                         gosub prevline1
                         goto L11060
L10990:               if keyhit% <>  4% then L11050
L11000:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L10890
                         gosub'163(fieldnr%, 1%)
                         if enabled% <> 0% then L10920
                         goto L11000
L11050:               if keyhit% <>  0% then       L10920
L11060:         gosub'153(fieldnr%)
                  if errormsg$ <> " " then L10920
                next fieldnr%
                return

        prevline
            if c% = 1% then return
                on fieldnr% gosub L11260,           /* Step Number      */~
                                  L11270,           /* Work Center Code */~
                                  L11280,           /* Move/Queue Days  */~
                                  L11300,           /* Set Up Time      */~
                                  L11320,           /* Run Time         */~
                                  L11350,           /* Yield Percentage */~
                                  L11360,           /* Setup Activity Co*/~
                                  L11370,           /* Activity Code    */~
                                  L11380,           /* Activity Descrip */~
                                  L11385,           /* % Complete (memo)*/~
                                  L11386            /* Handling Factor  */

            return

L11260:         step$    (c%) = step$    (c%-1) : return
L11270:         wc$      (c%) = wc$      (c%-1) : return
L11280:         mq$      (c%) = mq$      (c%-1)
                mqp$     (c%) = mqp$     (c%-1) : return
L11300:         su$      (c%) = su$      (c%-1)
                suh$     (c%) = suh$     (c%-1) : return
L11320:         run$     (c%) = run$     (c%-1)
                runh$    (c%) = runh$    (c%-1) : return
L11350:         yield$   (c%) = yield$   (c%-1) : return
L11360:         sucode$  (c%) = sucode$  (c%-1) : return
L11370:         actcode$ (c%) = actcode$ (c%-1)
L11380:         activity$(c%) = activity$(c%-1) : return
L11385:         comp$    (c%) = comp$    (c%-1) : return
L11386:         handfactor$(c%) = handfactor$(c%-1) : return

        prevline1
            if c% = 1% then return
                on fieldnr% gosub L11570,         /* Shift Differential */~
                                  L11580,         /* Concurrent WC One  */~
                                  L11590,         /* Capacity adj. Factr*/~
                                  L11600,         /* Activity Code 2    */~
                                  L11610,         /* Concurrent WC Two  */~
                                  L11620,         /* Capacity adj. Factr*/~
                                  L11630,         /* Activity Code 3    */~
                                  L11640,         /* Concurrent WC Three*/~
                                  L11650,         /* Capacity adj. Factr*/~
                                  L11660,         /* Activity Code 4    */~
                                  L11670,         /* Force Contiguity   */~
                                  L11680,         /* 1st Overlap Fctr   */~
                                  L11700          /* 2nd Overlap Fctr   */
            return

L11570:         shift$   (c%) = shift$   (c%-1) : return
L11580:         wc1$     (c%) = wc1$     (c%-1) : return
L11590:         nm1$     (c%) = nm1$     (c%-1) : return
L11600:         ca1$     (c%) = ca1$     (c%-1) : return
L11610:         wc2$     (c%) = wc2$     (c%-1) : return
L11620:         nm2$     (c%) = nm2$     (c%-1) : return
L11630:         ca2$     (c%) = ca2$     (c%-1) : return
L11640:         wc3$     (c%) = wc3$     (c%-1) : return
L11650:         nm3$     (c%) = nm3$     (c%-1) : return
L11660:         ca3$     (c%) = ca3$     (c%-1) : return
L11670:         cg$      (c%) = cg$      (c%-1) : return
L11680:         f1$   (c%,1%) = f1$   (c%-1,1%)
                f1$   (c%,2%) = f1$   (c%-1,2%) : return
L11700:         f2$   (c%,1%) = f2$   (c%-1,1%)
                f2$   (c%,2%) = f2$   (c%-1,2%) : return
        columnone
           wc$(c%), mq$(c%), su$(c%), run$(c%), comp$(c%), activity$(c%),~
           shift$(c%), actcode$(c%), yield$(c%), mqp$(c%), step$(c%),    ~
           sucode$(c%), cg$(c%), f1$(c%,1%), f1$(c%,2%), f2$(c%,1%),     ~
           f2$(c%,2%), suh$(c%), runh$(c%), handfactor$(c%) = " "
           textid$(c%) = all(hex(00))
           return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        editmode
            message$ = "Select Desired Function From PF Key List Below."
            editmode% = 1%
            gosub'201(0%)
                  errormsg$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       L12000
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 17% then gosub set_step_default_values
                  if keyhit%  = 24% then gosub override
                  if keyhit%  = 25% then gosub'181(lines_allowed% + 1%)
            goto editmode

L12000: REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                                                           *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

        line_summary              /* Summary Screen */
            errormsg$ = " "
            editmode% = 1%
            line% = max(0%,min(line%,maxlines%-13%))
            currentline% = 0
L12055:     message$ =   "To Modify a Line Item, Position Cursor and" &  ~
                         " press RETURN."
L12065:     gosub'214(0%)
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line% = 0%
                  if keyhit%  =  3 then line% = maxlines%
                  if keyhit%  =  4 then line% = line% - 13%
                  if keyhit%  =  5 then line% = line% + 13%
                  if keyhit%  =  6 then line% = line% - 1%
                  if keyhit%  =  7 then line% = line% + 1%
                  if keyhit% <> 17% then goto L12116
                     gosub set_step_default_values
                     goto line_summary
L12116:           if keyhit% <> 18% then goto L12126
                     gosub re_number_steps
                     goto L12055
L12126:           line% = max(0%,min(line%,maxlines%-14%))
                  if keyhit%  = 9% then editmode
                  copyalts%, copymode% = 0%
                  if keyhit% <> 14% then L12150       /* PF(14)Copy ? */
                     gosub determine_copy_alternates          /* Yup */
                     if copyalts% = 1% then goto L12150 /* Abort copy */
                     copymode% = 1%       /* Else we're in copy mode */
                     keyhit% = 11%
L12150:           if keyhit%  = 16 then datasave
                  if keyhit%  = 25 then gosub'181(lines_allowed% + 1%)
                  if keyhit% <> 28 then L12175
                     fieldnr% = 0
                     goto deletemode
L12175:     fieldnr% = max(cursor%(1)-6%, 0%)
            if keyhit% <> 11% or cursor%(1) > 1 then L12190
                c% = maxlines% : goto L12235
L12190:     errormsg$ = hex(84) & "Please Position Cursor First..."
            if fieldnr% > 14% then L12065
            if keyhit% < 8% or keyhit% = 25% then errormsg$ = " "
            if fieldnr% = 0% and keyhit% = 0% then fieldnr% = 1%
            if fieldnr% < 1% and keyhit% <> 11% then L12065
                c% = max(0%, min(line% + fieldnr%, maxlines%))
                if c% = 0% and keyhit% <> 11% then L12065
                fieldnr% = c% - line%
            errormsg$ = " "
L12235:     if keyhit% = 8% then gosub editalters
            errormsg$ = "Sorry, Route Can't Be Changed."
            if noedit% = 1% and keyhit% <> 0% then L12065
            errormsg$ = " "
            if keyhit% = 12 then deletemode
            if keyhit% = 11 then insertmode
            if keyhit% <> 0 then line_summary

        editpg3       /* Line Item Detail - First Screen      */
            errormsg$ = " "
            gosub describe_line
L12290:     message$ = "To Modify Displayed Values, Position Cursor " &  ~
                       "To Desired Value And Press RETURN."
            linemode% = 1
            lastfieldnr% = 0%
            gosub'212(0%)
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  <  2 or keyhit%  >  7 then L12360
                     if keyhit%  =  2 then c% = 1
                     if keyhit%  =  3 then c% = maxlines%
                     if keyhit%  =  5 then editpg4
                     if keyhit%  =  6 then c% = max(1, c%-1)
                     if keyhit%  =  7 then c% = min(maxlines%, c%+1)
                     goto editpg3
L12360:           if keyhit%  =  8 then gosub editalters
                  if keyhit%  =  9 then editmode
                  if keyhit%  = 16 then line_summary
                  if keyhit%  = 25 then gosub'181(c%)
                  if keyhit%  = 29 then L12410
                  if keyhit% <>  0 then L12290
                  if noedit% <>  1  then L12410
                     errormsg$ = "Route Can't Be Modified."
                     goto L12290

L12410:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 13 then L12290
            if fieldnr% = 4 or fieldnr% = 7 then L12290
            if fieldnr% > 7 then fieldnr% = fieldnr% - 1
            if fieldnr% > 4 then fieldnr% = fieldnr% - 1
            if fieldnr%  = lastfieldnr% then L12290
            if keyhit% <> 29% then L12460
                gosub'049(1%, fieldnr%)
                goto editpg3

L12460:     gosub'162(fieldnr%, 2%)
                if enabled% = 0% then L12290
L12470:     gosub'212(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12470
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12470
                     lastfieldnr% = fieldnr%
                     goto L12410

        editpg4       /* Line Item Detail - First Screen      */
            errormsg$ = " "
            gosub describe_line
L12525:     message$ = "To Modify Displayed Values, Position Cursor " &  ~
                       "To Desired Value And Press RETURN."
            linemode% = 1
            lastfieldnr% = 0%
            gosub'213(0%)
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  <  2 or keyhit%  >  7 then L12595
                     if keyhit%  =  2 then c% = 1
                     if keyhit%  =  3 then c% = maxlines%
                     if keyhit%  =  4 then editpg3
                     if keyhit%  =  6 then c% = max(1, c%-1)
                     if keyhit%  =  7 then c% = min(maxlines%, c%+1)
                     goto editpg4
L12595:           if keyhit%  =  8 then gosub editalters
                  if keyhit%  =  9 then editmode
                  if keyhit%  = 16 then line_summary
                  if keyhit%  = 25 then gosub'181(c%)
                  if keyhit%  = 29 then L12645
                  if keyhit% <>  0 then L12525
                  if noedit% <>  1  then L12645
                     errormsg$ = "Route Can't Be Modified."
                     goto L12525

L12645:     fieldnr% = cursor%(1) - 5
L12650:     if fieldnr% < 1 or fieldnr% > 14 then L12525
            if fieldnr% = 12 then L12525
            if fieldnr% > 12 then fieldnr% = fieldnr% - 1
            if fieldnr%  = lastfieldnr% then L12525
            if keyhit% <> 29% then L12690
                gosub'049(2%, fieldnr%)
                goto editpg4

L12690:     gosub'163(fieldnr%, 2%)
                if enabled% = 0% then L12525
L12700:     gosub'213(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12700
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L12700
                     lastfieldnr% = fieldnr%
                     if fieldnr% = 2% or fieldnr% = 3% and wc1$(c%)<>" " ~
                                                               then L12765
                     if fieldnr% = 5% or fieldnr% = 6% and wc2$(c%)<>" " ~
                                                               then L12765
                     if fieldnr% = 8% or fieldnr% = 9% and wc3$(c%)<>" " ~
                                                               then L12765
                     goto L12645
L12765:                   fieldnr% = fieldnr% + 1%
                          goto L12650

        REM *************************************************************~
            *       I N S E R T   &   D E L E T E   L O G I C           *~
            *                                                           *~
            * INSERT & DELETE CODE RESIDES HERE.                        *~
            *************************************************************

        insertmode
            if copymode% <> 1% then L13095
                REM Find Assembly To Copy In...
                cpart$ = " " : gosub find_rte
                if errormsg$ <> " " then line_summary
                readkey$ = str(cpart$,,25%) & str(crte$,,3%) & " "
                savehtxt$ = textid$(101)
                if pos(textid$(101) <> hex(00)) = 0 then savehtxt$ = " "
                if pos(textid$(101) <> hex(ff)) = 0 then savehtxt$ = " "
                renum% = maxlines%
                print at(4,1,80); hex(84); "Loading Data..."

L13095:     REM Copy all Elements Up One...
            maxlines%=maxlines% + 1%
            c% = c% + 1%
            if c%=maxlines% then L13165
                roll% = -1%
                for temp% = maxlines% to c% step -1%
                     gosub roll_lines
                next temp%
                if altmax% = 0% then L13165
                    altind% = 0%
                    for i% = 1% to altmax%
                     if altind%(i%) < c% then L13155
                     if altind% = 0% then altind% = i%
                     altind%(i%) = altind%(i%) + 1%
L13155:             next i%
                    if altind% = 0% then altind% = i%

L13165:     REM Get Line Item Data...
            if copymode% = 0% then L13265
               call "PLOWNEXT" (#1, readkey$, 28%, f1%(1))
                    if f1%(1) = 0 then L13205
               gosub load_rte_line
               if copyalts% = 16% then gosub copy_alternates
               copymode% = 2%
               goto insertmode

L13205:        REM Do Final Cleanup For External Copy...
               if savehtxt$ <> " " then textid$(101) = savehtxt$ else    ~
                       call "TXTFUTIL" (#9, f2%(9), "COPY", textid$(101))
               if renum% <> 0% then L13235
L13220:           keyhit1% = 3%
                  call "ASKUSER" (keyhit1%, "COPY ROUTE",                ~
                                               "Renumber Step Numbers?", ~
                              "Press <ENTER> to Copy Route Steps AS IS", ~
                              "Press PF1 to Renumber the Step Numbers")
                  if keyhit1% = 0% then L13255
                  if keyhit1% <> 1% then L13220
L13235:        gosub re_number_steps
L13255:        goto delete_line

L13265:     REM This Is Normal Insertmode Code...
            gosub inputlines
            if keyhit% = 16% then delete_line
            goto insertmode

        deletemode
            if maxlines% = 0 then line_summary
            message$ = "To DELETE Flashing Data, press RETURN, To Return ~
        ~without Delete, press PF1."
            gosub'125(fieldnr%)
                  if keyhit% =  1 then line_summary
                  if keyhit% = 16% then line_summary
                  if keyhit%<>0% then deletemode
            if fieldnr% <> 0 then delete_line
            for c% = 1% to maxlines%  /* Kill'em All */
                call "TXTFUTIL" (#9, f2%(9), "XOUT", textid$(c%))
                gosub columnone
            next c%
            mat altind% = zer
            altind%, altmax% = 0%
            maxlines% = 0%
            goto line_summary

        delete_line
            call "TXTFUTIL" (#9, f2%(9), "XOUT", textid$(c%))
            gosub delete_it
            goto line_summary

        delete_it
            if c% = maxlines% then L13425
                roll% = 1%
                for temp% = c% to maxlines%-1%
                     gosub roll_lines
                next temp%
L13425:         if altmax%=0% then L13455
                   for i%=1 to altmax%
                       if altind%(i%) < c% then L13450
                       if altind%(i%) = c% then altind%(i%) = 0%
                       altind%(i%) = altind%(i%) - 1%
L13450:            next i%
L13455:     c%=maxlines%
            gosub columnone
            maxlines%=maxlines%-1%
        return

        roll_lines
                step$     (temp%) = step$     (temp%+roll%)
                wc$       (temp%) = wc$       (temp%+roll%)
                su$       (temp%) = su$       (temp%+roll%)
                suh$      (temp%) = suh$      (temp%+roll%)
                run$      (temp%) = run$      (temp%+roll%)
                runh$     (temp%) = runh$     (temp%+roll%)
                mq$       (temp%) = mq$       (temp%+roll%)
                fill1$    (temp%) = fill1$    (temp%+roll%)
                textid$   (temp%) = textid$   (temp%+roll%)
                activity$ (temp%) = activity$ (temp%+roll%)
                actcode$  (temp%) = actcode$  (temp%+roll%)
                sucode$   (temp%) = sucode$   (temp%+roll%)
                comp$     (temp%) = comp$     (temp%+roll%)
                handfactor$(temp%) = handfactor$(temp%+roll%)
                yield$    (temp%) = yield$    (temp%+roll%)
                cg$       (temp%) = cg$       (temp%+roll%)
                f1$    (temp%,1%) = f1$    (temp%+roll%,1%)
                f1$    (temp%,2%) = f1$    (temp%+roll%,2%)
                f2$    (temp%,1%) = f2$    (temp%+roll%,1%)
                f2$    (temp%,2%) = f2$    (temp%+roll%,2%)
                wc1$      (temp%) = wc1$      (temp%+roll%)
                wc2$      (temp%) = wc2$      (temp%+roll%)
                wc3$      (temp%) = wc3$      (temp%+roll%)
                nm1$      (temp%) = nm1$      (temp%+roll%)
                nm2$      (temp%) = nm2$      (temp%+roll%)
                nm3$      (temp%) = nm3$      (temp%+roll%)
                ca1$      (temp%) = ca1$      (temp%+roll%)
                ca2$      (temp%) = ca2$      (temp%+roll%)
                ca3$      (temp%) = ca3$      (temp%+roll%)
                shift$    (temp%) = shift$    (temp%+roll%)
                mqp$      (temp%) = mqp$      (temp%+roll%)
        return

        REM *************************************************************~
            *               M I S C   R O U T I N E S                   *~
            *                                                           *~
            * Miscelleneous routines reside in this area.               *~
            *************************************************************

        describe_line
            wcdescr$(1), wcdescr$(2), wcdescr$(3), wcdescr$(4) =         ~
                                                hex(94) & "(Not On File)"
            call "GETCODE" (#3, wc$(c%),  wcdescr$(1), 1%, 99, f1%(3))
            call "GETCODE" (#3, wc1$(c%), wcdescr$(2), 1%, 99, f1%(3))
            call "GETCODE" (#3, wc2$(c%), wcdescr$(3), 1%, 99, f1%(3))
            call "GETCODE" (#3, wc3$(c%), wcdescr$(4), 1%, 99, f1%(3))
            actdescr$, sudescr$, cadescr$(1), cadescr$(2), cadescr$(3) = ~
                                            hex(94) & "(Will Be Created)"
            tempkey$ = "WC ACTVTY" & actcode$(c%)
            call "GETCODE" (#12, tempkey$, actdescr$, 0%, 99, f1%(12))
            tempkey$ = "WC ACTVTY" & sucode$(c%)
            call "GETCODE" (#12, tempkey$, sudescr$,  0%, 99, f1%(12))
            tempkey$ = "WC ACTVTY" & ca1$(c%)
            call "GETCODE" (#12, tempkey$, cadescr$(1), 1%, 99, f1%(12))
            tempkey$ = "WC ACTVTY" & ca2$(c%)
            call "GETCODE" (#12, tempkey$, cadescr$(2), 1%, 99, f1%(12))
            tempkey$ = "WC ACTVTY" & ca3$(c%)
            call "GETCODE" (#12, tempkey$, cadescr$(3), 1%, 99, f1%(12))
            if wc1$(c%) = " " then wcdescr$(2) = " "
            if wc2$(c%) = " " then wcdescr$(3) = " "
            if wc3$(c%) = " " then wcdescr$(4) = " "
            if ca1$(c%) = " " then cadescr$(1) = " "
            if ca2$(c%) = " " then cadescr$(2) = " "
            if ca3$(c%) = " " then cadescr$(3) = " "
            temp = 0
            convert runh$(c%) to temp, data goto L14290
            if temp <> 0 then temp = 1/temp
L14290:     call "CONVERT" (temp, 0.6, runp$)
        return

        find_rte
            readkey$ = cpart$
            incl(1) = 0
            phdr$(2)="  Part Assemblies             Part Descriptions"
            phdr$(1) = "  Existing RTEs For Part.  Use PF-1 To Select Ano~
        ~ther Part."
            phdr$(3) = hex(ac) & "Select the Assembly Part And/Or RTE ID.~
        ~  Use PF-16 to Cancel Search."
            readkey1$ = hex(06) & "Select the Part Assembly"
            REM *** Get Part & RTEID ***
            errormsg$ = hex(84) & "Search Canceled."
            call "PLOWCODE" (#1, readkey$, readkey1$, 8025%, -.01,       ~
                 f1%(1), phdr$(), 3.32, 0, incl(), incl$(), " ", " ", #2)
                if f1%(1) = 0% then return
            cpart$ = readkey$
            crte$ = str(readkey$,26%,3%)
            errormsg$ = " "
        return

        REM *************************************************************~
            *  FULL-SCREEN INPUT / EDIT / MAINT OF STEP DEFAULT VALUES  *~
            * All the code is tucked away here, so don't go looking in  *~
            * the 2xxxx's, 4xxxx's or 5xxxx's etc. for related code.    *~
            *************************************************************

        set_step_default_values
            keysave% = keyhit%
            gosub read_switches_rte_rec
            convert defstepstart% to defstepstart$, pic (###0)
            convert defstepincrm% to defstepincrm$, pic (###0)

            for stepfld% = 1% to 1%
                gosub'051(stepfld%)        /* Default / Enables */
                     if enabled% = 0% then L15210
L15150:         gosub'101(stepfld%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <> 16% then goto L15200
                          keyhit% = keysave%
                          return
L15200:              if keyhit% <>  0% then goto L15150
L15210:         gosub'150(stepfld%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L15150
            next stepfld%

        edit_step_default_values
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit% <> 16% then goto L15340
                     keyhit% = keysave%
                     call "READ101" (#20, swrtekey$, f1%(20%))
                     put #20 using L24570, swrtekey$, defstepstart%,      ~
                          defstepincrm%, " ", " "
                     if f1%(20%) = 0% then write #20 else rewrite #20
                     return
L15340:         if keyhit% <>  0% then goto edit_step_default_values
            gosub'051(1%)               /* Check Enables, Set Defaults */
                if enabled% =  0% then goto edit_step_default_values
L15370:     gosub'101(1%, 2%)           /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then goto L15370
            gosub'150(1%)               /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then goto L15370
            goto edit_step_default_values

        deffn'051(stepfld%)
            enabled% = 1%

        REM Def/Enable Default Step Start Value    DEFSTEPSTART$

        REM Def/Enable Default Step Increment      DEFSTEPINCRM$
            return

        deffn'101(stepfld%, edit%)
            gosub set_pf1
            init (hex(84)) lfac$()
            if stepfld% <> 0% then lfac$(stepfld%) = hex(82)
            str(line2$,,61%) = "Set Step Start Value and Increment"

L15580:     accept                                                       ~
               at (01,02), "Manage Production Work Center Routings",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Default Step Start Value",                   ~
               at (06,30), fac(lfac$(1%)), defstepstart$        , ch(04),~
                                                                         ~
               at (07,02), "Default Step Increment",                     ~
               at (07,30), fac(lfac$(1%)), defstepincrm$        , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1%)         , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3%)         , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L15810
                  call "MANUAL" ("RTEINPUT") : goto L15580

L15810:        if keyhit% <> 15% then L15840
                  call "PRNTSCRN" : goto L15580

L15840:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L16000     /*  Input Mode             */
            message$ = "Enter the default Step Start Value AND Increment."
            pfktext$(1%) = "(1)Start Over                           " &  ~
                     "                       (13)Instructions"
            pfktext$(2%) = "                                        " &  ~
                     "                       (15)Print Screen"
            pfktext$(3%) = "                                        " &  ~
                     "                       (16)Return      "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return

L16000: if stepfld% > 0% then L16100  /*  Edit Mode - Select Fld */
            message$ = "Press (RETURN) to edit the Step Default values."
            pfktext$(1%) = "(1)Start Over                           " &  ~
                     "                       (13)Instructions"
            pfktext$(2%) = "                                        " &  ~
                     "                       (15)Print Screen"
            pfktext$(3%) = "                                        " &  ~
                     "                       (16)SAVE/Return "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L16100:                              /*  Edit Mode - Enabled    */
            message$ = "Enter the default Step Start Value AND Increment."
            pfktext$(1%) = "(1)Start Over                           " &  ~
                     "                       (13)Instructions"
            pfktext$(2%) = "                                        " &  ~
                     "                       (15)Print Screen"
            pfktext$(3%) = "                                        " &  ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        deffn'150(stepfld%)
            errormsg$ = " "

        REM Test for Default Step Start Value     DEFSTEPSTART$
            if defstepstart$ = " " then defstepstart$ = "1"
            if pos(defstepstart$ = ".") = 0% then goto L16290
                errormsg$ = "Decimals not allowed: " & defstepstart$
                return
L16290:     if pos(defstepstart$ = "-") = 0% then goto L16320
                errormsg$ = "Negatives not allowed: " & defstepstart$
                return
L16320:     convert defstepstart$ to defstepstart%, data goto L16530
            if defstepstart% <> 0% then goto L16360
                errormsg$ = "Step Start Value may not be zero."
                return
L16360:     convert defstepstart% to defstepstart$, pic (####)

        REM Test for Default Step Increment       DEFSTEPINCRM$
            if defstepincrm$ = " " then defstepincrm$ = "1"
            if pos(defstepincrm$ = ".") = 0% then goto L16430
                errormsg$ = "Decimals not allowed: " & defstepincrm$
                return
L16430:     if pos(defstepincrm$ = "-") = 0% then goto L16460
                errormsg$ = "Negatives not allowed: " & defstepincrm$
                return
L16460:     convert defstepincrm$ to defstepincrm%, data goto L16540
            if defstepincrm% <> 0% then goto L16500
                errormsg$ = "Increment Value may not be zero."
                return
L16500:     convert defstepincrm% to defstepincrm$, pic (####)
            return

L16530:     errormsg$ = "Invalid entry: " & defstepstart$ : return
L16540:     errormsg$ = "Invalid entry: " & defstepincrm$ : return

        re_number_steps
            if maxlines% > 0% then goto L16600
                errormsg$ = "Sorry, there are no Steps to Re-number."
                return
L16600:     if defstepstart%+(defstepincrm%*(maxlines%-1%)) <= 9999%     ~
                then goto L16650      /* Skip re-# if it goes past 9999 */
                errormsg$ = "Sorry, Step #s would exceed 9999 at curren"&~
                     "t Step values."
                return
L16650:     for i% = 1% to maxlines%
                if i% = 1%                                               ~
                     then convert defstepstart% to step$(i%), pic (0000) ~
                     else convert defstepstart%+(defstepincrm%*(i%-1%))  ~
                          to step$(i%), pic (0000)
            next i%
            return

        REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *       ALTERNATE ROUTE STEPS                               *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

        editalters
            keysave% = keyhit%
            currentline% = c%
            l%, cc%, ss%, edtmax% = 0%
            mat edtalt% = con : mat edtalt% = (2001%)*edtalt%
            if altmax% = 0% then L17170
               for i% = 1% to altmax%
                if altind%(i%)<>currentline% then L17150
                edtmax%=edtmax% + 1%
                edtalt%(edtmax%) = i%
L17150:        next i%

L17170:     gosub'215(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then l% = 0%
                  if keyhit%  =  3 then l% = edtmax%
                  if keyhit%  =  4 then l% = l% - 12%
                  if keyhit%  =  5 then l% = l% + 12%
                  if keyhit%  =  6 then l% = l% - 1%
                  if keyhit%  =  7 then l% = l% + 1%
                  l% = max(0%,min(l%,edtmax%-12%))
                  if keyhit% <> 9% then L17290
                     c%, currentline% = max(1%, currentline%-1%)
                     goto editalters
L17290:           if keyhit% <> 10% then L17320
                     c%, currentline% = min(maxlines%, currentline%+1%)
                     goto editalters
L17320:           if keyhit% <> 16% then L17330
                          keyhit% = keysave%
                          return
L17330:           if noedit% <>  0% then L17170
                  if keyhit%  =  0% then L17390
                  if keyhit%  = 11% then L17390
                  if keyhit%  = 12% then L17390
                  goto L17170

L17390:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                ss% = max(0%, cursor%(1%) - 7%)
                if ss% > 13% then L17170
                if keyhit% = 11% then insertalts
                cc% = min(ss%+l%, edtmax%)
                ss% = cc% - l%
                if edtmax% = 0% or ss% = 0% then L17170
                if cc% > edtmax% then L17170
                if keyhit% = 12% then deletealts

L17500:         gosub'215(ss%)           /* NOW GET FIELD TO MODIFY    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then L17500
                gosub'154(ss%)
                      if errormsg$ <> " " then L17500
                goto L17170

        deletealts
                message$ = "To DELETE Flashing Data, press RETURN, To Ret~
        ~urn without Delete, press PF1."
                gosub'235(ss%)
                if keyhit% <> 0% then L17170

                altind%(edtalt%(cc%))=0%
                if cc%=edtmax% then L17750

                   for i% = cc% to edtmax% - 1%
                     edtalt%(i%)=edtalt%(i%+1%)
                   next i%

L17750:         edtalt%(edtmax%)=2001%
                edtmax%=edtmax%-1%
                l% = max(0%,min(l%,edtmax%-14%))
                goto L17170

        insertalts
                if edtmax% < 999% then L17850
                   errormsg$="MAXIMUM ALTERNATES FOR THIS STEP, CAN'T ADD~
        ~ MORE."
                   goto L17170
L17850:         if altmax% < 2000% then L17890
                   errormsg$="ALTERNATE LIST FULL, CAN'T ADD MORE."
                   goto L17170

L17890:         cc% = min(ss%+l%, edtmax%) + 1%
                ss% = cc% - l%

                if ss% < 14% then L17960
                   l% = l% + 1%
                   ss% = 13%

L17960:         if cc% <= edtmax% then L18000
                edtalt%(cc%)=altmax%+1%
                goto L18200

L18000:         for i% = altmax% to edtalt%(cc%) step -1%
                    awc0$(i%+1%) = awc0$(i%)
                    amq0$(i%+1%) = amq0$(i%)
                    asu0$(i%+1%) = asu0$(i%)
                    arn0$(i%+1%) = arn0$(i%)
                    awc1$(i%+1%) = awc1$(i%)
                    anm1$(i%+1%) = anm1$(i%)
                    awc2$(i%+1%) = awc2$(i%)
                    anm2$(i%+1%) = anm2$(i%)
                    awc3$(i%+1%) = awc3$(i%)
                    anm3$(i%+1%) = anm3$(i%)
                    asht$(i%+1%) = asht$(i%)
                    amqp$(i%+1%) = amqp$(i%)
                    altind%(i%+1%) = altind%(i%)
                next i%

                for i% = edtmax% to cc% step -1%
                    edtalt%(i%+1%)=edtalt%(i%)+1%
                next i%

L18200:         edtmax%=edtmax%+1%
                altmax%=altmax%+1%
L18220:         altind%(edtalt%(cc%))=0%
                i%=edtalt%(cc%)
                init (" ") awc0$(i%), amq0$(i%), asu0$(i%), arn0$(i%),   ~
                           awc1$(i%), anm1$(i%), awc2$(i%), anm2$(i%),   ~
                           awc3$(i%), anm3$(i%), asht$(i%), amqp$(i%),   ~
                           errormsg$

L18290:         gosub'204(ss%)
                     if keyhit% =  1% then gosub startover
                     if keyhit% =  2% then      L18220
                     if keyhit% =  6% then gosub lineabovealts
                     if keyhit% = 16% then      L18400
                     if keyhit% <> 0% then      L18290
                gosub'154(ss%)
                     if errormsg$<>" " then L18290
                altind%(edtalt%(cc%))=currentline%
                goto insertalts

L18400:            for i% = cc% to edtmax%
                      edtalt%(i%)=edtalt%(i%+1%)
                   next i%
                edtmax%=edtmax%-1%
                errormsg$=" "
                l% = max(0%,min(l%,edtmax%-13%))
                goto L17170

        lineabovealts
                if cc% = 1% then return
                    i% = edtalt%(cc%):j%=edtalt%(cc%-1%)
                    awc0$(i%) = awc0$(j%)
                    amq0$(i%) = amq0$(j%)
                    asu0$(i%) = asu0$(j%)
                    arn0$(i%) = arn0$(j%)
                    awc1$(i%) = awc1$(j%)
                    anm1$(i%) = anm1$(j%)
                    awc2$(i%) = awc2$(j%)
                    anm2$(i%) = anm2$(j%)
                    awc3$(i%) = awc3$(j%)
                    anm3$(i%) = anm3$(j%)
                    asht$(i%) = asht$(j%)
                    amqp$(i%) = amqp$(j%)
                    errormsg$  = " "
                return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if noedit% = 1% then L19170
            readkey$ =  str(part$,,25%) & str(rte$,,3%) & hex(000000)
            call "DELETE" (#1, readkey$, 28%)
            call "DELETE" (#7, readkey$, 28%)
            call "DELETE" (#10, readkey$, 28%)
            gosub L31000                  /* WRITE RTE RECORDS          */
L19170:     lastrte$ = "Last Mgd: " & part$ & " " & partdescr$ &         ~
                ", Rte " & rte$
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20120,         /* PART             */~
                                    L20160          /* WC ROUTE         */
                     return

L20120:    REM DEFAULT/ENABLE FOR PART
                message$ = "Enter Part To Manage Routing For.  Enter Blan~
        ~k or Partial to Search Part File."
                     return
L20160:    REM DEFAULT/ENABLE FOR WC ROUTE
                     gosub get_all_routes
                     if route$(1%) <> " " then L20230
                         information$ = " "
                         rte$ = "001"
                         goto L20290
L20230:              for y% = 1% to x%
                         if rfac$(y%) = hex(8d) then L20270
                         rte$ = route$(y%) : goto L20290
L20270:              next y%
                         rte$ = "001"
L20290:         message$ = "Select Which Route ID You Wish to Create or C~
        ~hange For This Part."
                return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   T A B L E      *~
            *                                                           *~
            * DEFAULTS/ENABLES FOR TABULAR INPUT. EVERY FIELD ENABLED,  *~
            * FIELD DESCRIPTION FLOWN ABOVE IN INFOMSG$.                *~
            *************************************************************

            deffn'162(fieldnr%, edit%)
                  call "ENABLSUB" ("SET", "RTEINPUT", scr%(), set%(), 1%,~
                                               fieldnr%, edit%, enabled%)
                  message$ = " "
                  on fieldnr% gosub L21230,         /* Step Number      */~
                                    L21400,         /* Work Center      */~
                                    L21440,         /* Move Queue Time  */~
                                    L21510,         /* Set Up Time      */~
                                    L21590,         /* Run Time         */~
                                    L21690,         /* Exp Proc Yield   */~
                                    L21750,         /* S.U. Activity Cod*/~
                                    L21790,         /* Activity Code    */~
                                    L21830,         /* Activity Descr   */~
                                    L21890,         /* % Comp at this st*/~
                                    L21940          /* Handling Factor  */

                     return

L21230:     REM DEFAULT/ENABLE FOR STEP NUMBER
                if step$(c%) <> " " then L21370
                     if c% <> 1% then goto L21280
                          convert defstepstart% to step$(c%), pic (0000)
                          goto L21370
L21280:              convert step$(c%-1%) to temp_b%, data goto L21281
L21281:              if c% < maxlines% then L21283
                          stepincrm% = defstepincrm% : goto L21290
L21283:              convert step$(c%+1%) to temp_a%, data goto L21284
L21284:              if temp_a% - temp_b% <= defstepincrm% then          ~
                          stepincrm% = (temp_a% - temp_b%) / 2% else     ~
                          stepincrm% = defstepincrm%
L21290:              temp% = temp_b% + stepincrm%
                     if temp% <= 9999% then goto L21300
                          errormsg$ = "Computed Step #s exceed 9999. Ma"&~
                               "nual entry required."
                          step$(c%) = "0000"
L21300:              convert temp% to step$(c%), pic (0000)
L21370:         message$ = "Step Is Used To Identify This Line"
                return

L21400:     REM DEFAULT/ENABLE FOR WORK CENTER
                message$="Work Center Code, or 'VEND' if outside Process"
                return

L21440:     REM DEFAULT/ENABLE FOR MOVE/QUEUE
                message$ = "Enter Move/Queue Days.  Put 'Y' in Box if M"&~
                           "/Q is After Step Rather than Before."
                if mq$(c%) <> " " then L21470
                    if defaults% <> 1% then L21470
                        if def_mq < 0 then L21480
                            call "CONVERT" (def_mq, 0.0, mq$(c%))
                            mqp$(c%) = def_mqp$
L21470:         call "STRING" addr("LJ", mq$(c%), 5%)
L21480:         if mqp$(c%) = " " then mqp$(c%) = "N"
                return

L21510:     REM DEFAULT/ENABLE FOR SETUP UNITS
                message$ = "Enter in Hours or *whole* WC Units.  Enter "&~
                           "Hours as HH.DDDD, HH:MM or HH:MM:SS"
                if su$(c%) = " " and suh$(c%) = " " then L21532 else L21540
L21532:             if defaults% <> 1% then L21540
                      if def_su < 0 then L21560
                        call "CONVERT" (def_su, 0.0, su$(c%))
                        call "WCUN2HRS" (#3, wc$(c%), 0, def_su, " ")
                        call "CONVERT" (def_su, 2.4, suh$(c%))
L21540:         call "STRING" addr("LJ", su$(c%), 6%)
                call "STRING" addr("LJ", suh$(c%), 8%)
L21560:         savesuh$ = suh$(c%)
                return

L21590:     REM DEFAULT/ENABLE FOR RUN UNITS
                message$ = "Enter in Hours (HH.DDDD, HH:MM or HH:MM:SS)"&~
                           ", WC Units (U.UUUU), or Parts/Hour."
                if run$(c%)= " " and runh$(c%)= " " then L21612 else L21622
L21612:             if defaults% <> 1% then L21622
                        if def_run < 0 then L21650
                            call "CONVERT" (def_run, 0.0, run$(c%))
                            call "WCUN2HRS" (#3, wc$(c%), 0, def_run, " ")
                            call "CONVERT" (def_run, 4.6, runh$(c%))
                            temp = 0
                            convert runh$(c%) to temp, data goto L21620
                            if temp <> 0 then temp = 1/temp
L21620:                     call "CONVERT" (temp, 0.6, runp$)
L21622:         call "STRING" addr("LJ", run$(c%), 6%)
                call "STRING" addr("LJ", runh$(c%), 8%)
                call "STRING" addr("LJ", runp$, 8%)
L21650:         saverun$ = run$(c%)
                saverunh$ = runh$(c%)
                return

L21690:    REM DEFAULT/ENABLE FOR EXPECTED PROCESS YIELD IN THIS STEP
               message$ = "Expected % Yield This Step (e.g.: 100, 92.3)"
               if yield$(c%) <> " " then L21720
                   if defaults% <> 1% then L21715
                       call "CONVERT" (def_yield, 0.0, yield$(c%))
L21715:        if yield$(c%) = " " then yield$(c%) = "100"
L21720:        call "STRING" addr("LJ", yield$(c%), 3%)
               return

L21750:    REM DEFAULT/ENABLE FOR SETUP ACTIVITY CODE
               message$ = "Enter Setup Activity Code"
               if sucode$(c%) <> " " then return
                   if defaults% <> 1% then return
                       sucode$(c%) = def_sucode$
               return

L21790:    REM DEFAULT/ENABLE FOR ACTIVITY CODE
               message$ = "Enter Activity Code"
               if actcode$(c%) <> " " then return
                   if defaults% <> 1% then return
                       actcode$(c%) = def_rucode$
                       actdescr$    = def_rudescr$
               return

L21830
*        DEFAULT/ENABLE FOR ACTIVITY DESCRIPTION
            if activity$(c%) <> " " then L21860
               if defaults% <> 1% then L21848
                   activity$(c%) = def_rudescr$
                   if activity$(c%) = " " then activity$(c%) = actdescr$
                   goto L21860
L21848:        tempkey$ = "WC ACTVTY" & actcode$(c%)
               call "GETCODE"(#12, tempkey$, activity$(c%),0%,99,f1%(12))
L21860:        message$ = "Enter Description Of Activity To Be Performed"
               return

L21890:    REM DEFAULT/ENABLE FOR % COMPLETE AT THIS STEP
               message$ = "% Product is Complete At This Step (Memo)"
               if comp$(c%) <> " " then L21920
                   if defaults% <> 1% then L21916
                      if def_comp < 0 then L21920
                       call "CONVERT" (def_comp, 0.0, comp$(c%))
L21916:        if comp$(c%) = " " then comp$(c%) = "0"
L21920:        call "STRING" addr("LJ", comp$(c%), 3%)
               return

L21940:    REM DEFAULT/ENABLE FOR HANDLING FACTOR
               message$ = "Enter Number Of Movement Units Per One Un" &  ~
                          "it Of The Part Being Built In The Job."
               if handfactor$(c%) <> " " then return
                   if defaults% <> 1% then L21955
                      call "CONVERT" (def_handfactor,-2.2,handfactor$(c%))
L21955:        if handfactor$(c%) = " " then handfactor$(c%) = "1.00"
               return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   T A B L E      *~
            *                                                           *~
            * DEFAULTS/ENABLES FOR TABULAR INPUT. EVERY FIELD ENABLED,  *~
            * FIELD DESCRIPTION FLOWN ABOVE IN INFOMSG$.                *~
            *************************************************************

            deffn'163(fieldnr%, edit%)
                  call "ENABLSUB" ("SET", "RTEINPUT", scr%(), set%(), 2%,~
                                               fieldnr%, edit%, enabled%)
                  message$ = " "
                  on fieldnr% gosub L22260,       /* Shift Differential */~
                                    L22340,       /* Concurrent WC One  */~
                                    L22380,       /* Capacity adj. Factr*/~
                                    L22480,       /* Activity Code      */~
                                    L22560,       /* Concurrent WC Two  */~
                                    L22610,       /* Capacity adj. Factr*/~
                                    L22710,       /* Activity Code      */~
                                    L22790,       /* Concurrent WC Three*/~
                                    L22840,       /* Capacity adj. Factr*/~
                                    L22940,       /* Activity Code      */~
                                    L23020,       /* Force Contiguity   */~
                                    L23080,       /* 1st Overlap Factor */~
                                    L23160        /* 2nd Overlap Factor */
                     return

L22260:     REM DEFAULT/ENABLE FOR SHIFT DIFFERENTIAL
                if wc$(c%) <> "VEND" then L22300
                     enabled% = 0
                     shift$(c%) = " "
L22300:         if shift$(c%) = " " then shift$(c%) = "1.00000"
                message$ = "Enter Shift Differential."
                return

L22340:     REM DEFAULT/ENABLE FOR CONCURRENT WC ONE
                message$ = "Enter First Concurrent Work Center Code"
                return

L22380:     REM DEFAULT/ENABLE FOR CAPACITY ADJ. FACTOR
                if wc1$(c%) <> " " then L22420
                     enabled% = 0
                     return
L22420:         message$ = "Enter .5 if this WC takes half as much Time a~
        ~s Primary, '1' if equal time, Etc."
                if nm1$(c%) = " " then nm1$(c%) = "1"
                call "STRING" addr("LJ", nm1$(c%), 6%)
                return

L22480:     REM DEFAULT/ENABLE FOR ACTIVITY CODE
                if wc1$(c%) <> " " then L22520
                     enabled% = 0
                     return
L22520:         message$ = "Enter Activity Code For This Concurrent WC"
                if ca1$(c%) = " " then ca1$(c%) = actcode$(c%)
                return

L22560:     REM DEFAULT/ENABLE FOR CONCURRENT WC TWO
                if wc1$(c%) = " " then enabled% = 0
                message$ = "Enter Second Concurrent Work Center Code"
                return

L22610:     REM DEFAULT/ENABLE FOR CAPACITY ADJ. FACTOR
                if wc2$(c%) <> " " then L22650
                     enabled% = 0
                     return
L22650:         message$ = "Enter .5 if this WC takes half as much Time a~
        ~s Primary, '1' if equal time, Etc."
                if nm2$(c%) = " " then nm2$(c%) = "1"
                call "STRING" addr("LJ", nm2$(c%), 6%)
                return

L22710:     REM DEFAULT/ENABLE FOR ACTIVITY CODE
                if wc2$(c%) <> " " then L22750
                     enabled% = 0
                     return
L22750:         message$ = "Enter Activity Code For This Concurrent WC"
                if ca2$(c%) = " " then ca2$(c%) = actcode$(c%)
                return

L22790:     REM DEFAULT/ENABLE FOR CONCURRENT WC THREE
                if wc2$(c%) = " " then enabled% = 0
                message$ = "Enter Third Concurrent Work Center Code"
                return

L22840:     REM DEFAULT/ENABLE FOR CAPACITY ADJ. FACTOR
                if wc3$(c%) <> " " then L22880
                     enabled% = 0
                     return
L22880:         message$ = "Enter .5 if this WC takes half as much Time a~
        ~s Primary, '1' if equal time, Etc."
                if nm3$(c%) = " " then nm3$(c%) = "1"
                call "STRING" addr("LJ", nm3$(c%), 6%)
                return

L22940:     REM DEFAULT/ENABLE FOR ACTIVITY CODE
                if wc3$(c%) <> " " then L22980
                     enabled% = 0
                     return
L22980:         message$ = "Enter Activity Code For This Concurrent WC"
                if ca3$(c%) = " " then ca3$(c%) = actcode$(c%)
                return

L23020:    REM DEFAULT/ENABLE FOR FORCE CONTIGUITY
               if cg$(c%) = " " then cg$(c%) = "N"
               message$ = "Enter 'Y' To Disallow Any Gap Between This Ste~
        ~p And The Previous Step."
               return

L23080:    REM DEFAULT/ENABLE FOR FIRST OVERLAP FACTOR
               if f1$(c%,1%) <> " " then L23120
                  f1$(c%,1%) = "0"
                  f1$(c%,2%) = "100"
L23120:        call "STRING" addr("LJ", f1$(c%,1%), 8%)
               call "STRING" addr("LJ", f1$(c%,2%), 8%)
               goto L23220

L23160:    REM DEFAULT/ENABLE FOR SECOND OVERLAP FACTOR
               if f2$(c%,1%) <> " " then L23200
                  f2$(c%,1%) = "0"
                  f2$(c%,2%) = "100"
L23200:        call "STRING" addr("LJ", f2$(c%,1%), 8%)
               call "STRING" addr("LJ", f2$(c%,2%), 8%)
L23220:        message$ = "Enter Qty and/or Percent Of Less Than 100 To A~
        ~llow Step Overlap When Planning."
               return

        REM *************************************************************~
            * Here are some subroutines for copying Alternates, etc.    *~
            *************************************************************

        determine_copy_alternates
            copyalts% = 2%                         /* Window at bottom */
            call "ASKUSER" (copyalts%, "*** ALTERNATES TOO? ***",        ~
                "Press PF(16) to COPY the Alternate W/C's also.",        ~
                "Press PF(8) to NOT copy the Alternate W/C's.",          ~
                "Press PF(1) to ABORT copy.")
            if copyalts% =  8% then return    /* DON'T copy alternates */
            if copyalts% =  1% then return     /* Abort copy operation */
            if copyalts% = 16% then return       /* DO copy alternates */
            goto determine_copy_alternates         /* Invalid response */

        copy_alternates/* Alternate W/Cs for the copied rte are copied */
            altrsky$ = xor altrsky$
            str(altrsky$,,31%) = readkey$

L24190:     call "PLOWNEXT" (#7, altrsky$, 31%, f1%(7%))
                if f1%(7%) = 0% then return

*        Got an Alternate for the Step record read. Store the Alternate.
            altmax% = altmax% + 1%  /* 1st, see if there's room for it */
            altind% = altind% + 1%
            if altmax% <= altrs_allowed% then goto L24290
                altmax% = altmax% - 1%
                errormsg$ = "Not enough room to copy all Alternates."
                return

L24290
*        Push the Alt array elements up to make a hole for the new one.
            if altmax% = 1% then goto L24480
            if altmax% = altind% then goto L24480
            for alt% = altmax% to altind% step -1%
                awc0$  (alt%) = awc0$  (alt%-1%)
                awc1$  (alt%) = awc1$  (alt%-1%)
                awc2$  (alt%) = awc2$  (alt%-1%)
                awc3$  (alt%) = awc3$  (alt%-1%)
                amq0$  (alt%) = amq0$  (alt%-1%)
                asu0$  (alt%) = asu0$  (alt%-1%)
                arn0$  (alt%) = arn0$  (alt%-1%)
                asht$  (alt%) = asht$  (alt%-1%)
                anm1$  (alt%) = anm1$  (alt%-1%)
                anm2$  (alt%) = anm2$  (alt%-1%)
                anm3$  (alt%) = anm3$  (alt%-1%)
                amqp$  (alt%) = amqp$  (alt%-1%)
                altind%(alt%) = altind%(alt%-1%)
            next alt%

L24480
*        OK, we've got a place to put it. So put it there.
            altind%(altind%) = c%
            gosub'200(altind%)
            goto L24190

        read_switches_rte_rec
            call "READ100" (#20, swrtekey$, f1%(20%))
            if f1%(20%) <> 0% then get #20 using L24570, swrtekey$,       ~
                defstepstart%, defstepincrm%
L24570:         FMT CH(20), 2*BI(4), CH(216), CH(256)
            return

        REM *************************************************************~
            *         I N I T I A L I Z E   E N A B L E S               *~
            * --------------------------------------------------------- *~
            * Initialize Soft Enable Settings.                          *~
            *************************************************************

        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer

            scr%(1, 1) =  1% : set%( 1) = 2%       /* Step Number      */
            scr%(1, 2) =  2% : set%( 2) = 2%       /* Work Center      */
            scr%(1, 3) =  3% : set%( 3) = 2%       /* Move Queue Time  */
            scr%(1, 4) =  4% : set%( 4) = 2%       /* Set Up Time      */
            scr%(1, 5) =  5% : set%( 5) = 2%       /* Run Time         */
            scr%(1, 6) =  6% : set%( 6) = 2%       /* Exp Proc Yield   */
            scr%(1, 7) =  7% : set%( 7) = 2%       /* S.U. Activity Cod*/
            scr%(1, 8) =  8% : set%( 8) = 2%       /* Activity Code    */
            scr%(1, 9) =  9% : set%( 9) = 2%       /* Activity Descr   */
            scr%(1,10) = 10% : set%(10) = 1%       /* % Comp at this st*/

            scr%(2, 1) = 11% : set%(11) = 1%     /* Shift Differential */
            scr%(2, 2) = 12% : set%(12) = 1%     /* Concurrent WC One  */
            scr%(2, 3) = 13% : set%(13) = 2%     /* Capacity adj. Factr*/
            scr%(2, 4) = 14% : set%(14) = 2%     /* Activity Code      */
            scr%(2, 5) = 15% : set%(15) = 1%     /* Concurrent WC Two  */
            scr%(2, 6) = 16% : set%(16) = 2%     /* Capacity adj. Factr*/
            scr%(2, 7) = 17% : set%(17) = 2%     /* Activity Code      */
            scr%(2, 8) = 18% : set%(18) = 1%     /* Concurrent WC Three*/
            scr%(2, 9) = 19% : set%(19) = 2%     /* Capacity adj. Factr*/
            scr%(2,10) = 20% : set%(20) = 2%     /* Activity Code      */
            scr%(2,11) = 21% : set%(21) = 2%     /* Force Contiguity   */
            scr%(2,12) = 22% : set%(22) = 2%     /* 1st Overlap Factor */
            scr%(2,13) = 23% : set%(23) = 2%     /* 2nd Overlap Factor */
            scr%(1,11) = 24% : set%(24) = 2%       /* Handling factor  */

            call "ENABLSUB" ("INIT", "RTEINPUT", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************

        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% = 1% then call "ENABLSUB" ("MODIFY", "RTEINPUT",   ~
                                          scr%(), set%(), s%, f%, 0%, 0%)
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29918:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                if u3% <> 0% then L29918

*        Wants to Start Over
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *        L O A D   O L D   R T E   F R O M   F I L E        *~
            *                                                           *~
            * THIS ROUTINE LOADS THE OLD WC ROUTE          FROM THE     *~
            * FILE, RETURNS IF THERE IS NOT ONE.                        *~
            *************************************************************

            altmax%, maxlines%, c% = 0%
            readkey$ = str(part$,,25%) & str(rte$,,3%) & " "
            call "GETCODE" (#2, part$, partdescr$, 1%, 99, f1%(2%))

*        Set PF Key prompt for ECR Inquiry (if any ECRs)
            ecrpfk$ = "(11)"
            call "ECRINQSB" ("C",        /* "C" to Check for ECR Info  */~
                                         /*    and return PFKey Prompt */~
                             part$,      /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                                         /* OUT: Formatted PFKey Prompt*/~
                                         /*    Will be BLANK if no ECRs*/~
                             #20,        /* SYSFILE2                   */~
                             #02)        /* HNYMASTR                   */

L30099:     call "PLOWNEXT" (#1, readkey$, 28%, f1%(1%))
                 if f1%(1%) = 0% then L30693
            c%, maxlines% = c% + 1%
            if c% = 1% then print at(4,1,80); hex(84); "Loading Data..."
            gosub load_rte_line
            goto L30099

        load_rte_line
            get #1, using L30450, wc$(c%), mq, su, run, wc1$(c%), nm1,    ~
                    wc2$(c%), nm2, wc3$(c%), nm3, shift, mqp$(c%),       ~
                    yield, step$(c%), comp, actcode$(c%), activity$(c%), ~
                    textid$(101), textid$(c%), ca1$(c%), ca2$(c%),       ~
                    ca3$(c%), sucode$(c%), cg$(c%), f11, f12, f21, f22,  ~
                    handfactor

            if copymode% = 0% then                                       ~
                       call "TXTFUTIL" (#9, f2%(9), "LOAD", textid$(c%)) ~
                          else                                           ~
                       call "TXTFUTIL" (#9, f2%(9), "COPY", textid$(c%))
            call "CONVERT" (mq,    0.0, mq$(c%))
            call "CONVERT" (su,    0.0, su$(c%))
            call "CONVERT" (run,   0.4, run$(c%))
                REM Now convert to hours...
                call "WCUN2HRS" (#3, wc$(c%), 0, su, " ")
                call "WCUN2HRS" (#3, wc$(c%), 0, run, " ")
                call "CONVERT" (su,    2.4, suh$(c%))
                call "CONVERT" (run,   4.6, runh$(c%))
            call "CONVERT" (comp,  0.0, comp$(c%))
            call "CONVERT" (handfactor ,  2.2, handfactor$(c%))
            call "CONVERT" (yield, 0.0, yield$(c%))
            call "CONVERT" (f11, 0.2, f1$(c%,1))
            call "CONVERT" (f12, 0.2, f1$(c%,2))
            call "CONVERT" (f21, 0.2, f2$(c%,1))
            call "CONVERT" (f22, 0.2, f2$(c%,2))
            if mqp$(c%) = " " then mqp$(c%) = "N"
            if shift = 0 then shift = 1
            call "CONVERT" (shift, -5.5, shift$(c%))
            if wc1$(c%) <> " " then call "CONVERT" (nm1, -0.4, nm1$(c%))
            if wc2$(c%) <> " " then call "CONVERT" (nm2, -0.4, nm2$(c%))
            if wc3$(c%) <> " " then call "CONVERT" (nm3, -0.4, nm3$(c%))
            return

L30450:     FMT CH(04),                  /* WORK CENTER                */~
                XX(25),                  /* PART                       */~
                XX(3),                   /* WC ROUTE                   */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                BI(4),                   /* MOVE QUEUE TIME IN DAYS    */~
                BI(2),                   /* SET UP TIME IN HOURS       */~
                PD(14,4),                /* RUN TIME IN HOURS          */~
                CH(4),                   /* CONCURRENT WC ONE          */~
                PD(7,4),                 /* WC CAPACITY ADJ. FACTOR    */~
                CH(4),                   /* CONCURRENT WC TWO          */~
                PD(7,4),                 /* WC CAPACITY ADJ. FACTOR    */~
                CH(4),                   /* CONCURRENT WC THREE        */~
                PD(7,4),                 /* WC CAPACITY ADJ. FACTOR    */~
                PD(7,6),                 /* WC SHIFT DIFERENTIAL       */~
                CH(1),                   /* MOVE QUE OPTION            */~
                BI(1),                   /* PROCESS YIELD EXPECTED %   */~
                XX(8),                   /* Effective yield (internal) */~
                CH(4),                   /* STEP CODE                  */~
                BI(1),                   /* % COMPLETE AT THIS STEP    */~
                CH(4),                   /* ACTIVITY CODE              */~
                CH(30),                  /* ACTIVITY PERFORMED         */~
                CH(4),                   /* Header Text ID             */~
                CH(4),                   /* Line Text ID               */~
                4*CH(4),                 /* Conc WC Act / Setup WC Act */~
                CH(1),                   /* Force Contiguity           */~
                4*PD(14,4),              /* Overlap factors            */~
                PD(14,4)                 /* Handling Factor            */~

L30693:     if maxlines% = 0% then return
            call "TXTFUTIL" (#9, f2%(9), "LOAD", textid$(101))
            readkey$ = str(part$,,25) & str(rte$,,3)

L30729:     call "PLOWNEXT" (#7, readkey$, 28%, f1%(7%))
                 if f1%(7%) = 0% then return
            c%, altmax% = altmax% + 1%
            if altmax% <= altrs_allowed% then goto L30783
                c%, altmax% = altmax% - 1%
                return
L30783:     gosub'200(c%)
            convert seq$ to altind%(c%), data goto L30792
L30792:     goto L30729

        deffn'200(x%)
*        Get the RTEALTRS data fields and put them in the designated
*        array element (element 'X%').
            get #7, using L30945, seq$, awc0$(x%), mq, su, run, awc1$(x%),~
                nm1, awc2$(x%), nm2, awc3$(x%), nm3, shift, amqp$(x%)
            call "CONVERT" (mq,    0.0, amq0$(x%))
            call "CONVERT" (su,    0.0, asu0$(x%))
            call "CONVERT" (run,   0.4, arn0$(x%))
            if shift = 0 then shift = 1
            call "CONVERT" (shift, -5.5, asht$(x%))
            if awc1$(x%)<>" " then call "CONVERT" (nm1, -0.4, anm1$(x%))
            if awc2$(x%)<>" " then call "CONVERT" (nm2, -0.4, anm2$(x%))
            if awc3$(x%)<>" " then call "CONVERT" (nm3, -0.4, anm3$(x%))
            return

L30945:     FMT XX(28), CH(3), XX(3), CH(4), BI(4), BI(2), PD(14,4),     ~
                CH(4), PD(7,4), CH(4), PD(7,4), CH(4), PD(7,4), PD(7,6), ~
                CH(1)

L31000: REM *************************************************************~
            *             W R I T E   R T E   T O   F I L E             *~
            *                                                           *~
            *    WRITES THE NEW ROUTING RECORDS TO THE RTEMASTR FILE    *~
            *                                                           *~
            *************************************************************

            print at(4,1,80); hex(84); "Saving Data..."
            if maxlines% = 0% then return

            REM Calculate Normalized Yield Percentages...
            mat nyield = con
            mat nyield = (100)*nyield
            for i% = 1% to maxlines%
                convert yield$(i%) to nyield(i%), data goto L31150
L31150:     next i%

            if maxlines% < 2% then L31220
            for i% = maxlines% - 1% to 1% step -1%
                nyield(i%) = nyield(i%) * nyield(i%+1%)/100
            next i%

L31220:     for temp% = 1 to maxlines%
                mq, su, run, comp, f11, f12, f21, f22 = 0 : yield = 100
                shift, nm1, nm2, nm3 = 1
                convert temp% to seqnr$, pic(###)
                convert mq$(temp%)    to mq,    data goto L31270
L31270:         convert su$(temp%)    to su,    data goto L31280
L31280:         convert run$(temp%)   to run,   data goto L31290
L31290:         convert comp$(temp%)  to comp,  data goto L31291
L31291:         convert handfactor$(temp%) to handfactor, data goto L31300
L31300:         convert yield$(temp%) to yield, data goto L31310
L31310:         convert shift$(temp%) to shift, data goto L31320
L31320:         convert nm1$(temp%)   to nm1,   data goto L31330
L31330:         convert nm2$(temp%)   to nm2,   data goto L31340
L31340:         convert nm3$(temp%)   to nm3,   data goto L31341
L31341:         convert f1$(temp%, 1) to f11,   data goto L31342
L31342:         convert f1$(temp%, 2) to f12,   data goto L31343
L31343:         convert f2$(temp%, 1) to f21,   data goto L31344
L31344:         convert f2$(temp%, 2) to f22,   data goto L31350
L31350:         if mqp$(temp%) <> "Y" then mqp$(temp%) = " "
                write #1, using L31820, wc$(temp%), part$, rte$, seqnr$,  ~
                          mq, su, run, wc1$(temp%), nm1, wc2$(temp%),    ~
                          nm2, wc3$(temp%), nm3, shift, mqp$(temp%),     ~
                          yield, nyield(temp%), step$(temp%), comp,      ~
                          actcode$(temp%), activity$(temp%),             ~
                          textid$(101), textid$(temp%), ca1$(temp%),     ~
                          ca2$(temp%), ca3$(temp%), sucode$(temp%),      ~
                          cg$(temp%), f11, f12, f21, f22, handfactor, " "
                write #10, using L32080, part$, rte$, step$(temp%),       ~
                                        seqnr$, " "

                if actcode$(temp%) = " " then L31520
                     write #12 using L31770, "WC ACTVTY", actcode$(temp%),~
                                  activity$(temp%), " " , eod goto L31520
L31520:         if sucode$(temp%) = " " then L31580
                     write #12 using L31770, "WC ACTVTY", sucode$(temp%), ~
                                  activity$(temp%), " " ,  eod goto L31580
L31580:         if ca1$(temp%) = " " then L31640
                     write #12 using L31770, "WC ACTVTY", ca1$(temp%),    ~
                                  activity$(temp%), " " ,  eod goto L31640
L31640:         if ca2$(temp%) = " " then L31700
                     write #12 using L31770, "WC ACTVTY", ca2$(temp%),    ~
                                  activity$(temp%), " " ,  eod goto L31700
L31700:         if ca3$(temp%) = " " then L31780
                     write #12 using L31770, "WC ACTVTY", ca3$(temp%),    ~
                                  activity$(temp%), " " ,  eod goto L31780
                          FMT CH(4), CH(30), CH(66)
L31770:                   FMT CH(9), CH(15), CH(30), CH(74)
L31780:     next temp%
            call "TXTFUTIL" (#9, f2%(9), "SAV2", "    ")
            goto L32140

L31820:     FMT CH(04),                  /* WORK CENTER                */~
                CH(25),                  /* PART                       */~
                CH(3),                   /* WC ROUTE                   */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                BI(4),                   /* MOVE QUEUE TIME IN DAYS    */~
                BI(2),                   /* SET UP TIME IN HOURS       */~
                PD(14,4),                /* RUN TIME IN HOURS          */~
                CH(4),                   /* CONCURRENT WC ONE          */~
                PD(7,4),                 /* WC CAPACITY ADJ. FACTOR    */~
                CH(4),                   /* CONCURRENT WC TWO          */~
                PD(7,4),                 /* WC CAPACITY ADJ. FACTOR    */~
                CH(4),                   /* CONCURRENT WC THREE        */~
                PD(7,4),                 /* WC CAPACITY ADJ. FACTOR    */~
                PD(7,6),                 /* WC SHIFT DIFERENTIAL       */~
                CH(1),                   /* MOVE QUE OPTION            */~
                BI(1),                   /* PROCESS YIELD EXPECTED %   */~
                PD(14,7),                /* Effective yield (internal) */~
                CH(4),                   /* STEP CODE                  */~
                BI(1),                   /* % COMPLETE AT THIS STEP    */~
                CH(4),                   /* ACTIVITY CODE              */~
                CH(30),                  /* ACTIVITY PERFORMED         */~
                CH(4),                   /* Header Text ID             */~
                CH(4),                   /* Line Text ID               */~
                3*CH(4),                 /* Concurrent WC Activity Cdes*/~
                CH(4),                   /* Setup WC Activity Codes    */~
                CH(1),                   /* Force Contiguity Flag      */~
                4*PD(14,4),              /* Overlap factors            */~
                PD(14,4),                /* Handling Factor            */~
                CH(209)                  /* Filler                     */

L32080:     FMT CH(25),                  /* PART                       */~
                CH(3),                   /* WC ROUTE                   */~
                CH(04),                  /* STEP NUMBER                */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(15)                   /* Filler                     */

L32140:     if altmax% = 0% then return
            mat edtalt% = zer
            for i% = 1% to altmax%
                if altind%(i%) < 1% or altind%(i%) > maxlines% then L32310
                edtalt%(altind%(i%))=edtalt%(altind%(i%)) + 1%
                mq, su, run = 0 : shift, nm1, nm2, nm3 = 1
                convert amq0$(i%) to mq,    data goto L32210
L32210:         convert asu0$(i%) to su,    data goto L32220
L32220:         convert arn0$(i%) to run,   data goto L32230
L32230:         convert asht$(i%) to shift, data goto L32240
L32240:         convert anm1$(i%) to nm1,   data goto L32250
L32250:         convert anm2$(i%) to nm2,   data goto L32260
L32260:         convert anm3$(i%) to nm3,   data goto L32270
L32270:            write #7, using L32340, part$, rte$, altind%(i%),      ~
                             edtalt%(altind%(i%)), awc0$(i%), mq, su,    ~
                             run, awc1$(i%), nm1, awc2$(i%), nm2,        ~
                             awc3$(i%), nm3, shift, amqp$(i%), " "
L32310:     next i%
            return

L32340:     FMT CH(25),                  /* PART                       */~
                CH( 3),                  /* ROUTE                      */~
                PIC(###),                /* ROUTE SEQUENCE             */~
                PIC(###),                /* ALTERNATE SEQUENCE         */~
                CH(04),                  /* WORK CENTER                */~
                BI(4),                   /* MOVE QUEUE TIME IN DAYS    */~
                BI(2),                   /* SET UP TIME IN HOURS       */~
                PD(14,4),                /* RUN TIME IN HOURS          */~
                CH(4),                   /* CONCURRENT WC ONE          */~
                PD(7,4),                 /* WC CAPACITY ADJ. FACTOR    */~
                CH(4),                   /* CONCURRENT WC TWO          */~
                PD(7,4),                 /* WC CAPACITY ADJ. FACTOR    */~
                CH(4),                   /* CONCURRENT WC THREE        */~
                PD(7,4),                 /* WC CAPACITY ADJ. FACTOR    */~
                PD(7,6),                 /* WC SHIFT DIFERENTIAL       */~
                CH(1),                   /* MOVE QUE OPTION            */~
                CH(69)                   /* Filler                     */

        check_usage
            used% = 1%
            errormsg$ = "Route has been Used For Planning."
            readkey$  = str(part$,,25%) &  str(rte$,,3%)
            call "PLOWALTS" (#6, readkey$,1%,28%, f1%(6%))/*IN JBCROSS2?*/
            if f1%(6%) = 1% then return

            readkey$ = str(part$,,25%) &  str(rte$,,3%)
            call "PLOWALTS" (#11, readkey$, 2%, 28%, f1%(11%))
            if f1%(11%) = 0% then L33070
                errormsg$ = "Route is referenced in Cost Set: " &        ~
                    str(readkey$,29%)
                return

L33070:     used% = 0%  :  errormsg$ = " "

            readkey$ =  part$
L33090:     call "PLOWNEXT" (#4, readkey$, 25%, f1%(4%))/* IN BOMMASTR? */
            if f1%(4%) = 0% then return
              if str(readkey$,29%,3%) <> "  0" then return
                get #4, using L33120, temp$
L33120:             FMT POS(87), CH(3)
                if temp$ <> rte$ then L33150
                    information$ = "MODIFICATION WARNING: Route is " &   ~
                                   "tied To BOM: " & str(readkey$,26,3)
                     return
L33150:         str(readkey$,29%,3%) = hex(ffffff)
                goto L33090

        get_all_routes
            init(hex(00)) rtekey$
            str(rtekey$,,25%) = part$
            rfac$() = all(hex(8c))
            route$() = " "
            x% = 0%

L34070:     call "PLOWNEXT" (#1, rtekey$, 25%, f1%(1%))
                  if f1%(1%) <> 1% then L34170
                  x% = min(x% + 1%, dim(route$(), 1%))
            rte$, route$(x%) = str(rtekey$,26%,3%)
                  gosub check_usage
                  if used% = 1% then rfac$(x%) = hex(8d)                 ~
                                else rfac$(x%) = hex(84)
            str(rtekey$,29%,31%) = all(hex(ff))
                  goto L34070

L34170:     information$ = "Routes listed below that are" & hex(84) &    ~
                           "highlighted" & hex(8d) & "CAN be modified."
            errormsg$ = " "

*        Now find the effective BOM/Route
            call "BOMFIND" (part$, date, #5, #20, temp$)
            if temp$ = " " then L34295
                bomkey$ = str(part$) & str(temp$,,3%) & "  0"
                call "READ100" (#4, bomkey$, f1%(4%))
                if f1%(4%) = 0% then L34295
                    get #04 using L34270, eff_rte$
L34270:                 FMT POS(87), CH(3)
                    if eff_rte$ = " " then L34295
                    return
L34295:     eff_rte$ = "None"
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(84)) lfac$()
                header$ = " "
                str(line2$,,61%) = "Part # and Route Selection"
                pfktext$(1) = "(1)Start Over     (4)Previous Field  (8)Vi~
        ~ew BOM Effectivity   (13)Instructions"
                pfktext$(2) = "                  (9)Examine BOM/RTE/STC R~
        ~elationships         (15)Print Screen"
                pfktext$(3) = "(17)Step Defaults (14)Find Existing Route ~
        ~For Edit             (16)Exit Program"
                pfkeys$ = hex(00010408090d0e0f10110bff)
                if ecrpfk$ = " " then str(pfkeys$,11%,1%) = hex(ff)
                str(pfktext$(2%),,14%) = ecrpfk$
                if admin% <> 0% then goto L36170
                     str(pfktext$(3%),,17%) = " "
                     str(pfkeys$,10%,1%) = hex(ff)
L36170:         if fieldnr% > 0% then L36320
                  REM Editmode Logic...
                  init(hex(9c)) rfac$()
                  pfktext$(1) = "(1)Start Over                        (8)~
        ~View BOM Effectivity   (13)Instructions"
                  pfktext$(2) = "(2)Manage Route Steps                   ~
        ~                       (15)Print Screen"
                  pfktext$(3) = "(17)Step Defaults                       ~
        ~ (25)Route Free Text   (16)Save Data"
                  str(pfktext$(2%),23%,14%) = ecrpfk$
                  pfkeys$ = hex(000102080d0f10ff19110bff)
                  if ecrpfk$ = " " then str(pfkeys$,11%,1%) = hex(ff)
                if admin% <> 0% then goto L36270
                     str(pfktext$(3%),,17%) = " "
                     str(pfkeys$,10%,1%) = hex(ff)
L36270:           if admin% <> 1% or noedit% <> 1% then goto L36460
                    str(pfkeys$,8%,1%) = hex(18) /* Enable PF(24) */
                    str(pfktext$(2%),42%,19%) = "(24)Override NO MOD"
                    goto L36460

L36320:         REM Inputmode Logic...
                if fieldnr% > 1% then L36430
                  str(pfktext$(1),19%,42%) = " " /* Shut Off Prev Field */
                  str(pfktext$(2),01%,55%) = " " /* Shut Off Inquiry    */
                  str(pfkeys$,3%,3%) = all(hex(ff))
                  str(pfkeys$,11%,1%) = hex(ff)
                  init(hex(8c)) rfac$()
                  if lastrte$ <> " " then header$ = lastrte$
                  goto L36460

L36430:        str(pfktext$(3),64) = " "        /* Shut Off Exit       */
               str(pfkeys$,9%,1%) = hex(ff)

L36460
*        Continue preparation for ACCEPT statement.
               on fieldnr% gosub L36540,            /* PART             */~
                                 L36540             /* WC ROUTE         */
                     goto L36610

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L36540:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L36610:     accept                                                       ~
               at (01,02), "Manage Production Work Center Routings",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), "Part to Produce",                            ~
               at (05,18), fac(lfac$( 1%)), part$               , ch(25),~
               at( 05,44), fac(hex(8c)), partdescr$             , ch(34),~
               at (06,02), "Route Vrsn ID.",                             ~
               at (06,18), fac(lfac$( 2%)), rte$                , ch(03),~
               at (06,45), "Effective Route: ",                          ~
               at( 06,62), fac(hex(8c)), eff_rte$               , ch(04),~
                                                                         ~
               at (08,02), fac(hex(8d)), information$           , ch(79),~
                                                                         ~
               at (10,02), fac(rfac$(  1%)), route$(  1%)       , ch(03),~
               at (10,06), fac(rfac$(  2%)), route$(  2%)       , ch(03),~
               at (10,10), fac(rfac$(  3%)), route$(  3%)       , ch(03),~
               at (10,14), fac(rfac$(  4%)), route$(  4%)       , ch(03),~
               at (10,18), fac(rfac$(  5%)), route$(  5%)       , ch(03),~
               at (10,22), fac(rfac$(  6%)), route$(  6%)       , ch(03),~
               at (10,26), fac(rfac$(  7%)), route$(  7%)       , ch(03),~
               at (10,30), fac(rfac$(  8%)), route$(  8%)       , ch(03),~
               at (10,34), fac(rfac$(  9%)), route$(  9%)       , ch(03),~
               at (10,38), fac(rfac$( 10%)), route$( 10%)       , ch(03),~
               at (10,42), fac(rfac$( 11%)), route$( 11%)       , ch(03),~
               at (10,46), fac(rfac$( 12%)), route$( 12%)       , ch(03),~
               at (10,50), fac(rfac$( 13%)), route$( 13%)       , ch(03),~
               at (10,54), fac(rfac$( 14%)), route$( 14%)       , ch(03),~
               at (10,58), fac(rfac$( 15%)), route$( 15%)       , ch(03),~
               at (10,62), fac(rfac$( 16%)), route$( 16%)       , ch(03),~
               at (10,66), fac(rfac$( 17%)), route$( 17%)       , ch(03),~
               at (10,70), fac(rfac$( 18%)), route$( 18%)       , ch(03),~
               at (10,74), fac(rfac$( 19%)), route$( 19%)       , ch(03),~
               at (10,78), fac(rfac$( 20%)), route$( 20%)       , ch(03),~
                                                                         ~
               at (11,02), fac(rfac$( 21%)), route$( 21%)       , ch(03),~
               at (11,06), fac(rfac$( 22%)), route$( 22%)       , ch(03),~
               at (11,10), fac(rfac$( 23%)), route$( 23%)       , ch(03),~
               at (11,14), fac(rfac$( 24%)), route$( 24%)       , ch(03),~
               at (11,18), fac(rfac$( 25%)), route$( 25%)       , ch(03),~
               at (11,22), fac(rfac$( 26%)), route$( 26%)       , ch(03),~
               at (11,26), fac(rfac$( 27%)), route$( 27%)       , ch(03),~
               at (11,30), fac(rfac$( 28%)), route$( 28%)       , ch(03),~
               at (11,34), fac(rfac$( 29%)), route$( 29%)       , ch(03),~
               at (11,38), fac(rfac$( 30%)), route$( 30%)       , ch(03),~
               at (11,42), fac(rfac$( 31%)), route$( 31%)       , ch(03),~
               at (11,46), fac(rfac$( 32%)), route$( 32%)       , ch(03),~
               at (11,50), fac(rfac$( 33%)), route$( 33%)       , ch(03),~
               at (11,54), fac(rfac$( 34%)), route$( 34%)       , ch(03),~
               at (11,58), fac(rfac$( 35%)), route$( 35%)       , ch(03),~
               at (11,62), fac(rfac$( 36%)), route$( 36%)       , ch(03),~
               at (11,66), fac(rfac$( 37%)), route$( 37%)       , ch(03),~
               at (11,70), fac(rfac$( 38%)), route$( 38%)       , ch(03),~
               at (11,74), fac(rfac$( 39%)), route$( 39%)       , ch(03),~
               at (11,78), fac(rfac$( 40%)), route$( 40%)       , ch(03),~
                                                                         ~
               at (12,02), fac(rfac$( 41%)), route$( 41%)       , ch(03),~
               at (12,06), fac(rfac$( 42%)), route$( 42%)       , ch(03),~
               at (12,10), fac(rfac$( 43%)), route$( 43%)       , ch(03),~
               at (12,14), fac(rfac$( 44%)), route$( 44%)       , ch(03),~
               at (12,18), fac(rfac$( 45%)), route$( 45%)       , ch(03),~
               at (12,22), fac(rfac$( 46%)), route$( 46%)       , ch(03),~
               at (12,26), fac(rfac$( 47%)), route$( 47%)       , ch(03),~
               at (12,30), fac(rfac$( 48%)), route$( 48%)       , ch(03),~
               at (12,34), fac(rfac$( 49%)), route$( 49%)       , ch(03),~
               at (12,38), fac(rfac$( 50%)), route$( 50%)       , ch(03),~
               at (12,42), fac(rfac$( 51%)), route$( 51%)       , ch(03),~
               at (12,46), fac(rfac$( 52%)), route$( 52%)       , ch(03),~
               at (12,50), fac(rfac$( 53%)), route$( 53%)       , ch(03),~
               at (12,54), fac(rfac$( 54%)), route$( 54%)       , ch(03),~
               at (12,58), fac(rfac$( 55%)), route$( 55%)       , ch(03),~
               at (12,62), fac(rfac$( 56%)), route$( 56%)       , ch(03),~
               at (12,66), fac(rfac$( 57%)), route$( 57%)       , ch(03),~
               at (12,70), fac(rfac$( 58%)), route$( 58%)       , ch(03),~
               at (12,74), fac(rfac$( 59%)), route$( 59%)       , ch(03),~
               at (12,78), fac(rfac$( 60%)), route$( 60%)       , ch(03),~
                                                                         ~
               at (13,02), fac(rfac$( 61%)), route$( 61%)       , ch(03),~
               at (13,06), fac(rfac$( 62%)), route$( 62%)       , ch(03),~
               at (13,10), fac(rfac$( 63%)), route$( 63%)       , ch(03),~
               at (13,14), fac(rfac$( 64%)), route$( 64%)       , ch(03),~
               at (13,18), fac(rfac$( 65%)), route$( 65%)       , ch(03),~
               at (13,22), fac(rfac$( 66%)), route$( 66%)       , ch(03),~
               at (13,26), fac(rfac$( 67%)), route$( 67%)       , ch(03),~
               at (13,30), fac(rfac$( 68%)), route$( 68%)       , ch(03),~
               at (13,34), fac(rfac$( 69%)), route$( 69%)       , ch(03),~
               at (13,38), fac(rfac$( 70%)), route$( 70%)       , ch(03),~
               at (13,42), fac(rfac$( 71%)), route$( 71%)       , ch(03),~
               at (13,46), fac(rfac$( 72%)), route$( 72%)       , ch(03),~
               at (13,50), fac(rfac$( 73%)), route$( 73%)       , ch(03),~
               at (13,54), fac(rfac$( 74%)), route$( 74%)       , ch(03),~
               at (13,58), fac(rfac$( 75%)), route$( 75%)       , ch(03),~
               at (13,62), fac(rfac$( 76%)), route$( 76%)       , ch(03),~
               at (13,66), fac(rfac$( 77%)), route$( 77%)       , ch(03),~
               at (13,70), fac(rfac$( 78%)), route$( 78%)       , ch(03),~
               at (13,74), fac(rfac$( 79%)), route$( 79%)       , ch(03),~
               at (13,78), fac(rfac$( 80%)), route$( 80%)       , ch(03),~
                                                                         ~
               at (14,02), fac(rfac$( 81%)), route$( 81%)       , ch(03),~
               at (14,06), fac(rfac$( 82%)), route$( 82%)       , ch(03),~
               at (14,10), fac(rfac$( 83%)), route$( 83%)       , ch(03),~
               at (14,14), fac(rfac$( 84%)), route$( 84%)       , ch(03),~
               at (14,18), fac(rfac$( 85%)), route$( 85%)       , ch(03),~
               at (14,22), fac(rfac$( 86%)), route$( 86%)       , ch(03),~
               at (14,26), fac(rfac$( 87%)), route$( 87%)       , ch(03),~
               at (14,30), fac(rfac$( 88%)), route$( 88%)       , ch(03),~
               at (14,34), fac(rfac$( 89%)), route$( 89%)       , ch(03),~
               at (14,38), fac(rfac$( 90%)), route$( 90%)       , ch(03),~
               at (14,42), fac(rfac$( 91%)), route$( 91%)       , ch(03),~
               at (14,46), fac(rfac$( 92%)), route$( 92%)       , ch(03),~
               at (14,50), fac(rfac$( 93%)), route$( 93%)       , ch(03),~
               at (14,54), fac(rfac$( 94%)), route$( 94%)       , ch(03),~
               at (14,58), fac(rfac$( 95%)), route$( 95%)       , ch(03),~
               at (14,62), fac(rfac$( 96%)), route$( 96%)       , ch(03),~
               at (14,66), fac(rfac$( 97%)), route$( 97%)       , ch(03),~
               at (14,70), fac(rfac$( 98%)), route$( 98%)       , ch(03),~
               at (14,74), fac(rfac$( 99%)), route$( 99%)       , ch(03),~
               at (14,78), fac(rfac$(100%)), route$(100%)       , ch(03),~
                                                                         ~
               at (15,02), fac(rfac$(101%)), route$(101%)       , ch(03),~
               at (15,06), fac(rfac$(102%)), route$(102%)       , ch(03),~
               at (15,10), fac(rfac$(103%)), route$(103%)       , ch(03),~
               at (15,14), fac(rfac$(104%)), route$(104%)       , ch(03),~
               at (15,18), fac(rfac$(105%)), route$(105%)       , ch(03),~
               at (15,22), fac(rfac$(106%)), route$(106%)       , ch(03),~
               at (15,26), fac(rfac$(107%)), route$(107%)       , ch(03),~
               at (15,30), fac(rfac$(108%)), route$(108%)       , ch(03),~
               at (15,34), fac(rfac$(109%)), route$(109%)       , ch(03),~
               at (15,38), fac(rfac$(110%)), route$(110%)       , ch(03),~
               at (15,42), fac(rfac$(111%)), route$(111%)       , ch(03),~
               at (15,46), fac(rfac$(112%)), route$(112%)       , ch(03),~
               at (15,50), fac(rfac$(113%)), route$(113%)       , ch(03),~
               at (15,54), fac(rfac$(114%)), route$(114%)       , ch(03),~
               at (15,58), fac(rfac$(115%)), route$(115%)       , ch(03),~
               at (15,62), fac(rfac$(116%)), route$(116%)       , ch(03),~
               at (15,66), fac(rfac$(117%)), route$(117%)       , ch(03),~
               at (15,70), fac(rfac$(118%)), route$(118%)       , ch(03),~
               at (15,74), fac(rfac$(119%)), route$(119%)       , ch(03),~
               at (15,78), fac(rfac$(120%)), route$(120%)       , ch(03),~
                                                                         ~
               at (16,02), fac(rfac$(121%)), route$(121%)       , ch(03),~
               at (16,06), fac(rfac$(122%)), route$(122%)       , ch(03),~
               at (16,10), fac(rfac$(123%)), route$(123%)       , ch(03),~
               at (16,14), fac(rfac$(124%)), route$(124%)       , ch(03),~
               at (16,18), fac(rfac$(125%)), route$(125%)       , ch(03),~
               at (16,22), fac(rfac$(126%)), route$(126%)       , ch(03),~
               at (16,26), fac(rfac$(127%)), route$(127%)       , ch(03),~
               at (16,30), fac(rfac$(128%)), route$(128%)       , ch(03),~
               at (16,34), fac(rfac$(129%)), route$(129%)       , ch(03),~
               at (16,38), fac(rfac$(130%)), route$(130%)       , ch(03),~
               at (16,42), fac(rfac$(131%)), route$(131%)       , ch(03),~
               at (16,46), fac(rfac$(132%)), route$(132%)       , ch(03),~
               at (16,50), fac(rfac$(133%)), route$(133%)       , ch(03),~
               at (16,54), fac(rfac$(134%)), route$(134%)       , ch(03),~
               at (16,58), fac(rfac$(135%)), route$(135%)       , ch(03),~
               at (16,62), fac(rfac$(136%)), route$(136%)       , ch(03),~
               at (16,66), fac(rfac$(137%)), route$(137%)       , ch(03),~
               at (16,70), fac(rfac$(138%)), route$(138%)       , ch(03),~
               at (16,74), fac(rfac$(139%)), route$(139%)       , ch(03),~
               at (16,78), fac(rfac$(140%)), route$(140%)       , ch(03),~
                                                                         ~
               at (17,02), fac(rfac$(141%)), route$(141%)       , ch(03),~
               at (17,06), fac(rfac$(142%)), route$(142%)       , ch(03),~
               at (17,10), fac(rfac$(143%)), route$(143%)       , ch(03),~
               at (17,14), fac(rfac$(144%)), route$(144%)       , ch(03),~
               at (17,18), fac(rfac$(145%)), route$(145%)       , ch(03),~
               at (17,22), fac(rfac$(146%)), route$(146%)       , ch(03),~
               at (17,26), fac(rfac$(147%)), route$(147%)       , ch(03),~
               at (17,30), fac(rfac$(148%)), route$(148%)       , ch(03),~
               at (17,34), fac(rfac$(149%)), route$(149%)       , ch(03),~
               at (17,38), fac(rfac$(150%)), route$(150%)       , ch(03),~
               at (17,42), fac(rfac$(151%)), route$(151%)       , ch(03),~
               at (17,46), fac(rfac$(152%)), route$(152%)       , ch(03),~
               at (17,50), fac(rfac$(153%)), route$(153%)       , ch(03),~
               at (17,54), fac(rfac$(154%)), route$(154%)       , ch(03),~
               at (17,58), fac(rfac$(155%)), route$(155%)       , ch(03),~
               at (17,62), fac(rfac$(156%)), route$(156%)       , ch(03),~
               at (17,66), fac(rfac$(157%)), route$(157%)       , ch(03),~
               at (17,70), fac(rfac$(158%)), route$(158%)       , ch(03),~
               at (17,74), fac(rfac$(159%)), route$(159%)       , ch(03),~
               at (17,78), fac(rfac$(160%)), route$(160%)       , ch(03),~
                                                                         ~
               at (18,02), fac(rfac$(161%)), route$(161%)       , ch(03),~
               at (18,06), fac(rfac$(162%)), route$(162%)       , ch(03),~
               at (18,10), fac(rfac$(163%)), route$(163%)       , ch(03),~
               at (18,14), fac(rfac$(164%)), route$(164%)       , ch(03),~
               at (18,18), fac(rfac$(165%)), route$(165%)       , ch(03),~
               at (18,22), fac(rfac$(166%)), route$(166%)       , ch(03),~
               at (18,26), fac(rfac$(167%)), route$(167%)       , ch(03),~
               at (18,30), fac(rfac$(168%)), route$(168%)       , ch(03),~
               at (18,34), fac(rfac$(169%)), route$(169%)       , ch(03),~
               at (18,38), fac(rfac$(170%)), route$(170%)       , ch(03),~
               at (18,42), fac(rfac$(171%)), route$(171%)       , ch(03),~
               at (18,46), fac(rfac$(172%)), route$(172%)       , ch(03),~
               at (18,50), fac(rfac$(173%)), route$(173%)       , ch(03),~
               at (18,54), fac(rfac$(174%)), route$(174%)       , ch(03),~
               at (18,58), fac(rfac$(175%)), route$(175%)       , ch(03),~
               at (18,62), fac(rfac$(176%)), route$(176%)       , ch(03),~
               at (18,66), fac(rfac$(177%)), route$(177%)       , ch(03),~
               at (18,70), fac(rfac$(178%)), route$(178%)       , ch(03),~
               at (18,74), fac(rfac$(179%)), route$(179%)       , ch(03),~
               at (18,78), fac(rfac$(180%)), route$(180%)       , ch(03),~
                                                                         ~
               at (19,02), fac(rfac$(181%)), route$(181%)       , ch(03),~
               at (19,06), fac(rfac$(182%)), route$(182%)       , ch(03),~
               at (19,10), fac(rfac$(183%)), route$(183%)       , ch(03),~
               at (19,14), fac(rfac$(184%)), route$(184%)       , ch(03),~
               at (19,18), fac(rfac$(185%)), route$(185%)       , ch(03),~
               at (19,22), fac(rfac$(186%)), route$(186%)       , ch(03),~
               at (19,26), fac(rfac$(187%)), route$(187%)       , ch(03),~
               at (19,30), fac(rfac$(188%)), route$(188%)       , ch(03),~
               at (19,34), fac(rfac$(189%)), route$(189%)       , ch(03),~
               at (19,38), fac(rfac$(190%)), route$(190%)       , ch(03),~
               at (19,42), fac(rfac$(191%)), route$(191%)       , ch(03),~
               at (19,46), fac(rfac$(192%)), route$(192%)       , ch(03),~
               at (19,50), fac(rfac$(193%)), route$(193%)       , ch(03),~
               at (19,54), fac(rfac$(194%)), route$(194%)       , ch(03),~
               at (19,58), fac(rfac$(195%)), route$(195%)       , ch(03),~
               at (19,62), fac(rfac$(196%)), route$(196%)       , ch(03),~
               at (19,66), fac(rfac$(197%)), route$(197%)       , ch(03),~
               at (19,70), fac(rfac$(198%)), route$(198%)       , ch(03),~
               at (19,74), fac(rfac$(199%)), route$(199%)       , ch(03),~
               at (19,78), fac(rfac$(200%)), route$(200%)       , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1%)         , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3%)         , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <>  8% then L38931
                  call "BOMEFDSP" (part$, #05, #04, #01, #02, #08, #11)
                  goto L36610

L38931:        if keyhit% <> 11% then L38940
                  gosub view_ecr_info
                  goto L36610

L38940:        if keyhit% <> 13% then L38980
                  call "MANUAL" ("RTEINPUT")
                  goto L36610

L38980:        if keyhit% <> 15% then L39020
                  call "PRNTSCRN"
                  goto L36610

L39020:        if keyhit% <> 9% then return
                  if fieldnr% <> 2% then L36610
                  call "BOMSRTES" (part$, #2, #4, #1, #11)
                  goto L36610

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            * --------------------------------------------------------- *~
            * Input/Edit of First Line Item Screen.                     *~
            *************************************************************

            deffn'202(fieldnr%)          /* Input Mode Lines */
                init(hex(8c)) lfac$()
                str(line2$,,61%) = "Line Item Screen Page 1"
                pfktext$(1) = "(1)Start Route Over                     "&~
                              "                       (13)Instructions"
                pfktext$(2) = "(2)Restart Line     (4)Prev. Field      "&~
                              "                       (15)Print Screen"
                pfktext$(3) = "                    (6)Same as Prev Line"&~
                              " (25)Step Free Text    (16)Edit Mode"
                pfkeys$ = hex(00010204060d0f1019)
                if fieldnr% > 1% then L41095
                  str(pfktext$(2),,37) = " "  /* Shut Off Prev Field */
                  str(pfkeys$,3,2) = hex(ffff)
                  goto L41245
L41095:         str(pfktext$(3),63) = " "   /* Shut Off Edit Mode  */
                str(pfkeys$,8,1) = hex(ff)
                goto L41245

            deffn'212(fieldnr%)          /* Edit Mode Lines */
                init(hex(86)) lfac$()
                str(line2$,,61%) = "Line Item Screen Page 1"
                if fieldnr% <> 0% then init(hex(8c)) lfac$()
                pfktext$(1) = "(1)Start RTE Over             (5)Next Pa"&~
                              "ge                     (13)Instructions"
                pfktext$(2) = "(2)First Step   (6)Prev Step  (8)Alterna"&~
                              "tes                    (15)Print Screen"
                pfktext$(3) = "(3)Last Step    (7)Next Step  (9)Header "&~
                              " (25)Step Free Text    (16)Line Summary"
                pfkeys$ = hex(000102060307090d0f101908051d)

            REM Turn Off Appropriate Fields. Are we editing a field?
                if fieldnr% = 0% then L41205  /* no */
                     str(pfktext$(1),30,15) = " "
                     str(pfktext$(2),,63), pfktext$(3) = " "
                     pfkeys$ = hex(00010d0f)
                     init(hex(8c)) lfac$()
                     goto L41245
L41205:     REM Display Mode...
                if c% > 1 then L41225
                  str(pfktext$(2),,29)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,3,2) = hex(ffff)
L41225:         if c% < maxlines% then L41245
                  str(pfktext$(3),,29)   = " "  /* Shut Off Next Stuff */
                  str(pfkeys$,5,2) = hex(ffff)

L41245:     REM Set Up header Portion Of Screen...
                str(pfktext$(3),63,1) = hex(84)
                header$ = part$ & " " & partdescr$ & ", Rte " & rte$ &   ~
                     ", Seq"
                convert c% to str(header$,len(header$)+2%,4%), pic(###)
                on fieldnr% gosub L41360,         /* Step Number        */~
                                  L41360,         /* Work Center Code   */~
                                  L41360,         /* Move/Queue Days    */~
                                  L41360,         /* Set Up Time        */~
                                  L41360,         /* Run Time           */~
                                  L41360,         /* Yield Percentage   */~
                                  L41360,         /* Setup Activity Code*/~
                                  L41360,         /* Activity Code      */~
                                  L41345,         /* Activity Descrip   */~
                                  L41360,         /* % Complete (memo  )*/~
                                  L41360          /* Handling Factor    */
                     goto L41380

L41345:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41360:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L41380:     accept                                                       ~
               at (01,02), "Manage Production Work Center Routings",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Step Number Reference",                      ~
               at (06,25), fac(lfac$( 1)), step$(c%)            , ch(04),~
                                                                         ~
               at (07,02), "Work Center",                                ~
               at (07,25), fac(lfac$( 2)), wc$(c%)              , ch(04),~
               at (07,41), fac(hex(8c)),   wcdescr$(1)          , ch(32),~
                                                                         ~
               at (08,02), "Move/Queue Days",                            ~
               at (08,24), fac(lfac$( 3)), mq$(c%)              , ch(05),~
               at (08,30), "After Step?",                                ~
               at (08,42), fac(lfac$( 3)), mqp$(c%)             , ch(01),~
                                                                         ~
               at (10,02), "WC Setup Hours: xxxxxxxx        WC Units:",  ~
               at (10,18), fac(lfac$( 4)), suh$(c%)             , ch(08),~
               at (10,44), fac(lfac$( 4)), su$(c%)              , ch(06),~
                                                                         ~
               at (11,02), "Run Hours/Part: xxxxxxxx   WC Units/Part: xxx~
        ~xxx   Parts/Hr:xxxxxxxx",                                        ~
               at (11,18), fac(lfac$( 5)), runh$(c%)            , ch(08),~
               at (11,44), fac(lfac$( 5)), run$(c%)             , ch(06),~
               at (11,63), fac(lfac$( 5)), runp$                , ch(08),~
                                                                         ~
               at (13,02), "%Yield From This Step",                      ~
               at (13,25), fac(lfac$( 6)), yield$(c%)           , ch(03),~
                                                                         ~
               at (14,02), "Setup Activity Code",                        ~
               at (14,25), fac(lfac$( 7)), sucode$(c%)          , ch(04),~
               at (14,41), fac(hex(8c)),   sudescr$             , ch(32),~
                                                                         ~
               at (15,02), "Run Activity Code",                          ~
               at (15,25), fac(lfac$( 8)), actcode$(c%)         , ch(04),~
                                                                         ~
               at (16,02), "Activity Description",                       ~
               at (16,25), fac(lfac$( 9)), activity$(c%)        , ch(30),~
                                                                         ~
               at (17,02), "%Complete At This Step",                     ~
               at (17,25), fac(lfac$(10)), comp$(c%)            , ch(03),~
                                                                         ~
               at (18,02), "Move Unit Conv. Factor",                     ~
               at (18,25), fac(lfac$(11)), handfactor$(c%)      , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfktext$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfktext$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfktext$(3)            , ch(79),~
                    keys(pfkeys$),                                       ~
                    key(keyhit%)

               if keyhit% <> 13 then L41670
                  call "MANUAL" ("RTEINPUT")
                  goto L41380

L41670:        if keyhit% <> 15 then L41690
                  call "PRNTSCRN"
                  goto L41380

L41690:        if linemode% = 0 then return
                  close ws
                  call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                  return


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   3      *~
            * --------------------------------------------------------- *~
            * Input/Edit of Second Line Item Screen.                    *~
            *************************************************************

            deffn'203(fieldnr%)          /* Input Mode Lines */
                init(hex(8c)) lfac$()
                str(line2$,,61%) = "Line Item Screen Page 2, Step " &    ~
                     step$(c%) & ", W/C " & wc$(c%)
                pfktext$(1) = "(1)Start Route Over                     "&~
                              "                       (13)Instructions"
                pfktext$(2) = "(2)Restart Line     (4)Prev. Field      "&~
                              "                       (15)Print Screen"
                pfktext$(3) = "                    (6)Same as Prev Line"&~
                              " (25)Step Free Text    (16)Edit Mode"
                pfkeys$ = hex(00010204060d0f1019)
                if fieldnr% > 1% then L42095
                  str(pfktext$(2),,37) = " "  /* Shut Off Prev Field */
                  str(pfkeys$,3,2) = hex(ffff)
                  goto L42245
L42095:         str(pfktext$(3),63) = " "   /* Shut Off Edit Mode  */
                str(pfkeys$,8,1) = hex(ff)
                goto L42245

            deffn'213(fieldnr%)          /* Edit Mode Lines */
                init(hex(86)) lfac$()
                str(line2$,,61%) = "Line Item Screen Page 2, Step " &    ~
                     step$(c%) & ", W/C " & wc$(c%)
                if fieldnr% <> 0% then init(hex(8c)) lfac$()
                pfktext$(1) = "(1)Start RTE Over             (4)Prev Pa"&~
                              "ge                     (13)Instructions"
                pfktext$(2) = "(2)First Step   (6)Prev Step  (8)Alterna"&~
                              "tes                    (15)Print Screen"
                pfktext$(3) = "(3)Last Step    (7)Next Step  (9)Header "&~
                              " (25)Step Free Text    (16)Line Summary"
                pfkeys$ = hex(000102060307090d0f101908041d)

            REM Turn Off Appropriate Fields. Are we editing a field?
                if fieldnr% = 0% then L42205  /* no */
                     str(pfktext$(1),30,15) = " "
                     str(pfktext$(2),,63), pfktext$(3) = " "
                     pfkeys$ = hex(00010d0f)
                     init(hex(8c)) lfac$()
                     goto L42245
L42205:     REM Display Mode...
                if c% > 1 then L42225
                  str(pfktext$(2),,29)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,3,2) = hex(ffff)
L42225:         if c% < maxlines% then L42245
                  str(pfktext$(3),,29)   = " "  /* Shut Off Next Stuff */
                  str(pfkeys$,5,2) = hex(ffff)

L42245:     REM Set Up header Portion Of Screen...
                str(pfktext$(3),63,1) = hex(84)
                header$ = part$ & " " & partdescr$ & ", Rte " & rte$ &   ~
                     ", Seq"
                convert c% to str(header$,len(header$)+2%,3%), pic(###)
                on fieldnr% gosub L42380,         /* Shift Differential */~
                                  L42380,         /* Concurrent WC One  */~
                                  L42380,         /* Capacity adj. Factr*/~
                                  L42380,         /* Activity Code      */~
                                  L42380,         /* Concurrent WC Two  */~
                                  L42380,         /* Capacity adj. Factr*/~
                                  L42380,         /* Activity Code      */~
                                  L42380,         /* Concurrent WC Three*/~
                                  L42380,         /* Capacity adj. Factr*/~
                                  L42380,         /* Activity Code      */~
                                  L42380,         /* Force Contiguity   */~
                                  L42380,         /* 1st Overlap Factor */~
                                  L42380          /* 2st Overlap Factor */
                     goto L42400

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42380:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L42400:     accept                                                       ~
               at (01,02), "Manage Production Work Center Routings",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Shift Differential",                         ~
               at (06,25), fac(lfac$( 1)), shift$(c%)           , ch(07),~
                                                                         ~
               at (07,02), "Concurrent Work Center",                     ~
               at (07,25), fac(lfac$( 2)), wc1$(c%)             , ch(04),~
               at (07,41), fac(hex(8c)),   wcdescr$(2)          , ch(32),~
                                                                         ~
               at (08,02), "  Capacity adj. Factor",                     ~
               at (08,25), fac(lfac$( 3)), nm1$(c%)             , ch(06),~
                                                                         ~
               at (09,02), "  Run Activity Code",                        ~
               at (09,25), fac(lfac$( 4)), ca1$(c%)             , ch(04),~
               at (09,41), fac(hex(8c)),   cadescr$(1)          , ch(32),~
                                                                         ~
               at (10,02), "Concurrent Work Center",                     ~
               at (10,25), fac(lfac$( 5)), wc2$(c%)             , ch(04),~
               at (10,41), fac(hex(8c)),   wcdescr$(3)          , ch(32),~
                                                                         ~
               at (11,02), "  Capacity adj. Factor",                     ~
               at (11,25), fac(lfac$( 6)), nm2$(c%)             , ch(06),~
                                                                         ~
               at (12,02), "  Run Activity Code",                        ~
               at (12,25), fac(lfac$( 7)), ca2$(c%)             , ch(04),~
               at (12,41), fac(hex(8c)),   cadescr$(2)          , ch(32),~
                                                                         ~
               at (13,02), "Concurrent Work Center",                     ~
               at (13,25), fac(lfac$( 8)), wc3$(c%)             , ch(04),~
               at (13,41), fac(hex(8c)),   wcdescr$(4)          , ch(32),~
                                                                         ~
               at (14,02), "  Capacity adj. Factor",                     ~
               at (14,25), fac(lfac$( 9)), nm3$(c%)             , ch(06),~
                                                                         ~
               at (15,02), "  Run Activity Code",                        ~
               at (15,25), fac(lfac$(10)), ca3$(c%)             , ch(04),~
               at (15,41), fac(hex(8c)),   cadescr$(3)          , ch(32),~
                                                                         ~
               at (16,02), "Force Contiguity With Next Step?",           ~
               at (16,35), fac(lfac$(11)), cg$(c%)              , ch(01),~
                                                                         ~
               at (17,04), fac(hex(ac)),   hdr1$                , ch(42),~
               at (17,47), fac(hex(ac)),   hdr$(1%)             , ch(08),~
               at (17,57), fac(hex(ac)),   hdr$(2%)             , ch(08),~
               at (18,04), "Minimum Remaining At Prev Step Completion:", ~
               at (18,47), fac(lfac$(12)), f1$(c%,1%)           , ch(08),~
               at (18,57), fac(lfac$(12)), f1$(c%,2%)           , ch(08),~
                                                                         ~
               at (19,04), "Run Required Before Next Step Can Start:",   ~
               at (19,47), fac(lfac$(13)), f2$(c%,1%)           , ch(08),~
               at (19,57), fac(lfac$(13)), f2$(c%,2%)           , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfktext$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfktext$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfktext$(3)            , ch(79),~
                    keys(pfkeys$),                                       ~
                    key(keyhit%)

               if keyhit% <> 13 then L42750
                  call "MANUAL" ("RTEINPUT")
                  goto L42400

L42750:        if keyhit% <> 15 then L42770
                  call "PRNTSCRN"
                  goto L42400

L42770:        if linemode% = 0 then return
                  close ws
                  call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *     L I N E   I T E M   H A N D L I N G   S C R E E N     *~
            *                                                           *~
            * THIS SCREEN HANDLES INPUT, EDIT, INSERT AND DELETE MODES  *~
            * FOR THE PRODUCTION STEPS TABLE.                           *~
            *************************************************************

            deffn'214(fieldnr%) /* Editmode */
                str(line2$,,61%) = "Production Routing Summary Screen"
                init (hex(8e)) tfac$() : init (hex(84)) lfac$()
                init (hex(8c)) dfac$()
                if noedit% = 1 and errormsg$ = " " then errormsg$ =      ~
                      hex(84) & "Route can be reviewed but not modified."
                pfktext$(1%)= "(1)St Over (4)Prev Pg  (8)Altrnts (11)Inse~
        ~rt    (17)Step Deflt (13)Instructions"
                pfktext$(2%)= "(2)1st Pg  (5)Next Pg  (9)Header  (12/28)D~
        ~elete (18)Re-# Steps (15)Print Screen"
                pfktext$(3%)= "(3)Last Pg (6)Dn (7)Up (10)Toggle (14)Copy~
        ~ Rte  (25)Rte Text   (16)Save Data"
                pfkeys$ = hex(0102040603050708090a0b0c0d0e0f101112191c00)
*        Lose Administrator Step Default values if not ADMIN%.
                if admin% <> 0% then goto L43111
                     str(pfktext$(1%),49%,14%) = " "/* Lose PF(17)StepD*/
                     str(pfktext$(2%),49%,14%) = " "/* Lose PF(18)Re-# */
                     str(pfkeys$,17%,2%) = hex(ffff)
L43111:         if maxlines% < lines_allowed% then goto L43130
                     str(pfktext$(3%),35%,12%) = " "/* Lose PF(14)Copy */
                     str(pfkeys$,14%,1%) = hex(ff)
                     str(pfktext$(1%),35%,10%) = " "/* Lose PF(11)Insrt*/
                     str(pfkeys$,11%,1%) = hex(ff)

L43130:         REM Flip Off Appropriate Fields
                if maxlines% > 0 then L43165
                  str(pfktext$(1),24,10) = " " /* Lose PF(8)Altrnts    */
                  str(pfktext$(2),35,13) = " " /* Lose PF(12/28)Delete */
                  str(pfktext$(3),24,10) = " " /* Lose PF(10)Toggle    */
                  str(pfkeys$,10%,1%), str(pfkeys$,12%,1%),              ~
                     str(pfkeys$,20%,1%), str(pfkeys$,8%,1%) = hex(ff)
L43165:         if line% > 0% then L43185
                  str(pfktext$(1%),12%,10%), str(pfktext$(2%),1%,9%),    ~
                      str(pfktext$(3),12%,5%)= " "/* Shut PF(2), 4 & 6 */
                  str(pfkeys$,2%,3%) = hex(ffffff)
L43185:         if line%+14% < maxlines% then L43210
                  str(pfktext$(2%),12%,10%), str(pfktext$(3%),1%,10%),   ~
                      str(pfktext$(3),18%,5%)= " "/* Shut PF(3), 5 & 7 */
                  str(pfkeys$,5%,3%) = hex(ffffff)

L43210:        numbers$() = " "
               if maxlines% < 1% then L43300
               for i% = 1% to min(maxlines%-line%, 14%)
                   if dmode% <> 1% then L43245
                   str(numbers$(i%,1%),3) = su$(line%+i%)  /* WC UNITS */
                   str(numbers$(i%,2%),3) = run$(line%+i%) /* WC UNITS */
                       goto L43295
L43245:            if dmode% <> 2% then L43285
                   temp, temp1 = 0
                   convert suh$(line%+i%) to temp, data goto L43285
                   convert runh$(line%+i%) to temp1, data goto L43285
                   numbers$(i%,1%), numbers$(i%,2%) = "fmt"
                   call "TIMEOK" (numbers$(i%,1%),temp," ")  /*HH:MM:SS*/
                   call "TIMEOK" (numbers$(i%,2%),temp1," ") /*HH:MM:SS*/
                       goto L43295
L43285:            numbers$(i%,1%) = suh$(line%+i%)   /* HH.DDDD */
                   numbers$(i%,2%) = runh$(line%+i%)  /* HH.DDDD */
L43295:        next i%
L43300:        goto L43375

            deffn'125(d%)  /* Delete Lines From Memory */
                init (hex(8c)) lfac$(), tfac$(), dfac$()
                str(line2$,,61%) = "Production Routing Summary Screen"
                pfktext$(1) = "(1)Cancel Delete Request                  ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "(ENTER) Delete Flashing Line(s)           ~
        ~                     (16)Don't Delete"
                pfkeys$ = hex(00010d0f10)
                if d% = 0% then init(hex(94)) lfac$(), tfac$(), dfac$()  ~
                           else lfac$(d%), tfac$(d%), dfac$(d%) = hex(94)

L43375:         header2$   = "          Work  Move    Hours  Hours Yield ~
        ~Activity"
                header1$   = "Seq. Step Cntr Queue    Setup  Run/Prt Pct ~
        ~Code Description"
                if dmode% = 1% then str(header2$,25,12) = "Units  Units"
                header$ = part$ & " " & partdescr$ & ", Rte " & rte$
                str(tfac$(), min(20, (maxlines%-line%)+1)) = all(hex(9c))
                str(pfktext$(3),63,1) = hex(84)

L43420:     accept                                                       ~
               at (01,02), "Manage Production Work Center Routings",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(8c)), header2$               , ch(79),~
               at (06,02), fac(hex(ac)), header1$               , ch(04),~
               at (06,07), fac(hex(ac)), str(header1$,6)        , ch(74),~
                                                                         ~
               at (07,02), fac(tfac$( 1)),   seq$    (line%+ 1) , ch(04),~
               at (08,02), fac(tfac$( 2)),   seq$    (line%+ 2) , ch(04),~
               at (09,02), fac(tfac$( 3)),   seq$    (line%+ 3) , ch(04),~
               at (10,02), fac(tfac$( 4)),   seq$    (line%+ 4) , ch(04),~
               at (11,02), fac(tfac$( 5)),   seq$    (line%+ 5) , ch(04),~
               at (12,02), fac(tfac$( 6)),   seq$    (line%+ 6) , ch(04),~
               at (13,02), fac(tfac$( 7)),   seq$    (line%+ 7) , ch(04),~
               at (14,02), fac(tfac$( 8)),   seq$    (line%+ 8) , ch(04),~
               at (15,02), fac(tfac$( 9)),   seq$    (line%+ 9) , ch(04),~
               at (16,02), fac(tfac$(10)),   seq$    (line%+10) , ch(04),~
               at (17,02), fac(tfac$(11)),   seq$    (line%+11) , ch(04),~
               at (18,02), fac(tfac$(12)),   seq$    (line%+12) , ch(04),~
               at (19,02), fac(tfac$(13)),   seq$    (line%+13) , ch(04),~
               at (20,02), fac(tfac$(14)),   seq$    (line%+14) , ch(04),~
                                                                         ~
               at (07,07), fac(dfac$( 1)),   step$   (line%+ 1) , ch(04),~
               at (08,07), fac(dfac$( 2)),   step$   (line%+ 2) , ch(04),~
               at (09,07), fac(dfac$( 3)),   step$   (line%+ 3) , ch(04),~
               at (10,07), fac(dfac$( 4)),   step$   (line%+ 4) , ch(04),~
               at (11,07), fac(dfac$( 5)),   step$   (line%+ 5) , ch(04),~
               at (12,07), fac(dfac$( 6)),   step$   (line%+ 6) , ch(04),~
               at (13,07), fac(dfac$( 7)),   step$   (line%+ 7) , ch(04),~
               at (14,07), fac(dfac$( 8)),   step$   (line%+ 8) , ch(04),~
               at (15,07), fac(dfac$( 9)),   step$   (line%+ 9) , ch(04),~
               at (16,07), fac(dfac$(10)),   step$   (line%+10) , ch(04),~
               at (17,07), fac(dfac$(11)),   step$   (line%+11) , ch(04),~
               at (18,07), fac(dfac$(12)),   step$   (line%+12) , ch(04),~
               at (19,07), fac(dfac$(13)),   step$   (line%+13) , ch(04),~
               at (20,07), fac(dfac$(14)),   step$   (line%+14) , ch(04),~
                                                                         ~
               at (07,12), fac(lfac$( 1)),   wc$     (line%+ 1) , ch(04),~
               at (08,12), fac(lfac$( 2)),   wc$     (line%+ 2) , ch(04),~
               at (09,12), fac(lfac$( 3)),   wc$     (line%+ 3) , ch(04),~
               at (10,12), fac(lfac$( 4)),   wc$     (line%+ 4) , ch(04),~
               at (11,12), fac(lfac$( 5)),   wc$     (line%+ 5) , ch(04),~
               at (12,12), fac(lfac$( 6)),   wc$     (line%+ 6) , ch(04),~
               at (13,12), fac(lfac$( 7)),   wc$     (line%+ 7) , ch(04),~
               at (14,12), fac(lfac$( 8)),   wc$     (line%+ 8) , ch(04),~
               at (15,12), fac(lfac$( 9)),   wc$     (line%+ 9) , ch(04),~
               at (16,12), fac(lfac$(10)),   wc$     (line%+10) , ch(04),~
               at (17,12), fac(lfac$(11)),   wc$     (line%+11) , ch(04),~
               at (18,12), fac(lfac$(12)),   wc$     (line%+12) , ch(04),~
               at (19,12), fac(lfac$(13)),   wc$     (line%+13) , ch(04),~
               at (20,12), fac(lfac$(14)),   wc$     (line%+14) , ch(04),~
                                                                         ~
               at (07,17), fac(dfac$( 1)),   mq$     (line%+ 1) , ch(05),~
               at (08,17), fac(dfac$( 2)),   mq$     (line%+ 2) , ch(05),~
               at (09,17), fac(dfac$( 3)),   mq$     (line%+ 3) , ch(05),~
               at (10,17), fac(dfac$( 4)),   mq$     (line%+ 4) , ch(05),~
               at (11,17), fac(dfac$( 5)),   mq$     (line%+ 5) , ch(05),~
               at (12,17), fac(dfac$( 6)),   mq$     (line%+ 6) , ch(05),~
               at (13,17), fac(dfac$( 7)),   mq$     (line%+ 7) , ch(05),~
               at (14,17), fac(dfac$( 8)),   mq$     (line%+ 8) , ch(05),~
               at (15,17), fac(dfac$( 9)),   mq$     (line%+ 9) , ch(05),~
               at (16,17), fac(dfac$(10)),   mq$     (line%+10) , ch(05),~
               at (17,17), fac(dfac$(11)),   mq$     (line%+11) , ch(05),~
               at (18,17), fac(dfac$(12)),   mq$     (line%+12) , ch(05),~
               at (19,17), fac(dfac$(13)),   mq$     (line%+13) , ch(05),~
               at (20,17), fac(dfac$(14)),   mq$     (line%+14) , ch(05),~
                                                                         ~
               at (07,23), fac(lfac$( 1)),   numbers$( 1%, 1%)  , ch(08),~
               at (08,23), fac(lfac$( 2)),   numbers$( 2%, 1%)  , ch(08),~
               at (09,23), fac(lfac$( 3)),   numbers$( 3%, 1%)  , ch(08),~
               at (10,23), fac(lfac$( 4)),   numbers$( 4%, 1%)  , ch(08),~
               at (11,23), fac(lfac$( 5)),   numbers$( 5%, 1%)  , ch(08),~
               at (12,23), fac(lfac$( 6)),   numbers$( 6%, 1%)  , ch(08),~
               at (13,23), fac(lfac$( 7)),   numbers$( 7%, 1%)  , ch(08),~
               at (14,23), fac(lfac$( 8)),   numbers$( 8%, 1%)  , ch(08),~
               at (15,23), fac(lfac$( 9)),   numbers$( 9%, 1%)  , ch(08),~
               at (16,23), fac(lfac$(10)),   numbers$(10%, 1%)  , ch(08),~
               at (17,23), fac(lfac$(11)),   numbers$(11%, 1%)  , ch(08),~
               at (18,23), fac(lfac$(12)),   numbers$(12%, 1%)  , ch(08),~
               at (19,23), fac(lfac$(13)),   numbers$(13%, 1%)  , ch(08),~
               at (20,23), fac(lfac$(14)),   numbers$(14%, 1%)  , ch(08),~
                                                                         ~
               at (07,32), fac(lfac$( 1)),   numbers$( 1%, 2%)  , ch(08),~
               at (08,32), fac(lfac$( 2)),   numbers$( 2%, 2%)  , ch(08),~
               at (09,32), fac(lfac$( 3)),   numbers$( 3%, 2%)  , ch(08),~
               at (10,32), fac(lfac$( 4)),   numbers$( 4%, 2%)  , ch(08),~
               at (11,32), fac(lfac$( 5)),   numbers$( 5%, 2%)  , ch(08),~
               at (12,32), fac(lfac$( 6)),   numbers$( 6%, 2%)  , ch(08),~
               at (13,32), fac(lfac$( 7)),   numbers$( 7%, 2%)  , ch(08),~
               at (14,32), fac(lfac$( 8)),   numbers$( 8%, 2%)  , ch(08),~
               at (15,32), fac(lfac$( 9)),   numbers$( 9%, 2%)  , ch(08),~
               at (16,32), fac(lfac$(10)),   numbers$(10%, 2%)  , ch(08),~
               at (17,32), fac(lfac$(11)),   numbers$(11%, 2%)  , ch(08),~
               at (18,32), fac(lfac$(12)),   numbers$(12%, 2%)  , ch(08),~
               at (19,32), fac(lfac$(13)),   numbers$(13%, 2%)  , ch(08),~
               at (20,32), fac(lfac$(14)),   numbers$(14%, 2%)  , ch(08),~
                                                                         ~
               at (07,41), fac(dfac$( 1)),   yield$  (line%+ 1) , ch(03),~
               at (08,41), fac(dfac$( 2)),   yield$  (line%+ 2) , ch(03),~
               at (09,41), fac(dfac$( 3)),   yield$  (line%+ 3) , ch(03),~
               at (10,41), fac(dfac$( 4)),   yield$  (line%+ 4) , ch(03),~
               at (11,41), fac(dfac$( 5)),   yield$  (line%+ 5) , ch(03),~
               at (12,41), fac(dfac$( 6)),   yield$  (line%+ 6) , ch(03),~
               at (13,41), fac(dfac$( 7)),   yield$  (line%+ 7) , ch(03),~
               at (14,41), fac(dfac$( 8)),   yield$  (line%+ 8) , ch(03),~
               at (15,41), fac(dfac$( 9)),   yield$  (line%+ 9) , ch(03),~
               at (16,41), fac(dfac$(10)),   yield$  (line%+10) , ch(03),~
               at (17,41), fac(dfac$(11)),   yield$  (line%+11) , ch(03),~
               at (18,41), fac(dfac$(12)),   yield$  (line%+12) , ch(03),~
               at (19,41), fac(dfac$(13)),   yield$  (line%+13) , ch(03),~
               at (20,41), fac(dfac$(14)),   yield$  (line%+14) , ch(03),~
                                                                         ~
               at (07,45), fac(dfac$( 1)),   actcode$(line%+ 1) , ch(04),~
               at (08,45), fac(dfac$( 2)),   actcode$(line%+ 2) , ch(04),~
               at (09,45), fac(dfac$( 3)),   actcode$(line%+ 3) , ch(04),~
               at (10,45), fac(dfac$( 4)),   actcode$(line%+ 4) , ch(04),~
               at (11,45), fac(dfac$( 5)),   actcode$(line%+ 5) , ch(04),~
               at (12,45), fac(dfac$( 6)),   actcode$(line%+ 6) , ch(04),~
               at (13,45), fac(dfac$( 7)),   actcode$(line%+ 7) , ch(04),~
               at (14,45), fac(dfac$( 8)),   actcode$(line%+ 8) , ch(04),~
               at (15,45), fac(dfac$( 9)),   actcode$(line%+ 9) , ch(04),~
               at (16,45), fac(dfac$(10)),   actcode$(line%+10) , ch(04),~
               at (17,45), fac(dfac$(11)),   actcode$(line%+11) , ch(04),~
               at (18,45), fac(dfac$(12)),   actcode$(line%+12) , ch(04),~
               at (19,45), fac(dfac$(13)),   actcode$(line%+13) , ch(04),~
               at (20,45), fac(dfac$(14)),   actcode$(line%+14) , ch(04),~
                                                                         ~
               at (07,50), fac(lfac$( 1)),   activity$(line%+ 1), ch(30),~
               at (08,50), fac(lfac$( 2)),   activity$(line%+ 2), ch(30),~
               at (09,50), fac(lfac$( 3)),   activity$(line%+ 3), ch(30),~
               at (10,50), fac(lfac$( 4)),   activity$(line%+ 4), ch(30),~
               at (11,50), fac(lfac$( 5)),   activity$(line%+ 5), ch(30),~
               at (12,50), fac(lfac$( 6)),   activity$(line%+ 6), ch(30),~
               at (13,50), fac(lfac$( 7)),   activity$(line%+ 7), ch(30),~
               at (14,50), fac(lfac$( 8)),   activity$(line%+ 8), ch(30),~
               at (15,50), fac(lfac$( 9)),   activity$(line%+ 9), ch(30),~
               at (16,50), fac(lfac$(10)),   activity$(line%+10), ch(30),~
               at (17,50), fac(lfac$(11)),   activity$(line%+11), ch(30),~
               at (18,50), fac(lfac$(12)),   activity$(line%+12), ch(30),~
               at (19,50), fac(lfac$(13)),   activity$(line%+13), ch(30),~
               at (20,50), fac(lfac$(14)),   activity$(line%+14), ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key  (keyhit%)

               if keyhit% <> 10% then L45112
                  dmode% = dmode% + 1%
                  if dmode% > 2% then dmode% = 0%
                  goto L43210

L45112:        if keyhit% <> 13% then L45120
                  call "MANUAL" ("RTEINPUT")
                  goto L43420

L45120:        if keyhit% <> 15% then L45160
                  call "PRNTSCRN"
                  goto L43420

L45160:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *     L I N E   I T E M   H A N D L I N G   S C R E E N     *~
            *     FOR CONCURRENT WC'S AND ALTERNATES                    *~
            * THIS SCREEN HANDLES INPUT, EDIT, INSERT AND DELETE MODES  *~
            * FOR THE PRODUCTION ROUTING TABLE.                         *~
            *************************************************************

            deffn'204(ss%)
                str(line2$,,61%) = "Route Step Alternate Work Centers"
                pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "                    (6)Same as Prev Line  ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                                          ~
        ~                     (16)Editmode"
                pfkeys$ = hex(0001060d0f10)
                message$="Enter Alternate Work Center And All Other Infor~
        ~mation"
                goto L46375

            deffn'215(ss%)
                str(line2$,,61%) = "Route Step Alternate Work Centers"
                if noedit% = 1 and errormsg$ = " " then errormsg$ =      ~
                      hex(84) & "Route can be reviewed but not modified."
                message$ = "Route can be reviewed but not modified."
                pfktext$(1) = "(1)Start Over                        (9)Pr~
        ~ev Step   (11)Insert (13)Instructions"
                pfktext$(2) = "(2)First Alts  (4)Prev  (6)Down One  (10)N~
        ~ext Step  (12)Delete (15)Print Screen"
                pfktext$(3) = "(3)Last Alts   (5)Next  (7)Up One         ~
        ~                     (16)Return      "
                pfkeys$ = hex(0001020406030507090a0b0c0d0f10)

                REM Flip Off Appropriate Fields
                if currentline% > 1% then L46175
                  str(pfktext$(1),38,13)   = " "  /* Shut Off Prev Step */
                  str(pfkeys$,9,1) = hex(ff)
L46175:         if currentline% < maxlines% then L46190
                  str(pfktext$(2),38,13)   = " "  /* Shut Off Next Step */
                  str(pfkeys$,10,1) = hex(ff)
L46190:         if edtmax% > 0% then L46205
                  str(pfktext$(2%),53%,10%) = " " /* Shut Off Delete */
                  str(pfkeys$,12%,1%) = hex(ff)
L46205:         if l% > 0% then L46220
                  str(pfktext$(2%),,36%)   = " "/* Shut Off Prev Stuff */
                  str(pfkeys$,3%,3%) = hex(ffffff)
L46220:         if l%+13% < edtmax% then L46240
                  str(pfktext$(3%),,36%) = " "  /* Shut Off Next Stuff */
                  str(pfkeys$,6%,3%) = hex(ffffff)

L46240:         if noedit% = 1% then L46375
                   message$="Enter Alternate Work Center And All Other In~
        ~formation"
                  if ss%=0% then message$="Position Cursor And Press Ente~
        ~r To Edit Entire Line."
                  init(hex(8e)) tfac$()
                  init(hex(84)) lfac$()
                  if ss% = 0% then L46390
                pfktext$(1) = "(1)Start Route Over                     "&~
                              "                       (13)Instructions"
                pfktext$(2) = "                                        "&~
                              "                       (15)Print Screen"
                pfktext$(3) = " "
                pfkeys$ = hex(0001060d0f10)
                  goto L46375

            deffn'235(ss%)
                pfktext$(1) = "(1)Cancel Delete Request                  ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "(ENTER) Delete Flashing Line(s)           ~
        ~                     (16)Don't Delete"
                pfkeys$ = hex(00010d0f10)
                init(hex(8c)) lfac$(), tfac$()
                lfac$(ss%), tfac$(ss%) = hex(94)
                goto L46390

L46375:         init(hex(8c)) lfac$(), tfac$()
                if ss% <> 0% then lfac$(ss%)=hex(81)

L46390:           header2$ = "Work  Move  --WC-UNITS--                   ~
        ~                         "
                  header1$ = "Cntr Queue  Setup    Run MQ Shift Concurren~
        ~t WC's And Units Factors"
                header$ = part$ & " " & partdescr$ & ", Rte " & rte$ &   ~
                     ", Step " & step$(currentline%)
                  str(pfktext$(3),63,1) = hex(84)
                  str(tfac$(), min(20, (edtmax%-l%)+1)) = all(hex(9c))

L46440:     accept                                                       ~
               at (01,02), "Manage Production Work Center Routings",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,11), fac(hex(8c)), header2$               , ch(70),~
               at (06,02), fac(hex(bc)), header1$               , ch(08),~
               at (06,11), fac(hex(ac)), header1$               , ch(70),~
                                                                         ~
               at (07,02), "PRIMARY",                                    ~
               at (08,02), "ALT",                                        ~
                                                                         ~
               at (07,11), fac(hex(8c)), wc$ (currentline%)     , ch( 4),~
               at (07,16), fac(hex(8c)), mq$ (currentline%)     , ch( 5),~
               at (07,22), fac(hex(8c)), su$ (currentline%)     , ch( 6),~
               at (07,29), fac(hex(8c)), run$(currentline%)     , ch( 6),~
               at (07,36), fac(hex(8c)),   mqp$(currentline%)   , ch( 1),~
               at (07,38), fac(hex(8c)),   shift$(currentline%) , ch( 7),~
               at (07,46), fac(hex(8c)),   wc1$(currentline%)   , ch( 4),~
               at (07,51), fac(hex(8c)),   nm1$(currentline%)   , ch( 6),~
               at (07,58), fac(hex(8c)),   wc2$(currentline%)   , ch( 4),~
               at (07,63), fac(hex(8c)),   nm2$(currentline%)   , ch( 6),~
               at (07,70), fac(hex(8c)),   wc3$(currentline%)   , ch( 4),~
               at (07,75), fac(hex(8c)),   nm3$(currentline%)   , ch( 6),~
                                                                         ~
               at (08,06), fac(tfac$( 1)),   seq$    (l%+ 1%)   , ch(04),~
               at (09,06), fac(tfac$( 2)),   seq$    (l%+ 2%)   , ch(04),~
               at (10,06), fac(tfac$( 3)),   seq$    (l%+ 3%)   , ch(04),~
               at (11,06), fac(tfac$( 4)),   seq$    (l%+ 4%)   , ch(04),~
               at (12,06), fac(tfac$( 5)),   seq$    (l%+ 5%)   , ch(04),~
               at (13,06), fac(tfac$( 6)),   seq$    (l%+ 6%)   , ch(04),~
               at (14,06), fac(tfac$( 7)),   seq$    (l%+ 7%)   , ch(04),~
               at (15,06), fac(tfac$( 8)),   seq$    (l%+ 8%)   , ch(04),~
               at (16,06), fac(tfac$( 9)),   seq$    (l%+ 9%)   , ch(04),~
               at (17,06), fac(tfac$(10)),   seq$    (l%+10%)   , ch(04),~
               at (18,06), fac(tfac$(11)),   seq$    (l%+11%)   , ch(04),~
               at (19,06), fac(tfac$(12)),   seq$    (l%+12%)   , ch(04),~
               at (20,06), fac(tfac$(13)),   seq$    (l%+13%)   , ch(04),~
                                                                         ~
               at (08,11), fac(lfac$( 1)),awc0$(edtalt%(l%+ 1%)), ch( 4),~
               at (09,11), fac(lfac$( 2)),awc0$(edtalt%(l%+ 2%)), ch( 4),~
               at (10,11), fac(lfac$( 3)),awc0$(edtalt%(l%+ 3%)), ch( 4),~
               at (11,11), fac(lfac$( 4)),awc0$(edtalt%(l%+ 4%)), ch( 4),~
               at (12,11), fac(lfac$( 5)),awc0$(edtalt%(l%+ 5%)), ch( 4),~
               at (13,11), fac(lfac$( 6)),awc0$(edtalt%(l%+ 6%)), ch( 4),~
               at (14,11), fac(lfac$( 7)),awc0$(edtalt%(l%+ 7%)), ch( 4),~
               at (15,11), fac(lfac$( 8)),awc0$(edtalt%(l%+ 8%)), ch( 4),~
               at (16,11), fac(lfac$( 9)),awc0$(edtalt%(l%+ 9%)), ch( 4),~
               at (17,11), fac(lfac$(10)),awc0$(edtalt%(l%+10%)), ch( 4),~
               at (18,11), fac(lfac$(11)),awc0$(edtalt%(l%+11%)), ch( 4),~
               at (19,11), fac(lfac$(12)),awc0$(edtalt%(l%+12%)), ch( 4),~
               at (20,11), fac(lfac$(13)),awc0$(edtalt%(l%+13%)), ch( 4),~
                                                                         ~
               at (08,16), fac(lfac$( 1)),amq0$(edtalt%(l%+ 1%)), ch( 5),~
               at (09,16), fac(lfac$( 2)),amq0$(edtalt%(l%+ 2%)), ch( 5),~
               at (10,16), fac(lfac$( 3)),amq0$(edtalt%(l%+ 3%)), ch( 5),~
               at (11,16), fac(lfac$( 4)),amq0$(edtalt%(l%+ 4%)), ch( 5),~
               at (12,16), fac(lfac$( 5)),amq0$(edtalt%(l%+ 5%)), ch( 5),~
               at (13,16), fac(lfac$( 6)),amq0$(edtalt%(l%+ 6%)), ch( 5),~
               at (14,16), fac(lfac$( 7)),amq0$(edtalt%(l%+ 7%)), ch( 5),~
               at (15,16), fac(lfac$( 8)),amq0$(edtalt%(l%+ 8%)), ch( 5),~
               at (16,16), fac(lfac$( 9)),amq0$(edtalt%(l%+ 9%)), ch( 5),~
               at (17,16), fac(lfac$(10)),amq0$(edtalt%(l%+10%)), ch( 5),~
               at (18,16), fac(lfac$(11)),amq0$(edtalt%(l%+11%)), ch( 5),~
               at (19,16), fac(lfac$(12)),amq0$(edtalt%(l%+12%)), ch( 5),~
               at (20,16), fac(lfac$(13)),amq0$(edtalt%(l%+13%)), ch( 5),~
                                                                         ~
               at (08,22), fac(lfac$( 1)),asu0$(edtalt%(l%+ 1%)), ch( 6),~
               at (09,22), fac(lfac$( 2)),asu0$(edtalt%(l%+ 2%)), ch( 6),~
               at (10,22), fac(lfac$( 3)),asu0$(edtalt%(l%+ 3%)), ch( 6),~
               at (11,22), fac(lfac$( 4)),asu0$(edtalt%(l%+ 4%)), ch( 6),~
               at (12,22), fac(lfac$( 5)),asu0$(edtalt%(l%+ 5%)), ch( 6),~
               at (13,22), fac(lfac$( 6)),asu0$(edtalt%(l%+ 6%)), ch( 6),~
               at (14,22), fac(lfac$( 7)),asu0$(edtalt%(l%+ 7%)), ch( 6),~
               at (15,22), fac(lfac$( 8)),asu0$(edtalt%(l%+ 8%)), ch( 6),~
               at (16,22), fac(lfac$( 9)),asu0$(edtalt%(l%+ 9%)), ch( 6),~
               at (17,22), fac(lfac$(10)),asu0$(edtalt%(l%+10%)), ch( 6),~
               at (18,22), fac(lfac$(11)),asu0$(edtalt%(l%+11%)), ch( 6),~
               at (19,22), fac(lfac$(12)),asu0$(edtalt%(l%+12%)), ch( 6),~
               at (20,22), fac(lfac$(13)),asu0$(edtalt%(l%+13%)), ch( 6),~
                                                                         ~
               at (08,29), fac(lfac$( 1)),arn0$(edtalt%(l%+ 1%)), ch( 6),~
               at (09,29), fac(lfac$( 2)),arn0$(edtalt%(l%+ 2%)), ch( 6),~
               at (10,29), fac(lfac$( 3)),arn0$(edtalt%(l%+ 3%)), ch( 6),~
               at (11,29), fac(lfac$( 4)),arn0$(edtalt%(l%+ 4%)), ch( 6),~
               at (12,29), fac(lfac$( 5)),arn0$(edtalt%(l%+ 5%)), ch( 6),~
               at (13,29), fac(lfac$( 6)),arn0$(edtalt%(l%+ 6%)), ch( 6),~
               at (14,29), fac(lfac$( 7)),arn0$(edtalt%(l%+ 7%)), ch( 6),~
               at (15,29), fac(lfac$( 8)),arn0$(edtalt%(l%+ 8%)), ch( 6),~
               at (16,29), fac(lfac$( 9)),arn0$(edtalt%(l%+ 9%)), ch( 6),~
               at (17,29), fac(lfac$(10)),arn0$(edtalt%(l%+10%)), ch( 6),~
               at (18,29), fac(lfac$(11)),arn0$(edtalt%(l%+11%)), ch( 6),~
               at (19,29), fac(lfac$(12)),arn0$(edtalt%(l%+12%)), ch( 6),~
               at (20,29), fac(lfac$(13)),arn0$(edtalt%(l%+13%)), ch( 6),~
                                                                         ~
               at (08,46), fac(lfac$( 1)),awc1$(edtalt%(l%+ 1%)), ch( 4),~
               at (09,46), fac(lfac$( 2)),awc1$(edtalt%(l%+ 2%)), ch( 4),~
               at (10,46), fac(lfac$( 3)),awc1$(edtalt%(l%+ 3%)), ch( 4),~
               at (11,46), fac(lfac$( 4)),awc1$(edtalt%(l%+ 4%)), ch( 4),~
               at (12,46), fac(lfac$( 5)),awc1$(edtalt%(l%+ 5%)), ch( 4),~
               at (13,46), fac(lfac$( 6)),awc1$(edtalt%(l%+ 6%)), ch( 4),~
               at (14,46), fac(lfac$( 7)),awc1$(edtalt%(l%+ 7%)), ch( 4),~
               at (15,46), fac(lfac$( 8)),awc1$(edtalt%(l%+ 8%)), ch( 4),~
               at (16,46), fac(lfac$( 9)),awc1$(edtalt%(l%+ 9%)), ch( 4),~
               at (17,46), fac(lfac$(10)),awc1$(edtalt%(l%+10%)), ch( 4),~
               at (18,46), fac(lfac$(11)),awc1$(edtalt%(l%+11%)), ch( 4),~
               at (19,46), fac(lfac$(12)),awc1$(edtalt%(l%+12%)), ch( 4),~
               at (20,46), fac(lfac$(13)),awc1$(edtalt%(l%+13%)), ch( 4),~
                                                                         ~
               at (08,51), fac(lfac$( 1)),anm1$(edtalt%(l%+ 1%)), ch( 6),~
               at (09,51), fac(lfac$( 2)),anm1$(edtalt%(l%+ 2%)), ch( 6),~
               at (10,51), fac(lfac$( 3)),anm1$(edtalt%(l%+ 3%)), ch( 6),~
               at (11,51), fac(lfac$( 4)),anm1$(edtalt%(l%+ 4%)), ch( 6),~
               at (12,51), fac(lfac$( 5)),anm1$(edtalt%(l%+ 5%)), ch( 6),~
               at (13,51), fac(lfac$( 6)),anm1$(edtalt%(l%+ 6%)), ch( 6),~
               at (14,51), fac(lfac$( 7)),anm1$(edtalt%(l%+ 7%)), ch( 6),~
               at (15,51), fac(lfac$( 8)),anm1$(edtalt%(l%+ 8%)), ch( 6),~
               at (16,51), fac(lfac$( 9)),anm1$(edtalt%(l%+ 9%)), ch( 6),~
               at (17,51), fac(lfac$(10)),anm1$(edtalt%(l%+10%)), ch( 6),~
               at (18,51), fac(lfac$(11)),anm1$(edtalt%(l%+11%)), ch( 6),~
               at (19,51), fac(lfac$(12)),anm1$(edtalt%(l%+12%)), ch( 6),~
               at (20,51), fac(lfac$(13)),anm1$(edtalt%(l%+13%)), ch( 6),~
                                                                         ~
               at (08,58), fac(lfac$( 1)),awc2$(edtalt%(l%+ 1%)), ch( 4),~
               at (09,58), fac(lfac$( 2)),awc2$(edtalt%(l%+ 2%)), ch( 4),~
               at (10,58), fac(lfac$( 3)),awc2$(edtalt%(l%+ 3%)), ch( 4),~
               at (11,58), fac(lfac$( 4)),awc2$(edtalt%(l%+ 4%)), ch( 4),~
               at (12,58), fac(lfac$( 5)),awc2$(edtalt%(l%+ 5%)), ch( 4),~
               at (13,58), fac(lfac$( 6)),awc2$(edtalt%(l%+ 6%)), ch( 4),~
               at (14,58), fac(lfac$( 7)),awc2$(edtalt%(l%+ 7%)), ch( 4),~
               at (15,58), fac(lfac$( 8)),awc2$(edtalt%(l%+ 8%)), ch( 4),~
               at (16,58), fac(lfac$( 9)),awc2$(edtalt%(l%+ 9%)), ch( 4),~
               at (17,58), fac(lfac$(10)),awc2$(edtalt%(l%+10%)), ch( 4),~
               at (18,58), fac(lfac$(11)),awc2$(edtalt%(l%+11%)), ch( 4),~
               at (19,58), fac(lfac$(12)),awc2$(edtalt%(l%+12%)), ch( 4),~
               at (20,58), fac(lfac$(13)),awc2$(edtalt%(l%+13%)), ch( 4),~
                                                                         ~
               at (08,63), fac(lfac$( 1)),anm2$(edtalt%(l%+ 1%)), ch( 6),~
               at (09,63), fac(lfac$( 2)),anm2$(edtalt%(l%+ 2%)), ch( 6),~
               at (10,63), fac(lfac$( 3)),anm2$(edtalt%(l%+ 3%)), ch( 6),~
               at (11,63), fac(lfac$( 4)),anm2$(edtalt%(l%+ 4%)), ch( 6),~
               at (12,63), fac(lfac$( 5)),anm2$(edtalt%(l%+ 5%)), ch( 6),~
               at (13,63), fac(lfac$( 6)),anm2$(edtalt%(l%+ 6%)), ch( 6),~
               at (14,63), fac(lfac$( 7)),anm2$(edtalt%(l%+ 7%)), ch( 6),~
               at (15,63), fac(lfac$( 8)),anm2$(edtalt%(l%+ 8%)), ch( 6),~
               at (16,63), fac(lfac$( 9)),anm2$(edtalt%(l%+ 9%)), ch( 6),~
               at (17,63), fac(lfac$(10)),anm2$(edtalt%(l%+10%)), ch( 6),~
               at (18,63), fac(lfac$(11)),anm2$(edtalt%(l%+11%)), ch( 6),~
               at (19,63), fac(lfac$(12)),anm2$(edtalt%(l%+12%)), ch( 6),~
               at (20,63), fac(lfac$(13)),anm2$(edtalt%(l%+13%)), ch( 6),~
                                                                         ~
               at (08,70), fac(lfac$( 1)),awc3$(edtalt%(l%+ 1%)), ch( 4),~
               at (09,70), fac(lfac$( 2)),awc3$(edtalt%(l%+ 2%)), ch( 4),~
               at (10,70), fac(lfac$( 3)),awc3$(edtalt%(l%+ 3%)), ch( 4),~
               at (11,70), fac(lfac$( 4)),awc3$(edtalt%(l%+ 4%)), ch( 4),~
               at (12,70), fac(lfac$( 5)),awc3$(edtalt%(l%+ 5%)), ch( 4),~
               at (13,70), fac(lfac$( 6)),awc3$(edtalt%(l%+ 6%)), ch( 4),~
               at (14,70), fac(lfac$( 7)),awc3$(edtalt%(l%+ 7%)), ch( 4),~
               at (15,70), fac(lfac$( 8)),awc3$(edtalt%(l%+ 8%)), ch( 4),~
               at (16,70), fac(lfac$( 9)),awc3$(edtalt%(l%+ 9%)), ch( 4),~
               at (17,70), fac(lfac$(10)),awc3$(edtalt%(l%+10%)), ch( 4),~
               at (18,70), fac(lfac$(11)),awc3$(edtalt%(l%+11%)), ch( 4),~
               at (19,70), fac(lfac$(12)),awc3$(edtalt%(l%+12%)), ch( 4),~
               at (20,70), fac(lfac$(13)),awc3$(edtalt%(l%+13%)), ch( 4),~
                                                                         ~
               at (08,75), fac(lfac$( 1)),anm3$(edtalt%(l%+ 1%)), ch( 6),~
               at (09,75), fac(lfac$( 2)),anm3$(edtalt%(l%+ 2%)), ch( 6),~
               at (10,75), fac(lfac$( 3)),anm3$(edtalt%(l%+ 3%)), ch( 6),~
               at (11,75), fac(lfac$( 4)),anm3$(edtalt%(l%+ 4%)), ch( 6),~
               at (12,75), fac(lfac$( 5)),anm3$(edtalt%(l%+ 5%)), ch( 6),~
               at (13,75), fac(lfac$( 6)),anm3$(edtalt%(l%+ 6%)), ch( 6),~
               at (14,75), fac(lfac$( 7)),anm3$(edtalt%(l%+ 7%)), ch( 6),~
               at (15,75), fac(lfac$( 8)),anm3$(edtalt%(l%+ 8%)), ch( 6),~
               at (16,75), fac(lfac$( 9)),anm3$(edtalt%(l%+ 9%)), ch( 6),~
               at (17,75), fac(lfac$(10)),anm3$(edtalt%(l%+10%)), ch( 6),~
               at (18,75), fac(lfac$(11)),anm3$(edtalt%(l%+11%)), ch( 6),~
               at (19,75), fac(lfac$(12)),anm3$(edtalt%(l%+12%)), ch( 6),~
               at (20,75), fac(lfac$(13)),anm3$(edtalt%(l%+13%)), ch( 6),~
                                                                         ~
               at (08,38), fac(lfac$( 1)),asht$(edtalt%(l%+ 1%)), ch( 7),~
               at (09,38), fac(lfac$( 2)),asht$(edtalt%(l%+ 2%)), ch( 7),~
               at (10,38), fac(lfac$( 3)),asht$(edtalt%(l%+ 3%)), ch( 7),~
               at (11,38), fac(lfac$( 4)),asht$(edtalt%(l%+ 4%)), ch( 7),~
               at (12,38), fac(lfac$( 5)),asht$(edtalt%(l%+ 5%)), ch( 7),~
               at (13,38), fac(lfac$( 6)),asht$(edtalt%(l%+ 6%)), ch( 7),~
               at (14,38), fac(lfac$( 7)),asht$(edtalt%(l%+ 7%)), ch( 7),~
               at (15,38), fac(lfac$( 8)),asht$(edtalt%(l%+ 8%)), ch( 7),~
               at (16,38), fac(lfac$( 9)),asht$(edtalt%(l%+ 9%)), ch( 7),~
               at (17,38), fac(lfac$(10)),asht$(edtalt%(l%+10%)), ch( 7),~
               at (18,38), fac(lfac$(11)),asht$(edtalt%(l%+11%)), ch( 7),~
               at (19,38), fac(lfac$(12)),asht$(edtalt%(l%+12%)), ch( 7),~
               at (20,38), fac(lfac$(13)),asht$(edtalt%(l%+13%)), ch( 7),~
                                                                         ~
               at (08,36), fac(lfac$( 1)),amqp$(edtalt%(l%+ 1%)), ch( 1),~
               at (09,36), fac(lfac$( 2)),amqp$(edtalt%(l%+ 2%)), ch( 1),~
               at (10,36), fac(lfac$( 3)),amqp$(edtalt%(l%+ 3%)), ch( 1),~
               at (11,36), fac(lfac$( 4)),amqp$(edtalt%(l%+ 4%)), ch( 1),~
               at (12,36), fac(lfac$( 5)),amqp$(edtalt%(l%+ 5%)), ch( 1),~
               at (13,36), fac(lfac$( 6)),amqp$(edtalt%(l%+ 6%)), ch( 1),~
               at (14,36), fac(lfac$( 7)),amqp$(edtalt%(l%+ 7%)), ch( 1),~
               at (15,36), fac(lfac$( 8)),amqp$(edtalt%(l%+ 8%)), ch( 1),~
               at (16,36), fac(lfac$( 9)),amqp$(edtalt%(l%+ 9%)), ch( 1),~
               at (17,36), fac(lfac$(10)),amqp$(edtalt%(l%+10%)), ch( 1),~
               at (18,36), fac(lfac$(11)),amqp$(edtalt%(l%+11%)), ch( 1),~
               at (19,36), fac(lfac$(12)),amqp$(edtalt%(l%+12%)), ch( 1),~
               at (20,36), fac(lfac$(13)),amqp$(edtalt%(l%+13%)), ch( 1),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys (pfkeys$),                                           ~
               key  (keyhit%)

               if keyhit% <> 13 then L47605
                  call "MANUAL" ("RTEINPUT")
                  goto L46440

L47605:        if keyhit% <> 15 then L47625
                  call "PRNTSCRN"
                  goto L46440

L47625:        if ss% <> 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *        T E X T   M A N A G E M E N T   R O U T I N E      *~
            *************************************************************
        deffn'181(i%)
            u3% = lines_allowed% + 1%
            convert c% to s$, pic(###)
            textmsg$ = part$ & " " & partdescr$ & ", Rte " & rte$
            if i% <> u3% then textmsg$ = textmsg$ & ", Step " & s$
            if i% <> u3% then src$ = "008" else src$ = "007"
            if noedit% = 1% then                                         ~
            call "TXTDSPLY" (#9, f2%(9), src$, textmsg$, textid$(i%),    ~
                                                           texta$())     ~
                     else                                                ~
            call "TXTINSUB" (#9, f2%(9), src$, textmsg$, textid$(i%),    ~
                                                           texta$())
            return

        REM *************************************************************~
            * Special Administrator-only routine to override no mod flag*~
            *************************************************************
        override
            if noedit% <> 0% then L49260
                print at(02,79)," ";
                print bell
                return

L49260: REM Show the user a warning before letting him/her have edit power

            keyhit% = 2% /* Window at bottom */
            call "ASKUSER" (keyhit%, "*** PROCEED WITH CAUTION ***",     ~
                "This option will permit you to MODIFY A ROUTE that is "&~
                "flagged to hold as is.", "Press (RETURN) to abort this"&~
                " option.    -- OR --", "Press PF(24) to exercise this "&~
                "option.")
                if keyhit% <> 15 then L49410
                   call "PRNTSCRN"
                   goto L49260
L49410:         if keyhit% <> 24% then goto L49420
                    noedit% = 0%
                    information$ = " "
                    goto L49430
L49420:         if keyhit% <>  0% then goto L49260
L49430:         keyhit% = 99
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, information$ = " "
                  on fieldnr% gosub L50105,         /* PART             */~
                                    L50135          /* WC ROUTE         */
                     return
L50105:    REM TEST DATA FOR PART CODE
               call "GETCODE" (#2, part$, partdescr$, 1%, 0, f1%(2))
                  if f1%(2) = 1 then return
                  errormsg$ = "Part Not On File: " & part$
                  return
L50135: REM TEST DATA FOR WORK CENTER ROUTE & LOAD OLD DATA IF THERE
               if rte$ <> " " then goto L50220
               errormsg$ = "Work Center Routing ID Can't Be Blank"
                     return

L50220:         REM NOW TRY FOR IT IN RTEMASTR FILE.
                    gosub check_usage
                    if used% = 1 then information$ = hex(84) &           ~
                   "Can be reviewed but not modified: " & errormsg$
                    errormsg$ = " "
                    if used% = 1 then noedit% = 1
                    gosub L30000          /* LOAD RTE RECORDS.  */
                    if maxlines% = 0 then noedit% = 0
                    if maxlines% = 0 then return
                       return clear all
                       goto editmode     /* EDIT ROUTING MODE. */

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M S      *~
            *                                                           *~
            * TESTS DATA FOR THE LINE ITEM INFORMATION, INCLUDING       *~
            * VALID WORK CENTER, AND POSITIVE MQ, SU, AND RUN TIMES     *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51110,         /* STEP NUMBER      */~
                                    L51200,         /* WORK CENTER      */~
                                    L51275,         /* STEP TIMES       */~
                                    L51310,         /* STEP TIMES       */~
                                    L51425,         /* STEP TIMES       */~
                                    L51635,         /* EXPECT YIELD     */~
                                    L51655,         /* SU ACTIVITY CODE */~
                                    L51695,         /* ACTIVITY CODE    */~
                                    L51735,         /* ACTIVITY DESCR   */~
                                    L51750,         /* % Complete       */~
                                    L51900          /* Handling Factor  */

                  return

L51110:     REM TEST DATA FOR STEP NUMBER
                if step$(c%) <> " " then L51130
                     errormsg$ = "Sorry, Step May Not Be Blank"
                     return
L51130:         if step$(c%) <> "DONE" then L51160
                     errormsg$ = "Sorry, 'DONE' Is a Reserved Label"
                     return
L51160:         convert step$(c%) to temp%, data goto L51170 /* Alpha? */
                convert step$(c%) to temp
                if temp <> int(temp) then L51170            /* Decimal */
                convert temp% to step$(c%), pic (0000) /* RJ numerics */
L51170:         mat loc% = zer
                search step$() = str(step$(c%),,4) to loc%() step 4
                if loc%(2) = 0 then return
                errormsg$ = "This Step ID Is Already Used On This Routing~
        ~: " & step$(c%)
                return
L51200:     REM TEST DATA FOR WORK CENTER
                if wc$(c%) <> "VEND" then L51235
                     init (" ") wc1$(c%), wc2$(c%), wc3$(c%), shift$(c%),~
                                nm1$(c%), nm2$(c%), nm3$(c%), mqp$(c%),  ~
                                ca1$(c%), ca2$(c%), ca3$(c%)
                                shift$(c%) = "1.00000"
                     return
L51235:         call "GETCODE" (#3, wc$(c%), wcdescr$(1), 1%, 0, f1%(3))
                if f1%(3%) <> 0% then L51255
                     errormsg$ = "Work Center Not On File"
                     return
L51255:         gosub scrnuch_conc_wcs
                gosub get_wc_defaults
                return

L51275:     REM TEST DATA FOR MOVE/QUEUE TIME
                call "NUMTEST" (mq$(c%), 0, 999, errormsg$, -0.01, mq)
                     if errormsg$ <> " " then return
                if mq = 0 then mqp$(c%) = " "
                if mqp$(c%) <> "N" then mqp$(c%) = "Y"
                return

L51310:     REM TEST DATA FOR SET UP TIME
                REM Decide which way the time was entered...
                if savesuh$ <> " " then L51325
                     if suh$(c%) <> " " then L51360
                     goto L51330
L51325:         if savesuh$ <> suh$(c%) then L51360

L51330:         REM Entered In Wcunits...
                call "NUMTEST" (su$(c%), 0, 65000, errormsg$, -0.01,test)
                     if errormsg$ <> " " then return
                call "WCUN2HRS" (#3, wc$(c%), 0, test, " ")
                goto L51410

L51360:         REM Entered In Hours...
                savesuh$ = suh$(c%)
                if pos(suh$(c%) = ":") = 0 then L51380
                     temp$ = suh$(c%) : gosub L51810 : suh$(c%) = temp$
L51380:         temp% = 0 /* Must be even mutiple of factor */
                call "NUMTEST" (suh$(c%), 0, 65000, errormsg$,-2.4,test)
                savesuh$ = suh$(c%)
                     if errormsg$ <> " " then return
                gosub L51770
                call "CONVERT" (test1, 0.0, su$(c%))
L51410:         call "CONVERT" (test, 2.4, suh$(c%))
                return

L51425:     REM TEST DATA FOR RUN TIME
                REM Decide which way the time was entered...
                if saverunh$ <> " " or saverun$ <> " " then L51441
                    if runh$(c%) <> " " then L51525
                    if run$(c%)  <> " " then L51490
                    goto L51450
L51441:         if saverunh$ <> runh$(c%) then L51525
                    if saverun$ <> run$(c%) then L51490

L51450:         REM Entered In Parts Per Hour...
                call "NUMTEST" (runp$, 0, 9e6, errormsg$, -0.6, runp)
                   if errormsg$ <> " " then return
                test = 0
                if runp <> 0 then test = 1/runp
                call "CONVERT" (test, -4.6, runh$(c%))
                goto L51525

L51490:         REM Entered In Wcunits...
                call "NUMTEST" (run$(c%), 0, 9e6, errormsg$, -0.4, test)
                saverun$ = run$(c%)
                   if errormsg$ <> " " then return
                call "WCUN2HRS" (#3, wc$(c%), 0, test, " ")
                goto L51580

L51525:         REM Entered In Hours...
                saverunh$ = runh$(c%)
                if pos(runh$(c%) = ":") = 0 then L51550
                     temp$ = runh$(c%)
                     gosub L51810 : runh$(c%) = temp$
L51550:         temp% = 4 /* Must be even mutiple of factor to 4 Places*/
                call "NUMTEST" (runh$(c%), 0, 9e6, errormsg$, -4.6, test)
                saverunh$ = runh$(c%)
                   if errormsg$ <> " " then return
                gosub L51770
                call "CONVERT" (test1, 0.4, run$(c%))
L51580:         call "CONVERT" (test, 4.6, runh$(c%))
                runp = 0
                if test <> 0 then runp = 1/test
                call "CONVERT" (runp, 0.6, runp$)
                if wc$(c%) <> "VEND" then return
                mq,su = 0
                convert mq$(c%) to mq, data goto L51615
L51615:         convert su$(c%) to su, data goto L51620
L51620:         if mq+su+test = 0 then mq$(c%) = "1"
                return

L51635:     REM TEST DATA FOR YIELD PERCENTAGE
                call "NUMTEST" (yield$(c%), 1, 100, errormsg$,-.01, 0)
                return

L51655
*        Test data for SETUP ACTIVITY CODE
            convert su$(c%) to temp
            if sucode$(c%) = " " and temp = 0 then return
            sudescr$ = hex(06) & "Select Set-Up Activity Code"
            tempkey$ = "WC ACTVTY" & sucode$(c%)
            call "PLOWCODE" (#12, tempkey$, sudescr$, 9%, 0.3, f1%(12))
                if f1%(12%) <> 0% then L51675
                    if sucode$(c%) = "?" then sucode$(c%) = " "
                    goto L51680
L51675:     sucode$(c%) = str(tempkey$,10)
L51680:     if sucode$(c%) = " " then sudescr$ = " "
            return

L51695
*        Test data for ACTIVITY CODE
            convert run$(c%) to temp
            if actcode$(c%) = " " and temp = 0 then return
            actdescr$ = hex(06) & "Select Run Activity Code"
            tempkey$ = "WC ACTVTY" & actcode$(c%)
            call "PLOWCODE" (#12, tempkey$, actdescr$, 9%,.3, f1%(12))
                if f1%(12%) <> 0% then L51715
                    if actcode$(c%) = "?" then actcode$(c%) = " "
                    goto L51720
L51715:     actcode$(c%) = str(tempkey$,10)
L51720:     if actcode$(c%) = " " then actdescr$ = " "
            return

L51735:    REM TEST DATA FOR ACTIVITY DESCRIPTION
                return

L51750:     REM TEST DATA FOR PERCENTAGE COMPLETE
                call "NUMTEST" (comp$(c%), 0, 100, errormsg$, -.01, 0)
                return

L51770:     REM Simulates Datasave Effect to Insure no surprizes later...
            adjfactor = .00000001
            call "WCUN2HRS" (#3, wc$(c%), factor, 0, " ")
            if factor <> 0 then adjfactor = 24/factor
            test1, test = round(test/adjfactor, temp%)
            call "WCUN2HRS" (#3, wc$(c%), 0, test, " ")
            return

L51810:     REM Take Time Entered As HH;MM;SS and convert to decimal...
            call "TIMEOK" (temp$, test, errormsg$)
            if errormsg$ = " " then L51825
                    return clear   :   return
L51825:     call "CONVERT" (test, 0.4, str(temp$,,8))
            return

L51900:     REM TEST DATA FOR HANDLING FACTOR
             call "NUMTEST" (handfactor$(c%), -9999999, 99999999,        ~
                             errormsg$, -2.2, temp1)
             if errormsg$ <> " " then return
             if temp1 <> 0 then  return
             errormsg$ = "The Factor Cannot Be 0."
             return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M S      *~
            *                                                           *~
            * TESTS DATA FOR THE LINE ITEM INFORMATION, INCLUDING       *~
            * VALID WORK CENTER, AND POSITIVE MQ, SU, AND RUN TIMES     *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52240,       /* Shift Differential */~
                                    L52320,       /* Concurrent WC One  */~
                                    L52410,       /* Capacity adj. Factr*/~
                                    L52460,       /* Activity Code      */~
                                    L52530,       /* Concurrent WC Two  */~
                                    L52620,       /* Capacity adj. Factr*/~
                                    L52670,       /* Activity Code      */~
                                    L52740,       /* Concurrent WC Three*/~
                                    L52900,       /* Capacity adj. Factr*/~
                                    L52950,       /* Activity Code      */~
                                    L53020,       /* FORCE CONTIGUITY   */~
                                    L53070,       /* 1st OVERLAP FACTOR */~
                                    L53130        /* 2st OVERLAP FACTOR */
                  return

L52240:    REM TEST DATA FOR SHIFT DIFFERENTIAL
               if shift$(c%) = " " then shift$(c%) = "1"
               call "NUMTEST" (shift$(c%), 0, 1, errormsg$, 5.5, shift)
                   if errormsg$ <> " " then return
               if shift > 0 then return
               shift$(c%) = "1.00000"
               return

L52320:    REM TEST DATA FOR CONCURRENT WC ONE
               if wc1$(c%) <> " " then L52350
                  nm1$(c%), ca1$(c%) = " " : goto scrnuch_conc_wcs
L52350:        call "GETCODE" (#3, wc1$(c%), wcdescr$(2), 1%, 0, f1%(3))
                    if f1%(3) = 0 then L52830
               if wc1$(c%) = wc$(c%) then L52850
               if wc1$(c%) = wc2$(c%) or wc1$(c%) = wc3$(c%) then L52870
               return

L52410:    REM TEST DATA FOR CAPACITY ADJ. FACTOR
               if wc1$(c%) = " " then return
               call "NUMTEST" (nm1$(c%), .0001, 9e6, errormsg$,-0.4, nm1)
               return

L52460
*        Test data for ACTIVITY CODE
            if wc1$(c%) = " " then return
               cadescr$(1) = hex(94) & "(Will Be Created)"
               tempkey$ = "WC ACTVTY" & ca1$(c%)
               call "PLOWCODE" (#12, tempkey$, cadescr$(1), 9%,.3,f1%(12))
               ca1$(c%) = str(tempkey$,10)
               if ca1$(c%) = " " then cadescr$(1) = " "
               return

L52530:    REM TEST DATA FOR CONCURRENT WC TWO
               if wc2$(c%) <> " " then L52560
                  nm2$(c%), ca2$(c%) = " " : goto scrnuch_conc_wcs
L52560:        call "GETCODE" (#3, wc2$(c%), wcdescr$(3), 1%, 0, f1%(3))
                    if f1%(3) = 0 then L52830
               if wc2$(c%) = wc$(c%) then L52850
               if wc2$(c%) = wc3$(c%) or wc2$(c%) = wc1$(c%) then L52870
               return

L52620:    REM TEST DATA FOR CAPACITY ADJ. FACTOR
               if wc2$(c%) = " " then return
               call "NUMTEST" (nm2$(c%), .0001, 9e6, errormsg$,-0.4, nm2)
               return

L52670
*        Test data for ACTIVITY CODE
            if wc2$(c%) = " " then return
               cadescr$(2) = hex(94) & "(Will Be Created)"
               tempkey$ = "WC ACTVTY" & ca2$(c%)
               call "PLOWCODE" (#12, tempkey$, cadescr$(2), 9%,.3,f1%(12))
               ca2$(c%) = str(tempkey$,10)
               if ca2$(c%) = " " then cadescr$(2) = " "
               return

L52740:    REM TEST DATA FOR CONCURRENT WC THREE
               if wc3$(c%) <> " " then L52770
                  nm3$(c%), ca3$(c%) = " " : goto scrnuch_conc_wcs
L52770:        call "GETCODE" (#3, wc3$(c%), wcdescr$(4), 1%, 0, f1%(3))
                    if f1%(3) = 0 then L52830
               if wc3$(c%) = wc$(c%) then L52850
               if wc3$(c%) = wc2$(c%) or wc3$(c%) = wc1$(c%) then L52870
               return

L52830:        errormsg$ = "Work Center Not On File"
               return
L52850:        errormsg$ = "Can't Be Same As Primary Work Center"
               return
L52870:        errormsg$ = "Can't Be Same As Concurrent Already On List"
               return

L52900:    REM TEST DATA FOR CAPACITY ADJ. FACTOR
               if wc3$(c%) = " " then return
               call "NUMTEST" (nm3$(c%), .0001, 9e6, errormsg$,-0.4, nm3)
               return

L52950
*        Test data for ACTIVITY CODE
            if wc3$(c%) = " " then return
               cadescr$(3) = hex(94) & "(Will Be Created)"
               tempkey$ = "WC ACTVTY" & ca3$(c%)
               call "PLOWCODE" (#12, tempkey$, cadescr$(3), 9%,.3,f1%(12))
               ca3$(c%) = str(tempkey$,10)
               if ca3$(c%) = " " then cadescr$(3) = " "
               return

L53020:    REM TEST DATA FOR FORCE CONTIGUITY FLAG
                if pos("YN" = cg$(c%)) = 0 then errormsg$ =              ~
                                               "Please Enter 'Y' or 'N'."
                return

L53070:    REM TEST DATA FOR FIRST OVERLAP FACTORS
                call "NUMTEST" (f1$(c%,1%), 0, 9e7, errormsg$,-0.2, 0)
                     if errormsg$ <> " " then return
                call "NUMTEST" (f1$(c%,2%), 0, 100, errormsg$,-0.2, 0)
                return

L53130:    REM TEST DATA FOR SECOND OVERLAP FACTORS
                call "NUMTEST" (f2$(c%,1%), 0, 9e7, errormsg$,-0.2, 0)
                     if errormsg$ <> " " then return
                call "NUMTEST" (f2$(c%,2%), 0, 100, errormsg$,-0.2, 0)
                return

        scrnuch_conc_wcs
            if wc1$(c%) = wc$(c%) then wc1$(c%) = " "
            if wc2$(c%) = wc$(c%) then wc2$(c%) = " "
            if wc3$(c%) = wc$(c%) then wc3$(c%) = " "
            if wc1$(c%)<>" " or wc2$(c%)<>" " or wc1$(c%)<>" " then L53260
               nm1$(c%),nm2$(c%),nm3$(c%),ca1$(c%),ca2$(c%),ca3$(c%)=" "
               goto L53340
L53260:     if wc1$(c%) <> " " then L53290
               wc1$(c%)=wc2$(c%) : nm1$(c%)=nm2$(c%) : ca1$(c%)=ca2$(c%)
               wc2$(c%)=" "
L53290:     if wc2$(c%) <> " " then L53320
               wc2$(c%)=wc3$(c%) : nm2$(c%)=nm3$(c%) : ca2$(c%)=ca3$(c%)
               wc3$(c%), nm3$(c%), ca3$(c%) = " "
L53320:     if wc3$(c%) <> " " then L53340
               nm3$(c%), ca3$(c%) = " "
L53340:        gosub describe_line
               return

        get_wc_defaults
            defaults% = 0%
            readkey$ = str(wc$(c%)) & " "
            call "READ100" (#13, readkey$, f1%(13%))
                if f1%(13%) <> 1% then return
            get #13 using L53470, def_mq, def_mqp$, def_handfactor,       ~
                         def_su, def_run, def_yield, def_sucode$,        ~
                         def_rucode$, def_rudescr$, def_comp
L53470:     FMT POS(6), BI(4), CH(1), PD(14,4), BI(4), PD(14,4), BI(4),  ~
                2*CH(4), CH(30), BI(4)
            defaults% = 1%
            return

        REM *************************************************************~
            *                                                           *~
            * TEST SECTION FOR ALTERNATE STEPS, CONCURRENT WC'S         *~
            *                                                           *~
            *************************************************************

        deffn'154(ss%)
            errormsg$=" "

        REM VALIDATION FOR ALTERNATE ROUTE STEP
            i% = edtalt%(cc%)
            if awc0$(i%) = "VEND" then L59150
               call "GETCODE" (#3, awc0$(i%), " ", 0%, 0, f1%(3))
                 if f1%(3) <> 0% then L59150
                 errormsg$="W/C Not On File:" & awc0$(i%):return
L59150:     REM TEST DATA FOR MOVE/QUEUE TIME
                call "NUMTEST" (amq0$(i%), 0, 9e6, errormsg$, -0.01, mq)
                   if errormsg$ <> " " then return
            REM TEST DATA FOR SET UP TIME
                call "NUMTEST" (asu0$(i%), 0, 65000, errormsg$, -0.01,su)
                   if errormsg$ <> " " then return
            REM TEST DATA FOR RUN TIME
                call "NUMTEST" (arn0$(i%), 0, 9e6, errormsg$, -0.4, run)
                   if errormsg$ <> " " then return
                if awc0$(i%) <> "VEND" then L59280
                     if mq+su+run = 0 then amq0$(i%) = "    1"
                     goto L59280

L59280: REM TEST FOR CONCURRENT WC'S IN ALTERNATE
            if awc0$(i%) = "VEND" then awc1$(i%) = " "
            if awc0$(i%) = "VEND" then asht$(i%) = " "
            if awc1$(i%) = "VEND" then awc1$(i%) = " "
            if awc2$(i%) = "VEND" then awc2$(i%) = " "
            if awc3$(i%) = "VEND" then awc3$(i%) = " "
            if awc3$(i%) = awc0$(i%) then awc3$(i%) = " "
            if awc3$(i%) = awc1$(i%) then awc3$(i%) = " "
            if awc3$(i%) = awc2$(i%) then awc3$(i%) = " "
            if awc2$(i%) = awc0$(i%) then awc2$(i%) = " "
            if awc2$(i%) = awc1$(i%) then awc2$(i%) = " "
            if awc1$(i%) = awc0$(i%) then awc1$(i%) = " "

            if awc1$(i%) <> " " then L59450
              init (" ") awc1$(i%), awc2$(i%), awc3$(i%),                ~
                         anm1$(i%), anm2$(i%), anm3$(i%)

L59450:     if awc2$(i%) <> " " then L59480
              init (" ") awc2$(i%), awc3$(i%), anm2$(i%), anm3$(i%)

L59480:     if awc3$(i%) <> " " then L59510
              init (" ") awc3$(i%), anm3$(i%)

L59510:     if amqp$(i%) <> " " then amqp$(i%) = "Y"

            if asht$(i%) = " " then asht$(i%) = "1"
L59540:     call "NUMTEST" (asht$(i%), 0, 1, errormsg$, 5.5, shift)
                   if errormsg$ <> " " then return
            if shift > 0 then L59600
               asht$(i%) = "1"
               goto L59540

L59600:     if awc1$(i%)=" " then return
               call "PLOWNEXT" (#3, str(awc1$(i%))&hex(00), 4%, f1%(3))
                 if f1%(3) <> 0% then L59640
                 errormsg$="W/C #1 Not On File:" & awc1$(i%):return
L59640:        call "NUMTEST" (anm1$(i%), .0001,9e6, errormsg$, 0.4, nm1)
                 if errormsg$ <> " " then return

            if awc2$(i%)=" " then return
               call "PLOWNEXT" (#3, str(awc2$(i%))&hex(00), 4%, f1%(3))
                 if f1%(3) <> 0% then L59710
                 errormsg$="W/C #2 Not On File:" & awc2$(i%):return
L59710:        call "NUMTEST" (anm2$(i%), .0001,9e6, errormsg$, 0.4, nm2)
                 if errormsg$ <> " " then return

            if awc3$(i%)=" " then return
               call "PLOWNEXT" (#3, str(awc3$(i%))&hex(00), 4%, f1%(3))
                 if f1%(3) <> 0% then L59780
                 errormsg$="W/C #3 Not On File:" & awc3$(i%):return
L59780:        call "NUMTEST" (anm3$(i%), .0001,9e6, errormsg$, 0.4, nm3)
               return

        view_ecr_info                    /* View ECR History this Part */
            call "ECRINQSB" ("A",        /* "A" - Show ALL ECRs this   */~
                                         /*    part, open or closed.   */~
                             part$,      /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                             #20,        /* SYSFILE2                   */~
                             #02)        /* HNYMASTR                   */
            return

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
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

            call "SHOSTAT" ("One Moment Please")
            end
