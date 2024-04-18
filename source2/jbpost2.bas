        REM THISPROGRAMWASGENERATEDUSINGTHEGENMENUPROGRAMWHICHISAPROPRIET~
            *                                                           *~
            *  JJJJJ  BBBB   PPPP    OOO    SSS   TTTTT   222           *~
            *    J    B   B  P   P  O   O  S        T    2   2          *~
            *    J    BBBB   PPPP   O   O   SSS     T       2           *~
            *  J J    B   B  P      O   O      S    T     2             *~
            *   JJ    BBBB   P       OOO    SSS     T    22222          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBPOST2 - Report completion of job, various other value   *~
            *           movements to/from job. See 10000 block.         *~
            *           One of multiple background posting tasks        *~
            *           servicing SFC. Runs in foreground just as well. *~
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
            * 09/27/85 ! ORIGINAL                                 ! HES *~
            * 04/07/86 ! Corrected Major Bug in Rpt Comp Logic    ! HES *~
            * 02/19/87 ! Changes for Serial Number Handling       ! LDJ *~
            *          ! Added error messages describing causes   !     *~
            *          ! for various cancel conditions.           !     *~
            * 06/11/87 ! Standard Costing Changes                 ! ERN *~
            * 11/09/87 ! Inialize JBCOST variable                 ! HES *~
            * 02/04/88 ! Remvd HNYHOLD,also fix @DECREMENT_PIPOUTS! HES *~
            * 08/15/88 ! Update ONORDER quantity for SCR, RWK, ADJ! TLJ *~
            * 03/28/89 ! Fixed decrementing of negative pipouts on!     *~
            *          ! by-products                              ! MLJ *~
            * 04/27/89 ! Corrected File # for HNYMASTR in call to ! MJB *~
            *          !  LOTTRACK at 26170.                      !     *~
            * 01/03/90 ! Modified code to combine like accounts   ! LAB *~
            *          ! for gl postings.  Accountancy Phase I    !     *~
            * 10/16/90 ! Merged GL Export Option & A/C I.         ! JDH *~
            * 11/01/90 ! Switched cr & cr fields in call to       ! MJB *~
            *          !  GLPRTSUB at in 25210 and 25370          !     *~
            * 05/21/91 ! Added Alt Key to the JBMASTR2. Thanks Sid! JDH *~
            *          ! Removed ref. to File #23 HNYRWKPF.Thx WPH!     *~
            *          ! No reads & puts if GL Export is off.     !     *~
            * 08/01/91 ! PRR 12109.  Correctly set tran type for  ! JDH *~
            *          !   byproducts.                            !     *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 04/20/92 ! PRR 12405. Init Est % Cmplt to 0 for Rwk.! JDH *~
            * 07/28/92 ! MPS/PFM - Added call to HNYUSESB.        ! MLJ *~
            * 05/15/93 ! Core Project                             ! KAB *~
            *          ! Post Core Ledgers (JBVLPOST, JBMTPOST,   !     *~
            *          !   JBCRPOST) [Gross Understatement]       !     *~
            *          ! Handle RWK PIPIN, On Order via Flag      !     *~
            *          ! Allow (via JBCRPOST) block of negative   !     *~
            *          !  costs (flag controlled)                 !     *~
            *          ! Don't Save Close Batch Records           !     *~
            *          ! Detect & Trap WIP / HNY ASSET Variance   !     *~
            *          ! Remove Obsolete HNYADJPF                 !     *~
            *          ! Format Close Adjustment Area for JBMSTR2 !     *~
            *          ! Pass new valuation Method to JBCRPOST    !     *~
            *          ! Decrement PIPOUT when comp JOB -> JOB    !     *~
            *          ! Note - System now supports comp at STD.  !     *~
            * 08/11/93 ! Purchased Jobs - Now gets rework job info! MLJ *~
            *          !  from new fields in SWITCHS.SFC instead  !     *~
            *          !  of calling RWKINFO.                     !     *~
            * 01/14/94 ! Added to GLCMBSUB argument list.         ! JDH *~
            * 08/15/94 ! Corrected problems if Next Rework Job #  ! JDH *~
            *          !  is blank. Note-Set via JBFLAGS. Thks KAB!     *~
            * 05/31/95 ! PRRs 11211,11701. For scrap, create      ! JDH *~
            *          !  HNYQUAN prior to HNYPST2 with scrap     !     *~
            *          !  asset acct and don't stomp on existing  !     *~
            *          !  asset account.                          !     *~
            * 10/07/95 ! Corrected dim of var passed to HNYPROCU. ! JDH *~
            * 08/13/96 ! Changes for the year 2000.               ! DXL *~
            ARYPRODUCTOFCAELUSASSOCIATESSPOKANEWAALLRIGHTSRESERVEDGENMENU

        dim acct$(4)9,                   /* Accounts For GLPOST Routine*/~
            activity$4,                  /* Activity                   */~
            apst(12), apst$(13)9,        /* HNYAPST costs and accts    */~
            auto%(2),                    /* Work Array                 */~
            blankdate$8,                 /* Blank Date for Comparison. */~
            bomid$3,                     /* Built using BOMID          */~
            cmbtext$109,                 /* TEXT FORM GLCMBSUB         */~
            comp_loc$30,                 /* Component S/N Location     */~
            comp_stat$1,                 /* Component S/N Status Code  */~
            corepart$25,                 /* Core Part Number           */~
            costs(12),costs$96,rcst(12), /* Inventory costs            */~
            cbom(12), cshd(12),          /* Core costs                 */~
            ccdt(12), cwip(12),          /* Core costs                 */~
            coreactive$1,                /* Core Active?               */~
            costflag$2,                  /* JBCRPOST cost flag         */~
            comperrcst$1,                /* JBCRPOST cost error flag   */~
            datetime$7,                  /* Date and Time Stamp        */~
            dfcore_var$9,                /* System Default Core Var.   */~
            dfcore_fga$9,                /* System Default Core FGA    */~
            dfcore_wip$9,                /* System Default Core WIP    */~
            earntype$12,                 /* Payroll Earnings Type      */~
            emp$12,                      /* Employee Code              */~
            errortext$20,                /* Bad Trans text             */~
            flotmisc$9, tlotmisc$9,      /* Lot Track Misc Fields      */~
            glamount(4),                 /* GL AMOUNTS TO POST         */~
            gllog$1,                     /* Write Supp. GL File?       */~
            gltext$100, gltext1$100,     /* GL Text String             */~
            hnyacct$9,                   /* Inventory Source Account   */~
            hnycosts(12),                /* Inventory Costs            */~
            hnytype$2,                   /* Inventory transaction Type */~
            jbcosts(12),                 /* Job completion costs       */~
            jbpart$25,                   /* Job Part To Build          */~
            jnlid$3,                     /* Journal Id.                */~
            job$8,                       /* Job Number                 */~
            jobname$30,                  /* Job Name (Rework)          */~
            lclass$4,                    /* Labor Class                */~
            location$30,                 /* Serial Number Location     */~
            lot$16,                      /* Inventory Lot Id.          */~
            lottype$1,                   /* For Lottracking            */~
            message$10,                  /* Messaging Work Variable    */~
            modno$2,                     /* Module Id.                 */~
            newcosts$96,                 /* Derived Costs              */~
            next_rwjob$8,                /* Next Rework Job Number     */~
            open$6,                      /* Date Job Was Closed        */~
            part$25,                     /* Inventory Part Number      */~
            passedin_acct$(50)109,       /* Array passed to glcmbsub   */~
            passedin_dbcr(50,2),         /* Array passed to glcmbsub   */~
            passedin_type$(50)2,         /* Array passed to glcmbsub   */~
            plend$6,                     /* Planned End Date For Job   */~
            plowkey$96, plowkey1$100,    /* Miscell Read/Plow Key      */~
            plowwrk$96,                  /* Miscell Read/Plow Key      */~
            port$4,                      /* Message Port Id.           */~
            postdate$6,                  /* Postdate By Transaction    */~
            postdatef$8,                 /* Module Posting Date formatd*/~
            posttext$40,                 /* Module Posting Text        */~
            procfrom$26,                 /*                            */~
            r_a$1,                       /* 'R'equested,'A'cutal Usage */~
            rcvkey$100,                  /* RCV Lines, Hnyds file key  */~
            rcvstr$3, rcvlot$6,          /* Need for RCVLINES          */~
            readkey$96,                  /* Work Variable              */~
            record$(7)50,                /* Work Variable              */~
            revkey$22,                   /* JBCRPOST reversal cost     */~
            rteid$3,                     /* Built using Route ID       */~
            rwkpip$1,                    /* Rework PIP Flag            */~
            savetif$1,                   /* Retain TIF Records?        */~
            serial$20,                   /* Serial number              */~
            ser_lot$16,                  /* Lot S/N withdrawn from     */~
            ser_lot2$16,                 /* Lot S/N withdrawn from     */~
            ser_store$3,                 /* Store S/N withdrawn from   */~
            ser_store2$3,                /* Store S/N withdrawn from   */~
            start$6,                     /* Actual Job Starting Date   */~
            status$1,                    /* New Serial Number Status   */~
            tagnr$19,                    /* Tag to JBCROSS2 record     */~
            text$100,                    /* For HNYDETAL History Rcrds */~
            tlot$16,                     /* Temporary Lot Number       */~
            tojob$8,                     /* Job Number To Move To      */~
            tojbpart$25,                 /* Job Part To Build ('To'Job)*/~
            trdate$6,                    /* TRANSACTION DATE           */~
            tstore$6,                    /* Temporary Store Number     */~
            store$3,                     /* Warehouse (Store) Id.      */~
            wc$4,                        /* Work Center Code           */~
            wipacct$9,                   /* Work In Process Account    */~
            wipvaracct$9,                /* Completion Variance Acct   */~
            work$56,                     /* Work Variable              */~
            work1$40,                    /* Work Variable              */~
            usedate$8,                   /* Usage Capture Tran Date    */~
            userid$3,                    /* User Logon Id.             */~
            useseq$3,                    /* Usage Capture Line Seq #   */~
            useso$16,                    /* Usage Capture SO Number    */~
            usetype$5                    /* Usage Capture Type Code    */

        dim export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            partcat$4,                   /* Part Category code         */~
            partclass$4,                 /* Part Class code            */~
            partgen$16,                  /* Part Generic code          */~
            tran_type$5,                 /* G/L transaction type       */~
            tran_type$(4)5,              /* G/L transaction type       */~
            uom$4                        /* Part unit of measure       */~

        dim                                                              ~
            f1%(64),                     /* RECORD STATUS FLAGS        */~
            f2%(64)                      /* FILE STATUS FLAGS          */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #2  ! PIPMASTR ! Planned inv. position master             *~
            * #3  ! HNYMASTR ! Inventory master (descriptions)          *~
            * #4  ! JBMASTR2 ! Job master file                          *~
            * #5  ! JBVALUE2 ! Job value added ledger                   *~
            * #6  ! JBMATER2 ! Job material ledger                      *~
            * #7  ! JBCREDIT ! Job finished goods ledger                *~
            * #10 ! JBTIF    ! Shop Floor Transaction Image File        *~
            * #12 ! GLMAIN   ! General ledger master file               *~
            * #20 ! USERINFO ! Users posting dates                      *~
            * #25 ! RCVLINES ! Recvr Lines File                         *~
            * #26 ! RCVHNYDS ! Recvr Lines Dist File                    *~
            * #22 ! JBJNLPF  ! Job gl print journal                     *~
            * #33 ! PIPIN    ! Planned position in                      *~
            * #34 ! PIPOUT   ! Planned position out                     *~
            * #40 ! COREXREF ! Core Cross Reference File                *~
            * #41 ! JBMASTRC ! JBMASTR2 Core Appendix                   *~
            * #50 ! WRKFILE  ! Work file for HNYPST2                    *~
            * #52 ! HNYQUAN  ! Inventory quantity  file                 *~
            * #54 ! SYSFILE2 ! Inventory default values                 *~
            * #55 ! HNYPOOL  ! Lifo/fifo pools- part/str/lot/rev seq    *~
            * #56 ! HNYDETAL ! Inventory detail file                    *~
            * #57 ! GLDETAIL ! Gl detail file                           *~
            * #58 ! SFCUM    ! Sales forecasts                          *~
            * #59 ! JBCROSS2 ! Job Actual to Planned Cross Reference fil*~
            * #61 ! SERTIF   ! Serial Numbers Transactions Buffer       *~
            * #62 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #63 ! SERDETAL ! Serial Number Tracking Relationships file*~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #2,   "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #3, "HNYMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 900,                                    ~
                       keypos = 1, keylen = 25

            select #4, "JBMASTR2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1300,                                   ~
                       keypos = 1, keylen = 8,                           ~
                       alt key 1, keypos = 1120, keylen = 19, dup,       ~
                           key 2, keypos =   58, keylen = 25, dup

            select  #5, "JBVALUE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 23

            select  #6, "JBMATER2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 1, keylen = 22,                         ~
                        alt key 1, keypos=23, keylen=48

            select #7,   "JBCREDIT",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 22,                        ~
                         alternate key 1, keypos = 23, keylen = 48

            select #10, "JBTIF",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos =    9, keylen =  18,                     ~
                        alt key  1, keypos =  1, keylen =  26

            select  #12, "GLMAIN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select #20, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #22, "JBJNLPF",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19                         ~

            select #25, "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24              ~

            select #26, "RCVHNYDS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 86,                          ~
                        alt key 1, keypos = 45, keylen = 42              ~

            select #33, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  60,                                   ~
                        keypos = 30, keylen = 19,                        ~
                        alternate key 1, keypos = 1, keylen = 48

            select #34, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  64,                                   ~
                        keypos = 1, keylen = 56,                         ~
                        alternate key 1, keypos = 20, keylen = 37

            select #52, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos= 17, keylen = 44,                         ~
                        alternate key 1, keypos =  1, keylen = 44

            select #40, "COREXREF",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 26, keylen = 50,                        ~
                         alternate key 1, keypos =  1, keylen = 50,      ~
                                   key 2, keypos = 76, keylen = 25, dup

            select #41, "JBMASTRC",                                      ~
                        varc, indexed, recsize = 600,                    ~
                        keypos =  1, keylen =  8

            select #54, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #55, "HNYPOOL",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #56, "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 42,                         ~
                         alternate key 1, keypos = 43, keylen = 6, dup,  ~
                                   key 2, keypos = 49, keylen = 2, dup

            select #57, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select #58, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos = 1, keylen = 25

            select #59, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =   94,              ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47

            select #61, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #62, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #63, "SERDETAL",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   46, keylen =  48,                     ~
                        alt key  1, keypos =    1, keylen =  93

            select #50, "WRKFILE",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19                         ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#02, 0%, f2%(02), 0%, " ")
            call "OPENCHCK" (#03, 0%, f2%(03), 0%, " ")
            call "OPENCHCK" (#04, 0%, f2%(04), 200%, " ")
            call "OPENCHCK" (#05, 0%, f2%(05), 0%, " ")
            call "OPENCHCK" (#06, 0%, f2%(06), 0%, " ")
            call "OPENCHCK" (#07, 0%, f2%(07), 0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10), 500%, " ")
            call "OPENCHCK" (#12, 0%, f2%(12), 0%, " ")
            call "OPENCHCK" (#20, 0%, f2%(20), 0%, " ")
            call "OPENCHCK" (#22, 0%, f2%(22), 100%, " ")
            call "OPENCHCK" (#25, 0%, f2%(25), 0%, " ")
            call "OPENCHCK" (#26, 0%, f2%(26), 0%, " ")
            call "OPENCHCK" (#33, 0%, f2%(33), 100%, " ")
            call "OPENCHCK" (#34, 0%, f2%(33), 0%, " ")
            call "OPENCHCK" (#40, 0%, f2%(40), 0%, " ")
            call "OPENCHCK" (#41, 0%, f2%(41), 0%, " ")
            call "OPENCHCK" (#52, 0%, f2%(52), 0%, " ")
            call "OPENCHCK" (#54, 0%, f2%(54), 0%, " ")
            call "OPENCHCK" (#55, 0%, f2%(55), 0%, " ")
            call "OPENCHCK" (#56, 0%, f2%(56), 0%, " ")
            call "OPENCHCK" (#57, 0%, f2%(57), 0%, " ")
            call "OPENCHCK" (#58, 0%, f2%(58), 0%, " ")
            call "OPENCHCK" (#59, 0%, f2%(59), 0%, " ")
            call "OPENCHCK" (#61, 0%, f2%(61), 0%, " ")
            call "OPENCHCK" (#62, 0%, f2%(62), 0%, " ")
            call "OPENCHCK" (#63, 0%, f2%(63), 0%, " ")
            call "WORKOPEN" (#50, "IO", 100%, f2%(50))

        REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "MSGCREAT" (#54, port$, "PORT.ID.JBPOST2", return%)
              if return% <> 0% then end

            REM Get Idle Timeout if specified...
                timeout% = 240%
                readkey$ = "SWITCHS.SFC"
                call "READ100" (#54, readkey$, f1%(54))
                     if f1%(54) = 0% then L09114
                get #54, using L09110, timeout%, savetif$, gllog$,         ~
                         comperrcst$, wipvaracct$, rwkpip$
L09110:              FMT XX(23), BI(2), POS(29), 2*CH(1), POS(56), CH(1),~
                         POS(61), CH(9), POS(70), CH(1)

L09114
*        See if G/L Export is on
            export_on$ = "N"
            readkey$ = "SWITCHS.GL"
            call "READ100" (#54, readkey$, f1%(54))
            if f1%(54) = 1% then get #54 using L09122, export_on$
L09122:         FMT POS(22), CH(1)

            REM Clear any old control transactions...
            call "DELETE" (#10, " J2" & hex(0100), 4%)
            call "DELETE" (#10, " J2" & hex(ff00), 4%)
            call "SHOSTAT"                                               ~
                 ("Shop Floor Control Posting Task #2 In Progress")
            flotmisc$, tlotmisc$ = " "   /* Lot Track Misc Fields JIC  */
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#54, plowkey$, corebank%)     /* SYSFILE2 */
            if corebank% = 0% then goto L10000

*        OK, grab the necessary system defaults from SWITCHS.COR.
            get #54 using L09360, dfcore_var$, dfcore_fga$, dfcore_wip$,   ~
                    coreactive$
L09360:         FMT POS(30), CH(9), POS(84), CH(9), POS(119), CH(9),     ~
                    POS(135), CH(1)
            if coreactive$ <> "Y" then corebank% = 0%
            if corebank% = 0% then goto L10000

            call "OPENCHCK" (#40, f1%(40%), f2%(40), 0%, " ")

L10000: REM *************************************************************~
            *                    M A I N   P R O G R A M                *~
            *-----------------------------------------------------------*~
            * Processing begins here.                                   *~
            *************************************************************

        next_trans
            call "PLOWNEXT" (#10, " J2" & hex(0000),3%,f1%(10))
                if f1%(10) = 0% then L10290
            inactive% = 0%
            get #10,using L10400,job$, function%, userid$, modno$, jnlid$,~
                                pstseq%, postdate$, part$, store$, lot$, ~
                                quan, work$, work1$, costs$
            newcosts$ = costs$
            acct$() = " " : mat glamount = zer
            passedin_acct$() = " " : mat passedin_dbcr = zer
            passedin_type$() = " "
            wc$, activity$, emp$, earntype$, lclass$ = " "

            call "SHOSTAT" ("Processing Job: " & job$ & " Part: " & part$~
                      & " Str: " & store$ & " Lot: " & lot$)

            on function% gosub L11000,    /* Report Completion (good)   */~
                               L12000,    /* Materials Moved Job To Job */~
                               L17000,    /* Value Moved Job To Job     */~
                               L13000,    /* Materials Moved Job To Inv */~
                               L18000,    /* Value Added                */~
                               L14000,    /* Report Completion (scraped)*/~
                               L15000,    /* Report Completion (rework) */~
                               L16000,    /* Adj. Job/Inventory         */~
                               L19000,    /* Report Completion -> Job   */~
                               L20000,    /* Direct Job Quantity Adj.   */~
                               L50000,    /* Close G/L Batch            */~
                               L10270     /* To Job Holder              */

            if function% = 99% then exit_program /* End task gracefully*/
L10270:     gosub good_trans             /* clear 'RETURN' stack */

L10290:     if message$ = "CANCEL" then exit_program
            if inactive% >= timeout% then exit_program

            REM Go into stand by mode           60 seconds
            call "MSGBFGND"                                              ~
                       ("Shop Floor Posting Task #2 Standing By",        ~
                        port$, message$, 10%, inactive%, u3%)
                if u3% = 16% then exit_program   /* Something's Whacko */
            goto next_trans

L10400:     FMT CH(8),                   /* Job Number                 */~
                XX(1),                   /* Record Status              */~
                XX(2),                   /* Port Id  (trans type)      */~
                XX(1),                   /* Priority                   */~
                XX(6),                   /* Date                       */~
                XX(8),                   /* Time (hhmmss100th)         */~
                BI(1),                   /* Function                   */~
                CH(3),                   /* User Id                    */~
                CH(2),                   /* Module                     */~
                CH(3),                   /* Journal Id                 */~
                BI(4),                   /* Posting Sequence           */~
                CH(6),                   /* Posting Date               */~
                CH(25),                  /* Component Part             */~
                CH(3),                   /* Store                      */~
                CH(6),                   /* Lot                        */~
                PD(14,4),                /* Quantity                   */~
                CH(56),                  /* Materials key, to job, csts*/~
                CH(40),                  /* Free text for val post,or? */~
                CH(96)                   /* Inventory Costs            */

L11000: REM *************************************************************~
            *            R E P O R T   C O M P L E T I O N              *~
            *               F I N I S H E D   G O O D S                 *~
            * Move job part out of job, update pipin file.              *~
            *************************************************************

            if abs(quan) < .0001 then return
            gosub get_jbpart

            postdatef$ = postdate$  :  call "DATEFMT" (postdatef$)
            procfrom$  = "*" & str(job$,,8)
            gltext$    = job$
            str(gltext$,31%) = str(part$) & str(store$) & lot$
            str(gltext$,69%), text$ = "COMPLETED FROM JOB"
            tran_type$(1) = "MPC01" : tran_type$(2) = "MPC02"
            if quan > 0 then L11038
                str(gltext$,69%), text$ =  "JOB PART MOVED BACK TO JOB"
                tran_type$(1) = "MRJ01" : tran_type$(2) = "MRJ02"
L11038:     text$ = text$ & ": " & job$ & " ON " & postdatef$
            if export_on$ = "Y" then gosub load_gl_info
            costflag$ = str(work1$,,2)
            rcvkey$ = " "
            rcvkey$ = str(work$,31,25)
            str(rcvkey$,26) = str(work1$,3,19)
            revkey$ = str(work$,9,22)
            if quan < 0 then L11052
               if rcvkey$ = " " then revkey$ = " "

L11052:     mat cbom = zer : mat cwip = zer : nonzero% = 0%
            if quan < 0 then L11106
            if corebank% = 0% then L11106
            if str(costflag$,,1%) = "Z" then L11106
            plowkey$ = " " : str(plowkey$,,25%) = part$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, core%)
               if core% <> 0% then L11106
            if str(costflag$,,1%) = "S" then L11106
*        Get Core Actuals
            call "READ100" (#41, job$, f1%(41%)) /* JBMASTRC */
               if f1%(41%) = 0% then L11106      /* Not Core, no legder */
                                                /* Normal Completeion  */
            get #41 using L11074, cbom(), cshd(), ccdt(), cwip()
L11074:         FMT  POS(17), 12*PD(14,4), POS(113), 12*PD(14,4),        ~
                     POS(217), 12*PD(14,4), POS(425), 12*PD(14,4)

            mat cbom    = cbom + cshd       /* Uncredited costs- BOM  */
            mat cbom    = cbom - ccdt

            for b% = 1% to 12%
                cbom(b%) = round(cbom(b%), 4)
                if cbom(b%) <> 0 then nonzero% = 1%
            next b%

L11106:     call "JBCRPOST"                                              ~
                     (job$,              /* JOB  TO BE UPDATED         */~
                      costflag$,         /* "B" -or- "P"               */~
                      comperrcst$,       /* Cost Error Flag            */~
                      part$,             /* PART NUMBER TO POST        */~
                      store$,            /* STORE NUMBER               */~
                      lot$,              /* LOT NUMBER                 */~
                      quan,              /* QUANTITY MOVED TO JOB      */~
                      revkey$,           /* Reversal Key               */~
                      jbcosts(),         /* Inventory Costs per unit   */~
                      cbom(),            /* Core Adjustments           */~
                      cwip(),            /* Core moved to WIP          */~
                      wiptotal,          /* Total Posted to Job        */~
                      " ",               /* TRANSFER TO JOB NUMBER     */~
                      postdate$,         /* POSTING DATE$              */~
                      userid$,           /* WHO                        */~
                      #4,                /* UFB ADDRESS OF JBMASTR2    */~
                      #7,                /* UFB ADDRESS OF JBCREDIT    */~
                      #54,               /* UFB ADDRESS OF SYSFILE2    */~
                      #59,               /* UFB ADDRESS OF JBCROSS2    */~
                      #6,                /* UFB ADDRESS OF JBMATER2    */~
                      #3,                /* UFB ADDRESS OF HNYMASTR    */~
                      return%)           /* ERROR RETURN FROM SUB      */

            if return% = 0% then L11158
        /*     RCVFAIL = QUAN : GOSUB RCV_DIST_TAG */
               put errortext$, using L11148, return%
L11148:            %JBCRPOST Return: ###
               if return% > 96% then bad_trans            /* REAL BAD */
               if p_enabled% <> 0% then gosub reverse_serial_numbers
               goto bad_trans

L11158:     if quan < 0 then L11408
            if corebank% = 0% then L11408
            acct$() = " " : mat glamount = zer
            cost = 0 : mat costs = zer
            plowkey$ = " " : str(plowkey$,,25%) = part$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, core%)
               if core% <> 0% then L11238

*       ** POST COREWIP --> WIP IF NECESSARY
            if nonzero% = 0% then L11408
            if jbcorewip$ = " " then gosub set_jb_core_wip
            acct$(2) = jbcorewip$
            acct$(1) = wipacct$
            posttext$ = "CVF: VALUE TO F/G WIP"
            call "CONVERT" (quan, 2.2, str(posttext$,31%,10%))
            gltext1$ = gltext$
            str(gltext$,65%) = "    CVF: VALUE TO F/G WIP"

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       8%,               /* TYPE - CORE COMP MEMO      */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       cbom())          /* COSTS PASSED IN            */

            for b% = 1% to 12%
                glamount(1) = glamount(1) + cbom(b%)
            next b%
            glamount(2) = glamount(1)
            goto L11400

*       ** POST COREWIP --> WIP IF NECESSARY

L11238:     corepart$ = str(plowkey$,26%,25%)
               call "STCCOSTS" (corepart$, " ", #54, 2%, cost, costs())
                  if cost = 0 then L11250
                     mat costs = (quan) * costs
                     glamount(1) = round(quan * cost, 2%)
                     glamount(3) = glamount(1)
L11250:            call "PACKZERO" (costs(), costs$)
               get #40 using L11254, acct$(1)
L11254:            FMT POS(137), CH(9)
               if acct$(1) <> " " then L11270
                  plowkey$ = " " : str(plowkey$,26%) = corepart$
                  call "REDALT0" (#40, plowkey$, 0%, f1%(40%))
                     if f1%(40%) = 0% then L11268
                  get #40 using L11254, acct$(1)
                     if acct$(1) <> " " then L11270
L11268:                 acct$(1) = dfcore_fga$
L11270:           acct$(3) = dfcore_var$

*        Get Job Actuals
            mat cbom = zer
            if  str(costflag$,1%,1%)  = "Z" then L11316
            if  str(costflag$,1%,1%) <> "S" then L11284
                mat cbom = costs : goto L11316
L11284:     call "READ100" (#41, job$, f1%(41%)) /* JBMASTRC */
               if f1%(41%) = 0% then L11316
            get #41 using L11294, cbom(), cshd(), ccdt(), cwip()
L11294:         FMT  POS(17), 12*PD(14,4), POS(113), 12*PD(14,4),        ~
                     POS(217), 12*PD(14,4), POS(425), 12*PD(14,4)

            mat cbom    = cbom + cshd       /* Uncredited costs- BOM  */
            mat cbom    = cbom - ccdt
            mat cbom    = cbom - cwip

            if str(costflag$,1%,1%) <> "P" then L11316
            qltb = jbqty - jbdone
            mat cbom    = (quan)     * cbom
            mat cbom    = (1 / qltb) * cbom

L11316:     for b% = 1% to 12%
                cbom(b%) = round(cbom(b%), 4)
                glamount(2) = glamount(2) + cbom(b%)
            next b%
            glamount(3) = glamount(3) - glamount(2)

*        Post to Job & G/L
            if jbcorewip$ = " " then gosub set_jb_core_wip
            acct$(2) = jbcorewip$
            posttext$ = "CVC: " & corepart$
            call "CONVERT" (quan, 2.2, str(posttext$,31%,10%))
            gltext1$ = gltext$
            str(gltext$,65%) = "    CVC: " & corepart$

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       6%,               /* TYPE - CORE CREDIT         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       cbom())          /* COSTS PASSED IN            */

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       9%,               /* TYPE - EFFECT ON CFG       */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       costs())          /* COSTS PASSED IN            */

L11400:     gosub post_gl
            acct$() = " " : mat glamount = zer
            gltext$ = gltext1$

L11408:     call "PACKZERO" (jbcosts(), newcosts$)
            mat hnycosts = jbcosts

            hnytype$ = "JC"

            gosub hnypst2_call

            jbcost = 0
            for b% = 1% to 12% : jbcost = jbcost + jbcosts(b%) : next b%
            glamount(1) = round(quan * jbcost, 2)
            glamount(2) = round(wiptotal, 2)
            acct$(1) = hnyacct$   /* Debit  */
            acct$(2) = wipacct$   /* Credit */
            passedin_acct$(1) = str(hnyacct$,1, 9) & gltext$
            passedin_acct$(2) = str(wipacct$,1, 9) & gltext$
            passedin_dbcr(1,1) = glamount(1)
            passedin_dbcr(1,2) = 0
            passedin_dbcr(2,2) = glamount(2)
            passedin_type$(1) = "01" : passedin_type$(2) = "02"
            cntr% = 2%

            if abs(glamount(1) - glamount(2)) < .01 then L11452
               cntr% = cntr% + 1%
               passedin_acct$(3) = str(wipvaracct$,1, 9) & gltext$
               passedin_dbcr(3,1), passedin_dbcr(3,2) = 0
               if glamount(1) - glamount(2) < 0 then                     ~
                  passedin_dbcr(3,1) = glamount(2) - glamount(1)         ~
                                                else                     ~
                  passedin_dbcr(3,2) = glamount(1) - glamount(2)

L11452:     mat apst = hnycosts - jbcosts
            qtyadj   = -quan
            str(gltext$,65, 4) = "ADJ"
            str(gltext$,   69) = "COMPLETION REV. VARIANCE"
            gosub hnyapst_call

            gosub consolidate_array_and_post

            lottype$ = "H"
            gosub lottrack_call

            gosub update_pipin_for_job

            call "HNYPROCU" (part$, procfrom$, postdate$, quan, hnycost, ~
                             plend$, #3, #54)

            rcvfail = 0 : gosub rcv_dist_tag

            if quan < 0 then L11503
               status$ = "2"               /* Inventory/Finished Goods */
               location$ = str(store$) & lot$
               plowkey$ = "JC" & str(job$) & str(work1$,38%,3%)
                  goto L11506
L11503:        status$ = "1"               /* Back into Job (WIP)     */
               location$ = job$
               plowkey$ = "JR" & job$
L11506:     gosub serial_number_handling

            if quan >= 0 then return

            if corebank% = 0% then return

            plowkey$ = " " : str(plowkey$,,25%) = part$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, f1%(40%))
               if f1%(40%) = 0% then return
            corepart$ = str(plowkey$,26%,25%)
               call "STCCOSTS" (corepart$, " ", #54, 2%, cost, costs())
                  if cost = 0 then return
                     acct$() = " " : mat glamount = zer
                     mat costs = (-quan) * costs
                     call "PACKZERO" (costs(), costs$)
                     mat rcst = (-1) * costs
                     glamount(1) = round(-quan * cost, 2%)
                     glamount(2) = glamount(1)
               get #40 using L11542, acct$(2)
L11542:            FMT POS(137), CH(9)
               if jbcorewip$ = " " then gosub set_jb_core_wip
               acct$(1) = jbcorewip$
               if acct$(2) <> " " then L11564
                  plowkey$ = " " : str(plowkey$,26%) = corepart$
                  call "REDALT0" (#40, plowkey$, 0%, f1%(40%))
                     if f1%(40%) = 0% then L11560
                  get #40 using L11542, acct$(2)
                  if acct$(2) <> " " then L11564
L11560:              acct$(2) = dfcore_fga$

L11564
*        Post to Job & G/L
            posttext$ = "CVD: " & corepart$
            call "CONVERT" (quan, 2.2, str(posttext$,31%,10%))
            str(gltext$,65%) = "    CVD: " & corepart$

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       6%,               /* TYPE - CORE CREDIT         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       rcst())           /* COSTS PASSED IN            */

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       9%,               /* TYPE - EFFECT ON CFG       */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       rcst())           /* COSTS PASSED IN            */

            gosub post_gl
            return

L12000: REM *************************************************************~
            * MOVE MATERIALS JOB TO JOB                                 *~
            *************************************************************~

            if abs(quan) < .0001 then return

            tojob$ = str(work$,,8)
            temp$  = job$
            job$   = tojob$
            gosub get_jbpart
            acct$(1)  = wipacct$  /* Debit */
            tojbpart$ = jbpart$
            tojbcorewip$ = jbcorewip$

            job$ = temp$
            gosub get_jbpart
            acct$(2) = wipacct$  /* Credit */
            fromjbcorewip$ = jbcorewip$

            store$ = "*" & str(tojob$,,2)
            lot$ = str(tojob$,3,6)

            gltext$ = job$ & " TO " & tojob$
            str(gltext$,31) = str(part$) & str(store$) & str(lot$)
                str(gltext$,69) = "MATERIAL MOVED JOB TO JOB"

            tran_type$(1), tran_type$(2) = "MJR01"
            iquan = quan
            if export_on$ = "Y" then gosub load_gl_info

            gosub material_from_job
               if coreflg$ = " "  then L12180
                  if jbcorewip$ = " " then gosub set_jb_core_wip
                  acct$(2) = jbcorewip$   /* Credit */
                  fromjbcorewip$ = jbcorewip$

L12180:     glamount(1) = round(quan * cost, 2%)
            glamount(2) = glamount(1)

            gosub byproduct_pipouts

            store$ = "*" & str(job$,,2)
            lot$ = str(job$,3,6)
            fromjob$ = job$
            job$ = tojob$

            gosub jbmtpost_call
               if coreflg$ = " "  then L12262
                  jbcorewip$ = tojbcorewip$
                  if jbcorewip$ = " " then gosub set_jb_core_wip
                  acct$(1) = jbcorewip$   /* Debit  */
                  tojbcorewip$ = jbcorewip$

L12262:     gosub decrement_pipouts

            job$ = fromjob$

            call "SERENABL" (part$, c_enabled%, u3%, #54, #3)
                if c_enabled% = 1% then L12315 /* Can't Move S/N */
                                              /* Components     */
        REM *** DeKit Lot Tracked Components if Applicable ***
            u3% = val(str(work$,55%,2%),2)
            plowkey$ = "JJ" & str(job$) & bin(u3%,3)
            gosub detach_lottrack_components

L12315:     lottype$ = "J"
            lot$ = "*" & str(job$,,5)
            store$ = str(job$,6,3)
            gosub lottrack_call

            lot$      = "*" & str(tojob$,,5)
            store$    = str(tojob$,6,3)
            jbpart$   = part$
            part$     = tojbpart$
            flotmisc$ = "C"

            gosub lottrack_call

            gosub post_gl

            if corebank% = 0% then return

            plowkey$ = " " : str(plowkey$,,25%) = jbpart$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, f1%(40%))
               if f1%(40%) = 0% then return
            corepart$ = str(plowkey$,26%,25%)
            call "STCCOSTS" (corepart$, " ", #54, 2%, cost, costs())
               if cost = 0 then return
                  mat costs = (quan) * costs
                  call "PACKZERO" (costs(), costs$)
                  mat rcst = (-1) * costs
                  glamount(1) = round(quan * cost, 2%)
                  glamount(2) = glamount(1)

*        Post to Jobs & G/L
            posttext$ = "CVA: " & corepart$
            call "CONVERT" (-quan, 2.2, str(posttext$,31%,10%))
            str(gltext$,65%) = "    CVA: " & corepart$

            jbcorewip$ = fromjbcorewip$
            if jbcorewip$ = " " then gosub set_jb_core_wip
            acct$(2) = jbcorewip$   /* Credit */

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       5%,               /* TYPE - CORE SHADOW         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       rcst())           /* COSTS PASSED IN            */

            job$ = tojob$
            jbcorewip$ = tojbcorewip$
            if jbcorewip$ = " " then gosub set_jb_core_wip
            acct$(1) = jbcorewip$   /* Debit  */
            call "CONVERT" (quan, 2.2, str(posttext$,31%,10%))

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       5%,               /* TYPE - CORE SHADOW         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       costs())          /* COSTS PASSED IN            */

            gosub post_gl
            return

L13000: REM *************************************************************~
            * MOVE FROM JOB TO INVENTORY (COMPONENT)                    *~
            *************************************************************

            if abs(quan) < .0001 then return

            gosub get_jbpart

            gltext$ = job$
            str(gltext$,31) = str(part$) & str(store$) & str(lot$)
            str(gltext$,69) = "MATERIAL RELEASED FROM JOB"

            tran_type$(1) = "MPW01"
            tran_type$(2) = "MPW02"
            if export_on$ = "Y" then gosub load_gl_info

            gosub material_from_job
               if coreflg$ = " " then L13150
                  if jbcorewip$ = " " then gosub set_jb_core_wip
                  wipacct$ = jbcorewip$

L13150:     glamount(1) = round(quan * cost, 2%)
            glamount(2) = glamount(1)
            hnytype$ = "JK"
            gosub byproduct_pipouts

            text$ = "MATERIAL RELEASED FROM JOB:" & job$

            call "SERENABL" (part$, c_enabled%, u3%, #54, #3)
               if c_enabled% = 1% then L13320    /* DeKit Serialized */
                                                /* Components */
        REM *** DeKit Lot Tracked Components if Applicable ***

            u3% = val(str(work$,55%,2%),2)
            plowkey$ = "RJ" & str(job$) & bin(u3%,3)
            gosub detach_lottrack_components
            goto L13360

L13320:     status$ = "2"     /* inventory   */
            location$ = str(store$) & lot$
            plowkey$ = "RJ" & job$

L13360:     gosub serial_number_handling

            mat hnycosts = costs
            gosub hnypst2_call
            acct$(1) = hnyacct$
            acct$(2) = wipacct$
            passedin_acct$(1) = str(hnyacct$,1,9) & gltext$
            passedin_acct$(2) = str(wipacct$,1,9) & gltext$
            passedin_type$(1) = "01"
            passedin_type$(2) = "02"
            passedin_dbcr(1,1) = glamount(1)
            passedin_dbcr(1,2) = 0
            passedin_dbcr(2,1) = 0
            passedin_dbcr(2,2) = glamount(2)
            cntr% = 2%

            gosub consolidate_array_and_post

            lottype$ = "H"
            gosub lottrack_call

            if corebank% = 0% then return

            plowkey$ = " " : str(plowkey$,,25%) = part$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, f1%(40%))
               if f1%(40%) = 0% then return
            corepart$ = str(plowkey$,26%,25%)
               call "STCCOSTS" (corepart$, " ", #54, 2%, cost, costs())
                  if cost = 0 then return
                     mat costs = (quan) * costs
                     call "PACKZERO" (costs(), costs$)
                     mat rcst = (-1) * costs
                     glamount(1) = round(quan * cost, 2%)
                     glamount(2) = glamount(1)
               get #40 using L13630, acct$(1)
L13630:            FMT POS(137), CH(9)
               if jbcorewip$ = " " then gosub set_jb_core_wip
               acct$(2) = jbcorewip$
               if acct$(1) <> " " then L13675
                  plowkey$ = " " : str(plowkey$,26%) = corepart$
                  call "REDALT0" (#40, plowkey$, 0%, f1%(40%))
                     if f1%(40%) = 0% then L13665
                  get #40 using L13630, acct$(1)
                  if acct$(1) <> " " then L13675
L13665:              acct$(1) = dfcore_fga$

L13675
*        Post to Job & G/L
            posttext$ = "CVA: " & corepart$
            call "CONVERT" (-quan, 2.2, str(posttext$,31%,10%))
            str(gltext$,65%) = "    CVA: " & corepart$

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       5%,               /* TYPE - CORE SHADOW         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       rcst())           /* COSTS PASSED IN            */

            gosub post_gl
            return

L14000: REM *************************************************************~
            * SCRAP HANDLING                                            *~
            *************************************************************

            if abs(quan) < .0001 then return
            acct$(1%) = " "
            gosub get_jbpart

            gltext$ = job$
            str(gltext$,31) = str(part$) & str(store$) & str(lot$)
            str(gltext$,69%) = "MATERIAL SCRAPPED FROM JOB"

            tran_type$(1) = "MPC05"
            tran_type$(2) = "MPC02"
            if export_on$ = "Y" then gosub load_gl_info

            gosub material_from_job
               if coreflg$ = " " then L14150
                  if jbcorewip$ = " " then gosub set_jb_core_wip
                  wipacct$ = jbcorewip$

L14150:     glamount(1) = round(quan * cost, 2%)
            glamount(2) = glamount(1)

*        Get asset account for scrap, then move to inventory...
            status$ = "2"     /* scrap inventory */
            location$ = str(store$) & lot$
            plowkey$  = "JC" & str(job$) & hex(000002)
            gosub serial_number_handling

            call "READ100" (#54, "SWITCHS.SFC", f1%(54%))
                if f1%(54%) = 0% then L14210
            get #54 using L14206, acct$(1%)
L14206:         FMT POS(106), CH(9)

L14210:     if acct$(1%) <> " " then L14220
                call "HNYGLGET" (part$, store$, lot$, acct$(1%), 3%,     ~
                                 #3, #52)

L14220:     if acct$(1%) <> " " then L14230
                call "READ100" (#54, "FISCAL DATES", f1%(54%))
                    if f1%(54%) = 0% then L14230
                get #54 using L14228, acct$(1%)
L14228:             FMT POS(417), CH(9)

L14230:     call "READ100" (#52, str(part$) & str(store$) & str(lot$),   ~
                                                                 f1%(52))
                if f1%(52) <> 0% then L14250

*        Dummy HNYPST1 quantities just to create HNYQUAN before HNYPST2
            call "HNYPST1" (part$, store$, lot$, 0, 0, 1, 0, 0, #52, #3, ~
                            #54, f2%(52%), f2%(3%), f2%(54%), 0%, u3%)
            call "HNYPST1" (part$, store$, lot$, 0, 0, -1, 0, 0, #52, #3,~
                            #54, f2%(52%), f2%(3%), f2%(54%), 0%, u3%)
            call "READ101" (#52, str(part$) & str(store$) & str(lot$),   ~
                                                                 f1%(52))
                if f1%(52) <> 1% then L14250  /* Shouldn't happen */
            put #52, using L14248, acct$(1)   /* Scrap asset acct */
L14248:         FMT POS(259), CH(9)
            rewrite #52
L14250:     text$ = "MATERIAL SCRAPPED FROM JOB: " & job$

            hnytype$ = "JT"
            mat hnycosts = costs
            gosub hnypst2_call
            acct$(1%) = hnyacct$

            lottype$ = "H"
            gosub lottrack_call

            scradj = quan : rwkadj = 0
            gosub jbmastr2_update

            acct$(2) = wipacct$
            passedin_acct$(1) = str(acct$(1),1,9) & gltext$
            passedin_acct$(2) = str(wipacct$,1,9) & gltext$
            passedin_dbcr(1,1) = glamount(1)
            passedin_dbcr(1,2) = 0
            passedin_dbcr(2,1) = 0
            passedin_dbcr(2,2) = glamount(2)
            passedin_type$(1) = "05"
            passedin_type$(2) = "02"
            cntr% = 2%

            gosub consolidate_array_and_post

            if corebank% = 0% then return

            plowkey$ = " " : str(plowkey$,,25%) = part$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, f1%(40%))
               if f1%(40%) = 0% then return
            corepart$ = str(plowkey$,26%,25%)
               call "STCCOSTS" (corepart$, " ", #54, 2%, cost, costs())
                  if cost = 0 then return
                     mat costs = (quan) * costs
                     call "PACKZERO" (costs(), costs$)
                     mat rcst = (-1) * costs
                     glamount(1) = round(quan * cost, 2%)
                     glamount(2) = glamount(1)
               get #40 using L14690, acct$(1)
L14690:            FMT POS(137), CH(9)
               if jbcorewip$ = " " then gosub set_jb_core_wip
               acct$(2) = jbcorewip$
               if acct$(1) <> " " then L14780
                  plowkey$ = " " : str(plowkey$,26%) = corepart$
                  call "REDALT0" (#40, plowkey$, 0%, f1%(40%))
                     if f1%(40%) = 0% then L14760
                  get #40 using L14690, acct$(1)
                  if acct$(1) <> " " then L14780
L14760:              acct$(1) = dfcore_fga$

L14780
*        Post to Job & G/L
            posttext$ = "CVS: " & corepart$
            call "CONVERT" (-quan, 2.2, str(posttext$,31%,10%))
            str(gltext$,65%) = "    CVS: " & corepart$

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       5%,               /* TYPE - CORE SHADOW         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       rcst())           /* COSTS PASSED IN            */

            gosub post_gl
            return

L15000: REM *************************************************************~
            * REWORK JOB                                                *~
            *************************************************************

            if abs(quan) < .0001 then return

            gosub get_jbpart
            acct$(2) = wipacct$ : tojob$ = " "
            fromjbcorewip$ = jbcorewip$ : tojbcorewip$ = " "
            gosub set_up_rework_job

            gltext$ = job$ & " TO: " & tojob$
            str(gltext$,31) = str(part$) & "*" & job$
            str(gltext$,69) = "MATERIAL MOVED TO REWORK"

            store$ = "*" & str(tojob$,,2)
            lot$   = str(tojob$,3)

            tran_type$(1) = "MPC06"
            tran_type$(2) = "MPC02"
            if export_on$ = "Y" then gosub load_gl_info

            gosub material_from_job
               if coreflg$ = " " then L15180
                  if jbcorewip$ = " " then gosub set_jb_core_wip
                  wipacct$ = jbcorewip$
                  fromjbcorewip$ = jbcorewip$
                  acct$(2) = wipacct$

L15180:     glamount(1) = round(quan * cost, 2%)
            glamount(2) = glamount(1)

            lot$ = str(tojob$,3%)
            status$ = "1"   /* WIP */
            location$ = tojob$
            plowkey$ = "JC" & str(job$) & hex(000001)
            gosub serial_number_handling

            rwkadj = quan : scradj = 0
            iquan  = quan
            gosub jbmastr2_update

            lottype$ = "J"
            lot$     = "*" & str(tojob$,,5)
            store$   = str(tojob$,6,3)
            gosub lottrack_call

            store$   = "*" & str(job$,,2)
            lot$     = str(job$,3)
            fromjob$ = job$
            job$     = tojob$
            gosub jbmtpost_call
               if coreflg$ = " " then L15360
                  jbcorewip$ = " " : gosub set_jb_core_wip
                  tojbcorewip$ = jbcorewip$
                  acct$(1) = jbcorewip$

L15360:     gosub post_gl

            jobend% = rwkend% : jobstart% = rwkstart%
            jbqty = 0 : jbdone = 0 : rwkadj = 0 : scradj = 0
            quan = -quan : gosub jbmastr2_update : quan = -quan

            init (hex(00)) work$
            str(work$,,11) = str(fromjob$,,8) & " J3"
            work1$ = key(#10)
            call "PLOWAL1" (#10, work$, 1%, 11%, f1%(10))
               if f1%(10) = 0% then L15451
            get #10, using L15450, record$()
            str(record$(),1,8)  = tojob$
            str(record$(),11,1) = "2"
            str(record$(),132,8) = fromjob$
            delete #10
            write #10, using L15450, record$()
L15450:           FMT 7*CH(50)
L15451:     call "READ100" (#10, work1$, f1%(10))

            if corebank% = 0% then return

            plowkey$ = " " : str(plowkey$,,25%) = part$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, f1%(40%))
               if f1%(40%) = 0% then return
            corepart$ = str(plowkey$,26%,25%)
            call "STCCOSTS" (corepart$, " ", #54, 2%, cost, costs())
               if cost = 0 then return
                  mat costs = (quan) * costs
                  call "PACKZERO" (costs(), costs$)
                  mat rcst = (-1) * costs
                  glamount(1) = round(quan * cost, 2%)
                  glamount(2) = glamount(1)

*        Post to Jobs & G/L
            posttext$ = "CVR: " & corepart$
            call "CONVERT" (-quan, 2.2, str(posttext$,31%,10%))
            str(gltext$,65%) = "    CVR: " & corepart$

            job$ = fromjob$ : jbcorewip$ = fromjbcorewip$
            if jbcorewip$ = " " then gosub set_jb_core_wip
            acct$(2) = jbcorewip$   /* Credit */

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       5%,               /* TYPE - CORE SHADOW         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       rcst())           /* COSTS PASSED IN            */

            job$ = tojob$
            jbcorewip$ = tojbcorewip$
            if jbcorewip$ = " " then gosub set_jb_core_wip
            acct$(1) = jbcorewip$   /* Debit  */
            call "CONVERT" (quan, 2.2, str(posttext$,31%,10%))

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       5%,               /* TYPE - CORE SHADOW         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       costs())          /* COSTS PASSED IN            */

            gosub post_gl
            return

L16000: REM *************************************************************~
            * ADJUST JOB LEDGER AND MOVE ADJUSTMENT TO INVENTORY        *~
            *************************************************************

            if abs(quan) < .0001 then return

            gosub get_jbpart
            acct$(2) = wipacct$
            acct% = 3%
            call "HNYGLGET" (part$, store$, lot$, hnyacct$, acct%,       ~
                             #3, #52)
                if acct% <= 0% then L16150
                   errortext$ = "INV. G/L ACCOUNT"
                   gosub bad_trans

L16150:     acct$(1) = hnyacct$

            get costs$, using L16180, costs()
L16180:         FMT 12*PD(14,4)

            cost = 0
            for b% = 1% to 12% : cost = cost + costs(b%) : next b%
            glamount(1) = cost
            glamount(2) = glamount(1)

            type% = 0%:convert str(work$,1,1) to type%, data goto L16270

L16270:     trdate$ = postdate$
            wc$, activity$, emp$, earntype$, lclass$ = " "
            unitrate = round(cost/quan, 4)
            if work1$ = " " then                                         ~
                    work1$ = "INV:" & str(part$) & str(store$) & lot$

            mat apst = (1/quan) * costs
            qtyadj   = quan
            iquan    = quan
            mat costs = (-1) * costs

            gltext$    = job$
            str(gltext$,31%) = str(part$) & str(store$) & lot$
            str(gltext$,69%), text$ = "JOB/INVENTORY ADJ."

            tran_type$(1) = "MPC01"
            tran_type$(2) = "MPC02"
            if export_on$ = "Y" then gosub load_gl_info

            gosub add_value

            gosub  post_gl

            str(gltext$,65, 4) = "ADJ"
            gosub hnyapst_call

            return

L17000: REM *************************************************************~
            *             V A L U E   J O B   T O   J O B               *~
            *-----------------------------------------------------------*~
            * Value transfer from job to job.                           *~
            *************************************************************

            get work$ using L17080, earntype$, lclass$, activity$,        ~
                                   unitrate, labor2, ovhd, fromjob$
L17080:         FMT CH(12), 2*CH(4), 3*PD(14,4), CH(8)
            get part$, using L17100, type%, emp$, wc$
L17100:         FMT BI(1), CH(12), CH(4)

            gosub get_jbpart
            acct$(1) = wipacct$
            if type% < 5% then L17150
               if jbcorewip$ = " " then gosub set_jb_core_wip
               acct$(1) = jbcorewip$


L17150:     get costs$, using L17160, costs()
L17160:         FMT 12*PD(14,4)

            cost = 0
            for b% = 1% to 12% : cost = cost + costs(b%) : next b%

            glamount(1) = round(cost, 2%)
            glamount(2) = glamount(1)

            gltext$ = fromjob$
            str(gltext$,31) = job$
            str(gltext$,69,32) = "VALUE MOVED JOB TO JOB"
            tojob$ = job$

            if work1$ = " " then                                         ~
                         work1$ = "VALUE MOVED FROM: " & fromjob$ &      ~
                                  " TO " & job$

            tran_type$(1), tran_type$(2) = "MVA01"
            if export_on$ = "Y" then gosub load_gl_info

            trdate$ = postdate$
            gosub add_value

            job$ = fromjob$
            mat costs = (-1) * costs
            quan = -quan
            iquan = -quan
            gosub get_jbpart
            acct$(2) = wipacct$
            if type% < 5% then L17390
               if jbcorewip$ = " " then gosub set_jb_core_wip
               acct$(2) = jbcorewip$

L17390:     gosub add_value

            gosub post_gl
            return

L18000: REM *************************************************************~
            *             M O V E   V A L U E   T O   J O B             *~
            *-----------------------------------------------------------*~
            * Post value added to job. (Job <-----> G/L)                *~
            *************************************************************

            gosub get_jbpart

            get work$ using L18100, earntype$, lclass$, activity$,        ~
                                   unitrate, labor2, ovhd, acct$(3)
L18100:         FMT CH(12), 2*CH(4), 3*PD(14,4), CH(9)
            get part$, using L18120, type%, emp$, wc$, trdate$
L18120:         FMT BI(1), CH(12), CH(4), CH(6)

            get costs$ using L18150, costs()
L18150:         FMT 12*PD(14,4)

            cost = 0
            for b% = 1% to 12% : cost = cost + costs(b%) : next b%

            glamount(1) = round(cost, 2%)
            glamount(2) = glamount(1)
            gltext$ = job$
            str(gltext$,69) = "VALUE ADDED TO JOB"
            acct$(1) = wipacct$
            acct$(2) = str(store$) & str(lot$) /* ACCT IS HID HERE */

            tran_type$(1) = "MVA01"
            if export_on$ = "Y" then gosub load_gl_info

            if type% <> 1% then acct$(3) = " "
            if trdate$ = " " then trdate$ = postdate$

            on type% goto L18380, L18480, L18530, L18580,                    ~
                          L18650, L18650, L18650
               put errortext$, using L18350, type%
L18350:           %JBVLPOST Type: ###
               goto bad_trans

L18380:     str(gltext$,31,12) = emp$
            gltext$ = gltext$ & " (LABOR)"
            tran_type$(2) = "MVA02"
            if acct$(3) = acct$(2) then acct$(3) = " "
            if acct$(3) = " "      then L18900
                tran_type$(3) = "MVA05"
                glamount(2) = round(labor2, 2)
                glamount(3) = round(ovhd, 2)
                goto L18900

L18480:     str(gltext$,31,4) = wc$
            gltext$ = gltext$ & " (WORK CENTER)"
            tran_type$(2) = "MVA03"
            goto L18900

L18530:     str(gltext$,31,6) = "DIRECT"
            gltext$ = gltext$ & " (DIRECT)"
            tran_type$(2) = "MVA04"
            goto L18900

L18580:     str(gltext$,31,20) = "CLOSING ADJUSTMENTS"
            gltext$ = gltext$ & " (CLOSING)"
            acct$(1) = acct$(2)
            acct$(2) = wipacct$
            tran_type$(2) = "MVA04"
            goto L18900

L18650:     if jbcorewip$ = " " then gosub set_jb_core_wip
            acct$(1) = jbcorewip$

            on (type% - 4%) goto L18700, L18750, L18800

L18700:     str(gltext$,31,25) = "CORE VALUE ADJUSTMENT"
            gltext$ = gltext$ & " (CORE DB)"
            tran_type$(2) = "MVA04"
            goto L18900

L18750:     str(gltext$,31,25) = "CORE CREDIT ADJUSTMENT"
            gltext$ = gltext$ & " (CORE CR)"
            tran_type$(2) = "MVA04"
            goto L18900

L18800:     str(gltext$,31,25) = "CORE CLOSING ADJUSTMENTS"
            gltext$ = gltext$ & " (CORE)"
            tran_type$(2) = "MVA04"
            acct$(1) = acct$(2)
            acct$(2) = jbcorewip$
            goto L18900

L18900:     gosub add_value

            iquan = quan
            gosub post_gl

            return

L19000: REM *************************************************************~
            * MOVE END ITEM FROM ONE JOB TO ANOTHER JOB AS A COMP.      *~
            *************************************************************

            if abs(quan) < .0001 then return
            gosub get_jbpart

            postdatef$ = postdate$  :  call "DATEFMT" (postdatef$)
            gltext$    = job$
            str(gltext$,31%) = str(part$) & "*" & str(work$,,8)
            str(gltext$,69%), text$ = "COMPLETED FROM JOB"
            text$ = text$ & ": " & job$ & " ON " & postdatef$
            tran_type$(1), tran_type$(2) = "MPC02"
            iquan = quan
            if export_on$ = "Y" then gosub load_gl_info
            costflag$ = str(work1$,,2)

            store$ = "*" & str(work$,,2)
            lot$   = str(work$,3,6)
            revkey$ = " "

            mat cbom = zer : mat cwip = zer : nonzero% = 0%
            if corebank% = 0% then L19100
            if str(costflag$,,1%) = "Z" then L19100
            plowkey$ = " " : str(plowkey$,,25%) = part$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, core%)
               if core% <> 0% then L19100
            if str(costflag$,,1%) = "S" then L19100
*        Get Core Actuals
            call "READ100" (#41, job$, f1%(41%)) /* JBMASTRC */
               if f1%(41%) = 0% then L19100      /* Not Core, no legder */
                                                /* Normal Completeion  */
            get #41 using L19068, cbom(), cshd(), ccdt(), cwip()
L19068:         FMT  POS(17), 12*PD(14,4), POS(113), 12*PD(14,4),        ~
                     POS(217), 12*PD(14,4), POS(425), 12*PD(14,4)

            mat cbom    = cbom + cshd       /* Uncredited costs- BOM  */
            mat cbom    = cbom - ccdt

            for b% = 1% to 12%
                cbom(b%) = round(cbom(b%), 4)
                if cbom(b%) <> 0 then nonzero% = 1%
            next b%

L19100:     call "JBCRPOST"                                              ~
                     (job$,              /* JOB  TO BE UPDATED         */~
                      costflag$,         /* "B" -or- "P"               */~
                      comperrcst$,       /* Cost Error Flag            */~
                      part$,             /* PART NUMBER TO POST        */~
                      store$,            /* STORE NUMBER               */~
                      lot$,              /* LOT NUMBER                 */~
                      quan,              /* QUANTITY MOVED TO JOB      */~
                      revkey$,           /* Reversal Key               */~
                      jbcosts(),         /* Inventory Costs per unit   */~
                      cbom(),            /* Core Adjustments           */~
                      cwip(),            /* Core moved to WIP          */~
                      wiptotal,          /* Total Posted to Job        */~
                      str(work$,,8),     /* TRANSFER TO JOB NUMBER     */~
                      postdate$,         /* POSTING DATE$              */~
                      userid$,           /* WHO                        */~
                      #4,                /* UFB ADDRESS OF JBMASTR2    */~
                      #7,                /* UFB ADDRESS OF JBCREDIT    */~
                      #54,               /* UFB ADDRESS OF SYSFILE2    */~
                      #59,               /* UFB ADDRESS OF JBCROSS2    */~
                      #6,                /* UFB ADDRESS OF JBMATER2    */~
                      #3,                /* UFB ADDRESS OF HNYMASTR    */~
                      return%)           /* ERROR RETURN FROM SUB      */

            if return% = 0% then L19148
               put errortext$, using L19142, return%
L19142:            %JBCRPOST Return: ###
               goto bad_trans

L19148:     if corebank% = 0% then L19428
            acct$() = " " : mat glamount = zer
            cost = 0 : mat costs = zer
            plowkey$ = " " : str(plowkey$,,25%) = part$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, core%)
               if core% <> 0% then L19226

*       ** POST COREWIP --> WIP IF NECESSARY
            if nonzero% = 0% then L19428
            if jbcorewip$ = " " then gosub set_jb_core_wip
            acct$(2) = jbcorewip$
            acct$(1) = wipacct$
            posttext$ = "CVF: VALUE TO F/G WIP"
            call "CONVERT" (quan, 2.2, str(posttext$,31%,10%))
            gltext1$ = gltext$
            str(gltext$,65%) = "    CVF: VALUE TO F/G WIP"

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       8%,               /* TYPE - CORE COMP. MEMO     */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       cbom())          /* COSTS PASSED IN            */

            for b% = 1% to 12%
                glamount(1) = glamount(1) + cbom(b%)
            next b%
            glamount(2) = glamount(1)
            goto L19420

*       ** POST COREWIP --> WIP IF NECESSARY

L19226:     corepart$ = str(plowkey$,26%,25%)
               call "STCCOSTS" (corepart$, " ", #54, 2%, cost, costs())
                  if cost = 0 then L19238
                     mat costs = (quan) * costs
                     glamount(1) = round(quan * cost, 2%)
                     glamount(3) = glamount(1)
L19238:            call "PACKZERO" (costs(), costs$)
                fromjob$       = job$
                fromwip$       = wipacct$
                fromjbcorewip$ = jbcorewip$
                job$           = str(work$,,8)
                gosub get_jbpart
                if jbcorewip$ = " " then gosub set_jb_core_wip
                acct$(1)   = jbcorewip$
                job$       = fromjob$
                wipacct$   = fromwip$
                jbcorewip$ = fromjbcorewip$
                acct$(3)   = dfcore_var$
                tojob$     = str(work$,,8)

*        Get Job Actuals
            mat cbom = zer
            if  str(costflag$,1%,1%)  = "Z" then L19308
            if  str(costflag$,1%,1%) <> "S" then L19276
                mat cbom = costs : goto L19308
L19276:     call "READ100" (#41, job$, f1%(41%)) /* JBMASTRC */
               if f1%(41%) = 0% then L19308
            get #41 using L19286, cbom(), cshd(), ccdt(), cwip()
L19286:         FMT  POS(17), 12*PD(14,4), POS(113), 12*PD(14,4),        ~
                     POS(217), 12*PD(14,4), POS(425), 12*PD(14,4)

            mat cbom    = cbom + cshd       /* Uncredited costs- BOM  */
            mat cbom    = cbom - ccdt
            mat cbom    = cbom - cwip

            if str(costflag$,1%,1%) <> "P" then L19308
            qltb = jbqty - jbdone
            mat cbom    = (quan)     * cbom
            mat cbom    = (1 / qltb) * cbom

L19308:     for b% = 1% to 12%
                cbom(b%) = round(cbom(b%), 4)
                glamount(2) = glamount(2) + cbom(b%)
            next b%
            glamount(3) = glamount(3) - glamount(2)

*        Post to Job & G/L
            if jbcorewip$ = " " then gosub set_jb_core_wip
            acct$(2) = jbcorewip$
            posttext$ = "CVJ: " & corepart$
            call "CONVERT" (quan, 2.2, str(posttext$,31%,10%))
            gltext1$ = gltext$
            str(gltext$,65%) = "    CVJ: " & corepart$

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       6%,               /* TYPE - CORE CREDIT         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       cbom())          /* COSTS PASSED IN            */

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       9%,               /* TYPE - EFFECT ON CFG       */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       costs())          /* COSTS PASSED IN            */

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #5,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       tojob$,           /* JOB  TO BE UPDATED         */~
                       5%,               /* TYPE - EFFECT ON CFG       */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       costs())          /* COSTS PASSED IN            */

L19420:     gosub post_gl
            acct$() = " " : mat glamount = zer
            gltext$ = gltext1$

L19428:     mat  costs = jbcosts
            call "PACKZERO" (costs(), newcosts$)

            jbcost = 0
            for b% = 1% to 12% : jbcost = jbcost + costs(b%) : next b%
            glamount(1) = round(quan * jbcost, 2)
            glamount(2) = round(wiptotal, 2)
            acct$(2) = wipacct$   /* Credit */

            gosub update_pipin_for_job

            fromjob$ = job$
            job$     = str(work$,,8)
            gosub get_jbpart
            acct$(1) = wipacct$   /* DEBIT  */
            store$   = "*" & str(fromjob$,,2)
            lot$     = str(fromjob$,3,6)
            ostore$, olot$ = " "

            gosub jbmtpost_call
               if coreflg$ = " " then L19480
                  if jbcorewip$ = " " then gosub set_jb_core_wip
                  acct$(1) = jbcorewip$

L19480:     gosub decrement_pipouts

            if abs(glamount(1) - glamount(2)) < .01 then L19600
               acct$(3) = wipvaracct$
               glamount(3) = round(glamount(1) - glamount(2), 2)

L19600:     gosub post_gl

            lot$      = "*" & str(job$,,5)
            store$    = str(job$,6,3)
            work$     = part$
            part$     = jbpart$
            jbpart$   = work$
            job$      = fromjob$
            lottype$  = "J"
            flotmisc$ = "P"

            gosub lottrack_call

            return

L20000: REM *************************************************************~
            * Directly Adjust Job Quantity                              *~
            *************************************************************

            if abs(quan) < .0001 then return

            gosub get_jbpart
            scradj, rwkadj = 0

            gosub jbmastr2_update
            return

        REM *************************************************************~
            * UPDATE JBMASTR2 FOR SCRAP AND REWORK                      *~
            *************************************************************
        jbmastr2_update

            put errortext$, using L21060, " "
L21060:     %Job Not On File    #
            call "READ101" (#4, job$, f1%(4))  /* JBMASTR2 */
                if f1%(4) = 0% then bad_trans
            currqty = max(jbdone, round(jbqty-quan, 2))
            put #4, using L21100, currqty
L21100:         FMT POS(83), PD(14,4)
            get #4, using L21120, scrap, rework, estcomp
L21120:         FMT POS(212), 2*PD(14,4), POS(1139), PD(14,4)
            scrap  = round(scrap  + scradj, 2)
            rework = round(rework + rwkadj, 2)
            if abs(currqty - jbdone) < .01 then estcomp = 100
            put #4, using L21120, scrap, rework, estcomp
                rewrite #4

            gosub update_pipin_for_job

            return

        REM *************************************************************~
            * PIPIN HANDLING FOR A JOB                                  *~
            *************************************************************

        update_pipin_for_job
            day% = jobend% : sihtp% = jobstart% : left = 0

            call "READ101" (#33, "JOB ORDER: " & str(job$,,8), f1%(33))
                if f1%(33)=0 then L21513
            get #33, using L21500, day%, left, sihtp%
L21500:         FMT XX(25), BI(4), XX(19), PD(14,4), BI(4)
            delete #33
            call "PIPFLAGS" (part$, 1%, day%, -left, #2, #58)

L21513:     now = 0
            if str(job$,,2%) <> "RW" then L21516
            if rwkpip$ <> "A" then L21530
L21516:        now = max(0, round((jbqty - jbdone) - quan, 2))
                                 /*Qty to Build      This Comp or Adj */
L21530:     if now = 0 then L21590
            write #33, using L21560, part$, day%, "JOB ORDER:", str(job$),~
                                    now, sihtp%
L21560:     FMT CH(25), BI(4), CH(11), CH(8), PD(14,4), BI(4)
            call "PIPFLAGS" (part$, 1%, day%, now, #2, #58)

L21590:     ooadj = round(now - left, 2)
            if ooadj = 0 then return
            call "HNYPST1" (part$, "001", " ", 0, 0, ooadj, 0, 0,        ~
                            #52, #3, #54, f2%(52), f2%(3), f2%(54),      ~
                            0%, return%)
            return

        REM *************************************************************~
            * CALL TO HNYPST2                                           *~
            *************************************************************
        hnypst2_call
            hnyacct$ = " "
            if hnytype$ = "JB" then goto L22130       /* No By-products */
            if hnytype$ <> "JK" then goto L22110
                quan = -quan                /* Kit/De-Kit transactions */
                gosub usage_capture
                quan = -quan
                goto L22130
L22110:     if hnytype$ <> "JC" then gosub usage_capture /* Completion */

L22130:     call "HNYPST2"   (part$,     /* PART TO BE UPDATED         */~
                      store$,            /* STORE CODE                 */~
                      lot$,              /* LOT NUMBER                 */~
                      quan,0,0,0,0,      /* POST TO ON HAND            */~
                      hnycosts(),        /* Per Unit Cost Breakdown    */~
                      hnycost,           /* Per Unit Cost              */~
                      0,                 /* UNIT PRICE                 */~
                      0,                 /* EXTENSION OF PRICE         */~
                      postdate$,         /* MODULE DATE (YYMMDD)       */~
                      hnytype$,          /* TRANSACTION TYPE (HNYDETAL)*/~
                      text$,             /* REFERENCE TEXT STRING      */~
                      hnyacct$,          /* TO ACCOUNT                 */~
                      wipacct$,          /* FROM ACCOUNT (wip)         */~
                      3%,2%,             /*                            */~
                      modno$,            /* MODULENO$                  */~
                      " ",               /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      gltext$,           /* GL TEXT STRING             */~
                      userid$,           /* WHO                        */~
                      #52, #56, #54,     /* HNYQUAN ,HNYDETAL,SYSFILE2 */~
                      #55, #3 , #2 ,     /* HNYPOOLS,HNYMASTR,PIPMASTR */~
                      #58, #12, #57,     /* SFCUM2  ,GLMAIN  ,GLDETAIL */~
                      #50, 0%, return%)  /* WRKFILE,  Rpt?, Ret Status */

                if return% <= 0% then return

                put errortext$, using L22400, return%
L22400:             %HNYPST2 RETURNED:###
                goto bad_trans

        REM *************************************************************~
            * CALL TO CONSOLIDATE                                       *~
            *************************************************************

        consolidate_array_and_post
                first% = 1%
                plowwrk$ = all (hex(00))
L22570:         call "PLOWNXT1" (#50, plowwrk$, 0%, f1%(50))
                     if f1%(50) = 0 and first% = 1% then L22800
                     if f1%(50) = 0 then L22760
                     first% = 0%
                get #50, using L22620, cmbtext$, dbamt, cramt
L22620:         FMT XX(25), CH(109), 2*PD(14,4)
                cntr% = cntr% + 1%
                passedin_acct$(cntr%) = cmbtext$
                passedin_dbcr(cntr%,1%) = dbamt
                passedin_dbcr(cntr%,2%) = cramt
*              TRAN_TYPE$ = TRAN_TYPE$(1)
                passedin_type$(cntr%) = str(tran_type$(1),4,2)
                if str(cmbtext$,76,1) = "H" then L22730
                passedin_type$(cntr%) = "04"
                if str(cmbtext$,74,1) = "S" then L22730
                passedin_type$(cntr%) = "03"
L22730:         delete #50
                goto L22570

L22760:         call "GLCMBSUB" (passedin_acct$(), passedin_dbcr(),      ~
                                 passedin_type$(), #54, cntr%)

        REM POST GL
L22800:     for x% = 1% to 50%
                if passedin_acct$(x%) = " " then goto L22930
                acct$(1) = str(passedin_acct$(x%),,9)
                acct$(2) = acct$(1)
                gltext$ = str(passedin_acct$(x%),10, 100)
                glamount(1) = passedin_dbcr(x%,1)
                glamount(2) = passedin_dbcr(x%,2)
                str(tran_type$(1),4,2) = passedin_type$(x%)
                str(tran_type$(2),4,2) = passedin_type$(x%)
                iquan = quan
                if x% > 2% then iquan = 0
                gosub post_gl
            next x%
L22930:         return

        REM *************************************************************~
            * CALL TO HNYAPST                                           *~
            *************************************************************
        hnyapst_call

            call "HNYAPST" (part$, store$, lot$, qtyadj, apst(), apst$(),~
                            #52, #54, #55, #3, u3%)
            if u3% <> 1% and u3% <> 2% then return
            apst = 0
            for  a% = 1% to 12%
                apst(a%) = round(apst(a%) * qtyadj, 2%)
                apst     = apst + apst(a%)
            next a%

            if abs(apst) < .01 then L23320
            if function% <> 1% then L23190
                passedin_dbcr(1,1) = passedin_dbcr(1,1) - apst
                goto L23320

L23190:         glamt = apst
                if export_on$ <> "Y" then L23230
                put gl_post_info$() using L23220, tran_type$(1), -glamt, 0
L23220:              FMT CH(5), POS(18), 2*PD(15,4)
L23230:         call "GLPOST2" (hnyacct$, 0, glamt, postdate$, 0%,       ~
                                modno$, gltext$, jnlid$, pstseq%,        ~
                                userid$, #12, #57, #54, return%,         ~
                                job$, gl_post_info$())
                if gllog$ = "N" then L23320
                     call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$,  ~
                                 postdate$, hnyacct$, gltext$, 0, glamt, ~
                                 #22, f2%(21%))

L23320:     a1% = 1% : a2% = 12% : ao% = 1%
            tran_type$ = str(tran_type$(1),,3) & "04"
            if u3% <> 1% then L23380
                a1%, a2% = 1%    : ao% = 0%
                apst(1)  = apst
                tran_type$ = str(tran_type$(1),,3) & "03"
L23380:     for  a% = a1% to a2%
                if abs(apst(a%)) < .01 then goto L23590
                     glamt = apst(a%)
                     if function% <> 1% then L23490
                          cntr% = cntr% + 1%
                          passedin_acct$(cntr%) = str(apst$(a%+ao%)) &   ~
                                                                  gltext$
                          passedin_dbcr(cntr%,1) = glamt
                          passedin_dbcr(cntr%,2) = 0
                          passedin_type$(cntr%) = str(tran_type$,4,2)
                          goto L23590
L23490:              if export_on$ <> "Y" then L23510
                     put gl_post_info$() using L23220, tran_type$, glamt,0
L23510:              call "GLPOST2" (apst$(a%+ao%), glamt, 0, postdate$, ~
                                     0%, modno$, gltext$, jnlid$,pstseq%,~
                                     userid$, #12, #57, #54, return%,    ~
                                     job$, gl_post_info$())
                     if gllog$ = "N" then L23590
                     call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$,  ~
                                      postdate$, apst$(a%+ao%), gltext$, ~
                                      glamt, 0, #22, f2%(21%))
L23590:     next a%

            return

        REM *************************************************************~
            * CALL TO LOTTRACK                                          *~
            *************************************************************
        lottrack_call

            call "LOTTRACK"                                              ~
                 ("J",                   /* FROM FLAG  J = JOB         */~
                  jbpart$,               /* PART BEING MOVED           */~
                  "*" & str(job$,,5),    /* FROM LOT                   */~
                  str(job$,6,3),         /* FROM STORE                 */~
                  flotmisc$,             /* FROM MISC.                 */~
                  lottype$,              /* TO FLAG H=INV., J = JOB    */~
                  part$,                 /* TO PART                    */~
                  lot$,                  /* TO LOT                     */~
                  store$,                /* TO STORE                   */~
                  tlotmisc$,             /* TO MISC.                   */~
                  quan,                  /* QUANTITY                   */~
                  #3,                    /* 'HNYMASTR' FILE            */~
                  #54)                   /* 'SYSFILE2' FILE            */
            flotmisc$, tlotmisc$ = " "   /* Lot Track Misc Fields JIC  */
            return

        REM *************************************************************~
            * MOVE MATERIALS OUT OF JOB                                 *~
            *************************************************************
        material_from_job

*        Adjust materials ledger for quan moved out...
            ostore$, olot$ = " "
            if str(work$,9,22) = " " then L25280
            call "READ101" (#6, str(work$,9,22), f1%(6)) /* JBMATR2 */
               if f1%(6) = 0% then L25280            /* C H E C K */
                  get #6, using L25120, tstore$, tlot$, moved, ostore$,   ~
                                       olot$
L25120:               FMT POS(48), CH(3), CH(6), POS(330), PD(14,4),     ~
                          POS(342), CH(3), CH(6)
                  put #6, using L25150, moved + quan
L25150:               FMT POS(330), PD(14,4)
                  rewrite #6
            /* Realign commited for material moved out...  */
               if str(tstore$,,1) = "*" then L25200
                  ostore$ = tstore$ : olot$ = tlot$
L25200:           if ostore$ = " " then L25280

               if str(store$,,1)  = "*" then L25280  /* to another JOB */

                  call "HNYPST1" (part$, ostore$, olot$, 0, 0, 0, -quan, ~
                                0, #52, #3, #54, f2%(52), f2%(3),        ~
                                f2%(54), 0%, return%)

L25280
*        Get value of materials being moved...
            get costs$, using L25300, costs()
L25300:         FMT 12*PD(14,4)
            cost = 0
            for b% = 1% to 12% : cost = cost + costs(b%) : next b%

            quan = - quan
            gosub jbmtpost_call
            quan = - quan

            return

        REM *************************************************************~
            * CALL TO JBMTPOST                                          *~
            *************************************************************
        jbmtpost_call
            gosub check_for_core
            call "JBMTPOST" (job$,       /* JOB  TO BE UPDATED         */~
                      part$,             /* PART NUMBER TO POST        */~
                      store$,            /* STORE NUMBER               */~
                      lot$,              /* LOT NUMBER                 */~
                      quan,              /* QTY MOVED TO JOB           */~
                                         /* ALL COSTS PER UNIT         */~
                      costs(),           /* Inventory Costs            */~
                      ostore$,           /* ORIGINAL STORE NUMBER      */~
                      olot$,             /* ORIGINAL LOT NUMBER        */~
                      postdate$,         /* POSTING DATE               */~
                      userid$,           /* WHO                        */~
                      coreflg$,          /* Core Flag                  */~
                      #4,                /* UFB ADDRESS OF JBMASTR2    */~
                      #6,                /* UFB ADDRESS OF JBMATER2    */~
                      #54,               /* UFB ADDRESS OF SYSFILE2    */~
                      #41,               /* UFB Address of JBMASTRC    */~
                      return%)           /* ERROR RETURN FROM SUBROUTIN*/~

            return

        check_for_core
               coreflg$ = " "
               if corebank% = 0% then return
               plowkey$ = " " : str(plowkey$,26%) = part$
               call "REDALT0" (#40, plowkey$, 0%, f1%(40%))
                  if f1%(40%) = 0% then return
               coreflg$ = "CORE"
               return

        REM *************************************************************~
            * DECREMENT NEGATIVE PIPOUTS (IF ANY) FOR BYPRODUCTS        *~
            *************************************************************
        byproduct_pipouts

            if quan < 0 then return
            temp = quan
            readkey$ = "JOB ORDER: " & str(job$,,8) & str(part$,,25)
            init (hex(00)) str(readkey$,45)

L26100:     call "PLOWNXT1" (#34, readkey$, 44%, f1%(34))
               if f1%(34) = 0% then return
            get #34, using L26130, out%, pipqty
L26130:         FMT XX(44), BI(4), XX(8), PD(14,4)
            if pipqty > 0 then L26100
            hnytype$ = "JB"
            temp = temp + pipqty
            if temp > 0 then newpip = 0 else newpip = temp
            put #34, using L26190, newpip
L26190:         FMT POS(57), PD(14,4)
            if newpip = 0 then delete #34 else rewrite #34
            call "PIPFLAGS" (part$, 1%, out%, pipqty-newpip, #2, #58)
            if temp > 0 then L26100

            return

        REM *************************************************************~
            * DECREMENT PIPOUTS (IF ANY) FOR COMP. JOB TO JOB           *~
            *************************************************************
        decrement_pipouts

            if quan < 0 then return
            temp = quan
            readkey$ = "JOB ORDER: " & str(job$,,8) & str(part$,,25)
            init (hex(00)) str(readkey$,45)

L26600:     call "PLOWNXT1" (#34, readkey$, 44%, f1%(34))
               if f1%(34) = 0% then return
            get #34, using L26630, out%, pipqty
L26630:         FMT XX(44), BI(4), XX(8), PD(14,4)
            if pipqty < 0 then L26600

            temp = temp - pipqty
            if temp > 0 then newpip = 0 else newpip = -temp
            put #34, using L26690, newpip
L26690:         FMT POS(57), PD(14,4)
            if newpip = 0 then delete #34 else rewrite #34
            call "PIPFLAGS" (part$, 1%, out%, pipqty-newpip, #2, #58)
            if temp > 0 then L26600

            return

        REM *************************************************************~
            * CALL TO JBVLPOST                                          *~
            *************************************************************

        add_value
            call "JBVLPOST"              /*                            */~
                     (#4, #5, #41,       /* JB-MASTR2, VALUE2, MASTRC, */~
                      return%,           /* Return Code                */~
                      job$,              /* JOB  TO BE UPDATED         */~
                      type%,             /* Type of Transaction        */~
                      trdate$,           /* TRANSACTION DATE           */~
                      postdate$,         /* DATE WORK WAS PERFORMED    */~
                      userid$,           /* User ID                    */~
                      work1$,            /* FREE TEXT                  */~
                      costs(),           /* Costs                      */~
                      unitrate,          /* Unit Rate ($/Unit)         */~
                      quan,              /* Amount of Units            */~
                      wc$,               /* Work Station               */~
                      activity$,         /* Activity                   */~
                      emp$,              /* Employee Code              */~
                      earntype$,         /* Earnings Type              */~
                      lclass$,           /* Labor Class Code           */~
                      " ")               /* Unit Description           */

            if return% = 0% then return
               put errortext$, using L27260, return%
L27260:           %JBVLPOST Return: ###
               goto bad_trans

        REM *************************************************************~
            * SET UP REWORK JOB                                         *~
            *************************************************************
        set_up_rework_job
            acct$(1%) = " "
*        If Job # invalid, then get next rework job number
            if str(tojob$,,2) = "RW" then goto L28100 /* S/b blank, but */
                                                     /* just in case.  */
L28068:         call "READ101" (#54, "SWITCHS.SFC", f1%(54%))
                    if f1%(54%) = 0% then L28094
                get #54 using L28072, next_rwjob$, rw_incr%
L28072:             FMT POS(85), CH(8), BI(4)
*        Find position of job numbers to increment...
            if next_rwjob$ = " " then next_rwjob$ = "RW000000"
            rw_incr% = max(rw_incr%, 1%) /* Ensure incr. is at least 1 */
            temp$, tojob$ = next_rwjob$
            for i% = 1% to 8%
                if str(temp$,i%,1%) < "0" or str(temp$,i%,1%) > "9"      ~
                    then str(temp$,i%,1%) = " "
            next i%
            temp% = pos(-temp$ <> " ")
            auto%(1%) = pos(-str(temp$,,temp%)=" ")+1%
            auto%(2%) = (temp% - auto%(1%)) + 1%
            convert str(next_rwjob$,auto%(1%),auto%(2%)) to job%,        ~
                                                         data goto L28068
*        Increment rework job number and save
            job% = job% + rw_incr%
            convert job% to temp$, pic(00000000)
            if auto%(2%) < 8% then L28093
                if pos(temp$ = "#") > 0% then L28094
                    goto L28095
L28093:     if pos(str(temp$,,8%-auto%(2%)) > "0") = 0% then L28095
L28094:         next_rwjob$ = " "  :  return
L28095:     str(next_rwjob$, auto%(1%), auto%(2%)) =                     ~
                                          str(temp$, 8% - auto%(2%) + 1%)
            put #54 using L28098, next_rwjob$
L28098:         FMT POS(85), CH(8)
            rewrite #54
L28100:     call "READ100" (#4, tojob$, f1%(4%))
                if f1%(4%) = 1% then L28068 /* If already used, go try */
                                           /* another job number.     */
*        If WIP Account # Blank, get it
            if acct$(1%) <> " " then L28140
                call "READ100" (#54, "SWITCHS.SFC", f1%(54%))
                    if f1%(54%) = 0% then L28110
                get #54 using L28108, acct$(1%)
L28108:             FMT POS(97), CH(9)
                if acct$(1%) <> " " then L28140
L28110:             call "READ100" (#54, "FISCAL DATES", f1%(54%))
                        if f1%(54%) = 0% then L28140
                    get #54 using L28113, acct$(1%)
L28113:                 FMT POS(417), CH(9)

L28140:     init (hex(00)) costs$
            jobname$ = "REWORK FROM JOB: " & job$
            call "PIPINDEX" (#54, str(work1$,,6), rwkend%, 0%)
            call "PIPINDEX" (#54, date, rwkstart%, 0%)

            write #4  using L28360, tojob$, jobname$,                     ~
                                   "JOB ORDER: ", tojob$,                ~
                                   part$, 0, 0, " ", date, blankdate$,   ~
                                   acct$(1), date, str(work1$,,6), quan, ~
                                   " ", 0, 0, " ",                       ~
                                   costs$, costs$, costs$, costs$,       ~
                                   costs$, costs$, costs$, costs$,       ~
                                   costs$, costs$, costs$, costs$,       ~
                                   " ", 0, costs$, costs$, " "

            costs$ = newcosts$
            return

L28360:         FMT       CH(8),         /* JOB NUMBER                 */~
                          CH(30),        /* JOB DESCRIPTION            */~
                          CH(11), CH(8), /* TAG TO PIP                 */~
                          CH(25),        /* PART TO BUILD              */~
                          PD(14,4),      /* QUANTITY TO MAKE           */~
                          PD(14,4),      /* QUANTITY COMPLETE          */~
                          CH(48),        /* Filler                     */~
                          CH(6),         /* START DATE                 */~
                          CH(6),         /* CLOSE DATE                 */~
                          CH(9),         /* WIP ACCOUNT                */~
                          CH(6),         /* PLANNED START              */~
                          CH(6),         /* PLANNED END                */~
                          PD(14,4),      /* Original Quantity To Build */~
                          CH(24),        /* Filler                     */~
                          PD(14,4),      /* QUANTITY SCRAPPED          */~
                          PD(14,4),      /* QUANTITY SENT TO REWORK    */~
                          CH(4),         /* Text ID                    */~
                          CH(8), 3*CH(96),         /* Job Debits       */~
                          CH(8), 3*CH(96),         /* Job Credits      */~
                          CH(8), 3*CH(96),         /* Std Costs        */~
                          CH(19),        /* Control Number             */~
                          PD(14,4),      /* Est. % Complete            */~
                          CH(8), CH(96), /* zeroes                     */~
                          CH(50)         /* Filler                     */

        REM *************************************************************~
            * Mark Receiver Distributions with CR Key (or blank)        *~
            *************************************************************

        rcv_dist_tag
        /* Purchase Jobs Receiver Update for reversals                 */
            if rcvkey$ = " " then return
               init (hex(00)) str(rcvkey$,45)
               call "PLOWNXT1" (#25, rcvkey$, 44%, f1%(25%))
                    if f1%(25%) = 0% then return  /* No Distr. Either */
               get #25 using L29110, rcvstr$, rcvlot$
L29110:            FMT POS(341), CH(3), CH(6)
               if rcvstr$ <> store$ then L29180
               if rcvlot$ <> lot$   then L29180
                  put #25 using L29150, revkey$, rcvfail
L29150:               FMT POS(616), CH(22), PD(14,4)
                  rewrite #25

L29180:        str(rcvkey$,45) = str(part$,,25) & str(store$,,3) & lot$
               init (hex(00)) str(rcvkey$,79)

L29210:        call "PLOWNXT1" (#26, rcvkey$, 78%, f1%(26%))
                    if f1%(26%) = 0% then return   /* No More Distr.   */
               put #26 using L29240, revkey$, rcvfail
L29240:            FMT POS(151), CH(22), PD(14,4)
               rewrite #26
               goto L29210

        REM *************************************************************~
            *         C O M M O N   G / L   P O S T   L O G I C         *~
            *                                                           *~
            * ACCT$(), GLTEXT$, and GLAMOUNT must be set prior to this. *~
            *************************************************************

        post_gl

            if export_on$ <> "Y" then L30080
            put gl_post_info$() using L30076, tran_type$(1), glamount(1), ~
                                                                    iquan
L30076:         FMT CH(5), POS(18), 2*PD(15,4)

L30080:     REM Account in ACCT$(1) is debited...

            if glamount(1) = 0 then L30280
            if export_on$ <> "Y" then L30090
            if function% = 2% or function% = 3% or function% = 7% or     ~
               function% = 9% then str(gl_post_info$(),252,8) = tojob$

L30090:     call "GLPOST2" (acct$(1),    /* ACCOUNT TO BE UPDATED      */~
                      glamount(1),       /* DEBIT AMOUNT (0 IF CREDIT) */~
                      0,                 /* CREDIT AMOUNT (0 IF DEBIT) */~
                      postdate$,         /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      modno$,            /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      #12,               /* UFB ADDRESS OF G/L MAIN    */~
                      #57,               /* UFB ADDRESS OF G/L DETAILS */~
                      #54,               /* UFB ADDRESS OF SYSFILE2    */~
                      return%,           /* ERROR RETURN FROM SUBROUTIN*/~
                      job$, gl_post_info$())

            if gllog$ = "N" then L30280
            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, postdate$,~
                         acct$(1), gltext$, glamount(1), 0, #22, f2%(22))

L30280:     REM Account in ACCT$(2), ...  is credited...
            if export_on$ <> "Y" then L30290
            if function% = 2% or function% = 3% or function% = 7% or     ~
               function% = 9% then str(gl_post_info$(),252,8) = job$

L30290:     for credit% = 2% to 4%
               if acct$(credit%) = " " then L30520
               if glamount(credit%) = 0 then L30520

            if export_on$ <> "Y" then L30330
            put gl_post_info$() using L30076, tran_type$(credit%),        ~
                                            -glamount(credit%), -iquan

L30330:     call "GLPOST2" (acct$(credit%),   /* ACCOUNT TO BE UPDATED */~
                      0,                 /* DEBIT AMOUNT (0 IF CREDIT) */~
                      glamount(credit%), /* CREDIT AMOUNT (0 IF DEBIT) */~
                      postdate$,         /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      modno$,            /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      #12,               /* UFB ADDRESS OF G/L MAIN    */~
                      #57,               /* UFB ADDRESS OF G/L DETAILS */~
                      #54,               /* UFB ADDRESS OF SYSFILE2    */~
                      return%,           /* ERROR RETURN FROM SUBROUTIN*/~
                      job$, gl_post_info$())

            if gllog$ = "N" then L30520
            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, postdate$,~
             acct$(credit%), gltext$, 0, glamount(credit%), #22, f2%(22))

L30520:     next credit%

        return

        usage_capture             /* Post Quantity Used to Usage Files */
            r_a$ = "A"                             /* Actual Usage     */
            usetype$ = "PROD"                      /* Production Usage */
            usedate$ = postdate$
            call "DATEOK" (usedate$, 0%, " ")
            call "DATUNFMT" (usedate$)
            call "HNYUSESB" (useso$, useseq$, store$, part$, r_a$,       ~
                  usedate$, usetype$, quan)
            return

        REM *************************************************************~
            *         M I S C E L L E N E O U S   R O U T I N E S       *~
            *                                                           *~
            * Some common routines that are shared by trans types.      *~
            *************************************************************

        get_jbpart
            put errortext$, using L31220, " "
            call "READ100" (#4, job$, f1%(4))      /* JBMASTR2 */
            if f1%(4) = 0% then bad_trans

            put errortext$, using L31230, " "
            get #4, using L31240, tagnr$, jbpart$, jbqty, jbdone,         ~
                    jbcorewip$, start$, open$, wipacct$, plend$
            if open$ <> " " and open$ <> blankdate$ then bad_trans
            call "PIPINDEX" (#54, plend$, jobend%, return%)
            call "PIPINDEX" (#54, start$, jobstart%, return%)
            call "SERENABL" (jbpart$, p_enabled%, u3%, #54, #3)
            if p_enabled% = 0% then L31201
            call "READ100" (#59, tagnr$, f1%(59))
            if f1%(59) = 0% then bomid$, rteid$ = " "                    ~
                            else get #59 using L31260, rteid$, bomid$
L31201:     if export_on$ <> "Y" then return
            call "READ100" (#3, part$, f1%(3))
                if f1%(3) = 0% then return
            get #3 using L31270, partgen$, uom$, partcat$, partclass$,    ~
                                parttype$
        return
L31220:     %Invalid Job Number #
L31230:     %Job Is Closed      #
L31240:     FMT POS(39), CH(19), CH(25), 2*PD(14,4), CH(9), POS(147),    ~
                CH(6), CH(6), CH(9), POS(174), CH(6)
L31260:     FMT POS(26), CH(3), POS(73), CH(3)
L31270:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)

        set_jb_core_wip
            jbcorewip$ = dfcore_wip$
            plowkey$ = jbpart$
            call "PLOWNEXT" (#40, plowkey$, 25%, f1%(40%))
               if f1%(40%) = 0% then L31380
            get #40 using L31360, jbcorewip$
L31360:         FMT POS(146), CH(9)
            if jbcorewip$ = " " then jbcorewip$ = dfcore_wip$
L31380:     call "READ101" (#4, job$, f1%(4))
               if f1%(4%) = 0% then return /* UNLIKELY */
            put #4 using L31410, jbcorewip$
L31410:         FMT POS(99), CH(9)
            rewrite #4
            return

        REM *************************************************************~
            * UPDATE TRANSACTION IMAGE FILE WITH STATUS (GOOD OR BAD)   *~
            *************************************************************

        bad_trans
            gosub load_trans
            str(record$(),9,1) = "X"   /* Mark as 'Bad' */
            str(record$(),280,20) = errortext$
            goto rewrite_trans

        good_trans
            gosub load_trans
            if savetif$  = "N" then goto_next_trans
            if function% > 10% then goto_next_trans
                str(record$(),9,1) = "O"   /* Mark as 'Old' */
        rewrite_trans
                put str(record$()) using L35150, newcosts$
L35150:              FMT POS(184), CH(96)
                write #10, using L35240, record$()
        goto_next_trans
            return clear all
            goto next_trans

        load_trans
            call "READ101" (#10, key(#10), f1%(10))
                if f1%(10) = 0 then goto_next_trans   /* Can't happen */
            get #10, using L35240, record$()
L35240:     FMT 7*CH(50)
            delete #10
        return

        REM *************************************************************~
            * SERIAL NUMBER UPDATING SECTION                            *~
            *************************************************************

        serial_number_handling
            if p_enabled% = 0% then return         /* Not a S/N  */

        plow_job_serial_numbers
            call "PLOWNEXT" (#61, plowkey$, 13%, f1%(61))
            if f1%(61) = 0% then delete_from_sertif
            serial$ = str(plowkey$, 43%,20%)
            readkey$ = str(part$) & serial$
            call "READ101" (#62, readkey$, f1%(62))
            if f1%(62) = 0% then plow_job_serial_numbers
            if function% = 4% or function% = 2% then L40430
            call "GETDTTM" addr(datetime$)
            put #62 using L41330,                                         ~
            status$,        /* Current Status Of a Serial # Part       */~
            location$,      /* Current Location Of a Serial # Part     */~
            job$,           /* Job Number built by                     */~
            bomid$,         /* Built using BOM identifier              */~
            rteid$,         /* Built using RTE identifier              */~
            start$,         /* Date production job actually started    */~
            postdate$,      /* Date Job Completed                      */~
            store$,         /* Last Inventory Store this item was in.  */~
            lot$,           /* Last Inventory Lot this item was in.    */~
            datetime$,      /* System Date and Time                    */~
            userid$,        /* Last modified by user.                  */~
            " ",            /* Type of Transaction currently holding   */~
                            /* this resource.                          */~
            " "             /* Transaction or Document Line Key        */~
                            /* currently holding this resource.        */

            rewrite #62

*        *** Completion or Scrap ***
            if function%=1% or function%=6% then plow_job_serial_numbers
            if function% <> 7% then L40430
               REM *** Transfer to Rework Job ***
                  comp_stat$ = "3"
                  comp_loc$  = str(tojob$) & serial$
                  readkey$ = "3" & str(job$) & serial$
                  goto complete_component_serial_nbrs
L40430:     if function% <> 4% then L40490
               REM *** Transfer to Job to Inventory ***
                  put #62 using L40680, status$, location$
                  rewrite #62
                  gosub detach_serial_number
                  goto plow_job_serial_numbers
L40490:     if function% <> 2% then L40550
               REM *** Transfer to Job to Job ***
                  put #62 using L40680, status$, location$
                  rewrite #62
                  gosub detach_serial_number
                  REM NEED TO clone attach code out of JBPOST1
L40550:     goto plow_job_serial_numbers /* Can't Get this far (i hope)*/

        REM *************************************************************~
            *    Find Any Component S/N's Tagged to this Job & Serial   *~
            *    Number and change their Status / Location.             *~
            *************************************************************
        complete_component_serial_nbrs
            call "PLOWAL1" (#62, readkey$, 2%, 31%, f1%(62))
            if f1%(62) = 0% then plow_job_serial_numbers
            put #62 using L40680, comp_stat$, comp_loc$
            rewrite #62
            goto complete_component_serial_nbrs

L40680:     FMT CH(1), CH(30)

        detach_serial_number
            readkey$ = str(part$) & serial$
            call "PLOWAL1" (#63, readkey$, 1%, 45%, f1%(63))
            if f1%(63) = 0% then return
            delete #63
            return

        delete_from_sertif
            call "DELETE" (#61, plowkey$, 42%)
            return

        /*  The below Routine determines whether it is necessary to De-Kit
            a Lot Tracked Component Part from a Job that is building
            Serialized Parts.  If it is necessary, the components (part,
            store, lot) are located and detached from the Old Parent S/N
        */
        detach_lottrack_components
            if p_enabled% = 0% then return
            call "PLOWNXT1" (#61, plowkey$, 13%, f1%(61))
            if f1%(61) = 0% then return
            get #61 using L41200,                                         ~
                serial$,    /* Parent S/N the below Component was      */~
                            /* kitted to.                              */~
                ser_store2$,/* Store Part was originally kitted from.  */~
                ser_lot2$,  /* Lot   Part was originally kitted from.  */~
                qty_kitted  /* Quantity Kitted.                        */~
                            /* Quantity = 1 if S/N field contains a    */~
                            /* real S/N. If S/N field contains         */~
                            /* Store&Lot then = Qty DeKitted           */

            delete #61
            readkey$ = part$
            str(readkey$,46%) = str(jbpart$) & str(serial$) & hex(000000)
L41030:     call "PLOWAL1" (#63, readkey$, 1%, 90%, f1%(63))
            if f1%(63) = 0% then detach_lottrack_components
            get #63 using L41170, ser_store$, ser_lot$
            if ser_store$ <> ser_store2$ or                              ~
               ser_lot$ <> ser_lot2$ then L41030
            get #63 using L41180, qty_in, qty_out, cur_qty
            qty_out = qty_out + abs(qty_kitted)
            cur_qty = qty_in - qty_out
            if cur_qty > 0 then L41140
               delete #63
               goto detach_lottrack_components
L41140:     put #63 using L41180, qty_in, qty_out, cur_qty
            rewrite #63
            goto detach_lottrack_components
L41170:        FMT POS(94), CH(3), CH(16)
L41180:        FMT POS(121), 3*PD(14,4)

L41200: FMT                 /* FILE: SERTIF                            */~
            POS(14),        /* Position for Field PARENT SERIAL NUMBER */~
            CH(20),         /* Parent S/N the below Component was      */~
            POS(43),        /* Position for Field COMPONENT SERIAL NBR */~
            CH(3),          /* Store Part was originally kitted from.  */~
            CH(16),         /* Lot   Part was originally kitted from.  */~
                            /* kitted to.                              */~
            POS(88),        /* Position for Field QUANTITY             */~
            PD(14,4)        /* Quantity Kitted.                        */~
                            /* Quantity = 1 if S/N field contains a    */~
                            /* real S/N. If S/N field contains         */~
                            /* Store&Lot then = Qty DeKitted           */

L41330: FMT                 /* FILE: SERMASTR                          */~
            CH(1),          /* Current Status Of a Serial Numbered     */~
                            /* Part.                                   */~
                            /* Start of Alt Key 2                      */~
            CH(30),         /* Current Location Of a Serial Numbered   */~
                            /* Part.                                   */~
            POS(97),        /* Position for Field JOB-NO               */~
            CH(8),          /* Job Number                              */~
                            /* Built by Job Number                     */~
            POS(165),       /* Position for Field BOM-ID               */~
            CH(3),          /* The specific BOM identifier for a Bill  */~
                            /* of Material                             */~
                            /* Built using BOM ID (Origin is           */~
                            /* JBCROSS2).                              */~
            CH(3),          /* The specific routing to use for a Bill. */~
                            /* Built Using RTE ID (origin is           */~
                            /* JBCROSS2).                              */~
            CH(6),          /* Date production job actually started    */~
            CH(6),          /* Date Job Completed                      */~
                            /* Date built (completed), Received        */~
                            /* (purchased), Added (HNYADDNS), ...      */~
            CH(3),          /* Warehouse or Store                      */~
                            /* Last Inventory Store this item was in.  */~
            CH(16),         /* Which lot in inventory - always used    */~
                            /* with STORE                              */~
                            /* Last Inventory Lot this item was in.    */~
            CH(7),          /* The system date a file or record was    */~
                            /* last modified                           */~
                            /* The System Time when a transaction was  */~
                            /* entered                                 */~
            CH(3),          /* user-id of specific user                */~
                            /* Last modified by user.                  */~
            POS(216),       /* Position for Field TRANS-TYPE           */~
            CH(2),          /* Identifies type of transaction.         */~
                            /* Type of Transaction currently holding   */~
                            /* this resource.                          */~
            CH(40)          /* Key to Original Transaction Item        */~
                            /* record.                                 */~
                            /* Transaction or Document Line Key        */~
                            /* currently holdingthis resource.         */


        /* A Completion has failed, Unlock the Serial #'s  */

        reverse_serial_numbers

            plowkey$ = "JC" & str(job$) & str(work1$,38%,3%)

L41860:     call "PLOWNEXT" (#61, plowkey$, 13%, f1%(61))
               if f1%(61%) <> 0% then L41920
                  init (hex(00)) str(plowkey$,14%)
                  call "DELETE" (#61, plowkey$, 13%)
                  return

L41920:     get #61 using L41940, str(plowkey1$,26%,20%),                 ~
                                 str(plowkey1$, 1%,25%)
L41940:         FMT POS(43), CH(20), CH(25)
            call "READ101" (#62, plowkey1$, f1%(62))
               if f1%(62%) = 0% then L41860
            put #62 using L41980, "1", "WP"
L41980:         FMT POS(1), CH(1), POS(216), CH(2)
            rewrite #62
            goto L41860

L50000: REM *************************************************************~
            * CLOSE G/L                                                 *~
            *************************************************************~

            call "JNLCLOSE" (modno$, jnlid$, pstseq%, return%)
            return

        REM *************************************************************~
            * LOAD G/L Export file information                          *~
            *************************************************************~

        load_gl_info

            put str(gl_post_info$(),,) using L51530,                      ~
                " ",                     /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Transaction Currency amount*/~
                0,                       /* Functional Currency amount */~
                0,                       /* Unit amount                */~
                " ",                     /* Customer code CH(9)        */~
                " ",                     /* Sales Order number CH(16)  */~
                " ",                     /* BOL number CH(3)           */~
                " ",                     /* Customer Type CH(2)        */~
                " ",                     /* State CH(2)                */~
                " ",                     /* Country CH(3)              */~
                " ",                     /* ZIP CH(9)                  */~
                " ",                     /* Sales Region CH(4)         */~
                " ",                     /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                " ",                     /* Salesman code CH(4)        */~
                " ",                     /* Invoice Number CH(8)       */~
                part$,                   /* Part Number CH(25)         */~
                partcat$,                /* Part Category CH(4)        */~
                partclass$,              /* Part Class CH(4)           */~
                partgen$,                /* Part Generic code CH(16)   */~
                parttype$,               /* Part Type CH(3)            */~
                uom$,                    /* Part UOM CH(4)             */~
                store$,                  /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                job$,                    /* Job number CH(8)           */~
                wc$,                     /* Work Center CH(4)          */~
                activity$,               /* Activity code CH(4)        */~
                emp$,                    /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                earntype$,               /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                lclass$,                 /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L51530: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Currency units per book    */~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice  CH(16)     */~
                CH(8),                   /* Check Payment Number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                CH(191)                  /* Filler                     */

        REM THISPROGRAMWASGENERATEDBYGENMENUAPROPRIETARYPRODUCTOFCCAELUS*~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENMENUGENMENUG

        exit_program
            call "FILEBGON" (#50)
            call "MESSAGE" addr("DE", port$, return%)
            end /* Later on Doogan */
