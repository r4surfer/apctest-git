        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP    AAA   Y   Y  U   U  PPPP   DDDD   TTTTT  EEEEE   *~
            *  P   P  A   A  Y   Y  U   U  P   P  D   D    T    E       *~
            *  PPPP   AAAAA   YYY   U   U  PPPP   D   D    T    EEEE    *~
            *  P      A   A    Y    U   U  P      D   D    T    E       *~
            *  P      A   A    Y     UUU   P      DDDD     T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYUPDTE - Vendor Invoice Updating.                       *~
            *   Update program for Vendor Invoice Management and        *~
            *   Adjustment Entry programs.  Performs all updating       *~
            *   functions except G/L postings (which are written to a   *~
            *   transaction file for later posting &/or printing).      *~
            *                                                           *~
            *   This program performs netted inventory postings in      *~
            *   an attempt to keep LIFO/FIFO pools as straight as       *~
            *   possible.  GL updating is improved by allowing multiple *~
            *   detail levels of printing and posting, including 'SUPER *~
            *   DETAIL'. Interim Liability postings have also been      *~
            *   fully implemented in this version.                      *~
            *                                                           *~
            *   Note that the 'expense account' (offset to AP) is       *~
            *   on the invoice line for invoices not linked to a        *~
            *   receiver.  For invoices that are linked to a receiver,  *~
            *   this (or these) account is found in the receiver        *~
            *   distribution file.  The invoice line acct is an offset  *~
            *   to the interim liability account in this case.          *~
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
            * 05/13/86 ! Original                                 ! ERN *~
            * 05/13/86 ! PAYLINES, MASTR, BUFFERS format change   ! HES *~
            *          ! Receiver Update Logic                    !     *~
            * 11/20/86 ! Fixed Job post logic for RCV linked invcs! HES *~
            * 03/09/87 ! Added Serial Numbers (via PAYTOSS).      ! ERN *~
            * 05/19/87 ! PAYBUF2, PAYLINES, RCVLINES, HNYPOOL,    ! JIM *~
            *          !   HNYMASTR, RCVHNYDS mods for Std cost.  !     *~
            *          !   VBKLINES removed (job comes fr RCVLINES)     *~
            * 07/29/87 ! Corrected call to JPURPOST               ! HES *~
            * 10/22/87 ! Corrected GET at 23110 on #6             ! HES *~
            * 11/11/87 ! Corrected Price/Cost Variance Logic      ! HES *~
            * 12/30/87 ! Happy New Year! Multi-currency mods.     ! JIM *~
            * 05/19/89 ! Remove GLTEXT$ overlay in PUT_IN_GL_TIF, ! MLJ *~
            *          ! Corrected Price/Cost rounding            !     *~
            * 12/28/89 ! Add Arrays and coding to summarize G/L   ! JEF *~
            *          ! entries for part of project #7890207.    !     *~
            * 10/29/90 ! Merge Accountancy Phase 1 & G/L Exoprt.  ! JDH *~
            * 05/22/91 ! GL Export code dependent on Export flag. ! JBK *~
            * 06/19/91 ! PRR 11936.  Ensure that Ref 1 has at     ! JDH *~
            *          !   least Vendor/Invoice in it.            !     *~
            * 04/01/92 ! PRR 12326.  Chg'd logic & processing seq ! MLJ *~
            *          !   of call to JNLINFO (9380 - 9630).      !     *~
            * 02/09/93 ! PRR 12519, 12590, 12719 - Program writes ! MLJ *~
            *          !   new invoice total to each PAYJRNTF     !     *~
            *          !   record (pos 165-172).  Also corrected  !     *~
            *          !   numerous implied integers.             !     *~
            * 04/05/93 ! Added Core Value Coding, Channel #17 for ! JBK *~
            *          !   JBMASTRC file.                         !     *~
            * 09/22/93 ! Purchase Jobs - Record Size (RCVHNYDS)   ! KAB *~
            *          !   Treat Purchase Jobs like N/Stock Part  !     *~
            * 01/14/94 ! Added to GLCMBSUB argument list.         ! JDH *~
            * 06/28/94 ! Purchasing Contracts- Added updte to file! ERN *~
            *          !  VPCXREF (in PAYTOSS)                    !     *~
            * 04/25/95 ! PRR11945,12026,12664. Inv # to HNYPROCU. ! JDH *~
            * 07/18/97 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim acct$(13)9,        /* Accounts numbers for HNYAPST sub     */~
            adjaccount$9,      /* System suspense account number       */~
            adjacct$9,         /* Adjusting account number             */~
            apdate$8,          /* A/P Module date                      */~
            aplineseq$3,       /* X-ref from PAYBUF2 to PAYLINES       */~
            apst(13),          /* Arguments for HNYAPST subroutine     */~
            blankdate$8,       /* Blank Date for Comparison            */~
            buf2key$30,        /* Plowkey for PAYBUF2                  */~
            buf2seqnr$3,       /* Line sequence number for PAYBUF2     */~
            bufferkey$30,      /* Plowkey for PAYBUFFR                 */~
            core_inv_flag$1,   /* Core Flags on for Inv type trans     */~
            cost$96,           /* G.P. Cost String                     */~
            curr$4,            /* Is multi-currency in effect?         */~
            currkey$28,        /* Currency-specific file key           */~
            curr_recd$100,     /* Currency-specific record hold area   */~
            currency$4,        /* Currency of transaction              */~
            date$6,            /* System date                          */~
            glacct$9,          /* General purpose G/L account number   */~
            gltext$100,        /* General purpose G/L detail text      */~
            gl_array$(50)109,  /* G/L detail text array for summary    */~
            gl_drcr(50,2),     /* G/L debit & credit array for summay  */~
            gl_type$(50)2,     /* Transaction type for summary         */~
            hnytext$50,        /* Inventory posting text               */~
            invcost(12),       /* Array for HNYPST2                    */~
            invoice$16,        /* Vendor Invoice Number                */~
            jnlid$3,           /* JOURNAL ID                           */~
            lot$6,             /* Inventory Lot number                 */~
            moduleno$2,        /* MODULE NUMBER                        */~
            newcost(12),       /* New Inventory Cost                   */~
            newexpacct$9,      /* Asset/Expense Account for new line   */~
            newjob$8,          /* Job/Project Number for new line      */~
            newpayacct$9,      /* Payables Account for new line        */~
            newpcvacct$9,      /* Price/Cost Variance Account          */~
            newpartnr$25,      /* NEW PART NUMBER                      */~
            newrec$(10)80,     /* Work Variable                        */~
            newstore$3,        /* NEW STORE NUMBER                     */~
            newlot$6,          /* NEW LOT CODE                         */~
            oldcost(12),       /* Old Inventory Cost                   */~
            oldexpacct$9,      /* Asset/Expense Account for old line   */~
            oldjob$8,          /* Job/Project Number for old line      */~
            oldpcvacct$9,      /* Price/Cost Variance Account          */~
            oldpartnr$25,      /* Part Number for old line             */~
            oldrec$(10)80,     /* Work Variable                        */~
            oldstore$3,        /* STORE/ WAREHOUSE CODE                */~
            oldlot$6,          /* LOT CODE                             */~
            partnr$25,         /* Part Number                          */~
            paylines_key$28,   /* PAYLINES KEY FOR DELETED LINES       */~
            pipext(12),        /* PIP extended costs                   */~
            pipacct$9,         /* PIP G/L account number               */~
            po$16,             /* Purchase Order Number                */~
            poline$3,          /* Purchase Order Line Number           */~
            procsrce$26,       /* Source of Procurement                */~
            readkey$90,        /* Misc. purpose read/plowkey           */~
            rcv$16,            /* Receiver Number                      */~
            stat$4,            /* Statutory currency                   */~
            store$3,           /* Store/ Warehouse code                */~
            summary$1,         /* SUMMARY INDICATOR                    */~
            titlj$40,          /* JOURNAL TITLE                        */~
            update$3,          /* Update this buffer? (YES/NO).        */~
            userid$3,          /* The guy really responbsible          */~
            vpcxref_key$48,    /* Delete key                           */~
            vencode$9,         /* Vendor who sent the Invoice          */~
            wipacct$9          /* WIP G/L account number               */

        dim                    /* G/L Export Posting Information       */~
            export_on$1,       /* G/L Export File processing?          */~
            gl_job$8,          /* Transaction type for summary         */~
            gl_post_info$(2)255,/* G/L export posting information      */~
            partcat$4,         /* Part category code                   */~
            partclass$4,       /* Part Class code                      */~
            partgen$16,        /* Part Generic code                    */~
            parttype$3,        /* Part Type code                       */~
            tran_type$5,       /* G/L transaction type                 */~
            uom$4,             /* Part Unit of measure                 */~
            ventype$4          /* Vendor type code                     */

        dim                              /* Core Value Information     */~
            adj_newcost(12),             /* Ajusted Inv Cost Bckt (NEW)*/~
            adj_oldcost(12),             /* Ajusted Inv Cost Bckt (OLD)*/~
            core_gl_array$(50)109,       /* GL Core Text Entry Array   */~
            core_gl_drcr(50,2),          /* GL Debit & Credits (Core)  */~
            core_gl_type$(50)2,          /* GL Trans Types (Core)      */~
            newcorepart$25,              /* Core Part Number (NEW)     */~
            new_cfg_acct$9,              /* Core Finshd Goods Acct(NEW)*/~
            oldcorepart$25,              /* Core Part Number (OLD)     */~
            old_cfg_acct$9,              /* Core Finshd Goods Acct(OLD)*/~
            save_gltext$100,             /* Save Current GLTEXT$ Value */~
            save_part$25                 /* Save Current Part Number   */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! USERINFO ! Users Default Information File           *~
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! PAYBUFFR ! Payables buffer header file              *~
            * #4  ! PAYBUF2  ! Payables line item buffer file           *~
            * #5  ! PAYMASTR ! Payables main file                       *~
            * #6  ! PAYLINES ! Payables line item file                  *~
            * #7  ! RCVLINES ! Receiver Line Items                      *~
            * #8  ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #9  ! HNYPOOL  ! Inventory LIFO/FIFO pool records         *~
            * #10 ! HNYMASTR ! Inventory Master File                    *~
            * #11 ! HNYDETAL ! Inventory detail file                    *~
            * #12 ! HNYPROC  ! Inventory where procurred from detail fi *~
            * #13 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #14 ! PIPOUT   ! Planned inventory use detail records     *~
            * #15 ! GLMAIN   ! General Ledger Main File                 *~
            * #16 ! GLDETAIL ! General ledger detail file               *~
            * #17 ! JBMASTRC ! Production job Core Value Added File     *~
            * #18 ! JOBMASTR ! WIP/JC Job Master File                   *~
            * #19 ! JBMASTR2 ! Production job master file               *~
            * #20 ! JBVALUE2 ! Production job value added detail file   *~
            * #21 ! JOBPURCH ! Job purchases detail file                *~
            * #22 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #23 ! PAYJRNTF ! Payables G/L Journal Transaction File    *~
            * #24 ! PAYHNYRF ! Payable's Inventory Trans Report File    *~
            * #25 ! VENDOR   ! Vendor MAster File                       *~
            * #26 ! CSHLINES ! Cash Disbursements File                  *~
            * #27 ! WORKFILE ! INVENTORY ADJUSTMENT WORK FILE           *~
            * #29 ! CSHMASTR ! 1099 POSTING NEEDS IT                    *~
            * #30 ! CSH1099  ! 1099 DETAIL FILE                         *~
            * #31 ! RCVHNYDS ! Receiver Inventory Distribution          *~
            * #33 ! SERTIF   ! Serial Numbers- TIF                      *~
            * #34 ! SERMASTR !               - Master                   *~
            * #35 ! SERHOLD  !               - Restart File             *~
            * #37 ! VPCXREF  ! Purchasing Contracts X-Ref File          *~
            * #41 ! PAYLNCUR ! Currency-specific PAYLINES line items    *~
            * #42 ! PAYLNBF2 ! Currency-specific PAYBUF2 line items     *~
            *************************************************************

            select  #1, "USERINFO",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 3

            select  #2, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select  #3, "PAYBUFFR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 350,                                  ~
                         keypos = 1, keylen = 10,                        ~
                         alternate key 1, keypos = 11, keylen = 25

            select  #4, "PAYBUF2",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 541,                                  ~
                         keypos = 36, keylen = 28,                       ~
                         alternate key 1, keypos = 1, keylen = 63,       ~
                                   key 2, keypos = 17, keylen = 47

            select  #5, "PAYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 350,                                  ~
                         keypos = 1, keylen = 25

            select  #6, "PAYLINES",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 541,                                  ~
                         keypos = 36, keylen = 28,                       ~
                         alternate key 1, keypos = 1, keylen = 63,       ~
                                  key 2, keypos = 17, keylen = 47

            select #7,  "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24

            select  #8, "HNYQUAN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 650,                                  ~
                         keypos= 17, keylen = 44,                        ~
                         alternate key 1, keypos =  1, keylen = 44

            select  #9, "HNYPOOL",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 38

            select #10, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  900,                                 ~
                         keypos =    1, keylen =  25,                    ~
                         alt key  1, keypos =  102, keylen =   9, dup,   ~
                             key  2, keypos =   90, keylen =   4, dup

            select #11, "HNYDETAL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 43, keylen = 6, dup,  ~
                                   key 2, keypos = 49, keylen = 2, dup

            select #12, "HNYPROC",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  134,                                 ~
                         keypos =   32, keylen =  40,                    ~
                         alt key  1, keypos =    7, keylen =  65,        ~
                             key  2, keypos =    1, keylen =  40, dup,   ~
                             key  3, keypos =   41, keylen =  31, dup    ~

            select #13, "PIPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #14, "PIPOUT",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 64,                                   ~
                         keypos = 1, keylen = 56,                        ~
                         alt key 1, keypos = 20, keylen = 37

            select #15, "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  300,                                 ~
                         keypos =    1, keylen =   9                     ~

            select #16, "GLDETAIL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  160,                                 ~
                         keypos =    1, keylen =  26                     ~

            select #17, "JBMASTRC",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  600,                                 ~
                         keypos = 1, keylen = 8

            select #18, "JOBMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  700,                                 ~
                         keypos = 1, keylen = 8

            select #19, "JBMASTR2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  1300,                                ~
                         keypos = 1, keylen = 8

            select #20, "JBVALUE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 23

            select #21, "JOBPURCH",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 16

            select #22, "SFCUM2",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 1985,                                 ~
                         keypos = 1, keylen = 25

            select #23, "PAYJRNTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 688,                                  ~
                         keypos =  1, keylen = 36

            select #24, "PAYHNYRF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos =  1, keylen = 61

            select #25, "VENDOR",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 600,                                  ~
                         keypos = 1, keylen = 9,                         ~
                         alternate key 1, keypos = 10, keylen = 30, dup

            select #26, "CSHLINES",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos = 1, keylen = 20,                        ~
                         alternate key 1, keypos = 21, keylen = 16, dup

            select #27,  "WORKFILE",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 10, keylen = 19                        ~

            select #29, "CSHMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 17,                         ~
                        alternate key 1, keypos = 41, keylen = 9, dup,   ~
                                         /* CASH IN BANK ACCOUNT       */~
                                  key 2, keypos = 50, keylen = 6, dup
                                         /* DATE POSTED                */

            select #30, "CSH1099",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos = 7, keylen = 20,                         ~
                        alt key 1, keypos =  1, keylen = 26,             ~
                            key 2, keypos = 27, keylen = 32

            select #31, "RCVHNYDS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 86,                          ~
                        alt key 1, keypos = 45, keylen = 42

            select #33, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #34, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #35, "SERHOLD",                                       ~
                        varc,     indexed,  recsize =  333,              ~
                        keypos = 1, keylen = 33

            select #37, "VPCXREF",                                       ~
                        varc,     indexed,  recsize = 133,               ~
                        keypos = 1, keylen = 49,                         ~
                        alternate key 1, keypos = 21, keylen = 49

            select #41,  "PAYLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 28,                      ~
                        alt key  1, keypos =   1, keylen =  32

            select #42, "PAYLNBF2",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos =  5, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 32

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  0%, f2%(1%),    0%, " ")
            call "OPENCHCK" (#2,  0%, f2%(2%),    0%, " ")
            call "OPENCHCK" (#3,  0%, f2%(3%),    0%, " ")
            call "OPENCHCK" (#4,  0%, f2%(4%),    0%, " ")
            call "OPENCHCK" (#5,  0%, f2%(5%),  100%, " ")
            call "OPENCHCK" (#6,  0%, f2%(6%),  200%, " ")
            call "OPENCHCK" (#7,  0%, f2%(7%),    0%, " ")
            call "OPENCHCK" (#8,  0%, f2%(8%),    0%, " ")
            call "OPENCHCK" (#9,  0%, f2%(9%),    0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10%),   0%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11%),   0%, " ")
            call "OPENCHCK" (#12, 0%, f2%(12%),   0%, " ")
            call "OPENCHCK" (#13, 0%, f2%(13%),   0%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14%), 100%, " ")
            call "OPENCHCK" (#15, 0%, f2%(15%),   0%, " ")
            call "OPENCHCK" (#16, 0%, f2%(16%),   0%, " ")
            call "OPENCHCK" (#18, 0%, f2%(18%),   0%, " ")
            call "OPENCHCK" (#19, 0%, f2%(19%),   0%, " ")
            call "OPENCHCK" (#20, 0%, f2%(20%),   0%, " ")
            call "OPENCHCK" (#21, 0%, f2%(21%),   0%, " ")
            call "OPENCHCK" (#22, 0%, f2%(22%),   0%, " ")
            call "OPENCHCK" (#23, 0%, f2%(23%), 250%, " ")
            call "OPENCHCK" (#24, 0%, f2%(24%), 200%, " ")
            call "OPENCHCK" (#25, 0%, f2%(25%),   0%, " ")
            call "OPENCHCK" (#26, 0%, f2%(26%),   0%, " ")
            call "WORKOPEN" (#27, "IO   ", 100%, f2%(27%))
            call "OPENCHCK" (#29, 0%, f2%(29%),   0%, " ")
            call "OPENCHCK" (#30, 0%, f2%(30%),   0%, " ")
            call "OPENCHCK" (#31, 0%, f2%(31%),   0%, " ")
            call "OPENCHCK" (#33, 0%, f2%(33%),   0%, " ")
            call "OPENCHCK" (#34, 0%, f2%(34%),   0%, " ")
            call "OPENCHCK" (#35, 0%, f2%(35%),   0%, " ")
            call "OPENCHCK" (#37, 0%, f2%(37%), 100%, " ")

*        Check for Multi-Currency
            curr$ = "N"
            call "READ100" (#02, "SWITCHS.CUR", f1%(2%))
            if f1%(2%) <> 0% then get #02 using L05120, curr$, stat$
L05120:         FMT POS(21), CH(1), CH(4)
            if curr$ <> "Y" then stat$ = " "
            if curr$ <> "Y" then goto L05180
                call "OPENCHCK" (#41, 0%, f2%(41%), 200%, " ")
                call "OPENCHCK" (#42, 0%, f2%(42%),   0%, " ")

L05180
*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#2, "SWITCHS.GL", f1%(2%))
            if f1%(2%) = 1% then get #2 using L05220, export_on$
L05220:         FMT POS(22), CH(1)

*        Check for Core
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#2, plowkey$, core_on%)
                if core_on% <> 1% then L09000
            get #2 using L05290, core_inv_flag$
L05290:         FMT POS(134), CH(1)
                if core_inv_flag$ = "Y" then L09000
            core_on% = 0%

L09000: REM *************************************************************~
            *        I N I T I A L I Z A T I O N   S E C T I O N        *~
            *-----------------------------------------------------------*~
            * Set-ups, tests, and variable initialization               *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

*        Get current user ID
*        Also, we'll set up some misc. variables.

            call "EXTRACT" addr("ID", userid$)
            date$ = date

            call "READ100" (#1, userid$, f1%(1%))
            if f1%(1%) = 0% then abend_userid

*        Verify the User's Accounts Payable posting date is within the
*        3 months open for G/L posting.

            get #1, using L09180, apdate$
L09180:         FMT XX(9), CH(6)
            call "WHICHMON" (#2, apdate$, glmonth%)
            if glmonth% = 0% then abend_month


*        Calculate TODAY's position on the planning Calendar
*
*          CALL "PIPINDEX" (#2, " ", TODAY%, RET%)
*
*        Find out if we are going to update this buffer ('YES') -or- if
*        we are just to create the journal transaction file ('NO ').

            call "GETPARM" addr ("I ", "R", "UPDATE  ", " ", "0001",     ~
                 "PAYUPD", "Update Inventory with Buffer?   ", 32%, "K", ~
                 "UPDATE  ", update$, 3%, 5%, 32%, "A")

L09364:      REM DO A GETPARM TO FIND JNLID$
                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "PAYUPD",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09364

            return% = 0%
            moduleno$ = "02"

            readkey$ = str(userid$) & hex(000000)
            call "READ100" (#23, readkey$, f1%(23%))
                if f1%(23%) = 0% then L09480          /* New Batch       */
            get #23 using L09450, pstseq%             /* Existing Batch  */
L09450:         FMT POS(40), BI(4)
            goto L09540

L09480:     call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,        ~
                           titlj$, apdate$, #2, f2%(2), return%)
            write #23 using L09520, userid$, hex(000000), " ", jnlid$,     ~
                                  pstseq%, " "
L09520:         FMT CH(3), CH(3), CH(30), CH(3), BI(4), CH(645)

L09540:     call "READ100" (#2, "FISCAL DATES", f1%(2%))
                if f1%(2%) = 0% then L09590
            get #2 using L09570, adjaccount$
L09570:         FMT POS(417), CH(16)

L09590:     if update$ = "YES" then                                      ~
                call "SHOSTAT" ("Updating System with Vendor Invoices")  ~
                    else                                                 ~
                call "SHOSTAT" ("Creating G/L Transaction File")

        REM *************************************************************~
            *        U P D A T E     U P D A T E     U P D A T E        *~
            *-----------------------------------------------------------*~
            *  Now it's time to start getting serious.  The update      *~
            *  performs somewhat along the following lines--            *~
            *                                                           *~
            *  The major control loop is the plow thru PAYBUFFR. Here,  *~
            *  all journal transactions are written (except for direct  *~
            *  inventory adjustments) and all major decisions are made. *~
            *                                                           *~
            *  This section looks for changed lines, posting only the   *~
            *  net changes between PAYBUF2 and PAYLINES.  When the line *~
            *  has been updated, the record in PAYLINES is deleted.     *~
            *                                                           *~
            *  The second line loop is thru the lines left in PAYLINES. *~
            *  These represent lines that have been deleted from the    *~
            *  invoice.  The 'NEW' line is dummied to pass thru the     *~
            *  line updating section with a minimum amount of trouble.  *~
            *************************************************************

*        For the first time only, setup the Plowkey for PAYBUFFR.
            init (hex(00)) bufferkey$
            str  (bufferkey$,,3%) = userid$

        header_loop  /* Plow thru PAYBUFFR to find out what to work on */
*       ---------------------------------------------------------------*
            call "PLOWNEXT" (#3, bufferkey$, 3%, f1%(3%))
                if f1%(3%) = 0% then normal_end
            get #3, using L10150, vencode$, invoice$, newpayacct$,        ~
                                 newinvamt
L10150:     FMT XX(10), CH(9), CH(16), XX(31), CH(9), XX(45), PD(14,4)

            vpcxref_key$ = "I" & str(vencode$,,9) & str(invoice$,,16) &  ~
                                                                  hex(00)
        vpcxref_delete_loop
            call "PLOWAL1" (#37, vpcxref_key$, 1%, 26%, f1%(37%))
            if f1%(37%) = 0% then L10165
                delete #37
                goto vpcxref_delete_loop

L10165:     if export_on$ <> "Y" then L10195
                gosub get_currency_info
                call "READ100" (#25, vencode$, f1%(25%))
                if f1%(25%) = 1% then get #25 using L10185, ventype$
L10185:              FMT POS(477), CH(4)

L10195:     REM Get old version of header from PAYMASTR (if it exists)
            readkey$ = str(vencode$) & str(invoice$)
            oldpayacct$ = " " : oldinvamt, adjqty = 0
            call "READ100" (#5, readkey$, f1%(5%))
                if f1%(5%) = 0% then L10315
            get #5, using L10225, oldpayacct$, oldinvamt
L10225:         FMT XX(56), CH(9), XX(45), PD(14,4)
            newpartnr$, parttype$, partcat$, partclass$, partgen$,       ~
                                                               uom$ = " "

            REM Reversal required?
            if oldinvamt   <> newinvamt   then L10270
            if oldpayacct$ <> newpayacct$ then L10270
            goto L10355

L10270:     REM DR (reverse) previous Payables Acct with OLD Invoice Amt
                gltext$ = " "
                str(gltext$,31%,38%) = str(vencode$,,9%) & str(invoice$)
                str(gltext$,65%,4%) = "REV"
                str(gltext$,69%,32%) = "PAYABLES:   VENDOR INVOICE GROSS"
                balflag% = 1% : glamt = oldinvamt : glacct$ = oldpayacct$
                tran_type$ = "VXJ01"
                gosub put_in_gl_tif

L10315:     REM CR Payables Account with NEW Invoice Amount
                gltext$ = " "
                str(gltext$,31%,38%) = str(vencode$,,9%) & str(invoice$)
                str(gltext$,69%,32%) = "PAYABLES:   VENDOR INVOICE GROSS"
                balflag% = 2% : glamt = newinvamt : glacct$ = newpayacct$
                tran_type$ = "VXJ01"
                gosub put_in_gl_tif

L10355:     REM Loop thru PAYBUF2 to pick up the new version of each     ~
                invoice line.  We'll call these variables 'NEW'.
            init(hex(00)) buf2key$
            str(buf2key$,,25%) = str(vencode$,,9%) & str(invoice$,,16%)

        buffer_line_loop
            oldexpacct$, oldpartnr$, oldjob$, oldstore$, oldlot$, rcv$,  ~
            po$, poline$, oldpcvacct$ = " "
            oldext, oldqty, oldprice, oldcost, adjqty = 0
            mat oldcost = zer
            mat gl_drcr = zer : init (" ") gl_array$() : entry% = 1%
            mat core_gl_drcr = zer  :  init (" ")  core_gl_array$()
            core_entry%    = 1%
            newoh%, oldoh% = 0%

                call "PLOWNEXT" (#4, buf2key$, 25%, f1%(4%))
                     if f1%(4%) <> 1% then deleted_line_loop
                buf2seqnr$ = str(buf2key$,26%,3%)

                get #4 using L10470, rcv$, po$, poline$, newexpacct$,     ~
                             newpartnr$, newqty, newext, newjob$,        ~
                             newstore$, newlot$, newprice, aplineseq$,   ~
                             newcost, newcost(), newpcvacct$
L10470:         FMT CH(16), CH(16), CH(3), XX(28), CH(9), CH(25),        ~
                    2*PD(14,4), CH(8), CH(3), CH(6), PD(14,7), CH(3),    ~
                    POS(183), PD(14,4), 12*PD(14,4), CH(9),              ~
                    POS(296), CH(20)
                gosub check_for_core_newcost
                gl_job$ = newjob$
                newinvprice = round(newqty * newprice, 2%)
                newpcvar = round(newqty * pc_var_newcost, 2%) -          ~
                                                              newinvprice
                if newjob$ <> " " then                                   ~
                   call "READ100" (#10, newpartnr$, newoh%)

*          Now we go find an 'old' version of this line item - if any.
*          If not, we'll dummy up the old line so that the update thinks
*          it's there.

*         Look for the old version of the line in PAYLINES.
            if aplineseq$ = " " then L11130 /* Not in PAYLINES */
                readkey$ = str(vencode$) & str(invoice$) & str(aplineseq$)
                call "READ100" (#6, readkey$, f1%(6%))
                   if f1%(6%) = 0% then L11130  /* OH ?!@#$? */
                get #6 using L10585, oldexpacct$, oldpartnr$, oldqty,     ~
                            oldext, oldjob$, oldstore$, oldlot$,         ~
                            oldprice, oldcost, oldcost(), oldpcvacct$
L10585:         FMT POS(64), CH(9), CH(25), 2*PD(14,4), CH(8), CH(3),    ~
                    CH(6), PD(14,7), POS(183), PD(14,4), 12*PD(14,4),    ~
                    CH(9)
                gosub check_for_core_oldcost
                gl_job$ = oldjob$
                oldinvprice = round(oldqty * oldprice, 2%)
                oldpcvar = round(oldqty * pc_var_oldcost, 2%) -          ~
                                                              oldinvprice
                if oldjob$ <> " " then                                   ~
                   call "READ100" (#10, oldpartnr$, oldoh%)

               if oldpartnr$  <>  newpartnr$                   then L11100
               if oldstore$   <>  newstore$                    then L11100
               if oldlot$     <>  newlot$                      then L11100
               if oldexpacct$ <>  newexpacct$                  then L11000
               if oldqty      <>  newqty                       then L11000
               if oldcost     <>  newcost                      then L11000
               if oldprice    <>  newprice                     then L11000
                  goto L11340

*             CR Asset/Expense Acct with Old Line extension (reverses)
L11000:     gltext$ = str(vencode$,1%,9%) & str(invoice$)
            str(gltext$,31%,34%)=str(oldpartnr$,,25%)& str(oldstore$,,3%)~
                                                      & str(oldlot$,,6%)
            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%) = "PAYABLES:   INVOICE LINE DIST."
            if po$ = " " then invcost = adj_oldcost else invcost =       ~
                                                                 oldprice
                balflag% = 2% : glacct$ = oldexpacct$
                glamt = round(oldqty * invcost, 2%)
                if oldoh% = 0% then tran_type$ = "VXJ02" else            ~
                                    tran_type$ = "VXJ11"
                gosub put_in_gl_array
                if core_on% = 1% and old_reman% = 1% then                ~
                                              gosub core_reverse_old_line
            if po$ <> " " then L11260
            gltext$ = str(vencode$,1%,9%) & str(invoice$)
            str(gltext$,31%,34%)=str(oldpartnr$,,25%)& str(oldstore$,,3%)~
                                                      & str(oldlot$,,6%)
            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%) = "PAYABLES:   COST/PRICE VAR."
                balflag% = 1% : glacct$ = oldpcvacct$
                glamt = oldpcvar
                tran_type$ = "VXJ03"
                gosub put_in_gl_array

                goto L11260

L11100
*        He has implied a 'move', treat as a delete & reenter
                aplineseq$ = " "

L11130:     REM We didn't find the old line in PAYLINES so we'll just    ~
                make one up.

                oldexpacct$ = newexpacct$
                oldjob$     = newjob$
                oldpartnr$  = newpartnr$
                oldstore$   = newstore$
                oldlot$     = newlot$
                oldpcvacct$ = newpcvacct$
                oldqty      = 0
                oldprice    = newprice
                oldcost     = newcost
                mat oldcost = newcost

                oldcorepart$    = newcorepart$
                old_cfg_acct$   = new_cfg_acct$
                adj_oldcost     = adj_newcost
                oldcorecost     = newcorecost
                pc_var_oldcost  = pc_var_newcost
                mat adj_oldcost = adj_newcost
                old_reman%      = new_reman%
                oldpurjob%      = newpurjob%

*         Here things come together again.
*              DR Asset/Expense Acct with New line extension
L11260:     gltext$ = str(vencode$,,9%) & str(invoice$)
            str(gltext$,31%,34%)=str(newpartnr$,,25%)& str(newstore$,,3%)~
                                                     & str(newlot$,,6%)
            str(gltext$,69,32) = "PAYABLES:   INVOICE LINE DIST."
            if po$ <>" "then str(gltext$,69%,32%)="PAYABLES:   PO#" & po$
            if po$ = " " then invcost = adj_newcost else invcost =       ~
                                                                 newprice
                balflag% = 1% : glacct$ = newexpacct$
                glamt = round(newqty * invcost, 2%)
                if newoh% = 0% then tran_type$ = "VXJ02" else            ~
                                    tran_type$ = "VXJ11"
                gosub put_in_gl_array
                if core_on% = 1% and new_reman% = 1% then                ~
                                     gosub core_post_new_line_extension
            if po$ <> " " then L11340
            str(gltext$,69%,32%) = "PAYABLES:   COST/PRICE VAR."
                balflag% = 2% : glacct$ = newpcvacct$
                glamt = newpcvar
                tran_type$ = "VXJ03"
                gosub put_in_gl_array

L11340:     if update$ = "YES" and po$ = " " then gosub line_item_update
            gosub receiver_update

*         Delete OLD line from PAYLINES
            if aplineseq$ = " "  then L11410
                readkey$ = str(vencode$) & str(invoice$) & str(aplineseq$)
                call "DELETE" (#6, readkey$, 28%)

L11410:     if entry% > 1% then gosub write_gl_array
            if core_on% = 1% and core_entry% > 1% then                   ~
                                     gosub prepare_to_write_gl_array_core

            goto buffer_line_loop

*        SECOND LINE LOOP.  Here we catch any left over PAYLINES.  These
*        guys represent lines that have been deleted from the invoice.
*        Since no new line exists, we have to dummy up the 'new' stuff.
*        Note that we are not deleting in the traditional sense if a
*        RCVLINE exists.  If that is the case, we 'revert' to where
*        everything was at the time the PO line was moved to the invoice.
*       ----------------------------------------------------------------*

        deleted_line_loop

            init (hex(00)) readkey$, buf2seqnr$
            str(readkey$,,25%) = str(vencode$,,9%) & str(invoice$,,16%)
                /* Note: Always take from the top  */

            init (" ") newexpacct$, partnr$, newjob$, store$, lot$
                       newext, newqty, newprice = 0

            call "PLOWNEXT" (#6, readkey$, 25%, f1%(6%))
            if f1%(6%) = 0% then finish_off_invoice

            get #6 using L12230, rcv$, po$, poline$, paylines_key$,       ~
                                oldexpacct$, oldpartnr$, oldqty, oldext, ~
                                oldjob$, oldstore$, oldlot$, oldprice,   ~
                                oldcost, oldcost(), oldpcvacct$
L12230:         FMT CH(16), CH(16), CH(3), CH(28), CH(9), CH(25),        ~
                    2*PD(14,4), CH(8), CH(3), CH(6), PD(14,7), POS(183), ~
                    PD(14,4), 12*PD(14,4), CH(9)
            gosub check_for_core_oldcost
            gl_job$ = oldjob$
            oldinvprice = round(oldqty * oldprice, 2%)
            oldpcvar = round(oldqty * oldcost, 2%) - oldinvprice

            newexpacct$ = oldexpacct$
            newpcvacct$ = oldpcvacct$
            newjob$     = oldjob$
            newpartnr$  = oldpartnr$
            newstore$   = oldstore$
            newlot$     = oldlot$
            newqty      = 0
            newprice    = oldprice
            newcost     = oldcost
            mat newcost = oldcost

            newcorepart$    = oldcorepart$
            new_cfg_acct$   = old_cfg_acct$
            adj_newcost     = adj_oldcost
            newcorecost     = oldcorecost
            pc_var_newcost  = pc_var_oldcost
            mat adj_newcost = adj_oldcost
            new_reman%      = old_reman%
            newpurjob%      = oldpurjob%

*              CR (Reverse) Asset/Expense Acct with New line extension
            gltext$ = str(vencode$,,9%) & str(invoice$)
            str(gltext$,31%,34%)=str(oldpartnr$,,25%)& str(oldstore$,,3%)~
                                                      & str(oldlot$,,6%)
            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%) = "PAYABLES:   INVOICE LINE DIST."
            if po$ <>" "then str(gltext$,69%,32%)="PAYABLES:   PO#" & po$
            if po$ = " " then invcost = adj_oldcost else invcost =       ~
                                                                 oldprice
                balflag% = 2% : glacct$ = oldexpacct$
                glamt = round(oldqty * invcost, 2%)
                if oldoh% = 0% then tran_type$ = "VXJ02" else            ~
                                    tran_type$ = "VXJ11"
                gosub put_in_gl_tif
                if core_on% = 1% and old_reman% = 1% then                ~
                                       gosub core_reverse_old_line_delete
            if po$ <> " " then L12485
            gltext$ = str(vencode$,,9%) & str(invoice$)
            str(gltext$,31%,34%)=str(oldpartnr$,,25%)& str(oldstore$,,3%)~
                                                      & str(oldlot$,,6%)
            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%)   = "PAYABLES:   INVOICE LINE DIST."
                balflag% = 1% : glacct$ = oldpcvacct$
                glamt = oldpcvar
                tran_type$ = "VXJ03"
                gosub put_in_gl_tif

L12485:     if update$ = "YES" and po$ = " " then gosub line_item_update
            gosub receiver_update
            call "DELETE" (#6, paylines_key$, 28%)
            goto deleted_line_loop


*        Everything has been finished as far as the lines are concerned.
*        All that is left is to update some balances and to toss the
*        Buffer files into the Master files.
*       --------------------------------------------------------------*

        finish_off_invoice
            gosub currency_lines_toss
            call "PAYTOSS" (apdate$, glmonth%,                           ~
                            #5, #6, #3, #4, #25, #26, #29, #30,          ~
                            #33, #34, #35, #37)
            gosub currency_lines_delete
            goto header_loop   /* Starting all over again....          */

        REM Subroutine to toss line item data from PAYLNBF2 to PAYLNCUR
        currency_lines_toss
            if curr$ <> "Y" then return
            currkey$ = str(vencode$) & str(invoice$) & hex(00)
            call "DELETE" (#41, currkey$, 25%)  /* PAYLNCUR */
L14160:     call "PLOWNEXT" (#42, currkey$, 25%, f1%(42%))
            if f1%(42%) = 0% then return
                get #42, curr_recd$    /* PAYLNBF2 */
                write #41, curr_recd$  /* PAYLNCUR */
                goto L14160

        REM Subroutine to delete line item data from PAYLNBF2
        currency_lines_delete
            if curr$ <> "Y" then return
            currkey$ = str(vencode$) & str(invoice$) & hex(00)
            call "DELETE" (#42, currkey$, 25%)  /* PAYLNBF2 */
            return

        REM Get currency and exchange rate for invoice
        get_currency_info
            currency$ = stat$
            convunt = 1
            currkey$ = str(vencode$) & str(invoice$) & hex(00)
            call "PLOWNEXT" (#42, currkey$, 25%, f1%(42%))
            if f1%(42%) = 0% then return
                get #42 using L14284, currency$, convunt
L14284:              FMT CH(4), POS(63), PD(14,7)
                return

        REM ***GENERAL LEDGER TRANSACTION FILE SUBROUTINE***************

        put_in_gl_tif      /* Write to G/L transaction file            */
                           /* BALFLAG% -  1= DEBIT,   2 = CREDIT       */
            if glamt = 0 then return
            if str(gltext$,,30%) = " " then str(gltext$,,30%) =          ~
                                        str(vencode$,,9%) & str(invoice$)
            pstqty = adjqty
            if str(tran_type$,4%,2%) ="03" or str(tran_type$,4%,2%) ="04"~
               then pstqty = 0
            if balflag% = 1% then amount = glamt else amount = -glamt
            save_part$ = newpartnr$  :  newpartnr$ = str(gltext$,31%,25%)
            if export_on$ = "Y" then gosub load_gl_info
            newpartnr$ = save_part$
            gosub condition_balflag

L14400:     write #23 using L14430, userid$, vencode$, invoice$, time,    ~
                                   glacct$, balflag%,abs(glamt),gltext$, ~
                                   apdate$, pstseq%, newinvamt, " ",     ~
                                   gl_post_info$(), eod goto L14400
L14430:         FMT  CH(3), CH(9), CH(16), CH(8), CH(9), BI(1), PD(14,4),~
                     CH(100), CH(6), BI(4), PD(14,4), CH(6), 2*CH(255)
            return

        put_in_gl_array        /* Place G/L data in array for summary */

            if glamt = 0 then return

            gosub condition_balflag

            gl_array$(entry%) = str(glacct$,,9%) & str(gltext$,,100%)
            gl_drcr(entry%, balflag%) = abs(glamt)
            gl_type$(entry%) = str(tran_type$,4%,2%)

            cntr%  = entry%
            entry% = entry% + 1%

            return

        write_gl_array         /* Summarize values and then write them */

            if entry% < 3% then goto L14720

            call "GLCMBSUB" (gl_array$(), gl_drcr(), gl_type$(), #02,    ~
                                                                   cntr%)

L14720:     for entry% = 1% to 50%

            if gl_drcr(entry%, 1%) = 0 and gl_drcr(entry%, 2%) = 0       ~
                     and gl_array$(entry%) = " " then goto L14795
            if gl_drcr(entry%, 1%) = 0 and gl_drcr(entry%, 2%) = 0       ~
                then goto L14790

            glacct$ = str(gl_array$(entry%),1%,9%)
            gltext$ = str(gl_array$(entry%),10%,100%)
            str(tran_type$,4%,2%) = gl_type$(entry%)
            if gl_drcr(entry%, 1%) = 0 then goto L14775
                balflag% = 1% : glamt = gl_drcr(entry%, 1%)
                gosub put_in_gl_tif

L14775:     if gl_drcr(entry%, 2%) = 0 then goto L14790
                balflag% = 2% : glamt = gl_drcr(entry%, 2%)
                gosub put_in_gl_tif

L14790:     next entry%

L14795:     return

        condition_balflag      /* BALFLAG% -  1= DEBIT,   2 = CREDIT  */
            if glacct$ = " " then glacct$ = adjaccount$
            glamt = round(glamt, 2%)
            if balflag% = 1% and glamt < 0 then L14860
            if balflag% = 2% and glamt < 0 then balflag% = 1%
            goto L14870
L14860:     balflag% = 2%
L14870:     return

*       ***********  END OF MAIN DRIVER CODE SECTION  ******************

        REM *************************************************************~
            *         L I N E   I T E M   U P D A T I N G               *~
            * GROSS G/L POSTING HAS BEEN HANDLED ABOVE, NOW FINE TUNE IT*~
            * This section of code handles all line specific updating   *~
            * except for G/L which was handled above.                   *~
            * note that because of the 'rigging' done above we only     *~
            * work on one part/store/lot at a time.                     *~
            *************************************************************

        line_item_update
            partnr$ = newpartnr$
            store$  = newstore$
            lot$    = newlot$

            adjqty = round(newqty - oldqty, 4%)
               if abs(adjqty) < .0001 then L15800 /* AT MOST, COST ADJ */

               if adjqty < 0 then L15500  /* OVER RECEIPT */

        REM  HERE WE ARE, ADDING TO THE RECEIPT
             mat apst = adj_newcost - adj_oldcost
                call "PACKZERO" (apst(), cost$)
                  if pos(cost$ > hex(00)) = 0% then L15280
                  apst_qty = oldqty
                  adjacct$ = newexpacct$
                     gosub call_hnyapst

L15280:        adjcost, invcost, hnycost = adj_newcost
               mat invcost = adj_newcost
               adjext = round(adj_newcost * adjqty, 2%)
                 gosub call_hnypost

               goto L18000

L15500: REM  here we are, subtracting from the receipt

               adjacct$ = oldexpacct$
               adjcost, hnycost = adj_oldcost
               invcost = 0
               mat invcost = zer
               adjext = round(oldprice * adjqty, 2%)
                 gosub call_hnypost


        REM now we'll fall thru to adjust what's left, if any

L15800: REM COST ADJUSTMENT
            mat apst = adj_newcost - adj_oldcost
               call "PACKZERO" (apst(), cost$)
            if pos(cost$ > hex(00)) = 0% then L18000
               apst_qty = newqty
               adjacct$ = newexpacct$
                  gosub call_hnyapst
            goto L18000

        call_hnypost

            hnytext$ = "AP INV " & invoice$ & " VEN " & vencode$
            gltext$ = " "
            str(gltext$,,30%) = str(vencode$) & str(invoice$)
            str(gltext$,31%,34%) = str(partnr$,,25%) & str(store$,,3%) & ~
                                                            str(lot$,,6%)
            str(gltext$,65%, 4%) = "   "
            str(gltext$,69%,32%) = "PAYABLES:   INVENTORY MOVEMENT"

                call "HNYPST2" (partnr$, store$, lot$, adjqty, 0,0,0,0,  ~
                                invcost(), invcost, hnycost, adjext,     ~
                                apdate$, "VT", hnytext$, " "," ", 3%, 1%,~
                                "02", "   ", pstseq%, gltext$, userid$,  ~
                                #8, #11,#2,#9,#10,#13,#22, #15, #16, #27,~
                                0%, ret%)

                REM Scoot G/L Trans on into PAYJRNTF
L16190:         call "PLOWNXT1" (#27, hex(0000), 0%, f1%(27%))
                     if f1%(27%) = 0% then L16290
                get #27, using L16220, glacct$, gltext$, damt, camt
L16220:         FMT XX(25), CH(9), CH(100), 2*PD(14,4)
                tran_type$ = "VXJ11"
                if str(gltext$,67%,1%) = "H" then L16230
                tran_type$ = "VXJ04"
                if str(gltext$,65%,1%) = "S" then L16230
                tran_type$ = "VXJ05"
L16230:         str(gltext$,,30%) = str(vencode$,,9%) & invoice$
                balflag% = 1% : glamt = damt : gosub put_in_gl_array
                balflag% = 2% : glamt = camt : gosub put_in_gl_array
                delete #27
                goto L16190

L16290:     REM If the inventory withdrawn was valued differently than on~
                the invoice, we'll have to square up the accounts.

                if adjqty >= 0 then return
                     apst_qty  = adjqty
                     mat apst  = adj_newcost - invcost

*          Sub-routine to handle HNYAPST call and subsequent G/L posts.

        call_hnyapst
              if apst_qty = 0 then return
                 call "PACKZERO" (apst(), cost$)
                     if pos(cost$ > hex(00)) = 0% then return

                call "HNYAPST" (partnr$, store$, lot$, apst_qty,         ~
                                apst(), acct$(), #8, #2, #9, #10, ret%)
                if ret% <> 1% and ret% <> 2% then return

                apst = 0
                for a% = 1% to 12%   /* EXTEND FOR G/L ADJ AMOUNTS     */
                     apst(a%) = round(apst_qty * apst(a%), 2%)
                     apst     = round(apst + apst(a%), 2%)
                next a%

                gltext$ = str(vencode$,,9%) & invoice$
                str(gltext$,31%,34%)= str(partnr$,,25%) & str(store$,,3%)~
                                       & str(lot$,,6%)
                str(gltext$,65%, 4%) = "ADJ"
                str(gltext$,69%,32%) = "PAYABLES:   INVENTORY VALUE ADJ."
                balflag% = 2% : glamt = apst : glacct$ = adjacct$
                tran_type$ = "VXJ05"
                gosub put_in_gl_array  /* Credit */

                a1% = 2% : a2% = 13% : tran_type$ = "VXJ04"
                if ret% <> 1% then L17310
                   a1%, a2% = 2%:acct$(2%) = acct$(1%):apst(1%) = apst
                   tran_type$ = "VXJ05"
L17310:         for a% = a1% to a2%
                     if abs(apst(a% - 1%)) < .01 then goto L17400
                     balflag% = 1%
                     glamt = apst(a% - 1%) : glacct$= acct$(a%)
                     gosub put_in_gl_array  /* Debit */
L17400:         next a%
            return

L18000
*        If we have a change in quantity (implies a non-PO sourced
*        line), then we write to the Inventory transactions register.

            if abs(adjqty) < .000001 then L20000
            if jnlid$ = "VEZ" then L20000  /* Non-stocked only */

            write #24 using L18080, userid$, vencode$, invoice$, partnr$, ~
                                   time, newprice, adjqty, store$, lot$, ~
                                   jnlid$, pstseq%, newjob$
L18080:         FMT CH(3), CH(9), CH(16), CH(25), CH(8), PD(14,7),       ~
                    PD(14,4), CH(3), CH(6), CH(3), BI(4), CH(7)


*        LOTTRACK UPDATING. Only update Lot Tracking if:
*              (1) No PO exists   -AND-
*              (2) The sequence number to PAYLINES will change.
*              (3) If the old part # is different than the new part #

*         Now write in new guy.
            call "LOTTRACK" ("V", str(vencode$) & str(invoice$),         ~
                             buf2seqnr$, "INV", " ", "H", partnr$, lot$, ~
                             store$, " ", adjqty, #10, #2)

L20000: procurement_history_and_job
*        PROCUREMENT HISTORY.  If no purchase exists, update the
*        procurement history file.  We make two passes here since the
*        subroutine will abort if the quantity is equal to zero.

            if abs(newqty - oldqty) >= .0001 then L20065
            if abs(newprice - oldprice) < .0000001 then pip_activity

L20065:     procsrce$ = str(vencode$) & "I" & str(invoice$)
            call "HNYPROCU" (oldpartnr$, procsrce$, date, -oldqty,       ~
                             -oldprice, apdate$, #10, #2)

            call "HNYPROCU" (newpartnr$, procsrce$, date, newqty,        ~
                             newprice, apdate$, #10, #2)

*        We finish this off with Job/Project related tasks.
*        If both Old and New jobs are blank then we are already done.
*        If the Part is a stocked part then we adjust the PIPs; if it
*        is a non-stock Part then we adjust the material values for
*        the Job/Project.

        pip_activity

            if update$ <> "YES" then return
            if oldjob$ = " " and newjob$ = " " then return
            if oldjob$ <> newjob$ then L21100
            if oldqty <> newqty then L21100
            if oldcost = newcost then return
            if oldpurjob% <> 0% then L21100
            if newpurjob% <> 0% then L21100
*             CALL "READ100" (#13, NEWPARTNR$, PLANNED%)
*                   IF PLANNED% <> 0% THEN RETURN

L21100: REM . . . SOMETHING TO DO.
            REM Note, If Reciever, Then Expense Account Is Elsewhere.
            if oldjob$ = " " then L21320
            if abs(oldqty) < .0001 then L21320
               pipjob$ = oldjob$
               pipqty  = -oldqty
               partnr$ = oldpartnr$
               pipext  = round(pipqty * oldcost, 2%)
               mat pipext = (pipqty) * oldcost
                   call "PACKZERO" (pipext(), cost$)
               pipacct$ = oldexpacct$
               if rcv$ <> " " then pipacct$ = adjacct$
               pippurjob% = oldpurjob%
               adjflag$ = "REV"

               gosub post_to_non_stocked

L21320:     if newjob$ = " " then return
            if abs(newqty) < .0001 then return
               pipjob$ = newjob$
               pipqty  = newqty
               partnr$ = newpartnr$
               pipext  = round(pipqty * newcost, 2%)
               mat pipext = (pipqty) * newcost
                   call "PACKZERO" (pipext(), cost$)
               pipacct$ = newexpacct$
               if rcv$ <> " " then pipacct$ = adjacct$
               pippurjob% = newpurjob%
               adjflag$ = " "

        post_to_non_stocked

            if pippurjob% <> 0% then L22060
               call "READ100" (#10, partnr$, stocked%)
                  if stocked% <> 0% then return

L22060:     call "READ100" (#19, pipjob$, f1%(19%))/* Read JBMASTR2    */
            if f1%(19%) = 0 then L22500             /* Not a JOB        */
                get #19 using L22090, temp$
L22090:              FMT XX(152), CH(6)
                if temp$ <> " " and temp$ <> blankdate$ ~
                   then return                      /* All done here.   */

            hnytext$ = "N/STOCKED ITEM:" & partnr$
            if pippurjob% = 0% then L22170
               hnytext$ = "AP:" & invoice$
               hnytext$ = hnytext$ & " P/JOB VALUE ADDED ADJ"

L22170:     call "JBVLPOST"(#19, #20, #17, return%, pipjob$, 3% ,apdate$,~
                  apdate$, userid$, hnytext$, pipext())

            if return% <> 0% then return
            if abs(pipext) < .01 then return

            call "READ100" (#19, pipjob$, f1%(19%))
                 if f1%(19%) = 0% then return
                    get #19 using L22260, wipacct$
L22260:                 FMT XX(158), CH(9)

            gltext$ = pipjob$
            str(gltext$,31%,34%) = str(vencode$,,9%) & invoice$
            str(gltext$,65%, 4%) = adjflag$
            str(gltext$,69%,32%) = "PAYABLES:   N/STOCKED ITEM TO JOB"
            if pippurjob% = 0% then L22340
               str(gltext$,69%,32%) = "PAYABLES:   P/JOB VALUE ADDED ADJ"
L22340:     balflag% = 2% : glamt = pipext : glacct$ = pipacct$
            tran_type$ = "VXJ02"
            gosub put_in_gl_tif  /* Credit */

            balflag% = 1% : glamt = pipext : glacct$ = wipacct$
            tran_type$ = "VXJ06"
            gosub put_in_gl_tif  /* Debit */
            return

L22500:     if pippurjob% <> 0% then return
            call "READ100" (#18, pipjob$, f1%(18%))/* Read PRJMASTR    */
            if f1%(18%) = 0 then return            /* Also not a proj  */
                get #18 using L22540, temp$
L22540:              FMT XX(44), CH(6)
                if temp$ <> " " then return        /* All done.        */

            call "JPURPOST" (pipjob$, vencode$, invoice$, apdate$, po$,  ~
                             partnr$, pipqty, pipext, pipacct$, #18, #21,~
                             f2%(18%), f2%(21%), return%)

            return

        receiver_update  /* Update Receiver and, if price changed, G/L */

            if rcv$ = " " then return
            mat apst = newcost - oldcost
              call "PACKZERO" (apst(), cost$)
            if oldqty   <> newqty   then L23050
            if oldprice <> newprice then L23050
            if pos(cost$ > hex(00)) = 0% then return

L23050: REM Need To Update, first re-align receiver quantities...
            total_invoiced = 0
            readkey$ = str(rcv$) & str(po$) & str(poline$)
L23080:     call "PLOWALTS" (#6, readkey$, 1%, 35%, f1%(6%))
                  if f1%(6%) = 0% then L23150
               get #6, using L23110, thisinvoice
L23110:            FMT POS(98), PD(14,4)
               total_invoiced = total_invoiced + thisinvoice
            goto L23080

L23150:     total_invoiced = total_invoiced + (newqty - oldqty)
            readkey$ = str(rcv$)& str(vencode$)& str(po$)& str(poline$)
            str(readkey$,45%) = all(hex(00))

            call "PLOWNXT1" (#7, readkey$, 44%, f1%(7%))
                if f1%(7%) = 0% then return  /* What?! */
            get #7, str(oldrec$())   /* Save For Later */
            get #7, using L23230, tqty, returned, qtytoinv, rcvext, oldjob$

L23230:     FMT POS(156), PD(14,4),      /* Total Quantity Received    */~
                POS(196), 2*PD(14,4),    /* Returned to Vendor, To OH  */~
                POS(248), PD(14,4),      /* Extension (Receiver Value) */~
                POS(400), CH(8)          /* Old job number             */

            uninvoiced = tqty - total_invoiced - returned
            call "PACKZERO" (newcost(), cost$)
            put #7, using L23290, uninvoiced, newprice, 0, 0, newcost,    ~
                    cost$
L23290:         FMT POS(356), PD(14,4), 3*PD(14,7), POS(512), PD(14,4),  ~
                    CH(96)

            uninvoiced = round(uninvoiced * newprice, 2%)
            invoiced = round(total_invoiced * newprice, 2%)
            rtvadj = round(returned * newprice, 2%)
            apadj = round((uninvoiced + invoiced + rtvadj) - rcvext, 2%)
            put #7, using L23360, uninvoiced, invoiced, apadj, rtvadj
L23360:         FMT POS(256), 4*PD(14,4)
            rewrite #7
            get #7, str(newrec$())   /* Save For Later */

        REM Did Price or Cost Change?
            get str(oldrec$()), using L23420, oldprice, oldcost, oldcost()
L23420:         FMT POS(364), PD(14,7), POS(512), 13*PD(14,4)
            gosub check_for_core_oldcost
            mat apst = newcost - oldcost
                call "PACKZERO" (apst(), cost$)
            if oldprice <> newprice then L23480
            if pos(cost$ > hex(00)) = 0% then return


L23480: REM Update G/L ...
*          This code is basically independent of the A/P update,
*          it essentially re-does the posting done by RCVUPDTE,
*          using the current A/P price.

        REM Rev for Interim Liabilty Account
            gltext$ = str(vencode$,,9%) & str(rcv$)
            str(gltext$,31%,34%) = str(oldpartnr$,,25%)
            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%) = "PAYABLES:   PRICE CHANGE FROM PO"
                balflag% = 1%  /* Debit (Rev) */
                get str(oldrec$(),156%,8%), using L24120, total_rcvd
                get str(oldrec$(),196%,8%), using L24120, returned
                glamt = round((total_rcvd - returned) * oldprice, 2%)
                glacct$ = str(oldrec$(),296%,9%)
                tran_type$ = "VXJ07"
                gosub put_in_gl_array
                REM Remake G/L entry...
                str(gltext$,65%, 4%) = "ADJ"
                balflag% = 2%  /* Credit */
                glamt = round((total_rcvd - returned) * newprice, 2%)
                tran_type$ = "VXJ07"
                gosub put_in_gl_array

        REM Rev for Price/Vost Variance Account
            gltext$ = str(vencode$,,9) & str(rcv$)
            str(gltext$,31%,34%) = str(oldpartnr$,,25%)
            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%) = "PAYABLES:   PRICE/COST VAR.(PO)"
                balflag% = 1%  /* Debit (Rev) */
                get str(oldrec$(),156%,8%), using L24120, total_rcvd
                get str(oldrec$(),196%,8%), using L24120, returned
                glamt = round((total_rcvd - returned) *                  ~
                                                      pc_var_oldcost, 2%)
                glamt = glamt -                                          ~
                        round((total_rcvd - returned) * oldprice, 2%)
                glacct$ = newpcvacct$ /* LOADED BY PAYINPUT */
                tran_type$ = "VXJ03"
                gosub put_in_gl_array
                REM Remake G/L entry...
                str(gltext$,65%, 4%) = "ADJ"
                balflag% = 2%  /* Credit */
                glamt = round((total_rcvd - returned) *                  ~
                                                      pc_var_newcost, 2%)
                glamt = glamt -                                          ~
                        round((total_rcvd - returned) * newprice, 2%)
                tran_type$ = "VXJ03"
                gosub put_in_gl_array

        REM Now reverse accounts that offset pre payable liablity...
            if pos(cost$ > hex(00)) = 0% then return
            str(gltext$,69%,32%) = "PAYABLES:   COST CHANGE FROM PO"
        REM Rev for inventory in receiver hold...
            str(gltext$,65%, 4%) = "REV"
                balflag% = 2%  /* Credit (Rev) */
                get str(oldrec$(),164%,8%), using L24120, qty_in_rec_hold
                glamt = round(qty_in_rec_hold * oldcost, 2%)
                glacct$ = str(oldrec$(),305%,9%)
                tran_type$ = "VXJ08"
                gosub put_in_gl_array
                REM Remake G/L entry...
                str(gltext$,65%, 4%) = "ADJ"
                balflag% = 1%  /* Debit */
                glamt = round(qty_in_rec_hold * newcost, 2%)
                tran_type$ = "VXJ08"
                gosub put_in_gl_array

        REM Rev for inventory in Q/C...
            str(gltext$,65%, 4%) = "REV"
                balflag% = 2%  /* Credit (Rev) */
                get str(oldrec$(),172%,8%), using L24120, qty_in_qc
                get str(oldrec$(),180%,8%), using L24120, qty_in_qc_hold
                glamt = round((qty_in_qc + qty_in_qc_hold) * oldcost, 2%)
                glacct$ = str(oldrec$(),314%,9%)
                tran_type$ = "VXJ09"
                gosub put_in_gl_array
                REM Remake G/L entry...
                str(gltext$,65%, 4%) = "ADJ"
                balflag% = 1%  /* Debit */
                glamt = round((qty_in_qc + qty_in_qc_hold) * newcost, 2%)
                tran_type$ = "VXJ09"
                gosub put_in_gl_array

        REM Rev for inventory to rework...
            str(gltext$,65%, 4%) = "REV"
                balflag% = 2%  /* Credit (Rev) */
                get str(oldrec$(),188%,8%), using L24120, qty_to_rwk
                glamt = round(qty_to_rwk * adj_oldcost, 2%)
                glacct$ = str(oldrec$(),323%,9%)
                tran_type$ = "VXJ10"
                gosub put_in_gl_array
                REM Remake G/L entry...
                str(gltext$,65%, 4%) = "ADJ"
                balflag% = 1%  /* Debit */
                glamt = round(qty_to_rwk * adj_newcost, 2%)
                tran_type$ = "VXJ10"
                gosub put_in_gl_array
            if oldpurjob% <> 0% then L24140
            if newpurjob% <> 0% then L24140
            if core_on% = 1% then gosub core_reverse_inventory_to_rwk
L24120:     FMT PD(14,4)

L24140: REM Now for inventory in moved to on hand...
            readkey$ = str(rcv$)& str(vencode$)& str(po$)& str(poline$)
L24160:     call "PLOWNXT1" (#31, readkey$, 44%, f1%(31%))
                if f1%(31%) = 0% then L24443
            get #31, using L24470, partnr$, store$, lot$, apst_qty,       ~
                                  invcost, adjacct$
            put #31, using L24210, newprice, newcost
L24210:         FMT POS(95), 2*PD(14,4)
            rewrite #31

        REM Rev for inventory moved to on hand...
            gltext$ = str(vencode$,,9%) & str(rcv$)
            str(gltext$,31%,34%) = str(oldpartnr$,,25%)
            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%) = "PAYABLES:   COST CHANGE FROM PO"
                balflag% = 2%  /* Credit (Rev) */
                glamt = round(apst_qty * adj_oldcost, 2%)
                glacct$ = adjacct$
                tran_type$ = "VXJ11"
                gosub put_in_gl_array
                REM Remake G/L entry...
                str(gltext$,65%,4%) = "ADJ"
                balflag% = 1%  /* Debit */
                glamt = round(apst_qty * adj_newcost, 2%)
                tran_type$ = "VXJ11"
                gosub put_in_gl_array
            if oldpurjob% <> 0% then L24160
            if newpurjob% <> 0% then L24160
            if core_on% = 1% then gosub core_reverse_inventory_to_oh

            REM Re-align HNYQUAN...
            mat apst = adj_newcost - adj_oldcost
            gosub call_hnyapst
            goto L24160

L24443:     if oldjob$ = " " then return
            newjob$ = oldjob$
            oldqty, newqty = qtytoinv
            gosub procurement_history_and_job
            return

L24470:     FMT                          /* RECEIVER INVENTORY DIST.   */~
                XX(16),                  /* Receiver Number            */~
                XX(9),                   /* Vendor code                */~
                XX(16),                  /* P.O. Number                */~
                XX(3),                   /* P.O. Line Sequence         */~
                CH(25),                  /* Part                       */~
                CH(3),                   /* Store                      */~
                CH(6),                   /* Lot                        */~
                XX(8),                   /* 2*BI(4) DATE/TIME STAMP    */~
                PD(14,4),                /* Quantity                   */~
                XX(8),                   /* Price per Vendor's invoice */~
                PD(14,4),                /* Total Inventory cost       */~
                XX(8),                   /* Filler                     */~
                CH(9)                    /* HNY Asset Account          */

        REM Load extra information for G/L Export, if on
        load_gl_info

            call "READ100" (#10, newpartnr$, f1%(10%))
                if f1%(10%) = 0% then L25045
            get #10 using L25035, partgen$, uom$, partcat$, partclass$,   ~
                parttype$
L25035:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
L25045:     put str(gl_post_info$(),,) using L25280,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                currency$,               /* Currency code CH(4)        */~
                convunt,                 /* Currency Units per Book    */~
                amount,                  /* Functional Currency amount */~
                pstqty,                  /* Unit amount                */~
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
                newpartnr$,              /* Part Number CH(25)         */~
                partcat$,                /* Part Category CH(4)        */~
                partclass$,              /* Part Class CH(4)           */~
                partgen$,                /* Part Generic code CH(16)   */~
                parttype$,               /* Part Type CH(3)            */~
                uom$,                    /* Part UOM CH(4)             */~
                newstore$,               /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                vencode$,                /* Vendor code CH(9)          */~
                ventype$,                /* Vendor type CH(2)          */~
                po$,                     /* Purchase Order CH(16)      */~
                rcv$,                    /* Receiver Number CH(16)     */~
                invoice$,                /* Invoice Number CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                gl_job$,                 /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L25280: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Transaction Currency amount*/~
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
                CH(16),                  /* Part Generic code CH(25)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice CH(16)      */~
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

*        Check for reman part and determine costs for it.  First
*        set-up some new variables

        check_for_core_newcost
            adj_newcost     = newcost
            pc_var_newcost  = newcost
            mat adj_newcost = newcost
            new_reman%      = 0%
            newpurjob%      = 0%

            if newjob$ = " " then L25704
               call "READ100" (#13, newpartnr$, newpurjob%)
               if newpurjob% = 0% then L25704
               return

L25704:     if core_on% <> 1% then return

            call "CORCSTSB" (newpartnr$, " ", " ", newcost, newcost(),   ~
                             newcorepart$, new_cfg_acct$, newcorecost,   ~
                             adj_newcost, adj_newcost(), pc_var_newcost, ~
                             #2, #8, #10, #23, ret%)

            if ret%        <> 0% then return
            if newcorecost  = 0  then return
            new_reman%      = 1%
            return


        check_for_core_oldcost
            adj_oldcost     = oldcost
            pc_var_oldcost  = oldcost
            mat adj_oldcost = oldcost
            old_reman%      = 0%
            oldpurjob%      = 0%

            if oldjob$ = " " then L25974
               call "READ100" (#13, oldpartnr$, oldpurjob%)
               if oldpurjob% = 0% then L25974
               return

L25974:     if core_on% <> 1% then return

            call "CORCSTSB" (oldpartnr$, " ", " ", oldcost, oldcost(),   ~
                             oldcorepart$, old_cfg_acct$, oldcorecost,   ~
                             adj_oldcost, adj_oldcost(), pc_var_oldcost, ~
                             #2, #8, #10, #23, ret%)

            if ret%        <> 0% then return
            if oldcorecost  = 0  then return
            old_reman%      = 1%
            return


        put_in_gl_array_core   /* Place G/L data in array for summary */

            if glamt = 0 then return

            gosub condition_balflag

            core_gl_array$(core_entry%) = str(glacct$,,9%)               ~
                                                     & str(gltext$,,100%)
            core_gl_drcr(core_entry%, balflag%) = abs(glamt)
            core_gl_type$(core_entry%) = str(tran_type$,4%,2%)

            core_entry% = core_entry% + 1%

            return

        prepare_to_write_gl_array_core
            if core_entry% = 1% then return
            mat gl_array$  = core_gl_array$
            mat gl_drcr    = core_gl_drcr
            mat gl_type$   = core_gl_type$
            entry%         = core_entry%
            gosub write_gl_array
            return

*        CR CORE Asset/Expense Acct with Old Line extension (reverses)
        core_reverse_old_line
            if po$ <> " " then return
            save_gltext$ = gltext$
            save_part$   = oldpartnr$

            gltext$ = str(vencode$,1%,9%) & str(invoice$)
            str(gltext$,31%,34%)=str(oldcorepart$,,25%) & " CORE VAL"

            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%) = "PAYABLES:   INVOICE LINE DIST."

                balflag% = 2% : glacct$ = old_cfg_acct$
                glamt = round(oldqty * oldcorecost, 2%)
                if oldoh% = 0% then tran_type$ = "VXJ12" else            ~
                                    tran_type$ = "VXJ13"
                gosub put_in_gl_array_core

            gltext$    = save_gltext$
            oldpartnr$ = save_part$
            return


*         Core DR Asset/Expense Acct with New line extension
        core_post_new_line_extension
            if po$ <> " " then return
            save_gltext$ = gltext$
            save_part$   = newpartnr$

            gltext$ = str(vencode$,,9%) & str(invoice$)
            str(gltext$,31%,34%)=str(newcorepart$,,25%) & " CORE VAL"

            str(gltext$,69,32) = "PAYABLES:   INVOICE LINE DIST."


                balflag% = 1% : glacct$ = new_cfg_acct$
                glamt = round(newqty * newcorecost, 2%)
                if newoh% = 0% then tran_type$ = "VXJ12" else            ~
                                    tran_type$ = "VXJ13"
                gosub put_in_gl_array_core

            gltext$    = save_gltext$
            newpartnr$ = save_part$
            return


*         Core CR (Reverse) Asset/Expense Acct with New line extension
        core_reverse_old_line_delete
            if po$ <> " " then return
            save_gltext$ = gltext$

            gltext$ = str(vencode$,,9%) & str(invoice$)
            str(gltext$,31%,34%)=str(oldcorepart$,,25%) & " CORE VAL"

            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%) = "PAYABLES:   INVOICE LINE DIST."

                balflag% = 2% : glacct$ = old_cfg_acct$
                glamt = round(oldqty * oldcorecost, 2%)
                if oldoh% = 0% then tran_type$ = "VXJ12" else            ~
                                    tran_type$ = "VXJ13"
                save_part$ = newpartnr$
                newpartnr$ = oldcorepart$
                gosub put_in_gl_tif
                gltext$    = save_gltext$
                newpartnr$ = save_part$
            return


        REM Rev for inventory to rework...
        core_reverse_inventory_to_rwk
            if old_reman% <> 1% and new_reman% <> 1% then return
            if oldcorecost <> newcorecost then L27660
            if old_cfg_acct$ <> new_cfg_acct$ then L27660
            return

L27660:     save_text$ = gltext$

            gltext$ = str(vencode$,,9) & str(rcv$)
            str(gltext$,31%,34%) = str(oldcorepart$,,25%) & " CORE VAL"
            str(gltext$,69%,32%) = "PAYABLES:   COST CHANGE FROM PO"
            str(gltext$,65%, 4%) = "REV"

            if old_reman% <> 1% then L27800
                balflag% = 2%  /* Credit (Rev) */
                glamt = round(qty_to_rwk * oldcorecost, 2%)
                glacct$ = old_cfg_acct$
                tran_type$ = "VXJ14"
                gosub put_in_gl_array

L27800:     if new_reman% <> 1% then L27890
                REM Remake G/L entry...
                str(gltext$,31%,34%)= str(newcorepart$,,25%) & " CORE VAL"
                str(gltext$,65%, 4%) = "ADJ"
                balflag% = 1%  /* Debit */
                glamt = round(qty_to_rwk * newcorecost, 2%)
                tran_type$ = "VXJ14"
                gosub put_in_gl_array

L27890:     gltext$ = save_text$
            return

        REM Rev Core for inventory moved to on hand...
        core_reverse_inventory_to_oh
            if old_reman% <> 1% and new_reman% <> 1% then return
            if oldcorecost <> newcorecost then L28070
            if old_cfg_acct$ <> new_cfg_acct$ then L28070
            return

L28070:     save_text$ = gltext$

            gltext$ = str(vencode$,,9%) & str(rcv$)
            str(gltext$,31%,34%) = str(oldcorepart$,,25%) & " CORE VAL"
            str(gltext$,65%, 4%) = "REV"
            str(gltext$,69%,32%) = "PAYABLES:   COST CHANGE FROM PO"

            if old_reman% <> 1% then L28210
                balflag% = 2%  /* Credit (Rev) */
                glamt = round(apst_qty * oldcorecost, 2%)
                glacct$ = old_cfg_acct$
                tran_type$ = "VXJ13"
                gosub put_in_gl_array

L28210:     if new_reman% <> 1% then L28310
                REM Remake G/L entry...
                str(gltext$,31%,34%)= str(oldcorepart$,,25%) & " CORE VAL"
                str(gltext$,65%,4%) = "ADJ"
                balflag% = 1%  /* Debit */
                glamt = round(apst_qty * newcorecost, 2%)
                glacct$ = new_cfg_acct$
                tran_type$ = "VXJ13"
                gosub put_in_gl_array

L28310:     gltext$ = save_text$
            return

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        normal_end
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#27)
            end 0%

        abend_userid  /* Abort- User ID was not found in SYSFILE2.     */
            call "SHOSTAT" ("UPDATE ABORTED: Unauthorized User ID")
            goto L65270

        abend_month  /* Abort- A/P date was not in an open G/L period. */
            call "SHOSTAT" ("UPDATE ABORTED: A/P date cannot be posted")
L65270:     call "FILEBGON" (#27)
            end 1%
