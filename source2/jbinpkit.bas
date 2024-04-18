        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   IIIII  N   N  PPPP   K   K  IIIII  TTTTT   *~
            *    J    B   B    I    NN  N  P   P  K  K     I      T     *~
            *    J    BBBB     I    N N N  PPPP   KKK      I      T     *~
            *  J J    B   B    I    N  NN  P      K  K     I      T     *~
            *   J     BBBB   IIIII  N   N  P      K   K  IIIII    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBINPKIT - Manual input of Jobs with Kit List (BOM)       *~
            *            and capacity allocations management.           *~
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
            * 07/20/83 ! ORIGINAL                                 ! KEN *~
            * 12/20/83 ! CREATE JBCROSS2 IF NOT THERE             ! RRS *~
            * 05/30/84 ! DISABLE QTY-TO-MAKE FOR REWORK JOBS      ! ERN *~
            * 07/23/84 ! ADD ORIG QUAN AND STD COST FLDS TO JBMAST! BLT *~
            * 04/15/85 ! BUG IN PHANTOM LOGIC LINE 33628          ! KAB *~
            * 05/02/85 ! CHECK QTYMAKE EDIT                       ! KAB *~
            * 01/09/86 ! Ergo overhaul                            ! HES *~
            * 02/04/86 ! Added 'Production Scheduler' checking    ! HES *~
            * 03/12/86 ! Added call to CDANPOST.                  ! LDJ *~
            * 03/24/86 ! FIXED PHANTOM MULTIPLIER LOGIC @ 33410   ! KAB *~
            * 04/02/86 ! AVOID BLANK DESCRIPTIONS                 ! HES *~
            * 10/29/86 ! Added Capacity Allocation Management     ! HES *~
            * 02/03/87 ! Changes in support of Serial & Lot Number! LDJ *~
            *          ! Tracking.  Also fixed little bug(s).     !     *~
            * 02/26/87 ! Added Text Management                    ! MJB *~
            * 03/27/87 ! Added Job Copy Functn, Pansize/MOQ logic ! HES *~
            * 06/15/87 ! Standard Costing Modifications           ! ERN *~
            * 02/29/88 ! Allow Printing of Pick List in BOM order ! HES *~
            * 03/01/88 ! Mark Job as 'in use' while editing       ! HES *~
            * 03/23/88 ! Fixed WCMASTR update bug related to copy ! HES *~
            * 08/08/88 ! Deleted test to not include by-products  ! TLJ *~
            *          ! when loading the components (LOAD_BOM)   !     *~
            * 08/08/88 ! Added call to CLEAR_IN_USE if Job was    ! TLJ *~
            *          ! previously closed and nit reopened.      !     *~
            * 10/21/88 ! Added 'append route' logic               ! WPH *~
            *          ! Corrected logic to derive seq. number    !     *~
            *          ! at lines 38221 & 38361                   !     *~
            * 12/21/88 ! Now if you try append route and fail from! WPH *~
            *          ! cap edit screen it places you at header  !     *~
            *          ! edit mode screen.  Improved handing of   !     *~
            *          ! append route if route has no set-up, run !     *~
            *          ! or move/queue.  Added control number     !     *~
            *          ! field for relating jobs together for     !     *~
            *          ! campaigns or to a sales order.  Added    !     *~
            *          ! PF 9 to header edit mode to reset        !     *~
            *          ! component pick dates to job plan start.  !     *~
            *          ! Added ability to copy BOM for other part.!     *~
            * 03/28/89 ! PRR 10764; If Kit List is empty, PF 9    ! RJM *~
            *          ! is turned off (Reset Kit List Dates).    !     *~
            * 03/30/89 ! PRR 10703; Fixed problem in generating   ! RJM *~
            *          ! WCOUT Sequence No's when a step takes    !     *~
            *          ! more than 1 day. Lines 38220-38226       !     *~
            * 05/08/90 ! Fixed Loop Append Rte/ VEND WC           ! KAB *~
            * 09/26/90 ! Added ESTIMATED PERCENT COMPLETE memo    ! SID *~
            *          ! field for pegging features, and an Alt   !     *~
            *          ! Key to JBMASTR2 select statement.        !     *~
            * 10/15/90 ! Moved large array variables from LET     ! MJB *~
            *          !  to INIT in INITIALIZE Section for       !     *~
            *          !  compatibility with BASIC 4.3.1          !     *~
            * 05/07/91 !(PRR 11601) 'PRR was Originally written   ! RJB *~
            *          !    against JBQSCHDL.' Ken corrected the  !     *~
            *          !    Loop Logic in PREPARE_FOR_DISPLAY sec-!     *~
            *          !    tion to allow for the WCOUT records to!     *~
            *          !    be written correctly when concurrent  !     *~
            *          !    Work Centers are used.                !     *~
            * 09/10/91 ! Removed copy BOM restriction in input    ! KAB *~
            *          !    mode.  Copy auto loads default BOM,   !     *~
            *          !    1st time.  PLOWCODE thereafter.       !     *~
            *          !    Simple tests @ DEFFN'201 & LOAD_BOM   !     *~
            * 04/14/92 ! Changed because JBINUSE now can Clear In-! JDH *~
            *          !    use flag & Write JBTIF In-use flag.   !     *~
            * 05/15/93 ! Core Project                             ! KAB *~
            *          !   Input/Edit of Core Wip if App.         !     *~
            *          !   No More Orphan Text on Startover       !     *~
            *          !   Serial #'s Handled on Copy             !     *~
            *          !     - Text Copied if Requested           !     *~
            *          !     - Est % Comp = 0; RWK, Qty = 0       !     *~
            *          !   Rework PIPIN, On Order via Flag        !     *~
            *          !   Pass JBMASTRC to JBQJOB                !     *~
            * 07/29/93 ! PRR 11311 - Added Stocking UOM beside    ! MLJ *~
            *          !  'Current Job Qty' on header screen.     !     *~
            *          ! Purchased Jobs -                         !     *~
            *          !  1) Added logic to ensur new job #'s do  !     *~
            *          !     begin with 'PJ'.                     !     *~
            *          !  2) Now honors Vendor, PO & Poline (new  !     *~
            *          !     fields in JBMASTR2, pos 108-135),    !     *~
            *          !     displayed on header if present.      !     *~
            *          !  3) Added VBKLINES for sync of mods to   !     *~
            *          !     Quantity & Date.                     !     *~
            *          !  4) Added minimum qty check on Current   !     *~
            *          !     Job Qty.                             !     *~
            *          !  5) Added (7)See PO Info (calls POSTATUS)!     *~
            *          !     on Purchased Jobs only.  Also added  !     *~
            *          !     RCVLINES, VBKMASTR, PAYLINES, and    !     *~
            *          !     PAYMASTR for call to POSTATUS.       !     *~
            * 08/24/93 !MISC - Corrected data conversion error    ! MLJ *~
            *          !  when trying to edit blank line caused by!     *~
            *          !  insert with an append.                  !     *~
            * 12/31/93 !Problem with Move Queue if Run < 1.  Order! KAB *~
            *          !  problem with Move Queue when loading    !     *~
            *          !  from route.                             !     *~
            * 03/30/94 !PRR 13110. Added testing for MQ/MA dates. ! JDH *~
            *          !PRR 13131. Added part type to hdr screen, !     *~
            *          !  blinking if < 500; Purchased parts now  !     *~
            *          !  default WIP account from HNYMASTR.      !     *~
            *          !PRR 13148. Added last modified by & date. !     *~
            * 03/30/94 ! Added PFKey prompt and call to ECRINQSB. ! LDJ *~
            *          ! Corrected subscript underflow (0)        !     *~
            *          ! recently introduced - see DEFFN'202 - C%.!     *~
            * 05/25/94 !Corrected potential subscript by zero.    ! MLJ *~
            * 06/23/94 ! Added management of VBKVSA file.         ! ERN *~
            *          !  Enabled activity field on WC screen.    !     *~
            * 07/25/94 ! PRR 13247.  Blow away SERTIF on strtover.! JDH *~
            * 09/21/94 !Removed 5/25 code. Corrected variable dims! LDJ *~
            * 11/27/95 ! PRR 13149. Added UOM toggle on kit list. ! JDH *~
            *          ! PRR 13531. Fixed reset of parent part    !     *~
            *          !  type when entering components.          !     *~
            * 06/05/96 ! Fix WCOUT SeqNumbers to iterate when new ! RJH *~
            *          !  step is encountered. Particular Problem !     *~
            *          !  when inserting steps manually.          !     *~
            * 06/05/96 ! Fix WCOUT SeqNumbers to iterate when new ! RJH *~
            *          !  step is encountered. Particular Problem !     *~
            *          !  when inserting steps manually.          !     *~
            * 09/06/96 ! Millie date conversion                   ! DER *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            ask$(3)80,                   /* Ask User Text              */~
            bdate$6,                     /* Planning Calendar Base Date*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$3,                       /* BOM Id.                    */~
            bom$(490)3,                  /* BOM List                   */~
            bomid$3,                     /* Which Alt BOM?             */~
            control$19,                  /* Code to relate jobs        */~
            copypart$25,                 /* part number for copy BOM   */~
            copybomid$3,                 /* BOM ID to copy             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateclosed$8,                /* Date job was closed        */~
            dfac$1,                      /* Field attribute characters */~
            disp1$11,                    /* Vendor Display Label       */~
            disp2$9,                     /* Vendor Display Value       */~
            disp3$9,                     /* PO Display Label           */~
            disp4$21,                    /* PO Display Value           */~
            ecrpfk$14,                   /* ECR Inquiry PFKey Prompt   */~
            effdate$8,                   /* Effectivity Date           */~
            errormsg$79,                 /* Error message              */~
            estperccomp$8,               /* Est. Percentage Complete   */~
            fromjob$8,                   /* From Job                   */~
            gltype$1,                    /* Account Type               */~
            hdr$(3)79,                   /* PLOWCODE header strings    */~
            header$79,                   /* Header                     */~
            header_kit$(2)79,            /* Header for Kit List        */~
            header$(1)1,                 /* PLOWCODE Argument          */~
            hfac$(20)1,                  /* Field attribute characters */~
            i$(24)80,                    /* Screen image               */~
            incl(1),                     /* PLOWCODE Argument          */~
            inpmessage$79,               /* Input message              */~
            jbcorewip$16,                /* Core WIP Account           */~
            jbcorewipd$32,               /* Core WIP Account Descr     */~
            jbcorewipp$32,               /* Core WIP Account Prompt    */~
            jbdescr$30,                  /* Description                */~
            job$8,                       /* Job number                 */~
            last_mod_at$8,               /* Date Last Modified         */~
            last_mod_by$8,               /* Who Last Modified          */~
            lastdate$8,                  /* Save for line item PIPOUT  */~
            lastget$5,                   /* Temp Work Center Variable  */~
            lastjob$8,                   /* Last Job                   */~
            lastwc$5,                    /* Temp Work Center Variable  */~
            lfac$(20,7)1,                /* Field attribute characters */~
            line2$79,                    /* Screen Line 2              */~
            manual$1,                    /* flag                       */~
            minqty$9,                    /* Minimum Qty To Make        */~
            mkr$2,                       /* BOM Marker                 */~
            mqp$1,                       /* Move - Queue flag          */~
            outtagnr$100,                /* Work Variable              */~
            old$8,                       /* Temporary holding variable */~
            olddate$8,                   /* Temp date variable         */~
            oldrun$7,                    /* Temp run qty     Variable  */~
            oldwc$4,                     /* Temp Work Center Variable  */~
            part$25, partdescr$34,       /* Part Number to Build       */~
            partclass$3,                 /* Part Class                 */~
            parttype$3,                  /* Part Type Code - Component */~
            pd%(255),                    /* wc step start date(spl pick*/~
            pfdescr$(3)79,               /* Description Of Keys Active */~
            pfkeys$32, pfkeys$(2)18,     /* P.F. Keys Active           */~
            plenddate$10,                /* Planned Finish             */~
            plowkey$99,                  /* Key for a farmer           */~
            plstdate$10,                 /* Planned Start              */~
            plstdate8$8,                 /* Planned Start (8 char )    */~
            po$16,                       /* Purchase Order Number      */~
            poline$3,                    /* Purchase Order Line Number */~
            qtyadj$10,                   /* Quantity Adjusted          */~
            qtycomp$10,                  /* Quantity Complete          */~
            qtyleft$10,                  /* Quantity Left to Make      */~
            qtymake$10,                  /* Quantity to Make           */~
            qtyorig$10,                  /* Original Quantity to Make  */~
            qtyrewk$10,                  /* Quantity Sent to Rework    */~
            qtyscrp$10,                  /* Quantity Scrapped          */~
            readkey$100,                 /* Working String             */~
            readkey$(100)100,            /* Working String             */~
            rwkpip$1,                    /* Rework PIP Flag            */~
            phfact(101),                 /* Phantom Multiplier         */~
            rteid$3,                     /* Route ID                   */~
            rteloaded$3,                 /* Rte ID Loaded              */~
            savejob$8,                   /* what it sez                */~
            screen$(2,3)79,              /* Screen Messages            */~
            seq$(1600)5,                 /* Sequence numbers for screen*/~
            stdate$10,                   /* Actual start               */~
            stp$4,                       /* Pick before step from bom  */~
            systime$8,                   /* System Time                */~
            temp$16,                     /* Temp variable              */~
            temp1$16,                    /* Temp variable              */~
            tempdate$8,                  /* Temp variable              */~
            tempwc$5,                    /* Temp workcenter variable   */~
            textid$4,                    /* Job Text ID                */~
            texta$(196,1)70,             /* Text Array                 */~
            textmsg$79,                  /* Message for text routines  */~
            tfac$1,                      /* Part Type - FAC            */~
            type$3,                      /* Part Type - Parent         */~
            uom$4,                       /* Stocking Unit Of Measure   */~
            userid$3,                    /* Guess who                  */~
            vendor$9,                    /* Vendor Code                */~
            vfac$(2)1,                   /* Vendor & PO DIsplay FACs   */~
            vsa_actv$1, vsa_gen$1,       /* VBK VSA Flags              */~
            vsa_status$1,                /* VBKVSA status              */~
            wctype$2,                    /* Work Center Type           */~
            wcwc1$4,                     /* Concurrent Work Center 1   */~
            wcwc2$4,                     /* Concurrent Work Center 2   */~
            wcwc3$4,                     /* Concurrent Work Center 3   */~
            wipacct$16, wipdescr$32,     /* WIP Acct                   */~
            wipsave$16,                  /* WIP ACCT                   */~
            yld(255),                    /* yeild                      */~
            zeroes$(8)111                /* Costs for new Job record   */

        dim                                                              ~
            awcu1%(490),                 /* concurrent 1 units avail   */~
            awcu2%(490),                 /* concurrent 2 units avail   */~
            awcu3%(490),                 /* concurrent 3 units avail   */~
            codes$(100)3,                /* Release Codes              */~
            cumuse(490),                 /*                            */~
            stpuse(490),                 /*                            */~
            cumf%(490),                  /*                            */~
            part$(1601)25,               /* Kit Part Number            */~
            partdescr$(1601,2)45,        /* Part Description & UOM     */~
            outdate%(1601),              /* PIPOUT Date Index          */~
            outdate$(1601)8,             /* PIPOUT Date Formatted      */~
            newdate%(1601),              /* PIPOUT Date Index          */~
            newquantity(1601),           /* PIPOUT Quantity            */~
            rte$(255)200,                /* Routing Array              */~
            quantity(1601),              /* PIPOUT Quantity            */~
            quantity$(1601)10,           /* PIPOUT Quantity Formatted  */~
            steps$(255)5,                /* ROUTING ARRAY STEPS FOR SRC*/~
            used%(490),                  /* WORK CENTER USAGE          */~
            uadj%(490),                  /* WORK ARRAY                 */~
            wcact$(1000)4,               /* Work Center Activity Codes */~
            wcdate$(1000)8,              /* WCOUT Dates                */~
            wcfill$(1000)2,              /* Other WCOUT Fields         */~
            wcnet$6,                     /* Net Change In WC Usage     */~
            wcnet$(1000)8,               /* Net Change In WC Usage     */~
            wcnet%(1000),                /* Net Change In WC Usage     */~
            wcpct$(1000)6,               /* WCs Percent Utilization    */~
            wcrun$(1000)7,               /* Run Time                   */~
            wcseq%(1000),                /* Route Sequence Number (+)  */~
            wcstep$(1000)7,              /* Routing Step Code          */~
            wcsu$(1000)6,                /* Setup Time                 */~
            wctype$(1000)2,              /* WCOUT Detail Type          */~
            wcwc$4,                      /* WCOUT Work Center Code     */~
            wc$(1000)4,                  /* WCOUT Work Center Code     */~
            wcwc$(1000)4,                /* WCOUT WC code for display  */~
            wcwcdescr$(1000)22,          /* Work Center Descriptions   */~
            wcwork$(10)196,              /* Work Center Descriptions   */~
            xxsd%(255),                  /*                            */~
            xsd%(255),                   /* Start date for route step  */~
            wa$(2000)1,                  /* IN CASE OF ARRAY OVERFLOW  */~
            wl$(2000)1,                  /*                            */~
            ac$(2000)4,                  /* activity codes             */~
            ws$(2000)5,                  /* WC STEP #                  */~
            du%(2000),                   /* DATE USED ARRAY STACK      */~
            au%(2000),                   /* AMOUNT USED ARRAY STACK    */~
            su%(2000),                   /* SET UP TIME TODAY          */~
            wa%(2000),                   /* WC ALTERNATE SEQ NO.       */~
            f2%(32),                     /* FILE STATUS FLAGS FOR      */~
            f1%(32)                      /* RECORD-ON-FILE FLAGS       */

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
            * #1  ! JBRELUSR ! User Work Order Release Cross Reference  *~
            * #2  ! SYSFILE2 ! System Information File                  *~
            * #3  ! JBMASTR2 ! Job Master file                          *~
            * #4  ! HNYMASTR ! Inventory Master file                    *~
            * #5  ! HNYQUAN  ! Inventory Quantity & GL Defaults         *~
            * #6  ! GLMAIN   ! General Ledger Chart of Accounts         *~
            * #7  ! VBKVSA   ! Vendor Service Advices                   *~
            * #8  ! PIPIN    ! Planned Additions to Inventory file      *~
            * #9  ! PIPMASTR ! Planned Position Master                  *~
            * #10 ! SFCUM2   ! Unused file                              *~
            * #11 ! BOMMASTR ! Bill of Materials Relationship file      *~
            * #12 ! ENGMASTR ! Engineering Master file                  *~
            * #13 ! WCOUT    ! Planned Capacity Allocations             *~
            * #14 ! WCMASTR  ! Work Center Master file                  *~
            * #15 ! TXTFILE  ! System Text File                         *~
            * #17 ! PIPOUT   ! Planned Inventory Withdrawals file       *~
            * #18 ! JBCROSS  ! Job Cross-Reference file                 *~
            * #19 ! RTEMASTR ! Routing Master file                      *~
            * #20 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #21 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #22 ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #23 ! JOBMASTR ! Project Master File                      *~
            * #24 ! JBMATER2 ! Job Material's Ledger                    *~
            * #25 ! JBVALUE2 ! Job 'Value' Ledgers                      *~
            * #26 ! JBTIF    ! Shop Floor Transaction Image File        *~
            * #27 ! JBMASTRC ! Job Master File Core Appendix            *~
            * #28 ! VBKLINES ! Purchase Order Line Item File            *~
            * #29 ! RCVLINES ! Receiver Line Items                      *~
            * #30 ! VBKMASTR ! Vendor Backlog Master File               *~
            * #31 ! PAYLINES ! Payables Line Items File                 *~
            * #32 ! PAYMASTR ! Payables Master File                     *~
            *************************************************************

            select  #1, "JBRELUSR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 70,                                    ~
                        keypos =    1, keylen =   6,                     ~
                        alt key 1, keypos =  4, keylen = 6

            select  #2, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #3, "JBMASTR2",                                      ~
                        varc, indexed, recsize = 1300,                   ~
                        keypos = 1, keylen = 8,                          ~
                        alt key  1, keypos = 1120, keylen = 19, dup,     ~
                            key  2, keypos =   58, keylen = 25, dup

            select  #4, "HNYMASTR",                                      ~
                        varc, indexed, recsize = 900,                    ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos = 90 , keylen = 4, dup

            select  #5, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #6, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #07, "VBKVSA",                                        ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 5,    keylen = 8,                       ~
                        alt key  1, keypos =    1, keylen =  12,         ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  6, keypos =   50, keylen =   4, dup

            select  #8, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 60,                                    ~
                        keypos = 30, keylen = 19,                        ~
                        alternate key 1, keypos = 1, keylen = 48

            select  #9, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos = 2, keylen = 25,                         ~
                        alternate key 1, keypos = 1, keylen = 26

            select #10, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos = 1, keylen = 25

            select #11, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select #12, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            select #13, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 68,                                    ~
                        keypos = 9, keylen = 23,                         ~
                        alt key 1, keypos=  1, keylen = 27

            select #14, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos = 2  , keylen = 5,                        ~
                        alt key 1, keypos = 1, keylen = 6

            select #15, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #17, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 64,                                    ~
                        keypos = 1, keylen = 56,                         ~
                        alt key 1, keypos= 20, keylen = 37

            select #18, "JBCROSS2",                                      ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize =  94,                                    ~
                       keypos =29, keylen = 19,                          ~
                       alternate key 1, keypos = 1 , keylen = 47,        ~
                                 key 2, keypos = 48, keylen = 47

            select #19, "RTEMASTR",                                      ~
                       varc, indexed, recsize = 400,                     ~
                       keypos =5,  keylen = 31,                          ~
                       alternate key 1, keypos = 1 , keylen = 35

            select #20, "SERTIF",                                        ~
                       varc, indexed, recsize = 100,                     ~
                       keypos = 1, keylen = 62

            select #21, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #22, "SERWORK",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  48,                                   ~
                        keypos = 1, keylen = 23

            select #23, "JOBMASTR",                                      ~
                        varc, indexed, recsize = 700,                    ~
                        keypos = 1, keylen = 8

            select #24, "JBMATER2",                                      ~
                        varc, indexed, recsize = 400,                    ~
                        keypos = 1, keylen = 22,                         ~
                        alt key  1, keypos = 23, keylen = 48

            select #25, "JBVALUE2",                                      ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 23

            select #26, "JBTIF",                                         ~
                        varc, indexed, recsize = 350,                    ~
                        keypos = 9, keylen = 18,                         ~
                        alt key  1, keypos =  1, keylen =  26


            select #27, "JBMASTRC"                                       ~
                        varc, indexed, recsize = 600,                    ~
                        keypos = 1, keylen =  8


            select #28, "VBKLINES"                                       ~
                        varc, indexed, recsize = 700,                    ~
                        keypos = 1, keylen = 28,                         ~
                        alt key  1, keypos = 333, keylen =  20, dup

            select #29, "RCVLINES",                                      ~
                        varc, indexed, recsize = 800,                    ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24

            select #30, "VBKMASTR",                                      ~
                        varc, indexed, recsize = 1030,                   ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #31, "PAYLINES",                                      ~
                        varc, indexed, recsize = 541,                    ~
                        keypos = 36, keylen = 28,                        ~
                        alt key 1, keypos =  1, keylen = 63,             ~
                            key 2, keypos = 17, keylen = 47

            select #32, "PAYMASTR",                                      ~
                        varc, indexed, recsize = 350,                    ~
                        keypos = 1, keylen = 25

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, 0%, f2%( 1),   0%, " ")
            call "OPENCHCK" (#02, 0%, f2%( 2),   0%, "REQUIRED")
            call "OPENCHCK" (#03, 0%, f2%( 3), 200%, " ")  /* JBMASTR2 */
            call "OPENCHCK" (#04, 0%, f2%( 4),   0%, "REQUIRED")
            call "OPENCHCK" (#05, 0%, f2%( 5),   0%, " ")
            call "OPENCHCK" (#06, 0%, f2%( 6),   0%, " ")
            call "OPENCHCK" (#07, 0%, f2%( 7), 100%, " ")  /* VBKVSA   */
            call "OPENCHCK" (#08, 0%, f2%( 8), 200%, " ")  /* PIPIN    */
            call "OPENCHCK" (#09, 0%, f2%( 9),   0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10),   0%, " ")
            call "OPENCHCK" (#11, 0%, f2%(14),   0%, " ")
            call "OPENCHCK" (#12, 0%, f2%(16),   0%, " ")
            call "OPENCHCK" (#13, 0%, f2%(13), 300%, " ")  /* WCOUT    */
            call "OPENCHCK" (#14, 0%, f2%(14),   0%, " ")
            call "OPENCHCK" (#17, 0%, f2%(17), 300%, " ")  /* PIPOUT   */
            call "OPENCHCK" (#18, 0%, f2%(18), 100%, " ")  /* JBCROSS2 */
            call "OPENCHCK" (#19, 0%, f2%(19),   0%, " ")
            call "OPENCHCK" (#20, 0%, f2%(20),   0%, " ")
            call "OPENCHCK" (#23, 0%, f2%(23),   0%, " ")
            call "OPENCHCK" (#24, 0%, f2%(24),   0%, " ")
            call "OPENCHCK" (#25, 0%, f2%(25),   0%, " ")
            call "OPENCHCK" (#26, 0%, f2%(26), 500%, " ")
            call "OPENCHCK" (#27, 0%, f2%(27),   0%, " ")
            call "OPENCHCK" (#28, 0%, f2%(28),   0%, " ")
            call "OPENCHCK" (#29, 0%, f2%(29),   0%, " ")
            call "OPENCHCK" (#30, 0%, f2%(30),   0%, " ")
            call "OPENCHCK" (#31, 0%, f2%(31),   0%, " ")
            call "OPENCHCK" (#32, 0%, f2%(32),   0%, " ")

            if f2%(2) <> 0% or f2%(4) <> 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date  :  call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)
            init(hex(00)) zeroes$()

            cok% = dim(wcwc$(),1)
            pok% = dim(part$(),1)
            lastget$ = hex(abcdef)

            call "READ100" (#2, "MONTHS OPEN", f1%(2))
                if f1%(2) = 0% then exit_program
            get #2, using L09260, bdate$
L09260:     FMT XX(32), CH(6)

*        See if operator is an administrator or not
            call "CMSMACHK" ("SFC", hfac$(1), hfac$(2))
            if hfac$(1) = "Y" or hfac$(2) = "Y" then L09430

*        Load Up His Release Codes For Cross Checking...
            call "READNEXT" (#1, f1%(1))
            if f1%(1) = 0% then L09430  /* Not using feature */
                readkey$ = str(userid$) & hex(00000000)
L09360:         call "PLOWNEXT" (#1, readkey$, 3%, f1%(1))
                if f1%(1) = 0 then L09420
                     c% = c% + 1%
                     get #1, using L09400, codes$(c%)
L09400:                   FMT XX(3), CH(3)
                     goto L09360
L09420:         if codes$() <> "ALL" then L09450   /* What did we get?   */
L09430:     admin% = 1%  /* This guy can do as he pleases...   */

L09450
*        On to bigger and better things...
            pfkeys$(1)=hex(000102060d0f10ff08ffffffffffffffff0e)
            pfkeys$(2)=hex(0001020304050607080b0c0dff0f101cff0e)

            screen$(1,2)="                                               ~
        ~                (15)Print Screen"
            screen$(2,1)="(1)Start Over    (4)Prev Lines  (6)Down One    ~
        ~(11)Insert Line (13)Instructions"
            screen$(2,2)="(2)First Lines   (5)Next Lines  (7)Up One      ~
        ~(12)Delete Line (15)Print Screen"

            for i% = 1% to dim(seq$(),1)
                convert i% to seq$(i%), pic(####)
                str(seq$(i%),5) = ")"
            next i%

            call "TXTFUTIL" (#15, f2%(15), "INTL", " ")

            readkey$ = "SWITCHS.COR"
            call "READ100" (#2, readkey$, corebank%)     /* SYSFILE2 */
               if corebank% = 0% then L09660
            get #2 using L09647, gltype$
L09647:         FMT POS(135), CH(1)
            if gltype$ <> "Y" then corebank% = 0%
            if corebank% <> 0% then jbcorewipp$ = "Core WIP Account"

L09660:     readkey$ = "SWITCHS.SFC"
            call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) = 0% then inputmode
            get #2, using L09700, picksl_order$, rwkpip$
L09700:     FMT POS(31), CH(1), POS(70), CH(1)

            call "VBKSWTCH" ("VBK_ACTV", vsa_actv$, vsa_actv, ret%)
            call "VBKSWTCH" ("VBK_GEN ", vsa_gen$ , vsa_gen , ret%)
        vsa_actv$, vsa_gen$ = "Y"
            vsa_actv = vsa_actv  :  vsa_gen = vsa_gen

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Input mode main program.                                  *~
            *************************************************************

        inputmode
            tgl% = 1%
            errormsg$ = " "

        inputmode_special
            call "ALLFREE"
            call "JBINUSE" (" ", 1%)  /* Clears In-use Flag, if there. */
            inpmessage$, jbdescr$, job$, rteid$, dateclosed$, part$,     ~
            partdescr$, plenddate$, plstdate$, qtycomp$, qtymake$,       ~
            readkey$, stdate$, wipacct$, wipdescr$, qtyorig$, qtyscrp$,  ~
            qtyrewk$, qtyleft$, qtyadj$, wipsave$, bomid$, control$,     ~
            estperccomp$, jbcorewip$, jbcorewipd$, uom$, vendor$, po$,   ~
            poline$, disp1$, disp2$, disp3$, disp4$, type$, ecrpfk$ = " "

            init(" ") steps$(), outdate$(), rteloaded$, part$(),         ~
            partdescr$(), quantity$(), wcstep$(), textid$, wcact$(),     ~
            wcdate$(), wcrun$(), wctype$(), rte$(), wcsu$(), wcwc$(),    ~
            wcwcdescr$(), wcpct$(),            last_mod_by$, last_mod_at$

            str(pfkeys$(1), 18) = hex(0e) : str(pfkeys$(2), 18) = hex(0e)
            wcfill$() = all(hex(00))  :  wcnet$()  = all(hex(ff))

            init(hex(9c)) vfac$()

            rtop%, found%, qtyorig, qtyscrp, qtyrewk, qtymake, qtycomp,  ~
            copy%, stdate%, enddate%, maxlines%, maxwcout%, estperccomp, ~
            pipouts_changed%, wcouts_changed%, edit%, start_qtymake = 0%

            mat quantity    = zer
            mat outdate%    = zer
            mat newquantity = zer
            mat newdate%    = zer
            mat wcnet%      = zer
            mat wcseq%      = zer

            call "TXTFUTIL" (#15, f2%(15), "INTL", " ")

L10170:     for fieldnr% = 1% to 13%
                gosub'051(fieldnr%)
                     if enabled% = 0% then L10215
L10185:         gosub'101(fieldnr%)
                     errormsg$ = " "
                     if keyhit%  =  1% then gosub startover
                     cp% = 0%
                     if keyhit%  =  3% then gosub copy_job
                     if cp% = 1% then L10170
                     if keyhit%  = 16% and fieldnr% = 1 then exit_program
                     if keyhit% <>  0% then       L10185
L10215:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10185
            next fieldnr%

            gosub inputmode_capacity
L10240:     gosub inputmode_kit_list
L10245:     goto  editmode


        inputmode_kit_list
            call "DATUFMTC" (plstdate$)
            lastdate$ = plstdate$
            call "DATFMTC" (plstdate$)
            call "DATEFMT" (lastdate$)
            pbas%, screenline%, c% = 0%

L10280:     screenline% = screenline% + 1%
            if screenline% < 13% then L10300
                pbas% = pbas% + 1%
                screenline% = 12%
L10300:     c% = pbas%+screenline%
            if maxlines% > pok%-2% then edit_kit_list
            gosub input_kit_list
            if keyhit% <> 16% then L10280 else return

        input_kit_list
            inpmessage$  = " "
            for fieldnr% = 1% to 4%
                if fieldnr% = 4% then outdate$(c%) = lastdate$
                if fieldnr% = 2% then L10405
L10350:         gosub'201(screenline%,fieldnr%)
                     if keyhit% =   1% then gosub startover
                     if keyhit% =   2% then       columnone1
                     if keyhit% =   6% then gosub lineabove1
                     if keyhit% <> 14% then L10385
                          gosub load_bom
                          return clear all
                          goto edit_kit_list
L10385:              if keyhit% =  16% and fieldnr% = 1% then return
                     if keyhit% <>  0% and keyhit% <> 6% then L10350
                gosub'153(c%,fieldnr%)
                     if errormsg$<>" " then L10350
L10405:     next fieldnr%
            maxlines% = maxlines% + 1%
            return

        columnone1
            init (" ") part$(c%), partdescr$(c%,1%), partdescr$(c%,2%),  ~
                       quantity$(c%), outdate$(c%), errormsg$
            goto input_kit_list

        lineabove1
            if c% = 1% then return
                on fieldnr% goto L10470, L10475, L10480, L10485
                return
L10470:              part$(c%)         = part$(c%-1%)         : return
L10475:              partdescr$(c%,1%) = partdescr$(c%-1%,1%)
                     partdescr$(c%,2%) = partdescr$(c%-1%,2%) : return
L10480:              quantity$(c%)     = quantity$(c%-1%)     : return
L10485:              outdate$(c%)      = outdate$(c%-1%)      : return

        inputmode_capacity
            cbas%, screenline%, c% = 0%
L10505:     if errormsg$ = " " then screenline% = screenline% + 1%

            if screenline% < 13% then L10530
                cbas% = cbas% + 1%
                screenline% = 12%
L10530:     c% = cbas% + screenline%

            if maxwcout% > cok% then L10555
            gosub input_capacity
            maxwcout% = maxwcout% + 1%
            if keyhit% <> 16% then L10505
L10555:         maxwcout% = maxwcout% - 1%
                gosub columnone2
                return

        input_capacity
L10580:     for fieldnr% = 1% to 6%
                gosub'054(fieldnr%)
                     if enabled% = 0% then L10635
L10595:         gosub'207(screenline%,fieldnr%)
                     if keyhit% =   1% then gosub startover
                     if keyhit% <>  2% then L10616
                           gosub columnone2
                           goto L10580
L10616:              if keyhit% =   6% then gosub lineabove2
                     if maxwcout% > 0% then L10626
                     if keyhit% <> 14% then L10626
                          gosub append_route
                          if errormsg$<>" " then L10595
                          errormsg$ = "Route Appended Successfully - see ~
        ~results using PF-3 on header screen"
                          return clear all
                          goto L10240  /* goto input mode for kit list */
L10626:              if keyhit% =  16% then return
                     if keyhit% <>  0% and keyhit% <> 6% then L10595
L10635:         gosub'154(c%,fieldnr%)
                     if errormsg$<>" " then L10595
            next fieldnr%

            return

        columnone2
            wcact$(c%), wcdate$(c%), wcrun$(c%), wcstep$(c%), wcsu$(c%), ~
            wcwc$(c%), wcwcdescr$(c%), wcfill$(c%), wctype$(c%),         ~
            wcpct$(c%), errormsg$ = " "
            wcseq%(c%) = 0%
            return

        lineabove2
            if c% = 1% then return
            on fieldnr% goto L10720, L10725, L10730, L10735, L10740, L10745
            return

L10720:     wctype$(c%) = wctype$(c%-1%) : return
L10725:     wcdate$(c%) = wcdate$(c%-1%) : return
L10730:     wcstep$(c%) = wcstep$(c%-1%) : return
L10735:     wcwc$(c%)   = wcwc$(c%-1%)   : return
L10740:     wcact$(c%)  = wcact$(c%-1%)  : return
L10745:     wcsu$(c%)   = wcsu$(c%-1%)
            wcrun$(c%)  = wcrun$(c%-1%)  : return

        copy_job
            cp% = 1%
            copy% = 0%
            readkey$  = hex(06) & "Pick Job To Copy, PF16 To Cancel"
            errormsg$ = hex(84) & "Copy Request Cancelled"
            savejob$  = job$
            job$      = " "
            fieldnr% = fieldnr% - 1%
            call "GETCODE" (#3, job$, readkey$, 0%, 0, f1%(3))
            if f1%(3%) <> 0% then L10808
L10800:        job$ = savejob$
               return
L10808
*          IF STR(JOB$,,2%) <> "RW" THEN 10820
*             ERRORMSG$ = "Sorry, cannot copy rework job"
*             GOTO 10806
            get #3 using L10824, part$
L10824:         FMT POS(58), CH(25)
            gosub check_for_restrictions
              if errormsg$ = " " then L10848
                 part$ = " "
                 goto L10800

L10848:         errormsg$ = " "
                fieldnr% = fieldnr% + 1%
                copy% = 1%
                gosub dataload
                fromjob$ = job$ : job$ = savejob$
                copy% = 0%
                cp% = 0%

*        Copy Text Into 'Memory', Creating new Ids...
            call "TXTFUTIL" (#15, f2%(15), "INTL", " ")  /*Just In Case*/
            if textid$ = " " then L10940
            if textid$ = hex(ffffffff) then L10940
            if textid$ = hex(00000000) then L10940
            /* GOTO 10932 to ALWAYS copy text */
            /* GOTO 10928 to NEVER  copy text */

            ask$()  = " "
            ask$(1) = "Press PF16 to copy text from job: " & fromjob$
            ask$(3) = "Press PF1 to omit copy of text to job: " & job$

L10923:     u3%  = 2%
            call "ASKUSER" (u3%, "* * * COPY TEXT * * *",                ~
                            ask$(1), ask$(2), ask$(3))
            if u3% = 16% then L10932
            if u3% <> 1% then L10923
               textid$ = " " : goto L10940

L10932:     call "TXTFUTIL" (#15, f2%(15), "COPY", textid$)

L10940:     wcouts_changed%, pipouts_changed% = 1%
            return clear all
            call "SERENABL" (part$, enabled%, u3%, #2, #4)
            if enabled% = 0% then editmode
               edit% = 1%
               fieldnr% = 5%
               gosub'051(fieldnr%)   /* Sets INPMESSAGE$ */
               errormsg$ = "Verify Current Job Quantity" &               ~
                            ", and select Serial #'s."
               goto editmode_sercopy /* and on to the ACCEPT stmnt */

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of edit mode for linear screens.        *~
            *************************************************************

        editmode
            inpmessage$ = "To edit, TAB to field and press RETURN. Only f~
        ~ields with TABs can be modified."
            edit% = 1%  /* welcome to edit mode */
L11090:     gosub'111(0%)
                  errormsg$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub edit_kit_list
                  if keyhit%  =  3% then gosub edit_capacity
                  if keyhit%  =  7% then                                 ~
                      call "POSTATUS" (vendor$, po$, poline$, 1%,        ~
                                       #28, #29, #30, #31, #32, #15)
                  if keyhit%  =  8% then                                 ~
                          call "JBQJOB" (job$, #3,#2,#4,#17,#24,#25,#27)
                  if keyhit%  = 25% then gosub edit_text
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L11090
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 2% or fieldnr% > 12% then L11090
            if fieldnr% = 4%                   then L11090
            if cursor%(2) > 37% then fieldnr% = 13%

            gosub'051(fieldnr%)
                if enabled%  = 0% then editmode
                if fieldnr% <> 5% then L11280
                     gosub check_for_restrictions
                     if errormsg$ <> " " then editmode
        editmode_sercopy
L11280:     gosub'111(fieldnr%)
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11280
            gosub'151(fieldnr%)
                if errormsg$ <> " " then L11280
                goto editmode


        REM *************************************************************~
            *               E D I T   K I T   L I S T                   *~
            *-----------------------------------------------------------*~
            * Handles operation of edit mode for PIPOUT line screen     *~
            *************************************************************

        edit_kit_list
            pbas% = 0%
        cont_edit_kit
            c%, screenline% = 0%
            pbas% = max(0%,min(pbas%,maxlines%-12%))
L12110:     inpmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Field And Press RETURN."
            errormsg$=" "

L12150:     gosub'202(0%,0%)
                  if keyhit% =  1% then gosub startover
                  if keyhit% =  2% then pbas% = 0%
                  if keyhit% =  3% then pbas% = pok%
                  if keyhit% =  4% then pbas% = pbas% - 12%
                  if keyhit% =  5% then pbas% = pbas% + 12%
                  if keyhit% =  6% then pbas% = pbas% - 1%
                  if keyhit% =  7% then pbas% = pbas% + 1%
                  pbas% = max(0%,min(pbas%,maxlines%-12%))
                  if keyhit% = 11% then insertline1
                  if keyhit% = 14% then gosub load_bom
                  if keyhit% = 28% then gosub delete_all1
                  if keyhit% = 16% then editmode
                  if keyhit% <> 0% and keyhit% <> 12% then L12150

            screenline%=cursor%(1)-7%
            if screenline%<1% or screenline%>12% then cont_edit_kit
            c%=screenline%+pbas%
            if c%>maxlines% then cont_edit_kit
            if keyhit%=12% then deleteline1
            fieldnr%=0%
            if cursor%(2)>60% then fieldnr%=3%
            if cursor%(2)>71% then fieldnr%=4%
            if fieldnr%=0% then L12470
            inpmessage$ = " "
L12400:     gosub'202(screenline%,fieldnr%)
                if keyhit% =  1% then gosub startover
                if keyhit% <> 0% then L12400
            gosub'153(c%,fieldnr%)
            if errormsg$<>" " then L12400
            goto L12110

L12470:     for fieldnr%= 3% to 4%
L12480:     gosub'202(screenline%,fieldnr%)
                if fieldnr% = 0% and keyhit% = 16% then editmode
                if keyhit% =  1% then gosub startover
                if keyhit% <> 0% then L12480
            gosub'153(c%,fieldnr%)
            if errormsg$<>" " then L12480
            next fieldnr%
            goto L12110

        insertline1
            if maxlines%>pok%-2% then cont_edit_kit
            screenline%=min(13, max(cursor%(1)-6,1))
            if screenline%<1% or screenline%>13% then cont_edit_kit
L12600:     c%=min(screenline%+pbas%,maxlines%+1)
            if screenline% > 12% then pbas% = max(0, c% - 12)
            if screenline% > 12% then screenline% = min(12,c%)
            if c% <= maxlines% then L12680
            screenline% = c%-pbas%
            if c%>pok%-2% then cont_edit_kit
            goto L12790

L12680:     for i%=maxlines% to c% step -1
                part$     (i%+1%)    = part$     (i%)
                partdescr$(i%+1%,1%) = partdescr$(i%,1%)
                partdescr$(i%+1%,2%) = partdescr$(i%,2%)
                quantity$ (i%+1%)    = quantity$ (i%)
                outdate$  (i%+1%)    = outdate$  (i%)
                quantity  (i%+1%)    = quantity  (i%)
                outdate%  (i%+1%)    = outdate%  (i%)
             newquantity  (i%+1%)    = newquantity  (i%)
                newdate%  (i%+1%)    = newdate%  (i%)
            next i%

L12790:         init (" ") part$(c%), partdescr$(c%,1%),                 ~
                           partdescr$(c%,2%), quantity$(c%), outdate$(c%)
                newdate%(c%),outdate%(c%)=0%
                newquantity(c%),quantity(c%)=0%

            gosub input_kit_list
            if maxlines%>pok%-2% then cont_edit_kit
            if keyhit%=16% then L12930
            screenline%=screenline%+1%
            if screenline%<13% then L12600
                pbas%=pbas%+12%
                screenline%=1%
            goto L12600

L12930:     if c% > maxlines% then cont_edit_kit
            for i%=c% to maxlines%
                part$     (i%)     = part$     (i%+1%)
                partdescr$(i%,1%)  = partdescr$(i%+1%,1%)
                partdescr$(i%,2%)  = partdescr$(i%+1%,2%)
                quantity$ (i%)     = quantity$ (i%+1%)
                outdate$  (i%)     = outdate$  (i%+1%)
                quantity  (i%)     = quantity  (i%+1%)
                outdate%  (i%)     = outdate%  (i%+1%)
             newquantity  (i%)     = newquantity  (i%+1%)
                newdate%  (i%)     = newdate%  (i%+1%)
            next i%
                init (" ") part$(i%+1%), partdescr$(i%+1%,1%),           ~
                           partdescr$(i%+1%,2%), outdate$(i%+1%),        ~
                           quantity$(i%+1%)
                newdate%(i%+1%), outdate%(i%+1)=0%
                newquantity(i%+1%), quantity(i%+1)=0%
                goto cont_edit_kit

        deleteline1
            gosub'201(screenline%,5%)
                if keyhit%<>0 then cont_edit_kit
            quantity$(c%)=" "
            gosub'153(c%,3%)
            goto cont_edit_kit

        delete_all1
                keyhit% = 2%
                ask$()  = " "
                ask$(1) = hex(8c) &                                      ~
            "Press RETURN to clear all lines for re-entry or deletion" & ~
                          hex(84)
                ask$(2) = hex(8c) &                                      ~
            "Note that NOTHING is permanently changed for the job " &    ~
            "until SAVED" & hex(84)
                ask$(3) = "Press Any PF Key To Return To Edit"

                call "ASKUSER" (keyhit%, "Delete it ?",                  ~
                                ask$(1), ask$(2), ask$(3))

                if keyhit% <> 0% then return
                pipouts_changed% = 1%
                init (" ") part$(), partdescr$(), quantity$(), outdate$()
                mat newdate% = zer
                mat outdate% = zer
                mat newquantity = zer
                mat quantity = zer
                maxlines%, pbas% = 0%
                return

        REM *************************************************************~
            *          E D I T   C A P A C I T Y   S C R E E N          *~
            *-----------------------------------------------------------*~
            * Handles operation of edit mode for WCOUT line screen      *~
            *************************************************************

        edit_capacity
            cbas% = 0
        cont_edit_cap
            c%, screenline% = 0
            cbas% = max(0%,min(cbas%,maxwcout%-12))
L14110:     inpmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Field And Press RETURN."
            errormsg$=" "

L14150:     gosub'208(0%,0%)
                  if keyhit% =  1 then gosub startover
                  if keyhit% =  2 then cbas% = 0
                  if keyhit% =  3 then cbas% = cok%
                  if keyhit% =  4 then cbas% = cbas% - 12
                  if keyhit% =  5 then cbas% = cbas% + 12
                  if keyhit% =  6 then cbas% = cbas% - 1
                  if keyhit% =  7 then cbas% = cbas% + 1
                  cbas% = max(0%,min(cbas%,maxwcout%-12%))
                  if keyhit% = 11% then insertline2
                  if keyhit% = 28% then gosub delete_all2
                  if keyhit% = 16% then editmode
                  if maxwcout% > 0% then L14270
                  if keyhit% <> 14% then L14270
                    gosub append_route
                    goto L14150 /* display the resulting wcouts for edit*/
L14270:           if keyhit% <> 0% and keyhit% <> 12% then L14150

            screenline% = cursor%(1) - 7%
            if screenline% < 1% or screenline% > 12% then cont_edit_cap
            c% = screenline%+cbas%
            if c% > maxwcout% then cont_edit_cap
            if keyhit% = 12% then deleteline2
            fieldnr% = 0%
            if cursor%(2) >  7% then fieldnr% = 1%
            if cursor%(2) > 10% then fieldnr% = 2%
            if cursor%(2) > 19% then fieldnr% = 3%
            if cursor%(2) > 27% then fieldnr% = 4%
            if cursor%(2) > 55% then fieldnr% = 5%
            if cursor%(2) > 60% then fieldnr% = 6%
            ltemp%, ltemp1% = fieldnr%
            if fieldnr% <> 0 then L14450
                ltemp% = 1% : ltemp1% = 6%

L14450:     for fieldnr% = ltemp% to ltemp1%
            gosub'054(fieldnr%)
                if enabled% = 0% then L14510
L14480:     gosub'208(screenline%,fieldnr%)
                if keyhit% =  1% then gosub startover
                if keyhit% <> 0% then L14480
L14510:     gosub'154(c%,fieldnr%)
            if errormsg$<>" " then L14480
            next fieldnr%
            goto L14110

        insertline2
            if maxwcout% > cok%-1 then cont_edit_cap
            screenline% = min(13, max(cursor%(1)-6,1))
            if screenline%<1 or screenline%>13 then cont_edit_cap
L14600:     c%=min(screenline%+cbas%,maxwcout%+1)
            if screenline% > 12 then cbas% = max(0, c% - 12)
            if screenline% > 12 then screenline% = min(12,c%)
            if c% <= maxwcout% then L14680
            screenline% = c% - cbas%
            if c% > cok%-1 then cont_edit_cap
            goto L14820

L14680:     for i% = maxwcout% to c% step -1%
                wcdate$   (i%+1) = wcdate$   (i%)
                wcfill$   (i%+1) = wcfill$   (i%)
                wcpct$    (i%+1) = wcpct$    (i%)
                wctype$   (i%+1) = wctype$   (i%)
                wcseq%    (i%+1) = wcseq%    (i%)
                wcstep$   (i%+1) = wcstep$   (i%)
                wcwc$     (i%+1) = wcwc$     (i%)
                wcwcdescr$(i%+1) = wcwcdescr$(i%)
                wcact$    (i%+1) = wcact$    (i%)
                wcsu$     (i%+1) = wcsu$     (i%)
                wcrun$    (i%+1) = wcrun$    (i%)
            next i%

L14820:     gosub columnone2
            maxwcout% = maxwcout% + 1
            if maxwcout% > cok% then L14930
            gosub input_capacity
            if keyhit% = 16 then L14930
            screenline% = screenline% + 1
            if screenline% < 13 then L14600
                cbas% = cbas% + 1
                screenline% = 12
            goto L14600

L14930:     maxwcout% = maxwcout% - 1
L14940:     if c% > maxwcout% then L15080
            for i% = c% to maxwcout%
                wcdate$   (i%) = wcdate$   (i%+1)
                wcfill$   (i%) = wcfill$   (i%+1)
                wcpct$    (i%) = wcpct$    (i%+1)
                wctype$   (i%) = wctype$   (i%+1)
                wcseq%    (i%) = wcseq%    (i%+1)
                wcstep$   (i%) = wcstep$   (i%+1)
                wcwc$     (i%) = wcwc$     (i%+1)
                wcwcdescr$(i%) = wcwcdescr$(i%+1)
                wcact$    (i%) = wcact$    (i%+1)
                wcsu$     (i%) = wcsu$     (i%+1)
                wcrun$    (i%) = wcrun$    (i%+1)
            next i%
L15080:         c% = maxwcout% + 1
                gosub columnone2
                goto cont_edit_cap

        deleteline2
            gosub'207(screenline%,7%)
                if keyhit%<>0 then cont_edit_cap
            wcouts_changed% = 1%
            maxwcout% = maxwcout% - 1%
            if maxwcout% = 0% then str(pfkeys$(2),18) = hex(0e)
            goto L14940

        delete_all2
                keyhit% = 2
                ask$()  = " "
                ask$(1) = hex(8c) &                                      ~
        "Press RETURN to clear all lines for re-entry or deletion" &     ~
                          hex(84)
                ask$(2) = hex(8c) &                                      ~
        "Note that NOTHING is permanently changed for the job until " &  ~
        "SAVED" & hex(84)
                ask$(3) = "Press Any PF Key To Return To Edit"

                call "ASKUSER" (keyhit%, "Delete it ?",                  ~
                                ask$(1), ask$(2), ask$(3))

                if keyhit% <> 0 then return
                wcact$(), wcdate$(), wcrun$(), wcstep$(), wcsu$(),       ~
                wcpct$(), wcwc$(), wcwcdescr$(), wcfill$(), wctype$()=" "
                mat wcseq% = zer
                maxwcout%, cbas% = 0
                wcouts_changed% = 1
                str(pfkeys$(2),18) = hex(0e)
                return

        REM *************************************************************~
            *                     E D I T   T E X T                     *~
            *-----------------------------------------------------------*~
            * Enter / edit job text                                     *~
            *************************************************************

        edit_text
            textmsg$ = "Text for Job Number '" & job$ & "'"
            call "TXTINSUB" (#15, f2%(15), "021", textmsg$, textid$,     ~
                                                            texta$())
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after input/editing.                   *~
            *************************************************************

        datasave
            gosub dataput    /* Save/Resave Job Master Datas */
            gosub save_serial_numbers
            gosub update_pips    /* Save Kit List */
            gosub update_wcs     /* Save Capacity Allocations    */
            if qtymake_changed% = 1% then                                ~
                call "JBVSASUB" (#2, #13, "RES", job$, part$, qtymake)
            lastjob$ = job$
            goto inputmode

        save_serial_numbers
            readkey$ = job$
            call "SERSAVE"                                               ~
                       (1%,              /* Line Item Pointer.         */~
                        "WP",            /* Source Transaction Type    */~
                        readkey$,        /* Source Transaction Key     */~
                        1%,              /* # Trans to Create File for */~
                        part$,           /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        "1",             /* Change Status to ...       */~
                        "6",             /* Change Status from ...     */~
                        1%,              /* Clear TIF after Save (YES) */~
                        #2,              /* SYSFILE2 UFB               */~
                        #20,             /* SERTIF UFB                 */~
                        #21,             /* SERMASTR UFB               */~
                        #22)             /* SERWORK  UFB               */
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                enabled% = 1%
                inpmessage$ = " "
                on fieldnr%  gosub  L20220,         /* Job Number       */~
                                    L20240,         /* Control Number   */~
                                    L20270,         /* Description      */~
                                    L20310,         /* Part Number      */~
                                    L20360,         /* Quantity to Make */~
                                    L20430,         /* WIP Acct         */~
                                    L20480,         /* Planned Start    */~
                                    L20530,         /* Planned End      */~
                                    L20570,         /* Actual Start     */~
                                    L20620,         /* BOM ID           */~
                                    L20780,         /* RTE ID           */~
                                    L20950,         /* Core WIP (Maybe) */~
                                    L20990          /* Est. Perc. Comp. */
                return

L20220
*        Default/Enable for JOB NUMBER
            inpmessage$ = "Enter Job Number To Input/Edit, Or Leave" &   ~
                          " Blank To Search For Existing Job."
            return

L20240
*        Default/Enable for CONTROL NUMBER
            inpmessage$ = "Optionally, enter a code to relate this"  &   ~
                          " job to others or a sales order number"
            return

L20270
*        Default/enable for DESCRIPTION
            inpmessage$ = "Enter Text Job Can Be Referenced By."
            return

L20310
*        Default/enable for PART NUMBER
            inpmessage$ = "Leave Blank And Press RETURN To See Parts" &  ~
                          " On File."
            return

L20360
*        Default/enable for QUANTITY TO MAKE
            inpmessage$ = "Enter Quantity To Build. Prefix w/ 'P' to" &  ~
                          " honor MOQ & PANSIZE."
            if str(job$,,2) = "RW" then enabled% = 0%
            if enabled% > 0% then call "STRING" addr("LJ", qtymake$, 10%)
            return

L20430
*        Default/enable for WIP ACCT
            inpmessage$ = "Enter G/L Work In Process Account Number."
            if wipacct$ = " " then wipacct$ = wipsave$
            return

L20480
*        Default/enable for PLANNED START
            inpmessage$ = "Enter Date that Job is Planned to Start."
            if plstdate$ = " " or plstdate$ = blankdate$ then plstdate$, plstdate8$ = date
            call "DATEOKC" (plstdate$, 0%, " ")
            return

L20530
*        Default/enable for PLANNED FINISH
            inpmessage$ = "Enter Date that Job is Planned To End."
            return

L20570
*        Default/enable for ACTUAL START
            inpmessage$ = "Enter Date that Job Actually Started."
            if stdate$ = " " or stdate$ = blankdate$ then stdate$ = plstdate$
            return

L20620
*        Default/enable for BOM ID
            if bomid$ <> " " then L20740
                effdate$ = plstdate$ /* Get DATEINDEX% per plan start  */
                call "DATUNFMT" (effdate$)
                call "PIPINDEX" (#2, effdate$, dateindex%, u3%)
                readkey$ = str(part$,,25) & "1" & hex(00)
                call "PLOWNEXT" (#12, readkey$, 26%, f1%(12))
                if f1%(12) <> 1% then L20740
                  gosub get_bom_string

                     if bom$(dateindex%) = " " then L20740
                     bomid$ = bom$(dateindex%)
L20740:     inpmessage$ = "Enter BOM Job will use. This is needed to"  & ~
                          " create a Materials List."
            return

L20780
*        Default/ enable for RTE ID
            if rteid$ <> " " then L20850
                readkey$ = str(part$,,25) & str(bomid$,,3) & "  0"
                call "READ100" (#11, readkey$, f1%(11))
                if f1%(11) = 0% then L20850
                     get #11, using L20840, rteid$
L20840:                   FMT POS(87), CH(3)
L20850:     inpmessage$ = "Enter RTE Job Will Use. This is needed to"  & ~
                          " create Capacity Allocations Data."
            return

L20950
*        Default/ enable for Core WIP Account
            if corebank% <> 0% then L20960
               jbcorewip$, jbcorewipd$ = " "
               enabled% = 0%
               return
L20960:     inpmessage$ = "Enter WIP Account for Core Values."
            return

L20990
*        Default/ enable for Estimated Percent Complete
            if edit% = 0% then enabled% = 0%
            inpmessage$ = "Enter An Estimated Percentage Of Complete"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   R O U T E       *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the rte interface    *~
            *************************************************************

            deffn'054(fieldnr%)
                  gosub load_rte
                  enabled% = 1%
                  inpmessage$, olddate$, oldwc$, oldsu$, oldrun$ = " "
                  on fieldnr% gosub L23180,         /* WCOUT REC TYPE   */~
                                    L23220,         /* DATE OUT         */~
                                    L23290,         /* STEP             */~
                                    L23380,         /* WC               */~
                                    L23550,         /* ACTIVITY CODE    */~
                                    L23650          /* SET UP & RUN HRS */

                     return
L23180:     REM DEFAULT/ENABLE FOR DETAIL RECORD TYPE
                inpmessage$ = "Type: Blank=Normal, MQ=Move/que, MA=Mq AFT~
        ~ER Step, C1-C3=Concurrent to prev. WC"
                return
L23220:     REM DEFAULT/ENABLE FOR WCOUT DATE
                olddate$ = wcdate$(c%)
                if c% = 1% then L23250
                if str(wctype$(c%),,1)="C" then wcdate$(c%)=wcdate$(c%-1)
L23250:         if str(wctype$(c%),,1)="C" then enabled% = 0%
                inpmessage$ = "Enter Date Activity Is To Occur On."
                if wcdate$() <> " " and wcdate$() <> blankdate$ then return
                   call "DATUFMTC" (plstdate$)
                   wcdate$(c%) = plstdate$
                   plstdate8$ = plstdate$
                   call "DATEFMT" (plstdate8$)
                   call "DATFMTC" (plstdate$)
                   call "DATEFMT" (wcdate$(c%))
                return
L23290:     REM DEFAULT/ENABLE FOR ROUTE STEP
                if c% = 1% then L23320
                if wctype$(c%) = "MA" then wcstep$(c%) = wcstep$(c%-1)
                if str(wctype$(c%),,1)="C" then wcstep$(c%)=wcstep$(c%-1)
L23320:         if str(wctype$(c%),,1)="C" then enabled% = 0%
                if wctype$(c%) = "MA" then enabled% = 0%
                if str(wctype$(c%),,1) = "C" then enabled% = 0%
                inpmessage$ = "Enter Route Step ID.  Make One Up To Indic~
        ~ate Diversion From Normal Routing."
                return
L23380:     REM DEFAULT/ENABLE FOR WORK CENTER
                oldwc$ = wcwc$(c%)
                if wcwc$(c%) <> " " then L23520
                if str(wctype$(c%),,1) = "C" then L23520
                if wcseq%(c%) = 0 then L23450
                   wcwc$(c%) = str(rte$(wcseq%(c%)),32)
                   goto L23510
L23450:         REM Search Details To See If Step Defined...
                for i% = 1% to maxwcout%
                     if i% = c% then L23500
                     if wcstep$(i%) <> wcstep$(c%) then L23500
                     wcwc$(c%) = wcwc$(i%) : i% = maxwcout%
L23500:         next i%
L23510:         if wcwc$(c%) <> " " then enabled% = 0%
L23520:         inpmessage$ = "Enter Work Center To Be Utilized."
                return

L23550
*        Default/ enable for ACTIVITY CODE
            if wcseq%(c%) <> 0% then enabled% = 0%
                inpmessage$ = "Enter Activity To Be Performed In Work" & ~
                              " Center (This Is Optional)."
                if wcact$(c%) <> " " then return
                if str(wctype$(c%),,1)="C" or wcseq%(c%)=0% then return
                if str(wctype$(c%),,1)<>"M" then L23620
                        enabled% = 0%
                        wcact$(c%) = " "
                        return
L23620:              wcact$(c%) = str(rte$(wcseq%(c%)),94,4)
                     return

L23650:     REM DEFAULT/ ENABLE SET UP & RUN HOURS
                oldsu$ = wcsu$(c%) : oldrun$ = wcrun$(c%)
                if str(wctype$(c%),,1) = "M" then enabled% = 0%
                inpmessage$ = "Enter HOURS To Be Spent In Set Up, And HOU~
        ~RS To Be Spent Processing."
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
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            call "SERSTOVR" (1%, "6", " ",  #21, #22)
            readkey$ = "WP" & job$
            call "DELETE" (#20, readkey$, 19%)   /* SERTIF for job */
            return clear all
            goto inputmode

        REM *************************************************************~
            *                     D A T A   L O A D                     *~
            *-----------------------------------------------------------*~
            * Load old job from file, if found                          *~
            *************************************************************
        dataload
            call "SHOSTAT" ("Loading Job Data")
            get #3, using L30110, jbdescr$, part$, qtymake, qtycomp,      ~
                                 jbcorewip$, vendor$, po$, poline$,      ~
                                 stdate$, dateclosed$,                   ~
                                 wipacct$, plstdate$, plenddate$,        ~
                                 qtyorig, qtyscrp, qtyrewk, textid$,     ~
                                 control$, estperccomp, last_mod_by$,    ~
                                 last_mod_at$
L30110:         FMT POS(9), CH(30), POS(58), CH(25), 2*PD(14,4), CH(9),  ~
                    CH(9), CH(16), CH(3),                                ~
                    POS(147), 2*CH(6), CH(9), 2*CH(6), PD(14,4),         ~
                    POS(212), 2*PD(14,4), CH(4), POS(1120), CH(19),      ~
                    POS(1139), PD(14,4), POS(1265), CH(3), CH(6)

            start_qtymake = qtymake

            if estperccomp = 0 then estperccomp$ = " "                   ~
               else convert estperccomp to estperccomp$, pic(#####.##)

            if copy% = 0% then L30200
                dateclosed$ = " " : /* TEXTID$ = " " to NEVER copy text */
                qtymake = qtyorig
                qtycomp, qtyscrp, qtyrewk, qtyadj = 0
                if str(savejob$,,2%) = "RW" then qtymake, qtyorig = 0
                estperccomp = 0 : estperccomp$ = " "

L30200:     qtyadj = -(qtyorig - qtyscrp - qtyrewk - qtymake)
            call "CONVERT" (qtymake, 0.2, qtymake$)
            call "CONVERT" (qtycomp, 0.2, qtycomp$)
            call "CONVERT" (qtymake - qtycomp, 0.2, qtyleft$)
            call "CONVERT" (qtyorig, 0.2, qtyorig$)
            call "CONVERT" (qtyscrp, 0.2, qtyscrp$)
            call "CONVERT" (qtyrewk, 0.2, qtyrewk$)
            call "CONVERT" (qtyadj , 0.2, qtyadj$)

            call "PIPINDEX" (#2, str(plstdate$,,6) , stdate%, u3%)
            call "PIPINDEX" (#2, str(plenddate$,,6), enddate%, u3%)

            plstdate8$ = plstdate$
            call "DATEFMT" (plstdate8$)
            call "DATFMTC" (plstdate$)
            call "DATFMTC" (plenddate$)
            call "DATFMTC" (stdate$)
            call "DATEFMT" (last_mod_at$)
            call "DESCRIBE" (#6, wipacct$, wipdescr$, 1%, f1%(6))
                 call "GLFMT" (wipacct$)
            if corebank%  = 0% then jbcorewip$ = " "
            if jbcorewip$ = " " then L30370
            call "DESCRIBE" (#6, jbcorewip$, jbcorewipd$, 1%, f1%(6))
                 call "GLFMT" (jbcorewip$)
L30370:     call "DESCRIBE" (#4, part$, partdescr$, 1%, f1%(4))

            call "READ100" (#4, part$, f1%(4%))
                if f1%(4%) = 0% then L30381
            get #4 using L30380, uom$, type$
L30380:         FMT POS(74), CH(4), POS(180), CH(3)
L30381:     if vendor$ & po$ & poline$ = " " then L30389
                disp1$ = "Vendor Code"
                disp2$ = vendor$ : call "STRING" addr("RJ", disp2$, 9%)
                disp3$ = "PO & Line"
                disp4$ = po$ & "  " & poline$
                call "STRING" addr("RJ", disp4$, 21%)
                vfac$(1%) = hex(8c) : vfac$(2%) = hex(84)
                goto L30400
L30389:     init(hex(9c)) vfac$()

L30400:     if dateclosed$ = " " or dateclosed$ = blankdate$ then L30510
                call "DATEFMT" (dateclosed$)
                keyhit% = 2%
                errormsg$ = hex(8c) & "This Job Was Closed On"
                errormsg$ = errormsg$ & hex(84) & dateclosed$
                ask$()  = " "
                ask$(1) = errormsg$
                ask$(2) = hex(8c) & "Press PF(8) To Re-open it" & hex(84)
                ask$(3) = "Press RETURN to manage another job"

                call "ASKUSER" (keyhit%, "Please Note",                  ~
                                ask$(1), ask$(2), ask$(3))

                if keyhit% = 8% then L30490
                  goto inputmode
L30490:         dateclosed$, errormsg$ = " "

L30510:     outtagnr$ = "JOB ORDER: " & str(job$)
            call "READ100" (#18, outtagnr$, f1%(18))
            if f1%(18) = 0% then L30560
                get #18, using L30550, rteid$, bomid$
L30550:              FMT XX(25), CH(3), XX(19), XX(25), CH(3)
L30560:     if copy% <> 0% then L30580
                call "TXTFUTIL" (#15, f2%(15), "LOAD", textid$)
L30580:     gosub load_pipouts
            gosub load_wcouts
            if copy% <> 0% then return
            call "SERLOAD"                                               ~
                       (1%,              /* Line Item Pointer.         */~
                        "WP",            /* Source Transaction Type    */~
                        job$,            /* Source Transaction Key     */~
                        1%,              /* # Trans to Create File for */~
                        "1",             /* Load from WIP only those   */~
                        job$,            /* S/N's that are in this Job.*/~
                        #2,              /* SYSFILE2 UFB               */~
                        #20,             /* SERTIF   UFB               */~
                        #21,             /* SERMASTR UFB               */~
                        #22, u3%)        /* SERWORK  UFB               */
*          Check for ECRs this Part
            ecrpfk$ = "(11)"
            call "ECRINQSB" ("C",        /* "C" to Check for ECR Info  */~
                                         /*    and return PFKey Prompt */~
                             part$,      /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                                         /* OUT: Formatted PFKey Prompt*/~
                                         /*    Will be BLANK if no ECRs*/~
                             #02,        /* SYSFILE2                   */~
                             #04)        /* HNYMASTR                   */
            return

        REM *************************************************************~
            *                        D A T A   P U T                    *~
            *-----------------------------------------------------------*~
            * Update, create job record and set pip to right data for   *~
            * planning system                                           *~
            *************************************************************
        dataput
            call "SHOSTAT"  ("Saving Job Record")
            if start_qtymake <> qtymake then qtymake_changed% = 1%       ~
                                        else qtymake_changed% = 0%
            call "DATUFMTC" (stdate$)
            call "DATUFMTC" (plstdate$)
            call "DATUFMTC" (plenddate$)
            call "GLUNFMT"  (wipacct$)
            call "GLUNFMT"  (jbcorewip$)
            last_mod_at$ = date
            last_mod_by$ = userid$

            outtagnr$ = "JOB ORDER: " & str(job$)

            call "READ101" (#3, job$, f1%(3))
            if f1%(3) = 0% then L31150
                put #3 using L31115, jbdescr$, qtymake, jbcorewip$,       ~
                                    stdate$, blankdate$, wipacct$, plstdate$, ~
                                    plenddate$, textid$, control$,       ~
                                    estperccomp, last_mod_by$,last_mod_at$
L31115:              FMT POS(9), CH(30), POS(83), PD(14,4),              ~
                         POS(99), CH(9), POS(147), 2*CH(6), CH(9),       ~
                         2*CH(6), POS(228), CH(4),                       ~
                         POS(1120), CH(19), PD(14,4),                    ~
                         POS(1265), CH(3), CH(6)
                rewrite #3
                goto L31194

L31150:         qtyorig = qtymake
                put #3 using L31470, job$, jbdescr$, outtagnr$, part$,    ~
                                    qtymake, qtycomp, jbcorewip$,        ~
                                    vendor$, po$, poline$, " ",          ~
                                    stdate$, blankdate$, wipacct$,       ~
                                    plstdate$, plenddate$,               ~
                                    qtyorig, " ", qtyscrp, qtyrewk,      ~
                                    textid$, zeroes$(), control$,        ~
                                    estperccomp, str(zeroes$(),,104),    ~
                                    " ", last_mod_by$, last_mod_at$, " "
                write #3

L31194:     if f1%(3) = 0% then call "CDANPOST" (#3, "A")                ~
                           else call "CDANPOST" (#3, "C")

            call "TXTFUTIL" (#15, f2%(15), "SAV2", " ")

        REM Synchronize VBLKINES If Necessary...
            if sync% = 0% then L31225
            po_openqty= qtymake - qtycomp - rcvholdqty - qcqty - qcholdqty
            readkey$ = str(vendor$) & str(po$) & str(poline$)
            call "READ101" (#28, readkey$, f1%(28%))
                if f1%(28%) = 0% then L31255
            put #28 using L31216, po_openqty, plenddate$
L31216:         FMT POS(109), PD(14,4), POS(142), CH(6)
            rewrite #28
            sync% = 0%

L31225:     readkey$ = "JOB ORDER: " & str(job$):qty = 0
            call "READ101" (#8, readkey$, f1%(8))
            if f1%(8) = 0% then L31260
                get #8, using L31245, temp%, qty
L31245:              FMT XX(25), BI(4), XX(19), PD(14,4)
                delete #8
L31255:         call "PIPFLAGS" (part$, 1%, temp%, -qty, #9, #10)

L31260:     newqty = 0
            if str(job$,,2%) <> "RW" then L31275
            if rwkpip$ <> "A" then L31280
L31275:        newqty = qtymake - qtycomp
L31280:     if abs(newqty) < .0001 then L31300
                call "PIPFLAGS" (part$, 1%, enddate%, newqty, #9, #10)
                write #8 using L31440, part$, enddate%, readkey$,         ~
                                      newqty, stdate%

L31300:     qty = newqty - qty
            if abs(qty) < .0001 then return
                call "HNYPST1"                                           ~
                     (part$,             /* PART TO BE UPDATED         */~
                      "001",             /* STORE CODE                 */~
                      " " ,              /* LOT NUMBER                 */~
                      0,0,               /*                            */~
                      qty,               /* QUANTITY + OR -            */~
                      0,0,               /*  1 = POST TO ON HAND       */~
                                         /*  2 =         B'ORD         */~
                                         /*  3 =         ON ORDER      */~
                                         /*  4 =         COMMITTED     */~
                                         /*  5 =         IN PROCESS    */~
                      #5,                /* UFB ADDRESS OF HNYQUAN     */~
                      #4,                /* UFB ADDRESS OF HNYMASTR    */~
                      #2,                /* UFB ADDRESS OF SYSFILE2    */~
                      f2%(5),            /* STATUS FLAG FOR HNYQUAN    */~
                      f2%(4),            /* STATUS FLAG FOR HNMASTR    */~
                      f2%(2),            /* STATUS FLAG FOR SYSFILE2   */~
                      0%,                /* WHETHER OR NOT TO PRINT AN */~
                                         /*  EXCEPTION REPORT WHEN A   */~
                                         /*  NEW HNYQUAN REC CREATED-  */~
                                         /*  1= PRINT, 0 = DON'T PRINT */~
                      err%)              /* ERROR RETURN FROM SUBROUTIN*/~
                                         /* 0  = RECORD POSTED         */
                return


L31440:         FMT  CH(25),             /* PART NUMBER                */~
                     BI(4),              /* DATE IN                    */~
                     CH(19),             /* TAG NUMBER                 */~
                     PD(14,4),           /* QUANTITY IN                */~
                     BI(4)               /* START DATE                 */

L31470:         FMT  CH(8),              /* Job Number                 */~
                     CH(30),             /* Job Description            */~
                     CH(19),             /* Tag to PIP                 */~
                     CH(25),             /* Part to Build              */~
                     PD(14,4),           /* Quantity to Make           */~
                     PD(14,4),           /* Quantity Complete          */~
                     CH(9),              /* Core WIP Account           */~
                     CH(9),              /* Vendor Number              */~
                     CH(16),             /* Purchase Order Number      */~
                     CH(3),              /* Purchase Order Line Number */~
                     CH(11),             /* Filler                     */~
                     CH(6),              /* Start Date                 */~
                     CH(6),              /* Close Date                 */~
                     CH(9),              /* WIP Account                */~
                     CH(6),              /* Planned Start              */~
                     CH(6),              /* Planned End                */~
                     PD(14,4),           /* Original Quantity to Make  */~
                     CH(24),             /* Filler                     */~
                     PD(14,4),           /* Quantity Scrapped          */~
                     PD(14,4),           /* Quantity Sent to Rework    */~
                     CH(4),              /* Text ID                    */~
                     8*CH(111),          /* Costs                      */~
                     CH(19),             /* Control Number             */~
                     PD(14,4),           /* Estimated Percent of Comp. */~
                     CH(104),            /* Zeroes                     */~
                     CH(14),             /* STC Stuff - Blanks for now */~
                     CH(3),              /* Last Modified By           */~
                     CH(6),              /* Last Modified Date         */~
                     CH(27)              /* Filler                     */

        REM *************************************************************~
            *        Read data from PIPOUT  (present kit list)          *~
            *-----------------------------------------------------------*~
            * Format data for display and edit.                         *~
            *************************************************************

        load_pipouts
            maxlines% = 0%
            outtagnr$ = "JOB ORDER: " & str(job$)
L32090:     call "PLOWNEXT" (#17, outtagnr$, 19%, f1%(17))
            if f1%(17) = 0% then L32350

            if maxlines% < pok% - 1% then L32230
                convert pok%-1% to temp$, pic(####)
                errormsg$ = "Sorry, Job " & job$
                errormsg$ = errormsg$ & " can't be managed here since" & ~
                            " it has over " & temp$ & " unissued parts."
                return clear
                if copy% = 0% then return
                     errormsg$ = "Warning: " & hex(84) &                 ~
                                 " Too many unissued parts, only " &     ~
                                 temp$ & " copied."
                     goto L32350
L32230:     i%, maxlines% = maxlines% + 1%
            if picksl_order$ = "B" then L32300
            get #17, using L32260, part$(i%), outdate%(i%), quantity(i%)
L32260:         FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
            gosub L32480
            goto L32090

L32300:     get #17, using L32320, str(partdescr$(i%,1%),9,29),           ~
                                  str(partdescr$(i%,1%),,8),             ~
                                  str(partdescr$(i%,1%),38,8)
L32320:         FMT XX(19), CH(29), CH(8), CH(8)
            goto L32090

L32350:     REM Sort Pick List Into BOM Order via Time Stamp...
            if picksl_order$ <> "B" or maxlines% = 0% then return
            call "SORT" addr(str(partdescr$(),1), i%, 90%,               ~
                                       str(partdescr$(),1), 1%, 8%, "A")
            for i% = 1% to maxlines%
                get partdescr$(i%,1%), using L32430, part$(i%),           ~
                                                    outdate%(i%),        ~
                                                    quantity(i%)
L32430:         FMT XX(8), CH(25), BI(4), PD(14,4)
                gosub L32480
            next i%
            return

L32480:     REM Format la PIPOUT datas...
            call "DATE" addr("G+", bdate$, outdate%(i%)-1%,              ~
                                                str(outdate$(i%),,6), 0%)
            call "DATEFMT" (outdate$(i%))
            call "DESCRIBE" (#4, part$(i%), partdescr$(i%,1%), 0%,       ~
                                                                f1%(4%))
            partdescr$(i%,2%) = "????"
            if f1%(4%) = 1% then get #4 using L32528, partdescr$(i%,2%)
L32528:         FMT POS(74), CH(4)
            call "CONVERT" (quantity(i%), 0.2, quantity$(i%))
            newdate%(i%)    = outdate%(i%)
            newquantity(i%) = quantity(i%)
            return

        REM *************************************************************~
            *    Read data from WCOUT (present capacity allocation)     *~
            *-----------------------------------------------------------*~
            * Format data for display and edit.                         *~
            *************************************************************

        load_wcouts
            maxwcout%, c%, vend% = 0%
            outtagnr$ = "JOB ORDER: " & str(job$)
            str(outtagnr$,20) = all(hex(00))

L33055:     call "PLOWNEXT" (#13, outtagnr$, 19%, f1%(13))
            if f1%(13) <> 0% then L33150
                if vend% = 0% or vsa_actv$ = "N" then return
*        Check for any released Vendor Service Advices
                plowkey$ = str(job$,,8) & hex(00)
            vbkvsa_loop
                call "PLOWALTS" (#7, plowkey$, 3%, 8%, f1%(7))
                if f1%(7) = 0% then return
                     get #7 using L33100, vsa_status$
L33100:                   FMT CH(1)
                     if vsa_status$ = "O" then vbkvsa_loop
                          keyhit% = 0%
                          call "ASKUSER" (keyhit%,                       ~
                               "Released Service Advices",               ~
                               "This job has service advices that have", ~
                               "been released.  These advices will not", ~
                               "reflect any changes made.     <RETURN> ")
                          return

L33150:     if maxwcout% < cok% then L33185
                convert cok% to temp$, pic(####)
                errormsg$ = "Sorry, Job " & job$
                errormsg$ = errormsg$ & " can't be managed here since" & ~
                            " it has over " & temp$ & " capacity details."
                return clear
                return
L33185:     i%, maxwcout% = maxwcout% + 1%
            get #13 using L33200, wcnet$, wcseq%(i%), wcsu, wcrun,        ~
                        wcstep$(i%), wcfill$(i%), wctype$(i%), wcact$(i%)
L33200:         FMT CH(6), BI(2), XX(23), 2*BI(4), CH(5), CH(2),         ~
                                                             CH(1), CH(4)
            get wcnet$ using L33215, wcwc$(i%), day%
L33215:         FMT CH(4), BI(2)
            if str(wcnet$,,4) = "VEND" and wcact$(i%) <> " " then vend%=1%

*        Save off intial allocations to minimize datasave efforts
            if copy% = 1% then L33280
            search str(wcnet$(),,(c%+1)*8) = str(wcnet$,,6) to cursor%() ~
                                                                   step 8
            if cursor%(1) > 0% then y% = (cursor%(1)+7%)/8%              ~
                               else y% = c%+1%
            wcnet$(y%) = str(wcnet$) & bin(y%,2)
            wcnet%(y%) = wcnet%(y%) - (wcsu + wcrun)
            c% = max(c%, y%)

L33280:     temp% = val(str(wcstep$(i%),5,1),1)
            str(wcstep$(i%),5,1) = " "
            if temp% = 0% then L33310
                wcstep$(i%) = wcstep$(i%) & "-"
                convert temp% to str(wcstep$(i%),pos(wcstep$(i%)="-")+1),~
                                                                  pic(00)
L33310:     wcadj% = 0%
            if copy% = 1% then wcadj% = int(wcsu + wcrun)
            gosub'99(wcwc$(i%), day%, wcadj%)
            wcpct$(i%) = temp$

            call "DATE" addr("G+",bdate$,day%-1%,str(wcdate$(i%),,6),0%)
            call "DATEFMT" (wcdate$(i%))
            call "DESCRIBE" (#14, wcwc$(i%), wcwcdescr$(i%), 0%, f1%(14))
            call "WCUN2HRS" (#14, wcwc$(i%), 0, wcsu, " ")
            call "CONVERT" (wcsu, 0.2, wcsu$(i%))
            call "WCUN2HRS" (#14, wcwc$(i%), 0, wcrun, " ")
            call "CONVERT" (wcrun, 0.2, wcrun$(i%))
            wcseq%(c%) = wcseq%(c%)/100
            if wctype$(i%) > hex(09) then wcseq%(c%) = 0%
            wctype$ = " "
            if wctype$(i%) = hex(00) then wctype$ = "MQ"
            if wctype$(i%) = hex(0a) then wctype$ = "MQ"
            if wctype$(i%) = hex(02) then wctype$ = "C1"
            if wctype$(i%) = hex(0c) then wctype$ = "C1"
            if wctype$(i%) = hex(03) then wctype$ = "C2"
            if wctype$(i%) = hex(0d) then wctype$ = "C2"
            if wctype$(i%) = hex(04) then wctype$ = "C3"
            if wctype$(i%) = hex(0e) then wctype$ = "C3"
            if wctype$(i%) = hex(09) then wctype$ = "MA"
            if wctype$(i%) = hex(13) then wctype$ = "MA"
            wctype$(i%) = wctype$
            goto L33055

        deffn'99(temp$, temp%, newused%)
            percent = 0
            if temp$ = lastget$ then L33500
            lastget$ = temp$
            call "READ100" (#14, lastget$, f1%(14))
                if f1%(14) <> 0% then L33490
                lastget$ = hex(abcdef)
                goto L33530
L33490:     get #14, using L33495, wcwork$()
L33495:         FMT POS(60), 10*CH(196)
L33500:     if day% < 1% or day% > 490% then return
                dayavail% = val(str(wcwork$(),temp%*2-1,2),2)
                dayused% = val(str(wcwork$(),979+temp%*2,2),2)
                dayused% = dayused% + newused%
                if dayavail% <> 0% then                                  ~
                        percent = max(min(100*dayused%/dayavail%, 999),0)
L33530:         call "CONVERT" (percent, 0.0, str(temp$,,3))
                str(temp$,4) = "%" & bin(dayavail%,2)
                return

        REM *************************************************************~
            *                 P L O W   R O U T I N E                   *~
            *        I F   N O   K I T   L I S T   O N   F I L E        *~
            *-----------------------------------------------------------*~
            * Plows through the bill of materials for the assembly part *~
            * number and 'PIPOUTS'                                      *~
            *************************************************************

        load_bom
            readkey$, copypart$ = part$ : copybomid$ = bomid$
            if maxlines% = 0% then str(readkey$,26,3) = bomid$
            incl(1) = 0
            hdr$(2) = "  Part Assemblies             Part Descriptions"
            hdr$(1) = "  Existing BOMs For Part.  Use PF-1 To Select Anot~
        ~her Part."
            hdr$(3) = hex(ac) & "Select the Assembly Part And/Or BOM To C~
        ~opy From.  Use PF-16 to Cancel Copy."
            outtagnr$ = hex(06) & "Select the Part Assembly"
            REM *** Get Part & BOMID To Copy ***
            errormsg$ = hex(84) & "Copy Request Cancelled"
            call "PLOWCODE" (#11,readkey$, outtagnr$, 8025%, -.30,       ~
              f1%(11), hdr$(), 3.32, 57, incl(), header$(), " ", " ", #4)
                if f1%(11) = 0% then return
            copypart$  = readkey$
            copybomid$ = str(readkey$,26%,3%)
            init (hex(00)) readkey$

            phfact(1), ph% = 1%
            call "SHOSTAT" ("Loading BOM Structure")
            errormsg$ = "  "
            readkey$ = str(copypart$,,25) & str(copybomid$,,3) & "  0"
L34130:     call "PLOWNEXT" (#11, readkey$, 28%, f1%(11))
                if f1%(11) = 0 then return
            if maxlines% > pok%-2 then return
            maxlines% = maxlines% + 1%
            get #11,using L34180,part$(maxlines%),qu,tu,fx,ov,mkr$,bom$,  ~
                            stp$
L34180:     FMT CH(25), XX(31), 4*PD(14,4), CH(2), XX(1), CH(3), XX(4),  ~
                            CH(4)
            if mkr$ = "SP" or mkr$ = "RE" then L34740 /* skip these */
            if part$(maxlines%) = part$ then L34740   /* skip redundancy */
            call "READ100" (#4,part$(maxlines%),f1%(4))
              get #4, using L34220, partdescr$(maxlines%,1%),             ~
                                   partdescr$(maxlines%,2%), parttype$
L34220:             FMT POS(26), CH(32), POS(74), CH(4) , POS(180), CH(3)
                convert parttype$ to type%, data goto L34740
            if type% > 0% and type% < 200% then L34740
            ext = qu*tu+ov
            if type% > 199% and type% < 500% then L34290
            if str(mkr$,,1) <> "P" then L34290
               phqty = round(ext*qtymake*phfact(ph%)+fx,2)
               goto L34380
L34290:     quantity(maxlines%),newquantity(maxlines%) =                 ~
                                      round(ext*qtymake*phfact(ph%)+fx,2)
            call "CONVERT" (quantity(maxlines%),0.2,quantity$(maxlines%))
            newdate%(maxlines%),outdate%(maxlines%) = stdate%
            outdate$(maxlines%) = plstdate8$
            pipouts_changed% = 1%
            if type% > 489% and type% < 500% then L34590
            if type% > 789% and type% < 800% then L34590
          goto L34130

L34380
*        PHANTOM PROCESSOR
            if ph% > 100% then L34975
            if bom$ <> " " then L34532
                if stdate%=0 then L34980
                outtagnr$ = str(part$(maxlines%),,25) & "1" & hex(00)
                call "PLOWNEXT" (#12, outtagnr$, 26%, f1%(12))
                    if f1%(12) = 0 then L34980
                       gosub get_bom_string

                    if bom$(stdate%) = " " or bom$(stdate%) = blankdate$ ~
                                                              then L34980
                bom$ = bom$(stdate%)

L34532:     readkey$(ph%)=readkey$
            readkey$ = str(part$(maxlines%),,25)&str(bom$,,3)&"  0"
            ph%=ph%+1%
*          PHFACT(PH%) = QU * TU * PHFACT(PH%-1%)
            phfact(ph%) = 0
            if qtymake = 0 then L34540
            phfact(ph%) = phqty / qtymake

L34540:     gosub L34740
            ph%=ph%-1%
            readkey$=readkey$(ph%)
            goto L34130

L34590
*        TOOL PROCESSOR
            if maxlines% > pok%-2% then L34740
            maxlines% = maxlines%+1%
            part$(maxlines%)=part$(maxlines%-1%)
            partdescr$(maxlines%,1%) =                                   ~
                str(partdescr$(maxlines%-1%,1%),,15%) & ":TOOL RETURN"
            partdescr$(maxlines%,2%) =                                   ~
                str(partdescr$(maxlines%-1%,2%),,15%) & ":TOOL RETURN"
            quantity(maxlines%), newquantity(maxlines%) =                ~
                                                  -quantity(maxlines%-1%)
            call "CONVERT" (quantity(maxlines%),0.2,quantity$(maxlines%))
            newdate%(maxlines%),outdate%(maxlines%) = stdate%+1%
            call "DATE" addr("G+", bdate$, stdate%,                      ~
                                          str(outdate$(maxlines%),,6),0%)
            call "DATEFMT" (outdate$(maxlines%))
            goto L34130

L34740:   init (" ") part$(maxlines%), partdescr$(maxlines%,1%),         ~
                     partdescr$(maxlines%,2%), quantity$(maxlines%),     ~
                     outdate$(maxlines%)
          newdate%(maxlines%),outdate%(maxlines%) = 0%
          quantity(maxlines%),newquantity(maxlines%) = 0%
          maxlines%=maxlines%-1%
          goto L34130

            FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                CH(25),                  /* ASSEMBLY PART NUMBER       */~
                CH(3),                   /* WHICH BOM STRUCTURE?       */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                CH(2),                   /* BOM MARKER                 */~
                CH(21),                  /* DESCRIPTION                */~
                CH(3),                   /* WHICH RTE STRUCTURE?       */~
                BI(2),                   /* RTE STEP NUMBER            */~
                CH(3),                   /* COMPONENTS SPEC BOM        */~
                CH(47)                   /* FILLER                     */

        get_bom_string
            get #12, using L34960, bom$()
L34960:       FMT XX(29), 490 * CH(3)
            return

L34975
*       ** Unable to explode Phantom, too many levels
            ask$(2) = " Maximum Phantom Levels Exceeded."
            goto L34985

L34980
*       ** Unable to explode Phantom, No BOM
            ask$(2) = " No BOM found for explosion."

L34985:     ask$(1) = "Part:" & " " & part$(maxlines%)
            ask$(1) = ask$(1) & ask$(2)
            ask$(2) = "Press PF1 to Bypass Part."
            ask$(2) = ask$(2) & "  Press RETURN to bypass Explosion."
            ask$(3) = "Press PF12 to Terminate Append."
L34991:     ask% = 2%
            call "ASKUSER" (ask%, "** UNABLE TO EXPLODE PHANTOM **",     ~
                            ask$(1), ask$(2), ask$(3))
            if ask%  =  1% then L34740
            if ask%  =  0% then L34290
            if ask% <> 12% then L34991
               init (hex(ff)) readkey$
               goto L34740

        REM *************************************************************~
            *                 L O A D   R O U T I N G                   *~
            *-----------------------------------------------------------*~
            * Using subroutine, loads route into memory if not already  *~
            * there                                                     *~
            *************************************************************

        load_rte
            if rteloaded$ = rteid$ then return
            init (" ") rte$(), steps$()
            rtop% = 0%
            call "PLANRTE" (part$, rteid$, bomid$, errormsg$,            ~
                              rte$(),     enddate%, rtop%, #11, #19, #12)
            if rtop% = 0% then L35170
                for i% = 1% to rtop%
                     steps$(i%) = str(rte$(i%),88,5)
                next i%
L35170:     rteloaded$ = rteid$
            return

        REM *************************************************************~
            *           A P P E N D   R O U T I N G                     *~
            *-----------------------------------------------------------*~
            * Logic from PLANSUB MFG_DATE section which acts like       *~
            * planning with the 'ignore capacity' switch active.  It    *~
            * books the workcenters as if no other load exists for them.*~
            * It gets a little strange using planning logic in context  *~
            * of a manual job.  The intent is to get the WCOUTs created *~
            * and let the user edit them to their heart's content. But  *~
            * little nasties like how many steps to schedule on one day *~
            * are not easily handled.  No auto jumping.                 *~
            *************************************************************

        append_route
           if rteid$ <> " " then L35305 /* better go back and tell em */
            errormsg$ = "No Route Specified for Job - set one on first sc~
        ~reen"
            return
L35305:    call "SHOSTAT" ("Loading Route Structure")
           errormsg$ = " "
           x%, loi%, c%, maxwcout%   = 0%
           mat au% = zer : mat su% = zer : mat du% = zer : mat wa% = zer
           mat pd% = zer : mat awcu1% = zer : mat awcu2% = zer
           mat xsd% = zer : mat xxsd% = zer : mat awcu3% = zer
           init (" ") wl$(), wa$(), ws$(), wc$(), steps$()

            if rteloaded$ = rteid$ then L35391

            gosub load_rte
            if errormsg$ <> " " then return

L35391:     loi% = rtop%
            if loi% > 0% then L35441
             errormsg$="ROUTE HAS NO STEPS, " & str(part$,,25) & rteid$
             return

L35441:      overlap = 0
             sd% = enddate%
             nextpass = 1 : savesdl% = 999% : mat cumuse = zer
             cumuse   = 0 : xxsd%(rtop%), xsd%(rtop%), useday%  = sd%
             for x% = rtop% to 1% step -1% /* WORK BACKWARD */
                     gosub decode_routestring  /* for this step */
                     mat stpuse = zer
                     savesdl% = min(sd%, savesdl%)
                     if requnits% >= 0% then L35465
                        xsd%(x%) = xxsd%(x%)
                        overlap  = 0
L35465:              if overlap <= 0 then L35471
                        nextpass = max(0, 1 - cumuse(xsd%(x%)))
                        nextpass = min(1, nextpass + overlap)
L35471:              sd%  = xsd%(x%):overlap = 0
                     if x% > 1% then xxsd%(x%-1%), xsd%(x%-1%) = sd%
                     if mqf% = 0% then L35487
                        xxsd%(x%), xsd%(x%) = min(sd% + mqf%, enddate%)
                        sd%  = xsd%(x%)
                        cumuse   = 0:mat cumuse = zer
                        useday%  = sd%
                        nextpass = 1
L35487:              pd%(x%)  = sd% - 1%
                     if mq% + tr% > 0 then L35505 /* if any thing to do */
                        if x% = rtop% then L35541 /* first trip only */
                        pd%(x%) = pd%(x%+1%)
                        goto L35541

L35505:              holdtr% = tr%:holdsu% = su%

                     if wcwc$ = "VEND" then L35519
                     if wcwc$ <> " "   then L35525
L35519:                 gosub alloc_vend_time
                        goto L35541

L35525:                 gosub alloc_wc_time

L35541:              tr% = holdtr% : su% = holdsu%
                     mat cumuse = cumuse + stpuse

           next x%                  /* repeat above for each step */

              sd% = min(savesdl%, sd%)

              sd% = min(pd%(x%), sd%)
              if maxwcout% = 0% then gosub no_wcouts_needed
              wcouts_changed% = 1%
              gosub prepare_for_display

           return      /* All done  */

        REM * * * VENDOR STEP, ALLOCATE TIME ONLY * * *
        alloc_vend_time

                gosub get_vend   /* to find which days are vend holidays*/
                if nextpass = 1 then L35603
                        sd%  = sd% - 1% : pd%(x%) = pd%(x%)-1%
                        if pd%(x%) > stdate% then L35601
                        gosub starts_too_early

L35601:         nextpass = 1                 /* 1 = full day available  */
L35603:         qwwq% = mq%+tr%             /* QWWQ% is amount required */
                if pd%(x%) - qwwq% + 1% > stdate% then L35613
                        gosub starts_too_early

L35613:         if maxwcout% < cok% then L35621
                        gosub too_big

                        /* NOTE: AWCU3%() is the Avail. in W/C "VEND" */
L35621:              if awcu3%(sd%) > 0% then L35631
                        sd%  = sd% - 1% : pd%(x%)=pd%(x%)-1%
                        if pd%(x%) > stdate% then L35621
                        gosub starts_too_early

L35631:              if qwwq% <= 0% then return
                     maxwcout%  = maxwcout% + 1%  /* move/queue prior */
                     wc$(maxwcout%) = wcwc$       /* to the work      */
                     du%(maxwcout%) = sd%
                     au%(maxwcout%) = 0%
                     su%(maxwcout%) = 0%
                     ac$(maxwcout%) = str(rte$(x%),94,4)
                     ws$(maxwcout%) = str(rte$(x%),88,5)
                     wl$(maxwcout%) = bin(x%)
                     wa$(maxwcout%)=hex(00)
                     wa%(maxwcout%) = 0%

                     pd%(x%), sd%  = sd% - 1%
                     if x% > 1% then xxsd%(x%-1%), xsd%(x%-1%) = sd%
                     if pd%(x%) > stdate% then L35667
                        gosub starts_too_early

L35667:         qwwq% = qwwq% - 1%
                if qwwq% > 0% then L35613
L35671:         if awcu3%(pd%(x%)) > 0% then return
                   pd%(x%)=pd%(x%)-1%
                   if pd%(x%) > stdate% then L35671
                      gosub starts_too_early
        REM           RETURN

        REM * * * * * LOAD VENDOR SCHEDULE FROM WCMASTR * * * * *
        get_vend
                if got_vend% = 1% then return
                mat awcu3% = con
                call"READ100"(#14, "VEND ", f1%(14))
                     if f1%(14%) <> 1% then L35701
                get #14, using L35697, awcu3%()
L35697:              FMT XX(59), 490*BI(2)
                got_vend% = 1%
L35701:         return

        REM * * * ALLOCATE WORK CENTER TIME AND CAPACITY * * *
        alloc_wc_time
                call"READ100"(#14, str(wcwc$,1,4)& " ",f1%(14))

                get #14, using L35718, cumf%()
L35718:           FMT  XX(59), 490 * BI(2)
                  /* CUMF% is avail                */
                got_vend% = 0%

                mat awcu1% = con : mat awcu1% = (999999%)*awcu1%
                mat awcu2% = con : mat awcu2% = (999999%)*awcu2%
                mat awcu3% = con : mat awcu3% = (999999%)*awcu3%

        REM * * * START OF ACTUAL WORK CENTER TIME ALLOCATION LOOP * * *

                if mqp$ <> "Y" then L35927  /* MQP$ is MQ prior to work */
                if mq%  <   1% then L35927
                if nextpass = 1 then L35855
                        sd% = sd% - 1%
                        if sd% > stdate% then L35855
                        gosub starts_too_early

L35855:         nextpass = 1:aaa% = mq%
L35857:         if aaa% < 1% then L35907
                if cumf%(sd%) <= 0% then L35895
                aaa% = aaa% - 1%
                if maxwcout% < cok% then L35869
                   gosub too_big

L35869:              maxwcout% = maxwcout% + 1%   /* primary move/queue */
                     wc$(maxwcout%) = wcwc$       /* at the end of work */
                     du%(maxwcout%) = sd%
                     au%(maxwcout%) = 0%
                     su%(maxwcout%) = 0%
                     wl$(maxwcout%) = bin(x%)
*                   AC$(MAXWCOUT%) = STR(RTE$(X%),94,4)
                     ac$(maxwcout%) = " "
                     wa$(maxwcout%) = hex(09)
                     ws$(maxwcout%) = str(rte$(x%),88,5)
                     wa%(maxwcout%) = 0%

L35895:         sd% = sd% - 1%
                if sd% >= stdate% then L35857
                     gosub starts_too_early


L35907:         if tr% > 0% then L35921
                if x% = loi% then pd%(x%) = sd% else pd%(x%) = pd%(x% +1%)
                aaa% = 0
                if x% > 1% then xxsd%(x%-1%), xsd%(x%-1%) = sd%
                if pd%(x%) > stdate% then L36149
                   gosub starts_too_early


L35921:         pd%(x%) = sd% - 1%
                if pd%(x%) <= stdate% then gosub starts_too_early

L35927:         if cumf%(sd%) <= 0% then L36137 /* if none, back up 1 day*/


                aaa%, asu% = 0%

                if tr% < 1% then L36149

                awcu1%(sd%) = max(awcu1%(sd%), 0%)
                awcu2%(sd%) = max(awcu2%(sd%), 0%)
                awcu3%(sd%) = max(awcu3%(sd%), 0%)

                aaa = min(cumf%(sd%)   -   0%,                           ~
                        cumf%(sd%)  *  shift, cumf%(sd%)* nextpass)
                aaa = min(aaa, awcu1%(sd%)/nm1)
                aaa = min(aaa, awcu2%(sd%)/nm2)
                aaa = min(aaa, awcu3%(sd%)/nm3)

                aaa% = max(0%, round(aaa,0))

                aaa% = min(tr%, aaa%) /* TR% is what the job realy needs*/
                if x% = 1% then L35991
                if padunits% < 0% then L35987
                if holdtr% - tr% > padunits% then L35989
L35987:            xsd%(x%-1%)  = sd%
L35989:            xxsd%(x%-1%) = sd%
L35991:         tr%  = tr% - aaa%
                aaa = aaa%
                aaa = 100*aaa/cumf%(sd%)
                stpuse(sd%) = stpuse(sd%) + (aaa/100)
                if tr% >= su% then L36007
                asu% = max(0%, su% - tr%)
                su%  = max(0%, su% - asu%)

L36007:         if maxwcout% < cok% then L36013
                   gosub too_big

L36013:              maxwcout% = maxwcout% + 1%    /* primary work */
                     wc$(maxwcout%) = wcwc$
                     du%(maxwcout%) = sd%
                     au%(maxwcout%) = aaa%
                     su%(maxwcout%) = asu%
                     ac$(maxwcout%) = str(rte$(x%),94,4)
                     wl$(maxwcout%) = bin(x%)
                     wa$(maxwcout%) = hex(01)
                     ws$(maxwcout%) = str(rte$(x%),88,5)
                     wa%(maxwcout%) = 0%


                if wcwc1$ = " " then L36125
                if maxwcout% < cok% then L36047
                   gosub too_big

L36047:              maxwcout% = maxwcout% + 1%   /* concurrent #1 */
                     wc$(maxwcout%) = wcwc1$
                     du%(maxwcout%) = sd%
                     au%(maxwcout%) = round(aaa%*nm1, 0)
                     su%(maxwcout%) = round(asu%*nm1, 0)
                     wl$(maxwcout%) = bin(x%)
                     if au%(maxwcout%) - su%(maxwcout%) <= 0% then       ~
                ac$(maxwcout%)=" " else ac$(maxwcout%)=str(rte$(x%),177,4)
                     wa$(maxwcout%) = hex(02)
                     awcu1%(sd%)    = awcu1%(sd%) - au%(maxwcout%)
                     ws$(maxwcout%) = ws$(maxwcout% - 1%)
                     wa%(maxwcout%) = wa%(maxwcout% - 1%)
                if wcwc2$ = " " then L36125
                if maxwcout% < cok% then L36075
                   gosub too_big

L36075:              maxwcout% = maxwcout% + 1%   /* concurrent #2 */
                     wc$(maxwcout%) = wcwc2$
                     du%(maxwcout%) = sd%
                     au%(maxwcout%) = round(aaa%*nm2, 0)
                     su%(maxwcout%) = round(asu%*nm2, 0)
                     wl$(maxwcout%) = bin(x%)
                     if au%(maxwcout%) - su%(maxwcout%) <= 0% then       ~
                ac$(maxwcout%)=" " else ac$(maxwcout%)=str(rte$(x%),181,4)
                     wa$(maxwcout%) = hex(03)
                     awcu2%(sd%)    = awcu2%(sd%) - au%(maxwcout%)
                     ws$(maxwcout%) = ws$(maxwcout% - 1%)
                     wa%(maxwcout%) = wa%(maxwcout% - 1%)
                if wcwc3$ = " " then L36125
                if maxwcout% < cok% then L36103
                   gosub too_big

L36103:              maxwcout% = maxwcout% + 1%  /* concurrent #3 */
                     wc$(maxwcout%) = wcwc3$
                     du%(maxwcout%) = sd%
                     au%(maxwcout%) = round(aaa%*nm3, 0)
                     su%(maxwcout%) = round(asu%*nm3, 0)
                     wl$(maxwcout%) = bin(x%)
                     wa$(maxwcout%) = hex(04)
                     if au%(maxwcout%) - su%(maxwcout%) <= 0% then       ~
                ac$(maxwcout%)=" " else ac$(maxwcout%)=str(rte$(x%),185,4)
                     awcu3%(sd%)    = awcu3%(sd%) - au%(maxwcout%)
                     ws$(maxwcout%) = ws$(maxwcout% - 1%)
                     wa%(maxwcout%) = wa%(maxwcout% - 1%)

L36125:         if padunits% < 0% then L36135
                if overlap <> 0 then L36135
                   overlap = max(0, holdtr% - (tr% + padunits%))
                   overlap = 100*overlap/cumf%(sd%)
                   overlap = overlap/100
L36135:         if tr% < 1% then L36149
L36137:            sd% = sd% - 1% : pd%(x%) = pd%(x%)-1%
                   nextpass = 1
                     if pd%(x%) > stdate% then L35927
                        gosub starts_too_early


L36149: REM ALLOCATE WORK CENTER MQ TIME IF ANY, SET FACTOR FOR NEXT STEP
L36153:         if cumf%(pd%(x%)) > 0% then L36163
                   pd%(x%) = pd%(x%) - 1%
                   if pd%(x%) > stdate% then L36153
                      gosub starts_too_early

L36163:         if holdtr% >= 1% then L36169
                   nextpass = 1
                   return
L36169:         aaa = aaa%
                aaa = 100*aaa/cumf%(du%(maxwcout%))
                aaa = aaa/100
                if du%(maxwcout%) <> useday% then cumuse = 0
                cumuse = aaa + cumuse
                aaa% = 1%
                useday% = du%(maxwcout%)
                if mqp$ = "Y" then L36187
                if mq% > 0% then aaa% = 1% + mq%
L36187:         if aaa% > 0% then L36193
                   nextpass = 1 - cumuse
                   return       /* we are done if no MQ prior to work */
L36193:         cumuse = 0
                nextpass = 1

L36199:         if aaa% < 1% then return   /* done if there is MQ */
                if cumf%(sd%) <= 0% then L36239
                aaa% = aaa% - 1%
                if sd%   = du%(maxwcout%) then L36239
                if maxwcout% < cok% then L36215
                   gosub starts_too_early

L36215:              maxwcout% = maxwcout% + 1%   /* primary move/queue */
                     wc$(maxwcout%) = wcwc$       /* prior to the work  */
                     du%(maxwcout%) = sd%
                     au%(maxwcout%) = 0%
                     su%(maxwcout%) = 0%
                     wl$(maxwcout%) = bin(x%)
                     wa$(maxwcout%) = hex(00)
                     ws$(maxwcout%) = str(rte$(x%),88,5)
*                   AC$(MAXWCOUT%) = STR(RTE$(X%),94,4)
                     ac$(maxwcout%) = " "
                     wa%(maxwcout%) = 0%

L36239:         sd% = sd% - 1%
                if x% <= 1% then L36249
        REM     IF PADUNITS% > 0% THEN 36247
                   xsd%(x%-1%)  = sd%
                   xxsd%(x%-1%) = sd%
L36249:         if sd% >= stdate% then L36199
                gosub starts_too_early


        no_wcouts_needed
           errormsg$="Route NOT APPENDED - Route Has No Setup, Run or Mov~
        ~e/Queue"
           return clear all

           if edit% = 1% then goto L10245 else goto L10240

        starts_too_early
           errormsg$="Route NOT APPENDED - quantity too big or end date t~
        ~oo soon."
           maxwcout% = 0%
           return clear all

           if edit% = 1% then goto L10245 else goto L10240

        too_big
           /* too many workcenter outs for the program array sizes */
           errormsg$="Route NOT APPENDED - too many workcenter reservatio~
        ~ns needed"
           maxwcout% = 0%
           return clear all

           if edit% = 1% then goto L10245 else goto L10240

        decode_routestring  /* Common convert routine for wc info */
            get str(rte$(x%),32), using L36479,                           ~
                    wcwc$, mq%, su%, run, wcwc1$, nm1, wcwc2$, nm2,      ~
                   wcwc3$, nm3, shift, mqp$, yld(x%), steps$(x%), phfact,~
                     padqty, padpct, reqqty, reqpct

L36479:         FMT  CH(4), BI(4), BI(2), PD(14,4), CH(4), PD(7,4),      ~
                     CH(4), PD(7,4), CH(4), PD(7,4), PD(7,6), CH(1),     ~
                     XX(1), PD(14,7), CH(5), XX(43), PD(14,4),           ~
                     POS(114), 4*PD(14,4)

           mqf% = 0%: if mq% >= 0% then L36497
           mqf% = -mq%
           mq%  = 0%

L36497:    if shift < .01 then shift = 1

           nm1 = max(.01, nm1)
           nm2 = max(.01, nm2)
           nm3 = max(.01, nm3)

           tr = max(0, su% + ((100*phfact*run*qtymake)/yld(x%)))
           if wcwc$  = "VEND" then L36515
           if wcwc$ <> " "    then L36525
L36515:         tr% = round(tr/8,0)
                requnits% = 0%
             if reqqty >= qtymake or reqpct >= 100 then requnits% = -1
                return

L36525:        if tr > 0 then tr = max(tr, 1)
               tr% = round(tr,0)

            padunits% = -1%
            if tr%-su% <= 0% then L36555
            if padpct >= 100 then L36555
            if padqty >= qtymake then L36555
               padunits1  = max(0, (100*phfact*run*qtymake)/yld(x%))
               padunits1% = (padunits1 * padpct)/100
               padunits2% = max(0, (100*phfact*run*padqty)/yld(x%))
               padunits%  = min(tr%-su%, max(padunits1%, padunits2%))

L36555:     requnits% = -1%
            if tr%-su% <= 0% then L36573
            if reqpct >= 100 then L36573
            if reqqty >= qtymake then L36573
               requnits1  = max(0, (100*phfact*run*qtymake)/yld(x%))
               requnits1% = (requnits1 * reqpct)/100
               requnits2% = max(0, (100*phfact*run*reqqty)/yld(x%))
               requnits%  = min(tr%-su%, max(requnits1%, requnits2%))

L36573:        if mq% > 0% then L36581
                  mqp$=" "
                  return

L36581:        if tr% > 0% then return
                  mqp$="Y"
                  return


        prepare_for_display
          for i1% = maxwcout% to 1% step -1%  /* array is backwards so  */
              i2% = i1%                     /* we invert it for display */
L36615:       if i2% = 1% then L36650
                 if wa$(i2%) = hex(00) then L36650
                 if wa$(i2%) = hex(09) then L36650
                 if wa$(i2% - 1%) >= wa$(i2%) then L36650
                 if wa$(i2% - 1%) = hex(00) then L36650
                 if wa$(i2% - 1%) = hex(09) then L36650
                 i2% = i2% - 1% : goto  L36615
L36650:    for i% = i2% to i1%
               j% = maxwcout% - i1% + (i%  - i2%) + 1%
               k% = val(wl$(i%))
               wcwc$(j%) = wc$(i%)
               wcsu = su%(i%)
               wcrun   = au%(i%) - su%(i%)
               wctype$(j%) = wa$(i%)
               wcact$(j%) = ac$(i%)
               wcseq%(j%) = 100% * k% + val(wa$(i%))
               day% = du%(i%)

*        Save off intial allocations to minimize datasave efforts

            temp% = val(str(ws$(i%),5,1),1)
            wcstep$(j%)   = str(ws$(i%),,4) & " "

            if temp% = 0% then L36750
                wcstep$(j%) = wcstep$(j%) & "-"
                convert temp% to str(wcstep$(j%),pos(wcstep$(j%)="-")+1),~
                                                                  pic(00)
L36750:     gosub'99(wcwc$(j%), day%, au%(i%)) /*calc utilization % */
            wcpct$(j%) = temp$
            call "DATE" addr("G+",bdate$,day%-1%,str(wcdate$(j%),,6),0%)
            call "DATEFMT" (wcdate$(j%))
            call "DESCRIBE" (#14, wcwc$(j%), wcwcdescr$(j%), 0%, f1%(14))
            call "WCUN2HRS" (#14, wcwc$(j%), 0, wcsu, " ")
            call "CONVERT" (wcsu, 0.2, wcsu$(j%))
            call "WCUN2HRS" (#14, wcwc$(j%), 0, wcrun, " ")
            call "CONVERT" (wcrun, 0.2, wcrun$(j%))
            wcseq%(j%) = wcseq%(j%)/100%
            if wctype$(j%) > hex(09) then wcseq%(i%) = 0%
            wctype$ = " "
            if wctype$(j%) = hex(00) then wctype$ = "MQ"
            if wctype$(j%) = hex(02) then wctype$ = "C1"
            if wctype$(j%) = hex(03) then wctype$ = "C2"
            if wctype$(j%) = hex(04) then wctype$ = "C3"
            if wctype$(j%) = hex(09) then wctype$ = "MA"
            wctype$(j%) = wctype$

            next i%

            i1% = i2%
            next i1%

            return

        REM *************************************************************~
            *  Data save section - update pip with new info             *~
            *-----------------------------------------------------------*~
            * Replaces old with new if needed.                          *~
            *************************************************************

        update_pips
            outtagnr$ = "JOB ORDER: " & str(job$)
            call "READ101" (#18, outtagnr$, f1%(18))
            if f1%(18) = 1% then delete #18
            if bomid$ = " " and rteid$ = " " then L37140
                write #18, using L37130, part$, rteid$, outtagnr$, part$, ~
                           bomid$, outtagnr$  /* JBCROSS2 RECORD */
L37130:              FMT CH(25), CH(3), CH(19), CH(25), CH(3), CH(19)
L37140:     if pipouts_changed% = 0% then return
            if maxlines% = 0% and found% = 0% then L37180
                call "SHOSTAT" ("Updating KIT List")

L37180:     init (hex(00)) str(outtagnr$,20)
L37190:     call "PLOWNXT1" (#17, outtagnr$, 19%, f1%(17))
                if f1%(17) = 0% then L37290
            get #17 using L37230,part$(pok%),outdate%(pok%),quantity(pok%)
L37230:     FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
            delete #17
            call "PIPFLAGS"(part$(pok%), 1%, outdate%(pok%),             ~
                                                 quantity(pok%), #9, #10)
            goto L37190

L37290:     if maxlines% = 0% then return
            for i% = 1% to maxlines%
                if abs(newquantity(i%)) < .0001 then L37370
                temp = round(newquantity(i%),2%)
L37330:         systime$ = time
                write #17 using L37410, outtagnr$, part$(i%),             ~
                          newdate%(i%), systime$, temp, eod goto L37330
                call "PIPFLAGS" (part$(i%),1%,newdate%(i%),-temp,#9,#10)
L37370:     next i%
            return


L37410: FMT                      /* FILE: PIPOUT                       */~
            CH(19),              /* Tag number                         */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date out of PIP in date subscript  */~
            CH(8),               /* Time from the system clock         */~
            PD(14,4)             /* Quantity                           */~

        FMT                      /* FILE: PIPIN                        */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            CH(19),              /* Tag number                         */~
            PD(14,4),            /* Quantity                           */~
            BI(4)                /* Date to start as a date subscript  */~


        REM *************************************************************~
            *  Data save section - update WC files with new info        *~
            *-----------------------------------------------------------*~
            * Replaces old with new if needed.                          *~
            *************************************************************
        update_wcs
            if wcouts_changed% = 0% then return
            if maxwcout% = 0% and found% = 0% then L38050
                call "SHOSTAT" ("Updating Capacity Details")

L38050:     outtagnr$ = "JOB ORDER: " & str(job$)
            init (hex(00)) str(outtagnr$,20)
            call "DELETE" (#13, outtagnr$, 19%)
            call "JBVSASUB" (#2, #13, "DEL", job$, part$, qtymake)
            qtymake_changed% = 0%
            if maxwcout% = 0% and found% = 0% then return

*        Net old & new allocations to minimize datasave efforts
            lastseq% = 0%  : laststep$ = " "
            c% = pos(wcnet$()=hex(ff))
            if c% > 0% then c% = (c%-1%)/8% else c% = cok%
            if maxwcout% = 0% then L38430

            gosub load_rte
            call "WCUN2HRS" (#14, " ", 0, 0, " ") /* Clear out */
            for i% = 1% to maxwcout%
                REM First prepare data for upcoming writes...
                call "DATUNFMT" (wcdate$(i%))
                call "PIPINDEX" (#2, str(wcdate$(i%),,6), day%, u3%)
                wcdate$(i%) = bin(day%,2)
                wcnet$ = str(wcwc$(i%)) & str(wcdate$(i%),,2)

                REM Unformat Step number Field...
                temp% = 0%
                if pos(wcstep$(i%)="-") = 0% then L38175
                convert str(wcstep$(i%),pos(wcstep$(i%)="-")+1) to temp%,~
                                                          data goto L38170
L38170:         str(wcstep$(i%),pos(wcstep$(i%)="-")) = " "
L38175:         str(wcstep$(i%),5) = bin(temp%,1)

                REM Insure sequence numbers are proprly Assigned...
                manual$ = " "
                search steps$() = str(wcstep$(i%),,5) to cursor%() step 5
                if cursor%(1) = 0% then manual$ = "M"
                if cursor%(1) > 0% then seq% = (cursor%(1) + 4%) / 5%    ~
                                   else seq%=0%
*              IF I% = 1% THEN 38220
*                   IF WCDATE$(I%) <> WCDATE$(I%-1) THEN LASTSEQ% =     ~
*                                            INT(LASTSEQ% / 100%) * 100%
*           IF WCDATE$(I%) = WCDATE$(I%-1) AND SEQ% = INT(LASTSEQ%/100%)~
*                           THEN WCSEQ%(I%) = WCSEQ%(I%) + 1%
*              IF SEQ% = 0% THEN WCSEQ%(I%) =  LASTSEQ% + 1%            ~
*                           ELSE WCSEQ%(I%) = SEQ% * 100%

                gosub set_wcseq /* Try a different tack on setting these*/

                REM Return Quantities to WC Units...
*              LASTSEQ% = WCSEQ%(I%)
                quantity(i%), newquantity(i%)=0/*Reuse As Work Variable*/
                convert wcsu$(i%) to quantity(i%), data goto L38255
L38255:         convert wcrun$(i%) to newquantity(i%), data goto L38260
L38260:         call "WCUN2HRS" (#14, wcwc$(i%), factor, 0, " ")
                adjfactor = 24/factor
                quantity(i%) = quantity(i%)/ adjfactor
                newquantity(i%) = newquantity(i%)/ adjfactor

                REM Net Into Capacity Usage Array...
                search str(wcnet$(),,(c%+1)*8) = str(wcnet$,,6)          ~
                                                      to cursor%() step 8
                if cursor%(1)>0 then y% = (cursor%(1)+7)/8 else y% = c%+1
                if y% > cok% then L38325  /* Shouldn't happen */
                wcnet$(y%) = str(wcnet$) & bin(y%,2)
                wcnet%(y%) = wcnet%(y%) + quantity(i%) + newquantity(i%)
                c% = max(c%, y%)

L38325:         REM Little more unformating, then write...
                wctype% = 1%
                if wctype$(i%) = "MQ" then wctype% = 0%
                if wctype$(i%) = "C1" then wctype% = 2%
                if wctype$(i%) = "C2" then wctype% = 3%
                if wctype$(i%) = "C3" then wctype% = 4%
                if wctype$(i%) = "MA" then wctype% = 9%
                if manual$ = "M" then wctype% = wctype% + 10%
*              WCSEQ%(I%) = WCSEQ%(I%) + WCTYPE%
                wcseq%(i%) = wcseq% + wctype%
                wctype$(i%) = bin(wctype%,1)

                day% = val(str(wcdate$(i%),,2), 2)
              if day% < 1% or day% > 490% then L38420 /*extra precaution*/
                write #13, using L38410, wcwc$(i%), day%, wcseq%(i%),     ~
                           outtagnr$, day%, wcseq%(i%), quantity(i%),    ~
                           newquantity(i%), wcstep$(i%), wcfill$(i%),    ~
                           wctype$(i%), wcact$(i%), " ", eod goto L38420

L38410:         FMT CH(4), 2*BI(2), CH(19), 2*BI(2), 2*BI(4), CH(5),     ~
                    CH(2), CH(1), CH(4), CH(17)
L38420:     next i%

L38430:     REM Update WCMASTR as required...
            call "JBVSASUB" (#2, #13, "RES", job$, part$, qtymake)
            qtymake_changed% = 0%
            table_top% = pos(wcnet$()=hex(ff))
            if table_top% = 0% then table_top% = cok%  else              ~
               table_top% = (table_top%-1)/8%
            if table_top% < 1% then return
            call "SORT" addr(wcnet$(), table_top%, 8%)
            lastwc$ = hex(01020304)
            for i% = 1% to table_top%
                y% = val(str(wcnet$(i%),7),2)
                if y% > cok% then L38505  /* ?? */
                if wcnet%(y%) = 0% then L38505
                if str(wcnet$(i%),,4) <> lastwc$ then gosub L38525
                day% = val(str(wcnet$(i%),5,2),2)
               if day% < 1% or day% > 490% then L38505 /*again,precaution*/
                uadj%(day%) = uadj%(day%) + wcnet%(y%)
L38505:     next i%
            gosub L38525
            return

L38525:     if lastwc$ = hex(01020304) then L38565
            call "READ101" (#14, lastwc$, f1%(14))
                if f1%(14) = 0% then L38565
            get #14, using L38545, used%()
L38545:     FMT POS(1040), 490 *BI(2)
            mat used% = used% + uadj%
            put #14, using L38545, used%()
            rewrite #14
            if lastget$ = lastwc$ then lastget$ = hex(abcdef)
L38565:     mat uadj% = zer
            lastwc$ = str(wcnet$(i%),,4)
            return

        set_wcseq
            /* If the step is the same we'll keep using the seq number */
            /* Iterate seq number when doing new step */
            if i% <> 1% then L38630
                wcseq%, lastseq%  = 1%
                goto L38650
L38630:     if wcstep$(i%) = laststep$ then wcseq%, lastseq%  = lastseq% ~
                         else wcseq%, lastseq%  = lastseq% + 1%

L38650:     wcseq% = wcseq% * 100%
            laststep$ = wcstep$(i%)

            return

        REM *************************************************************~
            *           T E S T   A U T H O R I Z A T I O N             *~
            *-----------------------------------------------------------*~
            * See if user can access the indicated part.                *~
            *************************************************************

        check_for_restrictions
            errormsg$ = " "
            if admin% = 1% then return
            call "READ100" (#4, part$, f1%(4))
            if f1%(4) = 0% then return
                get #4, using L39120, partclass$
L39120:              FMT POS(309), CH(3)
                search codes$() = str(partclass$,,3) to cursor%() step 3
                if cursor%(1) <> 0% then return
                     errormsg$ = "Sorry, you're not a valid Production" &~
                                 " Scheduler for this Part.  Part is"   &~
                                 " class " & partclass$
                     return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *-----------------------------------------------------------*~
            * Inputs document for first time.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  line2$  = "Last Job Managed Was: " & lastjob$
                  init(hex(84)) hfac$(), dfac$
                  pfdescr$(1) = "(1)Start Over                           ~
        ~                       (13)Instructions"
                  pfdescr$(2) = "                                        ~
        ~                       (15)Print Screen"
                  pfdescr$(3) = "                                        ~
        ~                       (16)Exit Program"
                  str(pfdescr$(2%),45%,14%) = ecrpfk$
                  pfkeys$ = hex(00010d0f10080bff)
                  if ecrpfk$ = " " then str(pfkeys$,7%,1%) = hex(ff)
                  if fieldnr% <> 2% then L40190
                  str(pfdescr$(1),25,22) = hex(84)&"(3)Copy Existing Job"
                  pfkeys$ = pfkeys$ & hex(03)
L40190:           goto L40410

            deffn'111(fieldnr%)
                  line2$  = "Last Job Managed Was: " & lastjob$
                  pfdescr$(1%)= "(1)Start Over          (7)See PO Info   ~
        ~                       (13)Instructions"
                  pfdescr$(2) = "                                        ~
        ~                       (15)Print Screen"
                  init(hex(8c)) hfac$(), dfac$
                  if fieldnr% = 0% then L40330
                  pfdescr$(3) = "(RETURN)Continue"
                  pfkeys$ = hex(00010d0f0807)
                  if str(job$,1%,2%) = "PJ" then L40310
                      str(pfdescr$(1%),24%,14%) = " "
                      str(pfkeys$,6%,1%) = hex(ff)
L40310:           goto L40410

L40330:           init(hex(86)) hfac$() /* make all fields tab stops */
                  init(hex(84)) dfac$, hfac$(4), hfac$(1) /*except 1 & 4*/
                  pfdescr$(2) = "(2)Edit Kit List       (8)Job Cost Summa~
        ~ry   (25)Manage Text   (15)Print Screen"
                  pfdescr$(3) = "(3)Edit Capacity Req'd (9)Reset Pick Dat~
        ~es                     (16)Save Data"
                  str(pfdescr$(1%),45%,14%) = ecrpfk$
                  pfkeys$ = hex(0001020308090d0f1019ff070bff)
                  if ecrpfk$ = " " then str(pfkeys$,13%,1%) = hex(ff)
                  if maxlines% > 0% then L40398
                      str(pfdescr$(3),24,20) = " "
                      str(pfkeys$,6,1) = hex(ff)
L40398:           if str(job$,1%,2%) = "PJ" then L40410
                      str(pfdescr$(1%),24%,14%) = " "
                      str(pfkeys$,12%,1%) = hex(ff)

L40410:           str(line2$,62) = "JBINPKIT: " & cms2v$
                  str(pfdescr$(3),63,1) = hex(84)
                  if type$ < "500" then tfac$ = hex(94)                  ~
                                   else tfac$ = hex(84)
                  on fieldnr% gosub L40570,         /* Job Number       */~
                                    L40570,         /* Control Number   */~
                                    L40560,         /* Job Description  */~
                                    L40570,         /* Part Number      */~
                                    L40570,         /* Quantity to Make */~
                                    L40570,         /* WIP Acct         */~
                                    L40570,         /* Planned Start    */~
                                    L40570,         /* Planned End      */~
                                    L40570,         /* Actual Start     */~
                                    L40570,         /* BOM ID           */~
                                    L40570,         /* RTE ID           */~
                                    L40570,         /* Core WIP (Maybe) */~
                                    L40575          /* Est. Perc. Comp. */
                     goto L40590

L40560:               hfac$(fieldnr%) = hex(80)  :  return
L40570:               hfac$(fieldnr%) = hex(81)  :  return
L40575:               hfac$(fieldnr%) = hex(82)  :  return  /* Numeric */

L40590:     accept                                                       ~
               at (01,02), "Manually Create/Manage Production Jobs",     ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$ , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Job Number",                                 ~
               at (06,18), fac(hfac$( 1)), job$                 , ch(08),~
               at (06,38), fac(vfac$(1%)), disp1$               , ch(11),~
               at (06,64), fac(vfac$(2%)), disp2$               , ch(09),~
               at (07,02), "Control No.",                                ~
               at (07,18), fac(hfac$( 2)), control$             , ch(19),~
               at (07,38), fac(vfac$(1%)), disp3$               , ch(09),~
               at (07,52), fac(vfac$(2%)), disp4$               , ch(21),~
               at (08,02), "Description",                                ~
               at (08,18), fac(hfac$( 3)), jbdescr$             , ch(30),~
               at (09,02), "Part Number",                                ~
               at (09,18), fac(hfac$( 4)), part$                , ch(25),~
               at (09,45), fac(hex(8c)),   partdescr$           , ch(34),~
                                                                         ~
               at (13,38), "Original Job Quantity",                      ~
               at (13,63), fac(dfac$),     qtyorig$             , ch(10),~
               at (14,38), "Quantity Scrapped    ",                      ~
               at (14,63), fac(dfac$),     qtyscrp$             , ch(10),~
               at (15,38), "Quantity to Rework   ",                      ~
               at (15,63), fac(dfac$),     qtyrewk$             , ch(10),~
               at (16,38), "Manual Adjustments   ",                      ~
               at (16,63), fac(dfac$),     qtyadj$              , ch(10),~
                                                                         ~
               at (10,02), "Current Job Qty",                            ~
               at (10,18), fac(hfac$( 5)), qtymake$             , ch(10),~
               at (10,29), fac(hex(8c)),   uom$                 , ch(04),~
               at (10,37), "Type:",                                      ~
               at (10,43), fac(tfac$),     type$                , ch(03),~
               at (10,49), "Qty Completed",                              ~
               at (10,63), fac(dfac$),     qtycomp$             , ch(10),~
                                                                         ~
               at (11,02), "WIP Account",                                ~
               at (11,25), fac(hfac$(6)),  wipacct$             , ch(12),~
               at (11,38), fac(hex(8c)),   wipdescr$            , ch(32),~
                                                                         ~
               at (12,02), "Planned Start Date",                         ~
               at (12,25), fac(hfac$( 7)), plstdate$            , ch(10),~
               at (12,38), "Estimated Percent Complete",                 ~
               at (12,65), fac(hfac$(13)), estperccomp$         , ch(08),~
               at (13,02), "Planned End Date",                           ~
               at (13,25), fac(hfac$( 8)), plenddate$           , ch(10),~
               at (14,02), "Actual Start Date",                          ~
               at (14,25), fac(hfac$( 9)), stdate$              , ch(10),~
               at (15,02), "Bill of Materials",                          ~
               at (15,25), fac(hfac$(10)), bomid$               , ch(03),~
               at (16,02), "Manufacturing Routing",                      ~
               at (16,25), fac(hfac$(11)), rteid$               , ch(03),~
               at (17,02), fac(hex(8c)), jbcorewipp$            , ch(20),~
               at (17,25), fac(hfac$(12)), jbcorewip$           , ch(12),~
               at (17,38), fac(hex(8c)), jbcorewipd$            , ch(32),~
               at (19,10), "Last Modified By: xxx   On: xxxxxxxx",       ~
               at (19,28), fac(hex(8c)), last_mod_by$           , ch(03),~
               at (19,38), fac(hex(8c)), last_mod_at$           , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)            , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 9% then L41114
                  gosub reset_pipout_dates
                  goto L40590

L41114:        if keyhit% <> 11% then L41120
                  gosub view_ecr_info
                  goto L40590

L41120:        if keyhit% <> 13% then L41160
                  call "MANUAL" ("JBINPKIT")
                  goto L40590

L41160:        if keyhit% <> 15% then L41200
                  call "PRNTSCRN"
                  goto L40590

L41200:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *            R E - S E T  P I P O U T  D A T E S            *~
            *-----------------------------------------------------------*~
            * Ask user to confirm, then reset pick dates to planstart   *~
            *************************************************************
        reset_pipout_dates
            k% = 2%
            ask$()  = " "
            ask$(1) =                                                    ~
                    "Press PF 12 to cause all Kitlist dates to be set to"
            ask$(2) =                                                    ~
                    "the job planned start date or press RETURN to Abort"

            call "ASKUSER" (k%, "**** OK TO RESET PICK DATES ?? ****",   ~
                            ask$(1), ask$(2), ask$(3))

            if k% <> 12% then L42160
            pipouts_changed% = 1%
            for i% = 1% to maxlines%
               old$ =  outdate$(i%)
               old% = newdate%(i%)
               outdate$(i%) = plstdate8$
               temp$ = outdate$(i%)
               call "DATUFMTC" (temp$)
               call "PIPINDEX" (#2, temp$, newdate%(i%), u3%)
                 if u3% =  0% then L42140
                 newdate%(i%) = old%
                 outdate$(i%) = old$
L42140:     next i%

L42160:     return


        REM *************************************************************~
            *            K I T L I S T  S C R E E N                     *~
            *-----------------------------------------------------------*~
            * Handles input and edit of kitlist (PIPOUTS)               *~
            *************************************************************

            deffn'202(screenline%,fieldnr%)
                  screen%=2%
                  str(pfkeys$(2%),18%) = hex(0e)
                  screen$(2,3)="(3)Last Lines   (14)Append BOM To List (8~
        ~)UOM  (28)Delete All  (16)Edit Header"
                  if c% < 1% then fieldnr% = 0% /* just to be sure     */
                  if fieldnr% = 0% then L45130
                  if part$(c%) = " " then fieldnr% = 0%
                  if fieldnr% <> 3% then L45130
                     convert quantity$(c%) to temp, data goto L45130
                     call "CONVERT" (temp,-0.2,quantity$(c%))
L45130:           init (hex(86)) lfac$()
                  if fieldnr% = 0% then  goto L45290  else goto L45280

            deffn'201(screenline%,fieldnr%)
                  screen$(1,1)="(1)Start Over      (14)Append BOM To List~
        ~                      (13)Instructions"
                  screen$(1,3)="(2)Start Line Over  (6)Same As Line Above~
        ~                      (16)Edit Mode"
                  str(pfkeys$(1),18) = hex(0e)

*                IF POS(PART$() <> " ") <> 0 THEN 45230 ELSE 45270
*
*                SCREEN$(1,1)="(1)Start Over                            ~
*                            (13)Instructions"
*                STR(PFKEYS$(1),18) = HEX(FF)

                  screen%=1%
L45280:           init(hex(8c)) lfac$()
L45290:           line2$  = "Job Number: " & job$ & "  " & jbdescr$

                  on fieldnr% gosub L45420,         /* PART             */~
                                    L45420,         /* DESCRIPTION      */~
                                    L45450,         /* QUANTITY         */~
                                    L45420,         /* DATE OUT         */~
                                    L45480          /* SPEC DELETE      */
                     goto L45530

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(screenline%,fieldnr%) = hex(80)
                      return
L45420:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(screenline%,fieldnr%) = hex(81)
                      return
L45450:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(screenline%,fieldnr%) = hex(82)
                      return
L45480:           REM SET BLINKING FAC FOR QUANTITY
                      init (hex(94)) str(lfac$(),7*screenline%-6%,7)
                      str(screen$(1,1),,25) = "(1)Cancel Delete Request"
                      screen$(1,3) = "(RETURN)Delete Item"
                      return

L45530:     if fieldnr% <> 0% then L45620
                for wph% = 1% to 12%
                     if quantity$(wph%+pbas%) = " " then L45610
                     convert quantity$(wph%+pbas%) to wph,data goto L45610
                     if wph <> 0 then L45610
                          for wphf% = 1% to 4%
                               lfac$(wph%, wphf%) = hex(8c)
                          next wphf%
L45610:         next wph%
L45620:     header_kit$(1%)="Seq. Part #                    Description  ~
        ~                 Quantity   Date"
            header_kit$(2%)="Seq. Part #                    UOM          ~
        ~                 Quantity   Date"

            accept                                                       ~
               at (01,02), "Manage Job's Kit (Materials) List",          ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$ , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "JOB PART NUMBER",                            ~
               at (04,19), fac(hex(84)),   part$                , ch(25),~
               at (04,46), fac(hex(8c)),   partdescr$           , ch(34),~
               at (05,02), "QUANTITY TO MAKE",                           ~
               at (05,19), fac(hex(84)), qtymake$               , ch(10),~
               at (06,02), fac(hex(94)), errormsg$              , ch(79),~
               at (07,02), fac(hex(ac)), header_kit$(tgl%)      , ch(79),~
                                                                         ~
                                                                         ~
               at (08,01), fac(hex(8c)),      seq$  (01%+pbas%) , ch(05),~
               at (09,01), fac(hex(8c)),      seq$  (02%+pbas%) , ch(05),~
               at (10,01), fac(hex(8c)),      seq$  (03%+pbas%) , ch(05),~
               at (11,01), fac(hex(8c)),      seq$  (04%+pbas%) , ch(05),~
               at (12,01), fac(hex(8c)),      seq$  (05%+pbas%) , ch(05),~
               at (13,01), fac(hex(8c)),      seq$  (06%+pbas%) , ch(05),~
               at (14,01), fac(hex(8c)),      seq$  (07%+pbas%) , ch(05),~
               at (15,01), fac(hex(8c)),      seq$  (08%+pbas%) , ch(05),~
               at (16,01), fac(hex(8c)),      seq$  (09%+pbas%) , ch(05),~
               at (17,01), fac(hex(8c)),      seq$  (10%+pbas%) , ch(05),~
               at (18,01), fac(hex(8c)),      seq$  (11%+pbas%) , ch(05),~
               at (19,01), fac(hex(8c)),      seq$  (12%+pbas%) , ch(05),~
                                                                         ~
               at (08,07), fac(lfac$( 1, 1)), part$( 1%+pbas%)  , ch(25),~
               at (09,07), fac(lfac$( 2, 1)), part$( 2%+pbas%)  , ch(25),~
               at (10,07), fac(lfac$( 3, 1)), part$( 3%+pbas%)  , ch(25),~
               at (11,07), fac(lfac$( 4, 1)), part$( 4%+pbas%)  , ch(25),~
               at (12,07), fac(lfac$( 5, 1)), part$( 5%+pbas%)  , ch(25),~
               at (13,07), fac(lfac$( 6, 1)), part$( 6%+pbas%)  , ch(25),~
               at (14,07), fac(lfac$( 7, 1)), part$( 7%+pbas%)  , ch(25),~
               at (15,07), fac(lfac$( 8, 1)), part$( 8%+pbas%)  , ch(25),~
               at (16,07), fac(lfac$( 9, 1)), part$( 9%+pbas%)  , ch(25),~
               at (17,07), fac(lfac$(10, 1)), part$(10%+pbas%)  , ch(25),~
               at (18,07), fac(lfac$(11, 1)), part$(11%+pbas%)  , ch(25),~
               at (19,07), fac(lfac$(12, 1)), part$(12%+pbas%)  , ch(25),~
                                                                         ~
               at (08,33), fac(lfac$( 1, 2)),partdescr$( 1+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (09,33), fac(lfac$( 2, 2)),partdescr$( 2+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (10,33), fac(lfac$( 3, 2)),partdescr$( 3+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (11,33), fac(lfac$( 4, 2)),partdescr$( 4+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (12,33), fac(lfac$( 5, 2)),partdescr$( 5+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (13,33), fac(lfac$( 6, 2)),partdescr$( 6+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (14,33), fac(lfac$( 7, 2)),partdescr$( 7+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (15,33), fac(lfac$( 8, 2)),partdescr$( 8+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (16,33), fac(lfac$( 9, 2)),partdescr$( 9+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (17,33), fac(lfac$(10, 2)),partdescr$(10+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (18,33), fac(lfac$(11, 2)),partdescr$(11+pbas%,tgl%),  ~
                                                                  ch(27),~
               at (19,33), fac(lfac$(12, 2)),partdescr$(12+pbas%,tgl%),  ~
                                                                  ch(27),~
                                                                         ~
               at (08,61), fac(lfac$( 1, 3)), quantity$( 1+pbas%),ch(10),~
               at (09,61), fac(lfac$( 2, 3)), quantity$( 2+pbas%),ch(10),~
               at (10,61), fac(lfac$( 3, 3)), quantity$( 3+pbas%),ch(10),~
               at (11,61), fac(lfac$( 4, 3)), quantity$( 4+pbas%),ch(10),~
               at (12,61), fac(lfac$( 5, 3)), quantity$( 5+pbas%),ch(10),~
               at (13,61), fac(lfac$( 6, 3)), quantity$( 6+pbas%),ch(10),~
               at (14,61), fac(lfac$( 7, 3)), quantity$( 7+pbas%),ch(10),~
               at (15,61), fac(lfac$( 8, 3)), quantity$( 8+pbas%),ch(10),~
               at (16,61), fac(lfac$( 9, 3)), quantity$( 9+pbas%),ch(10),~
               at (17,61), fac(lfac$(10, 3)), quantity$(10+pbas%),ch(10),~
               at (18,61), fac(lfac$(11, 3)), quantity$(11+pbas%),ch(10),~
               at (19,61), fac(lfac$(12, 3)), quantity$(12+pbas%),ch(10),~
                                                                         ~
               at (08,72), fac(lfac$( 1, 4)), outdate$ ( 1+pbas%),ch(08),~
               at (09,72), fac(lfac$( 2, 4)), outdate$ ( 2+pbas%),ch(08),~
               at (10,72), fac(lfac$( 3, 4)), outdate$ ( 3+pbas%),ch(08),~
               at (11,72), fac(lfac$( 4, 4)), outdate$ ( 4+pbas%),ch(08),~
               at (12,72), fac(lfac$( 5, 4)), outdate$ ( 5+pbas%),ch(08),~
               at (13,72), fac(lfac$( 6, 4)), outdate$ ( 6+pbas%),ch(08),~
               at (14,72), fac(lfac$( 7, 4)), outdate$ ( 7+pbas%),ch(08),~
               at (15,72), fac(lfac$( 8, 4)), outdate$ ( 8+pbas%),ch(08),~
               at (16,72), fac(lfac$( 9, 4)), outdate$ ( 9+pbas%),ch(08),~
               at (17,72), fac(lfac$(10, 4)), outdate$ (10+pbas%),ch(08),~
               at (18,72), fac(lfac$(11, 4)), outdate$ (11+pbas%),ch(08),~
               at (19,72), fac(lfac$(12, 4)), outdate$ (12+pbas%),ch(08),~
                                                                         ~
               at (21,02), fac(hex(ac)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), screen$(screen%,1)     , ch(79),~
               at (23,02), fac(hex(8c)), screen$(screen%,2)     , ch(79),~
               at (24,02), fac(hex(8c)), screen$(screen%,3)     , ch(79),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <>  8% then L46500
                  if tgl% = 1% then tgl% = 2% else tgl% = 1%
                  goto L45620

L46500:        if keyhit% <> 13% then L46540
                  call "MANUAL" ("JBINPKIT")
                  goto L45530

L46540:        if keyhit% <> 15% then L46580
                  call "PRNTSCRN"
                  goto L45530

L46580:        if screen%=1% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   4       *~
            *-----------------------------------------------------------*~
            * Screen for editing page 4 of document.                    *~
            *************************************************************

            deffn'208(screenline%,fieldnr%)
                  screen%=2%
                  screen$(2,3)="(3)Last Lines    (8)See Route  (14)Append~
        ~ Rte  (28)Delete All  (16)Edit Header"
                  if maxwcout% = 0% then L47100
                     str(screen$(2,3),32,16) = " "
                     str(pfkeys$(2),18) = hex(ff)

L47100:           if fieldnr% <> 0% then L47270
                  init (hex(86)) lfac$()
                  REM Dim dup dates for ledgablity...
                  for i% = cbas% + 2% to cbas% + 12%
                     if wcdate$(i%)=wcdate$(i%-1%) then                  ~
                                                lfac$(i%-cbas%,2)=hex(8e)
                  next i%
                  goto L47280

            deffn'207(screenline%,fieldnr%)
                  screen$(1,1)="(1)Start Over      (14)Append Route      ~
        ~                      (13)Instructions"
                  screen$(1,3)="(2)Start Line Over  (6)Same As Line Above~
        ~  (8)See Routing      (16)Continue "
                  if maxwcout% = 0% then L47260
                     str(screen$(1,1),20,16) = " "
                     str(pfkeys$(1),18) = hex(ff)
L47260:           screen%=1%
L47270:           init(hex(8c)) lfac$()
L47280:           line2$  = "Job Number: " & job$ & "  " & jbdescr$
                  header$="Seq. Type  Date   Step     WC  WC Description ~
        ~        Actv Usd% Set Up/Run Hrs"
                  on fieldnr% gosub L47430,         /* WCOUT REC TYPE   */~
                                    L47430,         /* STEP             */~
                                    L47430,         /* WORK CENTER CODE */~
                                    L47430,         /* DATE OUT         */~
                                    L47430,         /* ACTIVITY CODE    */~
                                    L47430,         /* SET UP/RUN HOURS */~
                                    L47490          /* SPEC DELETE      */
                     goto L47540

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(screenline%,fieldnr%) = hex(80)
                      return
L47430:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(screenline%,fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(screenline%,fieldnr%) = hex(82)
                      return
L47490:           REM SET BLINKING FAC FOR QUANTITY
                      init (hex(94)) str(lfac$(),7*screenline%-6%,7)
                      str(screen$(1,1),,25) = "(1)Cancel Delete Request"
                      screen$(1,3) = "(RETURN)Delete Item"
                      return
L47540:     accept                                                       ~
               at (01,02), "Manage Job's Capacity Allocations",          ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$ , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "JOB PART NUMBER",                            ~
               at (04,19), fac(hex(84)),   part$                , ch(25),~
               at (04,46), fac(hex(8c)),   partdescr$           , ch(34),~
               at (05,02), "QUANTITY TO MAKE",                           ~
               at (05,19), fac(hex(84)), qtymake$               , ch(10),~
               at (06,02), fac(hex(94)), errormsg$              , ch(79),~
               at (07,02), fac(hex(ac)), header$                , ch(78),~
                                                                         ~
               at (08,01), fac(hex(8c)),      seq$   (01+cbas%) , ch(05),~
               at (09,01), fac(hex(8c)),      seq$   (02+cbas%) , ch(05),~
               at (10,01), fac(hex(8c)),      seq$   (03+cbas%) , ch(05),~
               at (11,01), fac(hex(8c)),      seq$   (04+cbas%) , ch(05),~
               at (12,01), fac(hex(8c)),      seq$   (05+cbas%) , ch(05),~
               at (13,01), fac(hex(8c)),      seq$   (06+cbas%) , ch(05),~
               at (14,01), fac(hex(8c)),      seq$   (07+cbas%) , ch(05),~
               at (15,01), fac(hex(8c)),      seq$   (08+cbas%) , ch(05),~
               at (16,01), fac(hex(8c)),      seq$   (09+cbas%) , ch(05),~
               at (17,01), fac(hex(8c)),      seq$   (10+cbas%) , ch(05),~
               at (18,01), fac(hex(8c)),      seq$   (11+cbas%) , ch(05),~
               at (19,01), fac(hex(8c)),      seq$   (12+cbas%) , ch(05),~
                                                                         ~
               at (08,08), fac(lfac$( 1,1)), wctype$ (01+cbas%) , ch(02),~
               at (09,08), fac(lfac$( 2,1)), wctype$ (02+cbas%) , ch(02),~
               at (10,08), fac(lfac$( 3,1)), wctype$ (03+cbas%) , ch(02),~
               at (11,08), fac(lfac$( 4,1)), wctype$ (04+cbas%) , ch(02),~
               at (12,08), fac(lfac$( 5,1)), wctype$ (05+cbas%) , ch(02),~
               at (13,08), fac(lfac$( 6,1)), wctype$ (06+cbas%) , ch(02),~
               at (14,08), fac(lfac$( 7,1)), wctype$ (07+cbas%) , ch(02),~
               at (15,08), fac(lfac$( 8,1)), wctype$ (08+cbas%) , ch(02),~
               at (16,08), fac(lfac$( 9,1)), wctype$ (09+cbas%) , ch(02),~
               at (17,08), fac(lfac$(10,1)), wctype$ (10+cbas%) , ch(02),~
               at (18,08), fac(lfac$(11,1)), wctype$ (11+cbas%) , ch(02),~
               at (19,08), fac(lfac$(12,1)), wctype$ (12+cbas%) , ch(02),~
                                                                         ~
               at (08,11), fac(lfac$( 1, 2)), wcdate$(01+cbas%) , ch(08),~
               at (09,11), fac(lfac$( 2, 2)), wcdate$(02+cbas%) , ch(08),~
               at (10,11), fac(lfac$( 3, 2)), wcdate$(03+cbas%) , ch(08),~
               at (11,11), fac(lfac$( 4, 2)), wcdate$(04+cbas%) , ch(08),~
               at (12,11), fac(lfac$( 5, 2)), wcdate$(05+cbas%) , ch(08),~
               at (13,11), fac(lfac$( 6, 2)), wcdate$(06+cbas%) , ch(08),~
               at (14,11), fac(lfac$( 7, 2)), wcdate$(07+cbas%) , ch(08),~
               at (15,11), fac(lfac$( 8, 2)), wcdate$(08+cbas%) , ch(08),~
               at (16,11), fac(lfac$( 9, 2)), wcdate$(09+cbas%) , ch(08),~
               at (17,11), fac(lfac$(10, 2)), wcdate$(10+cbas%) , ch(08),~
               at (18,11), fac(lfac$(11, 2)), wcdate$(11+cbas%) , ch(08),~
               at (19,11), fac(lfac$(12, 2)), wcdate$(12+cbas%) , ch(08),~
                                                                         ~
               at (08,20), fac(lfac$( 1, 3)), wcstep$(01+cbas%) , ch(07),~
               at (09,20), fac(lfac$( 2, 3)), wcstep$(02+cbas%) , ch(07),~
               at (10,20), fac(lfac$( 3, 3)), wcstep$(03+cbas%) , ch(07),~
               at (11,20), fac(lfac$( 4, 3)), wcstep$(04+cbas%) , ch(07),~
               at (12,20), fac(lfac$( 5, 3)), wcstep$(05+cbas%) , ch(07),~
               at (13,20), fac(lfac$( 6, 3)), wcstep$(06+cbas%) , ch(07),~
               at (14,20), fac(lfac$( 7, 3)), wcstep$(07+cbas%) , ch(07),~
               at (15,20), fac(lfac$( 8, 3)), wcstep$(08+cbas%) , ch(07),~
               at (16,20), fac(lfac$( 9, 3)), wcstep$(09+cbas%) , ch(07),~
               at (17,20), fac(lfac$(10, 3)), wcstep$(10+cbas%) , ch(07),~
               at (18,20), fac(lfac$(11, 3)), wcstep$(11+cbas%) , ch(07),~
               at (19,20), fac(lfac$(12, 3)), wcstep$(12+cbas%) , ch(07),~
                                                                         ~
               at (08,28), fac(lfac$( 1, 4)), wcwc$  (01+cbas%) , ch(04),~
               at (09,28), fac(lfac$( 2, 4)), wcwc$  (02+cbas%) , ch(04),~
               at (10,28), fac(lfac$( 3, 4)), wcwc$  (03+cbas%) , ch(04),~
               at (11,28), fac(lfac$( 4, 4)), wcwc$  (04+cbas%) , ch(04),~
               at (12,28), fac(lfac$( 5, 4)), wcwc$  (05+cbas%) , ch(04),~
               at (13,28), fac(lfac$( 6, 4)), wcwc$  (06+cbas%) , ch(04),~
               at (14,28), fac(lfac$( 7, 4)), wcwc$  (07+cbas%) , ch(04),~
               at (15,28), fac(lfac$( 8, 4)), wcwc$  (08+cbas%) , ch(04),~
               at (16,28), fac(lfac$( 9, 4)), wcwc$  (09+cbas%) , ch(04),~
               at (17,28), fac(lfac$(10, 4)), wcwc$  (10+cbas%) , ch(04),~
               at (18,28), fac(lfac$(11, 4)), wcwc$  (11+cbas%) , ch(04),~
               at (19,28), fac(lfac$(12, 4)), wcwc$  (12+cbas%) , ch(04),~
                                                                         ~
               at (08,33), fac(hex(8c)),     wcwcdescr$(01+cbas%),ch(22),~
               at (09,33), fac(hex(8c)),     wcwcdescr$(02+cbas%),ch(22),~
               at (10,33), fac(hex(8c)),     wcwcdescr$(03+cbas%),ch(22),~
               at (11,33), fac(hex(8c)),     wcwcdescr$(04+cbas%),ch(22),~
               at (12,33), fac(hex(8c)),     wcwcdescr$(05+cbas%),ch(22),~
               at (13,33), fac(hex(8c)),     wcwcdescr$(06+cbas%),ch(22),~
               at (14,33), fac(hex(8c)),     wcwcdescr$(07+cbas%),ch(22),~
               at (15,33), fac(hex(8c)),     wcwcdescr$(08+cbas%),ch(22),~
               at (16,33), fac(hex(8c)),     wcwcdescr$(09+cbas%),ch(22),~
               at (17,33), fac(hex(8c)),     wcwcdescr$(10+cbas%),ch(22),~
               at (18,33), fac(hex(8c)),     wcwcdescr$(11+cbas%),ch(22),~
               at (19,33), fac(hex(8c)),     wcwcdescr$(12+cbas%),ch(22),~
                                                                         ~
               at (08,56), fac(lfac$( 1,5)), wcact$  (01+cbas%) , ch(04),~
               at (09,56), fac(lfac$( 2,5)), wcact$  (02+cbas%) , ch(04),~
               at (10,56), fac(lfac$( 3,5)), wcact$  (03+cbas%) , ch(04),~
               at (11,56), fac(lfac$( 4,5)), wcact$  (04+cbas%) , ch(04),~
               at (12,56), fac(lfac$( 5,5)), wcact$  (05+cbas%) , ch(04),~
               at (13,56), fac(lfac$( 6,5)), wcact$  (06+cbas%) , ch(04),~
               at (14,56), fac(lfac$( 7,5)), wcact$  (07+cbas%) , ch(04),~
               at (15,56), fac(lfac$( 8,5)), wcact$  (08+cbas%) , ch(04),~
               at (16,56), fac(lfac$( 9,5)), wcact$  (09+cbas%) , ch(04),~
               at (17,56), fac(lfac$(10,5)), wcact$  (10+cbas%) , ch(04),~
               at (18,56), fac(lfac$(11,5)), wcact$  (11+cbas%) , ch(04),~
               at (19,56), fac(lfac$(12,5)), wcact$  (12+cbas%) , ch(04),~
                                                                         ~
               at (08,61), fac(hex(8c)),     wcpct$  (01+cbas%) , ch(04),~
               at (09,61), fac(hex(8c)),     wcpct$  (02+cbas%) , ch(04),~
               at (10,61), fac(hex(8c)),     wcpct$  (03+cbas%) , ch(04),~
               at (11,61), fac(hex(8c)),     wcpct$  (04+cbas%) , ch(04),~
               at (12,61), fac(hex(8c)),     wcpct$  (05+cbas%) , ch(04),~
               at (13,61), fac(hex(8c)),     wcpct$  (06+cbas%) , ch(04),~
               at (14,61), fac(hex(8c)),     wcpct$  (07+cbas%) , ch(04),~
               at (15,61), fac(hex(8c)),     wcpct$  (08+cbas%) , ch(04),~
               at (16,61), fac(hex(8c)),     wcpct$  (09+cbas%) , ch(04),~
               at (17,61), fac(hex(8c)),     wcpct$  (10+cbas%) , ch(04),~
               at (18,61), fac(hex(8c)),     wcpct$  (11+cbas%) , ch(04),~
               at (19,61), fac(hex(8c)),     wcpct$  (12+cbas%) , ch(04),~
                                                                         ~
               at (08,66), fac(lfac$( 1,6)), wcsu$   (01+cbas%) , ch(06),~
               at (09,66), fac(lfac$( 2,6)), wcsu$   (02+cbas%) , ch(06),~
               at (10,66), fac(lfac$( 3,6)), wcsu$   (03+cbas%) , ch(06),~
               at (11,66), fac(lfac$( 4,6)), wcsu$   (04+cbas%) , ch(06),~
               at (12,66), fac(lfac$( 5,6)), wcsu$   (05+cbas%) , ch(06),~
               at (13,66), fac(lfac$( 6,6)), wcsu$   (06+cbas%) , ch(06),~
               at (14,66), fac(lfac$( 7,6)), wcsu$   (07+cbas%) , ch(06),~
               at (15,66), fac(lfac$( 8,6)), wcsu$   (08+cbas%) , ch(06),~
               at (16,66), fac(lfac$( 9,6)), wcsu$   (09+cbas%) , ch(06),~
               at (17,66), fac(lfac$(10,6)), wcsu$   (10+cbas%) , ch(06),~
               at (18,66), fac(lfac$(11,6)), wcsu$   (11+cbas%) , ch(06),~
               at (19,66), fac(lfac$(12,6)), wcsu$   (12+cbas%) , ch(06),~
                                                                         ~
               at (08,73), fac(lfac$( 1,6)), wcrun$  (01+cbas%) , ch(07),~
               at (09,73), fac(lfac$( 2,6)), wcrun$  (02+cbas%) , ch(07),~
               at (10,73), fac(lfac$( 3,6)), wcrun$  (03+cbas%) , ch(07),~
               at (11,73), fac(lfac$( 4,6)), wcrun$  (04+cbas%) , ch(07),~
               at (12,73), fac(lfac$( 5,6)), wcrun$  (05+cbas%) , ch(07),~
               at (13,73), fac(lfac$( 6,6)), wcrun$  (06+cbas%) , ch(07),~
               at (14,73), fac(lfac$( 7,6)), wcrun$  (07+cbas%) , ch(07),~
               at (15,73), fac(lfac$( 8,6)), wcrun$  (08+cbas%) , ch(07),~
               at (16,73), fac(lfac$( 9,6)), wcrun$  (09+cbas%) , ch(07),~
               at (17,73), fac(lfac$(10,6)), wcrun$  (10+cbas%) , ch(07),~
               at (18,73), fac(lfac$(11,6)), wcrun$  (11+cbas%) , ch(07),~
               at (19,73), fac(lfac$(12,6)), wcrun$  (12+cbas%) , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), screen$(screen%,1)     , ch(79),~
               at (23,02), fac(hex(8c)), screen$(screen%,2)     , ch(79),~
               at (24,02), fac(hex(8c)), screen$(screen%,3)     , ch(79),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 13% then L49140
                  call "MANUAL" ("JBINPKIT")
                  goto L47540

L49140:        if keyhit% <> 8% then L49180
                  call "RTEDSPLY" (part$, rteid$, #19, #4)
                  goto L47540

L49180:        if keyhit% <> 15% then L49220
                  call "PRNTSCRN"
                  goto L47540

L49220:        if screen%=1% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the items on page 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                errormsg$ = " "
                on fieldnr%  gosub  L50210,         /* Job Number       */~
                                    L50410,         /* Control Number   */~
                                    L50420,         /* Description      */~
                                    L50450,         /* Part Number      */~
                                    L50710,         /* Quantity to Make */~
                                    L51120,         /* WIP Acct         */~
                                    L51230,         /* Planned Start    */~
                                    L51410,         /* Planned End      */~
                                    L51540,         /* Actual Start     */~
                                    L51580,         /* BOM ID           */~
                                    L51750,         /* RTE ID           */~
                                    L51900,         /* Core WIP (Maybe) */~
                                    L51960          /* Est. Perc. Comp. */
                return

L50210
*        Test data for JOB NUMBER
            if job$ <> " " then L50270
                call "GETCODE" (#3, job$, " ", 0%, 0, found%)
                if found% <> 0% then L50330
                     errormsg$ = hex(00)
                     return
L50270:     call "READ100" (#3, job$, found%)
            if found% > 0% then L50330
                if str(job$,1%,2%) <> "PJ" then L50290
                     errormsg$ = "New Job Numbers CANNOT Begin With 'PJ'"
                     return
L50290:         call "READ100" (#23, job$, f1%(23))
                if f1%(23) = 0% then return
                     errormsg$ = "Job Number already used as a Project."
                     return
L50330:     u3% = 2%   /* Check In-use & Write In-use Flag if free */
            call "JBINUSE" (job$, u3%)
                if u3% = 0% then L50370
                errormsg$ = hex(00)
                return
L50370:     gosub dataload
            return clear all
            if errormsg$ <> " " then inputmode_special
            goto editmode

L50410
*        Test data for CONTROL number
            return

L50420
*        Test data for DESCRIPTION and CONTROL number
            return

L50450
*        Test data for PART NUMBER
            call "GETCODE" (#4, part$, partdescr$, 1%, .32, f1%(4))
            if f1%(4) <> 0% then L50500
                errormsg$ = "Part Not On File: " & part$
                return
L50500:     gosub check_for_restrictions
            if errormsg$ <> " " then return
                get #4 using L50514, uom$, type$
L50514:             FMT POS(74), CH(4), POS(180), CH(3)
                if jbdescr$ = " " then get #4, using L50530, jbdescr$
L50530:              FMT XX(25), CH(32)
*              Check for ECRs this Part
                ecrpfk$ = "(11)"
            call "ECRINQSB" ("C",        /* "C" to Check for ECR Info  */~
                                         /*    and return PFKey Prompt */~
                             part$,      /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                                         /* OUT: Formatted PFKey Prompt*/~
                                         /*    Will be BLANK if no ECRs*/~
                             #02,        /* SYSFILE2                   */~
                             #04)        /* HNYMASTR                   */
                if wipsave$ <> " " then return
                     readkey$ = part$
                     call "READ100" (#4, readkey$, f1%(4))
                     if f1%(4) = 1% then L50600
                          wipsave$ = " "
                          return
L50600:              get #4, using L50610, wipsave$
L50610:                   FMT POS(344), CH(9)
                     call "GLFMT" (wipsave$)
                     call "GETCODE" (#6, wipsave$, wipdescr$,            ~
                                                         1%, 99, f1%(6))
                     return

L50710
*        Test data for QUANTITY TO MAKE
            p% = pos(qtymake$ = "P")
            if p% = 0% then L50880
                str(qtymake$, p%, 1) = " "
                call "NUMTEST" (qtymake$, 0, 9e7, errormsg$, -0.2, qtyp)
                if errormsg$ <> " " then return
                     call "READ100" (#4, part$, f1%(4))
                     if f1%(4) = 0% then L50880
                          get #4, using L50800, moq$, pz
L50800:                        FMT POS(190), CH(10), POS(326), PD(14,4)
                     moq% = 0%
                     convert moq$ to moq%, data goto L50830
L50830:              qtyp = max(qtyp, moq%)
                     if pz < .0001 then L50870
                     if abs(mod(qtyp-moq%,pz)) < .00001 then L50870
                          qtyp=round(qtyp+pz-mod(qtyp-moq%,pz),2)
L50870:                   call "CONVERT" (qtyp, 0.2, qtymake$)
L50880:     call "NUMTEST" (qtymake$, 0, 9e7, errormsg$, -0.2, qtymake)
            if errormsg$ <> " " then return
            if qtymake >= qtycomp then L50892
                errormsg$ = "Completed Units Exceeds Quantity To Make"
                return

L50892:     minqty, rcvholdqty, qcqty, qcholdqty, po_openqty = 0
            sync% = 0%
            readkey$ = str(vendor$) & str(po$) & str(poline$)
            call "READ100" (#28, readkey$, f1%(28%))
                if f1%(28%) = 0% then L50908
            sync% = 1%
            get #28 using L50906, rcvholdqty, qcqty, qcholdqty
L50906:         FMT POS(373), 3*PD(14,4)
L50908:     minqty = qtycomp + rcvholdqty + qcqty + qcholdqty
            if qtymake >= minqty then L50930
                convert minqty to minqty$, pic(######.##)
                errormsg$ = "Current Job Qty CANNOT Be Less Than " &     ~
                            minqty$
                return

L50930:     call "SERENABL" (part$, enabled%, u3%, #2, #4)
            if enabled% = 1% then gosub enter_serial_numbers
            if errormsg$ > " " then return
            if found% <> 0% then L51000
                qtycomp$, qtyleft$, qtyscrp$, qtyrewk$ = "         0"
                estperccomp$ = "       0" : estperccomp = 0
                qtyorig = qtymake
                call "CONVERT" (qtyorig, 0.2, qtyorig$)
L51000:     qtyadj = -(qtyorig - qtyscrp - qtyrewk - qtymake)
            call "CONVERT" (qtyadj, 0.2, qtyadj$)
            qtyleft = qtymake - qtycomp
            call "CONVERT" (qtyleft, 0.2, qtyleft$)
            return

          enter_serial_numbers
            plowkey$ = job$
            call "SERINSUB" (part$," "," ",qtymake-qtycomp, 1%, 1%, "WP",~
                             plowkey$, errormsg$, #2, #4, #21, #22)
            return

L51120
*        Test data for WIP ACCT
            call "GETCODE" (#6, wipacct$, wipdescr$, 1%, 0, f1%(6))
            if f1%(6) <> 0% then L51170
                errormsg$ = "Account Not On File"
                return
L51170:     get #6, using L51180, gltype$
L51180:         FMT XX(39), CH(1)
            if gltype$ = "A" then return
                errormsg$ = "Not An Asset Account: " & wipacct$
                return

L51230
*        Test data for PLANNED START
            call "DATEOKC" (plstdate$, err%, errormsg$)
            if errormsg$ <> " " then return
                temp$ = plstdate$
                call "DATUFMTC" (temp$)
                plstdate8$ = temp$
                call "DATEFMT" (plstdate8$)
                call "PIPINDEX" (#2, temp$, stdate%, u3%)
                if u3% = 0% then L51360
                     if u3% = 2% then L51330
                          errormsg$ = "Can't Validate Against Calendar"
                          return
L51330:              if stdate% < 490% then L51360
                          errormsg$ = "Date is outside Planning Calendar"
                          return
L51360:         if plenddate$ = " " or plenddate$ = blankdate$ then return
                if stdate% <= enddate% then return
                     errormsg$ = "Can't Be After Planned End Date"
                     return

L51410
*        Test data for PLANNED FINISH
            call "DATEOKC" (plenddate$, err%, errormsg$)
            if errormsg$ <> " " then return
                temp$ = plenddate$
                call "DATUFMTC" (temp$)
                call "PIPINDEX" (#2, temp$, enddate%, u3%)
                if u3% = 0% then L51500
                     errormsg$ = "Date is outside Planning Calendar"
                     return
L51500:         if stdate% <= enddate% then return
                     errormsg$ = "Can't Be Before Planned Start Date"
                     return

L51540
*        Test data for ACTUAL START
            call "DATEOKC" (stdate$, err%, errormsg$)
            return

L51580
*        Test data for BOM ID
            rteloaded$ = " " /* so we force a reload of the route when */
                             /* they change BOM, 'cause phantom assys  */
            if bomid$ <> " " then L51630
                rteid$ = " "
                gosub load_rte
                return
L51630:     readkey$ = str(part$) & bomid$
            inpmessage$ = hex(06) & "Select the BOM to use for " &       ~
                                    "Part " & part$
            call "PLOWCODE" (#11, readkey$, inpmessage$, 2025%, .30,     ~
                             f1%(11), header$(), 3)
            if f1%(11) = 0% then L51710
                bomid$ = str(readkey$,26%,3%)
                return
L51710:     errormsg$ = "BOM: " & bomid$ & " Not on File... " &          ~
                        "Re-enter Or Leave Blank"
            return

L51750
*        Test data for RTE ID
            if rteid$ = " " then L51870
                readkey$ = str(part$) & rteid$
                inpmessage$ = hex(06) & "Select the Route to use for " & ~
                                        "Part " & part$
                call "PLOWCODE" (#19, readkey$, inpmessage$, 2025%, 0,   ~
                                 f1%(19), header$(), 3)
                if f1%(19) <> 0% then L51860
                     errormsg$ = "Route: " & rteid$ & " Not on File... "&~
                                 "Re-enter Or Leave Blank"
                     return
L51860:         rteid$ = str(readkey$,26%,3%)
L51870:     gosub load_rte
            return

L51900
*        Test data for Core Wip Account
            jbcorewipd$ = " "
            if jbcorewip$ = " " then return
            call "GETCODE" (#6, jbcorewip$, jbcorewipd$, 1%, 0, f1%(6))
            if f1%(6) <> 0% then return
                errormsg$ = "Account Not On File"
                return

L51960
*        Test data for Estimated Percentage of Complete
            call "NUMTEST" (estperccomp$, 0, 9e2, errormsg$, -2.2,       ~
                            estperccomp)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the kitting line items                     *~
            *************************************************************

            deffn'153(c%,fieldnr%)
            pipouts_changed% = 1%
            errormsg$=" "
            on fieldnr% gosub L53150,               /* PART             */~
                              L53330,               /* DESCRIPTION      */~
                              L53360,               /* QUANTITY         */~
                              L53410                /* DATE OUT         */
            return

L53150: REM TEST DATA FOR PART
            if part$(c%) <> part$ then L53190
                errormsg$="Cannot Commit Part To Build For Jobs"
                return
L53190:     partdescr$(c%,1%) = hex(06) & "Select the Part to Kit"
            call "GETCODE" (#4, part$(c%),partdescr$(c%,1%),0%,.32,f1%(4))
            if f1%(4) <> 0% then L53260
                errormsg$ = "Part Not On File"
                return
L53260:     get #4, using L53270, partdescr$(c%,2%), parttype$
L53270:         FMT POS(74), CH(4), POS(180), CH(3)
            convert parttype$ to type%, data goto L53300
            if type% = 0% or type% > 199% then return
L53300:         errormsg$="Not A Planned Part... Type=" & parttype$
                return

L53330: REM TEST DATA FOR DESCRIPTION
            return

L53360: REM TEST DATA FOR QUANTITY
            call "NUMTEST" (quantity$(c%),-9e7,9e7, errormsg$,-0.2,temp)
            if errormsg$ = " " then newquantity(c%) = temp
            return

L53410: REM TEST DATA FOR DATE
            call "DATEOK" (outdate$(c%), u3%, errormsg$)
                if errormsg$<>" " then return
            lastdate$ = outdate$(c%) /* DEFAULT FOR NEXT LINE OM INPUT */
            temp$=outdate$(c%)
            call "DATUNFMT" (temp$)
            call "PIPINDEX" (#2, temp$, newdate%(c%), u3%)
                if u3% = 0 then return
                errormsg$ = "Date is outside planning Calendar"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the routing line items.                    *~
            *************************************************************

            deffn'154(c%,fieldnr%)
            errormsg$ = " "
            wcouts_changed% = 1%
            on fieldnr% gosub L54170,               /* WCOUT REC TYPE   */~
                              L54400,               /* DATE             */~
                              L54990,               /* ROUTE STEP       */~
                              L55380,               /* WORK CENTER      */~
                                   ,               /* ACTIVITY CODE    */~
                              L55720                /* SET UP/RUN HOURS */
            return

L54170: REM TEST DATA FOR RECORD TYPE
                if wctype$(c%) = "  " then L54300
                if wctype$(c%) = "MQ" then L54300
                if wctype$(c%) = "C1" then L54270
                if wctype$(c%) = "C2" then L54270
                if wctype$(c%) = "C3" then L54270
                if wctype$(c%) = "MA" then L54270
                     errormsg$= "Must Be 'MQ', 'C1', 'C2', 'C3', or 'MA' ~
        ~or Blank"
                     return
L54270:     if c% > 1 then L54300
                     errormsg$ = "Must Be Preceeded By A Step"
                     return
L54300:     if str(wctype$(c%),,1%) = "M" then gosub test_mq_date
                if errormsg$ <> " " then return
            if wctype$(c%) = "MA" then L54350
            if str(wctype$(c%),,1) <> "C" then return
            if wctype$(c%) <> wctype$(c%-1) then L54350
                     errormsg$ = "Can't be same as prev. line"
                     return
L54350:     if wcstep$(c%) = " " or wcstep$(c%)=wcstep$(c%-1) then return
                     errormsg$ = "Primary WC (prev. line) Must Have Same ~
        ~Step Value."
                     return
        test_mq_date
            if wcdate$(c%) = " " or wcdate$(c%) = blankdate$ then return
                                                 /* Must be Inputmode */
                mat cursor% = zer
                search wcdate$() = wcdate$(c%) to cursor%() step 8
                if cursor%(2%) = 0% then L54380
                     errormsg$ = wctype$(c%) & " Can't Be On Same Day" & ~
                                                   " As Other Activities"
                     return
L54380:         percent, wcrun, wcsu = 0
                wcpct$(c%) = "  0%"
                wcrun$(c%) = "      0" : wcsu$(c%) = "     0"
                return

L54400: REM TEST DATA FOR DATE
            call "DATEOK" (wcdate$(c%), u3%, errormsg$)
                if errormsg$<>" " then return
            mat cursor% = zer
            search wcdate$() = wcdate$(c%) to cursor%() step 8
            if cursor%(2%) = 0 then L54490
            if str(wctype$(c%),,1%) <> "M" then L54450
                errormsg$ = wctype$(c%) & " Can't Be On Same Day " &     ~
                            "As Other Activities"
                return
L54450:     u3% = ((cursor%(1%) - 1%) / 8%) + 1%
            u4% = ((cursor%(2%) - 1%) / 8%) + 1%
            if u3% = c% then u3% = u4%
            if str(wctype$(u3%),,1) <> "M" then L54490
                errormsg$ = wctype$(u3%) & " is Already Scheduled on " & ~
                                           "that Day"
                return
L54490:     temp$ = wcdate$(c%)
            call "DATUNFMT" (temp$)
            temp% = max(c%-1%,1%)
            temp1$ = wcdate$(temp%)
            call "DATUNFMT" (temp1$)
            if temp$ >= temp1$ then L54580
                call "DATEFMT" (temp1$)
                errormsg$ = "Date Must Be On Or After " & temp1$
                return
L54580:     if c% >= maxwcout% then L54650
            temp1$ = wcdate$(c%+1%)
            call "DATUNFMT" (temp1$)
            if temp$ <= temp1$ then L54650
                call "DATEFMT" (temp1$)
                errormsg$ = "Date Must Be On Or Before " & temp1$
                return
L54650:     call "PIPINDEX" (#2, temp$, temp%, u3%)
                if u3% = 0% then L54700
                errormsg$ = "Date is outside Planning Calendar"
                return

L54700:     if wcdate$(c%) = olddate$ then return
            if olddate$ = " " or olddate$ = blankdate$ then return
            REM Adjust percentages shown on screen...
            tempwc$ = wcwc$(c%)
            wcrun, wcsu = 0
            convert wcrun$(c%) to wcrun, data goto L54760
L54760:     convert wcsu$(c%) to wcsu, data goto L54770
L54770:     temp1 = wcrun + wcsu
            if temp1 = 0 then L54840
               REM Reverse old days usage...
               tempdate$ = olddate$
               gosub L56050 /* Just to get factor */
               change% = -int(temp1/adjfactor)
               if change% <> 0% then gosub L55930
L54840:     hit% = 0%
            for i% = 1% to maxwcout%
                if i% = c% or wcwc$(i%) <> wcwc$(c%) then L54910
                if wcdate$(i%) <> wcdate$(c%) then L54910
                   wcpct$(c%) = wcpct$(i%)
                   hit% = 1%
                   i% = maxwcout%
L54910:     next i%
            if hit% = 0% then gosub calc_usage
            tempdate$ = wcdate$(c%)
            gosub L56050 /* Just to get factor */
            change% = int(temp1/adjfactor)
            if change% <> 0% then gosub L55930
            return

L54990: REM TEST DATA FOR ROUTE STEP
            if wcstep$(c%) <> " " then L55030
                errormsg$ = "Sorry, Route Step Can't Be Blank."
                return

L55030:     if str(wcstep$(c%),5,1) = "-" then L55110

            if str(wcstep$(c%),5,3) = " " then L55090
                errormsg$ = "Step can't be longer than 4 unless a dash in~
        ~ fifth position & Phantom number."
                return

L55090:         temp% = 0%
                goto unformat_step

L55110:     convert str(wcstep$(c%),6,2) to temp%, data goto L55130
              if temp% < 1% or temp% > 99% then L55120
            convert temp% to str(wcstep$(c%),6,2), pic(00)
            goto unformat_step

L55120:     errormsg$ = "Phantom indicator must be betweem 1 and 99"
               return

L55130:     errormsg$ = "Phantom indicator must be a two-digit number fol~
        ~lowing the dash"
               return

        unformat_step
             temp$ = str(wcstep$(c%),1,4) &  bin(temp%)
                REM Insure steps don't See Repeat Out Of Sequence...
                mat cursor% = zer
                search wcstep$() = str(wcstep$(c%)) to cursor%() step 7
                     if cursor%(2) = 0% then L55320
                if c% < 2% then L55280
                if wcstep$(c%-1%) = wcstep$(c%) then L55320
L55280:         if c% >= maxwcout% then L55300
                if wcstep$(c%+1%) = wcstep$(c%) then L55320
L55300:              errormsg$="Sorry, Step Can't Repeat Out Of Sequence"
                     return
L55320:         gosub load_rte
                search steps$() = str(temp$,,5) to cursor%() step 5
                if cursor%(1) = 0% then wcseq%(c%) = 0% else             ~
                                            wcseq%(c%) = (cursor%(1)+4)/5
                return

L55380: REM TEST DATA FOR WORK CENTER
            call "GETCODE" (#14, wcwc$(c%), wcwcdescr$(c%), 0%,0,f1%(14))
                if f1%(14) <> 0% then L55430
                   errormsg$ = "Work Center Not On File"
                   return
L55430:     if wcwc$(c%) = oldwc$ then return
            REM Adjust percentages shown on screen...
            tempdate$ = wcdate$(c%)
            wcrun, wcsu = 0
            convert wcrun$(c%) to wcrun, data goto L55480
L55480:     convert wcsu$(c%) to wcsu, data goto L55490
L55490:     temp1 = wcrun + wcsu
            if temp1 = 0 then L55570
            if oldwc$ = " " then L55570
               REM Reverse old WC's usage on this day...
               tempwc$ = oldwc$
               gosub L56050 /* Just to get factor */
               change% = -int(temp1/adjfactor)
               if change% <> 0% then gosub L55930
L55570:     hit% = 0%
            for i% = 1% to maxwcout%
                if i% = c% or wcwc$(i%) <> wcwc$(c%) then L55640
                if wcdate$(i%) <> wcdate$(c%) then L55640
                   wcpct$(c%) = wcpct$(i%)
                   hit% = 1%
                   i% = maxwcout%
L55640:     next i%
            if hit% = 0% then gosub calc_usage
            tempwc$ = wcwc$(c%)
            gosub L56050 /* Just to get factor */
            change% = int(temp1/adjfactor)
            if change% <> 0% then gosub L55930
            return

L55720: REM TEST DATA FOR SET UP AND RUN HOURS
            call "NUMTEST" (wcsu$(c%), 0, 9e7, errormsg$, -0.2, temp)
                if errormsg$ <> " " then return
            tempwc$ = wcwc$(c%)
            tempdate$ = wcdate$(c%)
            gosub L56050
            call "CONVERT" (temp, 0.2, wcsu$(c%))
            oldsu = 0
            convert oldsu$ to oldsu, data goto L55810
L55810:     change% = temp - oldsu

            call "NUMTEST" (wcrun$(c%), 0, 9e7, errormsg$, -0.2, temp)
                if errormsg$ <> " " then return
            gosub L56050
            call "CONVERT" (temp, 0.2, wcrun$(c%))
            oldrun = 0
            convert oldrun$ to oldrun, data goto L55890
L55890:     temp1 = change% + (temp - oldrun)
            change% = int(temp1/adjfactor)
            if change% = 0% then return

L55930:     REM Make Percentage Change, Roll change through lines...
            dayavail% = val(str(wcpct$(c%),5),2)
            if dayavail% = 0% then L56030
            convert str(wcpct$(c%),,3) to temp%, data goto L56030
            percent = max(min(temp% + 100*change%/dayavail%, 999),0)
            call "CONVERT" (percent, 0.0, str(wcpct$(c%),,3))
            for i% = 1% to maxwcout%
                if i% = c% or wcwc$(i%) <> tempwc$ then L56020
                if wcdate$(i%) = tempdate$ then wcpct$(i%) = wcpct$(c%)
L56020:     next i%
L56030:     return

L56050:     REM Simulates Datasave Effect to Insure no surprizes later...
            adjfactor = .00000001
            call "WCUN2HRS" (#14, tempwc$, factor, 0, " ")
            if factor <> 0 then adjfactor = 24/factor
            temp = int(temp/adjfactor)
            call "WCUN2HRS" (#14, tempwc$, 0, temp, " ")
            return

        calc_usage
            temp$ = wcdate$(c%)
            call "DATUNFMT" (temp$)
            call "PIPINDEX" (#2, temp$, day%, u3%)
            gosub'99(wcwc$(c%), day%, 0%)
            wcpct$(c%) = temp$
        return

        view_ecr_info                    /* View ECR History this Part */
            call "ECRINQSB" ("A",        /* "A" - Show ALL ECRs this   */~
                                         /*    part, open or closed.   */~
                             part$,      /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                             #02,        /* SYSFILE2                   */~
                             #04)        /* HNYMASTR                   */
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
