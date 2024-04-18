        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR    CCC   V   V  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  R   R  C   C  V   V    I    NN  N  P   P  U   U    T     *~
            *  RRRR   C      V   V    I    N N N  PPPP   U   U    T     *~
            *  R   R  C   C   V V     I    N  NN  P      U   U    T     *~
            *  R   R   CCC     V    IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVINPUT - Receiver oriented P.O. receipts.  Allows mult- *~
            *            iple Vendors/PO's per receiver.  Better inter- *~
            *            face to Payables, on-hand distribution, and QC *~
            *            distribution.                                  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/08/86 ! Original                                 ! KAB *~
            * 02/09/87 ! Enhanced Lot Tracking                    ! ERN *~
            * 05/19/87 ! Standard Costing Enhancements            ! ERN *~
            * 12/02/87 ! Corrected call to POSTATUS               ! HES *~
            * 02/24/87 ! Added Enhanced Access To inventory       ! MDE *~
            * 02/24/87 ! Soft Enables First Screen                ! MDE *~
            * 03/11/88 ! Receiver Ticket Printing                 ! MDE *~
            * 06/16/88 ! Renumbered to Standard                   ! MDE *~
            * 06/16/88 ! Fixed CARFRIEGHT INITIALIZATION          ! MDE *~
            * 06/16/88 ! SET QTYONORD = 0 IF A NEGATIVE AMT       ! MDE *~
            * 08/24/88 ! Added functionality to PO Info Scr (48000) RJM *~
            *          !   Fixed Job Number truncation @ FN'103   !     *~
            * 09/13/88 ! Added Paging PF Keys to PO Info Screen   ! RJM *~
            * 09/14/88 ! More Line Item Info Screen Mods.         ! RJM *~
            * 09/21/88 ! Added max. % & units over receipt.       ! JIM *~
            * 08/21/89 ! Added calls to VBKSWTCH for Default Rcv  ! JDH *~
            *          !  mode and Receiver Print. Thanks Marty.  !     *~
            * 10/18/90 ! Added check for lot tracked part and set ! MJB *~
            *          !  receive mode to 'D'.                    !     *~
            * 01/04/91 ! Added testing of dates for posting period! RAC *~
            * 01/25/91 ! Added call to VBKLIDSP.                  ! JDH *~
            * 06/26/91 ! Added PF24 access to HNYLCSUB permitting ! MLJ *~
            *          !   Location Management.                   !     *~
            * 07/11/91 ! Added ALLFREE.  Fixed problem with lot   ! JDH *~
            *          !   protected parts.                       !     *~
            * 12/03/91 ! PRR 12127.  Added posting date check for ! MLJ *~
            *          !   MASTER, 1/4/91 fix only checked TIF.   !     *~
            * 04/15/92 ! PRR 10741 - Added PF'(25)Part Text' on   ! MLJ *~
            *          !   Line Item Information Screen.          !     *~
            *          ! PRR 11576 - Added PF'(14)UOM Toggle' on  !     *~
            *          !   Line Summary in Edit Mode.             !     *~
            *          ! PRR 12193 - Overshipment calculation now !     *~
            *          !   includes returned quantities.          !     *~
            * 09/23/92 ! Passes Receiver Ticket Size to RCVTKTSB. ! JDH *~
            * 10/07/93 ! Added PIPMASTR for Purch. Job testing.   ! KAB *~
            * 12/10/93 ! PRR 11760. Warn if item has been invoiced! JDH *~
            *          ! PRR 12465. OK to distribute straight to  !     *~
            *          !   Inv for lot protected part if new Rcvr.!     *~
            *          ! PRR 12957. Extended posting period check !     *~
            *          !   to non-current period.                 !     *~
            *          ! PRR 12583. Rcvr# not enabled in Edit mode!     *~
            * 03/29/94 ! PRR 13152. PF28 Reset Qty only available ! JDH *~
            *          ! for new POs on a rcvr; else use POStrtOvr!     *~
            * 03/31/94 ! Test buffers for posting in process.     ! KAB *~
            * 04/20/94 ! PRR 12960. Honors switch to use the std  ! JDH *~
            *          !  cost as the inventory cost.             !     *~
            * 09/30/94 ! PRR 13297- Can now Input > 12 PO's.      ! RJH *~
            * 11/14/94 ! Switch above (04/20/94) excluded PJs.    ! JDH *~
            * 12/05/94 ! Added Vendor Channel to RCVTKTSB Call    ! RJH *~
            * 12/20/94 ! PRR 13333. Fixed undistributed qty on    ! JDH *~
            *          !   PF16 Edit.  Caused out-of-bal postings.!     *~
            * 01/06/95 ! Corrected my mistake of 04/20/94 by      ! JDH *~
            *          !   properly considering cores.            !     *~
            * 04/04/95 ! Pass store into HNYLCSUB.                ! JDH *~
            * 05/19/95 ! Added PF23 Find Part to LI Summary Screen! JDH *~
            * 07/19/95 ! Now consistent with PORELSUB and RCVINPUT! JDH *~
            *          !   with regards to receiving at standard. !     *~
            *          !   If flagged to, a PJ item uses its 'This!     *~
            *          !   Level' std cost for receiving at std,  !     *~
            *          !   even if it is a core part.             !     *~
            * 10/25/95 ! Adding the original due date to RCVTIF2. ! JDH *~
            * 07/02/96 ! PRR 13575 - Corrected display resulting  ! MLJ *~
            *          !   from a PF23 FIND when part is on a line!     *~
            *          !   greater than 87.                       !     *~
            * 09/06/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**
        dim                                                              ~
            a_bom(12),                   /* Bill of Material Costs     */~
            a_dtl(12),                   /* Misc Cost Details          */~
            a_rte(12),                   /* This Level Costs           */~
            askhdr$40,                   /* Ask User Header            */~
            askpf1$80,                   /* Ask User Message           */~
            askmid$80,                   /* Ask User Message           */~
            askpf2$80,                   /* Ask User Message           */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bol$30,                      /* Bill of Lading             */~
            cardescr$30,                 /* Carrier Description        */~
            carfacct$12,                 /* Carrier Freight Acct       */~
            carfacctdescr$30,            /* Carrier Freight Acct Descr */~
            carpacct$12,                 /* Carrier Payables Acct      */~
            carfrieght$10,               /* Carrier Freight            */~
            carvencode$9,                /* Carrier Vendor Code        */~
            costtype$1,                  /* Costing Method for Part    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            deffrgtacct$9,               /* Default Freight Account    */~
            defppayacct$9,               /* Default Pre-Payables Acct  */~
            defrcvddate$8,               /* Default Received Date      */~
            delvr$(100)20,               /* Deliver Received Items To  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdr$(2)79,                   /* PLOWCODE Argument          */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(1),                /* PLOWCODE Argument          */~
            incl_excl$(1)3,              /* PLOWCODE Argument          */~
            index%(100),                 /* Ser. Max Index per Line    */~
            inpmessage$80,               /* Informational Message      */~
            invcost$10,                  /* Inventory Cost for HNYCDIST*/~
            lfac$(80)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line3$79,                    /* Third Line of Screen Headr */~
            lines$(100)50,               /* Inventory Distribution     */~
            lot_sys$1, lot_unique$1,     /* Lot System Flags           */~
            lottrack$(100)1,             /* Lot flag from part         */~
            modedflt$1,                  /* Rcvr Mode Default Value    */~
            ohpost$1,                    /* Receipt mode option        */~
            oldreadkey$100,              /* G P. Key string            */~
            p%(1),                       /* Stupid Receiver for SEARCH */~
            pfmsg1$79,                   /* First PF Key line          */~
            pfmsg2$79,                   /* Second PF Key line         */~
            pfmsg3$79,                   /* Third PF Key line          */~
            pfkey$32,                    /* Active PF Keys             */~
            pgmname$8,                   /* Current Program Name       */~
            packslip$(100)16,            /* Vendor's Packslip          */~
            miscplow$99,                 /* PLOWKEY for Payables & Hold*/~
            ponumber$(100)16,            /* PO Numbers                 */~
            postatstdcost$1,             /* Post cost at std cost      */~
            prtopt$1,                    /* Receiver Print Option      */~
            qcacct$9,                    /* Q.C. Holding Account       */~
            rcvacct$9,                   /* Receivered Holding Account */~
            rcvddate$8,                  /* Default Received Date      */~
            rcvhnyds$(2500)50,           /* Receiver Inventory Dist.   */~
            rcvhnyds%(2500),             /* Receiver Inv. Dist. Index  */~
            rcvrno$16,                   /* Receiver Number            */~
            rcvtime$8,                   /* Time of Receipt            */~
            rcvcomment$30,               /* Whatever                   */~
            record$(16)50,               /* Receiver Line for Pass     */~
            remanpart$99,                /* Reman - Core Part Keys     */~
            reqstd$(100)20,              /* Who Requested Items        */~
            search_part$25,              /* Part to Search for         */~
            status$30,                   /* Line item status descriptn */~
            stdcost(12),                 /* Standard Cost              */~
            text$(84,1)70,               /* Text file work Area        */~
            trankey$40,                  /* Serial Number Trans. Key   */~
            subh$(3)80,                  /* Subroutine Header block    */~
            userid$3,                    /* Current User Id            */~
            vencode$(100)9,              /* Vendors on this recver     */~
            venname$(100)30,             /* Vendor's Name              */~
            xref%(1,15),                 /* Soft Enables x -ref        */~
            set%(255)                    /* Enable settings array      */~

        dim                                                              ~
            basedate$8,                  /* PLANNING BASE DATE         */~
            diskkey$60,                                                  ~
            headsc$(5)79,                /* HEADER FOR SCREEN          */~
            ospct$8,                     /* OVERSHIP ALLOANCE          */~
            osmsg$60,                    /* CANNED MESSAGE PREAMBLE    */~
            paydate$8,                   /* PAYABLES POSTING DATE      */~
            tempven$9,                   /* TEMPORARY VENCODE$         */~
            userstr$3,                   /* USER DEFAULT STORE         */~
            qtysave(8),                  /* SAVE LINE QTY'S FOR RESTRT */~
            workkey$79                   /* A Work Key                 */

        dim                                                              ~
            defstr$3,                    /* DEFAULT STORE NUMBER       */~
            orddate$8,                   /* ORDER DATE                 */~
            puracct$16                   /* LIABILITY ACCOUNT          */~

        dim                                                              ~
            qtyorig$(100,2)10,           /* OUR ORIGINAL ORDER         */~
            qty_uom$(13)10,              /* Orig Qty or UOM display    */~
            qtyonord$(100,2)10,          /* OUR REM ON ORDER           */~
            qtyrecd$(100,2)10,           /* OUR REC THIS SHIPMENT      */~
            qtyrecdtd$(100,2)10,         /* OUR PRIOR REC TO DATE      */~
            max_to_receive(100),         /* Overage allowed            */~
                                                                         ~
            acct$(100)16,                /* EXPENSE ACCOUNT            */~
            cat$(100)4,                  /* PART CATEGORY              */~
            cost$(100)104,               /* Inventory Costs            */~
            date1$8,                     /* LOW DATE FOR RECEIPT       */~
            date2$8,                     /* HIGH DATE FOR RECEIPT      */~
            duedate$(100)8,              /* DUE DATE - Current         */~
            dueorig$(100)6,              /* DUE DATE - Original        */~
            job$(100)8,                  /* Project/Job Number         */~
            lot$(100)6,                  /*  LOT CODE                  */~
            meas$(100)4,                 /* UNIT OF ISSUE              */~
            ohpost$(100)1,               /* ON HAND POST LINE ITEM     */~
            part$(100,2)25,              /* PART NUMBER                */~
            partdescr$(100,2)32,         /*  PART DESCRIPTION          */~
            rejcode$(100)6,              /* REJECTION (RTV) CODE       */~
            rejlot$(100)6,               /* REJECT HOLDING LOT         */~
            rejstr$(100)6,               /* REJECT HOLDING STORE       */~
            seqnr$(100)3,                /* LINE ITEM SEQUENCE #       */~
            status$(100)1,               /* LINE ITEM STATUS           */~
            str$(100)3,                  /*  STORE NUMBER              */~
            textpt$4,                    /* Part Number TEXT LINK      */~
            textpo$(100)4,               /* P.O. TEXT LINK             */~
            textrc$(100)4,               /* RCVR TEXT LINK             */~
            textqc$(100)4,               /* Q.C. TEXT LINK             */~
                                                                         ~
            factor(100),                 /* CASE MULTIPLIER            */~
            price(100),                  /* PURCHASE PRICE             */~
            ext(100),                    /* LINE ITEM EXTENSION        */~
            qtyorig(100),                /* ORIGINAL ORDER QUANTITY    */~
            qtyonord(100),               /* REMAINING ON ORDER         */~
            oldqtyonord(100),            /* REMAINING ON ORDER         */~
            qtyrecd(100),                /* REC'D THIS RUN             */~
            qtyrecdtd(100),              /* REC'D TO DTE NOT INCL. RECD*/~
            qtyqchold(100),              /* QUANTITY IN QC HOLD        */~
            qtyqcpend(100),              /* QTY IN Q/C NOT PROCESSED   */~
            qtyrcvhold(100),             /* QTY NOT DISPERSED          */~
            qtyrtv(100),                 /* QTY RETURNED TO VENDOR     */~
            qtyonhand(100),              /* QTY MOVED TO ON HAND       */~
            qtyscrap(100),               /* QTY SCRAPPED/REJECTED      */~
            uom$(100,2)4                 /* Stocking/Shipping UOM's    */~

        dim                              /* HNYHOLD RECORD AREAS       */~
            bufflot$(2600)6,             /*                            */~
            buffstr$(2600)3,             /*                            */~
            buffqty (2600),              /*                            */~
            buffidx%(2600),              /*                            */~
                                                                         ~
            postlot$(2600)6,             /*                            */~
            poststr$(2600)3,             /*                            */~
            postqty (2600),              /*                            */~
            postidx%(2600),              /*                            */~
                                                                         ~
            savemark$(2600)1             /*                            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con
            prtd% = 0%
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! Users Default Information File           *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! VENDOR   ! VENDOR MASTER FILE                       *~
            * # 4 ! VBKMASTR ! Backlog main header file                 *~
            * # 5 ! VBKLINES ! Backlog line item file                   *~
            * # 6 ! HNYMASTR ! Inventory Master File.                   *~
            * # 7 ! PAYMASTR ! PAYABLES MAIN HEADER FILE.               *~
            * # 8 ! PAYLINES ! PAYABLES LINE ITEMS FILE                 *~
            * #13 ! VBKBUFFR ! VBKINPUT HEADER BUFFER                   *~
            * #14 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #15 ! LOCATION ! Location Master File                     *~
            * #16 ! GLMAIN   ! General Ledger Main File                 *~
            * #19 ! STORNAME ! Store Info File - Name/Address           *~
            * #20 ! TXTFILE  ! Text File                                *~
            * #21 ! QCRTYPES ! REJECTION (SOB) CODES                    *~
            * #22 ! HNYQUAN  ! Inventory Quantities File                *~
            * #23 ! HNYPOOL  ! Inventory LOFO/FIFO Pool Records File    *~
            * #29 ! RCVJRNTF ! Receiver - G/L Tif                       *~
            * #30 ! RCVMASTR ! Receiver Master File                     *~
            * #31 ! RCVLINES ! Receiver Line Items                      *~
            * #32 ! RCVHNYDS ! Receiver Inventory Distribution          *~
            * #34 ! RCVHNYRP ! Receiver Inventory Report File           *~
            * #35 ! RCVTIF   ! Receiver Master File TIF                 *~
            * #36 ! RCVTIF2  ! Receiver Line Items  TIF                 *~
            * #37 ! RCVHNYTF ! Receiver Inventory Distribution TIF      *~
            * #39 ! COREXREF ! Core Part Cross Reference File           *~
            * #40 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #41 ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #42 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #50 ! SERMSAVE ! Serial Number Tracking Restore File      *~
            * #51 ! SERWKHLD ! TEMPORARY SERIAL #'S WORK FILE (RCVDIST) *~
            * #52 ! SERMHOLD ! SERIAL NUMBER MASTER FILE COPY (RCVDIST) *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *


            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =   3                      ~

            select # 2, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select # 3, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =  10,  keylen =  30, dup

            select # 4, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos  =  1, keylen =  25,                      ~
                        alt key    1, keypos =  10,  keylen =  16

            select # 5, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos  =    1, keylen =  28

            select # 6, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select  #7, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #8, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select  #9, "PIPMASTR",                                      ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos =  2, keylen = 25,                        ~
                        alternate key 1, keypos = 1, keylen = 26

            select #13, "VBKBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize  = 1044,                                 ~
                        keypos =   1,  keylen = 10,                      ~
                        alt key 1, keypos =  4,  keylen =  7, dup,       ~
                            key 3, keypos = 24,  keylen = 16

            select #14,  "HNYLOCNS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 443, keylen = 42,     ~
                                   key 2, keypos = 485, keylen = 42,     ~
                                   key 3, keypos = 527, keylen = 42,     ~
                                   key 4, keypos = 590, keylen = 42      ~

            select #15,  "LOCATION",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 11,                        ~
                         alternate key 1, keypos = 4, keylen = 11        ~

            select #16, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #19, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   3                      ~

            select #20, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #21, "QCRTYPES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 6

            select #22, "HNYQUAN",                                       ~
                        varc, indexed, recsize = 650,                    ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select #23, "HNYPOOL",                                       ~
                         varc, indexed, recsize = 300,                   ~
                         keypos = 1, keylen = 39

            select #29, "RCVJRNTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 670,                                   ~
                        keypos = 10, keylen = 10

            select #30, "RCVMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos= 1, keylen = 16                           ~

            select #31, "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24              ~

            select #32, "RCVHNYDS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 86,                          ~
                        alt key 1, keypos = 45, keylen = 42              ~

            select #34, "RCVHNYRP",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 29,   keylen = 55,                      ~
                        alt key 1, keypos =  1, keylen = 83              ~

            select #35, "RCVTIF",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos= 12, keylen = 16,                         ~
                        alt key 1, keypos = 1, keylen = 11               ~

            select #36, "RCVTIF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24              ~

            select #37, "RCVHNYTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 86,                          ~
                        alt key 1, keypos = 45, keylen = 42              ~

            select #39, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            select #40, "SERTIF",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos = 1, keylen = 62

            select #41, "SERWORK",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  48,                                   ~
                        keypos = 1, keylen = 23

            select #42, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #50, "SERMSAVE",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45                      ~

            select #51, "SERWKHLD",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  48,                                   ~
                        keypos = 1, keylen = 23

            select #52, "SERMHOLD",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45                      ~



            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7),   0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8),   0%, rslt$( 8))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9),   0%, rslt$( 9))
            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15),   0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
            call "OPENCHCK" (#19, fs%(19), f2%(19),   0%, rslt$(19))
            call "OPENCHCK" (#20, fs%(20), f2%(20),   0%, rslt$(20))
            call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, rslt$(21))
            call "OPENCHCK" (#22, fs%(22), f2%(22),   0%, rslt$(22))
            call "OPENCHCK" (#23, fs%(23), f2%(23),   0%, rslt$(23))
            call "OPENCHCK" (#29, fs%(29), f2%(29),   0%, rslt$(29))
            call "OPENCHCK" (#30, fs%(30), f2%(30),   0%, rslt$(30))
            call "OPENCHCK" (#31, fs%(31), f2%(31),   0%, rslt$(31))
            call "OPENCHCK" (#32, fs%(32), f2%(32),   0%, rslt$(32))
            call "OPENCHCK" (#34, fs%(34), f2%(34),   0%, rslt$(34))
            call "OPENCHCK" (#35, fs%(35), f2%(35), 100%, rslt$(35))
            call "OPENCHCK" (#36, fs%(36), f2%(36), 300%, rslt$(36))
            call "OPENCHCK" (#37, fs%(37), f2%(37), 300%, rslt$(37))
            call "OPENCHCK" (#40, fs%(40), f2%(40),   0%, rslt$(40))
            call "OPENCHCK" (#41, fs%(41), f2%(41),   0%, rslt$(41))
            call "OPENCHCK" (#42, fs%(42), f2%(42),   0%, rslt$(42))
            call "OPENCHCK" (#50, fs%(50), f2%(50),   0%, rslt$(50))
            call "OPENCHCK" (#51, fs%(51), f2%(51),   0%, rslt$(51))
            call "OPENCHCK" (#52, fs%(52), f2%(52),   0%, rslt$(52))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$, "CF", pgmname$)
            date$ = date
            call "DATEFMT" (date$)

            init (hex(00)) oldreadkey$ : str(oldreadkey$,1%,3%) = userid$
            call "PLOWNEXT" (#29, oldreadkey$, 3%, f1%(29%))
               if f1%(29%) = 0% then L09074
            askpf1$ = "You have unposted Receiver - G/L transactions"
            goto L09094

L09074:     init (hex(00)) oldreadkey$ : str(oldreadkey$,1%,3%) = userid$
            call "PLOWNEXT" (#34, oldreadkey$, 3%, f1%(34%))
               if f1%(34%)  = 0% then L09126
            askpf1$ = "You have unprocessed Receiver - Inventory records"

L09094:     f1%(35%) = 0% : askkey% = 0%
            askmid$ = " "
            askpf2$ = "Press RETURN or any function key to exit this prog~
        ~ram."
            askhdr$ = "* * * S O R R Y * * *"
            call "ASKUSER" (askkey%, askhdr$, askpf1$, askmid$, askpf2$)
            goto exit_program_a

L09126:     init (hex(00)) oldreadkey$ : str(oldreadkey$,1%,3%) = userid$
            call "PLOWALTS" (#35, oldreadkey$, 1%, 3%, f1%(35%))
               if f1%(35%)  = 0% then L09200

L09142:     f1%(35%) = 0% : askkey% = 0%
            askpf1$ = "You have unposted Receiver Update records"
            askmid$ = "You must be sure no task is actively updating"
            askpf2$ = "Press PF32 to continue.  Press RETURN to exit."
            askhdr$ = "* * * S O R R Y * * *"
            call "ASKUSER" (askkey%, askhdr$, askpf1$, askmid$, askpf2$)
            if askkey% = 32% then L09200
            if askkey% =  0% then exit_program_a
               goto L09142

L09200:     edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            pomax% = dim(ponumber$(),1)

                  call "READ100" (#1, userid$, f1%(1))
                  if f1%(1)<>0 then L09245
                     goto user_error

L09245:           get #1, using L09250, paydate$, userstr$
L09250:               FMT XX(9),CH(6),XX(48),CH(3)
                  call "WHICHMON" (#2, paydate$, per%)
                  if per% = 0% then posting_date_error
*                CALL "DATEFMT" (PAYDATE$)

            call "READ100" (#2,"MONTHS OPEN",f1%(2))
                if f1%(2)<>0 then L09295
                     goto months_open_error

L09295:         get #2, using L09300, basedate$
L09300:             FMT XX(32),CH(6)

            oldreadkey$ = "DEFAULTS.STORE." & userstr$
            call "READ100" (#2, oldreadkey$, f1%(2))
                if f1%(2) = 0% then store_error
            get #2 using L09330, defppayacct$,deffrgtacct$,rcvacct$,qcacct$
L09330:         FMT POS(99), CH(9), POS(135), 3*CH(9)

            if defppayacct$ <> " " then L09365
            call "READ100" (#2, "MODULE.DEFAULTS.AP  ", f1%(2))
                if f1%(2) = 0% then L09365
            get #2, using L09360, defppayacct$
L09360:         FMT XX(36), XX(9), XX(9), CH(9)  /* 3rd Account  */
L09365:
            call "VBKSWTCH" ("RCVRPRNT", prtopt$, temp, temp%)
            call "VBKSWTCH" ("RCVMODE ", modedflt$, temp, temp%)
            call "VBKSWTCH" ("TKTSIZE ", temp$, fil, temp%)
            if fil = 0 then fil = 6   /* Default Tkt Size is 3-1/2" */
            fill% = fil
            call "VBKSWTCH" ("POSTDCST", postatstdcost$, temp, u3%)
            if postatstdcost$ = "B" then postatstdcost$ = "R"

*        Check for Core
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#2, plowkey$, core_on%)
                if core_on% <> 1% then L09400
            get #2 using L09393, core_inv_flag$
L09393:         FMT POS(134), CH(1)
            if core_inv_flag$ <> "Y" then core_on% = 0%
            if core_on% <> 1% then L09400
                call "OPENCHCK" (#39, 0%, f2%(39%),  0%, " ")

L09400:     headsc$(1)="Vendor    P.O. Number    Vendor's Name           ~
        ~       Pack Slip             "

            headsc$(2)="Seq          Part Number      Org. Order Prior Re~
        ~c. Dist.  Received       Open"

            headsc$(3)="Seq Internal Part Number      Description        ~
        ~         Status  Store/Lot   "

            headsc$(4)="Seq Internal Part Number       Due By  -Receiving~
        ~ Window- Status  Store/Lot   "
            headsc$(5)="Seq Requested By          Deliver To            J~
        ~ob                           "
                      mat set% = con : mat set% = (99%) * set%
                      mat xref% = zer

                     for i% = 1% to 10%
                      xref%(1,i%) = i%
                      set%(i%) = 2%
                      next i%
                      set%(1%)= 13%

            call "ENABLSUB" ("INIT","RCVINPUT",xref%(),set%(),0%,0%,0%,  ~
                          0%)
        rem**************************************************************~
           *                                                            *~
           *  the following variable named  "OVERSHIPPCT" will be used  *~
           *  to determine if an overreceipt will be considered valid   *~
           *  when performing the test on the amount received field.    *~
           *                                                            *~
           **************************************************************

           overshippct = .5000    /* This means that any overshipment */
                                  /* received must be less than 50%   */
                                  /* over the original amount ordered */

           call "VBKSWTCH" ("RCPTSLMT", ospct$, overshippct, u3%)
           overshippct = overshippct * .01
           call "CONVERT" (overshippct*100, -2.2, str(ospct$,,7))
               osmsg$ = "Based on Allowed Overship of this part,"
           overshippct = overshippct + 1

            lot_sys$, lot_unique$ = "N"
            readkey$ = "SWITCHS.HNY"
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 0% then L10000
                get #2 using L09635, lot_sys$, lot_unique$
L09635:              FMT POS(92), 2*CH(1)
                if lot_sys$ = "N" then lot_unique$ = "N"


L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode

            gosub L29500  /* INITIALIZATION */

            for fieldnr% = 1 to  10
L10055:         gosub'051(fieldnr%,1%)   /* ENABLES Set Defaults */
                      if enabled% = 0% then L10135
L10065:         gosub'101(1%, fieldnr%)     /* Display & Accept Screen */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10105
L10080:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%,1)
                         if enabled% = 1% then L10065
                         if fieldnr% = 1% then L10055
                         goto L10080
L10105:               if keyhit% <> 16 then       L10115
                         if fieldnr% = 1% then exit_program
L10115:               if fieldnr% <> 1% then L10130
                      if keyhit%  =  8% then       L10135
                      if keyhit%  = 14% then       L10135
L10130:               if keyhit% <>  0% then       L10065
L10135:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10065
            next fieldnr%

            gosub input_pos
            goto edtpg2

        input_pos
            if maxpo% >= pomax% then return
            vl% = max(0%, maxpo% - 12%)
            v%  = maxpo% + 1%
            s%  = v% - vl%

L10200:     for fieldnr% = 1 to  3
L10205:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults */
                      if enabled% = 0 then L10325
L10215:         gosub'102(1%, fieldnr%)     /* Display & Accept Screen */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  2% then       L10240
                         gosub L10395
                         goto L10200
L10240:               if keyhit% <>  4% then       L10270
L10245:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10215
                         if fieldnr% = 1% then L10205
                         goto L10245
L10270:               if keyhit% <>  6% then       L10315
                         if v% = 1% then L10215
                            if fieldnr% = 1% then                        ~
                                          vencode$(v%) = vencode$(v%-1%)
                            if fieldnr% = 2% then                        ~
                                          ponumber$(v%) = ponumber$(v%-1%)
                            if fieldnr% = 3% then                        ~
                                          packslip$(v%) = packslip$(v%-1%)
                         goto L10215
L10315:               if keyhit%  = 16% and fieldnr% = 1% then L10395
                      if keyhit% <>  0% then L10215
L10325:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10215
            next fieldnr%

            gosub dataloadpo
               if errormsg$ <> " " then L10200
            maxpo% = maxpo% + 1%

            gosub edtpg3
               if u3% <> 0% then input_pos
                  gosub L10395
                  maxpo% = maxpo% - 1%
                  goto input_pos

L10395:     init (" ") vencode$(v%), ponumber$(v%), venname$(v%),        ~
                       packslip$(v%), errormsg$
            return

        inputlines_all
            c% = 1%
            if keyhit% = 10% then edit% = 0% else edit% = -1%

L10435:     if c% > maxlines% then return
                s% = min(c%, 13%)
                l% = c% - s%
                   if ohpost$(c%) <> " " then L10505
                   if abs(qtyonord(c%)) < .0001 then L10505
                   if status$(c%) <> "A" then L10505
                   if job$(c%) = " " then L10468
                      call "READ100" (#9, part$(c%,1%), testjob%)
                         if testjob% = 0% then L10468
                      testjob% = 3%
                      call "JBINUSE" (job$(c%), testjob%)
                         if testjob% <> 0% then L10505
L10468:               gosub save_qty
                      if edit% <> 0% then L10490
                         qtyrecd(c%)  = qtyonord(c%)
                         qtyonord(c%) = 0
                         ohpost$(c%)  = ohpost$
L10490:               gosub inputlines
                   if u3% = 0% then return
                   if keyhit%  = 16% then return
L10505:     c% = c% + 1%
            goto L10435

        inputlines
            gosub'040:u3% = 99%
            for e% = 2% to 4%
                fieldnr% = e%
                gosub'053(fieldnr%)
                      if enabled% =  0% then L10690
L10630:         gosub'103(1%, fieldnr%)
                      if keyhit% =  8% then mode% = mode% + 1%
                      if keyhit% <> 16% then L10660
                         if e% <> 2% then L10660
                            gosub restore_qty  /* Reset */
                            gosub'040          /* & Format */
                            errormsg$, ohpost$(c%) = " "
                            return
L10660:               if keyhit% <>  2% then L10675
                         gosub restore_qty
                         goto inputlines
L10675:               if keyhit%  =  1% then gosub startoverpo
                         if u3% = 0% then return
                      if keyhit% <>  0% then       L10630
L10690:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10630
                      if e% > 2% then L10710
                         if ohpost$(c%) = " " then return
L10710:         next e%
                gosub dist_quantity
                if return% = 0% then return
                gosub restore_qty
                goto inputlines

        save_qty
            qtysave( 1) = qtyonord  (c%)
            qtysave( 2) = qtyrecd   (c%)
            qtysave( 3) = qtyrcvhold(c%)
            qtysave( 4) = qtyqcpend (c%)
            qtysave( 5) = qtyqchold (c%)
            qtysave( 6) = qtyonhand (c%)
            qtysave( 7) = qtyscrap  (c%)
            qtysave( 8) = qtyrtv    (c%)
            ohhold$     = ohpost$   (c%)
            return

        restore_qty

            qtyonord  (c%) = qtysave( 1)
            qtyrecd   (c%) = qtysave( 2)
            qtyrcvhold(c%) = qtysave( 3)
            qtyqcpend (c%) = qtysave( 4)
            qtyqchold (c%) = qtysave( 5)
            qtyonhand (c%) = qtysave( 6)
            qtyscrap  (c%) = qtysave( 7)
            qtyrtv    (c%) = qtysave( 8)
            ohpost$   (c%) = ohhold$
            errormsg$ = " "

            return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            errormsg$ = " "
            gosub'101(2%, 0%)           /* Display Screen - No Entry */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       edtpg2
                  if keyhit%  = 16% then datasave
                  if keyhit%  = 12% then L11320
                  if keyhit%  = 29% then L11150
                  if keyhit% <>  0% then  edtpg1
L11150:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% > 10% then edtpg1
            if keyhit% <> 29% then L11210
                     call "ENABLSUB" ("MODIFY","RCVINPUT",xref%(),set%(),~
                     1%,fieldnr%,0%,0%)
                     goto edtpg1
L11210:     gosub'051(fieldnr%,2%)         /* Enables, Set Defaults  */
            if enabled% = 0% then edtpg1
L11230:     gosub'101(3%, fieldnr%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
            if keyhit% <> 0% then L11230
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11230
                     if fieldnr% <> 9% then edtpg1
                     if carfacct$ <> " " then edtpg1
                        fieldnr% = 10%
                        goto L11210
L11320:     call "RCVTKTSB" (fill%, #5, #6, #37, #22, #35, #36, #31, #3)
            prtd% = 1%
            goto edtpg1

        edtpg2

            gosub'102(2%, 0%)
              if keyhit% = 1% then gosub startover
              if keyhit% = 2% then vl% = 0%
              if keyhit% = 3% then vl% = maxpo%-13%
              if keyhit% = 4% then vl% = vl%-12%
              if keyhit% = 5% then vl% = vl%+12%
              if keyhit% = 6% then vl% = vl%-1%
              if keyhit% = 7% then vl% = vl%+1%
                 vl% = max(0%, min(87%, vl%, maxpo%-1%))
              if keyhit% = 9% then       edtpg1
              if keyhit%<>11% then       L11510
                 gosub input_pos
                 goto edtpg2
L11510:       if keyhit% = 16% then datasave
              if keyhit% <> 0% then      edtpg2
            fieldnr% = cursor%(1) - 6
            if fieldnr% < 1 or fieldnr% > 13 then edtpg2
            v%=min(vl%+fieldnr%,maxpo%)
            s% = v% - vl%
            if cursor%(2%) > 2% then L11630
              init (" ") qtyonord$()
L11580:        gosub dataloadpo
                 if errormsg$ <> " " then edtpg2
               gosub edtpg3
               goto edtpg2

L11630:     fieldnr% = 3%
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
L11660:     gosub'102(3%, fieldnr%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11660
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11660
            goto L11580

        edtpg3
            edit% = 1%:u3% = 99%

            gosub'103(2%,0%)
              if keyhit% <> 16% then L12070
                gosub dataputpo
                 return
L12070:       if keyhit% = 1% then gosub startoverpo
                 if u3% = 0% then return

              if keyhit% = 2% then l% = 0%
              if keyhit% = 3% then l% = maxlines%-13%
              if keyhit% = 4% then l% = l%-12%
              if keyhit% = 5% then l% = l%+12%
              if keyhit% = 6% then l% = l%-1%
              if keyhit% = 7% then l% = l%+1%
                 l% = max(0%, min(87%, l%, maxlines% - 1%))
              if keyhit%  =  8% then mode% = mode% + 1%
              if keyhit%  =  9% then L12220
              if keyhit% <> 10% then L12250
                 inpmessage$ = hex(a4) & "Receiving All Lines Complete"
                 print at (21,01), str(inpmessage$)
L12220:          gosub inputlines_all
                    if u3% = 0% then return
                 goto edtpg3
L12250:       if keyhit% = 14% then      edtpg4
              if keyhit% = 23% then gosub find_part
              if keyhit% = 25% then      L12350
              if keyhit% = 26% then      L12350
              if keyhit% <> 28% then     L12320
                 gosub reset_receiver
                    if u3% = 0% then return
                 goto edtpg3
L12320:       if keyhit% <> 0% then      edtpg3
            if fieldnr% > maxlines% then edtpg3
            if fieldnr% > 0% then L12360
L12350:     fieldnr% = cursor%(1) - 6
L12360:     if fieldnr% < 1 or fieldnr% > 13 then edtpg3
            c% = min(l%+fieldnr%,maxlines%)
            s% = c% - l%
            errormsg$ = " "

            if keyhit% <> 25% then L12396
               gosub edttext
               goto edtpg3

L12396:     if keyhit% <> 26% then L12400
               gosub dsptext
               goto edtpg3

L12400:     if status$(c%) = "A" then goto L12420
                status$ = "Status unknown"  /* Below s/b all possible */
                if status$(c%) = "C" then status$ = "Closed"
                if status$(c%) = "H" then status$ = "On Hold"
                if status$(c%) = "W" then status$ = "Awaiting Price"
                if status$(c%) = "N" then status$ = "Outside Receiving" &~
                                                    " Date Window"
                errormsg$ = "Receipt NOT Allowed.  Line item is: " &     ~
                            status$
                goto edtpg3

L12420
*        Check for invoice activity here
            init (hex(00)) miscplow$
            str(miscplow$,,35%) = str(rcvrno$) & str(ponumber$(v%)) &    ~
                                                          str(seqnr$(c%))
            call "PLOWALTS" (#08, miscplow$, 1%, 35%, f1%(8%))
            if f1%(8%) = 0% then L12440
L12431:         askhit% = 2%
                call "ASKUSER" (askhit%, "ITEM INVOICED",                ~
                     "This Receiver Item has already been Invoiced.",    ~
                     "Press RETURN to Abort Modification  -or-",         ~
                     "Press PF16 to Continue with Edit.")
                if askhit% = 0% then goto edtpg3
                if askhit% <> 16% then L12431

L12440:     if job$(c%) = " " then L12510
               call "READ100" (#9, part$(c%,1%), testjob%)
                 if testjob% = 0% then L12510
               testjob% = 3%
               call "JBINUSE" (job$(c%), testjob%)
                 if testjob% = 0% then L12510
                 errormsg$ = "Edit NOT Allowed.  Purchase Job is IN USE:"
                 errormsg$ = errormsg$ & " " & job$(c%) & "."
                 goto edtpg3

L12510:     if cursor%(2%) > 2% then edtpg3a

            if ohpost$(c%) <> " " then edtpg3a
               if abs(qtyonord(c%)) < .0001 then edtpg3a
L12550:           edit% = 0%
L12560:           gosub save_qty
                     gosub inputlines
                        if u3% = 0% then return
                     goto edtpg3


        edtpg3a
            if ohpost$(c%) <> " " then L12660
               if abs(qtyonord(c%)) >= .0001  then L12550
                  goto L12560
L12660:     gosub save_qty
            field%, fieldnr% = 2%
            if ohpost$(c%) = " " then L12720
            if cursor%(2%) > 57% then field%, fieldnr% = 3%
            if cursor%(2%) > 68% then field%, fieldnr% = 4%

L12720:     gosub'053(fieldnr%)
              if enabled% = 0% then edtpg3
            gosub'040
L12750:     gosub'103(3%,fieldnr%)
                  if keyhit%  =  1% then gosub startoverpo
                     if u3% = 0% then return
                  if keyhit% =  8% then mode% = mode% + 1%
                  if keyhit% <>  0% then       edtpg3a
            gosub'153(fieldnr%)
                  fieldnr% = min(fieldnr%, 4%)
                  if errormsg$ <> " " then L12750
                  if field% > 3% then edtpg3
                     gosub dist_quantity
                     if return% = 0% then L12870 /* EDTPG3 */
                     gosub restore_qty
L12870:              gosub'040
                     goto edtpg3

        edtpg4

            gosub'104(2%, 0%)
              if keyhit% <> 16% then L13060
                 gosub dataputpo
                 return
L13060:       if keyhit% = 14% then edtpg3
              if keyhit% = 1% then gosub startoverpo
                 if u3% = 0% then return
              if keyhit% = 2% then l% = 0%
              if keyhit% = 3% then l% = maxlines%-13%
              if keyhit% = 4% then l% = l%-12%
              if keyhit% = 5% then l% = l%+12%
              if keyhit% = 6% then l% = l%-1%
              if keyhit% = 7% then l% = l%+1%
                 l% = max(0%, min(87%, l%, maxlines% - 1%))
              if keyhit% = 8% then mode2% = mode2% + 1%
              if keyhit% = 25% then L13200
*            IF KEYHIT% = 26% THEN 13200
                 goto edtpg4
L13200:     fieldnr% = cursor%(1) - 6
            if fieldnr% < 1 or fieldnr% > 13 then edtpg4
            c% = min(l%+fieldnr%,maxlines%)
            s% = c% - l%
            if keyhit% <> 25% then L13260
               gosub dsp_partext
L13260:        goto edtpg4
*          IF KEYHIT% <> 26% THEN EDTPG4
*             GOSUB DSPTEXT
*             GOTO EDTPG4

        find_part
            fhit% = 0%
            search_part$ = " "
            call "ASKSTRNG" (fhit%, "FIND PART", "Enter the Part Number"&~
                             " to Find.", "Press RETURN to Search.",     ~
                             "Part", search_part$, 25%)
            if fhit% = 1% then return

            if m% = 1% then start% = 1% else start% = 26%
            search str(part$(),start%,) = search_part$ to p%() step 50%
            if p%(1%) = 0% then return
                temp% = p%(1%) - 1%
                l% = (temp% / 50%)
                    l% = max(0%, min(87%, l%, maxlines% - 1%))
                return

        REM *************************************************************~
            * RESET RECIEVED QUANTITY TO MINIMUM                        *~
            *************************************************************

        reset_receiver

            if maxlines% = 0% then return
                u3% = 99%
            for c% = 1% to maxlines%
                if job$(c%) = " " then L14090
                   call "READ100" (#9, part$(c%,1%), testjob%)
                      if testjob% = 0% then L14090
                   testjob% = 3%
                   call "JBINUSE" (job$(c%), testjob%)
                      if testjob% <> 0% then L14260
L14090:         gosub save_qty
                l% = max(0%, c% - 13%)
                s% = c% - l%
                fieldnr% = 3%
                qtyrecd$(c%,1%), qtyrecd$(c%,2%) = "0"
L14140:         gosub'153(fieldnr%)
                  if errormsg$ = " " then L14220
L14160:              gosub'103(3%,fieldnr%)
                       if keyhit%  =  1% then gosub startoverpo
                          if u3% = 0% then return
                       if keyhit% =  8% then mode% = mode% + 1%
                       if keyhit% <>  0% then L14160
                     goto L14140
L14220:         gosub dist_quantity
                  if return% = 0% then L14250
                     gosub restore_qty
L14250:         gosub'040
L14260:         next c%
                call "SERSTOVR" (0%, "6", " ", #42, #41)
                return

        REM *************************************************************~
            * TEXT HANDLING SUBROUTINES                                 *~
            *************************************************************

        edttext

            errormsg$ = "VENDOR: " & vencode$(v%) & ", PART: "
            errormsg$ = errormsg$ & part$(c%,1) & " " & partdescr$(c%,1)
            call "TXTINSUB" (#20, 0%, "005", errormsg$, textrc$(c%),     ~
                                                                 text$())
            errormsg$ = " "
            return

        dsptext

            errormsg$ = "VENDOR: " & vencode$(v%) & ", PART: "
            errormsg$ = errormsg$ & part$(c%,1) & " " & partdescr$(c%,1)
            call "TXTDSPLY" (#20, 0%, "004", errormsg$, textpo$(c%),     ~
                                                                 text$())
            errormsg$ = " "
            return

        dsp_partext
            errormsg$ = "VENDOR: " & vencode$(v%) & ", PART: "
            errormsg$ = errormsg$ & " " & part$(c%,1) & " " &            ~
                        partdescr$(c%,1)
            textpt$ = " "
            call "READ100" (#6, part$(c%,1), f1%(6))
                if f1%(6) <> 1% then L15310
            get #6 using L15300, textpt$
L15300:         FMT POS(98), CH(4)
L15310:     call "TXTDSPLY" (#20, 0%, "001", errormsg$, textpt$, text$())
            errormsg$ = " "
            return

        REM *************************************************************~
            * COMMON CONVERT FLOATING POINT TO STRING SECTION           *~
            *************************************************************

            deffn'040

            init (" ")                                                   ~
                 qtyorig$(c%,1), qtyonord$(c%,1), qtyrecd$(c%,1),        ~
                 qtyrecdtd$(c%,1),                                       ~
                 qtyorig$(c%,2), qtyonord$(c%,2), qtyrecd$(c%,2),        ~
                 qtyrecdtd$(c%,2)

            if oldqtyonord(c%) < 0 then qtyonord(c%) = 0
            if part$(c%,1) = " " then return

                factor(c%) = round(factor(c%), 4)

                venorig   = round(qtyorig  (c%)/factor(c%) ,7)  /*NO CHG*/
                venonord  = round(qtyonord (c%)/factor(c%) ,7)
                venrecd   = round(qtyrecd  (c%)/factor(c%) ,7)
                venrecdtd = round(qtyrecdtd(c%)/factor(c%) ,7)  /*NO CHG*/

                call "CONVERT" (qtyorig  (c%), 2.4, qtyorig$  (c%,1))
                call "CONVERT" (qtyonord (c%), 2.4, qtyonord$ (c%,1))
                call "CONVERT" (qtyrecd  (c%), 2.4, qtyrecd$  (c%,1))
                call "CONVERT" (qtyrecdtd(c%), 2.4, qtyrecdtd$(c%,1))

                call "CONVERT" (venorig  , 2.7, qtyorig$  (c%,2))
                call "CONVERT" (venonord , 2.7, qtyonord$ (c%,2))
                call "CONVERT" (venrecd  , 2.7, qtyrecd$  (c%,2))
                call "CONVERT" (venrecdtd, 2.7, qtyrecdtd$(c%,2))

                return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            gosub delete_hold_record
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%,modi%)
            call "ENABLSUB" ("SET","RCVINPUT",xref%(),set%(),1%,         ~
                     fieldnr%,modi%,enabled%)

                  on fieldnr% gosub L20200,         /* Receiver Number  */~
                                    L20250,         /* Bill of Lading   */~
                                    L20300,         /* Carrier Vencode  */~
                                    L20350,         /* Carrier Descr    */~
                                    L20400,         /* Def. Recvd date  */~
                                    L20450,         /* Def. Recvd Mode  */~
                                    L20500,         /* Time             */~
                                    L20600,         /* Comments         */~
                                    L20700,         /* Carrier Freight  */~
                                    L20800          /* Car Frieght Acct */
                     return

L20200:     REM Default/Enable for Receiver Number
                enabled% = 1%
                inpmessage$ = "Enter Receiver Control Number.  Use PF8 or~
        ~ PF14 to SCAN files."
                return

L20250:     REM Default/Enable for Bill of Lading
                inpmessage$ = "Enter Bill of Lading Number"
                return

L20300:     REM Default/Enable for Carrier Vendor Code
                inpmessage$ = "If Carrier is on file, enter carrier's Ven~
        ~dor Code."
                return

L20350:     REM Default/Enable for Carrier Description
                if carvencode$ <> " " then enabled% = 0%
                inpmessage$ = "Enter Carrier's Description"
                return

L20400:     REM Default/Enable for Default Received Date
                if defrcvddate$ <> " " and ~
                   defrcvddate$ <> blankdate$ then L20420
                     defrcvddate$ = paydate$
                     call "DATEFMT" (defrcvddate$)
L20420:         inpmessage$ = "Enter Date Received (Posting Date)."
                return

L20450:     REM Default/Enable for Default Received Mode
                if ohpost$ = " " then ohpost$ = modedflt$
                inpmessage$ = "Enter Receiving Distribution Mode: 'R', 'Q~
        ~', 'I', or 'D'."
                return

L20500:     REM Default/Enable for Time
                if rcvtime$ = " " then call "TIME" (rcvtime$)
                inpmessage$ = "Enter Time of Receipt"
                return

L20600:     REM Default/Enable for Comments
                inpmessage$ = "Enter Comments"
                return

L20700:     REM Default/Enable for Carrier Freight
                if carvencode$ = " " then enabled% = 0%
                inpmessage$ = "Enter Carrier Freight Amount."
                return

L20800:     REM Default/Enable for Carrier Freight Acct
                if carfrieght = 0 then enabled% = 0%
                if carfacct$ <> " " then L20920
                   call "READ100" (#3, carvencode$, f1%(3))
                      if f1%(3) = 0% then L20920
                   get #3 using L20860, carpacct$, carfacct$
L20860:                FMT POS(481), CH(9), POS(519), CH(9)
                   if carpacct$ = " " then carpacct$ = defppayacct$
                   if carfacct$ = " " then carfacct$ = deffrgtacct$
                   call "DESCRIBE" (#16, carfacct$,                      ~
                                        carfacctdescr$, 0%, f1%(16))
                   call "GLFMT" (carfacct$)
L20920:         inpmessage$ = "Enter Carrier Freight Account."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21200,         /* VENDOR, PO#      */~
                                    L21300,         /* PO NUMBER        */~
                                    L21400          /* PACK SLIP        */
                     return

L21200:     REM Default/Enable for VENDOR
                inpmessage$ = "Enter Vendor Code and/or P.O. Number."
                return

L21300:     REM Default/Enable for PONUMBER
                inpmessage$ = "Enter P.O. Number."
                return

L21400:     REM Default/Enable for Pack Slip
                inpmessage$ = "Enter Packing Slip for this P.O."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = abs(edit%)
                  on fieldnr% gosub L22120,         /* Tab Stop         */~
                                    L22200,         /* Receipt Mode     */~
                                    L22500,         /* Receipt Qty      */~
                                    L22600          /* Open Qty         */
L22120:           return

L22200: REM Default/Enable for RECEIPT MODE
            call "SERENABL" (part$(c%,1%), temp%, temp1%, #2, #6)
               if temp% = 0% then L22290
            inpmessage$ = "Serial Numbered Part.  Distribution Must be 'D~
        ~' or ' '."
L22250:     ohpost$(c%) = "D"
               enabled% = 1%
               return

L22290:     if new_recvr% = 1% then L22360
            call "HNYAVAIL" (#6, #22, part$(c%,1%), " ", " ", workkey$,  ~
                             9e9, temp, return%)
               if workkey$ = " " then L22360
                  workkey$ = " "
            inpmessage$ = "Quantity Controlled Part.  Distribution Must b~
        ~e 'D' or ' '."
            goto L22250

L22360:     inpmessage$ = " Lot tracked Part, Distribution MUST BE 'D'"
            if lot_sys$ = "Y" and lottrack$(c%) = "Y" then L22250


            inpmessage$ = "R/Q/I/D: R=Receiver hold; Q=all to QC; I=to "&~
                "Inventory; D=Distribute manually"
            if ohpost$(c%) <> " " then return
               ohpost$(c%) = ohpost$
                  enabled% = 1%
                  return


L22500: REM Default/Enable for QUANTITY RECEIVED
            if qtyonord(c%) > .0001 then enabled% = 1%
            inpmessage$ = "Enter Quantity Received (Vendor's Units)."
            return

L22600: REM Default/Enable for QUANTITY OPEN
            inpmessage$ = "Enter Quantity Remaining On-Order."
            if edit% < 0% then enabled% = 0%
            return

L29000: REM *************************************************************~
            * INITIALIZATION BLOCK (NEATER THAN CRAMMING IT AT 10000)   *~
            *************************************************************

        init (" ")                                                       ~
            errormsg$,                   /* ERROR MESSAGE              */~
            inpmessage$,                 /* INPUT MESSAGE              */~
                                                                         ~
            defstr$,                     /* DEFAULT STORE NUMBER       */~
            expacct$,                    /* PURCHASE EXPENSE ACCT      */~
            orddate$,                    /* ORDER DATE                 */~
            puracct$,                    /* LIABILITY ACCOUNT          */~
            rcvhnyds$(),                 /* RECEIVER HNY LINES         */~
                                                                         ~
            qtyorig$(),                  /* OUR ORIGINAL ORDER         */~
            qtyonord$(),                 /* OUR REM ON ORDER           */~
            qtyrecd$(),                  /* OUR REC THIS SHIPMENT      */~
            qtyrecdtd$(),                /* OUR PRIOR REC TO DATE      */~
                                                                         ~
            acct$(),                     /* EXPENSE ACCOUNT            */~
            cat$(),                      /* PART CATEGORY              */~
            date1$,                      /* LOW  DATE FOR RECEIPT      */~
            date2$,                      /* HIGH DATE FOR RECEIPT      */~
            duedate$(),                  /* DUE DATE                   */~
            job$(),                      /* Job/Project                */~
            lot$(),                      /*  LOT CODE                  */~
            meas$(),                     /* UNIT OF ISSUE              */~
            ohpost$(),                   /* ON HAND POST LINE ITEM     */~
            part$(),                     /* PART NUMBER                */~
            partdescr$(),                /*  PART DESCRIPTION          */~
            rejcode$(),                  /* REJECTION (SOB) CODE       */~
            rejlot$(),                   /* REJECT HOLDING LOT         */~
            rejstr$(),                   /* REJECT HOLDING STORE       */~
            seqnr$(),                    /* LINE ITEM SEQUENCE #       */~
            status$(),                   /* LINE ITEM STATUS           */~
            str$(),                      /*  STORE NUMBER              */~
            qty_uom$(),                  /* Orig Qty or UOM display    */~
            uom$()                       /* Stocking/Shipping UOM's    */~

        init (hex(ff))                                                   ~
            textpo$(),                   /* P.O. TEXT LINK             */~
            textrc$(),                   /* RCVR TEXT LINK             */~
            textqc$()                    /* Q.C. TEXT LINK             */~

        init (hex(00)) cost$()                                           ~

            mat factor      = zer        /* CASE MULTIPLIER            */
            mat price       = zer        /* PURCHASE PRICE             */
            mat ext         = zer        /* LINE ITEM EXTENSION        */
            mat qtyorig     = zer        /* ORIGINAL ORDER QUANTITY    */
            mat qtyonord    = zer        /* REMAINING ON ORDER         */
            mat oldqtyonord = zer        /* REMAINING ON ORDER         */
            mat qtyrecd     = zer        /* REC'D THIS RUN             */
            mat qtyrecdtd   = zer        /* REC'D TO DTE NOT INCL. RECD*/
            mat qtyrcvhold  = zer        /* RECEIVER HOLD              */
            mat qtyqchold   = zer        /* QUANTITY IN QC HOLD        */
            mat qtyqcpend   = zer        /* QTY IN Q/C NOT PROCESSED   */
            mat qtyrtv      = zer        /* QTY RET TO VENDOR/JUNKED   */
            mat qtyscrap    = zer        /* QTY TO SCRAP STORE/LOT     */
            mat qtyonhand   = zer        /* TOTAL TO ON-HAND           */
            mat rcvhnyds%   = zer        /* REC. HNY DIST. INDEX       */
            mat index%      = zer
            mat max_to_receive = zer

            mode2%, l% = 0% : mode% = 1%
            temp = 0
            call "TXTFUTIL" (#20, 0%, "INTL", hex(ffffffff))
            call "JBINUSE" (" ", 1%)
            call "ALLFREE"

        init (" ")                                                       ~
            bufflot$(),                  /*                            */~
            buffstr$(),                  /*                            */~
            poststr$(),                  /*                            */~
            postlot$(),                  /*                            */~
            savemark$()                  /*                            */~

            mat buffqty = zer
            mat postqty = zer

            mat buffidx% = zer
            mat postidx% = zer

            buffmax%, postmax% = 0%
            po_in_rcvlines% = 0%

            init (hex(00)) diskkey$
            call "DELETE" (#50, diskkey$, 0%)

            return


L29500: REM *************************************************************~
            * INITIALIZATION BLOCK (NEATER THAN CRAMMING IT AT 10000)   *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, rcvtime$, rcvcomment$,     ~
                      rcvrno$                     ,/* Receiver Number  */~
                      bol$                        ,/* Bill of Lading   */~
                      carvencode$                 ,/* Carrier Vencode  */~
                      cardescr$                   ,/* Carrier Descr    */~
                      defrcvddate$                ,/* Def. Recvd date  */~
                      carfrieght$                 ,/* Carrier Freight  */~
                      carfacct$                   ,/* Car Frieght Acct */~
                      carpacct$                   ,/* Car Payables Acct*/~
                      carfacctdescr$              ,/* Car Frieght Acct */~
                      vencode$()                  ,/* Vendors          */~
                      venname$()                  ,/* Vendor's Name    */~
                      ohpost$                     ,/* Recvr Mode       */~
                      ponumber$()                 ,/* P.O. Numbers     */~
                      packslip$()                 ,/* P.O. NUMBERS     */~
                      reqstd$()                   ,/* Requisitioner    */~
                      qtyonord$()                 ,/* Open Qtys remain */~
                      delvr$()                     /* Deliver To       */~

                      edit%, maxpo%, mode2% = 0%
                      mode% = 1%
                      carfrieght = 0
            powritten% = 0%
            firsttime% = 1%
            call "ALLFREE"
            call "JBINUSE" (" ", 1%)
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            gosub delete_hold_record
            goto inputmode

        startoverpo
            prtd% = 0%
L29834:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            if u3% <> 0% then L29834
            errormsg$ = " "

        REM RESET SERIAL NUMBER FILES

            call "SERSTOVR" (0%, "7", "2", #42, #41)
                 /* SO MUCH FOR NEW ADDS, ANYONE LOST? */
            init (hex(00)) diskkey$
L29856:     call "PLOWNEXT" (#50, diskkey$, 0%, f1%(50))
                if f1%(50) = 0% then return
            call "READ100" (#42, diskkey$, f1%(42))
                if f1%(42) <> 0% then L29856
            get #50 using L29866, str(record$(),1,300)
L29866:         FMT CH(300)
            write #42, using L29866, str(record$(),1,300), eod goto L29856
            goto L29856

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload

            init (hex(00)) diskkey$
            str(diskkey$,1,16) = str(rcvrno$)
            if f1%(35) = 0% then L30570

            get #35, using L30140,                                        ~
                     carvencode$, cardescr$, bol$, defrcvddate$, ohpost$,~
                     rcvtime$, rcvcomment$
L30140:         FMT POS(28), CH(9), CH(30), CH(30), CH(6), CH(1), XX(4), ~
                    CH(8), CH(30)
            call "WHICHMON" (#2, defrcvddate$, err%)
            if err% < per% then gosub posting_window_error

L30170:     call "PLOWNEXT" (#36, diskkey$, 16%, f1%(36))
                if f1%(36) = 0% then L30370
            get #36, using L30200, temp$
L30200:         FMT POS(67), CH(3)
            if temp$ <> " " then L30280
               get #36, using L30230, carpacct$, carfacct$, carfrieght
L30230:           FMT POS(296), CH(9), XX(18), CH(9), POS(364), PD(14,7)
               call "DESCRIBE" (#16,carfacct$,carfacctdescr$,0%,f1%(16))
               call "GLFMT" (carfacct$)
               call "CONVERT" (carfrieght, 2.2, carfrieght$)
                  goto L30170
L30280:     maxpo% = maxpo% + 1%
            get #36, using L30310, vencode$(maxpo%), ponumber$(maxpo%),   ~
                                  packslip$(maxpo%)
L30310:         FMT POS(42), CH(9), CH(16), XX(61), CH(16)
            call "GETCODE" (#3, vencode$(maxpo%), venname$(maxpo%), 0%,  ~
                                                         99, f1%(3))
            init (hex(ff)) str(diskkey$,42)
            goto L30170

L30370:     init (hex(00)) str(diskkey$,17)

L30390:     call "PLOWNEXT" (#31, diskkey$, 16%, f1%(31))
                if f1%(31) = 0% then L30830
            get #31, using L30420, temp$
L30420:         FMT POS(67), CH(3)
            if temp$ = " " then L30390
            get #31, using L30450, temp$
L30450:         FMT POS(51), CH(16)
            search str(ponumber$())=str(temp$,,16) to cursor%() step 16
                if cursor%(1%) <> 0% then L30540
            maxpo% = maxpo% + 1%
            get #31, using L30510, vencode$(maxpo%), ponumber$(maxpo%),   ~
                                  packslip$(maxpo%)
L30510:         FMT POS(42), CH(9), CH(16), XX(61), CH(16)
            call "GETCODE" (#3, vencode$(maxpo%), venname$(maxpo%), 0%,  ~
                                                         99, f1%(3))
L30540:     init (hex(ff)) str(diskkey$,42)
            goto L30390

L30570:     get #30, using L30600,                                        ~
                     carvencode$, cardescr$, bol$, defrcvddate$, ohpost$,~
                     rcvtime$, rcvcomment$
L30600:         FMT POS(17), CH(9), CH(30), CH(30), CH(6), CH(1), XX(4), ~
                    CH(8), CH(30)
            call "WHICHMON" (#2, defrcvddate$, err%)
            if err% < per% then gosub posting_window_error

L30630:     call "PLOWNEXT" (#31, diskkey$, 16%, f1%(31))
                if f1%(31) = 0% then L30830
            get #31, using L30660, temp$
L30660:         FMT POS(67), CH(3)
            if temp$ <> " " then L30740
               get #31, using L30690, carpacct$, carfacct$, carfrieght
L30690:           FMT POS(296), CH(9), XX(18), CH(9), POS(364), PD(14,7)
               call "DESCRIBE" (#16,carfacct$,carfacctdescr$,0%,f1%(16))
               call "GLFMT" (carfacct$)
               call "CONVERT" (carfrieght, 2.2, carfrieght$)
                  goto L30630
L30740:     maxpo% = maxpo% + 1%
            get #31, using L30770, vencode$(maxpo%), ponumber$(maxpo%),   ~
                                  packslip$(maxpo%)
L30770:         FMT POS(42), CH(9), CH(16), XX(61), CH(16)
            call "GETCODE" (#3, vencode$(maxpo%), venname$(maxpo%), 0%,  ~
                                                         99, f1%(3))
            init (hex(ff)) str(diskkey$,42)
            goto L30630

L30830:     return clear all
            rcvddate$ = defrcvddate$
            call "DATEFMT" (defrcvddate$)
            goto edtpg2

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataloadpo
            gosub L29000

            REM FIRST DOUBLE CHECK VENDOR, JUST IN CASE
                call "READ100" (#3, vencode$(v%), f1%(3))
                   if f1%(3)<>0 then L31065
                      errormsg$= "Vendor Not on File: " & vencode$(v%)
                      return
L31065:            get #3 using L31070, expacct$
L31070:                FMT POS(250), CH(9)

                call "REDALT0" (#13, ponumber$(v%), 3%, f1%(13))
                   if f1%(13) = 0% then L31110
                      errormsg$ = "This P.O. is being modified in P.O. in~
        ~put - no access is allowed"
                      return

L31110:     str(diskkey$,1) = str(vencode$(v%))
            str(diskkey$,10) = ponumber$(v%)

            REM MUST BE IN MASTER FILE
                    call "READ100" (#4, diskkey$, f1%(4))
                            if f1%(4) <> 0 then L31155
            errormsg$ = "P.O. not found on file"
               return

L31155:     REM LOAD HEADER INFORMATION, DESCRIBE ACCOUNTS, FORMAT DATA.
                inpmessage$ = hex(a4) & "Loading P.O. Line Items"
                print at (21,01), str(inpmessage$)
                get #4, using L31185, vencode$(v%), ponumber$(v%),        ~
                    venname$(v%), puracct$, orddate$, defstr$

L31185:             FMT CH(9), CH(16), CH(30), POS(226), CH(9), POS(451),~
                        CH(6), XX(20), CH(3)

            init (" ") reqstd$(), delvr$()
            REM READ LINE ITEMS
                c%, maxlines%, maxdist%, lastdist% = 0%

L31220:         call "PLOWNEXT" (#5, diskkey$, 25%, f1%(5))
                     if f1%(5) = 0 then L32075

                c%, maxlines% = maxlines% + 1

                get #5, using L31280, seqnr$(c%), part$(c%,1),            ~
                    partdescr$(c%,1), cat$(c%),                          ~
                    qtyorig(c%), qtyrecdtd(c%), qtyonord(c%), price(c%), ~
                    ext(c%), acct$(c%), duedate$(c%), lot$(c%), job$(c%),~
                    str$(c%), part$(c%,2), meas$(c%), factor(c%),        ~
                    dueorig$(c%), textpo$(c%), status$(c%), delvr$(c%),  ~
                    reqstd$(c%), date1$,date2$,cost$(c%)
L31280:         FMT POS(26), CH(3), XX(3), CH(25), CH(32), CH(4),        ~
                    3*PD(14,4), PD(14,7), PD(14,4), CH(9), CH(6), XX(12),~
                    CH(6), CH(8), CH(3), POS(197), CH(25), POS(288),     ~
                    CH(4), PD(14,4), CH(6), CH(4), CH(1),POS(313),CH(20),~
                    CH(20),POS(358), 2*CH(6), POS(421), CH(104)
                uom$(c%,2) = meas$(c%)
                call "READ100" (#6, part$(c%,1), f1%(6))
                if f1%(6) <> 1% then L31307
                     get #6 using L31305, uom$(c%,1), costtype$
L31305:         FMT POS(74), CH(4), POS(307), CH(1)
                gosub get_std_cost
L31307:         gosub get_allowed_overage /* Find max to receive */
                if status$(c%)  = " " then status$(c%) = "A"
                if status$(c%) <> "A" then L31340
                   if date1$ = " " or date1$ = blankdate$ then L31330
                if rcvddate$ < date1$ then status$(c%) = "N"
L31330:            if date2$ = " " or date2$ = blankdate$ then L31340
                if date2$ < rcvddate$ then status$(c%) = "N"
L31340:         if part$(c%,2) = " " then part$(c%,2) = part$(c%,1)
                if job$(c%) = " " then L31355
                   call "READ100" (#9, part$(c%,1%), testjob%)
                      if testjob% = 0% then L31355
                   testjob% = 3%
                   call "JBINUSE" (job$(c%), testjob%)
                   if testjob% = 0% then L31355
                 errormsg$ = "Edit NOT Allowed.  Purchase Job is IN USE:"
                 errormsg$ = errormsg$ & " " & job$(c%) & "."
                 return

L31355:         if part$(c%,1) <> " " then L31415
        /* IF DESCR ONLY... */
                init (" ") seqnr$(c%), part$(c%,1), partdescr$(c%,1),    ~
                     cat$(c%), acct$(c%), duedate$(c%), lot$(c%),        ~
                     str$(c%), part$(c%,2), meas$(c%), job$(c%)
                init(hex(00)) cost$(c%)
                qtyorig(c%), qtyrecdtd(c%), qtyonord(c%), price(c%),     ~
                factor(c%), oldqtyonord(c%) = 0

                c%, maxlines% = maxlines% - 1%
                goto L31220

L31415:         str(partdescr$(c%,2),1,8) = duedate$(c%)
                str(partdescr$(c%,2),10,8) = date1$
                str(partdescr$(c%,2),20,8) = date2$
                call "DATEFMT" (str(partdescr$(c%,2),1,8))
                call "DATEFMT" (str(partdescr$(c%,2),10,8))
                call "DATEFMT" (str(partdescr$(c%,2),20,8))
                if job$(c%) = " " then L31460
                   call "READ100" (#9, part$(c%,1%), testjob%)
                   if testjob% = 0% then                                 ~
                      str(partdescr$(c%,2),31,2) = "JB"                  ~
                                    else                                 ~
                      str(partdescr$(c%,2),31,2) = "PJ"

L31460:         init (hex(00)) str(diskkey$,29):temp = 0:found% = 0%
                oldreadkey$ = str(rcvrno$,,16) & str(diskkey$,,36)
                po_in_rcvlines% = 0%

                call "PLOWNEXT" (#31, oldreadkey$, 44%, f1%(31))
                     if f1%(31) = 0% then L31555
                po_in_rcvlines% = 1%
                get #31, using L31515, qtyrecd(c%),                       ~
                       qtyrcvhold(c%), qtyqcpend(c%), qtyqchold(c%),     ~
                       qtyscrap(c%), qtyrtv(c%), qtyonhand(c%),          ~
                       rejstr$(c%), rejlot$(c%), rejcode$(c%),           ~
                       textrc$(c%), textqc$(c%), temp%

L31515:         FMT POS(156), 7*PD(14,4), POS(341), CH(3), 2*CH(6),      ~
                    POS(388), 2*CH(4), BI(4)

                if temp% > 2000000% then temp% = 0%
                index%(c%) = max(index%(c%), temp%)
                qtyrecdtd(c%) = qtyrecdtd(c%) - qtyrecd(c%)
                temp = qtyrecd(c%)

L31555:         init (hex(00)) str(oldreadkey$,45)

                call "PLOWNEXT" (#36, oldreadkey$, 44%, f1%(36))
                     if f1%(36) = 0% then L31655
                get #36, using L31610, qtyrecd(c%),                       ~
                       qtyrcvhold(c%), qtyqcpend(c%), qtyqchold(c%),     ~
                       qtyscrap(c%), qtyrtv(c%), qtyonhand(c%),          ~
                       oldqtyonord(c%),                                  ~
                       rejstr$(c%), rejlot$(c%), rejcode$(c%),           ~
                       textrc$(c%), textqc$(c%), temp%

L31610:         FMT POS(156), 7*PD(14,4), POS(288), PD(14,4),            ~
                    POS(341), CH(3), 2*CH(6), POS(388), 2*CH(4), BI(4)

                if temp% > 2000000% then temp% = 0%
                index%(c%) = max(index%(c%), temp%)
                qtyonord(c%) = max(0, qtyonord(c%) + temp - qtyrecd(c%))
                qtyonord(c%) = max(0, qtyonord(c%) - oldqtyonord(c%))
                buffmax%, found% = 1%

L31655:         init (hex(00)) str(oldreadkey$,45)

L31665:         call "PLOWNEXT" (#37, oldreadkey$, 44%, f1%(37))
                     if f1%(37) = 0% then L31740
                maxdist% = maxdist% + 1%
                get #37 using L31695, str(rcvhnyds$(maxdist%),1,9),       ~
                                     str(rcvhnyds$(maxdist%),10,8),      ~
                                     str(rcvhnyds$(maxdist%),18,32)
L31695:             FMT POS(70),CH(9),POS(87),CH(8),POS(119),CH(32)
L31700:             FMT POS(70), CH(3), CH(6), POS(87), PD(14,4)
                rcvhnyds%(maxdist%) = c%
                buffmax% = buffmax% + 1%
                get #37 using L31700, buffstr$(buffmax%),                 ~
                        bufflot$(buffmax%), buffqty(buffmax%)
                buffidx%(buffmax%) = c%
                goto L31665

L31740:         init (hex(00)) str(oldreadkey$,45)
L31745:         call "PLOWNEXT" (#32, oldreadkey$, 44%, f1%(32))
                     if f1%(32) = 0% then L31815
                postmax% = postmax% + 1%
                get #32 using L31700, poststr$(postmax%),                 ~
                        postlot$(postmax%), postqty(postmax%)
                postidx%(postmax%) = c%
                if found% > 0% then L31745
                maxdist% = maxdist% + 1%
                get #32 using L31695, str(rcvhnyds$(maxdist%),1,9),       ~
                                     str(rcvhnyds$(maxdist%),10,8),      ~
                                     str(rcvhnyds$(maxdist%),18,32)
                rcvhnyds%(maxdist%) = c%
                goto L31745

L31815:         if maxdist% = lastdist% then L31915
                trankey$ = str(rcvrno$,,16) & str(ponumber$(v%),,16)
                for i% = lastdist% + 1% to maxdist%
                    temp% = val(str(rcvhnyds$(i%),42,3),3)
                    if temp% > 2000000% then L31900
                    if temp% = 0% then L31900
                       str(trankey$,33,3) = str(seqnr$(rcvhnyds%(i%)),,3)
                       str(trankey$,36,3) = str(rcvhnyds$(i%),42,3)
                       index%(c%) = max(index%(c%), temp%)
                       temp1% = 0%
                       convert seqnr$(rcvhnyds%(i%)) to temp1%,          ~
                            data goto L31875
L31875:                temp% = 10000%*temp1% + temp%
                       call "SERLOAD" (temp%, "PO", trankey$, 100%, " ", ~
                                       str(rcvhnyds$(i%),1,9),           ~
                                       #2, #40, #42, #41, temp1%, #50)
                       str(rcvhnyds$(i%),45,4) = bin(temp1%,4)
L31900:         next i%
                lastdist% = maxdist%

L31915:         temp, temp1, temp2 = 0
                call "PLOWALTS" (#36, diskkey$, 2%, 28%, f1%(36))
                     if f1%(36) = 0% then L32010
                get #36, using L31935, oldreadkey$, temp, temp1
L31935:             FMT POS(26), CH(44), POS(156), PD(14,4),             ~
                                 POS(288), PD(14,4)
                if str(oldreadkey$,1,16) = str(rcvrno$,1,16) then L31915

                init (hex(00)) str(oldreadkey$,45):temp2 = 0
                call "PLOWALTS" (#31, oldreadkey$, 0%, 44%, f1%(31))
                   if f1%(31) = 0% then L31980
                get #31, using L31975, temp2
L31975:             FMT POS(156), PD(14,4)
L31980:         qtyonord(c%) = max(0, round(qtyonord(c%)-temp+temp2, 2))
                qtyonord(c%) = max(0, round(qtyonord(c%)-abs(temp1), 2))
                qtyrecdtd(c%) = round(qtyrecdtd(c%) - temp2 + temp, 2)
                goto L31915

                if oldqtyonord(c%) < 0 or qtyrecd(c%) = 0 then L32015
L32010:         oldqtyonord(c%) = qtyonord(c%)+qtyrecd(c%)
L32015:         if factor(c%) <= 0 or factor(c%) > 1000000 then          ~
                                                           factor(c%) = 1

                gosub'040
                gosub set_distribution

                call "TXTFUTIL" (#20, 0%, "LOAD", textrc$(c%))

                if abs(qtyrecd(c%)) < .0001 then L31220
                   status$(c%) = "A"
                   goto L31220

L32075: REM EXIT, NOT NECESSARILLY WITH GRACE

            if maxlines% < 1% then                                       ~
               errormsg$ = "P.O. HAS NO LINES ELIGIBLE FOR RECEIPT"
            return

        get_allowed_overage
            ovrrecpct, ovrrecqty, max_to_receive(c%) = 0
            call "READ100" (#6, part$(c%,1), f1%(6))
            if f1%(6) <> 0 then get #6 using L32150, lottrack$(c%),       ~
                                       ovrrecpct, ovrrecqty
L32150:         FMT POS(130), CH(1), POS(211), 2*PD(14,4)

            if ovrrecpct <> 0 or ovrrecqty <> 0 then goto L32190
                max_to_receive(c%) = qtyorig(c%) * overshippct
                return
L32190:     max_to_receive(c%) = min(qtyorig(c%) * ovrrecpct * .01,      ~
                ovrrecqty)
            max_to_receive(c%) = max_to_receive(c%) + qtyorig(c%)
            return

        get_std_cost
            if postatstdcost$ <> "R" then return
            if pos("FST" = costtype$) = 0% then return
                mat stdcost = zer : stdcost = 0
                if str(job$(c%),,2%) = "PJ" then L32280
                     if core_on% <> 1% then L32280
                          gosub get_reman_core_std_cost
                          if reman% <> 0% then return
L32280:         call "STCCOSTS" (part$(c%,1%), " ", #2, 3%, stdcost,     ~
                                 stdcost(), a_bom(), a_rte(), a_dtl())
                if stdcost = 0 then gosub std_cost_warning
                if str(job$(c%),,2%) <> "PJ" then L32300
                     mat stdcost = a_dtl
                     stdcost = 0
                     for i% = 1% to 12%
                          stdcost = stdcost + a_dtl(i%)
                          next i%
L32300:         put cost$(c%) using L32310, stdcost
L32310:              FMT PD(14,4)
                call "PACKZERO" (stdcost(), str(cost$(c%),9%))
                return

        std_cost_warning
L32370:         u3% = 0%
                call "ASKUSER" (u3%, "** ZERO COST WARNING **",          ~
                     part$(c%,1%) & " is a Standard Costed Part,",       ~
                     "but its Standard Cost is Zero!",                   ~
                     "Press RETURN to Accept; Press PF1 to use " &       ~
                     "PO Inv Cost.")
                if u3% = 0% then return
                if u3% <> 1% then L32370
                     return clear    /* Don't return to GET_STD_COST */
                     return         /* Return as if from GET_STD_COST */

        REM *************************************************************~
            *  For Reman Part Determine Standard cost as a sum of the   *~
            *  Reman's Standard Cost and the Implied Core's Std Cost    *~
            *************************************************************
        get_reman_core_std_cost
            init (" ")  remanpart$
            remanpart$ = part$(c%, 1%)
            reman, core = 0
            call "PLOWALTS" (#39, remanpart$, 0%, 25%, reman%)
                if reman% = 0% then return
                if str(remanpart$,26%,25%) <> " " then L32620
                     reman% = 0%
                     return
L32620:     call "STCCOSTS" (str(remanpart$, 1%,25%), " ", #2, 1%, reman)
            call "STCCOSTS" (str(remanpart$,26%,25%), " ", #2, 1%, core)
            stdcost = reman + core
            call "CONVERT" (stdcost, 2.4, invcost$)
            call "HNYCDIST" ("I", part$(c%,1%), " ", " ", #2,            ~
                              str(cost$(c%),9%), invcost$, stdcost)
            put cost$(c%) using L32310, stdcost
            if stdcost = 0 then gosub std_cost_warning
            return
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            if rcvrno$ <> " " then L33070
                if powritten% = 0% then return
                     gosub assign_rcvr_number

L33070:     call "READ101" (#35, rcvrno$, f1%(35))

            call "GETDTTM" addr(str(datetime$,2,7))
            str(datetime$,1,1) = hex(00)

            put #35, using L35180, userid$, datetime$, rcvrno$,           ~
                     carvencode$, cardescr$, bol$, rcvddate$, ohpost$,   ~
                     " ", rcvtime$, rcvcomment$, " "

            if f1%(35) = 0% then write #35 else rewrite #35

            f1%(35) = 1%

            init (hex(00)) oldreadkey$
            str(oldreadkey$, 1,25) = "*CARRIER FREIGHT CHARGE* "
            str(oldreadkey$,26,16) = str(rcvrno$,,16)
L33230:     call "PLOWAL1" (#36, oldreadkey$, 1%, 41%, f1%(36))
               if f1%(36) = 0% then L33270
                  delete #36
                  goto L33230
L33270:     call "GLUNFMT" (carfacct$)
            carfrgtqty = abs(sgn(carfrieght))

L33300:     init (hex(00)) datetime$, cost$(1)
            put str(cost$(1)) using L33320, carfrieght, carfrieght
L33320:         FMT POS(1), 2*PD(14,4)
            call "GETDTTM" addr(str(datetime$,2))
            write #36, using L35340, "*CARRIER FREIGHT CHARGE* ",         ~
                       rcvrno$, carvencode$, "FREIGHT CHARGE", " ",      ~
                       datetime$, cardescr$,                             ~
                       rcvddate$, rcvddate$, rcvddate$,                  ~
                       " ", datetime$, " ", carfrgtqty,                  ~
                       0, 0, 0, 0, 0, carfrgtqty,                        ~
                       carfrieght, 0, 0, 1, "EACH", carfrieght,          ~
                       carfrieght, 0, 0, 0, 0,                           ~
                       carpacct$, rcvacct$, qcacct$, carfacct$, " ", " ",~
                       " ", " ", " ", carfrgtqty, carfrieght,            ~
                       0, 0, " ", " ", 0%, " ", cost$(1), cost$(1),      ~
                       " ", rcvddate$, " ", eod goto L33300
            call "GLFMT" (carfacct$)

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataputpo
            if rcvrno$ <> " " then L34035
                if maxlines% = 0% then L34240
                     gosub assign_rcvr_number

L34035:     diskkey$ = str(rcvrno$,1,16) & str(vencode$(v%),1,9) &       ~
                          ponumber$(v%)

            call "DELETE" (#36, diskkey$, 41%)
            call "DELETE" (#37, diskkey$, 41%)

            if maxlines% = 0% then L34240

            for i% = 1% to maxlines%

            oldqtyonord(i%) = oldqtyonord(i%) - qtyrecd(i%) - qtyonord(i%)

            ext(i%) = round(qtyrecd(i%) * price(i%), 2)

L34105:     call "GETDTTM" addr (str(datetime$,2,7))
            str(datetime$,1,1) = hex(00)

            write #36, using L35340, part$(i%,1), rcvrno$, vencode$(v%),  ~
                       ponumber$(v%), seqnr$(i%), datetime$,             ~
                       partdescr$(i%,1),                                 ~
                       orddate$, duedate$(i%), rcvddate$,                ~
                       packslip$(v%), datetime$,                         ~
                       textpo$(i%),                                      ~
                       qtyrecd(i%),                                      ~
                       qtyrcvhold(i%), qtyqcpend(i%), qtyqchold(i%),     ~
                       qtyscrap(i%), qtyrtv(i%), qtyonhand(i%),          ~
                       price(i%), 0, 0,                                  ~
                       factor(i%), meas$(i%),                            ~
                       ext(i%), ext(i%), 0, 0, 0, oldqtyonord(i%),       ~
                       puracct$, rcvacct$, qcacct$, acct$(i%),           ~
                       str$(i%), lot$(i%), rejstr$(i%), rejlot$(i%),     ~
                       rejcode$(i%), qtyrecd(i%),                        ~
                       price(i%), 0, 0,                                  ~
                       textrc$(i%), textqc$(i%), index%(i%),             ~
                       job$(i%), cost$(i%), cost$(i%), " ", dueorig$(i%),~
                       " ", eod goto L34105

            call "TXTFUTIL" (#20, 0%, "SAVE", textrc$(i%))

            next i%

            powritten% = 1%

L34240:     if maxdist% = 0% then L34655

            for i% = 1% to maxdist%
                if rcvhnyds%(i%) = 0% then L34645

            put str(record$()) using L34315, rcvrno$,                     ~
                                      vencode$(v%), ponumber$(v%),       ~
                                      seqnr$(rcvhnyds%(i%)),             ~
                                      part$(rcvhnyds%(i%),1%),           ~
                                      str(rcvhnyds$(i%),1,9), " ",       ~
                                      str(rcvhnyds$(i%),10,8),           ~
                                      price(rcvhnyds%(i%)),              ~
                                      str(cost$(rcvhnyds%(i%)),,8),      ~
                                      0,                                 ~
                                      str(rcvhnyds$(i%),18,32), " "
L34315:         FMT CH(16), CH(9), CH(16), CH(3), CH(25), CH(9),         ~
                    2*CH(8), PD(14,4), CH(8), PD(14,4), CH(32), CH(50)

                str(record$(),79,8) = all(hex(00))
L34335:         call "GETDTTM" addr(str(record$(),80,7))
                write #37, using L34350, str(record$(),1,200),            ~
                                                        eod goto L34335
L34350:               FMT CH(200)

                temp% = val(str(rcvhnyds$(i%),42,3),3)
                if temp% > 2000000% then L34425
                if temp% = 0% then L34425
                trankey$ = str(rcvrno$,,16) & str(ponumber$(v%),,16) &   ~
                  str(seqnr$(rcvhnyds%(i%)),,3) & str(rcvhnyds$(i%),42,3)
                temp1% = 0%
                convert seqnr$(rcvhnyds%(i%)) to temp1%, data goto L34395
L34395:         temp% = 10000%*temp1% + temp%

                call "SERSAVE" (temp%, "PO", trankey$, 100%,             ~
                                part$(rcvhnyds%(i%),1%), userid$, "2",   ~
                                "7", 0%, #2, #40, #42, #41)

L34425:     if savemark$(i%) <> " " then L34645
               held, need, workqty, buffqty, postqty = 0

            for j% = i% to maxdist%
                if rcvhnyds%(j%) <> rcvhnyds%(i%) then L34480
                if str(rcvhnyds$(j%),1,9) <> str(rcvhnyds$(i%),1,9)      ~
                                                  then L34480
                     get str(rcvhnyds$(j%),10,8) using L34465, temp
L34465:                  FMT PD(14,4)
                     workqty = workqty + temp
                     savemark$(j%) = "*"
L34480:     next j%

            if postmax% = 0% then L34545
            for j% = 1% to postmax%
                if postidx%(j%) <> rcvhnyds%(i%) then L34535
                if str(poststr$(j%),,3) <> str(rcvhnyds$(i%),,3)         ~
                                                 then L34535
                if str(postlot$(j%),,6) <> str(rcvhnyds$(i%),4,6)        ~
                                                 then L34535
                   postqty = postqty + postqty(j%)
                   postidx%(j%) = 0%
L34535:     next j%

L34545:     if buffmax% = 0% then L34605
            for j% = 1% to buffmax%
                if buffidx%(j%) <> rcvhnyds%(i%) then L34590
                if str(buffstr$(j%),,3) <> str(rcvhnyds$(i%),,3)         ~
                                                 then L34590
                if str(bufflot$(j%),,6) <> str(rcvhnyds$(i%),4,6)        ~
                                                 then L34590
                   buffqty = buffqty + buffqty(j%)
                   buffidx%(j%) = 0%
L34590:     next j%
            held = max(0, postqty - buffqty)

L34605:     need = max(0, postqty - workqty)

            holdadj = need - held
            if holdadj = 0 then L34645
            call "HNYHOLD" (#22, part$(rcvhnyds%(i%),1%),                ~
                     str(rcvhnyds$(i%),,3), str(rcvhnyds$(i%),4,6),      ~
                               holdadj, return%)

L34645:     next i%

L34655:     if f1%(35) = 0% then dataput
            return

         set_hold_record
            if rcvrno$ = " " then return
L34810:     call "GETDTTM" addr(str(datetime$,2%,7%))
            str(datetime$,1%,1%) = hex(00)
            put #35 using L34840, hex(000000), datetime$, datetime$,      ~
                                 rcvrno$, " "
                /* This is a temperary hold record that does not match */
                /* the normal file layout.  DATETIME$ is used in the   */
                /* Receiver field to eliminate duplicate keys.  HEX(00)*/
                /* is used so RCVTKTSB and RCVUPDTE don't try to       */
                /* process this record.  'Nuff said.                   */
L34840:         FMT CH(3), CH(8), CH(16), CH(16), CH(107)
            write #35, eod goto L34810
            recvr_held% = 1%
            return

         delete_hold_record
            if rcvrno$ = " " or recvr_held% = 0% then return
            init (hex(00)) miscplow$
L34915:     call "PLOWAL1" (#35, miscplow$, 1%, 3%, f1%(35%)) /*RCVTIF*/
            if f1%(35%) = 0% then return /* Uh Oh, didn't find one */
                get #35 using L34930, temp_rcvrno$
L34930:              FMT POS(28), CH(16)
                if temp_rcvrno$ <> rcvrno$ then L34915 /* Loop */
                     delete #35
                     recvr_held% = 0%
                     return

        check_hold_record
            if rcvrno$ = " " then return
            init (hex(00)) miscplow$
L34960:     call "PLOWALTS" (#35, miscplow$, 1%, 3%, f1%(35%)) /*RCVTIF*/
            if f1%(35%) = 0% then return /* Good, didn't find one */
                get #35 using L34975, temp_rcvrno$
L34975:              FMT POS(28), CH(16)
                if temp_rcvrno$ <> rcvrno$ then L34960 /* Loop */
                     errormsg$ = "Receiver already being processed."
                     return

        REM *************************************************************~
            *          F O R M A T   S T A T E M E N T S                *~
            *-----------------------------------------------------------*~
            * Major Format Statements for Receiver Files                *~
            *************************************************************

            FMT                          /* RECEIVER MASTER FILE       */~
                CH(16),                  /* Receiver Number            */~
                CH(9),                   /* Carrier  Vendor Code       */~
                CH(30),                  /* Carrier Description        */~
                CH(30),                  /* Bill of Lading Number      */~
                CH(6),                   /* Def. Received Date         */~
                CH(1),                   /* Current Disposition Option */~
                CH(4),                   /* Receiver Text Link         */~
                CH(8),                   /* Time                       */~
                CH(30),                  /* Comments                   */~
                CH(16)                   /* Filler                     */~

L35180:     FMT                          /* RECEIVER MASTER TIF        */~
                CH(3),                   /* User Id.                   */~
                CH(8),                   /* Date/Time Stamp            */~
                CH(16),                  /* Receiver Number            */~
                CH(9),                   /* Carrier  Vendor Code       */~
                CH(30),                  /* Carrier Description        */~
                CH(30),                  /* Bill of Lading Number      */~
                CH(6),                   /* Def. Received Date         */~
                CH(1),                   /* Current Disposition Option */~
                CH(4),                   /* Receiver Text Link         */~
                CH(8),                   /* Time                       */~
                CH(30),                  /* Comments                   */~
                CH(5)                    /* Filler                     */~

        REM FOR LINE ITEMS, TIF AND 'MASTER' FILES IDENTICAL

L35340:     FMT                          /* RECEIVER LINE ITEM         */~
                CH(25),                  /* Part Number                */~
                CH(16),                  /* Receiver Number            */~
                CH(9),                   /* Vendor Code                */~
                CH(16),                  /* P.O. Number (PIPIN Tag)    */~
                CH(3),                   /* P.O. Line Sequence Number  */~
                CH(8),                   /* Date/Time Stamp            */~
                CH(32),                  /* Part Description           */~
                CH(6),                   /* Date Ordered               */~
                CH(6),                   /* Date Due - Current         */~
                CH(6),                   /* Date Received              */~
                CH(16),                  /* Vendor's Packslip          */~
                CH(8),                   /* Date/Time Stamp            */~
                CH(4),                   /* P.O. level Receiver Text   */~
                PD(14,4),                /* Total Quantity Received    */~
                PD(14,4),                /*       Receiver Hold        */~
                PD(14,4),                /*       Q.C. Pending         */~
                PD(14,4),                /*       Q.C. Hold            */~
                PD(14,4),                /*       Scrapped/Reject      */~
                PD(14,4),                /*       Return to Vendor     */~
                PD(14,4),                /*       Total to On-Hand     */~
                PD(14,7),                /* Internal Unit Price        */~
                PD(14,7),                /* Filler                     */~
                PD(14,7),                /* Filler                     */~
                PD(14,4),                /* Conversion Factor          */~
                CH(4),                   /* Unit of Measure            */~
                PD(14,4),                /* Extension (Receiver Value) */~
                PD(14,4),                /* Open Amount                */~
                PD(14,4),                /* A/P Invoiced Amount        */~
                PD(14,4),                /* A/P Adjustment             */~
                PD(14,4),                /* Scrap Adjustment           */~
                PD(14,4),                /* Rework Adjustment          */~
                CH(9),                   /* (Pre) Payables Liab. Acct. */~
                CH(9),                   /* Receiver Holding Account   */~
                CH(9),                   /* Q.C. Holding Account       */~
                CH(9),                   /* Pur. Dist. (Exp) Account   */~
                CH(3),                   /* Default Store Target       */~
                CH(6),                   /* Default Lot Target         */~
                CH(3),                   /* Scrap Store                */~
                CH(6),                   /* Scrap Lot                  */~
                CH(6),                   /* Last (Current) Reject Code */~
                PD(14,4),                /* Open (uninvoiced) Qty      */~
                PD(14,7),                /* A/P Price                  */~
                PD(14,7),                /* Filler                     */~
                PD(14,7),                /* Filler                     */~
                CH(4),                   /* Line level Receiver Text   */~
                CH(4),                   /* Q.C. Line Item Text        */~
                BI(4),                   /* High Ser. Index this Doc.  */~
                CH(8),                   /* Job Number                 */~
                2*CH(104),               /* Inventory Costs            */~
                CH(30),                  /* Filler - Use in RCVLINES   */~
                CH(6),                   /* Original Due Date          */~
                CH(149)                  /* Filler                     */

        REM *************************************************************~
            *  LINK TO DISTRIBUTION SUBROUTINE                          *~
            *************************************************************

        call_rcvdist
            if rcvrno$ = " " then gosub assign_rcvr_number
            init (hex(20)) record$(), lines$(), subh$():linemax% = 0%

               str(subh$(1), 1) = "Part Number"
               str(subh$(1),17) = part$(c%,1)
               str(subh$(1),43) = partdescr$(c%,1)
               str(subh$(2), 1) = "Receiver Number"
               str(subh$(2),17) = rcvrno$
               str(subh$(2),34) = "Pack Slip Nmbr"
               str(subh$(2),49) = packslip$(v%)
               str(subh$(2),66) = "Rcvd"
               str(subh$(2),71) = defrcvddate$
               str(subh$(3), 1) = "Vendor Code"
               str(subh$(3),17) = vencode$(v%)
               str(subh$(3),34) = "Purchase Order"
               str(subh$(3),49) = ponumber$(v%)
               str(subh$(3),66) = "PO Line"
               str(subh$(3),74) = seqnr$(c%)

            put record$(), using L35340,                                  ~
                       part$(c%,1), rcvrno$, vencode$(v%),               ~
                       ponumber$(v%), seqnr$(c%), datetime$,             ~
                       partdescr$(c%,1),                                 ~
                       orddate$, duedate$(c%), rcvddate$,                ~
                       packslip$(v%), datetime$,                         ~
                       textpo$(c%),                                      ~
                       qtyrecd(c%),                                      ~
                       qtyrcvhold(c%), qtyqcpend(c%), qtyqchold(c%),     ~
                       qtyscrap(c%), qtyrtv(c%), qtyonhand(c%),          ~
                       price(c%), 0, 0,                                  ~
                       factor(c%), meas$(c%),                            ~
                       ext(c%), ext(c%), 0, 0, 0, 0,                     ~
                       puracct$, rcvacct$, qcacct$, acct$(c%),           ~
                       str$(c%), lot$(c%), rejstr$(c%), rejlot$(c%),     ~
                       rejcode$(c%), 0, 0, 0, 0, textrc$(c%),            ~
                       textqc$(c%), index%(c%), job$(c%), cost$(c%),     ~
                       cost$(c%), " ", dueorig$(c%), " "
            if maxdist% = 0% then L36490
            for i% = 1% to maxdist%
                if rcvhnyds%(i%) <> c% then L36470
                linemax% = linemax% + 1%
                lines$(linemax%) = rcvhnyds$(i%)
L36470:     next i%

L36490:     call "RCVDIST" (record$(), lines$(), linemax%, subh$(), 0%,  ~
                            #19, #20, #21, #6, #2, #22, #40, #41, #42,   ~
                            #51, #52, #23,  #4, #5, #1,                  ~
                            return%, rcvhnyds%(), rcvhnyds$(), maxdist%, ~
                            part$(), rejlot$(), c%,                      ~
                            bufflot$(), buffstr$(), buffqty(),           ~
                            postlot$(), poststr$(), postqty(),           ~
                            buffidx%(), postidx%(), buffmax%, postmax%,  ~
                            delvr$(c%), reqstd$(c%), #14, #15)

            if return% <> 0% then L36910
                  /* ELSE FALL THRU . . .  */

            get record$(),using L36680, qtyrecd(c%),                      ~
                       qtyrcvhold(c%), qtyqcpend(c%), qtyqchold(c%),     ~
                       qtyscrap(c%), qtyrtv(c%), qtyonhand(c%),          ~
                       rejstr$(c%), rejlot$(c%), rejcode$(c%),           ~
                       textrc$(c%), textqc$(c%), index%(c%)

L36680:        FMT POS(156), 7*PD(14,4), POS(341), CH(3), 2*CH(6),       ~
                   POS(388), 2*CH(4), BI(4)

            l% = linemax%
            if maxdist% = 0% then L36840
            for i% = 1% to maxdist%
                if rcvhnyds%(i%)  = 0% then L36760
                if rcvhnyds%(i%) <> c% then L36820
L36760:         if l% <> 0% then L36790
                   rcvhnyds%(i%) = 0%
                   goto L36820
L36790:         rcvhnyds$(i%) = lines$(1% + linemax% - l%)
                rcvhnyds%(i%) = c%
                l% = l% - 1%
L36820:     next i%

L36840:     if l% = 0% then L36910
               maxdist% = maxdist% + 1%
               rcvhnyds$(maxdist%) = lines$(1% + linemax% - l%)
               rcvhnyds%(maxdist%) = c%
               l% = l% - 1%
            goto L36840

L36910:     gosub set_distribution

            return

        dist_quantity
            return% = 0%
            if lot_sys$ = "Y" and lottrack$(c%) = "Y" then L37050
            call "SERENABL" (part$(c%,1%), temp%, temp1%, #2, #6)
               if temp% <> 0% then L37050
            if new_recvr% = 1% then L37040
            call "HNYAVAIL" (#6, #22, part$(c%,1%), " ", " ", workkey$,  ~
                             9e9, temp, temp1%)
               if workkey$ = " " then L37040
                  workkey$ = " "  :  goto L37050
L37040:     if qtyqchold(c%) = 0 then L37100
L37050:        ohpost$(c%) = "D"
               if abs(qtyrecd(c%) - qtyqchold(c%)) <> 0 then L37210
                  gosub clear_dist
                  goto set_distribution

L37100:     if qtyrecd(c%) <> 0 then L37130
               ohpost$(c%) = " "
                  goto clear_dist
L37130:     if ohpost$(c%) <> "I" then L37210
                if lot$(c%) = "?" then lot$(c%) = " "
                call "LOTVALID" (part$(c%,1%), str$(c%), lot$(c%),       ~
                                                 #2, #6, #22, errormsg$)
                if errormsg$ <> " " then L37200
                gosub test_lot_unique
                if errormsg$ = " " then L37210
L37200:              errormsg$ = " "  :  goto L37050
L37210:     on pos("RQID" = ohpost$(c%)) goto L37230, L37280, L37330, L37520

L37230: REM ALL TO RECEIVER HOLD
            gosub clear_dist
            qtyrcvhold(c%) = qtyrecd(c%) - qtyqchold(c%)
            return

L37280: REM ALL TO QC PENDING
            gosub clear_dist
            qtyqcpend(c%) = qtyrecd(c%) - qtyqchold(c%)
            return

L37330: REM ALL TO DEFAULT STORE/LOT
            gosub clear_dist
            qtyonhand(c%) = qtyrecd(c%) - qtyqchold(c%)

            if qtyonhand(c%) = 0 then return
            if maxdist% = 0% then L37420
            for i% = 1% to maxdist%
                if rcvhnyds%(i%) = 0% then L37440
            next i%
L37420:     maxdist%, i% = maxdist% + 1%

L37440:     put rcvhnyds$(i%) using L37470,                               ~
                              str$(c%), lot$(c%), qtyrecd(c%),           ~
                              acct$(c%), date, " ", 0%
L37470:        FMT CH(3), CH(6), PD(14,4), CH(9), CH(6), CH(14), BI(3)

            rcvhnyds%(i%) = c%
            return

L37520: REM DISTRIBUTION REQUIRED
            qtyrcvhold(c%) = qtyrecd(c%) - (qtyqcpend(c%)+qtyonhand(c%)+ ~
                                   qtyrtv(c%)+qtyqchold(c%)+qtyscrap(c%))
            if qtyrecd(c%) = 0 and qtyrcvhold(c%) = 0 then return
            gosub call_rcvdist
            return

        REM CLEAN HOUSE

        clear_dist

            qtyrcvhold(c%), qtyqcpend(c%), qtyonhand(c%), qtyrtv(c%),    ~
            qtyscrap(c%) = 0

            if maxdist% = 0% then L37710
            for i% = 1% to maxdist%
                if rcvhnyds%(i%) = c% then rcvhnyds%(i%) = 0%
            next i%

L37710:     return

        set_distribution

                dist% = 0%:ohpost$(c%) = " "
                if abs(qtyqchold(c%)) >= .01 then dist% = dist% + 2%
                if abs(qtyrtv(c%))    >= .01 then dist% = dist% + 2%
                if abs(qtyscrap(c%))  >= .01 then dist% = dist% + 2%
                if abs(qtyqcpend(c%)) >= .01 then dist% = dist% + 1%
                if abs(qtyrcvhold(c%))>= .01 then dist% = dist% + 1%
                if abs(qtyonhand(c%)) >= .01 then dist% = dist% + 1%
                if dist% < 2% then L37860
L37830:            ohpost$(c%) = "D"
                   return

L37860:         if abs(qtyqcpend(c%)) >= .01 then ohpost$(c%) = "Q"
                if abs(qtyrcvhold(c%))>= .01 then ohpost$(c%) = "R"
                if abs(qtyonhand(c%)) >= .01 then ohpost$(c%) = "I"

                if ohpost$(c%) <> "I" then return
                if maxdist% = 0% then return

                found% = 0%

                for i% = 1% to maxdist%
                    if rcvhnyds%(i%) <> c% then L37990
                       if found% <> 0% then L37830
                          found% = i%
L37990:         next i%

                if str(rcvhnyds$(found%),1,3) <> str$(c%) then L37830
                if str(rcvhnyds$(found%),4,6) <> lot$(c%) then L37830

                return

        test_lot_unique   /* Check that lot is not mentioned elsewhere */
            errormsg$ = " "
            if maxdist% = 0% or lot_unique$ <> "U" or lot$(c%) = " "     ~
                                                              then return
            for i% = 1% to maxdist%
                if rcvhnyds%(i%) = 0% then L38170
                if lot$(c%)   <>  str(rcvhnyds$(i%),4,6) then L38170
                if part$(c%,1) =  part$(rcvhnyds%(i%),1) then L38170
                     errormsg$ = "Lot already used for Part: " &         ~
                                                 part$(rcvhnyds%(i%),1)
                     return
L38170:     next i%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'101(scrn%, fieldnr%)

                  on scrn% goto L40110, L40220, L40310
                     return

L40110:           pfmsg1$ = "(1)Start Over                               ~
        ~                   (13)Instructions"
                  pfmsg2$ = "(2)PO Receipt History (4)Previous Field     ~
        ~                   (15)Print Screen"
                  pfmsg3$ = "(8)Scan Master File   (14)Scan Transaction H~
        ~olding File        (16)EXIT PROGRAM"
                  pfkey$  = hex(00010204080d0e0f100c1d)
                  if fieldnr% > 1% then pfmsg3$, str(pfmsg1$,23,35) = " "~
                                   else str(pfmsg2$,23,17) = " "
                  goto L40400

L40220:           pfmsg1$ = "                      (12)Print Receiver Tic~
        ~kets               (13)Instructions"
                  pfmsg2$ = "(2)P. O. Summary                            ~
        ~                   (15)Print Screen"
                  pfmsg3$ = "                                            ~
        ~                   (16)End Receiver"
                  pfkey$  = hex(00020c0d0f101d)
                  goto L40400

L40310:           pfmsg1$ = "                                            ~
        ~                   (13)Instructions"
                  pfmsg2$ = "                                            ~
        ~                   (15)Print Screen"
                  pfmsg3$ = "                                            ~
        ~                                   "
                  pfkey$  = hex(000d0f1d)
                  goto L40400

L40400:           line2$ = " "
                  str(line2$,62%) = pgmname$ & ": " & str(cms2v$,,8%)
                  init(hex(84)) lfac$():str(pfmsg3$,63,1) = hex(84)
                  on fieldnr% gosub L40630,         /* Receiver Number  */~
                                    L40630,         /* Bill of Lading   */~
                                    L40630,         /* Carrier Vencode  */~
                                    L40630,         /* Carrier Descr    */~
                                    L40630,         /* Def. Recvd date  */~
                                    L40630,         /* Def. Recvd Mode  */~
                                    L40630,         /* Time Received    */~
                                    L40630,         /* Comment          */~
                                    L40660,         /* Carrier Freight  */~
                                    L40630          /* Car Frieght Acct */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  if fieldnr% <> 0% then L41000
                     inpmessage$ = edtmessage$
                     init(hex(86)) lfac$()
                     goto L41000

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40630:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40660:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L41000:     accept                                                       ~
               at (01,02),                                               ~
                  "Input/Manage Purchase Order Receipts:  Receiver Inform~
        ~ation",                                                          ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Receiver Number",                                     ~
               at (06,30), fac(lfac$( 1)), rcvrno$              , ch(16),~
               at (07,02),                                               ~
                  "Bill of Lading",                                      ~
               at (07,30), fac(lfac$( 2)), bol$                 , ch(30),~
               at (08,02),                                               ~
                  "Carrier Vendor Code",                                 ~
               at (08,30), fac(lfac$( 3)), carvencode$          , ch(09),~
               at (09,02),                                               ~
                  "Carrier Description",                                 ~
               at (09,30), fac(lfac$( 4)), cardescr$            , ch(30),~
               at (10,02),                                               ~
                  "Received Date",                                       ~
               at (10,30), fac(lfac$( 5)), defrcvddate$         , ch(08),~
               at (11,02),                                               ~
                  "Default Received Mode",                               ~
               at (11,30), fac(lfac$( 6)), ohpost$              , ch(01),~
               at (12,02),                                               ~
                  "Time of Receipt",                                     ~
               at (12,30), fac(lfac$( 7)), rcvtime$             , ch(08),~
               at (13,02),                                               ~
                  "Comments",                                            ~
               at (13,30), fac(lfac$( 8)), rcvcomment$          , ch(30),~
               at (14,02),                                               ~
                  "Carrier Freight",                                     ~
               at (14,30), fac(lfac$( 9)), carfrieght$          , ch(10),~
               at (15,02),                                               ~
                  "Carrier Freight Acct",                                ~
               at (15,30), fac(lfac$(10)), carfacct$            , ch(12),~
               at (15,49), fac(hex(8c)),   carfacctdescr$       , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 2 then L41570
                  if fieldnr% <> 1 then L41570
                  call "OPENCHCK" (#7, fs%(7), f2%(7), 0%, rslt$(7))
                  call "OPENCHCK" (#8, fs%(8), f2%(8), 0%, rslt$(8))
                  vencode$(1), ponumber$(1), seqnr$(1) = " "
                  call "POSTATUS" (vencode$(1), ponumber$(1), seqnr$(1), ~
                                            0%, #5, #31, #4, #8, #7, #20)
                  vencode$(1), ponumber$(1), seqnr$(1) = " "
                  goto L41000
L41570:



               if keyhit% <> 13 then L41650
                  call "MANUAL" (pgmname$)
                  goto L41000

L41650:        if keyhit% <> 15 then L41690
                  call "PRNTSCRN"
                  goto L41000

L41690:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'102(scrn%, fieldnr%)
*                F% = V% + (20%*(FIELDNR%-1%))
                  f% = s% + (20%*(fieldnr%-1%))
                  on scrn% goto L42120, L42310, L42400
                     return

L42120:           pfmsg1$ = "(1)Start Over                               ~
        ~                   (13)Instructions"
                  pfmsg2$ = "(2)Column One      (4)Previous Field        ~
        ~                   (15)Print Screen"
                  pfmsg3$ = "                   (6)Previous Line         ~
        ~                   (16)Edit Mode   "
                  pfkey$  = hex(00010204060d0f10)

                  if fieldnr% > 1% then L42230
                     str(pfmsg2$,,37) = " ":str(pfkey$,3,2) = hex(ffff)
                        goto L42250
L42230:              str(pfmsg3$,63)  = " ":str(pfkey$,8,1) = hex(ff)

L42250:           if maxpo% = 0% then L42280
                     str(pfmsg1$,,16) = " ":str(pfkey$,2,1) = hex(ff)
                        goto L42490
L42280:              str(pfmsg3$,,37) = " ":str(pfkey$,5,1) = hex(ff)
                        goto L42490

L42310:           pfmsg1$ = "                                            ~
        ~  (9)Receiver      (13)Instructions"
                  pfmsg2$ = "(2)First P.O.s     (4)Previous   (6)Down One~
        ~                   (15)Print Screen"
                  pfmsg3$ = "(3)Last P.O.s      (5)Next       (7)Up One  ~
        ~  (11)Add P.O.s    (16)End Receiver"
                  pfkey$  = hex(00020304050607090b0d0f10)
                  goto L42490

L42400:           pfmsg1$ = "                                            ~
        ~                   (13)Instructions"
                  pfmsg2$ = "                                            ~
        ~                   (15)Print Screen"
                  pfmsg3$ = "                                            ~
        ~                                   "
                  pfkey$  = hex(000d0f1d)
                  goto L42490

L42490:           line2$ = " "
                  line2$ = "Receiver: " & rcvrno$
                  str(line2$,28%) = cardescr$
                  str(line2$,62%) = pgmname$ & ": " & str(cms2v$,,8%)
                  init(hex(84)) lfac$():str(pfmsg3$,63,1) = hex(84)
                  on fieldnr% gosub L42690,         /* VENDOR           */~
                                    L42710,         /* PO NUMBER        */~
                                    L42710          /* PACKSLIP         */

                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(f%) = or hex(10)
                  if fieldnr% <> 0% then L43000
                     inpmessage$ = edtmessage$
                     init(hex(86)) str(lfac$(),,20)
                     init(hex(86)) str(lfac$(),41,20)
                     goto L43000

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(f%) = hex(80)
                      return
L42690:           REM Set FAC's for Upper Case Only Input
                      lfac$(f% + 20%) = hex(81)
L42710:           REM Set FAC's for Upper Case Only Input
                      lfac$(f%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(f%) = hex(82)
                      return

L43000:     accept                                                       ~
               at (01,02),                                               ~
                  "Input/Manage Purchase Order Receipts:  Purchase Order ~
        ~Summary",                                                        ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), fac(hex(a4)), headsc$(1)             , ch(79),~
                                                                         ~
               at (07,02), fac(lfac$( 1%)), vencode$(vl%+ 1%)   , ch( 9),~
               at (08,02), fac(lfac$( 2%)), vencode$(vl%+ 2%)   , ch( 9),~
               at (09,02), fac(lfac$( 3%)), vencode$(vl%+ 3%)   , ch( 9),~
               at (10,02), fac(lfac$( 4%)), vencode$(vl%+ 4%)   , ch( 9),~
               at (11,02), fac(lfac$( 5%)), vencode$(vl%+ 5%)   , ch( 9),~
               at (12,02), fac(lfac$( 6%)), vencode$(vl%+ 6%)   , ch( 9),~
               at (13,02), fac(lfac$( 7%)), vencode$(vl%+ 7%)   , ch( 9),~
               at (14,02), fac(lfac$( 8%)), vencode$(vl%+ 8%)   , ch( 9),~
               at (15,02), fac(lfac$( 9%)), vencode$(vl%+ 9%)   , ch( 9),~
               at (16,02), fac(lfac$(10%)), vencode$(vl%+10%)   , ch( 9),~
               at (17,02), fac(lfac$(11%)), vencode$(vl%+11%)   , ch( 9),~
               at (18,02), fac(lfac$(12%)), vencode$(vl%+12%)   , ch( 9),~
               at (19,02), fac(lfac$(13%)), vencode$(vl%+13%)   , ch( 9),~
                                                                         ~
               at (07,12), fac(lfac$(21%)), ponumber$(vl%+ 1%)  , ch(14),~
               at (08,12), fac(lfac$(22%)), ponumber$(vl%+ 2%)  , ch(14),~
               at (09,12), fac(lfac$(23%)), ponumber$(vl%+ 3%)  , ch(14),~
               at (10,12), fac(lfac$(24%)), ponumber$(vl%+ 4%)  , ch(14),~
               at (11,12), fac(lfac$(25%)), ponumber$(vl%+ 5%)  , ch(14),~
               at (12,12), fac(lfac$(26%)), ponumber$(vl%+ 6%)  , ch(14),~
               at (13,12), fac(lfac$(27%)), ponumber$(vl%+ 7%)  , ch(14),~
               at (14,12), fac(lfac$(28%)), ponumber$(vl%+ 8%)  , ch(14),~
               at (15,12), fac(lfac$(29%)), ponumber$(vl%+ 9%)  , ch(14),~
               at (16,12), fac(lfac$(30%)), ponumber$(vl%+10%)  , ch(14),~
               at (17,12), fac(lfac$(31%)), ponumber$(vl%+11%)  , ch(14),~
               at (18,12), fac(lfac$(32%)), ponumber$(vl%+12%)  , ch(14),~
               at (19,12), fac(lfac$(33%)), ponumber$(vl%+13%)  , ch(14),~
                                                                         ~
               at (07,27), fac(hex(8c)), venname$(vl%+ 1%)      , ch(30),~
               at (08,27), fac(hex(8c)), venname$(vl%+ 2%)      , ch(30),~
               at (09,27), fac(hex(8c)), venname$(vl%+ 3%)      , ch(30),~
               at (10,27), fac(hex(8c)), venname$(vl%+ 4%)      , ch(30),~
               at (11,27), fac(hex(8c)), venname$(vl%+ 5%)      , ch(30),~
               at (12,27), fac(hex(8c)), venname$(vl%+ 6%)      , ch(30),~
               at (13,27), fac(hex(8c)), venname$(vl%+ 7%)      , ch(30),~
               at (14,27), fac(hex(8c)), venname$(vl%+ 8%)      , ch(30),~
               at (15,27), fac(hex(8c)), venname$(vl%+ 9%)      , ch(30),~
               at (16,27), fac(hex(8c)), venname$(vl%+10%)      , ch(30),~
               at (17,27), fac(hex(8c)), venname$(vl%+11%)      , ch(30),~
               at (18,27), fac(hex(8c)), venname$(vl%+12%)      , ch(30),~
               at (19,27), fac(hex(8c)), venname$(vl%+13%)      , ch(30),~
                                                                         ~
               at (07,58), fac(lfac$(41%)), packslip$(vl%+ 1%)  , ch(16),~
               at (08,58), fac(lfac$(42%)), packslip$(vl%+ 2%)  , ch(16),~
               at (09,58), fac(lfac$(43%)), packslip$(vl%+ 3%)  , ch(16),~
               at (10,58), fac(lfac$(44%)), packslip$(vl%+ 4%)  , ch(16),~
               at (11,58), fac(lfac$(45%)), packslip$(vl%+ 5%)  , ch(16),~
               at (12,58), fac(lfac$(46%)), packslip$(vl%+ 6%)  , ch(16),~
               at (13,58), fac(lfac$(47%)), packslip$(vl%+ 7%)  , ch(16),~
               at (14,58), fac(lfac$(48%)), packslip$(vl%+ 8%)  , ch(16),~
               at (15,58), fac(lfac$(49%)), packslip$(vl%+ 9%)  , ch(16),~
               at (16,58), fac(lfac$(50%)), packslip$(vl%+10%)  , ch(16),~
               at (17,58), fac(lfac$(51%)), packslip$(vl%+11%)  , ch(16),~
               at (18,58), fac(lfac$(52%)), packslip$(vl%+12%)  , ch(16),~
               at (19,58), fac(lfac$(53%)), packslip$(vl%+13%)  , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L43780
                  call "MANUAL" (pgmname$)
                  goto L43000

L43780:        if keyhit% <> 15 then L43820
                  call "PRNTSCRN"
                  goto L43000

L43820:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'103(scrn%, fieldnr%)
                  uom% = 0%
                  f% = s% + (20%*(fieldnr%-1%))
                  m% = 1% + mod(mode%,2%)
                  if m% = 1% then str(headsc$(2),5,8) = "Internal"       ~
                             else str(headsc$(2),5,8) = "Vendor's"
                  on scrn% goto L44070, L44120, L44165, L44205
                     return

L44070:           pfmsg1$ = "(1)Cancel Receipt Changes                   ~
        ~                   (13)Instructions"
                  pfmsg2$ = "(2)Restart Current Line                     ~
        ~                   (15)Print Screen"
                  pfmsg3$ = "                   (8)Change Display    (14)~
        ~UOM Toggle         (16)Edit Mode   "
                  str(pfmsg3$,63,1) = hex(84)
                  pfkey$  = hex(000102080d0e0f10)
                  if fieldnr% = 1% then str(pfmsg2$,20,17) = " "
                  goto L44265

L44120:           pfmsg1$ = "(1)ReEnter PO   (6/7)Up/Down   (12)Use    (9~
        ~/10)Rec. Lines/Comp.   (13)Instruc"
                  pfmsg2$ = "(2/3)First/Last (11)Inv Status (24)Locns  (2~
        ~5/26)Rcvr/P.O. Text    (15)Prt Scr"
                  pfmsg3$ = "(4/5)Prev/Next  (8/14)Chg. Disp/Info (28)Res~
        ~et Qty Recd (30)See LI (16)SAVE PO"
                  str(pfmsg3$,67%,1%) = hex(84)
            pfkey$ = hex(000102030405060708090a0b0c0d0e0f1018191a1c1d1e17)
                  if po_in_rcvlines% = 0% then L44265
                     str(pfmsg3$,38%,18%) = " "
                     str(pfkey$,21%,1%) = hex(ff)
                     goto L44265

L44165:           pfmsg1$ = "(1)Cancel Receipt Changes                   ~
        ~                   (13)Instructions"
                  pfmsg2$ = "                                            ~
        ~                   (15)Print Screen"
                  pfmsg3$ = "                   (8)Change Display        ~
        ~                                   "
                  pfkey$  = hex(0001080d0f)
                  goto L44265
L44205: REM IF Inventory Status PF11 was Selected
                  pfmsg1$ = "                                            ~
        ~                                   "
                  pfmsg2$ = "                                            ~
        ~                                   "
                  pfmsg3$ = "                                            ~
        ~                   (16)Exit Display"
                  str(pfmsg3$,63,1) = hex(84)
                  pfkey$ = all(hex(00))
                  pfkey$ = hex(000010)
                  inpmessage$ = "Position cursor and press Return to Sele~
        ~ct a Part For Inventory Display"
                  line3$ = " "
L44265:           line2$ = " "
                  line2$ = "Vendor: " & vencode$(v%)
                  str(line2$,19) = "P.O.Number: " & ponumber$(v%)
                  str(line2$,46) = "From Line:"
                  put str(line2$,57,2) using L44295, l% + 1%
                  str(line2$,62%) = pgmname$ & ": " & str(cms2v$,,8%)
L44295:              FMT PIC(##)
                  line3$ = "Ordered By: " & reqstd$(l%+1%)
                  str(line3$,33) = "Deliver To: " & str(delvr$(l%+1%))
*                STR(LINE3$,66) = "Job: " & STR(JOB$(L%+1%))
                  init(hex(84)) lfac$()
*                STR(LINE2$, 8,1) = HEX(84):STR(LINE2$,18,1) = HEX(8C)
*                STR(LINE2$,30,1) = HEX(84):STR(LINE2$,45,1) = HEX(8C)
                  str(line3$,12,1) = hex(84):str(line3$,32,1) = hex(8c)
                  str(line3$,44,1) = hex(84):str(line3$,65,1) = hex(8c)
                  str(line3$,70,1) = hex(84)
                  firsttime% = 0%
                  on fieldnr% gosub L44435,    /* Never Enable Tab Stop */~
                                    L44440,    /* Receipt Mode          */~
                                    L44455,    /* QTY This Receiver     */~
                                    L44455     /* QTY Still Open        */

                  if errormsg$ > " " and fieldnr% > 0% then              ~
                                                   lfac$(f%) = or hex(10)
                  if fieldnr% <> 0% then L45000
                  if scrn% > 3% then L44410
                     inpmessage$="To Receive a Line Item, Position Cursor~
        ~ and Touch ENTER.  PF23 to Find Part."
L44410:              init(hex(86)) str(lfac$())
                     goto L45000

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(f%) = hex(80)
L44435:               return
L44440:           REM Set FAC's for Upper Case Only Input
                      lfac$(f%) = hex(81)
                      return
L44455:           REM Set FAC's for Numeric Only Input
                      lfac$(f%) = hex(82)
                      return

L45000:     gosub set_qty_uom
            accept                                                       ~
               at (01,02),                                               ~
                  "Input/Manage Purchase Order Receipts:  Line Item Summa~
        ~ry",                                                             ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), line3$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), fac(hex(a4)), headsc$(2)             , ch(79),~
                                                                         ~
               at (07,02), fac(lfac$( 1)), seqnr$(l%+ 1%)       , ch(03),~
               at (08,02), fac(lfac$( 2)), seqnr$(l%+ 2%)       , ch(03),~
               at (09,02), fac(lfac$( 3)), seqnr$(l%+ 3%)       , ch(03),~
               at (10,02), fac(lfac$( 4)), seqnr$(l%+ 4%)       , ch(03),~
               at (11,02), fac(lfac$( 5)), seqnr$(l%+ 5%)       , ch(03),~
               at (12,02), fac(lfac$( 6)), seqnr$(l%+ 6%)       , ch(03),~
               at (13,02), fac(lfac$( 7)), seqnr$(l%+ 7%)       , ch(03),~
               at (14,02), fac(lfac$( 8)), seqnr$(l%+ 8%)       , ch(03),~
               at (15,02), fac(lfac$( 9)), seqnr$(l%+ 9%)       , ch(03),~
               at (16,02), fac(lfac$(10)), seqnr$(l%+10%)       , ch(03),~
               at (17,02), fac(lfac$(11)), seqnr$(l%+11%)       , ch(03),~
               at (18,02), fac(lfac$(12)), seqnr$(l%+12%)       , ch(03),~
               at (19,02), fac(lfac$(13)), seqnr$(l%+13%)       , ch(03),~
                                                                         ~
               at (07,06), fac(hex(84)),   part$(l%+ 1%,m%)     , ch(25),~
               at (08,06), fac(hex(84)),   part$(l%+ 2%,m%)     , ch(25),~
               at (09,06), fac(hex(84)),   part$(l%+ 3%,m%)     , ch(25),~
               at (10,06), fac(hex(84)),   part$(l%+ 4%,m%)     , ch(25),~
               at (11,06), fac(hex(84)),   part$(l%+ 5%,m%)     , ch(25),~
               at (12,06), fac(hex(84)),   part$(l%+ 6%,m%)     , ch(25),~
               at (13,06), fac(hex(84)),   part$(l%+ 7%,m%)     , ch(25),~
               at (14,06), fac(hex(84)),   part$(l%+ 8%,m%)     , ch(25),~
               at (15,06), fac(hex(84)),   part$(l%+ 9%,m%)     , ch(25),~
               at (16,06), fac(hex(84)),   part$(l%+10%,m%)     , ch(25),~
               at (17,06), fac(hex(84)),   part$(l%+11%,m%)     , ch(25),~
               at (18,06), fac(hex(84)),   part$(l%+12%,m%)     , ch(25),~
               at (19,06), fac(hex(84)),   part$(l%+13%,m%)     , ch(25),~
                                                                         ~
               at (07,32), fac(hex(8c)), qty_uom$  (01%)        , ch(10),~
               at (08,32), fac(hex(8c)), qty_uom$  (02%)        , ch(10),~
               at (09,32), fac(hex(8c)), qty_uom$  (03%)        , ch(10),~
               at (10,32), fac(hex(8c)), qty_uom$  (04%)        , ch(10),~
               at (11,32), fac(hex(8c)), qty_uom$  (05%)        , ch(10),~
               at (12,32), fac(hex(8c)), qty_uom$  (06%)        , ch(10),~
               at (13,32), fac(hex(8c)), qty_uom$  (07%)        , ch(10),~
               at (14,32), fac(hex(8c)), qty_uom$  (08%)        , ch(10),~
               at (15,32), fac(hex(8c)), qty_uom$  (09%)        , ch(10),~
               at (16,32), fac(hex(8c)), qty_uom$  (10%)        , ch(10),~
               at (17,32), fac(hex(8c)), qty_uom$  (11%)        , ch(10),~
               at (18,32), fac(hex(8c)), qty_uom$  (12%)        , ch(10),~
               at (19,32), fac(hex(8c)), qty_uom$  (13%)        , ch(10),~
                                                                         ~
               at (07,43), fac(hex(8c)), qtyrecdtd$(l%+ 1%,m%)  , ch(10),~
               at (08,43), fac(hex(8c)), qtyrecdtd$(l%+ 2%,m%)  , ch(10),~
               at (09,43), fac(hex(8c)), qtyrecdtd$(l%+ 3%,m%)  , ch(10),~
               at (10,43), fac(hex(8c)), qtyrecdtd$(l%+ 4%,m%)  , ch(10),~
               at (11,43), fac(hex(8c)), qtyrecdtd$(l%+ 5%,m%)  , ch(10),~
               at (12,43), fac(hex(8c)), qtyrecdtd$(l%+ 6%,m%)  , ch(10),~
               at (13,43), fac(hex(8c)), qtyrecdtd$(l%+ 7%,m%)  , ch(10),~
               at (14,43), fac(hex(8c)), qtyrecdtd$(l%+ 8%,m%)  , ch(10),~
               at (15,43), fac(hex(8c)), qtyrecdtd$(l%+ 9%,m%)  , ch(10),~
               at (16,43), fac(hex(8c)), qtyrecdtd$(l%+10%,m%)  , ch(10),~
               at (17,43), fac(hex(8c)), qtyrecdtd$(l%+11%,m%)  , ch(10),~
               at (18,43), fac(hex(8c)), qtyrecdtd$(l%+12%,m%)  , ch(10),~
               at (19,43), fac(hex(8c)), qtyrecdtd$(l%+13%,m%)  , ch(10),~
                                                                         ~
               at (07,57), fac(lfac$(21%)), ohpost$  (l%+ 1%)   , ch(01),~
               at (08,57), fac(lfac$(22%)), ohpost$  (l%+ 2%)   , ch(01),~
               at (09,57), fac(lfac$(23%)), ohpost$  (l%+ 3%)   , ch(01),~
               at (10,57), fac(lfac$(24%)), ohpost$  (l%+ 4%)   , ch(01),~
               at (11,57), fac(lfac$(25%)), ohpost$  (l%+ 5%)   , ch(01),~
               at (12,57), fac(lfac$(26%)), ohpost$  (l%+ 6%)   , ch(01),~
               at (13,57), fac(lfac$(27%)), ohpost$  (l%+ 7%)   , ch(01),~
               at (14,57), fac(lfac$(28%)), ohpost$  (l%+ 8%)   , ch(01),~
               at (15,57), fac(lfac$(29%)), ohpost$  (l%+ 9%)   , ch(01),~
               at (16,57), fac(lfac$(30%)), ohpost$  (l%+10%)   , ch(01),~
               at (17,57), fac(lfac$(31%)), ohpost$  (l%+11%)   , ch(01),~
               at (18,57), fac(lfac$(32%)), ohpost$  (l%+12%)   , ch(01),~
               at (19,57), fac(lfac$(33%)), ohpost$  (l%+13%)   , ch(01),~
                                                                         ~
               at (07,59), fac(lfac$(41%)), qtyrecd$ (l%+ 1%,m%), ch(10),~
               at (08,59), fac(lfac$(42%)), qtyrecd$ (l%+ 2%,m%), ch(10),~
               at (09,59), fac(lfac$(43%)), qtyrecd$ (l%+ 3%,m%), ch(10),~
               at (10,59), fac(lfac$(44%)), qtyrecd$ (l%+ 4%,m%), ch(10),~
               at (11,59), fac(lfac$(45%)), qtyrecd$ (l%+ 5%,m%), ch(10),~
               at (12,59), fac(lfac$(46%)), qtyrecd$ (l%+ 6%,m%), ch(10),~
               at (13,59), fac(lfac$(47%)), qtyrecd$ (l%+ 7%,m%), ch(10),~
               at (14,59), fac(lfac$(48%)), qtyrecd$ (l%+ 8%,m%), ch(10),~
               at (15,59), fac(lfac$(49%)), qtyrecd$ (l%+ 9%,m%), ch(10),~
               at (16,59), fac(lfac$(50%)), qtyrecd$ (l%+10%,m%), ch(10),~
               at (17,59), fac(lfac$(51%)), qtyrecd$ (l%+11%,m%), ch(10),~
               at (18,59), fac(lfac$(52%)), qtyrecd$ (l%+12%,m%), ch(10),~
               at (19,59), fac(lfac$(53%)), qtyrecd$ (l%+13%,m%), ch(10),~
                                                                         ~
               at (07,70), fac(lfac$(61%)), qtyonord$(l%+ 1%,m%), ch(10),~
               at (08,70), fac(lfac$(62%)), qtyonord$(l%+ 2%,m%), ch(10),~
               at (09,70), fac(lfac$(63%)), qtyonord$(l%+ 3%,m%), ch(10),~
               at (10,70), fac(lfac$(64%)), qtyonord$(l%+ 4%,m%), ch(10),~
               at (11,70), fac(lfac$(65%)), qtyonord$(l%+ 5%,m%), ch(10),~
               at (12,70), fac(lfac$(66%)), qtyonord$(l%+ 6%,m%), ch(10),~
               at (13,70), fac(lfac$(67%)), qtyonord$(l%+ 7%,m%), ch(10),~
               at (14,70), fac(lfac$(68%)), qtyonord$(l%+ 8%,m%), ch(10),~
               at (15,70), fac(lfac$(69%)), qtyonord$(l%+ 9%,m%), ch(10),~
               at (16,70), fac(lfac$(70%)), qtyonord$(l%+10%,m%), ch(10),~
               at (17,70), fac(lfac$(71%)), qtyonord$(l%+11%,m%), ch(10),~
               at (18,70), fac(lfac$(72%)), qtyonord$(l%+12%,m%), ch(10),~
               at (19,70), fac(lfac$(73%)), qtyonord$(l%+13%,m%), ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 14 then L45585
                  if scrn% <> 1% then L45585
                      if uom% = 0% then uom% = 1% else uom% = 0%
                      goto L45000

L45585:        if keyhit% <> 13 then L45605
                  call "MANUAL" (pgmname$)
                  goto L45000

L45605:        if keyhit% <> 15 then L45625
                  call "PRNTSCRN"
                  goto L45000

L45625:       if keyhit% <> 12% and keyhit% <> 30% then L45640
                 goto L45680

L45640:       if keyhit% <> 11% then L45647
                 goto L45720

L45647:       if keyhit% <> 24% then L45655
                 goto L45800

L45655:        close ws
               firsttime% = 0%
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L45680:        close ws  /* Both PF12 & PF30 handled here to save code */
              call "SCREEN" addr ("C",u3%, "I", i$(), cursor%())
              fieldnr% = cursor%(1) - 6
              if fieldnr% < 1% or fieldnr% > 13% then L45000
              c% = min(l% + fieldnr%, maxlines%)
              if keyhit% = 12%                                           ~
                   then call "PIPOASUB" (part$(c%,1),#1,#6,#4,#5)        ~
                   else call "VBKLIDSP" (vencode$(v%), ponumber$(v%),    ~
                                         seqnr$(c%), #5, #6, #19)
              return

L45720:        close ws
              call "SCREEN" addr ("C",u3%, "I", i$(), cursor%())
              fieldnr% = cursor%(1) - 6
              if fieldnr% < 1% or fieldnr% > 13% then L45000
              c% = min(l% + fieldnr%, maxlines%)
              call "HNYQDISP" (part$(c%,1), #6, #22, #23, #2)
              return

L45800:       close ws
              call "SCREEN" addr ("C",u3%, "I", i$(), cursor%())
              fieldnr% = cursor%(1) - 6
              if fieldnr% < 1% or fieldnr% > 13% then L45000
              c% = min(l% + fieldnr%, maxlines%)
              if qtyrecd$(c%,1) <> " " then L45880
                  qtypassed = 0
                  goto L45890
L45880:       convert qtyrecd$(c%,1) to qtypassed
L45890:       call "HNYLCSUB"    (part$(c%,1),   /*   Part Number      */~
                                  str$(c%),      /*   Store Number     */~
                                  " ",           /*   Lot Number       */~
                                  qtypassed,     /*   Quantity         */~
                                  3%,            /*   Action = ADD     */~
                                  #2,            /*   SYSFILE2         */~
                                  #19,           /*   STORNAME         */~
                                  #1,            /*   USERINFO         */~
                                  #6,            /*   HNYMASTR         */~
                                  #14,           /*   HNYLOCNS         */~
                                  #22,           /*   HNYQUAN          */~
                                  #15)           /*   LOCATION         */
              return

        set_qty_uom
            init (" ") qty_uom$()      /* No holdover phantoms */
            for i% = 1% to 13%
                if qtyorig$(l%+i%,m%) = " " then L45940
                if uom% = 0% then qty_uom$(i%) = qtyorig$(l%+i%,m%)      ~
                    else qty_uom$(i%) = "    " & uom$(l%+i%,m%)
L45940:     next i%
                if uom% = 0% then str(headsc$(2),31,10) = "Org. Order"   ~
                    else str(headsc$(2),31,10) = "    UOM    "
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'104(scrn%, fieldnr%)

                  m% = 1% + mod(mode2%,2%)

L46100:           pfmsg1$ = "(1)Cancel Receiver Changes                  ~
        ~                   (13)Instructions"
                  pfmsg2$ = "(2/3)First/Last  (6/7)Up/Down         (25)Pa~
        ~rt Text            (15)Print Screen"
                  pfmsg3$ = "(4/5)Prev/Next   (8/14)Chg. Disp/Info  (9)P.~
        ~O. Line Info.      (16)SAVE THIS PO"
                  pfkey$  = hex(0102030405060708090d0e0f1019)

                  line2$ = " "
                  line2$ = "Vendor: " & vencode$(v%)
                  str(line2$,19) = "P.O.Number: " & ponumber$(v%)
                  str(line2$,46) = "From Line:"
                  put str(line2$,57,2) using L46240, l% + 1%
                  str(line2$,62%) = pgmname$ & ": " & str(cms2v$,,8%)
L46240:              FMT PIC(##)
                  line3$ = "Ordered By: " & reqstd$(l%+1%)
                  str(line3$,33) = "Deliver To: " & str(delvr$(l%+1%))
*                STR(LINE3$,66) = "Job: " & STR(JOB$(L%+1%))
                  init(hex(86)) lfac$()
                  str(pfmsg3$,63,1) = hex(84)
                  inpmessage$ = "Press PF14 to Return to Receipt Processi~
        ~ng"
                  str(line3$,12,1) = hex(84):str(line3$,32,1) = hex(8c)
                  str(line3$,44,1) = hex(84):str(line3$,65,1) = hex(8c)
                  str(line3$,70,1) = hex(84)

L47000:     accept                                                       ~
               at (01,02),                                               ~
                  "Input/Manage Purchase Order Receipts:  Line Item Infor~
        ~mation",                                                         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), line3$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), fac(hex(a4)), headsc$(2%+m%)         , ch(79),~
                                                                         ~
               at (07,02), fac(lfac$( 1)), seqnr$(l%+ 1%)       , ch(03),~
               at (08,02), fac(lfac$( 2)), seqnr$(l%+ 2%)       , ch(03),~
               at (09,02), fac(lfac$( 3)), seqnr$(l%+ 3%)       , ch(03),~
               at (10,02), fac(lfac$( 4)), seqnr$(l%+ 4%)       , ch(03),~
               at (11,02), fac(lfac$( 5)), seqnr$(l%+ 5%)       , ch(03),~
               at (12,02), fac(lfac$( 6)), seqnr$(l%+ 6%)       , ch(03),~
               at (13,02), fac(lfac$( 7)), seqnr$(l%+ 7%)       , ch(03),~
               at (14,02), fac(lfac$( 8)), seqnr$(l%+ 8%)       , ch(03),~
               at (15,02), fac(lfac$( 9)), seqnr$(l%+ 9%)       , ch(03),~
               at (16,02), fac(lfac$(10)), seqnr$(l%+10%)       , ch(03),~
               at (17,02), fac(lfac$(11)), seqnr$(l%+11%)       , ch(03),~
               at (18,02), fac(lfac$(12)), seqnr$(l%+12%)       , ch(03),~
               at (19,02), fac(lfac$(13)), seqnr$(l%+13%)       , ch(03),~
                                                                         ~
               at (07,06), fac(hex(84)),   part$(l%+ 1%,1%)     , ch(25),~
               at (08,06), fac(hex(84)),   part$(l%+ 2%,1%)     , ch(25),~
               at (09,06), fac(hex(84)),   part$(l%+ 3%,1%)     , ch(25),~
               at (10,06), fac(hex(84)),   part$(l%+ 4%,1%)     , ch(25),~
               at (11,06), fac(hex(84)),   part$(l%+ 5%,1%)     , ch(25),~
               at (12,06), fac(hex(84)),   part$(l%+ 6%,1%)     , ch(25),~
               at (13,06), fac(hex(84)),   part$(l%+ 7%,1%)     , ch(25),~
               at (14,06), fac(hex(84)),   part$(l%+ 8%,1%)     , ch(25),~
               at (15,06), fac(hex(84)),   part$(l%+ 9%,1%)     , ch(25),~
               at (16,06), fac(hex(84)),   part$(l%+10%,1%)     , ch(25),~
               at (17,06), fac(hex(84)),   part$(l%+11%,1%)     , ch(25),~
               at (18,06), fac(hex(84)),   part$(l%+12%,1%)     , ch(25),~
               at (19,06), fac(hex(84)),   part$(l%+13%,1%)     , ch(25),~
                                                                         ~
               at (07,32), fac(hex(8c)), partdescr$(l%+ 1%,m%)  , ch(32),~
               at (08,32), fac(hex(8c)), partdescr$(l%+ 2%,m%)  , ch(32),~
               at (09,32), fac(hex(8c)), partdescr$(l%+ 3%,m%)  , ch(32),~
               at (10,32), fac(hex(8c)), partdescr$(l%+ 4%,m%)  , ch(32),~
               at (11,32), fac(hex(8c)), partdescr$(l%+ 5%,m%)  , ch(32),~
               at (12,32), fac(hex(8c)), partdescr$(l%+ 6%,m%)  , ch(32),~
               at (13,32), fac(hex(8c)), partdescr$(l%+ 7%,m%)  , ch(32),~
               at (14,32), fac(hex(8c)), partdescr$(l%+ 8%,m%)  , ch(32),~
               at (15,32), fac(hex(8c)), partdescr$(l%+ 9%,m%)  , ch(32),~
               at (16,32), fac(hex(8c)), partdescr$(l%+10%,m%)  , ch(32),~
               at (17,32), fac(hex(8c)), partdescr$(l%+11%,m%)  , ch(32),~
               at (18,32), fac(hex(8c)), partdescr$(l%+12%,m%)  , ch(32),~
               at (19,32), fac(hex(8c)), partdescr$(l%+13%,m%)  , ch(32),~
                                                                         ~
               at (07,65), fac(hex(8c)), status$      (l%+ 1%)  , ch(01),~
               at (08,65), fac(hex(8c)), status$      (l%+ 2%)  , ch(01),~
               at (09,65), fac(hex(8c)), status$      (l%+ 3%)  , ch(01),~
               at (10,65), fac(hex(8c)), status$      (l%+ 4%)  , ch(01),~
               at (11,65), fac(hex(8c)), status$      (l%+ 5%)  , ch(01),~
               at (12,65), fac(hex(8c)), status$      (l%+ 6%)  , ch(01),~
               at (13,65), fac(hex(8c)), status$      (l%+ 7%)  , ch(01),~
               at (14,65), fac(hex(8c)), status$      (l%+ 8%)  , ch(01),~
               at (15,65), fac(hex(8c)), status$      (l%+ 9%)  , ch(01),~
               at (16,65), fac(hex(8c)), status$      (l%+10%)  , ch(01),~
               at (17,65), fac(hex(8c)), status$      (l%+11%)  , ch(01),~
               at (18,65), fac(hex(8c)), status$      (l%+12%)  , ch(01),~
               at (19,65), fac(hex(8c)), status$      (l%+13%)  , ch(01),~
                                                                         ~
               at (07,70), fac(hex(8c)), str$         (l%+ 1%)  , ch(03),~
               at (08,70), fac(hex(8c)), str$         (l%+ 2%)  , ch(03),~
               at (09,70), fac(hex(8c)), str$         (l%+ 3%)  , ch(03),~
               at (10,70), fac(hex(8c)), str$         (l%+ 4%)  , ch(03),~
               at (11,70), fac(hex(8c)), str$         (l%+ 5%)  , ch(03),~
               at (12,70), fac(hex(8c)), str$         (l%+ 6%)  , ch(03),~
               at (13,70), fac(hex(8c)), str$         (l%+ 7%)  , ch(03),~
               at (14,70), fac(hex(8c)), str$         (l%+ 8%)  , ch(03),~
               at (15,70), fac(hex(8c)), str$         (l%+ 9%)  , ch(03),~
               at (16,70), fac(hex(8c)), str$         (l%+10%)  , ch(03),~
               at (17,70), fac(hex(8c)), str$         (l%+11%)  , ch(03),~
               at (18,70), fac(hex(8c)), str$         (l%+12%)  , ch(03),~
               at (19,70), fac(hex(8c)), str$         (l%+13%)  , ch(03),~
                                                                         ~
               at (07,74), fac(hex(8c)), lot$         (l%+ 1%)  , ch(06),~
               at (08,74), fac(hex(8c)), lot$         (l%+ 2%)  , ch(06),~
               at (09,74), fac(hex(8c)), lot$         (l%+ 3%)  , ch(06),~
               at (10,74), fac(hex(8c)), lot$         (l%+ 4%)  , ch(06),~
               at (11,74), fac(hex(8c)), lot$         (l%+ 5%)  , ch(06),~
               at (12,74), fac(hex(8c)), lot$         (l%+ 6%)  , ch(06),~
               at (13,74), fac(hex(8c)), lot$         (l%+ 7%)  , ch(06),~
               at (14,74), fac(hex(8c)), lot$         (l%+ 8%)  , ch(06),~
               at (15,74), fac(hex(8c)), lot$         (l%+ 9%)  , ch(06),~
               at (16,74), fac(hex(8c)), lot$         (l%+10%)  , ch(06),~
               at (17,74), fac(hex(8c)), lot$         (l%+11%)  , ch(06),~
               at (18,74), fac(hex(8c)), lot$         (l%+12%)  , ch(06),~
               at (19,74), fac(hex(8c)), lot$         (l%+13%)  , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 9% then L47530
                gosub disp_detls
                goto L46100

L47530:        if keyhit% <> 13 then L47550
                  call "MANUAL" (pgmname$)
                  goto L47000

L47550:        if keyhit% <> 15 then L47570
                  call "PRNTSCRN"
                  goto L47000

L47570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *               S C R E E N   P A G E   5                   *~
            *-----------------------------------------------------------*~
            * Display Details Screen                                    *~
            *************************************************************

        disp_detls
            pfmsg1$ = "(1)Start Over                                     ~
        ~             (13)Instructions"
            pfmsg2$ = "(2/3)First/Last         (6/7)Up/Down              ~
        ~             (15)Print Screen"
            pfmsg3$ = "(4/5)Prev/Next                                    ~
        ~             (16)Exit Display"
            pfkey$  = hex(0102030405060700000d0e0f10)

            line2$ = " "
            line2$ = "Vendor: " & vencode$(v%)
            str(line2$,19) = "P.O.Number: " & ponumber$(v%)
            str(line2$,46) = "From Line:"
            put str(line2$,57,2) using L48180, l% + 1%
            str(line2$,62%) = pgmname$ & ": " & str(cms2v$,,8%)
L48180:         FMT PIC(##)
            init(hex(86)) lfac$():str(pfmsg3$,63,1) = hex(84)
            inpmessage$ = "Press PF16 to Return to Receipt Processing"


L48230:     accept                                                       ~
               at (01,02),                                               ~
                  "Display Purchase Order Line Info:      Line Item Infor~
        ~mation",                                                         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), fac(hex(a4)), headsc$(5)             , ch(79),~
                                                                         ~
               at (07,02), fac(lfac$( 1)), seqnr$(l%+ 1%)       , ch(03),~
               at (08,02), fac(lfac$( 2)), seqnr$(l%+ 2%)       , ch(03),~
               at (09,02), fac(lfac$( 3)), seqnr$(l%+ 3%)       , ch(03),~
               at (10,02), fac(lfac$( 4)), seqnr$(l%+ 4%)       , ch(03),~
               at (11,02), fac(lfac$( 5)), seqnr$(l%+ 5%)       , ch(03),~
               at (12,02), fac(lfac$( 6)), seqnr$(l%+ 6%)       , ch(03),~
               at (13,02), fac(lfac$( 7)), seqnr$(l%+ 7%)       , ch(03),~
               at (14,02), fac(lfac$( 8)), seqnr$(l%+ 8%)       , ch(03),~
               at (15,02), fac(lfac$( 9)), seqnr$(l%+ 9%)       , ch(03),~
               at (16,02), fac(lfac$(10)), seqnr$(l%+10%)       , ch(03),~
               at (17,02), fac(lfac$(11)), seqnr$(l%+11%)       , ch(03),~
               at (18,02), fac(lfac$(12)), seqnr$(l%+12%)       , ch(03),~
               at (19,02), fac(lfac$(13)), seqnr$(l%+13%)       , ch(03),~
                                                                         ~
               at (07,06), fac(hex(84)), reqstd$(l%+ 1%)        , ch(20),~
               at (08,06), fac(hex(84)), reqstd$(l%+ 2%)        , ch(20),~
               at (09,06), fac(hex(84)), reqstd$(l%+ 3%)        , ch(20),~
               at (10,06), fac(hex(84)), reqstd$(l%+ 4%)        , ch(20),~
               at (11,06), fac(hex(84)), reqstd$(l%+ 5%)        , ch(20),~
               at (12,06), fac(hex(84)), reqstd$(l%+ 6%)        , ch(20),~
               at (13,06), fac(hex(84)), reqstd$(l%+ 7%)        , ch(20),~
               at (14,06), fac(hex(84)), reqstd$(l%+ 8%)        , ch(20),~
               at (15,06), fac(hex(84)), reqstd$(l%+ 9%)        , ch(20),~
               at (16,06), fac(hex(84)), reqstd$(l%+10%)        , ch(20),~
               at (17,06), fac(hex(84)), reqstd$(l%+11%)        , ch(20),~
               at (18,06), fac(hex(84)), reqstd$(l%+12%)        , ch(20),~
               at (19,06), fac(hex(84)), reqstd$(l%+13%)        , ch(20),~
                                                                         ~
               at (07,28), fac(hex(8c)),  delvr$   (l%+ 1%)     , ch(20),~
               at (08,28), fac(hex(8c)),  delvr$   (l%+ 2%)     , ch(20),~
               at (09,28), fac(hex(8c)),  delvr$   (l%+ 3%)     , ch(20),~
               at (10,28), fac(hex(8c)),  delvr$   (l%+ 4%)     , ch(20),~
               at (11,28), fac(hex(8c)),  delvr$   (l%+ 5%)     , ch(20),~
               at (12,28), fac(hex(8c)),  delvr$   (l%+ 6%)     , ch(20),~
               at (13,28), fac(hex(8c)),  delvr$   (l%+ 7%)     , ch(20),~
               at (14,28), fac(hex(8c)),  delvr$   (l%+ 8%)     , ch(20),~
               at (15,28), fac(hex(8c)),  delvr$   (l%+ 9%)     , ch(20),~
               at (16,28), fac(hex(8c)),  delvr$   (l%+10%)     , ch(20),~
               at (17,28), fac(hex(8c)),  delvr$   (l%+11%)     , ch(20),~
               at (18,28), fac(hex(8c)),  delvr$   (l%+12%)     , ch(20),~
               at (19,28), fac(hex(8c)),  delvr$   (l%+13%)     , ch(20),~
                                                                         ~
               at (07,50), fac(hex(8c)),   job$       (l%+ 1%)  , ch(08),~
               at (08,50), fac(hex(8c)),   job$       (l%+ 2%)  , ch(08),~
               at (09,50), fac(hex(8c)),   job$       (l%+ 3%)  , ch(08),~
               at (10,50), fac(hex(8c)),   job$       (l%+ 4%)  , ch(08),~
               at (11,50), fac(hex(8c)),   job$       (l%+ 5%)  , ch(08),~
               at (12,50), fac(hex(8c)),   job$       (l%+ 6%)  , ch(08),~
               at (13,50), fac(hex(8c)),   job$       (l%+ 7%)  , ch(08),~
               at (14,50), fac(hex(8c)),   job$       (l%+ 8%)  , ch(08),~
               at (15,50), fac(hex(8c)),   job$       (l%+ 9%)  , ch(08),~
               at (16,50), fac(hex(8c)),   job$       (l%+10%)  , ch(08),~
               at (17,50), fac(hex(8c)),   job$       (l%+11%)  , ch(08),~
               at (18,50), fac(hex(8c)),   job$       (l%+12%)  , ch(08),~
               at (19,50), fac(hex(8c)),   job$       (l%+13%)  , ch(08),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg1$                , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg2$                , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg3$                , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13% then L49020
                  call "MANUAL" (pgmname$)
                  goto L48230

L49020:        if keyhit%  >  7% then L49086
                  if keyhit% = 2% then l% = 0%
                  if keyhit% = 3% then l% = maxlines% - 13%
                  if keyhit% = 4% then l% = l% - 12%
                  if keyhit% = 5% then l% = l% + 12%
                  if keyhit% = 6% then l% = l% - 1%
                  if keyhit% = 7% then l% = l% + 1%
                  l% = max(0%, min(87%, l%, maxlines% - 1%))
                  goto disp_detls

L49086:        if keyhit% <> 15 then L49090
                  call "PRNTSCRN"
                  goto L48230

L49090:        if keyhit% <> 16% then L48230
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50200,         /* Receiver Number  */~
                                    L50300,         /* Bill of Lading   */~
                                    L50400,         /* Carrier Vencode  */~
                                    L50500,         /* Carrier Descr    */~
                                    L50550,         /* Def. Recvd date  */~
                                    L50600,         /* Def. Recvd Mode  */~
                                    L50650,         /* Time of Receipt  */~
                                    L50700,         /* Comments         */~
                                    L50800,         /* Carrier Freight  */~
                                    L50900          /* Car Frieght Acct */
                  return

L50200:     REM Test Data for Receiver Number
                if keyhit% <> 8% then L50245
                   call "GETCODE" (#30, rcvrno$, " ", 0%, 0, f1%)
                      if f1% <> 0% then L50260
L50235:                  errormsg$ = "No Receiver Selected."
                         return
L50245:         if keyhit% <> 14% then L50260
                   hdr$() = " "
                   incl_excl(1%) = -1.03
                   incl_excl$(1%) = hex(000000)
                   call "PLOWCODE" (#35, rcvrno$, " ", 0%, 0, f1%,       ~
                              hdr$(), 0, 0, incl_excl(), incl_excl$())
                      if f1% = 0% then L50235
L50260:         f1%(30), f1%(35), new_recvr% = 0%
                gosub check_hold_record : if errormsg$ <> " " then return
                gosub set_hold_record
                call "READ100" (#35, rcvrno$, f1%(35))
                   if f1%(35) <> 0% then dataload
                call "READ100" (#30, rcvrno$, f1%(30))
                   if f1%(30) <> 0% then dataload
                new_recvr% = 1%
                return

L50300:     REM Test Data for Bill of Lading
                return

L50400:     REM Test Data for Carrier Vendor Code
                if carvencode$ <> " " then L50430
L50410:            carvencode$, carfacct$, carpacct$, carfrieght$ = " "
                   carfacctdescr$ = " "
                   carfrieght = 0
                   return
L50430:         if carvencode$ = "?" then carvencode$ = " "
                f1%(3) = 0%
                call "GETCODE"(#3,carvencode$,cardescr$,0%,1.3,f1%(3))
                   if f1%(3) = 0% then L50410
                   return

L50500:     REM Test Data for Carrier Description
                return

L50550:     REM Test Data for Default Received Date
                call "DATEOK" (defrcvddate$, f1%(1), errormsg$)
                if errormsg$ <> " " then return
                rcvddate$ = defrcvddate$
                call "DATUNFMT" (rcvddate$)
                call "WHICHMON" (#2, rcvddate$, err%)
                   if err% = 0% then errormsg$ =                         ~
                   "Date is not open for postings."
                return

L50600:     REM Test Data for Default Received Mode
                if pos("IQRD" = ohpost$) <> 0% then return
                   errormsg$ = "Mode Must Be 'I', 'Q', 'R', or 'D'"
                   return

L50650:     REM Test Data for Time
                return

L50700:     REM Test Data for Comments
                return

L50800:     REM Test Data for Carrier Freight
                if carfrieght$ <> " " then L50840
                   carfrieght = 0
                   return
L50840:         call "NUMTEST" (carfrieght$, -9e7, 9e7, errormsg$, -2.2, ~
                                                             carfrieght)
                return

L50900:     REM Test Data for Carrier Freight Acct
                if carfacct$ <> " " then L50930
                if carfrieght$ = " " then return
L50930:         call "GETCODE" (#16,carfacct$,carfacctdescr$,0%,0,f1%(16))
                   if f1%(16) <> 0% then return
                errormsg$ = "Invalid Account Selection"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51200,         /* VENDOR NUMBER    */~
                                    L51400,         /* PONUMBER         */~
                                    L51700          /* VENDOR PACK SLIP */
                     return

L51200:     REM Test data for vendor number
                if vencode$(v%)=" " and ponumber$(v%)<>" " then L51400
                call "GETCODE"(#3,vencode$(v%),venname$(v%),0%,1.3,f1%(3))
                      if f1%(3) <> 0 then L51260
                      errormsg$ = "Vendor Not on File: " & vencode$(v%)
                      return
L51260:         if ponumber$(v%)=" " then return

L51400:     REM Test data for purchase order number
              if ponumber$(v%) = "?" then ponumber$(v%) = " "
              if ponumber$(v%) = " " then L51465
                 call "REDALT0" (#4, ponumber$(v%), 1%, f1%(4))
                    if f1%(4) = 0% then L51465
                      get #4 using L51430, tempven$
L51430:                   FMT CH(9)
                      if vencode$(v%) = " " then vencode$(v%) = tempven$
                      if vencode$(v%) = tempven$ then L51465
              errormsg$="PO:" & ponumber$(v%) & " Assigned to Vendor:" & ~
                                                                 tempven$
              return

L51465:     init (" ") oldreadkey$
              if vencode$(v%) <> " " then                                ~
                 oldreadkey$ = str(vencode$(v%),1,9) &                   ~
                                           str(ponumber$(v%),1,16)
              call "GETCODE" (#4,oldreadkey$,venname$(v%),0%,0,f1%(4))
                 if f1%(4) <> 0 then L51505
                    errormsg$ = "P/O MUST BE ON FILE"
                    return
L51505:          vencode$(v%)  = str(oldreadkey$,1,9)
                 ponumber$(v%) = str(oldreadkey$,10,16)

              call "REDALT0" (#13, ponumber$(v%), 3%, f1%(13))
                  if f1%(13) = 0% then L51550
                      errormsg$ = "This P.O. is Being Modified in P.O. In~
        ~put - No Access is Allowed"
                      return

L51550:       if v% = 1% then L51595
              for i% = 1% to min(maxpo% + 1%, pomax%)
                  if i% = v% then L51585
                  if vencode$(i%) <> vencode$(v%) then L51585
                  if ponumber$(i%) <> ponumber$(v%) then L51585
                     errormsg$ = "This P.O. Is Already On This Receiver"
                     return
L51585:       next i%

L51595:       fieldnr% = 2%
              return

L51700: REM Test data for vendor pack slip
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52120,         /* TAB STOP ????    */~
                                    L52200,         /* RECEIPT MODE     */~
                                    L52400,         /* QTY RECEIVED     */~
                                    L53000          /* QTY OPEN         */
L52120:           return

L52200: REM Test Data for MODE
            if ohpost$(c%) <> " " then L52230
               if abs(qtyrecd(c%)) < .001 then return
L52230:     call "SERENABL" (part$(c%,1%), temp%, temp1%, #2, #6)
               if temp% <> 0% then L52290
            if new_recvr% = 1% then L52272
            call "HNYAVAIL" (#6, #22, part$(c%,1%), " ", " ", errormsg$, ~
                             9e9, temp, return%)
               if errormsg$ <> " " then L52285
L52272:     if lot_sys$ = "Y" and lottrack$(c%) = "Y"                    ~
                                  then L52290 else L52320
L52285:        errormsg$ = " "
L52290:     if ohpost$(c%) = "D" then return
               errormsg$ = "Mode Must Be 'D'."
               return
L52320:     if pos("IQRD" = ohpost$(c%)) <> 0% then return
               errormsg$ = "Mode Must Be 'I', 'Q', 'R', or 'D'."
               return

L52400: REM Validation for rec this shpmnt
            if qtyrecd$(c%,2) = " " then qtyrecd$(c%,2) = qtyonord$(c%,2)
            convert qtyrecd$(c%,2) to temp, data goto L52470
            temp = round(temp, 7)
            if abs(temp - venrecd) < .0000001 then L52560
               temp = round(temp*factor(c%), 4)
               goto L52590
L52470:     errormsg$ = "Invalid Entry for Vendor Received Quantity"
            return

L52500:     qtyonord(c%) = max(0,round((qtyonord(c%)+qtyrecd(c%)-temp),4))
            qtyrecd(c%) = temp
L52520:     gosub'040
            gosub L52860
            goto L53320

L52560:     if qtyrecd$(c%,1) = " " then qtyrecd$(c%,1) = qtyonord$(c%,1)
            convert qtyrecd$(c%,1) to temp, data goto L52670
            temp = round(temp, 4)
L52590:     if abs(temp - qtyrecd(c%)) < .0001 then L52520
               overwarn% = 0%
               if temp < 0 then L52760
               if temp < qtyqchold(c%) then L52810
               if abs(temp) < .0001 then L52500
               if (temp + qtyrecdtd(c%)) - max_to_receive(c%) < .0001    ~
                    then goto L52500
               goto L52700
L52670:     errormsg$ = "Invalid Entry for Internal Received Quantity"
            return

L52700: REM Hard overreceipt error
            temp1 = max_to_receive(c%)
            temp1 = round(temp1 - qtyrecdtd(c%), 4)
            errormsg$ = osmsg$ & " Max Receipt is"
            goto L53900

L52760: REM Reversals are boo-boos, they play havoc with distribution
            errormsg$ = "Reversal Not Allowed.  Adjust Original Receiver ~
        ~or Use Manual Withdrawal."
            return

L52810: REM Some of this stuff is on qc-hold
            temp1 = qtyqchold(c%)
            errormsg$ = "Because of Hold Status, Minimum Receipt is"
            goto L53900

L52860: REM Set warning message for overreceipt, if needed
            if overwarn% = 0% then L52910
               overwarn% = 0%
               return

L52910:     temp1 = round((qtyrecd(c%) + qtyrecdtd(c%)) - qtyorig(c%), 4)
                temp1 = temp1 - qtyrtv(c%)
            if temp1 < .0001 then return

            errormsg$ = "WARNING:" & hex(8c)
            errormsg$ = errormsg$ &                                      ~
                             "This Receipt Indicates an Overshipment of"
            overwarn% = 1%
            return clear
            goto L53900

L53000: REM Validation for rem on order
            convert qtyonord$(c%,2) to temp, data goto L53070
            temp = round(temp, 7)
            if abs(temp - venonord) < .0000001 then L53140
               if temp < 0 then L53070
               temp = round(temp*factor(c%), 4)
               goto L53160
L53070:     errormsg$ = "Invalid Entry for Vendor's Open Quantity"
            return

L53100:     qtyonord(c%) = temp
L53110:     gosub'040
            goto L53320

L53140:     convert qtyonord$(c%,1) to temp, data goto L53230
            temp = round(temp, 4)
L53160:     if abs(temp - qtyonord(c%)) < .0001 then L53110
               overwarn% = 0%
               if abs(temp) < .0001 then L53100
               if temp < 0 then L53230
               if (temp + qtyrecd(c%) + qtyrecdtd(c%)) -                 ~
                    max_to_receive(c%) < .0001 then goto L53100
               goto L53260
L53230:     errormsg$ = "Invalid Entry For Internal Order Quantity"
            return

L53260: REM Hard error, resultant total shipment too large
            temp1 = max_to_receive(c%)
            temp1 = round(temp1 - (qtyrecd(c%)+qtyrecdtd(c%)), 4)
            errormsg$ = osmsg$ & " Max On Order is"
            goto L53900

L53320: REM Set warning and play with fieldnr for onorder, if needed
            /* WARNING - WARNING - WARNING */
            /* This code is common to both quantity validation routines */
            if overwarn% = 0% then L53390
               overwarn% = 0%
               return

L53390:     temp1=max(0,round(qtyorig(c%)-(qtyrecd(c%)+qtyrecdtd(c%)),4))
               if (qtyonord(c%) - temp1) < .0001 then return
            errormsg$ = "WARNING:" & hex(8c)
            errormsg$ = errormsg$  &                                     ~
                        "Quantity On Order Exceeds Derived Quantity of"
            overwarn% = 1%
            fieldnr% =  4%
            goto L53900

L53900: REM Common setup errormsgs involving numbers (caller sets TEMP1)
            temp2 = round(temp1/factor(c%), 7)
            call "CONVERT" (temp2, -2.4,                                 ~
                               str(errormsg$,len(errormsg$)+2%,10))
            errormsg$ = errormsg$ & "/"
            call "CONVERT" (temp1, -2.4,                                 ~
                               str(errormsg$,len(errormsg$)+1%,10))
            return

        REM *************************************************************~
            * ASSIGN NEXT RECEIVER NUMBER FOR THIS GUY                  *~
            *************************************************************
        assign_rcvr_number

            call "READ101" (#19, userstr$, f1%(19))
                if f1%(19) = 0% then store_error
            get #19, using L59080, nextrcvr$
L59080:         FMT POS(194), CH(7)
            nextrcvr% = 0%:convert nextrcvr$ to nextrcvr%, data goto L59100
L59100:     if nextrcvr% < 1% then nextrcvr% = 1%
            if nextrcvr% > 9999999% then nextrcvr% = 1%
            convert nextrcvr% to str(nextrcvr$,5), pic(0000000)
            str(nextrcvr$,1,4) = str(userstr$,,3) & "-"

            call "READ100" (#30, nextrcvr$, used%)
                if used% <> 0% then L59190
            call "READ100" (#35, nextrcvr$, used%)
                if used% = 0% then L59220
L59190:     nextrcvr% = nextrcvr% + 1%
            goto L59100

L59220:     rcvrno$ = nextrcvr$
            nextrcvr% = nextrcvr% + 1%
            if nextrcvr% > 9999999% then nextrcvr% = 1%
            convert nextrcvr% to nextrcvr$, pic(0000000)
            put #19, using L59080, nextrcvr$
            rewrite #19
            gosub set_hold_record /* Better late than never */
            return

        posting_window_error
*          You are here because the Default Date Received is outside of
*          your posting window.  Inventory & G/L can not post correctly.
*          Or that you have posted in a previous period (still postable).

            if err% <> 0% then L59670

            u3% = 2%
            call "ASKUSER" (u3%, "*** POSTING WINDOW ERROR ***",         ~
                "The Date Received (Post Date) is no longer within your"&~
                " Posting Window.", "To post 'as is', Module Dates and "&~
                " Current Period Open need adjusting.", "Press PF 1 to "&~
                " Startover -or- Press RETURN to use your A/P Date.")
            if u3% <> 1% then L59630
                return clear all
                gosub delete_hold_record
                goto inputmode
L59630:     if u3% <> 0% then posting_window_error
                defrcvddate$ = paydate$
                return

L59670:     u3% = 2%
            call "ASKUSER" (u3%, "*** POSTING WINDOW WARNING ***",       ~
                "Goods were Posted during the Previous Period (still op"&~
                "en).", "Press PF1 to Startover -or- Press PF16 to use "&~
                "Original Date", "-or- Press RETURN to use your A/P Date")
            if u3% <> 1% then L59750
                return clear all
                gosub delete_hold_record
                goto inputmode
L59750:     if u3% <> 0% then L59780
                defrcvddate$ = paydate$
                return
L59780:     if u3% <> 16% then L59670
                return

        REM *************************************************************~
            * Section to set ASKUSER Messages and BAIL OUT!!            *~
            *************************************************************

        user_error

            askpf1$ = "You are not listed as a valid user for this data b~
        ~ase."

            goto askuser_call

        posting_date_error

            askpf1$ = "Your Posting Date is Invalid."

            goto askuser_call

        months_open_error

            askpf1$ = "Unable to find PIP base date."

            goto askuser_call

        store_error

            askpf1$ = "Unable to locate Store Record for defaults."

            goto askuser_call

        askuser_call

            askmid$ = " "
            askpf2$ = "Press RETURN or any function key to exit this prog~
        ~ram."
            askhdr$ = "* * * S O R R Y * * *"
            askkey% = 0%

            call "ASKUSER" (askkey%, askhdr$, askpf1$, askmid$, askpf2$)

            goto exit_program

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
        if prtd% = 1% then L65090
        if prtopt$ = "1" then call "RCVTKTSB"(fill%, #5,#6,#37,#22,#35,  ~
                                                              #36,#31, #3)
L65090:     call "SHOSTAT" ("One Moment Please")
            init (hex(00)) oldreadkey$
            str(oldreadkey$,,3) = userid$
            call "PLOWALTS" (#35, oldreadkey$, 1%, 3%, f1%(35))
        exit_program_a
            call "FILEBGON" (#41)
            call "FILEBGON" (#50)
            call "FILEBGON" (#51)
            call "FILEBGON" (#52)


            end f1%(35)
