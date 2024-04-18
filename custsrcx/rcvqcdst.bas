        REM *************************************************************~
            *                                                           *~
            *  RRRR    CCC   V   V   QQQ    CCC   DDDD    SSS   TTTTT   *~
            *  R   R  C   C  V   V  Q   Q  C   C  D   D  S        T     *~
            *  RRRR   C      V   V  Q   Q  C      D   D   SSS     T     *~
            *  R   R  C   C   V V   Q Q Q  C   C  D   D      S    T     *~
            *  R   R   CCC     V     QQQ    CCC   DDDD    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVQCDST - DRIVER FOR RCVDIST.  DISTRIBUTES GOODS IN QC.  *~
            *            DRIVER SELECTS RCVLINES RECORD TO ACT UPON,    *~
            *            SUBROUTINE DOES THE DIRTY WORK, THEN THE       *~
            *            DRIVER HAS THE JOB OF DOING THE DATA SAVE.     *~
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
            * 05/06/86 ! ORIGINAL                                 ! KAB *~
            * 02/09/87 ! Lot Track Enhancements                   ! ERN *~
            * 05/19/87 ! Standard Costing Enhancements            ! ERN *~
            * 10/09/87 ! Fixed SELECT Statement for JBVALUE2      ! DAW *~
            * 06/02/88 ! Receiver Tickets and New RCVDIST         ! MDE *~
            * 06/02/88 ! Changed Plowcode Logic For Part Number   ! MDE *~
            * 06/10/88 ! Changed Plowcode Logic For receiver      ! MDE *~
            * 02/02/89 ! Removed restrictions on access, now is   ! MJB *~
            *          !  accessible if in RCVLINES               !     *~
            * 06/01/89 ! Fixed display to include part description! MLJ *~
            * 06/06/90 ! Fixed PRR 10845.  Corrected the Part     ! SID *~
            *          !  Description Display.                    !     *~
            * 08/29/90 ! G/L Export file distribution.            ! RAC *~
            * 05/06/91 ! PRR 11668 Perform double PLOWCODE for    ! SID *~
            *          !  selecting a Part Number.                !     *~
            * 05/06/91 ! Enhancements  In QC/QC HLD Added PF9/PF10!     *~
            *          !               In RCV HLD Added PF10      !     *~
            * 06/26/91 ! Added PF24 access to HNYLCSUB permitting ! MLJ *~
            *          !  Location Management.                    !     *~
            * 12/23/91 ! Changed the 2nd PLOWCODE screen from the ! SID *~
            *          !   part number field to display RECEIVER #!     *~
            *          !   /PO # instead of PART #/DESCRIPTION    !     *~
            * 01/07/92 ! Major Functions OverHaul on 1st Screen.  ! SID *~
            *          !   and added a call to 'ALLFREE'          !     *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 12/08/92 ! PRR 12702  Captured Vendor properly.     ! JDH *~
            * 03/18/93 ! PRR 12820  Honors ticket size option.    ! JDH *~
            * 03/26/93 ! PLOW for PF9/10 starts at part if entered! JDH *~
            * 12/14/93 ! Installed Rcvr hold logic ala RCVINPUT.  ! JDH *~
            * 03/31/94 ! Test buffers for posting in process.     ! KAB *~
            * 03/06/95 ! Added Channel #35 to call of RCVUPSUB.   ! JBK *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            aid$1,                       /* GETPARM Aid Byte           */~
            askhdr$40,                   /* Askuser Text               */~
            askpf1$80,                   /* Askuser Text               */~
            askmid$80,                   /* Askuser Text               */~
            askpf2$80,                   /* Askuser Text               */~
            delvr$(1)20,                 /* Deliver To                 */~
            diskkey$100,                 /* Vbklines Key$              */~
            date$8,                      /* System Date For Display    */~
            descr$30,                    /* Screen Title Message Prompt*/~
            descr_m(12),                 /* Descr Map For PlowCode     */~
            cursor%(2),                  /* Cursor Location            */~
            dummy%(1), dummy$(1)1,       /* To fill a space            */~
            dummy2$(2,2)2,               /*                            */~
            errormsg$79,                 /* Error Message Text         */~
            header$79,                   /* Screen Header              */~
            header$(1)79,                                                ~
            heder$(3)79,                 /* For PLOWCODE Call          */~
            hnydate$8,                   /* Inventory Posting Date     */~
            i$(24)80,                    /* Screen Image (Not Used)    */~
            inc(2),                      /* Plowcode Arguments of some */~
            inc$(2)16,                   /*   sort known only to LDJ   */~
            lastpart$25,                 /* Last Part Managed          */~
            lines$(100)50,               /* Line Item Array            */~
            lines$200,                   /* Line Item For Write        */~
            lfac$(20)1,                  /* FACs For Input             */~
            miscplow$99,                 /* PLOWKEY for Rcvr Hold      */~
            mode$3,                      /* Mode of Running            */~
            mode$(2)30,                  /* Mode of Running Message    */~
            message$79,                  /* Istructions For Input      */~
            part$25,                     /* Part Number                */~
            partdescr$34,                /* Part Description           */~
            pfdescr$(3)79,               /* PF Text For Display        */~
            pfkeys$16,                   /* PF Actual values           */~
            poline$3,                    /* Purchase Order Line Number */~
            pono$16,                     /* Purchase Order Number      */~
            prtopt$1,                                                    ~
            psno$16,                     /* Packing Slip Number        */~
            reqstd$(1)20,                /* Who Requested the Items    */~
            textid$4,                    /* Q.C. Free Text             */~
            trankey$100,                 /* Ser. Trans. Key            */~
            rcvdate$8,                   /* Date Of Receipt            */~
            rcvno$16,                    /* Receiver Number            */~
            readkey$100,                 /* Work Variable              */~
            readkey1$100,                /* Work Variable              */~
            record$(16)50,               /* Work Variable              */~
            subh$(3)80,                  /* Subroutine Headers         */~
            vendor$9,                    /* Vendor Number              */~
            oprntamt$(7)10,              /* Printable Amounts          */~
            orecvtd$10,                  /* Old Received To Date       */~
            ostuom$10,                   /* Old Store UOM              */~
            chk_freight$15,              /* Check freight              */~
            partdesc$32,                 /* PART DESCRIPTION           */~
            binloc$8,                    /* BIN                        */~
            scpbinloc$8,                 /* Scrap Bin Location         */~
            plowkey$100,                 /* Disk key for plow routines */~
            ponumber$16,                 /* Purchase Order Number      */~
            prntamt$(7)10,               /* Printable Amounts          */~
            recvtd$10,                   /* Received To Date           */~
            rejcode$6,                   /* Rejected Code              */~
            rcvkey$100,                  /* Receiver Number            */~
            stuom$4,                     /* Inven. Unit Of Measure     */~
            measr$10,                    /* Store Unit Of Measure      */~
            lottrk$1,                    /* LOT TRACKING FLAG          */~
            moq$10,                      /* Mininmum Order Quantity    */~
            newqty(7),                   /* New quantities             */~
            oldqty(7),                   /* Old quantities             */~
            pad$10,                      /* Pad w/ Spaces              */~
            qhand$10,                    /* QUANTITY ON HAND           */~
            qhandscp$10,                  /* Amount In Scrap           */~
            qtyin$10,                    /* QTY TO INDIVIDUAL STORE-LOT*/~
            ojobno$8,                    /* OLD JOBNO                  */~
            orejcode$6,                  /* OLD REJECT CODE            */~
            jobno$8,                     /*                            */~
            qtyrmain$10,oqtyrmain$10,    /* Qtyremain open             */~
            storeno$3,                   /* Store number               */~
            ostoreno$3,                  /* Old Store                  */~
            storlot$6,                   /* LOT                        */~
            ostorlot$6,                  /* OLD LOT                    */~
            scpstore$3,                  /* SCRAP STORE                */~
            oscpstore$3,                 /* OLD SCRAP STORE            */~
            scplot$6,                    /* SCRAP LOT                  */~
            oscplot$6,                   /* OLD SCRAP LOT              */~
            userid$3,                    /* Userid of this user        */~
            venpart$25,                  /* Vendor Part Number         */~
            varhead$25,                  /* Variable Header            */~
            recever$16,                  /* Receiver                   */~
            porcvtd$10,                  /* RECEIVED TO DATE (P.O.)    */~
            ovenuom$4,                   /* Vendor Unit of Measure     */~
            venuom$4,                    /* " "                        */~
            ticket$13,                   /* Ticket title               */~
            vencode$9,                   /* Vendor code                */~
            modno$2, jnlid$3             /* Pur Job Module/Journal     */~

        dim                              /* HNYHOLD RECORD AREAS       */~
            bufflot$(101)6,              /*                            */~
            buffstr$(101)3,              /*                            */~
            buffqty (101),               /*                            */~
            buffidx%(101),               /*                            */~
                                                                         ~
            postlot$(100)6,              /*                            */~
            poststr$(100)3,              /*                            */~
            postqty (100),               /*                            */~
            postidx%(100),               /*                            */~
                                                                         ~
            savemark$(100)1              /*                            */~

        dim f1%(64%),                    /* RECORD-ON-FILE FLAGS       */~
            f2%(64%),                                                    ~
            fs%(64%),                                                    ~
            rslt$(64)20

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch finalization of R6.04.00  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYQUAN  ! Inventory Quantities File                *~
            * # 2 ! HNYDETAL ! Inventory Detail File                    *~
            * # 3 ! SYSFILE2 ! CAELUS MANAGEMENT SYSTEM INFORMATION     *~
            * # 6 ! HNYMASTR ! Inventory Master File                    *~
            * # 9 ! STORNAME ! Store Info File - Name/Address           *~
            * #10 ! TXTFILE  ! System Text File                         *~
            * #11 ! VBKLINES ! P.O. Line Item Master File               *~
            * #12 ! QCRTYPES ! QC Rejection Codes                       *~
            * #13 ! HNYLOCNS ! Location Quantity Detail File (HNYLCSUB) *~
            * #14 ! LOCATION ! Location Master File (HNYLCSUB)          *~
            * #15 ! USERINFO ! USERS DEFAULT INFORMATION FILE           *~
            * #16 ! HNYPOOL  ! Inventory LIFO/FIFO Pool Records File    *~
            * #18 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #19 ! PIPIN    ! Planned Inventory Additions File         *~
            * #20 ! PIPOUT   ! Planned Inventory Uses File              *~
            * #21 ! SFCUM2   ! Cumulative Sales Forecast File           *~
            * #22 ! JBMASTR2 ! Production Job Master File               *~
            * #23 ! JOBMASTR ! Wip/JC Job Master File                   *~
            * #24 ! JBVALUE2 ! Value Added Detail File                  *~
            * #25 ! JOBPURCH ! Job Purchases File                       *~
            * #26 ! GLMAIN   ! General Ledger Master File               *~
            * #27 ! GLDETAIL ! General Ledger Transaction File          *~
            * #28 ! HNYPROC  ! Inventory Procurement History File       *~
            * #29 ! RCVJRNTF !                                          *~
            * #31 ! RCVLINES ! Receiver Line Items                      *~
            * #32 ! RCVHNYDS ! Receiver Inventory Distribution          *~
            * #33 ! PAYLINES ! Payables Line Item File                  *~
            * #34 ! RCVHNYRP !                                          *~
            * #35 ! RCVTIF   ! Receiver Master File TIF                 *~
            * #36 ! RCVTIF2  ! Receiver Line Items  TIF                 *~
            * #37 ! RCVHNYTF ! Receiver Inventory Distribution TIF      *~
            * #38 ! RCVQCREJ !                                          *~
            * #40 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #41 ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #42 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #50 ! SERMSAVE ! Serial Number Tracking Restore File      *~
            * #51 ! SERWKHLD ! TEMPORARY SERIAL #'S WORK FILE (RCVDIST) *~
            * #52 ! SERMHOLD ! SERIAL NUMBER MASTER FILE COPY (RCVDIST) *~
            * #53 ! HNYPOOL  ! HNYPOOL                                  *~
            * #54 ! VBKMASTR ! VENDOR BACK LOG PO'S                     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select  #1, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #2, "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select  #3, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select # 6, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 9, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   3                      ~

            select #10, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #11, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 28

            select #12, "QCRTYPES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos  = 1, keylen = 6

            select #13,  "HNYLOCNS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 443, keylen = 42,     ~
                                   key 2, keypos = 485, keylen = 42,     ~
                                   key 3, keypos = 527, keylen = 42,     ~
                                   key 4, keypos = 590, keylen = 42      ~

            select #14,  "LOCATION",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 11,                        ~
                         alternate key 1, keypos = 4, keylen = 11        ~

            select  #15, "USERINFO",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1,  keylen = 3

            select #16,  "HNYPOOL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #18,  "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #19,  "PIPIN",                                        ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 60,                                  ~
                          keypos =30, keylen = 19,                       ~
                          alt key 1, keypos = 01, keylen = 48

            select #20,  "PIPOUT",                                       ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 64,                                  ~
                          keypos = 1, keylen = 56,                       ~
                          alt key 1, keypos = 20, keylen = 37

            select #21,  "SFCUM2",                                       ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 1985,                                ~
                          keypos = 1, keylen = 25

            select #22, "JBMASTR2",                                      ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize =  1300,                               ~
                          keypos = 1, keylen = 8

            select #23,  "JOBMASTR",                                     ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize =  700,                                ~
                          keypos = 1, keylen = 8

            select #24,  "JBVALUE2",                                     ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 300,                                 ~
                          keypos = 1, keylen = 23

            select #25,  "JOBPURCH",                                     ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 200,                                 ~
                          keypos = 1, keylen = 16

            select #26, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #27, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  26                      ~

            select #28, "HNYPROC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  134,                                  ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =    7, keylen =  65,         ~
                            key  2, keypos =    1, keylen =  40, dup,    ~
                            key  3, keypos =   41, keylen =  31, dup     ~

            select #29, "RCVJRNTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 670,                                  ~
                         keypos = 10, keylen = 10                        ~

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

            select #33, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #34, "RCVHNYRP",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos= 29, keylen = 55,                         ~
                        alt key 1, keypos =  1, keylen = 83              ~

            select #35, "RCVTIF",                                        ~
                        varc, indexed, recsize = 150,                    ~
                        keypos= 12, keylen = 16,                         ~
                        alt key 1, keypos = 1, keylen = 11

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

            select #38, "RCVQCREJ",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 26, keylen = 17,                         ~
                        alt key 1, keypos = 1, keylen = 42               ~

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

            select #54, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #55, "DUMMY",                                         ~
                        varc,     indexed,  recsize  =  5,               ~
                        keypos =  1,  keylen =  5

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1%), f2%( 1%), 0%, rslt$( 1%))
            call "OPENCHCK" (# 2, fs%( 2%), f2%( 2%), 0%, rslt$( 2%))
            call "OPENCHCK" (# 3, fs%( 3%), f2%( 3%), 0%, rslt$( 3%))
            call "OPENCHCK" (# 6, fs%( 6%), f2%( 6%), 0%, rslt$( 6%))
            call "OPENCHCK" (# 9, fs%( 9%), f2%( 9%), 0%, rslt$( 9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%), 0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%), 0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%), 0%, rslt$(16%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%), 0%, rslt$(19%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%), 0%, rslt$(20%))
            call "OPENCHCK" (#21, fs%(21%), f2%(21%), 0%, rslt$(21%))
            call "OPENCHCK" (#22, fs%(22%), f2%(22%), 0%, rslt$(22%))
            call "OPENCHCK" (#23, fs%(23%), f2%(23%), 0%, rslt$(23%))
            call "OPENCHCK" (#26, fs%(26%), f2%(26%), 0%, rslt$(26%))
            call "OPENCHCK" (#27, fs%(27%), f2%(27%), 0%, rslt$(27%))
            call "OPENCHCK" (#28, fs%(28%), f2%(28%), 0%, rslt$(28%))
            call "OPENCHCK" (#29, fs%(29%), f2%(29%), 300%, rslt$(29%))
            call "OPENCHCK" (#31, fs%(31%), f2%(31%), 300%, rslt$(31%))
            call "OPENCHCK" (#32, fs%(32%), f2%(32%), 300%, rslt$(32%))
            call "OPENCHCK" (#33, fs%(33%), f2%(33%), 0%, rslt$(33%))
            call "OPENCHCK" (#34, fs%(34%), f2%(34%), 300%, rslt$(34%))
            call "OPENCHCK" (#35, fs%(35%), f2%(35%), 100%, rslt$(35%))
            call "OPENCHCK" (#36, fs%(36%), f2%(36%), 300%, rslt$(36%))
            call "OPENCHCK" (#37, fs%(37%), f2%(37%), 300%, rslt$(37%))
            call "OPENCHCK" (#38, fs%(38%), f2%(38%), 100%, rslt$(38%))
            call "OPENCHCK" (#40, fs%(40%), f2%(40%), 0%, rslt$(40%))
            call "OPENCHCK" (#41, fs%(41%), f2%(41%), 0%, rslt$(41%))
            call "OPENCHCK" (#42, fs%(42%), f2%(42%), 0%, rslt$(42%))
            call "OPENCHCK" (#50, fs%(50%), f2%(50%), 0%, rslt$(50%))
            call "OPENCHCK" (#51, fs%(51%), f2%(51%), 0%, rslt$(51%))
            call "OPENCHCK" (#52, fs%(52%), f2%(52%), 0%, rslt$(52%))
            call "OPENCHCK" (#54, fs%(54%), f2%(54%), 0%, rslt$(54%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)

            init (hex(00)) readkey$ : str(readkey$,1%,3%) = userid$
            call "PLOWNEXT" (#29, readkey$, 3%, f1%(29%))
               if f1%(29%) = 0% then L09062
            askpf1$ = "You have unposted Receiver - G/L transactions"
            goto L09072

L09062:     init (hex(00)) readkey$ : str(readkey$,1%,3%) = userid$
            call "PLOWNEXT" (#34, readkey$, 3%, f1%(34%))
               if f1%(34%)  = 0% then L09088
            askpf1$ = "You have unprocessed Receiver - Inventory records"

L09072:     f1%(29%) = 0% : f1%(34%) = 0% : askkey% = 0%
            askmid$ = " "
            askpf2$ = "Press RETURN or any function key to exit this prog~
        ~ram."
            askhdr$ = "* * * S O R R Y * * *"
            call "ASKUSER" (askkey%, askhdr$, askpf1$, askmid$, askpf2$)
            goto exit_program_a

L09088:     init (hex(00)) readkey$ : str(readkey$,1%,3%) = userid$
            call "PLOWALTS" (#35, readkey$, 1%, 3%, f1%(35%))
               if f1%(35%)  = 0% then L09200

L09096:     f1%(29%) = 0% : f1%(34%) = 0% : askkey% = 0%
            askpf1$ = "You have unposted Receiver Update records"
            askmid$ = "You must be sure no task is actively updating"
            askpf2$ = "Press PF32 to continue.  Press RETURN to exit."
            askhdr$ = "* * * S O R R Y * * *"
            call "ASKUSER" (askkey%, askhdr$, askpf1$, askmid$, askpf2$)
            if askkey% = 32% then L09200
            if askkey% =  0% then exit_program_a
               goto L09096

L09200:     mode$(1) = "in Receiver Hold"
            mode$(2) = "in Q. C."

            REM Scope out the mode we're in via a 'hidden' GETPARM
            call "GETPARM"                                               ~
                addr ("ID", "S", "RCVQCDST", aid$, "0001", "MODE  ", 0%, ~
                        "K", "MODE    ", mode$,       3%, 5%, 32%, "C")

            if mode$ <> "RCV" then mode$ = "QC"
            if mode$ = "QC" then mode% = 1% else mode% = 0%

            REM RETRIEVE INVENTORY DATE FOR THIS USER
                call "READ100" (#15, userid$, f1%(15))
                      if f1%(15) = 0 then user_error
                get #15, using L09370, hnydate$
L09370:                 FMT XX(9), CH(6)
                call "WHICHMON" (#3, hnydate$, whichmonth%)
                     if whichmonth% < 1 or whichmonth% > 3 then date_error

            prtopt$ = "0"
            call "VBKSWTCH" ("RCVRPRNT", prtopt$, temp, temp%)
            call "VBKSWTCH" ("TKTSIZE ", temp$, fil, temp%)
                if fil = 0 then fil = 6  /* Default Tkt Size is 3-1/2" */
                fill% = fil : temp$ = " "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
                 call "JBINUSE" (" ", 1%)
                 reprint% = 0%
                 mat newqty = zer : mat  oldqty = zer
                                 storlot$, scplot$,                      ~
            storeno$,  message$, part$, partdescr$, rcvno$, poline$,     ~
            rcvdate$, pono$, textid$, psno$, vendor$, savemark$() = " "
            cancel% = 0%
            mat inc = zer
            init (" ")  inc$()

            mat buffidx% = zer
            mat postidx% = zer
            buffmax%, postmax%, highfield%, maxlines% = 0%
            call "TXTFUTIL" (#10, 0%, "INTL", hex(ffffffff))
            call "ALLFREE"

            for fieldnr% = 1% to 5%
L10180:         gosub'051(fieldnr%)
                   if enabled% = 0% then L10450
L10200:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10262
                         fieldnr% = max(1%, fieldnr%-1%)
                         gosub'051(fieldnr%)
                         goto L10200
L10262:               if fieldnr% <> 1% or                               ~
                                     (keyhit% <> 9% and keyhit% <> 10%)  ~
                                                   then L10272
                         gosub list_all_parts
                         if errormsg$ <> " " then L10200
L10272:               if fieldnr% <> 2% or keyhit% <> 9% then L10290
                         gosub list_all_rec_qty
                         if errormsg$ <> " " then L10200
L10290:               if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10180  /* 10200 */
L10450:         gosub'151(fieldnr%)
                      if cancel% = 1% then L10200
                      if errormsg$ <> " " then L10200
                      highfield% = max(fieldnr%, highfield%)
            next fieldnr%
        pjob_inuse_retry
            errormsg$ = " "

            gosub setup_sub_pass

               if return% = 0% then L19000
                  call "SERSTOVR" (0%, "7", "2", #42, #41)
                  gosub delete_hold_record
                  goto inputmode

        REM *************************************************************~
            * Purchase Job In Use Intercept                             *~
            *************************************************************
        pjob_inuse

L11150:         gosub'202
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L11150
                goto pjob_inuse_retry

L19000: REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        REM DATASAVE

            REM NOW GO SAVE...
                gosub L31000
                gosub delete_hold_record
                goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  message$ = " "
                  on fieldnr% gosub L20160,         /* Part Number      */~
                                    L20200,         /* Receiver Number  */~
                                    L20320,         /* Vendor Number    */~
                                    L20440,         /* Purchase Order # */~
                                    L20560          /* P.O. Line Number */
                     return

L20160:     REM DEFAULT/ENABLE FOR PART NUMBER
                message$ = "Enter Part Number To Process.  Leave Blank An~
        ~d Press (RETURN) to Search File."
                return

L20200:     REM DEFAULT/ENABLE FOR RECEIVER NUMBER
                errormsg$ = " "
                message$ = "Enter Receiver Number.  Leave Blank And Press~
        ~ (RETURN) to Search File."
            return

L20320:     REM DEFAULT/ENABLE FOR VENDOR NUMBER
                errormsg$ = " "
                readkey$ = str(part$) & str(rcvno$)
                call "PLOWALTS" (#31, readkey$, 1%, 41%, f1%(31))
                     if f1%(31) = 0% then L20410
                str(vendor$) = str(readkey$,42,9)
L20410:         enabled% = 0%
                message$ = "Enter Vendor Code."
            return

L20440:     REM DEFAULT/ENABLE FOR PURCHASE ORDER NUMBER
                errormsg$ = " "
                message$ = "Enter Purchase Order Number."
            return

L20560:     REM DEFAULT/ENABLE FOR PURCHASE ORDER LINE NUMBER
                errormsg$ = " "
                message$ = "Enter Purchase Order Line Number."
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            errormsg$ = " "
            k% = 2%
            call "STARTOVR" (k%)
            on k%+1% goto L29942, L29948
            return

L29942:        REM START OVER            (ENTER)
                   return clear
                   gosub delete_hold_record
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return

L30000: REM *************************************************************~
            *             R E C A L L   O L D   D A T A                 *~
            *                                                           *~
            * LOADS EXISTING DATA FROM FILE                             *~
            *************************************************************

            init (hex(00)) readkey$
            call "DELETE" (#50, readkey$, 0%)

            readkey$ = str(part$) & str(rcvno$) & str(vendor$) &         ~
                       str(pono$) & str(poline$)

            REM Look in master...
            call "REDALT0" (#31, readkey$, 1%, f1%(31))
                if f1%(31) <> 0 then file% = 31

            REM Look in buffer (aka TIF)...
            call "REDALT0" (#36, readkey$, 1%, f1%(36))
                if f1%(36) <> 0 then file% = 36

            get #file%, using L30180, str(record$())
L30180:         FMT CH(800)

            get record$() using L30195, oldqty(),ostuom,ovenuom$,orecd,   ~
                          ostoreno$, ostorlot$, oscpstore$, oscplot$,    ~
                          orejcode$, oqtyrmain, ojobno$

L30195:         FMT POS(156), 7*PD(14,4),POS(236), PD(14,4),CH(4),       ~
                    POS(264), PD(14,4), POS(332), CH(3), CH(6), CH(3),   ~
                    CH(6), CH(6), PD(14,4), POS(400), CH(8)

            if ojobno$ = " " then L30320
               call "READ100" (#18, part$, testjob%)
               if testjob% = 0% then L30320
               testjob% = 3%                  /* Check, but don't lock */
               call "JBINUSE" (ojobno$, testjob%)
               if testjob% = 0% then L30320
                  return clear all
                  goto pjob_inuse

L30320:     maxlines% = 0
            if file% = 36% then buffmax% = 1%
            readkey$ = str(rcvno$) & str(vendor$) & str(pono$) &         ~
                                                str(poline$) & str(part$)
L30350:     call "PLOWNEXT" (#file%+1%, readkey$, 69%, f1%(file%+1))
                if f1%(file%+1) = 0 then L30700
            maxlines%, c% = maxlines% + 1
            get #file%+1, using L30410, str(lines$(c%),1,9),              ~
                                       str(lines$(c%),10,8),             ~
                                       str(lines$(c%),18,32)
L30410:         FMT POS(70),CH(9),POS(87),CH(8),POS(119),CH(32)

            temp% = val(str(lines$(c%),42,3),3)
            if temp% > 2000000% then L30540
            if temp% = 0% then L30540
            trankey$ = str(rcvno$) & str(pono$) & str(poline$) &         ~
                                                  str(lines$(c%),42,3)
            temp1% = 0%:convert poline$ to temp1%, data goto L30490
L30490:     temp% = 10000%*temp1% + temp%
            call "SERLOAD" (temp%, "PO", trankey$, 100%, " ",            ~
                  str(lines$(c%),1,9), #3, #40, #42, #41, temp1%, #50)
            str(lines$(c%),45,4) = bin(temp1%,4)

L30540:     if file% = 31% then L30640

            buffmax% = buffmax% + 1%
            get #37 using L30620, buffstr$(buffmax%),                     ~
                        bufflot$(buffmax%), buffqty(buffmax%)
            buffidx%(buffmax%) = 1%
            goto L30350

L30620:     FMT POS(70), CH(3), CH(6), POS(87), PD(14,4)

L30640:     postmax% = postmax% + 1%
            get #32 using L30620, poststr$(postmax%),                     ~
                        postlot$(postmax%), postqty(postmax%)
            postidx%(postmax%) = 1%
            goto L30350

L30700:     if file% = 31% then L30900

            readkey$ = str(rcvno$) & str(vendor$) & str(pono$) &         ~
                                                str(poline$) & str(part$)
L30740:     call "PLOWNEXT" (#32%, readkey$, 69%, f1%(32))
                if f1%(32) = 0 then L30900
            postmax% = postmax% + 1%
            get #32 using L30620, poststr$(postmax%),                     ~
                        postlot$(postmax%), postqty(postmax%)
            postidx%(postmax%) = 1%
            goto L30740

L30900:     str(textid$,,4) = str(record$(),4%*mode% + 388%,4%)
            call "TXTFUTIL" (#10, 0%, "LOAD", textid$)

           diskkey$ = all(hex(00))
           diskkey$ = str(vendor$) & str(pono$) & str(poline$)

                call "READ100" (#11, diskkey$, f1%(11%))
                     if f1%(11%) = 0 then L30925
                get #11 using L30924,  delvr$(1%), reqstd$(1%)
L30924:         FMT POS(313), CH(20),CH(20)
L30925:
            return
L31000: REM *************************************************************~
            *              S A V E   N E W   D A T A                    *~
            *                                                           *~
            * SAVES DATA ENTERED ON FILE                                *~
            *************************************************************

            call "SHOSTAT" ("Updating Files, One Moment Please.")

            lastpart$ = part$
            str(record$(),356,8) = str(record$(),156,8)
            str(record$(),364,24) = str(record$(),212,24)

            readkey$ = str(record$(),26,52)
            call "READ101" (#36, readkey$, f1%(36))
                if f1%(36) <> 0 then delete #36
            write #36, using L31080, str(record$())
L31080:         FMT CH(800)

            str(textid$,,4) = str(record$(),4%*mode% + 388%,4%)
            call "TXTFUTIL" (#10, 0%, "SAVE", textid$)

            readkey$ = str(rcvno$) & str(vendor$) & str(pono$) &         ~
                                                str(poline$) & str(part$)
            call "DELETE" (#37, readkey$, 69%)
            if maxlines% = 0 then L31455
            for c% = 1 to maxlines%
                lines$ = " "
                str(lines$,  1,44) = str(record$(), 26,44)
                str(lines$, 45,25) = str(record$(),  1,25)
                str(lines$, 70, 9) = str(lines$(c%), 1, 9)
                str(lines$, 79, 8) = all(hex(00))
                str(lines$, 87, 8) = str(lines$(c%),10, 8)
                str(lines$, 95,24) = str(record$(),364, 8) &             ~
                                     str(record$(),512, 8) &  " "
                str(lines$,119,32) = str(lines$(c%),18,32)
L31165:         call "GETDTTM" addr(str(lines$,80,7))
                write #37, using L31175, str(lines$,1,200), eod goto L31165
L31175:               FMT CH(200)

                temp% = val(str(lines$(c%),42,3),3)
                if temp% > 2000000% then L31235
                if temp% = 0% then L31235
                trankey$ = str(rcvno$) & str(pono$) & str(poline$) &     ~
                                                  str(lines$(c%),42,3)
                temp1% = 0%:convert poline$ to temp1%, data goto L31215
L31215:         temp% = 10000%*temp1% + temp%
                call "SERSAVE" (temp%, "PO", trankey$, 100%, part$,      ~
                               userid$, "2", "7", 0%, #3, #40%, #42, #41)

L31235:     if savemark$(c%) <> " " then L31445
               held, need, workqty, buffqty, postqty = 0

            for j% = c% to maxlines%
                if str(lines$(j%),1,9) <> str(lines$(c%),1,9)            ~
                                                  then L31285
                     get str(lines$(j%),10,8) using L31270, temp
L31270:                  FMT PD(14,4)
                     workqty = workqty + temp
                     savemark$(j%) = "*"
L31285:     next j%

            if postmax% = 0% then L31350
            for j% = 1% to postmax%
                if postidx%(j%) = 0% then L31340
                if str(poststr$(j%),,3) <> str(lines$(c%),,3)            ~
                                                 then L31340
                if str(postlot$(j%),,6) <> str(lines$(c%),4,6)           ~
                                                 then L31340
                   postqty = postqty + postqty(j%)
                   postidx%(j%) = 0%
L31340:     next j%

L31350:     if buffmax% = 0% then L31410
            for j% = 1% to buffmax%
                if buffidx%(j%) = 0% then L31395
                if str(buffstr$(j%),,3) <> str(lines$(c%),,3)            ~
                                                 then L31395
                if str(bufflot$(j%),,6) <> str(lines$(c%),4,6)           ~
                                                 then L31395
                   buffqty = buffqty + buffqty(j%)
                   buffidx%(j%) = 0%
L31395:     next j%
            held = max(0, postqty - buffqty)

L31410:     need = max(0, postqty - workqty)

            holdadj = need - held
            if holdadj = 0 then L31445
            call "HNYHOLD" (#1, part$, str(lines$(c%),,3),               ~
                            str(lines$(c%),4,6), holdadj, return%)

L31445:     next c%


L31455:     if prtopt$ = "0" then L31460
            select printer(134) : call "SETPRNT" ("RCV007", " ", 0%, 0%)
            gosub prt_rcv_tkt
            close printer       : call "SETPRNT" ("RCV007", " ", 0%, 1%)

L31460:     readkey$ = str(record$(),26,44)
            call "RCVUPSUB" (readkey$, 44%, hnydate$,                    ~
                        modno$, jnlid$, pstseq%,                         ~
                        #1,              /* HNYQUAN  */                  ~
                        #2,              /* HNYDETAL */                  ~
                        #3,              /* SYSFILE2 */                  ~
                        #6,              /* HNYMASTR */                  ~
                       #11,              /* VBKLINES */                  ~
                       #15,              /* USERINFO */                  ~
                       #16,              /* HNYPOOL  */                  ~
                       #18,              /* PIPMASTR */                  ~
                       #19,              /* PIPIN    */                  ~
                       #20,              /* PIPOUT   */                  ~
                       #21,              /* SFCUM2   */                  ~
                       #22,              /* JBMASTR2 */                  ~
                       #23,              /* JOBMASTR */                  ~
                       #24,              /* JBVALUE2 */                  ~
                       #25,              /* JOBPURCH */                  ~
                       #26,              /* GLMAIN   */                  ~
                       #27,              /* GLDETAIL */                  ~
                       #28,              /* HNYPROC  */                  ~
                       #29,              /* RCVJRNTF */                  ~
                       #31,              /* RCVLINES */                  ~
                       #32,              /* RCVHNYDS */                  ~
                       #33,              /* PAYLINES */                  ~
                       #34,              /* RCVHNYRP */                  ~
                       #35,              /* RCVTIF   */                  ~
                       #36,              /* RCVTIF2  */                  ~
                       #37,              /* RCVHNYTF */                  ~
                       #38,              /* RCVQCREJ */                  ~
                       #10,              /* TXTFILE  */                  ~
                       #40,              /* SERTIF   */                  ~
                       #42)              /* SERMASTR */                  ~

            return

         set_hold_record
            if rcvno$ = " " then return
L31710:     call "GETDTTM" addr(str(datetime$,2%,7%))
            str(datetime$,1%,1%) = hex(00)
            put #35 using L31755, hex(000000), datetime$, datetime$,      ~
                                 rcvno$, " "
                /* This is a temperary hold record that does not match */
                /* the normal file layout.  DATETIME$ is used in the   */
                /* Receiver field to eliminate duplicate keys.  HEX(00)*/
                /* is used so RCVTKTSB and RCVUPDTE don't try to       */
                /* process this record.  'Nuff said.                   */
L31755:         FMT CH(3), CH(8), CH(16), CH(16), CH(107)
            write #35, eod goto L31710
            recvr_held% = 1%
            return

         delete_hold_record
            if rcvno$ = " " or recvr_held% = 0% then return
            init (hex(00)) miscplow$
L31790:     call "PLOWAL1" (#35, miscplow$, 1%, 3%, f1%(35%)) /*RCVTIF*/
            if f1%(35%) = 0% then return /* Uh Oh, didn't find one */
                get #35 using L31805, temp_rcvrno$
L31805:              FMT POS(28), CH(16)
                if temp_rcvrno$ <> rcvno$ then L31790 /* Loop */
                     delete #35
                     recvr_held% = 0%
                     return

        check_hold_record
            if rcvno$ = " " then return
            init (hex(00)) miscplow$
L31845:     call "PLOWALTS" (#35, miscplow$, 1%, 3%, f1%(35%)) /*RCVTIF*/
            if f1%(35%) = 0% then return /* Good, didn't find one */
                get #35 using L31860, temp_rcvrno$
L31860:              FMT POS(28), CH(16)
                if temp_rcvrno$ <> rcvno$ then L31845 /* Loop */
                     errormsg$ = "Receiver already being processed."
                     return

        REM *************************************************************~
            * SET HEADER LINES FOR PASS TO SUBROUTINE                   *~
            *************************************************************~

        setup_sub_pass

            gosub L30000

            subh$(1) = str(i$(5),2)
            subh$(2) = str(i$(6),2)
            subh$(3) = str(i$(7),2)

        REM *** Now Set Up the Info not yet in place ***
          get record$() using L32140, rcvdate$, psno$
L32140:   FMT POS(122), CH(6), CH(16)
            call "DATEFMT" (rcvdate$)
            str(subh$(2),49,16) = psno$
            str(subh$(2),71,8)  = rcvdate$
            str(subh$(3),73,4)  = hex(8c) & poline$

            call "RCVDIST" (record$(), lines$(), maxlines%, subh$(),     ~
                       mode%, #9, #10, #12, #6, #3, #1,                  ~
                       #40, #41, #42, #51, #52,#16,#54,#11,#15,return%,  ~
                       dummy%(), dummy$(), 0%, dummy2$(), dummy$() , 0%, ~
                       bufflot$(), buffstr$(), buffqty(),                ~
                       postlot$(), poststr$(), postqty(),                ~
                       buffidx%(), postidx%(), buffmax%, postmax%,       ~
                       delvr$(1%),reqstd$(1%), #13, #14)



            return

        list_all_parts
            errormsg$ = " "
            mat descr_m = zer : mat inc = zer
            init(" ") inc$()
            if fieldnr% > 1% then return
               init(hex(00)) str(plowkey$,,100%)
               if part$ <> " " then str(plowkey$, 1%, 25%) = part$
               descr_m(01) =     1.25 : descr_m(02) = 0001.0
               descr_m(03) =    26.16 : descr_m(04) = 0027.0
               descr_m(05) =    51.16 : descr_m(06) = 0044.0
               descr_m(07) =    67.03 : descr_m(08) = 0062.0
               inc$(1) = hex(000000000000000f)

            if keyhit% = 10% then L36005  /* Go List Parts in QC Hold  */
               heder$(3%) = hex(84) & "Select Part Number" & " " &       ~
                            mode$(1% + mode%) & " To Manage"
             if mode% = 1% then                                          ~
               heder$(1%) = "  Part Number               Receiver     " &~
                            "    PO               Line           QC"     ~
                           else                                          ~
               heder$(1%) = "  Part Number               Receiver     " &~
                            "    PO               Line     Rcv Hold"
               descr_m(09) = (164.08+(8*mode%)) : descr_m(10) = 0070.0841
               inc(1) = -(164.08+(8*mode%))
               goto L36070  /* Go Plow RCVLINES */

L36005:     REM Setup Headings for List Parts in QC Hold
               heder$(3%) = hex(84) & "Select Part Number in QC Hold " & ~
                                      "to manage"
               heder$(1%) = "  Part Number               Receiver     " &~
                            "    PO               Line      QC Hold"
               descr_m(09) =  180.08  : descr_m(10) = 0070.0841
               inc(1) = -180.08

L36070:        call "PLOWCODE" (#31, plowkey$,"    "    , 9000%, 1.30,   ~
                f1%(31), heder$(),0,78,inc(),inc$(),"D","Y",#31,descr_m())
                if f1%(31) = 0% then return
                    goto partition_and_test

        list_all_rec_qty
            errormsg$ = " " : mat descr_m = zer : mat inc = zer
            init(" ") inc$(), plowkey$
            if fieldnr% <> 2% then return

               plowkey$ = part$

               descr_m(01) =    26.16 : descr_m(02) = 0001.0
               descr_m(03) =    51.16 : descr_m(04) = 0019.0
               descr_m(05) =    67.03 : descr_m(06) = 0038.0
               descr_m(07) =   164.08 : descr_m(08) = 0045.0841
               descr_m(09) =   172.08 : descr_m(10) = 0055.0841
               descr_m(11) =   180.08 : descr_m(12) = 0065.0841

               heder$(3%) = hex(84) & "Select a Receiver Number "        ~
                            & "for Part : " & part$ & " To Manage"
               heder$(1%) = "  Receiver          PO Number         Line"&~
                            "    Rcv Hold        QC   QC Hold"

               call "PLOWCODE" (#31, plowkey$,"    "    , 9025%, 1.30,   ~
                f1%(31), heder$(),0,78,inc(),inc$(),"D","Y",#31,descr_m())
                    if f1%(31) = 0% then return
            partition_and_test
                part$   = str(plowkey$, 1%,25%)
                call "DESCRIBE" (#6, part$, partdescr$, 0%, f1%(6%))
                rcvno$  = str(plowkey$,26%,16%)
                     fieldnr% = 2% : gosub'151(fieldnr%)
                     if errormsg$ = " " then L36880
                          vendor$, pono$, poline$ = " "
                          return
L36880:         vendor$ = str(plowkey$,42%, 9%)
                     fieldnr% = 3% : gosub'151(fieldnr%)
                     if errormsg$ = " " then L36890
                          pono$, poline$ = " "
                          return
L36890:         pono$   = str(plowkey$,51%,16%)
                     fieldnr% = 4% : gosub'151(fieldnr%)
                     if errormsg$ = " " then L36900
                          poline$ = " "
                          return
L36900:         poline$ = str(plowkey$,67%, 3%)
                fieldnr% = 5%
            return

        REM *************************************************************~
            *                  LOCATION MANAGEMENT                      *~
            *************************************************************

        locations

            call "HNYLCSUB" (part$,          /*  Part Number           */~
                             storeno$,       /*  Store Number          */~
                             storlot$,       /*  Lot Number            */~
                             0%,             /*  Quantity Unknown      */~
                             3%,             /*  Action = ADD          */~
                             #3,             /*  SYSFILE2              */~
                             #9,             /*  STORNAME              */~
                             #15,            /*  USERINFO              */~
                             #6,             /*  HNYMASTR              */~
                             #13,            /*  HNYLOCNS              */~
                             #1,             /*  HNYQUAN               */~
                             #14)            /*  LOCATION              */
            return


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(8c)) lfac$()
                header$ = " "
                pfdescr$(1) = "(1)Start Over     (4)Previous Field       ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~    (24)Locations    (16)Exit Program"
                pfkeys$ = hex(000104ffff0d0f1018)

                if mode% = 0% then L40171
                 if fieldnr% <> 1% then L40180
                   str(pfdescr$(2),19,20) = "( 9)List Parts in QC"
                   str(pfdescr$(3),19,25) = "(10)List Parts in QC Hold"
                   str(pfkeys$,4,1) = hex(09) : str(pfkeys$,5,1) = hex(0a)
                   goto L40180
L40171:          if fieldnr% <> 1% then L40180
                   str(pfdescr$(2),19,26) = "( 9)List Parts in Rcv Hold"
                   str(pfkeys$,4,1) = hex(09)

L40180:          if fieldnr% <> 2% then L40220
                   str(pfdescr$(2),19,26) = "(9)List All Receiver/Qty"
                   str(pfkeys$,4,1) = hex(09)

L40220:         REM Flip off appropriate fields...

                if fieldnr% > 1% then L40330
                str(pfdescr$(1),19,19) = " "
                str(pfkeys$,3,1) = hex(ff)
                if lastpart$ <> " " then header$ =                       ~
                                       "Last Part Managed: " & lastpart$
                goto L40350

L40330:         pfdescr$(3) = " "
                str(pfkeys$,6,1) = hex(ff)
L40350:         str(header$,62) = "RCVQCDST: " & str(cms2v$,,8)

                  str(pfdescr$(3),63,1) = hex(84)
                  on fieldnr% gosub L40820,         /* Part Number      */~
                                    L40820,         /* Receiver Number  */~
                                    L40820,         /* Vendor Number    */~
                                    L40820,         /* Purchase Order # */~
                                    L40820          /* P.O. Line Number */
                     goto L40920

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40820:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40920:     accept                                                       ~
               at (01,02), "Manage Disposition Of Goods",                ~
               at (01,30), fac(hex(8c)), mode$(1% + mode%)      , ch(16),~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   header$              , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (05,02), "Part Number",                                ~
               at (05,18), fac(lfac$(01)), part$                , ch(25),~
               at (05,44), fac(hex(8c)),   partdescr$           , ch(34),~
               at (06,02), "Receiver Number",                            ~
               at (06,18), fac(lfac$(02)), rcvno$               , ch(16),~
               at (06,35), "Pack Slip Nmbr",                             ~
               at (06,50), fac(hex(8c)),   psno$                , ch(16),~
               at (06,67), "Rcvd",                                       ~
               at (06,72), fac(hex(8c)),   rcvdate$             , ch(08),~
               at (07,02), "Vendor Code",                                ~
               at (07,18), fac(lfac$(03)), vendor$              , ch(09),~
               at (07,35), "Purchase Order",                             ~
               at (07,50), fac(lfac$(04)), pono$                , ch(16),~
               at (07,67), "PO Line",                                    ~
               at (07,75), fac(lfac$(05)), poline$              , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L41860
                call "MANUAL" ("RCVQCDST")
                goto L40920

L41860:        if keyhit% <> 15 then L41884
                  call "PRNTSCRN"
                  goto L40920

L41884:        if keyhit% <> 24 then L41900
                  gosub locations
                  goto L40920

L41900:        if fieldnr% <> 0% and fieldnr% <> 5% then return
               close ws
               call "SCREEN" addr ("C", k%, "I", i$(), cursor%())
               return

            deffn'202
                init(hex(84)) lfac$()

                pfdescr$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~                     (16)Exit Program"
                pfkeys$ = hex(0001ffff0d0f10)

                header$ = " "
                str(header$,62) = "RCVQCDST: " & str(cms2v$,,8)
                message$ = "Press RETURN to Retest for availability."
                errormsg$ = "Purchase Job is Currently In Use:"
                errormsg$ = errormsg$ & " " & ojobno$ & "."
                fieldnr% = 1%
                goto L40920

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  cancel% = 0%
                  errormsg$ = " "
                  on fieldnr% gosub L50150,         /* Part Number      */~
                                    L50350,         /* Receiver Number  */~
                                    L50470,         /* Vendor Number    */~
                                    L50590,         /* Purchase Order # */~
                                    L50710          /* P.O. Line Number */
                     return

L50150: REM TEST DATA FOR PART NUMBER
                if part$ <> "*CARRIER FREIGHT CHARGE*" then L50191
                   errormsg$ = "Sorry, Distribution of Freight Charges"  ~
                             & " is not allowed"
                   return
L50191:         readkey$ = str(part$)
                if part$ = "?" or part$ = " " then readkey$ = " "
                descr$ = hex(06) & "Select a Part Number"
                call "PLOWCODE" (#31, readkey$,descr$, -25%, -1.30,      ~
                                 f1%(31))
                if f1%(31) <> 0% then L50312
                   errormsg$ = "This Part Number does not exist."
                   return

L50312:         part$ = str(readkey$,,25)
                call "DESCRIBE" (#6, part$, partdescr$, 0%, f1%(6))
        return

L50350: REM TEST DATA FOR RECEIVER NUMBER
                mat inc = zer : mat descr_m = zer : init (" ")  inc$()
                readkey$ = str(part$) & str(rcvno$)
                if rcvno$ = "?" then str(readkey$,26) = " "
                call "PLOWALTS" (#31, readkey$, 1%, 41%, f1%(31))
                   if f1%(31) = 0% then L50377
                      gosub check_hold_record
                          if errormsg$ <> " " then return
                      gosub set_hold_record
                      get #31 using L50374, partdescr$
L50374:                             FMT    POS(78), CH(32)
                return

L50377:         heder$(3%) = hex(84) & "Select a Receiver Number for "   ~
                           & "Part : " & part$
                heder$(1%) = "  Receiver Number    Description"

                descr_m(01) = 26.16            : descr_m(02) = 0001.0
                descr_m(03) = 78.32            : descr_m(04) = 0020.0

                call "PLOWCODE" (#31, readkey$,"    "    ,  9025%, 1.30, ~
                      f1%(31), heder$(), 0, 78, inc(), inc$(), "D", "Y", ~
                      #31, descr_m())
                  if f1%(31) <> 0 then L50411
                     cancel% = 1%
                     part$ = str(readkey$,,25)
                     call "DESCRIBE" (#6, part$, partdescr$, 0%, f1%(6))
                  return

L50411:         part$ = str(readkey$,,25)
                call "DESCRIBE" (#6, part$, partdescr$, 0%, f1%(6))
                rcvno$  = str(readkey$,26,16)
                gosub check_hold_record : if errormsg$ <> " " then return
                gosub set_hold_record
                fieldnr% = 2%
        return

L50470: REM TEST DATA FOR VENDOR NUMBER
                readkey$ = str(part$) & str(rcvno$) & str(vendor$)
                if vendor$ = "?" then str(readkey$,42) = " "
                call "PLOWALTS" (#31, readkey$, 1%, 50%, f1%(31))
                   if f1%(31) <> 0% then return
                readkey1$ = hex(06) & "Select Vendor Off Receiver"
                readkey1$ = readkey1$ & " " & mode$(mode% + 1%)
                header$(1) = "  Vendor     Part Description"
                inc (1) = -(164.08 + (8*mode%))
                inc$(1) = hex(000000000000000f)
                call "PLOWCODE" (#31, readkey$, readkey1$, 5041%, 1.30,  ~
                                 f1%(31), header$(), 9, 0, inc(), inc$())
                     if f1%(31) <> 0 then L50560
                     errormsg$ = "Vendor Not On Receiver: " & vendor$
                     return
L50560:         vendor$ = str(readkey$,42)
                return

L50590:     REM TEST DATA FOR PURCHASE ORDER NUMBER
                readkey$=str(part$)&str(rcvno$)&str(vendor$)&str(pono$)
                if pono$ = "?" then str(readkey$,51) = " "
                call "PLOWALTS" (#31, readkey$, 1%, 66%, f1%(31))
                   if f1%(31) <> 0% then return

                heder$(3%) = hex(84) & "Select P.O. Off Receiver "       ~
                           & mode$(mode% + 1%)
                heder$(1%) = "  Vendor    Purchase Order     Line"
                descr_m(01) = 42.09            : descr_m(02) = 0001.0
                descr_m(03) = 51.16            : descr_m(04) = 0011.0
                descr_m(05) = 67.03            : descr_m(06) = 0031.0
                inc(1) = 26.16 : inc$(1) = rcvno$

                call "PLOWCODE" (#31, readkey$, readkey1$, 9025%, 1.30,  ~
                      f1%(31), heder$(), 0, 78, inc(), inc$(), "D", "Y", ~
                      #31, descr_m())
                     if f1%(31) <> 0 then L50678
                     errormsg$ = "Unknown Purchase Order: " & pono$
                     return

L50678:         vendor$ = str(readkey$,42,9)
                pono$   = str(readkey$,51,16)
                poline$ = str(readkey$,67,3)
                return

L50710:     REM TEST DATA FOR PURCHASE ORDER LINE NUMBER
                mat descr_m = zer : mat inc = zer : init(" ") inc$()
                readkey$=str(part$)&str(rcvno$)&str(vendor$)&str(pono$)
                if poline$ = "?" then poline$ = " "
                readkey$=str(part$)&str(rcvno$)&str(vendor$)&str(pono$)
                if poline$ = " " then L50731
                     call "NUMTEST" (poline$, 0, 999, " ", -0.001, 0)
L50731:         readkey$ = str(readkey$,,66) & poline$
                call "REDALT0" (#31, readkey$, 1%, f1%(31))
                   if f1%(31) <> 0% then return

                readkey1$ = hex(06) & "Select Line Item for PO : " & pono$
                header$(1) = " Line        Description"

                descr_m(01) =  67.03 : descr_m(02) =  0004.0
                descr_m(03) =  78.32 : descr_m(04) =  0009.0

                call "PLOWCODE" (#31, readkey$, readkey1$, 5066%, 1.30,  ~
                                 f1%(31), header$(), 0, 0, inc(), inc$())
                     if f1%(31) <> 0 then L50812
                     reprint% = 0%
                     errormsg$ = "Unknown P.O. Line Number: " & poline$
                     return
L50812:         poline$ = str(readkey$,67)
            reprint% = 1%

                return

        prt_rcv_tkt
            diskkey$ = all(hex(00))
            diskkey$ = part$
            str(diskkey$,26%) = str(rcvno$)
            str(diskkey$,42%) = str(vendor$)
            str(diskkey$,51%) = str(pono$)
            str(diskkey$,67%) = str(poline$)




           call "SHOSTAT" ("Printing Tickets For " & part$ )


            chk_freight$ = str(record$(),78,15)
            recever$ = rcvno$  /* Receiver No. */
            ponumber$ = pono$ /* PO NUMBER */
            vencode$ = vendor$ /* Vendor Code */

            get record$() using L51110, newqty(),stuom,venuom$,recvd
L51110:         FMT POS(156), 7*PD(14,4),POS(236), PD(14,4),CH(4),       ~
                POS(264), PD(14,4)

            get record$() using L51150, rejcode$, qtyrmain,jobno$
L51150:        FMT POS(350), CH(6), PD(14,4), POS(400), CH(8)

            currcv = newqty(2)
            currqc = newqty(3)
            mat newqty = newqty - oldqty
          /* READ VBKLINES FOR P.O. LINE INFORMATION */

            init (hex(00)) readkey$
            readkey$ = str(record$(),42,28)
            call "READ100" (#11,readkey$,f1%(11))
            if f1%(11) = 0% then return

            get #11, using L51520, partdesc$,porcvtd,venpart$

L51520:     FMT POS(57), CH(32),POS(101), PD(14,4), POS(197), CH(25)


            /* Hnymastr File */
            call "READ100" (#6, part$,f1%(6))
            if f1%(6) = 0% then varhead$ = "Non Stocked Part"
            if f1%(6) = 0% then L51640 /* Non Stocked Part */

            varhead$ = "Part"

            get #6, using L51630, stuom$, lottrk$, moq$
L51630:     FMT POS(74), CH(4), POS(130), CH(1),POS(190),CH(10)
L51640:
            gosub prnttkt

            rcvkey$ =  str(record$(),26,44) & str(record$(),,25)
            init (hex(00)) str(rcvkey$,70)
            orecd = 0
        nxthnytf
            call "PLOWNEXT" (#37,rcvkey$, 69%, f1%(37))
            if f1%(37) = 0% then return

            get #37 using L51730, storeno$, storlot$, qtyin
L51730:        FMT POS(70), CH(3), CH(6), POS(87), PD(14,4)
            readkey$ = all(hex(00))
               readkey$ = str(rcvkey$,,69%)
               str(readkey$,70%) = str(storeno$) & str(storlot$)
               init (hex(00)) str(readkey$,79%)

            orecd = 0
L51755:     call "PLOWNEXT" (#32, readkey$, 78%, f1%(32))
            if f1%(32) = 0% then L51785
            get #32 using L51770, anamt
L51770:     FMT POS(87), PD(14,4)
            orecd = orecd + anamt
            goto L51755
L51785:     /* GET ON HAND INVENTORY QTY FROM HNYQUAN */
            pad$ = "          "

         plowkey$ = str(part$,,25) & str(storeno$,,3) & str(storlot$,,6) ~
            & str(pad$,,10)
            call "READ100" (#1,plowkey$,f1%(1%))
            if f1%(1%) = 0% then L52000

            get #1, using L51860, binloc$, qhand
L51860:     FMT POS(61), CH(8), POS(69), PD(14,4)

            /* GET SCRAP QTY IN DEST. SCRAP STORE - LOT */

            if scpstore$ <> " " then chkscrap else L52000
        chkscrap
            plowkey$ = str(part$,,25) & str(scpstore$,,3)
            str(plowkey$,29) = str(scplot$,,6)
            str(plowkey$,35) = str(pad$,,10)
            call "READ100" (#1,plowkey$,f1%(1))
            if f1%(1) = 0% then L52000

            get #1 using L51990, scpbinloc$, qhandscp
L51990:     FMT POS(61), CH(8), POS(69), PD(14,4)
L52000:
            qtyin = qtyin - orecd


            call "CONVERT" (orecd,2.2,orecvtd$)
            call "CONVERT" (qtyin,2.2,qtyin$)


            gosub invtkt
            goto nxthnytf



          prnttkt
          init (" ") prntamt$(),oprntamt$(),recvtd$,qtyrmain$,orecvtd$,  ~
            oqtyrmain$,ostuom$,porcvtd$,qhand$,qhandscp$,qtyin$

            if reprint% = 0% then porcvtd = recvd


            for i% = 1% to 7%
            call "CONVERT" (newqty(i%),2.2,prntamt$(i%))
            next i%

            call "CONVERT" (recvd,2.2,recvtd$)
            call "CONVERT" (qtyrmain,2.2,qtyrmain$)
            call "CONVERT" (stuom,2.2,measr$)
            call "CONVERT" (qhand,2.2,qhand$)
            call "CONVERT" (qhandscp,2.2,qhandscp$)
            call "CONVERT" (qtyin,2.2,qtyin$)

            if reprint% <> 1% then L52390
            stat$ = "Re-print"
            for i% = 1% to 7%
            call "CONVERT" (oldqty(i%),2.2,oprntamt$(i%))
            next i%

            call "CONVERT" (orecd,2.2,orecvtd$)
            call "CONVERT" (oqtyrmain,2.2,oqtyrmain$)
            call "CONVERT" (ostuom,2.2,ostuom$)
            goto L52400

L52390:   stat$ = "Original"
L52400:   call "CONVERT" (porcvtd,2.2,porcvtd$)
          gosub rcvhtkt  /* Recvr Hold Ticket */
          gosub qctkt    /* QC Ticket */
          gosub qchldtkt /* QC HOLD TICKET */
          gosub rettkt  /* Vendor Ret Ticket */
          gosub rejtkt  /* Rejection Ticket */
          mat newqty = zer
          return


            rcvhtkt   /* Ticket for Items Moved To Receiver Hold */
            if currcv <> 0 then L52540
            return
L52540:     ticket$ = "Receiver Hold"
            call "CONVERT" (currcv,2.2,prntamt$(2))
            print using L53930
            print using L53950,ticket$,stat$,date$
            print using L53970
            print using L53990,varhead$,venpart$
            print using L54010,part$,partdesc$,prntamt$(2),porcvtd$
            print using L54030
            print using L54050
            print using L54070, ponumber$,recever$,jobno$,reqstd$(1%)
            print using L54090
            print using L54110
            print using L54130,storeno$,storlot$,binloc$,qhand$,delvr$(1%)
            print using L54150
            print using L54170
            print using L54260
            print using L54230
            print skip (fill%)
            currcv = 0
            prntamt$(2) = " "
            return


            qctkt   /* Ticket for Items Moved To Quality Control */
            if currqc <> 0 then L52780
            return
L52780:     ticket$ = "Q.C."
            call "CONVERT" (currqc,2.2,prntamt$(3))
            print using L53930
            print using L53950,ticket$,stat$,date$
            print using L53970
            print using L53990,varhead$,venpart$
            print using L54010,part$,partdesc$,prntamt$(3),oprntamt$(3)
            print using L54030
            print using L54050
            print using L54070, ponumber$,recever$,jobno$,reqstd$(1%)
            print using L54090
            print using L54110
            print using L54130,storeno$,storlot$,binloc$,qhand$,delvr$(1%)
            print using L54150
            print using L54210
            print using L54260
            print using L54230
            print skip (fill%)
            currqc = 0
            prntamt$(3) = " "

            return

            qchldtkt  /* Ticket for Items Moved To Quality Control  */
            if newqty(4) <> 0 then L53010
            return
L53010:     ticket$ = "Q.C. Hold"
            print using L53930
            print using L53950,ticket$,stat$,date$
            print using L53970
            print using L53990,varhead$,venpart$
            print using L54010,part$,partdesc$,prntamt$(4),oprntamt$(4)
            print using L54030
            print using L54050
            print using L54070, ponumber$,recever$,jobno$,reqstd$(1%)
            print using L54090
            print using L54110
            print using L54130,storeno$,storlot$,binloc$,qhand$,delvr$(1%)
            print using L54150
            print using L54210
            print using L54260
            print using L54230
            print skip (fill%)

            return

            rettkt   /* Ticket for Items RETURNED */
            if newqty(6) <> 0 then L53240
            return
L53240:     ticket$ = "Returned"
            print using L53930
            print using L53950,ticket$,stat$,date$
            print using L53970
            print using L53990,varhead$,venpart$
            print using L54010,part$,partdesc$,prntamt$(6),oprntamt$(6)
            print using L54030
            print using L54050
            print using L54070, ponumber$,recever$,jobno$,reqstd$(1%)
            print using L54090
            print using L54110
            print using L54130,storeno$,storlot$,binloc$,qhand$,delvr$(1%)
            print using L54150
            print using L54170
            print using L54260
            print using L54230
            print skip (fill%)

            return

            rejtkt   /* Ticket for Items TO REWORK OR SCRAP */
            if newqty(5) <> 0 then L53470
            return
L53470:     ticket$ = "Rejected"
            print using L53930
            print using L53950,ticket$,stat$,date$
            print using L53970
            print using L53990,varhead$,venpart$
            print using L54010,part$,partdesc$,prntamt$(5),oprntamt$(5)
            print using L54030
            print using L54050
            print using L54070, ponumber$,recever$,jobno$,reqstd$(1%)
            print using L54090
            print using L54110
            print using L54130, scpstore$, scplot$, scpbinloc$, qhandscp$,~
            delvr$(1%)
            print using L54150
            print using L54170
            print using L54190, rejcode$
            print using L54230
            print skip (fill%)

            return

            invtkt   /* Ticket for Items MOVED TO INVENTORY */
            if qtyin <> 0 then L53710
            return
L53710:     ticket$ = "Inventory"
            print using L53930
            print using L53950,ticket$,stat$,date$
            print using L53970
            print using L53990,varhead$,venpart$
            print using L54010,part$,partdesc$,qtyin$,orecvtd$
            print using L54030
            print using L54050
            print using L54070, ponumber$,recever$,jobno$,reqstd$(1%)
            print using L54090
            print using L54110
           print using L54130,storeno$,storlot$,binloc$,qhand$,delvr$(1%),~
            measr$
            print using L54150
            print using L54210
            print using L54260
            print using L54230
            print skip (fill%)

            return

        REM ******************  IMAGE STATEMENTS  ***********************
L53930: % +--------------------------------------------------------------~
        ~------------------+
L53950: % !         #############   RECEIVER DISTRIBUTION TICKET- #######~
        ~#  ########       !
L53970: % +--------------------------+-------------------------------+---~
        ~-------+----------+
L53990: % ! #########################!   #########################   !Qty~
        ~ Rec   !Prior Rec !
L54010: % ! #########################!###############################!###~
        ~#######!##########!
L54030: % +-----------------+----------------+--------+------------------~
        ~------------------!
L54050: % ! P.O. No.        !Receiver No.    !Job No. !Ordered By        ~
        ~                  !
L54070: % ! ################!################!########!##################~
        ~##                !
L54090: % +-----+-------+---------+---------------+----------------------~
        ~+-----------------!
L54110: % ! Store ! Lot   ! Bin     ! On Hand Qty ! Deliver To           ~
        ~!Stocking UOM     !
L54130: % ! ###   !###### !######## ! ##########  ! #################### ~
        ~!##########        !
L54150: % +-------+-------+---------+-------------+----------------------~
        ~------------------+
L54170: % ! Reason        !   Comments / Special Instructions            ~
        ~                  !
L54190: % ! ##########    !                                              ~
        ~                  !
L54210: % ! Special Instructions / Comments                              ~
        ~                  !
L54230: % +-------+-------+---------+-------------+----------------------~
        ~------------------+

L54260: % !                                                              ~
        ~                  !
        REM *************************************************************~
            * Section to set ASKUSER Messages and BAIL OUT!!            *~
            *************************************************************

        user_error

            askpf1$ = "You are not listed as a valid user for this data b~
        ~ase."

            goto askuser_call

        date_error

            askpf1$ = "Your Posting Date is Invalid."

            goto askuser_call

        askuser_call

            askmid$ = " "
            askpf2$ = "Press RETURN or any function key to exit this prog~
        ~ram."
            askhdr$ = "* * * S O R R Y * * *"
            askkey% = 0%

            call "ASKUSER" (askkey%, askhdr$, askpf1$, askmid$, askpf2$)

            goto exit_program

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************
        exit_program
            call "SHOSTAT" ("One Moment Please")

            if pstseq% <> 0% then call "JBJNLCLS" ("J2", userid$,        ~
                                       modno$, jnlid$, pstseq%, f1%(64))

            init (hex(00)) readkey$
            str(readkey$,,3) = str(userid$,,3)
            call "PLOWNEXT" (#29, readkey$, 3%, f1%(29))

            init (hex(00)) readkey$
            str(readkey$,,3) = str(userid$,,3)
            call "PLOWNEXT" (#34, readkey$, 3%, f1%(34))
        exit_program_a
            call "FILEBGON" (#41)
            call "FILEBGON" (#50)
            call "FILEBGON" (#51)
            call "FILEBGON" (#52)

            end (f1%(29) + f1%(34))
