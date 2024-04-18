        rem**************************************************************~
            *                                                           *~
            *  pppp    aaa   y   y  iiiii  n   n  pppp   u   u  ttttt   *~
            *  p   p  a   a  y   y    i    nn  n  p   p  u   u    t     *~
            *  pppp   aaaaa   yyy     i    n n n  pppp   u   u    t     *~
            *  p      a   a    y      i    n  nn  p      u   u    t     *~
            *  p      a   a    y    iiiii  n   n  p       uuu     t     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * payinput - payables input, enters invoices into buffer.   *~
            *            also links to receivers for automatic invoice  *~
            *            generation and reconciliation. program contains*~
            *            fairly extensive default & test data sections. *~
            *            note these sections assume the same receiver,  *~
            *            p.o. number, & p.o. line are allowed to occur  *~
            *            in the paybuf2 (invoice lines) file once at any*~
            *            given time, thus keeping paybuf2 out of the    *~
            *            picture (paylines data is considered in several*~
            *            places throughout the test/default sections.)  *~
            *            this program runs in three modes!.  (increased *~
            *            efficiency).  for offline, invoice is written  *~
            *            to buffer then immediately tossed - no gl or   *~
            *            inventory, but you do get 1099 and vender      *~
            *            purchase history updates.  for recurring mode, *~
            *            data goes straight to master, no update is     *~
            *            done (these are 'dummy invoices').             *~
            *                                                           *~
            * warning -- this program is now (12/23/87) split into two  *~
            *            portions -- this main/driver program and the   *~
            *            subroutine payinpts, which contains all screen *~
            *            accepts, datasave, and whatever else you may   *~
            *            decide to off-load there.                      *~
            *-----------------------------------------------------------*~
            *                  m o d i f i c a t i o n s                *~
            *---when---+------------------what--------------------+-who-*~
            * 04/29/80 ! original                                 ! bcw *~
            * 12/11/84 ! added oldseq number field                ! dsh *~
            * 07/09/85 ! final cleanup & gl code FMT CONTROL      ! KEN *~
            *          ! PROGRAM STANDARD IS EXTERNAL GL CODE     ! KEN *~
            * 12/13/85 ! Vendor file format changes               ! MJB *~
            * 05/30/86 ! Compatable with receiver logic, major    ! HES *~
            *          ! screen overhauls (essentially a rewrite),!     *~
            *          ! changed file layouts, added multi modes. !     *~
            * 08/15/86 ! Added Soft Enables                       ! HES *~
            * 12/15/86 ! Disallow edit of job field if linked toPO! HES *~
            * 02/05/87 ! Lot Track and Serial Number Enhancements ! ERN *~
            * 05/20/87 ! File format changes for Inventory cost;  ! JIM *~
            *          !   capture inventory cost via HNYCDIST;   !     *~
            *          !   capture Price cost variance account #. !     *~
            * 08/12/87 ! Show Job & Account For RCVrd Items       ! HES *~
            * 10/28/87 ! Auto change Inv cost when price changes  ! HES *~
            * 12/08/87 ! Format Price/Cost Variance Acct after GET! HES *~
            * 12/23/87 ! Multi-currency mods. Program split in 2. ! JIM *~
            * 03/14/88 ! Allow blank dflt pur account  (R5.00.05) ! HES *~
            * 10/13/88 ! Format GL Variance Account once more.    ! JDH *~
            * 05/22/89 ! Rem'd statement # 20240                  ! MLJ *~
            * 06/12/89 ! Added ASKUSER for Invoice total.         ! JDH *~
            * 06/13/89 ! Grabbed more currency info loading recvr ! JDH *~
            * 07/07/89 ! Grabbed more currency info loading PO    ! JDH *~
            * 10/03/89 ! Recurring Invoices ONLY in statutory.    ! JDH *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 05/31/90 ! Currency isn't blank unless MC is off.   ! JDH *~
            * 09/27/90 ! Cngd RETURN in DELETEMODE (not a sub).   ! JDH *~
            * 05/14/91 ! Added PF14)See Receipts. To show PO #,   ! SID *~
            *          !  Rcv Hld, QC, QC Hld, Store # and Lot #. !     *~
            * 05/17/91 ! PRR 11980 Added test on FIELDNR% in 2nd  ! SID *~
            *          !  input loop.                             !     *~
            *          !  Added 'ALLFREE'                         !     *~
            * 01/28/92 ! PRR 11597, 11743 - 1st invoice now locks ! MLJ *~
            *          !  price eliminating Interim Liab balancing!     *~
            *          !  issues.  No longer use extension field  !     *~
            *          !  on 2nd inv to back-calc a new price.    !     *~
            *          ! PRR 11684 - On PF17 'Auto Generate Inv'  !     *~
            *          !  plowcode, PF10 'Search By PO#' points to!     *~
            *          !  the PO specified instead of below it.   !     *~
            *          ! PRR 10715 - Added ability to exclude QC  !     *~
            *          !  Qty from Default Qty to Pay.  Added     !     *~
            *          !  (14) See Receipts in Edit Mode.         !     *~
            *          ! Corrected erroneous Price/Cost Variance  !     *~
            *          !  message.                                !     *~
            * 02/03/92 ! Added VAR$ for SWITCHS.VBK.  PRR 11538-  ! MLJ *~
            *          !  Added ASKUSER when the quantity entered !     *~
            *          !  greater than uninvoiced qty for PO line.!     *~
            * 03/31/92 ! PRR 11770 - Added ASKUSER notification   ! MLJ *~
            *          !  when max invoice lines has been reached.!     *~
            * 06/04/92 ! Minor mods for DEC compatibility.        ! MLJ *~
            * 07/20/92 ! Added '%' as needed to reduce object size! MLJ *~
            * 03/24/93 ! Added Core Value coding.  Default cost   ! JBK *~
            *          !  for std costed parts with flag on will  !     *~
            *          !  be the std cost.                        !     *~
            *          ! PRR 12720 - For line items from PO, Price!     *~
            *          !  & Cost will be independent in Input mode!     *~
            *          ! PRR 12743 - For exclude QC Qty from      !     *~
            *          !  default Qty to pay will not allow a     !     *~
            *          !  negative quantity.                      !     *~
            * 04/29/93 ! PRR 12483 - Issues ASKUSER when entering ! MLJ *~
            *          !  'new invoice' which has payments on file!     *~
            *          !  Also issues ASKUSER when deleteing lines!     *~
            *          !  of an invoice which has payments on file!     *~
            *          ! PRR (Mad River) - Issues ASKUSER when    !     *~
            *          !  trying to delete line from invoice which!     *~
            *          !  has pmts on file.                       !     *~
            *          ! PRR 12714, 12878 - Can now edit G/L accts!     *~
            *          !  and Job # on Non-Stocked line items.    !     *~
            *          ! MISC - Replaced FOR/NEXT Loop opens on   !     *~
            *          !  channels 1-21.  Initialize Data section !     *~
            *          !  now uses INIT(" ") instead of = " ".    !     *~
            *          ! PRR 13116 - Corrected erroneous error msg! MLJ *~
            *          !  when deleting a non-stocked part.       !     *~
            * 04/01/94 ! Condition for background posting         ! KAB *~
            * 07/07/94 ! Add Contract ID,&Line to Line Item Screen! RJH *~
            *          !  Add PF25 key for Vend Contract Inquiry  ! RJH *~
            * 08/15/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        com /* Any changes made here need to be made in PAYINPTS, also */~
            r41603fix$16,                /* FOR FIX LOGIC INTEGRITY    */~
            acct$(100)16,                /* ACCOUNT NUMBERS            */~
            acctdescr$32,                /* ACCOUNT DESCRIPTION        */~
            base%,                                                       ~
            c%,                                                          ~
            contract_id$(100)16,         /* Purchase Contract ID       */~
            contract_descr$32,           /* Purchase Contract Descriptn*/~
            contract_line$(100)4,        /* Purchase Contract Line #   */~
            core_on%,                                                    ~
            core,                        /* Core Std Cost              */~
            core_inv_flag$1,             /* Core Value Inv Trans Flag  */~
            cost(12), cost$(100)96,      /* Inventory cost hold areas  */~
            costtype$(100)1,             /* Inventory item Cost Type   */~
            convdate$6, conveqv, convunt,/* Currency conversion fields */~
            currkey$50,                  /* Currency lines read key    */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4, currdesc$32,     /* Currency code& description */~
            cursor%(2),                  /* CURSOR LOACATIONS FOR EDIT */~
            d%,                                                          ~
            date$8,                      /* SCREEN DATE STRING STUFF   */~
            date1$8,                     /* DATE FOR PAY DATE COMPUTING*/~
            date2$8,                     /* ANOTHER DATE...            */~
            datetime$7,                  /* DATE TIME STAMP            */~
            defaultacct$16,              /* DEFAULT ACCOUNT FOR INPUT  */~
            defstr$3,                    /* DEFAULT STORE NUMBR INPUT  */~
            delmax%,                                                     ~
            delpart$(100)25,             /* DELETED PART               */~
            delstore$(100)25,            /* DELETED PART'S STORE       */~
            dellot$(100)25,              /* DELETED PART'S STORE'S LOT */~
            delqty(100),                 /* DELETED PART'S HNYHOLD     */~
            delhold(100),                /* DELETED PART'S HNYHOLD     */~
            descr_m(16),                 /* Descr Map For PlowCode     */~
            dfac$(20)1,                  /* SUMMARY SCREEN FAC'S       */~
            disc_amt$9, disc_amt,        /* DISCOUNT AMOUNT            */~
            disc_pct$10, disc_pct,       /* DISCOUNT PERCENT           */~
            discdate$8,                  /* PAY TAKING DISCOUNT DATE   */~
            editmode%,                                                   ~
            errormsg$79,                 /* ERROR MESSAGE TEXT LINE    */~
            ext$(100)10, ext,            /* EXTENSIONS AND STUFF       */~
            factor$10,                   /* UOM Conversion Factor      */~
            factor(100),                 /* UOM Conversion Factor      */~
            fieldnr%,                                                    ~
            file%, lcur%,                                                ~
            freetext$20,                 /* FREE TEXT INFORMATION      */~
            groupdescr$32,               /* Cutover Group Description  */~
            header$(3)79,                /* SCREEN TITLE               */~
            hold$1,                      /* Invoice On Hold Flag       */~
            i$(24)80,                    /* JUNK SCREEN IMAGE (NOT USED*/~
            inc(2),                      /* For The Elusive "PLOWCODE" */~
            inc$(2)28,                   /* For The Elusive "PLOWCODE" */~
            infomsg$79,                  /* DESCRIPTIVE MESSAGE LINE   */~
            invamt,                                                      ~
            invcost(100),invcost$10,     /* Total Inventory cost       */~
            invdate$8,                   /* SCREEN INVOICE DATE        */~
            invnet$10,                   /* INVOICE TOTAL LESS DISCOUNT*/~
            invoicenr$16,                /* INVOICE NUMBER             */~
            invtype$,                    /* INVOICE TYPE (INTERNAL)    */~
            invtotal$10,                 /* INVOICE TOTAL              */~
            job$(100)8,                  /* JOB NUMBERS FOR LINES      */~
            jobdescr$32,                 /* JOB NUMBER DESCRIPTION     */~
            jobtext$33,                  /* JOB NUMBER POST FROM RCV   */~
            keyhit%,                                                     ~
            l%(2),                       /* Invoice Field Prompt Lenght*/~
            ll%,                                                         ~
            lastinvoice$16,              /* LAST INVOICE NUMBER INPUT  */~
            lastvendor$9,                /* LAST VENDOR PROCESSED      */~
            lfac$(20)1,                  /* LINEAR INPUT FAC'S         */~
            linemode%,                                                   ~
            lot$(100)6,                  /* LOT OF AP LINE ITEM        */~
            manual$8,                    /* For call to 'MANUAL'       */~
            maxlines%,                                                   ~
            message$79,                  /* INPUT MESSAGE              */~
            mode$3,                      /* "YES" Indicates Normal Mode*/~
            nethold, newhold,                                            ~
            nondisc$10, nondisc,         /* NON-DISCOUNTABLE AMOUNT    */~
            oldhold,                                                     ~
            oldseq$(100)3,               /* OLD SEQUENCE NUMBER        */~
            origdate$6,                  /* DATE OF ORIGINAL INVOICE   */~
            origuserid$3,                /* USERID OF ORIGINAL INVOICE */~
            part$(100)25,                /* PART NUMBERS FOR LINES     */~
            partdescr$34,                /* PART NUMBER DESCRIPTION    */~
            payacct$16,                  /* PAY ACCOUNT NUMBER         */~
            payacctdescr$32,             /* PAYABLES ACCOUNT DESCRIPTN */~
            payaccttype$1,               /* PAYABLES ACCOUNT TYPE      */~
            paydate$8,                   /* PAYABLES DATE INFORMATION  */~
            pfdescr$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            plowkey$90,                  /* KEY FOR PLOW ROUTINES      */~
            po$16,                       /* PO Number For Auto Generate*/~
            po$(100)16,                  /* PO Number For Line         */~
            poline$(100)3,               /* PO Line For Line!          */~
            prog$8,                      /* Program name               */~
            postatstdcost$1,             /* Post Cost at Std Cost      */~
            puracct$16,                  /* PURCHASES ACCOUNT NUMBER   */~
            puracctdescr$32,             /* PURCHASES ACCT DESCRIPTION */~
            price(100),                  /* CALCULATED PRICES FOR AP LN*/~
            price$(100)10,               /* CALCULATED PRICES FOR AP LN*/~
            prompt$(6)29,                /* Floating Screen Prompts    */~
            qty(100),                    /* QUANTITIES FOR EACH LINE   */~
            qtyhold(100,2),              /* OLD QUANTITIES FOR HNYHOLD */~
            qty$(100)10,                 /* QUANTITIES FOR EACH LINE   */~
            qty_inqc(100),               /* QTY IN QC      - EACH LINE */~
            qtytopay$1,                  /* QTY TO PAY SWITCH          */~
            qty_qchold(100),             /* QTY IN QC HOLD - EACH LINE */~
            rcv$(100)16,                 /* Reveiver Number For Line   */~
            readkey$90,                  /* KEY FOR PLOW ROUTINES      */~
            receiver$16,                 /* DEFAULT REVEIVER NUMBER    */~
            regulardate$8,               /* REGULAR DATE INFORMATION   */~
            reman%,                                                      ~
            remanpart$50,                /* Reman Part Key             */~
            return%,                                                     ~
            scr%(3,17), set%(255),       /* Enable arrays              */~
            search%(2),                  /* CURSOR LOCATIONS FOR EDIT  */~
            seq$(100)3,                  /* SEQUENCE NUMBERS FOR SCREEN*/~
            seqnr$3,                     /* SEQUENCE NUMBER FLAG       */~
                                         /* SERIAL NUMBER VARIABLES    */~
            sn_config$(100)36,           /*  Who were they for?        */~
                tmp_config$36,           /*                            */~
            sn_delex%(999), sn_delex%,   /*  Deleted Indices           */~
            sn_dpart$(999)25,            /*                            */~
            sn_index%(100),              /*  Unique Index for line-SNs */~
            sn_loc$30,                   /*  Current Location          */~
            sn_mastr$(2)150,             /*  Entire SERMASTR record    */~
            sn_source$1,                 /*  Source                    */~
            sn_status$1,                 /*  Save 'To' Status          */~
            sn_trankey$40,               /*  Transaction Key           */~
            sn_used%(100),               /*  Gone but not forgotten    */~
            sn_used_msg$30,              /*                            */~
                                                                         ~
            statutory$4,                 /* Statutory currency code    */~
            store$(100)3,                /* STORE # FOR AP LINE ITEMS  */~
            strdescr$32,                 /* JOB NUMBER DESCRIPTION     */~
            sumhdr$79,                   /* Line summary header        */~
            sysacct$(6)9,                /* SYSTEM DEFAULT ACCOUNTS    */~
            ten99$4,                     /* 1099 CATEGORY              */~
            ten99descr$32,               /* 1099 CATEGORY DESCRIPTION  */~
            text$4,                      /* Document Text Id. Number   */~
            textmsg$79,                  /* Text Rtn Message           */~
            text$(113,1)70,              /* Free Text Array            */~
            tfac$(20)1,                  /* SUMMARY SCREEN FAC'S       */~
            this%,                                                       ~
            topline$79,                  /* Floating Program Title     */~
            userid$3,                    /* USERID THIS USER           */~
            var$20,                      /* SWITCHS.VBK Variable       */~
            vbkkey$100,                  /* MISC READKEY               */~
            varacct$(100)16,             /* Price cost variance acct # */~
            varacctdescr$32,             /* Description of above       */~
            varmsg$34,                   /* Price cost variance Warning*/~
            venaddr$(6)30,               /* ADDRESS THIS VENDOR        */~
            vencode$9,                   /* VENDOR CODE THIS INVOICE   */~
            vendescr$32,                 /* VENDOR DESCRIPTION         */~
            venpart$(100)25,             /* Vendor's Part Number       */~
            venprice$10,                 /* Vendor Price For Line Item */~
            vpc_open%,                   /* VBKVSA Open Flag (1=Y,-1=N)*/~
            vpc_temp$16,                 /*    Contract Line for Query */~
            vpc_temp1$4,                 /*    Contract Line for Query */~
            venqty$10,                   /* Vendor Qty For Line Item   */~
            vpc_code$25,                 /* Vend Purchs Cntarct ItemCd */~
            vpc_potyp$1,                 /* PO Parts VPC Type (P,A,M,H)*/~
            vpc_type$1,                  /* Vend Purchs Cntarct Type   */~
            vpc_vendor$9,                /* Vend Purchs Cntarct Vendor */~
            xixacct$16,                  /* ACCOUNT WORK VARIABLE      */~
                                                                         ~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            f2%(64),                     /* FILE STATUS FLAGS          */~
            fs%(64),                     /* FILE STATUS FLAGS          */~
            rslt$(64)20                  /* RETURNED FROM OPENFILE     */

            dim ~
            blankdate$8,                 /* Blank Date for Comparisons */~
            tdate$8, tdate2$8            /* Temporary Date Variables   */
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! USER INFORMATION (PAYABLES DATE)         *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER.  SALES ACCT VERIFICATION *~
            * # 3 ! VENDOR   ! LOAD VENDOR MASTER INFORMATION HERE      *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN HEADER FILE.               *~
            * # 6 ! PAYLINES ! PAYABLES LINE ITEMS FILE                 *~
            * # 7 ! SYSFILE2 ! SYSTEM INFO (DEFAULT PAY DATES)          *~
            * # 8 ! STORNAME ! STORE MASTER FILE                        *~
            * # 9 ! PAYBUFFR ! PAYABLES INVOICE HEADER BUFFER           *~
            * #10 ! PAYBUF2  ! PAYABLES INVOICE LINE ITEM BUFFER.       *~
            * #11 ! JOBMASTR ! WIP/JC MASTER FILE                       *~
            * #12 ! HNYQUAN  ! INVENTORY QUANTITY FILE                  *~
            * #13 ! JBMASTR2 ! Production job master file               *~
            * #14 ! VBKLINES ! Backlog line item file                   *~
            * #15 ! VBKMASTR ! Backlog main header file                 *~
            * #16 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * #17 ! PAYTSKNG ! PAYINPUT Tasking Control File            *~
            * #18 ! RCVTIF2  ! Receiver Line Items                      *~
            * #19 ! RCVLINES ! Receiver Line Items                      *~
            * #20 ! GENCODES ! General Purpose Code File                *~
            * #21 ! PIPMASTR ! PLANNED PART?                            *~
            * #22 ! CSHMASTR ! 1099 POSTING NEEDS IT       Conditional  *~
            * #23 ! CSHLINES ! Cash Disbursements File     Conditional  *~
            * #24 ! CSH1099  ! 1099 DETAIL FILE            Conditional  *~
            * #25 ! TXTFILE  ! System Text File                         *~
            * #26 ! SERMASTR ! Serial Number Master File                *~
            * #27 ! SERWORK  !               Work File                  *~
            * #28 ! SERTIF   !               Transaction Image File     *~
            * #29 ! SERHOLD  !               Vendor Invoice Restore     *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #41 ! PAYLNCUR ! Currency-specific PAY line items         *~
            * #42 ! PAYLNBF2 ! Currency-specific PAYBUF2 line items     *~
            * #43 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #44 ! RCVLNCUR ! Currency Line Item Information (Receiver)*~
            * #45 ! VBKLNCUR ! Currency Line Item Information (VBK)     *~
            * #46 ! COREXREF ! Core Part Cross Reference File           *~
            * #47 ! VPCMASTR ! Vendor Purcase Master File               *~
            * #55 ! DUMMY    ! Dummy Channel for PLOWCODE               *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select # 2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select #4,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25

            select  #5, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #6, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #7,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #8,  "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize=300,                                     ~
                        keypos=1, keylen=3

            select #9,  "PAYBUFFR",      /* INVOICE HEADER BUFFER      */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 10,                         ~
                        alternate key 1, keypos = 11, keylen = 25

            select #10, "PAYBUF2",       /* INVOICE LINE ITEM BUFFER   */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #11, "JOBMASTR",      /* JOB MASTER FILE            */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8

            select #12, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select #13, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            select #14, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =  28

            select #15, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            select #16, "VENPRICE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  256,                                  ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34          ~

            select #17, "PAYTSKNG",                                      ~
                        varc,     indexed,  recsize =  28,               ~
                        keypos = 1, keylen = 25

            select #18, "RCVTIF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24

            select #19, "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24

            select #20, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 24

            select #21, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos = 2, keylen = 25,                         ~
                        alt key 1, keypos = 1, keylen = 26

            select #22, "CSHMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 17,                         ~
                        alternate key 1, keypos = 41, keylen = 9, dup,   ~
                                  key 2, keypos = 50, keylen = 6, dup

            select #23, "CSHLINES",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos = 1, keylen = 20,                        ~
                         alternate key 1, keypos = 21, keylen = 16, dup

            select #24, "CSH1099",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos = 7, keylen = 20,                         ~
                        alt key 1, keypos =  1, keylen = 26,             ~
                            key 2, keypos = 27, keylen = 32

            select #25, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #26, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #27, "SERWORK",                                       ~
                        varc,     indexed,  recsize =  48,               ~
                        keypos = 1, keylen = 23

            select #28, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #29, "SERHOLD",                                       ~
                        varc,     indexed,  recsize =  333,              ~
                        keypos = 1, keylen = 33

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #41,  "PAYLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 28,                      ~
                        alt key  1, keypos =   1, keylen =  32

            select #42, "PAYLNBF2",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos =  5, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 32

            select #43, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            select #44, "RCVLNCUR",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 5, keylen = 52,                         ~
                        alt key 1, keypos = 1, keylen = 56

            select #45, "VBKLNCUR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 5, keylen = 28,                         ~
                        alt key 1, keypos = 1, keylen = 32

            select #46, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            select #47, "VPCMASTR",                                      ~
                         varc,     indexed,  recsize =  600,             ~
                         keypos =  10, keylen = 20,                      ~
                         alt key  1, keypos =  1, keylen = 29,           ~
                             key  2, keypos = 60, keylen = 26, dup

            select #55, "DUMMY",                                         ~
                        varc,     indexed,  recsize  =  5,               ~
                        keypos =  1,  keylen =  5

            call "SHOSTAT"  ("Opening Files, One Moment Please.")

            call "OPENCHCK" (#1,  fs%( 1%), f2%( 1%),   0%, rslt$( 1%))
            call "OPENCHCK" (#2,  fs%( 2%), f2%( 2%),   0%, rslt$( 2%))
            call "OPENCHCK" (#3,  fs%( 3%), f2%( 3%),   0%, rslt$( 3%))
            call "OPENCHCK" (#4,  fs%( 4%), f2%( 4%),   0%, rslt$( 4%))
            call "OPENCHCK" (#5,  fs%( 5%), f2%( 5%), 100%, rslt$( 5%))
            call "OPENCHCK" (#6,  fs%( 6%), f2%( 6%), 200%, rslt$( 6%))
            call "OPENCHCK" (#7,  fs%( 7%), f2%( 7%),   0%, rslt$( 7%))
            call "OPENCHCK" (#8,  fs%( 8%), f2%( 8%),   0%, rslt$( 8%))
            call "OPENCHCK" (#9,  fs%( 9%), f2%( 9%), 100%, rslt$( 9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 200%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),   0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),   0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),   0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),   0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),   0%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),   0%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%), 100%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%),   0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),   0%, rslt$(19%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%),   0%, rslt$(20%))
            call "OPENCHCK" (#21, fs%(21%), f2%(21%),   0%, rslt$(21%))
            call "OPENCHCK" (#23, fs%(23%), f2%(23%),   0%, rslt$(23%))

*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            call "READ100" (#07, "SWITCHS.CUR", f1%(7%))
            if f1%(7%) <> 0% then get #07 using L04590, curr$, statutory$, ~
                currtype$
L04590:         FMT POS(21), CH(1), CH(4), POS(34), CH(1)
            if curr$ <> "Y" then statutory$ = " "
            if curr$ <> "Y" then goto L04670
                call "OPENCHCK" (#40, 0%, 0%,   0%, " ")
                call "OPENCHCK" (#41, 0%, 0%, 200%, " ")
                call "OPENCHCK" (#42, 0%, 0%, 200%, " ")
                call "OPENCHCK" (#43, 0%, 0%,   0%, " ")
                call "OPENCHCK" (#44, 0%, 0%,   0%, " ")
                call "OPENCHCK" (#45, 0%, 0%,   0%, " ")

L04670
*        Check for Core
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#7, plowkey$, core_on%)
                if core_on% <> 1% then L04780
            get #7 using L04730, core_inv_flag$
L04730:         FMT POS(134), CH(1)
            if core_inv_flag$ <> "Y" then core_on% = 0%
            if core_on% <> 1% then L04780
                call "OPENCHCK" (#46, 0%, 0%,  0%, " ")

L04780
*        Check for Vendor Contracts
            call "OPENCHCK" (#47, vpc_open%, f2%(47%),  0%, " ")

        REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * SETS USER ID, DATES, CONTROL INFO  ETC...                 *~
            *************************************************************

        blankdate$ = " "
        call "DATUFMTC" (blankdate$)

*        R41603FIX$ is required to retain 'fixed' datas integrity
*        following 4.16.03 release fix.  Don't remove references
*        for at least one year after 16.03 release date, (8-1-86)
            r41603fix$ = "A" & hex(5c) & "P CONVERSION"

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$, "CF", prog$)
            lines_allowed% = dim(part$(),1%)
            ll% = 6%      /* Default Lot Number Length       */
            var$ = "SWITCHS.VBK         "
*        Get Users Posting Date...
            call "READ100" (#1, userid$, f1%(1%))
                if f1%(1%) = 0% then L09300
            get #1, using L09250, paydate$, defstr$
L09250:         FMT XX(9), CH(6), XX(48), CH(3)

*        Validate Users Posting Date...
            call "WHICHMON" (#7, paydate$, this%)
            if this% <> 0% then L09350
L09300:         call "ASKUSER" (keyhit%, "Sorry, " & userid$,            ~
                      "Your Post Date Is Invalid", " ",                  ~
                                               "Press (RETURN) to Exit.")
                goto L65000

L09350:         call "DATEFMT" (paydate$)

*        Get Quantity To Pay Exclusion Default...
            call "VBKSWTCH" ("QTYTOPAY", qtytopay$, temp, u3%)

*        Find out if std cost items to be costed at std
            call "VBKSWTCH" ("POSTDCST", postatstdcost$, temp, u3%)

*        Get A/P System Defaults...
            call "READ100" (#7, "MODULE.DEFAULTS.AP  ", f1%(7%))
            if f1%(7%) = 0% then L09430
                get #7, using L09410,sysbillsdue%, sysdiscsdue%, sysacct$()
L09410:         FMT XX(20), 2*BI(4), XX(8), 6*CH(9)

L09430:     for i% = 1% to lines_allowed%
                convert i% to seq$(i%), pic(###)
            next i%

            topline$ = "Manage Vendor Invoices              Post Date:   ~
        ~        Today's Date:"
            str(topline$,48%,8%) = paydate$
            str(topline$,72%,8%) = date$
            manual$ = "PROC22" : l%(1%) = 1% : l%(2%) = 16%
            prompt$(1%) = "Invoice Number"
            prompt$(2%) = "Invoice Date"
            prompt$(3%) = "Regular Disbursement Date"
            prompt$(4%) = "Discount Disbursement Date"
            prompt$(5%) = "Invoice On Hold? (Y/N)"
            prompt$(6%) = "Default Receiver Number"

*        Check out the mode we're in via a 'hidden' GETPARM
            mode$ = "REC"   /* If run without procedure, you get this. */
            call "GETPARM" addr ("ID", "S", "RUNMODE ", "@", "0001",     ~
                  "SCREEN", 0%, "K", "INVOICES", mode$, 3%, 5%, 32%, "U")

            if mode$ <> "OFF" then L09760
               REM We're managing Offline invoices...
               str(topline$,,35%) = "Manage Offline Invoices"
               manual$ = "PROC23"
               call "ASKUSER" (2%, "Please Note", hex(8c) & "Invoices " &~
               "entered or changed using this program do NOT update " &  ~
               hex(84), hex(8c) &  "the G/L or Inventory.  Please use " &~
               "with caution." & hex(84), "Press (RETURN) to continue.")
               goto L09850

L09760:     if mode$ <> "REC" then L09850
               REM We're managing recurring control invoices...
               str(topline$,,56%) =                                      ~
                               "Manage Recurring Control Invoices"
               prompt$() = " "
               prompt$(1%) = "Invoice Cutover Group"
               prompt$(4%) = "Cutover Expiration Date"
               manual$ = "PAYINPUT" : l%(1%) = 2% : l%(2%) = 4%
               goto L09860

L09850
*        See it is ok to mess with buffers
            call "PAYBKCTL" (#9, ret%)
               if ret% <> 0% then end 0%

*        See if this User is a module administrator
L09860:     call "CMSMACHK" ("VBK", lfac$(1%), lfac$(2%))
                if lfac$(1%) = "Y" or lfac$(2%) = "Y" then admin% = 1%

            sumhdr$ = "Ln# Part Number                 Quantity   "    & ~
                "   Price  Extension PO Number"
            gosub init_enables

        REM *************************************************************~
            *     I N P U T   I N V O I C E   H E A D E R   I N F O     *~
            *                                                           *~
            * GETS INVOICE HEADER INFORMATION AND THAT SORT OF THING.   *~
            *************************************************************

        inputmode
            vencode$, invoicenr$ = " "
            gosub init_data

            for fieldnr% = 1% to 8%
                flag% = 1% : gosub L20000
                      if enabled% = 0% then L10115
L10065:         gosub deffn_201
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10100
L10080:                  fieldnr% = max(3%, fieldnr%-1%)
                         gosub L20000
                         if fieldnr% = 3% then L10065
                         if enabled% = 0% then L10080
                         goto L10065
L10100:               if keyhit%  = 16% and fieldnr% < 3% then L65000
                      if keyhit%  = 17% then gosub auto_invoice
                      if keyhit% <>  0% then       L10065
L10115:         gosub deffn_151
                      if errormsg$ <> " " then L10065
                next fieldnr%

L10135:     for fieldnr% = 1% to 6%
                flag% = 1% : gosub L21000
                      if enabled% = 0% then L10190
L10150:         gosub deffn_202
L10155:               if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10185
                         fieldnr% = max(1%, fieldnr%-1%)
                         gosub L21000
                         if fieldnr% = 1% then L10135
                         if enabled% = 0% then L10155
                         goto L10150
L10185:               if keyhit% <>  0% then       L10150
L10190:         gosub deffn_152
                      if errormsg$ <> " " then L10150
                next fieldnr%

L10210:     if maxlines% = lines_allowed% then L10230
                c% = maxlines% + 1%
                gosub inputlines
                if keyhit% <> 16% then L10240
L10230:              gosub columnone
                     goto line_summary
L10240:         maxlines% = maxlines% + 1%
                gosub total_up_invoice
                goto L10210

        inputlines
            linemode% = 0%
            gosub columnone
            if maxlines% = 0% and editmode% = 0% then                    ~
                    errormsg$ = hex(84) & "Enter The Invoice Details..."
L10285:     for fieldnr% = 1% to 16%       /* Page 1 of Line Items     */
                flag% = 1% : gosub L22000
                      if enabled% = 0% then L10380
L10300:         gosub deffn_103
                      if keyhit%  = 16% then return
                      if keyhit%  = 14% then gosub plow_rcvlines
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then gosub restart_line
                      if keyhit%  = 25% then gosub view_vendor_contracts
                      if keyhit% <>  6% then L10335
                         gosub prevline
                         goto L10380
L10335:               if keyhit% <>  4% then L10375
L10340:                  fieldnr% = max(1, fieldnr% - 1%)
                         if fieldnr% > 1% then L10360
                             gosub columnone_special
                             goto L10285
L10360:                  gosub L22000
                         if enabled% <> 0% then L10300
                         goto L10340
L10375:               if keyhit% <>  0% then       L10300
L10380:         gosub deffn_153
                      if errormsg$ <> " " then L10300
                next fieldnr%
                return

        prevline
            if c% = 1% then return
                on fieldnr% gosub L10486,           /* Currency code    */~
                                  L10490,           /* PO NUMBER & LINE */~
                                  L10500,           /* RECEIVER NUMBER  */~
                                  L10505,           /* PART NUMBER      */~
                                  L10510,           /* STORE NUMBER     */~
                                  L10515,           /* LOT NUMBER       */~
                                  L10520,           /* VENDOR PART NUMBR*/~
                                  L10522,           /* Contract ID &Line*/~
                                  L10525,           /* QUANTITY PER UNIT*/~
                                  L10530,           /* QUANTITY         */~
                                  L10535,           /* PRICE            */~
                                  L10540,           /* EXTENSION        */~
                                  L10541,           /* Inventory cost   */~
                                  L10545,           /* ACCOUNT NUMBER   */~
                                  L10546,           /* Cost Var acct #  */~
                                  L10550            /* JOB NUMBER       */
            if fieldnr% > 7% and fieldnr% < 11% then format_fields
            return

L10486:         return
L10490:         po$      (c%) = po$      (c%-1%)
                poline$  (c%) = poline$  (c%-1%) : return
L10500:         rcv$     (c%) = rcv$     (c%-1%) : return
L10505:         part$    (c%) = part$    (c%-1%)
                costtype$(c%) = costtype$(c%-1%) : return
L10510:         store$   (c%) = store$   (c%-1%) : return
L10515:         lot$     (c%) = lot$     (c%-1%) : return
L10520:         venpart$ (c%) = venpart$ (c%-1%) : return
L10522:         contract_id$(c%)   = contract_id$(c%-1%)
                contract_line$(c%) = contract_line$(c%-1%)   :  return
L10525:         factor   (c%) = factor   (c%-1%) : return
L10530:         qty      (c%) = qty      (c%-1%) : return
L10535:         price    (c%) = price    (c%-1%) : return
L10540:         ext$     (c%) = ext$     (c%-1%) : return
L10541:         invcost  (c%) = invcost  (c%-1%)
                cost$    (c%) = cost$    (c%-1%) : return
L10545:         acct$    (c%) = acct$    (c%-1%) : return
L10546:         varacct$ (c%) = varacct$ (c%-1%) : return
L10550:         call "READ100" (#21, part$(c%), f1%(21%))
                     if f1%(21%) <> 0% then return
                job$     (c%)   = job$  (c%-1%) : return

        auto_invoice
                readkey$ = vencode$
                errormsg$ = hex(06) & "Select P.O. To Invoice."
                if fieldnr% = 2% then L10640
                REM Find Vendor, P.Os...
                errormsg$ = errormsg$ &"  Use PF(10) To Search By P.O. #"
                call "GETCODE" (#15, readkey$,errormsg$,0%,1.16,f1%(15%))
                     if f1%(15%) <> 0% then L10608
                     errormsg$ = hex(00)
                     return
L10608:         vencode$ = readkey$
                po$ = str(readkey$,10%)
                errormsg$ = " "
                auto% = 1%
                keyhit% = 0%
                return

L10640:         REM We Know The Vendor, Show P.Os.
                header$(1%) = "  P.O. Number        Receiver Number"
                REM Show only lines with non zero quantity...
                call "PLOWCODE" (#19, readkey$, errormsg$,  3009%, 2.16, ~
                                             f1%(19%), header$(), 14, 26)
                     if f1%(19%) <> 0% then L10676
                     errormsg$ = hex(00)
                     return
L10676:         po$ = str(readkey$,10%)
                errormsg$ = " "
                auto% = 1%
                keyhit% = 0%
                return

        restart_line
L10710:     u3% = 2%
            call "ASKUSER" (u3%, "RESTART LINE",                         ~
                            "Press (RETURN) to RESTART Line Item",       ~
                            "- OR -", "Press PF-1 to EXIT Restart.")
            if u3% = 1% then return
                if u3% <> 0% then L10710
                     return clear
                     goto  inputlines

        REM *************************************************************~
            *           E D I T   I N V O I C E   H E A D E R           *~
            *                                                           *~
            * EDITS INVOICE HEADERS, PERMITTING ALL OF THE FIELDS TO BE *~
            * MODIFIED.                                                 *~
            *************************************************************

        editmode
            base%, auto% = 0% : editmode% = 1%

        editpg1
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."
            fieldnr% = 0% : gosub deffn_211
                 if keyhit%  =  1% then gosub startover
                 if keyhit%  =  2% then       line_summary
                 if keyhit%  =  5% then       editpg2
                 if keyhit%  = 16% then       datasave
                 if keyhit%  = 25% then gosub edit_text
                 if keyhit% <> 0% and keyhit% <> 29% then editpg1
            oldfieldnr% = 0%
L11210:     fieldnr% = cursor%(1) - 12%
            if fieldnr% < 3% or fieldnr% > 7% then editpg1
            if fieldnr% > 6% then fieldnr% = fieldnr% + 1%
            if fieldnr% = 6% and cursor%(2%) > 35% then fieldnr% = 7%
            if fieldnr% = oldfieldnr% then editpg1
            oldfieldnr% = fieldnr%
            if keyhit% <> 29% then L11320
                if admin% = 1% then call "ENABLSUB" ("MODIFY", manual$,  ~
                                   scr%(), set%(), 1%, fieldnr%, 0%, 0%)
                goto editpg1

L11320:     flag% = 2% : gosub L20000
                 if enabled% = 0% then editpg1
L11340:     gosub deffn_211
                 if keyhit%  =  1% then gosub startover
                 if keyhit% <>  0% then       L11340
            gosub deffn_151
                 if errormsg$ <> " " then L11340
            goto L11210

        editpg2
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."
            fieldnr% = 0% : gosub deffn_212
                 if keyhit%  =  1% then gosub startover
                 if keyhit%  =  2% then       line_summary
                 if keyhit%  =  4% then       editpg1
                 if keyhit%  = 16% then       datasave
                 if keyhit%  = 25% then gosub edit_text
                 if keyhit% <> 0% and keyhit% <> 29% then editpg2
            oldfieldnr% = 0%
L11520:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 6% then editpg2
            if fieldnr% = oldfieldnr% then editpg2
            oldfieldnr% = fieldnr%
            if keyhit% <> 29% then L11610
                if admin% = 1% then call "ENABLSUB" ("MODIFY", manual$,  ~
                                    scr%(), set%(), 2%, fieldnr%, 0%, 0%)
                goto editpg2

L11610:     flag% = 2% : gosub L21000
                 if enabled% = 0%  then editpg2
L11630:     gosub deffn_212
                 if keyhit%  =  1% then gosub startover
                 if keyhit% <>  0% then       L11630
            gosub deffn_152
                 if errormsg$ <> " " then L11630
            goto L11520

        REM *************************************************************~
            *           E D I T   I N V O I C E   L I N E S             *~
            *                                                           *~
            * EDITS INVOICE LINES ITEMS.                                *~
            *************************************************************

        line_summary              /* Summary Screen */
            base% = max(0%,min(base%,maxlines%-13%))
            editmode% = 1%
            message$ =   "To Modify a Line Item, Position Cursor and" &  ~
                         " press (RETURN)."
            infomsg$ = " "
            fieldnr% = 0% : gosub deffn_115
              errormsg$ = " "
              if keyhit% =   1% then gosub startover
              if keyhit% =   2% then base% = 0%
              if keyhit% =   3% then base% = maxlines%
              if keyhit% =   4% then base% = base% - 10%
              if keyhit% =   5% then base% = base% + 10%
              if keyhit% =   6% then base% = base% - 1%
              if keyhit% =   7% then base% = base% + 1%
              base% = max(0%,min(base%,maxlines%-13%))
              if keyhit% =   8% then       L12290
              if keyhit% =   9% then       editmode
              if keyhit% =  12% then       L12290
              if keyhit% =  16% then       datasave
              if keyhit%  = 25% then gosub edit_text
              if keyhit% <> 28% then L12290
                 fieldnr% = 0%
                 goto deletemode
L12290:     fieldnr% = cursor%(1%) - 6%
            if keyhit% <> 11% or cursor%(1%) > 1% then L12320
                c% = maxlines% : goto insertmode
L12320:     if fieldnr% < 1% or fieldnr% > 13% then line_summary
            if fieldnr% = 0% and keyhit% = 0% then fieldnr% = 1%
            if fieldnr% = 0% and keyhit% <> 11% then line_summary
                c% = min(base% + fieldnr%, maxlines%)
                if c% = 0% and keyhit% <> 11% then line_summary
                fieldnr% = c% - base%
            if keyhit%  = 12% then       deletemode
            if keyhit%  = 11% then       insertmode
            if keyhit%  <> 0% then       line_summary

        editpg3       /* Line Item Detail - First Screen      */
            errormsg$ = " "
            gosub describe_line
            message$ = "To Modify Displayed Values, Position Cursor " &  ~
                       "To Desired Value And Press RETURN."
            linemode% = 1%

L12490:     fieldnr% = 0% : gosub deffn_113
                  errormsg$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  <  2% or keyhit%  > 7% then L12580
                     if keyhit%  =  2% then c% = 1%
                     if keyhit%  =  3% then c% = maxlines%
                     if keyhit%  =  6% then c% = max(1%, c%-1%)
                     if keyhit%  =  7% then c% = min(maxlines%, c%+1%)
                     goto editpg3
L12580:           if keyhit%  =  9% then editmode
                  if keyhit% <> 14% then L12590
                      if auto% = 0% then L12585
                          init(" ") plowkey$, readkey$
                          str(readkey$,26%,16%) = rcv$(c%)
L12585:               gosub plow_rcvlines
L12590:           if keyhit%  = 16% then line_summary
                  if keyhit%  = 25% then gosub view_vendor_contracts
                  if keyhit% <> 0% and keyhit% <> 29% then L12490

            oldfieldnr% = 0%
L12630:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% > 6% then fieldnr% = fieldnr% + 1%
            if fieldnr% = 5% and cursor%(2%) > 61% then fieldnr% = 6%
            if fieldnr% < 1% or fieldnr% = 2% or fieldnr% > 16% then L12490
            if fieldnr% = oldfieldnr% then editpg3
            oldfieldnr% = fieldnr%
            if keyhit% <> 29% then L12720
                if admin% = 1% then call "ENABLSUB" ("MODIFY", manual$,  ~
                                    scr%(), set%(), 3%, fieldnr%, 0%, 0%)
                goto editpg3

L12720:     flag% = 2% : gosub L22000
                if enabled% = 0% then editpg3
L12740:     gosub deffn_113
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12740
            gosub deffn_153
                  if errormsg$ <> " " then L12740
            gosub total_up_invoice
            goto L12630

        REM *************************************************************~
            *       I N S E R T   &   D E L E T E   L O G I C           *~
            *                                                           *~
            * INSERT & DELETE CODE RESIDES HERE.                        *~
            *************************************************************

        insertmode
            if maxlines% = lines_allowed% then line_summary

          REM Copy all Elements Up One...
            maxlines% = maxlines% + 1%
            c% = c% + 1%
            if c% = maxlines% then L13090
                roll% = -1%
                for temp% = maxlines% to max(2%, c%) step -1%
                     gosub roll_lines
                next temp%
                if c% > 1% then L13090
                     sn_index%(c%) = 0%
                     gosub columnone

L13090:   REM Get Line Item Data...
            editmode% = 0%
            gosub inputlines
            gosub total_up_invoice
            if keyhit% = 16% then delete_line
        goto insertmode

        deletemode
            if maxlines% = 0% then line_summary
            if fieldnr%  = 0% then L13210
                if sn_used%(c%) = 0% then L13155
                  if sn_index%(c%) >= 538976288 then L13155
                     errormsg$ = "Can't Delete: Serial Numbers Used."
                     goto line_summary
L13155:         if mode$   = "OFF" or mode$    = "REC" then L13210
                if po$(c%) <> " "  or rcv$(c%) <> " "  then L13210
                     oldhold = max(0, qtyhold(c%,1%) - qtyhold(c%,2%))
                     newhold = max(0, qtyhold(c%,1%))
                     nethold = newhold - oldhold
                     if nethold <= 0 then L13210
                          call "HNYAVAIL" (#4, #12, part$(c%),           ~
                                           store$(c%), lot$(c%),         ~
                                           errormsg$, nethold,           ~
                                           newhold, return%)
                     if errormsg$ <> " " then line_summary
L13210:     message$ = "To DELETE Flashing Data, press RETURN, To" &     ~
                       " Return without Delete, press PF1."

            readkey$ = str(invoicenr$) & hex(00)
            call "REDALT0" (#23, readkey$, 1%, f1%(23%))
                goto L13223
L13222:     call "READNEXT" (#23, f1%(23%))
L13223:         if f1%(23%) = 0% then L13250      /* No Pmts On File     */
            get #23 using L13225, temp1$, temp2$
L13225:         FMT CH(9), XX(11), CH(16)
            if temp2$ <> invoicenr$ then L13250   /* No Pmts On File     */
            if temp1$ <> vencode$   then L13222   /* Not for this Vendor */

L13230:     u3% = 0%
            call "ASKUSER" (u3%, "**** PAYMENTS ON FILE ****",           ~
                 "Invoice " & invoicenr$ & " currently has payments on "&~
                 "file.", "Press RETURN to continue Deletion process -O"&~
                 "R-"," Press PF(16) to return without Deleting.")
            if u3% = 0% then L13250
                if u3% <> 16% then L13230
                    goto line_summary

L13250:     d% = fieldnr% : gosub deffn_125
                if keyhit% =  1% then line_summary
                if keyhit% <> 0% then deletemode
            if fieldnr% <> 0% then delete_line
                c% = 1%
                for i% = 1% to maxlines%  /* Kill'em All */
                     gosub delete_it
                     if errormsg$ <> " " then c% = c% + 1%
                next i%
                if maxlines% <> 0% then errormsg$ = "Not Possible to" &  ~
                         " Delete All Lines. Try Remainder Individually."
                gosub total_up_invoice
                goto line_summary

        delete_line
            gosub delete_it
            gosub total_up_invoice
            goto line_summary

        delete_it
            if sn_used%(c%) = 0% then L13335
              if sn_index%(c%) >= 538976288 then L13335
                errormsg$ = "Can't Delete: Serial Numbers Used."
                return
L13335:     if mode$ = "OFF" or mode$ = "REC" then L13420
            if po$(c%) <> " " or rcv$(c%) <> " " then L13420
            oldhold = max(0, qtyhold(c%,1%) - qtyhold(c%,2%))
            newhold = max(0, qtyhold(c%,1%))
            nethold = newhold - oldhold
            if nethold = 0 and oldhold = 0 then L13420
               call "HNYAVAIL" (#4, #12, part$(c%), store$(c%), lot$(c%),~
                                 errormsg$, nethold, newhold, return%)
               if errormsg$ <> " " then return
               delmax% = delmax% + 1%
               delpart$ (delmax%) = part$(c%)
               delstore$(delmax%) = store$(c%)
               dellot$  (delmax%) = lot$  (c%)
               delqty   (delmax%) = nethold
               delhold  (delmax%) = oldhold
               call "HNYHOLD" (#12, part$(c%), store$(c%), lot$(c%),     ~
                                       nethold, return%)
L13420:     if sn_index%(c%) = 0% then L13445
                call "SERSTOVR" (sn_index%(c%), "2", "2", #26, #27)
                sn_delex% = sn_delex% + 1%
                sn_delex%(sn_delex%) = sn_index%(c%)
                sn_dpart$(sn_delex%) = part$(c%)
L13445:     if c% = maxlines% then L13475
                roll% = 1%
                for temp% = c% to maxlines% - 1%
                     gosub roll_lines
                next temp%
            c% = maxlines% : sn_index%(c%) = 0%
L13475:     gosub columnone
            maxlines% = maxlines% - 1%
            return

        roll_lines
                po$       (temp%) = po$       (temp%+roll%)
                poline$   (temp%) = poline$   (temp%+roll%)
                rcv$      (temp%) = rcv$      (temp%+roll%)
                part$     (temp%) = part$     (temp%+roll%)
                costtype$ (temp%) = costtype$ (temp%+roll%)
                store$    (temp%) = store$    (temp%+roll%)
                lot$      (temp%) = lot$      (temp%+roll%)
                venpart$  (temp%) = venpart$  (temp%+roll%)
                contract_id$  (temp%) = contract_id$  (temp%+roll%)
                contract_line$(temp%) = contract_line$(temp%+roll%)
                factor    (temp%) = factor    (temp%+roll%)
                qty       (temp%) = qty       (temp%+roll%)
                qtyhold(temp%,1%) = qtyhold(temp%+roll%,1%)
                qtyhold(temp%,2%) = qtyhold(temp%+roll%,2%)
                qty$      (temp%) = qty$      (temp%+roll%)
                price     (temp%) = price     (temp%+roll%)
                price$    (temp%) = price$    (temp%+roll%)
                ext$      (temp%) = ext$      (temp%+roll%)
                invcost   (temp%) = invcost   (temp%+roll%)
                cost$     (temp%) = cost$     (temp%+roll%)
                acct$     (temp%) = acct$     (temp%+roll%)
                varacct$  (temp%) = varacct$  (temp%+roll%)
                job$      (temp%) = job$      (temp%+roll%)
                oldseq$   (temp%) = oldseq$   (temp%+roll%)
                sn_index% (temp%) = sn_index% (temp%+roll%)
                sn_used%  (temp%) = sn_used%  (temp%+roll%)
                sn_config$(temp%) = sn_config$(temp%+roll%)
        return

        columnone
            po$(c%), poline$(c%) = " "
        columnone_special
            if sn_index%(c%) = 0% then L13650
                call "SERSTOVR" (sn_index%(c%), "2", "2", #26, #27)
                sn_delex% = sn_delex% + 1%
                sn_delex%(sn_delex%) = sn_index%(c%)
                sn_dpart$(sn_delex%) = part$(c%)
L13650:     sn_index%(c%), sn_used%(c%) = 0%
            acct$(c%), part$(c%), qty$(c%), ext$(c%), job$(c%), infomsg$,~
            errormsg$, price$(c%), store$(c%), lot$(c%), oldseq$(c%),    ~
            rcv$(c%), partdescr$, acctdescr$, strdescr$, jobdescr$,      ~
            venprice$, venqty$, factor$, venpart$(c%), sn_used_msg$,     ~
            invcost$, varacct$(c%), varacctdescr$, costtype$(c%),        ~
            varmsg$, jobtext$, contract_id$(c%), contract_line$(c%),     ~
            contract_descr$, vpc_potyp$                             = " "
            init (hex(00)) cost$(c%), sn_config$(c%)
            qty(c%),price(c%),factor(c%),qtyhold(c%,1%),qtyhold(c%,2%),  ~
                invcost(c%) = 0
            return

        REM *************************************************************~
            *      M I S C E L L A N E O U S   S U B R O U T I N E S    *~
            *************************************************************

        describe_line
                jobdescr$, strdescr$, sn_used_msg$, jobtext$ = " "
                if curr$ = "N" then L14060
                if currency$ = " " then currency$ = statutory$
                call "DESCRIBE" (#40, currency$, currdesc$, 1%, f1%(40%))
                   if f1%(40%) = 0% then currdesc$ = "(Unknown currency)"
L14060:         call "DESCRIBE" (#4, part$(c%), partdescr$, 1%, f1%(4%))
                   if f1%(4%) = 0% then partdescr$ = "(Non-Stocked Item)"
                if store$(c%) = " " then L14110
                call "DESCRIBE" (#8, store$(c%), strdescr$, 1%, f1%(8%))
                   if f1%(8%) = 0% then strdescr$=hex(94)&"(Not On File)"
L14110:         call "GETCODE" (#2, acct$(c%),acctdescr$, 1%,99, f1%(2%))
                   if f1%(2%)= 0% then acctdescr$=hex(94)&"(Not On File)"
                call "GETCODE" (#2, varacct$(c%), varacctdescr$, 1%,99,  ~
                     f1%(2%))
                if f1%(2%) = 0% then varacctdescr$=hex(94)&"(Not On File)"
                if sn_used%(c%) = 0% then L14160
                     put sn_used_msg$ using L14150, sn_used%(c%)
L14150:                   %Serial Numbers Used: #####
L14160:         if part$(c%) = " " or job$(c%) = " " then L14215
                call "DESCRIBE" (#13,job$(c%),jobdescr$,1%,f1%(13%))
                      if f1%(13%) = 1% then L14215
                call "DESCRIBE" (#11,job$(c%),jobdescr$,1%,f1%(11%))
                      if f1%(11%) = 1% then L14215
                jobdescr$ = hex(94) & "(Not On File)"
L14215:         gosub format_fields
                gosub set_varmsg
                gosub set_vpc_part_type
                vbkkey$ = str(vencode$) &str(po$(c%)) &str(poline$(c%))
                call "READ100" (#14, vbkkey$, f1%(14%))
L14223:              if f1%(14%) = 0% then L14230
                get #14, using L14225, tempa$, tempj$
L14225:         FMT POS(133), CH(9), POS(166), CH(8)
                call "GLFMT" (tempa$)
                jobtext$ = "GL:" & tempa$
                if tempj$ <> " " then jobtext$ = jobtext$ &              ~
                                                    ", Job/Prj:" & tempj$
L14230:         return

        total_up_invoice
            invamt, disc_amt = 0
            if maxlines% = 0% then L14330
            for i% = 1% to maxlines%
                temp = 0:convert ext$(i%) to temp, data goto L14310
                invamt = round(invamt + temp,2)
L14310:     next i%
            if invamt < 0 then L14370
L14330:     convert disc_amt$ to disc_amt, data goto L14340
L14340:     convert disc_pct$ to disc_pct, data goto L14350
L14350:     if disc_pct <> 0 then disc_amt =                             ~
                       round((invamt-min(invamt,nondisc))*disc_pct/100,2)
L14370:     call "CONVERT" (disc_amt, -2.2, disc_amt$)
            call "CONVERT" (invamt, -2.2, invtotal$)
            call "CONVERT" (invamt-disc_amt, -2.2, invnet$)
        return

        edit_text
            textmsg$ = "Vendor: " & vencode$ & ", Invoice: " & invoicenr$
            call "TXTINSUB" (#25, 0%, "011", textmsg$, text$, text$())
        return

        format_fields  /* Formats Numbers For Display */
            if factor(c%) = 0 then factor(c%) = 1
            call "CONVERT" (factor(c%), 0.4, factor$)
            call "CONVERT" (qty(c%),    0.4, qty$(c%))
            call "CONVERT" (price(c%),  2.7, price$(c%))
            ext$(c%) = " ":if qty(c%) = 0 and po$(c%) = " " then L14540
                call "CONVERT" (qty(c%)*price(c%), 2.2, ext$(c%))
L14540:     call "CONVERT" (invcost(c%),  2.4, invcost$)
            venqty    = round(qty  (c%)/factor(c%), 7)
            venprice  = round(price(c%)*factor(c%), 4)
            call "CONVERT" (venqty,   0.7, venqty$)
            call "CONVERT" (venprice, 2.4, venprice$)
        return

        set_varmsg
            varmsg$ = " "
            if invcost(c%) = round(conveqv * price(c%), 4) then return
            varmsg$ = hex(a4) & "Price/Cost variance:"
            call "CONVERT" (invcost(c%)-(conveqv*price(c%)),             ~
                                                 -2.4, str(varmsg$,23%))
            varmsg$ = varmsg$ & hex(8c)
        return

        view_vendor_contracts
            vpc_temp$   = contract_id$(c%)
            vpc_temp1$  = contract_line$(c%)
            call "VPCINQSB" (                                            ~
               vpc_temp$,                   /* Contract to Query       */~
               vpc_temp1$,                  /* Contract Line for Query */~
               #47,   /* VPCMASTR Vendor Purchases Contract Master File*/~
               #04,   /* HNYMASTR Inventory Master File                */~
               #20,   /* GENCODES System General Codes file.           */~
               #03,   /* VENDOR   VENDOR MASTER RECORD                 */~
               #25,   /* TXTFILE  System Text File                     */~
               #15,   /* VBKMASTR Purchase Order Master file           */~
               #14,   /* VBKLINES Purchase Order Lines file            */~
               #05,   /* PAYMASTR A/P Invoice Master file              */~
               #06,   /* PAYLINES A/P Invoice Lines file               */~
               #19,   /* RCVLINES Receiver Line Items File             */~
               f1%)                      /* Contract Found Status      */~
                                         /*  1 = Found, 2 = Not        */

               f1% = f1%
               return

        set_vpc_part_type
            vpc_potyp$ = " "
            if str(part$(c%),,9%) <> "ACTIVITY:" then L14875
                vpc_potyp$ = "A"
                return
L14875:     call "READ100" (#4, part$(c%), f1%(4%))
            if f1%(4%)  = 0% then vpc_potyp$ = "M"                       ~
                             else vpc_potyp$ = "P"
            return

        REM *************************************************************~
            *          W R I T E   D A T A   T O   B U F F E R          *~
            *                                                           *~
            * THIS ROUTINE DELETES THE OLD INVOICE FROM THE BUFFER IF   *~
            * THERE WAS ONE.  THEN IT GOES AND CALLS THE ROUTINE THAT   *~
            * WRITES THE NEW ONE OUT THERE.                             *~
            *************************************************************
        datasave

            if currdesc$ <> " " then L19074
                call "DESCRIBE" (#40, currency$, currdesc$, 1%, f1%(40%))
L19074:     u3% = 0%
            call "ASKUSER" (u3%, "**  CHECK INVOICE TOTAL  **",          ~
                            "Invoice total is  " &                       ~
                             invtotal$ & "  " & currdesc$,               ~
                            "Press RETURN if invoice total is correct",  ~
                            "or PF-1 if invoice total is NOT correct")
                if u3% = 1% then line_summary
                if u3% <> 0% then L19074

            payinpts% = 999%
            gosub call_payinpts_subroutine /* Action occurs in PAYINPTS */
            goto inputmode

L20000: REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   H E A D E R   I N F O  *~
            *                                                           *~
            * SETS DEFAULTS FOR THE FIRST PAGE OF THE HEADER--ADDRESS,  *~
            * INVOICE DATE, DATE TO PAY WITH AND WITHOUT DISCOUNTS, AND *~
            * THAT SORT OF THING.                                       *~
            *************************************************************

                  enabled% = 1%
                  if manual$ <> "PROC22" then L20140
                  call "ENABLSUB" ("SET", manual$, scr%(), set%(), 1%,   ~
                                          fieldnr%, abs(flag%), enabled%)
                  if edit% = 2% and enabled% = 0% then return
L20140:           message$ = " "
                  on fieldnr% gosub L20260,         /* VENDOR CODE      */~
                                    L20300,         /* INVOICE NUMBER   */~
                                    L20340,         /* DATE OF INVOICE  */~
                                    L20400,         /* REGULAR DATE     */~
                                    L20460,         /* DISCOUNT DATE    */~
                                    L20530,         /* DISC_PCT         */~
                                    L20600,         /* DISC_AMT         */~
                                    L20650          /* NON-DISCOUNTABLE */
                if mode$ = "OFF" or mode$ = "REC" then return
*       *****  IF AUTO% <> 0% AND FIELDNR% <> 2% THEN ENABLED% = 0%
                return
L20260:     REM ENABLE STUFF FOR VENDOR CODE
                message$ = "Enter Vendor Code, Blank To Search"
                return
L20300:     REM INPUT ENABLE FOR INVOICE NUMBER
                message$ = "Enter Vendor's Invoice Number."
                if mode$ = "REC" then message$ = "Enter Invoice Group"
                return
L20340:     REM INPUT ENABLE FOR INVOICE DATE
                if mode$ = "REC" then enabled% = 0%
                if mode$ = "REC" then return
                message$ = "Enter Date Of Invoice."
                if invdate$ = " " or invdate$ = blankdate$ ~
                                then invdate$ = paydate$
                return
L20400:     REM INPUT ENABLE FOR REGULAR DISBURSEMENT DATE
                if mode$ = "REC" then enabled% = 0%
                if mode$ = "REC" then return
                message$ = "Enter Date This Invoice Should Be Paid By."
                if regulardate$ = " " or regulardate$ = blankdate$ ~
                                                 then gosub L56310
                return
L20460:     REM INPUT ENABLE FOR DISCOUNT DISBURSEMENT DATE
                if mode$ <> "REC" and (discdate$ = " " or ~
                                       discdate$ = blankdate$) then gosub L56650
                message$ = "Enter Date This Invoice Would Have To Be Paid~
        ~ By To Get The Discount."
                if mode$ = "REC" then message$ = "Enter Date This Invoice~
        ~ Can't Be Cutover (Created) After."
                return
L20530:     REM ENABLE DISCOUNT PERCENT
                message$ = "Enter Discount Percentage, If Any (Eg. Enter ~
        ~'2' If a 2% Discount Is Possible)."
                if disc_pct = 0 then return
                if disc_pct$ <> " " then return
                call "CONVERT" (disc_pct, -2.4, disc_pct$)
                return
L20600:     REM ENABLE DISCOUNT AMOUNT
                if manual$ = "PROC22" then L20630
                     if editmode% = 0% then enabled% = 0%
L20630:         message$ = "Enter Total Discount For Invoice."
                return
L20650:     REM INPUT ENABLE FOR NON-DISCOUNTABLE AMOUNT
                message$ = "Enter Portion Of Total Invoice That Is Non Di~
        ~scountable."
                return

L21000: REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   H E A D E R   I N F O  *~
            *                                                           *~
            * SETS DEFAULTS FOR THE SECOND PAGE OF THE HEADER.          *~
            *************************************************************

                  enabled% = 1%
                  if manual$ <> "PROC22" then L21120
                  call "ENABLSUB" ("SET", manual$, scr%(), set%(), 2%,   ~
                                          fieldnr%, abs(flag%), enabled%)
                  if edit% = 2% and enabled% = 0% then return
L21120:           message$ = " "
                  on fieldnr% gosub L21230,         /* FREE TEXT FIELD  */~
                                    L21270,         /* 1099 CATEGORY    */~
                                    L21370,         /* PAYABLES ACCOUNT */~
                                    L21530,         /* HOLD STATUS      */~
                                    L21600,         /* PURCHASES ACCT   */~
                                    L21740          /* RECEIVER NUMBER  */
                if mode$ = "OFF" or mode$ = "REC" then return
                if auto% <> 0% then enabled% = 0%
                return

L21230:     REM INPUT ENABLE FOR FREE TEXT FIELD.
                message$ = "Free Text Field."
                return

L21270:     REM INPUT ENABLE FOR 1099 CATEGORY
                message$ = "Enter The Invoice 1099 Category."
                if ten99$ <> " " then return
                get #3, using L21310, ten99$
L21310:             FMT XX(514), CH(4)
                if ten99$ = " " then enabled% = 0%
                readkey$ = "1099 CATS" & ten99$
                call "DESCRIBE" (#20, readkey$, ten99descr$, 0%, f1%(20%))
                return

L21370:     REM INPUT ENABLE FOR PAYABLES ACCOUNT NUMBER
                message$ = "Enter The Accounts Payable Account Number."
                if payacct$ <> " " then return
                get #3, using L21420, payacct$
L21420:                 FMT XX(258), CH(9)
                if payacct$ = " " then payacct$ = sysacct$(3%)
                call "DESCRIBE" (#2, payacct$, payacctdescr$, 1%, f1%(2%))
                   if f1%(2%) <> 0% then L21480
                      payacct$, payacctdescr$ = " "
                      return
L21480:         get #2, using L21490, payaccttype$
L21490:             FMT XX(39), CH(1)
                call "GLFMT" (payacct$)
                return

L21530:     REM INPUT ENABLE FOR HOLD STATUS
                if mode$ = "REC" then enabled% = 0%
                if mode$ = "REC" then hold$ = " "
                message$ = "Enter 'Y' to Post This Invoice To Payables, B~
        ~ut Disallow Payment."
                return

L21600:     REM INPUT ENABLE FOR DEFAULT PURCHASES ACCOUNT NUMBER
                message$ = "Enter The DEFAULT Purchases Account Number Fo~
        ~r Lines On This Invoice."
                if puracct$ <> " " then return
                get #3, using L21650, puracct$
L21650:                 FMT XX(249), CH(9)
                if puracct$ = " " then puracct$ = sysacct$(1%)
                call "DESCRIBE" (#2, puracct$, puracctdescr$, 1%, f1%(2%))
                   if f1%(2%) <> 0% then L21710
                      puracct$, puracctdescr$ = " "
                      return
L21710:         call "GLFMT" (puracct$)
                return

L21740:     REM INPUT ENABLE FOR DEFAULT RECEIVER NUMBER
                if mode$ = "REC" then enabled% = 0%
                if mode$ = "OFF" then enabled% = 0%
                message$ = "To Auto Create Invoice From a Receiver, Enter~
        ~ The Receiver #, or 'ALL'"
                return

L22000: REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   T A B L E   I N P U T  *~
            *                                                           *~
            * SETS DEFAULTS FOR THE VARIOUS LINE ITEMS.                 *~
            *************************************************************

            enabled% = 1%
            if rcv$(c%) <> r41603fix$ then L22050
                enabled% = 0%
                return
L22050:     if manual$ <> "PROC22" then L22070
            call "ENABLSUB" ("SET", manual$, scr%(), set%(), 3%,         ~
                                          fieldnr%, abs(flag%), enabled%)
            if edit% = 2% and enabled% = 0% then return
L22070:     message$ = " "
            on fieldnr%  goto       L22160,         /* Currency code    */~
                                    L22190,         /* PO NUMBER & LINE */~
                                    L22230,         /* RECEIVER NUMBER  */~
                                    L22335,         /* INVENTORY PART # */~
                                    L22365,         /* STORE NUMBER     */~
                                    L22415,         /* LOT NUMBER       */~
                                    L22470,         /* VEN PART NUMBER  */~
                                    L23515,         /* Vendor Contract  */~
                                    L22490,         /* QUANTITY PER UNIT*/~
                                    L22555,         /* QUANTITY         */~
                                    L22585,         /* PRICE            */~
                                    L23240,         /* EXTENSION        */~
                                    L23300,         /* Inventory cost   */~
                                    L23350,         /* DEBIT ACCOUNT    */~
                                    L23400,         /* Cost variance    */~
                                    L23450          /* JOB NUMBER       */
            return

L22160
*        Set enable & default for Currency code
            if linemode% = 1% then enabled% = 0%
            if mode$ <> "REC" then L22165
                currency$ = statutory$
                enabled% = 0%
L22165:     if curr$ <> "Y" then enabled% = 0%
            if c% <> 1% then enabled% = 0%
            if maxlines% > 1% then enabled% = 0%
            if po$(c%) <> " " then enabled% = 0%
            if rcv$(c%) <> " " then enabled% = 0%
            message$ = "Enter Transaction Currency Code for this Docume"&~
                "nt. Blank = statutory."
            return

L22190
*        SET DEFAULT FOR PO NUMBER & LINE
            if linemode% <> 0% then enabled% = 0%  /* In edit mode */
            if mode$ = "REC"   then enabled% = 0%
            if mode$ = "OFF"   then enabled% = 0%
            message$ = "Enter PO and Line # If One Was Used.  Enter" &   ~
                       " Partial Value To See Open PO Lines."
            return

L22230
*        SET DEFAULT FOR RECEIVER NUMBER
            hits%, enabled% = 0%
            if po$(c%) = " "   then return
            if linemode% <> 0% then return  /* This would blow update */
            if keyhit% = 4% then L22310 /* PF 4 */
            REM Checkout whats out there for receiver numbers...
                readkey$ = str(vencode$) &str(po$(c%)) &str(poline$(c%))
                str(readkey$,29%) = all(hex(00))
L22270:         call "PLOWALTS" (#19, readkey$, 2%, 28%, f1%(19%))
                     if f1%(19%) = 0% then L22300
                if rcv$(c%) <> key(#19,0%) or rcv$(c%) = " "             ~
                                                  then hits% = hits% + 1%
                rcv$(c%) = key(#19,0%)
                goto L22270

L22300:         if hits% < 2% then return
                rcv$(c%) = " "   /* Show'em */
L22310:     enabled% = 1%
            message$ = "Enter Receiver Number If These Goods Have Been" &~
                       " Received. Blank To Search."
            return

L22335
*        Set default inventory PART default value
            if oldseq$(c%) <> " " then enabled% = 0%
            if po$(c%)     <> " " then enabled% = 0%
            message$ = "Enter The Part Number Purchased."
            return

L22365
*        Set default STORE NUMBER
            if mode$        = "REC" then enabled% = 0%
            if oldseq$(c%) <> " "   then enabled% = 0%
            if po$(c%)     <> " "   then enabled% = 0%
            if partdescr$ = "(Non-Stocked Item)" then enabled% = 0%
            if enabled% > 0% and store$(c%) = " " then                   ~
                                                     store$(c%) = defstr$
            message$ = "Enter Store To Place Goods In."
            return

L22415
*        Set default for LOT NUMBER
            if mode$       =  "REC" then enabled% = 0%
            if po$(c%)     <> " "   then enabled% = 0%
            if oldseq$(c%) <> " "   then enabled% = 0%
            message$ = "Enter The Lot Number To Place In, If Any."
            if enabled% = 0% then return
                call "LOTENABL" (part$(c%), lotenbl%, ll%, #7, #4)
*              IF LOTENBL% > 0% THEN ENABLED% = 1% ELSE ENABLED% = 0%
                enabled% = min(lotenbl%, 1%)
                return

L22470
*        Set default for VENDOR PART NUMBER
            message$ = "Enter Vendor Part Number, If Any."
            return

L22490
*        Set default value for QUANTITY PER UNIT
            if factor(c%) <> 0 then L22535
                call "READ100" (#16, str(part$(c%)) & str(vencode$) &    ~
                                             str(venpart$(c%)), f1%(16%))
                if f1%(16%) = 0% then L22540
                     get #16, using L22520, venprce, factor(c%)
L22520:                   FMT POS(69), PD(14,4), POS(93), PD(14,4)
                     if factor(c%) = 0 then factor(c%) = 1
                     price(c%)  = round(venprce/factor(c%),7)
L22535:     gosub format_fields
L22540:     message$ = "Enter Quantity Contained In One Unit."
            return

L22555
*        Set default QUANTITY
            call "SPCSMASH" (venqty$)
            call "SPCSMASH" (qty$(c%))
            message$ = "Enter The Quantity Purchased."
            return

L22585:     REM SET DEFAULT PRICE
                REM Get Amounts Off Receiver...
                readkey$ = str(rcv$(c%)) & str(vencode$)
                str(readkey$,26%) = str(po$(c%)) & str(poline$(c%))
                str(readkey$,45%) = all(hex(00))
                call "PLOWNEXT" (#19, readkey$, 44%, f1%(19%))
                     if f1%(19%) = 0% then L23190  /* anything goes */
                get #19, using L23060, current_price
L23060:         FMT POS(364), PD(14,7)

                REM See Whats Out There In Invoice File...
                readkey$= str(rcv$(c%)) & str(po$(c%)) & str(poline$(c%))
L23100:         call "PLOWALTS" (#6, readkey$, 1%, 35%, f1%(6%))
                     if f1%(6%) = 0% then L23160
                if str(readkey$,45%,16%) = invoicenr$ then L23100
                     enabled% = 0%
                     return

L23160:         if price$(c%) <> " " then L23190
                     price(c%)  = current_price
                     gosub format_fields
L23190:         REM Edit Allowed...
                call "SPCSMASH" (price$(c%))
                call "SPCSMASH" (venprice$)
                message$ = "Enter The Non Discounted Price (Each)."
                return
L23240:     REM SET DEFAULT EXTENSION VALUE
                if linemode% = 0% then L23265
                readkey$= str(rcv$(c%)) & str(po$(c%)) & str(poline$(c%))
                call "PLOWALTS" (#6, readkey$, 1%, 35%, f1%(6%))
                     if f1%(6%) = 0% then L23270
                if str(readkey$,45%,16%) = invoicenr$ then L23270
L23265:              enabled% = 0%
                     return
L23270:         if enabled% = 1% then call "SPCSMASH" (ext$(c%))
                message$ = "Enter The Non Discounted Extension For This L~
        ~ine Of The Invoice."
                return

L23300: REM Get Inventory cost values via HNYCDIST
            f$ = " "
            if flag% <> 1% then L23325
               if po$(c%) <> " " then L23336
                     if postatstdcost$ <> "Y" or                         ~
                              pos("FST" = costtype$(c%)) = 0% then L23315
                          gosub call_std_cost
                          goto L23320
L23315:            temp = price(c%) * conveqv
                   call "CONVERT" (temp,  2.4, invcost$)
L23320:            f$ = "I"
L23325:     call "HNYCDIST" (f$, part$(c%), partdescr$, "PAYINPUT: " &   ~
                "Invoice Line Item: Inventory Cost Distribution",        ~
                #7, cost$(c%), invcost$, invcost(c%))
            if flag% <> 1% then L23336
            if f$ = "E" then enabled% = 0%
L23336:     gosub set_varmsg
            return

L23350:     REM SET DEFAULT VALUE FOR DEBIT ACCOUNT
                if acct$(c%) = " " then acct$(c%) = defaultacct$
                if mode$ = "REC" or partdescr$ = "(Non-Stocked Item)"    ~
                    then L23370
                if oldseq$(c%) <> " " then enabled% = 0%
                if po$(c%) <> " " then enabled% = 0%
L23370:         message$ = "Enter the G/L Purchases Account Number."
                return

L23400:     REM SET DEFAULT VALUE FOR Cost variance account
                if varacct$(c%) <> " " then L23422
                   get #3, using L23406, varacct$(c%)
L23406:                FMT POS(318), CH(9)
                   call "GLFMT" (varacct$(c%))
                if varacct$(c%) <> " " then L23422
                   call "READ100" (#7, "DEFAULTS.STORE."& str(defstr$),  ~
                                                                f1%(7%))
                      if f1%(7%) = 0% then L23418
                   get #7 using L23416, varacct$(c%)
L23416:                FMT POS(162), CH(9)
                   call "GLFMT" (varacct$(c%))
L23418:         if varacct$(c%) <> " " then L23422
                   varacct$(c%) = sysacct$(6%)
                   call "GLFMT" (varacct$(c%))
L23422:         if mode$ = "REC" or partdescr$ = "(Non-Stocked Item)"    ~
                    then L23438
                if oldseq$(c%) <> " " then enabled% = 0%
                if po$(c%) <> " " then enabled% = 0%
L23438:         message$ = "Enter the Price Cost Variance Account"
                return

L23450
*        Set default JOB (PROJECT) NUMBER VALUE.
*          IF JOB$(C%) <> " " THEN RETURN
*          IF MODE$ = "REC"   THEN ENABLED% = 0%
                call "READ100" (#21, part$(c%), f1%(21%))
                if f1%(21) <> 0% then enabled% = 0%
                message$ = "Enter Job or Project Number.  Enter Partial"&~
                           " Value To Search Files."
                if po$(c%) <> " " then enabled% = 0%
                return

L23515
*        Default / Enable for Purcahse Contract ID
            if vpc_open% =   1%  then L23555   /* File is OPEN */
                enabled% = 0%  :  return      /* File is Not OPEN */

L23555:     message$ = "Enter Purchase Contract ID and Line Number."

            return

        REM *************************************************************~
            * Get Standard Cost for Standard Costed Parts and Check for *~
            * Reman Parts.  If Reman, Cost is sum of Reman's Standard   *~
            * Cost and the Reman's Core's Standard Cost.                *~
            *************************************************************
        call_std_cost
            mat cost = zer  :  invcost(c%) = 0
            call "STCCOSTS" (part$(c%), " ", #7, 1%, invcost(c%))
            if core_on% <> 1% then L25170
                init (" ")  remanpart$
                remanpart$ = part$(c%)
                call "PLOWALTS" (#46, remanpart$, 0%, 25%, reman%)
                     if reman% <> 1% then L25170
                     if str(remanpart$,26%,25%) = " " then L25170
                call "STCCOSTS" (str(remanpart$,26%,25%), " ", #7, 1%,   ~
                                                                   core)
                invcost(c%) = invcost(c%) + core
L25170:     call "CONVERT" (invcost(c%), 2.4, invcost$)
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: /* Allow user opportunity to start over.            */
            startover% = 2%
            call "STARTOVR" (startover%)
            if startover% = 1% then return

            if delmax% = 0% then L29190
                for i% = 1% to delmax%
                   call "HNYHOLD" (#12, delpart$(i%), delstore$(i%),     ~
                                      dellot$(i%), - delqty(i%), return%)
                next i%
L29190:     call "SERSTOVR" (0%, "2", "2", #26, #27)
            readkey$ = str(vencode$) & str(invoicenr$)
            call "REDALT0" (#9, readkey$, 1%, f1%(9%))
            if f1%(9%) = 0% then call "DELETE" (#29, readkey$, 25%)
            call "READ101" (#17, readkey$, f1%(17%))
            if f1%(17%) = 0% then L29270
                get #17 using L29240, readkey$
L29240:              FMT XX(25), CH(3)
                if readkey$ = userid$ then delete #17
L29270:     return clear all
            goto inputmode

        init_data
            init(" ")                                                    ~
            invcost$, varacct$(), varacctdescr$, sn_loc$, sn_trankey$,   ~
            vendescr$, venaddr$(), po$(), invdate$, puracct$, origdate$, ~
            puracctdescr$, payacct$, payacctdescr$, discdate$, invtype$, ~
            regulardate$, nondisc$, acct$(), part$(), qty$(), freetext$, ~
            job$(), errormsg$, receiver$, oldseq$(), price$(), disc_amt$,~
            ten99$, ten99descr$, store$(), lot$(), disc_pct$, poline$(), ~
            hold$, rcv$(), ext$(), origuserid$, groupdescr$, text$,      ~
            venprice$, venqty$, factor$, venpart$(), po$, sn_source$,    ~
            tmp_config$, sn_dpart$(), currency$, currdesc$, convdate$,   ~
            costtype$(), contract_id$(), contract_line$()
            maxlines%, editmode%, disc_pct, disc_amt, invamt, auto% = 0
            delmax% = 0%
            call "ALLFREE"

            init(hex(00)) sn_config$(), cost$()
            mat invcost   = zer  :  mat cost = zer
            mat sn_index% = zer  :  mat sn_used% = zer
            mat sn_delex% = zer  :  sn_delex% = 0%
            mat qty = zer  : mat price = zer : mat factor = zer
            mat qtyhold = zer
            call "TXTFUTIL" (#25, 0%, "INTL", text$)
            conveqv, convunt = 1
        return

        REM *************************************************************~
            * D E F A U L T  /  E N A B L E   R O U T I N E S           *~
            * --------------------------------------------------------- *~
            * Routines for handling default enable switches.            *~
            *************************************************************

        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            if manual$ <> "PROC22" then return  /* Only one mode */
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
            scr%(1%,1%) =  1% : set%(1%) = 13%    /* Vendor Code      */
            scr%(1%,2%) =  2% : set%(2%) = 13%    /* Invoice Number   */
            scr%(1%,3%) =  3% : set%(3%) =  2%    /* Invoice Date     */
            scr%(1%,4%) =  4% : set%(4%) =  2%    /* Regular Date     */
            scr%(1%,5%) =  5% : set%(5%) =  2%    /* Discount Date    */
            scr%(1%,6%) =  6% : set%(6%) =  2%    /* Discount Percent */
            scr%(1%,7%) =  7% : set%(7%) =  1%    /* Discount Amount  */
            scr%(1%,8%) =  8% : set%(8%) =  2%    /* Non Disc Amount  */

            scr%(2%,1%) =  9% : set%( 9%) =  2%    /* Free Text Field  */
            scr%(2%,2%) = 10% : set%(10%) =  2%    /* 1099 Category    */
            scr%(2%,3%) = 11% : set%(11%) =  2%    /* Payables Account */
            scr%(2%,4%) = 12% : set%(12%) =  2%    /* Hold Status      */
            scr%(2%,5%) = 13% : set%(13%) =  2%    /* Purch Account    */
            scr%(2%,6%) = 14% : set%(14%) =  2%    /* Receiver Number  */

            scr%(3%, 1%) = 29% : set%(29%) =  2%  /* Currency code     */
            scr%(3%, 2%) = 15% : set%(15%) = 13%  /* PO Number & Line  */
            scr%(3%, 3%) = 16% : set%(16%) =  1%  /* Receiver Number   */
            scr%(3%, 4%) = 17% : set%(17%) =  2%  /* Part Number       */
            scr%(3%, 5%) = 18% : set%(18%) =  2%  /* Store Number      */
            scr%(3%, 6%) = 19% : set%(19%) =  2%  /* Lot Number        */
            scr%(3%, 7%) = 20% : set%(20%) =  2%  /* Vndr Part Number  */
            scr%(3%, 8%) = 30% : set%(30%) =  2%  /* Contract ID & Line*/
            scr%(3%, 9%) = 21% : set%(21%) =  2%  /* Quantity Per Unit */
            scr%(3%,10%) = 22% : set%(22%) =  2%  /* Quantity          */
            scr%(3%,11%) = 23% : set%(23%) =  2%  /* Price             */
            scr%(3%,12%) = 24% : set%(24%) =  2%  /* Extension         */
            scr%(3%,13%) = 25% : set%(25%) =  2%  /* Inventory cost    */
            scr%(3%,14%) = 26% : set%(26%) =  2%  /* Account Number    */
            scr%(3%,15%) = 27% : set%(27%) =  2%  /* Cost variance     */
            scr%(3%,16%) = 28% : set%(28%) =  2%  /* Job Number        */
*        Next available ^^% number is 31.

            call "ENABLSUB" ("INIT", manual$, scr%(), set%(),0%,0%,0%,0%)
            return

L30000: REM *************************************************************~
            *     L O A D   O L D   I N V O I C E   O F F   F I L E     *~
            * --------------------------------------------------------- *~
            *    PLOWS THROUGH BUFFER AND THEN THROUGH PAYABLES MASTER  *~
            * FILE LOOKING FOR THE GIVEN INVOICE.  IF ON FILE, LOAD AND *~
            * EDIT, OTHERWISE, JUST MAKE NEW INVOICE.                   *~
            *************************************************************

*        Prepare To Search Data Files...
            oldinvoiceonfile%, maxlines%, sn_last_index% = 0%
            readkey$ = str(vencode$) & str(invoicenr$)
L30055:     write #17 using L30060, readkey$, userid$, eod goto L30070
L30060:         FMT CH(25), CH(3)
            goto L30110
L30070:         call "READ100" (#17, readkey$, f1%(17%))
                if f1%(17) = 0% then L30055
                     get #17 using L30085, errormsg$
L30085:                   FMT XX(25), CH(3)
                     if errormsg$ = userid$ then L30110
                          errormsg$ = "Invoice is being modfified by:" & ~
                                      errormsg$
                          return
L30110:     mat sn_index% = zer  : mat sn_delex% = zer : sn_delex% = 0%
            init(hex(00)) sn_config$()
            errormsg$ = " "
            if mode$ = "REC" then L30180  /* Better not be in buffer! */

*        First Try To Read Header Record From Buffer...
            file% = 10% : lcur% = 42%
            call "REDALT0" (#9, readkey$, 1%, f1%(9%))
            if f1%(9%) = 0% then L30180
                get #9, using L30160, str(lot$(),,200%), str(lot$(),201%)
L30160:              FMT POS(11), CH(200), CH(140)
                call "SHOSTAT" ("Loading Invoice from Buffer")
                goto L30220

L30180
*        Not In Buffer, So Try Master File.
            file% = 6% : lcur% = 41%
            call "READ100" (#5, readkey$, f1%(5%))
                if f1%(5%) = 0% then L30197            /* Check CSHLINES */
            get #5, str(lot$(),,340%)
            if mode$="REC" then call "SHOSTAT" ("Loading Data") else     ~
                call "SHOSTAT" ("Loading Invoice From Master File")
            goto L30220

L30197
*        Not In Master, Try CSHLINES...
            readkey$ = str(invoicenr$) & hex(00)
            call "REDALT0" (#23, readkey$, 1%, f1%(23%))
                goto L30202
L30201:     call "READNEXT" (#23, f1%(23%))
L30202:         if f1%(23%) = 0% then L30840                   /* Return */
            get #23 using L30204, temp1$, temp2$
L30204:         FMT CH(9), XX(11), CH(16)
            if temp2$ <> invoicenr$ then L30840                /* Return */
            if temp1$ <> vencode$   then L30201                /* Return */

L30208:     u3% = 0%
            call "ASKUSER" (u3%, "**** PAYMENTS ON FILE ****",           ~
                 "Invoice " & invoicenr$ & " currently has payments on "&~
                 "file.", "Press RETURN to continue with this number -O"&~
                 "R-", "PF(16) to re-enter the invoice number.")
            if u3% = 0% then L30840
                if u3% <> 16% then L30208
                    errormsg$ = "Invoice Has Payment(s) On File"
                    readkey$ = str(vencode$) & str(invoicenr$)
                    call "DELETE" (#17, readkey$, 25%)
                    return

L30220
*        Actually get And Format Data Off Header...
            get str(lot$(),,340%), using L30945, receiver$, invdate$,     ~
                    puracct$, payacct$, payaccttype$, regulardate$,      ~
                    discdate$, nondisc, origdate$, origuserid$,          ~
                    freetext$, disc_pct, disc_amt, ten99$, hold$,        ~
                    invtype$, text$, currency$
            if currency$ = " " then currency$ = statutory$
            call "TXTFUTIL" (#25, 0%, "LOAD", text$)
            oldinvoiceonfile% = 1%
            lot$() = " "

            call "DESCRIBE" (#2, puracct$, puracctdescr$, 1%, f1%(2%))
                call "GLFMT" (puracct$)
                defaultacct$ = puracct$
            call "DESCRIBE" (#2, payacct$, payacctdescr$, 1%, f1%(2%))
                call "GLFMT" (payacct$)
            call "DATEFMT"  (invdate$)
            call "DATEFMT"  (regulardate$)
            if discdate$ = blankdate$ then discdate$ = "00/00/00" else ~
               call "DATEFMT"  (discdate$)
*          CALL "CONVERT" (NONDISC, -2.4, NONDISC$)
            if disc_pct <> 0 then call "CONVERT"(disc_pct,-2.4,disc_pct$)
*          CALL "CONVERT"(DISC_AMT, -2.2, DISC_AMT$)
            if mode$ = "REC" then hold$ = " "

            if ten99$ = " " then L30350
                 readkey$ = "1099 CATS" & ten99$
                 call "DESCRIBE"(#20,readkey$,ten99descr$,0%,f1%(20%))
L30350:     if mode$ <> "REC" then L30375
                 readkey$ = "APRECGRPS" & str(invoicenr$,2%,4%)
                 call "DESCRIBE"(#20,readkey$,groupdescr$,0%,f1%(20%))
                 if f1%(20%) <> 0% then call "PUTPAREN" (groupdescr$)

L30375
*        Load Up And Format Line Item Details...
            readkey$ = vencode$
            str(readkey$,10%) = invoicenr$
L30390:     call "PLOWNEXT" (#file%, readkey$, 25%, f1%(file%))
            if f1%(file%) = 0% then L30560  /* Return */

            c%, maxlines% = maxlines% + 1%
            get #file%, using L31060, rcv$(c%), po$(c%), poline$(c%),     ~
                        seqnr$, acct$(c%), part$(c%), qty(c%), ext,      ~
                        job$(c%), store$(c%), lot$(c%), price(c%),       ~
                        oldseq$(c%), factor(c%), venpart$(c%),           ~
                        sn_index%(c%), invcost(c%), cost(), varacct$(c%),~
                        contract_id$(c%), contract_line$(c%)
            call "READ100" (#4, part$(c%), f1%(4%))
            if f1%(4%) <> 1% then L30435
                get #4, using L30434, costtype$(c%)
L30434:              FMT POS(307), CH(1)
L30435:     if curr$ <> "Y" then goto L30451
            if currency$ = statutory$ then goto L30451
            if currency$ = " " then goto L30451
            call "READ100" (#lcur%, readkey$, f1%(lcur%))
            if f1%(lcur%) = 0% then goto L30451
                get #lcur% using L30445, ext, price(c%), convdate$,       ~
                    conveqv, convunt
L30445:         FMT POS(33), PD(14,4), PD(14,7), CH(6), 2*PD(14,7)
                if c% = 1% then call "DESCRIBE" (#40, currency$,         ~
                    currdesc$, 1%, f1%(40%))
L30451:     put cost$(c%) using L30452, cost()
L30452:         FMT 12*PD(14,4)
            if file% = 6% then oldseq$(c%) = seqnr$
            if sn_index%(c%) > sn_last_index% then                       ~
                                           sn_last_index% = sn_index%(c%)

            if mode$ = "REC"   or mode$ = "OFF"  then L30525
            if rcv$(c%) <> " " or po$(c%) <> " " then L30525
                qtyhold(c%,2%) = qty(c%)
                if file% = 10% then L30495
                     qtyhold(c%,1%) = qty(c%)
                     goto L30525
L30495:         plowkey$ = str(readkey$,,25%) & str(oldseq$(c%),,3%)
                call "READ100" (#6, plowkey$, f1%(6%))
                if f1%(6%) = 0% then L30525
                     get #6, using L30515, qtyhold(c%,1%)
L30515:                   FMT POS(98), PD(14,4)

L30525:     call "CONVERT" (price(c%), 2.7, price$(c%))
            call "CONVERT" (qty(c%), 0.2, qty$(c%))
            call "CONVERT" (ext, 2.2, ext$(c%))
            call "GLFMT" (acct$(c%))
            call "GLFMT" (varacct$(c%))
            goto L30390

L30560
*        Now load up deletes for 2nd edit
            nondisc  = nondisc  * convunt
            disc_amt = disc_amt * convunt
            call "CONVERT"(disc_amt, -2.2, disc_amt$)
            call "CONVERT" (nondisc, -2.4, nondisc$)
            if file% = 6% then L30660

                init (hex(00)) str(readkey$,26%)

L30585:         call "PLOWNEXT" (#6, readkey$, 25%, f1%(6%))
                   if f1%(6%) = 0% then L30660
                get #6, using L30600, temp$, temp
L30600:             FMT POS(61), CH(3), POS(98), PD(14,4)
                if temp <= 0 then L30585
                search str(oldseq$()) = str(temp$,,3%) to cursor%() step 3
                  if cursor%(1%) <> 0% then L30585
                delmax% = delmax% + 1%
                get #6 using L30635, delpart$(delmax%),delstore$(delmax%),~
                                    dellot$(delmax%)
L30635:             FMT POS(73), CH(25), POS(122), CH(3), CH(6)
                delhold(delmax%) = temp
                delqty (delmax%) = 0
                goto L30585

L30660
*        Check if edit allowed, total invoice, & return...
            i$(1%) = " "
            if maxlines% = 0% then L30775
            for i% = 1% to maxlines%
                if rcv$(i%) = " " then L30765
                readkey$ = str(rcv$(i%)) & str(po$(i%))& str(poline$(i%))
                gosub test_receiver_usage
                if errormsg$ = " " then L30765

              REM Failed, move to rear of class...
                i$(1%) = errormsg$  /* ERRORMSG$ gets it by COLUMNONE */
                if str(i$(1%),,8%) <> "Sorry, T" then L30740
                i$(1%) = "Sorry, Can't Edit Invoice Until Invoice "      ~
                                                & str(readkey$,45%,16%)
                    i$(1%) = i$(1%) & " Is Posted."
                    goto L30760
L30740:             if str(i$(1%),,8%) <> "Sorry, R" then L30760
                    i$(1%) = "Sorry, Can't Edit Invoice Until Receiver " ~
                                                               & rcv$(i%)
                    i$(1%) = i$(1%) & " Is Posted."
L30760:             i% = maxlines%
L30765:         next i%

L30775
*        Check other possible edit restrictions...
*        Note that I$(1%) may already be set from above code
            if invtype$ = "A" then i$(1%) =                              ~
                              "Adjustment Invoice, Can't Edit From Here."
            if invtype$ = "R" and mode$ <> "REC" then i$(1%) =           ~
                        "Recurring Master Invoice, Can't Edit From Here."
            if invtype$ = "O" and mode$ <> "OFF" then i$(1%) =           ~
                                 "Offline Invoice, Can't Edit From Here."
            if invtype$ <> "R" and mode$ = "REC" then i$(1%) =           ~
                "Only Recurring Master Invoices Can be Edited From Here."
            if i$(1%) <> " " then gosub init_data
            if i$(1%) <> " " then errormsg$ = i$(1%)

L30840:     gosub total_up_invoice
            if maxlines% = 0% or errormsg$ <> " " or mode$ = "R"         ~
                                                              then return

*        Load Serial Numbers for this Invoice
            if file% = 6% then sn_source$ = "h" else sn_source$ = " "
            for c% = 1% to maxlines%
              if sn_index%(c%) = 0% then L30930
                sn_trankey$ = str(vencode$) & invoicenr$
                convert sn_index%(c%)to str(sn_trankey$,26%,4%), pic(0000)
                sn_loc$ = str(store$(c%),,3%) & lot$(c%)
                call "SERLOAD" (sn_index%(c%), "VT", sn_trankey$, 25%,   ~
                                sn_source$, sn_loc$, #7, #28, #26, #27,  ~
                                sn_used%(c%), #29)
                sn_used%(c%)   = abs(qty(c%)) - sn_used%(c%)
                sn_config$(c%) = str(part$(c%)) & str(store$(c%)) &      ~
                                 str(lot$(c%),,6) & "+"
                if qty(c%) < 0 then str(sn_config$(c%),35%,1%) = "-"
L30930:     next c%
            return

L30945:     FMT /* File #5 (PAYMASTR) & file #9 (PAYBUFFR) rec layout  */~
                XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* INVOICE NUMBER             */~
                CH(16),                  /* RECEIVER NUMBER            */~
                CH(6),                   /* INVOICE DATE               */~
                CH(9),                   /* PURCHASES ACCOUNT          */~
                CH(9),                   /* PAYABLES ACCOUNT           */~
                CH(1),                   /* PAYABLES ACCOUNT TYPE (A,E)*/~
                CH(6),                   /* PAY W/O DISCOUNT DATE      */~
                CH(6),                   /* PAY W/DISCOUNT DATE        */~
                PD(14,4),                /* NON-DISCOUNTABLE AMOUNT    */~
                XX(6),                   /* LAST POSTING DATE          */~
                CH(6),                   /* ORIGINAL INPUT DATE        */~
                CH(3),                   /* ORIGINAL INPUT BY          */~
                XX(25),                  /* AUDIT INFO AND TOTALS      */~
                CH(20),                  /* FREE TEXT FIELD            */~
                PD(14,4),                /* DISCOUNT PERCENT           */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(1),                   /* HOLD STATUS                */~
                CH(1),                   /* INVOICE TYPE (INTERNAL)    */~
                CH(4),                   /* TEXT ID NUMBER             */~
                CH(4)                    /* Currency code              */

L31060:     FMT /*  File #6 or #10- PAYLINES or PAYBUF2 (#FILE%) input */~
                CH(16),                  /* RECEIVER NUMBER            */~
                CH(16),                  /* PO NUMBER                  */~
                CH(03),                  /* PO LINE NUMBER             */~
                XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* INVOICE NUMBER             */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                CH(25),                  /* PART NUMBER                */~
                PD(14,4),                /* QUANTITY                   */~
                PD(14,4),                /* EXTENSION                  */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(3),                   /* STORE NUMBER               */~
                CH(6),                   /* LOT NUMBER                 */~
                PD(14,7),                /* UNIT PRICE                 */~
                CH(3),                   /* OLD SEQUENCE NUMBER        */~
                PD(14,4),                /* Quantity Per Vendor Unit   */~
                XX(4),                   /* Unit Of Measure            */~
                CH(25),                  /* Vendors Part Number        */~
                BI(4),                   /* Serial Number X-ref Index  */~
                PD(14,4),                /* Total Inventory cost       */~
                12*PD(14,4),             /* Inventory cost array       */~
                CH(9),                   /* Cost variance account #    */~
                CH(16),                  /* Contract Id                */~
                CH(04)                   /* Contract Line Number       */

        plow_rcvlines
            mat inc = zer : init (" ") inc$()
          header$(3%) = hex(a4) &                                        ~
                          "  Reveiver Number " & rcv$(c%) & " For" &     ~
                          " Vendor " & vencode$
          header$(1%) = hex(a4) &                                        ~
                        " PO Number        Line#  Qty Rcv   Rcv Hld  "  &~
                        "     QC    QC Hld   Store  Lot"

            descr_m(01%) =   51.16  : descr_m(02) = 0001.0
            descr_m(03%) =   67.03  : descr_m(04) = 0018.0
            descr_m(05%) =  156.08  : descr_m(06) = 0024.0841
            descr_m(07%) =  164.08  : descr_m(08) = 0034.0841
            descr_m(09%) =  172.08  : descr_m(10) = 0043.0841
            descr_m(11%) =  180.08  : descr_m(12) = 0053.0841
            descr_m(13%) =  332.03  : descr_m(14) = 0065.0
            descr_m(15%) =  335.06  : descr_m(16) = 0071.0
            inc(1%) = 51.16         : inc$(1%) = po$(c%)
            plowkey$ = xor plowkey$
            str(plowkey$,,16%) = str(readkey$,26%,16%)
             call "PLOWCODE" (#19, plowkey$, "   ",   9016%,  0.3,       ~
              f1%(19%),header$(), 0, 0,inc(),inc$(),"D","Y",#55,descr_m())
            mat inc = zer : init (" ") inc$(), header$()
            return

        REM *************************************************************~
            *          A U T O   G E N E R A T E   I N V O I C E        *~
            *                                                           *~
            * SEARCHES THROUGH RECEIVER MASTER FILE FOR UNINVOICED      *~
            * RECEIPTS THIS RECEIVER.  USES INVOICE OPEN AMOUNT TO      *~
            * DETERMINE WHETHER OR NOT THE LINE IS STILL OPEN.          *~
            *************************************************************

        load_complete_po:
                message% = 0%
                errormsg$ = " "
                plowkey$ = str(vencode$) & str(po$) & hex(000000)
                plow% = 2%
                goto L36280

        load_complete_receiver:
                plow%, message% = 0%
                errormsg$ = " "
                if receiver$ <> "ALL" then L36220
                     plowkey$ = vencode$
                     plow% = 1%
                     goto L36280
L36220:         plowkey$ = str(receiver$) & str(vencode$)
                readkey$ = str(receiver$)
                REM Is receiver Being Editted?...
                gosub test_receiver_usage
                if errormsg$ <> " " then return

L36280:         REM Everything for a given Receiver option...
                on plow% goto L36380, L36470
L36300:         call "PLOWNEXT" (#19, plowkey$, 25%, f1%(19%))
                     if f1%(19%) <> 0% then L36330
                     errormsg$ = " " : return
L36330:         readkey$ = str(plowkey$,,16%) & str(plowkey$,26%,19%)
                errormsg$ = " " : gosub test_receiver_usage
                if errormsg$ <> " " then L36300
                goto L36550

L36380:         REM 'All' option...
L36390:         call "PLOWALTS" (#19, plowkey$, 2%, 9%, f1%(19%))
                     if f1%(19%) <> 0% then L36420
                     errormsg$ = " " : return
L36420:         readkey$ = str(key(#19,0%),,16%) &                       ~
                                                 str(key(#19,0%),26%,19%)
                errormsg$ = " " : gosub test_receiver_usage
                if errormsg$ <> " " then L36390
                goto L36550

L36470:         REM Everything for a given P.O. option...
L36480:         call "PLOWALTS" (#19, plowkey$, 2%, 25%, f1%(19%))
                     if f1%(19%) <> 0% then L36510
                     errormsg$ = " " : return
L36510:         readkey$ = str(key(#19,0%),,16%) &                       ~
                                                 str(key(#19,0%),26%,19%)
                errormsg$ = " " : gosub test_receiver_usage
                if errormsg$ <> " " then L36480

L36550:         REM Logic here on is common...
                get #19, using L36570, uninvoiced
L36570:         FMT POS(356), PD(14,4)
                if uninvoiced = 0 then L36280

                if message% <> 0% then L36630
                     call "SHOSTAT" ("Loading Open Receiver Data")
                     message% = 1%
L36630:         c%, maxlines% = maxlines% + 1%
                gosub load_receiver_line
                gosub test_for_dups
                if errormsg$ = " " then L36700
                     gosub delete_it
                     errormsg$ = " "
                     goto L36280
L36700:         if c% = lines_allowed% then return
                goto L36280

        test_receiver_usage:     /* Note: READKEY$ must already be set */
                REM Insure receiver line is not in Payables buffer...
L36750:         call "PLOWALTS" (#10, readkey$, 1%, 35%, f1%(10%))
                     if f1%(10%) = 0% then L36830
                if str(readkey$,45%,16%) = invoicenr$ then L36750
                errormsg$ = "Sorry, This Receiver/PO Line Has An A/P Inv"~
                  & "oice In Process. (Invoice: " & str(readkey$,45%,16%)
                errormsg$ = errormsg$ & ")"
                return

L36830:         REM Insure receiver is not in receiver buffer...
                str(readkey$,17%) = " "
                call "PLOWNEXT" (#18, readkey$, 16%, f1%(18%))
                     if f1%(18%) = 0% then return
                errormsg$ = "Sorry, Receiver Is Currently Being Edited."
                return

        load_receiver_line:
                get #19, using L37000, part$(c%), rcv$(c%), po$(c%),      ~
                     poline$(c%), qty_inqc(c%), qty_qchold(c%),          ~
                     factor(c%), ext, acct$(c%), qty(c%), price(c%),     ~
                     job$(c%), invcost(c%), cost$(c%)
                if qty(c%) = 0 then L36931
                if qtytopay$ <> "Y" then L36931
                     qty(c%) = max(0, qty(c%) - qty_inqc(c%) -           ~
                                                        qty_qchold(c%))
L36931:         currency$ = statutory$   /* In case of no hit below */
                call "READ100" (#44, key(#19), f1%(44%))
                   if f1%(44%) = 0% then L36940
                get #44 using L36937, currency$, price(c%), ext, conveqv, ~
                                     convunt, convdate$
L36937:             FMT CH(4), POS(57), PD(14,7), PD(14,4), PD(14,7),    ~
                                        PD(14,7), CH(6)
L36940:         call "CONVERT" (qty(c%), 0.2, qty$(c%))
                   ext = price(c%) * qty(c%)
                call "CONVERT" (ext, 2.2, ext$(c%))
                call "CONVERT" (price(c%), 2.7, price$(c%))
                call "GLFMT" (acct$(c%))
                vbkkey$ = str(vencode$) &str(po$(c%)) &str(poline$(c%))
                call "READ100" (#14, vbkkey$, f1%(14%))
                     if f1%(14%) = 0% then return
                get #14, using L36982, venpart$(c%), varacct$(c%)
L36982:              FMT POS(197), CH(25), POS(525), CH(9)
                call "GLFMT" (varacct$(c%))
              /* Check for Vendor Contracts */
                readkey$ = str(vencode$) &str(po$(c%)) &str(poline$(c%))
                call "READ100" (#14, readkey$, f1%(14%))
                     if f1%(14%) = 0% then L36995     /* Return */
                get #14, using L36994, contract_id$(c%), contract_line$(c%)
L36994:               FMT  POS(561), CH(16), CH(4)
L36995: return

L37000:     FMT   /* File #19- RCVLINES- Receiver Line Item            */~
                CH(25),                  /* Part Number                */~
                CH(16),                  /* Receiver Number            */~
                POS(51), CH(16),         /* P.O. Number (PIPIN Tag)    */~
                CH(3),                   /* P.O. Line Sequence Number  */~
                POS(172), PD(14,4),      /* Quantity in QC             */~
                PD(14,4),                /* Quantity in QC HOLD        */~
                POS(236), PD(14,4),      /* Conversion Factor          */~
                POS(256), PD(14,4),      /* Open Amount                */~
                POS(296), CH(9),         /* (Pre) Payables Liab. Acct. */~
                POS(356), PD(14,4),      /* Open (Uninvoiced) Quantity */~
                PD(14,7),                /* A/P Price                  */~
                POS(400), CH(8),         /* Job/Proj Number            */~
                POS(512),PD(14,4),CH(96) /* Total PO cost & breakdown  */

        test_for_dups
                REM Look For Dups...
                errormsg$ = " "
                f% = 1%  /* Get Things Started */
L37530:         mat search% = zer
                search str(rcv$(),f%) = rcv$(c%) to search%() step 16%
                for i% = 1% to 2%
                     if search%(i%) = 0% then return
                     u% = (f% + search%(i%) + 14%)/16%
                     if u% <> c% and po$(u%) = po$(c%) and               ~
                                     poline$(u%) = poline$(c%) then L37640
                next i%
                f% = u%*16% + 1%  /* Start search at next element */
                if f% > maxlines% * 16% then return  /* All kool */
                goto L37530
L37640:              errormsg$ = "This Receiver/PO/PO-line already " &   ~
                                 "occurs on this invoice once (line"
                     temp = u%
                     if u% > c% then temp = u% - 1%
                     call "CONVERT" (temp, -0.001, str(errormsg$,68%,3%))
                     errormsg$ = errormsg$ & ")."
                     return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

        deffn_201
            payinpts% = 201%
            gosub call_payinpts_subroutine
            return

        deffn_211
            payinpts% = 211%
            gosub call_payinpts_subroutine
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

        deffn_202
            payinpts% = 202%
            gosub call_payinpts_subroutine
            return

        deffn_212
            payinpts% = 212%
            gosub call_payinpts_subroutine
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   3      *~
            * --------------------------------------------------------- *~
            * Input/Edit of First Line Item Screen.                     *~
            *************************************************************

        deffn_103
            payinpts% = 103%
            gosub call_payinpts_subroutine
            return

        deffn_113
            payinpts% = 113%
            gosub call_payinpts_subroutine
            return

        REM *************************************************************~
            *       L I N E   S U M M A R Y   S C R E E N               *~
            * --------------------------------------------------------- *~
            * Line Item Summary Screen. (#5 if you're counting).        *~
            *************************************************************

        deffn_115
            payinpts% = 115%
            gosub call_payinpts_subroutine
            return

        deffn_125
            payinpts% = 125%
            gosub call_payinpts_subroutine
            return

        call_payinpts_subroutine
            call "PAYINPTS" (payinpts%,                                  ~
                #3,  /* VENDOR  - VENDOR MASTER INFORMATION */           ~
                #5,  /* PAYMASTR- PAYABLES MAIN HEADER FILE */           ~
                #6,  /* PAYLINES- PAYABLES LINE ITEMS FILE */            ~
                #7,  /* SYSFILE2- SYSTEM INFO (DEFAULT PAY DATES) */     ~
                #9,  /* PAYBUFFR- PAYABLES INVOICE HEADER BUFFER */      ~
                #10, /* PAYBUF2 - PAYABLES INVOICE LINE ITEM BUFFER */   ~
                #12, /* HNYQUAN - INVENTORY QUANTITY FILE */             ~
                #17, /* PAYTSKNG- PAYINPUT Tasking Control File */       ~
                #22, /* CSHMASTR- 1099 POSTING NEEDS IT */               ~
                #23, /* CSHLINES- Cash Disbursements File */             ~
                #24, /* CSH1099 - 1099 DETAIL FILE */                    ~
                #25, /* TXTFILE - System Text File */                    ~
                #26, /* SERMASTR- Serial Number Master File */           ~
                #27, /* SERWORK - Work File */                           ~
                #28, /* SERTIF  - Transaction Image File */              ~
                #29, /* SERHOLD - Vendor Invoice Restore */              ~
                #41, /* PAYLNCUR- Currency-specific PAY line items */    ~
                #42, /* PAYLNBF2- Currency-specific PAYBUF2 line items */~
                #19) /* RCVLINES- Receiver Line Items File (Purchasing)*/
            return

        REM *************************************************************~
            *         T E S T   D A T A   F R O M   H E A D E R         *~
            *                                                           *~
            * TESTS ALL THE DATA ON THE PAYABLES INVOICE HEADER.        *~
            *************************************************************

            deffn_151
                  errormsg$ = " "
                  on fieldnr% goto  L50180,         /* VENDOR CODE      */~
                                    L50310,         /* INVOICE NUMBER   */~
                                    L50560,         /* DATE OF INVOICE  */~
                                    L50650,         /* PAY W/O DISC DATE*/~
                                    L50700,         /* PAY W/DISC DATE  */~
                                    L50760,         /* DISC PCT         */~
                                    L50840,         /* DISC AMOUNT      */~
                                    L50920          /* NON-DISCOUNTABLE */
                  return

L50180:     REM TEST FOR VENDOR CODE ON FILE.
                vendescr$ = hex(0684) & "Select a Vendor"
                call "GETCODE" (#3, vencode$, vendescr$, 1%, 1.3, f1%(3%))
                     if f1%(3%) = 0% then L50290
                REM GET ADDRESS & STUFF, JUST FOR FUN.
                    get #3, using L50240, venaddr$(), billsdue%,          ~
                                         discsdue%, discpercent
L50240:             FMT  POS(40), 6*CH(30), POS(286), 3*PD(14,4)
                    disc_pct = discpercent /* DEFAULT VALUE           */
                    return
L50290:         errormsg$ = "Unknown Vendor: " & vencode$
                return
L50310:     REM TEST FOR VENDOR CODE/INVOICE ON FILE
                if mode$ = "REC" then L55000
                if invoicenr$ <> " " then L50460
                if auto% <> 0% then L50430
                readkey$ = vencode$
                errormsg$ = hex(06) & "Select Invoice For Edit?"
                header$(), inc$() = " "
                mat inc = zer
                inc(2%) =  168.01 : inc$(2%) = "N"
                if mode$ = "OFF" then inc$(2%) = "O"
                call "PLOWCODE" (#5, readkey$, errormsg$, 5009%,         ~
                              0, f1%(5%), header$(), 0, 0, inc(), inc$())
                     if f1%(5%) <> 0% then L50450
L50430:              errormsg$ = hex(00)
                     return
L50450:         invoicenr$ = str(readkey$,10%)
L50460:         errormsg$ = " "
                if str(invoicenr$,,1%) <> "#" then L50510
                   if str(invoicenr$,6%) <> " " then L50510
                     errormsg$ = "Sorry, Reserved Invoice Number"
                     return
L50510:         gosub L30000
                if errormsg$ <> " " then return
                if oldinvoiceonfile% = 0% then return
                   return clear all
                   goto editmode
L50560:     REM TEST FOR VALID INVOICE DATE
                if mode$ = "REC" then return
                if invdate$ <> " " and invdate$ <> blankdate$ then L50600
                   invdate$ = paydate$
L50600:         call "DATEOK" (invdate$, temp%, errormsg$)
                     if errormsg$ <> " " then return
                        gosub L56000                /* REGULAR DATE JUNK*/
                        gosub L56650                /* DISCOUNT DATE    */
                     return
L50650:     REM TEST FOR VALID "DISBURSEMENT WITHOUT DISCOUNT" DATE
                if mode$ = "REC" then return
                if regulardate$ = " " or regulardate$ = blankdate$ ~
                                                 then gosub L56310
                call "DATEOK" (regulardate$, temp%, errormsg$)
                     return
L50700:     REM TEST FOR VALID DISBURSEMENT WITH DISCOUNT DATE
                if mode$ = "REC" then L55220
                if discdate$ = " " or discdate$ = blankdate$ then gosub L56650
                if discdate$ = "00/00/00" then return
                call "DATEOK" (discdate$, temp%, errormsg$)
                     return
L50760:     REM TEST FOR VALID DISC PERCENT
                disc_pct  = 0
                if disc_pct$ = " " then L50820
                call"NUMTEST"(disc_pct$,-9e7,1000,errormsg$,2.4,disc_pct)
                disc_amt = round((invamt-nondisc)*disc_pct/100,2)
                call "CONVERT" (disc_amt, -2.2, disc_amt$)
L50820:         gosub total_up_invoice
                return
L50840:     REM TEST FOR VALID DISC AMOUNT
                call "NUMTEST"(disc_amt$,-9e7,9e7,errormsg$,2.2,disc_amt)
                     if errormsg$ <> " " then return
                if disc_amt = round((invamt-min(invamt,nondisc)) *       ~
                                               disc_pct/100,2) then L50900
                     disc_pct = 0 : disc_pct$ = " "
L50900:         gosub total_up_invoice
                return
L50920:     REM TEST FOR VALID NON-DISCOUNTABLE AMOUNT
                call "NUMTEST" (nondisc$, 0,9e7, errormsg$, 2.2, nondisc)
                gosub total_up_invoice
                return

        REM *************************************************************~
            *         T E S T   D A T A   F R O M   H E A D E R         *~
            *                                                           *~
            * TESTS ALL THE DATA ON THE PAYABLES INVOICE HEADER.        *~
            *************************************************************

            deffn_152
                  errormsg$ = " "
                  on fieldnr% goto  L51160,         /* FREE TEXT FIELD  */~
                                    L51200,         /* 1099 CATEGORY    */~
                                    L51320,         /* PAYABLES         */~
                                    L51420,         /* HOLD STATUS      */~
                                    L51490,         /* DEFAULT PURCHASES*/~
                                    L51580          /* RECEIVER NUMBER  */
                  return

L51160:     REM TEST FREE TEXT FIELD
                REM ACTUALLY, THERE'S NOTHING TO TEST...
                return

L51200:     REM TEST FOR 1099 CATEGORY
                ten99descr$ = " "
                if ten99$ = " " then return
                readkey$ = "1099 CATS" & ten99$
                ten99descr$ = hex(06) & "Select 1099 Category"
                call "PLOWCODE" (#20,readkey$,ten99descr$,9%,0,f1%(20%))
                     if f1%(20%) = 1% then L51290
                     errormsg$ = "Invalid 1099 Category Code"
                     return
L51290:         ten99$ = str(readkey$,10%)
                return

L51320:     REM TEST FOR VALID PAYABLES ACCOUNT
                payacctdescr$ = hex(0694) & "Select The Payables Account"
                call "GETCODE" (#2, payacct$,payacctdescr$, 1%,0,f1%(2%))
                     if f1%(2%) = 0% then L51390
                        get #2, using L51370, payaccttype$
L51370:                         FMT XX(39), CH(1)
                        return
L51390:         errormsg$ = "Acct Not On File: " & payacct$
                return

L51420:     REM TEST FOR HOLD STATUS
                if mode$ = "REC" then return
                if hold$ = " " then hold$ = "N"
                if pos("NY" = hold$) <> 0 then return
                     errormsg$ = "Enter Y or N: " & hold$
                     return

L51490:     REM TEST FOR VALID PURCHASES ACCOUNT DEFAULT
                puracctdescr$ = " "
                if puracct$ = " " then L51550
                puracctdescr$ = hex(0684) & "Select Purchase Account"
                call "GETCODE" (#2, puracct$, puracctdescr$,1%,0,f1%(2%))
                if f1%(2%) <> 0% then L51550
                   errormsg$="Acct Not on File: " & puracct$
                   return
L51550:         defaultacct$ = puracct$
                return

L51580:     REM TEST FOR DEFAULT RECEIVER NUMBER
                if mode$ = "REC" then return
                if auto% = 0% then L51640
                   savemaxlines% = maxlines%
                   gosub load_complete_po
                   goto L51820
L51640:         if receiver$ = " " then return
                if receiver$ = "ALL" then L51780
                readkey$ = str(receiver$,,16%) & vencode$
L51670:         call "PLOWNEXT" (#19, readkey$, 25%, f1%(19%))
                     if f1%(19%) <> 0% then L51710
                     errormsg$="Unknown/Closed Receiver: " & receiver$
                     return
L51710:         get #19, using L51720, tempopen
L51720:         FMT POS(356), PD(14,4)
                if tempopen = 0 then L51670
                receiver$ = readkey$

L51780:         savemaxlines% = maxlines%
                gosub load_complete_receiver
                if errormsg$ <> " " then return

                if c% <> lines_allowed% then L51820
                call "PLOWNEXT" (#19, plowkey$, 25%, f1%(19%))
                     if f1%(19%) <> 0% then L51804
L51792:         u3% = 2%
                call "ASKUSER" (u3%, "MAXIMUM INVOICE LINES", "The max"& ~
                     "imum of 100 invoice lines allowed has been reache"&~
                     "d.", "Press RETURN to acknowledge.", " ")
                if u3% <> 0% then L51792
                    goto L51820
L51804:         u3% = 2%
                call "ASKUSER" (u3%, "MAXIMUM INVOICE LINES", "The max"& ~
                     "imum of 100 invoice lines allowed has been reache"&~
                     "d.", "All Lines for receiver " & receiver$ & " ha"&~
                     "ve NOT been appended.", "Press RETURN to acknowle"&~
                     "dge.")
                if u3% <> 0% then L51804

L51820:         REM Auto Gen'd Invoice, show'em...
                base% = 0% : editmode% = 1%
                return clear all
                gosub total_up_invoice
                temp = maxlines% - savemaxlines%
                call "CONVERT" (temp, 0.0, str(errormsg$,,4%))
                errormsg$ = errormsg$ & " Lines" & hex(84) &             ~
                                        "Were Appended To This Invoice."
                goto line_summary

        REM *************************************************************~
            *            T E S T   L I N E   I T E M   D A T A          *~
            *                                                           *~
            * VERIFY ALL LINE ITEM DATA.                                *~
            *************************************************************

            deffn_153
                  errormsg$, infomsg$ = " "
                  if rcv$(c%) = r41603fix$ then return
                  on fieldnr% goto  L53755,         /* Currency code    */~
                                    L52130,         /* PO NUMBER & LINE */~
                                    L52330,         /* RECEIVER NUMBER  */~
                                    L52530,         /* INVENTORY PART   */~
                                    L52615,         /* STORE NUMBER     */~
                                    L52675,         /* LOT NUMBER       */~
                                         ,         /* VENDR PART NUMBER*/~
                                    L53860,         /* Vendor ContractID*/~
                                    L52805,         /* QUANTITY PER UNIT*/~
                                    L52830,         /* QUANTITY OF PART */~
                                    L53430,         /* PRICE OF PART    */~
                                    L53535,         /* EXTENSION        */~
                                         ,         /* Inventory cost   */~
                                    L53600,         /* DEBIT ACCOUNT    */~
                                    L53625,         /* Cost variance    */~
                                    L53655          /* JOB NUMBER       */
                  return

L52130:     REM TEST DATA FOR PURCHASE ORDER & LINE NUMBER
                jobtext$ = " "
                if po$(c%) = " " then poline$(c%),rcv$(c%),jobtext$ = " "
                if po$(c%) = " " then return
                if poline$(c%) = " " then L52160
                   call "NUMTEST" (poline$(c%), 0, 999, " ", -0.001, 0)
L52160:         readkey$ = str(vencode$) &str(po$(c%)) &str(poline$(c%))
                call "READ100" (#14, readkey$, f1%(14%))
                     if f1%(14%) = 0% then L52255
                get #14, using L52190, po$(c%), poline$(c%), part$(c%),   ~
                         qty(c%), price(c%), job$(c%), venpart$(c%),     ~
                         factor(c%), invcost(c%), cost$(c%),varacct$(c%),~
                         contract_id$(c%), contract_line$(c%)
L52190:             FMT XX(9), CH(16), CH(3), XX(3), CH(25),             ~
                        POS(109), PD(14,4), PD(14,7),                    ~
                        POS(166), CH(8),                                 ~
                        POS(197), CH(25), XX(70), PD(14,4),              ~
                        POS(421), PD(14,4), CH(96), CH(9),               ~
                        POS(561), CH(16), CH(4)
                yy% = 45% : zz% = 14% : gosub po_rcvr_verify_currency
                if errormsg$ <> " " then return
                gosub L14223   /* Set up 'JOBTEXT$' for scrn */
                call "READ100" (#15, str(readkey$,,25%), f1%(15%))
                     if f1%(15%) = 0% then L52230  /* Shouldn't Happen */
                get #15, using L52225, acct$(c%)
L52225:         FMT POS(226), CH(9)  /* Interim Liability Account */
L52230:         gosub format_fields
                call "GLFMT" (acct$(c%))
                call "GLFMT" (varacct$(c%))
                return

L52255:         errormsg$ = hex(06) & "Select Puchase Order & Line #"
                header$(1) = "  P.O. Number    Line   Part Number"
                REM Show P.O. That Are Out There...
                call "PLOWCODE" (#19, readkey$, errormsg$,  3009%, 2.25, ~
                                              f1%(19%), header$(), 19, 1)
                     if f1%(19%) <> 0% then L52295
                     errormsg$ = "Unknown P.O. & Line Number: " & po$(c%)
                     return
L52295:         po$(c%) = str(readkey$,10%)
                poline$(c%) = str(readkey$,26%)
                yy% = 44% : zz% = 19% : gosub po_rcvr_verify_currency
                if errormsg$ <> " " then return
                gosub load_receiver_line
                gosub L14223   /* Set up 'JOBTEXT$' for scrn */
                gosub format_fields
                return
L52330:     REM TEST DATA FOR RECEIVER NUMBER
                if po$(c%) = " " then return
                if enabled% <> 0% then L52390
                     if rcv$(c%) <> " " then L52390
                     if rcv$(c%) = " " then infomsg$ = hex(94) &         ~
                                 "WARNING" & hex(84) & "- No Receiver" & ~
                                 " On File, Indicates Pre-payment."
                     if rcv$(c%) = " " and rcv$() <> " " then            ~
                          errormsg$ = "Invoice Can't Have Some " &       ~
                                      "Lines Prepaid And Some Lines Not"
                     gosub test_for_dups
                     return
L52390:         REM Should Be At Least One Receiver If Here...
                readkey$ = str(part$(c%)) & str(rcv$(c%))
                errormsg$ = hex(06) & "Select Receiver Number"
                header$(1%) = "  Receiver Number    Date Received"
                mat inc = zer  : inc$() = " "
                inc(2%) = 42.28
                inc$(2%) = str(vencode$) & str(po$(c%)) & str(poline$(c%))
                call "PLOWCODE" (#19, readkey$, errormsg$,  5025%, 1.06, ~
                             f1%(19%), header$(), 16, 122, inc(), inc$())
                     if f1%(19%) <> 0% then L52455
                     errormsg$ = "Invalid Receiver: " & rcv$(c%)
                     return

L52455:         REM Look For Dups...
                rcv$(c%) = str(readkey$,26%)
                gosub test_for_dups
                if errormsg$ <> " " then return
                REM Check buffers...
                    yy% = 44% : zz% = 19% : gosub po_rcvr_verify_currency
                    if errormsg$ <> " " then return
                    readkey$=str(rcv$(c%))&str(po$(c%))&str(poline$(c%))
                    gosub test_receiver_usage
                    if errormsg$ <> " " then return
                REM All Clear, Load Line Item Default Data...
                gosub load_receiver_line
                gosub L14223   /* Set up 'JOBTEXT$' for scrn */
                gosub format_fields
                return

L52530
*        Test data for PART NUMBER
            if str(part$(c%),,9%) <> "ACTIVITY:" then L52535
                 partdescr$ = "SERVICE ACTIVITY"
                 vpc_potyp$ = "A"
                 return
L52535:     if mode$ <> "REC" then L52560
                call "DESCRIBE" (#4, part$(c%), " ", 0%, f1%(4%))
                if f1%(4%) = 0% then L52600
                     errormsg$ = "Sorry, Must Be A Non-Stocked Item"
                     return
L52560:     partdescr$ = hex(06) & "PF-16 For Non-Stock Item"
            if po$(c%) = " " then call "GETCODE" (#4, part$(c%),         ~
                                              partdescr$, 1%, 0, f1%(4%))
            call "DESCRIBE" (#4, part$(c%), partdescr$, 1%, f1%(4%))
            if f1%(4%) = 1% then get #4, using L52577, costtype$(c%)
L52577:         FMT POS(307), CH(1)
            if f1%(4%) = 1% then L52605
                if part$(c%) <> " " then L52600
                     errormsg$ = "Part Can't Be Blank"
                     return
L52600:         partdescr$ = "(Non-Stocked Item)"
                vpc_potyp$ = "M"
                goto L52610
L52605:     if str(part$(c%),,9%) = "ACTIVITY:"                          ~
                   then vpc_potyp$ = "A" else vpc_potyp$ = "P"

L52610:     if editmode% = 0% then return else L52675

L52615
*        Test data for STORE NUMBER
            strdescr$ = " "
            if po$(c%) <> " " and store$(c%) = " " then return
            if partdescr$ = "(Non-Stocked Item)" then return
            if mode$ = "REC" then return
                call "GETCODE" (#8, store$(c%), strdescr$, 1%, 0, f1%(8%))
                if f1%(8%) = 1% then L52660
                     errormsg$ = "Invalid Store: " & store$(c%)
                     return
L52660:         defstr$ = store$(c%)
                if editmode% = 0% then return else L52675

L52675
*        TEST DATA FOR LOT NUMBER OK.
            if po$(c%) <> " " then return
            if mode$ = "REC" then return
            if partdescr$ = "(Non-Stocked Item)" then return
                if lot$(c%) = "?" then lot$(c%) = " "
                readkey$ = str(part$(c%))& str(store$(c%))& str(lot$(c%))
                call "READ100" (#12, readkey$, f1%(12%))
                if f1%(12%) = 1% and lot$(c%) = " " and lotenbl% = 2%    ~
                                                               then L52735
                if f1%(12%) = 1% then L52760
                     infomsg$ = "WARNING:" & hex(8c) & "No such lot" &   ~
                                " number found in this store."
L52735:              call "LOTVALID" (part$(c%), store$(c%), lot$(c%),   ~
                                      #7, #4, #12, errormsg$)
                     if errormsg$ <> " " then return
                     call "LOTUNQUE" (part$(), lot$(), c%, #7, errormsg$)
                     if errormsg$ <> " " then return
L52760:         xixacct% = 3%
                call "HNYGLGET" (part$(c%), store$(c%), lot$(c%),        ~
                                            xixacct$, xixacct%, #4, #12)

                if xixacct% > 0% then L52795
                call "GLFMT" (xixacct$)
                if xixacct$ <> " " then defaultacct$ = xixacct$
L52795:         if editmode% = 0% then return else L52830

L52805:     REM TEST DATA FOR QUANTITY PER UNIT
                call"NUMTEST"(factor$, 0, 9e6, errormsg$,-0.4,factor(c%))
                if errormsg$ = " " then format_fields
                return

L52830
*        Test data for QUANTITY PURCHASED
            low = -9e8 : high = 9e8 : thisinvoice = 0

          REM Get Amounts Off Receiver...
            readkey$ = str(rcv$(c%)) & str(vencode$)
            str(readkey$,26%) = str(po$(c%)) & str(poline$(c%))
            str(readkey$,45%) = all(hex(00))
            call "PLOWNEXT" (#19, readkey$, 44%, f1%(19%))
            if f1%(19%) = 0% then L52945
                get #19, using L52880, uninvoiced
L52880:              FMT POS(356), PD(14,4)

              REM Get Original Invoice Amount From Master...
                readkey$= str(rcv$(c%)) & str(po$(c%)) & str(poline$(c%))
                str(readkey$,36%) = str(vencode$) & str(invoicenr$)
                call "PLOWALTS" (#6, readkey$, 1%, 60%, f1%(6%))
                if f1%(6%) = 0% then L52925
                     get #6, using L52920, thisinvoice
L52920:                   FMT POS(106), PD(14,4)
L52925:         high = thisinvoice + uninvoiced
                low  = min(0, high)
                high = max(0, high)

L52945:    REM Actually Test The data...
            temp = 0
            convert venqty$ to temp, data goto L52960 : goto L52970
L52960:         errormsg$ = "Invalid Vendor's Quantity"
                return
L52970:     temp = round(temp, 7)
            if abs(temp - venqty) > .000001 then L53020
                temp = 0
                convert qty$(c%) to temp, data goto L52990 : goto L53000
L52990:              errormsg$ = "Invalid Internal Quantity"
                     return
L53000:         temp = round(temp, 2)
                if temp - qty(c%) = 0 then L53030
                     if temp >= 0 then L53025 else L52990

L53020:     temp = round(temp*factor(c%), 7)
L53025:     qty(c%) = temp
L53030:     gosub format_fields
            if qty(c%) >= low then L53055
                errormsg$ = "Minimum XXXXXXXXXX Vendor Units"
                call "CONVERT" (low, 0.7, str(errormsg$,9,10))
                return
L53055:     if qty(c%) <= high then L53075
                call "CONVERT" (high, -0.2, high$)
L53058:         u3% = 2%
                call "ASKUSER" (u3%, "WARNING",                          ~
                    "Quantity Entered Exceeds Uninvoiced Quantity.",     ~
                    "Uninvoiced = " & high$,                             ~
                "Press PF1 to re-enter or press RETURN to continue.")
                if u3%  = 0% then L53075
                if u3% <> 1% then L53058
                   errormsg$ = "Re-enter Quantity."
                   if linemode% <> 0% then return
                   errormsg$ = errormsg$ & "  (or RESTART Line)"
                   return
L53075:     if rcv$(c%) <> " " or po$(c%) <> " " then return
            if mode$ = "OFF"   or mode$ = "REC"  then L53170
                  oldhold = max(0, qtyhold(c%,1%) - qtyhold(c%,2%))
                  newhold = max(0, qtyhold(c%,1%) - qty(c%))
                  nethold = newhold - oldhold
                  if nethold <= 0 then L53170
                    for i% = 1% to maxlines%
                       if i% = c% then L53145
                       if part$(i%)  <> part$(c%)  then L53145
                       if store$(i%) <> store$(c%) then L53145
                       if lot$(i%)   <> lot$(c%)   then L53145
                         oldhold = max(0, qtyhold(i%,1%) - qtyhold(i%,2%))
                         newhold = max(0, qtyhold(i%,1%) - qty(i%))
                         nethold = nethold + (newhold - oldhold)
L53145:             next i%
                  if nethold <= 0 then L53170
                    call "HNYAVAIL" (#4, #12, part$(c%), store$(c%),     ~
                                     lot$(c%), errormsg$, nethold, temp, ~
                                     return%)
L53170:   /* Take care of Serial Numbers for this line                 */
            if mode$ = "REC"  then return /* No also if PO$(C%) <> " " */
                call "SERENABL" (part$(c%), sn_enable%, err%, #7, #4)
                if sn_enable% = 0% and str(sn_config$(c%),,1%) = hex(00) ~
                                                              then return
             /* Set up configuration variable for current conditions   */
                tmp_config$ = str(part$(c%)) & str(store$(c%)) &         ~
                              str(lot$(c%)) & "+"
                if qty(c%)  < 0 then str(tmp_config$,35%,1%) = "-"
                if str(tmp_config$,,35%) <> str(sn_config$(c%),,35%) or  ~
                   str(tmp_config$,35%)  =  str(sn_config$(c%),35%)  or  ~
                   sn_used%(c%) = 0% then L53235
                     errormsg$ = "Cannot change sign of Quantity": return
L53235:      /* Check if need to clear any previous serial numbers     */
                if sn_index%(c%) = 0% then L53265   /* Nothing to clear */
                if tmp_config$ = sn_config$(c%) then L53265 /* Same-OOO */
                     call "SERSTOVR" (sn_index%(c%), "2", "2", #26, #27)
                     if str(tmp_config$,,35%) <> str(sn_config$(c%),,35%)~
                          then sn_used%(c%) = 0% /* Tie score- into OT */
L53265:      /* Now get serial number entry                            */
                if sn_enable% = 0% then L53405
                sn_qty = abs(qty(c%)) - sn_used%(c%)
                if sn_qty = 0 then L53405
                if sn_qty > 0 then L53320
                     convert abs(sn_used%(c%)) to str(errormsg$,,4%),    ~
                                                                pic(####)
                     errormsg$ = "Serial Numbers have been used; cannot"&~
                                 " change quantity below " &             ~
                                 str(tmp_config$,35%,1%) & errormsg$
                     return
L53320:         if sn_index%(c%) <> 0% then L53330
                     sn_index%(c%), sn_last_index% = sn_last_index% + 1%
L53330:         sn_loc$ = str(store$(c%)) & lot$(c%)
                sn_trankey$ = str(vencode$) & str(invoicenr$)
                convert sn_index%(c%)to str(sn_trankey$,29%,4%),pic(0000)
                if qty(c%) < 0 then L53380
                     call "SERINSUB" (part$(c%), store$(c%), lot$(c%),   ~
                                      sn_qty, sn_index%(c%), 25%, "VT",  ~
                                      sn_trankey$, errormsg$,            ~
                                      #7, #4, #26, #27)
                     goto L53400

L53380:              call "SERSELCT" (part$(c%), sn_loc$,                ~
                                      sn_qty, sn_index%(c%), 25%, "VT",  ~
                                      sn_trankey$, "2", "2", errormsg$,  ~
                                      #7, #4, #26, #27)
L53400:              if errormsg$ <> " " then return
L53405:      /* Clean up and get outta Dodge.                          */
                if sn_enable% = 0% then sn_config$(c%) = all(hex(00))    ~
                                   else sn_config$(c%) = tmp_config$
                return

L53430:     REM TEST DATA FOR PRICE
                temp = 0
                save_price = price(c%)
                convert venprice$ to temp, data goto L53465
                temp = round(temp, 4)
                if abs(temp - venprice) < .0001 then L53475
                if temp >= 0 then L53515
L53465:              errormsg$ = "Invalid Vendor's Price"
                     return
L53475:         temp = 0
                convert price$(c%) to temp, data goto L53500
                temp = round(temp, 7)
                if abs(temp - price(c%)) < .0000001 then L53560
                   if temp >= 0 then L53520
L53500:              errormsg$ = "Invalid Internal Price"
                     return

L53515:         temp = round(temp/factor(c%), 7)
L53520:         price(c%) = temp
                goto L53560

L53535:     REM TEST DATA FOR EXTENSION
                save_price = price(c%)
                call "NUMTEST" (ext$(c%), -9e8, 9e8, errormsg$,-2.2, ext)
                     if errormsg$ <> " " then return
                if qty(c%) <> 0 then price(c%) = ext/qty(c%)
L53560:         gosub format_fields
                if flag% = 1% then return  /* Return if Inputmode */
                if save_price = price(c%) then return /* No change */
                if round(conveqv*save_price,4) <> invcost(c%) then return
                if postatstdcost$ = "Y" and pos("FST" = costtype$(c%))   ~
                          > 0% then return
                if po$(c%) <> " " then return /* Price can't cost on PO */
                     temp = price(c%) * conveqv /* Auto change Inv costs*/
                     call "CONVERT" (temp, 2.4, invcost$)
                     f$ = "I" : gosub L23315 /* if same to start with */
                     return

L53600:     REM TEST DATA FOR VALID DEBIT ACCOUNT
                call "GETCODE" (#2, acct$(c%), acctdescr$, 1%,0,f1%(2%))
                if f1%(2%) <> 0% then return
                   errormsg$ = "Not On File: " & acct$(c%)
                   return
L53625:     REM TEST DATA FOR VALID Price cost variance account
                call "GETCODE" (#2, varacct$(c%), varacctdescr$,         ~
                     1%, 0, f1%(2%))
                if f1%(2%) <> 0% then return
                   errormsg$ = "Not On File: " & varacct$(c%)
                   return

L53655
*        Test data for JOB/PROJECT NUMBER
            jobdescr$ = " "
            if part$(c%)  = " " then return
            if job$ (c%)  = " " then return
            if po$  (c%) <> " " then L53700
                call "READ100" (#21, part$(c%), f1%(21%))
                if f1%(21%) = 0% then L53700
                      errormsg$ = "Planned Part, Job/Project Reference"& ~
                                  " not Allowed"
                      return
L53700:         call "DESCRIBE" (#13,job$(c%),jobdescr$,1%,f1%(13%))
                if f1%(13%) = 1% then return
                call "DESCRIBE" (#11,job$(c%),jobdescr$,1%,f1%(11%))
                if f1%(11%) = 1% then return
                call "GETCODE" (#13,job$(c%),jobdescr$,1%,0,f1%(13%))
                if f1%(13%) = 1% then return
                call "GETCODE" (#11,job$(c%),jobdescr$,1%,0,f1%(11%))
                if f1%(11%) = 1% then return
                     errormsg$ = "Job/Project Not On File"
                     return

L53755
*        Currency code                       CURRENCY$
            if enabled% = 0% then return
            if currency$ = " " then currency$ = statutory$
            call "GETCODE" (#40, currency$, currdesc$, 1%, 0, f1%(40%))
            if f1%(40%) <> 0% then goto L53785
                errormsg$ = "Invalid Currency code.  Try again." : return
L53785:     convdate$ = " " : conveqv, convunt = 1
            if currency$ = statutory$ then goto L53850
            call "DATREVRS" (invdate$, rev_date$, errormsg$)
            if errormsg$ <> " " then return
            currkey$ = str(currtype$)& str(currency$)& str(rev_date$,,6%)
            call "PLOWNEXT" (#43, currkey$, 5%, f1%(43%))
            if f1%(43%) <> 0% then goto L53840
                errormsg$ = "Invalid Currency Code for this transaction."
                return
L53840:     get #43 using L53845, convdate$, conveqv, convunt
L53845:         FMT POS(12), CH(6), 2*PD(14,7)
L53850:     return

L53860
*        Test for Purchase Contract ID
            if contract_id$(c%) = " " then return
            if contract_id$(c%) = "?" then contract_id$(c%) = " "
            if vpc_potyp$ <> "P" then L53900
                call "VPCPIKME" (vencode$, contract_id$(c%),             ~
                       contract_line$(c%), "P"  , part$(c%),    " ",     ~
                       #47, #3, ret%)
                goto L53945                   /* Get VPC DATA */
L53900:     if vpc_potyp$ <> "M" then L53925
                call "VPCPIKME" (vencode$, contract_id$(c%),             ~
                       contract_line$(c%), "M"  , " ", " ", #47, #3, ret%)
                goto L53945                   /* Get VPC DATA */

L53925:     call "VPCPIKME" (vencode$, contract_id$(c%),                 ~
                       contract_line$(c%), "A"  , " "      ,    " ",     ~
                       #47, #3, ret%)
                goto L53945                   /* Get VPC DATA */
L53945:     if ret% <> 0% then L53960
L53950:         contract_id$(c%),contract_line$(c%),contract_descr$ = " "
                return
L53960:     str(readkey$,   ,16%) = contract_id$(c%)
            str(readkey$,17%, 4%) = contract_line$(c%)

            call "READ100" (#47, readkey$,  f1%(47%))
            if f1%(47%) = 0% then L53950   /* Shouldn't Happen */
            get #47 using L53995, vpc_vendor$, vpc_type$, vpc_code$

L53995:     FMT CH(9), POS(60), CH(1), CH(25)

            gosub check_contract_for_conflicts
            return

        po_rcvr_verify_currency
            errormsg$ = " "
            if curr$ <> "Y" then return
            call "READ100" (#yy%, key(#zz%), f1%(yy%))
            if f1%(yy%) <> 0% then L54055
                po_curr$ = statutory$
                return
L54055:     get #yy%, using L54060, po_curr$
L54060:         FMT CH(4), POS(33), PD(14,7)
            if po_curr$ <> currency$ then L54100
                if yy% <> 45% then return
                get #45 using L54085, price(c%), conveqv, convunt,        ~
                                     convdate$
L54085:             FMT POS(33), PD(14,7), POS(49), PD(14,7), PD(14,7),  ~
                                     CH(6)
                return
L54100:             if po_curr$ = statutory$ then po_curr$ = "statutory"
                    errormsg$ = "PO/Receiver currency (" & po_curr$ &    ~
                        ") doesn't match Currency Code entered (" &      ~
                        currency$ & ")."
                    return

        check_contract_for_conflicts
            if vencode$ = vpc_vendor$ then L54150
                errormsg$ = "Vendor does Not match Contract Vendor"
                return
L54150:     if vpc_type$ = "H" then return       /* All Headers are OK */
            if vpc_type$ = "M" then return       /* All Misc are OK */

            if vpc_potyp$ = "A" then L54255       /* Service Contract */
            if vpc_potyp$ = "M" then L54215       /* Non Stocked Part */

           /* Test Stocked Part */
            if part$(c%) = vpc_code$ then L54190
                errormsg$ = "Contract is Not for this Part Number" :return
L54190:     if vpc_type$ <> "A" then L54205
L54195:         errormsg$ = "Service Contract can Not be Assigned to Part"
                return
L54205:     /* Part matches a 'P' Contract or Contract is a 'M' Type */
            goto L54295   /* Test for Warning Status */
L54215:   /* Test NonStocked Part */
            if vpc_type$ =  "A" then L54195  /* Re-use Error Msg */
            if vpc_type$ <> "P" then L54240
                errormsg$ = "Non Stocked Part can Not be Assigned to a Pa~
        ~rt" : return
L54240:     /* Non Stocked Part matches a 'M' Contract  */
            goto L54295   /* Test for Warning Status */

L54255:   /*Test Activity*/
            if vpc_type$ <> "P"  then return
                errormsg$ = "Service Activity can Not be Assigned to a Pa~
        ~rt Contract" : return
*          IF PART$(C%) = VPC_CODE$ THEN 54295
*              ERRORMSG$ = "Service Activity Contract does Not match PO ~
*       Activity"

L54295:     return

L55000: REM *************************************************************~
            *             S P E C I A L   T E S T   D A T A             *~
            *                                                           *~
            * TESTS VALUES IF IN "REC" MODE.                            *~
            *************************************************************

            REM TEST FOR VENDOR RECURRING INVOICE ON FILE
                readkey$ = "APRECGRPS" & str(invoicenr$,2%,4%)
                groupdescr$ = hex(06) & "Select Cutover Group"
                call "PLOWCODE" (#20,readkey$,groupdescr$,9%,.30,f1%(20%))
                     if f1%(20%) = 1% then L55130
                     errormsg$ = "Unknown Cutover Group"
                     return
L55130:         invoicenr$ = "#" & str(readkey$,10%,4%)
                call "PUTPAREN" (groupdescr$)
                errormsg$ = " "
                gosub L30000
                if errormsg$ <> " " then return
                if oldinvoiceonfile% = 0% then return
                   return clear all
                   goto editmode

L55220:     REM TEST FOR RECURRING INVOICE EXPIRATION DATE
            call "DATEOK" (discdate$, 0%, errormsg$)
                if errormsg$ <> " " then return
            call "DATUNFMT" (discdate$)
            if discdate$ <= date then errormsg$ = "Must Be After Today."
            call "DATEFMT" (discdate$)
            return

L56000: REM *************************************************************~
            * D I S B U R S E M E N T   D A T E   C O M P U T A T I O N *~
            *                                                           *~
            * THESE TWO SUBROUTINES FIGURE OUT THE TWO DISBURSEMENT     *~
            * DATES FOR THE INVOICE USING THE VARIOUS DEFAULTS.         *~
            *-----------------------------------------------------------*~
            * FOR THE BILLS DUE WITHOUT DISCOUNT FIELD, THERE ARE 3 WAYS*~
            *     1.) IF THE VENDOR'S BILLS DUE FIELD = 0 THEN WE SET   *~
            *         THE DATE TO THE INVOICE DATE + THE SYSTEM DEFAULT *~
            *         BILLS DUE (DAYS) PARAMETER. (THIS IS COMMON...)   *~
            *     2.) IF THE VENDOR'S BILLS DUE FIELD > 0 THEN WE SET   *~
            *         THE DATE TO THE INVOICE DATE + THE VENDOR'S       *~
            *         DEFAULT BILLS DUE (DAYS) FIELD.                   *~
            *     3.) IF THE VENDOR'S BILLS DUE FIELD < 0 THEN WE SET   *~
            *         THE DATE ON THE PROX SYSTEM--DUE MONTH EQUAL TO   *~
            *         MONTH OF INVOICE + 1, CARRYING THE YEAR IF        *~
            *         APPROPRIATE.  THE PROXX DAY IS ALWAYS THE ABSOLUTE*~
            *         VALUE OF THE NUMBER RECALLED FROM THE VENDOR'S    *~
            *         BILLS DUE FIELD.                                  *~
            *-----------------------------------------------------------*~
            * FOR THE BILLS DUE WITH DISCOUNTS FIELD, THERE ARE 3 CASES *~
            *     1.) IF THE DISCOUNTS DUE FIELD = 0 THEN THE DISCOUNT  *~
            *         DATE FOR THE INVOICE GETS A NULL VALUE.           *~
            *     2.) IF THE DISCOUNTS DUE FIELD > 0 THEN THE DISCOUNT  *~
            *         DATE GETS THE INVOICE DATE + THE VENDOR'S DISCOUNT*~
            *         DUE FIELD.                                        *~
            *     3.) IF THE DISCOUNTS DUE FIELD < 0 THEN THE DISCOUNT  *~
            *         DATE IS COMPUTED ON THE "PROX" SYSTEM.  THE       *~
            *         CALCULATION IS AS IN (3) ABOVE.                   *~
            *************************************************************

L56310:     REM ROUTINE THAT COMPUTES REGULARDATE$
                date1$ = invdate$
                call "DATUNFMT" (date1$)
                date2$ = date1$

                on sgn (billsdue%)+2 gosub L56510, L56410, L56470
                   regulardate$ = date2$
                   call "DATEFMT" (regulardate$)
                   return

L56410:         REM CASE 1--IF BILLS DUE = 0 THEN USE SYSTEM DEFAULT
                    if sysbillsdue% = 0% then return
                       billsdue% = sysbillsdue%
                       on sgn (billsdue%)+2 goto L56510,,L56470
                       return

L56470:         REM CASE 2--IF BILLS DUE > 0 THEN USE INV DATE + DAYS
                    call "DATE" addr("G+",date1$,billsdue%,date2$,err%)
                    return

L56510:         REM CASE 3--IF BILLS DUE < 0 THEN SEE EXPLANATION ABOVE
                    tdate$ = date1$
                    call "DATEFMT" (tdate$, 0%, date1$)
                    convert str(date1$,5%,2%) to month%
                    convert str(date1$,1%,4%) to year%
                    month% = month% + 1%
                    if month% < 13% then L56580      /* CARRY YEAR       */
                       year% = year% + 1%
                       month% = 1%
L56580:             day% = abs(billsdue%)
                    REM CONVERT DATES BACK TO REGULAR DATE FORMAT.
                        goto L56940

L56650:     REM ROUTINE THAT COMPUTES DISCDATE$
                date1$ = invdate$
                call "DATUNFMT" (date1$)
                date2$ = "00/00/00"
                if discpercent = 0 then L56720

                   on sgn (discsdue%)+2 gosub L56860, L56760, L56820
L56720:               discdate$ = date2$
                      if date2$ <> "00/00/00" then call "DATEFMT" (discdate$)
                         return

L56760:         REM CASE 1--IF BILLS DUE = 0 THEN USE SYSTEM DEFAULT
                    if sysdiscsdue% = 0% then return
                       discsdue% = sysdiscsdue%
                       on sgn (discsdue%)+2 goto L56860,,L56820
                       return

L56820:         REM CASE 2--IF DISCOUNTS DUE > 0 THEN USE INV DATE + DAYS
                    call "DATE" addr("G+",date1$,discsdue%,date2$,err%)
                    return

L56860:         REM CASE 3--IF DISCOUNTS DUE < 0 THEN SEE EXPLANATION
                    tdate$ = date1$
                    call "DATEFMT" (tdate$, 0%, date1$)
                    convert str(date1$,5%,2%) to month%
                    convert str(date1$,1%,4%) to year%
                    month% = month% + 1%
                    if month% < 13% then L56930      /* CARRY YEAR       */
                       year% = year% + 1%
                       month% = 1%
L56930:             day% = abs(discsdue%)
L56940:             REM CONVERT DATES BACK TO REGULAR DATE FORMAT.
                        convert year%  to str(tdate2$,1%,4%), pic(0000)
                        convert month% to str(tdate2$,5%,2%), pic(00)
                        convert day%   to str(tdate2$,7%,2%), pic(00)
                        call "DATECONV" (date1$)
                        call "DATECONV" (tdate2$)
                        date2$ = tdate2$
                           return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM. SET RETURN CODE FOR DOCUMENTS IN BUFFER.    *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#27)  /* Bye Bye, Bertie */
*        Set Return Code if Invoices found in buffer (worth 5^).
                readkey$ = all(hex(00))
                str(readkey$,,3%) = userid$
                call "PLOWNEXT" (#9, readkey$, 3%, f1%(9%))
            end  f1%(9%)
