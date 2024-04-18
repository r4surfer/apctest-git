        REM *************************************************************~
            *                                                           *~ 
            *   AAA   PPPP    CCC   IIIII  N   N  V   V   SSSS  BBBB    *~
            *  A   A  P   P  C   C    I    NN  N  P   V  S      B    B  *~
            *  AAAAA  PPPP   C        I    N N N  V   V   SSS   BBBB    *~
            *  A   A  P      C   C    I    N  NN   V V       S  B    B  *~
            *  A   A  P       CCC   IIIII  N   N    V    SSSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * Main Program - (APCINVBO) - Create BOL/Invoice            *~
            *                                                           *~
            * APCINVSB - Create an Invoic from a BOL for the Session    *~
            *            "EWD001" thru "EWD007" one for each day of     *~
            *             the week.                                     *~
            *                                                           *~
            *            Note - Use the Posting Date as the Invoice     *~
            *                   Date for all Invoices Created.          *~
            *                                                           *~
            * Note - Does not Support the Following.                    *~
            *        (1) Lot Number Tracking                            *~
            *        (2) Serial Number Tracking                         *~
            *        (3) DATED Terms Code                               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/28/93 ! New Subroutine for (APC) - LAST MOD DATE ! RHH *~
            * 11/19/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/31/98 ! Y2K modifications                        ! ERN *~
            * 08/27/98 ! (EWD001) Mod to fix Ship date            ! RHH *~
            * 02/09/06 ! (PAR000) Mod to sub part                 ! CMG *~
            * 08/26/08 ! (AWD002) Mods to dts over data daily for ! CMG *~
            *          !   warehouse data                         !     *~
            *09/20/2010! (AWD003) add unitid and itemid to invoice! CMG *~
            *02/04/2011! (AWD004) change discount calculations    ! CMG *~
            *11/15/2012! (AWD005) mods for ship audit             ! CMG *~
            *03/06/2013! (AWD006) mods for esc, escamt, escacct   ! CMG *~
            *04/16/2019! (CR1993) PlyGem new tax rate, HD Met and ! RDB *~
            *          !   HD discount                            !     *~
            *04/30/2021! CR2829 Add GS1-128 fields                ! RDB *~
            *************************************************************
 

        sub "APCINVSB"   (cuscode$,      /* Customer Code              */~
                          so$,           /* Sales Order Number         */~
                          bol$,          /* Bill of Lading Number      */~
                          userid$,       /*                            */~
                          session$,      /*                            */~
                          postdate$,     /* Internal PD format         */~
                          load$,         /* Load Number Shipped On     */~
                          program$,      /* Program Name  (AWD005)     */~
                          #1,            /* (ARIBUFFR )-Invoice Headers*/~
                          #2,            /* (ARIBUF2 )-Invoice Lines   */~
                          #3,            /* (SYSFILE2)-                */~
                          #4,            /* (STORNAME)- Store Names    */~
                          #5,            /* (ARINUMBR)- Inv Control    */~
                          #6,            /* (ARMTRIAL)- A/R TRIAL BAL  */~
                          #7,            /* (BCKMASTR)- S.O. Headers   */~
                          #8,            /* (BCKLINES)- S.O. Lines     */~
                          #9,            /* (SHPHDRS) - Ship Sched Head*/~
                          #11,           /* (SHPLINES)- Ship Sched Line*/~
                          #12,           /* (ARMTERMS)- A/R Payment Ter*/~
                          #13,           /* (CUSTOMER)- Customer Master*/~
                          #16,           /* (INVMASTR)-                */~
                          #17,           /* (CATEGORY)-                */~
                          #18,           /* (STXCODES)- SALES TAX CODES*/~
                          #19,           /* (BCKSUBPT) PAR000          */~
                          #20,           /* (ORABOL)  - (AWD002)       */~
                          #21,           /* (SHPAUDIT)  (AWD005)       */~
                          #22,           /* (BCKLIN2) CR2829           */~
                          inv% )         /* 0 = OK, <> = 0 (ERRORS)    */
                                         /* 1 = ALREADY INVOICED       */
                                         /* 2 = S.O. NOT ON FILE       */
                                         /* 3 = BOL  NOT ON FILE       */

        dim                              /* (APCINVSB)                 */~
            acct$12,                     /* General Use Acct Number    */~
            acctxref$9,                  /* Account X-Ref              */~
            aracct$12,                   /* Net Invoice Account        */~
            artype$1,                    /* A/R Type Code              */~
            billxref$9,                  /* Bill-to Cross Reference    */~
            blankdate$8,                 /* empty date (PD)            */~
            bol$3,                       /* Bill of Lading Shipped     */~
            buflot$(100%,30%)6,          /* Buffer Lots                */~
            bufqty(100%,30%),            /* Buffer Lot Quantity        */~
            carrier$,                    /* Carrier Code               */~
            cat$(1005)4,                 /* Part Category              */~
            comm%(3%),                   /* Commission Split %s        */~
            completemsg$(100%)30,        /* Flagged Complete in Shippng*/~
            conv(100%),                  /* Conversion Pricing->Stockng*/~
/*AWD004*/  cost(100%),                  /* Unit Cost Discount Applied */~
            crhold$1,                    /* Order Credit Status        */~
            currdflt$4,                  /* Customer default currency  */~
            currency$4,                  /* Currency code& description */~
            cuscode$9,                   /* Customer Code              */~
            custype$2,                   /* Customer Type Code         */~
            date$8,                      /* Today's Date (not used)    */~
            demtype$(100%)1,             /* Demand Type for SA         */~
            descr$(100%)32,              /* Part Description           */~
            discacct$12,                 /* Sales Discounts Account    */~
            discs$(100%)12,              /* Sales Disc Account- Lines  */~
            expdate$8,                   /* Recurring Expiration Date  */~
            errormsg$79,                 /* (EWD001)                   */~
            fob$20,                      /* FOB                        */~
            frtacct$12,                  /* Freight Account Number     */~
            escacct$12,                  /* ESC Account   AWD006       */~
            frtbill$20,                  /* Freight Bill               */~
            howship$20,                  /* How Ship                   */~
            inpmessage$79,               /* Informational Message      */~
            invdate$8,                   /* Invoice Date               */~
            invnr$8,                     /* Invoice Number             */~
            invrsn$9,                    /* Invoice Reason             */~
            invtype$1,                   /* Type of Invoice            */~
            item$(100%)3,                /* P.O. Item Number           */~
            linediscamt(100%),           /* Line Item Discount Amt     */~
            linediscpct(100%),           /* Line Item Discount %       */~
            lineext(100%),               /* Line Item Extensions       */~
            lot$(30%)6, lots$(100%,30%)6,/* Lot Numbers                */~
            lotqty(30%),lotqtys(100%,30%),/*Lot Quantities             */~
            lsts$(100%)1,                /* Line Status                */~
            nonstockmsg$(100)16,         /* Non-Stock Part Flag & Msg  */~
            openqty(100%),               /* Open Order Quantity        */~
            order(100%),                 /* Original Order Quantity    */~
            part$(100%)25,               /* Part Code                  */~
            epart$(100%)65,              /* Entire Part        PAR000  */~
            pc$1,                        /* Price Code                 */~
            po$16,                       /* Purchase Order Number      */~
            postdate$8,                  /* Post Date (from Session)   */~
            posthny$1,                   /* Post Inventory?            */~
            postlot$(100%,30%)6,         /* All Ready Posted           */~
            postqty(100%,30%),           /* All Ready Posted Qty.      */~
            price(100%),                 /* Unit Price (Pricing UOM)   */~
            pricestk(100%),              /* Unit Price (Stockng UOM)   */~
            priceuom$(100%)4,            /* Pricing Unit of Measure    */~
            project$(100%)8,             /* Project Number             */~
            qtyschld(100%),              /* Total Qty in Shipping      */~
            readkey$50, readkey1$100,    /* Misc Read Keys             */~
            readkey2$100,                /* Misc Read key (AWD002)     */~
            region$4,                    /* Region Code                */~
            sales$(100%)12,              /* Sales Acct- Lines          */~
            salesacct$12,                /* Sales Acct- Header         */~
            salesman$(3%)4,              /* Salesman Code / Split %    */~
            seq$(100%)3,                 /* Line Item Number           */~
            session$6,                   /* Session Number             */~
            ship(100%),                  /* Quantity Shipped           */~
            shipdate$8,                  /* Date Shipped               */~
            shipto$(6%)30,               /* Ship-to                    */~
            sn_last_lineid$4,            /*             High Seq Nr    */~
            sn_lineid$(100%)4,           /*             Line Seq Nr    */~
            so$16,                       /* Sales Order Number         */~
            soldto$(6%)30,               /* Sold-to                    */~
            soopen(100%),                /* Original Open Qty on SO    */~
            soorder(100%),               /* Order Quantity on SO       */~
            soseq$(100%)3,               /* SO Seq # X-Ref             */~
            statutory$4,                 /* Statutory currency code    */~
            stkuom$(100%)4,              /* Stocking Unit of Measure   */~
            stlmnt$12,                   /* Settlement Number          */~
            store$3,                     /* Store Code                 */~
            taxable$(100%)1,             /* Line Taxable? (Y/N)        */~
            taxacct$12,                  /* Sales Tax Account          */~
            taxcode$10, taxcodedescr$30, /* Sales Tax Code             */~
            taxpct$5,                    /* Sales Tax Percent          */~
            temp$16,                     /* Misc Temp Variable         */~
            terms$20,                    /* Payment Terms (Code)       */~
            termsdue(30%),               /* Dated- Amounts Due         */~
            termsdiscs(30%),             /*      - Cash Disc Percents  */~
            termsdisc$(30%)6,            /*      - Disc Due Dates      */~
            termsnet$(30%)6,             /*      - Net  Due Dates      */~
            textid$4, textidl$(100%)4,   /* Text ID- Hdr, Lines        */~
/*AWD004*/  unitdiscamt(100%),           /* Unit Disc Amt              */~
            userid$3,                    /* Current User Id            */~
            xx$10,                       /* (EWD001)                   */~ 
            vf$200,                      /* Variable Fields            */~
            energy$14,                   /* ESC       AWD006           */~
            pgpo$20,                     /* PlyGem PO number           */~ 
            pgid$20                      /* PlyGem ID number           */

        dim                              /* PAR000                     */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */

        dim load$5,                      /* Load Number (AWD002)       */~
            job$16,                      /* Customer Job (AWD002)      */~
            quote$10,                    /* Quote NUmber (AWD002)      */~
            orderdate$6,                 /* SO Order Date (AWD002)     */~
            duedate$6                    /* SO Due Date   (AWD002)     */
            
        dim itemid$(100%)10,             /* WW Item ID (AWD003)        */~
            unitid$(100%)10              /* WW Unit ID (AWD003)        */
        
        dim gs128$(100%)20,              /* GS1-128 barcode  CR2829    */~
            skunbr$(100%)10              /* SKU number                 */
            
        dim program$9                    /* (AWD005)                   */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Create an Invoice for a B.O.L.    "
            pname$ = "APCINVSB - Rev: R6.04"

        REM *************************************************************


        REM *************************************************************~
            *          M a i n   L i n e   P r o c e s s i n g          *~
            *                                                           *~
            *************************************************************
            call "DATUFMTC" (blankdate$)

                                         /* Check Existance of Session */
            inv% = 0%
            gosub initialize                      /* INIT ALL VARIABLES*/
            gosub load_sales_order
            if invoiced% <> 0% then goto L01690
               inv% = 1% : goto exit_program      /* ALREADY INVOICED  */

L01690:     if soonfile% <> 0% then goto L01720
               inv% = 2% : goto exit_program      /* S.O.- NOT ON FILE */

L01720:     if bolonfile% <> 0% then goto L01750    /* BOL - NOT ON FILE */
               inv% = 3% : goto exit_program

L01750:     gosub total_invoice

            gosub save_data
            goto exit_program


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

        initialize

            init(" ") readkey$, readkey1$, acct$, acctxref$, aracct$,    ~
                      artype$, billxref$, buflot$(), carrier$, cat$(),   ~
                      completemsg$(), crhold$, currdflt$, currency$,     ~
                      custype$, date$, demtype$(), descr$(), discacct$,  ~
                      discs$(), expdate$,fob$, frtacct$, frtbill$,       ~
                      howship$, inpmessage$, invdate$, invnr$, invrsn$,  ~
                      invtype$, item$(), lot$(), lots$(), lsts$(),       ~
                      nonstockmsg$(), part$(), pc$, po$, posthny$,       ~
                      postlot$(), priceuom$(), project$(), region$,      ~
                      sales$(), salesacct$, salesman$(), seq$(),         ~
                      shipdate$, shipto$(), sn_last_lineid$,             ~
                      sn_lineid$(), soldto$(), soseq$(), statutory$,     ~
                      stkuom$(), stlmnt$, store$, taxable$(), taxacct$,  ~
                      taxcode$, taxcodedescr$, taxpct$, temp$, terms$,   ~
                      termsdisc$(), termsnet$(), textid$, textidl$(),    ~
                      vf$, job$, orderdate$, quote$, duedate$, unitid$(),~
                      itemid$(), escacct$, skunbr$(), gs128$()
/* CR2829 */

            invdate$  = blankdate$
            shipdate$ = blankdate$

            mat bufqty      = zer
            mat comm%       = zer
            mat conv        = zer
            mat linediscamt = zer
            mat linediscpct = zer
            mat lineext     = zer
            mat lotqty      = zer
            mat lotqtys     = zer
            mat openqty     = zer
            mat order       = zer
            mat postqty     = zer
            mat price       = zer
            mat pricestk    = zer
            mat qtyschld    = zer
            mat ship        = zer
            mat soopen      = zer
            mat soorder     = zer
            mat termsdue    = zer
            mat termsdiscs  = zer
        
            mat cost        = zer         /* (AWD004) */
            mat unitdiscamt = zer         /* (AWD004) */

        return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L02340: FMT                 /* FILE: BCKMASTR  (Read Only)             */~
            XX(9),          /* Customer Code                           */~
            XX(16),         /* Sales Order Mumber                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship-To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* Payment Terms                           */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            XX(100),        /* Shipping Instructions                   */~
            CH(9),          /* Sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(4),        /* Salesman Codes                          */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region code                             */~
            CH(10),         /* Quote Number   (AWD002)                 */~
            XX(10),         /* Extra Space    (AWD002)                 */~
            CH(16),         /* Job Number     (AWD002)                 */~
            XX(4),          /* Extra Space    (AWD002)                 */~
            XX(160),        /* Variable Fields (AWD002) Rest of VF     */~
            XX(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Order Date     (AWD002)                 */~
            XX(6),          /* Cancellation Date                       */~
            CH(6),          /* Due Date default  (AWD002)              */~
            XX(6),          /* Date Released                           */~
            XX(6),          /* Originally Input On (date)              */~
            XX(3),          /* Originally Input By (user)              */~
            XX(6),          /* Last Modified On (date)                 */~
            XX(3),          /* Last Modified By (user)                 */~
            XX(9),          /* Adjustment Reason Code                  */~
            XX(1),          /* SA Period                               */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Order Discount Percent                  */~
            XX(8),          /* Open Order Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            POS(893), CH(4),/* Currency code                           */~
            POS(900), CH(8), /* ESC      (AWD006)                      */~
/*CR1993*/  POS(920), CH(20), /* PlyGem PO                             */~
            CH(20),           /* PlyGem Quote ID                       */~
            PD(14,4),         /* Tax rate                              */~
            PD(14,4),         /* HDMET                                 */~
            PD(14,4)          /* HDIDX                                 */            

L02680: FMT                 /* FILE: BCKLINES                          */~
            XX(9),          /* Customer Code                           */~
            XX(16),         /* Sales Order number                      */~
            CH(3),          /* Sequence Number                         */~
            CH(3),          /* Item Number                             */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part Number description                 */~
            CH(4),          /* Category code                           */~
            PD(14,4),       /* Order Quantity                          */~
            XX(8),          /* Quantity Shipped (total)                */~
            PD(14,4),       /* Quantity Open                           */~
            PD(14,4),       /* Quantity scheduled for Shipment         */~
            XX(16),         /* Filler                                  */~
            PD(14,4),       /* Unit Price @ Stocking UOM               */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor (Pricing to Stkng)    */~
            PD(14,4),       /* Unit Price                              */~
            PD(14,4),       /* Pricing Discount Percent                */~
            CH(1),          /* Taxable (y/n) indicator                 */~
            CH(9),          /* Sales Account number                    */~
            CH(9),          /* Discounts Account                       */~
            XX(6),          /* Due Date - Original                     */~
            XX(6),          /* Due Date - Current                      */~
            XX(6),          /* Required Ship Date                      */~
            CH(6),          /* Lot Number                              */~
            CH(8), XX(8),   /* Project Number, Filler                  */~
            CH(1),          /* Demand Type                             */~
            XX(1),          /* Priority Code                           */~
            XX(4),          /* Internal ID to text in TXTFILE.         */~
            XX(1),          /* Allocation Flag                         */~
            XX(8),          /* Invoice Number                          */~
            CH(10),         /* ItemID         (AWD003)                 */~
            CH(10),         /* UnitID         (AWD003)                 */~
/* CR2829 */                                                             ~
            XX(15),         /* Other fields                            */~
            CH(10),         /* SKU number                              */~
            XX(01)          /* Filler                                  */
/* CR2829 */
L02685: FMT                 /* FILE: BCKLIN2                           */~
            XX(9),          /* Customer Code                           */~
            XX(16),         /* Sales Order number                      */~
            XX(3),          /* Sequence Number                         */~
            XX(10),         /* SKU Number                              */~
            CH(20),         /* GS128 barcode number                    */~
            XX(198)         /* Filler                                  */

L03020: FMT                 /* FILE: ARIMASTR                          */~
            CH(9),          /* Customer Code                           */~
            CH(8),          /* Invoice Number                          */~
            CH(16),         /* Purchase Order Number                   */~
            CH(16),         /* Sales Order Number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            6*CH(30),       /* Ship To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(6),          /* Ship Date                               */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            CH(6),          /* Shipping Carrier Code                   */~
            PD(14,4),       /* Number of Cartons                       */~
            PD(14,4),       /* Shipment Weight                         */~
            CH(20),         /* Freight/ Air Bill Number                */~
            3*CH(4),        /* Salesman Id                             */~
            3*BI(01),       /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region Code                             */~
            CH(1),          /* Price Code                              */~
            CH(6),          /* Invoice Date                            */~
            CH(6),          /* Recurring Expiration Date               */~
            CH(6),          /* Post Date                               */~
            CH(6),          /* Entry Date                              */~
            CH(3),          /* User ID who entered transaction         */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(9),          /* Net Invoice Distr. Account (A/R)        */~
            CH(9),          /* Freight Account Code                    */~
            CH(9),          /* Sales Tax Account Code                  */~
            CH(9),          /* Sales Distribution Account              */~
            CH(9),          /* Sales Discounts Distribution Account    */~ 
            PD(14,4),       /* Gross Invoice Amount                    */~
            PD(14,4),       /* Discount Percentage                     */~
            PD(14,4),       /* Discount amount                         */~
            PD(14,4),       /* Freight Amount                          */~
            PD(14,4),       /* Sales Tax Amount                        */~
            PD(14,4),       /* Net Invoice Amount                      */~
            PD(14,4),       /* Current Balance                         */~
            CH(9),          /* Bill-to Cross Reference                 */~
            CH(12),         /* Settlement Code                         */~
            CH(3),          /* Store Code                              */~
            CH(10),         /* Sales Tax Code                          */~
            PD(14,4),       /* Sales Tax Percent                       */~
            CH(1),          /* Invoice Type                            */~
            CH(9),          /* Invoice Reason Code                     */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Post Inventory with this Transaction?   */~
            CH(1),          /* Post G/L with this Transaction?         */~
            CH(1),          /* A/R Type (A/C/E)                        */~
            CH(20),         /* Payment Terms                           */~
            30*PD(14,4),    /* Payment Amount Due                      */~
            30*PD(14,4),    /* Cash Discount Percent                   */~
            30*CH(6),       /* Discount Terms                          */~
            30*CH(6),       /* Net Payment Terms                       */~
            CH(2),          /* Customer Type                           */~
            CH(9),          /* Account X-Ref                           */~
            CH(4),          /* Currency code         (pos 1792)        */~
            CH(218)         /* Filler (Internal, unused space)         */

L03600: FMT                 /* FILE: ARILINES                          */~
            CH(9),          /* Customer Code                           */~
            CH(8),          /* Invoice Number                          */~
            CH(3),          /* General purpose sequence number         */~
            CH(3),          /* Purchase Order Line Number              */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part description                        */~
            CH(4),          /* Category Code                           */~
            PD(14,4),       /* Order Quantity                          */~
            PD(14,4),       /* Quantity Shipped                        */~
            PD(14,4),       /* Open Quantity                           */~
            PD(14,4),       /* Unit Price- at Stocking UOM             */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor (buy to sell)         */~
            PD(14,4),       /* Unit Price at Pricing UOM               */~
            PD(14,4),       /* Line Item Discount                      */~
            PD(14,4),       /* Discount Amount                         */~
            PD(14,4),       /* Extension                               */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(9),          /* Sales Account Number                    */~
            CH(9),          /* Discounts Account                       */~
            CH(6),          /* Filler                                  */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* General purpose sequence number         */~
            30*CH(6),       /* Lot Number                              */~
            30*PD(14,4),    /* Quantity in corresponding lot           */~
            CH(1),          /* Non-Stock Flag (Y=Non-Stock else blank) */~
            CH(8),          /* Project                                 */~
            CH(4),          /* Line ID for Serial Number Links         */~
            PD(14,4),       /* Total Cost                              */~
            PD(14,4),       /* Standard Cost                           */~
            CH(1),          /* Demand Type                             */~
            POS(659),                                                    ~
            CH(10),         /* Item ID     (AWD003)                    */~
            CH(10),         /* Unit ID     (AWD003)                    */~
            CH(09),                                                      ~
            CH(20),         /* GS1-128 barcode number    CR2829        */~
            CH(43)          /* Filler (Internal, unused space)         */

L03605: FMT                 /* FILE: ORABOL  (AWD002)                  */~ 
            CH(1),          /* Transaction Flag 'S'end                 */~
            CH(6),          /* System Date                             */~
            CH(5),          /* Load Number                             */~
            CH(9),          /* Customer Code                           */~
            CH(8),          /* Sales Order                             */~
            CH(3),          /* Sequence Number                         */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Ship Date                               */~
            CH(10),         /* Quote Number                            */~
            CH(25),         /* Part Number                             */~
            CH(20),         /* Subpart Number                          */~
            CH(20),         /* Infopart Number                         */~
            PD(14,4),       /* Order Qty                               */~
            PD(14,4),       /* Ship Qty                                */~
            PD(14,4),       /* Head Disc Percent                       */~
            PD(14,4),       /* Line Disc Percent                       */~
            PD(14,4),       /* Unit Price                              */~
            PD(14,4),       /* Extended Price                          */~
            CH(4),          /* Category Code                           */~
            CH(4),          /* Region Code                             */~
            CH(4),          /* Salesman Code                           */~
            CH(6),          /* Due Date                                */~
            CH(8),          /* Invoice Number                          */~
            CH(16),         /* Job                                     */~ 
            CH(16),         /* PO                                      */~
            CH(10),         /* Item ID     (AWD003)                    */~
            CH(10),         /* Unit ID     (AWD003)                    */~
            CH(20)          /* GS1-128 barcode number                  */
 

        REM *************************************************************~
            *          S p e c i a l   S u b r o u t i n e s            *~
            *************************************************************

        assign_invoice                   /* Assign Invoice Number      */
            store$ = "300"
            call "ARINEXT" (#4, #3, #5, #6, "O", store$, cuscode$,       ~
                                                      billxref$, invnr$ )
        return

        get_account_defaults  /* Load up all header accounts  */
            acct% = 4%
            gosub get_account_default
            aracct$ = acct$

            acct% = 6%  
/*CR1993*/  tflag% = 0%
            if taxrate > 0 then  taxcode$ = str(shipto$(6),19,2)
            if taxrate > 0 then  acct% = 13%
            gosub get_account_default
            taxacct$ = acct$

            acct% = 7%  :  gosub get_account_default
            frtacct$ = acct$
            
/* (AWD006) */
            acct% = 12% :  gosub get_account_default
            escacct$ = acct$  

        return

        get_account_default
            call "ARMGLGET" (acct%, cuscode$, " ", " ", taxcode$,        ~
                             store$, " ", #3, #13, #16, #17, #18, acct$)
        return

        REM *************************************************************~
            *           L O A D   S A L E S   O R D E R                 *~
            *************************************************************
        load_sales_order
            print at(04,02),"Loading Sales Order ........  ";so$
            close ws
            invoiced%, soonfile%, bolonfile%, maxlines% = 0%

*        First See if the Order is on file              /* (BCKMASTR) */
            readkey$ = str(cuscode$) & so$
            read #7,key = readkey$, eod goto L05250        /* EOD EXIT   */
               soonfile% = 1%

*        Next see if BOL is on file -or- if order is scheduled
                                                         /* (SHPHDRS ) */
                readkey$ = str(cuscode$) & str(so$) & str(bol$)
                read #9,key = readkey$, eod goto L05250    /* EOD EXIT   */
                   bolonfile% = 1%
                get #9,using L04420 , shipdate$, readkey$   /*Inv # */
L04420:             FMT POS(184), CH(6), POS(234), CH(8)
                if readkey$ = " " then goto L04460
                   return                          /* ALREADY INVOICED */

L04460:     invoiced% = 1%
*        Now load up Sales Order and Scheduled Data     /* (BCKMASTR) */
            get #7 using L02340 , po$, shipto$(), soldto$(), terms$,       ~
                howship$, fob$, salesacct$, discacct$, salesman$(),      ~
                comm%(), region$,  quote$, job$, store$, orderdate$,     ~
                duedate$, pc$, invdiscpct, crhold$, currency$, energy$,  ~
                pgpo$, pgid$, taxrate, hdmet, hdidx                                                 
                   /* (AWD002) added quote, job, orderdate, duedate & store */
            if taxrate < 0 or taxrate > 99999 then taxrate = 0   /* CR1993 */
            if hdmet < 0 or hdmet > 99999 then hdmet = 0
            if hdidx < 0 or hdidx > 99999 then hdidx = 0
/* (AWD006) */                   
            energy = 0.00
            if str(energy$,1%,14%) = " " then goto no_energy
 
               get str(energy$,1%,8%) using L31620, energy
L31620:                 FMT PD(14,4)
         no_energy                   
/* (\AWD006) */

*        Load BOL information                           /* (SHPHDRS ) */
                cartons, weight, frtamt = 0.0
                get #9 using L04570 , carrier$, howship$, fob$, shipdate$, ~
                    frtbill$, cartons, weight, frtamt, textid$
L04570:              FMT XX(37), CH(6), 2*CH(20), XX(100), CH(6), CH(20),~
                         3*PD(14,4), POS(242), CH(4)

            gosub get_account_defaults            /* A/R, TAX, AND FRT */

            artype$  = "A"
            invdate$ = postdate$                  /* USE POSTING DATE  */
                                                  /* FROM SESSIN       */
               gosub get_tax_defaults

*        Now Read in line items from  (BCKLINES)
            c%, maxlines% = 0%
            readkey$ = str(so$) & hex(00)
L04700:     read #8,key > readkey$, using L04710 , readkey$, eod goto L05250
L04710:        FMT POS(10), CH(19)
            if so$ <> str(readkey$,1%,16%) then goto L05250
                c%, maxlines% = c% + 1%
                get #8 using L02680 ,                                      ~
                     soseq$(c%), item$(c%), part$(c%), descr$(c%),       ~
                     cat$(c%), soorder(c%), soopen(c%),                  ~
                     qtyschld(c%), pricestk(c%),                         ~
                     stkuom$(c%), priceuom$(c%), conv(c%), price(c%),    ~
                     linediscpct(c%), taxable$(c%), sales$(c%),          ~
                     discs$(c%), dfltlot$, project$(c%), demtype$(c%),   ~
                     itemid$(c%), unitid$(c%), skunbr$(c%)
/* (AWD003) get itemid and unitid from bcklines */
/* CR2829 get gs128 from bcklin2 */

            read #22,key = readkey$, using L04720 , readkey$, eod goto L04840
L04720:        FMT POS(10), CH(19)
                get #22 using L02685, gs128$(c%)

        REM Get transaction amounts from the BCK 'shadow' file (BCKCURCY)
L04840:         order(c%) = soorder(c%)
                temp% = 0%
                     convert sn_last_lineid$ to temp%, data goto L04850
L04850:              temp% = temp% + 1%
                     convert temp% to sn_lineid$(c%), pic(0000)
                     sn_last_lineid$ = sn_lineid$(c%)
                lotqtys(c%,1) = ship(c%)
                convert c% to seq$(c%), pic(###)
                lsts$(c%) = "O"

/*PAR000*/
                init(" ") so_inv$, item_no$, epart$(c%)
                so_inv$  = so$
                item_no$ = soseq$(c%) 
                gosub lookup_subpart
                str(epart$(c%), 1,25) = part$(c%)
                str(epart$(c%),26,20) = str(bcksubpt_rec$,48,20)
                str(epart$(c%),46,20) = str(bcksubpt_rec$,132,20)


                                                         /* (SHPLINES) */
                   readkey1$ = str(so$) & str(bol$) & soseq$(c%)
                   read #11,key = readkey1$, eod goto L05170
                     get #11 using L04970 , ship(c%), lot$(), lotqty(),    ~
                                          textidl$(c%), completemsg$(c%)
L04970:              FMT XX(31), PD(14,4), 30*CH(6), 30*PD(14,4), CH(4), ~
                         POS(476), CH(1)
                     for l% = 1% to 30%
                       lots$   (c%, l%) = lot$  (l%)
                       buflot$ (c%, l%) = lot$  (l%)
                       postlot$(c%, l%) = lot$  (l%)
                       lotqtys (c%, l%) = lotqty(l%)
                       bufqty  (c%, l%) = lotqty(l%)
                       postqty (c%, l%) = lotqty(l%)
                     next l%
                     if shipdate$ <> blankdate$ then L05100
                          lotqtys(c%,1) = ship(c%)
                          lots$  (c%,1) = dfltlot$
L05100:         
/* (AWD004) begin */
REM                lineext(c%) = round(ship(c%) * price(c%) / conv(c%), 2)
REM                linediscamt(c%) =                                        ~
                            round(lineext(c%) * linediscpct(c%) * .01, 2)
REM                linediscamt(c%) = -linediscamt(c%)
REM                lineext(c%) = lineext(c%) + linediscamt(c%)

/* Discount Amt for one unit */
                unitdiscamt(c%) = round(price(c%) * linediscpct(c%) * .01, 2)
                unitdiscamt(c%) = -unitdiscamt(c%)

/* Cost of Unit.  Price minus Discount */
                cost(c%) = round(price(c%) + unitdiscamt(c%), 2)

/* Total Discount Amt.  Ship Quantity multiplied by unit discount amt */
                linediscamt(c%) = round(ship(c%) * unitdiscamt(c%), 2)

/* Line Extension with Discount */
                lineext(c%) = round(ship(c%) * cost(c%), 2)
/* (AWD004) end  */

                openqty(c%) = max(0, soopen(c%) - ship(c%))
                                                         /* (INVMASTR) */
L05170:         read #16,key = epart$(c%), eod goto L05190     /*PAR000*/
                   goto L05200
L05190:         nonstockmsg$(c%) = "(Non-Stock Part)"
L05200:         if completemsg$(c%) = "C" then                           ~
                   completemsg$(c%) = "Reported Complete in Shipping"    ~
                   else completemsg$(c%) = " "
                goto L04700

L05250: return

        get_tax_defaults
                                                         /* (CUSTOMER) */
            read #13,key = cuscode$, eod goto L05480
            get #13 using L05320 , acctxref$, billxref$, temp$, custype$,  ~
                                 taxcode$, currdflt$
L05320:         FMT POS(771), 2*CH(9), POS(793), CH(1), POS(1023),       ~
                    CH(2), CH(10), POS(1045), CH(4)

            if taxcode$ <> " " then L05420
                                                         /* (SYSFILE2) */
                readkey$ =  "DEFAULTS.STORE." & str(store$)
                call "READ100" (#3, readkey$, x%)
                if x% = 1% then get #3 using L05400 , taxcode$
L05400:              FMT POS(252), CH(10)
                                                         /* (STXCODES) */
L05420:     call "DESCRIBE" (#18, taxcode$, taxcodedescr$, 0%, x%)
            taxpct = 0  :  if x% = 0% then L05460
                get #18 using L05450 , taxpct
L05450:              FMT XX(40), PD(14,4)
L05460:     if taxrate > 0 then taxpct = taxrate         /* CR1993 */
            call "CONVERT" (taxpct, 2.4, taxpct$)
            if taxpct = 0 then taxpct$ = " "
L05480: return

        total_invoice
            grossinv, taxamt, taxable, netinv, escamt = 0
            hdmetamt, hdidxamt = 0
            lines%, deles% = 0%
            for c% = 1% to maxlines%

                if lsts$(c%) <> "D"  then lines% = lines% + 1%           ~
                                     else deles% = deles% + 1%
                if lsts$(c%) = "D" then L05600
                   grossinv = grossinv + lineext(c%)
                   if taxable$(c%) = "Y" then                            ~
                                        taxable = taxable + lineext(c%)
L05600:         next c%

/*CR1993*/  if hdmet = 0 and hdidx = 0 then goto L05605

             hdmetamt = round(grossinv * hdmet, 2)
             hdidxamt = round(grossinv * hdidx, 2)
/*CR1993*/             
L05605:     invdiscamt = round(grossinv * invdiscpct * .01, 2)
/*CR1993*/  invdiscamt = invdiscamt + hdmetamt + hdidxamt
            invdiscamt = -invdiscamt
            taxabledsc = round(taxable  * invdiscpct * .01, 2)
            taxable    = taxable - taxabledsc
            taxamt     = round(taxable  * taxpct     * .01, 2)  /* CR1993 here*/
/*AWD006*/  escamt     = round(grossinv * energy     * .01, 2)           
            netinv     = grossinv + invdiscamt + frtamt + taxamt + escamt 
            
            posthny$ = "N"

        return

        save_data
            f%  = 1% : invtype$ = "O"
            f1% = f% + 1%

*        Assign Invoice Number if so required.
            gosub assign_invoice
            inpmessage$ = "Saving Invoice " & invnr$ & "..."
            print at(04,02), str(inpmessage$)
/* (AWD002) check for sales order, seq already in file, delete */
            gosub orabol_check

*        Now throw the invoice into the files, lines 1st then Hdr.
            seq% = 0%
            for c% = 1% to maxlines%

                seq% = seq% + 1%  : convert seq% to seq$(c%), pic(###)
                for l% = 1% to 30%
                     lot$  (l%) = lots$  (c%,l%)
                     lotqty(l%) = lotqtys(c%,l%)
                next l%
                if nonstockmsg$(c%) <> " " then nonstockmsg$(c%) = "Y"


                openqty(c%) = 0.0                        /* CLOSE S.O. */
                                                         /* (ARIBUF2 ) */
                write #f1% using L03600 , cuscode$, invnr$, seq$(c%),      ~
                     item$(c%), part$(c%), descr$(c%), cat$(c%),         ~
                     order(c%), ship(c%), openqty(c%), pricestk(c%),     ~
                     stkuom$(c%), priceuom$(c%), conv(c%), price(c%),    ~
                     linediscpct(c%), linediscamt(c%), lineext(c%),      ~
                     taxable$(c%), sales$(c%), discs$(c%), " ",          ~
                     textidl$(c%), soseq$(c%), lot$(), lotqty(),         ~
                     nonstockmsg$(c%), project$(c%), sn_lineid$(c%),     ~
                     0, 0, demtype$(c%), itemid$(c%), unitid$(c%), " ",  ~
                     gs128$(c%), " "
/* (AWD003) write itemid and unitid to aribuf2 */

/* (AWD002) */
                write #20, using L03605, "S", date, load$, cuscode$, so$,    ~
                         seq$(c%), orderdate$,                               ~
                         shipdate$, quote$, part$(c%), str(epart$(c%),26,20),~
                         str(epart$(c%),46,20), order(c%), ship(c%),         ~
                         invdiscpct, linediscpct(c%), price(c%), lineext(c%),~
                         cat$(c%), region$, salesman$(1), duedate$, invnr$,  ~
                         job$, po$, itemid$(c%), unitid$(c%), gs128$(c%) 
/* (AWD003) write itemid and unitid to orabol */ 



/* (AWD005) */
            put #21, using SHPAUDIT_FMT, date, time, cuscode$, so$, seq$(c%), ~
                   bol$, invnr$, po$, shipdate$, userid$, session$, ~
                   load$, program$, "save_data", " "
                   
            write #21, eod goto NoAuditWrite
            
                   
        
SHPAUDIT_FMT:  FMT CH(06), CH(08), CH(09), CH(08), CH(03), CH(03), CH(09), ~
                   CH(16), CH(06), CH(03), CH(06), CH(05), CH(09), CH(20), ~
                   CH(128) 
                   
NoAuditWrite:                   
/* (\AWD005) */

            next c%

            bal = 0  :  /* Filler */

            init(" ") xx$                         /* (EWD001)   */
            date%  = 0%
            xx$ = shipdate$
            call "DATEOKC" (xx$, date%,errormsg$)
            if date% = 0% then init(" ") xx$
            if date% = 0% then xx$ = postdate$
            call "DATUFMTC" (xx$)
            init(" ") shipdate$
            shipdate$ = str(xx$,1%,6%)            /* (EWD001)   */            

            put #f% using L03020 , cuscode$, invnr$, po$, so$, bol$,       ~
                shipto$(), soldto$(), shipdate$, howship$, fob$,         ~
                carrier$, cartons, weight, frtbill$, salesman$(),        ~
                comm%(), region$, pc$, invdate$, expdate$, " ", date,    ~
                userid$, vf$, aracct$, frtacct$, taxacct$, salesacct$,   ~
                discacct$, grossinv, invdiscpct, invdiscamt, frtamt,     ~
                taxamt, netinv, bal, billxref$, stlmnt$, store$,         ~
                taxcode$, taxpct, invtype$, invrsn$, textid$, posthny$,  ~
                " ", artype$, terms$, termsdue(), termsdiscs(),          ~
                termsdisc$(), termsnet$(), custype$, acctxref$,          ~
                currency$, " "
                                                    /* (AWD006) */
            if f% = 1% then put #1 using L06200,energy, escamt, escacct$,     ~
                                                taxrate, pgpo$, hdmet, hdidx, ~
                                                session$, invtype$,           ~
                                                cuscode$, invnr$

L06200:         FMT POS(1811), PD(14,4), PD(14,4), CH(09),  ~
                    PD(14,4), CH(20), PD(14,4), PD(14,4),           ~
                    POS(2001), CH(6), CH(1), CH(9), CH(8)
                                                         /* (ARIBUFFR) */
            write #f%

*        Flag BOL as invoiced ("O" Types with BOL specified)
                                                         /* (SHPHDRS ) */
            readkey$ = str(cuscode$) & str(so$) & bol$
            read #9,hold,key = readkey$, eod goto L06330
               put #9, using L06290 , invnr$
L06290:           FMT POS(234), CH(8)
               rewrite #9

*        Clear SO In-process and go get next invoice
L06330: 
        return

        lookup_subpart                            /* PAR000  */
            init(" ") bcksubpt_rec$, flds$(), info_flds$()
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1" 

            convert so_inv$ to so_inv%, data goto sub_part1
sub_part1:
            convert item_no$ to item_no%, data goto sub_part2
sub_part2:
            convert so_inv% to so_inv$, pic(00000000)
         
            convert item_no% to item_no$, pic(###)

REM         call "SHOSTAT" (" SO AND ITEM " & so_inv$ & item_no$)   stop

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #19,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */
           
            if err1% <> 0% then                                          ~
                   str(bcksubpt_rec$,48%,20%) = "00000                    "

            if err1% = 0% then return
            return

            errormsg$ = "BCKSUBPT ERR= "&so_inv$ & " Line= " & item_no$  ~
                                      & " Flag= " & flag$


        return                                    /* PAR000 */

        orabol_check                             /* (AWD002) */
          init(" ") readkey2$
          readkey2$ = so$
          
         orabol_next
          read #20, hold, key 3 > readkey2$, using orabol_fmt, readkey2$, ~
                                                      eod goto orabol_done
orabol_fmt:          FMT POS(22), CH(11)

              if str(readkey2$,1,8) <> str(so$,1,8) then goto orabol_done

                   delete #20
                   goto orabol_next

        orabol_done
        return

        exit_program

           end



