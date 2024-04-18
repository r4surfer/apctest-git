        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  X   X  N   N  PPPP   U   U  TTTTT   *~
            *  A   A  R   R    I     X X   NN  N  P   P  U   U    T     *~
            *  AAAAA  RRRR     I      X    N N N  PPPP   U   U    T     *~
            *  A   A  R  R     I     X X   N  NN  P      U   U    T     *~
            *  A   A  R   R  IIIII  X   X  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIXNPUT - Invoice Entry, Export Version                  *~
            *************************************************************~
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
            * 11/05/87 ! Original (Cloned from ARIINPUT)          ! MJB *~
            * 11/17/87 ! Added Multi-currency, CURMASTR, ARICURCY,! JIM *~
            *          !   CURCONVR.                              !     *~
            * 04-14-88 ! Fixed Soft Enables                       ! MJB *~
            * 08/14/88 ! Changed part UOM conversion to PD(14,7)  ! JDH *~
            * 10/20/88 ! Fix in PRICING_STUFF Routine,            ! JDH *~
            *          ! Added Cancelled or credit hold checks.   !     *~
            * 03/06/89 ! Added SHPHDRS to call to ARINUMIN        ! JDH *~
            * 05/31/89 ! Currency type no longer modifiable when  ! MLJ *~
            *          !   invoice type = "O", added READKEY$ to  !     *~
            *          !   line 31220, branch to 31280 instead of !     *~
            *          !   31650 on 31240, added 31280 - 31310    !     *~
            * 10/06/89 ! Added channels to calls to ARIXNPTS,     ! JDH *~
            *          !   Added BCKMINSUB, fixed lot & serial #s.!     *~
            * 10/16/89 ! Added currency rates to be from BCKLNCUR,! JDH *~
            *          !   message on currency effective date     !     *~
            * 11/02/89 ! Over/Under Ship project                  ! JDH *~
            * 01/18/90 ! Changed to test for type 'X' invoices.   ! JDH *~
            * 02/28/90 ! Fixed PIC for exchange rate date.  Added ! JDH *~
            *          !  test for neg. lots when part is not lot !     *~
            *          !  tracked.                                !     *~
            * 06/07/90 ! Fixed error when summary over 85 lines,  ! JDH *~
            *          !  added Allow Dup Inv # flag, changed     !     *~
            *          !  SHPOVRSB qtys, added checks for serial  !     *~
            *          !  #s and no neg lots from BOL. Corrected  !     *~
            *          !  checking of available inventory.        !     *~
            * 06/07/90 ! Added files for location mangmnt. MLJ Mod! JDH *~
            * 11/01/90 ! Added check for STARTOVER before write   ! MJB *~
            *          !  to Print Tickler File.                  ! MJB *~
            * 06/06/91 ! PRR 11594-Added ARIMASTR to ARINUMIN call! JBK *~
            *          ! PRR 11753 & 11161- Moved tax defaults    !     *~
            *          !  ahead of account defaults               !     *~
            *          ! PRR 11614- Delete ARINUMBR in certain    !     *~
            *          !  case where manual invoice # entered     !     *~
            * 03/04/92 ! PRR 12290/1 Add 4th Alt Key to ARIMASTR. ! JDH *~
            *          ! Corrected Carrier Description Variable.  !     *~
            *          ! Fixed Qty Avail test for Cutover BOLs.   !     *~
            * 06/09/92 ! Allow blank lot even w/lot tracked parts.! JDH *~
            *          !  SHPXCFRM will not allow processing of   !     *~
            *          !  these, but it's ok for the time being.  !     *~
            * 07/08/93 ! Added Customer/Part# Xref Coding.        ! JBK *~
            * 07/29/93 ! Merged ARIXNPTS back into this.  ARIXNPTS! JDH *~
            *          !   Mod Block not merged.  See OBSOSUBS lib!     *~
            * 08/17/93 !Support for Extra Line Generation (core,?)! KAB *~
            * 10/20/93 ! Affect Inv? & Adj Reason Code field mods.! JDH *~
            * 03/21/94 ! PRR 13024. Honor Auto-close of SO.       ! JDH *~
            * 01/16/95 ! Precious Metal Surcharge Flags added to  ! RJH *~
            *          !   3rd Header screen & ARIMASTR File.     !     *~
            * 03/14/95 ! PRR 13187 - Use Xref Part Shadow File to ! RJH *~
            *          !   get or save Xref Parts & descrs.       !     *~
            * 04/25/95 ! PRR - 13283 Additional Key to ARIBUFFR.  ! RJH *~
            * 05/25/95 ! PRR 13362 - Use SHPHDRS Store as default ! RJH *~
            *          !   on Invoice if available.               !     *~
            * 09/25/95 ! Re-instate common block for line item    ! KAB *~
            *          !   pass to ARIEXASB                       !     *~
            * 12/14/95 ! Now correctly calcs the remaining open.  ! JDH *~
            * 09/05/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        com                              /* Line Items Pass to ARIEXSB */~
            bolqty(100),                 /* Qty shipped on BOL         */~
            buflot$(100,30)6,            /* Buffer Lots                */~
            bufmark$(100,30)1,           /* Buffer Lot Marker          */~
            bufqty(100,30),              /* Buffer Lot Quantity        */~
            cat$(100)4,                  /* Part Category              */~
            completemsg$(100)30,         /* Flagged Complete in Shippng*/~
            conv(100),                   /* Conversion Pricing->Stockng*/~
            cus_part$(100)25,            /* Customer Part Number       */~
            cus_part_descr$(100)32,      /* Customer Part Number Descr */~
            cus_part_type$(100)1,        /* Customer Part Number Source*/~
            def_percent(100),            /* Default % overship allowed */~
            def_unit(100),               /* Def. units overship allowed*/~
            demtype$(100)1,              /* Demand Type for SA         */~
            descr$(100)32,               /* Part Description           */~
            discs$(100)12,               /* Sales Disc Account- Lines  */~
            e_lines%(100),               /* Extra Lines                */~
            item$(100)3,                 /* P.O. Item Number           */~
            linediscamt(100),            /* Line Item Discount Amt     */~
            linediscpct(100),            /* Line Item Discount %       */~
            lineext(100),                /* Line Item Extensions       */~
            lots$(100,30)6,              /* Lot Numbers                */~
            lotqtys(100,30),             /* Lot Quantities             */~
            lotqtys_orig(100,30),        /* Lot Quantities (Original)  */~
            lsts$(100)1,                 /* Line Status                */~
            mark$(100,30)1,              /* Line Marker                */~
            nonstockmsg$(100)16,         /* Non-Stock Part Flag & Msg  */~
            openqty(100),                /* Open Order Quantity        */~
            order(100),                  /* Original Order Quantity    */~
            part$(100)25,                /* Part Code                  */~
            postlot$(100,30)6,           /* All Ready Posted           */~
            postmark$(100,30)1,          /* All Ready Posted Marker    */~
            postqty(100,30),             /* All Ready Posted Qty.      */~
            price(100),                  /* Unit Price (Pricing UOM)   */~
            pricestk(100),               /* Unit Price (Stockng UOM)   */~
            priceuom$(100)4,             /* Pricing Unit of Measure    */~
            project$(100)8,              /* Project Number             */~
            qtypreinv(100),              /* Total Qty Pre-invoiced     */~
            qtyschld(100),               /* Total Qty Scheduled        */~
            qtyshipped(100),             /* Total Qty Shipped & Invd   */~
            sales$(100)12,               /* Sales Acct- Lines          */~
            seq$(100)3,                  /* Line Item Number           */~
            ship(100),                   /* Quantity Shipped           */~
            sn_index2%(100,30),          /*             LotX Indices   */~
            sn_lineid$(100)4,            /*             Line Seq Nr    */~
            sn_lot1was$(100)6,           /*             Last Values    */~
            soopen(100),                 /* Original Open Qty on SO    */~
            soorder(100),                /* Order Quantity on SO       */~
            soseq$(100)3,                /* SO Seq # X-Ref             */~
            stkuom$(100)4,               /* Stocking Unit of Measure   */~
            taxable$(100)1,              /* Line Taxable? (Y/N)        */~
            textidl$(100)4               /* Text ID- Hdr, Lines        */
        dim                                                              ~
            acct$12, acctdescr$30,       /* General Use Acct Number    */~
            acctxref$9,                  /* Account X-Ref              */~
            affect_hny_msg$23,           /* Affect inventory message   */~
            aracct$12, aracctdescr$30,   /* Net Invoice Account        */~
            artype$1,  artypedescr$30,   /* A/R Type Code              */~
            askmsg$(4)79,                /* Message for ASKUSER prompt */~
            billxref$9,                  /* Bill-to Cross Reference    */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bol$3,                       /* Bill of Lading Shipped     */~
            bookinv$1,                   /* Book Non-So Invoice Flag   */~
            carrier$, carrierdescr$30,   /* Carrier Code               */~
            cartons$10,                  /* # of Cartons Shipped       */~
            catdescr$32,                 /*                            */~
            closeso$1,                   /* Allow Closing SO flag      */~
            comm%(3), comm$(3),          /* Commission Split %s        */~
            conv$10,                     /* Conversion Pricing->Stockng*/~
            convdate$6,                  /* Currency conversion date   */~
            crhold$1,                    /* Order Credit Status        */~
            currdflt$4,                  /* Customer default currency  */~
            curr_hdr$11,                 /* Currency header for Summary*/~
            currkey$50,                  /* Currency lines read key    */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4, currdesc$(2)32,  /* Currency code& description */~
            currmsg$33,                  /* Currency rate effective msg*/~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Code              */~
            cusdescr$30,                 /* Customer Descr (Sort Name) */~
            custaxable$1,                /* Customer Taxable? (Y/N)    */~
            custype$2,                   /* Customer Type Code         */~
            date$8,                      /* Today's Date               */~
            delemsg$30,                  /* Delete Message             */~
            dfac$1,                      /* Display FAC for Affect HNY */~
            dfltstr$3,                   /* Default Store for 1st time */~
            discacct$12,                 /* Sales Discounts Account    */~
            discacctdescr$32,            /*                            */~
            discsdescr$30,               /*                            */~
            dsply_part$(100)25,          /* Display Part Number        */~
            dsply_descr$(100)32,         /* Display Part Number Descr  */~
            dup_inv$1,                   /* Allow duplicate invoice #s */~
            edtmessage$79,               /* Edit screen message        */~
            effdate$8,                   /* Currency effective date    */~
            errormsg$79,                 /* Error message              */~
            expdate$8,                   /* Recurring Expiration Date  */~
            fcamt$10,                    /* Inv Amount- Fin Chgs       */~
            fd_expdate$26,               /* Field Descr- Exp Date      */~
            fd_fc$26,                    /*              Fin Chg Amt   */~
            fd_invdate$26,               /*              Invoice Date  */~
            fd_invnr$26,                 /*              Invoice Numbr */~
            fd_orderqty$26,              /*              Order Qty     */~
            fd_so$26,                    /*              SO, BOL       */~
            fob$20,                      /* FOB                        */~
            frtacct$12, frtacctdescr$30, /* Freight Account Number     */~
            frtamt$10,                   /* Freight Charges            */~
            frtbill$20,                  /* Freight Bill               */~
            hdr$(6,2)25,                 /* Summary Screen Headings    */~
            hnymsg$30,                   /* Inventory Inactive Message */~
            howship$20,                  /* How Ship                   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invdate$8,                   /* Invoice Date               */~
            invdiscamt$10,               /* Invoice Discount- Amount   */~
            invdiscpct$5,                /* Invoice Discount- Percent  */~
            invnr$8,                     /* Invoice Number             */~
            invrsn$9, invrsndescr$30,    /* Invoice Reason             */~
            invtypedescr$30,             /*                            */~
            lfac$(20)1, lfac2$1,         /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            linediscamt$10,              /*                            */~
            linediscpct$5,               /*                            */~
            lineext$10,                  /* Line Item Extensions       */~
            lotmsg$15,                   /* Lot not on file message    */~
            lot$(30)6,                   /* Lot Numbers                */~
            lotqty(30),                  /* Lot Quantities             */~
            lotqty$10,  lotqtys$(30)10,  /* 1st Lot Qty / pass qtys    */~
            mfg$(1)9,                    /* Manufacture's Code         */~
            mfgdescr$30,                 /* PLOWCODE Description       */~
            msg$79,                      /* Misc use Message           */~
            openqty$10,                  /* Open Order Quantity        */~
            order$10,                    /* Original Order Quantity    */~
            partflag$4,                  /* Special/Obsolete Flag      */~
            parttaxable$1,               /* Part Taxable? (Y/N/ )      */~
            pc$1, pcdescr$32,            /* Price Code                 */~
            pf$(3)79, pfkey$32,          /* PF Prompts and Keys        */~
            pgmname$8,                   /* Name based on Invoice Type */~
            plowdescr$79,                                                ~
            pm_hdr_dsply$30,pm_hdr_dsply2$10, /* Display header var    */~
            pm_inv$1,                    /* Precious Metal INV Flag    */~
            pm_sys_inv$1,                /* Precious Metal INV Flag    */~
            pm_on$1,                     /* Precious Metal ON Flag     */~
            pm_so$1,                     /* Precious Metal SO Flag     */~
            pm_sys_so$1,                 /* Precious Metal SO Flag     */~
            po$16,                       /* Purchase Order Number      */~
            poreqd$1,                    /* PO Required?               */~
            postdate$8,                  /* Post Date (from Session)   */~
            posthny$1,                   /* Post Inventory?            */~
            price$10,                    /* Unit Price (Pricing UOM)   */~
            pricestk$10,                 /* Unit Price (Stockng UOM)   */~
            priceuomdescr$32,            /* Pricing Unit of Measure    */~
            projectdescr$30,             /* Project Description        */~
            prtmsg$78,                                                   ~
            readkey$50, readkey1$100,    /* Misc Read Keys             */~
                        readkey2$100,    /*                            */~
            region$4,                    /* Region Code                */~
            regiondescr$32,              /*                            */~
            salesdescr$30,               /*                            */~
            salesacct$12,                /* Sales Acct- Header         */~
            salesacctdescr$30,           /*                            */~
            salesman$(3)4,               /* Salesman Code / Split %    */~
            salesmandescr$(3)30,         /*                            */~
            scr%(5,15), set%(255),       /* Soft Enables / Screen Refs */~
            session$6,                   /* Session Number             */~
            sext$(15)10,                 /* Summary- Extension         */~
            sfac$(15)1,                  /* Summary- FACs              */~
            ship$10,                     /* Quantity Shipped           */~
            shipdate$8,                  /* Date Shipped               */~
            shipto$(6)30,                /* Ship-to                    */~
            sn_index1%(30),              /* Serial Nos- Temp Indices   */~
            sn_last_lineid$4,            /*             High Seq Nr    */~
            sn_loc$30,                   /*             Location       */~
            sn_trankey$40,               /*             Trans Key      */~
            so$16,                       /* Sales Order Number         */~
            soinbuffr$16,                /* SO in buffer flag          */~
            soldto$(6)30,                /* Sold-to                    */~
            sship$(15)10,                /* Summary- Open Qty          */~
            soseqmsg$12,                 /* SO Seq Message             */~
            statutory$4,                 /* Statutory currency code    */~
            stkuomdescr$32,              /*                            */~
            stlmnt$12,                   /* Settlement Number          */~
            store$3, storedescr$32,      /* Store Code                 */~
            strs$(30)3,                  /* Stores for LOTDIST         */~
            taxacct$12, taxacctdescr$30, /* Sales Tax Account          */~
            taxcode$10, taxcodedescr$30, /* Sales Tax Code             */~
            taxpct$5,                    /* Sales Tax Percent          */~
            temp$16,                     /* Misc Temp Variable         */~
            terms$20, termsdescr$30,     /* Payment Terms (Code)       */~
            termsdue(30),                /* Dated- Amounts Due         */~
            termsdiscs(30),              /*      - Cash Disc Percents  */~
            termsdisc$(30)6,             /*      - Disc Due Dates      */~
            termsnet$(30)6,              /*      - Net  Due Dates      */~
            text$(113,1)70,              /* Text Array                 */~
            textid$4,                    /* Text ID- Hdr, Lines        */~
            userid$3,                    /* Current User Id            */~
            vf$200,                      /* Variable Fields            */~
            weight$10,                   /* Shipment Weight            */~
            xfac$1                       /* Field Attribute Characters */

        dim                                                              ~
            f2%(64),                     /* = 0 if the file is open    */~
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

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CUSTOMER ! Customer Master File                     *~
            * #02 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #03 ! GLMAIN   ! General Ledger Main File                 *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! BCKMASTR ! Backlog master file                      *~
            * #06 ! BCKLINES ! Back Log Line Item File                  *~
            * #07 ! SLMMASTR ! Salesman master file                     *~
            * #08 ! GENCODES ! General Codes File                       *~
            * #09 ! ARIBUFFR ! Invoice Buffer- Headers                  *~
            * #10 ! ARIBUF2  ! Invoice Buffer- Lines                    *~
            * #11 ! CATEGORY ! Inventory category codes file            *~
            * #12 ! STORNAME ! Store Info File - Name/Address           *~
            * #13 ! HNYQUAN  ! Inventory Part / Store / Lot Quantity Fi *~
            * #14 ! JOBMASTR ! WIP/JC Job Master File                   *~
            * #15 ! ARIMASTR ! Invoice Master- Headers                  *~
            * #16 ! ARILINES ! Invoice Master- Line Items               *~
            * #17 ! SHPHDRS  ! Shipment Schedule- Headers               *~
            * #18 ! SHPLINES ! Shipment Schedule- Lines                 *~
            * #19 ! BCKBUFFR ! Sales Order Buffer- Headers              *~
            * #20 ! STXCODES ! Sales TAx Codes File                     *~
            * #21 ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #22 ! TXTFILE  ! System Text File                         *~
            * #23 ! ARINUMBR ! Duplicate Invoice Number Control File    *~
            * #24 ! HNYGENER ! Generic Xref File                        *~
            * #25 ! ARMTRIAL ! A/R Trial Balance                        *~
            * #26 ! ARIINVRF ! Invoice Print Tickler File               *~
            * #27 ! USERINFO ! User's Posting Dates                     *~
            * #28 ! HNYLOCNS ! Locations Quantity Detail File           *~
            * #29 ! LOCATION ! Location Master File                     *~
            * #30 ! CUSPTXRF ! Customer Part Number Cross Reference Fil *~
            * #32 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #33 ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #34 ! SERTIF   ! Serial #'s Trans Image File (Buffer)     *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #41 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #42 ! ARIMSCUR ! Currency-specific ARI Master             *~
            * #43 ! ARILNCUR ! Currency-specific ARI lines              *~
            * #44 ! BCKLNCUR ! Currency-specific BCK lines              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #04, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #05, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #06, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #07, "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #08, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #09, "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos = 1, keylen =   17,                       ~
                        alt key  1, keypos = 2001, keylen =   24,        ~
                            key  2, keypos =   34, keylen =   16, dup

            select #10, "ARIBUF2",                                       ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =   1,  keylen = 20

            select #11, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #12, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #13, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #14, "JOBMASTR",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =   1,  keylen =  8

            select #15, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alternate key 1, keypos = 10, keylen =  8, dup,  ~
                                  key 2, keypos = 18, keylen = 16, dup,  ~
                                  key 3, keypos = 34, keylen = 16, dup,  ~
                                  key 4, keypos = 1783, keylen = 26

            select #16, "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #17, "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =  1,   keylen = 28

            select #18, "SHPLINES",                                      ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =   10, keylen =  22

            select #19, "BCKBUFFR",                                      ~
                        varc,     indexed,  recsize = 1020,              ~
                        keypos =    1, keylen =   10,                    ~
                        alt key  1, keypos =    4, keylen =   7, dup,    ~
                            key  2, keypos =   30, keylen =  16

            select #20, "STXCODES",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  10

            select #21, "ARMTERMS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  20

            select #22, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #23, "ARINUMBR",                                      ~
                        varc,     indexed,  recsize =   17,              ~
                        keypos =  1,   keylen = 17,                      ~
                        alt key  1, keypos =    10, keylen =  8, dup

            select #24, "HNYGENER",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41

            select #25, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21

            select #26, "ARIINVRF",                                      ~
                        varc,     indexed,  recsize =   20,              ~
                        keypos =    1, keylen =  20

            select #27, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos = 1, keylen =  3

            select #28, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos = 1, keylen = 42,                         ~
                        alt key  1, keypos =  432, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42

            select #29, "LOCATION",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos = 1, keylen = 11,                         ~
                        alt key  1, keypos =    4, keylen =  11

            select #30, "CUSPTXRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   26, keylen =  35,                     ~
                        alt key  1, keypos =  1, keylen = 60

            select #32, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #33, "SERWORK",                                       ~
                        varc,     indexed,  recsize =  48,               ~
                        keypos = 1, keylen = 23

            select #34, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #41, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            select #42, "ARIMSCUR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =   5,  keylen = 17,                      ~
                        alt key  1, keypos =   1, keylen =  21

            select #43, "ARILNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   5,  keylen = 20,                      ~
                        alt key  1, keypos =   1, keylen =  24

            select #44,  "BCKLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 19,                      ~
                        alt key  1, keypos =   1, keylen =  23

        call "SHOSTAT" ("Opening Files, One Moment Please")
            mat f1% = zer
            f1%( 9), f1%(15), f1%(19), f1%(42) = 200%
            f1%(10), f1%(16), f1%(43) = 400%
            f1%(23), f1%(24), f1%(25), f1%(26) = 100%
            for f% = 1% to 30%
              call "OPENCHCK" (#f%, fs%(f%), f2%(f%), f1%(f%), rslt$(f%))
            next f%

*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            call "READ100" (#02, "SWITCHS.CUR", f1%(2))
                if f1%(2) = 0% then L03950
            get #02 using L03870, curr$, statutory$, currtype$
L03870:         FMT POS(21), CH(1), CH(4), POS(29), CH(1)
            if curr$ <> "Y" then statutory$ = " "
            if curr$ <> "Y" then goto L03950
            for f% = 40% to 44%
              call "OPENCHCK" (#f%, fs%(f%), f2%(f%), f1%(f%), rslt$(f%))
            next f%

L03950
*        Check for Cust/Mfg Part Cross Reference
            pxref% = fs%(30%)  :  if pxref% < 0% then pxref% = 0%
            if pxref% <> 1% then L09000
            readkey1$ = "C"
            call "PLOWNEXT" (#30, readkey1$, 1%, cxref%)
            readkey1$ = "M"
            call "PLOWNEXT" (#30, readkey1$, 1%, mxref%)

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date  :  call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#27, userid$, f1%(27%))
            if f1%(27%) = 1% then get #27 using L09076, dfltstr$
L09076:         FMT POS(64), CH(3)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

                invtypedescr$  = "EXPORT INVOICE"
                       fd_so$  = "Sales Order - BOL"
                if fd_invnr$   = " " then fd_invnr$ = "Invoice Number"
                if fd_invdate$ = " " or fd_invdate$ = blankdate$ then ~
                   fd_invdate$ = "Invoice Date"
                pgmname$ = "ARIINPTX"
                if curr$ = "Y" then curr_hdr$ = "Currency in"            ~
                               else curr_hdr$ = " "
                askmsg$(4) = "This part MUST be manually entered.  " &   ~
                             "Press any PF key to confirm."

*        Clear any in-process flag for invoicing of a Sales Order.
            readkey$ = all(hex(00))  :  str(readkey$,,3) = userid$
            call "READ101" (#19, readkey$, f1%(19)) /* BCKBUFFR */
            if f1%(19) = 0% then L09580
                so$ = key(#19, 2%)
                get #19 using L09490, readkey$
L09490:              FMT XX(10), CH(9)
                if readkey$ = "ARIXNPUT" then L09530
                     call "ALLFREE"
                     goto L09580
L09530:         delete #19
                call "ASKUSER" (keyhit%, "RESTART NOTE",                 ~
                       "You were working on the Sales Order Shown Below",~
                       "Press (RETURN) To Continue", so$)

L09580
*        Now set up the pseudo session number
            session$ = all(hex(00))

*        Set some flags and misc variables
            gosub init_enables

            mat redim mfg$(1%)1%

            readkey$ = all(hex(00))      /* GENCODES Directory         */
            str(readkey$, 10) = "UOM"
            call "READ100" (#08, readkey$, uom%)

            call "BCKSWTCH" ("AR ", "BOOKINV ", bookinv$, temp, u3%)
            call "BCKSWTCH" ("AR ", "HNYACTVE", hnymsg$ , temp, u3%)
            call "BCKSWTCH" ("BCK", "CLOSESO ", closeso$, temp, u3%)
            call "BCKSWTCH" ("BCK", "OVRSHIP%", temp$, sys_def_percent,  ~
                              u3%)
                if hnymsg$ = "N" then hnymsg$ = "* Inventory Inactive *" ~
                                 else hnymsg$ = " "
                affect_hny_msg$ = "Affect Inventory?"
                if hnymsg$ <> " " then                                   ~
                              affect_hny_msg$ = "Inventory NOT Affected!"
            call "BCKSWTCH" ("AR ", "DUP_INV ", dup_inv$, temp, u3%)
            lot%    = 6%
            hdr$(1%,1%) = "Seq"         :  hdr$(1%,2%) = "Seq"
            hdr$(2%,1%) = "Part Number"
            hdr$(3%,1%) = "Part Description"
            hdr$(4%,1%) = "  Quantity"  :  hdr$(4%,2%) = "  Quantity"
            hdr$(5%,1%) = " Extension"  :  hdr$(5%,2%) = " Extension"
            hdr$(6%,1%) = "S"           :  hdr$(6%,2%) = "S"
            hdr$(2%,2%) = "Customer's Part Number"
            hdr$(3%,2%) = "Customer's Part Descr"

*        See if operator is an administrator or not
            call "CMSMACHK" ("ARM", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%

*        Check if Precious Metal Surcharge is on
            pm_on$, pm_sys_so$, pm_sys_inv$ = "N"
            call "READ100" (#02, "SWITCHS.BCK", f1%(02%))
            if f1%(02%) = 1% then get #02 using L09930, pm_on$,            ~
                                                  pm_sys_so$, pm_sys_inv$
L09930:         FMT POS(60), 3*CH(1)
            pm_hdr_dsply$, pm_hdr_dsply2$ = " "
            if pm_on$ <> "Y" then L10000
                  pm_hdr_dsply$  = "PM Surcharge at SO?"
                  pm_hdr_dsply2$ = "at INV?"

L10000: REM *************************************************************~
            *       I N P U T   M O D E   -  H E A D E R S              *~
            *-----------------------------------------------------------*~
            * Handles normal input for header screens.                  *~
            *************************************************************
        inputmode
            gosub init_for_input

*        Input subroutine to get Customer, Invoice, SO & BOL.
L10090:     call "ARINUMIN" ("X",      invtypedescr$, fd_invnr$,         ~
                             session$, postdate$, #9, #5, #6, #1, #17,#8,~
                             #25, #15, errormsg$, cuscode$, invnr$,      ~
                             so$, bol$, keyhit%)
            if keyhit% = 16% then exit_program
            get #1 using L10160, soldto$(), pm_so$, pm_inv$,              ~
                                           shipto$(), billxref$, temp$,  ~
                currdflt$
L10160:         FMT XX(39), 6*CH(30), POS(226), 2*CH( 1),                ~
                                      POS(253), 6*CH(30), POS(780),      ~
                    CH(9), POS(793), CH(1), POS(1045), CH(4)
            if pm_on$ <> "Y" then L10175
            if pm_so$  = " " then pm_so$  = pm_sys_so$
            if pm_inv$ = " " then pm_inv$ = pm_sys_inv$

L10175:     readkey1$ = "C" & cuscode$
            call "PLOWNEXT" (#30, readkey1$, 10%, c_cxref%)

            if curr$ <> "Y" then currdflt$ = " " /* Says Invisible Man */
            gosub'051              /* Screens 1                        */
            gosub load_invoice
            if errormsg$  <> " " then L10090
            if invonfile%  =  0% then L10260
                goto edithdr1
L10260:     gosub load_sales_order
            if errormsg$ <> " " then L10090
                gosub test_ar_acct
                if errormsg$ = " " then edithdr1
                fieldnr% = 8%
                goto so_ar_acct_test_failed   /* In Header 3 */

            mostin% = 0% : least% = 5%
            for fieldnr% = 5% to 10%     /*-----HEADER SCREEN 1--------*/
                gosub'050(1%, fieldnr%, 1%)
                      if enabled% =  0% then L10480
                      if fieldnr% = 10% then L10510
L10380:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10470
                         if fieldnr% <= least% then L10470
L10420:                  fieldnr% = max(least%, fieldnr% - 1%)
                         gosub'050(1%, fieldnr%, 1%)
                         if enabled% = 1%     then L10380
                         if fieldnr% = least% then L10380
                         goto L10420
L10470:               if keyhit% <>  0 then       L10380
L10480:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10380
                      mostin% = max(mostin%, fieldnr%)
L10510:     next fieldnr%


            mostin% = 0%
            for fieldnr% = 1% to 9%      /*-----HEADER SCREEN 2--------*/
*              IF FIELDNR% > MOSTIN% THEN GOSUB'052(FIELDNR%)
                gosub'050(2%, fieldnr%, 1%) /* Set enables, input msg */
                      if enabled% = 0% then L10680
L10590:         gosub'102(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10670
L10620:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(2%, fieldnr%, 1%)
                         if enabled% = 1% then L10590
                         if fieldnr% = 1% then L10590
                         goto L10620
L10670:               if keyhit% <>  0 then       L10590
L10680:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10590
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

            mostin% = 0%
            for fieldnr% = 1% to 13%     /*-----HEADER SCREEN 3--------*/
                if fieldnr% > mostin% then gosub'053(fieldnr%)
                gosub'050(3%, fieldnr%, 1%) /* Set enables, input msg */
                      if enabled% = 0% then L10870
L10780:         gosub'103(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10860
L10810:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(3%, fieldnr%, 1%)
                         if enabled% = 1% then L10780
                         if fieldnr% = 1% then L10780
                         goto L10810
L10860:               if keyhit% <>  0 then       L10780
L10870:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10780
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

*        Do Input for Variable Fields   /*-----HEADER SCREEN 4--------*/
            call "VFINPSUB" ("ARIMASTR", "I", invtypedescr$,             ~
                             str(line2$,,60), "NN", vf$, keyhit%)
            if keyhit% = 1% then startover2


        REM *************************************************************~
            *       I N P U T  /  A P P E N D   L I N E S               *~
            *-----------------------------------------------------------*~
            * Handles input or appending of line Items.                 *~
            *************************************************************
        appendlines
L11070:     if maxlines%                  >= 100% then L11240
            if maxlines% + total_e_lines% <  100% then L11080
               gosub extra_load_conflict
L11080:         c% = maxlines% + 1%
                     temp% = 0%
                     convert sn_last_lineid$ to temp%, data goto L11110
L11110:              temp% = temp% + 1%
                     convert temp% to sn_lineid$(c%), pic(0000)
                     sn_last_lineid$ = sn_lineid$(c%)
                gosub clear_line
                     fd_orderqty$ = "Sales Analysis Booking Qty "
                gosub inputline
                if keyhit% = 16% then endinsert
                     maxlines% = maxlines% + 1%
                     convert maxlines% to seq$(c%), pic(###)
                     lsts$(c%) = "A"
                     goto L11070
            endinsert
                gosub clear_line
L11240:         t% = max(0%, min(85%,maxlines%-12%))  /* Last Screen   */
                if maxlines% = 0% then edithdr1 else line_summary

        inputline
            mostin% = 0%
            for fieldnr% = 1% to 14%     /*-----LINE SCREEN 1 (#4)-----*/
                if fieldnr% > mostin% then gosub'054(fieldnr%)
                gosub'050(4%, fieldnr%, 1%)
                      if enabled% = 0 then L11440
L11330:         gosub'104(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub restart_line
                      if keyhit% <>  4 then       L11420
L11370:                  fieldnr% = max(2%, fieldnr% - 1%)
                         gosub'050(4%, fieldnr%, 1%)
                         if enabled% = 1% then L11330
                         if fieldnr% = 2% then L11330
                         goto L11370
L11420:               if keyhit%  = 16 and fieldnr% = 1% then return
                      if keyhit%  = 22% then gosub customer_part
                      if keyhit% <> 23% then L11430
                          gosub manufactures_part
                          if errormsg$ <> " " then L11330
L11430:               if keyhit% <>  0 then       L11330
L11440:         gosub'154(fieldnr%, 1%)
                      if errormsg$ <> " " then L11330
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

            mostin% = 0%
            for fieldnr% = 1% to 4%      /* Second Line Item Screen    */
                if fieldnr% > mostin% then gosub'055(fieldnr%)
                gosub'050(5%, fieldnr%, 1%)
                      if enabled% = 0 then L11650
L11540:         gosub'105(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub restart_line
                      if keyhit% <>  4 then       L11630
L11580:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(5%, fieldnr%, 1%)
                         if enabled% = 1% then L11540
                         if fieldnr% = 1% then L11540
                         goto L11580
L11630:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L11540
L11650:         gosub'155(fieldnr%)
                      if errormsg$ <> " " then L11540
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

            return


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edithdr1
            least% = 4%
            gosub'050(1%, 0%, 2%)       /* Set input message           */
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       line_summary
                  if keyhit%  =  5 then       edithdr2
                  if keyhit%  = 12 then gosub delete_invoice
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 25 then gosub edit_text_hdr
                  if keyhit%  = 29 then       L12095
                  if keyhit% <>  0 then       edithdr1
L12095:     fieldnr% = cursor%(1) - 5%
                if fieldnr% <   least% or fieldnr% > 14% then edithdr1
                if fieldnr% >=  5% and fieldnr%  <= 10% then fieldnr% = 5%
                if fieldnr%  =  5% and cursor%(2) > 40% then fieldnr% = 6%
                if fieldnr% >= 11% then fieldnr% = fieldnr% - 4%
                if fieldnr%  = lastfieldnr% then edithdr1
            if keyhit% <> 29% then L12140
                gosub'049(1%, fieldnr%)
                goto edithdr1
L12140:     gosub'050(1%, fieldnr%, 2%) /* Set input message, enables  */
                  if enabled% = 0% then       edithdr1
L12150:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12150
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12150
                          lastfieldnr% = fieldnr%
                          goto L12095

        edithdr2
            gosub'050(2%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'102(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       line_summary
                  if keyhit%  =  4 then       edithdr1
                  if keyhit%  =  5 then       edithdr3
                  if keyhit%  = 12 then gosub delete_invoice
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 25 then gosub edit_text_hdr
                  if keyhit%  = 29 then       L12250
                  if keyhit% <>  0 then       edithdr2
L12250:     fieldnr% = cursor%(1) - 5
                if fieldnr% <   1% or   fieldnr% >   9% then edithdr2
                if fieldnr%  = lastfieldnr% then edithdr2
            if keyhit% <> 29% then L12280
                gosub'049(2%, fieldnr%)
                goto edithdr2
L12280:     gosub'050(2%, fieldnr%, 2%)
                  if enabled% = 0% then       edithdr2
L12290:     gosub'102(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12290
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12290
                     lastfieldnr% = fieldnr%
                     goto L12250

        edithdr3
            gosub'050(3%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'103(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       line_summary
                  if keyhit%  =  4 then       edithdr2
                  if keyhit%  =  5 then       editvfs
                  if keyhit%  = 12 then gosub delete_invoice
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 25 then gosub edit_text_hdr
                  if keyhit%  = 29 then       L12395
                  if keyhit% <>  0 then       edithdr3
L12395:     fieldnr% = cursor%(1%) - 4%
                if fieldnr% <   1% or   fieldnr% >  15% then edithdr3
                if fieldnr% >=  3% and  fieldnr% <=  4% then fieldnr% = 2%
                if fieldnr% >   2% then fieldnr%  = fieldnr% - 2%
                if fieldnr%  = lastfieldnr% then edithdr3
            if keyhit% <> 29% then L12435
                gosub'049(3%, fieldnr%)
                goto edithdr3
L12435:     gosub'050(3%, fieldnr%, 2%)
                  if enabled% = 0% then       edithdr3
        so_ar_acct_test_failed
L12445:     gosub'103(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12445
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L12445
                     lastfieldnr% = fieldnr%
                     goto L12395

        editvfs      /* Edit Variable Fields       */
            call "VFINPSUB" ("ARIMASTR", "E", invtypedescr$,             ~
                             str(line2$,,60), "YN", vf$, keyhit%)
            if keyhit% =  1% then startover2
            if keyhit% =  4% then edithdr3
            if keyhit% = 16% then datasave
                             goto edithdr1


        REM *************************************************************~
            *           E D I T   M O D E   -  L I N E S                *~
            *-----------------------------------------------------------*~
            * Handles edit mode for line items.                         *~
            *************************************************************

        line_summary              /* Summary Screen */
            if maxlines% = 0% then appendlines
            gosub'050(9%, 0%, 1%)
            inpmessage$ = "To Modify a Line Item, Position Cursor and" & ~
                          " press RETURN."

L13120:     gosub'119
              pf8_flag% = 0%
              errormsg$ = " "
              if keyhit% = 1 then gosub startover
              if keyhit% = 2 then t%=0%
              if keyhit% = 3 then t%=max(0%,min(85%,maxlines%-15%))
              if keyhit% = 4 then t%=max(0%,t%-12%)
              if keyhit% = 5 then t%=max(0%,min(85%,t%+12%,maxlines%-12%))
              if keyhit% = 6 then t%=max(0%,t%-1%)
              if keyhit% = 7 then t%=max(0%,min(85%,t%+1%,maxlines%-1%))
               t% = max(0%, min(t%, maxlines%-15%))
              if keyhit% = 9 then       edithdr1
              if keyhit% =11 then       appendlines
              if keyhit% =12 then       L13270
              if keyhit% =16 then       datasave
              if keyhit%<> 0 and keyhit% <> 8% then L13120
L13270:     c% = cursor%(1) - 5%
            if c% < 1% or c% > 15% then L13120
                c% = c% + t%
                if c% < 1% or c% > maxlines% then L13120
            if keyhit%  = 12 then gosub sdelete_line
            if keyhit% <>  0 and keyhit% <> 8% then L13120

*        Modify Line C%
            if lsts$(c%) <> "D" then L13420
                keyhit1% = 2%
                call "ASKUSER" (keyhit1%, "REACTIVATE LINE",             ~
                      "Enter PF-16 to Reactive Deleted Line Item",       ~
                      "- OR - ", "Hit any PF-Key to Return to Display.")
                if keyhit1% <> 16% then L13120
                lsts$(c%) = "R"
L13420:     gosub describe_line
            if keyhit% <> 8 then editline1
               fieldnr% = 5%
               pf8_flag% = 1%
               lastfieldnr% = 0%
               goto L13650

        editline1
            gosub'050(4%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'104(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then       editline2
                  if keyhit%  =  8% then gosub set_openqty
                  if keyhit%  =  9 then       edithdr1
                  if keyhit%  = 12 then gosub delete_line
                  if keyhit%  = 16 then       line_summary
                  if keyhit%  = 25 then gosub edit_text_line
                  if keyhit%  = 29 then       L13560
                  if keyhit% <> 18% then       L13550
                     if e_lines%(c%) <= 0% then L13544
                        total_e_lines% = total_e_lines% - e_lines%(c%)
L13544:              call "ARIEXTRA" (cuscode$, part$(c%), " ",          ~
                                      e_lines%(c%), #2)
                        total_e_lines% = total_e_lines% + e_lines%(c%)
L13550:           if keyhit% <>  0 then       L13611
L13560:     fieldnr% = cursor%(1) - 5%
                if fieldnr% < 2% or fieldnr% > 14% then L13611
                if fieldnr% <> lastfieldnr% then L13620
L13611:              if pf8_flag% = 1% then line_summary
                     goto editline1
L13620:     if keyhit% <> 29% then L13650
                gosub'049(4%, fieldnr%)
                goto editline1
L13650:     if fieldnr% = 5% then gosub'054(fieldnr%) /*Get Min Qty/Inc*/
            gosub'050(4%, fieldnr%, 2%)
                  if enabled% = 0% then L13611
L13670:     gosub'104(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L13670
            gosub'154(fieldnr%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13670
                     lastfieldnr% = fieldnr%
                     goto L13560

        editline2
            gosub'050(5%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'105(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       editline1
                  if keyhit%  =  9 then       edithdr1
                  if keyhit%  = 12 then gosub delete_line
                  if keyhit%  = 16 then       line_summary
                  if keyhit%  = 25 then gosub edit_text_line
                  if keyhit%  = 29 then       L13870
                  if keyhit% <>  0 then       editline2
L13870:     fieldnr% = cursor%(1) - 8%
                if fieldnr% < 1% or fieldnr% > 4% then editline2
                if fieldnr% = lastfieldnr% then editline2
            if keyhit% <> 29% then L13930
                gosub'049(5%, fieldnr%)
                goto editline2
L13930:     gosub'050(5%, fieldnr%, 2%)
                  if enabled% = 0% then       editline2
L13950:     gosub'105(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L13950
            gosub'155(fieldnr%)
                  if errormsg$ <> " " then L13950
                     lastfieldnr% = fieldnr%
                     goto L13870

        delete_invoice
            u3% = 2%
            call "ASKUSER" (u3%, "D E L E T E",                          ~
                     "Press PF-16 to DELETE this Document,", "-OR-",     ~
                     "Press RETURN to abort delete and return to Edit.")
            if u3% <> 16% then return
                return clear
                if maxlines% = 0% then L14044
                     for i% = 1% to maxlines%
                          lsts$(i%) = "D"
                     next i%
L14044:         goto datasave

        sdelete_line      /* Delete Line on summary screen             */
            if maxlines% = 0% then return
                gosub'129(c%)
                if keyhit% <> 16% then return
                     i1%, i2% = c%
                     if c% = 0% then i1% = 1%
                     if c% = 0% then i2% = maxlines%
                     for c% = i1% to i2%
                          lsts$(c%) = "D"
                     next c%
                     return


        delete_line  /* Delete the line that we're editing   */
            u3% = 2%
            call "ASKUSER" (u3%, "DELETE LINE",                          ~
                            "Enter PF-16 to flag this line for deletion",~
                            "- OR -", "Any PF key to abort delete." )
            if u3% <> 16% then return
                lsts$(c%) = "D"
                return clear
                goto line_summary

        set_openqty
            openqty(c%) = 0
            call "CONVERT" (openqty (c%),   2.2, openqty$ )
            return

        REM *************************************************************~
            *          M I S C   S U P P O R T   R O U T I N E S        *~
            * --------------------------------------------------------- *~
            * Routines which apply to most of the screen functions.     *~
            *************************************************************

        edit_text_hdr
            call "TXTINSUB" (#22, f2%(22), "015", str(line2$,,60),       ~
                                                       textid$, text$())
            return


        edit_text_line
            call "TXTINSUB" (#22, f2%(22), "016", str(line2$,,60),       ~
                                                  textidl$(c%), text$())
            return


        describe_line     /* Add descriptors to already existing line  */
            lotmsg$ = " "
            call "CONVERT" (order   (c%),   2.2, order$   )
            call "CONVERT" (openqty (c%),   2.2, openqty$ )
            call "CONVERT" (ship    (c%),   2.2, ship$    )
            call "CONVERT" (conv    (c%),   7.7, conv$    )
            call "CONVERT" (lotqtys (c%,1), 2.2, lotqty$  )
            if lots$(c%,1) = " " then lotqty$ = " "
            if soseq$(c%) = " " then L15340
                fd_orderqty$ = "Order Quantity"
                soseqmsg$ = "SO Line: " & soseq$(c%)
                goto L15370
L15340:     fd_orderqty$ = "Sales Analysis Booking Qty "
            soseqmsg$ = " "
L15370:     call "DESCRIBE" (#11, cat$(c%), catdescr$, 0%, f1%(11))
            readkey$ = "UOM      " & stkuom$(c%)
            call "DESCRIBE" (#08, readkey$, stkuomdescr$, 0%, f1%(8))
            readkey$ = "UOM      " & priceuom$(c%)
            call "DESCRIBE" (#08, readkey$, priceuomdescr$, 0%, f1%(8))
            call "GLUNFMT" (sales$(c%))
            call "DESCRIBE" (#03, sales$(c%), salesdescr$, 0%, u3%)
            call "GLFMT" (sales$(c%))
            call "GLUNFMT" (discs$(c%))
            call "DESCRIBE" (#03, discs$(c%), discsdescr$, 0%, u3%)
            call "GLFMT" (discs$(c%))
            call "LOTENABL" (part$(c%), lot_enable%, lot%, #02, #04)
            if lots$(c%,1) = " " then L15520
                readkey$ = str(part$(c%)) & str(store$) & lots$(c%,1%)
                call "READ100" (#13, readkey$, f1%(13))
                if f1%(13) = 0% then lotmsg$ = "Lot not on file"
L15520:     call "DESCRIBE" (#14, project$(c%), projectdescr$, 0%, u3%)
            gosub pricing_stuff
            return


        pricing_stuff
*        Calculates dependant variables (for line C%) and formats
*        the fields related to pricing of the line item.
            pricestk(c%)     = round(price(c%) / conv(c%)           , 4)
            tempext          = round(price(c%) * ship(c%) / conv(c%), 2)
            linediscamt(c%)  = round(tempext * linediscpct(c%) * .01, 2)
            linediscamt(c%)  = -linediscamt(c%)
            lineext(c%)      = round(tempext + linediscamt(c%)      , 2)
            call "CONVERT" (price(c%)      , 4.4, price$      )
            call "CONVERT" (linediscpct(c%), 2.2, linediscpct$)
            call "CONVERT" (lineext(c%)    , 2.2, lineext$    )
            call "CONVERT" (linediscamt(c%), 2.2, linediscamt$)
            call "CONVERT" (pricestk(c%)   , 4.4, pricestk$   )
            return


        clear_line   /* Clear variables for line C%                    */
            part$(c%), descr$(c%), item$(c%), cat$(c%), order$, openqty$,~
            ship$, stkuom$(c%), priceuom$(c%), taxable$(c%),             ~
            nonstockmsg$(c%), catdescr$, stkuomdescr$, priceuomdescr$,   ~
            pricestk$, errormsg$, lot$(), sales$(c%),                    ~
            salesdescr$, discs$(c%), discsdescr$, project$(c%),          ~
            projectdescr$, conv$, price$, lsts$(c%), linediscpct$,       ~
            linediscamt$, lineext$, soseq$(c%), soseqmsg$, lotmsg$,      ~
            lotqty$, cus_part$(c%), cus_part_descr$(c%) = " "

            order(c%), openqty(c%), ship(c%), conv(c%), price(c%),       ~
            pricestk(c%), linediscpct(c%), linediscamt(c%), lineext(c%), ~
            soorder(c%), soopen(c%), qtyschld(c%), qtyshipped(c%),       ~
            qtypreinv(c%), bolqty(c%) = 0
            for l% = 1% to 30
                lots$  (c%,l%) = " "
                lotqtys(c%,l%) = 0
            next l%
            sn_lot1was$(c%) = all(hex(00))
            textidl$   (c%) = all(hex(ff))
            init (" ")  mfg$()  :  mat redim  mfg$(1%)1%
            total_e_lines% = total_e_lines% - e_lines%(c%)
            e_lines%(c%) = 0%
            return


        customer_part  /* Get CMS Part from Customer Xref File */
            if pxref% + cxref% + c_cxref% < 3% then return
            cus_part$(c%) = part$(c%)
            call "PTXREFSB" (1%, "C", cuscode$, cus_part$(c%),           ~
                             str(cus_part_descr$(c%),,30%), part$(c%),   ~
                             " ", #4, ret%)
            if ret% = 1% then L16060
                errormsg$ = "No Cross Reference for the Customer's " &   ~
                            "Part."
                return
L16060:     keyhit% = 0%
            cus_part_type$(c%) = "C"
            return

        call_customer_credit
            call "ARQCUSCR" (cuscode$)
            return

        manufactures_part  /* Get CMS Part from Manufacture Xref File */
            if pxref% + mxref% < 2% then return
            savefield% = fieldnr%
            fieldnr% = 15%
            inpmessage$ = "Enter The Manufacture's Code For This Part " &~
                          "Number."
            errormsg$ = " "
            mat redim  mfg$(1)9
L16650:     gosub'104(fieldnr%,1%)
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then gosub restart_line
                if keyhit% <>  0% then L16650
            if mfg$(1%) = "?" then mfg$(1%) = " "
            mfgdescr$ = hex(06) & "Select Manufacturer's Code"
            readkey$ = "MFG CODES" & mfg$(1%)
            call "PLOWCODE" (#8, readkey$, mfgdescr$, 9%, .3, f1%(8%))
                if f1%(8%) = 1% then L16760
                   errormsg$ = "Invalid Manufacturer's Code"
                   goto L16650
L16760:     mfg$(1%) = str(readkey$,10%)
            cus_part$(c%) = part$(c%)
            call "PTXREFSB" (1%, "M", mfg$(1%), cus_part$(c%),           ~
                             str(cus_part_descr$(c%),,30%), part$(c%),   ~
                             " ", #4, ret%)
            if ret% = 1% then L16850
                if part$(c%) = " " then                                  ~
                   errormsg$ = "Part Number CANNOT Be Blank"  else       ~
                   errormsg$ = "No Cross Ref Part For This Mfg Part"
L16850:     fieldnr% = savefield%
            init (" ")  mfg$()
            mat redim mfg$(1%)1%
            cus_part_type$(c%) = "M"
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * 1- Sum up Invoice.                                        *~
            * 2- Get any change data required, manage payment schedule. *~
            * 3- Get final save / delete approval.                      *~
            *************************************************************

        datasave

            pm_flags$ = str(pm_on$) & str(pm_so$) & pm_inv$

            call "ARIEXASB" (            /* All 63 Arguments . . . .   */~
                 currency$,              /* Currency code& description */~
                 cuscode$,               /* Customer Code              */~
                 invdate$,               /* Invoice Date               */~
                 invtype$,               /* Type of Invoice            */~
                 statutory$,             /* Statutory currency code    */~
                 store$,                 /* Store Code                 */~
                 pm_flags$,              /* PM Surcharge Flags         */~
                 so$,                    /* SO Number                  */~
                                                                         ~
                 convunt,                /* Currency conversion fields */~
                 maxlines%,              /* Total number of lines      */~
                                                                         ~
                            #2,          /* SYSFILE2                   */~
                            #4,          /* HNYMASTR                   */~
                            #13,         /* HNYQUAN                    */~
                            ret%)        /* Lines Added                */

            if ret% = 0% then L19200
               total_e_lines% = 0%
               if maxlines% = 0% then L19200
                  for l% = 1% to maxlines%
                      if e_lines%(l%) <= 0% then L19192
                      total_e_lines% = total_e_lines% + e_lines%(l%)
L19192:           next l%
            goto line_summary

L19200
*        FIRST add up the Invoice
            grossinv, taxamt, taxable, netinv = 0
            lines%, deles% = 0%
            inpmessage$, delemsg$ = " "

            if maxlines% = 0% then L19500
                for c% = 1% to maxlines%
                     if lsts$(c%) <> "D"  then lines% = lines% + 1%      ~
                                          else deles% = deles% + 1%
                     if lsts$(c%) = "D" then L19330
                          grossinv = grossinv + lineext(c%)
                          if taxable$(c%) = "Y" then                     ~
                                        taxable = taxable + lineext(c%)
L19330:         next c%
            if lines%  = 0% then delemsg$ = "Invoice will be Deleted"
            invdiscamt = round(grossinv * invdiscpct * .01, 2)
            invdiscamt = -invdiscamt
            taxabledsc = round(taxable  * invdiscpct * .01, 2)
            taxable    = taxable - taxabledsc
            taxamt     = round(taxable  * taxpct     * .01, 2)
            netinv     = grossinv + invdiscamt + frtamt + taxamt

*        Next see if we need to manage the Payment Schedule
            if terms$ <> "DATED" then L19500
                temp = 0
                for i% = 1% to 30%
                     temp = temp + termsdue(i%)
                next i%
                if temp <> netinv then gosub manage_payment_schedule

L19500
*        NOW, Display screen in input mode IF this is an old invoice.
            if posthny$   = " " then posthny$   = "Y"
            if hnymsg$   <> " " then posthny$   = "N"
            if summary% = 1% then L19700
                fieldnr% = 1%
                if invonfile% = 0% then L19570
L19540:         gosub'106(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then       L19540
L19570:         gosub'156(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L19540

L19700:     lastfieldnr% = 0%
            summary% = 1%
L19710:     gosub'106(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       line_summary
                  if keyhit%  =  9 then       edithdr1
                  if keyhit%  = 10 then gosub manage_payment_schedule
                  if keyhit%  = 16 then       save_data
                  if keyhit% <>  0 then       L19710
L19780:     fieldnr% = cursor%(1) - 5%
                if fieldnr% < 1% or fieldnr%  > 2% then L19700
                if hnymsg$ <> " " and fieldnr% = 2% then L19700
                if fieldnr%  = lastfieldnr% then L19700
L19810:     gosub'106(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L19810
            gosub'156(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L19810
                          lastfieldnr% = fieldnr%
                          goto L19780


        manage_payment_schedule
            str(i$(1),47,16) = "PAYMENT SCHEDULE"
            str(i$(2),41, 4) = " "
            call "ARIDATED" (str(i$(1),2), str(i$(2),2), netinv,         ~
                             invdate$, termsdue(), termsdiscs(),         ~
                             termsdisc$(), termsnet$(), u3%)
            if u3% = 16% then return
            return clear all
                if u3% = 1% then startover2
                if u3% = 2% then line_summary else edithdr1


        REM *************************************************************~
            *            D E F A U L T S   F O R   P A G E   1          *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for Screen 1 of Input.                      *~
            *************************************************************

        deffn'051
                  on fieldnr% gosub      ,         /* Customer Code    */~
                                         ,         /* Invoice Number   */~
                                         ,         /* SO - BOL         */~
                                         ,         /* Settlement #     */~
                                         ,         /* Ship-to          */~
                                         ,         /* Sold-to          */~
                                         ,         /* Invoice Date     */~
                                         ,         /* PO Number        */~
                                    L20440          /* Store Code       */~
                                                   /* Expiration Date  */
                     return

*        Customer Code                         CUSCODE$
*          RETURN

*        Invoice Number                        INVNR$
*          RETURN

*        Sales Order - BOL                     SO$ - BOL$
*          RETURN

*        Settlement Number                     SETLMNT$
*          RETURN

*        Ship-to                               SHIPTO$
*          RETURN

*        Sold-to                               SOLDTO$
*          RETURN

*        Invoice (Document) Date               INVDATE$
            invdate$ = date$
*          RETURN

*        Customer PO Number                    PO$
*          RETURN

L20440
*        Store Number                          STORE$
            if store$ = " " then store$ = dfltstr$
            return

*        Expiration Date                       EXPDATE$
            return

        REM *************************************************************~
            *            D E F A U L T S   F O R   P A G E   3          *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for third header screen.                    *~
            *************************************************************

            deffn'053(fieldnr%)
                  on fieldnr% gosub L22105,         /* Region           */~
                                    L22130,         /* Salesmen / Split */~
                                    L22482,         /* PM INV Flag      */~
                                    L22190,         /* Price Code Deflt */~
                                    L22215,         /* Invoice Disc %   */~
                                    L22250,         /* Sales Tax %/Code */~
                                    L22345,         /* A/R Type         */~
                                    L22365,         /* Payment Terms    */~
                                    L22405,         /* Net Invoice Acct */~
                                    L22425,         /* Sales Tax Acct   */~
                                    L22440,         /* Freight Account  */~
                                    L22455,         /* Sales Acct Deflt */~
                                    L22470          /* Sales Discs Acct */
                     return

L22105
*        Sales Region Code                     REGION$
            get #01 using L22115, region$
L22115:         FMT XX(728), CH(4)
            return

L22130
*        Salesman Code / Split %               SALESMAN$(3)
            get #01 using L22140, salesman$(), comm%()
L22140:         FMT XX(713), 3*CH(4), 3*BI(1)
            for i% = 1% to 3%
                if salesman$(i%) <> " " then L22170
                     comm%(i%) = 0%
                     comm$(i%) = " "
                     goto L22175
L22170:         convert comm%(i%) to comm$(i%), pic(##0)
L22175:     next i%
            return

L22190
*        Price Code Default                    PC$
            get #01 using L22200, pc$
L22200:         FMT XX(524), CH(1)
            return

L22215
*        Invoice Discount Percent              INVDISCPCT$
            get #01 using L22225, invdiscpct
L22225:         FMT XX(516), PD(14,4)
            call "CONVERT" (invdiscpct, 2.2, invdiscpct$)
            return

L22250
*        Sales Tax Code and Sales Tax %        TAXCODE$ & TAXPCT$
        get_tax_defaults
            get #01 using L22275, taxcode$
L22275:         FMT POS(1025), CH(10)
            if taxcode$ <> " " then L22305
                readkey$ =  "DEFAULTS.STORE." & str(store$)
                call "READ100" (#02, readkey$, f1%(2))
                if f1%(2) = 1% then get #02 using L22300, taxcode$
L22300:              FMT POS(252), CH(10)
L22305:     call "DESCRIBE" (#20, taxcode$, taxcodedescr$, 0%, f1%(20))
            taxpct = 0  :  if f1%(20) = 0% then L22325
                get #20 using L22320, taxpct
L22320:              FMT XX(40), PD(14,4)
L22325:     call "CONVERT" (taxpct, 2.4, taxpct$)
            if taxpct = 0 then taxpct$ = " "
            return

L22345
*        A/R Type                              ARTYPE$
            artype$ = "A"  :  artypedescr$ = "Accounts Receivable"
            return

L22365
*        Payment Terms (Code)                  TERMS$
            get #01 using L22375, terms$
L22375:         FMT XX(542), CH(20)
            if artype$ <> "A" and  terms$ = "DATED" then terms$ = " "
            if artype$  = "C" then terms$ = "CASH"
            if artype$  = "E" then terms$ = "EXPENSE"
            if stlmnt$ <> " " then terms$ = " "
            return

L22405
*        Net Invoice Distribution Account      ARACCT$
            gosub get_account_defaults   /* Gets ALL Header Accounts  */
            return

L22425
*        Sales Tax Account                     TAXACCT$
            return

L22440
*        Freight Account                       FRTACCT$
            return

L22455
*        Sales Account Default                 SALESACCT$
            return

L22470
*        Sales Discounts Account               DISCACCT$
            return

L22482: /* PM INV Flag */

            return

        get_account_defaults  /* Load up all header accounts  */
*        * * * WARNING, DUPLICATE IN ARXINPTS * * * *
            acct% = 4%  :  if artype$ = "C" then acct% = 5%
            gosub get_account_default
            aracct$ = acct$  :  aracctdescr$ = acctdescr$

            acct% = 6%  :  gosub get_account_default
            taxacct$ = acct$  :  taxacctdescr$ = acctdescr$

            acct% = 7%  :  gosub get_account_default
            frtacct$ = acct$  :  frtacctdescr$ = acctdescr$

            return

        get_account_default
            call "ARMGLGET" (acct%, cuscode$, " ", " ", taxcode$,        ~
                             store$, " ", #02, #01, #04, #11, #20, acct$)
            call "GLUNFMT" (acct$)
            call "DESCRIBE" (#03, acct$, acctdescr$, 0%, f1%(3))
            call "GLFMT" (acct$)
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for fields on First Line Item Screen.       *~
            * This code section is not executed for SO-BOL lines.       *~
            *************************************************************

        deffn'054(fieldnr%)
            on fieldnr%       gosub L23120,         /* Part Code        */~
                                    L23135,         /* Part Description */~
                                    L23165,         /* Part Category    */~
                                    L23195,         /* Stocking UOM     */~
                                    L23230,         /* Qty Shipped/Lot  */~
                                    L23250,         /* Order Qty        */~
                                    L23270,         /* Open Qty         */~
                                    L23285,         /* Pricing UOM      */~
                                    L23320,         /* Conversion       */~
                                    L23400,         /* Currency code    */~
                                    L23425,         /* Unit Price       */~
                                    L23475,         /* Line Item Disc   */~
                                    L23490,         /* Extension        */~
                                    L23505          /* Line Taxable?    */
                     return

L23120
*        Part Code                             PART$()
            return

L23135
*        Part Description                      DESCR$()
            if nonstockmsg$(c%) <> " " then return
                get #04 using L23150, descr$(c%)
L23150:              FMT XX(25), CH(32)
                return

L23165
*        Part Category                         CAT$()
            if nonstockmsg$(c%) <> " " then return
                get #04 using L23180, cat$(c%)
L23180:              FMT XX(89), CH(4)
                return

L23195
*        Stocking Unit of Measure                        STKUOM$()
            stkuom$(c%) = "EACH"
            if nonstockmsg$(c%) <> " " then return
                get #04 using L23215, stkuom$(c%)
L23215:              FMT XX(73), CH(4)
                return

L23230
*        Qty Shipped / Lot & Lot Qty           SHIP$ / LOT$()-LOTQTY$()
            minsoqty, minsoinc, def_percent(c%), def_unit(c%) = 0
            if nonstockmsg$(c%) <> " " then L23238
            get #04 using L23235, minsoqty, minsoinc, def_percent(c%),    ~
                                 def_unit(c%)
L23235:         FMT POS(706), 4*PD(14,4)
L23238:     call "LOTENABL" (part$(c%), lot_enable%, lot%, #02, #04)
            return

L23250
*        Order Qty                             ORDER$
            if bookinv$ = "Y" then order$ = ship$
            return

L23270
*        Open Order Qty                        OPENQTY$
            return

L23285
*        Pricing Unit of Measure               PRICEUOM$()
            priceuom$(c%) = stkuom$(c%)
            if nonstockmsg$(c%) <> " " then return
                get #04 using L23305, priceuom$(c%)
L23305:              FMT XX(77), CH(4)
                return

L23320
*        Conversion Pricing to Stocking        CONV$
            if nonstockmsg$(c%) <> " " then L23350
                get #04 using L23335, conv(c%)
L23335:              FMT POS(82), PD(14,7)
                call "CONVERT" (conv(c%), 7.7, conv$)
                return
L23350:     if stkuom$(c%) = priceuom$(c%) then conv$ = "1"
            if stkuom$(c%) = priceuom$(c%) then return
                readkey$ = "UOMCONV  " & str(priceuom$(c%)) & "-" &      ~
                                         str(stkuom$  (c%))
                call "READ100" (#08, readkey$, f1%(8))
                if f1%(8) = 0% then return
                     get #08 using L23385, conv$
L23385:                   FMT XX(24), CH(10)
                     return

L23400
*        Currency code                         CURRENCY$
            if curr$ <> "Y" then enabled% = 0%
            if frtamt <> 0  then enabled% = 0%
            if maxlines% > 1% then enabled% = 0%
            if c% <> 1% then enabled% = 0%
            if invtype$ = "X" then enabled% = 0%
            return

L23425
*        Unit Price                            PRICE$
            get #01 using L23435, custype$
L23435:         FMT XX(1022), CH(2)
            call "CPRASSGN" (cuscode$, custype$, part$(c%), cat$(c%),    ~
                pc$, invdate$, currtype$, currency$, -1, ship(c%),       ~
                #02, #04, price(c%), linediscpct(c%), errormsg$)
            if errormsg$ <> " " then return
                gosub pricing_stuff
                return

L23475
*        Line Item Discount %                  LINEDISCPCT$
            return

L23490
*        Line Item Extension                   LINEEXT$
            return

L23505
*        Line Taxable? (Y/N)                   TAXABLE$()
            get #01 using L23515, custaxable$
L23515:         FMT XX(793), CH(1)
            parttaxable$ = " "
            if nonstockmsg$(c%) =" " then get #04 using L23530,parttaxable$
L23530:         FMT XX(126), CH(1)
            if parttaxable$ = "N" then taxable$(c%) = "N"
            if parttaxable$ = "Y" then taxable$(c%) = "Y"
            if parttaxable$ = " " then taxable$(c%) = custaxable$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   5     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for Second Line Item Scree.                 *~
            * This code section is not executed for SO-BOL lines.       *~
            *************************************************************

        deffn'055(fieldnr%)
            on fieldnr%    gosub         ,         /* PO Item Number   */~
                                         ,         /* Project Number   */~
                                    L24320,         /* Sales Distr. Acct*/~
                                    L24430          /* Sales Discs Acct */
            return

*        PO Item                               ITEM$()
            return

*        Project Number                        PROJECT$()
            return

L24320
*        Sales Distr. Account                  SALES$()
            call "HNYGLGET" (part$(c%), store$, lot$(c%), sales$(c%), 5%,~
                             #04, #13)
            if sales$(c%) <> " " then call "GLFMT" (sales$(c%))
            if sales$(c%) <> " " then return
                call "ARMGLGET" (1%, cuscode$, part$(c%), cat$(c%), " ", ~
                                 store$, " ", #02, #01, #04, #11, #03,   ~
                                 sales$(c%))
                if sales$(c%) = " " then sales$(c%) = salesacct$
                return

L24430
*        Sales Discounts Account               DISCS$()
            call "ARMGLGET" (2%, cuscode$, part$(c%), cat$(c%), " ",     ~
                             store$, " ", #02, #01, #04, #11, #03,       ~
                             discs$(c%))
            if discs$(c%) = " " then discs$(c%) = discacct$
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
            scr%(1, 1) =  1% : set%( 1) = 13%      /* Customer Code    */
            scr%(1, 2) =  2% : set%( 2) = 13%      /* Invoice Number   */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* SO-BOL           */
            scr%(1, 4) =  4% : set%( 4) = 99%      /* Settlement #     */
            scr%(1, 5) =  5% : set%( 5) =  2%      /* Ship-to          */
            scr%(1, 6) =  6% : set%( 6) =  2%      /* Sold-to          */
            scr%(1, 7) =  7% : set%( 7) =  2%      /* Invoice Date     */
            scr%(1, 8) =  8% : set%( 8) =  2%      /* PO #             */
            scr%(1, 9) =  9% : set%( 9) = 99%      /* Store Number     */
            scr%(1,10) = 10% : set%(10) = 99%      /* Expiration Date  */

            scr%(2, 1) = 11% : set%(11) =  2%      /* Date Shipped     */
            scr%(2, 2) = 12% : set%(12) =  2%      /* How Shipped      */
            scr%(2, 3) = 13% : set%(13) =  2%      /* FOB              */
            scr%(2, 4) = 14% : set%(14) =  2%      /* Carrier          */
            scr%(2, 5) = 15% : set%(15) =  2%      /* Frt Bill         */
            scr%(2, 6) = 16% : set%(16) =  2%      /* Cartons          */
            scr%(2, 7) = 17% : set%(17) =  2%      /* Weight           */
            scr%(2, 8) = 49% : set%(49) =  2%      /* Currency         */
            scr%(2, 9) = 18% : set%(18) =  2%      /* Freight Charges  */

            scr%(3, 1) = 19% : set%(19) =  2%      /* Sales Region     */
            scr%(3, 2) = 20% : set%(20) =  2%      /* Salesmen/Split   */
            scr%(3, 3) = 50% : set%(50) =  2%      /* PM SO Flag       */
            scr%(3, 4) = 21% : set%(21) =  2%      /* Price Code       */
            scr%(3, 5) = 22% : set%(22) =  2%      /* Inv Disc %       */
            scr%(3, 6) = 23% : set%(23) =  2%      /* Sales Tax %/Code */
            scr%(3, 7) = 24% : set%(24) =  2%      /* A/R Type         */
            scr%(3, 8) = 25% : set%(25) =  2%      /* Payment Terms    */
            scr%(3, 9) = 26% : set%(26) =  2%      /* A/R Acct         */
            scr%(3,10) = 27% : set%(27) =  2%      /* Sales Tax Acct   */
            scr%(3,11) = 28% : set%(28) =  2%      /* Frt Acct         */
            scr%(3,12) = 29% : set%(29) =  2%      /* Sales Distr Acct */
            scr%(3,13) = 30% : set%(30) =  2%      /* Sales Discs Acct */

            scr%(4, 1) = 31% : set%(31) =  2%      /* Part Code        */
            scr%(4, 2) = 32% : set%(32) =  2%      /* Part Descr       */
            scr%(4, 3) = 33% : set%(33) =  2%      /* Part Category    */
            scr%(4, 4) = 34% : set%(34) =  2%      /* Stocking UOM     */
            scr%(4, 5) = 35% : set%(35) =  2%      /* Qty Shipped/Lot  */
            scr%(4, 6) = 36% : set%(36) =  2%      /* Order Qty        */
            scr%(4, 7) = 37% : set%(37) = 99%      /* Open Order Qty   */
            scr%(4, 8) = 38% : set%(38) =  2%      /* Pricing UOM      */
            scr%(4, 9) = 39% : set%(39) =  0%      /* Conversion Fctr  */
            scr%(4,10) = 48% : set%(48) =  2%      /* Currency code    */
            scr%(4,11) = 40% : set%(40) =  2%      /* Unit Price       */
            scr%(4,12) = 41% : set%(41) =  2%      /* Line Item Disc%  */
            scr%(4,13) = 42% : set%(42) = 99%      /* Line Ext         */
            scr%(4,14) = 43% : set%(43) =  2%      /* Line Taxable?    */

            scr%(5, 1) = 44% : set%(44) =  2%      /* PO Item Number   */
            scr%(5, 2) = 45% : set%(45) =  2%      /* Project Number   */
            scr%(5, 3) = 46% : set%(46) =  2%      /* Sales Account    */
            scr%(5, 4) = 47% : set%(47) =  2%      /* Sales Disc Acct  */
        REM Next available number is '51'.

            if pm_on$ =  "Y" then L27640
                                    set%(50) = 99% /* PM SO Flag       */

L27640:     call "ENABLSUB" ("INIT", pgmname$, scr%(), set%(),           ~
                                                         0%, 0%, 0%, 0%)

            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************
        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", pgmname$, scr%(), set%(),         ~
                              s%, f%, 0%, 0%)
            return

        REM *************************************************************~
            *        I N P U T   M E S S A G E S  &  E N A B L E S      *~
            * --------------------------------------------------------- *~
            * Sets Enable Flag, Input Message, and Standard PF Keys.    *~
            *************************************************************
        deffn'050(s%, f%, edit%)  /* EDIT%: 1=Input Mode; 2=Edit Mode */
*        Required: S%, F%, EDIT%
            if f% <> 0% then L28080
                inpmessage$ = edtmessage$
                goto L28180

L28080
*        First Define the Input Message
            r% = scr%(s%, f%)  /* Get sequential field number          */
            restore line = L28235, r%     /* Position for Read          */
            read inpmessage$             /* Read Input Message         */

*        Now set the Field Enable Flag (and over-ride if so required)
            call "ENABLSUB" ("SET", pgmname$, scr%(), set%(), s%, f%,    ~
                             edit%, enabled%)

            if s% = 2% and f% = 8% then enabled% = 0%

            if s% <> 4% then L28185
                if f% = 10% then enabled% = 0%
            if edit% <> 1% then L28160              /* Lines#1 in input */
                if nonstockmsg$(c%) = " " and (f% = 4% or f% = 8% or     ~
                                              f% = 9%) then enabled% = 0%
L28160:         if nonstockmsg$(c%) <> " " and f% = 6% then enabled% = 0%
                if soseq$(c%)       <> " " and f% = 6% then enabled% = 0%
                if soseq$(c%)        = " " and f% = 7% then enabled% = 0%

L28180
*        And, finally, set up LINE2$ for the display.
L28185:     line2$ = "Customer: " & cuscode$
            str(line2$,22) = "Invoice: " & invnr$
            if invnr$ = " " and (s% > 1% or f% > 2% or edit% = 2%) then  ~
                       str(line2$,22) = "Invoice: -NEW-"
            if s% = 4% or s% = 5% then str(line2$,41) = "/###"
            if s% = 4% or s% = 5% then                                   ~
                       convert c% to str(line2$,42,3), pic (000)
            str(line2$,62) = "ARIXNPUT: " & str(cms2v$,,8)
            return

L28235: data                                                             ~
        /* Screen 1                                                    */~
         "Enter Ship-to Customer Code.                                 ",~
         "Enter Invoice Number.                                        ",~
         "Enter Sales Order and Bill of Lading Numbers                 ",~
         "Enter Settlement Number.                                     ",~
         "Enter Ship-to Name and Address.                              ",~
         "Enter Sold-to, 'BILL-TO', or leave blank if same as ship-to. ",~
         "Enter Invoice Date.                                          ",~
         "Enter Customer's Purchase Order Number.                      ",~
         "Enter Store from which merchandise was shipped.              ",~
         "Enter Expiration Date for Recurring Invoice.                 ",~
                                                                         ~
        /* Screen 2                                                    */~
         "Enter Date of Shipment.                                      ",~
         "Enter How Merchandise was Shipped.                           ",~
         "Enter FOB Description.                                       ",~
         "Enter Carrier Code.                                          ",~
         "Enter Air/Freight Bill Number.                               ",~
         "Enter Number of Cartons in this shipment.                    ",~
         "Enter Weight of Merchandise Shipped.                         ",~
         "Enter Freight Charges                                        ",~
                                                                         ~
        /* Screen 3                                                    */~
         "Enter Sales Region Code.                                     ",~
         "Enter Salesmen Codes and Commission Splits.                  ",~
         "Enter Default Pricing Code.                                  ",~
         "Enter Invoice Level Discount Percent.                        ",~
         "Enter Sales Tax Code and % (leave % blank to use % on file). ",~
         "Enter A/R Type ('A'=A/R, 'C'=Cash, 'E'=Expense).             ",~
         "Enter Payment Terms (Description or Code) or 'DATED'.        ",~
         "Enter G/L Account to Distribute Net Invoice Amount to. (A/R) ",~
         "Enter G/L Account Code for Sales Tax.                        ",~
         "Enter G/L Account Code for Freight Charges.                  ",~
         "Enter G/L Account Code for Sales Distribution.               ",~
         "Enter G/L Account Code for Sales Discounts.                  ",~
                                                                         ~
        /* Screen 4                                                    */~
         "Enter the Part Code for this Line Item.                      ",~
         "Enter the Part Description for this Line Item.               ",~
         "Enter the Part Category Code.                                ",~
         "Enter the Stocking Unit of Measure (used for quantities).    ",~
         "Enter the Quantity Shipped, Lot Number and Qty from that Lot.",~
         "Enter the Original Order Quantity.                           ",~
         "Enter the Open Order Quantity remaining.                     ",~
         "Enter the Pricing Unit of Measure.                           ",~
         "Enter the Pricing UOM to Stocking UOM Conversion Factor.     ",~
         "Enter the Unit Price.                                        ",~
         "Enter the Discount Percent for this Line Item.               ",~
         "Enter the Extension for this line item.                      ",~
         "Is this Line Item Taxable? (Y/N)                             ",~
                                                                         ~
        /* Screen 5                                                    */~
         "Enter the P.O. Line Number Reference.                        ",~
         "Enter the Project Number.                                    ",~
         "Enter the Sales Distribution Account Number.                 ",~
         "Enter the Sales Discount Account Number.                     ",~
                                                                         ~
        /* Added to screen 4                                           */~
         "Enter Transaction Currency Code for this document. Blank = stat~
        ~utory.",                                                         ~
                                                                         ~
        /* Added to screen 2                                           */~
         "Enter Transaction Currency Code for this document. Blank = stat~
        ~utory."                                                         ,~
                                                                         ~
        /* Added to screen 3                                           */~
         "Are Precious Metal Surcharges to Adjusted at Invoicing(Y/N)?"

        REM *************************************************************~
            *        I N I T I A L I Z E   F O R   I N P U T            *~
            * --------------------------------------------------------- *~
            * Clear variables prior to input mode.                      *~
            *************************************************************
        init_for_input
            init(" ") acct$, acctdescr$, aracct$, aracctdescr$, artype$, ~
                      artypedescr$, billxref$,                           ~
                      bol$, carrier$, carrierdescr$, cartons$, cat$(),   ~
                      catdescr$, comm$(), conv$, crhold$, cuscode$,      ~
                      cusdescr$, custaxable$, custype$, delemsg$,        ~
                      descr$(), discacct$, discacctdescr$, discs$(),     ~
                      discsdescr$, errormsg$, expdate$, fcamt$, fob$,    ~
                      frtacct$, frtacctdescr$, frtamt$, frtbill$,        ~
                      howship$, inpmessage$, invdate$, invdiscamt$,      ~
                      invdiscpct$, invnr$, invrsn$, invrsndescr$,        ~
                      item$(), linediscamt$, linediscpct$, lineext$,     ~
                      lotmsg$, lot$(), lots$(), lotqty$, lsts$(),        ~
                      msg$, nonstockmsg$(), openqty$, order$,            ~
                      part$(), parttaxable$, pc$, pcdescr$,              ~
                      po$, poreqd$, posthny$, price$, pricestk$,         ~
                      priceuom$(), priceuomdescr$, project$(),           ~
                      projectdescr$, readkey$, readkey1$, region$,       ~
                      regiondescr$, sales$(), salesdescr$, salesacct$,   ~
                      salesacctdescr$, salesman$(), salesmandescr$(),    ~
                      seq$(), sext$(), ship$, shipdate$, shipto$(),      ~
                      so$, soldto$(), sship$(), soseq$(), stkuom$(),     ~
                      stkuomdescr$, stlmnt$, taxable$(), taxacct$,       ~
                      taxacctdescr$, taxcode$, taxcodedescr$, taxpct$,   ~
                      temp$, terms$, termsdescr$, termsdisc$(),          ~
                      termsnet$(), vf$, weight$, buflot$(), postlot$(),  ~
                      bufmark$(), mark$(), postmark$(), sn_loc$,         ~
                      sn_lineid$(), sn_last_lineid$, sn_trankey$,        ~
                      soinbuffr$, currency$, currdesc$(), convdate$,     ~
                      currdflt$, cus_part$(), cus_part_descr$(), mfg$(), ~
                      pm_so$, pm_inv$, cus_part_type$()
            init (hex(00)) sn_lot1was$()

            cartons, fcamt, frtamt, grossinv, invdiscamt, invdiscpct,    ~
            netinv, taxamt,  taxpct, weight = 0

            conveqv, convunt = 1

            maxlines%, c%, summary%, invonfile%, soonfile%, bolonfile%,  ~
            total_e_lines%, toggle%, t%  = 0%

            mat comm%       = zer
            mat conv        = zer
            mat e_lines%    = zer
            mat linediscamt = zer
            mat linediscpct = zer
            mat lineext     = zer
            mat lotqty      = zer
            mat lotqtys     = zer
            mat order       = zer
            mat openqty     = zer
            mat price       = zer
            mat pricestk    = zer
            mat qtyschld    = zer
            mat qtyshipped  = zer
            mat qtypreinv   = zer
            mat bolqty      = zer
            mat ship        = zer
            mat soopen      = zer
            mat soorder     = zer
            mat termsdue    = zer
            mat termsdiscs  = zer
            mat bufqty      = zer
            mat postqty     = zer
            mat sn_index1%  = zer
            mat sn_index2%  = zer
            mat redim  mfg$(1%)1%

            init (hex(ff)) textid$, textidl$()
            call "TXTFUTIL" (#22, f2%(22), "INTL", textid$)
            call "ALLFREE"
            return


        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants.   *~
            *************************************************************

        startover
L29365:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                if u3% <> 0% then L29365

*        Wants to Start Over
            return clear all
         startover2   /* (Entry point for Start Over in VF routine.) */
            call "SERSTOVR" (0%, "4", "2", #32, #33)
            if invonfile% = 1% or bol$ = " " then L29430
                readkey$ = "RT" & str(cuscode$) & str(invnr$)
                if invnr$ = " " then str(readkey$,12) = userid$ & hex(06)
                call "DELETE" (#34, readkey$, 19%)
L29430:     if invonfile% = 1% or invnr$ = " " then L29445
                readkey$ = str(cuscode$) & invnr$
                call "DELETE" (#23, readkey$, 17%) /* ARINUMBR */
L29445:     from_startover% = 1%
            goto clear_inprocess          /* Clear In-process Flag */

        restart_line
L29460:     u3% = 2%
            call "ASKUSER" (u3%, "RESTART LINE",                         ~
                            "Press (ENTER) to RESTART Line Item",        ~
                            "- OR -", "Press PF-1 to EXIT Restart.")
            if u3% = 1% then return
                if u3% <> 0% then L29460
                     return clear
                     for i% = 1% to 30%
                          if sn_index2%(c%,i%) <> 0% then                ~
                               call "SERSTOVR" (sn_index2%(c%,i%),       ~
                                                      "4", "2", #32, #33)
                     next i%
                     gosub clear_line
                     goto inputline

        REM *************************************************************~
            *              L O A D   I N V O I C E                      *~
            *-----------------------------------------------------------*~
            * Load the Invoice if its already in the Buffer file. If it *~
            * is not, then check that the invoice number has not        *~
            * already been used.                                        *~
            *************************************************************
        load_invoice
            invonfile%, soonfile%, bolonfile%, maxlines% = 0%
            sn_last_lineid$ = "0000"

*        First let's go for a direct hit in the buffer file
            readkey$ = str(cuscode$) & invnr$
            call "READ100" (#09, readkey$, invonfile%)
            if invonfile% = 0% then L30105
                get #09 using L30080, readkey$  /* Session Number */
L30080:              FMT POS(2001), CH(6)
                if readkey$ = session$ then L30220
                     errormsg$ = "This is not an EXPORT Invoice"
                     return

L30105
*        Invoice Does NOT exist - check it out for dups
            if invnr$ = " " then return
            call "REDALT0" (#23, invnr$, 1%, f1%(23))
            if f1%(23) = 1% then L30140
                write #23 using L30130, cuscode$, invnr$, eod goto L30140
L30130:              FMT CH(9), CH(8)
                return
L30140:     get #23, str(readkey$,,9)
            if str(readkey$,,9) <> cuscode$ then L30160
                errormsg$ = "Number already used for this Customer"
            return
L30160:     if dup_inv$ = "N" then L30205
            u3% = 2%
            call "ASKUSER" (u3%, "DUPLICATE NUMBER",                     ~
                            "This Invoice Number is already used.",      ~
                            "Enter PF-16 to reuse Invoice Number,",      ~
                            "Any other PF Key to re-enter Number.")
            if u3% = 16% then return
                  errormsg$ = hex(00) : return

L30205:           errormsg$ = "This Invoice Number already used for "  & ~
                              str(readkey$,,9) : return

L30220
*        Invoice DOES EXIST.  Load it up.

            prtmsg$ = hex(94) & "Loading Invoice..." & hex(8c)
            print at(04,02), str(prtmsg$)

            get #09  using L35370, cuscode$, invnr$, po$, so$, bol$,      ~
                     shipto$(), soldto$(), shipdate$, howship$, fob$,    ~
                     carrier$, cartons, weight, frtbill$, salesman$(),   ~
                     comm%(), region$, pc$, invdate$, expdate$,          ~
                     temp$, temp$, temp$, vf$, aracct$, frtacct$,        ~
                     taxacct$, salesacct$, discacct$, temp, invdiscpct,  ~
                     temp      , frtamt, temp  , temp1, temp, temp$,     ~
                     stlmnt$, store$, taxcode$, taxpct, temp$, invrsn$,  ~
                     textid$, posthny$, temp$, artype$, terms$,          ~
                     termsdue(), termsdiscs(), termsdisc$(), termsnet$(),~
                     temp$, temp$, currency$, temp$, temp$, temp$,       ~
                     pm_so$, pm_inv$

            for u3% = 1% to 30%
                if termsdisc$(u3%) = blankdate$ then termsdisc$(u3%) = " "
                if termsnet$(u3%)  = blankdate$ then termsnet$(u3%)  = " "
            next u3%

            if currency$ = " " then currency$ = statutory$
            call "READ100" (#42, key(#9), f1%(42))
                  if f1%(42) = 0% then L30345
            get #42 using L30330, currency$, frtamt, temp1,               ~
                           convdate$, conveqv, convunt,                  ~
                           termsdue()

L30330:          FMT CH(4), POS(38), PD(14,4), XX(8), PD(14,4), XX(8),   ~
                     CH(6), 2*PD(14,7), 30*PD(14,4)

L30345:     call "TXTFUTIL" (#22, f2%(22), "LOAD", textid$)
            fcamt = 0
            gosub format_header_data

*        Now load up line items
            maxlines%, c% = 0%
            readkey$ = str(cuscode$) & str(invnr$) & hex(00)
L30380:     call "PLOWNEXT" (#10, readkey$, 17%, f1%(10))
               if f1%(10%) <> 0% then L30386
L30382:           if maxlines% + total_e_lines% <= 100% then return
                  gosub extra_load_conflict
                  return

L30386:         if maxlines% < 100% then L30394
                   gosub severe_load_error
                   goto L30382

L30394:         maxlines%, c% = maxlines% + 1%
                get #10  using L35940, temp$, temp$, seq$(c%), item$(c%), ~
                     part$(c%), descr$(c%), cat$(c%), order(c%),         ~
                     ship(c%), openqty(c%), pricestk(c%), stkuom$(c%),   ~
                     priceuom$(c%), conv(c%), price(c%), linediscpct(c%),~
                     linediscamt(c%), lineext(c%), taxable$(c%),         ~
                     sales$(c%), discs$(c%), temp$, textidl$(c%),        ~
                     soseq$(c%), lot$(), lotqty(), nonstockmsg$(c%),     ~
                     project$(c%), sn_lineid$(c%), temp$, e_lines%(c%),  ~
                     temp$

        REM Get transaction amounts from the ARI 'shadow' file (ARICURCY)
            if currency$ = statutory$ then goto L30495 /* Not Stat, tho */
            if curr$ <> "Y" then goto L30495 /* Nor if no multi-currency*/
            call "READ100" (#43, readkey$, f1%(43))
            if f1%(43) = 0% then goto L30495
                get #43 using L30470, pricestk(c%), price(c%),            ~
                                     linediscamt(c%), lineext(c%)
L30470:         FMT POS(25), 4*PD(14,4)
                    if currency$ = " " then                              ~
                                    get #43 using L30490, currency$,      ~
                                            convdate$, conveqv, convunt
L30490:                 FMT CH(4), POS(57), CH(6), 2*PD(14,7)
L30495:         if sn_lineid$(c%) > sn_last_lineid$ then                 ~
                                         sn_last_lineid$ = sn_lineid$(c%)
                sn_lot1was$(c%) = lot$(1)
                if e_lines%(c%) < 0% then L30510
                   call "ARIEXTRA" (cuscode$, part$(c%), " ",            ~
                                    e_lines%(c%), #2)
                   total_e_lines% = total_e_lines% + e_lines%(c%)
L30510:         if soseq$(c%) <> " " then lsts$(c%) = "O"
                f1%(18) = 0%
                if so$ = " " or soseq$(c%) = " " then L30555
                     readkey1$ = str(so$) & soseq$(c%)
                     call "READ100" (#06, readkey1$, f1%(6))
                     if f1%(6) = 1% then get #6 using L30550, soorder(c%),~
                             qtyshipped(c%), soopen(c%), qtyschld(c%),   ~
                             qtypreinv(c%)
L30550:                   FMT XX(92), 4*PD(14,4), XX(8), PD(14,4)
L30555:         call "TXTFUTIL" (#22, f2%(22), "LOAD", textidl$(c%))
                for l% = 1% to 30%
                     lots$  (c%, l%) = lot$  (l%)
                     lotqtys(c%, l%) = lotqty(l%)
                     buflot$(c%, l%) = lot$  (l%)
                     bufqty (c%, l%) = lotqty(l%)
                     sn_index2%(c%,l%) = c% * 100% + l%
                     sn_trankey$ = str(cuscode$) & str(invnr$) &         ~
                                   str(sn_lineid$(c%)) & "##"
                     convert l% to str(sn_trankey$,22,2), pic(00)
                     call "SERLOAD" (sn_index2%(c%,l%), "RT",            ~
                                     sn_trankey$, 25%, " ", " ",         ~
                                     #02, #34, #32, #33, u3%)
                next l%
                if bol$ = " " then L30690
                   readkey1$ = str(so$) & str(bol$) & soseq$(c%)
                   call "READ100" (#18, readkey1$, f1%(18))
                      if f1%(18) = 0% then L30690
                   bolonfile% = 1%
                   get #18 using L30660, bolqty(c%), lot$(), lotqty(),    ~
                                        completemsg$(c%)
L30660:                FMT POS(32), PD(14,4), 30*CH(6), 30*PD(14,4),     ~
                           POS(476), CH(1)
                for l% = 1% to 30%
                    postlot$(c%, l%) = lot$  (l%)
                    postqty (c%, l%) = lotqty(l%)
                next l%
L30690:         if nonstockmsg$(c%) <> " " then                          ~
                                    nonstockmsg$(c%) = "(Non-Stock Part)"

*              IF PXREF% + CXREF% + C_CXREF% < 3% THEN 30720
                     call "PTUSEDSB" ("R","ARI ", readkey$, seq$(c%),    ~
                                      cus_part$(c%),                     ~
                                      str(cus_part_descr$(c%),,30%),     ~
                                      cus_part_type$(c%), ret%)
                     if ret% = 1% then L30720
                          cus_part$(c%) = "** No Cross Reference **"
                          cus_part_descr$(c%) = " "

L30720:         call "GLFMT"  (sales$(c%))
                call "GLFMT"  (discs$(c%))
                if completemsg$(c%) = "C" then                           ~
                   completemsg$(c%) = "Reported Complete in Shipping"    ~
                   else completemsg$(c%) = " "
                goto L30380

        format_header_data
            call "DATEFMT" (invdate$)
            call "DATEFMT" (shipdate$)
            call "DATEFMT" (expdate$)
            if artype$ = "A" then artypedescr$ = "Accounts Receivable"
            if artype$ = "C" then artypedescr$ = "Cash"
            if artype$ = "E" then artypedescr$ = "Expense"
            call "DESCRIBE" (#21, terms$  , termsdescr$  , 0%, f1%(21))
            if stlmnt$ <> " " then termsdescr$ = "Per Item Applied To"
            call "DESCRIBE" (#20, taxcode$, taxcodedescr$, 0%, f1%(20))
            call "DESCRIBE" (#03, aracct$ , aracctdescr$ , 0%, f1%(3))
            call "GLFMT" (aracct$   )
            call "DESCRIBE" (#03, taxacct$, taxacctdescr$, 0%, f1%(3))
            call "GLFMT" (taxacct$  )
            call "DESCRIBE" (#03, frtacct$, frtacctdescr$, 0%, f1%(3))
            call "GLFMT" (frtacct$  )
            call "DESCRIBE" (#03, salesacct$, salesacctdescr$, 0%, f1%(3))
            call "GLFMT" (salesacct$)
            call "DESCRIBE" (#03, discacct$, discacctdescr$, 0%, f1%(3))
            call "GLFMT" (discacct$)
            for i% = 1% to 3%
                if salesman$(i%) = " " then L30880
                     call "DESCRIBE" (#07, salesman$(i%),                ~
                                          salesmandescr$(i%), 0%, f1%(7))
                     convert comm%(i%) to comm$(i%), pic(##0)
L30880:     next i%
            readkey$ = "REGION   " & region$
            call "DESCRIBE" (#08, readkey$, regiondescr$, 0%, f1%(8))
            readkey$ = "PRICECODE" & pc$
            call "DESCRIBE" (#08 , readkey$, pcdescr$   , 0%, f1%(8))
            readkey$ = "INVREASON" & invrsn$
            call "DESCRIBE" (#08, readkey$, invrsndescr$, 0%, f1%(8))
            call "DESCRIBE" (#12, store$ , storedescr$ , 0%, f1%(12))
            call "CONVERT" (taxpct    , 2.4, taxpct$    )
            call "CONVERT" (invdiscpct, 2.2, invdiscpct$)
            call "CONVERT" (frtamt    , 2.2, frtamt$    )
            if cartons <> 0 then call "CONVERT" (cartons, 2.2, cartons$)
            if weight  <> 0 then call "CONVERT" (weight , 2.2, weight$ )
        set_currency_description
            init (" ") currdesc$(), effdate$, currmsg$
            if currency$ = statutory$ then return
            call "DESCRIBE" (#40, currency$, currdesc$(1), 0%, f1%(40))
            call "CONVERT" (convunt, 2.7, str(currdesc$(2), 1,10))
            currdesc$(2) = currdesc$(2) & " " & currency$
            currdesc$(2) = currdesc$(2) & "/" & statutory$
            effdate$ = convdate$
            call "DATEFMT" (effdate$)
            currmsg$ = "Currency rates effective " & effdate$
            return

        REM *************************************************************~
            *           L O A D   S A L E S   O R D E R                 *~
            *-----------------------------------------------------------*~
            * Gets the Sales Order and Scheduling Information.          *~
            *************************************************************
        load_sales_order
            soonfile%, bolonfile%, maxlines% = 0%

*        First See if the Order is on file
            readkey$ = str(cuscode$) & so$
            call "READ100" (#05, readkey$, soonfile%)
            if soonfile% = 1% then L31070
                errormsg$ = "Sales Order is not on file."  :  return
L31070:     invtype$ = " "     /* Init as blank */
            get #05 using L31080, export$, crhold$
L31080:         FMT POS(857), CH(1), POS(875), CH(1)
            if export$ = "Y" then L31100
                errormsg$ = "This is not an EXPORT Order"
                gosub delete_arinumbr  :  return
L31100:     invtype$ = "X"     /* Flag as an Export order */
            if crhold$ <> "C" then L31145
            u3% = 2%
            call "ASKUSER" (u3%,"**** CANCELLED ORDER ****",             ~
                 "This order has been Cancelled!!",                      ~
                 "You cannot invoice a cancelled order",                 ~
                 "Press Any PF-Key To Start Over")
            gosub delete_arinumbr  :  goto inputmode

L31145:     if crhold$ <> "H" then L31195
            u3% = 2%
            call "ASKUSER" (u3%,"**** CREDIT HOLD!! ****",               ~
                 "A Credit Hold Has Been Placed On This Order!!",        ~
                 "Press PF-16 To Continue With Order. ",                 ~
                 "Press Any Other PF-Key To Start Over")
            if u3% = 16% then L31195
                gosub delete_arinumbr  :  goto inputmode

*        Next see if BOL is on file -or- if order is scheduled
L31195:     if bol$ <> " " then L31245
                init (hex(00)) readkey$
                readkey$ = str(cuscode$) & str(so$)
                plowdescr$ = hex(06) & "BOL's on file for Sales Order "& ~
                             so$ & " are shown below"
                call "PLOWCODE" (#17,readkey$,plowdescr$,25%,0.0,f1%(17))
                    if f1%(17) <> 0% then bol$ = str(readkey$,26,3)
                    if f1%(17) <> 0% then L31250
                    errormsg$ = "Must enter Bill of Lading Number"
                    gosub delete_arinumbr  :  return
L31245:         readkey$ = str(cuscode$) & str(so$) & str(bol$)
L31250:         call "READ100" (#17, readkey$, bolonfile%)
                if bolonfile% = 1% then L31270
                    gosub delete_arinumbr
                    errormsg$ = "Bill of Lading not on file." : return
L31270:         invtype$ = " "     /* Init as blank */
                get #17 using L31280, readkey$, export$, expappr$
L31280:             FMT POS(234), CH(8), POS(246), CH(1), CH(1)
                if export$ = "Y" then L31300
                    errormsg$ = "This is not an EXPORT Bill of Lading"
                    gosub delete_arinumbr  :  return
L31300:         invtype$ = "X"     /* Flag as an Export order */
                if expappr$ = "Y" then L31325
                    errormsg$ = "This Order is NOT APPROVED for " &      ~
                                "shipment and cannot be invoiced"
                    gosub delete_arinumbr  :  return
L31325:     if readkey$ = " " then L31355
                errormsg$ = "BOL already recorded on Invoice " &         ~
                             readkey$
                gosub delete_arinumbr
            return

L31355
*        Now See if order is already in buffer.  If so can't touch
            call "REDALT0" (#19, so$, 2%, f1%(19))
            if f1%(19) = 0% then L31385
L31370:         errormsg$ = "Sales Order is currently being processed."
                gosub delete_arinumbr
                return
L31385:     readkey$ = " "     /* Flag order as in-process   */
            str(readkey$, 1, 3) = userid$
            str(readkey$, 4, 7) = all(hex(00))
            str(readkey$,11, 8) = "ARIXNPUT"
            str(readkey$,21,25) = str(cuscode$) & so$
            write #19 using L31420, readkey$, " ", " ", " ", " ",         ~
                                   eod goto L31370
L31420:         FMT CH(45), 3*CH(250), CH(225)


*        Now load up Sales Order and Scheduled Data
            prtmsg$ = hex(94) & "Loading Sales Information..." & hex(8c)
            print at(04,02), str(prtmsg$)
            get #05 using L35030, po$, shipto$(), soldto$(), terms$,      ~
                howship$, fob$, salesacct$, discacct$, salesman$(),      ~
                comm%(), region$,  store$, pc$, invdiscpct, crhold$,     ~
                currency$, sotmp$, invtmp$
            if pm_on$ <> "Y" then L31470
            if sotmp$  <> " " then pm_so$  = sotmp$
            if invtmp$ <> " " then pm_inv$ = invtmp$

L31470:     if currency$ = " " then currency$ = statutory$
            gosub get_currency_info

            get #17 using L31495, carrier$, howship$, fob$, shipdate$,    ~
                frtbill$, cartons, weight, frtamt, textid$
L31495:         FMT XX(37), CH(6), 2*CH(20), XX(100), CH(6), CH(20),     ~
                    3*PD(14,4), POS(242), CH(4)
            get #17 using L31504, store$  /*Allows ship frm diff stores*/
L31504:          FMT POS(29), CH(3)
            call "TXTFUTIL" (#22, f2%(22), "LOAD", textid$)
            gosub get_tax_defaults  :  gosub get_account_defaults
                call "GLUNFMT" (aracct$ )
                call "GLUNFMT" (taxacct$)
                call "GLUNFMT" (frtacct$)
            artype$  = "A"
            invdate$ = date
*          GOSUB GET_TAX_DEFAULTS
            gosub format_header_data

*        Now Read in line items from BCKLINES
            c%, maxlines% = 0%
            readkey$ = str(so$) & hex(00)
L31570:     call "PLOWNEXT" (#06, readkey$, 16%, f1%(6))
                if f1%(6) = 0% then L32135
            get #06 using L31585, temp$, openqty
L31585:         FMT POS(26), CH(3), POS(109), PD(14,4)
            if openqty <= 0 then L31570  /* Nothing left to Ship    */

            readkey1$ = str(so$) & str(bol$) & temp$
            call "READ100" (#18, readkey1$, f1%(18))
                if f1%(18) = 0% then L32040
            get #18 using L31620, qtyschd
L31620:         FMT POS(32), PD(14,4)
            if qtyschd <= 0 then L31570  /* Nothing scheduled */
                if maxlines% < 100% then L31635
                   gosub severe_load_error
                   goto L32135

L31635:     c%, maxlines% = c% + 1%
                get #6 using L35195,                                      ~
                     soseq$(c%), item$(c%), part$(c%), descr$(c%),       ~
                     cat$(c%), soorder(c%), qtyshipped(c%), soopen(c%),  ~
                     qtyschld(c%), qtypreinv(c%), pricestk(c%),          ~
                     stkuom$(c%), priceuom$(c%), conv(c%), price(c%),    ~
                     linediscpct(c%), taxable$(c%), sales$(c%),          ~
                     discs$(c%), dfltlot$, project$(c%)
        REM Get transaction amounts from the BCK 'shadow' file (BCKCURCY)
            if currency$ = statutory$ then goto L31715 /* Not Stat, tho */
            if curr$ <> "Y" then goto L31715 /* Nor if no multi-currency*/
            call "READ100" (#44, readkey$, f1%(44))
            if f1%(44) <> 0% then get #44 using L31705, pricestk(c%),     ~
                price(c%), convdate$, conveqv, convunt
L31705:         FMT POS(24), 2*PD(14,4), CH(6), 2*PD(14,7)
                gosub set_currency_description
L31715:     order(c%) = soorder(c%)
            temp% = 0%
            convert sn_last_lineid$ to temp%, data goto L31730
L31730:     temp% = temp% + 1%
            convert temp% to sn_lineid$(c%), pic(0000)
            sn_last_lineid$ = sn_lineid$(c%)
            call "ARIEXTRA" (cuscode$, part$(c%), " ",                   ~
                             e_lines%(c%), #2)
            total_e_lines% = total_e_lines% + e_lines%(c%)
            lotqtys(c%,1) = ship(c%)
            convert c% to seq$(c%), pic(###)
            lsts$(c%) = "O"

            get #18 using L31775, ship(c%), lot$(), lotqty(),             ~
                                 textidl$(c%), completemsg$(c%)
L31775:          FMT XX(31), PD(14,4), 30*CH(6), 30*PD(14,4), CH(4),     ~
                     POS(476), CH(1)
            bolqty(c%) = ship(c%)
            call "TXTFUTIL" (#22, f2%(22), "LOAD", textidl$(c%))
            for l% = 1% to 30%
                lots$   (c%, l%) = lot$  (l%)
                buflot$ (c%, l%) = lot$  (l%)
                postlot$(c%, l%) = lot$  (l%)
                lotqtys (c%, l%) = lotqty(l%)
                bufqty  (c%, l%) = lotqty(l%)
                postqty (c%, l%) = lotqty(l%)
                if shipdate$ = " " or shipdate$ = blankdate$ then L31940
                    sn_index2%(c%,l%) = c% * 100% + l%
                    sn_trankey$ = str(so$) & str(bol$) & soseq$(c%)
                    convert l% to str(sn_trankey$,23,2), pic(00)
                    call "SERLOAD" (sn_index2%(c%,l%), "RS", sn_trankey$,~
                                   25%, " ", " ", #02, #34, #32, #33, u3%)

                    if u3% > 0% then L31880
                        sn_index2%(c%,l%) = 0%
                        goto L31940
L31880:             readkey1$ = "RS" & sn_trankey$
                    readkey2$ = "RT" & str(cuscode$) &                   ~
                                 str(invnr$) & str(sn_lineid$(c%)) &     ~
                                 str(sn_trankey$,23,2)
                    if invnr$ = " " then str(readkey2$,12,8) =           ~
                                         userid$ & hex(06)
L31910:             call "PLOWNEXT" (#34, readkey1$, 42%, f1%(34))
                        if f1%(34) = 0% then L31940
                    put #34 using L31925, readkey2$
L31925:                 FMT POS(1), CH(42)
                    write #34
                    goto L31910
L31940:         next l%
                if shipdate$ <> " " and shipdate$ <> blankdate$ then L32040
                     call "SERENABL" (part$(c%), sn_enable%, lc%, #2, #4)
                     if sn_enable% = 0% then L31975
                          askmsg$(2) = "Serial numbered parts from sche"&~
                                       "duled BOL not recorded yet!"
                          goto L32005
L31975:              lotqtys(c%,1) = ship(c%)
                     call "HNYAVAIL" (#4, #13, part$(c%), store$,        ~
                                      dfltlot$, errormsg$, ship(c%),     ~
                                      temp1, f1%(13))
                     if errormsg$ = " " then L32035
                          askmsg$(2) = errormsg$
L32005:                   u3% = 0%
                          askmsg$(1) = "CUTOVER 'BOL' CONFLICT"
                          askmsg$(3) = "Part: " & part$(c%)
                          call "ASKUSER" (u3%, askmsg$(1), askmsg$(2),   ~
                                         askmsg$(3), askmsg$(4))
                          ship(c%), lotqtys(c%,1) = 0
L32035:             lots$  (c%,1) = dfltlot$
L32040:         lineext(c%) = round(ship(c%) * price(c%) / conv(c%), 2)
                linediscamt(c%) =                                        ~
                            round(lineext(c%) * linediscpct(c%) * .01, 2)
                linediscamt(c%) = -linediscamt(c%)
                lineext(c%) = lineext(c%) + linediscamt(c%)
                openqty(c%) = max(0, soopen(c%) - ship(c%))
                if closeso$ = "A" and completemsg$(c%) <> " " then       ~
                                                        openqty(c%) = 0
                call "GLFMT"  (sales$(c%))
                call "GLFMT"  (discs$(c%))
                call "READ100" (#04, part$(c%), f1%(4))
                if f1%(4) = 0% then nonstockmsg$(c%) = "(Non-Stock Part)"
                if completemsg$(c%) = "C" then                           ~
                   completemsg$(c%) = "Reported Complete in Shipping"    ~
                   else completemsg$(c%) = " "

                if pxref% + cxref% + c_cxref% < 3% then L32125
                     call "PTUSEDSB" ("R", "BCK ", so$, soseq$(c%),      ~
                                      cus_part$(c%),                     ~
                                      str(cus_part_descr$(c%),,30%),     ~
                                      cus_part_type$(c%), ret%)
                     if ret% = 1% then L32125
                          cus_part$(c%) = "** No Cross Reference **"
                          cus_part_descr$(c%) = " "

L32125:     goto L31570

L32135
*        Final tests after load

            if maxlines% + total_e_lines% <= 100% then L32143
               gosub extra_load_conflict

L32143
*        Lastly, and leastly, check that default lot info is A-ok fine.
            for c% = 1% to maxlines%
                lot_enable%, sn_enable% = 0%
                if shipdate$ = " " or shipdate$ = blankdate$ or ~
                  bolonfile% = 0% then L32180
                     readkey1$ = str(so$) & str(bol$) & soseq$(c%)
                     call "READ100" (#18, readkey1$, f1%(18))
                     if f1%(18) = 1% then L32290
                call "SERENABL" (part$(c%), sn_enable%,  lot%, #02, #04)
                     if ship(c%)  = 0 then sn_enable% = 0%
L32180:         call "LOTENABL" (part$(c%), lot_enable%, lot%, #02, #04)
                     if ship(c%)  = 0 then lot_enable% = 0%
                     if lot_enable% = 0% then lots$(c%,1%) = " "
                if ship(c%) = 0 or                                       ~
                       (lot_enable% < 2% and sn_enable% = 0%) then L32290
                   if lot_enable% = 2% then                              ~
                              errormsg$ = "Please supply Lot Number(s)."
                   if lot_enable% = 2% and lots$(c%,1) = " " then L32250
                     readkey$ = str(part$(c%)) & str(store$) & lots$(c%,1)
                     call "READ100" (#13, readkey$, f1%(13))
                     if f1%(13) = 1% and sn_enable% = 0% then L32290
                          if f1%(13) = 0% then                           ~
                               errormsg$ = "Invalid Lot Number"     else ~
                               errormsg$ = "Serial Numbers Required"
L32250:                   gosub describe_line
                          fieldnr% = 5%
                          gosub'050(4%, fieldnr%, 2%)
L32265:                   gosub'104(fieldnr%, 2%)
                               if keyhit%  =  1 then gosub startover
                               if keyhit% <>  0 then L32265
                          gosub'154(fieldnr%, 2%)
                               if errormsg$ <> " " then L32265
L32290:     next c%
            errormsg$ = " "
            return


        delete_arinumbr   /* Delete ARINUMBR record in some cases */
            if invtype$ <> "X" and invtype$ <> " " then return
            if invnr$    = " " then return
                readkey$ = str(cuscode$) & invnr$
                call "DELETE" (#23, readkey$, 17%)
                return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * (1) Remove Current Copy of Invoice if required.  All      *~
            *     done if deleting.                                     *~
            * (3) Get Invoice Number if required.                       *~
            * (4) Save Invoice to appropriate files.                    *~
            *************************************************************
        save_data
            if delemsg$ = " " then prtmsg$ = "Saving Invoice..."         ~
                              else prtmsg$ = "Deleting Entry..."
            print at(04,02), str(prtmsg$)
            if invonfile% = 0% then L33085
                readkey$ = str(cuscode$) & invnr$
                call "DELETE" (#09, readkey$, 17%) /* Header   */
                call "DELETE" (#10, readkey$, 17%) /* Lines    */
                call "DELETE" (#42, readkey$, 17%) /* Currency */
                call "DELETE" (#43, readkey$, 17%) /* Currency */
                call "PTUSEDSB" ("D", "ARI ", readkey$, "ALL",           ~
                                 " ", " ", " ", ret%)/* Del Xref Shadow */

L33085
*        If the DELETE message is not blank then awway we go with this
*        invoice.  If a BOL is behind all this it needs to be restored.
        if delemsg$ = " " then L33360
            call "FILEBGON" (#33)
            for c% = 1% to maxlines% : for l% = 1% to 30%
              if sn_index2%(c%,l%) = 0% then L33155
                sn_trankey$ = str(cuscode$) & str(invnr$) &              ~
                                              str(sn_lineid$(c%)) & "##"
                if invnr$ = " " then str(sn_trankey$,10,8) =             ~
                                                        userid$ & hex(06)
                convert l% to str(sn_trankey$,22,2), pic(00)
                call "SERSAVE" (sn_index2%(c%,l%), "RT", sn_trankey$,    ~
                                25%, part$(c%), userid$, "4", "2", 1%,   ~
                                #02, #34, #32, #33)
L33155:     next l%  :  next c%

            if bol$ = " " then L33310
                readkey$ = str(cuscode$) & str(so$) & bol$
                call "READ100" (#17, readkey$, f1%(17)) /* SHPHDRS */
                if f1%(17) = 0% then L33310
                     if invonfile% = 0% then L33280
                     readkey1$ = "RS" & str(so$) & str(bol$)& hex(00)
L33195:              call "PLOWNEXT" (#34, readkey1$, 21%, f1%(34))
                     if f1%(34) = 0% then L33280
                          get #34 using L33210, readkey$
L33210:                        FMT POS(43), CH(45)
                          call "REDALT1" (#32, readkey$, 1%, f1%(32))
                          if f1%(32) = 0% then L33195
                               get #32 using L33230, readkey$
L33230:                             FMT POS(1), CH(1)
                               if readkey$ = "2" then L33250
                                    call "DELETE" (#34, readkey1$, 62%)
                                    goto L33195
L33250:                        get #34 using L33255, readkey$
L33255:                             FMT POS(1), CH(42)
                               put #32 using L33265, hex(74), readkey$
L33265:                             FMT POS(1), CH(1), POS(216), CH(42)
                               rewrite #32
                               goto L33195
L33280:               readkey$ = str(cuscode$) & str(so$) & bol$
                      call "READ101" (#17, readkey$, f1%(17))
                      put #17 using L33295, " "
L33295:                 FMT POS(234), CH(8)
                      rewrite #17

L33310:         if invnr$ = " " then L33325
                     readkey$ = str(cuscode$) & invnr$
                     call "DELETE" (#23, readkey$, 17%)
L33325:         if invonfile% = 0% then clear_inprocess
                     for c% = 1% to maxlines%
                          gosub remove_hold
                     next c%
                     goto clear_inprocess


L33360
*        Assign Invoice Number if so required.
            if invnr$ <> " " then L33395
                call "ARINEXT" (#12, #02, #23, #25, "X", store$,         ~
                                cuscode$, billxref$, invnr$)
                inpmessage$ = "Saving Invoice " & invnr$ & "..."
                print at(04,02), str(inpmessage$)

L33395
*        Now throw the invoice into the files, lines 1st then Hdr.
            seq% = 0%
            stgrossinv, sttaxable = 0
            for c% = 1% to maxlines%
                if lsts$(c%) <> "D" then L33465
                    gosub remove_hold
                    for l% = 1% to 30%
                        if sn_index2%(c%, l%) = 0% then L33445
                            readkey1$ = bin(sn_index2%(c%,l%),3)
                            call "DELETE" (#33, readkey1$, 3%)
L33445:             next l%
                    call "TXTFUTIL" (#22, f2%(22%), "XOUT", textidl$(c%))
                    goto L33485

L33465:         gosub place_hold
                seq% = seq% + 1%  : convert seq% to seq$(c%), pic(###)
                call "GLUNFMT"  (sales$(c%))
                call "GLUNFMT"  (discs$(c%))
L33485:         for l% = 1% to 30%
                     lot$  (l%) = lots$  (c%,l%)
                     lotqty(l%) = lotqtys(c%,l%)
                     if sn_index2%(c%,l%) = 0% then L33545
                          sn_trankey$ = str(cuscode$) & str(invnr$) &    ~
                                        str(sn_lineid$(c%)) & "##"
                          convert l% to str(sn_trankey$,22,2), pic(00)
                          sn_clear% = 0%
                          call "SERSAVE" (sn_index2%(c%,l%), "RT",       ~
                                          sn_trankey$, 25%, part$(c%),   ~
                                          userid$, "4", "2", sn_clear%,  ~
                                          #02, #34, #32, #33)
L33545:         next l%
                if lsts$(c%) = "D" then L33680  /* Skip deleted lines */
                if nonstockmsg$(c%) <> " " then nonstockmsg$(c%) = "Y"
                if curr$ <> "Y" then goto L33635
                if currency$ = statutory$ then goto L33635
                write #43 using L33585, currency$, cuscode$, invnr$,      ~
                    seq$(c%), pricestk(c%), price(c%), linediscamt(c%),  ~
                    lineext(c%), convdate$, conveqv, convunt, " "
L33585:                      FMT CH(4), CH(9), CH(8), CH(3), 4*PD(14,4), ~
                                 CH(6), 2*PD(14,7), CH(22)
                pricestk(c%)    = round(pricestk(c%)    * conveqv, 4)
                price   (c%)    = round(price   (c%)    * conveqv, 4)
                linediscamt(c%) = round(linediscamt(c%) * conveqv, 4)
                lineext(c%)     = round(lineext (c%)    * conveqv, 4)
                stgrossinv      = stgrossinv + lineext(c%)
                     if taxable$(c%) = "Y" then                          ~
                                  sttaxable = sttaxable + lineext(c%)

L33635:         write #10 using L35940, cuscode$, invnr$, seq$(c%),       ~
                    item$(c%), part$(c%), descr$(c%), cat$(c%),          ~
                    order(c%), ship(c%), openqty(c%), pricestk(c%),      ~
                    stkuom$(c%), priceuom$(c%), conv(c%), price(c%),     ~
                    linediscpct(c%), linediscamt(c%), lineext(c%),       ~
                    taxable$(c%), sales$(c%), discs$(c%), " ",           ~
                    textidl$(c%), soseq$(c%), lot$(), lotqty(),          ~
                    nonstockmsg$(c%), project$(c%), sn_lineid$(c%), " ", ~
                    e_lines%(c%), " "

                if cus_part_type$(c%) = " " then L33680
                if cus_part$(c%) = "** No Cross Reference **" then L33680
                call "PTUSEDSB"("W","ARI ", str(cuscode$) & invnr$,      ~
                                      seq$(c%), cus_part$(c%),           ~
                                      str(cus_part_descr$(c%),,30%),     ~
                                      cus_part_type$(c%), ret%)

L33680:     next c%

            call "GLUNFMT"  (aracct$   )
            call "GLUNFMT"  (frtacct$  )
            call "GLUNFMT"  (taxacct$  )
            call "GLUNFMT"  (salesacct$)
            call "GLUNFMT"  (discacct$ )
            call "DATUNFMT" (invdate$ )
            call "DATUNFMT" (expdate$ )
            call "DATUNFMT" (shipdate$)
            bal = 0  :  /* Filler */
            if curr$ <> "Y" then goto L33830
            if currency$ = statutory$ then goto L33830
                write #42 using L33760, currency$, cuscode$, invnr$,      ~
                           grossinv, invdiscamt, frtamt, taxamt, netinv, ~
                           bal, convdate$, conveqv, convunt,             ~
                           termsdue(), " "
L33760:                FMT CH(4), CH(9), CH(8), 6*PD(14,4), CH(6),       ~
                           2*PD(14,7), 30*PD(14,4), CH(69)
                grossinv   = stgrossinv
                invdiscamt = round(grossinv   * invdiscpct * .01, 4)
                invdiscamt = -invdiscamt
                frtamt     = round(frtamt     * conveqv, 4)
                taxabledsc = round(sttaxable  * invdiscpct * .01, 4)
                taxable    = sttaxable - taxabledsc
                taxamt     = round(taxable    * taxpct     * .01, 4)
                netinv     = grossinv + invdiscamt + frtamt + taxamt
                for i% = 1% to 30%
                     termsdue(i%) = round(termsdue(i%) * conveqv, 4)
                next i%

L33830:     if stlmnt$ <> " " then terms$ = " "
            get #01 using L33840, acctxref$, billxref$, custype$
L33840:         FMT POS(771), 2*CH(9), POS(1023), CH(2)

            for u3% = 1% to 30%
                if termsdisc$(u3%) = " " then termsdisc$(u3%) = blankdate$
                if termsnet$(u3%)  = " " then termsnet$(u3%)  = blankdate$
            next u3%

            put #09 using L35370, cuscode$, invnr$, po$, so$, bol$,       ~
                shipto$(), soldto$(), shipdate$, howship$, fob$,         ~
                carrier$, cartons, weight, frtbill$, salesman$(),        ~
                comm%(), region$, pc$, invdate$, expdate$, " ", date,    ~
                userid$, vf$, aracct$, frtacct$, taxacct$, salesacct$,   ~
                discacct$, grossinv, invdiscpct, invdiscamt, frtamt,     ~
                taxamt, netinv, bal, billxref$, stlmnt$, store$,         ~
                taxcode$, taxpct, "X", invrsn$, textid$, posthny$,       ~
                " ", artype$, terms$, termsdue(), termsdiscs(),          ~
                termsdisc$(), termsnet$(), custype$, acctxref$,          ~
                currency$, billxref$, cuscode$, invnr$, pm_so$, pm_inv$, ~
                " ", session$, "X", cuscode$, invnr$

            write #09
                call "TXTFUTIL" (#22, f2%(22), "SAV2", " ")

*        Flag BOL as invoiced ("O" Types with BOL specified)
            if bol$ = " " then L33975
                readkey$ = str(cuscode$) & str(so$) & bol$
                call "READ101" (#17, readkey$, f1%(17))
                if f1%(17) = 0% then L33975
                     put #17 using L33955, invnr$
L33955:                   FMT POS(234), CH(8)
                     rewrite #17

*        Clear SO In-process and go get next invoice
L33975:     goto clear_inprocess


        remove_hold
            if invonfile% = 0% then return
            for l% = 1% to 30%
                if bufmark$(c%,l%) <> " " then L34125
                if bufqty(c%,l%) <= 0 then L34120
                   postqty, bufqty = 0
                   for i% = l% to 30%
                     if bufmark$(c%,i%) <> " " then L34050
                     if bufqty  (c%,i%) <= 0   then L34045
                     if buflot$(c%,i%) <> buflot$(c%,l%) then L34050
                        bufqty = bufqty + bufqty (c%,i%)
L34045:                 bufmark$(c%,i%) = "*"
L34050:            next i%
                if bolonfile% = 0% then L34100
                   for i% = 1% to 30%
                     if postmark$(c%,i%) <> " " then L34090
                     if postqty  (c%,i%) <= 0   then L34085
                     if postlot$(c%,i%) <> buflot$(c%,l%) then L34090
                        postqty = postqty + postqty (c%,i%)
L34085:                 postmark$(c%,i%) = "*"
L34090:            next i%

L34100:         held = max(0, bufqty - postqty)
                if held = 0 then L34120
                call "HNYHOLD" (#13, part$(c%), store$, buflot$(c%,l%),  ~
                                         -held, return%)
L34120:         bufmark$(c%,l%) = "*"
L34125:     next l%
            return


        place_hold
            for l% = 1% to 30%
                if mark$(c%,l%) <> " " then L34325
                if lotqtys(c%,l%) <= 0 then L34320
                   lotqty, postqty, bufqty = 0
                   for i% = l% to 30%
                     if mark$(c%,i%) <> " " then L34200
                     if lotqtys (c%,i%) <= 0   then L34195
                     if lots$  (c%,i%) <> lots$  (c%,l%) then L34200
                        lotqty = lotqty + lotqtys(c%,i%)
L34195:                 mark$(c%,i%) = "*"
L34200:            next i%
                if invonfile% = 0% then L34250
                   for i% = 1% to 30%
                     if bufmark$(c%,i%) <> " " then L34240
                     if bufqty  (c%,i%) <= 0   then L34235
                     if buflot$(c%,i%) <> lots$  (c%,l%) then L34240
                        bufqty = bufqty + bufqty (c%,i%)
L34235:                 bufmark$(c%,i%) = "*"
L34240:            next i%
                if bolonfile% = 0% then L34290
L34250:            for i% = 1% to 30%
                     if postmark$(c%,i%) <> " " then L34280
                     if postqty  (c%,i%) <= 0   then L34275
                     if postlot$(c%,i%) <> lots$   (c%,l%) then L34280
                        postqty = postqty + postqty (c%,i%)
L34275:                 postmark$(c%,i%) = "*"
L34280:            next i%

L34290:         held = max(0, bufqty - postqty)
                needed = max(0, lotqty - postqty)
                held = needed - held
                if held = 0 then L34320
                call "HNYHOLD" (#13, part$(c%), store$, lots$(c%,l%),    ~
                                          held, return%)
L34320:         mark$(c%,l%) = "*"
L34325:     next l%

            gosub remove_hold

            return

        clear_inprocess  /* Clear SO in-process tasking record         */
            readkey$ = all(hex(00))       /* Clear In-process Flag */
            str(readkey$,,3) = userid$
            call "DELETE" (#19, readkey$, 10%)
            if from_startover% <> 1% then L34430
                from_startover% = 0%
                goto inputmode
L34430
*       ** Now write invoice print tickler file
            readkey$ = "xxx" & str(cuscode$) & str(invnr$)
            call "READ100" (#26, readkey$, f1%(26))
                if f1%(26) = 1% then inputmode  /* Already there */
            write #26 using L34480, "xxx", cuscode$, invnr$
L34480:         FMT CH(3), CH(9), CH(8)
            goto inputmode

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE #05 -- BCKMASTR  (Read Only)       */~
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
            XX(200),        /* Variable Fields                         */~
            XX(4),          /* Internal ID to text in TXTFH.D.         */~
            CH(3),          /* Store Code                              */~
            XX(6),          /* Order Date                              */~
            XX(6),          /* Cancellation Date                       */~
            XX(6),          /* Due Date default                        */~
            XX(6),          /* Date Released                           */~
            XX(6),          /* Originally Input On (date)              */~
            XX(3),          /* Originally Input By (user)              */~
            XX(6),          /* Last Modified On (date)                 */~
            XX(3),          /* Last Modified By (user)                 */~
            XX(9),          /* Adjustment Reason Code                  */~
            XX(1),          /* Export FLag                             */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Order Discount Percent                  */~
            XX(8),          /* Open Order Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            POS(893), CH(4),/* Currency code                           */~
            XX(1),          /* How put on hold                         */~
            CH(1),          /* PM Surcharge SO Flag                    */~
            CH(1)           /* PM Surcharge INV Flag                   */

L35195: FMT                 /* FILE #06 -- BCKLINES                    */~
            XX(9),          /* Customer Code                           */~
            XX(16),         /* Sales Order number                      */~
            CH(3),          /* Sequence Number                         */~
            CH(3),          /* Item Number                             */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part Number description                 */~
            CH(4),          /* Category code                           */~
            PD(14,4),       /* Order Quantity                          */~
            PD(14,4),       /* Quantity Shipped & Invoiced             */~
            PD(14,4),       /* Quantity Open                           */~
            PD(14,4),       /* Quantity scheduled for Shipment         */~
            XX(08),         /* Filler                                  */~
            PD(14,4),       /* Quantity Pre-invoiced (Shipped NOT Invd)*/~
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
            XX(1),          /* Demand Type                             */~
            XX(1),          /* Priority Code                           */~
            XX(4),          /* Internal ID to text in TXTFILE.         */~
            XX(1),          /* Allocation Flag                         */~
            XX(54)          /* Filler                                  */


L35370: FMT                 /* FILE #09 -- ARIBUFFR                    */~
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
            CH(4),          /* Currency code                           */~
            CH(9),          /* Bill-to X-Ref                           */~
            CH(9),          /* Customer code                           */~
            CH(8),          /* Invoice Number                          */~
            CH(1),          /* PM Surcharge SO Flag                    */~
            CH(1),          /* PM Surcharge INV Flag                   */~
            CH(190),        /* Filler (Internal, unused space)         */~
            CH(6),          /* Session Number (all HEX(00) for EXPORT) */~
            CH(1),          /* Invoice Type ('X' for Export)           */~
            CH(9),          /* Customer Number                         */~
            CH(8)           /* Invoice Number                          */

L35940: FMT                 /* FILE #10 -- ARIBUF2                     */~
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
            CH(25),         /* Filler (Internal, unused space)         */~
            BI(4),          /* Extra lines                             */~
            CH(92)          /* Filler (Internal, unused space)         */

        REM *************************************************************~
            * Extra Lines Conflict Warnings/Errors                      *~
            *************************************************************
        extra_load_conflict

L36550: % Current No. of Lines: ###  (+ Implied Lines: ###) : Total #####

            put askmsg$(1%) using L36550, maxlines%, total_e_lines%,      ~
                                         maxlines% + total_e_lines%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press any PF key to confirm and continue"
            u3% = 2%
            call "ASKUSER" (u3%, "* * * MAXIMUM LINES CONFLICT * * *",   ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            return

        extra_append_conflict
            put askmsg$(1%) using L36550, c%, total_e_lines% + temp%,     ~
                                         c% + total_e_lines% + temp%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press PF16 to continue, Press RETURN to" &    ~
                          " re-enter Part Code"
L36720:     u3% = 2%
            call "ASKUSER" (u3%, "* * * APPEND LINES CONFLICT * * *",    ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            if u3% = 16% then return
            if u3% =  0% then return
               goto L36720

        severe_load_error
            askmsg$(1%) = "Attempting to Load in excess of 100 Lines"
            askmsg$(2%) = "This Unexpected Situation may cause" &        ~
                          " LOSS OF DATA"
            askmsg$(3%) = "Press PF25 to acknowledge and continue"
L36840:     u3% = 2%
            call "ASKUSER" (u3%, "* * * EXCESS LINES ERROR * * *",       ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            if u3% = 25% then return
               goto L36840

        REM *************************************************************~
            *   A C C E S S    L O C A T I O N    M A N A G E M E N T   *~
            *___________________________________________________________*~
            * Call HNYLCSUB and pass the Part, Store, Lot and Quantity  *~
            * so that the user does not have to re-enter them.          *~
            *************************************************************

         locations

            idx% = cursor%(1) - 5%
            if idx% < 1% or idx% > 15% then return
            idx% = idx% + t%
            if idx% < 1% or idx% > maxlines% then return

            call "HNYLCSUB"  (part$(idx%),                               ~
                              store$,                                    ~
                              lot$(idx%),                                ~
                              ship(idx%),                                ~
                              4%,    /*  Withdrawl Mode                */~
                              #02,   /*  SYSFILE2                      */~
                              #12,   /*  STORNAME                      */~
                              #27,   /*  USERINFO                      */~
                              #04,   /*  HNYMASTR                      */~
                              #28,   /*  HNYLOCNS                      */~
                              #13,   /*  HNYQUAN                       */~
                              #29)   /*  LOCATION                      */
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)       /* Header Screen 1            */
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf1
            on fieldnr%      gosub  L40110,         /* Customer Code    */~
                                    L40110,         /* Invoice Number   */~
                                    L40110,         /* Sales Order-BOL  */~
                                    L40110,         /* Settlement Number*/~
                                    L40110,         /* Ship-to Address  */~
                                    L40110,         /* Sold-to Address  */~
                                    L40110,         /* Invoice Date     */~
                                    L40110,         /* Customer PO      */~
                                    L40110,         /* Store Number     */~
                                    L40110          /* Expiration Date  */
            goto L40125
                                    lfac$(fieldnr%) = hex(80) : return
L40110:                             lfac$(fieldnr%) = hex(81) : return
                                    lfac$(fieldnr%) = hex(82) : return

L40125:     accept                                                       ~
               at (01,02), "EXPORT Invoicing",                           ~
               at (01,47), "Heading Page 1",                             ~
               at (01,62), "    Today:",                                 ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Ship-to Customer Code",                      ~
               at (06,30), fac(lfac$( 1)), cuscode$             , ch(09),~
               at (06,49), fac(hex(8c)),   cusdescr$            , ch(30),~
                                                                         ~
               at (07,02), fac(hex(8c)), fd_invnr$,                      ~
               at (07,30), fac(lfac$( 2)), invnr$               , ch(08),~
                                                                         ~
               at (08,02), fac(hex(8c)), fd_so$,                         ~
               at (08,30), fac(lfac$( 3)), so$                  , ch(16),~
               at (08,47), fac(lfac$( 3)), bol$                 , ch(03),~
                                                                         ~
               at (09,02), "Settlement Number",                          ~
               at (09,30), fac(lfac$( 4)), str(stlmnt$,,8)      , ch(08),~
               at (09,39), fac(lfac$( 4)), str(stlmnt$, 9)      , ch(02),~
                                                                         ~
               at (10,02), "Ship",  at (11,02), "  To",                  ~
               at (10,08), fac(lfac$( 5)), shipto$(1)           , ch(30),~
               at (11,08), fac(lfac$( 5)), shipto$(2)           , ch(30),~
               at (12,08), fac(lfac$( 5)), shipto$(3)           , ch(30),~
               at (13,08), fac(lfac$( 5)), shipto$(4)           , ch(30),~
               at (14,08), fac(lfac$( 5)), shipto$(5)           , ch(30),~
               at (15,08), fac(lfac$( 5)), str(shipto$(6), 1,17), ch(17),~
               at (15,26), fac(lfac$( 5)), str(shipto$(6),19, 2), ch(02),~
               at (15,29), fac(lfac$( 5)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (10,42), "Sold",  at (11,42), "  To",                  ~
               at (10,48), fac(lfac$( 6)), soldto$(1)           , ch(30),~
               at (11,48), fac(lfac$( 6)), soldto$(2)           , ch(30),~
               at (12,48), fac(lfac$( 6)), soldto$(3)           , ch(30),~
               at (13,48), fac(lfac$( 6)), soldto$(4)           , ch(30),~
               at (14,48), fac(lfac$( 6)), soldto$(5)           , ch(30),~
               at (15,48), fac(lfac$( 6)), str(soldto$(6), 1,17), ch(17),~
               at (15,66), fac(lfac$( 6)), str(soldto$(6),19, 2), ch(02),~
               at (15,69), fac(lfac$( 6)), str(soldto$(6),22, 9), ch(09),~
                                                                         ~
               at (16,02), fac(hex(8c)), fd_invdate$,                    ~
               at (16,30), fac(lfac$( 7)), invdate$             , ch(08),~
               at (16,48), fac(hex(8c)), currmsg$               , ch(33),~
                                                                         ~
               at (17,02), "Customer Purchase Order",                    ~
               at (17,30), fac(lfac$( 8)), po$                  , ch(16),~
                                                                         ~
               at (18,02), "Store Code",                                 ~
               at (18,30), fac(lfac$( 9)), store$               , ch(03),~
               at (18,49), fac(hex(8c)),   storedescr$          , ch(32),~
                                                                         ~
               at (19,02), fac(hex(8c)), fd_expdate$,                    ~
               at (19,30), fac(lfac$(10)), expdate$             , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L40460
                gosub call_customer_credit
                goto L40125

L40460:        if keyhit% <> 13 then L40475
                  call "MANUAL" ("ARIXNPUT") : goto L40125

L40475:        if keyhit% <> 15 then L40490
                  call "PRNTSCRN" : goto L40125

L40490:         if edit% = 1% then return
                close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf1
        if edit% = 2% then L40625         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "(14/30)Scan Holding File                 (26)Custo"&~
                    "mer Credit   (16)Exit Program"
           pfkey$ = hex(01ffff04ffffffffffffffff0d0e0f101eff1a00)
           if fieldnr% > 1% then L40570
                str(pf$(3%),42%,19%) = " " : str(pfkey$,19%,1%) = hex(ff)
L40570:    if fieldnr% = 1% then L40585
                str(pf$(3),64)   = " "
                str(pfkey$,16,1) = hex(ff)
L40585:    if fieldnr% > least% then L40600
                str(pf$(2),17,18) = " "
                str(pfkey$, 4, 1) = hex(ff)
L40600:    if fieldnr% <= 2% then L40615
                str(pf$(3),,30) = " "
                str(pfkey$,14,1), str(pfkey$,17,1) = hex(ff)
L40615:    return

L40625:  if fieldnr% > 0% then L40680     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                            (12)Delet"&~
                    "e Invoice    (13)Instructions"
           pf$(2) = "(2)Line Items    (5)Next Screen          (25)Manag"&~
                    "e Text       (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit   (16)End Invoice "
           pfkey$ = hex(0102ffff05ffffffffffff0c0dff0f10ff191d1a00)
           return

                                         /* Edit Mode- Field Enabled   */
L40680:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)       /* Header Screen 2            */
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf2
            on fieldnr%       gosub L41105,         /* Date Shipped     */~
                                    L41100,         /* How Ship         */~
                                    L41100,         /* FOB              */~
                                    L41105,         /* Carrier          */~
                                    L41100,         /* Freight Bill     */~
                                    L41110,         /* Cartons          */~
                                    L41110,         /* Weight           */~
                                    L41105,         /* Currency         */~
                                    L41110          /* Freight Charges  */
            goto L41120
L41100:                             lfac$(fieldnr%) = hex(80) : return
L41105:                             lfac$(fieldnr%) = hex(81) : return
L41110:                             lfac$(fieldnr%) = hex(82) : return

L41120:     accept                                                       ~
               at (01,02), "EXPORT Invoicing",                           ~
               at (01,47), "Heading Page 2",                             ~
               at (01,62), "    Today:",                                 ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Date Shipped",                               ~
               at (06,30), fac(lfac$( 1)), shipdate$            , ch(08),~
                                                                         ~
               at (07,02), "How Ship",                                   ~
               at (07,30), fac(lfac$( 2)), howship$             , ch(20),~
                                                                         ~
               at (08,02), "FOB",                                        ~
               at (08,30), fac(lfac$( 3)), fob$                 , ch(20),~
                                                                         ~
               at (09,02), "Carrier",                                    ~
               at (09,30), fac(lfac$( 4)), carrier$             , ch(06),~
               at (09,49), fac(hex(8c)),   carrierdescr$        , ch(32),~
                                                                         ~
               at (10,02), "Air/Frt Bill Number",                        ~
               at (10,30), fac(lfac$( 5)), frtbill$             , ch(20),~
                                                                         ~
               at (11,02), "Shipment- # of Cartons",                     ~
               at (11,30), fac(lfac$( 6)), cartons$             , ch(10),~
                                                                         ~
               at (12,02), "          Weight",                           ~
               at (12,30), fac(lfac$( 7)), weight$              , ch(10),~
                                                                         ~
               at (13,02), "Currency",                                   ~
               at (13,30), fac(lfac$( 8)), currency$            , ch(04),~
               at (13,49), fac(hex(8c)),   currdesc$(1)         , ch(32),~
                                                                         ~
               at (14,02), "Freight Charges ",                           ~
               at (14,30), fac(lfac$( 9)), frtamt$              , ch(10),~
               at (14,49), fac(hex(8c)),   currdesc$(2)         , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L41360
                gosub call_customer_credit
                goto L41120

L41360:        if keyhit% <> 13 then L41380
                  call "MANUAL" ("ARIXNPUT")
                  goto L41120

L41380:        if keyhit% <> 15 then L41400
                  call "PRNTSCRN"
                  goto L41120

L41400:         if edit% = 1% then return
                close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf2
        if edit% = 2% then L41480         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0fffffff1a00)
           return

L41480:  if fieldnr% > 0% then L41535     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over    (4)Prev Screen          (12)Delet"&~
                    "e Invoice    (13)Instructions"
           pf$(2) = "(2)Line Items    (5)Next Screen          (25)Manag"&~
                    "e Text       (15)Print Screen"
           pf$(3) = "                                         (25)Manag"&~
                    "mer Credit   (16)End Invoice "
           pfkey$ = hex(0102ff0405ffffffffffff0c0dff0f10ff191d1a00)
           return

                                         /* Edit Mode- Field Enabled   */
L41535:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen. Header Screen Page 3.     *~
            *************************************************************

        deffn'103(fieldnr%, edit%)       /* Header Screen 3            */
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf3
            on fieldnr%       gosub L42120,         /* Sales Region     */~
                                    L42120,         /* Salesmen / Split */~
                                    L42120,         /* PM INV Flag      */~
                                    L42120,         /* Price Code Deflt */~
                                    L42125,         /* Invoice Disc %   */~
                                    L42120,         /* Sales Tax / %    */~
                                    L42120,         /* A/R Type         */~
                                    L42120,         /* Payment Terms    */~
                                    L42120,         /* Net Invoice Acct */~
                                    L42120,         /* Sales Tax Acct   */~
                                    L42120,         /* Freight Account  */~
                                    L42120,         /* Sales Acct Deflt */~
                                    L42120          /* Sales Discs Acct */
            goto L42135
                                    lfac$(fieldnr%) = hex(80) : return
L42120:                             lfac$(fieldnr%) = hex(81) : return
L42125:                             lfac$(fieldnr%) = hex(82) : return

L42135:     accept                                                       ~
               at (01,02), "EXPORT Invoicing",                           ~
               at (01,47), "Heading Page 3",                             ~
               at (01,62), "    Today:",                                 ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Sales Region",                               ~
               at (05,30), fac(lfac$( 1)), region$              , ch(04),~
               at (05,49), fac(hex(8c)),   regiondescr$         , ch(32),~
                                                                         ~
               at (06,02), "Salesman Code / Split %",                    ~
               at (06,30), fac(lfac$( 2)), salesman$(1)         , ch(04),~
               at (06,36), fac(lfac$( 2)), comm$(1)             , ch(03),~
               at (06,49), fac(hex(8c)),   salesmandescr$(1)    , ch(32),~
               at (07,30), fac(lfac$( 2)), salesman$(2)         , ch(04),~
               at (07,36), fac(lfac$( 2)), comm$(2)             , ch(03),~
               at (07,49), fac(hex(8c)),   salesmandescr$(2)    , ch(32),~
               at (08,30), fac(lfac$( 2)), salesman$(3)         , ch(04),~
               at (08,36), fac(lfac$( 2)), comm$(3)             , ch(03),~
               at (08,49), fac(hex(8c)),   salesmandescr$(3)    , ch(32),~
                                                                         ~
               at (09,02), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (09,30), fac(hex(8c))  , pm_so$               , ch(01),~
               at (09,33), fac(hex(8c))  , pm_hdr_dsply2$       , ch(07),~
               at (09,42), fac(lfac$( 3)), pm_inv$              , ch(01),~
                                                                         ~
               at (10,02), "Price Code Default",                         ~
               at (10,30), fac(lfac$( 4)), pc$                  , ch(01),~
               at (10,49), fac(hex(8c)),   pcdescr$             , ch(32),~
                                                                         ~
               at (11,02), "Invoice Discount Percent",                   ~
               at (11,30), fac(lfac$( 5)), invdiscpct$          , ch(05),~
                                                                         ~
               at (12,02), "Sales Tax- Code / Percent",                  ~
               at (12,30), fac(lfac$( 6)), taxcode$             , ch(10),~
               at (12,41), fac(lfac$( 6)), taxpct$              , ch(05),~
               at (12,49), fac(hex(8c)),   taxcodedescr$        , ch(32),~
                                                                         ~
               at (13,02), "A/R Type (A/C/E)",                           ~
               at (13,30), fac(lfac$( 7)), artype$              , ch(01),~
               at (13,49), fac(hex(8c)),   artypedescr$         , ch(32),~
                                                                         ~
               at (14,02), "Payment Terms (Code)",                       ~
               at (14,30), fac(lfac$( 8)), terms$               , ch(20),~
               at (14,51), fac(hex(8c))  , termsdescr$          , ch(30),~
                                                                         ~
               at (15,02), "Net Invoice Distr Account",                  ~
               at (15,30), fac(lfac$( 9)), aracct$              , ch(12),~
               at (15,49), fac(hex(8c)),   aracctdescr$         , ch(32),~
                                                                         ~
               at (16,02), "Sales Tax Account",                          ~
               at (16,30), fac(lfac$(10)), taxacct$             , ch(12),~
               at (16,49), fac(hex(8c)),   taxacctdescr$        , ch(32),~
                                                                         ~
               at (17,02), "Freight Account",                            ~
               at (17,30), fac(lfac$(11)), frtacct$             , ch(12),~
               at (17,49), fac(hex(8c)),   frtacctdescr$        , ch(32),~
                                                                         ~
               at (18,02), "Sales Distr Account",                        ~
               at (18,30), fac(lfac$(12)), salesacct$           , ch(12),~
               at (18,49), fac(hex(8c)),   salesacctdescr$      , ch(32),~
                                                                         ~
               at (19,02), "Sales Discounts Account",                    ~
               at (19,30), fac(lfac$(13)), discacct$            , ch(12),~
               at (19,49), fac(hex(8c)),   discacctdescr$       , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L42500
                gosub call_customer_credit
                goto L42135

L42500:        if keyhit% <> 13 then L42520
                  call "MANUAL" ("ARIXNPUT")
                  goto L42135

L42520:        if keyhit% <> 15 then L42540
                  call "PRNTSCRN"
                  goto L42135

L42540:         if edit% = 1% then return
                close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf3
        if edit% = 2% then L42620         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0fffffff1a00)
           return

L42620:  if fieldnr% > 0% then L42675     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over    (4)Prev Screen          (12)Delet"&~
                    "e Invoice    (13)Instructions"
           pf$(2) = "(2)Line Items    (5)Next Screen          (25)Manag"&~
                    "e Text       (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit   (16)End Invoice "
           pfkey$ = hex(0102ff0405ffffffffffff0c0dff0f10ff191d1a00)
           return

                                         /* Edit Mode- Field Enabled   */
L42675:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen. First Line Item Screen.   *~
            *************************************************************

        deffn'104(fieldnr%, edit%)       /* Line Item Screen 1         */
            if fieldnr% <> 15% then L43035
                init(hex(8c)) lfac$()  :  xfac$ = hex(81)
                goto L43050
L43035:     if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% > 0% then lfac2$ = hex(8c) else lfac2$ = hex(84)
            xfac$ = hex(9c)
L43050:     gosub setpf4
            on fieldnr%       gosub L43150,         /* Part Code        */~
                                    L43145,         /* Part Description */~
                                    L43150,         /* Part Category    */~
                                    L43145,         /* Stocking UOM     */~
                                    L43155,         /* Shipd/Lot & Qty  */~
                                    L43155,         /* Order Qty        */~
                                    L43155,         /* Open Qty         */~
                                    L43145,         /* Pricing UOM      */~
                                    L43155,         /* Conversion       */~
                                    L43150,         /* Currency code    */~
                                    L43155,         /* Unit Price       */~
                                    L43155,         /* Line Item Disc   */~
                                    L43155,         /* Line Item Ext    */~
                                    L43150          /* Line Taxable?    */
            if fieldnr% <> 5% then L43140
                lfac2$ = hex(81)                   /* Lot Number       */
                if lot_enable% = 0% then lfac2$ = hex(8c)
L43140:     goto L43165
L43145:                             lfac$(fieldnr%) = hex(80) : return
L43150:                             lfac$(fieldnr%) = hex(81) : return
L43155:                             lfac$(fieldnr%) = hex(82) : return

L43165:     accept                                                       ~
               at (01,02), "EXPORT Invoicing",                           ~
               at (01,47), "Line Item Page 1",                           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Code",                                  ~
               at (06,30), fac(lfac$( 1)), part$(c%)            , ch(25),~
               at (06,62), fac(hex(8c))  , nonstockmsg$(c%)     , ch(16),~
               at (06,56), fac(xfac$),     mfg$(1%),                     ~
                                                                         ~
               at (07,02), "Part Description",                           ~
               at (07,30), fac(lfac$( 2)), descr$(c%)           , ch(32),~
               at (07,68), fac(hex(8c))  , soseqmsg$            , ch(12),~
                                                                         ~
               at (08,02), "Part Category",                              ~
               at (08,30), fac(lfac$( 3)), cat$(c%)             , ch(04),~
               at (08,49), fac(hex(8c)),   catdescr$            , ch(32),~
                                                                         ~
               at (09,02), "Stocking Unit of Measure",                   ~
               at (09,30), fac(lfac$( 4)), stkuom$(c%)          , ch(04),~
               at (09,49), fac(hex(8c)),   stkuomdescr$         , ch(32),~
                                                                         ~
               at (10,02), "Qty Shipped, Lot/Lot Qty",                   ~
               at (10,30), fac(lfac$( 5)), ship$                , ch(10),~
               at (10,41), fac(lfac2$   ), str(lots$(c%,1),,lot%),       ~
               at (10,48), fac(lfac2$   ), lotqty$              , ch(10),~
               at (10,59), fac(hex(8c))  , lotmsg$              , ch(15),~
                                                                         ~
               at (11,02), fac(hex(8c)),   fd_orderqty$         , ch(26),~
               at (11,30), fac(lfac$( 6)), order$               , ch(10),~
                                                                         ~
               at (12,02), "Open Quantity Remaining",                    ~
               at (12,30), fac(lfac$( 7)), openqty$             , ch(10),~
               at (12,49), fac(hex(84)),   completemsg$(c%)     , ch(30),~
                                                                         ~
               at (13,02), "Pricing Unit of Measure",                    ~
               at (13,30), fac(lfac$( 8)), priceuom$(c%)        , ch(04),~
               at (13,49), fac(hex(8c)),   priceuomdescr$       , ch(32),~
                                                                         ~
               at (14,02), "Conversion PPPP to SSSS",                    ~
               at (14,13), fac(hex(8c)), priceuom$(c%)          , ch(04),~
               at (14,21), fac(hex(8c)), stkuom$  (c%)          , ch(04),~
               at (14,30), fac(lfac$( 9)), conv$                , ch(10),~
                                                                         ~
               at (15,02), "Currency code",                              ~
               at (15,30), fac(lfac$(10)), currency$            , ch(04),~
               at (15,49), fac(hex(8c)),   currdesc$(1)         , ch(32),~
                                                                         ~
               at (16,02), "Unit Price @ Pricing UOM",                   ~
               at (16,30), fac(lfac$(11)), price$               , ch(10),~
               at (16,49), "Price @ Stocking UOM: ",                     ~
               at (16,71), fac(hex(8c))  , pricestk$            , ch(10),~
                                                                         ~
               at (17,02), "Line Item Discount %",                       ~
               at (17,30), fac(lfac$(12)), linediscpct$         , ch(05),~
               at (17,49), "Line Discount Amount: ",                     ~
               at (17,71), fac(hex(8c))  , linediscamt$         , ch(10),~
                                                                         ~
               at (18,02), "Line Item Extension",                        ~
               at (18,30), fac(lfac$(13)), lineext$             , ch(10),~
               at (18,49), fac(hex(8c)),   currdesc$(2)         , ch(32),~
                                                                         ~
               at (19,02), "Line Taxable? (Y/N)",                        ~
               at (19,30), fac(lfac$(14)), taxable$(c%)         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 10% then L43560
                    call "BCKPRCSB" (cuscode$, custype$, part$(c%),      ~
                          cat$(c%), pc$, currency$, currtype$, ship(c%), ~
                          #2, #1, #4, #40)
                    goto L43165

L43560:        if keyhit% <> 11% then L43580
                    call "SOSTATUS" (cuscode$, so$, soseq$(c%), 2%, #6,  ~
                          #18, #17, #5, #15, #16, #1, #5, #6, #22, 0%, 0%)

L43580:        if keyhit% <> 13 then L43600
                  call "MANUAL" ("ARIXNPUT")
                  goto L43165

L43600:        if keyhit% <> 15 then L43620
                  call "PRNTSCRN"
                  goto L43165

L43620:        if edit% = 1% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf4
        if edit% = 2% then L43725         /* Input Mode                 */
           pf$(1%) = "(1)Start Over   (22)Cust Pt Xref         (10)Cost"&~
                     "s & Prices    (13)Instructions"
           pf$(2%) = "(2)Restart Line  (4)Previous Field               "&~
                     "              (15)Print Screen"
           pf$(3%) = "                (23)Mfg Prt Xref                 "&~
                     "              (16)Line Summary"
           pfkey$ = hex(0102ff04ffffffffff0affff0dff0f10ffffffffff161700)
           if fieldnr% <> 1% then goto L43691
                str(pf$(1%),42%,18%) = " " : str(pfkey$,10%,1%) = hex(ff)
L43691:    if fieldnr% = 1% then L43695
                str(pf$(3%),64%,16%) = " " : str(pfkey$,16%,1%) = hex(ff)
L43695:    if fieldnr% = 1% and pxref% + cxref% + c_cxref% = 3% then L43699
                str(pf$(1%),17%,16%) = " " : str(pfkey$,22%,1%) = hex(ff)
L43699:    if fieldnr% = 1% and pxref% + mxref% = 2% then L43703
                str(pf$(3%),17%,16%) = " " : str(pfkey$,23%,1%) = hex(ff)
L43703:    if fieldnr% > 2% then L43707
                str(pf$(2%),18%,17%) = " " : str(pfkey$,4%,1%)  = hex(ff)
L43707:    if fieldnr% <> 15% then L43715
                init (" ")  str(pf$(1%),17%,46%), str(pf$(2%),17%,46%)
                init (" ")  str(pf$(3%),1%)
                init (hex(ff))  str(pfkey$,3%,10%), str(pfkey$,16%,8%)
L43715:    return

L43725:  if fieldnr% > 0% then L43790     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over    (5)Next Scrn (10)Prices (11)SO St"&~
                    "atus         (13)Instructions"
           pf$(2) = "                 (8)Set Open Qty = 0     (12)Delet"&~
                    "e Line Item  (15)Print Screen"
           pf$(3) = "                 (9)Header Screen        (25)Manag"&~
                    "e Line Text  (16)Line Summary"
           pfkey$ = hex(0102ffff05ffff08090a0b0c0dff0f10ff191d0012)
           if closeso$ = "Y" or closeso$ = "A" then L43775
                str(pf$(2),18,20) = " " : str(pfkey$,8,1) = hex(ff)
L43775:    return

                                         /* Edit Mode- Field Enabled   */
L43790:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   5                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen- Second Line Item Screen.  *~
            *************************************************************

        deffn'105(fieldnr%, edit%)       /* Line Item Screen 2         */
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf5
            on fieldnr%     gosub   L44150,         /* P.O. Item Number */~
                                    L44160,         /* Project Number   */~
                                    L44160,         /* Sales Distr. Acct*/~
                                    L44160          /* Sales Discs Acct */
            goto L44190
L44150:                             lfac$(fieldnr%) = hex(80) : return
L44160:                             lfac$(fieldnr%) = hex(81) : return
                                    lfac$(fieldnr%) = hex(82) : return

L44190:     accept                                                       ~
               at (01,02), "EXPORT Invoicing",                           ~
               at (01,47), "Line Item Page 2",                           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Part Code",                                  ~
               at (06,30), fac(hex(8c))  , part$(c%)            , ch(25),~
               at (06,62), fac(hex(8c))  , nonstockmsg$(c%)     , ch(16),~
               at (07,02), "Part Description",                           ~
               at (07,30), fac(hex(8c))  , descr$(c%)           , ch(32),~
                                                                         ~
               at (09,02), "P.O. Item Reference",                        ~
               at (09,30), fac(lfac$( 1)), item$(c%)            , ch(03),~
                                                                         ~
               at (10,02), "Project Number",                             ~
               at (10,30), fac(lfac$( 2)), project$(c%)         , ch(08),~
               at (10,49), fac(hex(8c)),   projectdescr$        , ch(30),~
                                                                         ~
               at (11,02), "Sales Distr. Account",                       ~
               at (11,30), fac(lfac$( 3)), sales$(c%)           , ch(12),~
               at (11,49), fac(hex(8c)),   salesdescr$          , ch(32),~
                                                                         ~
               at (12,02), "Sales Discounts Account",                    ~
               at (12,30), fac(lfac$( 4)), discs$(c%)           , ch(12),~
               at (12,49), fac(hex(8c)),   discsdescr$          , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 10% then L44590
                    call "BCKPRCSB" (cuscode$, custype$, part$(c%),      ~
                          cat$(c%), pc$, currency$, currtype$, ship(c%), ~
                          #2, #1, #4, #40)
                    goto L44190

L44590:        if keyhit% <> 13 then L44630
                  call "MANUAL" ("ARIXNPUT")
                  goto L44190

L44630:        if keyhit% <> 15 then L44670
                  call "PRNTSCRN"
                  goto L44190

L44670:           if edit% = 1% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        setpf5
        if edit% = 2% then L44830         /* Input Mode                 */
           pf$(1) = "(1)Start Over                            (10)Costs"&~
                    " & Prices    (13)Instructions"
           pf$(2) = "(2)Restart Line  (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(0102ff04ffffffffff0affff0dff0fffffffff00)
           return

L44830:  if fieldnr% > 0% then L44940     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over    (4)Previous Screen      (10)Costs"&~
                    " & Prices    (13)Instructions"
           pf$(2) = "                                         (12)Delet"&~
                    "e Line Item  (15)Print Screen"
           pf$(3) = "                 (9)Header Screen        (25)Manag"&~
                    "e Line Text  (16)Line Summary"
           pfkey$ = hex(01ffff04ffffffff090aff0c0dff0f10ff191d00)
           return

                                         /* Edit Mode- Field Enabled   */
L44940:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *           D A T A   S A V E   S C R E E N                 *~
            *-----------------------------------------------------------*~
            * Some last decisions to make and data to fill in.          *~
            *************************************************************

        deffn'106(fieldnr%, edit%)       /* Data Save Recap            */
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf6
            on fieldnr%      gosub  L46170,         /* Inv Reason Code  */~
                                    L46170,         /* Post Hny?        */~
                                    L46170,         /* Currency         */~
                                    L46180          /* FC Amount        */

            dfac$ = hex(8c)  /* Inventory on */
            if hnymsg$ = " " then L46200
                dfac$ = hex(84)      /* Inventory off */
                lfac$ (2) = hex(9c)
            goto L46200
                                    lfac$(fieldnr%) = hex(80) : return
L46170:                             lfac$(fieldnr%) = hex(81) : return
L46180:                             lfac$(fieldnr%) = hex(82) : return

L46200:     accept                                                       ~
               at (01,02), "EXPORT Invoicing",                           ~
               at (01,47), "INVOICE SUMMARY",                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Adjustment Reason Code",                     ~
               at (06,30), fac(lfac$( 1)), invrsn$              , ch(09),~
               at (06,49), fac(hex(8c)),   invrsndescr$         , ch(30),~
                                                                         ~
               at (07,02), fac(dfac$),     affect_hny_msg$      , ch(23),~
               at (07,30), fac(lfac$( 2)), posthny$             , ch(01),~
               at (07,49), fac(hex(84)),   hnymsg$              , ch(30),~
                                                                         ~
               at (08,02), "Currency",                                   ~
               at (08,30), fac(lfac$( 3)), currency$            , ch(04),~
               at (08,49), fac(hex(8c)),   currdesc$(1)         , ch(32),~
                                                                         ~
               at (09,02), fac(hex(8c)), fd_fc$,                         ~
               at (09,30), fac(lfac$( 4)), fcamt$               , ch(10),~
               at (09,49), fac(hex(8c)),   currdesc$(2)         , ch(32),~
                                                                         ~
               at (10,02), "Gross Invoice Amount",                       ~
               at (10,30), fac(hex(84)), grossinv,   pic(-##,###,###.00),~
               at (11,02), "Invoice Discounts",                          ~
               at (11,30), fac(hex(84)), invdiscamt, pic(-##,###,###.00),~
               at (12,02), "Freight Amount",                             ~
               at (12,30), fac(hex(84)), frtamt,     pic(-##,###,###.00),~
               at (13,02), "Sales Tax Amount",                           ~
               at (13,30), fac(hex(a4)), taxamt,     pic(-##,###,###.00),~
               at (14,02), "Net Invoice Amount",                         ~
               at (14,30), fac(hex(84)), netinv,     pic(-##,###,###.00),~
                                                                         ~
               at (10,49), "Taxable Amount   =",                         ~
               at (10,70), fac(hex(84)), taxable,       pic(-###,###.00),~
               at (11,49), "Discount Percent =",                         ~
               at (11,70), fac(hex(84)), invdiscpct,    pic(-###,###.00),~
               at (13,49), "Sales Tax Percent=",                         ~
               at (13,70), fac(hex(84)), taxpct,        pic(-###,###.00),~
               at (16,02), "A/R Type   ",                                ~
               at (16,30), fac(hex(84)), artypedescr$,                   ~
                                                                         ~
               at (18,02), "Number of Line Items",                       ~
               at (18,30), fac(hex(84)), lines%,               pic(###0),~
               at (18,49), fac(hex(94)), delemsg$               , ch(30),~
               at (19,02), "Lines Flagged for Delete",                   ~
               at (19,30), fac(hex(84)), deles%,               pic(###0),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L46800
                gosub call_customer_credit
                goto L46200

L46800:        if keyhit% <> 13 then L46830
                  call "MANUAL" ("ARIXNPUT") : goto L46200

L46830:        if keyhit% <> 15 then L46860
                  call "PRNTSCRN" : goto L46200

L46860:         if edit% = 1% then return
                close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf6
        if edit% = 2% then L47020         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffff1a00)
           return

L47020:  if fieldnr% > 0% then L47170     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Line Items     (10)Manage Payment Schedule     "&~
                    "             (15)Print Screen"
           pf$(3) = "(9)Header Screen                         (26)Custo"&~
                    "mer Credit   (16)Save Invoice"
           pfkey$ = hex(0102ffffffffffff090aff0c0dff0f10ffff1a00)
           if delemsg$ <> " " then str(pf$(3),68,12) = "DELETE Invce"
           if terms$ = "DATED" then L47140
                str(pf$(2),19,30) = " "
                str(pfkey$,10 ,1) = hex(ff)
L47140:    return

                                         /* Edit Mode- Field Enabled   */
L47170:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *       L I N E   S U M M A R Y   S C R E E N               *~
            * --------------------------------------------------------- *~
            * Line Item Summary Screen. (Screen #9).                    *~
            *************************************************************

        deffn'119                        /* Line Summary               */
            edit% = 1%
            init (hex(86)) lfac$()
            init (hex(84)) sfac$()
            init(" ") sship$(), sext$()
            for x% = 1% to 15%
                xt% = x% + t% : if seq$(xt%) = " " then L48090
                     if lsts$(xt%) = "D" then lfac$(x%) = hex(8e)
                     if lsts$(xt%) = "D" then sfac$(x%) = hex(8c)
                     call "CONVERT" (ship(xt%)   , 2.2, sship$ (x%))
                     call "CONVERT" (lineext(xt%), 2.2, sext$  (x%))
            next x%
L48090:     gosub set_summary_display
            gosub setpf9
            goto L48155

        deffn'129(fieldnr%)              /* Delete Line Item           */
            fieldnr% = fieldnr% - t%
            edit% = 2%
            init (hex(8c)) lfac$(), sfac$()
            lfac$(fieldnr%) = hex(94)
            sfac$(fieldnr%) = hex(94)
            gosub set_summary_display
            gosub setpf9
            goto L48155

L48155:     accept                                                       ~
               at (01,02), "EXPORT Invoicing",                           ~
               at (01,25), fac(hex(8c)), curr_hdr$              , ch(11),~
               at (01,37), fac(hex(8c)), currency$              , ch(04),~
               at (01,48), "*LINE SUMMARY*",                             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr$(1%, toggle% + 1%) , ch( 3),~
               at (05,06), fac(hex(ac)), hdr$(2%, toggle% + 1%) , ch(25),~
               at (05,32), fac(hex(ac)), hdr$(3%, toggle% + 1%) , ch(25),~
               at (05,58), fac(hex(ac)), hdr$(4%, toggle% + 1%) , ch(10),~
               at (05,69), fac(hex(ac)), hdr$(5%, toggle% + 1%) , ch(10),~
               at (05,80), fac(hex(ac)), hdr$(6%, toggle% + 1%) , ch( 1),~
               at (06,02), fac(hex(80)), seq$(t%+ 1%)           , ch(03),~
                                                                         ~
               at (06,02), fac(lfac$( 1)), seq$      (t%+ 1%)   , ch(03),~
               at (07,02), fac(lfac$( 2)), seq$      (t%+ 2%)   , ch(03),~
               at (08,02), fac(lfac$( 3)), seq$      (t%+ 3%)   , ch(03),~
               at (09,02), fac(lfac$( 4)), seq$      (t%+ 4%)   , ch(03),~
               at (10,02), fac(lfac$( 5)), seq$      (t%+ 5%)   , ch(03),~
               at (11,02), fac(lfac$( 6)), seq$      (t%+ 6%)   , ch(03),~
               at (12,02), fac(lfac$( 7)), seq$      (t%+ 7%)   , ch(03),~
               at (13,02), fac(lfac$( 8)), seq$      (t%+ 8%)   , ch(03),~
               at (14,02), fac(lfac$( 9)), seq$      (t%+ 9%)   , ch(03),~
               at (15,02), fac(lfac$(10)), seq$      (t%+10%)   , ch(03),~
               at (16,02), fac(lfac$(11)), seq$      (t%+11%)   , ch(03),~
               at (17,02), fac(lfac$(12)), seq$      (t%+12%)   , ch(03),~
               at (18,02), fac(lfac$(13)), seq$      (t%+13%)   , ch(03),~
               at (19,02), fac(lfac$(14)), seq$      (t%+14%)   , ch(03),~
               at (20,02), fac(lfac$(15)), seq$      (t%+15%)   , ch(03),~
                                                                         ~
               at (06,06), fac(sfac$( 1)), dsply_part$(t%+ 1%)  , ch(25),~
               at (07,06), fac(sfac$( 2)), dsply_part$(t%+ 2%)  , ch(25),~
               at (08,06), fac(sfac$( 3)), dsply_part$(t%+ 3%)  , ch(25),~
               at (09,06), fac(sfac$( 4)), dsply_part$(t%+ 4%)  , ch(25),~
               at (10,06), fac(sfac$( 5)), dsply_part$(t%+ 5%)  , ch(25),~
               at (11,06), fac(sfac$( 6)), dsply_part$(t%+ 6%)  , ch(25),~
               at (12,06), fac(sfac$( 7)), dsply_part$(t%+ 7%)  , ch(25),~
               at (13,06), fac(sfac$( 8)), dsply_part$(t%+ 8%)  , ch(25),~
               at (14,06), fac(sfac$( 9)), dsply_part$(t%+ 9%)  , ch(25),~
               at (15,06), fac(sfac$(10)), dsply_part$(t%+10%)  , ch(25),~
               at (16,06), fac(sfac$(11)), dsply_part$(t%+11%)  , ch(25),~
               at (17,06), fac(sfac$(12)), dsply_part$(t%+12%)  , ch(25),~
               at (18,06), fac(sfac$(13)), dsply_part$(t%+13%)  , ch(25),~
               at (19,06), fac(sfac$(14)), dsply_part$(t%+14%)  , ch(25),~
               at (20,06), fac(sfac$(15)), dsply_part$(t%+15%)  , ch(25),~
                                                                         ~
               at (06,32), fac(sfac$( 1)), dsply_descr$(t%+ 1%) , ch(25),~
               at (07,32), fac(sfac$( 2)), dsply_descr$(t%+ 2%) , ch(25),~
               at (08,32), fac(sfac$( 3)), dsply_descr$(t%+ 3%) , ch(25),~
               at (09,32), fac(sfac$( 4)), dsply_descr$(t%+ 4%) , ch(25),~
               at (10,32), fac(sfac$( 5)), dsply_descr$(t%+ 5%) , ch(25),~
               at (11,32), fac(sfac$( 6)), dsply_descr$(t%+ 6%) , ch(25),~
               at (12,32), fac(sfac$( 7)), dsply_descr$(t%+ 7%) , ch(25),~
               at (13,32), fac(sfac$( 8)), dsply_descr$(t%+ 8%) , ch(25),~
               at (14,32), fac(sfac$( 9)), dsply_descr$(t%+ 9%) , ch(25),~
               at (15,32), fac(sfac$(10)), dsply_descr$(t%+10%) , ch(25),~
               at (16,32), fac(sfac$(11)), dsply_descr$(t%+11%) , ch(25),~
               at (17,32), fac(sfac$(12)), dsply_descr$(t%+12%) , ch(25),~
               at (18,32), fac(sfac$(13)), dsply_descr$(t%+13%) , ch(25),~
               at (19,32), fac(sfac$(14)), dsply_descr$(t%+14%) , ch(25),~
               at (20,32), fac(sfac$(15)), dsply_descr$(t%+15%) , ch(25),~
                                                                         ~
               at (06,58), fac(sfac$( 1)), sship$    (    1%)   , ch(10),~
               at (07,58), fac(sfac$( 2)), sship$    (    2%)   , ch(10),~
               at (08,58), fac(sfac$( 3)), sship$    (    3%)   , ch(10),~
               at (09,58), fac(sfac$( 4)), sship$    (    4%)   , ch(10),~
               at (10,58), fac(sfac$( 5)), sship$    (    5%)   , ch(10),~
               at (11,58), fac(sfac$( 6)), sship$    (    6%)   , ch(10),~
               at (12,58), fac(sfac$( 7)), sship$    (    7%)   , ch(10),~
               at (13,58), fac(sfac$( 8)), sship$    (    8%)   , ch(10),~
               at (14,58), fac(sfac$( 9)), sship$    (    9%)   , ch(10),~
               at (15,58), fac(sfac$(10)), sship$    (   10%)   , ch(10),~
               at (16,58), fac(sfac$(11)), sship$    (   11%)   , ch(10),~
               at (17,58), fac(sfac$(12)), sship$    (   12%)   , ch(10),~
               at (18,58), fac(sfac$(13)), sship$    (   13%)   , ch(10),~
               at (19,58), fac(sfac$(14)), sship$    (   14%)   , ch(10),~
               at (20,58), fac(sfac$(15)), sship$    (   15%)   , ch(10),~
                                                                         ~
               at (06,69), fac(sfac$( 1)), sext$     (    1%)   , ch(10),~
               at (07,69), fac(sfac$( 2)), sext$     (    2%)   , ch(10),~
               at (08,69), fac(sfac$( 3)), sext$     (    3%)   , ch(10),~
               at (09,69), fac(sfac$( 4)), sext$     (    4%)   , ch(10),~
               at (10,69), fac(sfac$( 5)), sext$     (    5%)   , ch(10),~
               at (11,69), fac(sfac$( 6)), sext$     (    6%)   , ch(10),~
               at (12,69), fac(sfac$( 7)), sext$     (    7%)   , ch(10),~
               at (13,69), fac(sfac$( 8)), sext$     (    8%)   , ch(10),~
               at (14,69), fac(sfac$( 9)), sext$     (    9%)   , ch(10),~
               at (15,69), fac(sfac$(10)), sext$     (   10%)   , ch(10),~
               at (16,69), fac(sfac$(11)), sext$     (   11%)   , ch(10),~
               at (17,69), fac(sfac$(12)), sext$     (   12%)   , ch(10),~
               at (18,69), fac(sfac$(13)), sext$     (   13%)   , ch(10),~
               at (19,69), fac(sfac$(14)), sext$     (   14%)   , ch(10),~
               at (20,69), fac(sfac$(15)), sext$     (   15%)   , ch(10),~
                                                                         ~
               at (06,80), fac(sfac$( 1)), lsts$     (t%+ 1%)   , ch(01),~
               at (07,80), fac(sfac$( 2)), lsts$     (t%+ 2%)   , ch(01),~
               at (08,80), fac(sfac$( 3)), lsts$     (t%+ 3%)   , ch(01),~
               at (09,80), fac(sfac$( 4)), lsts$     (t%+ 4%)   , ch(01),~
               at (10,80), fac(sfac$( 5)), lsts$     (t%+ 5%)   , ch(01),~
               at (11,80), fac(sfac$( 6)), lsts$     (t%+ 6%)   , ch(01),~
               at (12,80), fac(sfac$( 7)), lsts$     (t%+ 7%)   , ch(01),~
               at (13,80), fac(sfac$( 8)), lsts$     (t%+ 8%)   , ch(01),~
               at (14,80), fac(sfac$( 9)), lsts$     (t%+ 9%)   , ch(01),~
               at (15,80), fac(sfac$(10)), lsts$     (t%+10%)   , ch(01),~
               at (16,80), fac(sfac$(11)), lsts$     (t%+11%)   , ch(01),~
               at (17,80), fac(sfac$(12)), lsts$     (t%+12%)   , ch(01),~
               at (18,80), fac(sfac$(13)), lsts$     (t%+13%)   , ch(01),~
               at (19,80), fac(sfac$(14)), lsts$     (t%+14%)   , ch(01),~
               at (20,80), fac(sfac$(15)), lsts$     (t%+15%)   , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$),                                       ~
                     key (keyhit%)

               if keyhit% <> 13% then L48780
                     call "MANUAL" ("ARIXNPUT")
                     goto L48155

L48780:        if keyhit% <> 15% then L48800
                     call "PRNTSCRN"
                     goto L48155

L48800:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

               if keyhit% <> 10% then L48835
                  gosub locations
                  goto L48155

L48835:        if keyhit% <> 22% then L48860
                  toggle% = mod(toggle% + 1%, 2%)
                  gosub set_summary_display
                  goto L48155

L48860:        return

        setpf9
        if edit% = 2% then L48965         /* Display Mode               */
           pf$(1) = "(1)Start Over   (4)Prev    (7)Up (22)Tgl Scrn (10)"&~
                    "Locations    (13)Instructions"
           pf$(2) = "(2)First        (5)Next    (8)Change Qty      (11)"&~
                    "Append Line  (15)Print Screen"
           pf$(3) = "(3)Last         (6)Down    (9)Header          (12)"&~
                    "Delete Line  (16)End Invoice "
           pfkey$ = hex(0102030405060708090a0b0c0dff0f10ffffffffff1600)
           if pxref% + cxref% + c_cxref% = 3% then L48915
                str(pf$(1%),34%,12%) = " " : str(pfkey$,22%,1%) = hex(ff)
L48915:    if t% <> 0% then L48935
              str(pf$(2),1,13), str(pf$(1),17,7), str(pf$(3),17,7) = " "
              str(pfkey$, 2,1), str(pfkey$, 4,1), str(pfkey$, 6,1)       ~
                                                              = hex(ff)
L48935:    if t% + 15% < maxlines% then L48955
              str(pf$(3),1,13), str(pf$(2),17,7), str(pf$(1),28,5%) = " "
              str(pfkey$, 3,1), str(pfkey$, 5,1), str(pfkey$, 7,1)       ~
                                                              = hex(ff)
L48955:    return

L48965:                                            /* Delete Line      */
           pf$(1) = "(1)EXIT Delete                                    "&~
                    "             (13)Instructions"
           pf$(2) = "(16)DELETE Line Item                              "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0f10ffffffff)
           return

        set_summary_display
            if pxref% + cxref% + c_cxref% < 3% then toggle% = 0%
            if toggle% = 1% then L49040
                mat dsply_part$  = part$
                mat dsply_descr$ = descr$
                return
L49040:     mat dsply_part$  = cus_part$
            mat dsply_descr$ = cus_part_descr$
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Customer Code    */~
                                    L50215,         /* Invoice Number   */~
                                    L50355,         /* SO-BOL  Number   */~
                                    L50400,         /* Settlement #     */~
                                    L50455,         /* Ship-to          */~
                                    L50480,         /* Sold-to          */~
                                    L50500,         /* Invoice Date     */~
                                    L50520,         /* Customer PO      */~
                                    L50560,         /* Store Number     */~
                                    L50595          /* Expiration Date  */
                  return

L50100
*        Customer Code                         CUSCODE$
                     return

L50215
*        Invoice Number                        INVNR$
*        First assist user in locating an existing invoice.
            return

L50355
*        Sales Order - BOL                     SO$ - BOL$
            return

L50400
*        Settlement Number                     STLMNT$
            if stlmnt$ = " " then return
            if str(stlmnt$,,8) = invnr$ then stlmnt$ = " "
            if stlmnt$ = " " then return
                if str(stlmnt$,9) = " " then str(stlmnt$,9) = "00"
                readkey$ = str(billxref$,,9) & str(stlmnt$,,10) & "00"
                call "READ100" (#25, readkey$, f1%(25))
                if f1%(25) = 1% then return
                     errormsg$ = "Settlement does not exist for Bill-to"
                     return

L50455
*        Ship-to                               SHIPTO$
            if str(shipto$()) <> " " then return
                errormsg$ = "Ship-to may not be blank"
                return

L50480
*        Sold-to                               SOLDTO$
            if soldto$(1) = " " then str(soldto$()) = " "
            return

L50500
*        Invoice Date                          INVDATE$
            call "DATEOK" (invdate$, u3%, errormsg$)
            return

L50520
*        Customer PO Number                    PO$
            if po$ <> " " then return
                get #01 using L50535, poreqd$
L50535:              FMT POS(1020), CH(1)
                if poreqd$ <> "Y" then return
                     errormsg$ = "PO is required for this Customer."
                     return

L50560
*        Store Code                            STORE$
            storedescr$ = hex(06) & "Please Select Store."
            call "GETCODE" (#12, store$, storedescr$, 0%, 0, f1%(12))
            if f1%(12) = 1% then return
                errormsg$ = "Invalid Store Code."
                return

L50595
*        Expiration Date (Recurring Only)      EXPDATE$
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51180,         /* Date Shipped     */~
                                    L51230,         /* How Ship         */~
                                    L51260,         /* FOB              */~
                                    L51290,         /* Carrier          */~
                                    L51400,         /* Freight Bill     */~
                                    L51430,         /* Shipment Cartons */~
                                    L51520,         /*          Weight  */~
                                    L51680,         /* Currency         */~
                                    L51610          /* Freight Charges  */
                  return

L51180
*        Date Shipped                          SHIPDATE$
            if shipdate$ = " " or shipdate$ = blankdate$ then return
            call "DATEOK" (shipdate$, u3%, errormsg$)
            return

L51230
*        How Ship                              HOWSHIP$
            return

L51260
*        FOB                                   FOB$
            return

L51290
*        Carrier                               CARRIER$
            carrierdescr$ = " "
            if carrier$ = " " then return
                readkey$ = "CARRIERS " & carrier$
                carrierdescr$ = hex(06) & "Select Carrier. PF-16 if none."
                call "PLOWCODE" (#08, readkey$, carrierdescr$, 9%, 0.30, ~
                                                                  f1%(8))
                if f1%(8) = 1% then L51370
                     carrier$, carrierdescr$ = " " : return
L51370:         carrier$ = str(readkey$,10)
                return

L51400
*        Freight Bill                          FRTBILL$
            return

L51430
*        Number of Cartons Shipped             CARTONS$
            if cartons$ = " " then cartons$ = "0"
            convert cartons$ to cartons, data goto L51460 : goto L51470
L51460:         errormsg$ = "Cartons must be a positive number" : return
L51470:     if cartons < 0 then L51460
            call "CONVERT" (cartons, 2.2, cartons$)
            if cartons = 0 then cartons$ = " "
            return

L51520
*        Shipment Weight                       WEIGHT$
            if weight$ = " " then weight$ = "0"
            convert weight$ to weight, data goto L51550 : goto L51560
L51550:         errormsg$ = "Weight must be a positive number" : return
L51560:     if weight < 0 then L51550
            call "CONVERT" (weight, 2.2, weight$)
            if weight = 0 then weight$ = " "
            return

L51610
*        Freight Charges                       FRTAMT$
            if frtamt$ = " " then frtamt$ = "0"
            convert frtamt$ to frtamt, data goto L51640 : goto L51650
L51640:         errormsg$ = "Invalid entry for Freight Charges" : return
L51650:     call "CONVERT" (frtamt, 2.2, frtamt$)
            return

L51680
*        Currency code                       CURRENCY$
            if currency$ = " " then currency$ = statutory$
            if invtype$ = "X"  then enabled% = 0%
            if enabled% = 0% then return
            if currency$ <> statutory$ then L51740
               currdesc$(), convdate$ = " " : conveqv, convunt = 1
               return
L51740:     call "GETCODE" (#40, currency$, currdesc$(1), 0%, 0, f1%(40))
            if f1%(40) <> 0% then L51780
                errormsg$ = "Invalid Currency code.  Try again."
                return
L51780:     convdate$ = " " : conveqv, convunt = 1
            if currency$ = statutory$ then return
            call "DATREVRS" (invdate$, rev_date$, errormsg$)
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#41, currkey$, 5%, f1%(41))
            if f1%(41) <> 0% then L51890
                errormsg$ = "Invalid Currency Code for this transaction."
                return
L51890:     get #41 using L51900, convdate$, conveqv, convunt
L51900:         FMT POS(12), CH(6), 2*PD(14,7)
            gosub set_currency_description
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52110,         /* Region Code      */~
                                    L52160,         /* Salesmen / Split */~
                                    L52820,         /* PM INV Flag      */~
                                    L52270,         /* Price Code Deflt */~
                                    L52315,         /* Order Discount % */~
                                    L52370,         /* Tax Code / Pct   */~
                                    L52470,         /* A/R Type Flag    */~
                                    L52525,         /* Payment Terms    */~
                                    L52605,         /* A/R Account      */~
                                    L52680,         /* Tax Account      */~
                                    L52710,         /* Freight Account  */~
                                    L52740,         /* Sales Account    */~
                                    L52780          /* Sales Discs Acct */
                  return

L52110
*        Region                                REGION$
            regiondescr$ = " " : if region$ = " " then return
                regiondescr$ = hex(06) & "Select Sales Region"
                readkey$ = "REGIONS  " & region$
                call "PLOWCODE" (#08,readkey$,regiondescr$,9%,.3,f1%(8))
                if f1%(8) = 1% then L52145
                     errormsg$ = "Invalid Sales Region Code." : return
L52145:         region$ = str(readkey$,10)
                return

L52160
*        Salesman Code / Split %               SALESMAN$()
            total% = 0%
            for i% = 1% to 3%
                if salesman$(i%) <> " " then L52190
                     salesmandescr$(i%), comm$(i%) = " "
                     comm%(i%) = 0%  :  goto L52245
L52190:         call "GETCODE" (#07, salesman$(i%), salesmandescr$(i%),  ~
                                                       0%, 0.30, f1%(7))
                if f1%(7) = 1% then L52215
                     errormsg$ = "Salesman Code " & salesman$(i%) &      ~
                                 " not on file."  :  return
L52215:         if comm$(i%) = " " then comm$(i%) = "0"
                convert comm$(i%) to comm%(i%), data goto L52230
                goto L52235
L52230:              errormsg$ = "Commission % must be 0 - 100." : return
L52235:         total% = total% + comm%(i%)
                convert comm%(i%) to comm$(i%), pic(##0)
L52245:     next i%
            if total% <= 100% then return
                errormsg$ = "Total Commission Splits can not exceed 100%."
                return

L52270
*        Price Code Default                    PC$
            if (pc$ >= "A" and pc$ <= "Q") or                            ~
               (pc$ >= "0" and pc$ <= "8") then L52295
                errormsg$ = "Valid Price Codes are 'A'-'Q' and '0'-'8'"
                return
L52295:     readkey$ = "PRICECODE" & pc$
            call "DESCRIBE" (#08, readkey$, pcdescr$, 0%, f1%(8))
            return

L52315
*        Invoice Discount Percent              INVDISCPCT$
            invdiscpct = 0
            if invdiscpct$ = " " then invdiscpct$ = "0"
            convert invdiscpct$ to invdiscpct, data goto L52340
            goto L52350
L52340:         errormsg$ = "Invoice Discount must be -25 to +100%"
                return
L52350:     if invdiscpct < -25 or invdiscpct > 100 then L52340
            call "CONVERT" (invdiscpct, 2.2, invdiscpct$)
            return

L52370
*        Sales Tax Code and Tax Percent        TAXCODE$ / TAXPCT$
            taxcodedescr$ = " "  :  if taxcode$ = " " then L52430
                taxcodedescr$ = hex(06) & "Select Tax Code."
                call "GETCODE" (#20, taxcode$, taxcodedescr$, 0%, 0.30,  ~
                                                                 f1%(20))
                if f1%(20) = 1% then L52405
                     errormsg$ = "Tax Code not on file" : return
L52405:         if taxpct$ <> " " then L52430
                     get #20 using L52415, taxpct
L52415:                   FMT XX(40), PD(14,4)
                     call "CONVERT" (taxpct, 2.4, taxpct$)
                     return
L52430:     if taxpct$ = " " then taxpct$ = "0"
            convert taxpct$ to taxpct, data goto L52440 : goto L52450
L52440:         errormsg$ = "Invalid entry for Tax Percent" : return
L52445:         errormsg$ = "Tax Percent Range is 0 - 100"  : return
L52450:     if taxpct < 0 or taxpct > 100 then L52445
            call "CONVERT" (taxpct, 2.4, taxpct$)
            return

L52470
*        A/R Type                              ARTYPE$
            if pos("ACE" = artype$) <> 0% then L52490
                errormsg$ = "A/R Type must be 'A', 'C', or 'E'."
                return
L52490:     if artype$ = "A" then artypedescr$ = "Accounts Receivable"
            if artype$ = "C" then artypedescr$ = "Cash"
            if artype$ = "E" then artypedescr$ = "Expense"
            return

L52525
*        Payment Terms (Code)                  TERMS$
            if stlmnt$ <> " " then terms$ = " "
            if terms$  <> "DATED" then L52575
                termsdescr$ = "Will be specified"
                return
L52575:     if terms$ = " " then L52590
                call "GETCODE" (#21, terms$, termsdescr$, 0%, 0, f1%(21))
                if f1%(21) = 1% then return
L52590:     termsdescr$ = "0% 0 Days, Net 0 Days"
            if stlmnt$ <> " " then termsdescr$ = "Per Item Applied To"
            return

L52605
*        Net Invoice Account                   ARACCT$
        test_ar_acct
            aracctdescr$ = hex(06) & "Select A/R Account"
            call "GETCODE" (#03, aracct$, aracctdescr$, 0%, 0.30, f1%(3))
            if f1%(3) = 1% then L52640
                errormsg$ = "Invalid Net Invoice Distribution Account"
                return
L52640:     get #03 using L52645, accttype$
L52645:         FMT XX(39), CH(1)
            if artype$ = "A" and accttype$ <> "A" then                   ~
                                  errormsg$ = "Must be an Asset Account"
            if artype$ = "C" and accttype$ <> "$" then                   ~
                                  errormsg$ = "Must be a Cash Account"
            return

L52680
*        Sales Tax Account
            taxacctdescr$ = hex(06) & "Select Sales Tax Account"
            call "GETCODE" (#03, taxacct$,taxacctdescr$, 0%, 0.30, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Tax Account" : return

L52710
*        Freight Account
            frtacctdescr$ = hex(06) & "Select Freight Account"
            call "GETCODE" (#03, frtacct$,frtacctdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Freight Account" : return

L52740
*        Sales Account Default                 SALESACCT$
            salesacctdescr$ = hex(06) & "Select Sales Account."
            call "GETCODE" (#03, salesacct$, salesacctdescr$, 0%, 0,     ~
                                                                  f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Account Code"
                return

L52780
*        Sales Discounts Account               DISCACCT$
            discacctdescr$ = hex(06) & "Select Sales Discounts Account."
            call "GETCODE" (#03, discacct$, discacctdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Discounts Account"
                return

L52820
*        Test Data for Precious Metal Surcharge at Invoice Update
            if pos("YN" = pm_inv$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N' for PM INV Surcharge"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4. (1st Line Item).     *~
            *************************************************************

            deffn'154(fieldnr%, edit%)
                  errormsg$ = " "
                  on fieldnr% gosub L53115,         /* Part Code        */~
                                    L53245,         /* Part Description */~
                                    L53260,         /* Part Category    */~
                                    L53295,         /* Stocking UOM     */~
                                    L53360,         /* Shipped, Lot/Qty */~
                                    L53375,         /* Order Qty        */~
                                    L53420,         /* Open Order Qty   */~
                                    L53475,         /* Pricing UOM      */~
                                    L53545,         /* Conversion       */~
                                    L53795,         /* Currency code    */~
                                    L53610,         /* Unit Price       */~
                                    L53670,         /* Line Item Disc   */~
                                    L53740,         /* Line Item Ext    */~
                                    L53760          /* Line Taxable?    */
                  return

L53115
*        Part Code                             PART$()
            if part$(c%) = " " then L53155
                call "READ100" (#04, part$(c%), f1%(4))
                if f1%(4) = 1% then L53190
                     call "HNYGREF" (part$(c%), #24, #04, f1%(4))
                     if f1%(4) = 0% then L53155
                          call "READ100" (#04, part$(c%), f1%(4))
                          if f1%(4) = 1% then L53190
L53155:     call "GETCODE" (#04, part$(c%), " ", 0%, 0, f1%(4))
            if f1%(4) = 1% then L53190
                if part$(c%) <> " " then L53180
                     errormsg$ = "Part Code cannot be left blank"
                     return
L53180:         nonstockmsg$(c%) = "(Non-Stock Part)"
                if pxref% + cxref% + c_cxref% < 3% then L53185
                     cus_part$(c%) = "** No Cross Reference **"
L53185:         return
L53190:     get #04 using L53191, partflag$
L53191:         FMT POS(166), CH(4)
*              IF PXREF% + CXREF% + C_CXREF% < 3% THEN 53200
*                   CUS_PART$(C%) = " "
*                   CALL "PTXREFSB" (2%, "C", CUSCODE$, CUS_PART$(C%),  ~
*                                    STR(CUS_PART_DESCR$(C%),,30%),     ~
*                                    PART$(C%), " ", #4, RET%)
*                   IF RET% = 1% THEN 53200
*                        CUS_PART$(C%) = CUS_PART_DESCR$(C%)
*                        CUS_PART_DESCR$(C%) = " "
            if partflag$ = " " then L53230
                u3% = 2%
                call "ASKUSER" (u3%, "PART FLAGGED",                     ~
                          "This Part is flagged as '" & partflag$ & "'.",~
                          "Press PF-16 to continue and use Part,",       ~
                          "Or press RETURN to re-enter Part Number.")
                if u3% = 16% then L53230
L53227:            errormsg$ = "Re-enter Part Number"
                   return

L53230:     call "ARIEXTRA" (cuscode$, part$(c%), " ", temp%, #2)
            if temp% <= 0% then return
            if c% + total_e_lines% + temp% <= 100% then L53235
               gosub extra_append_conflict
               if u3% = 0% then L53227
L53235:           e_lines%(c%) = temp%
                  total_e_lines% = total_e_lines% + temp%
                  return

L53245
*        Part Description                      DESCR$()
            return

L53260
*        Part Category                         CAT$()
            catdescr$ = hex(06) & "Select Part Category"
            call "GETCODE" (#11, cat$(c%), catdescr$, 0%, 0, f1%(11))
            if f1%(11) = 1% or cat$(c%) = " " then return
                errormsg$ = "Category Code not on file"
                return

L53295
*        Stocking Unit Of Measure              STKUOM$()
            stkuomdescr$ = " "
            if uom% = 1% then L53325
                if stkuom$(c%) <> " " then return
                     errormsg$ = "Stocking Unit of Measure can not be" & ~
                                 " Blank"  :  return
L53325:     stkuomdescr$ = hex(06) & "Select Stocking UOM"
            readkey$ = "UOM      " & stkuom$(c%)
            call "PLOWCODE" (#08,readkey$,stkuomdescr$,9%,.30,f1%(8))
            if f1%(8) = 1% then stkuom$(c%) = str(readkey$,10)           ~
                           else errormsg$   = "Stocking UOM not on file"
            return

L53360
*        Qty Shipped / Lot Number & Lot Qty    SHIP$,LOTS$(C%,1),LOTQTY$
            goto L56000

L53375
*        Sales Analysis Quantity               ORDER$
            if order$ = " " then order$ = "0"
            convert order$ to order(c%), data goto L53390 : goto L53395
L53390:         errormsg$ = "Invalid Sales Analysis Quantity" : return
L53395
*          IF ORDER(C%) >= 0 THEN 53405
*              ERRORMSG$ = "Order Quantity cannot be negative" : RETURN
            call "CONVERT" (order(c%), 2.2, order$)
            return

L53420
*        Open Order Quantity                   OPENQTY$
            if openqty$ = " " then openqty$ = "0"
            convert openqty$ to openqty(c%), data goto L53435 : goto L53440
L53435:         errormsg$ = "Invalid Open Order Quantity" : return
L53440:     if openqty(c%) >= 0% then L53455
                errormsg$ = "Open Order Quantity cannot be negative"
                return
L53455:     call "CONVERT" (openqty(c%), 2.2, openqty$)
            if edit% = 2% then gosub pricing_stuff
            return

L53475
*        Pricing Unit of Measure               PRICEUOM$()
            priceuomdescr$ = " "
            if uom% = 1% then L53505
                if priceuom$(c%) <> " " then return
                     errormsg$ = "Pricing Unit of Measure can not be"  & ~
                                 " Blank"  :  return
L53505:     priceuomdescr$ = hex(06) & "Select Pricing UOM"
            readkey$ = "UOM      " & priceuom$(c%)
            call "PLOWCODE" (#08,readkey$,priceuomdescr$,9%,0.30,f1%(8))
            if f1%(8) = 1% then priceuom$(c%) = str(readkey$,10)         ~
                           else errormsg$     = "Pricing UOM not on file"
            return


L53545
*        Conversion Pricing to Stocking        CONV$
            if priceuom$(c%) = stkuom$(c%) then conv$ = "1"
            convert conv$ to conv(c%), data goto L53560 : goto L53565
L53560:         errormsg$ = "Invalid Conversion Factor Entry" : return
L53565:     conv(c%) = round(conv(c%), 7)
            if conv(c%) > 0% then L53590
                errormsg$ = "Conversion Factor cannot be equal to or" &  ~
                            " less than zero."
                return
L53590:     call "CONVERT" (conv(c%), 7.7, conv$)
            if edit% = 2% then gosub pricing_stuff
            return

L53610
*        Unit Price                            PRICE$
            if price$ = " " then price$ = "0"
            convert price$ to price(c%), data goto L53625 : goto L53630
L53625:         errormsg$ = "Invalid Unit Price" : return
L53630:     price(c%) = round(price(c%), 4)
            if price(c%) >= 0% then L53650
                errormsg$ = "Unit Price cannot be less than zero."
                return
L53650:     call "CONVERT" (price(c%), 4.4, price$)
            if edit% = 2% then gosub pricing_stuff
            return

L53670
*        Line Item Discount %                  LINEDISCPCT$
            if linediscpct$ = " " then linediscpct$ = "0"
            convert linediscpct$ to linediscpct(c%), data goto L53690
            goto L53695
L53690:         errormsg$ = "Invalid Discount %" : return
L53695:     linediscpct(c%) = round(linediscpct(c%), 2)
            if linediscpct(c%) >= -50% and                               ~
               linediscpct(c%) <= 100 then L53720
                errormsg$ = "Discount must be between -50 and 100%."
                return
L53720:     call "CONVERT" (linediscpct(c%), 2.2, linediscpct$)
            gosub pricing_stuff
            return

L53740
*        Line Item Extension                   LINEEXT$
*        No test- field is disabled- left on the screen for customs
            return

L53760
*        Line Taxable? (Y/N)                   TAXABLE$()
            if taxable$(c%) = "Y" or taxable$(c%) = "N" then return
                errormsg$ = "Enter 'Y' or 'N'"
                return

L53795
*        Currency code                       CURRENCY$
            if currency$ = " " then currency$ = statutory$
            if invtype$ = "X" then enabled% = 0%
            if enabled% = 0% then return
            if currency$ <> statutory$ then L53825
               currdesc$(), convdate$ = " " : conveqv, convunt = 1
               return
L53825:     call "GETCODE" (#40, currency$, currdesc$(1), 0%, 0, f1%(40))
            if f1%(40) <> 0% then goto get_currency_info
                errormsg$ = "Invalid Currency code.  Try again." : return
        get_currency_info
            convdate$ = " " : conveqv, convunt = 1
            if currency$ = statutory$ then return
            call "DATREVRS" (invdate$, rev_date$, errormsg$)
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#41, currkey$, 5%, f1%(41))
            if f1%(41) <> 0% then goto L53900
                errormsg$ = "Invalid Currency Code for this transaction."
                return
L53900:     get #41 using L53905, convdate$, conveqv, convunt
L53905:         FMT POS(12), CH(6), 2*PD(14,7)
            gosub set_currency_description
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 5. (2nd Line Item).     *~
            *************************************************************

        deffn'155(fieldnr%)
            errormsg$ = " "
            on fieldnr%       gosub L54140,         /* P.O. Item        */~
                                    L54170,         /* Project Number   */~
                                    L54300,         /* Sales Distr. Acct*/~
                                    L54370          /* Sales Discs Acct */
                  return

L54140
*        P.O. Item                             ITEM$()
            return

L54170
*        Project Number                        PROJECT$()
            projectdescr$ = " " : if project$(c%) = " " then return
            call "GETCODE"(#14, project$(c%), projectdescr$, 0%, 0,      ~
                                                                 f1%(14))
            if f1%(14) = 1% then L54230
                errormsg$ = "Project not on file." : return
L54230:     get #14, using L54240, temp$  /* Close Date */
L54240:         FMT XX(44), CH(06)
            if temp$ = " " or temp$ = blankdate$ or temp$ >= date then return
                call "DATEFMT" (temp$)
                errormsg$ = "Project was closed on " & temp$
                return

L54300
*        Sales Distr. Account                  SALES$()
            salesacctdescr$ = hex(06) & "Select Sales Account."
            call "GETCODE" (#03, sales$(c%), salesdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Account Code"
                return

L54370
*        Sales Discounts Account               DISCS$()
            discsdescr$ = hex(06) & "Select Sales Discounts Account."
            call "GETCODE" (#03, discs$(c%), discsdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Discounts Account"
                return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Data Save Screen.              *~
            *************************************************************


        deffn'156(fieldnr%)
            errormsg$ = " "
            on fieldnr%        gosub     L55150,    /* Inv Reason       */~
                                         L55280,    /* Affect Inventory */~
                                         L55440,    /* Currency         */~
                                         L55330     /* FC Amount        */
            return

L55150
*        Adj Reason Code
          invrsndescr$ = " "
          if invrsn$ = " " then return
            readkey$ = "INVREASON" & invrsn$
            call "PLOWCODE" (#08,readkey$,invrsndescr$,9%,.3,f1%(8))
            if f1%(8) = 1% then invrsn$ = str(readkey$,10) else          ~
                            errormsg$ = "Invalid Invoice Reason Code."
            return

L55280
*        Affect Inventory
            if posthny$ = "Y" or posthny$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L55330
*        Finance Change Amount
            if fcamt$ = " " then fcamt$ = "0"
            convert fcamt$ to fcamt, data goto L55360  :  goto L55380
L55360:         errormsg$ = "Invalid entry for Finance Charge Amount"
                return
L55380:     call "CONVERT" (fcamt, 2.2, fcamt$)
            grossinv, netinv = fcamt
            delemsg$ = " "
            if fcamt = 0 then delemsg$ = "Invoice will be deleted."
            return

L55440
*        Currency code                       CURRENCY$
            if currency$ = " " then currency$ = statutory$
            if invtype$ = "X"  then enabled% = 0%
            if enabled% = 0% then return
            if currency$ <> statutory$ then L55500
               currdesc$(), convdate$ = " " : conveqv, convunt = 1
               return
L55500:     call "GETCODE" (#40, currency$, currdesc$(1), 0%, 0, f1%(40))
            if f1%(40) <> 0% then L55540
                errormsg$ = "Invalid Currency code.  Try again."
                return
L55540:     convdate$ = " " : conveqv, convunt = 1
            if currency$ = statutory$ then return
            call "DATREVRS" (invdate$, rev_date$, " ")
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#41, currkey$, 5%, f1%(41))
            if f1%(41) <> 0% then L55650
                errormsg$ = "Invalid Currency Code for this transaction."
                return
L55650:     get #41 using L55660, convdate$, conveqv, convunt
L55660:         FMT POS(12), CH(6), 2*PD(14,7)
            gosub set_currency_description
            return


L56000: REM *************************************************************~
            * TEST SHIPPED QUANTITY, LOT NUMBER and HANDLE SERIAL NRs.  *~
            * --------------------------------------------------------- *~
            * Test the data elements on the shipped quantity screen     *~
            * line.  I moved it down here because it was getting in the *~
            * way upstairs (had to renumber by twos, etc.!..).          *~
            *************************************************************

            call "SERENABL" (part$(c%), sn_enable%, lc%, #02, #04)

*        Test the Quantity Shipped
            if ship$ = " " then ship$ = "0"
            convert ship$ to ship(c%), data goto L56140 : goto L56150
L56140:         errormsg$ = "Invalid Shipment Quantity" : return
L56150:     call "CONVERT" (ship(c%), 2.2, ship$)
            if ship(c%) >= 0 then L56171
                errormsg$ = "Shipment Quantity must >= Zero." : return
L56171
*        Apply Part Code Min Quantity and Min Increment to Order Quantity
            call "BCKMINSB" (ship(c%), minsoqty, minsoinc, u3%)
            if u3%  = 16% then goto L56178
            if u3% <>  1% then goto L56171
                errormsg$ = hex(00)
                return
L56178
*        Test for allowable overshipment qty
L56179:     call "SHPOVRSB" (part$(c%), seq$(c%), (soopen(c%) +          ~
                  qtyshipped(c%)), (qtyschld(c%) + qtypreinv(c%) +       ~
                  ship(c%) - bolqty(c%)), def_percent(c%), def_unit(c%), ~
                  sys_def_percent, u3%)
            if u3% = 16% then L56190
            if u3% <> 1% then L56179
                errormsg$ = "Please reenter ship quantity."
                return
L56190:     openqty(c%) = max(0, soopen(c%) - ship(c%))
            call "CONVERT" (openqty(c%), 2.2, openqty$)

*          Clear distribution if (1) Shipped is less than or equal to 0
*                                (2) Non-inventory type invoice,  OR
*                                (3) non-lot and non-serial number part

            if ship(c%)   <= 0                      then L56290
            if lot_enable% = 0% and sn_enable% = 0% then L56290
            goto L56380
L56290:         lc% = 1%
                lotqty$ = " "  :  sn_lot1was$(c%) = all(hex(00))
                gosub clear_lot_info /* Clear out lot distr */
                lotqtys(c%,1) = ship(c%)
                if ship(c%) <= 0 then lotqtys(c%,1) = 0
                gosub test_qty_avail
                if errormsg$ <> " " then return
                if edit% = 2% then gosub pricing_stuff
                return


L56380
*        Test First Lot Number and it's distribution quantity
            if lot_enable% > 0% then L56410
                lots$(c%,1%) = " " : lotqty$ = ship$
L56410:     if lotqty$ = " "   then  lotqty$ = ship$
            convert lotqty$ to lotqtys(c%,1),data goto L56430 : goto L56450
L56430:         errormsg$ = "Invalid numeric entry for Lot Quantity"
                return
L56450:     if lotqtys(c%,1) > 0 and lotqtys(c%,1) <= ship(c%) then L56490
                errormsg$ = "Lot Quantity must be greater than Zero" &   ~
                            " and less than or equal to Qty Shipped."
                return
*        Note that we will allow blank lot and lot tracked part. This
*        can't be processed until valid lot is input, but ok for now.
L56490
*          IF LOT_ENABLE% <> 2% OR LOTS$(C%,1) <> " " THEN 56520
*              ERRORMSG$ = "Lot Number required for this Part"
*              RETURN
            if nonstockmsg$(c%) = " " or lots$(c%,1) = " " then L56550
                errormsg$ = "Lots may not be entered for Non-Stock Parts"
                return
*        Here again we don't worry about lot being blank even if part is
*        lot tracked.
L56550
*          IF LOTS$(C%,1) = " " AND LOT_ENABLE% < 2% THEN 56640
            if lots$(c%,1) = " " then L56640
                readkey$ = str(part$(c%)) & str(store$) & lots$(c%,1%)
                call "READ100" (#13, readkey$, f1%(13))
                if f1%(13) <> 0% then L56640
                     lotmsg$ = "Lot not on file"
                     if lot_enable% < 2% then L56640
                        errormsg$ = "Lot Number must be on file."
                        return

L56640:     lc% = 1% : gosub test_qty_avail
                       if errormsg$ <> " " then exit_qty_test

*        If the lot number has changed we need to clear any serial
*        numbers for this line on the first lot. LOTDISNey is no help.
            if str(sn_lot1was$(c%),,1) = hex(00) then L56870
            if sn_lot1was$(c%) = lots$(c%,1) or sn_enable%= 0% then L56870
                if sn_index2%(c%,1) <> 0% then                           ~
                     call "SERSTOVR" (sn_index2%(c%,1),"4","2", #32, #33)
                if sn_index2%(c%,1) = 0% then                            ~
                                       sn_index2%(c%,1) = c% * 1000% + 1%
                sn_loc$ = str(store$) & lots$(c%,1)
                sn_trankey$ = str(cuscode$) & str(invnr$) &              ~
                                               str(sn_lineid$(c%)) & "01"
                call "SERSELCT" (part$(c%), sn_loc$, lotqtys(c%,1%),     ~
                                 sn_index2%(c%,1), 25%, "RT",            ~
                                 sn_trankey$, "4", "2", errormsg$,       ~
                                 #02, #04, #32, #33)
                if errormsg$ <> " " then exit_qty_test
                     if lotqtys(c%,1%) <> ship(c%) then L56870
                          lc% = 2% : gosub clear_lot_info
                          goto exit_qty_test

L56870:     if lotqtys(c%,1%) <> ship(c%) then L57040

*        Distribution to a single lot number.  Get Serial Numbers.
            lc% = 2% : gosub clear_lot_info
            if sn_enable% = 0% then exit_qty_test
                if sn_index2%(c%,1) = 0% then sn_index2%(c%,1)= c%*100%
                sn_loc$ = str(store$) & lots$(c%,1)
                sn_trankey$ = str(cuscode$) & str(invnr$) &              ~
                                               str(sn_lineid$(c%)) & "01"
                if invnr$ = " " then str(sn_trankey$,10,8) = userid$ &   ~
                                                             hex(06)
                call "SERSELCT" (part$(c%), sn_loc$, lotqtys(c%,1%),     ~
                                 sn_index2%(c%,1), 25%, "RT",            ~
                                 sn_trankey$, "4", "2", errormsg$,       ~
                                 #02, #04, #32, #33)
                goto exit_qty_test

L57040
*        Multiple Lot Distribution-- get lots of serial numbers.
            u3% = 0% : lot$(), lotqtys$() = " " : mat lotqty = zer
            if sn_enable% > 0% and sn_index2%(c%,1) = 0% then            ~
                                             sn_index2%(c%,1) = c% * 100%
            for l% = 1% to 30%
                if lotqtys(c%,l%) = 0 then L57160
                lot$  (l%) = lots$  (c%,l%)
                lotqty(l%) = lotqtys(c%,l%)
                call "CONVERT" (lotqty(l%), 0.2, lotqtys$(l%))
                sn_index1%(l%) = sn_index2%(c%,l%)
                u3% = l%
            next l%
L57160:     strs$(1) = hex(ff)  :  strs$(2) = store$
            sn_trankey$ = str(cuscode$) & str(invnr$) &                  ~
                                               str(sn_lineid$(c%)) & "01"
            if invnr$= " " then str(sn_trankey$,10,8) = userid$ & hex(06)
            call "LOTDISN"  (part$(c%), strs$(), lot$(), lotqtys$(),     ~
                             errormsg$, 1%, u3%, 30%, ship(c%),          ~
                             sn_index1%(), " ", 0, c%, 25%, "RT",        ~
                             sn_trankey$, " ", "2", "4",                 ~
                             #04, #13, #12, #02, #32, #34, #33)
            for l% = 1% to 30%
                lots$  (c%,l%) = lot$  (l%)
                if lotqtys$(l%) = " " then lotqty(l%) = 0 else           ~
                                       convert lotqtys$(l%) to lotqty(l%)
                lotqtys(c%,l%) = lotqty(l%)
                sn_index2%(c%,l%) = sn_index1%(l%)
            next l%

            if errormsg$ <> " " then exit_qty_test

            for lc% = 1% to 30%
                if lotqtys(c%,lc%) = 0 then L57420
                gosub test_qty_avail
                if errormsg$ = " " then L57420
                   errormsg$ = "Distribution Conflict - Lot:"
                   errormsg$ = errormsg$ & " " & lots$(c%,lc%)
                     goto exit_qty_test
L57420:         next lc%

        exit_qty_test
            call "CONVERT" (lotqtys(c%,1%), 2.2, lotqty$)
            sn_lot1was$(c%) = lots$(c%, 1)
            if edit% = 2% then gosub pricing_stuff
            return

        clear_lot_info
            for l% = lc% to 30%
                lots$(c%,l%) = " " : lotqtys(c%,l%) = 0
                if sn_index2%(c%,l%) = 0% then L57550
                     call "SERSTOVR" (sn_index2%(c%,l%),"4","2",#32,#33)
L57550:     next l%
            return

        test_qty_avail
            call "HNYAVAIL" (#04, #13, part$(c%), store$, lots$(c%,lc%), ~
                                          errormsg$, 9e9, temp1, f1%(13))
            if errormsg$ = " " then return
            temp, bufqty, postqty = 0
            for i% = 1% to c%
                if part$(i%) <> part$(c%) then L57740
                   for j% = 1% to 30%
                       if lots$(i%,j%) <> lots$(c%,lc%) then L57690
                          temp = temp + max(0, lotqtys(i%,j%))
L57690:                if buflot$(i%,j%) <> lots$(c%,lc%) then L57710
                          bufqty = bufqty + max(0, bufqty(i%,j%))
L57710:                if postlot$(i%,j%) <> lots$(c%,lc%) then L57730
                          postqty = postqty + max(0, postqty(i%,j%))
L57730:            next j%
L57740:     next i%
            held   = max(0, bufqty - postqty)
            needed = max(0, temp - postqty)
            temp = needed - held
            if temp <= 0     then errormsg$ = " "
            if temp <= temp1 then errormsg$ = " "
            return


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
            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
