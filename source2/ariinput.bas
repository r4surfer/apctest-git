        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  A   A  R   R    I      I    NN  N  P   P  U   U    T     *~
            *  AAAAA  RRRR     I      I    N N N  PPPP   U   U    T     *~
            *  A   A  R  R     I      I    N  NN  P      U   U    T     *~
            *  A   A  R   R  IIIII  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIINPUT - Invoice Entry.                                 *~
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
            * 06/23/86 ! Original                                 ! ERN *~
            * 02/04/87 ! Enhanced Lot Tracking; Serial Numbers    ! ERN *~
            * 02/16/87 ! 'HNYHOLD' logic added                    ! KAB *~
            * 04/08/87 ! Added S/O chk of ARIBUFFR & Enhanced     !     *~
            *          ! Operation (less keys)                    ! HAL *~
            * 04/16/87 ! Fixed default from customer file.        ! ERN *~
            * 05/28/87 ! Standard Costing Enhancement Project.    ! ERN *~
            * 11/24/87 ! Added Multi-currency, CURMASTR, ARICURCY,! JIM *~
            *          !   CURCONVR.                              !     *~
            * 12/02/87 ! 'Enhanced' Invoice Number retrival       ! ERN *~
            * 02/03/88 ! Added check for CREDIT HOLD 'C' or 'H'   ! JDH *~
            * 03/17/88 ! Added currency rates to be from BCKLNCUR,! JDH *~
            *          !   message on currency effective date     !     *~
            * 04/14/88 ! Changed "CMSMACHK" module to 'ARM'       ! MJB *~
            * 08/04/88 ! HNYMASTR conversion factor to 7 decimals.! JIM *~
            * 10/20/88 ! Fix in PRICING_STUFF Routine,            ! JDH *~
            *          ! Resets SHIPPED_COMPLETE% now.            !     *~
            * 03/01/89 ! Proj 7890206 Min SO Quantity & Increment.! JIM *~
            * 05/26/89 ! Added SHPHDRS (#17) to call of ARINUMIN  ! MLJ *~
            * 05/31/89 ! Currency type no longer modifiable when  ! MLJ *~
            *          !   invoice type = "O" (Sales Order).      !     *~
            * 07/11/89 ! Add hooks for Demand Type for SA, GETSCRN! JDH *~
            *          !  Now gets currency info for type 'F' inv.!     *~
            * 10/19/89 ! MC off Mods, Inv Sum Scr handling mods   ! JDH *~
            * 10/20/89 ! Fixed problem with invoice amts on type F! JDH *~
            *          !  after exiting to header from save screen!     *~
            * 11/02/89 ! Over/Under Ship project                  ! JDH *~
            * 02/28/90 ! Fixed PIC for exchange rate date.  Chngd ! JDH *~
            *          !  handling of 'Ship Complete' when neg.   !     *~
            *          !  lots and serial numbers required.       !     *~
            * 06/01/90 ! Offloaded init code to ARIINPTS, fixed   ! JDH *~
            *          !  error when summary over 85 lines, Added !     *~
            *          !  Allow Dup Inv # flag, Default Cust Curr,!     *~
            *          !  Type 'F' can now edit page 2, Changed   !     *~
            *          !  SHPOVRSB qtys, Corrected checking of    !     *~
            *          !  available inv, Cutover BOLs check for   !     *~
            *          !  serial #s & neg. lots.                  !     *~
            * 06/07/90 ! Added files for location mangmnt. MLJ Mod! JDH *~
            * 09/27/90 ! Allow neg qty on non-stocked parts.      ! JDH *~
            * 06/06/91 ! PRR 11594-Added ARIMASTR to ARINUMIN call! JBK *~
            *          ! PRR 11753 & 11161- Moved tax defaults    !     *~
            *          !  ahead of account defaults               !     *~
            *          ! PRR 11614- Delete ARINUMBR in certain    !     *~
            *          !  case where manual invoice # entered     !     *~
            * 06/25/91 ! Force type 'R' & 'G' Affect Inv? to 'N'. ! JDH *~
            * 03/04/92 ! PRR 12290/1 Add 4th Alt Key to ARIMASTR. ! JDH *~
            *          ! PRR 12319 Fixed transposed GOTO @ 32310. !     *~
            *          ! PRR 11500 Can't Ship Complete if BOLs    !     *~
            *          !   exist.  Too easy to double ship.       !     *~
            *          ! PRR 12119 Fixed Qty Avail test for Ship  !     *~
            *          !   Complete.  Also for Cutover Schld BOLs.!     *~
            * 07/08/93 ! Added Customer/Part# Xref Coding.        ! JBK *~
            * 07/27/93 ! Merged ARIINPTS back into this.  ARIINPTS! JDH *~
            *          !   Mod Block not merged.  See OBSOSUBS lib!     *~
            * 08/17/93 !Support for Extra Line Generation (core,?)! KAB *~
            * 10/12/93 ! PRRs 12496,10041 Firm lock if inv inactiv! JDH *~
            *          ! PRRs 12503,12473 Cr memos and negative   !     *~
            *          !   adjustment invoices can now affect inv.!     *~
            *          !   and only they can have negative qtys.  !     *~
            *          ! PRR 11658  Came along for the ride.      !     *~
            *          ! Adj Reason field enable logic improved.  !     *~
            * 10/22/93 ! Ok, that seems to work. Now neg qty & lots JDH *~
            * 11/23/93 ! Core parts can't negatively effect inv.  ! JDH *~
            * 03/01/94 ! PRR 13024. Honor Auto-close of SO.       ! JDH *~
            * 05/24/94 ! Honor flag whether negs. can affect inv. ! JDH *~
            * 07/06/94 ! Corrected arg in call to BCKSWTCH for SA ! JDH *~
            *          !   Booking of non-SO invoices.            !     *~
            *          ! PRR 13253 Allow edit of SA qty for non-  !     *~
            *          !   stocked parts if SA flag is set to keep!     *~
            *          !   track of non-stocked parts at all.     !     *~
            * 11/16/94 ! Dont iterate Pend Qty on PreInvoiced BOLS! RJH *~
            * 01/04/95 ! Precious Metal Surcharge Flags added to  ! RJH *~
            *          !   3rd Header screen & ARIMASTR File.     !     *~
            * 03/14/95 ! PRR 13187 - Use Xref Part Shadow File to ! RJH *~
            *          !   get or save Xref Parts & descrs.       !     *~
            * 05/25/95 ! PRR 13362 - Use SHPHDRS Store as default ! RJH *~
            *          !   on Invoice if available.               !     *~
            * 07/18/95 ! Corrected CMSLINK call to CORDSPLY.      ! MLJ *~
            * 07/26/95 ! PRR 13477 Fixed subscript error, which   ! JDH *~
            *          !   caused crash when loading SO after a   !     *~
            *          !   startover.                             !     *~
            * 09/27/95 ! Re-Create Common Block for line variables! KAB *~
            *          !   only.  Must be available in ARIEXASB   !     *~
            * 12/14/95 ! Now correctly calcs the remaining open.  ! JDH *~
            * 09/05/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        com /* Common Block Contains ALL Line Item Elements for sub    */~
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
            carrier$6, carrierdescr$30,  /* Carrier Code               */~
            cartons$10,                  /* # of Cartons Shipped       */~
            catdescr$32,                 /*                            */~
            closeso$1,                   /* Allow Closing SO flag      */~
            comm%(3), comm$(3),          /* Commission Split %s        */~
            conv$10,                     /* Conversion Pricing->Stockng*/~
            convdate$6,                  /* Currency conversion Date   */~
            crhold$1,                    /* Order Credit Status        */~
            currdflt$4,                  /* Customer default currency  */~
            curr_hdr_msg$8,              /* Currency Hdr Msg - Line Sum*/~
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
            demtype_dflt$1,              /*                            */~
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
            export$1,                    /* Export Flag                */~
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
            invtype$1,                   /* Type of Invoice            */~
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
            neg_inv$1,                   /* Can Neg. Inv Affect Inv.   */~
            openqty$10,                  /* Open Order Quantity        */~
            order$10,                    /* Original Order Quantity    */~
            partflag$4,                  /* Special/Obsolete Flag      */~
            parttaxable$1,               /* Part Taxable? (Y/N/ )      */~
            pc$1, pcdescr$32,            /* Price Code                 */~
            pf$(3)79, pfkey$32,          /* PF Prompts and Keys        */~
            pgmname$8, procname$8,       /* Name based on Invoice Type */~
            plowkey$50,                  /* Misc Plow Key              */~
            pm_hdr_dsply$30,pm_hdr_dsply2$10, /* Display header var    */~
            pm_inv$1,                    /* Precious Metal INV Flag    */~
            pm_sys_inv$1,                /* Precious Metal INV Flag    */~
            pm_on$1,                     /* Precious Metal ON Flag     */~
            pm_so$1,                     /* Precious Metal SO Flag     */~
            pm_sys_so$1,                 /* Precious Metal SO Flag     */~
            pm_cus_inv$1,                /* PMetal INV Flag (Customer) */~
            pm_cus_so$1,                 /* PMetal SO  Flag (Customer) */~
            po$16,                       /* Purchase Order Number      */~
            poreqd$1,                    /* PO Required?               */~
            postdate$8,                  /* Post Date (from Session)   */~
            posthny$1,                   /* Post Inventory?            */~
            price$10,                    /* Unit Price (Pricing UOM)   */~
            pricestk$10,                 /* Unit Price (Stockng UOM)   */~
            priceuomdescr$32,            /* Pricing Unit of Measure    */~
            projectdescr$30,             /* Project Description        */~
            prtmsg$78,                   /* Print Message              */~
            qty_msg$25,                  /* Shipped or Credited        */~
            readkey$50, readkey1$100,    /* Misc Read Keys             */~
                        readkey2$100,    /*                            */~
            region$4,                    /* Region Code                */~
            regiondescr$32,              /*                            */~
            sa_nonstock$1,               /* SA for non-stocked parts?  */~
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
            * #1  ! CUSTOMER ! Customer Master File                     *~
            * #2  ! SYSFILE2 ! Caelus Management System General Informa *~
            * #3  ! GLMAIN   ! General Ledger Main File                 *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! BCKMASTR ! Backlog master file                      *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #7  ! SLMMASTR ! Salesman master file                     *~
            * #8  ! GENCODES ! General Codes File                       *~
            * #9  ! ARIBUFFR ! Invoice Buffer- Headers                  *~
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
            * #26 ! USERINFO ! User's Posting Dates                     *~
            * #27 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #28 ! LOCATION ! Location Master File                     *~
            * #29 ! CUSPTXRF ! Customer Part Number Cross Reference Fil *~
            * #32 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #33 ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #34 ! SERTIF   ! Serial #'s Trans Image File (Buffer)     *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #41 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #42 ! ARIMSCUR ! Currency-specific ARI Master             *~
            * #43 ! ARILNCUR ! Currency-specific ARI lines              *~
            * #44 ! BCKLNCUR ! Currency-specific BCK lines              *~
            * #45 ! COREXREF ! Core Tracking Cross-Reference file       *~
            * #50 ! USERLCMS ! CMS Program Access Control Use           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #3,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #4,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #5,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #6,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #7,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #8,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #9,  "ARIBUFFR",                                      ~
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

            select #26, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos = 1, keylen =  3

            select #27, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos = 1, keylen = 42,                         ~
                        alt key  1, keypos =  432, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42

            select #28, "LOCATION",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos = 1, keylen = 11,                         ~
                        alt key  1, keypos =    4, keylen =  11

            select #29, "CUSPTXRF",                                      ~
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

            select #45, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            select #50, "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =    4, keylen =  30, dup

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, fs%(01%), f2%(01%),   0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%),   0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%),   0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%),   0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%),   0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%),   0%, rslt$(06%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%),   0%, rslt$(07%))
            call "OPENCHCK" (#08, fs%(08%), f2%(08%),   0%, rslt$(08%))
            call "OPENCHCK" (#09, fs%(09%), f2%(09%), 200%, rslt$(09%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 400%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),   0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),   0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),   0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),   0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 200%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%), 400%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),   0%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%),   0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%), 200%, rslt$(19%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%),   0%, rslt$(20%))
            call "OPENCHCK" (#21, fs%(21%), f2%(21%),   0%, rslt$(21%))
            call "OPENCHCK" (#22, fs%(22%), f2%(22%),   0%, rslt$(22%))
            call "OPENCHCK" (#23, fs%(23%), f2%(23%), 100%, rslt$(23%))
            call "OPENCHCK" (#24, fs%(24%), f2%(24%), 100%, rslt$(24%))
            call "OPENCHCK" (#25, fs%(25%), f2%(25%), 100%, rslt$(25%))
            call "OPENCHCK" (#26, fs%(26%), f2%(26%),   0%, rslt$(26%))
            call "OPENCHCK" (#27, fs%(27%), f2%(27%),   0%, rslt$(27%))
            call "OPENCHCK" (#28, fs%(28%), f2%(28%),   0%, rslt$(28%))
            call "OPENCHCK" (#29, fs%(29%), f2%(29%),   0%, rslt$(29%))


*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            call "READ100" (#02, "SWITCHS.CUR", f1%(2))
               if f1%(2) = 0% then L03970
            get #02 using L03920, curr$, statutory$, currtype$
L03920:         FMT POS(21), CH(1), CH(4), POS(29), CH(1)
            if curr$ <> "Y" then statutory$ = " "
            if curr$ <> "Y" then goto L03970
              call "OPENCHCK" (#40, fs%(40%), f2%(40%),   0%, rslt$(40%))
              call "OPENCHCK" (#41, fs%(41%), f2%(41%),   0%, rslt$(41%))
              call "OPENCHCK" (#42, fs%(42%), f2%(42%), 100%, rslt$(42%))
              call "OPENCHCK" (#43, fs%(43%), f2%(43%), 400%, rslt$(43%))
              call "OPENCHCK" (#44, fs%(44%), f2%(44%),   0%, rslt$(44%))

L03970
*        Check for Cust/Mfg Part Cross Reference
            pxref% = fs%(29%)  :  if pxref% < 0% then pxref% = 0%
            if pxref% <> 1% then L09000
            readkey1$ = "C"
            call "PLOWNEXT" (#29, readkey1$, 1%, cxref%)
            readkey1$ = "M"
            call "PLOWNEXT" (#29, readkey1$, 1%, mxref%)

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date  :  call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#26, userid$, f1%(26%))
            if f1%(26%) = 1% then get #26 using L09076, dfltstr$
L09076:         FMT POS(64), CH(3)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

*        Find out which TYPE of Invoicing is being done
L09130:     call "GETPARM" addr ("I ", "R", "ARIINPUT", " ", "0001",     ~
                                 "FOOBAR", "INVOICE TYPE?   ", 16%,      ~
                                 "K", "INVTYPE ", invtype$,  1%,         ~
                                 0%, 0%, "A")
            qty_msg$ = "Qty Shipped, Lot/Lot Qty"
            on pos("OMDACFRG" = invtype$) goto L09200, L09220, L09230, L09240,   ~
                                               L09250, L09280, L09300, L09340
            goto L09130
L09200:         invtypedescr$    = "ON-ORDER INVOICE"
                     fd_so$      = "Sales Order - BOL"   :  goto L09350
L09220:         invtypedescr$    = "MANUAL INVOICE"      :  goto L09350
L09230:         invtypedescr$    = "DIRECT INVOICE"      :  goto L09350
L09240:         invtypedescr$    = "ADJUSTING INVOICE"   :  goto L09350
L09250:         invtypedescr$    = "CREDIT MEMO"
                     qty_msg$    = "Qty Credited, Lot/Lot Qty"
                     fd_invnr$   = "Credit Memo Number"
                     fd_invdate$ = "Credit Memo Date"    :  goto L09350
L09280:         invtypedescr$    = "FINANCE CHARGE"
                     fd_invdate$ = "Finance Charge Date" :  goto L09350
L09300:         invtypedescr$    = "RECURRING CONTROL"
                     fd_invnr$   = "Recurring Group Code"
                     fd_invdate$ = "Entry Date"
                     fd_expdate$ = "Expiration Date"     :  goto L09350
L09340:         invtypedescr$    = "GENERATED INVOICE"
L09350:         invtypedescr$  = invtypedescr$ & " ENTRY"
                if fd_invnr$   = " " then fd_invnr$   = "Invoice Number"
                if fd_invdate$ = " " or fd_invdate$ = blankdate$ ~
                                   then fd_invdate$ = "Invoice Date"
                if invtype$ = "F" then fd_fc$ = "Finance Charge Amount"
                pgmname$  = "ARIINPT" & invtype$
                procname$ = "PROCARI" & invtype$
                if curr$ = "Y" then curr_hdr_msg$ = "Crncy is"           ~
                               else curr_hdr_msg$ = " "
                askmsg$(4) = "This part MUST be manually entered.  " &   ~
                             "Press any PF key to confirm."

*        Clear any in-process flag for invoicing of a Sales Order.
          if invtype$ <> "O" then L09580
            readkey$ = all(hex(00))  :  str(readkey$,,3) = userid$
            call "READ101" (#19, readkey$, f1%(19)) /* BCKBUFFR */
            if f1%(19) = 0% then L09580
                so$ = key(#19, 2%)
                get #19 using L09490, readkey$
L09490:              FMT XX(10), CH(9)
                if readkey$ = "ARIINPUT" then L09530
                     call "ALLFREE"
                     goto L09580
L09530:         delete #19
                call "ASKUSER" (keyhit%, "RESTART NOTE",                 ~
                       "You were working on the Sales Order Shown Below",~
                       "Press (RETURN) To Continue", so$)

L09580
*        Now get which session to put the invoices in
          if invtype$ = "R" then L09631    /* Not needed for recurrings  */
            u3% = 1%
            call "UPDUSRLG" ("ARIUPDTE", "ARIINPUT", invtypedescr$,      ~
                             "1", session$, u3%, postdate$, " ")
            if u3% <> 0% then L65000

L09631
*        Check if Precious Metal Surcharge is on
            pm_on$, pm_sys_so$, pm_sys_inv$ = "N"
            call "READ100" (#02, "SWITCHS.BCK", f1%(02%))
            if f1%(02%) = 1% then get #02 using L09636, pm_on$,            ~
                                                  pm_sys_so$, pm_sys_inv$
L09636:         FMT POS(60), 3*CH(1)
            pm_hdr_dsply$, pm_hdr_dsply2$ = " "
            if pm_on$ <> "Y" then  L09650
                  pm_hdr_dsply$  = "PM Surcharge @ SO?"
                  pm_hdr_dsply2$ = "@ INV?"

L09650
*        Set some flags and misc variables
            gosub init_enables

            mat redim mfg$(1%)1%

            readkey$ = all(hex(00))      /* GENCODES Directory         */
            str(readkey$, 10) = "UOM"
            call "READ100" (#8, readkey$, uom%)

            call "BCKSWTCH" ("SA ", "BOOKINV ", bookinv$, temp, u3%)
            call "BCKSWTCH" ("SA ", "NONSTOCK", sa_nonstock$, temp, u3%)
            call "BCKSWTCH" ("AR ", "HNYACTVE", hnymsg$ , temp, u3%)
            call "BCKSWTCH" ("AR ", "NEG_INV ", neg_inv$, temp, u3%)
            call "BCKSWTCH" ("BCK", "CLOSESO ", closeso$, temp, u3%)
            call "BCKSWTCH" ("BCK", "OVRSHIP%", temp$, sys_def_percent,  ~
                              u3%)
                if hnymsg$ = "N" then hnymsg$ = "* Inventory Inactive *" ~
                                 else hnymsg$ = " "
            call "BCKSWTCH" ("AR ", "DEMTYPE ", demtype_dflt$, temp, u3%)
            if pos("12" = demtype_dflt$) = 0% then demtype_dflt$ = "1"
            call "BCKSWTCH" ("AR ", "DUP_INV ", dup_inv$, temp, u3%)

            affect_hny_msg$ = "Affect Inventory?"
            if invtype$ = "R" or hnymsg$ <> " " then                     ~
                              affect_hny_msg$ = "Inventory NOT Affected!"
            if invtype$  = "F" then affect_hny_msg$ = " "

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

            call "READ100" (#02, "SWITCHS.COR", core%)
            if core% = 1% then call "OPENCHCK" (#45, fs%(45%), f2%(45%), ~
                                                    f1%(45%), rslt$(45%))


        REM *************************************************************~
            *       I N P U T   M O D E   -  H E A D E R S              *~
            *-----------------------------------------------------------*~
            * Handles normal input for header screens.                  *~
            *************************************************************
        inputmode
            gosub init_for_input

*        Input subroutine to get Customer, Invoice, SO & BOL.
L10045:     call "ARINUMIN" (invtype$, invtypedescr$, fd_invnr$,         ~
                             session$, postdate$, #9, #5, #6, #1, #17,   ~
                             #8, #25, #15, errormsg$, cuscode$, invnr$,  ~
                             so$, bol$, keyhit%)
            if keyhit% = 16% then exit_program
            get #1 using L10069, soldto$(), pm_cus_so$, pm_cus_inv$,      ~
                                           shipto$(), billxref$, temp$,  ~
                custype$, currdflt$
L10069:         FMT XX(39), 6*CH(30), POS(226), 2*CH( 1),                ~
                                      POS(253), 6*CH(30), POS(780),      ~
                    CH(9), POS(793), CH(1), POS(1023), CH(2), POS(1045), ~
                    CH(4)

            gosub set_pm_flags

            readkey1$ = "C" & cuscode$
            call "PLOWNEXT" (#29, readkey1$, 10%, c_cxref%)

            if curr$ <> "Y" then currdflt$ = " " /* Says Invisible Man */
            gosub'051 : gosub'052  /* Screens 1 & 2 Defaults           */
            gosub load_invoice
            if errormsg$  <> " " then L10045
            if invonfile%  =  0% then L10120
                goto edithdr1
L10120:     if pos("OX" = invtype$) = 0% then L10165
                gosub load_sales_order
                if errormsg$ <> " " then L10045
                     errormsg$ = " " : gosub test_ar_acct
                     if errormsg$ = " " then edithdr1
                          fieldnr% = 8%
                          goto so_ar_acct_test_failed   /* In Header 3 */

L10165:     mostin% = 0% : least% = 5%
            if invtype$ = "A" or invtype$ = "C" then least% = 4%
            for fieldnr% = 4% to 10%     /*-----HEADER SCREEN 1--------*/
*              IF FIELDNR% > MOSTIN% THEN GOSUB'051(FIELDNR%)
                gosub'050(1%, fieldnr%, 1%)
                      if enabled% = 0% then L10265
                      if fieldnr% = 10% and invtype$ <> "R" then L10280
L10200:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10260
                         if fieldnr% <= least% then L10260
L10220:                  fieldnr% = max(least%, fieldnr% - 1%)
                         gosub'050(1%, fieldnr%, 1%)
                         if enabled% = 1%     then L10200
                         if fieldnr% = least% then L10200
                         goto L10220
L10260:               if keyhit% <>  0% then       L10200
L10265:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10200
                      mostin% = max(mostin%, fieldnr%)
L10280:     next fieldnr%

            if pos("CFAR" = invtype$) <> 0% then L10470
            mostin% = 0%
            for fieldnr% = 1% to 9%      /*-----HEADER SCREEN 2--------*/
*              IF FIELDNR% > MOSTIN% THEN GOSUB'052(FIELDNR%)
                gosub'050(2%, fieldnr%, 1%) /* Set enables, input msg */
                      if enabled% = 0% then L10445
L10400:         gosub'102(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10440
L10415:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(2%, fieldnr%, 1%)
                         if enabled% = 1% then L10400
                         if fieldnr% = 1% then L10400
                         goto L10415
L10440:               if keyhit% <>  0% then   L10400
L10445:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10400
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

L10470:     mostin% = 0%
            for fieldnr% = 1% to 13%     /*-----HEADER SCREEN 3--------*/
                if fieldnr% > mostin% then gosub'053(fieldnr%)
                gosub'050(3%, fieldnr%, 1%) /* Set enables, input msg */
                      if enabled% = 0% then L10540
L10495:         gosub'103(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10535
L10510:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(3%, fieldnr%, 1%)
                         if enabled% = 1% then L10495
                         if fieldnr% = 1% then L10495
                         goto L10510
L10535:               if keyhit% <>  0% then       L10495
L10540:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10495
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
            if invtype$   = "F"  then edithdr1
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
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then gosub restart_line
                      if keyhit% <>  4% then       L11420
L11370:                  fieldnr% = max(2%, fieldnr% - 1%)
                         gosub'050(4%, fieldnr%, 1%)
                         if enabled% = 1% then L11330
                         if fieldnr% = 2% then L11330
                         goto L11370
L11420:               if keyhit%  = 16% and fieldnr% = 1% then return
                      if keyhit%  = 22% then gosub customer_part
                      if keyhit% <> 23% then L11430
                          gosub manufactures_part
                          if errormsg$ <> " " then L11330
L11430:               if keyhit% <>  0% then       L11330
L11440:         gosub'154(fieldnr%, 1%)
                      if errormsg$ <> " " then L11330
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

            mostin% = 0%
            for fieldnr% = 1% to 5%      /* Second Line Item Screen    */
                if fieldnr% > mostin% then gosub'055(fieldnr%)
                gosub'050(5%, fieldnr%, 1%)
                      if enabled% = 0% then L11650
L11540:         gosub'105(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then gosub restart_line
                      if keyhit% <>  4% then       L11630
L11580:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(5%, fieldnr%, 1%)
                         if enabled% = 1% then L11540
                         if fieldnr% = 1% then L11540
                         goto L11580
L11630:               if keyhit%  = 16% then       exit_program
                      if keyhit% <>  0% then       L11540
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
            if invtype$ = "O" then least% = 4% else least% = 3%
            gosub'050(1%, 0%, 2%)       /* Set input message           */
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       line_summary
                  if keyhit%  =  5% and invtype$ = "F" then edithdr3
                  if keyhit%  =  5% then       edithdr2
                  if keyhit%  = 12% then gosub delete_invoice
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 25% then gosub edit_text_hdr
                  if keyhit%  = 29% then       L12095
                  if keyhit% <>  0% then       edithdr1
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
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12150
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12150
                          lastfieldnr% = fieldnr%
                          goto L12095

        edithdr2
            gosub'050(2%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'102(0%, 2%)
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       line_summary
                  if keyhit%  =  4% then       edithdr1
                  if keyhit%  =  5% then       edithdr3
                  if keyhit%  = 12% then gosub delete_invoice
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 25% then gosub edit_text_hdr
                  if keyhit%  = 29% then       L12250
                  if keyhit% <>  0% then       edithdr2
L12250:     fieldnr% = cursor%(1) - 5%
                if fieldnr% <   1% or   fieldnr% >   9% then edithdr2
                if fieldnr%  = lastfieldnr% then edithdr2
            if keyhit% <> 29% then L12280
                gosub'049(2%, fieldnr%)
                goto edithdr2
L12280:     gosub'050(2%, fieldnr%, 2%)
                  if enabled% = 0% then       edithdr2
L12290:     gosub'102(fieldnr%, 2%)
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12290
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12290
                     lastfieldnr% = fieldnr%
                     goto L12250

        edithdr3
            gosub'050(3%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'103(0%, 2%)
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       line_summary
                  if keyhit%  =  4% and invtype$ = "F" then edithdr1
                  if keyhit%  =  4% then       edithdr2
                  if keyhit%  =  5% then       editvfs
                  if keyhit%  = 12% then gosub delete_invoice
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 25% then gosub edit_text_hdr
                  if keyhit%  = 29% then       L12395
                  if keyhit% <>  0% then       edithdr3
L12395:     fieldnr% = cursor%(1) - 4%
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
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12445
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
              gosub call_screen
              pf8_flag% = 0%
              errormsg$ = " "
              if keyhit% = 1% then gosub startover
              if keyhit% = 2% then t%=0%
              if keyhit% = 3% then t%=maxlines%-15%
              if keyhit% = 4% then t%=t%-12%
              if keyhit% = 5% then t%=t%+12%
              if keyhit% = 6% then t%=t%-1%
              if keyhit% = 7% then t%=t%+1%
               t% = max(0%, min(t%, maxlines%-15%))
              if keyhit% = 9% then       edithdr1
              if keyhit% =11% then       appendlines
              if keyhit% =12% then       L13270
              if keyhit% =16% then       datasave
              if keyhit%<> 0 and keyhit% <> 8% then L13120
L13270:     c% = cursor%(1) - 5%
            if c% < 1% or c% > 15% then L13120
                c% = c% + t%
                if c% < 1% or c% > maxlines% then L13120
            if keyhit%  = 12% then gosub sdelete_line
            if keyhit% <>  0% and keyhit% <> 8% then L13120

*        Modify Line C%
            if lsts$(c%) <> "D" then L13420
                keyhit1% = 2%
                call "ASKUSER" (keyhit1%, "REACTIVATE LINE",             ~
                      "Enter PF-16 to Reactive Deleted Line Item",       ~
                      "- OR - ", "Hit any PF-Key to Return to Display.")
                if keyhit1% <> 16% then L13120
                lsts$(c%) = "R"
L13420:     gosub describe_line
            if keyhit% <> 8% then editline1
               fieldnr% = 5%
               pf8_flag% = 1%
               lastfieldnr% = 0%
               goto L13650

        editline1
            gosub'050(4%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'104(0%, 2%)
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       editline2
                  if keyhit%  =  8% then gosub set_openqty
                  if keyhit%  =  9% then       edithdr1
                  if keyhit%  = 12% then gosub delete_line
                  if keyhit%  = 16% then       line_summary
                  if keyhit%  = 25% then gosub edit_text_line
                  if keyhit%  = 29% then       L13560
                  if keyhit% <> 18% then       L13550
                     if e_lines%(c%) <= 0% then L13544
                        total_e_lines% = total_e_lines% - e_lines%(c%)
L13544:              call "ARIEXTRA" (cuscode$, part$(c%), " ",          ~
                                      e_lines%(c%), #2)
                        total_e_lines% = total_e_lines% + e_lines%(c%)
L13550:           if keyhit% <>  0% then       L13611
L13560:     fieldnr% = cursor%(1) - 5%
                if fieldnr% < 2% or fieldnr% > 14% then L13611
                if fieldnr% <> lastfieldnr% then L13620
L13611:              if pf8_flag% = 1% then line_summary
                     goto editline1
L13620:     if keyhit% <> 29% then L13650
                gosub'049(4%, fieldnr%)
                goto editline1
L13650:     if fieldnr% = 5% then gosub'054(fieldnr%) /*Get MIN QTY/INC*/
            gosub'050(4%, fieldnr%, 2%)
                  if enabled% = 0% then L13611
L13670:     gosub'104(fieldnr%, 2%)
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13670
            gosub'154(fieldnr%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13670
                     lastfieldnr% = fieldnr%
                     goto L13560

        editline2
            gosub'050(5%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'105(0%, 2%)
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editline1
                  if keyhit%  =  9% then       edithdr1
                  if keyhit%  = 12% then gosub delete_line
                  if keyhit%  = 16% then       line_summary
                  if keyhit%  = 25% then gosub edit_text_line
                  if keyhit%  = 29% then       L13870
                  if keyhit% <>  0% then       editline2
L13870:     fieldnr% = cursor%(1) - 8%
                if fieldnr% < 1% or fieldnr% > 5% then editline2
                if fieldnr% = lastfieldnr% then editline2
            if keyhit% <> 29% then L13930
                gosub'049(5%, fieldnr%)
                goto editline2
L13930:     gosub'050(5%, fieldnr%, 2%)
                  if enabled% = 0% then       editline2
L13950:     gosub'105(fieldnr%, 2%)
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13950
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
            orig_ship = ship(c%)  :  orig_ship$ = ship$
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
            call "DESCRIBE" (#8, readkey$, stkuomdescr$, 0%, f1%(8))
            readkey$ = "UOM      " & priceuom$(c%)
            call "DESCRIBE" (#8, readkey$, priceuomdescr$, 0%, f1%(8))
            call "GLUNFMT" (sales$(c%))
            call "DESCRIBE" (#3, sales$(c%), salesdescr$, 0%, u3%)
            call "GLFMT" (sales$(c%))
            call "GLUNFMT" (discs$(c%))
            call "DESCRIBE" (#3, discs$(c%), discsdescr$, 0%, u3%)
            call "GLFMT" (discs$(c%))
            call "LOTENABL" (part$(c%), lot_enable%, lot%, #2, #4)
            if pos("FR" = invtype$) > 0% then lot_enable% = 0%
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
            lotqty$, demtype$(c%), cus_part$(c%), cus_part_descr$(c%),   ~
            orig_ship$ = " "

            order(c%), openqty(c%), ship(c%), conv(c%), price(c%),       ~
            pricestk(c%), linediscpct(c%), linediscamt(c%), lineext(c%), ~
            soorder(c%), soopen(c%), qtyschld(c%), qtyshipped(c%),       ~
            qtypreinv(c%), bolqty(c%), orig_ship   = 0
            for l% = 1% to 30%
                lots$  (c%,l%) = " "
                lotqtys(c%,l%) = 0
                lotqtys_orig(c%,l%) = 0
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
            if ret% = 1% then L16050
                errormsg$ = "No Cross Reference for the Customer's " &   ~
                            "Part."
                return
L16050:     keyhit% = 0%
            cus_part_type$(c%) = "C"
            return

        manufactures_part  /* Get CMS Part from Manufacture Xref File */
            if pxref% + mxref% < 2% then return
            savefield% = fieldnr%
            fieldnr% = 15%
            inpmessage$ = "Enter The Manufacture's Code For This Part " &~
                          "Number."
            errormsg$ = " "
            mat redim  mfg$(1)9
L16150:     gosub'104(fieldnr%,1%)
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then gosub restart_line
                if keyhit% <>  0% then L16150
            if mfg$(1%) = "?" then mfg$(1%) = " "
            mfgdescr$ = hex(06) & "Select Manufacturer's Code"
            readkey$ = "MFG CODES" & mfg$(1%)
            call "PLOWCODE" (#8, readkey$, mfgdescr$, 9%, .3, f1%(8%))
                if f1%(8%) = 1% then L16260
                   errormsg$ = "Invalid Manufacturer's Code"
                   goto L16150
L16260:     mfg$(1%) = str(readkey$,10%)
            cus_part$(c%) = part$(c%)
            call "PTXREFSB" (1%, "M", mfg$(1%), cus_part$(c%),           ~
                             str(cus_part_descr$(c%),,30%), part$(c%),   ~
                             " ", #4, ret%)
            if ret% = 1% then L16330
                if part$(c%) = " " then                                  ~
                   errormsg$ = "Part Number CANNOT Be Blank"  else       ~
                   errormsg$ = "No Cross Ref Part For This Mfg Part"
L16330:     fieldnr% = savefield%
            init (" ")  mfg$()
            mat redim mfg$(1%)1%
            cus_part_type$(c%) = "M"
            return

        call_screen
            call "GETSCRN" ("C", " ", cursor%(), 0%)
            return

        call_customer_credit
            call "ARQCUSCR" (cuscode$)
            return

        pf1315
            if keyhit% <> 13% then L17130
                call "MANUAL" (procname$)
                keyhit% = 15%
                return
L17130:     if keyhit% <> 15% then return
                call "PRNTSCRN"
                return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * 1- Sum up Invoice.                                        *~
            * 2- Get any change data required, manage payment schedule. *~
            * 3- Get final save / delete approval.                      *~
            *************************************************************

        datasave

            if keyhit%  = 12% then L19215  /* Deleting Invoice */

            pm_flags$ = str(pm_on$) & str(pm_so$) & pm_inv$

            call "ARIEXASB" (            /* Relies on COM. . . . . .   */~
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
                      if e_lines%(l%) <= 0% then L19180
                      total_e_lines% = total_e_lines% + e_lines%(l%)
L19180:           next l%
            goto line_summary

L19200
*        FIRST add up the Invoice
*          IF KEYHIT%  = 12% THEN 19215  /* Deleting Invoice */
            if invtype$ = "F" and summary% = 1% then L19700
L19215:     grossinv, taxamt, taxable, netinv = 0
            lines%, deles% = 0%
            inpmessage$, delemsg$ = " "
            if invtype$ = "F" then call "CONVERT" (fcamt, 2.2, fcamt$)
            if invtype$ <> "F" and maxlines% = 0% then                   ~
                                    delemsg$ = "Invoice will be Deleted"
            if maxlines% = 0% then L19500
                for c% = 1% to maxlines%
                     if lsts$(c%) <> "D"  then lines% = lines% + 1%      ~
                                          else deles% = deles% + 1%
                     if lsts$(c%) = "D" then L19285
                          grossinv = grossinv + lineext(c%)
                          if taxable$(c%) = "Y" then                     ~
                                        taxable = taxable + lineext(c%)
L19285:         next c%
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
            if posthny$  = " "            then posthny$ = "Y"
            if hnymsg$  <> " "            then posthny$ = "N"
            if pos("FC" = invtype$) > 0% then posthny$ = "N"
            if summary%  = 1% then L19700
            for fieldnr% = 1% to 4%
                enabled% = 0%
                if fieldnr% = 1% and delemsg$ <> " " then L19615
                if fieldnr% = 2% then L19605 /*Accept default- Affect Inv*/
                if pos("ODMRFG" = invtype$) <> 0% and fieldnr% = 1%      ~
                                         and invonfile% <> 1% then L19605
                if invtype$ <> "F" and fieldnr% > 2% then L19615
                if invtype$  = "F" and fieldnr% = 3% then enabled% = 1%
                if curr$    <> "Y" and fieldnr% = 3% then L19615
L19590:         gosub'106(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then       L19590
L19605:         gosub'156(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L19590
L19615:     next fieldnr%

L19700:     lastfieldnr% = 0%
            summary% = 1%
L19710:     gosub'106(0%, 2%)           /* Display Screen - No Entry   */
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       line_summary
                  if keyhit%  =  9% then       edithdr1
                  if keyhit%  = 10% then gosub manage_payment_schedule
                  if keyhit%  = 16% then       save_data
                  if keyhit% <>  0% then       L19710
L19750:     fieldnr% = cursor%(1) - 5%
                if fieldnr% <  1% or  fieldnr%  > 4%  then L19700
                if hnymsg$ <> " " and fieldnr% = 2% then L19700
                if pos("FR" = invtype$) <> 0% and fieldnr% = 2% then L19700
                if invtype$ = "C" and neg_inv$ <> "Y"                    ~
                                              and fieldnr% = 2% then L19700
                if invtype$ <> "F" and fieldnr% > 2% then L19700
                if curr$    <> "Y" and fieldnr% = 3% then L19700
                if invtype$  = "F" and fieldnr% = 3% then enabled% = 1%
                if fieldnr%  = lastfieldnr% then L19700
L19785:     gosub'106(fieldnr%, 2%)     /* Display & Accept Screen     */
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L19785
            gosub'156(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L19785
                          lastfieldnr% = fieldnr%
                          goto L19750


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
*                ON FIELDNR% GOSUB 20190,         /* Customer Code    */~
*                                  20220,         /* Invoice Number   */~
*                                  20250,         /* SO - BOL         */~
*                                  20280,         /* Settlement #     */~
*                                  20310,         /* Ship-to          */~
*                                  20340,         /* Sold-to          */~
*                                  20370,         /* Invoice Date     */~
*                                  20410,         /* PO Number        */~
*                                  20440,         /* Store Code       */~
*                                  20470          /* Expiration Date  */
*                   RETURN

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

*        Store Number                          STORE$
            if store$ = " " then store$ = dfltstr$
*          RETURN

*        Expiration Date                       EXPDATE$
            return

        REM *************************************************************~
            *            D E F A U L T S   F O R   P A G E   2          *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for second header screen.  Section skipped  *~
            * for on-order invoicing where the defaults come from the   *~
            * Sales Order or the BOL.                                   *~
            *************************************************************

        deffn'052
                if invtype$ = "O" then return
*                ON FIELDNR% GOSUB 21200,         /* Ship Date        */~
*                                  21240,         /* How Ship         */~
*                                  21290,         /* FOB              */~
*                                  21340,         /* Carrier          */~
*                                  21370,         /* Freight Bill     */~
*                                  21400,         /* #-of-Cartons     */~
*                                  21430,         /* Shipment Weight  */~
*                                  21460          /* Freight Charges  */
*                   RETURN

*        Date Shipped                          SHIPDATE$
            shipdate$ = date$
*          RETURN

*        How Ship                              HOWSHIP$
            /* Let's use only one GET here */
            get #1 using L21260, howship$, fob$, currency$
L21260:         FMT XX(562), CH(20), CH(20), POS(1045), CH(4)
*          RETURN

*        FOB                                   FOB$
*          RETURN

*        Carrier                               CARRIER$
*          RETURN

*        Freight Bill                          FRTBILL$
*          RETURN

*        Number of Cartons                     CARTONS$
*          RETURN

*        Weight                                WEIGHT$
*          RETURN

*        Freight Charges                       FRTAMT$
            return

        REM *************************************************************~
            *            D E F A U L T S   F O R   P A G E   3          *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for third header screen.                    *~
            *************************************************************

            deffn'053(fieldnr%)
                  on fieldnr% goto  L22105,         /* Region           */~
                                    L22130,         /* Salesmen / Split */~
                                    L22810,         /* PM INV Flag      */~
                                    L22190,         /* Price Code Deflt */~
                                    L22215,         /* Invoice Disc %   */~
                                    L22250,         /* Sales Tax %/Code */~
                                    L22345,         /* A/R Type         */~
                                    L22365,         /* Payment Terms    */~
                                    L22405,         /* Net Invoice Acct */~
                                         ,         /* Sales Tax Acct   */~
                                         ,         /* Freight Account  */~
                                         ,         /* Sales Acct Deflt */~
                                    L22470          /* Sales Discs Acct */
                     return

L22105
*        Sales Region Code                     REGION$
            get #1 using L22115, region$
L22115:         FMT XX(728), CH(4)
            return

L22130
*        Salesman Code / Split %               SALESMAN$(3)
            get #1 using L22140, salesman$(), comm%()
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
            get #1 using L22200, pc$
L22200:         FMT XX(524), CH(1)
            return

L22215
*        Invoice Discount Percent              INVDISCPCT$
            get #1 using L22225, invdiscpct
L22225:         FMT XX(516), PD(14,4)
            if invtype$ = "F" then invdiscpct = 0
            call "CONVERT" (invdiscpct, 2.2, invdiscpct$)
            return

L22250
*        Sales Tax Code and Sales Tax %        TAXCODE$ & TAXPCT$
        get_tax_defaults
            if invtype$ <> "F" then L22270
                taxcode$ = " " : taxpct = 0 : goto L22325
L22270:     get #1 using L22275, taxcode$
L22275:         FMT POS(1025), CH(10)
            if taxcode$ <> " " then L22305
                readkey$ =  "DEFAULTS.STORE." & str(store$)
                call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) = 1% then get #2 using L22300, taxcode$
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
            get #1 using L22375, terms$
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

*        Sales Tax Account                     TAXACCT$
            return

*        Freight Account                       FRTACCT$
            return

*        Sales Account Default                 SALESACCT$
            return

L22470
*        Sales Discounts Account               DISCACCT$
            return


        get_account_defaults  /* Load up all header accounts  */
*        * * * WARNING, DUPLICATE IN ARINPTS * * * *
            acct% = 4%  :  if artype$ = "C" then acct% = 5%
            gosub get_account_default
            aracct$ = acct$  :  aracctdescr$ = acctdescr$

            acct% = 6%  :  gosub get_account_default
            taxacct$ = acct$  :  taxacctdescr$ = acctdescr$

            acct% = 7%  :  gosub get_account_default
            frtacct$ = acct$  :  frtacctdescr$ = acctdescr$

            if invtype$ = "O" then return

                acct% = 1%
                if invtype$ = "F" then acct% = 8%
                gosub get_account_default
                salesacct$ = acct$  :  salesacctdescr$ = acctdescr$

                acct% = 2%  :  gosub get_account_default
                discacct$ = acct$  :  discacctdescr$ = acctdescr$
                return


        get_account_default
            call "ARMGLGET" (acct%, cuscode$, " ", " ", taxcode$,        ~
                             store$, " ", #2, #1, #4, #11, #20, acct$)
            call "GLUNFMT" (acct$)
            call "DESCRIBE" (#3, acct$, acctdescr$, 0%, f1%(3))
            call "GLFMT" (acct$)
            return

L22810: /* PM INV Flag */

            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for fields on First Line Item Screen.       *~
            * This code section is not executed for SO-BOL lines.       *~
            *************************************************************

        deffn'054(fieldnr%)
            on fieldnr%       goto       ,         /* Part Code        */~
                                    L23135,         /* Part Description */~
                                    L23165,         /* Part Category    */~
                                    L23195,         /* Stocking UOM     */~
                                    L23230,         /* Qty Shipped/Lot  */~
                                    L23255,         /* Order Qty        */~
                                         ,         /* Open Qty         */~
                                    L23290,         /* Pricing UOM      */~
                                    L23325,         /* Conv Price - Stk */~
                                    L23405,         /* Currency code    */~
                                    L23420,         /* Unit Price       */~
                                         ,         /* Line Item Disc   */~
                                         ,         /* Extension        */~
                                    L23500          /* Line Taxable?    */
                     return

*        Part Code                             PART$()
            return

L23135
*        Part Description                      DESCR$()
            if nonstockmsg$(c%) <> " " then return
                get #4 using L23150, descr$(c%)
L23150:              FMT XX(25), CH(32)
                return

L23165
*        Part Category                         CAT$()
            if nonstockmsg$(c%) <> " " then return
                get #4 using L23180, cat$(c%)
L23180:              FMT XX(89), CH(4)
                return

L23195
*        Stocking Unit of Measure                        STKUOM$()
            stkuom$(c%) = "EACH"
            if nonstockmsg$(c%) <> " " then return
                get #4 using L23215, stkuom$(c%)
L23215:              FMT XX(73), CH(4)
                return

L23230
*        Qty Shipped / Lot & Lot Qty           SHIP$ / LOT$()-LOTQTY$()
            minsoqty, minsoinc, def_percent(c%), def_unit(c%) = 0
            if nonstockmsg$(c%) <> " " then L23240
            get #04 using L23235, minsoqty, minsoinc, def_percent(c%),    ~
                                 def_unit(c%)
L23235:         FMT POS(706), 4*PD(14,4)
L23240:     call "LOTENABL" (part$(c%), lot_enable%, lot%, #2, #4)
            if pos("FR" = invtype$) > 0% then lot_enable% = 0%
            return

L23255
*        Order Qty                             ORDER$
            if bookinv$ = "Y" then order$ = ship$
            return

*        Open Order Qty                        OPENQTY$
            return

L23290
*        Pricing Unit of Measure               PRICEUOM$()
            priceuom$(c%) = stkuom$(c%)
            if nonstockmsg$(c%) <> " " then return
                get #4 using L23310, priceuom$(c%)
L23310:              FMT XX(77), CH(4)
                return

L23325
*        Conversion Pricing to Stocking        CONV$
            if nonstockmsg$(c%) <> " " then L23355
                get #4 using L23340, conv(c%)
L23340:              FMT POS(82), PD(14,7)
                call "CONVERT" (conv(c%), 7.7, conv$)
                return
L23355:     if stkuom$(c%) = priceuom$(c%) then conv$ = "1"
            if stkuom$(c%) = priceuom$(c%) then return
                readkey$ = "UOMCONV  " & str(priceuom$(c%)) & "-" &      ~
                                         str(stkuom$  (c%))
                call "READ100" (#8, readkey$, f1%(8))
                if f1%(8) = 0% then return
                     get #8 using L23390, conv$
L23390:                   FMT XX(24), CH(10)
                     return

L23405
*        Currency code                         CURRENCY$
            if curr$ <> "Y" then enabled% = 0%
            if frtamt <> 0  then enabled% = 0%
            if c% <> 1% then enabled% = 0%
            if invtype$ = "O" then enabled% = 0%
            return

L23420
*        Unit Price                            PRICE$
            get #1 using L23430, custype$
L23430:         FMT XX(1022), CH(2)
            call "CPRASSGN" (cuscode$, custype$, part$(c%), cat$(c%),    ~
                             pc$, invdate$, currtype$, currency$, -1,    ~
                             ship(c%), #2, #4, price(c%),                ~
                             linediscpct(c%), errormsg$)
            if errormsg$ <> " " then return
                gosub pricing_stuff
                return

*        Line Item Discount %                  LINEDISCPCT$
            return

*        Line Item Extension                   LINEEXT$
            return

L23500
*        Line Taxable? (Y/N)                   TAXABLE$()
            get #1 using L23510, custaxable$
L23510:         FMT XX(793), CH(1)
            parttaxable$ = " "
            if nonstockmsg$(c%) =" " then get #4 using L23525, parttaxable$
L23525:         FMT XX(126), CH(1)
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
            on fieldnr%    goto          ,         /* PO Item Number   */~
                                         ,         /* Project Number   */~
                                    L24320,         /* Sales Distr. Acct*/~
                                    L24430,         /* Sales Discs Acct */~
                                    L24500          /* Demand Type      */
            return

*        PO Item                               ITEM$()
            return

*        Project Number                        PROJECT$()
            return

L24320
*        Sales Distr. Account                  SALES$()
            call "HNYGLGET" (part$(c%), store$, lot$(c%), sales$(c%), 5%,~
                             #4, #13)
            if sales$(c%) <> " " then call "GLFMT" (sales$(c%))
            if sales$(c%) <> " " then return
                call "ARMGLGET" (1%, cuscode$, part$(c%), cat$(c%), " ", ~
                                 store$, " ", #2, #1, #4, #11, #3,       ~
                                 sales$(c%))
                if sales$(c%) = " " then sales$(c%) = salesacct$
                return

L24430
*        Sales Discounts Account               DISCS$()
            call "ARMGLGET" (2%, cuscode$, part$(c%), cat$(c%), " ",     ~
                             store$, " ", #2, #1, #4, #11, #3,           ~
                             discs$(c%))
            if discs$(c%) = " " then discs$(c%) = discacct$
            return

L24500
*        Demand Type                           DEMTYPE$
            demtype$(c%) = demtype_dflt$
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
            scr%(1, 9) =  9% : set%( 9) =  2%      /* Store Number     */
            scr%(1,10) = 10% : set%(10) =  2%      /* Expiration Date  */

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
            scr%(3, 3) = 51% : set%(51) =  2%      /* PM SO Flag       */
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
            scr%(5, 5) = 50% : set%(50) = 99%      /* Demand Type      */
        REM Next available number is '52'.

*        Over-rides by Invoice Type
            if invtype$  = "A" then set%( 4) =  2% /* Settlement #     */
            if invtype$  = "C" then set%( 4) =  2% /* Settlement #     */
            if invtype$ <> "R" then set%(10) = 99% /* Expiration Date  */
            if invtype$ <> "O" then set%(37) = 99% /* Open Qty         */
            if invtype$ <> "O" then set%( 3) = 99% /* Sales Order-BOL  */
            if invtype$ =  "O" then set%( 9) = 99% /* Store Code       */
            if invtype$ <> "F" then L27406
                                    set%(22) = 99% /* Discount %       */
                                    set%(23) = 99% /* Tax Code and %   */
                                    set%(24) = 99% /* A/R Type         */

L27406:     if pm_on$ =  "Y" then L27410
                                    set%(51) = 99% /* PM SO Flag       */

L27410:     call "ENABLSUB" ("INIT", pgmname$, scr%(), set%(),           ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************
        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then L27830             /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", pgmname$, scr%(), set%(),         ~
                              s%, f%, 0%, 0%)
L27830:     return

        REM *************************************************************~
            *        I N P U T   M E S S A G E S  &  E N A B L E S      *~
            * --------------------------------------------------------- *~
            * Sets Enable Flag, Input Message, and Standard PF Keys.    *~
            *************************************************************
        deffn'050(s%, f%, edit%)  /* EDIT%: 1=Input Mode; 2=Edit Mode */
*        Required: S%, F%, EDIT%
            if f% <> 0% then L28080
                inpmessage$ = edtmessage$
                goto L28220

L28080
*        First Define the Input Message
            r% = scr%(s%, f%)  /* Get sequential field number          */
            restore line = L28280, r%     /* Position for Read          */
            read inpmessage$             /* Read Input Message         */

*        Now set the Field Enable Flag (and over-ride if so required)
            call "ENABLSUB" ("SET", pgmname$, scr%(), set%(), s%, f%,    ~
                             edit%, enabled%)
          if s% <> 2% then L28145
            if f% <> 8% then L28145
            if maxlines% > 0% or curr$ <> "Y" then enabled% = 0%
            if enabled% = 1% and currency$ = " "                         ~
                                then currency$ = currdflt$
L28145:   if s% <>  4% then L28205
            if f% <> 10% then L28175
            if c% <> 1% or maxlines% > 1% or curr$ <> "Y" or frtamt <> 0 ~
                  or edit% = 2% then enabled% = 0%
            if enabled% = 1% and currency$ = " "                         ~
                                then currency$ = currdflt$
L28175:     if edit% <> 1% then L28190              /* LINES#1 IN INPUT */
                if nonstockmsg$(c%) = " " and (f% = 4% or f% = 8% or     ~
                                              f% = 9%) then enabled% = 0%
L28190:     if sa_nonstock$ = "Y" then L28195
                if nonstockmsg$(c%) <> " " and f% = 6% then enabled% = 0%
L28195:     if soseq$(c%)       <> " " and f% = 6% then enabled% = 0%
            if soseq$(c%)        = " " and f% = 7% then enabled% = 0%
L28205:   if s% <>  5% then L28225
            if soseq$(c%)       <> " " and f% = 5% then enabled% = 0%

L28220
*        And, finally, set up LINE2$ for the display.
L28225:     line2$ = "Customer: " & cuscode$
            str(line2$,22) = "Invoice: " & invnr$
            if invnr$ = " " and (s% > 1% or f% > 2% or edit% = 2%) then  ~
                       str(line2$,22) = "Invoice: -NEW-"
            if s% = 4% or s% = 5% then str(line2$,41) = "/###"
            if s% = 4% or s% = 5% then                                   ~
                       convert c% to str(line2$,42,3), pic (000)
            str(line2$,46) = "Session: "  & session$
            str(line2$,62) = "ARIINPUT: " & str(cms2v$,,8)
            return

L28280: data                                                             ~
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
         "Enter Transaction Currency Code for this Document. Blank = stat~
        ~utory.",                                                         ~
                                                                         ~
        /* Added to screen 2                                           */~
         "Enter Transaction Currency Code for this Document. Blank = stat~
        ~utory.",                                                         ~
                                                                         ~
        /* Added to screen 5                                           */~
         "Enter the Demand Type for Sales Analyis (1=Fcstd, 2=UnFcstd).",~
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
                      currdflt$, currmsg$, effdate$, demtype$(),         ~
                      cus_part$(), cus_part_descr$(), mfg$(),pm_so$,     ~
                      pm_inv$, cus_part_type$()

            init (hex(00)) sn_lot1was$()

            cartons, fcamt, frtamt, grossinv, invdiscamt, invdiscpct,    ~
            netinv, taxamt, taxpct, weight = 0

            conveqv, convunt = 1

            maxlines%, c%, summary%, invonfile%, soonfile%, bolonfile%,  ~
            toggle%, t%, total_e_lines%, edit% = 0%

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
L29445:     goto clear_inprocess          /* Clear In-process Flag */

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
            * is not the check that the invoice number has not already  *~
            * been used.                                                *~
            *************************************************************
        load_invoice
            invonfile%, soonfile%, bolonfile%, maxlines% = 0%
            sn_last_lineid$ = "0000"
            f%  = 9% : if invtype$ = "R" then f% = 15%
            f1% = f% + 1%

*        First let's go for a direct hit in the buffer file
            readkey$ = str(cuscode$) & invnr$
            call "READ100" (#f%, readkey$, invonfile%)
            if invonfile% = 0% then L30125
                if invtype$ = "R" then L30240
                     get #9 using L30095, readkey$  /* Session Number */
L30095:                   FMT POS(2001), CH(6)
                     if readkey$ = session$ then L30240
                          errormsg$ = "Invoice already entered into" &   ~
                                      " Session " & readkey$
                          return

L30125
*        Invoice Does NOT exist- check it out for dups
            if invtype$ = "R" or invnr$ = " " then return
                if invtype$ <> "G" then L30150
                     errormsg$ = "Invoice must exist in buffer"
                     return
L30150:         call "REDALT0" (#23, invnr$, 1%, f1%(23))
                if f1%(23) = 1% then L30180
                     write #23 using L30170, cuscode$, invnr$,            ~
                                                          eod goto L30180
L30170:                   FMT CH(9), CH(8)
                     return
L30180:         get #23, str(readkey$,,9)
                if str(readkey$,,9) <> cuscode$ then L30200
                     errormsg$ = "Number already used for this Customer"
                     return
L30200:         if dup_inv$ = "N" then L30234
                u3% = 2%
                call "ASKUSER" (u3%, "DUPLICATE NUMBER",                 ~
                                "This Invoice Number is already used.",  ~
                                "Enter PF-16 to reuse Invoice Number,",  ~
                                "Any other PF Key to re-enter Number.")
                if u3% = 16% then return
                     errormsg$ = hex(00) : return

L30234:              errormsg$ = "This Invoice Number already used for "&~
                                 str(readkey$,,9) : return

L30240
*        Invoice DOES EXIST.  Load it up.
            get #f% using L30250, temp$    /* Invoice Type     */
L30250:         FMT POS(891), CH(1)
            if temp$ = invtype$ then L30270
                errormsg$ = "Invoice is wrong type (" & temp$ & ")."
                return
L30270:     prtmsg$ = "Loading Invoice..."
            print at(04,02), str(prtmsg$)
            get #f% using L35365, cuscode$, invnr$, po$, so$, bol$,       ~
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
            gosub set_pm_flags
            call "READ100" (#42, key(#f%), f1%(42))
                  if f1%(42) = 0% then L30346
            get #42 using L30336, currency$, frtamt, temp1,               ~
                           convdate$, conveqv, convunt,                  ~
                           termsdue()

L30336:          FMT CH(4), POS(38), PD(14,4), XX(8), PD(14,4), XX(8),   ~
                     CH(6), 2*PD(14,7), 30*PD(14,4)


L30346:     call "TXTFUTIL" (#22, f2%(22), "LOAD", textid$)
            if invtype$ = "F" then fcamt = temp1 else fcamt = 0
            if invtype$ <> "C" then L30365
                frtamt = -frtamt
                for i% = 1% to 30%
                     termsdue(i%) = -termsdue(i%)
                next i%
L30365:     gosub format_header_data

*        Now load up line items
            maxlines%, c% = 0%
            currkey$, readkey$ = str(cuscode$) & str(invnr$) & hex(00)
L30390:     call "PLOWNEXT" (#f1%, readkey$, 17%, f1%(f1%))
               if f1%(f1%) <> 0% then L30396
L30392:           if maxlines% + total_e_lines% <= 100% then return
                  gosub extra_load_conflict
                  return

L30396:         if maxlines% < 100% then L30404
                   gosub severe_load_error
                   goto L30392

L30404:         maxlines%, c% = maxlines% + 1%
                get #f1% using L35655, temp$, temp$, seq$(c%), item$(c%), ~
                     part$(c%), descr$(c%), cat$(c%), order(c%),         ~
                     ship(c%), openqty(c%), pricestk(c%), stkuom$(c%),   ~
                     priceuom$(c%), conv(c%), price(c%), linediscpct(c%),~
                     linediscamt(c%), lineext(c%), taxable$(c%),         ~
                     sales$(c%), discs$(c%), temp$, textidl$(c%),        ~
                     soseq$(c%), lot$(), lotqty(), nonstockmsg$(c%),     ~
                     project$(c%), sn_lineid$(c%), z14, z14,             ~
                     demtype$(c%), z14, e_lines%(c%), temp$


        REM Get transaction amounts from the ARI 'shadow' file (ARICURCY)
            if currency$ = statutory$ then goto L30480 /* Not Stat, tho */
            if curr$ <> "Y" then goto L30480 /* Nor if no multi-currency*/
            call "READ100" (#43, readkey$, f1%(43))
            if f1%(43) = 0% then goto L30480
                get #43 using L30454, pricestk(c%), price(c%),            ~
                                     linediscamt(c%), lineext(c%)
L30454:         FMT POS(25), 4*PD(14,4)
                    if currency$ = " " then                              ~
                                    get #43 using L30460, currency$,      ~
                                            convdate$, conveqv, convunt
L30460:                 FMT CH(4), POS(57), CH(6), 2*PD(14,7)
L30480:         if sn_lineid$(c%) > sn_last_lineid$ then                 ~
                                         sn_last_lineid$ = sn_lineid$(c%)
                sn_lot1was$(c%) = lot$(1)
                if e_lines%(c%) < 0% then L30495
                   call "ARIEXTRA" (cuscode$, part$(c%), " ",            ~
                                    e_lines%(c%), #2)
                   total_e_lines% = total_e_lines% + e_lines%(c%)
L30495:         if soseq$(c%) <> " " then lsts$(c%) = "O"
                f1%(18) = 0%
                if so$ = " " or soseq$(c%) = " " then L30535
                     readkey1$ = str(so$) & soseq$(c%)
                     call "READ100" (#6, readkey1$, f1%(6))
                     if f1%(6) = 1% then get #6 using L30530, soorder(c%),~
                             qtyshipped(c%), soopen(c%), qtyschld(c%),   ~
                             qtypreinv(c%)
L30530:                   FMT XX(92), 4*PD(14,4), XX(8), PD(14,4)
L30535:         call "TXTFUTIL" (#22, f2%(22), "LOAD", textidl$(c%))
                if invtype$ <> "C" then L30570
                     order      (c%) = -order      (c%)
                     ship       (c%) = -ship       (c%)
                     openqty    (c%) = -openqty    (c%)
                     linediscamt(c%) = -linediscamt(c%)
                     lineext    (c%) = -lineext    (c%)
L30570:         for l% = 1% to 30%
                     lots$  (c%, l%) = lot$  (l%)
                     lotqtys(c%, l%) = lotqty(l%)
                     lotqtys_orig(c%, l%) = lotqty(l%)
                     if invtype$ = "C" then                              ~
                                        lotqtys(c%,l%) = -lotqtys(c%,l%)
                     buflot$(c%, l%) = lot$  (l%)
                     bufqty (c%, l%) = lotqty(l%)
                     if pos("RFC" = invtype$) > 0% then L30645
                          sn_index2%(c%,l%) = c% * 100% + l%
                          sn_trankey$ = str(cuscode$) & str(invnr$) &    ~
                                        str(sn_lineid$(c%)) & "##"
                          convert l% to str(sn_trankey$,22,2), pic(00)
                          call "SERLOAD" (sn_index2%(c%,l%), "RT",       ~
                                          sn_trankey$, 25%, " ", " ",    ~
                                          #2, #34, #32, #33, u3%)
L30645:         next l%
                if bol$ = " " then L30705
                   readkey1$ = str(so$) & str(bol$) & soseq$(c%)
                   call "READ100" (#18, readkey1$, f1%(18))
                      if f1%(18) = 0% then L30705
                   bolonfile% = 1%
                   get #18 using L30680, bolqty(c%), lot$(), lotqty(),    ~
                                        completemsg$(c%)
L30680:                FMT POS(32), PD(14,4), 30*CH(6), 30*PD(14,4),     ~
                           POS(476), CH(1)
                for l% = 1% to 30%
                    postlot$(c%, l%) = lot$  (l%)
                    postqty (c%, l%) = lotqty(l%)
                next l%
L30705:         if nonstockmsg$(c%) <> " " then                          ~
                                    nonstockmsg$(c%) = "(Non-Stock Part)"

*              IF PXREF% + CXREF% + C_CXREF% < 3% THEN 30719
                     call "PTUSEDSB" ("R","ARI ", readkey$, seq$(c%),    ~
                                      cus_part$(c%),                     ~
                                      str(cus_part_descr$(c%),,30%),     ~
                                      cus_part_type$(c%), ret%)
                     if ret% = 1% then L30719
                          cus_part$(c%) = "** No Cross Reference **"
                          cus_part_descr$(c%) = " "
L30719:         call "GLFMT"  (sales$(c%))
                call "GLFMT"  (discs$(c%))
                if completemsg$(c%) = "C" then                           ~
                   completemsg$(c%) = "Reported Complete in Shipping"    ~
                   else completemsg$(c%) = " "
                goto L30390

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
            call "DESCRIBE" (#3 , aracct$ , aracctdescr$ , 0%, f1%(3))
            call "GLFMT" (aracct$   )
            call "DESCRIBE" (#3 , taxacct$, taxacctdescr$, 0%, f1%(3))
            call "GLFMT" (taxacct$  )
            call "DESCRIBE" (#3 , frtacct$, frtacctdescr$, 0%, f1%(3))
            call "GLFMT" (frtacct$  )
            call "DESCRIBE" (#3 , salesacct$, salesacctdescr$, 0%, f1%(3))
            call "GLFMT" (salesacct$)
            call "DESCRIBE" (#3,  discacct$, discacctdescr$, 0%, f1%(3))
            call "GLFMT" (discacct$)
            for i% = 1% to 3%
                if salesman$(i%) = " " then L30870
                     call "DESCRIBE" (#7, salesman$(i%),                 ~
                                          salesmandescr$(i%), 0%, f1%(7))
                     convert comm%(i%) to comm$(i%), pic(##0)
L30870:     next i%
            readkey$ = "REGION   " & region$
            call "DESCRIBE" (#8, readkey$, regiondescr$, 0%, f1%(8))
            readkey$ = "PRICECODE" & pc$
            call "DESCRIBE" (#8 , readkey$, pcdescr$   , 0%, f1%(8))
            readkey$ = "INVREASON" & invrsn$
            call "DESCRIBE" (#8, readkey$, invrsndescr$, 0%, f1%(8))
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
            * Gets the Sales Order and Scheduling Information. This     *~
            * Section of code is executed ONLY during Input Mode of a   *~
            * Type "O" invoice.                                         *~
            *************************************************************
        load_sales_order
            shipped_complete%, soonfile%, bolonfile%, maxlines% = 0%

*        First See if the Order is on file
            readkey$ = str(cuscode$) & so$
            call "READ100" (#5, readkey$, soonfile%)
            if soonfile% = 1% then L31080
                errormsg$ = "Sales Order is not on file."  :  return

L31080:     get #5 using L31085, export$, crhold$
L31085:         FMT POS(857), CH(1), POS(875), CH(1)
            if export$ <> "Y" then L31115
                errormsg$ = "You cannot Invoice an EXPORT Order here"
                gosub delete_arinumbr
                return

L31115:     if crhold$ <> "C" then L31160
            u3% = 2%
            call "ASKUSER" (u3%,"**** CANCELLED ORDER ****",             ~
                 "This order has been Cancelled!!",                      ~
                 "You cannot invoice a cancelled order",                 ~
                 "Press Any PF-Key To Start Over")
            gosub delete_arinumbr
            goto inputmode

L31160:     if crhold$ <> "H" then L31205
            u3% = 2%
            call "ASKUSER" (u3%,"**** CREDIT HOLD!! ****",               ~
                 "A Credit Hold Has Been Placed On This Order!!",        ~
                 "Press PF-16 To Continue With Order. ",                 ~
                 "Press Any Other PF-Key To Start Over")
            if u3% = 16% then goto L31210
                gosub delete_arinumbr  :  goto inputmode

L31205
*        Next see if BOL is on file -or- if order is scheduled
L31210:     if bol$ = " " then L31345
                readkey$ = str(cuscode$) & str(so$) & str(bol$)
                call "READ100" (#17, readkey$, bolonfile%)
                if bolonfile% = 1% then L31240
                     gosub delete_arinumbr
                     errormsg$ = "Bill of Lading not on file." : return
L31240:         get #17 using L31250, shipdate$, readkey$,  /*Inv # */    ~
                                     export$
L31250:             FMT POS(184), CH(6), POS(234), CH(8), POS(246), CH(1)
                if export$ <> "Y" then L31275
                    errormsg$ = "You cannot Invoice an EXPORT Order here"
                    gosub delete_arinumbr
                    return
L31275:         if readkey$ = " " then L31300
                     errormsg$ = "BOL already recorded on Invoice " &    ~
                                 readkey$
                     gosub delete_arinumbr
                     return
L31300:         if shipdate$ <> " " and shipdate$ <> blankdate$ then L31515
                     u3% = 2%
                     call "ASKUSER" (u3%, "BOL NOT SHIPPED",             ~
                          "The Bill of Lading has not been pre-invoiced",~
                          "Press PF-16 to continue with invoicing,     ",~
                          "-or- (RETURN) to re-enter SO/BOL numbers." )
                     if u3% = 16% then L31515
                          gosub delete_arinumbr
                          errormsg$ = "BOL not pre-invoiced" : return
L31345:     readkey$ = str(cuscode$) & str(so$) & hex(00)
            call "PLOWNEXT" (#17, readkey$, 25%, bols_exist%)
            if bols_exist% = 0% then L31400
                u3% = 2%
                call "ASKUSER" (u3%, "ORDER SCHEDULED",                  ~
                     "Order is Scheduled for Shipment (BOL(s) exists)",  ~
                     "Press PF-16 to continue invoicing order, -or-",    ~
                     "Press any other PF-KEY to re-enter SO/BOL #s")
                if u3% = 16% then L31400
                     gosub delete_arinumbr  :  errormsg$ = hex(00)
                     return
L31400:     readkey1$ = str(cuscode$) &  hex(00)
L31405:     call "PLOWNEXT" (#9, readkey1$, 9%, f1%(9))
            if f1%(9) = 0 then L31475
                get #9 using L31430, soinbuffr$
                if soinbuffr$ = so$ then L31435
                goto L31405
L31430:         FMT POS(34), CH(16)
L31435:     u3% = 2%
              call "ASKUSER" (u3%, "*** IN PROCESS WARNING ***",         ~
                   "An Invoice is being processed for this Sales Order.",~
                   "  You MAY be duplicating an invoice!  ",             ~
                   "Press PF-8 to cont. invoicing S.O.  -or-" &          ~
                   "  Any Other PF-KEY to Start Over")
              if u3% = 8% then L31475
                 gosub delete_arinumbr  :  goto inputmode
L31475:     shipped_complete% = 0%
            if bols_exist% = 1% then L31515
            u3% = 2%
            call "ASKUSER" (u3%, "SHIP ORDER COMPLETE?",                 ~
                            "Enter PF-16 to ship order complete,",       ~
                            "-ELSE-", "Press any other PF-Key.")
            if u3% = 16% then shipped_complete% = 1%

L31515
*        Now See if order is already in buffer.  If so can't touch
            call "REDALT0" (#19, so$, 2%, f1%(19))
            if f1%(19) = 0% then L31545
L31530:         errormsg$ = "Sales Order is currently being processed."
                gosub delete_arinumbr
                return
L31545:     readkey$ = " "     /* Flag order as in-process   */
            str(readkey$, 1, 3) = userid$
            str(readkey$, 4, 7) = all(hex(00))
            str(readkey$,11, 8) = "ARIINPUT"
            str(readkey$,21,25) = str(cuscode$) & so$
            write #19 using L31580, readkey$, " ", " ", " ", " ",         ~
                                   eod goto L31530
L31580:         FMT CH(45), 3*CH(250), CH(225)


*        Now load up Sales Order and Scheduled Data
            prtmsg$ = "Loading Sales Information..."
            print at(04,02), str(prtmsg$)
            get #5 using L35030, po$, shipto$(), soldto$(), terms$,       ~
                howship$, fob$, salesacct$, discacct$, salesman$(),      ~
                comm%(), region$,  store$, pc$, invdiscpct, crhold$,     ~
                currency$, sotmp$, invtmp$
            if pm_on$ <> "Y" then L31630
            if sotmp$  <> " " then pm_so$  = sotmp$
            if invtmp$ <> " " then pm_inv$ = invtmp$

L31630:     if currency$ = " " then currency$ = statutory$
            gosub get_currency_info
            if bolonfile% = 0% then L31670
                get #17 using L31655, carrier$, howship$, fob$, shipdate$,~
                    frtbill$, cartons, weight, frtamt, textid$
L31655:              FMT XX(37), CH(6), 2*CH(20), XX(100), CH(6), CH(20),~
                         3*PD(14,4), POS(242), CH(4)
                get #17 using L31663, store$/*Allows ship frm diff stores*/
L31663:              FMT POS(29), CH(3)
                call "TXTFUTIL" (#22, f2%(22%), "LOAD", textid$)
L31670:     gosub get_tax_defaults  :  gosub get_account_defaults
                call "GLUNFMT" (aracct$ )
                call "GLUNFMT" (taxacct$)
                call "GLUNFMT" (frtacct$)
            artype$  = "A"
            invdate$ = date
            gosub format_header_data

*        Now Read in line items from BCKLINES
            c%, maxlines% = 0%
            readkey$ = str(so$) & hex(00)
L31725:     call "PLOWNEXT" (#6, readkey$, 16%, f1%(6))
            if f1%(6) = 0% then L32380
                get #6 using L31740, openqty
L31740:              FMT POS(109), PD(14,4)
                if openqty <= 0 then L31725  /* Nothing left to Ship    */

                if maxlines% < 100% then L31755
                   gosub severe_load_error
                   goto L32380
L31755:         c%, maxlines% = c% + 1%
                get #6 using L35195,                                      ~
                     soseq$(c%), item$(c%), part$(c%), descr$(c%),       ~
                     cat$(c%), soorder(c%), qtyshipped(c%), soopen(c%),  ~
                     qtyschld(c%), qtypreinv(c%), pricestk(c%),          ~
                     stkuom$(c%), priceuom$(c%), conv(c%), price(c%),    ~
                     linediscpct(c%), taxable$(c%), sales$(c%),          ~
                     discs$(c%), dfltlot$, project$(c%), demtype$(c%)
        REM Get transaction amounts from the BCK 'shadow' file (BCKCURCY)
            if currency$ = statutory$ then goto L31835 /* Not Stat, tho */
            if curr$ <> "Y" then goto L31835 /* Nor if no multi-currency*/
            call "READ100" (#44, readkey$, f1%(44))
            if f1%(44) <> 0% then get #44 using L31825, pricestk(c%),     ~
                price(c%), convdate$, conveqv, convunt
L31825:         FMT POS(24), 2*PD(14,4), CH(6), 2*PD(14,7)
                     gosub set_currency_description
L31835:         order(c%) = soorder(c%)
                temp% = 0%
                     convert sn_last_lineid$ to temp%, data goto L31850
L31850:              temp% = temp% + 1%
                     convert temp% to sn_lineid$(c%), pic(0000)
                     sn_last_lineid$ = sn_lineid$(c%)
                call "ARIEXTRA" (cuscode$, part$(c%), " ",               ~
                                 e_lines%(c%), #2)
                total_e_lines% = total_e_lines% + e_lines%(c%)
                if shipped_complete% <> 1% then L31975
                     call "SERENABL" (part$(c%), sn_enable%, lc%, #2, #4)
                     if sn_enable% = 0% then L31895
                          askmsg$(2) = "Serial numbered parts cannot be"&~
                                       " automatically shipped complete!"
                          goto L31935
L31895:              ship(c%) = max(0, soopen(c%) - (qtyschld(c%) +      ~
                                                    qtypreinv(c%)))
                     lotqtys(c%,1) = ship(c%)
                     call "HNYAVAIL" (#4, #13, part$(c%), store$,        ~
                                      dfltlot$, errormsg$, ship(c%),     ~
                                      temp1, f1%(13))
                     if errormsg$ = " " then L31970
                          askmsg$(2) = errormsg$
L31935:                   u3% = 0%
                          askmsg$(1) = "'SHIP COMPLETE' CONFLICT"
                          askmsg$(3) = "Part: " & part$(c%)
                          call "ASKUSER" (u3%, askmsg$(1), askmsg$(2),   ~
                                         askmsg$(3), askmsg$(4))
                          ship(c%) = 0
                          goto L31975
L31970:              lots$(c%,1) = dfltlot$
L31975:         lotqtys(c%,1) = ship(c%)
                convert c% to seq$(c%), pic(###)
                lsts$(c%) = "O"
                if bolonfile% = 0% then L32285
                   readkey1$ = str(so$) & str(bol$) & soseq$(c%)
                   call "READ100" (#18, readkey1$, f1%(18))
                   if f1%(18) = 0% then L32285
                     get #18 using L32020, ship(c%), lot$(), lotqty(),    ~
                                          textidl$(c%), completemsg$(c%)
L32020:              FMT XX(31), PD(14,4), 30*CH(6), 30*PD(14,4), CH(4), ~
                         POS(476), CH(1)
                    bolqty(c%) = ship(c%)
                    call "TXTFUTIL" (#22, f2%(22%), "LOAD", textidl$(c%))
                     for l% = 1% to 30%
                       lots$   (c%, l%) = lot$  (l%)
                       buflot$ (c%, l%) = lot$  (l%)
                       postlot$(c%, l%) = lot$  (l%)
                       lotqtys (c%, l%) = lotqty(l%)
                       lotqtys_orig(c%, l%) = lotqty(l%)
                       bufqty  (c%, l%) = lotqty(l%)
                       postqty (c%, l%) = lotqty(l%)
                       if shipdate$ = " " or shipdate$ = blankdate$ then L32185
                          sn_index2%(c%,l%) = c% * 100% + l%
                          sn_trankey$ = str(so$) & str(bol$) & soseq$(c%)
                          convert l% to str(sn_trankey$,23,2), pic(00)
                          call "SERLOAD" (sn_index2%(c%,l%), "RS",       ~
                                          sn_trankey$, 25%, " ", " ",    ~
                                          #2, #34, #32, #33, u3%)
                          if u3% > 0% then L32125
                               sn_index2%(c%,l%) = 0%
                               goto L32185
L32125:                   readkey1$ = "RS" & sn_trankey$
                          readkey2$ = "RT" & str(cuscode$) &             ~
                                      str(invnr$) & str(sn_lineid$(c%)) &~
                                      str(sn_trankey$,23,2)
                          if invnr$ = " " then                           ~
                                  str(readkey2$,12,8) = userid$ & hex(06)
L32155:                   call "PLOWNEXT" (#34, readkey1$, 42%, f1%(34))
                          if f1%(34) = 0% then L32185
                               put #34 using L32170, readkey2$
L32170:                             FMT POS(1), CH(42)
                               write #34
                               goto L32155
L32185:              next l%
                     if shipdate$ <> " " and shipdate$ <> blankdate$ then L32285
                     call "SERENABL" (part$(c%), sn_enable%, lc%, #2, #4)
                     if sn_enable% = 0% then L32220
                          askmsg$(2) = "Serial numbered parts from sche"&~
                                       "duled BOL not recorded yet!"
                          goto L32250
L32220:              lotqtys(c%,1) = ship(c%)
                     call "HNYAVAIL" (#4, #13, part$(c%), store$,        ~
                                      dfltlot$, errormsg$, ship(c%),     ~
                                      temp1, f1%(13))
                     if errormsg$ = " " then L32280
                          askmsg$(2) = errormsg$
L32250:                   u3% = 0%
                          askmsg$(1) = "CUTOVER 'BOL' CONFLICT"
                          askmsg$(3) = "Part: " & part$(c%)
                          call "ASKUSER" (u3%, askmsg$(1), askmsg$(2),   ~
                                         askmsg$(3), askmsg$(4))
                          ship(c%), lotqtys(c%,1) = 0
L32280:                   lots$  (c%,1) = dfltlot$
L32285:         lineext(c%) = round(ship(c%) * price(c%) / conv(c%), 2)
                linediscamt(c%) =                                        ~
                            round(lineext(c%) * linediscpct(c%) * .01, 2)
                linediscamt(c%) = -linediscamt(c%)
                lineext(c%) = lineext(c%) + linediscamt(c%)
                openqty(c%) = max(0, soopen(c%) - ship(c%))
                if closeso$ = "A" and completemsg$(c%) <> " " then       ~
                                                        openqty(c%) = 0
                call "GLFMT"  (sales$(c%))
                call "GLFMT"  (discs$(c%))
                call "READ100" (#4, part$(c%), f1%(4))
                if f1%(4) = 0% then nonstockmsg$(c%) = "(Non-Stock Part)"
                if completemsg$(c%) = "C" then                           ~
                   completemsg$(c%) = "Reported Complete in Shipping"    ~
                   else completemsg$(c%) = " "


*              IF PXREF% + CXREF% + C_CXREF% < 3% THEN 32370
                     call "PTUSEDSB" ("R", "BCK ", so$, soseq$(c%),      ~
                                      cus_part$(c%),                     ~
                                      str(cus_part_descr$(c%),,30%),     ~
                                      cus_part_type$(c%), ret%)
                     if ret% = 1% then L32370
                          cus_part$(c%) = "** No Cross Reference **"
                          cus_part_descr$(c%) = " "

L32370:         goto L31725

L32380
*        Final tests after load
            if maxlines% + total_e_lines% <= 100% then L32384
               gosub extra_load_conflict

L32384
*        Lastly, and leastly, check that default lot info is A-ok fine.
            for c% = 1% to maxlines%
                lot_enable%, sn_enable% = 0%
                if shipdate$ = " " or shipdate$ = blankdate$ or ~
                                     bolonfile% = 0% then L32430
                     readkey1$ = str(so$) & str(bol$) & soseq$(c%)
                     call "READ100" (#18, readkey1$, f1%(18))
                     if f1%(18) = 1% then L32545
                call "SERENABL" (part$(c%), sn_enable%,  lot%, #2, #4)
                     if pos("CFR" = invtype$) > 0% then sn_enable%  = 0%
                     if ship(c%)  = 0 then sn_enable% = 0%
L32430:         call "LOTENABL" (part$(c%), lot_enable%, lot%, #2, #4)
                     if pos("FR" = invtype$) > 0% then lot_enable% = 0%
                     if ship(c%)  = 0 then lot_enable% = 0%
                     if lot_enable% = 0% then lots$(c%,1%) = " "
                if ship(c%) = 0 or                                       ~
                       (lot_enable% < 2% and sn_enable% = 0%) then L32545
                   if lot_enable% = 2% then                              ~
                              errormsg$ = "Please supply Lot Number(s)."
                   if lot_enable% = 2% and lots$(c%,1) = " " then L32505
                     readkey$ = str(part$(c%)) & str(store$) & lots$(c%,1)
                     call "READ100" (#13, readkey$, f1%(13))
                     if f1%(13) = 1% and sn_enable% = 0% then L32545
                          if f1%(13) = 0% then                           ~
                               errormsg$ = "Invalid Lot Number"     else ~
                               errormsg$ = "Serial Numbers Required"
L32505:                   gosub describe_line
                          fieldnr% = 5%
                          gosub'050(4%, fieldnr%, 2%)
L32520:                   gosub'104(fieldnr%, 2%)
                               if keyhit%  =  1 then gosub startover
                               if keyhit% <>  0 then L32520
                          gosub'154(fieldnr%, 2%)
                               if errormsg$ <> " " then L32520
L32545:     next c%
            errormsg$ = " "
            return


        delete_arinumbr   /* Delete ARINUMBR record in some cases */
            if invtype$ <> "O" then return
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

            f%  = 9% : if invtype$ = "R" then f% = 15%
            f1% = f% + 1%

            if invonfile% = 0% then L33115
                readkey$ = str(cuscode$) & invnr$
                call "DELETE" (#f% , readkey$, 17%) /* Header */
                call "DELETE" (#f1%, readkey$, 17%) /* Lines  */
                call "DELETE" (#42, readkey$, 17%) /* Currency */
                call "DELETE" (#43, readkey$, 17%) /* Currency */
                call "PTUSEDSB" ("D", "ARI ", readkey$, "ALL",           ~
                                 " ", " ", " ", ret%)/* Del Xref Shadow */

L33115
*        If the DELETE message is not blank then awway we go with this
*        invoice.  If a BOL is behind all this it needs to be restored.
        if delemsg$ = " " then L33390
            call "FILEBGON" (#33)
            for c% = 1% to maxlines% : for l% = 1% to 30%
              if sn_index2%(c%,l%) = 0% then L33185
                sn_trankey$ = str(cuscode$) & str(invnr$) &              ~
                                              str(sn_lineid$(c%)) & "##"
                if invnr$ = " " then str(sn_trankey$,10,8) =             ~
                                                        userid$ & hex(06)
                convert l% to str(sn_trankey$,22,2), pic(00)
                call "SERSAVE" (sn_index2%(c%,l%), "RT", sn_trankey$,    ~
                                25%, part$(c%), userid$, "4", "2", 1%,   ~
                                #2, #34, #32, #33)
L33185:     next l%  :  next c%

            if invtype$ <> "O" or bol$ = " " then L33340
                readkey$ = str(cuscode$) & str(so$) & bol$
                call "READ100" (#17, readkey$, f1%(17)) /* SHPHDRS */
                if f1%(17) = 0% then L33340
                     if invonfile% = 0% then L33310
                     readkey1$ = "RS" & str(so$) & str(bol$)& hex(00)
L33225:              call "PLOWNEXT" (#34, readkey1$, 21%, f1%(34))
                     if f1%(34) = 0% then L33310
                          get #34 using L33240, readkey$
L33240:                        FMT POS(43), CH(45)
                          call "REDALT1" (#32, readkey$, 1%, f1%(32))
                          if f1%(32) = 0% then L33225
                               get #32 using L33260, readkey$
L33260:                             FMT POS(1), CH(1)
                               if readkey$ = "2" then L33280
                                    call "DELETE" (#34, readkey1$, 62%)
                                    goto L33225
L33280:                        get #34 using L33285, readkey$
L33285:                             FMT POS(1), CH(42)
                               put #32 using L33295, hex(74), readkey$
L33295:                             FMT POS(1), CH(1), POS(216), CH(42)
                               rewrite #32
                               goto L33225
L33310:               readkey$ = str(cuscode$) & str(so$) & bol$
                      call "READ101" (#17, readkey$, f1%(17))
                      put #17 using L33325, " "
L33325:                 FMT POS(234), CH(8)
                      rewrite #17

L33340:         if invnr$ = " " then L33355
                     readkey$ = str(cuscode$) & invnr$
                     call "DELETE" (#23, readkey$, 17%)
L33355:         if invonfile% = 0% then clear_inprocess
                     for c% = 1% to maxlines%
                          gosub remove_hold
                     next c%
                     goto clear_inprocess


L33390
*        Assign Invoice Number if so required.
            if invnr$ <> " " then L33425
                call "ARINEXT" (#12, #2, #23, #25, invtype$, store$,     ~
                                             cuscode$, billxref$, invnr$)
                inpmessage$ = "Saving Invoice " & invnr$ & "..."
                print at(04,02), str(inpmessage$)

L33425
*        Now throw the invoice into the files, lines 1st then Hdr.
            seq% = 0%
            stgrossinv, sttaxable = 0
            if invtype$ <> "F" then L33480
                maxlines% = 1%
                part$(1), descr$(1) = "Finance Charge"
                ship(1), conv(1), lotqtys(1,1) = 1
                price(1), pricestk(1), lineext(1) = netinv
                nonstockmsg$(1) = "Y"
                taxable$(1) = "N"
                sales$(1), discs$(1) = salesacct$
L33480:     for c% = 1% to maxlines%
                if lsts$(c%) <> "D" then L33540
                     if pos("RF" = invtype$) > 0% then L33798
                          gosub remove_hold
                          for l% = 1% to 30%
                             if sn_index2%(c%, l%) = 0% then L33520
                               readkey1$ = bin(sn_index2%(c%,l%),3)
                               call "DELETE" (#33, readkey1$, 3%)
L33520:                   next l%
                  call "TXTFUTIL" (#22, f2%(22%), "XOUT", textidl$(c%))
                          goto L33595

L33540:         if pos("RF" = invtype$) = 0% then gosub place_hold

                seq% = seq% + 1%  : convert seq% to seq$(c%), pic(###)
                call "GLUNFMT"  (sales$(c%))
                call "GLUNFMT"  (discs$(c%))
                if invtype$ <> "C" then L33595
                     order      (c%) = -order      (c%)
                     ship       (c%) = -ship       (c%)
                     openqty    (c%) = -openqty    (c%)
                     linediscamt(c%) = -linediscamt(c%)
                     lineext    (c%) = -lineext    (c%)
L33595:         for l% = 1% to 30%
                     lot$  (l%) = lots$  (c%,l%)
                     lotqty(l%) = lotqtys(c%,l%)
                     if invtype$ = "C" then lotqty(l%) = -lotqty(l%)
                     if sn_index2%(c%,l%) = 0% then L33660
                          sn_trankey$ = str(cuscode$) & str(invnr$) &    ~
                                        str(sn_lineid$(c%)) & "##"
                          convert l% to str(sn_trankey$,22,2), pic(00)
                          sn_clear% = 0%
                          call "SERSAVE" (sn_index2%(c%,l%), "RT",       ~
                                          sn_trankey$, 25%, part$(c%),   ~
                                          userid$, "4", "2", sn_clear%,  ~
                                          #2, #34, #32, #33)
L33660:         next l%
                if lsts$(c%) = "D" then L33798  /* Skip deleted lines */
                if nonstockmsg$(c%) <> " " then nonstockmsg$(c%) = "Y"
                if curr$ <> "Y" then goto L33750
                if currency$ = statutory$ then goto L33750
                write #43 using L33700, currency$, cuscode$, invnr$,      ~
                    seq$(c%), pricestk(c%), price(c%), linediscamt(c%),  ~
                    lineext(c%), convdate$, conveqv, convunt, " "
L33700:                      FMT CH(4), CH(9), CH(8), CH(3), 4*PD(14,4), ~
                                 CH(6), 2*PD(14,7), CH(22)
                pricestk(c%)    = round(pricestk(c%)    * conveqv, 4)
                price   (c%)    = round(price   (c%)    * conveqv, 4)
                linediscamt(c%) = round(linediscamt(c%) * conveqv, 4)
                lineext(c%)     = round(lineext (c%)    * conveqv, 4)
                stgrossinv      = stgrossinv + lineext(c%)
                     if taxable$(c%) = "Y" then                          ~
                                  sttaxable = sttaxable + lineext(c%)

L33750:         write #f1% using L35655, cuscode$, invnr$, seq$(c%),      ~
                     item$(c%), part$(c%), descr$(c%), cat$(c%),         ~
                     order(c%), ship(c%), openqty(c%), pricestk(c%),     ~
                     stkuom$(c%), priceuom$(c%), conv(c%), price(c%),    ~
                     linediscpct(c%), linediscamt(c%), lineext(c%),      ~
                     taxable$(c%), sales$(c%), discs$(c%), " ",          ~
                     textidl$(c%), soseq$(c%), lot$(), lotqty(),         ~
                     nonstockmsg$(c%), project$(c%), sn_lineid$(c%),     ~
                     0, 0, demtype$(c%), 0, e_lines%(c%), " "
                if cus_part_type$(c%) = " " then L33798
                if cus_part$(c%) = "** No Cross Reference **" then L33798
                call "PTUSEDSB"("W","ARI ", str(cuscode$) & invnr$,      ~
                                      seq$(c%), cus_part$(c%),           ~
                                      str(cus_part_descr$(c%),,30%),     ~
                                      cus_part_type$(c%), ret%)
L33798:     next c%

*        Delete possible extraneous transactions from SERTIF
            if invonfile% = 1% or bol$ = " " then L33830
                readkey$ = "RT" & str(cuscode$) & str(userid$) & hex(06)
                call "DELETE" (#34, readkey$, 19%)

L33830:     call "GLUNFMT"  (aracct$   )
            call "GLUNFMT"  (frtacct$  )
            call "GLUNFMT"  (taxacct$  )
            call "GLUNFMT"  (salesacct$)
            call "GLUNFMT"  (discacct$ )
            call "DATUNFMT" (invdate$ )
            call "DATUNFMT" (expdate$ )
            call "DATUNFMT" (shipdate$)
            if invtype$ <> "C" then L33915
                grossinv   = -grossinv
                invdiscamt = -invdiscamt
                frtamt     = -frtamt
                taxamt     = -taxamt
                netinv     = -netinv
                for i% = 1% to 30%
                     termsdue(i%) = -termsdue(i%)
                next i%
L33915:     bal = 0  :  /* Filler */
            if curr$ <> "Y" then goto L34020
            if currency$ = statutory$ then goto L34020
                write #42 using L33950, currency$, cuscode$, invnr$,      ~
                           grossinv, invdiscamt, frtamt, taxamt, netinv, ~
                           bal, convdate$, conveqv, convunt,             ~
                           termsdue(), " "
L33950:                FMT CH(4), CH(9), CH(8), 6*PD(14,4), CH(6),       ~
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

L34020:     if stlmnt$ <> " " then terms$ = " "
            get #1 using L34030, acctxref$, billxref$, custype$
L34030:         FMT POS(771), 2*CH(9), POS(1023), CH(2)

            for u3% = 1% to 30%
                if termsdisc$(u3%) = " " then termsdisc$(u3%) = blankdate$
                if termsnet$(u3%)  = " " then termsnet$(u3%)  = blankdate$
            next u3%

            put #f% using L35365, cuscode$, invnr$, po$, so$, bol$,       ~
                shipto$(), soldto$(), shipdate$, howship$, fob$,         ~
                carrier$, cartons, weight, frtbill$, salesman$(),        ~
                comm%(), region$, pc$, invdate$, expdate$, " ", date,    ~
                userid$, vf$, aracct$, frtacct$, taxacct$, salesacct$,   ~
                discacct$, grossinv, invdiscpct, invdiscamt, frtamt,     ~
                taxamt, netinv, bal, billxref$, stlmnt$, store$,         ~
                taxcode$, taxpct, invtype$, invrsn$, textid$, posthny$,  ~
                " ", artype$, terms$, termsdue(), termsdiscs(),          ~
                termsdisc$(), termsnet$(), custype$, acctxref$,          ~
                currency$, billxref$, cuscode$, invnr$, pm_so$, pm_inv$, ~
                " "
            if f% = 9% then put #9 using L34100, session$, invtype$,      ~
                                                cuscode$, invnr$
L34100:         FMT POS(2001), CH(6), CH(1), CH(9), CH(8)
            write #f%
            if invtype$ = "R" then                                       ~
                call "TXTFUTIL" (#22, f2%(22), "SAV2", textid$) else     ~
                call "TXTFUTIL" (#22, f2%(22), "SAVE", textid$)

*        Flag BOL as invoiced ("O" Types with BOL specified)
            if invtype$ <> "O" or bol$ = " " then L34180
                readkey$ = str(cuscode$) & str(so$) & bol$
                call "READ101" (#17, readkey$, f1%(17))
                if f1%(17) = 0% then L34180
                     put #17 using L34160, invnr$
L34160:                   FMT POS(234), CH(8)
                     rewrite #17

*        Clear SO In-process and go get next invoice
L34180:     goto clear_inprocess


        remove_hold
            if invonfile% = 0% then return
            for l% = 1% to 30%
                if bufmark$(c%,l%) <> " " then L34330
                if bufqty(c%,l%) <= 0 then L34325
                   postqty, bufqty = 0
                   for i% = l% to 30%
                     if bufmark$(c%,i%) <> " " then L34255
                     if bufqty  (c%,i%) <= 0   then L34250
                     if buflot$(c%,i%) <> buflot$(c%,l%) then L34255
                        bufqty = bufqty + bufqty (c%,i%)
L34250:                 bufmark$(c%,i%) = "*"
L34255:            next i%
                if bolonfile% = 0% then L34305
                   for i% = 1% to 30%
                     if postmark$(c%,i%) <> " " then L34295
                     if postqty  (c%,i%) <= 0   then L34290
                     if postlot$(c%,i%) <> buflot$(c%,l%) then L34295
                        postqty = postqty + postqty (c%,i%)
L34290:                 postmark$(c%,i%) = "*"
L34295:            next i%

L34305
*              HELD = MAX(0, BUFQTY - POSTQTY)
                held = bufqty - postqty
                if held = 0 then L34325
                call "HNYHOLD" (#13, part$(c%), store$, buflot$(c%,l%),  ~
                                         -held, return%)
L34325:         bufmark$(c%,l%) = "*"
L34330:     next l%
            return


        place_hold
            for l% = 1% to 30%
                if mark$(c%,l%) <> " " then L34535
                if lotqtys(c%,l%) <= 0 then L34530
                   lotqty, lotqty_orig, postqty, bufqty = 0
                   for i% = l% to 30%
                     if mark$(c%,i%) <> " " then L34405
                     if lotqtys (c%,i%) <= 0   then L34400
                     if lots$  (c%,i%) <> lots$  (c%,l%) then L34405
                        lotqty = lotqty + lotqtys(c%,i%)
                        lotqty_orig = lotqty_orig + lotqtys_orig(c%,i%)
L34400:                 mark$(c%,i%) = "*"
L34405:            next i%
                if invonfile% = 0% then L34455
                   for i% = 1% to 30%
                     if bufmark$(c%,i%) <> " " then L34445
                     if bufqty  (c%,i%) <= 0   then L34440
                     if buflot$(c%,i%) <> lots$  (c%,l%) then L34445
                        bufqty = bufqty + bufqty (c%,i%)
L34440:                 bufmark$(c%,i%) = "*"
L34445:            next i%
                if bolonfile% = 0% then L34495
L34455:            for i% = 1% to 30%
                     if postmark$(c%,i%) <> " " then L34485
                     if postqty  (c%,i%) <= 0   then L34480
                     if postlot$(c%,i%) <> lots$   (c%,l%) then L34485
                        postqty = postqty + postqty (c%,i%)
L34480:                 postmark$(c%,i%) = "*"
L34485:            next i%

L34495:         if invtype$ = "C" then lotqty = -lotqty
*              HELD = MAX(0, BUFQTY - POSTQTY)
                held = bufqty - postqty
*              NEEDED = MAX(0, LOTQTY - POSTQTY)
                needed = lotqty - postqty
                held = needed - held
                if held = 0 then L34530
                if invtype$ = "O" and bolonfile% = 1% and invonfile% = 0%~
                     then L34518  else L34520
L34518:                 if orig_ship$ = ship$ then L34530
                            held = lotqty - lotqty_orig
L34520:         call "HNYHOLD" (#13, part$(c%), store$, lots$(c%,l%),    ~
                                          held, return%)
L34530:         mark$(c%,l%) = "*"
L34535:     next l%

            gosub remove_hold

            return

        clear_inprocess  /* Clear SO in-process tasking record         */
            if invtype$ <> "O" then L34800
                readkey$ = all(hex(00))       /* Clear In-process Flag */
                str(readkey$,,3) = userid$
                call "DELETE" (#19, readkey$, 10%)
L34800:         goto inputmode

        set_pm_flags
            if pm_on$ <> "Y" then return
            if pm_so$ <> " " then L34900
                if pm_cus_so$  = " " then pm_so$  = pm_sys_so$           ~
                                 else pm_so$  = pm_cus_so$
L34900:     if pm_inv$ <> " " then return
                if pm_cus_inv$ = " " then pm_inv$ = pm_sys_inv$          ~
                                 else pm_inv$ = pm_cus_inv$
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: BCKMASTR  (Read Only)             */~
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
            XX(4),          /* Internal ID to text in TXTFILE.         */~
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
            XX(1),          /* SA Period                               */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Order Discount Percent                  */~
            XX(8),          /* Open Order Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            POS(893), CH(4),/* Currency code                           */~
            XX(1),          /* How put on hold                         */~
            CH(1),          /* PM Surcharge SO Flag                    */~
            CH(1)           /* PM Surcharge INV Flag                   */

L35195: FMT                 /* FILE: BCKLINES                          */~
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
            CH(1),          /* Demand Type                             */~
            XX(1),          /* Priority Code                           */~
            XX(4),          /* Internal ID to text in TXTFILE.         */~
            XX(1),          /* Allocation Flag                         */~
            XX(54)          /* Filler                                  */


L35365: FMT                 /* FILE: ARIMASTR                          */~
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
            CH(190)         /* Filler (Internal, unused space)         */

L35655: FMT                 /* FILE: ARILINES                          */~
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
            PD(14,4),       /* MDMC                                    */~
            BI(4),          /* Extra Lines                             */~
            CH(92)          /* Filler (Internal, unused space)         */

        REM *************************************************************~
            * Extra Lines Conflict Warnings/Errors                      *~
            *************************************************************
        extra_load_conflict

L36050: % Current No. of Lines: ###  (+ Implied Lines: ###) : Total #####

            put askmsg$(1%) using L36050, maxlines%, total_e_lines%,      ~
                                         maxlines% + total_e_lines%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press any PF key to confirm and continue"
            u3% = 2%
            call "ASKUSER" (u3%, "* * * MAXIMUM LINES CONFLICT * * *",   ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            return

        extra_append_conflict
            put askmsg$(1%) using L36050, c%, total_e_lines% + temp%,     ~
                                         c% + total_e_lines% + temp%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press PF16 to continue, Press RETURN to" &    ~
                          " re-enter Part Code"
L36220:     u3% = 2%
            call "ASKUSER" (u3%, "* * * APPEND LINES CONFLICT * * *",    ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            if u3% = 16% then return
            if u3% =  0% then return
               goto L36220

        severe_load_error
            askmsg$(1%) = "Attempting to Load in excess of 100 Lines"
            askmsg$(2%) = "This Unexpected Situation may cause" &        ~
                          " LOSS OF DATA"
            askmsg$(3%) = "Press PF25 to acknowledge and continue"
L36340:     u3% = 2%
            call "ASKUSER" (u3%, "* * * EXCESS LINES ERROR * * *",       ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            if u3% = 25% then return
               goto L36340

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
                              #2,    /*  SYSFILE2                      */~
                              #12,   /*  STORNAME                      */~
                              #26,   /*  USERINFO                      */~
                              #4,    /*  HNYMASTR                      */~
                              #27,   /*  HNYLOCNS                      */~
                              #13,   /*  HNYQUAN                       */~
                              #28)   /*  LOCATION                      */
            return

        display_corebank
            ret% = 0%
            close ws
            call "CMSLINK" addr(#50, userid$, "R", "CORDSPLY",           ~
                                "        ", "      ", " ", "N",          ~
                                "                        ",              ~
                                "                ", "N", u3%, ret%)
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
               at (01,02), fac(hex(8c)), invtypedescr$,                  ~
               at (01,47), "Heading Page 1",                             ~
               at (01,62), "     Post:",                                 ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
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

L40460:        gosub pf1315
               if keyhit% = 15 then L40125
               return

        setpf1
        if edit% = 2% then L40560         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0fffffff1a00)
           if fieldnr% > least% then L40540
                str(pf$(2),17,18) = " "
                str(pfkey$, 4, 1) = hex(ff)
L40540:    if fieldnr% > 1% then L40550
                str(pf$(3%),42%,19%) = " " : str(pfkey$,19%,1%) = hex(ff)
L40550:    return

L40560:  if fieldnr% > 0% then L40635     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                            (12)Delet"&~
                    "e Invoice    (13)Instructions"
           pf$(2) = "(2)Line Items    (5)Next Screen          (25)Manag"&~
                    "e Text       (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit   (16)End Invoice "
           pfkey$ = hex(0102ffff05ffffffffffff0c0dff0f10ff191d1a00)
           if invtype$ <> "F" then L40610
                str(pf$(2),,13)   = " " : str(pfkey$,2,1)  = hex(ff)
L40610:    if maxlines% <> 0% then return
                str(pf$(1),42,18) = " " : str(pfkey$,12,1) = hex(ff)
                return

                                         /* Edit Mode- Field Enabled   */
L40635:    pf$(1) = "(1)Start Over                                     "&~
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
               at (01,02), fac(hex(8c)), invtypedescr$,                  ~
               at (01,47), "Heading Page 2",                             ~
               at (01,62), "     Post:",                                 ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
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
               at (13,49), fac(hex(8c)), currdesc$(1)           , ch(32),~
                                                                         ~
               at (14,02), "Freight Charges ",                           ~
               at (14,30), fac(lfac$( 9)), frtamt$              , ch(10),~
               at (14,49), fac(hex(8c)), currdesc$(2)           , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L41360
                gosub call_customer_credit
                goto L41120

L41360:        gosub pf1315
               if keyhit% = 15 then L41120
               return

        setpf2
        if edit% = 2% then L41435         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0fffffff1a00)
           return

L41435:  if fieldnr% > 0% then L41510     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over    (4)Prev Screen          (12)Delet"&~
                    "e Invoice    (13)Instructions"
           pf$(2) = "(2)Line Items    (5)Next Screen          (25)Manag"&~
                    "e Text       (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit   (16)End Invoice "
           pfkey$ = hex(0102ff0405ffffffffffff0c0dff0f10ff191d1a00)
           if invtype$ <> "F" then L41485
                str(pf$(2),,13) = " " : str(pfkey$,2,1) = hex(ff)
L41485:    if maxlines% <> 0% then return
                str(pf$(1),42,18) = " " : str(pfkey$,12,1) = hex(ff)
                return

                                         /* Edit Mode- Field Enabled   */
L41510:    pf$(1) = "(1)Start Over                                     "&~
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
               at (01,02), fac(hex(8c)), invtypedescr$,                  ~
               at (01,47), "Heading Page 3",                             ~
               at (01,62), "     Post:",                                 ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
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

L42500:        gosub pf1315
               if keyhit% = 15 then L42135
               return

        setpf3
        if edit% = 2% then L42575         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0fffffff1a00)
           return

L42575:  if fieldnr% > 0% then L42650     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over    (4)Prev Screen          (12)Delet"&~
                    "e Invoice    (13)Instructions"
           pf$(2) = "(2)Line Items    (5)Next Screen          (25)Manag"&~
                    "e Text       (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit   (16)End Invoice "
           pfkey$ = hex(0102ff0405ffffffffffff0c0dff0f10ff191d1a00)
           if invtype$ <> "F" then L42625
                str(pf$(2),,13) = " " : str(pfkey$,2,1) = hex(ff)
L42625:    if maxlines% <> 0% then return
                str(pf$(1),42,18) = " " : str(pfkey$,12,1) = hex(ff)
                return

                                         /* Edit Mode- Field Enabled   */
L42650:    pf$(1) = "(1)Start Over                                     "&~
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
               at (01,02), fac(hex(8c)), invtypedescr$,                  ~
               at (01,47), "Line Item Page 1",                           ~
               at (01,67), "Post:",                                      ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
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
               at (10,02), fac(hex(8c))  , qty_msg$             , ch(25),~
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

L43560:        if keyhit% <> 11% then L43585
                    call "SOSTATUS" (cuscode$, so$, soseq$(c%), 2%, #6,  ~
                          #18, #17, #5, #15, #16, #1, #5, #6, #22, 0%, 0%)
                     goto L43165

L43585:        if keyhit% <> 14% then L43605
                    gosub display_corebank
                    goto L43165

L43605:        gosub pf1315
               if keyhit% = 15 then L43165
               return

        setpf4
        if edit% = 2% then L43730         /* Input Mode                 */
           pf$(1%) = "(1)Start Over   (22)Cust Pt Xref         (10)Cost"&~
                    "s & Prices    (13)Instructions"
           pf$(2%) = "(2)Restart Line  (4)Previous Field               "&~
                    "              (15)Print Screen"
           pf$(3%) = "                (23)Mfg Prt Xref                 "&~
                    "              (16)Line Summary"
           pfkey$ = hex(0102ff04ffffffffff0affff0dff0f10ffffffffff161700)
           if core% = 0% or fieldnr% = 1% then L43685
               str(pf$(3%),1%,13%) = "(14)Core Bank"
               str(pfkey$,14%,1%) = hex(0e)
L43685:    if fieldnr% <> 1% then goto L43695
                str(pf$(1%),42%,18%) = " " : str(pfkey$,10%,1%) = hex(ff)
L43695:    if fieldnr% = 1% then L43705
                str(pf$(3%),64%,16%) = " " : str(pfkey$,16%,1%) = hex(ff)
L43705:    if fieldnr% = 1% and pxref% + cxref% + c_cxref% = 3% then L43707
                str(pf$(1%),17%,16%) = " " : str(pfkey$,22%,1%) = hex(ff)
L43707:    if fieldnr% = 1% and pxref% + mxref% = 2% then L43710
                str(pf$(3%),17%,16%) = " " : str(pfkey$,23%,1%) = hex(ff)
L43710:    if fieldnr% > 2% then L43716
                str(pf$(2%),18%,17%) = " " : str(pfkey$,4%,1%)  = hex(ff)
L43716:    if fieldnr% <> 15% then L43720
                init (" ")  str(pf$(1%),17%,46%), str(pf$(2%),17%,46%)
                init (" ")  str(pf$(3%),1%)
                init (hex(ff))  str(pfkey$,3%,10%), str(pfkey$,16%,8%)
L43720:    return

L43730:  if fieldnr% > 0% then L43815     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over    (5)Next Scrn (10)Prices (11)SO St"&~
                    "atus         (13)Instructions"
           pf$(2) = "                 (8)Set Open Qty = 0     (12)Delet"&~
                    "e Line Item  (15)Print Screen"
           pf$(3) = "                 (9)Header Screen        (25)Manag"&~
                    "e Line Text  (16)Line Summary"
           pfkey$ = hex(0102ffff05ffff08090a0b0c0dff0f10ff191d0012)
           if core% = 0% then L43785
               str(pf$(3%),1%,13%) = "(14)Core Bank"
               str(pfkey$,14%,1%) = hex(0e)
L43785:    if (closeso$ = "Y" or closeso$ = "A") and invtype$ = "O" then ~
                                                                    L43795
                str(pf$(2),18,20) = " " : str(pfkey$,8,1) = hex(ff)
L43795:    if invtype$ = "O" then L43805
                str(pf$(1),42,13) = " " : str(pfkey$,11,1) = hex(ff)
L43805:    return
                                         /* Edit Mode- Field Enabled   */
L43815:    pf$(1) = "(1)Start Over                                     "&~
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
            on fieldnr%     gosub   L44160,         /* P.O. Item Number */~
                                    L44170,         /* Project Number   */~
                                    L44170,         /* Sales Distr. Acct*/~
                                    L44170,         /* Sales Discs Acct */~
                                    L44180          /* Demand Type      */
            goto L44200
L44160:                             lfac$(fieldnr%) = hex(80) : return
L44170:                             lfac$(fieldnr%) = hex(81) : return
L44180:                             lfac$(fieldnr%) = hex(82) : return

L44200:     accept                                                       ~
               at (01,02), fac(hex(8c)), invtypedescr$,                  ~
               at (01,47), "Line Item Page 2",                           ~
               at (01,67), "Post:",                                      ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
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
        /*     AT (13,02), "Demand Type (for SA)",                       ~
               AT (13,30), FAC(LFAC$( 5)), DEMTYPE$(C%)         , CH(01),~
          */                                                             ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 10% then L44640
                    call "BCKPRCSB" (cuscode$, custype$, part$(c%),      ~
                          cat$(c%), pc$, currency$, currtype$, ship(c%), ~
                          #2, #1, #4, #40)
                    goto L44200

L44640:        if keyhit% <> 14% then L44680
                    gosub display_corebank
                    goto L44200

L44680:        gosub pf1315
               if keyhit% = 15 then L44200
               return

        setpf5
        if edit% = 2% then L44860         /* Input Mode                 */
           pf$(1) = "(1)Start Over                            (10)Costs"&~
                    " & Prices    (13)Instructions"
           pf$(2) = "(2)Restart Line  (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(0102ff04ffffffffff0affff0dff0fffffffff00)
           if core% = 0% then L44840
               str(pf$(3%),1%,13%) = "(14)Core Bank"
               str(pfkey$,14%,1%) = hex(0e)
L44840:    return

L44860:  if fieldnr% > 0% then L45000     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over    (4)Previous Screen      (10)Costs"&~
                    " & Prices    (13)Instructions"
           pf$(2) = "                                         (12)Delet"&~
                    "e Line Item  (15)Print Screen"
           pf$(3) = "                 (9)Header Screen        (25)Manag"&~
                    "e Line Text  (16)Line Summary"
           pfkey$ = hex(01ffff04ffffffff090aff0c0dff0f10ff191d00)
           if core% = 0% then L44970
               str(pf$(3%),1%,13%) = "(14)Core Bank"
               str(pfkey$,14%,1%) = hex(0e)
L44970:    return

                                         /* Edit Mode- Field Enabled   */
L45000:    pf$(1) = "(1)Start Over                                     "&~
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
            on fieldnr%      gosub  L46200,         /* Inv Reason Code  */~
                                    L46200,         /* Post Hny?        */~
                                    L46200,         /* Currency         */~
                                    L46210          /* FC Amount        */

            dfac$ = hex(8c)  /* Inventory on */
            if pos("RF" = invtype$) > 0% then L46175
            if hnymsg$ = " " then L46230
                dfac$ = hex(84)      /* Inventory off */
L46175:         lfac$ (2) = hex(9c)
            goto L46230
                                    lfac$(fieldnr%) = hex(80) : return
L46200:                             lfac$(fieldnr%) = hex(81) : return
L46210:                             lfac$(fieldnr%) = hex(82) : return

L46230:     accept                                                       ~
               at (01,02), fac(hex(8c)), invtypedescr$,                  ~
               at (01,47), "INVOICE SUMMARY",                            ~
               at (01,67), "Post:",                                      ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
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

            if keyhit% <> 26% then goto L46830
                gosub call_customer_credit
                goto L46230

L46830:        gosub pf1315
               if keyhit% = 15 then L46230
               return

        setpf6
        if edit% = 2% then L46980         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffff1a00)
           return

L46980:  if fieldnr% > 0% then L47160     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Line Items     (10)Manage Payment Schedule     "&~
                    "             (15)Print Screen"
           pf$(3) = "(9)Header Screen                         (26)Custo"&~
                    "mer Credit   (16)Save Invoice"
           pfkey$ = hex(0102ffffffffffff090aff0c0dff0f10ffff1a00)
           if delemsg$ <> " " then str(pf$(3),68,12) = "DELETE Invce"
           if invtype$ <> "F" then L47100
                str(pf$(2),,15) = " "
                str(pfkey$,2,1) = hex(ff)
L47100:    if terms$ = "DATED" then L47130
                str(pf$(2),19,30) = " "
                str(pfkey$,10 ,1) = hex(ff)
L47130:    return

                                         /* Edit Mode- Field Enabled   */
L47160:    pf$(1) = "(1)Start Over                                     "&~
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
                xt% = x% + t% : if seq$(xt%) =  " " then L48090
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
               at (01,02), fac(hex(8c)), invtypedescr$,                  ~
               at (01,33), fac(hex(8c)), curr_hdr_msg$          , ch(08),~
               at (01,42), fac(hex(8c)), currency$              , ch(04),~
               at (01,50), "*LINE SUMMARY*",                             ~
               at (01,67), "Post:",                                      ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
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

               gosub pf1315
               if keyhit% = 15 then L48155

               if keyhit% <> 14% then L48795
                  gosub display_corebank
                  goto L48155

L48795:        if keyhit% <> 10% then L48825
                  close ws
                  call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
                  gosub locations
                  goto L48155

L48825:        if keyhit% <> 22% then L48850
                  toggle% = mod(toggle% + 1%, 2%)
                  gosub set_summary_display
                  goto L48155

L48850:        return

        setpf9
        if edit% = 2% then L48965         /* Display Mode               */
           pf$(1) = "(1)Strt Ovr (4)Prev Page   (8)Chg Qty (11)Appnd Ln"&~
                    "             (13)Instructions"
           pf$(2) = "(2)1st Page (5)Next Page   (9)Header  (12)Del Ln  "&~
                    "             (15)Print Screen"
           pf$(3) = "(3)Last Pge (6)Down (7)Up (10)Locns   (14)Core Bnk"&~
                    "(22)Tgl Scrn (16)End Invoice "
           pfkey$ = hex(0102030405060708090a0b0c0d0e0f10ffffffffff1600)
           if pxref% + cxref% + c_cxref% = 3% then L48905
                str(pf$(3%),51%,12%) = " " : str(pfkey$,22%,1%) = hex(ff)
L48905:    if core% = 1% then L48915
              str(pf$(3%),39%,12%) = " " : str(pfkey$,14%,1%) = hex(ff)
L48915:    if t% <> 0% then L48935
              str(pf$(2),1,12), str(pf$(1),13,12), str(pf$(3),13,7) = " "
              str(pfkey$, 2,1), str(pfkey$, 4,1), str(pfkey$, 6,1)       ~
                                                              = hex(ff)
L48935:    if t% + 15% < maxlines% then L48955
              str(pf$(3),1,12), str(pf$(2),13,12), str(pf$(3),21,5)= " "
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
                  on fieldnr% gosub L50200,         /* Customer Code    */~
                                    L50230,         /* Invoice Number   */~
                                    L50260,         /* SO-BOL  Number   */~
                                    L50290,         /* Settlement #     */~
                                    L50400,         /* Ship-to          */~
                                    L50450,         /* Sold-to          */~
                                    L50490,         /* Invoice Date     */~
                                    L50530,         /* Customer PO      */~
                                    L50610,         /* Store Number     */~
                                    L50680          /* Expiration Date  */
                  return

L50200
*        Customer Code- In ARINUMIN            CUSCODE$
            return

L50230
*        Invoice Number- In ARINUMIN           INVNR$
            return

L50260
*        Sales Order & BOL- In ARINUMIN        SO$ - BOL$
            return

L50290
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

L50400
*        Ship-to                               SHIPTO$
            if str(shipto$()) <> " " then return
                errormsg$ = "Ship-to may not be blank"
                return

L50450
*        Sold-to                               SOLDTO$
            if soldto$(1) = " " then str(soldto$()) = " "
            return

L50490
*        Invoice Date                          INVDATE$
            call "DATEOK" (invdate$, u3%, errormsg$)
            return

L50530
*        Customer PO Number                    PO$
          if invtype$ = "F" then return
            if po$ <> " " then return
                get #1 using L50560, poreqd$
L50560:              FMT POS(1020), CH(1)
                if poreqd$ <> "Y" then return
                     errormsg$ = "PO is required for this Customer."
                     return

L50610
*        Store Code                            STORE$
            storedescr$ = hex(06) & "Please Select Store."
            call "GETCODE" (#12, store$, storedescr$, 0%, 0, f1%(12))
            if f1%(12) = 1% then return
                errormsg$ = "Invalid Store Code."
                return

L50680
*        Expiration Date (Recurring Only)      EXPDATE$
          if invtype$ <> "R" then return
            call "DATEOK" (expdate$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATEOK" (invdate$, temp%, errormsg$)
                if u3% <= temp%   then                                   ~
                     errormsg$ = "Expiration Date must be after Invoice"&~
                                 " Date"
                return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51125,         /* Date Shipped     */~
                                    L51150,         /* How Ship         */~
                                    L51165,         /* FOB              */~
                                    L51180,         /* Carrier          */~
                                    L51240,         /* Freight Bill     */~
                                    L51255,         /* Shipment Cartons */~
                                    L51300,         /*          Weight  */~
                                    L51380,         /* Currency         */~
                                    L51345          /* Freight Charges  */
                  return

L51125
*        Date Shipped                          SHIPDATE$
            if shipdate$ = " " or shipdate$ = blankdate$ then return
            call "DATEOK" (shipdate$, u3%, errormsg$)
            return

L51150
*        How Ship                              HOWSHIP$
            return

L51165
*        FOB                                   FOB$
            return

L51180
*        Carrier                               CARRIER$
            carrierdescr$ = " "
            if carrier$ = " " then return
                readkey$ = "CARRIERS " & carrier$
                carrierdescr$ = hex(06) & "Select Carrier. PF-16 if none."
                call "PLOWCODE" (#8, readkey$, carrierdescr$, 9%, 0.30,  ~
                                                                  f1%(8))
                if f1%(8) = 1% then L51225
                     carrier$, carrierdescr$ = " " : return
L51225:         carrier$ = str(readkey$,10)
                return

L51240
*        Freight Bill                          FRTBILL$
            return

L51255
*        Number of Cartons Shipped             CARTONS$
            if cartons$ = " " then cartons$ = "0"
            convert cartons$ to cartons, data goto L51270 : goto L51275
L51270:         errormsg$ = "Cartons must be a positive number" : return
L51275:     if cartons < 0 then L51270
            call "CONVERT" (cartons, 2.2, cartons$)
            if cartons = 0 then cartons$ = " "
            return

L51300
*        Shipment Weight                       WEIGHT$
            if weight$ = " " then weight$ = "0"
            convert weight$ to weight, data goto L51315 : goto L51320
L51315:         errormsg$ = "Weight must be a positive number" : return
L51320:     if weight < 0 then L51315
            call "CONVERT" (weight, 2.2, weight$)
            if weight = 0 then weight$ = " "
            return

L51345
*        Freight Charges                       FRTAMT$
            if frtamt$ = " " then frtamt$ = "0"
            convert frtamt$ to frtamt, data goto L51360 : goto L51365
L51360:         errormsg$ = "Invalid entry for Freight Charges" : return
L51365:     call "CONVERT" (frtamt, 2.2, frtamt$)
            return

L51380
*        Currency code                       CURRENCY$
            if currency$ = " " then currency$ = statutory$
            if invtype$ = "O" then enabled% = 0%
            if enabled% = 0% then return
            if currency$ <> statutory$ then L51415
               currdesc$(), convdate$ = " " : conveqv, convunt = 1
               return
L51415:     call "GETCODE" (#40, currency$, currdesc$(1), 0%, 0, f1%(40))
            if f1%(40) <> 0% then L51430
                errormsg$ = "Invalid Currency code.  Try again." : return
L51430
*        GET_CURRENCY_INFO
            convdate$ = " " : conveqv, convunt = 1
            currdesc$(2) = " "
            if currency$ = statutory$ then return
            call "DATREVRS" ( invdate$, rev_date$, errormsg$ )
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#41, currkey$, 5%, f1%(41))
            if f1%(41) <> 0% then goto L51495
                errormsg$ = "Invalid Currency Code for this transaction."
                return
L51495:     get #41 using L51500, convdate$, conveqv, convunt
L51500:         FMT POS(12), CH(6), 2*PD(14,7)
            call "CONVERT" (convunt, 2.7, str(currdesc$(2), 1,10))
            currdesc$(2) = currdesc$(2) & " " & currency$
            currdesc$(2) = currdesc$(2) & "/" & statutory$

            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52160,         /* Region Code      */~
                                    L52210,         /* Salesmen / Split */~
                                    L52855,         /* PM INV Flag      */~
                                    L52320,         /* Price Code Deflt */~
                                    L52365,         /* Order Discount % */~
                                    L52420,         /* Tax Code / Pct   */~
                                    L52520,         /* A/R Type Flag    */~
                                    L52565,         /* Payment Terms    */~
                                    L52640,         /* A/R Account      */~
                                    L52715,         /* Tax Account      */~
                                    L52745,         /* Freight Account  */~
                                    L52775,         /* Sales Account    */~
                                    L52815          /* Sales Discs Acct */
                return

L52160
*        Region                                REGION$
            regiondescr$ = " " : if region$ = " " then return
                regiondescr$ = hex(06) & "Select Sales Region"
                readkey$ = "REGIONS  " & region$
                call "PLOWCODE" (#8, readkey$, regiondescr$, 9%,.3,f1%(8))
                if f1%(8) = 1% then L52195
                     errormsg$ = "Invalid Sales Region Code." : return
L52195:         region$ = str(readkey$,10)
                return

L52210
*        Salesman Code / Split %               SALESMAN$()
            total% = 0%
            for i% = 1% to 3%
                if salesman$(i%) <> " " then L52240
                     salesmandescr$(i%), comm$(i%) = " "
                     comm%(i%) = 0%  :  goto L52295
L52240:         call "GETCODE" (#7, salesman$(i%), salesmandescr$(i%),   ~
                                                       0%, 0.30, f1%(7))
                if f1%(7) = 1% then L52265
                     errormsg$ = "Salesman Code " & salesman$(i%) &      ~
                                 " not on file."  :  return
L52265:         if comm$(i%) = " " then comm$(i%) = "0"
                convert comm$(i%) to comm%(i%), data goto L52280
                goto L52285
L52280:              errormsg$ = "Commission % must be 0 - 100." : return
L52285:         total% = total% + comm%(i%)
                convert comm%(i%) to comm$(i%), pic(##0)
L52295:     next i%
            if total% <= 100% then return
                errormsg$ = "Total Commission Splits can not exceed 100%."
                return

L52320
*        Price Code Default                    PC$
            if (pc$ >= "A" and pc$ <= "Q") or                            ~
               (pc$ >= "0" and pc$ <= "8") then L52345
                errormsg$ = "Valid Price Codes are 'A'-'Q' and '0'-'8'"
                return
L52345:     readkey$ = "PRICECODE" & pc$
            call "DESCRIBE" (#8, readkey$, pcdescr$, 0%, f1%(8))
            return

L52365
*        Invoice Discount Percent              INVDISCPCT$
            invdiscpct = 0
            if invdiscpct$ = " " then invdiscpct$ = "0"
            convert invdiscpct$ to invdiscpct, data goto L52390
            goto L52400
L52390:         errormsg$ = "Invoice Discount must be -25 to +100%"
                return
L52400:     if invdiscpct < -25 or invdiscpct > 100 then L52390
            call "CONVERT" (invdiscpct, 2.2, invdiscpct$)
            return

L52420
*        Sales Tax Code and Tax Percent        TAXCODE$ / TAXPCT$
            taxcodedescr$ = " "  :  if taxcode$ = " " then L52480
                taxcodedescr$ = hex(06) & "Select Tax Code."
                call "GETCODE" (#20, taxcode$, taxcodedescr$, 0%, 0.30,  ~
                                                                 f1%(20))
                if f1%(20) = 1% then L52455
                     errormsg$ = "Tax Code not on file" : return
L52455:         if taxpct$ <> " " then L52480
                     get #20 using L52465, taxpct
L52465:                   FMT XX(40), PD(14,4)
                     call "CONVERT" (taxpct, 2.4, taxpct$)
                     return
L52480:     if taxpct$ = " " then taxpct$ = "0"
            convert taxpct$ to taxpct, data goto L52490 : goto L52500
L52490:         errormsg$ = "Invalid entry for Tax Percent" : return
L52495:         errormsg$ = "Tax Percent Range is 0 - 100"  : return
L52500:     if taxpct < 0 or taxpct > 100 then L52495
            call "CONVERT" (taxpct, 2.4, taxpct$)
            return

L52520
*        A/R Type                              ARTYPE$
            if pos("ACE" = artype$) <> 0% then L52540
                errormsg$ = "A/R Type must be 'A', 'C', or 'E'."
                return
L52540:     if artype$ = "A" then artypedescr$ = "Accounts Receivable"
            if artype$ = "C" then artypedescr$ = "Cash"
            if artype$ = "E" then artypedescr$ = "Expense"
            return

L52565
*        Payment Terms (Code)                  TERMS$
            if stlmnt$ <> " " then terms$ = " "
            if terms$  <> "DATED" then L52605
                if pos("OMD" = invtype$) <> 0% then L52595
                     errormsg$ = "Terms may not be dated for this type"
                     return
L52595:         termsdescr$ = "Will be specified"
                return
L52605:     if terms$ = " " then L52620
                call "GETCODE" (#21, terms$, termsdescr$, 0%, 0, f1%(21))
                if f1%(21) = 1% then return
L52620:     termsdescr$ = "0% 0 Days, Net 0 Days"
            if stlmnt$ <> " " then termsdescr$ = "Per Item Applied To"
            return

L52640
*        Net Invoice Account                   ARACCT$
        test_ar_acct
            aracctdescr$ = hex(06) & "Select A/R Account"
            call "GETCODE" (#3, aracct$, aracctdescr$, 0%, 0.30, f1%(3))
            if f1%(3) = 1% then L52675
                errormsg$ = "Invalid Net Invoice Distribution Account"
                return
L52675:     get #3 using L52680, accttype$
L52680:         FMT XX(39), CH(1)
            if artype$ = "A" and accttype$ <> "A" then                   ~
                                  errormsg$ = "Must be an Asset Account"
            if artype$ = "C" and accttype$ <> "$" then                   ~
                                  errormsg$ = "Must be a Cash Account"
            return

L52715
*        Sales Tax Account
            taxacctdescr$ = hex(06) & "Select Sales Tax Account"
            call "GETCODE" (#3, taxacct$,taxacctdescr$, 0%, 0.30, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Tax Account" : return

L52745
*        Freight Account
            frtacctdescr$ = hex(06) & "Select Freight Account"
            call "GETCODE" (#3, frtacct$,frtacctdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Freight Account" : return

L52775
*        Sales Account Default                 SALESACCT$
            salesacctdescr$ = hex(06) & "Select Sales Account."
            call "GETCODE" (#3, salesacct$, salesacctdescr$, 0%, 0,      ~
                                                                  f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Account Code"
                return

L52815
*        Sales Discounts Account               DISCACCT$
            discacctdescr$ = hex(06) & "Select Sales Discounts Account."
            call "GETCODE" (#3, discacct$, discacctdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Discounts Account"
                return

L52855
*        Test Data for Precious Metal Surcharge at Invoice Update
            if pm_on$ <> "Y" then return
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
                                    L53790,         /* Currency code    */~
                                    L53610,         /* Unit Price       */~
                                    L53670,         /* Line Item Disc   */~
                                    L53740,         /* Line Item Ext    */~
                                    L53760          /* Line Taxable?    */
                  return

L53115
*        Part Code                             PART$()
            if part$(c%) = " " then L53155
                call "READ100" (#4, part$(c%), f1%(4))
                if f1%(4) = 1% then L53190
                     call "HNYGREF" (part$(c%), #24, #4, f1%(4))
                     if f1%(4) = 0% then L53155
                          call "READ100" (#4, part$(c%), f1%(4))
                          if f1%(4) = 1% then L53190
L53155:     call "GETCODE" (#4, part$(c%), " ", 0%, 0, f1%(4))
            if f1%(4) = 1% then L53190
                if part$(c%) <> " " then L53180
                     errormsg$ = "Part Code cannot be left blank"
                     return
L53180:         nonstockmsg$(c%) = "(Non-Stock Part)"
                cus_part$(c%) = "** No Cross Reference **"
                return
L53190:     get #4 using L53191, cat$(c%), partflag$
L53191:         FMT POS(90), CH(4), POS(166), CH(4)
                if cus_part$(c%) = " "  then                             ~
                     cus_part$(c%) = "** No Cross Reference **"
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
            call "PLOWCODE" (#8, readkey$, stkuomdescr$, 9%, 0.30, f1%(8))
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
            call "PLOWCODE" (#8, readkey$, priceuomdescr$, 9%,0.30,f1%(8))
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

L53790
*        Currency code                       CURRENCY$
            if currency$ = " " then currency$ = statutory$
            if invtype$ = "O"  then enabled% = 0%
            if enabled% = 0% then return
            if currency$ <> statutory$ then L53810
               currdesc$(), convdate$ = " " : conveqv, convunt = 1
               goto L53940
L53810:     call "GETCODE" (#40, currency$, currdesc$(1), 0%, 0, f1%(40))
            if f1%(40) <> 0% then goto get_currency_info
                errormsg$ = "Invalid Currency code.  Try again." : return
        get_currency_info
            convdate$ = " " : conveqv, convunt = 1
            if currency$ = statutory$ then L53940
            call "DATREVRS" (invdate$, rev_date$, errormsg$)
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#41, currkey$, 5%, f1%(41))
            if f1%(41) <> 0% then goto L53910
                errormsg$ = "Invalid Currency Code for this transaction."
                return
L53910:     get #41 using L53920, convdate$, conveqv, convunt
L53920:         FMT POS(12), CH(6), 2*PD(14,7)
            gosub set_currency_description
L53940:     if edit% <> 2% then return
                gosub L23420 /* CPRASSGN gets price */
                errormsg$ = " "
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
                                    L54370,         /* Sales Discs Acct */~
                                    L54440          /* Demand Type      */
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
            salesdescr$ = hex(06) & "Select Sales Account."
            call "GETCODE" (#3, sales$(c%), salesdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Account Code"
                return

L54370
*        Sales Discounts Account               DISCS$()
            discsdescr$ = hex(06) & "Select Sales Discounts Account."
            call "GETCODE" (#3, discs$(c%), discsdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Discounts Account"
                return

L54440
*        Demand Type                           DEMTYPE$()
            if pos("12" = demtype$(c%)) <> 0% then return
                errormsg$ = "Please enter '1' or '2'."
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
            call "PLOWCODE" (#8, readkey$, invrsndescr$, 9%, 0.3, f1%(8))
            if f1%(8) = 1% then invrsn$ = str(readkey$,10) else          ~
                            errormsg$ = "Invalid Invoice Reason Code."
            return

L55280
*        Affect Inventory
            if posthny$ = "Y" and (invtype$ = "C" or invtype$ = "A")     ~
                                           then gosub test_for_the_s_word
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
            lsts$(1%) = " " : lines% = 1% : deles% = 0%
            if fcamt <> 0 then return
                delemsg$ = "Invoice will be deleted."
                lsts$(1%) = "D" : lines% = 0% : deles% = 1%
            return

L55440
*        Currency code                       CURRENCY$
            if currency$ = " " then currency$ = statutory$
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
            call "DATREVRS" (invdate$, rev_date$, errormsg$)
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

*        Serial Numbers restrict credit memos from affecting inventory
*        Same is true for Reman Parts, so test here too.
        test_for_the_s_word
            core_part% = 0%
            if maxlines% = 0% then return
                for c% = 1% to maxlines%
                  if lsts$(c%) = "D" then L55790
                  if ship(c%) >= 0 and invtype$ = "A" then L55790
                     call "SERENABL" (part$(c%), sn_enable%, lc%, #2, #4)
                     if sn_enable% = 0% then L55782
L55752:                   if invtype$ = "C" then errormsg$ = "Credit Me"&~
                                      "mos" else errormsg$ = "Negative "&~
                                                             "Adustments"
                          if core_part% <> 1% then                       ~
                          errormsg$ = errormsg$ & " can't affect invent"&~
                                         "ory if Serial #s are involved."~
                               else                                      ~
                          errormsg$ = errormsg$ & " can't affect invent"&~
                                        "ory if Core Parts are involved."
                          return
L55782:           if core% = 0% then L55790
                     plowkey$ = str(part$(c%)) & hex(00)
                     call "PLOWNEXT" (#45, plowkey$, 25%, core_part%)
                     if core_part% = 1% then L55752
L55790:         next c%
                return

L56000: REM *************************************************************~
            * TEST SHIPPED QUANTITY, LOT NUMBER and HANDLE SERIAL NRs.  *~
            * --------------------------------------------------------- *~
            * Test the data elements on the shipped quantity screen     *~
            * line.  I moved it down here because it was getting in the *~
            * way upstairs (had to renumber by twos, etc.!..).          *~
            *************************************************************

            call "SERENABL" (part$(c%), sn_enable%, lc%, #2, #4)
            keep_sn_enable% = sn_enable%  /* For test below */
            if pos("CFR" = invtype$) > 0% then sn_enable% = 0%

*        Test the Quantity Shipped
            if ship$ = " " then ship$ = "0"
            convert ship$ to ship(c%), data goto L56140 : goto L56150
L56140:         errormsg$ = "Invalid Shipment Quantity" : return
L56150:     call "CONVERT" (ship(c%), 2.2, ship$)
            if keep_sn_enable% = 1% and ship(c%) < 0 then posthny$ = "N"
            if invtype$ = "A" or invtype$ = "C" or ship(c%) >= 0 then    ~
                                                                    L56171
            if nonstockmsg$(c%) <> " " then L56171
                errormsg$ = "Shipment Quantity must >= Zero." : return
L56171
*        Apply Part Code Min Quantity and Min Increment to Order Quantity
L56173:     call "BCKMINSB" (ship(c%), minsoqty, minsoinc, u3%)
            if u3%  = 16% then goto L56180
            if u3% <>  1% then goto L56173
                errormsg$ = hex(00)
                return
L56180
*        Test for allowable overshipment qty
L56181:     if invtype$ <> "O" then L56190
            call "SHPOVRSB" (part$(c%), seq$(c%), (soopen(c%) +          ~
                  qtyshipped(c%)), (qtyschld(c%) + qtypreinv(c%) +       ~
                  ship(c%) - bolqty(c%)), def_percent(c%), def_unit(c%), ~
                  sys_def_percent, u3%)
            if u3% = 16% then L56190
            if u3% <> 1% then L56181
                errormsg$ = "Please reenter ship quantity."
                return
L56190:     if invtype$ = "O" then                                       ~
                             openqty(c%) = max(0, soopen(c%) - ship(c%))
            call "CONVERT" (openqty(c%), 2.2, openqty$)

*          Clear distribution if (1) Shipped is less than or equal to 0
*                                (2) Non-inventory type invoice,  OR
*                                (3) non-lot and non-serial number part
*          IF SHIP(C%)              <= 0           THEN 56290
            if pos("FR" = invtype$) <> 0%          then L56290
            if lot_enable% = 0% and sn_enable% = 0% then L56290
            goto L56380
L56290:         lc% = 1%
                lotqty$ = " "  :  sn_lot1was$(c%) = all(hex(00))
                gosub clear_lot_info           /* Clear out lot distr */
                lotqtys(c%,1) = ship(c%)
                if pos("FR" = invtype$) > 0% then lotqtys(c%,1) = 0
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
L56450:     if pos("AC" = invtype$) = 0% then L56468
                if ship(c%) > 0 then L56468
                    if lotqtys(c%,1) = ship(c%) then L56490
                         errormsg$ = "Returns must go to a single Lot; "&~
                                     "Quantities must match."
                         return
L56468:     if lotqtys(c%,1) > 0 and lotqtys(c%,1) <= ship(c%) then L56490
                errormsg$ = "Lot Quantity must be greater than Zero" &   ~
                            " and less than or equal to Qty Shipped."
                return
L56490:     if lot_enable% <> 2% or lots$(c%,1) <> " " then L56520
                errormsg$ = "Lot Number required for this Part"
                return
L56520:     if nonstockmsg$(c%) = " " or lots$(c%,1) = " " then L56550
                errormsg$ = "Lots may not be entered for Non-Stock Parts"
                return
L56550:     if lots$(c%,1) = " " and lot_enable% < 2% then L56640
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
                                 #2, #4, #32, #33)
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
                                 #2, #4, #32, #33)
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
                             #4, #13, #12, #2, #32, #34, #33)
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
          if pos("RF" = invtype$) > 0% then return
            call "HNYAVAIL" (#4, #13, part$(c%), store$, lots$(c%,lc%),  ~
                                          errormsg$, 9e9, temp1, f1%(13))
            if errormsg$ = " " then return
            temp, bufqty, postqty, crqty, held, needed, lotqty = 0
            gosub rev_sign_for_credits
            for i% = 1% to c%
                if part$(i%) <> part$(c%) then L57740
                   for j% = 1% to 30%
                       if lots$(i%,j%) <> lots$(c%,lc%) then L57690
*                        LOTQTY = LOTQTY + MAX(0, LOTQTYS(I%,J%))
                          lotqty = lotqty + lotqtys(i%,j%)
L57690:                if buflot$(i%,j%) <> lots$(c%,lc%) then L57710
                          bufqty = bufqty + max(0, bufqty(i%,j%))
                          if bufqty(i%,j%) < 0 then                      ~
                                            crqty = crqty + bufqty(i%,j%)
L57710:                if postlot$(i%,j%) <> lots$(c%,lc%) then L57730
                          postqty = postqty + max(0, postqty(i%,j%))
L57730:            next j%
L57740:     next i%
            held   = max(0, bufqty - postqty)
*          NEEDED = MAX(0, LOTQTY - POSTQTY)
            needed = lotqty - postqty
            temp = needed - held
            if temp <= 0     then errormsg$ = " "
            if temp <= temp1 then errormsg$ = " "
        rev_sign_for_credits
            if invtype$ <> "C" then return
                ship(c%)    = (-1) * ship(c%)
                mat lotqtys = (-1) * lotqtys
                mat bufqty  = (-1) * bufqty
                mat postqty = (-1) * postqty
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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
            call "SHOSTAT" ("One Moment Please")
            u3% = 2%
            call "UPDUSRLG" ("ARIUPDTE", " ", " ", " ", session$, u3%,   ~
                                                                " ", " ")
            end
