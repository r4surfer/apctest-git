        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  BBBB    CCC   K   K   OOO   N   N  L      IIIII  N   N   *~
            *  B   B  C   C  K  K   O   O  NN  N  L        I    NN  N   *~
            *  BBBB   C      KKK    O   O  N N N  L        I    N N N   *~
            *  B   B  C   C  K  K   O   O  N  NN  L        I    N  NN   *~
            *  BBBB    CCC   K   K   OOO   N   N  LLLLL  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKONLIN - 'On-line' version of Sales Order Entry.        *~
            *            The on-line refers to planning functions--     *~
            *            all other updating is done in the normal       *~
            *            manner.                                        *~
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
            * 10/13/86 ! ORIGINAL (Kenneth A. Burnnett, Esq.)     ! ERN *~
            * 02/11/87 ! Lot Tracking Enhancements                ! ERN *~
            * 05/14/87 ! HNYMASTR, RETMASTR record length mods.   ! JIM *~
            *          !   HNYGLGET gets acct #5 (sales). Was #4. !     *~
            *          !   Added SYSFILE2 to CPRASSGN.            !     *~
            * 11/03/87 ! Added EXPORT Flag maintenance            ! MJB *~
            * 11/16/87 ! Added Multi-currency, CURMASTR, BCKLNCUR,! JIM *~
            *          !   CURCONVR. Also, see the cautions at    !     *~
            *          !   the top of the source file.            !     *~
            * 01/15/88 ! Added SO History checks                  ! JDH *~
            * 03/22/88 ! Added Cancelled sales order logic.       ! JDH *~
            * 04/27/88 ! Take out ability to delete & have only   ! JDH *~
            *          !   called up orders able to be cancelled  !     *~
            * 07/01/88 ! Open Order amt stored in Statutory curr. ! JIM *~
            * 08/04/88 ! Update CUSTOMER A/R w/ transaction curr. ! JIM *~
            * 08/04/88 ! Correct CUSTOMER BILLTO/SHIPTO FMT stmt. ! JIM *~
            * 08/04/88 ! HNYMASTR conversion factor to 7 decimals.! JIM *~
            * 08/29/88 ! Corrected Credit display on last screen. ! JDH *~
            * 09/27/88 ! Rounded Credit control amounts & line    ! JDH *~
            *          !   extentions to 2 decimal places.        !     *~
            * 09/29/88 ! Currency accums now use open qty & Save  ! JDH *~
            *          !   screen has proper values for Canceld SO!     *~
            * 10/05/88 ! Added DEMMASTR to call to BCKNEXT        ! JDH *~
            * 10/11/88 ! Fixed prog name in IN-PROCESS Msg,       ! JDH *~
            *          ! Honors Allow Deletion of SO Flag,        !     *~
            *          ! Added check of DEMMASTR for SO #,        !     *~
            *          ! Fixed precision of UOM Convertion factor,!     *~
            *          ! Initialised ORIGDATE$ & ORIGUSER$,       !     *~
            *          ! Fixed Shipdate/Datasave error            !     *~
            * 11/22/88 ! Fixed Call to PLNRSUB                    ! MJB *~
            * 12/02/88 ! Added Credit Parent Logic.               ! JDH *~
            * 02/24/89 ! Proj 7890206 Four SO Entry Modifications.! JIM *~
            * 04/25/89 ! Added Call to GETSCRN; fixed FMT @ 17200;! JDH *~
            *          !  Changed screen literal for line item    !     *~
            *          !  'Shipped' to Invoicd'; Added see prices;!     *~
            *          !  Added credit hold on # days late.       !     *~
            * 09/25/89 ! Corrected convert error for extension.   ! JDH *~
            * 10/04/89 ! Disabled CURRENCY in edit mode.          ! JDH *~
            * 12/07/89 ! Moved GET of MINSOQTY/MINSOINC to 50000s.! JDH *~
            * 12/12/89 ! Corrected arguments in call to BCKONL3.  ! JDH *~
            * 12/14/89 ! Corrected open order doubling on recall  ! MJB *~
            * 12/21/89 ! Fixed stocking/pricing conversion when   ! JDH *~
            *          !  converting to statutory.                !     *~
            * 12/27/89 ! Initialized CONV(C%) to one, not zero.   ! JDH *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 05/29/90 ! Allow delete on unsaved SO, added dates, ! JDH *~
            *          !  & amt to SO PLOWsreen.                  !     *~
            * 09/26/90 ! Variable fields accessible in Input Mode,! JDH *~
            *          !  disabled Invoiced field.                !     *~
            * 04/05/91 ! PRR 11801 Remove tacky STOP and fixed    ! SID *~
            *          !     FS95 when BILLTO doesn't exist.      !     *~
            *          ! PRR 11722 Added codes to avoid the users !     *~
            *          !     from deleting Scheduled, Shipped and !     *~
            *          !     Invoiced Line Items.                 !     *~
            *          ! PRR 11781 Inconsistent Allocation Method.!     *~
            * 05/24/91 ! PRR 11892 Avoid Cancel Reason Description! JIM *~
            * 02/04/92 ! Integrated BCKONL1,2,&3 back into this.  ! JDH *~
            *          ! PRR 11909 Check status of BillTo, Parent.!     *~
            *          ! PRR 10609 Honor Default Demand Type swtch!     *~
            *          ! PRR 11967 Allow Cancel of LIs with BOLs. !     *~
            *          ! PRR 11905, 11249, 11623  Honor new Auto- !     *~
            *          !   Hold option (Override manual or not).  !     *~
            *          ! PRR 10611, 10612  LI Text in Inputmode.  !     *~
            * 04/20/92 ! PRR 12402 Fixed 99 line limit problem.   ! JDH *~
            *          ! Orders w/o open qty not on CR Hold.      !     *~
            * 07/24/92 ! PRR 12531 Added Allocation type 'P' for  ! JDH *~
            *          !   Previously Allocated.                  !     *~
            * 09/08/92 ! Chngd call to PLANALLC for more info out.! JDH *~
            * 11/23/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 11/23/92 ! Cust Credit- Added calls to ARQCUSCR.    ! JIM *~
            * 11/23/92 ! Brought Core Deposit Module to R6.02.03. ! JIM *~
            * 12/01/92 ! PRR 12488 - If set, use default value for! JDH *~
            *          !   Allocation Method & Demand Type from   !     *~
            *          !   the Part Master file (HNYMASTR).       !     *~
            *          ! Clean up some implied integers in arrays.!     *~
            * 02/03/93 ! SO Entry Pgms back in sync;  Default str ! JDH *~
            *          !   from USERINFO, Calc default date from  !     *~
            *          !   offset, Override of Credit Hold, Cust  !     *~
            *          !   Status Check subroutine.               !     *~
            *          ! PRR 12614 Export Flag defaults from Cust.!     *~
            *          ! PRR 11200 Warning if PO already used.    !     *~
            * 06/24/93 !1) Allow use of Customer Part number on   !     *~
            *          !   input via PF(22)Cust Pt Xref.          !     *~
            *          !3) Added PF22 Pt Toggle on summary screen !     *~
            *          !   to toggle between CMS part /Cust Part. !     *~
            * 08/02/93 !Added MFG Part Cross Reference capability.! MLJ *~
            * 08/17/93 !Support for Extra Line Generation (core,?)! KAB *~
            * 10/27/93 !Support for Shipping Priority Code.       ! MLJ *~
            * 11/02/93 !PRR 13025 - Affed PF(18) call to HNYMSTSB ! MLJ *~
            *          ! and HNYPROC, VENDOR, & VENPRICE files.   !     *~
            * 11/08/93 !Initialized U3% = 0% before call to TASKUP! MLJ *~
            * 03/07/94 !Added support for BOMSPHDR file et al     ! WPH *~
            * 03/24/94 ! Honor Use ONLY A/R for credit limit check! JDH *~
            * 01/12/95 ! Added Precious Metal Surcharge Additions ! RJH *~
            *          !   to SO Price and Extention, governed by !     *~
            *          !   three PM Flags (Sys, SO, INV).         !     *~
            * 03/21/95 ! PRR 13143.  Added Print Acknowledgements?! JDH *~
            * 04/05/95 ! PRR - 13187 Track Cross-Reference Parts  ! RJH *~
            *          !   for future reference and printing on   !     *~
            *          !   acknowledgements, and invoices, etc.   !     *~
            * 04/26/95 ! PRR 13283 - Test for BCK-ARI conflict.   ! RJH *~
            * 07/18/95 ! Corrected CMSLINK call to CORDSPLY.      ! MLJ *~
            * 02/27/96 ! Acks print unless customer set to 'N'.   ! JDH *~
            * 09/06/96 ! Changes for the year 2000.               ! DXL *~
	    * 12/23/96 ! PRR 13677 - Edit Tax Field for exist SO!RJH/DER*~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


        com planflags$(25)20,            /* Planning System Flags      */~
            yymmdd$(490)6,                                               ~
            eff$(490)3,                  /* EFF, PLT WORK ARRAY        */~
            oldcompplowkey$(100)31,      /* FOR PHANTOM LOGIC          */~
            pip%(490),                                                   ~
            cumf%(490),                                                  ~
            awca%(490),                  /* CONCURRENT WC AVAILABILITY */~
            awcu1%(490),                 /* 1ST CONCURRENT USED        */~
            awcu2%(490),                 /* 2ND CONCURRENT USED        */~
            awcu3%(490),                 /* 3RD CONCURRENT USED        */~
            f1%(64),                     /* Record on file flag        */~
            phfact(101),                                                 ~
                               /* THE ABOVE ELEMENTS CANNOT BE CHANGED */~
            parts$   (1000)25,                                           ~
            intagnr$ (1000)19,                                           ~
            outtagnr$(1000)19,                                           ~
            rte$     (1000)3,            /* WHICH ROUTE TO USE         */~
            bom$     (1000)3,            /* WHICH BOM TO USE           */~
            ed%      (1000),                                             ~
            sd%      (1000),                                             ~
            parline% (1000),             /* PARENT LINE NUMBER         */~
            action%  (1000),             /* ACTION TO TAKE             */~
            lt%      (1000),             /* LEADTIME ARRAY             */~
            moq%     (1000),             /* MOQ                        */~
            type%    (1000),             /* PART TYPE                  */~
            qtyu     (1000),             /* QTY USED (NEEDED)          */~
            qtyp     (1000),             /* QTY TO PROCURE             */~
                               /* THE ABOVE ELEMENTS ARE THE MATERIALS */~
            wc$(2000)4,                  /* WORK CENTER                */~
            wl$(2000)1,                  /* JUST FOR SUMMARY REPORT    */~
            wa$(2000)1,                  /* IN CASE OF ARRAY OVERFLOW  */~
            ws$(2000)5,                  /* WC STEP #                  */~
            wl%(2000),                   /* LINE STACK                 */~
            du%(2000),                   /* DATE USED ARRAY STACK      */~
            au%(2000),                   /* AMOUNT USED ARRAY STACK    */~
            su%(2000),                   /* SET UP TIME TODAY          */~
            wa%(2000),                   /* WC ALTERNATE SEQ NO.       */~
                               /* THESE ARE THE WORK CENTER ARRAYS     */~
            rtestep$(255)200,            /* THE STEP AS A STRING       */~
            step$   (255)5,              /* STEP                       */~
            yld     (255),               /* STEP YIELD                 */~
            pd%     (255),               /* WC STEP START DATE(SPL PCK)*/~
            mmx%    (255),               /* START OF RTE STEP          */~
            xsd%    (255)                /* START OF RTE STEP          */~
                               /* THESE ARE FOR ROUTE STEPS            */

        dim                                                              ~
            acctxref$9,                  /* Account Xref               */~
            adjmsg$24,                   /* Adj/Cancel reason msg      */~
            adjrsn$9, adjrsndescr$30,    /* Adjustment Reason          */~
            alloc(100), alloc$(100)1,    /* Allocation Quantity/Flag   */~
            allocdflt$1,                 /* Allocation Flag Default    */~
            allocqty$10,                 /* Allocation Qty             */~
            allow_delete_save$1,         /* Save Allow Delete Flag     */~
            allow_delete$1,              /* Allow Deletion of SOs Flag */~
            ar(2), oo(2),                /* A/R, On Order amounts      */~
            askmsg$(3)79,                /* ASKUSER Messages           */~
            avail$10,                    /* Available Quantity         */~
            billto$9,                    /* Bill-to Customer ID        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            can$1,                       /* Cancel Reason Code Here?   */~
            canceldate$8,                /* Cancellation Date          */~
            cat$(100)4,                  /* Part Category              */~
            comm%(3), comm$(3),          /* Commission Split %s        */~
            conv(100), conv$10,          /* Conversion Pricing->Stockng*/~
            convdate$6,                  /* Currency conversion date   */~
            cr_ar_only$1,                /* Use Only A/R for Cr Limit  */~
            crflag$1,                    /* Put Orders on Hold?        */~
            crhold$1,                    /* Order Credit Status        */~
            crmsg$(2)23,                 /* On Credit Hold Message     */~
            currkey$50,                  /* Currency lines read key    */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4, currdesc$(2)32,  /* Currency code& description */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Code              */~
            custaxable$1,                /* Customer Taxable? (Y/N)    */~
            custype$2,                   /* Customer Type Code         */~
            date$8,                      /* Date for screen display    */~
            datetime$7,                  /* Buffer Date/Time Stamp     */~
            demstatus$1,                 /* Demand Status              */~
            demstatusmsg$22,             /* Demand Status Message      */~
            demtype$(100)1,              /* Planning Demand Type       */~
            descr(12),                   /* Plowcode Argument          */~
            descr$(100)32,               /* Part Description           */~
            dflt_dem_type$1,             /* Default Demand Type for SOs*/~
            dfltdue$8,                   /* Default Due Date           */~
            dfltbol$1, dfltpick$1,       /* Default Print Settings Y/N */~
            dfltacks$1,                  /* Default Print Settings Y/N */~
            dfltsales$12,                /* Default Sales Account      */~
            dfltship$8,                  /* Default Req'd Ship Date    */~
            discacct$12,                 /* Sales Discounts Account    */~
            discacctl$(100)12,           /* Sales Discounts (Lines)    */~
            discamt$10,                  /* Line Item Discount Amount  */~
            duedate$(100)8,              /* Due Date                   */~
            e_lines%(100),               /* Extra Lines Possible       */~
            errormsg$79,                 /* Error message              */~
            estnbr$8,                    /* Estimate number            */~
            export$1,                    /* Export Order flag          */~
            ext$10,                      /* Line Item Extension        */~
            fac25$1,                     /* PF(25) FAC                 */~
            filler$(100)54,              /* Line Filler Data           */~
            fob$20,                      /* FOB                        */~
            hdr_map$(24)80,              /* Header Fields Map          */~
            hfac$(25)1,                  /* Header Item FACs           */~
            holdflags$(25)20,            /* Planning System Flags      */~
            horizon$8,                   /* Planning Horizon Date      */~
            how_held$1,                  /* 'M'anual or 'blank'        */~
            howship$20,                  /* How Ship                   */~
            incl(1), incl$(1),           /* Plowcode argument          */~
            inpmessage$79,               /* Informational Message      */~
            invnbr$8,                    /* Invoice number             */~
            item$(100)3,                 /* P.O. Item Number           */~
            lfac$(30)1,                  /* Line Item FACs             */~
            xfac$1,                      /* Xref MFG Code FAC          */~
            lastchanged$8, lastuser$,    /* Last Change Info           */~
            lastplan$8,                  /* Date Last Planned          */~
            line1$79,                    /* First Line of Screen Headr */~
            linedisc(100), linedisc$6,   /* Line Item Discount %       */~
            line_map$(24)80,             /* Line Item Fields Map       */~
            lot$(100)6,                  /* Lot Number                 */~
            lsts$(100)6,                 /* Line Status                */~
            lta%(26),                    /* Lead Time Array            */~
            manadjre$1,                  /* Mandatory Adj Reason Code? */~
            mbom$3, mdate$8, mpart$25,   /* PLNMAXIM Arguments         */~
            mfgcode$13,                  /* Xref MFG Code              */~
            mfgdescr$30,                 /* PLOWCODE Description       */~
            mode$1,                      /* Mode for VFINPSUB          */~
            mlquote_seq$(100)11,         /* Quote number and line      */~
            mpriority$1, mquantity$10,   /*                            */~
            mscdescr$32,                 /* Misc. Description          */~
            msg$79,                      /* Misc use Message           */~
            nonstockmsg$(100)6,          /* Non-Stock Part Flag & Msg  */~
            offset_due$3,                /* Offset Days for Due Date   */~
            offset_shp$3,                /* Offset Days for Ship Date  */~
            oldbomid$3,                  /* Old BOM ID                 */~
            olddemtype$1,                /* Old Demand Type            */~
            oldhdr$(5)200,               /* BCKMASTR SO Header         */~
            oldline$(2)150,              /* BCKLINES SO Line Item      */~
            oldshipdate$8,               /* Old Ship Date              */~
            oldstore$3,                  /* Old Store Code             */~
            opdlrs(4),                   /* BOMOPSUB Cost, Price Info  */~
            openqty(100), openqty$10,    /* Open Order Quantity        */~
            order(100), order$10,        /* Original Order Quantity    */~
            orderdate$8,                 /* Order Date                 */~
            orderdisc$6,                 /* Order Discount Percent     */~
            origdate$8, origuser$3,      /* Originally Entered, User   */~
            origdue$(100)8,              /* Original Due Dates         */~
            override_crhld$1,            /* Allow override of cr hold? */~
            parent$9,                    /* Credit Parent X-ref        */~
            part$(100)25, savepart$25,   /* Part Code                  */~
            partflag$4,                  /* Special/Obsolete Flag      */~
            parttaxable$1,               /* Part Taxable? (Y/N/ )      */~
            pc$1,                        /* Price Code                 */~
            pcd$8,                       /* Planned Completion Date    */~
            pf$(3)79, pfkey$32,          /* PF Prompts and Keys        */~
            pickprint$1,                 /* Pick/BOL/Ack Print Flag    */~
            pickprint$(3)1,              /* Pick/BOL/Ack Print Array   */~
            plowhdr$(3)79,               /* PLOWCODE Headers           */~
            plowkey$99,                  /* Misc Use Plow Key          */~
            pm_hdr_dsply$30,             /* Display header variable    */~
            pm_inv$1,                    /* Precious Metal INV Flag    */~
            pm_sys_inv$1,                /* Precious Metal INV Flag    */~
            pm_on$1,                     /* Precious Metal ON Flag     */~
            pm_so$1,                     /* Precious Metal SO Flag     */~
            pm_sys_so$1,                 /* Precious Metal SO Flag     */~
            pm_adj%(100),                /* PM Price Adjusted Flag     */~
            pm_base(100),                /* PM Base Price Amount       */~
            pm_cus_inv$1,                /* PM INV Flag (Customer)     */~
            pm_cus_so$1,                 /* PM INV Flag (Customer)     */~
            po$16,                       /* Purchase Order Number      */~
            poreqd$1,                    /* PO Required?               */~
            preinv(100), preinv$10,      /* Qtys Pre-Invoice           */~
            price(100), price$10,        /* Unit Price (Pricing UOM)   */~
            pricestk(100),               /* Unit Price (Stockng UOM)   */~
            priceuom$(100)4,             /* Pricing Unit of Measure    */~
            priority$(100)1,             /* Planning Priority Code     */~
            project$(100)8,              /* Project Number             */~
            ptype$(100)3,                /* Part type                  */~
            qtyschld(100), qtyschld$10,  /* Total Qty in Shipping      */~
            readkey$50,                  /* Misc Read Key              */~
            refdesc$(100)30,             /* Reference Part Description */~
            reftype$(100)1,              /* Ref Type 'M'anuf, 'C'ustmr */~
            refpart$(100)25,             /* Reference Part Number      */~
            region$4,                    /* Region Code                */~
            reviewpart$25,               /* Part number for review     */~
            rte$3,                       /* PLANSUB Argument           */~
            salesacct$(100)12,           /* Sales Distr. Account       */~
            salesman$(3)4,               /* Salesman Code / Split %    */~
            shipcode$1,                  /* Customer Shipping Priority */~
            scode$4,                     /* XRef Source Code           */~
            seq$(100)3,                  /* Line Item Number           */~
            scr%(2,30), set%(255),       /* Soft Enables / Screen Refs */~
            sfac$(15)1,                  /* Summary- FACs              */~
            ship(100), ship$10,          /* Quantity Shipped           */~
            shipcode$(100)1,             /* Line Ship Priority Code    */~
            shipdate$(100)8,             /* Required Ship Date         */~
            shipinstr$(2)50,             /* Shipping Instructions      */~
            shipto$(6)30,                /* Ship-to                    */~
            smryh$79,                    /* Sum Hdr Display Variable   */~
            smryhdr$79, smry$(10)79,     /* Summary Display Variables  */~
            so$16,                       /* Sales Order Number         */~
            soassgn$3,                   /* Sales Order # Asgnmnt Flag */~
            soldto$(6)30,                /* Sold-to                    */~
            statutory$4,                 /* Statutory currency code    */~
            stkuom$(100)4,               /* Stocking Unit of Measure   */~
            store$3, dfltstore$3,        /* Store Code                 */~
            taxable$(100)1,              /* Line Taxable? (Y/N)        */~
            temp$16,                     /* Temporary Variable         */~
            tempcus$9, tempso$16,        /* Temp Customer Code, SO#    */~
            terms$20,                    /* Payment Terms (Code)       */~
            testdate$8,                  /* Variable for testing dates */~
            text$(113,1)70,              /* Text Array                 */~
            textid$4, textidl$(100)4,    /* Text ID- Hdr, Lines        */~
            tlines%(2),                  /* Lines Recap                */~
            topen(4), torder(4),         /* Dollars Recap              */~
            txt$4,                       /* For testing TEXTID values  */~
         /* UNPLANOPT$1,                 /* Unplan Option              */~
         /* UNPLANRPT$1,                 /* Unplan Reporting Option    */~
            userid$3,                    /* Current User Id            */~
            vf$200,                      /* Variable Fields            */~
            warn$79,                     /* Error message              */~
            weekday$9, weekdayh$9        /* Day of week                */

        dim f2%(64%), fs%(64%)           /* File Open Status           */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
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
            * #09 ! BCKBUFFR ! Backlog buffer for SO headers            *~
            * #10 ! BCKBUF2  ! Buffer for line items                    *~
            * #11 ! CATEGORY ! Inventory category codes file            *~
            * #12 ! STORNAME ! Store Info File - Name/Address           *~
            * #13 ! HNYQUAN  ! Inventory Part / Store / Lot Quantity Fi *~
            * #14 ! JOBMASTR ! WIP/JC Job Master File                   *~
            * #15 ! HNYGENER ! generic xref file                        *~
            * #16 ! BOMMASTR ! BOM relationship file                    *~
            * #17 ! BOMSPEC  ! options selected file                    *~
            * #18 ! RTEALTRS ! Job, PIP X-ref                           *~
            * #19 ! CALMASTR ! Planning Production Calendar File        *~
            * #20 ! ENGMASTR ! Engineering Master Filer                 *~
            * #21 ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #22 ! TXTFILE  ! System Text File                         *~
            * #23 ! DEMMASTR ! Demand Master File                       *~
            * #24 ! PIPMASTR ! PIP Master File                          *~
            * #25 ! HNYDETAL ! Inventory Detail File                    *~
            * #26 ! RTEMASTR ! Route Master File                        *~
            * #27 ! WCMASTR  ! Work Center Master File                  *~
            * #28 ! WCOUT    ! Work Center Utilization                  *~
            * #29 ! PIPIN    ! PIPINs                                   *~
            * #30 ! PIPOUT   ! PIPOUTs                                  *~
            * #31 ! PIPCROSS ! PIP Pegging File                         *~
            * #32 ! SFMASTR  ! Planning Forecast Master                 *~
            * #33 ! SFCUM2   ! Cummulative Planning Forecasts           *~
            * #34 ! JBCROSS2 ! Job X-Ref File                           *~
            * #35 ! JBPIPXRF ! Job, PIP X-ref                           *~
            * #36 ! HNYALTRS ! Alternate Parts                          *~
            * #37 ! BCKHLNES ! SO History Line Detail                   *~
            * #38 ! USERINFO ! INDIVIDUAL USER DEFAULT INFORMATION      *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #41 ! BCKLNCUR ! Currency-specific BCK line items         *~
            * #42 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #44 ! USERLCMS ! CAELUS User List File                    *~
            * #45 ! CUSPTXRF ! Customer Part Number Xref File           *~
            * #46 ! HNYPROC  ! Procurement History File                 *~
            * #47 ! VENDOR   ! Vendor Master File                       *~
            * #48 ! VENPRICE ! Vendor Price Catalogue File              *~
            * #49 ! BOMSPHDR ! Header file for BOMSPEC options          *~
            * #50 ! DUMMY    ! Dummy for PLOWCODE                       *~
            * #51 ! CCRMASTR ! Customer Credit Master file              *~
            * --- ! -------- ! ---------------------------------------- *~
            * #61 ! WORKFILE ! Work File For Planning                   *~
            * #62 ! WORKFILE ! Work File For Planning                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

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

            select #09, "BCKBUFFR",                                      ~
                        varc,     indexed,  recsize =  1020,             ~
                        keypos = 1, keylen =   10,                       ~
                        alt key  1, keypos =    4, keylen =   7, dup,    ~
                            key  2, keypos =   30, keylen =  16

            select #10, "BCKBUF2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =  10,  keylen = 19

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

            select #15, "HNYGENER",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41

            select #16, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56

            select #17, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #18, "RTEALTRS",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =    1, keylen =  34

            select #19, "CALMASTR",                                      ~
                        varc,     indexed,  recsize = 1962,              ~
                        keypos =    1, keylen =   2

            select #20, "ENGMASTR",                                      ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos =    1, keylen =  29

            select #21, "ARMTERMS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  20

            select #22, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #23, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25, dup

            select #24, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #25, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup     ~

            select #26, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #27, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #28, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #29, "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #30, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #31, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #32, "SFMASTR2",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  25                      ~

            select #33, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25

            select #34, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize = 94,                ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47

            select #35, "JBPIPXRF",                                      ~
                        varc,     indexed,  recsize = 63,                ~
                        keypos =    1, keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19

            select #36, "HNYALTRS",                                      ~
                        varc,     indexed,  recsize = 60,                ~
                        keypos =    1, keylen =  33

            select #37, "BCKHLNES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #38, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos = 1, keylen = 3

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #41,  "BCKLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 19,                      ~
                        alt key  1, keypos =   1, keylen =  23

            select #42, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            select #44, "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =    4, keylen =  30, dup

            select #45, "CUSPTXRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   26, keylen =  35,                     ~
                        alt key  1, keypos =  1, keylen = 60

            select #46, "HNYPROC",                                       ~
                        varc,     indexed,  recsize =  134,              ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =  7, keylen = 65,            ~
                            key  2, keypos =  1, keylen = 40, dup,       ~
                            key  3, keypos = 41, keylen = 31, dup

            select #47, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #48, "VENPRICE",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  58,                     ~
                        alt key  1, keypos =  1, keylen = 34, dup,       ~
                            key  2, keypos = 35, keylen = 34

            select #49, "BOMSPHDR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  53,                     ~
                        alt key  1, keypos = 35, keylen = 19

            select #50, "DUMMY", varc, indexed,                          ~
                                 recsize = 5, keypos = 1, keylen = 1

            select #51, "CCRMASTR",            /* CUSTOMER file shadow */~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            select #61, "PLNWORK1",                                      ~
                        varc,     indexed,  recsize = 120,               ~
                        keypos =   31, keylen =   8,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   35, keylen =   4,         ~
                            key  3, keypos =   30, keylen =   9

            select #62, "PLNWORK2",                                      ~
                        varc,     indexed,  recsize = 66,                ~
                        keypos =    5, keylen =  10,                     ~
                        alt key  1, keypos =    1, keylen =   4, dup

            call "SHOSTAT" ("Opening Files.")
                call "OPENCHCK" (#01, 0%, f2%(01%),   0%, " ")/*CUSTOMER*/
                call "OPENCHCK" (#51, 0%, f2%(51%),   0%, " ")/*'Shadow'*/
                call "OPENCHCK" (#02, 0%, f2%(02%),   0%, " ")
                call "OPENCHCK" (#03, 0%, f2%(03%),   0%, " ")
                call "OPENCHCK" (#04, 0%, f2%(04%),   0%, " ")
                call "OPENCHCK" (#05, 0%, f2%(05%), 200%, " ")
                call "OPENCHCK" (#06, 0%, f2%(06%), 400%, " ")
                call "OPENCHCK" (#07, 0%, f2%(07%),   0%, " ")
                call "OPENCHCK" (#08, 0%, f2%(08%),   0%, " ")
                call "OPENCHCK" (#09, 0%, f2%(09%), 200%, " ")
                call "OPENCHCK" (#10, 0%, f2%(10%), 400%, " ")
                call "OPENCHCK" (#11, 0%, f2%(11%),   0%, " ")
                call "OPENCHCK" (#12, 0%, f2%(12%),   0%, " ")
                call "OPENCHCK" (#13, 0%, f2%(13%),   0%, " ")
                call "OPENCHCK" (#14, 0%, f2%(14%),   0%, " ")
                call "OPENCHCK" (#15, 0%, f2%(15%),   0%, " ")
                call "OPENCHCK" (#16, 0%, f2%(16%),   0%, " ")
                call "OPENCHCK" (#17, 0%, f2%(17%), 100%, " ")
                call "OPENCHCK" (#18, 0%, f2%(18%), 100%, " ")
                call "OPENCHCK" (#19, 0%, f2%(19%),   0%, " ")
                call "OPENCHCK" (#20, 0%, f2%(20%),   0%, " ")
                call "OPENCHCK" (#21, 0%, f2%(21%),   0%, " ")
                call "OPENCHCK" (#22, 0%, f2%(22%),   0%, " ")
                call "OPENCHCK" (#23, 0%, f2%(23%), 100%, " ")
                call "OPENCHCK" (#24, 0%, f2%(24%),   0%, " ")
                call "OPENCHCK" (#25, 0%, f2%(25%),   0%, " ")
                call "OPENCHCK" (#26, 0%, f2%(26%),   0%, " ")
                call "OPENCHCK" (#27, 0%, f2%(27%),   0%, " ")
                call "OPENCHCK" (#28, 0%, f2%(28%), 100%, " ")
                call "OPENCHCK" (#29, 0%, f2%(29%), 100%, " ")
                call "OPENCHCK" (#30, 0%, f2%(30%), 100%, " ")
                call "OPENCHCK" (#31, 0%, f2%(31%), 100%, " ")
                call "OPENCHCK" (#32, 0%, f2%(32%), 100%, " ")
                call "OPENCHCK" (#33, 0%, f2%(33%), 100%, " ")
                call "OPENCHCK" (#34, 0%, f2%(34%), 100%, " ")
                call "OPENCHCK" (#35, 0%, f2%(35%), 100%, " ")
                call "OPENCHCK" (#36, 0%, f2%(36%), 100%, " ")
                call "OPENCHCK" (#37, 0%, f2%(37%),   0%, " ")
                call "OPENCHCK" (#38, 0%, f2%(38%),   0%, " ")
                call "OPENCHCK" (#45, fs%(45%), f2%(45%), 0%, " ")
                call "OPENCHCK" (#46, 0%, f2%(46%),   0%, " ")
                call "OPENCHCK" (#47, 0%, f2%(47%),   0%, " ")
                call "OPENCHCK" (#48, 0%, f2%(48%),   0%, " ")
                call "OPENCHCK" (#49, 0%, f2%(49%), 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "READ100" (#02, "SWITCHS.COR", core%)
            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#38, userid$, f1%(38%))
            if f1%(38%) = 1% then get #38 using L09058, dfltstore$
L09058:         FMT POS(64), CH(3)
            date$ = date : call "DATEFMT" (date$)
            call "SHOSTAT" ("Initializing...")
            ll% = 6%
            ref%, refflag%, cust% = 0%                    /* Xref Flags */
            cms% = 1%                                     /* Xref Flag  */
            xref% = fs%(45%)                              /* Xref Flag  */
*        Load in the Planning Calendar
            call "READ100" (#19, "10", f1%(19))
            if f1%(19) = 0% then L09125
                get #19 using L09095, str(yymmdd$(),1,1470)
L09095:              FMT XX(2), CH(1470)
            call "READ100" (#19, "11", f1%(19))
            if f1%(19) = 0% then L09125
                get #19, using L09095, str(yymmdd$(),1471,1470)
            call "PIPINDEX" (#02, " ", today%, u3%)
            if u3% = 0% then L09140
L09125:         errormsg$ = "INVALID or MISSING PLANNING CALENDAR"
                goto error_abort

L09140
*        Load in the Planning System Switches
            call "READ100" (#02, "PLANNING SYSTEM FLAG", f1%(2))
            if f1%(2) = 1% then L09165
                errormsg$ = "NO PLANNING SYSTEM FLAGS"
                goto error_abort
L09165:     get #02, using L09170, str(planflags$(),,480)
L09170:         FMT XX(20), CH(480)
            mat holdflags$ = planflags$
            get str(planflags$(), 281) using L09185, lta%()
L09185:         FMT 26*BI(4)

*        Set some flags and misc variables
            call "BCKSWTCH" ("BCK", "OFFSTDUE", offset_due$, offsetd, u3%)
                offsetd% = offsetd
            call "BCKSWTCH" ("BCK", "OFFSTSHP", offset_shp$, offsets, u3%)
                offsets% = offsets
            call "BCKSWTCH" ("BCK", "SOASSIGN", soassgn$  , temp, u3%)
            call "BCKSWTCH" ("BCK", "OVRRIDCR", override_crhld$, temp,u3%)
            call "BCKSWTCH" ("BCK", "CRHOLD  ", crflag$   , temp, u3%)
            call "BCKSWTCH" ("BCK", "ALLOCATE", allocdflt$, temp, u3%)
            call "BCKSWTCH" ("BCK", "PICKLIST", dfltpick$ , temp, u3%)
            call "BCKSWTCH" ("BCK", "BOL     ", dfltbol$  , temp, u3%)
            call "BCKSWTCH" ("BCK", "DELETE  ", allow_delete$, temp, u3%)
                allow_delete_save$ = allow_delete$
            call "BCKSWTCH" ("BCK", "MANADJRE", manadjre$, temp, u3%)
            call "BCKSWTCH" ("BCK", "DEM_TYPE", dflt_dem_type$, temp, u3%)
                if dflt_dem_type$ <> "2" then dflt_dem_type$ = "1"
            call "BCKSWTCH" ("BCK", "CRHOLDAR", cr_ar_only$, temp, u3%)

*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
            scr%(1%, 1%) =  1% : set%( 1%) = 13%   /* Customer Code    */
            scr%(1%, 2%) =  2% : set%( 2%) = 13%   /* Sales Order #    */
            scr%(1%, 3%) =  3% : set%( 3%) =  2%   /* Ship-to          */
            scr%(1%, 4%) =  4% : set%( 4%) =  2%   /* Store Code       */
            scr%(1%, 5%) = 45% : set%(45%) =  2%   /* Export Order flag*/
            scr%(1%, 6%) =  5% : set%( 5%) =  2%   /* Customer PO      */
            scr%(1%, 7%) =  6% : set%( 6%) =  2%   /* How Ship         */
            scr%(1%, 8%) =  7% : set%( 7%) =  2%   /* FOB              */
            scr%(1%, 9%) =  8% : set%( 8%) =  2%   /* Order Date       */
            scr%(1%,10%) =  9% : set%( 9%) =  2%   /* Default Due Date */
            scr%(1%,11%) = 10% : set%(10%) =  2%   /* Dflt Ship Date   */
            scr%(1%,12%) = 48% : set%(48%) =  2%   /* PM Schrge SO flg */
            scr%(1%,13%) = 49% : set%(49%) =  2%   /* PM Schrge INVflg */
            scr%(1%,14%) = 11% : set%(11%) =  2%   /* Sold-to          */
            scr%(1%,15%) = 12% : set%(12%) =  2%   /* Cancel Date      */
            scr%(1%,16%) = 13% : set%(13%) =  2%   /* Price Code       */
            scr%(1%,17%) = 14% : set%(14%) =  2%   /* Order Discount % */
            scr%(1%,18%) = 15% : set%(15%) =  2%   /* Terms Code       */
            scr%(1%,19%) = 16% : set%(16%) =  2%   /* Region           */
            scr%(1%,20%) = 17% : set%(17%) =  2%   /* Salesmen / Split */
            scr%(1%,21%) = 18% : set%(18%) =  2%   /* Sales Account    */
            scr%(1%,22%) = 19% : set%(19%) =  2%   /* Sales Disc Acct  */
            scr%(1%,23%) = 20% : set%(20%) =  2%   /* Shipping Instrs  */

            scr%(2%, 1%) = 21% : set%(21%) = 13%   /* Part Code        */
            scr%(2%, 2%) = 22% : set%(22%) =  2%   /* Part Descr       */
            scr%(2%, 3%) = 23% : set%(23%) =  2%   /* Part Category    */
            scr%(2%, 4%) = 24% : set%(24%) =  2%   /* Due Date         */
            scr%(2%, 5%) = 25% : set%(25%) =  2%   /* Required Ship    */
            scr%(2%, 6%) = 26% : set%(26%) =  2%   /* Stocking UOM     */
            scr%(2%, 7%) = 27% : set%(27%) =  2%   /* Pricing  UOM     */
            scr%(2%, 8%) = 28% : set%(28%) =  2%   /* Conversion Fctr  */
            scr%(2%, 9%) = 29% : set%(29%) =  2%   /* Order Qty        */
            scr%(2%,10%) = 30% : set%(30%) =  1%   /* Alloc Qty        */
            scr%(2%,11%) = 31% : set%(31%) =  2%   /* Open Qty         */
            scr%(2%,12%) = 32% : set%(32%) =  2%   /* Dflt Lot         */
            scr%(2%,13%) = 33% : set%(33%) =  2%   /* Quantity Shipped */
            scr%(2%,14%) = 46% : set%(46%) =  2%   /* Currency code    */
            scr%(2%,15%) = 34% : set%(34%) =  2%   /* Unit Price       */
            scr%(2%,16%) = 35% : set%(35%) =  2%   /* Disc Percent     */
            scr%(2%,17%) = 36% : set%(36%) =  2%   /* Taxable?         */
            scr%(2%,18%) = 37% : set%(37%) =  2%   /* PO Item Number   */
            scr%(2%,19%) = 38% : set%(38%) =  2%   /* Project Number   */
            scr%(2%,20%) = 39% : set%(39%) =  2%   /* Sales Account    */
            scr%(2%,21%) = 40% : set%(40%) =  2%   /* Sales Disc Acct  */
            scr%(2%,22%) = 47% : set%(47%) =  2%   /* Shipping Priority*/
            scr%(2%,23%) = 41% : set%(41%) =  2%   /* Demand Priority  */
            scr%(2%,24%) = 42% : set%(42%) =  2%   /* Demand Type      */
            scr%(2%,25%) = 43% : set%(43%) =  2%   /* Planning BOM     */
            scr%(2%,26%) = 44% : set%(44%) = 99%   /* Horizon to Date  */
*       * Next available slot is #50
            call "ENABLSUB" ("INIT", "BCKONLIN", scr%(), set%(), 0%, 0%, ~
                0%, 0%)
            goto L09540

        error_abort
            call "ASKUSER" (2%, "ON_LINE SALES ORDERS",                  ~
                "The Program cannot be run for the following reason:",   ~
                errormsg$, "Press any PF Key to Continue.")
            goto exit_program

L09540:     readkey$ = all(hex(00))      /* GENCODES Directory         */
            str(readkey$, 10) = "UOM"
            call "READ100" (#8, readkey$, uom%)

            line1$   = "On-Line S.O. Entry:"
            smryhdr$ = "Seq Part Number                Order Qty"     &  ~
                       "   Open Qty   Open Ext Rqd Ship Status"

            init(hex(00)) hdr_map$()
            init(hex(01)) str(hdr_map$ ( 2%),  1%) /* Customer Number  */
            init(hex(02)) str(hdr_map$ ( 2%), 22%) /* Sales Order #    */
            init(hex(03)) str(hdr_map$ ( 3%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 4%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 5%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 6%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 7%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 8%),  1%) /* Ship-to          */
            init(hex(04)) str(hdr_map$ ( 2%), 47%) /* Store Number     */
            init(hex(05)) str(hdr_map$ ( 2%), 65%) /* Export Flag      */
            init(hex(06)) str(hdr_map$ ( 3%), 47%) /* PO               */
            init(hex(07)) str(hdr_map$ ( 4%), 47%) /* How Ship         */
            init(hex(08)) str(hdr_map$ ( 5%), 47%) /* FOB              */
            init(hex(09)) str(hdr_map$ ( 6%), 47%) /* Order Date       */
            init(hex(0a)) str(hdr_map$ ( 6%), 68%) /* Due Date         */
            init(hex(0b)) str(hdr_map$ ( 7%), 47%) /* Req'd Ship       */
            init(hex(0c)) str(hdr_map$ ( 8%), 47%) /* PM SO Flag       */
            init(hex(0d)) str(hdr_map$ ( 8%), 68%) /* PM SO Flag       */
            init(hex(0e)) str(hdr_map$ (10%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (11%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (12%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (13%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (14%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (15%),  1%) /* Sold-to          */
            init(hex(0f)) str(hdr_map$ (10%), 47%) /* Cancel Date      */
            init(hex(10)) str(hdr_map$ (11%), 47%) /* Price Code       */
            init(hex(11)) str(hdr_map$ (12%), 47%) /* Order Disc %     */
            init(hex(12)) str(hdr_map$ (13%), 47%) /* Payment Terms    */
            init(hex(13)) str(hdr_map$ (14%), 47%) /* Region           */
            init(hex(14)) str(hdr_map$ (15%), 47%) /* Salesman #1      */
            init(hex(14)) str(hdr_map$ (16%), 47%) /* Salesman #2      */
            init(hex(15)) str(hdr_map$ (17%),  1%) /* Sales Account    */
            init(hex(14)) str(hdr_map$ (17%), 47%) /* Salesman #3      */
            init(hex(16)) str(hdr_map$ (18%),  1%) /* Sales Discs Acct */
            init(hex(17)) str(hdr_map$ (19%),  1%) /* Ship Instruction */
            init(hex(17)) str(hdr_map$ (20%),  1%) /* Ship Instruction */

            init(hex(00)) line_map$()
            init(hex(01)) str(line_map$(14%),  1%) /* Part Number      */
            init(hex(02)) str(line_map$(14%), 35%) /* Part Descr       */
            init(hex(03)) str(line_map$(15%),  1%) /* Category         */
            init(hex(04)) str(line_map$(15%), 12%) /* Due Date         */
            init(hex(05)) str(line_map$(15%), 25%) /* Ship Date        */
            init(hex(06)) str(line_map$(15%), 40%) /* Stocking UOM     */
            init(hex(07)) str(line_map$(15%), 57%) /* Pricing  UOM     */
            init(hex(08)) str(line_map$(15%), 67%) /* Conversion       */
            init(hex(09)) str(line_map$(16%),  1%) /* Order Qty        */
            init(hex(0a)) str(line_map$(16%), 18%) /* Allocation       */
            init(hex(0b)) str(line_map$(16%), 36%) /* Open Qty         */
            init(hex(0c)) str(line_map$(16%), 52%) /* Lot Number       */
            init(hex(0d)) str(line_map$(16%), 63%) /* Shipped          */
            init(hex(0e)) str(line_map$(17%),  1%) /* Currency code    */
            init(hex(0f)) str(line_map$(17%), 12%) /* Price            */
            init(hex(10)) str(line_map$(17%), 29%) /* Line Disc %      */
            init(hex(11)) str(line_map$(17%), 60%) /* Taxable?         */
            init(hex(12)) str(line_map$(18%),  1%) /* PO Item          */
            init(hex(13)) str(line_map$(18%), 13%) /* Project          */
            init(hex(14)) str(line_map$(18%), 29%) /* Sales Acct       */
            init(hex(15)) str(line_map$(18%), 54%) /* Sales Disc Acct  */
            init(hex(16)) str(line_map$(19%),  1%) /* Ship Priority    */
            init(hex(17)) str(line_map$(19%), 14%) /* Plan Priority    */
            init(hex(18)) str(line_map$(19%), 27%) /* Demand Type      */
            init(hex(19)) str(line_map$(19%), 36%) /* BOM ID           */
            init(hex(1a)) str(line_map$(20%),  1%) /* Horizon          */

*        See if operator is an administrator or not
            call "CMSMACHK" ("BCK", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then admin% = 1%

*        Check if a Sales Order is in-process for this User.  If one
*        is and it is from another program, inform and abort.  If it
*        is from this program then reload and continue.
            readkey$ = all(hex(00))  :  str(readkey$,,3) = userid$
            call "READ101" (#9, readkey$, f1%(9%))
            if f1%(9%) = 0% then L09744       /* Nothing in Process       */
                so$ = key(#9, 2%)
                get #9, str(readkey$,,20%)
*                   FMT XX(10), CH(9)
                if str(readkey$,11%,8%) = "BCKONLIN" then L09738
                     pf$(1%) = "You did not complete processing Sales" & ~
                              " Order " & so$
                     pf$(2%) = "in the program " & str(readkey$,11%,8%) &~
                               "."
                     pf$(3%) = "Press (RETURN) to exit this program."
                     call "ASKUSER" (keyhit%, "IN-PROCESS MESSAGE",      ~
                                     pf$(1%), pf$(2%), pf$(3%))
                     goto exit_program
L09738:         call "SHOSTAT" ("RESTARTING...")
                get #9, str(readkey$,,20), str(cuscode$,,9), str(so$,,16)

L09744
*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            call "READ100" (#02, "SWITCHS.CUR", f1%(2))
            if f1%(2) <> 0% then get #02 using L09752 , curr$, statutory$, ~
                                                      currtype$
L09752:         FMT POS(21), CH(1), CH(4), POS(27), CH(1)
            if curr$ <> "Y" then statutory$, currtype$ = " "
            if curr$ <> "Y" then L09765
                call "OPENCHCK" (#40, 0%, f2%(40),   0%, " ")
                call "OPENCHCK" (#41, 0%, f2%(41), 400%, " ")
                call "OPENCHCK" (#42, 0%, f2%(42),   0%, " ")

L09765
*        Check if Precious Metal Surcharge is on
            pm_on$, pm_sys_so$, pm_sys_inv$ = "N"
            call "READ100" (#02, "SWITCHS.BCK", f1%(02%))
            if f1%(02%) = 1% then get #02 using L09790, pm_on$,            ~
                                                  pm_sys_so$, pm_sys_inv$
L09790:         FMT POS(60), 3*CH(1)
            pm_hdr_dsply$, pm_hdr_dsply2$ = " "
            if pm_on$ <> "Y" then  L09820
                  pm_hdr_dsply$  = "PM Surcharge at SO?"
                  pm_hdr_dsply2$ = "at INV?"

L09820
*        See if CANREASON files in place
            can$ = "N"
            readkey$ = "CANREASON"
            call "PLOWNEXT" (#08, readkey$, 9%, f1%(8))
              if f1%(8) = 1% then can$ = "Y"

            str(line1$,52)= date$ & "  BCKONLIN: " & str(cms2v$,,8)
            if f1%(9) = 0% then L10000      /* Nothing in Process       */
            gosub restart_reload
            call "READ100" (#05, str(cuscode$,,9) & str(so$,,16),        ~
                soonfile%)
            goto summary_screen

L10000: REM *************************************************************~
            *       I N P U T   M O D E   -  H E A D E R                *~
            *-----------------------------------------------------------*~
            * Handles input for abbreviated Header Screen.              *~
            *************************************************************
        inputmode
            gosub init_for_input

            mostin% = 0%
            for fieldnr% = 1% to 13%     /* First Screen               */
                gosub L20000
                s% = 1% : f% = fieldnr% : edit% = 1% : gosub L28000
                      if enabled% = 0% then L10260
L10130:         edit% = 1% : gosub L40000
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10240
                         errormsg$ = " "
                         if fieldnr% <= 2% then L10240
L10190:                  fieldnr% = max(3%, fieldnr% - 1%)
                         s% = 1% : f% = fieldnr% : edit% = 1% :gosub L28000
                         if enabled% = 1% or fieldnr% = 3% then L10130
                         goto L10190
L10240:               if keyhit%  = 16 and fieldnr% = 1% then exit_program
                      if keyhit%  = 22% or keyhit% = 23% then L10260
                      if keyhit% <>  0 then       L10130
L10260:         edit% = 1% : gosub L50000
                      if errormsg$ <> " " then L10130
                      if fieldnr% = 1% and so$ <> " " then fieldnr% = 2%
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

*        Do Input for Variable Fields
            mode$ = "I"
            gosub variable_fields


            goto appendlines

        REM *************************************************************~
            *             S U M M A R Y   S C R E E N                   *~
            * --------------------------------------------------------- *~
            * Summary Display and Main Control Screen.                  *~
            *************************************************************

        summary_screen

        s% = 1% : f% = 0% : edit% = 2% : gosub L28000
        lastfieldnr% = 0%
L11100: fieldnr% = 0% : edit% = 2% : gosub L40000
            gosub call_screen
            mls% = maxlines%
            errormsg$ = " "
            if keyhit%  =  2 then top% = 1%
            if keyhit%  =  3 then top% = mls%-9%
            if keyhit%  =  4 then top% = top%-9%
            if keyhit%  =  5 then top% = min(mls%-9%, top%+9%)
            if keyhit%  =  6 then top% = top%-1%
            if keyhit%  =  7 then top% = min(mls%-9%, top%+1%)
                                  top% = max( 1%, top%)
            if keyhit%  =  9 then       header_detail
            if keyhit%  = 10 then       cancel_order
            if keyhit%  = 11 then       appendlines
            if keyhit%  = 12 then gosub delete_order
            if keyhit% <> 14 then gosub L11300
                reviewpart$ = " "
                c% = top% + cursor%(1) - 11%
                if c% < 1% or c% > maxlines% then L11280
                     reviewpart$ = part$(c%)
L11280:         gosub review_functions
                goto  summary_screen
L11300:     if keyhit%  = 16 then       datasave
            if keyhit%  = 18% then gosub show_part_master
            if keyhit%  = 25 then gosub edit_text_hdr
            if keyhit%  = 29 then       L11430
            if keyhit% <>  0 then       summary_screen
        if cursor%(1) < 11% then L11430
            c% = top% + cursor%(1) - 11%
            if c% < 1% or c% > maxlines% then summary_screen
            if str(lsts$(c%),,3) = "DEL" then summary_screen
                savetop% = top%
                top% = max(1%, c% - 1%)
                gosub edit_line_item
                top% = savetop%
                goto summary_screen
L11430: fieldnr% = val(str(hdr_map$(cursor%(1)), cursor%(2)))
            if fieldnr% < 3% or  fieldnr% > 13% then summary_screen
            if fieldnr%  = lastfieldnr% then summary_screen
        if keyhit% <> 29% then L11490
            s% = 1% : f% = fieldnr% : gosub L27490
            goto summary_screen
L11490: s% = 1% : f% = fieldnr% : edit% = 2% : gosub L28000
            if enabled% = 0% then       summary_screen
L11510: edit% = 2% : gosub L40000
            gosub call_screen
            if keyhit% <>  0 then L11510
            edit% = 2% : gosub L50000
            if errormsg$ <> " " then L11510
                lastfieldnr% = fieldnr%
                goto L11430

        delete_order
            if maxlines% > 0% then L11630
                errormsg$ = "No lines to delete."
                return
L11630:     u3% = 2%
            call "ASKUSER" (u3%, "DELETE ORDER",                         ~
                "Press PF-16 to DELETE the entire Sales Order",          ~
                "-OR- Press (RETURN) to EXIT Delete Function",           ~
          "Note: Scheduled, Shipped, Invoiced Lines will not be deleted.")
            if u3% <> 16% then return
                left% = 0%
                for c% = 1% to maxlines%
                     if str(lsts$(c%),,3) = "DEL" then L11780
                     if qtyschld(c%) = 0 then L11741
L11730:                   left% = left% + 1%
                          goto L11780
L11741:              if ship  (c%) = 0 then L11742 else L11730
L11742:              if preinv(c%) = 0 then L11750 else L11730
L11750:              str(lsts$(c%),,3) = "DEL"
                     edit% = 1%          /* No Describe     */
                     gosub save_line     /* Slight Misnomer */
L11780:         next c%
                if left% <> 0% then errormsg$ =                          ~
         "Note: Scheduled, Shipped, Invoiced Lines will not be deleted."
                if left% <> 0% then return
                     return clear
                     goto datasave

        cancel_order
            errormsg$ = " "
             if oldflag% = 1% then L11835
                 errormsg$ = "Order has NOT been Saved; cannot be Cance"&~
                             "lled."
                   goto L11100
L11835:      if crhold$ <> "C" then L11850
                 errormsg$ = "Order already cancelled"
                   goto L11100
L11850:      if maxlines% = 0% then  L11895
               for c% = 1 to maxlines%
                 if ship(c%) = 0 then L11870
                     goto cancel_lines
L11870:          if qtyschld(c%) = 0 then L11880
                     goto cancel_lines
L11880:          if preinv(c%)   = 0 then L11890
                     goto cancel_lines
L11890:        next c%
L11895:      u3% = 2%
             call "ASKUSER" (u3%, " ** CANCEL ORDER ** ",                ~
                             "Press PF10 to cancel order", "- or -",     ~
                             "PF16 to abort cancellation.")
             if u3% <> 16% and u3% <> 10% then L11895
             if u3% = 16% then L11100
             crhold$ = "C"
               for c% = 1 to maxlines%
                 openqty(c%) = 0
               next c%
             goto datasave

        cancel_lines
L11952:      u3% = 2%
             call "ASKUSER" (u3%, " ** CANCEL LINES ** ",                ~
                             "Some Line Items have open BOLs or have " & ~
                             "been Shipped & Invoiced.",                 ~
                             "Press PF10 to set the Open Quantity to " & ~
                             "the BOL Quantities", "- or -  Press PF16"& ~
                             " to abort cancellation.")
             if u3% <> 10% and u3% <> 16% then L11952
             if u3% = 16% then L11100
               for c% = 1 to maxlines%
                 openqty(c%) = qtyschld(c%) + preinv(c%)
               next c%
             goto datasave

        REM *************************************************************~
            *        H E A D E R   S C R E E N  -  D E T A I L          *~
            * --------------------------------------------------------- *~
            * Full Detail Header Screen.                                *~
            *************************************************************

        header_detail

            s% = 2% : f% = 0% : edit% = 2% : gosub L28000
            lastfieldnr% = 0%
            fieldnr% = 0% : edit% = 2% : gosub L41000
            gosub call_screen
               errormsg$ = " "
                  if keyhit%  =  9 then mode$ = "E"
                  if keyhit%  =  9 then gosub variable_fields
                  if keyhit% <> 14 then       L12160
                          reviewpart$ = part$(c%)
                          gosub review_functions
                          goto  header_detail
L12160:           if keyhit%  = 16 then       summary_screen
                  if keyhit%  = 25 then gosub edit_text_hdr
                  if keyhit%  = 29 then       L12200
                  if keyhit% <>  0 then       header_detail
L12200:     fieldnr% = val(str(hdr_map$(cursor%(1)), cursor%(2)))
                if fieldnr% < 3% or  fieldnr% > 23% then header_detail
                if fieldnr%  = lastfieldnr% then header_detail
            if keyhit% <> 29% then L12260
                s% = 1% : f% = fieldnr% : gosub L27490
                goto header_detail
L12260:     s% = 2% : f% = fieldnr% : edit% = 2% : gosub L28000
                  if enabled% = 0% then       header_detail
L12280:     edit% = 2% : gosub L41000
            gosub call_screen
                  if keyhit% <>  0 then L12280
            edit% = 2% : gosub L50000
                  if errormsg$ <> " " then L12280
                          lastfieldnr% = fieldnr%
                          goto L12200

        variable_fields
            call "VFINPSUB" ("BCKMASTR", mode$, "On-Line Order Entry   ",~
                             "Customer: " & cuscode$ & "  SO: " & so$,   ~
                             "NN", vf$, u3%)
            return

        REM *************************************************************~
            *              L I N E   I T E M  S C R E E N S             *~
            * --------------------------------------------------------- *~
            * Routines for appending and modifying Line Items.          *~
            *************************************************************

        appendlines
L13070:     if maxlines%  = 100% then L13180
            if lastseq%  >= 999% then L13180
            if maxlines% + total_e_lines% <  100% then L13090
               gosub extra_load_conflict

L13090:         c%   = maxlines% + 1%
                top% = max(1%, c% - 2%)
                gosub clear_line
                lsts$(c%) = "ADD"
                convert lastseq% + 1% to seq$(c%), pic(###)
                gosub inputline
                     if keyhit% <> 16% then L13070
*          END_APPEND
                gosub clear_line
L13180:         top% = max(1%, min(91%,maxlines%-9%))  /* Last Screen   */
                goto summary_screen

        inputline
            clear%  = 1%       /* For Options Entry */
            mostin% = 0%
            for fieldnr% = 1% to 26%     /* First Line Item Screen     */
                if fieldnr% > mostin% then gosub L22000
                s% = 3% : f% = fieldnr% : edit% = 1% : gosub L28000
                      if enabled% = 0 then L13410
L13280:         edit% = 1% : gosub L42000
                      if keyhit%  =  1 then gosub restart_line
                      if keyhit% <>  4 then       L13360
                         errormsg$ = " "
L13303:                  if fieldnr% <> 2% then L13310
                            total_e_lines% = total_e_lines% - e_lines%(c%)
                            e_lines%(c%) = 0%
L13310:                  fieldnr% = max(1%, fieldnr% - 1%)
                         s% = 3% : f% = fieldnr% : edit% = 1% :gosub L28000
                         if enabled% <> 0% then L13280
                         if fieldnr% =  1% then L13280
                         goto L13303
L13360:               if keyhit% <> 14 then L13390
                         reviewpart$ = part$(c%)
                         gosub review_functions
L13390:               if keyhit%  = 16 and fieldnr% = 1% then return
                      if keyhit%  = 18 then gosub show_part_master
                      if keyhit%  = 22% or keyhit% = 23% then L13410
                      if keyhit% <>  0 then       L13280
L13410:         edit% = 1% : gosub L52000
                      if errormsg$ <> " " then L13280
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

            maxlines% = maxlines% + 1%     /* Officially welcome line  */
            lastseq%  = lastseq%  + 1%     /* to the Sales Order       */
            convert lastseq% to seq$(c%), pic(###)
            edit%, append% = 1%
            gosub save_line
            gosub edit_line_item
            append% = 0%
            keyhit% = 99%
            return


        edit_line_item
            gosub describe_line

L13600:     s% = 3% : f% = 0% : edit% = 2% : gosub L28000
            lastfieldnr% = 0%
            fieldnr% = 0% : edit% = 2% : gosub L42000
            gosub call_screen
                  errormsg$   = " "
                  if keyhit%  =  6 then gosub prev_line
                  if keyhit%  =  7 then gosub next_line
                  if keyhit%  =  8 then gosub test_feasibility
*                IF KEYHIT%  = 24 THEN GOSUB TEST_FEASIBILITY
                  if keyhit%  =  9 then gosub how_many
                  if keyhit%  = 10 then gosub plan_one_level
*                IF KEYHIT%  = 26 THEN GOSUB PLAN_ONE_LEVEL
                  if keyhit%  = 11 then gosub plan_all_levels
                  if keyhit%  = 27 then gosub plan_all_levels
                  if keyhit%  = 12 then gosub delete_line
                  if keyhit% <> 14 then       L13780
                          reviewpart$ = part$(c%)
                          gosub review_functions
                          goto  L13600
L13780:           if keyhit%  = 16 then       return
                  if keyhit%  = 18 then gosub show_part_master
                  if keyhit%  = 25 then gosub edit_text_line
                  if keyhit%  = 29 then       L13820
                  if keyhit% <>  0 then       L13600
L13820:     fieldnr% = val(str(line_map$(cursor%(1)), cursor%(2)))
                if fieldnr% <> 1% then L13860
                     clear% = 0% :  gosub options
                     goto L13600
L13860:         if fieldnr% < 2% or fieldnr% > 26% then L13600
                if fieldnr% = lastfieldnr%         then L13600
            if keyhit% <> 29% then L13910
                s% = 2% : f% = fieldnr% : gosub L27490
                goto L13600
L13910:     s% = 3% : f% = fieldnr% : edit% = 2% : gosub L28000
                  if enabled% = 0% then L13600
L13930:     edit% = 2% : gosub L42000
            gosub call_screen
                  if keyhit% <>  0 then L13930
            edit% = 2% : gosub L52000
                  if errormsg$ <> " " then L13930
                     lastfieldnr% = fieldnr%
                     goto L13820


        prev_line
            if append% = 1% or c% = 1% then return
                nc% = c%        /* Find 1st Previous Active Line Item  */
L14040:         nc% = nc% - 1%
                if nc% = 0% then return
                if str(lsts$(nc%),,3) = "DEL" then L14040
                     goto L14160


        next_line
            if append% = 1% or c% = maxlines% then return
                nc% = c%       /* Find Next Active Line Item           */
L14130:         nc% = nc% + 1%
                if nc% > maxlines% then return
                if str(lsts$(nc%),,3) = "DEL" then L14130
L14160:              c%   = nc%
                     top% = max(1%, c% - 1%)
                     return clear
                     goto edit_line_item

        delete_line
            if allow_delete$ <> "Y" then return
            if qtyschld(c%) = 0 then L14231
              errormsg$ = "Can't delete a Scheduled Line Item"    : return
L14231:     if preinv(c%)   = 0 then L14233
              errormsg$ = "Can't delete a Pre-Invoiced Line Item" : return
L14233:     if ship  (c%)   = 0 then L14240
              errormsg$ = "Can't delete a Shipped Line Item"      : return
L14240:     u3% = 2%
            call "ASKUSER" (u3%, "DELETE LINE",                          ~
                            "Press PF-16 to DELETE this Line Item",      ~
                            "-OR-", "Press (RETURN) to abort Delete.")
            if u3% <> 16% then return
                str(lsts$(c%),,3) = "DEL"
                edit% = 1%  :  gosub save_line
                return clear
                return


        how_many
            errormsg$  = " "
            mbom$      = " "
            mdate$     = shipdate$(c%)
            mpart$     = part$(c%)
            mpriority$ = priority$(c%)
            mquantity$ = order$

            call "PLNMAXIM"                                              ~
                    (errormsg$,              /* Error Message          */~
                     mbom$,                  /* Bill of Materials      */~
                     mdate$,                 /* Due Date               */~
                     mpart$,                 /* Part                   */~
                     mpriority$,             /* Priority               */~
                     mquantity$,             /* Maximum Quantity       */~
                     today%,                 /* Index for Today        */~
                     #04,  #36,              /* HNYMASTR, HNYALTRS     */~
                     #16,  #24,              /* BOMMASTR, PIPMASTR     */~
                     #26,  #34,              /* RTEMASTR, JBCROSS2     */~
                     #27,  #28,              /* WCMASTR , WCOUT        */~
                     #29,  #30,              /* PIPIN   , PIPOUT       */~
                     #32,  #33,              /* SFMASTR2, SFCUM2       */~
                     #31,  #20,              /* PIPCROSS, ENGMASTR     */~
                     #17,  #35,              /* BOMSPEC , JBPIPXRF     */~
                     #18,                    /* RTEALTRS               */~
                     #61,  #62,              /* Work Files             */~
                     #23,  #25,              /* DEMMASTR, HNYDETAL     */~
                     #19      )              /* CALMASTR               */
            return


        test_feasibility
            planflag% = 0%
            goto plansub

        plan_one_level
            str(planflags$(),46,6) = hex(ffffffffffff)

        plan_all_levels
            planflag% = 1%

        plansub
*        Check that there is a Demand
            plowkey$ = str(so$,,16) & seq$(c%)
            call "REDALT0" (#23, plowkey$, 1%, f1%(23))
            if f1%(23) = 0% then return
                get #23 using L14850, temp1$, temp$
L14850:              FMT CH(1), XX(52), CH(10)
                if str(temp1$,1,1) > "1" then return  /* Planned */
                convert temp$ to demqty, data goto L14880 : goto L14900
L14880:         return

L14900:     temp$ = shipdate$(c%)
            call "DATUNFMT" (temp$)
            call "PIPINDEX" (#02, str(temp$,,6), pcd%, err%)
            if err% <> 0% then return

            if planflag% = 0% then L15030           /* Feasibility Test */
                ltss% = val(priority$(c%),1) - 64%
                if ltss% < 1% or ltss% > 26%    then L15030
                if pcd% - lta%(ltss%) <= today% then L15030
                     errormsg$ = "Not within Planning Horizon for" &     ~
                                 " this Priority"
                     return

L15030:     rte$ = " "
            if keyhit% > 16% then planflag% = planflag% + 10%
            call "PLANSUB"                                               ~
                    (so$,                /* Demand Code                */~
                     seq$(c%),           /* Demand Line                */~
                     demtype$(c%),       /* Demand Type                */~
                     priority$(c%),      /* Demand Priority            */~
                     part$(c%),          /* Part Needed                */~
                     demqty,             /* Quantity Needed            */~
                     pcd%,               /* Req'd Compl Date           */~
                     store$,             /* Deliver to Warehouse       */~
                     rte$,               /* The WC Route to Use        */~
                     bomid$,             /* Which BOM to Use           */~
                     errormsg$,          /* The Return Message         */~
                     u3%,                /* First Date Passed Back     */~
                     finaldate%,         /* Date Planned For           */~
                     today%,             /* Subscript for Today's Date */~
                     planflag%,          /* 1 = Plan, 0 = Check Only   */~
                     #36,                /* HNYALTRS                   */~
                     #16,                /* BOMMASTR                   */~
                     #24,                /* PIPMASTR                   */~
                     #26,                /* RTEMASTR                   */~
                     #34,                /* JBCROSS2                   */~
                     #27,                /* WCMASTR                    */~
                     #28,                /* WCOUT                      */~
                     #29,                /* PIPIN                      */~
                     #30,                /* PIPOUT                     */~
                     #32,                /* SFMASTR2                   */~
                     #33,                /* SFCUM2                     */~
                     #31,                /* PIPCROSS                   */~
                     #20,                /* ENGMASTR                   */~
                     #17,                /* BOMSPEC                    */~
                     #35,                /* JBPIPXRF                   */~
                     #18,                /* RTEALTRS                   */~
                     #61, #62)           /* Work Files                 */

            mat planflags$ = holdflags$

            if finaldate% < 0% or finaldate% > 490% then return

            if planflag% = 0% then L15630

*        Update the Demand with the new Info
            call "REDALT1" (#23, plowkey$, 1%, f1%(23))
            if f1%(23) = 0% then return
            put #23 using L15490, date, yymmdd$(finaldate%)
L15490:         FMT POS(77), CH(6), CH(6)

            if yymmdd$(finaldate%) > str(temp$,,6) then L15540
            if crhold$ <> "H" then demstatus$ = "9" else demstatus$ = "8"
            goto L15560
L15540:     if crhold$ <> "H" then demstatus$ = "7" else demstatus$ = "6"

L15560:     put #23 using L15570, demstatus$
L15570:         FMT POS(1), CH(1)
            rewrite #23
            gosub describe_line
            return


L15630:     errormsg$ = "Feas. test done. Dem. may be met."
            temp$ = yymmdd$(finaldate%)
            call "DATEFMT" (temp$)
            errormsg$ = errormsg$ & " " & temp$
            return

        show_part_master
            if part$(c%) = " " then return
            call "HNYMSTSB" (part$(c%), #02/* SYSFILE2 */,               ~
                #04/* HNYMASTR */, #11/* CATEGORY */, #03/* GLMAIN   */, ~
                #36/* HNYALTRS */, #47/* VENDOR   */, #46/* HNYPROC  */, ~
                #48/* VENPRICE */, #15/* HNYGENER */, #24/* PIPMASTR */, ~
                #20/* ENGMASTR */, #29/* PIPIN    */, #30/* PIPOUT   */, ~
                #13/* HNYQUAN  */, #33/* SFCUM2   */, #22/* TXTFILE  */, ~
                #08/* GENCODES */, #16/* BOMMASTR */)
            return

        REM *************************************************************~
            *          M I S C   S U P P O R T   R O U T I N E S        *~
            * --------------------------------------------------------- *~
            * Routines which apply to most of the screen functions.     *~
            *************************************************************

        text_prompt
            fac25$ = hex(84) /* Highlight PF(25)Manage Text */
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then fac25$ = hex(8c) /* Dim PF(25)Manage Text */
            return

        edit_text_hdr
            call "TXTINSUB" (#22, f2%(22), "013", str(line1$,,19),       ~
                                                       textid$, text$())
            return


        edit_text_line
            call "TXTINSUB" (#22, f2%(22), "014", str(line1$,,19),       ~
                                                  textidl$(c%), text$())
            return


        describe_line     /* Add descriptors to already existing line  */
            call "CONVERT" (order   (c%), 2.2, order$   )
            call "CONVERT" (openqty (c%), 2.2, openqty$ )
            call "CONVERT" (ship    (c%), 2.2, ship$    )
            call "CONVERT" (conv    (c%), 7.7, conv$    )
            call "CONVERT" (qtyschld(c%), 2.2, qtyschld$)
            call "CONVERT" (alloc   (c%), 2.2, allocqty$)
            call "CONVERT" (preinv  (c%), 2.2, preinv$  )
            gosub pricing_stuff
            gosub check_for_options

            oldalloc    = alloc(c%)
            oldalloc$   = alloc$(c%)
            oldorder    = order(c%)
            oldopenqty  = openqty(c%)
            olddemtype$ = demtype$(c%)

            gosub get_req_date     : avail = 0
            tempallc = openqty(c%) : gosub planallc

         /* Load and Describe the Demand for this Line Item            */
            pcd$, lastplan$, horizon$, bomid$, demstatus$ = " "
            if crhold$ <> "H" then demstatus$ = "1"
            demstatusmsg$ = "No Demand on File"
            readkey$ = str(so$) & str(seq$(c%))
            call "REDALT0" (#23, readkey$, 1%, demand%)
            if demand% = 0% then return
                get #23 using L16480, demstatus$, bomid$, lastplan$, pcd$
L16480:              FMT CH(1), POS(68), CH(3), POS(77), 2*CH(6)
                call "DATEFMT" (lastplan$)
                call "DATEFMT" (pcd$     )
                oldbomid$ = bomid$

              /* Determine Planning Horizon Date                       */
                temp$ = shipdate$(c%) : call "DATUNFMT" (temp$)
                call "PIPINDEX" (#02, str(temp$,,6), req%, err%)
                if err% = 0% then L16590
                     warn$ = "Warning: Ship Date Error" : goto L16680
L16590:         hz% = today% : req% = req% + 1%
                ltss% = val(priority$(c%),1) - 64%
                if ltss% < 1% or ltss% > 26% then L16630
                     hz% = req% - lta%(ltss%)
L16630:         hz% = max(today%, min(490%, hz%))
                horizon$ = yymmdd$(hz%)
                call "DATEFMT" (horizon$)

              /* Determine Demand Status                               */
L16680:         on pos(" 16789" = demstatus$) goto L16710, L16720, L16730,  ~
                                                   L16740, L16750, L16760
                   demstatusmsg$ = "Unknown Status        " : return
L16710:            demstatusmsg$ = "Unplanned, Unapproved " : return
L16720:            demstatusmsg$ = "Unplanned, Approved   " : return
L16730:            demstatusmsg$ = "Planned Late, Unapprvd" : return
L16740:            demstatusmsg$ = "Planned Late, Approved" : return
L16750:            demstatusmsg$ = "Planned OK, Unapproved" : return
L16760:            demstatusmsg$ = "Planned OK, Approved  " : return

        call_customer_credit
            if cuscode$ = " " then return
            call "ARQCUSCR" (cuscode$)
            return

        pricing_stuff
*        Calculates dependant variables (for line C%) and formats
*        the fields related to pricing of the line item.
            gosub adj_for_pm_surcharge  /*If PM effected part adjust the*/
                                    /* Price for PM Surcharge as Needed */
            pricestk(c%) = round(price(c%) / conv(c%)      , 4)
            ext          = round(price(c%) * openqty(c%) / conv(c%), 2)
            discamt      = round(ext * linedisc(c%) *.01   , 2)
            ext          = round(ext - discamt             , 2)
            call "CONVERT" (price(c%)   , 4.4, price$   )
            call "CONVERT" (linedisc(c%), 2.2, linedisc$)
            call "CONVERT" (ext         , 2.2, ext$     )
            call "CONVERT" (discamt     , 2.2, discamt$ )
            call "CONVERT" (pricestk(c%), 4.4, pricestk$)
            return

        get_req_date
            call "DATUNFMT" (shipdate$(c%))
            oldshipdate$ = shipdate$(c%)
            call "DATE" addr("GD", str(shipdate$(c%),,6), weekday$, u3%)
            call "PIPINDEX" (#2, shipdate$(c%), req%, u3%)
            call "DATEFMT"  (shipdate$(c%))
            return

        clear_line   /* Clear variables for line C%                    */
            part$(c%), descr$(c%), item$(c%), cat$(c%), order$, openqty$,~
            ship$, stkuom$(c%), priceuom$(c%), linedisc$, taxable$(c%),  ~
            nonstockmsg$(c%), pricestk$, discamt$, ext$, duedate$(c%),   ~
            shipdate$(c%), origdue$(c%), priority$(c%), demtype$(c%),    ~
            salesacct$(c%), discacctl$(c%), lot$(c%), oldbomid$, bomid$, ~
            conv$, price$, weekday$, errormsg$, qtyschld$, project$(c%), ~
            lsts$(c%), allocqty$, preinv$, seq$(c%), avail$, alloc$(c%), ~
            olddemtype$, oldshipdate$, horizon$, filler$(c%), pcd$,      ~
            lastplan$, demstatus$, demstatusmsg$, refpart$(c%),          ~
            refdesc$(c%),reftype$(c%),mfgcode$, shipcode$(c%), ptype$(c%)~
                                                               = " "

            order(c%), openqty(c%), ship(c%), price(c%), qtyschld(c%),   ~
            pricestk(c%), linedisc(c%), alloc(c%), preinv(c%), avail,    ~
            oldalloc, oldorder, oldopenqty, pm_base(c%) = 0
            conv(c%) = 1
            pm_adj%(c%) = 0%
            mat opdlrs = con
            mat opdlrs = (-1) * opdlrs

            textidl$(c%) = all(hex(ff))

            total_e_lines% = total_e_lines% - e_lines%(c%)
            e_lines%(c%) = 0%

            return


        options /* Call Options Processing Subroutine                  */
            if ptype$(c%) <> "000" then return
            gosub assign_so_number

            get #01 using L17200, opc$,   custype$
L17200:        FMT POS(734), CH(1), POS(1023), CH(2)
            opdlrs(3) = price(c%)
            if opc$ = " " then opc$ = pc$
            call "BOMOPSUB"                                              ~
                    (clear%,             /* Clear B4 Input (0=N 1= Y)  */~
                     part$(c%),          /* Part Number                */~
                     str(so$) & str(seq$(c%)),     /* Demand Code      */~
                     shipdate$(c%),      /* Date Required              */~
                     bomid$,             /* BOM ID                     */~
                     cuscode$,           /* Customer                   */~
                     custype$,           /* Customer Type              */~
                     cat$(c%),           /* Part Category              */~
                     opc$,               /* Price Code                 */~
                     orderdate$,         /* Order Date                 */~
                     opdlrs(),           /* Cost, Prices Info          */~
                     #04,                /* 'HNYMASTR'                 */~
                     #16,                /* 'BOMMASTR'                 */~
                     #17,                /* 'BOMSPEC'                  */~
                     #02,                /* 'SYSFILE2'                 */~
                     #20,                /* 'ENGMASTR'                 */~
                     #49,                /* 'BOMSPHDR'                 */~
                     u3%)                /* 0 = ALL OK, 1 = ERROR      */
            clear% = 0%
            price(c%) = opdlrs(4)
            gosub pricing_stuff
            if u3% <> 0 then bomid$ = " "
            gosub check_for_options
            return

        check_for_options
            str(lsts$(c%),6) = " "
            readkey$ = all(hex(00))
            str(readkey$,,19) = str(so$) & seq$(c%)
            call "PLOWALTS" (#17, readkey$, 1%, 19%, f1%(17))
            if f1%(17) = 1% then str(lsts$(c%),6) = "O"
            return

        set_summary
*        Defines Contents and FACs for Summary Window.  WNDW% defines
*        length.
            init(" ") smry$()
            if wndw% = 3% then init(hex(8c)) sfac$() else                ~
                               init(hex(86)) sfac$()
            top% = max(1%, min(top%, 91%))
            if wndw% = 3% then sfac$(c% - top% + 1%) = hex(84)
            sfac$(wndw%) = or hex(20)

            for s% = 1%  to  wndw%
              s1% = top% + s% - 1%       /* Line Number */
              if seq$(s1%) = " " then L17740
                str(smry$(s%), 1) = seq$  (s1%)
                if cms% = 1% then str(smry$(s%),5%) = part$ (s1%) else   ~
                                  str(smry$(s%),5%) = refpart$ (s1%)
                call "CONVERT" (order(s1%)  , 2.2, str(smry$(s%),31,10))
                call "CONVERT" (openqty(s1%), 2.2, str(smry$(s%),42,10))
                  if conv(s1%) = 0 then L17680
                temp  = round(openqty(s1%) * price(s1%) / conv(s1%), 2)
                  goto L17690
L17680:         temp  = round(openqty(s1%) * pricestk(s1%), 2)
L17690:         temp1 = round(temp * linedisc(s1%) * .01  , 2)
                temp  = round(temp - temp1                , 2)
                call "CONVERT" (temp        , 2.2, str(smry$(s%),53,10))
                str(smry$(s%),64) = shipdate$(s1%)
                str(smry$(s%),73) = lsts$(s1%)
L17740:     next s%

            return


        review_functions
            mode% = 6%
            call "PLNRSUB" (mode%,                                       ~
                            reviewpart$,                                 ~
                            #23,                   /* DEMMASTR         */~
                            #24,                   /* PIPMASTR         */~
                            #04,                   /* HNYMASTR         */~
                            #26,                   /* RTEMASTR         */~
                            #27,                   /* WCMASTR          */~
                            #19,                   /* CALMASTR         */~
                            #16,                   /* BOMMASTR         */~
                            #28,                   /* WCOUT            */~
                            #20,                   /* ENGMASTR         */~
                            #29,                   /* PIPIN            */~
                            #30,                   /* PIPOUT           */~
                            #31,                   /* PIPCROSS         */~
                            #32,                   /* SFMASTR2         */~
                            #33,                   /* SFCUM2           */~
                            #25,                   /* HNYDETAL         */~
                            #18,                   /* RTEALTRS         */~
                            #36)                   /* HNYALTRS         */
            return


        planallc     /* Determine Quantity AVAIL for allocation        */
          err% = req%
          call "PLANALLC"                                                ~
                       (part$(c%),       /* Part to Allocate           */~
                        tempallc,        /* Quantity Requested         */~
                        avail,           /* Quantity Allocatable (IN)  */~
                                         /* Prev Alloc Cmplete Qty(OUT)*/~
                        demtype$(c%),    /* Demand Type                */~
                        priority$(c%),   /* Priority                   */~
                        req%,            /* Date Required              */~
                        today%,          /* Today's Date               */~
                        holdflags$(),    /* Planning System Flags      */~
                        #24,             /* PIPMASTR                   */~
                        #33,             /* SFCUM2                     */~
                        err%)            /* Return Code   (IN)         */
                                         /* Old ship date (OUT)        */
            call "CONVERT" (avail, 2.2, avail$)
            return

        adj_for_pm_surcharge        /* If PM effected part adjust the   */
                                    /* Price for PM Surcharge as Needed */
            if pm_on$ <> "Y" then return
            if pm_so$ <> "Y" then return
            if abs(pm_base(c%) - price(c%)) < 0.0001 then return

            s_charge = 0
            qty = openqty(c%)
            call "PMCALSUB" (cuscode$, part$(c%),qty,0,1,"C", orderdate$,~
                             s_charge, " ", " ", pm_so$, pm_inv$, rslt%)

            if rslt% = 0% then return  /*No PM Associated with this Part*/

            /* Add the precious metal surcharge to the price */
            if qty = 0 then return   /* No PM set for zero qty */
                price(c%) = price(c%) + s_charge * conv(c%) / qty
                pm_adj%(c%) = 1%
                pm_base(c%) = price(c%)

                return

        display_pm_surcharges

            qty = openqty(c%)
            call "PMCALSUB" (cuscode$, part$(c%),qty,0,1,"D", orderdate$,~
                             s_charge, " ", " ", pm_so$, pm_inv$, rslt%)
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * 1- Sum up Order and see if it is on Credit Hold.          *~
            *    ... Credit information is gathered by s/r BCKCRDSB.    *~
            * 2- Get any change data required.                          *~
            * 3- Get final save approval.                               *~
            *************************************************************

        datasave
            curtemp3 = 0
            curtemp4 = 0
*        FIRST add up the order and determine if it is on credit hold.
            mat torder  = zer
            mat topen   = zer
            mat tlines% = zer
            inpmessage$ = " "
            if crhold$ <> "C" then L19130
              adjmsg$ = "Cancellation Reason Code"
                goto L19150
L19130:       adjmsg$ = "Adjustment Reason Code"

L19150
*         Read in Bill-to (even if same as ship-to).
            get #01 using L19170, billto$
L19170:         FMT XX(779), CH(9)
            if billto$ = " " then L19191
L19180:     call "READ100" (#01, billto$, f1%(1))
            if f1%(1) <> 0% then L19200
L19191:         u3% = 2%
                call "ASKUSER" (u3%, "*** MISSING BILL-TO CUSTOMER ***", ~
                     "There is no Bill-to Customer (" &billto$& ") for:",~
                     "Ship-to Customer " &cuscode$& " (" & shipto$(1) &  ~
                     ")", "Press (RETURN) to acknowledge & continue")
            billto$ = str(cuscode$)
            goto L19180

L19200
*        Call BCKCRDSB to compute Credit Limit, A/R & Open Order amounts.
*        It does this via the "Credit Parent" logic.
            par% = 0%        /* Not used in this program */
            call "BCKCRDSB" (billto$, oo(), ar(), crlimit, par%)

*         Add up the order we are working on
            tqty = 0 /* Adding apples & oranges just to see if anything */
                     /* if open for shipping.  Used in credit hold.     */
            if maxlines% = 0% then L19510
             for c% = 1% to maxlines%
                if str(lsts$(c%),,3) <> "DEL"                            ~
                                       then tlines%(1) = tlines%(1) + 1% ~
                                       else tlines%(2) = tlines%(2) + 1%
                if str(lsts$(c%),,3)  = "DEL" then L19400
                ext       = round(order(c%) * price(c%) / conv(c%), 2)
                tqty      = tqty + order(c%)
                tdisc     = round(ext * linedisc(c%) *.01, 2)
                torder(1) = torder(1) + ext
                torder(2) = torder(2) - tdisc

                ext       = round(openqty(c%) * price(c%) / conv(c%), 2)
                tdisc     = round(ext * linedisc(c%) *.01, 2)
                topen (1) = topen (1) + ext
                topen (2) = topen (2) - tdisc
L19400:     next c%
            torder(4) = torder(1) + torder(2)
            torder(3) = round(torder(4) * orderdisc * .01, 2)
            torder(3) = 0 - torder(3)
            torder(4) = torder(4) + torder(3)

            topen (4) = topen (1) + topen (2)
            topen (3) = round(topen (4) * orderdisc * .01, 2)
            topen (3) = 0 - topen (3)
            topen (4) = topen (4) + topen (3)

            oo(1) = oo(1) - origopen + (topen(4) * conveqv)
L19510:     oo(2) = oo(2) - origopen + (topen(4) * conveqv)
            totalar1 = ar(1) + oo(1)
            if cr_ar_only$ <> "Y" then totalar2 = ar(2%) + oo(2%)        ~
                                  else totalar2 = ar(2%)

*        Credit Hold Options are: (Note: CRHOLD$ for Hold or Cancelled)
*          'N' - No Automatic Hold; CRHOLD$ & HOW_HELD$ stay the same
*          'Y' - Automatic Hold, no override if Held or Released Manually
*          'O' - Automatic Hold, override of Manual Setting

            crmsg$() = " "
            if crhold$ <> "C" then L19531
                how_held$ = " " : goto L19590
L19531:     if crflag$ <> "O" then L19533
L19532:         crhold$, how_held$ = " " : goto L19538
L19533:     if crflag$ = "N" then L19590    /* Flags stay 'as are'. */
            if how_held$ <> "M" then L19532 /*To get this far CRFLAG$='Y'*/
                if crhold$ = "H" then                                    ~
                    crmsg$(1) = "*'Manually' Held Order*"
                goto L19590
L19538:     if tqty = 0 and topen(4) = 0 then L19590 /* Nothing there */
            if totalar2 > crlimit        then L19550 /* Over Cr Limit */

            call "BCKDAYSB" (billto$, u3%)
            if u3% = 0% then goto L19590

                crmsg$(1) = "* # Days Late Exceeded*" : goto L19560
L19550:         crmsg$(1) = "*Credit Limit Exceeded*"
L19560:         crmsg$(2) = "*Order on Credit Hold.*"
                crhold$   = "H"       /* S. O. is on credit hold */

L19590
*        NOW, Display screen.
            pickprint$ = " "
                if dfltpick$ = "Y" then pickprint$(2%) = "X"
                if dfltbol$  = "Y" then pickprint$(3%) = "X"
                if dfltacks$ <>"N" then pickprint$(1%) = "X"
                gosub encode_pickprint

            if modfld% = 1% and manadjre$ = "Y" then goto L19775
            if crhold$ <> "C" then L19780
L19775:         fieldnr% = 1% : goto L19890
L19780:     fieldnr% = 0% : edit% = 2% : gosub L45000
            gosub call_screen
                  if keyhit%  =  1 then       exit_datasave
                  if keyhit%  = 16 then       save_sales_order
                  if keyhit% <>  0 then       L19780
L19840:     fieldnr% = cursor%(1) - 9%
                if fieldnr% <  1% or  fieldnr%  > 2%  then L19780
                if fieldnr% =  1% and soonfile% = 0%  then L19780
                if fieldnr% <> lastfieldnr% then L19890
                lastfieldnr% = 0% : goto L19780
L19890:     edit% = 2% : gosub L45000
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L19890
            gosub L55000                 /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L19890
                          lastfieldnr% = fieldnr%
                          goto L19840

        exit_datasave     /* Branch back to editing of order */
            call "READ100" (#01, cuscode$, f1%(1))
            goto summary_screen

L20000: REM *************************************************************~
            *     D E F A U L T S               F O R   P A G E   1     *~
            *************************************************************

            if fieldnr% = 10% then L20130
            if fieldnr% = 11% then L20220
            return

L20130
*        Default Due Date                      DFLTDUE$
                if offset_due$ = " " then return
                     tempdate$ = orderdate$
                     call "DATUNFMT" (tempdate$)
                     call "DATE" addr ("G+", tempdate$, offsetd%,        ~
                                             dfltdue$, u3%)
                     call "DATEFMT" (dfltdue$)
            return

L20220
*        Default Req'd Ship Date               DFLTSHIP$
                if offset_shp$ = " " or ~
                      dfltdue$ = " " or dfltdue$ = blankdate$ then return
                     tempdate$ = dfltdue$
                     call "DATUNFMT" (tempdate$)
                     call "DATE" addr ("G+", tempdate$, -offsets%,       ~
                                             dfltship$, u3%)
                     call "DATEFMT" (dfltship$)

            return

L22000: REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

            on fieldnr%       gosub L22330,         /* Part Code        */~
                                    L22360,         /* Part Description */~
                                    L22420,         /* Part Category    */~
                                    L22480,         /* Due Date         */~
                                    L22520,         /* Required Ship    */~
                                    L22560,         /* Stocking UOM     */~
                                    L22790,         /* Pricing UOM      */~
                                    L22850,         /* Conversion       */~
                                    L22620,         /* Order Qty        */~
                                    L22660,         /* Allocation       */~
                                    L22690,         /* Open Order       */~
                                    L22730,         /* Lot              */~
                                    L22760,         /* Total Qty Shipped*/~
                                    L22781,         /* Currency code    */~
                                    L23010,         /* Unit Price       */~
                                    L23110,         /* Line Item Disc   */~
                                    L23140,         /* Line Taxable?    */~
                                    L23250,         /* PO Item Number   */~
                                    L23400,         /* Project Number   */~
                                    L23430,         /* Sales Distr. Acct*/~
                                    L23540,         /* Sales Discs Acct */~
                                    L23690,         /* Shipping Priority*/~
                                    L23280,         /* Planning Priority*/~
                                    L23360,         /* Demand Type      */~
                                    L23630,         /* BOM ID           */~
                                    L23660          /* Horizon Date     */
                     return

L22330
*        Part Code                             PART$()
            return

L22360
*        Part Description                      DESCR$()
            if nonstockmsg$(c%) <> " " then return
                get #04 using L22390, descr$(c%)
L22390:              FMT XX(25), CH(32)
                return

L22420
*        Part Category                         CAT$()
            if nonstockmsg$(c%) <> " " then return
                get #04 using L22450, cat$(c%)
L22450:              FMT XX(89), CH(4)
                return

L22480
*        Due Date                              DUEDATE$()
            duedate$(c%) = dfltdue$
            return

L22520
*        Required Ship Date                    SHIPDATE$()
            shipdate$(c%) = dfltship$
            return

L22560
*        Stocking Unit of Measure (Non-stock parts only) STKUOM$
            if nonstockmsg$(c%) <> " " then return
                get #04 using L22590, stkuom$(c%)
L22590:              FMT XX(73), CH(4)
                return

L22620
*        Original Order Quantity               ORDER$
            return

L22660
*        Allocation Qty, Instructions          ALLOC$()
            get #04 using L22664, alloc$(c%)
L22664:         FMT POS(242), CH(1)
            if alloc$(c%) = " " then  allocqty$ = allocdflt$             ~
                                else  allocqty$ = alloc$(c%)
            return

L22690
*        Open Order Quantity                   OPENQTY$
            openqty$ = order$
            return

L22730
*        Lot                                   LOT$()
            return

L22760
*        Total Quantity Shipped                SHIP$
            return

L22781
*        Currency code                         CURRENCY$
            return

L22790
*        Pricing Unit of Measure               PRICEUOM$()
            if nonstockmsg$(c%) <> " " then return
                get #04 using L22820, priceuom$(c%)
L22820:              FMT XX(77), CH(4)
                return

L22850
*        Conversion Pricing -> Stocking        CONV$
            if nonstockmsg$(c%) <> " " then L22910
                get #04 using L22880, conv(c%)
L22880:              FMT POS(82), PD(14,7)
                call "CONVERT" (conv(c%), 7.7, conv$)
                return
L22910:     if stkuom$(c%) = priceuom$(c%) then conv$ = "1"
            if stkuom$(c%) = priceuom$(c%) then return
                readkey$ = "UOMCONV  " & str(priceuom$(c%)) & "-" &      ~
                                         str(stkuom$  (c%))
                call "READ100" (#08, readkey$, f1%(8))
                if f1%(8) = 0% then return
                     get #08 using L22980, conv$
L22980:                   FMT XX(24), CH(10)
                     return

L23010
*        Unit Price                            PRICE$
            if ptype$(c%) = "000" then return
            get #01 using L23030, custype$
L23030:         FMT XX(1022), CH(2)
            call "CPRASSGN" (cuscode$, custype$, part$(c%), cat$(c%),    ~
                pc$, orderdate$, currtype$, currency$, opdlrs(2),        ~
                order(c%), #02, #04, price(c%), linedisc(c%), errormsg$)
            if errormsg$ <> " " then return
                gosub pricing_stuff
                return

L23110
*        Line Item Discount %                  LINEDISC$
            return

L23140
*        Line Taxable? (Y/N)                   TAXABLE$()
            get #01 using L23160, custaxable$
L23160:         FMT XX(793), CH(1)
            parttaxable$ = " "
            if nonstockmsg$(c%) =" " then get #04 using L23190,parttaxable$
L23190:         FMT XX(126), CH(1)
            if parttaxable$ = "N" then taxable$(c%) = "N"
            if parttaxable$ = "Y" then taxable$(c%) = "Y"
            if parttaxable$ = " " then taxable$(c%) = custaxable$
            return

L23250
*        PO Item                               ITEM$()
            return

L23280
*        Planning Priority Code                PRIORITY$()
            if nonstockmsg$(c%) <> " " then L23320
                get #04 using L23310, priority$(c%)
L23310:              FMT XX(333), CH(1)
L23320:         if priority$(c%) < "A" or priority$(c%) > "Z" then       ~
                                                      priority$(c%) = "A"
                return

L23360
*        Planning Demand Type                  DEMTYPE$()
            get #04 using L23364, demtype$(c%)
L23364:         FMT POS(243), CH(1)
            if demtype$(c%) = " " then  demtype$(c%) = dflt_dem_type$
            return

L23400
*        Project Number                        PROJECT$()
            return

L23430
*        Sales Distr. Account                  SALESACCT$()
            call "HNYGLGET" (part$(c%), store$, lot$(c%), salesacct$(c%),~
                             5%, #04, #13)
            if salesacct$(c%) <> " " then call "GLFMT" (salesacct$(c%))
            if salesacct$(c%) <> " " then return
                call "ARMGLGET" (1%, cuscode$, part$(c%), cat$(c%), " ", ~
                                 store$, " ", #02, #01, #04, #11, #03,   ~
                                 salesacct$(c%))
                if salesacct$(c%) = " " then salesacct$(c%) = dfltsales$
                return

L23540
*        Sales Discounts Account               DISCACCTL$()
            if nonstockmsg$(c%) <> " " then L23590
                call "ARMGLGET" (2%, cuscode$, part$(c%), cat$(c%), " ", ~
                                 store$, " ", #02, #01, #04, #11, #03,   ~
                                 discacctl$(c%))
L23590:     if discacctl$(c%) = " " then discacctl$(c%) = discacct$
            return


L23630
*        BOM ID                                BOMID$
            return

L23660
*        Horizon                               HORIZON$
            return

L23690
*        Shipping Priority Code                SHIPCODE$
            if shipcode$(c%) <> " " then return
                get #1 using L23720, shipcode$
L23720:             FMT POS(733), CH(1)
                if shipcode$ = " " then shipcode$ = "3"
                shipcode$(c%) = shipcode$
            return

        REM *************************************************************~
            *        I N I T I A L I Z E   F O R   I N P U T            *~
            * --------------------------------------------------------- *~
            * Clear variables prior to input mode.                      *~
            *************************************************************
        init_for_input
            init(" ") errormsg$, inpmessage$, cuscode$,so$,po$, ptype$(),~
                      shipto$(), soldto$(), canceldate$, dfltdue$,       ~
                      dfltship$, howship$, fob$, shipinstr$(), pc$,      ~
                      orderdisc$, terms$, region$, salesman$(), comm$(), ~
                      dfltsales$, discacct$, part$(), descr$(), seq$(),  ~
                      cat$(), order$, openqty$, ship$, priceuom$(),      ~
                      conv$, price$, linedisc$, taxable$(), duedate$(),  ~
                      shipdate$(), priority$(), demtype$(), salesacct$(),~
                      discacctl$(), lot$(), project$(), lsts$(), vf$,    ~
                      nonstockmsg$(), stkuom$(), pricestk$, ext$,        ~
                      origdue$(), qtyschld$, weekday$, weekdayh$, store$,~
                      alloc$(), item$(), adjrsn$, smry$(), oldbomid$,    ~
                      oldstore$, allocqty$, orderdate$, filler$(),       ~
                      export$, currency$, convdate$, adjrsndescr$,scode$,~
                      origdate$, origuser$, crmsg$(), crhold$, mode$,    ~
                      how_held$,refdesc$(),refpart$(),mfgcode$,          ~
                      shipcode$(), pickprint$(), dfltacks$, reftype$()
            init(hex(00)) datetime$
            init(hex(8c)) sfac$()
            nextbol%, top% = 1%
            maxlines%, c%, lastseq%, hdrflag%, modfld% = 0%
            oldflag%, soonfile% = 0%
            allow_delete$ = allow_delete_save$
            origopen = 0 : conveqv, convunt = 1
            mat comm%    = zer
            mat order    = zer
            mat openqty  = zer
            mat ship     = zer
            mat price    = zer
            mat pricestk = zer
            mat preinv   = zer
            mat qtyschld = zer
            mat alloc    = zer

            mat e_lines% = zer
            total_e_lines% = 0%

            smryh$ = smryhdr$

            init (hex(ff)) textid$, textidl$()
            call "TXTFUTIL" (#22, f2%(22), "INTL", textid$)
            return

L27490: REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************

            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "BCKONLIN", scr%(), set%(),       ~
                              s%, f%, 0%, 0%)
            return

L28000: REM *************************************************************~
            *        I N P U T   M E S S A G E S  &  E N A B L E S      *~
            * --------------------------------------------------------- *~
            * Sets Enable Flag, Input Message, and Standard PF Keys.    *~
            *************************************************************
        REM S% = Screen; EDIT% = 1=Input Mode; 2=Edit Mode
        if f% <> 0% then L28080
            if s% = 1% then inpmessage$ =                                ~
                "Position Cursor at Header field & RETURN -OR- at " &    ~
                "Line Item and use PF Key."
            if s% = 2% then inpmessage$ =                                ~
                "Position Cursor at Field to edit and Press RETURN."
            if s% = 3% then inpmessage$ =                                ~
                "Position Cursor at Field to edit and Press RETURN."
            return

L28080
*        First Define the Input Message
            if s% > 1% then s% = s% - 1%
            r% = scr%(s%, f%)  /* Get sequential field number          */
            restore line = L28210, r%     /* Position for Read          */
            read inpmessage$             /* Read Input Message         */

*        Now set the Field Enable Flag
            call "ENABLSUB" ("SET", "BCKONLIN", scr%(), set%(), s%, f%,  ~
                             edit%, enabled%)
            if s% = 1% and f% = 4% then store$ = dfltstore$
            if s% = 2% and f% = 14% and (c% <> 1% or maxlines% > 1% or   ~
                edit% = 2% or curr$ <> "Y") then enabled% =  0%
            if s% = 2% and f% = 13% then enabled% =  0%
            if s% = 1% and f% = 12% and pm_on$ <> "Y" then enabled% = 0%
            if s% = 1% and f% = 13% and pm_on$ <> "Y" then enabled% = 0%
            if c% = 0% or s% <> 2% then return
                if edit% = 1% and (f% = 6% or f% = 7% or f% = 8%) and    ~
                              nonstockmsg$(c%) = " " then enabled% =  0%
                if f% <> 25% then L28165
                if ptype$(c%) <> "000" then L28165
                     errormsg$ = "BOM can only be changed within " &     ~
                                 "Option Selection Function."
                     enabled%  = 0%
L28165:         if f% <> 10% or demstatus$ < "2" then L28180
                     errormsg$ = "Must Unplan to change Allocation."
                     enabled%  = 0%
L28180:         if fieldnr% <> 12% then return
                    call "LOTENABL" (part$(c%), enabled%, ll%, #02, #04)
                    if enabled% > 0% then return
                        lot$(c%) = " " : keyhit% = 0% : return
                return

L28210: data                                                             ~
        /* Screen 1 (Two Parts)                                        */~
         "Enter Customer Code and/or Sales Order Number.",               ~
         "Enter Sales Order Number ('?' to Scan existing Orders).",      ~
         "Enter Ship-to Name and Address.",                              ~
         "Enter Store that will ship merchandise.",                      ~
         "Enter Customer's Purchase Order Number.",                      ~
         "Enter How to Ship Instructions.",                              ~
         "Enter FOB Description.",                                       ~
         "Enter Order Date.",                                            ~
         "Enter Default Due Date.",                                      ~
         "Enter Date -or- '-' and number of days before due date.",      ~
                                                                         ~
         "Enter Sold-to, 'BILL-TO', or leave blank if same as ship-to.", ~
         "Enter Order Cancellation Date.",                               ~
         "Enter Pricing Code for this Order.",                           ~
         "Enter Order Level Discount Percentage.",                       ~
         "Enter Payment Terms for this Order.",                          ~
         "Enter the Region to be credited with this order.",             ~
         "Enter the Salesmen and Commission Split %'s for this order.",  ~
         "Enter the default Sales Account.",                             ~
         "Enter the Sales Discounts Account.",                           ~
         "Enter Shipping Instructions.",                                 ~
                                                                         ~
        /* Screen 2- Line Items                                        */~
         "Enter the Part Code for this Line Item.",                      ~
         "Enter the Part Description for this Line Item.",               ~
         "Enter the Part Category Code.",                                ~
         "Enter the Current Due Date for this line item.",               ~
         "Enter Required Ship Date -or- '-' and days before Due Date.",  ~
         "Enter the Stocking Unit of Measure.",                          ~
         "Enter the Pricing Unit of Measure.",                           ~
         "Enter the Pricing UOM to Stocking UOM Conversion Factor.",     ~
         "Enter the Original Order Quantity.",                           ~
         "Enter the Allocation Qty (A=ATC, C=Complete).",                ~
         "Enter the Open Order Quantity.",                               ~
         "Enter the Lot Number requested by the Customer.",              ~
         "Enter the Total Quantity Shipped to date.",                    ~
         "Enter the Unit Price (in the PRICING UOM).",                   ~
         "Enter the Discount Percent for this Line Item.",               ~
         "Is this line item Taxable? (Y/N)",                             ~
         "Enter the P.O. Line Number Reference.",                        ~
         "Enter the Project Number.",                                    ~
         "Enter the Sales Distribution Account Number.",                 ~
         "Enter the Sales Discount Account Number.",                     ~
         "Enter the Planning Priority Code.",                            ~
         "Enter the Planning Demand Type Code.",                         ~
         "Enter the Bill of Material to Over-ride the effective BOM.",   ~
         "Enter the Planning Horizon Date. (Field Disabled!?!)",         ~
                                                                         ~
        /* Screen 1 (add on)                                           */~
         "Enter 'Y' if this is an EXPORT Order",                         ~
                                                                         ~
        /* Screen 3 (add on)                                           */~
         "Enter Transaction Currency Code for this Document. Blank = stat~
        ~utory.",                                                         ~
         "Enter the Shipping Priority Code for this Line.",              ~
                                                                         ~
        /* Screen 1 (add on)                                           */~
         "Enter 'Y' if Precious Metal Surcharges to added at SO time.",  ~
         "Enter 'Y' if Precious Metal Surcharges to updated at INV time."

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants.   *~
            *************************************************************
        startover  /* ONLY accessed for new order prior to any writes. */
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                gosub clear_all_options
                goto inputmode

        restart_line      /* Only allowed in Input Mode      */
L29150:     u3% = 2%
            call "ASKUSER" (u3%, "RESTART LINE",                         ~
                            "Press (RETURN) to RESTART Line Item",       ~
                            "- OR -", "Press PF-1 to EXIT Restart.")
            if u3% = 1% then return
                if u3% <> 0% then L29150
                     return clear
                     gosub clear_line_options
                     gosub clear_line_text
                     gosub clear_line
                     goto  appendlines

        clear_line_text
            call "TXTFUTIL" (#22, f2%(22), "XOUT", textidl$(c%))
            return

        clear_all_options
              if maxlines% = 0% then return
            c8% = 1%  :  c9% = maxlines%  :  goto clear_options

        clear_line_options /* Clear any options entered for line C%    */
            if so$ = " " then return
                c8%, c9% = c%  :  str(lsts$(c%),,3) = "ADD"

        clear_options
            for c% = c8% to c9%
              if str(lsts$(c%),,3) <> "ADD" then L29350
                init(hex(00)) readkey$
                str(readkey$,,19) = str(so$) & str(seq$(c%))
                call "REDALT1"(#49, readkey$, 1%, f1%(49))  /* header */
                   if f1%(49) = 0% then L29310
                     delete #49

L29310:         call "PLOWAL1" (#17, readkey$, 1%, 19%, f1%(17))
                if f1%(17) = 0% then L29350
                     delete #17
                     goto L29310
L29350:     next c%
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
            *             L O A D   S A L E S   O R D E R               *~
            * --------------------------------------------------------- *~
            * Load up the Sale Order if on file and toss directly into  *~
            * buffer (do not pass go, do not collect $200).  If Order   *~
            * is not on file, set it up with all kinds of defaults.     *~
            *************************************************************
        load_sales_order
            soonfile% = 0%
            adjrsndescr$ = " "

*        First see if the order in question is in the master file
            readkey$  = str(so$) & hex(00)
            call "PLOWNEXT" (#06, readkey$, 16%, soonfile%)
            if soonfile% = 0% then allow_delete$ = "Y"
            if soonfile% = 0% then L30310
                get #06 using L30145, tempcus$
L30145:           FMT CH(9)
                if cuscode$ = " " then cuscode$ = tempcus$
                if cuscode$ = tempcus$ then L30206
                     errormsg$ = "Order already assigned to Customer " & ~
                                 tempcus$
                     return

*        Resurrect sales order?
L30206:     str(readkey$,,9) = str(cuscode$)
            str(readkey$,10,16) = str(so$)
              call "READ100" (#5, readkey$, f1%(5))
                get #05 using L30214, crhold$
L30214:           FMT POS(875), CH(1)
            if crhold$ <> "C" then L30310
L30220:     u3% = 2%
            call "ASKUSER" (u3%, "** CANCELLED ORDER **", "* Press PF10"&~
                " to Re-activate Order", "- Open Quantities MIGHT need "&~
                "to be Changed -", "* Press PF16 to exit")
            if u3% <> 16%  and u3% <> 10% then L30220
            if u3% = 16% then inputmode

L30310
*        See if order is already in buffer.  If so can't touch
            call "REDALT0" (#09, so$, 2%, f1%(9))
            if f1%(9) = 0% then L30335
                errormsg$ = "Order is already being processed."
                return
L30335:     if cuscode$ = " " then return
            call "BCKPREFX" (so$, errormsg$)
            if errormsg$ <> " " then return

*        See if order number is already used as a demand code
            if soonfile% <> 0% then L30358
            readkey$ = all(hex(00))
            str(readkey$,1,16) = str(so$)
            call "PLOWALTS" (#23, readkey$, 1%, 16%, f1%(23))
            if f1%(23) = 0% then L30358
              errormsg$ = "Order Number is already used as a Demand Code"
              return

L30358
*        All Ok-fine.  If this is a new Order then set it up; if it is
*        an Old Order, first toss it into the Buffers and then load the
*        data from the buffer files.  This little trick facilitates
*        restarting whether the order is new or old.
            if soonfile% = 0% then new_order_setup

            print at(09,02,79), "Loading Sales Order from Master..."
            oldflag% = 1%
            readkey$ = str(so$) & hex(00)
            call "DELETE"  (#10, readkey$, 16%) /* Just in Case */
L30400:     call "PLOWNEXT" (#06, readkey$, 16%, f1%(6))
            if f1%(6) = 0% then L30430
                get #06 using L30415, str(oldline$())
L30415:              FMT CH(300)
                write #10 using L30415, str(oldline$())
                goto L30400
L30430:     readkey$ = " "
            str(readkey$, 1, 3) = userid$
            str(readkey$, 4, 7) = all(hex(00))
            str(readkey$,11, 8) = "BCKONLIN"
            str(readkey$,21,25) = str(cuscode$) & so$
            call "READ101" (#05, str(readkey$,21,25), f1%(5))
            get #05 using L30465, str(oldhdr$())
L30465:         FMT CH(1000)
            write #09 using L30475, str(readkey$,,20), str(oldhdr$())
L30475:         FMT CH(20), CH(1000)

*        Now load from BUFFER and format data as required.
*        This is also our entry point for restarts.
            print at(09,02,79), "Loading Sales Order from Master..."
        restart_reload
            call "READ100" (#01, cuscode$, f1%(1))  /* May have changed */
            get #09 using L35060, po$, shipto$(), soldto$(), terms$,      ~
                howship$, fob$, shipinstr$(), dfltsales$, discacct$,     ~
                salesman$(), comm%(), region$, vf$, textid$, store$,     ~
                orderdate$, canceldate$, dfltdue$, dfltship$,            ~
                origdate$, origuser$, lastchanged$, lastuser$, export$,  ~
                pc$, orderdisc, origopen, crhold$, lastseq%, nextbol%,   ~
                currency$, how_held$, pm_so$, pm_inv$
            if currency$ = " " then currency$ = statutory$
            gosub set_pm_flags
            call "TXTFUTIL" (#22, f2%(22), "LOAD", textid$)
            holdorig  = origopen
            origopen = origopen - round((origopen * orderdisc * .01), 2)
            oldstore$ = store$
            call "GLFMT" (dfltsales$)
            call "GLFMT" (discacct$)
            for i% = 1% to 3%
                if salesman$(i%) = " " then L30770
                     convert comm%(i%) to comm$(i%), pic(##0)
L30770:     next i%
            call "DATEFMT" (orderdate$)
            call "DATEFMT" (canceldate$)
            call "DATEFMT" (dfltdue$)
            if dfltship$ <> " " and dfltship$ <> blankdate$ then        ~
                call "DATE" addr("GD", str(dfltship$,,6), weekday$, u3%)
            call "DATEFMT" (dfltship$)
            call "DATEFMT" (origdate$)
            call "DATEFMT" (lastchanged$)
            call "CONVERT" (orderdisc, 2.2, orderdisc$)

*        Now Read in line items from BCKBUF2
            c%, maxlines% = 0%
            currkey$, readkey$ = str(so$) & hex(00)
L30910:     call "PLOWNEXT" (#10, readkey$, 16%, f1%(10))
            if f1%(10) = 1% then L30960
                convert seq$(maxlines%) to temp%
                lastseq% = max(lastseq%, temp%)
                if maxlines% + total_e_lines% <= 100% then L30950
                   gosub extra_load_conflict
L30950:         return
L30960:     c%, maxlines% = c% + 1%
            get #10 using L35830, tempcus$, tempso$,                      ~
                seq$(c%), item$(c%), part$(c%), descr$(c%),              ~
                cat$(c%), order(c%), ship(c%), openqty(c%),              ~
                qtyschld(c%), alloc(c%), preinv(c%), pricestk(c%),       ~
                stkuom$(c%), priceuom$(c%), conv(c%), price(c%),         ~
                linedisc(c%), taxable$(c%), salesacct$(c%),              ~
                discacctl$(c%), origdue$(c%), duedate$(c%),              ~
                shipdate$(c%), lot$(c%), project$(c%), temp$,            ~
                demtype$(c%), priority$(c%), textidl$(c%),               ~
                                   alloc$(c%), invnbr$, estnbr$,         ~
                     bomid$  , mlquote_seq$(c%), shipcode$(c%),          ~
                     pm_adj%(c%), filler$(c%)

            pm_base(c%) = price(c%)

            if shipcode$(c%) <> " " then L31060
                if shipcode$ <> " " then L31058
                    get #1 using L31056, shipcode$
L31056:                 FMT POS(733), CH(1)
                if shipcode$ = " " then shipcode$ = "3"
L31058:         shipcode$(c%) = shipcode$
L31060: REM Get Corresponding Reference Part
            call "PTUSEDSB" ("R", "BCK ", tempso$, seq$(c%),             ~
                          refpart$(c%), refdesc$(c%), reftype$(c%), ret%)
            if ret% = 1% then refflag% = 1% else                         ~
                refpart$(c%) = "** No Cross Reference **"
        REM Get transaction amounts from the BCK 'shadow' file (BCKLNCUR)
            if currency$ = statutory$ then goto L31130 /* Not Stat, tho */
            if curr$ <> "Y" then goto L31130 /* Nor if no multi-currency*/
            call "READ100" (#41, readkey$, f1%(41))
            if f1%(41) = 0% then goto L31130
            get #41 using L31095, pricestk(c%), price(c%)
L31095:         FMT POS(24), 2*PD(14,4)
            pm_base(c%) = price(c%)
                if c% = 1% then get #41 using L31110, currency$,          ~
                    convdate$, conveqv, convunt
L31110:             FMT CH(4), POS(40), CH(6), 2*PD(14,7)
L31130:     call "TXTFUTIL" (#22, f2%(22), "LOAD", textidl$(c%))
            call "DATEFMT" (origdue$ (c%))
            call "DATEFMT" (duedate$ (c%))
            call "DATEFMT" (shipdate$(c%))
            call "GLFMT"  (salesacct$(c%))
            call "GLFMT"  (discacctl$(c%))
            if qtyschld(c%) <> 0 then str(lsts$(c%),4,1) = "s"
            if preinv  (c%) <> 0 then str(lsts$(c%),5,1) = "p"
            gosub check_for_options
            call "READ100" (#04, part$(c%), f1%(4))
            if f1%(4) <> 0% then L31231
                 nonstockmsg$(c%) = "NonStk"
                 refpart$(c%) = "** No Cross Reference **"
                 goto L31233
L31231:     get #4, using L31232, ptype$(c%)
L31232:         FMT POS(180), CH(3)
L31233:     call "ARIEXTRA" (cuscode$, part$(c%), " ", e_lines%(c%), #2)
            total_e_lines% = total_e_lines% + e_lines%(c%)
            goto L30910


        new_order_setup
*        Get all the Header Level Defaults that we can.  This section
*        eliminates the need for the Defaults Code Section.

            orderdate$ = date$
            get #01 using L31350, soldto$(), pm_cus_so$, pm_cus_inv$,     ~
                                            shipto$(), orderdisc, pc$,   ~
                                terms$, howship$, fob$, shipinstr$(),    ~
                                salesman$(), comm%(), region$, shipcode$,~
                                export$
L31350:         FMT POS(40), 6*CH(30), POS(226), 2*CH(01),               ~
                                       POS(253), 6*CH(30), POS(517),     ~
                    PD(14,4), CH(1), POS(543), 3*CH(20), POS(614),       ~
                    2*CH(50), 3*CH(4), 3*BI(1), CH(4), CH(1), POS(1091), ~
                    CH(1)
            if shipcode$ = " " then shipcode$ = "3"
            call "CONVERT" (orderdisc, 2.2, orderdisc$)
            for i% = 1% to 3%
                if salesman$(i%) <> " " then                             ~
                                 convert comm%(i%) to comm$(i%), pic(##0)
            next i%
            if export$ = " " then export$ = "N" /* Default Export value */
            c%, lastseq%, maxlines% = 0%
            origopen, holdorig = 0

            gosub set_pm_flags
            return


        REM *************************************************************~
            *             S A V E   S A L E S   O R D E R               *~
            * --------------------------------------------------------- *~
            * Update the lines with any changes entered but not yet     *~
            * saved.  Then update the Demands per Credit Hold Status.   *~
            * Next change the key on the header record, toss the text,  *~
            * align the cutomers and yell at the update.                *~
            *************************************************************

        save_sales_order

            if curr$ <> "Y" then goto L32110
                currkey$ = str(so$) & hex(00)
                call "DELETE" (#41, currkey$, 16%)

L32110
*        Update the lines in the buffer to retain any non-planning field
*        changes that have been made.
            if maxlines% = 0% then L32200
            call "SHOSTAT" ("Updating Lines with non-planning data")
            savepass% = 99%
            for c% = 1% to maxlines%
                gosub save_line
            next c%
            savepass% = 0%

L32200
*        Next, Set All Demands approved or unapproved per Credit Flag
          if maxlines% = 0% then L32450
            if crhold$ = "H" then                                        ~
                call "SHOSTAT" ("Setting Demands to Unapproved") else    ~
                call "SHOSTAT" ("Setting Demands to Approved")
            plowkey$ = str(so$) & hex(00)
L32260:     call "PLOWAL1" (#23, plowkey$, 1%, 16%, f1%(23))
            if f1%(23) = 0% then L32450
                get #23 using L32290, demstatus$
L32290:              FMT POS(1), CH(1)
                if crhold$ = "H" then L32360
                  /* Set any unapproved demands to approved */
                     if demstatus$ = " " then demstatus$ = "1"
                     if demstatus$ = "6" then demstatus$ = "7"
                     if demstatus$ = "8" then demstatus$ = "9"
                     goto L32400
L32360:           /* Set any approved demands to unapproved */
                     if demstatus$ = "1" then demstatus$ = " "
                     if demstatus$ = "7" then demstatus$ = "6"
                     if demstatus$ = "9" then demstatus$ = "8"
L32400:           /* Now rewrite demand                     */
                     put #23 using L32290, demstatus$
                     rewrite #23
                     goto L32260

L32450
*        Next write out the updated Header and move all text
            call "GETDTTM" addr(datetime$)
            gosub save_header
            call "TXTFUTIL" (#22, f2%(22), "SAV2", textid$)

*        Update the Customer(s) with the Open Order Dollars
          if abs(curtemp4 - origopen) < .01 then L32677
            call "READ100" (#01, cuscode$, f1%(1%))
            get #01 using L32540, billto$
L32540:         FMT POS(780), CH(9)
            topen(1%), topen(2%) = 0                            /* JIC */
            call "READ101" (#51, cuscode$, f1%(51%))       /* CCRMASTR */
            if f1%(51%) <> 0% then get #51 using L32544, topen(1), topen(2)
L32544:         FMT POS(114), 2*PD(14,4)
            if billto$ = cuscode$ then                                   ~
                topen(1) = topen(1) - origopen + curtemp4
                topen(2) = topen(2) - origopen + curtemp4
            if f1%(51%) = 0% then put #51 using L36590, cuscode$, 0, 0, 0,~
                " ", 0, " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0,  ~
                0, 0, 0, " ", " ", " "
            put #51 using L32590, date, topen(1), topen(2), userid$, date
L32590:         FMT POS(84), CH(6), POS(114), 2*PD(14,4), POS(146),      ~
                     CH(3), CH(6)
            if f1%(51%) = 0% then write #51 else rewrite #51

            if billto$ = cuscode$ then L32677
L32630:         call "READ100" (#01, billto$, f1%(1))
                if f1%(1%) = 1% then L32634
                 billto$ = str(cuscode$) /* No Bill to for this Ship to */
                 goto L32630
L32634:     topen(1%), topen(2%) = 0                            /* JIC */
            call "READ101" (#51, billto$, f1%(51%))        /* CCRMASTR */
            if f1%(51%) <> 0% then get #51 using L32544, topen(1), topen(2)
            topen(1) = topen(1) - origopen + curtemp4
            if f1%(51%) = 0% then put #51 using L36590, billto$, 0, 0, 0, ~
                " ", 0, " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0,  ~
                0, 0, 0, " ", " ", " "
            put #51 using L32590, date, topen(1), topen(2), userid$, date
            if f1%(51%) = 0% then write #51 else rewrite #51

L32677
*        Write to Precious Metal Shadow File if needed
            if maxlines% = 0% then L32700
            for c% = 1% to maxlines%
                if pm_adj%(c%) <> 1% then L32693
                call "PMCALSUB" (cuscode$, part$(c%), openqty(c%),       ~
                                 price(c%), conv(c%), "S", orderdate$,   ~
                                 s_charge, "S", str(so$) & seq$(c%),     ~
                                 pm_so$, pm_inv$, rslt%)
L32693:     next c%

L32700
*        FINALLY Crank up the update
            u3% = 0%
            call "TASKUP" ("SO", u3%)
            goto inputmode


        REM *************************************************************~
            *                S A V E   H E A D E R                      *~
            * --------------------------------------------------------- *~
            * Save the Header for the Order being worked on.            *~
            *************************************************************
        save_header
            gosub assign_so_number

*        Unformat Dates and G/L Accounts
            call "GLUNFMT"  (dfltsales$)
            call "GLUNFMT"  (discacct$ )
            call "DATUNFMT" (orderdate$ )
            call "DATUNFMT" (canceldate$)
            call "DATUNFMT" (dfltdue$   )
            call "DATUNFMT" (dfltship$  )
            call "DATUNFMT" (origdate$  )
            if origdate$ = " " or origdate$ = blankdate$ then origdate$ = date
            if origuser$ = " " then origuser$ = userid$

            readkey$ = all(hex(00))
            str(readkey$,,3) = userid$
            if datetime$ = hex(00000000000000) then L33290
              /* Save with the intention of updating         */
                call "DELETE" (#09, str(readkey$,,10), 10%)
                str(readkey$, 4, 7) = datetime$
                str(readkey$,11   ) = pickprint$

                topen(1) = topen(1) + topen(2)
                curtemp4 = round(curtemp3 * orderdisc * .01, 2)
                curtemp4 = curtemp3 - curtemp4
                goto L33340

L33290:       /* Save just to have and to hold               */
                str(readkey$, 4, 7) = all(hex(00))
                str(readkey$,11   ) = "BCKONLIN"
                topen(1) = holdorig

L33340:     get #01 using L33350, acctxref$, custype$
L33350:         FMT POS(771), CH(9), POS(1023), CH(2)

            call "READ101" (#09, str(readkey$,,10), f1%(9))
            put #09 using L35410, str(readkey$,,20),                      ~
                cuscode$, so$, po$, shipto$(), soldto$(), terms$,        ~
                howship$, fob$, shipinstr$(), dfltsales$, discacct$,     ~
                salesman$(), comm%(), region$, vf$, textid$, store$,     ~
                orderdate$, canceldate$, dfltdue$, dfltship$,            ~
                origdate$, origuser$, date, userid$, adjrsn$, export$,   ~
                pc$, orderdisc, curtemp3, crhold$, lastseq%, nextbol%,   ~
                custype$, acctxref$, currency$, how_held$,pm_so$,pm_inv$,~
                " "
            dfltstore$ = store$
            if soonfile% = 0% then put #09 using L33480, " "
L33480:         FMT POS(859), CH(9)  /* Blank Change Audits       */

            if f1%(9) = 0% then write #09 else rewrite #09
            hdrflag%  = 1%

*        Reformat Dates and Account Numbers
            call "GLFMT"   (dfltsales$)
            call "GLFMT"   (discacct$ )
            call "DATEFMT" (orderdate$ )
            call "DATEFMT" (canceldate$)
            call "DATEFMT" (dfltdue$   )
            call "DATEFMT" (dfltship$  )
            call "DATEFMT" (origdate$  )

            return


        assign_so_number
            if so$ = " " then call "BCKNEXT" (#12, #06, #10, #37, #23,   ~
                                              store$, so$)
        return

        set_pm_flags
            if pm_on$ <> "Y" then return
            if pm_so$ <> " " then L33740
                if pm_cus_so$  = " " then pm_so$  = pm_sys_so$           ~
                                 else pm_so$  = pm_cus_so$
L33740:     if pm_inv$ <> " " then return
                if pm_cus_inv$ = " " then pm_inv$ = pm_sys_inv$          ~
                                 else pm_inv$ = pm_cus_inv$
            return
        REM *************************************************************~
            *                    S A V E   L I N E                      *~
            * --------------------------------------------------------- *~
            * Save the Line Item (C%) to the Buffer file.               *~
            *************************************************************
        save_line
            if savepass% = 0% then                                       ~
                call "SHOSTAT" ("Saving Planning and Line Information.")
*              PRINT AT(09,02,79),"Saving Planning and Line Information"
            datetime$ = all(hex(00))
            if so$ = " " or hdrflag% = 0% then gosub save_header

            currkey$, readkey$ = str(so$) & str(seq$(c%))

            if str(lsts$(c%),,3) <> "DEL" then L34200
                call "TXTFUTIL" (#22, f2%(22), "XOUT", textidl$(c%))
                saveseq$ = seq$(c%)  :  savepart$ = part$(c%)
                gosub clear_line
                seq$(c%) = saveseq$  :  part$(c%) = savepart$
                lsts$(c%)  = "DEL"     /* Flag as deleted- here   */
                descr$(c%) = hex(ff)   /* Flag as deleted- update */
L34200:     call "DATUNFMT" (origdue$ (c%))
            call "DATUNFMT" (duedate$ (c%))
            call "DATUNFMT" (shipdate$(c%))
            call "GLUNFMT"  (salesacct$(c%))
            call "GLUNFMT"  (discacctl$(c%))

            if descr$(c%) = hex(ff) then goto L34316
            if curr$ <> "Y" then goto L34316
            if currency$ = statutory$ then goto L34316
                call "READ101" (#41, currkey$, f1%(41))
                put #41 using L36170, currency$, so$, seq$(c%),           ~
                     pricestk(c%), price(c%), convdate$, conveqv,        ~
                     convunt, " "
                if f1%(41) = 0% then write #41 else rewrite #41

L34316:     curtemp1 = round((pricestk(c%) * conveqv), 4)
            curtemp2 = round((price   (c%) * conveqv), 4)
            dummy    = round((openqty(c%) * curtemp2 / conv(c%)), 2)
            curtemp3 = curtemp3 + (dummy -                               ~
                       round((dummy*(linedisc(c%)/100)), 2))

            call "READ101" (#10, readkey$, f1%(10))
            put #10 using L35830, cuscode$, so$, seq$(c%),                ~
                     item$(c%), part$(c%), descr$(c%), cat$(c%),         ~
                     order(c%), ship(c%), openqty(c%), qtyschld(c%),     ~
                     alloc(c%), preinv(c%), curtemp1, stkuom$(c%),       ~
                     priceuom$(c%), conv(c%), curtemp2, linedisc(c%),    ~
                     taxable$(c%), salesacct$(c%), discacctl$(c%),       ~
                     origdue$(c%), duedate$(c%), shipdate$(c%), lot$(c%),~
                     project$(c%), " ", demtype$(c%), priority$(c%),     ~
                     textidl$(c%), alloc$(c%), invnbr$, estnbr$,         ~
                     bomid$  , mlquote_seq$(c%), shipcode$(c%),          ~
                     pm_adj%(c%), filler$(c%)
            if f1%(10) = 0% then write #10 else rewrite #10
              if xref%    = 0% then L34580  /* Must have a Xref File */
              if refpart$(c%) = "** No Cross Reference **"  then L34580
              if reftype$(c%) = " "  then L34580
                  /* Add Cross reference part to shadow file */
                  call "PTUSEDSB" ("W", "BCK ", so$, seq$(c%),           ~
                                   refpart$(c%), refdesc$(c%),           ~
                                   reftype$(c%), ret%)

L34580:     if savepass% = 99% then return

            call "DATEFMT" (origdue$ (c%))
            call "DATEFMT" (duedate$ (c%))
            call "DATEFMT" (shipdate$(c%))
            call "GLFMT"  (salesacct$(c%))
            call "GLFMT"  (discacctl$(c%))

         bckpipsb
            call "BCKPIPSB" (so$,                 /* Sales Order #     */~
                             seq$(c%),            /* Line Sequence     */~
                             store$,              /* Store             */~
                             crhold$,             /* Credit Status     */~
                             #10,                 /* BCKLINES (kinda)  */~
                             #23,                 /* DEMMASTR          */~
                             #02,                 /* SYSFILE2          */~
                             #24,                 /* PIPMASTR          */~
                             #29,                 /* PIPIN             */~
                             #30,                 /* PIPOUT            */~
                             #17,                 /* BOMSPEC           */~
                             #27,                 /* WCMASTR           */~
                             #28,                 /* WCOUT             */~
                             #32,                 /* SFMASTR2          */~
                             #33,                 /* SFCUM2            */~
                             #34,                 /* JBCROSS2          */~
                             #31,                 /* PIPCROSS          */~
                             #35 )                /* JBPIPXRF          */
            if edit% = 2% then gosub describe_line
            return

        call_screen
            call "GETSCRN" ("C", " ", cursor%(), 0%)
            return

        pf1315
            if keyhit% <> 13% then L34950
                call "MANUAL" ("BCKONLIN")
                keyhit% = 15%
                return
L34950:     if keyhit% <> 15% then return
                call "PRNTSCRN"
                return

        REM *************************************************************~
            *             F O R M A T    S T A T E M E N T S            *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE #09 -- BCKBUFFR  (Read Only)       */~
            XX(20),         /* Buffer Front-end                        */~
            XX(9),          /* Customer Code                           */~
            XX(16),         /* Sales Order Mumber                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship-To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* Payment Terms                           */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(9),          /* Sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(4),        /* Salesman Codes                          */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region code                             */~
            CH(200),        /* Variable Fields                         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Cancellation Date                       */~
            CH(6),          /* Due Date default                        */~
            CH(6),          /* Date Released                           */~
            CH(6),          /* Originally Input On (date)              */~
            CH(3),          /* Originally Input By (user)              */~
            CH(6),          /* Last Modified On (date)                 */~
            CH(3),          /* Last Modified By (user)                 */~
            XX(9),          /* Adjustment Reason Code                  */~
            CH(1),          /* Export Flag                             */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Order Discount Percent                  */~
            PD(14,4),       /* Open Order Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            BI(2),          /* Last Sequence Number Used               */~
            BI(4),          /* Next BOL Number                         */~
            POS(913), CH(4),/* Currency code                           */~
            CH(1),          /* How put on hold                         */~
            CH(1),          /* PM Surcharge SO Flag                    */~
            CH(1)           /* PM Surcharge INV Flag                   */

L35410: FMT                 /* FILE #09 -- BCKBUFFR                    */~
            CH(20),         /* User ID, Date/Time, PGM or Print Flag   */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* Sales Order Mumber                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship-To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* Payment Terms                           */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(9),          /* Sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(4),        /* Salesman Codes                          */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region code                             */~
            CH(200),        /* Variable Fields                         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Cancellation Date                       */~
            CH(6),          /* Due Date default                        */~
            CH(6),          /* Date Released                           */~
            CH(6),          /* Originally Input On (date)              */~
            CH(3),          /* Originally Input By (user)              */~
            CH(6),          /* Last Modified On (date)                 */~
            CH(3),          /* Last Modified By (user)                 */~
            CH(9),          /* Adjustment Reason Code                  */~
            CH(1),          /* Export Flag                             */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Order Discount Percent                  */~
            PD(14,4),       /* Open Order Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            BI(2),          /* Last Sequence Number Used               */~
            BI(4),          /* Next BOL Number                         */~
            CH(2),          /* Customer Type Code                      */~
            CH(9),          /* Account X-Ref                           */~
            CH(4),          /* Currency code                           */~
            CH(1),          /* How put on hold                         */~
            CH(1),          /* PM Surcharge SO Flag                    */~
            CH(1),          /* PM Surcharge INV Flag                   */~
            CH(101)         /* Filler                                  */

L35830: FMT                 /* FILEs #10 & #06 -- BCKBUF2 and BCKLINES */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* Sales Order number                      */~
            CH(3),          /* Sequence Number                         */~
            CH(3),          /* Item Number                             */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part Number description                 */~
            CH(4),          /* Category code                           */~
            PD(14,4),       /* Order Quantity                          */~
            PD(14,4),       /* Quantity Shipped (total)                */~
            PD(14,4),       /* Quantity Open                           */~
            PD(14,4),       /* Quantity scheduled for Shipment         */~
            PD(14,4),       /* Quantity Allocated                      */~
            PD(14,4),       /* Quantity Pre-Invoiced                   */~
            PD(14,4),       /* Unit Price @ Stocking UOM               */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor (Pricing to Stkng)    */~
            PD(14,4),       /* Unit Price                              */~
            PD(14,4),       /* Pricing Discount Percent                */~
            CH(1),          /* Taxable (y/n) indicator                 */~
            CH(9),          /* Sales Account number                    */~
            CH(9),          /* Discounts Account                       */~
            CH(6),          /* Due Date - Original                     */~
            CH(6),          /* Due Date - Current                      */~
            CH(6),          /* Required Ship Date                      */~
            CH(6),          /* Lot Number                              */~
            CH(8),          /* Code/# of a Project                     */~
            CH(8),          /* Filler                                  */~
            CH(1),          /* Demand Type                             */~
            CH(1),          /* Priority Code                           */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Allocation Flag                         */~
            CH(8),          /* Invoice Number                          */~
            CH(8),          /* Estimate Number                         */~
            CH(3),          /* Specific BOM version                    */~
            CH(11),         /* MLQ number and line                     */~
            CH(1),          /* Ship priority code                      */~
            BI(1),          /* Precious Metal Surcharge Added Flag     */~
            CH(22)          /* Filler                                  */

L36170: FMT                 /* FILE #41 -- BCKLNCUR                    */~
            CH(4),          /* Currency code                           */~
            CH(16),         /* Sales Order number                      */~
            CH(3),          /* Sequence Number                         */~
            PD(14,4),       /* Unit Price @ Stocking UOM               */~
            PD(14,4),       /* Unit Price                              */~
            CH(6),          /* Conversion factor effective date        */~
            PD(14,7),       /* Currency conversion factor              */~
            PD(14,7),       /* # Units per statutory currency unit     */~
            CH(39)          /* Filler                                  */

L36590:     FMT /* File #51- CCRMASTR Master file (input/output)       */~
                CH(9),         /*   1/ 9-   Customer Code (Key)        */~
                PD(14,4),      /*  10/ 8-   Total Sales                */~
                PD(14,4),      /*  18/ 8-   Total Credits              */~
                PD(14,4),      /*  26/ 8-   High Credit Limit          */~
                CH(6),         /*  34/ 6-   High Credit Limit Date     */~
                PD(14,4),      /*  40/ 8-   Last Invoice Amount        */~
                CH(8),         /*  48/ 8-   Last Invoice Number        */~
                PD(14,4),      /*  56/ 8-   Last Payment Amount        */~
                CH(10),        /*  64/10-   Last Payment Check #       */~
                PD(14,4),      /*  74/ 8-   Average # Days to Pay      */~
                BI(2),         /*  82/ 2-   # Invoices in Average Days */~
                5*CH(6),       /*  84/30-   'Dynamic' dates fr CUSTOMER*/~
                4*PD(14,4),    /* 114/32-   'Dynamic' amnts fr CUSTOMER*/~
                CH(3),         /* 146/ 3-   User Last Modified         */~
                CH(6),         /* 149/ 6-   Date Last Modified         */~
                CH(46)         /* 155/46-   Filler                     */

        REM *************************************************************~
            * Extra Lines Conflict Warnings/Errors                      *~
            *************************************************************
        extra_load_conflict

L37050: % Current No. of Lines: ###  (+ Implied Lines: ###) : Total #####

            put askmsg$(1%) using L37050, maxlines%, total_e_lines%,      ~
                                         maxlines% + total_e_lines%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press any PF key to confirm and continue"
            u3% = 2%
            call "ASKUSER" (u3%, "* * * MAXIMUM LINES CONFLICT * * *",   ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            return

        extra_append_conflict
            put askmsg$(1%) using L37050, c%, total_e_lines% + temp%,     ~
                                         c% + total_e_lines% + temp%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press PF16 to continue, Press RETURN to" &    ~
                          " re-enter Part Code"
L37220:     u3% = 2%
            call "ASKUSER" (u3%, "* * * APPEND LINES CONFLICT * * *",    ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            if u3% = 16% then return
            if u3% =  0% then return
               goto L37220

L40000: REM *************************************************************~
            *        H E A D E R  /  S U M M A R Y   S C R E E N        *~
            *-----------------------------------------------------------*~
            * Abbreviated Header and Line Item Summary Screen.          *~
            *************************************************************

            str(line1$,21,15) = "Main Screen"
            str(line1$,37,14) = "Crncy is" & " " & currency$
L40033:     wndw% = 10%  :  gosub set_summary
            gosub setpf1
            init(hex(86)) hfac$()
            if fieldnr% = 0% then L40130

            init(hex(8c)) hfac$()
            hfac$(fieldnr%) = hex(81)
            if fieldnr% = 1% then hfac$(2%) = hex(81)

L40130:     accept                                                       ~
               at (01,02), fac(hex(ac)), line1$                 , ch(79),~
               at (09,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (02,02), "Customer:",                                  ~
               at (02,12), fac(hfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (02,24), "SO: ",                                       ~
               at (02,28), fac(hfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (03,02), "Ship-to :",                                  ~
               at (03,12), fac(hfac$( 3)), shipto$(1)           , ch(30),~
               at (04,12), fac(hfac$( 3)), shipto$(2)           , ch(30),~
               at (05,12), fac(hfac$( 3)), shipto$(3)           , ch(30),~
               at (06,12), fac(hfac$( 3)), shipto$(4)           , ch(30),~
               at (07,12), fac(hfac$( 3)), shipto$(5)           , ch(30),~
               at (08,12), fac(hfac$( 3)), str(shipto$(6), 1,17), ch(17),~
               at (08,30), fac(hfac$( 3)), str(shipto$(6),19, 2), ch(02),~
               at (08,33), fac(hfac$( 3)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (02,47), "Store Code",                                 ~
               at (02,60), fac(hfac$( 4)), store$               , ch(03),~
                                                                         ~
               at (02,65), "Export?",                                    ~
               at (02,73), fac(hfac$( 5)), export$              , ch(01),~
                                                                         ~
               at (03,47), "PO Number",                                  ~
               at (03,60), fac(hfac$( 6)), po$                  , ch(16),~
                                                                         ~
               at (04,47), "How Ship",                                   ~
               at (04,60), fac(hfac$( 7)), howship$             , ch(20),~
                                                                         ~
               at (05,47), "FOB",                                        ~
               at (05,60), fac(hfac$( 8)), fob$                 , ch(20),~
                                                                         ~
               at (06,47), "Order Date",                                 ~
               at (06,60), fac(hfac$( 9)), orderdate$           , ch(08),~
               at (06,69), "Due",                                        ~
               at (06,73), fac(hfac$(10)), dfltdue$             , ch(08),~
                                                                         ~
               at (07,47), "Req'd Ship",                                 ~
               at (07,60), fac(hfac$(11)), dfltship$            , ch(08),~
               at (07,70), fac(hex(8c))  , weekdayh$            , ch(09),~
                                                                         ~
               at (08,47), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (08,67), fac(hfac$(12)), pm_so$               , ch(01),~
               at (08,70), fac(hex(8c))  , pm_hdr_dsply2$       , ch(07),~
               at (08,78), fac(hfac$(13)), pm_inv$              , ch(01),~
                                                                         ~
               at (10,02), fac(hex(ac))  , smryh$               , ch(79),~
               at (11,02), fac(sfac$( 1)), smry$( 1)            , ch(79),~
               at (12,02), fac(sfac$( 2)), smry$( 2)            , ch(79),~
               at (13,02), fac(sfac$( 3)), smry$( 3)            , ch(79),~
               at (14,02), fac(sfac$( 4)), smry$( 4)            , ch(79),~
               at (15,02), fac(sfac$( 5)), smry$( 5)            , ch(79),~
               at (16,02), fac(sfac$( 6)), smry$( 6)            , ch(79),~
               at (17,02), fac(sfac$( 7)), smry$( 7)            , ch(79),~
               at (18,02), fac(sfac$( 8)), smry$( 8)            , ch(79),~
               at (19,02), fac(sfac$( 9)), smry$( 9)            , ch(79),~
               at (20,02), fac(sfac$(10)), smry$(10)            , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 22% then L40406
              if edit% <> 2% then L40406
                  if cms% = 1% then L40402
                      cms% = 1%  :  ref% = 0%
                      smryh$ = smryhdr$
                      goto L40033
L40402:           cms% = 0%  :  ref% = 1%
                  smryh$ = "Seq Reference Part Number      " &           ~
                                                    str(smryhdr$,32%,48%)
                  goto L40033
L40406:     if keyhit% <> 26% then goto L40410
                gosub call_customer_credit
                goto L40130

L40410:         gosub pf1315
                if keyhit% = 15% then L40130
                return

        setpf1
        if edit% = 2% then L40530         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit   (16)Exit Program"
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0f10ffffff1a00)
           if fieldnr% > 3% then L40510
                str(pf$(2),18,17) = " " : str(pfkey$, 4, 1) = hex(ff)
L40510:    if fieldnr% = 1% then goto L40513
                str(pf$(3),64,16) = " " : str(pfkey$,16, 1) = hex(ff)
L40513:    if fieldnr% > 1% then L40520
                str(pf$(3),42,19) = " " : str(pfkey$,19, 1) = hex(ff)
L40520:    return

L40530:  if fieldnr% > 0% then L40585     /* Edit Mode- Select Option   */
           pf$(1) = "(2)First (5)Next  (9)Header Detail              (1"&~
                    "2)Delete (18)Part (13)Instruc"
           pf$(2) = "(3)Last  (6)Down (10)Cancel Order (14)Functions (2"&~
                    "6)Cust Crdt       (15)Prt Scr"
           pf$(3) = "(4)Prev  (7)Up   (11)Append Lines (22)Pt Toggle (2"&~
                    "5)Manage Txt      (16)End Odr"
           pfkey$ = hex(ff020304050607ff090a0b0c0d0e0f10ff191d161a1200)
           if refflag% = 1% then L40566
               str(pf$(3%),35%,14%) = " "
               str(pfkey$,20%,1%)   = hex(ff)
L40566:    str(pf$(3),63,1) = hex(8c) /* Hilight PF(25) if text exists */
           txt$ = textid$ : gosub text_prompt
           str(pf$(3),48,1) = fac25$
           if allow_delete$ <> "N" then L40575
               str(pf$(1), 49, 10) = " "
               str(pfkey$, 12,  1) = hex(ff)
L40575:    return

                                         /* Edit Mode- Field Enabled   */
L40585:    pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(ffffffffffffffffffffffff0dff0fffffffff00)
           return

L41000: REM *************************************************************~
            *           D E T A I L   H E A D E R   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Full Header Screen for Editing.                           *~
            *************************************************************

            str(line1$,21,15) = "Header Detail"
            str(line1$,37,14) = "Crncy is" & " " & currency$
            gosub setpf2
            init(hex(86)) hfac$()
            if fieldnr% = 0% then L41175

            init(hex(8c)) hfac$()
            hfac$(fieldnr%) = hex(81)
            if fieldnr% = 23% then hfac$(fieldnr%) = hex(80)

L41175:     accept                                                       ~
               at (01,02), fac(hex(ac)), line1$                 , ch(79),~
               at (09,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (02,02), "Customer:",                                  ~
               at (02,12), fac(hfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (02,24), "SO: ",                                       ~
               at (02,28), fac(hfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (03,02), "Ship-to :",                                  ~
               at (03,12), fac(hfac$( 3)), shipto$(1)           , ch(30),~
               at (04,12), fac(hfac$( 3)), shipto$(2)           , ch(30),~
               at (05,12), fac(hfac$( 3)), shipto$(3)           , ch(30),~
               at (06,12), fac(hfac$( 3)), shipto$(4)           , ch(30),~
               at (07,12), fac(hfac$( 3)), shipto$(5)           , ch(30),~
               at (08,12), fac(hfac$( 3)), str(shipto$(6), 1,17), ch(17),~
               at (08,30), fac(hfac$( 3)), str(shipto$(6),19, 2), ch(02),~
               at (08,33), fac(hfac$( 3)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (02,47), "Store Code",                                 ~
               at (02,60), fac(hfac$( 4)), store$               , ch(03),~
                                                                         ~
               at (02,65), "Export?",                                    ~
               at (02,73), fac(hfac$( 5)), export$              , ch(01),~
                                                                         ~
               at (03,47), "PO Number",                                  ~
               at (03,60), fac(hfac$( 6)), po$                  , ch(16),~
                                                                         ~
               at (04,47), "How Ship",                                   ~
               at (04,60), fac(hfac$( 7)), howship$             , ch(20),~
                                                                         ~
               at (05,47), "FOB",                                        ~
               at (05,60), fac(hfac$( 8)), fob$                 , ch(20),~
                                                                         ~
               at (06,47), "Order Date",                                 ~
               at (06,60), fac(hfac$( 9)), orderdate$           , ch(08),~
               at (06,69), "Due",                                        ~
               at (06,73), fac(hfac$(10)), dfltdue$             , ch(08),~
                                                                         ~
               at (07,47), "Req'd Ship",                                 ~
               at (07,60), fac(hfac$(11)), dfltship$            , ch(08),~
               at (07,70), fac(hex(8c))  , weekdayh$            , ch(09),~
                                                                         ~
               at (08,47), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (08,67), fac(hfac$(12)), pm_so$               , ch(01),~
               at (08,70), fac(hex(8c))  , pm_hdr_dsply2$       , ch(07),~
               at (08,78), fac(hfac$(13)), pm_inv$              , ch(01),~
                                                                         ~
               at (10,02), "Sold-to :",                                  ~
               at (10,12), fac(hfac$(14)), soldto$(1)           , ch(30),~
               at (11,12), fac(hfac$(14)), soldto$(2)           , ch(30),~
               at (12,12), fac(hfac$(14)), soldto$(3)           , ch(30),~
               at (13,12), fac(hfac$(14)), soldto$(4)           , ch(30),~
               at (14,12), fac(hfac$(14)), soldto$(5)           , ch(30),~
               at (15,12), fac(hfac$(14)), str(soldto$(6), 1,17), ch(17),~
               at (15,30), fac(hfac$(14)), str(soldto$(6),19, 2), ch(02),~
               at (15,33), fac(hfac$(14)), str(soldto$(6),22, 9), ch(09),~
                                                                         ~
               at (10,47), "Cancel Date",                                ~
               at (10,60), fac(hfac$(15)), canceldate$          , ch(08),~
                                                                         ~
               at (11,47), "Price Code",                                 ~
               at (11,60), fac(hfac$(16)), pc$                  , ch(01),~
                                                                         ~
               at (12,47), "Order Disc %",                               ~
               at (12,60), fac(hfac$(17)), orderdisc$           , ch(06),~
                                                                         ~
               at (13,47), "Paymnt Terms",                               ~
               at (13,60), fac(hfac$(18)), terms$               , ch(20),~
                                                                         ~
               at (14,47), "Region Code",                                ~
               at (14,60), fac(hfac$(19)), region$              , ch(04),~
                                                                         ~
               at (15,47), "Salesman / %",                               ~
               at (15,60), fac(hfac$(20)), salesman$(1)         , ch(04),~
               at (15,66), fac(hfac$(20)), comm$(1)             , ch(03),~
               at (16,60), fac(hfac$(20)), salesman$(2)         , ch(04),~
               at (16,66), fac(hfac$(20)), comm$(2)             , ch(03),~
               at (17,60), fac(hfac$(20)), salesman$(3)         , ch(04),~
               at (17,66), fac(hfac$(20)), comm$(3)             , ch(03),~
                                                                         ~
               at (17,02), "Sales Account Default",                      ~
               at (17,30), fac(hfac$(21)), dfltsales$           , ch(12),~
                                                                         ~
               at (18,02), "Sales Discounts Account",                    ~
               at (18,30), fac(hfac$(22)), discacct$            , ch(12),~
                                                                         ~
               at (19,02), "Shipping Instructions",                      ~
               at (19,30), fac(hfac$(23)), shipinstr$(1)        , ch(50),~
               at (20,30), fac(hfac$(23)), shipinstr$(2)        , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L41630
                gosub call_customer_credit
                goto L41175

L41630:         gosub pf1315
                if keyhit% = 15% then L41175
                return

        setpf2
         if fieldnr% > 0% then L41790     /* Edit Mode- Select Option   */
           pf$(1) = "                   (9)Variable Fields             "&~
                    "             (13)Instructions"
           pf$(2) = "                                        (26)Custom"&~
                    "er Credit    (15)Print Screen"
           pf$(3) = "                                        (25)Manage"&~
                    " Text        (16)Summary Scrn"
           str(pf$(3),56,1) = hex(8c) /* Hilight PF(25) if text exists */
           txt$ = textid$ : gosub text_prompt
           str(pf$(3),40,1) = fac25$
           pfkey$ = hex(ffffffffffffffff09ffffff0dff0f10ff191d1a00)
           return

                                         /* Edit Mode- Field Enabled   */
L41790:    pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(ffffffffffffffffffffffff0dff0fffffffff00)
           return

L42000: REM *************************************************************~
            *             L I N E   I T E M   S C R E E N               *~
            *-----------------------------------------------------------*~
            * Screen for entering and editing of line items.            *~
            *************************************************************

            xfac$ = hex(9c)  :  lastkey% = 0%
            str(line1$,21,15) = "Line Entry/Edit"
            if errormsg$ <> " " then warn$ = errormsg$
            str(line1$,37,14) = " "
            wndw% = 3%  :  gosub set_summary
            init(hex(84)) hfac$()
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf3
            on fieldnr%      gosub  L42390,         /* Part Code        */~
                                    L42380,         /* Part Description */~
                                    L42390,         /* Part Category    */~
                                    L42390,         /* Due Date         */~
                                    L42390,         /* Required Ship    */~
                                    L42380,         /* Stocking UOM     */~
                                    L42380,         /* Pricing UOM      */~
                                    L42400,         /* Conversion Factor*/~
                                    L42380,         /* Order Qty        */~
                                    L42380,         /* Alloc Qty        */~
                                    L42380,         /* Open Qty         */~
                                    L42390,         /* Lot Number       */~
                                    L42380,         /* Shipped Qty      */~
                                    L42390,         /* Currency code    */~
                                    L42400,         /* Unit Price       */~
                                    L42400,         /* Line Disc %      */~
                                    L42390,         /* Taxable?         */~
                                    L42380,         /* PO Item          */~
                                    L42390,         /* Project Number   */~
                                    L42390,         /* Sales Account    */~
                                    L42390,         /* Sales Discs Acct */~
                                    L42390,         /* Shipping Priority*/~
                                    L42390,         /* Demand Priority  */~
                                    L42390,         /* Demand Type      */~
                                    L42390,         /* Planning BOM     */~
                                    L42390          /* Horizon Date     */
            goto L42420
L42380:                             lfac$(fieldnr%) = hex(80) : return
L42390:                             lfac$(fieldnr%) = hex(81) : return
L42400:                             lfac$(fieldnr%) = hex(82) : return

L42420:     accept                                                       ~
               at (01,02), fac(hex(ac)), line1$                 , ch(79),~
               at (09,02), fac(hex(94)), warn$                  , ch(79),~
                                                                         ~
               at (02,02), "Customer:",                                  ~
               at (02,12), fac(hfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (02,24), "SO: ",                                       ~
               at (02,28), fac(hfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (03,02), "Ship-to :",                                  ~
               at (03,12), fac(hfac$( 3)), shipto$(1)           , ch(30),~
               at (04,12), fac(hfac$( 3)), shipto$(2)           , ch(30),~
               at (05,12), fac(hfac$( 3)), shipto$(3)           , ch(30),~
               at (06,12), fac(hfac$( 3)), shipto$(4)           , ch(30),~
               at (07,12), fac(hfac$( 3)), shipto$(5)           , ch(30),~
               at (08,12), fac(hfac$( 3)), str(shipto$(6), 1,17), ch(17),~
               at (08,30), fac(hfac$( 3)), str(shipto$(6),19, 2), ch(02),~
               at (08,33), fac(hfac$( 3)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (02,47), "Store Code",                                 ~
               at (02,60), fac(hfac$( 4)), store$               , ch(03),~
                                                                         ~
               at (02,65), "Export?",                                    ~
               at (02,73), fac(hfac$( 5)), export$              , ch(01),~
                                                                         ~
               at (03,47), "PO Number",                                  ~
               at (03,60), fac(hfac$( 6)), po$                  , ch(16),~
                                                                         ~
               at (04,47), "How Ship",                                   ~
               at (04,60), fac(hfac$( 7)), howship$             , ch(20),~
                                                                         ~
               at (05,47), "FOB",                                        ~
               at (05,60), fac(hfac$( 8)), fob$                 , ch(20),~
                                                                         ~
               at (06,47), "Order Date",                                 ~
               at (06,60), fac(hfac$( 9)), orderdate$           , ch(08),~
               at (06,69), "Due",                                        ~
               at (06,73), fac(hfac$(10)), dfltdue$             , ch(08),~
                                                                         ~
               at (07,47), "Req'd Ship",                                 ~
               at (07,60), fac(hfac$(11)), dfltship$            , ch(08),~
               at (07,70), fac(hex(8c))  , weekdayh$            , ch(09),~
                                                                         ~
               at (08,47), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (08,67), fac(hfac$(12)), pm_so$               , ch(01),~
               at (08,70), fac(hex(8c))  , pm_hdr_dsply2$       , ch(07),~
               at (08,78), fac(hfac$(13)), pm_inv$              , ch(01),~
                                                                         ~
               at (10,02), fac(hex(ac))  , smryhdr$             , ch(79),~
               at (11,02), fac(sfac$( 1)), smry$( 1)            , ch(79),~
               at (12,02), fac(sfac$( 2)), smry$( 2)            , ch(79),~
               at (13,02), fac(sfac$( 3)), smry$( 3)            , ch(79),~
               at (14,02), fac(hex(80))  , temp$, /* Pos Cursor        */~
                                                                         ~
               at (14,02), "Part",                                       ~
               at (14,08), fac(lfac$( 1)), part$(c%)            , ch(25),~
                                                                         ~
               at (14,34), "Descr",                                      ~
               at (14,40), fac(lfac$( 2)), descr$(c%)           , ch(32),~
               at (14,74), fac(hex(8c))  , nonstockmsg$(c%)     , ch(06),~
                                                                         ~
               at (15,02), "Catgy",                                      ~
               at (15,08), fac(lfac$( 3)), cat$(c%)             , ch(04),~
                                                                         ~
               at (15,13), "Due",                                        ~
               at (15,17), fac(lfac$( 4)), duedate$(c%)         , ch(08),~
                                                                         ~
               at (15,26), "Ship",                                       ~
               at (15,31), fac(lfac$( 5)), shipdate$(c%)        , ch(08),~
                                                                         ~
               at (15,40), "UOMS: Stkng",                                ~
               at (15,52), fac(lfac$( 6)), stkuom$(c%)          , ch(04),~
               at (15,57), "Prcg",                                       ~
               at (15,62), fac(lfac$( 7)), priceuom$(c%)        , ch(04),~
               at (15,67), "Cnv",                                        ~
               at (15,71), fac(lfac$( 8)), conv$                , ch(10),~
                                                                         ~
               at (16,02), "Order",                                      ~
               at (16,08), fac(lfac$( 9)), order$               , ch(10),~
               at (16,19), "Alloc",                                      ~
               at (16,25), fac(lfac$(10)), allocqty$            , ch(10),~
               at (16,36), "Open",                                       ~
               at (16,41), fac(lfac$(11)), openqty$             , ch(10),~
               at (16,52), "Lot",                                        ~
               at (16,56), fac(lfac$(12)), str(lot$(c%),,ll%),           ~
               at (16,63), "Invoicd",                                    ~
               at (16,71), fac(hex(8c)),   ship$                , ch(10),~
                                                                         ~
               at (17,02), "Curr",                                       ~
               at (17,07), fac(lfac$(14)), currency$            , ch(04),~
               at (17,12), "Price",                                      ~
               at (17,18), fac(lfac$(15)), price$               , ch(10),~
               at (17,29), "Disc%",                                      ~
               at (17,35), fac(lfac$(16)), linedisc$            , ch(06),~
               at (17,42), "OpnExt",                                     ~
               at (17,49), fac(hex(8c))  , ext$                 , ch(10),~
               at (17,60), "Tax?",                                       ~
               at (17,65), fac(lfac$(17)), taxable$(c%)         , ch(01),~
               at (17,67), "Sch",                                        ~
               at (17,71), fac(hex(8c)),   qtyschld$            , ch(10),~
                                                                         ~
               at (18,02), "PO Ln",                                      ~
               at (18,08), fac(lfac$(18)), item$(c%)            , ch(03),~
               at (18,13), "Proj",                                       ~
               at (18,18), fac(lfac$(19)), project$(c%)         , ch(08),~
               at (18,29), "Sales Acct",                                 ~
               at (18,40), fac(lfac$(20)), salesacct$(c%)       , ch(12),~
               at (18,53), "Sales Disc Acct",                            ~
               at (18,69), fac(lfac$(21)), discacctl$(c%)       , ch(12),~
                                                                         ~
               at (19,02), "Ship Prty",                                  ~
               at (19,12), fac(lfac$(22)), shipcode$(c%)        , ch(01),~
               at (19,14), "Plan: Prty",                                 ~
               at (19,25), fac(lfac$(23)), priority$(c%)        , ch(01),~
               at (19,27), "Demand",                                     ~
               at (19,34), fac(lfac$(24)), demtype$(c%)         , ch(01),~
               at (19,36), "BOM",                                        ~
               at (19,40), fac(lfac$(25)), bomid$               , ch(03),~
               at (19,44), "Last Planned",                               ~
               at (19,57), fac(hex(8c))  , lastplan$            , ch(08),~
               at (19,66), "P.C.D.",                                     ~
               at (19,73), fac(hex(8c))  , pcd$                 , ch(08),~
                                                                         ~
               at (20,08), "Horizon to",                                 ~
               at (20,19), fac(lfac$(26)), horizon$             , ch(08),~
               at (20,29), "Qty Available",                              ~
               at (20,39), fac(hex(8c))  , avail$               , ch(10),~
               at (20,51), "Status",                                     ~
               at (20,59), fac(hex(8c))  , demstatusmsg$        , ch(22),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               at (24,32), fac(xfac$)  ,   mfgcode$             , ch(13),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 23% then goto L43664
                   lastkey% = keyhit%
                   init(hex(81)) xfac$  :  init(hex(84)) lfac$()
                   inpmessage$ = "Enter The Manufacturer's Code For Thi"&~
                                 "s Part Number."
                   goto L42420

L43664:        if keyhit% <> 26% then goto L43671
                    gosub call_customer_credit
                    goto L42420

L43671:        if keyhit% <> 17% then L43675
                    gosub call_bckprcsb
                    goto L42420

L43675:        warn$ = " "

                if keyhit% <> 3% then L43770
                   u3% = today%
                   call "PLNFLSUB" (planflags$(), u3%)
                      mat holdflags$ = planflags$
                      get str(planflags$(), 281) using L43754, lta%()
L43754:                   FMT 26*BI(4)
                   goto L42420

L43770:        if keyhit% <> 25% then L43775
                  gosub edit_text_line : goto L42420

L43775:        if keyhit% <> 21% then L43780
                  gosub display_corebank
                  goto L42420

L43780:        if keyhit% <> 20% then L43790
                  gosub display_pm_surcharges
                  goto L42420

L43790:         gosub pf1315
                if keyhit% = 15% then L42420
                return

        setpf3
        if xref% = 0% then L43830
            init(hex(00)) plowkey$
            str(plowkey$,1%,1%)="C"  :  str(plowkey$,2%,9%)=str(cuscode$)
            call "READ104" (#45, plowkey$, f1%(45%))
                if f1%(45%) = 0% then L43830
            if str(plowkey$,1%,10%)= "C" & str(cuscode$) then cust% = 1%
L43830: if edit% = 2% then L43940         /* Input Mode                 */
           pf$(1) = "(1)Restart Line  (4)Prev Field    (14)Functions   "&~
                    "(17)Prices        (13)Instruc"
           pf$(2) = "             (22)Cust Pt Xref  (23)MFG Pt Xref    "&~
                    "(25)Line Text     (15)Prt Scr"
           pf$(3) = "                                                  "&~
                    "(26)Cust Crdt     (16)Ext Apd"
           pfkey$=hex(01ffff04ffffffffffffffff0d0e0f10111912ffff16171a00)
           if xref% = 1% then L43891
               str(pf$(2%),14%,16%) = " "  : str(pfkey$,22%,1%) = hex(ff)
               str(pf$(2%),32%,16%) = " "  : str(pfkey$,23%,1%) = hex(ff)
               goto L43901
L43891:    if cust% = 1% then L43894
               str(pf$(2%),14%,16%) = " "  : str(pfkey$,22%,1%) = hex(ff)
               goto L43901
L43894:    if fieldnr% = 1% then L43901
              str(pf$(1%),51%,17%) = "(17/18)Price/Part"
              str(pfkey$,19%,1%) = hex(12)
              str(pf$(2%),14%,16%) = " "  : str(pfkey$,22%,1%) = hex(ff)
              str(pf$(2%),32%,16%) = " "  : str(pfkey$,23%,1%) = hex(ff)
L43901:    if core% = 0% or fieldnr% = 1% then L43907
                str(pf$(3%),1%,13%) = "(21)Core Bank"
                str(pfkey$,21%,1%) = hex(15)
L43907:    gosub hilite_line_item_text
           if fieldnr% < 2% then goto L43910
                str(pf$(3),64,16) = " " : str(pfkey$,16, 1) = hex(ff)
L43910:    if fieldnr% > 1% then return
                str(pf$(1),18,17) = " " : str(pfkey$, 4,1) = hex(ff)
                str(pf$(1),51,10) = " " : str(pfkey$,17,1) = hex(ff)
                str(pf$(2),51,13) = " " : str(pfkey$,18,1) = hex(ff)
           return

L43940:  if fieldnr% > 0% then L44100     /* Edit Mode- Select Option   */
           pf$(1) = "(3)Plan Switches (9)How Many  (12)Delete Ln (17/18"&~
                    ")Price/Part             (13)I"
           pf$(2) = "(6/7)Prev/Next  (10)Plan One  (14)Functions       "&~
                    "                  (15)Prt Scr"
           pf$(3) = "(8)Feasibility  (11)Plan All                (26)Cu"&~
                    "st Crdt           (16)Summary"
           gosub hilite_line_item_text
           pfkey$=hex(ffff03ff12060708090a0b0c0d0e0f1011191d0018ff1a1bff)
               xfac$ = fac25$
               mfgcode$ = "(25)Line Text"
           if allow_delete$ <> "N" then L44020
               str(pf$(1), 31, 13) = " "
               str(pfkey$, 12,  1) = hex(ff)
L44020:    if append% <> 1% then L44062
                str(pf$(3),69,11) = "(16)Next Ln"
                str(pf$(2), 1,17) = " "   :  str(pfkey$, 6,2) = hex(ffff)
                str(pf$(1),31,13) = " "   :  str(pfkey$,12,1) = hex(ff)
L44062:    if core% = 0% then L44068
                str(pf$(2%),45%,13%) = "(21)Core Bank"
                str(pfkey$,22%,1%) = hex(15)
L44068:    if pm_adj%(c%) <> 1% then L44080
                str(pf$(1%),64%,11%) = "(20)PM Chrg"
                str(pfkey$,25%,1%) = hex(14)
L44080:    return
                                         /* Edit Mode- Field Enabled   */
L44100:    pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(ffffffffffffffffffffffff0dff0fffffffff00)
           return

        call_bckprcsb
            call "BCKPRCSB" (cuscode$, custype$, part$(c%), cat$(c%),    ~
                pc$, currency$, currtype$, order(c%), #2, #1, #4, #40)
            return

        hilite_line_item_text
           str(pf$(3),48,1) = hex(8c) /* Hilight PF(25) if text exists */
           txt$ = textidl$(c%) : gosub text_prompt
           str(pf$(3),34,1) = fac25$
            return

L45000: REM *************************************************************~
            *           D A T A   S A V E   S C R E E N                 *~
            *-----------------------------------------------------------*~
            * Some last decisions to make and data to fill in.          *~
            *************************************************************

            str(line1$,21,15) = "Order Recap"
            str(line1$,37,14) = " "
            init (hex(84)) hfac$()
            init (hex(86)) lfac$()
            gosub setpf4 : gosub set_currency_description
            if fieldnr% = 0% then L45150
                init(hex(8c)) lfac$()
                lfac$(fieldnr%) = hex(81)

L45150:     accept                                                       ~
               at (01,02), fac(hex(ac)), line1$                 , ch(79),~
               at (09,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (02,02), "Customer:",                                  ~
               at (02,12), fac(hfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (02,24), "SO: ",                                       ~
               at (02,28), fac(hfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (03,02), "Ship-to :",                                  ~
               at (03,12), fac(hfac$( 3)), shipto$(1)           , ch(30),~
               at (04,12), fac(hfac$( 3)), shipto$(2)           , ch(30),~
               at (05,12), fac(hfac$( 3)), shipto$(3)           , ch(30),~
               at (06,12), fac(hfac$( 3)), shipto$(4)           , ch(30),~
               at (07,12), fac(hfac$( 3)), shipto$(5)           , ch(30),~
               at (08,12), fac(hfac$( 3)), str(shipto$(6), 1,17), ch(17),~
               at (08,30), fac(hfac$( 3)), str(shipto$(6),19, 2), ch(02),~
               at (08,33), fac(hfac$( 3)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (02,47), "Store Code",                                 ~
               at (02,60), fac(hfac$( 4)), store$               , ch(03),~
                                                                         ~
               at (02,65), "Export?",                                    ~
               at (02,73), fac(hfac$( 5)), export$              , ch(01),~
                                                                         ~
               at (03,47), "PO Number",                                  ~
               at (03,60), fac(hfac$( 6)), po$                  , ch(16),~
                                                                         ~
               at (04,47), "How Ship",                                   ~
               at (04,60), fac(hfac$( 7)), howship$             , ch(20),~
                                                                         ~
               at (05,47), "FOB",                                        ~
               at (05,60), fac(hfac$( 8)), fob$                 , ch(20),~
                                                                         ~
               at (06,47), "Order Date",                                 ~
               at (06,60), fac(hfac$( 9)), orderdate$           , ch(08),~
               at (06,69), "Due",                                        ~
               at (06,73), fac(hfac$(10)), dfltdue$             , ch(08),~
                                                                         ~
               at (07,47), "Req'd Ship",                                 ~
               at (07,60), fac(hfac$(11)), dfltship$            , ch(08),~
               at (07,70), fac(hex(8c))  , weekdayh$            , ch(09),~
                                                                         ~
               at (08,47), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (08,67), fac(hfac$(12)), pm_so$               , ch(01),~
               at (08,70), fac(hex(8c))  , pm_hdr_dsply2$       , ch(07),~
               at (08,78), fac(hfac$(13)), pm_inv$              , ch(01),~
                                                                         ~
               at (10,02), fac(hex(80))  , str(temp$,,1),                ~
               at (10,02), fac(hex(8c)),   adjmsg$              , ch(24),~
               at (10,30), fac(lfac$( 1)), adjrsn$              , ch(09),~
               at (10,49), fac(hex(8c)),   adjrsndescr$         , ch(30),~
                                                                         ~
               at (11,02), "Print Docs:  Pick List x     BOL x     Acknow~
        ~ledgements x",                                                   ~
               at (11,25), fac(lfac$( 2)), pickprint$(2%)       , ch(01),~
               at (11,35), fac(lfac$( 2)), pickprint$(3%)       , ch(01),~
               at (11,58), fac(lfac$( 2)), pickprint$(1%)       , ch(01),~
                                                                         ~
               at (12,02), "Currency code",                              ~
               at (12,30), fac(hex(8c)),   currency$            , ch(04),~
               at (12,49), fac(hex(8c)),   currdesc$(1)         , ch(32),~
               at (13,49), fac(hex(8c)),   currdesc$(2)         , ch(32),~
                                                                         ~
               at (14,25), "----ORDER----",                              ~
               at (14,41), "----OPEN -----",                             ~
               at (15,02), "Gross Order Amount",                         ~
               at (15,25), fac(hex(84)), torder(1),  pic(-######,###.00),~
               at (15,41), fac(hex(84)), topen (1),  pic(-######,###.00),~
               at (16,02), "Line Item Discounts",                        ~
               at (16,25), fac(hex(84)), torder(2),  pic(-######,###.00),~
               at (16,41), fac(hex(84)), topen (2),  pic(-######,###.00),~
               at (17,02), "Order Level Discounts",                      ~
               at (17,25), fac(hex(a4)), torder(3),  pic(-######,###.00),~
               at (17,41), fac(hex(a4)), topen (3),  pic(-######,###.00),~
               at (18,02), "Net Order Amounts",                          ~
               at (18,25), fac(hex(84)), torder(4),  pic(-######,###.00),~
               at (18,41), fac(hex(84)), topen (4),  pic(-######,###.00),~
                                                                         ~
               at (14,57), "-Statutory Credit Info-",                    ~
               at (15,57), "Bill-to Cust",                               ~
               at (15,70), fac(hex(84))  , billto$              , ch(09),~
               at (16,57), "A/R Balance:",                               ~
               at (16,70), fac(hex(84))  , ar(1)       , pic(-#####,###),~
               at (17,57), "Open Orders:",                               ~
               at (17,70), fac(hex(a4))  , oo(1)       , pic(-#####,###),~
               at (18,57), "      Total:",                               ~
               at (18,70), fac(hex(84))  , totalar1    , pic(-#####,###),~
               at (19,57), fac(hex(84))  , crmsg$(1),                    ~
               at (20,57), fac(hex(84))  , crmsg$(2),                    ~
                                                                         ~
               at (20,10), fac(hex(84)), tlines%(1),           pic(###0),~
               at (20,15), "Lines remaining on order.",                  ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 24% then L46032
                crhold$ = " " : crmsg$() = " "
                str(pf$(2%),42%,20%) = " " : str(pfkey$,18%,1%) = hex(ff)
                goto L45150

L46032:     if keyhit% <> 26% then goto L46040
                gosub call_customer_credit
                goto L45150

L46040:         gosub pf1315
                if keyhit% = 15% then L45150
                return

        setpf4
        if edit% = 2% then L46250         /* Input Mode                 */
           pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "
           pfkey$ = hex(ffffff04ffffffffffffffff0dff0fffffff1a00)
           if fieldnr% > 1% then return
                str(pf$(2),18,17) = " " : str(pfkey$, 4, 1) = hex(ff)
           return

L46250:  if fieldnr% > 0% then L46360     /* Edit Mode- Select Option   */
           pf$(1) = "(1)Return to Header Summary                       "&~
                    "             (13)Instructions"
           pf$(2) = "                                         (24)Overr"&~
                    "ide Cr Hold  (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit   (16)Save Order  "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0f10ff181a00)
           if override_crhld$ = "Y" and crhold$ = "H" and                ~
                                              how_held$ <> "M" then L46330
              str(pf$(2%),42%,20%) = " " : str(pfkey$,18%,1%) = hex(ff)
L46330:    return

                                         /* Edit Mode- Field Enabled   */
L46360:    pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(ffffffffffffffffffffffff0dff0fffffffff00)
           return

        set_currency_description
            init (" ") currdesc$()
            if currency$ = statutory$ then return
            call "DESCRIBE" (#40, currency$, currdesc$(1), 0%, f1%(40))
            call "CONVERT" (convunt, 2.7, str(currdesc$(2), 1,10))
            currdesc$(2) = currdesc$(2) & " " & currency$
            currdesc$(2) = currdesc$(2) & "/" & statutory$
            return

        display_corebank
            ret% = 0%
            close ws
            call "CMSLINK" addr(#44, userid$, "R", "CORDSPLY",           ~
                                "        ", "      ", " ", "N",          ~
                                "                        ",              ~
                                "                ", "N", u3%, ret%)
            return

L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            errormsg$ = " "
            if edit% = 2% then modfld% = 1% /* Fld enabled in EDITMODE */
            on fieldnr%      goto   L50150,         /* Customer Code    */~
                                    L50200,         /* Sales Order #    */~
                                    L50355,         /* Ship-to          */~
                                    L50380,         /* Store Code       */~
                                    L50532,         /* Export Order flag*/~
                                    L50540,         /* PO Number        */~
                                         ,         /* How Ship         */~
                                         ,         /* FOB              */~
                                    L50610,         /* Order Date       */~
                                    L50630,         /* Default Due Date */~
                                    L50670,         /* Default Ship Date*/~
                                    L51360,         /* PM SO Flag       */~
                                    L51410,         /* PM INV Flag      */~
                                    L50850,         /* Sold-to          */~
                                    L50870,         /* Cancel Date      */~
                                    L50910,         /* Pricing Code     */~
                                    L50955,         /* Order Disc %     */~
                                    L51000,         /* Payment Terms    */~
                                    L51035,         /* Region           */~
                                    L51085,         /* Salesmen / %s    */~
                                    L51195,         /* Sales Account    */~
                                    L51235,         /* Sales Disc Acct  */~
                                    L51270          /* Shipping Instr   */
                  return

L50150
*        Test CUSTOMER
            if cuscode$ = " " and so$ <> " " then L50235
              if so$ = " " then L50160
              gosub check_hist
              if errormsg$ <> " " then return
L50160:         mscdescr$ = hex(06) & "Select Customer"
                call "PLOWCODE" (#01, cuscode$, mscdescr$, 0%,.30, f1%(1))
                if f1%(1) = 1% then L50180
                     errormsg$ = "Customer not on file." : return
L50180:         get #01 using L50184, soldto$(), dfltacks$, shipto$(),    ~
                                     billto$, custype$, currency$, parent$
L50184:              FMT XX(39), 6*CH(30), POS(238), CH(1), POS(253),    ~
                         6*CH(30), POS(780), CH(9), POS(1023), CH(2),    ~
                         POS(1045), CH(4), CH(9)
                if curr$ <> "Y" then currency$ = " "
                if so$ = " " then return

L50200
*        Sales Order Number                    SO$
            if so$ <> " " then L50235
                if soassgn$ = "m" then                                   ~
                     errormsg$ = "Sales Order may not be blank."
                if errormsg$ <> " " then return
                     gosub new_order_setup
                     goto  L50320
L50235:     if so$ <> "?" then L50297
                msg$ = hex(06) & "Select Sales Order for Edit"
                str(msg$, 65) = hex(8c) & "File: BCKMASTR"
                if cuscode$ = " " then u3% = 9000% else u3% = 9009%
                readkey$ = str(cuscode$) & hex(00)
                plowhdr$(1) = "  Sales Order       PO Number          " &~
                              "Order      Due       Open Amt"
                plowhdr$(1) = "  Customer  " & str(plowhdr$(1),2)
                mat incl = zer : incl$() = " "
                descr( 1) =   1.09   :   descr( 2) =  1.00
                descr( 3) =  10.16   :   descr( 4) = 12.00
                descr( 5) =  26.16   :   descr( 6) = 30.00
                descr( 7) = 806.061  :   descr( 8) = 48.00
                descr( 9) = 818.061  :   descr(10) = 58.00
                descr(11) = 867.08   :   descr(12) = 68.1042
                call "PLOWCODE" (#05, readkey$, msg$, u3%, 0.16, f1%(5), ~
                                 plowhdr$(), 0, 0, incl(), incl$(), "d", ~
                                 " ", #50, descr())
                if f1%(5) = 1% then L50290
                     errormsg$ = hex(00) : return
L50290:         cuscode$ = str(readkey$,,9)
                so$      = str(readkey$,10)
                gosub check_hist
                if errormsg$ <> " " then return
L50297:     call "BCKARISB" (so$, errormsg$) /* Check for SO in ARI SSN */
            if errormsg$ <> " " then return
            gosub load_sales_order
            if crhold$ = "C" then L50342
L50303:     if errormsg$ <> " " then return
                if soonfile% = 0% then L50318
                     call "READ100" (#01, cuscode$, f1%(1))
                     get #01 using L50311, dfltacks$
L50311:                   FMT POS(238), CH(1)
                     return clear all
                     goto summary_screen
L50318:         if cuscode$ = " " then L50160 /* Go get customer        */
L50320:              call "CUSCHECK" (cuscode$, cr_ar_only$, #01, ret%)
                     if ret% <> 0% then return
                         return clear all
                         goto inputmode

L50342:       crhold$ = " "
              if maxlines% = 0% then L50352
                for c% = 1 to maxlines%
                  openqty(c%) = order(c%)
                next c%
L50352:       goto L50303

L50355
*        Ship-to                               SHIPTO$
            if str(shipto$()) <> " " then return
                errormsg$ = "Ship-to may not be blank"
                return

L50380
*        Store Code                            STORE$
            mscdescr$ = hex(06) & "Select Store."
            f1%(12) = -9%
            call "PLOWCODE" (#12, store$, mscdescr$, 0%, .3, f1%(12))
            if f1%(12) = 1% then L50410
                errormsg$ = "Invalid Store Code."
                return
L50410:     if edit% = 2% then L50445
                oldstore$ = store$
                call "ARMGLGET" (1%, cuscode$, " ", " ", " ", store$,    ~
                                 " ", #02, #01, #04, #11, #03, dfltsales$)
                call "ARMGLGET" (2%, cuscode$, " ", " ", " ", store$,    ~
                                 " ", #02, #01, #04, #11, #03, discacct$)
                return
L50445:   /* Check if store has changed planned to/from unplanned      */
            if oldstore$ = store$ then return
            if maxlines% = 0% then L50525

            temp$ = "AA"
            if str(oldstore$,,1) >= "0" and str(oldstore$,,1) <= "9"     ~
                                                then str(temp$,1,1) = "#"
            if str(store$   ,,1) >= "0" and str(store$   ,,1) <= "9"     ~
                                                then str(temp$,2,1) = "#"
            if temp$ = "AA" or temp$ = "##" then L50525  /* No change   */
                msg$ =   "Changing from an Unplanned to a Planned Store."
                if temp$ = "#A" then msg$ =                              ~
                         "Changing from a Planned to an Unplanned Store."
                call "SHOSTAT" (msg$)
                for c% = 1% to maxlines%  :  gosub bckpipsb  :  next c%
                gosub save_header
L50525:     oldstore$ = store$
            return

L50532: REM Validate Export Order flag          EXPORT$
            if export$ = " " then export$ = "N"
            if export$ = "Y" or export$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N' for Export Order flag."
                return

L50540
*        PURCHASE ORDER NUMBER                 PO$
            if po$ <> " " then L50572
                get #01 using L50555, poreqd$
L50555:              FMT POS(1020), CH(1)
                if poreqd$ <> "Y" then L50572
                     errormsg$ = "PO is required for this Customer."
                     return
L50572
*        Now test for duplicate PO for this customer
            if po$ = " " then return
                call "BCKDUPPO" (cuscode$, so$, po$, errormsg$)
                return

*        How Ship                              HOWSHIP$
            return

*        FOB                                   FOB$
            return

L50610
*        Order Date                            ORDERDATE$
            call "DATEOK" (orderdate$, u3%, errormsg$)
            return

L50630
*        Default Due Date                      DFLTDUE$
            if dfltdue$ = " " or dfltdue$ = blankdate$ then return
                call "DATEOK" (dfltdue$, u3%, errormsg$)
                if errormsg$ <> " " then return
                     testdate$ = dfltdue$
                     gosub order_date_check
                     return

L50670
*        Default Req'd Ship Date               DFLTSHIP$
            if dfltship$ = " " or dfltship$ = blankdate$ then return
                call "SPCSMASH" (dfltship$)
                if str(dfltship$,,1) <> "-" then L50745
                     if dfltdue$ = " " or dfltdue$ = blankdate$ then L50710
                     convert dfltship$ to temp%, data goto L50705
                     goto L50725
L50705:                   errormsg$ = "Invalid days offset." : return
L50710:                   errormsg$ = "Can not use offset if Due Date" & ~
                                      " is blank."
                          return
L50725:              call "DATUNFMT" (dfltdue$)
                     call "DATE" addr("G+", str(dfltdue$,,6), temp%,     ~
                                      str(dfltship$,,6), u3%)
                     call "DATEFMT" (dfltdue$)
L50745:         call "DATEOK" (dfltship$, u3%, errormsg$)
                if errormsg$ <> " " then return
                     testdate$ = dfltship$
                     gosub order_date_check
                     if errormsg$ <> " " then return
                          call "DATUNFMT" (dfltship$)
                          call "DATE" addr("GD", str(dfltship$,,6),      ~
                                                          weekdayh$, u3%)
                          call "DATEFMT" (dfltship$)
                          return



L50850
*        Sold-to                               SOLDTO$
            if soldto$(1) = " " then str(soldto$()) = " "
            return

L50870
*        Cancellation Date                     CANCELDATE$
            if canceldate$ = " " or canceldate$ = blankdate$ then return
                call "DATEOK" (canceldate$, u3%, errormsg$)
                if errormsg$ <> " " then return
                     testdate$ = canceldate$
                     gosub order_date_check
                     return

L50910
*        Price Code Default                    PC$
            if (pc$ >= "A" and pc$ <= "Q") or                            ~
               (pc$ >= "0" and pc$ <= "8") then return
                errormsg$ = "Valid Price Codes are 'A'-'Q' and '0'-'8'"
                return

L50955
*        Order Discount Percent                ORDERDISC$
            orderdisc = 0
            if orderdisc$ = " " then orderdisc$ = "0"
            convert orderdisc$ to orderdisc, data goto L50975 : goto L50980
L50975:         errormsg$ = "Order Discount must be -25 to 100%" : return
L50980:     if orderdisc < -25 or orderdisc > 100 then L50975
            call "CONVERT" (orderdisc, 2.2, orderdisc$)
            return

L51000
*        Payment Terms (Code)                  TERMS$
            if terms$ = " " then return
                call "PLOWCODE" (#21, terms$, mscdescr$, 0%, .3, f1%(21))
                return

L51035
*        Region Code                           REGION$
            mscdescr$ = " " : if region$ = " " then return
                mscdescr$ = hex(06) & "Select Sales Region"
                readkey$ = "REGIONS  " & region$
                f1%(8) = -14%
                call "PLOWCODE" (#08, readkey$, mscdescr$, 9%,.3,f1%(8))
                if f1%(8) = 1% then L51070
                     errormsg$ = "Invalid Sales Region Code." : return
L51070:         region$ = str(readkey$,10)
                return

L51085
*        Salesman Code / Split %               SALESMAN$()
            total% = 0%
            for i% = 1% to 3%
                if salesman$(i%) <> " " then L51112
                     mscdescr$, comm$(i%) = " "
                     comm%(i%) = 0%  :  goto L51170
L51112:         f1%(7) = -14%
                call "PLOWCODE" (#07, salesman$(i%), mscdescr$,          ~
                                                       0%, 0.30, f1%(7))
                if f1%(7) = 1% then L51140
                     errormsg$ = "Salesman Code " & salesman$(i%) &      ~
                                 " not on file."  :  return
L51140:         if comm$(i%) = " " then comm$(i%) = "0"
                convert comm$(i%) to comm%(i%), data goto L51155
                goto L51160
L51155:              errormsg$ = "Commission % must be 0 - 100." : return
L51160:         total% = total% + comm%(i%)
                convert comm%(i%) to comm$(i%), pic(##0)
L51170:     next i%
            if total% <= 100% then return
                errormsg$ = "Total Commission Splits can not exceed 100%."
                return

L51195
*        Sales Account Default                 DFLTSALES$
            mscdescr$ = " " : if dfltsales$ = " " then return
                mscdescr$ = hex(06) & "Select Sales Acct Default."
                call "PLOWCODE" (#03,dfltsales$,mscdescr$,0%,.3,f1%(3))
                if f1%(3) = 1% then return
                     errormsg$ = "Invalid Sales Account Code"
                     return

L51235
*        Sales Discounts Account               DISCACCT$
            mscdescr$ = hex(06) & "Select Sales Discounts Account."
            call "PLOWCODE" (#03, discacct$, mscdescr$, 0%, 0.3, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Discounts Account"
                return

L51270
*        Shipping Instructions                 SHIPINSTR$()
            return

*        See if order is in history files
        check_hist
            readkey$  = str(so$) & hex(00)
            call "PLOWNEXT" (#37, readkey$, 16%, f1%(37))
              if f1%(37) = 0% then L51340
                errormsg$ = "Sales Order " & so$ & " is in the History "&~
                            "Files."
L51340:         return

L51360
*        Test Data for Precious Metal Surcharge at SO Entry
            if pm_on$ <> "Y" then return
            if pos("YN" = pm_so$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N' for PM SO Surcharge"
                return

L51410
*        Test Data for Precious Metal Surcharge at Invoice Update
            if pm_on$ <> "Y" then return
            if pos("YN" = pm_inv$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N' for PM INV Surcharge"
                return

L52000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the Line Item Elements.                     *~
            *************************************************************

            errormsg$ = " "
            if edit% = 2% then modfld% = 1% /* Fld enabled in EDITMODE */
            on fieldnr%      gosub  L52170,         /* Part Number      */~
                                    L52305,         /* Part Description */~
                                    L52310,         /* Part Category    */~
                                    L52345,         /* Due Date         */~
                                    L52385,         /* Ship Date        */~
                                    L52535,         /* Stocking UOM     */~
                                    L52600,         /* Pricing UOM      */~
                                    L52665,         /* Conversion Factor*/~
                                    L52725,         /* Order Qty        */~
                                    L52787,         /* Alloc Qty        */~
                                    L52910,         /* Open Qty         */~
                                    L53005,         /* Lot Number       */~
                                    L53020,         /* Shipped Qty      */~
                                    L53047,         /* Currency code    */~
                                    L53121,         /* Unit Price       */~
                                    L53125,         /* Line Disc Percent*/~
                                    L53180,         /* Taxable?         */~
                                    L53200,         /* PO Item #        */~
                                    L53215,         /* Project #        */~
                                    L53280,         /* Sales Account    */~
                                    L53320,         /* Sales Disc Acct  */~
                                    L53542,         /* Shipping Priority*/~
                                    L53360,         /* Priority         */~
                                    L53385,         /* Demand Type      */~
                                    L53430,         /* BOM ID           */~
                                    L53530          /* Horizon Date     */
            if edit% = 2% then gosub describe_line
            return

L52170
*        Part Code                             PART$()
            if keyhit% = 22% or lastkey% = 23% then L52249
            if part$(c%) = " " then L52188
                call "READ100" (#04, part$(c%), f1%(4%))
                if f1%(4%) = 1% then L52202
                     call "HNYGREF" (part$(c%), #15, #04, f1%(4%))
                     if f1%(4%) = 0% then L52188
                          call "READ100" (#04, part$(c%), f1%(4%))
                          if f1%(4%) = 1% then L52202
L52188:     f1%(4%) = -14%
            call "PLOWCODE" (#04, part$(c%), " ", 0%, 0.32, f1%(4%))
            if f1%(4%) = 1% then L52202
                if part$(c%) <> " " then L52200
                     errormsg$ = "Part Code can't be blank"
                     return
L52200:         refpart$(c%) = "** No Cross Reference **"
                nonstockmsg$(c%) = "NonStk"  :  return
L52202:     get #04 using L52204, cat$(c%), partflag$ , ptype$(c%)
L52204:         FMT POS(90), CH(4), POS(166), CH(4), POS(180), CH(3)

        REM Get Corresponding Ref Part For This CMS Part...
            if refpart$(c%) = " " then                                   ~
                refpart$(c%) = "** No Cross Reference **"
*          CALL "PTXREFSB" (2%, "C", CUSCODE$, REFPART$(C%),            ~
*               REFDESC$(C%), PART$(C%), DESCR$(C%), #4, RET%)
*          IF RET% = 1% THEN REFTYPE$(C%) = "C"
*          IF RET% = 1% THEN REFFLAG% = 1% ELSE                         ~
*              REFPART$(C%) = "** No Cross Reference **"
L52218:     if partflag$ = " " then L52233
                u3% = 2%
                call "ASKUSER" (u3%, "PART FLAGGED",                     ~
                          "This Part is flagged as '" & partflag$ & "'.",~
                          "Press PF-16 to continue and use Part,",       ~
                          "Or press (RETURN) to re-enter Part Number.")
                if u3% = 16% then L52233
L52230:            errormsg$ = "Re-enter Part Number"
                   return

L52233:     call "ARIEXTRA" (cuscode$, part$(c%), " ", temp%, #2)
            if temp% <= 0% then return
            if c% + total_e_lines% + temp% <= 100% then L52238
               gosub extra_append_conflict
               if u3% = 0% then L52230
L52238:           e_lines%(c%) = temp%
                  total_e_lines% = total_e_lines% + temp%
                  return

L52249: REM Get CMS Part For This Customer Part...
            refpart$(c%) = part$(c%)
            if lastkey% = 23% then L52261
        REM Customer Cross Reference...
            call "PTXREFSB" (1%, "C", cuscode$, refpart$(c%),            ~
                refdesc$(c%), part$(c%), descr$(c%), #4, ret%)
            if ret% = 1% then reftype$(c%) = "C"
            if ret% = 1% then L52286
                if part$(c%) = " " then                                  ~
                   errormsg$ = "Part Number CANNOT Be Blank"  else       ~
                   errormsg$ = "No Cross Ref Part For This Customer Part"
                return
L52261: REM Manufacturer's Cross Reference...
            if mfgcode$ = "?" then mfgcode$ = " "
            mfgdescr$ = hex(06) & "Select Manufacturer's Code"
            readkey$ = "MFG CODES" & str(mfgcode$,1%,9%)
            call "PLOWCODE" (#8, readkey$, mfgdescr$, 9%, .3, f1%(8%))
               if f1%(8%) = 1% then L52269
                   errormsg$ = "Invalid Manufacturer's Code"
                   return
L52269:     mfgcode$ = str(readkey$,10%)
            call "PTXREFSB" (1%, "M", str(mfgcode$,1,9),  refpart$(c%),  ~
                 refdesc$(c%), part$(c%), descr$(c%), #4, ret%)
            if ret% = 1% then reftype$(c%) = "M"
            if ret% = 1% then L52280
                if part$(c%) = " " then                                  ~
                   errormsg$ = "Part Number CANNOT Be Blank"  else       ~
                   errormsg$ = "No Cross Ref Part For This Mfg Part"
                   return
L52280:     call "GETCODE" (#4, part$(c%), " ", 0%, 0, f1%(4%))
            if f1%(4%) = 1% then L52286
                if part$(c%) = " " then                                  ~
                    errormsg$ = "Part Number CANNOT Be Blank"  else      ~
                    errormsg$ = "Part No Longer Exists In HNYMASTR File"
                    return
L52286:     get #04 using L52287, cat$(c%), partflag$ , ptype$(c%)
L52287:         FMT POS(90), CH(4), POS(166), CH(4), POS(180), CH(3)
            refflag% = 1%
            goto L52218

L52305
*        Part Description                      DESCR$()
            return

L52310
*        Part Category                         CAT$()
            mscdescr$ = hex(06) & "Select Part Category"
            f1%(11) = -14%
            call "PLOWCODE" (#11, cat$(c%), mscdescr$, 0%, 0.3, f1%(11))
            if f1%(11) = 1% or cat$(c%) = " " then return
                errormsg$ = "Category Code not on file"  :  return

L52345
*        Due Date                              DUEDATE$()
            call "DATEOK" (duedate$(c%), u3%, errormsg$)
            if errormsg$ <> " " then return
                testdate$ = duedate$(c%)
                gosub order_date_check : if errormsg$ <> " " then return
                   if origdue$(c%) = " " or origdue$(c%) = blankdate$ then ~
                      origdue$(c%) = duedate$(c%)
                   return

L52385
*        Required Ship Date                    SHIPDATE$()
            call "SPCSMASH" (shipdate$(c%))
            if str(shipdate$(c%),,1) <> "-" then L52435
                convert shipdate$(c%) to temp%, data goto L52410
                goto L52415
L52410:              errormsg$ = "Invalid days offset." : return
L52415:         call "DATUNFMT" (duedate$(c%))
                call "DATE" addr("G+", str(duedate$ (c%),,6), temp%,     ~
                                       str(shipdate$(c%),,6), u3%)
                call "DATEFMT" (duedate$(c%))
L52435:     call "DATEOK"  (shipdate$(c%), u3%, errormsg$)
            if errormsg$ <> " " then return
                testdate$ = shipdate$(c%) : gosub order_date_check
                if errormsg$ <> " " then return
                     call "DATUNFMT" (shipdate$(c%))
                     call "DATE" addr("GD", str(shipdate$(c%),,6),       ~
                                                          weekday$, u3%)
                     call "DATEFMT"  (shipdate$(c%))
*        Get Options for this part (Input Mode Only)
            if edit% = 1% then gosub options
*        Check for Change
            call "DATUNFMT" (shipdate$(c%))
            if edit% = 1% or oldshipdate$ = shipdate$(c%) then L52515
                call "DATEFMT" (shipdate$(c%))
                gosub save_line
                call "DATUNFMT" (shipdate$(c%))
L52515:     oldshipdate$ = shipdate$(c%)
            call "DATEFMT" (shipdate$(c%))
            return

L52535
*        Stocking Unit Of Measure              STKUOM$()
            mscdescr$ = " "  :  if uom% = 1% then L52560
                if stkuom$(c%) <> " " then return
                     errormsg$ = "Stocking UOM can't be blank" : return
L52560:     mscdescr$ = hex(06) & "Select Stocking UOM"
            readkey$ = "UOM      " & stkuom$(c%)
            f1%(8) = -14%
            call "PLOWCODE" (#08, readkey$, mscdescr$, 9%, 0.30, f1%(8))
            if f1%(8) = 1% then stkuom$(c%) = str(readkey$,10)           ~
                           else errormsg$   = "Stocking UOM not on file"
            return

L52600
*        Pricing Unit of Measure               PRICEUOM$()
            mscdescr$ = " "  :  if uom% = 1% then L52625
                if priceuom$(c%) <> " " then return
                     errormsg$ = "Pricing UOM can't be blank" : return
L52625:     mscdescr$ = hex(06) & "Select Pricing UOM"
            readkey$ = "UOM      " & priceuom$(c%)
            f1%(8) = -14%
            call "PLOWCODE" (#08, readkey$, mscdescr$, 9%, 0.30, f1%(8))
            if f1%(8) = 1% then priceuom$(c%) = str(readkey$,10)         ~
                           else errormsg$    = "Pricing UOM not on file"
            return

L52665
*        Conversion Pricing to Stocking        CONV$
            if priceuom$(c%)= stkuom$(c%) or conv$= " " then conv$ = "1"
            call "NUMTEST" (conv$, 0, 9e7, errormsg$, 7.7, conv(c%))
                if errormsg$ <> " " then return
            if edit% = 2% then gosub pricing_stuff
            return

L52725
*        Original Order Quantity               ORDER$
            if order$ = " " then order$ = "0"
            q$ = order$  :  gosub uom_trans  :  order$ = q$
            savorder = order(c%)
            call "NUMTEST" (order$, 0, 9e7, errormsg$, 2.2, order(c%))
                if errormsg$ <> " " then return
*        Apply Part Code Min Quantity and Min Increment to Order Quantity
            minsoqty, minsoinc = 0
            if nonstockmsg$(c%) <> " " then L52760
            call "READ100" (#04, part$(c%), f1%(4))
                if f1%(4) <> 1% then L52752
            get #04 using L52750, minsoqty, minsoinc
L52750:         FMT POS(706), 2*PD(14,4)
L52752:     call "BCKMINSB" (order(c%), minsoqty, minsoinc, u3%)
            if u3%  = 16% then L52760
            if u3% <>  1% then goto L52752
                order(c%) = savorder
                call "CONVERT" (order(c%), 2.2, order$)
                errormsg$ = hex(00)
                return
L52760:     if edit% = 2% then gosub test_ext  /* Is Ext too big? */
                if errormsg$ <> " " then return
            if edit% = 2% and oldorder <> order(c%) then gosub save_line
            oldorder = order(c%)
            if edit% = 2% then goto L52775
                tempallc = order(c%) : gosub get_req_date
                avail = 0            : gosub planallc
L52775:     return

L52787
*        Allocation Qty, Instruction           ALLOCQTY$
*        Note: Only get here if line is unplanned.
            if pos("NACZP" = str(allocqty$,,1)) <> 0% then L52825

            convert allocqty$ to allocqty, data goto L52890
            if allocqty < 0 or allocqty > avail + oldalloc then L52895
                alloc$(c%) = "N"
                goto L52865
L52825:     if allocqty$ = "C" then oldalloc = 0
            if oldalloc$ <> "C" then L52830
                avail = oldalloc : gosub planallc
                oldalloc = 0
L52830:     on pos("NACZP" = str(allocqty$,,1)) goto L52840, L52845,       ~
                                                     L52850, L52855, L52857
L52840:     allocqty = 0                :  alloc$(c%) = "N" : goto L52865
L52845:     allocqty = avail + oldalloc :  alloc$(c%) = "P" : goto L52865
L52850:     allocqty = order(c%)        :  alloc$(c%) = "C" : goto L52865
L52855:     allocqty = 0                :  alloc$(c%) = "N" : goto L52865
L52857:     allocqty = oldalloc         :  alloc$(c%) = "P" : goto L52865

L52865:     alloc(c%) = min(allocqty, order(c%))
            call "CONVERT" (alloc(c%), 2.2, allocqty$)
            if edit% = 2% and alloc(c%) <> oldalloc then gosub save_line
            oldalloc = alloc(c%)
            return
L52890:      errormsg$ = "Enter 'N', 'A', 'P', 'C', 'Z' or Quantity."
             return
L52895:      errormsg$ = "Must be 0 to Quantity Available."      : return


L52910
*        Open Order Quantity                   OPENQTY$
            if openqty$ = " " then openqty$ = "0"
            q$ = openqty$  :  gosub uom_trans  :  openqty$ = q$
*          CONVERT OPENQTY$ TO OPENQTY(C%), DATA GOTO 52930 : GOTO 52935
            call "NUMTEST" (openqty$, 0,9e7, errormsg$, 2.2, openqty(c%))
                if errormsg$ <> " " then return
            if openqty(c%) >= qtyschld(c%) then L52975
                errormsg$ = "Cannot reduce Open Quantity less than" &    ~
                            " Quantity Scheduled."
                return
L52975
*          IF EDIT% = 2% THEN GOSUB PRICING_STUFF
            if edit% = 2% then gosub test_ext  /* Is Ext too big? */
                if errormsg$ <> " " then return
            if edit% = 2% and oldopenqty <> openqty(c%) then             ~
                                                        gosub save_line
            oldopenqty = openqty(c%)
            return

L53005
*        Lot Number                            LOT$()
            if lot$(c%) <> " " then                                      ~
                call "LOTVALID" (part$(c%), store$, lot$(c%),            ~
                    #02, #04, #13, errormsg$)
            if errormsg$ <> " " then return
            call "LOTUNQUE" (part$(), lot$(), c%, #02, errormsg$)
            return

L53020
*        Total Quantity Shipped                SHIP$
            if ship$ = " " then ship$ = "0"
            q$ = ship$  :   gosub uom_trans  :  ship$ = q$
            call "NUMTEST" (ship$, 0,9e7, errormsg$, 2.2, ship(c%))
            return

L53047
*        Currency code                       CURRENCY$
            if enabled% = 0% then return
            if currency$ = " " then currency$ = statutory$
            call "GETCODE" (#40, currency$, " ", 0%, 0, f1%(40))
            if f1%(40) <> 0% then goto L53067
                errormsg$ = "Invalid Currency code.  Try again." : return
L53067:     convdate$ = " " : conveqv, convunt = 1
            if currency$ = statutory$ then goto L53103
            call "DATREVRS" ( orderdate$, rev_date$, errormsg$ )
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#42, currkey$, 5%, f1%(42))
            if f1%(42) <> 0% then goto L53097
                errormsg$ = "No Exchange Rates for this Currency."
                return
L53097:     get #42 using L53102, convdate$, conveqv, convunt
L53102:         FMT POS(12), CH(6), 2*PD(14,7)
L53103:     if edit% <> 2% then return
                gosub L23010      /* CPRASSGN gets price */
                errormsg$ = " "
                return

L53121
*        Unit Price                            PRICE$
            call "NUMTEST" (price$, 0,9e7, errormsg$, 4.4, price(c%))
            return

L53125
*        Line Item Discount %                  LINEDISC$
            call "NUMTEST" (linedisc$,-50,100,errormsg$,2.2,linedisc(c%))
                if errormsg$ <> " " then return
                gosub test_ext  /* Test to see if extension is too big */
            /* GOSUB PRICING_STUFF */  :  return

L53180
*        Line Taxable? (Y/N)                   TAXABLE$()
            if pos("YN" = taxable$(c%)) <> 0 then return
                errormsg$ = "Enter 'Y' or 'N'" : return

L53200
*        P.O. Item                             ITEM$()
            return

L53215
*        Project Number                        PROJECT$()
            if project$(c%) = " " then return
            f1%(14) = -14%
            call "PLOWCODE"(#14, project$(c%), mscdescr$, 0%, .3, f1%(14))
            if f1%(14) = 1% then L53245
                errormsg$ = "Project not on file." : return
L53245:     get #14, using L53250, testdate$
L53250:         FMT XX(44), CH(06)
            if testdate$ = " " or testdate$ = blankdate$ ~
                               or testdate$ > date$ then return
                call "DATEFMT" (testdate$)
                errormsg$ = "Project was closed on " & testdate$
                return

L53280
*        Sales Distr. Account                  SALESACCT$()
            mscdescr$ = hex(06) & "Select Sales Account."
            f1%(3) = -14%
            call "PLOWCODE" (#03,salesacct$(c%),mscdescr$,0%,.3,f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Account Code"
                return

L53320
*        Sales Discounts Account               DISCACCTL$()
            mscdescr$ = hex(06) & "Select Sales Discounts Account."
            f1%(3) = -14%
            call "PLOWCODE" (#03,discacctl$(c%),mscdescr$,0%,.3,f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Discounts Account"
                return

L53360
*        Planning Priority Code                PRIORITY$()
            if priority$(c%) >= "A" and priority$(c%) <= "Z" then return
                errormsg$ = "Enter 'A' - 'Z'."
                return

L53385
*        Planning Demand Type                  DEMTYPE$()
            if demtype$(c%) = "1" or demtype$(c%) = "2" then L53405
                errormsg$ = "Planning Demand type must be '1' or '2'"
                return
L53405:     if edit% = 1% then L53415
                if olddemtype$ <> demtype$(c%) then gosub save_line
L53415:     olddemtype$ = demtype$(c%)
            return

L53430
*        BOM ID                                BOMID$
            if bomid$ = oldbomid$ then return
            if bomid$ = " " then L53470
            plowkey$ = str(part$(c%),,25) & str(bomid$,,3)
            plowhdr$()="  Listed Below Are The Existing BOMs For Part: "&~
                                                                part$(c%)
            msg$ = hex(06) & "Select Bill Of Materials"
            call "PLOWCODE" (#16, plowkey$, msg$, 2025%, .30, f1%(16),   ~
                                                           plowhdr$(), 3)
                if f1%(16) <> 0% then L53453
                errormsg$ = "BOM not on file" : return
L53453:     bomid$ = str(plowkey$,26%,3%)
            if bomid$ = oldbomid$ then return

L53470:   /* Update DEMMASTR with New BOM ID         */
            str(filler$(c%),17,3), oldbomid$ = bomid$
            if edit% = 1% then return
                readkey$ = str(so$) & str(seq$(c%))
                call "REDALT1" (#23, readkey$, 1%, f1%(23))
                if f1%(23) = 0% then return
                     put #23 using L53505, bomid$
L53505:                   FMT POS(68), CH(3)
                     rewrite #23
                     return

L53530
*        Horizon Date                          HORIZON$
*          Field Disabled
            return

L53542
*        Shipping Prioity Code                 SHIPCODE$
            if shipcode$(c%) = " " then shipcode$(c%) = shipcode$
                if shipcode$(c%) >= "0" and                              ~
                   shipcode$(c%) <= "5" then return
                errormsg$ = "Enter 0 - 5"
                return

        test_ext
            ext = openqty(c%) * price(c%) * (1 - linedisc(c%)/100)
                if ext < 1e9 then return
                errormsg$ = "Extention is TOO large, please "         &  ~
                            "reduce Order Quantity."
                if edit% = 1% then fieldnr% = 9%
                return

L55000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Data Save Screen               *~
            *************************************************************


            errormsg$ = " "
            on fieldnr%        gosub     L55130,    /* Adj Reason       */~
                                         L55200     /* Print Pick List  */
            return

L55130
*        Adj Reason Code
            if crhold$ = "C" then L55184
            if adjrsn$ = " " and manadjre$ <> "Y" then return
            adjrsndescr$ = hex(06) & "Select Adjust Reason Code"
            readkey$ = "SO REASON" & adjrsn$
            call "PLOWCODE" (#08, readkey$, adjrsndescr$, 9%, 0.3, f1%(8))
            if f1%(8) = 1% then adjrsn$ = str(readkey$,10) else          ~
                            errormsg$ = "Invalid Adjustment Reason Code."
            if errormsg$ <> " " then return
            if modfld% <> 1% or manadjre$ <> "Y" then return
                if adjrsn$ <> " " then return
                    errormsg$ = "Adjustment Reason Code may not be blank."
                    return

L55184:     if can$ <> "Y" then return
            adjrsndescr$ = hex(06) & "Select Cancel Reason Code"
            readkey$ = "CANREASON" & adjrsn$
            call "PLOWCODE" (#08, readkey$, adjrsndescr$, 9%, 0.3, f1%(8))
            if f1%(8) = 1% then adjrsn$ = str(readkey$,10) else          ~
                          errormsg$ = "Invalid Cancellation Reason Code."
            return

L55200
*        Pick List, BOL, and Acknowledgement Printing
*          No real validation needed, so just encode PICKPRINT$
        encode_pickprint
            pickprint% = 0%
            for i% = 1% to 3%
                if pickprint$(i%) = " " then L55260
                     pickprint$(i%) = "X"
                     pickprint% = pickprint% + (1% + ((i% - 1%) * 2%))
L55260:         next i%
            convert pickprint% to pickprint$, pic(#)
            return

        REM *************************************************************~
            *         M I S C .   T E S T   S U B R O U T I N E S       *~
            *-----------------------------------------------------------*~
            * Some subroutines to aid the above test sections.          *~
            *************************************************************

        order_date_check
*         Checks that TESTDATE$ (valid and formatted) is on or after
*         the order date
            call "DATUNFMT" (testdate$)
            call "DATUNFMT" (orderdate$)
            if testdate$ < orderdate$ then                               ~
                            errormsg$ = "Must be on or after Order Date"
            call "DATEFMT" (orderdate$)
            if errormsg$ = " " then return
               errormsg$ = errormsg$ & " (" & orderdate$ & ")"
               return

        uom_trans  /* Translate entry from UOM entered to Stocking UOM */
            p% = pos(q$ = "/") : if p% = 0% then return
            if p% < 10% then L56670
                errormsg$ = "Invalid specification for UOM" : return
L56670:     u$ = str(q$,p%+1%) : q$ = str(q$,,p%-1%)
            convert q$ to q, data goto L56690  :  goto L56700
L56690:         errormsg$ = "Invalid entry for Quantity" : return
L56700:     readkey$ = "UOMCONV  " & str(u$,,4) & "-" & stkuom$(c%)
            call "READ100" (#08, readkey$, f1%(8))
            if f1%(8) = 1% then L56750
L56730:         errormsg$ = "Conversion factor invalid for UOM " & u$
                return
L56750:     get #08 using L56760, c$
L56760:         FMT XX(24), CH(10)
            convert c$ to c, data goto L56730
            q = round(q*c, 4)  :  call "CONVERT" (q, 4.4, str(q$,,10))
            return

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
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

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
