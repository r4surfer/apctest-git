        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  B   B  C   C  K  K     I    NN  N  P   P  U   U    T     *~
            *  BBBB   C      KKK      I    N N N  PPPP   U   U    T     *~
            *  B   B  C   C  K  K     I    N  NN  P      U   U    T     *~
            *  BBBB    CCC   K   K  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKINPUT - Manages Sales Orders.                          *~
            *                                                           *~
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
            * 06/23/86 ! Original                                 ! ERN *~
            * 02/11/87 ! Lot Tracking Enhancements                ! ERN *~
            * 04/13/87 ! Corrected Array index at ln 14110        ! MJB *~
            * 04/27/87 ! Added call to PIPATCSB to Check A.T.C.   ! MJB *~
            * 05/15/87 ! HNYMASTR record length mod.              ! JIM *~
            *          !   HNYGLGET gets acct #5 (sales). Was #4. !     *~
            *          !   Added SYSFILE2 to CPRASSGN.            !     *~
            * 11/03/87 ! Added Export Flag Maintenance            ! MJB *~
            * 11/10/87 ! Added Multi-currency, CURMASTR, BCKLNCUR,! JIM *~
            *          !   CURCONVR. Also, see the cautions at    !     *~
            *          !   the top of the source file.            !     *~
            * 01/14/88 ! Added SO History                         ! JDH *~
            * 03/18/88 ! Added Cancelled sales order logic.       ! JDH *~
            * 04/04/88 ! Added Last SO msg to first screen        ! MJB *~
            * 04/26/88 ! Take out ability to delete & have only   ! JDH *~
            *          !   called up orders able to be cancelled  !     *~
            * 08/04/88 ! HNYMASTR conversion factor to 7 decimals.! JIM *~
            * 08/04/88 ! Can't delete last line of a Sales Order. ! JIM *~
            * 08/24/88 ! Added COPY% to initialization            ! JDH *~
            * 10/05/88 ! Added DEMMASTR to call BCKNEXT & BCKINPTS! JDH *~
            * 10/13/88 ! Added check for Allow SO Deletion Flag,  ! JDH *~
            *          ! Added check to DEMMASTR for SO#,         !     *~
            *          ! Fixed precision of calc for UOM conv fctr!     *~
            *          ! Fixed input message for default ship date!     *~
            * 11/09/88 ! Enhanced for faster response time.       ! JDH *~
            * 02/23/89 ! Proj 7890206 Four SO Entry Modifications.! JIM *~
            * 04/28/89 ! Added GETSCRN; mod to reset NONSTOCKMSG, ! JDH *~
            *          !  DESCR, CAT, & CATDESCR when PF4 back to !     *~
            *          !  Part #; Changed screen literal for      !     *~
            *          !  Shipped & Pre-invoiced qty; Added see   !     *~
            *          !  Prices.                                 !     *~
            * 09/25/89 ! Corrected convert error for extension    ! JDH *~
            * 10/04/89 ! Disabled Currency in edit mode.          ! JDH *~
            * 12/07/89 ! Moved GET of MINSOQTY/MINSOINC to 50000s.! JDH *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 05/25/90 ! Init shipping qtys when copying order,   ! JDH *~
            *          !  Allow copy from history files, Allow    !     *~
            *          !  deletes if order copied, Added HNYQDISP,!     *~
            *          !  Added dates & amt to SO PLOWscreen.     !     *~
            * 04/23/91 ! PRR 11722 Added codes to avoid users from! SID *~
            *          !  deleting of the Shipped, Invoiced, and  !     *~
            *          !  Scheduled of Line Items.                !     *~
            *          ! PRR 11785 Fixed Line Items Status Marker.!     *~
            * 07/01/91 ! Expanded Status Indicator or LST$(100)3  ! SID *~
            *          !    so that 'S'cheduled Indicator won't   !     *~
            *          !    get written over by 'P're-Invoiced.   !     *~
            * 11/20/91 ! Added 'drop ship' PF key which copies    ! WPH *~
            *          ! ship-to address to sold-to and clears    !     *~
            *          ! ship-to address.                         !     *~
            * 01/21/92 ! Integrated BCKINPTS back into BCKINPUT.  ! JDH *~
            * 01/22/92 ! PRR 11909 Check status of BillTo, Parent.! JDH *~
            *          ! PRR 10609 Honor Default Demand Type swtch!     *~
            *          ! PRR 11967 Allow Cancel of LIs with BOLs. !     *~
            *          ! PRR 11905, 11249, 11623  Honor new Auto- !     *~
            *          !   Hold option (Override manual or not).  !     *~
            *          ! PRR 10611, 10612  LI Text in Inputmode.  !     *~
            * 02/04/92 ! PRR 11432,10936,10664  Copy Line Logic.  ! JDH *~
            *          ! PRR 11823  Modify Dates from LI Summary. !     *~
            * 04/20/92 ! Orders w/o open qty not on CR Hold.      ! JDH *~
            * 05/29/92 ! Added demand status msg and ATC for      ! JDH *~
            *          !   required ship date to LI Screen 1.     !     *~
            * 06/16/92 ! Orig Store default comes from USERINFO.  ! JDH *~
            *          ! Uses Offset Days for calc of Due & Ship. !     *~
            *          ! PF32 from LI Sum Scrn to Short Cut Save. !     *~
            *          ! Added Net Open Total Amt to LI Sumry Scr.!     *~
            *          ! Override of Credit Hold if allowed.      !     *~
            *          ! Customer Status checking now by CUSCHECK.!     *~
            * 06/19/92 ! Added call to SZRINSUB for entry of size ! WPH *~
            *          ! runs and logic to auto-create line items.!     *~
            * 07/24/92 ! PRR 12531 Added Allocation type 'P' for  ! JDH *~
            *          !   Previously Allocated.                  !     *~
            *          ! Added Reset Alloc Type on all line items.!     *~
            * 10/22/92 ! PRR 12637 Captures exchange rate even    ! JDH *~
            *          !   currency field disabled by soft enables!     *~
            * 11/10/92 ! Brought Core Deposit Module to R6.02.03. ! JIM *~
            * 11/10/92 ! Call to MANUAL for BCKINPTS now BCKINPUT.! JIM *~
            * 11/20/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 11/20/92 ! Cust Credit- Added calls to ARQCUSCR.    ! JIM *~
            * 12/02/92 ! PRR 12488 - If set, use default value for! JDH *~
            *          !   Allocation Method & Demand Type from   !     *~
            *          !   the Part Master file (HNYMASTR).       !     *~
            *          ! Clean up some implied integers in arrays.!     *~
            * 02/04/93 ! PRR 12614 Export Flag defaults from Cust.! JDH *~
            *          ! PRR 11200 Warning if PO already used.    !     *~
            *          ! PRR 12521 Fixed LI Sum Scrn on SO delete.!     *~
            * 06/22/93 ! (1) Added call to PTXREFSB sub for cust  ! MLJ *~
            *          !   part number cross referencing on input !     *~
            *          !   (enter REF part, get CMS part/desc).   !     *~
            *          ! (2) Added PF22 on line summary to toggle !     *~
            *          !   between CMS part/desc & REF part/desc  !     *~
            * 07/30/93 ! Added mfg part cross reference capability! MLJ *~
            * 08/17/93 !Support for Extra Line Generation (core,?)! KAB *~
            * 09/27/93 ! Added Multi-Line Quote Cutover logic.    ! JIM *~
            * 09/27/93 ! PRR 13020 Access to Estimate re-use error! JIM *~
            * 09/27/93 ! Got rid of f/c #50 DUMMY for PLOWCODE.   ! JIM *~
            * 09/28/93 ! Selected implied integer convs fixed.    ! JIM *~
            * 11/01/93 ! PRR 13025- Added calls to HNYMSTSB.      ! JIM *~
            * 11/08/93 ! Force U3% = 0% at CALL to TASKUP.        ! JIM *~
            * 11/18/93 ! Added support for Shipping Priority Code.! MLJ *~
            * 12/23/93 ! Added Ship prty Code to Size Run Lines.  ! MLJ *~
            * 03/07/94 !Added support for BOMSPHDR file et al     ! WPH *~
            * 03/24/94 ! PRR 12915.  Change all ship dates.       ! JDH *~
            *          ! Honor Use ONLY A/R for credit limit check!     *~
            * 04/05/94 ! PRR 13133.  Added ATC honoring Horizon.  ! JDH *~
            *          ! Also, ATC values blink if negative.      !     *~
            * 06/06/94 ! Fixed line seq. on cutover quotes.       ! JDH *~
            * 06/23/94 ! Added PF(11)Date Run on Line Item Screen.! MLJ *~
            *          !   Calls new DATRUNSB for creation of     !     *~
            *          !   multiple SO lines.                     !     *~
            * 07/25/94 ! Corrected problems with MLQ cutover and  ! WPH *~
            *          !   purging of options, added logic to     !     *~
            *          !   honor BOMOPSUB as the only means to set!     *~
            *          !   BOM version and the primary means to   !     *~
            *          !   set the price for type 000 parts.      !     *~
            * 11/29/94 ! Added PARTTYPE$() and tests for generic. ! WPH *~
            * 12/29/94 ! Added Support for Creating and Cutting   ! JBK *~
            *          !   over an Estimate for a part.           !     *~
            * 12/29/94 ! Added Precious Metal Surcharge Additions ! RJH *~
            *          !   to SO Price and Extention, governed by !     *~
            *          !   three PM Flags (Sys, SO, INV).         !     *~
            * 03/07/95 ! PRR - 13187 Track Cross-Reference Parts  ! RJH *~
            *          !   for future reference and printing on   !     *~
            *          !   acknowledgements, and invoices, etc.   !     *~
            * 03/21/95 ! PRR 13143.  Added Print Acknowledgements?! JDH *~
            * 04/05/95 ! PRR 13358 - BOMSPEC now removed when PF1 ! RJH *~
            *          !   Startover from input(append) mode.     !     *~
            * 04/26/95 ! PRR 13283 - Test for BCK-ARI conflict.   ! RJH *~
            * 04/26/95 ! PRR 13427 - PF6/7 to move to prev/next   ! RJH *~
            *          !   line item edit from detail edit mode.  !     *~
            * 06/29/95 ! Added Header and Line Date Tests for     ! JBK *~
            *          !   MLQ Quote Cutover.  Make sure that     !     *~
            *          !   'Due Dates' and 'Req. Ship Dates' for  !     *~
            *          !   Both Header and Lines are Valid.       !     *~
            *          ! Modified Quotation Selection so that only!     *~
            *          !   Quotions belonging to the Customer     !     *~
            *          !   can be selected for Cutover.  (This is !     *~
            *          !   partially due to need to protect       !     *~
            *          !   possible cross reference data create   !     *~
            *          !   as part the quote process.             !     *~
            *          ! Modified and Eased Estimate Selection.   !     *~
            *          !   Modified default pricing for estimated !     *~
            *          !   parts, now based on quantity ordered   !     *~
            *          !   and estimate price array.              !     *~
            * 07/11/95 ! Corrected Calls to CMSLINK.              ! JBK *~
            * 08/15/95 ! PRR 13491 -  On copied line item,        ! JBK *~
            *          !   allocation method comes from HNYMASTR  !     *~
            *          !   or the system default.                 !     *~
            * 02/27/96 ! Acks print unless customer set to 'N'.   ! JDH *~
            * 09/06/96 ! Changes for the year 2000.               ! DXL *~
            * 07/07/97 ! Pass unformated date to PIPATCDZ         ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim /*                                                         */~
            acctxref$9,                  /* Account Xref               */~
            adjmsg$24,                   /* Adj/Cancel reason msg      */~
            adjrsn$9, adjrsndescr$30,    /* Adjustment Reason          */~
            afac$1,                      /* Allocation Reset FAC       */~
            aset$1,                      /* Allocation Reset Variable  */~
            askmsg$(3)79,                /* Ask User Messages          */~
            alloc(100), allocqty$10,     /* Allocation Quantity        */~
            alloc$(100)1, allocdescr$32, /* Allocation Flag            */~
            allocdflt$1,                 /* Allocation Flag Default    */~
            allow_delete$1,              /* Allow Deletion of SOs Flag */~
            allow_delete_save$1,         /* Save Allow Delete Flag     */~
            ar(2), oo(2),                /* A/R, On Order amounts      */~
            atc$(100,2)41,               /* ATC msg for part/ship date */~
            atc1$8, atch1$8,             /* ATC 1 for part/ship date   */~
            atc2$8, atch2$8, atchz$3,    /* ATC 2 for part/ship date   */~
            billto$9, billtoname$30,     /* Bill-to Customer ID        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$(100)3,                  /* Specific BOM Id For Line   */~
            bomdescr$32,                 /*                            */~
            can$1,                       /* Cancel Reason Code Here?   */~
            canceldate$8,                /* Cancellation Date          */~
            cat$(100)4,                  /* Part Category              */~
            catdescr$32,                 /*                            */~
            comm%(3), comm$(3),          /* Commission Split %s        */~
            conv(100), conv$10,          /* Conversion Pricing->Stockng*/~
            convdate$6,                  /* Currency conversion fields */~
            cr_ar_only$1,                /* Use Only A/R for Cr Limit  */~
            crflag$1,                    /* Put Orders on Hold?        */~
            crhold$1,                    /* Order Credit Status        */~
            crmsg$68,                    /* On Credit Hold Message     */~
            currkey$50,                  /* Currency lines read key    */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            curr2$4,                     /* Currency Code testing      */~
            currency$4, currdesc$32,     /* Currency code& description */~
            currdesc_2$32,               /* Computed currency descript */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Code              */~
            cusdescr$30,                 /* Customer Descr (Sort Name) */~
            custaxable$1,                /* Customer Taxable? (Y/N)    */~
            custype$2,                   /* Customer Type Code         */~
            cutover_bom$3,               /* Estimate Cutover BOM ID    */~
            date$8,                      /* Date for screen display    */~
            datetime$7,                  /* Buffer Date/Time Stamp     */~
            default$30,                  /* Def. overrides from SZRINSB*/~
            demstatus$1,                 /* Line Item Demand Status    */~
            demstatusmsg$16,             /* Line Item Demand Status Msg*/~
            demtype$(100)1,              /* Planning Demand Type       */~
            descr(12),                   /* Plowcode Argument          */~
            descr$(100)32,               /* Part Description           */~
            dflt_dem_type$1,             /* Default Demand Type for SOs*/~
            dfltdue$8,                   /* Default Due Date           */~
            dfltbol$1, dfltpick$1,       /* Default Print Settings Y/N */~
            dfltacks$1,                  /* Default Print Settings Y/N */~
            dfltsales$12,                /* Default Sales Account      */~
            dfltsalesdescr$32,           /*                            */~
            dfltship$8,                  /* Default Req'd Ship Date    */~
            dfltstr$3,                   /* Default Store from USERINFO*/~
            discacct$12,                 /* Sales Discounts Account    */~
            discacctdescr$32,            /*                            */~
            discacctl$(100)12,           /* Sales Discounts (Lines)    */~
            discacctldescr$32,           /*                            */~
            discamt$10,                  /* Line Item Discount Amount  */~
            dr_date$(36)8,               /* Date Run - Due Date Array  */~
            dr_qty$(36)10,               /* Date Run - Quantity Array  */~
            dr_ship$(36)8,               /* Date Run - Ship Date Array */~
            duedate$(100)8,              /* Due Date                   */~
            e_lines%(100),               /* Potential Extra Lines      */~
            edtmessage$79,edtmessage2$79,/* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            edat$(7)6,                   /* Estimate Dates             */~
            edsc(7),                     /* Estimate Discounts         */~
            eprc(7),                     /* Estimate Prices            */~
            eqty(7),                     /* Estimate Quantities        */~
            est$(100)8,                  /* Estimate Number For Line   */~
            est_cutover$(100)1,          /* Estimate Cutover Flag      */~
            est_rte$(100)3,              /* Estimate Cutover Route ID  */~
            expdate$8,                   /* Quote Expire Date          */~
            export$1,                    /* Export Flag                */~
            ext$10,                      /* Line Item Extension        */~
            fac25$1,                     /* PF(25) FAC                 */~
            fob$20,                      /* FOB                        */~
            hdr$(8)25,                   /* Summary Screen Headings    */~
            how_held$1,                  /* 'M'anual or 'blank'        */~
            howship$20,                  /* How Ship                   */~
            incl(3),                     /* Plowcode argument          */~
            incl$(3)16,                  /* Plowcode argument          */~
            inputmsg$79,                 /* Input message for ASKDATE  */~
            inpmessage$79,               /* Informational Message      */~
            item$(100)3,                 /* P.O. Item Number           */~
            keepcus$9, keepso$16,        /* Temps for copy function    */~
            lfac$(20)1, lfac2$(20)1,     /* Field Attribute Characters */~
            xfac$1,                      /* Fac For Mfg Code Entry     */~
            lastchanged$8, lastuser$,    /* Last Change Info           */~
            lastlit$30,                  /* Last SO # Message          */~
            lastso$16,                   /* Saves SO number            */~
            line2$79,                    /* Second Line of Screen Headr*/~
            linedisc(100), linedisc$6,   /* Line Item Discount %       */~
            lot$(100)6,                  /* Lot Number                 */~
            lsts$(100)3,                 /* Line Status                */~
            manadjre$1,                  /* Mandatory Adj Reason Code? */~
            mfgcode$9,                   /* Xref Mfg Code              */~
            mfgdescr$30,                 /* PLOWCODE Description       */~
            mlqyorn$1, mlqdelt$1,        /* MLQ processing params      */~
            mlquote_cut%(100),           /* Indicates C/O this session */~
            mlquote_seq$(100)11,         /* Quote & Seq LI derived from*/~
            msg$79,                      /* Misc use Message           */~
            newdate$8,                   /* Change date for line items */~
            nonstockmsg$(100)14,         /* Non-Stock Part Flag & Msg  */~
            offset_due$3,                /* Offset Days for Due Date   */~
            offset_shp$3,                /* Offset Days for Ship Date  */~
            opc$1,                       /* Price code for options     */~
            opdlrs(4),                   /* BOMOPSUB Cost, Price Info  */~
            openqty(100), openqty$10,    /* Open Order Quantity        */~
            optmsg$30,                   /* Options on file message    */~
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
            parttype$(100)3,             /* Part Type                  */~
            passpart$(72)25,             /* Parts passed from SZRINSUB */~
            passqty$(72)10,              /* Qtys  passed from SZRINSUB */~
            pc$1, pcdescr$32,            /* Price Code                 */~
            pf$(3)79, pfkey$32,          /* PF Prompts and Keys        */~
            pickprint$1,                 /* Pick/BOL/Ack Print Flag    */~
            pickprint$(3)1,              /* Pick/BOL/Ack Print Array   */~
            plowhdr$(3)79,               /* PLOWCODE Headers           */~
            plowkey$50,                  /* Misc PLOW Key              */~
            plowkey2$99,                 /* Misc PLOW Key              */~
            plowkey3$99,                 /* Misc Read/Plow Key         */~
            pm_hdr_dsply$30,pm_hdr_dsply2$30, /* Display header var    */~
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
            preinvmsg$32,                /*                            */~
            price(100), price$10,        /* Unit Price (Pricing UOM)   */~
            pricestk(100), pricestk$10,  /* Unit Price (Stockng UOM)   */~
            priceuom$(100)4,             /* Pricing Unit of Measure    */~
            priceuomdescr$32,            /* Pricing Unit of Measure    */~
            priority$(100)1,             /* Planning Priority Code     */~
            project$(100)8,              /* Project Number             */~
            projectdescr$30,             /* Project Description        */~
            ptdisp$(100)25,              /* Part Number Display Array  */~
            qtyschld(100), qtyschld$10,  /* Total Qty in Shipping      */~
            quotenbr$8, quotesave$8,     /* Quotation # & Work Area    */~
            record$150,                  /* Misc working variable      */~
            readkey$50,                  /* Misc Read Key              */~
            refdesc$(100)32,             /* Reference Part Description */~
            refpart$(100)25,             /* Reference Part Number      */~
            reftype$(100)1,              /* Ref Type 'M'anuf, 'C'ustmr */~
            region$4,                    /* Region Code                */~
            regiondescr$32,              /*                            */~
            requestr$20,                 /* Dummy variable for PUTPARM */~
            salesacct$(100)12,           /* Sales Distr. Account       */~
            salesacctdescr$32,           /*                            */~
            salesman$(3)4,               /* Salesman Code / Split %    */~
            salesmandescr$(3)32,         /*                            */~
            scode$4,                     /* XRef Source Code           */~
            seq$(100)3, saveseq$3,       /* Line Item Number           */~
            scr%(4,16), set%(255),       /* Soft Enables / Screen Refs */~
            sext$(15)10,                 /* Summary- Extension         */~
            sfac$(15)1,                  /* Summary- FACs              */~
            ship(100), ship$10,          /* Quantity Shipped           */~
            shipcode$(100)1,             /* Shipping Prioirity Code    */~
            shipcode$1,                  /* Cust Deflt Ship Priority   */~
            shipdate$(100)8,             /* Required Ship Date         */~
            shipinstr$(2)50,             /* Shipping Instructions      */~
            shipto$(6)30,                /* Ship-to                    */~
            so$16,                       /* Sales Order Number         */~
            soassgn$3,                   /* Sales Order # Asgnmnt Flag */~
            so_cut$1,                    /* Cutover of Estimates OK?   */~
            soldto$(6)30,                /* Sold-to                    */~
            sopen$(15)10,                /* Summary- Open Qty          */~
            sorder$(15)10,               /* Summary- Order Qty         */~
            statutory$4,                 /* Statutory currency code    */~
            stkuom$(100)4,               /* Stocking Unit of Measure   */~
            stkuomdescr$32,              /*                            */~
            store$3,                     /* Store Code                 */~
            storedescr$32,               /*                            */~
            taxable$(100)1,              /* Line Taxable? (Y/N)        */~
            temp$16,                     /* Temporary File             */~
            tempcus$9, tempso$16,        /* Temp Customer Code, SO#    */~
            terms$20, termsdescr$30,     /* Payment Terms (Code)       */~
            testdate$8,                  /* Variable for testing dates */~
            text$(113,1)70,              /* Text Array                 */~
            textid$4, textidl$(100)4,    /* Text ID- Hdr, Lines        */~
            tlines%(2),                  /* Lines Recap                */~
            topen(4), torder(4),         /* Dollars Recap              */~
            txt$4,                       /* For testing TEXTID values  */~
            userid$3,                    /* Current User Id            */~
            vf$200,                      /* Variable Fields            */~
            weekday$9, weekdayh$9        /* Day of week                */
        /* Data file arrays                                            */
        dim f2%(64),                     /* = 0 if the file is open    */~
            fs%(64),                     /* = 1 if file found          */~
            f1%(64)                      /* = 1 if READ was successful */

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
            * #09 ! BCKBUFFR ! Backlog buffer for SO headers            *~
            * #10 ! BCKBUF2  ! Buffer for line items                    *~
            * #11 ! CATEGORY ! Inventory category codes file            *~
            * #12 ! STORNAME ! Store Info File - Name/Address           *~
            * #13 ! HNYQUAN  ! Inventory Part / Store / Lot Quantity Fi *~
            * #14 ! JOBMASTR ! WIP/JC Job Master File                   *~
            * #15 ! HNYGENER ! generic xref file                        *~
            * #16 ! BOMMASTR ! BOM relationship file                    *~
            * #17 ! BOMSPEC  ! options selected file                    *~
            * #18 ! DEMMASTR ! Demand Master File                       *~
            * #19 ! CALMASTR ! Planning Production Calendar File        *~
            * #20 ! ENGMASTR ! Engineering Master Filer                 *~
            * #21 ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #22 ! TXTFILE  ! System Text File                         *~
            * #23 ! ESTMASTR ! Estimate master file                     *~
            * #24 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #25 ! PIPIN    ! Planned inventory additions detail       *~
            * #26 ! PIPOUT   ! Planned inventory use detail             *~
            * #27 ! PIPCROSS ! hard peg cross reference file            *~
            * #28 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #29 ! HNYDETAL ! Inventory Movement Detail File           *~
            * #30 ! BCKHLNES ! Sales Order History Line item detail     *~
            * #31 ! BCKHMSTR ! Sales Order History Master               *~
            * #32 ! USERINFO ! INDIVIDUAL USER DEFAULT INFORMATION      *~
            * #33 ! WORKFILE ! For Valid Allocation Types               *~
            * #34 ! VENDOR   ! Vendor Master File                       *~
            * #35 ! HNYPROC  ! Procurement History File                 *~
            * #36 ! VENPRICE ! Vendor Price Catalogue File              *~
            * #37 ! HNYALTRS ! Alternate Parts File                     *~
            * #38 ! BOMSPHDR ! Header file for BOMSPEC                  *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #41 ! BCKLNCUR ! Currency-specific BCK line items         *~
            * #42 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #43 ! BCKHCLNS ! Currency-specific History line items     *~
            * #44 ! USERLCMS ! CAELUS User List File                    *~
            * #45 ! CUSPTXRF ! Customer Part Number Cross Reference     *~
            * #46 ! MLQMASTR ! Quotation Master file                    *~
            * #47 ! MLQLINES ! Quotation Line Item file                 *~
            * #48 ! MLQLNCUR ! Currency-specific MLQ line items         *~
            * #49 ! MLQSPEC  ! BOMSPEC file for quoteations             *~
            * #50 ! MLQSPHDR ! Header file for MLQSPEC options          *~
            * #51 ! CCRMASTR ! Customer Credit Master file              *~
            * #52 ! MLQPMSLD ! MLQ Shadow file of Precious Metal Srchges*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #02,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03,  "GLMAIN",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #04,  "HNYMASTR",                                     ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #05,  "BCKMASTR",                                     ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #06,  "BCKLINES",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #07,  "SLMMASTR",                                     ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #08,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #09,  "BCKBUFFR",                                     ~
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

            select #18, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key 1, keypos =10, keylen = 19,              ~
                            key 2, keypos = 1, keylen = 28

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

            select #23, "ESTMASTR",                                      ~
                        varc,     indexed,  recsize =  2000,             ~
                        keypos =   10, keylen =  8,                      ~
                        alt key  1, keypos =  18, keylen =  30, dup,     ~
                            key  2, keypos =  48, keylen =  25, dup,     ~
                            key  3, keypos =   1, keylen =  17,          ~
                            key  4, keypos = 416, keylen =  16, dup,     ~
                            key  5, keypos =1855, keylen =  33

            select #24, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #25, "PIPIN",                                         ~
                        varc,     indexed,  recsize =    60,             ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #26, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #27, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =   150,             ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33

            select #28, "SFCUM2",                                        ~
                        varc,     indexed,  recsize =  1985,             ~
                        keypos =    1, keylen =  25

            select #29, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =   150,             ~
                        keypos =      1, keylen = 42,                    ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select #30,  "BCKHLNES",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #31,  "BCKHMSTR",                                     ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #32, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos = 1, keylen = 3

            select #33, "WORKAREA",                                      ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos =    1, keylen =  1

            select #34,  "VENDOR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 600,                                  ~
                         keypos = 1, keylen = 9,                         ~
                         alt key 1, keypos = 10, keylen = 30, dup

            select #35, "HNYPROC",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 134,                                  ~
                         keypos =32, keylen = 40,                        ~
                         alternate key 1, keypos = 7, keylen = 65,       ~
                                   key 2, keypos = 1, keylen = 40, dup,  ~
                                   key 3, keypos =41, keylen = 31, dup

            select #36, "VENPRICE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 10, keylen = 59,                       ~
                         alternate key 1, keypos = 1, keylen = 34, dup,  ~
                                   key 2, keypos =35, keylen = 34

            select #37, "HNYALTRS",                                      ~
                        varc,     indexed,  recsize =  60,               ~
                        keypos =    1, keylen =  33

            select #38, "BOMSPHDR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  53,                     ~
                        alt key  1, keypos =   35, keylen =  19

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

            select #43,  "BCKHCLNS",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 19,                      ~
                        alt key  1, keypos =   1, keylen =  23

            select #44, "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =    4, keylen =  30, dup

            select #45, "CUSPTXRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   26, keylen =  35,                     ~
                        alt key  1, keypos =  1, keylen = 60

            select #46,  "MLQMASTR",                                     ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =   10, keylen =   8,                     ~
                        alt key  1, keypos =    1, keylen =  17

            select #47,  "MLQLINES",                                     ~
                        varc,     indexed,  recsize = 400,               ~
                        keypos =  10,  keylen = 11

            select #48,  "MLQLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 11,                      ~
                        alt key  1, keypos =   1, keylen =  15

            select #49, "MLQSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #50, "MLQSPHDR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  53,                     ~
                        alt key  1, keypos =   35, keylen =  19

            select #51, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            select #52, "MLQPMSLD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen =  21,                     ~
                         alternate key 1, keypos = 22, keylen = 25, dup, ~
                                   key 2, keypos = 65, keylen =  9, dup

        call "SHOSTAT" ("Opening Files, One Moment Please")
            f1%(5%), f1%(9%), f1%(17%), f1%(51%), f1%(38%) = 200%
            f1%(6%), f1%(10%), f1%(41%) = 400%

            call "OPENCHCK" (#01, 0%, f2%(01%), f1%(01%), " ")
            call "OPENCHCK" (#02, 0%, f2%(02%), f1%(02%), " ")
            call "OPENCHCK" (#03, 0%, f2%(03%), f1%(03%), " ")
            call "OPENCHCK" (#04, 0%, f2%(04%), f1%(04%), " ")
            call "OPENCHCK" (#05, 0%, f2%(05%), f1%(05%), " ")
            call "OPENCHCK" (#06, 0%, f2%(06%), f1%(06%), " ")
            call "OPENCHCK" (#07, 0%, f2%(07%), f1%(07%), " ")
            call "OPENCHCK" (#08, 0%, f2%(08%), f1%(08%), " ")
            call "OPENCHCK" (#09, 0%, f2%(09%), f1%(09%), " ")
            call "OPENCHCK" (#10, 0%, f2%(10%), f1%(10%), " ")
            call "OPENCHCK" (#11, 0%, f2%(11%), f1%(11%), " ")
            call "OPENCHCK" (#12, 0%, f2%(12%), f1%(12%), " ")
            call "OPENCHCK" (#13, 0%, f2%(13%), f1%(13%), " ")
            call "OPENCHCK" (#14, 0%, f2%(14%), f1%(14%), " ")
            call "OPENCHCK" (#15, 0%, f2%(15%), f1%(15%), " ")
            call "OPENCHCK" (#16, 0%, f2%(16%), f1%(16%), " ")
            call "OPENCHCK" (#17, 0%, f2%(17%), f1%(17%), " ")
            call "OPENCHCK" (#18, 0%, f2%(18%), f1%(18%), " ")
            call "OPENCHCK" (#19, 0%, f2%(19%), f1%(19%), " ")
            call "OPENCHCK" (#20, 0%, f2%(20%), f1%(20%), " ")
            call "OPENCHCK" (#21, 0%, f2%(21%), f1%(21%), " ")
            call "OPENCHCK" (#22, 0%, f2%(22%), f1%(22%), " ")
            call "OPENCHCK" (#23, 0%, f2%(23%), f1%(23%), " ")
            call "OPENCHCK" (#24, 0%, f2%(24%), f1%(24%), " ")
            call "OPENCHCK" (#25, 0%, f2%(25%), f1%(25%), " ")
            call "OPENCHCK" (#26, 0%, f2%(26%), f1%(26%), " ")
            call "OPENCHCK" (#27, 0%, f2%(27%), f1%(27%), " ")
            call "OPENCHCK" (#28, 0%, f2%(28%), f1%(28%), " ")
            call "OPENCHCK" (#29, 0%, f2%(29%), f1%(29%), " ")
            call "OPENCHCK" (#30, 0%, f2%(30%), f1%(30%), " ")
            call "OPENCHCK" (#31, 0%, f2%(31%), f1%(31%), " ")
            call "OPENCHCK" (#32, 0%, f2%(32%), f1%(32%), " ")
            call "OPENCHCK" (#34, 0%, f2%(34%), f1%(34%), " ")
            call "OPENCHCK" (#35, 0%, f2%(35%), f1%(35%), " ")
            call "OPENCHCK" (#36, 0%, f2%(36%), f1%(36%), " ")
            call "OPENCHCK" (#37, 0%, f2%(37%), f1%(37%), " ")
            call "OPENCHCK" (#45, fs%(45%), f2%(45%), 0%, " ")
            call "OPENCHCK" (#38, 0%, f2%(38%), f1%(38%), " ")
            call "OPENCHCK" (#51, 0%, f2%(51%), f1%(51%), " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#32, userid$, f1%(32%))
            if f1%(32%) = 1% then get #32 using L09045, dfltstr$
L09045:         FMT POS(64), CH(3)


            date$ = date : call "DATEFMT" (date$)
            ll%   = 6%
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Field & Press (RETURN)."
            edtmessage2$ = "To Modify Field, Position Cursor & Press (R"&~
                           "ETURN). (PF6/7=Prev/Next Line Item)"

            lastso$ =  " "
            ref%, refflag%, cust% = 0%          /* Cross Reference Flags*/
            cms% = 1%                           /* Cross Reference Flags*/
            xref% = fs%(45%)                    /* Does CUSPTXRF Exist? */

*        Clear any Sales Order for this User that was in process.
            readkey$ = all(hex(00)) : str(readkey$,,3%) = userid$
            call "READ101" (#09, readkey$, f1%(9%))
            if f1%(9%) = 0% then L09190
                so$ = key(#09, 2%)
                get #09 using L09135, readkey$
L09135:              FMT XX(10), CH(9)
                if readkey$ = "BCKINPUT" then L09175
                     call "ASKUSER" (keyhit%, "** IN-PROCESS MESSAGE **",~
                          "You did not complete processing Sales Order "&~
                               so$,                                      ~
                          "in the program " & str(readkey$,,8) & ".",    ~
                          "Press (RETURN) to exit this program.")
                     goto exit_program
L09175:         delete #09
                call "DELETE" (#10, so$, 16%)
                call "PTUSEDSB" ("D", "BCK ", so$, "ALL",                ~
                                 " ", " ", " ", ret%)/* Del Xref Shadow */

L09190
*        Set some flags and misc variables
            call "BCKSWTCH" ("BCK", "SOASSIGN", soassgn$  , temp, u3%)
            call "BCKSWTCH" ("BCK", "CRHOLD  ", crflag$   , temp, u3%)
            call "BCKSWTCH" ("BCK", "ALLOCATE", allocdflt$, temp, u3%)
            call "BCKSWTCH" ("BCK", "PICKLIST", dfltpick$ , temp, u3%)
            call "BCKSWTCH" ("BCK", "BOL     ", dfltbol$  , temp, u3%)
            call "BCKSWTCH" ("BCK", "DELETE", allow_delete$, temp, u3%)
            call "BCKSWTCH" ("BCK", "MANADJRE", manadjre$, temp, u3%)
            call "BCKSWTCH" ("BCK", "DEM_TYPE", dflt_dem_type$, temp, u3%)
                if dflt_dem_type$ <> "2" then dflt_dem_type$ = "1"
            call "BCKSWTCH" ("BCK", "OFFSTDUE", offset_due$, offsetd, u3%)
                offsetd% = offsetd
            call "BCKSWTCH" ("BCK", "OFFSTSHP", offset_shp$, offsets, u3%)
                offsets% = offsets
            call "BCKSWTCH" ("BCK", "OVRRIDCR", override_crhld$, temp,u3%)
            allow_delete_save$ = allow_delete$
            call "BCKSWTCH" ("BCK", "CRHOLDAR", cr_ar_only$, temp, u3%)

            readkey$ = all(hex(00))      /* GENCODES Directory         */
            str(readkey$, 10) = "UOM"
            call "READ100" (#08, readkey$, uom%)

            hdr$(1%) = "Part Code"
            hdr$(2%) = "Lin"
            hdr$(3%) = "PO"
            hdr$(4%) = " Order Qty"
            hdr$(5%) = "  Open Qty"
            hdr$(6%) = " Extension"
            hdr$(7%) = "Rqd Ship"
            hdr$(8%) = " ST"

*        See if operator is an administrator or not
            call "CMSMACHK" ("BCK", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then admin% = 1%

*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            readkey$ = "SWITCHS.CUR"
            call "READ100" (#02, readkey$, f1%(2%))
            if f1%(2%) <> 0% then get #02 using L09390, curr$, statutory$, ~
                currtype$
L09390:         FMT POS(21), CH(1), CH(4), POS(27), CH(1)
            if curr$ <> "Y" then statutory$, currtype$ = " "
            if curr$ <> "Y" then L09430  /* No need to open MC files */
                call "OPENCHCK" (#40, 0%, f2%(40%), f1%(40%), " ")
                call "OPENCHCK" (#41, 0%, f2%(41%), f1%(41%), " ")
                call "OPENCHCK" (#42, 0%, f2%(42%), f1%(42%), " ")
                call "OPENCHCK" (#43, 0%, f2%(43%), f1%(43%), " ")

L09430
*        See if CANREASON files in place
            can$ = "N"
            readkey$ = "CANREASON"
            call "PLOWNEXT" (#08, readkey$, 9%, f1%(8%))
              if f1%(8%) = 1% then can$ = "Y"

            gosub check_if_pm_on /* Check if Precious Metal Surcharge ON*/
            gosub init_enables

*        Write Allocation Options for GETCODE Selection
            call "WORKOPEN" (#33, "IO   ", 5%, 1%)
            write #33 using L09505, "A", "Allocate to ATC"
            write #33 using L09505, "N", "Don't Allocate (Total Planning)"
            write #33 using L09505, "C", "Allocate Complete (No Planning)"
            write #33 using L09505, "P", "Already Allocated; No Change"
            write #33 using L09505, "Z", "Zero Out Allocation"
L09505:         FMT CH(1), CH(49)

            readkey$ = "SWITCHS.COR"
            call "READ100" (#02, readkey$, core%)

*        Get Multi-Line Quotation processing values, if present.
            mlqdelt$, mlqyorn$ = "N"/* Default = Negatory on MLQ front */
            readkey$ = "SWITCHS.MLQ"
            call "READ100" (#02, readkey$, f1%(2%))   /* SYSFILE2 */
            if f1%(2%) <> 0% then get #02 using L09545, mlqyorn$, mlqdelt$
L09545:         FMT POS(32), CH(1), POS(43), CH(1)
            if mlqyorn$ <> "Y" then goto L09575     /* No MLQ processing */
                call "OPENCHCK" (#46, fs%(46%), f2%(46%), 0%, " ")
                call "OPENCHCK" (#47, fs%(47%), f2%(47%), 0%, " ")
                call "OPENCHCK" (#49, fs%(49%), f2%(49%), 0%, " ")
                call "OPENCHCK" (#50, fs%(50%), f2%(50%), 0%, " ")
                if curr$ <> "Y" then L09575  /* No need to open MC files */
                     call "OPENCHCK" (#48, fs%(48%), f2%(48%), 0%, " ")

L09575
*        Get Estimate processing values, if any
            readkey$ = "SWITCHS.EST"  :  so_cut$ = "N"
            call "READ100" (#02, readkey$, f1%(2%))     /* SYSFILE2 */
                if f1%(2%) = 0% then  L10000
            get #2 using L09625, so_cut$
L09625:         FMT POS(211), CH(1)


L10000: REM *************************************************************~
            *       I N P U T   M O D E   -  H E A D E R S              *~
            *-----------------------------------------------------------*~
            * Handles normal input for header screens.                  *~
            *************************************************************

        inputmode
            gosub init_for_input

            mostin% = 0%  :  s% = 1%  :  edit% = 1%
            for fieldnr% = 1% to 11%     /* First Screen               */
                if fieldnr% > mostin% then gosub L20000
                gosub'050(1%, fieldnr%, 1%)
                      if enabled% = 0% then L10130
L10065:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <> 27% then goto L10075
                          appendquote% = 0%     /* Not appending lines */
                          gosub cutover_quote_from_top
L10075:               if keyhit%  =  3% and fieldnr% = 3 then            ~
                                                   gosub copy_function
                      if keyhit% <>  4% then       L10120
                         errormsg$ = " "
                         if fieldnr% <= 2% then L10120
L10095:                  fieldnr% = max(3%, fieldnr% - 1%)
                         gosub'050(1%, fieldnr%, 1%)
                         if enabled% = 1% then L10065
                         if fieldnr% = 3% then L10065
                         goto L10095
L10120:               if keyhit%  = 16 and fieldnr% = 1% then exit_program
                      if keyhit% <>  0% then       L10065
L10130:         gosub L50000             /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10065
                      if fieldnr% = 1% and so$ <> " " then fieldnr% = 2%
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

            mostin% = 0%
            for fieldnr% = 1% to 12%
                if fieldnr% > mostin% then gosub L21000
                gosub'050(2%, fieldnr%, 1%) /* Set enables, input msg */
                      if enabled% = 0% then L10225
L10180:         gosub'102(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10220
                         errormsg$ = " "
L10195:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(2%, fieldnr%, 1%)
                         if enabled% = 1% then L10180
                         if fieldnr% = 1% then L10180
                         goto L10195
L10220:               if keyhit% <>  0 then       L10180
L10225:         gosub L51000
                      if errormsg$ <> " " then L10180
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

*        Do Input for Variable Fields
            call "VFINPSUB" ("BCKMASTR", "I", "Manage Sales Orders   ",  ~
                             str(line2$,,60), "NN", vf$, keyhit%)
            if keyhit% = 1% then startover2
            goto L11000

        copy_function     /* Allow selction of order to copy in        */
            readkey$ = " "
            inpmessage$ = hex(0684) & "Select Order to COPY from Curre"& ~
                                    "nt files"
            if f2%(31) = 0% then inpmessage$ = inpmessage$ &             ~
                                            " or PF16 for History Copy"
            call "GETCODE" (#05, readkey$, inpmessage$, 0%, 1.16, f1%(5))
            if f1%(5) = 0% then L10308
                copy_what% = 5%
                goto L10318
L10308:     if f2%(31) <> 0% then return   /* No History File */
            inpmessage$ = hex(0684) & "Select Order to COPY from Histo"& ~
                                      "ry files"
            call "GETCODE" (#31, readkey$, inpmessage$, 0%, 1.16, f1%(31))
            if f1%(31) = 0% then return
                copy_what% = 31%

L10318
*        Now get Order, new dates, etc., etc., etc..
            return clear
            keepcus$ = cuscode$
            keepso$  = so$
            cuscode$ = str(readkey$,,9)
            so$      = str(readkey$,10)
            copy% = 1%
            allow_delete$ = "Y"
            gosub copy_order

            soonfile% = 0%
            if cuscode$ = keepcus$ then L10390
                cuscode$  = keepcus$
                call "READ100" (#01, cuscode$, f1%(1))
                get #01 using L10385, soldto$(), pm_cus_so$, pm_cus_inv$, ~
                                                    dfltacks$, shipto$()
L10385:              FMT POS(40), 6*CH(30), POS(226), 2*CH(01),          ~
                                      POS(238), CH(1), POS(253), 6*CH(30)
L10390:     so$       = keepso$
            orderdate$, origdate$ = date
                call "DATEFMT" (orderdate$)
                call "DATEFMT" (origdate$ )
            origuser$ = userid$
            lastchanged$, lastuser$ = " "
            gosub set_pm_flags
            nextbol% = 1%
            mostin% = 0%
            for fieldnr% = 9% to 11%
                if fieldnr% > mostin% then gosub L20000
                gosub'050(1%, fieldnr%, 1%)
L10445:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                     if keyhit%  =  1 then gosub startover
                     if keyhit% <>  4 then       L10475
                          errormsg$ = " "
                          fieldnr% = max(9%, fieldnr% - 1%)
                          gosub'050(1%, fieldnr%, 1%)
                          goto L10445
L10475:              if keyhit% <>  0 then       L10445
                gosub L50000             /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10445
                     mostin% = max(mostin%, fieldnr%)
            next fieldnr%

            option% = 0% : parttype$(c%)  = " "
            for c% = 1% to maxlines%
                if option% = 1% then L10515
                    call "READ100" (#04, part$(c%), f1%(4%))
                    if f1%(4%) <> 0% then get #04 using L10509,           ~
                                               parttype$(c%),  alloc$(c%)
L10509:                  FMT POS(180), CH(3), POS(242), CH(1)
                    if parttype$(c%) = "000" then option% = 1%
L10515:         convert c% to seq$(c%), pic(##0)
                ship(c%), qtyschld(c%), alloc(c%), preinv(c%) = 0
                openqty(c%) = order(c%)
                origdue$(c%), duedate$(c%) = dfltdue$
                shipdate$(c%) = dfltship$
                if alloc$(c%) = " " then alloc$(c%) = allocdflt$
                lsts$(c%) = " A "
                est$(c%), est_cutover$(c%), est_rte$(c%) = " "
            next c%
            if option% = 0% then L10549
                u3% = 2%
                call "ASKUSER" (u3%, "OPTION PART COPIED", "At least 1 "&~
                     "Option part has been copied.", "Option selections"&~
                     " have NOT been copied and MUST be made manually.", ~
                     "Press RETURN to confirm")
                option% = 0%
L10549:     gosub adjust_currency_conversion
            lastseq% = maxlines%
            copy% = 0%
            goto edithdr1

        adjust_currency_conversion
            if currency$ = " " then currency$ = statutory$
            if currency$ = statutory$ then return
            call "DATREVRS" ( orderdate$, rev_date$, errormsg$ )
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#42, currkey$, 5%, f1%(42%))  /* CURCONVR */
            if f1%(42%) = 0% then return
            get #42 using L10708, convdate$, conveqv, convunt
L10708:         FMT POS(12), CH(6), 2*PD(14,7)
            return

L11000: REM *************************************************************~
            *       I N P U T  /  A P P E N D   L I N E S               *~
            *-----------------------------------------------------------*~
            * Handles input or appending of line Items.                 *~
            *************************************************************

        appendlines
L11090:     if maxlines%  = 100% then L11230
            if lastseq%  >= 999% then L11230
            if maxlines% + total_e_lines% <  100% then L11110
               gosub extra_load_conflict

L11110:         c% = maxlines% + 1%
                gosub clear_line
                    convert lastseq% + 1% to seq$(c%), pic(###)
                gosub inputline
                if keyhit% = 16% then endinsert
                    maxlines% = maxlines% + 1%
                    lastseq%  = lastseq%  + 1%
                    convert lastseq% to seq$(c%), pic(###)
                    str(lsts$(c%),2,1) = "A"
                    goto L11090
            endinsert
                gosub clear_line
L11230:         l% = max(0%, min(85%,maxlines%-12%))  /* Last Screen   */
                if maxlines% = 0% then edithdr1 else line_summary

        inputline
            clear%  = 1%       /* For Options Entry */
            mostin% = 0%
            for fieldnr% = 1% to 16%     /* First Line Item Screen     */
                if fieldnr% > mostin% or fieldnr% < 5% then gosub L22000

                gosub'050(3%, fieldnr%, 1%)
                      if enabled% = 0 then L11440
          reenter_from_size_run
L11330:         gosub'103(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub restart_line
                      if keyhit% <>  3 then       L11360
                         gosub copy_line
                         modfld% = 1% /* Line copied */
                         if e_warn% <> 0% then inputline
                         return
L11360:               if keyhit% <>  4 then       L11420
                         errormsg$ = " "
L11363:                  if fieldnr% <> 3% then L11370
                            total_e_lines% = total_e_lines% - e_lines%(c%)
                            e_lines%(c%) = 0%
L11370:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(3%, fieldnr%, 1%)
                         if enabled% = 1% then L11330
                         if fieldnr% = 1% and enabled% = 0% then L11470
                         if fieldnr% = 1% then L11330
                         goto L11363
L11420:               if keyhit%  = 16 and fieldnr% < 3% then return
                      if keyhit%  = 18% then gosub show_part_master_data
                      if keyhit%  = 6% then size_run
                      if keyhit%  = 11% then date_run
                      if keyhit%  = 22% or keyhit% = 23% then L11440
                      if keyhit% <>  0 then       L11330
L11440:         gosub'153(fieldnr%, 1%)
                      if errormsg$ <> " " then L11330
                      mostin% = max(mostin%, fieldnr%)
L11470:     next fieldnr%

            mostin% = 0%
            for fieldnr% = 1% to 8%      /* Second Line Item Screen    */
                if fieldnr% > mostin% then gosub L23000
                gosub'050(4%, fieldnr%, 1%)
                      if enabled% = 0 then L11650
L11540:         gosub'104(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub restart_line
                      if keyhit% <>  4 then       L11630
                         errormsg$ = " "
L11580:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(4%, fieldnr%, 1%)
                         if enabled% = 1% then L11540
                         if fieldnr% = 1% then L11540
                         goto L11580
L11630:               if keyhit%  = 16 then       exit_program
                      if keyhit%  = 18% then gosub show_part_master_data
                      if keyhit% <>  0 then       L11540
L11650:         gosub L54000
                      if errormsg$ <> " " then L11540
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%
            modfld% = 1%      /* Added a line */
            return

        copy_line   /* Copy Line Logic.  We will assume that copied */
                    /* data is just a valid as it was when entered. */
            call "SHOSTAT" ("Copying Previous Line...")

            e_warn%        = 0%
            total_e_lines% = total_e_lines% - e_lines%(c%)
            e_lines%(c%)   = 0%

        REM EST$(C%)         = EST$(C%-1%)/* REMd per KAB on PRR 13020 */
            est_cutover$(c%) = est_cutover$(c%-1%)
            est_rte$(c%)     = est_rte$(c%-1%)
            part$(c%)        = part$(c%-1%)
            refpart$(c%)     = refpart$(c%-1%)
            refdesc$(c%)     = refdesc$(c%-1%)
            reftype$(c%)     = reftype$(c%-1%)
            nonstockmsg$(c%) = nonstockmsg$(c%-1%)
            descr$(c%)       = descr$(c%-1%)
            cat$(c%)         = cat$(c%-1%)
            duedate$(c%)     = duedate$(c%-1%)
            origdue$(c%)     = origdue$(c%-1%)
            shipdate$(c%)    = shipdate$(c%-1%)
            stkuom$(c%)      = stkuom$(c%-1%)
            order(c%)        = order(c%-1%)
*          ALLOC$(C%)       = ALLOC$(C%-1%)
                call "READ100" (#04, part$(c%), f1%(4%))
                    if f1%(4%) <> 0% then get #04 using L11853, alloc$(c%)
L11853:                   FMT POS(242), CH(1)
                if alloc$(c%) = " " then alloc$(c%) = allocdflt$
            openqty(c%)      = openqty(c%-1%)
            lot$(c%)         = lot$(c%-1%)
            parttype$(c%)    = parttype$(c%-1%)
            priceuom$(c%)    = priceuom$(c%-1%)
            conv(c%)         = conv(c%-1%)
            pricestk(c%)     = pricestk(c%-1%)
            price(c%)        = price(c%-1%)
            linedisc(c%)     = linedisc(c%-1%)
            taxable$(c%)     = taxable$(c%-1%)
            item$(c%)        = item$(c%-1%)
            priority$(c%)    = priority$(c%-1%)
            demtype$(c%)     = demtype$(c%-1%)
            project$(c%)     = project$(c%-1%)
            salesacct$(c%)   = salesacct$(c%-1%)
            discacctl$(c%)   = discacctl$(c%-1%)
            bom$(c%)         = bom$(c%-1%)
            shipcode$(c%)    = shipcode$(c%-1%)
            pm_base(c%)      = price(c%)
            pm_adj%(c%)      = pm_adj%(c%-1%)

            gosub get_atc

            call "ARIEXTRA" (cuscode$, part$(c%), " ", e_lines%(c%), #2)
            total_e_lines% = total_e_lines% + e_lines%(c%)
            if c% + total_e_lines% <= 100% then L11950
               temp% = 0%
               gosub extra_append_conflict
                 if u3% = 16% then return
                 gosub clear_line
                 e_warn% = 1%
L11950:     return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edithdr1
            edit% = 2%
            gosub'050(1%, 0%, 2%)       /* Set input message           */
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
            gosub call_screen
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       line_summary
                  if keyhit%  =  5 then       edithdr2
                  if keyhit%  = 10 then       cancel_order
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 25 then gosub edit_text_hdr
                  if keyhit%  = 28 then       sdelete_order
                  if keyhit%  = 29 then       L12170
                  if keyhit% <>  0 then       edithdr1
L12170:     fieldnr% = cursor%(1) - 4%
                if fieldnr% <   3% or  fieldnr%  >  15% then edithdr1
                if fieldnr% >=  5% and fieldnr%  <= 10% then fieldnr% = 5%
                if fieldnr%  =  5% and cursor%(2) > 40% then fieldnr% = 6%
                if fieldnr% >= 11% then fieldnr% = fieldnr% - 4%
                if fieldnr%  = lastfieldnr% then edithdr1
            if keyhit% <> 29% then L12260
                gosub'049(1%, fieldnr%)
                goto edithdr1
L12260:     gosub'050(1%, fieldnr%, 2%) /* Set input message, enables  */
                  if enabled% = 0% then       edithdr1
L12280:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12280
            gosub L50000                 /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12280
                          lastfieldnr% = fieldnr%
                          goto L12170

        edithdr2
            gosub'050(2%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'102(0%, 2%)
            gosub call_screen
                  errormsg$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       line_summary
                  if keyhit%  =  4% then       edithdr1
                  if keyhit%  =  5% then       editvfs
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 25% then gosub edit_text_hdr
                  if keyhit%  = 28% then       sdelete_order
                  if keyhit%  = 29% then       L12480
                  if keyhit% <>  0% then       edithdr2
L12480:     fieldnr% = cursor%(1%) - 4%
                if fieldnr% <   1% or   fieldnr% >  15% then edithdr2
                if fieldnr%  =  4% then fieldnr%  =  3%
                if fieldnr% >=  9% and  fieldnr% <= 11% then fieldnr% = 9%
                if fieldnr% >   3% then fieldnr%  = fieldnr% - 1%
                if fieldnr% >= 11% then fieldnr%  = fieldnr% - 2%
                if fieldnr%  = lastfieldnr% then edithdr2
            if keyhit% <> 29% then L12580
                gosub'049(2%, fieldnr%)
                goto edithdr2
L12580:     gosub'050(2%, fieldnr%, 2%)
                  if enabled% = 0% then       edithdr2
L12600:     gosub'102(fieldnr%, 2%)
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12600
            gosub L51000
                  if errormsg$ <> " " then L12600
                     lastfieldnr% = fieldnr%
                     goto L12480

        editvfs      /* Edit Variable Fields       */
            call "VFINPSUB" ("BCKMASTR", "E", "Manage Sales Orders",     ~
                             str(line2$,,60), "YN", vf$, keyhit%)
            if keyhit% =  1% then startover2
            if keyhit% =  4% then edithdr2
            if keyhit% = 16% then datasave
                             goto edithdr1

        REM *************************************************************~
            *           E D I T   M O D E   -  L I N E S                *~
            *-----------------------------------------------------------*~
            * Handles edit mode for line items.                         *~
            *************************************************************

        line_summary              /* Summary Screen */
            aset$ = " "
            copy_line% = 0%
            if maxlines% = 0% then appendlines
            gosub'050(5%, 0%, 1%)
L13090:     inpmessage$ = "To Modify a Line Item, Position Cursor and" & ~
                          " press RETURN."

L13120:     gosub'115
            gosub call_screen
              errormsg$ = " "
              if keyhit% = 1% then gosub startover
              if keyhit% = 2% then l%=0%
              if keyhit% = 3% then l%=max(0%,min(85%,maxlines%-12%))
              if keyhit% = 4% then l%=max(0%,l%-12%)
              if keyhit% =5% then l%=max(0%,min(85%,l%+12%,maxlines%-12%))
              if keyhit% = 6% then l%=max(0%,l%-1%)
              if keyhit% = 7% then l%=max(0%,min(85%,l%+1%,maxlines%-1%))
              if keyhit% = 9% then       edithdr1
              if keyhit% =10% then       set_allocations
              if keyhit% =11% then       appendlines
              if keyhit% =12% then       L13270
              if keyhit% =16% then       datasave
              if keyhit% =18% then       L13270
              if keyhit% =23% then       mod_all_dates
              if keyhit% =24% then       L13270
              if keyhit%<>27% then       L13250
                appendquote% = 1%           /* Append Quote Lines only */
                gosub cutover_quote_from_top
L13250:       if keyhit% =28% then       sdelete_order
              if keyhit% =32% then       datasave_shortcut
              if keyhit%<> 0% then L13120
L13270:     c% = cursor%(1%) - 5%
            if c% < 1% or c% > 15% then L13120
                c% = c% + l%
                if c% < 1% or c% > maxlines% then L13120
            if keyhit% <> 18% then goto L13310
                gosub show_part_master_data
                goto L13120
L13310:     if keyhit%  = 12 then       sdelete_line
            if keyhit% <> 24 then       L13320
              copy_line% = 1%
              gosub describe_line
              for fieldnr% = 5% to 6%        /* Due Date Fields */
                gosub entry_for_modify_dates /* Forced Entry - Editmode */
                next fieldnr% : goto line_summary
L13320:     if keyhit% <>  0 then       L13120

*        Modify Line C%
            if str(lsts$(c%),2,1) <> "D" then L13420
                keyhit1% = 2%
                call "ASKUSER" (keyhit1%, "REACTIVATE LINE",             ~
                      "Enter PF-16 to Reactive Deleted Line Item",       ~
                      "- OR - ", "Hit any PF-Key to Return to Display.")
                if keyhit1% <> 16% then L13120
                str(lsts$(c%),2,1) = "R"
L13420:     gosub describe_line

        editline1
            screen% = 1%
            gosub'050(3%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'103(0%, 2%)
            gosub call_screen
            errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then       editline2
                  if keyhit%  =  6% then      previous_line_item
                  if keyhit%  =  7% then      next_line_item
                  if keyhit%  =  9 then       edithdr1
                  if keyhit%  = 12 then gosub delete_line
                  if keyhit%  = 16 then       line_summary
                  if keyhit%  = 18% then gosub show_part_master_data
                  if keyhit%  = 25 then gosub edit_text_line
                  if keyhit%  = 29 then       L13560
                  if keyhit% <>  0 then       editline1
L13560:     fieldnr% = cursor%(1) - 4%
                if fieldnr% <> 2% then L13600
                     clear% = 0% :  gosub options
                     goto editline1
L13600:         if fieldnr% = 1% and keyhit% = 29% then L13630
                if fieldnr% < 3% or fieldnr% > 16% then editline1
                if fieldnr% = lastfieldnr% then editline1
            if keyhit% <> 29% then L13650
L13630:         gosub'049(3%, fieldnr%)
                goto editline1
        entry_for_modify_dates   /* Comes from LI Summary Screen */
L13650:     gosub'050(3%, fieldnr%, 2%)
                  if enabled% = 0% then       editline1
L13670:     gosub'103(fieldnr%, 2%)
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L13670
            gosub'153(fieldnr%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13670
                  if copy_line% = 1% then return /* Back to Mod Dates */
                     lastfieldnr% = fieldnr%
                     goto L13560

        editline2
            screen% = 2%
            gosub'050(4%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'104(0%, 2%)
            gosub call_screen
            errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       editline1
                  if keyhit%  =  6% then      previous_line_item
                  if keyhit%  =  7% then      next_line_item
                  if keyhit%  =  9 then       edithdr1
                  if keyhit%  = 12 then gosub delete_line
                  if keyhit%  = 16 then       line_summary
                  if keyhit%  = 18% then gosub show_part_master_data
                  if keyhit%  = 25 then gosub edit_text_line
                  if keyhit%  = 29 then       L13870
                  if keyhit% <>  0 then       editline2
L13870:     fieldnr% = cursor%(1) - 5%
                if fieldnr% < 1% or fieldnr% > 8% then editline2
                if fieldnr% = lastfieldnr% then editline2
            if keyhit% <> 29% then L13930
                gosub'049(4%, fieldnr%)
                goto editline2
L13930:     gosub'050(4%, fieldnr%, 2%)
                  if enabled% = 0% then       editline2
L13950:     gosub'104(fieldnr%, 2%)
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L13950
            gosub L54000
                  if errormsg$ <> " " then L13950
                     lastfieldnr% = fieldnr%
                     goto L13870

        sdelete_order
            c% = 0%
        sdelete_line      /* Also bottom end of Order Delete           */
            inpmessage$  = " "
            if maxlines% = 0% then L13120
                if c% = 0% then L14110
                   if qtyschld(c%) = 0 then L14101
                        errormsg$ = "Can't delete a Scheduled Line Item"
                        goto L13120
L14101:            if preinv(c%)   = 0 then L14104
                       errormsg$ = "Can't delete a Pre-Invoiced Line Item"
                       goto L13120
L14104:            if ship  (c%)   = 0 then L14110
                       errormsg$ = "Can't delete Shipped Line Item"
                       goto L13120
L14110:         gosub'125(c%-l%)
                if keyhit% <>  0% then L13120
                gosub call_screen
                     i1%, i2% = c%
                     if c% = 0% then i1% = 1%
                     if c% = 0% then i2% = maxlines%
                     left% = 0%
                     for c% = i1% to i2%
                          if qtyschld(c%) = 0% then L14175
L14171:                      left% = left% + 1%
                             goto L14200
L14175:                   if preinv(c%)   = 0  then L14180 else L14171
L14180:                   if ship  (c%)   = 0  then L14184 else L14171
L14184:                   str(lsts$(c%),2,1) = "D"
                          modfld% = 1% /* Deleted line */
L14200:              next c%
            if left% <> 0% then errormsg$ =                              ~
           "Note: Scheduled, Shipped, Invoiced Lines will not be deleted."
            goto L13090

        delete_line  /* Delete the line that we're editing   */
            if qtyschld(c%) = 0 then L14272
                errormsg$ = "Can't delete a Scheduled Line Item"
                goto L14277
L14272:     if preinv(c%)   = 0 then L14275
                errormsg$ = "Can't delete a Pre-Invoiced Line Item"
                goto L14277
L14275:     if ship  (c%)   = 0 then L14280
                errormsg$ = "Can't delete Shipped Line Item"
L14277:         return
L14280:     u3% = 2%
            call "ASKUSER" (u3%, "DELETE LINE",                          ~
                         "Press RETURN to flag this line for deletion",  ~
                         "- OR -", "Any other PF key to abort delete.")
            if u3% <>  0% then return
                str(lsts$(c%),2,1) = "D"
                modfld% = 1% /* Deleted line */
                return clear
                goto line_summary

        cancel_order
            errormsg$ = " "
             if oldflag% = 1% then L14414
               errormsg$ = "This Order has not been Saved, therefore it"&~
                             " Cannot be Cancelled."
                   goto edithdr1
L14414:      if crhold$ <> "C" then L14435
               errormsg$ = "This Order is already Cancelled"
                   goto edithdr1
L14435:      if maxlines% = 0% then  L14498
               for c% = 1 to maxlines%
                 if ship(c%) = 0 then L14470
                   goto cancel_lines
L14470:          if qtyschld(c%) = 0 then L14485
                   goto cancel_lines
L14485:          if preinv(c%) = 0 then L14491
                   goto cancel_lines
L14491:        next c%
L14498:      u3% = 2%
             call "ASKUSER" (u3%, " ** CANCEL ORDER ** ",                ~
                             "Press PF10 to cancel order", "- or -",     ~
                             "PF16 to abort cancellation.")
             if u3% <> 10% and u3% <> 16% then L14498
             if u3% = 16% then edithdr1 /* was 12090 */
             crhold$ = "C"
               for c% = 1 to maxlines%
                 openqty(c%) = 0
               next c%
             goto datasave

        cancel_lines
L14595:      u3% = 2%
             call "ASKUSER" (u3%, " ** CANCEL LINES ** ",                ~
                             "Some Line Items have open BOLs or have " & ~
                             "been Shipped & Invoiced.",                 ~
                             "Press PF10 to set the Open Quantity to " & ~
                             "the BOL Quantities", "- or -  Press PF16"& ~
                             " to abort cancellation.")
             if u3% <> 10% and u3% <> 16% then L14595
             if u3% = 16% then edithdr1
               for c% = 1 to maxlines%
                 openqty(c%) = qtyschld(c%) + preinv(c%)
               next c%
             goto datasave

        previous_line_item
            orig_c% = c%
L14675:     if c% > 1% then L14680
               c% = orig_c%
               if screen% = 2% then goto editline2 else goto editline1
L14680:     c% = c% - 1%
            if str(lsts$(c%),2,1) = "D" then L14675  /*Try for Next Prev*/
            gosub describe_line
            if screen% = 2% then goto editline2 else goto editline1

        next_line_item
            orig_c% = c%
L14710:     if c% < maxlines% then L14720
               c% = orig_c%
               if screen% = 2% then goto editline2 else goto editline1
L14720:     c% = c% + 1%
            if str(lsts$(c%),2,1) = "D" then L14710  /* Try for Next */
            gosub describe_line
            if screen% = 2% then goto editline2 else goto editline1

        set_allocations
            if maxlines% = 0% then line_summary
                call "GETCODE" (#33, aset$, " ", 0%, 0.32, f1%(33%))
                if f1%(33%) = 0% then line_summary
                    for set% = 1% to maxlines%
                        alloc$(set%) = aset$
                    next set%
                    modfld% = 1% /* Changed allocation methods */
                    goto line_summary

        show_part_master_data        /* It's a call to HNYMSTSB is all */
            if part$(c%) = " " then return
            call "HNYMSTSB" (part$(c%), #02/* SYSFILE2 */,               ~
                #04/* HNYMASTR */, #11/* CATEGORY */, #03/* GLMAIN   */, ~
                #37/* HNYALTRS */, #34/* VENDOR   */, #35/* HNYPROC  */, ~
                #36/* VENPRICE */, #15/* HNYGENER */, #24/* PIPMASTR */, ~
                #20/* ENGMASTR */, #25/* PIPIN    */, #26/* PIPOUT   */, ~
                #13/* HNYQUAN  */, #28/* SFCUM2   */, #22/* TXTFILE  */, ~
                #08/* GENCODES */, #16/* BOMMASTR */)
            return

        mod_all_dates
            inputmsg$ = "To Reset ALL Line Item's Required Ship Date."
            k% = 0% : d% = 0%
            call "ASKDATE" (k%, "CHANGE ALL SHIP DATES!", inputmsg$,     ~
                     date, "20991231", newdate$, d%)
            if k% <> 0% then line_summary

            for z% = 1% to maxlines%
                shipdate$(z%) = newdate$
                call "DATEFMT" (shipdate$(z%))
            next z%
            goto line_summary

        REM *************************************************************~
            *          M I S C   S U P P O R T   R O U T I N E S        *~
            * --------------------------------------------------------- *~
            * Routines which apply to most of the screen functions.     *~
            *************************************************************

        call_customer_credit
            if cuscode$ = " " then return
            call "ARQCUSCR" (cuscode$)
            return


        deffn'040(txt$)
            fac25$ = hex(84) /* Highlight PF(25)Manage Text */
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then fac25$ = hex(8c) /* Dim PF(25)Manage Text */
            return

        edit_text_hdr
            call "TXTINSUB" (#22, f2%(22), "013", str(line2$,,60),       ~
                                                       textid$, text$())
            return

        edit_text_line
            call "TXTINSUB" (#22, f2%(22), "014", str(line2$,,60),       ~
                                                  textidl$(c%), text$())
            return

        check_if_pm_on      /* Check if Precious Metal Surcharge is on */
            pm_on$, pm_sys_so$, pm_sys_inv$ = "N"
            call "READ100" (#02, "SWITCHS.BCK", f1%(02%))
            if f1%(02%) = 1% then get #02 using L15152, pm_on$,           ~
                                                  pm_sys_so$, pm_sys_inv$
L15152:         FMT POS(60), 3*CH(1)
            pm_hdr_dsply$, pm_hdr_dsply2$ = " "
            if pm_on$ <> "Y" then return
                  pm_hdr_dsply$  = "PM Surcharge at SO?"
                  pm_hdr_dsply2$ = "PM Surcharge at INV?"
                  call "OPENCHCK" (#52, fs%(52%), f2%(52%), 0%, " ")

            return

        describe_line     /* Add descriptors to already existing line  */
            call "CONVERT" (order   (c%), 2.2, order$   )
            call "CONVERT" (openqty (c%), 2.2, openqty$ )
            call "CONVERT" (ship    (c%), 2.2, ship$    )
            call "CONVERT" (conv    (c%), 7.7, conv$    )
            call "CONVERT" (qtyschld(c%), 2.2, qtyschld$)
            call "CONVERT" (alloc   (c%), 2.2, allocqty$)
            call "CONVERT" (preinv  (c%), 2.2, preinv$  )
            call "DESCRIBE" (#11, cat$(c%), catdescr$, 0%, f1%(11))
            readkey$ = "UOM      " & stkuom$(c%)
            call "DESCRIBE" (#08, readkey$, stkuomdescr$, 0%, f1%(8))
            readkey$ = "UOM      " & priceuom$(c%)
            call "DESCRIBE" (#08, readkey$, priceuomdescr$, 0%, f1%(8))
            readkey$ = str(part$(c%)) & str(bom$(c%)) & "  0"
            call "DESCRIBE" (#16, readkey$, bomdescr$, 0%, f1%(16))
            call "GETCODE" (#03,salesacct$(c%),salesacctdescr$,0%,99,u3%)
            call "GETCODE" (#03,discacctl$(c%),discacctldescr$,0%,99,u3%)
            call "DESCRIBE" (#14, project$(c%), projectdescr$, 0%, u3%)
            gosub pricing_stuff
            gosub check_for_options
            call "DATUNFMT" (shipdate$(c%))
            call "DATE" addr("GD", str(shipdate$(c%),,6), weekday$, u3%)
            call "DATEFMT"  (shipdate$(c%))
            gosub describe_allocation
            gosub describe_demand
            preinvmsg$ = "Shipped - NOT Invcd  " & preinv$
            return

        describe_allocation
            allocdescr$ = " "
            if alloc$(c%) = "N" then allocdescr$ = "Don't Allocate"
            if alloc$(c%) = "A" then allocdescr$ = "Allocate to ATC"
            if alloc$(c%) = "P" then allocdescr$ = "Previous Alloc"
            if alloc$(c%) = "C" then allocdescr$ = "Allocate Complete"
            if alloc$(c%) = "Z" then allocdescr$ = "Zero Allocation"
            if (alloc$(c%) = "N" or alloc$(c%) = "P") and  alloc(c%) <> 0~
              then allocdescr$ = allocdescr$ & " (Qty=" & allocqty$ & ")"
            return

        pricing_stuff
*        Calculates dependent variables (for line C%) and formats
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

        clear_line   /* Clear variables for line C%                    */
            part$(c%), descr$(c%), item$(c%), cat$(c%), order$, openqty$,~
            stkuom$(c%), priceuom$(c%), linedisc$, taxable$(c%), optmsg$,~
            nonstockmsg$(c%), stkuomdescr$, priceuomdescr$, origdue$(c%),~
            pricestk$, discamt$, duedate$(c%), shipdate$(c%), alloc$(c%),~
            priority$(c%), demtype$(c%), salesacct$(c%), catdescr$, ext$,~
            salesacctdescr$, discacctl$(c%), discacctldescr$, preinvmsg$,~
            project$(c%), projectdescr$, conv$, weekday$, ship$, preinv$,~
            errormsg$, qtyschld$, lsts$(c%), lot$(c%), price$, bomdescr$,~
            allocqty$, seq$(c%), est$(c%), bom$(c%), allocdescr$,        ~
            demstatusmsg$, refpart$(c%), refdesc$(c%), mfgcode$,         ~
            mlquote_seq$(c%), shipcode$(c%), atc$(c%,1%), atc$(c%,2%),   ~
            parttype$(c%), est_cutover$(c%), est_rte$(c%), reftype$(c%)  ~
                                                                    = " "

            order(c%), openqty(c%), ship(c%), qtyschld(c%), price(c%),   ~
            pricestk(c%), linedisc(c%), alloc(c%), preinv(c%), ss,       ~
                                                         pm_base(c%)  = 0
            pm_adj%(c%)                                               = 0%
            conv(c%)                                                  = 1

            textidl$(c%) = all(hex(ff))

            mat opdlrs = con
            mat opdlrs = (-1) * opdlrs

            total_e_lines% = total_e_lines% - e_lines%(c%)
            e_lines%(c%), mlquote_cut%(c%)                           = 0%

            return

        options /* Call Options Processing Subroutine                  */
            if parttype$(c%) <> "000" then return
            if so$ <> " " then L16020
                 call "BCKNEXT" (#12, #06, #10, #30, #18, store$, so$)
L16020:     get #01 using L16030, opc$,   custype$
L16030:          FMT POS(734), CH(1), POS(1023), CH(2)
            if opc$ = " " then opc$ = pc$
            opdlrs(3) = price(c%)
            readkey$ = str(so$) & str(seq$(c%))
            call "REDALT0" (#18, readkey$, 1%, f1%(18))
            if f1%(18) = 1% then get #18 using L16090, bom$(c%)
L16090:         FMT POS(68), CH(3)

            call "BOMOPSUB"                                              ~
                    (clear%,             /* Clear B4 Input (0=N 1= Y)  */~
                     part$(c%),          /* Part Number                */~
                     str(so$) & str(seq$(c%)),     /* Demand Code      */~
                     shipdate$(c%),      /* Date Required              */~
                     bom$(c%),           /* BOM ID                     */~
                     cuscode$,           /* Customer Code              */~
                     custype$,           /* Customer Type              */~
                     cat$(c%),           /* Part Category              */~
                     opc$,               /* Price Code                 */~
                     orderdate$,         /* Order Date                 */~
                     opdlrs(),           /* Cost, Pricing Info         */~
                     #04,                /* 'HNYMASTR'                 */~
                     #16,                /* 'BOMMASTR'                 */~
                     #17,                /* 'BOMSPEC'                  */~
                     #02,                /* 'SYSFILE2'                 */~
                     #20,                /* 'ENGMASTR'                 */~
                     #38,                /* 'BOMSPHDR'                 */~
                     u3%)                /* 0 = ALL OK, 1 = ERROR      */
            clear% = 0%
            price(c%) = opdlrs(4)
            gosub pricing_stuff

            if u3% <> 0% then bom$(c%) = " "

            return

        check_for_options
            optmsg$ = " "
            readkey$ = all(hex(00))
            str(readkey$,,19) = str(so$) & seq$(c%)
            call "PLOWALTS" (#17, readkey$, 1%, 19%, f1%(17))
            if f1%(17) = 1% then optmsg$ = "BOM OPTIONS SPECIFIED!"
            return

        describe_demand
            demstatus$ = " "
            demstatusmsg$ = "     -No Demand-"
            readkey$ = str(so$) & str(seq$(c%))
            call "REDALT0" (#18, readkey$, 1%, f1%(18))
                if f1%(18) = 0% then return
            get #18 using L16470, demstatus$
L16470:         FMT CH(1)
            on pos(" 16789" = demstatus$) goto L16510, L16510, L16530,      ~
                                               L16530, L16550, L16550
                demstatusmsg$ =          "-Unknown Demand-" : goto L16570
L16510:         demstatusmsg$ =          "     -Unplanned-" : goto L16570
L16530:         demstatusmsg$ = hex(84) & " -Planned Late-" : goto L16570
L16550:         demstatusmsg$ = hex(84) & "   -Planned Ok-" : goto L16570
L16570:     return

        size_run   /* call the sub and then auto-generate line items */
            if dfltdue$ = " " or dfltdue$ = blankdate$ then ~
                    default_not_set_warning
            if dfltship$ = " " or dfltship$ = blankdate$ then ~
                    default_not_set_warning

            max% = maxlines%
            p% = 0% : e_warn% = 0%
            limit% = dim(part$(),1)  /* for customized systems */
            call "SZRINSUB" (1%, max%, passpart$(), passqty$(), default$,~
                             #02, #04, #13, #19, #18, #24, #25, #26, #27,~
                             #28, #29, limit%)
            if passpart$(1%) = " " then abort_size_run
            call "SHOSTAT" ("Creating Line Items...")
            modfld% = 1% /* Size Run added lines */
            for i% = 1% to 72%
               if passpart$(i%) = " " then L17760   /* all done */

               c%   = maxlines% + 1%
               gosub clear_line

               part$(c%) = passpart$(i%)
               order$    = passqty$(i%)
               convert order$ to order(c%), data goto L16980
L16980:        convert order$ to openqty(c%), data goto L17020

L17020:        call "READ100" (#04, part$(c%), f1%(4%))
                   if f1%(4%) = 0% then L17760

               for fieldnr% = 1% to 16%
               gosub L22000     /* Load up the 1st page defaults */
               next fieldnr%

               for fieldnr% = 1% to 8%
               gosub L23000     /* Load up the 2nd page defaults */
               next fieldnr%

               origdue$(c%) = dfltdue$
               str(lsts$(c%),2,1) = "A"
               if price(c%) <= 0 then gosub price_warning

               call"NUMTEST"(linedisc$,-50,100,errormsg$,2.2,linedisc(c%))
               if errormsg$ =  " " then L17420
                  linedisc$ = "0.00" : linedisc(c%) = 0
                  errormsg$ = " "

L17420:        if default$ = " " then L17620
               if str(default$,1,1) <> " " then                          ~
                                    alloc$(c%) = str(default$,1,1)
               if str(default$,2,1) <> " " then                          ~
                                    taxable$(c%) = str(default$,2,1)
               if str(default$,3,1) <> " " then                          ~
                                    demtype$(c%) = str(default$,3,1)
               if str(default$,4,1) <> " " then                          ~
                                    priority$(c%) = str(default$,4,1)

L17620:        maxlines% = maxlines% + 1%  /* Officially welcome line  */
               lastseq%  = lastseq%  + 1%  /* to the Sales Order       */
               convert lastseq% to seq$(c%), pic(###)
               call "ARIEXTRA" (cuscode$, part$(c%), " ",                ~
                                e_lines%(c%), #2)
               total_e_lines% = total_e_lines% + e_lines%(c%)
               if e_warn% <> 0% then L17680
               if maxlines% + total_e_lines% <  100% then L17680
                  gosub extra_load_conflict
                  e_warn% = 1%
L17680:        if maxlines% = 100% then L17760
               if lastseq% >= 999% then L17760
               next i%

L17760:        l% = max(0%, min(85%,maxlines%-12%))  /* Last Screen   */
               if maxlines% = 0% then edithdr1 else line_summary

        abort_size_run
            goto reenter_from_size_run  /* go back to top of input scr */

        price_warning
            price(c%) = 0
            if p% = 1% then return
            k% = 2%
            call "ASKUSER" (k%, "* * * WARNING * * *",                   ~
              "A price cannot be automatically determined for one or",   ~
              " more of the line items.  Please review all prices after",~
              " line items are created.  Press any key to acknowledge.")
            p% = 1%
            return

        default_not_set_warning
            call "ASKUSER" (k%, "* * * ABORT * * *",                     ~
              "The Default Ship Date and/or Default Due Date not set",   ~
              "Set them and then try this function again.",              ~
              "Press any key to acknowledge.")
            goto abort_size_run

        date_run
            convert seq$(c%) to cl%, data goto L18510
L18510:     call "DATRUNSB" (cuscode$, so$, cl%, c%, part$(c%-1%),       ~
                            orderdate$, dr_date$(), dr_qty$(),dr_ship$(),~
                            offsets%, #24, #04, #28, #19, #25, #26, #29, ~
                            #18, #27, ret%)
            if ret% = 32% then inputline              /* Abort Date Run */
            call "SHOSTAT" ("Creating Multiple Order Lines...")

            for i% = 1% to 36%
                if dr_date$(i%) = " " or dr_date$(i%) = blankdate$ then L18835

                part$(c%)        = part$(c%-1%)
                refpart$(c%)     = refpart$(c%-1%)
                refdesc$(c%)     = refdesc$(c%-1%)
                reftype$(c%)     = reftype$(c%-1%)
                nonstockmsg$(c%) = nonstockmsg$(c%-1%)
                descr$(c%)       = descr$(c%-1%)
                cat$(c%)         = cat$(c%-1%)
                duedate$(c%)     = dr_date$(i%)
                origdue$(c%)     = dr_date$(i%)
                shipdate$(c%)    = dr_ship$(i%)
                stkuom$(c%)      = stkuom$(c%-1%)
                convert dr_qty$(i%) to order(c%), data goto L18660
L18660:         alloc$(c%)       = alloc$(c%-1%)
                lsts$(c%)        = lsts$(c%-1%)
                convert dr_qty$(i%) to openqty(c%), data goto L18670
L18670:         lot$(c%)         = lot$(c%-1%)
                priceuom$(c%)    = priceuom$(c%-1%)
                conv(c%)         = conv(c%-1%)
                parttype$(c%)    = parttype$(c%-1%)
                pricestk(c%)     = pricestk(c%-1%)
                price(c%)        = price(c%-1%)
                linedisc(c%)     = linedisc(c%-1%)
                taxable$(c%)     = taxable$(c%-1%)
                item$(c%)        = item$(c%-1%)
                priority$(c%)    = priority$(c%-1%)
                demtype$(c%)     = demtype$(c%-1%)
                project$(c%)     = project$(c%-1%)
                salesacct$(c%)   = salesacct$(c%-1%)
                discacctl$(c%)   = discacctl$(c%-1%)
                bom$(c%)         = bom$(c%-1%)
                shipcode$(c%)    = shipcode$(c%-1%)
                pm_base(c%)      = price(c%)
                pm_adj%(c%)      = pm_adj%(c%-1%)
                convert cl% to seq$(c%), pic(##0)
                    gosub get_atc
                call "ARIEXTRA" (cuscode$, part$(c%), " ",               ~
                                                        e_lines%(c%), #2)
                total_e_lines% = total_e_lines% + e_lines%(c%)
                if c% + total_e_lines% <= 100% then L18785
                    gosub excess_lines

L18785:         if maxlines% = 100% then L18845
                    if lastseq% >= 999% then L18845
                maxlines% = maxlines% + 1%
                c% = c% + 1%  :  cl% = cl% + 1%
                lastseq% = lastseq% + 1%

L18835:     next i%

L18845:     l% = max(0%, min(85%,maxlines%-12%))       /* Last Screen   */
            goto line_summary

        excess_lines
L18870: % Current No. of Lines: ###  (+ Implied Lines: ###) : Total #####
            put askmsg$(1%) using L18870, c%, total_e_lines%,             ~
                                         c% + total_e_lines%
            u3% = 2%
            call "ASKUSER" (u3%, "* * * MAXIMUM LINES CONFLICT * * *",   ~
                  askmsg$(1%),                                           ~
                 "Some of the Implied Lines may not be generated",       ~
                 "Press any PF key to confirm and continue.")
            return

        adj_for_pm_surcharge        /* If PM effected part adjust the   */
                                    /* Price for PM Surcharge as Needed */
            if pm_on$ <> "Y" then return
            if pm_so$ <> "Y" then return
            if abs(pm_base(c%) - price(c%)) < 0.0001 then return

            s_charge = 0
            qty = openqty(c%)
            call "PMCALSUB" (cuscode$, part$(c%), qty,0,1,"C",orderdate$,~
                             s_charge, " ", " ", pm_so$, pm_inv$, rslt%)

            if rslt% = 0% then return  /*No PM Associated with this Part*/

            /* 1st Check if MLQ */
            if mlquote_cut%(c%) = 0% then L18962  /* Jump over MLQ Stuff */
                readkey$ = str(mlquote_seq$(c%)) & hex(20)
                call "READ100" (#52, readkey$, f1%(52%))
                    if f1%(52%) = 0%  then L18962
                get #52 using L18950, pm_code$, pm_base, pm_price
L18950:           FMT POS(20), CH(10), POS(55), PD(14,7), PD(14,7)
                if pm_code$ <> " " then L18962
                s_charge = qty * (pm_price - pm_base)
                pm_base(c%), price(c%) = pm_price
                pm_adj%(c%) = 1%
                return
L18962:     /* Add the precious metal surcharge to the price */
            if qty = 0 then  return  /* No PM set for zero qty */
                price(c%) = price(c%) + s_charge * conv(c%) / qty
                pm_adj%(c%) = 1%
                pm_base(c%) = price(c%)

                return

        display_pm_surcharges
            qty = openqty(c%)
            call "PMCALSUB" (cuscode$, part$(c%), qty,0,1,"D",orderdate$,~
                             s_charge, " ", " ", pm_so$, pm_inv$, rslt%)
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * 1- Sum up Order and see if it is on Credit Hold.          *~
            * 2- Get any change data required.                          *~
            * 3- Get final save approval.                               *~
            *************************************************************

        datasave
            shortcut% = 0%
            goto L19095
        datasave_shortcut
            if manadjre$ = "Y" and soonfile% = 1% then datasave
            shortcut% = 1%

L19095
*        FIRST add up the order and determine if it is on credit hold.
            mat torder  = zer
            mat topen   = zer
            mat tlines% = zer

            inpmessage$ = " "
            if crhold$ <> "C" then L19150
              adjmsg$ = "Cancellation Reason Code"
                goto L19160
L19150:       adjmsg$ = "Adjustment Reason Code"

L19160
*         Read in Bill-to (even if same as ship-to).
            get #01 using L19170, acctxref$, billto$, custype$
L19170:         FMT POS(771), 2*CH(9), POS(1023), CH(2)
            if billto$ = " " then L19190
L19180:     call "READ100" (#01, billto$, f1%(1))
            if f1%(1) <> 0% then L19230
L19190:         u3% = 2%
                call "ASKUSER" (u3%, "*** MISSING BILL-TO CUSTOMER ***", ~
                     "There is no Bill-to Customer (" &billto$& ") for:",~
                     "Ship-to Customer " &cuscode$& " (" & shipto$(1) &  ~
                     ")", "Press (RETURN) to acknowledge & continue")
            billto$ = str(cuscode$) /* No BillTo, BillTo Becomes ShipTo */
            goto L19180

L19230:     get #01 using L19235, billtoname$
L19235:         FMT XX(9), CH(30)

*        Call BCKCRDSB to compute Credit Limit, A/R & Open Order amounts.
*        It does this via the "Credit Parent" logic.
            par% = 0%     /* Not used in this subroutine */
            call "BCKCRDSB" (billto$, oo(), ar(), crlimit, par%)

*         Add up the order we are working on
            tqty = 0 /* Adding apples & oranges just to see if anything */
                     /* is open for shipping.  Used in credit hold.     */
            if maxlines% = 0% then L19410
             for c% = 1% to maxlines%
                if str(lsts$(c%),2,1) <> "D"                             ~
                                       then tlines%(1) = tlines%(1) + 1% ~
                                       else tlines%(2) = tlines%(2) + 1%
                if str(lsts$(c%),2,1)  = "D" then L19350
                ext       = round(order(c%) * price(c%) / conv(c%), 2)
                tqty      = tqty + order(c%)
                tdisc     = round(ext * linedisc(c%) *.01, 2)
                torder(1) = torder(1) + ext
                torder(2) = torder(2) - tdisc

                ext       = round(openqty(c%) * price(c%) / conv(c%), 2)
                tdisc     = round(ext * linedisc(c%) *.01, 2)
                topen (1) = topen (1) + ext
                topen (2) = topen (2) - tdisc
L19350:     next c%
            torder(4) = torder(1) + torder(2)
            torder(3) = round(torder(4) * orderdisc * .01, 2)
            torder(3) = 0 - torder(3)
            torder(4) = torder(4) + torder(3)

            topen (4) = topen (1) + topen (2)
            topen (3) = round(topen (4) * orderdisc * .01, 2)
            topen (3) = 0 - topen (3)
            topen (4) = topen (4) + topen (3)

            oo(1) = oo(1) - origopen + (topen(4) * conveqv)
L19410:     oo(2) = oo(2) - origopen + (topen(4) * conveqv)
            totalar1 = ar(1) + oo(1)
            if cr_ar_only$ <> "Y" then totalar2 = ar(2%) + oo(2%)        ~
                                  else totalar2 = ar(2%)

*        Credit Hold Options are: (Note: CRHOLD$ for Hold or Cancelled)
*          'N' - No Automatic Hold; CRHOLD$ & HOW_HELD$ stay the same
*          'Y' - Automatic Hold, no override if Held or Released Manually
*          'O' - Automatic Hold, override of Manual Setting

            crmsg$ = " "
            if crhold$ <> "C" then L19428
                how_held$ = " " : goto L19495
L19428:     if crflag$ <> "O" then L19432
L19430:         crhold$, how_held$ = " " : goto L19440
L19432:     if crflag$ = "N" then L19495    /* Flags stay 'as are'. */
            if how_held$ <> "M" then L19430 /*To get this far CRFLAG$='Y'*/
                if crhold$ = "H" then                                    ~
                    crmsg$ = "Order has been placed On Hold 'Manually'."
                goto L19495
L19440:     if tqty = 0 and topen(4) = 0 then L19495 /* Nothing there */
            if totalar2 > crlimit        then L19475 /* Over Cr Limit */

            call "BCKDAYSB" (billto$, u3%)
            if u3% = 0% then goto L19495

                crmsg$  = "A/R Invoice(s) exceed # of days late. Ord" &  ~
                          "er will be placed on hold." : goto L19485
L19475:         crmsg$  = "Credit Limit has been exceeded." &            ~
                          "  Order will be placed on hold."
L19485:         crhold$ = "H"       /* S. O. is on credit hold */

L19495
*        NOW, Display screen in input mode IF this is an old order.
            pickprint$ = " "
                if dfltpick$ = "Y" then pickprint$(2%) = "X"
                if dfltbol$  = "Y" then pickprint$(3%) = "X"
                if dfltacks$ <>"N" then pickprint$(1%) = "X"
                gosub encode_pickprint

            if crhold$ <> "H" and shortcut% = 1% then save_data
            if soonfile% = 0% or modfld% = 0% then L19590
            for fieldnr% = 1% to 1%
L19540:         gosub'106(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L19565
                         fieldnr% = max(1%, fieldnr% - 1%)
                         goto L19540
L19565:               if keyhit% <>  0 then       L19540
                gosub'156(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L19540
            next fieldnr%

L19590:     lastfieldnr% = 0%
L19595:     gosub'106(0%, 2%)           /* Display Screen - No Entry   */
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       exit_datasave
                  if keyhit%  =  9 then       exit_datasave
                  if keyhit%  = 16 then       save_data
                  if keyhit% <>  0 then       L19595
L19630:     fieldnr% = cursor%(1) - 4%
                if fieldnr% <  1% or  fieldnr%  > 2%  then L19590
                if fieldnr% =  1% and soonfile% = 0%  then L19590
                if fieldnr%  = lastfieldnr% then L19590
L19650:     gosub'106(fieldnr%, 2%)     /* Display & Accept Screen     */
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L19650
                gosub'156(fieldnr%)     /* Edit Field for Valid Entry */
                  if errormsg$ <> " " then L19650
                          lastfieldnr% = fieldnr%
                          goto L19630

        exit_datasave     /* Branch back to editing of order */
            call "READ100" (#01, cuscode$, f1%(1))
            if keyhit% = 2% then line_summary
            if keyhit% = 9% then edithdr1

        calc_net_for_kendra /* Coming from LI Sumry Scrn */
            mat topen = zer
            if maxlines% = 0% then return

            for i% = 1% to maxlines%
                if str(lsts$(i%),2%,1%)  = "D" then L19895
                ext       = round(openqty(i%) * price(i%) / conv(i%), 2)
                tdisc     = round(ext * linedisc(i%) *.01, 2)
                topen (1%) = topen (1%) + ext
                topen (2%) = topen (2%) - tdisc
L19895:     next i%

            topen (4) = topen (1) + topen (2)
            topen (3) = round(topen (4) * orderdisc * .01, 2)
            topen (3) = 0 - topen (3)
            topen (4) = topen (4) + topen (3)
            return

L20000: REM *************************************************************~
            *            D E F A U L T S   F O R   P A G E   1          *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for Screen 1 of Input.                      *~
            *************************************************************

*          DEFFN'051(FIELDNR%)
                  on fieldnr% goto  L20190,         /* Customer Code    */~
                                         ,         /* Sales Order #    */~
                                         ,         /* Purchase Order # */~
                                    L20272,         /* Export Order flag*/~
                                         ,         /* Ship-to          */~
                                         ,         /* Sold-to          */~
                                    L20340,         /* Store Code       */~
                                    L20370,         /* Order Date       */~
                                         ,         /* Cancellation Date*/~
                                    L20440,         /* Default Due Date */~
                                    L20470          /* Default Ship Date*/
                     return

L20190
*        Customer Code                         CUSCODE$
            return

*        Sales Order Number                    SO$
            return

*        Purchase Order Number                 PO$
                return

L20272
*        Export Order flag                     EXPORT$
            get #01 using L20274, export$
L20274:         FMT POS(1091), CH(1)
            if export$ = " " then export$ = "N"
            return

*        Ship-to                               SHIPTO$
                return

*        Sold-to                               SOLDTO$
                return

L20340
*        Store Code                            STORE$
                if defstore$ = " " then defstore$ = dfltstr$
                store$ = defstore$
                return

L20370
*        Order Date                            ORDERDATE$
                orderdate$ = date$
                return

*        Cancellation Date                     CANCELDATE$
                return

L20440
*        Default Due Date                      DFLTDUE$
                if offset_due$ = " " then return
                     tempdate$ = orderdate$
                     call "DATUNFMT" (tempdate$)
                     call "DATE" addr ("G+", tempdate$, offsetd%,        ~
                                             dfltdue$, u3%)
                     call "DATEFMT" (dfltdue$)
                     return

L20470
*        Default Req'd Ship Date               DFLTSHIP$
                if offset_shp$ = " " or ~
                      dfltdue$ = " " or dfltdue$ = blankdate$ then return
                     tempdate$ = dfltdue$
                     call "DATUNFMT" (tempdate$)
                     call "DATE" addr ("G+", tempdate$, -offsets%,       ~
                                             dfltship$, u3%)
                     call "DATE" addr ("GD", str(dfltship$,,6),          ~
                                             weekdayh$, u3%)
                     call "DATEFMT" (dfltship$)
                     return

L21000: REM *************************************************************~
            *            D E F A U L T S   F O R   P A G E   2          *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for second header screen.                   *~
            *************************************************************

*          DEFFN'052(FIELDNR%)
                  enabled% = 1%
                  on fieldnr% goto  L21210,         /* How Ship         */~
                                    L21260,         /* FOB              */~
                                    L21310,         /* Ship Instruct    */~
                                    L21360,         /* Price Code Deflt */~
                                    L21410,         /* Order Discount % */~
                                    L21470,         /* Payment Terms    */~
                                    L21520,         /* Region Code      */~
                                    L21570,         /* Salesman / Split */~
                                    L21790,         /* PM Surchrge SO Fl*/~
                                    L21840,         /* PM Surchrge INVFl*/~
                                    L21690,         /* Sales Acct Deflt */~
                                    L21740          /* Sales Discs Acct */
                     return

L21210
*        How Ship                              HOWSHIP$
            get #01 using L21230, howship$
L21230:         FMT XX(562), CH(20)
            return

L21260
*        FOB                                   FOB$
            get #01 using L21280, fob$
L21280:         FMT XX(582), CH(20)
            return

L21310
*        Shipping Instructions                 SHIPINSTR$(2)
            get #01 using L21330, shipinstr$()
L21330:         FMT XX(613), 2*CH(50)
            return

L21360
*        Price Code Default                    PC$
            get #01 using L21380, pc$
L21380:         FMT XX(524), CH(1)
            return

L21410
*        Order Discount Percent                ORDERDISC$
            get #01 using L21430, orderdisc
L21430:         FMT XX(516), PD(14,4)
            call "CONVERT" (orderdisc, 2.2, orderdisc$)
            return

L21470
*        Payment Terms (Code)                  TERMS$
            get #01 using L21490, terms$
L21490:         FMT XX(542), CH(20)
            return

L21520
*        Region Code                           REGION$
            get #01 using L21540, region$
L21540:         FMT XX(728), CH(4)
            return

L21570
*        Salesman Code / Split %               SALESMAN$(3)
            get #01 using L21590, salesman$(), comm%()
L21590:         FMT XX(713), 3*CH(4), 3*BI(1)
            for i% = 1% to 3%
                if salesman$(i%) <> " " then L21650
                     comm%(i%) = 0%
                     comm$(i%) = " "
                     goto L21660
L21650:         convert comm%(i%) to comm$(i%), pic(##0)
L21660:     next i%
            return

L21690
*        Sales Account Default                 DFLTSALES$
            call "ARMGLGET" (1%, cuscode$, " ", " ", " ", store$, " ",   ~
                             #02, #01, #04, #11, #03, dfltsales$)
            return

L21740
*        Sales Discounts Account               DISCACCT$
            call "ARMGLGET" (2%, cuscode$, " ", " ", " ", store$, " ",   ~
                             #02, #01, #04, #11, #03, discacct$)
            return

L21790
*        Precious Metal Surcharge SO Flag           PM_SO$
            if pm_on$ = "Y" then L21810
                enabled% = 0%
                return
L21810:     if pm_so$ <> " " then return
                if pm_cus_so$ = " " then pm_so$ = pm_sys_so$             ~
                                    else pm_so$ = pm_cus_so$
            return

L21840
*        Precious Metal Surcharge INV Flag           PM_INV$
            if pm_on$ = "Y" then return
                enabled% = 0%
                return
            if pm_inv$ <> " " then return
                if pm_cus_inv$ = " " then pm_inv$ = pm_sys_inv$          ~
                                     else pm_inv$ = pm_cus_inv$
            return

L22000: REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

*          DEFFN'053(FIELDNR%)
                 on fieldnr%  goto       ,         /* Estimate Number  */~
                                         ,         /* Part Code        */~
                                    L22150,         /* Part Description */~
                                    L22180,         /* Part Category    */~
                                    L22210,         /* Due Date         */~
                                    L22230,         /* Required Ship    */~
                                    L22250,         /* Stocking UOM     */~
                                    L22280,         /* Order Qty/Alloc  */~
                                    L22300,         /* Open Order / Lot */~
                                         ,         /* Total Qty Shipped*/~
                                    L22320,         /* Currency code    */~
                                    L22335,         /* Pricing UOM      */~
                                    L22365,         /* Conversion       */~
                                    L22445,         /* Unit Price       */~
                                         ,         /* Line Item Disc   */~
                                    L22545          /* Line Taxable?    */
                     return

*        Estimate Number                       EST$()
            return

*        Part Code                             PART$()
            return

L22150
*        Part Description                      DESCR$()
            if nonstockmsg$(c%) <> " " then return
                get #04 using L22165, descr$(c%)
L22165:              FMT XX(25), CH(32)
                return

L22180
*        Part Category                         CAT$()
            if nonstockmsg$(c%) <> " " then return
                get #04 using L22195, cat$(c%)
L22195:              FMT XX(89), CH(4)
                return

L22210
*        Due Date                              DUEDATE$()
            duedate$(c%) = dfltdue$
            return

L22230
*        Required Ship Date                    SHIPDATE$()
            shipdate$(c%) = dfltship$
            return

L22250
*        Stocking Unit of Measure (Non-stock parts only) STKUOM$
            if nonstockmsg$(c%) <> " " then return
                get #04 using L22265, stkuom$(c%)
L22265:              FMT XX(73), CH(4)
                return

L22280
*        Original Order Quantity / Allocate    ORDER$  / ALLOC$()
            if nonstockmsg$(c%) <> " " then L22286
            get #04 using L22284, alloc$(c%)
L22284:         FMT POS(242), CH(1)
L22286:     if  alloc$(c%) = " " then alloc$(c%) = allocdflt$
            return

L22300
*        Open Order Quantity / Lot Number      OPENQTY$ / LOT$()
            openqty$ = order$
            return

*        Total Quantity Shipped                SHIP$
            return

L22320
*        Currency code                         CURRENCY$
            return

L22335
*        Pricing Unit of Measure               PRICEUOM$()
            if nonstockmsg$(c%) <> " " then return
                get #04 using L22350, priceuom$(c%)
L22350:              FMT XX(77), CH(4)
                return

L22365
*        Conversion Pricing -> Stocking        CONV$
            if nonstockmsg$(c%) <> " " then L22395
                get #04 using L22380, conv(c%)
L22380:              FMT POS(82), PD(14,7)
                call "CONVERT" (conv(c%), 7.7, conv$)
                return
L22395:     if stkuom$(c%) = priceuom$(c%) then conv$ = "1"
            if stkuom$(c%) = priceuom$(c%) then return
                readkey$ = "UOMCONV  " & str(priceuom$(c%)) & "-" &      ~
                                         str(stkuom$  (c%))
                call "READ100" (#08, readkey$, f1%(8))
                if f1%(8) = 0% then return
                     get #08 using L22430, conv$
L22430:                   FMT XX(24), CH(10)
                     return

L22445
*        Unit Price                            PRICE$
            if parttype$(c%) = "000" then return  /* got via BOMOPSUB */
            if est$(c%) = " " then L22485
                tempdisc = edsc(1%)
                tempprice = eprc(1%)
*              LINEDISC(C%) = EDSC(EPTR%)
*              PRICE(C%) = 0
                for ii% = 1% to 7%
                     if eqty(ii%) <= 0 then L22465
                     if eprc(ii%) <= 0 then L22465
                     if order(c%) < eqty(ii%) then L22465
                     tempprice = eprc(ii%)
                     tempdisc  = edsc(ii%)
                next ii%
L22465:         linedisc(c%) = tempdisc
                if linedisc(c%) >= 100 then L22515
                tempprice = 100 * (tempprice/(100-linedisc(c%)))
                price(c%) = tempprice * conv(c%)
                goto L22515
L22485:     get #01 using L22490, custype$
L22490:         FMT POS(1023), CH(2)
            call "CPRASSGN" (cuscode$, custype$, part$(c%), cat$(c%),    ~
                pc$, orderdate$, currtype$, currency$, opdlrs(2),        ~
                order(c%), #02, #04, price(c%), linedisc(c%), errormsg$)
            if errormsg$ <> " " then return
L22515:         gosub pricing_stuff
                return

*        Line Item Discount %                  LINEDISC$
            return

L22545
*        Line Taxable? (Y/N)                   TAXABLE$()
            get #01 using L22555, custaxable$
L22555:         FMT XX(793), CH(1)
            parttaxable$ = " "
            if nonstockmsg$(c%) =" " then get #04 using L22570,parttaxable$
L22570:         FMT XX(126), CH(1)
            if parttaxable$ = "N" then taxable$(c%) = "N"
            if parttaxable$ = "Y" then taxable$(c%) = "Y"
            if parttaxable$ = " " then taxable$(c%) = custaxable$
            return

L23000: REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  4  of Input. *~
            *************************************************************

*         DEFFN'054(FIELDNR%)
            on fieldnr%    goto          ,         /* PO Item Number   */~
                                    L23190,         /* Planning Priority*/~
                                    L23270,         /* Demand Type      */~
                                         ,         /* Project Number   */~
                                    L23340,         /* Sales Distr. Acct*/~
                                    L23450,         /* Sales Discs Acct */~
                                    L23530,         /* Specific BOM id  */~
                                    L23550          /* Shipping Priority*/
            return

*        PO Item                               ITEM$()
            return

L23190
*        Planning Priority Code                PRIORITY$()
            if nonstockmsg$(c%) <> " " then L23230
                get #04 using L23220, priority$(c%)
L23220:              FMT XX(333), CH(1)
L23230:         if priority$(c%) < "A" or priority$(c%) > "Z" then       ~
                                                      priority$(c%) = "A"
                return

L23270
*        Planning Demand Type                  DEMTYPE$()
            if nonstockmsg$(c%) <> " " then L23276
            get #04 using L23274, demtype$(c%)
L23274:         FMT POS(243), CH(1)
L23276:     if demtype$(c%) = " " then demtype$(c%) = dflt_dem_type$
            return

*        Project Number                        PROJECT$()
            return

L23340
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

L23450
*        Sales Discounts Account               DISCACCTL$()
            if nonstockmsg$(c%) <> " " then L23500
                call "ARMGLGET" (2%, cuscode$, part$(c%), cat$(c%), " ", ~
                                 store$, " ", #02, #01, #04, #11, #03,   ~
                                 discacctl$(c%))
L23500:     if discacctl$(c%) = " " then discacctl$(c%) = discacct$
            return

L23530
*        Specific Bom ID                       BOM$()
            return

L23550
*        Shipping Priority Code                SHIPCODE$()
            if shipcode$(c%) <> " " then return
                get #01 using L23580, shipcode$
L23580:             FMT POS(733), CH(1)
            if shipcode$  = " " then shipcode$ = "3"
            shipcode$(c%) = shipcode$
            return

        REM *************************************************************~
            * Routines related to Multi-Line Quotation Cutover.         *~
            *************************************************************

        cutover_quote_from_top  /* Entire Sales Order from a Quotation */
            if appendquote% = 0%                                         ~
                then msg$ = hex(06) & "Select Quotation for Cutover to "&~
                     "Sales Order."                                      ~
                else msg$ = hex(06) & "Select Quotation to Append Line "&~
                     "Items from."
            mat incl = zer : mat descr = zer
            init (" ") incl$(), plowhdr$()
*          U3% = 2%                           /* Window at the bottom */
*          CALL "ASKUSER" (U3%, "*** QUOTE SELECTION CRITERION ***",    ~
*              "Press PF(4) to select Quote by Quote Number.", "Press "&~
*              "PF(8) to select Quote by Customer Code.", "Press PF(1)"&~
*              " to abort Quotation Cutover.")
*          IF U3% =  1% THEN RETURN                  /* Abort Cutover */
*          IF U3% =  4% THEN GOTO 24350 /* Select by Quotation Number */
*          IF U3% <> 8% THEN GOTO 24120   /* Select by Customer Code? */
*              PLOWHDR$(1%) = "  Customer  Name                       "&~
*                   "    Quote #  Description"
*              IF CUSCODE$ = "?" THEN CUSCODE$ = " "
*              READKEY$ = CUSCODE$
*              DESCR( 1%) =    1.09  : DESCR( 2%) =  1
*              DESCR( 3%) =  -10.30  : DESCR( 4%) = 11
*              DESCR( 5%) =   10.08  : DESCR( 6%) = 42
*              DESCR( 7%) = 1001.30  : DESCR( 8%) = 51
*              CALL "PLOWCODE" (#46, READKEY$, MSG$, 9000%, 1.3,        ~
*                   F1%(46%), PLOWHDR$(), 0, 1001, INCL(), INCL$(),     ~
*                   "d", " ", #01, DESCR())               /* MLQMASTR */
*              IF F1%(46%) <> 0% THEN GOTO 24500/* Quotation selected */
*                   ERRORMSG$ = HEX(00)          /* No selection made */
*                   RETURN

*        Select a Quotation by Quotation Number.
            quotenbr$ = " "                 /* No other value possible */
            plowhdr$(1%) = "  Quote #  Description                     "&~
                "Quote      Due     Open Amt  Expires"
            descr( 1%) =   10.08  : descr( 2%) =  1
            descr( 3%) = 1001.30  : descr( 4%) = 10
            descr( 5%) =  806.061 : descr( 6%) = 41
            descr( 7%) =  818.061 : descr( 8%) = 50
            descr( 9%) =  867.08  : descr(10%) = 59.1042
            descr(11%) = 1055.061 : descr(12%) = 70
            readkey$ = cuscode$
            call "PLOWCODE" (#46, readkey$, msg$, 9009%, 1.3, f1%(46%),  ~
                plowhdr$(), 0, 1001, incl(), incl$(), "d", " ", #46,     ~
                descr())                                   /* MLQMASTR */
            if f1%(46%) <> 0% then goto L24500    /* Quotation Selected */
                errormsg$ = hex(00)               /* No Selection Made */
                return

L24500
*        A Quotation has been selected- grab it & set up a Sales Order.
            mc% = 46% : lc% = 47% : cc% = 48%   /* MLQ file channel #s */
            mlq_line_date% = 0%
            get #46 using L24512, temp$, quotenbr$, curr2$, /* MLQMASTR */~
                                                           expdate$
L24512:         FMT CH(9), CH(8), POS(893), CH(4), POS(1055), CH(6)
            if expdate$ = " " or expdate$ = blankdate$ then L24531
            testdate$ = orderdate$
            if testdate$ <> " " and testdate$ <> blankdate$ then L24518
                testdate$ = date
                goto L24519
L24518:     call "DATUNFMT" (testdate$)
L24519:     if expdate$ >= testdate$ then L24531
            call "DATEFMT" (expdate$)
L24521:     u3% = 2%
            call "ASKUSER" (u3%, "*** QUOTE EXPIRATION WARNING ***",     ~
                "Quote Number " & quotenbr$ & " has Expired on " &       ~
                expdate$ & ".", "Press PF(8) to Continue With the Cut" & ~
                "over.", "Press PF(1) to Abort the Quotation Cutover.")
            if u3% = 1% then return
            if u3% = 8% then L24531
                goto L24521

L24531:     if curr2$ = " " then curr2$ = statutory$            /* JIC */
            if appendquote% = 0% then goto L24550
*        Test for and prohibit the mixing of different currencies.
                if curr2$ = currency$ then goto append_quote_lines /*OK*/
                     call "ASKUSER" (0%, "*** CAN'T MIX CURRENCIES ***", ~
                          "The Sales Order is in " & currency$ & " curr"&~
                          "ency.", "The Quote lines you wish to append "&~
                          "are in " & curr2$ & ".", "Press any PF key t"&~
                          "o acknowledge.")
                     return
L24550:     cuscode$ = temp$
            gosub get_header_fields

*        Now read & store the Quotation Line Items for the Sales Order.
            c%, maxlines% = 0%
        append_quote_lines
            plowkey3$ = quotenbr$ & hex(00)
L24640:     call "PLOWNEXT" (#47, plowkey3$, 8%, f1%(47%)) /* MLQLINES */
            if f1%(47%) = 0% then goto cutover_quote_exit     /* Done? */
            if maxlines% = dim(part$(),1%) then cutover_quote_exit/* ? */
                c%, maxlines% = maxlines% + 1%  /* No - Clr & Str Line */
                lastseq% = lastseq% + 1%
                gosub clear_line
                readkey$ = key(#47)
                gosub get_line_item_fields
                convert lastseq% to seq$(c%), pic(##0)
                lsts$(c%) = " A "
                temp$ = " "
                get #47 using L24720, str(temp$,1,3)
L24720:              FMT POS(18), CH(3)   /* The 'real' Quote LI Seq # */
                gosub cutover_options
                gosub check_for_options
                gosub cutover_xref_parts
                put mlquote_seq$(c%) using L24740, quotenbr$, temp$
L24740:              FMT CH(8), CH(3)
                mlquote_cut%(c%) = 1%/* Indicate 'Cutover this session'*/
                goto L24640

        cutover_quote_exit   /* Get out of here from here only, please */
            if mlq_line_date% = 0% then L24783
                u3% = 0%
                call "ASKUSER" (u3%, "*** QUOTE LINE DATE WARNING ***",  ~
                      "For at Least One Line Cutover From the Quote, " & ~
                      "Either or Both the Line 'Order", "Date' and/or "& ~
                      "'Required Ship Date' Were Defaulted to the "    & ~
                      "Header Dates.", "Press Any PF Key to Continue...")

L24783:     gosub adjust_currency_conversion
            if cuscode$ <> " " then goto L24880
                errormsg$ = "You must select a valid Customer code."
                fieldnr% = 1%             /* Gotta get a Customer Code */
                gosub'050(1%, fieldnr%, 2%)      /* Input msg, enables */
L24820:         gosub'101(fieldnr%, 2%)     /* Display & Accept Screen */
                gosub call_screen
                if keyhit%  = 1% then gosub startover
                if keyhit% <> 0% then L24820
                gosub L50000              /* Edit Field for Valid Entry */
                if errormsg$ <> " " then L24820
L24880:     call "DESCRIBE" (#01, cuscode$, cusdescr$, 0%, f1%(1%))
            if f1%(1%) <> 0% then goto L24920    /* Customer not found? */
                cusdescr$ = "Customer not on file"             /* Nope */
                goto edithdr1
L24920:     get #01 using L24930, poreqd$                   /* CUSTOMER */
L24930:         FMT POS(1020), CH(1)
            if po$ <> " " or poreqd$ <> "Y" then goto edithdr1
                errormsg$ = "PO is required for this Customer."
                fieldnr% = 3%                             /* PO.N.mber */
                gosub'050(1%, fieldnr%, 2%)      /* Input msg, enables */
L24980:         gosub'101(fieldnr%, 2%)     /* Display & Accept Screen */
                gosub call_screen
                if keyhit%  = 1% then gosub startover
                if keyhit% <> 0% then L24980
                gosub L50000              /* Edit Field for Valid Entry */
                if errormsg$ <> " " then goto L24980
                goto edithdr1

        cutover_options  /* copy from MLQSPEC to BOMSPEC */
            init(hex(00)) plowkey2$
            str(plowkey2$,1,8 ) = str(quotenbr$,,)
            str(plowkey2$,9,8 ) = hex(20)
            str(plowkey2$,17,3 ) = str(temp$,1,3)
L25110:     call "PLOWALTS" (#49, plowkey2$, 1%,19%,f1%(49%)) /*MLQSPEC */
            if f1%(49%) = 0% then goto do_mlqsphdr
            get #49, using L25140, record$
L25140:        FMT CH(150)
            if so$ <> " " then L25150
                 call "BCKNEXT" (#12, #06, #10, #30, #18, store$, so$)
L25150:     str(record$, 57, 16) =  str(so$,1,16)
            str(record$, 73, 3) =  str(seq$(c%),1,3) /* new line seq */
            write #17, using L25140, record$
            goto L25110

        do_mlqsphdr      /* copy from MLQSPHDR to BOMSPHDR */
            init(hex(00)) plowkey2$
            str(plowkey2$,1,8 ) = str(quotenbr$,,)
            str(plowkey2$,9,8 ) = hex(20)
            str(plowkey2$,17,3 ) = str(temp$,1,3)
            call "REDALT0" (#50, plowkey2$, 1%, f1%(50%)) /*MLQSPHDR*/
            if f1%(50%) = 0% then return
            get #50, using L25280, record$
L25280:        FMT CH(150)
            str(record$, 35, 19) =  str(so$,,) & str(seq$(c%),,)
            write #38, using L25140, record$
            return

        cutover_xref_parts
            call "PTUSEDSB" ("R", "MLQ ", quotenbr$, temp$,              ~
                          refpart$(c%), refdesc$(c%), reftype$(c%), ret%)
            if ret% <> 0% then L25400
                refdesc$(c%), reftype$(c%) = " "
                refpart$(c%) = "** No Cross Reference **"
                return
L25400:     refflag% = 1%
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
            scr%(1, 2) =  2% : set%( 2) = 13%      /* Sales Order #    */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* PO Number        */
            scr%(1, 4) = 43% : set%(43) =  2%      /* Export Order flag*/
            scr%(1, 5) =  4% : set%( 4) =  2%      /* Ship-to          */
            scr%(1, 6) =  5% : set%( 5) =  2%      /* Sold-to          */
            scr%(1, 7) =  6% : set%( 6) =  2%      /* Store Code       */
            scr%(1, 8) =  7% : set%( 7) =  2%      /* Order Date       */
            scr%(1, 9) =  8% : set%( 8) =  2%      /* Cancellation Dte */
            scr%(1,10) =  9% : set%( 9) =  2%      /* Default Due Date */
            scr%(1,11) = 10% : set%(10) =  2%      /* Dflt Ship Date   */

            scr%(2, 1) = 11% : set%(11) =  2%      /* How Ship         */
            scr%(2, 2) = 12% : set%(12) =  2%      /* FOB              */
            scr%(2, 3) = 13% : set%(13) =  2%      /* Shipping Instr   */
            scr%(2, 4) = 14% : set%(14) =  2%      /* Price Code       */
            scr%(2, 5) = 15% : set%(15) =  2%      /* Order Discount % */
            scr%(2, 6) = 16% : set%(16) =  2%      /* Terms Code       */
            scr%(2, 7) = 17% : set%(17) =  2%      /* Region           */
            scr%(2, 8) = 18% : set%(18) =  2%      /* Salesmen / Split */
            scr%(2%, 9%) = 46% : set%(46%) =  2%   /* PM Schrge SO flg */
            scr%(2%,10%) = 47% : set%(47%) =  2%   /* PM Schrge INVflg */
            scr%(2,11) = 19% : set%(19) =  2%      /* Sales Account    */
            scr%(2,12) = 20% : set%(20) =  2%      /* Sales Disc Acct  */

            scr%(3, 1) = 41% : set%(41) =  2%      /* Estimate Number  */
            scr%(3, 2) = 21% : set%(21) =  2%      /* Part Code        */
            scr%(3, 3) = 22% : set%(22) =  2%      /* Part Descr       */
            scr%(3, 4) = 23% : set%(23) =  2%      /* Part Category    */
            scr%(3, 5) = 24% : set%(24) =  2%      /* Due Date         */
            scr%(3, 6) = 25% : set%(25) =  2%      /* Required Ship    */
            scr%(3, 7) = 26% : set%(26) =  2%      /* Stocking UOM     */
            scr%(3, 8) = 27% : set%(27) =  2%      /* Order Qty        */
            scr%(3, 9) = 28% : set%(28) =  2%      /* Open Order Qty   */
            scr%(3,10) = 29% : set%(29) =  0%      /* Qty Shipped      */
            scr%(3,11) = 44% : set%(44) =  2%      /* Currency code    */
            scr%(3,12) = 30% : set%(30) =  2%      /* Pricing UOM      */
            scr%(3,13) = 31% : set%(31) =  2%      /* Conversion Fctr  */
            scr%(3,14) = 32% : set%(32) =  2%      /* Unit Price       */
            scr%(3,15) = 33% : set%(33) =  2%      /* Line Item Disc%  */
            scr%(3,16) = 34% : set%(34) =  2%      /* Line Taxable?    */

            scr%(4, 1) = 35% : set%(35) =  2%      /* PO Item Number   */
            scr%(4, 2) = 36% : set%(36) =  2%      /* Priority         */
            scr%(4, 3) = 37% : set%(37) =  2%      /* Demand Type      */
            scr%(4, 4) = 38% : set%(38) =  2%      /* Project Number   */
            scr%(4, 5) = 39% : set%(39) =  2%      /* Sales Account    */
            scr%(4, 6) = 40% : set%(40) =  2%      /* Sales Disc Acct  */
            scr%(4, 7) = 42% : set%(42) =  2%      /* BOM Id.          */
            scr%(4, 8) = 45% : set%(45) =  2%      /* Shipping Priority*/
        REM Next slot available is '48'

            if pm_on$ = "Y" then L27550
                  set%(46%) = 99%          /* PM SO Flag       */
                  set%(47%) = 99%          /* PM INV Flag      */

L27550:     call "ENABLSUB" ("INIT", "BCKINPUT", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************

        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "BCKINPUT", scr%(), set%(),       ~
                              s%, f%, 0%, 0%)
            return

        REM *************************************************************~
            *        I N P U T   M E S S A G E S  &  E N A B L E S      *~
            * --------------------------------------------------------- *~
            * Sets Enable Flag, Input Message, and Standard PF Keys.    *~
            *************************************************************

        deffn'050(s%, f%, edit%)  /* EDIT%: 1=Input Mode; 2=Edit Mode */
            if f% <> 0% then L28050
                inpmessage$ = edtmessage$
                goto L28170

L28050
*        First Define the Input Message
            r% = scr%(s%, f%)  /* Get sequential field number          */
            restore line = L28230, r%     /* Position for Read          */
            read inpmessage$             /* Read Input Message         */

*        Now set the Field Enable Flag
            call "ENABLSUB" ("SET", "BCKINPUT", scr%(), set%(), s%, f%,  ~
                             edit%, enabled%)
            if c% = 0% then L28170
            if s% = 1% and f% = 4% then export$ = "N" /* Default flag */
            if s% = 3% and (f% = 7% or f% = 12% or f% = 13%) and         ~
               nonstockmsg$(c%) = " " and edit% = 1% then enabled% =  0%
            if est$(c%) = " " or edit% <> 1% then L28116
                if s% = 3% and f% > 1% and f% < 8% then enabled% = 0%
                if s% = 3% and f% > 8% and f% < 13% then enabled% = 0%
                if s% = 4% then enabled% =  0%
L28116:     if s% = 3% and f% = 11% and (c% <> 1% or maxlines% > 1% or   ~
                edit% = 2% or curr$ <> "Y") then enabled% =  0%
            if s% = 4% and f% = 7% then L28121 else L28175
L28121:         if parttype$(c%) <> "000" then L28125
                   enabled% = 0%
                   goto L28175
L28125:         if bom$(c%) <> " " then L28175
                readkey$ = part$(c%) : boms% = 0
L28135:         call "PLOWNEXT" (#16, readkey$, 25%, f1%(16))
                     if f1%(16) = 0 then L28160
                boms% = boms% + 1 : if boms% > 1% then L28175
                str(readkey$,29) = all(hex(ff))
                goto L28135
L28160:         if boms% < 2% then enabled% = 0

L28170
*        And, finally, set up LINE2$ for the display.
L28175:     line2$ = "Customer: " & cuscode$
            str(line2$,22) = "Sales Order: " & so$
            if so$ = " " and (s% > 1% or f% > 2% or edit% = 2%) then     ~
                       str(line2$,22) = "Sales Order: Will be Assigned"
            if s% <> 3% and s% <> 4% then L28215
                str(line2$,52) = "Line: " & seq$(c%)
*              IF SEQ$(C%) = " " THEN                                   ~
*                   CONVERT LASTSEQ% + 1% TO STR(LINE2$,58,3), PIC(000)
L28215:     str(line2$,62) = "BCKINPUT: " & str(cms2v$,,8)

                /* Override default info msg if previous field's blank */
            if s% = 1% and f% = 11% and ~
                    (dfltdue$ = " " or dfltdue$ = blankdate$) then      ~
                inpmessage$ = "Enter Default Shipping Date"

            return

L28230: data                                                             ~
        /* Screen 1                                                    */~
         "Enter Customer Code and/or Sales Order Number.",               ~
         "Enter Sales Order Number ('?' to scan existing Orders).",      ~
         "Enter Purchase Order Number.",                                 ~
         "Enter Ship-to Name and Address.",                              ~
         "Enter Sold-to, 'BILL-TO', or leave blank if same as ship-to.", ~
         "Enter Store that will ship merchandise.",                      ~
         "Enter Order Date.",                                            ~
         "Enter Order Cancellation Date.",                               ~
         "Enter Default Due Date.",                                      ~
         "Enter Date -or- '-' and number of days before due date.",      ~
                                                                         ~
        /* Screen 2                                                    */~
         "Enter How Ship Instructions.",                                 ~
         "Enter FOB Description.",                                       ~
         "Enter Shipping Instructions.",                                 ~
         "Enter Pricing Code for this Order.",                           ~
         "Enter Order Level Discount Percentage.",                       ~
         "Enter Payment Terms for this Order.",                          ~
         "Enter Region to be credited with this order.",                 ~
         "Enter Salesmen and Commission Split %'s for this order.",      ~
         "Enter default Sales Account.",                                 ~
         "Enter Sales Discounts Account.",                               ~
                                                                         ~
        /* Screen 3                                                    */~
         "Enter Part Code for this Line Item.",                          ~
         "Enter Part Description for this Line Item.",                   ~
         "Enter Part Category Code.",                                    ~
         "Enter Current Due Date for this line item.",                   ~
         "Enter Required Ship Date -or- '-' and days before Due Date.",  ~
         "Enter Stocking Unit of Measure",                               ~
         "Enter Original Order Quantity and Allocate Flag (N/A/C/Z)",    ~
         "Enter Open Order Quantity (and Lot Number, if applicable).",   ~
         "Enter Total Quantity Shipped to date.",                        ~
         "Enter Pricing Unit of Measure.",                               ~
         "Enter Pricing UOM to Stocking UOM Conversion Factor.",         ~
         "Enter Unit Price.",                                            ~
         "Enter Discount Percent for this Line Item.",                   ~
         "Is this line item Taxable? (Y/N)",                             ~
                                                                         ~
        /* Screen 4                                                    */~
         "Enter P.O. Line Number Reference.",                            ~
         "Enter Planning Priority Code.",                                ~
         "Enter Planning Demand Type Code (1=Nettable, 2=Non-Nettable).",~
         "Enter Project Number (if applicable).",                        ~
         "Enter Sales Distribution Account.",                            ~
         "Enter Sales Discount Account.",                                ~
                                                                         ~
        /* Screen 3 (add on)                                           */~
        "Enter Estimate Id., blank if line is not linked to an estimate",~
                                                                         ~
        /* Screen 4 (add on)                                           */~
         "Entering a Bom Id. will over-ride the effective BOM.",         ~
                                                                         ~
        /* Screen 1 (add on)                                           */~
         "Enter 'Y' if this is an EXPORT Order; 'N' if Domestic.",       ~
                                                                         ~
        /* Screen 3 (add on)                                           */~
         "Enter Transaction Currency Code for this Document. Blank = stat~
        ~utory.",                                                         ~
                                                                         ~
        /* Screen 4 (add on)                                           */~
         "Enter the Shipping Priority Code for this line.",              ~
                                                                         ~
        /* Added to Hdr Screen 2                                       */~
         "Are Precious Metal Surcharges to be added at Ordering(Y/N)?"  ,~
         "Are Precious Metal Surcharges to Adjusted at Invoicing(Y/N)?"

        REM *************************************************************~
            *        I N I T I A L I Z E   F O R   I N P U T            *~
            * --------------------------------------------------------- *~
            * Clear variables prior to input mode.                      *~
            *************************************************************

        init_for_input
            call "ALLFREE"
            lastso$ = so$
            init(" ") dr_date$(), dr_qty$(), dfltacks$, pickprint$(),    ~
            errormsg$, inpmessage$, cuscode$, so$, po$, est$(), dfltdue$,~
            shipto$(), soldto$(), canceldate$, pc$, dfltship$, howship$, ~
            fob$, shipinstr$(), qtyschld$, orderdisc$, terms$, region$,  ~
            salesman$(), comm$(), dfltsales$, discacct$, part$(), vf$,   ~
            descr$(), seq$(), cat$(), order$, openqty$, ship$, lastuser$,~
            priceuom$(), conv$, price$, linedisc$, taxable$(), cusdescr$,~
            duedate$(), shipdate$(), priority$(), demtype$(), project$(),~
            salesacct$(), discacctl$(), lot$(), storedescr$, origdue$(), ~
            lsts$(), nonstockmsg$(), stkuom$(), pricestk$, regiondescr$, ~
            lastchanged$, discacctldescr$, projectdescr$, allocdescr$,   ~
            salesacctdescr$, projectdescr$, weekday$, optmsg$, adjrsn$,  ~
            dfltsalesdescr$, weekdayh$, alloc$(), item$(), adjrsndescr$, ~
            orderdate$, termsdescr$, pcdescr$, discacctdescr$, origdate$,~
            salesmandescr$(), ext$, origuser$, bom$(), export$, crmsg$,  ~
            currency$, currdesc$, convdate$, store$, crhold$, atc$(),    ~
            how_held$, refpart$(), refdesc$(), mfgcode$, mlquote_seq$(), ~
            quotesave$, shipcode$(),shipcode$, est_cutover$(),est_rte$(),~
            cutover_bom$, reftype$(), keepcus$
            allow_delete$ = allow_delete_save$

            nextbol%       = 1%
            maxlines%, c%, lastseq%, l%, soonfile%, copy%, modfld%   = 0%
            drop%, oldflag%, appendquote%                            = 0%
            origopen = 0 : conveqv, convunt = 1
            mat comm%    = zer
            mat order    = zer
            mat openqty  = zer
            mat ship     = zer
            mat price    = zer
            mat pricestk = zer
            mat preinv   = zer
            mat qtyschld = zer
            mat e_lines% = zer
            mat mlquote_cut% = zer
            total_e_lines% = 0%

            init (hex(ff)) textid$, textidl$()
            call "TXTFUTIL" (#22, f2%(22), "INTL", textid$)
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants.   *~
            *************************************************************

        startover
L29912:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                if u3% <> 0% then L29912

L29922
*        Wants to Start Over
            return clear all
        startover2   /* Entry point for Start Over in VF subrtn    */
            readkey$ = all(hex(00))       /* Clear In-process Flag */
            str(readkey$,,3) = userid$
            call "DELETE" (#09, readkey$, 10%)
            gosub clear_all_options
            goto inputmode

        restart_line
L29940:     u3% = 2%
            call "ASKUSER" (u3%, "RESTART LINE",                         ~
                            "Press (ENTER) to RESTART Line Item",        ~
                            "- OR -", "Press PF-1 to EXIT Restart.")
            if u3% = 1% then return
                if u3% <> 0% then L29940
                     return clear
                     gosub clear_line_options
                     gosub clear_line_text
                     gosub clear_line
                     goto inputline

        clear_line_text    /* Clear work area for possible text        */
            call "TXTFUTIL" (#22, f2%(22), "XOUT", textidl$(c%))
            return

        clear_all_options  /* Clear options that will become orphans   */
            c9% = max(maxlines%, c%)     /* get last one */
            if c9% = 0% then return
                c8% = 1%
                str(lsts$(c%),2,1) = "A"
                goto clear_options
        clear_line_options /* Clear any options entered for line C%    */
                c8%, c9% = c%  :  str(lsts$(c%),2,1) = "A"
        clear_options
            if so$ = " " then return
            for c% = c8% to c9%
                if str(lsts$(c%),2,1) <> "A" then L29994
                     init(hex(00)) readkey$
                     str(readkey$,,19) = str(so$) & str(seq$(c%))
                     call "REDALT1"(#38, readkey$, 1%, f1%(38))
                         if f1%(38) = 0% then L29988
                         delete #38
L29988:              call "PLOWAL1" (#17, readkey$, 1%, 19%, f1%(17))
                     if f1%(17) = 0% then L29994
                          delete #17
                          goto L29988
L29994:     next c%
            return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            * Note that only the SO# is used for accessing the SO- the  *~
            * Customer is then checked for a match (if he was entered). *~
            * A dummy header is written to the buffer file to show that *~
            * the order is currently being processed.                   *~
            *************************************************************

        load_data
            soonfile% = 0%

*        First see if the order in question is in the master file
            readkey$  = str(so$) & hex(00)
            call "PLOWNEXT" (#06, readkey$, 16%, soonfile%)
            if soonfile% = 0% then allow_delete$ = "Y"
            if soonfile% = 0% then L30125
                get #06, tempcus$
                if cuscode$ = " " then cuscode$ = tempcus$
                if cuscode$ = tempcus$ then L30125
                     errormsg$ = "Order already assigned to Customer " & ~
                                 tempcus$
                     return

L30125
*        See if order is already in buffer.  If so can't touch
            call "REDALT0" (#09, so$, 2%, f1%(9))
            if f1%(9) = 0% then L30150
L30140:         errormsg$ = "Order is already being processed."
                return
L30150:     if cuscode$  = " " then return
            if soonfile% = 0%  then call "BCKPREFX" (so$, errormsg$)
            if errormsg$ <> " " then return

*        See if order number is already used as a demand code
            if soonfile% <> 0% then L30215
            readkey$ = all(hex(00))
            str(readkey$,1,16) = str(so$)
            call "PLOWALTS" (#18, readkey$, 1%, 16%, f1%(18))
            if f1%(18) = 0% then L30215
              errormsg$ = "Order Number is already used as a Demand Code"
              return

L30215
*        All Ok-fine.  Flag Order as in-process.
            readkey$ = " "
            str(readkey$, 1, 3) = userid$
            str(readkey$, 4, 7) = all(hex(00))
            str(readkey$,11, 8) = "BCKINPUT"
            str(readkey$,21,25) = str(cuscode$) & so$
            write #09 using L30255, readkey$, " ", " ", " ", " ",         ~
                                  eod goto L30140
L30255:         FMT CH(45), 3*CH(250), CH(225)

*        Now load from master (if there) and format data as required.
            if soonfile% = 0% then return
            print at(04,02), hex(94)&"Loading Sales Order from Master..."~
                & hex(8c)
              oldflag% = 1%
            call "READ100" (#01, cuscode$, f1%(1))  /* May have changed */
         copy_order  /* Entry point for copy function */
            mc% = 5% : lc% = 6% : cc% = 41%        /* Current channels */
            if copy% <> 1% then L30320
            if copy_what% <> 31% then L30320
                mc% = 31% : lc% = 30% : cc% = 43%  /* History channels */
L30320:     readkey$ = str(cuscode$) & so$
            call "READ100" (#mc%, readkey$, f1%(mc%))
            gosub get_header_fields

*        Now Read in line items from BCKLINES
            c%, maxlines% = 0%
            currkey$, readkey$ = str(so$) & hex(00)
L30355:     call "PLOWNEXT" (#lc%, readkey$, 16%, f1%(lc%))
            if f1%(lc%) = 1% then L30395
                if copy% = 1% and crhold$ = "C" then gosub copy_cancelled
                convert seq$(maxlines%) to temp%
                lastseq% = max(lastseq%, temp%)
                if maxlines% + total_e_lines% <= 100% then L30390
                   gosub extra_load_conflict
L30390:         return
L30395:     c%, maxlines% = c% + 1%
            gosub get_line_item_fields
            goto L30355

        copy_cancelled
L30415:     u3% = 2%
            call "ASKUSER" (u3%, "*** ATTENTION ***",                    ~
                           "This is a Cancelled Order!", "PF3 will copy"&~
                           " & restore the original quantities in the "& ~
                           "new order.", "PF16 will 'STARTOVER' without"&~
                           " copy.")
            if u3% <> 3% and u3% <> 16% then L30415
            if u3% = 3% then L30465
                return clear all
                goto startover2
L30465:     crhold$ = " "
            for c% = 1 to maxlines%
                openqty(c%) = order(c%)
            next c%
            return

        get_header_fields       /* MC% must be set before calling this */
            if mc% = 46% then hold% = lastseq%
            get #mc% using L35060, po$, shipto$(), soldto$(), terms$,     ~
                howship$, fob$, shipinstr$(), dfltsales$, discacct$,     ~
                salesman$(), comm%(), region$, vf$, textid$, store$,     ~
                orderdate$, canceldate$, dfltdue$, dfltship$,            ~
                origdate$, origuser$, lastchanged$, lastuser$, export$,  ~
                pc$, orderdisc, origopen, crhold$, lastseq%, nextbol%,   ~
                currency$, how_held$, pm_so$, pm_inv$
            if mc% = 46% then lastseq% = hold%
            if currency$ = " " then currency$ = statutory$
            gosub set_pm_flags
            if copy% = 1%                                                ~
                then call "TXTFUTIL" (#22, f2%(22%), "COPY", textid$)    ~
                else call "TXTFUTIL" (#22, f2%(22%), "LOAD", textid$)
            origopen = origopen - round((origopen * orderdisc * .01), 2)
            if copy% = 1% then origopen = 0
            call "DESCRIBE" (#21, terms$, termsdescr$, 0%, f1%(21%))
            call "DESCRIBE" (#03, dfltsales$, dfltsalesdescr$, 0%,       ~
                f1%(3%))
            call "GLFMT" (dfltsales$)
            call "DESCRIBE" (#03, discacct$, discacctdescr$, 0%, f1%(3%))
            call "DESCRIBE" (#40, currency$, currdesc$, 0%, f1%(40%))
            call "GLFMT" (discacct$)
            for i% = 1% to 3%
                if salesman$(i%) = " " then L30625
                     call "DESCRIBE" (#07, salesman$(i%),                ~
                          salesmandescr$(i%), 0%, f1%(7%))
                     convert comm%(i%) to comm$(i%), pic(##0)
L30625:     next i%
            readkey$ = "REGIONS  " & region$
            call "DESCRIBE" (#08, readkey$, regiondescr$, 0%, f1%(8%))
            readkey$ = "PRICECODE" & pc$
            call "DESCRIBE" (#08, readkey$, pcdescr$, 0%, f1%(8%))
            call "DESCRIBE" (#12, store$, storedescr$, 0%, f1%(12%))
            if mc% = 46% then orderdate$ = date/* Use today for Quotes */
            call "DATEFMT" (orderdate$)
            call "DATEFMT" (canceldate$)
            call "DATEFMT" (dfltdue$)
            if dfltship$ <> " " and dfltship$ <> blankdate$ then         ~
                call "DATE" addr("GD", str(dfltship$,,6%), weekday$, u3%)
            call "DATEFMT" (dfltship$)
            call "DATEFMT" (origdate$)
            call "DATEFMT" (lastchanged$)
            call "CONVERT" (orderdisc, 2.2, orderdisc$)
            if mc% = 46% then gosub mlq_header_date_check
            return

        get_line_item_fields             /* Pre-set LC%, CC%, READKEY$ */
            get #lc% using L35830, tempcus$, tempso$,                     ~
                seq$(c%), item$(c%), part$(c%), descr$(c%),              ~
                cat$(c%), order(c%), ship(c%), openqty(c%),              ~
                qtyschld(c%), alloc(c%), preinv(c%), pricestk(c%),       ~
                stkuom$(c%), priceuom$(c%), conv(c%), price(c%),         ~
                linedisc(c%), taxable$(c%), salesacct$(c%),              ~
                discacctl$(c%), origdue$(c%), duedate$(c%),              ~
                shipdate$(c%), lot$(c%), project$(c%), temp$,            ~
                demtype$(c%), priority$(c%), textidl$(c%), alloc$(c%),   ~
                temp$, est$(c%), bom$(c%), mlquote_seq$(c%),             ~
                shipcode$(c%), pm_adj%(c%)
                pm_base(c%) = price(c%)
            if est$(c%) = " " then L30775
                call "READ100" (#23, est$(c%), f1%(23%))
                     if f1%(23%) <> 1% then L30775
                get #23 using L30772, est_cutover$(c%), est_rte$(c%)
L30772:              FMT POS(1888), CH(1), POS(1892), CH(3)

L30775: REM Corresponding Reference Part & Description...
            /* Test if Copy and Copy for same Customer */
            if keepcus$ = " " or keepcus$ = cuscode$ then L30781 else L30800
            /* Get Cross reference part from shadow file */
L30781:     scode$ = "BCK "
            if lc% = 30% then scode$ = "BCKH"
            if lc% = 47% then goto L30800
            call "PTUSEDSB" ("R", scode$, tempso$, seq$(c%),             ~
                          refpart$(c%), refdesc$(c%), reftype$(c%), ret%)

            if ret% = 1% then refflag% = 1% else                         ~
                refpart$(c%) = "** No Cross Reference **"
*          IF RET% = 1% THEN REFTYPE$(C%) = "C"

L30800: REM Get transaction amounts from the LI 'shadow' file (xxxLNCUR)
            if currency$ = statutory$ then goto L30850 /* Not Stat, tho */
            if curr$ <> "Y" then goto L30850 /* Nor if no multi-currency*/
            call "READ100" (#cc%, readkey$, f1%(cc%))
            if f1%(cc%) = 0% then goto L30850
            get #cc% using L30830, pricestk(c%), price(c%)
L30830:         FMT POS(24), 2*PD(14,4)
            pm_base(c%) = price(c%)
                if c% = 1% then get #cc% using L30845, currency$,         ~
                    convdate$, conveqv, convunt
L30845:             FMT CH(4), POS(40), CH(6), 2*PD(14,7)
L30850:     if copy% = 1% then                                           ~
                    call "TXTFUTIL" (#22, f2%(22%), "COPY", textidl$(c%))~
                else                                                     ~
                    call "TXTFUTIL" (#22, f2%(22%), "LOAD", textidl$(c%))
            call "DATEFMT" (origdue$ (c%))
            call "DATEFMT" (duedate$ (c%))
            call "DATEFMT" (shipdate$(c%))
            call "GLFMT"  (salesacct$(c%))
            call "GLFMT"  (discacctl$(c%))
            if qtyschld(c%) <> 0 then lsts$(c%) = "s  "
            if preinv  (c%) <> 0 then str(lsts$(c%),3%) = "p"
            call "READ100" (#04, part$(c%), f1%(4%))
            if f1%(4) <> 0% then L30912
                nonstockmsg$(c%) = "Non-Stock Part"
                refpart$(c%) = "** No Cross Reference **"
                goto L30915
L30912:     get #4, using L30913, parttype$(c%)
L30913:        FMT POS(180), CH(3)
L30915:     if lc% = 47% then gosub mlq_lines_date_check
            gosub get_atc
            call "ARIEXTRA" (cuscode$, part$(c%), " ", e_lines%(c%), #02)
            total_e_lines% = total_e_lines% + e_lines%(c%)
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * (1) Get next Sales Order Number if required.              *~
            * (2) Update buffers with the order.                        *~
            * (3) Update Customer Master with open order dollars        *~
            * (4) Invoke update task.                                   *~
            *************************************************************

        save_data
            curtemp3 = 0
            curtemp4 = 0
*        First, assign Sales Order Number if so required.
            if so$ = " " then                                            ~
               call "BCKNEXT" (#12, #06, #10, #30, #18, store$, so$)
            print at (04,02), hex(94)&"Saving Sales Order: "&so$&hex(8c)

*        Now throw the order into the buffer files (lines 1st; then hdr).
*        But first, get rid of the currency-specific line items.
            if curr$ <> "Y" then goto L31230
                currkey$ = str(so$) & hex(00)
                call "DELETE" (#41, currkey$, 16%)

L31230:     if maxlines% = 0% then L31630
              for c% = 1% to maxlines%
                if str(lsts$(c%),2,1) <> "D" then L31310
                     call "TXTFUTIL" (#22, f2%(22), "XOUT", textidl$(c%))
                     saveseq$ = seq$(c%)  :  savepart$ = part$(c%)
                     gosub clear_line
                     seq$(c%) = saveseq$  :  part$(c%) = savepart$
                     descr$(c%) = hex(ff)   /* Flag as deleted         */
L31310:         call "DATUNFMT" (origdue$ (c%))
                call "DATUNFMT" (duedate$ (c%))
                call "DATUNFMT" (shipdate$(c%))
                call "GLUNFMT"  (salesacct$(c%))
                call "GLUNFMT"  (discacctl$(c%))

                if curr$ <> "Y" then goto L31440
                if currency$ = statutory$ then goto L31440
                if descr$(c%) = hex(ff) then goto L31440
                write #41 using L36800, currency$, so$, seq$(c%),         ~
                     pricestk(c%), price(c%), convdate$, conveqv,        ~
                     convunt, " "

L31440:         curtemp1 = round((pricestk(c%) * conveqv), 4)
                curtemp2 = round((price   (c%) * conveqv), 4)
                dummy    = round((openqty(c%) * curtemp2 / conv(c%)), 2)
                curtemp3 = curtemp3 + (dummy -                           ~
                           round((dummy*(linedisc(c%)/100)), 2))
                if alloc$(c%) = "N" then alloc(c%) = 0

                write #10 using L35830, cuscode$, so$, seq$(c%),          ~
                     item$(c%), part$(c%), descr$(c%), cat$(c%),         ~
                     order(c%), ship(c%), openqty(c%), qtyschld(c%),     ~
                     alloc(c%), preinv(c%), curtemp1, stkuom$(c%),       ~
                     priceuom$(c%), conv(c%), curtemp2, linedisc(c%),    ~
                     taxable$(c%), salesacct$(c%), discacctl$(c%),       ~
                     origdue$(c%), duedate$(c%), shipdate$(c%), lot$(c%),~
                     project$(c%), " ", demtype$(c%), priority$(c%),     ~
                     textidl$(c%), alloc$(c%), " ", est$(c%), bom$(c%),  ~
                     mlquote_seq$(c%), shipcode$(c%), pm_adj%(c%), " "

              if xref%    = 0% then L31610  /* Must have a Xref File */
              if refpart$(c%) = "** No Cross Reference **" then L31610
              if reftype$(c%) = " " then L31610
                  /* Add Cross reference part to shadow file */
                  call "PTUSEDSB" ("W", "BCK ", so$, seq$(c%),           ~
                                   refpart$(c%), refdesc$(c%),           ~
                                   reftype$(c%), ret%)

L31610:       next c%

L31630:     call "GLUNFMT"  (dfltsales$)
            call "GLUNFMT"  (discacct$ )
            call "DATUNFMT" (orderdate$ )
            call "DATUNFMT" (canceldate$)
            call "DATUNFMT" (dfltdue$   )
            call "DATUNFMT" (dfltship$  )
            call "DATUNFMT" (origdate$  )
            if origdate$ = " " or origdate$ = blankdate$ then origdate$ = date
            if origuser$ = " " then origuser$ = userid$
            readkey$ = all(hex(00))       /* Clear In-process Flag */
            str(readkey$,,3) = userid$
            call "DELETE" (#09, readkey$, 10%)
            call "GETDTTM" addr(datetime$)

            topen(1) = topen(1) + topen(2)  /* Sum of line exts        */
            curtemp4 = round(curtemp3 * orderdisc * .01, 2)
            curtemp4 = curtemp3 - curtemp4

            put #09 using L35410, userid$, datetime$, pickprint$, " ",    ~
                cuscode$, so$, po$, shipto$(), soldto$(), terms$,        ~
                howship$, fob$, shipinstr$(), dfltsales$, discacct$,     ~
                salesman$(), comm%(), region$, vf$, textid$, store$,     ~
                orderdate$, canceldate$, dfltdue$, dfltship$,            ~
                origdate$, origuser$, date, userid$, adjrsn$, export$,   ~
                pc$, orderdisc, curtemp3, crhold$, lastseq%, nextbol%,   ~
                custype$, acctxref$, currency$, how_held$,               ~
                pm_so$, pm_inv$, " "
            if soonfile% = 0% then put #09 using L31900, " "
L31900:         FMT POS(859), CH(9)  /* Blank Change Audits       */
            write #9
            call "TXTFUTIL" (#22, f2%(22), "SAV2", textid$)

*        NOW adjust Open Order Dollars in Customer Master (CCRMASTR)
          if abs(curtemp4 - origopen) < .01 then L32142
            call "READ100" (#01, cuscode$, f1%(1%))
            get #01 using L31970, billto$
L31970:         FMT POS(780), CH(9)
            topen(1%), topen(2%) = 0                            /* JIC */
            call "READ101" (#51, cuscode$, f1%(51%))       /* CCRMASTR */
            if f1%(51%) <> 0% then get #51 using L31976, topen(1), topen(2)
L31976:         FMT POS(114), 2*PD(14,4)
            if billto$ = cuscode$ then                                   ~
                topen(1) = topen(1) - origopen + curtemp4
                topen(2) = topen(2) - origopen + curtemp4
            if f1%(51%) = 0% then put #51 using L37110, cuscode$, 0, 0, 0,~
                " ", 0, " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0,  ~
                0, 0, 0, " ", " ", " "   /* Create new CCRMASTR record */
            put #51 using L32020, date, topen(1%), topen(2%), userid$, date
L32020:         FMT POS(84), CH(6), POS(114), 2*PD(14,4), POS(146),      ~
                     CH(3), CH(6)
            if f1%(51%) = 0% then write #51 else rewrite #51

            if billto$ = cuscode$ then L32142
L32060:         call "READ100" (#01, billto$, f1%(1%))
                if f1%(1%) = 1% then L32100
                   billto$ = str(cuscode$)
                   goto L32060
L32100:         topen(1%), topen(2%) = 0                        /* JIC */
                call "READ101" (#51, billto$, f1%(51%))    /* CCRMASTR */
                if f1%(51%) <> 0% then get #51 using L31976, topen(1%),   ~
                     topen(2%)
                topen(1%) = topen(1%) - origopen + curtemp4
                if f1%(51%) = 0% then put #51 using L37110, billto$, 0,   ~
                     0, 0, " ", 0, " ", 0, " ", 0, 0%, " ", " ", " ",    ~
                     " ", " ", 0, 0, 0, 0, " ", " ", " " /* New CCRMASTR*/
                put #51 using L32020, date, topen(1%), topen(2%), userid$,~
                     date
                if f1%(51%) = 0% then write #51 else rewrite #51

L32142
*        Write to Precious Metal Shadow File if needed
            if maxlines% = 0% then L32160
            for c% = 1% to maxlines%
                if pm_adj%(c%) <> 1% then L32155
                call "PMCALSUB" (cuscode$, part$(c%), openqty(c%),       ~
                                 price(c%), conv(c%), "S", orderdate$,   ~
                                 s_charge, "S", str(so$) & seq$(c%),     ~
                                 pm_so$, pm_inv$, rslt%)
L32155:     next c%

L32160
*        FINALLY, Delete Quotes per criteria; Crank up the update.
            if mlqdelt$ = "N" then goto L32184 /* System behavior sw=NO */
                for c% = 1% to maxlines%
                     gosub quote_deletion
                next c%
L32184:     u3% = 0%
            call "TASKUP" ("SO", u3%)
            goto inputmode

        set_pm_flags
            if pm_on$ = "Y" then L33925
                pm_so$, pm_inv$  = " "
                return
L33925:     if pm_so$ <> " " then L33945
                if pm_cus_so$  = " " then pm_so$  = pm_sys_so$           ~
                                 else pm_so$  = pm_cus_so$
L33945:     if pm_inv$ <> " " then return
                if pm_cus_inv$ = " " then pm_inv$ = pm_sys_inv$          ~
                                 else pm_inv$ = pm_cus_inv$
            return

        call_screen
            call "GETSCRN" ("C", " ", cursor%(), 0%)
            return

        pf1315
            if keyhit% <> 13% then L34100
                call "MANUAL" ("BCKINPUT")
                keyhit% = 15%
                return
L34100:     if keyhit% <> 15% then return
                call "PRNTSCRN"
                return

        quote_deletion/* Maybe delete some Quotes from that sub-module */
            if mlquote_seq$(c%) = " " then return/* Any Quote cutover? */
            if mlquote_cut%(c%) = 0%  then return /* C/O this session? */
            if str(mlquote_seq$(c%),,8%) = quotesave$ then return
            quotesave$ = str(mlquote_seq$(c%),,8%)/* Consider each once*/
            if mlqdelt$ = "Y" then goto L34270/* System behavior sw=YES */
*        MLQDELT$ must be 'A'- Ask the user whether to delete the Quote.
L34200:         u3% = 0%                       /* Window in the middle */
                call "ASKUSER" (u3%, "*** DELETE THIS QUOTE? ***",       ~
                     "Quote # " & quotesave$ & " was cutover to this S."&~
                     "O. DELETE the Quote?", "Press PF(12) to DELETE th"&~
                     "e Quote.", "Press (RETURN) to RETAIN the Quote.")
                if u3% =   0% then return
                if u3% <> 12% then goto L34200
L34270
*        Well, here we go, deleting the Quote from MLQMASTR/LINES/LNCUR.
            call "DELETE" (#46, quotesave$, 8%)   /* Bye-bye, MLQMASTR */
            call "DELETE" (#47, quotesave$, 8%)   /* Bye-bye, MLQLINES */
            if curr$ = "Y"                       /* Bye-bye, MLQLNCUR? */~
                then call "DELETE" (#48, quotesave$, 8%)
            if pm_adj%(c%) <> 1% then L34290
                call "PMCALSUB" (cuscode$, part$(c%), openqty(c%),       ~
                          price(c%), conv(c%), "R", " ", s_charge, "M",  ~
                          quotesave$, " ", " ", rslt%) /* Bye PM Shadow*/
L34290:     /* Del Xref Shadow */
            call "PTUSEDSB" ("D", "MLQ ", quotesave$, "ALL",             ~
                              " ", " ", " ", ret%)

*        And now clean out option files, starting with MLQSPEC
            init(hex(00)) plowkey2$
            str(plowkey2$,1,8 ) = str(quotesave$)

L34298:     call "PLOWAL1" (#49, plowkey2$, 1%, 8%,f1%(49%)) /*MLQSPEC */
            if f1%(49%) = 0% then return
            delete #49

*        And followed by MLQSPHDR
            init(hex(00)) readkey$
            str(readkey$,1,8 ) = str(mlquote_seq$(c%),,8%)
            str(readkey$,9,8 ) = hex(20)
            str(readkey$,17,3 ) = str(plowkey2$,17%,3%)

            call "REDALT1" (#50, plowkey2$, 1%, f1%(50%)) /*MLQSPHDR*/
            if f1%(50%) = 0% then L34298
            delete #50
            goto L34298

        REM *************************************************************~
            * Extra Lines Conflict Warnings/Errors                      *~
            *************************************************************
        extra_load_conflict

L34370: % Current No. of Lines: ###  (+ Implied Lines: ###) : Total #####

            put askmsg$(1%) using L34370, maxlines%, total_e_lines%,      ~
                                         maxlines% + total_e_lines%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press any PF key to confirm and continue"
            u3% = 2%
            call "ASKUSER" (u3%, "* * * MAXIMUM LINES CONFLICT * * *",   ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            return

        extra_append_conflict
            put askmsg$(1%) using L34370, c%, total_e_lines% + temp%,     ~
                                         c% + total_e_lines% + temp%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press PF16 to continue, Press RETURN to" &    ~
                          " re-enter Part Code"
L34540:     u3% = 2%
            call "ASKUSER" (u3%, "* * * APPEND LINES CONFLICT * * *",    ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            if u3% = 16% then return
            if u3% =  0% then return
               goto L34540

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE #05 -- BCKMASTR  (Read Only)       */~
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
            POS(893), CH(4),/* Currency Code                           */~
            CH(1),          /* How put on hold                         */~
            CH(1),          /* PM Surcharge SO Flag                    */~
            CH(1)           /* PM Surcharge INV Flag                   */

L35410: FMT                 /* FILE #09 -- BCKBUFFR                    */~
            CH(3),          /* Current User                            */~
            CH(7),          /* Date/Time Stamp                         */~
            CH(1),          /* Pick List Print Flag                    */~
            CH(9),          /* Filler                                  */~
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
            CH(4),          /* Currency Code                           */~
            CH(1),          /* How put on hold                         */~
            CH(1),          /* PM Surcharge SO Flag                    */~
            CH(1),          /* PM Surcharge INV Flag                   */~
            CH(101)         /* Filler                                  */~

L35830: FMT                 /* FILEs #06 & #10 -- BCKBUF2 and BCKLINES */~
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
            CH(3),          /* Specific Bom Id.                        */~
            CH(11),         /* Multi-Line Quote & Seq # derived from   */~
            CH(1),          /* Shipping Priority Code                  */~
            BI(1),          /* Precious Metal Surcharge Added Flag     */~
            CH(22)          /* Filler                                  */

L36190: FMT                 /* FILE #23 -- ESTMASTR                    */~
            XX(9),          /* Customer Code                           */~
            XX(8),          /* Estimate Id. Number                     */~
            XX(30),         /* Customer Name                           */~
            CH(25),         /* Estimate Part Number                    */~
            XX(20),         /* buyer's name                            */~
            XX(10),         /* phone number                            */~
            XX(180),        /* Ship To Name and Address                */~
            XX(20),         /* Payment Terms Code                      */~
            XX(4),          /* region code                             */~
            XX(9),          /* Shipping Region Code                    */~
            XX(100),        /* Shipping Instructions                   */~
            XX(16),         /* Purchase Order Number                   */~
            CH(08),         /* Code/# of a Project (i.e. Contract, Job,*/~
            XX(12),         /* salesman code                           */~
            XX(3),          /* Percentage of Sale credited to salesman.*/~
            XX(25),         /* Part Number                             */~
            CH(32),         /* Part description                        */~
            CH(4),          /* category code                           */~
            XX(3),          /* Type of Part in HNYMASTR file           */~
            CH(4),          /* Stocking UOM                            */~
            XX(8),          /* minimum order qty. / minimum order multi*/~
            XX(25),         /* Part Number                             */~
            XX(3),          /* The specific BOM identifier for a Bill o*/~
            XX(25),         /* Part Number                             */~
            XX(3),          /* The specific routing to use for a Bill. */~
            XX(52),         /* Any free text information               */~
            XX(96),         /* Per Part Costs Followed By Per Run      */~
            XX(24),         /* Standard Cost                           */~
            XX(8),          /* Standard run quantity for standard costi*/~
            7*PD(14,4),     /* Quantity of Something                   */~
            XX(56),         /* More Material Costs                     */~
            XX(56),         /* Outside Processing Costs                */~
            XX(56),         /* Total Material & Outside Processing Csts*/~
            XX(28),         /* Handling Fee (%, mat & outproc only)    */~
            XX(56),         /* More Lab & Ovhd Costs                   */~
            XX(56),         /* Total Labor & Overhead Costs            */~
            XX(28),         /* Markup Percentage (lab & Ovhd only)     */~
            XX(56),         /* Total Cost Each                         */~
            XX(28),         /* Commission Percent Rate                 */~
            7*PD(7,3),      /* Discount Percent                        */~
            7*PD(14,4),     /* UNIT PRICE                              */~
            7*CH(6),        /* Date delivery requested                 */~
            XX(14),         /* Manufacturing Lead Time, In Days        */~
            XX(6),          /* Date 'something' expires                */~
            XX(6),          /* General Purpose Date Field              */~
            XX(3),          /* user-id of specific user                */~
            XX(48),         /* General Purpose Date Field              */~
            XX(21),         /* user-id of specific user                */~
            XX(16),         /* sales order number                      */~
            XX(3),          /* General purpose sequence number         */~
            XX(4),          /* Generic for any code in the system      */~
            XX(200),        /* Variable Fields Data Area               */~
            XX(4),          /* Internal ID to text in TXTFILE.         */~
            XX(160),        /* Office Route                            */~
            CH(1),          /* Qty Most Wanted (1-7)                   */~
            XX(8),          /* Estimate Number again                   */~
            XX(25),         /* Part Number again                       */~
            CH(1),          /* Estimate has been cutover flag          */~
            CH(3),          /* BOM Id ESTBOM was cutover to            */~
            CH(3),          /* RTE Id ESTRTE was cutover to            */~
            XX(106)         /* Filler                                  */

L36800: FMT                 /* FILE #41 -- BCKLNCUR                    */~
            CH(4),          /* Currency code                           */~
            CH(16),         /* Sales Order number                      */~
            CH(3),          /* Sequence Number                         */~
            PD(14,4),       /* Unit Price @ Stocking UOM               */~
            PD(14,4),       /* Unit Price                              */~
            CH(6),          /* Conversion factor effective date        */~
            PD(14,7),       /* Currency conversion factor              */~
            PD(14,7),       /* # Units per statutory currency unit     */~
            CH(39)          /* Filler                                  */

L37110:     FMT /* File #51- CCRMASTR Master file (input/output)       */~
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

        mlq_header_date_check
            for fieldnr% = 9% to 11%
                errormsg$ = " "
                gosub L50000
                     if errormsg$ <> " " then L38080
                if fieldnr% =  9% then L38130
                if fieldnr% = 10%  and ~
                   dfltdue$ <> " " and dfltdue$ <> blankdate$ then L38130
                if fieldnr% = 11%   and ~
                   dfltship$ <> " " and dfltship$ <> blankdate$ then L38130
                gosub L20000
L38080:         gosub'050(1%,fieldnr%,1%)
L38090:         gosub'101(fieldnr%,1%)
                     if keyhit% = 1% then gosub startover
                gosub L50000
                     if errormsg$ <> " " then L38090
L38130:     next fieldnr%
            errormsg$ = " "
            return

        mlq_lines_date_check
            mlq_date_check% = 1%
            for fieldnr% = 5% to 6%
                errormsg$ = " "
                gosub'153(fieldnr%,1%)
                     if errormsg$ = " " then L38390
                if fieldnr% <> 5% then L38290
                     if dfltdue$ = " " or dfltdue$ = blankdate$ then L38340
                          errormsg$ = " "
                          duedate$(c%) = dfltdue$
                          mlq_line_date% = mlq_line_date% + 1%
                          goto L38370
L38290:         if fieldnr% <> 6% then L38390
                     if dfltship$ = " " or dfltship$ = blankdate$ then L38340
                          errormsg$ = " "
                          shipdate$(c%) = dfltship$
                          mlq_line_date% = mlq_line_date% + 1%
                          goto L38370
L38340:         gosub'050(3%,fieldnr%,1%)
L38350:         gosub'103(fieldnr%,1%)
                     if keyhit% = 1% then gosub startover
L38370:         gosub'153(fieldnr%,1%)
                     if errormsg$ <> " " then L38350
L38390:     next fieldnr%
            errormsg$ = " "  :  mlq_date_check% = 0%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            lastlit$ = " "
            if lastso$ <> " " then lastlit$ = "Last S.O.: " & lastso$
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf1
            if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)
            if fieldnr% = 1% then lfac$(2%) = hex(81)

L40075:     accept                                                       ~
               at (01,02), "Sales Order Management",                     ~
               at (01,35), fac(hex(8c)), lastlit$               , ch(30),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Customer Code",                              ~
               at (05,30), fac(lfac$( 1)), cuscode$             , ch(09),~
               at (05,49), fac(hex(8c)),   cusdescr$            , ch(30),~
                                                                         ~
               at (06,02), "Sales Order Number",                         ~
               at (06,30), fac(lfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (07,02), "Purchase Order Number",                      ~
               at (07,30), fac(lfac$( 3)), po$                  , ch(16),~
                                                                         ~
               at (08,02), "Export Order? (Y or N)",                     ~
               at (08,30), fac(lfac$( 4)), export$              , ch(01),~
                                                                         ~
               at (09,02), "Ship",  at (10,02), "  To",                  ~
               at (09,08), fac(lfac$( 5)), shipto$(1)           , ch(30),~
               at (10,08), fac(lfac$( 5)), shipto$(2)           , ch(30),~
               at (11,08), fac(lfac$( 5)), shipto$(3)           , ch(30),~
               at (12,08), fac(lfac$( 5)), shipto$(4)           , ch(30),~
               at (13,08), fac(lfac$( 5)), shipto$(5)           , ch(30),~
               at (14,08), fac(lfac$( 5)), str(shipto$(6), 1,17), ch(17),~
               at (14,26), fac(lfac$( 5)), str(shipto$(6),19, 2), ch(02),~
               at (14,29), fac(lfac$( 5)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (09,42), "Sold",  at (10,42), "  To",                  ~
               at (09,48), fac(lfac$( 6)), soldto$(1)           , ch(30),~
               at (10,48), fac(lfac$( 6)), soldto$(2)           , ch(30),~
               at (11,48), fac(lfac$( 6)), soldto$(3)           , ch(30),~
               at (12,48), fac(lfac$( 6)), soldto$(4)           , ch(30),~
               at (13,48), fac(lfac$( 6)), soldto$(5)           , ch(30),~
               at (14,48), fac(lfac$( 6)), str(soldto$(6), 1,17), ch(17),~
               at (14,66), fac(lfac$( 6)), str(soldto$(6),19, 2), ch(02),~
               at (14,69), fac(lfac$( 6)), str(soldto$(6),22, 9), ch(09),~
                                                                         ~
               at (15,02), "Store Code",                                 ~
               at (15,30), fac(lfac$( 7)), store$               , ch(03),~
               at (15,49), fac(hex(8c)),   storedescr$          , ch(32),~
                                                                         ~
               at (16,02), "Order Date",                                 ~
               at (16,30), fac(lfac$( 8)), orderdate$           , ch(08),~
                                                                         ~
               at (17,02), "Cancellation Date",                          ~
               at (17,30), fac(lfac$( 9)), canceldate$          , ch(08),~
                                                                         ~
               at (18,02), "Default Due Date",                           ~
               at (18,30), fac(lfac$(10)), dfltdue$             , ch(08),~
                                                                         ~
               at (19,02), "Default Req'd Ship Date",                    ~
               at (19,30), fac(lfac$(11)), dfltship$            , ch(08),~
               at (19,49), fac(hex(8c))  , weekdayh$            , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L40415
                gosub call_customer_credit
                goto L40075

L40415:        gosub pf1315
               if keyhit% = 15 then L40075
               if keyhit% <> 8% then L40445
                  gosub dropship
                  goto L40075

L40445:         return

        setpf1
        if edit% = 2% then L40570         /* Input Mode                 */
           pf$(1) = "(1)Start Over    (3)Copy Existing Order           "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field       (26)Custo"&~
                    "mer Credit   (15)Print Screen"
           pf$(3) = "                 (8)Drop Ship            (27)Cutov"&~
                    "er Quotation (16)Exit Program"

           pfkey$ = hex(01ff0304ffffff08ffffffff0dff0f10ffffff1a1b00)
           if fieldnr% = 3% and mlqyorn$ = "Y" then L40505
                str(pf$(3%),42%,21%) = " " : str(pfkey$,21%,1%) = hex(ff)
L40505:    if fieldnr% = 5% then L40515
                str(pf$(3%),18%,12%) = " " : str(pfkey$,8%,1%) = hex(ff)
L40515:    if fieldnr% <  4% and copy% = 0% then L40525
           if fieldnr% < 10% and copy% = 1% then L40525 else L40530
L40525:         str(pf$(2%),18%,17%) = " " : str(pfkey$,4%,1%) = hex(ff)
L40530:    if fieldnr% = 3% then L40540
                str(pf$(1%),18%,23%) = " " : str(pfkey$,3%,1%) = hex(ff)
L40540:    if fieldnr% = 1% then goto L40555
                str(pf$(3%),64%,16%) = " " : str(pfkey$,16%,1%) = hex(ff)
                return
L40555:    str(pf$(2%),42%,19%) = " " : str(pfkey$,20%,1%) = hex(ff)
           return

L40570:  if fieldnr% > 0% then L40655     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                            (25)Manag"&~
                    "e Text       (13)Instructions"
           pf$(2) = "(2)Line Items    (5)Next Screen          (26)Custo"&~
                    "mer Credit   (15)Print Screen"
           pf$(3) = "                (10)Cancel Order         (28)Delet"&~
                    "e Order      (16)End Order   "
           str(pf$(1%),57%,1%) = hex(8c)/* Hilite PF(25) if text exists*/
           gosub'040(textid$)
           str(pf$(1%),41%,1%) = fac25$
           pfkey$ = hex(0102ffff05ffffffff0affff0dff0f10ff191c1d1a00)
           if allow_delete$ <> "N" then L40640
                str(pf$(3),42,16) = " "
                str(pfkey$,19, 1) = hex(ff)
L40640:    return

                                         /* Edit Mode- Field Enabled   */
L40655:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = " "

           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        dropship
            if drop% = 1% then return
            if soldto$() <> " " then L40760 /* don't mess with bill-to */
               soldto$(1%)              =   shipto$(1%)
               soldto$(2%)              =   shipto$(2%)
               soldto$(3%)              =   shipto$(3%)
               soldto$(4%)              =   shipto$(4%)
               soldto$(5%)              =   shipto$(5%)
               str(soldto$(6%), 1%,17%) =   str(shipto$(6%), 1%,17%)
               str(soldto$(6%),19%, 2%) =   str(shipto$(6%),19%, 2%)
               str(soldto$(6%),22%, 9%) =   str(shipto$(6%),22%, 9%)

L40760:     init(" ") shipto$()
            drop% = 1%    /* drop% prevents 2 x thru which clears all */
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf2

           if fieldnr% = 0% then L41080
           lfac$(fieldnr%) = hex(81)
           if fieldnr% = 3% then lfac$(fieldnr%) = hex(80)
           if fieldnr% = 5% then lfac$(fieldnr%) = hex(82)

L41080:     accept                                                       ~
               at (01,02), "Sales Order Management: Header Page 2",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "How Ship",                                   ~
               at (05,30), fac(lfac$( 1)), howship$             , ch(20),~
                                                                         ~
               at (06,02), "FOB",                                        ~
               at (06,30), fac(lfac$( 2)), fob$                 , ch(20),~
                                                                         ~
               at (07,02), "Shipping Instructions",                      ~
               at (07,30), fac(lfac$( 3)), shipinstr$(1)        , ch(50),~
               at (08,30), fac(lfac$( 3)), shipinstr$(2)        , ch(50),~
                                                                         ~
               at (09,02), "Price Code Default",                         ~
               at (09,30), fac(lfac$( 4)), pc$                  , ch(01),~
               at (09,49), fac(hex(8c)),   pcdescr$             , ch(32),~
                                                                         ~
               at (10,02), "Order Discount Percent",                     ~
               at (10,30), fac(lfac$( 5)), orderdisc$           , ch(06),~
                                                                         ~
               at (11,02), "Payment Terms (Code)",                       ~
               at (11,30), fac(lfac$( 6)), terms$               , ch(20),~
               at (11,51), fac(hex(8c))  , termsdescr$          , ch(30),~
                                                                         ~
               at (12,02), "Region Code",                                ~
               at (12,30), fac(lfac$( 7)), region$              , ch(04),~
               at (12,49), fac(hex(8c)),   regiondescr$         , ch(32),~
                                                                         ~
               at (13,02), "Salesman Code / Split %",                    ~
               at (13,30), fac(lfac$( 8)), salesman$(1)         , ch(04),~
               at (13,36), fac(lfac$( 8)), comm$(1)             , ch(03),~
               at (13,49), fac(hex(8c)),   salesmandescr$(1)    , ch(32),~
               at (14,30), fac(lfac$( 8)), salesman$(2)         , ch(04),~
               at (14,36), fac(lfac$( 8)), comm$(2)             , ch(03),~
               at (14,49), fac(hex(8c)),   salesmandescr$(2)    , ch(32),~
               at (15,30), fac(lfac$( 8)), salesman$(3)         , ch(04),~
               at (15,36), fac(lfac$( 8)), comm$(3)             , ch(03),~
               at (15,49), fac(hex(8c)),   salesmandescr$(3)    , ch(32),~
                                                                         ~
               at (16,02), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (16,30), fac(lfac$( 9%)), pm_so$              , ch(01),~
               at (17,02), fac(hex(8c))  , pm_hdr_dsply2$       , ch(28),~
               at (17,30), fac(lfac$(10%)), pm_inv$             , ch(01),~
                                                                         ~
               at (18,02), "Sales Account Default",                      ~
               at (18,30), fac(lfac$(11)), dfltsales$           , ch(12),~
               at (18,49), fac(hex(8c)),   dfltsalesdescr$      , ch(32),~
                                                                         ~
               at (19,02), "Sales Discounts Account",                    ~
               at (19,30), fac(lfac$(12)), discacct$            , ch(12),~
               at (19,49), fac(hex(8c)),   discacctdescr$       , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L41380
                gosub call_customer_credit
                goto L41080

L41380:        gosub pf1315
               if keyhit% = 15 then L41080
                return

        setpf2
        if edit% = 2% then L41495         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field       (26)Custo"&~
                    "mer Credit   (15)Print Screen"
           pf$(3) = " "

           pfkey$ = hex(01ffff04ffffffffffffffff0dff0fffffffff1a00)
           if fieldnr% > 1% then return
                str(pf$(2),18,17) = " " : str(pfkey$, 4, 1) = hex(ff)
           return

L41495:  if fieldnr% > 0% then L41560     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over    (4)Prev Screen          (25)Manag"&~
                    "e Text       (13)Instructions"
           pf$(2) = "(2)Line Items    (5)Next Screen          (26)Custo"&~
                    "mer Credit   (15)Print Screen"
           pf$(3) = "                                         (28)Delet"&~
                    "e Order      (16)End Order   "
           str(pf$(1%),57%,1%) = hex(8c) /*Hilite PF(25) if text exists*/
           gosub'040(textid$)
           str(pf$(1%),41%,1%) = fac25$
           pfkey$ = hex(0102ff0405ffffffffffff0c0dff0f10ff191c1d1a00)
             if allow_delete$ <> "N" then L41550
             str(pf$(3),42,16) = " "
             str(pfkey$,19, 1) = hex(ff)
L41550:    return
                                         /* Edit Mode- Field Enabled   */
L41560:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = " "

           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
            xfac$ = hex(9c) : lastkey%  = 0%
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% > 0% then init(hex(8c)) lfac2$()                 ~
                             else init(hex(86)) lfac2$()
            if maxlines%  > 1% then lfac$(11) = hex(8c)
            if maxlines% <= 1% and fieldnr% = 11% then lfac$(11) = hex(81)
            gosub setpf3
            on fieldnr%       gosub L42152,         /* Estimate Number  */~
                                    L42152,         /* Part Code        */~
                                    L42148,         /* Part Description */~
                                    L42152,         /* Part Category    */~
                                    L42152,         /* Due Date         */~
                                    L42152,         /* Req'd Ship Date  */~
                                    L42148,         /* Stocking UOM     */~
                                    L42148,         /* Order Qty        */~
                                    L42148,         /* Open Order Qty   */~
                                    L42148,         /* Total Qty Shipped*/~
                                         ,         /* Currency code    */~
                                    L42148,         /* Pricing UOM      */~
                                    L42156,         /* Conversion       */~
                                    L42156,         /* Unit Price       */~
                                    L42156,         /* Line Item Disc   */~
                                    L42152          /* Line Taxable?    */
            if fieldnr%  = 8% then lfac2$(8%)= hex(81) /* Alloc Flag   */
            if fieldnr% <> 9% then L42144
                call "LOTENABL" (part$(c%), lot_enable%, ll%, #02, #04)
                if lot_enable% > 0% then lfac2$(9%) = hex(81)
                if lot_enable% = 0% then lot$(c%)   = " "
L42144:     goto L42164
L42148:                             lfac$(fieldnr%) = hex(80) : return
L42152:                             lfac$(fieldnr%) = hex(81) : return
L42156:                             lfac$(fieldnr%) = hex(82) : return

L42164:     accept                                                       ~
               at (01,02), "Sales Order Management: Line Item Screen 1", ~
               at (01,47), fac(hex(8c)), demstatusmsg$          , ch(16),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,40), fac(hex(8c))  , atc$(c%,1%)          , ch(41),~
                                                                         ~
               at (05,02), "Estimate Number",                            ~
               at (05,30), fac(lfac$( 1)), est$(c%)             , ch(08),~
               at (05,40), fac(hex(8c))  , atc$(c%,2%)          , ch(41),~
                                                                         ~
               at (06,02), "Part Code",                                  ~
               at (06,30), fac(lfac$( 2)), part$(c%)            , ch(25),~
               at (06,56), fac(xfac$),     mfgcode$             , ch(09),~
               at (06,66), fac(hex(8c))  , nonstockmsg$(c%)     , ch(14),~
                                                                         ~
               at (07,02), "Part Description",                           ~
               at (07,30), fac(lfac$( 3)), descr$(c%)           , ch(32),~
                                                                         ~
               at (08,02), "Part Category",                              ~
               at (08,30), fac(lfac$( 4)), cat$(c%)             , ch(04),~
               at (08,49), fac(hex(8c)),   catdescr$            , ch(32),~
                                                                         ~
               at (09,02), "Due Date",                                   ~
               at (09,30), fac(lfac$( 5)), duedate$(c%)         , ch(08),~
               at (09,49), "Original Due Date: ",                        ~
               at (09,68), fac(hex(8c))  , origdue$(c%)         , ch(08),~
                                                                         ~
               at (10,02), "Required Ship Date",                         ~
               at (10,30), fac(lfac$( 6)), shipdate$(c%)        , ch(08),~
               at (10,39), fac(hex(8c))  , weekday$             , ch(09),~
               at (10,49), fac(hex(8c))  , optmsg$              , ch(30),~
                                                                         ~
               at (11,02), "Stocking Unit of Measure",                   ~
               at (11,30), fac(lfac$( 7)), stkuom$(c%)          , ch(04),~
               at (11,49), fac(hex(8c)),   stkuomdescr$         , ch(32),~
                                                                         ~
               at (12,02), "Order Quantity / Allocate",                  ~
               at (12,30), fac(lfac$( 8)), order$               , ch(10),~
               at (12,41), fac(lfac2$(8)), alloc$(c%)           , ch(01),~
               at (12,49), fac(hex(8c)),   allocdescr$          , ch(32),~
                                                                         ~
               at (13,02), "Open Order Quantity / Lot",                  ~
               at (13,30), fac(lfac$( 9)), openqty$             , ch(10),~
               at (13,41), fac(lfac2$(9)), str(lot$(c%),,ll%),           ~
               at (13,49), "Quantity Scheduled",                         ~
               at (13,70), fac(hex(8c))  , qtyschld$            , ch(10),~
                                                                         ~
               at (14,02), "Shipped - AND Invoiced",                     ~
               at (14,30), fac(lfac$(10)), ship$                , ch(10),~
               at (14,49), fac(hex(8c))  , preinvmsg$           , ch(32),~
                                                                         ~
               at (15,02), "Currency code",                              ~
               at (15,30), fac(lfac$(11)), currency$            , ch(04),~
               at (15,49), fac(hex(8c)),   currdesc$            , ch(32),~
                                                                         ~
               at (16,02), "Pricing Unit of Measure",                    ~
               at (16,30), fac(lfac$(12)), priceuom$(c%)        , ch(04),~
               at (16,49), fac(hex(8c)),   priceuomdescr$       , ch(32),~
                                                                         ~
               at (17,02), "Conversion PPPP to SSSS",                    ~
               at (17,13), fac(hex(8c)), priceuom$(c%)          , ch(04),~
               at (17,21), fac(hex(8c)), stkuom$  (c%)          , ch(04),~
               at (17,30), fac(lfac$(13)), conv$                , ch(10),~
                                                                         ~
               at (18,02), "Unit Price @ Pricing UOM",                   ~
               at (18,30), fac(lfac$(14)), price$               , ch(10),~
               at (18,49), "Price @ Stocking UOM: ",                     ~
               at (18,71), fac(hex(8c))  , pricestk$            , ch(10),~
                                                                         ~
               at (19,02), "Line Item Discount %",                       ~
               at (19,30), fac(lfac$(15)), linedisc$            , ch(06),~
               at (19,49), "Line Discount Amount: ",                     ~
               at (19,71), fac(hex(8c))  , discamt$             , ch(10),~
                                                                         ~
               at (20,02), "Line Taxable? (Y/N)",                        ~
               at (20,30), fac(lfac$(16)), taxable$(c%)         , ch(01),~
               at (20,49), "EXTENSION (Open Qty): ",                     ~
               at (20,71), fac(hex(8c))  , ext$                 , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 23% then L42540
                    lastkey% = keyhit%
                    init(hex(81)) xfac$ : init(hex(84)) lfac$()
                    inpmessage$ = "Enter The Manufacturer's Code For Th"&~
                                  "is Part Number."
                    goto L42164

L42540:     if keyhit% <> 26% then goto L42556
                gosub call_customer_credit
                goto L42164

L42556:        if keyhit% <> 10% then L42572
                    gosub call_bckprcsb
                    goto L42164

L42572:        if keyhit% <> 25% then L42578
                    gosub edit_text_line : goto L42164

L42578:        if keyhit% <>  5% then goto L42582
                    if edit% <> 1% then L42582
                        gosub create_estimate  :  goto L42164

L42582:        if keyhit% <> 23% then goto L42592
                    if edit% <> 2% then L42592
                          if fieldnr% <> 0% then L42592
                               if so_cut$ <> "Y" then L42592
                                   gosub cutover_estimate
                                   gosub setpf3  :  goto L42164

L42592:        if keyhit% <> 8% then L42604
                    call "PIPATCSB" (part$(c%), #24,                     ~
                            #04, #28, #19, #25, #26, #29, #18, #27)
                    goto L42164

L42604:        if keyhit% <> 24% then L42620
                  call "HNYQDISP" (part$(c%), #04, #13, #13, #02)
                  goto L42164

L42620:        if keyhit% <> 14% then L42632
                  gosub display_corebank
                  goto L42164

L42632:        if keyhit% <> 20% then L42640
                  gosub display_pm_surcharges
                  goto L42164

L42640:        gosub pf1315
               if keyhit% = 15% then L42164
               return

        setpf3
        if xref% = 0% then L42680
            init(hex(00)) plowkey$
            str(plowkey$,1%,1%)="C"  :  str(plowkey$,2%,9%)=str(cuscode$)
            call "READ104" (#45, plowkey$, f1%(45%))
                if f1%(45%) = 0% then L42680
            if str(plowkey$,1%,10%)= "C" & str(cuscode$) then cust% = 1%
L42680: if edit% = 2% then L42820         /* Input Mode                 */
           pf$(1) = "(1)St Ovr (4)Prev Fld              (10)Cost/Price "&~
                    "(11)Date Run (13)Instructions"
           pf$(2) = "(2)Restart Line (6)Size Run (22)Cust Pt Xref (25)L"&~
                    "ine Text     (15)Print Screen"
           pf$(3) = "(3)Copy Prev (8/24)ATC/QTY  (23)Mfg Pt Xref  (26)C"&~
                    "ust Credit   (16)Line Summary"
           gosub hilite_text
           pfkey$ = hex(01020304ff06ff08ff0a0bff0dff0f101819ff1a1617ff00)
           if fieldnr% > 1% then goto L42716
                str(pf$(2%),1%,15%) = " " : str(pfkey$,2%,1%) = hex(ff)
                str(pf$(2%),1%,15%) = "(5)Create Est."
                     str(pfkey$,5%,1%) = hex(05)
L42716:    if xref% = 1% then L42728
                str(pf$(2%),29%,16%) = " "  : str(pfkey$,21%,1%) = hex(ff)
                str(pf$(3%),29%,16%) = " "  : str(pfkey$,22%,1%) = hex(ff)
L42728:    if cust% = 1% then L42736
                str(pf$(2%),29%,16%) = " "  : str(pfkey$,21%,1%) = hex(ff)
L42736:    if fieldnr% < 2% then L42744
           if fieldnr% = 2% then L42752
                str(pf$(2%),29%,16%) = "(18)Part Data"
                str(pfkey$,23%,1%) = hex(12):str(pfkey$,21%,1%) = hex(ff)
                goto L42748
L42744:         str(pf$(2%),29%,16%) = " "  : str(pfkey$,21%,1%) = hex(ff)
L42748:         str(pf$(3%),29%,16%) = " "  : str(pfkey$,22%,1%) = hex(ff)
L42752:    if core% = 0% or fieldnr% = 1% then L42761
                str(pf$(1%),23%,13%) = "(14)Core Bnk"
                str(pfkey$,14%,1%) = hex(0e)
L42761:    if c% > 1% then L42764
                str(pf$(1%),51%,13%) = " " : str(pfkey$,11%,1%) = hex(ff)
L42764:    if fieldnr% < 3% then L42776
                str(pf$(2%),17%,11%) = " " : str(pfkey$,6%,1%) = hex(ff)
                str(pf$(3%),64%)     = " " : str(pfkey$,16%,1%) = hex(ff)
L42776:    if fieldnr% > 1% then L42784
                str(pf$(1%),11%,12%) = " " : str(pfkey$,4%,1%) = hex(ff)
L42784:    if fieldnr% > 2% then L42804
                str(pf$(2%),46%,13%) = " " : str(pfkey$,18%,1%) = hex(ff)
                str(pf$(3%),14%,15%) = " " : str(pfkey$,8%,1%) = hex(ff)
                                             str(pfkey$,17%,1%) = hex(ff)
                str(pf$(1%),36%,15%) = " " : str(pfkey$,10%,1%) = hex(ff)
L42804:    if c% > 1% then return
                str(pf$(3%),1%,12%) = " " : str(pfkey$,3%,1%) = hex(ff)
           return

L42820:  if fieldnr% > 0% then L42900     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over   (5)Next Screen               (10)C"&~
                    "osts/Prices  (13)Instructions"
           pf$(2) = "                (8/24)ATC/QTY  (18)Part Data (25)L"&~
                    "ine Text     (15)Print Screen"
           pf$(3) = "(12)Delete Line (9)Header Scrn               (26)C"&~
                    "ust Credit   (16)Line Summary"
           gosub hilite_text
           pfkey$ = hex(01ffffff05060708090aff0c0dff0f1018191d1a1200ffff)
           inpmessage$ = edtmessage2$
           if core% = 0% then L42860
                str(pf$(1%),32%,14%) = "(14)Core Bank"
                str(pfkey$,14%,1%) = hex(0e)
L42860:    if est$(c%) = " " then L42868
                if est_cutover$(c%) = "Y" then L42868
                    if so_cut$ <> "Y" then L42868
                        str(pf$(2%), 1%,15%) = "(23)Est Cutover"
                        str(pfkey$,24%,1%)    = hex(17)
L42868:    if allow_delete$ <> "N" then L42886
           if str(lsts$(c%),2,1) = "A" or str(lsts$(c%),2,1) = "R"       ~
                                                              then L42886
               str(pf$(3%),1%,15%) = " "
               str(pfkey$,12%,1%) = hex(ff)
L42886:    if pm_adj%(c%) <> 1% then L42894
                str(pf$(3%),32%,13%) = "(20)PM Charge"
                str(pfkey$,23%,1%) = hex(14)

L42894:    return
                                         /* Edit Mode- Field Enabled   */
L42900:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        hilite_text
           str(pf$(2%),59%,1%) = hex(8c) /*Hilte PF(25) if text exists*/
           gosub'040(textidl$(c%))
           str(pf$(2%),45%,1%) = fac25$
           return

        call_bckprcsb
            call "BCKPRCSB" (cuscode$, custype$, part$(c%), cat$(c%),    ~
                pc$, currency$, currtype$, order(c%), #02, #01, #04, #40)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'104(fieldnr%, edit%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf4

            if fieldnr% = 0% then L43125
            lfac$(fieldnr%) = hex(81)
            if fieldnr% = 1% then lfac$(fieldnr%) = hex(80)

L43125:     accept                                                       ~
               at (01,02), "Sales Order Management: Line Item Screen 2", ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "P.O. Item Reference",                        ~
               at (06,30), fac(lfac$( 1)), item$(c%)            , ch(03),~
                                                                         ~
               at (07,02), "Planning Priority Code",                     ~
               at (07,30), fac(lfac$( 2)), priority$(c%)        , ch(01),~
                                                                         ~
               at (08,02), "Planning Demand Type",                       ~
               at (08,30), fac(lfac$( 3)), demtype$(c%)         , ch(01),~
                                                                         ~
               at (09,02), "Project Number",                             ~
               at (09,30), fac(lfac$( 4)), project$(c%)         , ch(08),~
               at (09,49), fac(hex(8c)),   projectdescr$        , ch(30),~
                                                                         ~
               at (10,02), "Sales Distr. Account",                       ~
               at (10,30), fac(lfac$( 5)), salesacct$(c%)       , ch(12),~
               at (10,49), fac(hex(8c)),   salesacctdescr$      , ch(32),~
                                                                         ~
               at (11,02), "Sales Discounts Account",                    ~
               at (11,30), fac(lfac$( 6)), discacctl$(c%)       , ch(12),~
               at (11,49), fac(hex(8c)),   discacctldescr$      , ch(32),~
                                                                         ~
               at (12,02), "Specific BOM Id.",                           ~
               at (12,30), fac(lfac$( 7)), bom$(c%)             , ch(03),~
               at (12,49), fac(hex(8c)),   bomdescr$            , ch(32),~
                                                                         ~
               at (13,02), "Shipping Priority Code",                     ~
               at (13,30), fac(lfac$( 8)), shipcode$(c%)        , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L43326
                gosub call_customer_credit
                goto L43125

L43326:        if keyhit% <> 10% then L43330
                    gosub call_bckprcsb
                    goto L43125

L43330:        if keyhit% <> 25% then L43346
                    gosub edit_text_line
                    goto L43125

L43346:        if keyhit% <> 14% then L43350
                  gosub display_corebank
                  goto L43125

L43350:        gosub pf1315
               if keyhit% = 15 then L43125
               return

        setpf4
        if edit% = 2% then L43445         /* Input Mode                 */
           pf$(1) = "(1)Start Over   (4)Prev Field                (10)C"&~
                    "osts/Prices  (13)Instructions"
           pf$(2) = "(2)Restart Line             (18)Part Data    (25)L"&~
                    "ine Text     (15)Print Screen"
           pf$(3) = "                                             (26)C"&~
                    "ust Credit                   "
           gosub hilite_text
           pfkey$ = hex(0102ff04ffffffffff0affff0dff0fffffffff1a1200)
           if core% = 0% then L43436
                str(pf$(3%),1%,13%) = "(14)Core Bank"
                str(pfkey$,14%,1%) = hex(0e)
L43436:    if fieldnr% > 1% then return
                str(pf$(1%),17%,14%) = " " : str(pfkey$,4%,1%) = hex(ff)
           return

L43445:  if fieldnr% > 0% then L43500     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over   (4)Previous Screen           (10)C"&~
                    "osts/Prices  (13)Instructions"
           pf$(2) = "(12)Delete Line              (18)Part Data   (25)L"&~
                    "ine Text     (15)Print Screen"
           pf$(3) = "                (9)Header Screen             (26)C"&~
                    "ust Credit   (16)Line Summary"
           gosub hilite_text
           pfkey$ = hex(01ffff04ff0607ff090aff0c0dff0f10ff191d1a1200)
           inpmessage$ = edtmessage2$
           if core% = 0% then L43481
                str(pf$(3%),1%,13%) = "(14)Core Bank"
                str(pfkey$,14%,1%) = hex(0e)
L43481:    if allow_delete$ <> "N" then L43488
           if str(lsts$(c%),2,1) = "A" or str(lsts$(c%),2,1) = "R"       ~
                                                             then L43488
               str(pf$(2%),1%,15%) = " "
               str(pfkey$,12%,1%) = hex(ff)
L43488:    return

                                         /* Edit Mode- Field Enabled   */
L43500:    pf$(1) = "(1)Start Over                                     "&~
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
            * Line Item Summary Screen. (#5 if you're counting).        *~
            *************************************************************

        deffn'115                        /* Display and Select Line    */
            if cms% = 1% then mat ptdisp$ = part$                        ~
                         else mat ptdisp$ = refpart$
            edit% = 1%
            init (hex(86)) lfac$() : init (hex(84)) sfac$()
            afac$ = hex(81)
            init(" ") sorder$(), sopen$(), sext$()
            for x% = 1% to 15%
                xl% = x% + l% : if seq$(xl%) = " " then L44105
                     if str(lsts$(xl%),2,1) = "D" then lfac$(x%) = hex(8e)
                     if str(lsts$(xl%),2,1) = "D" then sfac$(x%) = hex(8c)
                     temp  = round(openqty(xl%) * price(xl%)/conv(xl%), 2)
                     temp1 = round(temp * linedisc(xl%) * .01  , 2)
                     temp  = round(temp - temp1                , 2)
                     call "CONVERT" (order  (xl%), 2.2, sorder$(x%))
                     call "CONVERT" (openqty(xl%), 2.2, sopen$ (x%))
                     call "CONVERT" (temp        , 2.2, sext$  (x%))
            next x%
L44105:     goto L44150

        deffn'125(fieldnr%)              /* Delete Line Item           */
            edit% = 2%
            init (hex(8c)) lfac$(), sfac$(), afac$
            if fieldnr% <> 0% then lfac$(fieldnr%) = hex(94)             ~
                              else init (hex(94)) lfac$()
            if fieldnr% <> 0% then sfac$(fieldnr%) = hex(94)             ~
                              else init (hex(94)) sfac$()
            init(" ") sorder$(), sopen$(), sext$()
            for x% = 1% to 15%
                xl% = x% + l% : if seq$(xl%) = " " then L44150
                     temp  = round(openqty(xl%) * price(xl%)/conv(xl%), 2)
                     temp1 = round(temp * linedisc(xl%) * .01  , 2)
                     temp  = round(temp - temp1                , 2)
                     call "CONVERT" (order  (xl%), 2.2, sorder$(x%))
                     call "CONVERT" (openqty(xl%), 2.2, sopen$ (x%))
                     call "CONVERT" (temp        , 2.2, sext$  (x%))
            next x%
L44150:     gosub setpf5
            gosub calc_net_for_kendra

L44165:     accept                                                       ~
               at (01,02), "Sales Order Management: Line Item Summary",  ~
               at (01,46), "Currency is ",                               ~
               at (01,58), fac(hex(8c)), currency$              , ch(04),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,40), "Total Net Open Amount: ",                    ~
               at (03,63), fac(hex(84)), topen(4),  pic(-###,###,###.00),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr$(1)                , ch(25),~
               at (05,28), fac(hex(ac)), hdr$(2)                , ch( 3),~
               at (05,32), fac(hex(ac)), hdr$(3)                , ch( 3),~
               at (05,36), fac(hex(ac)), hdr$(4)                , ch(10),~
               at (05,47), fac(hex(ac)), hdr$(5)                , ch(10),~
               at (05,58), fac(hex(ac)), hdr$(6)                , ch(10),~
               at (05,69), fac(hex(ac)), hdr$(7)                , ch( 8),~
               at (05,78), fac(hex(ac)), hdr$(8)                , ch( 3),~
               at (06,02), fac(hex(80))   , ptdisp$   (l%+ 1%)  , ch(25),~
                                                                         ~
               at (06,02), fac(lfac$( 1%)), ptdisp$   (l%+ 1%)  , ch(25),~
               at (07,02), fac(lfac$( 2%)), ptdisp$   (l%+ 2%)  , ch(25),~
               at (08,02), fac(lfac$( 3%)), ptdisp$   (l%+ 3%)  , ch(25),~
               at (09,02), fac(lfac$( 4%)), ptdisp$   (l%+ 4%)  , ch(25),~
               at (10,02), fac(lfac$( 5%)), ptdisp$   (l%+ 5%)  , ch(25),~
               at (11,02), fac(lfac$( 6%)), ptdisp$   (l%+ 6%)  , ch(25),~
               at (12,02), fac(lfac$( 7%)), ptdisp$   (l%+ 7%)  , ch(25),~
               at (13,02), fac(lfac$( 8%)), ptdisp$   (l%+ 8%)  , ch(25),~
               at (14,02), fac(lfac$( 9%)), ptdisp$   (l%+ 9%)  , ch(25),~
               at (15,02), fac(lfac$(10%)), ptdisp$   (l%+10%)  , ch(25),~
               at (16,02), fac(lfac$(11%)), ptdisp$   (l%+11%)  , ch(25),~
               at (17,02), fac(lfac$(12%)), ptdisp$   (l%+12%)  , ch(25),~
               at (18,02), fac(lfac$(13%)), ptdisp$   (l%+13%)  , ch(25),~
               at (19,02), fac(lfac$(14%)), ptdisp$   (l%+14%)  , ch(25),~
               at (20,02), fac(lfac$(15%)), ptdisp$   (l%+15%)  , ch(25),~
                                                                         ~
               at (06,28), fac(sfac$( 1)), seq$      (l%+ 1%)   , ch(03),~
               at (07,28), fac(sfac$( 2)), seq$      (l%+ 2%)   , ch(03),~
               at (08,28), fac(sfac$( 3)), seq$      (l%+ 3%)   , ch(03),~
               at (09,28), fac(sfac$( 4)), seq$      (l%+ 4%)   , ch(03),~
               at (10,28), fac(sfac$( 5)), seq$      (l%+ 5%)   , ch(03),~
               at (11,28), fac(sfac$( 6)), seq$      (l%+ 6%)   , ch(03),~
               at (12,28), fac(sfac$( 7)), seq$      (l%+ 7%)   , ch(03),~
               at (13,28), fac(sfac$( 8)), seq$      (l%+ 8%)   , ch(03),~
               at (14,28), fac(sfac$( 9)), seq$      (l%+ 9%)   , ch(03),~
               at (15,28), fac(sfac$(10)), seq$      (l%+10%)   , ch(03),~
               at (16,28), fac(sfac$(11)), seq$      (l%+11%)   , ch(03),~
               at (17,28), fac(sfac$(12)), seq$      (l%+12%)   , ch(03),~
               at (18,28), fac(sfac$(13)), seq$      (l%+13%)   , ch(03),~
               at (19,28), fac(sfac$(14)), seq$      (l%+14%)   , ch(03),~
               at (20,28), fac(sfac$(15)), seq$      (l%+15%)   , ch(03),~
                                                                         ~
               at (06,32), fac(sfac$( 1)), item$     (l%+ 1%)   , ch(03),~
               at (07,32), fac(sfac$( 2)), item$     (l%+ 2%)   , ch(03),~
               at (08,32), fac(sfac$( 3)), item$     (l%+ 3%)   , ch(03),~
               at (09,32), fac(sfac$( 4)), item$     (l%+ 4%)   , ch(03),~
               at (10,32), fac(sfac$( 5)), item$     (l%+ 5%)   , ch(03),~
               at (11,32), fac(sfac$( 6)), item$     (l%+ 6%)   , ch(03),~
               at (12,32), fac(sfac$( 7)), item$     (l%+ 7%)   , ch(03),~
               at (13,32), fac(sfac$( 8)), item$     (l%+ 8%)   , ch(03),~
               at (14,32), fac(sfac$( 9)), item$     (l%+ 9%)   , ch(03),~
               at (15,32), fac(sfac$(10)), item$     (l%+10%)   , ch(03),~
               at (16,32), fac(sfac$(11)), item$     (l%+11%)   , ch(03),~
               at (17,32), fac(sfac$(12)), item$     (l%+12%)   , ch(03),~
               at (18,32), fac(sfac$(13)), item$     (l%+13%)   , ch(03),~
               at (19,32), fac(sfac$(14)), item$     (l%+14%)   , ch(03),~
               at (20,32), fac(sfac$(15)), item$     (l%+15%)   , ch(03),~
                                                                         ~
               at (06,36), fac(sfac$( 1)), sorder$   (    1%)   , ch(10),~
               at (07,36), fac(sfac$( 2)), sorder$   (    2%)   , ch(10),~
               at (08,36), fac(sfac$( 3)), sorder$   (    3%)   , ch(10),~
               at (09,36), fac(sfac$( 4)), sorder$   (    4%)   , ch(10),~
               at (10,36), fac(sfac$( 5)), sorder$   (    5%)   , ch(10),~
               at (11,36), fac(sfac$( 6)), sorder$   (    6%)   , ch(10),~
               at (12,36), fac(sfac$( 7)), sorder$   (    7%)   , ch(10),~
               at (13,36), fac(sfac$( 8)), sorder$   (    8%)   , ch(10),~
               at (14,36), fac(sfac$( 9)), sorder$   (    9%)   , ch(10),~
               at (15,36), fac(sfac$(10)), sorder$   (   10%)   , ch(10),~
               at (16,36), fac(sfac$(11)), sorder$   (   11%)   , ch(10),~
               at (17,36), fac(sfac$(12)), sorder$   (   12%)   , ch(10),~
               at (18,36), fac(sfac$(13)), sorder$   (   13%)   , ch(10),~
               at (19,36), fac(sfac$(14)), sorder$   (   14%)   , ch(10),~
               at (20,36), fac(sfac$(15)), sorder$   (   15%)   , ch(10),~
                                                                         ~
               at (06,47), fac(sfac$( 1)), sopen$    (    1%)   , ch(10),~
               at (07,47), fac(sfac$( 2)), sopen$    (    2%)   , ch(10),~
               at (08,47), fac(sfac$( 3)), sopen$    (    3%)   , ch(10),~
               at (09,47), fac(sfac$( 4)), sopen$    (    4%)   , ch(10),~
               at (10,47), fac(sfac$( 5)), sopen$    (    5%)   , ch(10),~
               at (11,47), fac(sfac$( 6)), sopen$    (    6%)   , ch(10),~
               at (12,47), fac(sfac$( 7)), sopen$    (    7%)   , ch(10),~
               at (13,47), fac(sfac$( 8)), sopen$    (    8%)   , ch(10),~
               at (14,47), fac(sfac$( 9)), sopen$    (    9%)   , ch(10),~
               at (15,47), fac(sfac$(10)), sopen$    (   10%)   , ch(10),~
               at (16,47), fac(sfac$(11)), sopen$    (   11%)   , ch(10),~
               at (17,47), fac(sfac$(12)), sopen$    (   12%)   , ch(10),~
               at (18,47), fac(sfac$(13)), sopen$    (   13%)   , ch(10),~
               at (19,47), fac(sfac$(14)), sopen$    (   14%)   , ch(10),~
               at (20,47), fac(sfac$(15)), sopen$    (   15%)   , ch(10),~
                                                                         ~
               at (06,58), fac(sfac$( 1)), sext$     (    1%)   , ch(10),~
               at (07,58), fac(sfac$( 2)), sext$     (    2%)   , ch(10),~
               at (08,58), fac(sfac$( 3)), sext$     (    3%)   , ch(10),~
               at (09,58), fac(sfac$( 4)), sext$     (    4%)   , ch(10),~
               at (10,58), fac(sfac$( 5)), sext$     (    5%)   , ch(10),~
               at (11,58), fac(sfac$( 6)), sext$     (    6%)   , ch(10),~
               at (12,58), fac(sfac$( 7)), sext$     (    7%)   , ch(10),~
               at (13,58), fac(sfac$( 8)), sext$     (    8%)   , ch(10),~
               at (14,58), fac(sfac$( 9)), sext$     (    9%)   , ch(10),~
               at (15,58), fac(sfac$(10)), sext$     (   10%)   , ch(10),~
               at (16,58), fac(sfac$(11)), sext$     (   11%)   , ch(10),~
               at (17,58), fac(sfac$(12)), sext$     (   12%)   , ch(10),~
               at (18,58), fac(sfac$(13)), sext$     (   13%)   , ch(10),~
               at (19,58), fac(sfac$(14)), sext$     (   14%)   , ch(10),~
               at (20,58), fac(sfac$(15)), sext$     (   15%)   , ch(10),~
                                                                         ~
               at (06,69), fac(sfac$( 1)), shipdate$ (l%+ 1%)   , ch(08),~
               at (07,69), fac(sfac$( 2)), shipdate$ (l%+ 2%)   , ch(08),~
               at (08,69), fac(sfac$( 3)), shipdate$ (l%+ 3%)   , ch(08),~
               at (09,69), fac(sfac$( 4)), shipdate$ (l%+ 4%)   , ch(08),~
               at (10,69), fac(sfac$( 5)), shipdate$ (l%+ 5%)   , ch(08),~
               at (11,69), fac(sfac$( 6)), shipdate$ (l%+ 6%)   , ch(08),~
               at (12,69), fac(sfac$( 7)), shipdate$ (l%+ 7%)   , ch(08),~
               at (13,69), fac(sfac$( 8)), shipdate$ (l%+ 8%)   , ch(08),~
               at (14,69), fac(sfac$( 9)), shipdate$ (l%+ 9%)   , ch(08),~
               at (15,69), fac(sfac$(10)), shipdate$ (l%+10%)   , ch(08),~
               at (16,69), fac(sfac$(11)), shipdate$ (l%+11%)   , ch(08),~
               at (17,69), fac(sfac$(12)), shipdate$ (l%+12%)   , ch(08),~
               at (18,69), fac(sfac$(13)), shipdate$ (l%+13%)   , ch(08),~
               at (19,69), fac(sfac$(14)), shipdate$ (l%+14%)   , ch(08),~
               at (20,69), fac(sfac$(15)), shipdate$ (l%+15%)   , ch(08),~
                                                                         ~
               at (06,78), fac(sfac$( 1)), lsts$     (l%+ 1%)   , ch(03),~
               at (07,78), fac(sfac$( 2)), lsts$     (l%+ 2%)   , ch(03),~
               at (08,78), fac(sfac$( 3)), lsts$     (l%+ 3%)   , ch(03),~
               at (09,78), fac(sfac$( 4)), lsts$     (l%+ 4%)   , ch(03),~
               at (10,78), fac(sfac$( 5)), lsts$     (l%+ 5%)   , ch(03),~
               at (11,78), fac(sfac$( 6)), lsts$     (l%+ 6%)   , ch(03),~
               at (12,78), fac(sfac$( 7)), lsts$     (l%+ 7%)   , ch(03),~
               at (13,78), fac(sfac$( 8)), lsts$     (l%+ 8%)   , ch(03),~
               at (14,78), fac(sfac$( 9)), lsts$     (l%+ 9%)   , ch(03),~
               at (15,78), fac(sfac$(10)), lsts$     (l%+10%)   , ch(03),~
               at (16,78), fac(sfac$(11)), lsts$     (l%+11%)   , ch(03),~
               at (17,78), fac(sfac$(12)), lsts$     (l%+12%)   , ch(03),~
               at (18,78), fac(sfac$(13)), lsts$     (l%+13%)   , ch(03),~
               at (19,78), fac(sfac$(14)), lsts$     (l%+14%)   , ch(03),~
               at (20,78), fac(sfac$(15)), lsts$     (l%+15%)   , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (22,51), fac(afac$  ),   aset$                , ch(01),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key (keyhit%)

            if keyhit% <> 22% then goto L44911
                if cms% = 1% then L44906
                    cms% = 1%  :  ref% = 0%
                    hdr$(1) = "Part Code"
                    mat ptdisp$ = part$
                    goto L44165
L44906:         cms% = 0%  :  ref% = 1%
                hdr$(1) = "Reference Part Code"
                mat ptdisp$ = refpart$
                goto L44165

L44911:     if keyhit% <> 26% then goto L44920
                gosub call_customer_credit
                goto L44165

L44920:        gosub pf1315
               if keyhit% = 15% then L44165
               return

        setpf5
        if edit% = 2% then L45220         /* Display Mode               */
           pf$(1) = "(1)Start Over (2)First (5)Next (9)Hdr (10)Alloc= x"&~
                    " (18)PartDtl (13)Instructions"
           pf$(2) = "(22)Part Tgl  (3)Last  (6)Down (11/27)App Lns/Qt ("&~
                    "23)All Dates (15)Print Screen"
           pf$(3) = "(26)Cust Crdt (4)Prev  (7)Up   (12)Del Ln/Order  ("&~
                    "24)Mod Date  (16/32)End/Save "
           pfkey$= hex(01020304050607ff090a0b0c0dff0f10181c201a161b121700)
           if mlqyorn$ = "Y" then L45047
                str(pf$(2%),32%,17%) = "(11)Append Lines"
                str(pfkey$,22%,1%) = hex(ff)
L45047:    if refflag% = 1% then L45050
                str(pf$(2%),1%,13%) = " " : str(pfkey$,21%,1%) = hex(ff)
L45050:    if allow_delete$ <> "N" then L45090
               str(pf$(3%),32%,16%) = " "
               str(pfkey$,12%,1%) = hex(ff)
               str(pfkey$,18%,1%) = hex(ff)
L45090:    if l% <> 0% then L45130
                str(pf$(1%),15%,8%), str(pf$(3%),15%,7%),                ~
                     str(pf$(2%),24%,7%) = " "
                str(pfkey$,2%,1%), str(pfkey$,4%,1%), str(pfkey$,6%,1%)  ~
                     = hex(ff)
L45130:    if l% + 15% < maxlines% then L45200
                str(pf$(2%),15%,7%), str(pf$(1%),24%,7%),                ~
                     str(pf$(3%),24%,7%) = " "
                str(pfkey$,3%,1%), str(pfkey$,5%,1%), str(pfkey$,7%,1%)  ~
                     = hex(ff)
L45200:    return

L45220:  if fieldnr% > 0% then L45330     /* Delete Entire Order        */
           pf$(1) = "( 1)EXIT Delete                                   "&~
                    "             (13)Instructions"
           pf$(2) = "(RETURN)DELETE Entire Order                       "&~
                    "             (15)Print Screen"
           pf$(3) = " "

           pfkey$ = hex(0001ffffffffffffffffffff0dff0fffffffffff)
           return

                                         /* Delete One Line Item       */
L45330:    pf$(1) = "( 1)EXIT Delete                                   "&~
                    "             (13)Instructions"
           pf$(2) = "(RETURN)DELETE Line Item                          "&~
                    "             (15)Print Screen"
           pf$(3) = " "

           pfkey$ = hex(0001ffffffffffffffffffff0dff0fffffffffff)
           return

        display_corebank
            ret% = 0%
            close ws
            call "CMSLINK" addr(#44, userid$, "R", "CORDSPLY",           ~
                                "        ", "      ", " ", "N",          ~
                                "                        ",              ~
                                "                ",  "N", u3%, ret%)
            return

        create_estimate    /* Create PUTPARM & Link to ESTINPUT */

            call "PUTPARM" addr("E", "LINKFROM", 15%,                    ~
                 "LINKSRCE", "BCKINPUT", 8%,                             ~
                 "CUSTOMER", cuscode$, 9%,                               ~
                 "TERMS   ", terms$        ,  20%,                       ~
                 "REGION  ", region$       ,   4%,                       ~
                 "SHIPTOON", shipto$(1%)   ,  30%,                       ~
                 "SHIPTOTW", shipto$(2%)   ,  30%,                       ~
                 "SHIPTOTH", shipto$(3%)   ,  30%,                       ~
                 "SHIPTOFO", shipto$(4%)   ,  30%,                       ~
                 "SHIPTOFI", shipto$(5%)   ,  30%,                       ~
                 "SHIPTOSI", shipto$(6%)   ,  30%,                       ~
                 "SHPINSTO", shipinstr$(1%),  50%,                       ~
                 "SHPINSTT", shipinstr$(2%),  50%,                       ~
                 "SLESMNON", salesman$(1%) ,   4%,                       ~
                 "SLESMNTW", salesman$(2%) ,   4%,                       ~
                 "SLESMNTH", salesman$(3%) ,   4%, "@")

            call "PUTPARM" addr("E", "LINKFRM1", 12%,                    ~
                 "SLECOMON", comm$(1%)     ,   3%,                       ~
                 "SLECOMTW", comm$(2%)     ,   3%,                       ~
                 "SLECOMTH", comm$(3%)     ,   3%,                       ~
                 "PORFQ   ", po$           ,  16%,                       ~
                 "CUSDESCR", cusdescr$     ,  30%,                       ~
                 "REQUESTR", requestr$     ,  20%,                       ~
                 "PARTNUMB", part$(c%)     ,  25%,                       ~
                 "PARTDESC", descr$(c%)    ,  32%,                       ~
                 "CATEGORY", cat$(c%)      ,   4%,                       ~
                 "STKUOM  ", stkuom$(c%)   ,   4%,                       ~
                 "PARTTYPE", parttype$(c%) ,   3%,                       ~
                 "PROJECT ", project$(c%)  ,   8%, "@")

            ret% = 0%
            close ws
            call "CMSLINK" addr(#44, userid$, "R", "ESTINPUT",           ~
                                "        ", "      ", " ", "N",          ~
                                "                        ",              ~
                                "                ",  "N", u3%, ret%)

*       Return from linked program, can we get any data
            readkey$ = "ESTINPUT-BCKINPUT" & userid$
            call "READ101" (#2, readkey$, sysf2%)
                if sysf2% = 0% then return
            get #2 using L45650, est$(c%)
L45650:         FMT POS(30), CH(8)
            delete #2

            if so_cut$ <> "Y" then return
            askmsg$(1%) = "A New Estimate Has Just Been Created."
            askmsg$(2%) = "Do you want to 'CUTOVER' the Estimate to the"&~
                          " Regular CMS Files?"
            askmsg$(3%) = "PF12 to 'CUTOVER', Return to Continue withou"&~
                          "t 'CUTOVER'."
            u3% = 0%
            call "ASKUSER" (u3%, "CUTOVER the Estimate", askmsg$(1%),    ~
                            askmsg$(2%), askmsg$(3%))
                if u3% = 12% then L45795
            return

L45795: cutover_estimate
            call "PUTPARM" addr("E", "LINKFROM", 2%,                     ~
                 "LINKSRCE", "BCKINPUT", 8%,                             ~
                 "ESTIMATE", est$(c%), 8%, "@")

            ret% = 0%
            close ws
            call "CMSLINK" addr(#44, userid$, "R", "ESTDISPO",           ~
                                "        ", "      ", " ", "N",          ~
                                "                        ",              ~
                                "                ",  "N", u3%, ret%)

            call "READ100" (#23, est$(c%), f1%(23%))
                if f1%(23%) <> 1% then L45960
            get #23 using L45930, est_cutover$(c%), cutover_bom$,         ~
                                 est_rte$(c%)
L45930:         FMT POS(1888), CH(1), 2*CH(3)
            if est_cutover$(c%) = "Y" and cutover_bom$ <> " " then       ~
                                                  bom$(c%) = cutover_bom$
L45960:     return

        REM *************************************************************~
            *           D A T A   S A V E   S C R E E N                 *~
            *-----------------------------------------------------------*~
            * Some last decisions to make and data to fill in.          *~
            *************************************************************

        deffn'106(fieldnr%, edit%)
            str(line2$,62) = "BCKINPUT: " & str(cms2v$,,8)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpf6 : gosub set_currency_description
            inpmessage$ = " "
            on fieldnr%      gosub  L46160,         /* Adj Reason Code  */~
                                    L46160          /* Pick List Print  */

            if fieldnr% = 1% then inpmessage$ = "Enter the Adjustment " &~
                                  "Reason Code or '?' to see a list."
            if fieldnr% = 2% then inpmessage$ = "Place a Non-Blank Cha" &~
                                "racter Next to Documents to be Printed."
            goto L46190
                                    lfac$(fieldnr%) = hex(80) : return
L46160:                             lfac$(fieldnr%) = hex(81) : return
                                    lfac$(fieldnr%) = hex(82) : return

L46190:     accept                                                       ~
               at (01,02), "Sales Order Management: DATA SAVE SUMMARY",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)),   adjmsg$              , ch(24),~
               at (05,30), fac(lfac$( 1)), adjrsn$              , ch(09),~
               at (05,49), fac(hex(8c)),   adjrsndescr$         , ch(30),~
                                                                         ~
               at (06,02), "Print Docs:  Pick List x     BOL x     Acknow~
        ~ledgements x",                                                   ~
               at (06,25), fac(lfac$( 2)), pickprint$(2%)       , ch(01),~
               at (06,35), fac(lfac$( 2)), pickprint$(3%)       , ch(01),~
               at (06,58), fac(lfac$( 2)), pickprint$(1%)       , ch(01),~
                                                                         ~
               at (07,02), "Currency code",                              ~
               at (07,30), fac(hex(8c)),   currency$            , ch(04),~
               at (07,49), fac(hex(8c)),   currdesc$            , ch(32),~
               at (08,49), fac(hex(8c)),   currdesc_2$          , ch(32),~
                                                                         ~
               at (09,30), "----ORDER$----",                             ~
               at (09,48), "--OPEN ORDER--",                             ~
               at (10,02), "Gross Order Amount",                         ~
               at (10,30), fac(hex(84)), torder(1), pic(-###,###,###.00),~
               at (10,48), fac(hex(84)), topen (1), pic(-###,###,###.00),~
               at (11,02), "Line Item Discounts",                        ~
               at (11,30), fac(hex(84)), torder(2), pic(-###,###,###.00),~
               at (11,48), fac(hex(84)), topen (2), pic(-###,###,###.00),~
               at (12,02), "Order Level Discounts",                      ~
               at (12,30), fac(hex(a4)), torder(3), pic(-###,###,###.00),~
               at (12,48), fac(hex(a4)), topen (3), pic(-###,###,###.00),~
               at (13,02), "Net Order Amounts",                          ~
               at (13,30), fac(hex(84)), torder(4), pic(-###,###,###.00),~
               at (13,48), fac(hex(84)), topen (4), pic(-###,###,###.00),~
                                                                         ~
               at (15,02), "Number of Line Items",                       ~
               at (15,30), fac(hex(84)), tlines%(1),           pic(###0),~
               at (16,02), "Lines Flagged for Delete",                   ~
               at (16,30), fac(hex(84)), tlines%(2),           pic(###0),~
                                                                         ~
               at (18,02), "A/R shown is Statutory.",                    ~
               at (18,29), "Bill-to:",                                   ~
               at (18,38), fac(hex(84))  , billto$              , ch(09),~
               at (18,48), fac(hex(84))  , billtoname$          , ch(32),~
                                                                         ~
               at (19,02), "Credit A/R Balance",                         ~
               at (19,21), fac(hex(84))  , ar(1)     , pic(-###,###,###),~
               at (19,35), "Open Orders",                                ~
               at (19,47), fac(hex(84))  , oo(1)     , pic(-###,###,###),~
               at (19,61), "TOTAL:",                                     ~
               at (19,68), fac(hex(84))  , totalar1  , pic(-###,###,###),~
               at (20,06), fac(hex(84))  , crmsg$               , ch(68),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L46780
                gosub call_customer_credit
                goto L46190

L46780:        if keyhit% <> 13 then L46810
                  call "MANUAL" ("BCKINPUT") : goto L46190

L46810:        if keyhit% <> 15 then L46830
                  call "PRNTSCRN" : goto L46190

L46830:        if keyhit% <> 24 then L46840
                  crhold$ = " " : crmsg$ = " "
                  str(pf$(1), 26, 24) = " " : str(pfkey$,18, 1) = hex(ff)
                  goto L46190

L46840:         return

        setpf6
        if edit% = 2% then L46990         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "

           pfkey$ = hex(01ffff04ffffffffffffffff0dff0fffffffff1a00)
           if fieldnr% <> 1% then L46970
              str(pf$(2), 18, 18) = " " : str(pfkey$, 4, 1) = hex(ff)
L46970:    return

L46990:  if fieldnr% > 0% then L47100     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over            (24)Override Credit Hold "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Line Items                                     "&~
                    "             (15)Print Screen"
           pf$(3) = "(9)Header Screen                         (26)Custo"&~
                    "mer Credit   (16)Save Order  "
           pfkey$ = hex(0102ffffffffffff09ffffff0dff0f10ff18ff1a00)
           if override_crhld$ = "Y" and crhold$ = "H" and                ~
                                              how_held$ <> "M" then L47070
              str(pf$(1), 26, 24) = " " : str(pfkey$,18, 1) = hex(ff)
L47070:    return

                                         /* Edit Mode- Field Enabled   */
L47100:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = " "

           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        set_currency_description
            currdesc_2$ = " "
            if currency$ = statutory$ then return
            call "CONVERT" (convunt, 2.7, str(currdesc_2$, 1,10))
            currdesc_2$ = currdesc_2$ & " " & currency$
            currdesc_2$ = currdesc_2$ & "/" & statutory$
            return

L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
            if soonfile% = 0% then L50035    /* No Mod flag if new order*/
            if edit% = 2% then modfld% = 1% /* Fld enabled in EDITMODE */
L50035:           errormsg$ = " "
                  on fieldnr% goto  L50100,         /* Customer Code    */~
                                    L50150,         /* Sales Order #    */~
                                    L50275,         /* Purchase Order # */~
                                    L50307,         /* Export Order flag*/~
                                    L50315,         /* Ship-to          */~
                                    L50340,         /* Sold-to          */~
                                    L50360,         /* Store Code       */~
                                    L50395,         /* Order Date       */~
                                    L50415,         /* Cancellation Date*/~
                                    L50455,         /* Default Due Date */~
                                    L50495          /* Default Ship Date*/
                  return

L50100
*        Customer Code                         CUSCODE$
            if cuscode$ = " " and so$ <> " " then L50165
              if so$ = " " then L50110
              gosub check_hist
              if errormsg$ <> " " then return
L50110:         cusdescr$ = hex(06) & "Select Customer Code"
                call "GETCODE" (#01, cuscode$, cusdescr$, 0%, 1, f1%(1))
                if f1%(1) = 1% then L50130
                     errormsg$ = "Customer not on file." : return
L50130:         get #01 using L50134, soldto$(), pm_cus_so$, pm_cus_inv$, ~
                                     dfltacks$, shipto$(), billto$,      ~
                                     custype$, currency$, parent$
L50134:              FMT XX(39), 6*CH(30), POS(226), 2*CH(01), POS(238), ~
                                    CH(1), POS(253), 6*CH(30), POS(780), ~
                         CH(9), POS(1023), CH(2), POS(1045), CH(4), CH(9)
                if curr$ = "Y" then goto L50139
                    currency$, currdesc$ = " " : goto L50140
L50139:         call "DESCRIBE" (#40, currency$, currdesc$, 0%, f1%(40))
L50140:         gosub set_pm_flags
                if so$ = " " then return

L50150
*        Sales Order Number                    SO$
            if so$ <> " " then L50165
                if soassgn$ = "m" then                                   ~
                     errormsg$ = "S.O. Number Can't Be Blank."
                if errormsg$ <> " " then return else L50231
L50165:     if so$ <> "?" then L50204
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
                                 " ", #05, descr())
                if f1%(5) = 1% then L50198
                     errormsg$ = hex(00) : return
L50198:         cuscode$ = str(readkey$,,9)
                so$      = str(readkey$,10)
L50204:     gosub check_hist
            if errormsg$ <> " " then return
            call "BCKARISB" (so$, errormsg$) /* Check for SO in ARI SSN */
            if errormsg$ <> " " then return
            gosub load_data
            if crhold$ = "C" then L50243
L50216:     if errormsg$ <> " " then return
                if soonfile% = 0% then L50231
                     call "READ100" (#01, cuscode$, f1%(1))
                     get #01 using L50224, dfltacks$
L50224:                   FMT POS(238), CH(1)
                     return clear all
                     goto edithdr1
L50231:         if cuscode$ = " " then L50110 /* Go get customer        */
                     call "CUSCHECK" (cuscode$, cr_ar_only$, #01, ret%)
                     if ret% <> 0% then return
                          return clear all
                          goto startover2
L50243:     u3% = 2%
            call "ASKUSER" (u3%, "** CANCELLED ORDER **", "* Press PF10"&~
                " to Re-activate Order", "- Open Quantities MIGHT need "&~
                "to be Changed -", "* Press PF16 to exit")
            if u3% <> 16% and u3% <> 10% then L50243
            if u3% = 16% then L29922
              crhold$ = " "
              if maxlines% = 0% then L50272
                for c% = 1 to maxlines%
                  openqty(c%) = order(c%)
                next c%
L50272:       goto L50216

L50275
*        PURCHASE ORDER NUMBER                 PO$
            if po$ <> " " then L50298
                get #01 using L50290, poreqd$
L50290:              FMT POS(1020), CH(1)
                if poreqd$ <> "Y" then L50298
                     errormsg$ = "PO is required for this Customer."
                     return
L50298
*        Now test for duplicate PO for this customer
            if po$ = " " then return
                call "BCKDUPPO" (cuscode$, so$, po$, errormsg$)
                return

L50307: REM Validate Export Order flag          EXPORT$
            if export$ = " " then export$ = "N"
            if export$ = "Y" or export$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N' for Export Order flag."
            return

L50315
*        Ship-to                               SHIPTO$
            if str(shipto$()) <> " " then return
                errormsg$ = "Ship-to can't be blank"
                return

L50340
*        Sold-to                               SOLDTO$
            if soldto$(1) = " " then str(soldto$()) = " "
            return

L50360
*        Store Code                            STORE$
            storedescr$ = hex(06) & "Select Store."
            call "GETCODE" (#12, store$, storedescr$, 0%, 0, f1%(12))
            if f1%(12) = 1% then L50386
                errormsg$ = "Invalid Store Code."
                return
L50386:     defstore$ = store$
            return

L50395
*        Order Date                            ORDERDATE$
            call "DATEOK" (orderdate$, u3%, errormsg$)
            return

L50415
*        Cancellation Date                     CANCELDATE$
            if canceldate$ = " " or canceldate$ = blankdate$ then return
                call "DATEOK" (canceldate$, u3%, errormsg$)
                if errormsg$ <> " " then return
                     testdate$ = canceldate$
                     gosub order_date_check
                     return

L50455
*        Default Due Date                      DFLTDUE$
            if (dfltdue$ = " " or dfltdue$ = blankdate$) and copy% = 0% ~
                  then return
                call "DATEOK" (dfltdue$, u3%, errormsg$)
                if errormsg$ <> " " then return
                     testdate$ = dfltdue$
                     gosub order_date_check
                     return

L50495
*        Default Req'd Ship Date               DFLTSHIP$
            if (dfltship$ = " " or dfltship$ = blankdate$) and copy% = 0% ~
                  then return
                call "SPCSMASH" (dfltship$)
                if str(dfltship$,,1) <> "-" then L50570
                     if dfltdue$ = " " or dfltdue$ = blankdate$ then L50535
                     convert dfltship$ to temp%, data goto L50530
                     goto L50550
L50530:                   errormsg$ = "Invalid days offset." : return
L50535:                   errormsg$ = "Can't use offset if Due Date" &   ~
                                      " is left blank."
                          return
L50550:              call "DATUNFMT" (dfltdue$)
                     call "DATE" addr("G+", str(dfltdue$,,6), temp%,     ~
                                      str(dfltship$,,6), u3%)
                     call "DATEFMT" (dfltdue$)
L50570:         call "DATEOK" (dfltship$, u3%, errormsg$)
                if errormsg$ <> " " then return
                     testdate$ = dfltship$
                     gosub order_date_check
                     if errormsg$ <> " " then return
                          call "DATUNFMT" (dfltship$)
                          call "DATE" addr("GD", str(dfltship$,,6),      ~
                                                          weekdayh$, u3%)
                          call "DATEFMT" (dfltship$)
                          if weekdayh$ <> "SATURDAY" and                 ~
                             weekdayh$ <> "SUNDAY"   then return
                               u3% = 2%
                               call "ASKUSER" (u3%, "WEEKEND",           ~
                                 "The Ship Date falls on a weekend.",    ~
                                 "Press PF-1 to Accept, -OR-",           ~
                                 "Press (RETURN) to re-enter.")
                               if u3% = 1% then return
                                         errormsg$ = hex(00)
                                         return

*        See if order is in history files
        check_hist
            readkey$  = str(so$) & hex(00)
            call "PLOWNEXT" (#30, readkey$, 16%, f1%(30))
              if f1%(30) = 0% then L50910
                errormsg$ = "Sales Order number " & so$ & " is in the" & ~
                            " History Files."
L50910:         return

L51000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%)
            if soonfile% = 0% then L51035    /* No Mod flag if new order*/
            if edit% = 2% then modfld% = 1% /* Fld enabled in EDITMODE */
L51035:           errormsg$ = " "
                  on fieldnr% goto  L51105,         /* How Ship         */~
                                         ,         /* FOB              */~
                                         ,         /* Ship Instruct    */~
                                    L51150,         /* Price Code Deflt */~
                                    L51195,         /* Order Discount % */~
                                    L51240,         /* Payment Terms    */~
                                    L51275,         /* Region Code      */~
                                    L51325,         /* Salesman / Split */~
                                    L51520,         /* PM Srchrg SO Flag*/~
                                    L51570,         /* PM Srchrg INV Flg*/~
                                    L51435,         /* Sales Acct Deflt */~
                                    L51480          /* Sales Discs Acct */
                  return

L51105
*        How Ship                              HOWSHIP$
            return

*        FOB                                   FOB$
            return

*        Shipping Instructions                 SHIPINSTR$()
            return

L51150
*        Price Code Default                    PC$
            if (pc$ >= "A" and pc$ <= "Q") or                            ~
               (pc$ >= "0" and pc$ <= "8") then L51175
                errormsg$ = "Valid Price Codes are 'A'-'Q' and '0'-'8'"
                return
L51175:     readkey$ = "PRICECODE" & pc$
            call "DESCRIBE" (#08, readkey$, pcdescr$, 0%, f1%(8))
            return

L51195
*        Order Discount Percent                ORDERDISC$
            orderdisc = 0
            if orderdisc$ = " " then orderdisc$ = "0"
            convert orderdisc$ to orderdisc, data goto L51215 : goto L51220
L51215:         errormsg$ = "Order Discount range: -25 to +100%" : return
L51220:     if orderdisc < -25 or orderdisc >= 100 then L51215
            call "CONVERT" (orderdisc, 2.2, orderdisc$)
            return

L51240
*        Payment Terms (Code)                  TERMS$
            if terms$ = " " then L51260
                call "GETCODE" (#21, terms$, termsdescr$, 0%, 0, f1%(21))
                if f1%(21) = 1% then return
L51260:              termsdescr$ = "0% 0 Days, Net 0 Days"
                     return

L51275
*        Region Code                           REGION$
            regiondescr$ = " " : if region$ = " " then return
                regiondescr$ = hex(06) & "Select Sales Region"
                readkey$ = "REGIONS  " & region$
                call "PLOWCODE" (#08,readkey$,regiondescr$,9%,.3,f1%(8))
                if f1%(8) = 1% then L51310
                     errormsg$ = "Invalid Sales Region Code." : return
L51310:         region$ = str(readkey$,10)
                return

L51325
*        Salesman Code / Split %               SALESMAN$()
            total% = 0%
            for i% = 1% to 3%
                if salesman$(i%) <> " " then L51355
                     salesmandescr$(i%), comm$(i%) = " "
                     comm%(i%) = 0%  :  goto L51410
L51355:         call "GETCODE" (#07, salesman$(i%), salesmandescr$(i%),  ~
                                                       0%, 0.30, f1%(7))
                if f1%(7) = 1% then L51380
                     errormsg$ = "Salesman Code " & salesman$(i%) &      ~
                                 " not on file."  :  return
L51380:         if comm$(i%) = " " then comm$(i%) = "0"
                convert comm$(i%) to comm%(i%), data goto L51395
                goto L51400
L51395:              errormsg$ = "Commission % must be 0 - 100." : return
L51400:         total% = total% + comm%(i%)
                convert comm%(i%) to comm$(i%), pic(##0)
L51410:     next i%
            if total% <= 100% then return
                errormsg$ = "Total Commission Splits exceed 100%."
                return

L51435
*        Sales Account Default                 DFLTSALES$
            dfltsalesdescr$ = " " : if dfltsales$ = " " then return
                dfltsalesdescr$ = hex(06) & "Select Sales Acct Default."
                call "GETCODE" (#03, dfltsales$, dfltsalesdescr$, 0%,    ~
                                                               0, f1%(3))
                if f1%(3) = 1% then return
                     errormsg$ = "Invalid Sales Account"
                     return

L51480
*        Sales Discounts Account               DISCACCT$
            discacctdescr$ = hex(06) & "Select Sales Discounts Account."
            call "GETCODE" (#03, discacct$, discacctdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Discounts Account"
                return

L51520
*        Test Data for Precious Metal Surcharge at SO Entry
            if enabled% = 0% then return
            if pos("YN" = pm_so$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N' for PM SO Surcharge"
                return

L51570
*        Test Data for Precious Metal Surcharge at Invoice Update
            if enabled% = 0% then return
            if pos("YN" = pm_inv$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N' for PM INV Surcharge"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

            deffn'153(fieldnr%, edit%)
            if soonfile% = 0% then L52035    /* No Mod flag if new order*/
            if edit% = 2% then modfld% = 1% /* Fld enabled in EDITMODE */
L52035:           errormsg$ = " "
                  on fieldnr% goto  L52125,         /* Estimate Number  */~
                                    L52345,         /* Part Code        */~
                                         ,         /* Part Description */~
                                    L52500,         /* Part Category    */~
                                    L52530,         /* Due Date         */~
                                    L52570,         /* Required Ship    */~
                                    L52675,         /* Stocking UOM     */~
                                    L52735,         /* Order Qty/Alloc  */~
                                    L52810,         /* Open Order/Lot   */~
                                    L52920,         /* Total Qty Shipped*/~
                                    L52965,         /* Currency code    */~
                                    L53055,         /* Pricing UOM      */~
                                    L53115,         /* Conversion       */~
                                    L53175,         /* Unit Price       */~
                                    L53235,         /* Line Item Disc   */~
                                    L53290          /* Line Taxable?    */
                  return
L52125
*        Estimate Number
            if est$(c%) = " " then return
            readkey$ = str(cuscode$) & est$(c%)
            plowhdr$() = "  Estimate   Part Number               Customer~
        ~ Contact      R.F.Q. Number"
            msg$ = hex(06) & "Select Estimate To Generate Order From"
            mat descr = zer : mat incl = zer : incl$() = " "
            incl(1) = -1440.06      /* Customer Decision Date <> " " */
*          INCL(2) = -1854.01 : INCL$(2) = "0" /* Quan Element <> 0 */
            incl(3) =  1467.16      /* S.O. Field Must = " " */
            descr(1) = 48.25  : descr(2) = 1
            descr(3) = 73.20  : descr(4) = 27
            descr(5) = 416.16 : descr(6) = 49
            call "PLOWCODE" (#23, readkey$, msg$, 9009%, 3.01, f1%(23),  ~
                plowhdr$(), 0, 0, incl(), incl$(), "N", "N", #02, descr())
                if f1%(23) = 1% then L52215
                     errormsg$ = "Unknown or Unfinalized Estimate Number"
                     return
L52215:     est$(c%) = str(readkey$,10)
            mat tlines% = zer
            search est$() = str(est$(c%),,8) to tlines%() step 8
                if tlines%(2%) = 0% then L52245
                askmsg$(1%) = " "
                askmsg$(2%) = "Estimate already appears on this Order."
                askmsg$(3%) = "Return to Continue or PF1 to skip this " &~
                              "Estimate."
                u3% = 0%
                call "ASKUSER" (u3%, "ESTIMATE ALREADY REFERENCED",      ~
                                askmsg$(1%), askmsg$(2%), askmsg$(3%))
                if u3% <> 1% then L52245
                     est$(c%) = " "
                     return
L52245:     REM Search Buffer...
            call "PLOWNEXT" (#10, hex(00), 0%, f1%(10))
                if f1%(10) = 0 then L52295 : goto L52265
L52260:     read #10, eod goto L52295
L52265:     get #10, using L52270, temp$
L52270:     FMT POS(255), CH(8)
            if temp$ <> est$(c%) then L52260
              errormsg$="Estimate appears on order: " & str(key(#10),,16%)
              return

L52295:     REM All clear to read data...
            get #23, using L36190, part$(c%), project$(c%), descr$(c%),   ~
                     cat$(c%), stkuom$(c%), eqty(), edsc(), eprc(),      ~
                     edat$(), temp$, est_cutover$(c%), cutover_bom$,     ~
                     est_rte$(c%)
            bom$(c%) = cutover_bom$
*          CONVERT TEMP$ TO EPTR%, DATA GOTO 52335
*          IF EPTR% = 0% THEN 52335
*          IF DFLTDUE$= " " THEN DFLTDUE$= EDAT$(EPTR%) ELSE GOTO 52330
*              CALL "DATEFMT" (DFLTDUE$)
*          CALL "CONVERT" (EQTY(EPTR%), -0.2, ORDER$)
            return

L52345
*        Part Code                             PART$()
            if keyhit% = 22% or lastkey% = 23% then L52436  /* Xref Part */
            nonstockmsg$(c%), catdescr$ = " "
            if est$(c%) = " " then descr$(c%), cat$(c%) = " "
            if part$(c%) = " " then L52364
                call "READ100" (#04, part$(c%), f1%(4%))
                if f1%(4%) = 1% then L52378
                     call "HNYGREF" (part$(c%), #15, #04, f1%(4%))
                     if f1%(4%) = 0% and est$(c%) <> " " then L52368
                     if f1%(4%) = 0% then L52364
                          call "READ100" (#04, part$(c%), f1%(4%))
                          if f1%(4%) = 1% then L52378
L52364:     call "GETCODE" (#04, part$(c%), " ", 0%, 0, f1%(4))
            if f1%(4%) = 1% then L52376
L52368:         if part$(c%) <> " " then L52374
                     errormsg$ = "Part Number CANNOT Be Blank"
                     return
L52374:         nonstockmsg$(c%) = "Non-Stock Part"
                refpart$(c%) = "** No Cross Reference **" : return
L52376:     /* If this is a Generic part then make sure we have an SO#  */
L52378:      get #04 using L52380, cat$(c%), partflag$, parttype$(c%), ss
L52380:          FMT POS(90), CH(4), POS(166), CH(4), POS(180), CH(3),   ~
                     POS(318), PD(14,4)

        REM Get Customer Part For This CMS Part ...
            if refpart$(c%) = " " then                                   ~
                   refpart$(c%) = "** No Cross Reference **"

*          CALL "PTXREFSB" (2%, "C", CUSCODE$, REFPART$(C%),            ~
*               REFDESC$(C%), PART$(C%), DESCR$(C%), #4, RET%)
*          IF RET% = 1% THEN REFFLAG% = 1% ELSE                         ~
*              REFPART$(C%) = "** No Cross Reference **"
*          IF RET% = 1% THEN REFTYPE$(C%) = "C"

L52396:     if parttype$(c%) = "000" and so$ = " " then                  ~
                   call "BCKNEXT" (#12, #06, #10, #30, #18, store$, so$)
            if partflag$ = " " then L52420
                u3% = 2%
                call "ASKUSER" (u3%, "PART FLAGGED",                     ~
                          "This Part is flagged as '" & partflag$ & "'.",~
                          "Press PF-16 to continue and use Part,",       ~
                          "Or press RETURN to re-enter Part Number.")
                if u3% = 16% then L52420
L52414:            errormsg$ = "Re-enter Part Number"
                   return

L52420:     call "ARIEXTRA" (cuscode$, part$(c%), " ", temp%, #2)
            if temp% <= 0% then return
            if c% + total_e_lines% + temp% <= 100% then L52430
               gosub extra_append_conflict
               if u3% = 0% then L52414
L52430:           e_lines%(c%) = temp%
                  total_e_lines% = total_e_lines% + temp%
                  return

L52436: REM Get CMS Part For This Cross Referenced Part...
            refpart$(c%) = part$(c%)
            if lastkey% = 23% then L52448
        REM Customer Cross Reference...
            call "PTXREFSB" (1%, "C", cuscode$, refpart$(c%),            ~
                 refdesc$(c%), part$(c%), descr$(c%), #4, ret%)
            if ret% = 1% then reftype$(c%) = "C"
            if ret% = 1% then L52473
                if part$(c%) = " " then                                  ~
                   errormsg$ = "Part Number CANNOT Be Blank"  else       ~
                   errormsg$ = "No Cross Ref Part For This Customer Part"
                return
L52448: REM Manufacturer's Cross Reference...
            if mfgcode$ = "?" then mfgcode$ = " "
            mfgdescr$ = hex(06) & "Select Manufacturer's Code"
            readkey$ = "MFG CODES" & mfgcode$
            call "PLOWCODE" (#8, readkey$, mfgdescr$, 9%, .3, f1%(8%))
               if f1%(8%) = 1% then L52456
                   errormsg$ = "Invalid Manufacturer's Code"
                   return
L52456:     mfgcode$ = str(readkey$,10%)
            call "PTXREFSB" (1%, "M", mfgcode$, refpart$(c%),            ~
                 refdesc$(c%), part$(c%), descr$(c%), #4, ret%)
            if ret% = 1% then reftype$(c%) = "M"
            if ret% = 1% then L52467
                if part$(c%) = " " then                                  ~
                   errormsg$ = "Part Number CANNOT Be Blank"  else       ~
                   errormsg$ = "No Cross Ref Part For This Mfg Part"
                   return
L52467:     call "GETCODE" (#4, part$(c%), " ", 0%, 0, f1%(4%))
            if f1%(4%) = 1% then L52473
                if part$(c%) = " " then                                  ~
                    errormsg$ = "Part Number CANNOT Be Blank"  else      ~
                    errormsg$ = "Part No Longer Exists In HNYMASTR File"
                    return
L52473:     get #4 using L52474, cat$(c%), partflag$, parttype$(c%) , ss
L52474:         FMT POS(90), CH(4), POS(166), CH(4), POS(180), CH(3),    ~
                    POS(318), PD(14,4)
            refflag% = 1% : lastkey% = 0%
            goto L52396

*        Part Description                      DESCR$()
            return

L52500
*        Part Category                         CAT$()
            catdescr$ = hex(06) & "Select Part Category"
            call "GETCODE" (#11, cat$(c%), catdescr$, 0%, 0, f1%(11))
            if f1%(11) = 1% or cat$(c%) = " " then return
                errormsg$ = "Category Code not on file"  :  return

L52530
*        Due Date                              DUEDATE$()
            call "DATEOK" (duedate$(c%), u3%, errormsg$)
            if errormsg$ <> " " then return
                testdate$ = duedate$(c%)
                gosub order_date_check : if errormsg$ <> " " then return
                   if origdue$(c%) = " " or origdue$(c%) = blankdate$ ~
                      then origdue$(c%) = duedate$(c%)
                   return

L52570
*        Required Ship Date                    SHIPDATE$()
            call "SPCSMASH" (shipdate$(c%))
            if str(shipdate$(c%),,1) <> "-" then L52620
                convert shipdate$(c%) to temp%, data goto L52595
                goto L52600
L52595:              errormsg$ = "Invalid days offset." : return
L52600:         call "DATUNFMT" (duedate$(c%))
                call "DATE" addr("G+", str(duedate$ (c%),,6), temp%,     ~
                                       str(shipdate$(c%),,6), u3%)
                call "DATEFMT" (duedate$(c%))
L52620:     call "DATEOK"  (shipdate$(c%), u3%, errormsg$)
            if errormsg$ <> " " then return
                testdate$ = shipdate$(c%) : gosub order_date_check
                if errormsg$ <> " " then return
                     call "DATUNFMT" (shipdate$(c%))
                     call "DATE" addr("GD", str(shipdate$(c%),,6),       ~
                                                          weekday$, u3%)
                     call "DATEFMT"  (shipdate$(c%))
                     if mlq_date_check% = 1% then return
                     gosub get_atc
*        Get Options for this part (Input Mode Only)
            if edit% = 2% then return else gosub options  :  return

L52675
*        Stocking Unit Of Measure              STKUOM$()
            stkuomdescr$ = " "  :  if uom% = 1% then L52700
                if stkuom$(c%) <> " " then return
                     errormsg$ = "Stocking Unit of Measure Can't Be" &   ~
                                 " Blank"  :  return
L52700:     stkuomdescr$ = hex(06) & "Select Stocking UOM"
            readkey$ = "UOM      " & stkuom$(c%)
            call "PLOWCODE" (#08,readkey$,stkuomdescr$,9%,0.30,f1%(8))
            if f1%(8) = 1% then stkuom$(c%) = str(readkey$,10)           ~
                           else errormsg$   = "Stocking UOM not on file"
            return

L52735
*        Original Order Quantity / Alloc Flag  ORDER$/ ALLOC$()
            if order$ = " " then order$ = "0"
            q$ = order$  :  gosub uom_trans  :  order$ = q$
            convert order$ to order(c%), data goto L52755 : goto L52760
L52755:         errormsg$ = "Invalid Order Quantity" : return
L52760:     if order(c%) >= 0 then L52770
                errormsg$ = "Order Quantity can't be negative" : return
L52770:     call "CONVERT" (order(c%), 2.2, order$)
*        Apply Part Code Min Quantity and Min Increment to Order Quantity
            minsoqty, minsoinc = 0
            if nonstockmsg$(c%) <> " " then L52785
            call "READ100" (#04, part$(c%), f1%(4))
                if f1%(4) <> 1% then L52778
            get #04 using L52777, minsoqty, minsoinc
L52777:         FMT POS(706), 2*PD(14,4)
L52778:     call "BCKMINSB" (order(c%), minsoqty, minsoinc, u3%)
                if u3% =  16% then goto L52785
                if u3% <>  1% then goto L52778
                    errormsg$ = hex(00)
                    return
L52785:     if pos("NACZP" = alloc$(c%)) <> 0% then L52800
                call "GETCODE" (#33, alloc$(c%), " ", 0%, 0.32, f1%(33%))
                if f1%(33%) = 1% then L52800
                    errormsg$ = "Allocation method must be 'A', 'C', " & ~
                                "'N', 'P', or 'Z'."
                    return
L52800:     gosub describe_allocation  :  return

L52810
*        Open Order Quantity / Lot Number      OPENQTY$ / LOT$()
            if openqty$ = " " then openqty$ = "0"
            q$ = openqty$  :  gosub uom_trans  :  openqty$ = q$
            convert openqty$ to openqty(c%), data goto L52830 : goto L52835
L52830:         errormsg$ = "Invalid Open Order Quantity" : return
L52835:     if openqty(c%) >= 0% then L52850
                errormsg$ = "Open Order Quantity can't be negative"
                return
L52850:     if openqty(c%) >= qtyschld(c%) then L52870
                errormsg$ = "Cannot reduce Open Quantity less than" &    ~
                            " Quantity Scheduled."
                return
L52870:     call "CONVERT" (openqty(c%), 2.2, openqty$)
            if edit% = 2% then gosub pricing_stuff
            if edit% = 2% then gosub L53282
          /* Test Lot Number for syntax and uniquenss      */
            if lot$(c%) <> " " then                                      ~
                call "LOTVALID" (part$(c%), store$, lot$(c%),            ~
                    #02, #04, #13, errormsg$)
                if errormsg$ <> " " then return
                call "LOTUNQUE" (part$(), lot$(), c%, #02, errormsg$)
*          IF OPENQTY(C%) <= SS OR SS = 0 THEN RETURN
*             KH% = 2
*             CALL "ASKUSER" (KH%, "*** WARNING ***", "Open Order Qty Ex~
*       ceeds Safety Stock!", " ", "Press any key to acknowledge")
            return

L52920
*        Total Quantity Shipped                SHIP$
            if ship$ = " " then ship$ = "0"
            q$ = ship$  :   gosub uom_trans  :  ship$ = q$
            convert ship$ to ship(c%), data goto L52940 : goto L52945
L52940:         errormsg$ = "Invalid Shipped Quantity" : return
L52945:     if ship(c%) >= 0% then L52955
                errormsg$ = "Shipped Quantity can't be negative" : return
L52955:     call "CONVERT" (ship(c%), 2.2, ship$)  :  return

L52965
*        Currency code                       CURRENCY$
            if maxlines% = 0% and curr$ = "Y" and edit% = 1% then L52970
                if enabled% = 0% then return
L52970:     if currency$ = " " then currency$ = statutory$
            call "GETCODE" (#40, currency$, currdesc$, 0%, 0, f1%(40))
            if f1%(40) <> 0% then goto L52987
                errormsg$ = "Invalid Currency code.  Try again." : return
L52987:     convdate$ = " " : conveqv, convunt = 1
            if currency$ = statutory$ then goto L53030
            call "DATREVRS" ( orderdate$, rev_date$, errormsg$ )
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#42, currkey$, 5%, f1%(42))
            if f1%(42) <> 0% then goto L53026
                errormsg$ = "Invalid Currency Code for this transaction."
                return
L53026:     get #42 using L53027, convdate$, conveqv, convunt
L53027:         FMT POS(12), CH(6), 2*PD(14,7)
L53030:     if edit% <> 2% then return
                gosub L22485 /* CPRASSGN gets price */
                errormsg$ = " "
                return

L53055
*        Pricing Unit of Measure               PRICEUOM$()
            priceuomdescr$ = " "  :  if uom% = 1% then L53080
                if priceuom$(c%) <> " " then return
                     errormsg$ = "Pricing Unit of Measure can't be"  &   ~
                                 " blank"  :  return
L53080:     priceuomdescr$ = hex(06) & "Select Pricing UOM"
            readkey$ = "UOM      " & priceuom$(c%)
            call "PLOWCODE" (#08,readkey$,priceuomdescr$,9%,0.30,f1%(8))
            if f1%(8) = 1% then priceuom$(c%) = str(readkey$,10)         ~
                           else errormsg$    = "Pricing UOM not on file"
            return

L53115
*        Conversion Pricing to Stocking        CONV$
            if priceuom$(c%) = stkuom$(c%) then conv$ = "1"
            convert conv$ to conv(c%), data goto L53130 : goto L53135
L53130:         errormsg$ = "Invalid Conversion Factor Entry" : return
L53135:     conv(c%) = round(conv(c%), 7)  :  if conv(c%) > 0% then L53155
                errormsg$ = "Conversion Factor cannot be equal to or" &  ~
                            " less than zero."
                return
L53155:     call "CONVERT" (conv(c%), 7.7, conv$)
            if edit% = 2% then gosub pricing_stuff
            return

L53175
*        Unit Price                            PRICE$
            if price$ = " " then price$ = "0"
            convert price$ to price(c%), data goto L53190 : goto L53195
L53190:         errormsg$ = "Invalid Unit Price" : return
L53195:     price(c%) = round(price(c%), 4)
            if price(c%) >= 0% then L53215
                errormsg$ = "Unit Price can't be less than zero."
                return
L53215:     call "CONVERT" (price(c%), 4.4, price$)
            if edit% = 2% then gosub pricing_stuff
            if edit% = 2% then gosub L53282
            return

L53235
*        Line Item Discount %                  LINEDISC$
            if linedisc$ = " " then linedisc$ = "0"
            convert linedisc$ to linedisc(c%), data goto L53250: goto L53255
L53250:         errormsg$ = "Invalid Discount %" : return
L53255:     linedisc(c%) = round(linedisc(c%), 2)
            if linedisc(c%) > -50% and linedisc(c%) <= 100 then L53275
                errormsg$ = "Discount must be between -50 and 100%."
                return
L53275:     call "CONVERT" (linedisc(c%), 2.2, linedisc$)
            gosub pricing_stuff
L53282
*        Now test for big extension
            if ext < 1e9 then return
                errormsg$ = "Extention is TOO large, please "         &  ~
                            "reduce Order Quantity."
                if edit% = 1% then fieldnr% = 8% else fieldnr% = 9%
                return

L53290
*        Line Taxable? (Y/N)                   TAXABLE$()
            if taxable$(c%) = "Y" or taxable$(c%) = "N" then return
                errormsg$ = "Enter 'Y' or 'N'" : return

        uom_trans  /* Translate entry from UOM entered to Stocking UOM */
            p% = pos(q$ = "/") : if p% = 0% then return
            if p% < 10% then L53330
                errormsg$ = "Invalid specification for UOM" : return
L53330:     u$ = str(q$,p%+1%) : q$ = str(q$,,p%-1%)
            convert q$ to q, data goto L53340  :  goto L53345
L53340:         errormsg$ = "Invalid entry for Quantity" : return
L53345:     readkey$ = "UOMCONV  " & str(u$,,4) & "-" & stkuom$(c%)
            call "READ100" (#08, readkey$, f1%(8))
            if f1%(8) = 1% then L53370
L53360:         errormsg$ = "Conversion factor invalid for UOM " & u$
                return
L53370:     get #08 using L53375, c$
L53375:         FMT XX(24), CH(10)
            convert c$ to c, data goto L53360
            q = round(q*c, 4)  :  call "CONVERT" (q, 4.4, str(q$,,10))
            return

L54000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4.                      *~
            *************************************************************

        deffn'154(fieldnr%)
            if soonfile% = 0% then L54070    /* No Mod flag if new order*/
            if edit% = 2% then modfld% = 1% /* Fld enabled in EDITMODE */
L54070:     errormsg$ = " "
            on fieldnr%       goto  L54170,         /* P.O. Item        */~
                                    L54200,         /* Planning Priority*/~
                                    L54250,         /* Demand Type      */~
                                    L54300,         /* Project Number   */~
                                    L54430,         /* Sales Distr. Acct*/~
                                    L54510,         /* Sales Discs Acct */~
                                    L54590,         /* Bom Id.          */~
                                    L54702          /* Shipping Priority*/
                  return

L54170
*        P.O. Item                             ITEM$()
            return

L54200
*        Planning Priority Code                PRIORITY$()
            if priority$(c%) >= "A" and priority$(c%) <= "Z" then return
                errormsg$ = "Enter 'A' - 'Z'."
                return

L54250
*        Planning Demand Type                  DEMTYPE$()
            if demtype$(c%) = "1" or demtype$(c%) = "2" then return
                errormsg$ = "Planning Demand type must be '1' or '2'"
                return

L54300
*        Project Number                        PROJECT$()
            if project$(c%) = " " then return
            call "GETCODE"(#14, project$(c%), projectdescr$, 0%, 0,      ~
                                                                 f1%(14))
            if f1%(14) = 1% then L54360
                errormsg$ = "Project not on file." : return
L54360:     get #14, using L54370, testdate$
L54370:         FMT XX(44), CH(06)
            if testdate$ = " " or testdate$ = blankdate$ or ~
               testdate$ > date  then return
                call "DATEFMT" (testdate$)
                errormsg$ = "Project closed on " & testdate$
                return

L54430
*        Sales Distr. Account                  SALESACCT$()
            salesacctdescr$ = hex(06) & "Select Sales Account."
            call "GETCODE" (#03, salesacct$(c%), salesacctdescr$, 0%,    ~
                                                               0, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Account"
                return

L54510
*        Sales Discounts Account               DISCACCTL$()
            discacctldescr$ = hex(06) & "Select Sales Discounts Account."
            call "GETCODE" (#03, discacctl$(c%), discacctldescr$, 0%, 0, ~
                                                                  f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Invalid Sales Discounts Account"
                return

L54590
*        Specific BOM Id.
            if bom$(c%) = " " then return
            readkey$ = str(part$(c%)) & str(bom$(c%))
            plowhdr$()="  Listed Below Are The Existing BOMs For Part: "&~
                                                                part$(c%)
            bomdescr$ = hex(06) & "Select Bill Of Materials"
            call "PLOWCODE" (#16, readkey$, bomdescr$, 2025%, .30,       ~
                                                  f1%(16), plowhdr$(), 3)
                if f1%(16) <> 0% then L54690
                errormsg$ = "BOM not on file" : return
L54690:     bom$(c%) = str(readkey$,26%,3%)
            return

L54702
*        Shipping Priority Code
            if shipcode$(c%) = " " then shipcode$(c%) = shipcode$
                if shipcode$(c%) >="0" and                               ~
                   shipcode$(c%) <= "5" then return
            errormsg$ = "Shipping Priority must be 0 - 5"
            return

        get_atc
            pip%, shelf%, atc1%, atc2%, err%, atch1%, atch2%, horzn% = 0%
            atc$(c%,1%), atc$(c%,2%) = " "
            call "DATUNFMT" (shipdate$(c%))
            call "PIPATCDZ" (part$(c%), shipdate$(c%), #24, #02, #28,    ~
                            pip%, shelf%, atc1%, atc2%, err%, atch1%,    ~
                            atch2%, horzn%)
            call "DATEFMT"  (shipdate$(c%))
            if err% <> 1% then L54816
                atc1$ = "    DATE" : atc2$ = "PROBLEM"
L54816:     if err% <> 2% then L54818
                atc1$ = "     NOT" : atc2$ = "AVAIL"
L54818:     atc$(c%,1%) = "ATC1/2 for Ship Date: " & atc1$ & " / " & atc2$
                if err% <> 0% then return
            convert atc1%  to  atc1$, pic(-#######)
            convert atc2%  to  atc2$, pic(-#######)
            convert atch1% to atch1$, pic(-#######)
            convert atch2% to atch2$, pic(-#######)
            convert horzn% to atchz$, pic(###)
            call "STRING" addr("LJ",  atc2$, 8%)
            call "STRING" addr("LJ", atch2$, 8%)
            atc$(c%,1%) = "ATC1/2 for Ship Date: " & atc1$ & " / " & atc2$
            if atc1% < 0% then str(atc$(c%,1%),22%,1%) = hex(94)         ~
                          else str(atc$(c%,1%),22%,1%) = hex(84)
            str(atc$(c%,1%),31%,1%) = hex(8c)
            if atc2% < 0% then str(atc$(c%,1%),33%,1%) = hex(94)         ~
                          else str(atc$(c%,1%),33%,1%) = hex(84)
            atc$(c%,2%) = " with Horizon of xxx: "& atch1$ &" / " & atch2$
            str(atc$(c%,2%),18%,3%) = atchz$
            if atch1% < 0% then str(atc$(c%,2%),22%,1%) = hex(94)        ~
                           else str(atc$(c%,2%),22%,1%) = hex(84)
            str(atc$(c%,2%),31%,1%) = hex(8c)
            if atch2% < 0% then str(atc$(c%,2%),33%,1%) = hex(94)        ~
                           else str(atc$(c%,2%),33%,1%) = hex(84)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Data Save Screen               *~
            *************************************************************

        deffn'156(fieldnr%)
            errormsg$ = " "
            on fieldnr%        goto      L55120,    /* Adj Reason       */~
                                         L55350     /* Print Pick List  */
            return

L55120
*        Adj Reason Code
            if crhold$ = "C" then L55270
            if adjrsn$ = " " and manadjre$ <> "Y" then return
            adjrsndescr$ = hex(06) & "Select Adjust Reason Code"
            readkey$ = "SO REASON" & adjrsn$
            call "PLOWCODE" (#08, readkey$, adjrsndescr$, 9%, 0.3, f1%(8))
            if f1%(8) = 1% then adjrsn$ = str(readkey$,10) else L55200
                return     /* Valid Adj Reason so return */
L55200:     if adjrsn$ = " " then goto L55230
                errormsg$ = "Invalid Adjustment Reason Code."
                return     /* Return with error message */
L55230:     if modfld% <> 1% or manadjre$ <> "Y" then return
                    errormsg$ = "Adjustment Reason Code may not be blank."
                    return

L55270:     if can$ <> "Y" then return
            adjrsndescr$ = hex(06) & "Select Cancel Reason Code"
            readkey$ = "CANREASON" & adjrsn$
            call "PLOWCODE" (#08, readkey$, adjrsndescr$, 9%, 0.3, f1%(8))
            if f1%(8) = 1% then adjrsn$ = str(readkey$,10) else          ~
                          errormsg$ = "Invalid Cancellation Reason Code."
            return

L55350
*        Pick List, BOL, and Acknowledgement Printing
*          No real validation needed, so just encode PICKPRINT$
        encode_pickprint
            pickprint% = 0%
            for i% = 1% to 3%
                if pickprint$(i%) = " " then L55440
                     pickprint$(i%) = "X"
                     pickprint% = pickprint% + (1% + ((i% - 1%) * 2%))
L55440:         next i%
            convert pickprint% to pickprint$, pic(#)
            return

        REM *************************************************************~
            *           M I S C   T E S T   R O U T I N E S             *~
            *-----------------------------------------------------------*~
            * A collection of routines used in more than one test.      *~
            *************************************************************

        order_date_check
*         Checks that TESTDATE$ (valid and formatted) is on or after
*         the order date
            call "DATUNFMT" (testdate$)
            call "DATUNFMT" (orderdate$)
            if testdate$ < orderdate$ then                               ~
                            errormsg$ = "Must be on or after Order Date"
            call "DATEFMT" (orderdate$)
            if errormsg$ <> " " then                                     ~
                          errormsg$ = errormsg$ & " (" & orderdate$ & ")"
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
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#33)
            end
