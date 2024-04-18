        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  V   V  BBBB   K   K  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  V   V  B   B  K  K     I    NN  N  P   P  U   U    T     *~
            *  V   V  BBBB   KKK      I    N N N  PPPP   U   U    T     *~
            *   V V   B   B  K  K     I    N  NN  P      U   U    T     *~
            *    V    BBBB   K   K  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKINPUT - Purchase Order Entry and Maintenance Program.  *~
            *            Creates / Maintains Purchase Orders using the  *~
            *            standard Buffer file logic.  Uses standard     *~
            *            line summary and detail input screens for      *~
            *            line item processing.  Because of compiler     *~
            *            constraints due to the size of the program,    *~
            *            the screens have been externalized using the   *~
            *            subroutine VBKINPTS.                           *~
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
            * 12/22/83 ! ORIGINAL                                 ! KEN *~
            * 08/28/84 ! MODIFY FOR 7 DECIMAL PLACES IN UNIT PRICE! BLT *~
            * 08/30/84 ! ADD UNIT PRICE TO POBUFFR                ! BLT *~
            * 07/09/85 ! FINAL CLEANUP & GL CODE FMT CONTROL      ! KEN *~
            *          ! PROGRAM STANDARD IS EXTERNAL GL CODE     ! KEN *~
            *          ! EXCEPT EXPACCT$ AND ACCT$ (WORK ACCOUNTS)! KEN *~
            * 10/15/85 ! Expanded file formats for address and    ! ERN *~
            *          ! added a few fields.                      !     *~
            *          ! NOTE- Prev line disabled. Program won't  !     *~
            *          !       compile (too many branches!?!?).   !     *~
            *          ! Split into two programs (see below)      !     *~
            * 02/05/86 ! Buyer Validation Logic                   ! HES *~
            * 05/09/86 ! Changed VENPRICE select & subroutine     ! LDJ *~
            *          !   calls for procurement history & vendor !     *~
            *          !   price catalog.                         !     *~
            * 06/01/86 ! Let seq# goto 999 but hold maxlines<=100 ! kab *~
            * 06/16/86 ! Put POPRNT in 1st byte of filler         ! kab *~
            * 07/17/86 ! Disable Expense Account If Stocked Part  ! HES *~
            * 08/11/86 ! MODIFIED BUG AT LINE # 25592             ! SGA *~
            * 06/09/87 ! JBMASTR2 record length changed to 1300   ! JIM *~
            * 08/05/87 ! Corrected test on Order dollar limit     ! HES *~
            * 08/12/87 ! Added order total & vendor phone #       ! HES *~
            * 12/??/87 ! Multi Currency Project                   ! KAB *~
            * 02/24/88 ! Correct multi-cur price lookup           ! JDH *~
            * 05/31/89 ! Added GETSCRN, added RCVLINES file & test! JDH *~
            *          !  for any recving activity before delete  !     *~
            *          !  is allowed, chgd scroll on LI sum scrn, !     *~
            *          !  chgd inv cost logic to mimic PAYINPUT,  !     *~
            *          !  phone & contact change when BF changes, !     *~
            *          !  fixed price after re-start or PF4.      !     *~
            * 06/02/89 ! Threw some initializing code into        ! JDH *~
            *          !  VBKINPTS to reduce size of this program !     *~
            * 10/02/89 ! Mods to pricing.  Thank David.           ! JDH *~
            * 10/04/89 ! Disabled Currency field in edit mode.    ! JDH *~
            * 12/13/89 ! Threw some more initializing code into   ! JBK *~
            *          !  VBKINPTS to reduce size of this program.!     *~
            *          !  Added code to get std cost set and pick !     *~
            *          !  up std cost as inventory cost for std   !     *~
            *          !  cost items.  Project #7890207.          !     *~
            * 03/01/90 ! Changed PIC for echange rate reverse date! JDH *~
            * 06/07/90 ! Added a variable OVERSHIPPCT to the COM  ! SID *~
            *          !  statement.                              !     *~
            * 06/27/90 ! Fixed Revision Level Logic on header Scrn! SID *~
            * 06/27/90 ! Fixed PRR's                              ! SID *~
            *          !  11301- Corrected Input/Edit Message     !     *~
            *          !  11209- Added a Max. Dollar Amount Descr.!     *~
            *          !  10313- Enbled (PF14)Find Existing PO's  !     *~
            *          !         once the Vendor code is entered  !     *~
            * 07/17/90 ! Cutover from RO will update inv cost if  ! JDH *~
            *          !  std costed part & VBK flag set.         !     *~
            * 10/17/90 ! Added ln 14675 to initialize FACTOR$     ! MJB *~
            *          !  when loading a line item for edit.      !     *~
            *          !  (code added for R6.00.04)               !     *~
            * 04/02/91 ! Fixed PRR  11804 Stock UOM Display.      ! SID *~
            * 05/07/91 ! Added testing against history files.     ! JDH *~
            * 05/22/91 ! PRR 11880 Enable/PLOWCODE VENDOR code    ! JIM *~
            * 05/22/91 ! PRR 10369 VENTYPE$ Described at DATALOAD.! JIM *~
            * 11/07/91 ! PRR 12132,10826. Our Price must be < 1E7.! JDH *~
            *          ! Merged VBKINPTS back into this.          !     *~
            *          ! PRR 10370. Added Stocking UOM Description!     *~
            *          !   to Line Item Screen 1.                 !     *~
            *          ! PRR 10699,10561 Added Copy Previous Line.!     *~
            * 12/31/91 ! PRR 12243. Added mass change of line date! WPH *~
            * 04/10/92 ! PRR 12201 - Added PF(10)See PIP access to! MLJ *~
            *          !   PIPATCSB from line item detail screen. !     *~
            * 04/13/92 ! PRR 12350 - Disabled Expense Acct field  ! MLJ *~
            *          !   in edit mode for stocked parts only.   !     *~
            * 06/04/92 ! Minor mods for DEC compatibility         ! MLJ *~
            * 07/07/92 ! Added selection of currency for PO cut   ! JDH *~
            *          !   over from a directive.  (Wee Mods)     !     *~
            * 10/20/92 ! Only select currency if directive does   ! JDH *~
            *          !   not specify one.  Cut over directives  !     *~
            *          !   by currency code.                      !     *~
            * 10/21/92 ! Corrected soft enable access on 1st scrn.! JDH *~
            * 10/28/92 ! Added call to SZRINSUB and logic to auto-! WPH *~
            *          ! create line items for size runs.         !     *~
            * 11/02/92 ! Vendor's currency code defaults in for   ! JDH *~
            *          !   manually created POs.                  !     *~
            * 11/10/92 ! Copy cost distribution along with rest.  ! JDH *~
            * 03/02/93 ! Honors directive prices in foreign curr. ! JDH *~
            * 03/16/93 ! Core Value Coding Added.  Inventory cost ! JBK *~
            *          !   for Reman Parts costed at std is sum   !     *~
            *          !   of cost of Reman Part and Core Part.   !     *~
            * 05/24/93 ! Allow three different Boiler Plate texts.! JDH *~
            * 10/06/93 ! Purchase Job Project.                    ! JBK *~
            * 11/17/93 ! PRR 11784 - Standardized Screen Headers  ! JBK *~
            *          !   to some degree, corrected date mislabel!     *~
            *          ! PRR 12507 - Job or Project must be open  !     *~
            *          !   when entered for a non-stocked part.   !     *~
            *          ! PRR 12930 - Size Run now available for   !     *~
            *          !   line item #1 as well as all other lines!     *~
            * 03/23/94 ! Exchange rates no longer change when a   ! JDH *~
            *          !   line is added to an existing PO; rates !     *~
            *          !   remain as originally determined.       !     *~
            * 04/20/94 ! Now considers 'B' option for inv cost dflt JDH *~
            * 07/05/94 ! Purchase Contract Project -added Contract! RJH *~
            *          !   ID & Line Fields to Part Line 1stScreen!     *~
            *          !   Moved Job/Proj Code to 1st Screen.     !     *~
            *          !   Added Call to Purchase Contract Inquiry!     *~
            *          !   Sub.                                   !     *~
            *          ! Vendor Service Project - Added PF26 to   !     *~
            *          !   call POVRELSB  to process service PO's !     *~
            * 11/23/94 ! Now Looks for Default Contract ID value. ! LDJ *~
            *          !   Changed lot of FP constants to Integers!     *~
            *          !   Fixed Bug in looking at Procurement    !     *~
            *          !   History from Line Summary screen.      !     *~
            * 12/01/94 ! Corrected creating PO from Purchase      ! LDJ *~
            *          ! advice - now loads in Job field also.    !     *~
            * 01/10/95 ! Added missing 5th Alt Key to PORLSE.     ! JDH *~
            * 02/02/95 ! PRR 13321 - PF(8) History key re-fixed.  ! RJH *~
            * 04/04/95 ! PRR 13378 - Honor Kit PJs Complete switch! JDH *~
            * 04/13/95 ! PRR 13391 - Fixed PF25 Change Dates with ! JDH *~
            *          !   date field's soft enables set to 1.    !     *~
            * 04/14/95 ! PRR 13392 - Update Rev Date when Revision! JDH *~
            *          !   Level is changed (auto or manual).     !     *~
            * 04/17/95 ! PRR 13377. Use this level costs for PJs. ! JDH *~
            * 04/21/95 ! Format correction.  No functional change.! JDH *~
            * 07/17/95 ! Use this level costs for PJs that are    ! JDH *~
            *          !   Core parts.  Oversight from 04/17/95.  !     *~
            * 08/23/95 ! Determine the expense account for non-   ! JDH *~
            *          !   stocked directives.                    !     *~
            * 08/24/95 ! Added DATERUN for multi-copies.          ! JDH *~
            * 08/28/95 ! Price change for VSA now hits correct    ! JDH *~
            *          !   inventory cost bucket.                 !     *~
            * 08/13/96 ! Fixed blanking of header GL Accounts.    ! JDH *~
            * 09/04/96 ! PRR 13638. Re-enabled F6 'Same as Prev'. ! JDH *~
            * 08/26/97 ! Millie Date Conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            a_bom(12),                   /* Bill of Material Costs     */~
            a_dtl(12),                   /* Misc Cost Details          */~
            a_rte(12),                   /* This Level Costs           */~
            acct$(100)16,                /* Expense Account            */~
            acctdescr$32,                /* Expense Account            */~
            appnd$(100)1,                /* Appended this session flgs */~
            ask_msg$(3)78,               /* ASKUSER lines              */~
            basedate$8,                  /* Planning Base Date         */~
            bfcode$6, bfdescr$30,        /* Buy from Code & Descr      */~
            blank$79,                    /* A bunch of blanks          */~
            blankdate$8,                 /* Blank date                 */~
            bom_id$(100)3,               /* Purchased Job BOM ID       */~
            buyer$3,                     /* Buyer Code (Header)        */~
            buyerl$(100)3,               /* Buyer Code (Line Items)    */~
            buyshp$9,                    /* Buy/Ship Address Prompt    */~
            candate$8,                   /* Cancellation Date          */~
            cat$(100)4,                  /* Part Category              */~
            catdescr$32,                 /* Category                   */~
            closeddate$6,                /* Date Job or Project Closed */~
            codes$(100)3,                /* Part/buyer Release codes   */~
            confirm$1,                   /* Confirmed? (Y/N)           */~
            contract_id$(100)16,         /* Purchase Contract ID       */~
            contract_descr$32,           /* Purchase Contract Descriptn*/~
            contract_line$(100)4,        /* Purchase Contract Line #   */~
            copytextdescr$(3)30,         /* Hdr Boiler plate Descr     */~
            copytextid$(3)4,             /* Hdr Boiler Plate Text ID   */~
            copytextname$(3)10,          /* Hdr Boiler Plate Text ID   */~
            core_inv_flag$1,             /* Core Value Inv Trans Flag  */~
            costtype$(100)1,             /* Item costing method        */~
            cur_dir$4,                   /* Currency from Pur Directive*/~
            cur_stat$4,                  /* Default Statuatory Currency*/~
            cur_po_table$1,              /* P.O. Currency Exchange Tbl.*/~
            cur_tran$4,                  /* Transaction Currency       */~
            cur_eff_date$6,              /* Currency trans. eff date   */~
            cur_descr$(5)25,             /* Currency Conv. Info        */~
            cur_vend$4,                  /* Vendor's default currency  */~
            cur_vpm$4,                   /* Currency in price catalog  */~
            curr_dsply$22,               /* Max. Dollar Limit Descr.   */~
            cursor%(2),                  /* Cursor Location for Edit   */~
            daterun_qty(100),            /* Qtys from DATERUN          */~
            daterun_date$(100)8,         /* Dates from DATERUN         */~
            date$8,                      /* Date for screen display    */~
            datetime$7,                  /* Date/time for data save    */~
            default$30,                  /* Default passed from SZRINSB*/~
            defbuyer$3,                  /* Buyer code for Req's       */~
            defjob$8,                    /* Default job number         */~
            defjobdescr$32,              /* Default job number         */~
            deforddate$8,                /* Req's on or before         */~
            defstr$3,                    /* Default store number       */~
            defstrdescr$32,              /* Default Store number       */~
            dlvrto$(100)20,              /* Deliver To                 */~
            dmap(4),                     /* PLOWCODE Screen mapping    */~
            dspaddr$(6)30,               /* Address to display         */~
            duedate$(100)8,              /* Due date                   */~
            duedate$8,                   /* Default due date           */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            expacct$16,                  /* Purchase expense acct      */~
            ext$(100)10,                 /* Line item extension net    */~
            extg$(100)10,                /* Line item extension gross  */~
            ext_loaded(100),             /* Extension loaded frm Lines */~
            factor$10,                   /* UOM Conversion Factor      */~
            factor(100),                 /* UOM Conversion Factor      */~
            fillerl$(100)176,            /* Filler                     */~
            fob$30,                      /* F.O.B.                     */~
            frgtflag$1,                  /* Freight terms              */~
            frgtflagdescr$32,            /* Freight terms              */~
            hdr$(3)79,                   /* PLOWCODE Column Headers    */~
            header$(6)200,               /* Multi-use Variable         */~
            headr1$(2)79,                /* Header for screen          */~
            hnycost$10,                  /* Inventory Cost Total       */~
            hnycost(100),                /* Inventory Cost Total       */~
            hnycostd$(100)96,            /* Inventory Cost Details     */~
            id1$79, id2$79,              /* Screen ID Lines            */~
            ie(1), ie$(1)1,              /* PLOWCODE Dummies           */~
            inputmsg$79,                 /* Input message for ASKDATE  */~
            inpmessage$79,               /* Input message              */~
            invcost(12),                 /* Inventory cost buckets     */~
            item$25,                     /* Contract Specific Item     */~
            job$(100)8,                  /* Specific job/proj          */~
            jobdescr$32,                 /* Specific job/proj          */~
            lastpo$14,                   /* Last PO #                  */~
            lastseq$3,                   /* Last sequence # used       */~
            lastven$9,                   /* Last vendor                */~
            lfac$(20)1,                  /* Field attribute characters */~
            lineref$(100)5,              /* Line Item Reference        */~
            linerev$(100)2,              /* Line Revision Level        */~
            line_source%(100),           /* Source of Line             */~
            lot$(100)6,                  /* Lot code                   */~
            line2$79,                    /* Screen Line #2             */~
            lstdate$8,                   /* last date test             */~
            maxdlrs$10, maxdate$8,       /* Limits of possibles        */~
            mindate$8,                   /* min date test              */~
            meas$(100)4,                 /* Unit of issue              */~
            newdate$8,                   /* New line item due date     */~
            newdatef$8,                  /* New date, formatted        */~
            nextdate$(100)8,             /* Next shipment              */~
            notbefore$(100)8,            /* Do not receive before date */~
            notafter$ (100)8,            /* Do not receive after  date */~
            ohpost$(100)1,               /* Last recpt posting mode    */~
            oldreadkey$100,              /* G.P. Read key              */~
            orddate$8,                   /* Order date                 */~
            origdue$(100)8,              /* Original Due Date          */~
            origrev$2,                   /* Revision when Loaded       */~
            osmsg$40,                    /* Canned msg fragment        */~
            ospct$8,                     /* Over shipment percentage   */~
            ourprice$10,                 /* Our Unit Price             */~
            packslip$(100)16,            /* Vendor's packing slip      */~
            part$(100,2)25,              /* Part number (ours, his)    */~
            partdescr$(100)32,           /* Part Description           */~
            passpart$(72)25,             /* Parts passed from SZRINSUB */~
            passqty$(72)10,              /* Qtys  passed from SZRINSUB */~
            paydate$8,                   /* Payables posting date      */~
            pcacct$(100)16, pcacct$9,    /* Price / Cost Var. Account  */~
            pcacctdescr$32,              /* Price / Cost Var. Acct Dscr*/~
            penddate$8,                  /* Planning Cal End Date      */~
            pf$(3)79, pfk$32,            /* PF Descrs and Keys         */~
            phone$16,                    /* Vendors Phone Number       */~
            plowkey$100,                 /* G.P. Read key              */~
            poassgn$3,                   /* How to assign PO #s-m/s/SSS*/~
            pobufkey$(100)66,            /* Link to req buffer         */~
            ponumber$16,                 /* Purchase order number      */~
            poprnt$1,                    /* PO Print Flag              */~
            postatstdcost$1,             /* Post cost at std cost      */~
            pototal$10,                  /* Cumulative PO Total        */~
            prcpart$25,                  /* Proc hist part number      */~
            prcven$9,                    /* Proc hist vencode          */~
            price(100),                  /* Purchase price             */~
            puracct$16,                  /* Liability account purchases*/~
            puracctdescr$32,             /* Liability account          */~
            qtyonord$10,                 /* Remaining Qty On Order     */~
            qtyonord(100),               /* Remaining Qty On Order     */~
            qtyorig$10,                  /* Original Order Quantity    */~
            qtyorig(100),                /* Original Order Quantity    */~
            qtyrecd$10,                  /* Qty Received Last Shipment */~
            qtyrecd(100),                /* Qty Received Last Shipment */~
            qty_loaded(100),             /* Quantity loaded from Lines */~
            readkey$50,                  /* Misc use Readkey           */~
            readkey2$50,                 /* Misc use Readkey           */~
            recdate$(100)8,              /* Last Received Date         */~
            remanpart$99,                /* Reman - Core Part Keys     */~
            rest$184,                    /* Rest of hdr - Filler       */~
            rev_level$2, rev_date$8,     /* Revision Level and Date    */~
            rev_order_date$8,            /* Reverse Order date         */~
            rev_order_seed$8,            /* seed date for reverse ordr */~
            rcvrinuse$50,                /* RCVTIF2 Key                */~
            rcvqtys$(100)48,             /* Disposition Qtys           */~
            rqstnr$(100)20,              /* Requisitioner              */~
            scr%(4,17), set%(255),       /* For Soft Enables           */~
            selectmsg$79,                /* Selection message          */~
            seqnr$(100)3,                /* Line item sequence #       */~
            serial_no$1,                 /* Serial Number Tracked Flag */~
            setid$8,                     /* Standard Cost Set ID       */~
            shipvia$30,                  /* Ship via                   */~
            shpaddr$(6)30,               /* Ship-to Address            */~
            status$(100)1,               /* Line Item Status           */~
            statusdescr$30,              /* Line Item Status Descr     */~
            stkg$(100)4,                 /* Stocking unit of measure   */~
            stkg_descr$25,               /* Stocking UOM Description   */~
            str$(100)3,                  /* Store number               */~
            strdescr$32,                 /* Store number               */~
            strexpacct$9,                /* Store dflt- Expense acct   */~
            strpuracct$9,                /* Store Dflt- Intrm liablty  */~
            sumonord$(100,2)10,          /* Summary screen on order    */~
            sysacct$(6)9,                /* System default accounts    */~
            taxflag$1,                   /* Taxable? (Y/N)             */~
            temp$(3)4,                   /* Temp variables for ASKUSER */~
            temp$79,                     /* Temp variable              */~
            tempdate$8,                  /* temporary test date        */~
            tempven$9,                   /* Temporary VENCODE$         */~
            testdate$8,                  /* Temporary Order Date-Prices*/~
            texta$(113,1)70,             /* Text Array                 */~
            textid$4, textidl$(100)4,    /* Text ID- Hdr, Lines        */~
            textmsg$79,                  /* Text Rtn Message           */~
            type$(100)3,                 /* Part type                  */~
            userid$3,                    /* Userid this user           */~
            userstr$3,                   /* User default store         */~
            varfield$(10)20,             /* Variable fields            */~
            venaddr$(6)30,               /* Vendor buy from name/addrs */~
            vencode$9,                   /* Vendor code                */~
            vencont$20,                  /* Contact's name             */~
            vendescr$30,                 /* Vendor Description         */~
            venonord$10,                 /* On Order Qty - Vendor U/M  */~
            venorig$10,                  /* Orig Order Qty - Vendor U/M*/~
            venprice$10,                 /* Vendor Price - Vendor U/M  */~
            venrecd$10,                  /* Received Qty - Vendor U/M  */~
            ventype$4,                   /* Vendor Type                */~
            ventypedesc$30,              /* Vendor Type Description    */~
            unfmaxdt$8,                  /* unfmt max date 20991231    */~
            zip_dash$1                   /* ZIP code format character  */

        dim                              /* Purchase Job Variables     */~
            byprod$1,                    /* By-Product Rpt Print Deflt */~
            kitcomp$1,                   /* Kit Complete Default Flag  */~
            kit$(100,5)1,                /* Kit Cmplt and Print Options*/~
            kitmsg$(5)20,                /* Kit Cmplt & Prt Option Msgs*/~
            npj_no$8,                    /* Next Purchased Job #       */~
            picklist$1,                  /* Print Pick List Default    */~
            pjfac$(2)1,                  /* Kit Cmplt & Prt Option FACs*/~
            pj_bom$1,                    /* Purchased Job BOM flag     */~
            pj_date$8,                   /* Purchased Job Order Date   */~
            pj_lead$10,                  /* Purchased Job Lead Time    */~
            piptag$(100)19,              /* Purchased Job PIP Tags     */~
            pj_stat%(100),               /* Purchased Job Status       */~
            slbom$1,                     /* Single Level BOM Default   */~
            traveler$1,                  /* Print Traveler Default     */~
            vpc_code$25,                 /* Vend Purchs Cntarct ItemCd */~
            vpc_warn$80,                 /* VPC Conflicts to VBK Line  */~
            vpc_end$8, lin_end$8,        /* Vend Purchs Cntarct EndDate*/~
            vpc_potyp$1,                 /* PO Parts VPC Type (P,A,M,H)*/~
            vpc_start$8,lin_start$8,     /* Vend Purchs Cntarct StrtDat*/~
            vpc_stkuom$4,                /* Vend Purchs Cntarct UOM    */~
            vpc_temp$16,                    /* Contract to Query       */~
            vpc_temp1$4,                    /* Contract Line for Query */~
            vpc_type$1,                  /* Vend Purchs Cntarct Type   */~
            vpc_used$(100)20,            /* VPC's Used in PO Session   */~
            vpc_vendor$9,                /* Vend Purchs Cntarct Vendor */~
            vpc_use_idx%(100,101)        /* VPC's Used PO Xref Index   */


        dim                                                              ~
            f2%(64),                     /* = 0 If the file is open    */~
            f1%(64)                      /* = 1 If read was successful */

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
            * # 1 ! USERINFO ! Users Default Information File           *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! VENDOR   ! Vendor Master File                       *~
            * # 4 ! VBKMASTR ! Backlog main header file                 *~
            * # 5 ! VBKLINES ! Backlog line item file                   *~
            * # 6 ! VBKBUFFR ! Backjob buffer for PO headers            *~
            * # 7 ! VBKBUF2  ! Buffer for line items                    *~
            * # 8 ! HNYMASTR ! Inventory Master File                    *~
            * # 9 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #10 ! HNYPROC  ! Inventory where procurred from detail fi *~
            * #11 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * #12 ! HNYGENER ! generic xref file                        *~
            * #13 ! POADVMAS ! Purchase Advices (Suplimental) Master    *~
            * #14 ! VENDORBF ! Vendor Buy From File                     *~
            * #15 ! CATEGORY ! Inventory category codes file            *~
            * #16 ! GLMAIN   ! General Ledger Main File                 *~
            * #17 ! JOBMASTR ! WIP/JC Job Master File                   *~
            * #18 ! JBMASTR2 ! Production job master file               *~
            * #19 ! STORNAME ! Store Info File - Name/Address           *~
            * #20 ! TXTFILE  ! System Text File                         *~
            * #21 ! PIPIN    ! Purchase Advice File                     *~
            * #22 ! PORLSE   ! Buyers Release File                      *~
            * #23 ! PIPMASTR ! Planned Inventory Position Master        *~
            * #24 ! SFCUM2   ! Cumm Forecasts                           *~
            * #25 ! PIPCROSS ! PIP Cross Reference                      *~
            * #26 ! RCVTIF2  ! Receiver TIF Lines                       *~
            * #27 ! PORELUSR ! User Buy Order Release Cross Reference   *~
            * #28 ! GENCODES ! General Codes File                       *~
            * #29 ! RCVLINES ! Receiver Line Items                      *~
            * #30 ! VBKLNCUR ! Currency Line Item Information           *~
            * #31 ! CURCONVR ! Currency Conversion Tables               *~
            * #32 ! VBKHMSTR ! Master PO History File                   *~
            * #33 ! CALMASTR ! Calendar Master File                     *~
            * #34 ! PIPOUT   ! Planned Inventory Withdrawal File        *~
            * #35 ! HNYDETAL ! Inventory Detail File                    *~
            * #36 ! DEMMASTR ! Demand   Master File                     *~
            * #37 ! CURMASTR ! Multi-Currency Master file               *~
            * #38 ! SFCUM2   ! Cumulative Forecast File                 *~
            * #39 ! COREXREF ! Core Part Cross Reference File           *~
            * #40 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * #41 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #42 ! VPCMASTR ! Vendor Purchase Contract Master File     *~
            * #43 ! PAYLINES ! PAYABLES LINE ITEM FILE                  *~
            * #44 ! PAYMASTR ! PAYABLES MAIN HEADER FILE.               *~
            * #45 ! HNYACTXF ! HNY, WC Activity Cross Reference         *~
            * #50 ! WORKFILE ! Workfile for Purchased Job PIP's         *~
            *************************************************************~
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
                        alt key  1, keypos = 10, keylen = 30, dup

            select # 4, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            select # 5, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =  28,                     ~
                        alt key 1, keypos = 333, keylen = 20, dup

            select # 6, "VBKBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1044,                                 ~
                        keypos =    1, keylen =  10,                     ~
                        alt key 1, keypos =   4, keylen =   7, dup,      ~
                            key 3, keypos =  24, keylen =  16

            select # 7, "VBKBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =  28

            select # 8, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 9, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44

            select #10, "HNYPROC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  134,                                  ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =    7, keylen =  65,         ~
                            key  2, keypos =    1, keylen =  40, dup,    ~
                            key  3, keypos =   41, keylen =  31, dup     ~

            select #11, "VENPRICE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  256,                                  ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34          ~

            select #12, "HNYGENER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41          ~

            select #13, "POADVMAS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =   35, keylen =  19,                     ~
                        alt key  1, keypos =   10, keylen =  44,         ~
                            key  2, keypos =    1, keylen =  53,         ~
                            key  3, keypos =   62, keylen =   8, dup

            select #14, "VENDORBF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 15

            select #15, "CATEGORY",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  200,                                  ~
                        keypos =    1, keylen =   4                      ~

            select #16, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #17, "JOBMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =   8                      ~

            select #18, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

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

            select #21, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #22, "PORLSE",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 492,                                   ~
                        keypos =   1, keylen =  66,                      ~
                        alt key  1, keypos =   48, keylen =  19, dup,    ~
                            key  2, keypos =    5, keylen =  62, dup,    ~
                            key  3, keypos =   14, keylen =  53, dup,    ~
                            key  4, keypos =   39, keylen =  28, dup,    ~
                            key  5, keypos =  242, keylen =  19, dup

            select #23, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =   2, keylen =  25,                      ~
                        alt key  1, keypos =    1, keylen =  26

            select #24, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =   1, keylen =  25

            select #25, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos =   1, keylen =  71,                      ~
                        alt key 1, keypos =  20, keylen =  52,           ~
                            key 2, keypos =  39, keylen =  33

            select #26, "RCVTIF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize  =  800,                                 ~
                        keypos  =  26,  keylen  =  52,                   ~
                        alt key 1,  keypos  =  1,  keylen  = 69,         ~
                            key 2,  keypos  = 42,  keylen  = 36,         ~
                            key 3,  keypos  =128,  keylen  = 24

            select #27, "PORELUSR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 70,                                    ~
                        keypos =    1, keylen =   6,                     ~
                        alt key 1, keypos =  4, keylen = 6

            select #28, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 24

            select #29, "RCVLINES", varc, indexed, recsize = 800,        ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24

            select #30, "VBKLNCUR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 5, keylen = 28,                         ~
                        alt key 1, keypos = 1, keylen = 32

            select #31, "CURCONVR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 11

            select #32, "VBKHMSTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            select #33, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1962,                                 ~
                        keypos =    1, keylen =   2

            select #34, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =    64,                                 ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  20,  keylen =  37

            select #35, "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   150,                                 ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  43,  keylen =   6, dup,    ~
                            key  2, keypos =  49,  keylen =   2, dup

            select #36, "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   123,                                 ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =  10,  keylen =  19,         ~
                            key  2, keypos =   1,  keylen =  28,         ~
                            key  3, keypos =  29,  keylen =  25, dup

            select #37, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #38, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25

            select #39, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            select #40, "BOMMASTR",                                      ~
                         varc,     indexed,  recsize =  150,             ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select #41, "ENGMASTR" ,                                     ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos = 1, keylen = 29

            select #42, "VPCMASTR",                                      ~
                         varc,     indexed,  recsize =  600,             ~
                         keypos =  10, keylen = 20,                      ~
                         alt key  1, keypos =  1, keylen = 29,           ~
                             key  2, keypos = 60, keylen = 26, dup

            select #43, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key  1, keypos =    1, keylen =  63,         ~
                            key  2, keypos =   17, keylen =  47          ~

            select #44, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select #45, "HNYACTXF",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  29,                     ~
                        alt key  1, keypos =   26, keylen =   4, dup

            select #50, "WORKFILE",                                      ~
                         varc,     indexed,  recsize =   64,             ~
                         keypos =   1, keylen = 56,                      ~
                         alt key  1, keypos = 20, keylen = 37

            call "SHOSTAT" ("Opening Files, One Moment Please")

            mat f1% = zer

            call "OPENCHCK" (#01, 0%, f2%(01),   0%, " ")
            call "OPENCHCK" (#02, 0%, f2%(02),   0%, " ")
            call "OPENCHCK" (#03, 0%, f2%(03),   0%, " ")
            call "OPENCHCK" (#04, 0%, f2%(04), 200%, " ")
            call "OPENCHCK" (#05, 0%, f2%(05), 200%, " ")
            call "OPENCHCK" (#06, 0%, f2%(06), 100%, " ")
            call "OPENCHCK" (#07, 0%, f2%(07), 100%, " ")
            call "OPENCHCK" (#08, 0%, f2%(08),   0%, " ")
            call "OPENCHCK" (#09, 0%, f2%(09),   0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10),   0%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11),   0%, " ")
            call "OPENCHCK" (#12, 0%, f2%(12),   0%, " ")
            call "OPENCHCK" (#13, 0%, f2%(13),   0%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14),   0%, " ")
            call "OPENCHCK" (#15, 0%, f2%(15),   0%, " ")
            call "OPENCHCK" (#16, 0%, f2%(16),   0%, " ")
            call "OPENCHCK" (#17, 0%, f2%(17),   0%, " ")
            call "OPENCHCK" (#18, 0%, f2%(18),   0%, " ")
            call "OPENCHCK" (#19, 0%, f2%(19),   0%, " ")
            call "OPENCHCK" (#20, 0%, f2%(20),   0%, " ")
            call "OPENCHCK" (#21, 0%, f2%(21),   0%, " ")
            call "OPENCHCK" (#22, 0%, f2%(22),   0%, " ")
            call "OPENCHCK" (#23, 0%, f2%(23),   0%, " ")
            call "OPENCHCK" (#24, 0%, f2%(24),   0%, " ")
            call "OPENCHCK" (#25, 0%, f2%(25),   0%, " ")
            call "OPENCHCK" (#26, 0%, f2%(26),   0%, " ")
            call "OPENCHCK" (#27, 0%, f2%(27),   0%, " ")
            call "OPENCHCK" (#28, 0%, f2%(28),   0%, " ")
            call "OPENCHCK" (#29, 0%, f2%(29),   0%, " ")
            call "OPENCHCK" (#32, 0%, f2%(32),   0%, " ")
            call "OPENCHCK" (#33, 0%, f2%(33),   0%, " ")
            call "OPENCHCK" (#34, 0%, f2%(34),   0%, " ")
            call "OPENCHCK" (#35, 0%, f2%(35),   0%, " ")
            call "OPENCHCK" (#36, 0%, f2%(36),   0%, " ")
            call "OPENCHCK" (#38, 0%, f2%(38),   0%, " ")
            call "OPENCHCK" (#42, vpc_open%, f2%(42%),  0%, " ")
            if vpc_open% =  -1% then L09000
                call "OPENCHCK" (#43, 0%, f2%(43%),   0%, " ")
                call "OPENCHCK" (#44, 0%, f2%(44%),   0%, " ")
                call "OPENCHCK" (#45, 0%, f2%(45%),   0%, " ")

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            tempdate$ = blankdate$
            unfmaxdt$  = "20991231"
            call "DATECONV" (unfmaxdt$)
            ll% = 6%
            plowkey$ = all(hex(00))  :  str(plowkey$,,3) = userid$
            call "READ101" (#6, plowkey$, f1%)
            if f1% = 0% then L09145
                ponumber$ = key(#6, 3%)
                call "ASKUSER" (keyhit%, "RESTARTING",                   ~
                     "THE PO # SHOWN WILL BE REMOVED FROM THE BUFFER",   ~
                     "Press (RETURN) To Continue", ponumber$)
                get #6 using L09125, plowkey$
L09125:         FMT XX(14), CH(25)
                delete #6
                call "DELETE" (#7, plowkey$, 25%)

L09145:     call "READ100" (#1, userid$, f1%)
            if f1% <> 0% then L09160
                keyhit% = 0%
                call "ASKUSER" (keyhit%, "* DATA BASE *", " ",           ~
                     "NO PAYABLES DATE FOR USER.  (RETURN)", " " )
                goto L65000
L09160:     get #1, using L09165, paydate$, userstr$
L09165:          FMT XX(9), CH(6), XX(48), CH(3)
            call "DATEFMT" (paydate$)

            call "READ100" (#2,"MONTHS OPEN",f1%)
            if f1% <> 0 then L09210
L09190:         keyhit% = 0%
                call "ASKUSER" (keyhit%, "* DATA BASE *", " ",           ~
                     "ERROR IN PLANNING DATES.  (RETURN)", " " )
                goto L65000
L09210:     get #2, using L09215, basedate$
L09215:         FMT XX(32),CH(6)
            call "DATEOK" (basedate$, err%, pf$(1))
            if pf$(1) <> " " then L09190
            call "DATUNFMT" (basedate$)

            REM Read in Defaults Record
            call "READ100" (#2, "MODULE.DEFAULTS.AP  ", f1%)
                if f1% = 0 then L09270
            get #2, using L09260, sysacct$()
L09260:     FMT XX(36), 6*CH(9)

L09270:     curr_dsply$, cur_stat$ = " "
            call "READ100" (#2, "SWITCHS.CUR         ", f1%(2))
               if f1%(2) = 0 then L09325
            get #2 using L09290, temp$, cur_stat$, cur_po_table$
L09290:         FMT POS(21), CH(1), CH(4), POS(28), CH(1)
            if temp$ <> "Y" then cur_stat$ = " "
            if temp$ <> "Y" then L09325
                curr_dsply$ = "(Transaction Currency)"
                call "OPENCHCK" (#30%, 0%, f2%(30%), 200%, " ")
                call "OPENCHCK" (#31%, 0%, f2%(31%),   0%, " ")
                call "OPENCHCK" (#37%, 0%, f2%(33%),   0%, " ")

L09325:     edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value and Press RETURN."

            str(line2$,62%) = "VBKINPUT: " & str(cms2v$,,8%)

         headr1$(1)="Part Number               Part Description          ~
        ~Byr S   Quantity  Extension"
         headr1$(2)="Vendor's Part Number      Part Description          ~
        ~Byr S   Quantity  Extension"

            overshippct = .5000   /* THIS MEANS THAT ANY OVERSHIPMENT */
                                  /* RECEIVED MUST BE LESS THAN 50%   */
                                  /* OVER THE ORIGINAL AMOUNT ORDERED */
            call "VBKSWTCH" ("RCPTSLMT", ospct$, overshippct, u3%)
            overshippct = overshippct * .01
            call "CONVERT" (overshippct*100, -2.2, str(ospct$,,7))
            osmsg$ = "BASED ON ALLOWED OVERSHIP OF " & ospct$ & "%"
            overshippct = overshippct + 1

*        Find out how to assign PO Numbers
            call "VBKSWTCH" ("POASSIGN", poassgn$, temp, u3%)
            call "VBKSWTCH" ("POPROPT ", temp$, popropt, u3%)

*        Find out if std cost items to be costed at std
            call "VBKSWTCH" ("POSTDCST", postatstdcost$, temp, u3%)
            if postatstdcost$ = "B" then postatstdcost$ = "Y"

*        See if operator is an administrator or not
            call "CMSMACHK" ("VBK", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%
            buyer% = 1  /* Can buy anything (see below) */
            if admin% = 1 then L09570  /* Wheel */

            REM See If Buyer, Load Release Codes For Cross Checking...
            readkey$ = all(hex(00))
            call "PLOWNEXT" (#27, readkey$, 0%, f1%(27))
                if f1%(27) = 0 then L09570  /* not using feature */

            buyer% = 2  /* Can buy some things */
            readkey$ = str(userid$) & hex(00000000)
L09510:     call "PLOWNEXT" (#27, readkey$, 3%, f1%(27))
                if f1%(27) = 0 then L09550
            c% = c% + 1%
            if c% = 100% then L09550
                get #27, using L09535, codes$(c%)
L09535:             FMT XX(3), CH(3)
                goto L09510

L09550:     REM What did we get?
            if codes$() = " " then buyer% = 0%    /* Can buy nothing */
            if codes$() = "ALL" then buyer% = 1%  /* Can buy anything */

L09570
*        Get Standard Cost Set ID
            buckets% = 1%
            call "STCSETID" (buckets%, #2, setid$, hnycost$)
                if buckets% > 0% then L09588
            setid$ = " "

L09588:     gosub init_enables

            plowkey$ = all(hex(00))              /* If UOM file set up */
            str(plowkey$,10%) = "UOM"            /* then edit entries. */
            call "READ100" (#28, plowkey$, uom%)

*        Check for Core
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#2, plowkey$, core_on%)
                if core_on% <> 1% then L09636
            get #2 using L09621, core_inv_flag$
L09621:         FMT POS(134), CH(1)
            if core_inv_flag$ <> "Y" then core_on% = 0%
            if core_on% <> 1% then L09636
                call "OPENCHCK" (#39, 0%, f2%(39%),  0%, " ")

L09636
*        Check for Purchased Jobs
            job% = 8%  :  init (hex(9c))  pjfac$()
            plowkey$ = "SWITCHS.SFC"
            call "READ100" (#2, plowkey$, pj_on%)
                if pj_on% <> 1% then L09700
            get #2 using L09648, npj_no$, traveler$, picklist$, byprod$,   ~
                               slbom$
L09648:         FMT POS(115), CH(8), POS(127), 4*CH(1)
            call "VBKSWTCH" ("PJ KIT C", kitcomp$, temp, u3%)
            if kitcomp$ <> "N" then kitcomp$ = "Y"
            if npj_no$ = " " then pj_on% = 0%
            if pj_on% = 0% then L09700
                call "OPENCHCK" (#40, 0%, f2%(40%),  0%, " ")
                call "OPENCHCK" (#41, 0%, f2%(41%),  0%, " ")
                call "WORKOPEN" (#50, "IO   ",     100%, f2%(50%))

                kitmsg$(1%) = "Kit Complete"
                kitmsg$(2%) = "Print -  Traveler:"
                kitmsg$(3%) = "Pick List:"
                kitmsg$(4%) = "By-Product List:"
                kitmsg$(5%) = "Single Level BOM:"

L09700: REM *************************************************************~
            *                    PREAMBLE SCREEN.                        ~
            * --------------------------------------------------------- *~
            * Select whose work to honor & obey.                         ~
            *************************************************************

            buyscrn% = 11%

        select_buyer
            oldreadkey$ = "R" & hex(00000000)
            call "PLOWNEXT" (#22, oldreadkey$, 1%, f22%)
                if f22% <> 0% then L09726
                   if buyscrn% <> 11% then L65000
                      buyscrn% =  12%
                      goto L10000
L09726:     errormsg$=" "
            if deforddate$ <> " " and deforddate$ <> blankdate$ then ~
                call "DATEOK" (deforddate$, err%, errormsg$)
            if errormsg$ <> " " then deforddate$ = " "

L09736:     call "STRING" addr ("CT", errormsg$, 66%)

L09740: accept                                                           ~
               at (06,06),                                               ~
        "****************************************************************~
        ~*******",                                                        ~
               at (07,06), "*", at (07,76), "*",                         ~
               at (08,06), "*", at (08,76), "*",                         ~
               at (09,06), "*", at (09,76), "*",                         ~
               at (10,06), "*", at (10,76), "*",                         ~
               at (11,06), "*", at (11,76), "*",                         ~
               at (12,06), "*", at (12,76), "*",                         ~
               at (13,06), "*", at (13,76), "*",                         ~
               at (14,06),                                               ~
        "****************************************************************~
        ~*******",                                                        ~
               at (08,17), "Honor Purchase Directives From BUYER ID.",   ~
               at (08,58), fac(hex(81)), defbuyer$              , ch(03),~
               at (10,19), "To Be Purchased on or Before",               ~
               at (10,48), fac(hex(81)), deforddate$            , ch(08),~
               at (12,09), fac(hex(94)), errormsg$              , ch(66),~
               at (16,11), "PF8 To Select Buyer w/Directives",           ~
               at (16,51), "PF16 To EXIT Program",                       ~
                                                                         ~
                     keys(hex(00080f10)), key(keyhit%)

            if keyhit% = 13% or keyhit% = 15% then gosub keyhit1315
            if keyhit% = 13% or keyhit% = 15% then L09736
            if keyhit% <> 8% then L09798
                gosub view_buyers
                goto L09740
L09798:     if keyhit% = 16 then L65000
            if keyhit% <> 0 then L09736

            errormsg$ = " " : buyscrn% =  12%
            if defbuyer$ <> " " then L09812
                deforddate$ = " "
                goto L10000
L09812:     call "DATEOK" (deforddate$, err%, errormsg$)
            if errormsg$ <> " " then L09736
                call "DATUNFMT" (deforddate$)
                if defbuyer$ = userid$ then L10000
                     oldreadkey$ = "R" & str(defbuyer$,,3) & hex(00)
L09822:              call "PLOWNEXT" (#22, oldreadkey$, 4%, f22%)
                     if f22% <> 0% then L09836
                          call "DATEOK" (deforddate$, err%, errormsg$)
                          errormsg$ ="No Purchase Directives From Buyer "~
                                      & defbuyer$ & " By " & deforddate$ ~
                                      & " Were Found."
                          goto L09736
L09836:              if str(oldreadkey$,48,19) <> " "         then L09822
                     if str(oldreadkey$,39, 6)  > deforddate$ then L09822
                     goto L10000

        view_buyers
            oldreadkey$ = "R" & hex(000000)
            defbuyer$ = " "
            hdr$(2) = "            Buyer" : hdr$(1) = " "
            plowkey$ = hex(0684) & "Select Buyer with Directives;   " &  ~
                                   "or PF16 to Return"
            call "PLOWCODE" (#22, oldreadkey$, plowkey$, -3004%, -.03,   ~
                            f1%, hdr$(), 0, 2)
            if f1% = 1% then defbuyer$ = str(plowkey$,,3%)
            return


L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            gosub L29000   /* Initialize variables  */
            oldpoonfile%, maxlines%, mode%, l%, lastseq%=0%

            mostin% = 0%
            for fieldnr% = 1% to 10%     /* First Page- Header         */
                if fieldnr% <= mostin% then edit% = -1% else edit% = 1%
                gosub deffn_051
                      if enabled% = 0% then L10145
L10075:         gosub deffn_101
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10110
L10090:                   fieldnr% =  fieldnr% - 1% : edit% = -1%
                          if fieldnr% <= 1% then L10160
                              gosub deffn_051
                              if enabled% <> 0% then L10075 else goto L10090
L10110:               if keyhit%  = 16% and fieldnr% <4% then select_buyer
                      if keyhit%  = 10% and fieldnr% = 1% then gosub porel
                      if keyhit%  = 26% and fieldnr%= 1% then gosub povrel
                      if keyhit%  = 11% then gosub show_porlse
        REM           IF KEYHIT%  = 10 AND FIELDNR% = 2                  ~
                                       AND DEFBUYER$<>" " THEN GOSUB POREL
                      if keyhit%  =  8% then gosub L59000
                      if keyhit%  = 14% then gosub pomstr
                      if keyhit% <>  0% then       L10075
L10145:         gosub deffn_151
                      if errormsg$ <> " " then L10075
                mostin% = max(mostin%, fieldnr%)
L10160:     next fieldnr%

            gosub L32000   /* LOAD LINES FROM PURCHASE DIRECTIVES FILE  */
                          /* LOAD HERE TO SET UP DEFAULTS FOR PAGE 2.  */

            mostin% = 0%
            for fieldnr% = 1% to 12%      /* Second page- Header      */
            if fieldnr% <= mostin% then edit% = -1% else edit% = 1%
                gosub deffn_052
                      if enabled% = 0% then L10240
L10195:         gosub deffn_102
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10230
L10210:                   fieldnr% = fieldnr% - 1% : edit% = -1%
                          if fieldnr% = 0% then L10255
                              gosub deffn_052
                              if enabled% <> 0% then L10195 else goto L10210
L10230:               if keyhit%  =  8% then gosub L59000
                      if keyhit% <>  0% then       L10195
L10240:         gosub deffn_152
                      if errormsg$ <> " " then L10195
                mostin% = max(mostin%, fieldnr%)
L10255:         next fieldnr%


*        Input variable fields section
            gosub inpvfs

            if maxlines% > 0% then line_summary

        insertlines
L10310:     if maxlines%  = 100%    then L10375
            if lastseq%  >= 999%    then L10375
                c% = maxlines% + 1%
                gosub inputlines
                if keyhit% = 16% then endinsert
                     maxlines% = maxlines% + 1%
                     lastseq%  = lastseq%  + 1%
                     convert lastseq% to seqnr$(c%), pic(###)
                     appnd$(c%) = "I"
                     if part$(c%,1%) <> " " then buyerl$(c%) = buyer$
                     goto L10310
            endinsert
                gosub columnone
                daterun_mode% = 0%
L10375:         if mode% <> 0% then return else line_summary

        inputlines
            gosub L14000                  /* Describe Line Item Fields  */
            if daterun_mode% = 1% then daterun_process
            mostin% = 0%
            for fieldnr% = 1% to 17%    /* Page 1 of Line Items       */
                if fieldnr% <= mostin% then edit% = -1% else edit% = 1%
                gosub deffn_053
                      if enabled% = 0% then L10505
                if fieldnr% <> 16% then L10420
*                 TEMP$ = " " : GOSUB CALL_HNYCDIST
                      goto L10505
L10420:         gosub deffn_103
                      if keyhit%  = 16% and fieldnr% = 1% then return
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  2% then       L10444
                         gosub columnone
                         goto inputlines
L10444:               if keyhit% <>  3% then       L10448
                         goto copy_prev_line
L10448:               if keyhit% =   5% then daterun_process
                      if keyhit% <>  6% then L10465
                         gosub prevline1
                         goto L10505
L10465:               if keyhit% <>  4% then L10495
L10470:                  fieldnr% = fieldnr% - 1% : edit% = -1%
                         if fieldnr% = 0% then L10520
                             gosub deffn_053
                             if enabled% <> 0% then L10420
                             goto L10470
L10495:               if keyhit%  =  8% then gosub L59100
                      if keyhit%  =  9% then size_run
                      if keyhit%  = 10% then gosub L59150
                      if keyhit%  = 25% then gosub view_vendor_contracts
                      if keyhit% <>  0% then       L10420
L10505:         gosub deffn_153
                      if errormsg$ <> " " then L10420
                mostin% = max(mostin%, fieldnr%)
L10520:     next fieldnr%

            mostin% = 0%
            for fieldnr% = 1% to 13%    /* Page TWO of Line Items */
        input_li_pg2
                if fieldnr% <= mostin% then edit% = -1% else edit% = 1%
                if change_date% = 1% then edit% = 2%
                gosub deffn_054
                      if enabled% = 0% then L10640
L10555:         gosub deffn_104
                      if keyhit%  = 16% and fieldnr% = 1% then return
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  2% then       L10585
                         if change_date% = 1% then return
                            gosub columnone
                            goto inputlines
L10585:               if keyhit% <>  6% then       L10600
                         gosub prevline2
                         goto L10640
L10600:               if keyhit% <>  4% then       L10630
L10605:                  fieldnr% = fieldnr% - 1% : edit% = -1%
                         if fieldnr% = 0% then L10655
                         gosub deffn_054
                             if enabled% <> 0% then L10555
                             goto L10605
L10630:               if keyhit%  =  8% then gosub L59100
                      if keyhit%  = 10% then gosub L59150
                      if keyhit%  = 25% then gosub view_vendor_contracts
                      if keyhit% <>  0% then       L10555
L10640:         gosub deffn_154
                      if errormsg$ <> " " then L10555
                mostin% = max(mostin%, fieldnr%)
L10655:         next fieldnr%
                if pj_on% <> 1% then L10660
                     kitmode% = 1%
                     gosub pj_kit_call
L10660:         return

        columnone
            init (" ") part$(c%,1%),partdescr$(c%),seqnr$(c%),meas$(c%), ~
              lot$(c%),cat$(c%),acct$(c%),str$(c%),job$(c%),duedate$(c%),~
              recdate$(c%),nextdate$(c%),type$(c%), packslip$(c%),       ~
              part$(c%,2%),rcvqtys$(c%),pobufkey$(c%),ohpost$(c%),       ~
              origdue$(c%), status$(c%), dlvrto$(c%), rqstnr$(c%),       ~
              lineref$(c%), linerev$(c%), appnd$(c%), notbefore$(c%),    ~
              notafter$(c%), buyerl$(c%), ext$(c%), extg$(c%),           ~
              sumonord$(c%,1%),sumonord$(c%,2%), pcacct$(c%),            ~
              costtype$(c%), jobdescr$, stkg$(c%), bom_id$(c%),          ~
              piptag$(c%), kit$(c%,1%), kit$(c%,2%), kit$(c%,3%),        ~
              kit$(c%,4%), kit$(c%,5%),contract_id$(c%),contract_line$(c%)
            init (hex(00)) rcvqtys$(c%), hnycostd$(c%)
            textidl$(c%) = all(hex(ff))
            factor(c%), qtyorig(c%), qtyonord(c%), qtyrecd(c%),          ~
            price(c%), hnycost(c%) = 0%
            pj_stat%(c%), line_source%(c%) = 0%

            if maxlines% = 0% then cur_tran$ = " "

            return

        prevline1
            if c% = 1% then return

            if fieldnr% =  1% then part$  (c%,1%) = part$   (c%-1,1%)
            if fieldnr% =  1% then type$     (c%) = type$     (c%-1%)
            if fieldnr% =  1% then costtype$ (c%) = costtype$ (c%-1%)
            if fieldnr% <> 1% then L10770
                line_source%(c%) = 0%
                gosub purchase_job_check
L10770:     if fieldnr% =  2% then partdescr$(c%) = partdescr$(c%-1%)
            if fieldnr% =  3% then str$      (c%) = str$      (c%-1%)
            if fieldnr% =  4% then lot$      (c%) = lot$      (c%-1%)
            if fieldnr% =  5% then cat$      (c%) = cat$      (c%-1%)
            if fieldnr% =  6% then part$  (c%,2%) = part$   (c%-1,2%)
            if fieldnr% =  7% then meas$     (c%) = meas$     (c%-1%)
            if fieldnr% =  7% then factor    (c%) = factor    (c%-1%)
            if fieldnr% =  8% then contract_id$(c%)= contract_id$(c%-1%)
            if fieldnr% =8% then contract_line$(c%)=contract_line$(c%-1%)
            if fieldnr% <> 9% then L10820
               call "READ100" (#23, part$(c%,1%), f1%(23%))
                   if f1%(23%) <> 0% then L10810
                                   job$      (c%) = job$      (c%-1%)
                                   goto L10820
L10810:            if pj_on% <> 1% then L10820
                   if pj_stat%(c%) = 0% then L10820
                              job$      (c%) = str(job$(c%-1%),1%,2%)
L10820:     if fieldnr% = 10% then cur_tran$      = cur_tran$
            if fieldnr% = 11% then qtyorig   (c%) = qtyorig   (c%-1%)
            if fieldnr% = 12% then qtyonord  (c%) = qtyonord  (c%-1%)
            if fieldnr% = 13% then qtyrecd   (c%) = qtyrecd   (c%-1%)
            if fieldnr% = 14% then price     (c%) = price     (c%-1%)
            if fieldnr% = 15% then extg$     (c%) = extg$     (c%-1%)
            if fieldnr% = 16% then hnycost   (c%) = hnycost   (c%-1%)
            if fieldnr% = 17% then hnycostd$ (c%) = hnycostd$ (c%-1%)
*          IF FIELDNR% = 18% THEN STATUS$   (C%) = STATUS$   (C%-1%)
            if fieldnr% >= 10% and fieldnr% <= 16% then gosub deffn_040
          return

        prevline2
            if c% = 1% then return

            if fieldnr% =  1% then duedate$  (c%) = duedate$  (c%-1%)
            if fieldnr% =  1% then notbefore$(c%) = notbefore$(c%-1%)
            if fieldnr% =  1% then notafter$ (c%) = notafter$ (c%-1%)
            if fieldnr% =  2% then origdue$  (c%) = origdue$  (c%-1%)
            if fieldnr% =  3% then nextdate$ (c%) = nextdate$ (c%-1%)
            if fieldnr% =  4% then recdate$  (c%) = recdate$  (c%-1%)
            if fieldnr% =  5% then packslip$ (c%) = packslip$ (c%-1%)
            if fieldnr% =  6% then acct$     (c%) = acct$     (c%-1%)
            if fieldnr% =  7% then pcacct$   (c%) = pcacct$   (c%-1%)
            if fieldnr% =  8% then linerev$  (c%) = linerev$  (c%-1%)
            if fieldnr% =  9% then dlvrto$   (c%) = dlvrto$   (c%-1%)
            if fieldnr% = 10% then rqstnr$   (c%) = rqstnr$   (c%-1%)
            if fieldnr% = 11% then lineref$  (c%) = lineref$  (c%-1%)
            if fieldnr% = 12% then buyerl$   (c%) = buyerl$   (c%-1%)
            if fieldnr% <> 13% then L10945
            if pj_on%   <>  1% then L10945
            if pj_stat%(c%) = 0% then L10945
            if line_source%(c%-1%) > 1% then L10940
               kit$(c%,1%) = kit$(c%-1%,1%) : kit$(c%,2%) = kit$(c%-1%,2%)
               kit$(c%,3%) = kit$(c%-1%,3%) : kit$(c%,4%) = kit$(c%-1%,4%)
               kit$(c%,5%) = kit$(c%-1%,5%)
               goto L10945
L10940:     kit$(c%,1%) = kitcomp$   : kit$(c%,2%) = traveler$
            kit$(c%,3%) = picklist$  : kit$(c%,4%) = byprod$
            kit$(c%,5%) = slbom$
L10945:   return

        copy_prev_line
            if daterun_mode% = 1% then L10952
                call "SHOSTAT" ("Copying Previous Line...")
L10952
*        We will assume copied data is just as valid as before
            part$(c%, 1%)  = part$(c%-1%, 1%) : type$(c%)   = type$(c%-1%)
            partdescr$(c%) = partdescr$(c%-1%)
            costtype$(c%)  = costtype$(c%-1%) : str$(c%)    = str$(c%-1%)
            part$(c%, 2%)  = part$(c%-1%, 2%) : lot$(c%)    = lot$(c%-1%)
            qtyorig(c%)    = qtyorig(c%-1%)   : cat$(c%)    = cat$(c%-1%)
            qtyonord(c%)   = qtyorig(c%-1%)   : stkg$(c%)   = stkg$(c%-1%)
            price(c%)      = price(c%-1%)     : meas$(c%)   = meas$(c%-1%)
            factor(c%)     = factor(c%-1%)    : acct$(c%)   = acct$(c%-1%)
            hnycost(c%)    = hnycost(c%-1%)
            hnycostd$(c%)  = hnycostd$(c%-1%)
            status$(c%)    = status$(c%-1%)
            duedate$(c%)   = duedate$(c%-1%)
            notbefore$(c%) = notbefore$(c%-1%)
            notafter$(c%)  = notafter$(c%-1%)
            origdue$(c%)   = origdue$(c%-1%) :nextdate$(c%) = origdue$(c%)
            if daterun_mode% = 1% then gosub daterun_data_swap
            linerev$(c%)   = linerev$(c%-1%)
            pcacct$(c%)    = pcacct$(c%-1%)
            dlvrto$(c%)    = dlvrto$(c%-1%)
            rqstnr$(c%)    = rqstnr$(c%-1%)
            lineref$(c%)   = lineref$(c%-1%)
            contract_id$  (c%) = contract_id$  (c%-1%)
            contract_line$(c%) = contract_line$(c%-1%)

            gosub purchase_job_check
            if pj_stat%(c%) <> 0% then L10981
                job$(c%) = job$(c%-1%)
                goto L10997
L10981:     job$(c%) = str(job$(c%-1%),1%,2%)
            if line_source%(c%-1%) > 1% then L10987
               kit$(c%,1%) = kit$(c%-1%,1%) : kit$(c%,2%) = kit$(c%-1%,2%)
               kit$(c%,3%) = kit$(c%-1%,3%) : kit$(c%,4%) = kit$(c%-1%,4%)
               kit$(c%,5%) = kit$(c%-1%,5%)
               goto L10997
L10987:     kit$(c%,1%) = kitcomp$   : kit$(c%,2%) = traveler$
            kit$(c%,3%) = picklist$  : kit$(c%,4%) = byprod$
            kit$(c%,5%) = slbom$
L10997:     gosub deffn_040
            return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode

        edtpg1       /* Edit First Page of PO Header         */
            inpmessage$ = edtmessage$ : edit% = 2% : chg% = 0%
L11100:     fieldnr% = 0% : gosub deffn_111
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       line_summary
                  if keyhit% <>  3% then       L11160
                     buyshp% = mod(buyshp% + 1%, 2%)
L11160:           if keyhit%  =  5% then       edtpg2
                  if keyhit%  = 16% then gosub datasave
                  if keyhit%  =  8% then gosub L59000
                  if keyhit%  =  9% then gosub L59200
                  if keyhit%  = 24% then gosub edit_text_hdr
                  if keyhit% <>  0% and keyhit% <> 29% then L11100
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% >= 4% and fieldnr% <= 9% then fieldnr% = 4%
            if fieldnr% >  4% then fieldnr% = fieldnr% - 5%
            if fieldnr% <  1% or  fieldnr%  > 10% then L11100
            if keyhit% <> 29% then L11290
                if admin% <> 1% then L11280
                call "ENABLSUB" ("MODIFY", "VBKINPUT", scr%(), set%(),   ~
                                 1%, fieldnr%, 0%, 0%)
L11280:         goto L11100
L11290:     if fieldnr% =  1% or  fieldnr%   =  3% then L11100

            gosub deffn_051
                if enabled% = 0% then edtpg1
L11330:     gosub deffn_111
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11330
            gosub deffn_151
                  if errormsg$ <> " " then L11330
            goto edtpg1

        edtpg2       /* Edit Second Page of PO Header        */
            inpmessage$ = edtmessage$
L11420:     fieldnr% = 0% : gosub deffn_112
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       line_summary
                  if keyhit%  =  4% then       edtpg1
                  if keyhit%  =  5% then       edtvfs
                  if keyhit%  = 16% then gosub datasave
                  if keyhit%  =  8% then gosub L59000
                  if keyhit%  = 24% then gosub edit_text_hdr
                  if keyhit% <>  0% and keyhit% <> 29% then L11420
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 14% then L11420
            if fieldnr% > 12% then fieldnr% = 12%
            if keyhit% <> 29% then L11580
                if admin% <> 1% then L11560
                call "ENABLSUB" ("MODIFY", "VBKINPUT", scr%(), set%(),   ~
                                 2%, fieldnr%, 0%, 0%)
L11560:         goto L11420

L11580:     gosub deffn_052
                if enabled% = 0% then edtpg2
L11600:     gosub deffn_112
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11600
            gosub deffn_152
                  if errormsg$ <> " " then L11600
            goto edtpg2

        line_summary              /* Summary Screen */
        REM IF MAXLINES% = 0% THEN INSERTLINES
            errormsg$ = " "  :  chg% = 0%
            dl% = l% + 1%
            gosub total_po
            selectmsg$ = "To Modify a Line Item, Position Cursor and" &  ~
                         " press RETURN."
            edit% = 2%
L11730:     fieldnr% = 0% : gosub deffn_115
            gosub call_screen
             if keyhit% = 1% then gosub startover
             if keyhit% = 2% then l%=0%
             if keyhit% = 3% then l%=max(0%,min(85%,maxlines%-15%))
             if keyhit% = 4% then l%=max(0%,l%-15%)
             if keyhit% = 5% then l%=max(0%,min(85%,l%+15%,maxlines%-15%))
             if keyhit% = 6% then l%=max(0%,l%-1%)
             if keyhit% = 7% then l%=max(0%,min(85%,l%+1%,maxlines%-1%))
             if keyhit% > 0% and keyhit% < 8% then L11730
             if keyhit% = 8% then       L11870
             if keyhit% = 9% then       edtpg1
             if keyhit% =11% then       insertmode
             if keyhit% =12% then       L11870
             if keyhit% =16% then gosub datasave
             if keyhit% =28% then       deletemode
L11870:     fieldnr% = cursor%(1%) - 5%
            if keyhit% <> 8% then L11920
                if fieldnr% < 1% or fieldnr% > 15% then goto line_summary
                    c% = l% + fieldnr%
                    if c% > maxlines% then goto line_summary
                    gosub L59100 /* Proc Hstry w/ line */
                    goto line_summary
L11920:     if fieldnr% < 1% or fieldnr% > 15% then line_summary
                c% = min(l% + fieldnr%, maxlines%)
                if c% = 0% then line_summary
                fieldnr% = c% - l%
            if keyhit%  = 12% then deletemode
            if keyhit% <> 24% then L11992
                gosub edit_text_line
                goto  line_summary
L11992:     if keyhit% <> 25% then L12010
                if part$(c%, 1%) = " " then line_summary
                     mostin% = 0%
                     change_date% = 1%
                     for fieldnr% = 1% to 3%
                          gosub input_li_pg2  /* Uses NEXT from there */
                          change_date% = 0%
                          goto line_summary
                     next fieldnr%   /* This is a dummy NEXT, not used! */

L12010:     gosub L14000   /* Describe Line Item Fields  */
                inpmessage$ = " "
                if appnd$(c%) <> "D" then edtpg3
                     inpmessage$ = "Reactivated Deleted Line Item"
                     appnd$(c%)  = " "

        edtpg3       /* Line Item Detail - First Screen      */
            if inpmessage$ = " " then inpmessage$ = edtmessage$
            errormsg$   = " "
            chg% = 2%
L12100:     fieldnr% = 0%
            gosub deffn_113
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then edtpg4
                  if keyhit% <>  6% then L12170      /* Prev Line  */
                                    c% = max(1%, c%-1%)
L12150:                             gosub L14000
                                    goto edtpg3
L12170:           if keyhit% <>  7% then L12200      /* Next Line  */
                                    c% = min(maxlines%, c%+1%)
                                    goto L12150
L12200:           if keyhit%  =  9% then       edtpg1
                  if keyhit%  = 16% then       L12740
                  if keyhit%  =  8% then gosub L59100
                  if keyhit%  = 10% then gosub L59150
                  if keyhit% <> 11% then L12228
                     kitmode% = 2%  :  gosub pj_kit_call
L12228:           if keyhit%  = 25% then gosub view_vendor_contracts
                  if keyhit% <>  0% and keyhit% <> 29% then L12100
            fieldnr% = cursor%(1%) - 4%
            if fieldnr% > 3% then fieldnr% = fieldnr% + 1%
            if fieldnr% = 3% and cursor%(2%) > 46% then fieldnr% = 4%
            if fieldnr% < 2%   or  fieldnr% > 17% then L12100
            if keyhit% <> 29% then L12300
                if admin% <> 1% then L12290
                call "ENABLSUB" ("MODIFY", "VBKINPUT", scr%(), set%(),   ~
                                 3%, fieldnr%, 0%, 0%)
L12290:         goto L12100
L12300:     if part$(c%,1%) = " " and fieldnr% >  2 then L12100

L12320:     gosub deffn_053
                if enabled% = 0% then edtpg3
            if fieldnr% <> 16% then L12340
                 temp$ = " ":gosub call_hnycdist
               goto L12370
L12340:     gosub deffn_113
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12340
L12370:     gosub deffn_153
                  if errormsg$ <> " " then L12340
                  if fieldnr% =  10% then L12384
                     inpmessage$ = edtmessage$
                     goto edtpg3

L12384:              inpmessage$ = edtmessage$
                     fieldnr% = 14%
                     goto L12320

        edtpg4       /* Line Item Detail - Second Screen      */
            inpmessage$ = edtmessage$
            errormsg$ = " "
L12440:     fieldnr% = 0%:gosub deffn_114
            gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then edtpg3
                  if keyhit% <>  6% then L12510
                                    c% = max(1%, c%-1%)
L12490:                             gosub L14000
                                    goto edtpg4
L12510:           if keyhit% <>  7% then L12540
                                    c% = min(maxlines%, c%+1%)
                                    goto L12490
L12540:           if keyhit%  =  9% then       edtpg1
                  if keyhit%  = 16% then       L12740
                  if keyhit%  =  8% then gosub L59100
                  if keyhit%  = 10% then gosub L59150
                  if keyhit% <> 11% then L12569
                     kitmode% = 2%  :  gosub pj_kit_call
L12569:           if keyhit%  = 25% then gosub view_vendor_contracts
                  if keyhit% <>  0% and keyhit% <> 29% then L12440
            fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 14% then L12440
            if fieldnr% = 14% then fieldnr% = 13%
            if keyhit% <> 29% then L12650
                if admin% <> 1% then L12630
                call "ENABLSUB" ("MODIFY", "VBKINPUT", scr%(), set%(),   ~
                                 4%, fieldnr%, 0%, 0%)
L12630:         goto L12440

L12650:     gosub deffn_054
                if enabled% = 0% then edtpg4
L12670:     gosub deffn_114
            gosub call_screen
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12670
            gosub deffn_154
                  if errormsg$ <> " " then L12670
            goto edtpg4

L12740:     l% = max(0%, min(85%, c%-1%))
            if maxlines% < 16 then l% = 0
            goto line_summary        /* Summary Screen   */

        insertmode
            mode% = 3%
            gosub insertlines
            mode% = 0%:edit% = 2%
            goto line_summary        /* Summary Screen   */

        deletemode   /* Flag line(s) for deletion                      */
            if keyhit% = 12% then delete% = 1% else delete% = 2%
            selectmsg$ = "To DELETE Blinking Line, press RETURN.  To" &  ~
                         " CANCEL DELETE, press PF1."
            if delete% = 2% then selectmsg$ =                            ~
                         "To DELETE ALL Line Items, press RETURN.  To" & ~
                         " CANCEL DELETE, press PF1."
            if delete% = 2% then fieldnr% = 0%
L12920:     gosub deffn_125              /* Get confirmation */
                if keyhit%  = 1 then line_summary
                if keyhit% <> 0 then L12920
            gosub check_recv
            if errormsg$ <> " " then L12920
            d1% = 1% : d2% = maxlines%
            if delete% = 1% then d1%, d2% = c%
            for c% = d1% to d2%
                appnd$(c%) = "D"
            next c%
            goto line_summary   /* Back to Summary Screen */


        inpvfs
            call "VFINPSUB" ("VBKMASTR", "I", "Manage Purchase Orders",  ~
                              "PO " & ponumber$, "NN", str(varfield$()), ~
                              keyhit%)
            if keyhit% = 1% then gosub startover2
            return

        edtvfs
            call "VFINPSUB" ("VBKMASTR", "E", "Manage Purchase Orders",  ~
                              "PO " & ponumber$, "YY", str(varfield$()), ~
                              keyhit%)
                 if keyhit%  =  1 then gosub startover2
                 if keyhit%  =  4 then       edtpg2
                 if keyhit%  =  5 then       edtpg1
                 if keyhit%  = 16 then gosub datasave
                                       goto  edtpg1

        check_recv
            if delete% = 1% then check_one
            rcvrinuse$ = str(vencode$,,9) & str(ponumber$) & hex(00000000)
            call "PLOWALTS" (#26, rcvrinuse$, 2%, 25%, f1%(26%))
            if f1%(26%) <> 0% then L13340
            call "PLOWALTS" (#29, rcvrinuse$, 2%, 25%, f1%(29%))
            if f1%(29%) = 0% then L13355
L13340:         errormsg$ = "There is Receiving Activity Against this" & ~
                            " PO, Cannot DELETE Entire PO."
                return
L13355:     if pj_on% <> 1% then return
            if maxlines% = 0% then return
                for i% = 1% to maxlines%
                     if pj_stat%(i%) = 5% or pj_stat%(c%) = 6% then L13385
                next i%
                return
L13385:     errormsg$ = "There is Purchase Job Activity Against this PO"&~
                        ", Cannot DELETE Entire PO."
            return

        check_one
            rcvrinuse$ = str(vencode$,,9) & str(ponumber$,,16) &         ~
                         seqnr$(c%) & hex(00000000)
            call "PLOWALTS" (#26, rcvrinuse$, 2%, 28%, f1%(26%))
            if f1%(26%) <> 0% then L13470
            call "PLOWALTS" (#29, rcvrinuse$, 2%, 28%, f1%(29%))
            if f1%(29%) = 0% then L13485
L13470:         errormsg$ = "There is Receiving Activity Against this" & ~
                            " Line Item, Cannot DELETE!"
                return
L13485:     if pj_on% <> 1% then return
            if pj_stat%(c%) <> 5% and pj_stat%(c%) <> 6% then return
                errormsg$ = "There is Purchase Job Activity Against th" &~
                            "is Line Item, Cannot DELETE!"
                return

        call_screen
            call "GETSCRN" ("C", " ", cursor%(), 0%)
            return

        daterun_data_swap
            qtyorig(c%)   = daterun_qty(i%)
            qtyonord(c%)  = daterun_qty(i%)
            duedate$(c%)  = daterun_date$(i%)
            origdue$(c%)  = daterun_date$(i%)
            nextdate$(c%) = daterun_date$(i%)
            notbefore$(c%), notafter$(c%) = " "
            temp$ = duedate$(c%)
            call "DATUNFMT" (temp$)
            if notbefore$(c% - 1%) = " " or ~
                notbefore$(c% - 1%) = blankdate$ then L13774
                call "DATE" addr("G+", temp$, nb_offset%, notbefore$(c%),~
                                                                     u3%)
                call "DATEFMT" (notbefore$(c%))
L13774:     if notafter$(c% - 1%) = " " or ~
                notafter$(c% - 1%) = blankdate$ then L13784
                call "DATE" addr("G+", temp$, na_offset%, notafter$(c%), ~
                                                                     u3%)
                call "DATEFMT" (notafter$(c%))
L13784:     return

        REM *************************************************************~
            * Processing of DATERUN stuff happens here.                 *~
            * Basically, after calling DATERUN, we loop through the     *~
            * Copy line function however many times we need while       *~
            * swapping the date and quantity per our arrays.            *~
            *************************************************************
        daterun_process
            if daterun_mode% = 1% then L13910
                how_many% = 100% - maxlines% /* In as lines available */
                daterun_mode% = 1%
*            1st time thru, calculate offsets for Not After & Not Before
                nb_offset%, na_offset% = 0%
                if notbefore$(c% - 1%) = " " or ~
                    notbefore$(c% - 1%) = blankdate$ then L13850
                  temp1$ = duedate$(c% - 1%)   : call "DATUNFMT" (temp1$)
                  temp2$ = notbefore$(c% - 1%) : call "DATUNFMT" (temp2$)
                  call "DATE" addr("G-", temp1$, temp2$, nb_offset%, u3%)
L13850:         if notafter$(c% - 1%) = " " or ~
                    notafter$(c% - 1%) = blankdate$ then L13860
                  temp1$ = duedate$(c% - 1%)   : call "DATUNFMT" (temp1$)
                  temp2$ = notafter$(c% - 1%)  : call "DATUNFMT" (temp2$)
                  call "DATE" addr("G-", temp1$, temp2$, na_offset%, u3%)
L13860
*            Next, call DATERUN to gather qtys and dates
                call "VBKDATRN" (vencode$, vendescr$, part$(c% - 1%, 1%),~
                                partdescr$(c% - 1%), how_many%,          ~
                                daterun_qty(), daterun_date$(),          ~
                                #23, #08, #24, #33, #21, #34, #35, #36,  ~
                                #25, #02, ret%)
                i% = 0%             /* Counter based on # of qty/dates */
                if ret% <> 99% then L13910
                     gosub columnone
                     daterun_mode% = 0%
                     goto inputlines
L13910
*        The real process begins
                i% = i% + 1%
                if i% <= how_many% then L13960
                     keyhit% = 16% /* Get out of insert lines loop */
                     return
L13960:         gosub copy_prev_line
                return

L14000: REM *************************************************************~
            * LINE ITEM DESCRIPTION SECTION WITH PRICES & QUANTITIES 2! *~
            *************************************************************

            init (" ") strdescr$, catdescr$, acctdescr$, jobdescr$,      ~
                       statusdescr$, pcacctdescr$, stkg_descr$

            if part$(c%,1%) = " " then L14280

*        Describe STORE NUMBER
            call "DESCRIBE" (#19, str$(c%), strdescr$, 1%, f1%(19))

*        Describe PART CATEGORY
            call "DESCRIBE" (#15, cat$(c%), catdescr$, 1%, f1%(15))

*        Describe Stocking UOM
            readkey$ = "UOM      " & stkg$(c%)
            call "DESCRIBE" (#28, readkey$, stkg_descr$, 0%, f1%(28))

*        Describe EXPENSE ACCOUNT/PC ACCOUNT
            temp$ = acct$(c%)
            call "GLUNFMT" (temp$)
            call "DESCRIBE" (#16, temp$, acctdescr$, 1%, f1%(16))
            temp$ = pcacct$(c%)
            call "GLUNFMT" (temp$)
            call "DESCRIBE" (#16, temp$, pcacctdescr$, 1%, f1%(16))

*        Describe SPECIFIC JOB/PROJ
            job% = 8%
            if job$(c%) = "PJ" then L14250
            if job$(c%) <> " " then                                      ~
                call "DESCRIBE" (#18, job$(c%), jobdescr$, 1%, f1%(18))
            goto L14260
L14250:     jobdescr$ = "(Future Purchased Job)"  :  job% = 2%

L14260
*        Describe LINE STATUS
            gosub describe_status

L14280
*        Format Numeric Fields
            gosub deffn_040
            gosub deffn_041
            return

        describe_status
            if status$(c%) = "A" then statusdescr$ = "(Active)         "
            if status$(c%) = "C" then statusdescr$ = "(Closed)         "
            if status$(c%) = "H" then statusdescr$ = "(On Hold)        "
            if status$(c%) = "W" then statusdescr$ = "(Awaiting Price) "
          return

        REM *************************************************************~
            * COMMON CONVERT FLOATING POINT TO STRING SECTION           *~
            *************************************************************

            deffn_040

            init (" ") factor$, extg$(c%), ext$(c%), hnycost$,           ~
              qtyorig$, qtyonord$, qtyrecd$, ourprice$, sumonord$(c%,1%),~
              venorig$, venonord$, venrecd$, venprice$, sumonord$(c%,2%),~
              cur_descr$()

            if part$(c%,1%) = " " then return

                call "CONVERT" (qtyorig(c%), 0.4, qtyorig$)
                call "CONVERT" (qtyonord(c%),0.4, qtyonord$)
                call "CONVERT" (qtyrecd(c%), 0.4, qtyrecd$)
                call "CONVERT" (price(c%), 2.7, ourprice$)
                if price(c%) = 0 then ourprice$ = " NO CHARGE"
                call "CONVERT" (hnycost(c%), 2.4, hnycost$)

                venorig   = round(qtyorig  (c%)/factor(c%), 7)
                venonord  = round(qtyonord (c%)/factor(c%), 7)
                venrecd   = round(qtyrecd  (c%)/factor(c%), 7)  /*NO CHG*/
                venprice  = round(price    (c%)*factor(c%), 4)

                totvencost = round(venprice*venorig,2)
                totourcost = round(price(c%)*qtyorig(c%),2)
                totvennet  = round(venprice*venonord,2)
                totournet  = round(price(c%)*qtyonord(c%),2)

                call "CONVERT" (factor(c%), -0.4, factor$)
                call "CONVERT" (venorig , 0.7, venorig$ )
                call "CONVERT" (venonord, 0.7, venonord$)
                call "CONVERT" (venrecd , 0.7, venrecd$ )
                call "CONVERT" (venprice, 2.4, venprice$)
                if venprice = 0 then venprice$ = " NO CHARGE"

                call "CONVERT" (totourcost, 2.2, extg$(c%))
                if totourcost = 0 then extg$(c%) = " NO CHARGE"
                   if abs(totvencost - totourcost) < .01 then L14790
                      str(extg$(c%),,1) = "*"

L14790:         call "CONVERT" (totournet, 2.2, ext$(c%))
                   if abs(totvennet - totournet) < .01 then L14830
                      str(ext$(c%),,1) = "*"

L14830:         sumonord$(c%,1%) = qtyonord$

                sumonord$(c%,2%) = venonord$

                if cur_loaded% = 0% then return
                   cur_descr$(1) = cur_stat$ & " Equivalent"
                   call "CONVERT" (cur_factor2, 2.7,                     ~
                                               str(cur_descr$(5), 1,10))
                      cur_descr$(5) = cur_descr$(5) & " " & cur_tran$
                      cur_descr$(5) = cur_descr$(5) & "/" & cur_stat$
                   cur_descr$(4) = cur_stat$
                   call "CONVERT" (totourcost * cur_factor1, 2.2,        ~
                                               str(cur_descr$(3), 1,10))
                   call "CONVERT" (totournet  * cur_factor1, 2.2,        ~
                                               str(cur_descr$(3),16,10))
                   call "CONVERT" (venprice   * cur_factor1, 2.4,        ~
                                               str(cur_descr$(2), 1,10))
                   call "CONVERT" (price(c%)  * cur_factor1, 2.7,        ~
                                               str(cur_descr$(2),16,10))
            return

        call_hnycdist
             if postatstdcost$ = "Y" and setid$ <> " " and edit% <> 2%   ~
                then goto call_stccosts
             if postatstdcost$ = "Y" and setid$ <> " " and edit% = 2%    ~
                and pos("FST" = costtype$(c%)) > 0 then temp$ = "E"
        call_hnycdist2
             call "HNYCDIST" (temp$, part$(c%,1%), partdescr$(c%), " ",  ~
                              #2, hnycostd$(c%), hnycost$, hnycost(c%))

             return
        deffn_041
            if part$(c%,1%) = " " then return
                factor$ = " "
                call "CONVERT" (factor(c%), -0.4, factor$)
                return

        call_stccosts
             if ro% = 1% then L15270   /* Cutover from RO */
             if pos("FST" = costtype$(c%)) = 0 then call_hnycdist2
*           IF PJ_ON% = 1% AND PJ_STAT%(C%) > 2% THEN CALL_HNYCDIST2
             if edit% <> 1% then return

L15270:      mat invcost = zer : hnycost(c%) = 0
             if job$(c%) = "PJ" then L15280
                if core_on% <> 1% then L15280
                     gosub get_reman_core_std_cost
                     if reman% = 0% then L15280
                     return
L15280:      call "STCCOSTS" (part$(c%, 1%), setid$, #2, 3%,             ~
                              hnycost(c%), invcost(), a_bom(), a_rte(),  ~
                              a_dtl())
             if job$(c%) <> "PJ" then L15300
                mat invcost = a_dtl
                hnycost(c%) = 0
                for i% = 1% to 12%
                     hnycost(c%) = hnycost(c%) + a_dtl(i%)
                     next i%
L15300:      call "PACKZERO" (invcost(), str(hnycostd$(c%)))
             call "CONVERT" (hnycost(c%), 2.4, hnycost$)
             return

            std_cost_warning
                if pos("FST" = costtype$(c%)) = 0% then return
                     call "STCCOSTS" (part$(c%, 1%), " ", #2, 1%, temp)
                     if temp <> 0 then return
                u3% = 0%
                call "ASKUSER" (u3%, "** ZERO COST WARNING **",          ~
                     part$(c%,1%) & " is a Standard Costed Part,",       ~
                     "but its Standard Cost is Zero!",                   ~
                     "Press RETURN to Acknowledge...")
                return

        REM *************************************************************~
            *  For Reman Part Determine Standard cost as a sum of the   *~
            *  Reman's Standard Cost and the Implied Core's Std Cost    *~
            *************************************************************
        get_reman_core_std_cost
            init (" ")  remanpart$
            remanpart$ = part$(c%, 1%)
            call "PLOWALTS" (#39, remanpart$, 0%, 25%, reman%)
                if reman% = 0% then return
                if str(remanpart$,26%,25%) <> " " then L15480
                     reman% = 0%
                     return
L15480:     call "STCCOSTS" (str(remanpart$, 1%,25%), " ", #2, 1%, reman)
            call "STCCOSTS" (str(remanpart$,26%,25%), " ", #2, 1%, core)

            hnycost(c%) = reman + core
            call "CONVERT" (hnycost(c%), 2.4, hnycost$)
            temp$ = "I"
            gosub call_hnycdist2
            return

        vsa_bucket
*        If this is a VSA and the price is changing, the inventory cost
*        needs to change too.  This section of code is for determining
*        which cost bucket to put the new cost in and priming HNYCOSTD$

            if job$(c%) = " " or str(part$(c%,1%),,9%) <> "ACTIVITY:"    ~
                                                              then return
                call "READ100" (#18, job$(c%), f1%(18%))
                     if f1%(18%) = 0% then return
                get #18 using L15660, readkey$
L15660:              FMT POS(58), CH(25)
                str(readkey$,26%,4%) = str(part$(c%,1%),11%,4%)
                call "READ100" (#45, readkey$, f1%(45%))
                     if f1%(45%) = 0% then return
                get #45 using L15710, vsa_bucket%
L15710:              FMT POS(53), BI(4)

                init (hex(00)) hnycostd$(c%)
                put str(hnycostd$(c%),(vsa_bucket%-1)*8+1,8%),           ~
                                                     using L15750, temp
L15750:              FMT PD(14,4)
                hnycost(c%) = temp
                vsa_bucket_chg% = 1%  /* Don't need to call HNYCDIST  */
                                    /* since all 3 cost vars are set. */
                return

        REM *************************************************************~
            *               E D I T   T E X T                           *~
            * --------------------------------------------------------- *~
            * Allow editing of Vendor Text.                             *~
            *************************************************************
        edit_text_hdr
            textmsg$ = "Vendor: " & vencode$ & ", PO: " & ponumber$
            call "TXTINSUB" (#20, f2%(20), "003", textmsg$, textid$,     ~
                                                               texta$())
            return

        edit_text_line
            textmsg$ = "Vendor: " & vencode$ & ", PO: " & ponumber$ &    ~
                       ", Part: " & partdescr$(c%)
            call "TXTINSUB" (#20, f2%(20), "004", textmsg$, textidl$(c%),~
                                                               texta$())
            return

        REM *************************************************************~
            * Search Master for POs on file.                            *~
            *************************************************************
        pomstr
            errormsg$ = " "
            readkey$  = str(vencode$) &  ponumber$
            call "GETCODE" (#4, readkey$, venaddr$(1), 0%, 0, f1%(4))
            if f1%(4) = 0% then return
                vencode$  = str(readkey$,,9)
                ponumber$ = str(readkey$,10)
                return

        show_porlse
            errormsg$ = " "
            selectmsg$ = hex(06) & "Select a Vendor Code for Buyer: " &  ~
                defbuyer$
            if vencode$ = "?" then vencode$ = " "
            plowkey$ = "R" & str(defbuyer$) & str(vencode$) & hex(00)
            init (" ") hdr$(), ie$()
            hdr$(1) = "  Vendor #  Vendor Name"
            mat dmap = zer   :  mat ie = zer
            dmap(1) =   5.09 : dmap(2) =   1
            dmap(3) = -10.30 : dmap(4) =  11
            call "PLOWCODE" (#22, plowkey$, selectmsg$,  9004%,  .3,     ~
                f1%(22), hdr$(), 9, 0, ie(), ie$(), "d", " ", #03,       ~
                dmap()) /* PORLSE file */
            if f1%(22) <> 0% then goto L18450
                errormsg$ = "You must select a Vendor Code."
                return
L18450:     get #22 using L18460, vencode$
L18460:         FMT POS(5), CH(9) /* PORLSE */
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Save this sucker after Maximum Dollar verification.       *~
            *************************************************************

        datasave
            if maxdlrs$ = " " then L19140
                gosub total_po
                convert maxdlrs$ to maxdlrs
                if totalpo <= maxdlrs then L19140
                     gosub toomanydollars
                     return

L19140
*        Get how to Print Purchase Order.
            keyhit1% = popropt + 5
                if keyhit1% > 5% and keyhit1% < 9% then L19240

L19150:     keyhit1% = 2%
            pf$(1) = "Please specify how to print this P.O. "        &   ~
                     " Enter PF-9 to return to Edit of P.O. "
            pf$(2) = "                                      "        &   ~
                     "                                      "
            pf$(3) = "(6)Don't Print   (7)Print (Standard)  "        &   ~
                     "(8)Print for Current Revision Level   "
            call "ASKUSER" (keyhit1%, "PO PRINT OPTIONS",                ~
                                                 pf$(1), pf$(2), pf$(3))
                if keyhit1% = 9% then return
                if keyhit1% < 6% or keyhit1% > 8% then L19150

L19240:              poprnt$     = " "
                     if keyhit1% = 6% then poprnt$ = "N"
                     if keyhit1% = 7% then poprnt$ = "S"
                     if keyhit1% = 8% then poprnt$ = "R"

            if ponumber$ = " " then gosub pogenerate
            call "SHOSTAT" ("Updating Purchasing Data Files")
            return clear all

            gosub L31000        /* Move PO to Buffers */

            lastven$ = vencode$
            lastpo$  = ponumber$
            goto inputmode

        total_po
            totalpo = 0
            if maxlines% = 0% then L19451           /* No line items */
            for x% = 1% to maxlines%
              if appnd$(x%)    = "D" then L19450    /* Deleted Line     */
              if part$(x%,1%) = " " then L19450     /* Descriptive Line */
                ext     = round(qtyonord(x%) * price(x%), 2)
                totalpo = round(totalpo + ext , 2)
L19450:     next x%
L19451:     call "CONVERT" (totalpo, 0.2, pototal$)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets Defaults and Enables = Input and Edit Mode.          *~
            *************************************************************

            deffn_051
                call "ENABLSUB" ("SET", "VBKINPUT", scr%(), set%(), 1%,  ~
                                 fieldnr%, abs(edit%), enabled%)
                if edit% = 2% and enabled% = 0% then return
                inpmessage$ = " "
                  on fieldnr% goto  L20115,         /* VENDOR CODE      */~
                                    L20210,         /* Buy From ID      */~
                                    L20255,         /* PONUMBER         */~
                                    L20285,         /* Name & Address   */~
                                    L20330,         /* Vendor Type      */~
                                    L20365,         /* Contact's Name   */~
                                    L20410,         /* DEFSTORE         */~
                                    L20450,         /* Revision Level   */~
                                    L20470,         /* Revision Date    */~
                                    L20510          /* Buyer Code       */
                  return

L20115
*        Default / Enable for VENDOR CODE
            errormsg$ = " "
            if defbuyer$ = " " then L20190
                oldreadkey$ = "R" & str(defbuyer$,,3) & hex(00)
L20135:         call "PLOWNEXT" (#22, oldreadkey$, 4%, f1%(22))
                     if f1%(22) <> 0% then L20170
                     errormsg$ = "No More Purchase Directives F"   &     ~
                                 "or This Buyer: " & defbuyer$
                     if defbuyer$ = userid$ then L20190
                     return clear all
                     goto L09700
L20170:         if str(oldreadkey$,48,19) <> " " then L20135
                if str(oldreadkey$,39,6)   > deforddate$ then L20135
                     vencode$ = str(oldreadkey$,5,9) /* Req Found      */
L20190:     inpmessage$ = "To Recall by Purchase Order Number, Leave"  & ~
                          " Vendor Code blank."
            return

L20210
*        Default / Enable for VENDOR BUY FROM
            inpmessage$ = "Enter Buy From ID. (Enter partial code to" &  ~
                          " initiate search.)"
            if edit%   <> 1% then return
                readkey$ = str(vencode$) & hex(00)
                call "PLOWNEXT" (#14, readkey$, 9%, f1%(14))
                enabled%   = f1%(14)
                if f1%(14) = 0% then bfcode$, bfdescr$ = " "
                return

L20255
*        Default / Enable for PURCHASE ORDER NUMBER
            if ponumber$ <> " " then enabled% = 0%
            inpmessage$ = "Enter Purchase Order Number."
            if poassgn$ <> "m" then inpmessage$ = inpmessage$ &          ~
                                "  Leave blank for Automatic Generation."
            return

L20285
*        Default / Enable for VENDOR NAME & ADDRESS (Buy From).
            inpmessage$ = "Enter Vendor Buy From Name and Address."
            if buyshp%  = 1% then                                        ~
                inpmessage$ = "Leave Ship To Address Blank to default " &~
                              "to Store " & defstr$ & "'s Address."
            if edit%   <> 1% then return
              get #3 using L20305, venaddr$(), tempp$
L20305:             FMT XX(39), 6*CH(30), POS(240), CH(10)
              if f1%(14) = 1% then get #14 using L20315, venaddr$(), tempp$
L20315:             FMT XX(45), 6*CH(30), POS(246), CH(10)
              gosub format_phone
                return

L20330
*        Default / Enable for VENDOR TYPE
            inpmessage$ = "Enter Vendor Type."
            if edit%   <> 1% then return
                get #3  using L20350, ventype$
L20350:              FMT XX(476), CH(4)
                return

L20365
*        Default / Enable for CONTACT'S NAME
            inpmessage$ = "Enter Vendor's Contact (or '?' to select from ~
        ~list)."
            if edit%   <> 1% then return
            get #3 using L20385, vencont$
L20385:         FMT XX(219), CH(20)
            if f1%(14) = 1% then get #14 using L20395, vencont$
L20395:         FMT XX(225), CH(20)
            return

L20410
*        Default / Enable for DEFAULT STORE NUMBER
            inpmessage$ = "Enter Default Store Number."
            if edit%   <> 1% then return
                defstr$ = userstr$
                call "DESCRIBE" (#19, defstr$, defstrdescr$, 1%, f1%(19))
                if f1%(19) = 0% then enabled% = 1%
                return

L20450
*        Default / Enable for REVISION LEVEL
            inpmessage$ = "Enter Revision Level."
            return

L20470
*        Default / Enable for REVISION DATE
            inpmessage$ = "Enter Revision Date."
            if edit% = 1% and rev_level$ = " " then enabled% = 0%
            if (rev_date$ <> " " and rev_date$ <> blankdate$) ~
                or rev_level$ = " " then return
                rev_date$ = date
                call "DATEFMT" (rev_date$)
                return

L20510
*        Default / Enable for BUYER CODE
            inpmessage$ = "Enter Buyer/Planner Code."
            if edit%   <> 1% then return
                if defbuyer$ = " " then buyer$ = userid$                 ~
                                   else buyer$ = defbuyer$
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            * --------------------------------------------------------- *~
            * Sets Defaults and Enables - both Input and Edit Mode.     *~
            *************************************************************

            deffn_052
                call "ENABLSUB" ("SET", "VBKINPUT", scr%(), set%(), 2%,  ~
                                 fieldnr%, abs(edit%), enabled%)
                if edit% = 2% and enabled% = 0% then return
                inpmessage$ = " "
                on fieldnr% goto    L21125,         /* Liability Acct   */~
                                    L21235,         /* Taxable          */~
                                    L21280,         /* Frt Terms        */~
                                    L21325,         /* FOB              */~
                                    L21370,         /* Ship Via         */~
                                    L21415,         /* ORDER DATE       */~
                                    L21470,         /* DEFAULT DUE DATE */~
                                    L21505,         /* CANCELLATION DATE*/~
                                    L21535,         /* CONFIRMED? (Y/N) */~
                                    L22070,         /* Maximum Dollars  */~
                                    L22110,         /* Maximum Date     */~
                                    L22200          /* Copy Text ID     */
                     return

L21125
*        Default / Enable for LIABILITY ACCOUNT
*        Get first from Remit-to (VENDOR) and over-ride with Buy From
*        (VENDBF). If blank, use Store Defaults (STORDFLT).
         inpmessage$ = "Enter Purchases Liability Account."
        determine_acct
         if edit% <> 1% then return
            if f1%(14) = 1% then get #14 using L21160, expacct$, puracct$
L21160:          FMT XX(318), 2*CH(9)
            if expacct$ = " " then get #3, using L21170, expacct$
L21170:          FMT XX(249), CH(9)
            if puracct$ = " " then get #3, using L21180, puracct$
L21180:          FMT POS(481), CH(9)
            if puracct$ = " " then puracct$ = strpuracct$
            if expacct$ = " " then expacct$ = strexpacct$
            call "DESCRIBE" (#16, puracct$, puracctdescr$, 1%, f1%(16))
            call "GLFMT" (puracct$)
            return

L21235
*        Default / Enable for TAXABLE FLAG
            inpmessage$ = "Indicate Tax Status (Y -or- N)."
            if edit%   <> 1% then return
                                     get #3  using L21260, taxflag$
                if f1%(14) = 1% then get #14 using L21265, taxflag$
L21260:             FMT XX(445), CH(1)
L21265:             FMT XX(317), CH(1)
                return

L21280
*        Default / Enable for FREIGHT TERMS
            inpmessage$ = "Indicate Freight Terms (N, A, or C)."
            if edit%   <> 1% then return
                                     get #3  using L21305, frgtflag$
                if f1%(14) = 1% then get #14 using L21310, frgtflag$
L21305:             FMT XX(414), CH(1)
L21310:             FMT XX(256), CH(1)
                return

L21325
*        Default / Enable for FOB.
            inpmessage$ = "Enter F.O.B. Description."
            if edit%   <> 1% then return
                                      get #3  using L21350, fob$
                if f1%(14) = 1% then  get #14 using L21355, fob$
L21350:             FMT XX(415), CH(30)
L21355:             FMT XX(257), CH(30)
                return

L21370
*        Default / Enable for SHIP VIA
            inpmessage$ = "Enter 'Ship Via' Description."
            if edit%   <> 1% then return
                                      get #3  using L21395, shipvia$
                if f1%(14) = 1% then  get #14 using L21400, shipvia$
L21395:             FMT XX(446), CH(30)
L21400:             FMT XX(287), CH(30)
                return

L21415
*        Default / Enable for ORDER DATE
            if orddate$ = " " or orddate$ = blankdate$ then orddate$ = paydate$
            inpmessage$ = "Enter Order Date."
            if cur_tran$ = " " then L21425
            if cur_tran$ = cur_stat$ then L21425
               enabled% = 0%
               return
L21425:     if edit%   <> 1% then return
                if defbuyer$ = " " or mindate$ = unfmaxdt$ then L21455
                     orddate$ = mindate$
                     if orddate$ = " " or orddate$ = blankdate$ then L21455
                     call "DATEFMT" (orddate$)
                     return
L21455:         orddate$ = paydate$
                return

L21470
*        Default / Enable for DEFAULT DUE DATE
            inpmessage$ = "Enter Default Due Date."
            if edit%   <> 1% then return
                if defbuyer$ = " " or lstdate$ = unfmaxdt$ then return
                  if lstdate$ = " " or lstdate$ = blankdate$ then return
                     duedate$ = lstdate$ : call "DATEFMT" (duedate$)
                     return

L21505
*        Default / Enable for CANCELLATION DATE
            inpmessage$ = "Enter Cancellation Date."
            if edit%   <> 1% then return
                candate$ = duedate$
                return

L21535
*        Default / Enable for CONFIRMED?
            inpmessage$ = "Enter Confirmation Status (Y or N)."
            if edit%   <> 1% then return
                                     get #3  using L22030, confirm$
                if f1%(14) = 1% then get #14 using L22040, confirm$
L22030:              FMT XX(413), CH(1)
L22040:              FMT XX(255), CH(1)
                return

L22070
*        Default / Enable for MAXIMUM DOLLAR AMOUNT
            inpmessage$ = "Enter Maximum Monetary Amount "               ~
                        & "(Blank = no limit)"
            return

L22110
*        Default / Enable for MAXIMUM DATE
            inpmessage$= "Enter Last Date To Allow P.O. Modification."
            return

L22200
*        Default / Enable for COPY TEXT ID
            inpmessage$ = "Enter Copy Element Name(s) or '?' for List."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            * --------------------------------------------------------- *~
            * Sets Defaults and Enable for Input and Edit- Line Scrn 1. *~
            *************************************************************

            deffn_053
                call "ENABLSUB" ("SET", "VBKINPUT", scr%(), set%(), 3%,  ~
                                 fieldnr%, abs(edit%), enabled%)
                if edit% = 2% and enabled% = 0% then return
                inpmessage$, errormsg$ = " "
                if fieldnr% > 2% and part$(c%,1%) = " " then enabled% = 0%
                if fieldnr% > 2% and part$(c%,1%) = " " then return
                  on fieldnr% goto  L23290,         /* PART NUMBER      */~
                                    L23340,         /* PART DESCRIPTION */~
                                    L23380,         /* STORE NUMBER     */~
                                    L23450,         /* LOT CODE         */~
                                    L23490,         /* PART CATEGORY    */~
                                    L23570,         /* VEN PART NUMBER  */~
                                    L23770,         /* UNIT OF ISSUE    */~
                                                   /* QUANTITY PER UNIT*/~
                                    L24410,         /* Purchase Contract*/~
                                    L24470,         /* Job/Project Code */~
                                    L23902,         /* TRANSACTION CURR.*/~
                                    L23920,         /* ORIGINAL ORDER   */~
                                    L23990,         /* ON ORDER         */~
                                    L24070,         /* REC LAST SHPMNT  */~
                                    L24150,         /* PURCHASE PRICE   */~
                                    L24250,         /* EXTENSION        */~
                                    L24300,         /* Inventory Cost   */~
                                    L24350          /* Line Status      */
                     return

L23290
*        Default / Enable for PART NUMBER
            inpmessage$ = "Enter Part Number (Leave Blank for"   &       ~
                          " Comment Line)."
            return

L23340
*        Default / Enable for  PART DESCRIPTION
            inpmessage$ = "Enter Part Description (or Comment)."
            return

L23380
*        Default / Enable for  STORE NUMBER
            inpmessage$  = "Enter Store Number."
            if edit%    <> 1% then return
                str$(c%)  = defstr$
                strdescr$ = defstrdescr$
                return

L23450
*        Default / Enable for  LOT CODE
            inpmessage$  = "Enter Lot Number."
            call "LOTENABL" (part$(c%,1), le%, ll%, #2, #8)
            if le% = 0% then enabled% = 0%
            if le% = 0% then lot$(c%) = " "
            return

L23490
*        Default / Enable for PART CATEGORY
            inpmessage$ = "Enter Category Code."
            if edit%   <> 1% or f1%(8) = 0% then return
                get #8, using L23530, cat$(c%)
L23530:              FMT POS(90), CH(4)
                call "DESCRIBE"(#15, cat$(c%), catdescr$, 1%,f1%(15))
                return

L23570
*        Default / Enable for VENDOR PART NUMBER
            inpmessage$ = "Enter Vendor's Part Number."
            if edit% <> 1 then return
            venprce = 0
            if part$(c%,2%) <> " " then L23630
L23610:         call "VPRPSLCT" (part$(c%,1%), vencode$, part$(c%,2%),   ~
                                               1%, " ", #11, #8, #3, #10)
L23630:         call "READ100" (#11, str(part$(c%,1%),,25) &             ~
                                     str(vencode$,,9) &                  ~
                                     str(part$(c%,2%),,25), f1%(11))
                if f1%(11) = 0 then L23750
                     testdate$ = orddate$
                     call "DATUNFMT" (testdate$)
                     if testdate$ = " " or testdate$ = blankdate$ then testdate$ = date
                     get #11, using L23680, venprce, meas$(c%),factor(c%),~
                                  nextprice, effdate$, expdate$, cur_vpm$
                     if cur_vpm$ = " " then cur_vpm$ = cur_stat$
                     if c% < 2% then cur_tran$ = cur_vpm$
                     if effdate$ = " " or effdate$ = blankdate$ then L23700
                     if testdate$ < effdate$ or                          ~
                        testdate$ > expdate$ then L23700
                        venprce = nextprice
L23680:                  FMT POS(69),PD(14,4),POS(89),CH(4),PD(14,4),    ~
                             PD(14,4), CH(6), CH(6), POS(158), CH(4)
L23700:              if factor(c%) <= 0 or factor(c%) > 1000000          ~
                                                      then factor(c%) = 1
                     factor(c%) = round(factor(c%),4)
L23722:              convert_price% = 0%
                     if cur_stat$ = " " or c% < 2% then L23740
                          if cur_tran$ = cur_vpm$ then L23740
                               gosub L23751
                               call "ASKUSER" (convert_price%,           ~
                                        "CURRENCY CONFLICT", ask_msg$(1),~
                                         ask_msg$(2), ask_msg$(3))
                               if convert_price% = 0% then L23610
                               if convert_price% <> 1% then L23722
L23740:              enabled%   = 0%
                     gosub deffn_041
L23750:         return
L23751:     ask_msg$(1) = "You have chosen a price in " & cur_vpm$ & "."
            ask_msg$(2) = "But the PO is in " & cur_tran$ & "."
            ask_msg$(3) = "Press RETURN to re-select -or- PF1 to  " &    ~
                       "convert the selected price to " & cur_tran$ & "."
            return

L23770
*        Default / Enable for UNIT OF ISSUE and QUANTITY PER UNIT
            inpmessage$ = "Enter Unit Of Measure to be Printed On"  &    ~
                          " the P.O. and Quantity Per One Unit."
            if edit%   <> 1% then L23840
                if meas$(c%) = " " then meas$(c%) = stkg$(c%)
*              RETURN

L23840
*        Default / Enable for QUANTITY PER UNIT
*          INPMESSAGE$ = "Enter Quantity Contained In One Unit."
            if edit%   <> 1% then L23890
                if factor(c%) <> 0% then L23885
                   factor(c%) = 1
L23885:         gosub deffn_041
L23890
*          CALL "STRING" ADDR("LJ", FACTOR$, 10%)
            return

L23902
*        Default / Enable for TRANSACTION CURRENCY
            inpmessage$ = "Enter Transaction Currency for this Document."
                if cur_tran$ = " " then cur_tran$ = cur_vend$
                if cur_tran$ = " " then cur_tran$ = cur_stat$
            if cur_stat$ = " " then enabled% = 0%
            if c% > 1% then enabled% = 0%
            if maxlines% > 1% then enabled% = 0%
            if edit% = 2% then enabled% = 0%
            if enabled% = 0% then inpmessage$ = " "
            if enabled% = 0% then L23914


L23914:     return

L23920
*        Default / Enable for ORIGINAL ORDER
            if edit% <> 1% then L23950
                 tmpprce = venprce
                 if convert_price% = 0% then L23948
                     if cur_vpm$ = cur_stat$ then L23940
                        /* Here we need to get rates to convert to stat */
                          call "CURRATSB" (cur_vpm$, cur_po_table$,      ~
                                orddate$, cur_factor3, temp, errormsg$)
                          tmpprce = round(tmpprce * cur_factor3, 4)
                   /* Now convert statutory to transaction price */
L23940:              tmpprce = round(tmpprce * cur_factor2, 4)
L23948:          price(c%)  = round(tmpprce/factor(c%),7)
L23950:     gosub deffn_040
            inpmessage$ = "Enter Original Order Vendor Units."
            call "STRING" addr("LJ", qtyorig$, 10%)
            call "STRING" addr("LJ", venorig$, 10%)
            return

L23990
*        Default / Enable for  REMAINING ON ORDER
            inpmessage$ = "Enter Quantity Remaining On Order."
            if edit%   <> 1% then L24030
                qtyonord(c%) = qtyorig(c%) : gosub deffn_040
L24030:     call "STRING" addr("LJ", venonord$, 10%)
            call "STRING" addr("LJ", qtyonord$, 10%)
            return

L24070
*        Default / Enable for  RECIEVED LAST SHIPMENT
            inpmessage$ = "Enter Quantity Received (Last Shipment)."
            if edit%   <> 1% then L24110
                qtyrecd (c%) = 0  :  gosub deffn_040
L24110:     call "STRING" addr("LJ", venrecd$, 10%)
            call "STRING" addr("LJ", qtyrecd$, 10%)
            return

L24150
*        Default / Enable for PURCHASE PRICE
            inpmessage$ = "Enter Purchase Price per Unit. (NC for No" &  ~
                          " Charge.)"
            if edit%   <> 1% then L24210
                if abs(price(c%)) < .0000001 then enabled% = 1%
*              VENPRICE$, OURPRICE$ = " "
L24210:     call "STRING" addr("LJ", venprice$, 10%)
            call "STRING" addr("LJ", ourprice$, 10%)
            return

L24250
*        Default / Enable for EXTENSION
            inpmessage$ = "Enter Line Extension."
            call "STRING" addr("LJ", extg$(c%), 10%)
            return

L24300
*        Default / Enable for Inventory Costs
          temp$ = " "
          if edit% <> 1% and edit% <> -1% then return
             if pj_on% = 1% and pj_stat%(c%) > 2% then L24310
             if postatstdcost$ = "Y" and setid$ <> " " and               ~
                pos("FST" = costtype$(c%)) > 0 then goto L24315
L24310:      temp = price(c%)
             if cur_loaded% <> 0% then temp = temp * cur_factor1
             call "CONVERT" (temp, 4.4, hnycost$)
L24315:      temp$ = "I"
             gosub call_hnycdist
             if temp$ = "E" then enabled% = 0%
             return

L24350
*        Default / Enable for Line Item Status
            inpmessage$ = "Status Codes: (A)ctive (H)old (C)losed (W)" & ~
                          "aiting for Price."
            if status$(c%) = " " then status$(c%) = "A"
            return

L24410
*        Default / Enable for Purcahse Contract ID
            if vpc_open% <> -1%  then L24430   /* File is OPEN */
                enabled% = 0%  :  return      /* File is Not OPEN */

L24430:     inpmessage$ = "Enter Purchase Contract ID."
            if contract_id$(c%) <> " " then return
            gosub set_vpc_type
            if vpc_potyp$ = "M" then return
            if vpc_potyp$ = "A" then item$ = str(part$(c%,1%),11%,4%)    ~
                                else item$ = part$(c%,1%)
            call "VPCDEFLT" (vencode$,contract_id$(c%),contract_line$(c%)~
                            ,vpc_potyp$, item$, duedate$, #42, f1%(42%))
            return

L24470
*        Default / Enable for SPECIFIC JOB/PROJ
            job% = 8%
            call "READ100" (#23, part$(c%,1%), f1%(23%))
                if f1%(23%) <> 0% then L24540
                     inpmessage$ = "Enter Specific Job or Project for " &~
                                   "this item."
                     return                /* Not stocked or planned */
L24540:         if pj_on% <> 1% then L24670
                if pj_stat%(c%) < 1% then L24670
                if str(job$(c%),3%) <> " " then L24670
                if pj_stat%(c%) < 3% then L24620
                if edit% <> 1% then L24620
                if job$(c%) <> " " then L24620
                     job$(c%) = "PJ"
                     jobdescr$ = "(Future Purchased Job)"
L24620:         job% = 2%
                inpmessage$ = "Enter 'PJ' to Create a 'Purchased Job' " &~
                              "or Blank for a 'Purchase Order'."
                return

L24670:         enabled% = 0%    /* No Job # allowed or */
                return           /* No Messing with it */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4     *~
            * --------------------------------------------------------- *~
            * Defaults and enables for Line Items screen 2. Input/Edit. *~
            *************************************************************

            deffn_054
                call "ENABLSUB" ("SET", "VBKINPUT", scr%(), set%(), 4%,  ~
                                 fieldnr%, abs(edit%), enabled%)
                if edit% = 2% and enabled% = 0% then return
                inpmessage$ = " "
            if part$(c%,1%) = " " then enabled% = 0%
            if part$(c%,1%) = " " then return
                  on fieldnr% goto  L25270,         /* LINE DUE DATE    */~
                                    L25330,         /* Orig Due Date    */~
                                    L25380,         /* NEXT SHIPMENT    */~
                                    L25440,         /* LAST RECIVED DATE*/~
                                    L25490,         /* PACK SLIP        */~
                                    L25570,         /* EXPENSE ACCT     */~
                                    L25660,         /* PC VAR ACCT      */~
                                    L25690,         /* Revision Level   */~
                                    L25720,         /* Deliver To       */~
                                    L25740,         /* Requisitioner    */~
                                    L25760,         /* Line Ref         */~
                                    L25780,         /* Buyer Code       */~
                                    L25800          /* Kit Compelte, etc*/
                     return

L25270
*        Default / Enable for DUE DATE
            inpmessage$ = "Enter Due Date and Receiving Dates Window."
            if edit% <> 1% then return
            if pj_stat%(c%) > 2% and ~
                (duedate$(c%) = " " or duedate$(c%) = blankdate$) then L25300
L25290:         if duedate$(c%) = " " or duedate$(c%) = blankdate$ then duedate$(c%) = duedate$
                return
L25300:     testdate$ = orddate$  :  call "DATUNFMT" (testdate$)
            call "DATE" addr("G+", testdate$, pj_lead%, duedate$(c%),    ~
                             u3%)
                call "DATEFMT" (duedate$(c%))
                goto L25290

L25330
*        Default / Enable for ORIGINAL DUE DATE
            inpmessage$ = "Enter Original Due Date."
            if origdue$(c%) = " " or ~
                origdue$(c%) = blankdate$ then origdue$(c%) = duedate$(c%)
            return

L25380
*        Default / Enable for NEXT SHIPMENT
            inpmessage$ = "Enter Expected Date of Next Shipment."
            if edit%   <> 1% then return
                nextdate$(c%) = duedate$(c%)
                return

L25440
*        Default / Enable for LAST RECIVED DATE
            inpmessage$ = "Enter Date of Last Shipment (blank if none)."
            return


L25490
*        Default / Enable for VENDOR'S PACKING SLIP
            inpmessage$ = "Enter Vendor's Packing Slip Number."
            return

L25570
*        Default / Enable for EXPENSE ACCOUNT
            if edit% = 1% then L25580
                call "READ100" (#8, part$(c%,1%), f1%(8%))
                    if f1%(8%) = 0% then L25580
                        enabled% = 0%
                        if pj_on% <> 1% then return
                        if pj_stat%(c%) < 1% then return
                        if pj_stat%(c%) > 4% then return
                        enabled% = 1%
L25580:     inpmessage$ = "Enter Purchasing Expense Account."
                     if pj_on% <> 1% then L25590
                     if pj_stat%(c%) < 1% then L25590
                     if pj_stat%(c%) > 4% then L25590
                     if job$(c%) <> "PJ"  then L25590
                     inpmessage$ = "Enter Purchase Job Expense Account "&~
                                   "(WIP)."
L25590:     if edit% <> 1% then return
                if pj_on% <> 1% then L25600
                if pj_stat%(c%) < 1% then L25600
                if pj_stat%(c%) > 4% then L25600
                if job$(c%) <> "PJ" then L25600
                     call "HNYGLGET" (part$(c%,1%), str$(c%), lot$(c%),  ~
                                      acct$(c%), 2%, #8, #9)
                          goto L25610
L25600:         acct$(c%) = acct$
L25610:         call "DESCRIBE" (#16, acct$(c%), acctdescr$, 1%, f1%(16))
                call "GLFMT" (acct$(c%))
                if f1%(16) <> 0% then return
                   acct$(c%) = " "
                   enabled%  = 1%
                   return

L25660
*        Default / Enable for P/C VAR ACCOUNT
            inpmessage$ = "Enter Price/Cost Variance Account."
            if pcacct$(c%) <> " " then return
            get #3, using L25666, pcacct$(c%)
L25666:         FMT POS(318), CH(9)
            if pcacct$(c%) <> " " then L25680
            call "READ100" (#2, "DEFAULTS.STORE."& str(str$(c%)), f1%(2))
                if f1%(2) = 0% then L25678
            get #2 using L25676, pcacct$(c%)
L25676:         FMT POS(162), CH(9)
L25678:     if pcacct$(c%) = " " then pcacct$(c%) = sysacct$(6)
L25680:     call "DESCRIBE" (#16, pcacct$(c%), pcacctdescr$, 1%, f1%(16))
            call "GLFMT" (pcacct$(c%))
                if f1%(16) <> 0% then return
                   pcacct$(c%) = " "
                   enabled%  = 1%
                   return

L25690
*        Default / Enable for LINE REVISION LEVEL
            inpmessage$ = "Enter last Revision Level for this line."
            if edit% = 1% then gosub next_revision
            if edit% = 1% then linerev$(c%) = rev_level$
            return

L25720
*        Default / Enable for EXPENSE ACCOUNT
            inpmessage$ = "Enter who item should be delivered to."
            return

L25740
*        Default / Enable for REQUISITIONER
            inpmessage$ = "Enter who requisitioned this item."
            return

L25760
*        Default / Enable for LINE REF NUMBER
            inpmessage$ = "Internal Reference -or- blank."
            return

L25780
*        Default / Enable for BUYER CODE
            inpmessage$ = "Enter Buyer responsible for this Line Item."
            return

L25800
*        Default / Enable for KIT COMPLETE & PRINT OPTIONS
            enabled% = 0%
            if pj_on% <> 1% or pj_stat%(c%) = 0% then return
            if job$(c%) <> "PJ" then return
            enabled% = 1%
            if kit$(c%,1%) = " " then kit$(c%,1%) = kitcomp$
            if kit$(c%,2%) = " " then kit$(c%,2%) = traveler$
            if kit$(c%,3%) = " " then kit$(c%,3%) = picklist$
            if kit$(c%,4%) = " " then kit$(c%,4%) = byprod$
            if kit$(c%,5%) = " " then kit$(c%,5%) = slbom$
            inpmessage$ = "Enter Kit Complete and Report Choices or " &  ~
                          "Accept the Defaults."
            return

        size_run   /* call the sub and then auto-generate line items */
            if duedate$ = " " or duedate$ = blankdate$ then  default_not_set_warning

            max% = maxlines%
            p% = 0%
            limit% = dim(partdescr$(),1)
            call "SZRINSUB" (2%, max%, passpart$(), passqty$(), default$,~
                             #2, #8, #9, #33, #36, #23, #21, #34, #25,   ~
                             #38, #35, limit%)

            if passpart$(1%) = " " then abort_size_run

            for i% = 1% to 72%
               if passpart$(i%) = " " then L26810   /* all done */

               c%   = maxlines% + 1%
               gosub columnone /* clear the variables */

               part$(c%,1%) = passpart$(i%)
               qtyorig$    = passqty$(i%)
               convert qtyorig$ to qtyorig(c%), data goto L26150
               qtyonord(c%) = qtyorig(c%)

               buyerl$(c%) = buyer$
L26150:        duedate$(c%)   = duedate$
               origdue$(c%)  = duedate$(c%)
               nextdate$(c%) = duedate$(c%)
               str$(c%)     = defstr$
               acct$(c%)    = expacct$
               gosub next_revision
               linerev$(c%) = rev_level$
               status$(c%) = "A"
               appnd$(c%) = "I"

               pj_stat%(c%) = 0%           /* No Purchase Jobs Allowed */
               line_source%(c%) = 0%

               call "READ100" (#8, part$(c%,1%), f1%(8))
                 if f1%(8) = 0% then L26790  /* next part */
                  get #8, using L26290, partdescr$(c%), stkg$(c%),        ~
                                       cat$(c%), type$(c%), costtype$(c%)
L26290:              FMT POS(26), CH(32), POS(74), CH(4), POS(90), CH(4),~
                         POS(180), CH(3), POS(307), CH(1)
               gosub std_cost_warning
               edit% = 1%
               gosub  L23570          /* vendor part number and price */
               gosub  L23770          /* unit of issue    */
               gosub  L23920          /* original order   */
               gosub  L24300          /* cost             */
               if price(c%) = 0  then gosub price_warning

               gosub  L23770          /* unit of issue    */

               call "HNYGLGET" (part$(c%,1%), str$(c%), lot$(c%),        ~
                                            acct$(c%), 3%, #8, #9)

               call "GLFMT" (acct$(c%))
                  get #3, using L26460, pcacct$(c%)
L26460:              FMT POS(318), CH(9)
               if pcacct$(c%) <> " " then L26570

               call "READ100" (#2, "DEFAULTS.STORE."& str(str$(c%)),     ~
                                    f1%(2))
                  if f1%(2) = 0% then L26560

               get #2 using L26540, pcacct$(c%)
L26540:              FMT POS(162), CH(9)

L26560:        if pcacct$(c%) = " " then pcacct$(c%) = sysacct$(6)
L26570:        call "GLFMT" (pcacct$(c%))

               if pcacct$(c%) = " " or acct$(c%) = " " then              ~
                                 goto gl_not_set_warning
               hnycost(c%) = price(c%)
               if pos("FST" = costtype$(c%)) = 0 then L26680
               if postatstdcost$ <> "Y" then L26680
                  ro% = 1%
                  gosub call_stccosts
                  ro% = 0%

               if cur_tran$ = " " then cur_tran$ = cur_vend$
L26680:        if cur_tran$ = " " then cur_tran$ = cur_stat$
*             IF CUR_LOADED% <> 0% THEN                                 ~
*                          PRICE(C%) = ROUND(PRICE(C%) * CUR_FACTOR2, 7)
               gosub deffn_041  /* Converts FACTOR to FACTOR$  */
               gosub deffn_040  /* Converts floating points to strings */

               maxlines% = maxlines% + 1%  /* Officially welcome line  */
               lastseq%  = lastseq%  + 1%  /* to the Order             */
               convert lastseq% to seqnr$(c%), pic(###)
               if maxlines% = 100% then L26810
               if lastseq% >= 999% then L26810
L26790:        next i%

L26810:     if maxlines% = 0% then abort_size_run else line_summary

        abort_size_run
            goto L10420    /* go back to top of input screen */

        price_warning
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
              "The Default Due Date is not set",                         ~
              "Set it and then try this function again.",                ~
              "Press any key to acknowledge.")
            goto abort_size_run

        gl_not_set_warning
            call "ASKUSER" (k%, "* * * ABORT * * *",                     ~
            "A default cannot be found for one or both G/L Accounts",    ~
              "Set them and then try this function again.",              ~
              "Press any key to acknowledge.")
            goto abort_size_run

*       *****************************************************************
*        INITIALIZE 'SOFT' ENABLE SWITCHES.                             *
*       *****************************************************************
        init_enables

*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
*        First Page of Header Screen
            scr%(1%, 1%) =  1% : set%( 1%) = 13%   /* Vendor Code      */
            scr%(1%, 2%) =  2% : set%( 2%) =  2%   /* Buy From Code    */
            scr%(1%, 3%) =  3% : set%( 3%) = 13%   /* P.O. Number      */
            scr%(1%, 4%) =  4% : set%( 4%) =  2%   /* Name and Address */
            scr%(1%, 5%) =  5% : set%( 5%) =  2%   /* Vendor Type Code */
            scr%(1%, 6%) =  6% : set%( 6%) =  2%   /* Contact's Name   */
            scr%(1%, 7%) =  7% : set%( 7%) =  2%   /* Default Store    */
            scr%(1%, 8%) =  8% : set%( 8%) =  1%   /* Revision Level   */
            scr%(1%, 9%) =  9% : set%( 9%) =  1%   /* Revision Date    */
            scr%(1%,10%) = 10% : set%(10%) =  2%   /* Buyer Code       */

            scr%(2%, 1%) = 16% : set%(16%) =  2%   /* Liability Acct   */
            scr%(2%, 2%) = 17% : set%(17%) =  2%   /* Taxable          */
            scr%(2%, 3%) = 18% : set%(18%) =  2%   /* Frt Terms        */
            scr%(2%, 4%) = 19% : set%(19%) =  2%   /* FOB              */
            scr%(2%, 5%) = 20% : set%(20%) =  2%   /* Ship Via         */
            scr%(2%, 6%) = 21% : set%(21%) =  2%   /* Order Date       */
            scr%(2%, 7%) = 22% : set%(22%) =  2%   /* Dflt Due Date    */
            scr%(2%, 8%) = 23% : set%(23%) =  2%   /* Cancellation Dte */
            scr%(2%, 9%) = 24% : set%(24%) =  2%   /* Confirmed?       */
            scr%(2%,10%) = 25% : set%(25%) =  2%   /* Max Dollar Amt   */
            scr%(2%,11%) = 26% : set%(26%) =  2%   /* Max change Date  */
            scr%(2%,12%) = 27% : set%(27%) =  2%   /* Copy Text ID     */

            scr%(3%, 1%) = 31% : set%(31%) = 13%   /* Part Number      */
            scr%(3%, 2%) = 32% : set%(32%) =  2%   /* Part Description */
            scr%(3%, 3%) = 33% : set%(33%) =  2%   /* Store Number     */
            scr%(3%, 4%) = 34% : set%(34%) =  2%   /* Lot Number       */
            scr%(3%, 5%) = 35% : set%(35%) =  2%   /* Category         */
            scr%(3%, 6%) = 36% : set%(36%) =  2%   /* Vendor Part #    */
            scr%(3%, 7%) = 37% : set%(37%) =  2%   /* Vendor U-O-Ship  */
            scr%(3%, 8%) = 61% : set%(61%) =  2%   /* Contract ID      */
            scr%(3%, 9%) = 50% : set%(50%) =  2%   /* Job/Proj Code    */
            scr%(3%,10%) = 38% : set%(38%) =  2%   /* Trans Currency   */
            scr%(3%,11%) = 39% : set%(39%) =  2%   /* Orig On-Order    */
            scr%(3%,12%) = 40% : set%(40%) =  1%   /* Now On-Order     */
            scr%(3%,13%) = 41% : set%(41%) =  1%   /* Rcvd to date     */
            scr%(3%,14%) = 42% : set%(42%) =  2%   /* Price            */
            scr%(3%,15%) = 43% : set%(43%) =  2%   /* Extension        */
            scr%(3%,16%) = 60% : set%(60%) =  1%   /* Inventory Cost   */
            scr%(3%,17%) = 44% : set%(44%) =  2%   /* Line Status      */

            scr%(4%, 1%) = 45% : set%(45%) =  2%   /* Due Date         */
            scr%(4%, 2%) = 46% : set%(46%) =  1%   /* Orig Due Date    */
            scr%(4%, 3%) = 47% : set%(47%) =  2%   /* Next Ship Date   */
            scr%(4%, 4%) = 48% : set%(48%) =  1%   /* Last Rcvd Date   */
            scr%(4%, 5%) = 49% : set%(49%) =  2%   /* Packing Slip     */
            scr%(4%, 6%) = 51% : set%(51%) =  2%   /* Expense Account  */
            scr%(4%, 7%) = 57% : set%(57%) =  2%   /* PC Var. Account  */
            scr%(4%, 8%) = 52% : set%(52%) =  2%   /* Revision Level   */
            scr%(4%, 9%) = 53% : set%(53%) =  2%   /* Deliver To       */
            scr%(4%,10%) = 54% : set%(54%) =  2%   /* Requisitioner    */
            scr%(4%,11%) = 55% : set%(55%) =  2%   /* Line Ref #       */
            scr%(4%,12%) = 56% : set%(56%) =  0%   /* Buyer Code       */
            scr%(4%,13%) = 58% : set%(58%) =  2%   /* Kit Complete, etc*/
            scr%(4%,14%) = 59% : set%(59%) = 99%   /*                  */

        REM *** Last used SET% = 61%  ***

            call "ENABLSUB" ("INIT", "VBKINPUT", scr%(), set%(),         ~
                             0%, 0%, 0%, 0%)
            return

L29000: REM *************************************************************~
            * INITIALIZATION BLOCK (NEATER THAN CRAMMING AT 10000)      *~
            *************************************************************

            init(hex(ff)) textid$, textidl$(), copytextid$()
            call "TXTFUTIL" (#20, f2%(20), "INTL", textid$)

            errormsg$, expacct$, confirm$, defjob$, buyer$, notafter$(), ~
            candate$, orddate$, ponumber$, puracct$, defjobdescr$, fob$, ~
            defstr$, defstrdescr$, duedate$, frgtflag$, frgtflagdescr$,  ~
            header$(), lastseq$, acct$(), cat$(), duedate$(), vendescr$, ~
            puracctdescr$, rest$, shipvia$, taxflag$, str$(), vencode$,  ~
            varfield$(), vencont$, ext$(), extg$(), ventype$, strdescr$, ~
            strexpacct$, strpuracct$, acctdescr$, catdescr$, shpaddr$(), ~
            jobdescr$, factor$, qtyorig$, qtyonord$, qtyrecd$, seqnr$(), ~
            ourprice$, venorig$, venonord$, venrecd$, bfcode$, venprice$,~
            copytextname$(),copytextdescr$(),type$(),fillerl$(), part$(),~
            meas$(), packslip$(), nextdate$(), statusdescr$, origdue$(), ~
            sumonord$(), partdescr$(), pobufkey$(), bfdescr$, pcacct$(), ~
            recdate$(), rev_date$, venaddr$(), rev_level$, linerev$(),   ~
            notbefore$(), maxdlrs$, maxdate$, origrev$, appnd$(), lot$(),~
            dlvrto$(), lineref$(), buyerl$(), dspaddr$(), status$(),     ~
            rqstnr$(), ohpost$(), buyshp$, job$(),                       ~
            cur_eff_date$, cur_tran$, pcacct$, phone$, ventypedesc$,     ~
            costtype$(), stkg_descr$, bom_id$(), piptag$(), kit$(),      ~
            contract_id$(),contract_line$(), vpc_used$()           = " "

            mindate$ = blankdate$
            lstdate$ = blankdate$

            init (hex(00)) hnycostd$(), rcvqtys$()

            mat factor       = zer       /* CASE MULTIPLIER            */
            mat price        = zer       /* PURCHASE PRICE             */
            mat qtyonord     = zer       /* REMAINING ON ORDER         */
            mat qtyorig      = zer       /* ORIGINAL ORDER             */
            mat qtyrecd      = zer       /* RECIEVED LAST SHIPMENT     */
            mat hnycost      = zer
            mat pj_stat%     = zer
            mat line_source% = zer
            mat vpc_use_idx% = zer
            mat ext_loaded   = zer
            mat qty_loaded   = zer
            change_date% = 0%
            cur_loaded%, buyshp%, poflagged% = 0%
            cur_factor1, cur_factor2 = 1
            call "ALLFREE"
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

        startover2
                return clear all
                init(hex(00)) plowkey$
                str(plowkey$,,3) = userid$
                call "READ101" (#6, plowkey$, f1%(6))
                if f1%(6) = 1% then delete #6
                init(hex(00)) plowkey$
                call "DELETE" (#50, plowkey$, 0%)
                poflagged% = 0%
                goto inputmode


L30000: REM *************************************************************~
            *       L O A D   O L D   P U R C H A S E   O R D E R       *~
            * --------------------------------------------------------- *~
            * LOADS OLD PURCHASE ORDER FROM THE BUFFER IF ONE IS THERE. *~
            * WILL ALSO LOAD THE THING FROM THE MAIN FILE.              *~
            *************************************************************

            buyshp%, oldpoonfile% = 0%
            if ponumber$ = " " then return

*        See if PO is in BUFFER (if so can't touch)
            call "REDALT0" (#6, ponumber$, 3%, f1%(6))
            if f1%(6) = 0 then L30064
                errormsg$ = "Purchase Order is being updated."
                return

L30064
*        Not in buffer-- try next in MAIN file.
            call "REDALT0" (#4, ponumber$, 1%, f1%(4))
            if f1%(4) = 0 then return    /* Not no where, man          */

*        Test data in header (is this ok to mess with??)
            get #4 using L30088, tempven$
L30088:         FMT CH(9), POS(486), CH(3)
            if vencode$ = " "      then vencode$ = tempven$
            if vencode$ = tempven$ then L30112
                errormsg$ = "PO: " & ponumber$ & " Assigned to Vendor: " ~
                                                              & tempven$
                return
L30112:
            get #4 using L30120, maxdate$
L30120:         FMT XX(583), CH(6)
            if maxdate$ = " "  or maxdate$ = blankdate$ then L30132
            if maxdate$ < date then gosub date_notification
L30132:         errormsg$ = " "

            call "READ100" (#3, vencode$, f1%(3))
            if f1%(3) <> 0 then L30156
                errormsg$ = "Vendor Not On File: " & vencode$
                return
L30156:     gosub get_vendor_stuff
            oldpoonfile% = 1%

*        Load HEADER information, describe accounts, format data.
            get #4 using L30556,                                          ~
                    vencode$, ponumber$, venaddr$(), vencont$,           ~
                    puracct$, str(varfield$(), 1), orddate$, candate$,   ~
                    defjob$, duedate$, defstr$, buyer$, confirm$,        ~
                    frgtflag$, fob$, taxflag$, shipvia$, ventype$,       ~
                    bfcode$, textid$, rev_level$, rev_date$,             ~
                    maxdlrs$, maxdate$, lastseq$, copytextname$(1%),     ~
                    shpaddr$(), pcacct$, copytextname$(2%),              ~
                    copytextname$(3%), rest$
            origrev$ = rev_level$
            call "TXTFUTIL" (#20, f2%(20), "LOAD", textid$)
            gosub describe_copy_name

            call "DATEFMT"  (orddate$)
            if duedate$   = blankdate$ then duedate$  = " "
            if candate$   = blankdate$ then candate$  = " "
            if rev_date$  = blankdate$ then rev_date$ = " "
            if maxdate$   = blankdate$ then maxdate$  = " "

            if duedate$  <> " " then call "DATEFMT" (duedate$)
            if candate$  <> " " then call "DATEFMT" (candate$)
            if rev_date$ <> " " then call "DATEFMT" (rev_date$)
            if maxdate$  <> " " then call "DATEFMT" (maxdate$)
            call "DESCRIBE" (#16, puracct$, puracctdescr$, 1%, f1%(16))
                call "GLFMT" (puracct$)
            gosub describe_vendor_type
            call "DESCRIBE" (#19, defstr$ , defstrdescr$ , 1%, f1%(19))
            readkey$ = str(vencode$) & bfcode$
                call "READ100" (#14, readkey$, f1%(14))
                if f1%(14) = 1% then get #14 using L30276, bfdescr$       ~
                                else bfcode$ = " "
L30276:              FMT XX(15), CH(30)
            if defjob$ = " " then L30300
                call "DESCRIBE" (#18, defjob$, defjobdescr$, 1%, f1%(18))
                if f1%(18) <> 0 then L30300
                call "DESCRIBE" (#17, defjob$, defjobdescr$, 1%, f1%(17))

L30300
*        Read LINE ITEMS from VBKLINE
            c%, maxlines%, vpcs%  = 0%
            cur_tran$ = cur_stat$
            oldreadkey$   = str(vencode$,,9) & ponumber$
            call "PLOWNEXT" (#30, oldreadkey$, 25%, f1%(30))
               init (" ") str(oldreadkey$,26)
               if f1%(30) = 0% then L30348
            get #30 using L30336, cur_tran$, cur_factor1, cur_factor2,    ~
                                 cur_eff_date$
L30336:         FMT CH(4), POS(49), 2*PD(14,7), CH(6)
            cur_loaded% = 1%

L30348:     call "PLOWNEXT" (#5, oldreadkey$, 25%, f1%(5))
            if f1%(5) = 0% then L30524
                c%, maxlines% = c% + 1%
                get #5, using L30684,                                     ~
                    seqnr$(c%), part$(c%,1%), partdescr$(c%), cat$(c%),  ~
                    qtyorig(c%), qtyrecd(c%), qtyonord(c%), price(c%),   ~
                    ext, acct$(c%), duedate$(c%), recdate$(c%),          ~
                    nextdate$(c%), lot$(c%), job$(c%),                   ~
                    str$(c%), ohpost$(c%), type$(c%), packslip$(c%),     ~
                    part$(c%,2%), pobufkey$(c%), meas$(c%),              ~
                    factor(c%), origdue$(c%), textidl$(c%), status$(c%), ~
                    linerev$(c%), dlvrto$(c%), rqstnr$(c%),              ~
                    lineref$(c%),  notbefore$(c%), notafter$(c%),        ~
                    buyerl$(c%), rcvqtys$(c%), hnycost(c%),              ~
                    hnycostd$(c%), pcacct$(c%), contract_id$(c%),        ~
                    contract_line$(c%), fillerl$(c%)

            call "TXTFUTIL" (#20, f2%(20), "LOAD", textidl$(c%))
            if part$(c%,1%) = " " then L30348   /* Description Line */
                call "DATEFMT" (duedate$  (c%))
                call "DATEFMT" (recdate$  (c%))
                call "DATEFMT" (origdue$  (c%))
                call "DATEFMT" (nextdate$ (c%))
                call "DATEFMT" (notbefore$(c%))
                call "DATEFMT" (notafter$ (c%))
                call "GLFMT"   (acct$(c%))
                call "GLFMT"   (pcacct$(c%))
                line_source%(c%) = 2%
                qty_loaded(c%) = qtyorig(c%)
                ext_loaded(c%) = ext

                gosub purchase_job_check
                if contract_id$(c%) =  " " then L30454
                gosub  set_vpc_index :  gosub  set_vpc_type
                gosub set_vbk_pay_sums
L30454:         call "READ100" (#8, part$(c%,1%), f1%(8))
                if f1%(8) = 1% then get #8 using L30464, stkg$(c%),       ~
                               costtype$(c%)
L30464:              FMT POS(74), CH(4), POS(307), CH(1)
                if factor(c%) <= 0 or factor(c%) > 1000000               ~
                                                      then factor(c%) = 1
                if cur_loaded% = 0% then L30508
                   call "READ100" (#30, key(#5), f1%(30))
                      if f1%(30) = 0 then L30500
                   get #30 using L30492, price(c%)
L30492:                FMT POS(33), PD(14,7)
                   goto L30508
L30500:         price(c%) = round(price(c%) * cur_factor2, 7)

L30508:         gosub deffn_041      /* Format Numerics  */
                gosub deffn_040      /* Format Numerics  */
                goto L30348

L30524:      temp%, lastseq% = 0%
             if maxlines% > 0% then convert seqnr$(maxlines%) to temp%,  ~
                               data goto L30536
L30536:      convert lastseq$ to lastseq%, data goto L30540
L30540:      lastseq% = max(lastseq%, temp%)
             edit% = 2%
             goto L32000        /* Append Buy Orders */


L30556:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                6*CH(30),                /* Buy From Name, Address     */~
                CH(20),                  /* CONTACT'S NAME             */~
                CH(9),                   /* PAYABLES ACCOUNT DEFAULT   */~
                XX(16),                  /* LAST INVOICE NUMBER        */~
                CH(200),                 /* VARIABLE FIELDS (10 * 20)  */~
                CH(6),                   /* ORDER DATE                 */~
                CH(6),                   /* CANCELATION DATE           */~
                CH(8),                   /* DEFAULT JOB NUMBER         */~
                CH(6),                   /* DEFAULT DUE DATE           */~
                CH(3),                   /* DEFAULT STORE NUMBER       */~
                XX(6),                   /* REC-DATE                   */~
                CH(3),                   /* BUYER CODE                 */~
                CH(01),                  /* CONFIRMTION                */~
                CH(01),                  /* FRIEGHT TERMS              */~
                CH(30),                  /* FOB                        */~
                CH(01),                  /* TAXABLE                    */~
                CH(30),                  /* SHIP VIA                   */~
                CH(04),                  /* VENDOR TYPE                */~
                CH(06),                  /* Buy From ID                */~
                CH(04),                  /* Text ID                    */~
                CH(02),                  /* Revision Level             */~
                CH(06),                  /* Revision Date              */~
                CH(10),                  /* Maximum Dollar Amount      */~
                CH(06),                  /* Maximum Date               */~
                CH(03),                  /* LAST SEQUENCE USED         */~
                XX(34),                  /* Skip- Master only stuff    */~
                CH(10),                  /* Copy Element Name (1)      */~
              6*CH(30),                  /* Ship-To Address            */~
                XX(01),                  /* Print Flag                 */~
                CH(09),                  /* Price-Cost Variance Acct   */~
              2*CH(10),                  /* Copy Element Name (2 & 3)  */~
                CH(184)                  /* REST OF RECORD             */

L30684:     FMT XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                XX(3),                   /* ITEM NUMBER                */~
                CH(25),                  /* PART NUMBER                */~
                CH(32),                  /* DESCRIPTION                */~
                CH(4),                   /* CATEGORY  CDE              */~
                PD(14,4),                /* QUANTITY ORIGINALLY ORDERED*/~
                PD(14,4),                /* QUANTITY RECEIVED          */~
                PD(14,4),                /* QUANTITY ON ORDER          */~
                PD(14,7),                /* UNIT PRICE                 */~
                PD(14,4),                /* EXTENSION                  */~
                CH(9),                   /* PURCHASES ACCOUNT NUMBER   */~
                CH(6),                   /* DATE DUE INFORMATION       */~
                CH(6),                   /* DATE OF LAST RECEIPT       */~
                CH(6),                   /* DATE OF NEXT RECEIPT       */~
                CH(6),                   /* LOT NUMBER                 */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(3),                   /* STORE NUMBER               */~
                CH(1),                   /* USED FOR QC FLAG           */~
                CH(3),                   /* ITEM TYPE                  */~
                CH(16),                  /* PACKING SLIP #             */~
                CH(25),                  /* VENDOR PART                */~
                CH(66),                  /* POBUFFR LINK               */~
                CH(4),                   /* UNIT OF ISSUE              */~
                PD(14,4),                /* QUANTITY PER UNIT          */~
                CH(6),                   /* Original Due Date          */~
                CH(4),                   /* Text ID                    */~
                CH(1),                   /* Line Status                */~
                CH(2),                   /* Revision Level             */~
                CH(20),                  /* Deliver To                 */~
                CH(20),                  /* Requisitioner              */~
                CH(05),                  /* Line Ref (User Internal)   */~
              2*CH(6),                   /* Don't receive before/after */~
                CH(3),                   /* Buyer Code                 */~
                CH(48),                  /* Disposition Qtys & Filler  */~
                PD(14,4),                /* Inventory Cost             */~
                CH(96),                  /* Inventory Cost Detail      */~
                CH(9),                   /* Price/Cost Var. Acct       */~
                POS(561),                /* Purchase Job Filler        */~
                CH(16),                  /* Contract Id                */~
                CH(04),                  /* Contract Line Number       */~
                CH(120)                  /* Filler                     */


        set_vpc_index
            if vpcs% = 0% then L30895
            for i% = 1% to vpcs%
                if vpc_used$(i%) <> contract_id$(c%) & contract_line$(c%)~
                                                              then  L30890
                    j% = i% :  goto L30905
L30890:     next i%
L30895:     vpcs%, j% = vpcs% + 1%    /* New Contract */
            goto L30925
L30905:     /* Test for Duplication */
            if vpc_use_idx%(j%, 101%) = 0%  then goto L30925
            for i% = 1% to vpc_use_idx%(j%, 101%)
                 if c% = vpc_use_idx%(j%, i%) then goto L30945
            next i%
            /* Set POline to Contract Index */
L30925:     vpc_used$(j%) = contract_id$(c%) & contract_line$(c%)
            vpc_use_idx%(j%, 101%) = vpc_use_idx%(j%, 101%) + 1%
            vpc_use_idx%(j%, vpc_use_idx%(j%, 101%)) = c%
L30945:     return

        set_vpc_type
            vpc_potyp$ = " "
            if str(part$(c%,1%),,9%) <> "ACTIVITY:" then L30980
                vpc_potyp$ = "A"
                return
L30980:     call "READ100" (#8, part$(c%,1%), f1%(8%))
            if f1%(8%)  = 0% then vpc_potyp$ = "M"                       ~
                             else vpc_potyp$ = "P"
            return

L31000: REM *************************************************************~
            *         W R I T E   O R D E R   T O   B U F F E R         *~
            * --------------------------------------------------------- *~
            * Write Order to Buffer Files.                              *~
            *************************************************************

*        Convert all the variables to Disk format.
            call "DATUNFMT" (orddate$)
            call "DATUNFMT" (candate$)
            call "DATUNFMT" (duedate$)
            call "DATUNFMT" (maxdate$)

            orderamt, onorderamt = 0

            plowkey$ = str(vencode$,,9) & ponumber$
            call "DELETE" (#30, plowkey$, 25%)

            if pcacct$ <> " " then L31112
            get #3, using L31080, pcacct$
L31080:         FMT POS(318), CH(9)
            if pcacct$ <> " " then L31112
            call "READ100" (#2, "DEFAULTS.STORE."& str(defstr$), f1%)
                if f1% = 0% then L31104
            get #2 using L31100, pcacct$
L31100:         FMT POS(162), CH(9)
L31104:     if pcacct$ = " " then pcacct$ = sysacct$(6)

L31112
*        Herd up the LINES and move 'em out.
            if maxlines% = 0% then L31428           /* No line items?!? */
            for c% = 1% to maxlines%
              if part$(c%,1%)  = " " then L31256    /* Descriptive Line */
              if appnd$(c%)    = "D" then L31256    /* Deleted Line     */
                convert c% to item$, pic(###)

                extg = round(qtyorig (c%) * price(c%), 2)
                ext  = round(qtyonord(c%) * price(c%), 2)

                call "DATUNFMT" (duedate$  (c%))
                call "DATUNFMT" (recdate$  (c%))
                call "DATUNFMT" (origdue$  (c%))
                call "DATUNFMT" (nextdate$ (c%))
                call "DATUNFMT" (notbefore$(c%))
                call "DATUNFMT" (notafter$ (c%))
                call "GLUNFMT"  (acct$(c%))
                call "GLUNFMT"  (pcacct$(c%))
                if cur_loaded% = 0% then L31236

                write #30 using L31208, cur_tran$, vencode$, ponumber$,   ~
                                       seqnr$(c%), price(c%), ext,       ~
                                       cur_factor1, cur_factor2,         ~
                                       cur_eff_date$, " "
L31208:         FMT CH(4), CH(9), CH(16), CH(3), PD(14,7), PD(14,4),     ~
                    2*PD(14,7), CH(6), CH(30)

                extg = round(extg * cur_factor1, 2)
                ext  = round(ext  * cur_factor1, 2)
                price(c%) = round(price(c%) * cur_factor1, 7)

L31236:         orderamt   = round(orderamt   + extg, 2)
                onorderamt = round(onorderamt + ext , 2)
                if pj_on% = 1% then gosub save_purchase_job_data

                goto L31316

L31256:       /* Initialization for Description Only & Deleted lines   */
                if pj_on% = 1% then gosub delete_work_file
                init(" ") item$, cat$(c%), acct$(c%), part$(c%,1%),      ~
                          duedate$(c%), recdate$(c%), nextdate$(c%),     ~
                          lot$(c%), job$(c%), str$(c%), ohpost$(c%),     ~
                          type$(c%), packslip$(c%), part$(c%,2%),        ~
                          str(pobufkey$(c%),,47), dlvrto$(c%),           ~
                          meas$(c%), fillerl$(c%), rqstnr$(c%),          ~
                          lineref$(c%), linerev$(c%), status$(c%),       ~
                          origdue$(c%), buyerl$(c%), notbefore$(c%),     ~
                          notafter$(c%), pcacct$(c%), contract_id$(c%),  ~
                          contract_line$(c%)
                if appnd$(c%) <> "D" then L31300
                     partdescr$(c%) = hex(ff)
                     gosub next_revision
L31300:         init (hex(00)) hnycostd$(c%), rcvqtys$(c%)
                qtyorig(c%), qtyrecd(c%), qtyonord(c%), price(c%),       ~
                factor(c%) , ext, hnycost(c%) = 0

L31316:         write #7, using L31688,                                   ~
                     vencode$, ponumber$, seqnr$(c%), item$,             ~
                     part$(c%,1%), partdescr$(c%), cat$(c%),             ~
                     qtyorig(c%), qtyrecd(c%), qtyonord(c%),             ~
                     price(c%), ext, acct$(c%), duedate$(c%),            ~
                     recdate$(c%), nextdate$(c%), lot$(c%), job$(c%),    ~
                     str$(c%), ohpost$(c%), type$(c%), packslip$(c%),    ~
                     part$(c%,2%), str(pobufkey$(c%),,47), "PO",         ~
                     str(ponumber$,,14), seqnr$(c%), meas$(c%),          ~
                     factor(c%), origdue$(c%), textidl$(c%),             ~
                     status$(c%), linerev$(c%), dlvrto$(c%),             ~
                     rqstnr$(c%), lineref$(c%), notbefore$(c%),          ~
                     notafter$(c%), buyerl$(c%), rcvqtys$(c%),           ~
                     hnycost(c%), hnycostd$(c%), pcacct$(c%),            ~
                     piptag$(c%), kit$(c%,1%), kit$(c%,2%), kit$(c%,3%), ~
                     kit$(c%,4%), kit$(c%,5%), bom_id$(c%),              ~
                     contract_id$(c%), contract_line$(c%), fillerl$(c%)

                if pobufkey$(c%) = " " then L31420
                     call "READ101" (#22, pobufkey$(c%), f1%)
                     if f1% = 0 then L31420
                         get #22, using L31416, str(header$(),,492)
                         delete #22
                         str(header$(),48, 2) = "PO"
                         str(header$(),50,14) = str(ponumber$,,14)
                         str(header$(),64, 3) = str(seqnr$(c%),,3)
                         write #22, using L31416, str(header$(),,492)
L31416:                        FMT CH(492)
L31420:         next c%

L31428
*        Now, Write the HEADER to the file.
            call "DATUNFMT" (rev_date$)
            orderamt = round(orderamt,2)
            if maxlines% = 0% then L31452
               temp% = 0%
               convert seqnr$(maxlines%) to temp%, data goto L31448
L31448:        lastseq% = max(temp%, lastseq%)
L31452:     convert lastseq% to lastseq$, pic(###)
            call "GLUNFMT" (puracct$)
            plowkey$ = all(hex(00)) : str(plowkey$,,3) = userid$
            call "DELETE" (#6, plowkey$, 10%) : poflagged% = 0%

L31480:     call "GETDTTM" addr(datetime$)
            write #6, using L31536, userid$, datetime$, poprnt$, " ",     ~
                     vencode$, ponumber$, venaddr$(),                    ~
                     vencont$, puracct$, " ", str(varfield$()),          ~
                     orddate$, candate$, defjob$, duedate$, defstr$,     ~
                     " ", buyer$, confirm$, frgtflag$, fob$,             ~
                     taxflag$, shipvia$, ventype$, bfcode$, textid$,     ~
                     rev_level$, rev_date$, maxdlrs$, maxdate$,          ~
                     lastseq$, " ", orderamt, onorderamt,                ~
                     copytextname$(1%), shpaddr$(), poprnt$, pcacct$,    ~
                     copytextname$(2%), copytextname$(3%),               ~
                     rest$, eod goto L31480
            call "TXTFUTIL" (#20, f201%, "SAV2", textid$)
            err% = 0%
            call "TASKUP" ("PO", err%)
            return

L31536:     FMT CH(3),                   /* KEY- User ID               */~
                CH(7),                   /* DATE/TIME STAMP            */~
                CH(1),                   /* PO Print Instructions      */~
                CH(3),                   /* Filler                     */~
                CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                6*CH(30),                /* Name and Address           */~
                CH(20),                  /* CONTACT'S NAME             */~
                CH(9),                   /* PAYABLES ACCOUNT DEFAULT   */~
                CH(16),                  /* LAST INVOICE NUMBER        */~
                CH(200),                 /* VARIABLE FIELDS            */~
                CH(6),                   /* ORDER DATE                 */~
                CH(6),                   /* CANCELATION DATE           */~
                CH(8),                   /* DEFAULT JOB NUMBER         */~
                CH(6),                   /* DEFAULT DUE DATE           */~
                CH(3),                   /* DEFAULT STORE NUMBER       */~
                CH(6),                   /* FILLER (Used in MASTER)    */~
                CH(3),                   /* BUYER CODE                 */~
                CH(01),                  /* CONFIRMTION                */~
                CH(01),                  /* FRIEGHT TERMS              */~
                CH(30),                  /* FOB                        */~
                CH(01),                  /* TAXABLE                    */~
                CH(30),                  /* SHIP VIA                   */~
                CH(04),                  /* VENDOR TYPE                */~
                CH(06),                  /* Buy From ID                */~
                CH(04),                  /* Text ID                    */~
                CH(02),                  /* Revision Level             */~
                CH(06),                  /* Date of Revision           */~
                CH(10),                  /* Maximum Dollar Amount      */~
                CH(06),                  /* Maximum Date Limit         */~
                CH(03),                  /* LAST SEQNR                 */~
                CH(18),                  /* Hold for MASTER            */~
                PD(14,4),                /* TOTAL ORDER AMOUNT (ORIG)  */~
                PD(14,4),                /* CURRENT ORDER AMOUNT       */~
                CH(10),                  /* Boiler Plate Text Name - 1 */~
              6*CH(30),                  /* Ship-To Address            */~
                CH(01),                  /* Print Flag                 */~
                CH(09),                  /* Price Cost Variance Acct   */~
              2*CH(10),                  /* Boiler Plate Text Name- 2&3*/~
                CH(184)                  /* Filler                     */

L31688:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(3),                   /* ITEM NUMBER                */~
                CH(25),                  /* PART NUMBER                */~
                CH(32),                  /* DESCRIPTION                */~
                CH(4),                   /* CATEGORY CODE              */~
                PD(14,4),                /* QUANTITY ORIGINALLY ORDERED*/~
                PD(14,4),                /* QUANTITY RECEIVED          */~
                PD(14,4),                /* QUANTITY ON ORDER          */~
                PD(14,7),                /* UNIT PRICE                 */~
                PD(14,4),                /* EXTENSION                  */~
                CH(9),                   /* PAYABLES ACCOUNT NUMBER    */~
                CH(6),                   /* DATE DUE INFORMATION       */~
                CH(6),                   /* DATE OF LAST RECEIPT       */~
                CH(6),                   /* DATE OF NEXT RECEIPT       */~
                CH(6),                   /* LOT NUMBER                 */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(3),                   /* STORE NUMBER               */~
                CH(1),                   /* RESERVED FOR VBKRELIEF     */~
                CH(3),                   /* PART TYPE                  */~
                CH(16),                  /* PACKING SLIP #             */~
                CH(25),                  /* VENDOR PART                */~
                CH(47),                  /* POBUFFR LINK               */~
                CH(2),                   /* DITTO                      */~
                CH(14),                  /* DITTO                      */~
                CH(3),                   /* DITTO                      */~
                CH(4),                   /* UNIT OF ISSUE              */~
                PD(14,4),                /* QUANTITY PER UNIT          */~
                CH(06),                  /* Original Due Date          */~
                CH(04),                  /* Text ID                    */~
                CH(01),                  /* Line Item Status           */~
                CH(02),                  /* Revision Level             */~
                CH(20),                  /* Deliver To                 */~
                CH(20),                  /* Requisitioner              */~
                CH(05),                  /* User's Ref #               */~
              2*CH(06),                  /* Don't receive Before/After */~
                CH(03),                  /* Buyer Code                 */~
                CH(48),                  /* Disposition Qtys & Filler  */~
                PD(14,4),                /* Inventory Cost             */~
                CH(96),                  /* Inventory Cost Detail      */~
                CH(9),                   /* Price/cost var. acct       */~
                CH(19),                  /* PIPTAG for PIPPOUTs        */~
                CH(01),                  /* Kit Complete, Purch Job    */~
                CH(01),                  /* Print Traveler, Purch Job  */~
                CH(01),                  /* Print Pick List, Purch Job */~
                CH(01),                  /* Print By-Prod Rpt, Pur Job */~
                CH(01),                  /* Print Sin Levl BOM, Pur Job*/~
                CH(03),                  /* BOM ID for Purchase Jobs   */~
                CH(16),                  /* Contract Id                */~
                CH(04),                  /* Contract Line Number       */~
                CH(120)                  /* Filler                     */

L32000: REM *************************************************************~
            *                                                           *~
            *  LOAD LINES FROM PURCHASE DIRECTIVES FILE                 *~
            *                                                           *~
            *************************************************************

            if defbuyer$ = " " then return
            gosub determine_acct
            load_told% = 0
            temp1$ = deforddate$
            rev_order_seed$ = paydate$
            call "DATEOK" (paydate$, err%, errormsg$)  /* ORDDATE$ will */
            if errormsg$ = " " then L32096           /* become PAYDATE$. */
                rev_order_seed$ = date

L32096:     call "DATREVRS" (rev_order_seed$, rev_order_date$, errormsg$)
            mindate$ = unfmaxdt$
            lstdate$ = unfmaxdt$
            oldreadkey$ = "R" & str(defbuyer$) & str(vencode$) & hex(00)
L32112:     if maxlines%  = 100% then return
            if lastseq%  >= 999% then return
            call "PLOWNEXT" (#22, oldreadkey$, 13%, f1%(22))
            if f1%(22) = 0% then return
            if str(oldreadkey$,39, 6)  > temp1$ then L32112
            if str(oldreadkey$,48,19) <> " "    then L32112
            if str(oldreadkey$,1%,1%) <> "R"    then L32112
              if cur_stat$ = " " then L32190   /* MC not active */
                get #22, using L32148, cur_dir$
L32148:              FMT POS(238), CH(4)
                if maxlines% = 0% then L32162
                     if doing_blanks% = 1% and cur_dir$ = " " then L32190
                          if cur_tran$ <> cur_dir$ then L32112
                               goto L32190
L32162:         if cur_dir$ = " " then doing_blanks% = 1%                ~
                                  else doing_blanks% = 0%
                gosub select_cur /* Select if blank; validate if not */

L32190:     if load_told% = 0 then                                       ~
                           call "SHOSTAT" ("Loading Purchase Directives")
            load_told% = 1
            c%, maxlines% = maxlines% + 1% : lastseq% = lastseq% + 1%
            pobufkey$(c%) = str(oldreadkey$,,66)

            get #22, using L32280, part$(c%,1%), temp2$,                  ~
                       qtyorig(c%), ext, part$(c%,2%), duedate$(c%),     ~
                       meas$(c%), factor(c%), price(c%), hnycost(c%),    ~
                       hnycostd$(c%), piptag$(c%), job$(c%),             ~
                                contract_id$(c%),  contract_line$(c%)
L32280:         FMT XX(13), CH(25), CH(6), XX(22), 2*PD(14,4), CH(25),   ~
                    CH(6), CH(4), PD(14,4), PD(14,7), PD(14,4), CH(96),  ~
                    XX(4), CH(19), POS(261), CH(8), CH(16), CH(4)

            if temp2$ < mindate$ then mindate$ = temp2$
            if duedate$(c%) < lstdate$ then lstdate$ = duedate$(c%)
            if mindate$ < date then mindate$ = date
            if lstdate$ < date then lstdate$ = date
            buyerl$(c%) = defbuyer$
            call "DATEFMT" (duedate$(c%))
            origdue$(c%)  = duedate$(c%)
            nextdate$(c%) = duedate$(c%)
            convert lastseq% to seqnr$(c%), pic(###)
            str$(c%)     = defstr$
            acct$(c%)    = expacct$
            qtyonord(c%) = qtyorig(c%)
            gosub next_revision
            linerev$(c%) = rev_level$
            status$(c%) = "A"
            appnd$(c%) = "R"
            line_source%(c%) = 1%
            gosub purchase_job_check

            call "READ100" (#8, part$(c%,1%), f1%(8))
            if f1%(8) = 0% then L32600
                  get #8, using L32520, partdescr$(c%), stkg$(c%),        ~
                                       cat$(c%), type$(c%), costtype$(c%)
L32520:              FMT POS(26), CH(32), POS(74), CH(4), POS(90), CH(4),~
                         POS(180), CH(3), POS(307), CH(1)

            if hnycost(c%) <> -1 then L32575  /* Costs not deferred */
            if pos("FST" = costtype$(c%)) = 0 then L32560
            if postatstdcost$ <> "Y" then L32560
                ro% = 1%
                gosub call_stccosts
                ro% = 0%
                goto L32575
L32560:     temp = price(c%)
            if cur_loaded% <> 0% then temp = temp * cur_factor1
            call "CONVERT" (temp, 4.4, hnycost$)
            temp$ = "I"
            gosub call_hnycdist2

L32575:     temp% = 3%
            if pj_on% <> 1% then L32580
                if job$(c%) = "PJ" then temp% = 2%

L32580:     call "HNYGLGET" (part$(c%,1%), str$(c%), lot$(c%), acct$(c%),~
                                                          temp%, #8, #9)

L32600:         call "GLFMT" (acct$(c%))
            get #3, using L32610, pcacct$(c%)
L32610:         FMT POS(318), CH(9)
            if pcacct$(c%) <> " " then L32645
            call "READ100" (#2, "DEFAULTS.STORE."& str(str$(c%)), f1%(2))
                if f1%(2) = 0% then L32640
            get #2 using L32635, pcacct$(c%)
L32635:         FMT POS(162), CH(9)
L32640:     if pcacct$(c%) = " " then pcacct$(c%) = sysacct$(6)
L32645:         call "GLFMT" (pcacct$(c%))
            if cur_tran$ = " " then cur_tran$ = cur_stat$
                gosub deffn_041
                gosub deffn_040

            gosub set_vpc_type

            goto L32112

*        Increment revision level and date if Line added.
        next_revision
            if rev_level$ <> "+1" then L32750
                rev_level$ = origrev$
                goto L32770
L32750:     if origrev$ <> rev_level$ then return  /* Already upped    */
            if rev_date$ = date$ then return       /* 1-a-day          */
L32770:         if rev_level$ <> " " then L32810
                     rev_level$ = " A"
                     rev_date$  = date$
                     return
L32810:         rev2% = val(str(rev_level$,2,1))
                rev1% = val(str(rev_level$,,1))
                rev2% = rev2% + 1%
                if rev2% < 91% then L32870
                     rev2% = 65%
                     rev1% = min(rev1% + 1%, 90%)
L32870:         str(rev_level$,2,1) = bin(rev2%)
                str(rev_level$,,1) = bin(rev1%)
                rev_date$  = date$ /* If Rev Level changes; Date chngs */
            return

        select_cur
            if cur_stat$ = " " then return        /* Multi-Currency OFF */
                temp$ = hex(0684) & "Select Currency for Purchase Order."
                cur_tran$ = cur_dir$
                if cur_tran$ = " " then cur_tran$ = hex(00) /*Force Plow*/
                ourprice$ = " "
L32960:         gosub test_currency
                if cur_loaded% = 1% or cur_tran$ = cur_stat$ then return
                     temp$ = hex(0684) & "No Exchange Rates on file for"&~
                             " " & cur_tran$ & ". Choose other."
                     ask_msg$(1%) = "You are attempting to create a PO "&~
                                    "from a directive with currency " &  ~
                                    cur_tran$ & "."
                     ask_msg$(2%) = "There are no Exchange Rates on " &  ~
                                    "file for this Currency."
                     ask_msg$(3%) = "Press PF1 to abort and enter " &    ~
                                    "rates -or- press RETURN to specify"&~
                                    " another."
L33070:              u3% = 0%
                     call "ASKUSER" (u3%, "CURRENCY PROBLEM",            ~
                           ask_msg$(1%), ask_msg$(2%), ask_msg$(3%))
                     if u3% = 1% then startover2
                     if u3% <> 0% then L33070
                          cur_tran$ = hex(00) /*Force Plow*/
                          errormsg$ = " "
                          goto L32960

        purchase_job_check
            pj_stat%(c%) = 0%
            if pj_on% <> 1% then return
                call "READ100" (#23, part$(c%,1%), f1%(23%))
                     if f1%(23%) = 0% then return       /* Not Planned */
                call "READ100" (#8, part$(c%,1%), f1%(8%))
                     if f1%(8%) = 0% then return        /* Not on File */
                get #8 using L34140, serial_no$
L34140:              FMT POS(131), CH(1)
                if serial_no$ = "Y" then return   /* No Serial # Parts */
                testdate$ = orddate$  :  call "DATUNFMT" (testdate$)
                call "BOMFIND" (part$(c%,1%), testdate$, #41, #2,        ~
                                                              bom_id$(c%))
                     if bom_id$(c%) = " " then return        /* No BOM */
                readkey2$ = str(part$(c%,1%)) & str(bom_id$(c%)) & "  0"
                call "READ100" (#40, readkey2$, f1%(40%))
                     if f1%(40%) = 0% then return            /* No BOM */
                pj_stat%(c%) = 1%               /* Maybe Purchased Job */
                get #40 using L34240, pj_bom$
L34240:              FMT POS(145), CH(1)
                if pj_bom$ <> "Y" then L34270
                pj_stat%(c%) = 3%                  /* Is Purchased Job */
L34270:         if line_source%(c%) = 0% then return   /* Direct Entry */
                if line_source%(c%) > 1% then L34380
                                             /* Source is PORLSE Entry */
                     if str(piptag$(c%),1%,2%) <> "RW" then L34330
                          pj_stat%(c%) = 3%  :  job$(c%) = "PJ"
                          kit$(c%,1%) = kitcomp$
                          kit$(c%,2%) = traveler$
                          kit$(c%,3%) = picklist$
                          kit$(c%,4%) = byprod$
                          kit$(c%,5%) = slbom$
                          return
L34330:              if str(piptag$(c%),1%,2%) <> "RO" then L34360
                          pj_stat%(c%) = 1%  :  job$(c%) = " "
                          return
L34360:              pj_stat%(c%) = 0%
                     return
L34380:                                    /* Source is VBKLINES Entry */
                if job$(c%) <> " " then L34430
                     piptag$(c%) = "PO" & str(ponumber$,1%,14%) &        ~
                                   str(seqnr$(c%))
                     return
L34430:         if str(job$(c%),1%,2%) = "PJ" then L34460
                     pj_stat%(c%) = 0%             /* Shouldn't Happen */
                     return
L34460:                                /* Purchased Job Already Set-up */
                piptag$(c%) = "JOB ORDER: " & job$(c%)
                pj_stat%(c%) = 5%              /* Active Purchased Job */
                call "READ100" (#18, job$(c%), f1%(18%))
                     if f1%(18%) = 0% then pj_stat%(c%) = 7%   /* Del? */
                return

        pj_kit_call
            if pj_on% <> 1% then return
            if job$(c%) <> "PJ" then return
                pj_date$ = orddate$
                testdate$ = duedate$(c%)
                call "DATUNFMT" (pj_date$)
                call "DATUNFMT" (testdate$)
                call "PJINPSUB" (part$(c%,1%), partdescr$(c%),           ~
                                 vencode$, vendescr$, part$(c%,2%),      ~
                                 piptag$(c%), pj_date$, testdate$,       ~
                                 bom_id$(c%), qtyorig(c%), pj_stat%(c%), ~
                                 kitmode%, #34, #8, #40, #41, #23, #38,  ~
                                 #2, #50)
                if kitmode% = 99% then gosub startover2

                return

        save_purchase_job_data
            if pj_stat%(c%) = 0% then return
            on line_source%(c%) + 1% gosub L34730, L34820, L34940
            return

L34730
*        Clean-up for PIP, etc. for Direct Entry Line
            if piptag$(c%) = " " then return
                if job$(c%) <> " " then  L34780
                     gosub delete_work_file
                     return
L34780:         if job$(c%) <> "PJ" then return
                     gosub update_kit_list
                     return

L34820
*        Clean-up for PIP, etc. for PORLSE Entry Line
            if job$(c%) <> "PJ" then L34870
                if mod(pj_stat%(c%),2%) = 1% then return
                     gosub update_kit_list
                     return
L34870:     if str(piptag$(c%),1%,2%) <> "RO" then L34900
                gosub delete_work_file
                return
L34900:     gosub delete_work_file
            gosub update_kit_list
            return

L34940
*        Clean-up for PIP, etc. for VBKLINES Entry Line
            if pj_stat%(c%) > 4% then return
                if job$(c%) <> " " then L35000
                     if mod(pj_stat%(c%),2%) = 1% then return
                          gosub delete_work_file
                          return
L35000:         if mod(pj_stat%(c%),2%) = 1% then return
                     gosub update_kit_list
                     return

        delete_work_file
            if piptag$(c%) = " " then return
                init (hex(00))  plowkey$
                plowkey$ = piptag$(c%)
                call "DELETE" (#50, plowkey$, 19%)
                return

        update_kit_list
            if piptag$(c%) = " " then return
                pj_date$ = orddate$
                testdate$ = duedate$(c%)
*              CALL "DATUNFMT" (PJ_DATE$)
*              CALL "DATUNFMT" (TESTDATE$)
                call "PJINPSUB" (part$(c%,1%), partdescr$(c%), vencode$, ~
                                 vendescr$, part$(c%,2%), piptag$(c%),   ~
                                 pj_date$, testdate$, bom_id$(c%),       ~
                                 qtyorig(c%), pj_stat%(c%), 3%, #34, #8, ~
                                 #40, #41, #23, #38, #2, #50)
                return

        REM *************************************************************~
            *      I N P U T / E D I T   S C R E E N   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Input and Edit for first page of header.                  *~
            *************************************************************

            deffn_101                    /* Input Mode       */
                init(hex(8c)) lfac$()
                gosub set_fac1
                gosub set_pfi1
                if fieldnr% = 1% then lfac$(3) = lfac$(1)
                goto L40215

            deffn_111                    /* Edit Mode        */
                init(hex(8c)) lfac$()
                if fieldnr% = 0% then init(hex(86)) str(lfac$(),4)
                if fieldnr% = 0% then init(hex(86)) str(lfac$(2))
                gosub set_fac1
                gosub set_pfe1
                goto L40215

        set_fac1: on fieldnr% gosub L40180,         /* VENDOR CODE      */~
                                    L40180,         /* Vendor BF Code   */~
                                    L40180,         /* PONUMBER         */~
                                    L40165,         /* Name and Address */~
                                    L40180,         /* VENDOR TYPE      */~
                                    L40165,         /* CONTACT'S NAME   */~
                                    L40180,         /* DEFSTORE         */~
                                    L40180,         /* Revision Level   */~
                                    L40180,         /* Revision Date    */~
                                    L40180          /* Buyer Code       */
                     return

L40165:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40180:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40215:     buyshp$ = "Buy From:" : str(dspaddr$()) = str(venaddr$())
            if buyshp% = 1% then buyshp$ = "Ship To :"
            if buyshp% = 1% then    str(dspaddr$()) = str(shpaddr$())
            if str(dspaddr$(6),27,1) = " " then zip_dash$ = " "          ~
                                           else zip_dash$ = "-"
            str(line2$,1%,19%) = "A/P Date: " & paydate$
            str(line2$,31%,29%) = "Last P.O. #: " & lastpo$

            accept                                                       ~
               at (01,02), "Enter/Manage Purchase Orders",               ~
               at (01,32), "Last Vendor:",                               ~
               at (01,45), fac(hex(84)), lastven$               , ch( 9),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(8c)), line2$                 , ch(79),~
               at (03,02), fac(hex(ac)), blank$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Vendor Code",                                ~
               at (06,30), fac(lfac$( 1)), vencode$             , ch(09),~
               at (06,49), fac(hex(8c))  , vendescr$            , ch(30),~
               at (07,02), "Buy From ID",                                ~
               at (07,30), fac(lfac$( 2)), bfcode$              , ch(06),~
               at (07,49), fac(hex(8c))  , bfdescr$             , ch(30),~
               at (08,02), "Purchase Order Number",                      ~
               at (08,30), fac(lfac$( 3)), ponumber$            , ch(14),~
               at (09,02), "????????: Name",                             ~
               at (09,02), fac(hex(8c)), buyshp$                , ch(09),~
               at (09,30), fac(lfac$( 4)), dspaddr$(1)          , ch(30),~
               at (10,02), "          Address 1",                        ~
               at (10,30), fac(lfac$( 4)), dspaddr$(2)          , ch(30),~
               at (11,02), "          Address 2",                        ~
               at (11,30), fac(lfac$( 4)), dspaddr$(3)          , ch(30),~
               at (12,02), "          Address 3",                        ~
               at (12,30), fac(lfac$( 4)), dspaddr$(4)          , ch(30),~
               at (13,02), "          Address 4",                        ~
               at (13,30), fac(lfac$( 4)), dspaddr$(5)          , ch(30),~
               at (14,02), "          City ST Zip",                      ~
               at (14,30), fac(lfac$( 4)), str(dspaddr$(6),1,17), ch(17),~
               at (14,50), fac(lfac$( 4)), str(dspaddr$(6),19,2), ch(02),~
               at (14,54), fac(lfac$( 4)), str(dspaddr$(6),22,5), ch(05),~
               at (14,60), fac(hex(8c)),   zip_dash$            , ch(01),~
               at (14,62), fac(lfac$( 4)), str(dspaddr$(6),27,4), ch(04),~
               at (15,02), "Vendor Type Code",                           ~
               at (15,30), fac(lfac$( 5)), ventype$             , ch(04),~
               at (15,49), fac(hex(8c)),   ventypedesc$         , ch(30),~
               at (16,02), "Contact's Name",                             ~
               at (16,30), fac(lfac$( 6)), vencont$             , ch(20),~
               at (16,55), "Phone:",                                     ~
               at (16,62), fac(hex(8c)),   phone$               , ch(16),~
               at (17,02), "Default Store Number",                       ~
               at (17,30), fac(lfac$( 7)), defstr$              , ch(03),~
               at (17,49), fac(hex(8c)),   defstrdescr$         , ch(32),~
               at (18,02), "Revision Level   ",                          ~
               at (18,30), fac(lfac$( 8)), rev_level$           , ch(02),~
               at (19,02), "Last Revision Date",                         ~
               at (19,30), fac(lfac$( 9)), rev_date$            , ch(08),~
               at (20,02), "Default Buyer Code",                         ~
               at (20,30), fac(lfac$(10)), buyer$               , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1)                 , ch(79),~
               at (23,02), fac(hex(8c)), pf$(2)                 , ch(79),~
               at (24,02), fac(hex(8c)), pf$(3)                 , ch(79),~
                     keys(pfk$),                                         ~
                     key (keyhit%)
            if keyhit% = 13% or keyhit% = 15% then gosub keyhit1315
            if keyhit% = 13% or keyhit% = 15% then L40215

               if keyhit% <> 27% then L40585
                   textmsg$ = "1st Boiler Plate Text for PO: " & ponumber$
                   call "TXTDSPLY" (#20, f201%, "CPY", textmsg$,         ~
                                               copytextid$(1%), texta$())
               goto L40215

L40585:     if buyshp% = 0% then str(venaddr$()) = str(dspaddr$())       ~
                            else str(shpaddr$()) = str(dspaddr$())

                return

        set_pfi1     /* PF Keys for Input Mode               */
            init (" ") pf$()
            pfk$ = hex(000104080a0d0e0f10120b1affffffffffffffffffff)
            pf$(1) = "(1)Start Over                  (10/26)Mn"   &      ~
                     "g Buy/Service Orders   (13)Instructions"
            pf$(2) = "               (4)Prev. Field  (11)See B"   &      ~
                     "uyer's Directives      (15)Print Screen"
            pf$(3) = "(8)Procurement History         (14)Find "   &      ~
                     "Existing P.O.          (16)EXIT PROGRAM"
            if fieldnr% < 4% then goto L40675
                str(pf$(1),32,30) = " " : str(pfk$, 5,1) = hex(ff)
                                          str(pfk$, 12%,1%) = hex(ff)
                str(pf$(2),32,26) = " " : str(pfk$,11,1) = hex(ff)
                str(pf$(3),32,22) = " " : str(pfk$, 7,1) = hex(ff)
L40675:     if buyer% = 0 then str(pf$(1),32,30) = " "
            if buyer% = 0 then str(pfk$,5,1) = hex(ff)
            if buyer% = 0% then str(pfk$, 12%,1%) = hex(ff)
            if defbuyer$ <> " " then goto L40695
                str(pf$(2),32,26) = " " : str(pfk$,11,1) = hex(ff)
L40695:     return

        set_pfe1     /* PF Keys for Edit Mode                */
            init (" ") pf$()
            pfk$ = hex(000102030508090d0f101112181b1dffffffffffffffff)
            pf$(1) = "(1)Start Over              (2)Line Items"   &      ~
                     " (9)Change Line Dates  (13)Instructions"
            pf$(2) = "(27)See PO Copy Text #1    (3)Show Buy F"   &      ~
                     "rom Addr               (15)Print Screen"
            pf$(3) = "(24)Maintain Header Text   (5)Next Page "   &      ~
                     "(8)Proc. History       (16)SAVE DATA   "
            if buyshp% = 0% then str(pf$(2),31,18) = "Show Ship To Addr "
            if str(copytextname$()) <> " " then L40775
                str(pf$(2),,25) = " "
                str(pfk$,14,1) = hex(ff)

L40775:     return

        REM *************************************************************~
            *      I N P U T / E D I T   S C R E E N   P A G E   2      *~
            * --------------------------------------------------------- *~
            * Input and Edit for second page of Header.                 *~
            *************************************************************

            deffn_102
                init(hex(8c)) lfac$()
                gosub set_fac2
                gosub set_pfi2
                goto L41430

            deffn_112
                init(hex(8c)) lfac$()
                if fieldnr% = 0% then init(hex(86)) lfac$()
                gosub set_fac2
                gosub set_pfe2
                goto L41430

        set_fac2: on fieldnr% gosub L41360,         /* Liability Acct   */~
                                    L41360,         /* Taxable?         */~
                                    L41360,         /* Frieght Terms    */~
                                    L41360,         /* FOB              */~
                                    L41360,         /* Ship Via         */~
                                    L41360,         /* ORDER DATE       */~
                                    L41360,         /* DEFAULT DUE DATE */~
                                    L41360,         /* CANCELLATION DATE*/~
                                    L41360,         /* CONFIRMED? (Y/N) */~
                                    L41360,         /* Maximum Dollars  */~
                                    L41360,         /* Maximum Date     */~
                                    L41360          /* Copy Text ID     */
                     return

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41360:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41430:     gosub describe_id1
            str(id1$,71) = " "
            init (" ")  str(line2$,1%,61%)
            str(line2$,1%,19%) = "A/P Date: " & paydate$

L41460:     accept                                                       ~
               at (01,02), "Enter/Manage Purchase Orders, Page 2",       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(8c)), line2$                 , ch(79),~
               at (03,02), fac(hex(ac)), id1$                   , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Liability Account",                          ~
               at (06,30), fac(lfac$( 1)), puracct$             , ch(12),~
               at (06,49), fac(  hex(8c)), puracctdescr$        , ch(32),~
               at (07,02), "Taxable? (Y/N)",                             ~
               at (07,30), fac(lfac$( 2)), taxflag$             , ch(01),~
               at (08,02), "Freight Terms",                              ~
               at (08,30), fac(lfac$( 3)), frgtflag$            , ch(01),~
               at (08,40), fac(  hex(8c)), frgtflagdescr$       , ch(32),~
               at (09,02), "F.O.B.",                                     ~
               at (09,30), fac(lfac$( 4)), fob$                 , ch(30),~
               at (10,02), "Ship Via",                                   ~
               at (10,30), fac(lfac$( 5)), shipvia$             , ch(30),~
               at (11,02), "Order Date",                                 ~
               at (11,30), fac(lfac$( 6)), orddate$             , ch(08),~
               at (12,02), "Default Due Date For Lines",                 ~
               at (12,30), fac(lfac$( 7)), duedate$             , ch(08),~
               at (13,02), "Cancellation Date",                          ~
               at (13,30), fac(lfac$( 8)), candate$             , ch(08),~
               at (14,02), "Confirmed? (Y/N)",                           ~
               at (14,30), fac(lfac$( 9)), confirm$             , ch(01),~
               at (15,02), "Max. Monetary Limit",                        ~
               at (15,30), fac(lfac$(10)), maxdlrs$             , ch(10),~
               at (15,49), fac(hex(8c)), curr_dsply$            , ch(22),~
               at (16,02), "Max. Date   Limit",                          ~
               at (16,30), fac(lfac$(11)), maxdate$             , ch(08),~
               at (17,02), "Copy Text ('Boiler Plate')",                 ~
               at (17,30), fac(lfac$(12)), copytextname$(1%)    , ch(10),~
               at (17,49), fac(hex(8c)),   copytextdescr$(1%)   , ch(30),~
               at (18,30), fac(lfac$(12)), copytextname$(2%)    , ch(10),~
               at (18,49), fac(hex(8c)),   copytextdescr$(2%)   , ch(30),~
               at (19,30), fac(lfac$(12)), copytextname$(3%)    , ch(10),~
               at (19,49), fac(hex(8c)),   copytextdescr$(3%)   , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1)                 , ch(79),~
               at (23,02), fac(hex(8c)), pf$(2)                 , ch(79),~
               at (24,02), fac(hex(8c)), pf$(3)                 , ch(79),~
                     keys(pfk$),                                         ~
                     key (keyhit%)

               if keyhit% = 13% or keyhit% = 15% then gosub keyhit1315
               if keyhit% = 13% or keyhit% = 15% then L41460

               if keyhit% <> 27% then L41980
                   gosub call_screen
                   n% = cursor%(1%) - 16%
                   if n% < 1% or n% > 3% then L41980
                   if copytextid$(n%) = " " then L41980
                   temp$ = "1st2nd3rd"
                   textmsg$ = "Boiler Plate Text for PO: " & ponumber$
                   textmsg$ = str(temp$, ((n%*3%)-2%),3%) & " " & textmsg$
                   call "TXTDSPLY" (#20, f201%, "CPY", textmsg$,         ~
                                               copytextid$(n%), texta$())
                   goto L41460
L41980:                                  f201% = f201%

            blank$ = " "
            return

        set_pfi2     /* PF Keys for Input Mode               */
            init (" ") pf$()
            pfk$ = hex(000104080d0f10ffffffffffffffffffffffffffffff)
            pf$(1) = "(1)Start Over                           "   &      ~
                     "                       (13)Instructions"
            pf$(2) = "                    (4)Prev. Field      "   &      ~
                     "                       (15)Print Screen"
            pf$(3) = "(8)Procurement History                  "   &      ~
                     "                       (16)EXIT PROGRAM"
            return

        set_pfe2     /* PF Keys for Edit Mode                */
            init (" ") pf$()
            pfk$ = hex(0001020405080d0f10181d1bffffffffffffffffffff)
            pf$(1) = "(1)Start Over              (2)Line Items"   &      ~
                     "                       (13)Instructions"
            pf$(2) = "(27)See PO Copy Text       (4)Prev Page "   &      ~
                     "                       (15)Print Screen"
            pf$(3) = "(24)Maintain Header Text   (5)Free Text "   &      ~
                     "(8)Proc. History       (16)SAVE DATA   "
            if str(copytextname$()) <> " " then L42260
                str(pf$(2),,25) = " "
                str(pfk$,12,1) = hex(ff)
L42260:     return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   3      *~
            * --------------------------------------------------------- *~
            * Input/Edit of First Line Item Screen.                     *~
            *************************************************************

            deffn_103                    /* Input Mode       */
                init(hex(8c)) lfac$()
                gosub set_fac3
                gosub set_pfi3
                goto  L43255

            deffn_113                    /* Edit Mode        */
                init(hex(86)) lfac$() : lfac$(1) = hex(8c)
                if fieldnr% <> 0% then init(hex(8c)) lfac$()
                gosub set_fac3
                gosub set_pfe3
                goto L43255

        set_fac3: on fieldnr% gosub L43200,         /* PART NUMBER      */~
                                    L43200,         /* PART DESCRIPTION */~
                                    L43200,         /* STORE NUMBER     */~
                                    L43200,         /* LOT CODE         */~
                                    L43200,         /* PART CATEGORY    */~
                                    L43200,         /* VEN PART NUMBER  */~
                                    L43200,         /* UNIT OF ISSUE    */~
                                                   /* QUANITY PER UNIT */~
                                    L43200,         /* Purchase Contract*/~
                                    L43200,         /* JOB/PROJ NUMBER  */~
                                    L43200,         /* Currency         */~
                                    L43215,         /* ORIGINAL ORDER   */~
                                    L43215,         /* ON ORDER         */~
                                    L43215,         /* REC LAST SHPMNT  */~
                                    L43200,         /* PURCHASE PRICE   */~
                                    L43200,         /* EXTENSION        */~
                                    L43200,         /* INVENTORY COST   */~
                                    L43200          /* Line Status      */
                     return

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43200:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L43215:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
                  REM SET FAC'S FOR BRIGHT PROTECT, TAB STOP, DUMMY CURSOR
                      lfac$(fieldnr%) = hex(86)
                      lfac$(20%)      = hex(80)
                      return

L43255:     gosub describe_id1
            if part$(c%,1%) = " " then lfac$(8%) = hex(9c)
            if fieldnr% = 16% then return /* No screen for Inv cost    */

            accept                                                       ~
               at (01,02), "Enter/Manage Purchase Order - Line Items",   ~
               at (01,44), "Page 1",                                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(8c)), id1$                   , ch(79),~
               at (02,77), fac(hex(84)), c%                   , pic(###),~
               at (03,02), fac(hex(ac)), blank$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Part Number",                                ~
               at (05,30), fac(lfac$( 1)), part$(c%,1%)         , ch(25),~
               at (06,02), "   Part Description",                        ~
               at (06,30), fac(lfac$( 2)), partdescr$(c%)       , ch(32),~
               at (07,02), "   Store Number",                            ~
               at (07,30), fac(lfac$( 3)), str$(c%)             , ch(03),~
               at (07,34), "   Lot Code",                                ~
               at (07,49), fac(lfac$( 4)), str(lot$(c%),,ll%),           ~
               at (08,02), "Part Category",                              ~
               at (08,30), fac(lfac$( 5)), cat$(c%)             , ch(04),~
               at (08,49), fac(hex(8c))  , catdescr$            , ch(32),~
               at (09,02), "Vendor's Part Number",                       ~
               at (09,30), fac(lfac$( 6)), part$(c%,2%)         , ch(25),~
               at (09,57), "Stock UOM",                                  ~
               at (09,70), fac(hex(8c)),   stkg$(c%)            , ch(04),~
               at (10,02), "Vendor Unit Of Shipment",                    ~
               at (10,30), fac(lfac$( 7)), meas$(c%)            , ch(04),~
               at (10,42), "Quantity per Vendor Unit",                   ~
               at (10,70), fac(lfac$( 7)), factor$              , ch(10),~
               at (11,02), "Purchase Contract ID",                       ~
               at (11,30), fac(lfac$( 8%)), contract_id$(c%)  ,   ch(16),~
               at (11,48), "Line",                                       ~
               at (11,54), fac(lfac$( 8%)), contract_line$(c%),   ch( 4),~
               at (11,60), fac(hex(8c))  , contract_descr$      , ch(22),~
               at (12,02), "Specific Job/Proj Code",                     ~
               at (12,30), fac(lfac$( 9%)), str(job$(c%),1%,job%),       ~
               at (12,49), fac(hex(8c))  , jobdescr$            , ch(32),~
               at (13,02), "Transaction Currency",                       ~
               at (13,30), fac(lfac$(10)), cur_tran$            , ch(04),~
               at (13,41), "  INTERNAL @",                               ~
               at (13,54), fac(hex(8c)),   stkg_descr$          , ch(25),~
               at (14,02), "Original Order Quantity",                    ~
               at (14,30), fac(lfac$(11)), venorig$             , ch(10),~
               at (14,41), fac(lfac$(11)), qtyorig$             , ch(10),~
               at (14,55), fac(hex(8c)),   cur_descr$(5)        , ch(25),~
               at (15,02), "   Remaining on Order",                      ~
               at (15,30), fac(lfac$(12)), venonord$            , ch(10),~
               at (15,41), fac(lfac$(12)), qtyonord$            , ch(10),~
               at (16,02), "   Total Received to Date",                  ~
               at (16,30), fac(lfac$(13)), venrecd$             , ch(10),~
               at (16,41), fac(lfac$(13)), qtyrecd$             , ch(10),~
               at (16,55), fac(hex(8c)),   cur_descr$(1)        , ch(25),~
               at (17,02), "Purchase Price",                             ~
               at (17,30), fac(lfac$(14)), venprice$            , ch(10),~
               at (17,41), fac(lfac$(14)), ourprice$            , ch(10),~
               at (17,55), fac(hex(8c)),   cur_descr$(2)        , ch(25),~
               at (18,02), "Original/Current Extension",                 ~
               at (18,30), fac(lfac$(15)), extg$(c%)            , ch(10),~
               at (18,41), fac(  hex(8c)), ext$ (c%)            , ch(10),~
               at (18,55), fac(hex(8c)),   cur_descr$(3)        , ch(25),~
               at (19,02), "Inventory Cost",                             ~
         /*    AT (19,41), FAC(LFAC$(20)), HNYCOST$             , CH(10),~
          */   at (19,41), fac(lfac$(16)), hnycost$             , ch(10),~
               at (19,55), fac(hex(8c)),   cur_descr$(4)        , ch(25),~
               at (20,02), "Line Item Status",                           ~
               at (20,30), fac(lfac$(17)), status$(c%)          , ch(01),~
               at (20,49), fac(  hex(8c)), statusdescr$         , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1),                         ~
               at (23,02), fac(hex(8c)), pf$(2),                         ~
               at (24,02), fac(hex(8c)), pf$(3),                         ~
                    keys(pfk$),  key(keyhit%)

            if keyhit% = 13% or keyhit% = 15% then gosub keyhit1315
            if keyhit% = 13% or keyhit% = 15% then L43255


                return

        set_pfi3     /* PF Keys for Input Mode               */
            init (" ") pf$()
            pfk$ = hex(010203040506ff08090a0d0f10ffffffffffffffff00)
            pf$(1) = "(1)Start PO Over                        "   &      ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Restart Line     (4)Prev. Field      "   &      ~
                     "           (9)Size Run (15)Print Screen"
            pf$(3) = "(3)Copy Prev (5)Date Run (6)Same as Prev"   &      ~
                     "  (8)Hist  (10)See PIP (16)EDIT MODE   "
            if fieldnr% = 1% and maxlines% > 0% then L43674
                str(pf$(3),,25) = " " : str(pfk$,3,1) = hex(ff)
                str(pfk$,5,1) = hex(ff)
L43674:     if maxlines% > 0% then L43676
                str(pf$(3),26,15) = " " : str(pfk$,6,1) = hex(ff)
L43676:     if vpc_open% <> 1% then return
                str(pf$(1%),43%,12%) = "(25)Contract"
                str(pfk$,25%,1%) = hex(19)
            return

        set_pfe3     /* PF Keys for Edit Mode                */
            init (" ") pf$()
            pfk$ = hex(000105060708090a0d0f101dffffffffffffffffffffffff)
            pf$(1) = "(1)Start PO Over                        "   &      ~
                     "                       (13)Instructions"
            pf$(2) = "                    (6)Prev Line   (8)Pr"   &      ~
                     "oc Hist  (10)See PIP   (15)Print Screen"
            pf$(3) = "(5)Next Page        (7)Next Line   (9)He"   &      ~
                     "ader Screen            (16)LINE SUMMARY"
            if pj_on% <> 1% then L43755
                if job$(c%) <> "PJ" then L43748
                     str(pf$(1%),50%,12%) = "(11)Kit List"
                     str(pfk$,13%,1%) = hex(0b)
L43748:     if vpc_open% <> 1% then return
                str(pf$(1%),36%,12%) = "(25)Contract"
                str(pfk$,25%,1%) = hex(19)
L43755:     return

        REM *************************************************************~
            *      I N P U T / E D I T   S C R E E N   P A G E   4      *~
            * --------------------------------------------------------- *~
            * Line Item Screen - Page 2.                                *~
            *************************************************************

            deffn_104                    /* Input Mode       */
                init(hex(8c)) lfac$()
                gosub set_fac4
                gosub set_pjfaci4
                gosub set_pfi4
                goto  L45220

            deffn_114                    /* Edit Mode        */
                init(hex(86)) lfac$()
                if fieldnr% <> 0% then init(hex(8c)) lfac$()
                gosub set_fac4
                gosub set_pjface4
                gosub set_pfe4
                goto L45220

        set_fac4: on fieldnr% gosub L45185,         /* LINE DUE DATE    */~
                                    L45185,         /* Original Due Date*/~
                                    L45185,         /* NEXT SHIPMENT    */~
                                    L45185,         /* LAST RECIVED DATE*/~
                                    L45185,         /* PACK SLIP        */~
                                    L45185,         /* EXPENSE ACCT     */~
                                    L45185,         /* PC VAR. ACCT     */~
                                    L45185,         /* Revision Level   */~
                                    L45185,         /* Deliver To       */~
                                    L45185,         /* Requisitioner    */~
                                    L45185,         /* Internal Ref #   */~
                                    L45185          /* Buyer Code       */
                     return

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L45185:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L45220:     gosub describe_id2

            accept                                                       ~
               at (01,02), "Enter/Manage Purchase Order - Line Items",   ~
               at (01,44), "Page 2",                                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(8c)), id1$                   , ch(79),~
               at (02,77), fac(hex(84)), c%                   , pic(###),~
               at (03,02), fac(hex(ac)), id2$                   , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Current Due Date",                           ~
               at (05,30), fac(lfac$( 1)), duedate$(c%)         , ch(08),~
               at (05,40), "Not Before", at(05,61), "Not After",         ~
               at (05,51), fac(lfac$( 1)), notbefore$(c%)       , ch(08),~
               at (05,71), fac(lfac$( 1)), notafter$ (c%)       , ch(08),~
               at (06,02), "Original Due Date",                          ~
               at (06,30), fac(lfac$( 2)), origdue$(c%)         , ch(08),~
               at (07,02), "Next Shipment Date",                         ~
               at (07,30), fac(lfac$( 3)), nextdate$(c%)        , ch(08),~
               at (08,02), "   Last Received Date",                      ~
               at (08,30), fac(lfac$( 4)), recdate$(c%)         , ch(08),~
               at (09,02), "   Last Vendor Pack Slip",                   ~
               at (09,30), fac(lfac$( 5)), packslip$(c%)        , ch(16),~
               at (10,02), "Expense Account",                            ~
               at (10,30), fac(lfac$(6%)), acct$(c%)            , ch(12),~
               at (10,49), fac(hex(8c))  , acctdescr$           , ch(32),~
               at (11,02), "Price Cost Account",                         ~
               at (11,30), fac(lfac$(7%)), pcacct$(c%)          , ch(12),~
               at (11,49), fac(hex(8c))  , pcacctdescr$         , ch(32),~
               at (12,02), "Line Revision Level",                        ~
               at (12,30), fac(lfac$(8%)), linerev$(c%)         , ch(02),~
               at (13,02), "Deliver To",                                 ~
               at (13,30), fac(lfac$(9%)), dlvrto$(c%)          , ch(20),~
               at (14,02), "Requisitioner",                              ~
               at (14,30), fac(lfac$(10%)), rqstnr$(c%)         , ch(20),~
               at (15,02), "Internal Ref #",                             ~
               at (15,30), fac(lfac$(11%)), lineref$(c%)        , ch(05),~
               at (16,02), "Buyer Code    ",                             ~
               at (16,30), fac(lfac$(12%)), buyerl$(c%)         , ch(03),~
               at (17,02), fac(pjfac$(1)), kitmsg$(1%)          , ch(12),~
               at (17,30), fac(pjfac$(2)), kit$(c%,1%)          , ch(01),~
               at (18,02), fac(pjfac$(1)), kitmsg$(2%)          , ch(18),~
               at (18,21), fac(pjfac$(2)), kit$(c%,2%)          , ch(01),~
               at (18,24), fac(pjfac$(1)), kitmsg$(3%)          , ch(10),~
               at (18,35), fac(pjfac$(2)), kit$(c%,3%)          , ch(01),~
               at (18,38), fac(pjfac$(1)), kitmsg$(4%)          , ch(17),~
               at (18,55), fac(pjfac$(2)), kit$(c%,4%)          , ch(01),~
               at (18,57), fac(pjfac$(1)), kitmsg$(5%)          , ch(17),~
               at (18,74), fac(pjfac$(2)), kit$(c%,5%)          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1),                         ~
               at (23,02), fac(hex(8c)), pf$(2),                         ~
               at (24,02), fac(hex(8c)), pf$(3),                         ~
                    keys(pfk$),  key(keyhit%)

            if keyhit% = 13% or keyhit% = 15% then gosub keyhit1315
            if keyhit% = 13% or keyhit% = 15% then L45220


                return

        set_pfi4     /* PF Keys for Input Mode               */
            init (" ") pf$()
            pfk$ = hex(0001020406080a0d0fffffffffffffffffffffffffff)
            pf$(1) = "(1)Start PO Over                        "   &      ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Restart Line     (4)Prev. Field      "   &      ~
                     "                       (15)Print Screen"
            pf$(3) = "                    (6)Same as Prev Line"   &      ~
                     "  (8)Proc Hist  (10)See PIP            "
            if change_date% <> 1% then L45544
                str(pf$(2),,15) = " "  : str(pfk$,3,1) = hex(ff)
L45544:     if vpc_open% <> 1% then return
                str(pf$(1%),43%,12%) = "(25)Contract"
                str(pfk$,25%,1%) = hex(19)
            return

        set_pfe4     /* PF Keys for Edit Mode                */
            init (" ") pf$()
            pfk$ = hex(000104060708090a0d0f101dffffffffffffffffffffff)
            pf$(1) = "(1)Start PO Over                        "   &      ~
                     "                       (13)Instructions"
            pf$(2) = "                    (6)Prev Line   (8)Pr"   &      ~
                     "oc Hist  (10)See PIP   (15)Print Screen"
            pf$(3) = "(4)Prev Page        (7)Next Line   (9)He"   &      ~
                     "ader Screen            (16)LINE SUMMARY"
            if pj_on% <> 1% then L45605
                if job$(c%) <> "PJ" then L45605
                     str(pf$(1%),50%,12%) = "(11)Kit List"
                     str(pfk$,13%,1%) = hex(0b)
L45605:     if vpc_open% <> 1% then return
                str(pf$(1%),36%,12%) = "(25)Contract"
                str(pfk$,25%,1%) = hex(19)
            return

        set_pjfaci4
            init (hex(9c))  pjfac$()
            if pj_on% <> 1 or pj_stat%(c%) = 0% then return
            if job$(c%) <> "PJ" then return
            init (hex(8c)) pjfac$()
            if fieldnr% = 13% then pjfac$(2%) = hex(81)
            return

        set_pjface4
            init (hex(9c))  pjfac$()
            if pj_on% <> 1 or pj_stat%(c%) = 0% then return
            if job$(c%) <> "PJ" then return
            pjfac$(1%) = hex(8c)
            if fieldnr% =  0% then pjfac$(2%) = hex(86)
            if fieldnr% <> 0% then pjfac$(2%) = hex(8c)
            if fieldnr% = 13% then pjfac$(2%) = hex(81)
            return

        REM *************************************************************~
            *       L I N E   S U M M A R Y   S C R E E N               *~
            * --------------------------------------------------------- *~
            * Line Item Summary Screen. (#5 if you're counting).        *~
            *************************************************************

            deffn_115
            init (hex(86)) lfac$()
            for x% = 1% to 15%
                if appnd$(x%+l%) = "D" then lfac$(x%) = hex(8e)
            next x%
            dl% = l% + 1%
            lsl% = min(maxlines%, l% + 15%)
            goto L46210

            deffn_125
            init (hex(8c)) lfac$()
            if fieldnr% <> 0% then lfac$(fieldnr%) = hex(94)             ~
                              else init (hex(94)) lfac$()
            goto L46210

L46210:     accept                                                       ~
               at (01,02), "Purchase Order Line Item Summary",           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), "Vendor Code:",                               ~
               at (02,15), fac(hex(84)), vencode$               , ch( 9),~
               at (02,25), fac(hex(8c)), vendescr$              , ch(30),~
               at (02,58), "Total Open:",                                ~
               at (02,70), fac(hex(84)), pototal$               , ch(10),~
               at (03,02), "P. O. Number:",                              ~
               at (03,16), fac(hex(84)), ponumber$              , ch(16),~
               at (03,41), "Displaying Lines",                           ~
               at (03,58), fac(hex(84)), dl%                  , pic(###),~
               at (03,62), " thru",                                      ~
               at (03,68), fac(hex(84)), lsl%                 , pic(###),~
               at (03,72), " of",                                        ~
               at (03,76), fac(hex(84)), maxlines%            , pic(###),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(a4)), headr1$(t% + 1%)       , ch(79),~
                                                                         ~
               at (06,02), fac(lfac$( 1)), part$(l%+ 1%,t%+1%)  , ch(25),~
               at (07,02), fac(lfac$( 2)), part$(l%+ 2%,t%+1%)  , ch(25),~
               at (08,02), fac(lfac$( 3)), part$(l%+ 3%,t%+1%)  , ch(25),~
               at (09,02), fac(lfac$( 4)), part$(l%+ 4%,t%+1%)  , ch(25),~
               at (10,02), fac(lfac$( 5)), part$(l%+ 5%,t%+1%)  , ch(25),~
               at (11,02), fac(lfac$( 6)), part$(l%+ 6%,t%+1%)  , ch(25),~
               at (12,02), fac(lfac$( 7)), part$(l%+ 7%,t%+1%)  , ch(25),~
               at (13,02), fac(lfac$( 8)), part$(l%+ 8%,t%+1%)  , ch(25),~
               at (14,02), fac(lfac$( 9)), part$(l%+ 9%,t%+1%)  , ch(25),~
               at (15,02), fac(lfac$(10)), part$(l%+10%,t%+1%)  , ch(25),~
               at (16,02), fac(lfac$(11)), part$(l%+11%,t%+1%)  , ch(25),~
               at (17,02), fac(lfac$(12)), part$(l%+12%,t%+1%)  , ch(25),~
               at (18,02), fac(lfac$(13)), part$(l%+13%,t%+1%)  , ch(25),~
               at (19,02), fac(lfac$(14)), part$(l%+14%,t%+1%)  , ch(25),~
               at (20,02), fac(lfac$(15)), part$(l%+15%,t%+1%)  , ch(25),~
                                                                         ~
               at (06,28), fac(lfac$( 1)), partdescr$(l%+ 1%)   , ch(25),~
               at (07,28), fac(lfac$( 2)), partdescr$(l%+ 2%)   , ch(25),~
               at (08,28), fac(lfac$( 3)), partdescr$(l%+ 3%)   , ch(25),~
               at (09,28), fac(lfac$( 4)), partdescr$(l%+ 4%)   , ch(25),~
               at (10,28), fac(lfac$( 5)), partdescr$(l%+ 5%)   , ch(25),~
               at (11,28), fac(lfac$( 6)), partdescr$(l%+ 6%)   , ch(25),~
               at (12,28), fac(lfac$( 7)), partdescr$(l%+ 7%)   , ch(25),~
               at (13,28), fac(lfac$( 8)), partdescr$(l%+ 8%)   , ch(25),~
               at (14,28), fac(lfac$( 9)), partdescr$(l%+ 9%)   , ch(25),~
               at (15,28), fac(lfac$(10)), partdescr$(l%+10%)   , ch(25),~
               at (16,28), fac(lfac$(11)), partdescr$(l%+11%)   , ch(25),~
               at (17,28), fac(lfac$(12)), partdescr$(l%+12%)   , ch(25),~
               at (18,28), fac(lfac$(13)), partdescr$(l%+13%)   , ch(25),~
               at (19,28), fac(lfac$(14)), partdescr$(l%+14%)   , ch(25),~
               at (20,28), fac(lfac$(15)), partdescr$(l%+15%)   , ch(25),~
                                                                         ~
               at (06,54), fac(lfac$( 1)), buyerl$   (l%+ 1%)   , ch(03),~
               at (07,54), fac(lfac$( 2)), buyerl$   (l%+ 2%)   , ch(03),~
               at (08,54), fac(lfac$( 3)), buyerl$   (l%+ 3%)   , ch(03),~
               at (09,54), fac(lfac$( 4)), buyerl$   (l%+ 4%)   , ch(03),~
               at (10,54), fac(lfac$( 5)), buyerl$   (l%+ 5%)   , ch(03),~
               at (11,54), fac(lfac$( 6)), buyerl$   (l%+ 6%)   , ch(03),~
               at (12,54), fac(lfac$( 7)), buyerl$   (l%+ 7%)   , ch(03),~
               at (13,54), fac(lfac$( 8)), buyerl$   (l%+ 8%)   , ch(03),~
               at (14,54), fac(lfac$( 9)), buyerl$   (l%+ 9%)   , ch(03),~
               at (15,54), fac(lfac$(10)), buyerl$   (l%+10%)   , ch(03),~
               at (16,54), fac(lfac$(11)), buyerl$   (l%+11%)   , ch(03),~
               at (17,54), fac(lfac$(12)), buyerl$   (l%+12%)   , ch(03),~
               at (18,54), fac(lfac$(13)), buyerl$   (l%+13%)   , ch(03),~
               at (19,54), fac(lfac$(14)), buyerl$   (l%+14%)   , ch(03),~
               at (20,54), fac(lfac$(15)), buyerl$   (l%+15%)   , ch(03),~
                                                                         ~
               at (06,58), fac(lfac$( 1)), appnd$    (l%+ 1%)   , ch(01),~
               at (07,58), fac(lfac$( 2)), appnd$    (l%+ 2%)   , ch(01),~
               at (08,58), fac(lfac$( 3)), appnd$    (l%+ 3%)   , ch(01),~
               at (09,58), fac(lfac$( 4)), appnd$    (l%+ 4%)   , ch(01),~
               at (10,58), fac(lfac$( 5)), appnd$    (l%+ 5%)   , ch(01),~
               at (11,58), fac(lfac$( 6)), appnd$    (l%+ 6%)   , ch(01),~
               at (12,58), fac(lfac$( 7)), appnd$    (l%+ 7%)   , ch(01),~
               at (13,58), fac(lfac$( 8)), appnd$    (l%+ 8%)   , ch(01),~
               at (14,58), fac(lfac$( 9)), appnd$    (l%+ 9%)   , ch(01),~
               at (15,58), fac(lfac$(10)), appnd$    (l%+10%)   , ch(01),~
               at (16,58), fac(lfac$(11)), appnd$    (l%+11%)   , ch(01),~
               at (17,58), fac(lfac$(12)), appnd$    (l%+12%)   , ch(01),~
               at (18,58), fac(lfac$(13)), appnd$    (l%+13%)   , ch(01),~
               at (19,58), fac(lfac$(14)), appnd$    (l%+14%)   , ch(01),~
               at (20,58), fac(lfac$(15)), appnd$    (l%+15%)   , ch(01),~
                                                                         ~
               at (06,60), fac(lfac$( 1)),sumonord$(l%+ 1%,t%+1%),ch(10),~
               at (07,60), fac(lfac$( 2)),sumonord$(l%+ 2%,t%+1%),ch(10),~
               at (08,60), fac(lfac$( 3)),sumonord$(l%+ 3%,t%+1%),ch(10),~
               at (09,60), fac(lfac$( 4)),sumonord$(l%+ 4%,t%+1%),ch(10),~
               at (10,60), fac(lfac$( 5)),sumonord$(l%+ 5%,t%+1%),ch(10),~
               at (11,60), fac(lfac$( 6)),sumonord$(l%+ 6%,t%+1%),ch(10),~
               at (12,60), fac(lfac$( 7)),sumonord$(l%+ 7%,t%+1%),ch(10),~
               at (13,60), fac(lfac$( 8)),sumonord$(l%+ 8%,t%+1%),ch(10),~
               at (14,60), fac(lfac$( 9)),sumonord$(l%+ 9%,t%+1%),ch(10),~
               at (15,60), fac(lfac$(10)),sumonord$(l%+10%,t%+1%),ch(10),~
               at (16,60), fac(lfac$(11)),sumonord$(l%+11%,t%+1%),ch(10),~
               at (17,60), fac(lfac$(12)),sumonord$(l%+12%,t%+1%),ch(10),~
               at (18,60), fac(lfac$(13)),sumonord$(l%+13%,t%+1%),ch(10),~
               at (19,60), fac(lfac$(14)),sumonord$(l%+14%,t%+1%),ch(10),~
               at (20,60), fac(lfac$(15)),sumonord$(l%+15%,t%+1%),ch(10),~
                                                                         ~
               at (06,71), fac(lfac$( 1)), ext$      (l%+ 1%)   , ch(10),~
               at (07,71), fac(lfac$( 2)), ext$      (l%+ 2%)   , ch(10),~
               at (08,71), fac(lfac$( 3)), ext$      (l%+ 3%)   , ch(10),~
               at (09,71), fac(lfac$( 4)), ext$      (l%+ 4%)   , ch(10),~
               at (10,71), fac(lfac$( 5)), ext$      (l%+ 5%)   , ch(10),~
               at (11,71), fac(lfac$( 6)), ext$      (l%+ 6%)   , ch(10),~
               at (12,71), fac(lfac$( 7)), ext$      (l%+ 7%)   , ch(10),~
               at (13,71), fac(lfac$( 8)), ext$      (l%+ 8%)   , ch(10),~
               at (14,71), fac(lfac$( 9)), ext$      (l%+ 9%)   , ch(10),~
               at (15,71), fac(lfac$(10)), ext$      (l%+10%)   , ch(10),~
               at (16,71), fac(lfac$(11)), ext$      (l%+11%)   , ch(10),~
               at (17,71), fac(lfac$(12)), ext$      (l%+12%)   , ch(10),~
               at (18,71), fac(lfac$(13)), ext$      (l%+13%)   , ch(10),~
               at (19,71), fac(lfac$(14)), ext$      (l%+14%)   , ch(10),~
               at (20,71), fac(lfac$(15)), ext$      (l%+15%)   , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   selectmsg$           , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over (2)First (4)Prev (6)Down (8)History (24)~
        ~Text     (13)Instructions",                                      ~
               at (23,02),                                               ~
                  "              (3)Last  (5)Next (7)Up   (9)Header  (11)~
        ~Insert   (15)Print Screen",                                      ~
               at (24,02),                                               ~
                  "              (25)Change Dates (10)Change Screen  (12/~
        ~28)Delete(16)SAVE DATA   ",                                      ~
                                                                         ~
               keys(hex(000102030405060708090a0b0c0d0f10181c19)),        ~
               key (keyhit%)

               if keyhit% = 13% or keyhit% = 15% then gosub keyhit1315
               if keyhit% = 13% or keyhit% = 15% then L46210
               if keyhit% <> 10% then L47570
                  t% = mod(t% + 1%, 2%)
                  goto L46210

L47570:        return

*        Screen Subroutine (Misc and Asundry)
*        Internal routine to handle PF 13's and 15's.
        keyhit1315
            if keyhit% = 13% then call "MANUAL" ("VBKINPUT")
            if keyhit% = 15% then call "PRNTSCRN"
          return


*        This code supplies the ID lines for second page screens
        describe_id2      /* For line item screens */
            id2$ = "  Part " & part$(c%,1%) & "  " & partdescr$(c%)
            str(id2$,57) = " Store " & str$(c%) & " Lot " & lot$(c%)

        describe_id1      /* For line and header screens */
            id1$ = "Vendor " & vencode$
            if bfcode$ = " " then id1$ = id1$ & "  " & vendescr$         ~
                 else id1$ = id1$ & "-" & bfcode$ & "  " & bfdescr$
            str(id1$,50) = "PO " & ponumber$
            str(id1$,71) = "Line :###"
            return

        REM *************************************************************~
            *          D A T E   N O T I F I C A T I O N                *~
            * --------------------------------------------------------- *~
            * Notify User that Maximum Change Date has been exceeded    *~
            * and see what he wants to do about it.                     *~
            *************************************************************
        date_notification
            call "DATEFMT" (maxdate$)

            keyhit1%    = 2%
            errormsg$   = "MAXIMUM CHANGE DATE EXCEEDED: " & maxdate$
            inpmessage$ = "ENTER PF-1 to Acknowledge and Continue,"
            blank$      = "  -OR-  RETURN to Select another PO."
            call "ASKUSER" (keyhit1%, " MAX. CHANGE DATE ",              ~
                            errormsg$, inpmessage$, blank$)
            errormsg$, inpmessage$, blank$ = " "

            if keyhit1% = 1% then return

                return clear all
                goto inputmode


        REM *************************************************************~
            *          T O O   M A N Y   D O L L A R S                  *~
            * --------------------------------------------------------- *~
            * Notify User that Maximum Dollar Liimt has been exceeded.  *~
            * Data Save is aborted until situation is remedied.         *~
            *************************************************************
        toomanydollars
            keyhit1%  = 2%
            errormsg$ = "The Maximum Monetary Limit has been exceeded."
            inpmessage$ = "This condition must be corrected before the" &~
                          " PO can be saved."
            blank$ = "Limit:######,###.00  PO Amount:######,###.00"  &   ~
                     "   Press RETURN to continue."
            convert maxdlrs to str(blank$,  7, 14), pic(######,###.00)
            convert totalpo to str(blank$, 32, 14), pic(######,###.00)
            call "ASKUSER" (keyhit1%, " PO MONETARY LIMIT  ",            ~
                            errormsg$, inpmessage$, blank$)
            errormsg$, inpmessage$, blank$ = " "

            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for page one of PO Header.                      *~
            *************************************************************

            deffn_151
                  errormsg$ = " "
                  on fieldnr% goto  L50092,         /* VENDOR CODE      */~
                                    L50140,         /* Buy From ID      */~
                                    L50245,         /* PO Number        */~
                                    L50400,         /* Name and Address */~
                                    L50410,         /* Vendor Type Code */~
                                    L50425,         /* Contact's Name   */~
                                    L50440,         /* Default Store    */~
                                    L50505,         /* Revision Level   */~
                                    L50565,         /* Revision Date    */~
                                    L50600          /* Buyer Code       */
                     return

L50092
*        Test Data for VENDOR CODE
            if vencode$ = " " and ponumber$ <> " " then L50268
L50095:         vendescr$ = hex(06) & "Select Vendor Code."
                call "GETCODE" (#3, vencode$, vendescr$, 0%, 1, f1%(3))
                if f1%(3) = 1% then L50102 /* VENDOR file */
                     if vencode$ <> " " then L50100
                     return clear all  :  goto inputmode
L50100:                  errormsg$ = "Vendor Not On File: " & vencode$
                         return
L50102:         gosub get_vendor_stuff
                if ponumber$ <> " " then L50245
                return

L50140
*        Test Data for Buy From ID
            f1%(14) = 0% : bfdescr$ = " "
                if bfcode$ <> " " then L50180
                    if edit% <> -1% and edit% <> 2% then L50178
                       get #3 using L50176, venaddr$(), vencont$, tempp$
L50176:                  FMT XX(39), 6*CH(30), POS(220), CH(20), CH(10)
                       gosub format_phone
L50178:             return

L50180:         bfdescr$ = hex(06) & "Buy From for Vendor " & vencode$
                readkey$  = str(vencode$) & bfcode$
                call "PLOWCODE" (#14, readkey$, bfdescr$, 9%, .3,f1%(14))
                if f1%(14) = 1% then L50210
                     errormsg$ = "Invalid Buy From ID."
                     return
L50210:         bfcode$ = str(readkey$,10,6)
                get #14 using L50220, expacct$
L50220:              FMT XX(318), CH(9)
                if edit% <> 2% and edit% <> -1% then L50235
                  get #14 using L50230, venaddr$(), vencont$, tempp$
L50230:              FMT XX(45), 6*CH(30), POS(226), CH(20), CH(10)
                  gosub format_phone
L50235:         return

L50245
*        Test Data for PURCHASE ORDER NUMBER
            if ponumber$ <> " " then goto L50268
L50255:         if poassgn$ = "m" then                                   ~
                             errormsg$ = "P.O. Number Cannot Be Blank"
                return

L50268:     if ponumber$ <> "?" then L50305
               ponumber$ = " "
                  gosub pomstr
                     if vencode$ = " " then L50092   /* NO VENDOR YET */
                     if ponumber$ = " " then L50255  /* NO SELECTION  */
                        goto L50092    /* REVALIDATE VENDOR & DROP THRU */

L50305:     if poflagged% = 1% then return  /* Already been thru this */
              gosub check_history
              if errormsg$ <> " " then return
                gosub L30000   /* Attempt to load if PO is on file */
                if errormsg$ <> " " then return
                if vencode$   = " " then L50095     /* Load Vendor */
                gosub test_recv
                if errormsg$ <> " " then return
                if oldpoonfile% = 0% and fieldnr% < 3% then return
                     init(hex(00)) str(header$())
                          str(header$(), 1, 3) = userid$
                          str(header$(),24,16) = ponumber$
                          str(header$(),15, 9) = vencode$
                     write #6, str(header$(),,1044), eod goto L50365
                     poflagged% = 1%     /* Shows PO is in use    */
                     goto L50375
L50365:                   errormsg$ = "P.O. is Currently Being Updated."
                          return
L50375:         if oldpoonfile% = 0% then return
                    return clear all
                    goto editmode

        test_recv
            rcvrinuse$ = str(vencode$,,9) & ponumber$
            call "PLOWALTS" (#26, rcvrinuse$, 2%, 25%, f1%(26))
            if f1%(26) = 1% then gosub L29000 else return
            vencode$ = str(rcvrinuse$,,9)
            ponumber$ = str(rcvrinuse$,10,16)
            errormsg$ = "This P.O. is In Use by PO Receiving - No" &     ~
                           " Access is Allowed at This Time"
            return

L50400
*        Test Data for VENDOR NAME & ADDRESS (& SHIP-TO NAME & ADDRESS)
            return

L50410
*        Test Data for VENDOR TYPE
            gosub describe_vendor_type
            return

L50425
*        Test Data for CONTACT'S NAME
            return

L50440
*        Test Data for DEFAULT STORE NUMBER
            call "GETCODE" (#19, defstr$, defstrdescr$, 1%, 0, f1%(19))
                if f1%(19) = 1% then L50460
                errormsg$ = "Store Not On File: " & defstr$ : return
L50460:     call "READ100" (#2, "DEFAULTS.STORE."& str(defstr$), f1%(2))
                if f1%(2) = 0% then L50480
                get #2 using L50475, strexpacct$, strpuracct$
L50475:         FMT XX(89), 2*CH(9)
L50480:     if strexpacct$ = " " then strexpacct$ = sysacct$(1)
            if strpuracct$ = " " then strpuracct$ = sysacct$(2)
            return

L50505
*        Test Data for REVISION LEVEL
            if rev_level$ = "+1" then gosub next_revision
            if len(rev_level$) = 1 then rev_level$ = " " & rev_level$
            if rev_level$ = " " then return
                if str(rev_level$,,1) = " " then L50540
                     if str(rev_level$,,1) < "A" or                      ~
                        str(rev_level$,,1) > "Z" then L50550
L50540:         if str(rev_level$,2,1) < "A" or                          ~
                   str(rev_level$,2,1) > "Z" then L50550
                if rev_level$ <> origrev$ then rev_date$ = date$
                return

L50550:         errormsg$ = "Revision Must Be Alpha ' A' TO 'ZZ'."
                return

L50565
*        Test Data for REVISION DATE
            if rev_level$ = " " then rev_date$ = " "
            if rev_level$ = " " then return
                if rev_date$ = " " or rev_date$ = blankdate$ then rev_date$ = date
                call "DATEOK" (rev_date$, err%, errormsg$)
                return

L50600
*        Test Data for BUYER CODE
            return

        get_vendor_stuff
            get #3, using L50640, vendescr$, tempp$, expacct$, ventype$,  ~
                                 cur_vend$
L50640:     FMT XX(9), CH(30), POS(240), CH(10), CH(9), POS(477), CH(4), ~
                POS(528), CH(4)
        format_phone
            phone$ = "(" & str(tempp$,,3) & ") " & str(tempp$,4,3)
            phone$ = phone$ & "-" & str(tempp$,7,4)
        describe_vendor_type
            readkey$, ventypedesc$ = " "
            readkey$ = "VEN TYPES" & str(ventype$)
            call "READ100" (#28, readkey$, f1%(28))
            if f1%(28) = 1% then get #28 using L50666, ventypedesc$
L50666:         FMT XX(24), CH(30)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for page two of PO Header.                      *~
            *************************************************************

            deffn_152
                  errormsg$ = " "
                  on fieldnr% goto  L51210,         /* Liability Acct   */~
                                    L51280,         /* Taxable?         */~
                                    L51330,         /* Frt Terms        */~
                                    L51380,         /* FOB              */~
                                    L51410,         /* Ship Via         */~
                                    L51440,         /* Order Date       */~
                                    L51480,         /* Default Due Date */~
                                    L51520,         /* Cancellation Date*/~
                                    L51560,         /* Confirmed?       */~
                                    L51610,         /* Max Dollar Amt   */~
                                    L51690,         /* Max Date Amt     */~
                                    L51740          /* Copy Text Name   */
                     return

L51210
*        Test Data for LIABILITY ACCOUNT
            call "GETCODE" (#16, puracct$, puracctdescr$, 1%, 0, f1%(16))
            if f1%(16)=0 then                                            ~
                errormsg$ = "Purchases Liability Account Not On File: "& ~
                            puracct$
                return

L51280
*        Test Data for TAXABLE?
            if pos("YN" = taxflag$) = 0% then                            ~
                                errormsg$ = "Answer 'Y' or 'N' Please."
            return

L51330
*        Test Data for FREIGHT TERMS
            if pos("ANC " = frgtflag$) = 0% then                         ~
                            errormsg$="Answer 'N', 'A' or 'C' Please."
*          RETURN

L51380
*        Test Data for F.O.B.
*          RETURN

L51410
*        Test Data for SHIP VIA
            return

L51440
*        Test Data for ORDER DATE
            call "DATEOK" (orddate$, err%, errormsg$)
            if errormsg$ <> " " then return
            rev_order_seed$ = orddate$
            call "DATREVRS" (rev_order_seed$, rev_order_date$, errormsg$)
            return

L51480
*        Test Data for DEFAULT DUE DATE
            if duedate$ = " " or duedate$ = blankdate$ then return
            call "DATEOK" (duedate$, err%, errormsg$)
            return

L51520
*        Test Data for CANCELLATION DATE
            if candate$ = " " or candate$ = blankdate$ then return
            call "DATEOK" (candate$, err%, errormsg$)
            return

L51560
*        Test Data for CONFIRMED? (Y/N)
            if confirm$ = "Y" or confirm$ = "N" then return
                errormsg$ = "Answer 'Y' or 'N' Please"
                return

L51610
*        Test Data for MAXIMUM DOLLAR AMOUNT
            if maxdlrs$ = " " then return
                call "NUMTEST" (maxdlrs$, 0, 9e7, errormsg$, 2.2, 0)
                return

L51690
*        Test Data for MAXIMUM DATE
            if maxdate$ <> " " and maxdate$ <> blankdate$ then ~
                                call "DATEOK" (maxdate$, err%, errormsg$)
            return

L51740:
*        Test Data for COPY ELEMENT NAME
            copytextdescr$() = " " : init(hex(ff)) copytextid$()
          for n% = 1% to 3%
            if copytextname$(n%) = " " then L51862
                temp$ = "1st2nd3rd"
                copytextdescr$(n%) = hex(06) & str(temp$,((n%*3%)-2%),3%)~
                                                   & " Boiler Plate Text"
                plowkey$ = "C" & copytextname$(n%)
                call "PLOWCODE" (#20, plowkey$, copytextdescr$(n%), 1%,  ~
                                                           0.3, f1%(20%))
                if f1%(20%) = 1% then L51850
                     errormsg$ = "Invalid Text Name: " & copytextname$(n%)
                     return
L51850:         copytextname$(n%) = str(plowkey$,2%,10%)
                get #20 using L51930, copytextdescr$(n%), copytextid$(n%)
L51862:     next n%
            return

        describe_copy_name
            for n% = 1% to 3%
                plowkey$ = "C" & copytextname$(n%)
                copytextdescr$(n%) = " " : copytextid$(n%) = all(hex(ff))
                if copytextname$(n%) = " " then L51940
                call "READ100" (#20, plowkey$, f1%(20%))
                if f1%(20%) = 0% then L51940
                     get #20 using L51930, copytextdescr$(n%),            ~
                                          copytextid$(n%)
L51930:                   FMT XX(11), CH(30), CH(4)
L51940:         next n%
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test Data for Items on Page 3 (1st line item screen).     *~
            *************************************************************

            deffn_153
                  errormsg$ = " "
                  on fieldnr% goto  L53096,         /* PART NUMBER      */~
                                    L53176,         /* PART DESCRIPTION */~
                                    L53196,         /* STORE NUMBER     */~
                                    L53224,         /* LOT CODE         */~
                                    L53272,         /* PART CATEGORY    */~
                                    L53304,         /* VEN PART NUMBER  */~
                                    L53316,         /* UNIT OF ISSUE    */~
                                                   /* QUANTITY PER UNIT*/~
                                    L54362,         /* Purchase Contract*/~
                                    L54600,         /* Job/Project Code */~
                                    L53355,         /* Trans. Currency  */~
                                    L53392,         /* ORIGINAL ORDER   */~
                                    L53680,         /* ON ORDER         */~
                                    L53816,         /* RECVD TO DATE    */~
                                    L53436,         /* PURCHASE PRICE   */~
                                    L53541,         /* EXTENSION        */~
                                    L53088,         /* Inventory Cost   */~
                                    L54325          /* Line Status      */
L53088:              return

L53096
*        Test for PART NUMBER
            f1%(8) = 0% : stkg$(c%), stkg_descr$, acct$, vpc_potyp$ = " "
            if part$(c%,1%) = " " then return      /* Comment Line     */

            if str(part$(c%,1%),,9%) <> "ACTIVITY:" then L53112
                 partdescr$(c%) = "SERVICE ACTIVITY"
                 vpc_potyp$ = "A"
                 return

L53112:         partdescr$(c%) = hex(06) & "PF-16 for Non Stock Item"
L53116:         call "GETCODE" (#8, part$(c%,1%), partdescr$(c%), 0%, 0, ~
                                                                 f1%(8))
                if f1%(8) = 1% then L53144
                     call "HNYGREF" (part$(c%,1%), #12, #8, f1%(8))
                     if f1%(8) = 1% then L53116
                        partdescr$(c%) = "NON STOCKED PART"
                        vpc_potyp$ = "M"
                        return
L53144:         get #8, using L53147, partdescr$(c%), stkg$(c%),          ~
                                     serial_no$, pj_lead$, type$(c%),    ~
                                     costtype$(c%)
L53147:              FMT POS(26), CH(32), POS(74), CH(4), POS(131),      ~
                          CH(1), POS(170), CH(10), CH(3), POS(307), CH(1)
                gosub std_cost_warning
                gosub purchase_job_check
                pj_lead% = 0%
                if pj_lead$ <> " " then                                  ~
                     convert pj_lead$ to pj_lead%, data goto L53156

L53156:         readkey$ = "UOM      " & stkg$(c%)
                call "DESCRIBE" (#28, readkey$, stkg_descr$, 0%, f1%(28))

                if type$(c%) <= "000" then                               ~
                    errormsg$="Build To Option Part, Cannot Be Purchased"
                if str(part$(c%,1%),,9%) = "ACTIVITY:"                   ~
                   then vpc_potyp$ = "A" else vpc_potyp$ = "P"
                return

L53176
*        Test for PART DESCRIPTION
            if part$(c%,1%) <> " " or partdescr$(c%) <> " " then return
                errormsg$ = "Part Description May Not Be Blank."
                return

L53196
*        Test for STORE NUMBER
            strdescr$ = " "
            if part$(c%,1%) = " " then return
                call "GETCODE" (#19,str$(c%),strdescr$,1%,0,f1%(19))
                if f1%(19) = 0% then errormsg$ = "Invalid Store Code."
                return

L53224
*        Test for LOT CODE
            if part$(c%,1%) = " " then return
            if lot$(c%) <> " " then                                      ~
                call "LOTVALID" (part$(c%,1), str$(c%), lot$(c%),        ~
                                                   #2, #8, #9, errormsg$)
                if errormsg$ <> " " then return
                call "LOTUNQUE" (part$(), lot$(), c%, #2, errormsg$)
                if errormsg$ <> " " then return
            acct% = 3%
            call "HNYGLGET" (part$(c%,1%), str$(c%), lot$(c%), acct$,    ~
                                                        acct%, #8, #9)
            if acct% > 0% then acct$ = expacct$
            return

L53272
*        Test for PART CATEGORY
           catdescr$ = " "
            if part$(c%,1%) = " " then return
            if cat$ (c%) = " " then return
                call "GETCODE" (#15, cat$(c%), catdescr$, 1%, 0, f1%(15))
                if f1%(15)=0 then errormsg$ = "Category Code Not on File"
                return

L53304
*        Test for VEN PART NUMBER
            return

L53316
*        Test for UNIT OF ISSUE
            if part$(c%,1%) = " " then return
            if uom% = 0% then L53338
            plowkey$ = "UOM      " & meas$(c%)
            selectmsg$ = hex(06) & "Unit of Measure"
            call "PLOWCODE" (#28, plowkey$, selectmsg$, 9%, 0.30, f1%(28))
            if f1%(28) = 1% then L53332
                errormsg$ = "Invalid Unit of Measure Code."  :  return
L53332:     meas$(c%) = str(plowkey$,10,4)
*          RETURN

L53338
*        Test for QUANTITY PER UNIT
*          IF PART$(C%,1%) = " " THEN RETURN
            if factor$  <> " " then L53346
               temp = factor(c%) : goto L53350
L53346:     call "NUMTEST" (factor$, 0.0001, 1000000, errormsg$, -0.4,   ~
                                                             factor(c%))
L53350:     if errormsg$ = " " then gosub deffn_041
            return

L53355
*        Test for transaction Currency
            temp$ = hex(0684) & "Select Currency for Purchase Order."
        test_currency   /* Can get here from loading directives... */
            if cur_loaded% = 1% then L53384
            if cur_tran$ = " " then cur_tran$ = cur_stat$
L53360:     if cur_tran$ <> cur_stat$ then L53363
               cur_factor1, cur_factor2 = 1
               cur_descr$(), cur_eff_date$ = " ":cur_loaded% = 0%:return
L53363:     call "GETCODE" (#37, cur_tran$, temp$, 0%, 0, f1%(37))
            if cur_tran$ = cur_stat$ then L53360
            plowkey$ = cur_po_table$ & str(cur_tran$) & rev_order_date$
            call "PLOWNEXT" (#31, plowkey$, 5%, f1%(31))
               if f1%(31) <> 0% then L53381
                  errormsg$ = "No Exchange Rates on file for " & cur_tran$
                  return
L53381:     get #31 using L53382, cur_eff_date$, cur_factor1, cur_factor2
L53382:         FMT POS(12), CH(6), 2*PD(14,7)
            cur_loaded% = 1%
L53384:     if ourprice$ <> " " then gosub deffn_040
            return

L53392
*        Test for ORIGINAL ORDER QTY
            if part$(c%,1%) = " " then return
            gosub L53397   :  if errormsg$ <> " " then return
            gosub vpc_test_qty     /* End With Vendor Contract Testing */
               return
L53397:   /* Start Order Quantity Test */
            if venorig$  = " " then L53620
            convert venorig$ to temp, data goto L53424
            temp = round(temp, 7)
            if abs(temp - venorig) < .0000001 then L53620
               temp = round(temp*factor(c%), 4)
               goto L53632
L53424:     errormsg$ = "Invalid Entry for Vendor's Order Quantity"
            if errormsg$ = " " then gosub deffn_040
            return

L53436
*        Test for PURCHASE PRICE
            if part$(c%,1%) = " " then return
            savprice = price(c%)
            temp = 0
            if venprice$ = "NO CHARGE" then venprice$ = "NC"
            if ourprice$ = "NO CHARGE" then ourprice$ = "NC"
            if ourprice$ = " " and venprice$ = "NC" then ourprice$ = "NC"
            if venprice$ = "NC" then L53468
            convert venprice$ to temp, data goto L53480
L53468:     temp = round(temp, 4)
            if abs(temp - venprice) < .0001 then L53512
               if temp >= 0 then L53492
L53480:              errormsg$ = "Invalid Entry for Vendor's Price"
                     return

L53492:     if temp/factor(c%) >= 1e7 then L53496
               temp = round(temp/factor(c%), 7) : goto L53499
L53496:           errormsg$ = "Vendor Price would make Our Internal " &  ~
                              "Price TOO Large ( > 10,000,000)." : return
L53499:     price(c%) = temp
            goto L53566

L53512:     temp = 0 : if ourprice$ = "NC" then L53524
            convert ourprice$ to temp, data goto L53532
            if temp < 1e7 then L53524
               errormsg$ = "Our Internal Price MUST be Less Than " &     ~
                           "10,000,000." : return
L53524:     temp = round(temp, 7)
            if abs(temp - price(c%)) < .0000001 then L53566
               if temp >= 0 then L53499
L53532:              errormsg$ = "Invalid Entry for Internal Price"
                     return

L53541: REM TEST FOR EXTENSION
            if part$(c%,1%)=" " then return
            gosub L53546   :  if errormsg$ <> " " then return
            gosub vpc_test_dollars  /* End With Vendor Contract Testing */
               return
L53546:   /* Start Extension Testing */
            temp = 0
            savprice = price(c%)
            if extg$(c%) = "NO CHARGE" then extg$(c%) = "NC"
            if extg$(c%) = "NC" then L53558
            convert extg$(c%) to temp, data goto L53584
L53558:     temp = round(temp, 2)
            if temp < 0 then L53584
            if abs(qtyorig(c%)) < .0001 then L53566
               price(c%) = round(temp/qtyorig(c%), 7)
L53566:     gosub deffn_040
            if chg% <> 2% then return
            if savprice = price(c%) then return
              if round(savprice*cur_factor1,4) <> hnycost(c%) then return
              if postatstdcost$ = "Y" and setid$ <> " " and              ~
                 pos("FST" = costtype$(c%)) > 0 then return
              temp = price(c%)
              if cur_loaded% <> 0% then temp = temp * cur_factor1
              call "CONVERT" (temp, 4.4, hnycost$)
              temp$ = "I"
              gosub vsa_bucket : if vsa_bucket_chg% = 1% then L53581
              gosub call_hnycdist : vsa_bucket_chg% = 0%
L53581:     return


L53584:     errormsg$ = "Invalid Entry for Extension"
            return

L53604:     qtyorig(c%) = temp
            if edit% = -1% then qtyonord(c%) = temp
L53608:     gosub deffn_040
            return

L53620:     if qtyorig$ = " " then qtyorig$ = "0"
            convert qtyorig$ to temp, data goto L53648
            temp = round(temp, 4)
L53632:     if abs(temp - qtyorig(c%)) < .0001 then L53608
               if ((qtyonord(c%) + qtyrecd(c%)) -                        ~
                                 (temp * overshippct)) < .0001 then L53604
                  goto L53660
L53648:     errormsg$ = "Invalid Entry for Internal Order Quantity"
            return

L53660:     temp1 = round((qtyonord(c%)+qtyrecd(c%))/overshippct,4)

            errormsg$ = osmsg$ & " MIN. ORDER IS"
            goto L54280

L53680
*        Test for REMAINING ON ORDER (Open Qty)
            if part$(c%,1%) = " " then return
            if venonord$ = " " then L53744
            convert venonord$ to temp, data goto L53716
            temp = round(temp, 7)
            if abs(temp - venonord) < .0000001 then L53744
               if temp < 0 then L53716
               temp = round(temp*factor(c%), 4)
               goto L53756
L53716:     errormsg$ = "Invalid Entry for Vendor's Open Quantity"
            return

L53728:     qtyonord(c%) = temp
L53732:     gosub deffn_040
            goto L54220

L53744:     if qtyonord$ = " " then qtyonord$ = "0"
            convert qtyonord$ to temp, data goto L53784
            temp = round(temp, 4)
L53756:     if abs(temp - qtyonord(c%)) < .0001 then L53732
               overwarn% = 0%
               if abs(temp) < .0001 then L53728
               if temp < 0 then L53784
               if ((temp + qtyrecd(c%)) -                                ~
                          (qtyorig(c%) * overshippct)) < .0001 then L53728
               goto L53796
L53784:     errormsg$ = "Invalid Entry for Internal Order Quantity"
            return

L53796:     temp1 = qtyorig(c%) * overshippct
            temp1 = round(temp1 - qtyrecd(c%), 4)
            errormsg$ = osmsg$ & " MAX. ON ORDER IS"
            goto L54280

L53816
*        Test for RECEIVED TO DATE
            if part$(c%,1%) = " " then return
            if venrecd$  = " " then L54075
            convert venrecd$ to temp, data goto L54030
            temp = round(temp, 7)
            if abs(temp - venrecd) < .0000001 then L54075
               temp = round(temp*factor(c%), 4)
               goto L54090
L54030:     errormsg$ = "Invalid Entry for Vendor Received Quantity"
            return

L54045:     qtyonord(c%) = max(0,round((qtyonord(c%)+qtyrecd(c%)-temp),4))
            qtyrecd(c%) = temp
L54055:     gosub deffn_040
            gosub L54155
            goto L54220

L54075:     if qtyrecd$ = " " then qtyrecd$ = "0"
            convert qtyrecd$ to temp, data goto L54120
            temp = round(temp, 4)
L54090:     if abs(temp - qtyrecd(c%)) < .0001 then L54055
               overwarn% = 0%
               if abs(temp) < .0001 then L54045
               if temp < 0 then L54120
               if (temp  - (qtyorig(c%) * overshippct)) < .0001 then L54045
               goto L54135
L54120:     errormsg$ = "Invalid Entry for Received Quantity"
            return

L54135:     temp1 = round(qtyorig(c%) * overshippct, 4)
            errormsg$ = osmsg$ & " MAX. RECEIPT IS"
            goto L54280

L54155: REM SET WARNING MESSAGE FOR OVERRECEIPT
            if overwarn% = 0% then L54180
               overwarn% = 0%
               return

L54180:     temp1 = round(qtyrecd(c%) - qtyorig(c%), 4)
            if temp1 < .0001 then return
                errormsg$ = "WARNING, This Receipt Indicates an"  &      ~
                            " Overshipment of"
                overwarn% = 1%
                return clear
                goto L54280

L54220: REM SET WARNING ERRORMSG AND PLAY WITH FIELDNR FOR ONORDER
            if overwarn% = 0% then L54245
               overwarn% = 0%
               return

L54245:     temp1= max(0, round(qtyorig(c%)-qtyrecd(c%),4))
            if (qtyonord(c%) - temp1) < .0001 then return
                errormsg$ = "WARNING, Quantity On Order Exceeds Derived"&~
                            " Quantity of"
                overwarn% = 1%
                fieldnr% = 10%

L54280: REM SET ERRORMSGS INVOLVING NUMBERS
            temp2 = round(temp1/factor(c%), 7)
            call "CONVERT" (temp2, -2.7,                                 ~
                                     str(errormsg$,len(errormsg$)+2%,10))
            errormsg$ = errormsg$ & "/"
            call "CONVERT" (temp1, -0.7,                                 ~
                                     str(errormsg$,len(errormsg$)+1%,10))
            return

L54325
*        Test for LINE STATUS
            if part$(c%,1%) = " " then return
            if pos("ACHW" = status$(c%)) <> 0% then L54350
                errormsg$ = "Invalid entry for Line Item Status."
                return
L54350:     gosub describe_status
            return

L54362
*        Test for Purchase Contract ID
            if contract_id$(c%) = " " then return
            if contract_id$(c%) = "?" then contract_id$(c%) = " "
            if vpc_potyp$ <> "P" then L54378
                call "VPCPIKME" (vencode$, contract_id$(c%),             ~
                       contract_line$(c%), "P"  , part$(c%,1%), " ",     ~
                       #42, #3, ret%)
                goto L54396                   /* Get VPC DATA */
L54378:     if vpc_potyp$ <> "M" then L54388
                call "VPCPIKME" (vencode$, contract_id$(c%),             ~
                       contract_line$(c%), "M"  , " ", " ", #42, #3, ret%)
                goto L54396                   /* Get VPC DATA */

L54388:     call "VPCPIKME" (vencode$, contract_id$(c%),                 ~
                       contract_line$(c%), "A"  , " "         , " ",     ~
                       #42, #3, ret%)
                goto L54396                   /* Get VPC DATA */
L54396:     if ret% <> 0% then L54402
L54398:         contract_id$(c%),contract_line$(c%),contract_descr$ = " "
                errormsg$ = "Vendor Contract Not On File"
                return
L54402:     str(readkey$,   ,16%) = contract_id$(c%)
            str(readkey$,17%, 4%) = contract_line$(c%)

            call "READ100" (#42, readkey$,  f1%(42%))
            if f1%(42%) = 0% then L54398   /* Shouldn't Happen */
            get #42 using L54416, vpc_vendor$, vpc_type$, vpc_code$,      ~
                                 vpc_stkuom$, vpc_price
L54416:     FMT CH(9), POS(60), CH(1), CH(25), POS(98), CH(4), POS(118), ~
                 PD(14,7)
            gosub check_contract_for_conflicts
                if errormsg$ <> " " then return
            gosub set_vbk_pay_sums

            gosub set_vpc_index
            gosub check_and_set_vendor_price
            gosub display_contract_warning
            return

            vpc_lin% = vpc_lin% : vpc_min_dlr = vpc_min_dlr : lin_min_dlr~
             = lin_min_dlr : lin_min_qty = lin_min_qty  /* Junk Line */
        vpc_test_qty
            vpc_warn$ = " "
            if contract_id$(c%) = " " then return
            gosub get_contract_net_change

            if lin_max_qty > lin_vbk_qty+lin_pay_qty+netchg_qty then L54506
                vpc_warn$ = "PO Quantity is Greater than Contract Maximum"
                gosub display_contract_warning   :  return
L54506:     return

        vpc_test_dollars
            vpc_warn$ = " "
            if contract_id$(c%) = " " then return
            gosub get_contract_net_change
          /* Test Header Level */
            if vpc_hdr% = 0% then L54528
            if vpc_max_dlr > vpc_vbk_dlr + vpc_pay_dlr + netchg_dlr      ~
                                                                then L54528
                vpc_warn$ = "PO Dollars are Greater than Contract Maximum"
                gosub display_contract_warning   :  return
L54528:    /* Test Line Level */
            if lin_max_dlr > lin_vbk_dlr + lin_pay_dlr + netchg_dlr      ~
                                                                then L54544
                vpc_warn$ = "PO Dollars are Greater than Contract Maximum"
                gosub display_contract_warning   :  return
L54544:     return

        vpc_test_dates
            vpc_warn$ = " "
            if contract_id$(c%) = " " then return
          /* Test Header Level */
            if vpc_hdr% = 0% then L54564
            if vpc_start$ < tempdate$  then L54558
                vpc_warn$ = "PO Date Before Contract Start"
                gosub display_contract_warning   :  return
L54558:     if vpc_end$   > tempdate$  then L54564
                vpc_warn$ = "PO Date is After Contract End"
                gosub display_contract_warning   :  return
L54564:    /* Test Line Level */
            if lin_start$ < tempdate$  then L54574
                vpc_warn$ = "PO Date is Before Contract Start"
                gosub display_contract_warning   :  return
L54574:     if lin_end$   > tempdate$  then L54580
                vpc_warn$ = "PO Date is After Contract End"
                gosub display_contract_warning   :  return
L54580:     return

        get_contract_net_change
            netchg_dlr, netchg_qty = 0
            for i% = 1% to vpcs%
                if vpc_used$(i%) <> contract_id$(c%) & contract_line$(c%)~
                                                              then  L54588
                    j% = i% :  goto L54589
L54588:     next i%
L54589:     for i% = 1% to vpc_use_idx%(j%, 101%)
                idx%    = vpc_use_idx%(j%, i%)
                tempext = 0
                convert extg$(idx%) to tempext,  data goto L54594

L54594:         netchg_dlr = netchg_dlr + tempext       - ext_loaded(idx%)
                netchg_qty = netchg_qty + qtyorig(idx%) - qty_loaded(idx%)
            next i%
            return

L54600
*        Test for JOB/PROJ CODE
            if part$(c%,1%) = " " then return
            call "READ100" (#23, part$(c%,1%), f1%(23%))
               if f1%(23%) <> 0% then L55829
            if job$(c%) = " " then return
               call "DESCRIBE" (#18, job$(c%), jobdescr$, 1%, f1%(18%))
                     if f1%(18%) <> 0% then L55800
               call "DESCRIBE" (#17, job$(c%), jobdescr$, 1%, f1%(17%))
                     if f1%(17%) <> 0% then L55810
               init (" ")  hdr$(), ie$()  :  mat ie = zer
               selectmsg$ = hex(06) & "Select OPEN Production Job or PF16~
        ~ To See Projects"
               ie(1%) = 153.060
               call "PLOWCODE" (#18, job$(c%), selectmsg$, 5000%, 0.30,  ~
                                f1%(18%), hdr$(), 0, 0, ie(), ie$())
                     if f1%(18%) <> 0% then L55800
               selectmsg$ = hex(06) & "Select OPEN Project or Job"
               ie(1%) = 45.060
               call "PLOWCODE" (#17, job$(c%), selectmsg$, 5000%, 0.30,  ~
                                f1%(17%), hdr$(), 0, 0, ie(), ie$())
                     if f1%(17%) <> 0% then L55810
               errormsg$ = "Job/Project Not on File"
               return

        check_contract_for_conflicts
            if vencode$ = vpc_vendor$ then L54735
                errormsg$ = "Vendor does Not match Contract Vendor"
                return
L54735:     if vpc_type$ = "H" then return       /* All Headers are OK */
            if vpc_type$ = "M" then return       /* All Misc are OK */

            if vpc_potyp$ = "A" then L54819       /* Service Contract */
            if vpc_potyp$ = "M" then L54787       /* Non Stocked Part */

           /* Test Stocked Part */
            if part$(c%,1%) = vpc_code$ then L54767
                errormsg$ = "Contract is Not for this Part Number" :return
L54767:     if vpc_type$ <> "A" then L54779
L54771:         errormsg$ = "Service Contract can Not be Assigned to Part"
                return
L54779:     /* Part matches a 'P' Contract or Contract is a 'M' Type */
            goto L54851   /* Test for Warning Status */
L54787:   /* Test NonStocked Part */
            if vpc_type$ =  "A" then L54771  /* Re-use Error Msg */
            if vpc_type$ <> "P" then L54807
                errormsg$ = "Non Stocked Part can Not be Assigned to a Pa~
        ~rt" : return
L54807:     /* Non Stocked Part matches a 'M' Contract  */
            goto L54851   /* Test for Warning Status */

L54819:   /*Test Activity*/
            if vpc_type$ <> "P" then return
                errormsg$ = "Service Activity can Not be Assigned to a Pa~
        ~rt Contract" : goto L54851
*          IF PART$(C%,1%) = VPC_CODE$ THEN 54851
*              ERRORMSG$ = "Service Activity Contract does Not match PO ~
*       Activity" : RETURN

L54851:   /* Test for Warning Status */
            if stkg$(c%) = vpc_stkuom$ then L54863
                vpc_warn$ = "Unit of Measure Do Not Match"
L54863:     return

        display_contract_warning
            if vpc_warn$ = " " then return
            ask% = 2%
            call "ASKUSER" (ask%, " *** PURCHASE CONTRACT WARNING *** ", ~
                     "The VPC & PO Line conflict in the Following Way: ",~
                     vpc_warn$, "Press RETURN to Acknowledge Warning" )

            return

        check_and_set_vendor_price
            if vpc_potyp$ <> "P"      then return
            if vpc_price  = 0         then return
            if vpc_price  = price(c%) then return
            call "CONVERT" (vpc_price, 2.4, temp1$)
            if price(c%)  = 0 then L54932
            call "CONVERT" (price(c%), 2.4, temp2$)
L54916:     ask% = 0%
            call "ASKUSER" (ask%, " **** CONTRACT PRICE CONFLICT **** ", ~
               "To Accept Contract Price of " & temp1$ & " Press RETURN",~
               "To Accept Default Price of " &  temp2$ & " Press PF16  ",~
                                                            " ")
            if ask% = 0% then L54932
            if ask% <> 16% then L54916       /* Try Again */
                return                  /* Accept Vendor Price Default */
L54932:     price(c%), venprce = vpc_price
            factor(c%) = 1
            gosub deffn_040

            return

        set_vbk_pay_sums
            call "VPCSUMSB"              /* SUMMARIZE VPC VBK/PAY INFO */~
                   ( #42,                /* VPCMASTR channel           */~
                     #5,                 /* VBKLINES channel           */~
                     #43,                /* PAYLINES channel           */~
                     contract_id$(c%),   /* Contract to summarized     */~
                     contract_line$(c%), /* Line to be  summarized     */~
                                         /*   (" " => hdr level)       */~
                                         /*                            */~
                                         /* *** HEADER LEVEL INFO ***  */~
                     vpc_hdr%,           /*  HDR Exists? 0=No, 1=Yes   */~
                     vpc_min_dlr,        /*  Minimum Dollars           */~
                     vpc_max_dlr,        /*  Maximum Dollars           */~
                     vpc_start$,         /*  Start Date                */~
                     vpc_end$,           /*  End   Date                */~
                     vpc_vbk_dlr,        /*  PO  Dollars               */~
                     vpc_pay_dlr,        /*  A/P Dollars               */~
                                         /*                            */~
                                         /* **** LINE LEVEL INFO ****  */~
                     vpc_lin%,           /*  Line Exists? 0=No, 1=Yes  */~
                     lin_min_dlr,        /*  Minimum Dollars           */~
                     lin_max_dlr,        /*  Maximum Dollars           */~
                     lin_min_qty,        /*  Minimum Qty               */~
                     lin_max_qty,        /*  Maximum Qty               */~
                     lin_start$,         /*  Start Date                */~
                     lin_end$,           /*  End   Date                */~
                     lin_vbk_qty,        /*  PO  Quantity              */~
                     lin_vbk_dlr,        /*  PO  Dollars               */~
                     lin_pay_qty,        /*  A/P Quantity              */~
                     lin_pay_dlr )       /*  A/P Dollars               */

                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for second line item screen.                    *~
            *************************************************************

            deffn_154
                  errormsg$ = " "
                  on fieldnr% goto  L55230,         /* LINE DUE DATE    */~
                                    L55580,         /* Orig Due Date    */~
                                    L55630,         /* NEXT SHIPMENT    */~
                                    L55690,         /* LAST RECIVED DATE*/~
                                    L55750,         /* PACK SLIP        */~
                                    L55910,         /* EXP/ASSET ACCT   */~
                                    L55964,         /* PRICE/COST ACCT  */~
                                    L55980,         /* Revision Level   */~
                                    L56100,         /* Deliver To       */~
                                    L56130,         /* Requisitioner    */~
                                    L56160,         /* Internal Ref#    */~
                                    L56190,         /* Buyer Code       */~
                                    L56220          /* Kit Complete, etc*/
                     return

L55230: REM TEST FOR LINE DUE DATE
            if part$(c%,1%)=" " then return
            gosub L55245   :  if errormsg$ <> " " then return
            gosub vpc_test_dates   /* End With Vendor Contract Testing */
               return
L55245:   /* Start Due Date Test */
            call "DATEOK" (duedate$(c%), err%, errormsg$)
                if errormsg$ <> " " then errormsg$ = "DUE DATE: " &      ~
                                                               errormsg$
                if errormsg$ <> " " then return
            tempdate$ = duedate$(c%)
            call "DATUNFMT" (tempdate$)
            call "PIPINDEX" (#2, tempdate$, u3%, err%) : u3% = u3%
            if err% <> 0% then                                           ~
                     errormsg$ = "DUE DATE Not Within Planning Calendar"
            if errormsg$ <> " " then return
            if notbefore$(c%) = " " or ~
                notbefore$(c%) = blankdate$ then L55390
                call "DATEOK" (notbefore$(c%), err%, errormsg$)
                if errormsg$ = " " then L55390
                     errormsg$ = "NOT BEFORE: " & errormsg$ : return
L55390:     if notafter$(c%) = " " or ~
                notafter$(c%) = blankdate$ then L55440
                call "DATEOK" (notafter$(c%), err%, errormsg$)
                if errormsg$ = " " then L55440
                     errormsg$ = "NOT AFTER: " & errormsg$
                     return
L55440:     if (notbefore$(c%) = " " or notbefore$(c%) = blankdate$) and ~
                (notafter$(c%) = " " or notafter$(c%) = blankdate$) then return
                call "DATUNFMT" (notbefore$(c%))
                call "DATUNFMT" (notafter$ (c%))
                call "DATUNFMT" (duedate$  (c%))
                if notafter$(c%) = " " or notafter$(c%) = blankdate$ then ~
                    notafter$(c%) = unfmaxdt$
                if notbefore$(c%) <= duedate$ (c%) and                   ~
                   duedate$  (c%) <= notafter$(c%) then L55520
                     errormsg$ = "Receiving Dates Out of Order."
L55520:         if notafter$(c%) = unfmaxdt$ then notafter$(c%) = blankdate$
                call "DATEFMT" (notbefore$(c%))
                call "DATEFMT" (notafter$ (c%))
                call "DATEFMT" (duedate$  (c%))
                return

L55580
*        Test for ORIGINAL DUE DATE
            if part$(c%,1%) = " " then return
            call "DATEOK" (origdue$(c%), err%, errormsg$)
            return

L55630
*        Test for NEXT SHIPMENT
            if part$(c%,1%) = " " or nextdate$(c%) = " " or ~
               nextdate$(c%) = blankdate$ then return
            call "DATEOK" (nextdate$(c%), err%, errormsg$)
            return

L55690
*        Test for LAST RECIVED DATE
            if part$(c%,1%) = " " or recdate$(c%) = " " or ~
               recdate$(c%) = blankdate$ then return
            call "DATEOK" (recdate$(c%), err%, errormsg$)
            return

L55750
*        Test for PACK SLIP
            return

L55800
*        Check for open production job
            get #18 using L55803, jobdescr$, closeddate$
L55803:         FMT POS(9), CH(30), POS(153), CH(6)
            if closeddate$ = " " or closeddate$ = blankdate$ then return
                errormsg$ = "Production Job is Closed."
                return

L55810:     get #17 using L55811, jobdescr$, closeddate$
L55811:         FMT POS(9), CH(30), POS(45), CH(6)
            if closeddate$ = " " or closeddate$ = blankdate$ then return
                errormsg$ = "Project is Closed."
                return

L55829:     if pj_on% = 1% then L55849
                if job$(c%) = " " then return
                     errormsg$ = "Planned Part, Job/Project Reference Not~
        ~  Allowed."
                     return
L55849:     if job$(c%) <> " " then L55861
                jobdescr$ = " "
                return
L55861:    if job$(c%) <> "PJ      " then L55877
                jobdescr$ = "(Future Purchased Job)"
                return

L55877:         errormsg$ = "Enter 'PJ' for a 'Purchased Job' or leave Bl~
        ~ank."
                return

L55910
*        Test for EXP/ASSET ACCT
            acctdescr$ = " "
            if part$(c%,1%) = " " or enabled% = 0% then return
            call "GETCODE" (#16, acct$(c%), acctdescr$, 1%, 0, f1%(16))
            if f1%(16)=0 then errormsg$ = "Expense Account Not on File"
            return

L55964
*        Test for PC VAR. ACCT
            pcacctdescr$ = " "
            if part$(c%,1%) = " " then return
            call "GETCODE" (#16, pcacct$(c%), pcacctdescr$, 1%,0,f1%(16))
            if f1%(16)=0 then                                            ~
                    errormsg$ = "Price/Cost Variance Account Not on File"
            return

L55980
*        Test for REVISION LEVEL
            if len(linerev$(c%)) =1 then linerev$(c%) = " " & linerev$(c%)
            if linerev$(c%) = " " then return
                if str(linerev$(c%),,1) = " " then L56040
                     if str(linerev$(c%),,1) < "A" or                    ~
                        str(linerev$(c%),,1) > "Z" then L56070
L56040:         if str(linerev$(c%),2,1) < "A" or                        ~
                   str(linerev$(c%),2,1) > "Z" then L56070
            return
L56070:         errormsg$ = "Revision Must Be Alpha ' A' To 'ZZ'."
                return

L56100
*        Test for DELIVER TO
            return

L56130
*        Test for REQUISITIONER
            return

L56160
*        Test for INTERNAL REFERENCE NUMBER
            return

L56190
*        Test for BUYER CODE
            return

L56220
*        Test for KIT COMPLETE and PRINT OPTIONS
            if part$(c%,1%) = " " then return
            if pj_on% <> 1% then return
            if pj_stat%(c%) = 0% then return
            if job$(c%) <> "PJ" then return
                for i% = 1% to 5%
                     if pos("YN " = kit$(c%,i%)) <> 0% then L56300
                     errormsg$ = "Please Enter 'Y', 'N' or Leave Blank."
L56300:         next i%
                if errormsg$ <> " " then return
                kitcomp$  = kit$(c%,1%)
                traveler$ = kit$(c%,2%)
                picklist$ = kit$(c%,3%)
                byprod$   = kit$(c%,4%)
                slbom$    = kit$(c%,5%)
                return

        REM *************************************************************~
            *       AUTOMATIC PO NUMBER GENERATION                      *~
            *************************************************************
        pogenerate
            temp$ = defstr$
            if poassgn$ = "m" or poassgn$ = "s" then L58070
                temp$ = poassgn$
L58070:     call "READ101" (#19, temp$, f1%(19))
            if f1%(19) = 0% then L58380

            get #19 using L58110, oldponum$
L58110:         FMT XX(161), CH(7)
            convert str(oldponum$,2,6) to n%, data goto L58380
L58130:     n% = n% + 1%
            if n% < 1000000% then L58200
                a% = val(str(oldponum$,,1))
                a% = a% + 1%
                if a% > val("Z") then a% = val("A")
                n% = 1%
                str(oldponum$,,1) = bin(a%)
L58200:     convert n% to str(oldponum$,2,6), pic(000000)
            ponumber$ = temp$ & "-" & str(oldponum$,,7)

            gosub check_history
                if errormsg$ <> " " then L58130
            call "REDALT0" (#4, ponumber$, 1%, f1%(4))
            if f1%(4) <> 0 then L58130
            call "REDALT0" (#6, ponumber$, 3%, f1%(6))
            if f1%(6) <> 0 then L58130

            put #19 using L58290, oldponum$
L58290:         FMT POS(162), CH(7)
            rewrite #19
            init(hex(00)) str(header$())
            str(header$(),,3) = userid$ : str(header$(),24,16) = ponumber$
                                          str(header$(),15, 9) = vencode$
            write #6, str(header$(),,1044), eod goto L58130
            poflagged% = 1%
            return

L58380:     errormsg$ = "Problem with Assignment of PO#.  Please Enter."
            return clear all
            fieldnr% = 3%
            goto L11330

        check_history
            errormsg$ = " "
            call "REDALT0" (#32, ponumber$, 1%, f1%(32))
                if f1%(32) = 0% then return
                    errormsg$ = "Purchase Order number " & ponumber$ &   ~
                            " is in the History Files."
                return

L59000: REM *************************************************************~
            *       CALLING CONTROLERS FOR PROCUREMENT HISTORY          *~
            *************************************************************

            prcven$ = vencode$
            call "HNYPRCSB" (prcven$, prcpart$, 0%, #10, #8, #3, #11)
            prcpart$, prcven$=" "
            return

L59100:     REM Called From Input Lines
            if str(part$(c%,1%),,9%) = "ACTIVITY:" then return
            prcpart$=part$(c%,1%)
            goto L59000

L59150:     REM Called from Lines
            call "PIPATCSB" (part$(c%,1%), #23, #8, #24, #33, #21, #34,  ~
                            #35, #36, #25)
            return

L59200: REM *************************************************************~
            *       CALLING CONTROLERS FOR CHANGE ALL LINE DATES        *~
            *************************************************************

            call "DATE" addr( "G+", basedate$, 489%, penddate$, d%)

            inputmsg$ = "To Reset all P.O. Line Due Dates,"
            k% = 0%
            call "ASKDATE" (k%, "CHANGE ALL DUE DATES?", inputmsg$,      ~
                     date, penddate$, newdate$, d%)
            if k% <> 0% then return

            newdatef$ = newdate$
            call "DATEFMT" (newdatef$)
            k% = 2%
            call "ASKUSER" (k%, "ARE YOU SURE?",                         ~
               "Press RETURN to proceed to change all line due dates" &  ~
               " to " & newdatef$ & " -or-",                             ~
               "Press PF 16 to Abort.  Note that lines will not be" ,    ~
               "changed where the new date is outside the 'not before'"& ~
               "/'not after' range.")
            if k% <> 0% then return
            ch%, sk% = 0%

            for z% = 1% to maxlines%
              if notbefore$(z%) = " " or notbefore$(z%) = blankdate$ then L59370
                 tempdate$ = notbefore$(z%)
                 call "DATUNFMT"(tempdate$)
                 if newdate$ < tempdate$ then skip_this_line

L59370:      if notafter$(z%) = " " or notafter$(z%) = blankdate$ then  L59410
                 tempdate$ = notafter$(z%)
                 call "DATUNFMT"(tempdate$)
                 if newdate$ > tempdate$ then skip_this_line

L59410:      duedate$(z%) = newdatef$
             ch% = ch% + 1%
             goto L59440

        skip_this_line
             sk% = sk% + 1%

L59440:      next z%

             convert ch% to temp$(1), pic(####)
             convert sk% to temp$(2), pic(####)
             convert maxlines% to temp$(3), pic(####)
             k% = 2%
             call "ASKUSER" (k%, "RESULTS",                              ~
                        " Lines Changed = " &  temp$(1)  &               ~
                        ", Lines Skipped = " &  temp$(2),                ~
                        "Total Lines Processed = " & temp$(3),           ~
                            "Press any key to Continue.")


             return

        view_vendor_contracts
            vpc_temp$  = contract_id$(c%)
            vpc_temp1$ = contract_line$(c%)
            call "VPCINQSB" (                                            ~
               vpc_temp$,                   /* Contract to Query       */~
               vpc_temp1$,                  /* Contract Line for Query */~
               #42,   /* VPCMASTR Vendor Purchases Contract Master File*/~
               #08,   /* HNYMASTR Inventory Master File                */~
               #28,   /* GENCODES System General Codes file.           */~
               #03,   /* VENDOR   VENDOR MASTER RECORD                 */~
               #20,   /* TXTFILE  System Text File                     */~
               #04,   /* VBKMASTR Purchase Order Master file           */~
               #05,   /* VBKLINES Purchase Order Lines file            */~
               #44,   /* PAYMASTR A/P Invoice Master file              */~
               #43,   /* PAYLINES A/P Invoice Lines file               */~
               #29,   /* RCVLINES Receiver Line Items File             */~
                f1%)                         /* Contract Found Status  */~
                                         /*  1 = Found, 2 = Not        */

             return

        REM *************************************************************~
            *   Release Po Advice Call                                  *~
            *************************************************************
        porel
            call "PORELSUB" (#21, #22, #3, #8, #11, #2, #23, #24, #10,   ~
                                       #12, #25, #13, #28, #42, f2%(22%))
            return

        povrel
            call "POVRELSB" (#2, #22, #3, #37, #28, #42, #11, #8, #10)

            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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

            call "SHOSTAT" ("Releasing Files, One Moment Please")
            end
