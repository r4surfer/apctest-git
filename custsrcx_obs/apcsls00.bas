        REM *************************************************************~
            *  *** Special Hook Area ***  PF(9) Turn on Debug Display   *~
            *                             Line No. = L60770  (RHHTEST)  *~
            *  *** Special Hook Area ***  PF(6) Purge Data for a        *~
            *                           (Specified Date) PURGE_DATE$    *~
            *  Program Name      - APCSLS00                             *~
            *  Creation Date     - 02/06/95                             *~
            *  Last Modified Date- 06/16/00                             *~
            *  Description       - This Program Pulls Data from Invoices*~
            *                      and Sales Orders.                    *~
            *                      (1) Creates a Summary record in      *~
            *                          'APCSLSDT' to Use for Sales      *~
            *                          Analysis Reports and Commission  *~
            *                          Report.                          *~
            *                      (2) Commision Data is Calculated and *~
            *                          Stored in Record. No Longer Used *~
            *                      (3) SLS_ERR$ - Error Code Values     *~
            *                          '1' = Invalid Line Item Part     *~
            *                          '2' = Invalid G/L Acct. Not '36' *~
            *                          '3' = Unable to Update (APCSLSDT)*~
            *                           ?  = Other Costing Error        *~
            *                                                           *~
            *  Code Tables Used  - (SLS CODE0) - Sales Analysis Grouping*~
            *                                    Code for Reports.      *~
            *                    - (SLS CODE1) - Commission Grouping    *~
            *                                    Code for Reports.      *~
            *                    - (SLS CODE2) - Sales Analysis Grouping*~
            *                                    Code Descriptions      *~
            *                    - (SLS CODE3) - Commission Grouping    *~
            *                                    Code Descriptions      *~
            *                    - (COST SALE) - Used for Product and   *~
            *                                    Parts Not Costed.      *~
            *                                    By Model, Pcnt Product,*~
            *                                    Pcnt Part, $$'s Part   *~
            *                    - (COST NONE) - By Model, Products we  *~
            *                                    Do Not Cost.           *~
            *  Special Comments  - (Key 1) -Used by NEW Report(APCSLS2 )*~
            *                      (Key 2) -Used by NEW Report(APCSLS3 )*~
            *                              -Used by NEW Report(APCCS4SB)*~
            *                      (Key 3) -Used by NEW Report(APCSLS02)*~
            *                      (Key 4) -Used by NEW Report(APCSLS1 )*~
            *                              -Used by NEW Report(APCSLS4 )*~
            *                              -Used by NEW Report(APCCS5SB)*~
            *                      (Key 5) -Used by NEW Report(APCCST08)*~
            *      CST()         - (1) = Product Material Cost          *~
            *                      (2) = Product Labor Cost             *~
            *                      (3) = Product Overhead               *~
            *                      (4) = Product Freight                *~
            *                      (5) = Product Vinyl Disc             *~
            *                      (6) = Total MFG Cost                 *~
            *                      (7) = Total Transportation Cost      *~
            *                      (8) = Total Price Including all Disc.*~
            *                      (9) = Total Sales Quantity           *~
            *      SLS_FOB             = How Ship Code                  *~
            *                                                           *~
            *  Note - Rebuild Descriptions Set to a Max of (99)         *~
            *         Lines (61553) - BUILD_DESCRIPT                    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/06/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 02/10/95 ! Mod Two New Files (APCSLSCC) Comm Descrip! RHH *~
            *          !   (APCSLSCS) Sales Descr. Assoc with New !     *~
            *          !   Grouping Sort Codes.                   !     *~
            * 04/07/95 ! Mod to modify the calc of commission     ! RHH *~
            *          !   using both Header and line item Discount     *~
            * 07/11/95 ! Mod to Clean-Up the Update for the last  ! RHH *~
            *          !   Invoice Processed. Insure only that    !     *~
            *          !   only Invoice are Counted.              !     *~
            * 10/24/95 ! Mod to Incorporate Product Cost with     ! RHH *~
            *          !   Sales including transportation costs.  !     *~
            * 01/31/96 ! Mod to 'LOAD_FOB' Subroutine to set a    ! RHH *~
            *          !   Fixed Samples and Display Sort Code    !     *~
            *          !   Samples Code (U9A)                     !     *~
            *          !   Display Code (U9F)                     !     *~
            * 06/06/96 ! Mod to Add Debug Display Logic and Set-Up! RHH *~
            *          !   PF(9) Key.                             !     *~
            *          ! Mod to Add CST_FLAG% to Zero Cost for    ! RHH *~
            *          !   Invoice Reason Codes 11 thr 30         !     *~
            * 04/03/97 ! Mods (1) Build Data and Set Sort Keys    ! RHH *~
            *          !   using the G/L Account.                 !     *~
            *          !      (2) Add Two new Fields SALES_ACCT$  ! RHH *~
            *          !          and the Category Code - CAT$.   !     *~
            *          !          To the (APCSLSDT) Analysis File.!     *~
            * 04/18/97 ! Mods to (LOOKUP_SLS_COM) Sub. to Rebuild ! RHH *~
            *          !   the Sales Analysis Data by Obtaining   !     *~
            *          !   the G/L Acct from the CATEGORY File.   !     *~
            *          !   Also taking into Account the Howship   !     *~
            *          !   Codes for Sample, Display, Scrap,      !     *~
            *          !   Salvage and Parts.                     !     *~
            * 04/18/97 ! Mods to remove Sub 'LOAD_FOB' No longer  ! RHH *~
            *          !   needed.                                !     *~
            * 06/24/97 ! Mods to Clean Up Parts, Samples, Sash's, ! RHH *~
            *          !   and Displays.                          !     *~
            * 06/27/97 ! Mods to put error code in the (APCSLSER) ! RHH *~
            *          !   exception file. SLS_ERR$  Establish    !     *~
            *          !   (BUILD_G_L$) Load with the Invoice No. !     *~
            *          !   for where you want to "STOP" correcting!     *~
            *          !   Category and G/L Account Number.       !     *~
            *          !   Default = '00341353' Rebuild all Cat.  !     *~
            *          !   and G/L Account using Part & Howship.  !     *~
            *          !   Note - 1st Invoice for (1997)          !     *~
            * 11/08/97 ! Mods to add a new file for costing       ! RHH *~
            *          !   (APCPLNDP) use by APCCST5B             !     *~
            * 11/12/97 ! Mods to (ARIMASTR) Add 4th Alt Index     ! RHH *~
            *          !                                          !     *~
	    * 03/19/98 ! Y2K modifications                        ! ERN *~
            * 08/27/98 ! (EWD001) Mod to correct No Charge Price  ! RHH *~
            *          !   also take out Commission Calc.         !     *~
            * 05/10/99 ! (EWD002) Modification to create a new    ! RHH *~
            *          !   audit report to check Category Code for!     *~
            *          !   proper G/L Account.                    !     *~
            * 04/10/00 ! (RHHTEST) - New Debug Routine            ! RHH *~
            * 04/14/00 ! (EWD003) Mods for New Cross-Ref DATABASE ! RHH *~
            *          !   (APCSLSTT).                            !     *~
            * 04/17/00 ! (EWD004) Mods for Surviver Buy Back      ! RHH *~
            *          !                                          !     *~     
            * 05/16/00 ! (EWD005) Mods to put Net Invoice Amount  ! RHH *~
            *          !          for Line Item into (linecom)    !     *~
            *          !          new routine'calc_inv_net'       !     *~
            *          !          Also Mods for 'prt_dtl'         !     *~
            * 06/05/00 ! (EWD006) Mods to change calculation for  ! CMG *~
            *          !          amount to use price & qtyshp    !     *~
            *          !          instead of lineext.             !     *~
            * 06/05/00 ! (EWD007) Mods to change credits & adjust.! CMG *~
            *          !          600 surviors to be written into !     *~
            *          !          apcslser.                       !     *~
            * 06/13/00 ! (EWD008) Mods that if acct from cat. is  ! CMG *~
            *          !          not equal to '36' then goto     !     *~
            *          !          apcslser.                       !     *~
            * 06/22/00 ! (EWD009) Mods to put total invoice line  ! CMG *~
            *          !          amt into APCSLSER and if Surv. &!     *~
            *          !          acct is 3606-312 to assign to   !     *~
            *          !          acct 6214-210.                  !     *~            
            *************************************************************

        dim                                                              ~
            acct(50%), acct$(50%)12,     /* Audit Report Original Acct */~
            acct1(50%), acct1$(50%)12,   /* Audit Report New Account   */~
            acct$9,                      /* ACCOUNT CUSTOMER FOR INVOI */~
            account$9,                   /* ACCOUNT CUSTOMER FOR INVOI */~
            accountname$30,              /* ACCOUNT CUSTOMER NAME      */~
            billto$9,                    /* BILLTO CUSTOMER FOR INVOICE*/~
            billtoname$30,               /* BILLTO CUSROMER NAME       */~
	    blankdate$6,		 /* PD representation of null  */~
            bol$3,                       /* BILL OF LADING NUM FOR INV */~
            cat_key$4,                   /* CATEGORY CODE KEY          */~
            cnt$8, cnt1$40,              /* Invoice Scanned Counter    */~
            com_code$3,                  /* Salesman Comm. Grouping    */~
            com$(100%)3,                 /* Store Comm Codes Descr Chg */~
            cost_inv$8,                  /* Starting Invoice No for Cos*/~
            cost_date$10,                /* Costing Invoice Date       */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            descr$32,                    /* Gencode Description        */~
            dummy$8, dummy1$8,           /* DUMMY VARIABLE FOR READ    */~
            dummy2$1, dummy3$1,          /* DUMMY VARIABLE FOR READ    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            gencdkey$50,                 /* KEY TO GENCODES FILE       */~
            hdr$40,                      /* Used by ASKUSER            */~
            hows$20,                     /* How Ship Codes             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invdate$6,                   /* INVOICE DATE               */~
            invlinekey$20,               /* KEY TO ARILINES FILE       */~
            invnumber$8,                 /* LAST INV NUMBER PROCESSED  */~
            invoicekey$8,                /* ALTERNATE KEY FOR ARIMASTR */~
            invreason$9,                 /* Invoice Reason Code        */~
            inv_type$1,                  /* invoice Type code (EWD002) */~
            last_inv$8,                  /* LAST INV NO. PREV RUN      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            msg$(3%)79,                  /* Used by ASKUSER            */~
            or_cat$4, sav_cat$4,         /*                            */~
            or_hows$2,                   /*                            */~
            part$25,                     /* PART NUMBER ON INVOICE LINE*/~
            partdesc$32,                 /* PART DESCRIPTION           */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            poline$3,                    /* HP9000 - Defined  11/01/94 */~
            ponum$16,                    /* CUSTOMER PO NUMBER         */~
            postdate$6,                  /* DATE INVOICE WAS POSTED    */~
            prcode$1,                    /* PRICE CODE USED ON INVOICE */~
            purge_date$8,                /* Purge Date                 */~
            purge_key$20,                /* Primary Key                */~
            purge_rec$34,                /* Part Rec                   */~
            rebuild_key$20,              /* Use When Rebuild Srt Codes */~
            sales_acct$9, cat$4,         /* G/L Sales Distribution Acct*/~
            sav_sales_acct$9,            /* Audit any Override (EWD002)*/~
            sav_cus$9, build_g_l$8,      /* SAVE CUSTOMER CODE         */~
            sav_fob$25, sls_err$1,       /* SAVE CUS AND S.O. NUMBER   */~
            shipdate$6,                  /* DATE PRODUCTS WERE SHIPPED */~
            shipto$9,                    /* SHIP TO CUSTOMER NUMBER    */~
            shiptoname$30,               /* SHIP TO CUSTOMER NAME      */~
            sls$(100%)3, slser_key$20,   /* Store Sls Codes Descr Chg  */~
            sls_code$3,                  /* Sales Analysis Grouping Cod*/~
            sls_free$6,                  /* Sales Analysis Free Area   */~
            slsman$4,                    /* SALESMAN ON INVOICE        */~
            slsname$30,                  /* SALESMAN NAME              */~
            sls_rec$(2%)250,             /* Use to Define Whole Record */~
            soline$3,                    /* SALES ORDER LINE NUMBER    */~
            sonum$16,                    /* SALES ORDER NUMBER         */~
            stlmnt$12,                   /* SETTLEMENT NUMBER          */~
            store$3,                     /* STORE NUMBER ON INVOICE    */~
            s_prv$30,                    /* Private Label Name (EWD003)*/~ 
            s_1$2,                       /* Private Label Code (EWD003)*/~
            s_23$8, s_23m$3,             /* Series Name/Model Code     */~
            taxable$1,                   /* TAXABLE Y OR N FLAG        */~
            title$40,                    /* ERROR LOG REPORT TITLE     */~
            tt_rec$64,                   /* Cross Ref Record           */~
            tt_key$46,                   /* Primary Key                */~
            tt_series$4,                 /* Product Series             */~
            tt_style$4,                  /* Product Style              */~
            userid$3,                    /* Current User Id            */~
            x$1                          /* Dummy variable             */

        dim bg_inv$10, bg_date$10,       /* Starting Inv No., Post Date*/~
            ed_inv$10, ed_date$10,       /* Ending Inv No., Post Date  */~
            calc_code$1, calc_desc$30,   /* S.A Calculation Code 1,2,3 */~
            sale_code$1, sale_desc$30,   /* Salesman Calc Code 1, 2    */~
            txt$(9%)40,                  /* Screen Text                */~
            last_calc$1,                 /* Last S.A. Code Used        */~
            last_sale$1,                 /* Last Salesman Calc Code    */~
            post_yr$4,                   /* Invoice Posting Year       */~
	        tempdate1$10, tempdate2$10,  /* Temp work vars	           */~
            yr1$4, yr2$4                 /* Processing Years           */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */

        dim                              /* Costing Variables          */~
            cst(9%), apc_err$20,         /* MFG Calculated Costs       */~
            old_cst(9%),                 /* Old Cost Data              */~
            apc_inv$8, apc_txt$30,       /* For Processing Shostat     */~
            sls_fob$20,                  /* Delivery / Shipping Info.  */~
            tmp$(7%,50%)25,              /* Store all Raw Mat'l Part No*/~
            tmc(7%,50%),                 /* Assoc. Cut Inches In Dec.  */~
            tmct(7%,50%),                /* Assoc. Total Cost Raw Mat'l*/~
            tmu%(7%,50%),                /* Assoc. Unit of Measure     */~
            tmd$(7%,50%)32,              /* Assoc. Raw Mat'l Desc      */~
            tmuc(7%,50%),                /* Assoc. Raw Mat'l Unit Cost */~
            tmsi(7%,50%),                /* Assoc. Scrap Inches Decimal*/~
            tmsc(7%,50%),                /* Assoc. Scrap Mat'l Cost    */~
            tmeq$(7%,50%)3,              /* Assoc. Calc Typ and Eq No. */~
            tmph$(7%,50%)5,              /* Assoc. Phantom Designature */~
            tcnt%(7%),                   /* Assoc. Count for Each Type */~
            lab(10%),                    /* Breakdown of Labor Cost    */~
            avg_pay(15%),                /* Avg Pay By Dept            */~
            uph(15%),                    /* Avg Unit Per Manhour Dept  */~
            tc(25%),                     /* Total Cost's               */~
            tt(25%),                     /* Cost Total Buckets         */~
            rm_mat(20%),                 /* Total Vinyl,Misc, Mat      */~
            rm_mats(20%),                /* Total Vinyl,Misc, Mat Scrap*/~
            apc_err%(20%),               /* Store Error Code each Modul*/~
            err$(25%)20,                 /* APPLICABLE ERROR MSGS      */~
            pc(36%),                     /* 36 PRICE SHEETS            */~
            cuscode$9,                   /* CUSTOMER CODE              */~
            sale(1000%,3%),              /* STORE 'COST SALE' VALUES   */~
            sale$(1000%)2                /* STORE NO COST FLAG         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 04/14/2000 Sales Analysis - Sales Detail"
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! ARIMASTR ! Invoice Master File                      *~
            * #3  ! ARILINES ! Invoice Line Items File                  *~
            * #4  ! BCKMASTR ! S.O. Header Master File                  *~
            * #5  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #6  ! SLMMASTR ! Salesman master file                     *~
            * #7  ! APCSLCOM ! APC Sales Commission Percentage File     *~
            * #8  ! APCSLSDT ! APC SALES ANALYSIS DETAIL FILE           *~
            * #9  ! GENCODES ! GENERAL SYSTEM CODES FILE                *~
            * #10 ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #11 ! APCSLSCC ! SALES COMMISSION CODE DESCRIPTIONS       *~
            * #12 ! APCSLSCS ! SALES ANALYSIS CODE DESCRIPTIONS         *~
            * #14 ! CATEGORY ! SALES CATAGORY CODES                     *~
            * #15 ! APCSLSER ! Sales Analysis Exception File            *~
            * #16 ! APCSLSTT ! Sales Analysis Cross-Ref      (EWD003)   *~ 
            *************************************************************~
            *                  C O S T I N G   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #21 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #22 ! HNYMASTR ! Part Master File                         *~
            * #23 ! HNYQUAN  ! Inventory Quantities Master File         *~
            * #09 ! GENCODES ! Master Code Table File                   *~
            * #25 ! AMTBOMCD ! Master Equation File                     *~
            * #26 ! AMTBOMIF ! Master Part Validity File                *~
            * #27 ! APCEMPLY ! Employee Master File                     *~
            * #28 ! APCEQUAT ! Equation an Parts Cross Reference File   *~
            * #29 ! APCCSTHP ! Hardware and Packaging Costing Components*~
            * #30 ! APCCSTLR ! Departments Average Hourly Rates         *~
            * #31 ! CPRPRICE ! Master System Price File                 *~
            * #10 ! CUSTOMER ! Master Customer File                     *~
            * #32 ! APCPLNDP ! Planning Master Department File          *~
            * #33 ! APCPCMST ! Pricing Definition file                  *~
            * #34 ! APCSKUNO ! Home Center's Skuno File                 *~
            * #35 ! APCPCMSK ! Pricing Key Definition File              *~
            * #36 ! APCPCMSD ! Pricing Master Calc Definition File      *~
            * #37 ! APCCSTEX ! APC COSTING EXCEPTION FILE               *~
            * #38 ! APCSTOCK ! APC STOCK MASTER FILE                    *~
            * #39 ! APCSLSW1 ! APC SLS TEMPORARY COSTING FILE           *~
            * #40 ! APCSLSWK ! APC SLS ERROR FILE FOR REPORT            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #2,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =   10, keylen =   8, dup,    ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  3, keypos =   34, keylen =  16, dup,    ~
                            key  4, keypos = 1783, keylen =  26

            select #3,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #4,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  26, keylen = 16, dup

            select #5,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    10,keylen =   19

            select #6,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #7,  "APCSLCOM",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =    1, keylen =   8

            select #8,  "APCSLSDT",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =   15, keylen =   20,                    ~
                        alt key 1,keypos = 331, keylen = 36,             ~
                            key 2,keypos = 340, keylen = 27,             ~
                            key 3,keypos =   7, keylen = 28,             ~
                            key 4,keypos = 375, keylen = 27,             ~
                            key 5,keypos =  76, keylen = 25, dup

            select #9,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            select #10,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #11, "APCSLSCC",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =    1, keylen =   3

            select #12, "APCSLSCS",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =    1, keylen =   3

            select #14, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #15, "APCSLSER",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    7, keylen =  20,                     ~
                        alt key  1, keypos  =     1, keylen = 26
                                                        /* (EWD003)    */
            select #16, "APCSLSTT",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  1,   keylen =  46,                     ~
                        alt key  1, keypos  =     7, keylen = 40,        ~
                            key  2, keypos  =     9, keylen = 38,        ~
                            key  3, keypos  =    15, keylen = 32
                                                        /* (EWD003)    */
        REM - COSTING FILES

            select #21, "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #22, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #23, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #25, "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #26, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

            select #27, "APCEMPLY",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =    6, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  10, dup,    ~
                            key  2, keypos  =  11, keylen =  26, dup

            select #28, "APCEQUAT",                                      ~
                        varc,     indexed,  recsize =   16,              ~
                        keypos =    1, keylen =   8

            select #29, "APCCSTHP",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  20

            select #30, "APCCSTLR",                                      ~
                        varc,     indexed,  recsize =  102,              ~
                        keypos =    1, keylen =  3

            select #31, "CPRPRICE"                                       ~
                        varc,     indexed,  recsize = 700,               ~
                        keypos = 1,    keylen =  47

            select #32, "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =    32,             ~
                        keypos =   11, keylen =  12,                     ~
                        alt key  1, keypos  =   9, keylen =  14,         ~
                            key  2, keypos  =   4, keylen =  12,         ~
                            key  3, keypos  =   1, keylen =  15

            select #33, "APCPCMST"                                       ~
                        varc,     indexed,  recsize = 102,               ~
                        keypos = 9,    keylen =  40,                     ~
                        alt key  1, keypos  =     1, keylen = 8

            select #34, "APCSKUNO"                                       ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos = 1,    keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #35, "APCPCMSK"                                       ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 1,    keylen =   5

            select #36, "APCPCMSD"                                       ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos = 1,    keylen =   9

            select #37,  "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            select #38, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32

            select #39, "APCSLSW1",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =  25

            select #40, "APCSLSWK",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =  25

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%, rslt$(7%))
            call "OPENCHCK" (#8 , fs%(8%), f2%(8%),2500%, rslt$(8%))
            call "OPENCHCK" (#9 , fs%(9%), f2%(9%), 0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 50%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 50%, rslt$(12%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%), 50%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),500%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),500%, rslt$(16%))
        REM - COSTING FILES
            call "OPENCHCK" (#21, fs%(21%), f2%(21%),  0%, rslt$(21%))
            call "OPENCHCK" (#22, fs%(22%), f2%(22%),  0%, rslt$(22%))
            call "OPENCHCK" (#23, fs%(23%), f2%(23%),  0%, rslt$(23%))
            call "OPENCHCK" (#25, fs%(25%), f2%(25%),  0%, rslt$(25%))
            call "OPENCHCK" (#26, fs%(26%), f2%(26%),  0%, rslt$(26%))
            call "OPENCHCK" (#27, fs%(27%), f2%(27%),  0%, rslt$(27%))
            call "OPENCHCK" (#28, fs%(28%), f2%(28%),  0%, rslt$(28%))
            call "OPENCHCK" (#29, fs%(29%), f2%(29%),  0%, rslt$(29%))
            call "OPENCHCK" (#30, fs%(30%), f2%(30%),  0%, rslt$(30%))
            call "OPENCHCK" (#31, fs%(31%), f2%(31%),  0%, rslt$(31%))
            call "OPENCHCK" (#32, fs%(32%), f2%(32%),  0%, rslt$(32%))
            call "OPENCHCK" (#33, fs%(33%), f2%(33%),  0%, rslt$(33%))
            call "OPENCHCK" (#34, fs%(34%), f2%(34%),  0%, rslt$(34%))
            call "OPENCHCK" (#35, fs%(35%), f2%(35%),  0%, rslt$(35%))
            call "OPENCHCK" (#36, fs%(36%), f2%(36%),  0%, rslt$(36%))
            call "OPENCHCK" (#37, fs%(37%), f2%(37%),  0%, rslt$(37%))
            call "OPENCHCK" (#38, fs%(38%), f2%(38%),  0%, rslt$(38%))

            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            gosub load_sale

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
	    call "DATUFMTC" (blankdate$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "APCSLS00: " & str(cms2v$,,8)


            err$(1% ) = "(Error) In Labor Cal"
            err$(2% ) = "(Error) in Material "
            err$(3% ) = "(Error) in Glass    "
            err$(4% ) = "(Error) in Screen   "
            err$(5% ) = "(Error) in Locks    "
            err$(6% ) = "(Error) in Hardware "
            err$(7% ) = "(Error) in Packaging"
            err$(8% ) = "(Error) in Pricing  "
            err$(9% ) = "                    "
            err$(10%) = "                    "
            err$(11%) = "                    "
            err$(12%) = "(Err) CSTM Material "
            err$(13%) = "                    "
            err$(14%) = "                    "
            err$(15%) = "                    "
            err$(16%) = "(Err) CSTM Hardware "
            err$(17%) = "(Err) CSTM Packaging"
            err$(18%) = "                    "
            err$(19%) = "                    "
            err$(20%) = "                    "
            cst_err% = 0%                     /* COUNT COSTING ERRORS */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       process_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Two Valid Processing Years? <Current>  <Previous>?   ",~
         "Enter a Valid Starting Invoice Number or (ALL)?              ",~
         "Enter a Valid Ending Invoice Number or (END)?                ",~
         "Enter a Sales Analysis Calc Code? 1=Both, 2=Comm, 3=No Comm  ",~
         "Enter a Salesman From Calc Code? 1 = Customer, 2 = Invoice   ",~
         "Enter Invoice Number to Start Costing Sales, or 99999999=None"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, bg_inv$, ed_inv$,          ~
                      bg_date$, ed_date$, sale_code$, sale_desc$,        ~
                      calc_code$, calc_desc$, txt$(), invnumber$,        ~
                      last_inv$, last_calc$, last_sale$, post_yr$,       ~
                      yr1$, yr2$, purge_key$, purge_rec$, rebuild_key$,  ~
                      sls_rec$(), purge_date$, sav_cus$, sav_fob$,       ~
                      cost_inv$, cost_date$
            gosub load_last
            txt$(1%) = "****************************************"
            txt$(2%) = "*       Previous Processing Data       *"
            txt$(3%) = "*   Starting Invoice Number : XXXXXXXX *"
            txt$(4%) = "*   Ending Invoice Number   : XXXXXXXX *"
            txt$(5%) = "*   S.A. Analysis Calc Code : X        *"
            txt$(6%) = "*   Salesman Calc Code      : X        *"
            txt$(7%) = "****************************************"
            str(txt$(3%),31%,8%) = last_inv$
            str(txt$(4%),31%,8%) = invnumber$
            str(txt$(5%),31%,1%) = last_calc$
            str(txt$(6%),31%,1%) = last_sale$
            debug% = 0%
            build_g_l$ = "00341353"    /* Set to Correct All Cat's and */
                                       /* and Line Item G/L Accounts   */
        return                         /* for Inv's Less than BUILD_G_L*/

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
           qtyshp   = sav_qty            /* (EWD001) - Carrect Values */    
           price    = sav_price
           linedisc = sav_disc
           lineext  = sav_ext
           linecom  = sav_ext

           gosub calc_inv_net            /* (EWD005)                  */ 
           linecom = amount              /* Net Line Item Amount      */
                                         /* (EWD005)                  */ 
           com_pct  = 0.0                /* (EWD001) -                */

           rec% = 0%
           put #8, using L35040 ,                                         ~
                   postdate$,       /* Invoice Posting Date (POSTDATE$)*/~
                   slsman$,         /* Salesman Code          (SLSMAN$)*/~
                   prcode$,         /* Customer Pricing Code  (PRCODE$)*/~
                   com_code$,       /* Commission Group Code(COM_CODE$)*/~
                   invlinekey$,     /* INV Line Item Key  (INVLINEKEY$)*/~
                   sonum$,          /* Sales Order Number      (SONUM$)*/~
                   soline$,         /* Sales Order Line Item  (SOLINE$)*/~
                   bol$,            /* Bill of Lading No.        (BOL$)*/~
                   ponum$,          /* Customer PO Number      (PONUM$)*/~
                   poline$,         /* PO Line Item Number    (POLINE$)*/~
                   part$,           /* MFG Part Number          (PART$)*/~
                   shiptoname$,     /* Ship To Name       (SHIPTONAME$)*/~
                   accountname$,    /* Customer Acct Name(ACCOUNTNAME$)*/~
                   shipdate$,       /* Shipping Date        (SHIPDATE$)*/~
                   invdate$,        /* Invoice Date          (INVDATE$)*/~
                   grsinv,          /* Gross Inv Amt           (GRSINV)*/~
                   invdisc,         /* Invoice Discount Amt   (INVDISC)*/~
                   freight,         /* Freight Amtmt          (FREIGHT)*/~
                   slstax,          /* Sales Tax               (SLSTAX)*/~
                   netinv,          /* Net Invoice Amt         (NETINV)*/~
                   stlmnt$,         /* Inv Settlement Code    (STLMNT$)*/~
                   store$,          /* MFG Store Code          (STORE$)*/~
                   partdesc$,       /* MFG Part Description (PARTDESC$)*/~
                   qtyshp,          /* Quantity Shipped        (QTYSHP)*/~
                   price,           /* MFG Price                (PRICE)*/~
                   linedisc,        /* Line item Disc        (LINEDISC)*/~
                   lineext,         /* Line Item Extended Prc (LINEEXT)*/~
                   taxable$,        /* Item Taxable Y or N   (TAXABLE$)*/~
                   slsname$,        /* Salesman Name         (SLSNAME$)*/~
                   linecom,         /* Line Item Net Amt      (LINECOM)*/~
                   account$,        /* Billing Account       (ACCOUNT$)*/~
                   shipto$,         /* Ship to Account        (SHIPTO$)*/~
                   slsman$,         /* Salesman Code          (SLSMAN$)*/~
                   sls_code$,       /* Sales Analysis Group (SLS_CODE$)*/~
           str(invlinekey$,10%,11%),/* Invoice Line Item KeyINVLINEKEY$*/~
                   com_pct,         /* Comm Pct Used          (COM_PCT)*/~
                   slsman$,         /* Salesman Code          (SLSMAN$)*/~
                   shipto$,         /* Ship To Acct           (SHIPTO$)*/~
                   sls_code$,       /* Sales Analysis Group (SLS_CODE$)*/~
           str(invlinekey$,10%,11%),/* Invoice Line Item KeyINVLINEKEY$*/~
                   cst(),           /* New Costing Buckets             */~
                   sls_fob$,        /* Delivery/Shipping Info          */~
                   sales_acct$,     /* G/L SALES ACCOUNT CODE          */~
                   cat$,            /* CATEGORY CODE                   */~
                   sls_free$        /* Free Data Area (6)              */

                                                /* (256) + (256) = 512 */
           write #8, eod goto L30600
           rec% = 1%
           gosub update_cross_ref                    /* (EWD003)       */

        REM    if sls_code$ = "xxx" then gosub prt_dtl /* (EWD005)     */
                                         /* For Detail report. When    */
                                   /* used turn off at sls_err% = 4 b% */           
L30600: return
                                                     /* (EWD003)       */
        update_cross_ref
           init(" ") s_prv$, s_23$, s_1$, s_23$, tt_series$, tt_style$
           s_23m$ = str(part$,1%,3%)
           s_23% = 0%
           x_er% = 0%

        call "APCPRZSB" (1%,             /* 0%=APCPLNDT, 1%=BCKLINES   */~
                        s_1$,            /* Priv Label CodeInput/Output*/~
                 str(invlinekey$,1%,9%), /* Customer Code      Input   */~
                        s_23m$,          /* Model Code         Input   */~
                        sonum$,          /* Sales Order Number Input   */~
                        soline$,         /* Sales Order Ln ItemInput   */~
                        s_prv$,          /* Private Label Name Output  */~
                        s_23$,           /* Series Name        Output  */~
                        s_23%,           /* Length of Name     Output  */~
                        #10,             /* (CUSTOMER) - Customer Maste*/~
                        #9,              /* (GENCODES) - Code Tables   */~
                        #5,              /* (APCPLNDT) - Planning DTL  */~
                        #5,              /* (BCKLINES) - S.O. Detail   */~
                        x_er% )          /* Return Code                */

                                     /* RE-CALC NET INVOICE AMOUNT     */
           amount  = 0.0              /* PRICE AFTER LINE ITEM DISCOUNT */
           lineamt = 0.0                                      /* (EWD006) */    
           lineamt = round(qtyshp * price, 2)                 /* (EWD006) */
           amount  = round(lineamt * (1.0 - (linedisc/100.0)), 2)  /* (EWD006) */
                                     /* PRICE AFTER ORDER DISCOUNT     */
           amount  = round(amount * (1.0 - (invdisc/100)), 2)

           qtyshp% = qtyshp
            
           pp% = pos(s_23$ = " ") 
           if pp% < 4% then goto L30670
              tt_series$ = str(s_23$,1%,pp% - 1%)
              tt_style$  = str(s_23$,pp%+1%,3%)

L30670:    init(" ") tt_rec$, tt_key$
           str(tt_rec$,1%,6%)   = postdate$       /* Invoice Post Date */ 
           str(tt_rec$,7%,2%)   = s_1$            /* Private Label     */
           str(tt_rec$,9%,6%)   = str(invlinekey$,1%,6%) /* Customer   */
           str(tt_rec$,15%,4%)  = tt_series$      /* Series            */
           str(tt_rec$,19%,4%)  = tt_style$       /* Style             */
           str(tt_rec$,23%,4%)  = cat$            /* Catagory Code     */
           str(tt_rec$,27%,20%) = invlinekey$     /* Cust/Inv/Line     */
           str(tt_rec$,47%,3%)  = s_23m$          /* Model Code        */
           put str(tt_rec$,50%,2%), using L30680, qtyshp%
L30680:        FMT BI(2)                          /* Line Item Qty     */

           put str(tt_rec$,52%,8%), using L30690, amount
L30690:        FMT PD(14,4)                       /* Line Item Net Pric*/ 

           str(tt_rec$,60%,5%)  = " "             /* Filler            */

           tt_key$ = str(tt_rec$,1%,46%)
           read #16,hold,key = tt_key$, eod goto L30700
              delete #16

L30700:    put #16, using L30710, tt_rec$
L30710:       FMT CH(64)

           write #16, eod goto L30720   

        return
L30720:     errormsg$ = "No Update of --> " & tt_key$
            comp% = 2%
            hdr$ = "** E R R O R   E R R O R   **"
            msg$(1%) = "  Creating Cross Reference Data in (APCSLSTT)    "
            msg$(2%) = errormsg$
            msg$(3%) = "Press <RETURN> To Exit, PF(16) to Continue !!!   "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))

            if comp% = 0% then exit_program
        return
                                        /* (EWD003)     */
        calc_inv_net                /* Net Amount for Line Item       */
            amount  = 0.0            /* RE-CALC NET INVOICE AMOUNT     */
                                    /* PRICE AFTER LINE ITEM DISCOUNT */
            lineamt = round(qtyshp * price, 2)  /* (EWD006) */
            amount  = round(lineamt * (1.0 - (linedisc/100.0)), 2) /* (EWD006) */
                                    /* PRICE AFTER ORDER DISCOUNT     */
            amount  = round(amount * (1.0 - (invdisc/100)), 2)
        return

        REM *************************************************************~
            *       F o m a t   S t a t e m e n t s                     *~
            *************************************************************

L35040:     FMT  CH(06),            /* Invoice Posting Date (POSTDATE$)*/~
                 CH(04),            /* Salesman Code          (SLSMAN$)*/~
                 CH(01),            /* Customer Pricing Code  (PRCODE$)*/~
                 CH(03),            /* Commission Group Code(COM_CODE$)*/~
                 CH(20),            /* INV Line Item Key  (INVLINEKEY$)*/~
                 CH(16),            /* Sales Order Number      (SONUM$)*/~
                 CH(03),            /* Sales Order Line Item  (SOLINE$)*/~
                 CH(03),            /* Bill of Lading No.        (BOL$)*/~
                 CH(16),            /* Customer PO Number      (PONUM$)*/~
                 CH(03),            /* PO Line Item Number    (POLINE$)*/~
                 CH(25),            /* MFG Part Number          (PART$)*/~
                 CH(30),            /* Ship To Name       (SHIPTONAME$)*/~
                 CH(30),            /* Customer Acct Name(ACCOUNTNAME$)*/~
                 CH(06),            /* Shipping Date        (SHIPDATE$)*/~
                 CH(06),            /* Invoice Date          (INVDATE$)*/~
                 PD(14,4),          /* Gross Inv Amt           (GRSINV)*/~
                 PD(14,4),          /* Invoice Discount Amt   (INVDISC)*/~
                 PD(14,4),          /* Freight Amtmt          (FREIGHT)*/~
                 PD(14,4),          /* Sales Tax               (SLSTAX)*/~
                 PD(14,4),          /* Net Invoice Amt         (NETINV)*/~
                 CH(12),            /* Inv Settlement Code    (STLMNT$)*/~
                 CH(03),            /* MFG Store Code          (STORE$)*/~
                 CH(32),            /* MFG Part Description (PARTDESC$)*/~
                 PD(14,4),          /* Quantity Shipped        (QTYSHP)*/~
                 PD(14,4),          /* MFG Price                (PRICE)*/~
                 PD(14,4),          /* Line item Disc        (LINEDISC)*/~
                 PD(14,4),          /* Line Item Extended Prc (LINEEXT)*/~
                 CH(01),            /* Item Taxable Y or N   (TAXABLE$)*/~
                 CH(30),            /* Salesman Name         (SLSNAME$)*/~
                 PD(14,4),          /* Line Item Net Amt      (LINECOM)*/~
                 CH(09),            /* Billing Account       (ACCOUNT$)*/~
                 CH(09),            /* Ship to Account        (SHIPTO$)*/~
                 CH(04),            /* Salesman Code          (SLSMAN$)*/~
                 CH(03),            /* Sales Analysis Group (SLS_CODE$)*/~
                 CH(11),            /* Invoice Line Item KeyINVLINEKEY$*/~
                 PD(14,4),          /* Comm Pct Used          (COM_PCT)*/~
                 CH(04),            /* Salesman Code          (SLSMAN$)*/~
                 CH(09),            /* Ship To Acct           (SHIPTO$)*/~
                 CH(03),            /* Sales Analysis Group (SLS_CODE$)*/~
                 CH(11),            /* Invoice Line Item KeyINVLINEKEY$*/~
                 9*PD(14,4),        /* New Costing Buckets (STORE_COST)*/~
                 CH(20),            /* Delivery / Shipping Info        */~
                 CH(09),            /* G/L SALES ACCOUNT CODE          */~
                 CH(04),            /* Category Code                   */~
                 CH(06)             /* Free Area                       */

L35500:     FMT  CH(06),            /* Invoice Posting Date (POSTDATE$)*/~
                 CH(20),            /* Customer Code         (CUSCODE$)*/~
                                    /* Invoice Number      (INVOICENO$)*/~
                                    /* Invoice Line Item    (LINEITEM$)*/~
                 CH(25),            /* Part Number              (PART$)*/~
                 PD(14,4),          /* Quanty Shipped         (QTYSHP$)*/~
                 PD(14,4),          /* MFG Price                (PRICE)*/~
                 PD(14,4),          /* Line Item Disc       (LINEDISC$)*/~
                 PD(14,4),          /* Invoice Discount Amt  (INVDISC$)*/~
                 CH(03),            /* S.A. Group Code      (SLS_CODE$)*/~
                 CH(09),            /* G/L Sales Account  (SALES_ACCT$)*/~
                 CH(04),            /* Catagory Code             (CAT$)*/~
                 CH(16),            /* Sales Order Number      (SONUM$)*/~
                 CH(04),            /* Salesman Code          (SLSMAN$)*/~
                 9*PD(14,4),        /* New Costing Buckets (STORE_COST)*/~
                 CH(01),            /* Error Type Code 1, 2, or 3      */~
                 CH(08)             /* Free Area                       */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,          /* Process Years      */ ~
                                L40200,          /* Beg Invoice No.    */ ~
                                L40200,          /* End Invoice No.    */ ~
                                L40200,          /* S.A Calc Code      */ ~
                                L40200,          /* Salesman Calc Code */ ~
                                L40200           /* Start Cost Inv No. */
              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02),                                               ~
                "APC Sales Analysis and Commission File Create Utility", ~
               at (01,62), "Today:",                                     ~
               at (01,70), fac(hex(8c)), date$                  , ch(10),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Processing Years       :",                   ~
               at (05,30), fac(lfac$(1%)), yr1$                 , ch(04),~
               at (05,40), fac(lfac$(1%)), yr2$                 , ch(04),~
                                                                         ~
               at (06,02), "Starting Invoice Number:",                   ~
               at (06,30), fac(lfac$(2%)), bg_inv$              , ch(08),~
               at (06,40), "Posting Date:",                              ~
               at (06,55), fac(hex(84)),   bg_date$             , ch(10),~
                                                                         ~
               at (07,02), "Ending Invoice Number  :",                   ~
               at (07,30), fac(lfac$(3%)), ed_inv$              , ch(08),~
               at (07,40), "Posting Date:",                              ~
               at (07,55), fac(hex(84)),   ed_date$             , ch(10),~
                                                                         ~
               at (08,02), "Sales Calculation Code :",                   ~
               at (08,30), fac(lfac$(4%)), calc_code$           , ch(01),~
               at (08,40), fac(hex(84)),   calc_desc$           , ch(30),~
                                                                         ~
               at (09,02), "Salesman Calc Code     :",                   ~
               at (09,30), fac(lfac$(5%)), sale_code$           , ch(01),~
               at (09,40), fac(hex(84)),   sale_desc$           , ch(30),~
                                                                         ~
               at (10,02), "Calc Cost at Invoice No:",                   ~
               at (10,30), fac(lfac$(6%)), cost_inv$            , ch(08),~
               at (10,40), fac(hex(84)),   cost_date$           , ch(10),~
                                                                         ~
               at (12,21), fac(hex(84)),   txt$(1%)             , ch(40),~
               at (13,21), fac(hex(84)),   txt$(2%)             , ch(40),~
               at (14,21), fac(hex(84)),   txt$(3%)             , ch(40),~
               at (15,21), fac(hex(84)),   txt$(4%)             , ch(40),~
               at (16,21), fac(hex(84)),   txt$(5%)             , ch(40),~
               at (17,21), fac(hex(84)),   txt$(6%)             , ch(40),~
               at (18,21), fac(hex(84)),   txt$(7%)             , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 6% then L40750   /* Purge Prior Data.      */
                  gosub purge_data            /* Requires Progam Change */
                                              /* Change 'PURGE_DATE$'   */
L40750:        if keyhit% <> 7% then L40790      /* Sales Analysis Sort */
                  code% = 0%
                  gosub rebuild_sls_com

L40790:        if keyhit% <> 8% then L40830   /* Commission Sort Code   */
                  code% = 1%
                  gosub rebuild_sls_com

L40830:        if keyhit% <> 9% then L40860   /* Turn-On Debug Display  */
                  debug% = 1%

L40860:        if keyhit% <> 15 then L40890
                  call "PRNTSCRN" : goto L40230

L40890:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41110             /*  Input Mode            */
            pf$(1%)= "(1)Start Over    (4)Previous Field      " &        ~
                     "(7)Rebuild S.A. Srt                    "
            pf$(2%)= "                                        " &        ~
                     "(8)Rebuild Comm Srt    (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "(6)Purge Data          (16)Exit Program"
            pfkeys$ = hex(01ffff04ff060708ffffff0cffff0f1000)
            if fieldnr% = 1% then L41070
                str(pf$(1%),40%,22%) = " " : str(pfkeys$,7%,1%) = hex(ff)
                str(pf$(2%),40%,22%) = " " : str(pfkeys$,8%,1%) = hex(ff)
                str(pf$(3%),40%,22%) = " " : str(pfkeys$,6%,1%) = hex(ff)
                str(pf$(3%),64%) = " "    : str(pfkeys$,16%,1%) = hex(ff)
L41070:     if fieldnr% > 1% then L41090
                str(pf$(1%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41090:     return

L41110: if fieldnr% > 0% then L41220  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (9)Debug Display       " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Process Data"
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
            if userid$ = "RHH" then return
               str(pf$(2%),18%,18%) = " " : str(pfkeys$,9%,1%) = hex(ff)
            return
L41220:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50160,         /* Processing Years       */~
                              L50240,         /* Starting Invoice No.   */~
                              L50430,         /* Ending Invoice No.     */~
                              L50640,         /* S.A Calc Code          */~
                              L50780,         /* Salesman Calc Code     */~
                              L50920          /* Start Cost Invoice No. */
            return

L50160: REM Test for Processing Years             YR1$, YR2$
              if yr1$ <> " " then goto L50200
                 yr1$ = str(date$,7%,4%)

L50200:       convert yr1$ to yr1%, data goto L50224

              convert (yr1% - 1%) to yr2$, pic(####)

              if yr2$ > yr1$ then goto L50224
        return
L50224:      errormsg$ = "(Error) - Invalid Process Years?"
             init(" ") yr1$, yr2$
        return

L50240: REM Test for Starting Invoice Number      BG_INV$
              bg_date$ = "NONE  "
              if bg_inv$ <> " " then goto L50300
                 bg_inv$ = invnumber$
L50300:          convert bg_inv$ to bg_inv%, data goto L50390

                 convert bg_inv% to bg_inv$, pic(00000000)

                 read #2,key 1% = bg_inv$, using L50360, bg_date$,        ~
                                                           eod goto L50380
L50360:             FMT POS(533), CH(6)
                 call "DATFMTC" (bg_date$)
L50380: return
L50390:   errormsg$ = "(Error) - Invalid Starting Invoice Number?"
          init(" ") bg_inv$, bg_date$
        return

L50430: REM Test for Ending Invoice Number        ED_INV$
              ed_date$ = "NONE  "
              if ed_inv$ <> " " then goto L50500
                 ed_inv$ = "99999999"

L50500:          convert ed_inv$ to ed_inv%, data goto L50600

                 convert ed_inv% to ed_inv$, pic(00000000)

                 read #2,key 1% = ed_inv$, using L50560, ed_date$,        ~
                                                           eod goto L50580
L50560:             FMT POS(533), CH(6)
                 call "DATFMTC" (ed_date$)
L50580:          if bg_inv% > ed_inv% then goto L50600
        return
L50600:   errormsg$ = "(Error) - Invalid Ending Invoice Number?"
          init(" ") ed_inv$, ed_date$
        return

L50640: REM Test for Sales Analysis Calc Code     CALC_CODE$
              if calc_code$ <> " " then goto L50680
                 calc_code$ = "1"

L50680:     p% = pos(calc_code$ = "123")
            if p% = 0% then goto L50740
            if p% = 1% then calc_desc$ = "Both Sales and Commission "
            if p% = 2% then calc_desc$ = "Commissions Only          "
            if p% = 3% then calc_desc$ = "Do Not Do Commissions     "
        return
L50740:     errormsg$ = "(Error) - Invalid Sales Analysis Calc Code?"
            init(" ") calc_code$, calc_desc$
        return

L50780: REM Test for Salesman Calc Code           SALE_CODE$
              if sale_code$ <> " " then goto L50820
                 sale_code$ = "1"

L50820:     p% = pos(sale_code$ = "12")
            if p% = 0% then goto L50880
            if p% = 1% then sale_desc$ = "Obtain Salesman From Customer"
            if p% = 2% then sale_desc$ = "Obtain Salesman From Invoice "

        return
L50880:     errormsg$ = "(Error) - Invalid Sales Analysis Calc Code?"
            init(" ") sale_code$, sale_desc$
        return

L50920: REM Starting Invoice Number for Costing   COST_INV$
              init(" ") cost_date$
              if cost_inv$ <> " " then goto L50960
                 cost_inv$ = bg_inv$
L50960:          convert cost_inv$ to cost_inv%, data goto L51050

                 convert cost_inv% to cost_inv$, pic(00000000)

                 read #2,key 1% = cost_inv$, using L51020, cost_date$,    ~
                                                           eod goto L51040
L51020:             FMT POS(533), CH(6)
                 call "DATFMTC" (cost_date$)
L51040: return
L51050:   errormsg$ = "(Error) - Invalid Starting Cost Invoice Number?"
          init(" ") cost_inv$, cost_date$
        return

        REM *************************************************************~
            *          C O S T I N G   E R R L O G   R E P O R T        *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~--------------+
L55060: %!---------------------------------------------------------------~
        ~--------------!
L55080: %!########## @ ########   #######################################~
        ~#    Page: ###!
L55100: %!QTY !<--- MFG Part Number --->!<-- Error Message ->!MFG Cost! M~
        ~FG Quantity   !
L55120: %!----!-------------------------!--------------------!--------!--~
        ~--------------!
L55140: %!####!#########################!####################!####.##-!  ~
        ~  ########.##-!
                                                           /* (EWD002) */  

L55142: %+---------------------------------------------------------------~
        ~---------------------------------------------------------+
L55144: %!---------------------------------------------------------------~
        ~---------------------------------------------------------!
L55146: %!########## @ ########           ###############################~
        ~#########                                       Page: ###!

L55150: %!Customer!Invoice !<--- MFG Part Number --->!Old Cat!New Cat!Old~
        ~ Account!New Account!HowS! Qty !Init Price ! Total Price !

L55160: %!--------!--------!-------------------------!-------!-------!---~
        ~--------!-----------!----!-----!-----------!-------------!
       
L55170: %! ###### !########!#########################!  #### !  #### ! ##~
        ~####### ! ######### ! ## !####-! #####.##- !  ##,###.##- !

L55180: %!Totals for G/L Account Replacements Original G/L Account Totals~
        ~            New G/L Account totals                       !

L55190: %!                                    ------------  -------------~
        ~-         -----------  -------------                     !

L55200: %!                                      #########    ####,###.##-~
        ~           #########    ####,###.##-                     !

L55210: %!                                                               ~
        ~                                                         !
L55220: %!                                                   ####,###.##-~
        ~                        ####,###.##-                     !
                                                           /* (EWD002) */
    
        REM *************************************************************~
            *               S P E C I A L   R O U T I N E S             *~
            *************************************************************

        select_printer
            title$ = " EWD Costing Sales Analysis Error Report"
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            init(" ") xtime$
            call "TIME" (xtime$)
            call "DATFMTC" (date$)
            call "SETPRNT" (" ","CERR",0%,0%)
            select printer(134)
        return

        sel_prt                                            /* (EWD002)  */
            title$ = " EWD Sales Audit Log for (Category Code)"
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            init(" ") xtime$
            call "TIME" (xtime$)
            call "DATFMTC" (date$)
            call "SETPRNT" (" ","AUDIT",0%,0%)
            select printer(134)
        return

        prt_header
            if lcnt% <> 99% then print using L55040
            pageno% = pageno% + 1%
            print page
            print using L55040
            print using L55080, date$, xtime$, title$, pageno%
            print using L55060
            print using L55100
            lcnt% = 4%
        return

        prt_hdr                                            /* (EWD002) */
            if lcnt% <> 99% then print using L55142
            pageno% = pageno% + 1%
            print page
            print using L55142
            print using L55146, date$, xtime$, title$, pageno%
            print using L55144
            print using L55150
            lcnt% = 4%
        return

       prt_detail
            if lcnt% > 57% then gosub prt_header
               print using L55120
               print using L55140, e_qty%, part$, apc_err$, tot_cst,      ~
                           x_qty

            lcnt% = lcnt% + 2%
        return

       prt_dtl                                             /* (EWD002) */
            if lcnt% > 57% then gosub prt_hdr
                                                      /* (EWD005)        */
        REM       tt_price = round(qtyshp * price, 2)
               tt_price = round(linecom, 2)           /* (EWD005)        */
               qtyshp% = qtyshp
               print using L55160
               print using L55170, str(invlinekey$,1%,6%),                ~
                                str(invlinekey$,10%,8%), part$, sav_cat$, ~
                                cat$, sav_sales_acct$, sales_acct$,       ~
                                str(hows$,1%,2%), qtyshp%, price, tt_price

            lcnt% = lcnt% + 2%
        return

        prt_totals
            tot_acct = 0.0 : tot_acct1 = 0.0
            print using L55142
            pageno% = pageno% + 1%
            print page
            print using L55142
            print using L55146, date$, xtime$, title$, pageno%
            print using L55144
            print using L55180
            print using L55190
            xx_max% = acct_max%
            if xx_max% < acct1_max% then xx_max% = acct1_max%
               for aa% = 1% to xx_max% 
                   print using L55200, acct$(aa%), acct(aa%), acct1$(aa%),~
                                                              acct1(aa%)
                   print using L55210

                   tot_acct  = round(tot_acct  + acct(aa%) , 2)
                   tot_acct1 = round(tot_acct1 + acct1(aa%), 2)
               next aa%
            print using L55190
            print using L55220, tot_acct, tot_acct1  
        return

        load_last
            read #1,key = "APC SALES ANAL INV",using L60195  ,invnumber$, ~
                          last_inv$, last_calc$, last_sale$,eod goto L60200
L60195:          FMT XX(20),CH(8), CH(8), CH(1), CH(1)
L60200: return

        update_last
            last_calc$ = calc_code$
            last_sale$ = sale_code$
            read #1,hold,key = "APC SALES ANAL INV",using L60195,dummy$,  ~
                                dummy1$, dummy2$, dummy3$, eod goto L60275
                delete #1
            dummy1$ = dummy$                 /* Starting Invoice No.   */
            dummy$  = invnumber$             /* Ending Invoice NO.     */
                                             /* Last Invoice Processed */
            write #1,using L60285,"APC SALES ANAL INV", dummy$,           ~
                                  dummy1$, last_calc$, last_sale$, " "
            return
                                                 /* Only the 1st Time  */
L60275:  write #1,using L60285,"APC SALES ANAL INV", invnumber$,          ~
                              "00000001", last_calc$, last_sale$, " "
L60285:      FMT CH(20),CH(8), CH(8), CH(1), CH(1), CH(462)
        return

        process_data
           call "APCPAUSE" (apc%, "APCSLS00")
           if apc% <> 0% then goto exit_program

           call "SHOSTAT" ("Creating Sales Analysis Detail")

           gosub sel_prt                                   /* (EWD002) */
           acct_max%  = 0%                       /* Original G/L Acct  */ 
           acct1_max% = 0%                       /* New G/L Account    */
           mat acct  = zer
           mat acct1 = zer   
           cnt% = 0%
           init(" ") invoicekey$, acct$(), acct1$()
           invnumber$ = bg_inv$
           str(invoicekey$,1%,8%) = invnumber$
        arimastr_loop
            read #2,key 1% > invoicekey$,using L60370, shipto$,          ~
                          invoicekey$,ponum$,sonum$,bol$,                ~
                    shipdate$, hows$, sls_fob$, slsman$,prcode$,invdate$,~
                  postdate$,grsinv,invdisc,freight,slstax,netinv,billto$,~
                          stlmnt$,store$, inv_type$, invreason$, account$,~
                          eod goto update_summary
L60370:          FMT CH(9),CH(8),CH(16),CH(16),CH(3), POS(413), CH(6),   ~
                     CH(20), CH(20), POS(501), CH(4), POS(520), CH(1),   ~
                   CH(6),XX(6),CH(6),POS(793),PD(14,4),PD(14,4),XX(8),   ~
                    PD(14,4),PD(14,4),PD(14,4),XX(8),CH(9),CH(12),CH(3), ~
                    XX(18), CH(1), CH(9), POS(1770),CH(9)

           apc_inv$ = str(invoicekey$,1%,8%)
           if apc_inv$ > ed_inv$ then goto update_summary
           cnt% = cnt% + 1%
           if mod(cnt%,250) <> 0 then goto L60450
              apc_txt$ = "SCN= XXXXXXX INV NO.= XXXXXXXX"
              convert cnt% to str(apc_txt$,6%,7%), pic(#######)

              str(apc_txt$,23%,8%) = apc_inv$
              call "SHOSTAT" (apc_txt$)

L60450:    billtoname$,shiptoname$,accountname$ = all(hex(20))
	   tempdate1$ = postdate$
	   call "DATEFMT" (tempdate1$, tempdate1%, tempdate2$)
           post_yr$ = str(tempdate2$,1%,4%)        /* Save Posting Year */
           /* Special Calc */
           if post_yr$ <> yr1$ and post_yr$ <> yr2$ then                 ~
                                                       goto arimastr_loop
                                 /* (1) Salesman from Customer         */
                                 /* (2) Salesman from Invoice ( S.O. ) */
           if sale_code$ = "2" then                                      ~
             read #10, key = shipto$, using L60515,shiptoname$,           ~
                                      eod goto L60525                     ~
                               else                                      ~
             read #10,key = shipto$,using L60520,shiptoname$, slsman$,    ~
                                      eod goto L60525
L60515:          FMT POS(253),CH(30)
L60520:          FMT POS(253),CH(30), POS(714), CH(4)
L60525:    read #10,key = billto$,using L60515,billtoname$,               ~
                                                           eod goto L60535
L60535:    read #10,key = account$,using L60515,accountname$,             ~
                                                           eod goto L60545
L60545:     if len(slsman$) < 3 then slsman$ = "0000"
            read #6,key = slsman$,using L60555,slsname$, eod goto L60560
L60555:              FMT XX(4),CH(30)
L60560:     str(invlinekey$,1%,9%)  = str(shipto$,1%,9%)
            str(invlinekey$,10%,8%) = str(invoicekey$,1%,8%)
            str(invlinekey$,18%,3%) = all(hex(20))
            cst_flag% = 0%
            convert invreason$ to cst_flag%, data goto L60585
L60585:
            gosub check_howship
        arilines_loop
            sls_err% = 0%                            /* (EWD002) - Fix */
            crd_err% = 0%                            /* (EWD007) - Fix */
            read #3,key > invlinekey$,using L60620,invlinekey$,poline$,  ~
                          part$,partdesc$,cat$,qtyshp,price,linedisc,    ~
                          lineext,taxable$, sales_acct$, soline$,        ~
                          eod goto arimastr_loop
L60620:         FMT CH(20),CH(3),CH(25),CH(32),CH(4),POS(93),PD(14,4),   ~
                    POS(133),PD(14,4),PD(14,4),POS(157),PD(14,4),CH(1),  ~
                    CH(9),POS(194),CH(3)

            if str(invlinekey$,10%,8%) <> str(invoicekey$,1%,8%) then    ~
                                                            arimastr_loop
            sav_qty   = qtyshp             /* (EWD001) - Carrect Values */    
            sav_price = price
            sav_disc  = linedisc
            sav_ext   = lineext
            sls_qty   = qtyshp
            sls_price = price              /* (EWD001) -               */
            sav_sales_acct$ = sales_acct$  /* (EWD002)                 */ 

            sav_cat$ = cat$                /* Save Copy of Category    */
            model$ = str(part$,1%,3%)
            part% = 0%                     /* By Pass All Alpha Prod   */
            if str(model$,1%,1%) = "9" then part% = 1%
            if str(model$,1%,1%) = "9" then goto L60630
               convert model$ to part%, data goto update_apcslser
                                           /* By-Pass Invalid Part     */

                                           /* (EWD001)                 */
                                           /* Remove Commission Calc   */
L60630:     gosub lookup_sls_com                 /* Sales Analysis Srt */
                                                 /* Commission Sort    */
            if sls_com% <> 0% then goto L60720
               sls_err% = 2%                     /* Invalid G/L Acct   */
               goto update_apcslser

L60720:     read #8,hold,key = invlinekey$, using L60730, old_cst(),      ~
                                                         eod goto L60770
L60730:        FMT POS(402), 9*PD(14,4)
                 delete #8
             if str(invlinekey$,10%,8%) >= cost_inv$ then goto L60770
                for i% = 1% to 9%
                    cst(i%) = old_cst(i%)
                next i%
                goto L60780
             
                                                      /* (RHHTEST)   */
L60770:   if Debug% = 0% then goto L60775
             call "SHOSTAT" ("Building Data for Invoice- " & apc_inv$)
             stop

L60775:   gosub calc_cost
                                                      /* (RHHTEST)   */
L60780:   if debug% = 1% then                                          ~
             call "SHOSTAT" ("Last Invoice Processed" & apc_inv$)

          if crd_err% <> 0% then goto update_apcslser  /* (EWD007) - Fix */
          
             gosub dataput
                                                   /* (EWD002) First (4)*/
                                                   /* digits of Account */ 
             if str(sav_sales_acct$,1%,4%) = str(sales_acct$,1%,4%) then ~
                                                      goto L60790
                sls_err% = 4%                      /* Account Replaced  */
                gosub prt_dtl                      /* Comment for Detail*/
                gosub update_exception
                goto L60800
 
L60790:      if sav_cat$ = cat$ then goto L60800
                sls_err% = 5%                      /* Account Replaced  */
                gosub update_exception             /* Not on Report     */              
                                                   /* (EWD002)          */
L60800:      if rec% <> 0% then goto L60820
                sls_err% = 3%                      /* Data Update Error */
        update_apcslser                            /* Track all Errors  */
                if part% = 0% then sls_err% = 1%   /* Invalid Part No.  */
                gosub delete_old
                goto arilines_loop

L60820
*       RHH
            x$ = str(invlinekey$,10%,1%)
            convert x$ to rhh%, data goto arilines_loop /*ONLY INVOICES*/
            if rhh% <> 0% then goto arilines_loop
            if str(invlinekey$,10%,8%) > invnumber$ then invnumber$ =    ~
                                                 str(invlinekey$,10%,8%)
            goto arilines_loop

        update_summary
            gosub update_last
        
            gosub prt_totals
            close printer                                  /* (EWD002) */

            gosub create_report
        return clear all
        goto exit_program

        delete_old
            gosub update_exception
            read #8,hold,key = invlinekey$, using L60910, old_cst(),      ~
                                                         eod goto L60920
L60910:        FMT POS(402), 9*PD(14,4)
                 delete #8
L60920: return

        update_exception
                                                      /* (RHHTEST)   */
        if debug% = 0% then goto L60950
           call "SHOSTAT" ("Exception for Invoice- " & apc_inv$)
           stop

L60950:     gosub calc_cost

            price = 0.00                            /* (EWD009)      */
            price = sls_price * qtyshp              /* (EWD009)      */
            init(" ") slser_key$, sls_err$
            convert sls_err% to sls_err$, pic(#)

            slser_key$ = invlinekey$
            read #15,hold,key = slser_key$, eod goto L60970
               delete #15
L60970:     put #15, using L35500, postdate$, invlinekey$,                ~
                                   part$, qtyshp, price, linedisc,        ~
                                   invdisc, sls_code$, sales_acct$, cat$, ~
                                   sonum$, slsman$, cst(), sls_err$, " "
            write #15, eod goto L60995
                                                 /* (EWD002) - Log      */
            if sls_err% <> 4% then goto L60995   /* Only for Account    */

            aa% = 0% : bb% = 0%
            if acct_max% = 0% then goto L60975   /* Check Original Acct */
               for aa% = 1% to acct_max%
                   if acct$(aa%) = sav_sales_acct$ then goto L60980
               next aa%
L60975:        aa% = aa% + 1%
               if aa% > 50% then aa% = 50%
               acct_max% = aa%
L60980:        if acct1_max% = 0% then goto L60985/* Check New Account  */ 
                  for bb% = 1% to acct1_max%
                      if acct1$(bb%) = sales_acct$ then goto L60990
                  next bb%
L60985:           bb% = bb% + 1%
                  if bb% > 50% then bb% = 50%
                  acct1_max% = bb%
L60990:           acct1$(bb%) = sales_acct$
                  acct1(bb%)  = round(acct1(bb%) + tt_price, 2)
                                                       /* New Account   */
                  acct$(aa%)  = sav_sales_acct$
                  acct(aa%)   = round(acct(aa%)  + tt_price, 2)
                                                       /* Old Orig Acct */ 
 
L60995:     price = sav_price                          /* (EWD009)      */
         return


        lookup_sls_com                 /* MAKE "6297/6283" LIKE "3603" */
            gosub build_category
            sales_acct$ = acct$        /* Set G/L Account by Category  */
            sls_com% = 0%              /* Make '6297/6283' Like '3603' */
            if str(sales_acct$,1%,4%) <> "6297"  and                     ~
               str(sales_acct$,1%,4%) <> "6283" then goto L61300
               str(sales_acct$,1%,4%) = "3603"
               goto L61310
L61300:     if str(sales_acct$,1%,2%) <> "36" then return

L61310:     init(" ") gencdkey$          /* Account begins "36" Always */
            str(gencdkey$,1%,9%)   = "SLS CODE6"
            str(gencdkey$,10%,15%) = str(sales_acct$,1%,4%)
            read #9,key = gencdkey$,using L61330,sls_code$,eod goto L61350
L61330:        FMT POS(25), CH(3)
            com_code$ = sls_code$
            sls_com% = 1%
        return
L61350:     sls_code$ = "S1M"                        /* Misc Product   */
            com_code$ = "S1M"
            sales_acct$ = "3615-313"
            cat$ = "MISC"
            sls_com% = 1%
        return

        purge_data
            gosub prompt_user
            if comp% = 16% then goto L61420
               if comp% <> 0% then goto purge_data
                  return clear all
                  goto inputmode

L61420:     purge_key$ = all(hex(00))
            purge_date$ = "19981231"            /* RHH - 02/15/1999  */
            cnt% = 0% : cnt1% = 0%
            call "SHOSTAT" ("Purging Data Prior to ("&purge_date$&")")
            cnt1$ = "Inv Checked=xxxxxxx Purged=xxxxxxx"
        purge_next
            read #8,hold,key > purge_key$, using L61455, purge_rec$,      ~
                                                      eod goto purge_done
L61455:        FMT CH(34)
            cnt% = cnt% + 1%
            if mod(cnt%,100) <> 0 then goto L61480
                convert cnt% to str(cnt1$,13%,7%), pic(0000000)

                convert cnt1% to str(cnt1$,28%,7%), pic(0000000)
              call "SHOSTAT" (cnt1$)
L61480:     purge_key$ = str(purge_rec$,15%,20%)
	    call "DATEFMT" (str(purge_rec$,1%,6%), tempdate1%, tempdate2$)

            if tempdate2$ > purge_date$ then goto purge_next
               delete #8
               cnt1% = cnt1% + 1%
               goto purge_next
        purge_done
        return clear all
        goto exit_program

        rebuild_sls_com
            gosub prompt_user
            if comp% = 16% then goto L61560
               if comp% <> 0% then goto rebuild_sls_com
                  return clear all
                  goto inputmode
                                /* Code% = 0% - Rebuild Sales Analysis */
                                /* Code% = 1% - Rebuild Commission Srt */
L61560:     if code% = 0% then                                           ~
                 call "SHOSTAT" ("Rebuilding Sales Analysis Sort")
            if code% = 1% then                                           ~
                 call "SHOSTAT" ("Rebuilding Commission Sort")
            cnt%, sls_max%, com_max% = 0%
            init(" ") rebuild_key$, sls$(), com$()
        rebuild_next
            read #8,hold,key > rebuild_key$, using L61605, sls_rec$(),    ~
                                                    eod goto rebuild_done
L61605:         FMT CH(250), CH(200)
            rebuild_key$ = str(sls_rec$(1%),15%,20%)
            model$       = str(sls_rec$(1%),76%,3%)
            cnt% = cnt% + 1%
            if mod(cnt%,100) <> 0 then goto L61640
               convert cnt% to cnt$, pic(00000000)
               call "SHOSTAT" ("Invoices Scanned ---> ("& cnt$ &")" )
L61640:     delete #8
            gosub lookup_sls_com
            put #8, using L61605, sls_rec$()
            if code% = 0% then put #8,using L61665, sls_code$, sls_code$  ~
                          else put #8,using L61670, com_code$
L61665:                           FMT POS(353), CH(3), POS(388), CH(3)
L61670:                           FMT POS(12), CH(3)
            write #8, eod goto L61710
            gosub build_descript          /* Also Rebuild Descriptions */

            goto rebuild_next
        rebuild_done
        return clear all
        goto exit_program
L61710:     stop "(Error) - Could not Update Invoice --> " & rebuild_key$
            close ws
            goto rebuild_next

        prompt_user
            comp% = 2%
            hdr$ = "* Sales Analysis/Commission *"
            msg$(1) = "Do you wish to run Sales Analysis/Commission Util"
         if keyhit% = 7% then                                            ~
            msg$(2) = "R e b u i l d   Sales Analysis   S o r t  C o d e"
         if keyhit% = 8% then                                            ~
            msg$(2) = "  R e b u i l d   Commission S o r t   C o d e   "
         if keyhit% = 6% then                                            ~
            msg$(2) = "         * * * P u r g e   D a t a * * *         "
         if keyhit% = 12% then                                           ~
            msg$(2) = "   * * * F i x   C o s t i n g   D a t a * * *   "
            msg$(3) = "Press <RETURN> To Exit, PF(16) to Continue !!!   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))

        REM COMP% = 0%               /* Force Exit with '0%' */
        return

        build_descript
            init(" ") gencdkey$
            if code% <> 0% then goto L61940
               if sls_max% = 99% then return  /* Done with Descriptions*/
               if sls_max% = 0% then goto L61865
                  for i% = 1% to sls_max%
                      if sls_code$ <> sls$(i%) then goto L61860
                         return               /* Done - Only Do Once   */
L61860:           next i%
L61865:        sls_max% = sls_max% + 1%
               i% = sls_max%
               sls$(i%) = sls_code$
                                              /* '0'- Sales Analysis   */
            str(gencdkey$,1%,9%)   = "SLS CODE2"
            str(gencdkey$,10%,15%) = sls_code$
            read #9,key = gencdkey$, using L61900, descr$, eod goto L61935
L61900:           FMT POS(25), CH(30)
            read #12,hold,key = sls_code$, eod goto L61920
                delete #12                    /* Clear Out Old Descript*/

L61920:     put #12,using L61925, sls_code$, str(descr$,1%,20%)
L61925:        FMT CH(3), CH(20)
            write #12, eod goto L61935
L61935: return
L61940:        if com_max% = 99% then return  /* Done with Descriptions*/
               if com_max% = 0% then goto L61970
                  for i% = 1% to com_max%
                      if com_code$ <> com$(i%) then goto L61965
                         return               /* Done - Only Do Once   */
L61965:           next i%
L61970:        com_max% = com_max% + 1%
               i% = com_max%
               com$(i%) = com_code$
                                              /* '1'- Sales Commission */
            str(gencdkey$,1%,9%)   = "SLS CODE3"
            str(gencdkey$,10%,15%) = com_code$
            read #9,key = gencdkey$, using L62005, descr$, eod goto L62040
L62005:           FMT POS(25), CH(30)
            read #11,hold,key = com_code$, eod goto L62025
                delete #11                    /* Clear Out Old Descript*/

L62025:     put #11,using L62030, sls_code$, str(descr$,1%,20%)
L62030:        FMT CH(3), CH(20)
            write #11, eod goto L62040
L62040: return

        calc_cost
                                                      /* (RHHTEST)   */
        if debug% = 0% then goto L62050
           call "SHOSTAT" ("(A) Begin Costing - " & part$)
           stop
 
L62050:    calc% = 0%
           mat lab  = zer     : mat tc   = zer
           mat apc_err% = zer : mat pc = zer
           width = 0  
           convert str(part$,13%,4%) to width, data goto L62080
L62080:
           kk% = 1%
           convert str(part$,1%,3%) to kk%, data goto L62095
L62095:
           cuscode$ = shipto$
           if sls_qty <> 0 then goto L62135
              mat cst = zer                    /* No Product Shipped */
              tot_cst = 0.0                    /* therfore, No Cost  */
              return
                                               /* 04/14/1999 Skip    */
           gosub check_error_log               /* (EWD002) - Fix       */
           if error_log% <> 0% then return
L62135:    p_err% = 0%
           x_err% = 0%
           if len(part$) < 19 then x_err% = 2%  /* Product is a Part   */
           if width = 0 then x_err% = 2%        /* Product is a Mull   */
           if x_err% <> 0% then goto L62445      /* which is the Same   */
                                                /* as a Part.          */
           if str(sale$(kk%),2%,1%) = "*" then calc% = 8% /* Price Only*/
                                                      /* (RHHTEST)   */
           if debug% = 0% then goto L62150
              convert calc% to rhh$, pic(###)
                                      
              call "SHOSTAT" ("(B) Start Costing - " & rhh$)
              stop
 
L62150:    call "APCCST0B" ( calc%,      /* Calculation Method         */~
                             part$,      /* MFG Part Number            */~
                             0.0,        /* Cost Adjustment Dollars    */~
                             tmp$(),     /* Raw Mat'l Part Numbers     */~
                             tmc(),      /* Raw Mat'l Cut Inches in Dec*/~
                             tmct(),     /* Raw Mat'l Costs            */~
                             tmu%(),     /* Raw Mat'l Calc Unit of Meas*/~
                             tmd$(),     /* Raw Mat'l Descriptions     */~
                             tmuc(),     /* Raw Mat'l Unit Cost        */~
                             tmsi(),     /* Raw Mat'l Scrap Inches Dec */~
                             tmsc(),     /* Raw Mat'l Scrap Cost       */~
                             tmeq$(),    /* Calc Type and Equation No. */~
                             tmph$(),    /* Phantom Number             */~
                             tcnt%(),    /* Raw Mat'l Type Counts      */~
                             "A",        /* Labor Type (A) or (S)tand  */~
                             lab(),      /* Labor Costs (1 thru 10)    */~
                             avg_pay(),  /* Avg Hourly Pay by Dept     */~
                             uph(),      /* Avg Units Per Manhour Dept */~
                             tc(),       /* Material Costs (1 thru 25) */~
                             tt(),       /* Total Cost Buckets         */~
                             rm_mat(),   /* Material Costs (1 thru 10) */~
                             rm_mats(),  /* Mat'l Scrap Costs(1 thru 9)*/~
                             "EM0100",   /* Customer Code for Pricing  */~
                             pc(),       /* 35 Prices                  */~
                             price,      /* Calc. Price for Customer   */~
                             #21,        /*   (APCCUTEQ)               */~
                             #22,        /*   (HNYMASTR)               */~
                             #23,        /*   (HNYQUAN )               */~
                             #9,         /*   (GENCDSIN)               */~
                             #25,        /*   (AMTBOMCD)               */~
                             #27,        /*   (APCEMPLY)               */~
                             #28,        /*   (APCEQUAT)               */~
                             #29,        /*   (APCCSTHP)               */~
                             #30,        /*   (APCCSTLR)               */~
                             #31,        /*   (CPRPRICE)               */~
                             #10,        /*   (CUSTOMER)               */~
                             #33,        /*   (APCPCMST)               */~
                             #34,        /*   (APCSKUNO)               */~
                             #35,        /*   (APCPCMSK)               */~
                             #36,        /*   (APCPCMSD)               */~
                             #37,        /*   (APCCSTEX)               */~
                             #38,        /*   (APCSTOCK)               */~
                             #32,        /*   (APCPLNDP)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */

                                                      /* (RHHTEST)   */
           if debug% = 0% then goto L62405
              call "SHOSTAT" ("(C) Completed Costing of - " & part$)
              stop
 

L62405:    if str(sale$(kk%),2%,1%) <> "*" then goto L62410
               x_err% = 3%
               p_err% = 0%
               goto L62445
L62410:     for i% = 1% to 20%
              if apc_err%(i%) = 0% then goto L62435
                 p_err% = i%
                 x_err% = 1%
                 cst_err% = cst_err% + 1%
L62435:     next i%

L62445:   gosub store_cost                    /* Gather All MFG Costs  */
          if p_err% <> 0% then gosub update_error_log

                                                        /* (RHHTEST)   */
           if debug% = 0% then return
              call "SHOSTAT" ("(D) Done Costing - " & part$)
              stop
 
        return

        store_cost
           gosub calc_transport
           mat cst = zer                     /* Total Vinyl and Misc.  */
           tot_mat = 0.0 : tot_cst = 0.0     /* (Mat'l + Scrap) Cost   */
           tot_mat = round(tc(3%) + tc(6%) + tc(9%) + tc(12%) + tc(15%) +~
                           tc(18%) + tc(21%) + tc(19%) - tc(20%), 4%)
                                             /* Include W/F Amount     */
                                             /* Include Freight        */
                                             /* Deduct Vinyl Discount  */
           if x_err% > 1% then gosub compute_cost
                                     /* Use Cost from Costing Errors   */
           tot_cst = round( tot_mat + lab(8%) + lab(9%), 4)
                                     /* Note - CST() are the Values    */
                                     /*   Assoc. with each Line Item   */
           cst(1%) = round(tot_mat * sls_qty, 2) /* Total Material     */
           cst(2%) = round(lab(8%) * sls_qty, 2) /* Total Dir/Ind Labor*/
           cst(3%) = round(lab(9%) * sls_qty, 2) /* Total Overhead Cost*/
           cst(4%) = round(tc(19%) * sls_qty, 2) /* Total Freight Cost */
           cst(5%) = round(tc(20%) * sls_qty, 2) /* Total Vinyl Disc't */
           cst(6%) = round(tot_cst * sls_qty, 2) /* Total MFG Cost     */
           cst(7%) = round(trn_amt * sls_qty, 2) /* Total Trans Cost   */
           if sls_qty < 0 then cst(7%) = 0.0     /* NO TRANS - CREDIT  */
           cst(8%) = round(sls_price, 2)         /* Total Prc W/Disc */
           cst(9%) = round(sls_qty, 2)           /* Total Quantity     */
           if cst_flag% < 11% then return        /* Zero Cost Based on */
              cst(1%) = 0.0 : cst(2%) = 0.0      /* Invoice Reason Code*/
              cst(3%) = 0.0 : cst(4%) = 0.0
              cst(5%) = 0.0 : cst(6%) = 0.0
              cst(7%) = 0.0
        return

        calc_transport
           if sav_cus$ = cuscode$ then return
              sav_cus$ = cuscode$

           gencdkey$ = " " : sls_regn% = 0% : trn_amt = 0.0
           str(gencdkey$,1%,9%)   = "COST TRAN"
           str(gencdkey$,10%,15%) = cuscode$
           read #9,key = gencdkey$, using L62665, descr$, eod goto L62680
L62665:       FMT POS(25), CH(30)
           convert str(descr$,1%,2%) to sls_regn%, data goto L62680

L62680:    convert sls_regn% to sls_regn$, pic(00)
           gencdkey$ = " "
           str(gencdkey$,1%,9%)   = "COST REGN"
           str(gencdkey$,10%,15%) = sls_regn$
           read #9,key = gencdkey$, using L62665, descr$, eod goto L62720

           convert str(descr$,1%,8%) to trn_amt, data goto L62720

L62720: return

        compute_cost
           tot_mat = 0.0
           mat lab = zer : mat tc = zer
           if str(sale$(kk%),1%,1%) <> "*" then kk% = 1%
           unit_price = 0.0
           if sls_qty <> 0 then unit_price = round(sls_price/sls_qty, 2)

           if x_err% <> 2% then goto L62800
                                                  /* Product is a Part */
              if unit_price = 0 then tot_mat = sale(kk%,3%)              ~
                                else tot_mat = unit_price * sale(kk%,2%)
           if tot_mat = 0 then goto L62800
        return
                                                  /* Costing Error     */
L62800:    tot_mat = (pc(1%) * .50) * sale(kk%,1%)/* Calc based on the */
                                                  /* Catalog Price     */
           if tot_mat = 0 then tot_mat = unit_price * sale(kk%,1%)
           if tot_mat = 0 then tot_mat = sale(kk%,3%)
        return
                                           /* (EWD002) Check Error Log */
        check_error_log
           error_log% = 0%
        return   
           read #40,key = part$, using L62810, cst(), eod goto L62820
L62810:       FMT POS(64), 9*PD(14,4)
           error_log% = 1%
L62820: return

        update_error_log
           read #40,key = part$, eod goto L62845
        return
L62845:    e_qty% = 1.0
           x_qty = sls_qty
           x_cst = abs(tot_cst) * x_qty
           put #40, using L62875, part$, e_qty%, x_qty, err$(p_err%),     ~
                                 x_cst, cst()
           write #40, eod goto L62880
L62875:      FMT CH(25), BI(2), PD(14,4), CH(20), PD(14,4), 9*PD(14,4)
L62880: return

        create_report
           gosub select_printer
           part$ = " "
        create_rpt_nxt
           read #40,key > part$, using L62875, part$, e_qty%, x_qty,      ~
                          apc_err$, tot_cst, eod goto create_rpt_done
           gosub prt_detail
           goto create_rpt_nxt
        create_rpt_done
           print using L55040
           close printer
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#39,mode$, 500%, f2%)
            if f2% <> 0% then goto L63010

            call "WORKOPN2" (#40,mode$, 500%, f2%)
            if f2% <> 0% then goto L63020
        return
L63010:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCSLSW1)") : stop
        return
L63020:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCSLSWK)") : stop
        return
        delete_work
            call "FILEBGON" (#39)
            call "FILEBGON" (#40)
        return

        load_sale
           call "SHOSTAT" ("Loading Costing Tables")
           mat sale = zer
           init(" ") gencdkey$, sale$()
           str(gencdkey$,1%,9%)   = "COST SALE"
        load_sale_nxt
           read #9,key > gencdkey$, using L63095, gencdkey$, descr$,      ~
                                    eod goto load_sale_done
L63095:       FMT CH(24), CH(30)
           if str(gencdkey$,1%,9%) <> "COST SALE" then goto load_sale_done
              kk% = 1%
              convert str(gencdkey$,10%,3%) to kk%, data goto L63115
L63115:
              convert str(descr$,1%,8%)  to sale(kk%,1%), data goto L63130
L63130:
              convert str(descr$,11%,8%) to sale(kk%,2%), data goto L63140
L63140:
              convert str(descr$,22%,8%) to sale(kk%,3%), data goto L63150
L63150:
              sale(kk%,1%) = sale(kk%,1%) / 100.0
              sale(kk%,2%) = sale(kk%,2%) / 100.0
              str(sale$(kk%),1%,1%) = "*"       /* COST OF SALE EXISTS */
              goto load_sale_nxt
        load_sale_done
           gencdkey$ = " "
           str(gencdkey$,1%,9%) = "COST NONE"
        load_nocost_nxt
           read #9,key > gencdkey$, using L63095, gencdkey$, descr$,      ~
                                                eod goto load_nocost_done
           if str(gencdkey$,1%,9%) <> "COST NONE" then                   ~
                                                  goto load_nocost_done
              kk% = 0%
              convert str(gencdkey$,10%,3%) to kk%, data goto L63225
L63225:
              if kk% = 0% then goto load_nocost_nxt
              str(sale$(kk%),2%,1%) = "*"      /* SET DO NOT COST FLAG */
              goto load_nocost_nxt
        load_nocost_done
        return


        build_category
            init(" ") cat_key$, cat$, acct$

                                                             /* (EWD008) */
            if str(sav_sales_acct$,1%,2%) = "36" then goto L63230
               crd_err% = 1%
               acct$ = sav_sales_acct$
            return
L63230:     if inv_type$ <> "A" and inv_type$ <> "C" then goto L63250
               cat$  = sav_cat$
               acct$ = sav_sales_acct$
            if str(part$,1%,7%) = "6602 SU" or or_cat$ = "SURV"           ~
                                                       then crd_err% = 1%
            if acct$ = "3606-312" then goto L63380   /* (EWD009)        */
                                                     /* (EWD007) - Fix  */
               return
                                                     /* (EWD002) - Fix  */
L63250:     if len(or_cat$) < 4 then goto L63290     /* Not a Special   */
               cat_key$ = or_cat$                    /* Set for Special */
               goto L63340                           /*    
                                                     /* (EWD002) - Fix  */
L63290:                                              /* No Category     */
                                                     /* (EWD004)        */
            if str(part$,1%,7%) = "6602 SU" or or_cat$ = "SURV"           ~
                                                       then goto L63380
            if acct$ = "3606-312" then goto L63380   /* (EWD009)        */
                                                     /* (EWD004)        */     
            if len(part$) > 18 then goto L63300
               cat_key$ = "PART"                     /* Must be Part    */
               if str(part$,5%,4%) = "WARR" then cat_key$ = "WARR"
               pp% = pos(part$ = "W")
               if pp% = 0% then goto L63340
               if str(part$,pp%,4%) = "WARR" then cat_key$ = "WARR"
               goto L63340
                                                     /* Valid Part No   */
L63300:     pp% = pos( "456" = str(part$,11%,1%) )
            if pp% > 0% then cat_key$ = "PART"       /* Set Category    */
            if len(cat_key$) > 3 then goto L63340    /* Vaild Category  */
                                                     /* No Code Based   */ 
               str(cat_key$,1%,3%) = str(part$,1%,3%)/* on Model        */
               str(cat_key$,4%,1%) = "2"

L63340:     read #14,key = cat_key$, using L63350, cat$, acct$,           ~
                                                           eod goto L63375
L63350:        FMT CH(4), POS(35), CH(9)

            if str(acct$,1%,2%) <> "36" then crd_err% = 1%  /* (EWD008) */
        return                          /* (EWD002) - Check Category   */
L63375:     cat$  = "MISC"                          /* Default Category*/
            acct$ = "3615-313"                      /* Code for Invalid*/
        return
L63380:     cat$    = "SURV"            /* (EWD004) - Surviver By Back */
            acct$   = "6214-210"
            slsman$ = "2100"
        return                          /* (EWD004) - Surviver         */ 

        check_howship
            init(" ") gencdkey$, or_hows$, or_cat$
            if inv_type$ = "C" or inv_type$ = "A" then return
                                                    /* (EWD002) - Fix  */
            str(gencdkey$,1%,9%)   = "PLAN HOWS"
            str(gencdkey$,10%,15%) = str(hows$,1%,2%)
            read #9,key = gencdkey$, using L63420, descr$,eod goto L63465
L63420:        FMT POS(25), CH(30)
            str(hows$,3%,18%) = "/" & descr$
            or_hows$ = str(hows$,1%,2%)
            if or_hows$ = "02" or or_hows$ = "04" or or_hows$ = "06" or  ~
               or_hows$ = "22" then or_cat$ = "SAMP"
            if or_hows$ = "03" or or_hows$ = "05" then or_cat$ = "DISP"
            if or_hows$ = "30" then or_cat$ = "SALV"
            if or_hows$ = "31" then or_cat$ = "SCRA"
        return
L63465:    if str(hows$,1%,4%) <> "SAMP" then goto L63475
              or_cat$ = "SAMP" : return                     /* SAMPLES */
L63475:    if str(hows$,1%,4%) <> "DISP" then goto L63485
              or_cat$ = "DISP" : return                     /* DISPLAY */
L63485:     hows$ = "00/Our Truck"
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            if cst_err% <> 0% then close printer
            gosub delete_work
            call "SHOSTAT" ("One Moment Please")

            end