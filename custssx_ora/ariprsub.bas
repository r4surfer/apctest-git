        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   AAA   RRRR   IIIII  PPPP   RRRR    SSS   U   U  BBBB    *~
            *  A   A  R   R    I    P   P  R   R  S      U   U  B   B   *~
            *  AAAAA  RRRR     I    PPPP   RRRR    SSS   U   U  BBBB    *~
            *  A   A  R   R    I    P      R   R      S  U   U  B   B   *~
            *  A   A  R   R  IIIII  P      R   R   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIPRSUB - Prints customer invoices based on the data     *~
            *            passed from the calling program.               *~
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
            * 10/14/86 ! Original                                 ! JIM *~
            * 04/01/87 ! Corrected Tax Base Print Variable        ! HES *~
            * 05/06/87 ! Added Serial Number printing.            ! JIM *~
            * 05/12/87 ! Removed Extra Space Between Lines w/Text ! HES *~
            * 10/26/87 ! Now prints Sales Tax to three places     ! HES *~
            * 12/11/87 ! ARIMASTR Alt keys.                       ! JIM *~
            * 12/22/87 ! Mod to print in specified currency       ! MJB *~
            * 08/01/88 ! Invoice totals expressed in tran currency! JIM *~
            * 11/17/88 ! Price now not formatted via CURRFMT      ! JDH *~
            * 05/18/89 ! DISCOUNT$ & LINEXTENSION$ now print on   ! MLJ *~
            *          !   2nd detail line of invoice.            !     *~
            * 11/09/89 ! Added SYSFILE2 checking multi-currency   ! MLJ *~
            *          !  usage instead of currency in ARIMASTR,  !     *~
            *          !  also formatted 9 char zip codes.        !     *~
            * 11/06/91 ! PRRs 10356,11272. Honor ARMTERMS Flag for! JDH *~
            *          !  Printing Terms Description on Invoice.  !     *~
            * 06/03/92 ! Added SO text as a possible source for   ! JDH *~
            *          !  printing.                               !     *~
            * 11/10/92 ! Brought Core Deposit Module to R6.02.03. ! JIM *~
            * 03/30/93 ! Fixed bad branch just above REG_LINE.    ! JDH *~
            * 12/08/94 ! PRR 13323.  Price printed honoring flag  ! JDH *~
            *          !  pertaining to discounts.                !     *~
            *          ! PRR 13327.  Fixed rpt id sent to text prt!     *~
            * 03/14/95 ! PRR 13187 - Print Xref Parts if exist &  ! RJH *~
            *          !   BCK Flags indicate to print.           !     *~
            * 10/24/95 ! Fix image lines #62400& #62600. (EWD)    ! RJH *~
            * 11/09/90 ! APC Mods for Printing Part Description.  ! RHH *~
            *          ! Lines - 2099,(2870-2900),(11261-11287)   ! RHH *~
            *          !         11240, (63210-63420) BUILD_PRINT ! RHH *~
            * 11/04/92 ! MODS FOR CREDIT MEMO - (12041-12053)     ! RHH *~
            * 10/12/93 ! MOD - PUT STORE SKU NUMBER ON INVOICE FOR! RHH *~
            *          !   EACH PART NUMBER                       !     *~
            * 02/09/94 ! MOD - PUT Sku Number from S.O. Line Item ! RHH *~
            *          !   Text on the Invoice for Only HQ        !     *~
            *          !   (BUILD_PRINT), (LOOKUP_CUSTOMER),      !     *~
            *          !   (LOOKUP_SKU), (LOOKUP_SKU_TT) (EWD)    !     *~
            * 03/31/98 ! Mods of (Y2K)                            ! RHH *~
            * 11/10/98 ! (EWD001) Mods for Private Label          ! RHH *~
            * 11/30/98 ! (EWD002) Mod to put job name on Invoice  ! RHH *~ 
            * 05/08/00 ! (EWD003) Mod for faxing of Invoices      ! CMG *~
            * 01/31/02 ! (EWD004) Mod to put load on Invoice      ! TBM *~
            * 07/19/02 ! (EWD005) Mod to get desc from oracle     ! TBM *~
            * 06/16/04 ! (EWD006)Mod to fix qty on invoice when   ! CMG *~
            *          !         part of order is backordered     !     *~
            *03/09/2013! (AWD007) mod for energy surcharge        ! CMG *~
            *04/10/2015! (IM3628) mod to add approval code.       ! PWW *~
            *06/03/2015! (SR64269) mod to expand qty to 3 digits  ! PWW *~
            *02/13/2018! (CR784) mod to chk ORA config=2 no price ! RDB *~
            *04/24/2019! (CR1993) mod to add PlyGem PO to report  ! RDB *~
            *08/20/2021! CR2885 mod energy surcharge to surcharge ! RDB *~
			*01/23/2023! CR3226 add credit card service wording   ! RDB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARIPRSUB" (dup%, prt%, abend%, cust_code$, invoice$, rptid$, ~
                        inv_no$)

                /* DUP% equal to 1 indicates a duplicate invoice       */
                /* PRT% is set to 1 if any invoice is printed          */
                /* ABEND% is set to 1 if errors suggest 'ABORT'        */
                /* CUST_CODE$ is the passed customer code              */
                /* INVOICE$ is the associated Invoice # to print       */
                /* RPTID$ is the passed report ID                      */
/* (EWD003)        INV_NO$ indicates whether or not the inv. was faxed */

        REM *************************************************************~
            *This subroutine will print a customer invoice based on the *~
            *customer code and invoice number passed to it.             *~
            *************************************************************

        dim                              /* (EWD) - Begin              */~
            tid$4,                       /* S.O. Line Item Text Id     */~
            tt_key$19,                   /* S.O. Line Item Key         */~
            sav_tt$11, tid_key$11,       /* Text Id Key- S.O. Line Item*/~
            tt$15,                       /* 1st 15 Char of Text - Sku# */~
            apc$(3%)30,                  /* PRINT DESCRIPTION TEXT     */~
            apc_scr$120,                 /* SCREEN DESCRIPTION TEXT    */~
            apc_prt$60,                  /* PRINT DESCRIPTION TEXT     */~
            apc_sze$20,                  /* SIZE DESCRIPTION TEXT      */~
            sku_code$3,                  /* CUSTOMER SKU CODE          */~
            sku_key$28,                  /* SKU LOOKUP KEY             */~
            sku_no$25,                   /* CUSTOMER SKU NUMBER(EWD)   */~
            settle$12,                   /* SETTLEMENT/INVOICE NO -    */~
            inv_reason$9,                /* INVOICE REASON CODE -      */~
            reason$20,                   /* REASON DESCRIPTION -(EWD)En*/~
            amtdue$10,                   /* Edited amount due          */~
            arilines_key$20,             /* Key to ARILINES            */~
            arimastr_key$17,             /* Key to ARIMASTR            */~
            arimscur_key$17,             /* Key to ARIMSCUR            */~
            bck_key$25, bck_job$20,      /* Job Name          (EWD002) */~
            apc_key$8, apc_load$5,       /* Load Number       (EWD004) */~
            bol$3,                       /* Bill of lading fr ARIMASTR */~
            crmemo$24,                   /* Credit memo text           */~
            currency$4, currdesc$32,     /* Currency code              */~
            curr$1,                      /* Multi-currency usage flag  */~
            cust_code$9,                 /* Customer code from ARIINVRF*/~
            desc$57,                     /* Descriptions for totals    */~
            disc$7,                      /* Edited percentages         */~
            discount$10,                 /* Edited line discount amount*/~
            discsw$1,                    /* Controls discount functions*/~
            drcuscode$9,                 /* CORDRMAS provided data for */~
            drinvcenbr$8,                /*          drop-off invoices */~
            drinvcelin$3,                /*                            */~
            drinvcedat$8,                /*                            */~
            drpurchord$16,               /*                            */~
            drpurchlin$3,                /*                            */~
            drpurchdat$8,                /*                            */~
            drpartcd$25,                 /* Core Part # (CORDRMAS)     */~
            drpartds$32,                 /* Core Part Description      */~
            duplicate$24,                /* Duplicate invoice text     */~
            fmttotal$15,                 /* Formatted total amount     */~
            fob$20,                      /* F.O.B. from ARIMASTR       */~
            frt_bill$20,                 /* Frt bill from ARIMASTR     */~
            how_ship$20,                 /* How to ship from ARIMASTR  */~
            inv_date$8,                  /* Invoice date from ARIMASTR */~
            inv_print$1,                 /* Print ARMTERMS Descr?      */~
            inv_type$1,                  /* Invoice type from ARIMASTR */~
            invoice$8,                   /* Invoice # from ARIINVRF    */~
            mask$85,                     /* Text mask                  */~
            lineid$4,                    /* ARILINES Line ID for S/N's */~
            linextension$12,             /* Formatted line extension   */~
            nonstock$1,                  /* Line Non-stocked flag      */~
            part_nmbr$25,                /* Part number from ARILINES  */~
            part_desc$32,                /* Part descr from ARILINES   */~
            po$16,                       /* PO number from ARIMASTR    */~
            pgpo$20,                     /* PlyGem PO number           */~ 
            price$10,                    /* Edited unit price          */~
            price_uom$4,                 /* Pricing UOM from ARILINES  */~
            prog$8,                      /* Program name               */~
            readkey$100,                 /* General purpose read var   */~
            qty_ordr$10,                 /* Edited open quantity       */~
            qty_ship$10,                 /* Edited order quantity      */~
            rptid$6,                     /* Report ID from caller      */~
            seqnr$3,                     /* ARI Sequence Number        */~
            ser$6,                       /* Serial # literal           */~
            serprint$76,                 /* Serial # print area        */~
            serial$20,                   /* Serial # from SERMASTR     */~
            serkey$99,                   /* SERMASTR key               */~
            ship_to$(6%)31,              /* Ship to from ARIMASTR      */~
            so$16,                       /* Sales Order # from ARIMASTR*/~
            so_seq$4,                    /* Sales Order Line Seq #     */~
            sold_to$(6%)31,              /* Sold to from ARIMASTR      */~
            stax$12,                     /* Edited sales tax base amt  */~
            stock_uom$4,                 /* Stocking UOM from ARILINES */~
            stor_code$3,                 /* Store # from ARIMASTR      */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code   (EWD001) */~
            s_so$8,                      /* Sales Order       (EWD001) */~
            s_ln$3,                      /* Sales Order Line  (EWD001) */~
            s_prv$30,                    /* Private Label Name(EWD001) */~
            s_1$2,                       /* Private Label Code(EWD001) */~  
            taxable$3,                   /* Taxable code description   */~
            term_amt(30%),               /* Terms amount due (ARIMASTR)*/~
            term_code$20,                /* Terms code from ARIMASTR   */~
            term_date$(30%)8,            /* Terms date from ARIMASTR   */~
            term_desc$41,                /* Terms descripion           */~
            term_hdr$31,                 /* Terms descripion (header)  */~
            term_net$(30%)8,             /* Terms net date fr ARIMASTR */~
            term_pct(30%),               /* Terms discount % (ARIMASTR)*/~
            txbl$1,                      /* Taxable code from ARILINES */~
            txtid$4,                     /* Text ID- ARIMASTR/ARILINES */~
            xref_cus$1,                  /* Print Customer Xref Part   */~
            xref_mnf$1,                  /* Print Manufactur Xref Part */~
            xref_descr$32,               /* Xref Part # Description    */~
            xref_part$25,                /* Xref Part Number           */~
            xref_type$1                  /* Xref Part Type(Cust or Mnf)*/

        dim ccdesc$83                    /* CR3226 */

        dim f2%(32%),                    /* = 0 if the file is open    */~
            f1%(32%),                    /* = 1 if READ was successful */~
            fs%(32%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32%)20                 /* Text from file opening     */

        dim inv_no$10                    /* Invoice Message   (EWD003) */
                                                                              /*(EWD005)*/
        dim approval_need$1, approval_code$20, approval_code_d$13,       ~
            quote_num$10                                      /*IM3628*/
        dim                                                              ~
            a$31,                        /* Print Description          */~
            userid$3,                    /* User ID                    */~     
            error$256,                   /* Error String               */~
            ora_seqnr$3,                 /* Oracle Sequence number     */~
            sv_seqnr$3,                  /* Save Oracle Seq Number     */~
            orapart_desc$250,            /* Oracle Part description    */~
            oraqty_ordr$10,              /* Oracle Qty ordered         */~
            oraqty_ship$10,              /* Oracle qty shipped         */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            field$256,                   /* Query Return Field         */~
            fields$(4%)100,              /* String of Oracle Info      */~
            stmt1$250,                   /* First query String         */~
            stmt2$250                    /* Second Query String        */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #2  ! SYSFILE2 ! CMS General Information                  *~
            * #4  ! ARIMASTR ! Invoice Master file                      *~
            * #5  ! ARILINES ! Invoice Line Items file                  *~
            * #6  ! TXTFILE  ! SYSTEM TEXT FILE                         *~
            * #7  ! HNYMASTR ! Inventory Master File                    *~
            * #8  ! ARMTERMS ! A/R Payment Terms                        *~
            * #9  ! SERMASTR ! Serial Number Tracking Master File       *~
            * #10 ! ARILNCUR ! Currency-specific ARI lines              *~
            * #11 ! CURMASTR ! Multi-Currency Master file               *~
            * #12 ! ARIMSCUR ! Currency-specific ARI Master             *~
            * #13 ! AMTBOMIF ! BOM GENERATOR MASTER VALIDITY FILE       *~
            * #14 ! BCKMASTR ! Backlog master file                      *~
            * #15 ! BCKLINES ! Back Log Line Item File                  *~
            * #16 ! CUSTOMER ! Customer Master File                     *~
            * #17 ! GENCODES ! MASTER TABLE FILE                        *~
            * #18 ! APCSKUNO ! SKU NUMBER MASTER FILE  (EWD)            *~
            * #20 ! CORDRMAS ! Core Bank Debit Master File              *~
            * #21 ! APCPLNOR ! New Planning S.O. Header                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen = 20

            select #4,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                    alt key 1, keypos =  10, keylen =  8, dup,           ~
                        key 2, keypos =  18, keylen = 16, dup,           ~
                        key 3, keypos =  34, keylen = 16, dup,           ~
                        key 4, keypos = 1783, keylen = 26

            select #5,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =   1, keylen =  20

            select #6,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #7, "HNYMASTR", varc, indexed, recsize = 900,         ~
                        keypos =   1, keylen =  25,                      ~
                    alt key 1, keypos = 102, keylen =  9, dup,           ~
                        key 2, keypos =  90, keylen =  4, dup

            select #8, "ARMTERMS", varc, indexed, recsize = 100,         ~
                        keypos =   1,  keylen = 20

            select #9, "SERMASTR", varc, indexed, recsize = 300,         ~
                keypos = 52, keylen = 45,                                ~
                alt key  1, keypos = 32, keylen = 45,                    ~
                    key  2, keypos =  1, keylen = 76

            select #10, "ARILNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   5,  keylen = 20,                      ~
                        alt key  1, keypos =   1, keylen =  24

            select #11, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #12, "ARIMSCUR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =   5,  keylen = 17,                      ~
                        alt key  1, keypos =   1, keylen =  21

            select #13, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =  1,   keylen =  32

            select #14,  "BCKMASTR",                                     ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #15,  "BCKLINES",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #16, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30,         ~
                            key 2,  keypos = 424,  keylen =   9,         ~
                            key 3,  keypos = 771,  keylen =   9,         ~
                            key 4,  keypos = 780,  keylen =   9

            select #17, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #18, "APCSKUNO",                                      ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #20, "CORDRMAS",                                      ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  4, keypos =   61, keylen =  40, dup,    ~
                            key  3, keypos =   36, keylen =  34, dup,    ~
                            key  2, keypos =   19, keylen =  11,         ~
                            key  1, keypos =    1, keylen =  29

            select #21, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos = 1,  keylen = 51,                        ~
                        alt key 1,  keypos = 27,  keylen = 25,           ~
                            key 2,  keypos = 70,  keylen =  8,           ~
                            key 3,  keypos = 78,  keylen =  8,           ~
                            key 4,  keypos = 52,  keylen =  8,           ~
                            key 5,  keypos = 36,  keylen = 16

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            * Initialization code is done one time only.                *~
            *************************************************************

            if been_here_before% <> 0% then goto L10000
            been_here_before% = 1%
            abend% = 1% /* Abnormal terminate if errors during OPENs */
                rslt$(4 ) = "REQUIRED"
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
                if fs%(4) < 0 then exit_program
                rslt$(5 ) = "REQUIRED"
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
                if fs%(5) < 0 then exit_program
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
                curr$ = "N"
                call "READ100" (#2, "SWITCHS.CUR", f1%(2))
                if f1%(2) <> 0% then get #2 using L09137, curr$
L09137:            FMT POS(21), CH(1)

*        Try to enable the Core Deposit Drop-Off Invoice scheme.
                call "READ100" (#2, "SWITCHS.COR", core_track%)
                if core_track% <> 0% then call "OPENCHCK" (#20, fs%(20%),~
                     f2%(20%), 0%, rslt$(20%))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 0%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))/*(EWD)*/
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15), 0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16), 0%, rslt$(16))
            call "OPENCHCK" (#17, fs%(17), f2%(17), 0%, rslt$(17))/*(EWD)*/
            call "OPENCHCK" (#18, fs%(18), f2%(18), 0%, rslt$(18))/*(EWD)*/
            call "OPENCHCK" (#21, fs%(21), f2%(21), 0%, rslt$(21))

            call "EXTRACT" addr("CF", prog$)
            select printer (87)
            call "SETPRNT" (rptid$, prog$, 0%, 0%)

            max_lines% = 35%
            call "BCKSWTCH" ("BCK", "DISCS   ", discsw$, disc1or2, comp%)
            discsw$ = "N"
            if disc1or2 = 2 then discsw$ = "Y"

            call "READ100" (#02, "SWITCHS.BCK", f1%(02%))
            if f1%(02%) = 1% then get #02 using L09340, xref_cus$, xref_mnf$
L09340:         FMT POS(63), CH(1), CH(1)

        REM CHECK XREF PRINT SETTINGS
            call "READ100" (#16, cust_code$, f1%(16%))
            if f1%(16%) = 0% then L09980     /* Shouldn't happen */
            get #16 using L09400 , temp_cus$, temp_mnf$
L09400:         FMT POS(1092), CH(1), CH(1)

            if temp_cus$ = " " then L09450
                if pos("YNB" = temp_cus$) <> 0% then xref_cus$ = temp_cus$

L09450:     if temp_mnf$ = " " then L09980
                if pos("YNB" = temp_mnf$) <> 0% then xref_mnf$ = temp_mnf$

L09980:     call "SHOSTAT" ("Printing Customer Invoices")

            call "EXTRACT" addr("ID", userid$)            /*  (EWD005)  */
L10000: REM *************************************************************~
            *             M A I N   P R O G R A M   L O O P             *~
            *-----------------------------------------------------------*~
            *  The following code is executed once for each call of this*~
            *  subroutine -- which is equivalent to a single invoice.   *~
            *************************************************************

            config%, end_config% = 0%                     /*  (EWD005)  */
            tt_key$ = " "                          /* (EWD) - Begin */
            abend% = 0% /* Indicate normal (no error) processing    */
            invtaxable = 0
/*CR1993*/  pgpo$ = " "
            currdesc$, duplicate$ = " "
            if dup% <> 0% then duplicate$ = "THIS IS A DUPLICATE COPY"

            arimscur_key$, arimastr_key$ = str(cust_code$,,9) & invoice$
            call "READ100" (#4, arimastr_key$, f1%(4))
            if f1%(4) = 0% then goto exit_program
            get #4 using L10230, po$, so$, bol$, ship_to$(), sold_to$(),  ~
                how_ship$, fob$, frt_bill$, inv_date$, invgrossamt,      ~
                invdiscpct, invdiscamt, invfrtamt, invstaxamt, invnetamt,~
                settle$,                                                 ~
                stor_code$, invstaxpct, inv_type$,                       ~
                inv_reason$, txtid$, term_code$,                         ~
                term_amt(), term_pct(), term_date$(), term_net$(),       ~
                currency$, escamt,                                       ~
/* CR1993 */    taxrate, pgpo$, hdmet, hdidx

             str(tt_key$,1%,16%) = so$

             gosub lookup_job_name                 /* (EWD002)         */ 
REM             gosub oracle_connect                  /* (EWD005)         */
             gosub lookup_load                    /*  (EWD004)         */ 
             
        REM Partial record layout for file 'ARIMASTR' *******************
L10230:         FMT  POS(18), CH(16),    /* Purchase Order number      */~
                     POS(34), CH(16),    /* Sales Order number         */~
                     POS(50), CH( 3),    /* Bill of lading             */~
                     POS(53), 6*CH(30),  /* Ship-to name & address     */~
                     POS(233), 6*CH(30), /* Sold-to name & address     */~
                     POS(419), CH(20),   /* How shipped (shipped via)  */~
                     POS(439), CH(20),   /* FOB                        */~
                     POS(481), CH(20),   /* Freight bill number        */~
                     POS(521), CH( 6),   /* Invoice date               */~
                     POS(793), PD(14,4), /* Gross invoice amount       */~
                     POS(801), PD(14,4), /* Invoice discount percent   */~
                     POS(809), PD(14,4), /* Invoice discount amount    */~
                     POS(817), PD(14,4), /* Freight amount             */~
                     POS(825), PD(14,4), /* Sales tax amount           */~
                     POS(833), PD(14,4), /* Net invoice amount         */~
                     POS(858), CH(12),   /* SETTLEMENT CODE -      APC */~
                     POS(870), CH( 3),   /* Store code                 */~
                     POS(883), PD(14,4), /* Sales tax percent          */~
                     POS(891), CH( 1),   /* Invoice Type               */~
                     POS(892), CH(9),    /* INVOICE REASON CODE - (EWD)*/~
                     POS(901), CH( 4),   /* Text ID (header)           */~
                     POS(908), CH(20),   /* Terms code                 */~
                     POS(928), 30*PD(14,4),/* Terms amounts due        */~
                     POS(1168), 30*PD(14,4),/* Terms cash discount pct */~
                     POS(1408), 30*CH(6),/* Terms discount due dates   */~
                     POS(1588), 30*CH(6),/* Terms net due              */~
                     POS(1779), CH(4),   /* Currency code              */~
                     POS(1819), PD(14,4),/* Energy Surcharge (AWD007)  */~
                     POS(1836), PD(14,4), CH(20), PD(14,4), PD(14,4) /* CR1993*/
                                         /* (EWD) - End                */

            if currency$ = " " or curr$ <> "Y" then goto L10530
                call "DESCRIBE" (#11, currency$, currdesc$, 1%, f1%(11))
                if currdesc$ = " " then currdesc$ = "(" & currency$ & ")"

L10530: REM Now replace invoice totals with transaction currency amounts,~
            if header record exists in ARIMSCUR.
            if curr$ <> "Y" then L10630
               call "READ100" (#12, arimscur_key$, f1%(12))
                   if f1%(12) = 0% then goto L10630
               get #12 using L10600, invgrossamt, invdiscamt, invfrtamt,  ~
                   invstaxamt, invnetamt, term_amt()

L10600:         FMT POS(22), 5*PD(14,4), POS(92), 30*PD(14,4)

L10630:     prt% = 1% /* Indicate print activity */
            call "DATEFMT" (inv_date$)
            crmemo$ = " "
            if inv_type$ <> "C" then L10670
               reason$ = " "
               crmemo$ = ">>>>> CREDIT MEMO <<<<<"
               readkey$ = all(hex(00))
               readkey$ = "INVREASON" & inv_reason$
               call "DESCRIBE" (#17, readkey$, reason$, 0%, f1%(17%))

L10670:     term_hdr$ = "SEE SCHEDULE BELOW"
            if term_code$ = "DATED" then goto smasher
                amtdue$, term_desc$ = " " : t% = 1%
                call "READ100" (#8, term_code$, f1%(8))
                if f1%(8) <> 1% then L10700
                     get #8 using L10694, term_desc$, inv_print$
L10694:                   FMT POS(21), CH(30), POS(77), CH(1)
                     if inv_print$ = "Y" then L10710
                term_desc$ = " "
L10700:         gosub term_descriptor
L10710:         call "STRING" addr("LJ", term_desc$, 41%)
                term_hdr$ = term_desc$
        smasher
            if str(ship_to$(6),17,1) <> " " or str(ship_to$(6),16,1)     ~
               <> " " or pos(str(ship_to$(6),27,4) = " ") > 0% then L10736
            temp$ = str(ship_to$(6),27,4)
               str(ship_to$(6),28,4) = temp$
               str(ship_to$(6),27,1) = "-"
L10736:           call "LINSMASH" (ship_to$())
            if str(sold_to$(6),17,1) <> " " or str(sold_to$(6),16,1)     ~
               <> " " or pos(str(sold_to$(6),27,4) = " ") > 0% then L10748
            temp$ = str(sold_to$(6),27,4)
               str(sold_to$(6),28,4) = temp$
               str(sold_to$(6),27,1) = "-"
L10748:           call "LINSMASH" (sold_to$())

/*IM3628 +  */
            init(" ") stmt1$, approval_need$, approval_code$, approval_code_d$
            str(stmt1$,1%,40%)   = "SELECT APPROVALNEEDED, APPROVALCODE FROM"
            str(stmt1$,41%,40%)  = " ORDERMASTER WHERE ORDERID = " &     ~
                                   str(quote_num$,2%, 9%)

            gosub oracle_flush
            gosub oracle_query                             /*Tina*/
            gosub oracle_fetch
            if oci_err% > 0 or oci_err% < -1 then goto skip_approval
            field_num% = 1%
            gosub oracle_getfield
            approval_need$ = str(field$,1%, 1%)
            if approval_need$ <> "1" then skip_approval
            gosub oracle_flush
            gosub oracle_query                             /*Tina*/
            gosub oracle_fetch
            if oci_err% > 0 or oci_err% < -1 then goto skip_approval
            field_num% = 2%
            gosub oracle_getfield
            approval_code$ = str(field$,1%,20%)
            
        skip_approval

/*IM3628 -  */
            page_nbr% = 0%
            gosub page_heading
            txt% = (page_nbr% * 100%) + nbr_lines%
            gosub bckmastr_text
            gosub'200(txtid$)
            if txt% = (page_nbr% * 100%) + nbr_lines% then L10830
               REM Text printed, so add blank line after it...
               print : nbr_lines% = nbr_lines% + 1%
L10830:     arilines_key$ = key(#4) /* Customer code and Invoice number*/
            str(arilines_key$,18) = all(hex(00))


            init(" ") stmt1$, stmt2$, txtid$                        /*(EWD005) */
            str(stmt1$,1%,24%) = "CALL MSSQL.RPT_DISPLAY('"
            str(stmt1$,25%,20%) = str(so$,1%,8%) & "', '" & userid$ & "')"
            gosub oracle_flush
            gosub oracle_exec

REM            str(stmt1$,1%,40%)   = "SELECT * FROM MSSQL.DISPLAY WHERE SALESO"
REM            str(stmt1$,41%,30%)  = "RDER = '"&str(so$,1%,8%)&"' AND USERID ="
REM            str(stmt1$,71%,32%)  = "'"&userid$&"' ORDER BY DISPLAYORDER ASC,"
REM            str(stmt1$,103%,30%)  = "LINENUMBER ASC, CONFIG DESC"

            init(" ") stmt1$
            str(stmt1$,1%,40%)   = "SELECT * FROM MSSQL.DISPLAY WHERE SALESO" 
            str(stmt1$,41%,26%)  = "RDER = '" & str(so$,1%,8%) & "' AND USER"
            str(stmt1$,67%,27%)  = "ID = '" & userid$ & "' ORDER BY DISPLAY"
            str(stmt1$,94%,40%)  = "ORDER ASC, LINENUMBER ASC, CONFIG DESC, "
            str(stmt1$,134%,15%) = "UNITID ASC   "


            gosub oracle_flush
            gosub oracle_query                             /*Tina*/


        arilines_loop
            if config% <> 0% then goto line_continue
            gosub oracle_fetch
            if oci_err% > 0 or oci_err% < -1 then goto line_config
        line_continue
            gosub load_newline                                   /*(EWD005)*/
        line_config
            if end_config% = 1% then goto L11000
            call "PLOWNEXT" (#5, arilines_key$, 17%, f1%(5))
            if f1%(5) = 0% then goto end_of_lines
            get #5 using L10940, seqnr$, part_nmbr$, part_desc$, qty_ordr,~
                qty_ship, stock_prc, stock_uom$, price_uom$, disc,       ~
                lindiscamt, linextension, txbl$, txtid$, so_seq$,        ~
                nonstock$, lineid$

        if config% = 1% then return

        REM Partial record layout for file 'ARILINES' *******************
L10940:         FMT  POS(18), CH( 3),    /* ARI Sequence Number        */~
                     POS(24), CH(25),    /* Part number                */~
                     POS(49), CH(32),    /* Part description           */~
                     POS(85), PD(14,4),  /* Original order quantity    */~
                     POS(93), PD(14,4),  /* Quantity shipped           */~
                     POS(109), PD(14,4), /* Unit price @ stocking      */~
                     POS(117), CH( 4),   /* Stocking UOM               */~
                     POS(121), CH( 4),   /* Pricing UOM                */~
                     POS(141), PD(14,4), /* Line discount %            */~
                               PD(14,4), /* Line discount amount       */~
                     POS(157), PD(14,4), /* Line extension             */~
                     POS(165), CH( 1),   /* Taxable code               */~
                     POS(190), CH( 4),   /* Text ID (detail line)      */~
                               CH( 4),   /* SO Line Seq. Number        */~
                     POS(617), CH( 1),   /* Non-stocked Flag           */~
                     POS(626), CH( 4)    /* S/N Line ID                */

L11000:     end_config% = 0%             /* Don't get next caelus line!! */
REM            if qty_ship = 0 then goto arilines_loop
            call "READ100" (#10, arilines_key$, f1%(10))
            if f1%(10) <> 0% then get #10 using L11100, stock_prc,        ~
                lindiscamt, linextension
L11100:         FMT POS(25), PD(14,4), POS(41), 2*PD(14,4)
REM ????
            if so$ = " " then qty_ordr = qty_ship
            orapart_desc$ = str(fields$(), 32%,250%)                       /*(EWD005)*/
            convert str(fields$(), 26%,3%) to oraqty_ordr, data goto L11115   
L11115:
            convert str(fields$(), 29%,3%) to oraqty_ship, data goto L11120   
L11120:

            if str(fields$(),4%,1%) = "1" then goto L11130
            if discsw$ = "Y" then L11130
                stock_prc = stock_prc - ((stock_prc * disc) / 100)
L11130:     call "CONVERT" (qty_ordr, 2.0, qty_ordr$)
            call "CONVERT" (qty_ship, 0, oraqty_ship$)   /* (EWD006)  */
REM            call "SHOSTAT" (" QTY SHIP " & oraqty_ship$)  stop
            call "CONVERT" (stock_prc, 2.4, price$)
            call "CONVERT" (qty_ordr, 0, oraqty_ordr$)
REM            call "SHOSTAT" (" QTY ORDR " & oraqty_ordr$)  stop
REM            call "CONVERT" (oraqty_ship, 0, oraqty_ship$)
                                                                           /*(EWD005)*/
            apc_prt$ = str(fields$(),282%,50%)
            if curr$ = "Y" then call "CURRFMT"                           ~
                       (linextension, currency$, linextension$, "N")     ~
               else call "CONVERT" (linextension, 2.2, linextension$)
            discount$ = " "
            if discsw$ = "N"  then L11200
            if lindiscamt = 0 then L11200
               if curr$ = "Y" then call "CURRFMT"                        ~
                          (lindiscamt, currency$, discount$, "N")        ~
                  else call "CONVERT" (lindiscamt, 2.2, discount$)

L11200:     taxable$ = "NO"
            if txbl$ <> "Y" then goto L11232
                taxable$ = "YES"
                invtaxable = invtaxable + linextension

L11232:      /* Get Xref Part Number, if available */
             call "PTUSEDSB" ( "R", "ARI ",                              ~
                              str(arilines_key$,,17%), seqnr$,           ~
                              xref_part$, xref_descr$, xref_type$, ret%)
             if ret% = 0% then xref_part$,xref_descr$,xref_type$ = " "
                                    /* EWD MOD - 2% FOR 4%         */
            if max_lines% - nbr_lines% < 4% then gosub page_heading
            if so$ <> " " and oci_err% <= 0% and oci_err% > -2%          ~
                                   then gosub print_desc     /*(EWD005)*/
            if so$ = " " or oci_err% > 0% then goto L11240
            if so$ = " " or oci_err% < -1% then goto L11240
            if str(fields$(),4%,1%) = "0" then goto L11350
               config% = 1%
               goto get_config                                         /*(EWD005)*/

L11240:     if xref_part$ = " " then L11250
            if xref_type$ = "M" and xref_mnf$ <> "Y" then L11250
            if xref_type$ = "C" and xref_cus$ <> "Y" then L11250
            if xref_type$ <> "C" and xref_type$ = "M" then L11250
                part_nmbr$ = xref_part$ : part_desc$ = xref_descr$

L11250:     print using L62600,part_nmbr$,stock_uom$,qty_ordr$,price_uom$
            nbr_lines% = nbr_lines% + 2%                   /* Trust Me */

L11350:     if core_track% = 0% then reg_line
            if nonstock$ <> "Y" then reg_line
            call "READ100" (#7, part_nmbr$, f1%(7))
                if f1%(7) <> 0% then reg_line
            call "READ100" (#20, str(part_desc$, 4%, 20%), f1%(20%))
                if f1%(20) = 0% then reg_line

            print using L62400, " ",        taxable$, qty_ship$, price$,  ~
                               discount$, linextension$

            init (" ") drpartcd$, drpartds$, drpurchord$, drpurchlin$,   ~
                drpurchdat$
            get part_desc$ using L11421, drcuscode$, drinvcenbr$,         ~
                                        drinvcelin$, drinvcedat$
L11421:         FMT POS(4), CH(9), CH(8), CH(3), CH(6)
            call "DATEFMT" (drinvcedat$)
            get #20 using L11461, drpartcd$, drpurchord$, drpurchlin$,    ~
                                 drpurchdat$
L11461:         FMT POS(36), CH(25), POS(126), CH(16), CH(3), CH(6)
            call "DATEFMT" (drpurchdat$)
            call "DESCRIBE" (#07, drpartcd$, drpartds$, 0%, f1%(7))
            if max_lines% - nbr_lines% < 3% then gosub page_heading
            print
            if str(arilines_key$,,9%) = str(drcuscode$,,9%) then         ~
               print using L63210, drpartcd$, "#", drinvcenbr$,           ~
                                  drinvcelin$, drinvcedat$               ~
            else                                                         ~
               print using L63230, drpartcd$, drcuscode$, drinvcenbr$,    ~
                                  drinvcelin$, drinvcedat$

            if drpurchdat$ <> " " then L11681

            if drpurchlin$ <> " " then                                   ~
               print using L63250, drpartds$, "#", drpurchord$, "/",      ~
                                  drpurchlin$, " ", " "                  ~
            else                                                         ~
               print using L63250, drpartds$, "#", drpurchord$, " ",      ~
                                  " ", " ", " "
            goto L11751

L11681:     if drpurchlin$ <> " " then                                   ~
               print using L63250, drpartds$, "#", drpurchord$, "/",      ~
                                  drpurchlin$, "DATE:", drpurchdat$      ~
            else                                                         ~
               print using L63250, drpartds$, "#", drpurchord$, " ",      ~
                                  " ", "DATE:", drpurchdat$

L11751:     nbr_lines% = nbr_lines% + 3%
            goto L13000

        reg_line                           /* (EWD) MOD - PRINT DESCRIPTION */
            if so$ <> " " and oci_err% <= 0% and oci_err% > -2% then goto L13000
            gosub build_print
            if err% = 0% then goto L11755
              print using L62400, part_desc$, taxable$, qty_ship$, price$,~
                                 discount$, linextension$
              nbr_lines% = nbr_lines% + 2%
              goto L13000
L11755:     print using L62400, apc$(1), taxable$, qty_ship$, price$,     ~
                                 discount$, linextension$
            print using L62400, apc$(2), " ", " ", " ", " ", " "
            print using L62400, apc$(3), " ", " ", " ", " ", " "
            nbr_lines% = nbr_lines% + 4%
                                    /* (EWD) - Mod End               */  
L13000:     gosub xref_print       /* Print cross reference part number */
            gosub serial_number_print
            gosub bcklines_text
            gosub'200(txtid$)

        REM Print the part text (if any) from the 'HNYMASTR' file *******
            call "READ100" (#7, part_nmbr$, f1%(7))
                if f1%(7) = 0% then L13090
            get #7 using L13080, txtid$ : gosub'200(txtid$)
L13080:     FMT POS(98), CH(4)      /* HNYMASTR part text ID      */
L13090:     print : nbr_lines% = nbr_lines% + 1%
            goto arilines_loop

        end_of_lines
            if nbr_lines% > max_lines% then gosub page_heading
            print using L63140   /* Underscore it */
            nbr_lines% = nbr_lines% + 1%
            if curr$ = "Y" then desc$ =                                  ~
                       currency$ & " " & currdesc$ & " INVOICE TOTAL:"   ~
               else desc$ = " INVOICE TOTAL:"
            amount = invgrossamt
            if curr$ = "Y" then call "CURRFMT"                           ~
                       (amount, currency$, fmttotal$, "Y")               ~
               else call "CONVERT" (amount, 2.2, fmttotal$)
            gosub total_printer
            if invdiscamt = 0 then goto try_sales_tax
                convert invdiscpct to disc$, pic (-###.##)
                desc$ = disc$ & "% INVOICE DISCOUNT:"
                amount = invdiscamt
                if curr$ = "Y" then call "CURRFMT"                       ~
                           (amount, currency$, fmttotal$, "N")           ~
                   else call "CONVERT" (amount, 2.2, fmttotal$)
                gosub total_printer

        try_sales_tax
            if invstaxamt = 0 then goto try_freight
                taxabledsc = round(invtaxable * invdiscpct * .01, 2)
                invtaxable = round(invtaxable - taxabledsc, 2)
                convert invstaxpct to disc$, pic (-##.###)
                if curr$ = "Y" then call "CURRFMT"                       ~
                           (invtaxable, currency$, stax$, "N")           ~
                   else call "CONVERT" (invtaxable, 2.2, stax$)
                amount = invstaxamt
                if curr$ = "Y" then call "CURRFMT"                       ~
                           (amount, currency$, fmttotal$, "N")           ~
                   else call "CONVERT" (amount, 2.2, fmttotal$)
                desc$ = disc$ & "% SALES TAX ON " &                      ~
                     str(stax$, pos(stax$ <> " ")) & ":"
                gosub total_printer

        try_freight
            if invfrtamt = 0 then goto try_net_esc     /* (AWD007) */
                amount = invfrtamt
                if curr$ = "Y" then call "CURRFMT"                       ~
                           (amount, currency$, fmttotal$, "N")           ~
                   else call "CONVERT" (amount, 2.2, fmttotal$)
                desc$ = "FREIGHT CHARGES:"
                gosub total_printer
                
/* (AWD007) */                
        try_net_esc
            if escamt = 0 then goto try_net_amount
                amount = escamt
                if curr$ = "Y" then call "CURRFMT"                       ~
                           (amount, currency$, fmttotal$, "N")           ~
                   else call "CONVERT" (amount, 2.2, fmttotal$)
                desc$ = "       SURCHARGE:"            /* CR285 */
                gosub total_printer

        try_net_amount
REM            if invnetamt = invgrossamt then goto exit_program
            if invnetamt = invgrossamt then goto cc_print
                amount = invnetamt
                if curr$ = "Y" then call "CURRFMT"                       ~
                           (amount, currency$, fmttotal$, "Y")           ~
                   else call "CONVERT" (amount, 2.2, fmttotal$)
                desc$ = "INVOICE NET AMOUNT:"
                gosub total_printer
/* CR3226*/
cc_print: ccdesc$ = "* Price shown is a cash price. If paying via credit card,"
          gosub print_cc	
          ccdesc$= "   a 2.5% service charge will be added at time of payment."  
          gosub print_cc			
          goto exit_program

        page_heading
            page_nbr% = page_nbr% + 1% : nbr_lines% = 0%
            if page_nbr% > 1% then print using L62710,                    ~
                " >>>>>  C O N T I N U E D  <<<<<", " *CONTINUED*"
            print page
            print skip(1)
                                                           /* (EWD003)  */
            call "STRING" addr ("RJ", inv_no$, 10%)
            print using L60050, inv_no$
                                                           /* (EWD003)  */            
            print using L60050, crmemo$
            print using L60050, duplicate$
            print skip (2)
            print using L60070, " ", invoice$, inv_date$, page_nbr%
            print skip(3) /* SKIP TO SOLD-TO, SHIP-TO PORTION OF FORM */
            for s% = 1% to 6%
                print using L61000, sold_to$(s%), ship_to$(s%)
            next s%
                                                     /* (EWD002) Begin */
            if curr$ = "Y" then goto L12040                                        
               print using L61110, "         JOB NAME: " & bck_job$,     ~
                                        "CUSTOMER NUMBER: " & cust_code$ 
/*IM3628*/     if approval_need$ <> "1" then                             ~
                  print using L61115, "         Load Number: " & apc_load$~
               else                                                       ~
                  print using L61110, "         Load Number: " & apc_load$,~
                                      "Approval Code: " & approval_code$
               goto L12045
L12040:     print using L61110, " CURRENCY: " & currency$ & " " &        ~
                             currdesc$, "CUSTOMER NUMBER: " & cust_code$
/*IM3628*/  if approval_need$ = "1" then                                 ~
               print using L61110, "           " & approval_code_d$ &    ~
                                   "Approval Code: " & approval_code$
L12045:
                                                      /* (EWD002) End  */
            print skip(2)
/*CR1993*/  if pgpo$ > " " then goto L12048
               if so$ = " " and bol$ = " " ~        
                then print using L62000, term_hdr$, po$                   ~
                else print using L62000, term_hdr$, po$, so$, "-",  bol$
               goto L12049                
L12048:
               if so$ = " " and bol$ = " " ~
                then print using L62000, term_hdr$, pgpo$                   ~
                else print using L62000, term_hdr$, pgpo$, so$, "-",  bol$ 
                
L12049:     print skip(2)
                                                     /* (EWD) - Begin */  
         if inv_type$ = "C" then goto L12052
            print using L62200, how_ship$, fob$, frt_bill$
            goto L12060
L12052:     print using L62200, how_ship$, reason$, str(settle$,1%,8%)
                                                     /* (EWD) - End   */
L12060:     print skip(2) /* SKIP TO BODY OF FORM */
            if term_code$ <> "DATED" then return
            if page_nbr% > 1% then return
            term_hdr$ = "SEE SCHEDULE ON PAGE 1"
            print using L62310, "----------- PAYMENT SCHEDULE ------------"

            nbr_lines% = nbr_lines% + 1%
            for t% = 1% to 30%
                if term_amt(t%) = 0% then goto L14060
                   if curr$ = "Y" then call "CURRFMT"                    ~
                              (term_amt(t%), currency$, amtdue$, "N")    ~
                      else call "CONVERT" (term_amt(t%), 2.2, amtdue$)
                   term_desc$ = amtdue$
                   gosub term_descriptor
                   print using L62310, term_desc$
                   nbr_lines% = nbr_lines% + 1%
L14060:     next t%
            term_desc$ = all("-")
            print using L62310, term_desc$
            nbr_lines% = nbr_lines% + 1%
            ser$ = "S/N'S:"
            return

        term_descriptor
            if term_pct(t%) = 0 then goto net_terms
               convert term_pct(t%) to disc$, pic (-###.##)
               call "DATEFMT" (term_date$(t%))
               term_desc$ = amtdue$ & disc$ & "% " & term_date$(t%) & ","

        net_terms
            call "DATEFMT" (term_net$(t%))
            str(term_desc$, len(term_desc$)+1%) = " NET " & term_net$(t%)
            return

        total_printer
            if nbr_lines% > max_lines% then gosub page_heading
            call "STRING" addr ("RJ", desc$, len(str(desc$)))
            print using L63000, desc$, fmttotal$
            print using L60050, inv_no$
            nbr_lines% = nbr_lines% + 1%
            return

/* CR3226 */			
        print_cc
		    if nbr_lines% > max_lines% then gosub page_heading
			call "STRING" addr("RJ", ccdesc$, 83%)
			print using L63212, ccdesc$
		    nbr_lines% = nbr_lines% + 1%
		    return 
			
        serial_number_print
            ser$ = "S/N'S:" : serkey$, serprint$ = " " : ser% = 0%
            serkey$ = "4" & str(cust_code$) & str(invoice$) &            ~
                str(lineid$) & hex(00)
        plow_serial_master
            call "PLOWALTS" (#9, serkey$, 2%, 22%, f1%(9))
            if f1%(9) = 0% then goto plow_serial_done
                get #9 using L14390, serial$
L14390:              FMT  /* SERMASTR #9  */ POS(32), CH(20)
                if len(serprint$) + len(serial$) + ser% >                ~
                     len(str(serprint$)) then gosub serial_print
                str(serprint$, len(serprint$) + ser%) = serial$
                ser% = 2%
                goto plow_serial_master

        plow_serial_done
            if ser% <> 0% then gosub serial_print
            return

        serial_print
            if nbr_lines% >= max_lines% then gosub page_heading
            print using L63110, ser$, serprint$
            ser$, serprint$ = " " : ser% = 0%
            nbr_lines% = nbr_lines% + 1%
            return

        bckmastr_text
            channel% = 14%
            goto common_bcktext

        bcklines_text
            channel% = 15%

        common_bcktext
            save_txtid$ = txtid$
            txtid$ = hex(ffffffff)
            if channel% = 14% then readkey$ = str(cust_code$) & so$      ~
                              else readkey$ = str(so$) & so_seq$
            call "READ100" (#channel%, readkey$, bcktext%)
            if bcktext% <> 1% then L14760
                if channel% = 14% then get #channel% using L14730, txtid$ ~
                                  else get #channel% using L14740, txtid$
L14730:              FMT POS(799), CH(4)
L14740:              FMT POS(242), CH(4)
                gosub'200(txtid$)
L14760:         txtid$ = save_txtid$
                return

        xref_print
            if xref_type$ = "M" and xref_mnf$ = "B" then L14810 else L14840
L14810:         print using L63280, "Manufactor Part No.:", xref_part$,   ~
                                    "(" & xref_descr$ & ")"
                goto L14870             /* Iterate */
L14840:     if xref_type$ = "C" and xref_cus$ = "B" then L14850 else L14880
L14850:         print using L63280, "Customer Part No.:", xref_part$,     ~
                                    "(" & xref_descr$ & ")"
L14870:         nbr_lines% = nbr_lines% + 1%
L14880: return

        get_config                                               /*(EWD005)*/

            init(" ") sv_seqnr$, fields$()
            sv_seqnr$ = ora_seqnr$
        next_config
            ora_seqnr% = 0%
            gosub oracle_fetch
            if oci_err% > 0% then goto no_config

            if max_lines%  - nbr_lines% < 4% then gosub page_heading   
            field_num% = 5%
            gosub oracle_getfield
            str(ora_seqnr$,1%,3%) = str(field$,1%,3%)

            convert ora_seqnr$ to ora_seqnr%, data goto exit_program

            if sv_seqnr$ <> ora_seqnr$ then goto no_config
            init (" ") orapart_desc$, oraqty_ship$, apc_prt$
            a$ = all(hex(20))

            field_num% = 9%
            gosub oracle_getfield
            str(oraqty_ship$,1%,3%) = str(field$,1%,3%)
            convert oraqty_ship$ to oraqty_ship, data goto L13190

L13190:
REM            call "STRING" addr ("RJ", oraqty_ship$,  len(str(oraqty_ship$)))
            field_num% = 10%
            gosub oracle_getfield
            orapart_desc$ = str(field$,1%,250%)
            field_num% = 11%
            gosub oracle_getfield
            apc_prt$ = str(field$,1%,50%)
            call "CONVERT" (qty_ship, 0, oraqty_ship$)
            if discsw$ = "Y" then L13200
                stock_prc = stock_prc - ((stock_prc * disc) / 100)
L13200:     call "CONVERT" (stock_prc, 2.4, price$)
            if curr$ = "Y" then call "CURRFMT"                           ~
                       (linextension, currency$, linextension$, "N")     ~
               else call "CONVERT" (linextension, 2.2, linextension$)

            config% = 1%

            gosub print_desc
            gosub line_config
            goto next_config

        no_config
               print
               nbr_lines% = nbr_lines% + 1%
               gosub load_newline

               end_config% = 1%
               config%     = 0%

        goto L11000

        print_desc
REM            if nbr_lines% >= max_lines% then gosub page_heading
            gosub lookup_customer
            if max_lines%  - nbr_lines% < 6% then gosub page_heading
            str(a$,1%,29%) = str(orapart_desc$,1%,29%)
            str(a$,30%,1%)  = "-"
            

            init(" ") qty_ship$                      /* (EWD006)  */
REM            if config% = 0% then qty_ship$ = str(oraqty_ordr$,9%,2%)     ~
            else                 qty_ship$ = str(oraqty_ship$,9%,2%)

/*SR64269+  if config% = 0% then qty_ship$ = str(oraqty_ship$,9%,2%)     ~
            else                 qty_ship$ = str(oraqty_ordr$,9%,2%)  */
            if config% = 0% then qty_ship$ = str(oraqty_ship$,8%,3%)     ~
            else                 qty_ship$ = str(oraqty_ordr$,8%,3%)

/*          if qty_ship = 0 then qty_ship$ = " 0"                     */
/*SR64269-*/if qty_ship = 0 then qty_ship$ = "  0"
REM            if config% = 1% then                                         ~
REM               call "STRING" addr ("RJ", qty_ship$,  len(str(qty_ship$)))

REM            if nbr_lines% >= (max_lines% + 6%) then gosub page_heading
            gosub print_line

            pos% = 31%
            for i% = 1% to 8%
                a$                =  all (hex(20))
                str(a$, 1%,30%)   =  str(orapart_desc$,pos%,30%)
                str(a$, 31%,1%)   =  "-"
                if str(a$,1%,30%) <> " " then                        ~
                   print using L62400, a$, " ", " ", " ", " ", " "
                if str(a$,1%,30%) <> " " then                        ~
                nbr_lines% = nbr_lines% + 1%
                pos% = pos% + 30%
            next i%

            if str(fields$(),4%,1%) = "1" then goto L13590
            a$              = all(hex(20))
            str(a$,1%,50%) = apc_prt$
            if str(a$,1%,80%) <> " " then                           ~
               print using L62400, a$, " ", " ", " ", " ", " "
            if str(a$,1%,30%) <> " " then                        ~
                nbr_lines% = nbr_lines% + 1%
            if str(apc$(2),16%,15%) = " " then goto L13590
               str(a$,1%,30%) = str(apc$(2),16%,15%)
               print using L62400, a$, " ", " ", " ", " ", " "               
               nbr_lines% = nbr_lines% + 1%
L13590: return 
        print_line                                                         /*(EWD005)*/
           if str(fields$(),4%,1%) = "1" then goto print_line_config    
           if str(fields$(),4%,1%) = "2" then goto print_line_config  /* CR784 */           
           print using L62400, a$, taxable$, qty_ship$,  price$,    ~
                                 discount$, linextension$        
           nbr_lines% = nbr_lines% + 1%
        return
        print_line_config
           print using L62400, a$, taxable$, qty_ship$,  " ",    ~
                                 " ", " "
           nbr_lines% = nbr_lines% + 1%
REM            goto get_config
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
            *      PRINT TEXT FOR EITHER THE HEADER OR DETAIL LINES     *~
            *************************************************************

            deffn'200(txtid$)
            if txtid$ = hex(ffffffff) then return
            if txtid$ = " " then return
                comp% = 0%
L50090:         call "TXTPRINT" (#6, f2%(6),  87%, txtid$, rptid$, 5%,   ~
                     nbr_lines%, max_lines%, "N", mask$, comp%)
                if comp% = 0% then return
                     gosub page_heading
                     goto L50090

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L60050: %                                                          ######~
        ~##################
L60070: %          ##############################                  ######~
        ~####### ######## ###
L61000: %          ###############################            ###########~
        ~####################
L61110: % ##################################################  ###########~
        ~###################
L61115: % #####################################
L62000: % ###############################  ################   ###########~
        ~##### # ############
L62200: % ##############################   ####################   #######~
        ~####################
L62310: % #########################################
L62400: % ################################ #### ########## ########## ###~
        ~####### -####,###.##
L62600: % ################################ #### ########## ####

L62710: % ###############################################################~
        ~####    ############
L63000: % ###############################################################~
        ~#### ###############
L63110: % ###### ########################################################~
        ~####################
L63212: % ###############################################################~
        ~####################

L63140: %                                                                ~
        ~     ---------------

L63210: % ################################ INV#: ######## / ###          ~
        ~  DATE: ##########
L63230: % ################################ ######### / ######## / ###    ~
        ~  DATE: ##########
L63250: % ################################ P/O#: ################ # ###  ~
        ~  ##### ##########

L63280: %        ################### #########################  #########~
        ~#########################
                                         /* (EWD) - Begin           */
        build_print                      /* PRINT DESCRIPTIONS      */
                                         /* APC$(1) = 30 CHARACTERS */
                                         /* APC$(2) = 30 CHARACTERS */
            err% = 0%                    /* APC$(3) = 30 CHARACTERS */
            if len(part_nmbr$) < 19% then err% = 1%
            if err% = 1% then return
            init(" ") apc$(), apc_scr$, apc_prt$, apc_sze$
            read #7,key = part_nmbr$,using L60390, apc$(), eod goto L60430
L60390:       FMT POS(606), 2*CH(30), CH(20)
            apc_sze$ = apc$(3)
            goto L60480

L60430:       call "APCDESCR" (part_nmbr$,apc_scr$,apc_prt$,apc_sze$,#13,~
                                                          err% )
              apc$(1) = str(apc_prt$, 1%,30%)
              apc$(2) = str(apc_prt$,31%,30%)
                                                      /* BUILD SIZE */
L60480:       apc$(3) = "WIDTH: " & str(apc_sze$,1%,7%) & " HEIGHT: " &  ~
                                    str(apc_sze$,11%,6%)
              gosub lookup_customer
            s_23% = 0%                         /* (EWD001) - Begin    */
            s_23m$ = str(part_nmbr$,1%,3%)
            s_so$  = str(so$,1%,8%)
            s_ln$  = str(so_seq$,1%,3%)
            init(" ") s_prv$, s_1$, s_23$ 
            prv% = 1%                                 /* Use BCKLINES    */
            call "APCPRZSB" (prv%, s_1$, cust_code$, s_23m$, s_so$,        ~
                                   s_ln$, s_prv$, s_23$, s_23%,            ~
                                   #16, #17, #15, #15, x_er% )
            if x_er% <> 0% then return
               str(apc$(1%),1%,8%) = s_23$

        return                                  /* (EWD001) - End     */

        lookup_customer
              if str(cust_code$,1%,2%) <> "HQ" then goto L60580
                 gosub lookup_sku_tt
                 if txt% = 1% then return

L60580:       init(" ") sku_no$, sku_code$
              read #16,key = cust_code$, using L60610, sku_code$,         ~
                                                      eod goto L60640
L60610:          FMT POS(1000), CH(3)
              if str(sku_code$,1%,1%) <> "0" then return
              gosub lookup_sku
L60640: return

        lookup_sku
              sku_no$ = " "
              sku_key$ = all(hex(00))
              str(sku_key$,1%,3%)  = sku_code$
              str(sku_key$,4%,25%) = part_nmbr$
              read #18,key 1% = sku_key$, using L60730, sku_no$,          ~
                                                       eod goto L60750
L60730:          FMT POS(4), CH(25)
              str(apc$(2),16%,15%) = "SKU- " & str(sku_no$,1%,10%)
L60750: return

        deffn'099(tid$)                     /* Text Id from Bcklines   */
          if tid$ = hex(00000000) or tid$ = hex(ffffffff) or tid$ = " "  ~
                                                              then return
          txt% = 1%
        return

        lookup_sku_tt                       /* Look Up Sku Text        */
                                            /* TT_KEY$ - Set By Header */
                                            /* Lookup in ARIMASTR      */
            init(" ") tid$, tid_key$, sav_tt$, tt$ : txt% = 0%
            read #15,key > tt_key$, using L60890, tt_key$, tid$,          ~
                                                          eod goto L61070
L60890:        FMT POS(10), CH(19), POS(242), CH(4)
            if str(so$,1%,8%) <> str(tt_key$,1%,8%) then return
            gosub'099(tid$)
            if txt% = 0% then return

            tid_key$ = all(hex(00))
            str(tid_key$,1%,1%) = "M"
            str(tid_key$,2%,3%) = "   "
            str(tid_key$,5%,4%) = tid$
            str(tid_key$,9%,1%) = "1"
            sav_tt$ = tid_key$
            read #6,key > tid_key$, eod goto L61080
               get #6, using L61020, tid_key$, tt$
L61020:          FMT CH(11), POS(64), CH(15)
            if str(sav_tt$,1%,9%) <> str(tid_key$,1%,9%) then goto L61080
            if tt$ <> " " then                                           ~
                          str(apc$(2),16%,15%) = "SKU-" & str(tt$,1%,11%)~
                          else goto L61080
L61070: return
L61080:     txt% = 0%
        return                                    /* (EWD) - End       */         

        lookup_job_name                           /* (EWD002)-BCKMASTR */
          init(" ") bck_key$, bck_job$
          str(bck_key$,1%,9%)   = cust_code$
          str(bck_key$,10%,16%) = so$
          read #14,key = bck_key$, using L64000,    /* IM3628*/          ~
                         quote_num$, bck_job$, eod goto L64010
L64000:      FMT POS(599), CH(10), POS(619), CH(20)  /*IM3628 */
L64010:   return                                  /* (EWD002) - End    */


        lookup_load                               /* (EWD004)-APCPLNOR */
          init(" ") apc_key$, apc_load$
          str(apc_key$,1%,8%) = so$
          read #21, key 4% = apc_key$, using L65000, apc_load$, eod goto L65010
L65000:         FMT POS(94), CH(5)
L65010:   return                                 /* (EWD004) - End    */

         
        oracle_connect                                     /*(EWD005)*/
            init(" ") user$, pass$, server$
            user$   = "MSSQL"
            pass$   = "MSSQL"
            oci_err% = 0%
            call "CONNECT"  (user$, pass$, server$, oci_err%)
        return

        oracle_discnnct
            call "DISCNNCT" (oci_err%)
        return

        oracle_query
            oci_err% = 0%
            call "QUERY"  (stmt1$, stmt2$, oci_err%)
        return

        oracle_exec
            init(" ") error$
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)

        return

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_fetch
            oci_err%  = 0%
            no_fields% = 0%
            call "FETCH"  (no_fields%, oci_err%)
        return


        oracle_getfield
            init(" ") field$
            oci_err% = 0%
            field_len% = 0%
            init(" ") field$, name$
REM            call "GETFIELD"  (field_num%, field$, oci_err%)
            call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)
            
        return

        load_newline
            config%, text_lne% = 0%
            init(" ") fields$()
            pos% = 1%
            field_num% = 5%
            gosub oracle_getfield
            ora_seqnr$ = field$
REM            call "SHOSTAT" ("seqnr=" & seqnr$)   stop
            for field_num% = 6%  to 15%                                
                gosub oracle_getfield
                str(fields$(),pos%, field_len%) = field$
                pos% = pos% + field_len%
            next field_num%

            field_num% = 16%
            gosub oracle_getfield
            convert str(field$,1%,len(field$)) to text_lne%, data goto next_lne
            str(fields$(),pos%,4%) = BIN(text_lne%,4)
            next_lne
            pos% = pos% + 4%
        return
       
        oracle_delete
            init(" ") stmt1$
            str(stmt1$,1%,40%)  = "DELETE FROM MSSQL.DISPLAY WHERE SALESORD"
            str(stmt1$,41%,27%) = "ER = '"& str(so$,1%,8%) & "' AND USERID "
            str(stmt1$,68%,10%) = "= '" & userid$ & "'"
            gosub oracle_flush
            gosub oracle_exec
        return                                                  /*(EWD005)*/
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
             gosub oracle_delete                               /*(EWD005)*/
REM             gosub oracle_discnnct                             /*(EWD005)*/

            end



