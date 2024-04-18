        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  U   U  PPPP   DDDD   TTTTT  EEEEE   *~
            *  A   A  R   R    I    U   U  P   P  D   D    T    E       *~
            *  AAAAA  RRRR     I    U   U  PPPP   D   D    T    EEEE    *~ 
            *  A   A  R   R    I    U   U  P      D   D    T    E       *~
            *  A   A  R   R  IIIII   UUU   P      DDDD     T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIUPDTE - Invoice Update.                                *~
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
            * 09/09/86 ! Original                                 ! ERN *~
            * 02/25/87 ! Serial Number and Lot Track Enhancements ! ERN *~
            * 05/21/87 ! Fix BCKLINES scheduled amount for line   ! ERN *~
            *          ! scheduled but not invoiced; fix release  !     *~
            *          ! of serial numbers for deleted BOL lines. !     *~
            * 05/28/87 ! Standard Costing Enhancements Project    ! ERN *~
            * 09/01/87 ! Now posts only 'net' invoice to Projects ! HES *~
            * 11/11/87 ! Mod to skip Invoice print of EXPORTs     ! MJB *~
            * 12/02/87 ! Added multi-currency, CURCONVR, etc.     ! JIM *~
            * 11/02/89 ! Over/Under Shipments Project.            ! JDH *~
            * 11/08/89 ! Now writes filler to ARMTBCEX file.      ! JDH *~
            * 12/06/89 ! Expanded SADETAIL file and added data.   ! JEF *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 07/31/90 ! Added G/L Export file logic              ! RAC *~
            * 11/16/90 ! Added Management DMC to ARILINES.        ! JDH *~
            * 11/21/90 ! Added Management Values to GLEXPORT.     ! JDH *~
            * 05/15/91 ! PRR 11762 Changed PLOWALTS Break Key on  ! JDH *~
            *          !   UPDSESSN from 3% to 11%.  Thanks Sid.  !     *~
            *          ! No extra reads if G/L Export OFF.        !     *~
            *          ! Calls WRAWSUPD for poss Wrty file update.!     *~
            *          !   Thanks Jim.                            !     *~
            *          ! Added adjustment for line item rounding  !     *~
            *          !   to Rounding Err Acct.  Second try!     !     *~
            *          !   PRRs 11791 & 11802 go away with proper !     *~
            *          !   implementation of this feature.        !     *~
            *          ! Corrected ARIMSCUR file length at Select.!     *~
            *          ! PRR 11621.  Credit Hold flag set to blank!     *~
            *          !   if SO closed and was on hold.          !     *~
            * 03/04/92 ! PRR 12290 Added 4th Alt Key to ARIMASTR. ! JDH *~
            * 04/22/92 ! Corrected PUT of erroneous data to pos   ! MLJ *~
            *          !   23-24 of SERMASTR on type '4' recs.    !     *~
            * 09/22/92 ! PRR 12613 Ensured Schld qty is zero if   ! JDH *~
            *          !   BOL is from pre-invoicing.             !     *~
            * 10/07/92 ! Ensured that 4th Alt Key is not blank.   ! JDH *~
            * 10/26/92 ! PRR 12651 If customer can't be found,    ! JDH *~
            *          !   ASKUSER can Exit or Try-Again.         !     *~
            * 11/09/92 ! Brought Core Deposit Module to R6.02.03. ! JIM *~
            * 11/12/92 ! Cust Credit- Upd Invoice, etc to CCRMASTR! JIM *~
            * 10/19/93 ! Neg. qty. (credit/adjs) can affect invtry! JDH *~
            * 10/20/93 ! Corrected invoice date in CCRMASTR.      ! JDH *~
            * 03/07/94 ! Increased BOMSPEC record length          ! WPH *~
            * 05/26/94 ! Honor flag about neg qty affect inventory! JDH *~
            * 05/25/95 ! PRR 13362 - shipments from diff. Stores  ! RJH *~
            *          !   Now correctly reflected on ARILINES so !     *~
            *          !   we must get default store from BCK when!     *~
            *          !   resetting open order QTY via HNYPST1.  !     *~
            * 07/24/96 ! Changes for the year 2000.               ! DXL *~
            * 12/11/97 ! Transfer call (EWD) Mod's to the current ! RHH *~
            *          ! Caelus version. All mods will be marked  !     *~
            *          ! with (EWD). EDI trading partner logic    !     *~
            *          ! for Lowes and Norendex stores. No Credits!     *~
            *          ! sent to Lowes. Skip invoice reason codes !     *~
            *          ! 30 thru 34. Call 'UPDATE_GROUP' to Chg   !     *~
            *          ! status code for planning (EWD)           !     *~
            * 03/31/98 ! Mods to Update Latest Caelus Version(EWD)! RHH *~
            * 09/29/98 ! (EWD002) - Skip Reason Codes 2           ! RHH *~
            * 09/08/99 ! (EWD003) - Change arguments to APCEDI03  ! RHH *~
            *          !    add channel for 'HNYMASTR'            !     *~
            * 01/24/00 ! (EWD004) - Mod for Samples/Displays and  ! RHH *~
            *          !    Literature.                           !     *~
            * 08/30/00 ! EDI trading partner logic for Norandex   ! CMG *~
            *          !   Reynolds stores. No Credits/Adj. sent. !     *~
            *          !   Skip invoice reason codes 30 thru 34.  !     *~
            *          !   Call 'UPDATE_GROUP' to Chg status code !     *~
            *          !   for planning (EWD005).                 !     *~
            * 11/06/00 ! (EWD006) - Mod to stop cred & Adj in EDI ! CMG *~
            * 09/23/04 ! (AWD007) - Mod to Plymart Invoicing      ! CMG *~
            * 06/01/06 ! (AWD008) - Add Strober files & subroutine! DES *~
            * 07/05/06 ! (AWD009)-  Mod for Energy Surcharge      ! CMG *~
            * 01/09/07 ! (AWD010) - Add Eastern Aluminum &        ! DES *~
            * 02/19/07 ! (AWD011) - Mod to make invoice date the  ! CMG *~
            *          !        same as postdate in session       !     *~
            * 02/22/07 ! (AWD012) - Add 84 Lumber                 ! DES *~
            * 04/12/07 ! (AWD013) - mod to add oracle inv trans   ! CMG *~
            *10/20/2009! (AWD014) - mod to add sub-routine track  ! CMG *~
            *          !        processed invoice numbers         !     *~
            *04/19/2011! (AWD015) mod for unitid, itemid, skunum  ! CMG *~
            *03/06/2013! (AWD016) mod for esc, escamt & escacct$  ! CMG *~
            *11/13/2014! (AWD017) mod to add schema               ! CMG *~
            *04/19/2019! (CR1993) Tax_rate change, HDMET, HDIDX   ! RDB *~
            *08/08/2019! CR2173 Trigger Atlas on invoice          ! RDB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            or_rec$170,                  /* GROUP RECORD          (EWD)*/~
            or_inv$8,                    /* INVOICE NUMBER        (EWD)*/~
            or_key4$8,                   /* S.O. KEY ALT 4    (EWD)*/~  
            acct1$9,                     /* Intra-divisional Sales Acct*/~
            acct2$9,                     /* Alt Owners Cust Sales Acct */~
            acctxref$9,                  /* Customer Account X-Ref     */~
            aractive$1,                  /* A/R System Active? (Y/N)   */~
            artype$1,                    /* A/R Type (A/C/E)           */~
            billship$180,                /* Bill-to Ship-to Address    */~
            billsold$180,                /* Bill-to Sold-to Address    */~
            billxref$9,                  /* Bill-to Customer X-Ref     */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bol$3,                       /* Bill of Lading Number      */~
            bolshipdate$6,               /* BOL Pre-invoice Ship Date  */~
            cat$4,                       /* Part Category Code         */~
            cdate$6,                     /* Currency Date              */~
            cdate1$6,                    /* Currency Date              */~
            convdate$6,                  /* Currency conversion date   */~
            corebank$(4)250,             /* Core Bank Work Area        */~
            crhold$1,                    /* Order Credit Status        */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4,                  /* Currency code              */~
            ctermsdue(30),               /* Currency Dating            */~
            cuscode$9,                   /* Customer Code              */~
            custype$2,                   /* Customer Type Code         */~
            cusbals(4),                  /* Customer Balances          */~
            descr$20,                    /* Session Description        */~
            discs$9,                     /* Line Item Sales Disc Acct  */~
            discacct$9,                  /* Header Discounts Account   */~
            dt_key$11,                   /* (EWD004) - Mod for Market  */~
            export_on$1,                 /* G/L Export File processing?*/~
            edi_id$3,                    /* EDI Code '001'-       (EWD)*/~
            edi_cus$9,                   /* EDI CUSTOMER CODE     (EWD)*/~
            edi_inv$8,                   /* EDI INVOICE NUMBER    (EWD)*/~
            edi_seq$3,                   /* EDI INVOICE SEQ NUMBER(EWD)*/~
            frtacct$9,                   /* Freight Account            */~
            fy_start$6,                  /* Fiscal Yr start date       */~
            fy_period$2,                 /* Period # within fiscal year*/~
            gain_loss$9,                 /* Currency Gain/Loss Acct    */~
            glpost$1,                    /* Session GL Post Flag (Y/N) */~
            high_ar_date$6,              /* Date of High A/R Balance   */~
            icc$6,                       /* Intercompany Corporate Code*/~
            invacct$9,                   /* Net Invoice Account        */~
            invdate$6,                   /* Invoice Date               */~
            invhdr$(10)200,              /* Invoice Header Record      */~
            invline$(5)150,              /* The whole Invoice Line     */~
            invnr$8,                     /* Invoice Number             */~
            invtype$1,                   /* Invoice Type               */~
            invuser$3,                   /* User who entered Invoice   */~
            linetextid$4,                /* Line Item Text ID          */~
            mgtrpt_on$1,                 /* Is Management Reporting on?*/~
            neg_inv$1,                   /* Neg Qty Affect Inv?        */~
            newlot$(30)6,                /* Invoice Lots Shipped       */~
            newlotqty(30),               /* Invoice Lot Qtys           */~
            oldlot$(30)6,                /* BOL Lots Shipped           */~
            oldlotqty(30),               /* BOL Lots Qtys              */~
/*PAR000*/  epart$45,                    /* Entire part                */~
            part$25,                     /* Part Number                */~
/*PAR000*/  subp$20,                     /* Subpart number             */~           
            plowkey$100,                 /* Multi-purpose Plow Key     */~
            po$16,                       /* Customer's PO #            */~
            postacct$9,                  /* Posting GL Account         */~
            postdate$6,                  /* Post Date (from Session)   */~
            postdescr$25,                /* Part Number or Literal     */~
            postdisc$1,                  /* Post Line Disc Separate?Y/N*/~
            posthny$1,                   /* Invoice Level Post Hny?    */~
            postseq$3,                   /* Posting Line Item #        */~
            posttype$4,                  /* Posting Type Literal       */~
            prnttype$10,                 /* Invoice Types to Print     */~
            project$8,                   /* Project Number             */~
            readkey$100, readkey2$100,   /* Multi-purpose Read Key     */~
            region$4,                    /* Region Code                */~
            rounding_acct$9,             /* Rounding error account     */~
            rsncode$9,                   /* Invoice Reason Code        */~
            sa$(2)150,                   /* Sales Analysis Data        */~
            sales$9,                     /* Line Item Sales Account    */~
            salesman$4,                  /* Salesman for Sales Analysis*/~
            seq$3,                       /* Invoice Line Sequence Numbr*/~
            session$6,                   /* Session Being Updated      */~
            shipdate$6,                  /* Ship Date                  */~
            shipto$180,                  /* Invoice Ship-to Address    */~
            so$16,                       /* Sales Order Number         */~
            soinvnr$8,                   /* Last Inv # to updte SO Line*/~
            soldto$180,                  /* Invoice Sold-to Address    */~
            solot$6,                     /* Sales Order Line Lot #     */~
            soseq$3,                     /* Sales Order Line Number    */~
            stat$4,                      /* Statutory Currency Code    */~
            stlmnt$12,                   /* Settlement Code            */~
            stlmnt_type$1,               /* Trial Balance Trans Type   */~
            store$3,                     /* Store Code                 */~
            tstore$3,                    /* Store Code (Temp)          */~
            taxacct$9,                   /* Sales Tax G/L Account      */~
            tcurrency$4,                 /* Transaction Currency Code  */~
            tdate$8,                     /* Temporary Date             */~
            temp$6,                      /* Temp Date                  */~
            terms$20,                    /* Payment Terms Code         */~
            terms_disc$(30)6,            /*               Disc Due Dte */~
            terms_due(30),               /*               Paymnt Amt   */~
            terms_net$(30)6,             /*               Net Due Date */~
            terms_pct(30),               /*               Cash Disc %  */~
            textid$4,                    /* Invoice Header Text ID     */~
            userid$3,                    /* User who is running update */~
            energy$14,                   /* ESC                        */~
            pg_line$2                    /* Line number for PGORLNTR   */

        dim                              /* G/L export posting info    */~
            country$3,                   /* Customer Country Code      */~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            generic$16,                  /* Part Generic code          */~
            prtclass$4,                  /* Part Class                 */~
            prttype$3,                   /* Part Type                  */~
            save_tran_type$5,            /* Temp Transaction Type      */~
            taxcode$10,                  /* Invoice Tax code           */~
            tran_type$5,                 /* G/L transaction type       */~
            uom$4,                       /* Part Stocking unit of meas */~
            pgpo$20                      /* PlyGem PO  CR1993          */

        dim bcksubpt_key$11,             /* BCKSUBPT Read Key          */~
            bcksubpt_key1$11,            /* BCKSUBPT Read key 1%       */~
            bcksubpt_rec$256             /* BCKSUBPT Record            */

        dim                              /* PAR000                     */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */
/* (AWD016) */        
        dim acct$12,                     /* General Use Acct Number    */~
            esc$8,                       /* ESC Percentage             */~
            escamt$8,                    /* ESC Amt                    */~
            escacct$9                    /* Energy Surcharge acct      */        

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 Open, -1 doesn't exist */~
                                         /*   0 if not checked         */~
            rslt$(64)20                  /* Text from file opening     */

        dim key_data$90
        dim unitid$10,itemid$10,skuno$9  /* (AWD015) additional fields */

        dim schema$8,                    /* Schema (AWD017)            */~
            invSys$3                     /* Calling System ANC or NTX  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy   (EWD)   "
        REM *************************************************************

            mat f2% = con
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! UPDSESSN ! Update Session Definitions               *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! CUSTOMER ! Customer Master File                     *~
/*PAR000*/  * #04 ! INVMASTR ! Inventory Master File                    *~
            * #05 ! ARIMASTR ! Invoice Master File                      *~
            * #06 ! ARILINES ! Invoice Line Items File                  *~
            * #07 ! BCKMASTR ! Sales Order Master- Headers              *~
            * #08 ! BCKLINES ! Sales Order Master- Line Items           *~
            * #09 ! ARIBUFFR ! Invoice Buffer File- Headers             *~
            * #10 ! ARIBUF2  ! Invoice Buffer File- Lines               *~
            * #11 ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #12 ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #13 ! GLMAIN   ! General Ledger.                          *~
            * #14 ! GLDETAIL ! General Ledger Detail File               *~
            * #15 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #16 ! PIPIN    ! Planned Inventory Additions Detail       *~
            * #17 ! PIPOUT   ! Planned Inventory Use Detail             *~
            * #18 ! TXTFILE  ! System Text File                         *~
            * #19 ! SHPHNYTF ! Shipping-Inventory Transaction File      *~
/*PAR000*/  * #20 ! INVQUAN  ! Inventory Quantity File                  *~
/*PAR000*/  * #21 ! INVPOOL  ! Inventory LIFO/FIFO pool records         *~
            * #23 ! HNYPSTGL ! HNYPOST- G/L Transaction Work File       *~
/*PAR000*/  * #24 ! INVDETAL ! Inventory Details                        *~
            * #25 ! SFCUM2   ! Cumulative sales forecast file           *~
REM         * #26 ! JOBMASTR ! Project Master File                      *~
REM         * #27 ! JOBSALES ! Project Sales Detail File                *~
            * #28 ! ARIJRLTF ! Invoice Journal Transaction File         *~
            * #29 ! ARMTRIAL ! A/R Trial Balance                        *~
            * #30 ! ARIREGTF ! Invoice Register Transaction File        *~
            * #31 ! ARIINVRF ! Invoice Report File                      *~
            * #32 ! DEMMASTR ! Demand Master File                       *~
            * #33 ! WCMASTR  ! Work Center Master                       *~
            * #34 ! WCOUT    ! Planned Work Center Use Detail Record    *~
            * #35 ! SFMASTR2 ! Sales Forecast Master File               *~
            * #36 ! JBCROSS2 ! Cross Reference of RTE & BOM planned for *~
            * #37 ! PIPCROSS ! Hard Peg Cross Reference                 *~
/* CR2173 reuse */* #38 ! JBPIPXRF ! Option Part Harder Peg             *~
            * #39 ! BOMSPEC  ! Options Selected File                    *~
            * #40 ! SERMASTR ! Serial Number Master File                *~
            * #41 ! SERTIF   ! Serial Number Transaction File           *~
            * #42 ! ARIMSCUR ! Currency-specific ARI Master file        *~
            * #43 ! ARMTBCEX ! A/R Trial Balance Currency Exposure file *~
            * #44 ! ARMTBCRC ! A/R Trial Balance Currency Reconciliation*~
            * #45 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #46 ! CURMASTR ! Multi-Currency Master file               *~
            * #47 ! ARILNCUR ! Currency-specific ARI line items         *~
            * #50 ! CORARITF ! Core A/R Interface File                  *~
            * #51 ! CCRMASTR ! Customer Credit Master file              *~
            * #52 ! CUSDETAL ! (EWD004) Marketing Data for Samp/Disp/Lit*~
            * #55 ! APCPLNOR ! (EWD) New S.O. Hdr replace APCORDER (EWD)*~
            * #56 ! GENCODES ! (EWD) Master Code Table File        (EWD)*~
            * #57 ! APCEDIMC ! (EWD) 'EDI' Master Control File     (EWD)*~
            * #58 ! APCSKUNO ! (EWD) Sku Part Number Reference     (EWD)*~
            * #59 ! ARMTERMS ! (EWD) Terms Code and Cash Disc File (EWD)*~
            * #64 ! BCKSUBPT ! Sub Part File                 CR347      *~
            * #53 ! EDISTRBR ! Strober edi / district cross reff        *~
            * #54 ! DISSTRBR ! Strober district bill to information     *~
            * #22 ! APCEDIST ! (EWD) 'EDI' Master Control File - Strober*~
            * #48 ! APCEDIGU ! (EWD) 'EDI' Master Control File -Guardian*~
            * #49 ! APCEDILN ! (EWD) 'EDI' Master Control File - Lansing*~
            * #26 ! APCEDIEA ! (EWD) 'EDI' Master File- Eastern Aluminum*~
REM         * #27 ! APCEDI84 ! (EWD) 'EDI' Master File- 84 Lumber       *~
            * #27 ! AWDSKUXR ! Lowe's sku xref file                     *~
            * #26 ! APCEDIFL ! (EWD) 'EDI' Master File- Multiple sku's  *~
*AWD013     * #38 ! ORAINV   ! ORACLE INVOICES TO SEND DAILY            *~
/*CR2173 */ * #38 ! PGORLNTR ! PlyGem ATLaS Trigger Remote Order Line Fi*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "UPDSESSN",                                      ~
                        varc, indexed, recsize =  200,                   ~
                        keypos = 4, keylen = 17,                         ~
                        alt key  1, keypos =  1, keylen =  20

            select #02, "SYSFILE2",                                      ~
                        varc, indexed, recsize =  500,                   ~
                        keypos = 1, keylen = 20

            select #03, "CUSTOMER",                                      ~
                        varc, indexed, recsize = 1200,                   ~
                        keypos = 1, keylen =   9,                        ~
                        alt key  1, keypos =  10, keylen =  30, dup,     ~
                            key  2, keypos = 424, keylen =   9, dup,     ~
                            key  3, keypos = 771, keylen =   9, dup,     ~
                            key  4, keypos = 780, keylen =   9, dup

* PAR000
            select #04, "INVMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 1024,                                 ~
                         keypos = 1, keylen = 45,                        ~
                         alternate key 1, keypos = 122, keylen = 9, dup, ~
                                   key 2, keypos = 110, keylen = 4, dup, ~
                                   key 3, keypos = 46, keylen = 32, dup   

            select #05, "ARIMASTR",                                      ~
                        varc, indexed, recsize = 2000,                   ~
                        keypos = 1, keylen =  17,                        ~
                        alt key 1, keypos = 10, keylen =  8, dup,        ~
                            key 2, keypos = 18, keylen = 16, dup,        ~
                            key 3, keypos = 34, keylen = 16, dup,        ~
                            key 4, keypos = 1783, keylen = 26

            select #06, "ARILINES",                                      ~
                        varc, indexed, recsize = 750,                    ~
                        keypos = 1, keylen = 20

            select #07, "BCKMASTR",                                      ~
                        varc, indexed, recsize = 1000,                   ~
                        keypos = 1, keylen =  25,                        ~
                        alt key  1, keypos =  26, keylen =  16, dup

            select #08, "BCKLINES",                                      ~
                        varc, indexed, recsize =  300,                   ~
                        keypos = 10, keylen = 19

            select #09, "ARIBUFFR",                                      ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 1, keylen =   17,                       ~
                        alt key  1, keypos = 2001, keylen =  24,         ~
                            key  2, keypos =   34, keylen =  16, dup

            select #10, "ARIBUF2",                                       ~
                        varc, indexed, recsize = 750,                    ~
                        keypos = 1, keylen = 20

            select #11, "SHPHDRS",                                       ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 28

            select #12, "SHPLINES",                                      ~
                        varc, indexed, recsize = 600,                    ~
                        keypos = 10, keylen = 22

            select #13, "GLMAIN",                                        ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 9

            select #14, "GLDETAIL",                                      ~
                        varc, indexed, recsize =  160,                   ~
                        keypos = 1, keylen = 26

            select #15, "PIPMASTR",                                      ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 2, keylen = 25,                         ~
                        alt key  1, keypos =  1, keylen =  26

            select #16, "PIPIN",                                         ~
                        varc, indexed, recsize = 60,                     ~
                        keypos = 30, keylen = 19,                        ~
                        alt key   1, keypos =  1, keylen =  48

            select #17, "PIPOUT",                                        ~
                        varc, indexed, recsize = 64,                     ~
                        keypos =  1, keylen = 56,                        ~
                        alt key   1, keypos = 20, keylen =  37

            select #18, "TXTFILE",                                       ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 1, keylen =  11

            select #19, "SHPHNYTF",                                      ~
                        varc, indexed, recsize = 572,                    ~
                        keypos = 1, keylen = 46,                         ~
                        alt key  1, keypos = 47, keylen =  80

            select #20,  "INVQUAN",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   768,                                 ~
                        keypos =   17, keylen =  64,                     ~
                        alt key  1, keypos =    1, keylen =  64

* PAR000
            select #21, "INVPOOL",                                       ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 58

/* #22 */
            select #22, "APCEDIST",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1 , keylen =  22,                     ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup

            select #23, "HNYPSTGL",                                      ~
                        varc, indexed, recsize = 160,                    ~
                        keypos = 10, keylen = 10

            select #24, "INVDETAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  62,                     ~
                        alt key  1, keypos =   63, keylen =   6, dup,    ~
                            key  2, keypos =   69, keylen =   2, dup

            select #25, "SFCUM2",                                        ~
                        varc, indexed, recsize = 1985,                   ~
                        keypos = 1, keylen = 25

REM         select #26, "JOBMASTR",                                      ~
REM                     varc, indexed, recsize = 700,                    ~
REM                     keypos = 1, keylen = 8

REM         select #27, "JOBSALES",                                      ~
REM                     varc, indexed,  recsize = 200,                   ~
REM                     keypos = 1, keylen = 16

            select #28, "ARIJRLTF",                                      ~
                        varc, indexed, recsize = 638,                    ~
                        keypos = 1, keylen = 43

            select #29, "ARMTRIAL",                                      ~
                        varc, indexed, recsize = 256,                    ~
                        keypos = 1, keylen = 21

            select #30, "ARIREGTF",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos = 1, keylen = 26

            select #31, "ARIINVRF",                                      ~
                        varc, indexed, recsize = 20,                     ~
                        keypos = 1, keylen = 20

            select #32, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28

            select #33, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #34, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #35, "SFMASTR2",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  25

            select #36, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =  94,               ~
                        keypos =  29,  keylen = 19,                      ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47

            select #37, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =  1,   keylen = 71,                      ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33

* AWD013
REM            select #38, "JBPIPXRF",                                      ~
REM                        varc,     indexed,  recsize =  63,               ~
REM                        keypos = 1,    keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19

            select #39, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #40, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #41, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #42, "ARIMSCUR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  17,                     ~
                        alt key 1, keypos = 1, keylen = 21

            select #43, "ARMTBCEX",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  21,                     ~
                        alt key 1, keypos = 1, keylen = 25

            select #44, "ARMTBCRC",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  21,                     ~
                        alt key 1, keypos = 1, keylen = 25

            select #45, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            select #46, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #47, "ARILNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  20,                     ~
                        alt key 1, keypos = 1, keylen = 24

 
/* #48, #49 */
            select #48, "APCEDIGU",                                      ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =  1,   keylen = 22,                      ~
                        alt key  1, keypos  =    10, keylen = 13, dup,   ~
                            key  2, keypos  =   233, keylen =  6, dup

            select #49, "APCEDILN",                                      ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =  1,   keylen = 22,                      ~
                        alt key  1, keypos  =    10, keylen = 13, dup,   ~
                            key  2, keypos  =   233, keylen =  6, dup

            select #26, "APCEDIEA",                                      ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =  1,   keylen = 22,                      ~
                        alt key  1, keypos  =    10, keylen = 13, dup,   ~
                            key  2, keypos  =   233, keylen =  6, dup

REM         select #27, "APCEDI84",                                      ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =  1,   keylen = 22,                      ~
                        alt key  1, keypos  =    10, keylen = 13, dup,   ~
                            key  2, keypos  =   233, keylen =  6, dup

            select #27,   "AWDSKUXR",                                     ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =    1, keylen =  16,                    ~
                        alt key  1, keypos =  17, keylen =  20,         ~
                            key  2, keypos =  37, keylen =  45, dup

            select #50, "CORARITF",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  24

            select #51, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

                                                    /* (EWD004)        */    
            select #52, "CUSDETAL",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos =   35, keylen =  11,         ~
                            key  2, keypos =   29, keylen =  17
/* #53, #54 */
                                                    /* (EWD) - Begin */
            select #55,  "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #56, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #57, "APCEDIMC",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1 , keylen =  22,                     ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup

            select #58, "APCSKUNO",                                      ~
                        varc,     indexed,  recsize = 73,                ~
                        keypos =   1 , keylen =  28,                     ~
                        alt key 1, keypos = 29, keylen = 28, dup

            select #59, "ARMTERMS",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   1 , keylen =  20
                                                       /* (EWD) - End */

            select #60, "EWDFTPMR",                                      ~
                        varc,     indexed,  recsize = 224,               ~
                        keypos =   7 , keylen =  22,                     ~
                        alt key    1 , keypos =   1, keylen =  28

            select #61, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =  1,   keylen =  32                        
                                                     /*  (EWD005)  */



            select #62, "AWDEDIPL",           /* (AWD007)  */             ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1 , keylen =  22,                     ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup
                                                  /*  (AWD007)  */

            select #63, "AWDEDIIB",                                     ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =  1 , keylen = 22,                      ~
                        alt key   1 , keypos = 10, keylen =  13,  dup,  ~
                            key   2 , keypos = 233, keylen = 06,  dup

            select #64, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup  

            select #53, "EDISTRBR",                                      ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos =    1, keylen =  6                       ~

            select #54, "DISSTRBR",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  2                       


* (AWD013) 
* (AWD014) Not used anymore, opened in subroutine
REM         select #38, "ORAINV",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24       
                       
/* CR2173 */
            select #38,  "PGORLNTR",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   21, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  64,         ~
                            key  2, keypos =   54, keylen =  11, dup

REM     call "SHOSTAT" ("Opening Files")
            call "OPENCHCK" (#01, fs%(1 ), f2%(1 ),   0%, rslt$(1 ))
            call "OPENCHCK" (#02, fs%(2 ), f2%(2 ),   0%, rslt$(2 ))
            call "OPENCHCK" (#03, fs%(3 ), f2%(3 ),   0%, rslt$(3 ))
            call "OPENCHCK" (#04, fs%(4 ), f2%(4 ),   0%, rslt$(4 ))
            call "OPENCHCK" (#05, fs%(5 ), f2%(5 ), 100%, rslt$(5 ))
            call "OPENCHCK" (#06, fs%(6 ), f2%(6 ), 200%, rslt$(6 ))
            call "OPENCHCK" (#07, fs%(7 ), f2%(7 ),   0%, rslt$(7 ))
            call "OPENCHCK" (#08, fs%(8 ), f2%(8 ),   0%, rslt$(8 ))
            call "OPENCHCK" (#09, fs%(9 ), f2%(9 ), 1000%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10),   0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12),   0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15),   0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
            call "OPENCHCK" (#17, fs%(17), f2%(17),   0%, rslt$(17))
            call "OPENCHCK" (#18, fs%(18), f2%(18),   0%, rslt$(18))
            call "OPENCHCK" (#19, fs%(19), f2%(19),   0%, rslt$(19))
            call "OPENCHCK" (#20, fs%(20), f2%(20),   0%, rslt$(20))
            call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, rslt$(21))
            call "OPENCHCK" (#22, fs%(22), f2%(22), 1000%, rslt$(22))
            call "WORKOPEN" (#23, "IO   ", 10%, f2%(23))
            call "OPENCHCK" (#24, fs%(24), f2%(24),   0%, rslt$(24))
            call "OPENCHCK" (#25, fs%(25), f2%(25),   0%, rslt$(25))
REM         call "OPENCHCK" (#26, fs%(26), f2%(26),   0%, rslt$(26))
REM            if f2%(26) = 0% then f1%(27) = 100%
REM         call "OPENCHCK" (#27, fs%(27), f2%(27), f1%(27), rslt$(27))
            call "OPENCHCK" (#28, fs%(28), f2%(28),5000%, rslt$(28))
            call "OPENCHCK" (#29, fs%(29), f2%(29), 500%, rslt$(29))
            call "OPENCHCK" (#30, fs%(30), f2%(30), 500%, rslt$(30))
            call "OPENCHCK" (#31, fs%(31), f2%(31), 500%, rslt$(31))
            call "OPENCHCK" (#32, fs%(32), f2%(32),   0%, rslt$(32))
            call "OPENCHCK" (#33, fs%(33), f2%(33),   0%, rslt$(33))
            call "OPENCHCK" (#34, fs%(34), f2%(34),   0%, rslt$(34))
            call "OPENCHCK" (#35, fs%(35), f2%(35),   0%, rslt$(35))
            call "OPENCHCK" (#36, fs%(36), f2%(36),   0%, rslt$(36))
            call "OPENCHCK" (#37, fs%(37), f2%(37),   0%, rslt$(37))
REM            call "OPENCHCK" (#38, fs%(38), f2%(38),   0%, rslt$(38))
            call "OPENCHCK" (#39, fs%(39), f2%(39),   0%, rslt$(39))
            call "OPENCHCK" (#40, fs%(40), f2%(40),   0%, rslt$(40))
            call "OPENCHCK" (#41, fs%(41), f2%(41),   0%, rslt$(41))
            call "OPENCHCK" (#42, fs%(42), f2%(42), 200%, rslt$(42))
               if f2%(42) <> 0% then L05430
                  f1%(43), f1%(44) = 100%
L05430:     call "OPENCHCK" (#43, fs%(43), f2%(43), f1%(43), rslt$(43))
               if f2%(43) <> 0% then L05440
                  f1%(44) = 100%
L05440:     call "OPENCHCK" (#44, fs%(44), f2%(44), f1%(44), rslt$(44))
            call "OPENCHCK" (#45, fs%(45), f2%(45),      0%, rslt$(45))
            call "OPENCHCK" (#46, fs%(46), f2%(46),      0%, rslt$(46))
            call "OPENCHCK" (#47, fs%(47), f2%(47),      0%, rslt$(47))
            call "OPENCHCK" (#48, fs%(48), f2%(48),   1000%, rslt$(48))
            call "OPENCHCK" (#49, fs%(49), f2%(49),   1000%, rslt$(49))
            call "OPENCHCK" (#51, fs%(51), f2%(51),    100%, rslt$(51))
            call "OPENCHCK" (#52, fs%(52), f2%(52),    100%, rslt$(52))

                                                    /* (EWD) - Begin */
        call "OPENCHCK" (#55, fs%(55), f2%(55),   1000%, rslt$(55))
            call "OPENCHCK" (#56, fs%(56), f2%(56),      0%, rslt$(56))
            call "OPENCHCK" (#57, fs%(57), f2%(57),   1000%, rslt$(57))
            call "OPENCHCK" (#58, fs%(58), f2%(58),      0%, rslt$(58))
            call "OPENCHCK" (#59, fs%(59), f2%(59),      0%, rslt$(59))
                                                    /* (EWD) - End   */
            call "OPENCHCK" (#60, fs%(60), f2%(60),   1000%, rslt$(60))
            call "OPENCHCK" (#61, fs%(61), f2%(61),      0%, rslt$(61))            
                                                    /* (EWD005)    */
                                                    /* (AWD007)    */
            call "OPENCHCK" (#62, fs%(62), f2%(62),   1000%, rslt$(62))
            call "OPENCHCK" (#63, fs%(63), f2%(63),   1000%, rslt$(62))
            call "OPENCHCK" (#64, fs%(64), f2%(64),   1000%, rslt$(64))
            call "OPENCHCK" (#26, fs%(26), f2%(26),   1000%, rslt$(26))
            call "OPENCHCK" (#27, fs%(27), f2%(27),   1000%, rslt$(27))
                                                    
            call "OPENCHCK" (#53, fs%(53%), f2%(53%),  0%, rslt$(53%))
            call "OPENCHCK" (#54, fs%(54%), f2%(54%),  0%, rslt$(54%))

* (AWD013) 
* (AWD014) 
REM         call "OPENCHCK" (#38, fs%(38%), f2%(38%), 100%, rslt$(38%))
/* CR2173 */
            call "OPENCHCK" (#38, fs%(38%), f2%(38%), 25%, rslt$(38%))

            if f2%(19) = 0% then L09000
                call "OPENCHCK" (#19, fs%(19), f2%(19), 200%, rslt$(19))
                write #19 using L05490, "0", "0", "1", " ", " "
L05490:              FMT CH(46), CH(80), CH(1), CH(200), CH(173)


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)


            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #56, schema_err%)
            invSys$ = "ANC"
            if schema% = 2% then invSys$ = "NTX"

            return% = 0%  /* Assumes processing ok */

            call "BCKSWTCH" ("AR ", "POSTDISC", postdisc$, temp, ret%)
            call "BCKSWTCH" ("AR ", "ARACTIVE", aractive$, temp, ret%)
            call "BCKSWTCH" ("AR ", "PRNTTYPE", prnttype$, temp2, ret%)
            call "BCKSWTCH" ("AR ", "NEG_INV ", neg_inv$,  temp2, ret%)

REM         call "SHOSTAT" ("U P D A T I N G   I N V O I C E S")

*        Check for Core Bank
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#02, plowkey$, core_bank%)
               if core_bank% = 0% then L09130
            get #02 using L09126, str(plowkey$,1%,1%)
L09126:         FMT POS(128), CH(1)
            if str(plowkey$,1%,1%) <> "Y" then core_bank% = 0%
            if core_bank% = 0% then L09130
            call "OPENCHCK" (#50, fs%(50), f2%(50), 200%, rslt$(50))
L09130
*        Check for Multi-Currency
            curr$ = "N" : currtype$ = " "
            plowkey$ = "SWITCHS.CUR"
            call "READ100" (#02, plowkey$, f1%(2))
               if f1%(2) = 0% then L09210
            get #02, using L09180, curr$, stat$, currtype$, rounding_acct$
L09180:         FMT POS(21), CH(1), CH(4), POS(31), CH(1), POS(36), CH(9)

L09210
*        See if G/L Export or Management Reporting is on
            export_on$, mgtrpt_on$ = "N"
            plowkey$ = "SWITCHS.GL"
            call "READ100" (#02, plowkey$, f1%(2))
            if f1%(2) = 1% then get #02 using L09240, export_on$, mgtrpt_on$
L09240:         FMT POS(22), CH(1), POS(59), CH(1)
            if mgtrpt_on$ = "Y" then gosub init_mgt

*        Get Suspense Account for Rounding Error account, if necessary.
            if rounding_acct$ <> " " or curr$ <> "Y" then L10000
            plowkey$ = "FISCAL DATES"
                call "READ100" (#02, plowkey$, f1%(2))
                   if f1%(2) = 0% then L10000
                get #02, using L09320, rounding_acct$
L09320:            FMT POS(417), CH(9)

L10000: REM *************************************************************~
            *          S E S S I O N   C O N T R O L S                  *~
            * --------------------------------------------------------- *~
            * Get the Next Session to update and update it's invoices.  *~
            * Always pull from the top of the stack.                    *~
            *************************************************************

        session_loop
*        Get next Session to Update.  If none, we're all done.
            plowkey$ = str(userid$) & "ARIUPDTE" & hex(00)
            call "PLOWALTS" (#01, plowkey$, 1%, 11%, f1%(1)) /*UPDSESSN*/
            if f1%(1) = 0% then exit_program

            get #01 using L10140, session$, descr$, postdate$, glpost$
L10140:         FMT POS(12), CH(6), XX(3), CH(20), POS(41), CH(6),       ~
                    POS(51), CH(1)

*        Write Posting Header to ARIJRLTF
            write #28 using L10200, userid$, session$, glpost$, postdate$,~
                      descr$, " ", " ", " ", eod goto L10230
L10200:     FMT CH(3), CH(40), CH(1), CH(6), CH(20), CH(58), 2*CH(255)

*        Update the Invoices that are in the Session.
L10230:     gosub invoice_update

*        Remove Session record freom the file.
            plowkey$ = "ARIUPDTE" & str(session$) & hex(00)
            call "DELETE" (#01, plowkey$, 14%)
            goto session_loop


        REM *************************************************************~
            *         I N V O I C E   U P D A T E   C O N T R O L       *~
            * --------------------------------------------------------- *~
            * Get the Invoices for the session and perform the update   *~
            * Tasks.  We always pull the invoice AND lines from the     *~
            * first available and toss as we go.  This alone causes a   *~
            * certain level of restartability.                          *~
            *************************************************************
        invoice_update

        header_loop  /* Load up the Next Invoice, SO and BOL Headers   */
            gosub load_headers
            if f1%(9) = 0% then return   /* Done with the Session      */
            gosub update_cust_invoice

            line_loop
                gosub load_line_items    /* Get Next for Inv, So & BOL */
                if f1%(10) = 0% then header_updating
                     gosub post_inventory
                     gosub adjust_order_line
                     gosub sales_analysis_posting
                     gosub update_project
                     gosub update_gl_line
                     gosub toss_line
                     goto  line_loop

            header_updating
                if curr$ = "Y" then gosub adjust_for_rounding
                gosub adjust_order
                gosub delete_bol
                gosub update_serial_numbers
                gosub update_trial_balance
                gosub update_gl_header
                gosub update_customer
                gosub update_invoice_header

* (AWD013)
                gosub update_orainv
                goto  header_loop        /* Done with the Invoice      */


*       ****************************************************************~
*                       U P D A T E   R O U T I N E S                  *~
*        ------------------------------------------------------------- *~
*        Subroutines which perform the specific updating tasks.        *~
*       ****************************************************************

        post_inventory
            invcost = -1
            if posthny$ = "N" then return  /* Invoice Level - Don't Do */
            if soinvnr$ = invnr$ then return /* Been here before       */
            if invtype$ = "O" or invtype$ = "X" then L11120
            if (invtype$ = "C" or invtype$ = "A") and neg_inv$ = "Y"     ~
                                                               then L11120
                if invshipd <= 0 then return  /* No Returns!           */

L11120
*        OK Fine, call posting subroutine.               PAR000
            call "SHPHNYSB"  ("I", store$, part$, subp$, seq$, soseq$,   ~
                              cuscode$,                                  ~
                              so$, bol$, invnr$, pricestk, postdate$,    ~
                              oldlot$(), oldlotqty(),                    ~
                              newlot$(), newlotqty(), invcost,           ~
                              invuser$, #19, #02, #23, #20, #24, #21,    ~
                              #04, #15, #25, #13, #14)     /* PAR000 */

            return

*       ****************************************************************
        adjust_order_line
*        Adjust Order, Shipped, Open, Allocated, and Pre-Invoiced Qtys
*        and rewrite the line item.  Also adjust PIPs.
            if soline%  = 0%     then return  /* No Sales Order Line   */
            if soinvnr$ = invnr$ then return  /* Already Updated       */

            newschld  = max(0, soschld - bolschld)  /* Adjust Scheduld */
            newopen   = max(0, soopen  - invshipd)  /* Adjust Open Qty */
                if invopen = 0 then newopen = 0     /* Honor Close SO  */
            newopen   = max(newopen, newschld)      /* Retain to Schld */
            newshipd  = soshipd + invshipd          /* Calc Total Shpd */
            newalloc  = max(0, soalloc - invshipd)  /* Reduce Allocatd */
            newpreinv = max(0, sopreinv - bolshipd) /* Adjust PreInvcd */
            chngopen  = newopen - soopen  /* Normally Negative */

*        Adjust Quantity Backordered
            if chngopen = 0 then L11450
                readkey$ = str(cuscode$) & so$
                call "READ100" (#07, readkey$, f1%(7%))
                if f1%(7%) = 0% then L11404
                get #7 using L11399, tstore$
L11399:              FMT POS(803), CH(3)
                goto L11406

L11404:         tstore$ = store$
* PAR000
L11406:         call "INVPST1" (part$, subp$, tstore$, solot$,           ~
                                0, chngopen, 0, 0, 0,                    ~
                                #20, #04, #02, f2%(20), f2%(4), f2%(2),  ~
                                0%, ret%)

L11450
*        Rewrite Sales Order Line  (BCKLINES)
            readkey$ = str(so$) & soseq$
            call "READ101" (#08, readkey$, f1%(8))
            if f1%(8) = 0% then L11540    /* Would be very queer indeed */
            put #08 using L11510, newshipd, newopen, newschld, newalloc,  ~
                                newpreinv, invnr$
L11510:         FMT POS(101), 5*PD(14,4), POS(247), CH(8)
            rewrite #08

L11540
*        Take care of PIPs for this line item      /* (EWD) - Begin    */
                call "APCPIPSB" (so$,              /* Sales Order #    */~
                                 soseq$,           /* Sequence #       */~
                                 store$,           /* Store Code       */~
                                 crhold$,          /* Credit Hold Flag */~
                                 #08,              /* BCKLINES,        */~
                                 #32,              /* DEMMASTR         */~
                                 #02,              /* SYSFILE2         */~
                                 #15,              /* PIPMASTR         */~
                                 #16,              /* PIPIN            */~
                                 #17,              /* PIPOUT           */~
                                 #39,              /* BOMSPEC          */~
                                 #33,              /* WCMASTR          */~
                                 #34,              /* WCOUT            */~
                                 #35,              /* SFMASTR2         */~
                                 #25,              /* SFCUM2           */~
                                 #36,              /* JBCROSS2         */~
                                 #37,              /* PIPCROSS         */~
/*(AWD014)*/                     #37 )             /* JBPIPXRF         */
                return                             /* (EWD) - End      */

*       ****************************************************************
        sales_analysis_posting
*        Update (1) Shipment History for Invoice;
*               (1) Booking History if indicated in Order Qty

*        First the vanilla Shipment Transaction
            sagross = line_ext - line_disc_amt
            call "WHICHPER" (#02, postdate$, whichper%)
            if whichper% < 14% then                                      ~
                get #02 using L11811, fy_start$                           ~
            else                                                         ~
                get #02 using L11812, fy_start$
L11811:              FMT POS(23), CH(6)
L11812:              FMT POS(127), CH(6)

            if whichper% > 13% then whichper% = whichper% - 13%
            convert whichper% to fy_period$, pic(00)
            put sa$() using L12180,                                       ~
                     "3",                /* Shipment Transaction       */~
                     " ",                /* Date/Time Stamp            */~
                     postdate$,          /* Posting Date               */~
                     so$, bol$,          /* Sales Order, BOL #s        */~
                     invnr$, seq$,       /* Invoice and Line #s        */~
                     shipdate$,          /* Date Shipped               */~
                     rsncode$,           /* Invoice Reason Code        */~
                     invshipd,           /* Qty Shipped                */~
                     sagross,            /* Gross Value                */~
                     line_ext,           /* Net Value                  */~
                     temp,               /* Returned (Total Cost/unit) */~
                     " ",                /* Stocked Item Flag          */~
                     part$,              /* Part Number                */~
                     cat$,               /* Part Category              */~
                     acctxref$,          /* Customer Account           */~
                     cuscode$,           /* Ship-to Customer Number    */~
                     custype$,           /* Ship-to Customer Type      */~
                     store$,             /* Shipping Store             */~
                     region$,            /* Region Code                */~
                     salesman$,          /* Salesman Code              */~
                     invcost,            /* Inventory Cost             */~
                     currency$,          /* Currency                   */~
                     convdate%,          /* Convertion Eff. Date       */~
                     conveqv,            /* Convertion Factor          */~
                     convunt,            /* Convertion Factor          */~
                     cprice1,            /* Trans. Price (Stcking)     */~
                     cprice2,            /* Trans. Price (Prcing)      */~
                     project$,           /* Project Number             */~
                     sales$,             /* Sales Account              */~
                     fy_period$,         /* Fiscal Yr period           */~
                     fy_start$,          /* Fiscal Yr start date       */~
                     temp2,              /* Temp for MDMC              */~
                     " "                 /* Filler                     */

            call "SAPOSTSB" (sa$(), #02, #04)
            get sa$() using L12055, stdcost, mdmc
L12055:         FMT POS(84), PD(14,4), POS(226), PD(14,4)
            stdcost = stdcost * invshipd
            dmc = mdmc  /* For ARILINES */

*        Secondly, the Bookings Transaction (if so required)
            if soseq$ <> " " or invorder = 0 then return
                sagross = round(invorder * pricestk, 2)
                sadisc  = round(sagross  * line_disc_pct * .01, 2)
                sanet   = sagross - sadisc
                str(sa$(),,1) = "4"    /* Invoice Booking Transaction */
                put sa$() using L12140, invorder, sagross, sanet, -1
L12140:              FMT POS(60), 3*PD(14,4), POS(153), PD(14,4)
                call "SAPOSTSB" (sa$(), #02, #04)
                return

L12180:     FMT CH(1), CH(7), CH(6), CH(16), CH(3), CH(8), CH(3), CH(6), ~
                CH(9), 4*PD(14,4), CH(1), CH(25), CH(4), 2*CH(9), CH(2), ~
                CH(3), 2*CH(4), PD(14,4), CH(4), BI(4), 2*PD(14,7),      ~
                2*PD(14,4), CH(8), CH(9), CH(2), CH(6), PD(14,4), CH(30)

*       ****************************************************************
        update_project
*        Update project for this invoice line
            return
REM         if project$ = " " or f2%(26) <> 0% then return
REM         disc_amt_this_line = round(line_ext * invd_pct * .01, 2)
REM         jsalpost_amt = line_ext - disc_amt_this_line
REM         call "JSALPOST" (project$, cuscode$, invnr$, invdate$,       ~
REM                          po$, part$, invshipd, jsalpost_amt,         ~
REM                          sales$, #26, #27, f2%(26), f2%(27), ret%)
REM         return

*       ****************************************************************
        update_gl_line
*        Write Transaction file for this line's GL Postings.
*        If POSTDISC$ = "Y" then Line Discounts are posted separately
*        and the Gross Line Extension is posted; else JUST the Net Line
*        Extension is posted.

            postdescr$ = part$
            postseq$   = seq$

            linesinv = linesinv + line_ext  /* For testing rounding Err*/

            if postdisc$ = "Y" then L12460   /* Post Disc, Gross Extsn  */
                postamt = -line_ext
                goto L12520                  /* CR Net Extension Only   */

L12460:     postamt   = -line_disc_amt      /* DR Line Discount        */
            postacct$ = discs$
            posttype$ = "DISC"
            tran_type$ = "RIN02"
            gosub write_gl_tf

            postamt   = (line_ext - line_disc_amt) * (-1) /* Gross Ext */
L12520:     postacct$ = sales$
            posttype$ = "SALE"
            tran_type$ = "RIN01"

            if mgtrpt_on$ <> "Y" then L12540  /* Skip Management Rpting */
            if mdmc = -1 then L12540  /* No Mgt DMC on file for part    */
                call "MGTVALUS" ("I", cuscode$, part$, invdate$, factor, ~
                                 acct1$, acct2$, icc$, #03, #04, #02)
                msval = postamt : msvol = invshipd
                if icc$ = " " then L12540  /* Regular Sale */
                if factor = -999 then factor = 1  /* Factor not found */
                msval = mdmc * factor * invshipd * (-1)
                if acct1$ <> " " and acct2$ <> " " then gosub xtra_mgt_rec
L12540:     gosub write_gl_tf

            if mgtrpt_on$ = "Y" then gosub init_mgt
            return

        adjust_for_rounding
            round_error = round(grossinv, 2) - round(linesinv, 2)
            if round_error = 0 then return
                postdescr$ = "ROUNDING ERROR ADJ."
                postamt    = -round_error
                postacct$  = rounding_acct$
                posttype$  = "RND "
                tran_type$ = "RIN10"
                gosub write_gl_tf
                return

*       ****************************************************************
        write_gl_tf
*        Common spot to update G/L Transaction File ARIJRLTF.  An EOD
*        branch indicates that we have already written the record.
            dr, cr = 0
            if postamt > 0 then dr = postamt else cr = -postamt
            if dr = 0 and cr = 0 then return
L12645:     if export_on$ = "Y" then gosub load_gl_info
                write #28 using L12690, userid$, session$, "D", postacct$,~
                        invnr$, cuscode$, postseq$, posttype$, billxref$,~
                        invtype$, store$, postdescr$, dr, cr, invuser$,  ~
                        project$, " ", gl_post_info$(), eod goto L12720
L12690:             FMT CH(3), CH(6), CH(1), CH(9), CH(8), CH(9), CH(3), ~
                        CH(4), CH(9), CH(1), CH(3), CH(25), 2*PD(14,4),  ~
                        CH(3), CH(8), CH(20), 2*CH(255)
L12720:         return

*       ****************************************************************
        toss_line
*        Move Invoice Line to Master file (along with text)
            get #10 using L12780, str(invline$())
L12780:         FMT CH(750)
            put #06 using L12780, str(invline$())
            put #06 using L12792, invcost, stdcost, dmc, itemid$, unitid$, ~
                                           skuno$
/* (AWD015) additional fields */
L12792:         FMT POS(630), 2*PD(14,4), POS(647), PD(14,4), POS(659), CH(10),~
                       CH(10), CH(09)
            write #06, eod goto L12810
L12810:     call "TXTFUTIL" (#18, f2%(18), "TOSS", linetextid$)
                                              /* (EWD) - Begin         */
            if schema% = 2% then gosub updateRemoteOrderLine    /* CR2173 */
            gosub update_marketing            /* (EWD004)              */
            gosub update_subpart              /* CR347 */

*        Update APC EDI Master File           /* Last Mod - 08/14/96   */
            init(" ") edi_id$                 /* REASON CODE (30) SKIP */
            read #3,key = edi_cus$, using L12815, edi_id$, eod goto L12820
L12815:        FMT POS(1000), CH(3)
L12820:     if len(edi_id$) < 2    then L12840  
               edi_rsn% = 0%
               convert str(rsncode$,1%,2%) to edi_rsn%, data goto L12822
L12822:                                        /* SKIP 30 THRU 34      */
              if edi_rsn% > 29% and edi_rsn% < 35% then goto L12840
              if edi_rsn% = 2% then goto L12840   /* (EWD002)          */ 

        REM Mod to stop all CREDITS for Lows's and HQ - 06/25/97
        REM ADJUSTMENTS continue to be sent for Lowes

REM            if edi_id$    > "002" then goto L12825       /* (EWD006) */
               if invtype$    = "C"   then L12840 /* Low's and HQ Skip */
               if invtype$    = "A"   then L12840 /* Credits & Adjust  */

L12825:     if edi_id$    <> "001" then L12830
               

L12830:    if edi_id$    <> "010" then L12835     /* (EWD005)  */
               if invtype$ = "A"  or  invtype$ = "C"  then L12840
               call "EWDFTP01" (#9, #10, #3, #56,#08, #60, #04, #61, ~
                                #18, edi_cus$, edi_inv$, edi_seq$)
               goto L12840                         /* (EWD005)  */     

                                                   /*  (AWD005)  */
L12835:    if edi_id$    <> "020" then L12836
              if invtype$ = "A"  or  invtype$ = "C"  then L12840
              call "AWDEDI01" (#9, #10, #3, #56, #7, #62, #58, #59, #04,   ~
                             edi_cus$, edi_inv$, " ", " ", edi_seq$, "L", 0)
               goto L12840               /* (AWD007)  */  
   
L12836:    if edi_id$    <> "030" then L12842
              if invtype$ = "A" or invtype$ = "C" then L12840
              call " AWDIBS04" (#9, #10, #3, #56, #7, #63, #58, #59, #04,  ~
                               edi_cus$, edi_inv$, " ", " ", edi_seq$, "L", 0)

L12842:    if edi_id$ < "040" or edi_id$ > "041" then goto L12843    /* (AWD017) */
             call "APCEDI10" (#9, #10, #3, #56, #7, #22, #58, #59, #04,   ~
             #53, #54, edi_cus$, edi_inv$, " ", " ", edi_seq$, "L", invSys$)
                                                  /* (EWD008)          */                                          
L12843:    if edi_id$ < "042" or edi_id$ > "043" then goto L12844    /* (AWD017) */
             call "APCEDI10" (#9, #10, #3, #56, #7, #48, #58, #59, #04,   ~
             #53, #54, edi_cus$, edi_inv$, " ", " ", edi_seq$, "L", invSys$)

L12844:    if edi_id$ < "044" or edi_id$ > "044" then goto L12845    /* (AWD017) */
             call "APCEDI10" (#9, #10, #3, #56, #7, #49, #58, #59, #04,   ~
             #53, #54, edi_cus$, edi_inv$, " ", " ", edi_seq$, "L", invSys$)

L12845:    if edi_id$ < "047" or edi_id$ > "047" then goto L12848    /* (AWD017) */
             call "APCEDI10" (#9, #10, #3, #56, #7, #26, #58, #59, #04,   ~
             #53, #54, edi_cus$, edi_inv$, " ", " ", edi_seq$, "L", invSys$)

L12848:    if edi_id$ < "048" or edi_id$ > "059" then goto L12849
REM          CALL "APCEDI10" (#9, #10, #3, #56, #7, #27, #58, #59, #04, #53, #54, EDI_CUS$, EDI_INV$, " ", " ", EDI_SEQ$, "L")

L12849:    if edi_id$ <> "002" then goto L12840
              call "APCEDI03" (#9, #10, #3, #56, #7, #57, #58, #59, #04,   ~
                            edi_cus$, edi_inv$, " ", " ", edi_seq$, "L", #27)
                                                  /* (EWD003)          */
                                                                     /* (AWD017) */
L12840:    if edi_id$ > "049" and edi_id$ < "060" then                     ~
             call "APCEDI10" (#9, #10, #3, #56, #7, #26, #58, #59, #04,   ~
             #53, #54, edi_cus$, edi_inv$, " ", " ", edi_seq$, "L", invSys$)


            call "DELETE" (#10, str(invline$(),,20), 20%)
                                                  /* (EWD) - End       */
*        Kill the BOL Line if it exists
            if bolline% = 0% then toss_core
                readkey$ = str(so$) & str(bol$) & soseq$
                call "DELETE" (#12, readkey$, 22%)

        toss_core
            if core_bank% = 0% then return
            init (" ") corebank$()
            get #6 using L12846, str(corebank$(),51%)
L12846:         FMT CH(750)
            put str(corebank$()) using L12850, po$, so$, invdate$
L12850:         FMT POS(801), 2*CH(16), CH(6)
            put str(corebank$()) using L12856, userid$, session$,         ~
                postdate$, glpost$, hex(00)
L12856:         FMT CH(3), 2*CH(6), 2*CH(1)
L12858:     call "GETDTTM" addr(str(corebank$(), 18%, 7%))
            write #50 using L12862, str(corebank$()), eod goto L12858
L12862:         FMT CH(1000)
            return

        update_marketing                                 /* (EWD004)  */
            init(" ") dt_key$
            str(dt_key$,1%,8%) = str(so$,1%,8%)
            str(dt_key$,9%,3%) = seq$
            read #52,hold,key 1% = dt_key$, eod goto L12875
               put #52, using L12870, "Y"   /* Prod has been Invoiced */ 
L12870:            FMT POS(125), CH(1)
               rewrite #52  
L12875: return
/* CR2173 */
        updateRemoteOrderLine                                  /* (CR2173)  */
          convert str(invline$(),19%,2%) to pg_line%
          convert pg_line% to pg_line$, pic(00) 
          call "APCORLNS" (cuscode$, so$, pg_line$, "20", "ARIUPDTE", ~
                           #38, error%)
          error% = 0%     
        return
   
*       ****************************************************************
        adjust_order
*        Recalculate Order value now that the lines have been adjusted.
            oldorderamt, neworderamt, adjorder, total_li_qty = 0
            if soonfile% = 0% then return   /* No Sales Order  */

            readkey$ = str(cuscode$) & str(so$)
            call "READ101" (#07, readkey$, f1%(7))  /* BCKMASTR */
            if f1%(7) = 0% then return    /* Well, we tried    */

            get #07 using L13010, sohdiscpct, oldorderamt, crhold$
L13010:         FMT POS(859), 2*PD(14,4), CH(1)
            plowkey$ = str(so$)
L13030:     call "PLOWNEXT" (#08, plowkey$, 16%, f1%(8)) /* BCKLINES */
            if f1%(8) = 0% then L13130
                get #08 using L13060, soopen, stkprice, sodiscpct
L13060:              FMT POS(109), PD(14,4), POS(141), PD(14,4),         ~
                         POS(173), PD(14,4)
                ext         = round(soopen * stkprice    , 2)
                disc        = round(ext * sodiscpct * .01, 2)
                neworderamt = neworderamt + (ext - disc)
                total_li_qty = total_li_qty + soopen
                goto L13030

L13130:     if crhold$ = "H" and total_li_qty = 0 then crhold$ = " "
            put #07 using L13140, neworderamt, crhold$
L13140:         FMT POS(867), PD(14,4), CH(1)
            rewrite #07
            adjorder = neworderamt - oldorderamt
            adjorder = adjorder - round(adjorder * sohdiscpct * .01, 2)
            return

*       ****************************************************************
        delete_bol
*        Remove left over Bill of Lading from SHPHDRS and SHPLINES.
*        If the BOL Line was NOT processed with the Invoice, and it was
*        pre-invoiced, then reverse the Pre-invoicing Inventory Postings
            if bolonfile% = 0% then return

L13270
*        Need to reverse pre-invoicing inventory postings on lines
L13280:     init (" ") newlot$()
            mat newlotqty = zer
            plowkey$ = str(so$) & str(bol$) & hex(00)
            call "PLOWNXT1" (#12, plowkey$, 19%, f1%(12))
            if f1%(12) = 0% then L13770  /* Kill header and get out     */

*        Adjust Sales Order line and, if per-invoiced, inventory & SNs.
            get #12 using L13370, soseq$, bolschld, oldlot$(),            ~
                                 oldlotqty()
L13370:         FMT POS(29), CH(3), PD(14,4), 30*CH(6), 30*PD(14,4)
            delete #12
            readkey$ = str(so$) & soseq$
            call "READ101" (#08, readkey$, f1%(8))   /* BCKLINES */
            if f1%(8) = 0% then L13280               /* Ouch!!!! */
            get #08 using L13430, part$, soschld, sopreinv, pricestk
L13430:         FMT POS(32), CH(25), POS(117), PD(14,4), POS(133),       ~
                                                               2*PD(14,4)
            soschld = max(0, soschld - bolschld)

            init(" ") so_inv$, item_no$
            so_inv$  = so$
            item_no$ = soseq$
            gosub lookup_subpart
            subp$ = str(bcksubpt_rec$,48%,20%)

            if bolshipdate$ <> " " and bolshipdate$ <> blankdate$ then ~
                                   sopreinv = max(0, sopreinv - bolschld)
            put #08 using L13490, soschld, sopreinv
L13490:         FMT POS(117), PD(14,4), POS(133), PD(14,4)
            rewrite #08
            if bolshipdate$ = " " or bolshipdate$ = blankdate$ then L13270
* PAR000
                call "SHPHNYSB"  ("I", store$, part$,  subp$,            ~
                                                seq$, soseq$, cuscode$,  ~
                                  so$, bol$, invnr$, pricestk, postdate$,~
                                  oldlot$(), oldlotqty(),                ~
                                  newlot$(), newlotqty(), temp,          ~
                                  invuser$, #19, #02, #23, #20, #24, #21,~
                                  #04, #15, #25, #13, #14)
                readkey$ = "RS" & str(so$) & str(bol$) & str(soseq$) &   ~
                                                                hex(0000)
L13620:         call "PLOWNXT1" (#41, readkey$, 24%, f1%(41))
                if f1%(41) = 0% then L13270
                     get #41 using L13660, str(readkey2$,26),             ~
                                          str(readkey2$,,25)
L13660:                   FMT POS(43), CH(20), CH(25)
                     delete #41
                     call "READ101" (#40, readkey2$, f1%(40))
                     if f1%(40) = 0% then L13620
                          get #40 using L13710, readkey2$
L13710:                        FMT POS(259), CH(30)
                          put #40 using L13730, "2", readkey2$
L13730:                        FMT POS(1), CH(1), POS(2), CH(30)
                          rewrite #40
                          goto L13620

L13770
*        And lastly get rid of BOL Heading
            readkey$ = str(cuscode$) & str(so$) & str(bol$)
            call "DELETE" (#11, readkey$, 28%)
            return

*       ****************************************************************
        update_gl_header
*        Update G/L Transaction File with Header Level figures.
            postseq$, project$ = " "
            part$, cat$, generic$, prtclass$, prttype$, uom$ = " "
* PAR000
            subp$, epart$ = " "
            invshipd = 0

*        Net Invoice Amount
            postdescr$ = "NET INVOICE"
            if invtype$ = "C" then postdescr$ = "CREDIT MEMO"
            if artype$  = "A" then postdescr$ = postdescr$ & " (A/R)"
            if artype$  = "C" then postdescr$ = postdescr$ & " (CASH)"
            if artype$  = "E" then postdescr$ = postdescr$ & " (EXPENSE)"
            if invtype$ = "F" then postdescr$ = "FINANCE CHARGE"
            postamt   = netinv - gain_loss /* Debit to A/R or ... */
            postacct$ = invacct$
            posttype$ = "NET"
            tran_type$ = "RIN03"
            gosub write_gl_tf

            postdescr$ = "EX. GAIN/LOSS"
            postamt   = gain_loss
            postacct$ = gain_loss$
            posttype$ = "G/L"
            tran_type$ = "RIN07"
            gosub write_gl_tf

*        Invoice Level Discounts
            postdescr$ = "INVOICE DISCOUNTS"
            postamt    = -invdisc        /* Debit */
            postacct$  = discacct$
            posttype$  = "DISC"
            tran_type$ = "RIN04"
            gosub write_gl_tf

*        Sales Tax
            postdescr$ = "SALES TAX"
            postamt    = -taxamt         /* Credit */
            postacct$  = taxacct$
            posttype$  = "TAX "
            tran_type$ = "RIN05"
            gosub write_gl_tf

*        Freight
            postdescr$ = "FREIGHT"
            postamt    = -frtamt         /* Credit */
            postacct$  = frtacct$
            posttype$  = "FRT "
            tran_type$ = "RIN06"
            gosub write_gl_tf

*        Energy
            if escamt <= 0.00 and invtype$ <> "C" then return

            postdescr$ = "ENERGY"
            postamt    = -escamt
            postacct$  = escacct$
            posttype$  = "ENER"
            tran_type$ = "RIN06"            /*  ?????    */
            gosub write_gl_tf

            return

        load_gl_info

            put str(gl_post_info$(),,) using L14690,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                currency$,               /* Currency code CH(4)        */~
                convunt,                 /* Currency units per book    */~
                postamt,                 /* Functional Currency amount */~
                invshipd,                /* Unit amount                */~
                cuscode$,                /* Customer code CH(9)        */~
                so$,                     /* Sales Order number CH(16)  */~
                bol$,                    /* BOL number CH(3)           */~
                custype$,                /* Customer Type CH(2)        */~
                str(shipto$,169,2),      /* State CH(2)                */~
                country$,                /* Country CH(3)              */~
                str(shipto$,172,9),      /* ZIP CH(9)                  */~
                region$,                 /* Sales Region CH(4)         */~
                taxcode$,                /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                salesman$,               /* Salesman code 3*CH(3)      */~
                invnr$,                  /* Invoice Number CH(8)       */~
                part$,                   /* Part Number CH(25)         */~
                cat$,                    /* Part Category CH(4)        */~
                prtclass$,               /* Part Class CH(4)           */~
                generic$,                /* Part Generic code CH(16)   */~
                prttype$,                /* Part Type CH(3)            */~
                uom$,                    /* Part UOM CH(4)             */~
                store$,                  /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                project$,                /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                mdmc,                    /* Mgt DMC PD(14,4)           */~
                msval,                   /* Mgt Sales Value  PD(15,4)  */~
                msvol,                   /* Mgt Sales Volume PD(15,4)  */~
                " ",                     /* Filler                     */~
                icc$,                    /* Intercompany Corp CH(6)    */~
                acct1$,                  /* Intradiv Sales Acct CH(9)  */~
                acct2$,                  /* Alt Owner Sales Acct CH(9) */~
                " ",                     /* Intradiv CoGS Acct CH(9)   */~
                " ",                     /* Alt Owner CoGS Acct CH(9)  */~
                postacct$,               /* CMS Posting Account        */~
                " "                      /* Filler                     */

            return

L14690: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Currency conversion        */~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice CH(16)      */~
                CH(8),                   /* Check Payment Number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                PD(14,4),                /* Mgt DMC PD(14,4)           */~
                PD(15,4),                /* Mgt Sales Value  PD(15,4)  */~
                PD(15,4),                /* Mgt Sales Volume PD(15,4)  */~
                CH(8),                   /* Filler                     */~
                CH(6),                   /* Intercompany Corp CH(6)    */~
                CH(9),                   /* Intradiv Sales Acct CH(9)  */~
                CH(9),                   /* Alt Owner Sales Acct CH(9) */~
                CH(9),                   /* Intradiv CoGS Acct CH(9)   */~
                CH(9),                   /* Alt Owner CoGS Acct CH(9)  */~
                CH(9),                   /* CMS Posting Account CH(9)  */~
                CH(108)                  /* Filler                     */

*       ****************************************************************
        xtra_mgt_rec  /* Type 3 transaction creates extra mgt records. */
            save_postamt    = postamt
            save_invshipd   = invshipd
            save_postacct$  = postacct$
            save_tran_type$ = tran_type$
            dr, cr, invshipd, postamt = 0

*        Set up for Intra-Divisional Sales Record
            postacct$ = acct1$
            tran_type$ = "RIN08"
            gosub L12645  /* WRITE_GL_TF without DR/CR = 0 test  */

*        Set up for Alternate Owner Sales Record
            postacct$ = acct2$
            tran_type$ = "RIN09"
            msval = save_postamt
            gosub L12645  /* WRITE_GL_TF without DR/CR = 0 test  */

            postamt    = save_postamt
            invshipd   = save_invshipd
            postacct$  = save_postacct$
            tran_type$ = save_tran_type$
            msval, msvol = 0
            return

*       ****************************************************************
        init_mgt      /* Init Mgt Values so they don't hang on.        */
            init (" ") icc$, acct1$, acct2$
            mdmc, msval, msvol = 0
            factor = 1
            return

*       ****************************************************************
        update_trial_balance /* Update Open Trial Balance for Bill-to  */
*        First update payment terms for non-dated invoice
            gain_loss, invbal = 0
            if terms$ = "DATED" then L21100   /* Already done in input  */
                terms_due(1) = netinv  :  grace% = 0%
                ctermsdue(1) = cnetinv
                call "ARMTCDTE" (terms$, invdate$, terms_pct(1),         ~
                                 terms_disc$(1), terms_net$(1), grace%)

L21100:     if artype$ <> "A" or aractive$ <> "Y" then return
            invbal = netinv

*        Set the Settlement Transaction Type per the Invoice Type
            stlmnt_type$ = "I"
            if invtype$  = "A" then stlmnt_type$ = "A"
            if invtype$  = "C" then stlmnt_type$ = "C"
            if invtype$  = "F" then stlmnt_type$ = "F"

*        Next update the Trial Balance with this transaction
            if stlmnt_type$ = "I" or stlmnt_type$ = "F" or               ~
                                                 stlmnt$ = " " then L21400

*        Applied transaction. Assign Trans ID and write record
L21240:     call "ARMSTLNO" (billxref$, stlmnt$, #29, #02, ret%)
            if ret% = 1% then L21390  /* '00' missing, make unapplied */
            readkey$ = str(billxref$,,9) & str(stlmnt$,,10) & "00"
            call "READ100" (#43, readkey$, app_other_currency%)
               if app_other_currency% <> 0% then apply_other
            p% = 1%  :  gosub tb_put
            write #29, eod goto L21240
            if other_currency% = 0% then return
            write #44 using L21350, currency$, billxref$, stlmnt$,        ~
                                   ctermsdue(p%), conveqv, convunt,      ~
                                   convdate$, terms_due(p%), 0, " "
L21350:     FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6),       ~
                2*PD(14,4), CH(29)
            return

L21390
*        Unapplied Transactions
L21400:     stlmnt$  = str(invnr$,,8) & "0000"
            readkey$ = str(billxref$,,9) & str(stlmnt$,,8)
            call "PLOWNEXT" (#29, readkey$, 17%, f1%(29))
            if f1%(29) = 0% then L21460
L21440:         stlmnt$ = "U"
                call "ARMSTLNO" (billxref$, stlmnt$, #29, #02, ret%)
L21460:     if terms_net$(2%) = " " or terms_net$(2%) = blankdate$ then ~
                                              L21510       /* NOT Dated Terms */
                   grace% = 0%
                   str(stlmnt$,9,2) = "01"
                   if terms_disc$(1%) = " " or terms_disc$(1%) = blankdate$ ~
                        then terms_disc$(1%) = terms_net$(1%)
L21510:         p% = 1%  :  gosub tb_put        /* Single Payment */
                write #29, eod goto L21440
                gosub write_tb_curr_exposure
                if terms_net$(2%) = " " or terms_net$(2%) = blankdate$ ~
                                              then return  /* NOT Dated Terms */

                grace% = 0%
                for p% = 2% to 30%
                     if terms_net$(p%) = " " or terms_net$(p%) = blankdate$ ~
                                              then return
                          if terms_disc$(p%) = " " or ~
                             terms_disc$(p%) = blankdate$ then ~
                                terms_disc$(p%) = terms_net$(p%)
                          convert p% to str(stlmnt$,9,2), pic(00)
                          gosub tb_put
                          gosub write_tb_curr_exposure
                          write #29
                next p%
                str(stlmnt$,9,2) = "##"
                return

        tb_put  /* Put record into buffer for File #29 -- ARMTRIAL     */
            if terms_disc$(p%) = " " then terms_disc$(p%) = blankdate$
            if terms_net$(p%)  = " " then terms_net$(p%)  = blankdate$

            put #29 using L21900, billxref$, stlmnt$,                     ~
                                 terms_pct(p%), terms_disc$(p%), grace%, ~
                                 terms_net$(p%), po$, " ", " ",          ~
                                 terms_due(p%), invacct$, "01", "I",     ~
                                 stlmnt_type$, invnr$, postdate$,        ~
                                 invdate$, cuscode$, so$, bol$, store$,  ~
                                 terms_pct(p%), terms_disc$(p%), grace%, ~
                                 terms_net$(p%), po$, currency$,         ~
                                 convdate$, conveqv, convunt, " "
            return

        write_tb_curr_exposure
            if other_currency% = 0% then return
            write #43 using L21860, currency$, billxref$, stlmnt$,        ~
                                   ctermsdue(p%), conveqv, convunt,      ~
                                   convdate$, " "
L21860:     FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6), CH(45)

            return

L21900:         FMT        /* File #29 -- ARMTRIAL                     */~
                    CH(9),               /* Bill-to Number             */~
                    CH(12),              /* Settlement Number          */~
                    PD(14,4),            /* Cash Discount Percent      */~
                    CH(6),               /* Cash Discount Due Date     */~
                    BI(1),               /* Grace Days                 */~
                    CH(6),               /* Net Payment Due Date       */~
                    CH(16),              /* Customer PO Number         */~
                    CH(3),               /* Last Modified- User ID     */~
                    CH(6),               /*                Date        */~
                    PD(14,4),            /* Transaction Amount         */~
                    CH(9),               /* A/R Debit Account          */~
                    CH(2),               /* Next Transaction ID        */~
                    CH(1),               /* Source Code                */~
                    CH(1),               /* Type Designator            */~
                    CH(8),               /* Source Document Number     */~
                    CH(6),               /* Post Date                  */~
                    CH(6),               /* Source Document Date       */~
                    CH(9),               /* Ship-to Customer Number    */~
                    CH(16),              /* Sales Order Number         */~
                    CH(3),               /* Bill-of-Lading Suffix      */~
                    CH(3),               /* Store Shipped From         */~
                    PD(14,4),            /* Original Cash Discount %   */~
                    CH(6),               /*          Disc Due Date     */~
                    BI(1),               /*          Grace Days        */~
                    CH(6),               /*          Net Due Date      */~
                    CH(16),              /*          Customer PO       */~
                    CH(4),               /* Currency code              */~
                    CH(6),               /* Currency conversion date   */~
                    PD(14,7),            /* Conversion factor          */~
                    PD(14,7),            /* # currency units           */~
                    CH(54)               /* Filler                     */

        apply_other

            get #43 using L22260, tcurrency$, tconveqv, tconvunt, cdate$
L22260:         FMT CH(4), POS(34), 2*PD(14,7), CH(6)
            gain_loss$ = " "
            call "READ100" (#46, tcurrency$, f1%(46))
               if f1%(46) = 0% then L22270
            get #46 using L22265, gain_loss$
L22265:         FMT POS(41), CH(9)
L22270:     if tcurrency$ = currency$ then same_currency

            readkey$ = str(currtype$,,1) & str(tcurrency$,,4)
            call "DATREVRS" ( invdate$, str(readkey$, 6%, 6%), " " )
            cdate1$ = cdate$:tconveqv1 = tconveqv:tconvunt1 = tconvunt
            call "PLOWNEXT" (#45, readkey$, 5%, f1%(45))
               if f1%(45) = 0% then L22380
            get #45 using L22370, cdate1$, tconveqv1, tconvunt1
L22370:         FMT POS(12), CH(6), 2*PD(14,7)
L22380:     tnetinv = netinv * tconvunt1
            statutory = tnetinv * tconveqv
            gain_loss = netinv - statutory

            if terms_disc$(p%) = " " then terms_disc$(p%) = blankdate$
            if terms_net$(p%)  = " " then terms_net$(p%)  = blankdate$

            write #29 using L21900, billxref$, stlmnt$,                   ~
                                 terms_pct(p%), terms_disc$(p%), grace%, ~
                                 terms_net$(p%), po$, " ", " ",          ~
                                 statutory, invacct$, "01", "I",         ~
                                 stlmnt_type$, invnr$, postdate$,        ~
                                 invdate$, cuscode$, so$, bol$, store$,  ~
                                 terms_pct(p%), terms_disc$(p%), grace%, ~
                                 terms_net$(p%), po$, currency$,         ~
                                 convdate$, conveqv, convunt, " "

            write #43 using L22550, tcurrency$, billxref$, stlmnt$,       ~
                                   tnetinv, tconveqv, tconvunt, cdate$,  ~
                                   " "
L22550:     FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6), CH(45)

            write #44 using L22610, currency$, billxref$, stlmnt$,        ~
                                   cnetinv, conveqv, convunt,            ~
                                   convdate$, netinv, gain_loss,         ~
                                   tcurrency$, tconveqv1, tconvunt1,     ~
                                   cdate1$, " "
L22610:     FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6),       ~
                2*PD(14,4), CH(4), 2*PD(14,7), CH(6), CH(3)
            invbal = invbal - gain_loss
            return

        same_currency
            statutory = cnetinv * tconveqv
            gain_loss = netinv - statutory

            if terms_disc$(p%) = " " then terms_disc$(p%) = blankdate$
            if terms_net$(p%)  = " " then terms_net$(p%)  = blankdate$

            write #29 using L21900, billxref$, stlmnt$,                   ~
                                 terms_pct(p%), terms_disc$(p%), grace%, ~
                                 terms_net$(p%), po$, " ", " ",          ~
                                 statutory, invacct$, "01", "I",         ~
                                 stlmnt_type$, invnr$, postdate$,        ~
                                 invdate$, cuscode$, so$, bol$, store$,  ~
                                 terms_pct(p%), terms_disc$(p%), grace%, ~
                                 terms_net$(p%), po$, currency$,         ~
                                 convdate$, conveqv, convunt, " "

            write #43 using L22870, currency$, billxref$, stlmnt$,        ~
                                   cnetinv, tconveqv, tconvunt, cdate$,  ~
                                   " "
L22870:     FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6), CH(45)

            write #44 using L22920, currency$, billxref$, stlmnt$,        ~
                                   cnetinv, conveqv, convunt,            ~
                                   convdate$, netinv, gain_loss, " "
L22920:     FMT CH(4), CH(9), CH(12), PD(14,4), 2*PD(14,7), CH(6),       ~
                2*PD(14,4), CH(29)
            invbal = invbal - gain_loss
            return

*       ****************************************************************
        update_customer
*        SHIP-TO: Update Open Order Dollars
*        BILL-TO: Update Open Order Dollars and Open A/R Dollars

*        First the SHIP-TO
            if billxref$ = cuscode$ then L23210    /* Bill-to = Ship-to */
L23070:         call "READ100" (#03, cuscode$, f1%(3%))
                if f1%(3%) = 1% then L23140
L23085:              u3% = 2%
        init(" ") key_data$
    str(key_data$,01,01) = hex(22)          
    str(key_data$,02,60) =                                 ~
                         "Ship-to Customer " & cuscode$ & " is Missing!" & ~
                         "  Invoice: " & invnr$
    str(key_data$,78,01) = hex(22)          
REM        call "ARIMAIL" (key_data$)                               
        return% = 1%
        goto exit_program

                     call "ASKUSER" (u3%, "DATA BASE ERROR",             ~
                         "Ship-to Customer " & cuscode$ & " is Missing!"&~
                         "  Invoice: " & invnr$,                         ~
                         "CUSTOMER file may be corrupt.  " &             ~
                         "This condition must be rectified immediately.",~
                         "Press PF1 to Try Again;  Press PF16 to Abort.")
                     if u3% = 1% then L23070
                     if u3% <> 16% then L23085
                          return% = 1%
                          goto exit_program
L23140:         gosub update_cust_ship_to/* Ship-To updated in CCRMASTR */

L23210
*        And Now The Bill-to
L23220:         call "READ100" (#03, billxref$, f1%(3%))
                if f1%(3%) = 1% then L23290
L23235:              u3% = 2%
        init(" ") key_data$
    str(key_data$,01,01) = hex(22)          
    str(key_data$,02,60) =                                           ~
                         "Bill-to Customer " & billxref$ &" is Missing!"&~
                         "  Invoice: " & invnr$
    str(key_data$,17,06) = session$           
    str(key_data$,78,01) = hex(22)          
REM     call "ARIMAIL" (key_data$)                               
    return% = 1%
    goto exit_program

                     call "ASKUSER" (u3%, "DATA BASE ERROR",             ~
                         "Bill-to Customer " & billxref$ &" is Missing!"&~
                         "  Invoice: " & invnr$,                         ~
                         "CUSTOMER file may be corrupt.  " &             ~
                         "This condition must be rectified immediately.",~
                         "Press PF1 to Try Again;  Press PF16 to Abort.")
                     if u3% = 1% then L23220
                     if u3% <> 16% then L23235
                          return% = 1%
                          goto exit_program
L23290:         get #03 using L23310, billsold$, billship$
L23310:              FMT POS(40), CH(180), POS(253), CH(180)
                if billsold$ = " " or billsold$ = "BILL-TO" then         ~
                                                   billsold$ = billship$
                gosub update_cust_bill_to/* Bill-To updated in CCRMASTR */
                return

*       ****************************************************************
        update_invoice_header
*        Finish Off the Invoice Header and Toss It and it's text
            get #09 using L24040, str(invhdr$())  /* ARIBUFFR */
L24040:         FMT CH(2000)
            /* Stuff 4th Alt Key just in case */
            str(invhdr$(), 1783%, 26%) = str(invhdr$(), 849%,  9%) &     ~
                                         str(invhdr$(),   1%, 17%)
            put #05 using L24040, str(invhdr$())  /* ARIMASTR */

            if soldto$ = "BILL-TO" then soldto$ = billsold$
            if soldto$ = " "      then soldto$ = shipto$

            for p% = 1% to 30%
               if terms_disc$(p%) = " " then terms_disc$(p%) = blankdate$
               if terms_net$(p%)  = " " then terms_net$(p%)  = blankdate$
            next p%
 

/* (AWD011) make invoice date same as postdate$ */
/* CR1993 */
            put #05 using L24130, soldto$, postdate$, postdate$, netinv, ~
                                0, stlmnt$,                              ~
                                glpost$, terms_due(), terms_pct(),       ~
                                terms_disc$(), terms_net$(), esc,        ~
                                escamt, escacct$,  tax_rate, pgpo$,      ~
                                hdmet, hdidx
                                
L24130:         FMT POS( 233), CH(180)    , POS( 521), CH(6),  /* (AWD011) */~
                    POS( 533), CH(6),                                    ~
                    POS( 833), PD(14,4),                                 ~
                    POS( 841), PD(14,4)   , POS( 858), CH(12),           ~
                    POS( 906), CH(1)      , POS( 928), 30*PD(14,4),      ~
                    POS(1168), 30*PD(14,4), POS(1408), 30*CH(6),         ~
                    POS(1588), 30*CH(6), POS(1811), PD(14,4), PD(14,4),  ~
                    CH(09),PD(14,4), CH(20), PD(14,4), PD(14,4)


            write #05, eod goto L24190
L24190:     call "TXTFUTIL" (#18, f2%(18), "TOSS", textid$)

            gosub update_group                      /* (EWD) - Mod  */

*        Update Invoice Register File
            write #30 using L24260, userid$, session$, invnr$, cuscode$,  ~
                                   billxref$, str(shipto$,,30), invtype$,~
                                   store$, so$, bol$, invdisc, frtamt,   ~
                                   taxamt, netinv, escamt, eod goto L24290
                                   
/* (AWD016) mod for esc amt */                                   
L24260:         FMT CH(3), CH(6), CH(8), CH(9), CH(9), CH(30), CH(1),    ~
                    CH(3), CH(16), CH(3), 4*PD(14,4), PD(14,4)

L24290
*        Update Invoice Print File
            if invtype$ = "X" then L24342
            if pos(prnttype$ = invtype$) = 0% then L24342
            write #31 using L24340, userid$, cuscode$, invnr$,            ~
                                   eod goto L24342
L24340:         FMT CH(3), CH(9), CH(8)
                                               /* (EWD) - Begin        */
L24342
*        Update EDI Master File                /* Last Mode - 08/14/96 */
            init(" ") edi_id$                  /* REASON CODE (30) SKIP*/
            read #3,key = cuscode$, using L24344, edi_id$, eod goto L24346
L24344:          FMT POS(1000), CH(3)
L24346:     if len(edi_id$) < 2 then L24360
               edi_rsn% = 0%
               convert str(rsncode$,1%,2%) to edi_rsn%, data goto L24348
L24348:                                        /* SKIP 30 THRU 34      */
              if edi_rsn% > 29% and edi_rsn% < 35% then goto L24360
              if edi_rsn% = 2% then goto L24360   /* (EWD002) Fix      */ 

        REM Mod to stop all CREDITS for Lows's and HQ - 06/25/97
        REM ADJUSTMENTS continue to be sent for Lowes

REM         if edi_id$    > "002" then goto L24350    /* (EWD006) */
               if invtype$    = "C"   then L24360 /* Low's and HQ Skip */
               if invtype$    = "A"   then L24360 /* Credits & Adjust  */

L24350:    if edi_id$  = "010" then L24360     /* (EWD005) - Don't send Norandex */ 

           if edi_id$    <> "001" then L24352


L24352:    if edi_id$ <> "020" then L24353
              if invtype$ = "A"  or  invtype$ = "C"  then L24360
* PAR000 in the following three EDI subroutines,
* HNYMASTR (INVMASTR) is only used to lookup sku number
* Since we do not currently use sku number with Lowe's
* I am not modifing!!
              call "AWDEDI01" (#9, #10, #3, #56, #7, #62, #58, #59, #04,   ~
                             cuscode$, invnr$, terms_disc$(1%),          ~
                             terms_net$(1%), "  0", "H", escamt)
              goto L24360
 
L24353:     if edi_id$ <> "030" then L24357
              if invtype$ = "A" or invtype$ = "C" then L24360
              call "AWDIBS04" (#9, #10, #3, #56, #7, #63, #58, #59, #04,  ~
                               cuscode$, invnr$, terms_disc$(1%),          ~
                               terms_net$(1%), "  0", "H", escamt)

L24357:    if edi_id$ < "040" or edi_id$ > "041" then L24358    /* (AWD017) */
            call "APCEDI10" (#9, #10, #3, #56, #7, #22, #58, #59, #04,   ~
                   #53, #54, cuscode$, invnr$, terms_disc$(1%),          ~
                   terms_net$(1%), "  0", "H", invSys$)
                                                 /* (EWD008)           */

L24358:    if edi_id$ < "042" or edi_id$ > "043" then L24359    /* (AWD017) */
            call "APCEDI10" (#9, #10, #3, #56, #7, #48, #58, #59, #04,   ~
                   #53, #54, cuscode$, invnr$, terms_disc$(1%),          ~
                   terms_net$(1%), "  0", "H", invSys$)

L24359:    if edi_id$ < "044" or edi_id$ > "044" then L24356    /* (AWD017) */
            call "APCEDI10" (#9, #10, #3, #56, #7, #49, #58, #59, #04,   ~
                   #53, #54, cuscode$, invnr$, terms_disc$(1%),          ~
                   terms_net$(1%), "  0", "H", invSys$)

L24356:    if edi_id$ < "047" or edi_id$ > "047" then L24354    /* (AWD017) */
            call "APCEDI10" (#9, #10, #3, #56, #7, #26, #58, #59, #04,   ~
                   #53, #54, cuscode$, invnr$, terms_disc$(1%),          ~
                   terms_net$(1%), "  0", "H", invSys$)

L24354:    if edi_id$ < "048" or edi_id$ > "049" then L24355
REM         CALL "APCEDI10" (#9, #10, #3, #56, #7, #27, #58, #59, #04, #53, #54, CUSCODE$, INVNR$, TERMS_DISC$(1%), TERMS_NET$(1%), "  0", "H")

                                                                /* (AWD017) */
L24355:    if edi_id$ > "049" and edi_id$ < "060" then                    ~
            call "APCEDI10" (#9, #10, #3, #56, #7, #26, #58, #59, #04,   ~
                   #53, #54, cuscode$, invnr$, terms_disc$(1%),          ~
                   terms_net$(1%), "  0", "H", invSys$)

           if edi_id$ <> "002" then L24360
            call "APCEDI03" (#9, #10, #3, #56, #7, #57, #58, #59, #04,   ~
                             cuscode$, invnr$, terms_disc$(1%),          ~
                             terms_net$(1%), "  0", "H", #27)
                                                 /* (EWD003)           */
L24360
*        Delete Header from Buffer
            readkey$ = str(cuscode$) & invnr$
            call "DELETE" (#09, readkey$, 17%)
            return

                                                   /* (EWD) - End   */
*       ****************************************************************
        update_serial_numbers
*          Clear SERTIF with any records for a BOL.  Then update
*          status for Invoices in the SERMASTR and clear the TIF.

*        Remove BOL from SERTIF file.
            if bol$ = " " then L25100
                plowkey$ = "RS" & str(so$) & str(bol$)
                call "DELETE" (#41, plowkey$, 21%)

L25100
*        Now clean up Serial Numbers for the Invoice.
            plowkey$ = "RT" & str(cuscode$) & invnr$
L25120:     call "PLOWNEXT" (#41, plowkey$, 19%, f1%(41))
            if f1%(41) = 0% then L25250
                get #41 using L25150, str(readkey$,26), str(readkey$,,25)
L25150:              FMT POS(43), CH(20), CH(25)
                call "WRAWSUPD" (str(readkey$,26,20), str(readkey$,,25), ~
                     invnr$, invdate$, cuscode$)
                call "READ101" (#40, readkey$, f1%(40))
                if f1%(40) = 0% then L25120
                put #40 using L25210, "4", str(plowkey$,3,21), " "
L25210:              FMT POS(1), CH(1), POS(2), CH(21), CH(9)
                rewrite #40
                goto L25120

L25250:     plowkey$ = "RT" & str(cuscode$) & invnr$
            call "DELETE" (#41, plowkey$, 19%)
            return

*       ****************************************************************
        update_cust_invoice  /* Update CCRMASTR with last invoice data */
            call "READ101" (#51, cuscode$, f1%(51%))
            totsales, totcrdts = 0
            if f1%(51%) <> 0% then get #51 using L25340, totsales, totcrdts
L25340:         FMT POS(10), 2*PD(14,4)
            if invtype$ = "C"         /* Update Total Sales or Credits */~
                then totcrdts = totcrdts + netinv                        ~
                else totsales = totsales + netinv
            if f1%(51%) = 0% then put #51 using L35980, cuscode$,         ~
                totsales, totcrdts, 0, " ", netinv, invnr$, 0, " ",      ~
                0, 0%, date, invdate$, " ", date, " ", 0, 0, 0, 0,       ~
                userid$, date, " "
            put #51 using L25440, totsales, totcrdts, netinv, invnr$,     ~
                date, invdate$, userid$, date
L25440:         FMT POS(10), 2*PD(14,4), POS(40), PD(14,4), CH(8),       ~
                     POS(84), 2*CH(6), POS(146), CH(3), CH(6)
            if f1%(51) = 0% then write #51 else rewrite #51
            return

*       ****************************************************************
        update_cust_ship_to    /* Update CCRMASTR for Ship-To balances */
            call "READ101" (#51, cuscode$, f1%(51%))
            mat cusbals = zer : high_ar_date$ = date
            if f1%(51%) <> 0% then get #51 using L25550, high_ar_date$,   ~
                cusbals()
L25550:         FMT POS(108), CH(6), 4*PD(14,4)
            cusbals(2%) = cusbals(2%) + adjorder
            if f1%(51%) = 0% then put #51 using L35980, cuscode$,         ~
                0, 0, 0, " ", 0, " ", 0, " ", 0, 0%, date, date, " ",    ~
                " ", high_ar_date$, cusbals(), userid$, date, " "
            put #51 using L25620, date, invdate$, high_ar_date$,          ~
                                                 cusbals(), userid$, date
L25620:         FMT POS(84), 2*CH(6), POS(108), CH(6), 4*PD(14,4),       ~
                     POS(146), CH(3), CH(6)
            if f1%(51) = 0% then write #51 else rewrite #51
            return

*       ****************************************************************
        update_cust_bill_to    /* Update CCRMASTR for Bill-To balances */
            call "READ101" (#51, billxref$, f1%(51%))
            mat cusbals = zer : high_ar_date$ = date
            if f1%(51%) <> 0% then get #51 using L25730, high_ar_date$,   ~
                cusbals()
L25730:         FMT POS(108), CH(6), 4*PD(14,4)
            cusbals(1%) = cusbals(1%) + adjorder
            if billxref$ = cuscode$                                      ~
                then cusbals(2%) = cusbals(2%) + adjorder
            cusbals(3%) = cusbals(3%) + invbal
            if cusbals(3%) < cusbals(4%) then L25800
                 cusbals(4%) = cusbals(3%) : high_ar_date$ = date
L25800:     temp$ = " " : if billxref$ = cuscode$ then temp$ = date
            if f1%(51%) = 0% then put #51 using L35980, billxref$,        ~
                0, 0, 0, " ", 0, " ", 0, " ", 0, 0%, date, temp$, " ",   ~
                " ", high_ar_date$, cusbals(), userid$, date, " "
            put #51 using L25860, date, temp$, high_ar_date$, cusbals(),  ~
                userid$, date
L25860:         FMT POS(84), 2*CH(6), POS(108), CH(6), 4*PD(14,4),       ~
                     POS(146), CH(3), CH(6)
            if f1%(51) = 0% then write #51 else rewrite #51
            return

        REM *************************************************************~
            *        L O A D   D A T A   F R O M   F I L E S            *~
            * --------------------------------------------------------- *~
            * Subroutines to bring in data from files.                  *~
            *************************************************************

        load_headers
            linesinv = 0  /* Initialize line item totals */
*        First Load the Next Invoice Header from the Buffer File
            plowkey$ = str(session$) & hex(00)
            call "PLOWALTS" (#09, plowkey$, 1%, 6%, f1%(9)) /* ARIBUFFR */
            if f1%(9) = 0% then return

            get #09 using L35060, cuscode$, invnr$, po$, so$, bol$,       ~
                                shipto$, soldto$, shipdate$, salesman$,  ~
                                region$, invdate$, invuser$, invacct$,   ~
                                frtacct$, taxacct$, discacct$, grossinv, ~
                                invd_pct, invdisc, frtamt, taxamt,       ~
                                netinv, billxref$, stlmnt$, store$,      ~
                                taxcode$, invtype$, rsncode$, textid$,   ~
                                posthny$, artype$, terms$, terms_due(),  ~
                                terms_pct(), terms_disc$(), terms_net$(),~
                                custype$, acctxref$, currency$, esc$,    ~
                                escamt$, escacct$, tax_rate, pgpo$,      ~
                                hdmet, hdidx 

* (AWD011) Modify the invoice date
            if tax_rate < 0 or tax_rate > 99999 then tax_rate = 0   /* CR1993 */
            if hdmet < 0 or hdmet > 99999 then hdmet = 0
            if hdidx < 0 or hdidx > 99999 then hdidx = 0
            invdate$ = postdate$

* (AWD011/) 
/* (AWD016) */
            if billxref$ = " " then gosub lookup_billxref
            esc, escamt = 0.00
            if str(esc$,1%,14%) = " " then goto bad_escamt
 
            get str(esc$,1%,8%) using L30226, esc
L30226:                   FMT PD(14,4)
            
            get str(escamt$,1%,8%) using L30226, escamt
            
bad_escamt:                        
            if esc < 0.00 then esc = 0.00
            if escamt < 0.00 and invtype$ <> "C" then escamt = 0.00
            if escacct$ = " " and esc > 0.00 then gosub lookup_escacct

            if export_on$ <> "Y" then L30230
                call "READ100" (#03, cuscode$, f1%(3))
                    if f1%(3) = 0% then L30230
                get #3 using L30228, country$
L30228:             FMT POS(1073), CH(3)

L30230:     
            invdate$ = postdate$                    /* (AWD011) */
            convdate$ = blankdate$
            conveqv, convunt = 1 : currency$ = stat$
            cnetinv = netinv : mat ctermsdue = terms_due:convdate%=0%

            call "READ100" (#42, key(#9), other_currency%)
               if other_currency% = 0% then L30320
            get #42 using L30270, currency$, cnetinv, convdate$, conveqv, ~
                                 convunt, ctermsdue()
L30270:         FMT CH(4), POS(54), PD(14,4), XX(8), CH(6), 2*PD(14,7),  ~
                    30*PD(14,4)
            tdate$ = convdate$
            call "DATEFMT" (tdate$, convdate%)

L30320
*        Next, Load Sales Order and BOL
            soonfile%, bolonfile% = 0%
            if invtype$ <> "O" and invtype$ <> "X" then return
                readkey$ = str(cuscode$) & so$
                call "READ100" (#07, readkey$, soonfile%)   /* BCKMASTR */
                get #07 using L30380, crhold$, energy$
L30380:           FMT POS(875), CH(1), POS(900), CH(8)  /* (AWD009) */
        REM       FMT POS(875), CH(1), POS(897), CH(8)  /* (AWD009) */
        REM       FMT POS(875), CH(1), POS(900), CH(8)  /* (AWD009) */

REM                energy, energytotal = 0.00
REM                if str(energy$,1%,14%) = " " then goto no_energy
 
REM                get str(energy$,1%,8%) using L30390, energy
REM L30390                   FMT PD(14,4)

REM                energy = 0.00
REM                energytotal = netinv * (energy / 100)

REM                netinv = netinv + energytotal

REM            no_energy


                readkey$ = str(cuscode$) & str(so$) & bol$
                call "READ100" (#11, readkey$, bolonfile%) /* SHPHDRS  */
                if bolonfile% = 1% then get #11 using L30420, bolshipdate$
L30420:              FMT POS(184), CH(6)
                return


        load_line_items
            init(" ") edi_cus$, edi_inv$, edi_seq$         /* (EWD)    */
            init(" ") itemid$, unitid$, skuno$             /* (AWD015) */
            plowkey$ = str(cuscode$) & str(invnr$)
            edi_cus$ = cuscode$                            /* (EWD)    */
            edi_inv$ = invnr$                              /* (EWD)    */  
            call "PLOWNEXT" (#10, plowkey$, 17%, f1%(10))  /* ARIBUF2  */
            if f1%(10) = 0% then return

            get #10 using L35680, seq$, part$, cat$, invorder, invshipd,  ~
                          invopen, pricestk, uom$,  temp, line_disc_pct, ~
                                 line_disc_amt, line_ext, sales$, discs$,~
                                 linetextid$, soseq$, newlot$(),         ~
                                 newlotqty(), project$

            edi_seq$ = seq$                                 /* (EWD) */
* PAR000
                init(" ") so_inv$, item_no$, epart$
                so_inv$  = so$
                item_no$ = soseq$
                gosub lookup_subpart
                subp$ = str(bcksubpt_rec$,48%,20%)
                epart$ = str(part$,,25) & str(subp$,,20)

            if export_on$ <> "Y" then L30561
                generic$, prtclass$, prttype$ = " "
                call "READ100" (#04, epart$, f1%(4))      /* PAR000 */
                    if f1%(4) = 0% then L30561
                get #04 using L30559, generic$, prtclass$, prttype$
* PAR000
L30559:             FMT POS(78), CH(16), POS(153), CH(4), POS(200), CH(3)

L30561:     cprice1 = pricestk:cprice2 = temp
            if other_currency% = 0% then L30580
            call "READ100" (#47, key(#10), f1%(47))
               if f1%(47) = 0% then L30580
            get #47 using L30566, cprice1, cprice2
L30566:         FMT POS(25), 2*PD(14,4)

*        Get Corresponding Lines for Sales Order and BOL
L30580:     soline%, bolline% = 0%
            init(" ") oldlot$(), soinvnr$, solot$
            soopen, soshipd, soschld, soalloc, sopreinv, bolshipd,       ~
                                                         bolschld = 0
            mat oldlotqty = zer
            if (invtype$ <> "O" and invtype$ <> "X") or soseq$ = " "     ~
                                then return
                readkey$ = str(so$) & soseq$
                call "READ100" (#08, readkey$, soline%)     /* BCKLINES */
                if soline% = 0% then L30730
                     get #08 using L30710, soshipd, soopen, soschld,      ~
                                         soalloc, sopreinv, solot$,      ~
                                         soinvnr$, itemid$, unitid$, skuno$
/* (AWD015) add unitid$, itemid$ & skuno$ */
L30710:                   FMT POS(101), 5*PD(14,4), POS(218), CH(6),     ~
                              POS(247), CH(8), POS(255), CH(10), CH(10), ~
                              POS(290), CH(09)


L30730:         if bolonfile% = 0% then return
                     readkey$ = str(so$) & str(bol$) & soseq$
                     call "READ100" (#12, readkey$, bolline%)
                     if bolline% = 0% then return
                       get #12 using L30780, bolschld
L30780:                     FMT POS(32), PD(14,4)
                       if bolshipdate$ = " " or bolshipdate$ = blankdate$ ~
                             then return
                          bolschld = 0 /* Must be if pre-invoiced */
                          get #12 using L30820, bolshipd, oldlot$(),      ~
                                               oldlotqty()
L30820:                        FMT POS(32), PD(14,4), 30*CH(6),          ~
                                                             30*PD(14,4)
                return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: ARIMASTR / ARIBUFFR               */~
            CH(9),          /* Customer Code                           */~
            CH(8),          /* Invoice Number                          */~
            CH(16),         /* Purchase Order Number                   */~
            CH(16),         /* Sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(180),        /* Ship To Name and Address                */~
            CH(180),        /* Sold-To Name and Address                */~
            CH(6),          /* Ship Date                               */~
            XX(20),         /* How Ship information                    */~
            XX(20),         /* FOB Terms                               */~
            XX(6),          /* Shipping Carrier Code                   */~
            XX(8),          /* Number of Cartons                       */~
            XX(8),          /* Shipment Weight                         */~
            XX(20),         /* Freight/ Air Bill Number                */~
            CH(4), XX(8),   /* Salesman Id for Sales Analysis          */~
            XX(3),          /* Percentage of Sale Credited to salesman.*/~
            CH(4),          /* Region Code                             */~
            XX(1),          /* Price Code                              */~
            CH(6),          /* Invoice Date                            */~
            XX(6),          /* Recurring Expiration Date               */~
            XX(6),          /* Post Date                               */~
            XX(6),          /* Date a transaction was entered          */~
            CH(3),          /* User ID who entered transaction         */~
            XX(200),        /* Variable Fields Data Area               */~
            CH(9),          /* Receivables Account Number              */~
            CH(9),          /* Freight Account Code                    */~
            CH(9),          /* Sales Tax Account Code                  */~
            XX(9),          /* Sales Distribution Account              */~
            CH(9),          /* Sales Discounts Distribution Account    */~
            PD(14,4),       /* Gross Invoice Amount                    */~
            PD(14,4),       /* Discount Percentage                     */~
            PD(14,4),       /* Discount Amount                         */~
            PD(14,4),       /* Freight Amount                          */~
            PD(14,4),       /* Sales Tax Amount                        */~
            PD(14,4),       /* Net Invoice Amount                      */~
            XX(8),          /* Current Balance                         */~
            CH(9),          /* Bill-to Cross Reference                 */~
            CH(12),         /* Settlement Code                         */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(10),         /* Sales Tax Code                          */~
            XX(8),          /* Sales Tax Percent                       */~
            CH(1),          /* Invoice Type Code                       */~
            CH(9),          /* Invoice Reason Code                     */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Post Inventory with this Transaction?   */~
            XX(1),          /* Post G/L with this Transaction?         */~
            CH(1),          /* A/R Type (A/C/E)                        */~
            CH(20),         /* Payment Terms Code                      */~
            30*PD(14,4),    /* Payment Amounts Due                     */~
            30*PD(14,4),    /* Cash Discount Percents                  */~
            30*CH(6),       /* Discount Terms                          */~
            30*CH(6),       /* Net Payment Terms                       */~
            CH(2),          /* Customer type                           */~
            CH(9),          /* Acccount Cross Reference.               */~
            CH(4),          /* Currency code                           */~
            XX(28),         /* Bill-To,Cuscode,Inv,PM_SO_Fl,PO_INV_Fl  */~
/*            PD(14,4),    */   /* ESC Percentage                          */~
/*            PD(14,4),    */   /* ESC AMT                                 */~
            CH(08),         /* ESC Percentage                          */~
            CH(08),         /* ESC Amt                                 */~
            CH(9),          /* ESC Acct                                */~
            PD(14,4),       /* Tax_rate                 CR1993         */~
            CH(20),         /* PlyGem PO                CR1993         */~
            PD(14,4),       /* Home Depot MET discount  CR1993         */~
            PD(14,4),       /* Home Depot IDX discount  CR1993         */~
            XX(146)         /* Filler (Rest of Record)                 */

L35680: FMT                 /* FILE: ARILINES / ARIBUF2                */~
            XX(9),          /* Customer Code                           */~
            XX(8),          /* Invoice Number                          */~
            CH(3),          /* General purpose sequence number         */~
            XX(3),          /* Purchase Order Line Number              */~
            CH(25),         /* Part Number                             */~
            XX(32),         /* Part description                        */~
            CH(4),          /* Category Code                           */~
            PD(14,4),       /* Order Quantity.                         */~
            PD(14,4),       /* Quantity Shipped                        */~
            PD(14,4),       /* Open Quantity                           */~
            PD(14,4),       /* Unit Price- Stocking UOM                */~
            CH(4),          /* Stocking UOM                            */~
            XX(4),          /* Pricing Unit of Measure                 */~
            XX(8),          /* Conversion Factor (Price to Stk)        */~
            PD(14,4),       /* Unit Price- Pricing UOM                 */~
            PD(14,4),       /* Line Item Discount                      */~
            PD(14,4),       /* Discount Amount                         */~
            PD(14,4),       /* Extension                               */~
            XX(1),          /* Taxable Purchase (Y/N)                  */~
            CH(9),          /* Sales Account Number                    */~
            CH(9),          /* Discounts Account                       */~
            XX(6),          /* Filler                                  */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Sales Order Sequence Number             */~
            30*CH(6),       /* Lot Number                              */~
            30*PD(14,4),    /* Quantity of Something                   */~
            XX(1),          /* Non-Stock Part Flag. (Y= Non-Stock)     */~
            CH(8),          /* Project                                 */~
            XX(125)         /* Filler (Rest of Record)                 */

L35980:     FMT /* File #51- CCRMASTR Master file                      */~
                CH(9),                   /* Customer Code (Key)        */~
                PD(14,4),                /* Total Sales                */~
                PD(14,4),                /* Total Credits              */~
                PD(14,4),                /* High Credit Limit          */~
                CH(6),                   /* High Credit Limit Date     */~
                PD(14,4),                /* Last Invoice Amount        */~
                CH(8),                   /* Last Invoice Number        */~
                PD(14,4),                /* Last Payment Amount        */~
                CH(10),                  /* Last Payment Check #       */~
                PD(14,4),                /* Average # Days to Pay      */~
                BI(2),                   /* # Invoices in Average Days */~
                5*CH(6),                 /* 'Dynamic' dates fr CUSTOMER*/~
                4*PD(14,4),              /* 'Dynamic' amnts fr CUSTOMER*/~
                CH(3),                   /* User Last Modified         */~
                CH(6),                   /* Date Last Modified         */~
                CH(46)                   /* Filler                     */

                                         /* (EWD) - Begin              */
        update_group
          or_key4$ = str(invhdr$(1%),34%,8%)       /* S.O. NUMBER      */
          or_inv$  = str(invhdr$(1%),10%,8%)
          read #55,hold,key 4% = or_key4$, using L40000 , or_rec$,       ~
                                                         eod goto L45000
L40000:      FMT CH(170)
          if str(or_rec$,60%,2%) > "19" then goto L45000
             delete #55
          str(or_rec$,60%,2%) = "20"
          str(or_rec$,62%,8%) = date
          str(or_rec$,70%,8%) = or_inv$
          write #55, using L40000 , or_rec$, eod goto L45000

L45000: return
                                         /* (EWD) - End                */
        update_subpart                   /* CR347   - Begin            */

            init(" ") bcksubpt_key$, bcksubpt_key1$, bcksubpt_rec$

            str(bcksubpt_key$,1%,8%) = so$                    /* SO NUMB      */
            str(bcksubpt_key$,9%,3%) = str(invline$(),18%,3%) /* INVOICE Line */

            read #64, hold, key = bcksubpt_key$, using BCKSUBPT_FMT,        ~
                                       bcksubpt_rec$, eod goto subpart_done
BCKSUBPT_FMT:          FMT CH(256)


                  if str(bcksubpt_key$,1%,11%) <> str(bcksubpt_rec$,1%,11%)     ~
                                                     then goto subpart_done

                  delete #64
                                                   /* Invoice Number */
                  str(bcksubpt_rec$,12%,8%) = str(invline$(),10%,8%)
                                                   /* Invoice Item   */
                  str(bcksubpt_rec$,20%,3%) = str(invline$(),18%,3%) 

                  put #64, using BCKSUBPT_FMT, bcksubpt_rec$

                  write #64, eod goto subpart_done


        subpart_done
        return                           /* CR347   - END              */


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

REM         call "SHOSTAT" (" SO AND ITEM " & so_inv$ & item_no$)  stop

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
                          #64,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */
           
            if err1% <> 0% then                                          ~
                   str(bcksubpt_rec$,48%,20%) = "0000000000000000000000000"

            if err1% = 0% then return


            return
            errormsg$ = "BCKSUBPT ERR= "&so_inv$ & " Line= " & item_no$  ~
                                      & " Flag= " & flag$


        return                                    /* PAR000 */

* AWD013

        update_orainv


           call "LOGINV" (cuscode$, invnr$, postdate$)


REM        put #38, using orainv_fmt,              ~
                    date,         /* SYSTEM DATE */~
                    "S",          /* Tran type   */~
                    cuscode$,     /* CusCode     */~
                    invnr$,       /* Inv Number  */~
                    postdate$,    /* PostDate    */~
                    " "           /* Filler      */
 
REM        write #38, eod goto no_orainv

        no_orainv
        return

REM orainv_fmt          FMT CH(06), CH(01), CH(09), CH(08), CH(06), CH(218)

* AWD013 
/* (AWD016) */
        lookup_escacct
            acct% = 12% :  gosub get_account_default
            escacct$ = acct$  

        return

        get_account_default
            call "ARMGLGET" (acct%, cuscode$, " ", " ", taxcode$,        ~
                             store$, " ", #2, #3, #4, " ", " ", acct$)
        return
        
        lookup_billxref
          call "READ100" (#03, cuscode$, f1%(3%)) 
          get #03, using BILLXREF_FMT, billxref$
BILLXREF_FMT:   FMT POS(780), CH(09)          
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
REM         call "SHOSTAT" ("One Moment Please")
                call "FILEBGON" (#23)   /* HNYPSTGL Work File */
            end return%


