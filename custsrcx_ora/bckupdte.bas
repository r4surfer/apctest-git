        REM *************************************************************~
            *                   ( As Of 10/01/2019 )                    *~
            *  (APCPLN5B)                      (APCPLN6B)               *~
            *   02 = Unable to Add S.O.         12 = Error Adding Pulls *~
            *   04 = Unabel to Update Planned   14 = Unable to Update   *~
            *   06 = Unable to Update Invoiced                (APCPLNSC)*~
            *   08 = Unable to Update Paid      16 = Unable to Update   *~
            *   10 = Unable to Update Changed                 (APCPLNOR)*~
            *   12 = Unable to Delete                         (APCPLNOR)*~
            *  (EWDPLN6B)                                               *~
            *   Called from 'APCPLN6B'              New Files (CUSLINK) *~
            *                                                 (CUSDATA) *~
            *  (BCKUPDTE)                                               *~
            *   90 = S.O. Not Added or Changed.                         *~
            *   91 = Data Conversion Error (#7) - EWD004                *~
            *   99 = S.O. Not Deleted                                   *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *  BBBB    CCC   K   K  U   U  PPPP   DDDD   TTTTT  EEEEE   *~
            *  B   B  C   C  K  K   U   U  P   P  D   D    T    E       *~
            *  BBBB   C      KKK    U   U  PPPP   D   D    T    EEEE    *~
            *  B   B  C   C  K  K   U   U  P      D   D    T    E       *~
            *  BBBB    CCC   K   K   UUU   P      DDDD     T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKUPDTE - Move Sales Order from the buffers to the master*~
            *            files.  Update Demands and HNYQUAN records for *~
            *            the backorder quantities.                      *~
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
            * 07/21/86 ! Original                                 ! ERN *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 04/16/87 ! Remove any bring down records upon exit  ! ERN *~
            * 05/18/87 ! Changed HNYMASTR Record length           ! JIM *~
            * 02/02/88 ! Added cancelled SOs  - BKNGTYPE$ = "4"   ! JDH *~
            *          !                  and RECORDTYPE$ = "5"   ! JDH *~
            * 06/17/88 ! Added Alt Key 3 on DEMMASTR              ! MJB *~
            * 08/10/89 ! Allocation type 'C' now stays a 'C' even ! JDH *~
            *          !   though the open qty is zero.           !     *~
            * 11/27/89 ! Expanded size of SADETAIL file.          ! JEF *~
            * 09/10/90 ! SA treats cancelled the same as deleted. ! JDH *~
            * 06/03/91 ! PRRs 11651 & 11902.  BCKPRIDX deletes &  ! JDH *~
            *          !   writes modified.                       !     *~
            * 09/17/91 ! Resurrected orders treated as new for SA.! JDH *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 07/23/92 ! PRR 12531 Added Allocation Type 'P'.     ! JDH *~
            * 07/24/92 ! ALLOC for type 'A' no longer gives up    ! JDH *~
            *          !   previously allocated quantity.         !     *~
            * 08/13/92 ! MPS/PFM - Added Coding for calls to      ! JBK *~
            *          !   Usage Capture Subroutine (HNYUSESB).   !     *~
            * 09/08/92 ! Chngd call to PLANALLC for more info out.! JDH *~
            * 03/09/93 ! PRR 11696,12416 Rpt ID for unplan report.! JDH *~
            * 03/11/93 ! Integrated Kenny's PORT/MESSAGE subs.    ! JDH *~
            * 07/20/93 ! Init OLDALLOC$ on new line to 'blank'.   ! JDH *~
            * 09/27/93 ! Added Multi-Line Quote cutover logic.    ! JIM *~
            * 11/18/93 ! BUFFER plowkey now primed to get records ! MLJ *~
            *          !  with a date > than day 1 ignoring all   !     *~
            *          !  orders currently in edit.               !     *~
            * 03/08/94 ! Changed record length for BOMSPEC file   ! WPH *~
            * 10/17/94 ! PRR 13285 - Calculate the Stocking Price ! RJH *~
            *          !   instead of using the less accurate file!     *~
            *          !   value.                                 !     *~
            * 03/10/95 ! PRR 13187 - Delete Part Xref Shadow Recrd! RJH *~
            * 03/10/95 ! PMetal - Delete Part Scharge Shadow Recrd! RJH *~
            * 03/21/95 ! PRR 13143. Added flag for Acknowledgment?! JDH *~
            * 07/31/96 ! Changes for the year 2000.               ! DXL *~
            * 08/17/90 ! (EWD) Modifications                      ! RHH *~
            * 10/10/90 ! MOD TO ELIMINATE PIPOUTS - LINE 11610    ! RHH *~
            * 02/06/91 ! Add New Audit Update Routine             ! RHH *~
            * 08/13/91 ! NEW FILES (APCORDER)                     ! RHH *~
            * 06/06/96 ! APC Mod to SupportNew Planning System.   ! RHH *~
            *          !   Uses New Subroutines (APCPLN5B) and    !     *~
            *          !   (APCPLN6B). Order Entry uses the new   !     *~
            *          !   Date routine for Delivery Date.        !     *~
            *          !   (APCPLN3B).                            !     *~
            * 10/22/96 ! S.O. on Credit Hold Cannot be Planned    ! RHH *~
            *          ! Set Status Code to '99'. UPDATE_PLANNING !     *~
            * 01/24/97 ! Mods to (APCPLN5B) Arguments for Sending ! RHH *~
            *          !   Stock (Sales Order) and Stock Loads.   !     *~
            * 02/14/97 ! Mod Create (APCPLNSA) for Daily Sales    ! RHH *~
            *          !   Report (APCRPT04). New Planning Error  !     *~
            *          !   Log File (APCPLNER) Loag all Errors    !     *~
            * 10/13/97 ! Mod change the size of the error log     ! RHH *~
            *          !   file (APCPLNER) from 64 to 80.         !     *~
            *          !   Change Key ERR_KEY$ - 39               !     *~
            * 11/17/97 ! Mod for upgrade to R6.04.03              !     *~
            *          !   Use Caelus Version from R7.00.00       !     *~
            * 12/12/97 ! (EWD) Modifications end                  ! RHH *~
            * 03/31/98 ! Mods to convert to (Y2K) Version (EWD)   ! RHH *~
            * 08/14/98 ! (EWD002) - Mods to Update Daily Sales    ! RHH *~
            *          !    Analysis. (in APCPLN6B)               !     *~
            *          ! (EWD003) - New File for Special Schedule !     *~
            *          !   (EWDSCHED). Tempered Glass & Grid      !     *~
            * 09/26/98 ! (VIQS001) - Added code to support delete ! DJD *~
            *          ! orders from VIQS in CAELUS.  Easysoft    !     *~
            *          ! ODBC does NOT support HEX(ff) in a byte  !     *~
            *          ! field so VIQS is putting a HEX(01)       !     *~
            *          ! to accomodate the delete functionality   !     *~
            * 09/29/98 ! (VIQS002) - Added Fax Ability to auto    ! DJD *~
            *          ! Fax an acknowledgement when the order    !     *~
            *          !                                          !     *~
            * 10/01/98 ! VARIABLE FIELD 3 IS temporarily used to  ! DJD *~
            *          ! put the fax flag                         !     *~
            * 10/07/98 ! (EWD004) - Write APCPLNER for bad data-#7! BWS *~
            * 04/08/99 ! (EWD005) - New Subroutine to Validate    ! RHH *~
            *          !    Category Code and G/L Account Number  !     *~
            * 11/08/99 ! (EWD006) - Mods to Prevent Misc. Cancel  ! RHH *~
            *          !    Problem.                              !     *~
            * 01/20/00 ! (EWDXXX) - Mods were made to 'APCPLN6B'  ! RHH *~
            *          !    for Samples/Displays/Literature. A    !     *~
            *          !    subroutine was created for processing !     *~
            *          !    of data. 'EWDPLN6B'                   !     *~
            * 05/23/00 ! (EWD007) - Mods for validating cat$ to   ! RHH *~
            *          !    insure the correct G/L Account is     !     *~
            *          !    posted too.                           !     *~
            * 08/21/00 ! (EWD008) - Mods for new Howship code for ! RHH *~
            *          !    'UPS' Code = 35. Change made to Sub's !     *~
            *          !    (APCPLN5B and APCPLN6B, Prog=APCPLN05 !     *~
            *          !                                          !     *~
            * 11/04/00 ! (EWD009) - Mod for new Freight Model for ! CMG *~
            *          !    entering freight charges.             !     *~
            * 06/26/01 ! (EWD010) - Mod to cancel entire order    ! CMG *~
            *          !    from 'D' in adjrsn$ instead of        !     *~
            *          !    looking at individual lines.          !     *~
            * 06/28/01 ! (EWD011) - Mod delete entire order and   ! CMG *~
            *          !    update from just what is in the       !     *~
            *          !    buffer files.                         !     *~
            * 01/10/02 ! (EWD012) - Mods for new 'UPS' Howship    ! RHH *~
            *          !    codes.                                !     *~
            * 04/17/02 ! (EWD013) - Mods to pull BCKBUFFR Records ! CMG *~
            *          !     from Oracle.                         !     *~
            * 08/18/03 ! (EWD014) - Mods to update new Bookings   ! CMG *~
            *          !     Files.                               !     *~
            * 04/05/04 ! (AWD015) - Mods to APCPLNSD              ! CMG *~
            * 10/10/05 ! (AWD016) - CR347 Mod to add file BCKSUBPT! CMG *~
            * 02/21/06 ! (AWD017) - mods for North east           ! CMG *~
            * 04/27/06 ! (AWD018) - mod to set up specific categor! CMG *~
            *          !          codes for intercompany customer !     *~
            * 06/20/06 ! (AWD019) - mod for special mull          ! CMG *~
            * 07/05/06 ! (AWD020) - mod to fix esc percentage     ! CMG *~
            * 07/05/06 ! (AWD021) - mod to move ship to from cust ! CMG *~
            *          !              to end of BCKMASTR          !     *~
            * 03/28/07 ! (AWD022) - mod for additional info fields! CMG *~
            * 04/25/08 ! (AWD023) - mod for credit percentage     ! CMG *~
            *          !         to bckmastr/bnkmastr             !     *~
            * 11/10/08 ! (AWD024) - mod to add mull row and column! CMG *~
            *          !   indexes; also add sku num to bcklines  !     *~
            *10/05/2009!(AWD025) - mod for NEW GBW                ! CMG *~
            *03/08/2010!(AWD026) - add file KANBANPT              ! CMG *~
            *03/16/2010!(AWD027) - check for screens to put on hold!CMG *~
            *06/06/2011!(AWD028) - add orcl usr & pswd lookup     ! CMG *~
            *06/26/2012!(AWD029) - mod for dev background task    ! CMG *~
            *07/10/2013!(AWD030) - mod for shape dimensions       ! CMG *~
            *06/12/2014!(AWD031) - mod to add series & style      ! CMG *~
            *09/09/2014!(AWD032) - mod for stc rating             ! CMG *~
            *06/24/2015!(SR66200) - mods for new fields in        ! CMG *~
            *          !    OE_BCKBUF2 in file ORADES2            !     *~
            *11/04/2015!(SR69995) increase size of l1_desc        ! CMG *~
            *12/28/2015!(SR67154) add NFRC2016 Flag               ! CMG *~
            *05/13/2016!(SR71501) wrong gl account for credit     ! CMN *~
            *10/27/2018!(CR1781) Credits with incorrect GL Account! CMN *~
            *12/17/2018!(CR1830) Mods to add PlyGem fields to     ! CMN *~
            *          !         caelus to transmit to ATLaS      !     *~
            *01/22/2019!(CR1885) mods to add PlyGem TDI number &  ! CMN *~
            *          !           Florida approval number        !     *~
            *02/18/2019!(CR1933) modify to add plygem order flag  ! CMG *~
            *06/16/2019!(CR2068) Add Contact Information &        ! CMN *~
            *          !  nameordered by field to Caelus          !     *~
            *06/24/2019!(CR2090) Add new nominal size to bcksubpt ! RDB *~
            *07/03/2019!(CR2114) Add l1_size, l1_width, l1_height ! CMN *~
            *10/01/2019!(CR2264) add screen ship sep flag         ! CMN *~
            *01/20/2020!(CR2378) No tax on part 020               ! RDB *~
            *03/30/2020! (CR1837) PlyGem Parts set to PGPA        ! RDB *~
            *04/07/2020! (CR2501) Add brand 18 with 19            ! RDB *~
            *04/28/2021! (CR2825) New Quality Dept Flag from WW   ! RDB *~
            *05/06/2021! (CR2829) GS128 and new bcklin2           ! RDB *~
			*10/27/2022! (CR3186) Add prefix field to apcplnor    ! RDB *~	
            *04/27/2023! CR3305 Add new ship separate type        ! RDB *~            
            *************************************************************

        dim                                                              ~
            or_no$(5%)9,                 /* Cust, S.O., Inv, Chk  (EWD)*/~
/*PAR000*/  stk$(99%)25,                 /* MFG STOCK PRODUCT     (EWD)*/~
/*PAR000*/  stk_sub$(99%)20,             /* Part (20)                  */~
/*PAR000*/  stk_desc$(99%)30,            /* Stock Desck                */~
            qty$(99%),                   /* MFG STOCK QUANTITY    (EWD)*/~
            adjrsn$9,                    /* Adjustment Reason Code     */~
            alloc$1,                     /* Allocation Flag            */~
            billto$9,                    /* Bill-to Customer ID   (EWD013)*/~
            bufferkey$10,                /* BCKBUFFR Primary Key       */~
            crhold$1,                    /* Credit Hold Status (b/H)   */~
            currency$4,                  /* Transaction Currency       */~
            convdate$6,                  /* Convertion eff. date       */~
            cuscode$9,                   /* Customer Code              */~
            time$6,                       /* Current Time Stamp         */~
            firstship$6,                 /* Earliest Req'd Ship Date   */~
            linekey$19,                  /* Line Item Key              */~
            message$10,                  /* Messaging Work Variable    */~
            mlqyorn$1, mlqxref$1,        /* MLQ processing params      */~
            mlquote_seq$11,              /* Quote & Seq LI derived from*/~
            newest$8,                    /* Buffer Estimate Number     */~
            newdemtype$1,                /* Buffer Demand Type         */~
            newhdr$(5)200,               /* Buffer Header Record       */~
            newhdr2$(5)200,              /* Buffer2 Header (CR2068)    */~
            newline$(2)150,              /* Buffer Line Item Record    */~
            ne2line$256,                 /* Buffer Line 2 Item CR2829  */~
            newlot$6,                    /* Buffer Lot Number          */~
            neworderdate$,               /* New Order Date             */~
            neworigdue$6,                /* Buffer Original Due Date   */~
            newsa$(2)150,                /* New Sales Analysis Data    */~
            newstore$3,                  /* Buffer Store Code          */~
            newusagedate$6,              /* Buffer Usage Capture Date  */~
            oldalloc$1,                  /* Allocation Flag            */~
            oldcrhold$1,                 /* Previously Cancelled Order */~
            olddemtype$1,                /* Master Line Demand Type    */~
            oldest$8,                    /* Master Estimate Number     */~
            oldhdr$(5)200,               /* Master Header Record       */~
            oldline$(2)150,              /* Master Line Item Record    */~
            oldlot$6,                    /* Master Lot Number          */~
            oldorderdate$,               /* Old Order Date             */~
            oldorigdue$6,                /* Old Original Due Date      */~
            oldreqdship$6,               /* Old Required Ship Date     */~
            oldsa$(2)150,                /* New Sales Analysis Data    */~
            oldstore$3,                  /* Master Store Code          */~
            oldusagedate$6,              /* Old Usage Capture Date     */~
            part$25,                     /* Part Number                */~
            planflags$(25)20,            /* Planning System Flags      */~
            port$4,                      /* Messaging Port             */~
            print$1,                     /* Pick List/BOL Print Flag   */~
            priority$1,                  /* Demand Priority            */~
            recordtype$1,                /* Record type for SADETAIL   */~
            readkey$50,                  /* Multi-use read key         */~
            reqdship$6,                  /* Required Ship Date         */~
            rptid$6,                     /* Report ID                  */~
            seq$3,                       /* Line Sequence Number       */~
            shiptoname$30,               /* Ship-to Name from Buffer   */~
            so$16,                       /* Sales Order Number         */~
            stat$4,                      /* Statutory Currency         */~
            tdate$8,                     /* Temporary Date Variable    */~
            topen(4),                    /* Dollars Recap      (EWD013)*/~
            type$1,                      /* Demand Type                */~
            usage_date$1,                /* Usage Capture Date Flag    */~
            usage_on$1,                  /* Usage Capture ON-OFF Flag  */~
            usage_store$1,               /* Usage Capture Store Flag   */~
            usagetype$5,                 /* MPS/PFM Usage Type         */~
            userid$3                     /* User ID                    */

                                         /* FILE = (APCPLNOR)(EWD)Begin*/
        dim or_due$8,                    /* Order Due Date             */~
            or_cuscode$9,                /* Customer Code              */~
            or_userid$3, err_key$39,     /* Last Mod User Id.          */~
            or_hows$2,   err_rec$80,     /* Special Instr (PLAN HOWS)  */~
            or_fob$2,    err_code$8,     /* Special Delivery(PLAN DEL?)*/~
            or_sls$4,                    /* Salesman Code              */~
            or_text$4,                   /* Header Text Id             */~
            or_po$16,                    /* P.O. Number                */~
            or_so$8,                     /* S.O. Number                */~
            or_status$2,                 /* Sales Order Status Code    */~
			prefix_so$1                   /* Prefix of sales order      */


    /* VIQS002 */
        dim error%                       /* Error Flag             */
        dim fax_ack$1                    /* Fax Ack                    */
    /* VIQS002 */


        dim  f2%(64%),                   /* = 0 if the file is open    */~
/*(AWD016)*/ f1%(64%),                   /* = 1 if READ was successful */~
             fs%(64%),                   /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
             rslt$(64)20                 /* result                     */

        dim                              /* (EWD005) - Begin           */~
            new_cat$4,                   /* Category Code Key          */~
            sales_acct$9, sls_acct$9,    /* New G/L Account Code Sales */~
            disc_acct$9,  dsc_acct$9,    /* new G/L Account Code Disc  */~
            sales_acct_cr$9, sls_acct_cr$9, /* Credit Sales Accounts   */~
            hows$2,                      /* How Ship from Order Header */~
            cat$4                        /* Category Code from Line Itm*/
                                         /* (EWD005) - End             */

        dim                              /*  (EWD013)                  */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            field$256,                   /* Query Return Field         */~
            hdr_text$10,                 /* Header Text ID - UNPACKED  */~
            lne_text$10,                 /* Line   Text ID - UNPACKED  */~
            text_key$64,                 /* Text File Key              */~
            text_rec$(28%)70,            /* Text Lines                 */~
            name$50                      /* Field Name Returned        */

        dim trans_number$16,              /* Transaction Record Number  */~
            bnkdate$6,                   /* Bookings Date              */~
            bnktime$6,                   /* Bookings Time              */~
            bnklne$(2)150,               /* Buffer Line Item Record    */~
            bnkhdr$(5)200                /* Buffer Header Record       */

        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            hdr$45, msg$(3%)79           /* Askuser - Var's            */

        dim                              /* (AWD016)                   */~
            flag$1,                      /* 0 = Sales Order 1 = Invoice*/~
            pgm$1,                       /* 0 = BCKUPDTE 1 = Other     */~
                                         /* 2 = BCKUPDTE Delete Order  */~
            subpart$20,                  /* Sub Part Number            */~
            infopart$20,                 /* Information Part           */~
            brand$2,                     /* Brand Info                 */~
            part_desc$64,                /* Part description           */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Fields                     */~
            info_flds$(35%)4             /* Information Fields         */

/* (AWD017) */
        dim schema$8                     /* Database schema            */

/* (AWD031) */
        dim series$16,                   /* WW Series                  */~
            style$10,                    /* WW Style                   */~
/*(AWD032)*/stcrating$3                  /* WW Stc Rating              */

        dim oradescr_rec$(4%)256,        /* (SR66200)                  */~
            config_ln$3,                 /* WW Line Number             */~
            oraclePart$40,               /* Oracle Part Number         */~
/*SR69995*/ l1_desc$250,                 /* L1 Desc                    */~
            l2_desc$(2%)250,             /* L2 Desc                    */~
            l1_mutype$50,                /* L1 Mutype                  */~
            l3_mulltype$25               /* L3 Mulltype                */
/* (CR1830) */
        dim plygem_po$20,                /* PlyGem PO                  */~
            plygem_quote_id$20,          /* PlyGem Quote ID            */~
            ship_id$6,                   /* Ship ID  (CR2068)          */~
            plygem_dprating$10,          /* PlyGem DP Rating           */~
            plygem_cpd$30,               /* PlyGem NFRC CPD Num        */~
            plygem_prd_series$25,        /* PlyGem Production Series   */~
            plygem_perflbl$4,            /* PlyGem Performance Label   */~
            plygem_buyout$2,             /* PlyGem BuyOut              */~
            plygem_thd_lne$3,            /* PlyGem THD Line Num        */~
/*(CR1885) */                                                            ~
            plygem_tdi$8,                /* PlyGem TDI Number          */~
            plygem_fld$12,               /* PlyGem Florida Approval    */~
            plygem_lne$1,                /* PlyGem Order  (CR1933)     */~
            nameorderedby$20,            /* PlyGem Order  (CR2068)     */~
            plygem_contact$100,          /* PlyGem Order  (CR2068)     */~
/* CR2090 */                                                             ~
            L2_widthns$10,               /* WW Nominal width           */~
            L2_heightns$10,              /* WW Nominal height          */~
            L2_unit_size_code$2,         /* WW Exact or Nominal code   */~
/* (CR2114) */                                                           ~
            L1_unit_size_code$2,         /* WW Exact or Nominal code   */~
/* (CR2825) */                                                           ~
            qlty_dept_flag$1,            /* WW Quality department flag */~
/* (CR2264) */                                                           ~
            scrnshpsep$1,                /* Screen Ship Separately Flag*/~
/* CR3305 */                                                             ~
            shipseptype$3,               /* Ship separately type       */~
            shiplinenbr$3,               /* Shipping line number       */~
            shipbarcode$18,              /* Shipping barcode           */~
            distordernbr$8,              /* Ship Distributor Order Nbr */~
            customerpo$22,               /* Ship Customer PO           */~
            distcustnbr$12               /* Ship Distributor Cust Nbr  */

/* CR2829 */
        dim gs128$20,                    /* GS128 barcode number       */~
            gsinput$9                    /* unitid last 9 positions    */             

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 04/08/99                    (EWD) Mod's  "
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
            * #1  ! SYSFILE2 ! Caelus Management System General Informa *~
            * #2  ! DEMMASTR ! Demand Master File                       *~
            * #3  ! INVQUAN  ! Inventory Part / Store / Lot Quantity Fi *~
            * #4  ! INVMASTR ! Inventory Master File                    *~
            * #5  ! BCKMASTR ! Backlog master file                      *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #7  ! BCKBKGRF ! SO Bookings Report file                  *~
            * #8  ! BCKPRIDX ! BCK Print Index File                     *~
            * #9  ! BCKBUFFR ! Backlog buffer for SO headers            *~
            * #10 ! BCKBUF2  ! Buffer for line items                    *~
            * #11 ! APCPLNSA ! Daily Sales Analysis                     *~
            * #12 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #13 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #14 ! WCMASTR  ! Workcenter Master File                   *~
            * #15 ! WCOUT    ! Planned work center use detail record    *~
            * #16 ! PIPIN    ! Planned inventory additions detail       *~
            * #17 ! PIPOUT   ! Planned inventory use detail record      *~
            * #18 ! SFMASTR2 ! Sales forecast master file               *~
            * #19 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #20 ! PIPCROSS ! hard peg cross reference                 *~
            * #21 ! BOMSPEC  ! options selected file                    *~
            * #22 ! JBPIPXRF ! option part harder peg                   *~
            * #23 ! ESTMASTR ! Estimate master file                     *~
            * #24 ! BCKLNCUR ! Transaction Currency File                *~
            * #25 ! EWDSCHED ! Special Schedule Analysis                *~
            * #30 ! MLQMASTR ! Quotation Master file                    *~
            * #31 ! CATEGORY ! System Category File for Sales (EWD005)  *~
            * #33 ! MLQCROSS ! Quotation-to-S.O. Cross Reference file   *~
            * #35 ! CUSTOMER ! Customer Master File        (EWD) Begin  *~
            * #36 ! APCPLNOR ! APC Planning S.O. Header History         *~
            * #37 ! APCPLNSC ! APC Planning Master Scheduling File      *~
            * #38 ! GENCODES ! Master Code Table Files                  *~
            * #39 ! INVPULLS ! Pull from Inventory Master    PAR000     *~
            * #40 ! APCPLNDP ! Planning Master Department File          *~
            * #41 ! APCPLNSD ! Planning S.O. Scheduling Dept Detail     *~
            * #42 ! APCSOAUD ! APC Daily Sales Order Audit File         *~
            * #43 ! APCPLNER ! APC Planning Error Log                   *~
            * #44 ! TXTFILE  ! Text File VIQS002                        *~
            * #45 ! ARMTERMS ! Terms File VIQS002      (EWD) End        *~
            * #46 ! ARIBUFFR ! A/R Buffer File Header  (AWD023)         *~
            * #47 ! ARIBUF2  ! A/R Buffer File Lines   (AWD023)         *~
            * #48 ! ARMTRIAL ! A/R Trial Balance File  (AWD023)         *~
            * #51 ! CCRMASTR ! Customer Credit Master file (EWD013)     *~
            * #52 ! BNKMASTR ! Bookings Master File                     *~
            * #53 ! BNKLINES ! Bookings Line File                       *~
            * #56 ! KANBANPT ! Master Kanban Mfg Part File   (AWD026)   *~
            * #57 ! HLDSCHED ! New EWDSCHED                  (AWD027)   *~
            * #58 ! ORADESC2 ! WW Description File          (SR66200)   *~
            * #59 ! BCKMASTR2! Backlog master file 2         (CR2068)   *~
            * #60 ! BCKLIN2  ! Backlog lines file 2          CR2829     *~
            * #63 ! BCKSUBPT ! Sub Part File                 (AWD016)   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #2,  "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25, dup

            select #3,  "INVQUAN",                                       ~
                        varc,     indexed,  recsize =  768,              ~
                        keypos =   17, keylen =  64,                     ~
                        alt key  1, keypos =    1, keylen =  64

            select #4,  "INVMASTR",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =  45,                     ~
                        alt key  1, keypos =  122, keylen =   9, dup,    ~
                            key  2, keypos =  110, keylen =   4, dup,    ~
                            key  3, keypos =   46, keylen =  32, dup

            select #5,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #6,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #7,  "BCKBKGRF",                                      ~
                        varc,     indexed,  recsize =  88,               ~
                        keypos =  1,    keylen = 33

            select #8,  "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  11,   keylen =  29,                    ~
                        alt key  1, keypos =    1, keylen =  39

            select #9,  "BCKBUFFR",                                      ~
                        varc,     indexed,  recsize = 1020,              ~
                        keypos =    1, keylen =  10,                     ~
                        alt key  1, keypos =    4, keylen =   7, dup,    ~
                            key  2, keypos =   30, keylen =  16

            select #10, "BCKBUF2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

                                                    /* (EWD) - Begin */
            select #11, "APCPLNSA",                                      ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos = 11,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen = 19, dup
                                                    /* (EWD) - End   */
            select #12, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =  94,               ~
                        keypos =  29,  keylen = 19,                      ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47

            select #13, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #14, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #15, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #16, "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #17, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #18, "SFMASTR2",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  25

            select #19, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25

            select #20, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =  1,   keylen = 71,                      ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33

            select #21, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #22, "JBPIPXRF",                                      ~
                        varc,     indexed,  recsize =  63,               ~
                        keypos = 1,    keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19

            select #23, "ESTMASTR",                                      ~
                        varc,     indexed,  recsize =  2000,             ~
                        keypos =   10, keylen =  8,                      ~
                        alt key  1, keypos =  18, keylen =  30, dup,     ~
                            key  2, keypos =  48, keylen =  25, dup,     ~
                            key  3, keypos =   1, keylen =  17,          ~
                            key  4, keypos = 416, keylen =  16, dup

            select #24, "BCKLNCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 5,    keylen =  19,                     ~
                        alt key  1, keypos =   1, keylen =  23

            select #25, "EWDSCHED",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  38,                     ~
                        alt key  1, keypos =   16, keylen =  23

            select #26, "NCBCKUPD"                                       ~
                                consec , recsize = 100

            select #27, "NEBCKUPD"                                       ~
                                consec , recsize = 100

            select #28, "DFXBKUPD"                                       ~
                                consec , recsize = 100

            select #29, "DNFBKUPD"                                       ~
                                consec , recsize = 100

            select #30,  "MLQMASTR",                                     ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =   10, keylen =   8,                     ~
                        alt key  1, keypos =    1, keylen =  17
                                                 /* (EWD005) - Begin   */
            select #31, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4
                                                 /* (EWD005) - End     */
            select #33,  "MLQCROSS",                                     ~
                        varc,     indexed,  recsize =    41,             ~
                        keypos =    1, keylen =  33,                     ~
                        alt key  1, keypos =    1, keylen =   8, dup,    ~
                            key  2, keypos =    9, keylen =  25, dup,    ~
                            key  3, keypos =   18, keylen =  16, dup,    ~
                            key  4, keypos =    9, keylen =   9, dup

                                                     /* (EWD) - Begin  */
            select #35,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #36,  "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #37,  "APCPLNSC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #38, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #39,  "INVPULLS",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   10, keylen =   10,                    ~
                        alt key  1, keypos =    1, keylen =  19,         ~
                            key  2, keypos =   20, keylen =  45, dup

            select #40,  "APCPLNDP",                                     ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =   11, keylen =   12,                    ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos =    4, keylen =  12,         ~
                            key  3, keypos =    1, keylen =  15
/* (AWD015) - Mod to key and reclen */
            select #41, "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  23

            select #42, "APCSOAUD",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =    7, keylen =  19

            select #43, "APCPLNER",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =    1, keylen =  39

        /* VIQS002 */
            select #44, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

        /* VIQS002 */

            select #45, "ARMTERMS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  20


        /* (AWD023) */
            select #46, "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos = 1, keylen =   17,                       ~
                        alt key  1, keypos = 2001, keylen =   24

            select #47, "ARIBUF2",                                       ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =   1,  keylen = 20

            select #48, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21


            select #49, "STXCODES",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  10


        /* (AWD023) */
                                /* (EWD) - End      */
                                /* (EWD013) - Add CCRMASTR */
            select #51, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            select #52, "BNKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  16,                     ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup

            select #53, "BNKLINES",                                      ~
                        varc,     indexed,  recsize =  336,              ~
                        keypos =    1, keylen =  19,                     ~
                        alt key  1, keypos =   46, keylen =  19, dup
/* (AWD016)  */

            select #54, "STORNAME"                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  3

            select #55, "ARINUMBR",                                      ~
                        varc,     indexed,  recsize =   17,              ~
                        keypos =  1,   keylen = 17,                      ~
                        alt key  1, keypos =    10, keylen =  8, dup

/* (AWD026) */
            select #56, "KANBANPT",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =  1,    keylen = 10,                     ~
                        alt key  1, keypos =   11, keylen =  45

/* (AWD027) */
            select #57, "HLDSCHED",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  40,                     ~
                        alt key  1, keypos =   16, keylen =  25,         ~
                            key  2, keypos =   27, keylen =  16

/* (SR66200) */
            select #58, "ORADESC2",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =   1,   keylen = 11

/* (CR2068) */
            select #59, "BCKMAST2",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup
/* CR2829 */
            select #60, "BCKLIN2",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  19
                        
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup

        call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "SYSFILE2" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "DEMMASTR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "INVQUAN" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "INVMASTR" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
REM            filename$ = "BCKBKGRF"  call "EWDOPEN" (#7, filename$, err%)
REM            if err% <> 0% then gosub open_error
            filename$ = "BCKPRIDX" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKBUFFR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKBUF2" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
REM            filename$ = "APCPLNSA"  call "EWDOPEN" (#11, filename$, err%)
REM            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 100%, " ")


            filename$ = "JBCROSS2" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PIPMASTR" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "WCMASTR" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "WCOUT" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PIPIN" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PIPOUT" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SFMASTR2" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SFCUM2" : call "EWDOPEN" (#19, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PIPCROSS" : call "EWDOPEN" (#20, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BOMSPEC" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "JBPIPXRF" : call "EWDOPEN" (#22, filename$, err%)
            if err% <> 0% then gosub open_error
REM            filename$ = "ESTMASTR"  call "EWDOPEN" (#23, filename$, err%)
REM            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#23, fs%(23%), f2%(23%), 100%, " ")

            call "OPENCHCK" (#24, fs%(24%), f2%(24%),   0%, " ")
            filename$ = "EWDSCHED" : call "EWDOPEN" (#25, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CATEGORY" : call "EWDOPEN" (#31, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "MLQCROSS" : call "EWDOPEN" (#33, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#35, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#36, filename$, err%)
            if err% <> 0% then gosub open_error
REM            filename$ = "APCPLNSC"  call "EWDOPEN" (#37, filename$, err%)
REM            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#37, fs%(37%), f2%(37%), 100%, " ")

            filename$ = "GENCODES" : call "EWDOPEN" (#38, filename$, err%)
            if err% <> 0% then gosub open_error
REM         filename$ = "APCPULLS"   call "EWDOPEN" (#39, filename$, err%)
REM         if err% <> 0% then gosub open_error
            call "OPENCHCK" (#39, fs%(39%), f2%(39%), 100%, " ")
            filename$ = "APCPLNDP" : call "EWDOPEN" (#40, filename$, err%)
            if err% <> 0% then gosub open_error


REM            filename$ = "APCPLNSD"  call "EWDOPEN" (#41, filename$, err%)
REM            if err% <> 0% then gosub open_error
REM            filename$ = "APCSOAUD"  call "EWDOPEN" (#42, filename$, err%)
REM            if err% <> 0% then gosub open_error
REM            filename$ = "APCPLNER"  call "EWDOPEN" (#43, filename$, err%)
REM            if err% <> 0% then gosub open_error


            call "OPENCHCK" (#41, fs%(41%), f2%(41%), 100%, " ")

            call "OPENCHCK" (#42, fs%(42%), f2%(42%), 100%, " ")

            call "OPENCHCK" (#43, fs%(43%), f2%(43%), 100%, " ")


            filename$ = "TXTFILE" : call "EWDOPEN" (#44, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARMTERMS" : call "EWDOPEN" (#45, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARIBUFFR" : call "EWDOPEN" (#46, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARIBUF2" : call "EWDOPEN" (#47, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARMTRIAL" : call "EWDOPEN" (#48, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "STXCODES" : call "EWDOPEN" (#49, filename$, err%)
            if err% <> 0% then gosub open_error


            filename$ = "CCRMASTR" : call "EWDOPEN" (#51, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#52, fs%(52%), f2%(52%), 100%, " ")
            call "OPENCHCK" (#53, fs%(53%), f2%(53%), 100%, " ")
/* (AWD016) */

            filename$ = "STORNAME" : call "EWDOPEN" (#54, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARINUMBR" : call "EWDOPEN" (#55, filename$, err%)
            if err% <> 0% then gosub open_error

/* (AWD026) */
            filename$ = "KANBANPT" : call "EWDOPEN" (#56, filename$, err%)
            if err% <> 0% then gosub open_error
/* (AWD027) */
            call "OPENCHCK" (#57, fs%(57%), f2%(57%), 100%, " ")

/* (SR66200) */
            call "OPENCHCK" (#58, fs%(58%), f2%(58%), 100%, " ")
            call "OPENCHCK" (#59, fs%(59%), f2%(59%), 100%, " ") /*(CR2068)*/
            call "OPENCHCK" (#60, fs%(60%), f2%(60%), 100%, " ") /*(CR2829)*/
            
            call "OPENCHCK" (#63, fs%(63%), f2%(63%), 100%, " ")

            mat f1% = zer

            select printer  /* In an attempt to print unplan report */
                            /* where the users wants it.            */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            debug% = 0%

            rptid$ = "BCK014"

*        Dig Up Port Id For Inter-Task Messaging...
REM            call "MSGCREAT" (#1, port$, "PORT.ID.BCKUPDTE", return%)
REM               if return% <> 0% then end

*        Remove any left over 'Kill me' tranactions.
            bufferkey$ = " "
            call "DELETE" (#9, bufferkey$, 3%)

            call "BCKSWTCH" ("BCK", "IDLETIME", message$, idle, u3%)
            if idle < 10 then idle = 10
            message$ = " "
            inactive% = -1% /* Force First Shostat */

*        Load in Planning System Flags for Allocations.  Use Default
*        Values if record is not on file.
            call "PIPINDEX" (#1, " ", today%, ret%)
            call "READ100" (#1, "PLANNING SYSTEM FLAG", f1%(1))
            if f1%(1) = 1% then L09330
                str(planflags$(), 1,1) = "N"       /* Display Option   */
                str(planflags$(),34,6) = hex(030000000100) /* SS Intru */
                str(planflags$(),82,6) = hex(000000000100) /* Ign ATC  */
                goto L09360
L09330:     get #1, using L09340, str(planflags$(),1,480)
L09340:         FMT XX(20), CH(480)

L09360:     call "READ100" (#1, "SWITCHS.CUR", f1%(1))
               if f1%(1) = 0% then L09500
            get #1, using L09390, stat$
L09390:         FMT POS(22), CH(4)

L09500
*        Load HNYFLAGS values for MPS/PFM Usage Capture
            usage_on$ = "N"
            call "READ100" (#1, "SWITCHS.HNY", f1%(1%))
                if f1%(1%) = 0% then L09570
            get #1, using L09550, usage_store$, usage_on$, usage_date$
L09550:         FMT POS(109), 2*CH(1), POS(119), CH(1)

L09570
*        Get Multi-Line Quotation processing values, if present.
            mlqxref$, mlqyorn$ = "N"/* Default = Negatory on MLQ front */
            call "READ100" (#01, "SWITCHS.MLQ", f1%(1%))   /* SYSFILE2 */
            if f1%(1%) <> 0% then get #01 using L09610, mlqyorn$, mlqxref$
L09610:         FMT POS(32), CH(1), POS(41), CH(1)
            if mlqyorn$ <> "Y" then goto L10000    /* No MLQ processing */
                call "OPENCHCK" (#30, fs%(30%), f2%(30%), 0%, " ")
                if mlqxref$ = "Y" then /* OPEN MLQCROSS Cross-Ref file?*/~
                     call "OPENCHCK" (#33, fs%(33%), f2%(33%), 100%, " ")

L10000: REM *************************************************************~
            *           G E T   N E X T   T R A N S C A T I O N         *~
            * --------------------------------------------------------- *~
            * Wait for the next Sales Order to appear in the buffer or  *~
            * for a time out / shutdown instruction.                    *~
            *************************************************************

               s_err% = 0%
               call "SCHEMA" (schema$, schema%, #38, s_err%)

               if s_err% = 0% then goto run_oracle


               goto exit_program

run_oracle:


        if schema% = 1% then gosub open_ncfile
        if schema% = 2% then gosub open_nefile
        if schema% = 4% then gosub open_dfxfile
        if schema% = 5% then gosub open_dnffile

        gosub oracle_connect                               /* (EWD013) */
        if oci_err% < 0 then goto exit_program

        next_trans        /* Looking into buffer for a PO to update.   */
            pass% = 0%                                     /* (EWD011) */

* Flush Data Before Query
            gosub oracle_flush
            init(" ") stmt1$, stmt2$, time$                 /* (EWD013) */
            stmt1$  = "select * from OE_BCKBUFFR WHERE BCK_TIME <> '0'"
            stmt1$  = stmt1$ & " order by BCK_TIME ASC"


            if schema% = 2% then str(stmt1$,15%,2%) = "NE"
            if schema% = 5% then str(stmt1$,15%,2%) = "NE"


            time$ = time
            gosub oracle_query
            gosub oracle_fetch                             /* (EWD013) */

REM  if oci_err% <= 0% then order found
            if oci_err% <= 0% then goto check_out_trans

            sleep% = 0%
            sleep% = 30%
      /* Sleep 30 sec if NC */
REM IF SCHEMA% <> 2% THEN SLEEP% = 30%
      /* Sleep one minute if NE*/
REM IF SCHEMA% = 2% THEN SLEEP% = 60%
REM IF SCHEMA% = 5% THEN SLEEP% = 60%
      /* Sleep one minute if NE*/
REM IF SCHEMA% = 2% THEN SLEEP% = 60%
REM IF SCHEMA% = 5% THEN SLEEP% = 60%

            if debug% = 0% then goto noSleepDebug
              sleep% = 0%
              call "SHOSTAT" (" SLEEP " ) : stop
noSleepDebug:
            call "EWDSLEEP" (sleep%, er%)
REM IF STR(TIME$,1%,2%) < "18" THEN INACTIVE% = 0
                inactive% = max(inactive%, 1%)

REM IF MESSAGE$  = "CANCEL" THEN EXIT_PROGRAM

REM IF INACTIVE% > IDLE THEN EXIT_PROGRAM
REM CALL "MSGBFGND" (" ", PORT$, MESSAGE$, 10%, INACTIVE%, U3%)


                if u3% = 16 then exit_program    /* Something's Whacko */
                     goto next_trans

        check_out_trans   /* See if what we've found is to be updated  */
            if inactive% <> 0% then call "SHOSTAT" ("Processing")
            inactive% = 0%     /* Reset Idle Time Control    */

            field_num% = 1%
            gosub oracle_getfield
            userid$ = field$
            if userid$ <> " " then goto continueProcessing
               goto next_trans
REM IF USERID$ = " " THEN EXIT_PROGRAM     /* Kill Update Task */

continueProcessing:

        REM *************************************************************~
            *            P R O C E S S   T R A N S A C T I O N          *~
            * --------------------------------------------------------- *~
            * Control loading and processing of Sales Order.  Toss      *~
            * lines as we go to facilitate restarting (BCKINPUT always  *~
            * writes lines found in Master to the buffer; therefore if  *~
            * not found in buffer the line has already been processed). *~
            *************************************************************

        gosub initialize

        gosub load_headers
REM GOSUB UPDATE_CCRMASTER                             /*  (EWD013)  */


*       *** Process LINES ITEMS ****************************************
*        Get next line from buffer and, if any, the corresponding line *
*        from the master.  Update the backorder quantity and Sales     *
*        Analysis.  Then toss the line into the Master file.  Lastly,  *
*        update the Planning System Demand and PIPs.                   *
*       ****************************************************************

        linekey$ = str(so$) & hex(00)

        line_loop
* #10 is New Order #6 is Delete or Resumbit
          n% = 10%                                           /* (EWD010) */
          if str(adjrsn$,1%,1%) = "D" or                     ~
             str(adjrsn$,1%,1%) = "R" then n% = 6%           /* (EWD010) */



          if n% = 10% then goto oracle_line                   /* (EWD013) */
             call "PLOWNEXT" (#n%, linekey$, 16%, f1%(n%))    /* (EWD010) */

                                                             /* (EWD011) */
          if f1%(n%) <> 0% or str(adjrsn$,1%,1%) <> "R" then goto L10620
             linekey$ = str(so$) & hex(00)
             adjrsn$ = "N"
             pass% = 1%
             gosub bnk_old_header
             init(" ") trans_number$, bnkdate$, bnktime$      /*  (EWD014) */
             goto line_loop

L10620:
          if f1%(n%) = 0% then header_update                 /* (EWD010) */
          goto get_line_info
        oracle_line                                          /* (EWD013) */
          gosub build_query
          gosub oracle_fetch
          if oci_err% > 0% then goto header_update
        get_line_info
          gosub load_lines                                   /* (EWD011) */
          if str(newline$()) = str(oldline$()) and newstore$ = oldstore$ ~
                           and str(adjrsn$,1%,1%) <> "D"                 ~
                           and str(adjrsn$,1%,1%) <> "R"    then L10689

*        Adjust INVQUAN (Quantity On Order)
            if oldline% = 0% or oldopenqty = 0 then L10630
                qty = -oldopenqty
                call "INVPST1" (part$, subpart$, oldstore$, oldlot$,     ~
                                0, qty,        0, 0, 0,                  ~
                                #3, #4, #1, f2%(3),f2%(4),f2%(1),0%,u3%)

               if str(adjrsn$,1%,1%) = "D"  or             /* (EWD013) */~
                  str(adjrsn$,1%,1%) = "R"    then L10689
L10630:     if newopenqty = 0 then L10664
                call "INVPST1" (part$, subpart$, newstore$, newlot$,     ~
                                0, newopenqty, 0, 0, 0,                  ~
                                #3, #4, #1, f2%(3),f2%(4),f2%(1),0%,u3%)

L10664
*        Check for MPS/PFM Usage Capture
            if pos("DSB" = usage_on$) > 0% then gosub usage_capture

*        Update Estimate Master File...
REM do not estimate logic anymore
            goto L10689

            if oldline% = 0% or oldest$ = " " then L10679
            call "READ101" (#23, oldest$, f1%(23))
                if f1%(23) = 0 then L10679
            put #23, using L10677, " ", " "
            rewrite #23
L10677:     FMT POS(1467), CH(16), CH(3)

L10679:     if newest$ = " " then L10689
            call "READ101" (#23, newest$, f1%(23))
                if f1%(23) = 0 then L10689
            put #23, using L10677, so$, str(newline$(),26,3)
            rewrite #23

L10689
*        Update Sales Analysis
            get str(newhdr$()) using L10710, neworderdate$
            get str(oldhdr$()) using L10710, oldorderdate$
L10710:         FMT POS(806), CH(6)

            if oldhdr% = 0% or oldline% = 0% then L11020
            if resurrected% = 1% then L11020  /* Resurrected do as new */
                oldsagross = round(oldorderqty * oldprice, 2)
                temp       = round(oldsagross  * olddiscpct * .01, 2)
                oldsanet   = oldsagross - temp
                oldsagross = -oldsagross
                oldsanet   = -oldsanet
                recordtype$ = "2"
                if crhold$ = "C" then recordtype$ = "5"
                put oldsa$() using L11430,                                ~
                     recordtype$,                  /* Adj. or Cancelled*/~
                     " ",                          /* Date/ Time Stamp */~
                     oldorderdate$,                /* SA Post Date     */~
                     so$, " ", " ",                /* SO, BOL, Invoice */~
                     str(newline$(), 26, 3),       /* Sequence Number  */~
                     " ",                          /* Date Shipped     */~
                     str(newhdr$(), 848, 9),       /* Adj Reason Code  */~
                     -oldorderqty,                 /* Open Units       */~
                     oldsagross,                   /* Gross Value      */~
                     oldsanet,                     /* Net Value        */~
                     0,                            /* Cost             */~
                     " ",                          /* Stocked Item?    */~
                     part$,                        /* Part Code        */~
                     str(oldline$(), 89, 4),       /* Category Code    */~
                     str(oldhdr$(), 884, 9),       /* Account Xref     */~
                     cuscode$,                     /* Customer Code    */~
                     str(oldhdr$(), 882, 2),       /* Customer Type    */~
                     oldstore$,                    /* Store Code       */~
                     str(oldhdr$(), 595, 4),       /* Region Code      */~
                     str(oldhdr$(), 580, 4),       /* Salesman         */~
                     -1,                           /* Inventory Cost   */~
                     currency$,          /* Currency                   */~
                     convdate%,          /* Convertion Eff. Date       */~
                     conveqv,            /* Convertion Factor          */~
                     convunt,            /* Convertion Factor          */~
                     cprice1,            /* Trans. Price (Stcking)     */~
                     cprice2,            /* Trans. Price (Prcing)      */~
                     " "                           /* Filler           */

L11020:         newsagross = round(neworderqty * newprice, 2)
                temp       = round(newsagross  * newdiscpct * .01, 2)
                newsanet   = newsagross - temp
                put newsa$() using L11430,                                ~
                     "1",                          /* New Bookng Trans */~
                     " ",                          /* Date/ Time Stamp */~
                     neworderdate$,                /* SA Post Date     */~
                     so$, " ", " ",                /* SO, BOL, Invoice */~
                     str(newline$(), 26, 3),       /* Sequence Number  */~
                     " ",                          /* Date Shipped     */~
                     str(newhdr$(), 848, 9),       /* Adj Reason Code  */~
                     neworderqty,                  /* Open Units       */~
                     newsagross,                   /* Gross Value      */~
                     newsanet,                     /* Net Value        */~
                     0,                            /* Cost             */~
                     " ",                          /* Stocked Item?    */~
                     part$,                        /* Part Code        */~
                     str(newline$(), 89, 4),       /* Category Code    */~
                     str(newhdr$(), 884, 9),       /* Account Xref     */~
                     cuscode$,                     /* Customer Code    */~
                     str(newhdr$(), 882, 2),       /* Customer Type    */~
                     newstore$,                    /* Store Code       */~
                     str(newhdr$(), 595, 4),       /* Region Code      */~
                     str(newhdr$(), 580, 4),       /* Salesman         */~
                     -1,                           /* Inventory Cost   */~
                     currency$,          /* Currency                   */~
                     convdate%,          /* Convertion Eff. Date       */~
                     conveqv,            /* Convertion Factor          */~
                     convunt,            /* Convertion Factor          */~
                     cprice1,            /* Trans. Price (Stcking)     */~
                     cprice2,            /* Trans. Price (Prcing)      */~
                     " "                           /* Filler           */

*        Now see if there's a need to Update Sales Analysis Files
          if oldhdr% = 0% or oldline% = 0% then donew /* No Old Line   */
          if resurrected% = 1% then donew /* Resurrected treated as new */
            if crhold$ = "C"                    then doold /* Cancelled*/
            if neworderdate$ <> oldorderdate$   then doold /* Perd Chg */
            if str(oldsa$(),93) <> str(newsa$(),93)                      ~
                then doold                                 /* Code Chg */
            if neworderqty - oldorderqty <> 0   then doold /* Qty Chg  */
            if newsagross + oldsagross <> 0     then doold /* $$$ Chg  */
            if newsanet   + oldsanet   <> 0     then doold /* $$$ Chg  */
            goto L11480  /* Same New Line as the Old- Don't do a thing  */
          doold /* Need to Do OLD (implies adjusting transactions)     */
                str(newsa$(),,1) = "2"
                  if crhold$ = "C" then str(newsa$(),,1) = "5"
                call "SAPOSTSB" (oldsa$(), #1, #4)
          donew /* Need to do NEW IF the line hasn't been deleted or   */
/* VIQS001 */
            if str(newline$(),57%,1%) = hex(ff) or                   ~
                    str(newline$(),57%,1%) = hex(01) then L11480
            if str(adjrsn$,1%,1%) = "D" or str(adjrsn$,1%,1%) = "R" then  ~
                                             goto L11480   /* (EWD010) */
                                                           /* (EWD011) */

            if crhold$ = "C" then L11480                  /* cancelled. */
                call "SAPOSTSB" (newsa$(), #1, #4)

L11430:         FMT CH(1), CH(7), CH(6), CH(16), CH(3), CH(8), CH(3),    ~
                    CH(6), CH(9), 4*PD(14,4), CH(1), CH(25), CH(4),      ~
                    CH(9), CH(9), CH(2), CH(3), CH(4), CH(4), PD(14,4),  ~
                    CH(4), BI(4), 2*PD(14,7), 2*PD(14,4), CH(100)

L11480
*        Toss Line into Master
            call "DELETE" (#6, linekey$, 19%)
                                                           /* (EWD011)     */
            call "DELETE" (#60, linekey$, 19%)             /* CR2829       */

/* VIQS001 */
            if str(newline$(), 57%, 1%) = hex(ff) or                     ~
                    str(newline$(),57%,1%) = hex(01) then L11540 /* Deleted  */
                if str(adjrsn$,1%,1%) = "D" or                           ~
                   str(adjrsn$,1%,1%) = "R"  then goto L11540   /* (EWD010) */
                                                                /* (EWD011) */
                put #6 using L11520, str(newline$())
L11520:              FMT CH(300)
                write #6

/* CR3305 changes to allow all customers */
/* CR2829 */    init(" ") ne2line$    
REM                if str(newline$(),1%,2%) <> "LO" and ~
                   str(newline$(),1%,2%) <> "LX"     then goto L11540

REM                if str(newline$(),290%,1%) = "0"  or ~
                   str(newline$(),290%,1%) = " "  then goto L11540
                   
                str(ne2line$,1%,28%) = str(newline$(),1%,28%)
                str(ne2line$,29%,10%) = str(newline$(),290%,10%)
                gosub calc_gs128
                str(ne2line$,39%,20%) = gs128$
                str(ne2line$,59%, 3%) = shipseptype$
                str(ne2line$,62%, 3%) = shiplinenbr$
                str(ne2line$,65%,18%) = shipbarcode$                
                put #60 using L11530, str(ne2line$)
L11530:              FMT CH(256)
                write #60
      
L11540:     if str(adjrsn$,1%,1%) = "R" then goto L11560   /* (EWD011) */

            gosub oracle_lne_del
            gosub oracle_lne_text                          /*  (EWD013) */

L11560:
*        Remove Shadow files

/* VIQS001 */
REM         if str(newline$(),57%,1%) <> hex(ff) and                     ~
                   str(newline$(),57%,1%) <> hex(01) then L11570 /*No Deleted*/

            if str(adjrsn$,1%,1%) <> "D" and str(adjrsn$,1%,1%) <> "R"   ~
                                                       then goto L11570

            /* Remove Cross Reference Shadow Record */
            call "PTUSEDSB" ("D", "BCK ", str(linekey$,,16%),            ~
                             str(linekey$,17%,3%), " ", " ", " ", ret%)

            /* Remove Precious Metal Surcharge Shadow File */
            call "PMCALSUB" (" ", " ", 0, 0, 1, "R", " ", 0, "S",        ~
                             str(linekey$,,19%), " ", " ", 0%)

L11570
*        And lastly, take care of Planning Interface for this line item
            if str(newline$()) = str(oldline$()) and                     ~
                   newstore$   = oldstore$       then L11795

            call "SETPRNT" (rptid$, " ", 0%, 0%) /* For unplan rpt */
                                              /* (EWD) - Begin         */
            call "APCPIPSB" (so$,             /* Sales Order Number    */~
                             str(newline$(),26,3), /* Seq Number       */~
                             newstore$,       /* For new transaction   */~
                             crhold$,         /* "H" = on Hold         */~
                             #6 ,             /* BCKLINES              */~
                             #2 ,             /* DEMMASTR              */~
                             #1 ,             /* SYSFILE2              */~
                             #13,             /* PIPMASTR              */~
                             #16,             /* PIPIN                 */~
                             #17,             /* PIPOUT                */~
                             #21,             /* BOMSPEC               */~
                             #14,             /* WCMASTR               */~
                             #15,             /* WCOUT                 */~
                             #18,             /* SFMASTR2              */~
                             #19,             /* SFCUM2                */~
                             #12,             /* JBCROSS2              */~
                             #20,             /* PIPCROSS              */~
                             #22 )            /* JBPIPXRF              */
                                              /* (EWD) - End           */
            call "SETPRNT" (rptid$, " ", 0%, 1%) /* For unplan rpt */

L11795
*        Here are the final operations for the line items.
            gosub process_line_for_mlq

REM            call "SHOSTAT" ("WRITING AWDBNKLN")  stop
               init(" ") bnklne$()
               bnk_err% = 0%
               if adjrsn$ = "N" then bnklne$() = newline$()              ~
                  else               bnklne$() = oldline$()
               call "AWDBNKLN" (bnklne$(), adjrsn$, trans_number$,       ~
                                bnkdate$, bnktime$, #38, #53, bnk_err%)

REM            call "SHOSTAT" ("DONE AWDBNKLN")  stop


/* (AWD016) - Beginning */

REM               if adjrsn$ <> "N" and adjrsn$ <> "D" then goto line_loop


REM            call "SHOSTAT" ("WRITING BCKSUBPT")  stop
               init(" ") bcksubpt_rec$, flds$(), info_flds$()
               subpterr% = 0%
               flag$ = "0"              /* Call By Sales Order */
               pgm$  = "0"              /* Called By BCKUPDTE  */
               if adjrsn$ = "D" then pgm$ = "2" /* To Delete Order */
               if adjrsn$ = "R" then pgm$ = "2" /* To Delete Order */

               gosub set_bcksubpt

               call "AWDBKSUB" (   flag$, pgm$, so$, seq$,           ~
                                bcksubpt_rec$, flds$(), info_flds$(),~
                                                   #63, subpterr%    )



REM            call "SHOSTAT" ("DONE BCKSUBPT")  stop
/* (AWD016) - End       */


/* (SR66200) */
               oradescerr% = 0%
               gosub set_oradescr

               call "AWDORASB" (pgm$, so$, seq$, oradescr_rec$(), #58,  ~
                                                           oradescerr%)
/* (SR66200) */

            goto line_loop


*       ** HEADER UPDATING *********************************************
*        Update the report and print files as required and then toss   *
*        the new header into the master if there are any lines left    *
*       ****************************************************************
        header_update

*        Check to see if there are any lines left on the order
            linekey$ = str(so$) & hex(00)
                                                         /* (EWD006)   */
            linesleft% = 0%
            read #6,hold,key > linekey$, using L11800, linekey$,         ~
                                                           eod goto L11810
L11800:          FMT POS(10), CH(19)
            if str(so$,1%,8%) <> str(linekey$,1%,8%) then goto L11810
REM CONVERT STR(LINEKEY$,17%,3%) TO LINESLEFT%, DATA GOTO L11810

REM IF LINESLEFT% > 0% THEN LINESLEFT% = 1%
REM IF LINESLEFT% < 1% THEN DELETE #6       /* NOT A VALID RECORD */
               linesleft% = 1%
L11810:
                                                         /* (EWD006)      */
REM call "SHOSTAT" ("WRITING AWDBNKMR")  stop

          init(" ") bnkhdr$()
          bnk_err% = 0%

/* CR3186 */		  
		  if adjrsn$ = "N" then str(newhdr$(), 991, 1) = prefix_so$       ~
		     else               str(oldhdr$(), 991, 1) = prefix_so$	  

          if adjrsn$ = "N" then bnkhdr$() = newhdr$()                      ~
             else               bnkhdr$() = oldhdr$()

          call "AWDBNKMR" (bnkhdr$(), adjrsn$, trans_number$, bnkdate$,    ~
                          bnktime$, #38, #52, bnk_err%)

REM call "SHOSTAT" ("DONE AWDBNKLN")  stop

*        Now toss the Header around a bit
            if credit% = 1% then                                        ~
                         put str(newhdr$(),846,8) using L62050, gross_amt

            readkey$ = str(cuscode$) & so$
            call "DELETE" (#5, readkey$, 25%)   /* Remove Old Header   */
            if linesleft% = 0% then L12380        /* Order Deleted      */
                put #5 using L12360, str(newhdr$())
L12360:              FMT CH(1000)
                write #5

L12380:
            call "DELETE" (#59, readkey$, 25%)  /*Remove Old Header (CR2068)*/                
            if linesleft% = 0% then L12385      /* Order Deleted    (CR2068)*/
                put #59 using L12360, str(newhdr2$())        /* (CR2068)    */

                write #59                                    /* (CR2068)    */

                                                 /* (EWD) - Begin      */
L12385:                                          /* APC MOD - 04/01/96 */
                                                 /* OPT% = 1% - ADDED  */
                                                 /* OPT% = 8% - CHANGED*/
                                                 /* OPT% = 9% - DELETED*/

            gosub oracle_hdr_text                            /*  (EWD013) */
            or_cuscode$ = cuscode$
            or_so$ = so$

            if oldhdr% = 0% then opt% = 1%                    /* (EWD011) */
            if oldhdr% <> 0% then opt% = 8%                   /* (EWD011) */
            if linesleft% = 0% or str(adjrsn$,1%,1%) = "D" then opt% = 9%
                                               /*  (EWD011)            */
            or_hows$= str(newhdr$(), 422%, 2%) /* Set How Ship Code    */
            or_fob$ = str(newhdr$(), 442%, 2%) /* Set Delivery Code    */
            or_sls$ = str(newhdr$(), 580%,4%)  /* Set Salesman Code    */
            or_text$ = str(newhdr$(),799%,4%)  /* Header Text Id       */
REM **** Take out before putting into production *****
REM            opt% = 8%                           /*(AWD023)*/
            if or_hows$ <> "99" and credit% = 0% then gosub update_planning ~
               else gosub update_credit

        /* VIQS002 */
            if fax_ack$ = "F" then gosub fax_ack

            gosub oracle_delete_header
            goto next_trans
                                                 /* (EWD) - End        */


REM L13000 err% = 91%                    /* Log data conversion error for */
REM        gosub update_error_log        /* write to BCKBKGRF.(EWD004-NEW)*/
REM        return


           bnk_old_header
REM            call "SHOSTAT" ("WRITING AWDBNKMR RESUBMIT")  stop
               init(" ") bnkhdr$()
               bnk_err% = 0%
/* CR3186 */	 	  
               if adjrsn$ = "N" then str(newhdr$(), 991, 1) = prefix_so$    ~
		          else               str(oldhdr$(), 991, 1) = prefix_so$
				  
               if adjrsn$ = "N" then bnkhdr$() = newhdr$()                  ~
                  else               bnkhdr$() = oldhdr$()

               call "AWDBNKMR" (bnkhdr$(), adjrsn$, trans_number$, bnkdate$,~
                                bnktime$, #38, #52, bnk_err%)

REM            call "SHOSTAT" ("DONE AWDBNKMR RESUBMIT")  stop

           return

/* CR2829 */

        REM *************************************************************~
            *     Processing GS1-128 for Lowes                          *~
            *************************************************************
        calc_gs128    

            init(" ") gs128$, gsinput$ 
            
            gsinput$ = str(newline$(),266%,9%)   /* offset to 266 for pos 1-9 */
            call "AWDGS128" (gsinput$, gs128$, error%)
            if error% <> 0% then init(" ") gs128$
         
        return
        
        REM *************************************************************~
            *             I N I T I A L I Z A T I O N                   *~
            * --------------------------------------------------------- *~
            * Clear variables prior to processing transaction           *~
            *************************************************************
        initialize
            init (" ") newhdr$(), oldhdr$(), newstore$, oldstore$,       ~
                       crhold$, shiptoname$, adjrsn$, oldcrhold$,        ~
                       stk$(), qty$(), trans_number$, bnkdate$, bnktime$,~
/*PAR000*/             stk_sub$(), stk_desc$(), newhdr2$()
                                                   /* (EWD)             */
            firstship$ = all(hex(ff))
            resurrected% = 0%
            newgross, newdisc, oldgross, olddisc = 0
            newsagross, newsanet, oldsagross, oldsanet, mlqtotal = 0
            return

        REM *************************************************************~
            *            L O A D   D A T A   R O U T I N E S            *~
            * --------------------------------------------------------- *~
            * Routines for loading data from files.                     *~
            *************************************************************

        load_headers
            gosub load_newheader

                                         /* APC MODS - 08/17/90 */

            get str(newhdr$()) using L30120, cuscode$, so$, or_po$,      ~
                     shiptoname$, newstore$, or_due$, adjrsn$, newdisc,  ~
                     newgross, crhold$


L30120:         FMT CH(9), CH(16), CH(16), CH(30), POS(803), CH(3),      ~
              POS(818), CH(6), POS(848), CH(9), XX(2), 2*PD(14,4), CH(1)

            hows$ = str(newhdr$(),422%,2%)          /* (EWD005) - Set   */
                                                    /*   Howship Code   */
                                                    /* (EWD) - End      */
            if debug% = 0% then goto noHowsDebug
              call "SHOSTAT" ("CUST & SO -->  " & cuscode$ & " -- " & so$ )
              stop
noHowsDebug:
            newdisc = round(newgross * newdisc * .01, 2)

            fax_ack$ = str(newhdr$(),639,1)
            str(newhdr$(),639,1) = hex(20)

                                                        
            str(newhdr2$(),01%,25%) = newhdr$()              /* (CR2068) */
            str(newhdr2$(),26%,20%) = nameorderedby$         /* (CR2068) */
            str(newhdr2$(),46%,100%) = plygem_contact$       /* (CR2068) */
            str(newhdr2$(),146%,8%) = distordernbr$          /* CR3305   */
            str(newhdr2$(),154%,22%) = customerpo$           /* CR3305   */ 
            str(newhdr2$(),176%,12%) = distcustnbr$          /* CR3305   */
            str(newhdr2$(),999%,1%) = qlty_dept_flag$        /* CR2825   */

            call "READ100" (#5, str(newhdr$(),,25), oldhdr%)
            if oldhdr% = 0% and adjrsn$ <> "D"                          ~
                            then adjrsn$ = "N"   /* (EWD011) - for New  */

            if oldhdr% = 0% then return
                                            /* (EWD011) - for Resubmit  */
            if str(adjrsn$,1%,1%) <> "D" then str(adjrsn$,1%,1%) = "R"
                get #5 using L30190, str(oldhdr$())
L30190:              FMT CH(1000)
                get str(oldhdr$()) using L30220,                          ~
                          oldstore$, olddisc, oldgross, oldcrhold$
L30220:              FMT POS(803), CH(3), POS(859), 2*PD(14,4), CH(1)
                olddisc = round(oldgross * olddisc * .01, 2)
                if oldcrhold$ = "C" and crhold$ <> "C" then              ~
                                                        resurrected% = 1%

                return

        update_ccrmaster                                   /* (EWD013) - BEG */

*        Update the Customer(s) with the Open Order Dollars
         oldopengross, newopengross = 0.00
         oldopengross = oldgross - round(oldgross * olddisc * .01, 2)
         newopengross = newgross - round(newgross * newdisc * .01, 2)
         if str(adjrsn$,1%,1%) = "D" then newopengross = 0.00

          if abs(newopengross - oldopengross) < .01 then return
            call "READ100" (#35, cuscode$, f1%(35%))
            get #35 using L30250, billto$
REM L30250  FMT POS(780), CH(9)
            topen(1%), topen(2%) = 0
            call "READ101" (#51, cuscode$, f1%(51%))       /* CCRMASTR */
            if f1%(51%) <> 0% then get #51 using L30260, topen(1), topen(2)
L30260:         FMT POS(114), 2*PD(14,4)
            if billto$ = cuscode$ then                                   ~
                topen(1) = topen(1) - oldopengross + newopengross
                topen(2) = topen(2) - oldopengross + newopengross
            if f1%(51%) = 0% then put #51 using L30295, cuscode$, 0, 0, 0,~
                " ", 0, " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0,  ~
                0, 0, 0, " ", " ", " "
            put #51 using L30270, date, topen(1), topen(2), userid$, date
L30270:         FMT POS(84), CH(6), POS(114), 2*PD(14,4), POS(146),      ~
                     CH(3), CH(6)
            if f1%(51%) = 0% then write #51 else rewrite #51

            if billto$ = cuscode$ then return
L30280:         call "READ100" (#35, billto$, f1%(35))
                if f1%(35) = 1% then L30290
                 billto$ = str(cuscode$) /* No Bill to for this Ship to */
                 goto L30280
L30290:     topen(1%), topen(2%) = 0
            call "READ101" (#51, billto$, f1%(51%))        /* CCRMASTR */
            if f1%(51%) <> 0% then get #51 using L30260, topen(1), topen(2)
            topen(1) = topen(1) - oldopengross + newopengross
            if f1%(51%) = 0% then put #51 using L30295, billto$, 0, 0, 0, ~
                " ", 0, " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0,  ~
                0, 0, 0, " ", " ", " "
            put #51 using L30270, date, topen(1), topen(2), userid$, date
            if f1%(51%) = 0% then write #51 else rewrite #51


L30295:     FMT /* File #51- CCRMASTR Master file (input/output)       */~
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
        return                                             /* (EWD013) - END */


        load_lines   /* Load new and old line (dummy up old line if it */
                     /* doesn't exist).  Also take care of allocation. */
            mlq_line, oldsanet, newsanet = 0
*        Load in new line item

            if n% = 10% then gosub load_newline            /* (EWD013) */ ~
            else get #n% using L30310, str(newline$())     /* (EWD010) */

L30310:         FMT CH(300)                                 /* (AWD016) */
            get str(newline$()) using L30340, seq$, part$, part_desc$,   ~
                    neworderqty,                                         ~
                    newopenqty,  newprice,                               ~
                                conv_fact, temp, newdiscpct, neworigdue$,~
                    reqdship$, newlot$, newdemtype$, newest$,            ~
                    mlquote_seq$, brand$
/* band 18 = PlyGem brand 19 = PlyGem THD Series P = PlyGem */
/* (CR1933) BEG */
            plygem_lne$ = "0" 
            if brand$ = "18" or brand$ = "19" then plygem_lne$ = "1"
            if str(series$,01%,01%) = "P" then plygem_lne$ = "1"
/* (CR1933) END */

L30340:         FMT XX(25), CH(3), XX(3), CH(25), CH(32),                ~
                                                  POS(93), PD(14,4),     ~
                    XX(8), PD(14,4), POS(141), PD(14,4), POS(157),       ~
                                                               PD(14,7), ~
                    2*PD(14,4), POS(200), CH(6), POS(212), CH(6), CH(6), ~
                    POS(240), CH(1), POS(255), CH(8), POS(266), CH(11) , ~
                    POS(282), CH(2)

                                              /* (EWD005) - Place    */
                                              /* Category Fix Here   */
            cat$ = str(newline$(),89%,4%)     /* Save Category Code  */
            sls_acct$ = str(newline$(),182%,9%) /* Sales G/L Account */
            dsc_acct$ = str(newline$(),191%,9%) /* Disc G/L Account  */
            gosub build_category

            if cat$ <> new_cat$ then cat$ = new_cat$
                                                /* (EWD007)          */
            sls_acct$ = sales_acct$
            dsc_acct$ = disc_acct$
                                                /* (EWD007)          */
            str(newline$(),89%,4%)  = cat$
            str(newline$(),182%,9%) = sls_acct$
            str(newline$(),191%,9%) = dsc_acct$

                                              /* (EWD005) - Place Fix*/

            newprice = temp / conv_fact       /* More accurate value */

            if reqdship$ < firstship$ then firstship$ = reqdship$

            currency$ = stat$:conveqv, convunt = 1:convdate% = 0%
            cprice1 = newprice:cprice2 = temp:convdate$ = " "

               init(" ") readkey$
               readkey$ = str(cuscode$) & so$
               call "READ100" (#24, readkey$, f1%(24))        /* (EWD013) */
               if f1%(24) = 0% then L30400
            get #24 using L30379, currency$, cprice1, cprice2, convdate$, ~
                                 conveqv, convunt
L30379:        FMT CH(4), POS(24), 2*PD(14,4), POS(40), CH(6), 2*PD(14,7)
            tdate$ = convdate$
            call "DATEFMT" (tdate$, convdate%)

*        Take care of Allocation
L30400:     gosub L30740  /* I need the old line, so load it up first */
            get str(newline$()) using L30410, alloc, preinv, alloc$
L30410:         FMT POS(125), 2*PD(14,4), POS(246), CH(1)
            get str(oldline$()) using L30414, oldalloc$
L30414:         FMT POS(246), CH(1)
            if oldline% = 0% then oldalloc$ = " " /* New line item */
            if newopenqty = 0 and alloc$ <> "C" then alloc$ = "Z"
            if pos("ACNZP" = alloc$) = 0% then alloc$ = "N"
            on pos("ACNZP" = alloc$) goto L30450, L30620, L30650, L30680,L30650
L30450:     /* Allocate to ATC */
            /* If a line is being reallocated 'A' then this logic will */
            /* not give up any previously allocated quantity.  If the  */
            /* user want to reallocate from scratch, then the line     */
            /* must be save using 'Z' first and then save using 'A'.   */
                get str(newline$()) using L30470, type$, priority$
L30470:              FMT POS(240), 2*CH(1)
                alloc_adj = 0
                call "PIPINDEX" (#1, reqdship$, reqdship%, ret%)
                if oldalloc$ <> "C" then L30496
                   alloc_adj = alloc
                   alloc = 0
                   call "PIPINDEX" (#1, oldreqdship$, oldreqdship%, ret%)
                   ret% = oldreqdship%
L30496:         req_qty = min(newopenqty - preinv, newopenqty - alloc)
                call "PLANALLC"                                          ~
                       (part$,           /* Part to Allocate           */~
                        req_qty,         /* Quantity Requested         */~
                        alloc_adj,       /* Quantity Allocatable (IN)  */~
                                         /* Old Alloc Complete Qty(OUT)*/~
                        type$,           /* Demand Type                */~
                        priority$,       /* Priority                   */~
                        reqdship%,       /* Date Required              */~
                        today%,          /* Today's Date               */~
                        planflags$(),    /* Planning System Flags      */~
                        #13, #19,        /* PIPMASTR, SFCUM2           */~
                        ret%)            /* RETURN CODE (IN)           */
                                         /* Old shipdate (OUT)         */
                alloc$ = "P"   /* Prevents redoing unless told to */
                alloc  = min(newopenqty, alloc + alloc_adj)
                goto L30710
L30620:     /* Allocate Complete */
                alloc = newopenqty
                goto L30710
L30650:     /* No Allocation (or Preciously Allocated) */
            /* Difference is 'N' will have ALLOC = 0   */
                alloc = min(newopenqty, alloc)
                goto L30710
L30680:     /* Zero Allocation  */
                alloc = 0
                alloc$ = "N"
L30710:     /* Put allocation results back into NEWLINE      */
                put str(newline$()) using L30722, alloc, alloc$
L30722:             FMT POS(125), PD(14,4), POS(246), CH(1)
            return

L30740
*        Load up (or dummy up) Old Line Item
            str(oldline$()) = " "
            call "READ100" (#6, linekey$, oldline%)
            if oldline% = 0% then put #6 using L30810, 0, 0, 0, 1, 0, 0,  ~
                               " ", " ", " ", " ", " "
            get #6 using L30310, str(oldline$())
            get str(oldline$()) using L30810, oldorderqty, oldopenqty,    ~
                                   oldprice, conv_fact, unitprice,       ~
                                             olddiscpct, oldorigdue$,    ~
                                   oldreqdship$, oldlot$, olddemtype$,   ~
                                   oldest$
L30810:         FMT POS( 93), PD(14,4), XX(8)   , PD(14,4),              ~
                    POS(141), PD(14,4), POS(157), PD(14,7), PD(14,4),    ~
                                        POS(173), PD(14,4),              ~
                    POS(200), CH(6), POS(212), CH(6), CH(6),             ~
                    POS(240), CH(1), POS(255), CH(8)
            if conv_fact <> 0 then                                       ~
                oldprice = unitprice / conv_fact /* More accurate value */

            return

        usage_capture

*        If the MPS/PFM Usage Capture flag is turned on, then check to
*        see if the item is a planned item, if not don't update usage
*        files.  If an old lines record truely exists then check to see
*        if its previously captured usage should be removed so that the
*        usage of the buffer line can be added.

            call "READ100" (#13, part$, f1%(13%))
                if f1%(13%) <>  1% then return

*       Determine which dates to use and set them for 'requested' usage
            oldusagedate$ = oldreqdship$  :  newusagedate$ = reqdship$
            if usage_date$ <> "O" then L32160
                oldusagedate$ = oldorigdue$ : newusagedate$ = neworigdue$

L32160
*        Check if old line needs usage modified
            if oldline% = 0% or oldorderqty = 0 then usage_add
            if oldcrhold$ = "C" then usage_add
            if crhold$ = "C" then usage_remove
            if str(newline$(),57%,1%) = hex(ff) then usage_remove
/* VIQS001 */
            if str(newline$(),57,1) = hex(01) then usage_remove
                                                           /* (EWD010) */
            if str(adjrsn$,1%,1%) = "D" then goto usage_remove
            if str(adjrsn$,1%,1%) = "R" then goto usage_remove

            if oldorderqty <> neworderqty then usage_remove
            if oldusagedate$ <> newusagedate$ then usage_remove
            if usage_store$ = "Y" and oldstore$ <> newstore$ then        ~
                usage_remove
            if olddemtype$ <> newdemtype$ then usage_remove
            goto L32400

        usage_remove
            usageqty = -oldorderqty
            usagetype$ = "UNKN"
            if olddemtype$ = "1" then usagetype$ = "FCST"
            if olddemtype$ = "2" then usagetype$ = "NFCT"
            call "HNYUSESB" (so$, seq$, oldstore$, part$, "R",           ~
                             oldusagedate$, usagetype$, usageqty)

        usage_add
            if crhold$ = "C" then L32400
REM            if str(newline$(),57%,1%) = hex(ff) then L32400
/* VIQS001 */
REM            if str(newline$(),51%,1%) = hex(01) then L32400
                                                          /* (EWD010) */
            if str(adjrsn$,1%,1%) = "D" then goto L32400
            if str(adjrsn$,1%,1%) = "R" then goto L32400

            usagetype$ = "UNKN"
            if newdemtype$ = "1" then usagetype$ = "FCST"
            if newdemtype$ = "2" then usagetype$ = "NFCT"

            usageqty = neworderqty
            call "HNYUSESB" (so$, seq$, newstore$, part$, "R",           ~
                             newusagedate$, usagetype$, usageqty)

L32400:     return

        process_line_for_mlq        /* Multi-Line Quotation Line Items */
            return
            if mlqyorn$ <> "Y" then return/* Nothing to do? Do nothing */
            if mlquote_seq$ = " " then return  /* No Quote? Do nothing */
            mlq_line = newsanet + oldsanet /* Line Item net +/- change */
*        Begin the MLQ update process. First update the Cross-Ref file.
            if mlqxref$ <> "Y" then goto L32520/* No X-Ref? Next action */
                mlqtotal = 0
                readkey$ = str(mlquote_seq$,,8%) & str(cuscode$) & so$
                call "READ101" (#33, readkey$, f1%(33%))   /* MLQCROSS */
                if f1%(33%) <> 0% then get #33 using L32465, mlqtotal
L32465:              FMT POS(34), PD(14,4)
                mlqtotal = mlqtotal + mlq_line
                put #33 using L32490, str(mlquote_seq$,,8%), cuscode$,    ~
                     so$, mlqtotal                         /* MLQCROSS */
L32490:              FMT CH(8), CH(9), CH(16), PD(14,4)
                if f1%(33%) = 0% then write #33 else rewrite #33

L32520
*        Next, update the MLQMASTR file. This indicates 'Quote awarded'.
            readkey$ = str(mlquote_seq$,,8%)
            call "READ101" (#30, readkey$, f1%(30%))       /* MLQMASTR */
            if f1%(30%) = 0% then goto L32589 /* Not there? Next action */
                get #30 using L32550, mlqtotal  /* Get prev Award Total */
L32550:              FMT POS(1092), PD(14,4)
                mlqtotal = mlqtotal + mlq_line               /* Adjust */
                put #30 using L32565, cuscode$, so$, date, mlqtotal
L32565:              FMT POS(1061), CH(9), CH(16), CH(6), PD(14,4)
                rewrite #30                                /* MLQMASTR */

L32589
*        MLQ Line Item processing continues.
            return



        update_planning                  /* OPT% = 1% Add New S.O.     */
           err% = 0%                     /* OPT% = 8% Modify/Change S.O*/
           or_userid$ = userid$          /* OPT% = 9% Delete a S.O.    */
           or_status$ = "00"
           if crhold$ = "H" then or_status$ = "99" /* Cannot be Planned*/
           if crhold$ <> "H" and opt% <> 1% then or_status$ = "01"
           or_no$(1%) = or_cuscode$
           or_no$(2%) = or_so$
           or_no$(3%) = "99999999"
           or_no$(4%) = "99999999"
           if debug% = 0% then goto noPlanDebug
             call "SHOSTAT" ("APCPLN6B --> " & or_so$ )
             stop

noPlanDebug:

        call "APCPLN5B"  (opt%,          /* Option Code                */~
                          or_due$,       /* S.O. Due Date/Delivery Date*/~
                          or_no$(),      /* Customer Number            */~
                                         /* Sales Order Number         */~
                                         /* APC Inv. for S.O.          */~
                                         /* APC Chk Assoc. with S.O.   */~
                          or_po$,        /* Customer P.O. Number       */~
                          or_status$,    /* Curr S.O. Status(PLAN STAT)*/~
                          date,          /* Date Assoc. with Stat Chg. */~
                          or_sls$,       /* APC Salesman Code          */~
                          or_fob$,       /* Customer Delivery(PLAN DEL?*/~
                          or_hows$,      /* Secial Instr (PLAN HOWS)   */~
                          "99999",       /* Load Number Sched/Assigned */~
                          date,          /* Date S.O. Created          */~
                          date,          /* Date S.O. Last Changed     */~
                          or_userid$,    /* S.O. Last Modified By      */~
                          " ",           /* Date B.O.L. Created        */~
                          or_text$,      /* S.O. Header Text Id        */~
                          stk$(),        /* MFG PART NO. STOCK         */~
/*PAR000*/                stk_sub$(),    /* Stock Subpart numbers      */~
/*PAR000*/                stk_desc$(),   /* Stock Description          */~
                          qty$(),        /* MFG QUANTITY               */~
/*CR3186*/                prefix_so$,    /* New prefix of sales order  */~							  
                          schema%,       /* User Schema                */~
                          #35,           /* CUSTOMER - Master File     */~
                          #36,           /* APCPLNOR - APC Header File */~
                          #37,           /* APCPLNSC - APC Schedule    */~
                          #5,            /* BCKMASTR - S.O. HEADER     */~
                          #6,            /* BCKLINES - S.O. DETAIL     */~
                          #38,           /* GENCODES - TABLES          */~
                          #39,           /* APCPULLS - APC Pull Stock  */~
                          #3,            /* INVQUAN  - Inventory Qty   */~
                          #40,           /* APCPLNDP - Plan Dept Master*/~
                          #41,           /* APCPLNSD - Plan S.O. Sched */~
                          #4,            /* INVMASTR - PART MASTER     */~
                          #11,           /* APCPLNSA Daily Sales-EWD002*/~
                          #25,           /* EWDSCHED Spec Sched(EWD002)*/~
                          #63,           /* BCKSUBPT PAR000            */~
                          #56,           /* KANBANPT (AWD026)          */~
                          #57,           /* HLDSCHED Spec Sched(AWD027)*/~
                          err% )         /* 0% = Ok, <> 0% Error       */
            if err% <> 0% then gosub update_error_log                    ~
                          else gosub check_planning
        return

        update_error_log
            init(" ") err_rec$, err_key$, err_code$
            err_cnt% = 0%
            err_code$ = "Err = XX"
            convert err% to str(err_code$,7%,2%), pic(00)

            str(err_key$,1%,8%)  = date
            str(err_key$,9%,8%)  = time
            str(err_key$,17%,1%) = " "
            str(err_key$,18%,8%) = or_so$
            str(err_key$,26%,1%) = " "
            str(err_key$,27%,9%) = or_cuscode$
            str(err_key$,36%,2%) = "  "

L60850:     convert err_cnt% to str(err_key$,38%,2%), pic(##)

            read #43,hold,key = err_key$, eod goto L60910
               err_cnt% = err_cnt% + 1%
               goto L60850

L60910:     str(err_rec$,1%,39%)  = err_key$
            str(err_rec$,40%,6%)  = date
            str(err_rec$,46%,2%)  = "  "
            str(err_rec$,48%,3%)  = userid$
            str(err_rec$,51%,2%)  = "  "
            str(err_rec$,53%,16%) = or_po$
            str(err_rec$,69%,2%)  = "  "
            str(err_rec$,71%,8%)  = err_code$
            str(err_rec$,79%,2%)  = " "

            put #43, using L61020, err_rec$
L61020:       FMT CH(80)
            write #43, eod goto L61040
L61040: return

        check_planning
            read #36,key 4% = or_so$, eod goto L61120
            if opt% <> 9% then return
               err% = 99%
               gosub update_error_log
        return
L61120:     if opt% = 9% then return
            err% = 90%
            gosub update_error_log
        return

/* VIQS002 */
fax_ack:

    call "APCFAXSD" (or_cuscode$  , /* Customer Code  */    ~
                         or_so$       , /* Sales Order No */    ~
                         userid$      , /* User ID        */    ~
             error%       , /* Error Flag     */    ~
             #38          , /* Gencodes File  */    ~
             #05          , /* BCKMASTR File  */    ~
             #06          , /* BCKLINES File  */    ~
             #44          , /* TXTFILE  File  */    ~
             #35          , /* CUSTOMER File  */    ~
             #45          , /* ARMTERMS File  */    ~
             #04          , /* HNYMASTR File  */    ~
             #21)           /* BOMSPEC  File  */
    return
/* VIQS002 */
                                            /* (EWD) - End            */

                                                 /* (EWD005)          */
        build_category
            gosub check_howship
            init(" ") sales_acct$, disc_acct$, sales_acct_cr$, sls_acct_cr$
                                                    /* (EWD007) Set by  */
                                                    /* Howship          */
            gosub check_customer                    /* (AWD018)         */
            gosub check_credit                      /* (SR71501)        */
            if len(new_cat$) > 3 then goto Build_2  /* Special Category */
               init(" ") sls_acct$, dsc_acct$       /* (EWD007)         */

               new_cat$ = cat$                      /* Set to Line Item */
                                                    /* Analyize Category*/
            if len(part$) > 18 then goto Build_1
               new_cat$ = "PART"
  /* CR1837  CR2501 */               
               if brand$ = "19" or brand$ = "18" then new_cat$ = "PGPA"   
               if str(part$,5%,4%) = "WARR" then new_cat$ = "WARR"
/* (EWD009) Mod for new freight and drop shipment new models  */
               if str(part$,1%,4%) = "0052" then new_cat$ = str(part$,1%,4%)
               if str(part$,1%,4%) = "0062" then new_cat$ = str(part$,1%,4%)
               if str(part$,1%,4%) = "0202" then new_cat$ = str(part$,1%,4%)

               pp% = pos(part$ = "W")
               if pp% = 0% then goto Build_2
               if str(part$,pp%,4%) = "WARR" then new_cat$ = "WARR"
               goto Build_2
                                                    /* Only Valid MFG   */
Build_1:    pp% = pos( "456" = str(part$,11%,1%))   /* Check Sash's     */
            if pp% > 0% then new_cat$ = "PART"      /* Set Cat for Sash */
/* CR1837  CR2501 */  
            if pp% > 0% and (brand$ = "19" or brand$ = "18")   ~
                     then new_cat$ = "PGPA"   

            if len(new_cat$) > 3 then goto Build_2  /* Vaild Category   */
                                                    /* No Code Based    */
            str(new_cat$,1%,3%) = str(part$,1%,3%)/* on Model        */
            str(new_cat$,4%,1%) = "2"
                                                    /* File - CATEGORY  */
Build_2:    read #31,key = new_cat$, using Build_3, sales_acct$, disc_acct$,~
                         sales_acct_cr$, sls_acct_cr$, eod goto Build_4
Build_3:        FMT POS(35), CH(9), CH(9), XX(08), CH(9), CH(9)

/* (SR71501) */
            if credit% = 0% then return

              sales_acct$ = sales_acct_cr$
              disc_acct$  = sls_acct_cr$
/* (SR71501) - end */
        return
Build_4:    new_cat$    = "MISC"                     /* Default Category*/
            sales_acct$ = "3615-313"                 /* Code for Invalid*/
            disc_acct$  = "3615-313"
/* (SR71501) */
            if credit% = 0% then return
              sales_acct$ = "3615-312"
              disc_acct$  = "3615-312"
/* (SR71501) - end */
        return
/* + (SR71501) */
        check_credit
          if new_cat$ = "MISC" then return  /* (CR1781) */
          if credit% = 0% then return
          new_cat$ = str(part$,1%,4%)
          str(new_cat$,4%,1%) = "2"
        return
/* - (SR71501) */


        check_howship                            /* Affects Whole Order */
                                                 /* (EWD008) nothing for*/
                                                 /* 'UPS'               */
                                                 /* (EWD012) Some New   */
                                                 /* codes for Samp/Disp */
            init(" ") new_cat$
            if hows$ = "02" or hows$ = "04" or hows$ = "06" or       ~
               hows$ = "22" then new_cat$ = "SAMP"
            if hows$ = "03" or hows$ = "05" then new_cat$ = "DISP"
            if hows$ = "30" then new_cat$ = "SALV"
            if hows$ = "31" then new_cat$ = "SCRA"
                                                 /* (EWD007)           */
            if hows$ = "24" then new_cat$ = "PART"
            if hows$ = "34" then new_cat$ = "SAMP"
                                                 /* (EWD012)           */
            if hows$ = "38" or hows$ = "39" or hows$ = "40" then     ~
                                 new_cat$ = "SAMP"

            if len(new_cat$) < 4 then return
               init(" ") sls_acct$, dsc_acct$
                                                 /* (EWD007)           */
        return

        check_customer                            /* (AWD018) */

             if str(cuscode$,1,6) = "TH0050" then new_cat$ = "T050"
             if str(cuscode$,1,6) = "TH0051" then new_cat$ = "T051"
             if str(cuscode$,1,6) = "TH0052" then new_cat$ = "T052"
             if str(cuscode$,1,6) = "TH0053" then new_cat$ = "T053"
             if str(cuscode$,1,6) = "TH0054" then new_cat$ = "T054"
             if str(cuscode$,1,6) = "TH0055" then new_cat$ = "T055"
             if str(cuscode$,1,6) = "TH0056" then new_cat$ = "T056"
             if str(cuscode$,1,6) = "TH0057" then new_cat$ = "T057"
             if str(cuscode$,1,6) = "TH0058" then new_cat$ = "T058"
             if str(cuscode$,1,6) = "TH0059" then new_cat$ = "T059"
             if str(cuscode$,1,6) = "TH0061" then new_cat$ = "T061"
             if str(cuscode$,1,6) = "TH0062" then new_cat$ = "T062"
             if str(cuscode$,1,6) = "TH0063" then new_cat$ = "T063"
             if str(cuscode$,1,6) = "TH0064" then new_cat$ = "T064"
             if str(cuscode$,1,6) = "TH0065" then new_cat$ = "T065"
             if str(cuscode$,1,6) = "TH0066" then new_cat$ = "T066"
             if str(cuscode$,1,6) = "TH0067" then new_cat$ = "T067"
             if str(cuscode$,1,6) = "TH0068" then new_cat$ = "T068"
             if str(cuscode$,1,6) = "TH0069" then new_cat$ = "T069"
             if str(cuscode$,1,6) = "TH0070" then new_cat$ = "T070"
             if str(cuscode$,1,6) = "TH0071" then new_cat$ = "T071"
             if str(cuscode$,1,6) = "TH0072" then new_cat$ = "T072"

             if str(cuscode$,1,6) = "AT0200" then new_cat$ = "A200"
             if str(cuscode$,1,6) = "AT0201" then new_cat$ = "A201"
             if str(cuscode$,1,6) = "AT0202" then new_cat$ = "A202"
             if str(cuscode$,1,6) = "AT0203" then new_cat$ = "A203"
             if str(cuscode$,1,6) = "AT0204" then new_cat$ = "A204"
             if str(cuscode$,1,6) = "AT0205" then new_cat$ = "A205"
             if str(cuscode$,1,6) = "AT0206" then new_cat$ = "A206"

             if str(cuscode$,1,6) = "AT0212" then new_cat$ = "A201"
             if str(cuscode$,1,6) = "AT0213" then new_cat$ = "A207"
             if str(cuscode$,1,6) = "AT0215" then new_cat$ = "A201"
             if str(cuscode$,1,6) = "AT0216" then new_cat$ = "A207"
             if str(cuscode$,1,6) = "AT0301" then new_cat$ = "A201"
             if str(cuscode$,1,6) = "AT0201" then new_cat$ = "A201"
             if str(cuscode$,1,6) = "AT4201" then new_cat$ = "A201"

REM             if str(cuscode$,1,6) = "DA0500" then new_cat$ = "D500"

             if str(cuscode$,1,6) = "KI0150" then new_cat$ = "K150"
             if str(cuscode$,1,6) = "KI0151" then new_cat$ = "K151"


        return
                                                     /* (AWD018) */

        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


                                                 /* (EWD005) - End     */

        oracle_connect
            init(" ") user$, pass$, server$
            gosub get_user_pswd                /* (AWD028) */
            oci_err% = 0%
            call "CONNECT" (user$, pass$, server$, oci_err%)
        return

        oracle_query
            oci_err% = 0%
            call "QUERY" (stmt1$, stmt2$, oci_err%)
        return

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_exec
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

        oracle_fetch
            oci_err% = 0%
            no_fields% = 0%
            call "FETCH" (no_fields%, oci_err%)
        return

        oracle_getfield
            oci_err% = 0%
            field_len% = 0%
            init(" ") field$, name$

            call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)

            if field_num% <> 5% then return

REM IF STR(FIELD$,1%,8%) = "BA111000" THEN FIELD$ = "BA111 000"
REM IF STR(FIELD$,1%,8%) = "BB111000" THEN FIELD$ = "BB111 000"
REM IF STR(FIELD$,1%,5%) = "BA111" THEN FIELD$ = STR(FIELD$,1%,5%) & " " & STR(FIELD$,6%,3%)
REM IF STR(FIELD$,1%,5%) = "BB111" THEN FIELD$ = STR(FIELD$,1%,5%) & " " & STR(FIELD$,6%,3%)
        return


        load_newheader
           credit% = 0%
           gross_amt = 0.00
           init(" ") field$, hdr_text$
           field_num% = 2%
           gosub oracle_getfield
              bufferkey$ = userid$ & field$
           field_num% = 3%
           gosub oracle_getfield
               print$ = field$
               pos% = 1%

REM ---- Fields --------------------  (CR1830) field documentation
REM ---- field_num% = field placement in oe_bckbuffr & ne_bckbuffr -----------
REM  cuscode, so-no, po-no, ship-to-addr(01), ship-to-addr(02), ship-to-addr(03)
REM  ship-to-addr(04), ship-to-addr(05), ship-to-addr(06), terms, howship, fob,
REM  ship_instruction(01), ship_instruction(02), salesacct, discacct, slsmcode(01),
REM  slsmcode(02), slsmcode(03)
           for field_num% = 5% to 29%
               gosub oracle_getfield
               str(newhdr$(),pos%,field_len%) = field$
                pos% = pos% + field_len%
           next field_num%

REM ---- Fields --------------------
REM  commission_split(01), commission_split(02), commission_split(03)
           for field_num% = 30% to 32%
               gosub oracle_getfield
               comm% = 0%
               convert str(field$,1%,len(field$)) to comm%, data goto next_comm
               str(newhdr$(),pos%,1%) = BIN(comm%,1)
                 pos% = pos% + 1%
next_comm
           next field_num%

REM ---- Fields --------------------
REM  regcode, variable_fields(01), variable_fields(02),
REM  variable_fields(03), variable_fields(04), variable_fields(05),
REM  variable_fields(06), variable_fields(07), variable_fields(08),
REM  variable_fields(09), variable_fields(10)
           for field_num% = 33% to 43%
               gosub oracle_getfield
               str(newhdr$(),pos%,field_len%) = field$
                pos% = pos% + field_len%
           next field_num%

REM ---- Fields --------------------
REM  textid
           field_num% = 44%
           gosub oracle_getfield
           text_id% = 0%
           str(hdr_text$,1%,10) = str(field$,1%,len(field$))
           convert str(field$,1%,len(field$)) to text_id%, data goto next_text
           str(newhdr$(),pos%,4%) = BIN(text_id%,4)
next_text
            pos% = pos% + 4%

REM ---- Fields --------------------
REM  storeid
           field_num% = 45%
           gosub oracle_getfield
           str(newhdr$(),803%,3%) = field$
            pos% = pos% + 3%

REM ---- Fields --------------------
REM  order_date, cancel_date, due_date, ship_date, date_entered
           for field_num% = 46% to 50%
               gosub oracle_getfield
               call "DATUFMTC" (str(field$,1%,10%))
               str(newhdr$(),pos%,6%) = str(field$,1%,6%)
                pos% = pos% + 6%
           next field_num%

REM ---- Fields --------------------
REM  userid
           field_num% = 51%
           gosub oracle_getfield
           str(newhdr$(),pos%,3%) = field$
            pos% = pos% + 3%

REM ---- Fields --------------------
REM  date_changed
           field_num% = 52%
           gosub oracle_getfield
           call "DATUFMTC" (str(field$,1%,10%))
           str(newhdr$(),pos%,6%) = str(field$,1%,6%)
            pos% = pos% + 6%

REM ---- Fields --------------------
REM  userid_of_last_mod, adj_reason, export, price_code
           for field_num% = 53% to 56%
               gosub oracle_getfield
               str(newhdr$(),pos%,field_len%) = field$
                pos% = pos% + field_len%
           next field_num%

REM ---- Fields --------------------
REM  discpercent, gross_open_amt
           for field_num% = 57% to 58%
               gosub oracle_getfield
               amount = 0.00
               convert str(field$,1%,len(field$)) to amount,    ~
                                             data goto next_amount
               put str(newhdr$(),pos%,8%) using L62050, amount

L62050:         FMT PD(15,4)

                pos% = pos% + 8%
next_amount
            next field_num%

REM ---- Fields --------------------
REM  creditholdflag
           field_num% = 59%
           gosub oracle_getfield
           str(newhdr$(),pos%,1%) = field$
           pos% = pos% + 1%

REM ---- Fields --------------------
REM  seqnr
           field_num% = 60%
           gosub oracle_getfield
           seqnr% = 0%
           convert str(field$,1%,len(field$)) to seqnr%, data goto next_seqnr
           str(newhdr$(),pos%,2%) = BIN(seqnr%,2)
next_seqnr
            pos% = pos% + 2%

REM ---- Fields --------------------
REM  nextbol
           field_num% = 61%
           gosub oracle_getfield
           bol% = 0%
           convert str(field$,1%,len(field$)) to bol%, data goto next_bol
           str(newhdr$(),pos%,4%) = BIN(bol%,4)
next_bol
            pos% = pos% + 4%

REM ---- Fields --------------------
REM  cus-type, acct-xref, currentcy_code
           for field_num% = 62% to 64%
               gosub oracle_getfield
               str(newhdr$(),pos%,field_len%) = field$
                pos% = pos% + field_len%
           next field_num%

REM Add 3 blank spaces for last 3 fields in BCKMASTR
REM how_field, pmetal_so_flag, pmetal_inv_flag
           pos% = pos% + 3%      /* (AWD020) */

REM ---- Fields --------------------
REM  esc - energy surcharge
           field_num% = 65%
           gosub oracle_getfield
           esc = 0.00
           convert str(field$,1%,len(field$)) to esc, data goto next_esc
           put str(newhdr$(),pos%,8%) using L62050, esc

           pos% = pos% + 8%
next_esc


/* (AWD023) begin */
REM ---- Fields --------------------
REM  credit_percent
           pos% = 911%               /* (CR1830) reset pos%, moved ship_id */
           field_num% = 66%
           gosub oracle_getfield
           credit_percent = 0.00
           convert str(field$,1%,len(field$)) to credit_percent,   ~
                                               data goto next_credit

           put str(newhdr$(),pos%,8%) using L62050, credit_percent

           credit_percent = credit_percent / 100

           if credit_percent > 0.00 then credit% = 1%

           pos% = pos% + 8%
next_credit

/*(AWD025) */
REM ---- Fields --------------------
REM  NEWGBW
           field_num% = 67%
           gosub oracle_getfield
           str(newhdr$(),pos%,1%) = field$
           pos% = pos% + 1%

REM IF CREDIT% = 1% THEN PUT STR(NEWHDR$(),846,8) USING L62050, 0.00
/* (CR1830)  begin */
/* (AWD021) - begin */
REM  ship_to
REM  STR(NEWHDR$(),POS%,3%) = STR(NEWHDR$(),7%,3%)
REM  CMG$ = STR(NEWHDR$(),POS%,3%)
REM  POS% = POS% + 3%
REM  should not be needed anylonger
REM  STR(NEWHDR$(),7%,3%) = "   "

REM ---- Fields --------------------
REM  PlyGem PO pos% = 920%
           field_num% = 68%
           gosub oracle_getfield
           plygem_po$ = field$
           str(newhdr$(),pos%,field_len%) = field$
           pos% = pos% + 20%

REM ---- Fields --------------------
REM  PlyGem Quote Number pos% = 940%
           field_num% = 69%
           gosub oracle_getfield
           plygem_quote_id$ = field$
           str(newhdr$(),pos%,field_len%) = field$
           pos% = pos% + 20%
           
REM ---- Fields --------------------
REM  Order shipid 
REM    do not add to pos% - data & pos% manually set
           field_num% = 70%
           gosub oracle_getfield
           ship_id$ = "00" & field$
REM STR(NEWHDR$(),908%,3%) = STR(SHIP_ID$,2%,3%) /* !! ONLY 3 NEED TO BE 4 */
           str(newhdr$(),908%,3%) = str(ship_id$,4%,3%) /* !! only 3 need to be 4 */
/* (AWD021) - end   */
           
REM ---- Fields --------------------
REM  Tax Rate, HDMET, & HDIDX  POS% = 960%
        cuscode$ = str(newhdr$(),01%,09%)
        gosub lookupBillTo                                 /* (CR2068) */
        for field_num% = 71% to 73%
           gosub oracle_getfield
           amount = 0.00
           convert str(field$,1%,len(field$)) to amount, data goto next_amount1
           put str(newhdr$(),pos%,8%) using L62050, amount
           
next_amount1:                              /*+ (CR2068) check for THD DISC */
           goto notTHD /* need to test credit and no charge */
           if str(billto$,01%,06%) <> "341000" then goto notTHD
              if field_num% = 71% then goto notTHD    /* Tax Column */
                 if amount > 0 then goto notTHD
                   if field_num% = 72% then amount = 0.005  /* SOS HDMET */
                   if field_num% = 73% then amount = 0.0045 /* SOS HDIDX */
                   put str(newhdr$(),pos%,8%) using L62050, amount
notTHD:                                    /*- (CR2068) check for THD DISC */
           pos% = pos% + 8%
        next field_num%                     
        
        str(newhdr$(),984%,6%) = ship_id$            /* !! 6 ship_to$          */
        pos% = pos% + 6%
        
        
/* (CR2825) */
        field_num% = 76%
        gosub oracle_getfield
        qlty_dept_flag$ = field$  
        pos% = pos% + 1%
        str(newhdr$(),990%,1%) = qlty_dept_flag$
                  
REM ---- Fields --------------------
REM  Filler Area           
                                            /* 11% is filler area  */
                                            /* 10% is filler area  CR2825 */                                            

           str(newhdr$(),pos%,1000%-pos%) = " "
           pos% = pos% + (1000%-pos%)

/* (AWD023) end */
/* (CR1830) end */
/* (CR2068)     */
           field_num% = 74%
           gosub oracle_getfield
           nameorderedby$ = field$
           field_num% = 75%
           gosub oracle_getfield
           plygem_contact$ = field$

/* (CR2068)    */

/* + (CR3186) */
            init(" ") prefix_so$
	        prefix_so$ = "0"
		    field_num% = 77%
		    gosub oracle_getfield
		    prefix_so$ = str(field$,1%,1%)

/* - (CR3186) */
/* + CR3305  Customer Information */
            init(" ") distordernbr$, customerpo$, distcustnbr$
            
		    field_num% = 78%
		    gosub oracle_getfield
            convert str(field$,1%,len(field$)) to distordernbr%, ~
                                      data goto bad_distorder                                     
bad_distorder:
            if distcustnbr% = 0% then distordernbr$ = " "  ~
              else convert distordernbr% to distordernbr$, pic(#######0)
            
		    field_num% = 79%
		    gosub oracle_getfield
            customerpo$ = str(field$,1%,22%)
            
		    field_num% = 80%
		    gosub oracle_getfield
            convert str(field$,1%,len(field$)) to distcustnbr%, ~
                                      data goto bad_distcustnbr                                    
bad_distcustnbr:  
            if distcustnbr% = 0% then distcustnbr$ = " " ~                                
              else convert distcustnbr% to distcustnbr$, pic(###########0)
            
/* - CR3305 */
        return                    : REM END load_newheader


        build_query
            init(" ") stmt1$, stmt2$
            stmt1$ = "SELECT * FROM OE_BCKBUF2 WHERE SALES_ORDER_NUMBER = '"
            stmt1$ = stmt1$ & so$ & "' ORDER BY SALES_ORDER_NUMBER, SEQUENCE_NUMBER"

            if schema% = 2% then str(stmt1$,15%,2%) = "NE"
            if schema% = 5% then str(stmt1$,15%,2%) = "NE"

            gosub oracle_flush
            gosub oracle_query
        return

        load_newline
        init(" ") field$, lne_text$, series$, style$, stcrating$
        pos% = 1%
        for field_num% = 1% to 7%
            gosub oracle_getfield
            if field_num% = 5% then part$ = field$
            str(newline$(),pos%,field_len%) = field$
            pos% = pos% + field_len%
        next field_num%
        for field_num% = 8% to 14%
            gosub oracle_getfield
            lne_amount = 0.00
            convert str(field$,1%,len(field$)) to lne_amount, ~
                                          data goto next_lne_qty

            if field_num% = 14% and credit% = 1%  ~
                         then gosub get_lne_amount

            put str(newline$(),pos%,8%) using L62050, lne_amount

        next_lne_qty
        pos% = pos% + 8%
        next field_num%
        for field_num% = 15% to 16%
            gosub oracle_getfield
            str(newline$(),pos%,field_len%) = field$
            pos% = pos% + field_len%
        next field_num%

REM ---- Fields --------------------
REM  conversion_factor
        field_num% = 17%
        gosub oracle_getfield
        lne_amount = 0.00
        convert str(field$,1%,len(field$)) to lne_amount, ~
                                           data goto next_fact
        put str(newline$(),pos%,8%) using L62080, lne_amount

L62080:         FMT PD(15,7)

next_fact:
        pos% = pos% + 8%

REM ---- Fields --------------------
REM  Unit_Price & Line_Item_Discount_Pct
        for field_num% = 18% to 19%
           gosub oracle_getfield
           lne_amount = 0.00
           convert str(field$,1%,len(field$)) to lne_amount, ~
                                         data goto next_lne_prc

           if field_num% = 18% and credit% = 1%       ~
                              then gosub get_lne_amount

           put str(newline$(),pos%,8%) using L62050, lne_amount

        next_lne_prc
        pos% = pos% + 8%
        next field_num%

REM ---- Fields --------------------
REM  Taxable, Sales_Account, Discount_Account
        for field_num% = 20% to 22%
            gosub oracle_getfield
            if str(part$,1%,3%) = "020" and field_num% = 20% then field$ = "N"
            str(newline$(),pos%,field_len%) = field$
            pos% = pos% + field_len%
        next field_num%

REM ---- Fields --------------------
REM  due date, due date, shipment date
        for field_num% = 23% to 25%
            gosub oracle_getfield
            call "DATUFMTC" (str(field$,1%,10%))
            str(newline$(),pos%,6%) = str(field$,1%,6%)
            pos% = pos% + 6%
        next field_num%

REM ---- Fields --------------------
REM  lot, project, filler, demand, priority
        for field_num% = 26% to 30%
            gosub oracle_getfield
            str(newline$(),pos%,field_len%) = field$
            pos% = pos% + field_len%
        next field_num%

REM ---- Fields --------------------
REM  textid
        field_num% = 31%
        gosub oracle_getfield
            text_lne% = 0%
            str(lne_text$,1%,10) = str(field$,1%,len(field$))
            convert str(field$,1%,len(field$)) to text_lne%, data goto next_lne
            str(newline$(),pos%,4%) = BIN(text_lne%,4)
        next_lne
        pos% = pos% + 4%

REM ---- Fields --------------------
REM  allocation flag, inv number, estimate id, bom_id,
REM  quote_number, quote_seq_number, ship_code
        for field_num% = 32% to 38%
            gosub oracle_getfield
            str(newline$(),pos%,field_len%) = field$
            pos% = pos% + field_len%
        next field_num%

REM ---- Fields --------------------
REM  pmetal_adjusted
        field_num% = 39%
        gosub oracle_getfield
            metal% = 0%
            convert str(field$,1%,len(field$)) to metal%, data goto next_metal
            str(newhdr$(),pos%,1%) = BIN(metal%,1)
        next_metal
        pos% = pos% + 1%

REM ---- Fields --------------------
REM  group code, config_code, private_label, config_line, screen_type
        for field_num% = 40% to 44%
            gosub oracle_getfield
            if field_num% = 43% then gosub format_configln
            str(newline$(),pos%,field_len%) = field$
            pos% = pos% + field_len%
        next field_num%

/* (AWD019) - beg */

/* (AWD016) - BEG */
REM FIELD_NUM% = 45%         /* BCKLINES/BCKBUF2 FILLER */
REM GOSUB ORACLE_GETFIELD
REM STR(NEWLINE$(),POS%,FIELD_LEN%) = FIELD$
REM POS% = POS% + 12%            /* FILLER SIZE */

REM ---- Fields --------------------
REM  Special Mulled
        for field_num% = 45% to 45%
            gosub oracle_getfield
            str(newline$(),pos%,field_len%) = field$
            pos% = pos% + field_len%
        next field_num%

REM Skip 46 or field$ = filler_2
/* (AWD019) - end */


        linekey$ = str(newline$(),10%,19%)

        gosub get_bcksubpt_fields

REM pos still set for sku number; this is field number 45 in bcklines

REM ---- Fields --------------------
REM  field 51 -> Sku Number
        for field_num% = 51% to 51%
            gosub oracle_getfield
            str(newline$(),pos%,field_len%) = field$
            pos% = pos% + field_len%
        next field_num%


REM pos% = pos% + 2%     /* BCKLINES/BCKBUF2 Filler */
        pos% = pos% + 1%   /* BCKLINES/BCKBUF2 Filler PLYGEM Filler now CH(01)*/



REM only newpos below
REM newpos is used to put the mulled row and column index
REM in group and config code instead of putting at end
REM only 11 characters left before the sku number of 9

REM ---- Fields --------------------
REM  mulledunit_rowindex, mulledunit_columnindex
        newpos% = 279%
        for field_num% = 49% to 50%
            gosub oracle_getfield

            index% = 0%
            convert str(field$,1%,len(field$)) to index%, ~
                                          data goto bad_index

            convert index% to str(newline$(),newpos%,1%),pic(0)

bad_index:

            newpos% = newpos% + 2%
        next field_num%


REM newpos is used to put the itemID and unitID
REM in estimate, bom-id, quote-no and seqnr
REM instead of putting at end

REM ---- Fields --------------------
REM  ItemID, UnitID
        newpos% = 255%
        for field_num% = 52% to 53%
            gosub oracle_getfield
            ww_number% = 0%
            convert str(field$,1%,10%) to ww_number%, ~
                                          data goto next_ww_number


            convert ww_number% to str(newline$(),newpos%,10%),pic(0000000000)


next_ww_number:
            newpos% = newpos% + 10%
        next field_num%

REM ---- Fields --------------------
REM  Dim1es
/* (AWD030) */
        dim1es, dim2es = 0.00
        field_num% = 54%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to dim1es, ~
                                      data goto bad_dim1
bad_dim1:

REM ---- Fields --------------------
REM  Dim2es
        newpos% = newpos% + 10%
        field_num% = 55%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to dim2es, ~
                                     data goto bad_dim2
bad_dim2:
        newpos% = newpos% + 10%
/* (\AWD030) */

/*(AWD031) */
REM ---- Fields --------------------
REM  Dim3es
        field_num% = 56%
        gosub oracle_getfield
        series$ = field$

REM ---- Fields --------------------
REM  Style
        newpos% = newpos% + 16%
        field_num% = 57%
        gosub oracle_getfield
        style$ = field$
        newpos% = newpos% + 10%
/*(\AWD031) */
/* (AWD039) */

REM ---- Fields --------------------
REM  STCRating
        field_num% = 58%
        gosub oracle_getfield
        stcrating$ = field$
        newpos% = newpos% + 3%
/*(\AWD039) */

/* + (SR66200) */
        init(" ") oraclePart$, l1_desc$, l2_desc$(), l1_mutype$, l3_mulltype$
        init(" ") plygem_dprating$, plygem_cpd$, plygem_prd_series$,         ~
                  plygem_perflbl$, plygem_buyout$, plygem_thd_lne$,/*(CR1830)*/~
                  plygem_tdi$, plygem_fld$, nameorderedby$, plygem_contact$, ~
                  L2_widthns$, L2_heightns$, L2_unit_size_code$,             ~
                  L1_unit_size_code$
/* (CR1885) */

        widthes, heightes = 0.00
        ufactor, shgc, vistrans = 0.00            /* (CR1830) */
        linetypeid% = 0%
        buyout%, thd_line% = 0%                   /* (CR1830) */

REM ---- Fields --------------------
REM  Oracle Part
        field_num% = 59%
        gosub oracle_getfield
        oraclePart$ = field$

REM ---- Fields --------------------
REM  WidthEs
        field_num% = 60%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to widthes, ~
                                      data goto bad_widthes

bad_widthes:

REM ---- Fields --------------------
REM  HeightEs
        field_num% = 61%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to heightes, ~
                                      data goto bad_heightes

bad_heightes:

REM ---- Fields --------------------
REM  L1_desc
        field_num% = 62%
        gosub oracle_getfield
        l1_desc$ = field$

REM ---- Fields --------------------
REM  l2_desc
        field_num% = 63%
        gosub oracle_getfield
        str(l2_desc$(),1%,250%) = field$

REM ---- Fields --------------------
REM  L2_desc1
        field_num% = 64%
        gosub oracle_getfield
        str(l2_desc$(),251%,250%) = field$

REM ---- Fields --------------------
REM  l1_linetypeid
        field_num% = 65%
        gosub oracle_getfield

        convert str(field$,1%,len(field$)) to linetypeid%, ~
                                          data goto bad_linetypeid
bad_linetypeid:

REM ---- Fields --------------------
REM  l1_mutype
        field_num% = 66%
        gosub oracle_getfield
        l1_mutype$ = field$

REM ---- Fields --------------------
REM  l3_mulltype
        field_num% = 67%
        gosub oracle_getfield
        l3_mulltype$ = field$

/* - (SR66200) */

/* (SR67154) Northern NFRC Required*/

REM ---- Fields --------------------
REM  northern nfrc
        northern%  = 0%
        field_num% = 68%
        newpos%    = 275%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to northern%, data goto next_northern

next_northern:

        str(newline$(),newpos%,1%) = BIN(northern%,1)
/* (\SR67154) */
/* (CR1830) - begin */
REM ---- Fields --------------------
REM  PlyGem DP Rating
        field_num% = 69%
        gosub oracle_getfield
        plygem_dprating$ = field$

REM ---- Fields --------------------
REM  PlyGem UFactor
        field_num% = 70%
        gosub oracle_getfield
        ufactor = 0.00
        convert str(field$,1%,len(field$)) to ufactor, data goto badUFactor

badUFactor:

REM ---- Fields --------------------
REM  PlyGem SHGC
        field_num% = 71%
        gosub oracle_getfield
        shgc = 0.00
        convert str(field$,1%,len(field$)) to shgc, data goto badSHGC

badSHGC:

REM ---- Fields --------------------
REM  PlyGem Vistrans
        field_num% = 72%
        gosub oracle_getfield
        vistrans = 0.00
        convert str(field$,1%,len(field$)) to vistrans, data goto badVistrans

badVistrans:

REM ---- Fields --------------------
REM  PlyGem NFRC CPD
        field_num% = 73%
        gosub oracle_getfield
        plygem_cpd$ = field$


REM ---- Fields --------------------
REM  PlyGem Production Series
        field_num% = 74%
        gosub oracle_getfield
        plygem_prd_series$ = field$


REM ---- Fields --------------------
REM  PlyGem Performance Label
        field_num% = 75%
        gosub oracle_getfield
        plygem_perflbl$ = field$

REM ---- Fields --------------------
REM  PlyGem Buy Out
        field_num% = 76%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to buyout%, data goto badBuyOut

badBuyOut:
        convert buyout% to plygem_buyout$, pic(00)

        plygem_buyout$ = field$

REM ---- Fields --------------------
REM  PlyGem THD Line
        field_num% = 77%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to thd_line%, data goto badTHDLne

badTHDLne:
        convert thd_line% to plygem_thd_lne$, pic(##0)
/* (CR1830) - end */
/* (CR1885) - beg */
REM ---- Fields --------------------
REM  PlyGem TDI Number
        field_num% = 78%
        gosub oracle_getfield
        plygem_tdi$ = field$

REM ---- Fields --------------------
REM  PlyGem Florida Approval Code
        field_num% = 79%
        init(" ") plygem_fld$, field$ 
        gosub oracle_getfield
        PLYGEM_FLD$ = FIELD$               /* Active CR2090 */
/* (CR1885) - end */

/* CR2090  + */
        field_num% = 80%
        gosub oracle_getfield
        L2_widthns$ = field$
        
        field_num% = 81%
        gosub oracle_getfield
        L2_heightns$ = field$
        
        field_num% = 82%
        gosub oracle_getfield
        L2_unit_size_code$ = field$    /* ES - Exact or NS - Nonimal */
        
/* CR2090  - */
/* + (CR2114) */
        l1widthes, l1heightes = 0.00       
REM ---- Fields --------------------
REM  L1 WidthEs
        field_num% = 83%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to l1widthes, ~
                                      data goto bad_l1widthes

bad_l1widthes:

REM ---- Fields --------------------
REM  L1 HeightEs
        field_num% = 84%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to l1heightes, ~
                                      data goto bad_l1heightes

bad_l1heightes:

REM ---- Fields --------------------
REM  L1 size code
        field_num% = 85%
        gosub oracle_getfield
        L1_unit_size_code$ = str(field$,1%,2%)    /* ES, NS, OS */
/* - (CR2114) */

/* + (CR2264) */
REM Screen Ship Separately FLAG
        init(" ") scrnshpsep$
        scrnshpsep% = 0%
        field_num% = 86%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to scrnshpsep%, ~
                                      data goto bad_scrnshpsep
                                      
bad_scrnshpsep:                                      
        convert scrnshpsep% to scrnshpsep$, pic(0)

/* - (CR2264) */

/* + (CR3305) */
REM Screen Shipping Fields for customer
        init(" ") shipseptype$, shiplinenbr$, shipbarcode$
        shipseptype% = 0%
        field_num% = 87%
        gosub oracle_getfield
        convert str(field$,1%,len(field$)) to shipseptype%, ~
                                      data goto bad_shipseptype                                     
bad_shipseptype:                                      
        convert shipseptype% to shipseptype$, pic(##0)

        field_num% = 88%
        gosub oracle_getfield
        shiplinenbr$ = str(field$,1%,3%)        
        
        field_num% = 89%
        gosub oracle_getfield
        shipbarcode$ = str(field$,1%,18%)
        
/* - (CR3305) */

        return                    : REM END load_newline

/* (SR66200) */
        format_configln
          configln% = 0%
          convert field$ to configln%, data goto badConfigLn

badConfigLn:

          convert configln% to field$, pic(##0)
          config_ln$ = field$
        return
/* (SR66200) */

        get_lne_amount
REM call "SHOSTAT" ("CREDIT PERCENTAGE " ) stop

             lne_amount = (lne_amount * credit_percent)
             gross_amt = gross_amt + lne_amount
        return



        get_bcksubpt_fields
           init(" ") subpart$, infopart$
           sub_pos% = 1%
           info_pos% = 1%

REM ---- Fields --------------------
REM  extended part, info_part
           for field_num% = 47% to 48%
                gosub oracle_getfield
                if field_num% = 47% then                    ~
                   str(subpart$,sub_pos%, field_len%) = field$
                if field_num% = 47% then                    ~
                sub_pos%  = sub_pos% + field_len%



                if field_num% = 48% then                    ~
                   str(infopart$,info_pos%, field_len%) = field$
                if field_num% = 48% then                    ~
                info_pos% = info_pos% + field_len%
           next field_num%



           p% = pos(subpart$ = " ")
           if p% = 0% then return
           str(subpart$,p%,(21%-p%)) = "000000000000000"

           p% = pos(infopart$ = " ")
           if p% = 0% then return
           str(infopart$,p%,(21%-p%)) = "000000000000000"

        return

        set_bcksubpt
            str(bcksubpt_rec$,1%,8%) = so$           /* Sales Order No    */
            str(bcksubpt_rec$,9%,3%) = seq$          /* Sequence Number   */
            str(bcksubpt_rec$,12%,8%) = "        "   /* No Invoice No Yet */
            str(bcksubpt_rec$,20%,3%) = "   "        /* Invoice Item      */
            str(bcksubpt_rec$,23%,25%) = part$       /* Part Number       */
            str(bcksubpt_rec$,48%,20%) = subpart$    /* Sub Part Items    */
            str(bcksubpt_rec$,68%,64%) = part_desc$  /* Part Description  */
/*(AWD022) */                                        /* Information Part  */
            str(bcksubpt_rec$,132%,7%) = str(infopart$,1,7)
            str(bcksubpt_rec$,139%,2%) = brand$      /* Brand Information */
/*(AWD019)*/
/*(AWD022)*/                                          /* End of Infopart   */
            str(bcksubpt_rec$,141%,11%) = str(infopart$,10,11)
/*(AWD019)*/
                                                     /* Where to Mull     */
            str(bcksubpt_rec$,152%,1%)   = str(newline$(),289%,1%)
/* (AWD030) */
            put str(bcksubpt_rec$,153%,8%) using L62050, dim1es

            put str(bcksubpt_rec$,161%,8%) using L62050, dim2es
/* (AWD031)   */
            str(bcksubpt_rec$,169%,16%) = series$
            str(bcksubpt_rec$,185%,10%) = style$
            str(bcksubpt_rec$,195%,03%) = stcrating$
/* (\AWD031)   */
            str(bcksubpt_rec$,198%,01%) = plygem_lne$     /* (CR1933) */
            str(bcksubpt_rec$,199%,02%) = plygem_buyout$   /* PlyGem Buyout */ 
/* CR2090 */
            str(bcksubpt_rec$,201%,10%) = L2_widthns$  /* PG Nominal Width*/       
            str(bcksubpt_rec$,211%,10%) = L2_heightns$ /* PG Nominal Heigh*/       
            str(bcksubpt_rec$,221%,02%) = L2_unit_size_code$ /* l2 size code*/  
/* (CR2114) */
            put str(bcksubpt_rec$,223%,8%) using L62050, l1widthes

            put str(bcksubpt_rec$,231%,8%) using L62050, l1heightes
            
            str(bcksubpt_rec$,239%,02%) = L1_unit_size_code$ /* l1 size code*/  
        return

/* (AWD016) - END  */

/* (SR66200) */
        set_oradescr
           str(oradescr_rec$(),1%,8%)     = so$          /* Sales Order No    */
           str(oradescr_rec$(),9%,3%)     = seq$         /* Sequence Number   */
           str(oradescr_rec$(),12%,3%)    = config_ln$   /* WW Line Number    */
           str(oradescr_rec$(),15%,40%)   = oraclePart$  /* WW Oracle Part    */

           put str(oradescr_rec$(),55%,8%) using L62050, widthes
           put str(oradescr_rec$(),63%,8%) using L62050, heightes
/* (SR69995) */
           str(oradescr_rec$(),71%,250%)  = l1_desc$     /* L1_ITEM Descr     */
           str(oradescr_rec$(),321%,500%) = l2_desc$()   /* L2_UNIT Descr     */
           str(oradescr_rec$(),821%,2%)   = BIN(linetypeid%,2) /*L1_LineTypeID*/
           str(oradescr_rec$(),823%,50%)  = l1_mutype$   /* L1_MUTYPE         */
           str(oradescr_rec$(),873%,25%)  = l3_mulltype$ /* L3_MULLTYPE       */
/* (SR69995\) */
/* (CR1830) begin */
           str(oradescr_rec$(),898%,10%)  = plygem_dprating$  /* PlyGem Dp Rat*/
           put str(oradescr_rec$(),908%,8%) using L62050, ufactor
           put str(oradescr_rec$(),916%,8%) using L62050, shgc
           put str(oradescr_rec$(),924%,8%) using L62050, vistrans
           str(oradescr_rec$(),932%,30%)  = plygem_cpd$      /* PlyGem CPD    */
           str(oradescr_rec$(),962%,25%)  = plygem_prd_series$/*PlyGem PrdSeri*/
           str(oradescr_rec$(),987%,04%)  = plygem_perflbl$  /* PlyGem PerfLbl*/
           str(oradescr_rec$(),991%,02%)  = plygem_buyout$   /* PlyGem Buyout */
           str(oradescr_rec$(),993%,03%)  = plygem_thd_lne$  /* PlyGem THD Lne*/
           str(oradescr_rec$(),996%,08%)  = plygem_tdi$      /* PlyGem TDI Num*/
           str(oradescr_rec$(),1004%,12%) = plygem_fld$      /* PlyGem FL Appr*/
           str(oradescr_rec$(),1016%,01%) = scrnshpsep$      /*(CR2264)Scrn Flag*/
/* (CR1830) end   */
        return
/* - (SR66200) */
        oracle_lne_text
            if str(lne_text$,1%, len(lne_text$)) = " " then return
            init(" ") stmt1$, stmt2$
            stmt1$ = "SELECT * FROM OE_TEXTFILE WHERE TEXT_ID = '"
            stmt1$ = stmt1$ & str(lne_text$,1%,len(lne_text$)) & "'"

            if schema% = 2% then str(stmt1$,15%,2%) = "NE"
            if schema% = 5% then str(stmt1$,15%,2%) = "NE"

            gosub oracle_flush
            gosub oracle_query
            gosub oracle_fetch
            if oci_err% > 0% then return
            text_key$, text_rec$() = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = BIN(text_lne%,4)
            str(text_key$,9%,1%) = "1"
            str(text_key$,10%,2%) = BIN(0001%,2)

            str(text_key$,12%,3%) = "014"
            str(text_key$,15%,2%) = BIN(0003%,2)
            str(text_key$,17%,1%) = BIN(0070%,1)
            str(text_key$,18%,47%) = " "
            pos% = 1%
            for field_num% = 12% to 14%
                gosub oracle_getfield
                str(text_rec$(pos%),1%,70%) = field$
                pos% = pos% + 1%
            next field_num%
            gosub write_text
            init(" ") stmt1$, stmt2$
            stmt1$ = "DELETE FROM OE_TEXTFILE WHERE TEXT_ID = '"
            stmt1$ = stmt1$ & str(lne_text$,1%,len(lne_text$)) & "'"

            if schema% = 2% then str(stmt1$,13%,2%) = "NE"
            if schema% = 5% then str(stmt1$,13%,2%) = "NE"

            gosub oracle_flush
            gosub oracle_exec
        return

        oracle_hdr_text
            if str(hdr_text$,1%, len(hdr_text$)) = " " then return
            init(" ") stmt1$, stmt2$, text_rec$()
            stmt1$ = "SELECT * FROM OE_TEXTFILE WHERE TEXT_ID = '"
            stmt1$ = stmt1$ & str(hdr_text$,1%,len(hdr_text$)) & "'"

            if schema% = 2% then str(stmt1$,15%,2%) = "NE"
            if schema% = 5% then str(stmt1$,15%,2%) = "NE"

            gosub oracle_flush
            gosub oracle_query
            gosub oracle_fetch
            if oci_err% > 0% then return
            text_key$, text_rec$() = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = BIN(text_id%,4)
            str(text_key$,9%,1%) = "1"
            str(text_key$,10%,2%) = BIN(0001%,2)

            str(text_key$,12%,3%) = "013"
            str(text_key$,15%,2%) = BIN(0003%,2)
            str(text_key$,17%,1%) = BIN(0070%,1)
            str(text_key$,18%,47%) = " "

            pos% = 1%
            for field_num% = 12% to 14%
                gosub oracle_getfield
                str(text_rec$(pos%),1%,70%) = field$
                pos% = pos% + 1%
            next field_num%
            gosub write_text
            init(" ") stmt1$, stmt2$
            stmt1$ = "DELETE FROM OE_TEXTFILE WHERE TEXT_ID = '"
            stmt1$ = stmt1$ & str(hdr_text$,1%,len(hdr_text$)) & "'"

            if schema% = 2% then str(stmt1$,13%,2%) = "NE"
            if schema% = 5% then str(stmt1$,13%,2%) = "NE"

            gosub oracle_flush
            gosub oracle_exec
        return

        write_text
            init(" ") readkey$
            str(readkey$,1%,11%) = str(text_key$,1%,11%)
            read #44, hold, key = readkey$, eod goto no_old_text

            delete #44
        no_old_text
            if adjrsn$ = "D" then return
            put #44, using L62150, text_key$, text_rec$(1), text_rec$(2), ~
                                   text_rec$(3)

L62150:           FMT CH(64), CH(70), CH(70), CH(70)


            write #44, eod goto bad_txt_write
        return
        bad_txt_write
        return


        oracle_lne_del
            init(" ") stmt1$, stmt2$
            stmt1$ = "DELETE FROM OE_BCKBUF2 WHERE SALES_ORDER_NUMBER = '"
            stmt1$ = stmt1$ & str(linekey$,1%,8%) & "' and SEQUENCE_NUMBER = '"
            stmt1$ = stmt1$ & str(linekey$,17%,3%) & "'"

            if schema% = 2% then str(stmt1$,13%,2%) = "NE"
            if schema% = 5% then str(stmt1$,13%,2%) = "NE"

            gosub oracle_flush
            gosub oracle_exec

        return

        oracle_delete_header
            init(" ") stmt1$, stmt2$
            stmt1$ = "DELETE FROM OE_BCKBUFFR WHERE SALES_ORDER_NUMBER = '"
            stmt1$ = stmt1$ & str(newhdr$(),10%,8%) & "'"

            if schema% = 2% then str(stmt1$,13%,2%) = "NE"
            if schema% = 5% then str(stmt1$,13%,2%) = "NE"

            gosub oracle_flush
            gosub oracle_exec

        return




        open_ncfile
          f2%(26%) = 0%

* First check to see if file exists; if so then end only run once
          call "OPENOLIB" (#26, "INPUT", f2%(26%), rslt$(26%), " ")
          if f2%(26%) = 0% then end         /* file found  */

* Second if no file open in OUTLIB or SESDBASE
          call "OPENOLIB" (#26, "OUTPT", f2%(26%), rslt$(26%), " ")

        return

        open_nefile
          f2%(27%) = 0%

* First check to see if file exists; if so then end only run once
          call "OPENOLIB" (#27, "INPUT", f2%(27%), rslt$(27%), " ")
          if f2%(27%) = 0% then end         /* file found  */

* Second if no file open in OUTLIB or SESDBASE
          call "OPENOLIB" (#27, "OUTPT", f2%(27%), rslt$(27%), " ")

        return


        open_dfxfile
          f2%(28%) = 0%

* First check to see if file exists; if so then end only run once
          call "OPENOLIB" (#28, "INPUT", f2%(28%), rslt$(28%), " ")
          if f2%(28%) = 0% then end         /* file found  */

* Second if no file open in OUTLIB or SESDBASE
          call "OPENOLIB" (#28, "OUTPT", f2%(28%), rslt$(28%), " ")

        return

        open_dnffile
          f2%(29%) = 0%

* First check to see if file exists; if so then end only run once
          call "OPENOLIB" (#29, "INPUT", f2%(29%), rslt$(29%), " ")
          if f2%(28%) = 0% then end         /* file found  */

* Second if no file open in OUTLIB or SESDBASE
          call "OPENOLIB" (#29, "OUTPT", f2%(29%), rslt$(29%), " ")

        return

        update_credit
             inv% = 0%

             call "AWDCRDIT" (cuscode$,           /* Customer Code */~
                              so$,                /* Sales order   */~
                              userid$,            /* Userid        */~
                              "CREDIT",           /* SESSION       */~
                              #46,                /* ARIBUFFR      */~
                              #47,                /* ARIBUF2       */~
                              #1,                 /* SYSFILE2      */~
                              #48,                /* ARMTRIAL      */~
                              #5,                 /* BCKMASTR      */~
                              #6,                 /* BCKLINES      */~
                              #45,                /* ARMTERMS      */~
                              #35,                /* CUSTOMER      */~
                              #4,                 /* INVMASTER     */~
                              #31,                /* CATEGORY      */~
                              #49,                /* STXCODES      */~
                              #63,                /* BCKSUBPT      */~
                              #54,                /* STORNAME      */~
                              #55,                /* ARINUMBR      */~
                              inv%  )             /* Error Return  */

        return

/* (AWD028) beg */
        get_user_pswd
            call "READ100" (#01, "ORACLE PASSWORD", f1%(1%))   /* SYSFILE2 */
            if f1%(1%) <> 0% then get #01 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return
/* (AWD028) end */
/* + (CR2068) */
        lookupBillTo
            call "READ100" (#35, cuscode$, f1%(35%))
            get #35 using L30250, billto$
L30250:         FMT POS(780), CH(9)
        return
/* - (CR2068) */        
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
*        Remove any left over 'Kill me' tranactions.
            bufferkey$ = " "
            call "DELETE" (#9, bufferkey$, 3%)
/* (AWD017) */
REM            call "MESSAGE" addr("DE", port$, return%)

REM            call "SHOSTAT" ("DELETING FILES " )  stop



            if f2%(26%) = 0% and schema% = 1%                   ~
                     then call "FILEBGON" addr(#26)
            if f2%(27%) = 0% and schema% = 2%                   ~
                     then call "FILEBGON" addr(#27)
            if f2%(28%) = 0% and schema% = 4%                   ~
                     then call "FILEBGON" addr(#28)
            if f2%(29%) = 0% and schema% = 5%                   ~
                     then call "FILEBGON" addr(#29)

            call "DISCNNCT" (oci_err%)
            end


