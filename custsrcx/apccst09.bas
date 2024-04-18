        REM *************************************************************~
            *     Note - (SLS_FOB$) is not Rebuilt                      *~
            *  Program Name      - APCCST09                             *~
            *  Creation Date     - 02/06/95                             *~
            *  Last Modified Date- 10/31/05                             *~
            *  Description       - This Utiltiy Program Reads the Sales *~
            *                      Analysis Data File (APCSLSDT) and    *~
            *                      Re-Builds only the Cost Data.        *~
            *                                                           *~
            *  Special Notes - (COST SALE) Table used for Calc of Parts *~
            *                   with no cost, or Product not costed.    *~
            *                   1st Field = Pcnt for MFG Product        *~
            *                   2nd Field = Pcnt for Part               *~
            *                   3rd Field = Dollar Value for Part       *~
            *                                                           *~
            *                  (COST NONE) Table by Model used to       *~
            *                   define Manufactured Products which we   *~
            *                   do not Calculate the Cost for.          *~
            *                                                           *~
            *                   (COMPUTE_COST) Routine Calculates the   *~
            *                   cost of Parts and No Cost Product.      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/25/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/11/97 ! Mod for Costing to add new file. APCPLNDP! RHH *~
            *          !   used by labor sub (APCCST5B)           !     *~
            * 11/12/97 ! Mod add alt index 4 to ARIMASTR          ! RHH *~
            * 10/31/05 ! (AWD001) CR347 Mod for Sub Part          ! CMG *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *02/13/2019! CR-1894  Increase EMP DEPT size to 3     ! DES *~
            *************************************************************

        dim                                                              ~
            invoicekey$20, inv_key$8,    /* Invoice Line Items         */~
            cnt$8, cnt1$8,               /* Invoice Scanned Counter    */~
            sav_key$17,                  /* SAVE CUSTOMER/INVOICE NO   */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$32,                    /* Gencode Description        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            gencdkey$50,                 /* KEY TO GENCODES FILE       */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invnumber$8,                 /* LAST INV NUMBER PROCESSED  */~
            last_inv$8,                  /* LAST INV NO. PREV RUN      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            part$25,                     /* PART NUMBER ON INVOICE LINE*/~
/*PAR000*/  partno1$20,                  /* MFG subpart Number         */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            postdate$6,                  /* DATE INVOICE WAS POSTED    */~
            purge_date$6,                /* Purge Date                 */~
            purge_key$20,                /* Primary Key                */~
            purge_rec$34,                /* Part Rec                   */~
            rebuild_key$20,              /* Use When Rebuild Srt Codes */~
            sav_cus$9,                   /* SAVE CUSTOMER CODE         */~
            sav_fob$25,                  /* SAVE CUS AND S.O. NUMBER   */~
            sls_rec$(2%)250,             /* Use to Define Whole Record */~
            title$40,                    /* ERROR LOG REPORT TITLE     */~
            userid$3                     /* Current User Id            */

        dim bg_inv$8, bg_date$8,         /* Starting Inv No., Post Date*/~
            ed_inv$8, ed_date$8,         /* Ending Inv No., Post Date  */~
            calc_code$1, calc_desc$30,   /* S.A Calculation Code 1,2,3 */~
            sale_code$1, sale_desc$30,   /* Salesman Calc Code 1, 2    */~
            txt$(9%)40,                  /* Screen Text                */~
            last_calc$1,                 /* Last S.A. Code Used        */~
            last_sale$1,                 /* Last Salesman Calc Code    */~
            post_yr$2,                   /* Invoice Posting Year       */~
            yr1$4, yr2$4                 /* Processing Years           */

        dim f2%(64%),                    /* = 0 if the file is open    */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

        dim                              /* Costing Variables          */~
            cst(9%), apc_err$20,         /* MFG Calculated Costs       */~
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

/* Y2K */
        dim bg_date10$10,                /* Begin Date MM/DD/YYYY      */~
            ed_date10$10,                /* End Date   MM/DD/YYYY      */~
            workdate10$10                /* Work Date  MM/DD/YYYY      */
/* Y2K */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 11/11/97 Sales Analysis - Invoice Create"
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
            * #63 ! BCKSUBPT ! Sub Part File                 (PAR000)   *~
            * #41 ! AWDPCMST ! Pricing Definition file     CR1894       *~               
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

        REM - COSTING FILES

            select #21, "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
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
                        keypos =    7, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos  =  12, keylen =  26, dup

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
                        varc,     indexed,  recsize = 128,               ~
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
/*AWD001*/              varc,     indexed,  recsize =  768,              ~
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
                        varc,     indexed,  recsize =  64,               ~
                        keypos =    1, keylen =  25

/*PAR000*/
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup     

            select #41, "AWDPCMST",                                      ~
	                varc,     indexed,  recsize =   128,                 ~
		            keypos =    9, keylen =  53,                         ~
		            alt key  1, keypos  =  1, keylen =  8
         

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%, rslt$(7%))
REM            call "OPENCHCK" (#8 , fs%(8%), f2%(8%),2500%, rslt$(8%))
            call "OPENCHCK" (#9 , fs%(9%), f2%(9%), 0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 50%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 50%, rslt$(12%))
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
/*PAR000*/  call "OPENCHCK" (#63, fs%(63%), f2%(63%),  0%, rslt$(63%))
/*CR1894*/  call "OPENCHCK" (#21, fs%(21%), f2%(21%),  0%, rslt$(21%))
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
            call "DATEFMT" (date$)

            /* Y2K */
            workdate10$ = date
            call "DATFMTC" (workdate10$)
            /* Y2K */

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "APCCST09: " & str(cms2v$,,8)


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

            for fieldnr% = 1% to  5%
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
            if fieldnr% < 1% or fieldnr% >  5% then editpg1
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
         "Enter a Salesman From Calc Code? 1 = Customer, 2 = Invoice   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
/* Y2K */
            init(" ") errormsg$, inpmessage$, bg_inv$, ed_inv$,          ~
                      bg_date$, ed_date$, sale_code$, sale_desc$,        ~
                      calc_code$, calc_desc$, txt$(), invnumber$,        ~
                      last_inv$, last_calc$, last_sale$, post_yr$,       ~
                      yr1$, yr2$, purge_key$, purge_rec$, rebuild_key$,  ~
                      sls_rec$(), purge_date$, sav_cus$, sav_fob$,       ~
                      bg_date10$, ed_date10$

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
        return

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

        REM *************************************************************~
            *       F o m a t   S t a t e m e n t s                     *~
            *************************************************************

            FMT  CH(06),            /* Invoice Posting Date (POSTDATE$)*/~
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
                 PD(14,4),          /* Line Item Comm Amt     (LINECOM)*/~
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
                 CH(19)             /* Free Area                       */

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
                                L40200           /* Salesman Calc Code */

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02),                                               ~
                "APC Sales Analysis Quick Costing Rebuild",              ~
               at (01,62), "Today:",                                     ~
               at (01,70), fac(hex(8c)), date$                  , ch(08),~
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
               at (06,55), fac(hex(84)),   bg_date10$           , ch(10),~
                                                                         ~
               at (07,02), "Ending Invoice Number  :",                   ~
               at (07,30), fac(lfac$(3%)), ed_inv$              , ch(08),~
               at (07,40), "Posting Date:",                              ~
               at (07,55), fac(hex(84)),   ed_date10$           , ch(10),~
                                                                         ~
               at (08,02), "Sales Calculation Code :",                   ~
               at (08,30), fac(lfac$(4%)), calc_code$           , ch(01),~
               at (08,40), fac(hex(84)),   calc_desc$           , ch(30),~
                                                                         ~
               at (09,02), "Salesman Calc Code     :",                   ~
               at (09,30), fac(lfac$(5%)), sale_code$           , ch(01),~
               at (09,40), fac(hex(84)),   sale_desc$           , ch(30),~
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

               if keyhit% <> 9% then L40720
                  debug% = 1%
                  keyhit% = 16%

L40720:        if keyhit% <> 15 then L40750
                  call "PRNTSCRN" : goto L40230

L40750:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40930     /*  Input Mode             */
            pf$(1%)= "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffff0cffff0f1000)
            if fieldnr% = 1% then L40900
                str(pf$(3%),64%) = " "    : str(pfkeys$,16%,1%) = hex(ff)
L40900:     if fieldnr% > 1% then L40910
L40910:     return

L40930: if fieldnr% > 0% then L41040  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                   (9)Debug Display     " &        ~
                     "                       (16)Process Data"
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
            if userid$ = "RHH" then return
               str(pf$(2%),60%)     = " " : str(pfkeys$,9%,1%) = hex(ff)
            return
L41040:                              /*  Edit Mode - Enabled    */
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
                              L50780          /* Salesman Calc Code     */

            return

L50160: REM Test for Processing Years             YR1$, YR2$
              if yr1$ <> " " then goto L50200
                 yr1$ = str(workdate10$,7%,4%)

L50200:       convert yr1$ to yr1%, data goto L50224

              convert (yr1% - 1%) to yr2$, pic(####)

              if yr2$ > yr1$ then goto L50224
        return
L50224:      errormsg$ = "(Error) - Invalid Process Years?"
             init(" ") yr1$, yr2$
        return

L50240: REM Test for Starting Invoice Number      BG_INV$
              bg_date10$ = "NONE    "
              if bg_inv$ <> " " then goto L50300
                 bg_inv$ = invnumber$
L50300:          convert bg_inv$ to bg_inv%, data goto L50390

                 convert bg_inv% to bg_inv$, pic(00000000)

                 read #2,key 1% = bg_inv$, using L50360, bg_date$,        ~
                                                           eod goto L50380
L50360:             FMT POS(533), CH(6)
                 bg_date10$ = bg_date$
                 call "DATFMTC" (bg_date10$)
L50380: return
L50390:   errormsg$ = "(Error) - Invalid Starting Invoice Number?"
          init(" ") bg_inv$, bg_date$, bg_date10$
        return

L50430: REM Test for Ending Invoice Number        ED_INV$
              ed_date$ = "NONE    "
              if ed_inv$ <> " " then goto L50500
                 ed_inv$ = "99999999"

L50500:          convert ed_inv$ to ed_inv%, data goto L50600

                 convert ed_inv% to ed_inv$, pic(00000000)

                 read #2,key 1% = ed_inv$, using L50560, ed_date$,        ~
                                                           eod goto L50580
L50560:             FMT POS(533), CH(6)
                 ed_date10$ = ed_date$
                 call "DATFMTC" (ed_date10$)
L50580:          if bg_inv% > ed_inv% then goto L50600
        return
L50600:   errormsg$ = "(Error) - Invalid Ending Invoice Number?"
          init(" ") ed_inv$, ed_date$, ed_date10$
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

        REM *************************************************************~
            *          C O S T I N G   E R R L O G   R E P O R T        *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~--------------+
L55060: %!---------------------------------------------------------------~
        ~--------------!
L55080: %! ######## @ ########    #######################################~
        ~#    Page: ###!
L55100: %!QTY !<--- MFG Part Number --->!<-- Error Message ->!MFG Cost! M~
        ~FG Quantity   !
L55120: %!----!-------------------------!--------------------!--------!--~
        ~--------------!
L55140: %!####!#########################!####################!####.##-!  ~
        ~  ########.##-!

        REM *************************************************************~
            *               S P E C I A L   R O U T I N E S             *~
            *************************************************************

        select_printer
            title$ = " APC Costing Sales Analysis Error Report"
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            init(" ") xtime$
            call "TIME" (xtime$)
            call "DATEFMT" (date$)
            call "SETPRNT" (" ","CERR",0%,0%)
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

        prt_detail
            if lcnt% > 57% then gosub prt_header
               print using L55120
               print using L55140, e_qty%, part$, apc_err$, tot_cst,      ~
                           x_qty

            lcnt% = lcnt% + 2%
        return

        load_last
            read #1,key = "APC SALES ANAL INV",using L60390  ,invnumber$, ~
                          last_inv$, last_calc$, last_sale$,eod goto L60400
L60390:          FMT XX(20),CH(8), CH(8), CH(1), CH(1)
L60400: return

        process_data
            call "SHOSTAT" ("Rebuild Costing Data Only")
            cnt%  = 0%
            cnt1% = 0%
            inv_key$ = " "
            str(inv_key$,1%,8%) = bg_inv$
        process_next
            if cnt% <> 0% then goto L60540
              read #2,key 1% = inv_key$, using L60560, cuscode$, inv_key$,~
                                                  eod goto L60540
              goto L60570

L60540:     read #2,key 1% > inv_key$, using L60560, cuscode$, inv_key$,  ~
                                             eod goto update_summary
L60560:        FMT CH(9), CH(8)
L60570:     cnt% = cnt% + 1%
            if mod(cnt%,25) <> 0 then goto L60630
               convert cnt% to cnt$,   pic(########)
               convert cnt1% to cnt1$, pic(########)
               call "SHOSTAT" ("SCANNED= " & cnt$& " REBUILT= "& cnt1$)

L60630:     if inv_key$ > ed_inv$ then goto update_summary
            invoicekey$ = all(hex(00))
            str(invoicekey$,1%,9%)  = cuscode$
            str(invoicekey$,10%,8%) = inv_key$
            sav_key$ = str(invoicekey$,1%,17%)
        read_loop
            read #8,key > invoicekey$, using L60720  , postdate$,         ~
                             invoicekey$, part$, invdisc, sls_qty,       ~
                             linedisc, lineext, eod goto process_next
L60720:        FMT CH(6), POS(15), CH(20), POS(76), CH(25), POS(181),    ~
                   PD(14,4), POS(260), PD(14,4), POS(276), 2*PD(14,4)
            if sav_key$ <> str(invoicekey$,1%,17%) then                  ~
                                                   goto process_next
            cnt1% = cnt1% + 1%      /* RE-CALC NET INVOICE AMOUNT     */
                                    /* PRICE AFTER LINE ITEM DISCOUNT */
            sls_price = round(lineext * (1.0 - (linedisc/100.0)), 2)
                                    /* PRICE AFTER ORDER DISCOUNT     */
            sls_price = round(sls_price * (1.0 - (invdisc/100)), 2)

            factor = 1.0
            if sls_price < 0 then factor = -1.0
            sls_qty   = abs(sls_qty) * factor

            if sls_qty <> 0 then goto L60930
               mat cst = zer                      /* NO QUANTITY, THEN */
               goto L60940                         /* NO COST           */

L60930:     gosub calc_cost
L60940:     read #8,hold,key = invoicekey$, eod goto L61030
               put #8, using L60960, cst()
L60960:          FMT POS(402), 9*PD(14,4)
            rewrite #8
            goto read_loop
        update_summary
            gosub create_report
        return clear all
        goto exit_program
L61030:     call "SHOSTAT" ("Error-Updating --- "& invoicekey$)
            stop
            goto read_loop

        calc_cost
REM I am just setting partno1$ to zero b/c this program has to be old
REM It is still using APCSLSDT which is an obsolete file.

           partno1$ = "00000"
           mat lab  = zer     : mat tc   = zer
           mat apc_err% = zer : mat pc = zer
           width = 0.0        : price = 0.0
           convert str(part$,13%,4%) to width, data goto L61120
L61120:
           kk% = 1%
           convert str(part$,1%,3%) to kk%, data goto L61150
L61150:
           p_err% = 0%
           x_err% = 0%
           calc% = 0%
           if debug% = 1% then calc% = 99%
           if len(part$) < 19 then x_err% = 2%      /* PROD IS A PART */
           if str(part$,1%,1%) = "4" then x_err% = 2%        /* PART */
           if width = 0 then x_err% = 2%                     /* PART */
           if x_err% <> 0% then goto L61800

           if str(sale$(kk%),2%,1%) = "*" then calc% = 8% /* PRICE ONLY*/
           call "APCCST0B" ( calc%,      /* Calculation Method         */~
                             part$,      /* MFG Part Number            */~
/* PAR000 */                 partno1$,  /* MFG Sub part number        */~
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
                             #21,        /*   (AWDPCMST)   CR1894      */~
                             #34,        /*   (APCSKUNO)               */~
                             #35,        /*   (APCPCMSK)               */~
                             #36,        /*   (APCPCMSD)               */~
                             #37,        /*   (APCCSTEX)               */~
                             #38,        /*   (APCSTOCK)               */~
                             #32,        /*   (APCPLNDP)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */
            if str(sale$(kk%),2%,1%) <> "*" then goto L61730
               x_err% = 3%
               p_err% = 0%
               goto L61800
L61730:     for i% = 1% to 20%
              if apc_err%(i%) = 0% then goto L61780
                 p_err% = i%
                 x_err% = 1%
                 cst_err% = cst_err% + 1%
L61780:     next i%

L61800:   gosub store_cost                    /* Gather All MFG Costs  */
          if p_err% <> 0% then gosub update_error_log

        return

        store_cost
           gosub calc_transport
           mat cst = zer                     /* Total Vinyl and Misc.  */
                                             /* (Mat'l + Scrap) Cost   */
           tot_mat = round(tc(3%) + tc(6%) + tc(9%) + tc(12%) + tc(15%) +~
                           tc(18%) + tc(21%), 4) /* Include W/F Amount */
           if x_err% > 1% then gosub compute_cost
                                     /* Use Cost from Costing Errors   */
           tot_cst = round( tot_mat + lab(8%) + lab(9%) + tc(19%) +      ~
                                                          tc(20%), 4)
                                     /* Note - CST() are the Values    */
                                     /*   Assoc. with each Line Item   */
           cst(1%) = round(tot_mat * sls_qty, 2) /* Total Material     */
           cst(2%) = round(lab(8%) * sls_qty, 2) /* Total Dir/Ind Labor*/
           cst(3%) = round(lab(9%) * sls_qty, 2) /* Total Overhead Cost*/
           cst(4%) = round(tc(19%) * sls_qty, 2) /* Total Freight Cost */
           cst(5%) = round(tc(20%) * sls_qty, 2) /* Total Vinyl Disc't */
           cst(6%) = round(tot_cst * sls_qty, 2) /* Total MFG Cost     */
           cst(7%) = round(trn_amt * sls_qty, 2) /* Total Trans Cost   */
           if factor < 0 then cst(7%) = 0.0      /* No Trans Credit    */
           cst(8%) = round(sls_price, 2)         /* Total Price W/Disc */
           cst(9%) = round(sls_qty, 2)           /* Total Quantity     */
        return

        calc_transport
           if sav_cus$ = cuscode$ then return
              sav_cus$ = cuscode$

           gencdkey$ = " " : sls_regn% = 0% : trn_amt = 0.0
           str(gencdkey$,1%,9%)   = "COST TRAN"
           str(gencdkey$,10%,15%) = cuscode$
           read #9,key = gencdkey$, using L62200, descr$, eod goto L62230
L62200:       FMT POS(25), CH(30)
           convert str(descr$,1%,2%) to sls_regn%, data goto L62230

L62230:    convert sls_regn% to sls_regn$, pic(00)
           gencdkey$ = " "
           str(gencdkey$,1%,9%)   = "COST REGN"
           str(gencdkey$,10%,15%) = sls_regn$
           read #9,key = gencdkey$, using L62200, descr$, eod goto L62310

           convert str(descr$,1%,8%) to trn_amt, data goto L62310

L62310: return

        compute_cost
           tot_mat = 0.0
           mat lab = zer : mat tc = zer
           if str(sale$(kk%),1%,1%) <> "*" then kk% = 1%
           unit_price = 0.0
           if sls_qty <> 0 then unit_price = round(sls_price/sls_qty, 2)

           if x_err% <> 2% then goto L62470
                                                  /* Product is a Part */
              if unit_price = 0 then tot_mat = sale(kk%,3%)              ~
                                else tot_mat = unit_price * sale(kk%,2%)
           if tot_mat = 0 then goto L62470
        return
                                                  /* Costing Error     */
L62470:    tot_mat = (pc(1%) * .50) * sale(kk%,1%)/* Calc based on the */
                                                  /* Catalog Price     */
           if tot_mat = 0 then tot_mat = unit_price * sale(kk%,1%)
           if tot_mat = 0 then tot_mat = sale(kk%,3%)
        return

        update_error_log
           read #40,hold,key = part$, eod goto L62560
        return
L62560:    e_qty% = 1.0
           x_qty = sls_qty
           x_cst = abs(tot_cst) * x_qty
           put #40, using L62620, part$, e_qty%, x_qty, err$(p_err%),     ~
                                 x_cst
           write #40, eod goto L62630
L62620:      FMT CH(25), BI(2), PD(14,4), CH(20), PD(14,4)
L62630: return

        create_report
           gosub select_printer
           part$ = " "
        create_rpt_nxt
           read #40,key > part$, using L62620, part$, e_qty%, x_qty,      ~
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
            if f2% <> 0% then goto L64010

            call "WORKOPN2" (#40,mode$, 500%, f2%)
            if f2% <> 0% then goto L64030
        return
L64010:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCSLSW1)") : stop
        return
L64030:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCSLSWK)") : stop
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
           read #9,key > gencdkey$, using L64180, gencdkey$, descr$,      ~
                                    eod goto load_sale_done
L64180:       FMT CH(24), CH(30)
           if str(gencdkey$,1%,9%) <> "COST SALE" then goto load_sale_done
              kk% = 1%
              convert str(gencdkey$,10%,3%) to kk%, data goto L64220
L64220:
              convert str(descr$,1%,8%)  to sale(kk%,1%), data goto L64250
L64250:
              convert str(descr$,11%,8%) to sale(kk%,2%), data goto L64270
L64270:
              convert str(descr$,22%,8%) to sale(kk%,3%), data goto L64290
L64290:
              sale(kk%,1%) = sale(kk%,1%) / 100.0
              sale(kk%,2%) = sale(kk%,2%) / 100.0
              str(sale$(kk%),1%,1%) = "*"       /* COST OF SALE EXISTS */
              goto load_sale_nxt
        load_sale_done
           gencdkey$ = " "
           str(gencdkey$,1%,9%) = "COST NONE"
        load_nocost_nxt
           read #9,key > gencdkey$, using L64180, gencdkey$, descr$,      ~
                                                eod goto load_nocost_done
           if str(gencdkey$,1%,9%) <> "COST NONE" then                   ~
                                                  goto load_nocost_done
              kk% = 0%
              convert str(gencdkey$,10%,3%) to kk%, data goto L64440
L64440:
              if kk% = 0% then goto load_nocost_nxt
              str(sale$(kk%),2%,1%) = "*"      /* SET DO NOT COST FLAG */
              goto load_nocost_nxt
        load_nocost_done
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
