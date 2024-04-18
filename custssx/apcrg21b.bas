        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRG21B (Like APCRGA1B)             *~
            *  Creation Date     - 01/21/97                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - This Program provides RGA Detail     *~
            *                      Entry/Update and RGA Detail Inquiry  *~
            *                                                           *~
            *  Special Comments  - Uses NEW Planning Files              *~
            *                      Hidden PF10 for Costing Recalc.      *~
            *                      Hidden PF10 for Costing Recalc.      *~
            *  Subroutines       - APCCST0B (Costing)                   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/21/97 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 11/18/97 ! Checked For 60403 Revision Mods.         ! DJD *~
            *          !                                          !     *~
            * 02/20/98 ! Y2K Conversions                          ! DJD *~
            * 01/18/01 ! Mod to for eight digit comp num (EWD002) ! CMG *~             
            * 01/11/06 ! (AWD003) CR347 Mod for new sub part      ! CMG *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            * 09/03/08 ! (AWD004) mod for P load numbers          ! CMG *~
            *02/13/2019! CR-1894  Increase EMP DEP size to 3      ! DES *~
            *************************************************************

        sub "APCRG21B"   (rga_cuscode$,  /* RGA Customer No.           */~
                          rga_number$,   /* RGA No.                    */~
                          rga_item$,     /* RGA Item                   */~
                          proc%,         /* APCRGA01 Process Flag      */~
                          #2,            /* APCRGADT                   */~
                          #3,            /* GENCODES                   */~
                          #4,            /* CUSTOMER                   */~
                          #5,            /* TXTRGA                     */~
                          #63 )          /* BCKSUBPT                   */

        dim rga_cuscode$9,               /* RGA Customer No.           */~
            rga_dt_status$2,             /* RGA Item Status Code       */~
            rga_number$4,                /* RGA Number                 */~
            rga_item$2,                  /* RGA Item                   */~
/*(EWD002)*/rga_compt$8,                 /* RGA Complaint No.          */~
/*(EWD002)*/comp_key$5,                  /* PRIMARY KEY                */~
            rga_reason$3,                /* RGA Item Reason Code       */~
            rga_dept$3,                  /* RGA Item Department        */~
            rga_part$25,                 /* RGA Item Part No.          */~
            rga_so$8,                    /* RGA Item Sales Order No.   */~
            rga_line$2,                  /* RGA S.O. Line Item         */~
            rga_piece$4,                 /* RGA S.O. Piece Count (x of)*/~
            rga_qty$4,                   /* RGA S.O. Quantity          */~
            rga_po$16,                   /* RGA Purchase Order No.     */~
            rga_load$5,                  /* RGA Item Load No.          */~
            rga_inv$8,                   /* RGA Item Invoice No.       */~
            rga_chk$8,                   /* RGA Item Check No.         */~
            rga_credit$14,               /* RGA Item Credit Display    */~
            rga_hows$2,                  /* RGA How Shipped Code       */~
            rga_prod_dte$8,              /* RGA Item Production Date   */~
            rga_gl_acct$9,               /* General Ledger Account No. */~
            rga_gl_posted$1,             /* RGA Item Posted to G/L Flag*/~
            rga_pickup_load$(3%)5,       /* RGA Item Pickup Load No.   */~
            rga_pickup_dte$(3%)8,        /* RGA Item Pickup Date       */~
            rga_salesman$4,              /* RGA Salesman               */~
            rga_dt_desc_txt$4,           /* RGA Description Text Code  */~
            rga_dt_txt$1,                /* RGA Description Text Flag  */~
            rga_dt_userid$3,             /* Userid of RGA Entry/Mod    */~
            rga_dt_mod_dte$8,            /* RGA Item Entry/Mod Date    */~
/*PAR000*/  rga_dt_filler$249,           /* APCRGADT Filler Area       */~
/*PAR000*/  rga_dt_subp$20,              /* APCRGADT Subpart           */~
            or_so$8,                     /* APCPLNOR S.O. NO.          */~
            or_load$5,                   /* APCPLNOR Load No.          */~
            or_po$16,                    /* APCPLNOR P.O. No.          */~
            or_inv$8,                    /* APCPLNOR Invoice No.       */~
            or_chk$8,                    /* APCPLNOR Check No.         */~
            or_hows$2,                   /* APCPLNOR How Shipped       */~
            bck_so$16,                   /* BCKLINES S.O. No.          */~
            bck_part$25,                 /* BCKLINES Part No.          */~
            dt_part$25,                  /* APCPLNDT Part No.          */~
            bck_text$4,                  /* BCKLINES Order Text ID     */~
            dt_bar$18,                   /* APCPLNDT Barcode           */~
            dt_dept$3,                   /* APCPLNDT Department Code   */~
            dt_proc$2,                   /* APCPLNDT Process Code      */~
            dt_date$6,                   /* APCPLNDT Production Date   */~
            dt_load$5,                   /* APCPLNDT Load No.          */~
            dt_ref$8, warranty$8,        /* APCPLNDT Warranty No.      */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Long Form             */~
            sls_regn$2,                  /* Sales Region for Costing   */~
            sav_key$9,                   /* Save Table Name            */~
            gencdkey$24,                 /* GENCODES File Read Key     */~
            readkey$24,                  /* GENCODES File Read Key     */~
            dtlkey$6,                    /* APCRGADT File Read Key     */~
            calckey$6,                   /* APCRGADT Cost Recalc Key   */~
            mastr_key$25,                /* BCKMASTR File Read Key     */~
            mastr_key1$16,               /* BCKMASTR File Alt Key 1    */~
            bck_key$19,                  /* BCKLINES File Read Key     */~
            or_key4$8,                   /* APCPLNOR File Alt Key 4    */~
            or_key1$25,                  /* APCPLNOR File Alt Key 1    */~
            dt_key$23,                   /* APCPLNDT File Read Key     */~
            dt_key4$8,                   /* APCPLNDT File Alt Key 4    */~
            comp_rec$150,                /* APCCOMPT File Record       */~
            status_desc$30,              /* Detail Status Description  */~
            reason_desc$30,              /* Reason Code Description    */~
            dept_desc$30,                /* Department Code Description*/~
            part_desc$32,                /* Part Description           */~
            desc$30,                     /* Costing Part Description   */~
            descr$30,                    /* Display GENCODES Lookup    */~
            tab_hdr$30,                  /* Display Screen Header      */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$60,                 /* Error message              */~
            text$(113,1)70,              /* Text Buffer Area           */~
            txt$4,                       /* Text ID                    */~
            text_desc$60,                /* Line Item Text             */~
            text_key$11,                 /* Text File Key              */~
            sav_key1$11,                 /* Text File Save Key         */~
            atext$(2)70,                 /* Text (2) Lines             */~
            dt_flag$1,                   /* Line Item Text as Part Desc*/~
            header$79,                   /* Text Edit Description Hdr. */~
            i$(24%)80,                   /* Detail Line(10) Array Area */~
            inpmessage$79,               /* Informational Message      */~
            cursor%(2%),                 /*                            */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            progid$18,                   /* Screen Line #2 Program ID  */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim                              /* PAR000                     */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */


        dim f2%(38%),                                                    ~
            f1%(38%),                                                    ~
            fs%(38%),                                                    ~
            rslt$(38%)20

        dim                              /* Costing Variables          */~
            cst(9%),                     /* MFG Calculated Costs       */~
            tmp$(7%,50%)25,              /* Store all Raw Mat'l Part No*/~
            tmc(7%,50%),                 /* Assoc. Cut Inches In Dec.  */~
            tmct(7%,50%),                /* Assoc. Total Cost Raw Mat'l*/~
            tmu%(7%,50%),                /* Assoc. Unit of Measure     */~
            tmd$(7%,50%)32,              /* Assoc. Raw Mat'l Desc      */~
            tmuc(7%,50%),                /* Assoc. Raw Mat'l Unit Cost */~
            tmsi(7%,50%),                /* Assoc. Scrap Inches Decimal*/~
            tmsc(7%,50%),                /* Assoc. Scrap Mat'l Cost    */~
            tmeq$(7%,50%)3,              /* Assoc. Calc Typ and Eq No. */~
            tmph$(7%,50%)5,              /* Assoc. Phantom Designator  */~
            tcnt%(7%),                   /* Assoc. Count for Each Type */~
            lab(10%),                    /* Breakdown of Labor Cost    */~
            avg_pay(15%),                /* Avg Pay By Dept            */~
            uph(15%),                    /* Avg Unit Per Manhour Dept  */~
            tc(25%),                     /* Total Cost's               */~
            tt(25%),                     /* Cost Total Buckets         */~
            rm_mat(20%),                 /* Total Vinyl,Misc, Mat      */~
            rm_mats(20%),                /* Total Vinyl,Misc, Mat Scrap*/~
            apc_err%(20%),               /* Store Error Code each Modul*/~
            pc(36%),                     /* 36 Price Sheets            */~
            cuscode$9,                   /* Customer Code              */~
            sale(1000%,3%),              /* Store 'COST SALE' Values   */~
            sale$(1000%)2                /* Store 'No Cost' Flag       */

        dim                              /* PAR000                     */~
            part$25,                     /* MFG Part Number            */~
            partno1$20                   /* MFG Sub Part               */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 01/23/97 RGA Detail Inquiry/Update Prog "

        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #02 ! APCRGADT ! APC RGA Detail Master File               *~
            * #03 ! GENCODES ! System Code Table File                   *~
            * #04 ! CUSTOMER ! Customer Master File                     *~
            * #05 ! TXTRGA   ! RGA Text File                            *~
            * #06 ! APCPLNOR ! Group S.O. Header File                   *~
            * #07 ! APCCOMPT ! Complaint Tracking Master File           *~
            * #09 ! HNYMASTR ! Inventory Master File                    *~
            * #10 ! TXTFILE  ! Text File                                *~
            * #11 ! AMTBOMIF ! Inventory Validity Check                 *~
            * #12 ! BCKLINES ! S.O. Line Items File                     *~
            * #13 ! BCKMASTR ! S.O. Header File                         *~
            * #14 ! APCPLNDT ! APC Tracking Master File (New)           *~
            *************************************************************~
            *                  C O S T I N G   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #21 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #09 ! HNYMASTR ! Part Master File                         *~
            * #23 ! HNYQUAN  ! Inventory Quantities Master File         *~
            * #03 ! GENCODES ! Master Code Table File                   *~
            * #25 ! AMTBOMCD ! Master Equation File                     *~
            * #27 ! APCEMPLY ! Employee Master File                     *~
            * #28 ! APCEQUAT ! Equation an Parts Cross Reference File   *~
            * #29 ! APCCSTHP ! Hardware and Packaging Costing Components*~
            * #30 ! APCCSTLR ! Departments Average Hourly Rates         *~
            * #31 ! CPRPRICE ! Master System Price File                 *~
            * #04 ! CUSTOMER ! Master Customer File                     *~
            * #33 ! APCPCMST ! Pricing Definition file                  *~
            * #34 ! APCSKUNO ! Home Center's Skuno File                 *~
            * #35 ! APCPCMSK ! Pricing Key Definition File              *~
            * #36 ! APCPCMSD ! Pricing Master Calc Definition File      *~
            * #37 ! APCCSTEX ! APC COSTING EXCEPTION FILE               *~
            * #38 ! APCSTOCK ! APC STOCK MASTER FILE                    *~
            * #39 ! APCPLNDP ! Planning Master Department File          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #6,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =  27,  keylen = 25,          ~
                            key  2, keypos =  70,  keylen =  8, dup,     ~
                            key  3, keypos =  78,  keylen =  8, dup,     ~
                            key  4, keypos =  52,  keylen =  8,          ~
                            key  5, keypos =  36,  keylen = 16, dup

            select #7,  "APCCOMPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =    5,                    ~
                        alt key  1, keypos =   6,  keylen = 10,          ~
                            key  2, keypos =  16,  keylen = 16, dup,     ~
                            key  3, keypos =  32,  keylen =  8, dup,     ~
                            key  4, keypos =  40,  keylen = 13, dup,     ~
                            key  5, keypos = 191,  keylen = 10, dup

            select #9,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #10, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =   11

            select #11, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

            select #12, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #13, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #14, "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   24, keylen =  23,                     ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup

        REM - COSTING FILES

            select #21, "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #23, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #25, "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

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
/*AWD003*/              varc,     indexed,  recsize =  768,              ~
                        keypos = 1,    keylen =   9


            select #37,  "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            select #38, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32


            select #39,  "APCPLNDP",                                     ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =   11, keylen =   12,                    ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos =    4, keylen =  12,         ~
                            key  3, keypos =    1, keylen =  15

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#6,  fs%(6%),  f2%(6%),   0%, rslt$(6%))
            call "OPENCHCK" (#7,  fs%(7%),  f2%(7%),   0%, rslt$(7%))
            call "OPENCHCK" (#9,  fs%(9%),  f2%(9%),   0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),  0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),  0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),  0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),  0%, rslt$(14%))
        REM - COSTING FILES
            call "OPENCHCK" (#21, fs%(21%), f2%(21%),  0%, rslt$(21%))
            call "OPENCHCK" (#23, fs%(23%), f2%(23%),  0%, rslt$(23%))
            call "OPENCHCK" (#25, fs%(25%), f2%(25%),  0%, rslt$(25%))
            call "OPENCHCK" (#27, fs%(27%), f2%(27%),  0%, rslt$(27%))
            call "OPENCHCK" (#28, fs%(28%), f2%(28%),  0%, rslt$(28%))
            call "OPENCHCK" (#29, fs%(29%), f2%(29%),  0%, rslt$(29%))
            call "OPENCHCK" (#30, fs%(30%), f2%(30%),  0%, rslt$(30%))
            call "OPENCHCK" (#31, fs%(31%), f2%(31%),  0%, rslt$(31%))
            call "OPENCHCK" (#33, fs%(33%), f2%(33%),  0%, rslt$(33%))
            call "OPENCHCK" (#34, fs%(34%), f2%(34%),  0%, rslt$(34%))
            call "OPENCHCK" (#35, fs%(35%), f2%(35%),  0%, rslt$(35%))
            call "OPENCHCK" (#36, fs%(36%), f2%(36%),  0%, rslt$(36%))
            call "OPENCHCK" (#37, fs%(37%), f2%(37%),  0%, rslt$(37%))
            call "OPENCHCK" (#38, fs%(38%), f2%(38%),  0%, rslt$(38%))
            call "OPENCHCK" (#39, fs%(39%), f2%(39%),  0%, rslt$(39%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            progid$ = "APCRG21B: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            if proc% = 1%             then L10100
            fieldnr% = 1% :           goto L11250
L10100:     for fieldnr% = 1% to 1%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then L10270

L10140:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit% =  1% then gosub startover
                     if keyhit% <> 4% then L10240 /* Previous Field */
L10170:                   fieldnr% = max(1%, fieldnr% - 1%)

                          gosub'051(fieldnr%)
                               if enabled% = 1% then L10140
                               if fieldnr% = 1% then L10110
                               goto L10170

L10240:              if keyhit% = 16% and fieldnr% = 1% then exit_sub
                     if keyhit% <> 0%           then L10140

L10270:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " "        then L10140

            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit% =  1%       then gosub startover
                if keyhit% =  7%       then gosub dataput
                if keyhit% = 12%       then gosub data_delete
                if keyhit% = 14%       then gosub dataput
                if keyhit% = 16%       then exit_sub
                if keyhit% <> 0%       then editpg1

L11150:     fieldnr% = 1%
            if fieldnr% = lastfieldnr% then editpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% = 0%       then editpg1

L11210:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit% =  1%       then gosub startover
                if keyhit% <> 0%       then L11210

L11250:     gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " "    then L11210
                lastfieldnr% = fieldnr%
            goto L11150

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************
        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, rga_reason$, rga_compt$,   ~
                rga_dept$, rga_part$, rga_dt_txt$, rga_so$, rga_line$,   ~
                rga_piece$, rga_qty$, rga_po$, rga_load$, rga_inv$,      ~
                rga_chk$, rga_credit$, rga_hows$, rga_salesman$,         ~
                rga_prod_dte$, rga_gl_acct$, rga_gl_posted$, or_inv$,    ~
                rga_pickup_load$(), rga_pickup_dte$(), rga_dt_userid$,   ~
                rga_dt_mod_dte$, rga_dt_filler$, or_so$, or_hows$,       ~
                or_chk$, or_load$, or_po$, bck_text$, bck_part$,         ~
                status_desc$, reason_desc$, dept_desc$, part_desc$,      ~
/*PAR000*/      dt_date$, bck_so$, dt_bar$, dt_load$, rga_dt_subp$

            rga_dt_status$ = "10"
            rga_frt_cost   = 0.00 : rga_mat_cost    = 0.00
            rga_labor_cost = 0.00 : rga_overhd_cost = 0.00
            rga_trans_cost = 0.00 : rga_vinyl_disc  = 0.00
            rga_credit     = 0.00
            credit         = round(rga_credit, 2)
            convert credit to rga_credit$, pic(###0.00-)

            init(hex(ff)) rga_dt_desc_txt$, text$()
            call "TXTFUTIL" (#5, f2%(5), "INTL", rga_dt_desc_txt$)

            edit% = 0% : debug% = 0%
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
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
        dataload
            rec% = 0%
            str(dtlkey$,1%,4%) = rga_number$
            str(dtlkey$,5%,2%) = rga_item$

        dataread
            read #2,key = dtlkey$, eod goto L30660

            get #2, using L35030,                                         ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_dt_status$,     /* RGA Item Status            */~
                     rga_number$,        /* RGA Number                 */~
                     rga_item$,          /* RGA Item                   */~
/* (EWD002) */   str(rga_compt$,1%,4%),  /* RGA Complaint No.          */~
                     rga_reason$,        /* RGA Reason Code            */~
                     rga_dept$,          /* RGA Department Code        */~
                     rga_part$,          /* RGA Part No.               */~
                     rga_so$,            /* RGA Sales Order No.        */~
                     rga_line$,          /* RGA S.O. Line Item         */~
                     rga_piece$,         /* RGA Line Item Piece Count  */~
                     rga_qty$,           /* RGA Line Item Quantity     */~
                     rga_po$,            /* RGA Purchase Order No.     */~
                     rga_load$,          /* RGA Load No.               */~
                     rga_inv$,           /* RGA S.O. Invoice No.       */~
                     rga_chk$,           /* RGA Invoice Check No.      */~
                     rga_credit,         /* RGA Credit Amount          */~
                     rga_mat_cost,       /* RGA Item Material Cost     */~
                     rga_labor_cost,     /* RGA Item Labor Cost        */~
                     rga_overhd_cost,    /* RGA Item Overhead Cost     */~
                     rga_trans_cost,     /* RGA Item Transport. Cost   */~
                     rga_frt_cost,       /* RGA Freight Cost           */~
                     rga_vinyl_disc,     /* RGA Vinyl Discount Cost    */~
                     rga_hows$,          /* RGA How Shipped Code       */~
                     rga_prod_dte$,      /* RGA Production Date        */~
                     rga_gl_acct$,       /* RGA General Legder Account */~
                     rga_gl_posted$,     /* RGA G/L Posted Flag        */~
                     rga_pickup_load$(), /* RGA Pickup Load No. (3)    */~
                     rga_pickup_dte$(),  /* RGA Pickup Date     (3)    */~
                     rga_salesman$,      /* RGA Salesman               */~
                     rga_dt_desc_txt$,   /* RGA Item Description Code  */~
                     rga_dt_txt$,        /* RGA Item Text Flag         */~
                     rga_dt_userid$,     /* Userid of Item Entry/Mod   */~
                     rga_dt_mod_dte$,    /* RGA Item Entry/Mod Date    */~
/*PAR000*/           rga_dt_subp$,       /* RGA Item Subpart           */~
                     rga_dt_filler$      /* APCRGADT Filler Area       */

            if rga_prod_dte$     <> " " and rga_prod_dte$ <> blankdate$ then L30510
            goto L30530
L30510:     call "DATEFMT" (rga_prod_dte$)

L30530:     if rga_pickup_dte$(1) <> " " and rga_pickup_dte$(1) <> blankdate$ then L30540
            goto L30560
L30540:     call "DATEFMT" (rga_pickup_dte$(1))

L30560:     if rga_pickup_dte$(2) <> " " and rga_pickup_dte$(2) <> blankdate$  then L30570
            goto L30590
L30570:     call "DATEFMT" (rga_pickup_dte$(2))

L30590:     if rga_pickup_dte$(3) <> " " and rga_pickup_dte$(3) <> blankdate$ then L30600
            goto L30620
L30600:     call "DATEFMT" (rga_pickup_dte$(3))

L30620:     credit = round(rga_credit,2)
            convert credit to rga_credit$, pic(###0.00-)

            gosub unpack_comp_number                  /* (EWD002) */
            rec% = 1%
L30660:     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        data_delete
            if userid$        = "JBF" then dataput
            if rga_dt_status$ = "10"  then dataput
            call "SHOSTAT" ( "ERROR - Unable to Delete APCRGADT" ) : stop

        return

        dataput
            call "SHOSTAT" ( "Updating APCRGADT Data" )

            str(dtlkey$,1%,4%) = rga_number$
            str(dtlkey$,5%,2%) = rga_item$
            read #2,hold,key = dtlkey$, eod goto L31210

            delete #2

L31210:     if keyhit% <> 12%           then L31250
                gosub delete_text

                goto L32080
L31250:     if rga_prod_dte$ <> " " and rga_prod_dte$ <> blankdate$ then L31260
            goto L31280
L31260:     call "DATUNFMT" (rga_prod_dte$)

L31280:     if rga_pickup_dte$(1) <> " " and rga_pickup_dte$(1) <> blankdate$ then L31290
            goto L31310
L31290:     call "DATUNFMT" (rga_pickup_dte$(1))

L31310:     if rga_pickup_dte$(2) <> " " and rga_pickup_dte$(2) <> blankdate$ then L31320
            goto L31340
L31320:     call "DATUNFMT" (rga_pickup_dte$(2))

L31340:     if rga_pickup_dte$(3) <> " " and rga_pickup_dte$(3) <> blankdate$ then L31350
            goto L31370
L31350:     call "DATUNFMT" (rga_pickup_dte$(3))

L31370:     convert rga_credit$ to rga_credit, data goto L31390

L31390:     if rga_mat_cost >  0        then L31420
            if rga_part$   <> " "       then gosub rga_cost
L31420:     rga_dt_userid$  = userid$
            str(rga_dt_mod_dte$,1%,6%) = date
            gosub pack_comp_number                    /* (EWD002) */
            
            put #2, using L35030,                                         ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_dt_status$,     /* RGA Item Status            */~
                     rga_number$,        /* RGA No.                    */~
                     rga_item$,          /* RGA Item                   */~
/* (EWD002) */   str(rga_compt$,1%,4%),  /* RGA Complaint No.          */~
                     rga_reason$,        /* RGA Reason Code            */~
                     rga_dept$,          /* RGA Department Code        */~
                     rga_part$,          /* RGA Part No.               */~
                     rga_so$,            /* RGA Sales Order No.        */~
                     rga_line$,          /* RGA S.O. Line Item         */~
                     rga_piece$,         /* RGA Line Item Piece Count  */~
                     rga_qty$,           /* RGA Line Item Quantity     */~
                     rga_po$,            /* RGA Purchase Order No.     */~
                     rga_load$,          /* RGA Load No.               */~
                     rga_inv$,           /* RGA S.O. Invoice No.       */~
                     rga_chk$,           /* RGA Invoice Check No.      */~
                     rga_credit,         /* RGA Credit Amount          */~
                     rga_mat_cost,       /* RGA Item Material Cost     */~
                     rga_labor_cost,     /* RGA Item Labor Cost        */~
                     rga_overhd_cost,    /* RGA Item Overhead Cost     */~
                     rga_trans_cost,     /* RGA Item Transport. Cost   */~
                     rga_frt_cost,       /* RGA Freight Cost           */~
                     rga_vinyl_disc,     /* RGA Vinyl Discount Cost    */~
                     rga_hows$,          /* RGA How Shipped Code       */~
                     rga_prod_dte$,      /* RGA Production Date        */~
                     rga_gl_acct$,       /* RGA General Legder Account */~
                     rga_gl_posted$,     /* RGA G/L Posted Flag        */~
                     rga_pickup_load$(), /* RGA Pickup Load No. (3)    */~
                     rga_pickup_dte$(),  /* RGA Pickup Date     (3)    */~
                     rga_salesman$,      /* RGA Salesman               */~
                     rga_dt_desc_txt$,   /* RGA Item Description Code  */~
                     rga_dt_txt$,        /* RGA Item Text Flag         */~
                     rga_dt_userid$,     /* Userid of Item Entry/Mod   */~
                     rga_dt_mod_dte$,    /* RGA Item Entry/Mod Date    */~
/*PAR000*/           rga_dt_subp$,       /* RGA Item Subpart           */~
                     rga_dt_filler$      /* APCRGADT Filler Area       */

            write #2, eod goto L32100

            if rga_pickup_dte$(1) <> " " and rga_pickup_dte$(1) <> blankdate$ then L31850
            goto L31870
L31850:     call "DATEFMT" (rga_pickup_dte$(1))

L31870:     if rga_pickup_dte$(2) <> " " and rga_pickup_dte$(2) <> blankdate$ then L31880
            goto L31900
L31880:     call "DATEFMT" (rga_pickup_dte$(2))

L31900:     if rga_pickup_dte$(3) <> " " and rga_pickup_dte$(3) <> blankdate$ then L31910
            goto L31930
L31910:     call "DATEFMT" (rga_pickup_dte$(3))

L31930:     gosub update_complaint

            if proc%         = 0%  then L32080
                convert rga_item$  to rga_item%, data goto L31980

L31980:         if rga_item% = 99% then L32040
                rga_item%    = rga_item% + 1%
                convert rga_item%  to rga_item$, pic(00)

                if keyhit%   = 7%  then L32060

L32040: return clear all
        goto inputmode
L32060: return clear all
        goto editpg1
L32080: return clear all
        goto exit_sub
L32100:     call "SHOSTAT" ( "ERROR - Unable to Update APCRGADT"  ) : stop

        return

        update_complaint                   /* (EWD002)  */
            read   #7,key = comp_key$, using L32170, comp_rec$,          ~
                eod goto L32310
L32170:         FMT CH(150)

            if str(comp_rec$,32%,8%) > " "     then L32310
            read   #7,hold,key = comp_key$, eod goto L32310

            delete #7

            str(comp_rec$, 32%,4%) =  rga_number$
            str(comp_rec$,135%,3%) =  userid$
            str(comp_rec$,138%,6%) =  date$
            put    #7, using L32170, comp_rec$

            write  #7

L32310: return

        edit_text
            gosub'099(rga_dt_desc_txt$)
            if txt% = 0% then L32380
                call "TXTFUTIL" (#5, f2%(5), "LOAD", rga_dt_desc_txt$)

L32380:     header$ = "Edit Detail TEXT for " & rga_number$ & rga_item$
            call "TXTINSUB" (#5, f2%(5), "RGD", header$,                 ~
                rga_dt_desc_txt$, text$() )

            gosub detail_text_edit
            if edit% <> 2% then fieldnr% = fieldnr% + 1%
            if txt%  =  0% then return
                call "TXTFUTIL" (#5, f2%(5), "SAV2", rga_dt_desc_txt$)

        return

        delete_text
            call "SHOSTAT" ("Deleting Text for RGA (" & dtlkey$&")")
            call "TXTFUTIL" (#5, f2%(5), "DELE", rga_dt_desc_txt$)

        return

        REM *************************************************************~
            *               F O R M A T    S T A T E M E N T S          *~
            *************************************************************
L35030:     FMT CH(09),                  /* Customer No.               */~
                CH(02),                  /* RGA Item Status            */~
                CH(04),                  /* RGA No.                    */~
                CH(02),                  /* RGA Item No.               */~
                CH(05),                  /* RGA Complaint No.          */~
                CH(03),                  /* RGA Reason Code            */~
                CH(03),                  /* RGA Department Code        */~
                CH(25),                  /* RGA Part No.               */~
                CH(08),                  /* RGA Sales Order No.        */~
                CH(02),                  /* RGA S.O. Line Item         */~
                CH(04),                  /* RGA Line Item Piece Count  */~
                CH(04),                  /* RGA Line Item Quantity     */~
                CH(16),                  /* RGA Purchase Order No.     */~
                CH(05),                  /* RGA Load No.               */~
                CH(08),                  /* RGA S.O. Invoice No.       */~
                CH(08),                  /* RGA Invoice Check No.      */~
                PD(14,4),                /* RGA Credit Amount          */~
                PD(14,4),                /* RGA Item Material Cost     */~
                PD(14,4),                /* RGA Item Labor Cost        */~
                PD(14,4),                /* RGA Item Overhead Cost     */~
                PD(14,4),                /* RGA Item Transport. Cost   */~
                PD(14,4),                /* RGA Freight Cost           */~
                PD(14,4),                /* RGA Vinyl Discount Cost    */~
                CH(02),                  /* RGA How Shipped Code       */~
                CH(08),                  /* RGA Production Date        */~
                CH(09),                  /* RGA General Legder Account */~
                CH(01),                  /* RGA G/L Posted Flag        */~
                3*CH(05),                /* RGA Pickup Load No. (3)    */~
                3*CH(08),                /* RGA Pickup Date     (3)    */~
                CH(04),                  /* RGA Salesman               */~
                CH(04),                  /* RGA Item Description Code  */~
                CH(01),                  /* RGA Item Text Flag         */~
                CH(03),                  /* Userid of Item Entry/Mod   */~
                CH(08),                  /* RGA Item Entry/Mod Date    */~
/*PAR000*/      CH(20),                  /* RGA Item Subpart           */~
/*PAR000*/      CH(249)                  /* APCRGADT Filler Area       */

L35390:     FMT POS(36), CH(16),         /* APCPLNOR P.O. No.          */~
                CH(08),                  /* APCPLNOR S.O. No.          */~
                XX(10),  CH(08),         /* APCPLNOR Invoice No.       */~
                CH(08),                  /* APCPLNOR Check No.         */~
                XX(06),  CH(02),         /* APCPLNOR How shipped code  */~
                CH(05)                   /* APCPLNOR Load No.          */

L35460:     FMT POS(10), CH(16),         /* BCKLINES S.O. No.          */~
                XX(06),  CH(25),         /* BCKLINES Part No.          */~
                XX(186), CH(04)          /* BCKLINES Text ID           */

L35500:     FMT CH(05),                  /* APCPLNDT Load              */~
                XX(18), CH(18),          /* APCPLNDT Barcode           */~
                CH(03),                  /* APCPLNDT Department Code   */~
                CH(02),                  /* APCPLNDT Process           */~
                CH(06),                  /* APCPLNDT Production Date   */~
                XX(43),  CH(08),         /* APCPLNDT Warranty No.      */~
                XX(37),  PD(14,4),       /* APCPLNDT Material Cost     */~
                PD(14,4),                /* APCPLNDT Labor Cost        */~
                PD(14,4),                /* APCPLNDT Overhead Cost     */~
                PD(14,4),                /* APCPLNDT Freight Cost      */~
                PD(14,4),                /* APCPLNDT Vinyl Discount    */~
                XX(08),  CH(25)          /* APCPLNDT Part No.          */

L35620:     FMT POS(10), CH(16),         /* BCKMASTR S.O. No.          */~
                CH(16)                   /* BCKMASTR P.O. No.          */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)
        start_screen
            inpmessage$ = edtmessage$
            gosub set_pf1

            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% > 0% then gosub L40150
            goto accept_screen
                lfac$(fieldnr%) = hex(80)  :  return   /* Up / Low   */
L40150:         lfac$(fieldnr%) = hex(81)  :  return   /* Upper Only */
                lfac$(fieldnr%) = hex(82)  :  return   /* Numeric    */

        accept_screen
            accept                                                       ~
                at (01,02),                                              ~
                   "RGAII Tracking Detail Entry/Edit"           ,        ~
                                                                         ~
                at (01,66), "Today:"                            ,        ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                                                                         ~
                at (02,02), fac(hex(94)),   errormsg$           , ch(60),~
                at (02,63), fac(hex(8c)),   progid$             , ch(18),~
                                                                         ~
                at (03,02), "RGA No.    :"                      ,        ~
                at (03,15), fac(hex(84)),   rga_number$         , ch(04),~
                at (03,27), "RGA Item:"                         ,        ~
                at (03,37), fac(hex(84)),   rga_item$           , ch(02),~
                                                                         ~
                at (04,02), "Item Status:"                      ,        ~
                at (04,15), fac(hex(84)),   rga_dt_status$      , ch(02),~
                at (04,19), fac(hex(84)),   status_desc$        , ch(30),~
                                                                         ~
                at (05,02), "Warranty   :"                      ,        ~
                at (05,15), fac(lfac$(1%)), warranty$           , ch(08),~
                at (05,26), "Complaint:"                        ,        ~
                at (05,37), fac(lfac$(1%)), rga_compt$          , ch(08),~
                                                                         ~
                at (06,02), "Part No.   :"                      ,        ~
                at (06,15), fac(lfac$(1%)), rga_part$           , ch(25),~
/*PAR000*/      at (06,42), fac(lfac$(1%)), rga_dt_subp$        , ch(20),~                
/*PAR000*/      at (07,42), fac(hex(84)),   part_desc$          , ch(32),~
                                                                         ~
                at (07,02), "Reason     :"                      ,        ~
                at (07,15), fac(lfac$(1%)), rga_reason$         , ch(03),~
/*PAR000*/      at (07,20), fac(hex(84)),   reason_desc$        , ch(20),~
                                                                         ~
                at (08,02), "Department :"                      ,        ~
                at (08,15), fac(lfac$(1%)), rga_dept$           , ch(03),~
                at (08,20), fac(hex(84)),   dept_desc$          , ch(32),~
                                                                         ~
                at (09,02), "S.O. No.   :"                      ,        ~
                at (09,15), fac(lfac$(1%)), rga_so$             , ch(08),~
                at (09,26), "S.O. Line:"                        ,        ~
                at (09,37), fac(lfac$(1%)), rga_line$           , ch(02),~
                at (09,43), "Piece:"                            ,        ~
                at (09,50), fac(lfac$(1%)), rga_piece$          , ch(04),~
                at (09,59), "Quantity:"                         ,        ~
                at (09,69), fac(lfac$(1%)), rga_qty$            , ch(04),~
                                                                         ~
                at (10,02), "P.O. No.   :"                      ,        ~
                at (10,15), fac(lfac$(1%)), rga_po$             , ch(16),~
                                                                         ~
                at (11,02), "Load No.   :"                      ,        ~
                at (11,15), fac(lfac$(1%)), rga_load$           , ch(05),~
                                                                         ~
                at (12,02), "Invoice No.:"                      ,        ~
                at (12,15), fac(lfac$(1%)), rga_inv$            , ch(08),~
                                                                         ~
                at (13,02), "Check No.  :"                      ,        ~
                at (13,15), fac(lfac$(1%)), rga_chk$            , ch(08),~
                                                                         ~
                at (14,02), "Credit Amt.:"                      ,        ~
                at (14,15), fac(lfac$(1%)), rga_credit$         , ch(09),~
                                                                         ~
                at (15,02), "Item Text  :"                      ,        ~
                at (15,15), fac(lfac$(1%)), rga_dt_txt$         , ch(01),~
                                                                         ~
                at (17,02), "Pick Up    : Date -"               ,        ~
                at (17,22), fac(lfac$(1%)), rga_pickup_dte$(1)  , ch(08),~
                at (17,34), "Load -"                            ,        ~
                at (17,42), fac(lfac$(1%)), rga_pickup_load$(1) , ch(05),~
                                                                         ~
                at (18,02), "Attempt            "               ,        ~
                at (18,22), fac(lfac$(1%)), rga_pickup_dte$(2)  , ch(08),~
                at (18,42), fac(lfac$(1%)), rga_pickup_load$(2) , ch(05),~
                                                                         ~
                at (19,22), fac(lfac$(1%)), rga_pickup_dte$(3)  , ch(08),~
                at (19,42), fac(lfac$(1%)), rga_pickup_load$(3) , ch(05),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 5% then L41060
                     tab_1% = 1%
                     gosub lookup_codes

                     goto start_screen
L41060:         if keyhit% <> 6% then L41110
                     tab_1% = 2%
                     gosub lookup_codes

                     goto start_screen
L41110:         if keyhit% = 8% and cursor%(1%) = 15% then               ~
                     gosub edit_text

                if keyhit% <>  9% then L41180
                     if userid$ = "JBF" then debug% = 1%
                     goto start_screen

L41180:         if keyhit% <> 10% then L41210
                     if userid$ = "JBF" then gosub recalc_cost

L41210:         if keyhit% <> 15% then L41250
                     call "PRNTSCRN"

                     goto accept_screen
L41250:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            return

        set_pf1
        if edit% = 2% then L41440     /*  Input Mode             */
            pf$(1) = "(1)Start Over     (5)Dsp. Reason Codes   " &       ~
                     "                                      "
            pf$(2) = "                  (6)Dsp. Dept. Codes    " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                      (16)Return      "
            pfkeys$ = hex(01ffffff0506ffffffffffffffff0f1000)

            if fieldnr% = 1% then L41420
              str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L41420:     return

L41440: if fieldnr% > 0% then L41600  /*  Edit Mode - Select Field */
            pf$(1) = "(1)Start Over     (5)Dsp. Reason Codes   " &       ~
                     "(8)Add/Edit Text      (14)Update Data "
            pf$(2) = "                  (6)Dsp. Dept. Codes    " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                  (7)Dup RGA Line Item   " &       ~
                     "(12)Delete            (16)Return      "
            pfkeys$ = hex(01ffffff05060708090aff0cff0e0f1000)

            if rec%  <> 0% then L41550
              str(pf$(3%),42%,20%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41550:     if proc% <> 0% then L41570
              str(pf$(3%),19%,21%) = " " : str(pfkeys$,7%,1%)  = hex(ff)
L41570:     return

                                     /*  Edit Mode - Enabled    */
L41600:     pf$(1) = "(1)Start Over     (5)Dsp. Reason Codes   " &       ~
                     "(8)Add/Edit Text                      "
            pf$(2) = "                  (6)Dsp. Dept. Codes    " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                                      "
            pfkeys$ = hex(01ffffff0506ff08ffffffffffff0fff00)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        deffn'151(fieldnr%)
            errormsg$ = " "
            gosub field_edits

        return

        field_edits
        REM RGA No.                             RGA_NUMBER$
            if edit% = 2%           then L50180
            gosub dataload
                if errormsg$ <> " " then return

        REM Item Status                         RGA_DT_STATUS$
L50180:     gosub lookup_status
                if errormsg$ <> " " then return

        REM Warranty                            WARRANTY$
            if warranty$ <> " "     then L50230
            goto L50400
L50230:     gosub lookup_apcplndt4

            if dt_bar$      = " "   then L50400
            rga_so$         = str(dt_bar$, 1%,8%)
            rga_line$       = str(dt_bar$, 9%,2%)
            rga_piece$      = str(dt_bar$,11%,4%)
            rga_qty$        = str(dt_bar$,15%,4%)
            rga_part$       = dt_part$
            rga_prod_dte$   = dt_date$
            rga_mat_cost    = dt_mat
            rga_labor_cost  = dt_labor
            rga_overhd_cost = dt_over
            rga_frt_cost    = dt_frght
            rga_vinyl_disc  = dt_vdisc
            gosub lookup_apcplnor4

        REM RGA Complaint No.                   RGA_COMPT$
L50400:     if rga_compt$ <> " "    then L50410
            goto L50520
L50410:     convert rga_compt$ to rga_compt%, data goto L50490

            convert rga_compt% to rga_compt$, pic(00000000)

            gosub lookup_complaint
                if errormsg$ <> " " then return

            goto L50520
L50490:     errormsg$ = "(Error) - Invalid COMPLAINT NO. ?" : return

        REM RGA Part No.                        RGA_PART$
L50520:     if rga_part$ <> " "     then L50550
                init(" ") part_desc$
                goto reason_edit
L50550:     gosub lookup_part
                if errormsg$ <> " " then return

        REM Reason Code                         RGA_REASON$
        reason_edit
            if rga_reason$ <> " "   then L50630
                init(" ") reason_desc$
                errormsg$ = "(Error) - Invalid REASON Code ?" : return
L50630:     gosub lookup_reason
                if errormsg$ <> " " then return

        REM RGA Department Code                 RGA_DEPT$
            if rga_dept$ <> " "     then so_edit
                init(" ") dept_desc$

        REM RGA Sales Order No.                 RGA_SO$
        so_edit
            if rec% = 1%            then L50800
            if rga_so$   <> " "     then L50730
            goto L50800
L50730:     convert rga_so$     to rga_so%, data goto L50960

            convert rga_so%     to rga_so$, pic(00000000)

            if dt_bar$   <> " "     then L50800
            gosub lookup_apcplnor4

L50800:     if rga_line$ <> " "     then L50810
            goto L50850
L50810:     convert rga_line$   to rga_line%,  data goto L50970

            convert rga_line%   to rga_line$,  pic(00)

L50850:     if rga_piece$ <> " "    then L50860
            goto L50900
L50860:     convert rga_piece$  to rga_piece%, data goto L50980

            convert rga_piece%  to rga_piece$, pic(0000)

L50900:     if rga_qty$  <> " "     then L50910
            goto po_no_edit
L50910:     convert rga_qty$    to rga_qty%,   data goto L50990

            convert rga_qty%    to rga_qty$,   pic(0000)

            goto po_no_edit
L50960:     errormsg$   = "(Error) - Invalid S.O. NO. ?"      : return
L50970:     errormsg$   = "(Error) - Invalid S.O. LINE ?"     : return
L50980:     errormsg$   = "(Error) - Invalid S.O. PIECE ?"    : return
L50990:     errormsg$   = "(Error) - Invalid S.O. QUANTITY ?" : return

        REM RGA Purchase Order No.              RGA_PO$
        po_no_edit
            if rga_so$ <> " "       then load_no_edit
            if rga_po$ <> " "       then L51055
            errormsg$ = "(Error) - Warranty or S.O. or P.O. ?" : return
L51055:     if rec% = 1%            then load_no_edit
            gosub lookup_apcplnor1
                if errormsg$ <> " " then return

        REM RGA Load No.                        RGA_LOAD$
        load_no_edit
            if rga_load$ <> " "     then L51120
            goto inv_no_edit
L51120:     convert rga_load$  to rga_load%, data goto L51170

            convert rga_load%  to rga_load$, pic(00000)

/* (AWD004) */
L51170:     convert str(rga_load$,2,4) to rga_load%, data goto L51175

            convert rga_load% to str(rga_load$,2,4), pic(0000)
/* (AWD004) end */
            goto inv_no_edit
L51175:     errormsg$ = "(Error) - Invalid LOAD NO. ?" : return

        REM RGA Invoice No.                     RGA_INV$
        inv_no_edit
            if rga_inv$ <> " "       then L51220
            goto chk_no_edit
L51220:     convert rga_inv$  to rga_inv%, data goto L51270

            convert rga_inv%  to rga_inv$, pic(00000000)

            goto chk_no_edit
L51270:     errormsg$ = "(Error) - Invalid INVOICE NO. ?" : return

        REM RGA Check No.                       RGA_CHK$
        chk_no_edit

        REM RGA Credit Amount                   RGA_CREDIT
        REM CREDIT_AMT_EDIT
            convert rga_credit$ to rga_credit, data goto L51420

            if rga_reason$ = "11" then L51390
            if proc%       =  1%  then gosub lookup_price

L51390:     convert rga_credit  to rga_credit$, pic(###0.00-)

            goto detail_text_edit
L51420:     errormsg$ = "(Error) - Invalid CREDIT AMOUNT ?" : return

        REM RGA Detail Text Flag                RGA_DT_TXT$
        detail_text_edit
            rga_dt_txt$ = "N"
            gosub'099(rga_dt_desc_txt$)

            if txt% = 1%            then rga_dt_txt$ = "Y"

        REM RGA Pickup Attempt Date             RGA_PICKUP_DTE$()
            for d% = 1% to 3%
                if rga_pickup_dte$ (d%) <> " " and rga_pickup_dte$(d%) <> blankdate$ then L51550
                if rga_pickup_load$(d%) <> " " then L51550
                goto L51610
L51550:         call "DATEOK" (rga_pickup_dte$(d%), date%, errormsg$)

                if date% <> 0%      then L51610
                     init(" ") rga_pickup_dte$(d%)
                     d% = 3%

L51610:     next d%

                if errormsg$ <> " " then return

        REM RGA Pickup Attempt Load No.         RGA_PICKUP_LOAD$()
            for l% = 1% to 3%
                if rga_pickup_load$(l%) <> " " then L51690
                goto L51790
L51690:         convert rga_pickup_load$(l%)  to rga_pickup_load%,       ~
                     data goto L51760

                convert rga_pickup_load% to rga_pickup_load$(l%),        ~
                     pic(00000)

                goto L51790
L51760:             errormsg$ = "(Error) - Invalid PICKUP LOAD NO. ?"
                    l% = 3%

L51790:     next l%

                if errormsg$ <> " " then return

        REM RGA Salesman                        RGA_SALESMAN$
            gosub lookup_salesman

        return

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************
        lookup_part                           /* Check HNYMASTR        */
            init(" ") part_desc$, apc_prt$, apc_sze$, apc_scr$, dt_flag$
            read #9,key = rga_part$, using L60035, part_desc$, apc_prt$,  ~
                apc_sze$, eod goto L60050
L60035:         FMT XX(25), CH(32), POS(606), CH(60), CH(20)

            goto L60120
L60050:     err% = 0%
            if len(rga_part$) > 18% then L60090
                part_desc$ = "COMPONENT PART"
                gosub lookup_text

                if dt_flag$ = "Y"   then part_desc$ = text_desc$
                goto L60120

L60090:     call "APCDESCR" (rga_part$, apc_scr$, apc_prt$, apc_sze$,    ~
                             #11, err%)

            if err% <> 0%           then L60125
            str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
            str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)
L60120: return
L60125:     errormsg$ = "(Error) - Invalid PART No. Lookup?"
        return

        lookup_status
            init(" ") readkey$
            str(readkey$,1%,9%)   = "APC  RGA1"
            str(readkey$,10%,15%) =  rga_dt_status$
            read #3,key = readkey$, using L60170, status_desc$,           ~
                eod goto L60185
L60170:         FMT POS(25), CH(30)

        return
L60185:     errormsg$ = "(Error) - Invalid STATUS Lookup?"
            init(" ") status_desc$
        return

        lookup_reason
            init(" ") readkey$
            str(readkey$,1%,9%)   = "COMPLAINT"
            str(readkey$,10%,15%) =  rga_reason$
            read #3,key = readkey$, using L60235, reason_desc$,           ~
                eod goto L60250
L60235:         FMT POS(25), CH(30)

        return
L60250:     errormsg$ = "(Error) - Invalid REASON Lookup?"
            init(" ") reason_desc$
        return

        lookup_apcplndt4
            init(" ") dt_key4$
            dt_key4$ = warranty$
            read #14,key 4% = dt_key4$, using L35500, dt_load$, dt_bar$,  ~
                dt_dept$, dt_proc$, dt_date$, dt_ref$, dt_mat, dt_labor,  ~
                dt_over,  dt_frght, dt_vdisc, dt_part$, eod goto L60410

            gosub lookup_department

            if rga_dept$ > " "                   then L60410
            init(" ") rga_dept$, dept_desc$, dt_key$
            str(dt_key$, 1%,18%) = dt_bar$
            str(dt_key$,19%, 5%) = " "

        next_apcplndt4
            read #14,key > dt_key$, using L35500, dt_load$, dt_bar$,      ~
                dt_dept$, dt_proc$, dt_date$, dt_ref$, dt_mat, dt_labor,  ~
                dt_over,  dt_frght, dt_vdisc, dt_part$, eod goto L60410

            if dt_bar$ <> str(dt_key$,1%,18%)    then L60410
            gosub lookup_department

            if rga_dept$ > " "                   then L60410
            init(" ") rga_dept$, dept_desc$, dt_key$
            str(dt_key$, 1%,18%) = dt_bar$
            str(dt_key$,19%, 3%) = dt_dept$
            str(dt_key$,22%, 2%) = dt_proc$
            goto next_apcplndt4
L60410: 
        so_inv$  = str(dt_bar$,1%,8%)
        item_no$ = str(dt_bar$,9%,2%)
        gosub lookup_bcksubpt
        rga_dt_subp$ = str(bcksubpt_rec$,48%,20%)
       return

        lookup_department
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN SUPP"
            str(readkey$,10%,15%) =  dt_dept$
            read #3,key = readkey$, using L60450, dept_desc$,             ~
                eod goto L60465
L60450:         FMT POS(25), CH(30)

            goto L60500
L60465:     rga_dept$             =  dt_dept$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) =  dt_dept$
            read #3,key = readkey$, using L60490, dept_desc$,             ~
                eod goto L60500
L60490:         FMT POS(25), CH(30)

L60500: return

        lookup_complaint                      /* (EWD002)  */
            gosub pack_comp_number
            read #7,key = comp_key$, using L60525, comp_code$,           ~
                eod goto L60545
L60525:         FMT POS(82), CH(03)

            rga_reason$ = comp_code$
            gosub unpack_comp_number
        return
L60545:     errormsg$ = "(Error) - Invalid COMPLAINT NO. ?"
            init(" ") rga_compt$
        return

        lookup_apcplnor4
            init(" ") or_key4$
            or_key4$ = rga_so$
            read #6,key 4% = or_key4$, eod goto check_bckmastr_so
            get #6, using L35390,                                         ~
                or_po$,                   /* APCPLNOR P.O. No.          */~
                or_so$,                   /* APCPLNOR S.O. No.          */~
                or_inv$,                  /* APCPLNOR Invoice No.       */~
                or_chk$,                  /* APCPLNOR Check No.         */~
                or_hows$,                 /* APCPLNOR How shipped code  */~
                or_load$                  /* APCPLNOR Load No.          */

            rga_po$   = or_po$
            rga_load$ = or_load$
            rga_inv$  = or_inv$
            rga_chk$  = or_chk$
            rga_hows$ = or_hows$
            rga_load$ = or_load$
            gosub lookup_bcklines

/*PAR000*/  so_inv$     = rga_so$
/*PAR000*/  item_no$    = rga_line$
/*PAR000*/  gosub lookup_bcksubpt
/*PAR000*/  rga_dt_subp$ = str(bcksubpt_rec$,48%,20%)

        return

        lookup_apcplnor1
            init(" ") or_key1$
            str(or_key1$, 1%, 9%) = rga_cuscode$
            str(or_key1$,10%,16%) = rga_po$
            read #6,key 1% = or_key1$, eod goto check_bckmastr_po
            get #6, using L35390,                                         ~
                or_po$,                   /* APCORDER P.O. No.          */~
                or_so$,                   /* APCORDER S.O. No.          */~
                or_inv$,                  /* APCORDER Invoice No.       */~
                or_chk$,                  /* APCORDER Check No.         */~
                or_hows$,                 /* APCORDER How shipped code  */~
                or_load$                  /* APCORDER Load No.          */

            rga_so$     = or_so$
            rga_po$     = or_po$
            rga_load$   = or_load$
            rga_inv$    = or_inv$
            rga_chk$    = or_chk$
            rga_hows$   = or_hows$
            rga_load$   = or_load$
            gosub lookup_bcklines
/*PAR000*/  so_inv$     = rga_so$
/*PAR000*/  item_no$    = rga_line$
/*PAR000*/  gosub lookup_bcksubpt
/*PAR000*/  rga_dt_subp$ = str(bcksubpt_rec$,48%,20%)
        return

        check_bckmastr_so
            init(" ") mastr_key$
            str(mastr_key$, 1%,9%) = rga_cuscode$
            str(mastr_key$,10%,8%) = rga_so$
            read #13, key = mastr_key$, using L35620, bck_so$, bck_po$,   ~
                eod goto L60840

            rga_po$ = bck_po$
        return
L60840:     str(rga_so$,1%,1%) = "*"
	    if rga_po$ <> " " then str(rga_po$,1%,1%) = "*"
        return

        check_bckmastr_po
            init(" ") mastr_key1$
            mastr_key1$     = rga_po$
            read #13,key 1% = mastr_key1$, using L35620, bck_so$, bck_po$,~
                eod goto L60895

            rga_so$ = str(bck_so$,1%,8%)
        return
L60895:     str(rga_po$,1%,1%) = "*"
	    if rga_so$ <> " " then str(rga_so$,1%,1%) = "*"
        return

        lookup_bcklines
            bck_key$            = all(hex(20))
            str(bck_key$,1%,8%) = rga_so$
            convert rga_line$  to xx%, data goto L60930
L60930:
            convert xx%        to str(bck_key$,17%,3%), pic(###)

            read #12,key = bck_key$, using L35460, bck_so$, bck_part$,    ~
                bck_text$, eod goto L60980

            if rga_part$ <> " " then L60980
            rga_part$     = bck_part$
            gosub lookup_part

L60980: return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_desc$, textid$, text_key$, sav_key1$, atext$()
            textid$ = bck_text$
            gosub'099(textid$)

            if txt% = 0% then L61095
            text_key$            = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$            = text_key$
            read #10,key > text_key$, eod goto L61095

            get  #10, using L61065, text_key$, atext$()
L61065:         FMT CH(11), POS(64), 2*CH(70)

            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then L61095
            if atext$(1)  <> " " then text_desc$ = str(atext$(1),1%,60%) ~
                                 else text_desc$ = str(atext$(2),1%,60%)
            if text_desc$ <> " " then dt_flag$ = "Y"
L61095: return

        lookup_salesman
            read #4,key = rga_cuscode$, using L61120, rga_salesman$,      ~
                eod goto L61130
L61120:         FMT POS(714), CH(04)

L61130: return

        lookup_price
            if rga_so$ <> " "              then L61155
            goto L61280
L61155:     if str(rga_load$,1%,1%)  = "S" then L61280   /* SKIP STOCK */
            ord_disc = 0.0 : ln_disc = 0.0 : line_tot = 0.0
            init(" ") bck_key$
            str(bck_key$,1%,9%)  = rga_cuscode$
            str(bck_key$,10%,8%) = rga_so$
            read #13,key = bck_key$, using L61190, ord_disc,              ~
                eod goto L61280
L61190:         FMT POS(859), PD(14,4)

            init(" ") bck_key$
            str(bck_key$,1%,16%) = rga_so$
            convert rga_line$ to xx%, data goto L61215
L61215:
            convert xx%       to str(bck_key$,17%,3%), pic(###)

            read #12,key = bck_key$, using L61240, ord_price, ln_disc,    ~
                eod goto L61280
L61240:         FMT POS(165), 2*PD(14,4)

            ord_price  = round(ord_price * 1.0, 2)
            discamt    = round(ord_price * ln_disc * .01, 2)
            line_tot   = round(ord_price - discamt, 2)
            discamt    = round(line_tot  * ord_disc * .01, 2)
            line_tot   = round(line_tot  - discamt, 2)
            rga_credit = line_tot
L61280: return

        lookup_codes                   /* Load Data for Display Screen */
            on tab_1% gosub t_comp, t_dept
        return

        t_comp                             /* Lookup Reason Codes      */
            tab_hdr$ = " RGAII Reason Codes    "
            sav_key$ = "COMPLAINT"
            goto L61345
        t_dept                             /* Lookup Department Codes  */
            tab_hdr$ = " APC Dept. Codes       "
            sav_key$ = "PLAN DEPT"
L61345:     init(" ") readkey$
            str(readkey$,1%,9%) = sav_key$
            descr$ = hex(06) & tab_hdr$
            call "PLOWCODE" (#3, readkey$, descr$, 9%, .30, f1%(3))

        return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then L61405
            txt% = 1%
L61405: return

        rga_cost
            cuscode$ = rga_cuscode$
            part$    = rga_part$
/*PAR000*/  partno1$ = rga_dt_subp$
            gosub load_sale
            gosub calc_cost

            rga_mat_cost    = tt ( 8%)
            rga_labor_cost  = lab( 8%)
            rga_overhd_cost = lab( 9%)
            rga_trans_cost  = trn_amt
            rga_frt_cost    = tc (19%)
            rga_vinyl_disc  = tc (20%)
        return

        calc_cost
            mat lab      = zer : mat tc = zer
            mat apc_err% = zer : mat pc = zer
            width  = 0
            convert str(part$,13%,4%) to width, data goto L61510
L61510:
            kk%    = 0%
            convert str(part$,1%,3%)  to kk%,   data goto L61525
L61525:
            kk%    = kk% + 1%
            price  = 0%
            p_err% = 0%
            x_err% = 0%
            calc%  = 0%
            if debug% = 1%            then calc%  = 99%
            if len(part$)       < 19  then x_err% = 2%  /*PROD IS PART */
            if str(part$,1%,1%) = "4" then x_err% = 2%         /* PART */
            if width            =  0  then x_err% = 2%         /* PART */
            if x_err%          <>  0% then L61870

            if str(sale$(kk%),2%,1%) = "*" then calc% = 8% /*PRICE ONLY*/
            call "APCCST0B" ( calc%,     /* Calculation Method         */~
                              part$,     /* MFG Part Number            */~
/* PAR000 */                  partno1$,  /* MFG Sub part number        */~
                              0.0,       /* Cost Adjustment Dollars    */~
                              tmp$(),    /* Raw Mat'l Part Numbers     */~
                              tmc(),     /* Raw Mat'l Cut Inches in Dec*/~
                              tmct(),    /* Raw Mat'l Costs            */~
                              tmu%(),    /* Raw Mat'l Calc Unit of Meas*/~
                              tmd$(),    /* Raw Mat'l Descriptions     */~
                              tmuc(),    /* Raw Mat'l Unit Cost        */~
                              tmsi(),    /* Raw Mat'l Scrap Inches Dec */~
                              tmsc(),    /* Raw Mat'l Scrap Cost       */~
                              tmeq$(),   /* Calc Type and Equation No. */~
                              tmph$(),   /* Phantom Number             */~
                              tcnt%(),   /* Raw Mat'l Type Counts      */~
                              "A",       /* Labor Type (A) or (S)tand  */~
                              lab(),     /* Labor Costs (1 thru 10)    */~
                              avg_pay(), /* Avg Hourly Pay by Dept     */~
                              uph(),     /* Avg Units Per Manhour Dept */~
                              tc(),      /* Material Costs (1 thru 25) */~
                              tt(),      /* Total Cost Buckets         */~
                              rm_mat(),  /* Material Costs (1 thru 10) */~
                              rm_mats(), /* Mat'l Scrap Costs(1 thru 9)*/~
                              cuscode$,  /* Customer Code for Pricing  */~
                              pc(),      /* 35 Prices                  */~
                              price,     /* Calc. Price for Customer   */~
                              #21,       /*   (APCCUTEQ)               */~
                              #9,        /*   (HNYMASTR)               */~
                              #23,       /*   (HNYQUAN )               */~
                              #3,        /*   (GENCDSIN)               */~
                              #25,       /*   (AMTBOMCD)               */~
                              #27,       /*   (APCEMPLY)               */~
                              #28,       /*   (APCEQUAT)               */~
                              #29,       /*   (APCCSTHP)               */~
                              #30,       /*   (APCCSTLR)               */~
                              #31,       /*   (CPRPRICE)               */~
                              #4,        /*   (CUSTOMER)               */~
                              #33,       /*   (APCPCMST)               */~
                              #34,       /*   (APCSKUNO)               */~
                              #35,       /*   (APCPCMSK)               */~
                              #36,       /*   (APCPCMSD)               */~
                              #37,       /*   (APCCSTEX)               */~
                              #38,       /*   (APCSTOCK)               */~
                              #39,       /*   (APCPLNDP)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */

            if str(sale$(kk%),2%,1%) <> "*" then L61835
                x_err% = 3%
                p_err% = 0%
                goto L61870

L61835:     for i% = 1% to 20%
                if apc_err%(i%) = 0%        then L61860
                     p_err%     = i%
                     x_err%     = 1%

L61860:     next i%

L61870:     gosub store_cost                  /* Gather All MFG Costs  */

        return

        store_cost
            gosub calc_transport

            sls_price = price
            sls_qty   = 1 : qtyshp = 1
            trn_amt = 0.0
            mat cst = zer                    /* Total Vinyl and Misc.  */
                                             /* (Mat'l + Scrap) Cost   */
            tot_mat = round(tc(3%)  + tc(6%) + tc(9%) + tc(12%) + tc(15%)~
                          + tc(18%) + tc(21%) + tc(19%) - tc(20%), 4)
                                                  /*Include W/F Amount */
                                                  /*Include Freight    */
            if x_err% > 1% then gosub compute_cost
                                     /* Use Cost from Costing Errors   */
            tot_cst = round(tot_mat + lab(8%) + lab(9%), 4)
                                     /* Note - CST() are the Values    */
                                     /*   Assoc. with each Line Item   */
            cst(1%) = round(tot_mat * qtyshp, 2)  /*Total Material     */
            cst(2%) = round(lab(8%) * qtyshp, 2)  /*Total Dir/Ind Labor*/
            cst(3%) = round(lab(9%) * qtyshp, 2)  /*Total Overhead Cost*/
            cst(4%) = round(tc(19%) * qtyshp, 2)  /*Total Freight Cost */
            cst(5%) = round(tc(20%) * qtyshp, 2)  /*Total Vinyl Disc't */
            cst(6%) = round(tot_cst * qtyshp, 2)  /*Total MFG Cost     */
            cst(7%) = round(trn_amt * qtyshp, 2)  /*Total Trans Cost   */
            cst(8%) = round(sls_price, 2)         /*Total Price W/Disc */
            cst(9%) = round(sls_qty, 2)           /*Total Quantity     */
        return

        calc_transport
            gencdkey$ = " " : sls_regn% = 0% : trn_amt = 0.0
            str(gencdkey$,1%,9%)   = "COST TRAN"
            str(gencdkey$,10%,15%) = cuscode$
            read #3,key = gencdkey$, using L62055, descr$, eod goto L62075
L62055:         FMT POS(25), CH(30)

            convert str(descr$,1%,2%) to sls_regn%, data goto L62075

L62075:     convert sls_regn%         to sls_regn$, pic(00)

            gencdkey$ = " "
            str(gencdkey$,1%,9%)   = "COST REGN"
            str(gencdkey$,10%,15%) = sls_regn$
            read #3,key = gencdkey$, using L62055, descr$, eod goto L62120

            convert str(descr$,1%,8%) to trn_amt, data goto L62120

L62120: return

        compute_cost
            tot_mat = 0.0
            mat lab = zer : mat tc = zer
            if str(sale$(kk%),1%,1%) <> "*" then kk% = 1%
            if x_err%     <> 2% then L62185
                                                  /* Product is a Part */
            if sls_price = 0    then tot_mat = sale(kk%,3%)              ~
                                else tot_mat = sls_price * sale(kk%,2%)
            if tot_mat   = 0    then L62185
        return
                                                   /* Costing Error    */
L62185:     tot_mat = (pc(1%) * .50) * sale(kk%,1%)/* Calc based on the*/
                                                   /* Catalog Price    */
            if tot_mat = 0      then tot_mat = sls_price * sale(kk%,1%)
            if tot_mat = 0      then tot_mat = sale(kk%,3%)
        return

        recalc_cost
            str(dtlkey$,1%,4%) = rga_number$
            str(dtlkey$,5%,2%) = rga_item$
            calckey$           = all(hex(20))
            gosub load_sale

        recalc_next
            read #2,key > calckey$, eod goto L62675
            get  #2, using L35030,                                        ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_dt_status$,     /* RGA Item Status            */~
                     rga_number$,        /* RGA Number                 */~
                     rga_item$,          /* RGA Item                   */~
/* (EWD002) */   str(rga_compt$,1%,4%),  /* RGA Complaint No.          */~
                     rga_reason$,        /* RGA Reason Code            */~
                     rga_dept$,          /* RGA Department Code        */~
                     rga_part$,          /* RGA Part No.               */~
                     rga_so$,            /* RGA Sales Order No.        */~
                     rga_line$,          /* RGA S.O. Line Item         */~
                     rga_piece$,         /* RGA Line Item Piece Count  */~
                     rga_qty$,           /* RGA Line Item Quantity     */~
                     rga_po$,            /* RGA Purchase Order No.     */~
                     rga_load$,          /* RGA Load No.               */~
                     rga_inv$,           /* RGA S.O. Invoice No.       */~
                     rga_chk$,           /* RGA Invoice Check No.      */~
                     rga_credit,         /* RGA Credit Amount          */~
                     rga_mat_cost,       /* RGA Item Material Cost     */~
                     rga_labor_cost,     /* RGA Item Labor Cost        */~
                     rga_overhd_cost,    /* RGA Item Overhead Cost     */~
                     rga_trans_cost,     /* RGA Item Transport. Cost   */~
                     rga_frt_cost,       /* RGA Freight Cost           */~
                     rga_vinyl_disc,     /* RGA Vinyl Discount Cost    */~
                     rga_hows$,          /* RGA How Shipped Code       */~
                     rga_prod_dte$,      /* RGA Production Date        */~
                     rga_gl_acct$,       /* RGA General Legder Account */~
                     rga_gl_posted$,     /* RGA G/L Posted Flag        */~
                     rga_pickup_load$(), /* RGA Pickup Load No. (3)    */~
                     rga_pickup_dte$(),  /* RGA Pickup Date     (3)    */~
                     rga_salesman$,      /* RGA Salesman               */~
                     rga_dt_desc_txt$,   /* RGA Item Description Code  */~
                     rga_dt_txt$,        /* RGA Item Text Flag         */~
                     rga_dt_userid$,     /* Userid of Item Entry/Mod   */~
                     rga_dt_mod_dte$,    /* RGA Item Entry/Mod Date    */~
/*PAR000*/           rga_dt_subp$,       /* RGA Item Subpart           */~
                     rga_dt_filler$      /* APCRGADT Filler Area       */

            str(calckey$,1%,4%) = rga_number$
            str(calckey$,5%,2%) = rga_item$
            gosub rga_cost

            read   #2,hold,key  = calckey$, eod goto L62465
L62465:     delete #2
            put    #2, using L35030,                                      ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_dt_status$,     /* RGA Item Status            */~
                     rga_number$,        /* RGA No.                    */~
                     rga_item$,          /* RGA Item                   */~
/* (EWD002) */   str(rga_compt$,1%,4%),  /* RGA Complaint No.          */~ 
                     rga_reason$,        /* RGA Reason Code            */~
                     rga_dept$,          /* RGA Department Code        */~
                     rga_part$,          /* RGA Part No.               */~
                     rga_so$,            /* RGA Sales Order No.        */~
                     rga_line$,          /* RGA S.O. Line Item         */~
                     rga_piece$,         /* RGA Line Item Piece Count  */~
                     rga_qty$,           /* RGA Line Item Quantity     */~
                     rga_po$,            /* RGA Purchase Order No.     */~
                     rga_load$,          /* RGA Load No.               */~
                     rga_inv$,           /* RGA S.O. Invoice No.       */~
                     rga_chk$,           /* RGA Invoice Check No.      */~
                     rga_credit,         /* RGA Credit Amount          */~
                     rga_mat_cost,       /* RGA Item Material Cost     */~
                     rga_labor_cost,     /* RGA Item Labor Cost        */~
                     rga_overhd_cost,    /* RGA Item Overhead Cost     */~
                     rga_trans_cost,     /* RGA Item Transport. Cost   */~
                     rga_frt_cost,       /* RGA Freight Cost           */~
                     rga_vinyl_disc,     /* RGA Vinyl Discount Cost    */~
                     rga_hows$,          /* RGA How Shipped Code       */~
                     rga_prod_dte$,      /* RGA Production Date        */~
                     rga_gl_acct$,       /* RGA General Legder Account */~
                     rga_gl_posted$,     /* RGA G/L Posted Flag        */~
                     rga_pickup_load$(), /* RGA Pickup Load No. (3)    */~
                     rga_pickup_dte$(),  /* RGA Pickup Date     (3)    */~
                     rga_salesman$,      /* RGA Salesman               */~
                     rga_dt_desc_txt$,   /* RGA Item Description Code  */~
                     rga_dt_txt$,        /* RGA Item Text Flag         */~
                     rga_dt_userid$,     /* Userid of Item Entry/Mod   */~
                     rga_dt_mod_dte$,    /* RGA Item Entry/Mod Date    */~
/*PAR000*/           rga_dt_subp$,       /* RGA Item Subpart           */~
                     rga_dt_filler$      /* APCRGADT Filler Area       */

            write #2, eod goto L62665

L62665:     goto recalc_next

L62675:     gosub dataread

        return

        load_sale
            call "SHOSTAT" ("Loading Costing Tables")

            mat sale = zer
            init(" ") readkey$, sale$()
            str(readkey$,1%,9%) = "COST SALE"

        load_sale_nxt
            read #4,key > readkey$, using L62745, readkey$, desc$,        ~
                eod goto load_sale_done
L62745:         FMT CH(24), CH(30)

           if str(readkey$,1%,9%) <> "COST SALE" then load_sale_done
                kk% = 0%
                convert str(readkey$,10%,3%) to kk%, data goto L62770
L62770:
                kk% = kk% + 1%
                convert str(desc$,1%,8%)  to sale(kk%,1%), data goto L62785
L62785:
                convert str(desc$,11%,8%) to sale(kk%,2%), data goto L62795
L62795:
                convert str(desc$,22%,8%) to sale(kk%,3%), data goto L62805
L62805:
                sale(kk%,1%)          = sale(kk%,1%) / 100.0
                sale(kk%,2%)          = sale(kk%,2%) / 100.0
                str(sale$(kk%),1%,1%) = "*"     /* COST OF SALE EXISTS */
                goto load_sale_nxt

        load_sale_done
            readkey$ = " "
            str(readkey$,1%,9%) = "COST NONE"
        load_nocost_nxt
            read #4,key > readkey$, using L62745, readkey$, desc$,        ~
                eod goto load_nocost_done

            if str(readkey$,1%,9%) <> "COST NONE" then load_nocost_done
            kk% = 0%
            convert str(readkey$,10%,3%) to kk%, data goto L62885
L62885:
            kk% = kk% + 1%
            str(sale$(kk%),2%,1%) = "*"      /* SET DO NOT COST FLAG */
            goto load_nocost_nxt
        load_nocost_done
        return

        pack_comp_number                    /*  (EWD002)   */
            convert rga_compt$ to comp_number%, data goto L63500
L63500:

            put str(rga_compt$,1%,4%), using L63510, comp_number%
            str(rga_compt$,5%,3%) = " "
            str(rga_compt$,5%,1%) = " "
            str(comp_key$,1%,5%)    = str(rga_compt$,1%,5%)
        return

        unpack_comp_number
            get str(rga_compt$,1%,4%), using L63510, comp_number%
L63510:             FMT BI(4)
            convert comp_number% to rga_compt$, pic(00000000)

        return                              /*  (EWD002)  */      

/* PAR000 - beg */
        lookup_bcksubpt 
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
                          #63,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */
           
            if err1% <> 0% then str(bcksubpt_rec$,48%,20%) = "00000                    "
            if err1% = 0% then return

            return

            errormsg$ = "BCKSUBPT ERR= "&so_inv$ & " Line= " & item_no$ & " Flag= " & flag$

        return
/* PAR000 - end */

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_sub
            call "SHOSTAT" ("One Moment Please")
            close #6  : close #7  : close #9  : close #10 : close #38
            close #11 : close #12 : close #13 : close #14 : close #21
            close #23 : close #25 : close #27 : close #28 : close #29
            close #30 : close #31 : close #33 : close #34 : close #35
            close #36 : close #37

        end
