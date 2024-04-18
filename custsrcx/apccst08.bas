        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    CCC    SSSSS  TTTTT   000    888   *~
            *  A   A  P   P  C   C  C   C  S         T    0   0  8   8  *~
            *  AAAAA  PPPP   C      C        S       T    0   0   888   *~
            *  A   A  P      C   C  C   C       S    T    0   0  8   8  *~
            *  A   A  P       CCC    CCC   SSSSS     T     000    888   *~
            *                                                           *~
            *  *** Special *** Use PF(9) to Turn on Debug Display       *~
            *                  Hook Area - Line No. 61955               *~
            *-----------------------------------------------------------*~
            * APCCST08 - Costing Gross Profit Analysis Report           *~
            *                                                           *~
            *      Routines - (APCCST5B) - Sub to Calculate the Direct  *~
            *                              and Indirect Labor Cost      *~
            *                              Assoc. with a MFG Part.      *~
            *                     Tables - 'CAPACITY ', 'COST 01LB'     *~
            *                              'COST 02LB', 'COST 03LB'     *~
            *                              'COST 04LB', 'COST 05LB'     *~
            *                                                           *~
            *                 (APCCST1B) - Sub to Calculate the Primary,*~
            *                              Misc., and Grid/Liting       *~
            *                              Material Cost Associated     *~
            *                              with a MFG Part. Scrap Cost  *~
            *                              is also Calculated.          *~
            *                              (APCCSTSB - Used )           *~
            *                     Tables - 'COST 03LT', 'COST 01SP'     *~
            *                              'COST 01PD' = Liting Pads    *~
            *                                                           *~
            *                 (APCCST9B) - Calc the Unit Cost Assoc.    *~
            *                              with a Raw Mat'l Part No.    *~
            *                                                           *~
            *                 (APCCST2B) - Calc the Glass and Screen    *~
            *                              Cost of Mat'ls for a MFG     *~
            *                              Part. Scrap Cost also Calc.  *~
            *                     Tables - 'COST 01GM', 'COST 01GS'     *~
            *                              'COST 02SC', 'COST 01SP'     *~
            *                                                           *~
            *                 (APCCST3B) - Calc the Cost of Locks and   *~
            *                              Keeper Rail for MFG Part     *~
            *                     Tables - 'COST 06LK'                  *~
            *                               Note - Lock and Keeper Raw  *~
            *                                      Material             *~
            *                            - 'COST 07LK'                  *~
            *                               Note - Lock and Keeper      *~
            *                                      Screws               *~
            *                                                           *~
            *                 (APCCST4B) - Calc. the Cost of Hardware   *~
            *                              and Packaging. When doing    *~
            *                              Hardware also Calc the Cost  *~
	    *            of Balcance Tubes.           *~
            *                     Tables - 'COST 05BL'                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/15/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 10/20/95 ! Mod to Calc Cost same as (APCSLS00)      ! RHH *~
            * 05/31/96 ! Mod to put in place a Debug Hook area at ! RHH *~
            *          ! line '61955' o "*RHH".                   ! RHH *~
            * 11/11/97 ! Mod to add new file for labor APCPLNDP   ! RHH *~
            *          ! used by sub (APCCST5B)                   ! RHH *~
            *          !                                          ! RHH *~
            * 03/16/98 ! y2k compliance                           ! DJD *~
            * 10/31/05 ! (AWD001) CR347 Mod for Sub Part          ! CMG *~
            *06/18/2019! CR-1894 Increase EMP DEP to 3-bytes      ! DES *~
            *************************************************************

        dim                              /* Report Variables           */~
            lb_typ$1, lb_typ_d$30,       /* LAB TYPE (S)tandard (A)ctua*/~
            bg_mod$3, bg_mod_d$30,       /* Beginning Model Code all   */~
            ed_mod$3, ed_mod_d$30,       /* Ending MOdel Code          */~
            adj_mat$4,                   /* Material Adjustment        */~
            cst_mat$7,                   /* Material Cost              */~
            cst_lab$7,                   /* Labor Cost - Direct/Labor  */~
            cst_ovr$7,                   /* Labor Overhead             */~
            cst_tot$7,                   /* Total of all Pieces        */~
            ind_pct$7, ind$3,            /* Independent Discount Pcnt  */~
            ind_prc$7,                   /* INDEPENDENT Price          */~
            ind_gpft$7,                  /* Independent Gross Profit   */~
            ind_gpcnt$3,                 /* Independent Percent        */~
            x_pct$7, sav_key$25, in1$3,  /* 1st Store Advertising Pcnt */~
            x_prc$7,                     /* 1st Store Price            */~
            x_gpft$7,                    /* 1st Store Gross Profit     */~
            x_gpcnt$3,                   /* 1st Store GP Pcnt          */~
            y_pct$7, in2$3,              /* 2nd Store Advertising Pcnt */~
            y_prc$7,                     /* 2nd Store Price            */~
            y_gpft$7,                    /* 2nd Store Gross Profit     */~
            y_gpcnt$3,                   /* 2nd Store GP Pcnt          */~
            store1$9, store1_d$30,       /* 1st Store Code             */~
            store2$9, store2_d$30,       /* 2nd Store Code             */~
            table$9,                     /* NAME OF HIGH VOL TABLE     */~
            apc_key$25, cd$1,            /* Gencodes Primary Key       */~
	    model$16,                    /* Virtual Model              */~
	    tpp$2,                       /* Triple Pane Packag(SR67154)*/~
	    nfrc$2,                                                     ~
	    forcedfoam$2,                                               ~
	    sash5_0$2,                                                  ~
            apc_desc$30                  /* Gencodes Description       */

        dim                              /* (Program) - Variables      */~
            part$25, part_desc$32,       /*                            */~
            partno1$20,                  /* Sub Part1        (AWD001)  */~
            mod$3,                       /*                            */~
            apc_scr$120, scr$8,          /*                            */~
            apc_prt$60,                  /*                            */~
            apc_sze$20,                  /*                            */~
            title$40, date$8,            /* REPORT TITLE               */~
            runtime$8, code$3,           /* REPORT RUN TIME            */~
            readkey$50, desc$30,         /* Generic Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,hdr$40,msg$(3%)79, /* Error message            */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim                              /* Costing Variables          */~
            hq(5%), hq$(5%)7,            /* Gross profit Buckets       */~
            lo(5%), lo$(5%)7,            /* Gross Profit Buckets Last  */~
            ll(5%), ll$(5%)7,            /* Gross profit Buckets       */~
            hql(5%), hql$(5%)7,          /* Gross Profit Buckets Last  */~
            lol(5%), lol$(5%)7,          /* Gross Profit Buckets Last  */~
            lll(5%), lll$(5%)7,          /* Gross Profit Buckets Last  */~
                                         /* Y2K - yr vars to 4 bytes   */~
            yr$4, c_yr$4, p_yr$4, cc$2,  /* Year, Curr Year, Prv Year  */~
                                         /* Y2K    Y2K       Y2K       */~
            ss1$2, ss2$2,                /* STORE (1) AND (2) CODE     */~
            cst(9%), customer$9,         /* Sales Analysis Cost Bucket */~
            postdate$6,                  /* INVOICE POST DATE          */~
            post$8, pst_dte$6            /* ENDING DATE FOR DATA       */

        dim                              /* Pricing Variables          */~
            size$1,                      /* (O)pening, (E)xact         */~
            pc(36%),                     /* Calc Dealer Price Catalog  */~
            upd$1,                       /* Update Prices Y or N       */~
            cuscode$9,                   /* Customer Code              */~
            ref$(30%)2,                  /* Ref Type Codes Catalog     */~
            ref1$(30%)2,                 /* Ref Type Codes Spec. Cat.  */~
            ref_p(30%),                  /* Ref Prices APC CAtalog     */~
            ref_p1(30%)                  /* Ref Prices Spec. Cat.      */

        dim f2%(25%),                    /* = 0 if the file is open    */~
            f1%(25%),                    /* = 1 if READ was successful */~
            fs%(25%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(25%)20                 /* Text from file opening     */

        dim                              /* Costing Variables          */~
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
            sale(1000%,3%),              /* STORE 'COST SALE' VALUES   */~
            sale$(1000%)2                /* STORE NO COST FLAG         */


        dim bg_date$8, bg_dte$6,         /* Beginning Post Date        */~
            ed_date$8, ed_dte$6, xx$8,   /* Ending Post Date           */~
            check_qty$4, mm$1,           /* Check qty Greater          */~
            total$12,                    /* Report Total               */~
            spec$30,                     /* Special Screen Count       */~
            spec_title$34                /* Special Title              */

        dim workdate1$10,                /* 10 Char Date               */~
            bg_date10$10,                /* 10 Char Date               */~
            ed_date10$10                 /* 10 Char Date               */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = " Costing Gross Profit Analysis Report   "
            pname$ = "APCCST08 - Rev: R6.04"

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
            * #01 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #02 ! HNYMASTR ! Part Master File                         *~
            * #03 ! HNYQUAN  ! Inventory Quantities Master File         *~
            * #04 ! GENCODES ! Master Code Table File                   *~
            * #05 ! AMTBOMCD ! Master Equation File                     *~
            * #06 ! AMTBOMIF ! Master Part Validity File                *~
            * #07 ! APCEMPLY ! Employee Master File                     *~
            * #08 ! APCEQUAT ! Equation an Parts Cross Reference File   *~
            * #09 ! APCCSTHP ! Hardware and Packaging Costing Components*~
            * #10 ! APCCSTLR ! Departments Average Hourly Rates         *~
            * #11 ! CPRPRICE ! Master System Price File                 *~
            * #12 ! CUSTOMER ! Master Customer File                     *~
            * #13 ! APCPCMST ! Pricing Definition file                  *~
            * #14 ! APCSKUNO ! Home Center's Skuno File                 *~
            * #15 ! APCPCMSK ! Pricing Key Definition File              *~
            * #16 ! APCPCMSD ! Pricing Master Calc Definition File      *~
            * #17 ! APCCSTEX ! APC COSTING EXCEPTION FILE DEF'S         *~
            * #18 ! APCSTOCK ! APC MASTER STOCK FILE                    *~
            * #19 ! APCSLSDT ! APC Sales Analysis Master File           *~
            * #20 ! APCCSTWK ! Costing Report Work File                 *~
            * #21 ! APCPLNDP ! MASTER DEPARTMENT FILE                   *~
            * #22 ! AWDPCMST ! Pricing Definition file     CR1894       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #3,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  24

            select #5,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #6,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

            select #7,  "APCEMPLY",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =    7, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos  =  12, keylen =  26, dup

            select #8,  "APCEQUAT",                                      ~
                        varc,     indexed,  recsize =   16,              ~
                        keypos =    1, keylen =   8

            select #9,  "APCCSTHP",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  20

            select #10, "APCCSTLR",                                      ~
                        varc,     indexed,  recsize =  102,              ~
                        keypos =    1, keylen =  3

            select #11, "CPRPRICE"                                       ~
                        varc,     indexed,  recsize = 700,               ~
                        keypos = 1,    keylen =  47

            select #12, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            select #13, "APCPCMST"                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 9,    keylen =  53,                     ~
                        alt key  1, keypos  =     1, keylen = 8

            select #14, "APCSKUNO"                                       ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos = 1,    keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #15, "APCPCMSK"                                       ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 1,    keylen =   5

            select #16, "APCPCMSD"                                       ~
/*AWD001*/              varc,     indexed,  recsize =  768,              ~
                        keypos = 1,    keylen =   9

            select #17,  "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            select #18, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32

            select #19, "APCSLSDT",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =   15, keylen =  20,                     ~
                        alt key  1, keypos =  331, keylen =  36,         ~
                            key  2, keypos =  340, keylen =  27,         ~
                            key  3, keypos =    7, keylen =  28,         ~
                            key  4, keypos =  375, keylen =  27,         ~
                            key  5, keypos =   76, keylen =  25, dup

            select #20, "APCCSTWK",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    1, keylen =  25

            select #21, "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =    32,             ~
                        keypos =   11, keylen =  12,                     ~
                        alt key  1, keypos  =   9, keylen =  14,         ~
                            key  2, keypos  =   4, keylen =  12,         ~
                            key  3, keypos  =   1, keylen =  15

            select #22, "AWDPCMST",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    9, keylen =  53,                     ~
                        alt key  1, keypos  =  1, keylen =  8

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),  0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),  0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),  0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),  0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),  0%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),  0%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%), 10%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 10%, rslt$(18%))
        REM CALL "OPENCHCK" (#19, FS%(19%), F2%(19%),  0%, RSLT$(19%))
            call "OPENCHCK" (#21, fs%(21%), f2%(21%), 10%, rslt$(21%))

            filename$ = "AWDPCMST" : call "EWDOPEN" (#22, filename$, err%)
            if err% <> 0% then gosub open_error

            mat f1% = zer
            des% = 0%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)

            date$ = date
	    call "DATEFMT" (date$)

            workdate1$ = date                          /* Y2K */
            call "DATEFMT" (workdate1$)                /* Y2K */

            c_yr$ = str(workdate1$,7%,4%)                   /* CURRENT YEAR */
            p_yr% = 95%
            convert c_yr$ to p_yr%, data goto L09140
L09140:
            convert (p_yr%-1%) to p_yr$, pic(####)       /* PREVIOUS YEAR*/
                                                         /* Y2K - 4 chars */

            rem Y2K post$ = date$
            str(workdate1$,4%,2%) = "01"
            call "DATUFMTC" (workdate1$)
            pst_dte$ = str(workdate1$, 1%,6%)

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
        REM TABLE$ = "COST TEST"             /* HIGH VOLUME TABLE      */
            table$ = "COST 0000"             /* HIGH VOLUME TABLE      */
            gosub load_sale                  /* LOAD 'COST SALE' TABLE */
                                             /*  AND 'COST NONE' TABLE */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 8%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 14% then goto inputmode_a
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 10% then gosub print_report
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 8% then editpg1
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
            *       I N P U T   M O D E   R E P O R T   S C R E E N     *~
            *************************************************************

        inputmode_a
            gosub initialize_variables

            for fieldnr% = 1% to 2%
L12080:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12200
L12100:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12180
L12130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L12100
                         if fieldnr% = 1% then L12080
                         goto L12130
L12180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12100
L12200:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12100
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   R E P O R T   S C R E E N      *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub gen_special
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg2
L12350:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 2% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L12400:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12400
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12400
                  lastfieldnr% = fieldnr%
            goto L12350

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            gosub select_printer
            gosub generate_report
            close printer
            gosub delete_work
        return clear all
        goto inputmode

        select_printer
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
        return

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
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter an (S)tandard or (A)ctual for the Costimg Method to Use",~
         "Enter a Beginning Model Product Code or (ALL)?               ",~
         "Enter a Ending Model Product Code?                           ",~
         "Enter a Valid Independent Price Discount Percent in Decimal? ",~
         "Enter a Valid Store One Customer Code?                       ",~
         "Enter a Valid Store One Advertising Discount Percent in Dec.?",~
         "Enter a Valid Store Two Customer Code?                       ",~
         "Enter a Valid Store Two Advertising Discount Percent in Dec.?"

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28305
                inpmessage$ = edtmessage$
                return

L28305
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Beginning and Ending Post Date?                ",~
         "Enter a Valid Greater than 'Check Quantity'?                 "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, lb_typ$, desc$,  ~
                      lb_typ_d$, bg_mod$, bg_mod_d$, ed_mod$, ed_mod_d$, ~
                      cst_mat$, cst_lab$, cst_ovr$, cst_tot$, ind_pct$,  ~
                      ind_prc$, ind_prc$, ind_gpft$, ind_gpcnt$,         ~
                      x_pct$, x_prc$, x_gpft$, x_gpcnt$, y_pct$,         ~
                      y_prc$, y_gpft$, y_gpcnt$, apc_key$, apc_desc$,    ~
                      title$, runtime$, store1$, store2$, store1_d$,     ~
                      store2_d$, bg_date$, ed_date$, bg_dte$, ed_dte$,   ~
                      check_qty$, total$, spec$, ss1$, ss2$, ind$, in1$, ~
                      in2$, partno1$
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
        REM DATALOAD

        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40230,         /* (S)tandard,(A)ctual  */~
                                L40230,         /* Beginning Model Code */~
                                L40230,         /* Ending Model Code    */~
                                L40230,         /* Independant Discount */~
                                L40230,         /* 1st Store Customer Cd*/~
                                L40230,         /* 1st Store Advertise %*/~
                                L40230,         /* 2nd Store Customer Cd*/~
                                L40230          /* 2nd Store Advertise %*/
              goto L40260

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40260:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "(S)tandard, (A)ctual Method:",               ~
               at (04,31), fac(lfac$(1%)), lb_typ$              , ch(01),~
               at (04,40), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (05,02), "Beginning Model Code (ALL) :",               ~
               at (05,31), fac(lfac$(2%)), bg_mod$              , ch(03),~
               at (05,40), fac(hex(84)), bg_mod_d$              , ch(30),~
                                                                         ~
               at (06,02), "Ending Model Code          :",               ~
               at (06,31), fac(lfac$(3%)), ed_mod$              , ch(03),~
               at (06,40), fac(hex(84)), ed_mod_d$              , ch(30),~
                                                                         ~
               at (07,02), "Independent Price Percent  :",               ~
               at (07,31), fac(lfac$(4%)), ind_pct$             , ch(07),~
                                                                         ~
               at (08,02), "1st Store Customer Code    :",               ~
               at (08,31), fac(lfac$(5%)), store1$              , ch(09),~
               at (08,42), fac(hex(84)), store1_d$              , ch(30),~
                                                                         ~
               at (09,02), "1st Store Advertising Pcnt :",               ~
               at (09,31), fac(lfac$(6%)), x_pct$               , ch(07),~
                                                                         ~
               at (10,02), "2nd Store Customer Code    :",               ~
               at (10,31), fac(lfac$(7%)), store2$              , ch(09),~
               at (10,42), fac(hex(84)), store2_d$              , ch(30),~
                                                                         ~
               at (11,02), "2nd Store Advertising Pcnt :",               ~
               at (11,31), fac(lfac$(8%)), y_pct$               , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40730
                  debug% = 1%

L40730:        if keyhit% <> 15% then goto L40770
                  call "PRNTSCRN"
                  goto L40070

L40770:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40970     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Special Rpt "
            pf$(2%)= "                   (4)Previous Field    " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40930
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40930:     if fieldnr% > 1% then L40950
               str(pf$(2%),20%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40950: return

L40970: if fieldnr% > 0% then L41090  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                   (9)Debug Display     " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff09ffffffff0e0f1000)
            if userid$ = "RHH" or userid$ = "DJD" then return
               str(pf$(2%),20%,20%) = " " : str(pfkeys$,9%,1%) = hex(ff)
        return

L41090:     pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub set_pf2

              gosub'060(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42180,         /* BEG/END DATE         */~
                                L42180          /* CHECK QUANTITY       */

              goto L42210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Starting Post date:",                        ~
               at (05,22), fac(lfac$(1%)), bg_date10$           , ch(10),~
                                                                         ~
               at (05,40), "Ending Post Date  :",                        ~
               at (05,60), fac(lfac$(1%)), ed_date10$           , ch(10),~
                                                                         ~
               at (06,02), "Check Qty Greater :",                        ~
               at (06,22), fac(lfac$(2%)), check_qty$           , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42480
                  call "PRNTSCRN"
                  goto L42210

L42480:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42670     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                   (4)Previous Field    " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L42630
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42630:     if fieldnr% > 1% then L42650
               str(pf$(2%),20%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42650: return

L42670: if fieldnr% > 0% then L42770  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
        return

L42770:     pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50180,         /* (S)TANDARD,(A)CTUAL   */ ~
                              L50300,         /* Beg Model Code        */ ~
                              L50450,         /* End Model Code        */ ~
                              L50600,         /* Indpendent Price Pcnt */ ~
                              L50730,         /* Store One Customer Cod*/ ~
                              L50890,         /* Store One Advertise % */ ~
                              L51030,         /* Store Two Customer Cod*/ ~
                              L51190          /* Store Two Advertise % */
            return

L50180: REM Standard / Actual Costing Method      LB_TYP$, LB_TYP_D$
            if lb_typ$ <> " " then goto L50220
               lb_typ$ = "A"

L50220:     if lb_typ$ <> "S" and lb_typ$ <> "A" then goto L50260
               if lb_typ$ = "S" then lb_typ_d$ ="Standard Costing Method"
               if lb_typ$ = "A" then lb_typ_d$ ="Actual Costing Method"
        return
L50260:     errormsg$ = "(Error) - Invalid Costing Method Selected??"
            lb_typ$, lb_typ_d$ = " "
        return

L50300: REM Beginning Mode Code              BG_MOD$, BG_MOD_D$
            if bg_mod$ <> " " then goto L50350
L50320:        bg_mod$ = "ALL"
               bg_mod_d$  = "(ALL) Model Codes in Table"
               return
L50350:     if str(bg_mod$,1%,1%) = "A" then goto L50320
            code$ = bg_mod$
            gosub check_model
            if code% = 0% then goto L50410
            bg_mod_d$ = desc$
        return
L50410:     errormsg$ = "(Error) - Invalid Model Code "
            bg_mod$, bg_mod_d$ = " "
        return

L50450: REM Ending                           ED_MOD$, Ed_MOD_D$
            if ed_mod$ <> " " then goto L50500
L50470:        ed_mod$ = "ALL"
               bg_mod_d$  = "(ALL) Model Codes in Table"
               return
L50500:     if str(bg_mod$,1%,1%) = "A" then goto L50470
            code$ = ed_mod$
            gosub check_model
            if code% = 0% then goto L50560
            ed_mod_d$ = desc$
        return
L50560:     errormsg$ = "(Error) - Invalid Model Code "
            bg_mod$, bg_mod_d$ = " "
        return

L50600: REM Independent Price Discount        IND_PCT$,
            convert ind_pct$ to ind_pct, data goto L50690

            convert ind_pct to ind_pct$, pic(###.##-)

            if ind_pct < .00 or ind_pct > .99 then goto L50690
               ind% = int(ind_pct * 100.0)
               convert ind% to ind$,pic(##-)
        return
L50690:   errormsg$="(Error) - Invalid Price Discount Percent?"
          init(" ") ind_pct$, ind$
        return

L50730: REM 1st Customer Code                STORE1$, STORE1_D$
            cuscode$ = " " : srce1% = 0% : store1% = 1%
            str(cuscode$,1%,9%) = store1$
            read #12,key = cuscode$, eod goto L50850
                get #12, using L50780, store1_d$, cd$
L50780:       FMT POS(10), CH(30), POS(525), CH(1)

REM         add check for first character being aphabetic to zero store
            if (str(store1$,1%,1%) >= "A" and str(store1$,1%,1%) <= "Z" or    ~
                str(store1$,1%,1%) >= "a" and str(store1$,1%,1%) <= "z") and  ~
               str(store1$,3%,4%) = "0000" then store1% = 0%
REM         if str(store1$,3%,4%) = "0000" then store1% = 0%
            ss1$ = str(store1$,1%,2%)
            if cd$ >= "A" and cd$ <= "Z" then                            ~
               srce1% = val(cd$) - 64%          /* (01) Thru (26) */     ~
               else  srce1% = val(cd$) - 21%    /* (27) Thru (36) */
        return
L50850:     errormsg$ = "(Error) - Invalid Store (1) Customer Code?"
            init(" ") store1$, store1_d$, ss1$
        return

L50890: REM 1st Store Advertising             X_PCT$,
            convert x_pct$ to x_pct, data goto L50990

            convert x_pct to x_pct$, pic(###.##-)

            if x_pct < .00 or x_pct > .99 then goto L50990
               in1% = int(x_pct * 100.0)
               convert in1% to in1$,pic(##-)

        return
L50990:   errormsg$="(Error) - Invalid Advertising Percentage?"
          init(" ") x_pct$, in1$
        return

L51030: REM 2nd Customer Code                STORE2$, STORE2_D$
            cuscode$ = " " : srce2% = 0% : store2% = 1%
            str(cuscode$,1%,9%) = store2$
            read #12,key = cuscode$, eod goto L51150
                get #12, using L51080, store2_d$, cd$
L51080:       FMT POS(10), CH(30), POS(525), CH(1)
REM         add check for first character being aphabetic to zero store
            if (str(store2$,1%,1%) >= "A" and str(store2$,1%,1%) <= "Z" or   ~
                str(store2$,1%,1%) >= "a" and str(store2$,1%,1%) <= "z") and ~
               str(store2$,3%,4%) = "0000" then store2% = 0%
REM         if str(store2$,3%,4%) = "0000" then store2% = 0%
            ss2$ = str(store2$,1%,2%)
            if cd$ >= "A" and cd$ <= "Z" then                            ~
               srce2% = val(cd$) - 64%          /* (01) Thru (26) */     ~
               else  srce2% = val(cd$) - 21%    /* (27) Thru (36) */
        return
L51150:     errormsg$ = "(Error) - Invalid Store (2) Customer Code?"
            init(" ") store2$, store2_d$, ss2$
        return

L51190: REM Store (2) Advertising Pcnt        Y_PCT$,
            convert y_pct$ to y_pct, data goto L51290

            convert y_pct to y_pct$, pic(###.##-)

            if y_pct < .00 or y_pct > .99 then goto L51290
               in2% = int(y_pct * 100.0)
               convert in2% to in2$,pic(##-)

        return
L51290:   errormsg$="(Error) - Invalid Advertising Percentage?"
          init(" ") y_pct$, in2$
        return

        check_model
           code% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)    = "MODEL    "
           str(readkey$,10%,15%)  = code$
           read #4,key = readkey$, using L51390, desc$, eod goto L51410
L51390:        FMT POS(25), CH(30)
           code% = 1%
L51410: return

        REM *************************************************************~
            *               R e p o r t   E d i t s                     *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52100,         /* (S)TANDARD,(A)CTUAL   */ ~
                              L52360          /* Beg Model Code        */
            return

L52100: REM Beginning Post Date                   BG_DATE$, ED_DATE$
            if bg_date10$ <> " " then goto L52140
               bg_date10$ = date

L52140:     call "DATEOKC" (bg_date10$, date%, errormsg$)
            if date% = 0% then return

            if ed_date10$ <> " " then goto L52200
               ed_date10$ = bg_date10$

L52200:     call "DATEOKC" (ed_date10$, date%, errormsg$)
            if date% = 0% then return

            workdate1$ = bg_date10$
            call "DATUNFMT" (workdate1$)
            bg_dte$ = str(workdate1$,1%,6%)
            workdate1$ = ed_date10$
            call "DATUNFMT" (workdate1$)
            ed_dte$ = str(workdate1$,1%,6%)
            if bg_dte$ > ed_dte$ then goto L52320

        return
L52320:     errormsg$ = "(Error) - Invalid Beg/End Date?"
            init(" ") bg_date$, ed_date$, bg_dte$, ed_dte$, bg_date10$, ed_date10$
        return

L52360: REM Beginning Mode Code              CHECK_QTY$
            if check_qty$ <> " " then goto L52390
               check_qty$ = "0100"
L52390:     convert check_qty$ to check_qty%, data goto L52440

            convert check_qty% to check_qty$, pic(####)

        return
L52440:     errormsg$ = "(Error) - Invalid Check Greater Quantity?"
            init(" ") check_qty$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55060: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
L55080: %! ######## @ ########                        ###################~
        ~#####################                     (APCCST08) Page: #### !

L55110: %! Costing Method: ##############################                ~
        ~                                        Ind Multiplier: ####### !
        %!                                                               ~

L55150: %!                                !Mater'l! Labor !Over Hd! Total~
        ~ ! Total ! Gross !###%!      #########!###%!      #########!###%!

L55180: %!<------ Description ----------->! Cost  ! Cost  ! Cost  ! Cost ~
        ~ ! Price ! Profit!Pcnt! Price !GProfit!Pcnt! Price !GProfit!Pcnt!

L55210: %!** ######################### ***!       !       !       !      ~
        ~ !       !       !    !       !       !    !       !       !    !

L55240: %!################################!#######!#######!#######!######~
        ~#!#######!#######!###%!#######!#######!###%!#######!#######!###%!

L55270: %!   Volume YTD / LYTD in Units   !       !       !       !      ~
        ~ !#######!#######!    !#######!#######!    !#######!#######!    !

L55300: %!   Results This YTD             !       !       !       !######~
        ~#!#######!#######!###%!#######!#######!###%!#######!#######!###%!

L55330: %!   Results Last LYTD            !       !       !       !######~
        ~#!#######!#######!###%!#######!#######!###%!#######!#######!###%!

L55360: %!--------------------------------!-------!-------!-------!------~
        ~-!-------!-------!----!-------!-------!----!-------!-------!----!


L55400: %+---------------------------------------------------------------~
        ~--------------+
L55420: %!---------------------------------------------------------------~
        ~--------------!
L55440: %! ######## @ ########     ##################################    ~
        ~    Page: ####!

L55470: %! Beginning Date: ##########                          Ending Dat~
        ~e: ########## !
        %!                                                               ~

L55510: %!<------Part Number  ------>!<------------------------------>!  ~
        ~  Quantity    !

L55540: %! ######################### !################################!  ~
        ~############  !

L55570: %!---------------------------!--------------------------------!--~
        ~--------------!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55110, lb_typ_d$, ind_pct$
           print using L55060
           print using L55150, ind$, store1$, in1$,      ~
                              store2$, in2$
           print using L55180
           lcntr% = 6%
        return

        print_header_spec
           if lcntr% <> 99% then print using L55400
           print page
           pageno% = pageno% + 1%
           print using L55400
           print using L55440, date$, runtime$, spec_title$, pageno%
           print using L55470, bg_date10$, ed_date10$
           print using L55420
           print using L55510
           lcntr% = 5%
        return

        print_detail
           if lcntr% > 54% then gosub print_header
              print using L55360
              print using L55210, part$
              print using  L55240, part_desc$, cst_mat$, cst_lab$,        ~
                                cst_ovr$, cst_tot$, ind_prc$, ind_gpft$, ~
                                  ind_gpcnt$, x_prc$, x_gpft$, x_gpcnt$, ~
                                  y_prc$, y_gpft$, y_gpcnt$
          print using L55270, ll$(1%), lll$(1%), hq$(1%), hql$(1%),       ~
                                                lo$(1%), lol$(1%)

          print using L55300, ll$(2%), ll$(3%), ll$(4%), ll$(5%), hq$(3%),~
                             hq$(4%), hq$(5%), lo$(3%), lo$(4%), lo$(5%)


          print using L55330, lll$(2%), lll$(3%), lll$(4%), lll$(5%),     ~
                             hql$(3%), hql$(4%), hql$(5%), lol$(3%),     ~
                             lol$(4%), lol$(5%)

          lcntr% = lcntr% + 6%
        return

        print_special
           if lcntr% > 57% then gosub print_header_spec
              print using L55570
              print using L55540, sav_key$, part_desc$, total$
              lcntr% = lcntr% + 2%
        return

        generate_report
           call "SHOSTAT" ("Sorting Products")
           gosub sort_product
           scr$ = "[ #### ]" :cnt% = 0%
           title$ = "***** APC Costing Analysis Report ******"
           call "SHOSTAT" ("Calculating Product Costs")
           apc_key$ = " "
           read #20,key > apc_key$, using L60360, part$, part_desc$,      ~
                                         adj_mat$, eod goto generate_done
           goto L60365
        generate_nxt
           read #20, using L60360, part$, part_desc$, adj_mat$,           ~
                                                   eod goto generate_done
L60360:       FMT POS(26), CH(25), CH(32), CH(4)
L60365:    init(" ") sav_key$
           sav_key$ = part$
           cnt% = cnt% + 1%
           if mod(cnt%,5%) <> 0 then goto L60400
              convert cnt% to str(scr$,3%,4%), pic(####)

              print at(03,35);hex(84);scr$;
L60400:    gosub calc_cost
           str(cuscode$,1%,9%) = store1$     /* Set Store (1)         */
           gosub calc_price
                                             /* Catalog / Independent */
                                             /* Independent - Price   */
                                             /* Ind Gross Profit      */
                                             /* Ind Gross Profit Pcnt */
           ind_prc   = round( pc(1%) * ind_pct, 2)
           ind_gpft  = round(ind_prc - cst_tot, 2)
           if ind_prc > 0 then goto L60465
              ind_prc = 1.0
              part_desc$ = "***** APC PRICE (ERROR) ******"

L60465:    ind_gpcnt = round( (ind_gpft / ind_prc) * 100.0, 2)
           convert ind_prc   to ind_prc$,   pic(###.##-)
           convert ind_gpft  to ind_gpft$,  pic(###.##-)
           x% = int( ind_gpcnt + .5)
           convert x% to ind_gpcnt$, pic(##-)
                                             /* 1st Store             */
                                             /* 1st Store Price       */
                                             /* 1st Store Gross Profit*/
                                             /* 1st Gross Profit Pcnt */
           price = pc(srce1%)             /* Cat. Price with Cust Disc*/
           if sp% > 0% and p1 > 0.1 then price = p1  /* Special Price */
           x = price * x_pct

           x_prc   = round( price - x , 2)
           x_gpft  = round(x_prc - cst_tot, 2)
           if x_prc > 0 then goto L60560
              x_prc = 1.0
              part_desc$ = "***** STORE 0NE (ERROR) ******"

L60560:    x_gpcnt = round( (x_gpft / x_prc) * 100.0, 2)
           convert x_prc   to x_prc$,   pic(###.##-)
           convert x_gpft  to x_gpft$,  pic(###.##-)
           x% = int( x_gpcnt + .5)
           convert x% to x_gpcnt$, pic(##-)
           str(cuscode$,1%,9%) = store2$     /* Set Store (2)         */
           gosub calc_price
                                             /* 2nd Store             */
                                             /* 2nd Store Price       */
                                             /* 2nd Store Gross Profit*/
                                             /* 2nd Gross Profit Pcnt */
           price = pc(srce2%)             /* Cat. Price with Cust Disc*/
           if sp% > 0% and p1 > 0.1 then price = p1  /* Special Price */
           x = price * y_pct

           y_prc   = round( price - x , 2)
           y_gpft  = round(y_prc - cst_tot, 2)
           if y_prc > 0 then goto L60665
              y_prc = 1.0
              part_desc$ = "***** STORE TWO (ERROR) ******"

L60665:    y_gpcnt = round( (y_gpft / y_prc) * 100.0, 2)
           convert y_prc   to y_prc$,   pic(###.##-)
           convert y_gpft  to y_gpft$,  pic(###.##-)
           x% = int( y_gpcnt + .5)
           convert x% to y_gpcnt$, pic(##-)

              gosub calc_margins               /* CALC MARGINS FOR YTD */
              gosub print_detail               /* AND LYTD FOR SALES   */
           goto generate_nxt
        generate_done
           print using L55040
        return

        sort_product
           call "SHOSTAT" ("Sorting Products")
           readkey$ = " "
           str(readkey$,1%,9%) = table$
           read #4,key > readkey$, using L60775, readkey$, desc$,         ~
                                                   eod goto sort_done
           goto L60780
        sort_nxt
           read #4, using L60775, readkey$, desc$, eod goto sort_done
L60775:        FMT CH(24), CH(30)
L60780:    if str(readkey$,1%,9%) <> table$ then goto sort_done
           mod$ = str(desc$,1%,3%)
           if bg_mod$ = "ALL" then goto L60805
              if mod$ < bg_mod$ then goto sort_nxt
              if mod$ > ed_mod$ then goto sort_nxt
L60805:          part$ = str(desc$,1%,25%)
                 adj_mat$ = str(desc$,27%,4%)
                 call "APCDESCR" (part$, apc_scr$, apc_prt$, apc_sze$,   ~
                                                               #6, err% )
             str(part_desc$,1%,16%) = str(apc_prt$,1%,16%)
             str(part_desc$,17%,16%)= str(apc_sze$,1%,16%)
             apc_key$ = " "
             str(apc_key$,1%,4%)  = str(part$,1%,4%)
             str(apc_key$,5%,7%)  = str(part$,13%,7%)
             str(apc_key$,12%,2%) = str(part$,7%,2%)
             str(apc_key$,14%,2%) = str(part$,5%,2%)
             str(apc_key$,16%,4%) = str(part$,9%,4%)

             write #20, using L60880, apc_key$, part$, part_desc$,        ~
                            adj_mat$, "              ", eod goto sort_nxt
L60880:           FMT CH(25), CH(25), CH(32), CH(4), CH(14)
             goto sort_nxt
        sort_done
        return

        calc_cost
           gosub calculated_cost
           cst_mat   = cst(1%)
           sub_labor = cst(2%)
           ovr_labor = cst(3%)
           cst_tot   = cst(6%)
           convert cst_mat   to cst_mat$, pic(###.##-)  /* Total Mat'l */
           convert sub_labor to cst_lab$, pic(###.##-)  /* Tot D/I Lab */
           convert ovr_labor to cst_ovr$, pic(###.##-)  /* Tot Overhead*/
           convert cst_tot   to cst_tot$, pic(###.##-)  /* Total Cost  */
        return

        calc_price                  /* Get Correct Size and Price      */
          mat pc     = zer
          mat ref_p  = zer
          mat ref_p1 = zer
          p1    = 0.0      /* Spc Customer Price, Dealer Catalog Always*/
          sp%   = 0%       /* (0%) Dealer Price Catalog (Only)         */
                           /* (1%) Spc Customer Catalog Price in (P1)  */
                           /* (2%) Spc Customer EDI Price in     (P1)  */
                           /* Note - Information from 'CUS PRICE' Table*/
          size$ = "E"      /* Always Exact Size                        */
          upd$ = "N"       /* Do Not Update Price Sheets               */
          err%  = 0%       /* Clear Error Flag                         */
	  init(" ") sash5_0$, model$, tpp$, nfrc$, forcedfoam$
	  des% = des% + 1%
          call "APCPRSUB" ( part$,       /* Part Number                */~
                            partno1$,    /* Sub Part 1         (AWD001)*/~
                            size$,       /* (O)pen,(E)xact,(F)No Deduct*/~
                            pc(),        /* Calc Dealer Price Catalog  */~
                            p1,          /* Special Customer Price     */~
                            sp%,         /* Special Price Code 0,1,2   */~
                            upd$,        /* Update Price Sheet Y or N  */~
                            cuscode$,    /* Customer Code              */~
                            err%,        /* Error Return Codes         */~
                            ref$(),      /* Ref. Type Codes Catalog    */~
                            ref1$(),     /* Ref. Type Codes Spec Cat.  */~
                            ref_p(),     /* Ref Prices APC Catalog     */~
                            ref_p1(),    /* Ref Prices Special Cat.    */~
			    sash5_0$,   ~
			    model$,     ~
			    tpp$,       ~
			    nfrc$,      ~
			    forcedfoam$,~
                            #13,         /* Channel of (APCPCMST) File */~
                            #22,         /* Channel of (AWDPCMST) File */~
                            #15,         /* Channel of (APCPCMSK) File */~
                            #16,         /* Channel of (APCPCMSD) File */~
                            #4,          /* Channel of (GENCODES) File */~
                            #11,         /* Channel of (CPRPRICE) File */~
                            #12,         /* Channel of (CUSTOMER) File */~
                            #14 )        /* Channel of (APCSKUNO) File */
                                         /* After Call PART$ Always    */
                                         /* has Exact Size             */
           if err% <> 0% then apc_err% = 8%   /* Pricing Error         */
           if apc_err% <> 0% then apc_err%(apc_err%) = 8%
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#20,mode$, 500%, f2%)
            if f2% <> 0% then goto L61190
        return
L61190:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCCSTWK)") : stop
        return
        delete_work
            call "FILEBGON" (#20)
        return

        calc_margins
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),100%, rslt$(19%))
            mat hq = zer : mat hql = zer
            mat lo = zer : mat lol = zer
            mat ll = zer : mat lll = zer

            init(" ") yr$,cc$,hq$(),lo$(),ll$(),hql$(),lol$(),lll$()
            init(" ") readkey$
            part$ = sav_key$
            str(readkey$,1%,25%) = part$
            read #19,key 5% = readkey$,using L61300,postdate$, readkey$,  ~
                            customer$, cst(), eod goto calc_margins_done
            goto L61305
        calc_margins_next
            read #19,using L61300,postdate$, readkey$, customer$, cst(),  ~
                                              eod goto calc_margins_done
L61300:       FMT CH(6),POS(76),CH(25),POS(340),CH(9),POS(402),9*PD(14,4)
L61305:     if str(readkey$,1%,25%) <> sav_key$ then                     ~
                                                goto calc_margins_done

               if postdate$ >= pst_dte$ then goto calc_margins_next
/* Y2K */           
               workdate1$ = postdate$
               call "DATFMTC" (workdate1$)
               yr$ = str(workdate1$, 7%,4%)
/* Y2K */

               cc$ = str(customer$,1%,2%)
                                   /* CST(9%) - Tot Quantity Shipped   */
                                   /* CST(8%) - Tot Sale Price W/Disc  */
                                   /* CST(6%) - Tot MFG Cost           */
         if yr$ <> c_yr$ then goto L61470
            if cc$ <> ss1$ then goto L61395              /* STORE (1)   */
              if store1% = 0% then goto L61375
                 if store1$ <> str(customer$,1%,9%) then goto L61435

L61375:        hq(1%) = round(hq(1%) + cst(9%), 2) /*UNITS*/
               hq(2%) = round(hq(2%) + cst(6%), 2) /*COST */
               hq(3%) = round(hq(3%) + cst(8%), 2) /*SALES*/
               goto calc_margins_next
L61395:     if cc$ <> ss2$ then goto L61435              /* STORE (2)   */
              if store2% = 0% then goto L61415
                 if store2$ <> str(customer$,1%,9%) then goto L61435

L61415:        lo(1%) = round(lo(1%) + cst(9%), 2) /*UNITS*/
               lo(2%) = round(lo(2%) + cst(6%), 2) /*COST */
               lo(3%) = round(lo(3%) + cst(8%), 2) /*SALES*/
               goto calc_margins_next
L61435: REM - OTHERS
                                                              /* OTHER */
            ll(1%) = round(ll(1%) + cst(9%), 2) /*UNITS*/
            ll(2%) = round(ll(2%) + cst(6%), 2) /*COST */
            ll(3%) = round(ll(3%) + cst(8%), 2) /*SALES*/
                  goto calc_margins_next

L61470:  if yr$ <> p_yr$ then goto calc_margins_next
            if cc$ <> ss1$ then goto L61515              /* STORE (1)   */
              if store1% = 0% then goto L61495
                 if store1$ <> str(customer$,1%,9%) then goto L61555

L61495:        hql(1%) = round(hql(1%) + cst(9%), 2) /*UNITS*/
               hql(2%) = round(hql(2%) + cst(6%), 2) /*COST */
               hql(3%) = round(hql(3%) + cst(8%), 2) /*SALES*/
               goto calc_margins_next
L61515:     if cc$ <> ss2$ then goto L61555              /* STORE (2)
              IF STORE2% = 0% THEN GOTO 61535
                 IF STORE2$ <> STR(CUSTOMER$,1%,6%) THEN GOTO 61555

               LOL(1%) = ROUND(LOL(1%) + CST(9%), 2) /*UNITS*/
               lol(2%) = round(lol(2%) + cst(6%), 2) /*COST */
               lol(3%) = round(lol(3%) + cst(8%), 2) /*SALES*/
               goto calc_margins_next
L61555: REM - OTHERS
                                                              /* OTHER */
            lll(1%) = round(lll(1%) + cst(9%), 2) /*UNITS*/
            lll(2%) = round(lll(2%) + cst(6%), 2) /*COST */
            lll(3%) = round(lll(3%) + cst(8%), 2) /*SALES*/
            goto calc_margins_next

        calc_margins_done
            hq(4%) = hq(3%) - hq(2%)
            if hq(3%) = 0 then goto L61615
               hq(5%) = round((hq(4%) / hq(3%))*100.,2)

L61615:     lo(4%) = lo(3%) - lo(2%)
            if lo(3%) = 0 then goto L61635
               lo(5%) = round((lo(4%) / lo(3%))*100.,2)

L61635:     ll(4%) = ll(3%) - ll(2%)
            if ll(3%) = 0 then goto L61655
               ll(5%) = round((ll(4%) / ll(3%))*100.,2)

L61655:     hql(4%) = hql(3%) - hql(2%)
            if hql(3%) = 0 then goto L61675
               hql(5%) = round((hql(4%) / hql(3%))*100.,2)

L61675:     lol(4%) = lol(3%) - lol(2%)
            if lol(3%) = 0 then goto L61695
               lol(5%) = round((lol(4%) / lol(3%))*100.,2)

L61695:     lll(4%) = lll(3%) - lll(2%)
            if lll(3%) = 0 then goto L61715
               lll(5%) = round((lll(4%) / lll(3%))*100.,2)

L61715:     convert hq(1%) to hq$(1%), pic(######-)
            convert hq(2%) to hq$(2%), pic(######-)
            convert hq(3%) to hq$(3%), pic(######-)
            convert hq(4%) to hq$(4%), pic(######-)
            x% = int(hq(5%) + .5)
            convert x% to hq$(5%), pic(##-)

            convert lo(1%) to lo$(1%), pic(######-)
            convert lo(2%) to lo$(2%), pic(######-)
            convert lo(3%) to lo$(3%), pic(######-)
            convert lo(4%) to lo$(4%), pic(######-)
            x% = int(lo(5%) + .5)
            convert x% to lo$(5%), pic(##-)

            convert ll(1%) to ll$(1%), pic(######-)
            convert ll(2%) to ll$(2%), pic(######-)
            convert ll(3%) to ll$(3%), pic(######-)
            convert ll(4%) to ll$(4%), pic(######-)
            x% = int(ll(5%) + .5)
            convert x% to ll$(5%), pic(##-)

            convert hql(1%) to hql$(1%), pic(######-)
            convert hql(2%) to hql$(2%), pic(######-)
            convert hql(3%) to hql$(3%), pic(######-)
            convert hql(4%) to hql$(4%), pic(######-)
            x% = int(hql(5%) + .5)
            convert x% to hql$(5%), pic(##-)

            convert lol(1%) to lol$(1%), pic(######-)
            convert lol(2%) to lol$(2%), pic(######-)
            convert lol(3%) to lol$(3%), pic(######-)
            convert lol(4%) to lol$(4%), pic(######-)
            x% = int(lol(5%) + .5)
            convert x% to lol$(5%), pic(##-)

            convert lll(1%) to lll$(1%), pic(######-)
            convert lll(2%) to lll$(2%), pic(######-)
            convert lll(3%) to lll$(3%), pic(######-)
            convert lll(4%) to lll$(4%), pic(######-)
            x% = int(lll(5%) + .5)
            convert x% to lll$(5%), pic(##-)
          close #19
        return

        calculated_cost
*       RHH
           calc% = 0%
             if debug% = 0% then goto L61965
                calc% = 99%
                part$ = "3212080006040586794"
*       RHH
L61965:    mat lab  = zer     : mat tc   = zer
           mat apc_err% = zer : mat pc = zer
           width = 0
           convert str(part$,13%,4%) to width, data goto L61985
L61985:
           kk% = 1%
           convert str(part$,1%,3%) to kk%, data goto L62000
L62000:
           p_err% = 0%
           x_err% = 0%
           if len(part$) < 19 then x_err% = 2%      /* PROD IS A PART */
           if str(part$,1%,1%) = "4" then x_err% = 2%        /* PART */
           if width = 0 then x_err% = 2%                     /* PART */
           if x_err% <> 0% then goto L62315

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
                             #1,         /*   (APCCUTEQ)               */~
                             #2,         /*   (HNYMASTR)               */~
                             #3,         /*   (HNYQUAN )               */~
                             #4,         /*   (GENCDSIN)               */~
                             #5,         /*   (AMTBOMCD)               */~
                             #7,         /*   (APCEMPLY)               */~
                             #8,         /*   (APCEQUAT)               */~
                             #9,         /*   (APCCSTHP)               */~
                             #10,        /*   (APCCSTLR)               */~
                             #11,        /*   (CPRPRICE)               */~
                             #12,        /*   (CUSTOMER)               */~
                             #13,        /*   (APCPCMST)               */~
                             #22,        /*   (APCPCMST)               */~
                             #14,        /*   (APCSKUNO)               */~
                             #15,        /*   (APCPCMSK)               */~
                             #16,        /*   (APCPCMSD)               */~
                             #17,        /*   (APCCSTEX)               */~
                             #18,        /*   (APCSTOCK)               */~
                             #21,        /*   (APCPLNDP)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */
            if str(sale$(kk%),2%,1%) <> "*" then goto L62285
               x_err% = 3%
               p_err% = 0%
               goto L62315
L62285:     for i% = 1% to 20%
              if apc_err%(i%) = 0% then goto L62305
                 p_err% = i%
                 x_err% = 1%
L62305:     next i%

L62315:   gosub store_cost                    /* Gather All MFG Costs  */
          if p_err% <> 0% then str(part_desc$,13%,20%) = err$(p_err%)

        return

        store_cost
           sls_price = pc(1)                 /* Set To Catalog Price   */
           sls_qty = 1 : qtyshp = 1          /* Default Qty one        */
           trn_amt = 0.0                     /* Transportation Not App */
           mat cst = zer                     /* Total Vinyl and Misc.  */
                                             /* (Mat'l + Scrap) Cost   */
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
           cst(1%) = round(tot_mat * qtyshp, 2)  /* Total Material     */
           cst(2%) = round(lab(8%) * qtyshp, 2)  /* Total Dir/Ind Labor*/
           cst(3%) = round(lab(9%) * qtyshp, 2)  /* Total Overhead Cost*/
           cst(4%) = round(tc(19%) * qtyshp, 2)  /* Total Freight Cost */
           cst(5%) = round(tc(20%) * qtyshp, 2)  /* Total Vinyl Disc't */
           cst(6%) = round(tot_cst * qtyshp, 2)  /* Total MFG Cost     */
           cst(7%) = round(trn_amt * qtyshp, 2)  /* Total Trans Cost   */
           cst(8%) = round(sls_price, 2)         /* Total Price W/Disc */
           cst(9%) = round(sls_qty, 2)           /* Total Quantity     */
        return

        compute_cost
           tot_mat = 0.0
           mat lab = zer : mat tc = zer
           if str(sale$(kk%),1%,1%) <> "*" then kk% = 1%
           if x_err% <> 2% then goto L62520
                                                  /* Product is a Part */
              if sls_price = 0 then tot_mat = sale(kk%,3%)               ~
                               else tot_mat = sls_price * sale(kk%,2%)
           if tot_mat = 0 then goto L62520
        return
                                                  /* Costing Error     */
L62520:    tot_mat = (pc(1%) * .50) * sale(kk%,1%)/* Calc based on the */
                                                  /* Catalog Price     */
           if tot_mat = 0 then tot_mat = sls_price * sale(kk%,1%)
           if tot_mat = 0 then tot_mat = sale(kk%,3%)
        return

        load_sale
           call "SHOSTAT" ("Loading Costing Tables")
           mat sale = zer
           init(" ") readkey$, sale$()
           str(readkey$,1%,9%)   = "COST SALE"
        load_sale_nxt
           read #4,key > readkey$, using L62590, readkey$, desc$,         ~
                                                eod goto load_sale_done
L62590:       FMT CH(24), CH(30)
           if str(readkey$,1%,9%) <> "COST SALE" then goto load_sale_done
              kk% = 1%
              convert str(readkey$,10%,3%) to kk%, data goto L62610
L62610:
              convert str(desc$,1%,8%)  to sale(kk%,1%), data goto L62625
L62625:
              convert str(desc$,11%,8%) to sale(kk%,2%), data goto L62635
L62635:
              convert str(desc$,22%,8%) to sale(kk%,3%), data goto L62645
L62645:
              sale(kk%,1%) = sale(kk%,1%) / 100.0
              sale(kk%,2%) = sale(kk%,2%) / 100.0
              str(sale$(kk%),1%,1%) = "*"       /* COST OF SALE EXISTS */
              goto load_sale_nxt
        load_sale_done
           readkey$ = " "
           str(readkey$,1%,9%) = "COST NONE"
        load_nocost_nxt
           read #4,key > readkey$, using L62590, readkey$, desc$,         ~
                                                eod goto load_nocost_done
           if str(readkey$,1%,9%) <> "COST NONE" then                    ~
                                                  goto load_nocost_done
              kk% = 0%
              convert str(readkey$,10%,3%) to kk%, data goto L62720
L62720:
              if kk% = 0% then goto load_nocost_nxt
              str(sale$(kk%),2%,1%) = "*"      /* SET DO NOT COST FLAG */
              goto load_nocost_nxt
        load_nocost_done
        return

        gen_special
            call "SHOSTAT" ("Creating High Volume Check List")

            gosub select_printer
            init(" ") part$, sav_key$, total$
            spec_title$ = "[Check Quantity Greater Than "&check_qty$&"]"
            end_of_file% = 0%
            spec% = 0%
            spec$ =  "SCANNED PRODUCT [ ######## ]"
L62800:     call "OPENCHCK" (#19, fs%(19%), f2%(19%),100%, rslt$(19%))
            read #19,key 5% > part$, using L62815, part$,                 ~
                                                eod goto gen_special_done
L62815:        FMT POS(76), CH(25)
            close #19
        gen_special_next
            spec% = spec% + 1%
            if mod(spec%,25%) <> 0 then goto L62855
               convert spec% to str(spec$,19%,8%), pic(########)
               print at(04,26);hex(84);spec$;

L62855:     if end_of_file% = 1% then goto gen_special_done
            gosub calc_special
            if mm$ <> "1" then goto L62890
               init(" ") part$
               str(part$,1%,1%) = "3"
        REM    GOTO GEN_SPECIAL_DONE
               goto L62800
L62890:     if mm$ <> "5" then goto L62910
               init(" ") part$
               str(part$,1%,1%) = "6"
               goto L62800
L62910:     if total < check_qty% then goto gen_special_next
               convert total to total$, pic(########.##-)

               call "APCDESCR" (sav_key$, apc_scr$, apc_prt$, apc_sze$,  ~
                                                               #6, err% )
               str(part_desc$,1%,16%) = str(apc_prt$,1%,16%)
               str(part_desc$,17%,16%)= str(apc_sze$,1%,16%)

               gosub print_special
               goto gen_special_next
        gen_special_done
           print using L55400
           close printer
        return clear all
        goto exit_program

        calc_special
            total = 0.0
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),  0%, rslt$(19%))
            sav_key$ = part$
            mm$ = str(part$,1%,1%)
            read #19,key 5% = part$, using L63045, postdate$, part$, qty, ~
                                              eod goto calc_special_end
            goto L63050
        calc_special_next
            read #19,using L63045, postdate$, part$, qty,                 ~
                                              eod goto calc_special_end
L63045:       FMT CH(6), POS(76), CH(25), POS(260), PD(14,4)
L63050:     if part$ <> sav_key$ then goto calc_special_done
            if mm$ = "1" or mm$ = "2" or mm$ = "5" then                  ~
                                                   goto calc_special_next
            if postdate$ < bg_dte$ or postdate$ > ed_dte$ then           ~
                                                   goto calc_special_next
            total = total + abs(qty)
            goto calc_special_next
        calc_special_done
          close #19
        return
        calc_special_end
          end_of_file% = 1%
          close #19
        return

        error_prompt
	   hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
	   comp% = 2%
	   msg$(2%) = errormsg$
	   msg$(3%) = "Press Any Key To Continue."
	   call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
	return

	open_error                                    /* (EWD004)        */
	   errormsg$ = "(Open Error) - File = " & filename$
	   gosub error_prompt
        return



        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" addr(#20)
            end
