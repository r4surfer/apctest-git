        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA23  (Like APCRGA03)            *~
            *  Creation Date     - 01/21/97                             *~
            *  Last Modified Date- 10/31/05                             *~
            *  Description       - This Program provides RGA Barcoding  *~
            *                      Data Entry/Edit for APCRGADT.        *~
            *                                                           *~
            *  Subroutines       - APCRG23B                             *~
            *                                                           *~
            *  Special Comments  - Uses NEW Planning Files              *~
            *                      Converted to Sub to run from         *~
            *                      APCSCANN                             *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/21/97 ! New Program for (APC) - Last Mod Date    ! JBF *~
	    * 02/26/98 ! Y2K COMPLIANCE                           ! DJD *~
            * 10/31/05 ! (AWD001) CR347 Mod for new Sub part      ! CMG *~
            *02/13/2019! CR-1894  Increase EMP DEPT size to 3     ! DES *~
            *************************************************************

            sub "APCRGA23"     (#3,      /* GENCODES                   */~
                                #5,      /* APCPLNAD                   */~
                                #14)     /* APCPLNDT                   */

        dim rga_cuscode$9,               /* RGA Customer No.           */~
            rga_dt_status$2,             /* RGA Item Status Code       */~
            rga_number$4,                /* RGA Number                 */~
            rga_item$2,                  /* RGA Item                   */~
            rga_compt$5,                 /* RGA Complaint No.          */~
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
            rga_dt_filler$13,            /* APCRGADT Filler Area       */~
            rga_hd_status$2,             /* RGA Item Status Code       */~
            rga_auth_id$3,               /* Authorizing Userid         */~
            rga_dte$8,                   /* RGA Enter Date             */~
            rga_filed_dte$8,             /* RGA Filed Date             */~
            rga_hd_desc_txt$4,           /* RGA Header Text Code       */~
            rga_hd_txt$1,                /* RGA Header Text Flag       */~
            rga_userid$3,                /* Userid of Entry/Mod        */~
            rga_mod_dte$8,               /* RGA Entry/Mod Date         */~
            rga_rg$1,                    /* RGA/RG Flag                */~
            rga_filler$29,               /* APCRGAHD Filler Area       */~
            or_so$8,                     /* APCPLNOR S.O. No.          */~
            or_load$5,                   /* APCPLNOR Load No.          */~
            or_po$16,                    /* APCPLNOR P.O. No.          */~
            or_inv$8,                    /* APCPLNOR Invoice No.       */~
            or_chk$8,                    /* APCPLNOR Check No.         */~
            or_hows$2,                   /* APCPLNOR How Shipped       */~
            bck_key$19,                  /* BCKMASTR/BCKLINES Key      */~
            bck_so$16,                   /* BCKLINES S.O No.           */~
            bck_part$25,                 /* BCKLINES Part No.          */~
            bck_text$4,                  /* BCKLINES Order Text ID     */~
            dt_part$25,                  /* APCPLNDT Part No.          */~
            dt_bar$18, dt_ref$8,         /* APCPLNDT Barcode/Warranty  */~
            dt_dept$3,                   /* APCPLNDT Department Code   */~
            dt_proc$2,                   /* APCPLNDT Process Code      */~
            dt_date$6,                   /* APCPLNDT Production Date   */~
            ad_rec$64, ad_rec1$64,       /* APCPLNAD Record            */~
            ad_key$33, ad_time$8,        /* APCPLNAD Read Key/Time     */~
            dt_load$5,                   /* APCPLNDT Load No.          */~
            dt_shft$2,                   /* APCPLNDT Shift             */~
            gencdkey$24,                 /* GENCODES File Read Key     */~
            readkey$24,                  /* GENCODES File Read Key     */~
            desc$30,                     /* GENCODES Part Description  */~
            dtlkey$6,                    /* APCRGADT File Read Key     */~
            mastr_key$25,                /* BCKMASTR File Read Key     */~
            or_key4$8,                   /* APCPLNOR File Alt Key 4    */~
            dt_key$23,                   /* APCPLNDT File Read Key     */~
            sls_regn$2,                  /* Sales Region for Costing   */~
            xx$(7%)50,                   /* Display Screen Text        */~
            gg$(7%)50,                   /* Scan Status Display Text   */~
            rga$(7%)50,                  /* Scan Barcode Text          */~
            barcode$18,                  /* Barcode No. Scanned        */~
            prevcode$18,                 /* Previous Barcode Entered   */~
            date$8,                      /* Date Scanned               */~
            dateout$8,                   /* Date for Screen Display    */~
            errormsg$60,                 /* Error message              */~
            inp_text$(2%)79,             /* Input Prompt Text          */~
            i$(24%)80,                   /* Detail Line(10) Array Area */~
            inpmessage$79,               /* Informational Message      */~
            cursor%(2%),                 /*                            */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pfkeys$32,                   /* PF Key Variable            */~
            progid$,                     /* Screen Line #2 Program ID  */~
            line2$79,                    /* Screen Line #2 Time Field  */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            userid$3                     /* Current User Id            */

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
            pc(36%),                     /* 36 PRICE SHEETS            */~
            cuscode$9,                   /* CUSTOMER CODE              */~
            sale(1000%,3%),              /* STORE 'COST SALE' VALUES   */~
            sale$(1000%)2                /* STORE NO COST FLAG         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 10/16/95 RGA Barcode Scanning/Update Prg"

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! APCRGAHD ! APC RGA Header Master File               *~
            * #02 ! APCRGADT ! APC RGA Detail Master File               *~
            * #03 ! GENCODES ! System Code Table File                   *~
            * #05 ! APCPLNAD ! Planning Master Audit File               *~
            * #06 ! APCPLNOR ! Group S.O. Header File                   *~
            * #12 ! BCKLINES ! S.O. Line Items File                     *~
            * #13 ! BCKMASTR ! S.O. Header File                         *~
            * #14 ! APCPLNDT ! Planning Master Tracking File            *~
            *************************************************************~
            *                  C O S T I N G   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #21 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #22 ! HNYMASTR ! Part Master File                         *~
            * #23 ! HNYQUAN  ! Inventory Quantities Master File         *~
            * #03 ! GENCODES ! Master Code Table File                   *~
            * #25 ! AMTBOMCD ! Master Equation File                     *~
            * #27 ! APCEMPLY ! Employee Master File                     *~
            * #28 ! APCEQUAT ! Equation an Parts Cross Reference File   *~
            * #29 ! APCCSTHP ! Hardware and Packaging Costing Components*~
            * #30 ! APCCSTLR ! Departments Average Hourly Rates         *~
            * #31 ! CPRPRICE ! Master System Price File                 *~
            * #32 ! CUSTOMER ! Master Customer File                     *~
            * #33 ! APCPCMST ! Pricing Definition file                  *~
            * #34 ! APCSKUNO ! Home Center's Skuno File                 *~
            * #35 ! APCPCMSK ! Pricing Key Definition File              *~
            * #36 ! APCPCMSD ! Pricing Master Calc Definition File      *~
            * #37 ! APCCSTEX ! APC COSTING EXCEPTION FILE               *~
            * #38 ! APCSTOCK ! APC STOCK MASTER FILE                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =   4,                     ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

            select #2,  "APCRGADT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   12, keylen =   6,                     ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

        REM SELECT #3,  "GENCODES",                                      ~
                        VARC,     INDEXED,  RECSIZE =  128,              ~
                        KEYPOS =    1, KEYLEN =   24

        REM SELECT #5,  "APCPLNAD",                                      ~
                        VARC,     INDEXED,  RECSIZE =   64,              ~
                        KEYPOS =   19, KEYLEN =  33,                     ~
                        ALT KEY  1, KEYPOS =   1, KEYLEN =  33

            select #6,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =  27,  keylen = 25,          ~
                            key  2, keypos =  70,  keylen =  8, dup,     ~
                            key  3, keypos =  78,  keylen =  8, dup,     ~
                            key  4, keypos =  52,  keylen =  8,          ~
                            key  5, keypos =  36,  keylen = 16, dup

            select #12, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #13, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

        REM SELECT #14, "APCPLNDT",                                      ~
                        VARC,     INDEXED,  RECSIZE =  256,              ~
                        KEYPOS =   24, KEYLEN =  23,                     ~
                        ALT KEY  1, KEYPOS =   47, KEYLEN =  57,         ~
                            KEY  2, KEYPOS =   53, KEYLEN =  51,         ~
                            KEY  3, KEYPOS =    1, KEYLEN =  23, DUP,    ~
                            KEY  4, KEYPOS =   96, KEYLEN =   8, DUP

        REM - COSTING FILES

            select #21, "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #22, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

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

            select #32, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

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
/*AWD001*/              varc,     indexed,  recsize =  768,              ~
                        keypos = 1,    keylen =   9


            select #37,  "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            select #38, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),   0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#5,  fs%(5%),  f2%(5%),   0%, rslt$(5%))
            call "OPENCHCK" (#6,  fs%(6%),  f2%(6%),   0%, rslt$(6%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),  0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),  0%, rslt$(13%))
        REM - COSTING FILES
            call "OPENCHCK" (#21, fs%(21%), f2%(21%),  0%, rslt$(21%))
            call "OPENCHCK" (#22, fs%(22%), f2%(22%),  0%, rslt$(22%))
            call "OPENCHCK" (#23, fs%(23%), f2%(23%),  0%, rslt$(23%))
            call "OPENCHCK" (#25, fs%(25%), f2%(25%),  0%, rslt$(25%))
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

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

            b_max%        = 10%
            progid$       = "APCRGA23: " & str(cms2v$,,8)
            prevcode$     = " "
            inp_text$(1%) = "Scan or Enter RGA Barcode Number"
            inp_text$(2%) = "Scan or Enter Original APC Barcode Number"

            rga$(1%) = "RRRRRR    GGGGG     AAAAA        #   #        "
            rga$(2%) = "R     R  G     G   A     A       #   #        "
            rga$(3%) = "R     R  G         A     A     #########      "
            rga$(4%) = "RRRRRR   G   GGG   AAAAAAA       #   #        "
            rga$(5%) = "R RR     G     G   A     A     #########      "
            rga$(6%) = "R  RR    G     G   A     A       #   #        "
            rga$(7%) = "R   RR    GGGGGG   A     A       #   #        "

            gg$(1%)  = "OOOOOOOOOO''''''OOOOOOK'''KKKKKK'''KKKKKKKKKKK"
            gg$(2%)  = "OOOOOOOO''''''''''OOOOK'''KKKKK'''KKKKKKKKKKKK"
            gg$(3%)  = "OOOOOO'''OOOOOOOO'''OOK'''KKK'''KKKKKKKKKKKKKK"
            gg$(4%)  = "OOOOO'''OOOOOOOOOO'''OK'''K'''KKKKKKKKKKKKKKKK"
            gg$(5%)  = "OOOOOO'''OOOOOOOO'''OOK'''KKK'''KKKKKKKKKKKKKK"
            gg$(6%)  = "OOOOOOOO''''''''''OOOOK'''KKKKK'''KKKKKKKKKKKK"
            gg$(7%)  = "OOOOOOOOOO''''''OOOOOOK'''KKKKKK'''KKKKKKKKKKK"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            init(" ") errormsg$, dateout$, dtlkey$, barcode$, wand1$,    ~
                wand2$, xx$()
            copy rga$() to xx$()
            option%   = 1%

            fieldnr%  = 1%
            lfac$(1%) = hex(81)
            lfac$(3%) = hex(99)
            lfac$(2%) = hex(84)
            lfac$(4%) = hex(84)
            gosub scan_1

            fieldnr%  = 2%
            lfac$(1%) = hex(84)
            lfac$(3%) = hex(84)
            lfac$(2%) = hex(81)
            lfac$(4%) = hex(99)
            wand1$    = " "
            gosub scan_2

            goto inputmode

        scan_1
            gosub'100(fieldnr%,option%)

            if keyhit% =  1%        then startover
            if keyhit% = 16%        then exit_program
            if keyhit% <> 0%        then scan_1
            gosub check_rga
                if errormsg$ <> " " then scan_1

        return

        scan_2
            gosub'100(fieldnr%,option%)

            if keyhit% =  1%        then startover
            if keyhit% = 16%        then exit_program
            if keyhit% <> 0%        then scan_2
            gosub edit_barcode
                if errormsg$ <> " " then scan_2

            gosub dataput
                if errormsg$ <> " " then scan_2

            prevcode$ = barcode$
            gosub ok_scan

        return

        REM *************************************************************~
            * Display this Screen if Barcode is scanned and No Errors.  *~
            *************************************************************
        ok_scan
            errormsg$ = all(hex(20))
            print at(04,02);hex(84);gg$(1%)
            print at(11,17);hex(84);gg$(1%)
            print at(12,17);hex(84);gg$(2%)
            print at(13,17);hex(84);gg$(3%)
            print at(14,17);hex(84);gg$(4%)
            print at(15,17);hex(84);gg$(5%)
            print at(16,17);hex(84);gg$(6%)
            print at(17,17);hex(84);gg$(7%)
            for i% = 1% to b_max%
                print at (13,75); bell;

            next i%

            call "PAUSE" addr(100%)

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
            errormsg$ = " "
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            rec% = 0%
            read #2,key = dtlkey$, eod goto L30470

            get #2, using L35030,                                         ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_dt_status$,     /* RGA Item Status            */~
                     rga_number$,        /* RGA Number                 */~
                     rga_item$,          /* RGA Item                   */~
                     rga_compt$,         /* RGA Complaint No.          */~
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
                     rga_dt_filler$      /* APCRGADT Filler Area       */

            rec% = 1%
L30470:     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            read #2,hold,key = dtlkey$, eod goto L31800
            delete #2

            if rga_mat_cost >  0        then L31110
            if rga_part$   <> " "       then gosub rga_cost
L31110:     rga_dt_status$   =  "12"
            rga_dt_userid$   =  userid$
            rga_dt_mod_dte$  =  date

            put #2, using L35030,                                         ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_dt_status$,     /* RGA Item Status            */~
                     rga_number$,        /* RGA No.                    */~
                     rga_item$,          /* RGA Item                   */~
                     rga_compt$,         /* RGA Complaint No.          */~
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
                     rga_dt_filler$      /* APCRGADT Filler Area       */

            write #2, eod goto L31800

        REM *************************************************************~
            *                                                           *~
            *  Read APCRGAHD for RGA_NUMBER TO SEE IF Status has been   *~
            *  modified to reflect scanned details ("04" - Open).       *~
            *                                                           *~
            *************************************************************
            read  #1,key = rga_number$, eod goto L31790

            get   #1, using L35670,                                       ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_hd_status$,     /* Status of RGA Header       */~
                     rga_number$,        /* RGA Number                 */~
                     rga_auth_id$,       /* Authorizing Userid         */~
                     rga_dte$,           /* RGA Enter Date             */~
                     rga_filed_dte$,     /* RGA Filed Date             */~
                     rga_hd_desc_txt$,   /* RGA Header Text Code       */~
                     rga_hd_txt$,        /* RGA Header Text Flag       */~
                     rga_userid$,        /* Userid of Entry/Mod        */~
                     rga_mod_dte$,       /* RGA Entry/Mod Date         */~
                     rga_rg$,            /* RGA/RG Flag                */~
                     rga_filler$         /* APCRGAHD Filler Area       */

            if rga_hd_status$ > "03"  then L31780
            gosub update_header
L31780:     if dt_proc$      <> " "   then gosub update_audit
L31790: return
L31800:     errormsg$ = "(In Use) RGA Record In Use.  Try again later?"
        return

        update_header
            read #1,hold,key = rga_number$, eod goto L32070
            delete #1

            rga_hd_status$ = "04"
            rga_userid$    = userid$
            rga_mod_dte$   = date

            put    #1, using L35670,                                      ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_hd_status$,     /* Status of RGA Header       */~
                     rga_number$,        /* RGA Number                 */~
                     rga_auth_id$,       /* Authorizing Userid         */~
                     rga_dte$,           /* RGA Enter Date             */~
                     rga_filed_dte$,     /* RGA Filed Date             */~
                     rga_hd_desc_txt$,   /* RGA Header Text Code       */~
                     rga_hd_txt$,        /* RGA Header Text Flag       */~
                     rga_userid$,        /* Userid of Entry/Mod        */~
                     rga_mod_dte$,       /* RGA Entry/Mod Date         */~
                     rga_rg$,            /* RGA/RG Flag                */~
                     rga_filler$         /* APCRGAHD Filler Area       */

            write  #1, eod goto L32070

L32070: return

        update_audit
            init(" ") ad_rec$, ad_time$, ad_key$, ad_rec1$
            call "TIME" (ad_time$)
            str(ad_rec$,1%,18%) = barcode$                /* Barcode   */
            str(ad_rec$,19%,6%) = date$                   /* Scan Date */
            str(ad_rec$,25%,3%) = "114"                   /* Department*/
            str(ad_rec$,28%,2%) = dt_proc$                /* Process   */
            str(ad_rec$,30%,2%) = dt_shft$                /* Shift Code*/
            str(ad_rec$,32%,2%) = "50"                    /* Status    */
            str(ad_rec$,34%,18%)= barcode$                /* Barcode   */
            str(ad_rec$,52%,8%) = ad_time$                /* Time Stamp*/
            str(ad_rec$,60%,3%) = userid$                 /* User Id   */
            str(ad_rec$,63%,2%) = "  "                    /* Filler    */
            ad_key$ = str(ad_rec$,19%,33%)
            read   #5, hold, key = ad_key$, using L32270, ad_rec1$,       ~
                eod goto L32260
            delete #5
L32260:     put    #5, using L32270, ad_rec$
L32270:         FMT CH(64)
            write  #5, eod goto L32310

        return
L32310:      errormsg$ = "Error - Unable to Update AUDIT File!!!"
        return

        rga_dispersing
            call "APCRG23B" ( #01,       /*   (APCRGAHD)               */~
                              #02,       /*   (APCRGADT)               */~
                              #03 )      /*   (GENCODES)               */

        return clear all
        goto inputmode

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
                CH(13)                   /* APCRGADT Filler Area       */

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
                CH(02),                  /* APCPLNDT Shift             */~
                XX(35),  PD(14,4),       /* APCPLNDT Material Cost     */~
                PD(14,4),                /* APCPLNDT Labor Cost        */~
                PD(14,4),                /* APCPLNDT Overhead Cost     */~
                PD(14,4),                /* APCPLNDT Freight Cost      */~
                PD(14,4),                /* APCPLNDT Vinyl Discount    */~
                XX(08),  CH(25)          /* APCPLNDT Part No.          */

L35640:     FMT POS(10), CH(16),         /* BCKMASTR S.O. No.          */~
                CH(16)                   /* BCKMASTR P.O. No.          */

L35670:     FMT CH(09),                  /* RGA Customer               */~
                CH(02),                  /* RGA Header Status          */~
                CH(04),                  /* RGA Number                 */~
                CH(03),                  /* Authorizing Userid         */~
                CH(08),                  /* RGA Entry Date             */~
                CH(08),                  /* RGA Filed Date             */~
                CH(04),                  /* RGA Header Text Code       */~
                CH(01),                  /* RGA Header Text Flag       */~
                CH(03),                  /* Userid of Mod/Entry        */~
                CH(08),                  /* RGA Mod/Entry Date         */~
                CH(01),                  /* RGA/RG Flag                */~
                CH(29)                   /* APCRGAHD Filler Area       */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'100(fieldnr%,option%)
            dateout$ = " "
            call "TIME" (dateout$)

            str(line2$,72%) = dateout$
            inpmessage$     = inp_text$(fieldnr%)
            gosub set_pfkeys

            accept                                                       ~
                at (01,02)                                      ,        ~
                   "RGAII Barcode Scanning"                     ,        ~
                at (01,36), "APC Building Products"             ,        ~
                at (01,66), "Today:"                            ,        ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                                                                         ~
                at (02,02), fac(hex(ac)),   line2$              , ch(79),~
                                                                         ~
                at (04,02), fac(hex(94)),   errormsg$           , ch(79),~
                                                                         ~
                at (05,02), "RGA No.    : "                     ,        ~
                at (05,16), fac(lfac$(1%)), dtlkey$             , ch(06),~
                at (05,23), fac(lfac$(3%)), wand1$              , ch(01),~
                                                                         ~
                at (06,02), "Barcode No.: "                     ,        ~
                at (06,16), fac(lfac$(2%)), barcode$            , ch(18),~
                at (06,35), fac(lfac$(4%)), wand2$              , ch(01),~
                at (06,38), "Last Barcode No.: "                ,        ~
                at (06,57), fac(hex(84)),   prevcode$           , ch(18),~
                                                                         ~
                at (11,17), fac(hex(84)),   xx$(1%)             , ch(50),~
                at (12,17), fac(hex(84)),   xx$(2%)             , ch(50),~
                at (13,17), fac(hex(84)),   xx$(3%)             , ch(50),~
                at (14,17), fac(hex(84)),   xx$(4%)             , ch(50),~
                at (15,17), fac(hex(84)),   xx$(5%)             , ch(50),~
                at (16,17), fac(hex(84)),   xx$(6%)             , ch(50),~
                at (17,17), fac(hex(84)),   xx$(7%)             , ch(50),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <>  3% then L40520
                     gosub rga_dispersing

L40520:         if keyhit% <> 15% then L40550
                     call "PRNTSCRN"

L40550:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            return

        set_pfkeys
        REM                          /*  Input Mode             */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "(3)RGA Dispersing Scan                   " &       ~
                     "                      (16)Exit Program"
            pfkeys$ = hex(01ff03ffffffffffffffffffffff0f1000)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        check_rga
            errormsg$ = " "
            convert dtlkey$ to dtlkey%, data goto L50130

            convert dtlkey% to dtlkey$, pic(000000)

            gosub dataload
            if rec%       = 1%       then L50150
L50130:         errormsg$ = "Invalid RGA Number! Re-enter or Scan Again."
                goto L50190
L50150:     if rga_dt_status$ > "11" then L50180
                errormsg$ = " "
                goto L50190
L50180:         errormsg$ = "RGA ("& dtlkey$ &") Already Scanned!!!"
L50190: return

        edit_barcode
            errormsg$  = " "
            dt_bar$    = barcode$
            rga_so$    = str(barcode$, 1%,8%)
            rga_line$  = str(barcode$, 9%,2%)
            rga_piece$ = str(barcode$,11%,4%)
            rga_qty$   = str(barcode$,15%,4%)
            gosub lookup_apcplndt
            gosub lookup_apcplnor4

            if rga_part$     <> " " then L50330
                gosub lookup_bcklines
L50330:     if rga_salesman$ <> " " then L50350
                gosub lookup_salesman
L50350:     if rga_credit     =  0  then gosub lookup_price
        return

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************
        lookup_apcplnor4
            init(" ") or_key4$
            or_key4$ = rga_so$
            read #6,key 4% = or_key4$, eod goto check_bckmastr_so
            get #6, using L35390,                                         ~
                or_po$,                  /* APCPLNOR P.O. No.          */~
                or_so$,                  /* APCPLNOR S.O. No.          */~
                or_inv$,                 /* APCPLNOR Invoice No.       */~
                or_chk$,                 /* APCPLNOR Check No.         */~
                or_hows$,                /* APCPLNOR How shipped code  */~
                or_load$                 /* APCPLNOR Load No.          */

            rga_po$   = or_po$
            rga_inv$  = or_inv$
            rga_chk$  = or_chk$
            rga_hows$ = or_hows$
            rga_load$ = or_load$
        return

        check_bckmastr_so
            init(" ") mastr_key$
            str(mastr_key$, 1%,9%) = rga_cuscode$
            str(mastr_key$,10%,8%) = rga_so$
            read #13, key = mastr_key$, using L35640, bck_so$, bck_po$,   ~
                eod goto L60310

            rga_po$ = bck_po$
        return
L60310:     str(rga_so$,1%,1%) = "*"
            if rga_po$ <> " " then str(rga_po$,1%,1%) = "*"
        return

        lookup_bcklines
            bck_key$            = all(hex(20))
            str(bck_key$,1%,8%) = rga_so$
            convert rga_line$  to xx%, data goto L60390
L60390:
            convert xx%        to str(bck_key$,17%,3%), pic(###)

            read #12,key = bck_key$, using L35460, bck_so$, bck_part$,    ~
                bck_text$, eod goto L60460

            rga_part$    = bck_part$
L60460: return

        lookup_apcplndt
            init(" ") dt_key$
            str(dt_key$, 1%,18%) = dt_bar$
            str(dt_key$,19%, 5%) = " "

        next_apcplndt
            read #14,key > dt_key$, using L35500, dt_load$, dt_bar$,      ~
                dt_dept$, dt_proc$, dt_date$, dt_ref$, dt_shft$, dt_mat, ~
                dt_labor, dt_over,  dt_frght, dt_vdisc, dt_part$,        ~
                eod goto L60760

            if dt_bar$ <> str(dt_key$,1%,18%)    then L60760
            rga_part$       = dt_part$
            rga_prod_dte$   = dt_date$
            rga_mat_cost    = dt_mat
            rga_labor_cost  = dt_labor
            rga_overhd_cost = dt_over
            rga_frt_cost    = dt_frght
            rga_vinyl_disc  = dt_vdisc
            if rga_dept$    > " "                then L60760
            gosub lookup_department

            if rga_dept$ > " "                   then L60760
            init(" ") rga_dept$, dept_desc$, dt_key$
            str(dt_key$, 1%,18%) = dt_bar$
            str(dt_key$,19%, 3%) = dt_dept$
            str(dt_key$,22%, 2%) = dt_proc$
            goto next_apcplndt
L60760: return

        lookup_department
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN SUPP"
            str(readkey$,10%,15%) =  dt_dept$
            read #3,key = readkey$, using L60840, dept_desc$,             ~
                eod goto L60870
L60840:         FMT POS(25), CH(30)

            goto L60880
L60870:     rga_dept$             =  dt_dept$
L60880: return

        lookup_price
            if rga_so$ = " "               then L61170
            if str(rga_load$,1%,1%)  = "S" then L61170   /* SKIP STOCK */
            ord_disc = 0.0 : ln_disc = 0.0 : line_tot = 0.0
            init(" ") bck_key$
            str(bck_key$,1%,9%)  = rga_cuscode$
            str(bck_key$,10%,8%) = rga_so$
            read #13,key = bck_key$, using L60990, ord_disc,              ~
                eod goto L61170
L60990:         FMT POS(859), PD(14,4)

            init(" ") bck_key$
            str(bck_key$,1%,16%) = rga_so$
            convert rga_line$ to xx%, data goto L61040
L61040:
            convert xx%       to str(bck_key$,17%,3%), pic(###)

            read #12,key = bck_key$, using L61090, ord_price, ln_disc,    ~
                eod goto L61170
L61090:         FMT POS(165), 2*PD(14,4)

            ord_price  = round(ord_price * 1.0, 2)
            discamt    = round(ord_price * ln_disc * .01, 2)
            line_tot   = round(ord_price - discamt, 2)
            discamt    = round(line_tot  * ord_disc * .01, 2)
            line_tot   = round(line_tot  - discamt, 2)
            rga_credit = line_tot
L61170: return

        lookup_salesman
            read #32,key = rga_cuscode$, using L61220, rga_salesman$,     ~
                eod goto L61240
L61220:         FMT POS(714), CH(04)

L61240: return

        rga_cost
            cuscode$ = rga_cuscode$
            part$    = rga_part$
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
            width = 0
            convert str(part$,13%,4%) to width, data goto L61450
L61450:
            kk%   = 0%
            convert str(part$,1%,3%)  to kk%,   data goto L61480
L61480:
            kk%   = kk% + 1%
            price  = 0%
            p_err% = 0%
            x_err% = 0%
            calc%  = 0%
            if len(part$)       < 19  then x_err% = 2%  /*PROD IS PART */
            if str(part$,1%,1%) = "4" then x_err% = 2%         /* PART */
            if width            =  0  then x_err% = 2%         /* PART */
            if x_err%           <> 0% then L62170

            if str(sale$(kk%),2%,1%) = "*" then calc% = 8% /*PRICE ONLY*/
            call "APCCST0B" ( calc%,     /* Calculation Method         */~
                              part$,     /* MFG Part Number            */~
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
                              "EM0100",  /* Customer Code for Pricing  */~
                              pc(),      /* 35 Prices                  */~
                              price,     /* Calc. Price for Customer   */~
                              #21,       /*   (APCCUTEQ)               */~
                              #22,       /*   (HNYMASTR)               */~
                              #23,       /*   (HNYQUAN )               */~
                              #3,        /*   (GENCDSIN)               */~
                              #25,       /*   (AMTBOMCD)               */~
                              #27,       /*   (APCEMPLY)               */~
                              #28,       /*   (APCEQUAT)               */~
                              #29,       /*   (APCCSTHP)               */~
                              #30,       /*   (APCCSTLR)               */~
                              #31,       /*   (CPRPRICE)               */~
                              #32,       /*   (CUSTOMER)               */~
                              #33,       /*   (APCPCMST)               */~
                              #34,       /*   (APCSKUNO)               */~
                              #35,       /*   (APCPCMSK)               */~
                              #36,       /*   (APCPCMSD)               */~
                              #37,       /*   (APCCSTEX)               */~
                              #38,       /*   (APCSTOCK)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */

            if str(sale$(kk%),2%,1%) <> "*" then L62090
                x_err% = 3%
                p_err% = 0%
                goto L62170

L62090:     for i% = 1% to 20%
                if apc_err%(i%) = 0%        then L62150
                     p_err%     = i%
                     x_err%     = 1%
                     cst_err%   = cst_err% + 1%

L62150:     next i%

L62170:     gosub store_cost                  /* Gather All MFG Costs  */

        return

        store_cost
            gosub calc_transport

            sls_price = price
            sls_qty   = 1
            trn_amt = 0.0
            mat cst = zer                    /* Total Vinyl and Misc.  */
                                             /* (Mat'l + Scrap) Cost   */
            tot_mat = round(tc(3%)  + tc(6%) + tc(9%) + tc(12%) + tc(15%)~
                          + tc(18%) + tc(21%) + tc(19%) - tc(20%), 4)
                                                  /*Include W/F Amount */
                                                  /*Include Freight    */
            if x_err% > 1% then gosub compute_cost
                                     /* Use Cost from Costing Errors   */
            tot_cst = round( tot_mat + lab(8%) + lab(9%), 4)
                                     /* Note - CST() are the Values    */
                                     /*   Assoc. with each Line Item   */
            cst(1%) = round(tot_mat * sls_qty, 2) /*Total Material     */
            cst(2%) = round(lab(8%) * sls_qty, 2) /*Total Dir/Ind Labor*/
            cst(3%) = round(lab(9%) * sls_qty, 2) /*Total Overhead Cost*/
            cst(4%) = round(tc(19%) * sls_qty, 2) /*Total Freight Cost */
            cst(5%) = round(tc(20%) * sls_qty, 2) /*Total Vinyl Disc't */
            cst(6%) = round(tot_cst * sls_qty, 2) /*Total MFG Cost     */
            cst(7%) = round(trn_amt * sls_qty, 2) /*Total Trans Cost   */
            cst(8%) = round(sls_price, 2)         /*Total Price W/Disc */
            cst(9%) = round(sls_qty, 2)           /*Total Quantity     */
        return

        calc_transport
            gencdkey$ = " " : sls_regn% = 0% : trn_amt = 0.0
            str(gencdkey$,1%,9%)   = "COST TRAN"
            str(gencdkey$,10%,15%) = cuscode$
            read #3,key = gencdkey$, using L62540, descr$, eod goto L62580
L62540:         FMT POS(25), CH(30)

            convert str(descr$,1%,2%) to sls_regn%, data goto L62580

L62580:     convert sls_regn%         to sls_regn$, pic(00)

            gencdkey$ = " "
            str(gencdkey$,1%,9%)   = "COST REGN"
            str(gencdkey$,10%,15%) = sls_regn$
            read #3,key = gencdkey$, using L62540, descr$, eod goto L62670

            convert str(descr$,1%,8%) to trn_amt, data goto L62670

L62670: return

        compute_cost
            tot_mat = 0.0
            mat lab = zer : mat tc = zer
            if str(sale$(kk%),1%,1%) <> "*" then kk% = 1%
            if x_err%     <> 2% then L62800
                                                  /* Product is a Part */
            if sls_price = 0    then tot_mat = sale(kk%,3%)              ~
                                else tot_mat = sls_price * sale(kk%,2%)
            if tot_mat   = 0    then L62800
        return
                                                   /* Costing Error    */
L62800:     tot_mat = (pc(1%) * .50) * sale(kk%,1%)/* Calc based on the*/
                                                   /* Catalog Price    */
            if tot_mat = 0      then tot_mat = sls_price * sale(kk%,1%)
            if tot_mat = 0      then tot_mat = sale(kk%,3%)
        return

        load_sale
            call "SHOSTAT" ("Loading Costing Tables")

            mat sale = zer
            init(" ") readkey$, sale$()
            str(readkey$,1%,9%) = "COST SALE"

        load_sale_nxt
            read #32,key > readkey$, using L62960, readkey$, desc$,       ~
                eod goto load_sale_done
L62960:         FMT CH(24), CH(30)

           if str(readkey$,1%,9%) <> "COST SALE" then load_sale_done
                kk% = 0%
                convert str(readkey$,10%,3%) to kk%, data goto L63010
L63010:
                kk% = kk% + 1%
                convert str(desc$,1%,8%)  to sale(kk%,1%), data goto L63040
L63040:
                convert str(desc$,11%,8%) to sale(kk%,2%), data goto L63060
L63060:
                convert str(desc$,22%,8%) to sale(kk%,3%), data goto L63080
L63080:
                sale(kk%,1%)          = sale(kk%,1%) / 100.0
                sale(kk%,2%)          = sale(kk%,2%) / 100.0
                str(sale$(kk%),1%,1%) = "*"     /* COST OF SALE EXISTS */
                goto load_sale_nxt

        load_sale_done
            readkey$ = " "
            str(readkey$,1%,9%) = "COST NONE"
        load_nocost_nxt
            read #32,key > readkey$, using L62960, readkey$, desc$,       ~
                eod goto load_nocost_done

            if str(readkey$,1%,9%) <> "COST NONE" then load_nocost_done
            kk% = 0%
            convert str(readkey$,10%,3%) to kk%, data goto L63240
L63240:
            kk% = kk% + 1%
            str(sale$(kk%),2%,1%) = "*"      /* SET DO NOT COST FLAG */
            goto load_nocost_nxt
        load_nocost_done
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            close #1  : close #2  : close #6  : close #12 : close #13
            close #23 : close #25 : close #27 : close #28 : close #29
            close #30 : close #31 : close #33 : close #34 : close #35
            close #36 : close #37 : close #38 : close #21 : close #32
            close #22

        end
