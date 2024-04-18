        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA03                             *~
            *  Creation Date     - 10/12/95                             *~
            *  Last Modified Date- 10/31/05                             *~
            *  Description       - This Program provides RGA Barcoding  *~
            *                      Data Entry/Edit for APCRGADT.        *~
            *                                                           *~
            *  Subroutines       - APCRGA3B                             *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/12/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 09/29/97 ! Mod - Replace (APCORDER) with (APCPLNOR) ! RHH *~
            *          !       Replace (APCMAST ) with (APCPLNLD) !     *~
            *          !       Replace (APCPLNTK) with (APCPLNDT) !     *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID To 60403      ! DJD *~
            *          !                                          !     *~
            * 09/28/00 ! Mod to support new alph rga numbers      ! CMG *~
            *          !      (EWD001)                            !     *~
            * 10/31/05 ! (AWD002) CR347 Mod for Sub Part          ! CMG *~
            *02/22/2019! CR-1894 Increase EMP DEPT size to 3      ! DES *~
            *************************************************************

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
            rga_how_ship$2,              /* RGA How Shipped Code       */~
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
/*PAR000*/  rga_dt_subp$20,              /* RGA Item Sub Part          */~
/*PAR000*/  rga_dt_filler$249,           /* APCRGADT Filler Area       */~
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
            grp_cuscode$9,               /* APCORDER Customer No.      */~
            grp_so$8,                    /* APCORDER S.O. No.          */~
            grp_load$5,                  /* APCORDER Load No.          */~
            grp_po$16,                   /* APCORDER P.O. No.          */~
            grp_inv$8,                   /* APCORDER Invoice No.       */~
            grp_chk$8,                   /* APCORDER Check No.         */~
            grp_howship$10,              /* APCORDER How Shipped       */~
            dtlkey$6,                    /* APCRGADT File Read Key     */~
            bck_key$19,                  /* BCKMASTR/BCKLINES Key      */~
            bck_so$16,                   /* BCKLINES S.O No.           */~
            bck_part$25,                 /* BCKLINES Part No.          */~
            bck_text$4,                  /* BCKLINES Order Text ID     */~
            apc_prod$8,                  /* APCMAST  Production Date   */~
            gencdkey$25,                 /* GENCODES File Read Key     */~
            order_key1$17,               /* APCORDER File Alt Key 1    */~
            plntk_key$23,                /* APCPLNTK File Read Key     */~
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

        dim f2%(64%),                                                    ~
            f1%(64%),                                                    ~
            fs%(64%),                                                    ~
            rslt$(64%)20

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

	dim blankdate$6,		 /* Blank (empty) date         */~
	    workdate10$10,		 /* Century date mm-dd-yyyy    */~
	    workdate8$8			 /* Regular date mm-dd-yy      */

/* PAR000 */
        dim bcksubpt_key$11,             /* BCKSUBPT Read key          */~
            part$25,                     /* PART Number                */~
            partno1$20                   /* Sub Part Number            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 RGA Barcode Scanning/Update Prg"

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! APCRGAHD ! APC RGA Header Master File               *~
            * #02 ! APCRGADT ! APC RGA Detail Master File               *~
            * #03 ! GENCODES ! System Code Table File                   *~
            * #6  ! APCPLNOR ! S.O. Header File (APCORDER) - Old        *~
            * #8  ! APCPLNLD ! EWD Load Master File                     *~
            * #12 ! BCKLINES ! S.O. Line Items File                     *~
            * #13 ! BCKMASTR ! S.O. Header File                         *~
            * #14 ! APCPLNDT ! Production Master Detail File.           *~
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
            * #39 ! APCPLNDP ! Planning Master Department File          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =   4,                     ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

            select #2,  "APCRGADT",                                      ~
/*PAR000*/              varc,     indexed,  recsize =  512,              ~
                        keypos =   12, keylen =   6,                     ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            select #6,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =  27,  keylen = 25,          ~
                            key  2, keypos =  70,  keylen =  8, dup,     ~
                            key  3, keypos =  78,  keylen =  8, dup,     ~
                            key  4, keypos =  52,  keylen =  8,          ~
                            key  5, keypos =  36,  keylen = 16, dup

            select #8,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   11, keylen =    5,                    ~
                        alt key  1, keypos =   3,  keylen = 13,          ~
                            key  2, keypos =   1, keylen =  15

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
/*AWD002*/              varc,     indexed,  recsize =  768,              ~
                        keypos = 1,    keylen =   9


            select #37,  "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            select #38, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32


/*PAR000*/
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup     


            select #39,  "APCPLNDP",                                     ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =   11, keylen =   12,                    ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos =    4, keylen =  12,         ~
                            key  3, keypos =    1, keylen =  15


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),   0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#6,  fs%(6%),  f2%(6%),   0%, rslt$(6%))
            call "OPENCHCK" (#8,  fs%(8%),  f2%(8%),   0%, rslt$(8%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),  0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),  0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),  0%, rslt$(14%))
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
            call "OPENCHCK" (#39, fs%(39%), f2%(39%),  0%, rslt$(39%))
/*PAR000*/  call "OPENCHCK" (#63, fs%(63%), f2%(63%),  0%, rslt$(63%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

	    call "DATUFMTC" (blankdate$)

            b_max%        = 10%
            progid$       = "APCRGA03: " & str(cms2v$,,8)
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
                     rga_how_ship$,      /* RGA How Shipped Code       */~
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
/*PAR000*/           rga_dt_subp$,       /* RGA Item Sub Part          */~
                     rga_dt_filler$      /* APCRGADT Filler Area       */

            rec% = 1%
L30470:     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            read #2,hold,key = dtlkey$, eod goto L31810

            delete #2

            if rga_salesman$ =  " " then gosub lookup_salesman
            if rga_part$     <> " " then gosub rga_cost
            rga_dt_status$   =  "12"
            rga_dt_userid$   =  userid$
            rga_dt_mod_dte$  =  date$

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
                     rga_how_ship$,      /* RGA How Shipped Code       */~
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

            write #2, eod goto L31810

        REM *************************************************************~
            *                                                           *~
            *  Read APCRGAHD for RGA_NUMBER TO SEE IF Status has been   *~
            *  modified to reflect scanned details ("04" - Open).       *~
            *                                                           *~
            *************************************************************
            read  #1,key = rga_number$, eod goto L31800

            get   #1, using L35510,                                       ~
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

            if rga_hd_status$ > "03" then L31800
            gosub update_header

L31800: return
L31810:     errormsg$ = "(In Use) RGA Record In Use.  Try again later?"
        return

        update_header
            read #1,hold,key = rga_number$, eod goto L32090

            delete #1

            rga_hd_status$ = "04"
            rga_userid$    = userid$
            rga_mod_dte$   = date$

            put    #1, using L35510,                                      ~
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

            write  #1, eod goto L32090

L32090: return

        rga_dispersing
            call "APCRGA3B" ( #01,       /*   (APCRGAHD)               */~
                              #02,       /*   (APCRGADT)               */~
                              #03 )      /*   (GENCDSIN)               */

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
/*PAR000*/      CH(20),                  /* RGA Item Subpart           */~
/*PAR000*/      CH(249)                   /* APCRGADT Filler Area       */

L35390:     FMT POS(27), CH(09),         /* APCPLNOR Customer No.      */~
                CH(16),                  /* APCPLNOR P.O. No.          */~
                CH(08),                  /* APCPLNOR S.O. No.          */~
                XX(10),  CH(08),         /* APCPLNOR Invoice No.       */~
                CH(08),                  /* APCPLNOR Check No.         */~
                XX(06),  CH(02),         /* APCPLNOR Howshipped Code   */~
                CH(05)                   /* APCPLNOR Load No.          */


L35470:     FMT POS(10), CH(16),         /* BCKLINES S.O. No.          */~
                XX(06),  CH(25),         /* BCKLINES Part No.          */~
                XX(186), CH(04)          /* BCKLINES Text ID           */

L35510:     FMT CH(09),                  /* RGA Customer               */~
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
            errormsg$ = " "              /* EWD001) */
            convert str(dtlkey$,3%,4%) to dtlkey%, data goto L50140

            convert dtlkey% to str(dtlkey$,3%,4%), pic(0000)

            gosub dataload

            if rec%       = 1%       then L50160
L50140:         errormsg$ = "Invalid RGA Number! Re-enter or Scan Again."
                goto L50200
L50160:     if rga_dt_status$ > "11" then L50190
                errormsg$ = " "
                goto L50200
L50190:         errormsg$ = "RGA ("& dtlkey$ &") Already Scanned!!!"
L50200: return

        edit_barcode
            errormsg$  = " "
            rga_so$    = str(barcode$, 1%,8%)
            rga_line$  = str(barcode$, 9%,2%)
            rga_piece$ = str(barcode$,11%,4%)
            rga_qty$   = str(barcode$,15%,4%)
            gosub lookup_apcorder1
                if errormsg$ <> " " then L50360

            if rga_dept$     =  " " then gosub lookup_apcplntk
            if rga_part$     =  " " then gosub lookup_bcklines
/*PAR000*/  if rga_dt_subp$  =  " " then gosub lookup_bcksubpt
            if rga_prod_dte$ =  " " or rga_prod_dte$ = blankdate$ then gosub lookup_apcmast
            if rga_credit    =  0   then gosub lookup_price

L50360: return

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************
        lookup_apcorder1
            order_key1$ = " "
            str(order_key1$,1%,9%)  = rga_cuscode$
            str(order_key1$,10%,8%) = rga_so$
            read #6,key 1% = order_key1$, eod goto L60250

            get #6, using L35390,                                         ~
                grp_cuscode$,            /* APCPLNOR Customer No.      */~
                grp_po$,                 /* APCPLNOR P.O. No.          */~
                grp_so$,                 /* APCPLNOR S.O. No.          */~
                grp_inv$,                /* APCPLNOR Invoice No.       */~
                grp_chk$,                /* APCPLNOR Check No.         */~
                grp_howship$,            /* APCPLNOR How shipped code  */~
                grp_load$                /* APCPLNOR Load No.          */

            str(rga_po$,1%,16%) = str(grp_po$,1%,16%)
            rga_load$           = grp_load$
            rga_inv$            = grp_inv$
            rga_chk$            = grp_chk$
            rga_how_ship$       = str(grp_howship$,1%,2%)
            errormsg$           = " "
        return
L60250:     gosub check_bcklines

        return

        check_bcklines
            bck_key$     = all(hex(20))
            read #12,key > bck_key$, using L60330, bck_so$,               ~
                eod goto L60370
L60330:         FMT POS(10), CH(16)

            if rga_so$   < str(bck_so$,1%,8%) then L60370
            errormsg$    = "(Error) - Invalid SALES ORDER NO. ?"
L60370: return

        lookup_bcklines
            bck_key$            = all(hex(20))
            str(bck_key$,1%,8%) = rga_so$
            convert rga_line$  to xx%, data goto L60430
L60430:
            convert xx%        to str(bck_key$,17%,3%), pic(###)

            read #12,key = bck_key$, using L35470, bck_so$, bck_part$,    ~
                bck_text$, eod goto L60500

            rga_part$    = bck_part$
L60500: return


/* PAR000 - begin */
        lookup_bcksubpt
            init(" ") bcksubpt_key$, rga_dt_subp$
            str(bcksubpt_key$,1%,8%) = rga_so$
            convert rga_line$  to xx%, data goto L60435
L60435:
            convert xx%        to str(bcksubpt_key$,9%,3%), pic(###)

            read #63,key = bcksubpt_key$, using L35470, rga_dt_subp$,  ~
                 eod goto bcksubpt_done


        bcksubpt_done
        return

/* PAR000 - END */

        lookup_apcmast
            read #8,key = rga_load$, using L60550, apc_prod$,             ~
                eod goto L60590
L60550:         FMT POS(70), CH(08)

            rga_prod_dte$ = apc_prod$

L60590: return

        lookup_apcplntk
            plntk_key$             = all(hex(20))
            str(plntk_key$, 1%,8%) = rga_so$
            str(plntk_key$, 9%,2%) = rga_line$
            str(plntk_key$,11%,4%) = rga_piece$
            str(plntk_key$,15%,4%) = rga_qty$

        next_apcplntk
            read #14,key > plntk_key$, using L60710, plntk_key$,          ~
                eod goto L60780
L60710:         FMT POS(24), CH(23)

            if str(plntk_key$, 1%,8%) <> rga_so$ then L60780
            if str(plntk_key$,19%,3%) = "011"                            ~
            or str(plntk_key$,19%,3%) = "021"    then next_apcplntk
            rga_dept$                 =  str(plntk_key$,19%,3%)
        return
L60780:     rga_dept$ = " "
        return

        lookup_price
            if rga_so$ = " "               then L61080
            if str(rga_load$,1%,1%)  = "S" then L61080   /* SKIP STOCK */
            ord_disc = 0.0 : ln_disc = 0.0 : line_tot = 0.0
            init(" ") bck_key$
            str(bck_key$,1%,9%)  = rga_cuscode$
            str(bck_key$,10%,8%) = rga_so$
            read #13,key = bck_key$, using L60900, ord_disc,              ~
                eod goto L61080
L60900:         FMT POS(859), PD(14,4)

            init(" ") bck_key$
            str(bck_key$,1%,16%) = rga_so$
            convert rga_line$ to xx%, data goto L60950
L60950:
            convert xx%       to str(bck_key$,17%,3%), pic(###)

            read #12,key = bck_key$, using L61000, ord_price, ln_disc,    ~
                eod goto L61080
L61000:         FMT POS(165), 2*PD(14,4)

            ord_price  = round(ord_price * 1.0, 2)
            discamt    = round(ord_price * ln_disc * .01, 2)
            line_tot   = round(ord_price - discamt, 2)
            discamt    = round(line_tot  * ord_disc * .01, 2)
            line_tot   = round(line_tot  - discamt, 2)
            rga_credit = line_tot
L61080: return

        lookup_salesman
            read #32,key = rga_cuscode$, using L61130, rga_salesman$,     ~
                eod goto L61150
L61130:         FMT POS(714), CH(04)

L61150: return

        rga_cost
            cuscode$ = rga_cuscode$
            part$    = rga_part$
/*PAR000*/  partno1$ = rga_dt_subp$                    
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
            convert str(part$,13%,4%) to width, data goto L61350
L61350:
            kk%   = 0%
            convert str(part$,1%,3%)  to kk%,   data goto L61380
L61380:
            kk%   = kk% + 1%
        REM CUSCODE$ = SHIPTO$
        REM IF SLS_QTY <> 0 THEN 62350
        REM     MAT CST = ZER                    /* NO PRODUCT SHIPPED */
        REM     GOSUB LOAD_FOB                   /* NO COST            */
        REM RETURN

            price  = 0%
            p_err% = 0%
            x_err% = 0%
            calc%  = 0%
            if len(part$)       < 19  then x_err% = 2%  /*PROD IS PART */
            if str(part$,1%,1%) = "4" then x_err% = 2%         /* PART */
            if width            =  0  then x_err% = 2%         /* PART */
            if x_err%           <> 0% then L62130

            if str(sale$(kk%),2%,1%) = "*" then calc% = 8% /*PRICE ONLY*/
            call "APCCST0B" ( calc%,     /* Calculation Method         */~
                              part$,     /* MFG Part Number            */~
/* PAR000 */                  partno1$,  /* MFG Sub Part Number        */~
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
                              #39,       /*   (APCPLNDP)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */

            if str(sale$(kk%),2%,1%) <> "*" then L62050
                x_err% = 3%
                p_err% = 0%
                goto L62130

L62050:     for i% = 1% to 20%
                if apc_err%(i%) = 0%        then L62110
                     p_err%     = i%
                     x_err%     = 1%
                     cst_err%   = cst_err% + 1%

L62110:     next i%

L62130:     gosub store_cost                  /* Gather All MFG Costs  */

        REM IF P_ERR% <> 0%                 THEN GOSUB UPDATE_ERROR_LOG
        return

        store_cost
            gosub calc_transport

            mat cst = zer                    /* Total Vinyl and Misc.  */
                                             /* (Mat'l + Scrap) Cost   */
            tot_mat = round(tc(3%)  + tc(6%) + tc(9%) + tc(12%) + tc(15%)~
                          + tc(18%) + tc(21%), 4) /*Include W/F Amount */
            if x_err% > 1% then gosub compute_cost
                                     /* Use Cost from Costing Errors   */
            tot_cst = round( tot_mat + lab(8%) + lab(9%) + tc(19%) +     ~
                                                           tc(20%), 4)
                                     /* Note - CST() are the Values    */
                                     /*   Assoc. with each Line Item   */
            cst(1%) = round(tot_mat * sls_qty, 2) /*Total Material     */
            cst(2%) = round(lab(8%) * sls_qty, 2) /*Total Dir/Ind Labor*/
            cst(3%) = round(lab(9%) * sls_qty, 2) /*Total Overhead Cost*/
            cst(4%) = round(tc(19%) * sls_qty, 2) /*Total Freight Cost */
            cst(5%) = round(tc(20%) * sls_qty, 2) /*Total Vinyl Disc't */
            cst(6%) = round(tot_cst * sls_qty, 2) /*Total MFG Cost     */
            cst(7%) = round(trn_amt * sls_qty, 2) /*Total Trans Cost   */
            if sls_qty < 0 then cst(7%) = 0.0     /*NO TRANS - CREDIT  */
            cst(8%) = round(sls_price, 2)         /*Total Price W/Disc */
            cst(9%) = round(sls_qty, 2)           /*Total Quantity     */
        return

        calc_transport
        REM IF SAV_CUS$ = CUSCODE$ THEN RETURN
        REM    SAV_CUS$ = CUSCODE$
            gencdkey$ = " " : sls_regn% = 0% : trn_amt = 0.0
            str(gencdkey$,1%,9%)   = "COST TRAN"
            str(gencdkey$,10%,15%) = cuscode$
            read #3,key = gencdkey$, using L62500, descr$, eod goto L62540
L62500:         FMT POS(25), CH(30)

            convert str(descr$,1%,2%) to sls_regn%, data goto L62540

L62540:     convert sls_regn%         to sls_regn$, pic(00)

            gencdkey$ = " "
            str(gencdkey$,1%,9%)   = "COST REGN"
            str(gencdkey$,10%,15%) = sls_regn$
            read #3,key = gencdkey$, using L62500, descr$, eod goto L62630

            convert str(descr$,1%,8%) to trn_amt, data goto L62630

L62630: return

        compute_cost
            tot_mat = 0.0
            mat lab = zer : mat tc = zer
            if str(sale$(kk%),1%,1%) <> "*" then kk% = 1%
            unit_price    = 0.0
            if sls_qty   <> 0  then                                      ~
                               unit_price = round(sls_price/sls_qty, 2)
            if x_err%    <> 2% then goto L62780
            if unit_price = 0  then tot_mat = sale(kk%,3%)               ~
                               else tot_mat = unit_price * sale(kk%,2%)
            if tot_mat    = 0  then goto L62780
        return
                                                   /* Costing Error    */
L62780:     tot_mat = (pc(1%) * .50) * sale(kk%,1%)/* Calc based on the*/
                                                   /* Catalog Price    */
            if tot_mat    = 0  then tot_mat = unit_price * sale(kk%,1%)
            if tot_mat    = 0  then tot_mat = sale(kk%,3%)
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

        end
