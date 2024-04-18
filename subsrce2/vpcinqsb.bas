        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  PPPP    CCC   IIIII  N   N   QQQ    SSS   BBBB    *~
            *  V   V  P   P  C   C    I    NN  N  Q   Q  S      B   B   *~
            *  V   V  PPPP   C        I    N N N  Q   Q   SSS   BBBB    *~
            *   V V   P      C   C    I    N  NN  Q  QQ      S  B   B   *~
            *    V    P       CCC   IIIII  N   N   QQQQ   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPCINQSB - Inquiry routine for Purchasing Contracts.      *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/07/94 ! Original                                 ! LDJ *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "VPCINQSB" (                                                 ~
            contract_id$,                /* Contract to Query          */~
            contract_line$,              /* Contract Line for Query    */~
            #01,   /* VPCMASTR Vendor Purchases Contract Master File   */~
            #03,   /* HNYMASTR Inventory Master File                   */~
            #04,   /* GENCODES System General Codes file.              */~
            #05,   /* VENDOR   VENDOR MASTER RECORD                    */~
            #06,   /* TXTFILE  System Text File                        */~
            #07,   /* VBKMASTR Purchase Order Master file              */~
            #08,   /* VBKLINES Purchase Order Lines file               */~
            #09,   /* PAYMASTR A/P Invoice Master file                 */~
            #10,   /* PAYLINES A/P Invoice Lines file                  */~
            #11,   /* RCVLINES Receiver Line Items File                */~
            f1%)                         /* Contract Found Status      */
                                         /*  1 = Found, 2 = Not        */

        dim                                                              ~
            audit$79,                    /* Audit fields display       */~
            comments$(2)50,              /* Comments                   */~
            contract_descr$30,           /* Contract Description       */~
            contract_id$16,              /* Contract ID                */~
            contract_line$4,             /* Contract Line ID           */~
            changed_by$3,                /* Last Changed By            */~
            changed_on$10,               /* Last Changed On            */~
            created_by$3,                /* Created By                 */~
            created_on$10,               /* Created On                 */~
            date$8,                      /* Date for screen display    */~
            descr_map(22),               /* PLOWCODE Arg               */~
            end_date$10,                 /*          End Date          */~
            errormsg$79,                 /* Error message              */~
            hdr$(3)80,                   /* PLOWCODE Arg               */~
            hdr_descr$30, hdr_vendor$9,  /* 'Header' Variables         */~
            hdr_end$8, hdr_start$8,      /*                            */~
            hdr_line$79,                 /* Screen text                */~
            incl_excl(1), incl_excl$(1)1,/* PLOWCODE Args              */~
            inpmessage$79,               /* Informational Message      */~
            item_code$25,                /* Item Code                  */~
            item_code_descr$32,          /* Item Code Description      */~
            item_type$1,                 /* Item Type (P/A/M)          */~
            item_type_descr$32,          /* Item Type (P/A/M)          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lin_line$79,                 /* Screen text                */~
            lin_start$6, lin_end$6,      /* Line Start-End YYMMDDs     */~
            line2$79,                    /* Screen Line #2             */~
            max_dollar$10,               /* Maximum Dollars Allowed    */~
            max_qty$10,                  /* Maximum Quantity           */~
            min_dollar$10,               /* Minimum Dollars Allowed    */~
            min_qty$10,                  /* Minimum Quantity           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            record1$(3)250,              /* Before Pic                 */~
            record2$(3)250,              /* After  Pic                 */~
            start_date$10,               /* Contract Start Date        */~
            text$(392,1)70,              /* Text Array                 */~
            text_id$4,                   /* Text ID                    */~
            uom$4,                       /* Unit of Measure            */~
            uom_descr$32,                /* Unit of Measure Descr      */~
            unit_price$10,               /* Unit Price                 */~
            userid$3,                    /* Current User Id            */~
            vpc_start$6, vpc_end$6,      /* Hdr  Start-End YYMMDDs     */~
            vendor$9,                    /* Vendor                     */~
            vendor_descr$32              /* Vendor                     */~

        dim f2%(14),                     /* = 0 if the file is open    */~
            f1%(14),                     /* = 1 if READ was successful */~
            fs%(14),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(14)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
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
            * #01 ! VPCMASTR ! Vendor Purchases Contract Master File    *~
            * #02 ! VPCXREF  ! Purchasing Contract, VBK/PAY Lines X-Ref *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! GENCODES ! System General Codes file.               *~
            * #05 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #06 ! TXTFILE  ! System Text File                         *~
            * #07 ! VBKMASTR ! Purchase Order Master file               *~
            * #08 ! VBKLINES ! Purchase Order Lines file                *~
            * #09 ! PAYMASTR ! A/P Invoice Master file                  *~
            * #10 ! PAYLINES ! A/P Invoice Lines file                   *~
            * #11 ! RCVLINES ! Receiver Line Items File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "VPCXREF",                                       ~
                        varc,     indexed,  recsize =  133,              ~
                        keypos = 1,    keylen = 49,                      ~
                        alt key  1, keypos =   21, keylen =  49

            call "OPENCHCK" (#02, fs%( 2%), f2%( 2%), 0%, rslt$( 2%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            f1%(1%) = 0%
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62%) = "VPCINQSB: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Select Contract to View.                                  *~
            *************************************************************

        inputmode
            gosub initialize_variables
            call "VPCPIKME" (vendor$, contract_id$, contract_line$,      ~
                     item_type$, item_code$, " ", #01, #05, f1%(1%))
            if f1%(1%) = 0% then exit_program
            gosub dataload

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            inpmessage$ = "Select one of the PF Keys below to perform "  ~
                        & "the indicated Action"
            str(line2$,,60%)= "Contract ID: " & contract_id$   & "-"  &  ~
                                                contract_line$        &  ~
                                        " ("  & contract_descr$ & ")"
            gosub display_contract      /* Display Screen - No Entry   */
                  if keyhit%  = 16% then       exit_program
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub view_purchases_summary
                  if keyhit%  =  3% then gosub view_purchases_detail
                  if keyhit%  =  4% then gosub view_payables_detail
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  = 25% then gosub view_text
                  goto  editpg1

        editpg2
            call "VFINPSUB" ("VPCMASTR", "D", "Purchasing Contracts  ",  ~
                             str(line2$,,60%), "YN", vf$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then editpg1
            if keyhit% = 16% then editpg1
                             goto editpg1


        view_text
            call "TXTDSPLY" (#06, f2%(6%), "038", str(line2$,,60%),      ~
                                                      text_id$, text$())
            return

        view_purchases_summary
            call "VPCSUMSB" (                                            ~
                     #01,                /* VPCMASTR channel           */~
                     #08,                /* VBKLINES channel           */~
                     #10,                /* PAYLINES channel           */~
                     contract_id$,       /* Contract to summarized     */~
                     contract_line$,     /* Line to be  summarized     */~
                                         /* *** HEADER LEVEL INFO ***  */~
                     hdr%,               /*  HDR Exists? 0=No, 1=Yes   */~
                     vpc_min_dlr,        /*  Minimum Dollars           */~
                     vpc_max_dlr,        /*  Maximum Dollars           */~
                     vpc_start$,         /*  Start Date                */~
                     vpc_end$,           /*  End   Date                */~
                     vpc_vbk_dlr,        /*  PO  Dollars               */~
                     vpc_pay_dlr,        /*  A/P Dollars               */~
                                         /* **** LINE LEVEL INFO ****  */~
                     lin%,               /*  Line Exists? 0=No, 1=Yes  */~
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

            lin% = lin%   /* get rid of compiler warning     */

            convert vpc_min_dlr to vpc_min_dlr$, pic($##,###,###.##-)
            convert vpc_max_dlr to vpc_max_dlr$, pic($##,###,###.##-)
            if hdr% = 0% then vpc_min_dlr$, vpc_max_dlr$ = " "
            hdr_tot_dlr = vpc_pay_dlr + vpc_vbk_dlr
            lin_tot_dlr = lin_vbk_dlr + lin_pay_dlr
            lin_tot_qty = lin_vbk_qty + lin_pay_qty
            gosub display_summary
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  3% then gosub view_purchases_detail
                  if keyhit%  =  4% then gosub view_payables_detail
                  if keyhit%  = 16% then return
                  if keyhit%  = 25% then gosub view_text
            goto view_purchases_summary

        view_purchases_detail
            plowkey$ = str(contract_id$) & str(contract_line$) & "P" &   ~
                       vendor$
            readkey$ = hex(06) & "Select a Line and press (ENTER/RETURN) ~
        ~to view the details for that Line"
            hdr$(3%) = hex(ac) & "View P. O. Details for " &             ~
                       "Contract: " & contract_id$ & " " & contract_line$
            str(hdr$(3%),61%) = str(line2$,61%)
            mat incl_excl = zer
            mat descr_map = zer
            hdr$(1%) = "Purchase Order #  Line Qty-On-Order Qty-Received ~
        ~      Value For Job  Due/Rec'd"
            descr_map(01%) = 031.16  : descr_map(02%) = 0001   /* P.O. #*/
            descr_map(03%) = 047.03  : descr_map(04%) = 0018   /* Line #*/
            descr_map(05%) =-032.25  : descr_map(06%) = 1010   /* Part #*/
            descr_map(07%) =-057.32  : descr_map(08%) = 1036   /* Descr */
            descr_map(09%) =-109.08  : descr_map(10%) = 022.124/*OnOrder*/
            descr_map(11%) =-101.08  : descr_map(12%) = 035.124/*Rec'ved*/
            descr_map(13%) =-125.08  : descr_map(14%) = 047.124/*$ Value*/
            descr_map(15%) =-166.08  : descr_map(16%) = 060    /*Job    */
            descr_map(17%) =-142.061 : descr_map(18%) = 069    /*DateDue*/
            descr_map(19%) =-148.061 : descr_map(20%) =1069    /*DateRcv*/

            call "PLOWCODE" (#02, plowkey$, readkey$, 9030%, .8, f1%(2%),~
                     hdr$(), 0, -22, incl_excl(), incl_excl$(), "D", "Y",~
                     #08, descr_map())
            if f1%(2%) = 0% then return
            call "POSTATUS" (vendor$, str(plowkey$,31%,16%),             ~
                 str(plowkey$,47%,03%), 0%, #08, #11,#07,#10,#09,#06)
            goto view_purchases_detail

        view_payables_detail
            plowkey$ = str(contract_id$) & str(contract_line$) & "I" &   ~
                       vendor$
            readkey$ = hex(06) & "Below are the Invoice Lines referencing~
        ~ this Contract."
            hdr$(3%) = hex(ac) & "View A/P Invoice Details for " &       ~
                       "Contract: " & contract_id$ & " " & contract_line$
            str(hdr$(3%),61%) = str(line2$,61%)
            mat incl_excl = zer
            mat descr_map = zer
            hdr$(1%) = "  Invoice Number  Line     Quantity   Unit Price ~
        ~  Extension   For Job"
            descr_map(01%) = 031.16  : descr_map(02%) = 0001   /* P.O. #*/
            descr_map(03%) = 047.03  : descr_map(04%) = 0018   /* Line #*/
            descr_map(05%) =-073.25  : descr_map(06%) = 1010   /* Part #*/
            descr_map(07%) =-154.25  : descr_map(08%) = 1036   /*VenPart*/
            descr_map(09%) =-098.08  : descr_map(10%) = 022.124/* Qty   */
            descr_map(11%) =-131.08  : descr_map(12%) =035.1272/* Price */
            descr_map(13%) =-106.08  : descr_map(14%) =047.1242/*Extensn*/
            descr_map(15%) =-114.08  : descr_map(16%) = 061    /*Job    */

            call "PLOWCODE" (#02, plowkey$, readkey$, 9030%, .8, f1%(2%),~
                     hdr$(), 0, -22, incl_excl(), incl_excl$(), "D", "Y",~
                     #10, descr_map())
            if f1%(2%) = 0% then return
            goto view_payables_detail

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      comments$(1), comments$(2), contract_descr$,       ~
                                    end_date$, item_code$, item_type$,   ~
                      item_type_descr$, max_qty$, min_qty$, start_date$, ~
                      unit_price$, vendor$, vendor_descr$, uom$,         ~
                      item_code_descr$, uom_descr$,                      ~
                      min_dollar$, max_dollar$, hdr_vendor$, hdr_start$, ~
                      hdr_end$, hdr_descr$

            init (" ") vf$, created_on$, changed_by$, changed_on$,       ~
                       record1$(), record2$(), str(line2$,,60), audit$

            text_id$ = all(hex(ff))

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
                return clear all
                contract_id$, contract_line$ = " "
                goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #1 using L35060, vendor$, contract_id$, contract_line$,   ~
                                contract_descr$,                         ~
                                item_type$, item_code$, start_date$,     ~
                                end_date$, uom$, min_qty, max_qty,       ~
                                unit_price, min_dollar, max_dollar,      ~
                                comments$(), text_id$, vf$,              ~
                                created_by$, created_on$,                ~
                                changed_by$, changed_on$
            call "DATEFMT" (created_on$)
            call "DATEFMT" (changed_on$)
            if changed_by$ = " " then                                    ~
                audit$ = "Record created by " & created_by$ & " on "  &  ~
                         created_on$ & "."                               ~
            else                                                         ~
                audit$ = "Record created by " & created_by$ & " on "  &  ~
                         created_on$ & "; " & "last changed by "      &  ~
                         changed_by$ & " on " & changed_on$ & "."
            call "DESCRIBE" (#05, vendor$, vendor_descr$, 1%, f1%(5%))
            item_type_descr$ = "(Miscellaneous)"
            if item_type$ =  "H" then item_type_descr$ = "(Header)"
            if item_type$ <> "P" then L30550
              item_type_descr$ = "(Part Code)"
              call "DESCRIBE" (#03,item_code$,item_code_descr$,1%,f1%(3%))
L30550:     if item_type$ <> "A" then L30590
              item_type_descr$ = "(WC Activity)"
              readkey$ = "WC ACTVTY" & item_code$
              call "DESCRIBE" (#04,readkey$,item_code_descr$,1%,f1%(4%))
L30590:     call "DATEFMT" (start_date$)
            call "DATEFMT" (end_date$  )
            readkey$ = "UOM      " & uom$
            call "DESCRIBE" (#04, readkey$, uom_descr$, 1%, f1%(4%))
            if min_qty <> 0 then                                         ~
                call "CONVERT" (min_qty   , -0.4, min_qty$   )
            if max_qty <> 0 then                                         ~
                call "CONVERT" (max_qty   , -0.4, max_qty$   )
            if unit_price <> 0 then                                      ~
                call "CONVERT" (unit_price, -2.7, unit_price$)
            if min_dollar <> 0 then                                      ~
                call "CONVERT" (min_dollar, -2.2, min_dollar$)
            if max_dollar <> 0 then                                      ~
                call "CONVERT" (max_dollar, -2.2, max_dollar$)

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060:     FMT                          /* FILE: VPCMASTR             */~
                CH(09),                  /* Vendor Code                */~
                CH(16),                  /* Contract ID                */~
                CH(04),                  /* Contract Line              */~
                CH(30),                  /* Contract Description       */~
                CH(01),                  /* Item Type Code             */~
                CH(25),                  /* Item Code                  */~
                CH(06),                  /* Start Date                 */~
                CH(06),                  /* End   Date                 */~
                CH(04),                  /* Stocking UOM               */~
                PD(14,4),                /* Minimum Quantity           */~
                PD(14,4),                /* Maximum Quantity           */~
                PD(14,7),                /* Unit Price                 */~
                2*PD(14,4),              /* Min-Max Dollars            */~
                2*CH(50),                /* Comments                   */~
                CH(04),                  /* Text ID                    */~
                CH(200),                 /* Variable Fields            */~
                CH(03),                  /* Created By                 */~
                CH(06),                  /* Created On                 */~
                CH(03),                  /* Changed By                 */~
                CH(06),                  /* Changed On                 */~
                CH(137)                  /* Filler                     */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        display_contract
              gosub set_pf1
              init(hex(86)) lfac$()
              if item_type$ = "H" then item_type_descr$ = "(Header)"
              if item_type$ = "P" then item_type_descr$ = "(Part Code)"
              if item_type$ = "A" then item_type_descr$ = "(WC Activity)"
              if item_type$ = "M" then item_type_descr$ ="(Miscellaneous)"

L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "View Purchasing Contracts Information",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Contract ID / Line",                         ~
               at (06,30), fac(lfac$( 1)), contract_id$         , ch(16),~
               at (06,48), fac(lfac$( 1)), contract_line$       , ch(04),~
                                                                         ~
               at (07,02), "         Description",                       ~
               at (07,30), fac(lfac$( 2)), contract_descr$      , ch(30),~
                                                                         ~
               at (08,02), "Vendor",                                     ~
               at (08,30), fac(lfac$( 3)), vendor$              , ch(09),~
               at (08,49), fac(hex(8c)),   vendor_descr$        , ch(32),~
                                                                         ~
               at (09,02), "Item Type (P/A/M)",                          ~
               at (09,30), fac(lfac$( 4)), item_type$           , ch(01),~
               at (09,49), fac(hex(8c)),   item_type_descr$     , ch(32),~
                                                                         ~
               at (10,02), "Item Code",                                  ~
               at (10,30), fac(lfac$( 5)), item_code$           , ch(25),~
               at (10,49), fac(hex(8c))  , item_code_descr$     , ch(32),~
                                                                         ~
               at (11,02), "Start/End Dates",                            ~
               at (11,30), fac(lfac$( 6)), start_date$          , ch(08),~
               at (11,42), fac(lfac$( 6)), end_date$            , ch(08),~
                                                                         ~
               at (12,02), "Min/Max Quantities",                         ~
               at (12,30), fac(lfac$( 7)), min_qty$             , ch(10),~
               at (12,42), fac(lfac$( 7)), max_qty$             , ch(10),~
                                                                         ~
               at (13,02), "Unit of Measure ",                           ~
               at (13,30), fac(lfac$( 8)), uom$                 , ch(04),~
               at (13,49), fac(hex(8c)),   uom_descr$           , ch(30),~
                                                                         ~
               at (14,02), "Unit Price",                                 ~
               at (14,30), fac(lfac$( 9)), unit_price$          , ch(10),~
                                                                         ~
               at (15,02), "Min/Max Dollars",                            ~
               at (15,30), fac(lfac$(10)), min_dollar$          , ch(10),~
               at (15,42), fac(lfac$(10)), max_dollar$          , ch(10),~
                                                                         ~
               at (16,02), "Comments",                                   ~
               at (16,30), fac(lfac$(11)), comments$(1)         , ch(50),~
               at (17,30), fac(lfac$(11)), comments$(2)         , ch(50),~
                                                                         ~
               at (19,02), fac(hex(8c)),   audit$               , ch(70),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40740
                  call "MANUAL" ("VPCINQSB") : goto L40100

L40740:        if keyhit% <> 15% then return
                  call "PRNTSCRN" : goto L40100


        set_pf1
                                     /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Reselect Contract / Start Over       " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Purchases Summary   (4)See Invoice Li" &        ~
                     "nes                    (15)Print Screen"
            pf$(3) = "(3)See P.O. Lines      (5)Next Page     " &        ~
                     "(25)View Text          (16)Exit Program"
            pfkeys$ = hex(0102030405ffffffffff19ff0dff0f10ff)
            if text_id$ < hex(ffffffff) then return
            str(pf$(3%),41%,13%) = " "
            str(pfkeys$,11%,1%) = hex(ff)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Purchase Contract Summary Information Screen              *~
            *************************************************************

        display_summary
              if item_type$ = "H" then item_type_descr$ = "Header"
              if item_type$ = "P" then item_type_descr$ = "Part"
              if item_type$ = "A" then item_type_descr$ = "Activity"
              if item_type$ = "M" then item_type_descr$ = "Miscellaneous"
              str(line2$,,60%) = "Summary Purchase Information for " &   ~
                     "Contract"
              gosub set_pf2
              hdr_line$ = "Total Purchases against this Contract"
              if contract_line$ = " " then                               ~
                lin_line$="Purchases posted directly to the Contract " & ~
                          "Header"                                       ~
              else                                                       ~
                lin_line$="Purchases posted directly to this Line of " & ~
                          "the Contract"

L41180:     accept                                                       ~
               at (01,02),                                               ~
                  "View Purchasing Contracts Information",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Contract ID / Line:",                        ~
               at (04,22), fac(lfac$(1%)), contract_id$         , ch(16),~
               at (04,39), fac(lfac$(1%)), contract_line$       , ch(04),~
                                                                         ~
               at (04,46), fac(lfac$(2%)), contract_descr$      , ch(30),~
                                                                         ~
               at (05,02), "Vendor:",                                    ~
               at (05,22), fac(lfac$(3%)), vendor$              , ch(09),~
               at (05,46), fac(hex(8c)),   vendor_descr$        , ch(32),~
                                                                         ~
               at (06,02), "Contract Type:",                             ~
               at (06,22), fac(lfac$(3%)), item_type_descr$     , ch(14),~
               at (06,46), fac(hex(8c)),   item_code$           , ch(25),~
                                                                         ~
               at (08,02), fac(hex(ac)), hdr_line$,                      ~
               at (09,12), "Contract Limits",                            ~
               at (10,02), "Maximum Dollars:",                           ~
               at (10,20),fac(lfac$(4%)),vpc_max_dlr$,                   ~
               at (11,02), "Minimum Dollars:",                           ~
               at (11,20),fac(lfac$(5%)),vpc_min_dlr$,                   ~
                                                                         ~
               at (09,51), "Contract Expenditures",                      ~
                                                                         ~
               at (10,40), "Uninvoiced Purchases   :",                   ~
              at (10,65),fac(lfac$(6%)),vpc_vbk_dlr,pic($##,###,###.##-),~
               at (11,40), "Invoiced Purchases     :",                   ~
               at (11,65),fac(hex(a6)), vpc_pay_dlr,pic($##,###,###.##-),~
                                                                         ~
               at (12,40), "        Total Purchases:",                   ~
              at (12,65),fac(lfac$(8%)),hdr_tot_dlr,pic($##,###,###.##-),~
                                                                         ~
               at (14,02), fac(hex(ac)), lin_line$,                      ~
               at (15,12), "  Line Limits",                              ~
               at (16,02), "Maximum Dollars:",                           ~
               at (16,20),fac(lfac$(9%)),lin_max_dlr,pic($##,###,###.##),~
               at (17,02), "Minimum Dollars:",                           ~
               at (17,20),fac(lfac$(9%)),lin_min_dlr,pic($##,###,###.##),~
               at (18,02), "Maximum Qty    :",                           ~
               at (18,20),fac(lfac$(9%)),lin_max_qty,pic(###,###,###.##),~
               at (19,02), "Minimum Qty    :",                           ~
               at (19,20),fac(lfac$(9%)),lin_min_qty,pic(###,###,###.##),~
                                                                         ~
               at (15,51), "Contract Line Expenditures",                 ~
               at (16,40), "Uninvoiced Purchases   :",                   ~
              at (16,65),fac(lfac$(9%)),lin_vbk_dlr,pic($##,###,###.##-),~
               at (17,40), "Invoiced Purchases     :",                   ~
               at (17,65),fac(hex(a6)),lin_pay_dlr,pic($##,###,###.##-), ~
                                                                         ~
               at (18,40), "        Total Purchases:",                   ~
              at (18,65),fac(lfac$(9%)),lin_tot_dlr,pic($##,###,###.##-),~
                                                                         ~
               at (19,40), "Total Qty against Line :",                   ~
             at (19,65),fac(lfac$(10%)),lin_tot_qty,pic(###,###,###.##-),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41940
                  call "MANUAL" ("VPCINQSB") : goto L41180

L41940:        if keyhit% <> 15% then return
                  call "PRNTSCRN" : goto L41180

        set_pf2
                                     /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Reselect Contract / Start Over       " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(3)See P.O. Lines                       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)See Invoice Lines                    " &        ~
                     "(25)View Text          (16)Return      "
            pfkeys$ = hex(01ff0304ffffffffffff19ff0dff0f10ff)
            if text_id$ < hex(ffffffff) then return
            str(pf$(3%),41%,13%) = " "
            str(pfkeys$,11%,1%) = hex(ff)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution - returns to caller.                 *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            f1% = f1%(1%)
            end
