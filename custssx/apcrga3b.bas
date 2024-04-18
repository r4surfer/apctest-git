        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA3B                             *~
            *  Creation Date     - 11/18/95                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - This Program provides RGA Line Item  *~
            *                      Distribution for APC RGAII.          *~
            *                                                           *~
            *  Special Comments  - Subroutine of APCRGA03               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/18/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
	    * 02/26/98 ! MODIFIED FOR Y2K COMPLIANCE              ! DJD *~
            * 09/28/00 ! Mod to support new alph rga numbers      ! CMG *~
            *          !      (EWD001)                            !     *~	    
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *************************************************************

        sub "APCRGA3B"   ( #01,          /*   (APCRGAHD)               */~
                           #02,          /*   (APCRGADT)               */~
                           #03 )         /*   (GENCDSIN)               */

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
/*PAR000*/  rga_dt_filler$256,           /* APCRGADT Filler Area       */~
/*PAR000*/  rga_dt_subp$20,              /* APCRGADT Subpart           */~
            rga_dt1$4,                   /* APCRGADT File Read         */~
            dtlkey$6,                    /* APCRGADT File Read Key     */~
            dtlkey1$8,                   /* APCRGADT File Read Alt Key1*/~
            hdrkey$4,                    /* APCRGAHD File Read Key     */~
            readkey$25,                  /* GENCODES File Read Key     */~
            rga_hd_rec$80,               /* APCRGAHD Read Record       */~
            coderec$128,                 /* GENCODES Read Record       */~
            xx$(7%)50,                   /* Scan Status Display Text   */~
            gg$(7%)50,                   /* Scan Status Display Text   */~
            rga$(7%)50,                  /* Scan Barcode Text          */~
            status$2,                    /* Distribution Status Code   */~
            date$8,                      /* Date Scanned               */~
            dateout$8,                   /* Date for Screen Display    */~
            tab_hdr$30,                  /* Display Screen Header      */~
            errormsg$60,                 /* Error message              */~
            inp_text$(2%)79,             /* Input Prompt Text          */~
            i$(24%)80,                   /* Detail Line(10) Array Area */~
            inpmessage$79,               /* Informational Message      */~
            cursor%(2%),                 /*                            */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pfkeys$32,                   /* PF Keys Variable           */~
            progid$,                     /* Screen Line #2 Program ID  */~
            line2$79,                    /* Screen Line #2 Time Field  */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            userid$3                     /* Current User Id            */

        dim f1%(3%)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 11/18/95 RGA Line Item Distribution Prg."

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! APCRGAHD ! APC RGA Header Master File               *~
            * #02 ! APCRGADT ! APC RGA Detail Master File               *~
            * #03 ! GENCODES ! System Code Table File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
        REM SELECT #1,  "APCRGAHD",                                      ~
                        VARC,     INDEXED,  RECSIZE =   80,              ~
                        KEYPOS =   12, KEYLEN =   4,                     ~
                        ALT KEY  1, KEYPOS =  10, KEYLEN =   6,          ~
                            KEY  2, KEYPOS =   1, KEYLEN =  15

        REM SELECT #2,  "APCRGADT",                                      ~
                        VARC,     INDEXED,  RECSIZE =  256,              ~
                        KEYPOS =   12, KEYLEN =   6,                     ~
                        ALT KEY  1, KEYPOS =  10, KEYLEN =   8,          ~
                            KEY  2, KEYPOS =   1, KEYLEN =  17

        REM SELECT #3,  "GENCODES",                                      ~
                        VARC,     INDEXED,  RECSIZE =  128,              ~
                        KEYPOS =    1, KEYLEN =   24

        REM CALL "SHOSTAT" ("Opening Files, One Moment Please")
        REM CALL "OPENCHCK" (#1,  FS%(1%),  F2%(1%),   0%, RSLT$(1%))
        REM CALL "OPENCHCK" (#2,  FS%(2%),  F2%(2%),   0%, RSLT$(2%))
        REM CALL "OPENCHCK" (#3,  FS%(3%),  F2%(3%),   0%, RSLT$(3%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

            b_max%        = 10%
            progid$       = "APCRGA3B: " & str(cms2v$,,8)
            inp_text$(1%) = "Scan or Enter RGA Barcode Number"
            inp_text$(2%) = "Scan or Enter RGA Status Code   "

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
            init(" ") errormsg$, dateout$, dtlkey$, status$, wand1$,     ~
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
            if keyhit% = 16%        then exit_sub
            if keyhit% <> 0%        then scan_1
            gosub check_rga
                if errormsg$ <> " " then scan_1

        return

        scan_2
            gosub'100(fieldnr%,option%)

            if keyhit% =  1%        then startover
            if keyhit% = 16%        then exit_sub
            if keyhit% <> 0%        then scan_2
            gosub check_status
                if errormsg$ <> " " then scan_2

            gosub dataput
                if errormsg$ <> " " then scan_2

            gosub ok_scan

        return

        REM *************************************************************~
            * Display this Screen if Barcode is scanned and No Errors.  *~
            *************************************************************
        ok_scan
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
/*PAR000*/           rga_dt_subp$,       /* RGA Item Subpart           */~
                     rga_dt_filler$      /* APCRGADT Filler Area       */

            rec% = 1%
L30470:     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            read #2,hold,key = dtlkey$, eod goto L31560

            delete #2

            rga_dt_status$  = status$
            rga_dt_userid$  = userid$
            rga_dt_mod_dte$ = date

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

            write #2, eod goto L31560

            gosub check_dt_status

        return
L31560:     errormsg$ = "(In Use) RGA Record In Use.  Try again later?"
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
                CH(249)                  /* APCRGADT Filler Area       */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'100(fieldnr%,option%)
L40060:     dateout$ = " "
            call "TIME" (dateout$)

            str(line2$,72%) = dateout$
            inpmessage$     = inp_text$(fieldnr%)
            gosub set_pfkeys

            accept                                                       ~
                at (01,02)                                      ,        ~
                   "RGAII Destination Status"                   ,        ~
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
                at (06,02), "Status Code: "                     ,        ~
                at (06,16), fac(lfac$(2%)), status$             , ch(02),~
                at (06,19), fac(lfac$(4%)), wand2$              , ch(01),~
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

                if keyhit% <> 5%  then L40510
                     gosub lookup_status

                     goto L40060
L40510:         if keyhit% <> 15% then L40540
                     call "PRNTSCRN"

L40540:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            return

        set_pfkeys
        REM                          /*  Input Mode             */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "(5)Display Status Codes                  " &       ~
                     "                      (16)Exit Program"
            pfkeys$ = hex(01ffffff05ffffffffffffffffff0f1000)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        check_rga
            errormsg$ = " "      /* EWD001) */
            convert str(dtlkey$,3%,4%) to dtlkey%, data goto L50140

            convert dtlkey% to str(dtlkey$,3%,4%), pic(0000)

            gosub dataload

            if rec% = 1% then L50160
L50140:         errormsg$ = "Invalid RGA Number! Re-enter or Scan Again."
                goto L50250
L50160:     if rga_dt_status$ < "12" then L50190
                errormsg$ = " "
                goto L50210
L50190:         errormsg$ = "RGA ("& dtlkey$ &") Not Scanned In Yet !!!"
                goto L50250
L50210:     if rga_dt_status$ > "12" then L50240
                errormsg$ = " "
                goto L50250
L50240:         errormsg$ = "RGA ("& dtlkey$ &") Already Dispersed !!!"
L50250: return

        check_status
            errormsg$ = " "
            if status$ < "13" or status$ > "22" then L50370
            readkey$              =  all(hex(20))
            str(readkey$,1%,9%)   = "APC  RGA1"
            str(readkey$,10%,15%) =  status$
            read #3,key = readkey$, using L50340, coderec$, eod goto L50370
L50340:         FMT CH(128)

        return
L50370:     errormsg$ = "(Error) - Invalid STATUS Lookup?"
            status$   = "  "
        return

        check_dt_status
            dtlkey1$            =  all(hex(20))
            str(dtlkey1$,1%,2%) = "11"
            str(dtlkey1$,3%,4%) =  str(dtlkey$,1%,4%)
            read #2,key > dtlkey1$, using L50460, rga_dt1$, eod goto L50490
L50460:         FMT POS(12), CH(4)

            if rga_dt1$ = str(dtlkey$,1%,4%) then L50570
L50490:     dtlkey1$            =  all(hex(20))
            str(dtlkey1$,1%,2%) = "12"
            str(dtlkey1$,3%,4%) =  str(dtlkey$,1%,4%)
            read #2,key > dtlkey1$, using L50460, rga_dt1$, eod goto L50550

            if rga_dt1$ = str(dtlkey$,1%,4%) then L50570
L50550:     gosub update_header

L50570: return

        update_header
            hdrkey$     = str(dtlkey$,1%,4%)
            read #1,key = hdrkey$, using L50630, rga_hd_rec$,             ~
                eod goto L50770
L50630:         FMT CH(80)

            read #1,hold,key = hdrkey$, using L50630, rga_hd_rec$,        ~
                eod goto L50770

            delete #1

            str(rga_hd_rec$,10%,2%) = "05"
            str(rga_hd_rec$,40%,3%) = userid$
            str(rga_hd_rec$,43%,6%) = date
            put   #1, using L50630, rga_hd_rec$

            write #1, eod goto L50770

L50770: return

        lookup_status                  /* Load Data for Display Screen */
            tab_hdr$ = "RGA Status Codes    "
            init(" ") readkey$
            str(readkey$,1%,9%) = "APC  RGA1"
            descr$ = hex(06) & tab_hdr$
            call "PLOWCODE" (#3, readkey$, descr$, 9%, .30, f1%(3))

        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_sub
            call "SHOSTAT" ("One Moment Please")

        end
