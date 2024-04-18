        REM THISPROGRAMWASGENERATEDUSINGTHEGENFSPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   AAA   RRRR   IIIII   SSS   H   H  PPPP   IIIII  N   N   *~
            *  A   A  R   R    I    S      H   H  P   P    I    NN  N   *~
            *  AAAAA  RRRR     I     SSS   HHHHH  PPPP     I    N N N   *~
            *  A   A  R   R    I        S  H   H  P        I    N  NN   *~
            *  A   A  R   R  IIIII   SSS   H   H  P      IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARISHPIN - This program allows for the modification of    *~
            *            shipping information on A/R invoices.          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/28/92 ! Original                                 ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            bill$20,                     /* Air/Frt Bill Number        */~
            carrier$6,                   /* Carrier                    */~
            carrierdescr$32,             /* Carrier                    */~
            cartons$10,                  /* Shipment- # of Cartons     */~
            cuscode$9,                   /* Ship-to Customer Code      */~
            cuscodedescr$32,             /* Ship-to Customer Code      */~
            date$8,                      /* Date for screen display    */~
            enblinp$(20)1,               /* Input Enable Y/N           */~
            enblmod$(20)1,               /* Modify Enable Y/N          */~
            errormsg$79,                 /* Error message              */~
            fob$20,                      /* FOB                        */~
            header$(2)79,                /* Header for PLOWCODE        */~
            howship$20,                  /* How Ship                   */~
            info$79,                     /* Info message               */~
            inpfac$(5)30,                /* Input FAC Strings          */~
            inpmessage$79,               /* Informational Message      */~
            invoice$8,                   /* Invoice Number             */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            modfac$(5)30,                /* Modify FAC Strings         */~
            orgfac$(5)30,                /* Original FAC Strings       */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            testerror$79,                /* Error Return from Testing  */~
            userid$3,                    /* Current User Id            */~
            weight$10                    /*           Weight           */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
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
            * #01 ! ARIMASTR ! Invoice Master File                      *~
            * #02 ! GENCODES ! System General Codes file.               *~
            * #03 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alternate key 1, keypos = 10, keylen =  8, dup,  ~
                                  key 2, keypos = 18, keylen = 16, dup,  ~
                                  key 3, keypos = 34, keylen = 16, dup

            select #02, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #03, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =    1, keylen =  09,                     ~
                        alternate key 1, keypos =  10, keylen = 30, dup, ~
                                  key 2, keypos = 424, keylen =  9, dup, ~
                                  key 3, keypos = 771, keylen =  9, dup, ~
                                  key 4, keypos = 780, keylen =  9, dup, ~
                                  key 5, keypos =1049, keylen =  9, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62) = "ARISHPIN: " & str(cms2v$,,8)

*       ** See if User is a Module and/or DB Administrator
            call "CMSMACHK" ("ARM", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%

            init(hex(bc)) inpfac$()
            str(inpfac$(1),, 8) = hex(8181808081808282)
            mat modfac$ = inpfac$  :  mat orgfac$ = inpfac$
            str(modfac$(1%),,1) = hex(8c)

            call "ENABLFSB" ("INIT", "ARISHPIN", inpfac$(), modfac$())

        REM *************************************************************~
            *       I N P U T   M O D E   K E Y   F I E L D             *~
            *-----------------------------------------------------------*~
            * Input & Validate the Key Field Only                       *~
            *************************************************************

        inputmode
            gosub initialize_variables
            mode% = 1%
        input_key
            gosub screen1                  /* Key Field Input */
                if keyhit% = 16% then exit_program
                if keyhit% <> 0% then input_key
            gosub test_key            /* Key Field Validation */
                if errormsg$ <> " " then input_key
                goto dataload

        REM *************************************************************~
            *       I N P U T   M O D E   F U L L   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Full Screen Input                                         *~
            *************************************************************

*       INPUT_SCREEN1
            mode% = 2%
        input_screen1a
            gosub screen1                /* Full Screen Input */
                if keyhit%  = 10% then mode% = 2%
                if keyhit% <>  0% then input_screen1a
            gosub test_screen1            /* Validate Fields */
                if errormsg$ = " " then edit_display1
            mode% = 4%
            goto input_screen1a


        REM *************************************************************~
            *        E D I T   M O D E   F U L L   S C R E E N          *~
            *-----------------------------------------------------------*~
            * Handles Full Screen Edit Mode                             *~
            *************************************************************

        edit_display1
            mode% = 3%
            gosub screen1        /* Display Screen - No Entry */
                if keyhit%  = 16% then process_data
                if keyhit% <> 10% then edit_display1
            mode% = 5%
        edit_screen1
            gosub screen1            /* Full Screen Edit Mode */
                if keyhit%  = 10% then mode% = 5%
                if keyhit% <>  0% then edit_screen1
            gosub test_screen1         /* Validate Field Data */
                if errormsg$ = " " then edit_display1
                mode% = 4%
                goto edit_screen1


        REM *************************************************************~
            *          P R O C E S S I N G   S E C T I O N              *~
            *-----------------------------------------------------------*~
            * Process Data Prior to File Output.                        *~
            *************************************************************

        process_data
            gosub dataput
            goto inputmode

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      bill$, carrier$, carrierdescr$, cartons$,          ~
                      cuscode$, cuscodedescr$, fob$, howship$,           ~
                      invoice$, weight$

            call "ALLFREE"
            return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            get #01 using L30120, howship$, fob$, carrier$, cartons,      ~
                                 weight, bill$
L30120:         FMT POS(419), 2*CH(20), CH(6), 2*PD(14,4), CH(20)
            if carrier$ = " " then L30160
            plowkey$ = "CARRIERS " & carrier$
            call "DESCRIBE" (#2, plowkey$, carrierdescr$, 0%, f1%(2))
                if f1%(2) = 0% then carrierdescr$ = "*** Not on File ***"
L30160:     if cartons <> 0 then call "CONVERT" (cartons, 2.2, cartons$)
            if weight  <> 0 then call "CONVERT" (weight , 2.2, weight$ )
            goto edit_display1

        REM *************************************************************~
            *              W R I T E   D A T A   T O   F I L E S        *~
            *-----------------------------------------------------------*~
            * Writes data from Program Variables into Files.            *~
            *************************************************************

        dataput
            plowkey$ = str(cuscode$) & invoice$
            call "READ101" (#01, plowkey$, f1%(1))
            if f1%(1) = 0% then return  /* Hope this never happens! */
                put #01 using L30120, howship$, fob$, carrier$, cartons,  ~
                                     weight, bill$
                rewrite #01
            return

        REM *************************************************************~
            *               S C R E E N   N U M B E R   1               *~
            *-----------------------------------------------------------*~
            * Input and Edit Screen Number 1                            *~
            *************************************************************

        screen1
            gosub setpf1
            on mode% gosub set_key,     /* Input Key Field Only */       ~
                           set_full1,   /* Full Screen Input    */       ~
                           set_edit,    /* Edit Mode - Display  */       ~
                           set_errs,    /* Error Screen         */       ~
                           set_full1    /* Full Screen Edit     */
            goto display_screen1

        set_full1       /* Full Screen Input and Edit */
            inpmessage$ = "Enter or Modify ALL Data as Required and"  &  ~
                            " Press RETURN to VALIDATE"
            init(hex(8c)) lfac$()
            if mode% = 2% then str(lfac$()) = str(inpfac$(1%))           ~
                          else str(lfac$()) = str(modfac$(1%))
            lfac$(1) = hex(8c)
            lfac$(2) = hex(8c)
            return

        display_screen1

            accept                                                       ~
                at (01,02),                                              ~
                    "Invoice Shipping Detail Maintenance",               ~
                at (01,64), "1 of 1 :",                                  ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (04,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (06,02), "Ship-to Customer Code",                     ~
                at (06,30), fac(lfac$( 1)), cuscode$            , ch(09),~
                at (06,49), fac(hex(8c)),   cuscodedescr$       , ch(32),~
                                                                         ~
                at (07,02), "Invoice Number",                            ~
                at (07,30), fac(lfac$( 2)), invoice$            , ch(08),~
                                                                         ~
                at (08,02), "How Ship",                                  ~
                at (08,30), fac(lfac$( 3)), howship$            , ch(20),~
                                                                         ~
                at (09,02), "FOB",                                       ~
                at (09,30), fac(lfac$( 4)), fob$                , ch(20),~
                                                                         ~
                at (10,02), "Carrier",                                   ~
                at (10,30), fac(lfac$( 5)), carrier$            , ch(06),~
                at (10,49), fac(hex(8c)),   carrierdescr$       , ch(32),~
                                                                         ~
                at (11,02), "Air/Frt Bill Number",                       ~
                at (11,30), fac(lfac$( 6)), bill$               , ch(20),~
                                                                         ~
                at (12,02), "Shipment- # of Cartons",                    ~
                at (12,30), fac(lfac$( 7)), cartons$            , ch(10),~
                                                                         ~
                at (13,02), "          Weight",                          ~
                at (13,30), fac(lfac$( 8)), weight$             , ch(10),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% = 29% and admin% = 1%                         ~
                    then gosub enable_screen1
                gosub pfkey_hit
                if keyhit% = 15% then screen1
                return

        setpf1
        if mode% > 1% then setpf_full       /* Key Field Only */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return

        setpf_full      /* Full Screen input, edit or error */
        if mode% = 3% then setpf1_display
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            if mode% <> 4% and mode% <> 5% then return
            str(pf$(3),41,16) = "(10)Full Screen"
            str(pfkeys$,10,1) = hex(0a)
            return

        setpf1_display               /* Edit Mode - Display */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "(10)Full Screen Edit   (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)SAVE DATA   "
            pfkeys$ = hex(01ffffffffffffffff0affff0dff0f101d)

            return

        REM *************************************************************~
            *          S O F T   E N A B L E   S C R E E N   1          *~
            *************************************************************

        enable_screen1
            inpmessage$ = "Enter 'N' to DISABLE the Field"
            init(hex(81)) lfac$()
            init("Y") enblinp$()  :  init("Y") enblmod$()
            lfac$(1%) = hex(8c)   :  enblmod$(1%) = "N"
            lfac$(2%) = hex(8c)   :  enblmod$(2%) = "N"
            for i% = 1% to  8%
                if str(inpfac$( 1%),i%,1%) = hex(8c)                     ~
                    then enblinp$(i%) = "N"
                if str(modfac$( 1%),i%,1%) = hex(8c)                     ~
                    then enblmod$(i%) = "N"
            next i%

            accept                                                       ~
                at (01,02),                                              ~
                    "Invoice Shipping Detail Maintenance",               ~
                at (01,64), "1 of 1 :",                                  ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,24), "Soft Enable Settings for Screen 1",         ~
                at (05,25), "Input Enabled?",                            ~
                at (05,44), "Modify Enabled?",                           ~
                at (06,02), "Ship-to Customer Code",                     ~
                at (06,30), fac(lfac$( 1)), enblinp$( 1),         ch(01),~
                at (06,50), fac(lfac$( 1)), enblmod$( 1),         ch(01),~
                at (07,02), "Invoice Number",                            ~
                at (07,30), fac(lfac$( 2)), enblinp$( 2),         ch(01),~
                at (07,50), fac(lfac$( 2)), enblmod$( 2),         ch(01),~
                at (08,02), "How Ship",                                  ~
                at (08,30), fac(lfac$( 3)), enblinp$( 3),         ch(01),~
                at (08,50), fac(lfac$( 3)), enblmod$( 3),         ch(01),~
                at (09,02), "FOB",                                       ~
                at (09,30), fac(lfac$( 4)), enblinp$( 4),         ch(01),~
                at (09,50), fac(lfac$( 4)), enblmod$( 4),         ch(01),~
                at (10,02), "Carrier",                                   ~
                at (10,30), fac(lfac$( 5)), enblinp$( 5),         ch(01),~
                at (10,50), fac(lfac$( 5)), enblmod$( 5),         ch(01),~
                at (11,02), "Air/Frt Bill Number",                       ~
                at (11,30), fac(lfac$( 6)), enblinp$( 6),         ch(01),~
                at (11,50), fac(lfac$( 6)), enblmod$( 6),         ch(01),~
                at (12,02), "Shipment- # of Cartons",                    ~
                at (12,30), fac(lfac$( 7)), enblinp$( 7),         ch(01),~
                at (12,50), fac(lfac$( 7)), enblmod$( 7),         ch(01),~
                at (13,02), "          Weight",                          ~
                at (13,30), fac(lfac$( 8)), enblinp$( 8),         ch(01),~
                at (13,50), fac(lfac$( 8)), enblmod$( 8),         ch(01),~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (23,02), "(1)Return Without Setting",                 ~
                at (23,38), "(14)Set Enables for Current Session Only",  ~
                at (24,38), "(30)Save Enable Settings Permanently",      ~
                keys(hex(010e1e)), key(sethit%)

            if sethit% = 1% then return
            for i% = 1% to  8%
                if enblinp$(i%) = "N" then                               ~
                    str(inpfac$(1%),i%,1%) = hex(8c) else                ~
                    str(inpfac$(1%),i%,1%) = str(orgfac$(1%),i%,1%)
                if enblmod$(i%) = "N" then                               ~
                    str(modfac$(1%),i%,1%) = hex(8c) else                ~
                    str(modfac$(1%),i%,1%) = str(orgfac$(1%),i%,1%)
            next i%

            if sethit%  = 14% then return
            if sethit% <> 30% then enable_screen1
            call "ENABLFSB" ("MODIFY", "ARISHPIN", inpfac$(), modfac$())
            return

        REM *************************************************************~
            *      C O M M O N   S C R E E N   S U B R O U T I N E S    *~
            *-----------------------------------------------------------*~
            * Subroutines Common to all Screens.                        *~
            *************************************************************

        set_key         /* Input For Code Field Only */
            inpmessage$ = "Enter Ship-to Customer and Invoice Number."
            init(hex(8c)) lfac$()
            lfac$(1) = hex(81)
            lfac$(2) = hex(81)
            return

        set_edit        /* Edit Mode Display Only */
            inpmessage$ = "Press PF-10 for Full Screen Edit - OR - "  &  ~
                            "PF-16 to SAVE DATA"
            init(hex(8c)) lfac$()
            return

        set_errs        /* Error Correction Screen */
            inpmessage$ = "Correct ALL Bright Fields and Press RETURN" & ~
                            " to Validate Entries"
            return


        pfkey_hit
            if keyhit% <> 1% then pf13
                gosub startover
                keyhit% = 15%
                return

        pf13
            if keyhit% <> 13% then pf15
                call "MANUAL" ("ARISHPIN")
                keyhit% = 15%
                return

        pf15
            if keyhit% <> 15% then return
            call "PRNTSCRN"
            return

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *                 T E S T   K E Y   D A T A                 *~
            *-----------------------------------------------------------*~
            * Test data for Key Value Entered                           *~
            *************************************************************

        test_key
            errormsg$ = " "
            init(hex(8c)) lfac$()

        REM Test for Key Values Entered
*        1st Customer
            if cuscode$ = " " and invoice$ <> " " then L50310
            cuscodedescr$ = hex(0684) & "Select Ship-to Customer"
            call "GETCODE" (#03, cuscode$, cuscodedescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then L50180
                errormsg$ = "Customer not on file."
                lfac$( 1) = hex(81)
                return

L50180
*        2nd Invoice
            plowkey$ = str(cuscode$) & invoice$
            info$ = hex(0684) & "Select Customer Invoice for " &         ~
                    cuscode$ & " (" & cuscodedescr$ & ")"
            call "PLOWCODE" (#01, plowkey$, info$, 9%, 0, f1%(1))
            if f1%(1) = 1% then L50260
                errormsg$ = "Invoice not on file."
                lfac$( 2) = hex(81)
                return
L50260:     invoice$ = str(plowkey$, 10, 8)
            return

L50310
*        Go for just Invoice #
            plowkey$ = str(invoice$)
            header$(1) = "  Invoice    Customer Code"
            info$ = hex(0684) & "Select Customer Invoice"
            call "PLOWCODE" (#01, plowkey$, info$, 3000%, 1.09, f1%(1),  ~
                             header$(), 0.0, 1.0)
            if f1%(1) = 1% then L50390
                errormsg$ = "Invoice not on file."
                lfac$( 1) = hex(81) : lfac$( 2) = hex(81)
                return
L50390:     invoice$ = str(plowkey$, 1, 8)
            get #01 using L50410, cuscode$
L50410:         FMT CH(9)
            call "DESCRIBE" (#03, cuscode$, cuscodedescr$, 0%, f1%(3))
            if f1%(3) = 0% then cuscodedescr$ = "** Not on File **"
            return

        REM *************************************************************~
            *                 T E S T   D A T A                         *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        test_screen1
            testerror$, errormsg$ = " "
            init(hex(8c)) lfac$()

*       TEST1_FIELD03  /* Test for How Ship                    */
*        Memo field; no testing.

*       TEST1_FIELD04  /* Test for FOB                         */
*        Memo field; no testing.

*       TEST1_FIELD05  /* Test for Carrier                     */
            carrierdescr$ = " "
            if carrier$ = " " then test1_field06
                plowkey$ = "CARRIERS " & carrier$
                carrierdescr$ = hex(0684) & "Select Carrier. " &         ~
                                "PF16 if none."
                call "PLOWCODE" (#2, plowkey$, carrierdescr$, 9%, 0.30,  ~
                                                                  f1%(2))
                if f1%(2) = 1% then L51292
                     carrier$, carrierdescr$ = " " : goto test1_field06
L51292:         carrier$ = str(plowkey$,10)

        test1_field06   /* Test for Air/Frt Bill Number         */
*        Memo field; no testing.

*       TEST1_FIELD07  /* Test for Shipment- # of Cartons      */
            if cartons$ = " " then cartons$ = "0"
            convert cartons$ to cartons, data goto L51415 : goto L51420
L51415:         testerror$ = "Cartons must be a positive number"
                goto L51440
L51420:     if cartons < 0 then L51415
            call "CONVERT" (cartons, 2.2, cartons$)
            if cartons = 0 then cartons$ = " "
L51440:     if testerror$ = " " then test1_field08
                if errormsg$ = " " then errormsg$ = testerror$
                lfac$( 7) = hex(82)
                testerror$ = " "

        test1_field08   /* Test for           Weight            */
            if weight$ = " " then weight$ = "0"
            convert weight$ to weight, data goto L51485 : goto L51490
L51485:         testerror$ = "Weight must be a positive number"
                goto L51510
L51490:     if weight < 0 then L51485
            call "CONVERT" (weight, 2.2, weight$)
            if weight = 0 then weight$ = " "
L51510:     if testerror$ = " " then end_test1
                if errormsg$ = " " then errormsg$ = testerror$
                lfac$( 8) = hex(82)

        end_test1
            return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
