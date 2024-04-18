        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  BBBB   K   K   SSS   TTTTT   AAA   TTTTT          *~
            *  V   V  B   B  K  K   S        T    A   A    T            *~
            *  V   V  BBBB   KKK     SSS     T    AAAAA    T            *~
            *   V V   B   B  K  K       S    T    A   A    T            *~
            *    V    BBBB   K   K   SSS     T    A   A    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKSTAT   - A simple driver for POSTATUS                  *~
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
            * 11/04/92 ! Original                                 ! MLJ *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$35,                    /* Description                */~
            errormsg$79,                 /* Error message              */~
            hdr$(2)79,                   /* PLOWCODE Headers           */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            ponumber$16,                 /* Purchase Order Number      */~
            userid$3,                    /* Current User Id            */~
            vendor$9                     /* Vendor Number              */~

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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * #01 ! PAYMASTR ! PAYABLES MAIN FILE  (INVOICE DATES)      *~
            * #02 ! PAYLINES ! PAYABLES LINE ITEM FILE                  *~
            * #03 ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #04 ! VBKMASTR ! PURCHASE ORDER HEADER FILE               *~
            * #05 ! VBKLINES ! Purchase Order Line Items File           *~
            * #06 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #07 ! TXTFILE  ! System Text File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "PAYMASTR",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  25                      ~

            select #02, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key  2, keypos =   17, keylen =  47,         ~
                            key  1, keypos =    1, keylen =  63          ~

            select #03, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  3, keypos =  128, keylen =  24,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  1, keypos =    1, keylen =  69          ~

            select #04, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16          ~

            select #05, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos =  333, keylen =  20, dup     ~

            select #06, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup     ~

            select #07, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#02, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#03, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#04, fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (#05, fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#06, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENCHCK" (#07, fs%(7%), f2%(7%), 0%, rslt$(7%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            inpmessage$  = "To Modify Displayed Values, Press (RETURN)."

            str(line2$,62%) = "VBKSTAT : " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
L10080:     mode% = 2%
            inpmessage$ = "Enter Vendor Number Or Leave BLANK and Enter"&~
                          " PO Number."
            init(hex(81)) lfac$()
L10120:         gosub'101
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then exit_program
                      if keyhit% <>  0% then L10120
                gosub'151
                      if errormsg$ = " " then editpg1
                goto L10120

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            mode% = 3%
            inpmessage$ = "Press RETURN To Modify Or Select A PFkey Opt"&~
                          "ion Below."
            init(hex(84)) lfac$()
L11110:     gosub'101
                  if keyhit%  =  0% then L10080
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then show_status
                  goto L11110

        REM *************************************************************~
            *            L I N K   T O   P O S T A T U S                *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        show_status

            call "POSTATUS" (vendor$,        /* Vendor Number          */~
                             ponumber$,      /* Purchase Order Number  */~
                             " ",            /* BLANK for any line     */~
                             1%,             /* Our Units              */~
                             #5,             /* VBKLINES File UFB      */~
                             #3,             /* RCVLINES File UFB      */~
                             #4,             /* VBKMASTR File UFB      */~
                             #2,             /* PAYLINES File UFB      */~
                             #1,             /* PAYMASTR File UFB      */~
                             #7)             /* TXTFILE  File UFB      */
            goto inputmode

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      ponumber$, vendor$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101
              gosub set_pf1

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Purchase Order Inquiry",                              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Vendor Number",                              ~
               at (06,30), fac(lfac$(1%)), vendor$              , ch(09),~
                                                                         ~
               at (07,02), "Purchase Order Number",                      ~
               at (07,30), fac(lfac$(2%)), ponumber$            , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40340
                  call "MANUAL" ("VBKSTAT ")
                  goto L40090

L40340:        if keyhit% <> 15% then L40380
                  call "PRNTSCRN"
                  goto L40090

L40380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            if mode% = 3% then L40610
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return

L40610:     pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)PO Status   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151

        REM  Test for Vendor Number                     VENDOR$
             errormsg$ = " "
             if vendor$ = " " then L50150
                 if vendor$ = "?" then vendor$ = " "
             descr$ = hex(06) & "Select Vendor Number"
             call "GETCODE" (#6, vendor$, descr$, 0%, 1, f1%(6%))

L50150: REM  Test for Purchase Order Number             PONUMBER$
             errormsg$ = " "
             if ponumber$ = "?" or ponumber$ = " "                       ~
                 then ponumber$ = all(hex(00))
             if vendor$ = " " then L50450
                 if str(ponumber$,,1%)  = hex(00) then L50270
             readkey$ = str(vendor$) & str(ponumber$)
             call "READ100" (#4, readkey$, f1%(4%))
                 if f1%(4%) = 1% then return
                     errormsg$ = "Purchase Order Number is NOT Valid fo"&~
                                 "r this Vendor"
                     return
L50270:      plowkey$ = str(vendor$) & str(ponumber$)
             call "PLOWALTS" (#4, plowkey$, 0%, 9%, f1%(4%))
                 if f1%(4%) = 1% then L50330
                     errormsg$ = "There are NO Purchase Orders for this"&~
                                 " Vendor"
                     return
L50330:      plowkey$ = str(vendor$) & str(ponumber$)
             hdr$(1) = "PO Number            Vendor Description"
             descr$ = hex(06) & "Select Purchase Order Number"
             call "PLOWCODE" (#4, plowkey$,descr$, 3009%, .30, f1%(4%),  ~
                             hdr$(), 0, 26)
             if f1%(4%) <> 1% then L50410
                 ponumber$ = str(plowkey$,10%,16%)
                 return
L50410:      errormsg$ = "You Must Enter or Select a Valid PO Number fo"&~
                         "r this Vendor"
             return

L50450:      plowkey$ = str(ponumber$)
             errormsg$ = " "
             descr$ = hex(06) & "Select Purchase Order Number"
             call "PLOWCODE" (#4, plowkey$, descr$, 3000%, 1.30, f1%(4%),~
                             hdr$(), 0, 26)
             if f1%(4%) = 1% then L50530
               errormsg$ = "You must Enter or Select a valid PO Number"
               return
L50530:      ponumber$ = str(plowkey$,1%,16%)
             call "REDALT0" (#4, plowkey$, 1%, f1%(4%))
                 get #4 using L50560, vendor$
L50560:      FMT CH(9)
             return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
