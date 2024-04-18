        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  PPPP    CCC   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  V   V  P   P  C   C    I    NN  N  P   P  U   U    T     *~
            *  V   V  PPPP   C        I    N N N  PPPP   U   U    T     *~
            *   V V   P      C   C    I    N  NN  P      U   U    T     *~
            *    V    P       CCC   IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPCINPUT - Manage Purchasing Contracts.                   *~
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
            * 06/16/94 ! Original                                 ! ERN *~
            * 08/28/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            audit$79,                    /* Audit fields display       */~
            blankdate$8,                 /* blank unfmt date           */~
            comments$(2)50,              /* Comments                   */~
            contract_descr$30,           /* Contract Description       */~
            contract_id$16,              /* Contract ID                */~
            contract_line$4,             /* Contract Line ID           */~
            changed_by$3,                /* Last Changed By            */~
            changed_on$10,               /* Last Changed On            */~
            created_by$3,                /* Created By                 */~
            created_on$10,               /* Created On                 */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            end_date$10,                 /*          End Date          */~
            end_dateu$10,                /* end date unfmt             */~
            errormsg$79,                 /* Error message              */~
            hdr_descr$30, hdr_vendor$9,  /* 'Header' Variables         */~
            hdr_end$8, hdr_start$8,      /*                            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            item_code$25,                /* Item Code                  */~
            item_code_descr$32,          /* Item Code Description      */~
            item_type$1,                 /* Item Type (P/A/M)          */~
            item_type_descr$32,          /* Item Type (P/A/M)          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            line_start$6, line_end$6,    /* Test Dates                 */~
            max_date$8,                  /* Maximum date 20991231      */~
            max_dollar$10,               /* Maximum Dollars Allowed    */~
            max_qty$10,                  /* Maximum Quantity           */~
            min_dollar$10,               /* Minimum Dollars Allowed    */~
            min_qty$10,                  /* Minimum Quantity           */~
            msg$(3)79,                   /* Messages array             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            record1$(3)250,              /* Before Pic                 */~
            record2$(3)250,              /* After  Pic                 */~
            start_date$10,               /* Contract Start Date        */~
            start_dateu$10,              /* start date unfmt           */~
            temp_start$6, temp_end$6,    /* Test Dates                 */~
            text$(392,1)70,              /* Text Array                 */~
            text_id$4,                   /* Text ID                    */~
            uom$4,                       /* Unit of Measure            */~
            uom_descr$30,                /* Unit of Measure Descr      */~
            unit_price$10,               /* Unit Price                 */~
            userid$3,                    /* Current User Id            */~
            vendor$9,                    /* Vendor                     */~
            vendor_descr$32              /* Vendor                     */~

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! GENCODES ! System General Codes file.               *~
            * #05 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #06 ! TXTFILE  ! System Text File                         *~
            * #07 ! VBKLINES ! Purchase Order Lines                     *~
            * #08 ! PAYLINES ! Payables Line Items                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "VPCMASTR",                                      ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =  10,  keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   60, keylen =  26, dup

            select #03, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #04, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #05, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup

            select #06, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select # 7, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =  28,                     ~
                        alt key 1, keypos = 333, keylen = 20, dup

            select  #8, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 1%, rslt$(01))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))

            if fs%(1) + fs%(3) + fs%(4) + fs%(5) + fs%(6) = 5% then L09000
                call "ASKUSER" (keyhit%, "FILE(S) UNAVAILABLE",          ~
                                "Unable to open all files",              ~
                                " ", "Press <RETURN> to continue...")
                goto exit_program

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            max_date$  = "20991231"
            call "DATECONV" (max_date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "VPCINPUT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            edit_mode% = 0%

            for fieldnr% = 1% to 11%
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

            str(line2$,,60) = "Contract ID: " & contract_id$   & "-"  &  ~
                                                contract_line$        &  ~
                                        " ("  & contract_descr$ & ")"
            call "VFINPSUB" ("VPCMASTR", "I", "Purchasing Contracts  ",  ~
                             str(line2$,,60), "NN", vf$, keyhit%)
            if keyhit% = 1% then inputmode

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            edit_mode% = 1%
            str(line2$,,60) = "Contract ID: " & contract_id$ & "  ("     ~
                              & contract_descr$ & ")"
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  =  8% then gosub delete_record
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 25% then gosub edit_text
                  if keyhit% <>  0% then       editpg1

L11170:     fieldnr%     = cursor%(1%) - 5%
            if fieldnr% <=  1%          then editpg1
            if fieldnr%  =  6%          then fieldnr% = 5%
            if fieldnr% <=  5%          then L11230
                                             fieldnr% = fieldnr% - 1%
            if fieldnr% >= 11%          then fieldnr% = 11%
L11230:     if fieldnr%  = lastfieldnr% then editpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11270:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11270
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11270
                  lastfieldnr% = fieldnr%
            goto L11170


        editpg2
            call "VFINPSUB" ("VPCMASTR", "E", "Purchasing Contracts  ",  ~
                             str(line2$,,60), "YN", vf$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then editpg1
            if keyhit% = 16% then datasave
                             goto editpg1


        edit_text
            call "TXTINSUB" (#06, f2%(6), "038", str(line2$,,60),        ~
                                                      text_id$, text$())
            return


        delete_record
            call "ASKUSER" (key%, "** CONFIRM DELETION **",              ~
                "Press PF-8 to DELETE this Record", "-OR-",              ~
                "Press RETURN to CANCEL deletion.")
            if key% = 0% then return
                if key% <> 8% then delete_record
                     readkey$ = str(contract_id$,,16) & contract_line$
                     call "DELETE"   (#1, readkey$, 20%)
                     call "TXTFUTIL" (#6, f2%(6), "DELE", text_id$)
                     return clear all
                     goto inputmode


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20210,         /* Contract ID            */~
                              L20240,         /* Contract Descr         */~
                              L20290,         /* Vendor                 */~
                              L20350,         /* Item Type (P/A/M)      */~
                              L20420,         /* Item Code              */~
                              L20460,         /* Start-End Dates        */~
                              L20550,         /* Min-Max Quantities     */~
                              L20590,         /* Unit of Measure        */~
                              L20630,         /* Unit Price             */~
                              L20670,         /* Min-Max Dollars        */~
                              L20790          /* Comments               */
            return

L20210: REM Def/Enable Contract ID                 CONTRACT_ID$ _LINE$
            return

L20240: REM Def/Enable Contract Description        CONTRACT_DESCR$
            if first% = 1% and contract_descr$ = " " then                ~
                contract_descr$ = hdr_descr$
            return

L20290: REM Def/Enable Vendor                      VENDOR$
            if first% = 0% then L20330
                enabled% = 0%
                vendor$  = hdr_vendor$
L20330:     return

L20350: REM Def/Enable Item Type (P/A/M/H)         ITEM_TYPE$
            if contract_line$ <> " " then L20400
                item_type$ = "H"
                enabled%   = 0%
                return
L20400:     return

L20420: REM Def/Enable Item Code                   ITEM_CODE$
            if contract_line$ = " " or item_type$ = "M" then enabled% = 0%
            return

L20460: REM Def/Enable Contract Start Date         START_DATE$, END_DATE$
            if first% = 0% or ~
               start_date$ <> " " or start_date$ <> blankdate$ or ~
               end_date$   <> " " or end_date$   <> blankdate$ then return
               if hdr_start$ = blankdate$ then ~
                  start_date$ = date      else ~
                  start_date$ = hdr_start$
               start_dateu$ = start_date$
               call "DATFMTC" (start_date$)
               if hdr_end$   = blankdate$ then ~
                  end_date$  = max_date$  else ~
                  end_date$  = hdr_end$
               end_dateu$ = end_date$
               call "DATFMTC" (end_date$)
            return

L20550: REM Def/Enable Minimum Quantity            MIN_QTY$, MAX_QTY$
            if contract_line$ = " " then enabled% = 0%
            return

L20590: REM Def/Enable Unit of Measure             UOM$
            if contract_line$ <> " " then L20603
                enabled% = 0%
                return
L20603:     if item_type$ = "P" and f1%(3) = 1% and uom$ = " " then      ~
                get #3 using L20606, uom$
L20606:              FMT POS(74), CH(4)
            return

L20630: REM Def/Enable Unit Price                  UNIT_PRICE$
            if contract_line$ = " " then enabled% = 0%
            return

L20670: REM Def/Enable Min, Max $s                 MIN_DOLLAR$, MAX_
            if unit_price$ = " " then unit_price = 0                     ~
                                 else convert unit_price$ to unit_price
            if edit_mode% = 1%  or unit_price = 0 then return
                min_dollar = min_qty * unit_price
                max_dollar = max_qty * unit_price
                if min_dollar <> 0 then                                  ~
                     call "CONVERT" (min_dollar, -0.2, min_dollar$)
                if max_dollar <> 0 then                                  ~
                     call "CONVERT" (max_dollar, -0.2, max_dollar$)
                return

L20790: REM Def/Enable Comments                    COMMENTS$()
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
         "Enter Contract ID and Line Number.                           ",~
         "Enter Contract Description.                                  ",~
         "Enter Vendor.  Leave blank to search file.                   ",~
         "Enter Item Type (P=Part, A=Activity, M=Miscellaneous).       ",~
         "Enter Item Code.  For Item Types P & A leave blank to search.",~
         "Enter Contract Start and End dates.                          ",~
         "Enter Minimum and Maximum Quantities.                        ",~
         "Enter Unit of Measure Code.                                  ",~
         "Enter Unit Price.                                            ",~
         "Enter Minimum and Maximum Dollar Limits.                     ",~
         "Enter Comments.                                              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables

            old_max_dollar = 0

            init(" ") errormsg$, inpmessage$,                            ~
                      comments$(1), comments$(2), contract_descr$,       ~
                      contract_id$, item_code$, item_type$,   ~
                      item_type_descr$, max_qty$, min_qty$,  ~
                      unit_price$, vendor$, vendor_descr$, uom$,         ~
                      item_code_descr$, uom_descr$, contract_line$,      ~
                      min_dollar$, max_dollar$, hdr_vendor$, hdr_descr$, ~
                      start_date$, end_date$

            end_dateu$, start_dateu$, hdr_start$, hdr_end$ = blankdate$

            init (" ") vf$, created_on$, changed_by$, changed_on$,       ~
                       record1$(), record2$(), str(line2$,,60), audit$

            text_id$ = all(hex(ff))
            call "TXTFUTIL" (#06, f2%(6), "INTL", text_id$)

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
            readkey$ = str(contract_id$,,16) & "   "
            call "READ100"  (#1, readkey$, header%)
            first% = header%
            if header% = 1% then L30120
                readkey$ = str(contract_id$,,16) & hex(00)
                call "PLOWNEXT" (#1, readkey$, 16%, first%)
                if first% = 0% then L30160
L30120:             get #1 using L30140, hdr_vendor$, hdr_descr$,         ~
                                        hdr_start$ , hdr_end$
L30140:                   FMT  CH(9), XX(20), CH(30), POS(86), 2*CH(6)

L30160:     created_by$ = userid$
            created_on$ = date$
            if contract_line$ = " " then                                 ~
                audit$ = "** New Header Record."                         ~
            else                                                         ~
                audit$ = "** New Item Record."
            if first% = 1% then                                          ~
                audit$ = audit$& "  Contract entry(s) already on file."  ~
            else                                                         ~
                audit$ = audit$& "  No entries for this contract on file."

            readkey$ = str(contract_id$) & contract_line$
            call "READ100" (#1, readkey$, f1%(1))

            if f1%(1) = 0% then return

            get #1 using L35060, vendor$, contract_id$, contract_line$,   ~
                                contract_descr$,                         ~
                                item_type$, item_code$, start_date$,     ~
                                end_date$, uom$, min_qty, max_qty,       ~
                                unit_price, min_dollar, max_dollar,      ~
                                comments$(), text_id$, vf$,              ~
                                created_by$, created_on$,                ~
                                changed_by$, changed_on$
            old_max_dollar = max_dollar
            call "DATEFMT" (created_on$)
            call "DATEFMT" (changed_on$)
            if changed_by$ = " " then                                    ~
                audit$ = "Record created by " & created_by$ & " on "  &  ~
                         created_on$ & "."                               ~
            else                                                         ~
                audit$ = "Record created by " & created_by$ & " on "  &  ~
                         created_on$ & "; " & "last changed by "      &  ~
                         changed_by$ & " on " & changed_on$ & "."
            call "DESCRIBE" (#5, vendor$, vendor_descr$, 0%, f1%(5))
            item_type_descr$ = "Miscellaneous"
            if item_type$ =  "H" then item_type_descr$ = "Header"
            if item_type$ <> "P" then L30560
                item_type_descr$ = "Part  Code"
                call "DESCRIBE" (#3, item_code$, item_code_descr$, 0%,   ~
                                                                  f1%(3))
L30560:     if item_type$ <> "A" then L30610
                item_type_descr$ = "WC Activity"
                readkey$ = "WC ACTVTY" & item_code$
                call "DESCRIBE" (#4, readkey$, item_code_descr$, 0%,     ~
                                                                  f1%(4))
L30610:     start_dateu$ = start_date$
            end_dateu$   = end_date$
            call "DATFMTC" (start_date$)
            call "DATFMTC" (end_date$  )
            readkey$ = "UOM      " & uom$
            call "DESCRIBE" (#4, readkey$, uom_descr$, 0%, f1%(4))
            if min_qty <> 0 then                                         ~
                call "CONVERT" (min_qty   , -0.4, min_qty$   )
            if max_qty <> 0 then                                         ~
                call "CONVERT" (max_qty   , -0.4, max_qty$   )
            if unit_price <> 0 then                                      ~
                call "CONVERT" (unit_price, -2.7, unit_price$)
            if min_dollar <> 0 then                                      ~
                call "CONVERT" (min_dollar, -0.2, min_dollar$)
            if max_dollar <> 0 then                                      ~
                call "CONVERT" (max_dollar, -0.2, max_dollar$)

            call "TXTFUTIL" (#06, f2%(6), "LOAD", text_id$)

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "DATUFMTC" (start_date$)
            call "DATUFMTC" (end_date$  )
            if min_qty$    = " " then min_qty$    = "0"
            if max_qty$    = " " then max_qty$    = "0"
            if unit_price$ = " " then unit_price$ = "0"
            if min_dollar$ = " " then min_dollar$ = "0"
            if max_dollar$ = " " then max_dollar$ = "0"
            convert min_qty$    to min_qty
            convert max_qty$    to max_qty
            convert unit_price$ to unit_price
            convert min_dollar$ to min_dollar
            convert max_dollar$ to max_dollar
            call "DATUNFMT" (created_on$)
            call "DATUNFMT" (changed_on$)
            readkey$ = str(contract_id$) & contract_line$
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 1% then get #1 using L31145, str(record1$())
L31145:         FMT CH(600)
            put #1 using L35060, vendor$, contract_id$, contract_line$,   ~
                                contract_descr$,                         ~
                                item_type$, item_code$, start_date$,     ~
                                end_date$, uom$, min_qty, max_qty,       ~
                                unit_price, min_dollar, max_dollar,      ~
                                comments$(), text_id$, vf$,              ~
                                created_by$, created_on$,                ~
                                changed_by$, changed_on$, " "
            get #1 using L31145, str(record2$())
            if f1%(1) = 1% and (str(record1$()) <> str(record2$())) then ~
                put #1 using L31240, userid$, date
L31240:              FMT POS(455), CH(3), CH(6)
            if f1%(1) = 0% then write #1 else rewrite #1
            call "TXTFUTIL" (#06, f2%(6), "TOS2", text_id$)
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

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40260,         /* Contract ID       */   ~
                                L40250,         /* Contract Descr    */   ~
                                L40260,         /* Vendor            */   ~
                                L40260,         /* Item Type (P/A/M) */   ~
                                L40260,         /* Item Code         */   ~
                                L40260,         /* Start, End Dates  */   ~
                                L40270,         /* Min, Max Qtys     */   ~
                                L40260,         /* Unit of Measure   */   ~
                                L40270,         /* Unit Price        */   ~
                                L40270,         /* Min-Max Dollars   */   ~
                                L40250          /* Comments          */
              goto L40290

L40250:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40260:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40270:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40290:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Purchasing Contracts",                         ~
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
               at (09,02), "Item Type",                                  ~
               at (09,30), fac(lfac$( 4)), item_type$           , ch(01),~
               at (09,49), fac(hex(8c)),   item_type_descr$     , ch(32),~
                                                                         ~
               at (10,02), "Item Code",                                  ~
               at (10,30), fac(lfac$( 5)), item_code$           , ch(25),~
                                                                         ~
               at (11,02), "Item Code Description",                      ~
               at (11,30), fac(hex(8c))  , item_code_descr$     , ch(32),~
                                                                         ~
               at (12,02), "Start/End Dates",                            ~
               at (12,30), fac(lfac$( 6)), start_date$          , ch(10),~
               at (12,42), fac(lfac$( 6)), end_date$            , ch(10),~
                                                                         ~
               at (13,02), "Min/Max Quantities",                         ~
               at (13,30), fac(lfac$( 7)), min_qty$             , ch(10),~
               at (13,42), fac(lfac$( 7)), max_qty$             , ch(10),~
                                                                         ~
               at (14,02), "Unit of Measure ",                           ~
               at (14,30), fac(lfac$( 8)), uom$                 , ch(04),~
               at (14,49), fac(hex(8c)),   uom_descr$           , ch(30),~
                                                                         ~
               at (15,02), "Unit Price",                                 ~
               at (15,30), fac(lfac$( 9)), unit_price$          , ch(10),~
                                                                         ~
               at (16,02), "Min/Max Dollars",                            ~
               at (16,30), fac(lfac$(10)), min_dollar$          , ch(10),~
               at (16,42), fac(lfac$(10)), max_dollar$          , ch(10),~
                                                                         ~
               at (17,02), "Comments",                                   ~
               at (17,30), fac(lfac$(11)), comments$(1)         , ch(50),~
               at (18,30), fac(lfac$(11)), comments$(2)         , ch(50),~
                                                                         ~
               at (19,02), fac(hex(8c)),   audit$               , ch(70),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40880
                  call "MANUAL" ("VPCINPUT") : goto L40290

L40880:        if keyhit% <> 15 then L40910
                  call "PRNTSCRN" : goto L40290

L40910:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41100     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41060
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41060:     if fieldnr% > 2% then L41080
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41080:     return

L41100: if fieldnr% > 0% then L41190  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (8) Delete Record      " &        ~
                     "(25) Manage Text       (15)Print Screen"
            pf$(3) = "(5) Next Page                           " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffff05ffff08ffff19ff0dff0f1000)
            str(pf$(2),39,1) = hex(84)
            str(pf$(2),57,1) = hex(8c)
            if text_id$ = hex(00000000) or text_id$ = hex(ffffffff) or   ~
               text_id$ = " "                                      then  ~
                str(pf$(2),39,1) = hex(8c)
            return
L41190:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50210,         /* Contract ID            */~
                              L50340,         /* Contract Descr         */~
                              L50390,         /* Vendor                 */~
                              L50470,         /* Item Type (P/A/M)      */~
                              L50650,         /* Item Code              */~
                              L50900,         /* Start-End Dates        */~
                              L51470,         /* Min-Max Quantities     */~
                              L51940,         /* Unit of Measure Code   */~
                              L52060,         /* Unit Price             */~
                              L52110,         /* Min-Max Dollars        */~
                              L52970          /* Comments               */
            return

L50210: REM Test for Contract ID                  CONTRACT_ID$
            if contract_id$  = "?" then contract_id$ = " "
            if contract_id$ <> " " then L50290
                call "VPCPIKME" (vendor$, contract_id$, contract_line$,  ~
                                          " ", " ", " ", #1, #5, f1%(1%))
                if f1%(1%) <> 0% then L50290
                     errormsg$ = hex(00)
                     return
L50290:     gosub dataload
            if f1%(1) = 0% then return
                return clear all
                goto editpg1

L50340: REM Test for Contract Description         CONTRACT_DESCR$
            if contract_descr$ = " " then                                ~
                errormsg$ = "Contract Description cannot be blank."
            return

L50390: REM Test for Vendor                       VENDOR$
            vendor_descr$ = hex(06) & "Select Vendor Code."
            f1%(5) = -8%
            call "PLOWCODE" (#5, vendor$, vendor_descr$, 0%, 0.3, f1%(5))
            if f1%(5) = 0% then                                          ~
                errormsg$ = "Vendor not on file.  Please re-enter."
            return

L50470: REM Test for Item Type (P/A/M)            ITEM_TYPE$
            item_type_descr$     = " "
            if contract_line$   <> " " then L50530
                item_type$       = "H"
                item_type_descr$ = "Header"
                return
L50530:     if pos("PAM" = item_type$) > 0% then L50560
                errormsg$ = "Enter 'P', 'A', or 'M' for Item Type."
                return
L50560:     if item_type$  = "P" then item_type_descr$ = "Part Code"
            if item_type$  = "A" then item_type_descr$ = "WC Activity"
            if item_type$  = "M" then item_type_descr$ = "Miscellaneous"

            if edit_mode%  = 0%  then return
              fieldnr%     = fieldnr% + 1%
              lastfieldnr% = fieldnr%
              cursor%(1)   = 10%

L50650: REM Test for Item Code                    ITEM_CODE$
            if item_type$ = "H" then return
            on pos("PAM" = item_type$) goto L50690, L50780, L50880

L50690
*         Test Part Number
            item_code_descr$ = hex(06) & "Select Part Code."
            f1%(3) = -10%
            call "PLOWCODE" (#3, item_code$, item_code_descr$,           ~
                                                         0%, 0.3, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "Part Code not on file.  Please re-enter."
                return

L50780
*         Test Activity Code
            plowkey$ = "WC ACTVTY" & item_code$
            item_code_descr$ = hex(06) & "Select WC Activity Code."
            f1%(4) = -10%
            call "PLOWCODE" (#4, plowkey$, item_code_descr$,             ~
                                                         9%, 0.3, f1%(4))
            if f1%(4) = 1% then L50870
                errormsg$ = "Activity Code not on file.  Please re-enter."
                return
L50870:     item_code$ = str(plowkey$,10)
L50880:     return

L50900: REM Test for Contract Start-End Dates     START_DATE$, END_DATE$
            if start_date$ = " " or start_date$ = blankdate$ then ~
               start_date$ = date
            call "DATEOKC" (start_date$, start_date%, errormsg$)
            if errormsg$ <> " " then return
            start_dateu$ = start_date$
            call "DATUFMTC" (start_dateu$)

            if end_date$ = " " or end_date$ = blankdate$ then ~
               end_date$ = start_date$
            call "DATEOKC" (end_date$, end_date%, errormsg$)
            if errormsg$ <> " " then return
            end_dateu$ = end_date$
            call "DATUFMTC" (end_dateu$)

            if start_dateu$ <= end_dateu$ then L51040
                errormsg$ = "Start Date must be on or before End Date."
                return

*        If a line w/ a header, test line dates against header dates
L51040:     if contract_line$ = " " or header% = 0% then L51200
               call "DATEFMT" (hdr_start$, hdr_start%)
               call "DATEFMT" (hdr_end$,   hdr_end%)
               call "DATUNFMT" (hdr_start$)
               call "DATUNFMT" (hdr_end$)

                if start_dateu$ >= hdr_start$ then L51110
                     errormsg$ = "Line start date is before the " &      ~
                                 "contract start date."
                     return
L51110:         if start_dateu$ <= hdr_end$ then L51150
                     errormsg$ = "Line start date is after the "  &      ~
                                 "contract end date."
                     return
L51150:         if end_dateu$ <= hdr_end$ then return
                     errormsg$ = "Line end date is after the "  &        ~
                                 "contract end date."
L51180:              return

L51200
*        If a header, check that dates don't step on existing lines
            if contract_line$ <> " " then return
                line_start$ = max_date$
                line_end$   = blankdate$
                readkey$ = str(contract_id$) & hex(20202021)
L51240:         call "PLOWNEXT" (#1, readkey$, 16%, f1%(1))
                if f1%(1) = 0% then L51330
                     get #1 using L51270, temp_start$, temp_end$
L51270:                   FMT POS(86), 2*CH(6)
                     if temp_start$ < line_start$ then                   ~
                          line_start$ = temp_start$
                     if temp_end$   > line_end$   then                   ~
                          line_end$   = temp_end$
                     goto L51240

L51330:         if line_start$ = max_date$ then return
L51380:         if start_dateu$ <= line_start$ then L51420
                   errormsg$ = "Line item(s) already exist with "&~
                               "earlier start dates."
                return
L51420:         if end_dateu$ >= line_end$ then return
                   errormsg$ = "Line item(s) already exist with "&~
                               " later end dates."
                return

L51470: REM Test for Min-Max Quantity             MIN_QTY$, MAX_QTY$
            call "NUMTEST" (min_qty$, 0, 9e7, errormsg$, 0.4, min_qty)
            if errormsg$ <> " " then return

            call "NUMTEST" (max_qty$, min_qty, 9e7, errormsg$, 0.4,      ~
                                                                 max_qty)
            if min_qty = 0 and max_qty = 0 then min_qty$, max_qty$ = " "

*        Check that actuals do not exceed limits
            call "VPCSUMSB" (                                            ~
                     #01,                /* VPCMASTR channel           */~
                     #07,                /* VBKLINES channel           */~
                     #08,                /* PAYLINES channel           */~
                     contract_id$,       /* Contract to summarized     */~
                     contract_line$,     /* Line to be  summarized     */~
                                         /* *** HEADER LEVEL INFO ***  */~
                     hdr%,               /*  HDR Exists? 0=No, 1=Yes   */~
                     hdr_min_dlr,        /*  Minimum Dollars           */~
                     hdr_max_dlr,        /*  Maximum Dollars           */~
                     temp_start$,        /*  Start Date                */~
                     temp_end$,          /*  End   Date                */~
                     vpc_vbk_dlr,        /*  PO  Dollars               */~
                     vpc_pay_dlr,        /*  A/P Dollars               */~
                                         /* **** LINE LEVEL INFO ****  */~
                     lin%,               /*  Line Exists? 0=No, 1=Yes  */~
                     lin_min_dlr,        /*  Minimum Dollars           */~
                     lin_max_dlr,        /*  Maximum Dollars           */~
                     lin_min_qty,        /*  Minimum Qty               */~
                     lin_max_qty,        /*  Maximum Qty               */~
                     temp_start$,        /*  Start Date                */~
                     temp_end$,          /*  End   Date                */~
                     lin_vbk_qty,        /*  PO  Quantity              */~
                     lin_vbk_dlr,        /*  PO  Dollars               */~
                     lin_pay_qty,        /*  A/P Quantity              */~
                     lin_pay_dlr )       /*  A/P Dollars               */

            init(" ") msg$()
            if max_qty >= lin_vbk_qty + lin_pay_qty then return
                msg$(1) =                                                ~
                    "PO + Invoice quantities exceed line item maximum."
                msg$(2) = " "
                msg$(3) = "Press RETURN to acknowledge message."
                call "ASKUSER" (keyhit%, "OVER LIMIT CONDITION",         ~
                                msg$(1), msg$(2), msg$(3))
                return
            return

L51940: REM Test for Unit of Measure Code         UOM$
            if contract_line$ = " " then return
            plowkey$ = "UOM      " & uom$
            uom_descr$ = hex(06) & "Select Unit of Measure Code."
            f1%(4) = -14%
            call "PLOWCODE" (#4, plowkey$, uom_descr$, 9%, 0.3, f1%(4))
            errormsg$ = "Invalid Unit of Measure Code.  Please re-enter."
            if f1%(4) = 0% then L52040
                errormsg$ = " "
                uom$ = str(plowkey$,10)
L52040:     return

L52060: REM Test for Unit Price                   UNIT_PRICE$
            call "NUMTEST" (unit_price$, 0, 9e7, errormsg$, 0.4, temp)
            if temp = 0 then unit_price$ = " "
            return

L52110: REM Test for Min-Max Dollars              MIN_ MAX_DOLLAR$
            call "NUMTEST" (min_dollar$, 0, 9e7,                         ~
                                             errormsg$, 0.2, min_dollar)
            if errormsg$ <> " " then return

            call "NUMTEST" (max_dollar$, min_dollar, 9e7,                ~
                                             errormsg$, 0.2, max_dollar)
            if min_dollar = 0 and max_dollar = 0 then                    ~
                                          min_dollar$, max_dollar$ = " "
            if min_dollar = 0 and max_dollar = 0 then return

*        Do some dollar checks...
*        Check that line items maxs do not exceed header maxs then
*        check that current actuals do not exceed the max for the
*        entity being maintained.
            call "VPCSUMSB" (                                            ~
                     #01,                /* VPCMASTR channel           */~
                     #07,                /* VBKLINES channel           */~
                     #08,                /* PAYLINES channel           */~
                     contract_id$,       /* Contract to summarized     */~
                     contract_line$,     /* Line to be  summarized     */~
                                         /* *** HEADER LEVEL INFO ***  */~
                     hdr%,               /*  HDR Exists? 0=No, 1=Yes   */~
                     hdr_min_dlr,        /*  Minimum Dollars           */~
                     hdr_max_dlr,        /*  Maximum Dollars           */~
                     temp_start$,        /*  Start Date                */~
                     temp_end$,          /*  End   Date                */~
                     vpc_vbk_dlr,        /*  PO  Dollars               */~
                     vpc_pay_dlr,        /*  A/P Dollars               */~
                                         /* **** LINE LEVEL INFO ****  */~
                     lin%,               /*  Line Exists? 0=No, 1=Yes  */~
                     lin_min_dlr,        /*  Minimum Dollars           */~
                     lin_max_dlr,        /*  Maximum Dollars           */~
                     lin_min_qty,        /*  Minimum Qty               */~
                     lin_max_qty,        /*  Maximum Qty               */~
                     temp_start$,        /*  Start Date                */~
                     temp_end$,          /*  End   Date                */~
                     lin_vbk_qty,        /*  PO  Quantity              */~
                     lin_vbk_dlr,        /*  PO  Dollars               */~
                     lin_pay_qty,        /*  A/P Quantity              */~
                     lin_pay_dlr )       /*  A/P Dollars               */

*        Sum line item maximum dollars up...
            lin_sum = 0
            readkey$ = str(contract_id$,,16) & hex(20202021)
L52560:     call "PLOWNEXT" (#1, readkey$, 16%, f1%(1))
            if f1%(1) = 0% then L52630
                get #1 using L52590, temp
L52590:              FMT POS(134), PD(14,4)
                lin_sum = lin_sum + temp
                goto L52560

L52630
*        Adjust entity being changed
            adj = max_dollar - old_max_dollar
            if contract_line$ = " " then hdr_max_dlr = hdr_max_dlr + adj
            if contract_line$ = " " then L52700
                lin_max_dlr   = lin_max_dlr + adj
                lin_sum       = lin_sum     + adj

L52700:     real_hdr = vpc_vbk_dlr + vpc_pay_dlr
            real_lin = lin_vbk_dlr + lin_pay_dlr

            init(" ") msg$()
            if contract_line$ <> " " then L52820

*        Header tests
            if hdr_max_dlr < real_hdr then msg$(1) =                     ~
                "PO + Invoice dollars exceed header maximum."
            if hdr_max_dlr < lin_sum  then msg$(2) =                     ~
                "Contract line dollars exceed header maximum."
            goto L52820

*        Line tests
L52820:     if lin_max_dlr < real_lin then msg$(1) =                     ~
                "PO + Invoice dollars exceed line item maximum."
            if hdr_max_dlr < lin_sum and hdr% = 1% and hdr_max_dlr > 0   ~
                                      then msg$(2) =                     ~
                "Contract line dollars exceed header maximum."

            if msg$(1) = " " and msg$(2) = " " then return
                if msg$(1) <> " " then L52910
                     msg$(1) = msg$(2)
                     msg$(2) = " "
L52910:         msg$(3) = "Press RETURN to acknowledge message."
                call "ASKUSER" (keyhit%, "OVER LIMIT CONDITION",         ~
                                msg$(1), msg$(2), msg$(3))
                return


L52970: REM Test for Comments                     COMMENTS$
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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("Session Terminating")

            end
