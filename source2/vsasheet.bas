        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V   SSS    AAA    SSS   H   H  EEEEE  EEEEE  TTTTT   *~
            *  V   V  S      A   A  S      H   H  E      E        T     *~
            *  V   V   SSS   AAAAA   SSS   HHHHH  EEE    EEE      T     *~
            *   V V       S  A   A      S  H   H  E      E        T     *~
            *    V     SSS   A   A   SSS   H   H  EEEEE  EEEEE    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VSASHEET - Standalone driver to VSASHTSB for printing of  *~
            *            Vendor Service Sheets.                         *~
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
            * 07/13/94 ! Original                                 ! ERN *~
            * 11/11/94 ! Changes to Dummy File Channel, Screen Hdg! LDJ *~
            * 09/02/97 ! Year 2000 add all(hex(00)) to plowkey    ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            activity$4,                  /* WC Activity Code           */~
            act_descr$30,                /* WC Activity Code Descr     */~
            advice$8,                    /* Vendor Service Advice      */~
            comment$40,                  /* Shipment Log Comment       */~
            contract$16,                 /* Contract ID                */~
            contract_line$4,             /* Contract Line Number       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            doc$20,                      /* Shipment Document Ref #    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            job$8, job_descr$30,         /* Job Number                 */~
            job_part$25,                 /* What's we to build         */~
            job_part_descr$32,           /* What's we call it          */~
            l$(5)79, l$79, lf$1,         /* Display lines              */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            on_file$30,                  /* Descriptor Line            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plow_hdr$(2)80,              /* PLOWCODE sh--              */~
            plow_inex(2), plow_inex$(2)2,/*                            */~
            plow_map(20),                /*                            */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po$16, po_line$3,            /* Purchase Order             */~
            qty_out$10,                  /* Quantity Shipped to Vendor */~
            rte_step$4,                  /* Step on the Route          */~
            ship_date$8,                 /* Date Shipped               */~
            shipper$30,                  /* Shipper                    */~
            srce$8,                      /* Source Program             */~
            srce_user$3,                 /* Original User ID           */~
            temp_date$8,                 /* Temporary Date Element     */~
            trans_date$8,                /* Transaction Date           */~
            trans_time$8,                /* Transaction Time Stamp     */~
            trans_type$1,                /* Ship Log Trans Type        */~
            uom$4, uom_descr$30,         /* Unit of Measure            */~
            userid$3,                    /* Current User Id            */~
            vendor$9,                    /* The Vendor Code            */~
            vsa_qty$30,                  /* Advice Qty formatted       */~
            vsa_out$10, vsa_in$10,       /* Total ins and outs         */~
            wc_start$8, wc_end$8         /* WC Start and End Dates     */

        dim f2%(14),                     /* = 0 if the file is open    */~
            fs%(14),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            f1%(14),                     /* Read routine statii        */~
            rslt$(14)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "


        REM *************************************************************~
            *                     F I L E S                             *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! VSAOUTIN ! Outside Processing Transfer Log          *~
            * #02 ! VBKVSA   ! Vendor Service Advices file              *~
            * #03 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #04 ! JBMASTR2 ! Production job master file               *~
            * #05 ! HNYMASTR ! Inventory Master File                    *~
            * #06 ! GENCODES ! System General Codes file.               *~
            * #07 ! DUMMY    ! For PLOWCODE                             *~
            *************************************************************~

            select #01, "VSAOUTIN",                                      ~
                        varc, indexed, recsize =  316,                   ~
                        keypos =   1, keylen =  26,                      ~
                        alt key    1, keypos =  44, keylen =  8, dup,    ~
                            key    2, keypos =  52, keylen = 23, dup

            select #02, "VBKVSA",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    5, keylen =   8,                     ~
                        alt key  6, keypos =   50, keylen =   4, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  1, keypos =    1, keylen =  12          ~

            select #03, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #04, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select #05, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos =  90, keylen = 4, dup,  ~
                                  key 3, keypos =  26, keylen = 32, dup

            select #06, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #07, "DUMMY", varc, indexed, recsize = 69,            ~
                                 keypos = 1, keylen =  2

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 1%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))

            if f2%(1) + f2%(2) + f2%(3) + f2%(4) + f2%(5) + f2%(6) = 0%  ~
                                                                then L09000
                call "ASKUSER" (0%, "FILE OPEN ERROR",                   ~
                                "Unable to open all required files.",    ~
                                " ",                                     ~
                                "Please press RETURN to exit program.")
                goto exit_program


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)

            date$ = date  :  call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "VSASHEET: " & str(cms2v$,,8)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 14% and fieldnr% = 1% then L10230
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
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
                  if keyhit%  = 14% then gosub print_vsa
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% >  5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130


        print_vsa
            temp_date$ = ship_date$  :  call "DATUNFMT" (temp_date$)
            call "VSASHTSB" (advice$, shipper$, qty_out, doc$,           ~
                             temp_date$, errormsg$)
            if errormsg$ <> " " then return
                return clear all
                goto   inputmode


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20170,         /* VSA                    */~
                              L20270,         /* Quantity Shipped       */~
                              L20420,         /* Shipper                */~
                              L20450,         /* Shipment Document      */~
                              L20550          /* Ship Date              */
            return

L20170: REM Def/Enable Vendor Service Advice       ADVICE$
            return

L20270: REM Def/Enable Quantity Shipped to Vendor  QTY_OUT$
            return

        REM Def/Enable Comment                     COMMENT$
            return

L20420: REM Def/Enable Shipper                     SHIPPER$
            return

L20450: REM Def/Enable Shipment Document Ref #     DOC$
            return

L20550: REM Def/Enable Ship Date                   SHIP_DATE$
            if ship_date$ = " " then ship_date$ = date$
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
         "Enter Advice #.  PF-14 to recall an existing log record.     ",~
         "Enter Quantity Shipped to Vendor.                            ",~
         "Enter Shipper's Name.                                        ",~
         "Enter Shipment Document Reference Number (BOL, for example). ",~
         "Enter actual/expected ship date.                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, trans_date$, trans_time$,  ~
                      doc$, qty_out$, shipper$, l$(),                    ~
                      advice$, on_file$, job_descr$, srce$,              ~
                      job$, rte_step$, wc_start$, wc_end$, job_part$,    ~
                      vendor$, activity$, po_line$, uom$, srce_user$,    ~
                      contract$, contract_line$, po$, ship_date$,        ~
                      vsa_qty$, act_descr$, job_part_descr$, uom_descr$
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

        load_advice_info
            get #2 using L35035, advice$, job$, rte_step$, wc_start$,     ~
                                wc_end$, vendor$, activity$,             ~
                                contract$, contract_line$, po$,          ~
                                po_line$, vsa_qty, uom$
            gosub calc_outins
            call "READ100" (#4, job$, f1%(4%))
            if f1%(4%) = 1% then get #4 using L30070, job_part$
L30070:          FMT POS(58), CH(25)
            return


        load_outin_info
            get #1 using L30115, trans_date$, trans_time$, trans_type$,   ~
                                qty_out, qty_in, advice$, job_part$,     ~
                                shipper$, doc$, srce$, srce_user$,       ~
                                comment$, ship_date$
L30115:         FMT POS(13), CH(6), CH(8), CH(1), 2*PD(14,4), CH(8),     ~
                    POS(75), CH(25), XX(4), CH(30), CH(20), CH(8), CH(3),~
                    CH(40), CH(6)
            if qty_out <> 0 then call "CONVERT" (qty_out, -0.4, qty_out$)
               qty_in  =  qty_in
            call "DATEFMT" (ship_date$)
	    call "DATEFMT" (trans_date$)
            call "READ100" (#2, advice$, f1%(2%))  /* VBKVSA */
            if f1%(2%) = 0% then                                         ~
                errormsg$ = "Unable to read Advice record."              ~
            else                                                         ~
                gosub load_advice_info
            return


        calc_outins
            plowkey$ = str(job$,,8) & str(rte_step$,,4) & all(hex(00))
            vsa_in, vsa_out = 0

L30205:     call "PLOWNEXT" (#1, plowkey$, 12%, f1%(1%))
            if f1%(1%) = 0% then return
                get #1 using L30220, out, in
L30220:              FMT POS(28), 2*PD(14,4)
                vsa_in  = vsa_in  + in
                vsa_out = vsa_out + out
                goto L30205


        concoct_display
            if on_file%  = 1% then                                       ~
                on_file$ = trans_date$ & "-" & str(trans_time$) & "; " & ~
                           srce$       & " " & srce_user$

            call "READ100" (#5, job_part$, f1%(5%))
            if f1%(5%) = 1% then get #5 using L30295, job_part_descr$
L30295:         FMT POS(26), CH(32)

            call "DATEFMT" (wc_start$) : call "DATEFMT" (wc_end$)

            plowkey$ = "WC ACTVTY" & activity$
            call "READ101" (#6, plowkey$, f1%(6%))
            if f1%(6%) = 1% then get #6 using L30350, act_descr$
L30350:         FMT POS(25), CH(30)

            plowkey$ = "UOM      " & uom$
            call "READ101" (#6, plowkey$, f1%(6%))
            if f1%(6%) = 1% then get #6 using L30350, uom_descr$

            call "READ100" (#4, job$, f1%(4%))
            if f1%(4%) = 1% then get #4 using L30395, job_descr$          ~
                            else job_descr$ = "Unavailable"
L30395:         FMT POS(9), CH(30)

            call "CONVERT" (vsa_qty, -0.4, str(vsa_qty$,,10))
            vsa_qty$ = "Advice Qty = " & vsa_qty$
            call "CONVERT" (vsa_out, -0.4, vsa_out$)

            l$(1) = "Job Number " & job$ & ", " & job_descr$ &           ~
                                     ";  Step " & rte_step$
            l$(2) = "Activity " & activity$ & ", " & act_descr$
            l$(3) = "Job Part " & job_part$ & ", " & job_part_descr$
            l$(4) = "WC Dates from " & wc_start$ & " to " & wc_end$ & "."
            l$(4) = l$(4) & "  " & vsa_out$ & " sent, " & vsa_in$ &      ~
                                              " received."
            if contract$ = " " then l$(5) = "No Contract"                ~
                               else l$(5) = "Contract " & contract$
            if contract_line$ <> " " then l$(5) = l$(5) & "-" &          ~
                                                           contract_line$
            if po$ = " " then l$(5) = l$(5) & ", No Purchase Order."     ~
                         else l$(5) = l$(5) & ", Purchase Order "        ~
                                            &   po$ & " - " & po_line$   ~
                                            & "."
            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35035: FMT                 /* FILE: VBKVSA                            */~
            XX(1),          /* Status Indicator                        */~
            XX(3),          /* Buyer/planner code                      */~
            CH(8),          /* Advice Number                           */~
            CH(8),          /* Job Number                              */~
            CH(4),          /* RTE Step To Identify A Route Line       */~
            XX(4),          /* Work Center ID                          */~
            CH(06),         /* WC Start Date                           */~
            CH(06),         /* WC End Date                             */~
            CH(9),          /* Vendor                                  */~
            CH(04),         /* Activity to be performed                */~
            CH(16),         /* Purchasing Contract ID                  */~
            CH(4),          /* Contract Line Number                    */~
            CH(16),         /* Purchase Order Number                   */~
            CH(03),         /* Purchase Order Line Number              */~
            XX(1),          /* Automatic Adjustment Flag               */~
            XX(40),         /* Comment                                 */~
            PD(14,4),       /* Quantity to Buy                         */~
            CH(4),          /* Unit of Measure                         */~
            PD(14,7),       /* Unit Price                              */~
            BI(4),          /* Standard Cost Bucket                    */~
            CH(25),         /* Vendor Part Number                      */~
            CH(4),          /* Transaction currency code               */~
            CH(114)         /* Unused filler area in record (reserved b*/

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if l$(1) <> " "  then lf$ = hex(ac) else lf$ = hex(8c)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40230,         /* VSA               */   ~
                                L40230,         /* Quantity Shipped  */   ~
                                L40210,         /* Shipper           */   ~
                                L40210,         /* Shipment Document */   ~
                                L40220          /* Ship Date         */
              goto L40250

L40210:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40230:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40250:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Vendor Service Sheets",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Vendor Service Advice",                      ~
               at (06,30), fac(lfac$( 1)), advice$              , ch(08),~
               at (06,49), fac(hex(8c)),   on_file$             , ch(30),~
                                                                         ~
               at (07,02), "Quantity Shipped to Vendor",                 ~
               at (07,30), fac(lfac$( 2)), qty_out$             , ch(10),~
               at (07,42), fac(hex(8c)), uom$                   , ch( 4),~
               at (07,49), fac(hex(8c)), uom_descr$             , ch(30),~
                                                                         ~
               at (08,02), "Shipper",                                    ~
               at (08,30), fac(lfac$( 3)), shipper$             , ch(30),~
                                                                         ~
               at (09,02), "Shipment Document Ref #",                    ~
               at (09,30), fac(lfac$( 4)), doc$                 , ch(20),~
                                                                         ~
               at (10,02), "Shipment Date",                              ~
               at (10,30), fac(lfac$( 5)), ship_date$           , ch(08),~
                                                                         ~
               at (14,02), fac(lf$    ),   l$                   , ch(79),~
               at (15,02), fac(hex(8c)),   l$(1)                , ch(79),~
               at (16,02), fac(hex(8c)),   l$(2)                , ch(79),~
               at (17,02), fac(hex(8c)),   l$(3)                , ch(79),~
               at (18,02), fac(hex(8c)),   l$(4)                , ch(79),~
               at (19,02), fac(hex(8c)),   l$(5)                , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40790
                  call "MANUAL" ("VSASHEET") : goto L40250

L40790:        if keyhit% <> 15 then L40820
                  call "PRNTSCRN" : goto L40250

L40820:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41020     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                       (14)Pick Existing" &        ~
                     " Log Record            (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0d0e0f1000)
            if fieldnr% = 1% then L40980
                str(pf$(3),20)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                                            str(pfkeys$,14,1) = hex(ff)
L40980:     if fieldnr% > 2% then L41000
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41000:     return

L41020: if fieldnr% > 0% then L41110  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                       (14)Print Vendor " &        ~
                     "Service Sheet          (16)Next  Entry "
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f1000)
            return
L41110:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50170,         /* VSA                    */~
                              L50910,         /* Quantity Shipped       */~
                              L51060,         /* Shipper                */~
                              L51090,         /* Shipment Document      */~
                              L51120          /* Ship Date              */
            return

L50170: REM Test for Vendor Service Advice        ADVICE$
            on_file%    =  0%
            if keyhit% <> 14% then L50480

*        Search VSAOUTIN for existing shipment record...
            plow_hdr$(1%) = "  Job Nbr   Step  Advice #  Vendor #   Ac" &~
                            "ty     Qty Out      Qty In  ShipDate     "

            plow_map( 1%) = 001.08 : plow_map( 2%) = 0001     /* Job #  */
            plow_map( 3%) = 009.04 : plow_map( 4%) = 0011     /* Step   */
            plow_map( 5%) = 044.08 : plow_map( 6%) = 0017     /* Advice */
            plow_map( 7%) = 052.09 : plow_map( 8%) = 0027     /* Vendor */
            plow_map( 9%) = 100.04 : plow_map(10%) = 0038     /* Actvty */
            plow_map(11%) = 028.08 : plow_map(12%) = 0044.104 /* QtyOut */
            plow_map(13%) = 036.08 : plow_map(14%) = 0056.104 /* Qty In */
            plow_map(15%) = 205.061: plow_map(16%) = 0068     /* ShpDte */

            plow_inex(1%) = 27.010 : plow_inex$(1%) = "S"

            errormsg$ = hex(06) & "Select existing log record."
            plowkey$  = " "
            call "PLOWCODE" (#1, plowkey$, errormsg$, 9000%, 0.01,       ~
                             on_file%, plow_hdr$(), 0.0, 0.0,            ~
                             plow_inex(), plow_inex$(),                  ~
                             "d", " ", #07, plow_map())
            errormsg$ = " "
            if on_file% = 1% then L50430
                errormsg$ = hex(00)
                return
L50430:     gosub load_outin_info
            gosub concoct_display
            if errormsg$ <> " " then return
                return clear all
                goto editpg1

L50480
*        Get VBKVSA record to provide most information...
            convert advice$ to advice%, data goto L50540
            convert advice% to advice$, pic (00000000)
            call "READ100" (#2, advice$, f1%(2%))
            if f1%(2%) = 1% then L50750

L50540:     plow_hdr$(1%) = "  Job Nbr   Acty  Advice #  Vendor #   St" &~
                            "ep  Purchase Order -Line  Total Qty      "

            plow_map( 1%) = 013.08 : plow_map( 2%) = 0001     /* Job #  */
            plow_map( 3%) = 050.04 : plow_map( 4%) = 0011     /* Actvty */
            plow_map( 5%) = 005.08 : plow_map( 6%) = 0017     /* Advice */
            plow_map( 7%) = 041.09 : plow_map( 8%) = 0027     /* Vendor */
            plow_map( 9%) = 021.04 : plow_map(10%) = 0038     /* Step   */
            plow_map(11%) = 074.16 : plow_map(12%) = 0044     /* PO     */
            plow_map(13%) = 090.03 : plow_map(14%) = 0061     /* PO Line*/
            plow_map(15%) = 134.08 : plow_map(16%) = 0067.104 /* PO Line*/

            mat plow_inex = zer
                plow_inex$() = " "

            errormsg$ = hex(06) & "Select advice to enter log record for."
            plowkey$  = " "
            call "PLOWCODE" (#2, plowkey$, errormsg$, 9000%, 3.01,       ~
                             f1%(2%), plow_hdr$(), 0.0, 0.0, plow_inex(),~
                             plow_inex$(), "d", " ", #07, plow_map())
            errormsg$ = " "
            if f1%(2%) = 1% then L50750
                if advice$ = " " then                                    ~
                     errormsg$ = hex(00)                                 ~
                else                                                     ~
                     errormsg$ = "Vendor Service Advice must be on file."
                return

L50750:     gosub load_advice_info
            gosub concoct_display
            return

L50910: REM Test for Quantity Shipped to Vendor   QTY_OUT$
            call "NUMTEST" (qty_out$, 0.01, 9e7, errormsg$, 0.4, qty_out)
            return

L51060: REM Test for Shipper                      SHIPPER$
            return

L51090: REM Test for Shipment Document Ref #      DOC$
            return

L51120: REM Test for Ship Date                    SHIP_DATE$
            if ship_date$ <> " " then                                    ~
                call "DATEOK" (ship_date$, f1%(07%), errormsg$)
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
            call "SHOSTAT" ("One Moment Please")

            end
