
        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V   SSS    AAA    SSS   H   H  PPPP    SSS   BBBB    *~
            *  V   V  S      A   A  S      H   H  P   P  S      B   B   *~
            *  V   V   SSS   AAAAA   SSS   HHHHH  PPPP    SSS   BBBB    *~
            *   V V       S  A   A      S  H   H  P          S  B   B   *~
            *    V     SSS   A   A   SSS   H   H  P       SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VSASHPSB - Manages records in the file VSAOUTIN, the      *~
            *            Vendor Shipment Log.                           *~
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
            * 07/11/94 ! Original                                 ! ERN *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "VSASHPSB" (#1, #2, #3, #4, #5, #6)


        dim                                                              ~
            activity$4,                  /* WC Activity Code           */~
            act_descr$30,                /* WC Activity Code Descr     */~
            advice$8,                    /* Vendor Service Advice      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            comment$40,                  /* Comment                    */~
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
            on_file$32,                  /* Descriptor Line            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plow_hdr$(2)80,              /* PLOWCODE sh--              */~
            plow_inex(2), plow_inex$(2)2,/*                            */~
            plow_map(20),                /*                            */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po$16, po_line$3,            /* Purchase Order             */~
            qty_in$10,                   /* Quantity Rec'd from Vendor */~
            qty_out$10,                  /* Quantity Shipped to Vendor */~
            rte_step$4,                  /* Step on the Route          */~
            ship_date$8,                 /* Date Shipped               */~
            shipper$30,                  /* Shipper                    */~
            srce$8,                      /* Source Program             */~
            srce_user$3,                 /* Original User ID           */~
            temp_date$8,                 /* Temporary Date (EZ Sal)    */~
            trans_date$6,                /* Transaction Date           */~
            trans_descr$30,              /* Trans Type Descriptor      */~
            trans_time$8,                /* Transaction Time Stamp     */~
            trans_type$1,                /* Transaction Type           */~
            uom$4, uom_descr$30,         /* Unit of Measure            */~
            userid$3,                    /* Current User Id            */~
            vendor$9,                    /* The Vendor Code            */~
            vsa_qty$30,                  /* Advice Qty formatted       */~
            vsa_out$10, vsa_in$10,       /* Total ins and outs         */~
            wc_start$8, wc_end$8         /* WC Start and End Dates     */

        dim f1%(64)                      /* = 1 if READ was successful */


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
            * #44 ! DUMMY    ! For PLOWCODE                             *~
            *************************************************************~

            select #44, "LARRY", varc, indexed, recsize = 69,            ~
                                 keypos = 1, keylen =  2


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)

            date$ = date  :  call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "VSASHPSB: " & str(cms2v$,,8)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  8%
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
            if fieldnr% < 2% or fieldnr% >  8% then editpg1
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
            if trans_type$ = "S" and qty_out > 0 then L11320
                errormsg$  = "Can only print for Shipment entries with" &~
                             " positive quantities."
                return
L11320:     temp_date$ = ship_date$  :  call "DATUNFMT" (temp_date$)
            call "VSASHTSB" (advice$, shipper$, qty_out, doc$,           ~
                             temp_date$, errormsg$)
            if errormsg$ = " " then                                      ~
                errormsg$ = "Service Sheet has been printed."
            return


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
            on fieldnr% gosub L20170,         /* VSA                    */~
                              L20200,         /* Transaction Type       */~
                              L20270,         /* Quantity Shipped       */~
                              L20350,         /* Quantity Rec'd         */~
                              L20390,         /* Comment                */~
                              L20420,         /* Shipper                */~
                              L20450,         /* Shipment Document      */~
                              L20550          /* Ship Date              */
            return

L20170: REM Def/Enable Vendor Service Advice       ADVICE$
            return

L20200: REM Def/Enable Transaction Type            TRANS_TYPE$
            if on_file%    = 0%  then L20240
                enabled%   = 0%
                return
L20240:     if trans_type$ = " " then trans_type$ = "S"
            return

L20270: REM Def/Enable Quantity Shipped to Vendor  QTY_OUT$
            enabled%     = 0%
            if on_file%  = 1% or trans_type$ = "R" then return
                enabled% = 1%
                if trans_type$ <> "S" or qty_out$ <> " " then return
                    qty_open = vsa_qty - max(vsa_out, vsa_in)
                    call "CONVERT" (qty_open, -0.4, qty_out$)
                    if qty_open <= 0 then qty_out$ = " "
                    return

L20350: REM Def/Enable Quantity Rec'd from Vendor  QTY_IN$
            enabled% = 0%
            if on_file%  = 1% or trans_type$ = "S" then return
                enabled% = 1%
                if trans_type$ <> "R" or qty_in$ <> " " then return
                     vsa_ven = max(vsa_out - vsa_in, 0)
                     call "CONVERT" (vsa_ven, -0.4, qty_in$)
                     if vsa_ven <= 0 then qty_in$ = " "
                     return

L20390: REM Def/Enable Comment                     COMMENT$
            return

L20420: REM Def/Enable Shipper                     SHIPPER$
            return

L20450: REM Def/Enable Shipment Document Ref #     DOC$
            return

L20550: REM Def/Enable Ship Date                   SHIP_DATE$
            if ship_date$ = " " or ship_date$ = blankdate$ then ~
                   ship_date$ = date$
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
         "Enter Transaction Type: 'S'hipment, 'R'eceipt, 'A'djustment. ",~
         "Enter Quantity Shipped to Vendor.                            ",~
         "Enter Quantity Received from Vendor.                         ",~
         "Enter Comment.                                               ",~
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
                      comment$, doc$, qty_in$, qty_out$, shipper$, l$(), ~
                      trans_type$, advice$, on_file$, job_descr$, srce$, ~
                      job$, rte_step$, wc_start$, wc_end$, job_part$,    ~
                      vendor$, activity$, po_line$, uom$, srce_user$,    ~
                      contract$, contract_line$, po$, ship_date$,        ~
                      vsa_qty$, trans_descr$, act_descr$,                ~
                      job_part_descr$, uom_descr$
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
            if qty_in  <> 0 then call "CONVERT" (qty_in , -0.4, qty_in$ )
            call "DATEFMT" (ship_date$)
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
            on_file$ = trans_date$
            call "DATEFMT" (on_file$)
            if on_file%  = 1% then                                       ~
                on_file$ = on_file$ & "-" & str(trans_time$) & "; " & ~
                           srce$       & " " & srce_user$                ~
            else                                                         ~
                on_file$ = "*** New Log Record ***"

            call "READ100" (#5, job_part$, f1%(5%))
            if f1%(5%) = 1% then get #5 using L30295, job_part_descr$
L30295:         FMT POS(26), CH(32)

            if trans_type$ = "A" then trans_descr$ = "Adjustment"
            if trans_type$ = "S" then trans_descr$ = "Shipment  "
            if trans_type$ = "R" then trans_descr$ = "Receipt   "

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
            call "CONVERT" (vsa_in , -0.4, vsa_in$ )

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
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Saving Log Record")
            qty_out, qty_in = 0
            call "DATUNFMT" (ship_date$)
            if qty_out$ <> " " then convert qty_out$ to qty_out
            if qty_in$  <> " " then convert qty_in$  to qty_in
            if on_file% = 1% then L31170
              srce$       = "VBKSHIP"
              srce_user$  = userid$
              trans_date$ = date
              trans_time$ = time
              goto L31200
L31170:     plowkey$ = str(job$,,8) & str(rte_step$,,4) &                ~
                       str(trans_date$,,6) & trans_time$
            call "READ101" (#1, plowkey$, f1%(1%))
L31200:     put   #1 using L31260, job$, rte_step$,                       ~
                                  trans_date$, trans_time$, trans_type$, ~
                                  qty_out, qty_in, advice$, vendor$,     ~
                                  trans_date$, trans_time$, job_part$,   ~
                                  activity$, shipper$, doc$, srce$,      ~
                                  srce_user$, comment$, ship_date$, " "
L31260:         FMT CH(8), CH(4), CH(6), CH(8), CH(1), 2*PD(14,4), CH(8),~
                    CH(9), CH(6), CH(8), CH(25), CH(4), CH(30), CH(20),  ~
                    CH(8), CH(3), CH(40), CH(06), CH(106)

            if on_file% = 0% then L31350
                rewrite #1
                goto L31370
L31330:     put #1 using L31340, time
L31340:         FMT POS(19), CH(8)
L31350:     write #1, eod goto L31330

L31370:     if trans_type$ <> "S" or on_file% = 1% or qty_out <=0        ~
                                                              then return
L31390:         print% = 0%
                call "ASKUSER" (print%, "Print Service Sheet?",          ~
                          "Press PF-14 to Print Vendor Service Sheet",   ~
                          "- OR -", "Press RETURN to continue...")
                if print%  =  0% then return
                  if print% <> 14% then L31390
                     temp_date$ = ship_date$
                     call "VSASHTSB" (advice$, shipper$, qty_out, doc$,  ~
                                      temp_date$, errormsg$)
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
              if on_file% = 1% then lfac$(1), lfac$(2), lfac$(3),        ~
                                                        lfac$(4) = hex(8c)
              on fieldnr% gosub L40230,         /* VSA               */   ~
                                L40220,         /* Transaction Type  */   ~
                                L40230,         /* Quantity Shipped  */   ~
                                L40230,         /* Quantity Rec'd    */   ~
                                L40210,         /* Comment           */   ~
                                L40210,         /* Shipper           */   ~
                                L40210,         /* Shipment Document */   ~
                                L40220          /* Ship Date         */
              goto L40250

L40210:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40230:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40250:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Vendor Shipment Log",                          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Vendor Service Advice",                      ~
               at (06,30), fac(lfac$( 1)), advice$              , ch(08),~
               at (06,47), fac(hex(8c)),   on_file$             , ch(32),~
                                                                         ~
               at (07,02), "Transaction Type",                           ~
               at (07,30), fac(lfac$( 2)), trans_type$          , ch(01),~
               at (07,49), fac(hex(8c)),   trans_descr$         , ch(32),~
                                                                         ~
               at (08,02), "Quantity Shipped to Vendor",                 ~
               at (08,30), fac(lfac$( 3)), qty_out$             , ch(10),~
               at (08,42), fac(hex(8c)), uom$                   , ch( 4),~
               at (08,49), fac(hex(8c)), uom_descr$             , ch(30),~
                                                                         ~
               at (09,02), "Quantity Rec'd from Vendor",                 ~
               at (09,30), fac(lfac$( 4)), qty_in$              , ch(10),~
               at (09,49), fac(hex(8c)), vsa_qty$,                       ~
                                                                         ~
               at (10,02), "Comment",                                    ~
               at (10,30), fac(lfac$( 5)), comment$             , ch(40),~
                                                                         ~
               at (11,02), "Shipper",                                    ~
               at (11,30), fac(lfac$( 6)), shipper$             , ch(30),~
                                                                         ~
               at (12,02), "Shipment Document Ref #",                    ~
               at (12,30), fac(lfac$( 7)), doc$                 , ch(20),~
                                                                         ~
               at (13,02), "Shipment Date",                              ~
               at (13,30), fac(lfac$( 8)), ship_date$           , ch(08),~
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
                  call "MANUAL" ("VSASHPSB") : goto L40250

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
                     "Service Sheet          (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f1000)
            if trans_type$ = "S" then L41100
                str(pf$(3),22,32) = " "
                str(pfkeys$,14,1) = hex(ff)
L41100:     return
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
                              L50790,         /* Transaction Type       */~
                              L50910,         /* Quantity Shipped       */~
                              L50970,         /* Quantity Rec'd         */~
                              L51030,         /* Comment                */~
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

            errormsg$ = hex(06) & "Select existing log record."
            plowkey$  = " "
            call "PLOWCODE" (#1, plowkey$, errormsg$, 9000%, 0.01,       ~
                             on_file%, plow_hdr$(), 0.0, 0.0,            ~
                             plow_inex(), plow_inex$(),                  ~
                             "D", " ", #44, plow_map())
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
*        Gonna add a new one.  Base on VBKVSA record...
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

            errormsg$ = hex(06) & "Select advice to enter log record for."
            plowkey$  = " "
            call "PLOWCODE" (#2, plowkey$, errormsg$, 9000%, 3.01,       ~
                             f1%(2%), plow_hdr$(), 0.0, 0.0, plow_inex(),~
                             plow_inex$(), "d", " ", #44, plow_map())
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

L50790: REM Test for Transaction Type             TRANS_TYPE$
            if pos ("SRA" = trans_type$) > 0% then L50821
                errormsg$ = "Enter 'S' for Shipment, 'R' for Receipt, " &~
                            "or 'A' for Adjustment."
L50821:     if trans_type$ = "A" then trans_descr$ = "Adjustment"
            if trans_type$ = "S" then trans_descr$ = "Shipment  "
            if trans_type$ = "R" then trans_descr$ = "Receipt   "
            if trans_type$ = "A" then return
                if trans_type$ = "S" then L50881
                     if qty_out$ = " " then return
                          if qty_in$ = " " then qty_in$ = qty_out$
                          qty_out$ = " "
                          return
L50881:         if qty_in$ = " " then return
                     if qty_out$ = " " then qty_out$ = qty_in$
                     qty_in$  = " "
                     return

L50910: REM Test for Quantity Shipped to Vendor   QTY_OUT$
            call "NUMTEST" (qty_out$, -9e7, 9e7, errormsg$, 0.4, qty_out)
            if errormsg$ <> " " then return
                if qty_out <> 0 or trans_type$ <> "S" then return
                     qty_out$ = " "
                     errormsg$ = "Shipped Quantity required for " &      ~
                                 "Shipment Transaction Type."
                     return

L50970: REM Test for Quantity Rec'd from Vendor   QTY_IN$
            call "NUMTEST" (qty_in$, -9e7, 9e7, errormsg$, 0.4, qty_in)
            if errormsg$ <> " " then return
                if qty_in <> 0 or trans_type$ <> "R" then return
                     qty_in$ = " "
                     errormsg$ = "Quantity Received required for " &     ~
                                 "Receipt Transaction Type."
                     return

L51030: REM Test for Comment                      COMMENT$
            return

L51060: REM Test for Shipper                      SHIPPER$
            return

L51090: REM Test for Shipment Document Ref #      DOC$
            return

L51120: REM Test for Ship Date                    SHIP_DATE$
            if ship_date$ = " " or ship_date$ = blankdate$ then return
                call "DATEOK" (ship_date$, f1%(44%), errormsg$)
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
