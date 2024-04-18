        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  RRRR   EEEEE  TTTTT  IIIII  N   N   *~
            *  H   H  NN  N  Y   Y  R   R  E        T      I    NN  N   *~
            *  HHHHH  N N N   YYY   RRRR   EEEE     T      I    N N N   *~
            *  H   H  N  NN    Y    R   R  E        T      I    N  NN   *~
            *  H   H  N   N    Y    R   R  EEEEE    T    IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYRETIN - Allows returning of inventory that was with-   *~
            *            drawn either by an invoice or via HNYWDWAL.    *~
            *            Necessary to (1) maintain lot tracking links   *~
            *            and/or (2) put serial numbers back into stock. *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/26/87 ! Original                                 ! ERN *~
            * 05/13/87 ! File format changes; # costs from 3 to 12! JIM *~
            * 08/13/90 ! Basic 4.3 & SSL Compatibility            ! KAB *~
            * 06/21/91 ! Added PF8 access to HNYLCSUB permitting  ! MLJ *~
            *          !  Location control.                       !     *~
            * 01/21/92 ! Now captures USETYPE$ for HNYRETJN/Usage.! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            x_$100,                      /* Temporary Variable         */~
            costs(12), costs$10, cost$96,/* Inventory Costs            */~
            cr_acct$12, cr_acct_descr$30,/* Credit Account             */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9, cusname$30,       /* Ship-to Customer Number    */~
            datetime$7,                  /* Date-Time Stamp            */~
            descr$32,                    /* Description / Free Text    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            from_str$3, from_lot$6,      /* From Store / Lot           */~
            from_str_descr$30,           /*                            */~
            hny_acct$12,                 /* Inventory Account          */~
            hny_acct_descr$30,           /*                            */~
            hny_date$8, hny_datu$6,      /* HNY Post Date              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invmsg$30,                   /* Invoice Number Message     */~
            invnr$8,                     /* Invoice Number             */~
            last_type$1, last_cuscode$9, /* Save values to supply      */~
            last_invnr$8, last_descr$32, /*   defaults.                */~
            last_from_str$3,             /*                            */~
            last_to_str$3,               /*                            */~
            last_hny_acct$12,            /*                            */~
            last_cr_acct$12,             /*                            */~
            lfac$(20)1, lfac2$(20)1,     /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lots$(30)6, lotqtys(30),     /* Invoice Lots and Qtys      */~
            lotmsg$30,                   /* Lot on file message        */~
            part$25, part_descr$32,      /* Part Number                */~
            pf4$18, pf5$18, pf16$16,     /* PF Key Screen Literals     */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            qty$10,                      /* Quantity Returned          */~
            seqnrs$(100)3,               /* Invoice Seq Numbers        */~
            sn_loc$30,                   /* Location code              */~
            sn_source$1,                 /* Source per TYPE$           */~
            sn_trankey$40,               /* Transaction Key            */~
            to_str$3, to_lot$6,          /* To Store / Lot             */~
            to_str_descr$30,             /*                            */~
            type$1, type_descr$30,       /* Type of Withdrawal         */~
            userid$3                     /* Current User Id            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        dim usetype$5, usedsnb$1, useprompt$20, usetype_descr$30

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
            * # 1 ! USERINFO ! Users Default Information File           *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! CUSTOMER ! Customer Master File                     *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 6 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * # 7 ! GLMAIN   ! General Ledger CHart Of Accounts File.   *~
            * # 8 ! STORNAME ! Store Information File                   *~
            * # 9 ! HNYRETTF ! Inventory Returns Transaction File       *~
            * #10 ! LOTMVMNT ! Lot Movement File                        *~
            * #11 ! ARIMASTR ! Invoice Master File                      *~
            * #12 ! ARILINES ! Invoice Master File- Line Items          *~
            * #13 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #14 ! LOCATION ! Location Master File                     *~
            * #20 ! SERMASTR ! Serial Number Master File                *~
            * #21 ! SERWORK  ! Serial Number Work File                  *~
            * #22 ! SERTIF   ! Serial Number Transaction Image File     *~
            * #32 ! PFMUTYPE ! Usage Type Master File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =   3

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select # 3, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select # 4, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select # 6, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select # 7, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select # 8, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select # 9, "HNYRETTF",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen = 13

            select #10, "LOTMVMNT",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  88,                     ~
                        alt key  1, keypos =   45, keylen =  88

            select #11, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17

            select #12, "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #13,  "HNYLOCNS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 443, keylen = 42,     ~
                                   key 2, keypos = 485, keylen = 42,     ~
                                   key 3, keypos = 527, keylen = 42,     ~
                                   key 4, keypos = 590, keylen = 42      ~

            select #14,  "LOCATION",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 11,                        ~
                         alternate key 1, keypos = 4, keylen = 11        ~

            select #20, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #21, "SERWORK",                                       ~
                        varc,     indexed,  recsize =  48,               ~
                        keypos = 1, keylen = 23

            select #22, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #32, "PFMUTYPE",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =  1,    keylen = 5

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7),   0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8),   0%, rslt$( 8))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 100%, rslt$( 9))
            call "OPENCHCK" (#10, fs%(10), f2%(10),   0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12),   0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

*        Make sure an Update is not in process for this user
            plowkey$ = str(userid$) & "x"
            call "READ100" (#9, plowkey$, f1%(9))
            if f1%(9) = 1% then exit_program else L09160

L09160
*        Retrieve Inventory Date
            call "READ100" (#1, userid$, f1%(1))
            if f1%(1) = 1% then L09230
                call "ASKUSER" (0%, "***ERROR***",                       ~
                     "Unable to locate your Inventory Posting Date!",    ~
                     " ", "Press RETURN to acknowlege and Exit.")
                       goto exit_program
L09230:     get #1, using L09240, hny_date$
L09240:         FMT XX(27), CH(6)
            call "WHICHMON" (#2, hny_date$, thismonth%)
            if thismonth% > 0% and thismonth% < 4% then L09330
                call "ASKUSER" (0%, "INVALID POSTING DATE",              ~
                     "Your Inventory Posting Date is not within the " &  ~
                     "posting window.",                                  ~
                     "Please change your posting date & try again.",     ~
                     "Press (RETURN) to acknowlege and Exit.")
                      goto exit_program
L09330:     hny_datu$ = hny_date$  :  call "DATEFMT" (hny_date$)

*        And set up some miscellaneous variables
            lot_len% = 6%
            str(line2$,62) = "HNYRETIN: " & str(cms2v$,,8)

*        See if we need to consider the Usage Sub-Module.
            hi% = 10%
            call "READ100" (#02, "SWITCHS.HNY", f1%(2))    /* SYSFILE2 */
                if f1%(2) = 0% then goto L10000             /* No Usage */
            get #02 using L09430, usedsnb$
L09430:         FMT POS(110), CH(1)
            if usedsnb$ = "N" then goto L10000              /* No Usage */
*        OK, Set program to capture Usage Type.
                useprompt$ = "Usage Type"
                call "OPENCHCK" (#32, fs%(32), f2%(32), 0%, rslt$(32))
                hi% = 11%

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$  = " "
            pf5$  = "(5)Clear Defaults"
            pf16$ = "(16)Exit Program"
            gosub L29000

            for fieldnr% = 1 to hi%
                if fieldnr% > 6% then pf4$  = "(4)Previous Field"        ~
                                 else pf4$  = " "
                if fieldnr% > 1% then pf5$  = " "
                if fieldnr% > 1% then pf16$ = " "
L10170:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10310
L10190:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 or fieldnr% < 6% then L10270
L10220:                  fieldnr% = max(6%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10190
                         if fieldnr% = 6% then L10170
                         goto L10220
L10270:               if keyhit% =  5 and fieldnr% = 1 then              ~
                                                    gosub clear_defaults
                      if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10190
L10310:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10190
            next fieldnr%
            goto L11000


        clear_defaults
            init(" ") last_type$, last_cuscode$, last_invnr$,            ~
                      last_descr$, last_from_str$, last_to_str$,         ~
                      last_hny_acct$, last_cr_acct$, type$, usetype$,    ~
                      usetype_descr$
            return


L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$, pf5$ = " " : pf16$ = "(16)Save Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%

            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg1
L11160:     fieldnr% = cursor%(1) - 7%
            fieldnr% = max(6%, min(fieldnr%, hi%))
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11220:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
                  lastfieldnr% = fieldnr%
            goto L11160

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
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20210,         /* Type of Withdrawal */    ~
                              L20270,         /* Ship-to Customer   */    ~
                              L20330,         /* Invoice Number     */    ~
                              L20390,         /* Part Number        */    ~
                              L20430,         /* From Store / Lot   */    ~
                              L20560,         /* Qty, To Store/Lot  */    ~
                              L20630,         /* Description        */    ~
                              L20680,         /* Inventory Costs    */    ~
                              L20760,         /* Inventory Account  */    ~
                              L20820,         /* Credit Account     */    ~
                              L20870          /* Usage Type         */
            return

L20210
*        Def/Enable TYPE OF WITHDRAWAL          TYPE$
            inpmessage$ = "Enter 'I' for Invoice, 'W' for Inventory" &   ~
                          " Withdrawal."
            type$ = last_type$
            return

L20270
*        Def/Enable SHIP-TO CUSTOMER NUMBER     CUSCODE$
            inpmessage$ = "Enter Customer that inventory was sent to."
            if type$ = "I" then cuscode$ = last_cuscode$ else            ~
                                enabled% = 0%
            return

L20330
*        Def/Enable INVOICE NUMBER              INVNR$
            inpmessage$ = "Enter Invoice ('?' to see invoices on file)."
            if type$ = "I" then invnr$   = last_invnr$   else            ~
                                enabled% = 0%
            return

L20390
*        Def/Enable PART NUMBER                 PART$
            inpmessage$ = "Enter Part Number that was returned."
            return

L20430
*        Def/Enable FROM STORE / LOT            FROM_STR$
            inpmessage$ = "Enter Store that Part was taken from" &       ~
                          " (if known)."
            if lot_enable% > 0% then inpmessage$ = inpmessage$ &         ~
                                     "  Lot # may also be entered."
            from_str$ = last_from_str$
            if invoice% = 0% then return
                get #11 using L20510, from_str$
L20510:              FMT POS(870), CH(3)
                if lot_enable% = 0% then enabled% = 0% else              ~
                                    inpmessage$ = "Enter Lot number."
            return

L20560
*        Def/Enable QUANTITY RETURNED, TO STORE & LOT$
            inpmessage$ = "Enter the Quantity returned and where to" &   ~
                          " put it."
            if to_str$ = " " then to_str$ = last_to_str$
            if to_str$ = " " then to_str$ = from_str$
            return

L20630
*        Def/Enable DESCRIPTION / Free Text     DESCR$
            inpmessage$ = "Enter Description regarding this return."
            if descr$ = " " then descr$ = last_descr$
            return

L20680
*        Def/Enable INVENTORY COSTS             COSTS$
            return

L20760
*        Def/Enable INVENTORY ACCOUNT           HNY_ACCT$
            inpmessage$ = "Please supply Inventory Account Number."
            if hny_acct$ = " " then hny_acct$ = last_hny_acct$
            return

L20820
*        Def/Enable CREDIT ACCOUNT              CR_ACCT$
            inpmessage$ = "Please supply Account Number to Credit."
            if cr_acct$ = " " then cr_acct$ = last_cr_acct$
            return

L20870
*        Def/Enable Usage Type                  USETYPE$
            if usedsnb$ <> " " and usedsnb$ <> "N" then goto L20883
                enabled% = 0%
                return
L20883:     if usetype$ = " " then usetype$ = "PROD"
            inpmessage$ = "Enter Usage Type, partial, or '?' for a list."
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, type$, cuscode$, cusname$, ~
                      invnr$, part$, part_descr$, from_str$, from_lot$,  ~
                      from_str_descr$, qty$, to_str$, to_lot$, invmsg$,  ~
                      to_str_descr$, descr$, costs$, hny_acct$,          ~
                      hny_acct_descr$, cr_acct$, cr_acct_descr$, lotmsg$,~
                      type_descr$, sn_source$, usetype$, usetype_descr$
            init (hex(00)) cost$
            cost, invoice% = 0
            mat costs = zer
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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
                call "SERSTOVR" (1%, "2", sn_source$, #20, #21)
                goto   inputmode

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "GLUNFMT" (hny_acct$)
            call "GLUNFMT" (cr_acct$)
            call "GETDTTM" addr(datetime$)
            call "PACKZERO" (costs(), cost$)
            write #9 using L35040, userid$, type$, datetime$, cuscode$,   ~
                                  invnr$, part$, from_str$, from_lot$,   ~
                                  qty, to_str$, to_lot$, descr$,         ~
                                  hny_acct$, cr_acct$, cost$, usetype$,  ~
                                  " "

            sn_trankey$ = str(to_str$) & to_lot$
            call "SERSAVE" (1%, "IR", sn_trankey$, 5%, part$, userid$,   ~
                            "2", sn_source$, 1%, #2, #22, #20, #21)
            return


        locations
            if qty$ <> " " then L32060
                qty = 0
                goto L32080

L32060:     convert qty$ to qty

L32080:     call "HNYLCSUB" (part$,                                      ~
                             to_str$,                                    ~
                             to_lot$,                                    ~
                             qty,                                        ~
                             3%,       /*   ACTION = ADD               */~
                             #2,       /*   SYSFILE2                   */~
                             #8,       /*   STORNAME                   */~
                             #1,       /*   USERINFO                   */~
                             #4,       /*   HNYMASTR                   */~
                             #13,      /*   HNYLOCNS                   */~
                             #6,       /*   HNYQUAN                    */~
                             #14)      /*   LOCATION                   */
            return
        REM *************************************************************~
            *             R E C O R D   L A Y O U T S                   *~
            *************************************************************

L35040: FMT                              /* #9- HNYRETTF               */~
            CH( 3),                      /* User ID                    */~
            CH( 1),                      /* Type of Withdrawal         */~
            CH( 9),                      /* Date-Time Stamp            */~
            CH( 9),                      /* Ship-to Customer Code      */~
            CH( 8),                      /* Invoice Number             */~
            CH(25),                      /* Part Code                  */~
            CH( 3),                      /* From Store                 */~
            CH( 6),                      /*      Lot                   */~
            PD(14,4),                    /* Quantity Returned          */~
            CH( 3),                      /* To Store                   */~
            CH( 6),                      /*    Lot                     */~
            CH(32),                      /* Description / Free Text    */~
            CH( 9),                      /* Inventory Assets Account   */~
            CH( 9),                      /* Credit Account             */~
            CH(96),                      /* Inventory costs            */~
            CH(5),                       /* Usage Type                 */~
            CH(24)                       /* Filler                     */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "HNYRETIN: " & str(cms2v$,,8%)
              init(hex(8c)) lfac$(), lfac2$()
              if fieldnr% = 0% then init(hex(86)) str(lfac$(),6)
              on fieldnr% gosub L40270,         /* Type of Withdrawal*/   ~
                                L40270,         /* Ship-to Customer  */   ~
                                L40270,         /* Invoice Number    */   ~
                                L40270,         /* Part Number       */   ~
                                L40270,         /* From Store / Lot  */   ~
                                L40270,         /* Qty, To Store/Lot */   ~
                                L40260,         /* Description       */   ~
                                L40252,         /* Inventory Costs   */   ~
                                L40270,         /* Inventory Account */   ~
                                L40270,         /* Credit Account    */   ~
                                L40270          /* Usage Type        */

              if fieldnr% <> 5% then L40230
                if lot_enable% > 0% then lfac2$(5) = hex(81)
                if invoice%    = 1% then lfac$ (5) = hex(8c)
L40230:       if fieldnr% <> 6% then L40250
                if lot_enable% > 0% then lfac2$(6) = hex(81)
L40250:       if fieldnr% = 8% then return /* No screen for costs */
              goto L40300
L40252: REM Capture & distribute Inventory costs via HNYCDIST
            call "HNYCDIST" (" ", part$, part_descr$,                    ~
                "HNYRETIN: Inventory Returns: Distribute Inventory cost",~
                #2, cost$, costs$, cost):call "CONVERT" (cost,-2.4,costs$)
            get cost$ using L40257, costs()
L40257:         FMT  12*PD(14,4)
            return

L40260:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40270:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40300:     accept                                                       ~
               at (01,02), "Enter Returned Inventory",                   ~
               at (01,66), " Post:",                                     ~
               at (01,73), fac(hex(8c)), hny_date$              , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Return Withdrawal Type",                     ~
               at (06,30), fac(lfac$( 1)), type$                , ch(01),~
               at (06,49), fac(hex(8c))  , type_descr$          , ch(30),~
                                                                         ~
               at (07,02), "Ship-to Customer Number",                    ~
               at (07,30), fac(lfac$( 2)), cuscode$             , ch(09),~
               at (07,49), fac(hex(8c))  , cusname$             , ch(30),~
                                                                         ~
               at (08,02), "Invoice Number",                             ~
               at (08,30), fac(lfac$( 3)), invnr$               , ch(08),~
               at (08,49), fac(hex(8c))  , invmsg$              , ch(30),~
                                                                         ~
               at (09,02), "Part Number",                                ~
               at (09,30), fac(lfac$( 4)), part$                , ch(25),~
               at (10,02), "     Description",                           ~
               at (10,30), fac(hex(8c)),   part_descr$          , ch(32),~
                                                                         ~
               at (11,02), "From Store / Lot",                           ~
               at (11,30), fac(lfac$( 5)), from_str$            , ch(03),~
               at (11,34), fac(lfac2$(5)), str(from_lot$,,lot_len%),     ~
               at (11,49), fac(hex(8c)),   from_str_descr$      , ch(30),~
                                                                         ~
               at (12,02), "Quantity Returned",                          ~
               at (12,30), fac(hex(80))  , qty$                 , ch(10),~
               at (12,30), fac(lfac$( 6)), qty$                 , ch(10),~
               at (12,49), fac(hex(8c)),   lotmsg$              , ch(30),~
               at (13,02), "   To Store / Lot",                          ~
               at (13,30), fac(lfac$( 6)), to_str$              , ch(03),~
               at (13,34), fac(lfac2$(6)), str(to_lot$,,lot_len%),       ~
               at (13,49), fac(hex(8c)),   to_str_descr$        , ch(30),~
                                                                         ~
               at (14,02), "Description / Free Text",                    ~
               at (14,30), fac(lfac$( 7)), descr$               , ch(32),~
                                                                         ~
               at (15,02), "Inventory Cost",                             ~
               at (15,30), fac(lfac$( 8)), costs$               , ch(10),~
                                                                         ~
               at (16,02), "Inventory Account",                          ~
               at (16,30), fac(lfac$( 9)), hny_acct$            , ch(12),~
               at (16,49), fac(hex(8c))  , hny_acct_descr$      , ch(30),~
                                                                         ~
               at (17,02), "Credit Account",                             ~
               at (17,30), fac(lfac$(10)), cr_acct$             , ch(12),~
               at (17,49), fac(hex(8c))  , cr_acct_descr$       , ch(30),~
                                                                         ~
               at (18,02), fac(hex(8c))  , useprompt$           , ch(20),~
               at (18,30), fac(lfac$(11)), usetype$             , ch(05),~
               at (18,49), fac(hex(8c))  , usetype_descr$       , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), "(8)Locations",                               ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,20), fac(hex(8c)), pf5$                           ,~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                   keys(hex(00010405080d0f10)), key (keyhit%)

               if keyhit% <> 8 then L40960
                  gosub locations
                  goto L40300

L40960:        if keyhit% <> 13 then L41000
                  call "MANUAL" ("HNYRETIN")
                  goto L40300

L41000:        if keyhit% <> 15 then L41040
                  call "PRNTSCRN"
                  goto L40300

L41040:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50200,         /* Type of Withdrawal*/     ~
                              L50290,         /* Ship-to Customer  */     ~
                              L50370,         /* Invoice Number    */     ~
                              L50530,         /* Part Number       */     ~
                              L50760,         /* From Store / Lot  */     ~
                              L51170,         /* Qty, To Store/Lot */     ~
                              L51540,         /* Description       */     ~
                              L51590,         /* Inventory Costs   */     ~
                              L51670,         /* Inventory Account */     ~
                              L51740,         /* Credit Account    */     ~
                              L51810          /* Usage Type        */
            return

L50200
*       * Test for TYPE OF WITHDRAWAL           TYPE$
            if pos("IW" = type$) > 0% then last_type$ = type$  else      ~
                                      errormsg$ = "Enter 'I' -or- 'W'."
            if type$ = "I" then type_descr$ = "Withdrawn by Invoicing"   ~
                           else type_descr$ = "Inventory Withdrawal"
            if type$ = "I" then sn_source$  = "4"                        ~
                           else sn_source$  = "0"
            return

L50290
*       * Test for SHIP-TO CUSTOMER NUMBER      CUSCODE$
            if type$ = "W" then return
                cusname$ = hex(06) & "Select Ship-to Customer"
                call "GETCODE" (#3, cuscode$, cusname$, 0%, 1.30, f1%(3))
                if f1%(3) = 0% then errormsg$ = "Invalid Customer Code." ~
                               else last_cuscode$ = cuscode$
            return

L50370
*       * Test for INVOICE NUMBER               INVNR$
          if type$ = "W" or invnr$ = " " then return
            if invnr$ <> "?" then L50460
                plowkey$ = cuscode$
                call "PLOWCODE" (#11, plowkey$, " ", 9%, 0.16, invoice%)
                if invoice% = 1% then invnr$ = str(plowkey$,10,8) else   ~
                                      invnr$ = " "
                if invoice% = 1% then L50480
                     invnr$ = " " : errormsg$ = hex(00) : return
L50460:     plowkey$ = str(cuscode$) & invnr$
            call "READ100" (#11, plowkey$, invoice%)
L50480:     if invoice% = 1% then invmsg$ = "Invoice is on file."        ~
                             else invmsg$ = "Invoice is not on file."
          last_invnr$ = invnr$
          return

L50530
*       * Test for PART NUMBER                  PART$
            part_descr$ = hex(06) & "Select Part Number"
            call "GETCODE" (#4, part$, part_descr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then L50580
                errormsg$ = "Part not on file"  :  return
L50580:     call "LOTENABL" (part$, lot_enable%, lot_len%, #2, #4)
            call "SERENABL" (part$, sn_enable% , u3%,      #2, #4)
            if invoice% = 0% then return
                shipped, seqnrs% = 0%
                plowkey$ = str(cuscode$) & str(invnr$)
L50630:         call "PLOWNEXT" (#12, plowkey$, 17%, f1%(12))
                if f1%(12) = 1% then L50680
                     if shipped <= 0 then errormsg$ =                    ~
                                 "Part not shipped on Invoice " & invnr$
                     return
L50680:         get #12 using L50690, x_$, x_
L50690:              FMT POS(24), CH(25), POS(93), PD(14,4)
                if x_$ <> part$ or x_ <= 0 then L50630
                     shipped = shipped + x_
                     seqnrs% = seqnrs% + 1%
                     seqnrs$(seqnrs%) = str(plowkey$,18,3)
                     goto L50630

L50760
*       * Test for FROM STORE / LOT             FROM_STR$, FROM_LOT$
          oldhnyquan% = 0%
          from_str_descr$ = " " : if from_str$ = " " then return
            if invoice% = 1% then x_ = 99 else x_ = 0
            call "GETCODE" (#8, from_str$, from_str_descr$, 0%, x_,f1%(8))
            if f1%(8) = 1% then last_from_str$ = from_str$
            if f1%(8) = 1% or invoice% = 1% then L50850
                errormsg$ = "Store Code not on file."  :  return

L50850
*        Test that invoice has lot number on it.
            if invoice% = 0% then L51050
                temp   = 0
                for s% = 1% to seqnrs%
                     plowkey$ = str(cuscode$) & str(invnr$) & seqnrs$(s%)
                     call "READ100" (#12, plowkey$, f1%(12))
                     if f1%(12) = 0% then L50990
                     get #12 using L50930, lots$(), lotqtys()
L50930:                   FMT POS(197), 30*CH(6), 30*PD(14,4)
                     for l% = 1% to 30%
                          if lotqtys(l%) = 0 then L50990
                               if lots$(l%) = from_lot$                  ~
                                           then temp = temp + lotqtys(l%)
                     next l%
L50990:         next s%
                if temp > 0 then L51030
                     errormsg$ = "Part not invoiced from this lot."
                     return
L51030:         shipped = temp

L51050
*        Load HNYQUAN record if exists.  Will use for cost defaults.
            if from_str$ = " " then return
                plowkey$ = str(part$) & str(from_str$) & from_lot$
                call "READ100" (#6, plowkey$, oldhnyquan%)
                if oldhnyquan% <> 1% then return
                get #6 using L51110, costs(), hny_acct$
L51110:              FMT POS(125), 12*PD(14,4), POS(259), CH(9)
                cost = costs( 1) + costs( 2) + costs( 3) + costs( 4) +   ~
                       costs( 5) + costs( 6) + costs( 7) + costs( 8) +   ~
                       costs( 9) + costs(10) + costs(11) + costs(12)
                call "CONVERT" (cost, -2.4, costs$)
                put cost$ using L51116, costs()
L51116:              FMT  12*PD(14,4)
                call "GLFMT" (hny_acct$)
                return


L51170
*       * Test for QTY, TO STORE/LOT            QTY$, TO_STR$, TO_LOT$
*        First QTY
            if invoice% = 1% then x_ = shipped else x_ = 9999999.98
            call "NUMTEST" (qty$, .01, x_, errormsg$, 2.4, qty)
            if errormsg$ <> " " then return

*        And Next To Store
            to_str_descr$ = hex(06) & "Select TO Store"
            call "GETCODE" (#8, to_str$, to_str_descr$, 0%, 0, f1%(8))
            if f1%(8) = 1% then last_to_str$ = to_str$
            if f1%(8) = 1% then L51300
                errormsg$ = "Store not on file."  :  return

L51300
*        And now test Lot Number
            lotmsg$  = " "
            if to_lot$ = "?" then to_lot$ = " "
            plowkey$ = str(part$) & str(to_str$) & to_lot$
            call "READ100" (#6, plowkey$, f1%(6))
            if f1%(6) = 0% then L51390
                get #6 using L51360, hny_acct$
L51360:              FMT POS(259), CH(9)
                call "GLFMT" (hny_acct$)
                goto L51440
L51390:     call "LOTVALID" (part$, to_str$, to_lot$, #2, #4, #6,        ~
                                                               errormsg$)
            lotmsg$ = "Lot" & hex(94) & "not" & hex(8c) & "on file."
            if errormsg$ <> " " then return

L51440
*        Finally take care of Serial Numbers
            if sn_enable% = 0% then return
            sn_loc$ = " "
            sn_trankey$ = userid$ & "Returns"
            if type$ = "I" then sn_loc$ = str(cuscode$) & invnr$
            call "SERSELCT" (part$, sn_loc$, qty, 1%, 5%, "IR",          ~
                             sn_trankey$, "2", sn_source$, errormsg$,    ~
                             #2, #4, #20, #21)
            return

L51540
*        Test for Description / Free Text      DESCR$
            if descr$ = " " then errormsg$ = "May not be blank"          ~
                            else last_descr$ = descr$
            return

L51590
*        Test for Inventory Costs              COSTS$
            return

L51670
*        Test for Inventory Account            HNY_ACCT$
            hny_acct_descr$ = hex(06) & "Select Inventory Account"
            call "GETCODE" (#7, hny_acct$, hny_acct_descr$, 0%,0, f1%(7))
            if f1%(7) = 1% then last_hny_acct$ = hny_acct$  else         ~
                                errormsg$ = "Account Code not on file."
            return

L51740
*        Test for Credit Account               CR_ACCT$
            cr_acct_descr$ = hex(06) & "Select Credit Account"
            call "GETCODE" (#7, cr_acct$, cr_acct_descr$, 0%,0, f1%(7))
            if f1%(7) = 1% then last_cr_acct$ = cr_acct$  else           ~
                                errormsg$ = "Account Code not on file."
            return

L51810
*        Test for Usage Type                   USETYPE$
            if usetype$ = "?" then usetype$ = " "
            usetype_descr$ = hex(06) & "Please select a Usage Type."
            call "PLOWCODE" (#32, usetype$, usetype_descr$, 0%, .3,      ~
                f1%(32))
            if f1%(32) <> 0% then return
                errormsg$ = "Usage Type not on file. Try again."
                return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#21)
            plowkey$ = str(userid$)
            call "PLOWNEXT" (#9, plowkey$, 3%, f1%(9))
            end f1%(9)
