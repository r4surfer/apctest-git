        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   QQQ   EEEEE  DDDD   IIIII  TTTTT   *~
            *  H   H  NN  N  Y   Y  Q   Q  E      D   D    I      T     *~
            *  HHHHH  N N N   YYY   Q   Q  EEEE   D   D    I      T     *~
            *  H   H  N  NN    Y    Q Q Q  E      D   D    I      T     *~
            *  H   H  N   N    Y     QQQ   EEEEE  DDDD   IIIII    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYQEDIT - Edit Non-Critical Fields in HNYQUAN File.      *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/19/93 ! Original                                 ! WPH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            ecd$8,                       /* Date of last exp date chng */~
            pcd$8,                       /* Date of last potency change*/~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            exp_date$8,                  /* Lot Expiration Date        */~
            exp_date_save$6,             /* Lot Expir. Date - For test */~
            exp_date_save2$6,            /* Lot Expir. Date - For test */~
            exp_chg_date$6,              /* Date of last exp date chng */~
            pot_chg_date$6,              /* Date of last potency change*/~
            exp_chg_user$3,              /* User who last chnged exp dt*/~
            pot_chg_user$3,              /* User who last chnged potncy*/~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            location$8,                  /* Location (Primary Bin)     */~
            lot$6,                       /* Lot Number                 */~
            lotdescr$32,                 /* Lot on file message        */~
            msg$79,                      /* Info message for plowcode  */~
            chg_msg$(2)31,               /* Last changed messages      */~
            part$25,                     /* Part Number                */~
            partdescr$34,                /* Part Description           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfac$1,                      /* Fac for field prompts, etc */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prompt$(3)20,                /* Field Prompts              */~
            potency$10,                  /* Lot Potency                */~
            store$3,                     /* Store                      */~
            strdescr$32,                 /* Store Description          */~
            userid$3,                    /* Current User Id            */~
            vf$200                       /* Variable Fields string     */

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
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #02 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #03 ! STORNAME ! Store/Warehouse Master File              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos = 90,  keylen = 4, dup

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #03, "STORNAME",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 3

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
            call "DATEOK" (date$, today%, errormsg$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "HNYQEDIT: " & str(cms2v$,,8)
            prompt$(1%) = "Bin Location"
            prompt$(2%) = "Expiration  "
            prompt$(3%) = "Lot Potency "
            chg_msg$(1), chg_msg$(2) = "Last Changed on XXXXXXXX by XXX"
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
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

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  <> 9% then L11100
                     gosub edit_vf
                     goto editpg1
L11100:           if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
            if cursor%(1%) < 10% then editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% > 6% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        edit_vf
            call "VFINPSUB"("HNYQUAN ", "E",                             ~
                 "Inventory Display- Edit Variable Fields",              ~
                 "Part: " & part$ & " Store: " & store$ & " Lot: " &     ~
                 lot$, "NN", vf$, keyhit%)
            if keyhit% <> 1% then return
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
            on fieldnr% gosub L20100,         /* Part Number            */~
                              L20200,         /* Store                  */~
                              L20300,         /* Lot Number             */~
                              L20400,         /* Location               */~
                              L20500,         /* Expiration Date        */~
                              L20600          /* Potency                */

            return
L20100: REM Def/Enable Part Number                 PART$
            return

L20200: REM Def/Enable Store                       STORE$
            return

L20300: REM Def/Enable Lot Number                  LOT$
            return

L20400: REM Def/Enable Location (Primary Bin)      LOCATION$
            return

L20500: REM Def/Enable Expiration Date             EXP_DATE$
            return

L20600: REM Def/Enable Lot Potency                 POTENCY$
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
         "Enter Part Number                                            ",~
         "Enter Store                                                  ",~
         "Enter Lot Number                                             ",~
         "Enter Location (Primary Bin)                                 ",~
         "Enter Lot Expiration Date                                    ",~
         "Enter Lot Potency (number)                                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$,                            ~
                      location$, lot$, part$, store$, vf$, potency$,     ~
                      lotdescr$, partdescr$, strdescr$, exp_date$,       ~
                      exp_chg_date$, ecd$, pot_chg_date$, pcd$,          ~
                      exp_chg_user$, pot_chg_user$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
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
            plowkey$ = str(part$,,25) & str(store$,,3) & lot$
            call "READ100" (#02, plowkey$, f1%(2))
            if f1%(2) <> 1% then return

            get #02 using L30240,                                         ~
                     part$, store$, lot$, location$, ohqty,              ~
                      exp_date$, potency,      vf$, exp_chg_user$,       ~
                       exp_chg_date$, pot_chg_user$, pot_chg_date$

L30240:         FMT POS(17), CH(25), CH(3), CH(6), XX(10), CH(8),        ~
                           PD(14,4), POS(404), CH(6), PD(14,4), POS(422),~
                           CH(200), CH(3), CH(6), CH(3), CH(6)

            exp_date_save2$ = exp_date$
            exp_date_save$ = exp_date$
            potency_save = potency
            call "CONVERT" (potency, -2.4, potency$)

            call "DATEFMT" (exp_date$)
            ecd$ =  exp_chg_date$
            call "DATEFMT" (ecd$)
            pcd$ =  pot_chg_date$
            call "DATEFMT" (pcd$)

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            plowkey$ = str(part$,,25) & str(store$,,3) & lot$
            call "READ101" (#02, plowkey$, f1%(2))
            if f1%(2) <> 1% then return
            call "DATUNFMT" (exp_date$)
            put #02 using L31180, location$,  exp_date$, potency,         ~
                     vf$, exp_chg_user$, exp_chg_date$, pot_chg_user$,   ~
                     pot_chg_date$

L31180:         FMT POS(61), CH(08), POS(404), CH(6), PD(14,4),          ~
                     POS(422), CH(200), CH(3), CH(6), CH(3), CH(6)
            rewrite #2

*       Adjust the PIP for Expiration Date Change
            if exp_date_save2$ = exp_date$ then return

            call "HNYEXSUB" (lot$, store$, part$, exp_date$, ohqty)
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        REM                 /* FILE- HNYQUAN                           */~
            CH(16),         /* Lot Number                              */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Bin Location                            */~
            PD(14,4),       /* quantity on-hand                        */~
            PD(14,4),       /* quantity back-ordered                   */~
            PD(14,4),       /* quantity on order                       */~
            PD(14,4),       /* quantity committed                      */~
            PD(14,4),       /* quantity in process                     */~
            PD(14,4),       /* Pending Inventory Withdrawals Quantity. */~
            PD(14,4),       /* Total Cost                              */~
            12*PD(14,4),    /* Cost field                              */~
            CH(10),         /* minimum on-hand                         */~
            CH(10),         /* maximum on-hand                         */~
            CH(9),          /* Inventory Source Account for Purchases  */~
            CH(9),          /* Inventory Source Account for Manufacture*/~
            CH(9),          /* Inventory Assets Account                */~
            CH(9),          /* Cost of Goods Sold Account              */~
            CH(9),          /* Sales Distribution Account              */~
            CH(9),          /* Inventory Adjustments Account           */~
            12*CH(9),       /* Inventory Variance Accounts             */~
            CH(1),          /* inventory costing method                */~
            CH(6),          /* Date 'something' expires                */~
            PD(14,4),       /* Lot Potency Factor                      */~
            CH(4),          /* Internal ID to text in TXTFILE          */~
            10*CH(20),      /* Variable Fields Data Area               */~
            CH(3),          /* User-ID of specific user                */~
            CH(6),          /* Date something changed                  */~
            CH(3),          /* User ID of Last Modification            */~
            CH(6),          /* The system date a file or record was las*/~
            CH(11)          /* Unused Space                            */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)

            init(hex(8c)) lfac$()
            init(hex(9c)) pfac$
            if fieldnr% > 3% then init(hex(8c)) pfac$
            if edit% <> 2% then L40044
                init(hex(8c)) pfac$
                if fieldnr% = 0% then                                    ~
                           init(hex(86)) lfac$(4), lfac$(5), lfac$(6)

L40044:     str(chg_msg$(1),17,8 ) = str(ecd$,,)
            str(chg_msg$(1),29,3 ) = str(exp_chg_user$,,)
            str(chg_msg$(2),17,8 ) = str(pcd$,,)
            str(chg_msg$(2),29,3 ) = str(pot_chg_user$,,)

            gosub'050(1%, fieldnr%)
            gosub set_pf1

              on fieldnr% gosub L40078,         /* Part Number       */   ~
                                L40078,         /* Store             */   ~
                                L40078,         /* Lot Number        */   ~
                                L40078,         /* Bin Location      */   ~
                                L40078,         /* Expiration Date   */   ~
                                L40079          /* Potency           */

              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40078:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40079:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Edit Miscellaneous Fields in HNYQUAN File",           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,16), fac(lfac$( 1)), part$                , ch(25),~
               at (06,45), fac(hex(8c)), partdescr$             , ch(34),~
                                                                         ~
               at (07,02), "Store",                                      ~
               at (07,16), fac(lfac$( 2)), store$               , ch(03),~
               at (07,45), fac(hex(8c)), strdescr$              , ch(32),~
                                                                         ~
               at (08,02), "Lot Number",                                 ~
               at (08,16), fac(lfac$( 3)), lot$                 , ch(06),~
               at (08,45), fac(hex(8c)), lotdescr$              , ch(32),~
                                                                         ~
               at (10,02), fac(pfac$   ), prompt$(1%)           , ch(20),~
               at (10,16), fac(lfac$(4)), location$             , ch(08),~
                                                                         ~
               at (11,02), fac(pfac$   ), prompt$(2%)           , ch(20),~
               at (11,16), fac(lfac$(5)), exp_date$             , ch(08),~
               at (11,45), fac(pfac$   ), chg_msg$(1%)          , ch(31),~
                                                                         ~
               at (12,02), fac(pfac$   ), prompt$(3%)           , ch(20),~
               at (12,16), fac(lfac$(6)), potency$              , ch(10),~
               at (12,45), fac(pfac$   ), chg_msg$(2%)          , ch(31),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40260
                  call "MANUAL" ("HNYQEDIT") : goto L40105

L40260:        if keyhit% <> 15 then L40275
                  call "PRNTSCRN" : goto L40105

L40275:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40370     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40350
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40350:     if fieldnr% > 2% and fieldnr% <> 4% then L40360
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40360:     return

L40370: if fieldnr% > 0% then L40415  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over          (9)Variable Field" &        ~
                     "s                      (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffff09ffffff0dff0f1000)
            return
L40415:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50080,         /* Part Number            */~
                              L50290,         /* Store                  */~
                              L50430,         /* Lot Number             */~
                              L50700,         /* Location               */~
                              L50800,         /* Expiration Date        */~
                              L50900          /* Potency                */

            return

L50080:     partdescr$ = hex(06) & "Select a Part Number"
            if part$ = "?" then part$ = " "
            call "PLOWCODE" (#01, part$, partdescr$, 0%, 0.32, f1%(1))
            if f1%(1) = 1% then L50230
                errormsg$ = "Part Number not on file."
                return

L50230:     plowkey$ = part$
            call "PLOWNEXT" (#02, plowkey$, 25%, f1%(2))
                if f1%(2) = 1% then L50271
                     errormsg$ = "No quantity records exist for this part"
                return
L50271:     call "PUTPAREN"(partdescr$)
                return

L50290
*        Test for STORE CODE                                STORE$
            strdescr$ = hex(06) & "Select a Store Number"
            if store$ = "?" then store$ = " "
            call "PLOWCODE" (#03, store$, strdescr$, 0%, 0.30, f1%(3))
            if f1%(3) = 1% then L50360
                errormsg$ = "Store Number not on file."
                return

L50360:     plowkey$ = str(part$) & str(store$) & hex(00)
                call "PLOWNEXT" (#02, plowkey$, 28%, f1%(2))
                if f1%(2) = 1% then L50411
                     errormsg$ = "No quantity records exist for this" &  ~
                                 " Part & Store."
                     return
L50411:     call "PUTPAREN"(strdescr$)
                     return

L50430
*        Test for LOT NUMBER                                LOT$
            if lot$ = "?" then keyhit% = 8%  /* Thanks LDJ !!!!! */
            if keyhit% <> 8% then L50530
                msg$ = hex(06) & "Select a Lot Number"
                plowkey$ = str(part$) & str(store$) & hex(00)
                call "PLOWCODE" (#02, plowkey$, msg$, 28%, 0, f1%(2))
                if f1%(2) = 1% then L50520
                     errormsg$ = "No Lots on File."
                     return
L50520:         lot$ = str(plowkey$,29,6)
L50530:     gosub dataload
            if f1%(2) = 0% then L50580
                lotdescr$ = "(Lot Found)"
            return clear all
            goto editpg1
L50580:         errormsg$ = "No quantity record found for this lot."
                return

L50700
*        TEST FOR LOCATION

            return

L50800
*        TEST FOR EXPIRATION DATE
            if exp_date$ = " " then L50865
            call "DATEOK" (exp_date$, u3%, errormsg$)
            if errormsg$ <> " " then return
            if u3% > today% then goto L50860
               errormsg$ = "Expiration Date Must Be A Future Date."
               return
L50860:     call "DATUNFMT" (exp_date$)
L50865:     if exp_date$ = exp_date_save$ then  L50880
               exp_date_save$ = exp_date$
               ecd$, exp_chg_date$ = date
               exp_chg_user$ = userid$
               call "DATEFMT" (ecd$)
L50880:     call "DATEFMT" (exp_date$)
            return

L50900
*        TEST FOR POTENCY
            call "NUMTEST" (potency$, 0, 9e7, errormsg$, 2.4, potency)
            if errormsg$ <> " " then return
            if potency > 0 then  L50980
            errormsg$ = "Standard Potency must be greater than zero"
            return

L50980:     if potency = potency_save then return
               potency_save = potency
               pcd$, pot_chg_date$ = date
               pot_chg_user$ = userid$
               call "DATEFMT" (pcd$)
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
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
