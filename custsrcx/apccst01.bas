        REM *************************************************************~
            * APCCST01 - Utility Program to Calculate the Number of     *~
            *            Transactions for a period of time. The Files   *~
            *            used are associated with Sales Orders and      *~
            *            Purchase Orders.                               *~
            *                                                           *~
            *     Note - The (RCVMASTR) and (RCVLINES) files are used   *~
            *            to Count P.O. Transactions, because of the     *~
            *            use of Open P.O's.                             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/20/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for New Release Upgrade to R6.04.03  ! RHH *~
            * 11/12/97 ! Mod add new Index to VBKLINES Alt 2      ! RHH *~
            * 03/12/98 ! Y2K Conversion                           ! DJD *~
            *************************************************************

        dim                              /* (Program Variables )       */~
            bck_key$25,                  /* BCKMASTR - Primary Key     */~
            bck_key1$19,                 /* BCKLINES - Primary Key     */~
            so$16,                       /* Sales Order Number         */~
            bck_id$3,                    /* User Id for Entry          */~
            bck_dte$6,                   /* Date Sales Order Entered   */~
            bck_part$25,                 /* Line Item Part Number      */~
            bck_qty$10,                  /* S.O. Quantity              */~
            bck_cust$9, sav_cust$9,      /* S.O. Customer Code         */~
            vbk_key$25,                  /* VBKMASTR - Primary Key     */~
            vbk_key1$28,                 /* VBKLINES - Primary Key     */~
            vbk_part$25, vbk$1,          /* Material Part Number       */~
            vbk_dte$6,                   /* Date Material Received     */~
            sav_ven$9,                   /* Save Vendor Number         */~
            cnt(12%,4%),                 /* Counting Matrix            */~
            scr$(15%)55,                 /* Screen Text                */~
            bg_date$8, bg_dte$6,         /* Starting Date              */~
            ed_date$8, ed_dte$6,         /* Ending Date                */~
            sel$1, sel_d$(3%)30,         /* Selection Code   1 or 2    */~
            ty$1, ty_d$(3%)30            /* Type Line Items, Quantity  */

        dim                              /* (Program) - Variables      */~
            date$8,                      /* Run Date                   */~
            cursor%(2),                  /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(5),                      /* = 0 if the file is open    */~
            f1%(5),                      /* = 1 if READ was successful */~
            fs%(5),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5)20                   /* Text from file opening     */

/* Y2K */
        dim blankdate$6,                 /* Blank date compares         */~
            workdate10$10,               /* 10 Char Work Date           */~
            workdate8$8,                 /* 8 Char work date            */~
            bg_date10$10,                /* 10 char begin date          */~
            ed_date10$10                 /* 10 char end date            */
/* Y2K */
        
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Utility to Count S.O.'s and P.O.'s"
            pname$ = "APCCST01 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! BCKMASTR ! S.O. Master File                         *~
            * #2  ! BCKLINES ! S.O. Detail File                         *~
            * #3  ! VBKMASTR ! Purchase Order Header File               *~
            * #4  ! VBKLINES ! Purchase Order Line Item File            *~
            * #5  !          !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos = 1,    keylen = 25,                      ~
                        alt key 1, keypos = 26, keylen = 16, dup

            select #2,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos = 10,   keylen = 19

            select #3,  "VBKMASTR",                                      ~
                        varc,     indexed,  recsize =  1030,             ~
                        keypos =  1,   keylen = 25,                      ~
                        alt key 1, keypos =  10, keylen = 16

            select #4,  "VBKLINES",                                      ~
                        varc,     indexed,  recsize =   700,             ~
                        keypos =  1,   keylen = 28,                      ~
                        alt key 1, keypos = 333, keylen = 20, dup

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

         scr$( 1%)="     Home Ctr's     EDI         Misc.      Total    "
         scr$( 2%)="     ----------  ----------  ----------  ---------- "
         scr$( 3%)="(0)  xxxxxxxxxx  xxxxxxxxxx  xxxxxxxxxx  xxxxxxxxxx "
         scr$( 4%)="(1)                                                 "
         scr$( 5%)="(2)                                                 "
         scr$( 6%)="(3)                                                 "
         scr$( 7%)="(4)                                                 "
         scr$( 8%)="(5)                                                 "
         scr$( 9%)="(6)                                                 "
         scr$(10%)="(7)                                                 "
         scr$(11%)="(8)                                                 "
         scr$(12%)="(9)                                                 "
         scr$(13%)="     ----------  ----------  ----------  ---------- "
         scr$(14%)="     xxxxxxxxxx  xxxxxxxxxx  xxxxxxxxxx  xxxxxxxxxx "
        REM              !-- (6)     !-- (18)    !-- (30)    !-- (42)

            sel_d$(1%) = "(1) - Sales Order Tranactions "
            sel_d$(2%) = "(2) - Purchase Order Transact "
            sel_d$(3%) = "                              "
            ty_d$(1%)  = "(1) - Count Line Item Trans.  "
            ty_d$(2%)  = "(2) - Count Product Quantities"
            ty_d$(3%)  = "                              "
            sel%, ty% = 3%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 16% then gosub scan_one
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
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

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Begining and Ending Transaction Period?        ",~
         "Enter a (1) = Sales Orders, (2) = Purchase Orders for Calc.  ",~
         "Enter a (1) = Line Items, (2) = Product Quantities?          "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, bck_key$, bck_key1$,       ~
                      so$, bck_id$, bck_dte$, bck_part$, bck_qty$,       ~
                      bg_date$, bg_dte$, ed_date$, ed_dte$, sel$, ty$,   ~
                      bg_date10$, ed_date10$

            scan% = 0%
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40180,         /* BEG/END Calc Date    */~
                                L40180,         /* (1,2) S.O. , P.O.    */~
                                L40180          /* (1,2) Lines, Quantity*/
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Begin Calculation Date:",                    ~
               at (03,30), fac(lfac$( 1)), bg_date10$           , ch(08),~
                                                                         ~
               at (03,40), "End Date:",                                  ~
               at (03,50), fac(lfac$( 1)), ed_date10$           , ch(08),~
                                                                         ~
               at (04,02), "Sales/Purchase Orders :",                    ~
               at (04,30), fac(lfac$( 2)), sel$                 , ch(01),~
               at (04,40), fac(hex(84)), sel_d$(sel%)           , ch(30),~
                                                                         ~
               at (05,02), "Line Items/Quantities :",                    ~
               at (05,30), fac(lfac$( 3)), ty$                  , ch(01),~
               at (05,40), fac(hex(84)), ty_d$(ty%)             , ch(30),~
                                                                         ~
               at (07,15), fac(hex(84)), scr$( 1%)              , ch(55),~
               at (08,15), fac(hex(84)), scr$( 2%)              , ch(55),~
               at (09,15), fac(hex(84)), scr$( 3%)              , ch(55),~
               at (10,15), fac(hex(84)), scr$( 4%)              , ch(55),~
               at (11,15), fac(hex(84)), scr$( 5%)              , ch(55),~
               at (12,15), fac(hex(84)), scr$( 6%)              , ch(55),~
               at (13,15), fac(hex(84)), scr$( 7%)              , ch(55),~
               at (14,15), fac(hex(84)), scr$( 8%)              , ch(55),~
               at (15,15), fac(hex(84)), scr$( 9%)              , ch(55),~
               at (16,15), fac(hex(84)), scr$(10%)              , ch(55),~
               at (17,15), fac(hex(84)), scr$(11%)              , ch(55),~
               at (18,15), fac(hex(84)), scr$(12%)              , ch(55),~
               at (19,15), fac(hex(84)), scr$(13%)              , ch(55),~
               at (20,15), fac(hex(84)), scr$(14%)              , ch(55),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40680
                  call "PRNTSCRN"
                  goto L40210

L40680: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1
        if edit% = 2% then L40870     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40830
                str(pf$(3),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40830:     if fieldnr% > 1% then L40850
                str(pf$(2),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40850: return

L40870: if fieldnr% > 0% then L41000  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Calc. Data  "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            if scan% = 0% then return
               pf$(3) = "                                        " &     ~
                        "                       (16)Exit Program"
        return

L41000:     pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Purchase Order Display Screen.                            *~
            *************************************************************

        deffn'102(fieldnr%)

L42090:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Begin Calculation Date:",                    ~
               at (03,30), fac(hex(84)),   bg_date10$           , ch(10),~
                                                                         ~
               at (03,40), "End Date:",                                  ~
               at (03,50), fac(lfac$( 1)), ed_date10$           , ch(10),~
                                                                         ~
               at (04,02), "Sales/Purchase Orders :",                    ~
               at (04,30), fac(hex(84)),   sel$                 , ch(01),~
               at (04,40), fac(hex(84)), sel_d$(sel%)           , ch(30),~
                                                                         ~
               at (05,02), "Line Items/Quantities :",                    ~
               at (05,30), fac(hex(84)),   ty$                  , ch(01),~
               at (05,40), fac(hex(84)), ty_d$(ty%)             , ch(30),~
                                                                         ~
               at (07,15), fac(hex(84)), scr$( 1%)              , ch(55),~
               at (08,15), fac(hex(84)), scr$( 2%)              , ch(55),~
               at (09,15), fac(hex(84)), scr$( 3%)              , ch(55),~
               at (10,15), fac(hex(84)), scr$( 4%)              , ch(55),~
               at (11,15), fac(hex(84)), scr$( 5%)              , ch(55),~
               at (12,15), fac(hex(84)), scr$( 6%)              , ch(55),~
               at (13,15), fac(hex(84)), scr$( 7%)              , ch(55),~
               at (14,15), fac(hex(84)), scr$( 8%)              , ch(55),~
               at (15,15), fac(hex(84)), scr$( 9%)              , ch(55),~
               at (16,15), fac(hex(84)), scr$(10%)              , ch(55),~
               at (17,15), fac(hex(84)), scr$(11%)              , ch(55),~
               at (18,15), fac(hex(84)), scr$(12%)              , ch(55),~
               at (19,15), fac(hex(84)), scr$(13%)              , ch(55),~
               at (20,15), fac(hex(84)), scr$(14%)              , ch(55),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42560
                  call "PRNTSCRN"
                  goto L42090

L42560: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2

         scr$( 1%)="              Purchase order Totals                 "
         scr$( 2%)="              ---------------------                 "
         scr$( 3%)="                                                    "
         scr$( 4%)="     Aluminum Trans  Vinyl Trans.  Misc. Trans.     "
         scr$( 5%)="     --------------  ------------  ------------     "
         scr$( 6%)="(*)   XXXXXXXXXXXX   XXXXXXXXXXXX  XXXXXXXXXXXX     "
         scr$( 7%)="                                                    "
         scr$( 8%)="                                                    "
         scr$( 9%)="                                                    "
         scr$(10%)="                                                    "
         scr$(11%)="                                                    "
         scr$(12%)="                                                    "
         scr$(13%)="                                                    "
         scr$(14%)="                                                    "
        REM               !- (7)         !-- (22)      !-- (36)

            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* Beg/End Production Dat*/ ~
                              L50360,         /* Selection S.O./P.O.   */ ~
                              L50500          /* Line Items/Quantities */
            return

/* Y2K */
/* CHANGED ALL REFREENCES OF BG_DATE$ TO BG_DATE10$ AND */
/* ED_DATE$ TO ED_DATE10$                               */
/* Y2K */

L50140: REM Calculation Dates                     BG_DATE$, BG_DTE$, BG_DATE10$
            if bg_date10$ <> " " then goto L50180

               /* for blank date - use today */
               workdate10$ = date
               call "DATFMTC" (workdate10$) 

               bg_date10$ = workdate10$

L50180:     date% = 0%
            call "DATEOK" (bg_date10$, date%, errormsg$)
            if date% = 0% then return
            x$ = bg_date10$
            call "DATUNFMT" (x$)
            bg_dte$ = str(x$,1%,6%)

        REM Ending Calculation Date               ED_DATE$, ED_DTE$, ED_DATE10$
            if ed_date10$ <> " " then goto L50280
               ed_date10$ = bg_date10$

L50280:     date% = 0%
            call "DATEOK" (ed_date10$, date%, errormsg$)
            if date% = 0% then return
            x$ = ed_date10$
            call "DATUNFMT" (x$)
            ed_dte$ = str(x$,1%,6%)
        return

L50360: REM Selection Code                        SEL$,SEL_D$()
            if sel$ <> " " then goto L50400
               sel$ = "1"

L50400:     sel% = 3%
            convert sel$ to sel%, data goto L50420
L50420:
            if sel% < 1% or sel% > 2% then goto L50460
        return
L50460:   errormsg$="(Error) - Invalid Selection Code? ( 1 or 2 )"
          sel$ = " "
        return

L50500: REM Calc Type Code                        TY$, TY_D$()
            if ty$ <> " " then goto L50540
               ty$ = "1"

L50540:     ty% = 3%
            convert ty$ to ty%, data goto L50560
L50560:
            if ty% < 1% or ty% > 2% then goto L50600
        return
L50600:   errormsg$="(Error) - Invalid Calc Type Code? ( 1 or 2 )"
          ty$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        open_files

            call "APCPAUSE" (apc%, "APCCST01")
            if apc% <> 0% then goto exit_program

            call "SHOSTAT" ("Initialization")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),   0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),   0%, rslt$(4%))

            mat f1% = zer
        return

        scan_one
            gosub open_files
            if sel% = 2% then goto scan_two

            call "SHOSTAT" ("Begin Processing - Sales Orders")
            rhh% = 0%
            sav_cust$ = " "
            bck_key$ = " "
            read #1,key > bck_key$, using L60280, bck_key$, bck_dte$,     ~
                                          bck_id$, eod goto scan_one_done
L60280:        FMT CH(25), POS(830), CH(6), CH(3)
            goto L60330
        scan_one_nxt
            read #1, using L60280, bck_key$,bck_dte$, bck_id$,            ~
                                                   eod goto scan_one_done
L60330:     if sav_cust$ = str(bck_key$,1%,9%) then goto L60400
               bck_cust$ = str(bck_key$,1%,9%)
               sav_cust$ = bck_cust$
             print at(02,27);hex(84);"Processing Customer - ";bck_cust$;
               rhh% = rhh% + 1%
        REM    IF RHH% > 50% THEN GOTO SCAN_ONE_DONE

L60400:     if bck_dte$ < bg_dte$ or bck_dte$ > ed_dte$ then             ~
                                                        goto scan_one_nxt
               cl% = 3%                          /* MISCELLANEOUS      */
               if str(bck_cust$,1%,2%) = "HQ" or                         ~
                  str(bck_cust$,1%,2%) = "LO" then cl% = 1%
               if bck_id$ = "EDI" then cl% = 2%
               gosub scan_one_dtl
               goto scan_one_nxt
        scan_one_done
            scan% = 1%
            gosub format_screen_one
L60510:     gosub'101(0%,2%)
            if keyhit% = 16% then goto exit_program
               goto L60510
        return

        scan_one_dtl
            so$       = str(bck_key$,10%,16%)
            bck_key1$ = " "
            str(bck_key1$,1%,16%) = so$
            read #2,key > bck_key1$, using L60620, bck_key1$, bck_part$,  ~
                                                  bck_qty, eod goto L60690
L60620:        FMT POS(10), CH(19), POS(32), CH(25), POS(93), PD(14,4)
        scan_one_dtl_nxt
            read #2, using L60620, bck_key1$, bck_part$, bck_qty,         ~
                                                           eod goto L60690
            if so$ <> str(bck_key1$,1%,16%) then goto L60690
               gosub trans_count_one
               goto scan_one_dtl_nxt
L60690: return

        trans_count_one
            pd% = 0%                                   /* PRODUCT LINE */
            convert str(bck_part$,1%,1%) to pd%, data goto L60740
L60740:
            pd% = pd% + 1%
            if str(bck_part$,1%,3%) = "312" then pd% = 5% /* VINYL PATI*/
            x = 1.0
            if ty% = 2% then x = bck_qty
            cnt(pd%,cl%) = cnt(pd%,cl%) + x     /* PROD TOTAL BY CLASS */
            cnt(pd%,4%)  = cnt(pd%,4%)  + x     /* PROD TOTAL ALL CLASS*/
            cnt(12%,cl%) = cnt(12%,cl%) + x     /* CLASS TOTAL ALL PROD*/
            cnt(12%, 4%) = cnt(12%, 4%) + x     /* GRAND TOTALS        */
        return

        format_screen_one
         for i% = 1% to 12%
           if i% = 11% then goto L60960
           convert cnt(i%,1%) to str(scr$(2%+i%),6%,10%),pic(######.##-)

           convert cnt(i%,2%) to str(scr$(2%+i%),18%,10%),pic(######.##-)

           convert cnt(i%,3%) to str(scr$(2%+i%),30%,10%),pic(######.##-)

           convert cnt(i%,4%) to str(scr$(2%+i%),42%,10%),pic(######.##-)

L60960:  next i%
        return

        scan_two                                   /* Purchase Orders  */
            call "SHOSTAT" ("Begin Processing _ Purchase Orders")
            vbk1, vbk2, vbk3 = 0.0                 /* (VBKMASTR) - File*/
            rhh% = 0%
            init(" ") sav_ven$, vbk_key$
            read #3,key > vbk_key$, using L61060, vbk_key$, vbk_dte$,     ~
                                                   eod goto scan_two_done
L61060:        FMT CH(25), POS(451), CH(6)
            goto L61110
        scan_two_nxt
            read #3, using L61060, vbk_key$, vbk_dte$,                    ~
                                                 eod goto scan_two_done
L61110:     if sav_ven$ = str(vbk_key$,1%,9%) then goto L61170
               sav_ven$ = str(vbk_key$,1%,9%)
             print at(02,27);hex(84);"Processing Vendor - ";sav_ven$;
               rhh% = rhh% + 1%
        REM    IF RHH% > 50% THEN GOTO SCAN_TWO_DONE

L61170:     if vbk_dte$ < bg_dte$ or vbk_dte$ > ed_dte$ then             ~
                                                        goto scan_two_nxt
               gosub scan_two_dtl
               goto scan_two_nxt
        scan_two_done
            gosub format_screen_two
L61230:     gosub'102(0%)
            if keyhit% = 16% then goto exit_program
               goto L61230
        return

        scan_two_dtl                               /* (VBKLINES) - File*/
            vbk_key1$ = " "
            str(vbk_key1$,1%,25%) = vbk_key$
            read #4,key > vbk_key1$, using L61340, vbk_key1$, vbk_part$,  ~
                                                  vbk_qty, eod goto L61420
L61340:        FMT CH(25), POS(32), CH(25), POS(93), PD(14,4)
        scan_two_dtl_nxt
            read #4, using L61340, vbk_key1$, vbk_part$, vbk_qty,         ~
                                                           eod goto L61420
            if vbk_key$ <> str(vbk_key1$,1%,25%) then goto L61420
               gosub trans_count_two
               goto scan_two_dtl_nxt
L61420: return

        trans_count_two                          /* Purchase Orders    */
            vbk$ = str(vbk_part$,2%,1%)
            x = 1.0
            if ty% = 2% then x = vbk_qty
            if vbk$ <> "1" then goto L61510       /* Not Vinyl Material */
               vbk2 = vbk2 + x                        /* Vinyl Mat.    */
               return
L61510:     if vbk$ <> "4" and vbk$ <> "5" then goto L61540
               vbk1 = vbk1 + x                        /* Aluminum Mat. */
               return
L61540: vbk3 = vbk3 + x                               /* Misc. Mat.    */
        return

        format_screen_two                        /* Purchase Orders    */
           gosub set_pf2

           convert vbk1 to str(scr$(6%),7%,12%), pic(########.##-)

           convert vbk2 to str(scr$(6%),22%,12%),pic(########.##-)

           convert vbk3 to str(scr$(6%),36%,12%),pic(########.##-)

        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
