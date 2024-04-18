        REM *************************************************************~
            *                                                           *~
            *  RRRR    OOO   PPPP    CCC   L       SSS   IIIII  N   N   *~
            *  R   R  O   O  P   P  C   C  L      S        I    NN  N   *~
            *  RRRR   O   O  PPPP   C      L       SSS     I    N N N   *~
            *  R   R  O   O  P      C   C  L          S    I    N  NN   *~
            *  R   R   OOO   P       CCC   LLLLL   SSS   IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ROPCLSIN - Maintain ROP Part Class Codes                  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/27/89 ! Original                                 ! MJB *~
            * 04/06/94 ! APC Version                              ! RHH *~
            * 11/21/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            *************************************************************

        dim                                                              ~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_map(10%),              /* Description Map for PLOW   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fill$13,                     /* Record Filler              */~
            hdr$(3%)132,                 /* for PLOWCODE               */~
            incl(1%),                    /* for PLOWCODE               */~
            incl$(1%)1,                  /* for PLOWCODE               */~
            inpmessage$79,               /* Informational Message      */~
            inpmsg$(5%,15%)79,           /* Input Message Array        */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$25,                  /* for PLOWCODE               */~
            ropcflag$1,                  /* ROP Calculation Flaf       */~
            ropcldescr$30,               /* ROP Class Description      */~
            ropcode$4,                   /* ROP Class Code             */~
            ropformula$1,                /* ROP Calc Formula           */~
            ropxflag$1,                  /* ROP Excess Test Flag       */~
            rptdescr$35,                 /* for PLOWCODE               */~
            scrtime$8,                   /* Screen Time                */~
            userid$3                     /* Current User Id            */~

        dim f2%(2%),                     /* = 0 if the file is open    */~
            f1%(2%),                     /* = 1 if READ was successful */~
            fs%(2%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(2%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) R.O.P. Class Code Maintenance     "
            pname$ = "ROPCLSIN - Rev: R6.04"

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
            * #01 ! ROPCLASS ! ROP Part Class Codes                     *~
            * #02 ! ROPHNY   ! ROP Master Part File                     *~
            *************************************************************

            select #01, "ROPCLASS",                                      ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos = 1,    keylen =  4

            select #02, "ROPHNY",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen =  25,                     ~
                        alt key  1, keypos =  104, keylen = 4, dup


            call "SHOSTAT" ("Opening Files")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 50%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  fill$ = " "
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            gosub load_messages

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10205
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10205:               if keyhit% = 14% then gosub print_report
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then       delete_code
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% >  5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            read #1,hold,key = ropcode$, eod goto L19090
               delete #1
L19090:     put #1 using L35060, ropcode$, ropcldescr$, ropxflag$,        ~
                                ropcflag$, ropformula$, fill$
            write #1
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20140,         /* ROP Class Code         */~
                              L20170,         /* ROP Class Desc         */~
                              L20200,         /* ROP Excess Flag        */~
                              L20240,         /* ROP Calc Flag          */~
                              L20280          /* ROP Calc Formula       */
            return
L20140: REM Def/Enable ROP Class Code              ROPCODE$
            return

L20170: REM Def/Enable ROP Class Description       ROPCLDESCR$
            return

L20200: REM Def/Enable ROP Excess Test Flag        ROPXFLAG$
            if ropxflag$ = " " then ropxflag$ = "N"
            return

L20240: REM Def/Enable ROP Calculation Flaf        ROPCFLAG$
            if ropcflag$ = " " then ropcflag$ = "Y"
            return

L20280: REM Def/Enable ROP Calc Formula            ROPFORMULA$
            if ropformula$ = " " then ropformula$ = "1"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        load_messages
            init (" ") inpmsg$()
            inpmsg$(1%,  1%) = "Enter ROP Class Code?               "
            inpmsg$(1%,  2%) = "Enter ROP Class Description?        "
            inpmsg$(1%,  3%) = "Enter 'Y' or 'N' to test the Deviati" &  ~
                               "on Ratio for this Class - N/A?"
            inpmsg$(1%,  4%) = "Enter 'Y' or 'N' to Calculate ROP "   &  ~
                               "for this Class?"
            inpmsg$(1%,  5%) = "Enter 1, 2, or 3 to designate the "   &  ~
                               "Formula to Use for ROP Calculation?"
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      ropcflag$, ropcldescr$, ropcode$, ropformula$,     ~
                      ropxflag$
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
        dataload
            rec% = 0%
            read #1,key = ropcode$, eod goto L30110
            get #1 using L35060, ropcode$, ropcldescr$, ropxflag$,        ~
                                ropcflag$, ropformula$, fill$
            rec% = 1%
L30110: return

        REM *************************************************************~
            *           P R I N T   C O D E S   R E P O R T             *~
            *-----------------------------------------------------------*~
            * Call PLOWCODE to Print ROP Class Codes Report.            *~
            *************************************************************
        print_report
            plowkey$ = " "
            rptdescr$ = hex(06) & "REORDER POINT CLASS CODES LISTING"
            incl(1%) = 0  :  incl$(1%) = " "
            hdr$(1%) = "ROP Class Code  Description                   "& ~
                       "Test --- N/A  Calculate ROP   Formula         "

                     /*     xxxx       xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx */
                     /*     x              x            x             */

            descr_map(1%) =  1.04  : descr_map( 2%) =  6.0
            descr_map(3%) =  5.30  : descr_map( 4%) = 17.0
            descr_map(5%) = 35.01  : descr_map( 6%) = 52.0
            descr_map(7%) = 36.01  : descr_map( 8%) = 67.0
            descr_map(9%) = 37.01  : descr_map(10%) = 80.0

            call "PLOWCODE" (#1, plowkey$, rptdescr$, -9000%, 0.3,       ~
                             f1%(1%), hdr$(), 0, 0, incl(), incl$(),     ~
                             "r", " ", #2, descr_map())
            return

        REM *************************************************************~
            *           D E L E T E   C L A S S   C O D E               *~
            *-----------------------------------------------------------*~
            * Attempt to Delete an ROP Class Code                       *~
            *************************************************************
        delete_code
            read #2,key 1% = ropcode$, eod goto L32080
               goto L32190
L32080:     ask% = 2%
            call "ASKUSER" (ask%, "***** DELETE CONFIRMATION *****",     ~
                            "Code May Be DELETED", "Press PF-24 to " &   ~
                            "DELETE", "or Press PF-1 to RETURN to E" &   ~
                            "dit Mode")
            if ask% = 1% then editpg1
            if ask% <> 24% then L32080
            read #1,hold,key = ropcode$, eod goto inputmode
            delete #1
            goto inputmode

L32190:     ask% = 2%
            call "ASKUSER" (ask%, "***** Delete Denial *****",           ~
                           "ROP Code '" & ropcode$ & "' is currently " & ~
                           "Assigned to one or more Parts", "It CANNOT" &~
                           " be DELETED at this time", "Press PF-1 to" & ~
                           " RETURN to Edit Mode")
            if ask% = 1% then editpg1
            goto L32190

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *-----------------------------------------------------------*~
            * Internal Subroutines for GETSCRN, MANUAL and PRNTSCRN.    *~
            *************************************************************

            call_screen
                call "GETSCRN" ("C", " ", cursor%(), 0%)
                return

            pf1315
                if keyhit% <> 15% then return
                    call "PRNTSCRN"
                    return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: ROPCLASS                          */~
            CH(4),          /* ROP Part Class Code                     */~
            CH(30),         /* Description                             */~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(13)          /* Unused Space                            */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              if fieldnr% = 0% then inpmessage$ = edtmessage$            ~
                               else inpmessage$ = inpmsg$(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,         /* ROP Class Code    */   ~
                                L40190,         /* ROP Class Desc    */   ~
                                L40200,         /* ROP Excess Flag   */   ~
                                L40200,         /* ROP Calc Flag     */   ~
                                L40200          /* ROP Calc Formula  */
              goto L40230

L40190:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "ROP Class Code",                             ~
               at (06,35), fac(lfac$(1%)), ropcode$             , ch(04),~
                                                                         ~
               at (07,02), "ROP Class Description",                      ~
               at (07,35), fac(lfac$(2%)), ropcldescr$          , ch(30),~
                                                                         ~
               at (08,02), "Dev. Ratio Test Flag - N/A",                 ~
               at (08,35), fac(lfac$(3%)), ropxflag$            , ch(01),~
                                                                         ~
               at (09,02), "ROP Calculation Flag - (Y/N)",               ~
               at (09,35), fac(lfac$(4%)), ropcflag$            , ch(01),~
                                                                         ~
               at (10,02), "ROP Calculation Formula Code",               ~
               at (10,35), fac(lfac$(5%)), ropformula$          , ch(01),~
              at (14,19), "Formula '1' is ROP = Usage & Safety Stock",   ~
              at (15,19), "        '2' is ROP = Safety Stock & LeadTime",~
              at (16,19), "        '3' is ROP = Usage & OH",             ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               gosub pf1315
               if keyhit% = 15% then L40230
               return

        set_pf1
            scrtime$ = " " : call "TIME" (scrtime$)
        if edit% = 2% then L40780     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                      (1" &       ~
                      "4)Print Listing        (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40740
               str(pf$(3%),64%)    = " "  : str(pfkeys$,16%,1%) = hex(ff)
               str(pf$(3%),39%,18%) = " " : str(pfkeys$,14%,1%) = hex(ff)
L40740:     if fieldnr% > 2% then L40760
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40760:     return

L40780: if fieldnr% > 0% then L40870  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                          (" &       ~
                      "8)Delete Code                          "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                      (1" &       ~
                      "4)Print Listing        (16)Save Data   "
            pfkeys$ = hex(01fffffffffff708ffffffffff0e0f1000)
            return
L40870:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* ROP Class Code         */~
                              L50250,         /* ROP Class Desc         */~
                              L50300,         /* ROP Excess Flag        */~
                              L50350,         /* ROP Calc Flag          */~
                              L50400          /* ROP Calc Formula       */
            return
L50140: REM Test for ROP Class Code               ROPCODE$
            if ropcode$ <> " " then L50200
            call "GETCODE" (#1, ropcode$, " ", 0%, 0.30, f1%(1))
                if f1%(1) <> 0% then L50200
                     errormsg$ = hex(00)
                     return
L50200:     gosub dataload
            if rec% = 0% then return
                return clear all
                goto editpg1

L50250: REM Test for ROP Class Description        ROPCLDESCR$
            if ropcldescr$ <> " " then return
            errormsg$ = "ROP Class Code Description Cannot be Blank"
            return

L50300: REM Test for ROP Excess Test Flag         ROPXFLAG$
            if ropxflag$ = "Y" or ropxflag$ = "N" then return
                errormsg$ = "Must Enter 'Y' or 'N'"
                return

L50350: REM Test for ROP Calculation Flag         ROPCFLAG$
            if ropcflag$ = "Y" or ropcflag$ = "N" then return
                errormsg$ = "Must Enter 'Y' or 'N'"
                return

L50400: REM Test for ROP Calc Formula             ROPFORMULA$
            if ropformula$ = "1" or ropformula$ = "2" or                 ~
                                    ropformula$ = "3" then return
                errormsg$ = "Must Enter 1, 2, or 3"
                return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
