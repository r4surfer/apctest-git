        Rem *************************************************************~
            *                                                           *~
            *  Program Name      - APCCS1SB                             *~
            *  Creation Date     - 09/15/95                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - Screen Utility For the Selection of  *~
            *                      all Applicable Codes Associated with *~
            *                      a Specified Costing Definition.      *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/15/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for Upgrade to R6.04.03                RHH *~
            * 03/12/98 ! y2k checked                                DJD *~
            *************************************************************

        sub "APCCS1SB" (table$,          /* Name of Applicable Table   */~
                        cc_len%,         /* Code Lenth  Max = 4%       */~
                        cc$(),           /* Selected Codes             */~
                        #1 )             /* GENCODES - Master Table Fil*/

        dim                                                              ~
            cc$(50%)4,                   /* Selected Codes             */~
            s$(400%)1,                   /* Selection Flag             */~
            c$(400%)35,                  /* Codes From Applicable Table*/~
            hdr1$37, hdr2$37, hdr$40,    /* Screen Headers             */~
            code_key$24, desc$30,        /* Table Lookups              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            table$9,                     /* Specified Table Name       */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        Rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$ = "(New) Costmate Screen Selection Utility "
            pname$ = "APCCS1SB - Rev: R6.04"


        Rem *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! GENCODES ! Master Code Table File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        Rem *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            Date$ = Date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        Rem *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            GoSub initialize_variables

            For fieldnr% = 1 To 1
L00820:         gosub'051(fieldnr%)        /* Default / Enables */
                      If Enabled% = 0 Then L00940
L00840:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      If keyhit% = 1 Then GoSub startover
                      If keyhit% <> 4 Then L00920
L00870:                  fieldnr% = Max(1, fieldnr% - 1)
                         gosub'051(fieldnr%)
                         If Enabled% = 1 Then L00840
                         If fieldnr% = 1 Then L00820
                         GoTo L00870
L00920:               If keyhit% = 16 And fieldnr% = 1 Then exit_sub
                      If keyhit% <> 0 Then L00840
L00940:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      If errormsg$ <> " " Then L00840
            Next fieldnr%

        Rem *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  If keyhit% = 1 Then GoSub startover
                  If keyhit% = 14 Then GoSub process_data
                  If keyhit% = 16 Then GoTo exit_sub
                  If keyhit% <> 0 Then editpg1
L01110: Rem FIELDNR% = CURSOR%(1%) - 4%
            fieldnr% = 1
            If fieldnr% < 1 Or fieldnr% > 1 Then editpg1
            If fieldnr% = lastfieldnr% Then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  If Enabled% = 0 Then editpg1
L01170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  If keyhit% = 1 Then GoSub startover
                  If keyhit% <> 0 Then L01170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  If errormsg$ <> " " Then L01170
                  lastfieldnr% = fieldnr%
            GoTo L01110

        Rem *************************************************************~
            *         C o n s o l i d a t e   B u f f e r               *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        process_data                     /* Note - Max = 50 Selections */
            cc_max% = 0
            init(" ") cc$()                 /* Pack the Codes Selected */
            For i% = 1 To c_max%
              If s$(i%) <> "X" Then GoTo L01390
                 cc_max% = cc_max% + 1
                 Str(cc$(cc_max%), 1, cc_len%) = Str(c$(i%), 1, cc_len%)
                 If cc_max% = 50 Then GoTo L01400
L01390:     Next i%
L01400: return clear all
        GoTo exit_sub

        Rem *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn '051(fieldnr%)
            Enabled% = 1
        Return

        Rem *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn '050(scrnr%, fieldnr%)
            If fieldnr% <> 0 Then L01640
                inpmessage$ = edtmessage$
                Return

L01640
*        Define the Input Message for the Screen/Field Indicated
            If scrnr% = 1 Then restore Line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            Return

        scrn1_msg  :  data                                               ~
         "Place an 'X' Beside all Applicable Codes?  (Note Max = 50)    "

        Rem *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$
            GoSub load_data
        Return

        Rem *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2
            call "STARTOVR" (u3%)
            If u3% = 1 Then Return
            return clear all
            GoTo inputmode

        Rem *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn '101(fieldnr%, edit%)
L02040:       gosub'050(1%, fieldnr%)
              GoSub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              if fieldnr% <> 0% then                                     ~
                               lfac$(fieldnr%) = hex(81) /* UPPER ONLY */

L02120:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), hdr$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), fac(hex(84)), hdr1$                  , ch(37),~
               at (03,40), fac(hex(84)), hdr1$                  , ch(37),~
               at (04,02), fac(hex(84)), hdr2$                  , ch(37),~
               at (04,40), fac(hex(84)), hdr2$                  , ch(37),~
                                                                         ~
               at (05,02), fac(lfac$(1%)), s$( 1%+i%)           , ch(01),~
               at (05,04), fac(hex(84)),   c$( 1%+i%)           , ch(35),~
               at (05,40), fac(lfac$(1%)), s$( 2%+i%)           , ch(01),~
               at (05,42), fac(hex(84)),   c$( 2%+i%)           , ch(35),~
                                                                         ~
               at (06,02), fac(lfac$(1%)), s$( 3%+i%)           , ch(01),~
               at (06,04), fac(hex(84)),   c$( 3%+i%)           , ch(35),~
               at (06,40), fac(lfac$(1%)), s$( 4%+i%)           , ch(01),~
               at (06,42), fac(hex(84)),   c$( 4%+i%)           , ch(35),~
                                                                         ~
               at (07,02), fac(lfac$(1%)), s$( 5%+i%)           , ch(01),~
               at (07,04), fac(hex(84)),   c$( 5%+i%)           , ch(35),~
               at (07,40), fac(lfac$(1%)), s$( 6%+i%)           , ch(01),~
               at (07,42), fac(hex(84)),   c$( 6%+i%)           , ch(35),~
                                                                         ~
               at (08,02), fac(lfac$(1%)), s$( 7%+i%)           , ch(01),~
               at (08,04), fac(hex(84)),   c$( 7%+i%)           , ch(35),~
               at (08,40), fac(lfac$(1%)), s$( 8%+i%)           , ch(01),~
               at (08,42), fac(hex(84)),   c$( 8%+i%)           , ch(35),~
                                                                         ~
               at (09,02), fac(lfac$(1%)), s$( 9%+i%)           , ch(01),~
               at (09,04), fac(hex(84)),   c$( 9%+i%)           , ch(35),~
               at (09,40), fac(lfac$(1%)), s$(10%+i%)           , ch(01),~
               at (09,42), fac(hex(84)),   c$(10%+i%)           , ch(35),~
                                                                         ~
               at (10,02), fac(lfac$(1%)), s$(11%+i%)           , ch(01),~
               at (10,04), fac(hex(84)),   c$(11%+i%)           , ch(35),~
               at (10,40), fac(lfac$(1%)), s$(12%+i%)           , ch(01),~
               at (10,42), fac(hex(84)),   c$(12%+i%)           , ch(35),~
                                                                         ~
               at (11,02), fac(lfac$(1%)), s$(13%+i%)           , ch(01),~
               at (11,04), fac(hex(84)),   c$(13%+i%)           , ch(35),~
               at (11,40), fac(lfac$(1%)), s$(14%+i%)           , ch(01),~
               at (11,42), fac(hex(84)),   c$(14%+i%)           , ch(35),~
                                                                         ~
               at (12,02), fac(lfac$(1%)), s$(15%+i%)           , ch(01),~
               at (12,04), fac(hex(84)),   c$(15%+i%)           , ch(35),~
               at (12,40), fac(lfac$(1%)), s$(16%+i%)           , ch(01),~
               at (12,42), fac(hex(84)),   c$(16%+i%)           , ch(35),~
                                                                         ~
               at (13,02), fac(lfac$(1%)), s$(17%+i%)           , ch(01),~
               at (13,04), fac(hex(84)),   c$(17%+i%)           , ch(35),~
               at (13,40), fac(lfac$(1%)), s$(18%+i%)           , ch(01),~
               at (13,42), fac(hex(84)),   c$(18%+i%)           , ch(35),~
                                                                         ~
               at (14,02), fac(lfac$(1%)), s$(19%+i%)           , ch(01),~
               at (14,04), fac(hex(84)),   c$(19%+i%)           , ch(35),~
               at (14,40), fac(lfac$(1%)), s$(20%+i%)           , ch(01),~
               at (14,42), fac(hex(84)),   c$(20%+i%)           , ch(35),~
                                                                         ~
               at (15,02), fac(lfac$(1%)), s$(21%+i%)           , ch(01),~
               at (15,04), fac(hex(84)),   c$(21%+i%)           , ch(35),~
               at (15,40), fac(lfac$(1%)), s$(22%+i%)           , ch(01),~
               at (15,42), fac(hex(84)),   c$(22%+i%)           , ch(35),~
                                                                         ~
               at (16,02), fac(lfac$(1%)), s$(23%+i%)           , ch(01),~
               at (16,04), fac(hex(84)),   c$(23%+i%)           , ch(35),~
               at (16,40), fac(lfac$(1%)), s$(24%+i%)           , ch(01),~
               at (16,42), fac(hex(84)),   c$(24%+i%)           , ch(35),~
                                                                         ~
               at (17,02), fac(lfac$(1%)), s$(25%+i%)           , ch(01),~
               at (17,04), fac(hex(84)),   c$(25%+i%)           , ch(35),~
               at (17,40), fac(lfac$(1%)), s$(26%+i%)           , ch(01),~
               at (17,42), fac(hex(84)),   c$(26%+i%)           , ch(35),~
                                                                         ~
               at (18,02), fac(lfac$(1%)), s$(27%+i%)           , ch(01),~
               at (18,04), fac(hex(84)),   c$(27%+i%)           , ch(35),~
               at (18,40), fac(lfac$(1%)), s$(28%+i%)           , ch(01),~
               at (18,42), fac(hex(84)),   c$(28%+i%)           , ch(35),~
                                                                         ~
               at (19,02), fac(lfac$(1%)), s$(29%+i%)           , ch(01),~
               at (19,04), fac(hex(84)),   c$(29%+i%)           , ch(35),~
               at (19,40), fac(lfac$(1%)), s$(30%+i%)           , ch(01),~
               at (19,42), fac(hex(84)),   c$(30%+i%)           , ch(35),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys (pfkeys$), Key(keyhit%)

               If keyhit% <> 2 Then GoTo L03110
                  i% = 0
                  GoTo L02040

L03110:        If keyhit% <> 3 Then GoTo L03160
L03120:           x% = Int(c_max% / 30)
                  i% = (x% * 30) + 1
                  GoTo L02040

L03160:        If keyhit% <> 4 Then GoTo L03220
                  If i% < 31 Then i% = 0
                  If i% = 0 Then GoTo L02040
                  i% = i% - 30
                  GoTo L02040

L03220:        If keyhit% <> 5 Then GoTo L03300
                  i% = i% + 30
                  If i% < c_max% Then GoTo L02040
                     x% = (c_max% / 30) - 1
                     If x% = 0 Then GoTo L03120
                     i% = x% * 30
                     GoTo L02040

L03300:        If keyhit% <> 15 Then GoTo L03340
                  call "PRNTSCRN"
                  GoTo L02120

L03340:        Close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               Return

        set_pf1
        if edit% = 2% then L03500      /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Prev Page           " &       ~
                      "                                       "
            pf$(2%) = "(2)First Page    (5)Next Page           " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page                            " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffffffffffffffff0f1000)
            GoSub check_screen
        Return

L03500: if fieldnr% > 0% then L03600   /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over    (4)Prev Page           " &       ~
                      "                       (14)Update Data "
            pf$(2%) = "(2)First Page    (5)Next Page           " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page                            " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffffffffffffff0e0f1000)
            GoSub check_screen
            Return
L03600:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over    (4)Prev Page           " &       ~
                      "                                       "
            pf$(2%) = "(2)First Page    (5)Next Page           " &       ~
                      "                                       "
            pf$(3%) = "(3)Last Page                            " &       ~
                      "                                       "
            pfkeys$ = hex(0102030405ffffffffffffffffffffff00)
            GoSub check_screen
            Return

        check_screen
            If c_max% > 30 Then GoTo L03780
               GoSub no_first
               GoSub no_next
               GoSub no_last
               GoSub no_prev
               Return
L03780:      If i% >= 30 Then GoTo L03810
                GoSub no_first
                GoSub no_prev
L03810:      If (i% + 30) <= c_max% Then GoTo L03830
                GoSub no_last
L03830:      If i% <= (c_max% - 30) Then GoTo L03850
                GoSub no_next
L03850: Return
        no_first
            Str(pf$(2), 1, 14) = " ": Str(pfkeys$, 2, 1) = Hex(ff)
        Return
        no_next
            Str(pf$(2), 18, 20) = " ": Str(pfkeys$, 5, 1) = Hex(ff)
        Return
        no_last
            Str(pf$(3), 1, 14) = " ": Str(pfkeys$, 3, 1) = Hex(ff)
        Return
        no_prev
            Str(pf$(1), 18, 20) = " ": Str(pfkeys$, 4, 1) = Hex(ff)
        Return

        Rem *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn '151(fieldnr%)
            errormsg$ = " "
            gosub L04110                         /* Product/Model Code */

        Return

L04110: Rem Selected Codes                       S$()

        Return

        load_data
            call "SHOSTAT" ("Loading Table Data")
            init(" ") s$(), c$()
            hdr$ = "Selecting Data From the XXXXXXXXX Table "
            hdr1$ = "S <---------Description ------------>"
            hdr2$ = "- -----------------------------------"
            Str(hdr$, 25, 9) = Table$
            i% , c_max% = 0
            code_key$ = " "
            Str(code_key$, 1, 9) = Table$
        load_next
            read #1,key > code_key$, using  L04280 , code_key$, desc$,     ~
                                                   eod goto load_done
L04280:        FMT CH(24), CH(30)
            If Str(code_key$, 1, 9) <> Table$ Then GoTo load_done
               c_max% = c_max% + 1
               If c_max% > 400 Then c_max% = 400
               Str(c$(c_max%), 1, cc_len%) = Str(code_key$, 10, cc_len%)
               Str(c$(c_max%), cc_len% + 1, 31) = "-" & Desc$
               GoTo load_next
        load_done
            cc_max% = 0
            for k% = 1% to 50%               /* Count Codes Selected */
              If Str(cc$(k%), 1, 1) = " " Then GoTo L04420
                 cc_max% = cc_max% + 1
            Next k%

L04420:     if cc_max% = 0% then return         /* Check Exiting Codes */
               For k% = 1 To c_max%
                   For j% = 1 To cc_max%
                     If Str(c$(k%), 1, cc_len%) <> cc$(j%) Then GoTo L04480
                        s$(k%) = "X"
                        GoTo L04490
L04480:            Next j%
L04490:        Next k%
        Return

        Rem *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_sub

        End

