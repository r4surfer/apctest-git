        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCCS3SB                             *~
            *  Creation Date     - 09/15/95                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - Screen Utility For the Selection of  *~
            *                      all Applicable Hardware\Packaging    *~
            *                      Codes for Costmate.                  *~
            *                                                           *~
            *  Special Comments  - EQ_FLAG% = 1% then Select Hardware   *~
            *                               = 2% then Select Packaging  *~
            *                                                           *~
            *                    - Hardware and Packaging Type Codes    *~
            *                      (3) and (4) are reserved for         *~
            *                      Costmate.                            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/15/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for New Release Upgrade to R6.04.03  ! RHH *~
            * 03/12/98 ! y2k checked                              ! DJD *~
            *************************************************************

        sub "APCCS3SB" (eq_flag%,        /* 1%=Hardware, 1%=Packaging  */~
                        apc_mod$,        /* Specified Model Code       */~
                        apc_color$,      /* Specified Color Code       */~
                        cc$(),           /* Selected Codes             */~
                        #1 )             /* APCCSTHP - Hardware/Package*/


        dim                                                              ~
            cc$(25%)15,                  /* Type & Raw Material No.    */~
            s$(100%)1,                   /* Selection Flag             */~
            c$(100%)35,                  /* Codes From Applicable Table*/~
            hdr1$37, hdr2$37, hdr$40,    /* Screen Headers             */~
            desc$19,                     /* Table Lookups              */~
            scan_key$20,sav_key$5,       /* Linealmate Primary Key     */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Costmate Utility for Hardware/Pack"
            pname$ = "APCCS3SB - Rev: R6.04"


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCCSTHP ! Hardware/Packaging                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 1%
L00880:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L01000
L00900:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L00980
L00930:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L00900
                         if fieldnr% = 1% then L00880
                         goto L00930
L00980:               if keyhit% = 16% and fieldnr% = 1% then exit_sub
                      if keyhit% <> 0% then       L00900
L01000:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L00900
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
                  if keyhit%  = 14% then gosub process_data
                  if keyhit%  = 16% then goto exit_sub
                  if keyhit% <>  0% then       editpg1
L01170: REM FIELDNR% = CURSOR%(1%) - 4%
            fieldnr% = 1%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L01230:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L01230
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L01230
                  lastfieldnr% = fieldnr%
            goto L01170

        REM *************************************************************~
            *         C o n s o l i d a t e   B u f f e r               *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        process_data                     /* Note - Max = 25 Selections */
            cc_max% = 0%                 /* Pack Codes in Array-No Hole*/
            init(" ") cc$()
            for i% = 1% to c_max%                  /* Type (1)         */
              if s$(i%) <> "X" then goto L01450      /* Raw Material (14)*/
                 cc_max% = cc_max% + 1%
                 cc$(cc_max%) = str(c$(i%),1%,15%)
                 if cc_max% = 25% then goto L01460
L01450:     next i%
L01460: return clear all
        goto exit_sub

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
            if fieldnr% <> 0% then L01700
                inpmessage$ = edtmessage$
                return

L01700
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Place an 'X' Beside all Applic Linealmate Eq's (Note Max = 25)"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$
            gosub load_data
        return

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

        deffn'101(fieldnr%, edit%)
L02100:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              if fieldnr% <> 0% then                                     ~
                               lfac$(fieldnr%) = hex(81) /* UPPER ONLY */

L02180:     accept                                                       ~
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
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L03170
                  i% = 0%
                  goto L02100

L03170:        if keyhit% <> 3% then goto L03220
L03180:           x% = int(c_max%/30.0)
                  i% = (x% * 30%) + 1%
                  goto L02100

L03220:        if keyhit% <> 4% then goto L03280
                  if i% < 31% then i% = 0%
                  if i% = 0% then goto L02100
                  i% = i% - 30%
                  goto L02100

L03280:        if keyhit% <> 5% then goto L03360
                  i% = i% + 30%
                  if i% < c_max% then goto L02100
                     x% = (c_max% / 30%) - 1%
                     if x% = 0% then goto L03180
                     i% = x% * 30%
                     goto L02100

L03360:        if keyhit% <> 15 then goto L03400
                  call "PRNTSCRN"
                  goto L02180

L03400:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L03560      /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Prev Page           " &       ~
                      "                                       "
            pf$(2%) = "(2)First Page    (5)Next Page           " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page                            " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffffffffffffffff0f1000)
            gosub check_screen
        return

L03560: if fieldnr% > 0% then L03660   /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over    (4)Prev Page           " &       ~
                      "                       (14)Update Data "
            pf$(2%) = "(2)First Page    (5)Next Page           " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page                            " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffffffffffffff0e0f1000)
            gosub check_screen
            return
L03660:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over    (4)Prev Page           " &       ~
                      "                                       "
            pf$(2%) = "(2)First Page    (5)Next Page           " &       ~
                      "                                       "
            pf$(3%) = "(3)Last Page                            " &       ~
                      "                                       "
            pfkeys$ = hex(0102030405ffffffffffffffffffffff00)
            gosub check_screen
            return

        check_screen
            if c_max% > 30% then goto L03840
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L03840:      if i% >= 30% then goto L03870
                gosub no_first
                gosub no_prev
L03870:      if (i% + 30%) <= c_max% then goto L03890
                gosub no_last
L03890:      if i% <= (c_max% - 30%) then goto L03910
                gosub no_next
L03910: return
        no_first
            str(pf$(2%),1%,14%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(2%),18%,20%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(3%),1%,14%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(1%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            gosub L04170                         /* Product/Model Code */

        return

L04170: REM Selected Codes                       S$()

        return

        load_data
            call "SHOSTAT" ("Loading Hardware/Packaging Data")
            init(" ") s$(), c$(), sav_key$, scan_key$
            hdr$  = "   Selecting Data From XXXXXXXXX File   "
            hdr1$ = "S TRaw Material  -<-- Description--->"
            hdr2$ = "- -----------------------------------"
            i%, c_max% = 0%
            if eq_flag% <> 1% then goto L04320
               str(scan_key$,1%,1%) = "0"
               str(hdr$,24%,9%) = "Hardware "
               goto L04350
L04320:     str(scan_key$,1%,1%) = "1"
            str(hdr$,24%,9%) = "Packaging"

L04350:     str(scan_key$,2%,3%) = apc_mod$
            str(scan_key$,5%,1%) = apc_color$
            sav_key$ = str(scan_key$,1%,5%)
        load_next
            read #1,key > scan_key$, using  L04410 , scan_key$,desc$,      ~
                                                   eod goto load_done
L04410:        FMT CH(20), CH(19)
            if str(scan_key$,1%,5%) <> sav_key$ then goto load_done
            if str(scan_key$,6%,1%) <> "3" and                           ~
               str(scan_key$,6%,1%) <> "4" then goto load_next

               c_max% = c_max% + 1%
               if c_max% > 100% then c_max% = 100%
               str(c$(c_max%),1%,15%) = str(scan_key$,6%,15%)
               str(c$(c_max%),16%,20%) = "-" & desc$
               goto load_next
        load_done
            cc_max% = 0%
            for k% = 1% to 25%
              if str(cc$(k%),1%,1%) = " " then goto L04580
                 cc_max% = cc_max% + 1%
            next k%

L04580:     if cc_max% = 0% then return         /* Check Exiting Codes */
               for k% = 1% to c_max%
                   for j% = 1% to cc_max%
                     if str(c$(k%),1%,15%) <> cc$(j%) then goto L04640
                        s$(k%) = "X"
                        goto L04650
L04640:            next j%
L04650:        next k%
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_sub

        end

