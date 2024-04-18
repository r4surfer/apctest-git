        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  BBBB   K   K  DDDD    AAA   TTTTT  RRRR   N   N   *~
            *  V   V  B   B  K  K   D   D  A   A    T    R   R  NN  N   *~
            *  V   V  BBBB   KKK    D   D  AAAAA    T    RRRR   N N N   *~
            *   V V   B   B  K  K   D   D  A   A    T    R   R  N  NN   *~
            *    V    BBBB   K   K  DDDD   A   A    T    R   R  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKDATRN - This subroutine allows entry of paired qty/date*~
            *            fields to automatically build PO lines by      *~
            *            copying the previous line and swapping in qty  *~
            *            and date info.                                 *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1995  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/25/95 ! Original                                 ! JDH *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "VBKDATRN" (vencode$,      /* Vendor Code                  */~
                        vendescr$,     /* Vendor Description           */~
                        part$,         /* Part Number                  */~
                        partdescr$,    /* Part Decription              */~
                        how_many%,     /* In - How many lines available*/~
                                       /* Out- How many lines to build */~
                        qty(),         /* Quantity Array               */~
                        date$(),       /* Date Array                   */~
                        #23,           /* PIPMASTR                     */~
                        #08,           /* HNYMASTR                     */~
                        #24,           /* SFCUM2                       */~
                        #33,           /* CALMASTR                     */~
                        #21,           /* PIPIN                        */~
                        #34,           /* PIPOUT                       */~
                        #35,           /* HNYDETAL                     */~
                        #36,           /* DEMMASTR                     */~
                        #25,           /* PIPCROSS                     */~
                        #02,           /* SYSFILE2                     */~
                        ret%)          /* 1% = Abort; Other = OK       */

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            col_hdr$79,                  /* Column Headers             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date$(36)8,                  /* Date Array                 */~
            date2$(36)8,                 /* Date Array                 */~
            dfac$(36)1, qfac$(36)1,      /* Field Attribute Characters */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Screen Line #2             */~
            line4$79,                    /* Screen Line #4             */~
            part$25,                     /* Part Number                */~
            partdescr$32,                /* Part Description           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            qty(36),                     /* Quantity Array             */~
            qty$(36)10,                  /* Quantity Array             */~
            qty2$(36)10,                 /* Quantity Array             */~
            userid$3,                    /* Current User Id            */~
            vencode$9,                   /* Vendor Code                */~
            vendescr$30                  /* Vendor Description         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            line2$ = "Vendor: " & vencode$ & "  (" & vendescr$ & ")"
            str(line2$,62) = "VBKDATRN: " & str(cms2v$,,8)
            line4$ = "Part: " & part$ & "  (" & partdescr$ & ")"
            col_hdr$  = "   Date       Quantity    " &                   ~
                        "   Date       Quantity    " &                   ~
                        "   Date       Quantity    "

            how_many% = min(how_many%, 36%)
            if how_many% > 0% then L10000
                call "ASKUSER" (2%, "NO LINES LEFT", "The Date Run " &   ~
                                "function requires at least one line",   ~
                                "to be available on the PO.  This PO " & ~
                                "is maxed out.", "Press RETURN to " &    ~
                                "acknowledge...")
                keyhit% = 32%
                goto datasave

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
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
L10210:               if keyhit% = 32% then datasave
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
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 12% then editpg1
            fieldnr% = 1%
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
            *             S A V E   D A T A   I N   A R R A Y           *~
            *-----------------------------------------------------------*~
            * Ending routine smashes and returns.                       *~
            *************************************************************

        datasave
            ret% = 0%   /* OK */
            if keyhit% <> 32% then L19100
L19080:         ret% = 99%   /* Don't build lines */
                goto exit_program
L19100
*        OK, almost done
            gosub smash_arrays
            if how_many% = 0% then L19080
            for i% = 1% to how_many%
                if date$(i%) = " " or date$(i%) = blankdate$ then L19200
                convert qty$(i%) to qty(i%)
                next i%
L19200:     goto exit_program

        smash_arrays
            c% = 0%
            for i% = 1% to how_many%
                if date$(i%) = " " or date$(i%) = blankdate$ then L19570
                     c% = c% + 1%
                     date2$(c%) = date$(i%)
                     qty2$(c%)  = qty$(i%)
L19570:         next i%
            how_many% = c%
            mat date$ = date2$
            mat qty$  = qty2$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100
            return
L20100: REM Def/Enable Date & Quantity
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
         "Enter Dates and Quantities                                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      date$(), qty$()
            mat qty = zer
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1995  an unpublished work by CAELUS,       *~
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

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            init(hex(8c)) dfac$(), qfac$()
            if fieldnr% < 1% then L40095
                for i% = 1% to how_many%
                    dfac$(i%) = hex(81)             /* Upper Only */
                    qfac$(i%) = hex(82)             /* Numeric    */
                    next i%

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Multiple PO Line Entry by Date",                      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c)), line4$                 , ch(79),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), fac(hex(ac)), col_hdr$               , ch(79),~
                                                                         ~
               at (07,05), fac(dfac$(01%)), date$(01%)          , ch(08),~
               at (07,14), fac(qfac$(01%)), qty$(01%)           , ch(10),~
               at (07,31), fac(dfac$(02%)), date$(02%)          , ch(08),~
               at (07,40), fac(qfac$(02%)), qty$(02%)           , ch(10),~
               at (07,57), fac(dfac$(03%)), date$(03%)          , ch(08),~
               at (07,66), fac(qfac$(03%)), qty$(03%)           , ch(10),~
                                                                         ~
               at (08,05), fac(dfac$(04%)), date$(04%)          , ch(08),~
               at (08,14), fac(qfac$(04%)), qty$(04%)           , ch(10),~
               at (08,31), fac(dfac$(05%)), date$(05%)          , ch(08),~
               at (08,40), fac(qfac$(05%)), qty$(05%)           , ch(10),~
               at (08,57), fac(dfac$(06%)), date$(06%)          , ch(08),~
               at (08,66), fac(qfac$(06%)), qty$(06%)           , ch(10),~
                                                                         ~
               at (09,05), fac(dfac$(07%)), date$(07%)          , ch(08),~
               at (09,14), fac(qfac$(07%)), qty$(07%)           , ch(10),~
               at (09,31), fac(dfac$(08%)), date$(08%)          , ch(08),~
               at (09,40), fac(qfac$(08%)), qty$(08%)           , ch(10),~
               at (09,57), fac(dfac$(09%)), date$(09%)          , ch(08),~
               at (09,66), fac(qfac$(09%)), qty$(09%)           , ch(10),~
                                                                         ~
               at (10,05), fac(dfac$(10%)), date$(10%)          , ch(08),~
               at (10,14), fac(qfac$(10%)), qty$(10%)           , ch(10),~
               at (10,31), fac(dfac$(11%)), date$(11%)          , ch(08),~
               at (10,40), fac(qfac$(11%)), qty$(11%)           , ch(10),~
               at (10,57), fac(dfac$(12%)), date$(12%)          , ch(08),~
               at (10,66), fac(qfac$(12%)), qty$(12%)           , ch(10),~
                                                                         ~
               at (11,05), fac(dfac$(13%)), date$(13%)          , ch(08),~
               at (11,14), fac(qfac$(13%)), qty$(13%)           , ch(10),~
               at (11,31), fac(dfac$(14%)), date$(14%)          , ch(08),~
               at (11,40), fac(qfac$(14%)), qty$(14%)           , ch(10),~
               at (11,57), fac(dfac$(15%)), date$(15%)          , ch(08),~
               at (11,66), fac(qfac$(15%)), qty$(15%)           , ch(10),~
                                                                         ~
               at (12,05), fac(dfac$(16%)), date$(16%)          , ch(08),~
               at (12,14), fac(qfac$(16%)), qty$(16%)           , ch(10),~
               at (12,31), fac(dfac$(17%)), date$(17%)          , ch(08),~
               at (12,40), fac(qfac$(17%)), qty$(17%)           , ch(10),~
               at (12,57), fac(dfac$(18%)), date$(18%)          , ch(08),~
               at (12,66), fac(qfac$(18%)), qty$(18%)           , ch(10),~
                                                                         ~
               at (13,05), fac(dfac$(19%)), date$(19%)          , ch(08),~
               at (13,14), fac(qfac$(19%)), qty$(19%)           , ch(10),~
               at (13,31), fac(dfac$(20%)), date$(20%)          , ch(08),~
               at (13,40), fac(qfac$(20%)), qty$(20%)           , ch(10),~
               at (13,57), fac(dfac$(21%)), date$(21%)          , ch(08),~
               at (13,66), fac(qfac$(21%)), qty$(21%)           , ch(10),~
                                                                         ~
               at (14,05), fac(dfac$(22%)), date$(22%)          , ch(08),~
               at (14,14), fac(qfac$(22%)), qty$(22%)           , ch(10),~
               at (14,31), fac(dfac$(23%)), date$(23%)          , ch(08),~
               at (14,40), fac(qfac$(23%)), qty$(23%)           , ch(10),~
               at (14,57), fac(dfac$(24%)), date$(24%)          , ch(08),~
               at (14,66), fac(qfac$(24%)), qty$(24%)           , ch(10),~
                                                                         ~
               at (15,05), fac(dfac$(25%)), date$(25%)          , ch(08),~
               at (15,14), fac(qfac$(25%)), qty$(25%)           , ch(10),~
               at (15,31), fac(dfac$(26%)), date$(26%)          , ch(08),~
               at (15,40), fac(qfac$(26%)), qty$(26%)           , ch(10),~
               at (15,57), fac(dfac$(27%)), date$(27%)          , ch(08),~
               at (15,66), fac(qfac$(27%)), qty$(27%)           , ch(10),~
                                                                         ~
               at (16,05), fac(dfac$(28%)), date$(28%)          , ch(08),~
               at (16,14), fac(qfac$(28%)), qty$(28%)           , ch(10),~
               at (16,31), fac(dfac$(29%)), date$(29%)          , ch(08),~
               at (16,40), fac(qfac$(29%)), qty$(29%)           , ch(10),~
               at (16,57), fac(dfac$(30%)), date$(30%)          , ch(08),~
               at (16,66), fac(qfac$(30%)), qty$(30%)           , ch(10),~
                                                                         ~
               at (17,05), fac(dfac$(31%)), date$(31%)          , ch(08),~
               at (17,14), fac(qfac$(31%)), qty$(31%)           , ch(10),~
               at (17,31), fac(dfac$(32%)), date$(32%)          , ch(08),~
               at (17,40), fac(qfac$(32%)), qty$(32%)           , ch(10),~
               at (17,57), fac(dfac$(33%)), date$(33%)          , ch(08),~
               at (17,66), fac(qfac$(33%)), qty$(33%)           , ch(10),~
                                                                         ~
               at (18,05), fac(dfac$(34%)), date$(34%)          , ch(08),~
               at (18,14), fac(qfac$(34%)), qty$(34%)           , ch(10),~
               at (18,31), fac(dfac$(35%)), date$(35%)          , ch(08),~
               at (18,40), fac(qfac$(35%)), qty$(35%)           , ch(10),~
               at (18,57), fac(dfac$(36%)), date$(36%)          , ch(08),~
               at (18,66), fac(qfac$(36%)), qty$(36%)           , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 8% then L40680
                  call "PIPATCSB" (part$, #23, #08, #24, #33, #21, #34,  ~
                                   #35, #36, #25)
                  goto L40095

L40680:        if keyhit% <> 13 then L40700
                  call "MANUAL" ("VBKDATRN") : goto L40095

L40700:        if keyhit% <> 15 then L40730
                  call "PRNTSCRN" : goto L40095

L40730:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40950     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (8)See ATC             " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                     (32)Abort Date Run"
            pfkeys$ = hex(01ffffffffffff08ffffffff0dff0f2000)
            return

L40950: if fieldnr% > 0% then L41040  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (8)See ATC             " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "   (32)Abort Date Run  (16)Create Lines"
            pfkeys$ = hex(01ffffffffffff08ffffffff0dff0f102000)
            return
L41040:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (8)See ATC             " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffff08ffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "

        REM Test for Date & Quantity
            for i% = 1% to 36%
                if date$(i%) = " " and date$(i%) <> blankdate$ ~
                          and qty$(i%) <> " " then             ~
                     errormsg$ = "No Date for Quantity (" & qty$(i%) & ")"
                if date$(i%) <> " " and date$(i%) <> blankdate$ ~
                          and qty$(i%) = " " then              ~
                     errormsg$ = "No Quantity for " & date$(i%)
                if errormsg$ <> " " then return
                if date$(i%) = " " or date$(i%) = blankdate$ then L50340

                convert qty$(i%) to qty(i%), data goto L50180
                goto L50190
L50180:              errormsg$ = qty$(i%) & " is not a number"
                     return
L50190:         if qty(i%) > 0 then L50210
                     errormsg$ = "Quantity (" & qty$(i%) & ") must be " &~
                                 "greater than zero"
                     return
L50210:         qty(i%) = round(qty(i%), 2)
                call "CONVERT" (qty(i%), 2.2, qty$(i%))

                call "DATEOK" (date$(i%), u3%, errormsg$)
                if errormsg$ <> " " then return
                temp$ = date$(i%)
                call "DATUNFMT" (temp$)
                call "PIPINDEX" (#2, temp$, u3%, err%)
                if err% = 0% then L50340
                     errormsg$ = date$(i%) & " is outside the planning "&~
                                 "calendar"
                     return
L50340:         next i%
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
            *  Copyright (c) 1995  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program

            end
