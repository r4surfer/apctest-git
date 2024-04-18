        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  X   X  TTTTT   CCC   PPPP   Y   Y  IIIII  N   N   *~
            *    T     X X     T    C   C  P   P  Y   Y    I    NN  N   *~
            *    T      X      T    C      PPPP    YYY     I    N N N   *~
            *    T     X X     T    C   C  P        Y      I    N  NN   *~
            *    T    X   X    T     CCC   P        Y    IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTCPYIN - This program manages Copy Elements and their   *~
            *            text.  Copy elements may be copied into other  *~
            *            text when in the subroutine TXTINSUB.          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/12/85 ! ORIGINAL                                 ! ERN *~
            * 03/23/92 ! Rehost.  Text file may allow 1 - 28 lines! KAB *~
            *          !   of text per record.  Monitors record   !     *~
            *          !   length to determine how many.          !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankline$79,                /* A blank line               */~
            copyid$10,                   /* Copy Element ID            */~
            cursor%(2),                  /* Cursor location for edit   */~
            descr$30,                    /* Copy Element Description   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            filler$(20)100,              /* Spaces                     */~
            filler64$64,                 /* Spaces                     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            msg$79,                      /* TXTINSUB Message           */~
            text$(56,1)70,               /* Text Array                 */~
            textid$4                     /* Text ID pointer            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
        REM *************************************************************
            mat f2% = con

                     /* The variables F2%() and AXD$() should not be   */
                     /* modified.   They are an intrinsic part of the  */
                     /* file opening routines.                         */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! TXTFILE  ! System Text File                         *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  11                      ~


*          CALL "OPENFILE" (#1,  "SHARE", F2%(1 ), RSLT$(1 ), AXD$(1 ))
*             Note- File will be created in TXTINSUB if required.
*             NOTE- No it won't. KAB
            rslt$(1 ) = "REQUIRED"
            call "OPENCHCK" (#1, fs%, f2%(1 ), 0%, rslt$(1 ))
            if fs% < 0% then L65000

            call "GETUFBRS" addr(#1, filler64$)
            textsize% = val(str(filler64$,,2), 2)
            textsize% = textsize% - 64%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************


            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value and Press (ENTER)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, copyid$, descr$
            width% = 70%

            for fieldnr% = 1 to  3
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editmode
L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then       delete_text
                  if keyhit%  =  9 then       edit_text
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 2 or fieldnr% >  3 then L11060
            if new% = 0% and fieldnr% = 3%   then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *                 E D I T   T E X T                         *~
            * --------------------------------------------------------- *~
            * Call text management subroutine to allow maintenance of   *~
            * text.  Upon return, save header and toss text from        *~
            * buffer to master.                                         *~
            *************************************************************
        edit_text
            str(text$(1,1),1,1) = hex(01)  /* Pass width to subrtn     */
            convert width% to str(text$(1,1),2,2), pic(00)
            msg$ = "Manage Copy Element " & copyid$ & ", '" & descr$& "'"
            call "TXTINSUB" (#1, f2%(1), "CPY", msg$, textid$, text$())

            gosub save_data1   /* Save copy element header             */

            gosub save_data2   /* Toss Text from buffer to Master      */

            goto inputmode


        REM *************************************************************~
            *                 D E L E T E    T E X T                    *~
            * --------------------------------------------------------- *~
            * Kill this sucker if confirmed.                            *~
            *************************************************************
        delete_text
L13060:     gosub'112
                if keyhit%  =  1% then editmode
                if keyhit% <> 16% then L13060

            delete% = 1%
            gosub save_data1
            gosub save_data2
            delete% = 0%

            goto inputmode


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves Copy Header.                                        *~
            *************************************************************

        datasave
            gosub save_data1
            goto inputmode


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Copy Element ID  */~
                                    L20200,         /* Copy Element Dscr*/~
                                    L20300          /* Text Line Width  */
                     return
L20100:     REM Default/Enable for Copy Element ID
                inpmessage$ = "Enter code to use for later retrieval" &  ~
                              " of the text."
                return
L20200:     REM Default/Enable for Copy Element Description
                inpmessage$ = "Enter description of this copy element."
                return
L20300:     REM Default/Enable for Text Line Width
                inpmessage$ = "Enter maximum width of text allowed " &   ~
                              " (20 - 70 characters)."
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29918:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L29942, L29948, L29952
               return

L29942:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L29952:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L29918

        REM *************************************************************~
            *             L O A D   D A T A                             *~
            * --------------------------------------------------------- *~
            * Load in first record of text to get established width.    *~
            *************************************************************
        load_data
            if f1%(1) = 1% then L30120    /* F1% from PLOWCODE          */
                width%  = 70%
                new%    = 1%
                textid$ = hex(ffffffff)
                return

L30120
*        Get elements from header record (already in buffer)
            new% = 0%
            get #1 using L30140, descr$, textid$, width%
L30140:         FMT XX(11), CH(30), CH(4), BI(1)

*        See if there is any text
            plowkey$ = "M   " & str(textid$) & "C" & hex(0001)
            call "READ100" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then new% = 1%  /* Allows changing width    */
            if f1%(1) = 1% then                                          ~
                call "TXTFUTIL" (#1, f2%(1), "LOAD", textid$)
            return clear all
            goto editmode


        REM *************************************************************~
            *          S A V E   D A T A    R O U T I N E S             *~
            * --------------------------------------------------------- *~
            * Routines to (1) Save Copy Element header and (2) Text.    *~
            *************************************************************

        save_data1   /* Save Copy Element header                       */
            plowkey$ = "C" & str(copyid$)
            call "DELETE" (#1, plowkey$, 11%)
            if delete% = 1% then return
            put filler64$ using L31160, "C", copyid$, descr$, textid$,    ~
                                        width%, " "
L31160:         FMT CH(1), CH(10), CH(30), CH(4), BI(1), CH(18)
            write #1, str(filler64$,,64), str(filler$(),,textsize%)
            return


        save_data2   /* Toss Text from Buffer to Master                */
            if delete% = 1% then                                         ~
                call "TXTFUTIL" (#1, f2%(1), "XOUT", textid$)
            call "TXTFUTIL" (#1, f2%(1), "TOS2", textid$)

            return


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Inputs Document for the first time.                       *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40160,         /* Copy Element ID  */~
                                    L40130,         /* Copy Element Dscr*/~
                                    L40190          /* Text Line Width  */
                     goto L40230

L40130:           REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L40160:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
L40190:           REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L40230:   accept                                                         ~
            at (01,02), "Manage Copy Element Text",                      ~
            at (03,02), fac(hex(a4)), blankline$                , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Copy Element ID",                               ~
            at (06,30), fac(lfac$( 1)), copyid$                 , ch(10),~
            at (07,02), "Copy Element Description",                      ~
            at (07,30), fac(lfac$( 2)), descr$                  , ch(30),~
            at (08,02), "Text Line Width",                               ~
            at (08,30), fac(lfac$( 3)), width%, pic(0#),                 ~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "P.F. KEYS ACTIVE:",                             ~
            at (23,03), "(1)START OVER",                                 ~
            at (22,65), "(13)INSTRUCTIONS",                              ~
            at (23,65), "(15)PRINT SCREEN",                              ~
            at (24,65), "(16)EXIT PROGRAM",                              ~
                                                                         ~
                keys(hex(00010d0f10)),                                   ~
                key (keyhit%)

                if keyhit% <> 13 then L40510
                     call "MANUAL" ("TXTCPYIN")
                     goto L40230

L40510:         if keyhit% <> 15 then return
                     call "PRNTSCRN"
                     goto L40230

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 1 of document.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41130,         /* Copy Element ID  */~
                                    L41130,         /* Copy Element Dscr*/~
                                    L41190          /* Text Line Width  */
                     goto L41230

L41130:           REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
L41190:           REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L41230:   accept                                                         ~
            at (01,02), "Manage Copy Element Text",                      ~
            at (03,02), fac(hex(a4)), blankline$                , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Copy Element ID",                               ~
            at (06,30), fac(lfac$( 1)), copyid$                 , ch(10),~
            at (07,02), "Copy Element Description",                      ~
            at (07,30), fac(lfac$( 2)), descr$                  , ch(30),~
            at (08,02), "Text Line Width",                               ~
            at (08,30), fac(lfac$( 3)), width%, pic(0#),                 ~
                                                                         ~
            at (21,02), fac(hex(a4)),   edtmessage$             , ch(79),~
            at (22,02), "P.F. KEYS ACTIVE:",                             ~
            at (23,03), "(1)START OVER      (9)EDIT TEXT   (12)DELETE",  ~
            at (22,65), "(13)INSTRUCTIONS",                              ~
            at (23,65), "(15)PRINT SCREEN",                              ~
            at (24,65), "(16)SAVE DATA",                                 ~
                                                                         ~
                keys(hex(0001090c0d0f10)),                               ~
                key (keyhit%)

                if keyhit% <> 13 then L41510
                     call "MANUAL" ("TXTCPYIN")
                     goto L41230

L41510:         if keyhit% <> 15 then L41550
                     call "PRNTSCRN"
                     goto L41230

L41550:         close ws
                call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
                u3% = u3%
                return

        REM *************************************************************~
            *      C O N F I R M    D E L E T I O N                     *~
            * --------------------------------------------------------- *~
            * Get Confirmation be deleting text.                        *~
            *************************************************************

        deffn'112
            init(hex(84)) lfac$()
            inpmessage$ = "PRESS PF-16 TO CONFIRM DELETE -OR- PF-1" &    ~
                          " TO RETURN TO EDIT MODE."

L41820:   accept                                                         ~
            at (01,02), "Manage Copy Element Text",                      ~
            at (03,02), fac(hex(a4)), blankline$                , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Copy Element ID",                               ~
            at (06,30), fac(lfac$( 1)), copyid$                 , ch(10),~
            at (07,02), "Copy Element Description",                      ~
            at (07,30), fac(lfac$( 2)), descr$                  , ch(30),~
            at (08,02), "Text Line Width",                               ~
            at (08,30), fac(lfac$( 3)), width%, pic(0#),                 ~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "P.F. KEYS ACTIVE:",                             ~
            at (23,03), "(1)EXIT DELETE",                                ~
            at (22,65), "(13)INSTRUCTIONS",                              ~
            at (23,65), "(15)PRINT SCREEN",                              ~
            at (24,65), "(16)DELETE TEXT ",                              ~
                                                                         ~
                keys(hex(00010d0f10)),                                   ~
                key (keyhit%)

                if keyhit% <> 13 then L42080
                     call "MANUAL" ("TXTCPYIN")
                     goto L41820

L42080:         if keyhit% <> 15 then return
                     call "PRNTSCRN"
                     goto L41820


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Copy Element ID  */~
                                    L50200,         /* Copy Element Dscr*/~
                                    L50300          /* Text Line Width  */
                     return

L50100:     REM Test Data for Copy Element ID
                plowkey$ = "C" & copyid$
                call "PLOWCODE" (#1, plowkey$, descr$, 1%, .3, f1%(1))
                copyid$ = str(plowkey$,2)
                if copyid$ <> " " then L50170
                     errormsg$ = "Copy ID may not be blank."
                     return
L50170:         gosub load_data
                return

L50200:     REM Test Data for Copy Element Description
                if descr$ <> " " then return
                     errormsg$ = "Description may not be blank."
                     return

L50300:     REM Test Data for Text Line Width
                if width% >= 20 and width% <= 70 then return
                     errormsg$ = "Width must be between 20 and 70."
                     return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")

            end
