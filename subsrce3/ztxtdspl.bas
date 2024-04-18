        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  X   X  TTTTT  DDDD    SSSS  PPPP   L      Y   Y   *~
            *    T     X X     T    D   D  S      P   P  L       Y Y    *~
            *    T      X      T    D   D   SSS   PPPP   L        Y     *~
            *    T     X X     T    D   D      S  P      L        Y     *~
            *    T    X   X    T    DDDD   SSSS   P      LLLLL    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTDSPLY - Subroutine to allow displaying of text. See    *~
            *            below for more notes.                          *~
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
            * 08/28/85 ! ORIGINAL                                 ! ERN *~
            * 04/19/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! KAB *~
            *          !   Also Minor bug in print logic.         !     *~
            * 03/23/92 ! Rehost.  Text file may allow 1 - 28 lines! KAB *~
            *          !   of text per record.  Monitors record   !     *~
            *          !   length to determine how many.          !     *~
            * 02/11/93 ! Minor Mods for UNIX.                     ! JDH *~
            * 02/14/94 ! Happy Valentine. No conditional on OPENCHK JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "ZTXTDSPL"    (#1, f201%,          /* TXTFILE & Status */~
                               srce$,              /* Source Code      */~
                               srcemsg$,           /* Screen Message   */~
                               textid$,            /* TEXT TO MAINTAIN */~
                               text$() )           /* TEXT ARRAY       */

*        CALLING ARGUMENTS NOTES
*          FILE CHANNEL -     The subroutine will open TXTFILE.
*                             F201% should be the F2%() element in the
*                             caller.
*
*          SRCE$         -    The source code assigned to identify the
*                             caller.  This code is used to determine
*                             which Text Types are available to the
*                             operator. CH(3).
*
*          SRCEMSG$      -    This element appears in the header of the
*                             screen to provide continuity between the
*                             caller and the maintenance routine. CH(79).
*
*          TEXTID$       -    The internal code used to represent the
*                             text.  Must be supplied.
*
*
*
*          TEXT$()       -    The MATRICES to maintain the text in.
*                             This is passed from the caller to allow
*                             some control over the amount of memory used
*                             (and therefore, the amount of text that may
*                             be entered).  The matrix size should be
*                             defined as the largest line width allowed
*                             for the source times the number of lines
*                             allowed (in multiples of 28). IF TOO SMALL,
*                             YOU ARE IN TROUBLE!!!!!


        dim                                                              ~
            errormsg$79,                 /* Error message              */~
            filler64$64,                 /* Record Header Area         */~
            find$16,                     /* Find Text (as entered)     */~
            find1$16,                    /* Find Text (actual search)  */~
            flip$(24)80,                 /* Screen area for 'flip'     */~
            inpmessage$79,               /* Informational Message      */~
            plowkey$100,                 /* Multi-purpose Plow key     */~
            readkey$100,                 /* Multi-purpose Read key     */~
            srce$3,                      /* Definition of caller       */~
            srcedescr$25,                /* Source Description         */~
            srcemsg$79,                  /* Message from calling prgm  */~
            tabs$10,                     /* Tab stop positions         */~
            text$(100,1)50,              /* Text being maintained      */~
            textid$4,                    /* Text ID (internal)         */~
            type$1,                      /* Text Type being maintained */~
            typedescr$25,                /* Type Description           */~
            types$(10)1,                 /* Valid types for Source     */~
            typesdescr$(10)25,           /* Descriptions of Text Types */~
            user$3,                      /* Current User ID            */~
            w%(1),                       /* Work array for search      */~
            widths%(10),                 /* Text widths (per Type)     */~
            work70$70,                   /* Work String                */~
            work$(28)70,                 /* Work Array                 */~
            xiosw$8,                     /* WSXIO- I/O Word Status Byte*/~
            xorder1$4,                   /* WSXIO- Write Order Var.    */~
            xorder2$4,                   /* WSXIO- Read  Order Var.    */~
            xorder3$4,                   /* WSXIO- I/O for Types Selc  */~
            xscrn$(24)80                 /* WSXIO- Screen Array        */

        dim f1%(02)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f1% = con

                     /* The variables F1%() and AXD$() should not be   */
                     /* modified.   They are an intrinsic part of the  */
                     /* file opening routines.                         */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! TXTFILE  ! Standard Text File                       *~
            * #5  ! CRTFILE  ! Work Station                             *~
            *************************************************************~
            *       FILE SELECTION AND OPEN CALLS                       *

*          SELECT #1,  "TXTFILE",                                       ~
*                      VARC,                                            ~
*                      INDEXED,                                         ~
*                      RECSIZE = 2024,                                  ~
*                      KEYPOS =     1, KEYLEN = 11

            select #5,  "CRTFILE",                                       ~
                        consec, recsize = 1924

*        Open TXTFILE.

          call "OPENCHCK" (#1, openstatus%, f201%, 0%, " ")
          if openstatus% < 0% then L65090
             call "GETUFBRS" addr(#1, filler64$)
             textsize% = val(str(filler64$,,2), 2)
             textsize% = textsize% - 64%
             textincr% = textsize% / 70%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            if len(str(textid$)) <> 4% then L65070

*        Check TEXT$ for "no text" yet values.
            if str(textid$) = hex(00000000) or                           ~
               str(textid$) = hex(20202020) then textid$ = all(hex(ff))
            if textid$ = hex(ffffffff) then L65110

*        First make sure that the SOURCE is A-OK. If not, bye-bye!
            if srce$ = "CPY" then srcedescr$ = "Manage Copy Text"
            if srce$ = "CPY" then L09220
            if srce$ = " " then L65020
            readkey$ = "S" & srce$
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then L65020
            get #1 using L09200, srcedescr$
L09200:         FMT XX(11), CH(25)

L09220
*        Next, load in any TYPES for this source.
            types% = 0%
            init (" ") types$(), typesdescr$()
            if srce$ <> "CPY" then L09250
                types%         = 1%
                types$(1)      = "C"
                typesdescr$(1) = "Copy Element"
                widths%(1)     = 70%
                goto L09360
L09250:     plowkey$ = "T" & str(srce$) & hex(00)
L09260:     call "PLOWNEXT" (#1, plowkey$, 4%, f1%(1))
            if f1%(1) = 0% and types% = 0% then L65030
            if f1%(1) = 0% then L09360
                types% = types% + 1%
                get #1 using L09320, types$(types%), typesdescr$(types%),  ~
                                   widths%(types%)
L09320:              FMT XX(4), CH(1), XX(6), CH(25), BI(1)
                goto L09260


L09360
*        Now get an image of the current screen.  This may be used later
*        if a 'screen flip' is requested. After we get it, we clean it
*        up just a little bit for the redisplay.
            close ws
            call "SCREEN" addr ("C", u3%, "I", flip$(), f1%())


*        Set some variables and get ready for the show.
            matrows% = dim(text$(), 1)
            matcols% = len(str(text$(1,1)))
            matsize% = matrows% * matcols%
            call "EXTRACT" addr("ID", user$)
            pass_width% = -1%
            if str(text$(1,1),,1) <> hex(01) then L09550
                convert str(text$(1,1),2,2) to pass_width%, data goto L09550
L09550:     xorder3$ = " "

*        Set up Workstation for WSXIO.
            close ws
            call "WSXIO" addr("O", 255%, "Y", #5, u3%)

        REM *************************************************************~
            * E N T E R   T E X T   T Y P E                             *~
            * --------------------------------------------------------- *~
            * Get which text type to maintain text for.  User places    *~
            * cursor at desired type and simply presses return key.     *~
            *************************************************************

        enter_text_type
            init(" ") errormsg$, inpmessage$, find$
            temp% = 1% : if types% = 1% then L10170

L10120:     gosub types_screen
                if keyhit%  = 16% then L65000
            temp% = cursor1% - 5%
            if temp% < 1% or temp% > types% then L10120

L10170:     type$       = types$(temp%)
            typedescr$  = typesdescr$(temp%)
            type_width% = widths%(temp%)


        REM *************************************************************~
            * S E T - U P   F O R   T E X T   M A I N T A N C E         *~
            * --------------------------------------------------------- *~
            * This section of code performs the steps required before   *~
            * the text display may execute.  These steps include--      *~
            *         (1) INITIALIZATION of variables;                  *~
            *         (2) SCREEN format definition    -and-             *~
            *         (3) Loading of text if on file.                   *~
            *************************************************************

            init (" ") errormsg$, inpmessage$, text$()

            gosub init_screen

*        Set-up Screen header lines.
                xscrn$( 1)      = hex(8c) & "Display text:"        &     ~
                                  hex(84) & srcedescr$ & hex(8c)   &     ~
                                  "type"  & hex(84)    & typedescr$
            str(xscrn$( 1),70)  = hex(a4)
                xscrn$( 2)      = hex(ac) & srcemsg$

*        Now, load the text (if any) from the text file.

            gosub read_text

            gosub set_tabs


        REM *************************************************************~
            * M A I N   S C R E E N - O P T I O N S   S E L E C T I O N *~
            * --------------------------------------------------------- *~
            * This little section of code controls the main display and *~
            * its options.                                              *~
            *************************************************************

        options_screen
            gosub load_screen_with_text
            gosub set_pf_options
            gosub text_screen_io
                if keyhit%  =  1% then gosub reselect_type
                if keyhit%  =  1% then       options_screen
                if keyhit% <=  7% then gosub move_screen
                if keyhit%  =  8% then       find_text
                if keyhit%  = 14% then gosub show_column
                if keyhit%  = 14% then       options_screen
                if keyhit%  = 16% then       L65000
                if keyhit%  = 17% then gosub flip_screen
                if keyhit%  = 31% then       print_text
            errormsg$ = " "
            goto options_screen

        REM *************************************************************~
            * F L I P   S C R E E N                                     *~
            * --------------------------------------------------------- *~
            * Show screen from caller.                                  *~
            *************************************************************
        flip_screen
            errormsg$ = " "
*        Display Screen
            xorder1$ = hex(01a00101)
            call "WSXIO" addr("X", #5, hex(80), xorder1$, flip$(),       ~
                              1920%, xiosw$)

*        Read Screen
            call "WSXIO" addr("X", #5, hex(50), xorder1$, xscrn$(),      ~
                              0%, xiosw$)
            xorder1$ = xorder2$
            return

        REM *************************************************************~
            * F I N D   T E X T                                         *~
            * --------------------------------------------------------- *~
            * Locate text string requested.                             *~
            *************************************************************
        find_text
            errormsg$ = " "
            if lastline% <> 0% then L13300
                errormsg$ = "There is no text to search."
                goto options_screen

L13300:     if case% <> 2% then case% = 3%
            errormsg$, inpmessage$ = " "
            sl% = 3% :  sr% = 9% :  tl% = top% : tr% = 1%
            first% = 1%

            str(xscrn$(1), 71) = "  FIND"
            xscrn$(20) = hex(8c) &                                       ~
                "(1)EXIT  (RETURN)Find Next  (8)Find Previous"
            xscrn$(21) = hex(8c)
            xscrn$(22) = hex(8c) &                                       ~
                "Find Text:" & hex(a0) & str(find$) & hex(8c)
            xscrn$(23) = hex(8c) & "(2) Case Sensitive Search"
            xscrn$(24) = hex(8c) & "(3) Case Insensitive Search"

        find_text1                       /* DISPLAY LOOP POINT         */
            gosub load_screen_with_text
            str(xscrn$(23),,1), str(xscrn$(24),,1) = hex(8c)
            if case% = 2% then  str(xscrn$(23),,1) = hex(84) else        ~
                                str(xscrn$(24),,1) = hex(84)
            if first% <> 1% then L13520
                xorder1$ = hex(01a00d16)
                goto L13540
L13520:     xorder1$ = hex(01a0) & bin(sr%) & bin(sl%)

L13540:     gosub text_screen_io
                if keyhit%  =  0% then L13650
                if keyhit%  =  1% then errormsg$ = " "
                if keyhit%  =  1% then restore%  = 1%
                if keyhit%  =  1% then options_screen
                if keyhit%  =  2% then case% = 2%
                if keyhit%  =  3% then case% = 3%
                if keyhit%  <  4% then find_text1
                if keyhit%  =  8% then L13650
            goto L13540

L13650:     errormsg$ = " "
            find$ = str(xscrn$(22), 13, 16)
            if case% = 3% then find1$ = find$ or all(hex(20))            ~
                          else find1$ = find$
            if keyhit% = 8% then L13790
                step% = 1%  :  end%  = lastline%           /* Forward  */
                if first% = 1% then L13750
                     tr% = tr% + 1%
                     if tr% < width% then L13860
                          tl% = tl% + 1% : tr% = 1%
L13750:                   if tl% < lastline% then L13860
L13760:                        errormsg$ = "No more occurances."
                               goto find_text1

L13790:         step% = -1%  :  end%  = 1%                 /* Backward */
                if first% = 1% then L13840
                     tr% = tr% - 1%
                     if tr% >= 1% then L13860
                          tl% = tl% - 1% : tr% = width%
L13840:                   if tl% >= 1% then L13860 else goto L13760

L13860:     for l% = tl%  to end%  step step%
                work70$ = text$(l%,1)
                if case% = 3% then work70$ = or all (hex(20))
                if step% = 1% then                                       ~
                     search  str(work70$,  tr%) = find1$ to w%()         ~
                             else                                        ~
                     search -str(work70$,1,tr%) = find1$ to w%()
                if w%(1) <> 0% then L13990
                if step% = 1% then tr% = 1% else tr% = width%
            next l%
            tl% = savetl%  :  tr%  = savetr%
            goto L13760         /* No more message  */

L13990:     first% = 0%
            if step% = -1% then L14040
                sr% = w%(1) + tr% + 7%  : tr% = w%(1) + tr% - 1%
                goto L14060

L14040:         sr% = w%(1) + 8%        : tr% = w%(1)

L14060:     if l% > top% + 14% or l% < top% then top% = l%
            sl% = l% - top% + 3%
            tl% = l%
            savetl% = tl%  :  savetr%  = tr%
            goto find_text1


        REM *************************************************************~
            * P R I N T    T E X T                                      *~
            * --------------------------------------------------------- *~
            * A little quickie print routine to get this onto paper.    *~
            *************************************************************
        print_text
            errormsg$ = " "
            if lastline% > 0% then L15100
                errormsg$ = "There is no text to print."
                goto options_screen

L15100:     xscrn$(19) = hex(a4) & "TEXT PRINTING IN PROGRESS...."
            xorder1$ = hex(13201301)
            call "WSXIO" addr("X", #5, hex(80), xorder1$, xscrn$(19),    ~
                              80%, xiosw$)

            select printer(80)
            l% = 857%
            init ("-") work70$

            for i% = 1% to lastline%
                if l% < 55% then L15280
                     print page
                     print "TEXT PRINT FOR USER: "; user$
                     print "SOURCE: "; srcedescr$; "   TYPE: "; typedescr$
                     print srcemsg$
                     print str(work70$,,4); "  "; work70$
                     l% = 5%

L15280:         print using L15350, i%, text$(i%,1)
                l% = l% + 1%
            next i%

            close printer
            goto options_screen

L15350: %####  ##########################################################~
        ~############


        REM *************************************************************~
            * S H O W   C O L U M N                                     *~
            * --------------------------------------------------------- *~
            * Set ERRORMSG$ = current column of cursor in text.         *~
            *************************************************************
        show_column
            errormsg$ = " "
            if cursor1% < 3% or cursor1% > 17% then L15640
            col% = cursor2% - 8%
            if col% < 1% or col% > width% then L15640
            convert col% to errormsg$, pic(#0)
            errormsg$ = "Column position = " & errormsg$
            return

L15640:     errormsg$ = "Cursor is outside of text area."
            return


        REM *************************************************************~
            * S C R E E N   A S S I S T A N C E   R O U T I N E S       *~
            * --------------------------------------------------------- *~
            * Subroutines to aid in setting the screen up, defining PF  *~
            * keys and so on and so forth.                              *~
            *************************************************************

        load_screen_with_text
*        Moves text into the screen array, sets FACS for options screen.

*          IF TOP% < 1% OR TOP% > LASTLINE% - 14% THEN TOP% = 1%
            for l% = 3% to 17%           /* Clear text area            */
                xscrn$(l%) = hex(8c)
            next l%
            l% = 3%
            if lastline% = 0% then L20220
            for x% = top% to min(top% + 14%, lastline%)
                str(xscrn$(l%),6) = hex(8c06)
                str(xscrn$(l%),8) = hex(84) & str(text$(x%,1)) &         ~
                                    hex(8c) & hex(06)
                l% = l% + 1%
            next x%
L20220:   return


        adjust_move_pfs
*        Removes unnecessary screen movement PF keys from available
*        PF-key area.  Obviously, these must be inserted before coming
*        here.
            if top% <> 1% then L20330
                     str(xscrn$(21), 2,8) = " "     /* 2- 1st    */
                     str(xscrn$(22), 2,8) = " "     /* 4- Prev   */
                     str(xscrn$(23), 2,8) = " "     /* 6- Down   */
L20330:     if top% < lastline% - 14% then return
                     str(xscrn$(21),11,8) = " "     /* 3- Last   */
                     str(xscrn$(22),11,8) = " "     /* 5- Next   */
                     str(xscrn$(23),11,8) = " "     /* 7- Last   */
            return


        init_screen  /* Clear Screen array and set 1st pos = HEX(8C)   */
            for x% = 1% to 24%
                xscrn$(x%) = hex(8c)
            next x%
            restore% = 2%
          return


        set_message_lines   /* Add error and input messages to screen  */
            xscrn$(18) = hex(8c)
            if errormsg$ <> " " then xscrn$(18) = hex(94) & errormsg$
            if str(errormsg$,,6) = "Column" then                         ~
                                             str(xscrn$(18),,1) = hex(84)
            xscrn$(19) = hex(a4) & inpmessage$
          return


        move_screen
*        Handle move PF keys (2-7) and attempt to keep cursor on the
*        same text as the movement occurs.
            errormsg$ = " "
            sline%   = cursor1% + top% - 3%
            savetop% = top%
            restore% = 0%
                if keyhit%  =  0% then options_screen
                if keyhit%  =  2% then top% = 1%
                if keyhit%  =  3% then top% = max(1%, lastline% - 14%)
                if keyhit%  =  4% then top% = max(1%, top% - 14%)
                if keyhit%  =  5% then top% = min(top% + 14%,            ~
                                              max(1, lastline% - 14%))
                if keyhit%  =  6% then top% = max(1%, top% - 1%)
                if keyhit%  =  7% then top% = min(lastline% - 7%,        ~
                                                             top% + 1%)
                if top% < 1% then top% = 1%
            if sline% < top% or sline% > top% + 14% then L21080
                savetop% = savetop% - top%
                str(xorder2$,4,1) = bin(cursor1% + savetop%)
                return

L21080:         str(xorder2$,3,2) = hex(0903)      /* Reset to 1st     */
                return


        set_tabs
            call "EXTRACT" addr("S#", str(tabs$,,6))
            convert str(tabs$,,6) to tabs%, data goto L21200
            tabs$ = hex(09131d27313b45000000)
            tabs% = 2% + (width% / 10%)
            str(tabs$,tabs%) = all(hex(00))
            if order2$ = " " then order1$ = hex(01a00903) else           ~
                                  order1$ = order2$
            call "WSXIO" addr("X", #5, hex(84), order1$, tabs$, 10%,     ~
                              xiosw$)
L21200:     return


*       ***** PF KEY AREA DEFINITION ROUTINES **************************
*        These routines define the PF key area for the various screens.*
*        They are stuck down here to keep the code above a bit more    *
*        straight forward.                                             *
*       ****************************************************************


        set_pf_options    /* Setup PF keys for the OPTIONS screen.     */
            xscrn$(20) = hex(8c) & "(1)Reselect Type  ( 8)Find Text    "&~
                         "    (14)Cursor Pos.   (13)Show Instructions "
            xscrn$(21) = hex(8c) & "(2)First (3)Last                   "&~
                         "    (17)Flip Screen   (15)Print Screen      "
            xscrn$(22) = hex(8c) & "(4)Prev  (5)Next                   "&~
                         "                      (31)Print Text        "
            xscrn$(23) = hex(8c) & "(6)Down  (7)Up                     "&~
                         "                      (16)EXIT TEXT DISPLAY "
            xscrn$(24) = hex(8c) & "                                   "&~
                         "                                            "
            if types%  = 1% then str(xscrn$(20),2,16) = " "
            gosub adjust_move_pfs
            if restore% = 0% then xorder1$ = xorder2$
            if restore% = 1% then xorder1$ = hex(01a0) &bin(sr%) &bin(sl%)
            if restore% = 2% then xorder1$ = hex(01a00903)
            restore% = 0%
            str(xscrn$(1), 71) = " OPTIONS "
            inpmessage$ = " "
          return


        REM *************************************************************~
            * R E S E L E C T   T Y P E                                 *~
            * --------------------------------------------------------- *~
            * A little clean up before we return to the Type selection  *~
            * screen.                                                   *~
            *************************************************************
        reselect_type
            if types% = 1% then return
            errormsg$ = " "
            return clear all
            goto enter_text_type


        REM *************************************************************~
            * F I L E   I / O,  M I S C.   S U B R O U T I N E S        *~
            * --------------------------------------------------------- *~
            * Some of the routines for accessing data to/from disk, etc *~
            *************************************************************

        read_text
*        Read text from the work area.
*

            top% = 1%
            if textid$ <> hex(ffffffff) then L30170
L30120:         lastline% = 1%
                width% = type_width%
                if pass_width% > 0% then width% = pass_width%
                gosub redim_matrix
                text$(1,1) = "** NO TEXT ENTERED **"
                return

L30170:     readkey$ = "M" & "   "      & str(textid$) & type$ & hex(0001)
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then L30120
                get #1 using L30270, lastline%, width%
L30270:              FMT XX(14), BI(2), BI(1)
                gosub redim_matrix

*          Now read the text into the matrices.
                plowkey$ = str(readkey$,,9) & hex(0000)
                i% = 0%
L30320:         call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
                if f1%(1) = 0% then return
                get #1, str(filler64$), str(work$(),,textsize%)
                for ti% = 1% to textincr%
                    i% = i% + 1%
                    if i% > rows% then L30390
                       text$(i%, 1%) = work$(ti%)
L30390:         next ti%
                          FMT XX(64), 28*CH(70)
                goto L30320


        redim_matrix
            rows% = matsize% / width%
        /*  ROWS% = 28% * (ROWS% / 28%) No Longer Needed */
            mat redim  text$(rows%, 1)width%
            return


        REM *************************************************************~
            * T Y P E S   S C R E E N                                   *~
            * --------------------------------------------------------- *~
            * Allows User to specify which type of text to maintain.    *~
            *************************************************************
        types_screen
            gosub init_screen
            xscrn$(1) = hex(8c) & "Text Display for "    & hex(84) &     ~
                        srcedescr$
            xscrn$(2) = hex(ac) & srcemsg$
            str(xscrn$(5), 10) = "Types Available for this source are:"
            for w% = 1% to 10%
                if types$(w%) = " " then L40150
                     str(xscrn$(w%+5%), 19) = hex(860b8c) & " " &        ~
                                          str(typesdescr$(w%)) & hex(8c)
                     readkey$ = "M   " & str(textid$) & types$(w%) &     ~
                                hex(0001)
                     call "READ100" (#1, readkey$, f1%(1))
                     if f1%(1) = 0% then str(xscrn$(w%+5%),19,1) = hex(8c)
L40150:     next w%
            inpmessage$ = "TAB cursor to Text Type desired and press" &  ~
                          " RETURN."
            gosub set_message_lines
            str(xscrn$(20), 59) = "(13) Show Instructions"
            str(xscrn$(21), 59) = "(15) Print Screen     "
            str(xscrn$(23), 59) = "(16) RETURN TO CALLER "

L40220
*        Display Screen
            if xorder3$ = " " then xorder3$ = hex(01a01406)
            call "WSXIO" addr("X", #5, hex(80), xorder3$, xscrn$(),      ~
                              1920%, xiosw$)

*        Read Screen
            xorder3$ = hex(01a00000)
            call "WSXIO" addr("X", #5, hex(50), xorder3$, xscrn$(),      ~
                              0%, xiosw$)

*        Determine Cursor position and PF key hit
            keyhit% = val(str(xiosw$,3%),1) - 64%
            if keyhit% > 16% then keyhit% = val(str(xiosw$,3%),1) - 80%
            cursor1% = val(str(xorder3$,4),1)
            cursor2% = val(str(xorder3$,3),1)

            if keyhit% <> 13% then L40450
                call "WSXIO" addr("C", #5)
                close ws
                call "ZMANUAL" ("TXTDSPLY")
                close ws
                call "WSXIO" addr("O", 255%, "Y", #5, u3%)
                goto L40220

L40450:     if keyhit% <> 15% then return
                call "WSXIO" addr("C", #5)
                close ws
                call "ZPRNTSCR"
                call "WSXIO" addr("O", 255%, "Y", #5, u3%)
                goto L40220


        REM *************************************************************~
            * T E X T   S C R E E N   D I S P L A Y                     *~
            * --------------------------------------------------------- *~
            * Displays screen with text and handles input.              *~
            *************************************************************
        text_screen_io
            gosub set_message_lines

L41100
*        Display Screen
            call "WSXIO" addr("X", #5, hex(80), xorder1$, xscrn$(),      ~
                              1920%, xiosw$)

*        Read Screen
            xorder2$ = hex(01a00000)
            call "WSXIO" addr("X", #5, hex(50), xorder2$, xscrn$(),      ~
                              1920%, xiosw$)

*        Determine Cursor position and PF key hit
            keyhit% = val(str(xiosw$,3%),1) - 64%
            if keyhit% > 16% then keyhit% = val(str(xiosw$,3%),1) - 80%
            cursor1% = val(str(xorder2$,4),1)
            cursor2% = val(str(xorder2$,3),1)

            if keyhit% <> 13% then L41300
                call "WSXIO" addr("C", #5)
                close ws
                call "ZMANUAL" ("TXTDSPLY")
                close ws
                call "WSXIO" addr("O", 255%, "Y", #5, u3%)
                gosub set_tabs
                goto L41100

L41300:     if keyhit% <> 15% then return
                call "WSXIO" addr("C", #5)
                close ws
                call "ZPRNTSCR"
                call "WSXIO" addr("O", 255%, "Y", #5, u3%)
                goto L41100

*       ****   E X I T S   **********************************************
L65000: mat redim text$(matrows%, 1)matcols%   /* Normal Exit point    */
        call "WSXIO" addr("C", #5)
        end

*       *** ERROR EXITS ****
L65020: errormsg$ = "Source '" & srce$ & "' not on file."   :  goto L65300
L65030: errormsg$ = "No TYPES found under Source '" & srce$ & "'."
                          goto L65300
L65070: errormsg$ = "TEXT ID MUST BE PASSED IN W/ LENGTH OF 4!!!!!!"
                          goto L65300
L65090: errormsg$ = "File TXTFILE not found."
                          goto L65300
L65110: errormsg$ = "There is no text entered for this element."
                          goto L65300
L65300: inpmessage$ = "Text can't be displayed due to the above reason."&~
                      " Press RETURN to continue."
        accept                                                           ~
            at(01,02), "TEXT DISPLAY ROUTINE",                           ~
            at(02,02), fac(hex(8c)), srcemsg$,                           ~
            at(03,02), fac(hex(a4)), errormsg$,                          ~
            at(21,02), fac(hex(a4)), inpmessage$

            close ws
            end
