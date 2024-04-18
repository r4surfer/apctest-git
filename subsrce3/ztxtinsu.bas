        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  X   X  TTTTT  IIIII  N   N   SSSS  U   U  BBBB    *~
            *    T     X X     T      I    NN  N  S      U   U  B   B   *~
            *    T      X      T      I    N N N   SSS   U   U  BBBB    *~
            *    T     X X     T      I    N  NN      S  U   U  B   B   *~
            *    T    X   X    T    IIIII  N   N  SSSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTINSUB - Subroutine to allow entry and modification of  *~
            *            text.  The routine is intended to be general   *~
            *            enough to satisfy most text requirements that  *~
            *            exist in CMS. See below for more notes.        *~
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
            * 03/23/92 ! Rehost.  Text file may allow 1 - 28 lines! KAB *~
            *          !   of text per record.  Monitors record   !     *~
            *          !   length to determine how many.          !     *~
            * 04/01/92 ! Don't assign a Text ID if no text        ! MRM *~
            * 03/11/93 ! Implement clipboard cut, copy, paste     ! MRM *~
	    * 06/27/96 ! Removed square brackets '[]' from PFkeys ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "ZTXTINSU"    (#1, f201%,          /* TXTFILE & Status */~
                               srce$,              /* Source Code      */~
                               srcemsg$,           /* Screen Message   */~
                               textid$,            /* TEXT TO MAINTAIN */~
                               text$() )           /* TEXT ARRAY       */

*        CALLING ARGUMENTS NOTES
*          FILE CHANNEL -     The subroutine will open and, if required,
*                             create TXTFILE.  F201% should be the F2%()
*                             element in the caller.
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
*                             text.  If the text to maintain is new the
*                             variable should be set to all HEX(FF).
*                             BI(4).
*
*          TEXT$()       -    The MATRICES to maintain the text in.
*                             This is passed from the caller to allow
*                             some control over the amount of memory used
*                             (and therefore, the amount of text that may
*                             be entered).  The matrix size should be0007
*                             for the source times the number of lines
*                             allowed (in multiples of 28).
*


        dim                                                              ~
            copy$10,                     /* Copy member ID             */~
            copyid$4,                    /* Copy member Text ID        */~
            end$5,                       /* Ending line for delete     */~
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
            start$5,                     /* Starting Line for delete   */~
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
            work$(28)70,                 /* Work array to read in copy */~
            work2$2,                     /* 2 Character work string    */~
            work70$70,                   /* Work String                */~
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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f1% = con

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

*        Open TXTFILE.  Create if not found.

          if openstatus% = 0% then                                       ~
             call "OPENCHCK" (#1, openstatus%, f201%, 0%, " ")
          if openstatus% < 0% then L65010

            call "GETUFBRS" addr(#1, readkey$)
            textsize% = val(str(readkey$,,2), 2)
            textsize% = textsize% - 64%
            textincr% = textsize% / 70%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            if len(str(textid$)) <> 4% then L65070

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

*        Check TEXT$ for "no text" yet values.
            if str(textid$) = hex(00000000) or                           ~
               str(textid$) = hex(20202020) then textid$ = all(hex(ff))

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
            * the text management routines may execute.  These steps    *~
            * include (1) INITIALIZATION of variables;                  *~
            *         (2) SCREEN format definition    -and-             *~
            *         (3) Loading of text if on file.                   *~
            *************************************************************
        set_up
            init (" ") errormsg$, inpmessage$, text$()

            gosub init_screen

*        Set-up Screen header lines.
                xscrn$( 1)      = hex(8c) & "Edit Text for"        &     ~
                                  hex(84) & srcedescr$ & hex(8c)   &     ~
                                  "type"  & hex(84)    & typedescr$
            str(xscrn$( 1),70)  = hex(a4)
                xscrn$( 2)      = hex(ac) & srcemsg$

*        Now, load the text (if any) from the text file. If no value
*        is found in TEXTID$, set up for a new guy.
            gosub read_text

            gosub set_tabs


        REM *************************************************************~
            * M A I N   S C R E E N - O P T I O N S   S E L E C T I O N *~
            * --------------------------------------------------------- *~
            * This little section of code controls the main display and *~
            * its plethora of options. Lord help the person who has to  *~
            * add a PF key into this mess.                              *~
            *************************************************************

        options_screen
            gosub load_screen_with_text
            gosub set_pf_options
            gosub text_screen_io
                if keyhit%  =  1% then gosub startover
                if keyhit% <=  7% then gosub move_screen
                if keyhit%  =  8% then       find_text
                if keyhit%  =  9% then       modify_text
                if keyhit%  = 10% then       extrnl_copy
                if keyhit%  = 11% then       insert_line
                if keyhit%  = 12% then       delete_line
                if keyhit%  = 14% then gosub show_column
                if keyhit%  = 14% then       options_screen
                if keyhit%  = 16% then       datasave
                if keyhit%  = 17% then gosub flip_screen
                if keyhit%  = 18% then restore% = 2%
                if keyhit%  = 19% then str(xorder2$,3,1) = bin(09)
                if keyhit%  = 20% then str(xorder2$,3,1) = bin(8+width%)
                if keyhit%  = 26% then paste_from_clipboard
                if keyhit%  = 27% then copy_to_clipboard
                if keyhit%  = 28% then cut_to_clipboard
                if keyhit%  = 31% then       print_text
                if keyhit%  = 32% then       datasave
            errormsg$ = " "
            goto options_screen

        REM *************************************************************~
            * I N S E R T   L I N E                                     *~
            * --------------------------------------------------------- *~
            * Allow insertation of a line at the cursor position.       *~
            * After first line is inserted, keep going until told to    *~
            * stop.                                                     *~
            *************************************************************
        insert_line
            errormsg$ = " "
            if lastline% < rows% then L11620
                errormsg$ = "There is no room to insert a line."
                goto options_screen

L11620:     sline% = cursor1% - 2%       /* Text line on screen.       */
                if sline% <  1% then sline% =  1%
                if sline% > 15% then sline% = 15%
            line% = sline% + top% - 1%   /* Line # in Text             */

         insert_line2                    /* LOOP POINT                 */
            if line% > lastline% then line% = lastline% + 1%
            cursor1% = line% - top% + 3% /* Now = screen line          */
            if cursor1% < 16% then L11730
                top% = top% + 1%         /* Set for insert near bottom */
                cursor1% = cursor1% - 1%
L11730:     gosub push_lines
            gosub load_screen_with_text
            str(xscrn$(cursor1%),,1) = hex(84)

            gosub set_pf_insert_line
            saverow% = cursor1%          /* Save screen input line     */
            str(xscrn$(cursor1%),8,1) = hex(80)
                     text$(line%,1) = str(xscrn$(saverow%),9,width%)
L11810:     gosub text_screen_io
                str(xscrn$(cursor1%),8,1) = hex(8c)
                if keyhit% = 14% then gosub show_column
                if keyhit% = 14% then       L11810

                text$(line%,1) = str(xscrn$(saverow%),9,width%)
                if keyhit% = 1% or lastline% >= rows% then               ~
                                                        exit_insert_line
                     line% = line% + 1%
                     goto insert_line2

        exit_insert_line
            if lastline% <= rows% then L11930
                errormsg$ = "There is no more room for inserts."
                goto L11940
L11930:     gosub pop_line
L11940:     goto  options_screen

        REM *************************************************************~
            * M O D I F Y   T E X T                                     *~
            * --------------------------------------------------------- *~
            * Allow modification of text (straight overstrike).         *~
            *************************************************************
        modify_text
            errormsg$ = " "
            if lastline% > 0% then L12110
                errormsg$ = "There is no text to modify. Use Insert" &   ~
                            " options first."
                goto options_screen

L12110:     errormsg$, inpmessage$ = " "

L12130
*        Enable lines that have valid text.
            gosub load_screen_with_text
            work2$ = " "
            for l% = 3% to 17%
                if str(xscrn$(l%),2,4) = "Add-" then L12200
                if str(xscrn$(l%),2,4) = "EOT-" then L12200
                     str(xscrn$(l%), 1, 1) = hex(84)
                     str(xscrn$(l%), 8, 1) = hex(80)
L12200:     next l%

            gosub set_pf_modify
L12230:     gosub text_screen_io
                if keyhit%  =  1% then errormsg$ = " "
                if keyhit%  =  1% then options_screen
                if keyhit%  < 18% or keyhit% > 20% then L12310
                     on keyhit% - 17% gosub center_scrn, r_justify_scrn, ~
                                            l_justify_scrn
                     if work2$ <> " " then L12130  /* Error encountered */
                     xorder1$ = xorder2$
                     goto L12230
L12310:         if keyhit%  = 14% then gosub show_column
                if keyhit%  = 14% then       L12230

                if keyhit% <=7% or keyhit% = 16% then L12350 else L12230
L12350:         gosub load_text_from_screen
                if keyhit%  =  0% then options_screen
                if keyhit%  >= 2% or keyhit% <= 7% then gosub move_screen
                errormsg$ = " "
                goto L12130


        REM *************************************************************~
            * CENTER, RIGHT & LEFT JUSTIFY OPTIONS                      *~
            * --------------------------------------------------------- *~
            * These routines allow the User to Center, Right Justify or *~
            * Left Justify text.  The text handled is at and to the     *~
            * right of the cursor location.                             *~
            *************************************************************
        center_scrn     :  work2$ = "CT"  :  goto crl_scrn_common
        r_justify_scrn  :  work2$ = "RJ"  :  goto crl_scrn_common
        l_justify_scrn  :  work2$ = "LJ"  :  goto crl_scrn_common

        crl_scrn_common
            errormsg$ = " "
            l% = cursor1%  :  r% = cursor2% - 8%
            if l% < 3% or l% > 17%          then crl_err_exit
            if str(xscrn$(l%),2,4) = "Add-" then crl_err_exit
            if str(xscrn$(l%),2,4) = "EOT-" then crl_err_exit
            if r% < 1% or r% >= width%      then crl_err_exit

            len% = width% - r% + 1%  :  r% = r% + 8%
            call "STRING" addr(work2$, str(xscrn$(l%), r%, len%), len%)
            work2$ = " "
            return


        crl_err_exit
            errormsg$ = "Please move cursor before selecting option."
            return


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
            * D E L E T E   L I N E ( S )                               *~
            * --------------------------------------------------------- *~
            * This routine allows deleting of a single line of text,    *~
            * a group of lines, or all text.                            *~
            *************************************************************
        delete_line
            errormsg$ = " "
            if lastline% > 0% then L14310
                errormsg$ = "There is no text to delete."
                goto options_screen

L14310
*        Translate cursor position into default starting point.
            sl% = cursor1%  :  sr% = cursor2%  :  restore% = 1%
            tl% = sl% + top% - 3%
            if tl% < top% or tl% > lastline% then tl% = 0%
            convert tl% to start$, pic(#####) : end$ = " "
            if tl% = 0% then start$ = " "
            for p% = 1% to 5%
                if str(start$,,1) = " " then start$ = str(start$,2)
            next p%

L14390
*        Display screen and get line range for delete.
            gosub load_screen_with_text
            gosub set_pf_delete_line
            gosub text_screen_io
                if keyhit%  =  1% then errormsg$ = " "
                if keyhit%  =  1% then options_screen
                if keyhit%  =  2% then top% = 1%
                if keyhit%  =  3% then top% = max(1%, lastline% - 14%)
                if keyhit%  =  4% then top% = max(1%, top% - 14%)
                if keyhit%  =  5% then top% = min(top% + 14%,            ~
                                              max(1, lastline% - 14%))
                if keyhit%  =  6% then top% = max(1%, top% - 1%)
                if keyhit%  =  7% then top% = min(lastline% - 7%,        ~
                                                             top% + 1%)
                if keyhit% <>  0% then L14390

            errormsg$ = " "
            start$ = str(xscrn$(22),39,5) : end$ = str(xscrn$(23),39,5)
            if start$ <> "ALL" then L14620
L14570:         init(" ") text$()
                lastline% = 0%
                top%      = 1%
                restore%  = 2%
                goto options_screen

L14620:     if start$ <> "FIRST" then L14640
                start% = 1%  :  goto L14690
L14640:     convert start$ to start%, data goto L14660
            if start% >= 1% and start% <= lastline% then L14690
L14660:         errormsg$ = "Invalid entry for Starting Line."
                goto L14390

L14690:     if end$ <> "LAST"  then L14710
                end% = lastline%  :  goto L14740
L14710:     if end$ <> " " then L14730
                end% = start%     :  goto L14740
L14730:     convert end$ to end%, data goto L14750
L14740:     if end% >= start% and end% <= lastline% then L14780
L14750:         errormsg$ = "Invalid entry for Ending Line."
                goto L14390

L14780
*        Now delete the specified range of lines.
            if start% = 1% and end% = lastline% then L14570   /* ALL    */
            if end% <> lastline% then L14880
L14810:         for l% = start% to lastline%
                     text$(l%,1) = " "
                next l%
                lastline% = max(0%, start% - 1%)
                if top% >= lastline% then restore% = 2%
                if top% >= lastline% then top% = max(1%, lastline% - 14%)
                goto options_screen

L14880:     tl% = start%
            for l% = end% + 1%  to  lastline%
                text$(tl%,1) = text$(l%,1)
                tl% = tl% + 1%
            next l%
            start% = tl%  :  end% = lastline%
            goto L14810


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
            if str(xscrn$(cursor1%),2,4) = "Add-" then L15640
            if str(xscrn$(cursor1%),2,4) = "EOT-" then L15640
            col% = cursor2% - 8%
            if col% < 1% or col% > width% then L15640
            convert col% to errormsg$, pic(#0)
            errormsg$ = "Column position = " & errormsg$
            return

L15640:     errormsg$ = "Cursor is outside of text area."
            return


        REM *************************************************************~
            * E X T E R N A L   C O P Y                                 *~
            * --------------------------------------------------------- *~
            * Allow User to copy text from "Copy Master" into this text.*~
            * Insert point is defined by cursor position on screen.     *~
            *************************************************************
        extrnl_copy
            copy$, errormsg$ = " "
            sl% = cursor1%  :  sr% = cursor2%  :  restore% = 1%
            if sl% < 3% or sl% > 17%      then L16110
            tl% = sl% + top% - 3%
            if tl% < 1% or tl% > lastline% + 1% then L16110  else L16122
L16110:         errormsg$ = "Position cursor at insert point first."
                goto options_screen

L16122
*        Screw up screen to indicate where copy text will go
            for l% = sl% to 17%
                xscrn$(l%) = hex(84) & "Copy" & hex(8c)
            next l%

L16140:     gosub set_pf_extrnl_copy
            gosub text_screen_io
                if keyhit%  =  1% then errormsg$ = " "
                if keyhit%  =  1% then       options_screen
                if keyhit% <>  0% then       L16140
            errormsg$ = " "

*        Now try to execute copy.
            copy$ = str(xscrn$(22),40,10)
            readkey$ = "C" & copy$
            call "WSXIO" addr("C", #5)
            close ws
            call "ZPLOWCOD" (#1, readkey$, " ", 1%, .3, f1%(1))
            close ws  :  call "WSXIO" addr("O", 255%, "Y", #5, u3%)
                         gosub set_tabs
            if f1%(1) = 1% then L16260
                errormsg$ = "Copy element not on file."
                goto L16140
L16260:     get #1 using L16270, copyid$
L16270:         FMT XX(11), XX(30), CH(4)
            copy$ = str(readkey$,2,10)
            readkey$ = "M" & "   " & str(copyid$) & "C" & hex(0001)
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 1% then L16330
L16310:         errormsg$ = "There is no text in the copy element."
                goto L16140
L16330:     get #1 using L16335, copylines%, copywidth%
L16335:         FMT XX(14), BI(2), BI(1)
            if copylines% = 0% then L16310
            if copywidth% = width% then L16390
                errormsg$ = " "
                put errormsg$ using L16380, copywidth%, width%
L16380:              %Copy width (##) differs from text width (##).
L16390:     if copylines% <= rows% - lastline% then L16440
                errormsg$ = " "
                temp% = copylines% - (rows% - lastline%)
                put errormsg$ using L16420, temp%
L16420:              %Not enough room. #### more lines are required.
                if temp% = 1% then errormsg$ =                           ~
                     "Not enough room. 1 more line is required."
                goto L16140
L16440:
*        Seems plausible --  Get confirmation if required.
            if errormsg$ = " " then L16510
            gosub set_pf_extrnl_copy2
L16470:     gosub text_screen_io
                if keyhit%  =  1%  then       options_screen
                if keyhit% <>  0%  then       L16470

L16510
*        Perform the copy and then return to options screen.
*         First scoot Text array down to make room for copy member.
            errormsg$ = " "
            if tl% > lastline%  then L16560
            for l% = lastline%  to  tl%  step - 1%
                text$(l% + copylines%,1) = text$(l%,1)
                text$(l%,1) = " "
            next l%
L16560
*         Now read text into the open space.
            plowkey$ = "M" & "   " & str(copyid$) & "C" & hex(0000)
            lastline% = lastline% + copylines%
            i% = tl% - 1%  :  copylines% = tl% + copylines% - 1%
L16580:     call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
            if f1%(1) = 0% then options_screen
            get #1, str(filler64$,,64), str(work$(),,textsize%)
                FMT XX(64), CH(1960)
            for l% = 1% to textincr%
                i% = i% + 1% : if i% > copylines% then options_screen
                text$(i%,1) = work$(l%)
            next l%
            goto L16580

        REM *************************************************************~
            * C O P Y   L I N E ( S )   T O   C L I P B O A R D         *~
            * --------------------------------------------------------- *~
            * This routine allows copying of a single line of text,     *~
            * a group of lines, or all text to the user's clipboard     *~
            *************************************************************
        copy_to_clipboard
            errormsg$ = " "
            if lastline% > 0% then L17310
                errormsg$ = "There is no text to copy."
                goto options_screen

L17310
*        Translate cursor position into default starting point.
            sl% = cursor1%  :  sr% = cursor2%  :  restore% = 1%
            tl% = sl% + top% - 3%
            if tl% < top% or tl% > lastline% then tl% = 0%
            convert tl% to start$, pic(#####) : end$ = " "
            if tl% = 0% then start$ = " "
            for p% = 1% to 5%
                if str(start$,,1) = " " then start$ = str(start$,2)
            next p%

L17390
*        Display screen and get line range for delete.
            gosub load_screen_with_text
            gosub set_pf_copy_line
            gosub text_screen_io
                if keyhit%  =  1% then errormsg$ = " "
                if keyhit%  =  1% then options_screen
                if keyhit%  =  2% then top% = 1%
                if keyhit%  =  3% then top% = max(1%, lastline% - 14%)
                if keyhit%  =  4% then top% = max(1%, top% - 14%)
                if keyhit%  =  5% then top% = min(top% + 14%,            ~
                                              max(1, lastline% - 14%))
                if keyhit%  =  6% then top% = max(1%, top% - 1%)
                if keyhit%  =  7% then top% = min(lastline% - 7%,        ~
                                                             top% + 1%)
                if keyhit% <>  0% then L17390

            errormsg$ = " "
            start$ = str(xscrn$(22),39,5) : end$ = str(xscrn$(23),39,5)
            if start$ <> "ALL" then L17620
                start% = 1%
                end%   = lastline%
                goto L17790

L17620:     if start$ <> "FIRST" then L17640
                start% = 1%  :  goto L17690
L17640:     convert start$ to start%, data goto L17660
            if start% >= 1% and start% <= lastline% then L17690
L17660:         errormsg$ = "Invalid entry for Starting Line."
                goto L17390

L17690:     if end$ <> "LAST"  then L17710
                end% = lastline%  :  goto L17740
L17710:     if end$ <> " " then L17730
                end% = start%     :  goto L17740
L17730:     convert end$ to end%, data goto L17750
L17740:     if end% >= start% and end% <= lastline% then L17780
L17750:         errormsg$ = "Invalid entry for Ending Line."
                goto L17390

L17780
*        Now copy the specified range of lines to the clipboard.
L17790:         gosub save_to_clipboard
                goto options_screen

        REM *************************************************************~
            * C U T   L I N E ( S )   T O   C L I P B O A R D           *~
            * --------------------------------------------------------- *~
            * This routine allows deleting of a single line of text,    *~
            * a group of lines, or all text and putting it in clipboard.*~
            *************************************************************
        cut_to_clipboard
            errormsg$ = " "
            if lastline% > 0% then L18310
                errormsg$ = "There is no text to delete."
                goto options_screen

L18310
*        Translate cursor position into default starting point.
            sl% = cursor1%  :  sr% = cursor2%  :  restore% = 1%
            tl% = sl% + top% - 3%
            if tl% < top% or tl% > lastline% then tl% = 0%
            convert tl% to start$, pic(#####) : end$ = " "
            if tl% = 0% then start$ = " "
            for p% = 1% to 5%
                if str(start$,,1) = " " then start$ = str(start$,2)
            next p%

L18390
*        Display screen and get line range for delete.
            gosub load_screen_with_text
            gosub set_pf_cut_line
            gosub text_screen_io
                if keyhit%  =  1% then errormsg$ = " "
                if keyhit%  =  1% then options_screen
                if keyhit%  =  2% then top% = 1%
                if keyhit%  =  3% then top% = max(1%, lastline% - 14%)
                if keyhit%  =  4% then top% = max(1%, top% - 14%)
                if keyhit%  =  5% then top% = min(top% + 14%,            ~
                                              max(1, lastline% - 14%))
                if keyhit%  =  6% then top% = max(1%, top% - 1%)
                if keyhit%  =  7% then top% = min(lastline% - 7%,        ~
                                                             top% + 1%)
                if keyhit% <>  0% then L18390

            errormsg$ = " "
            start$ = str(xscrn$(22),39,5) : end$ = str(xscrn$(23),39,5)
            if start$ <> "ALL" then L18620
                start%    = 1%
                end%      = lastline%
                goto L18775

L18620:     if start$ <> "FIRST" then L18640
                start% = 1%  :  goto L18690
L18640:     convert start$ to start%, data goto L18660
            if start% >= 1% and start% <= lastline% then L18690
L18660:         errormsg$ = "Invalid entry for Starting Line."
                goto L18390

L18690:     if end$ <> "LAST"  then L18710
                end% = lastline%  :  goto L18740
L18710:     if end$ <> " " then L18730
                end% = start%     :  goto L18740
L18730:     convert end$ to end%, data goto L18750
L18740:     if end% >= start% and end% <= lastline% then L18775
L18750:         errormsg$ = "Invalid entry for Ending Line."
                goto L18390

L18775:     gosub save_to_clipboard
*        Now delete the specified range of lines.
            if start% = 1% and end% = lastline% then L18810   /* ALL    */
            if end% <> lastline% then L18880
L18810:         for l% = start% to lastline%
                     text$(l%,1) = " "
                next l%
                lastline% = max(0%, start% - 1%)
                if top% >= lastline% then restore% = 2%
                if top% >= lastline% then top% = max(1%, lastline% - 14%)
                goto options_screen

L18880:     tl% = start%
            for l% = end% + 1%  to  lastline%
                text$(tl%,1) = text$(l%,1)
                tl% = tl% + 1%
            next l%
            start% = tl%  :  end% = lastline%
            goto L18810


        REM *************************************************************~
            * P A S T E   F R O M   C L I P B O A R D                   *~
            * --------------------------------------------------------- *~
            * Allow User to copy text from "Clipboard" into this text.  *~
            * Insert point is defined by cursor position on screen.     *~
            *************************************************************
        paste_from_clipboard
            copy$, errormsg$ = " "
            sl% = cursor1%  :  sr% = cursor2%  :  restore% = 1%
            if sl% < 3% or sl% > 17%      then L19110
            tl% = sl% + top% - 3%
            if tl% < 1% or tl% > lastline% + 1% then L19110  else L19122
L19110:         errormsg$ = "Posistion cursor at insert point first."
                goto options_screen

L19122
*        Screw up screen to indicate where copy text will go
            for l% = sl% to 17%
                xscrn$(l%) = hex(84) & "Copy" & hex(8c)
            next l%

L19140:     gosub set_pf_clipboard_paste
            gosub text_screen_io
                if keyhit%  =  1% then errormsg$ = " "
                if keyhit%  =  1% then       options_screen
                if keyhit% <>  0% then       L19140
            errormsg$ = " "

*        Now try to execute copy.
            readkey$ = "U" & str(user$) & hex(00000000) & hex(00000000)
            call "PLOWNEXT" (#1, readkey$, 4%, f1%(1))
            if f1%(1) = 1% then L19330
L19310:         errormsg$ = "There is no text in the copy element."
                goto L19140
L19330:     get #1 using L19335, copylines%, copywidth%
L19335:         FMT XX(14), BI(2), BI(1)
            if copylines% = 0% then L19310
            if copywidth% = width% then L19390
                errormsg$ = " "
                put errormsg$ using L19380, copywidth%, width%
L19380:              %Copy width (##) differs from text width (##).
L19390:     if copylines% <= rows% - lastline% then L19450
                errormsg$ = " "
                temp% = copylines% - (rows% - lastline%)
                put errormsg$ using L19420, temp%
L19420:              %Not enough room. #### more lines are required.
                if temp% = 1% then errormsg$ =                           ~
                     "Not enough room. 1 more line is required."
                goto L19140

L19450
*        Seems plausible --  Get confirmation if required.
            if errormsg$ = " " then L19510
            gosub set_pf_extrnl_copy2
L19470:     gosub text_screen_io
                if keyhit%  =  1%  then       options_screen
                if keyhit% <>  0%  then       L19470

L19510
*        Perform the copy and then return to options screen.
*         First scoot Text array down to make room for copy member.
            errormsg$ = " "
            if tl% > lastline%  then L19560
            for l% = lastline%  to  tl%  step - 1%
                text$(l% + copylines%,1) = text$(l%,1)
                text$(l%,1) = " "
            next l%
L19560
*         Now read text into the open space.
            readkey$ = "U" & str(user$) & hex(00000000) & hex(00000000)
            lastline% = lastline% + copylines%
            i% = tl% - 1%  :  copylines% = tl% + copylines% - 1%
L19580:     call "PLOWNEXT" (#1, readkey$, 4%, f1%(1))
            if f1%(1) = 0% then options_screen
            get #1, str(filler64$,,64), str(work$(),,textsize%)
                FMT XX(64), CH(1960)
            for l% = 1% to textincr%
                i% = i% + 1% : if i% > copylines% then options_screen
                text$(i%,1) = work$(l%)
            next l%
            goto L19580

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub save_data
            if keyhit% = 16% and types% > 1% then goto enter_text_type   ~
                                             else goto L65000


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
                xscrn$(l%) = hex(8c) & "Add-"
                if l% + top% - 3% > rows% then                           ~
                                            xscrn$(l%) = hex(8c) & "EOT-"
            next l%
            l% = 3%
            if lastline% = 0% then L20220
            for x% = top% to min(top% + 14%, lastline%)
                convert x% to str(xscrn$(l%),2,4), pic(###0)
                str(xscrn$(l%),6) = hex(8c06)
                str(xscrn$(l%),8) = hex(8c) & str(text$(x%,1)) &         ~
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


        push_lines
*         Move text array down from LINE% and clear text at LINE%
            if line% > lastline% then L20460
            for x% = lastline% to line% step -1%
                text$(x%+1%,1) = text$(x%,1)
            next x%
L20460:     text$(line%,1) = " "
            lastline% = lastline% + 1%
          return


        pop_line
*         Move text array up and over LINE% and clear last line
            if keyhit% <> 1% then return  /* --> Array full            */
            if line% = lastline% then L20570
            for x% = line% to lastline%
                text$(x%,1) = text$(x%+1%,1)
            next x%
L20570:     text$(lastline%,1) = " "
            lastline% = lastline% - 1%
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


        load_text_from_screen
*        Move all text on screen into its appropriate spot in the text
*        array
            for l% = 3% to 17%
                if str(xscrn$(l%), 2, 4) = "Add-" then L20810
                if str(xscrn$(l%), 2, 4) = "EOT-" then L20810
                     line% = l% + top% - 3%
                     text$(line%,1) = str(xscrn$(l%), 9,width%)
L20810:     next l%
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
            xscrn$(20) = hex(8c) & "(1)Start Over     ( 8)Find Text    "&~
                         "    (14)Cursor Pos.   (13)Show Instructions "
            xscrn$(21) = hex(8c) & "(2)First (3)Last  ( 9)Modify Text  "&~
                         "    (17)Flip Screen   (15)Print Screen      "
            xscrn$(22) = hex(8c) & "(4)Prev  (5)Next  (10/26)ExtCopy/Pa"&~
                         "ste (18)Cursor Home   (31)Print Text        "
            xscrn$(23) = hex(8c) & "(6)Down  (7)Up    (11/27)Insert/CB "&~
                         "Copy(19)Cursor Right  (16)Save,Reselect Type"
            xscrn$(24) = hex(8c) & "                  (12/28)Delete/CB "&~
                         "Cut (20)Cursor Left   (32)Save and Return   "
            if types%  = 1% then str(xscrn$(23),59) =                    ~
                                               "(16)Save and Return   "
            if types%  = 1% then str(xscrn$(24),59) = " "
            gosub adjust_move_pfs
            if restore% = 0% then xorder1$ = xorder2$
            if restore% = 1% then xorder1$ = hex(01a0) &bin(sr%) &bin(sl%)
            if restore% = 2% then xorder1$ = hex(01a00903)
            restore% = 0%
            str(xscrn$(1), 71) = " OPTIONS "
            inpmessage$ = " "
          return


        set_pf_insert_line     /* INSERT LINE screen                   */
            xscrn$(20) = hex(8c) & "(1)EXIT LINE INSERT                "&~
                         "                      (13)Show Instructions "
            xscrn$(21) = hex(8c) & "                                   "&~
                         "                      (15)Print Screen      "
            xscrn$(22) = hex(8c) & "                                   "&~
                         "                                            "
            xscrn$(23) = hex(8c) & "                                   "&~
                         "                                            "
            xscrn$(24) = hex(8c) & "                  (14   )Show Colum"&~
                         "n Pos.                                      "
            inpmessage$= "Enter Text.  Press PF-1 to exit Line Insert."
            str(xscrn$(1), 71) = "Line Insrt"
            xorder1$   = hex(01a009) &  bin(cursor1%)
          return


        set_pf_modify     /* PF keys for MODIFY option                 */
            xscrn$(20) = hex(8c) & "(1)Exit Modify Option              "&~
                         "                      (13)Show Instructions "
            xscrn$(21) = hex(8c) & "(2)First (3)Last                   "&~
                         "       (18)Center     (15)Print Screen      "
            xscrn$(22) = hex(8c) & "(4)Prev  (5)Next                   "&~
                         "       (19)R. Justify                       "
            xscrn$(23) = hex(8c) & "(6)Down  (7)Up      (14)Show Column"&~
                         " Pos.  (20)L. Justify (16)Keep Current Scrn "
            gosub adjust_move_pfs
            xorder1$   = xorder2$
            str(xscrn$(1), 71) = "  MODIFY  "
            inpmessage$ = "Make modifications and press ENTER, or select"~
                          & " from PF options below:       "
          return


        set_pf_delete_line
            xscrn$(20) = hex(8c) & "(1)EXIT Delete  (RETURN)DELETE line"&~
                         "s indicated           (13)Show Instructions "
            xscrn$(21) = hex(8c) & "(2)First (3)Last                   "&~
                         "                      (15)Print Screen      "
            xscrn$(22) = hex(8c) & "(4)Prev  (5)Next   DELETE FROM LINE"&~
                         ":" & hex(a1) & str(start$) & hex(8c)
            xscrn$(23) = hex(8c) & "(6)Down  (7)Up              TO LINE"&~
                         ":" & hex(a1) & str(end$) & hex(8c)
            xscrn$(24) = hex(8c) & "                                   "&~
                         "                                            "
            gosub adjust_move_pfs
            xorder1$ = hex(01a02716)
            str(xscrn$(1), 71) = "Delte Line"
            inpmessage$ = "Enter Line range (#, FIRST, LAST, ALL) for" & ~
                          " delete.  PF-1 to EXIT."
          return


        set_pf_startover  /* Define start over options                 */
            xscrn$(20) = hex(8c) & "(1) CONTINUE Edit     - Return to O"&~
                         "ption Selection Screen.                     "
            xscrn$(21) = hex(8c) & "(2) RESTART  Edit     - Reload text"&~
                         " from last saved copy and restart editing.  "
            xscrn$(22) = hex(8c) & "(3) START OVER        - Discard All"&~
                         " changes made this session and return to    "
            xscrn$(23) = hex(8c) & "                        Type Select"&~
                         "ion Screen.                                 "
            if types%  = 1% then xscrn$(23) =                            ~
                         hex(8c) & "                        Calling Pro"&~
                         "gram.                                       "
            xscrn$(24) = hex(8c) & " "
            str(xscrn$(20), 5,1), str(xscrn$(21), 5,1),                  ~
                                  str(xscrn$(22), 5,1)  = hex(84)
            str(xscrn$(20),14,1), str(xscrn$(21),14,1)  = hex(8c)
            str(xscrn$(22),19,1)                        = hex(8c)
            inpmessage$= "START OVER OPTIONS"
            str(xscrn$(1), 71) = "START OVER"
            xorder1$   = xorder2$
          return


        set_pf_extrnl_copy
            xscrn$(20) = hex(8c) & "(1)EXIT External Copy  (RETURN)CONT"&~
                         "INUE with Copy        (13)Show Instructions "
            xscrn$(21) = hex(8c) & "                                   "&~
                         "                      (15)Print Screen      "
            xscrn$(22) = hex(84) & "                     Copy Element I"&~
                         "D:" & hex(a1) & str(copy$) & hex(8c)
            xscrn$(23) = hex(8c) & "                                   "&~
                         "                                            "
            xscrn$(24) = hex(8c) & "                                   "&~
                         "                                            "
            xorder1$ = hex(01a02816)
            str(xscrn$(1), 71) = "Xtrnl Copy"
            inpmessage$ = "Enter name that has been assigned to the c" & ~
                          "opy element.           "
          return


        set_pf_extrnl_copy2    /* Confirmation                         */
            xscrn$(20) = hex(84) & "(1)EXIT External Copy  (RETURN)CONT"&~
                         "INUE with Copy        (13)Show Instructions "
            xscrn$(21) = hex(8c) & "                                   "&~
                         "                      (15)Print Screen      "
            xscrn$(22) = hex(8c) & "                     Copy Element I"&~
                         "D:" & hex(a4) & str(copy$) & hex(8c)
            xscrn$(23) = hex(8c) & "                                   "&~
                         "                                            "
            xscrn$(24) = hex(8c) & "                                   "&~
                         "                                            "
            str(xscrn$(20),23,1) = hex(8c)
            xorder1$ = hex(01a009) & bin(sl%)
            str(xscrn$(1), 71) = "Xtrnl Copy"
            inpmessage$ = "Press RETURN to continue with copy, PF-1 t" & ~
                          "o abort.               "
          return


        set_pf_copy_line
            xscrn$(20) = hex(8c) & "(1)EXIT Copy  (RETURN)COPY lines in"&~
                         "dicated to clipboard  (13)Show Instructions "
            xscrn$(21) = hex(8c) & "(2)First (3)Last                   "&~
                         "                      (15)Print Screen      "
            xscrn$(22) = hex(8c) & "(4)Prev  (5)Next   COPY FROM LINE  "&~
                         ":" & hex(a1) & str(start$) & hex(8c)
            xscrn$(23) = hex(8c) & "(6)Down  (7)Up              TO LINE"&~
                         ":" & hex(a1) & str(end$) & hex(8c)
            xscrn$(24) = hex(8c) & "                                   "&~
                         "                                            "
            gosub adjust_move_pfs
            xorder1$ = hex(01a02716)
            str(xscrn$(1), 71) = "Copy Line "
            inpmessage$ = "Enter Line range (#, FIRST, LAST, ALL) for" & ~
                          " copy.    PF-1 to EXIT."
          return


        set_pf_cut_line
            xscrn$(20) = hex(8c) & "(1)EXIT Cut  (RETURN)CUT lines indi"&~
                         "cated to clipboard    (13)Show Instructions "
            xscrn$(21) = hex(8c) & "(2)First (3)Last                   "&~
                         "                      (15)Print Screen      "
            xscrn$(22) = hex(8c) & "(4)Prev  (5)Next      CUT FROM LINE"&~
                         ":" & hex(a1) & str(start$) & hex(8c)
            xscrn$(23) = hex(8c) & "(6)Down  (7)Up              TO LINE"&~
                         ":" & hex(a1) & str(end$) & hex(8c)
            xscrn$(24) = hex(8c) & "                                   "&~
                         "                                            "
            gosub adjust_move_pfs
            xorder1$ = hex(01a02716)
            str(xscrn$(1), 71) = "Cut Line  "
            inpmessage$ = "Enter Line range (#, FIRST, LAST, ALL) for" & ~
                          " cut.     PF-1 to EXIT."
          return


        set_pf_clipboard_paste
            xscrn$(20) = hex(84) & "(1)EXIT C'Board Paste    (RETURN)CO"&~
                         "NTINUE with Paste     (13)Show Instructions "
            xscrn$(21) = hex(8c) & "                                   "&~
                         "                      (15)Print Screen      "
            xscrn$(22) = hex(8c) & "                                   "&~
                         "  "
            xscrn$(23) = hex(8c) & "                                   "&~
                         "                                            "
            xscrn$(24) = hex(8c) & "                                   "&~
                         "                                            "
            str(xscrn$(20),23,1) = hex(8c)
            xorder1$ = hex(01a009) & bin(sl%)
            str(xscrn$(1), 71) = "Paste Clip"
            inpmessage$ = "Press RETURN to continue with copy, PF-1 t" & ~
                          "o abort.               "
          return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  May also    *~
            * reload text and restart edit.                             *~
            *************************************************************
        startover
            errormsg$ = " "
            gosub set_pf_startover
L29090:     gosub text_screen_io
                if keyhit% = 1% then return
                if keyhit% = 2% then reload
                if keyhit% = 3% then startover2
            goto L29090

        startover2   /* User really wants to start over                */
            return clear all
            if types% > 1% then enter_text_type else L65000

        reload       /* Load text back from file and restart edit      */
            return clear all
            goto set_up


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
L30120:         lastline% = 0%
                width% = type_width%
                if pass_width% > 0% then width% = pass_width%
                gosub redim_matrix
                return

L30170:     readkey$ = "W" & str(user$) & str(textid$) & type$ & hex(0001)
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

                get #1, str(filler64$,,64), str(work$(),,textsize%)
                    FMT XX(64), CH(1960)
                for ti% = 1% to textincr%
                    i% = i% + 1%
                    if i% > rows% then L30400
                    text$(i%,1%) = work$(ti%)
L30400:         next ti%
                goto L30320


        redim_matrix
            rows% = matsize% / width%
        /*  ROWS% = 28% * (ROWS% / 28%)  No longer Needed */
            mat redim  text$(rows%, 1)width%
            return


        REM *************************************************************~
            * S A V E   D A T A                                         *~
            * --------------------------------------------------------- *~
            * Write this sucker out to the work file.                   *~
            *************************************************************
        save_data
            if textid$ <> hex(ffffffff) then L31200
            if lastline% = 0% then return /* Don't save if nothing there*/
                readkey$ = "N"
L31080:         call "READ101" (#1, readkey$, f1%(1))
                if f1%(1) = 1% then L31120
                     init (" ") work$()
                     put filler64$ using L31100, "N", 1%, " "
L31100:                  FMT CH(11), BI(4), CH(49)
                     write #1, str(filler64$,,64%),                      ~
                               str(work$(),,textsize%), eod goto L31080
                     goto L31080
L31120:         get #1 using L31130, nextid%
L31130:              FMT XX(11), BI(4)
L31140:         textid$ = bin(nextid%, 4)
                nextid% = nextid% + 1%
                if str(textid$) = hex(00000000) then L31140
                if str(textid$) = hex(20202020) then L31140
                if str(textid$) = hex(ffffffff) then L31140
                put #1 using L31175, nextid%
L31175:              FMT POS(12), BI(4)
                rewrite #1

L31200:     i%     = 0%
            seqnr% = 1%
            plowkey$ = "W" & str(user$) & str(textid$) & type$ & hex(0000)
            call "DELETE" (#1, plowkey$, 9%)

            str(filler64$, 1,1) = "W"
            str(filler64$, 2,3) = str(user$,,3)
            str(filler64$, 5,4) = str(textid$,,4)
            str(filler64$, 9,1) = str(type$,,1)
        /*  STR(FILLER64$,10,2) = BIN(SEQNR%,2) Just to keep things ok */
            str(filler64$,12,3) = str(srce$,,3)
            str(filler64$,15,2) = bin(lastline%,2)
            str(filler64$,17,1) = bin(width%,1)
            str(filler64$,18)   = " "

L31350:     str(filler64$,10,2) = bin(seqnr%,2)
            for ti% = 1% to textincr%
                i% = i% + 1%
                if i% <= lastline% then work$(ti%) = text$(i%,1%)        ~
                                   else work$(ti%) = " "
            next ti%
            write #1, str(filler64$,,64), str(work$(),,textsize%)
            seqnr% = seqnr% + 1%
            if i% < lastline% then L31350
            return


        FMT CH(1),                       /* Logical file ID            */~
            CH(3),                       /* User ID                    */~
            CH(4),                       /* Text ID  (Binary Number)   */~
            CH(1),                       /* Text Type                  */~
            BI(2),                       /* Record Sequence            */~
            CH(3),                       /* Source Code                */~
            BI(2),                       /* # of Text Lines            */~
            BI(1),                       /* Line width                 */~
            CH(47),                      /* Filler                     */~
            CH(1960)                     /* Text Lines                 */

        REM *************************************************************~
            * S A V E   T O   C L I P B O A R D                         *~
            * --------------------------------------------------------- *~
            * Write this sucker out to the clipboard.                   *~
            *************************************************************

        save_to_clipboard
            i%     = start% - 1%
            seqnr% = 1%
            plowkey$ = "U" & str(user$) & hex(00000000) & hex(00000000)
            call "DELETE" (#1, plowkey$, 4%)

            str(filler64$, 1,1) = "U"
            str(filler64$, 2,3) = str(user$,,3)
            str(filler64$, 5,4) = hex(00000000)
            str(filler64$, 9,1) = hex(20)
        /*  STR(FILLER64$,10,2) = BIN(SEQNR%,2) Just to keep things ok */
            str(filler64$,12,3) = " "
            str(filler64$,15,2) = bin(end% - start% + 1%,2)
            str(filler64$,17,1) = bin(width%,1)
            str(filler64$,18)   = " "

L32350:     str(filler64$,10,2) = bin(seqnr%,2)
            for ti% = 1% to textincr%
                i% = i% + 1%
                if i% <= end%      then work$(ti%) = text$(i%,1%)        ~
                                   else work$(ti%) = " "
            next ti%
            write #1, str(filler64$,,64), str(work$(),,textsize%)
            seqnr% = seqnr% + 1%
            if i% < end% then L32350
            return

        REM *************************************************************~
            * T Y P E S   S C R E E N                                   *~
            * --------------------------------------------------------- *~
            * Allows User to specify which type of text to maintain.    *~
            *************************************************************
        types_screen
            gosub init_screen
            xscrn$(1) = hex(8c) & "Text Management for " & hex(84) &     ~
                        srcedescr$
            xscrn$(2) = hex(ac) & srcemsg$
            str(xscrn$(5), 10) = "Types Available for this source are:"
            for w% = 1% to 10%
                if types$(w%) = " " then L40150
                     str(xscrn$(w%+5%), 19) = hex(860b8c) & " " &        ~
                                          str(typesdescr$(w%)) & hex(8c)
L40150:     next w%
            inpmessage$ = "Place cursor at Text Type desired and press" &~
                          " RETURN."
            gosub set_message_lines
            str(xscrn$(20), 59) = "(13) Show Instructions"
            str(xscrn$(21), 59) = "(15) Print Screen     "
            str(xscrn$(23), 59) = "(16) Return to Caller "

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
                call "ZMANUAL" ("TXTINSUB")
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
                call "ZMANUAL" ("TXTINSUB")
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
L65010: end

*       *** ERROR EXITS ****
L65020: errormsg$ = "Source '" & srce$ & "' not on file."   :  goto L65300
L65030: errormsg$ = "No TYPES found under Source '" & srce$ & "'."
                          goto L65300
L65070: errormsg$ = "TEXT ID MUST BE PASSED IN W/ LENGTH OF 4!!!!!!"
                          goto L65300
L65300: inpmessage$ = "Text can not be entered due to the above reason."&~
                      " Press RETURN to continue."
        accept                                                           ~
            at(01,02), "TEXT MANAGEMENT ROUTINE",                        ~
            at(02,02), fac(hex(8c)), srcemsg$,                           ~
            at(03,02), fac(hex(a4)), errormsg$,                          ~
            at(21,02), fac(hex(a4)), inpmessage$

            close ws
            end
