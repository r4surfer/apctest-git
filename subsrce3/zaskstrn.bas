        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *    A     SSS   K  K    SSS   TTTTT  RRRR   N    N   GGG   *~
            *   A A   S      K K    S        T    R   R  N N  N  G      *~
            *  AAAAA   SSS   KK      SSS     T    RRRR   N  N N  G  GG  *~
            *  A   A      S  K K        S    T    R  R   N   NN  G   G  *~
            *  A   A   SSS   K  K    SSS     T    R   R  N    N   GGG   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ASKSTRNG - General routine for requesting a string        *~
            *            from User.  Overlays message window/box onto   *~
            *            current screen & waits for user response.      *~
            *            User can enter a string of characters.  If     *~
            *            a channel number is passed in, then the        *~
            *            string is validated against that file via      *~
            *            GETCODE (assumes primary key).                 *~
                         F1% passed back indicates validation success.  *~
            *            Caller controls whether message box appears at *~
            *            top or bottom of screen, title of box, and an  *~
            *            input message.                                 *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/23/93 ! Original                                 ! WPH *~
            * 07/07/94 ! REMed out conditional after call to WSXIO! JDH *~
            *          !   & called ZGETCODE instead of GETCODE.  !     *~
            * 12/06/94 ! Instring treated as a true default, ie.no! RJH *~
            *          !   Testing.                               !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

           sub "ZASKSTRN"(keyhit%,   /* in = control of box position  */ ~
                                     /* out = PF key that user pressed*/ ~
                          hdr$,      /* box header/title passed in    */ ~
                          inputmsg$, /* user message text passed in   */ ~
                          inputmsg2$,/* user message text passed in   */ ~
                          what$,     /* What is this thing?           */ ~
                          string$,   /* In = default, out = THE STRING*/ ~
                          len%,      /* Max Length of string          */ ~
                          #1,        /* Optional file channel         */ ~
                          f1%)       /* F1% read success flag         */


        REM  Note that KEYHIT% is passed both ways                       ~
             - if passed in as 1, box is positioned at top, 0 = bottom   ~
             - passed back as 1 if user aborted, 0 if all is OK.

        REM ****************(------- HDR$ -------)***********************~
            *   (----------------- INPUTMSG$ -----------------------)   *~
            *   (----------------- INPUTMSG2$ ----------------------)   *~
            *         ########################################          *~
            *   (----------------- ERRORMSG$ -----------------------)   *~
            ************** or Press PF 1 to Abort. **********************

        dim                                                              ~
            hdr$40,                      /* Message header             */~
            inputmsg$79,                 /* User defined message       */~
            inputmsg2$79,                /* Second line of message     */~
            st$79,                       /* The working variable       */~
            in_string$79,                /* Default Input String       */~
            errormsg$79,                 /* Errormessage from DATEOK   */~
            what$20                      /* What is this thing?        */

        dim                              /* WSXIO/Screen IO Variables  */~
            descr$99,                    /* Description for GETCODE    */~
            order$4,                     /* Screen Order Area          */~
            s$(24)80,                    /* Screen Text Array          */~
            iosw_receiver$8              /* IO Word Status Bytes       */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************

            close ws
            validation% = 0%    /* no validation to occur */
            call "NARGS" addr(args%)
            if args% = 9% then validation% = 1% /*they passed file stuff*/
            in_string$ = string$

            select #5,  "CRTFILE",                                       ~
                        consec,                                          ~
                        recsize = 1924


*        Close the Workstation & Reopen it Under WSXIO
            close ws
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
*          IF X% > 0% THEN END

*        Set Up Header
            if hdr$ = " " then hdr$ = "*** ASK STRING ***"

*        Setup for GETCODE
            if validation% = 0% then L02150
            whichkey = 0
            f1% = 0%
            descr$ = hex(06)& "Select the " & what$ & " from the list."

*        Other misc. set-up
L02150:     len% = min(len%, len(str(string$)))

*        First Read the Prior Screen & Dim it/Turn Off Tabs
            order$ = hex(01000000)
            call "WSXIO" addr("X", #5, hex(40), order$, s$(), 1920%,     ~
                     iosw_receiver$)
            x% = 0%
L09050:     y% = x% + pos(str(s$(), x% + 1%) > hex(7f))
            on y% - x% + 1% goto L09130
                str(s$(),y%,1%) = and hex(fd)
                str(s$(),y%,1%) = or  hex(0c)
                x% = y%
                if x% < 1920% then L09050


L09130:     if keyhit% < 1% or keyhit% > 2% then keyhit% = 2%

*        Set the top of box per KEYHIT% passed in
            if keyhit% = 1% then y% = 1% else y% = 19%

*        Build the top line of the box
            str(s$(y%), 1%, 1%) = hex(84)
            str(s$(y%), 2%,79%) = all(hex(0b))

*        Build the left and right edges of box
            for x% = y% + 1%  to  y% + 5%
                str(s$(x%), 1%, 79%) = hex(840b)
                str(s$(x%),80%,  1%) = hex(0b)
            next x%

*        Build the bottom line of the box
            str(s$(y%+5%),1%,1%) = hex(84)
            str(s$(y%+5%),2%,79%)= all(hex(0b))

*        Embed the header in the top line of box
            str(s$(y%),21%,40%) = hex(94) & hdr$ & hex(84)
            call "STRING" addr("CT", str(s$(y%),21%,40%), 40%)
            tran(str(s$(y%),21%,max(1%,pos(str(s$(y%),21%,40%) > " "))), ~
                 hex(0b20))replacing
            tran(str(s$(y%),min(60%,20%+pos(-str(s$(y%),21%,40%) > " ")))~
                ,hex(0b20))replacing

*        Place and center the input message passed in by the user
            str(s$(y%+1%),03%,77%) = inputmsg$
            call "STRING" addr("CT", str(s$(y%+1%),03%,77%), 77%)

*        Build and center the date range message line
            str(s$(y%+2%),03%,77%) = inputmsg2$
            call "STRING" addr("CT", str(s$(y%+2%),03%,77%), 77%)

*        Place the data entry field
            tran(str(string$, len(string$)), hex(0b20))replacing
            str(s$(y%+3%),03%,77%) = hex(81) &                           ~
                                           str(string$,1,len%) & hex(8c)
            call "STRING" addr("CT", str(s$(y%+3%),03%,77%), 77%)

*        Figure out the starting position for the entry field
            start% = pos(str(s$(y%+3%)) = hex(81)) /* was A1 */
            start% = start% + 1%

*        Center the PF Key message
            str(s$(y%+5%),21%,40%) = hex(94) &                           ~
                      "Or Press PF Key 1 to Abort"  &   hex(84)
            call "STRING" addr("CT", str(s$(y%+5%),21%,40%), 40%)

            tran(str(s$(y% + 5%),21%,max(1%,                             ~
                             pos(str(s$(y% + 5%),21%,40%) > " "))),      ~
                             hex(0b20))replacing
            tran(str(s$(y% + 5%),min(60%,                                ~
                             20%+pos(-str(s$(y% + 5%),21%,40%) > " "))), ~
                             hex(0b20))replacing

*        Now Write the Screen
        write_the_screen
L09720:     order$ = hex(01) & hex(a4) & bin(0%) & bin(y%+3%)

        re_write_the_screen
            call "WSXIO" addr("X", #5, hex(80), order$, s$(), 1920%,     ~
                              iosw_receiver$)

*        Read the Screen
            order$ = hex(01000000)
            call "WSXIO" addr("X", #5, hex(40), order$, s$(), 1920%,     ~
                              iosw_receiver$)
            keyhit% = val(str(iosw_receiver$, 3%, 1%)) - 64%
            if keyhit% > 32% then keyhit% = keyhit% - 16%

            if keyhit% = 1% then close_and_exit
            if keyhit% = 15% then print_screen
            if keyhit% <> 0% then write_the_screen

            st$ = str(s$(y%+3%),start%,len%)
            tran(st$,hex(200b))replacing
            order$ = hex(01) & hex(e4) & bin(0%) & bin(y%+3%)
            if in_string$ <> " " and st$ = in_string$ then close_and_exit
            if validation% = 1% then L09970
            if st$ <> " " then close_and_exit
            errormsg$ = "Blank Not Allowed - Enter a " & what$ & " or " &~
                        "Press PF 1 to Exit."
            goto L10050

L09970
*        Test the string entered
            errormsg$ = " "
           call "WSXIO" addr("C", #5)
            call "ZGETCODE" (#1, st$, descr$, 0%, whichkey, f1%)
           call "WSXIO" addr("O", 255%, "Y", #5, x%)
               if f1% = 1% then close_and_exit

            errormsg$ = "Enter or Select a Valid " & what$ &             ~
                                    ", or PF 1 to Exit."

L10050
*        Center the Error message and go write the screen again
            str(s$(y%+4%),03%,77%) = "Error -" & errormsg$
            call "STRING" addr("CT", str(s$(y%+4%),03%,77%), 77%)
            goto re_write_the_screen

*        Stuff to do PF 15 Print Screen
        print_screen
           call "WSXIO" addr("C", #5)
           call "SCREEN" addr("C", x%, "P", "                      ")
           call "WSXIO" addr("O", 255%, "Y", #5, x%)
           goto L09720

        close_and_exit
           string$ = st$
           if keyhit% = 1% then string$ = " "
           call "WSXIO" addr("C", #5)

           end
