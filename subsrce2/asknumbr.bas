        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *    A     SSS   K  K   N   N  U   U  M   M  BBBB   RRR     *~
            *   A A   S      K K    NN  N  U   U  M M M  B   B  R  R    *~
            *  AAAAA   SSS   KK     N N N  U   U  M   M  BBBB   RRR     *~
            *  A   A      S  K K    N  NN  U   U  M   M  B   B  R  R    *~
            *  A   A   SSS   K  K   N   N   UUU   M   M  BBBB   R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ASKNUMBER- General routine for requesting a Number        *~
            *            from User.  Overlays message window/box onto   *~
            *            current screen & waits for user response.      *~
            *            User can enter a valid number or PF 1 to abort.*~
            *            Caller controls whether message box appears at *~
            *            top or bottom of screen, title of box, and an  *~
            *            input message.  This routine calls NUMTEST to  *~
            *            validate number and checks to see if number    *~
            *            entered is within the range passed in by       *~
            *            the caller.                                    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/25/93 ! Original (with due homage to ASKDATE)    ! JDH *~
            * 07/07/94 ! REMed out conditional after call to WSXIO! JDH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

          sub "ASKNUMBR" (keyhit%,   /* in = control of box position  */ ~
                                     /* out = PF key that user pressed*/ ~
                          hdr$,      /* box header/title passed in    */ ~
                          inputmsg$, /* user message text passed in   */ ~
                          low,       /* low number range              */ ~
                          high,      /* high number range             */ ~
                          number,    /* returned number               */ ~
                          format)    /* number format as with NUMTEST */

        REM  Note that KEYHIT% is passed both ways                       ~
             - if passed in as 1, box is positioned at top, 0 = bottom   ~
             - passed back as 1 if user aborted, 0 if all is OK.

        REM ****************(------- HDR$ -------)***********************~
            *   (----------------- INPUTMSG$ -----------------------)   *~
            * Enter a number from LOW to HIGH (inclusive) & press RETURN*~
            *                  NUMBER  ********                         *~
            *   (----------------- ERRORMSG$ -----------------------)   *~
            ************** or Press PF 1 to Abort. **********************

        dim                                                              ~
            hdr$40,                      /* Message header             */~
            inputmsg$79,                 /* User defined message       */~
            errormsg$79,                 /* Errormessage from NUMTEST  */~
            fnum$10,                     /* From Number                */~
            number$10,                   /* From Date, unformatted     */~
            rangemsg$77,                 /* Range Message              */~
            tnum$10                      /* To Number                  */

        dim                              /* WSXIO/Screen IO Variables  */~
            order$4,                     /* Screen Order Area          */~
            s$(24)80,                    /* Screen Text Array          */~
            iosw_receiver$8              /* IO Word Status Bytes       */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
        REM *************************************************************

            select #5,  "CRTFILE",                                       ~
                        consec,                                          ~
                        recsize = 1924


*        Close the Workstation & Reopen it Under WSXIO
            close ws
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
*          IF X% > 0% THEN END

*        Set Up Header
            if hdr$ = " " then hdr$ = "*** ASK NUMBER ***"

*        Format the Number range passed in
            call "CONVERT" (low,  format, fnum$)
            call "CONVERT" (high, format, tnum$)

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

*        Build and center the Number range message line
            rangemsg$ = "Enter a Number from " & str(fnum$,,len(fnum$)) &~
                        " to " & str(tnum$,,len(tnum$)) &                ~
                        ", and press RETURN."
            call "SPCESMSH" (rangemsg$, 1%)
            str(s$(y%+2%),03%,77%) = rangemsg$
            call "STRING" addr("CT", str(s$(y%+2%),03%,77%), 77%)

*        Place the data entry field
            call "CONVERT" (0, format, number$)
            str(s$(y%+3%),31%,20%) = "Number: " & hex(a2) & str(number$)&~
                                                                  hex(8c)

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
L10560:     order$ = hex(01) & hex(a4) & bin(0%) & bin(y%+3%)
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

            number$ = str(s$(y%+3%),40%,10%)
            order$ = hex(01) & hex(e4) & bin(0%) & bin(y%+3%)
*        Test the Number entered
            errormsg$ = " "
            call "NUMTEST" (number$, low, high, errormsg$, format, number)
            if errormsg$ = " " then close_and_exit

*        Center the Error message and go write the screen again
            str(s$(y%+4%),03%,77%) = "Error - " & errormsg$
            call "STRING" addr("CT", str(s$(y%+4%),03%,77%), 77%)
            goto re_write_the_screen

*        Stuff to do PF 15 Print Screen
        print_screen
           call "WSXIO" addr("C", #5)
           call "SCREEN" addr("C", x%, "P", "                      ")
           call "WSXIO" addr("O", 255%, "Y", #5, x%)
           goto L10560

        close_and_exit
           if keyhit% = 1% then number$ = " "
           call "WSXIO" addr("C", #5)

           end
