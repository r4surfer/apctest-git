        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *    A     SSS   K  K   DDDD     A    TTTTT  EEEEE          *~
            *   A A   S      K K    D   D   A A     T    E              *~
            *  AAAAA   SSS   KK     D   D  AAAAA    T    EEEE           *~
            *  A   A      S  K K    D   D  A   A    T    E              *~
            *  A   A   SSS   K  K   DDDD   A   A    T    EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ASKDATE  - General routine for requesting a date          *~
            *            from User.  Overlays message window/box onto   *~
            *            current screen & waits for user response.      *~
            *            User can enter a valid date or PF 1 to abort.  *~
            *            Caller controls whether message box appears at *~
            *            top or bottom of screen, title of box, and an  *~
            *            input message.  This routine calls DATEOK to   *~
            *            validate date and checks to see if date        *~
            *            entered is within date range passed in by      *~
            *            the caller.                                    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/24/91 ! Original (with due homage to ASKUSER)    ! WPH *~
            * 07/07/94 ! REMed out conditional after call to WSXIO! JDH *~
            * 07/31/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

           sub "ASKDATE" (keyhit%,   /* in = control of box position  */ ~
                                     /* out = PF key that user pressed*/ ~
                          hdr$,      /* box header/title passed in    */ ~
                          inputmsg$, /* user message text passed in   */ ~
                          fromdate$, /* unformated low date in        */ ~
                          todate$,   /* unformated high date in       */ ~
                          date$,     /* unformatted return date out   */ ~
                          d%)        /* date in YYYYMMDD (ala dateokc)*/

        REM  Note that KEYHIT% is passed both ways                       ~
             - if passed in as 1, box is positioned at top, 0 = bottom   ~
             - passed back as 1 if user aborted, 0 if all is OK.

        REM ****************(------- HDR$ -------)***********************~
            *   (----------------- INPUTMSG$ -----------------------)   *~
            * Enter a date between (FDATE$) and (TDATE$), & press RETURN*~
            *                    DATE  ********                         *~
            *   (----------------- ERRORMSG$ -----------------------)   *~
            ************** or Press PF 1 to Abort. **********************

        dim                                                              ~
            hdr$40,                      /* Message header             */~
            inputmsg$79,                 /* User defined message       */~
            errormsg$79,                 /* Errormessage from DATEOK   */~
            fdate$10,                    /* From Date, formatted       */~
            fromdate$10,                 /* From Date, unformatted     */~
            tdate$10,                    /* To   Date, formatted       */~
            todate$10,                   /* To   Date, unformatted     */~
            date$10,                     /* Captured Date, unformatted */~
            d$10                         /* Captured Date, formatted   */

        dim                              /* WSXIO/Screen IO Variables  */~
            order$4,                     /* Screen Order Area          */~
            s$(24)80,                    /* Screen Text Array          */~
            iosw_receiver$8,             /* IO Word Status Bytes       */~
            workdate$10                  /* Temporary Date             */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            select #5,  "CRTFILE",                                       ~
                        consec,                                          ~
                        recsize = 1924


*        Close the Workstation & Reopen it Under WSXIO
            close ws
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
*          IF X% > 0% THEN END

*        Set Up Header
            if hdr$ = " " then hdr$ = "*** ASK DATE ***"

*        Format the date range passed in
            fdate$ = fromdate$
            tdate$ = todate$

            call "DATFMTC" (fdate$)
            call "DATFMTC" (tdate$)

*        First Read the Prior Screen & Dim it/Turn Off Tabs
            order$ = hex(01000000)
            call "WSXIO" addr("X", #5, hex(40), order$, s$(), 1920%,     ~
                     iosw_receiver$)
            x% = 0%
L09050:     y% = x% + pos(str(s$(), x% + 1%) > hex(7f))
            on y% - x% + 1% goto L09121
                str(s$(),y%,1%) = and hex(fd)
                str(s$(),y%,1%) = or  hex(0c)
                x% = y%
                if x% < 1920% then L09050


L09121:     if keyhit% < 1% or keyhit% > 2% then keyhit% = 2%

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
            str(s$(y%+2%),03%,77%) = "Enter a date between " & fdate$ &  ~
                 " and " & tdate$ & ", and press RETURN."
            call "STRING" addr("CT", str(s$(y%+2%),03%,77%), 77%)

*        Place the data entry field
            workdate$ = fdate$
            str(s$(y%+3%),33%,18%) = "Date: " & hex(a1) & str(workdate$, 1%,10%) & hex(8c)


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
L11120:     order$ = hex(01) & hex(a4) & bin(0%) & bin(y%+3%)
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

            workdate$ = str(s$(y%+3%),40%,10%)
            order$ = hex(01) & hex(e4) & bin(0%) & bin(y%+3%)
*        Test the date entered
            errormsg$ = " "

            call "DATEOKC"(workdate$, d%, errormsg$)
               if errormsg$ <> " " then L11366
            d$ = workdate$
            call "DATUFMTC"(d$)
            call "DATUFMTC"(fdate$) : call "DATUFMTC"(tdate$)
            if d$ >= fdate$ and d$ <= tdate$ then close_and_exit
            call "DATFMTC"(fdate$)  : call "DATFMTC"(tdate$)
            errormsg$ = "The date entered is not within the range."

*        Center the Error message and go write the screen again
L11366:     str(s$(y%+4%),03%,77%) = "Error -" & errormsg$
            call "STRING" addr("CT", str(s$(y%+4%),03%,77%), 77%)
            goto re_write_the_screen

*        Stuff to do PF 15 Print Screen
        print_screen
           call "WSXIO" addr("C", #5)
           call "SCREEN" addr("C", x%, "P", "                      ")
           call "WSXIO" addr("O", 255%, "Y", #5, x%)
           goto L11120

        close_and_exit
           date$ = d$
           if keyhit% = 1% then date$ = " "
           if keyhit% = 1% then call "DATUNFMT" (date$)
           call "WSXIO" addr("C", #5)

           end
