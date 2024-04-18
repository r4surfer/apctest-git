        REM *************************************************************~
            * APCPL10B - 1.Check to see if Sales Order Has Been Assigned*~
            *              and that the Delivery Code is Correct.       *~
            *            2.New Password (Window) as of 10/13/97         *~
            *            3.Check for Release R6.04.03                   *~
            *************************************************************

        sub "APCPL10B" (or_key4$, or_cuscode$, or_po$, sp$, check%, #1 )

        dim                                                              ~
            or_key4$8, or_key1$25,       /* Alt Key 4 and Alt Key 1    */~
            or_cuscode$9,                /*                            */~
            or_po$16,                    /*                            */~
            sp$2, date$8,                /* Only Set For Specified Delv*/~
            password$6,                  /* Password                   */~
            pfkeys$32,                   /*                            */~
            inpmessage$79,               /*                            */~
            s$(14%)50,                   /*                            */~
            pf$(3%)79, errormsg$79,      /*                            */~
            or_status$2                  /* Sales Order Status         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Password Check Utility            "
            pname$ = "APCPL11B - Rev: R6.04"
            date$ = date : call "DATEFMT" (date$)
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNOR ! Planning Sales Order Header              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *             C H E C K   O R D E R   S T A T U S           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************
            count%, check% = 0%
            init(" ") or_key1$
            str(or_key1$,1%,9%)   = or_cuscode$
            str(or_key1$,10%,16%) = or_po$
            read #1,hold,key 1% = or_key1$, using L00580  , or_status$,     ~
                                                        eod goto check_so
            if or_status$ > "01" and or_status$ <> "99" then             ~
                                                      goto display_screen
        check_so
            read #1,hold,key 4% = or_key4$, using L00580  , or_status$,     ~
                                                  eod goto check_delivery
L00580:        FMT POS(60), CH(2)
            if or_status$ > "01" and or_status$ <> "99" then             ~
                                                      goto display_screen
        check_delivery
            if sp$ <> "00" then goto display_screen
               goto exit_sub
        display_screen
            gosub set_pf1
L00660:     password$ = " "
            count% = count% + 1%
            if count% > 3% then gosub set_pf2
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,16), fac(lfac$), s$(1%)                   , ch(50),~
               at (04,16), fac(lfac$), s$(2%)                   , ch(50),~
               at (05,16), fac(lfac$), s$(3%)                   , ch(50),~
               at (06,16), fac(lfac$), s$(4%)                   , ch(50),~
               at (07,16), fac(lfac$), s$(5%)                   , ch(50),~
               at (08,16), fac(lfac$), s$(6%)                   , ch(50),~
               at (09,16), fac(lfac$), s$(7%)                   , ch(50),~
               at (10,16), fac(lfac$), s$(8%)                   , ch(50),~
               at (11,16), fac(lfac$), s$(9%)                   , ch(50),~
               at (12,16), fac(lfac$), s$(10%)                  , ch(50),~
               at (13,16), fac(lfac$), s$(11%)                  , ch(50),~
               at (14,16), fac(lfac$), s$(12%)                  , ch(50),~
               at (15,16), fac(lfac$), s$(13%)                  , ch(50),~
               at (16,16), fac(lfac$), s$(14%)                  , ch(50),~
                                                                         ~
               at (19,16), "Please Enter Password ?",                    ~
               at (19,40), fac(hex(b8)), password$              , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

        if keyhit% = 16% then goto exit_sub
        if count% > 3% then goto exit_sub
        if or_status$ > "01" and or_status$ <> "99" then goto exit_sub

        if password$ <> "Window" then goto L00660
           check% = 0%
           goto exit_sub

        set_pf1
            inpmessage$ = "Enter Password to Process S.O and Override the~
        ~ Delivery Code?"
            if or_status$ > "01" then                                    ~
               inpmessage$ = "Sales Order Cannot be Saved, Please Exit?"

            check% = 1%
            lfac$ = hex(84)
            s$(1%) = "**************************************************"
            s$(2%) = "**************************************************"
            s$(3%) = "**  N   N   OOO                                 **"
            s$(4%) = "**  NN  N  O   O                                **"
            s$(5%) = "**  N N N  O   O   N   N   OOO                  **"
            s$(6%) = "**  N  NN  O   O   NN  N  O   O                 **"
            s$(7%) = "**  N   N   OOO    N N N  O   O   N   N   OOO   **"
            s$(8%) = "**                 N  NN  O   O   NN  N  O   O  **"
            s$(9%) = "**                 N   N   OOO    N N N  O   O  **"
            s$(10%)= "**                                N  NN  O   O  **"
            s$(11%)= "**                                N   N   OOO   **"
            s$(12%)= "**                                              **"
            s$(13%)= "**************************************************"
            s$(14%)= "**************************************************"
            pf$(1%)= "                                        " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(ffffffffffffffffffffffffffffff1000)
        return

        set_pf2
            inpmessage$ = "'Good Bye' You Missed !!!!"
            lfac$ = hex(94) : check% = 1%
            s$(1%)= "**************************************************"
            s$(2%)= "*       GGGGG      OO        OO     DDDD         *"
            s$(3%)= "*      G     G   O    O    O    O   D    D       *"
            s$(4%)= "*      G        O      O  O      O  D     D      *"
            s$(5%)= "*      G   GGG  O      O  O      O  D     D      *"
            s$(6%)= "*      G     G   O    O    O    O   D    D       *"
            s$(7%)= "*       GGGGG      OO        OO     DDDD         *"
            s$(8%)= "*                                                *"
            s$(9%)= "*            BBBBBB   Y     Y  EEEEEE            *"
           s$(10%)= "*            B     B   Y   Y   E                 *"
           s$(11%)= "*            BBBBBB      Y     EEEE              *"
           s$(12%)= "*            B     B     Y     E                 *"
           s$(13%)= "*            BBBBBB      Y     EEEEEE            *"
           s$(14%)= "**************************************************"
            pf$(1%)= "<Return> To Exit                        " &        ~
                     "                                       "
            pf$(2%)= "                              <Return> T" &        ~
                     "o Exit                                 "
            pf$(3%)= "                                        " &        ~
                     "                       <Return> To Exit"
            pfkeys$ = hex(ffffffffffffffffffffffffffffffff00)
        return

        exit_sub

        end
