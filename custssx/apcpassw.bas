        REM *************************************************************~
            * APCPASSW - 1.New Password Screen Utility - 11/13/97       *~
            *              Mod for Release Upgrade to R6.04.03          *~
            *************************************************************

        sub "APCPASSW" (prog$, user$, check%)

        dim                                                              ~
            prog$8,                      /* Name of Program Called From*/~
            user$3,                      /* User Id Making Call        */~
            gencdkey$24,                 /* Primary Gencode Key        */~
            password$8, pass$8,          /* Password Entered, Stored   */~
            pfkeys$32,                   /*                            */~
            inpmessage$79,               /*                            */~
            s$(14%)50,                   /*                            */~
            pf$(3%)79,                   /*                            */~
            hdr$40, msg$(3%)79           /* Askuser Arrays             */

        dim f2%(1%),                     /* = 0 if the file is open    */~
            f1%(1%),                     /* = 1 if READ was successful */~
            fs%(1%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(1%)20                  /* Text from file opening     */

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Master Code Table Files                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "GENCODES",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   24

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            f1%(1) = 0%

        REM *************************************************************~
            *             C H E C K   O R D E R   S T A T U S           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************
            count% = 0% : check% = 1%
            init(" ") gencdkey$, pass$
            str(gencdkey$,1%,9%)   = "APCMASTER"
            str(gencdkey$,10%,8%)  = prog$
            str(gencdkey$,18%,7%)  = user$
            read #1,key = gencdkey$, using L00580  , pass$,                 ~
                                                       eod goto exit_sub
L00580:        FMT POS(25), CH(8)
            gosub set_pf1
        display_screen
            init(" ") password$
            count% = count% + 1%
            if count% > 3% then goto no_access
            accept                                                       ~
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
               at (10,48), fac(hex(b8)), password$              , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

        if keyhit% = 16% then goto no_access
        if count% > 3% then goto no_access

        if password$ <> pass$ then goto display_screen
           check% = 0%
           goto exit_sub

        set_pf1
            inpmessage$ = "Enter Password to Access Program/Function or P~
        ~F(16) to Exit ?"
            check% = 1%
            lfac$ = hex(84)
            s$(1%) = "**************************************************"
            s$(2%) = "**************************************************"
            s$(3%) = "**                                              **"
            s$(4%) = "**       P a s s w o r d   S c r e e n          **"
            s$(5%) = "**                                              **"
            s$(6%) = "**                                              **"
            s$(7%) = "**                                              **"
            s$(8%) = "**       Please Enter Password?                 **"
            s$(9%) = "**                                              **"
            s$(10%)= "**                                              **"
            s$(11%)= "**                                              **"
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

        no_access
           comp% = 2%
           hdr$ = "***** A c c e s s   D e n i e d ****"
           msg$(1%) = "- N o   A c c e s s - - N o   A c c e s s -"
           msg$(2%) = "             B y e e e e e e !             "
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           check% = 1%                                /* Access Denied */
        exit_sub

        end
