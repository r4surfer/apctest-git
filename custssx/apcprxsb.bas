        REM *************************************************************~
            * APCPRXSB - 1.Look-Up Customer and find out what series    *~
            *              name to Use. Table = ELLISON01               *~
            *            2.1st Prompt for Series Code and use           *~
            *              Table = ELLISON02                            *~
            *            3.2nd Prompt for Window Type and use           *~
            *              Table = ELLISON03                            *~
            *            4.Find the Appropriate Model code in           *~
            *              Table = ELLISON04                            *~
            *              Using the New Product Description            *~
            *              <Series><Type> for Specified Model           *~
            *            5.Return Output          Returned In           *~
            *              New Model Code      =  S_23M$                *~
            *              New Series Code     =  S_23$                 *~
            *              Description Length  =  S_23%                 *~
            *              Return Code         =  0% = Ok               *~
            *                                  =  1% = Series N/A       *~
            *************************************************************~
            * Special Notes - OPT% = 1% This is a Special Case Using the*~
            *                           Model and and the Customer's    *~
            *                           Private Label Code. ELLISON01.  *~
            *                           Call Routine 'VERIFY_SERIES' and*~
            *                           Use the table 'ELLISON05' to    *~
            *                           find series Description.        *~
            *                 OPT% = 2% This force a call back thru     *~
            *                           validity so that the 'PRICE'    *~
            *                           can be re-calculated, and the   *~
            *                           MFG Part Number verified.       *~
            *               S_23M$ = +  When a '+' is found in the 1st  *~
            *                           Character of the Part, the user *~
            *                           has forced the Series Prompt    *~
            *                           Screen to appear.               *~
            *************************************************************~
            * Programs Using Sub   =    APCPRCQT - Price Quote Utility  *~
            *                           BCKFASTR - S.O. Entry           *~
            *                                                           *~
            *************************************************************

        sub "APCPRXSB" (opt%,            /* 0%=Input, 1%=Verify Only   */~
                        cuscode$,        /* Customer Code              */~
                        s_23m$,          /* Output New Model Code      */~
                        s_23$,           /* New Series Code(Description*/~
                        s_23%,           /* Length of Description      */~
                        #1,              /* (CUSTOMER) -               */~
                        #2,              /* (GENCODES) -               */~
                        x_er% )          /* Return Code                */

        dim                                                              ~
            readkey$24,                  /* GENCODES Key               */~
            cuscode$9,                   /* Customer Code              */~
            s_1$2, s_1d$30,              /* Series Name Code & Desc    */~
            s_2$4, s_2d$30,              /* Series Type Code           */~
            s_3$4, s_3d$30,              /* Series Description Code    */~
            s_23$8, s_23m$3,             /* Product/Model Lookup Code  */~
            s_verify$5,                  /* Use to Auto Verify         */~
            sav_key$12,                  /* Use for Verify Test        */~
            hdr$(7%)20,                  /* Screen Display Header      */~
            pfkeys$32,                   /*                            */~
            inpmessage$79,               /*                            */~
            errormsg$79,                 /*                            */~
            pf$(3%)79,                   /*                            */~
            lfac$(2%)1                   /* Sales Order Status         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Customer Series Name and Products "
            pname$ = "APCPRXSB - Rev: R6.04"

            date$ = date
            x_er%   = 0%
            call "DATEFMT" (date$)

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CUSTOMER ! Customer Master File                     *~
            * #2  ! GENCODES ! Caelus Table Master File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            hdr$(1%)  ="********************"
            hdr$(2%) = "*    xxxxxxxxxx    *"
            hdr$(3%) = "*                  *"
            hdr$(4%) = "*      (???)       *"
            hdr$(5%) = "*                  *"
            hdr$(6%) = "*    (????????)    *"
            hdr$(7%) = "********************"

            s_23% = 0% : count% = 0% : edit% = 0%
            init(" ") readkey$, s_1$, s_1d$, s_2$, s_2d$, s_3$, s_3d$
            read #1,key = cuscode$, using L01040 , s_1$, eod goto L01180
L01040:        FMT POS(960), CH(2)
            str(readkey$,1%,9%)   = "ELLISON01"
            str(readkey$,10%,15%) = s_1$
            read #2,key = readkey$, using L01080 , s_1d$, eod goto L01180
L01080:        FMT POS(25), CH(30)
            if opt% = 0% then goto L01170
               if str(s_23m$,1%,1%) = "+" then goto L01140

               gosub verify_series
               if x_er% <> 2% then goto exit_sub
L01140:           init(" ") s_23m$
                  x_er% = 0%
                  opt% = 2%                        /* Re-Calc Price    */
L01170:     goto main_line
L01180: x_er% = 1%                                 /* Series N/A       */
           if str(s_23m$,1%,1%) = "+" then opt% = 2% /* RE-CALC PRICE  */
        goto exit_sub

        REM *************************************************************~
            *             M a i n   L i n e   C o d e                   *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************
        main_line
            if keyhit% = 0% and count% > 1% then goto exit_sub
            gosub display_screen
            init(" ") errormsg$
            gosub lookup_series
            if check% <> 1% then goto main_line   /* Series Code Found */
            gosub lookup_window
            if check% <> 2% then goto main_line   /* Window Type Found */
               gosub lookup_model
               if check% <> 3% then goto main_line /*Product Code Found*/
               count% = count% + 1%
               edit% = 1%
               str(hdr$(4%),9%,3%) = s_23m$        /* Model Code Prod  */
               str(hdr$(6%),7%,8%) = s_23$         /* New Series Code  */
               s_23% = len(s_23$)                  /* Descript Length  */
               goto main_line

        display_screen
L01450:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (03,31), fac(hex(84)), hdr$(1%)               , ch(20),~
               at (04,31), fac(hex(84)), hdr$(2%)               , ch(20),~
               at (05,31), fac(hex(84)), hdr$(3%)               , ch(20),~
               at (06,31), fac(hex(84)), hdr$(4%)               , ch(20),~
               at (07,31), fac(hex(84)), hdr$(5%)               , ch(20),~
               at (07,31), fac(hex(84)), hdr$(6%)               , ch(20),~
               at (08,31), fac(hex(84)), hdr$(7%)               , ch(20),~
               at (09,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (10,02), "Product Series Code:",                       ~
               at (10,25), fac(lfac$(1%)), s_2$                 , ch(04),~
               at (10,30), fac(hex(84)), s_2d$                  , ch(30),~
                                                                         ~
               at (11,02), "Product Window Code:",                       ~
               at (11,25), fac(lfac$(2%)), s_3$                 , ch(04),~
               at (11,30), fac(hex(84)), s_3d$                  , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 10% then goto L01800
                  edit% = 0%
                  goto L01450

L01800:        if keyhit% <> 14% then goto L01850
                  opt% = 0%
                  x_er% = 1%
                  goto exit_sub

L01850:        if keyhit% <> 16% then goto L01880
                  goto exit_sub

L01880:        if keyhit% <> 1% then goto L01930
                  tab% = 22%
                  gosub display_codes
                  goto L01450

L01930:        if keyhit% <> 2% then goto L01980
                  tab% = 23%
                  gosub display_codes
                  goto L01450

L01980:        if keyhit% <> 3% then goto L02030
                  tab% = 24%
                  gosub display_codes
                  goto L01450

L02030:        if keyhit% <> 4% then goto L02080
                  tab% = 25%
                  gosub display_codes
                  goto L01450

L02080: return

        set_pf1
            inpmessage$ = "Enter the Applicable Series Code and Window Co~
        ~de?"

            str(hdr$(2%),6%,10%) = s_1d$

            pf$(1%)= "(1)Disp Series Codes   (4)Series Validit" &        ~
                     "y                    (10)Change        "
            pf$(2%)= "(2)Disp Window Types                    " &        ~
                     "                     (14)Not Applicable"
            pf$(3%)= "(3)Disp Products                        " &        ~
                     "                     (16)Finished      "
            pfkeys$ = hex(01020304ffffffffff0affffff0eff1000)

            if edit% = 1% then goto L02280
               lfac$(1%) = hex(81)
               lfac$(2%) = hex(81)
               return
L02280:  lfac$(1%) = hex(84)
         lfac$(2%) = hex(84)
        return

        lookup_series                              /* Code Length (4) */
            check% = 0%
            init(" ") readkey$, s_2d$
            str(readkey$,1%,9%)   = "ELLISON02"
            str(readkey$,10%,15%) = s_2$
            read #2,key = readkey$, using L02380 , s_2d$, eod goto L02420
L02380:        FMT POS(25), CH(30)
            check% = check% + 1%
            s2% = len(s_2$)
        return
L02420:     errormsg$ = "(Error) - Invalid Product Series Code?"
            init(" ") s_2$, s_2d$
        return

        lookup_window                              /* Code Length (4) */
            check% = 1%
            init(" ") readkey$, s_3d$
            str(readkey$,1%,9%)   = "ELLISON03"
            str(readkey$,10%,15%) = s_3$
            read #2,key = readkey$, using L02380 , s_3d$, eod goto L02550
            check% = check% + 1%
            s3% = len(s_3$)
        return
L02550:     errormsg$ = "(Error) - Invalid Product Window Code?"
            init(" ") s_3$, s_3d$
        return

        lookup_model
            check% = 2%
            init(" ") readkey$, s_23$, s_23m$
            s_23$ = str(s_2$,1%,s2%) & " " & str(s_3$,1%,s3%)
            str(readkey$,1%,9%) = "ELLISON04"
            str(readkey$,10%,15%) = s_23$
            read #2,key = readkey$, using L02660 , s_23m$, eod goto L02690
L02660:        FMT POS(25), CH(3)
            check% = check% + 1%
        return
L02690:     errormsg$ = "(Error) - Cannot Find a Valid Series Product?"
            init(" ") s_23$, s_23m$, s_2$, s_2d$, s_3$, s_3d$
        return

        display_codes
            call "APCPLN1B" (tab%,#2)
        return

        verify_series
            s_23% = 0%
            init(" ") readkey$, s_23$, s_verify$
            str(s_verify$,1%,3%)  = s_23m$
            str(s_verify$,4%,2%)  = s_1$
            str(readkey$,1%,9%)   = "ELLISON05"
            str(readkey$,10%,15%) = s_verify$
            read #2,key = readkey$, using L02850 , s_23$, eod goto L02890
L02850:        FMT POS(25), CH(8)
            x_er% = 0%
            s_23% = len(s_23$)
        return
L02890:     init(" ") readkey$, s_verify$, sav_key$
            str(s_verify$,1%,3%) = s_23m$
            str(readkey$,1%,9%) = "ELLISON05"
            str(readkey$,10%,15%) = s_verify$
            sav_key$ = str(readkey$,1%,12%)
            read #2,key > readkey$, using L02950 , readkey$, eod goto L03010
L02950:        FMT CH(24)
            if str(readkey$,1%,12%) <> sav_key$ then goto L03010
            x_er% = 2%                              /* Model Not Valid */
            init(" ") s_23$, s_23m$                 /* Now Must Prompt */
            errormsg$ = "(Error) - Invalid Model Code for Customer?"
        return
L03010:     x_er% = 1%
        return

        exit_sub
            if x_er% = 0% then end
               init(" ") s_23m$, s_23$
        end

