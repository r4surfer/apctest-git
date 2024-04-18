        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC    GGG   RRRR    AAA   PPPP   H   H  2222    *~
            *  W   W  C      G      R   R  A   A  P   P  H   H      2   *~
            *  W   W  C      G GGG  RRRR   AAAAA  PPPP   HHHHH   2 2    *~
            *  W W W  C      G   G  R  R   A   A  P      H   H  2       *~
            *   W W    CCC    GGG   R   R  A   A  P      H   H  22222   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCGRAPH2 - Server to PC program of the same name.         *~
            *            Starts the PC program and services requests    *~
            *            for data.                                      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * Dec,  95 ! Original                                 ! ERN *~
            * 10/28/97 ! Century date modification                ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            avail%(490)                  /* WC units available        */,~
            client_msg$79                /* Message from client       */,~
            command$256                  /* Command to client         */,~
            dtl_activity$4               /* WCOUT Stuff...            */,~
            dtl_part$25                                                 ,~
            dtl_plowkey$30                                              ,~
            dtl_rte_step$4                                              ,~
            dtl_tag$19                                                  ,~
            dtl_wc$4                                                    ,~
            errormsg$79                  /* You goofed message        */,~
            h7f$1                        /* HEX 7F                    */,~
            id$4                         /* Message ID                */,~
            msg$256                      /* Message to client         */,~
            p%(1)                        /* SEARCH receiver           */,~
            pip_day1$10                  /* pip start day             */,~
            plowkey$100                  /* Plow Variable             */,~
            sum_avail%(490)              /* dem added up              */,~
            sum_used%(490)               /* dem added up, too         */,~
            tdate$10, udate$8            /* temp dates                */,~
            used%(490)                   /* WC units allocated        */,~
            wc$4, wc2$4                  /* Work Center ID            */,~
            wc_descr$30                  /* Work Center Description   */,~
            wc_group$3                   /* Work Center Group ID      */,~
            wc_groups$(300)3             /* Work Center Groups        */



        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #02 ! SYSFILE2 ! System Records File                      *~
            * #11 ! WCMASTR  ! Work Center Master File                  *~
            * #12 ! CALMASTR ! Production Calendar                      *~
            * #14 ! GENCODES ! General Codes Validation File            *~
            * #23 ! WCOUT    ! Work Center Usage File                   *~
            * #25 ! JBCROSS2 ! Job/Bom/Rte Xref File                    *~
            * #26 ! PIPCROSS ! Hard Pegging File                        *~
            * #27 ! DEMMASTR ! Demand Master File                       *~
            * #28 ! PIPIN    ! Expected Inventory Additions file        *~
            *************************************************************

            select #02, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select #11, "WCMASTR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2  , keylen = 5,                       ~
                         alt key 1, keypos = 1, keylen = 6

            select #12, "CALMASTR",                                      ~
                         varc, indexed, recsize = 1962,                  ~
                         keypos = 1, keylen = 2

            select #14,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #23, "WCOUT",                                         ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 68,                                   ~
                         keypos = 9, keylen = 23,                        ~
                         alt key 1, keypos = 1, keylen = 27

            select #25,  "JBCROSS2",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 94,                                   ~
                         keypos = 29, keylen = 19,                       ~
                         alt key   1, keypos =  1, keylen = 47,          ~
                             key   2, keypos = 48, keylen = 47

            select #26,  "PIPCROSS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen =  71,                       ~
                         alt key  1, keypos =  20, keylen =  52,         ~
                             key  2, keypos =  39, keylen =  33

            select #27,  "DEMMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 123,                                  ~
                         keypos = 2, keylen =  27,                       ~
                         alt key  1, keypos =  10, keylen =  19,         ~
                             key  2, keypos =   1, keylen =  28

            select #28, "PIPIN",                                         ~
                        varc,     indexed,  recsize =    60,             ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

           call "SHOSTAT" ("WCGRAPH2 Server Program")

           call "OPENCHCK" (#02, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#12, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#14, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#23, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#25, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#26, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#27, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#28, 0%, 0%, 0%, " ")


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            h7f$ = hex(7f)

*         Make sure that we can GUI...
            call "CHECKGUI" addr(gui%)
            errormsg$ = "Presentation protocol is not GUI capable."
            if gui% =  0% then error_exit


*        Get Production Calendar info
            call "PIPINDEX" (#2, " ", today%, ret%)
            if ret%  = 1% then                                           ~
                 errormsg$ = "No MONTHS OPEN record in SYSFILE2."
            if ret%  > 1% then                                           ~
                 errormsg$ = "Today is not in PIP Calendar."
            if ret% <> 0% then error_exit
            get #2, using L09235, pip_day1$
L09235:         FMT XX(32), CH(6)
            tdate$ = pip_day1$
            call "DATEFMT" (tdate$, tnum%, udate$)
            pip_day1$ = str(udate$,3%,6%)

*        Start up the Client program
             call "GUIVBRUN" ("WCGRAPH2.EXE", 0%, ret%)
            errormsg$ = "Unable to initiate client program WCGRAPH2."
            if ret% < 0% then error_exit


*        Now send a message to let the client do any start up stuff...
            nbr% = 0%
            id$  = "0001" : msg$ = "Ready to Rock and Roll"
            gosub send_command

*        Ship over the Starting Planning Calendar Date and Today
            convert today% to msg$, pic(000)
            id$ = "0100" : msg$ = str(pip_day1$,,6%) & msg$
            gosub send_command

*        Send the List of Work Centers
            wc_count%  = 0%
            wc_groups% = 0%
            nbr%       = 0%
            plowkey$   = hex(00)
          wc_read_loop
            call "READNEXT" (#11, f1%)
            if f1% <> 1% then end_wc_read_loop
                get #11 using L09450, wc$, wc_descr$, wc_group$
L09450:             FMT XX(1), CH(4), XX(1), CH(30), POS(2022), CH(3)
                wc_count% = wc_count% + 1%
                id$ = "0101" : msg$  = str(wc$,,4) & "  " & wc_descr$
                gosub send_command
                if wc_group$ = " " then wc_read_loop
                     search str(wc_groups$()) = wc_group$ to p%() step 3%
                     if p%(1) <> 0% then wc_read_loop
                          wc_groups%             = wc_groups% + 1%
                          wc_groups$(wc_groups%) = wc_group$
                          goto wc_read_loop
          end_wc_read_loop
            errormsg$ = "There were no Work Centers found."
            if wc_count% = 0% then goto error_exit
            if wc_count% = 1% then end_of_wcs

            id$ = "0101" : msg$  = "All" & "  * All Work Centers *"
            gosub send_command

            if wc_groups% = 0% then end_of_wcs

            for w% = 1% to wc_groups%
                plowkey$ = "WCGROUPS " & wc_groups$(w%)
                call "READ100" (#14, plowkey$, f1%)
                msg$ = " "
                if f1% = 1% then get #14 using L09700, msg$
L09700:              FMT XX(24), CH(30)
                msg$ = str(msg$,,28)
                msg$ = str(wc_groups$(w%),,3) & "   [" & msg$ & "]"
                msg$ = "g" & msg$
                gosub send_command
            next w%

          end_of_wcs


*        Now tell the client that we're done with the preliminaries...
            id$ = "0999" : msg$ = "You're IT"
            gosub send_command


        REM *************************************************************~
            *                       M A I N                             *~
            * --------------------------------------------------------- *~
            * The code after initialization and before the gosubs...    *~
            *************************************************************

            event_loop
                timer% =  0%
                len%   = 79%
                call "GUIFNGER" (timer%, len%, client_msg$,  ret%)
                if ret% < 0% then event_loop

                if ret% = 16% or ret% = 32% then exit_program

                if ret% = 20% then gosub wc_basics

                if ret% = 22% then gosub wc_details

                if ret% = 24% then gosub floor_loading

                goto event_loop


*       -------------------------------
        wc_basics
*       --------------------------------
*        Ship out the WC's descr and the avail and used hours.
            nbr% = 0%
            mat sum_avail% = zer
            mat sum_used%  = zer

            wc$ = str(client_msg$,,4)

            if wc$ = "All" or str(wc$,,1) = "g" then plow_wcs


*        Single WC Wanted...
            call "REDALT0" (#11, wc$, 0%, f1%)
            if f1% = 1% then L10430
                id$ = "1901"
                msg$ = "Unable to load data for this Work Center"
                gosub send_command
                return
L10430:     get #11, using L10440, sum_avail%(), sum_used%(), wc_group$
L10440:         FMT XX(59), 490*BI(2), 490*BI(2), XX(2), CH(3)
            goto send_wc_basics


        plow_wcs
            plowkey$ = all(hex(00))

          all_wcs_loop
            call "PLOWNEXT" (#11, plowkey$, 0%, f1%)
            if f1% = 0% then send_wc_basics
                get #11, using L10440, avail%(), used%(), wc_group$
                if str(wc$,,1) = "g" and str(wc$,2,3) <> wc_group$       ~
                     then all_wcs_loop
                          mat sum_avail% = sum_avail% + avail%
                          mat sum_used%  = sum_used%  + used%
                          goto all_wcs_loop

        send_wc_basics
            for i% = 1% to 490%
                convert 1000% + i% to id$, pic(0000)
                msg$ = "00000000000000000000"
                convert sum_avail%(i%) to str(msg$, 1,10), pic(0000000000)
                convert sum_used% (i%) to str(msg$,11,10), pic(0000000000)
                gosub send_command
            next i%
            id$ = "1999" : msg$ = "EOD: WC Basics"
            gosub send_command
            return


*       --------------------------------
        wc_details
*       --------------------------------
            id$  = "2000"
            nbr% = 0%

            wc$ = str(client_msg$,,4)

            convert str(client_msg$,5,3) to date4_idx

            if wc$ = "All" or str(wc$,,1) = "g" then dtl_plow_wcs

*        Single WC Wanted...
            wc2$ = wc$
            gosub send_wcouts
            goto  eodetails


        dtl_plow_wcs
            plowkey$ = all(hex(00))

          dtl_wcs_loop
            call "PLOWNEXT" (#11, plowkey$, 0%, f1%)
            if f1% = 0% then eodetails
                get #11, using L11030, wc2$, wc_group$
L11030:             FMT XX(1), CH(4), POS(2022), CH(3)
                if str(wc$,,1) = "g" and str(wc$,2,3) <> wc_group$       ~
                     then dtl_wcs_loop
                          gosub send_wcouts
                          goto  dtl_wcs_loop

        eodetails
            id$ = "2999" : msg$ = "EOD: WC Details"
            gosub send_command
            return


        send_wcouts
            put dtl_plowkey$ using L11170, wc2$, date4_idx
L11170:         FMT CH(4), BI(2)
            str(dtl_plowkey$,7,3) = hex(000000)

          dtl_wcout_loop
            call "PLOWALTS" (#23, dtl_plowkey$, 1%, 6%, f1%)
            if f1% = 0% then return
                get #23 using L11250, dtl_wc$, dtl_date, dtl_tag$,        ~
                     dtl_setup, dtl_run, dtl_rte_step$, dtl_activity$
L11250:              FMT CH(4), BI(2), POS(9), CH(19), POS(32), 2*BI(4), ~
                         CH(4), POS(48), CH(4)
                dtl_part$ = " "
                call "READ100" (#25, dtl_tag$, f1%)
                if f1% = 1% then get #25, dtl_part$
                put msg$ using L11310, dtl_wc$, dtl_date, dtl_tag$,       ~
                     dtl_setup, dtl_run, dtl_rte_step$, dtl_activity$,   ~
                     dtl_part$
L11310:              FMT CH(4), PIC(000), CH(19), PIC(0000000000),       ~
                         PIC(0000000000), 2*CH(4), CH(25)
                gosub send_command
                goto  dtl_wcout_loop


*       ---------------------------------
        floor_loading
*       ---------------------------------

            id$  = "3000"
            nbr% = 0%

            convert str(client_msg$,,3) to load_pip_idx

            plowkey$ = all(hex(00))

          load_wcs_loop
            call "PLOWNEXT" (#11, plowkey$, 0%, f1%)
            if f1% = 0% then eoload
                get #11, using L11630, wc$, avail%(), used%()
L11630:             FMT XX(1), CH(4), POS(60), 490*BI(2), 490*BI(2)
                used% = (used%(load_pip_idx) * 100) / avail%(load_pip_idx)
                if used% <   0% then used% =   0%
                if used% > 150% then used% = 150%
                convert used% to msg$, pic(000.00)
                msg$ = str(wc$,,4) & msg$
                gosub send_command
                goto  load_wcs_loop

        eoload
            id$ = "3999" : msg$ = "EOD: Floor Loading"
            gosub send_command
            return

            return


        REM *************************************************************~
            *       M I S C   U T I L I T Y   R O U T I N E S           *~
            * --------------------------------------------------------- *~
            * Stuff to make life easier.                                *~
            *************************************************************

        send_command
            nbr = nbr
            if nbr%  = 1% then convert nbr to msg$, pic(-000000000.0000)
            command$ = h7f$ & "UWVBXWCGRAPH2," & id$ & "," & msg$ & h7f$
            call "SENDCMD" (str(command$,,len(command$)))
        return


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            * --------------------------------------------------------- *~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

        error_exit
            if gui% = 0% then                                            ~
                call "ASKUSER" (0%, "aBnOrMaL eXit",                     ~
                          "Unable to continue for the following reason:",~
                          errormsg$, "Press RETURN to Continue...")      ~
            else                                                         ~
                call "ASKGUI" (16%, "Error!", errormsg$, ret%)

        exit_program
            nbr%     = 0%
            id$      = "9999"
            msg$     = "TTFN"
            if gui%  = 1% then gosub send_command
            end
