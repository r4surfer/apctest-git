        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPL3SB                             *~
            *  Creation Date     - 04/18/96                             *~
            *  Last Modified Date- 11/14/97                             *~
            *  Description       - Display Pertinent Customer Infor-    *~
            *                      mation from the Customer Master      *~
            *                      File.                                *~
            *  Special Comments                                         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/13/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            *          !                                          !     *~
            *************************************************************

            sub "APCPL3SB" ( cuscode$,   /* Customer Code              */~
                             #1,         /* FILE = (CUSTOMER)          */~
                             #2,         /* FILE = (GENCODES)          */~
                             #3,         /* FILE = (SLMMASTR)          */~
                             #4 )        /* FILE = (TXTFILE )          */

        dim                              /* Subroutine - Variables     */~
            cus_name$30, cuscode$9,      /* Customer Code-CUSCODE$9,   */~
            c1$20,                       /* Contact Name               */~
            c2$10,                       /* Phone Number               */~
            c3$12,                       /* Fax Phone Number           */~
            c4$2, c4d$20,                /* Fax Code and Description   */~
            c5$4, c5d$20,                /* Salesman Code and Descript */~
            c6$14,                       /* Credit Limit               */~
            c7$2, c7d$20,                /* Cut-Off Code and Descript  */~
            c8$2, c8d$20,                /* Delivery Code and Descript */~
            c9$2, c9d$20,                /* Cut-Off Code and Descript-B*/~
            c10$2, c10d$20,              /* Delivery Code and Descrip-B*/~
            c11$2, c11d$20,              /* Region Code and Discription*/~
            c12$5, c12d$20,              /* Route Code and Descript    */~
            txtid$4, textid$4,           /* Customer Text Id           */~
            text_key$11, sav_txt$9,      /* Text Lookup Key            */~
            tt$(30%)70, lfac$1,          /* A Page of Text             */~
            rkey$24, pp$16,              /* Table Lookup Key           */~
            pf$79, p$(15%)10,            /* PF Key Description         */~
            hd$22, hdr1$79, vf$(10%)20,  /* Page Counter Script        */~
            cursor%(2%),                 /* Cursor Location for Edit   */~
            i$(24%)80,                   /* SCREEN IMAGE               */~
            pfkeys$32                    /* PF Key Values              */

            hd$   = "Display for Customer :"
            hdr1$ = "                                 * T E X T *        ~
        ~                         "

            p$(1%) = "Contact  :"    : p$( 9%) = "Salesman :"
            p$(2%) = "Phone No.:"    : p$(10%) = "Credit Lm:"
            p$(3%) = "Fax Ph No:"    : p$(11%) = "Region Cd:"
            p$(4%) = "Fax Code :"    : p$(12%) = "Route Cd :"
            p$(5%) = "P-Cut-Off:"
            p$(6%) = "P-Deliver:"
            p$(7%) = "S-Cut-Off:"
            p$(8%) = "S-Deliver:"
            k% = 0%

        mainline
            gosub read_customer
            gosub display_codes
            if keyhit% = 16% then goto exit_sub
            goto mainline

        exit_sub
        end

        reset_data
           init(" ") cus_name$, c1$, c2$, c3$, c4$, c4d$, c5$, c5d$, c6$,~
                     c7$, c7d$, c8$, c8d$, c9$, c9d$, c10$, c10d$, c11$, ~
                     c11d$, c12$, c12d$, tt$(), sav_txt$, vf$(),txtid$,  ~
                     text_key$
        return

        read_customer
           rec% = 0%
           gosub reset_data
           read #1,key = cuscode$, eod goto read_input
              get #1, using L00900   , cus_name$, /* Customer Name        */~
                                    c1$,       /* Contact Name         */~
                                    c2$,       /* Phone Number         */~
                                    c6,        /* Credit Limit         */~
                                    c5$,       /* Salesman Code        */~
                                    txtid$,    /* Customer Text Id     */~
                                    vf$()      /* Variable Fields      */

L00900:        FMT POS(10), CH(30), POS(433), CH(20), CH(10), POS(526),  ~
                   PD(14,4), POS(714), CH(4), POS(789), CH(4), POS(820), ~
                   10*CH(20)

           lfac$ = hex(84)
           convert c6 to c6$, pic(##########.##-)
           c3$ = str(vf$(1%),1%,12%)           /* Fax Phone Number     */
           c4$ = str(vf$(2%),1%,2%)            /* Fax Code             */
           c7$ = str(vf$(3%),1%,2%)            /* Cust Cut-Off Code    */
           c8$ = str(vf$(4%),1%,2%)            /* Cust Delivery Code   */
           c9$ = str(vf$(5%),1%,2%)            /* Cust Cut-Off Code (B)*/
           c10$= str(vf$(6%),1%,2%)            /* Cust Delivery Code(B)*/
           c11$= str(vf$(7%),1%,2%)            /* Region Code          */
           c12$= str(vf$(9%),1%,5%)            /* Customer Route Code  */

           init(" ") rkey$
           str(rkey$,1%,9%)   = "PLAN FAX "
           str(rkey$,10%,15%) = c4$
           read #2,key = rkey$,using L01090 , c4d$, eod goto L01100
L01090:       FMT POS(25), CH(20)
L01100:    init(" ") rkey$
           str(rkey$,1%,9%)   = "PLAN CUTO"
           str(rkey$,10%,15%) = c7$
           read #2,key = rkey$,using L01090 , c7d$, eod goto L01140
L01140:    init(" ") rkey$
           str(rkey$,1%,9%)   = "PLAN DELV"
           str(rkey$,10%,15%) = c8$
           read #2,key = rkey$,using L01090 , c8d$, eod goto L01180
L01180:    init(" ") rkey$
           str(rkey$,1%,9%)   = "PLAN CUTO"
           str(rkey$,10%,15%) = c9$
           read #2,key = rkey$,using L01090 , c9d$, eod goto L01220
L01220:    init(" ") rkey$
           str(rkey$,1%,9%)   = "PLAN DELV"
           str(rkey$,10%,15%) = c10$
           read #2,key = rkey$,using L01090 , c10d$, eod goto L01260
L01260:    init(" ") rkey$
           str(rkey$,1%,9%)   = "PLAN REGN"
           str(rkey$,10%,15%) = c11$
           read #2,key = rkey$,using L01090 , c11d$, eod goto L01300
L01300:    init(" ") rkey$
           str(rkey$,1%,9%)   = "PLAN RTE "
           str(rkey$,10%,15%) = c12$
           read #2,key = rkey$,using L01090 , c12d$, eod goto L01350

L01350:    call "DESCRIBE" (#3, c5$, c5d$, 0%, x%)
           gosub lookup_text
           rec% = 1%
           edit% = 0%
        return

        read_input
            gosub reset_data
            init(" ") cuscode$
            lfac$ = hex(81)
            edit% = 1%
        return

        REM *************************************************************~
            *           D I S P L A Y   C O D E   T A B L E             *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_codes
L01550:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(84)), hd$                    , ch(22),~
               at (01,25), fac(lfac$)  , cuscode$               , ch(09),~
               at (01,40), fac(hex(84)), cus_name$              , ch(30),~
                                                                         ~
               at (03,02), fac(hex(a4)), p$(1%)                 , ch(10),~
               at (03,15), fac(hex(84)), c1$                    , ch(20),~
                                                                         ~
               at (04,02), fac(hex(a4)), p$(2%)                 , ch(10),~
               at (04,15), fac(hex(84)), c2$                    , ch(10),~
                                                                         ~
               at (03,40), fac(hex(a4)), p$(3%)                 , ch(10),~
               at (03,55), fac(hex(84)), c3$                    , ch(12),~
                                                                         ~
               at (04,40), fac(hex(a4)), p$(4%)                 , ch(10),~
               at (04,55), fac(hex(84)), c4$                    , ch(02),~
               at (04,60), fac(hex(84)), c4d$                   , ch(20),~
                                                                         ~
               at (05,02), fac(hex(a4)), p$(5%)                 , ch(10),~
               at (05,15), fac(hex(84)), c7$                    , ch(02),~
               at (05,20), fac(hex(84)), c7d$                   , ch(20),~
                                                                         ~
               at (06,02), fac(hex(a4)), p$(6%)                 , ch(10),~
               at (06,15), fac(hex(84)), c8$                    , ch(02),~
               at (06,20), fac(hex(84)), c8d$                   , ch(20),~
                                                                         ~
               at (05,40), fac(hex(a4)), p$(7%)                 , ch(10),~
               at (05,55), fac(hex(84)), c9$                    , ch(02),~
               at (05,60), fac(hex(84)), c9d$                   , ch(20),~
                                                                         ~
               at (06,40), fac(hex(a4)), p$(8%)                 , ch(10),~
               at (06,55), fac(hex(84)), c10$                   , ch(02),~
               at (06,60), fac(hex(84)), c10d$                  , ch(20),~
                                                                         ~
               at (07,02), fac(hex(a4)), p$(9%)                 , ch(10),~
               at (07,15), fac(hex(84)), c5$                    , ch(04),~
               at (07,20), fac(hex(84)), c5d$                   , ch(20),~
                                                                         ~
               at (08,02), fac(hex(a4)), p$(10%)                , ch(10),~
               at (08,15), fac(hex(84)), c6$                    , ch(14),~
                                                                         ~
               at (07,40), fac(hex(a4)), p$(11%)                , ch(10),~
               at (07,55), fac(hex(84)), c11$                   , ch(02),~
               at (07,60), fac(hex(84)), c11d$                  , ch(20),~
                                                                         ~
               at (08,40), fac(hex(a4)), p$(12%)                , ch(10),~
               at (08,55), fac(hex(84)), c12$                   , ch(05),~
               at (08,62), fac(hex(84)), c12d$                  , ch(18),~
                                                                         ~
               at (09,02), fac(hex(a4)), hdr1$                  , ch(79),~
                                                                         ~
               at (10,02), fac(hex(84)), tt$(1% + k%)           , ch(70),~
               at (11,02), fac(hex(84)), tt$(2% + k%)           , ch(70),~
               at (12,02), fac(hex(84)), tt$(3% + k%)           , ch(70),~
               at (13,02), fac(hex(84)), tt$(4% + k%)           , ch(70),~
               at (14,02), fac(hex(84)), tt$(5% + k%)           , ch(70),~
               at (15,02), fac(hex(84)), tt$(6% + k%)           , ch(70),~
               at (16,02), fac(hex(84)), tt$(7% + k%)           , ch(70),~
               at (17,02), fac(hex(84)), tt$(8% + k%)           , ch(70),~
               at (18,02), fac(hex(84)), tt$(9% + k%)           , ch(70),~
               at (19,02), fac(hex(84)), tt$(10%+ k%)           , ch(70),~
               at (20,02), fac(hex(84)), tt$(11%+ k%)           , ch(70),~
               at (21,02), fac(hex(84)), tt$(12%+ k%)           , ch(70),~
               at (22,02), fac(hex(84)), tt$(13%+ k%)           , ch(70),~
               at (23,02), fac(hex(84)), tt$(14%+ k%)           , ch(70),~
                                                                         ~
               at (24,02), fac(hex(a4)), pf$                    , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then goto L02300
                  gosub read_input
                  goto L01550

L02300:        if keyhit% <> 2% then goto L02340
                  if k% = 0% then k% = 14% else k% = 0%
                  goto L01550

L02340:        if keyhit% <> 15 then goto L02380
                  call "PRNTSCRN"
                  goto L01550

L02380:        if keyhit% <> 16 and keyhit% <> 0% then goto L02430
                  if keyhit% = 16% then return
                  if edit% = 1% then goto L02430
                     keyhit% = 16%

L02430:        close ws
               call "SCREEN" addr ("C", x%, "I", i$(), cursor%())
        return

        set_pf1
            if k% = 0% then pp$ = "(2)Page Two Text"                     ~
                       else pp$ = "(2)Page One Text"

            pf$    = "(1)Enter New Customer         " &                  ~
                     pp$ & "   Press PF(16)/Return to Exit?"
            pfkeys$ = hex(0102ffffffffffffffffffffff0e0f1000)
        return

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_key$, sav_txt$, tt$()
            gosub'099(txtid$)
            if txt% = 0% then return

            text_key$ = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = txtid$
            str(text_key$,9%,1%) = "1"
            sav_txt$ = text_key$
            for jj% = 1% to 25% step 3%
                read #4,key > text_key$, using L02770 , text_key$,         ~
                                                      eod goto L02830
L02770:            FMT CH(11)
                if sav_txt$ <> str(text_key$,1%,9%) then return
                   get #4, using L02810 , tt$(jj%), tt$(jj% + 1%),         ~
                                                  tt$(jj% + 2%)
L02810:          FMT POS(64), 3*CH(70)
            next jj%
L02830: return

