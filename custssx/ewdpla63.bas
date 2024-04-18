        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLA63                             *~
            *  Creation Date     - 09/28/98                             *~
            *  Last Modified Date- 09/28/98                             *~
            *  Description       - Display Entries for GENCODES Tables. *~
            *                      Table Name or Table No. (to retain   *~
            *                      backward-compatability with subprgrm *~
            *                      APCPLN1B) is passed in.              *~
            *                                                           *~
            *  Special Comments (Table Nos. from APCPLN1B)              *~
            *     TAB% -  TABLE NAME      TAB% TABLE NAME               *~
            *     ----    --------------  ---- -----------              *~
            *      1%     (PLAN DEPT)     11%  (PLAN HOWS)              *~
            *      2%     (PLAN PROC)     12%  (PLAN REGN)              *~
            *      3%     (MODEL    )     13%  (PLAN UPMH)              *~
            *      4%     (PLAN SHFT)     14%  (APC  RGA1)              *~
            *      5%     (PLAN UNIT)     15%  (APC  RGA2)              *~
            *      6%     (PLAN DATE)     16%  (ROUTECODE)              *~
            *      7%     (PLAN STAT)     17%  (PLAN FAX )              *~
            *      8%     (PLAN SORT)     18%  (PLAN SUPP)              *~
            *      9%     (PLAN CUTO)     19%  (PLAN TEMP)              *~
            *     10%     (PLAN DELV)     20%  (PLAN TWTP)              *~
            *                                                           *~
            *     21%     (ELLISON01)     26%  (         )              *~
            *     22%     (ELLISON02)     27%  (         )              *~
            *     23%     (ELLISON03)     28%  (         )              *~
            *     24%     (ELLISON04)     29%  (         )              *~
            *     25%     (ELLISON05)     30%  (*RESERVED - Don't Use*) *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/28/98 ! New Program - Copied & Mod APCPLN1B.     ! BWS *~
            * 10/13/98 ! (EWD001) Mod lengths of val/desc; add PF8! BWS *~
            *************************************************************

            sub "EWDPLA63" ( tab%,       /* Current Planning Tables    */~
                             #1,         /* FILE = (GENCODES)          */~
                             tab_name$,  /* Table Name for Display     */~
                             fnt%,       /* Function: 0=Display, 1=Rpt */~
                             start_val$) /* Starting Tbl Value; " "=1st*/

        dim                              /* Subroutine - Variables     */~
            tab$(30%)10, t_hdr$(30%)30,  /* Code Table Names           */~
/*EWD001*/  val$(3026%)15, desc$30,      /* Table Value and Description*/~
/*EWD001*/  vald$(3026%)30,              /* Table Descriptions         */~
/*EWD001*/  valwk$(3026%)30,             /* Table Descriptions - Work  */~
            readkey$24,                  /* Table Lookup Key           */~
            cursor%(2%),                 /* Cursor Location for Edit   */~
            i$(24%)80,                   /* SCREEN IMAGE               */~
            pf$(3%)79,                   /* PF Key Description         */~
            pageno$16, cnt$16,           /* Page Counter Script        */~
            pfkeys$32,                   /* PF Key Values              */~
            tab_name$9,                  /* Passed-in Table Name       */~
            start_val$15,                /* Passed-in Start Table Value*/~
/*EWD001*/  find_val$15,                 /* Value to Find from <PF8>   */~
/*EWD001*/  dash$(1)30,                  /* String of dashes           */~
            time$8,                      /* System time                */~
            date$8,                      /* Date for screen display    */~
            userid$3                     /* Current User Id            */


            tab$(1%)  = "PLAN DEPT" : tab$(2%)  = "PLAN PROC"
            tab$(3%)  = "MODEL    " : tab$(4%)  = "PLAN SHFT"
            tab$(5%)  = "PLAN UNIT" : tab$(6%)  = "PLAN DATE"
            tab$(7%)  = "PLAN STAT" : tab$(8%)  = "PLAN SORT"
            tab$(9%)  = "PLAN CUTO" : tab$(10%) = "PLAN DELV"
            tab$(11%) = "PLAN HOWS" : tab$(12%) = "PLAN REGN"
            tab$(13%) = "PLAN UPMH" : tab$(14%) = "APC  RGA1"
            tab$(15%) = "APC  RGA2" : tab$(16%) = "PLAN RTE "
            tab$(17%) = "PLAN FAX " : tab$(18%) = "PLAN SUPP"
            tab$(19%) = "PLAN TEMP" : tab$(20%) = "PLAN TWTP"


            tab$(21%) = "ELLISON01" : tab$(26%) = "         "
            tab$(22%) = "ELLISON02" : tab$(27%) = "         "
            tab$(23%) = "ELLISON03" : tab$(28%) = "         "
            tab$(24%) = "ELLISON04" : tab$(29%) = "         "
            tab$(25%) = "ELLISON05" /*tab$(30%) RESERVED - Don't Use*/

            u3% = 0%
            t_hdr$(1%) = "Planning Department Codes"
            t_hdr$(2%)  = " Planning Process Codes  "
            t_hdr$(3%)  = "  Planning Model Codes   "
            t_hdr$(4%)  = "  Planning Shift Codes   "
            t_hdr$(5%)  = "   Planning Unit Codes   "
            t_hdr$(6%)  = "Planning Prod. Date Codes"
            t_hdr$(7%)  = "  Planning Status Codes  "
            t_hdr$(8%)  = "  Production Sort Codes  "
            t_hdr$(9%)  = "Customer Cut-Off Day     "
            t_hdr$(10%) = "Customer Delivery Codes  "
            t_hdr$(11%) = "Plan/Scheduling How Ship "
            t_hdr$(12%) = "Plan/Scheduling Region Cd"
            t_hdr$(13%) = "Scheduling UPMH Goals %  "
            t_hdr$(14%) = "    RGA Status Codes     "
            t_hdr$(15%) = "    RGA Reason Codes     "
            t_hdr$(16%) = " Scheduling Route Codes  "
            t_hdr$(17%) = "    Special FAX Codes    "
            t_hdr$(18%) = " Support Department Codes"
            t_hdr$(19%) = "   Tempered Glass Codes  "
            t_hdr$(20%) = " Twin/Triple Model Codes "
            t_hdr$(21%) = " New Product Series Name "
            t_hdr$(22%) = " New Product Series Code "
            t_hdr$(23%) = "Product Type Description "
            t_hdr$(24%) = "    New Product Codes    "
            t_hdr$(25%) = "New Series Validity Table"
            t_hdr$(26%) = "                         "
            t_hdr$(27%) = "                         "
            t_hdr$(28%) = "                         "
            t_hdr$(29%) = "                         "
          /*t_hdr$(30%) RESERVED - Don't Use       */


            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            time$ = " "
            call "TIME" (time$)

            pageno$ = "Page: XXX of XXX"

/*EWD001*/  find_val$ = " "
/*EWD001*/  vl = 8                      /* Value Length */
            if tab% = 0% then tab% = 30%
            tab$(30) = tab_name$
            if tab% <> 30% then L01090
                readkey$ = hex(000000000000000000) & tab_name$
                call "DESCRIBE" (#1, readkey$, t_hdr$(30), 0%, f1%)
                    if f1% = 0% then end    /* Table Not Found */
                get #1, using L01050, vl$               /*EWD001*/
L01050:             fmt pos(55), ch(2)                  /*  |   */
                convert vl$ to vl, data goto L01090     /*  |   */
                vl = max(vl, 4)                         /*EWD001*/

L01090:     gosub load_table
            on fnt% + 1% gosub display_codes, print_codes
            mat redim valwk$(3026)30                    /*EWD001*/
            mat redim dash$(1)30                        /*EWD001*/

        end

        load_table
           call "SHOSTAT" ("Loading " & t_hdr$(tab%))
           init(" ") val$(), vald$(), readkey$, valwk$() /*EWD001*/
           cnt% = 0% : cnt$ = "Codes: [ XXXXX ]"
           k% = 0%
           val_max% = 0%
           val% = 0%
           str(readkey$,1%,9%)  = tab$(tab%)
           if len(start_val$) = 15% then str(readkey$,10%,15%) =           ~
                str(start_val$,,14%) & bin(val(str(start_val$,15,1)) - 1)  ~
           else                                                            ~
                str(readkey$,10%,15%) = str(start_val$,,14%) & hex(00)
           read #1,key > readkey$, using L01180 , readkey$, desc$,         ~
                                                           eod goto L01340
L01180:        FMT CH(24), CH(30)
           goto L01230
        load_next
           read #1, using L01180 , readkey$, desc$, eod goto L01340

L01230:    if tab$(tab%) <> str(readkey$,1%,9%) then goto L01340
              cnt% = cnt% + 1%
              if mod(cnt%,50%) <> 0% then goto L01290
                 convert cnt% to str(cnt$,10%,5%), pic(#####)
                 print at(14,28);hex(84);cnt$;

L01290:       val% = val% + 1%
              if val% > 3025% then val% = 3025%    /* One (1) Less Max */
              val$(val%)  = str(readkey$,10%,15%)  /* (89) PAGES       */
              vald$(val%) = desc$
              goto load_next
L01340:    val_max% = val%
           xx% = (val_max% / 34%) + 1%
           convert xx% to str(pageno$,14%,3%), pic(###)

           convert cnt% to str(cnt$,10%,5%), pic(#####)
           if cnt% <> val_max% then str(cnt$,15%,1%) = "*"  /*EWD001*/
        return

        REM *************************************************************~
            *           D I S P L A Y   C O D E   T A B L E             *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_codes
/*EWD001*/  dl = 38 - vl            /* Description Length   */
/* Begin*/  d1 = 2  + vl + 1        /* Descr. #1 Start      */
            v2 = d1 + dl + 1        /* Value #2 Start       */
            d2 = v2 + vl + 1        /* Descr. #2 Start      */
            if dl >= 30 then goto skip_redim
                mat redim valwk$(3026)dl
                mat redim dash$(1)dl
          skip_redim
            for x% = 1% to val_max%
                valwk$(x%) = vald$(x%)
            next x%
/*EWD001*/  init("-") dash$()
/* End  */  call "STRING" addr("CT",t_hdr$(30),30%)

L01480:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(84)), cnt$                   , ch(16),~
/*EWD001*/     at (01,30), "Table ID = ",                                ~
/*EWD001*/     at (01,41), fac(hex(8c)), tab$(tab%)             , ch(09),~
               at (01,62), fac(hex(84)), pageno$                , ch(16),~
                                                                         ~
               at (02,25), fac(hex(84)), t_hdr$(tab%)           , ch(30),~
                                                                         ~
/*EWD001*/     at (04,02), "Code",                                       ~
/* Begin*/     at (04,d1), "Description",                                ~
               at (05,02), fac(hex(8c)), str(dash$(1),,vl)      , ch(15),~
               at (05,d1), fac(hex(8c)), str(dash$(1),,dl)      ,        ~
                                                                         ~
               at (04,v2), "Code",                                       ~
               at (04,d2), "Description",                                ~
               at (05,v2), fac(hex(8c)), str(dash$(1),,vl)      , ch(15),~
               at (05,d2), fac(hex(8c)), str(dash$(1),,dl)      ,        ~
                                                                         ~
               at (06,02), fac(hex(84)), val$(k% + 1%)          , ch(15),~
               at (06,d1), fac(hex(84)), str(valwk$(k%+1%),,dl) ,        ~
                                                                         ~
               at (06,v2), fac(hex(84)), val$(k% + 18%)         , ch(15),~
               at (06,d2), fac(hex(84)), str(valwk$(k%+18%),,dl),        ~
                                                                         ~
               at (07,02), fac(hex(84)), val$(k% + 2%)          , ch(15),~
               at (07,d1), fac(hex(84)), str(valwk$(k%+2%),,dl) ,        ~
                                                                         ~
               at (07,v2), fac(hex(84)), val$(k% + 19%)         , ch(15),~
               at (07,d2), fac(hex(84)), str(valwk$(k%+19%),,dl),        ~
                                                                         ~
               at (08,02), fac(hex(84)), val$(k% + 3%)          , ch(15),~
               at (08,d1), fac(hex(84)), str(valwk$(k%+3%),,dl) ,        ~
                                                                         ~
               at (08,v2), fac(hex(84)), val$(k% + 20%)         , ch(15),~
               at (08,d2), fac(hex(84)), str(valwk$(k%+20%),,dl),        ~
                                                                         ~
               at (09,02), fac(hex(84)), val$(k% + 4%)          , ch(15),~
               at (09,d1), fac(hex(84)), str(valwk$(k%+4%),,dl) ,        ~
                                                                         ~
               at (09,v2), fac(hex(84)), val$(k% + 21%)         , ch(15),~
               at (09,d2), fac(hex(84)), str(valwk$(k%+21),,dl) ,        ~
                                                                         ~
               at (10,02), fac(hex(84)), val$(k% + 5%)          , ch(15),~
               at (10,d1), fac(hex(84)), str(valwk$(k%+5%),,dl) ,        ~
                                                                         ~
               at (10,v2), fac(hex(84)), val$(k% + 22%)         , ch(15),~
               at (10,d2), fac(hex(84)), str(valwk$(k%+22%),,dl),        ~
                                                                         ~
               at (11,02), fac(hex(84)), val$(k% + 6%)          , ch(15),~
               at (11,d1), fac(hex(84)), str(valwk$(k%+6%),,dl) ,        ~
                                                                         ~
               at (11,v2), fac(hex(84)), val$(k% + 23%)         , ch(15),~
               at (11,d2), fac(hex(84)), str(valwk$(k%+23%),,dl),        ~
                                                                         ~
               at (12,02), fac(hex(84)), val$(k% + 7%)          , ch(15),~
               at (12,d1), fac(hex(84)), str(valwk$(k%+7%),,dl) ,        ~
                                                                         ~
               at (12,v2), fac(hex(84)), val$(k% + 24%)         , ch(15),~
               at (12,d2), fac(hex(84)), str(valwk$(k%+24%),,dl),        ~
                                                                         ~
               at (13,02), fac(hex(84)), val$(k% + 8%)          , ch(15),~
               at (13,d1), fac(hex(84)), str(valwk$(k%+8%),,dl) ,        ~
                                                                         ~
               at (13,v2), fac(hex(84)), val$(k% + 25%)         , ch(15),~
               at (13,d2), fac(hex(84)), str(valwk$(k%+25%),,dl),        ~
                                                                         ~
               at (14,02), fac(hex(84)), val$(k% + 9%)          , ch(15),~
               at (14,d1), fac(hex(84)), str(valwk$(k%+9%),,dl) ,        ~
                                                                         ~
               at (14,v2), fac(hex(84)), val$(k% + 26%)         , ch(15),~
               at (14,d2), fac(hex(84)), str(valwk$(k%+26%),,dl),        ~
                                                                         ~
               at (15,02), fac(hex(84)), val$(k% + 10%)         , ch(15),~
               at (15,d1), fac(hex(84)), str(valwk$(k%+10%),,dl),        ~
                                                                         ~
               at (15,v2), fac(hex(84)), val$(k% + 27%)         , ch(15),~
               at (15,d2), fac(hex(84)), str(valwk$(k%+27%),,dl),        ~
                                                                         ~
               at (16,02), fac(hex(84)), val$(k% + 11%)         , ch(15),~
               at (16,d1), fac(hex(84)), str(valwk$(k%+11%),,dl),        ~
                                                                         ~
               at (16,v2), fac(hex(84)), val$(k% + 28%)         , ch(15),~
               at (16,d2), fac(hex(84)), str(valwk$(k%+28%),,dl),        ~
                                                                         ~
               at (17,02), fac(hex(84)), val$(k% + 12%)         , ch(15),~
               at (17,d1), fac(hex(84)), str(valwk$(k%+12%),,dl),        ~
                                                                         ~
               at (17,v2), fac(hex(84)), val$(k% + 29%)         , ch(15),~
               at (17,d2), fac(hex(84)), str(valwk$(k%+29%),,dl),        ~
                                                                         ~
               at (18,02), fac(hex(84)), val$(k% + 13%)         , ch(15),~
               at (18,d1), fac(hex(84)), str(valwk$(k%+13%),,dl),        ~
                                                                         ~
               at (18,v2), fac(hex(84)), val$(k% + 30%)         , ch(15),~
               at (18,d2), fac(hex(84)), str(valwk$(k%+30%),,dl),        ~
                                                                         ~
               at (19,02), fac(hex(84)), val$(k% + 14%)         , ch(15),~
               at (19,d1), fac(hex(84)), str(valwk$(k%+14%),,dl),        ~
                                                                         ~
               at (19,v2), fac(hex(84)), val$(k% + 31%)         , ch(15),~
               at (19,d2), fac(hex(84)), str(valwk$(k%+31%),,dl),        ~
                                                                         ~
               at (20,02), fac(hex(84)), val$(k% + 15%)         , ch(15),~
               at (20,d1), fac(hex(84)), str(valwk$(k%+15%),,dl),        ~
                                                                         ~
               at (20,v2), fac(hex(84)), val$(k% + 32%)         , ch(15),~
               at (20,d2), fac(hex(84)), str(valwk$(k%+32%),,dl),        ~
                                                                         ~
               at (21,02), fac(hex(84)), val$(k% + 16%)         , ch(15),~
               at (21,d1), fac(hex(84)), str(valwk$(k%+16%),,dl),        ~
                                                                         ~
               at (21,v2), fac(hex(84)), val$(k% + 33%)         , ch(15),~
               at (21,d2), fac(hex(84)), str(valwk$(k%+33%),,dl),        ~
                                                                         ~
               at (22,02), fac(hex(84)), val$(k% + 17%)         , ch(15),~
               at (22,d1), fac(hex(84)), str(valwk$(k%+17%),,dl),        ~
                                                                         ~
               at (22,v2), fac(hex(84)), val$(k% + 34%)         , ch(15),~
               at (22,d2), fac(hex(84)), str(valwk$(k%+34%),,dl),        ~
                                                                         ~
               at (24,02), fac(hex(a4)), str(pf$(1%),,41%)      , ch(41),~
/*EWD001*/     at (24,44), fac(hex(a9)), find_val$              , ch(15),~
/* End  */     at (24,60), fac(hex(a4)), str(pf$(1%),60%,20%)   , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L02710             /* First    */
L02680:           k% = 0%
                  goto L01480

L02710:        if keyhit% <> 3% then goto L02760             /* Last      */
L02720:           x% = int(val_max% / 34%)
                  k% = (x%*34%)
                  goto L01480

L02760:        if keyhit% <> 4% then goto L02820             /* Previous */
                  if k% < 35% then goto L02680
                  k% = k% - 34%
                  if k% <= 1% then goto L02680
                  goto L01480

L02820:        if keyhit% <> 5% then goto L02850             /* Next     */
                  k% = k% + 34%
                  if k% < val_max% then goto L01480
                  goto L02720

L02850:        if keyhit% <> 8% then goto L02870             /* Find     */
                  for x% = 1% to val_max%                /* EWD001 - New */
                      if val$(x%) >= find_val$ then k% = x% - 1% else    ~
                  next x%
                  goto L01480

L02870:        if keyhit% <> 15 then goto L02910
                  call "PRNTSCRN"
                  goto L01480

L02910:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            xx% = (k% / 34%) + 1%
            convert xx% to str(pageno$,7%,3%), pic(###)
/*EWD001*/  if k% > 0% and k% - 34% < 0% then str(pageno$,7%,3%) = "<--"
/* Begin*/  if k% + 34% > val_max% and mod(k% + 34%, 34%) <> 0%          ~
                then str(pageno$,7%,3%) = "END"

            pf$(1) = "(2)First (3)Last (4)Prev (5)Next (8)Find" &        ~
/*EWD001*/           ": xxxxxxxxxxxxxxx  (15)Prt <Enter> Cont"
/* End  */  pfkeys$ = hex(ff02030405ffff08ffffffffffff0f1000)
            gosub check_screen      
            return

        check_screen
            if val_max% > 34% then goto L03120
               gosub no_first      
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L03120:      if k% >= 34% then goto L03150
                gosub no_prev
                gosub no_first     
L03150:      if (k% + 34%) <= val_max% then goto L03170
                gosub no_last
L03170:      if k% <= (val_max% - 34%) then goto L03190
                gosub no_next
L03190: return
        no_first
/*EWD001*/  if k% <> 0% then return
/*EWD001*/  str(pf$(1%),1%, 9%) = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
/*EWD001*/  str(pf$(1%),26%,8%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
/*EWD001*/  str(pf$(1%),10%,8%) = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
/*EWD001*/  str(pf$(1%),18%,8%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        REM *************************************************************~
            *             P R I N T   C O D E   T A B L E               *~
            *-----------------------------------------------------------*~
            * Print Report                                              *~
            *************************************************************

        print_codes
            ln% = 99%  :  pg% = 0%
            select printer
            for x% = 1% to val_max%
                gosub generate_report
            next x%
            close printer
            return

        generate_report
            if ln% > 58% then gosub print_headings
            print using L60050, val$(x%), vald$(x%)
            ln% = ln% + 1%
            return

        print_headings
            pg% = pg% + 1%
            print page
            print using L60010, date$, time$, userid$, pg%
            print using L60020, cnt$, tab$(tab%), t_hdr$(tab%)
            print
            print using L60030
            print using L60040
            ln% = 5%
            return

L60010: %  Date: ########  Time: ########                GENCODES Table L~
        ~isting                                    User: ###  Page: ####

L60020: %  ################                       Table Name = ######### ~
        ~  ( ############################## )

L60030: % Code              Description                 

L60040: % ---------------   ------------------------------

L60050: % ###############   ##############################

