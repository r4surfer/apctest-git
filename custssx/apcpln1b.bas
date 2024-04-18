        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN1B                             *~
            *  Creation Date     - 12/27/95                             *~
            *  Last Modified Date- 05/16/05                             *~
            *  Description       - Display Entries for Valid Planning   *~
            *                      Tables. Tables Listed Below.         *~
            *                                                           *~
            *  Special Comments                                         *~
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
            *     21%     (ELLISON01)     26%  (RGASTATUS)      (AWD001)*~
            *     22%     (ELLISON02)     27%  (RGAREASON)      (AWD001)*~
            *     23%     (ELLISON03)     28%  (RGATR-LOC)      (AWD001)*~
            *     24%     (ELLISON04)     29%  (         )              *~
            *     25%     (ELLISON05)     30%  (RGAPRDTYP)              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/18/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 04/01/97 ! Mod to add New Table Files for New       ! RHH *~
            *          !   Family and Product changes>            !     *~
            * 11/13/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 05/16/05 ! (AWD001) Add the tables for the new RGA  ! RHH *~
            *          !   System. Three (3) Tables 26%,27%,28%   ! RHH *~ 
			* 02/15/23 ! CR3233 Add Table 30%                     ! RDB *~
            *************************************************************

            sub "APCPLN1B" ( tab%,       /* Current Planning Tables    */~
                             #1 )        /* FILE = (GENCODES)          */

        dim                              /* Subroutine - Variables     */~
            tab$(30%)10, t_hdr$(30%)25,  /* Code Table Names           */~
            val$(1020%)6, desc$30,       /* Table Value and Description*/~
            vald$(1020%)30,              /* Table Descriptions         */~
            readkey$24,                  /* Table Lookup Key           */~
            cursor%(2%),                 /* Cursor Location for Edit   */~
            i$(24%)80,                   /* SCREEN IMAGE               */~
            pf$(3%)79,                   /* PF Key Description         */~
            pageno$16, cnt$16,           /* Page Counter Script        */~
            pfkeys$32                    /* PF Key Values              */

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


            tab$(21%) = "ELLISON01" : tab$(26%) = "RGASTATUS"
            tab$(22%) = "ELLISON02" : tab$(27%) = "RGAREASON"
            tab$(23%) = "ELLISON03" : tab$(28%) = "RGATR-LOC"
            tab$(24%) = "ELLISON04" : tab$(29%) = "EQUATYPE "
            tab$(25%) = "ELLISON05" : tab$(30%) = "RGAPRDTYP"

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
            t_hdr$(26%) = "RGA Salvage Tracking Cds "         /* (AWD001)  */
            t_hdr$(27%) = "RGA Reason Code W/Return "         /* (AWD001)  */
            t_hdr$(28%) = "RGA Trailer/Location     "         /* (AWD001)  */
            t_hdr$(29%) = "                         "
            t_hdr$(30%) = "RGA Product Type         "

            pageno$ = "Page: XXX of XXX"

            if tab% = 0% then tab% = 1%

            gosub load_table
            gosub display_codes

        end

        load_table
           call "SHOSTAT" ("Loading " & t_hdr$(tab%))
           init(" ") val$(), vald$(), readkey$
           cnt% = 0% : cnt$ = "Codes: [ XXXXX ]"
           k% = 0%
           val_max% = 0%
           val% = 0%
           str(readkey$,1%,9%)  = tab$(tab%)
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
                 print at(01,02);hex(84);cnt$;

L01290:       val% = val% + 1%
              if val% > 1019% then val% = 1019%    /* One (1) Less Max */
              val$(val%)  = str(readkey$,10%,6%)   /* (30) PAGES       */
              vald$(val%) = desc$
              goto load_next
L01340:    val_max% = val%
           xx% = (val_max% / 34%) + 1%
           convert xx% to str(pageno$,14%,3%), pic(###)

           convert cnt% to str(cnt$,10%,5%), pic(#####)
        return

        REM *************************************************************~
            *           D I S P L A Y   C O D E   T A B L E             *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_codes
L01480:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(84)), cnt$                   , ch(16),~
               at (01,62), fac(hex(84)), pageno$                , ch(16),~
                                                                         ~
               at (02,28), fac(hex(84)), t_hdr$(tab%)           , ch(30),~
                                                                         ~
               at (04,02), "Code   Description                   ",      ~
               at (05,02), "------ ------------------------------",      ~
                                                                         ~
               at (04,40), "Code   Description                   ",      ~
               at (05,40), "------ ------------------------------",      ~
                                                                         ~
               at (06,02), fac(hex(84))  , val$(k% + 1%)        , ch(06),~
               at (06,09), fac(hex(84))  , vald$(k% + 1%)       , ch(30),~
                                                                         ~
               at (06,40), fac(hex(84))  , val$(k% + 18%)       , ch(06),~
               at (06,47), fac(hex(84))  , vald$(k% + 18%)      , ch(30),~
                                                                         ~
               at (07,02), fac(hex(84))  , val$(k% + 2%)        , ch(06),~
               at (07,09), fac(hex(84))  , vald$(k% + 2%)       , ch(30),~
                                                                         ~
               at (07,40), fac(hex(84))  , val$(k% + 19%)       , ch(06),~
               at (07,47), fac(hex(84))  , vald$(k% + 19%)      , ch(30),~
                                                                         ~
               at (08,02), fac(hex(84))  , val$(k% + 3%)        , ch(06),~
               at (08,09), fac(hex(84))  , vald$(k% + 3%)       , ch(30),~
                                                                         ~
               at (08,40), fac(hex(84))  , val$(k% + 20%)       , ch(06),~
               at (08,47), fac(hex(84))  , vald$(k% + 20%)      , ch(30),~
                                                                         ~
               at (09,02), fac(hex(84))  , val$(k% + 4%)        , ch(06),~
               at (09,09), fac(hex(84))  , vald$(k% + 4%)       , ch(30),~
                                                                         ~
               at (09,40), fac(hex(84))  , val$(k% + 21%)       , ch(06),~
               at (09,47), fac(hex(84))  , vald$(k% + 21%)      , ch(30),~
                                                                         ~
               at (10,02), fac(hex(84))  , val$(k% + 5%)        , ch(06),~
               at (10,09), fac(hex(84))  , vald$(k% + 5%)       , ch(30),~
                                                                         ~
               at (10,40), fac(hex(84))  , val$(k% + 22%)       , ch(06),~
               at (10,47), fac(hex(84))  , vald$(k% + 22%)      , ch(30),~
                                                                         ~
               at (11,02), fac(hex(84))  , val$(k% + 6%)        , ch(06),~
               at (11,09), fac(hex(84))  , vald$(k% + 6%)       , ch(30),~
                                                                         ~
               at (11,40), fac(hex(84))  , val$(k% + 23%)       , ch(06),~
               at (11,47), fac(hex(84))  , vald$(k% + 23%)      , ch(30),~
                                                                         ~
               at (12,02), fac(hex(84))  , val$(k% + 7%)        , ch(06),~
               at (12,09), fac(hex(84))  , vald$(k% + 7%)       , ch(30),~
                                                                         ~
               at (12,40), fac(hex(84))  , val$(k% + 24%)       , ch(06),~
               at (12,47), fac(hex(84))  , vald$(k% + 24%)      , ch(30),~
                                                                         ~
               at (13,02), fac(hex(84))  , val$(k% + 8%)        , ch(06),~
               at (13,09), fac(hex(84))  , vald$(k% + 8%)       , ch(30),~
                                                                         ~
               at (13,40), fac(hex(84))  , val$(k% + 25%)       , ch(06),~
               at (13,47), fac(hex(84))  , vald$(k% + 25%)      , ch(30),~
                                                                         ~
               at (14,02), fac(hex(84))  , val$(k% + 9%)        , ch(06),~
               at (14,09), fac(hex(84))  , vald$(k% + 9%)       , ch(30),~
                                                                         ~
               at (14,40), fac(hex(84))  , val$(k% + 26%)       , ch(06),~
               at (14,47), fac(hex(84))  , vald$(k% + 26%)      , ch(30),~
                                                                         ~
               at (15,02), fac(hex(84))  , val$(k% + 10%)       , ch(06),~
               at (15,09), fac(hex(84))  , vald$(k% + 10%)      , ch(30),~
                                                                         ~
               at (15,40), fac(hex(84))  , val$(k% + 27%)       , ch(06),~
               at (15,47), fac(hex(84))  , vald$(k% + 27%)      , ch(30),~
                                                                         ~
               at (16,02), fac(hex(84))  , val$(k% + 11%)       , ch(06),~
               at (16,09), fac(hex(84))  , vald$(k% + 11%)      , ch(30),~
                                                                         ~
               at (16,40), fac(hex(84))  , val$(k% + 28%)       , ch(06),~
               at (16,47), fac(hex(84))  , vald$(k% + 28%)      , ch(30),~
                                                                         ~
               at (17,02), fac(hex(84))  , val$(k% + 12%)       , ch(06),~
               at (17,09), fac(hex(84))  , vald$(k% + 12%)      , ch(30),~
                                                                         ~
               at (17,40), fac(hex(84))  , val$(k% + 29%)       , ch(06),~
               at (17,47), fac(hex(84))  , vald$(k% + 29%)      , ch(30),~
                                                                         ~
               at (18,02), fac(hex(84))  , val$(k% + 13%)       , ch(06),~
               at (18,09), fac(hex(84))  , vald$(k% + 13%)      , ch(30),~
                                                                         ~
               at (18,40), fac(hex(84))  , val$(k% + 30%)       , ch(06),~
               at (18,47), fac(hex(84))  , vald$(k% + 30%)      , ch(30),~
                                                                         ~
               at (19,02), fac(hex(84))  , val$(k% + 14%)       , ch(06),~
               at (19,09), fac(hex(84))  , vald$(k% + 14%)      , ch(30),~
                                                                         ~
               at (19,40), fac(hex(84))  , val$(k% + 31%)       , ch(06),~
               at (19,47), fac(hex(84))  , vald$(k% + 31%)      , ch(30),~
                                                                         ~
               at (20,02), fac(hex(84))  , val$(k% + 15%)       , ch(06),~
               at (20,09), fac(hex(84))  , vald$(k% + 15%)      , ch(30),~
                                                                         ~
               at (20,40), fac(hex(84))  , val$(k% + 32%)       , ch(06),~
               at (20,47), fac(hex(84))  , vald$(k% + 32%)      , ch(30),~
                                                                         ~
               at (21,02), fac(hex(84))  , val$(k% + 16%)       , ch(06),~
               at (21,09), fac(hex(84))  , vald$(k% + 16%)      , ch(30),~
                                                                         ~
               at (21,40), fac(hex(84))  , val$(k% + 33%)       , ch(06),~
               at (21,47), fac(hex(84))  , vald$(k% + 33%)      , ch(30),~
                                                                         ~
               at (22,02), fac(hex(84))  , val$(k% + 17%)       , ch(06),~
               at (22,09), fac(hex(84))  , vald$(k% + 17%)      , ch(30),~
                                                                         ~
               at (22,40), fac(hex(84))  , val$(k% + 34%)       , ch(06),~
               at (22,47), fac(hex(84))  , vald$(k% + 34%)      , ch(30),~
                                                                         ~
               at (24,02), fac(hex(a4)), pf$(1%)                , ch(79),~
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

L02820:        if keyhit% <> 5% then goto L02870             /* Next     */
                  k% = k% + 34%
                  if k% < val_max% then goto L01480
                  goto L02720

L02870:        if keyhit% <> 15 then goto L02910
                  call "PRNTSCRN"
                  goto L01480

L02910:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            xx% = (k% / 34%) + 1%
            convert xx% to str(pageno$,7%,3%), pic(###)

            pf$(1) = "(2)First     (3)Last     (4)Previous    " &        ~
                     " (5)Next (15)Print Screen <Return> Cont"
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
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
                gosub no_first
                gosub no_prev
L03150:      if (k% + 34%) <= val_max% then goto L03170
                gosub no_last
L03170:      if k% <= (val_max% - 34%) then goto L03190
                gosub no_next
L03190: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),41%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(1%),14%,9%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(1%),26%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

