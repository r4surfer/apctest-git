        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLNDD                             *~
            *  Creation Date     - 05/24/96                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - Utility for Debugging Cost Utilities.*~
            *                                                           *~
            *  Selections - 01 = LAB(), AVG_PAY(), UPH()                *~
            *               02 = TC(), TC(19%,20%,21%)                  *~
            *               03 = RM_MAT(), RM_MATS()                    *~
            *               04 = TT()                                   *~
            *               05 = APC_ERR%()                             *~
            *               06 = Cost Explosion Labor                   *~
            *               07 = Cost Explosion Material                *~
            *               08 = Cost Explosion Glass                   *~
            *               09 = Cost Explosion Screen                  *~
            *               10 = Cost Explosion Hardware                *~
            *               11 = Cost Explosion Package                 *~
            *===========================================================*~
            *                                                           *~
            *  LAB()- Assoc. labor Cost      TC()- Assoc. Mat'l Cost    *~
            *   1% = Dir Prod Line Labor      1% = Mat'l Vinyl          *~
            *   2% = Dir Glass labor          2% = Mat'l Misc           *~
            *   3% = Dir Screen Labor         3% = Mat'l Total          *~
            *   4% = Ind Mat'l Labor          4% = Glass Mat'l Vinyl    *~
            *   5% = Ind Staging Labor        5% = Glass Mat'l Misc     *~
            *   6% = Ind Loading Labor        6% = Glass Mat'l Total    *~
            *   7% = Std Indirect Labor       7% = Scrn Mat'l Vinyl     *~
            *   8% = Total Dir + Ind          8% = Scrn Mat'l Misc      *~
            *   9% = Tot Labor Overhead       9% = Scrn Mat'l Total     *~
            *  10% = Total Labor             10% = Lock Mat'l Vinyl     *~
            *                                11% = Lock Mat'l Misc      *~
            *  AVG_PAY() - Avg Pay           12% = Lock Mat'l Total     *~
            *  UPH() - Unit per Manhour      13% = Hardware Mat'l Vinyl *~
            *    1% = Mfg Direct             14% = Hardware Mat'l Misc  *~
            *    4% = Glass Direct           15% = Hardware Mat'l Total *~
            *    6% = Screen Direct          16% = Packaging Mat'l Vinyl*~
            *    7% = Material Indirect      17% = Packaging Mat'l Misc *~
            *    8% = Staging Indirect       18% = Packaging Mat'l Total*~
            *    9% = Loading Indirect       19% = Total Freight Amount *~
            *                                20% = Total Vinyl Discount *~
            *                                21% = Total Cost Wood Surr *~
            *===========================================================*~
            *                                                           *~
            *  RM_MAT()- Mat'l Cost          RM_MATS()- Mat'l Scrap Cost*~
            *    1% = Vinyl Mat'l Cost         1% = Vinyl Mat'l Scrap   *~
            *    2% = Misc Mat'l Cost          2% = Misc Mat'l Scrap    *~
            *    3% = Total Mat'l Cost         3% = Total Mat'l Scrap   *~
            *    4% = Vinyl Glass Cost         4% = Vinyl Glass Scrap   *~
            *    5% = Misc Glass Cost          5% = Misc Glass Scrap    *~
            *    6% = Total Glass Cost         6% = Total Glass Scrap   *~
            *    7% = Vinyl Scrn Cost          7% = Vinyl Scrn Cost     *~
            *    8% = Misc Scrn Cost           8% = Misc Scrn Cost      *~
            *    9% = Total Scrn Cost          9% = Total Scrn Cost     *~
            *   10% = Vinyl Lock Cost                                   *~
            *   11% = Misc Lock Cost                                    *~
            *   12% = Total Lock Cost                                   *~
            *   13% = Vinyl Hdr Cost                                    *~
            *   14% = Misc Hdr Cost                                     *~
            *   15% = Total Hdr Cost                                    *~
            *   16% = Vinyl Pck Cost                                    *~
            *   17% = Misc Pck Cost                                     *~
            *   18% = Total Pck Cost                                    *~
            *===========================================================*~
            *                                                           *~
            *  TT()- Cost Total Buckets      Type Values                *~
            *    1% = Total Vinyl Cost         1% = Inv Raw Materials   *~
            *    2% = Total Vinyl Adj Cost     2% = Glass Materials     *~
            *    3% = Total Glass Cost         3% = Screen Materials    *~
            *    4% = Total Screen Cost        4% = Lock(s) Materials   *~
            *    5% = Total Hardware Cost      5% = Hardware Materials  *~
            *    6% = Total Packaging Cost     6% = Packaging Materials *~
            *    7% = Total Misc Cost          7% = ( Available )       *~
            *    8% = Total Material Cost                               *~
            *    9% = Total Materail Adj                                *~
            *   10% = Total Product Cost                                *~
            *                                                           *~
            *===========================================================*~
            *                                                           *~
            *   ? = Type Value ( 1 thru 7 )                             *~
            *  ?? = Number of Entries ( 1 thru 50 )                     *~
            *                                                           *~
            *  TMP$(?%,??%)                 Store all Raw Mat'l Part No *~
            *  TMC(?%,??%)                  Assoc. Cut Inches In Dec.   *~
            *  TMCT(?%,??%)                 Assoc. Total Cost Raw Mat'l *~
            *  TMU%(?%,??%)                 Assoc. Unit of Measure      *~
            *  TMD$(?%,??%)                 Assoc. Raw Mat'l Desc       *~
            *  TMUC(?%,??%)                 Assoc. Raw Mat'l Unit Cost  *~
            *  TMSI(?%,??%),                Assoc. Scrap Inches Decimal *~
            *  TMSC(?%,??%),                Assoc. Scrap Mat'l Cost     *~
            *  TMEQ$(?%,??%),               Assoc. Calc Type Eq. No.    *~
            *  TMPH$(?%,??%),               Assoc. Phantom Designature  *~
            *  TCNT%(?%),                   Assoc. Count for Each Type  *~
            *                                                           *~
            *===========================================================*~
            *                                                           *~
            *   FF%() - File Channels                                   *~
            *    1% = (APCCUTEQ)            10% = (APCCSTLR)            *~
            *    2% = (HNYMASTR)            11% = (CPRPRICE)            *~
            *    3% = (HNYQUAN )            12% = (CUSTOMER)            *~
            *    4% = (GENCDSIN)            13% = (APCPCMST)            *~
            *    5% = (AMTBOMCD)            14% = (APCSKUNO)            *~
            *    6% =                       15% = (APCPCMSK)            *~
            *    7% = (APCEMPLY)            16% = (APCPCMSD)            *~
            *    8% = (APCEQUAT)                                        *~
            *    9% = (APCCSTHP)                                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/24/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            *************************************************************

            sub "APCPLNDD" ( sel$,       /* Debug Screen Selection     */~
                             cuscode$,   /* Customer Code for Pricing  */~
                             part$,      /* MFG Part Number            */~
                             lab(),      /* Labor Costs (1 thru 10)    */~
                             avg_pay(),  /* Avg Pay per Dept           */~
                             uph(),      /* Avg Units Per Manhour Dept */~
                             tc(),       /* Material Costs (1 thru 20) */~
                             tt(),       /* Total Cost Buckets         */~
                             rm_mat(),   /* Material Costs (1 thru 10) */~
                             rm_mats(),  /* Mat'l Scrap Costs(1 thru 9)*/~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */

        dim                              /* Costing Variables          */~
            part$25,                     /* MFG Part Number            */~
            lab(10%),                    /* Breakdown of Labor Cost    */~
            avg_pay(15%),                /* Avg Pay By Dept            */~
            uph(15%),                    /* Avg Unit Per Manhour Dept  */~
            tc(25%),                     /* Total Cost's               */~
            tt(25%),                     /* Cost Total Buckets         */~
            rm_mat(20%),                 /* Total Vinyl,Misc, Mat      */~
            rm_mats(20%),                /* Total Vinyl,Misc, Mat Scrap*/~
            apc_err%(20%), err$(30%)20,  /* Store Error Code each Modul*/~
            inpmessage$79,               /* INPUT MESSAGE              */~
            cursor%(2%), date$8,         /* Cursor Location for Edit   */~
            i$(24%)80, runtime$8,        /* SCREEN IMAGE               */~
            pf$(3%)79,                   /* PF Key Description         */~
            pfkeys$32,                   /* PF Key Values              */~
            cuscode$9                    /* Customer Code              */

        dim                              /* Subroutine - Variables     */~
            hdr1$40, hdr2$40,            /* Screen Header Data         */~
            cc$(30%)37,sel$2,            /* Data Description           */~
            sel_d$30,                    /* INPUT MESSAGE              */~
            ln1$79                       /* 1st Line Sscreen Display   */

            err$(1% ) = "(Error) In Labor Cal"
            err$(2% ) = "(Error) in Material "
            err$(3% ) = "(Error) in Glass    "
            err$(4% ) = "(Error) in Screen   "
            err$(5% ) = "(Error) in Locks    "
            err$(6% ) = "(Error) in Hardware "
            err$(7% ) = "(Error) in Packaging"
            err$(8% ) = "(Error) in Pricing  "
            err$(9% ) = "                    "
            err$(10%) = "                    "
            err$(11%) = "                    "
            err$(12%) = "(Err) CSTM Material "
            err$(13%) = "                    "
            err$(14%) = "                    "
            err$(15%) = "                    "
            err$(16%) = "(Err) CSTM Hardware "
            err$(17%) = "(Err) CSTM Packaging"
            err$(18%) = "                    "
            err$(19%) = "                    "
            err$(20%) = "                    "
            init(" ") runtime$
            date$ = date
            call "DATEFMT" (date$)
            call "TIME" (runtime$)

            str(ln1$,1%,19%) = date$ & " @ " & runtime$
            str(ln1$,23%,38%) = "Debug Part ("& part$ &")"
            str(ln1$,65%,14%) = "APCPLNDD: 1.00"
        main_line
            init(" ") hdr1$, hdr2$, cc$()
            if sel$ = "00" then goto exit_sub
            gosub check_select
            gosub display_screen
            if keyhit% = 16% or keyhit% = 0% then goto exit_sub
            goto main_line
        exit_sub
        end

        check_select
            sel% = 1%
            convert sel$ to sel%, data goto L01890
L01890:
            convert sel% to sel$, pic(00)

            on sel% gosub build_lab, build_tc, build_mat, total_cost,    ~
                          build_err, sc_lab, sc_mat, sc_glass, sc_screen,~
                          sc_hardware, sc_package
        return

        REM *************************************************************~
            *           D I S P L A Y   C O D E   T A B L E             *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_screen
L02040:     gosub set_debug
            accept                                                       ~
               at (01,02), fac(hex(84)), ln1$                   , ch(79),~
                                                                         ~
               at (02,02), "Enter Display Selection:",                   ~
               at (02,30), fac(hex(81)), sel$                   , ch(02),~
               at (02,40), fac(hex(84)), sel_d$                 , ch(30),~
                                                                         ~
               at (04,02), fac(hex(a4)), hdr1$                  , ch(37),~
               at (04,40), fac(hex(a4)), hdr2$                  , ch(37),~
                                                                         ~
               at (05,02), fac(hex(84))  , cc$(k% + 1%)         , ch(37),~
               at (05,40), fac(hex(84))  , cc$(k% + 16%)        , ch(37),~
                                                                         ~
               at (06,02), fac(hex(84))  , cc$(k% + 2%)         , ch(37),~
               at (06,40), fac(hex(84))  , cc$(k% + 17%)        , ch(37),~
                                                                         ~
               at (07,02), fac(hex(84))  , cc$(k% + 3%)         , ch(37),~
               at (07,40), fac(hex(84))  , cc$(k% + 18%)        , ch(37),~
                                                                         ~
               at (08,02), fac(hex(84))  , cc$(k% + 4%)         , ch(37),~
               at (08,40), fac(hex(84))  , cc$(k% + 19%)        , ch(37),~
                                                                         ~
               at (09,02), fac(hex(84))  , cc$(k% + 5%)         , ch(37),~
               at (09,40), fac(hex(84))  , cc$(k% + 20%)        , ch(37),~
                                                                         ~
               at (10,02), fac(hex(84))  , cc$(k% + 6%)         , ch(37),~
               at (10,40), fac(hex(84))  , cc$(k% + 21%)        , ch(37),~
                                                                         ~
               at (11,02), fac(hex(84))  , cc$(k% + 7%)         , ch(37),~
               at (11,40), fac(hex(84))  , cc$(k% + 22%)        , ch(37),~
                                                                         ~
               at (12,02), fac(hex(84))  , cc$(k% + 8%)         , ch(37),~
               at (12,40), fac(hex(84))  , cc$(k% + 23%)        , ch(37),~
                                                                         ~
               at (13,02), fac(hex(84))  , cc$(k% + 9%)         , ch(37),~
               at (13,40), fac(hex(84))  , cc$(k% + 24%)        , ch(37),~
                                                                         ~
               at (14,02), fac(hex(84))  , cc$(k% + 10%)        , ch(37),~
               at (14,40), fac(hex(84))  , cc$(k% + 25%)        , ch(37),~
                                                                         ~
               at (15,02), fac(hex(84))  , cc$(k% + 11%)        , ch(37),~
               at (15,40), fac(hex(84))  , cc$(k% + 26%)        , ch(37),~
                                                                         ~
               at (16,02), fac(hex(84))  , cc$(k% + 12%)        , ch(37),~
               at (16,40), fac(hex(84))  , cc$(k% + 27%)        , ch(37),~
                                                                         ~
               at (17,02), fac(hex(84))  , cc$(k% + 13%)        , ch(37),~
               at (17,40), fac(hex(84))  , cc$(k% + 28%)        , ch(37),~
                                                                         ~
               at (18,02), fac(hex(84))  , cc$(k% + 14%)        , ch(37),~
               at (18,40), fac(hex(84))  , cc$(k% + 29%)        , ch(37),~
                                                                         ~
               at (19,02), fac(hex(84))  , cc$(k% + 15%)        , ch(37),~
               at (19,40), fac(hex(84))  , cc$(k% + 30%)        , ch(37),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(84)), pf$(1%)                , ch(79),~
               at (23,02), fac(hex(84)), pf$(2%)                , ch(79),~
               at (24,02), fac(hex(84)), pf$(3%)                , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L02710             /* First    */
L02680:           k% = 0%
                  goto L02040

L02710:        if keyhit% <> 3% then goto L02760             /* Last      */
L02720:           x% = int(d_max% / 30%)
                  k% = (x%*30%)
                  goto L02040

L02760:        if keyhit% <> 4% then goto L02820             /* Previous */
                  if k% < 31% then goto L02680
                  k% = k% - 30%
                  if k% <= 1% then goto L02680
                  goto L02040

L02820:        if keyhit% <> 5% then goto L02870             /* Next     */
                  k% = k% + 30%
                  if k% < d_max% then goto L02040
                  goto L02720

L02870:        if keyhit% <> 15 then goto L02910
                  call "PRNTSCRN"
                  goto L02040

L02910:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_debug
            inpmessage$ =                                                ~
               "(Debug Display) Press <Return> or PF(16) To Continue?"

            d_max% = 30% : u3% = 0%
            pf$(1%)= "(2)First     (5)Next                    " &        ~
                     "                                       "
            pf$(2%)= "(3)Last                                 " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "(4)Previous                             " &        ~
                     "                       (16)Exit Screen "
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            gosub check_debug
            return

        check_debug
            if d_max% > 30% then goto L03170
               gosub n_first
               gosub n_next
               gosub n_last
               gosub n_prev
               return
L03170:      if k% >= 30% then goto L03200
                gosub n_first
                gosub n_prev
L03200:      if (k% + 30%) <= d_max% then goto L03220
                gosub n_last
L03220:      if k% <= (d_max% - 30%) then goto L03240
                gosub n_next
L03240: return
        n_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        n_next
            str(pf$(1%),14%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        n_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        n_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        build_lab
            sel_d$ = "Calc of Labor/Avg Pay/UPMH    "
            hdr1$ = "Calculation Description    * Values *"
            str(cc$(1%),1%,26%) = "Direct Product Line Labor:"
            convert lab(1%) to str(cc$(1%),28%,10%), pic(####.####-)

            str(cc$(2%),1%,26%) = "Direct Glass Labor       :"
            convert lab(2%) to str(cc$(2%),28%,10%), pic(####.####-)

            str(cc$(3%),1%,26%) = "Direct Screen Labor      :"
            convert lab(3%) to str(cc$(3%),28%,10%), pic(####.####-)

            str(cc$(4%),1%,26%) = "Indirect Material Labor  :"
            convert lab(4%) to str(cc$(4%),28%,10%), pic(####.####-)

            str(cc$(5%),1%,26%) = "Indirect Staging Labor   :"
            convert lab(5%) to str(cc$(5%),28%,10%), pic(####.####-)

            str(cc$(6%),1%,26%) = "Indirect Loading Labor   :"
            convert lab(6%) to str(cc$(6%),28%,10%), pic(####.####-)

            str(cc$(7%),1%,26%) = "Standard Indirect Labor  :"
            convert lab(7%) to str(cc$(7%),28%,10%), pic(####.####-)

            str(cc$(8%),1%,26%) = "Total Direct and Indirect:"
            convert lab(8%) to str(cc$(8%),28%,10%), pic(####.####-)

            str(cc$(9%),1%,26%) = "Total Labor Overhead     :"
            convert lab(9%) to str(cc$(9%),28%,10%), pic(####.####-)

            str(cc$(10%),1%,26%) = "Total Product Labor      :"
            convert lab(10%) to str(cc$(10%),28%,10%), pic(####.####-)
        REM
            hdr2$ = "Department     Average Pay *  UPMH  *"
            str(cc$(16%),1%,14%) = "MFG Department"
            convert avg_pay(1%) to str(cc$(16%),16%,10%),pic(####.####-)
            convert uph(1%) to str(cc$(16%),28%,10%), pic(####.####-)

            str(cc$(19%),1%,14%) = "Glass Insulate"
            convert avg_pay(4%) to str(cc$(19%),16%,10%),pic(####.####-)
            convert uph(4%) to str(cc$(19%),28%,10%), pic(####.####-)

            str(cc$(20%),1%,14%) = "Glass Cutting "
            convert avg_pay(5%) to str(cc$(20%),16%,10%),pic(####.####-)
            convert uph(5%) to str(cc$(20%),28%,10%), pic(####.####-)

            str(cc$(21%),1%,14%) = "Screen Direct "
            convert avg_pay(6%) to str(cc$(21%),16%,10%),pic(####.####-)
            convert uph(6%) to str(cc$(21%),28%,10%), pic(####.####-)

            str(cc$(22%),1%,14%) = "Material Indir"
            convert avg_pay(7%) to str(cc$(22%),16%,10%),pic(####.####-)
            convert uph(7%) to str(cc$(22%),28%,10%), pic(####.####-)

            str(cc$(23%),1%,14%) = "Staging Indir "
            convert avg_pay(8%) to str(cc$(23%),16%,10%),pic(####.####-)
            convert uph(8%) to str(cc$(23%),28%,10%), pic(####.####-)

            str(cc$(24%),1%,14%) = "Loading Indir "
            convert avg_pay(9%) to str(cc$(24%),16%,10%),pic(####.####-)
            convert uph(9%) to str(cc$(24%),28%,10%), pic(####.####-)

            str(cc$(27%),1%,14%) = "Wood Surr/Fact"
            convert avg_pay(12%) to str(cc$(27%),16%,10%),pic(####.####-)
            convert uph(12%) to str(cc$(27%),28%,10%), pic(####.####-)

        return

        build_tc
            sel_d$ = "Total Mat'l Cost Include Scrap"
            hdr1$ = "Department *Vinyl * * Misc.* * Total*"
            str(cc$(1%),1%,10%) = "Material  "
            convert tc(1%) to str(cc$(1%),12%,8%), pic(####.##-)
            convert tc(2%) to str(cc$(1%),21%,8%), pic(####.##-)
            convert tc(3%) to str(cc$(1%),30%,8%), pic(####.##-)

            str(cc$(2%),1%,10%) = "Glass     "
            convert tc(4%) to str(cc$(2%),12%,8%), pic(####.##-)
            convert tc(5%) to str(cc$(2%),21%,8%), pic(####.##-)
            convert tc(6%) to str(cc$(2%),30%,8%), pic(####.##-)

            str(cc$(3%),1%,10%) = "Screen    "
            convert tc(7%) to str(cc$(3%),12%,8%), pic(####.##-)
            convert tc(8%) to str(cc$(3%),21%,8%), pic(####.##-)
            convert tc(9%) to str(cc$(3%),30%,8%), pic(####.##-)

            str(cc$(4%),1%,10%) = "Lock Mat'l"
            convert tc(10%) to str(cc$(4%),12%,8%), pic(####.##-)
            convert tc(11%) to str(cc$(4%),21%,8%), pic(####.##-)
            convert tc(12%) to str(cc$(4%),30%,8%), pic(####.##-)

            str(cc$(5%),1%,10%) = "Hardware  "
            convert tc(13%) to str(cc$(5%),12%,8%), pic(####.##-)
            convert tc(14%) to str(cc$(5%),21%,8%), pic(####.##-)
            convert tc(15%) to str(cc$(5%),30%,8%), pic(####.##-)

            str(cc$(6%),1%,10%) = "Packaging "
            convert tc(16%) to str(cc$(6%),12%,8%), pic(####.##-)
            convert tc(17%) to str(cc$(6%),21%,8%), pic(####.##-)
            convert tc(18%) to str(cc$(6%),30%,8%), pic(####.##-)
        REM
            hdr2$ = "Description                * Values *"
            str(cc$(16%),1%,26%) = "Total Freight Amount     :"
            convert tc(19%) to str(cc$(16%),28%,10%), pic(####.####-)

            str(cc$(17%),1%,26%) = "Total Vinyl Discount Amt :"
            convert tc(20%) to str(cc$(17%),28%,10%), pic(####.####-)

            str(cc$(18%),1%,26%) = "Total Cost Wood/Surround :"
            convert tc(21%) to str(cc$(18%),28%,10%),pic(####.####-)
        return

        build_mat
            sel_d$ = "Total Mat'l No Scrap/Scrp Only"
            hdr1$ = "No Scrap   *Vinyl * * Misc.* * Total*"
            str(cc$(1%),1%,10%) = "Material  "
            convert rm_mat(1%) to str(cc$(1%),12%,8%), pic(####.##-)
            convert rm_mat(2%) to str(cc$(1%),21%,8%), pic(####.##-)
            convert rm_mat(3%) to str(cc$(1%),30%,8%), pic(####.##-)

            str(cc$(2%),1%,10%) = "Glass     "
            convert rm_mat(4%) to str(cc$(2%),12%,8%), pic(####.##-)
            convert rm_mat(5%) to str(cc$(2%),21%,8%), pic(####.##-)
            convert rm_mat(6%) to str(cc$(2%),30%,8%), pic(####.##-)

            str(cc$(3%),1%,10%) = "Screen    "
            convert rm_mat(7%) to str(cc$(3%),12%,8%), pic(####.##-)
            convert rm_mat(8%) to str(cc$(3%),21%,8%), pic(####.##-)
            convert rm_mat(9%) to str(cc$(3%),30%,8%), pic(####.##-)

            str(cc$(4%),1%,10%) = "Lock Mat'l"
            convert rm_mat(10%) to str(cc$(4%),12%,8%), pic(####.##-)
            convert rm_mat(11%) to str(cc$(4%),21%,8%), pic(####.##-)
            convert rm_mat(12%) to str(cc$(4%),30%,8%), pic(####.##-)

            str(cc$(5%),1%,10%) = "Hardware  "
            convert rm_mat(13%) to str(cc$(5%),12%,8%), pic(####.##-)
            convert rm_mat(14%) to str(cc$(5%),21%,8%), pic(####.##-)
            convert rm_mat(15%) to str(cc$(5%),30%,8%), pic(####.##-)

            str(cc$(6%),1%,10%) = "Packaging "
            convert rm_mat(16%) to str(cc$(6%),12%,8%), pic(####.##-)
            convert rm_mat(17%) to str(cc$(6%),21%,8%), pic(####.##-)
            convert rm_mat(18%) to str(cc$(6%),30%,8%), pic(####.##-)
        REM
            hdr2$ = "Scrap Only *Vinyl * * Misc.* * Total*"
            str(cc$(16%),1%,10%) = "Material  "
            convert rm_mats(1%) to str(cc$(16%),12%,8%), pic(####.##-)
            convert rm_mats(2%) to str(cc$(16%),21%,8%), pic(####.##-)
            convert rm_mats(3%) to str(cc$(16%),30%,8%), pic(####.##-)

            str(cc$(17%),1%,10%) = "Glass     "
            convert rm_mats(4%) to str(cc$(17%),12%,8%), pic(####.##-)
            convert rm_mats(5%) to str(cc$(17%),21%,8%), pic(####.##-)
            convert rm_mats(6%) to str(cc$(17%),30%,8%), pic(####.##-)

            str(cc$(18%),1%,10%) = "Screen    "
            convert rm_mats(7%) to str(cc$(18%),12%,8%), pic(####.##-)
            convert rm_mats(8%) to str(cc$(18%),21%,8%), pic(####.##-)
            convert rm_mats(9%) to str(cc$(18%),30%,8%), pic(####.##-)
        return

        total_cost
            sel_d$ = "Total Material Cost Buckets   "
            hdr1$ = "Calculation Description    * Values *"
            str(cc$(1%),1%,26%) = "Total Vinyl Cost         :"
            convert tt(1%) to str(cc$(1%),28%,10%), pic(####.####-)

            str(cc$(2%),1%,26%) = "Total Vinyl Adj Cost     :"
            convert tt(2%) to str(cc$(2%),28%,10%), pic(####.####-)

            str(cc$(3%),1%,26%) = "Total Glass Cost         :"
            convert tt(3%) to str(cc$(3%),28%,10%), pic(####.####-)

            str(cc$(4%),1%,26%) = "Total Screen Cost        :"
            convert tt(4%) to str(cc$(4%),28%,10%), pic(####.####-)

            str(cc$(5%),1%,26%) = "Total Hardware Cost      :"
            convert tt(5%) to str(cc$(5%),28%,10%), pic(####.####-)

            str(cc$(6%),1%,26%) = "Total Packaging Cost     :"
            convert tt(6%) to str(cc$(6%),28%,10%), pic(####.####-)

            str(cc$(7%),1%,26%) = "Total Miscellaneous Cost :"
            convert tt(7%) to str(cc$(7%),28%,10%), pic(####.####-)

            str(cc$(8%),1%,26%) = "Total 'Material Cost'    :"
            convert tt(8%) to str(cc$(8%),28%,10%), pic(####.####-)

            str(cc$(9%),1%,26%) = "Total Material Adj. Cost :"
            convert tt(9%) to str(cc$(9%),28%,10%), pic(####.####-)

            str(cc$(10%),1%,26%) = "Total Product Cost       :"
            convert tt(10%) to str(cc$(10%),28%,10%), pic(####.####-)
        REM
            hdr2$ = "                                     "
        return

        build_err
            sel_d$ = "Display of Valid Error Codes  "
            hdr1$ = "E r r o r   D i s c r i p t i o n s  "
            for i% = 1% to 30%
                cc$(i%) = " N o n e " & "                     "
                if i% > 20% then goto L05440            /* MAX=20 ERRORS */
                if apc_err%(i%) = 0% then goto L05440   /* SCREEN = 30   */
                   cc$(i%) = err$(i%) & "         "
L05440:     next i%
        REM
            hdr2$ = "                                     "
        return

        sc_lab
            sel_d$ = "Explosion Lab/Avg Pay/UPMH    "
            hdr1$ = "Calculation Description    * Values *"
            str(cc$(1%),1%,26%) = "Direct Product Line Labor:"
            convert lab(1%) to str(cc$(1%),28%,10%), pic(####.####-)

            str(cc$(2%),1%,26%) = "Direct Glass Labor       :"
            convert lab(2%) to str(cc$(2%),28%,10%), pic(####.####-)

            str(cc$(3%),1%,26%) = "Direct Screen Labor      :"
            convert lab(3%) to str(cc$(3%),28%,10%), pic(####.####-)

            str(cc$(4%),1%,26%) = "Indirect Material Labor  :"
            convert lab(4%) to str(cc$(4%),28%,10%), pic(####.####-)

            str(cc$(5%),1%,26%) = "Indirect Staging Labor   :"
            convert lab(5%) to str(cc$(5%),28%,10%), pic(####.####-)

            str(cc$(6%),1%,26%) = "Indirect Loading Labor   :"
            convert lab(6%) to str(cc$(6%),28%,10%), pic(####.####-)

            str(cc$(7%),1%,26%) = "Standard Indirect Labor  :"
            convert lab(7%) to str(cc$(7%),28%,10%), pic(####.####-)

            str(cc$(8%),1%,26%) = "Total Direct and Indirect:"
            convert lab(8%) to str(cc$(8%),28%,10%), pic(####.####-)

            str(cc$(9%),1%,26%) = "Total Labor Overhead     :"
            convert lab(9%) to str(cc$(9%),28%,10%), pic(####.####-)

            str(cc$(10%),1%,26%) = "Total Product Labor      :"
            convert lab(10%) to str(cc$(10%),28%,10%), pic(####.####-)
        REM
            hdr2$ = "Department     Average Pay *  UPMH  *"
            str(cc$(16%),1%,14%) = "MFG Department"
            convert avg_pay(1%) to str(cc$(16%),16%,10%),pic(####.####-)
            convert uph(1%) to str(cc$(16%),28%,10%), pic(####.####-)

            str(cc$(19%),1%,14%) = "Glass Insulate"
            convert avg_pay(4%) to str(cc$(19%),16%,10%),pic(####.####-)
            convert uph(4%) to str(cc$(19%),28%,10%), pic(####.####-)

            str(cc$(20%),1%,14%) = "Glass Cutting "
            convert avg_pay(5%) to str(cc$(20%),16%,10%),pic(####.####-)
            convert uph(5%) to str(cc$(20%),28%,10%), pic(####.####-)

            str(cc$(21%),1%,14%) = "Screen Direct "
            convert avg_pay(6%) to str(cc$(21%),16%,10%),pic(####.####-)
            convert uph(6%) to str(cc$(21%),28%,10%), pic(####.####-)

            str(cc$(22%),1%,14%) = "Material Indir"
            convert avg_pay(7%) to str(cc$(22%),16%,10%),pic(####.####-)
            convert uph(7%) to str(cc$(22%),28%,10%), pic(####.####-)

            str(cc$(23%),1%,14%) = "Staging Indir "
            convert avg_pay(8%) to str(cc$(23%),16%,10%),pic(####.####-)
            convert uph(8%) to str(cc$(23%),28%,10%), pic(####.####-)

            str(cc$(24%),1%,14%) = "Loading Indir "
            convert avg_pay(9%) to str(cc$(24%),16%,10%),pic(####.####-)
            convert uph(9%) to str(cc$(24%),28%,10%), pic(####.####-)

            str(cc$(27%),1%,14%) = "Wood Surr/Fact"
            convert avg_pay(12%) to str(cc$(27%),16%,10%),pic(####.####-)
            convert uph(12%) to str(cc$(27%),28%,10%), pic(####.####-)

        return

        sc_mat
            sel_d$ = "Explosion Material Values     "
        sc_build
            hdr1$ = "Calculation Description    * Values *"
            str(cc$(1%),1%,26%) = "Vinyl Material Cost      :"
            convert lab(1%) to str(cc$(1%),28%,10%), pic(####.####-)

            str(cc$(2%),1%,26%) = "Misc. Material Cost      :"
            convert lab(2%) to str(cc$(2%),28%,10%), pic(####.####-)

            str(cc$(3%),1%,26%) = "Total Material Cost      :"
            convert lab(3%) to str(cc$(3%),28%,10%), pic(####.####-)

            str(cc$(4%),1%,26%) = "Vinyl Material Cost Scrap:"
            convert lab(4%) to str(cc$(4%),28%,10%), pic(####.####-)

            str(cc$(5%),1%,26%) = "Misc. Material Cost Scrap:"
            convert lab(5%) to str(cc$(5%),28%,10%), pic(####.####-)

            str(cc$(6%),1%,26%) = "Total Cost Scrap         :"
            convert lab(6%) to str(cc$(6%),28%,10%), pic(####.####-)

            str(cc$(7%),1%,26%) = "Total Cost Freight       :"
            convert lab(7%) to str(cc$(7%),28%,10%), pic(####.####-)

            str(cc$(8%),1%,26%) = "Total Vinyl Discount     :"
            convert lab(8%) to str(cc$(8%),28%,10%), pic(####.####-)

        REM
            hdr2$ = "                                    *"
        return

        sc_glass
            sel_d$ = "Explosion Glass Values        "
            gosub sc_build
        return

        sc_screen
            sel_d$ = "Explosion Screen Values       "
            gosub sc_build
        return

        sc_hardware
            sel_d$ = "Explosion Hardware Values     "
            gosub sc_build
        return

        sc_package
            sel_d$ = "Explosion Package Values      "
            gosub sc_build
        return

