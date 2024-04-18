*       ****************************************************************~
*                            ( R e p o r t s )                         *~
*                                                                      *~
*                           ( As of 11/12/97 - RHH )                   *~
*        APCEFF1B - Efficiency Detail Table Report                     *~
*                                                                      *~
*       ****************************************************************

        sub "APCEFF1B"      ( sc_wk$,         /* Production Week       */~
                              sc_day$,        /* Production Day        */~
                              rpt_dte$,       /* Production Date       */~
                              rpt$,           /* Report Type (1,2,3)   */~
                              rpt_sel$,       /* Report Sel (1,2,3,4)  */~
                              #1,             /* (APCEFFTB) - File     */~
                              #2 )            /* (GENCODES) - File     */

        dim rpt_dte$8,                   /* Report Date                */~
            eff_key$20,                  /* Primary Key                */~
            sav_key1$5,                  /* SAVE SELECTION             */~
            sc_wk$2,                     /* Production Week            */~
            sc_day$1,                    /* Production Day             */~
            rpt$1,                       /* Report Type                */~
            rpt_sel$1,                   /* Report Sel 1=DAILY,2=WEEKLY*/~
                                         /*          3=MONTHLY,4=YEARLY*/~
            readkey$24,                  /* Code Table Key             */~
            descr$32, dept$11,           /* Code Table Description     */~
            col1$(5%)14, ss$2,           /* Daily,Weekly,Monthly,Yearly*/~
            col2$(4%)7,  dd$2            /* Data Description           */

        dim tb(3%),                      /* Table Data Each Dept       */~
            td(75%),                     /* Calc Data Each Dept (4X14) */~
            p(75%,13%)                   /* Report Data                */

            col1$(1%) = "  D a i l y   "  : col1$(3%) = "M o n t h l y "
            col1$(2%) = " W e e k l y  "  : col1$(4%) = " Y e a r l y  "
            col1$(5%) = "  T a b l e   "

            col2$(1%) = " Daily "
            col2$(2%) = "Weekly "
            col2$(3%) = "Monthly"
            col2$(4%) = "Yearly "
            mat tb = zer
            mat td = zer
            mat p  = zer

            sel%   = 0%
            pindir = 0.0
            convert rpt_sel$ to sel%, data goto L00480
L00480:
            eff_proc$ = "0"
            if rpt$ = "4" then eff_proc$ = "1"
            if rpt$ = "5" then eff_proc$ = "2"

            call "SETPRNT" ("APCC", " ", 0%, 0%)
            select printer (134)

            if rpt$ = "1" or rpt$ = "2" then sel% = 5%

            call "SHOSTAT" ("Creating "&col1$(sel%)&" Report")
            if rpt$ = "1" or rpt$ = "2" then goto rpt_detail

            init(" ") eff_key$, readkey$, sav_key1$
            str(eff_key$,1%,1%) = eff_proc$
            if rpt$ = "3" then goto rpt_nxt
               str(eff_key$,2%,2%) = sc_wk$
               str(eff_key$,4%,1%) = sc_day$
               str(eff_key$,5%,1%) = eff_proc$
               sav_key1$ = str(eff_key$,1%,5%)

        rpt_nxt
            read #1,key > eff_key$, using   L00720  , eff_key$,             ~
                                                           eod goto rpt_a
L00720:        FMT CH(20)
            if rpt$ <> "3" then goto L00760
               if str(eff_key$,1%,1%) <> "0" then goto rpt_a
               goto L00780
L00760:     if str(eff_key$,1%,5%) <> sav_key1$ then goto rpt_a

L00780:     if str(eff_key$,11%,5%) <> "TOTAL" then goto rpt_nxt
               seq% = 0%
               convert str(eff_key$,6%,2%) to seq%, data goto L00810
L00810:
               gosub get_record
               gosub convert_data
            goto rpt_nxt
        rpt_a
            gosub complete_calcs

            gosub header
            readkey$ = " "
            str(readkey$,1%,9%) = "APC EFF01"
        rpt_anxt
            read #2,key > readkey$, using   L00940  , readkey$, descr$,     ~
                                                 eod goto rpt_done
L00940:        FMT CH(24), CH(30)
            if str(readkey$,1%,9%) <> "APC EFF01" then goto rpt_done
            ss$ = str(readkey$,10%,2%)              /* SEQ. NUMBER     */
            dd$ = str(readkey$,12%,2%)              /* DEPARTMENT CODE */
            p% = pos(descr$ = "-")
            dept$ = str(descr$,p%+2%,11%)
            if dd$ = "AA" then goto rpt_anxt
            if dd$ = "YY" then goto rpt_done
            if dd$ <> "HH" then goto L01050
               print using L03090 , str(descr$,8%,11%)
               goto rpt_anxt
L01050:     convert ss$ to seq%, data goto L01060
L01060:
            gosub detail
            if dd$ = "TT" then print using L02870
            goto rpt_anxt
        rpt_done
            print using L02750
            call "SETPRNT" ("APCC", " ", 0%, 1%)
            goto exit_end

        get_record
           get #1, using  L01170  , tb(), td()
L01170:       FMT POS(27), 3*PD(14,4), 56*PD(14,4)
        return

        convert_data
                                      /* SEL% - 1%=D, 2%=W, 3%=M, 4%=Y */
                                      /* SEQ% - SUBSCRIPT TO STORE DATA*/
           p(seq%,1%) = td(20% + sel%)               /* UPMH GOAL      */
           p(seq%,2%) = td(28% + sel%)               /* ACTUAL UPMH    */
           p(seq%,3%) = td(sel%)                     /* ACT PROD UNITS */
           p(seq%,4%) = td(16% + sel%)               /* PRODUCT DOLLARS*/
           p(seq%,5%) = td(4% + sel%)                /* REG HOURS      */
           p(seq%,6%) = td(8% + sel%)                /* OVR HOURS      */
           p(seq%,7%) = td(12% + sel%)               /* TOTAL HOURS    */
           p(seq%,8%) = td(24% + sel%)               /* LABOR DOLLARS  */
           p(seq%,9%) = td(32% + sel%)               /* % LABOR HOURS  */
           p(seq%,10%)= td(36% + sel%)               /* ACT LABOR %    */
           p(seq%,11%)= td(40% + sel%)               /* EFF PERCENT %  */
           p(seq%,12%)= td(52% + sel%)               /* SCRAP PERCENT  */
           p(seq%,13%)= td(44% + sel%)               /* SCRAP WEIGHT   */

           p(seq%,8%) = round(p(seq%,8%), 0)
           p(seq%,9%) = round(p(seq%,9%), 4)
           if seq% > 7% then goto L01470
                                                     /* TOTAL ALUMINUM */
              for i% = 1% to 13%
                  p(7%,i%)  = round(p(7%,i%) + p(seq%,i%), 4)
                  p(30%,i%) = round(p(30%,i%) + p(seq%,i%), 4)
              next i%
              goto L01690

L01470:    if seq% > 20% then goto L01550
                                                     /* TOTAL VINYL    */
              for i% = 1% to 13%
                  p(20%,i%) = round(p(20%,i%) + p(seq%,i%), 4)
                  p(30%,i%) = round(p(30%,i%) + p(seq%,i%), 4)
              next i%
              goto L01690

L01550:    if seq% > 29% then goto L01630
                                                     /* TOTAL SUPPORT  */
              for i% = 1% to 13%
                  p(29%,i%) = round(p(29%,i%) + p(seq%,i%), 4)
                  p(30%,i%) = round(p(30%,i%) + p(seq%,i%), 4)
              next i%
              goto L01690

L01630:    if seq% > 41% then goto L01690
                                                     /* TOTAL INDIRECT */

              if seq% > 30% and seq% < 34% then gosub L01695      /* (EWD0001) */

              for i% = 1% to 13%
                  p(41%,i%) = round(p(41%,i%) + p(seq%,i%), 4)
              next i%

L01690:       for i% = 1% to 13%                     /* TOTAL PLANT    */
                  p(49%,i%) = round(p(49%,i%) + p(seq%,i%), 4)
              next i%

        return

/* (EWD0001) */  
/* pindir is used to total to the first three (row 31 - 33) seq numbers in  */
/* Indirect Values for a new calc on Daily Eff Percent (column 11) */
              
L01695:  pindir = round(pindir + p(seq%,7%), 4)
          
         return
/* (EWD0001) */

        complete_calcs
            p(7%,1%), p(7%,2%), p(20%,1%), p(20%,2%) = 0.0
            p(29%,1%), p(29%,2%), p(30%,1%), p(30%,2%) = 0.0
            p(7%,10%), p(20%,10%), p(29%,10%), p(30%,10%) = 0.0
            p(7%,11%), p(20%,11%), p(29%,11%), p(30%,11%) = 0.0
            p(7%,12%), p(20%,12%), p(29%,12%), p(30%,12%) = 0.0
            p(49%,11%) = 0.0
            k% = 7%
            for i% = 1% to 29%
               if i% = 7%  then goto L02130
               if i% = 8%  then k% = 20%
               if i% = 20% then goto L02130
               if i% = 21% then k% = 29%
               if i% = 29% then goto L02130
               
                                      /* K%=7%  EFF_UPMHG - DEPARTMENT */
                                      /* K%=20% EFF_UPMHG - VINYL TOT  */
                                      /* K%=29% EFF_UPMHG - SUPPORT    */
               if p(k%,3%) = 0 or p(i%,1%) = 0 then goto L02000
                  rhh = ((p(i%,3%)/p(k%,3%)) * p(i%,1%))
                  p(k%,1%) = round( p(k%,1%) + rhh, 4)

                                      /* K%=7%  EFF_UPMHA - DEPARTMENT */
                                      /* K%=20% EFF_UPMHA - VINYL TOT  */
                                      /* K%=29% EFF_UPMHA - SUPPORT    */
L02000:        if p(k%,3%) = 0 or p(i%,2%) = 0 then goto L02050
        REM       RHH = ((P(I%,3%)/P(K%,3%)) * P(I%,2%))
                                    /* Mod - 01/23/96 - Sum Line Items */
                  p(k%,2%) = round( p(k%,2%) + p(i%,2%), 4)
                                      /*        EFF_EFF -              */
L02050:        if p(k%,7%) = 0 or p(i%,11%) = 0 then goto L02090
                  rhh = ((p(i%,7%)/p(k%,7%)) * p(i%,11%))
                  p(k%,11%) = round( p(k%,11%) + rhh, 4)
                                      /*        EFF_SCRP -             */
L02090:        if p(k%,4%) = 0 or p(i%,12%) = 0 then goto L02270
                  rhh = ((p(i%,4%)/p(k%,4%)) * p(i%,12%))
                  p(k%,12%) = round(p(k%,12%) + rhh, 4)
            goto L02270
L02130:                               /* K%=30% EFF_UPMHG - TOT DIR    */
                                      /* I% = 7%, 20%, 29%             */
               if p(30%,3%) = 0 or p(i%,1%) = 0 then goto L02190
                  rhh = ((p(i%,3%)/p(30%,3%)) * p(i%,1%))
                  p(30%,1%) = round(p(30%,1%) + rhh, 4)
                                      /* K%=30% EFF_UPMHA - TOT DIR    */
L02190:        if p(30%,3%) = 0 or p(i%,2%) = 0 then goto L02240
        REM       RHH = ((P(I%,3%)/P(30%,3%)) * P(I%,2%))
                                     /* Mod - 01/23/96 Sum Line Items  */
                  p(30%,2%) = round(p(30%,2%) + p(i%,2%), 4)
                                      /* K%=  % EFF_EFF   - TOT DIR    */
L02240:        if p(30,7) = 0 or p(i%,11%) = 0 then goto L02270
                  rhh = ((p(i%,7%)/p(30%,7%)) * p(i%,11%))
                  p(30%,11%) = round(p(30%,11%) + rhh, 4)
L02270:     next i%
            p(41%,11%) = 0.0             /* NEW CALC EFFICIENCY TOT */
        REM IF P(41%,2%) = 0 THEN GOTO 2300
        REM    P(41%,11%) = ROUND(P(41%,1%) / P(41%,2%), 4)
            for i% = 31% to 40%
               if p(41,7) = 0 then goto L02350
                  rhh = ((p(i%,7%) / pindir) * p(i%,11%))       /* (EWD0001) */
                  p(41%,11%) = round(p(41%,11%) + rhh, 4)
L02350:     next i%

            if p(49%,7%) = 0 then goto L02440
               rhh = (p(30%,7%) / p(49%,7%)) * p(30%,11%)
               p(49%,11%) = round(p(49%,11%) + rhh, 4)

               rhh = (p(41%,7%) / p(49%,7%)) * p(41%,11%)
               p(49%,11%) = round(p(49%,11%) + rhh, 4)

L02440:     x1, x2, x3, x5 = 0.0
            x5 = ( p(7%,4%) + p(20%,4%) )
            if x5 = 0 then goto L02490
            x1 = ((p(7%,4%) / ( p(7%,4%) + p(20%,4%))) * .666 ) * p(7%,12%)
            x2 = ((p(20%,4%) / ( p(7%,4%) + p(20%,4%))) * .666 ) * p(20%,12%)
L02490:     x3 = p(29%,12%) * .333
            p(30%,12%) = round(x1 + x2 + x3, 4)
            p(49%,12%) = p(30%,12%)
            p(49%,1%), p(49%,2%) = 0.0

            p(7%,10%), p(20%,10%), p(30%,10%), p(49%,10%) = 0.0
            if p(7%,4%) = 0 then goto L02570
               p(7%,10%)  = round( p(7%,8%)  / p(7%,4%), 4)
L02570:     if p(20%,4%) = 0 then goto L02590
               p(20%,10%) = round( p(20%,8%) / p(20%,4%), 4)
L02590:     if p(30%,4%) = 0 then goto L02610
               p(30%,10%) = round( p(30%,8%) / p(30%,4%), 4)
L02610:     if p(49%,4%) = 0 then goto L02640
               p(49%,10%) = round( p(49%,8%) / p(49%,4%), 4)
            p(49%,2%) = round(p(30%,2%) + p(41%,2%), 4%)
L02640:
            for i% = 1% to 60%
                p(i%,9%)  = round(p(i%,9%) * 100.00, 2)
                p(i%,10%) = round(p(i%,10%) * 100.00, 2)
                p(i%,11%) = round(p(i%,11%) * 100.00, 2)
                p(i%,12%) = round(p(i%,12%) * 100.00, 2)
            next i%
        return

        REM - Format Statements

L02750: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+

L02790: %!Production Week: ##                  P r o d u c t i o n   E f ~
        ~f i c i e n c y   R e p o r t                           Page: ###~
        ~  !

L02830: %!Production Day : ##                                      ######~
        ~########                                                   ######~
        ~##!

L02870: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--!
                                                          /* Header (1) */
L02910: %!           !       !       !        !         !          !     ~
        ~     !  Total   !         !   %   ! Actual!#######!#######!######~
        ~##!

                                                          /* Header (2) */
L02960: %!Department ! UPMH  ! Hours ! Actual ! Product.! Regular  ! Over~
        ~time ! Actual   !  Labor  ! Labor ! Labor !  Eff  ! Scrap ! Weigh~
        ~t !

                                                          /* Header (3) */
L03010: %!           !  Goal ! Earned!Product ! Dollars !  Hours   !  Hou~
        ~rs   !  Hours   ! Dollars ! Hours !   %   !   %   !   %   !      ~
        ~  !

L03050: %!###########!###.##-!######-!#######-!########-!######.##-!#####~
        ~#.##-!######.##-!########-!###.##-!###.##-!###.##-!###.##-!######~
        ~#-!

L03090: %!###########!       !       !        !         !          !     ~
        ~     !          !         !       !       !       !       !      ~
        ~  !

L03130: %!( ############### )   (AVG PRICE: ########.##-)   (STD UPMH: ##~
        ~######.##-)   (PROD SCRAP WT: ########.##-)                      ~
        ~  !

L03170: %!                                                               ~
        ~                                                                 ~
        ~ !

        REM - FIELDS (10 THRU 14)
L03220: %!DAILY   UNITS: ########.##-    REG HRS: ########.##-   OVR HRS:~
        ~ ########.##-    TOT HRS: ########.##-    PROD PRICE: ########.##~
        ~- !
L03250: %!WEEKLY  UNITS: ########.##-    REG HRS: ########.##-   OVR HRS:~
        ~ ########.##-    TOT HRS: ########.##-    PROD PRICE: ########.##~
        ~- !
L03280: %!MONTHLY UNITS: ########.##-    REG HRS: ########.##-   OVR HRS:~
        ~ ########.##-    TOT HRS: ########.##-    PROD PRICE: ########.##~
        ~- !
L03310: %!YEARLY  UNITS: ########.##-    REG HRS: ########.##-   OVR HRS:~
        ~ ########.##-    TOT HRS: ########.##-    PROD PRICE: ########.##~
        ~- !
        REM - FIELDS (15 THRU 19)
L03350: %!DAILY   UPMHG: ########.##-    WAGES  : ########.##-   UPMHA  :~
        ~ ########.##-    LAB DOL: ########.##-    ACT LAB % : ########.##~
        ~- !
L03380: %!WEEKLY  UPMHG: ########.##-    WAGES  : ########.##-   UPMHA  :~
        ~ ########.##-    LAB DOL: ########.##-    ACT LAB % : ########.##~
        ~- !
L03410: %!MONTHLY UPMHG: ########.##-    WAGES  : ########.##-   UPMHA  :~
        ~ ########.##-    LAB DOL: ########.##-    ACT LAB % : ########.##~
        ~- !
L03440: %!YEARLY  UPMHG: ########.##-    WAGES  : ########.##-   UPMHA  :~
        ~ ########.##-    LAB DOL: ########.##-    ACT LAB % : ########.##~
        ~- !

        REM - FIELDS (20 THRU 23)
L03490: %!DAILY   EFF %: ########.##-    ACT SRP: ########.##-   PRD SRP:~
        ~ ########.##-    SCRAP %: ########.##-                           ~
        ~  !
L03520: %!WEEKLY  EFF %: ########.##-    ACT SRP: ########.##-   PRD SRP:~
        ~ ########.##-    SCRAP %: ########.##-                           ~
        ~  !
L03550: %!MONTHLY EFF %: ########.##-    ACT SRP: ########.##-   PRD SRP:~
        ~ ########.##-    SCRAP %: ########.##-                           ~
        ~  !
L03580: %!YEARLY  EFF %: ########.##-    ACT SRP: ########.##-   PRD SRP:~
        ~ ########.##-    SCRAP %: ########.##-                           ~
        ~  !



        header
          pageno% = pageno% + 1%
          print page
          print using L02750
          print using L02790 , sc_wk$, pageno%
          print using L02830 , sc_day$, col1$(sel%), rpt_dte$
          print using L02870
          print using L02910 , col2$(sel%), col2$(sel%), col2$(sel%)
          print using L02960
          print using L03010
          print using L02870
        return

        header_table
          if lcntr% <> 99% then print using L02750
          pageno% = pageno% + 1%
          print page
          print using L02750
          print using L02790 , sc_wk$, pageno%
          print using L02830 , sc_day$, col1$(sel%), rpt_dte$
          lcntr% = 3%
        return

        detail
            x% = p(seq%,2%)                        /* EARNED HOURS     */
            print using L03050 , dept$,  p(seq%,1%),  x%,                  ~
                               p(seq%,3%),  p(seq%,4%),  p(seq%,5%),     ~
                               p(seq%,6%),  p(seq%,7%),  p(seq%,8%),     ~
                               p(seq%,9%),  p(seq%,10%),  p(seq%,11%),   ~
                               p(seq%,12%),  p(seq%,13%)
        return

        detail_table
           if lcntr% > 45% then gosub header_table
           print using L02870
           print using L03130 , str(eff_key$,6%,15%), tb(1%), tb(2%), tb(3%)
           print using L03170
           print using L03220 , td(1%), td(5%), td(9%),  td(13%), td(17%)
           print using L03250 , td(2%), td(6%), td(10%), td(14%), td(18%)
           print using L03280 , td(3%), td(7%), td(11%), td(15%), td(19%)
           print using L03310 , td(4%), td(8%), td(12%), td(16%), td(20%)

           print using L03350 , td(21%), td(25%), td(29%), td(33%), td(37%)
           print using L03380 , td(22%), td(26%), td(30%), td(34%), td(38%)
           print using L03410 , td(23%), td(27%), td(31%), td(35%), td(39%)
           print using L03440 , td(24%), td(28%), td(32%), td(36%), td(40%)

           print using L03490 , td(41%), td(45%), td(49%), td(53%)
           print using L03520 , td(42%), td(46%), td(50%), td(54%)
           print using L03550 , td(43%), td(47%), td(51%), td(55%)
           print using L03580 , td(44%), td(48%), td(52%), td(56%)
           lcntr% = lcntr% + 15%

        return

        rpt_detail
            lcntr% = 99%
            init(" ") eff_key$, readkey$
            str(eff_key$,1%,1%) = "0"
        rpt_detail_nxt
            read #1,key > eff_key$, using   L04250 , eff_key$,             ~
                                                eod goto rpt_detail_done
L04250:        FMT CH(20)
            if str(eff_key$,1%,1%) <> "0" then goto rpt_detail_done
            if rpt$ = "1" then goto L04300
              if str(eff_key$,11%,5%) <> "TOTAL" then goto rpt_detail_nxt

L04300:        gosub get_record
               gosub detail_table
            goto rpt_detail_nxt
        rpt_detail_done
            print using L02750
            call "SETPRNT" ("APCC", " ", 0%, 1%)

        exit_end
        end

