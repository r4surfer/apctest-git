        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLD45                             *~
            *  Creation Date     - 05/08/98                             *~
            *  Last Modified Date- 04/23/2015                           *~
            *  Description       - Display of Glass Remake Data for the *~
            *                      production Lines by Department       *~
            *                                                           *~
            *  Special Comments  - Temorary Fix for 'GLS' Userid        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/08/98 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !   Re-Write of Old (APCPLD45).            !     *~
            * 07/22/98 ! (EWD001) Mod for Production and Inhouse  ! RHH *~
            *          !   Re-Make Glass                          !     *~
            * 12/07/98 ! (EWD002) Mod to Clean-up for Selection   ! RHH *~
            *          !   changes in (APCPLA45)                  !     *~
            * 04/16/99 ! (EWD003) Mod to track glass by Userid    ! RHH *~
            *          !   Note Userid is Extracted. 'OV?' is     !     *~
            *          !   assigned to each oven.(For Completed   !     *~
            *          !   Glass). Oven Code in table (PLANGLASS) !     *~
            * 12/20/00 ! (EWD004) Mod to codes defining in-house  ! RHH *~
            *          !   glass remakes. Codes 26 thru 49        !     *~
            * 04/21/04 ! (EWD005) Mod for Received Glass Bucket   ! CMG *~
            * 12/20/04 ! (AWD006) Mods for remake buckets         ! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mod for new sub part      ! CMG *~
            *06/07/2007! (PAR001) reason 20 should be k1%=6, not 1! DES *~
            *12/15/2009! (AWD007) add florida to screen           ! CMG *~
            *12/15/2009! (AWD008) add Ultra to screen remakes     ! CMG *~
            *04/23/2015! (IM8022) mod for glass remakes           ! CMG *~
            *************************************************************

            sub "APCPLD45" ( gls%,       /* Called From 0% = APCSCANN  */~
                                         /*             1% = APCPLA45  */~
                             dept$,      /* Department Code - APCSCANN */~
                             shft$,      /* Shift Code (EWD003)        */~
                             glstype%,   /* (IM8022)                   */~
                             #1 ,        /* FILE = (APCPLNGR)          */~
                             #2 )        /* FILE = (GENCODES) AWD007   */

        dim                              /* Subroutine - Variables     */~
            dept$3,                      /* Applicable to (APCSCANN)   */~ 
            rm_ky$33, rm_rec$(4%)128,    /* Remake Key and Record      */~
            rm_cnt$16, rm_num$3,         /* Process Counter            */~
            rm_time$8, rm_time_d$8,      /* Time Scanned as Re-make    */~
            rm_dte$8,                    /* Date Re-make Scanned       */~
            rm_tot$4, rm_reason$2,       /* Total time to Complete     */~
            rm_shft$2, shft$2,           /* (EWD003) - Shift Codes     */~
            glstype$20,                  /* IM8022 Glass Type          */~
            trk$1, check_trk$1,          /* (EWD003) - Track code-Oven */~ 
            hd$80, h1$40, h2$40,         /* Header Text                */~
            ud$80, u1$40, u2$40,         /* Underline Text             */~  
            scr_dte$8, sav_dte$6,        /* Processing Date            */~
            rm_dept$3,                   /* Production Dept            */~ 
            pname$30,                    /* Screen Header              */~
            txt$(3%)40,                  /* Screen Header Text         */~
/*AWD008*/  hdr$78,                      /* Header for Screen          */~
/*AWD007*/  fl%(2%), fl$(2%)8,           /* Florida Screen Info        */~
/*AWD008*/  ul%(2%), ul$(2%)8,           /* Ultra Screen Info          */~
/*AWD007*/  readkey$100,                 /* Readkey                    */~
/*AWD007*/  model$3,                     /* Model Number               */~
/*AWD008*/  gls$2,                       /* Glass Code                 */~
/*EWD005*/  rm%(9%), rm$(9%)8,           /* Glass Analysis Bckts(EWD001)*/~
/*EWD005*/  tt$(7%,1010%)79, hh$(7%)35,  /* Detail Display (EWD001)    */~
            cursor%(2%),                 /* Cursor Location for Edit   */~
            i$(24%)80,                   /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            pf$(3%)79, rhh$1,            /* PF Key Description         */~
            pageno$16, cnt$16,           /* Page Counter Script        */~
            pfkeys$32, userid$3          /* PF Key Values              */

        dim rm_st$2,                     /* Status                     */~
            tot%(9%)                     /* Remake Total Count         */

            call "EXTRACT" addr("ID", userid$)

/* (AWD008) */
            str(hdr$,40%,8%) = "Remake"
            str(hdr$,55%,8%) = "Florida"
            str(hdr$,70%,8%) = "Ultra"
/* (/AWD008) */

                                             /* (EWD003)- Tracking Flag*/
            trk% = 0%                        /*  Request by 'OV' User  */
                                             /* Temp    Change for'GLS'*/
            if str(userid$,1%,3%) = "GLS" then                           ~
               userid$ = "OV" & str(shft$,2%,1%) /* Set to 1,2, or 3   */
                                             /* Temp                   */
            
            if str(userid$,1%,2%) <> "OV" then goto L00500
               check_trk$ = str(userid$,3%,1%)
               convert check_trk$ to trk%, data goto L00500
                                             /* (EWD003) -             */
L00500:     analysis% = 0%                   /* Only Analyize Once (1) */
            rm_max%   = 1008%
            shft%     = 0%                   /* No Shift Assigned      */
            convert shft$ to shft%, data goto L00510

L00510:
                                             /* (EWD001) Begin         */
            pageno$ = "Page: XXX of XXX"         /* k1% = Array Values */
            hh$(1%) = "Scanned Re-Make Glass-Prod. PF(6) :"   /* k1%=1%*/
            hh$(2%) = "Scanned-Re-Make Glass-House PF(7) :"   /* k1%=6%*/
            hh$(3%) = "Scheduled Re-Make Glass     PF(8) :"   /* k1%=2%*/
            hh$(4%) = "Completed Re-Make Glass Tod PF(9) :"   /* k1%=5%*/
            hh$(5%) = "Scheduled Glass            PF(10) :"   /* k1%=3%*/
            hh$(6%) = "Completed Glass (Today)    PF(11) :"   /* k1%=4%*/
                                                          /*  (EWD005) */
            hh$(7%) = "Received  Glass (Today)    PF(12) :"   /* k1%=7%*/

            if gls% <> 0% then goto main
                                             /* (EWD003) Glass Analysis*/ 
               if trk% <> 0% then goto main  /* Show all (5)           */
                  init(" ") hh$(5%), hh$(6%) /* Must be Production Dept*/
                                             /* Otherwise (1) thru (4) */
        main
            gosub analysis
            if keyhit% = 16% then goto L00560
                                                       /*  (EWD005)   */
            if keyhit% < 6% or keyhit% > 12% then goto main
               if keyhit% = 6%  then k1% = 1%
               if keyhit% = 7%  then k1% = 6%
               if keyhit% = 8%  then k1% = 2%
               if keyhit% = 9%  then k1% = 5%
               if keyhit% = 10% then k1% = 3%
               if keyhit% = 11% then k1% = 4%
                                                /* (EWD001) End        */
                                                     /*  (EWD005)      */
               if keyhit% = 12% then k1% = 7%

            gosub display_detail
            goto main

L00560: end

        REM *************************************************************~
            *       A n a l y s i s   S u m m a r y   S c r e e n       *~
            *-----------------------------------------------------------*~
            * Glass Analysis Screen                                     *~
            *************************************************************

        analysis                        /* ONLY DO ONCE                */
            if analysis% <> 0% then goto analysis_display
            init(" ") rm_ky$, scr_dte$, sav_dte$, rm_rec$(), rm$(), tt$(), ~
                      glstype$
            mat rm% = zer
            mat fl% = zer
            mat ul% = zer
            mat tot% = zer
            rm_cnt$ = "Checked [xxxxxx]"
            cnt% = 0%
            sav_dte$ = date
            scr_dte$ = date : call "DATEFMT" (scr_dte$)
            call "SHOSTAT" ("Analyzing Sched/Remk Glass")

        analysis_nxt
            read #1,key 3% > rm_ky$, using L00790  , rm_rec$(),             ~
                                                  eod goto analysis_done
L00790:        FMT 4*CH(128)                               /* (IM8022) */

            rm_ky$   = str(rm_rec$(),13%,21%)  /* Re-Make Status Key    */
            rm_st$   = str(rm_rec$(),13%,1%)   /* Re-Make Status Code   */
            rm_num$  = str(rm_rec$(),31%,3%)   /* Re-Make Number        */
            rm_dept$ = str(rm_rec$(),249%,3%)  /* Production Department */
            glstype$ = str(rm_rec$(),335%,20%) /* (IM8022)              */
            if glstype% = 1% and str(glstype$,1%,8%) <> "ANNEALED" then goto ~
                                                 analysis_nxt
            if glstype% = 2% and str(glstype$,1%,8%) <> "TEMPERED" then goto ~
                                                 analysis_nxt                                                 
/* (AWD007) */
            model$   = str(rm_rec$(),72%,3%)
            gls$     = str(rm_rec$(),77%,2%)
            florida% = 0%
            gosub check_planflor

/* (/AWD007) */

/* (AWD007) */
            ultra% = 0%
            gosub check_planultra
/* (/AWD007) */
            if rm_dept$ = "043" then goto analysis_nxt  /* (AWD006)     */
            rm_reason$= str(rm_rec$(),34%,2%)  /* (EWD001)              */

REM@@@        if rm_reason$ > "18" then goto L01800
            rm_shft$  = str(rm_rec$(),42%,2%)  /* (EWD003) Shift action */
                                               /*   occurred on.        */
            trk$      = str(rm_rec$(),65%,1%)  /* (EWD003) Oven Code    */
                                               /*   when applicable     */
            cnt% = cnt% + 1%
            if mod(cnt%,500%) <> 0 then goto L00880
               convert cnt% to str(rm_cnt$,10%,6%), pic(######)
               print at(03,33%);hex(84);rm_cnt$;

L00880:     if rm_st$ = "2" then goto analysis_done /* No Completed yet*/
            if gls% <> 0% then goto L00890    /* (APCPLA45)-Glass Prog */
                                              /* (EWD003)              */
               if trk% <> 0% then goto L00890 /* glass House  Ovens    */
                  if dept$ <> rm_dept$ then goto analysis_nxt
                                              /* Data for Dept Only    */
L00890:        if rm_st$ <> "0" then goto L00940
                  k1% = 1%                    /* Scanned in Re-Make    */
                  k2% = 1%                    /* (AWD007) */
                  gosub stuff_rm              /* Scanned Re-Make Glass */
                  goto analysis_nxt

L00940:        k1% = 3%                       /* Must be (1) Scheduled */
               gosub stuff_rm                 /* Scheduled Glass       */

               if str(rm_num$,2%,2%) = "00" then goto analysis_nxt
                  k1% = 2%                    /* Scheduled Re_Make GLS */
                  gosub stuff_rm
               goto analysis_nxt
        analysis_done
            cnt% = 0%
            call "SHOSTAT" ("Analyzing Completed Glass for Today")
            init(" ") rm_ky$, rm_rec$()
            str(rm_ky$,1%,6%) = sav_dte$
        analysis_done1
               read #1,key 1% > rm_ky$, using L00790  , rm_rec$(),          ~
                                                  eod goto analysis_done2
               cnt% = cnt% + 1%
               if mod(cnt%,100%) <> 0 then goto L01150
                  convert cnt% to str(rm_cnt$,10%,6%), pic(######)
                  print at(03,33%);hex(84);rm_cnt$;

L01150:     rm_ky$  = str(rm_rec$(),7%,27%)
            rm_st$  = str(rm_rec$(),13%,1%)
            rm_num$ = str(rm_rec$(),31%,3%)
            rm_dept$= str(rm_rec$(),249%,3%)
                                                /* (EWD003) - shift and*/
            rm_shft$= str(rm_rec$(),42%,2%)
            trk$    = str(rm_rec$(),65%,1%)
                                                /* (EWD003) Track Code */  
            if str(rm_ky$,1%,6%) <> sav_dte$ then goto analysis_done2
                                                    /*  (EWD005)       */
REM         if rm_st$ <> "2" then goto analysis_done1
            if rm_st$ <> "2" and rm_st$ <> "4" then goto analysis_done1

            if gls% <> 0% then goto L01160
                                                /* (EWD003) - 'OV'     */
               if trk% <> 0% then goto L01160   /* Glass House - Ovens */
                  if dept$ <> rm_dept$ then goto analysis_done1
                                                /* Data for Dept Only  */
L01160:        if trk% = 0% then goto L01170    /* Production Analysis */
                  if shft% = 0% then goto L01165
                     if rm_shft$ <> shft$ then goto analysis_done1

L01165:           if trk$ <> check_trk$ then goto analysis_done1
                   
                                                /* (EWD003) - End      */  
L01170:        k1% = 4%                         /* Completed Glass     */
               if rm_st$ = "4" then k1% = 7%    /* (EWD005)            */
               gosub stuff_rm
            if str(rm_num$,2%,2%) = "00" then goto analysis_done1
               k1% = 5%                         /* Completed Re-Make   */
               gosub stuff_rm
               if rm_st$ = "4" then k1% = 7%    /* (EWD005)            */
               goto analysis_done1
        analysis_done2
            for i% = 1% to 7%                   /*  (EWD005)           */
                convert rm%(i%) to rm$(i%), pic(########)

            next i%
            for i% = 1% to 2%                   /*  (AWD007)           */
                convert fl%(i%) to fl$(i%), pic(########)
                                                /*  (AWD008)           */
                convert ul%(i%) to ul$(i%), pic(########)
            next i%
        analysis_display
L01320:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(30),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), scr_dte$               , ch(08),~
                                                                         ~
               at (03,21), fac(hex(84)), txt$(1%)               , ch(40),~
               at (04,21), fac(hex(84)), txt$(2%)               , ch(40),~
               at (05,21), fac(hex(84)), txt$(3%)               , ch(40),~
                                                                         ~
               at (07,02), fac(hex(84)), hdr$                   , ch(78),~
               at (08,02), fac(hex(8c)), hh$(1%)                , ch(35),~
               at (08,40), fac(hex(84)), rm$(1%)                , ch(08),~
/*(AWD007)*/   at (08,55), fac(hex(84)), fl$(1%)                , ch(08),~
/*(AWD008)*/   at (08,70), fac(hex(84)), ul$(1%)                , ch(08),~
                                                                         ~
               at (10,02), fac(hex(8c)), hh$(2%)                , ch(35),~
               at (10,40), fac(hex(84)), rm$(6%)                , ch(08),~
/*(AWD007)*/   at (10,55), fac(hex(84)), fl$(2%)                , ch(08),~
/*(AWD008)*/   at (10,70), fac(hex(84)), ul$(2%)                , ch(08),~
                                                                         ~ 
               at (12,02), fac(hex(8c)), hh$(3%)                , ch(35),~
               at (12,40), fac(hex(84)), rm$(2%)                , ch(08),~
                                                                         ~
               at (14,02), fac(hex(8c)), hh$(4%)                , ch(35),~
               at (14,40), fac(hex(84)), rm$(5%)                , ch(08),~
                                                                         ~
               at (16,02), fac(hex(8c)), hh$(5%)                , ch(35),~
               at (16,40), fac(hex(84)), rm$(3%)                , ch(08),~
                                                                         ~
               at (18,02), fac(hex(8c)), hh$(6%)                , ch(35),~
               at (18,40), fac(hex(84)), rm$(4%)                , ch(08),~
                                                                         ~
/*(EWD005) */  at (19,02), fac(hex(8c)), hh$(7%)                , ch(35),~
/*(EWD005) */  at (19,40), fac(hex(84)), rm$(7%)                , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
         
                                                       /*  (EWD005)   */
               if keyhit% < 6% or keyhit% > 12% then goto L01670
                  if keyhit% = 6%  then k1% = 1%     /* (EWd001) Begin */
                  if keyhit% = 7%  then k1% = 6%
                  if keyhit% = 8%  then k1% = 2%
                  if keyhit% = 9%  then k1% = 5%
                  if keyhit% = 10% then k1% = 3%
                  if keyhit% = 11% then k1% = 4%
                  if keyhit% = 12% then k1% = 7%     /*  (EWD005)      */
                                                     /* (EWD001) End   */
L01670:        if keyhit% <> 15% then goto L01710
                  call "PRNTSCRN"
                  goto L01320

L01710:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1
            analysis% = 1%                       /* (EWD003) - Fix     */
            k% = 0%
            pname$   = "Glass - APCPLD45 (04/16/99)   "
            txt$(1%) = "****************************************"
            txt$(2%) = "*      G l a s s   A n a l y s i s     *"
            txt$(3%) = "****************************************"

            inpmessage$ = "Select PF(Key), or PF(16) to Exit?         "
            pf$(1%) = "(6)Scanned Re-Make(Prod)  (9)Completed R" &       ~
                      "e-Make Glass Today     (12)Received Gls"
            pf$(2%) = "(7)Scanned Re-Make(House) (10)Scheduled " &       ~
                      "Glass                  (15)Print Screen"
            pf$(3%) = "(8)Scheduled Re-Make      (11)Completed " &       ~
                      "Glass Today            (16)Exit Screen "
            pfkeys$ = hex(01ffffffff060708090a0b0cffff0f1000)
            if gls% <> 0% then return
               if trk% <> 0% then return              /* (EWD003)      */ 
               str(pf$(2%),27%,26%) = " " : str(pfkeys$,10%,1%) = hex(ff)
               str(pf$(3%),27%,26%) = " " : str(pfkeys$,11%,1%) = hex(ff)
               init(" ") rm$(3%), rm$(4%)
        return

        
        stuff_rm

                                            /* (EWD001) - Begin        */
           if rm_st$ <> "0" then goto L01800
                                            /* (EWD002) - Clean-up     */
                                            /* (EWD004) - 12/20/00     */
                                            /* (AWD006) - BEGIN        */
REM           if rm_reason$ < "11" then goto L01800
REM           if rm_reason$ > "18" then goto L01800
                                                    
              if rm_reason$ >= "11" and rm_reason$ <= "18" then k1% = 6%
              if rm_reason$ >= "90" and rm_reason$ <= "99" then k1% = 6%

	      /* <PAR001> */
              if rm_reason$ = "20" then k1% = 6%
	      /* </PAR001> */
              /* (AWD007) */
              if k1% = 6% then k2% = 2%
 

REM                k1% = 6%                    /* In-House Re-Make Glass  */
                                            /* (EWD004)                */

                                            /*  (AWD006) - END  */
L01800:    

          tot%(k1%) = tot%(k1%) + 1%

          if florida% = 0 and ultra% = 0% then ~
                 rm%(k1%) = rm%(k1%) + 1%   /* Update Specified Bucket */
                                            /* (EWD001) - End          */
           kk% = tot%(k1%)                  /* Current Bucket Value    */
           if kk% > rm_max% then return     /* Screen Full, No More    */

/* (AWD007) */

           if florida% <> 0% and ultra% = 0% and rm_st$ = "0"  ~
                                then fl%(k2%) = fl%(k2%) + 1%
/* (AWD008) */
REM Per John G Ultra takes priority over florida%
           if ultra% <> 0% and rm_st$ = "0"                    ~
                               then ul%(k2%) = ul%(k2%) + 1%

           str(tt$(k1%,kk%),1%, 9%) = str(rm_rec$(),22%,9%) /* Barcode */
           str(tt$(k1%,kk%),11%,3%) = str(rm_rec$(),72%,3%) /* Model Cd*/
           str(tt$(k1%,kk%),15%,3%) = str(rm_rec$(),249%,3%)/* Dept Cde*/
           str(tt$(k1%,kk%),19%,4%) = str(rm_rec$(),243%,4%)/* Seq. No */ 
           str(tt$(k1%,kk%),24%,8%) = str(rm_rec$(),163%,8%)/* S.O. No */

           rm_dte$ = str(rm_rec$(),52,6%)        /* Re_make Glass Date */
                                                 /* or Last Status Date*/
           if len(rm_dte$) < 3 then rm_dte$ = str(rm_rec$(),36%,6%)
           call "DATEFMT" (rm_dte$)
           str(tt$(k1%,kk%),33%,8%) = rm_dte$  /* Scan/Remake Date     */

           rm_time$ = "xx:xx:xx"
           rm_time_d$ = str(rm_rec$(),44%,8%)   /* (EWD002) Clean-up   */   
           if len(rm_time_d$) > 4 then goto L01810
              str(rm_time$,1%,2%) = str(rm_rec$(),14%,2%) /* Hours     */
              str(rm_time$,4%,2%) = str(rm_rec$(),17%,2%) /* Minutes   */
              str(rm_time$,7%,2%) = "00"                /* Seconds     */
              goto L01820                       /* Time of last Status */     
                                                /* Change              */ 
L01810:    str(rm_time$,1%,2%) = str(rm_rec$(),44%,2%) /* Hours        */
           str(rm_time$,4%,2%) = str(rm_rec$(),46%,2%) /* Minutes      */
           str(rm_time$,7%,2%) = str(rm_rec$(),48%,2%) /* Seconds      */

L01820:    str(tt$(k1%,kk%),42%,8%) = rm_time$  /* Re-Make Time        */
                                                /* (EWD002) End        */
           str(tt$(k1%,kk%),51%,3%) = str(rm_rec$(),58%,3%)/* User Id  */
           rm_tot$ = str(rm_rec$(),61%,4%)
           if len(rm_tot$) < 3% then rm_tot$ = "0000"
           str(tt$(k1%,kk%),55%,3%) = str(rm_tot$,1%,2%) & "H"
           str(tt$(k1%,kk%),59%,3%) = str(rm_tot$,3%,2%) & "M"
 

           str(tt$(k1%,kk%),63%,9%) = str(rm_rec$(),85%,9%) /* Calc Wid*/
           str(tt$(k1%,kk%),73%,8%) = str(rm_rec$(),94%,8%) /* Calc Hei*/
        return

        REM *************************************************************~
            *           D I S P L A Y   C O D E   T A B L E             *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_detail
L02210:     hh% = k1%
            if k1% = 1% then hh% = 1%
            if k1% = 6% then hh% = 2%
            if k1% = 2% then hh% = 3%
            if k1% = 5% then hh% = 4%
            if k1% = 3% then hh% = 5%
            if k1% = 4% then hh% = 6%
            if k1% = 7% then hh% = 7%                      /*  (EWD005)  */
            gosub set_pf2   
            accept                                                       ~
               at (01,02), fac(hex(84)), cnt$                   , ch(16),~
               at (01,62), fac(hex(84)), pageno$                , ch(16),~
                                                                         ~
               at (02,26), fac(hex(84)), str(hh$(hh%),1%,27%)   , ch(28),~
                                                                         ~
               at (04,02), fac(hex(84))  , hd$                  , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84))  , ud$                  , ch(79),~
                                                                         ~
               at (06,02), fac(hex(84))  , tt$(k1%,k% + 1%)     , ch(79),~
               at (07,02), fac(hex(84))  , tt$(k1%,k% + 2%)     , ch(79),~
                                                                         ~
               at (08,02), fac(hex(84))  , tt$(k1%,k% + 3%)     , ch(79),~
               at (09,02), fac(hex(84))  , tt$(k1%,k% + 4%)     , ch(79),~
                                                                         ~
               at (10,02), fac(hex(84))  , tt$(k1%,k% + 5%)     , ch(79),~
               at (11,02), fac(hex(84))  , tt$(k1%,k% + 6%)     , ch(79),~
                                                                         ~
               at (12,02), fac(hex(84))  , tt$(k1%,k% + 7%)     , ch(79),~
               at (13,02), fac(hex(84))  , tt$(k1%,k% + 8%)     , ch(79),~
                                                                         ~
               at (14,02), fac(hex(84))  , tt$(k1%,k% + 9%)     , ch(79),~
               at (15,02), fac(hex(84))  , tt$(k1%,k% +10%)     , ch(79),~
                                                                         ~
               at (16,02), fac(hex(84))  , tt$(k1%,k% +11%)     , ch(79),~
               at (17,02), fac(hex(84))  , tt$(k1%,k% +12%)     , ch(79),~
                                                                         ~
               at (18,02), fac(hex(84))  , tt$(k1%,k% +13%)     , ch(79),~
               at (19,02), fac(hex(84))  , tt$(k1%,k% +14%)     , ch(79),~
                                                                         ~
               at (20,02), fac(hex(84))  , tt$(k1%,k% +15%)     , ch(79),~
               at (21,02), fac(hex(84))  , tt$(k1%,k% +16%)     , ch(79),~
                                                                         ~
                                                                         ~
               at (24,02), fac(hex(a4)), pf$(1%)                , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L02810             /* First    */
L02780:           k% = 0%
                  goto L02210

L02810:        if keyhit% <> 3% then goto L02860             /* Last      */
L02820:           x% = int(val_max% / 16%)
                  k% = (x%*16%)
                  goto L02210

L02860:        if keyhit% <> 4% then goto L02920             /* Previous */
                  if k% < 17% then goto L02780
                  k% = k% - 16%
                  if k% <= 1% then goto L02780
                  goto L02210

L02920:        if keyhit% <> 5% then goto L02970             /* Next     */
                  k% = k% + 16%
                  if k% < val_max% then goto L02210
                  goto L02820

L02970:        if keyhit% <> 15 then goto L03010
                  call "PRNTSCRN"
                  goto L02210

L03010:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            h1$ = "Barcode   Mod Dpt Seq. S. Order RMK Date"
            u1$ = "--------- --- --- ---- -------- --------"
            h2$ = " RMK Time Usr Hrs Mns Cut Width Cut High"
            u2$ = " -------- --- --- --- --------- --------"
            
            if hh% > 4% then str(h1$,33%,8%)="Cut Date"
            if hh% > 4% then str(h2$,2%,8%) ="** N/A *"
      
            hd$ = h1$ & h2$
            ud$ = u1$ & u2$

            analysis% = 1%
            cnt$ = "Found [xxxxxxxx]"
REM            val_max% = rm%(k1%)
            val_max% = tot%(k1%)
            if val_max% > (rm_max% - 16%) then val_max% = rm_max% - 16%
                                                        /* Display Max */
            yy% = ( val_max% / 16% ) + 1%
            xx% = (k% / 16%) + 1%
            convert xx% to str(pageno$,7%,3%), pic(###) /* Current Page*/

            convert yy% to str(pageno$,14%,3%), pic(###)/* Total Pages */

REM            convert rm%(k1%) to str(cnt$,8%,8%), pic(########)

            convert tot%(k1%) to str(cnt$,8%,8%), pic(########)

            pf$(1) = "(2)First     (3)Last     (4)Previous    " &        ~
                     " (5)Next (15)Print Screen <Return> Cont"
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            gosub check_screen
            return

        check_screen
            if val_max% > 16% then goto L03320
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L03320:      if k% >= 16% then goto L03350
                gosub no_first
                gosub no_prev
L03350:      if (k% + 16%) <= val_max% then goto L03370
                gosub no_last
L03370:      if k% <= (val_max% - 16%) then goto L03390
                gosub no_next
L03390: return
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


/*(AWD007) */
        check_planflor
           return                /* Do not make flordia window anymore */
           florida% = 0%
           init(" ") readkey$
           str(readkey$,1,9)  = "PLAN FLOR"
           str(readkey$,10,3) = model$

           read #2, key = readkey$, eod goto no_flor
                florida% = 1%

        no_flor
        return

/*(\AWD007)*/


/*(AWD008) */
        check_planultra
           ultra% = 0%
           init(" ") readkey$
           str(readkey$,1,9)  = "PLANULTRA"
           str(readkey$,10,3) = model$
           str(readkey$,13,2) = "**"    /* All glass */

           read #2, key = readkey$, eod goto noAllUltra
                ultra% = 1%


noAllUltra: 

           init(" ") readkey$
           str(readkey$,1,9)  = "PLANULTRA"
           str(readkey$,10,3) = model$
           str(readkey$,13,2) = gls$    /* glass code */

           read #2, key = readkey$, eod goto noUltra
                ultra% = 1%

        noUltra
        return

/*(\AWD008)*/







