        REM *************************************************************~
            *                     ( AS OF 10/17/07 )                    *~
            *  (APCEMPPG) - Purge (APCEMPDT) and (APCEMPMT) of Data for *~
            *               and Employee for One (1) Day.               *~
            *                                                           *~
            *        NOTE - To Purge all Data for a Year and Prior to   *~
            *               and including a Production week. Enter Year *~
            *               and Production Week. Followed by hidden key *~
            *               PF(9).                                      *~
            *-----------------------------------------------------------*~
            * 03/25/98 ! ERN ! Y2K modifications                        *~
            * 03/10/99 ! (EWD001) Mods for security add MVK, LLJ, ! RHH *~
            *          !    LBH.                                  !     *~ 
            * 06/21/06 ! (EWD002) Mods for security add MKN       ! DES *~
            * 07/21/06 ! (EWD003) Mods for security add MKN       ! DES *~
            * 10/17/07 ! (AWD004) Mods for Security               ! CMG *~
            *03/30/2013! (AWD005) mod for file lengths            ! CMG *~
            *05/28/2014! (AWD006) Add user LNT.                   ! PWW *~
            *07/01/2014! (SR66589) Add use EAM to purge security  ! MES *~      
            *07/22/2016! (SR76249) Add use BAP to purge security  ! PWW *~     
            *01/29/2019! CR-1821 ADP Increase Emp ID field length ! DES *~     
            *03/01/2019! CR-1894 Increase EMP DEP to 3 bytes      ! DES *~ 
            *05/07/2020! CR2490  Increase size for Employee Number! RDB *~            
            *************************************************************

        dim readkey$18,                  /* GENERIC KEY                */~            
            e_no$5,                      /* Emp Number                 */~
            cursor%(2),                  /*                            */~
            i$(24)80,                    /*                            */~
            date$8, etime$8,             /* DATE AND TIME              */~
            inpmessage$79,               /* ENTRY PROMPT               */~
            errormsg$40,                 /*                            */~
            pf1$79,                      /*                            */~
            sav_key$10,                  /* SAVE PRIME KEY             */~
            sc_dept$3,                   /* EMPLOYEE DEPARTMENT        */~
            sc_yr$4,                     /* CURRENT AND PREVIOUS       */~
            sc_yr_bi$2,                  /* BI(2) value of sc_yr$      */~ 
            sc_wk$2,                     /* WEEK                       */~
            sc_day$1,                    /* DAY                        */~
            sc_emp$8,                    /* EMPLOYEE NUMBER            */~
            dt_rec$128,                  /* DETAIL RECORD              */~
            lfac$1,                      /* UPPER ONLY                 */~
            userid$3,                    /* USER ID                    */~
            purge_key$18,                /* PURGE KEY                  */~
            purge$50,                    /* Purge Data Text            */~
            mt_key$13                    /* MASTER KEY                 */

        dim f2%(2),                      /* = 0 if the file is open    */~
                                         /* = 1 if READ was successful */~
            fs%(2),                      /* = 1 if file open, -1 if it */~
            rslt$(2)20                   /* Text from file opening     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! APCEMPDT ! EMP DETAIL FILE                          *~
            * #02 ! APCEMPMT ! EMP MASTER FILE                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCEMPDT",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =  18,                     ~
                        alt key 1, keypos = 114, keylen =  6, dup /*ADP */

            select #2,  "APCEMPMT",                                      ~
                        varc,     indexed,  recsize =   128,  /* ADP*/   ~
                        keypos =    1, keylen =  13,                     ~
                        alt key 1, keypos =  4, keylen = 10, dup, ~
                            key 2, keypos = 82, keylen =  6, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1), f2%(1),0%, rslt$(1))
            call "OPENCHCK" (#2, fs%(2), f2%(2),0%, rslt$(2))

            call "EXTRACT" addr("ID", userid$)

            if userid$ = "RHF" or userid$ = "RFN" then goto begin_purge
            if userid$ = "LNT" or userid$ = "LGT" then goto begin_purge
            if userid$ = "EAM" or userid$ = "DES" then goto begin_purge
            if userid$ = "KEF" or userid$ = "KFI" then goto begin_purge 
            if userid$ = "CMG" or userid$ = "CGN" then goto begin_purge
            if userid$ = "RDB" or userid$ = "RBN" then goto begin_purge
                  goto exit_program
begin_purge:

/*(AWD004/) */
        REM *************************************************************~
            *                    C O N V E R T   D A T A                *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

L00720:     gosub display_data

            x% = 0%
            convert sc_yr$ to x%, data goto L00740
L00740:     if x% > 1980 then goto L00741
                sc_yr$ = " "
                goto L00720
L00741:     put sc_yr_bi$ using L00742, x%
L00742:         fmt bi(2)

            x% = 0%
            convert sc_wk$ to x%, data goto L00750
L00750:
            if x% <> 0% then goto L00780
               sc_wk$ = "  " : goto L00720
L00780:     convert x% to sc_wk$, pic(##)
            x% = 0%
/* CR2490  */
            e_no$ = "00000"
L12345:             FMT BI(4)            
REM            if str(sc_emp$,1%,1%) = "A" then goto L00890     
               convert sc_emp$ to x%, data goto L00720
               put str(e_no$,1%,4%) using L12345, x%
               str(e_no$,5%,1%) = " "
L00820:
               if x% <> 0% then goto L00950
                  sc_emp$ = "00000000" : goto L00720
L00850:           convert x% to sc_emp$, pic(#####000)
/* CR2490 */
                  goto L00950

L00890:        convert str(sc_emp$,2%,4%) to x%, data goto L00900
L00900:
               if x% <> 0% then goto L00930
                  sc_emp$ = "  " : goto L00720
L00930:           convert x% to str(sc_emp$,2%,4%), pic(0000)

L00950:     if sc_yr$ <> " " then goto L00970
               goto L00720
L00970:     if sc_wk$ <> " " then goto L00990
               goto L00720
L00990:     if sc_day$ <> " " then goto L01010
               goto L00720
L01010:     if sc_emp$ <> " " then goto L01040
               goto L00720

L01040:     readkey$ = all(hex(00))
            str(readkey$,1%,2%) = sc_yr_bi$
            str(readkey$,3%,2%) = sc_wk$
            str(readkey$,5%,1%) = sc_day$
            str(readkey$,6%,5%) = e_no$
            str(readkey$,10%,1%) = " "
            sav_key$ = str(readkey$,1%,10%)

        read_dt
            read #1,hold,key > readkey$, using   L01130, dt_rec$,          ~
                                                       eod goto dt_done
L01130:         FMT CH(128)              /* (AWD005) */
            readkey$ = str(dt_rec$,1%,18%)
               if sav_key$ <> str(readkey$,1%,10%) then goto dt_done
         stop " DETAIL DATA ----> " & readkey$
                  sc_dept$ = str(dt_rec$,11%,3%)
                  delete #1
                  gosub delete_master
                  goto read_dt
        dt_done
        goto exit_program

        delete_master
            mt_key$ = all(hex(00))
            str(mt_key$,1%,3%)  = sc_dept$
            str(mt_key$,4%,2%)  = sc_yr_bi$
/* CR2490 */
            str(mt_key$,6%,5%)  = e_no$
            str(mt_key$,11%,2%) = sc_wk$
            str(mt_key$,13%,1%) = sc_day$
            read #2,hold,key = mt_key$, eod goto mt_done

            delete #2
        mt_done
        return

        exit_program
            call "SHOSTAT" ("One Moment Please!!! ")
        end

        display_data
            inpmessage$ = "Enter All the Applicable Data? "
            date$ = date : u3% = 0%
            call "DATEFMT" (date$)
            etime$ = " "
            lfac$ = hex(81)
            call "TIME" (etime$)
            pf1$ = "PF(1)Start Over        PF(12)Delete Data" &          ~
                   "                     PF(16)Exit Program"
            init(" ") sc_wk$, sc_emp$, sc_day$, sc_yr$

L01520:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge Data For a Specified Employee ",                ~
               at (02,02), "Today:",                                     ~
               at (02,09), fac(hex(84)), date$                  , ch(08),~
               at (02,66), "Time :",                                     ~
               at (02,73), fac(hex(84)), etime$                 , ch(08),~
               at (03,25), fac(hex(94)), errormsg$              , ch(40),~
                                                                         ~
               at (04,02), "Employee Number         :",                  ~
               at (04,30), fac(lfac$), sc_emp$                  , ch(08),~
                                                                         ~
               at (05,02), "Production Year         :",                  ~
               at (05,30), fac(lfac$), sc_yr$                   , ch(04),~
                                                                         ~
               at (06,02), "Production Week (1-52)  :",                  ~
               at (06,30), fac(lfac$), sc_wk$                   , ch(02),~
                                                                         ~
               at (07,02), "Production Day(1 thru 7):",                  ~
               at (07,30), fac(lfac$), sc_day$                  , ch(01),~
                                                                         ~
               at (23,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (24,02), fac(hex(8c)),   pf1$                 , ch(79),~
                                                                         ~
               keys(hex(0001090c0f10)), key(keyhit%)

               if keyhit% = 1% then goto display_data
               if keyhit% = 9% then gosub purge_data
               if keyhit% = 16% then goto exit_program
               if keyhit% <> 12% then goto L01520

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        purge_data
             call "SHOSTAT" ( "Purging Detail Data" )
          purge$ = "Do you really want to Purge Data Prior To Week XX "
             str(purge$,48%,2%) = sc_wk$

             stop purge$
             close ws

             if userid$ = "RDB" or userid$ = "JFH" then goto L01999
                goto purge_done

L01999:      cnt1%, cnt2% = 0%
             purge_key$ = " "

            x% = 0%
            convert sc_yr$ to x%, data goto display_data
            if x% > 1980 then goto L02000
                sc_yr$ = " "
                goto display_data
L02000:     put sc_yr_bi$ using L02001, x%
L02001:         fmt bi(2)

             str(purge_key$,1%,2%) = sc_yr_bi$
       
        purge_dt
             read #1,hold,key > purge_key$, using  L02010, purge_key$,     ~
                                                         eod goto L02110
L02010:          FMT CH(18)
             if mod(cnt1%,25%) <> 0 then goto L02040
                print at(03,38);hex(84);"[";cnt1%;"]"
L02040:      if str(purge_key$,1%,2%) <> sc_yr_bi$ then goto L02110
                if str(purge_key$,3%,2%) > sc_wk$ then goto L02110

                delete #1
                cnt1% = cnt1% + 1%
                goto purge_dt

L02110:      purge_key$ = " "
             str(purge_key$,1%,2%) = sc_yr_bi$
             call "SHOSTAT" ( "Purging Master Data" )
        purge_mt
             read #2,hold,key 1% > purge_key$, using  L02170, purge_key$,  ~
                                                            eod goto L02260
L02170:          FMT POS(4), CH(10)
             if mod(cnt2%,25%) <> 0 then goto L02200
                print at(03,38);hex(84);"[";cnt2%;"]"
L02200:      if str(purge_key$,1%,2%) <> sc_yr_bi$ then goto purge_done
                if str(purge_key$,8%,2%) > sc_wk$ then goto purge_mt

                delete #2
                cnt2% = cnt2% + 1%
                goto purge_mt
L02260: purge_done
             convert cnt1% to cnt1$, pic(######)

             convert cnt2% to cnt2$, pic(######)

             stop " Detail Records Purged --------->  " & cnt1$
             stop " Master Records Purged --------->  " & cnt2$
        return clear all
        goto exit_program
