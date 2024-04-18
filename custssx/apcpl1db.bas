        REM *************************************************************~
            * APCPL1DB - Utility to Dump the Planning Capacity Arrays   *~
            *            to a Printer                                   *~
            *                   As of (11/14/97)  Check for R6.04.03    *~
            *************************************************************

        sub "APCPL1DB" (plan%, i%, k%, s1$, s2, s3%, s4$, s5$, s6%,      ~
                               s7%, s8%, s9%, s10, s11, s12, s13 )

        dim                                                              ~
            s1$8,                        /* DPT$(I%) DDD PP SS DAY     */~
            s2$1,                        /* CAP(I%)  DAILY CAPACITY    */~
            s3$1,                        /* UNT%(I%) DAILY PLN UNITS   */~
            s4$3,                        /* D_DP$(K%)        DEPARTMENT*/~
            s5$12,                       /* D_DP$(K%),4%,12%)   SORT   */~
            s6$1,                        /* D_SB%(K%,1%)  START SUB    */~
            s7$1,                        /* D_SB%(K%,2%)  1ST OPN DAY  */~
            s8$1,                        /* D_SB%(K%,3%)  END SUB      */~
            s9$1,                        /* D_SB%(K%,4%)  START DAY FAC*/~
            s10$1,                       /* D_WT(K%,1%) 1-WEIGHTED UPMH*/~
            s11$1,                       /* D_WT(K%,2%) 2-             */~
            s12$1, title$60,             /* D_WT(K%,3%) 3-             */~
            s13$1, date$8, runtime$8     /* D_WT(K%,4%) 4- SHIFTS      */

        REM *************************************************************~
            *          Debug Utility Print                              *~
            *************************************************************
            init(" ") s2$, s3$, s6$, s7$, s8$, s9$, s10$, s11$, s12$,s13$
        REM *************************************************************~
            *          Open Printer                                     *~
            *************************************************************

            if header% = 999% then goto prt_detail
               select printer (134)
               pageno% = 0%
               lcntr% = 99%
               gosub prt_header
               goto prt_detail

        prt_header
            header% = 999%
            title$ =                                                     ~
           "Plan Display of the Capacity Buckets Loaded in Memory-Before"
            init(" ") runtime$, date$
            pageno% = pageno% + 1%
            call "TIME" (runtime$)
            date$ = date
            call "DATEFMT" (date$)
            if lcntr% <> 99% then print using L00560
            print page
            print using L00560
            print using L00530   , date$, runtime$, title$, pageno%
L00530: %! ######## @ ########        ###################################~
        ~#########################                             Page: ### !
            print using L00560
L00560: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
            print using L00600
            print using L00560
L00600: %! I%! K%!DEPT!PC!SH! PD! CAPACITY ! PUNITS  ! DEPT ! PROD SORT  ~
        ~  !1SUB!SDAY!ESUB!SDAY!WGHT-1 !WGHT-2 !WGHT-3 !WGHT-4 !         !

L00630: %!###!## ! ###!##!##! # ! ####.##- ! ####.##-! ###  ! ###########~
        ~# !####!####!####!####!###.##-!###.##-!###.##-!###.##-!         !
            lcntr% = 5%
        return

        prt_detail
            if lcntr% > 57% then gosub prt_header
            print using L00630  ,i%, k%, str(s1$,1%,3%), str(s1$,5%,2%),    ~
                              str(s1$,7%,2%), str(s1$,4%,1%), s2, s3%,   ~
                              s4$, s5$, s6%, s7%, s8%, s9%, s10, s11,    ~
                              s12, s13
            lcntr% = lcntr% + 1%
            if plan% = i% then close printer

        end

