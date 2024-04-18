        REM *************************************************************~
            * APCPL2DB - Utility to Dump the Planning Capacity Arrays   *~
            *            to a Printer After the Plan Analysis if        *~
            *            Complete.   As of (11/14/97) Check for R6.04.03*~
            *************************************************************

        sub "APCPL2DB" (plan%, i%, k%, s1$, s2, s3%, s4$, s6%, s7%, s8$, ~
                                                                    s9% )

        dim                                                              ~
            s1$8,                        /* DPT$(I%) DDD PP SS DAY     */~
            s2$1,                        /* CAP(I%)  DAILY CAPACITY    */~
            s3$1,                        /* UNT%(I%) DAILY PLN UNITS   */~
            s4$3,                        /* D_DP$(K%)        DEPARTMENT*/~
            s6$1,                        /* D_SB%(K%,1%)  START SUB    */~
            s7$1,                        /* D_SB%(K%,3%)  End Sub Scrip*/~
            s8$3,                        /* Product Model Code         */~
            s9$1, title$60,              /* Planned Units              */~
            date$8, runtime$8            /* Date and Time              */

        REM *************************************************************~
            *          Debug Utility Print                              *~
            *************************************************************
            init(" ") s2$, s3$, s6$, s7$, s9$, date$, runtime$
        REM *************************************************************~
            *          Open Printer                                     *~
            *************************************************************

            if header% = 999% then goto prt_detail
               pageno% = 0%
               select printer (134)
               lcntr% = 99%
               gosub prt_header
               goto prt_detail

        prt_header
            header% = 999%
            title$ =                                                     ~
           "Plan Display of the Capacity Buckets Loaded in Memory-After "
            init(" ") runtime$, date$
            pageno% = pageno% + 1%
            call "TIME" (runtime$)
            date$ = date
            call "DATEFMT" (date$)
            if lcntr% <> 99% then print using L00520
            print page
            print using L00520
            print using L00490   ,date$, runtime$, title$, pageno%
L00490: %! ######## @ ########        ###################################~
        ~#########################                             Page: ### !
            print using L00520
L00520: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
            print using L00560
            print using L00520
L00560: %! I%! K%!DEPT!PC!SH! PD! Capacity !Plan Unts! Dept !St Sub!Ed Su~
        ~b!!Model!Plan Unts!                                             !

L00590: %!###!## ! ###!##!##! # ! ####.##- ! ####.##-! ###  ! #### ! ####~
        ~ !! ### ! ######  !                                             !
            lcntr% = 5%
        return

        prt_detail
            if lcntr% > 57% then gosub prt_header
            print using L00590  ,i%, k%, str(s1$,1%,3%), str(s1$,4%,2%),    ~
                              str(s1$,6%,2%), str(s1$,8%,1%), s2, s3%,   ~
                              s4$, s6%, s7%, s8$, s9%
            lcntr% = lcntr% + 1%
            if plan% = i% then close printer

        end

