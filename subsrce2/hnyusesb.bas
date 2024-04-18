        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y  U   U   SSS   EEEEE   SSS   BBBB    *~
            *  H   H  NN  N  Y   Y  U   U  S      E      S      B   B   *~
            *  HHHHH  N N N   YYY   U   U   SSS   EEEE    SSS   BBBB    *~
            *  H   H  N  NN    Y    U   U      S  E          S  B   B   *~
            *  H   H  N   N    Y     UUU    SSS   EEEEE   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYUSESB - Posts to the Usage Detail or Summary files     *~
            *            or Neither or Both sets, based on the data     *~
            *            passed as well as the settings in SYSFILE2.    *~
            *            Posts to existing records or creates new       *~
            *            records as required.                           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/19/91 ! Original (Major Sub-Module Rewrite).     ! JIM *~
            * 02/03/92 ! Modify to add VALUE variable calc.       ! RAN *~
            * 03/10/92 ! Attempt to get P/N MPS Calendar if       ! RAN *~
            *          !   MPSONLY$<>"Y"; fix misc bugs; set up   !     *~
            *          !   for 'linked' calendars.                !     *~
            * 07/10/92 ! Misc mods for release.                   ! MLJ *~
            * 08/17/92 ! Added PART # as Alt to MPS Usage Files.  ! MLJ *~
            * 08/25/92 ! Rewrote Summary File Update, added alter.! JBK *~
            *          !   keys to summary files.                 !     *~
            * 09/02/92 ! Removed ADDR from calls to DATEOFF.      ! JBK *~
            * 09/16/92 ! Usage Recorded for Planned Parts Only.   ! JBK *~
            * 09/23/92 ! Added Net Change Fields for Detail Usage.! JBK *~
            * 06/28/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "HNYUSESB"   (slsordr$,      /* Sales Order Number         */~
                          sequnce$,      /* Line Item Sequence Number  */~
                          storenr$,      /* Store Code                 */~
                          partnbr$,      /* Part Number                */~
                          req_act$,      /* 'R'equested/'A'ctual Usage */~
                          usedate$,      /* Usage Date                 */~
                          usetype$,      /* Usage Type Code            */~
                          quantity)      /* Usage Quantity             */

        dim                                                              ~
            calcode$8,                   /* Cal code for summaries     */~
            calkey$8,                    /* Cal code for summaries     */~
            calstart$6,                  /* Cal Start Date for Summary */~
            date$6,                      /* System Date For Updating   */~
            demtype$1,                   /* BCKLINES Demand Type       */~
            dettext$90,                  /* Preserve Detail Text       */~
            dfltcal$8,                   /* Dflt Cal Code for non-MPS  */~
            enddate$6,                   /* Ending Date                */~
            errormsg$79,                 /* For date validation        */~
            mpsgrp$8,                    /* Part's MPS Group           */~
            mpsonly$1,                   /* MPS Parts only? (Y/N)      */~
            nbrpds$2,                    /* PFMCAL record # periods    */~
            nextcal$8,                   /* Next Calendar              */~
            offset%(61),                 /* PFMCAL record date offsets */~
            partnbr$25,                  /* Part Number                */~
            p1start$6,                   /* PFMCAL period 1 Start Date */~
            prevcal$8,                   /* Previous Calendar          */~
            prev_next$4,                 /* Prev-Next Cal Search Flag  */~
            prev_next_date$6,            /* Prev-Next Cal Search Date  */~
            pstdate$8,                   /* Usage Date to post         */~
            pststor$3,                   /* Store Code value to post   */~
            quanta(60),                  /* Summary Quantity array     */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            req_act$1,                   /* 'R'equested/'A'ctual Usage */~
            revdate$8,                   /* Reverse Date               */~
            sequnce$3,                   /* Sales Order Line Seq Number*/~
            slsordr$16,                  /* Sales Order Number         */~
            storenr$3,                   /* Store Code                 */~
            usedate$6,                   /* Usage Date                 */~
            usedsnb$1,                   /* Detail/Summary/Neither/Both*/~
            userid$3,                    /* Current User Id            */~
            usestor$1,                   /* Post Store Codes?          */~
            usetype$5                    /* Usage Type Code            */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! HNYUADET ! Inventory Usage- Actual, Detail          *~
            * #02 ! HNYUASUM ! Inventory Usage- Actual, Summary         *~
            * #03 ! HNYURDET ! Inventory Usage- Requested, Detail       *~
            * #04 ! HNYURSUM ! Inventory Usage- Requested, Summary      *~
            * #05 ! MPSITEMS ! MPS Items Master File                    *~
            * #06 ! PFMUTYPE ! Usage Type Code Master File              *~
            * #07 ! BCKLINES ! Backlog Line-Item File                   *~
            * #08 ! SYSFILE2 ! Caelus Management System Information     *~
            * #09 ! PFMCAL   ! Forecast/Usage Calendar file             *~
            * #10 ! MPSGROUP ! MPS Groups Master file                   *~
            * #11 ! PIPMASTR ! Planned Inventory Position Master File   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYUADET",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 1,    keylen = 39,                      ~
                        alt key  1, keypos =   34, keylen =   6, dup,    ~
                            key  2, keypos =  147, keylen =   6, dup,    ~
                            key  3, keypos =    4, keylen =  25, dup

            select #02, "HNYUASUM",                                      ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos = 1,    keylen = 44,                      ~
                        alt key  1, keypos =   4, keylen =   41,         ~
                            key  2, keypos =  37, keylen =    8, dup

            select #03, "HNYURDET",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 1,    keylen = 39,                      ~
                        alt key  1, keypos =   34, keylen =   6, dup,    ~
                            key  2, keypos =  147, keylen =   6, dup,    ~
                            key  3, keypos =    4, keylen =  25, dup

            select #04, "HNYURSUM",                                      ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos = 1,    keylen = 44,                      ~
                        alt key  1, keypos =   4, keylen =   41,         ~
                            key  2, keypos =  37, keylen =    8, dup

            select #05, "MPSITEMS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =   8, dup,    ~
                            key  2, keypos =   34, keylen =   6, dup,    ~
                            key  3, keypos =   40, keylen =   2, dup,    ~
                            key  4, keypos =   42, keylen =   33

            select #06, "PFMUTYPE",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =  1,    keylen = 5

            select #07, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #08, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #09, "PFMCAL",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   8,                     ~
                        alternate key 1, keypos = 39, keylen = 6, dup

            select #10, "MPSGROUP",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos = 1,    keylen = 8,                       ~
                        alternate key 1, keypos = 39, keylen = 8, dup,   ~
                                  key 2, keypos = 47, keylen = 8, dup

            select #11, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Attempt to exit. Else perform program initializations.    *~
            *************************************************************

            if beenherebefore% <> 0% then goto L10000
                beenherebefore% = 1%
                mat f2% = con
                mpsonly$, usestor$, usedsnb$ = "N"
                call "OPENCHCK" (#8, fs%(8%), f2%(8%), 0%, rslt$(8%))

                call "READ100" (#8, "SWITCHS.HNY", f1%(8%))
                    if f1%(8) = 0% then goto exit_program
                get #8 using L09180, mpsonly$, usestor$, usedsnb$, dfltcal$
L09180:             FMT POS(108), 3*CH(1), CH(8)
                if pos("DSB" = usedsnb$) = 0% then exit_program

        REM  Detail/Summary files are opened only as needed...
                call "OPENCHCK" (#5, fs%(5%), f2%(5%), 0%, rslt$(5%))
                call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
                call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%, rslt$(7%))
                call "OPENCHCK" (#9, fs%(9%), f2%(9%), 0%, rslt$(9%))
                call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
                call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))

                if usedsnb$ = "S" then L09360
                     call "OPENCHCK" (#1, fs%(1%), f2%(1%), 200%,        ~
                                                                rslt$(5%))
                     call "OPENCHCK" (#3, fs%(3%), f2%(3%), 200%,        ~
                                                                rslt$(3%))

L09360:         if usedsnb$ = "D" then L09420
                     call "OPENCHCK" (#2, fs%(2%), f2%(2%), 100%,        ~
                                                                rslt$(2%))
                     call "OPENCHCK" (#4, fs%(4%), f2%(4%), 100%,        ~
                                                                rslt$(4%))

L09420:         date$ = date
                call "EXTRACT" addr ("ID", userid$)
                mat quanta = zer

L10000: REM *************************************************************~
            *        M A I N   S U B R O U T I N E   L O G I C          *~
            *************************************************************

            if pos("DSB" = usedsnb$) = 0% then exit_program
            if quantity  = 0              then exit_program
            if pos("RA"  = req_act$) = 0% then exit_program

        REM  Validate Store Number...
            if usestor$ <> "Y" and storenr$ = " " then L10130
            if str(storenr$,,1%) < "0" or str(storenr$,,1%) > "9"        ~
                then exit_program
L10130:     if usestor$ = "Y"                                            ~
                then pststor$ = storenr$          /* Set store to post */~
                else pststor$ = " "

        REM  Check for Planned Part...
            call "READ100" (#11, partnbr$, f1%(11%))
                if f1%(11%) = 0% then exit_program

        REM  If MPS only, check for the part in MPSITEMS file...
            calcode$ = " "
            call "READ100" (#5, partnbr$, f1%(5%))        /*  MPSITEMS */
               if f1%(5%) = 0% and mpsonly$="Y" then exit_program
                   if f1%(5%) = 0% then L10290
            get #5 using L10230, mpsgrp$
L10230:         FMT POS(26), CH(8)
            call "READ100" (#10, mpsgrp$, f1%(10%))        /* MPSGROUP */
               if f1%(10%) <> 0% then get #10 using L10260, calcode$
L10260:            FMT POS(47), CH(8)

        REM  Determine appropriate Calendar Code for summaries...
L10290:     if calcode$ = " " then calcode$ = dfltcal$

        REM  Validate passed-in Usage date, normalize to YYMMDD...
            pstdate$ = usedate$
            call "DATEOK" (pstdate$, r%, errormsg$)
                if r% = 0% then exit_program
            call "DATUNFMT" (pstdate$)

        REM  Validate passed-in Usage Type...
            if usetype$ = " " then usetype$ = "UNKN"
            call "READ100" (#6, usetype$, f1%(6%))        /* PFMUTYPE */
            if f1%(6%) = 0% then usetype$ = "UNKN"

        REM  If Usage Type 'UNKN', set it per the SO Demand Type...
            if usetype$ <> "UNKN" then L10530
            if slsordr$ = " " or sequnce$ = " " then L10530
                call "READ100" (#7, str(slsordr$) & sequnce$, f1%(7%))
                if f1%(7%) = 0% then L10530
                    get #7 using L10490, demtype$
L10490:                 FMT POS(240), CH(1)
                    if demtype$ = "1" then usetype$ = "FCST"
                    if demtype$ = "2" then usetype$ = "NFCT"

L10530: REM  Compute usage value based on standard costs...
            value = 0
            call "STCCOSTS" (partnbr$, " ", #8, 1%, value)

        REM  Post appropriate files, Detail, Summary or Both...
            if usedsnb$ <> "D" then L10620
                if req_act$ = "R"                                        ~
                     then gosub'201(3%)                                  ~
                     else gosub'201(1%)
                goto exit_program
L10620:     if usedsnb$ <> "S" then goto L10680
                if req_act$ = "R"                                        ~
                     then gosub'202(4%)                                  ~
                     else gosub'202(2%)
                goto exit_program
L10680:     if req_act$ <> "R" then L10720
                gosub'201(3%)
                gosub'202(4%)
                goto exit_program
L10720:     gosub'201(1%)
            gosub'202(2%)
            goto exit_program

        REM *************************************************************~
            *        S U P P O R T I N G   S U B R O U T I N E S        *~
            *************************************************************

        deffn'201(f%)                            /* HNYURDET, HNYUADET */
            oldqty, oldval, netchgqty, netchgval = 0  :  dettext$ = " "
            readkey$ = str(pststor$) & str(partnbr$) & str(usetype$) &   ~
                       str(pstdate$)
            call "READ101" (#f%, readkey$, f1%(f%))
            if f1%(f%) <> 0% then                                        ~
                get #f% using L15300, oldqty, dettext$, oldval, netchgqty,~
                                     netchgval
L15300:             FMT POS(40), PD(14,4), POS(57), CH(90), POS(153),    ~
                        3*PD(14,4)
            oldqty = oldqty + quantity
            oldval = oldval + (value * quantity)
            if usedsnb$ <> "D" then L15330
                netchgqty = netchgqty + quantity
                netchgval = netchgval + (value * quantity)
L15330:     call "DATREVRS" (pstdate$, revdate$, errormsg$)
            put #f% using L35040, pststor$, partnbr$, usetype$, pstdate$, ~
                oldqty, date$, userid$, dettext$, revdate$, oldval,      ~
                netchgqty, netchgval, " "
            if f1%(f%) = 0% then write #f% else rewrite #f%
            return

        deffn'202(f%)                            /* HNYURSUM, HNYUASUM */
            if calcode$ = " " then return        /* Nothing to update  */
            call "DATEOFF" ("GI", pstdate%, pstdate$, ret%)
                if ret% <> 0% then return
            calkey$ = " "     :  bucket_flag% = -1%
            prev_next$ = " "  :  prev_next_date$ = " "

L17070:     call "READ100" (#9, calcode$, f1%(9))
                if f1%(9%) = 0% then update_check

            get #9 using L17120, p1start$, enddate$, nbrpds$, nextcal$,   ~
                                prevcal$
L17120:         FMT POS(39), CH(6), POS(169), CH(6), CH(2), POS(180),    ~
                    2*CH(8)

            if prev_next$ = " " then test_start_date
                if prev_next$ = "NEXT" and prev_next_date$ <> p1start$   ~
                                         then update_check
                if prev_next$ = "PREV" and prev_next_date$ <> enddate$   ~
                                         then update_check

        test_start_date
            call "DATEOFF" ("GI", p1start%, p1start$, ret%)
                if ret% <> 0% then update_check

            if pstdate% >= p1start% then test_end_date
                calkey$ = calcode$
                bucket_flag% = 1%             /* We have underflow */
                if prevcal$ = " " then update_check
                     calcode$        = prevcal$
                     calstart$       = p1start$
                     prev_next$      = "PREV"
                     prev_next_date$ = p1start$
                     goto L17070

        test_end_date
            call "DATEOFF" ("GI", enddate%, enddate$, ret%)
                if ret% <> 0% then update_check

            if pstdate% < enddate% then good_calendar
                calkey$ = calcode$
                bucket_flag% = 3%             /* We have overflow  */
                if nextcal$ = " " then update_check
                     calcode$        = nextcal$
                     calstart$       = p1start$
                     prev_next$      = "NEXT"
                     prev_next_date$ = enddate$
                     goto L17070

        good_calendar
            convert nbrpds$ to nbrpds%, data goto update_check
            call "MXFL2GT" addr(#9, 44%, offset%(), nbrpds%)
            offset%(nbrpds% + 1%) = enddate%

            for p% = 1% to nbrpds% + 1%
                if offset%(p%) > pstdate% then goto L17590    /* Got it */
            next p%
            goto update_check       /* Shouldn't get here */

L17590:     calkey$         = calcode$
            calstart$       = p1start$
            bucket_flag%    = 2%             /* We have a bucket index */
            bucket%         = p% - 1%

        update_check
            if calkey$ = " " then return

        REM Init summary arrays for disk compression of qty and value...
            oldqty, oldval = 0
            readkey$ = str(pststor$) & str(partnbr$) & str(pststor$) &   ~
                       str(usetype$) & calkey$
            call "READ101" (#f%, readkey$, f1%(f%))
            if f1%(f%) <> 0% then L17770
                call "MXFLPPT" addr(#f%,   50%, quanta(), 60%, 4%)
                call "MXFLPPT" addr(#f%,  530%, quanta(), 60%, 4%)
                call "MXFLPPT" addr(#f%, 1010%, quanta(),  4%, 4%)

L17770:     on bucket_flag% goto underflow_update, bucket_update,        ~
                                 overflow_update
            return                         /* Should'nt get here */

        underflow_update
            oldqty_index% = 1026%  :  oldval_index% = 1034%
            gosub quantity_update
            goto record_update

        bucket_update
            oldqty_index% =  42%+(bucket%*8%)
            oldval_index% = 522%+(bucket%*8%)
            gosub quantity_update
            goto record_update

        overflow_update
            oldqty_index% = 1010%  :  oldval_index% = 1018%
            gosub quantity_update
            goto record_update

        record_update
            put #f% using L35170, pststor$, partnbr$, pststor$, usetype$, ~
                    calkey$, calstart$, date$, userid$, " "

            if f1%(f%) = 0% then write #f% else rewrite #f%
            return

        quantity_update
            call "MXFLPGT" addr(#f%, oldqty_index%, oldqty, 1%, 4%)
            call "MXFLPGT" addr(#f%, oldval_index%, oldval, 1%, 4%)
            oldqty = oldqty + quantity
            oldval = oldval + (value * quantity)
            call "MXFLPPT" addr(#f%, oldqty_index%, oldqty, 1%, 4%)
            call "MXFLPPT" addr(#f%, oldval_index%, oldval, 1%, 4%)
            return

        REM *************************************************************~
            *            F O R M A T    S T A T E M E N T S             *~
            *************************************************************

L35040:     FMT                 /* #1 HNYUADET and #3 HNYURDET         */~
                CH(3),          /* Store Code                          */~
                CH(25),         /* Part Number                         */~
                CH(5),          /* Usage Type Code                     */~
                CH(6),          /* Date of Usage YYMMDD                */~
                PD(14,4),       /* Cumulative Quantity Used            */~
                CH(6),          /* Date record last modified YYMMDD    */~
                CH(3),          /* Last user to modify record          */~
                CH(90),         /* Audit Text                          */~
                CH(6),          /* Reverse Search Date                 */~
                PD(14,4),       /* Cumulative Value Used               */~
                PD(14,4),       /* Net Change Quantity                 */~
                PD(14,4),       /* Net Change Value                    */~
                CH(124)         /* Filler (Unused Space)               */

L35170:     FMT                 /* #2 HNYUASUM and #4 HNYURSUM         */~
                CH(3),          /* Store Code                          */~
                CH(25),         /* Part Number                         */~
                CH(3),          /* Store Code again                    */~
                CH(5),          /* Usage Type Code                     */~
                CH(8),          /* Calendar Code                       */~
                CH(6),          /* Calendar Start Date YYMMDD          */~
                POS(1043),      /* Skip Usage Qty and Value arrays     */~
                CH(6),          /* Date record last modified YYMMDD    */~
                CH(3),          /* Last user to modify record          */~
                CH(49)          /* Filler (Unused Space)               */

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
