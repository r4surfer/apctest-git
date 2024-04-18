        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  JJJJJ  BBBB   TTTTT   AAA    SSS   K   K  PPPP   RRRR    *~
            *    J    B   B    T    A   A  S      K  K   P   P  R   R   *~
            *    J    BBBB     T    AAAAA   SSS   KKK    PPPP   RRRR    *~
            *  J J    B   B    T    A   A      S  K  K   P      R   R   *~
            *   J     BBBB     T    A   A   SSS   K   K  P      R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTASKPR - Allows the user to print reports (& optionally *~
            *            purge) from JBTCTASK, the Time Card Task       *~
            *            Summary file.                                  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/01/93 ! Original (PRR 11339)                     ! JIM *~
            * 08/13/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            asterisk1$16, asterisk2$16,  /* For printing totals        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            brkdate$10,                  /* Time Card Date break var   */~
            brkdept$4,                   /* Department break variable  */~
            brkempl$12,                  /* Employee break variable    */~
            brkshft$1,                   /* Shift # break variable     */~
            brktask$6,                   /* Time Card Task break var   */~
            col_header$25,               /* Screen Column Header       */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dayshft$1,                   /* Shift #                    */~
            departm$(4)5, departm$4,     /* Department Code range      */~
            dptdescr$32,                 /* Department Code description*/~
            edtmessage$79,               /* Edit screen message        */~
            employe$(4)12, employe$12,   /* Employee Code range        */~
            empdescr$32,                 /* Employee Code description  */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            infomsg$79, infac$1,         /* Informational message      */~
            inpmessage$79,               /* Informational Message      */~
            lbrclas$(4)5, lbrclas$4,     /* Labor Class Code range     */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            msg$79,                      /* Misc message               */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            sortseq$1,                   /* Sort Sequence              */~
            subheader$60,                /* Report Sub-Header          */~
            tc_date$(4)10, tc_date$10,   /* T/C Date range             */~
            tc_task$(4)6, tc_task$6,     /* T/C Task Code range        */~
            tskdescr$32,                 /* T/C Task Code description  */~
            tsktype$1,                   /* Task Type Code             */~
            thrs(16), tval(16),          /* Totals (15 levels + 'Run') */~
            time$8,                      /* Time of Day                */~
            userid$3,                    /* Current User Id            */~
            workkey$34                   /* Miscellaneous WORKFILE Key */

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

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! JBTCTASK ! Summary of hours by time card task code  *~
            * #03 ! GENCODES ! System General Codes file.               *~
            * #04 ! JBTCCDES ! Time Card Indirect Activity Codes        *~
            * #05 ! PRLDEPTF ! Payroll department codes                 *~
            * #06 ! PERMASTR ! Personnel master file                    *~
            * #20 ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "JBTCTASK",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  34

            select #03, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #04, "JBTCCDES",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =  1,   keylen =  6

            select #05, "PRLDEPTF",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   4

            select #06, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select #20, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   55,              ~
                        keypos =  1,   keylen = 39

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%), 0%, rslt$(06%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr ("ID", userid$)
            call "CMSMACHK" ("SFC", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then goto L09150
                u3% = 0%
                call "ASKUSER" (u3%, "*** SORRY, " & userid$ & " ***",   ~
                     "You must be a 'SFC' module or database administra"&~
                     "tor to run this program.", " ", "Press any PF key"&~
                     " to acknowledge and abort.")
                goto exit_program
L09150:     date$ = date : call "DATEFMT" (date$)
            edtmessage$ = "To modify displayed values, position cursor "&~
                "to desired value & press (RETURN)."
            str(line2$,62%) = "JBTASKPR: " & str(cms2v$,,8%)
            col_header$ = "From         To"
            call "COMPNAME" (12%, company$, u3%)
            maxlines% = 55%     /* Max number of print lines on a page */
            rptid$ = "JB0014"
            init ("*") asterisk1$
            run% = 6%         /* Highest level of total ('Run' totals) */
            infomsg$ = "Print ONLY was last. Use these values again, Ed"&~
                "it them, Purge or Start Over."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 6%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then goto L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then goto L10200
L10150:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then goto L10120
                          if fieldnr% = 1% then goto L10100
                          goto L10150
L10200:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then goto L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then goto L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 16% then goto print_and_or_purge
                if keyhit%  = 24% then goto print_and_or_purge
                if keyhit%  = 32% then goto print_and_or_purge
                if keyhit% <>  0% then goto editpg1
L11140:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% > 6% then goto editpg1
            if fieldnr% = lastfieldnr% then goto editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then goto editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then goto L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then goto L11190
                lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            *   R E S P O N D   T O   T H E   U S E R ' S   I N P U T   *~
            *************************************************************

        print_and_or_purge
            plowkey$ = xor plowkey$
            str(plowkey$,,6%) = tc_date$(3%)
            purged%, onetime%, f1%(1%) = 0%    /* Prime it for READ103 */
            if keyhit% = 16% then goto L12170            /* Print only? */
L12090:         u3% = 2%      /* Nope- confirm purge; window at bottom */
                call "ASKUSER" (u3%, "*** ARE YOU SURE? ***",            ~
                     "Are you sure you want to PURGE the T/C Task Summa"&~
                     "ry file?", "Press PF(12) to confirm the PURGE.",   ~
                     "Press PF(1) to abort purge.")
                if u3% =   1% then goto editpg1
                if u3% <> 12% then goto L12090

L12170
*        Give the user a progress report.
            if keyhit% <> 24% then goto L12210
                call "SHOSTAT" ("Purging JBTCTASK (no printed report)")
                goto L12290
L12210:     subheader$ = "TIME CARD TASK SUMMARY FILE"
            if keyhit% <> 32% then goto L12260
                call "SHOSTAT" ("Reading JBTCTASK for Print and Purge")
                call "FMTTITLE" (subheader$, "PRINT AND PURGE", 2%)
                goto L12290
L12260:     call "SHOSTAT" ("Reading JBTCTASK for Print only")
            call "FMTTITLE" (subheader$, "PRINT", 2%)

L12290
*        Here's the top of the loop that READs JBTCTASK.
            if f1%(1%) = 0%                                              ~
                then call "READ103" (#01, plowkey$, f1%(1%))/* JBTCTASK*/~
                else call "READNXT1" (#01, f1%(1%))
            if f1%(1%) = 0% then goto end_of_jbtctask
            get #01 using L35040, tc_date$, departm$, tc_task$, employe$, ~
                dayshft$, lbrclas$, tsktype$, hours, value /* JBTCTASK */

*        Try to eliminate records based on user's criteria.
            if tc_date$ > str(tc_date$(4%),,6%) then goto end_of_jbtctask
            if departm$ < str(departm$(3%),,4%) then goto L12290
            if departm$ > str(departm$(4%),,4%) then goto L12290
            if tc_task$ < tc_task$(3%)          then goto L12290
            if tc_task$ > tc_task$(4%)          then goto L12290
            if employe$ < employe$(3%)          then goto L12290
            if employe$ > employe$(4%)          then goto L12290
            if lbrclas$ < str(lbrclas$(3%),,4%) then goto L12290
            if lbrclas$ > str(lbrclas$(4%),,4%) then goto L12290

*        Can't eliminate it. Gotta report and/or purge it.
            if keyhit% <> 24% then goto L12540           /* Purge only? */
                delete #01             /* Yup- delete it from JBTCTASK */
                purged% = purged% + 1%                    /* Count 'em */
                goto L12290

L12540
*        Well, it's going to be reported at least. Maybe purged, too.
            if onetime% = 0% then gosub one_time_routine
            workkey$ = xor workkey$
            on sortseq% goto sort_work_1, sort_work_2, sort_work_3,      ~
                sort_work_4, sort_work_5

        sort_work_1 /* Also the default in the event of zaniness above */
            put workkey$ using L12630, tc_date$, dayshft$, departm$,      ~
                tc_task$, employe$
L12630:         FMT CH(6), CH(1), CH(4), CH(6), CH(12)
            goto write_to_workfile

        sort_work_2
            put workkey$ using L12690, departm$, tc_date$, dayshft$,      ~
                tc_task$, employe$
L12690:         FMT CH(4), CH(6), CH(1), CH(6), CH(12)
            goto write_to_workfile

        sort_work_3
            put workkey$ using L12750, employe$, departm$, tc_date$,      ~
                dayshft$, tc_task$
L12750:         FMT CH(12), CH(4), CH(6), CH(1), CH(6)
            goto write_to_workfile

        sort_work_4
            put workkey$ using L12810, employe$, tc_date$, dayshft$,      ~
                departm$, tc_task$
L12810:         FMT CH(12), CH(6), CH(1), CH(4), CH(6)
            goto write_to_workfile

        sort_work_5
            put workkey$ using L12870, tc_task$, tc_date$, dayshft$,      ~
                departm$, employe$
L12870:         FMT CH(6), CH(6), CH(1), CH(4), CH(12)

        write_to_workfile
            write #20 using L35140, workkey$, lbrclas$, tsktype$, hours,  ~
                value                                 /* #20- WORKFILE */
            if keyhit% = 32% then delete #01 /* Purge it from JBTCTASK */
            goto L12290

        end_of_jbtctask        /* End of input. Now see what we've got */
            if keyhit% <> 24% then goto L12990
                if purged% = 0% then goto L13000
                goto inputmode
L12990:     if onetime% <> 0% then goto L13070
L13000:         u3% = 2%                           /* Window at bottom */
                call "ASKUSER" (u3%, "*** NULL SET SELECTED ***",        ~
                     "There are no JBTCTASK records that satisfy your c"&~
                     "riteria.", " ", "Press any PF key to acknowledge "&~
                     "and continue.")
                goto editpg1

L13070
*        We've got something to print. Read WORKFILE & print it.
            call "SHOSTAT" ("Printing report. Please stand by.")
            f1%(20%) = 0%                        /* Primed for READ102 */
            plowkey$ = xor plowkey$
            init (hex(00)) brkdept$, brkdate$, brkempl$, brkshft$,       ~
                brktask$                 /* Initialize break variables */

L13140
*        Read the WORKFILE. Print the report. Accumulate & print totals.
            if f1%(20%) = 0%                                             ~
                then call "READ102" (#20, plowkey$, f1%(20%))/*WORKFILE*/~
                else call "READNEXT" (#20, f1%(20%))
            if f1%(20%) = 0% then goto end_of_report
            get #20 using L35140, workkey$, lbrclas$, tsktype$, hours,    ~
                value
            hours = round(hours, 2)
            value = round(value, 2)

*        Now determine the field sequence in the key, get the key fields,
*        and test for control breaks.
            on sortseq% goto get_key_data_1, get_key_data_2,             ~
                get_key_data_3, get_key_data_4, get_key_data_5

        get_key_data_1 /* Also the default in the event of error above */
            get workkey$ using L13320, tc_date$, dayshft$, departm$,      ~
                tc_task$, employe$
L13320:         FMT CH(6), CH(1), CH(4), CH(6), CH(12)
            call "DATEFMT" (tc_date$)
            if tc_date$ = brkdate$ then goto L13410
                gosub'201(1%)                /* Employee total level 1 */
                gosub'202(2%)                    /* Task total level 2 */
                gosub'203(3%)              /* Department total level 3 */
                gosub'204(4%)                   /* Shift total level 4 */
                gosub'205(5%)                    /* Date total level 5 */
                linecount% = 999%              /* Guarantee page break */
L13410:     if dayshft$ = brkshft$ then goto L13470
                gosub'201(1%)                /* Employee total level 1 */
                gosub'202(2%)                    /* Task total level 2 */
                gosub'203(3%)              /* Department total level 3 */
                gosub'204(4%)                   /* Shift total level 4 */
                print : linecount% = linecount% + 1%
L13470:     if departm$ = brkdept$ then goto L13520
                gosub'201(1%)                /* Employee total level 1 */
                gosub'202(2%)                    /* Task total level 2 */
                gosub'203(3%)              /* Department total level 3 */
                print : linecount% = linecount% + 1%
L13520:     if tc_task$ = brktask$ then goto L13560
                gosub'201(1%)                /* Employee total level 1 */
                gosub'202(2%)                    /* Task total level 2 */
                print : linecount% = linecount% + 1%
L13560:     if employe$ = brkempl$ then goto print_detail_and_accumulate
                gosub'201(1%)                /* Employee total level 1 */
                print : linecount% = linecount% + 1%
                goto print_detail_and_accumulate

        get_key_data_2
            get workkey$ using L13640, departm$, tc_date$, dayshft$,      ~
                tc_task$, employe$
L13640:         FMT CH(4), CH(6), CH(1), CH(6), CH(12)
            call "DATEFMT" (tc_date$)
            if departm$ = brkdept$ then goto L13730
                gosub'201(1%)                /* Employee total level 1 */
                gosub'202(2%)                    /* Task total level 2 */
                gosub'204(3%)                   /* Shift total level 3 */
                gosub'205(4%)                    /* Date total level 4 */
                gosub'203(5%)              /* Department total level 5 */
                linecount% = 999%              /* Guarantee page break */
L13730:     if tc_date$ = brkdate$ then goto L13790
                gosub'201(1%)                /* Employee total level 1 */
                gosub'202(2%)                    /* Task total level 2 */
                gosub'204(3%)                   /* Shift total level 3 */
                gosub'205(4%)                    /* Date total level 4 */
                print : linecount% = linecount% + 1%
L13790:     if dayshft$ = brkshft$ then goto L13840
                gosub'201(1%)                /* Employee total level 1 */
                gosub'202(2%)                    /* Task total level 2 */
                gosub'204(3%)                   /* Shift total level 3 */
                print : linecount% = linecount% + 1%
L13840:     if tc_task$ = brktask$ then goto L13880
                gosub'201(1%)                /* Employee total level 1 */
                gosub'202(2%)                    /* Task total level 2 */
                print : linecount% = linecount% + 1%
L13880:     if employe$ = brkempl$ then goto print_detail_and_accumulate
                gosub'201(1%)                /* Employee total level 1 */
                print : linecount% = linecount% + 1%
                goto print_detail_and_accumulate

        get_key_data_3
            get workkey$ using L13960, employe$, departm$, tc_date$,      ~
                dayshft$, tc_task$
L13960:         FMT CH(12), CH(4), CH(6), CH(1), CH(6)
            call "DATEFMT" (tc_date$)
            if employe$ = brkempl$ then goto L14050
                gosub'202(1%)                    /* Task total level 1 */
                gosub'204(2%)                   /* Shift total level 2 */
                gosub'205(3%)                    /* Date total level 3 */
                gosub'203(4%)              /* Department total level 4 */
                gosub'201(5%)                /* Employee total level 5 */
                linecount% = 999%              /* Guarantee page break */
L14050:     if departm$ = brkdept$ then goto L14110
                gosub'202(1%)                    /* Task total level 1 */
                gosub'204(2%)                   /* Shift total level 2 */
                gosub'205(3%)                    /* Date total level 3 */
                gosub'203(4%)              /* Department total level 4 */
                print : linecount% = linecount% + 1%
L14110:     if tc_date$ = brkdate$ then goto L14160
                gosub'202(1%)                    /* Task total level 1 */
                gosub'204(2%)                   /* Shift total level 2 */
                gosub'205(3%)                    /* Date total level 3 */
                print : linecount% = linecount% + 1%
L14160:     if dayshft$ = brkshft$ then goto L14200
                gosub'202(1%)                    /* Task total level 1 */
                gosub'204(2%)                   /* Shift total level 2 */
                print : linecount% = linecount% + 1%
L14200:     if tc_task$ = brktask$ then goto print_detail_and_accumulate
                gosub'202(1%)                    /* Task total level 1 */
                print : linecount% = linecount% + 1%
                goto print_detail_and_accumulate

        get_key_data_4
            get workkey$ using L14280, employe$, tc_date$, dayshft$,      ~
                departm$, tc_task$
L14280:         FMT CH(12), CH(6), CH(1), CH(4), CH(6)
            call "DATEFMT" (tc_date$)
            if employe$ = brkempl$ then goto L14370
                gosub'202(1%)                    /* Task total level 1 */
                gosub'203(2%)              /* Department total level 2 */
                gosub'204(3%)                   /* Shift total level 3 */
                gosub'205(4%)                    /* Date total level 4 */
                gosub'201(5%)                /* Employee total level 5 */
                linecount% = 999%              /* Guarantee page break */
L14370:     if tc_date$ = brkdate$ then goto L14430
                gosub'202(1%)                    /* Task total level 1 */
                gosub'203(2%)              /* Department total level 2 */
                gosub'204(3%)                   /* Shift total level 3 */
                gosub'205(4%)                    /* Date total level 4 */
                print : linecount% = linecount% + 1%
L14430:     if dayshft$ = brkshft$ then goto L14480
                gosub'202(1%)                    /* Task total level 1 */
                gosub'203(2%)              /* Department total level 2 */
                gosub'204(3%)                   /* Shift total level 3 */
                print : linecount% = linecount% + 1%
L14480:     if departm$ = brkdept$ then goto L14520
                gosub'202(1%)                    /* Task total level 1 */
                gosub'203(2%)              /* Department total level 2 */
                print : linecount% = linecount% + 1%
L14520:     if tc_task$ = brktask$ then goto print_detail_and_accumulate
                gosub'202(1%)                    /* Task total level 1 */
                print : linecount% = linecount% + 1%
                goto print_detail_and_accumulate

        get_key_data_5
            get workkey$ using L14600, tc_task$, tc_date$, dayshft$,      ~
                departm$, employe$
L14600:         FMT CH(6), CH(6), CH(1), CH(4), CH(12)
            call "DATEFMT" (tc_date$)
            if tc_task$ = brktask$ then goto L14690
                gosub'201(1%)                /* Employee total level 1 */
                gosub'203(2%)              /* Department total level 2 */
                gosub'204(3%)                   /* Shift total level 3 */
                gosub'205(4%)                    /* Date total level 4 */
                gosub'202(5%)                    /* Task total level 5 */
                linecount% = 999%              /* Guarantee page break */
L14690:     if tc_date$ = brkdate$ then goto L14750
                gosub'201(1%)                /* Employee total level 1 */
                gosub'203(2%)              /* Department total level 2 */
                gosub'204(3%)                   /* Shift total level 3 */
                gosub'205(4%)                    /* Date total level 4 */
                print : linecount% = linecount% + 1%
L14750:     if dayshft$ = brkshft$ then goto L14800
                gosub'201(1%)                /* Employee total level 1 */
                gosub'203(2%)              /* Department total level 2 */
                gosub'204(3%)                   /* Shift total level 3 */
                print : linecount% = linecount% + 1%
L14800:     if departm$ = brkdept$ then goto L14840
                gosub'201(1%)                /* Employee total level 1 */
                gosub'203(2%)              /* Department total level 2 */
                print : linecount% = linecount% + 1%
L14840:     if employe$ = brkempl$ then goto print_detail_and_accumulate
                gosub'201(1%)                /* Employee total level 1 */
                print : linecount% = linecount% + 1%

        REM *************************************************************~
            * Print the detail line and accumulate into the total arrays ~
            *************************************************************

        print_detail_and_accumulate
            on sortseq% goto print_detail_1, print_detail_2,             ~
                print_detail_3, print_detail_4, print_detail_5

        print_detail_1 /* Also default in the event of looniness above */
            if linecount% > maxlines% then gosub page_heading
            print using L60430, tc_date$, dayshft$, departm$, tc_task$,   ~
                employe$, lbrclas$, tsktype$, hours, value
            goto accumulate_level_1

        print_detail_2
            if linecount% > maxlines% then gosub page_heading
            print using L60460, departm$, dptdescr$, tc_date$, dayshft$,  ~
                tc_task$, employe$, lbrclas$, tsktype$, hours, value
            goto accumulate_level_1

        print_detail_3
            if linecount% > maxlines% then gosub page_heading
            print using L60490, employe$, empdescr$, departm$, tc_date$,  ~
                dayshft$, tc_task$, lbrclas$, tsktype$, hours, value
            goto accumulate_level_1

        print_detail_4
            if linecount% > maxlines% then gosub page_heading
            print using L60520, employe$, empdescr$, tc_date$, dayshft$,  ~
                departm$, tc_task$, lbrclas$, tsktype$, hours, value
            goto accumulate_level_1

        print_detail_5
            if linecount% > maxlines% then gosub page_heading
            print using L60550, tc_task$, tskdescr$, tc_date$, dayshft$,  ~
                departm$, employe$, lbrclas$, tsktype$, hours, value

        accumulate_level_1 /* Accum detail values into 1st total level */
            thrs(1%) = thrs(1%) + hours
            tval(1%) = tval(1%) + value
            goto L13140

        end_of_report  /* Print final totals, Run totals, 'EOR' & bail */
            init (hex(ff)) departm$, tc_date$, employe$, lbrclas$,       ~
                tc_task$, dayshft$/* Initialize break variables for EOR*/
            on sortseq% goto final_total_1, final_total_2, final_total_3,~
                final_total_4, final_total_5

        final_total_1 /* Also the default in the event of lunacy above */
            gosub'201(1%) : gosub'202(2%) : gosub'203(3%) : gosub'204(4%)
            gosub'205(5%) : goto print_run_total

        final_total_2
            gosub'201(1%) : gosub'202(2%) : gosub'204(3%) : gosub'205(4%)
            gosub'203(5%) : goto print_run_total

        final_total_3
            gosub'202(1%) : gosub'204(2%) : gosub'205(3%) : gosub'203(4%)
            gosub'201(5%) : goto print_run_total

        final_total_4
            gosub'202(1%) : gosub'203(2%) : gosub'204(3%) : gosub'205(4%)
            gosub'201(5%) : goto print_run_total

        final_total_5
            gosub'201(1%) : gosub'203(2%) : gosub'204(3%) : gosub'205(4%)
            gosub'202(5%)

        print_run_total
            if linecount% > maxlines% then gosub page_heading
            gosub'220(run%)
            print using L60580, asterisk2$, "Run Total:", thrs(run%),     ~
                tval(run%)
            print
            gosub get_time_of_day
            print using L60640, time$                  /* End of Report */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            if keyhit% <> 16% then goto inputmode
                infac$ = hex(84)              /* Show the INFOMSG$ ... */
                goto editpg1   /* ... & permit re-use of same criteria */

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        one_time_routine
            onetime% = 1%                                     /* Guess */
            call "FILEBGON" (#20) : f2%(20%) = 1% /* Bye-bye, WORKFILE */
            call "WORKOPEN" (#20, "IO   ", 500%, f2%(20%)) /* Hello WF */
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            linecount% = 999%          /* Guarantee initial page break */
            page_nbr% = 0%
            gosub get_time_of_day
L18130:     i% = pos(str(i$()) > hex(7f))             /* Nuke the FACs */
            if i% = 0% then L18170
                str(i$(), i%, 1%) = hex(20)
                goto L18130
L18170:     gosub page_0_heading
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26)
            print "------------------------------------------------------~
        ~--------------------------"
            mat thrs = zer
            mat tval = zer
            return

        get_time_of_day
            time$ = " "
            call "TIME" (time$)
            return

        page_heading
            page_nbr% = page_nbr% + 1%
            linecount% = 7%
        page_0_heading
            print page
            print using L60040, date$, time$, company$, "-" & rptid$
            print using L60070, subheader$, page_nbr%
            if page_nbr% = 0% then return
            print

*        Print column headers appropriate for each sort sequence.
            on sortseq% goto column_1, column_2, column_3, column_4,     ~
                column_5

        column_1    /* Also the default in case the above went bonkers */
            print using L60100 : print using L60130 : goto underscores

        column_2
            print using L60160 : print using L60190 : goto underscores

        column_3
            print using L60220 : print using L60250 : goto underscores

        column_4
            print using L60280 : print using L60310 : goto underscores

        column_5
            print using L60340 : print using L60370

        underscores
            print using L60400                    /* Common underscores */
            print
            return

        deffn'201(a%)
*        Print Employee total; roll totals up 1 level; zero totals this
*        (A%) level; set break variable; describe new Employee Code.
            if brkempl$ = hex(000000000000000000000000) then goto set_empl
            if linecount% > maxlines% then gosub page_heading
            if a% = 1% then gosub underscore_totals
            gosub'220(a%)
            print using L60580, asterisk2$, "Employee " & brkempl$ & " " &~
                empdescr$ & " Total:", thrs(a%), tval(a%)
            linecount% = linecount% + 1%
            thrs(a% + 1%) = thrs(a% + 1%) + thrs(a%) /* Roll up totals */
            tval(a% + 1%) = tval(a% + 1%) + tval(a%) /* Roll up totals */
            thrs(a%), tval(a%) = 0           /* Zero this level totals */
            if employe$ = hex(ffffffffffffffffffffffff) then return
        set_empl
            brkempl$ = employe$
            call "READ100" (#06, employe$, f1%(6%))        /* PERMASTR */
            if f1%(6%) <> 0% then goto L18900
                empdescr$ = "Employee Code not on file"
                goto L18930
L18900:     get #06 using L18910, temp1$, temp2$, temp3$
L18910:         FMT POS(2), CH(15), CH(10), CH(1)
            empdescr$ = temp1$ & ", " & temp2$ & " " & temp3$
L18930:     call "PUTPAREN" (empdescr$)
            return

        deffn'202(b%)
*        Print Task total; roll totals up 1 level; zero totals this (B%)
*        level; set break variable; describe new Task Code.
            if brktask$ = hex(000000000000) then goto set_task
            if linecount% > maxlines% then gosub page_heading
            if b% = 1% then gosub underscore_totals
            gosub'220(b%)
            print using L60580, asterisk2$, "Task " & brktask$ & " " &    ~
                tskdescr$ & " Total:", thrs(b%), tval(b%)
            linecount% = linecount% + 1%
            thrs(b% + 1%) = thrs(b% + 1%) + thrs(b%) /* Roll up totals */
            tval(b% + 1%) = tval(b% + 1%) + tval(b%) /* Roll up totals */
            thrs(b%), tval(b%) = 0           /* Zero this level totals */
            if tc_task$ = hex(ffffffffffff) then return
        set_task
            brktask$ = tc_task$
            call "DESCRIBE" (#04, tc_task$, tskdescr$, 0%, /* JBTCCDES */~
                f1%(4%))
            if f1%(4%) = 0% then tskdescr$ = "Task Code not on file"
            call "PUTPAREN" (tskdescr$)
            return

        deffn'203(c%)
*        Print Department total; roll totals up 1 level; zero totals this
*        (C%) level; set break variable; describe new Department Code.
            if brkdept$ = hex(00000000) then goto set_dept
            if linecount% > maxlines% then gosub page_heading
            if c% = 1% then gosub underscore_totals
            gosub'220(c%)
            print using L60580, asterisk2$, "Department " & brkdept$ & " "~
                & dptdescr$ & " Total:", thrs(c%), tval(c%)
            linecount% = linecount% + 1%
            thrs(c% + 1%) = thrs(c% + 1%) + thrs(c%) /* Roll up totals */
            tval(c% + 1%) = tval(c% + 1%) + tval(c%) /* Roll up totals */
            thrs(c%), tval(c%) = 0           /* Zero this level totals */
            if departm$ = hex(ffffffff) then return
        set_dept
            brkdept$ = departm$
            call "DESCRIBE" (#05, departm$, dptdescr$, 0%, /* PRLDEPTF */~
                f1%(5%))
            if f1%(5%) = 0% then dptdescr$ = "Department Code not on file"
            call "PUTPAREN" (dptdescr$)
            return

        deffn'204(d%)
*        Print Shift total; roll totals up 1 level; zero totals this (D%)
*        level; set break variable.
            if brkshft$ = hex(00) then goto set_shift
            if linecount% > maxlines% then gosub page_heading
            if d% = 1% then gosub underscore_totals
            gosub'220(d%)
            print using L60580, asterisk2$, "Shift " & brkshft$ &         ~
                " Total:", thrs(d%), tval(d%)
            linecount% = linecount% + 1%
            thrs(d% + 1%) = thrs(d% + 1%) + thrs(d%) /* Roll up totals */
            tval(d% + 1%) = tval(d% + 1%) + tval(d%) /* Roll up totals */
            thrs(d%), tval(d%) = 0           /* Zero this level totals */
            if dayshft$ = hex(ff) then return
        set_shift
            brkshft$ = dayshft$
            return

        deffn'205(e%)
*        Print Date total; roll totals up 1 level; zero totals this (E%)
*        level; set break variable.
            if brkdate$ = hex(00000000000000000000) then goto set_date
            if linecount% > maxlines% then gosub page_heading
            if e% = 1% then gosub underscore_totals
            gosub'220(e%)
            print using L60580, asterisk2$, "Date " & brkdate$ &          ~
                " Total:", thrs(e%), tval(e%)
            linecount% = linecount% + 1%
            thrs(e% + 1%) = thrs(e% + 1%) + thrs(e%) /* Roll up totals */
            tval(e% + 1%) = tval(e% + 1%) + tval(e%) /* Roll up totals */
            thrs(e%), tval(e%) = 0           /* Zero this level totals */
            if tc_date$ = hex(ffffffffffffffffffff) then return
        set_date
            brkdate$ = tc_date$
            return

        deffn'220(z%)                /* Edit asterisks for total print */
            asterisk2$ = str(asterisk1$,,z%)
            call "STRING" addr ("RJ", asterisk2$, len(str(asterisk2$)))
            return

        underscore_totals
            print using L60610                 /* Underscore for totals */
            linecount% = linecount% + 1%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20160,         /* Time Card Date range   */~
                              L20200,         /* Department Code range  */~
                              L20240,         /* T/C Task Code range    */~
                              L20280,         /* Employee Code range    */~
                              L20320,         /* Labor Class Code range */~
                              L20360          /* Sort Sequence code     */
            return

L20160: REM Def/Enable T/C Date range              TC_DATE$()
            if str(tc_date$()) = " " then tc_date$(1%) = "ALL"
            return

L20200: REM Def/Enable Department Code range       DEPARTM$()
            if str(departm$()) = " " then departm$(1%) = "ALL"
            return

L20240: REM Def/Enable T/C Task Code range         TC_TASK$()
            if str(tc_task$()) = " " then tc_task$(1%) = "ALL"
            return

L20280: REM Def/Enable Employee Code range         EMPLOYE$()
            if str(employe$()) = " " then employe$(1%) = "ALL"
            return

L20320: REM Def/Enable Labor Class Code range      LBRCLAS$()
            if str(lbrclas$()) = " " then lbrclas$(1%) = "ALL"
            return

L20360: REM Def/Enable Sort Sequence               SORTSEQ$
            if sortseq$ = " " then sortseq$ = "1"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter T/C Date range, 'FIRST', 'LAST', 'ALL', or 'TODAY' (inclu~
        ~des Shift).",                                                    ~
         "Enter Department Code range, partial, 'FIRST', 'LAST', 'ALL' or~
        ~ '?' Wildcard.",                                                 ~
         "Enter T/C Task Code range, partial, 'FIRST', 'LAST', 'ALL' or '~
        ~?' Wildcard.",                                                   ~
         "Enter Employee Code range, partial, 'FIRST', 'LAST', 'ALL' or '~
        ~?' Wildcard.",                                                   ~
         "Enter Labor Class Code range, partial, 'FIRST', 'LAST', 'ALL' o~
        ~r '?' Wildcard.",                                                ~
         "Enter Sort Sequence Code 1 thru 5.                           "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, departm$(), employe$(),    ~
                lbrclas$(), sortseq$, tc_date$(), tc_task$()
            call "ALLFREE"
            infac$ = hex(9c)                      /* Hide the INFOMSG$ */
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *              R E C O R D   L A Y O U T S                  *~
            *************************************************************

L35040:     FMT /* File #01- JBTCTASK key data fields (Read only)      */~
                CH(6),               /*   1/ 6 Time Card Date          */~
                CH(4),               /*   7/ 4 Department Code         */~
                CH(6),               /*  11/ 6 Time Card Task Code     */~
                CH(12),              /*  17/12 Employee Code           */~
                CH(1),               /*  29/ 1 Shift                   */~
                CH(4),               /*  30/ 4 Labor Class Code        */~
                CH(1),               /*  34/ 1 Time Card Task Type Code*/~
                2*PD(14,4)           /*  35/16 Hours, Value            */

L35140:     FMT /* File #20- WORKFILE (I/O)                            */~
                CH(34), CH(4), CH(1), 2*PD(14,4)

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L40200,         /* Time Card Date range   */~
                              L40200,         /* Department Code range  */~
                              L40200,         /* T/C Task Code range    */~
                              L40200,         /* Employee Code range    */~
                              L40200,         /* Labor Class Code range */~
                              L40210          /* Sort Sequence code     */
            goto L40230

                lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:         lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:         lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
                at (01,02), "Time Card Task Code Summary Print/Purge",   ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                at (04,02), fac(infac$),  infomsg$              , ch(79),~
                at (06,30), fac(hex(ac)), col_header$           , ch(25),~
                                                                         ~
                at (07,02), "T/C Date range",                            ~
                at (07,30), fac(lfac$(1%)), tc_date$(1%)        , ch(10),~
                at (07,43), fac(lfac$(1%)), tc_date$(2%)        , ch(10),~
                                                                         ~
                at (08,02), "Department Code range",                     ~
                at (08,30), fac(lfac$(2%)), departm$(1%)        , ch(05),~
                at (08,43), fac(lfac$(2%)), departm$(2%)        , ch(05),~
                                                                         ~
                at (09,02), "T/C Task Code range",                       ~
                at (09,30), fac(lfac$(3%)), tc_task$(1%)        , ch(06),~
                at (09,43), fac(lfac$(3%)), tc_task$(2%)        , ch(06),~
                                                                         ~
                at (10,02), "Employee Code range",                       ~
                at (10,30), fac(lfac$(4%)), employe$(1%)        , ch(12),~
                at (10,43), fac(lfac$(4%)), employe$(2%)        , ch(12),~
                                                                         ~
                at (11,02), "Labor Class Code range",                    ~
                at (11,30), fac(lfac$(5%)), lbrclas$(1%)        , ch(05),~
                at (11,43), fac(lfac$(5%)), lbrclas$(2%)        , ch(05),~
                                                                         ~
                at (12,02), "Sort Sequence Code",                        ~
                at (12,30), fac(lfac$(6%)), sortseq$            , ch(01),~
                at (12,33), "1 = Date & Shift / Dept / Task / Employee", ~
                at (13,33), "2 = Dept / Date & Shift / Task / Employee", ~
                at (14,33), "3 = Employee / Dept / Date & Shift / Task", ~
                at (15,33), "4 = Employee / Date & Shift / Dept / Task", ~
                at (16,33), "5 = Task / Date & Shift / Dept / Employee", ~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L40700
                 call "MANUAL" ("JBTASKPR") : goto L40230

L40700:     if keyhit% <> 15% then L40730
                 call "PRNTSCRN" : goto L40230

L40730:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit% = 2% then L40920     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40880
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40880:     if fieldnr% > 1% then L40900
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40900:     return

L40920: if fieldnr% > 0% then L41010  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "(24)PURGE ONLY         (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "(32)PURGE and Print    (16)Print ONLY  "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10002018ff)
            return
L41010:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50160,         /* Time Card Date range   */~
                              L50610,         /* Department Code range  */~
                              L50660,         /* T/C Task Code range    */~
                              L50720,         /* Employee Code range    */~
                              L50780,         /* Labor Class Code range */~
                              L50950          /* Sort Sequence code     */
            return

L50160: REM Test for T/C Date range               TC_DATE$()
*        First, validate the 'From' Date.
            if tc_date$(1%) <> "ALL" then goto L50230
                tc_date$(2%) = " "
                tc_date$(3%) = all(hex(00))
                tc_date$(4%) = all(hex(ff))
                goto L50550
L50230:     if tc_date$(1%) <> "FIRST" then goto L50260
                tc_date$(3%) = all(hex(00))
                goto L50380
L50260:     if tc_date$(1%) <> "LAST" then goto L50290
                tc_date$(3%) = all(hex(ff))
                goto L50380
L50290:     if tc_date$(1%) <> "TODAY" then goto L50320
                tc_date$(3%) = date
                goto L50360
L50320:     call "DATEOKC" (tc_date$(1%), u3%, errormsg$)
            if errormsg$ <> " " then return
            tc_date$(3%) = tc_date$(1%)
            call "DATUFMTC" (tc_date$(3%))
L50360:     str(tc_date$(3%),,6%) = addc all(hex(ff))     /* For PLOWs */

L50380
*        Then, of course, the 'To' Date.
            if tc_date$(1%) <> "ALL" and (tc_date$(2%) = " " or ~
               tc_date$(2%) = blankdate$) then tc_date$(2%) = tc_date$(1%)
            if tc_date$(2%) <> "FIRST" then goto L50440
                tc_date$(4%) = all(hex(00))
                goto L50550
L50440:     if tc_date$(2%) <> "LAST" then goto L50470
                tc_date$(4%) = all(hex(ff))
                goto L50550
L50470:     if tc_date$(2%) <> "TODAY" then goto L50500
                tc_date$(4%) = date
                goto L50550
L50500:     call "DATEOKC" (tc_date$(2%), u3%, errormsg$)
            if errormsg$ <> " " then return
            tc_date$(4%) = tc_date$(2%)
            call "DATUFMTC" (tc_date$(4%))

L50550
*        Test to make sure there is an actual FROM - TO range.
            if tc_date$(3%) <= tc_date$(4%) then return
                errormsg$ = "The 'FROM' date must be earlier than or eq"&~
                     "ual to the 'TO' Date."
                return

L50610: REM Test for Department Code range        DEPARTM$()
            call "TESTRNGE" (departm$(1%), departm$(2%), departm$(3%),   ~
                departm$(4%), errormsg$, #05)              /* PRLDEPTF */
            return

L50660: REM Test for T/C Task Code range          TC_TASK$()
            call "TESTRNGE" (tc_task$(1%), tc_task$(2%), tc_task$(3%),   ~
                tc_task$(4%), errormsg$, #04)              /* JBTCCDES */
            tc_task$(3%) = addc (hex(01))
            return

L50720: REM Test for Employee Code range          EMPLOYE$()
            call "TESTRNGE" (employe$(1%), employe$(2%), employe$(3%),   ~
                employe$(4%), errormsg$, #06)              /* PERMASTR */
            employe$(3%) = addc (hex(01))
            return

L50780: REM Test for Labor Class Code range       LBRCLAS$()
            if pos(lbrclas$(1%) = "?") = 0% then goto L50850
                str(lbrclas$(1%),pos(lbrclas$(1%)="?"),1%) = " "
                readkey$ = "LBR CLASS" & lbrclas$(1%)
                msg$ = hex(06) & "Select a 'FROM' Labor Class."
                call "PLOWCODE" (#03, readkey$, msg$, 9%, .3, f1%(3%))
                if f1%(3%) <> 0% then lbrclas$(1%) = str(readkey$,10%,4%)
L50850:     if pos(lbrclas$(2%) = "?") = 0% then goto L50910
                str(lbrclas$(2%),pos(lbrclas$(2%)="?"),1%) = " "
                readkey$ = "LBR CLASS" & lbrclas$(2%)
                msg$ = hex(06) & "Select a 'TO' Labor Class."
                call "PLOWCODE" (#03, readkey$, msg$, 9%, .3, f1%(3%))
                if f1%(3%) <> 0% then lbrclas$(2%) = str(readkey$,10%,4%)
L50910:     call "TESTRNGE" (lbrclas$(1%), lbrclas$(2%), lbrclas$(3%),   ~
                lbrclas$(4%), errormsg$)
            return

L50950: REM Test for Sort Sequence                SORTSEQ$
            on pos("12345" = sortseq$) goto L51000, L51000, L51000, L51000,  ~
                L51000
                errormsg$ = "Sort Sequence must be 1 thru 5. See list."
                return
L51000:     convert sortseq$ to sortseq%, data /* HUH? */ goto L51030
            return

L51030:     errormsg$ = "Invalid data in Sort Sequence. Try again."
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                      JBTASKPR####~
        ~###
L60070: %                                   #############################~
        ~###############################                           PAGE: #~
        ~###
L60100: % TIME CARD        T/C    EMPLOYEE     LABOR TASK                ~
        ~                                                                 ~

L60130: %DATE  /SHIFT DEPT TASK   CODE         CLASS TYPE                ~
        ~                                                   HOURS       VA~
        ~LUE
L60160: %                                       TIME CARD   T/C    EMPLOY~
        ~EE     LABOR TASK                                                ~

L60190: %DEPT DESCRIPTION                      DATE / SHIFT TASK   CODE  ~
        ~       CLASS TYPE                                  HOURS       VA~
        ~LUE
L60220: %EMPLOYEE                                            TIME CARD   ~
        ~T/C    LABOR TASK                                                ~

L60250: %CODE         NAME                             DEPT DATE  /SHIFT ~
        ~TASK   CLASS TYPE                                  HOURS       VA~
        ~LUE
L60280: %EMPLOYEE                                       TIME CARD        ~
        ~T/C    LABOR TASK                                                ~

L60310: %CODE         NAME                             DATE  /SHIFT DEPT ~
        ~TASK   CLASS TYPE                                  HOURS       VA~
        ~LUE
L60340: %T/C                                      TIME CARD        EMPLOY~
        ~EE     LABOR TASK                                                ~

L60370: %TASK   DESCRIPTION                      DATE / SHIFT DEPT CODE  ~
        ~       CLASS TYPE                                  HOURS       VA~
        ~LUE
L60400: %----------------------------------------------------------------~
        ~-------------------------------------------- ----------- --------~
        ~---
L60430: %##########/# #### ###### ############ ####   #                  ~
        ~                                             ########.## ########~
        ~.##
L60460: %#### ################################ ##########/# ###### ######~
        ~###### ####   #                              ########.## ########~
        ~.##
L60490: %############ ################################ #### ##########/# ~
        ~ ###### ####   #                              ########.## #######~
        ~#.##
L60520: %############ ################################ ##########/# #### ~
        ~ ###### ####   #                              ########.## #######~
        ~#.##
L60550: %###### ################################ ##########/# #### ######~
        ~###### ####   #                              ########.## ########~
        ~.##
L60580: %################ ###############################################~
        ~############################################ ########.## ########~
        ~.##
L60610: %                                                                ~
        ~                                             ----------- --------~
        ~---
L60640: %                                                   ** END OF REP~
        ~ORT @ ######## **

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
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
