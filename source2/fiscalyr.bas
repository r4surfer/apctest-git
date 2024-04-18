        REM *************************************************************~
            *                                                           *~
            *  FFFFF  IIIII   SSS    CCC    AAA   L      Y   Y  RRRR    *~
            *  F        I    S      C   C  A   A  L      Y   Y  R   R   *~
            *  FFFF     I     SSS   C      AAAAA  L       YYY   RRRR    *~
            *  F        I        S  C   C  A   A  L        Y    R   R   *~
            *  F      IIIII   SSS    CCC   A   A  LLLLL    Y    R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FISCALYR - This program allows the user to input the      *~
            *            period breakdown of their fiscal year, and the *~
            *            number of the primary period currently open.   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/01/81 ! ORIGINAL                                 ! TOM *~
            * 11/08/83 ! CHANGED TO CONFORM TO G/L REVISIONS      ! HES *~
            * 03/04/86 ! Period Ending Dates-- save unformatted   ! ERN *~
            * 04/09/86 ! Edit dates for next fiscal year start    ! HES *~
            * 09/23/86 ! Removed SA Month references              ! ERN *~
            * 05/21/87 ! Support of obsolete flag on GLMAIN       ! HES *~
            * 09/21/87 ! Added FISCAL HISTORY record to SYSFILE2  ! LKM *~
            *          ! to save fiscal date structure for prior  !     *~
            *          ! years.                                   !     *~
            * 01/04/89 ! Fixed period-end computation (PRR 10354).! JIM *~
            * 04/03/92 ! PRR 12157 - Modified to allow more than  ! MLJ *~
            *          !   a 365 day calendar.                    !     *~
            * 06/30/92 ! Now writes a next year FISCAL HISTORY    ! JDH *~
            *          !  record based on periods 14-17 of this yr!     *~
            * 04/19/93 ! PRR 12817  Only calc previous year's end ! JDH *~
            *          !  dates if necessary (i.e. brand new).    !     *~
            *          ! Changed leap year logic.                 !     *~
            * 09/18/96 ! Changes for the year 2000.               ! DXL *~
            * 06/19/97 ! Corrected month in SYS2-FISCAL YEAR BEGIN! DER *~
            *************************************************************
        dim                                                              ~
            adjacct$16,                  /* GL ACCOUNT CODE            */~
            acctdescr$32,                /* GL ACCOUNT CODE description*/~
            cursor%(2),                  /* CURSOR LOCATIONS FOR EDIT  */~
            date$8,                      /* TODAY'S CALENDAR DATE      */~
            dates$(17)10,                /* PERIOD START DATES         */~
            datedescr$(17)18,            /* DATE NAMES                 */~
            days$3,                      /* LENGTH OF CALENDAR         */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fmted$(32)8,                 /* PERIOD ENDING DATES+PRIORYR*/~
            glnumstring$130,             /* GLMAIN NUMBER STRING       */~
            header1$6,                   /* SUB TITLE                  */~
            header2$29,                  /* SUB TITLE                  */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            i$(24)80,                    /* SCREEN IMAGE FOR EDIT      */~
            julian$7,                    /* Julian date                */~
            dfac$(25)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfac$(25)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            month$(12)9,                 /* NAMES OF MONTHS            */~
            monthopen$2,                 /* PERIOD CURRENTLY OPEN      */~
            opendescr$35,                /* DESCR OF PERIOD(S) OPEN    */~
            periods$2,                   /* NUMBER OF PERIODS IN YEAR  */~
            pf16$16,                     /* PF16 Prompt                */~
            readkey$20,                  /* All Purpose Read Key       */~
            save$(17)10,                 /* Save edited dates          */~
            tdate$10,                    /* Temporary Date Variable    */~
            temp$10,                     /* NUMBER OF PERIODS IN YEAR  */~
            title$58,                    /* Screen Title               */~
            udate$8,                     /* Temporary Date Variable    */~
            yymmdd$6                     /* Work date                  */


        dim f1%(24)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE (NEXT CHECK #)   *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER ACCOUNT MASTER FILE       *~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos =   1, keylen =  20

            select #2,  "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9

            call"SHOSTAT"("Preparing For G/L Fiscal Calendar Management")

            call "OPENCHCK" (#1, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            title$ = "Define G/L Fiscal Year Structure    Todays Date: " ~
                                                                  & date$
            header1$ = "Period"
            header2$ = "          Start Dates"
            init (hex(00)) glnumstring$

            month$( 1) = "January"
            month$( 2) = "February"
            month$( 3) = "March"
            month$( 4) = "April"
            month$( 5) = "May"
            month$( 6) = "June"
            month$( 7) = "July"
            month$( 8) = "August"
            month$( 9) = "September"
            month$(10) = "October"
            month$(11) = "November"
            month$(12) = "December"

            gosub L30000

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            inputmode% = 1%
            init(" ") errormsg$, inpmessage$, dates$(), periods$,        ~
                      datedescr$(), opendescr$, monthopen$,              ~
                      adjacct$, acctdescr$

            for fieldnr% = 1% to  20%
                gosub'161(fieldnr%)
                      if enabled% = 0% then L10220
L10150:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
                if fieldnr% = 13 and periods% = 12 then fieldnr% = 14
L10220:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        editmode
            inputmode% = 0
            inpmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."
L11100:     gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 and errormsg$ = " " then datasave
                  if keyhit% <>  0 then       L11100
            fieldnr% = cursor%(1) - 2
            if fieldnr% = 1 then L11100
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 2 or fieldnr% > 14 then L11100
            if cursor%(2) < 40 then L11260
            if fieldnr% > 5 then L11220
                fieldnr% = fieldnr% + 13
                goto L11270
L11220:     if fieldnr% = 8 then fieldnr% = 19
            if fieldnr% = 12 then fieldnr% = 20
            if fieldnr% < 19 or fieldnr% >  20 then L11100

L11260:     if fieldnr% = 14 and periods% = 12 then L11100
L11270:     gosub'211(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11270
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11270
            goto L11100

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000                  /* WRITE FISCAL YR BEGIN MONTH*/
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  inpmessage$ = " "
                  if fieldnr% = 19% then L20320     /* ACCOUNT CODE     */
                  on fieldnr% goto  L20140,         /* # PERIODS        */~
                                    L20210          /* YEAR START DATE  */
                  goto L20270                       /* PERIOD START DATS*/

L20140:     REM DEFAULT/ENABLE FOR NUMBER OF PERIODS
                enabled% = 1
                inpmessage$ = "Enter 12 or 13 To Indicate The Number Of P~
        ~eriods In Your Fiscal Year."
                periods$ = "12"
                return

L20210:     REM DEFAULT/ENABLE FOR FISCAL YEAR BEGINNING DATE
                enabled% = 1
                inpmessage$ = "Enter the first day in your current fiscal~
        ~ year."
                return

L20270:     REM DEFAULT/ENABLE FOR FISCAL YEAR BEGINNING OF EACH PERIOD
                enabled% = 1
                inpmessage$ = "Enter the First day of this period"
                return

L20320:     REM DEFAULT/ENABLE FOR GL ACCOUNT CODE
                enabled% = 1%
                inpmessage$ = "Leave Blank To Search For Existing Account~
        ~, Or Enter New Account To Be Created."
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            k% = 0%
            call "STARTOVR" (k%)
            on k%+1% goto L29942, L29948
            return

L29942:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return

L30000: REM *************************************************************~
            *      L O A D   F I S C A L   D A T E S   R E C O R D      *~
            *                                                           *~
            * LOADS THE FISCAL PERIOD STRUCTURE FROM SYSFILE2.          *~
            *************************************************************

            call "READ101" (#1, "FISCAL DATES", on_file%)
                 if on_file% = 0% then return

            get #1, using L30110, periods%, dates$(), monthopen%,         ~
                                 fmted$(), adjacct$, data goto inputmode
L30110:     FMT XX(20), BI(2), 17*CH(08), BI(2), 32*CH(8), CH(16)

            l% = 14%
            call "DATE" addr("G-",str(dates$(1),,6),str(dates$(l%),,6),  ~
                                                                  l%,u3%)
            convert periods% to periods$, pic(##)
            convert monthopen% to monthopen$, pic(##)
            call "DESCRIBE" (#2, adjacct$, acctdescr$, 1%, f1%(2))
            if f1%(2) = 0 then acctdescr$ = hex(94)& "(Will Be Created)"
            call "STRING" addr("CT", acctdescr$, 32%)
            call "GLFMT" (adjacct$)
            gosub format_dates
            goto editmode

L31000: REM *************************************************************~
            *       S A V E   F I S C A L  D A T E S   R E C O R D      *~
            * WRITES OR REWRITES THE FISCAL YEAR PERIOD STRUCTURE TO    *~
            * THE FILE SYSFILE2                                         *~
            *************************************************************

            call "SHOSTAT" ("Saving Fiscal Dates Structure")
            if adjacct$ <> " " then L31100
            return clear : fieldnr% = 16% : goto  L11270

L31100:     gosub unformat_dates
            convert periods$ to periods%, data goto editmode
            convert monthopen$ to monthopen%, data goto editmode
            gosub set_period_ending_dates
            call "GLUNFMT" (adjacct$)
            put #1, using L31180, "FISCAL DATES", periods%, dates$(),     ~
                                  monthopen%, fmted$(), adjacct$, " "

L31180:             FMT CH(20), BI(2), 17*CH(8), BI(2), 32*CH(8), CH(16),~
                        CH(68)
            if on_file% <> 0% then rewrite # 1                           ~
                              else   write # 1

            tdate$ = dates$(1%)
            call "DATEFMT" (tdate$, 0%, udate$)

            call "READ101" (#1, "FISCAL YEAR BEGINS", f1%(1))
            call "STRING" addr ("LJ", datedescr$(1), 18%)
            put #1, using L31260,"FISCAL YEAR BEGINS", str(udate$,5,2), ~
            str(datedescr$(1),,max(1,pos(datedescr$(1)=" ")))," "," "," "
L31260:     FMT CH(20), CH(2), CH(20),CH(200), CH(200), CH(58)
            if f1%(1) <> 0 then rewrite # 1                              ~
                           else   write # 1

*        Write the fiscal period dates to a history record
            tdate$ = dates$(1%)
            call "DATEFMT" (tdate$, 0%, udate$)
            readkey$ = "FISCAL HISTORY" & str(udate$,1%,6%)
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L31294, readkey$, periods%, dates$()
L31294:     FMT CH(20), BI(2), 17*CH(8)
            if f1%(1) <> 0 then rewrite # 1                              ~
                           else   write # 1

*        Now do a next years tentative period dates based on 14-17
            tdate$ = dates$(14%)
            call "DATEFMT" (tdate$, 0%, udate$)
            readkey$ = "FISCAL HISTORY" & str(udate$,1%,6%)
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L31310, readkey$, periods%, dates$(14),          ~
                                dates$(15), dates$(16), dates$(17), " "
L31310:         FMT CH(20), BI(2), 4*CH(8), CH(104)
            if f1%(1) <> 0 then rewrite # 1 else write # 1               ~

            call "READ100" (#2, str(adjacct$,,9), f1%(2))
            if f1%(2) <> 0 then return

            REM Create system catch-all account if need be...
            write #2, using L31360, adjacct$, "System Suspense Account",  ~
                    "E", glnumstring$, glnumstring$
L31360:     FMT CH(9), CH(30), CH(1), CH(130), CH(130)
            return

        REM *************************************************************~
            *                F O R M A T    D A T E S                   *~
            *                                                           *~
            * PUTS ALL DATES INTO MM/DD/YY FORMAT. BUCKET 14 IS DEAD.   *~
            *************************************************************

        format_dates
            for u3% = 1% to 17%
                tdate$ = dates$(u3%)
                call "DATEFMT" ( tdate$, 0%, udate$ )
                convert str(udate$,5%,2%) to month%, data goto L32150
                convert str(udate$,7%,2%) to day%, data goto L32150
                datedescr$(u3%) = month$(month%) & " " &  ~
                    str(udate$,7%,2%) & ", " & str(udate$,1%,4%)
                call "STRING" addr ("RJ", datedescr$(u3%), 18%)
                call "DATFMTC"  (dates$(u3%))
L32150:     next u3%
            convert periods$ to periods%, data goto L32180

L32180:     convert monthopen$ to monthopen%, data goto L32270
            also1% = monthopen%-1%
            if also1% = 13% and periods% = 12% then also1% = 12%
            also2% = monthopen%+1%
            if also2% = 13% and periods% = 12% then also2% = 14%
            opendescr$ = " "
            if monthopen%=1% then opendescr$="PERIOD 2 Is Also Postable" ~
             else  put opendescr$, using L32250, also1%, also2%
L32250:         %PERIODS ## & ## Are Also Postable
            call "STRING" addr("CT", opendescr$, 35%)
L32270: return

        REM *************************************************************~
            *             U N - F O R M A T    D A T E S                *~
            *                                                           *~
            * PUTS ALL DATES INTO YYMMDD FORMAT. BUCKET 14 IS DEAD.     *~
            *************************************************************

        unformat_dates
            save$() = dates$()
            for u3% = 1% to 17%
                call "DATUFMTC"  (dates$(u3%))
            next u3%

            t% = periods%
            for u3% = 2% to t%
                if dates$(u3%) < dates$(u3% - 1%) then L33250
            next u3%
            if dates$(14%) < dates$(t%) then L33250
            for u3% =  15% to 17%
                if dates$(u3%) < dates$(u3% - 1%) then L33250
            next u3%

            t% = 14%
            call "DATE" addr("G-",str(dates$(1%),,6%),str(dates$(t%),,6%),  ~
                                                                t%,u3%)
            if u3% <> 0% then L33250
                if l% = t% then L33230
                    convert t% to days$, pic(###)
L33216:             u3% = 2%
                call "ASKUSER" (u3%, "*** CALENDAR LENGTH ***",          ~
                     "Your Calendar is set for " & days$ & " days.",     ~
                     " ", "Press RETURN to Acknowledge and continue.")
                if u3% <> 0% then L33216
L33230: return

L33250:     errormsg$ = "NOT SAVED... DATE STRUCTURE IS INVALID"
            dates$()=save$() /* Restore edited dates */
            return clear all
            goto editmode

        REM *************************************************************~
            *       S E T   P E R I O D   E N D I N G    D A T E S      *~
            * CREATES AN ARRAY CONTAINING PERIOD ENDING DATES RATHER    *~
            * THEN STARTING DATES. DATES START WITH WHAT WOULD BE THE   *~
            * THE PREVIOUS YEAR.  ELEMENT 16 IS THE ENDING DATE OF THE  *~
            * FIRSTPERIOD IN THE CURRENT YEAR... EACH DATE CORROSPONDS  *~
            * DIRECTLY WITH A BUCKET IN THE GL MAIN RECORD.             *~
            *************************************************************

        set_period_ending_dates
*        1st set buckets 15-31 with the current year's starting dates
            for u3% = 15% to 31%
                fmted$(u3%) = dates$(u3% - 14%)
            next u3%

*        If this is not a brand new record, don't mess with previous year
            if on_file% = 1% then L34190

*        Set previous year's ending dates to same as current year's minus
*        a year
            for u3% = 1% to 13%
                fmted$(u3%) = dates$(u3%)
            next u3%
            fmted$(14%) = fmted$(1%)

            for u3% = 1% to periods%
                yymmdd$ = str(fmted$(u3%),,6%) : incr% = -1%
                gosub same_date_different_year
                str(fmted$(u3%),,6%) = yymmdd$
                call "DATE" addr ("G+", str(fmted$(u3%),,6%), -1%,       ~
                                        str(fmted$(u3%),,6%), err%)
                if err% <> 0% then L33250
            next u3%

            call "DATE" addr ("G+", str(fmted$(14%),,6%), -1%,           ~
                                    str(fmted$(14%),,6%), err%)
            if err% <> 0% then L33250

            if periods% = 13% then L34190
                fmted$(13%) = fmted$(14%)
                fmted$(14%) = " "

*        Now set current year's ending dates
L34190:         for u3% = 16% to 31%
                   if u3% = 27% and periods% = 12% then L34240
                   call "DATE" addr ("G+", str(fmted$(u3%),,6%), -1%,    ~
                                           str(fmted$(u3%),,6%), err%)
                   if err% <> 0% then L33250
L34240:         next u3%

                fmted$(32%) = fmted$(19%)
                yymmdd$ = str(fmted$(32%),,6%) : incr% = 1%
                gosub same_date_different_year
                str(fmted$(32%),,6%) = yymmdd$

                if periods% = 13% then L34370
                     fmted$(27%) = fmted$(28%)
                     fmted$(28%) = " "

*        Bucket 15 is always CLOSING
L34370:         fmted$(15%) = "CLOSING"

        return

        REM Routine computes the same date in another year.
        same_date_different_year
            call "DATE" addr ("GJ", yymmdd$, julian$, err%)
            if err% <> 0 then L33250
            call "DATJULCV" ( julian$ )            /* to jSTD  */
            convert str(julian$,1%,4%) to year%, data goto L33250
            convert str(julian$,5%,3%) to days%, data goto L33250

*        Adjust number of days to account for leap years.
            if days%>59% and mod(year%+incr%,4%)=0% then days%=days%+1%
            if days%>60% and mod(year%+incr%,4%)=3% then days%=days%-1%
*          IF DAYS%=60% AND MOD(YEAR%+INCR%,4%)=0% THEN DAYS%=61%

            year% = year% + incr%
            convert year%*1000%+days% to julian$, pic (#######)
            call "DATJULCV" ( julian$ )            /* to jI.F. */
            call "DATE" addr ("JG", julian$, yymmdd$, err%)
            if err% <> 0 then L33250
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                pf16$ = "(16)Exit Program"
                if fieldnr% > 1 then pf16$ = " "
                goto L40150

            deffn'211(fieldnr%)
                pf16$ = "(16)Save Data   "
                if fieldnr% <> 0 then pf16$ = " "

L40150:           init(hex(8c)) lfac$(), dfac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$()
*                IF FIELDNR% = 0% THEN INIT(HEX(84)) DFAC$()
                  on fieldnr% gosub L40480,         /* # PERIODS        */~
                                    L40440,         /* FIRST DAY OF FYR */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* NEXT PERIOD      */~
                                    L40440,         /* GL ACCOUNT CODE  */~
                                    L40480          /* PERIOD OPEN      */
                     goto L40530

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      dfac$(fieldnr%) = hex(84)
                      return
L40440:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      dfac$(fieldnr%) = hex(84)
                      return
L40480:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      dfac$(fieldnr%) = hex(84)
                      return

L40530: accept                                                           ~
            at (01,12), fac(hex(ac)),   title$                  , ch(57),~
            at (02,02), fac(hex(94)),   errormsg$               , ch(79),~
            at (03,02), "Number of Periods in Fiscal Year (12 or 13)?",  ~
            at (03,47), fac(lfac$( 1)), periods$                , ch(02),~
            at (05,03), "      Current Fiscal Year Structure          Nex~
        ~t Fiscal Year Overflow       ",                                 ~
            at (06,04), fac(hex(ac)),   header1$                , ch(06),~
            at (06,11), fac(hex(ac)),   header2$                , ch(29),~
            at (06,42), fac(hex(ac)),   header1$                , ch(06),~
            at (06,49), fac(hex(ac)),   header2$                , ch(29),~
            at (07,3), " 1)",  at (07,42), "14)",                        ~
            at (08,3), " 2)",  at (08,42), "15)",                        ~
            at (09,3), " 3)",  at (09,42), "16)",                        ~
            at (10,3), " 4)",  at (10,42), "17)",                        ~
            at (11,3), " 5)",                                            ~
            at (12,3), " 6)",  at (12,41), "    System Suspense Account N~
           ~umber:    ",                                                 ~
            at (13,3), " 7)",                                            ~
            at (14,3), " 8)",                                            ~
            at (15,3), " 9)",                                            ~
            at (16,3), "10)",  at (16,41), "      The Current Period Open~
           ~ is:      ",                                                 ~
            at (17,3), "11)",                                            ~
            at (18,3), "12)",                                            ~
            at (19,3), "13)",                                            ~
                                                                         ~
            at (07,11), fac(lfac$( 2)), dates$( 1)              , ch(10),~
            at (07,22), fac(dfac$( 2)), datedescr$( 1)          , ch(18),~
            at (07,49), fac(lfac$(15)), dates$(14)              , ch(10),~
            at (07,60), fac(dfac$(15)), datedescr$(14)          , ch(18),~
            at (08,11), fac(lfac$( 3)), dates$( 2)              , ch(10),~
            at (08,22), fac(dfac$( 3)), datedescr$( 2)          , ch(18),~
            at (08,49), fac(lfac$(16)), dates$(15)              , ch(10),~
            at (08,60), fac(dfac$(16)), datedescr$(15)          , ch(18),~
            at (09,11), fac(lfac$( 4)), dates$( 3)              , ch(10),~
            at (09,22), fac(dfac$( 4)), datedescr$( 3)          , ch(18),~
            at (09,49), fac(lfac$(17)), dates$(16)              , ch(10),~
            at (09,60), fac(dfac$(17)), datedescr$(16)          , ch(18),~
            at (10,11), fac(lfac$( 5)), dates$( 4)              , ch(10),~
            at (10,22), fac(dfac$( 5)), datedescr$( 4)          , ch(18),~
            at (10,49), fac(lfac$(18)), dates$(17)              , ch(10),~
            at (10,60), fac(dfac$(18)), datedescr$(17)          , ch(18),~
            at (11,11), fac(lfac$( 6)), dates$( 5)              , ch(10),~
            at (11,22), fac(dfac$( 6)), datedescr$( 5)          , ch(18),~
            at (12,11), fac(lfac$( 7)), dates$( 6)              , ch(10),~
            at (12,22), fac(dfac$( 7)), datedescr$( 6)          , ch(18),~
            at (13,11), fac(lfac$( 8)), dates$( 7)              , ch(10),~
            at (13,22), fac(dfac$( 8)), datedescr$( 7)          , ch(18),~
            at (14,11), fac(lfac$( 9)), dates$( 8)              , ch(10),~
            at (14,22), fac(dfac$( 9)), datedescr$( 8)          , ch(18),~
            at (15,11), fac(lfac$(10)), dates$( 9)              , ch(10),~
            at (15,22), fac(dfac$(10)), datedescr$( 9)          , ch(18),~
            at (16,11), fac(lfac$(11)), dates$(10)              , ch(10),~
            at (16,22), fac(dfac$(11)), datedescr$(10)          , ch(18),~
            at (17,11), fac(lfac$(12)), dates$(11)              , ch(10),~
            at (17,22), fac(dfac$(12)), datedescr$(11)          , ch(18),~
            at (18,11), fac(lfac$(13)), dates$(12)              , ch(10),~
            at (18,22), fac(dfac$(13)), datedescr$(12)          , ch(18),~
            at (19,11), fac(lfac$(14)), dates$(13)              , ch(10),~
            at (19,22), fac(dfac$(14)), datedescr$(13)          , ch(18),~
                                                                         ~
            at (13,56), fac(lfac$(19)), adjacct$                , ch(12),~
            at (14,44), fac(dfac$(19)), acctdescr$              , ch(32),~
            at (17,59), fac(lfac$(20)), monthopen$              , ch(02),~
            at (18,43), fac(dfac$(20)), opendescr$              , ch(35),~
                                                                         ~
            at (22,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (23,02), "(1)Start Over",                                 ~
            at (23,45), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), fac(hex(84)),   pf16$                   , ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L41420
                call "MANUAL" ("FISCALYR")
                goto L40530

L41420:        if keyhit% <> 15 then L41460
                  call "PRNTSCRN"
                  goto L40530

L41460:        if fieldnr% <> 0 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50300,         /* # PERIODS        */~
                                    L50370,         /* START DATE       */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50370,         /* NEXT DATE        */~
                                    L50540,         /* GL ADJACCT$      */~
                                    L50700          /* PERIOD OPEN      */
                     return

L50300:     REM TEST DATA FOR NUMBER OF PERIODS
                if periods$ <> "12" and periods$ <> "13" then L50340
                convert periods$ to periods%
                return
L50340:            errormsg$ = "Please Enter 12 Or 13"
                   return

L50370:     REM TEST DATA FOR FIRST DAYS OF FISCAL PERIODS
                t% = fieldnr% - 1%
                temp$ = dates$(t%)
                call "DATEOKC" (temp$, u3%, errormsg$)
                if errormsg$ <> " " then return
                dates$(t%), tdate$ = temp$
                call "DATUFMTC" (tdate$, 0%, temp$)
                convert str(temp$,5%,2%) to month%
                convert str(temp$,7%,2%) to day%
                datedescr$(t%) = month$(month%) & " " & str(temp$,7%,2%) & ~
                                                ", " & str(temp$,1%,4%)
                call "DATECONV" (temp$)
                call "STRING" addr ("RJ", datedescr$(t%), 18%)
                if t% = 1% and inputmode% = 1% then gosub set_defaults
                if monthopen$<>" " then L50700
             return

L50540: REM TEST FOR GL ACCOUNT CODE
            if adjacct$ <> " " then L50630
            REM Note thet getcode returns 0 even if account is on file   ~
                if the account is flagged as obsolete...
            call "GETCODE" (#2, adjacct$, acctdescr$, 1%, 0, f1%(2))
            call "STRING" addr("CT", acctdescr$, 32%)
              if f1%(2) <> 0 then return
            errormsg$ = "If desired Account is not on file, type it in an~
        ~yway... It'll be created."
            return

L50630:     call "GLVALID" (adjacct$, temp$, errormsg$)
                if errormsg$ <> " " then return
            call "DESCRIBE" (#2, temp$, acctdescr$, 1%, f1%(2))
            if f1%(2) = 0 then acctdescr$ = hex(94)& "(Will Be Created)"
            call "STRING" addr("CT", acctdescr$, 32%)
            if f1%(2) = 0 then return
            get #2, using L50673, temp%
L50673:     FMT POS(41), BI(1)
            if temp% < 1% or temp% > 2% then return
                errormsg$ = "Sorry, This Account Is Flagged As Obsolete"
                return

L50700:     REM TEST DATA FOR MONTH CURRENTLY OPEN
                convert monthopen$ to t%, data goto L50930
                if t% < 1 or t% > 16 then L50930
                if periods% = 12 and t% = 13 then L50950

                temp$=dates$(t%)
                call "DATEOKC" (temp$, u3%, errormsg$)
                call "DATUFMTC" (temp$)

                also1% = t%-1
                if also1% = 13 and periods% = 12 then also1% = 12
                also2% = t%+1
                if also2% = 13 and periods% = 12 then also2% = 14
                opendescr$ = " "
                if t% = 1 then opendescr$ = "PERIOD 2 Is Also Postable"  ~
                else put opendescr$, using L50900, also1%, also2%
L50900:         %PERIODS ## & ## Are Also Postable
                call "STRING" addr("CT", opendescr$, 35%)
                return
L50930:      errormsg$ = "Must Be A Number Between 1 And 16"
             return
L50950:      errormsg$ = "Can't Be 13 Since There Are Only 12 Periods In ~
        ~Your Fiscal Year"
             return

        REM *************************************************************~
            *                 S E T   D E F A U L T S                   *~
            *                                                           *~
            * SETS THE DEFAULTS WHEN RECORD IS NOT ON FILE.             *~
            *************************************************************

        set_defaults
            call "DATUFMTC" (dates$(1%))
            if periods% = 13% then L52220

            for u3% = 2% to 12%
                tdate$ = dates$(u3%-1%)
                call "DATEFMT" (tdate$, 0%, udate$)
                convert str(udate$,5%,2%) to month%
                convert str(udate$,1%,4%) to year%
                if month% = 12% then year% = year% + 1%
                month% = month% + 1%
                if month% = 13% then month% = 1%
                dates$(u3%) = udate$
                convert month% to str(dates$(u3%),5%,2%), pic(00)
                convert year%  to str(dates$(u3%),1%,4%), pic(0000)
                call "DATECONV" (dates$(u3%))
            next u3%
            goto L52270

L52220:     for u3% = 2% to 13%
                call "DATE" addr("G+", str(dates$(u3%-1),,6), 28%,       ~
                                                str(dates$(u3%),,6), t%)
            next u3%

L52270:     for u3% = 14% to 17%       /* SET THE FOUR 'EXTRA' BUCKETS TO */
                dates$(u3%) = dates$(u3%-13%) /* CATCH OVERFLOW AT CLOSE */
                yymmdd$ = dates$(u3%) : incr% = 1%
                gosub same_date_different_year
                dates$(u3%) = yymmdd$
            next u3%
            gosub format_dates
            str(datedescr$(),19%) = " "
        return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
