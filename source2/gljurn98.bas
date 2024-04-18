        REM *************************************************************~
            *                                                           *~
            *   GGG   L      JJJJJ  U   U  RRRR   N   N   999    888    *~
            *  G      L        J    U   U  R   R  NN  N  9   9  8   8   *~
            *  G GGG  L        J    U   U  RRRR   N N N   9999   888    *~
            *  G   G  L      J J    U   U  R   R  N  NN      9  8   8   *~
            *   GGG   LLLLL   J      UUU   R   R  N   N   999    888    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLJURN98 - PRINTS A MONTHLY JOURNAL FOR ANY OF THE TEN    *~
            *            JOURNALS, FOR ANY OF THE THREE MONTHS OPEN     *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/30/86 ! ORIGINAL                                 ! RAC *~
            * 09/15/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/10/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 10/10/88 ! Really fixed call to JNLINFO.            ! JDH *~
            * 10/26/88 ! Break Logic.  Periods can now span Months! JDH *~
            *          !   & initialize totals.  Thanks to KAB    !     *~
            * 11/17/89 ! Consolidated periods into one report.    ! JDH *~
            * 04/10/90 ! Move Work File Sizing so it works        ! KAB *~
            * 12/14/90 ! Changed Call to JNLINFO for GL Batches   ! RAC *~
            * 01/11/93 ! Page 0 Facs          & End Report Time.  ! RJH *~
	    * 06/24/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            blankline$79,                /* LINE UNDERLINED IN SCREEN  */~
            chojrn$(15)1,                /* WHICH JOURNALS TO PRINT    */~
            chomon$(15)1,                /* WHICH MONTHS TO PRINT      */~
            compname$60,                 /* Company name               */~
            credit$13,                   /* Field to Print             */~
            cursor%(2),                                                  ~
            date$8,                      /* FORMATTED G/L DATE         */~
            dates$(32)8,                 /* FISCAL DATE STRUCTURE      */~
            day1$(3)6,                   /* FIRST OF RELEVENAT MONTH   */~
            day2$(3)6,                   /* LAST  OF RELEVANT MONTH    */~
            debit$13,                    /* Field to Print             */~
            descr$36,                    /* ACCOUNT DESCRIPTION        */~
            dual_books$1,                /* Dual books in effect?      */~
            errormsg$79,                 /* ERROR MESSAGE FOR DISPLAY  */~
            firstacct$(15)3,             /* FIRST JOURNAL              */~
            from$(15)3,                  /* FROM Last Journal          */~
            i$(24)80,                    /* Screen Image               */~
            jnlid$3,                     /* JOURNAL ID                 */~
            lastacct$(15)3,              /* LAST Journal               */~
            line4$79, line5$79,                                          ~
            look%(1),                    /* SEARCH TO VARIBLE          */~
            modno$2,                     /* MODULE NUMBER              */~
            months$(12)9,                /* MONTHS LIST AVAILABLE      */~
            monthe$3,                    /* MONTH END?                 */~
            mdate$(3)12,                 /* DATES OF AVAILABLE MONTHS  */~
            modules$7,                                                   ~
            periodsopen$12,                                              ~
            prttotaldescr$40,            /* SUMMARY LINE DESCRIPTION   */~
            oldreadkey$50,               /* OLD KEY FOR PLOW ROUTINES  */~
            rangeofjournals$17,                                          ~
            ref1$30,                     /* Reference 1 desc           */~
            ref2$30,                     /* Reference 2 desc           */~
            rptid$6,                     /* Report ID                  */~
            set$1, setdescr$30, sethdr$60,/* Set of books to use       */~
            setfac$1,                    /* FAC for SET$               */~
            setmsg1$18, setmsg2$40,      /* Screen message for SET     */~
            sortflag$(4)1,               /* SORT INDICATORS            */~
            sum$3,                       /* SUMMARY PRINTING ONLY      */~
            temp$8,                      /*                            */~
            tasktype$1,                  /* FOREGROUND OR BACKGROUND   */~
	    tdate$8,                     /* Temp. Date                 */~
            title$40,                    /* JOURNAL TITLE              */~
            time$8,                      /* System Time                */~
            to$(15)3,                    /* TO Journal                 */~
            transtypes$(25)2,            /* TRANSACTION TYPES          */~
            transhead$(25)30,            /* TRANSACTION HEADINGS       */~
	    udate$8,                     /* Unformated Date            */~
            userid$3,                    /* USER ID CURRENT USER       */~
            version$79,                  /* DISPLAY PROGRAM ID         */~
            wjnlid$3,                    /* JOURNAL ID                 */~
            wmodno$2,                    /* MODULE NUMBER              */~
            zetype$2                     /* JOURNAL TYPE THIS RUN      */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            fs%(64)                      /* NEW FILE STATUS            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                    S E L E C T   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #01 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #02 ! GLDETAIL ! GENERAL LEDGER DETAIL FILE               *~
            * #03 ! SYSFILE2 ! SYSTEM INFORMATION (WHICH MONTHS OPEN)   *~
            * #04 ! WORKFILE ! SORT WORK FILE FOR DETAIL TO PRINT       *~
            * #11 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #12 ! GLDETAL2 ! G. L. detail records for local authority *~
            *************************************************************

            select #01, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #02, "GLDETAIL",     /* GENERAL LEDGER DETAILS     */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select  #03, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #04, "WORKFILE",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 39,                                    ~
                        keypos = 1, keylen = 13

            select #11, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #12, "GLDETAL2",                                      ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos = 1,    keylen = 26

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENCHCK" (#01%, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#02%, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#03%, fs%( 3), f2%( 3), 0%, rslt$( 3))
            if fs%(2) <= 0% then L02425
                get rslt$(2) using L02423, record%
L02423:              FMT XX(16), BI(4)
L02425:     dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#03, "SWITCHS.GL", f1%(3))
                if f1%(3) = 0% then goto L09000
            get #03 using L02445, dual_books$
L02445:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#11%, fs%(11), f2%(11), 0%, rslt$(11))
                call "OPENCHCK" (#12%, fs%(12), f2%(12), 0%, rslt$(12))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES CONTROL VARIABLES, SETS AVAILABLE DATES, ETC. *~
            *************************************************************

            setfac$ = hex(9c)
            if dual_books$ <> "Y" then goto L09110
                setmsg1$ = "G/L System to use:"
                setmsg2$ = "('1' = Statutory; '2' = Local Authority)"
                setfac$ = hex(82)
L09110:     if dual_books$ = "Y" then lo% = 1% else lo% = 2%
            line4$ = "Enter: a character beside desired Periods and Mod"&~
                "ules; Journal Ranges as ap-"
            if dual_books$ <> "Y"                                        ~
                then line5$ = "plicable; 'YES', 'NO' or 'SEQ' for summa"&~
                     "ry."                                               ~
                else line5$ = "plicable; G/L System code you desire; 'Y"&~
                     "ES', 'NO' or 'SEQ' for summary."
            call "EXTRACT" addr ("ID", userid$, "TT", tasktype$)
            date$ = date : call "DATEFMT" (date$)
            call "COMPNAME" (12%, compname$, 0%)
            str(version$,62) = "GLJURN98: " & str(cms2v$,1,8)
            periodsopen$ = "Periods Open"
            modules$ = "Modules"
            rangeofjournals$ = "Range of Journals"
            rptid$ = "G/L002"

            months$( 1)="Jan" : months$( 2)="Feb" : months$( 3)="Mar"
            months$( 4)="Apr" : months$( 5)="May" : months$( 6)="Jun"
            months$( 7)="Jul" : months$( 8)="Aug" : months$( 9)="Sep"
            months$(10)="Oct" : months$(11)="Nov" : months$(12)="Dec"
            call "READ100" (#03, "FISCAL DATES", f1%(3))
                if f1%(3) <> 0 then L09410
L09340:     err% = 0%
L09350:     call "ASKUSER" (err%, "ERROR ON SETTING DATE RANGE", "Revie"&~
                "w your fiscal date structure", "Press PF-16 to Acknowl"&~
                "edge and Exit Program", " ")
            if err% <> 16% then L09340
            goto L65000

L09410: REM Continue here
            get #03, using L09430, periods%, monthopen%, dates$()
L09430:         FMT XX(20), BI(2), XX(136), BI(2), 32*CH(8)
            mdate$(1) = dates$(monthopen%+16)
            if monthopen% = 12 then mdate$(1) = dates$(41-periods%)
            if monthopen% = 17 then mdate$(1) = " "
            mdate$(2) = dates$(monthopen%+15)
            mdate$(3) = dates$(monthopen%+14)
            if monthopen% = 1 then mdate$(3) = dates$(periods%+1)
            if monthopen% = 14% then mdate$(3%) = dates$(periods%+15%)
            temp$ = dates$(monthopen%+13%)
            if monthopen% = 1% then temp$ = dates$(periods%)
            if monthopen% = 2% then temp$ = dates$(periods%+1%)
            if monthopen% = 14% then temp$ = dates$(periods%+14%)
            if monthopen% = 15% then temp$ = dates$(periods%+15%)
            day1$(3%) = temp$
            for temp% = 1% to 3%
                day2$(temp%) = mdate$(temp%)
                if temp% < 3% then day1$(temp%) = mdate$(temp%+1%)
		tdate$ = mdate$(temp%)
		call "DATEFMT" (tdate$, tdate%, udate$)
                convert str(udate$,5%,2%)to month%,data goto L09350
		tdate$ = day2$(temp%)
		call "DATEFMT" (tdate$, tdate%, udate$)
                mdate$(temp%) = months$(month%) & " " &                  ~
                    str(udate$,7%,2%) & ", " & str(udate$,1%,4%)
                call "DATE" addr ("G+",day1$(temp%),1%,day1$(temp%),u3%)
                if u3% <> 0% then L09350
            next temp%

        REM SET TRANSACTION TYPES
            i% = 0%
            init(hex(ff)) transtypes$()
            oldreadkey$ = "MODULENO:0000"
L09710:     call "PLOWNEXT" (#03, oldreadkey$, 11%, f1%(3))
                if f1%(3) = 0 then L09780
            i% = i% + 1 : if i% > 11 then L09780
            call "DESCRIBE" (#03, oldreadkey$, transhead$(i%), 0%, f1%(3))
            transtypes$(i%) = str(oldreadkey$,12,2)
            goto L09710

L09780: REM DO A GETPARM TO FIND IF WE ARE AT MONTH END
            call "GETPARM" addr ("ID", "S", "MONTHEND", "@", "0001",     ~
                "GLJN98", "Are We at Month End?", 20%, "K", "MONTHEND",  ~
                monthe$, 3%, 5%, 32%, "A")
            if monthe$ <> "YES" then L10000
                init(" ") chomon$(), chojrn$()
                chomon$(2) = "X"
                str(chojrn$(), 1, 10) = "XXXXXXXX"
                sortflag$() = " XX "
                sum$ = "NO"
                for temp% = 1 to 15             /* SET ACCT RANGES  */
                    firstacct$(temp%) = all(hex(00))
                    lastacct$(temp%)  = all(hex(7f))
                next temp%
                set$ = "1" : set = 1
                gosub gl_set_setup
                go to L10400

L10000: REM *************************************************************~
            *         I N P U T   W H I C H   M O N T H / JOURNAL       *~
            *                                                           *~
            * CONTROLING SECTION FOR WHICH MONTH AND JOURNALS TO PRINT  *~
            *************************************************************

            if monthe$ = "YES" then L65000
            sum$ = "NO "
            init(" ") errormsg$, set$, setdescr$, sethdr$
L10090: REM GET MONTHS AND JOURNALS, etc. TO PRINT
            init(" ") chomon$(), chojrn$(), to$()
            for temp% = 1 to 15                /* SET ACCT RANGES  */
                from$(temp%) = "ALL"
            next temp%
            bigdebits, bigcredits = 0
L10150:     gosub L40000
                  if keyhit%  =   1 then gosub startover
                  if keyhit%  =  16 then       L65000
                  if keyhit% <>   0 then       L10150
            gosub L50000
                  if errormsg$ <> " " then L10150

L10220:     gosub'211(fieldnr%)
                if keyhit%  =   1 then gosub startover
                if keyhit%  =  16 then       L10400
                if keyhit% <>   0 then       L10220

L10270:     gosub'221(fieldnr%)
                if keyhit%  =   1 then gosub startover
                if keyhit% <>   0 then       L10270
            gosub L50000
                if errormsg$ <> " " then L10270
            goto L10220

L10400: REM RESET VARIABLES
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            zemonth%, zejrn% = 0
            zetype$ = " "
            printline% = 100
            pageno%, headflag% = 0

L11000: REM *************************************************************~
            *               C O N T R O L   S E C T I O N               *~
            *                                                           *~
            * CONTROLLER SECTION FOR PRINTING, AND TOTALLING            *~
            *************************************************************

        REM INCREMENT MONTH
            zemonth% = zemonth% + 1
            if zemonth% <= 3 then L11090
                call "SETPRNT" (rptid$, " ", 0%, 1%)
                close printer
                goto L10000
L11090:     if chomon$(zemonth%) = " " then L11000
            zejrn%, totaldebits, totalcredits = 0
            gosub L14000

            goto  L11000

L14000: REM *************************************************************~
            *S O R T  A N D  P R I N T  A  R A N G E  O F  J O U R N A L*~
            *                                                           *~
            *OPENS A SORT FILE FOR A PERIOD, SORTS,AND THEN PRINTS      *~
            *************************************************************

            call "WORKOPEN" (#04, "IO", max(100%, (record%/3%)), f2%(4%))
                if f2%(4%) <> 0 then L16000
            oldreadkey$ = all(hex(00))
            headflag%, wseq% = 0
            pageflag% = 1%
            call "SHOSTAT" ("Sorting G/L Detail for Period Ending " &    ~
                            mdate$(zemonth%))

        REM DO PLOW ROUTINE, GATHERING APPROPRIATE INFORMATION
L14140:     call "READ102" (#main%, oldreadkey$, f1%(main%))
                if f1%(main%) = 0 then L14490
            get #main%, using L14170, account$
L14170:         FMT CH(9)
            init (hex(00)) oldreadkey$
            str(oldreadkey$,,22)=str(account$,1,16) & day1$(zemonth%)
L14210:     gosub L30000
            if f1%(detl%) = 0 then L14140
            if prtposted$ > day2$(zemonth%)    then L14140
            search transtypes$() = modno$ to look%() step 2
            if look%(1) = 0                    then L14210
            modno% = (look%(1) + 1)/2
            if chojrn$(modno%)   = " "         then L14210
            if jnlid$ < firstacct$(modno%)     then L14210
            if jnlid$ > lastacct$(modno%)      then L14210

        REM NOW SAVE THE POINTER IN THE WORKFILE
            write #04, using L14400, modno$, jnlid$, pstseq%, wseq%,      ~
                account$, prtposted$, seq%, eod goto L16000
L14400:     FMT CH(2),CH(3),BI(4),BI(4),CH(16),CH(6), BI(4)
            wseq% = wseq% + 1
            goto L14210

L14490: REM NOW PRINT THE WORKFILE
            call "SHOSTAT" (" Printing Journals For Period Ending "      ~
                & mdate$(zemonth%))
            init (hex(00)) newreadkey$
            init (hex(20)) modno$, jnlid$
            pstseq% = -2
            call "READ102" (#04, newreadkey$, f1%(4))
            goto L14580
L14570:     call "READNEXT" (#04, f1%(4))
L14580:         if f1%(4) = 0 then L15250
            get #04 using L14400, wmodno$, wjnlid$, wpstseq%, wseq%,      ~
                account$, prtposted$, seq%
            if wmodno$ <> modno$ then L15000
            if sortflag$(3) <> " " and wjnlid$ <> jnlid$ then L14950
            if sortflag$(4) <> " " and wpstseq% <> pstseq% then L14910
L14640:     put oldreadkey$, using L14650, account$, prtposted$, seq%
L14650:         FMT CH(16), CH(6), BI(4)
            gosub L30050
            if f1%(detl%) = 0 then L16000
            if headflag% = 0 then gosub sub_heading
            headflag% = 1
            gosub L14730
            goto L14570

L14730: REM PRINT DETAIL LINE
            call "DATEFMT" (prtposted$)
            call "CONVERT" (debit, 2.2, debit$)
            call "CONVERT" (credit, 2.2, credit$)
            if debit = 0  then debit$ = " "
            if credit = 0 then credit$ = " "
            totaldebits = totaldebits + debit
            totalcredits = totalcredits + credit
            if sum$ = "YES" then L14870
            if printline% + 1% > 59% then gosub heading
            printline% = printline% + 1%
            if set = 1                                                   ~
                then call "GLFMT" (account$)                             ~
                else call "GLFMT2" (account$)
            print using L55520, account$, prtposted$, modno$, jnlid$,     ~
                ref1$, ref2$, descr$, debit$, credit$
            pageflag% = 0%
L14870:     return

L14910: REM SUBTOTAL SECOND SUBTOTAL
            gosub L15310
            goto L14640

L14950: REM SUBTOTAL FIRST SUBTOTAL
            gosub L15310
            gosub L15395
            goto L14640

L15000: REM SUBTOTAL MODULE/ACCOUNT
            gosub L15050
            headflag% = 0
            goto L14640

L15050: REM ACTUAL TOTAL ROUTINE FOR MODULE
            gosub L15310
            gosub L15395
            if sortdebits = 0 and sortcredits = 0 then return
            if printline% + 5 > 59% then gosub heading
            printline% = printline% + 5
            prttotaldescr$ = "** MODULE "&modno$&" TOTAL **"
            if str(sortflag$(),3,2) <> " " then                          ~
                gosub'80(sortdebits, sortcredits)                        ~
                else gosub'60(sortdebits, sortcredits)
            bigdebits = bigdebits + sortdebits
            bigcredits = bigcredits + sortcredits
            totaldebits, totalcredits, sub1debits, sub1credits,          ~
                sortdebits, sortcredits = 0
            return

L15250: REM TOTAL FOR REPORT
            gosub L15050
            prttotaldescr$ = "** GRAND TOTALS **"
            if printline% + 4% > 59% then gosub heading
            printline% = printline% + 4%
            gosub'80(bigdebits, bigcredits)
            if printline% + 2% > 59% then gosub heading
            print skip ( 1)
            time$ = " "  :  call "TIME" (time$)
            print using L55680, time$     /* End Report Message */
            call "FILEBGON" (#04)
            printline% = 100%
            f2%(4%) = 1
            return

L15310: REM SUBTOTAL FOR SUBTOTAL2
            if totaldebits = 0 and totalcredits = 0 then L15385
            if sortflag$(4) = " " then L15365
            prttotaldescr$ = "** POSTING TOTAL **"
            if printline% + 3% > 59% then gosub heading
            printline% = printline% + 3%
            gosub'60(totaldebits,totalcredits)
            if sortflag$(1) <> " " then L15365
            if sortflag$(4) <> " " then headflag% = 0
L15365:     sub1debits = sub1debits + totaldebits
            sub1credits= sub1credits + totalcredits
            totaldebits, totalcredits = 0
L15385:     return

L15395: REM SUBTOTAL FOR SUBTOTAL1
             if sub1debits = 0 and sub1credits = 0 then L15485
             if sortflag$(3) = " " then L15465

             prttotaldescr$ = "** JOURNAL "&jnlid$&" TOTAL **"
             if printline% + 3% > 59% then gosub heading
             printline% = printline% + 3%
             if sortflag$(4) <> " " then                                 ~
                   gosub'80(sub1debits,sub1credits)                      ~
                else gosub'60(sub1debits,sub1credits)
             if sortflag$(3) <> " " then headflag% = 0
L15465:      sortdebits = sortdebits + sub1debits
             sortcredits= sortcredits + sub1credits
             sub1debits, sub1credits = 0
L15485:      return


        deffn'60(subdebits,subcredits)
            print using L55460
            print using L55550, prttotaldescr$, subdebits, subcredits
            print using L55430
            pageflag% = 1%
            return

        deffn'70(subdebits,subcredits)
            subtotal = subdebits - subcredits
            if subtotal < 0 then L15780
            prtdebits = subtotal : prtcredits = 0
            goto L15790
L15780:     prtdebits = 0 : prtcredits = -1 * subtotal
L15790:     convert prtdebits  to prtdebits$, pic(-#########.##)
            convert prtcredits to prtcredits$, pic(-#########.##)
            if prtdebits = 0 and prtcredits = 0 then L15840
            if prtdebits = 0 then prtdebits$ = " "
            if prtcredits = 0 then prtcredits$ = " "
L15840:     if prtdebits = 0 and prtcredits = 0 then prtcredits$ = " "
            print skip(1)
            print using L55580, prttotaldescr$, prtdebits$, prtcredits$
            print skip(1)
            pageflag% = 1%
            return

        deffn'80(bigprtdebits, bigprtcredits)
            if bigprtdebits = 0 and bigprtcredits = 0 then return
            print using L55460
            print using L55550, prttotaldescr$,bigprtdebits,bigprtcredits
            print using L55460
            print using L55430
            pageflag% = 1%
            return

L16000: REM ERROR ROUTINE FOR NO WORKFILE OR WORKFILE FULL
            errormsg$ = "Workfile Not Opened or Workfile Full/Corrupted"
            call "FILEBGON" (#04)
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            f2%(4%) = 1
            goto L10090

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return
            return clear all
            goto L10000

L30000: REM *************************************************************~
            *        D E T A I L  P L O W  R O U T I N E                *~
            *************************************************************

            test% = 16%          /* TEST FOR NORMAL PLOW       */
            goto L30080
L30050:     call "READ100"  (#detl%, oldreadkey$, f1%(detl%))
            goto L30090
L30080:     call "PLOWNEXT" (#detl%, oldreadkey$, test%, f1%(detl%))
L30090:     if f1%(detl%) = 0 then return
            get #detl%, using L30130, prtposted$, seq%, modno$, debit,    ~
                credit, ref1$, ref2$, descr$, jnlid$, pstseq%

L30130:             FMT XX(16),                    /* SKIP ACCOUNT     */~
                        CH(6),                     /* MODULE DATE POSTD*/~
                        BI(4),                     /* SKIP SEQ#        */~
                        CH(2),                     /* TYPE CODE INFO   */~
                        2*PD(14,4),                /* DEBITS, CREDITS  */~
                        CH(30),                    /* REF TEXT STRING  */~
                        CH(34),                    /* REF 2 TEXT       */~
                        CH(36),                    /* DESCRIPTION      */~
                        CH(3),                     /* JOURNAL ID       */~
                        BI(4),                     /* SEQUENCE POST NUM*/~
                        XX(3),                     /* USERID OF CREATOR*/~
                        XX(6)                      /* SYSTEM DATE      */

            return

L40000: REM *************************************************************~
            *        I N P U T   C H O I C E   O F  MONTHS/JOURNALS     *~
            *                                                           *~
            * INDICATE WHICH MONTHS AND JOURNALS YOU DESIRE             *~
            *************************************************************

        deffn'201(fieldnr%)
            pfmessage$ = "(16)EXIT PROGRAM"
            goto L40140

        deffn'221(fieldnr%)
            pfmessage$ = " "
L40140:     blankline$ = " "
L40160:     accept                                                       ~
               at (01,02), "Print Monthly Journals by Module & Journal", ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), version$               , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(8c)), line4$                 , ch(79),~
               at (05,02), fac(hex(8c)), line5$                 , ch(79),~
                                                                         ~
               at (07,04), fac(hex(ac)), periodsopen$           , ch(12),~
               at (11,03), "(Ending Dates)",                             ~
               at (08,02), fac(hex(81)), chomon$(1)             , ch(01),~
               at (09,02), fac(hex(81)), chomon$(2)             , ch(01),~
               at (10,02), fac(hex(81)), chomon$(3)             , ch(01),~
               at (08,04), fac(hex(8c)), mdate$(1)              , ch(12),~
               at (09,04), fac(hex(8c)), mdate$(2)              , ch(12),~
               at (10,04), fac(hex(8c)), mdate$(3)              , ch(12),~
                                                                         ~
               at (18,02), fac(hex(8c)), setmsg1$               , ch(18),~
               at (18,21), fac(setfac$), set$                   , ch(01),~
               at (18,25), fac(hex(8c)), setmsg2$               , ch(40),~
                                                                         ~
               at (19,06), "Summary only?:",                             ~
               at (19,21), fac(hex(81)), sum$                   , ch(03),~
               at (19,25), "(Enter YES, NO, or SEQ)",                    ~
                                                                         ~
               at (07,33), fac(hex(ac)), modules$               , ch(07),~
               at (08,22), fac(hex(81)), chojrn$( 1)            , ch(01),~
               at (09,22), fac(hex(81)), chojrn$( 2)            , ch(01),~
               at (10,22), fac(hex(81)), chojrn$( 3)            , ch(01),~
               at (11,22), fac(hex(81)), chojrn$( 4)            , ch(01),~
               at (12,22), fac(hex(81)), chojrn$( 5)            , ch(01),~
               at (13,22), fac(hex(81)), chojrn$( 6)            , ch(01),~
               at (14,22), fac(hex(81)), chojrn$( 7)            , ch(01),~
               at (15,22), fac(hex(81)), chojrn$( 8)            , ch(01),~
               at (08,24), fac(hex(8c)), transhead$( 1)         , ch(30),~
               at (09,24), fac(hex(8c)), transhead$( 2)         , ch(30),~
               at (10,24), fac(hex(8c)), transhead$( 3)         , ch(30),~
               at (11,24), fac(hex(8c)), transhead$( 4)         , ch(30),~
               at (12,24), fac(hex(8c)), transhead$( 5)         , ch(30),~
               at (13,24), fac(hex(8c)), transhead$( 6)         , ch(30),~
               at (14,24), fac(hex(8c)), transhead$( 7)         , ch(30),~
               at (15,24), fac(hex(8c)), transhead$( 8)         , ch(30),~
                                                                         ~
               at (07,59), fac(hex(ac)), rangeofjournals$       , ch(17),~
               at (08,60), fac(hex(81)), from$( 1)              , ch(03),~
               at (09,60), fac(hex(81)), from$( 2)              , ch(03),~
               at (10,60), fac(hex(81)), from$( 3)              , ch(03),~
               at (11,60), fac(hex(81)), from$( 4)              , ch(03),~
               at (12,60), fac(hex(81)), from$( 5)              , ch(03),~
               at (13,60), fac(hex(81)), from$( 6)              , ch(03),~
               at (14,60), fac(hex(81)), from$( 7)              , ch(03),~
               at (15,60), fac(hex(81)), from$( 8)              , ch(03),~
                                                                         ~
               at (08,66), "TO",                                         ~
               at (09,66), "TO",                                         ~
               at (10,66), "TO",                                         ~
               at (11,66), "TO",                                         ~
               at (12,66), "TO",                                         ~
               at (13,66), "TO",                                         ~
               at (14,66), "TO",                                         ~
               at (15,66), "TO",                                         ~
                                                                         ~
               at (08,71), fac(hex(81)),   to$( 1)              , ch(03),~
               at (09,71), fac(hex(81)),   to$( 2)              , ch(03),~
               at (10,71), fac(hex(81)),   to$( 3)              , ch(03),~
               at (11,71), fac(hex(81)),   to$( 4)              , ch(03),~
               at (12,71), fac(hex(81)),   to$( 5)              , ch(03),~
               at (13,71), fac(hex(81)),   to$( 6)              , ch(03),~
               at (14,71), fac(hex(81)),   to$( 7)              , ch(03),~
               at (15,71), fac(hex(81)),   to$( 8)              , ch(03),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (24,65), fac(hex(8c)),  pfmessage$             ,ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41050
                  call "MANUAL" ("GLJURN98")
                  goto L40160

L41050:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40160

        REM *************************************************************~
            *        I N P U T   C H O I C E   O F  MONTHS/JOURNALS     *~
            *                                                           *~
            * INDICATE WHICH MONTHS AND JOURNALS YOU DESIRE             *~
            *************************************************************

            deffn'211(fieldnr%)
            blankline$ = "Press RETURN to Edit Selection"
L42080:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Monthly Journals by Module & Journal",          ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), version$               , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(8c)), line4$                 , ch(79),~
               at (05,02), fac(hex(8c)), line5$                 , ch(79),~
                                                                         ~
               at (07,04), fac(hex(ac)), periodsopen$           , ch(12),~
               at (11,03), "(Ending Dates)",                             ~
               at (08,02), fac(hex(84)), chomon$(1)             , ch(01),~
               at (09,02), fac(hex(84)), chomon$(2)             , ch(01),~
               at (10,02), fac(hex(84)), chomon$(3)             , ch(01),~
               at (08,04), fac(hex(8c)), mdate$(1)              , ch(12),~
               at (09,04), fac(hex(8c)), mdate$(2)              , ch(12),~
               at (10,04), fac(hex(8c)), mdate$(3)              , ch(12),~
                                                                         ~
               at (18,02), fac(hex(8c)), setmsg1$               , ch(18),~
               at (18,21), fac(hex(84)), set$                   , ch(01),~
               at (18,25), fac(hex(8c)), setmsg2$               , ch(40),~
                                                                         ~
               at (19,06), "Summary only?:",                             ~
               at (19,21), fac(hex(8c)), sum$                   , ch(03),~
               at (19,25), "(Enter YES, NO, or SEQ)",                    ~
                                                                         ~
               at (07,33), fac(hex(ac)), modules$               , ch(07),~
               at (08,22), fac(hex(84)), chojrn$( 1)            , ch(01),~
               at (09,22), fac(hex(84)), chojrn$( 2)            , ch(01),~
               at (10,22), fac(hex(84)), chojrn$( 3)            , ch(01),~
               at (11,22), fac(hex(84)), chojrn$( 4)            , ch(01),~
               at (12,22), fac(hex(84)), chojrn$( 5)            , ch(01),~
               at (13,22), fac(hex(84)), chojrn$( 6)            , ch(01),~
               at (14,22), fac(hex(84)), chojrn$( 7)            , ch(01),~
               at (15,22), fac(hex(84)), chojrn$( 8)            , ch(01),~
               at (08,24), fac(hex(8c)), transhead$( 1)         , ch(30),~
               at (09,24), fac(hex(8c)), transhead$( 2)         , ch(30),~
               at (10,24), fac(hex(8c)), transhead$( 3)         , ch(30),~
               at (11,24), fac(hex(8c)), transhead$( 4)         , ch(30),~
               at (12,24), fac(hex(8c)), transhead$( 5)         , ch(30),~
               at (13,24), fac(hex(8c)), transhead$( 6)         , ch(30),~
               at (14,24), fac(hex(8c)), transhead$( 7)         , ch(30),~
               at (15,24), fac(hex(8c)), transhead$( 8)         , ch(30),~
                                                                         ~
               at (07,59), fac(hex(ac)), rangeofjournals$       , ch(17),~
               at (08,60), fac(hex(84)), from$( 1)              , ch(03),~
               at (09,60), fac(hex(84)), from$( 2)              , ch(03),~
               at (10,60), fac(hex(84)), from$( 3)              , ch(03),~
               at (11,60), fac(hex(84)), from$( 4)              , ch(03),~
               at (12,60), fac(hex(84)), from$( 5)              , ch(03),~
               at (13,60), fac(hex(84)), from$( 6)              , ch(03),~
               at (14,60), fac(hex(84)), from$( 7)              , ch(03),~
               at (15,60), fac(hex(84)), from$( 8)              , ch(03),~
                                                                         ~
               at (08,66), "TO",                                         ~
               at (09,66), "TO",                                         ~
               at (10,66), "TO",                                         ~
               at (11,66), "TO",                                         ~
               at (12,66), "TO",                                         ~
               at (13,66), "TO",                                         ~
               at (14,66), "TO",                                         ~
               at (15,66), "TO",                                         ~
                                                                         ~
               at (08,71), fac(hex(84)),   to$( 1)              , ch(03),~
               at (09,71), fac(hex(84)),   to$( 2)              , ch(03),~
               at (10,71), fac(hex(84)),   to$( 3)              , ch(03),~
               at (11,71), fac(hex(84)),   to$( 4)              , ch(03),~
               at (12,71), fac(hex(84)),   to$( 5)              , ch(03),~
               at (13,71), fac(hex(84)),   to$( 6)              , ch(03),~
               at (14,71), fac(hex(84)),   to$( 7)              , ch(03),~
               at (15,71), fac(hex(84)),   to$( 8)              , ch(03),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (24,65),                                               ~
                  "(16)PRINT REPORT",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L43020
                  call "MANUAL" ("GLJURN98")
                  goto L42080

L43020:        if keyhit% <> 15 then goto L43060
                  call "PRNTSCRN"
                  goto L42080

L43060:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L50000: REM *************************************************************~
            *        T E S T   D A T A   E N T E R E D                  *~
            *                                                           *~
            * TEST CHOICE OF MONTHS AND JOURNALS                        *~
            *************************************************************

            errormsg$ = " "
            REM TEST FOR NON BLANK
                temp = pos(str(chomon$(), 1) <> " ")
                if temp <> 0 then L50120
                   errormsg$ = "Please choose at least one month"
                   return
L50120:         temp = pos(str(chojrn$(), 1) <> " ")
                if temp <> 0 then L50170
                   errormsg$ = "Please Choose at Least One Module"
                   return

L50170: REM Test to see if dual books is in effect and which set to use
                set = 1 : main% = 1% : detl% = 2%     /* Set defaults */
                if dual_books$ <> "Y" then goto L50310
                call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
                if errormsg$ = " " then goto L50220
                    errormsg$ = "G/L System code must be '1'(Statutory)"&~
                        " or '2'(Local Authority)"
                    return
L50220:         gosub gl_set_setup

L50310:         if sum$ = "YES" then L50370
                if sum$ = "NO"  then L50370
                if sum$ = "SEQ" then L50370
                   errormsg$ = "Answer 'YES', 'NO' or 'SEQ' for Summary P~
        ~rinting"
                return
L50370:         if sum$ = "SEQ" then sortflag$() = " XXX"                ~
                   else sortflag$() = " XX "

        REM *************************************************************~
            *             R A N G E   V A L I D A T I O N               *~
            * THE PURPOSE OF THIS ROUTINE IS TO NORMALIZE THE KEYS FOR  *~
            * THE RANGE AND ERROR IF LAST < FIRST.                      *~
            *************************************************************

             errormsg$ = " "
             for temp% = 1 to 15%
                firstacct$(temp%) = from$(temp%)
                lastacct$(temp%)  = to$(temp%)
                REM HANDLES CASE FOR "ALL" JOURNALS
                    if firstacct$(temp%) <> "ALL" then L50550
                       init(hex(00)) firstacct$(temp%)
                       init(hex(ff)) lastacct$(temp%)
                       goto L50680
L50550:         REM HANDLES CASE FOR SINGLE JOURNALS
                    if lastacct$(temp%) <> " " then L50590
                       lastacct$(temp%) = firstacct$(temp%)

L50590:         REM HANDLES CASE FOR A RANGE OF JOURNALS
                    if lastacct$(temp%) < firstacct$(temp%) then L50640
                       firstacct$(temp%) = firstacct$(temp%) addc        ~
                                           all(hex(ff))
                       goto L50680
L50640:         REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                    errormsg$ = "Invalid Range for " & transhead$(temp%) ~
                              & " Module.  Please Respecify"
                    return
L50680:      next temp%
             return

        gl_set_setup
            setdescr$ = "Statutory"
            main% = 1% : detl% = 2%
            if set = 1 then goto L51060
                setdescr$ = "Local Authority"
                main% = 11% : detl% = 12%
L51060:     sethdr$ = setdescr$
            call "FMTTITLE" (sethdr$, "G/L SYSTEM", 12%)
            call "PUTPAREN" (setdescr$)
            return

        REM *************************************************************~
            * P A G E   H E A D E R   C O N T R O L L E R               *~
            *                                                           *~
            * PAGE HEADDER CONTROLLER ROUTINE                           *~
            *************************************************************

        heading
            select printer (134)
            if pageno% <> 0% then goto L55120
                time$ = " " : call "TIME" (time$)
                call "SETPRNT" (rptid$, " ", 0%, 0%)
L55096:         i% = pos(str(i$()) > hex(7f))
                if i% = 0% then L55101
                    str(i$(), i%, 1%) = hex(20)
                    goto L55096
L55101:         gosub L55130    /* Header */
                print skip (2)
                print using L55361, "R E P O R T   S E L E C T I O N S:"
                print
                for x% = 4% to 20%
                    print using L55361, i$(x%)
                next x%
L55120:     if pageflag% = 0% then print using L55430
L55130:     print page
            print using L55310, date$, time$, compname$, "-" & rptid$
            print using L55340, pageno%
            print using L55331, sethdr$
            print using L55370, mdate$(zemonth%)
            print
            if pageno% = 0% then L55280
                search transtypes$() = modno$ to look%() step 2
                if look%(1) = 0% then goto L55240
                    modno% = (look%(1) + 1)/2
                    if from$(modno%) = "ALL"                             ~
                        then print using L55390, modno$                   ~
                        else print using L55410, from$(modno%),           ~
                            to$(modno%), modno$
L55240:         print using L55430
                print using L56030
                print using L56060
                printline% = 9
L55280:     pageno% = pageno% + 1
            return

L55310: %RUN: ######## @ ########            ############################~
        ~################################                     GLJURN98####~
        ~###
L55331: %                                    ############################~
        ~################################
L55340: %                                           M O N T H L Y   J O U~
        ~ R N A L   L I S T I N G                                  PAGE: #~
        ~###
L55361: %                          ######################################~
        ~##########################################
L55370: %                                                    PERIOD ENDIN~
        ~G ################
L55390: % ALL JOURNALS FOR MODULE ##

L55410: % JOURNALS ### TO ### FOR MODULE ##

L55430: %================================================================~
        ~=================================================================~
        ~===
L55460: %                                                                ~
        ~                                                                 ~

L55490: %   ACCOUNT CODE: ##########  DESCRIPTION: ######################~
        ~##############                                                   ~

L55520: % ######### ######## ## ### ######################### ###########~
        ~############## ####################### ############# ############~
        ~#
L55550: %  ########################################                      ~
        ~                                       -#########.## -#########.#~
        ~#
L55580: %  ########################################                      ~
        ~                                       ############# ############~
        ~#
L55610: %  MODULE NUMBER: ##  JOURNAL ID: ###  POSTING SEQUENCE: ########~
        ~##  TITLE: ########################################              ~

L55640: %  MODULE NUMBER: ##  JOURNAL ID: ###  TITLE: ###################~
        ~#####################                                            ~

L55670: %  MODULE NUMBER: ##
L55680:       %                                       - - -  E N D  O F  ~
        ~R E P O R T  @  ########  - - -

        sub_heading
            call "JNLINFO" (wmodno$, wjnlid$, 0%, " ", title$, " ",      ~
                            #3, f2%(3), 1%)
            call "DESCRIBE" (#main%,account$,description$,0%,f1%(main%))
            if printline% + 4% > 59 then gosub heading
            printline% = printline% + 4%
            print using L55460
            if sortflag$(1) <> " " then L55900
            sortflag% = pos(-sortflag$()<>(hex(20)))
            on sortflag% gosub L55900, L55860, L55840, L55820
            goto L55910
L55820:     print using L55610, wmodno$, wjnlid$, wpstseq%, title$
            return
L55840:     print using L55640, wmodno$, wjnlid$, title$
            return
L55860:     print using L55670, wmodno$
            return
            print using L55490, account$, description$
            return
L55900:     print using L55490, account$, description$
L55910:     print using L55460
            print using L55950
            return

L55950: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+
L56030: % ACCOUNT   DATE     MD JNL REFERENCE1                REFERENCE 2~
        ~               DESCRIPTION                     DEBIT         CRED~
        ~IT
L56060: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#04)
            end
