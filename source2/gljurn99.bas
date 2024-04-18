        REM *************************************************************~
            *                                                           *~
            *   GGG   L      JJJJJ  U   U  RRRR   N   N   999    999    *~
            *  G      L        J    U   U  R   R  NN  N  9   9  9   9   *~
            *  G GGG  L        J    U   U  RRRR   N N N   9999   9999   *~
            *  G   G  L      J J    U   U  R   R  N  NN      9      9   *~
            *   GGG   LLLLL   J      UUU   R   R  N   N   999    999    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLJURN99 - PRINTS A MONTHLY MODULE RECAP FOR ANY OF THE   *~
            *            TEN MODULES, FOR ANY OF THE THREE MONTHS OPEN  *~
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
            * 06/23/81 ! ORIGINAL                                 ! TEM *~
            * 08/04/81 ! MONTH END PROCESSING                     ! TEM *~
            * 11/08/82 ! RANGE PROCESSING FOR ACCOUNTS, CLEANUP   ! ECR *~
            * 08/13/85 ! EXTENDED ACCT #, GLDETAIL REFORMAT       ! HES *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 09/16/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/10/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 11/08/88 ! Prints for both sets of books if 'dual   ! JDH *~
            *          !  books' flag is yes and run from PROCE7. !     *~
            * 05/06/89 ! Chg to re-print acct. # on page break    ! MLJ *~
            *          !  during detail printing - ( Prod Merge)  !     *~
            * 11/17/89 ! Keep printing next months data, just skip! JDH *~
            *          !  GL Year End if printed before.          !     *~
            * 09/26/91 ! PRR 11350 Reformat printout to incl REF2.! JIM *~
            * 01/12/93 ! Page 0 Facs fix,     & End Report Time.  ! RJH *~
            * 03/01/93 ! PRR 12800 - Changes lines 11424 & 11426  ! MLJ *~
            *          !   to format PRTACCOUNT$ (was ACCOUNT$).  !     *~
	    * 06/24/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            accttype$1,                  /* GL ACCOUNT TYPE            */~
            aid$1,                       /* AID CHARACTER IN GETPARM   */~
            blankline$79,                /* LINE UNDERLINED IN SCREEN  */~
            chojrn$(11)1,                /* WHICH MODULES TO PRINT     */~
            chomon$(11)1,                /* WHICH MONTHS TO PRINT      */~
            compname$60,                 /* Company name               */~
            cursor%(2),                                                  ~
            date$8,                      /* FORMATTED G/L DATE         */~
            dates$(32)8,                 /* FISCAL DATE STRUCTURE      */~
            day1$(3)6,                   /* FIRST OF RELEVENAT MONTH   */~
            day2$(3)6,                   /* LAST  OF RELEVANT MONTH    */~
            descr$30,                    /* ACCOUNT DESCRIPTION        */~
            detaildate$6,                /* DATE FROM DETAIL FOR COMP  */~
            dual_books$1,                /* Dual books in effect?      */~
            errormsg$79,                 /* ERROR MESSAGE FOR DISPLAY  */~
            firstacct$(11)16,            /* FIRST G/L ACCT             */~
            from$(11)16,                 /* FROM G/L ACCT              */~
            i$(24)80,                    /* Screen Image               */~
            lastacct$(11)16,             /* LAST G/L ACCT              */~
            lfac$(11)1,                  /* FEILD ATRIBUTE CHARACTERS  */~
            line2$79,                                                    ~
            line4$79, line5$79,                                          ~
            modules$7,                                                   ~
            months$(12)9,                /* MONTHS LIST AVAILABLE      */~
            monthe$3,                    /* MONTH END?                 */~
            mdate$(3)12,                 /* DATES OF AVAILABLE MONTHS  */~
            oldreadkey$50,               /* OLD KEY FOR PLOW ROUTINES  */~
            ovflaccount$16,              /* LAST ACCOUNT # PRINTED     */~
            periodsopen$12,                                              ~
            plowkey$50,                  /* GL MAIN FILE PLOW KEY      */~
            prtaccount$16,               /* PRINT ACCOUNT NUMBER       */~
            rangeofaccounts$17,                                          ~
            ref$50, ref2$34,             /* REFERENCE TEXT STRINGS     */~
            rpthdr$60,                                                   ~
            rptid$6,                     /* Report ID                  */~
            set$1, setdescr$30, sethdr$60,/* Set of books to use       */~
            setfac$1,                    /* FAC for SET$               */~
            setmsg1$18, setmsg2$40,      /* Screen message for SET     */~
            sum$3,                       /* SUMMARY PRINTING ONLY      */~
            temp$8,                      /*                            */~
            tasktype$1,                  /* FOREGROUND OR BACKGROUND   */~
            tfac$(11)1,                  /* FEILD ATRIBUTE CHARACTERS  */~
            time$8,                      /* System Time                */~
            to$(11)16,                   /* TO G/L ACCT                */~
            tomsg$(11)2,                 /* SCREEN LITERIALS           */~
            transtypes$(11)2,            /* TRANSACTION TYPES          */~
            transhead$(11)30,            /* TRANSACTION HEADINGS       */~
            type$2,                      /* TRANSACTION TYPE FOR DETAIL*/~
            userid$3,                    /* USER ID CURRENT USER       */~
            zetype$2                     /* MODULE TYPE THIS RUN       */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            fs%(64),                     /* For OPENCHCK statements    */~
            rslt$(64)20                  /* RETURN CODE FROM "OPENFILE"*/

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
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#03, "SWITCHS.GL", f1%(3))
                if f1%(3) = 0% then goto L09000
            get #03 using L02374, dual_books$
L02374:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#11%, fs%(11), f2%(11), 0%, rslt$(11))
                call "OPENCHCK" (#12%, fs%(12), f2%(12), 0%, rslt$(12))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES CONTROL VARIABLES, SETS AVAILABLE DATES, ETC. *~
            *************************************************************

            setfac$ = hex(9c)
            if dual_books$ <> "Y" then goto L09062
                setmsg1$ = "G/L System to use:"
                setmsg2$ = "('1' = Statutory; '2' = Local Authority)"
                setfac$ = hex(82)
L09062:     if dual_books$ = "Y" then lo% = 1% else lo% = 2%
            line4$ = "Enter: a character beside desired Periods and Mod"&~
                "ules; Journal Ranges as ap-"
            if dual_books$ <> "Y"                                        ~
                then line5$ = "plicable; 'YES' or 'NO' for summary."     ~
                else line5$ = "plicable; G/L System code you desire; 'Y"&~
                     "ES' or 'NO' for summary."
            call "EXTRACT" addr ("ID", userid$, "TT", tasktype$)
            date$ = date : call "DATEFMT" (date$)
            call "COMPNAME" (12%, compname$, 0%)
            str(line2$,62) = "GLJURN99: " & str(cms2v$,1,8)
            periodsopen$ = "Periods Open"
            modules$ = "Modules"
            rangeofaccounts$ = "Range of Accounts"
            rptid$ = "G/L012"

            months$( 1)="Jan" : months$( 2)="Feb" : months$( 3)="Mar"
            months$( 4)="Apr" : months$( 5)="May" : months$( 6)="Jun"
            months$( 7)="Jul" : months$( 8)="Aug" : months$( 9)="Sep"
            months$(10)="Oct" : months$(11)="Nov" : months$(12)="Dec"
            call "READ100" (#03, "FISCAL DATES", f1%(3))
                if f1%(3) <> 0 then L09260
            err% = 0%
L09220:     call "ASKUSER" (err%, "ERROR ON SETTING DATE RANGE", "Revie"&~
                "w your fiscal date structure", "Press PF-16 to Acknowl"&~
                "edge and Exit Program", " ")
            goto L65000
L09260:         get #03, using L09270, periods%, monthopen%, dates$()
L09270:                 FMT XX(20), BI(2), XX(136), BI(2), 32*CH(8)
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
                   convert str(udate$,5%,2%)to month%,data goto L09220
		   tdate$ = day2$(temp%)
		   call "DATEFMT" (tdate$, tdate%, udate$)
                   mdate$(temp%) = months$(month%) & " " &               ~
                            str(udate$,7%,2%) & ", " & str(udate$,1%,4%)
                   call "DATE"addr("G+",day1$(temp%),1%,day1$(temp%),u3%)
                   if u3% <> 0% then L09220
                next temp%

            REM SET TRANSACTION TYPES
            i% = 0%
            init(hex(ff)) transtypes$()
            oldreadkey$ = "MODULENO:0000"
L09550:     call "PLOWNEXT" (#03, oldreadkey$, 11%, f1%(3%))
                if f1%(3%) = 0% then L09620
            i% = i% + 1 : if i% > 11 then L09620
            call "DESCRIBE" (#03, oldreadkey$, transhead$(i%), 0%, f1%(3))
            transtypes$(i%) = str(oldreadkey$,12,2)
            goto L09550

L09620:     REM SET UP SREEN FACS...
            lfac$() = all(hex(81))
            tfac$() = all(hex(8c))
            for i% = 1 to 11
                if transtypes$(i%) > "^" then lfac$(i%),tfac$(i%)=hex(9c)
            next i%
            tomsg$() = "TOTOTOTOTOTOTOTOTOTOTO"/* Not in Kansas anymore */

            REM DO A GETPARM TO FIND IF WE ARE AT MONTH END
                call "GETPARM" addr ("I ", "R", "MONTHEND", aid$, "0001",~
                     "GLTRAL", "Are We at Month End?", 20%, "K",         ~
                     "MONTHEND", monthe$, 3%, 5%, 32%, "A")
                if monthe$ <> "YES" then L10000
                   init(" ") chomon$(), chojrn$()
                   chomon$(2) = "X"
                   str(chojrn$(), 1, 10) = "XXXXXXXXXX"
                   sum$ = "YES"
                   already% = 0
                   for temp% = 1 to 11             /* SET ACCT RANGES  */
                       firstacct$(temp%) = all(hex(00))
                       lastacct$(temp%)  = all(hex(ff))
                   next temp%
                set$ = "1" : set = 1
L09850:         gosub gl_set_setup
                   go to L10400

L10000: REM *************************************************************~
            *         I N P U T   W H I C H   M O N T H / MODULE(S)     *~
            *                                                           *~
            * CONTROLING SECTION FOR WHICH MONTH AND MODULE(S) TO PRINT *~
            *************************************************************

            call "ALLFREE"
            if monthe$ <> "YES" then L10070
            if dual_books$ <> "Y" then L65000
            if set = 2 then L65000      /* Let's not do it again */
            set$ = "2" : set = 2
            goto L09850

L10070:     sum$ = "YES"

            REM GET MONTHS AND MODULES TO PRINT
                init(" ") chomon$(), chojrn$(), to$(), errormsg$, set$,  ~
                    setdescr$, sethdr$
                criteriasw% = 0%
                for temp% = 1 to 11                /* SET ACCT RANGES  */
                    from$(temp%) = "ALL"
                next temp%

L10150:         gosub L40000
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
            zemonth%, zejrn% = 0
            zetype$ = " "
            already% = 0

L11000: REM *************************************************************~
            *  T R U C K   T H R U   C O N T R O L L I N G   I T   ALL  *~
            *                                                           *~
            * CONTROLLER SECTION FOR PRINTING, AND TOTALLING            *~
            *************************************************************

            REM INCREMENT MONTH
                zemonth% = zemonth% + 1
                if zemonth% > 3 then L11930
                if chomon$(zemonth%) = " " then L11000
                zejrn% = 0

L11120:     REM INCREMENT MODULE
                zejrn% = zejrn% + 1
                if zejrn% > 11 then L11000
                if chojrn$(zejrn%) = " " then L11120

            REM SET VARIABLES FOR NEW MODULE
                zetype$ = transtypes$(zejrn%)
                if zetype$ = "99" and  already% = 1 then L11120
                if zetype$ = "99" then already% = 1
                plowkey$ = firstacct$(zejrn%)
                bigtotaldebits, bigtotalcredits = 0
                pageline% = 1000
                pagenumber% = 1
                call "SHOSTAT" ("Processing " & transhead$(zejrn%) &     ~
                    " Module for Period Ending " & mdate$(zemonth%))

                rpthdr$ = "MONTHLY MODULE RECAP FOR "
                call "FMTTITLE" (rpthdr$, transhead$(zejrn%), 12%)
L11280: REM PLOW THRU MAIN FILE
                call "PLOWNEXT" (#main%, plowkey$, 0%, f1%(main%))
                     if f1%(main%) = 0 then L11840
                accttype$ = " "
                get #main%, using L11330, prtaccount$, descr$, accttype$
L11330:                 FMT CH(9), CH(30), CH(1)
              if str(prtaccount$,,9)>str(lastacct$(zejrn%),,9) then L11840
                totaldebits, totalcredits = 0

            REM GO GET AND SORT DETAIL BY MODULE DATE POSTED
            init (hex(00)) oldreadkey$
            str(oldreadkey$,,22)=str(prtaccount$,,16) &                  ~
                                              str(day1$(zemonth%),1,6)
            if zetype$ = "99" then str(oldreadkey$,17,6) = "010101"
L11378:           call "PLOWNEXT" (#detl%, oldreadkey$, 16%, f1%(detl%))
                       if f1%(detl%) = 0 then L11280
                  get #detl%, using L11384, detaildate$,type$
L11384:           FMT  XX(16), CH(6), XX(4), CH(2)
                      if type$ = "99" and zetype$ = "99" then L11400
                      if detaildate$ > day2$(zemonth%) then L11280
                      if type$ <> zetype$ then L11378

L11400:     REM PRINT DETAIL FOR THIS ACCOUNT
                gosub L60000
                if set = 1                                               ~
                    then call "GLFMT" (prtaccount$)                      ~
                    else call "GLFMT2" (prtaccount$)
                print using L60520, prtaccount$, descr$
                ovflaccount$ = prtaccount$
                gosub L21000              /* ACCOUNT TYPE DESCRIPTION   */
                goto L11500

L11480:         call "PLOWNEXT" (#detl%, oldreadkey$, 16%, f1%(detl%))
                     if f1%(detl%) = 0 then L11640
L11500:         get #detl%,using L11510,detaildate$,type$,debit,credit,   ~
                     ref2$, ref$
L11510:         FMT  XX(16),CH(6),XX(4),CH(2),2*PD(14,4), POS(75),CH(34),~
                     POS(113), CH(32)
                      if detaildate$ > day2$(zemonth%) then L11640
                      if type$ <> zetype$ then L11480
                temp$ = detaildate$
                call "DATEFMT" (temp$)
                if sum$ = "YES" then L11590
                gosub L60000
                debit = round (debit, 2%)
                credit= round (credit, 2%)
                if pageline% = 8 and ovflaccount$ = prtaccount$ then     ~
                     print using L60550, prtaccount$, ref$, ref2$, temp$, ~
                     debit, credit                                       ~
                else                                                     ~
                     print using L60550, typedescr$, ref$, ref2$, temp$,  ~
                     debit, credit
L11590:         typedescr$ = " "
                totaldebits = totaldebits + debit
                totalcredits = totalcredits + credit
                go to L11480

L11640:     REM PRINT TOTAL FOR THIS ACCOUNT
                totaldebits = round (totaldebits, 2%)
                totalcredits= round (totalcredits, 2%)
                print using L60550," ","***** TOTALS *****"," ", " ",     ~
                                  totaldebits, totalcredits
                print using L60460
                pageline% = pageline% + 2
                bigtotaldebits = bigtotaldebits + totaldebits
                bigtotalcredits = bigtotalcredits + totalcredits

                go to L11280

L11840:     REM PRINT TOTAL FOR THIS MODULE
                bigtotaldebits = round (bigtotaldebits, 2%)
                bigtotalcredits= round (bigtotalcredits, 2%)
                if pageline% = 1000 then gosub L60000
                print
                print using L60580, "MODULE TOTALS", bigtotaldebits,      ~
                                    bigtotalcredits
                goto L11120

L11930:     REM CLOSE PRINTER AND SEND THIS BATCH OFF
                print using L60460
                print
                time$ = " "   :   call "TIME" (time$)
                print using L60600, time$    /* End of Report Message */
                close printer
                call "SETPRNT" (rptid$, " ", 0%, 1%)
                go to L10000

L21000: REM *************************************************************~
            *   A C C O U N T   T Y P E   S E T T E R                   *~
            *                                                           *~
            * SET THE ACCOUNT TYPE DESCRIPTION GIVEN THE TYPE ITSELF    *~
            *************************************************************

            typedescr$ = "Unknown"
            if accttype$ = "$" then typedescr$ = "Cash     "
            if accttype$ = "A" then typedescr$ = "Asset    "
            if accttype$ = "L" then typedescr$ = "Liability"
            if accttype$ = "C" then typedescr$ = "Capital  "
            if accttype$ = "R" then typedescr$ = "Revenue  "
            if accttype$ = "E" then typedescr$ = "Expense  "
            return

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return
            return clear all
            goto L10000

L40000: REM *************************************************************~
            *        I N P U T   C H O I C E   O F  MONTHS/MODULE(S)    *~
            *                                                           *~
            * INDICATE WHICH MONTHS AND MODULES YOU DESIRE              *~
            *************************************************************

        deffn'201(fieldnr%)
            pfmessage$ = "(16)EXIT PROGRAM"
            goto L40057

        deffn'221(fieldnr%)
            pfmessage$ = " "
L40057:     blankline$ = " "
L40060:     accept                                                       ~
               at (01,02), "General Ledger Module Recap",                ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
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
               at (19,25), "(Enter YES or NO)",                          ~
                                                                         ~
               at (07,31), fac(hex(ac)), modules$               , ch(07),~
               at (08,20), fac(lfac$(01)), chojrn$( 1)          , ch(01),~
               at (09,20), fac(lfac$(02)), chojrn$( 2)          , ch(01),~
               at (10,20), fac(lfac$(03)), chojrn$( 3)          , ch(01),~
               at (11,20), fac(lfac$(04)), chojrn$( 4)          , ch(01),~
               at (12,20), fac(lfac$(05)), chojrn$( 5)          , ch(01),~
               at (13,20), fac(lfac$(06)), chojrn$( 6)          , ch(01),~
               at (14,20), fac(lfac$(07)), chojrn$( 7)          , ch(01),~
               at (15,20), fac(lfac$(08)), chojrn$( 8)          , ch(01),~
                                                                         ~
               at (08,22), fac(hex(8c)), transhead$( 1)         , ch(30),~
               at (09,22), fac(hex(8c)), transhead$( 2)         , ch(30),~
               at (10,22), fac(hex(8c)), transhead$( 3)         , ch(30),~
               at (11,22), fac(hex(8c)), transhead$( 4)         , ch(30),~
               at (12,22), fac(hex(8c)), transhead$( 5)         , ch(30),~
               at (13,22), fac(hex(8c)), transhead$( 6)         , ch(30),~
               at (14,22), fac(hex(8c)), transhead$( 7)         , ch(30),~
               at (15,22), fac(hex(8c)), transhead$( 8)         , ch(30),~
                                                                         ~
               at (07,57), fac(hex(ac)), rangeofaccounts$       , ch(17),~
               at (08,52), fac(lfac$(01)), from$( 1)            , ch(12),~
               at (09,52), fac(lfac$(02)), from$( 2)            , ch(12),~
               at (10,52), fac(lfac$(03)), from$( 3)            , ch(12),~
               at (11,52), fac(lfac$(04)), from$( 4)            , ch(12),~
               at (12,52), fac(lfac$(05)), from$( 5)            , ch(12),~
               at (13,52), fac(lfac$(06)), from$( 6)            , ch(12),~
               at (14,52), fac(lfac$(07)), from$( 7)            , ch(12),~
               at (15,52), fac(lfac$(08)), from$( 8)            , ch(12),~
                                                                         ~
               at (08,65), fac(tfac$(01)), tomsg$(01)           , ch(02),~
               at (09,65), fac(tfac$(02)), tomsg$(02)           , ch(02),~
               at (10,65), fac(tfac$(03)), tomsg$(03)           , ch(02),~
               at (11,65), fac(tfac$(04)), tomsg$(04)           , ch(02),~
               at (12,65), fac(tfac$(05)), tomsg$(05)           , ch(02),~
               at (13,65), fac(tfac$(06)), tomsg$(06)           , ch(02),~
               at (14,65), fac(tfac$(07)), tomsg$(07)           , ch(02),~
               at (15,65), fac(tfac$(08)), tomsg$(08)           , ch(02),~
                                                                         ~
               at (08,68), fac(lfac$(01)),   to$( 1)            , ch(12),~
               at (09,68), fac(lfac$(02)),   to$( 2)            , ch(12),~
               at (10,68), fac(lfac$(03)),   to$( 3)            , ch(12),~
               at (11,68), fac(lfac$(04)),   to$( 4)            , ch(12),~
               at (12,68), fac(lfac$(05)),   to$( 5)            , ch(12),~
               at (13,68), fac(lfac$(06)),   to$( 6)            , ch(12),~
               at (14,68), fac(lfac$(07)),   to$( 7)            , ch(12),~
               at (15,68), fac(lfac$(08)),   to$( 8)            , ch(12),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,65), "(15)Print Screen",                           ~
               at (22,65), "(13)Instructions",                           ~
               at (24,65), fac(hex(8c)),  pfmessage$             ,ch(16),~
                                                                         ~
                                                                         ~
               keys(hex(00010d0f10)),  key (keyhit%)

               if keyhit% <> 13 then L41080
                  call "MANUAL" ("GLJURN99")
                  goto L40060

L41080:        if keyhit% <> 15 then goto L41120
                  call "PRNTSCRN"
                  goto L40060

L41120:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *        I N P U T   C H O I C E   O F  MONTHS/JOURNALS     *~
            *                                                           *~
            * INDICATE WHICH MONTHS AND JOURNALS YOU DESIRE             *~
            *************************************************************

            deffn'211(fieldnr%)
            blankline$ = "Press RETURN to Edit Selection"
L42080:     accept                                                       ~
               at (01,02), "General Ledger Module Recap",                ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
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
               at (19,21), fac(hex(84)), sum$                   , ch(03),~
               at (19,25), "(Enter YES or NO)",                          ~
                                                                         ~
               at (07,31), fac(hex(ac)), modules$               , ch(07),~
               at (08,20), fac(hex(84)),   chojrn$( 1)          , ch(01),~
               at (09,20), fac(hex(84)),   chojrn$( 2)          , ch(01),~
               at (10,20), fac(hex(84)),   chojrn$( 3)          , ch(01),~
               at (11,20), fac(hex(84)),   chojrn$( 4)          , ch(01),~
               at (12,20), fac(hex(84)),   chojrn$( 5)          , ch(01),~
               at (13,20), fac(hex(84)),   chojrn$( 6)          , ch(01),~
               at (14,20), fac(hex(84)),   chojrn$( 7)          , ch(01),~
               at (15,20), fac(hex(84)),   chojrn$( 8)          , ch(01),~
                                                                         ~
               at (08,22), fac(hex(8c)), transhead$( 1)         , ch(30),~
               at (09,22), fac(hex(8c)), transhead$( 2)         , ch(30),~
               at (10,22), fac(hex(8c)), transhead$( 3)         , ch(30),~
               at (11,22), fac(hex(8c)), transhead$( 4)         , ch(30),~
               at (12,22), fac(hex(8c)), transhead$( 5)         , ch(30),~
               at (13,22), fac(hex(8c)), transhead$( 6)         , ch(30),~
               at (14,22), fac(hex(8c)), transhead$( 7)         , ch(30),~
               at (15,22), fac(hex(8c)), transhead$( 8)         , ch(30),~
                                                                         ~
               at (07,57), fac(hex(ac)), rangeofaccounts$       , ch(17),~
               at (08,52), fac(hex(84)),   from$( 1)            , ch(12),~
               at (09,52), fac(hex(84)),   from$( 2)            , ch(12),~
               at (10,52), fac(hex(84)),   from$( 3)            , ch(12),~
               at (11,52), fac(hex(84)),   from$( 4)            , ch(12),~
               at (12,52), fac(hex(84)),   from$( 5)            , ch(12),~
               at (13,52), fac(hex(84)),   from$( 6)            , ch(12),~
               at (14,52), fac(hex(84)),   from$( 7)            , ch(12),~
               at (15,52), fac(hex(84)),   from$( 8)            , ch(12),~
                                                                         ~
               at (08,65), fac(tfac$(01)), tomsg$(01)           , ch(02),~
               at (09,65), fac(tfac$(02)), tomsg$(02)           , ch(02),~
               at (10,65), fac(tfac$(03)), tomsg$(03)           , ch(02),~
               at (11,65), fac(tfac$(04)), tomsg$(04)           , ch(02),~
               at (12,65), fac(tfac$(05)), tomsg$(05)           , ch(02),~
               at (13,65), fac(tfac$(06)), tomsg$(06)           , ch(02),~
               at (14,65), fac(tfac$(07)), tomsg$(07)           , ch(02),~
               at (15,65), fac(tfac$(08)), tomsg$(08)           , ch(02),~
                                                                         ~
               at (08,68), fac(hex(84)),     to$( 1)            , ch(12),~
               at (09,68), fac(hex(84)),     to$( 2)            , ch(12),~
               at (10,68), fac(hex(84)),     to$( 3)            , ch(12),~
               at (11,68), fac(hex(84)),     to$( 4)            , ch(12),~
               at (12,68), fac(hex(84)),     to$( 5)            , ch(12),~
               at (13,68), fac(hex(84)),     to$( 6)            , ch(12),~
               at (14,68), fac(hex(84)),     to$( 7)            , ch(12),~
               at (15,68), fac(hex(84)),     to$( 8)            , ch(12),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,65), "(15)Print Screen",                           ~
               at (22,65), "(13)Instructions",                           ~
               at (24,65), "(16)PRINT REPORT",                           ~
                                                                         ~
                                                                         ~
               keys(hex(00010d0f10)),  key (keyhit%)

               if keyhit% <> 13 then L43110
                  call "MANUAL" ("GLJURN99")
                  goto L42080

L43110:        if keyhit% <> 15 then goto L43150
                  call "PRNTSCRN"
                  goto L42080

L43150:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L50000: REM *************************************************************~
            *        T E S T   D A T A   E N T E R E D                  *~
            *                                                           *~
            * TEST CHOICE OF MONTHS AND MODULES                         *~
            *************************************************************

            errormsg$ = " "
            REM TEST FOR NON BLANK
                temp = pos(str(chomon$(), 1) <> " ")
                if temp <> 0 then L50120
                   errormsg$ = "Please choose at least one month"
                   return
L50120:         temp = pos(str(chojrn$(), 1) <> " ")
                if temp <> 0 then L50170
                   errormsg$ = "Please Choose At Least One Module"
                   return

L50170: REM Test to see if dual books is in effect and which set to use
                set = 1 : main% = 1% : detl% = 2%     /* Set defaults */
                if dual_books$ <> "Y" then goto L50240
                call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
                if errormsg$ = " " then goto L50220
                    errormsg$ = "G/L System code must be '1'(Statutory)"&~
                        " or '2'(Local Authority)"
                    return
L50220:         gosub gl_set_setup

L50240:         if sum$ = "YES" then L50290
                if sum$ = "NO"  then L50290
                   errormsg$ = "Answer 'YES' or 'NO' For Summary Printing"
                   return

L50290: REM *************************************************************~
            *             R A N G E   V A L I D A T I O N               *~
            * THE PURPOSE OF THIS ROUTINE IS TO NORMALIZE THE KEYS FOR  *~
            * THE RANGE AND ERROR IF LAST < FIRST.                      *~
            *************************************************************

             errormsg$ = " "
             for temp% = 1 to 11%
                firstacct$(temp%) = from$(temp%)
                lastacct$(temp%)  = to$(temp%)
                REM HANDLES CASE FOR "ALL" ACCTS
                    if firstacct$(temp%) <> "ALL" then L50440
                       init(hex(00)) firstacct$(temp%)
                       init(hex(ff)) lastacct$(temp%)
                       goto L50650
L50440: REM HANDLES CASE FOR SINGLE ACCT
            if set = 1                                                   ~
           then call "GLVALID" (from$(temp%),firstacct$(temp%),errormsg$)~
           else call "GLVALD2" (from$(temp%),firstacct$(temp%),errormsg$)
            if errormsg$ = " " then L50490
                errormsg$ = errormsg$ & ": " & from$(temp%)
                return
L50490:     if lastacct$(temp%) <> " " then L50520
                lastacct$(temp%) = firstacct$(temp%)
                goto L50580
L50520: REM HANDLES CASE FOR A RANGE OF ACCTS
            if set = 1                                                   ~
            then call "GLVALID" (to$(temp%), lastacct$(temp%), errormsg$)~
            else call "GLVALD2" (to$(temp%), lastacct$(temp%), errormsg$)
                if errormsg$ = " " then L50570
                errormsg$ = errormsg$ & ": " & to$(temp%)
                return
L50570:     if lastacct$(temp%) < firstacct$(temp%) then L50610
L50580:         firstacct$(temp%) = firstacct$(temp%) addc all(hex(ff))
                goto L50650
L50610: REM HANDLES ERROR MESSAGE -- LAST < FIRST.
            errormsg$ = "Invalid Range For " & transhead$(temp%) &       ~
                " Module.  Please Respecify"
            return
L50650:     next temp%
            return

        gl_set_setup
            criteriasw% = 0%
            setdescr$ = "Statutory"
            main% = 1% : detl% = 2%
            if set = 1 then goto L53880
                setdescr$ = "Local Authority"
                main% = 11% : detl% = 12%
L53880:     sethdr$ = setdescr$
            call "FMTTITLE" (sethdr$, "G/L SYSTEM", 12%)
            call "PUTPAREN" (setdescr$)
            return

L60000: REM *************************************************************~
            * P A G E   H E A D E R   C O N T R O L L E R               *~
            *                                                           *~
            * PAGE HEADER CONTROLLER ROUTINE                            *~
            *************************************************************

            pageline% = pageline% + 1
            if pageline% < 55 then return
*          IF PAGENUMBER% <> 0% THEN GOTO 60190
            if criteriasw% <> 0% then goto L60190
                criteriasw% = 1%
                select printer (134)
                pagenumber% = 0%
                time$ = " " : call "TIME" (time$)
L60102:         i% = pos(str(i$()) > hex(7f))
*         ** Fix Facs **
                if i% = 0% then L60110
                    str(i$(), i%, 1%) = hex(20)
                    goto L60102

L60110:         call "SETPRNT" (rptid$, " ", 0%, 0%)
                pagesw% = 1%
                gosub L60200
                pagesw% = 0%
                print skip (2)
                print using L60440, "R E P O R T   S E L E C T I O N S:"
                print
                if monthe$ <> "YES" then L60160
                i$(21) = "End of Month Report on " & setdescr$ & " Books"
                print using L60440, i$(21)
                goto L60181
L60160:         for x% = 4% to 20%
                    print using L60440, i$(x%)
                next x%
L60181:         goto L60200
L60190:     print using L60460
L60200:     print page
            print using L60340, date$, time$, compname$, "-" & rptid$
            print using L60370, rpthdr$, pagenumber%
            print using L60400, sethdr$
            print using L60420, mdate$(zemonth%)
            print
            if pagesw% <> 0% then goto L60300
            print using L60460
            print using L60490
            print using L60460
L60300:     pagenumber% = pagenumber% + 1%
            pageline% = 8%
            return

L60340: %RUN: ######## @ ########            ############################~
        ~################################                     GLJURN99####~
        ~###
L60370: %                                    ############################~
        ~################################                          PAGE: #~
        ~###
L60400: %                                    ############################~
        ~################################
L60420: %                                                   PERIOD ENDING~
        ~ ###############
L60440: %                          ######################################~
        ~##########################################
L60460: %+-------------+---------------------------------+---------------~
        ~--------------------+----------+-----------------+---------------~
        ~--+
L60490: %! ACCOUNT     ! ACCOUNT/DETAIL DESCRIPTION      ! DETAIL REFEREN~
        ~CE                  !  POSTED  !          DEBITS !         CREDIT~
        ~S !
L60520: %! ############! ################################!               ~
        ~                    !          !                 !               ~
        ~  !
L60550: %! ############! ################################! ##############~
        ~####################! ######## !-####,###,###.## !-####,###,###.#~
        ~# !
L60580: %                     ########################################   ~
        ~                                -####,###,###.## !-####,###,###.#~
        ~#
L60600:       %                                                 *** END O~
        ~F REPORT  @  ########  ***

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One moment, please")
            end
