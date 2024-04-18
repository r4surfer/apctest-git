        REM *************************************************************~
            *                                                           *~
            *   GGG   L      DDDD   EEEEE  TTTTT   SSS   U   U  M   M   *~
            *  G      L      D   D  E        T    S      U   U  MM MM   *~
            *  G GGG  L      D   D  EEEE     T     SSS   U   U  M M M   *~
            *  G   G  L      D   D  E        T        S  U   U  M   M   *~
            *   GGG   LLLLL  DDDD   EEEEE    T     SSS    UUU   M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLDETSUM - Summarizies general ledger within a given      *~
            *            period by sequence number or by journal        *~
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
            * 06/13/86 ! ORIGINAL                                 ! RAC *~
            * 08/04/87 ! Cleaned Detail Lines to Show 2 Place     !     *~
            *          ! Decimals.  Also Cleaned Headings.        ! DAW *~
            * 08/17/88 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/17/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 10/25/88 ! Fixed Break Logic. Periods can span month! JDH *~
            * 11/27/89 ! Date and seq # associated with summarized! JDH *~
            *          !  record is now the highest seq # of a    !     *~
            *          !  summed group and the date of that seq #.!     *~
            * 06/15/90 ! Changed Report ID to G/L009              ! MJB *~
            * 12/14/90 ! Changed Call to JNLINFO for GL Batches   ! RAC *~
            * 10/28/91 ! PRR 12148, 10687.  Fixed summarization   ! JDH *~
            *          !  when detail has both debit & credit.    !     *~
            * 10/20/92 ! Removed FACs from page zero.             ! JIM *~
            * 10/26/92 ! PRR 12653 - Corrected JNLINFO arguments. ! MLJ *~
	    * 06/24/96 ! Changes for the year 2000.               ! DLX *~
            *************************************************************

        dim                                                              ~
            account$9,                   /* G/L ACCOUNT NUMBER         */~
            blankline$79,                /* LINE UNDERLINED IN SCREEN  */~
            chojrn$(15)1,                /* WHICH JOURNALS TO PRINT    */~
            chomon$(15)1,                /* WHICH MONTHS TO PRINT      */~
            compname$60,                 /* Company name               */~
            credit$13,                   /* Credit Amount for Report   */~
            cursor%(2),                                                  ~
            date$8,                      /* FORMATTED G/L DATE         */~
            dates$(32)8,                 /* FISCAL DATE STRUCTURE      */~
            day1$(3)6,                   /* FIRST OF RELEVENAT MONTH   */~
            day2$(3)6,                   /* LAST  OF RELEVANT MONTH    */~
            debit$13,                    /* Debit Amonut for Report    */~
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
            location$2,                  /* LOCATION OF ACCT ON STACK  */~
            modno$2,                     /* MODULE NUMBER              */~
            modules$7,                                                   ~
            months$(12)9,                /* MONTHS LIST AVAILABLE      */~
            mdate$(3)12,                 /* DATES OF AVAILABLE MONTHS  */~
            periodsopen$12,                                              ~
            prttotaldescr$40,            /* SUMMARY LINE DESCRIPTION   */~
            oldreadkey$50,               /* OLD KEY FOR PLOW ROUTINES  */~
            rangeofjournals$17,                                          ~
            ref1$30,                     /* Reference 1 desc           */~
            ref2$30,                     /* Reference 2 desc           */~
            rptid$6,                     /* Report ID                  */~
            set$1, setdescr$30, sethdr$60, /* Set of books code, etc.  */~
            setfac$1,                    /* FAC for SET$               */~
            setmsg1$18, setmsg2$40,      /* Screen message for SET     */~
            sortflag$(4)1,               /* SORT INDICATORS            */~
            sum$3,                       /* SUMMARY PRINTING ONLY      */~
            temp$8,                      /*                            */~
            tasktype$1,                  /* FOREGROUND OR BACKGROUND   */~
            text$100,                    /*                            */~
	    tdate$8,                     /* Temporary Date variable    */~
            title$40,                    /* JOURNAL TITLE              */~
            to$(15)3,                    /* TO Journal                 */~
            transtypes$(25)2,            /* TRANSACTION TYPES          */~
            transhead$(25)30,            /* TRANSACTION HEADINGS       */~
	    udate$8,			 /* Temp. unformatted date     */~
            userid$3,                    /* USER ID CURRENT USER       */~
            version$79,                  /* SCREEN PROGRAM DISPLAY     */~
            zetype$2                     /* JOURNAL TYPE THIS RUN      */~

        dim                                                              ~
            debitsstk(1500),             /* STACK FOR DEBIT ACCT AMTS  */~
            debitsstk$(1500)9,           /* STACK FOR DEBIT ACCOUNTS   */~
            debitseqstk%(1500),          /* STACK FOR DEBIT ACCT SEQ NR*/~
            debitdatestk$(1500)6,        /* STACK FOR DEBIT Dates Postd*/~
            debitpseqstk%(1500),         /* STACK FOR DEBIT Post SEQ NR*/~
            creditsstk(1500),            /* STACK FOR CREDIT ACCT AMTS */~
            creditsstk$(1500)9,          /* STACK FOR CREDIT ACCOUNTS  */~
            creditdatestk$(1500)6,       /* STACK FOR CREDIT Date Postd*/~
            creditpseqstk%(1500),        /* STACK FOR CREDIT Post SEQ N*/~
            creditseqstk%(1500)          /* STACK FOR CREDIT ACCT SEQ N*/~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            fs%(64)                      /* AXD POINTER FROM "OPENFILE"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con :

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

            call "OPENCHCK" (#01, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#02, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#03, fs%( 3), f2%( 3), 0%, rslt$( 3))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#03, "SWITCHS.GL", f1%(3))
                if f1%(3) = 0% then goto L02590
            get #03 using L02540, dual_books$
L02540:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L02590
                call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
                call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))

L02590:     if f2%(2) = 0% then L02670
L02600:     key% = 0%
            call "ASKUSER" (key%, "*** FILE NOT FOUND ***",              ~
                "GLDETAIL file not found", " ", "Press RETURN to acknow"&~
                "ledge and exit program")
            if key% <> 0% then L02600
            goto L65000

L02670:     if dual_books$ <> "Y" or f2%(12) = 0% then L02750
L02680:     key% = 0%
            call "ASKUSER" (key%, "*** FILE NOT FOUND ***",              ~
                "GLDETAL2 file not found", " ", "Press RETURN to acknow"&~
                "ledge and exit program")
            if key% <> 0% then L02680
            goto L65000

L02750:     get rslt$(2), using L02760, record%
L02760:     FMT XX(16), BI(4)

            call "SETPRTSZ" addr(max(1000%, record%/3%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES CONTROL VARIABLES, SETS AVAILABLE DATES, ETC. *~
            *************************************************************

            rptid$ = "G/L009"
            setfac$ = hex(9c)
            if dual_books$ <> "Y" then goto L09064
                setmsg1$ = "G/L System to use:"
                setmsg2$ = "('1' = Statutory; '2' = Local Authority)"
                setfac$ = hex(82)
L09064:     line4$ = "Enter: a character beside desired Periods and Mod"&~
                "ules; Journal Ranges as ap-"
            if dual_books$ <> "Y"                                        ~
                then line5$ = "plicable; 'JNL' or 'SEQ' for summary."    ~
                else line5$ = "plicable; G/L System code you desire; 'J"&~
                     "NL' or 'SEQ' for summary."
            call "EXTRACT" addr ("ID", userid$, "TT", tasktype$)
            date$ = date
            call "DATEFMT" (date$)
            call "TIME" (time$)
            call "COMPNAME" (12%, compname$, 0%)
            str(version$,62) = "GLDETSUM: " & str(cms2v$,1,8)

            REM FORMAT DATES FOR DETAIL SELECTION
            months$( 1) = "Jan"
            months$( 2) = "Feb"
            months$( 3) = "Mar"
            months$( 4) = "Apr"
            months$( 5) = "May"
            months$( 6) = "Jun"
            months$( 7) = "Jul"
            months$( 8) = "Aug"
            months$( 9) = "Sep"
            months$(10) = "Oct"
            months$(11) = "Nov"
            months$(12) = "Dec"
                call "READ100" (#03, "FISCAL DATES", f1%(3))
                     if f1%(3) <> 0 then L09280
L09245:     err% = 0%
L09250:     call "ASKUSER" (err%, "ERROR ON SETTING DATE RANGE",         ~
                            "Review your fiscal date structure",         ~
                           "Press PF-16 to Acknowledge and Exit Program",~
                           " ")
            if err% <> 16% then L09245
            goto L65000
L09280:         get #03, using L09290, periods%, monthopen%, dates$()
L09290:                 FMT XX(20), BI(2), XX(136), BI(2), 32*CH(8)

                mdate$(1) = dates$(monthopen%+16)
                if monthopen% = 12 then mdate$(1) = dates$(41-periods%)
                if monthopen% = 17 then mdate$(1) = " "

                mdate$(2) = dates$(monthopen%+15)

                mdate$(3) = dates$(monthopen%+14)
                if monthopen% =  1 then mdate$(3) = dates$(periods%+1)
                if monthopen% = 14 then mdate$(3) = dates$(periods%+15)

                temp$ = dates$(monthopen%+13)
                if monthopen% =  1 then temp$ = dates$(periods%)
                if monthopen% =  2 then temp$ = dates$(periods%+1)
                if monthopen% = 14 then temp$ = dates$(periods%+14)
                if monthopen% = 15 then temp$ = dates$(periods%+15)
                day1$(3) = temp$

                for temp% = 1% to 3%
                    day2$(temp%) = mdate$(temp%)
                    if temp% < 3 then day1$(temp%) = mdate$(temp%+1)
		    tdate$ = mdate$(temp%)
		    call "DATEFMT" (tdate$, tdate%, mdate$(temp%) )
                    convert str(mdate$(temp%),5%,2%)to month%,data goto L09250
		    tdate$ = day2$(temp%)
		    call "DATEFMT" (tdate$, tdate%, udate$)
                    mdate$(temp%) = months$(month%) & " " &              ~
                          str(udate$,7%,2%) & ", " & str(udate$, 1%, 4%)
                    call "DATE"addr("G+",day1$(temp%),1%,day1$(temp%),u3%)
                    if u3% <> 0 then L09250
                next temp%

            REM SET TRANSACTION TYPES
            i% = 0
            init(hex(ff)) transtypes$()
            oldreadkey$ = "MODULENO:0000"
L09660:     call "PLOWNEXT" (#03, oldreadkey$, 11%, f1%(3))
                if f1%(3) = 0 then L09750
            i% = i% + 1 : if i% > 11 then L09750
            call "DESCRIBE" (#03,oldreadkey$,transhead$(i%),0%,f1%(3))
            transtypes$(i%) = str(oldreadkey$,12,2)
            goto L09660

L09750:     gosub L18000   /* Initialize Stack                          */

            periodsopen$ = "Periods Open"
            modules$ = "Modules"
            rangeofjournals$ = "Range of Journals"

L10000: REM *************************************************************~
            *         I N P U T   W H I C H   M O N T H / JOURNAL       *~
            *                                                           *~
            * CONTROLING SECTION FOR WHICH MONTH AND JOURNALS TO PRINT  *~
            *************************************************************

        inputmode
            sum$ = "SEQ"
            init(" ") errormsg$, set$, setdescr$, sethdr$
            set = 1 : main% = 1% : detl% = 2% /* Defaults */
            bigdebits, bigcredits = 0
L10090:     REM GET MONTHS AND JOURNALS TO PRINT
                init(" ") chomon$(), chojrn$(), to$()
                for temp% = 1 to 15                /* SET ACCT RANGES  */
                    from$(temp%) = "ALL"
                next temp%

L10150:         gosub L41000    /* Input Choice Mnths/Jrnls             */
                      if keyhit%  =   1 then gosub startover
                      if keyhit%  =  16 then       L65000
                      if keyhit% <>   0 then       L10150
                gosub L50000    /* Test Data Entered                    */
                      if errormsg$ <> " " then L10150

L10270:     gosub'211(fieldnr%)   /* Edit Choice Mnths/Jrnls           */
                      if keyhit%  =   1 then gosub startover
                      if keyhit%  =  16 then       L10390
                      if keyhit% <>   0 then       L10270

L10320:     gosub'221(fieldnr%)   /* Input Routine                     */
                      if keyhit%  =   1 then gosub startover
                      if keyhit% <>   0 then       L10320
                gosub L50000       /* Test Data Entered                 */
                      if errormsg$ <> " " then L10320
            goto L10270

L10390:     REM RESET VARIABLES
               select printer(134)
               call "SETPRNT" (rptid$, " ", 0%, 0%)
               zemonth%, zejrn% = 0
               zetype$ = " "
               printline% = 100
               pageno%, headflag% = 0

L11000: REM *************************************************************~
            *            C O N T R O L   S E C T I O N                  *~
            *                                                           *~
            * CONTROLLER SECTION FOR PRINTING, AND TOTALLING            *~
            *************************************************************

            REM INCREMENT MONTH
                zemonth% = zemonth% + 1
                if zemonth% > 3 then close printer
                if zemonth% > 3 then L10000
                if chomon$(zemonth%) = " " then L11000
                zejrn%, totaldebits, totalcredits = 0
                gosub L14000

                goto  L11000

L14000: REM *************************************************************~
            *S O R T  A N D  P R I N T  A  R A N G E  O F  J O U R N A L*~
            *                                                           *~
            *OPENS A SORT FILE FOR A PERIOD, SORTS,AND THEN PRINTS      *~
            *************************************************************

            call "WORKOPEN" (#04, "IO", max(100%, record%/3%), f2%(4%))
                if f2%(4%) <> 0 then L16000  /* Error Routine Workfile  */
            oldreadkey$ = all(hex(00))
            headflag%, wseq% = 0
            pageflag% = 1%
            call "SHOSTAT" ("Sorting G/L Detail For Period Ending " &    ~
                            mdate$(zemonth%))

            REM DO PLOW ROUTINE, GATHERING APPROPRIATE INFORMATION
L14140:         call "READ102" (#main%, oldreadkey$, f1%(main%))
                     if f1%(main%) = 0 then L14490
                get #main%, using L14170, account$
L14170:                 FMT CH(9)
                init (hex(00)) oldreadkey$
                str(oldreadkey$, 1, 16)  = str(account$)
                str(oldreadkey$, 17, 6) = day1$(zemonth%)
L14210:         gosub L30000    /* Detail Plow Routine                  */
                if f1%(detl%) = 0 then L14140
                if prtposted$ > day2$(zemonth%)    then L14140
                search transtypes$() = modno$ to look%() step 2
                if look%(1) = 0                    then L14210
                modno% = (look%(1) + 1)/2
                if chojrn$(modno%)   = " "         then L14210
                if jnlid$ < firstacct$(modno%)     then L14210
                if jnlid$ > lastacct$(modno%)      then L14210

            REM NOW SAVE THE POINTER IN THE WORKFILE
                write #04, using L14400, modno$, jnlid$, pstseq%, wseq%,  ~
                              account$, prtposted$, seq%, eod goto L16000
L14400:         FMT CH(2),CH(3),BI(4),BI(4),CH(16),CH(6), BI(4)
                wseq% = wseq% + 1
                goto L14210

L14490:     REM NOW PRINT THE WORKFILE
                call "SHOSTAT" ("Printing Journals For Periods "         ~
                                & mdate$(zemonth%))
                init (hex(00)) newreadkey$
                init (hex(20)) modno$, jnlid$
                pstseq% = -2
                call "READ102" (#04, newreadkey$, f1%(4))
                     goto L14580
L14570:         call "READNEXT" (#04, f1%(4))
L14580:              if f1%(4) = 0 then L15250
                get #04 using L14400, wmodno$, wjnlid$, wpstseq%, wseq%,  ~
                           account$, prtposted$, seq%
                if wmodno$ <> modno$ then L15000
                if sortflag$(3) <> " " and wjnlid$ <> jnlid$ then L14950
                if sortflag$(4) <> " " and wpstseq% <> pstseq% then L14910
L14640:         put oldreadkey$, using L14650, account$, prtposted$, seq%
L14650:         FMT CH(16), CH(6), BI(4)
                gosub L30050    /* Detail Plow Routine                  */
                if f1%(detl%) = 0 then L16000 /* Error Routine Workfile */
                dateposted$ = prtposted$
                delete #detl%
                if headflag% = 0 then gosub sub_heading
                headflag% = 1
                gosub L14730
                goto L14570

L14730:     REM PRINT DETAIL LINE
                call "DATEFMT" (prtposted$)
                call "CONVERT" (debit,2.2, debit$)
                call "CONVERT" (credit,2.2, credit$)
                if debit = 0  then debit$ = " "
                if credit = 0 then credit$ = " "
                totaldebits = totaldebits + debit
                totalcredits = totalcredits + credit
                printline% = printline% + 1
                if printline% > 55% then gosub heading
                tempacct$ = account$
                if set = 1                                               ~
                    then call "GLFMT" (tempacct$)                        ~
                    else call "GLFMT2" (tempacct$)
                print using L55456,tempacct$, prtposted$, modno$, jnlid$, ~
                    ref1$, ref2$, descr$, debit$, credit$
        REM  DEF'162 and DEF'163 are Stack Pushing Routines
                if debit = 0 or credit = 0 then L14868
                     if debit <> credit then L14865
                          debit = 0 : credit = 0 : goto L14868
L14865:              if debit > credit then L14867
                          credit = credit - debit : debit = 0 : goto L14868
L14867:              debit = debit - credit : credit = 0
L14868:         if debit  <> 0 then gosub'162 (account$, debit)
                if credit <> 0 then gosub'163 (account$, credit)
                pageflag% = 0%
                return

L14910:         REM SUBTOTAL SECOND SUBTOTAL
                    gosub L17000          /* Rewrite G/L Detail         */
                    gosub L15310          /* Subtotal for Subtotal2     */
                    goto L14640

L14950:         REM SUBTOTAL FIRST SUBTOTAL
                    gosub L17000          /* Rewrite G/L Detail         */
                    gosub L15310          /* Subtotal for Subtotal2     */
                    gosub L15395          /* Subtotal for Subtotal1     */
                    goto L14640

L15000:         REM SUBTOTAL MODULE/ACCOUNT
                    gosub L15050
                    headflag% = 0
                    goto L14640

L15050:        REM ACTUAL TOTAL ROUTINE FOR MODULE
                    gosub L17000          /* Rewrite G/L Detail         */
                    gosub L15310          /* Subtotal for Subtotal2     */
                    gosub L15395          /* Subtotal for Subtotal1     */
                    if sortdebits = 0 and sortcredits = 0 then return
                    printline% = printline% + 5
                    prttotaldescr$ = "** MODULE "&modno$&" TOTAL **"
                    if str(sortflag$(),3,2) <> " " then                  ~
                        gosub'80(sortdebits, sortcredits)                ~
                        else gosub'60(sortdebits, sortcredits)
                    bigdebits = bigdebits + sortdebits
                    bigcredits = bigcredits + sortcredits
                    totaldebits, totalcredits, sub1debits, sub1credits,  ~
                        sortdebits, sortcredits = 0
                    printline% = 100%
                    return

L15250:         REM TOTAL FOR REPORT
                     gosub L15050
                     if bigdebits  = 0 and bigcredits = 0 then L15285
                     prttotaldescr$ = "** GRAND TOTALS - MONTH OF " &    ~
                          mdate$(zemonth%)
                     printline% = 100%
                     gosub'80(bigdebits, bigcredits)
                     if printline% + 2% > 59% then gosub heading
                     print skip ( 1)
                     print using L55617 /* END OF REPORT */
L15285:              call "FILEBGON" (#04)
                     close printer
                     call "SETPRNT" (rptid$, " ", 0%, 1%)
                     f2%(4%) = 1
                     printline% = 100%
                     return

L15310:         REM SUBTOTAL FOR SUBTOTAL2
                     if totaldebits = 0 and totalcredits = 0 then L15385
                     if sortflag$(4) = " " then L15365
                     prttotaldescr$ = "** POSTING TOTAL **"
                     printline% = printline% + 3%
                     gosub'60(totaldebits,totalcredits)
                     if sortflag$(1) <> " " then L15365
                     if sortflag$(4) <> " " then headflag% = 0
L15365:              sub1debits = sub1debits + totaldebits
                     sub1credits= sub1credits + totalcredits
                     totaldebits, totalcredits = 0
L15385:              return

L15395:         REM SUBTOTAL FOR SUBTOTAL1
                     if sub1debits = 0 and sub1credits = 0 then L15485
                     if sortflag$(3) = " " then L15465

                     prttotaldescr$ = "** JOURNAL "&jnlid$&" TOTAL **"
                     printline% = printline% + 4%
                     if sortflag$(4) <> " " then                         ~
                           gosub'80(sub1debits,sub1credits)              ~
                        else gosub'60(sub1debits,sub1credits)
                     if sortflag$(3) <> " " then headflag% = 0
L15465:              sortdebits = sortdebits + sub1debits
                     sortcredits= sortcredits + sub1credits
                     sub1debits, sub1credits = 0
L15485:              return


        deffn'60(subdebits,subcredits)

               if printline% > 55 then gosub heading
               print using L56120
               print using L55480, prttotaldescr$, subdebits, subcredits
               print using L55360
               pageflag% = 1%
               return

        deffn'70(subdebits,subcredits)

               subtotal = subdebits - subcredits
               if subtotal < 0 then L15780
               prtdebits = subtotal : prtcredits = 0
               goto L15790
L15780:        prtdebits = 0 : prtcredits = -1 * subtotal
L15790: REM    CONVERT PRTDEBITS  TO PRTDEBITS$, PIC(-#########.##)
        REM    CONVERT PRTCREDITS TO PRTCREDITS$, PIC(-#########.##)
               call "CONVERT" (prtdebits, 2.2, prtdebits$)
               call "CONVERT" (prtcredits, 2.2, prtcredits$)
               if prtdebits  = 0 and prtcredits   = 0 then L15840
               if prtdebits  = 0 then prtdebits$  = " "
               if prtcredits = 0 then prtcredits$ = " "
L15840:        if prtdebits  = 0 and prtcredits = 0 then prtcredits$ = " "
               if printline% > 59 then gosub heading
               print using L56090
               print using L55504, prttotaldescr$, prtdebits$, prtcredits$
               print using L56090
               pageflag% = 1%
               return

        deffn'80(bigprtdebits, bigprtcredits)
            if bigprtdebits = 0 and bigprtcredits = 0 then return
            if printline% > 55% then gosub heading
            print using L55384
            print using L55528, prttotaldescr$,bigprtdebits,bigprtcredits
            print using L55384
            print using L55360
            pageflag% = 1%
            return

L16000: REM ERROR ROUTINE FOR NO WORKFILE OR WORKFILE FULL

            errormsg$ = "Workfile Not Opened or Workfile Full/Corrupted"
            call "FILEBGON" (#04)
            f2%(4%) = 1
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto L10090

L17000: REM *************************************************************~
            *               R E W R I T E  G L D E T A I L              *~
            *                                                           *~
            * Rewrite GLDETAIL from stack.                              *~
            *************************************************************

                text$ = "SUMMARY"
                str(text$, 31) = "SUMMARY"
                str(text$, 69) = title$

                rcpptr% = 0%
                if debitsptr% = 0 then L17405
L17280:         rcpptr% = rcpptr% + 1
                gldebit = round(debitsstk(rcpptr%),2)
                sumacct$=debitsstk$(rcpptr%)
                dateposted$ = debitdatestk$(rcpptr%)
                seqnr% = debitseqstk%(rcpptr%)
                pstseq% = debitpseqstk%(rcpptr%)
                if sumacct$ = "OVERFLOW" then                            ~
                   str(text$,1,30) = "OVERFLOW"
                glcredit = 0
                write #detl%, using L30130,                               ~
                          sumacct$, dateposted$, seqnr%,  modno$,        ~
                          gldebit, glcredit, str(text$, 1, 30),          ~
                          str(text$, 31, 34), str(text$, 65),            ~
                           jnlid$,  pstseq%, userid$, date
                if rcpptr% < debitsptr% then L17280

L17405:         rcpptr% = 0%
                if creditsptr% = 0 then L17550
L17420:         rcpptr% = rcpptr% + 1
                glcredit = round(creditsstk(rcpptr%),2)
                sumacct$=creditsstk$(rcpptr%)
                dateposted$ = creditdatestk$(rcpptr%)
                seqnr% = creditseqstk%(rcpptr%)
                pstseq% = creditpseqstk%(rcpptr%)
                if sumacct$ = "OVERFLOW" then                            ~
                   str(text$,1,30) = "OVERFLOW"
                gldebit = 0
                write #detl%, using L30130,                               ~
                          sumacct$, dateposted$, seqnr%,  modno$,        ~
                          gldebit, glcredit, str(text$, 1, 30),          ~
                          str(text$, 31, 34), str(text$, 65),            ~
                           jnlid$,  pstseq%, userid$, date
                if rcpptr% < creditsptr% then L17420

L17550:         gosub L18000
                return

L18000: REM *************************************************************~
            *             I N I T I A L I Z E  S T A C K                *~
            *                                                           *~
            * Initializes Account summary stacks                        *~
            *************************************************************

            init(hex(ff)) debitsstk$(), creditsstk$()
            mat debitsstk = zer
            mat debitseqstk% = zer
            mat creditsstk = zer
            mat creditseqstk% = zer
            debitsptr% = 0%
            creditsptr% = 0%
            return


        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *        D E T A I L  P L O W  R O U T I N E                *~
            *************************************************************
                    test% = 16%          /* TEST FOR NORMAL PLOW       */
                    goto L30080
L30050:             call "READ101"  (#detl%, oldreadkey$, f1%(detl%))
                         goto L30090
L30080:             call "PLOWNEXT" (#detl%,oldreadkey$,test%,f1%(detl%))
L30090:                  if f1%(detl%) = 0 then return
                    get #detl% using L30130, account$,                    ~
                                         prtposted$, seq%, modno$, debit,~
                    credit, ref1$, ref2$, descr$, jnlid$, pstseq%

L30130:                     FMT CH(16),            /* SKIP ACCOUNT     */~
                                CH(6),             /* MODULE DATE POSTD*/~
                                BI(4),             /* SKIP SEQ#        */~
                                CH(2),             /* TYPE CODE INFO   */~
                                2*PD(14,4),        /* DEBITS, CREDITS  */~
                                CH(30),            /* REF TEXT STRING  */~
                                CH(34),            /* REF 2 TEXT       */~
                                CH(36),            /* DESCRIPTION      */~
                                CH(3),             /* JOURNAL ID       */~
                                BI(4),             /* SEQUENCE POST NUM*/~
                                CH(3),             /* USERID OF CREATOR*/~
                                CH(6)              /* SYSTEM DATE      */

                    return

L41000: REM *************************************************************~
            *        I N P U T   C H O I C E   O F  MONTHS/JOURNALS     *~
            *                                                           *~
            * INDICATE WHICH MONTHS AND JOURNALS YOU DESIRE             *~
            *************************************************************
            deffn'201(fieldnr%)
               pfmessage$ = "(16)EXIT PROGRAM"
               goto L41029

            deffn'221(fieldnr%)
               pfmessage$ = " "

L41029:     blankline$ = " "
L41030:     accept                                                       ~
               at (01,02),                                               ~
                  "Summarize General Ledger Detail: Input Selection",    ~
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
               at (19,02), "Summary Level",                              ~
               at (19,16), fac(hex(81)), sum$                   , ch(03),~
               at (19,20), "[Enter JNL or SEQ]",                         ~
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
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (24,65), fac(hex(8c)),  pfmessage$             ,ch(16),~
                                                                         ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41500
                  call "MANUAL" ("GLDETSUM")
                  goto L41030

L41500:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41030

        REM *************************************************************~
            *          E D I T   C H O I C E   O F  MONTHS/JOURNALS     *~
            *                                                           *~
            * INDICATE WHICH MONTHS AND JOURNALS YOU DESIRE             *~
            *************************************************************
            deffn'211(fieldnr%)

            blankline$ = "Press RETURN to edit selection"
L42080:     accept                                                       ~
               at (01,02),                                               ~
                  "Summarize General Ledger Detail: Edit Selection",     ~
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
               at (19,02), "Summary Levels",                             ~
               at (19,16), fac(hex(84)), sum$                   , ch(03),~
               at (19,20), "[Enter JNL or SEQ]",                         ~
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
                  "(16)SUMMARIZE",                                       ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L43020
                  call "MANUAL" ("GLDETSUM")
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
                   errormsg$ = "Please Choose At Least One Month"
                   return
L50120:         temp = pos(str(chojrn$(), 1) <> " ")
                if temp <> 0 then L50165
                   errormsg$ = "Please Choose At Least One Module"
                   return

L50165: REM Test to see if dual books is in effect and which set to use
                set = 1 : main% = 1% : detl% = 2%     /* Set defaults */
                if dual_books$ <> "Y" then goto L50211
                call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
                if errormsg$ = " " then goto L50205
                    errormsg$ = "G/L System code must be '1'(Statutory)"&~
                        " or '2'(Local Authority)"
                    return
L50205:         gosub gl_set_setup

L50211:         if sum$ = "JNL" then L50220
                if sum$ = "SEQ" then L50220
                   errormsg$ = "Answer 'JNL' or 'SEQ' for Summary Printin~
        ~g"
                return
L50220:         if sum$ = "SEQ" then sortflag$() = " XXX"                ~
                   else sortflag$() = " XX "
                if sum$ = "SEQ" then rpttype$ = "POSTING SEQUENCE"       ~
                   else rpttype$ = "JOURNAL"

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
                    if firstacct$(temp%) <> "ALL" then L50420
                       init(hex(00)) firstacct$(temp%)
                       init(hex(ff)) lastacct$(temp%)
                       goto L50550
L50420:         REM HANDLES CASE FOR SINGLE JOURNALS
                    if lastacct$(temp%) <> " " then L50460
                       lastacct$(temp%) = firstacct$(temp%)

L50460:         REM HANDLES CASE FOR A RANGE OF JOURNALS
                    if lastacct$(temp%) < firstacct$(temp%) then L50510
                       firstacct$(temp%) = firstacct$(temp%) addc        ~
                                           all(hex(ff))
                       goto L50550
L50510:         REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                    errormsg$ = "Invalid Range For " & transhead$(temp%) ~
                              & " Module.  Please Respecify"
                    return
L50550:      next temp%
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
            * PAGE HEADER CONTROLLER ROUTINE                            *~
            *************************************************************

        heading
            select printer (134)
            if pageno% <> 0% then goto L55130
                time$ = " " : call "TIME" (time$)
                call "SETPRNT" (rptid$, " ", 0%, 0%)
                gosub L55136
                print skip (2)
                print using L55377, "R E P O R T   S E L E C T I O N S:"
                print
L55113:         i% = pos(str(i$()) > hex(7f))
                if i% = 0% then goto L55120
                     str(i$(), i%, 1%) = " "
                     goto L55113
L55120:         for x% = 4% to 20%
                    print using L55377, i$(x%)
                next x%
L55130:     if pageflag% = 0% then print using L55360
L55136:     print page
            pageno% = pageno% + 1
            print using L55256, date$, time$, compname$, "-" & rptid$
            print using L55280, rpttype$, pageno%
            print using L55273, sethdr$
            print using L55312, mdate$(zemonth%)
            print
            search transtypes$() = modno$ to look%() step 2
            if look%(1) = 0 then L55224
            modno% = (look%(1) + 1)/2
            if from$(modno%) = "ALL"  then print using L55328,  modno$    ~
                else                                                     ~
            print using L55344, from$(modno%), to$(modno%), modno$
L55224:     print using L55360
            printline% = 9
            return

L55256: %RUN: ######## @ ########            ############################~
        ~################################                     GLDETSUM####~
        ~###
L55273: %                                    ############################~
        ~################################
L55280: %                              LISTING OF GENERAL LEDGER DETAIL R~
        ~ECORDS SUMMARIZED BY ################                    PAGE: ##~
        ~###

L55312: %                                                       MONTH OF ~
        ~###############
L55328: % ALL JOURNALS FOR MODULE ##

L55344: % JOURNALS ### TO ### FOR MODULE ##

L55360: %================================================================~
        ~=================================================================~
        ~===
L55377: %                          ######################################~
        ~##########################################
L55384: %!                                                               ~
        ~                                                                 ~
        ~  !
L55408: %!  ACCOUNT CODE: ##########  DESCRIPTION: ######################~
        ~##############                                                   ~
        ~  !
        %!######## ## ### ######################### #####################~
        ~#### #################################!#############!############~
        ~# !
L55456: %!############ ######## ## ### ######################### ########~
        ~################# ####################!#############!############~
        ~# !
L55480: %! ########################################                      ~
        ~                                      !-#########.##!-#########.#~
        ~# !
L55504: %! ########################################                      ~
        ~                                      !-#########.##!-#########.#~
        ~# !
L55528: %! ########################################                      ~
        ~                                       -#########.## -#########.#~
        ~# !
L55552: %! MODULE NUMBER: ##  JOURNAL ID: ###  POSTING SEQUENCE: ########~
        ~##  TITLE: ########################################              ~
        ~  !
L55576: %! MODULE NUMBER: ##  JOURNAL ID: ###  TITLE: ###################~
        ~#####################                                            ~
        ~  !
L55600: %! MODULE NUMBER: ##                                             ~
        ~                                                                 ~
        ~  !
L55617: %                                            - - -  E N D   O F  ~
        ~ R E P O R T - - -

        sub_heading


            call "JNLINFO" (wmodno$, wjnlid$, 0%, " ", title$, " ", #03, ~
                            f2%(3), 1%)
            call "DESCRIBE" (#main%,account$,description$,0%,f1%(main%))
            printline% = printline% + 7%
            if printline% > 55 then gosub heading
            print using L55384
            if sortflag$(1) <> " " then L55800
            sortflag% = pos(-sortflag$()<>(hex(20)))
            on sortflag% gosub L55800, L55768, L55752, L55736
            goto L55808
L55736:     print using L55552, wmodno$, wjnlid$, wpstseq%, title$
            return
L55752:     print using L55576, wmodno$, wjnlid$, title$
            return
L55768:     print using L55600, wmodno$
            return
            print using L55408, account$, description$
            return
L55800:     print using L55408, account$, description$
L55808:     print using L55384
            print using L55872
            if sum$ = "YES" then L55848
            print using L56030
            print using L56060
L55848:     print using L56090
            return

L55872: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+
L56030: %!  ACCOUNT   !  DATE  !MD!JNL!     REFERENCE 1         !      RE~
        ~FERENCE 2        !    DESCRIPTION     !    DEBIT    !    CREDIT  ~
        ~  !
L56060: %+---------------------------------------------------------------~
        ~--------------------------------------+-------------+------------~
        ~--+
L56090: %!                                                               ~
        ~                                      !             !            ~
        ~  !
L56120: %!                                                               ~
        ~                                      !-------------!------------~
        ~--!
        %!                                                               ~
        ~                                       ------------- ------------~
        ~--!

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  if abs(amount) < .001 then return
                  search str(debitsstk$(),1) = str(account$)             ~
                               to location$ step 9 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L57160  /* PUSH NEW ITEM*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH CELL?  */
                     debitsstk(junk%) = debitsstk(junk%) + amount
                     debitpseqstk%(junk%) = pstseq%
                     debitseqstk%(junk%) = seq%
                     debitdatestk$(junk%) = dateposted$
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L57160:           REM PUSH NEW ITEM ONTO STACK.
                      if debitsptr% < dim(debitsstk$(),1) then L57220
                         debitsstk$(debitsptr%) = "OVERFLOW"
                         debitsstk(debitsptr%) =                         ~
                         debitsstk(debitsptr%) + amount
                         return
L57220:               debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      debitdatestk$(debitsptr%) = dateposted$
                      debitsstk(debitsptr%) = amount
                      debitseqstk%(debitsptr%) = seq%
                      debitpseqstk%(debitsptr%) = pstseq%
                      return

            deffn'163(account$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  if abs(amount) < .001 then return
                  search str(creditsstk$(),1) = str(account$)            ~
                               to location$ step 9
                               /* SCAN ONLY WHAT IS USED OF STACK      */
                  if location$ = hex(0000) then L57370  /* IF NO ==> NEW*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH ELEMENT*/
                     creditsstk(junk%) = creditsstk(junk%) + amount
                     creditseqstk%(junk%) = seq%
                     creditpseqstk%(junk%) = pstseq%
                     creditdatestk$(junk%) = dateposted$
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L57370:           REM PUSH NEW ENTRY ONTO STACK.
                      if creditsptr% < dim(creditsstk$(),1) then L57430
                         creditsstk$(creditsptr%) = "OVERFLOW"
                         creditsstk(creditsptr%) =                       ~
                         creditsstk(creditsptr%) + amount
                         return
L57430:               creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      creditdatestk$(creditsptr%) = dateposted$
                      creditsstk(creditsptr%) = amount
                      creditseqstk%(creditsptr%) = seq%
                      creditpseqstk%(creditsptr%) = pstseq%
                      return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            call "FILEBGON" (#04)
            end
