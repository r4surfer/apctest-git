        REM *************************************************************~
            *                                                           *~
            *   GGG   L      DDDD    SSS   PPPP   L      Y   Y          *~
            *  G      L      D   D  S      P   P  L      Y   Y          *~
            *  G GGG  L      D   D   SSS   PPPP   L       YYY           *~
            *  G   G  L      D   D      S  P      L        Y            *~
            *   GGG   LLLLL  DDDD    SSS   P      LLLLL    Y            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLDSPLY  - Displays General Ledger Accounts. Lets us      *~
            *            print summaries and details for them.  Will    *~
            *            print in background too.                       *~
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
            * 06/06/80 ! ORIGINAL                                 ! BCW *~
            * 09/07/82 ! REDEFINE GLMAIN TO INCL FWD & 13TH MONTHS! ECR *~
            * 11/09/82 ! CLEANUP, SALES ACCOUNT FLAG              ! ECR *~
            * 07/27/83 ! ADDED OPTIONS TO SPECIFY ENDING DATE RANG! HES *~
            * 02/02/84 ! READ100 instead of READ101 on "FISCAL..."! ECR *~
            * 07/22/85 ! GLDETAIL file layout change, ext. acct # ! HES *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 09/09/86 ! Fixed bug - can now print Detail for a   ! LDJ *~
            *          !   given account.                         !     *~
            * 05/21/87 ! Obsolete Flag In GLMAIN, ergo overhaul   ! HES *~
            * 09/10/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/09/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 08/23/88 ! PRR 10111- fix date range end default.   ! JIM *~
            * 09/30/88 ! PRR 10226- fix test on F1% for 2 books.  ! JDH *~
            * 10/14/88 ! Now report & display honor the End date, ! JDH *~
            *          !   Added PF 3 to display to show Totals,  !     *~
            *          !   Standards Cleanup.                     !     *~
            * 10/17/88 ! Now blank end date means end of century. ! JDH *~
            * 06/05/89 ! Corrected display of dates on month open ! MLJ *~
            *          !   = 16 for a 12 period year.             !     *~
            * 01/15/90 ! R6 QC rework tweeks.                     ! JDH *~
            * 09/25/91 ! PRR 11185 Enable PF(13)Instructions key. ! JIM *~
            * 09/25/91 ! PRR 11150 Field sizes expanded best way. ! JIM *~
            * 09/25/91 ! PRR 11489 Field sizes expanded best way. ! JIM *~
            * 09/25/91 ! PRR 11581 Field sizes expanded best way. ! JIM *~
            * 09/25/91 ! PRR 11712 sticky cursor on dtl toggles.  ! JIM *~
            * 09/26/91 ! PRR 11755 retain values to avoid reentry.! JIM *~
            * 11/12/91 ! R60102 QC Rework- enlarged NET$ to 14.   ! JIM *~
            * 12/09/91 ! Added the ability to Display/Print the   ! SID *~
            *          !    GL Detail off of the Archived File.   !     *~
            * 09/16/93 ! PRR 13021 Print larger seq #s if needed. ! JDH *~
            * 02/22/94 ! PRR 13105 Corrected PF5 functionality on ! MLJ *~
            *          !    toggled detail screen.                !     *~
	    * 06/24/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim acct$9,                      /* ENCRYPTED ACCOUNT          */~
            account$16,                  /* ACCOUNT NUMBER TO DISPLAY  */~
            activity(32),                /* NET ACTIVITY AMOUNTS       */~
            arcyear$7,                   /* Archived Year Litteral     */~
            choice$4,                    /* PICKYEAR return choice     */~
            coname$60,                                                   ~
            cursor%(2),                  /* Cursor location for edit   */~
            dates$(32)8,                 /* FISCAL DATE STRUCTURE      */~
            description$32,              /* ACCOUNT DESCRIPTION        */~
            dual_books$1,                /* Dual books in effect?      */~
            errormsg$79,                 /* ERROR MESSAGE TEXT.        */~
            fac$(2),                     /* FACs                       */~
            fileid$4,                    /* File ID for PICKYEAR       */~
            firstaccount$16,             /* FIRST G/L ACCT IN RANGE PRT*/~
            firstacct$9,                 /* FIRST G/L ACCT IN RANGE PRT*/~
            gldeprname$8,                /* Current GLDETAIL File Name */~
            grandnet$14,                 /* NET GRAND TOTAL            */~
            history$(17)13,              /* HISTORY AMOUNTS 12 MONTHS  */~
            i$(24)80,                    /* Screen Image               */~
            lastaccount$16,              /* LAST ACCOUNT IN RANGE PRINT*/~
            lastacct$9,                  /* LAST ACCOUNT IN RANGE PRINT*/~
            line$(2,20)79,               /* LINE ITEMS STRING.         */~
            line1$79,                    /* First Line of Screen Header*/~
            line2$79,                    /* Second Line of Screen Headr*/~
            linenumber%(2),              /* LINE COUNTER FOR STUFF     */~
            month$(17)08,                /* MONTH DESCRIPTION PROMPTS  */~
            newreadkey$50,               /* NEW KEY FOR PLOW ROUTINE   */~
            net$14,                      /* NET ACCOUNT TOTAL          */~
            olddate$8,                                                   ~
            oldreadkey$50,               /* KEY FOR PLOW ROUTINE       */~
            period$(2)4,                 /*  DISPLAY PERIODS           */~
            period%(2),                  /*  Current & Next PERIOD     */~
            pf4fac$1, pf4key$1, pf4msg$22,/* PF(4) controls            */~
            pf9fac$1, pf9key$1, pf9msg$24,/* PF(9) controls            */~
            pf4on$1,                     /* Can PF4 be enabled?        */~
            prevaccount$16,              /* PREV ACCOUNT MANAGED       */~
            printmessage$79,             /* PRINT MESSAGE TEXT.        */~
            prtcredit$13,                /* CREDITS FOR PRINT          */~
            prtdate$8,                   /* DATE CREATED               */~
            prtdebit$13,                 /* DEBITS FOR PRINT           */~
            prtdescr$32,                 /* FOR DISPLAY                */~
            prtgcredit$14,               /* TOTAL CREDITS FOR PRINT    */~
            prtgdebit$14,                /* TOTAL DEBITS FOR PRINT     */~
            prtjnl$3,                    /* JOURNAL FOR PRINT          */~
            prtmod$2,                    /* MODULE FOR PRINT           */~
            prtposted$8,                 /* DATE POSTED (FORMATTED)    */~
            prtseq$5,                    /* POST SEQUENCE (FORMATTED)  */~
            prttypedescr$20,             /* DESCRIPTION OF ACCT TYPE   */~
            prttext$36,                  /* REF TEXT PRINT PROGRAM     */~
            prtuserid$3,                 /* USERID OF DETAIL CREATOR   */~
            ref1$30,                     /* REFERENCE 1 TEXT           */~
            ref2$34,                     /* REFERENCE 2 TEXT           */~
            rptid$6,                                                     ~
            sales$1,                     /* SALES ACCOUNT FLAG         */~
            set$1, setdescr$30, sethdr$60,/* Set of books to use       */~
            setmsg$18,                   /* Screen message for SET     */~
            startdate$10,                /* DATE FOR START OF DETAIL   */~
            starting$10,                 /* DATE FOR INPUT SCREENS     */~
            statdescr$32,                /* Account Obsolete Desription*/~
            sticky$(20)1,                /* Sticky FACs go here        */~
            enddate$10,                  /* DATE FOR END OF DETAIL     */~
            ending$10,                   /* DATE FOR INPUT SCREENS     */~
            time$8,                                                      ~
            title$(2)79,                 /* TITLE FOR DETAIL DISPLAY   */~
            tamt$10,                     /* TRANSACTION AMOUNT RANGE   */~
            tdc$1,                       /* DEBIT/CREDIT RANGE         */~
            tmod$2,                      /* MODULE NUMBER RANGE        */~
            ttyp$2,                      /* TRANSACTION AMOUNT COMP FLG*/~
            tusr$3,                      /* USER ID RANGE              */~
            type$1,                      /* ACCOUNT TYPE               */~
            typedescr$15,                /* ACCOUNT TYPE DESCRIPTION   */~
            types$(6)15                  /* ACCOUNT TYPE DESCRIPTIONS  */

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                    S E L E C T   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #01 ! GLMAIN   ! General ledger main file                 *~
            * #02 ! GLDETAIL ! General ledger detail file               *~
            * #03 ! SYSFILE2 ! System information (months open list)    *~
            * #11 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #12 ! GLDETAL2 ! G. L. detail records for local authority *~
            *************************************************************

            select #01, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1,                                      ~
                        keylen = 9

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

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#02, 0%, f2%, 0%, " ")
            call "OPENCHCK" (#03, 0%, 0%, 0%, " ")
            dual_books$ = "N" :  set = 1             /* Default to 'no' */
            call "READ100" (#03, "SWITCHS.GL", f1%(3))
                if f1%(3) = 0% then goto L09000
            get #03 using L02360, dual_books$
L02360:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#12, 0%, 0%, 0%, " ")

L09000: REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * INITIALIZES CONTROL VARIABLES.                            *~
            *************************************************************

            if dual_books$ =  "Y" then setmsg$ = "G/L System to use:"
            date$ = date
            call "DATEFMT" (date$)
            rptid$ = "G/L011"
            call "COMPNAME" (12%, coname$, u3%)
            arcyear$ = "Current"

            REM READ IN THE FISCAL DATE STRUCTURE OUT OF SYSFILE2.
                call "READ100" (#03, "FISCAL DATES", f1%(3))
                  if f1%(3) <> 0 then L09200
                        call "ASKUSER" (2%, "*** FISCAL DATE ERROR ***", ~
                        "FISCAL DATES record in SYSFILE2 was not found", ~
                        " ","Press enter to acknowledge and exit program")
                       end

L09200:          get #03, using L09210, periods%, monthopen%, dates$()
L09210:                  FMT XX(20), BI(2), XX(136), BI(2), 32*CH(8)
                 for p% = 1% to 32%
                     if p% <> 15% then call "DATEFMT" (dates$(p%))
                 next p%
                 if monthopen% > 15 then L09280
                 p%=16 : if monthopen% = 12 and periods% = 12 then p%=17
                 str(dates$(),(monthopen%+p%)*8+1)=" "
L09280:          if periods% = 13 then L09330
*               IF MONTHOPEN% > 15 THEN 9330
                 copy str(dates$(), 14*8+1) to str(dates$(), 13*8+1)
                 copy str(dates$(), 27*8+1) to str(dates$(), 26*8+1)
                 dates$(31) = " " : dates$(32) = " "

L09330:     mat read types$
            data "$CASH IN BANK",                                        ~
                 "AASSET       ",                                        ~
                 "LLIABILITY   ",                                        ~
                 "CCAPITAL     ",                                        ~
                 "RREVENUE     ",                                        ~
                 "EEXPENSE     "

            p% = 0%
            main% = 1% : detl% = 2%
            call "EXTRACT" addr ("TT", task$)
            if str(task$,1,1) = "B" then L13000     /* BACKGROUND PROC  */
            lo% = 1% : hi% = 2%
            pf4on$  = hex(ff)
            if dual_books$ <> "Y" then goto L09470
                pf4msg$ = "(4)Select set of books"
                pf4on$  = hex(04)
L09470:     pf9msg$ = "(9)Print Account Details"
            goto L10080

L10000: REM *************************************************************~
            * G E T   A C C O U N T   N U M B E R   T O   D I S P L A Y *~
            *                                                           *~
            * GETS AN ACCOUNT NUMBER TO DISPLAY, PERMITS US TO PRINT 'EM*~
            *************************************************************

            lo%, hi% = 2%
            call "ALLFREE"
L10080:     errormsg$ = " " : see% = 1%
            if p% = 0% then prevaccount$ = account$
            account$, sales$ = " "
            gosub set_values
            if dual_books$ <> "Y" then set$ = " "
            p%, printmode% = 0%

            for f% = lo% to hi%
                if f% <> 1% then goto L10160
                    pf9fac$ = hex(9c) : pf9key$ = hex(ff)
                    pf4fac$ = hex(9c) : pf4key$ = hex(ff) : goto L10170
L10160:         pf4fac$ = hex(8c) : pf4key$ = pf4on$
                pf9fac$ = hex(8c) : pf9key$ = hex(09)
L10170:     gosub'200(f%)                /* GET ACCOUNT NUMBER         */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  4 then goto L10210
                      lo% = 1% : hi% = 2% : goto L10080
L10210:           if keyhit%  =  9 then goto printmode
                  if keyhit%  = 14 then select_archive_year
                  if keyhit%  = 16 then goto L65000
                  if keyhit% <>  0 then goto L10170
            gosub'150(f%)
                  if errormsg$ <> " " then goto L10170
            next f%

            convert set to set$, pic (#)

L11000: REM *************************************************************~
            *   C O M P U T E   S U M M A R Y   I N F O R M A T I O N   *~
            *                                                           *~
            * COMPUTE SUMMARY INFORMATION FROM THE GLMAIN FILE INFO.    *~
            *************************************************************

            get #main%, using L11090, acct$, description$, type$, ob%,    ~
                activity()

L11090:             FMT CH(9),           /* SKIP ACCOUNT NUMBER        */~
                        CH(30),          /* DETAIL SEQUENCE NUMBER     */~
                        CH(1),           /* ACCOUNT TYPE               */~
                        BI(1),           /* ACCOUNT STATUS             */~
                        XX(3),           /* SEQUENCE NUMBER FOR DETAILS*/~
                        32*PD(14,4)      /* Activity                   */

            REM FORMAT HEADER INFORMATION FOR SCREEN.
                call "PUTPAREN" (description$)
                statdescr$ = "(Active)"
                if ob%=1% then statdescr$=" (No New Usage Allowed)"
                if ob%=2% then statdescr$=" (No New Usage, Not Postable)"
                if str(statdescr$,,1)=" " then str(statdescr$,,1)=hex(94)
                REM NOW HANDLE DISPLAY FOR ACCOUNT TYPE.
                    search types$() = type$ to location$ step 15
                    if str(location$,1,2)=hex(0000) then L11290
                      typedescr$=str(types$((val(location$,2)-1)/15+1),2)
                      call "PUTPAREN" (typedescr$)

L11290:     REM NOW COMPUTE SCREEN DATE INFORMATION FOR Period HISTORY.  ~
                (CURRENT MONTH REFERS TO MONTH OPEN)

                init(" ") month$(), history$(), period$()

                if periods% = 12 and monthopen% = 12 then period%(1) =   ~
                   monthopen% + 2% else period%(1) = monthopen% + 1%
                period%(2) = monthopen%
                   for temp% = 1 to 2
                      convert period%(temp%) to period$(temp%), pic (##)
                      call "PUTPAREN" (period$(temp%))
                   next temp%

L11345:                                  %+#########.##
*        Establish beginning and ending elementd
             ce% = monthopen% + 15%  :   le% = ce% + 1%
             if monthopen% = 12% and periods% = 12% then le% = le% + 1%
             be% = ce% - 13%  :  accruedbal = 0
             if monthopen% = 12% and periods% = 12% then be% = be% + 1%

*        Calculate Accrued Balance
             for idx% = 1 to le%
                   accruedbal = round(accruedbal + activity(idx%), 2)
             next idx%
                put history$(1), using L11345, accruedbal
                if str(history$(1),8) = "+0.00" then                     ~
                   str(history$(1),8) = " 0.00"
                if periods% = 13% then L11440
                   if monthopen% < 12% then month$(1) = dates$(le% - 1%)
                   if monthopen% = 12% then month$(1) = dates$(le% - 2%)
                   if monthopen% > 12% then month$(1) = dates$(le% - 2%)
                   goto L11450
L11440:         month$(1) = dates$(le%)

L11450
*        Accrued Transactions
             put accruedperiod$, using L11345, activity(le%)
             if str(accruedperiod$,8) = "+0.00" then                     ~
                str(accruedperiod$,8) = " 0.00"

*        Current Balance
             put history$(2), using L11345, accruedbal - activity(le%)
             if str(history$(2),8) = "+0.00" then                        ~
                str(history$(2),8) = " 0.00"
             if periods% = 13% then L11515
                if monthopen% < 12% then month$(2) = dates$(le% - 2%)
                if monthopen% = 12% then month$(2) = dates$(le% - 3%)
                if monthopen% > 12% then month$(2) = dates$(le% - 3%)
                goto L11525
L11515:      month$(2) = dates$(le% - 1%)

L11525
*        Current Transactions
             if periods% = 13% then L11539
                if monthopen% < 12% then                                 ~
                   put currentperiod$, using L11345, activity(le% - 1%)
                if monthopen% = 12% then                                 ~
                   put currentperiod$, using L11345, activity(le% - 2%)
                if monthopen% > 12% then                                 ~
                   put currentperiod$, using L11345, activity(le% - 1%)
                   goto L11542
L11539:      put currentperiod$, using L11345, activity(le% - 1%)
L11542:      if str(currentperiod$,8) = "+0.00" then                     ~
                str(currentperiod$,8) = " 0.00"

*        Prior Balance
             if periods% = 12% and monthopen% = 12% then                 ~
                 activity =  activity(le%) + activity(le% - 2%) else     ~
                 activity =  activity(le%) + activity(le% - 1%)
                 prior_bal = accruedbal - activity
                 put history$(3), using L11345, prior_bal
                 per_bal = prior_bal
             if str(history$(3),8) = "+0.00" then                        ~
                str(history$(3),8) = " 0.00"
             if periods% = 13% then L11600
                if monthopen% < 12% then month$(3) = dates$(le% - 3%)
                if monthopen% = 12% then month$(3) = dates$(le% - 4%)
                if monthopen% > 12% then month$(3) = dates$(le% - 4%)
                goto L11610
L11600:      month$(3) = dates$(le% - 2%)

L11610
*        Build History
             if periods% = 13% then L11618
                if monthopen% < 12% then le% = le% - 2%
                if monthopen% = 12% then le% = le% - 3%
                if monthopen% = 14% then le% = le% - 3%
                if monthopen% > 14% then le% = le% - 2%
                goto L11620
L11618:      le% = le% - 2%
L11620:      he% = 4%

             for idx% = le% to be% step -1
              if periods% = 12% and (idx% = 14% or idx% = 28%) then L11660
                per_bal = per_bal - activity(idx%)
                put history$(he%), using L11345, per_bal
                if str(history$(he%),8) = "+0.00" then                   ~
                   str(history$(he%),8) = " 0.00"
                if periods% = 12% then L11642
                   month$(he%) = dates$(idx% - 1%)  :  goto L11655
L11642:         month$(he%) = dates$(idx% - 2%)
                if idx% > 28% then month$(he%) = dates$(idx% - 3%)
                if idx% < 15% then month$(he%) = dates$(idx% - 1%)
L11655:         he% = he% + 1%
L11660:      next idx%

           REM NOW GET SALES ACCOUNT FLAG
                call "READ100" (#03, "SALES" & set$ & acct$, f1%(3))
                     if f1%(3) <> 0 then sales$ = "Y" else sales$ = "N"

L11720:     REM NOW DISPLAY ALL THIS JUNK AND LET THE USER FIGURE OUT    ~
                    WHAT TO DO WITH IT.
L11740:         gosub L41000
                      if keyhit%  =  0 then goto L10000
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then goto L12000
                      if keyhit%  = 16 then goto L10000
                      if keyhit% <> 14 then goto L11740

                REM HANDLES FOR PRINT ACCOUNT FOR USER.
                    p%, printmode% = 1%
                    gosub L12000
                    printmode% = 0%
                    call "SHOSTAT" ("Printing Account Information")
                    jurnline%=1000
                    jurnpage%=0
                    prtdescr$ = account$
                    if set = 1                                           ~
                        then call "GLUNFMT" (prtdescr$)                  ~
                        else call "GLUNFM2" (prtdescr$)
                    gosub'250(prtdescr$,startdate$)
                    if set = 1                                           ~
                        then call "GLFMT" (account$)                     ~
                        else call "GLFMT2" (account$)
                    close printer
                    call "SETPRNT" (rptid$, " ", 0%, 1%)
                    goto L11000           /* REDISPLAY STUFF.           */

        set_values
            errormsg$ = " "
            if starting$ <> " " then goto L11965
                startdate$, starting$ = "ALL"
                enddate$, ending$ = " "
L11965:     if firstaccount$ <> " " then goto L11985
                firstaccount$ = "ALL"
                lastaccount$ = " "
L11985:     if tamt$ = " " then tamt$ = "0.00"
            if ttyp$ = " " then ttyp$ = "GE"
            if tdc$  = " " then tdc$  = "B"
            return

L12000: REM *************************************************************~
            *      P L O W   T H R O U G H   D E T A I L   F I L E      *~
            *                                                           *~
            * LOOKUP ROUTINE FOR DETAIL INFORMATION TO DISPLAY.         *~
            *************************************************************

            REM WHAT START DATE
                re_search:
                gosub set_values
L12065:         gosub L44000
                      if keyhit%  =  0 then gosub L12095
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then goto L10000
                      goto L12065

L12095:         if starting$ <> "ALL" then L12110
                     startdate$ = "19010101"
                     enddate$   = "20991231"
                     call "DATFMTC" (startdate$)
		     goto L12111
L12110:         startdate$ = starting$
                call "DATEOKC" (starting$, u3%, errormsg$)
                      if errormsg$ <> " " then return
L12111:         call "DATEOKC" (startdate$, u3%, errormsg$)
                      if errormsg$ <> " " then return
                if ending$ <> " " then goto L12125
                     enddate$   = "20991231"
                     call "DATFMTC" (enddate$)
		     goto L12126
L12125:         enddate$   = ending$
                call "DATEOKC" (ending$, u3%, errormsg$)
                      if errormsg$ <> " " then return
L12126:         call "DATEOKC" (enddate$, u3%, errormsg$)
                      if errormsg$ <> " " then return
                call "DATUFMTC" (enddate$)
                call "DATUFMTC" (startdate$)
                if startdate$ <= enddate$ then  L12175
                errormsg$ = "Start Date Can't Exceed End Date!"
		starting$ = startdate$
		call "DATEOK" (starting$, u3%, errormsg$)
		ending$ = enddate$
		call "DATEOK" (ending$, u3%, errormsg$)
                return

L12175:         if ttyp$="GE" or ttyp$="LE" or ttyp$="EQ" then L12195
                     errormsg$ = "Amount compare flag must be LE(Less or ~
        ~=), GE(Greater or =), or EQ(Equal To)."
                     return
L12195:         call "NUMTEST" (tamt$, 0, 9999999.99, errormsg$,2.2,tamt)
                      if errormsg$ <> " " then return
                if tamt = 0 and ttyp$ = "GE" then L12225
                     if tamt > 0 then L12225
                     errormsg$ = "No Amounts Less Then Or Equal To Zero"
                     return
L12225:         if tdc$ = " " then tdc$ = "B"
                if pos("BCD" = tdc$) > 0 then L12245
                errormsg$ = "Debit/Credit Flag Must Be 'D', 'C', or 'B'"
                return
L12245:         if tmod$ = " " then L12285
                prttypedescr$ = "MODULENO:" & tmod$
                str(prttypedescr$,12) = all(hex(00))
                call "PLOWNEXT" (#03, prttypedescr$, 11%, f1%(3))
                     if f1%(3) <> 0 then L12285
                errormsg$ = "Undefined Module Number"
                return

L12285:     return clear
            if printmode% = 1% then return
            call "SHOSTAT" ("Searching Detail File...")


            REM SET INFORMATION TO PLOW THROUGH DETAIL FILE.
                prttypedescr$ = type$
                str(prttypedescr$, len(prttypedescr$)+2)=typedescr$
                init(" ") line$()
                displayline%  = 1%
                displaytotal% = 0%
                init (hex(00)) oldreadkey$
                str(oldreadkey$,1,16) = str(acct$,,9)
                str(oldreadkey$,17,6) = str(startdate$,,6%)
                call "READ100" (#detl%, oldreadkey$, f1%(detl%))
                   if f1%(detl%) <> 1% then L12375
                   gosub get_last_gldetail
                   if bad% = 2% then L12485  /* DONE, End of Date range */
                   if bad% = 0% then L12395

L12375:     REM PLOW ROUTINE FOR DETAIL FILE.
                gosub load_next_gldetail
                if f1%(detl%) = 0% or bad% = 2% then L12485   /* DONE. */

L12395:     REM PRINT ENTRIES DISCOVERED IN DETAIL FILE.
                call "DATEFMT" (prtposted$)
                call "CONVERT" (debit, 2.2, prtdebit$)
                call "CONVERT" (credit, 2.2, prtcredit$)
                put line$(1,displayline%),using L12460,prtposted$,prtmod$,~
                    prtjnl$, prtdebit$ & prtcredit$, str(prttext$,5,32), ~
                    prtuserid$
                if pstseq% < 10000% then L12430
                     trunc_date$ = str(prtposted$,,6%) & hex(22) /* " */
                     put line$(2,displayline%),using L12474, trunc_date$, ~
                         pstseq%, ref1$, ref2$
                     goto L12440
L12430:         put line$(2,displayline%),using L12470,prtposted$,        ~
                    pstseq%, ref1$, ref2$
L12440:         gosub L20000              /* DISPLAY CONTROLLER         */
            REM GOTO NEXT ITEM IN FILE.
                goto L12375

L12460: %######## ## ### ########################## #####################~
        ~########### ###
L12470: %######## #### ############################## ###################~
        ~###############
L12474: %####### ##### ############################## ###################~
        ~###############
        %######## ## ### -####### ######################### #############~
        ~###############

L12485:     REM EXIT PLOW ROUTINE--GETS OUT, DISPLAYS UNFILLED ARRAY, ETC
                REM IF THERE'S STUFF YET TO DISPLAY, DO SO...
                if displayline% = 1% then gosub L12520 /* SORRY, NO DETL*/
                   displayline% = 1000%
                   gosub L20000           /* SCREEN DISPLAYER.          */
                   goto L10000

L12520:         REM ON EXIT, THIS SETS UP FOR NO DETAILS THIS ACCOUNT
                    init(" ")line$()
                    tempmsg$ = "NO DETAILS"
                    str(line$(1%,8),40-len(tempmsg$)/2)=tempmsg$
                    return

L13000: REM *************************************************************~
            *         P R I N T   F R O M   B A C K G R O U N D         *~
            *                                                           *~
            * THIS IS THE ROUTINE WE USE IF THE PROGRAM IS CURRENTLY    *~
            * RUNNING IN BACKGROUND MODE.  IT JUST PRINTS THE WHOLE FILE*~
            *************************************************************

            init(hex(00)) oldreadkey$
            jurnline% = 1000%
            jurnpage% = 0%

L13110:     call "READ102" (#main%, oldreadkey$, f1%(main%))
                 if f1%(main%) = 0% then goto L65000
            get #main%, using L13140, newreadkey$
L13140:             FMT CH(9)
            oldreadkey$ = newreadkey$
            gosub'250(oldreadkey$,startdate$)
            goto L13110

L16000: REM *************************************************************~
            *     P R I N T   A   R A N G E   O F   A C C O U N T S     *~
            *                                                           *~
            * PRINTS A RANGE OF GENERAL LEDGER ACCOUNTS.  NOTE THE STUFF*~
            * TO SKIP OUT AND PRINT ALL IN BACKGROUND MODE...           *~
            *************************************************************

        printmode
            p% = 1%
            gosub set_values
L16110:     gosub L43000
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto L10000
                  if keyhit% <>  0% then goto L16110
            gosub L51000                  /* NORMALIZE KEYS AFTER TEST  */
                  if errormsg$ <> " " then goto L16110

            REM SET UP FOR PLOW ROUTINE
                oldreadkey$ = firstacct$
                grandtotalcredits, grandtotaldebits = 0
                jurnline% = 1000%
                jurnpage% = 0%
                call "SHOSTAT" ("Printing General Ledger Detail Report")

L16240:     REM DO PLOW ROUTINE, PRINTING WHATEVER.
                call "READ102" (#main%, oldreadkey$, f1%(main%))
                     if f1%(main%) = 0 then L16340
                get #main%, using L16280, newreadkey$
L16280:                 FMT CH(9)
                if newreadkey$ > lastacct$ then L16340
                oldreadkey$ = newreadkey$
                gosub'250(newreadkey$,startdate$)
                goto L16240

L16340:    REM CLOSE THE PRINTER BEFORE THE NEXT PRINT
               print skip(1)
               net = grandtotaldebits - grandtotalcredits
               call "CONVERT" (net, 2.2, grandnet$)
               call "CONVERT" (grandtotaldebits, 2.2, prtgdebit$)
               call "CONVERT" (grandtotalcredits, 2.2, prtgcredit$)
               print using L18870, grandnet$, prtgdebit$, prtgcredit$
               close printer
               call "SETPRNT" (rptid$, " ", 0%, 1%)
               goto L16000

        REM *************************************************************~
            *      P R I N T   I N D I V I D U A L   A C C O U N T      *~
            *                                                           *~
            * PRINTS INDIVIDUAL ACCOUNT NUMBER DATA.  THIS ROUTINE IS   *~
            * CALLED FROM L. 16310 ABOVE.                               *~
            *************************************************************

            deffn'250(account$,startdate$)
                 subtotal%, subtag%, hits_this_date% = 0%
                 subtotaldebit,subtotalcredit,totaldebits,totalcredits=0
                 colsdone% = 0%
                 oldreadkey$ = all(hex(00))
                 str(oldreadkey$,,16) = account$
                 str(oldreadkey$,17,6) = startdate$
                 mat linenumber% = con
                 if jurnline% > 61% and jurnline% < 100%                 ~
                     then jurnline% = 1000%        /* TRIGGER PAGING   */

            REM GET ACCOUNT NUMBER WITH IMPORTANT STUFF ABOUT IT.
                call "READ100" (#main%, account$, f1%(main%))
                     if f1%(main%) = 0% then return

                get #main%, using L17230, description$
L17230:             FMT XX(9),           /* SKIP ACCOUNT NUMBER        */~
                        CH(30)           /* ACCCOUNT DESCRIPTION       */~

L17260:     REM CONTROL ROUTINE FOR BOTH COLUMNS.
                for column% = 1% to 2%
                    on column% gosub L17450, L17630
                next column%
                if colsdone% = 2% then L17390       /* SEPARATOR LINE   */
                   gosub L18450
                   print using L18800, prtdescr$, prtposted$, prtdate$,   ~
                               prtuserid$, prtseq$, prtmod$, prtjnl$,    ~
                               prttext$, prtdebit$, prtcredit$
                   lastline% = 0%        /* FLAG SO NO 2 SEP LINES AT  */
                                         /* END OF PAGE                */
                   goto L17260            /* NEXT COLUMN                */

L17390:         REM PRINT SEPARATOR LINE AND EXIT.
                    gosub L18450
                    if jurnline% > 7% then print using L18710
                    lastline% = 1%       /* SEPARATOR WAS LAST ONE PRTD*/
                    return

L17450:     REM PRINT FIRST COLUMN OF STUFF--NAME AND THAT SORT OF THING
                on linenumber%(1) gosub L17480, L17530, L17570
                   return
L17480:         REM FIRST CASE -- ACCOUNT NUMBER.
                    prtdescr$ = account$
                    if set = 1                                           ~
                        then call "GLFMT" (prtdescr$)                    ~
                        else call "GLFMT2" (prtdescr$)
                    linenumber%(1) = 2%
                    return
L17530:         REM SECOND CASE -- ACCOUNT DESCRIPTION
                    prtdescr$ = description$
                    linenumber%(1) = 3%
                    return
L17570:         REM THIRD CASE -- BLANK EVERYTHING OUT.
                    prtdescr$ = " "
                    linenumber%(1) = 4%
                    colsdone% = colsdone% + 1%
                    return

L17630:     REM PRINT SECOND COLUMN.
                on linenumber%(2) gosub L17660, L18390
                   return
L17660:         REM PLOW ROUTINE TO DO THAT STUFF UP RIGHT WITH.
                    if subtotal% = 0% then L17680
                       gosub get_last_gldetail
                       if bad% = 2% then L18170    /* End of Date Range */
                       if f1%(detl%) = 0% then L18170  /* End of Detail */
                       if bad% = 0% then L17880
L17680:             gosub load_next_gldetail
                    if f1%(detl%) = 0% or bad% = 2% then L18170
L17880:             subtotal% = 0%
                    if subtag% = 0% then olddate$ = str(prtposted$,,6)
                    subtag% = 1%
                    if olddate$ = str(prtposted$,,6) then L18040
        REM Date Changed, Print Sub Total, maybe
                    if hits_this_date% < 2% then L18020
                    subtotal% = 1%
                    net = subtotaldebit - subtotalcredit
                    call "CONVERT" (net, 2.2, net$)
                    call "DATEFMT" (olddate$)
                    put prttext$, using L18830, olddate$, net$
                    prtposted$, prtdate$, prtuserid$ = " "
                    prtmod$, prtjnl$, prtseq$ = " "
                    debit  = subtotaldebit
                    credit = subtotalcredit

L18020:             subtotaldebit, subtotalcredit = 0
                    hits_this_date% = 0%
                    olddate$ = str(prtposted$,,6)
L18040:             if prtdate$   <> " " then call "DATEFMT" (prtdate$)
                    if prtposted$ <> " " then call "DATEFMT" (prtposted$)
                    call "CONVERT" (debit , 2.2, prtdebit$)
                    call "CONVERT" (credit, 2.2, prtcredit$)
                    if subtotal% <> 0 then return

        REM Accumulate Totals for each detail line printed
                    convert abs(pstseq%) to prtseq$, pic (####0)
                    hits_this_date% = hits_this_date% + 1%
                    subtotaldebit   = round(subtotaldebit  + debit,  2)
                    subtotalcredit  = round(subtotalcredit + credit, 2)
                    totaldebits     = round( totaldebits   + debit,  2)
                    totalcredits    = round( totalcredits  + credit, 2)
                    return

L18170:         REM PRINT TOTALS
                    prttext$, prtdebit$, prtcredit$ = " "
                    if subtag% = 0% then L18270          /* No Detail */
                    if subtag% = 2% then L18300     /* Do Grand Total */
                    if hits_this_date% < 2% then L18300
                    net = subtotaldebit - subtotalcredit
                    call "CONVERT" (net, 2.2, net$)
                    call "DATEFMT" (olddate$)
                    put prttext$, using L18830, olddate$, net$
                    call "CONVERT" (subtotaldebit, 2.2, prtdebit$)
                    call "CONVERT" (subtotalcredit, 2.2, prtcredit$)
L18270:             subtag% = 2%
                    goto L18360

L18300:             net = totaldebits - totalcredits
                    call "CONVERT" (net, 2.2, net$)
                    call "CONVERT" (totaldebits , 2.2, prtdebit$)
                    call "CONVERT" (totalcredits, 2.2, prtcredit$)
                    put prttext$, using L18850, net$
                    grandtotaldebits  =                                  ~
                             round( grandtotaldebits  + totaldebits,  2)
                    grandtotalcredits =                                  ~
                             round( grandtotalcredits + totalcredits, 2)
                    linenumber%(2) = 2%
L18360:             prtposted$, prtdate$, prtuserid$ = " "
                    prtmod$, prtjnl$, prtseq$= " "
                    return
L18390:         REM ZAP EVERYTHING.
                    prtposted$, prtdate$, prtuserid$, prttext$, prtjnl$, ~
                    prtdebit$, prtcredit$, prtmod$, prtseq$ = " "
                    linenumber%(2) = 3%
                    colsdone% = colsdone% + 1%
                    return

L18450: REM PAGE CONTROL SUBROUTINE FOR THE WHOLE WORKS.
            jurnline% = jurnline% + 1%
                if linenumber%(1) < 3% and jurnline% > 52% then L18480
            if jurnline% < 54% then return
L18480:         select printer(134)
                if jurnpage% <> 0% then goto L18490
                    call "SETPRNT" (rptid$, " ", 0%, 0%)
                    time$ = " " : call "TIME" (time$)
L18490:         if lastline% = 0% and jurnpage% <> 0%                    ~
                    then print using L18710       /* SEPARATOR LINE   */
                print page
                jurnpage% = jurnpage% + 1%
                print using L18670, date$, time$, coname$, "-" & rptid$
                print using L18690, jurnpage%
                print using L18702, sethdr$
                print
                print using L18710
                print using L18740
                print using L18770
                print using L18710
                jurnline% = 8%
                prtdescr$ = account$
                if set = 1                                               ~
                    then call "GLFMT" (prtdescr$)                        ~
                    else call "GLFMT2" (prtdescr$)
                if linenumber%(1)=4% then colsdone%=max(0,colsdone%-1%)
                linenumber%(1) = 2%
                return

L18670: %RUN: ######## @ ########            ############################~
        ~################################                      GLDSPLY####~
        ~###
L18690: %                                      G E N E R A L   L E D G E ~
        ~R   D E T A I L   L I S T I N G                           PAGE: #~
        ~###
L18702: %                                    ############################~
        ~################################
L18710: %+------------------------------+--------+--------+---+-----+--+-~
        ~--+------------------------------------+-------------+-----------~
        ~--+
L18740: %!        ACCOUNT NUMBER        !  DATE  !  DATE  ! BY! POST!  ! ~
        ~  !                                    !        DEBIT!       CRED~
        ~IT!
L18770: %!         (DESCRIPTION)        ! POSTED ! CREATED!USR!  SEQ!MD!J~
        ~NL!    F R E E   T E X T   F I E L D   !       AMOUNT!       AMOU~
        ~NT!
L18800: %!##############################!########!########!###!#####!##!#~
        ~##!####################################!#############!###########~
        ~##!
L18830: %*** ######## TOTAL ***##############

L18850: %*** ACCOUNT TOTAL *** ##############

L18870: %         **** GRAND TOTALS ****           NET = ##############  ~
        ~           DEBITS = ##############        CREDITS = #############~
        ~#

        load_next_gldetail
L19010:     call "PLOWNEXT" (#detl%, oldreadkey$, 16%, f1%(detl%))
                if f1%(detl%) = 0 then return
        get_last_gldetail
            get #detl%, using L19070, prtposted$, prtmod$, debit, credit, ~
                          ref1$, ref2$, prttext$, prtjnl$, pstseq%,      ~
                          prtuserid$, prtdate$

L19070:                     FMT XX(16),            /* SKIP ACCOUNT     */~
                                CH(6),             /* MODULE DATE POSTD*/~
                                XX(4),             /* SKIP SEQ#        */~
                                CH(2),             /* MODULE CODE      */~
                                2*PD(14,4),        /* DEBITS, CREDITS  */~
                                CH(30),            /* REFERENCE 1      */~
                                CH(34),            /* REFERENCE 2      */~
                                CH(36),            /* DESCRIPTION      */~
                                CH(3),             /* JOURNAL ID       */~
                                BI(4),             /* POSTING SEQUENCE */~
                                CH(3),             /* USERID OF CREATOR*/~
                                CH(6)              /* SYSTEM DATE      */
            gosub test_gldetail
            if bad% = 1% then L19010
        return

        test_gldetail
            bad% = 0%
            REM Include/Exclude Logic...
            if tmod$ <> " " and prtmod$ <> tmod$ then bad% = 1%
            if tusr$ <> " " and prtuserid$ <> tusr$ then bad% = 1%
            temp = max(abs(debit), abs(credit))
            if ttyp$ = "GE" and temp < tamt then bad% = 1%
            if ttyp$ = "EQ" and temp <> tamt then bad% = 1%
            if ttyp$ = "LE" and temp > tamt then bad% = 1%
            if debit = 0 and tdc$ = "D" then bad% = 1%
            if credit = 0 and tdc$ = "C" then bad% = 1%
            if str(prtposted$,,6) > str(enddate$,,6) then bad% = 2%
        return

L20000: REM *************************************************************~
            *   D I S P L A Y   C O N T R O L   F O R   D E T A I L S   *~
            *                                                           *~
            *************************************************************

            displayline% = displayline% + 1%
            if displayline% <= 20% then return
               init (hex(8c)) sticky$() /* Nothing sticky so far */
*             SEE% = 1% /* Initialize the detail display */
L20070:        gosub'222                 /* AND JUST SHOW SCREEN AS IS */
                     if keyhit%  =  1% then gosub startover
                     if keyhit%  =  2% then       L20170
                     if keyhit%  =  3% then       L20500
                     if keyhit%  =  5% and displayline% < 1000%          ~
                                       then       L20200
                     if keyhit%  =  9% then       L11720
                     if keyhit%  = 14% then       L20300
                     if keyhit%  = 16% then       L10000
                     if keyhit%  <> 0% then       L20150
                          if see% = 1% then see% = 2% else see% = 1%
                          init (hex(8c)) sticky$()
                          if cursor%(1) < 5% then goto L20150
                          sticky$(cursor%(1)-4%) = hex(8e) /* Sticky */
L20150:              goto L20070          /* FOR STARTOVER, CRAZY KEYHIT*/

L20170:        REM HANDLES CASE WHERE WE WANT TO GO TO FIRST INVOICE
                   return clear
                   goto re_search
L20200:        REM HANDLES CASE WHERE HE WANTS NEXT PAGE (15 LINES)
                   displayline% = 2%     /* SET LINE COUNTER           */
                   line$(1,1) = line$(1,20)
                   line$(2,1) = line$(2,20)
                   for i% = 2 to 20
                        line$(1,i%),line$(2,i%) = " "
                   next i%
                   return
L20300:        REM HANDLES CASE WHERE HE WANTS TO PRINT THING (GOSUB)
                   jurnpage% = 0%
                   jurnline% = 10000%
                   displaytotal% = 0%
                   call "SHOSTAT" ("Printing Account Information")
                   prtdescr$ = account$
                    if set = 1                                           ~
                        then call "GLUNFMT" (prtdescr$)                  ~
                        else call "GLUNFM2" (prtdescr$)
                   gosub'250(prtdescr$,startdate$)
                   if set = 1                                            ~
                        then call "GLFMT" (account$)                     ~
                        else call "GLFMT2" (account$)
                   close printer
                   goto L20000

        REM SHOW TOTALS
L20500:         newreadkey$ = oldreadkey$
                if displaytotal% = 1% then L20700
                init (hex(00)) oldreadkey$
                net, totaldebits, totalcredits = 0
                str(oldreadkey$,1,16) = str(acct$,,9)
                str(oldreadkey$,17,6) = str(startdate$,,6%)
                call "READ100" (#2, oldreadkey$, f1%(2%))
                   if f1%(2%) <> 1% then L20620
                   gosub get_last_gldetail
                   if bad% = 2% then L20700  /* DONE, End of Date range */
                   if bad% = 0% then L20660

L20620:     REM PLOW ROUTINE FOR DETAIL FILE.
                gosub load_next_gldetail
                if f1%(detl%) = 0 or bad% = 2% then L20700   /* DONE. */

L20660:         totaldebits  = round( totaldebits + debit, 2)
                totalcredits = round( totalcredits + credit, 2)
                goto L20620

L20700:         displaytotal% = 1%
                net = totaldebits - totalcredits
                call "CONVERT" (net, -2.2, net$)
                call "CONVERT" (totaldebits, -2.2, prtdebit$)
                call "CONVERT" (totalcredits, -2.2, prtcredit$)
                keyhit% = 3%
                call "ASKUSER" (keyhit%, "*** TOTALS ***",               ~
                               "    Net = " & net$,                      ~
                               " Debits = " & prtdebit$,                 ~
                               "Credits = " & prtcredit$)
                oldreadkey$ = newreadkey$
                goto L20070


        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto L10000

        select_archive_year
            fileid$ = "GLDE"
            call "PICKYEAR" (fileid$, choice$)
              if f2% = 0 then close #02 /* Close Curr. Detail File */
              if choice$ = "CURR" or choice$ = " " then L30050 else L30060
L30050:       gldeprname$ = "GLDETAIL" : goto L30070
L30060:       gldeprname$ = "GLDE" & choice$
L30070:       call "PUTPRNAM" addr(#02, gldeprname$)
              call "OPENCHCK" (#02, 0%, f2%, 0%, " ")
              if choice$ = "CURR" or choice$ = " "                       ~
               then arcyear$ = "Current" else arcyear$ = choice$ & "   "
            goto L10170  /* Start from the Top */

        REM *************************************************************~
            *    I N P U T   A C C O U N T   N U M B E R   T O   D O    *~
            *                                                           *~
            * GETS ACCOUNT NUMBER TO BE DONE.  GOES TO PRINT MODE IF    *~
            * NECESSARY.                                                *~
            *************************************************************

            deffn'200(f%)

            init(hex(8c)) fac$()
            if f% <> 1% then goto L40150
                if dual_books$ <> "Y" then return
                fac$(1) = hex(82)
                printmessage$ = "Enter '1' for Statutory set of books; "&~
                    "'2' for Local Authority."
                goto L40162
L40150:     fac$(2) = hex(81)
            printmessage$ = "Enter blank or partial Account Number to sea~
        ~rch for desired account."
L40162:     line1$ = " "
            line1$ = "Display General Ledger Accounts"
            str(line1$,40) = "Archive Year: " & arcyear$
            str(line1$,65) = "Today: " & date$
            line2$ = " "
            if prevaccount$ <> " " then                                  ~
                line2$ = "Last Account Managed: " & prevaccount$
            str(line2$,62%) = " GLDSPLY: " & str(cms2v$,,8%)

            accept                                                       ~
               at (01,02), fac(hex(8c)), line1$                 , ch(79),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)), setmsg$                , ch(18),~
               at (06,23), fac(fac$(1)), set$                   , ch(01),~
               at (06,26), fac(hex(8c)), setdescr$              , ch(30),~
               at (07,02), "Account Number:",                            ~
               at (07,23), fac(fac$(2)), account$               , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)), printmessage$          , ch(79),~
               at (22,20), fac(pf4fac$), pf4msg$                , ch(22),~
               at (23,20), fac(pf9fac$), pf9msg$                , ch(24),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,39), "(14)Select Archive Year",                    ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(000d0e0f10) & pf4key$ & pf9key$), key (keyhit%)

               if keyhit% <> 13 then L40470
                  call "MANUAL" ("GLDSPLY")
                  return

L40470:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

L41000: REM *************************************************************~
            *        D I S P L A Y   A C C O U N T   S U M M A R Y      *~
            *                                                           *~
            * DISPLAYS SUMMARY OF THE ACCOUNT TRANSACTIONS OVER THE LAST*~
            * N MONTHS.                                                 *~
            *************************************************************

            line1$ = " "
            line1$ = "Display " & setdescr$ & " General Ledger Accounts"
            str(line1$,40) = "Archive Year: " & arcyear$
            line2$ = " "
            str(line1$,65) = "Today: " & date$
            if prevaccount$ <> " " then                                  ~
                line2$ = "Last Account Managed: " & prevaccount$
            str(line2$,62%) = " GLDSPLY: " & str(cms2v$,,8%)
            printmessage$ = "Press (RETURN) to display another account, o~
        ~r select function from list below."

            accept                                                       ~
               at (01,02), fac(hex(8c)), line1$                 , ch(79),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "Account Number",                             ~
               at (04,18), fac(hex(84)), account$               , ch(12),~
               at (05,02), "Description",                                ~
               at (05,18), fac(hex(84)), description$           , ch(32),~
               at (06,02), "Account Type",                               ~
               at (06,18), fac(hex(84)), type$                  , ch(01),~
               at (06,20), fac(hex(8c)), typedescr$             , ch(14),~
               at (07,02), "Sales Account?",                             ~
               at (07,18), fac(hex(84)), sales$                 , ch(01),~
               at (08,02), "Account Status",                             ~
               at (08,18), fac(hex(84)), statdescr$             , ch(32),~
                                                                         ~
               at (13,02), "Prior Balance",                              ~
               at (13,28), fac(hex(8c)), month$(3)              , ch(08),~
               at (13,38), fac(hex(8c)), history$(3)            , ch(13),~
               at (14,02), "Current Transactions",                       ~
               at (14,28), fac(hex(8c)), period$(2)             , ch(04),~
               at (14,38), fac(hex(8c)), currentperiod$         , ch(13),~
               at (15,38), "=============",                              ~
               at (16,02), "Current Balance",                            ~
               at (16,28), fac(hex(8c)), month$(2)              , ch(08),~
               at (16,38), fac(hex(8c)), history$(2)            , ch(13),~
               at (17,06), "Accrued Transactions",                       ~
               at (17,28), fac(hex(8c)), period$(1)             , ch(04),~
               at (17,38), fac(hex(8c)), accruedperiod$         , ch(13),~
               at (18,38), "=============",                              ~
               at (19,06), "Accrued Balance",                            ~
               at (19,28), fac(hex(8c)), month$(1)              , ch(08),~
               at (19,38), fac(hex(8c)), history$(1)            , ch(13),~
               at (04,57), "+-----------------------",                   ~
               at (05,57), "!       History",                            ~
               at (06,57), "!",  at (07,57), "!",                        ~
               at (08,57), "!",  at (09,57), "!",                        ~
               at (10,57), "!",  at (11,57), "!",                        ~
               at (12,57), "!",  at (13,57), "!",                        ~
               at (14,57), "!",  at (15,57), "!",                        ~
               at (16,57), "!",  at (17,57), "!",                        ~
               at (18,57), "!",  at (19,57), "!",                        ~
               at (20,57), "+-----------------------",                   ~
                                                                         ~
               at (07,59), fac(hex(8c)), month$( 4)             , ch(08),~
               at (07,68), fac(hex(8c)), history$( 4)           , ch(13),~
               at (08,59), fac(hex(8c)), month$( 5)             , ch(08),~
               at (08,68), fac(hex(8c)), history$( 5)           , ch(13),~
               at (09,59), fac(hex(8c)), month$( 6)             , ch(08),~
               at (09,68), fac(hex(8c)), history$( 6)           , ch(13),~
               at (10,59), fac(hex(8c)), month$( 7)             , ch(08),~
               at (10,68), fac(hex(8c)), history$( 7)           , ch(13),~
               at (11,59), fac(hex(8c)), month$( 8)             , ch(08),~
               at (11,68), fac(hex(8c)), history$( 8)           , ch(13),~
               at (12,59), fac(hex(8c)), month$( 9)             , ch(08),~
               at (12,68), fac(hex(8c)), history$( 9)           , ch(13),~
               at (13,59), fac(hex(8c)), month$(10)             , ch(08),~
               at (13,68), fac(hex(8c)), history$(10)           , ch(13),~
               at (14,59), fac(hex(8c)), month$(11)             , ch(08),~
               at (14,68), fac(hex(8c)), history$(11)           , ch(13),~
               at (15,59), fac(hex(8c)), month$(12)             , ch(08),~
               at (15,68), fac(hex(8c)), history$(12)           , ch(13),~
               at (16,59), fac(hex(8c)), month$(13)             , ch(08),~
               at (16,68), fac(hex(8c)), history$(13)           , ch(13),~
               at (17,59), fac(hex(8c)), month$(14)             , ch(08),~
               at (17,68), fac(hex(8c)), history$(14)           , ch(13),~
               at (18,59), fac(hex(8c)), month$(15)             , ch(08),~
               at (18,68), fac(hex(8c)), history$(15)           , ch(13),~
               at (19,59), fac(hex(8c)), month$(16)             , ch(08),~
               at (19,68), fac(hex(8c)), history$(16)           , ch(13),~
                                                                         ~
               at (21,02), fac(hex(a4)), printmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,02), "(2)Display Account Details",                 ~
               at (24,02), "(ENTER)Next Account",                        ~
               at (23,37), "(14)Print Account Details",                  ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return",                                 ~
                                                                         ~
               keys(hex(0001020d0e0f10)), key (keyhit%)

               if keyhit% <> 13 then L41485
                  call "MANUAL" ("GLDSPLY")
                  return

L41485:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *        D I S P L A Y   D E T A I L S   S C R E E N        *~
            *                                                           *~
            * DISPLAYS GENERAL LEDGER TRANSACTION DETAILS FOR THE ACCT  *~
            * NUMBER SPECIFIED.                                         *~
            *************************************************************

            deffn'222

            title$(1) = " Posted  MD Jnl         Debit       Credit Descr~
        ~iption                      Usr"
            title$(2) = " Posted   Seq Reference 1                    Ref~
        ~erence 2"

L42120:     accept                                                       ~
               at (01,02),                                               ~
                  "(ENTER)Toggle Text (2)Reselect (5)Next (14)Print (15)P~
        ~rnt Scrn (16)Next Account",                                      ~
               at (02,02), "Account:",                                   ~
               at (02,11), fac(hex(84)), account$               , ch(12),~
               at (02,26), "(3)See Totals  (9)Summary",                  ~
               at (02,54), "Type:",                                      ~
               at (02,60), fac(hex(84)), prttypedescr$          , ch(20),~
               at (03,11), fac(hex(84)), prtdescr$              , ch(32),~
                                                                         ~
               at (04,02), fac(hex(ac)), title$(see%)           , ch(79),~
               at (05,02), fac(sticky$( 1)), line$(see%, 1),      ch(79),~
               at (06,02), fac(sticky$( 2)), line$(see%, 2),      ch(79),~
               at (07,02), fac(sticky$( 3)), line$(see%, 3),      ch(79),~
               at (08,02), fac(sticky$( 4)), line$(see%, 4),      ch(79),~
               at (09,02), fac(sticky$( 5)), line$(see%, 5),      ch(79),~
               at (10,02), fac(sticky$( 6)), line$(see%, 6),      ch(79),~
               at (11,02), fac(sticky$( 7)), line$(see%, 7),      ch(79),~
               at (12,02), fac(sticky$( 8)), line$(see%, 8),      ch(79),~
               at (13,02), fac(sticky$( 9)), line$(see%, 9),      ch(79),~
               at (14,02), fac(sticky$(10)), line$(see%,10),      ch(79),~
               at (15,02), fac(sticky$(11)), line$(see%,11),      ch(79),~
               at (16,02), fac(sticky$(12)), line$(see%,12),      ch(79),~
               at (17,02), fac(sticky$(13)), line$(see%,13),      ch(79),~
               at (18,02), fac(sticky$(14)), line$(see%,14),      ch(79),~
               at (19,02), fac(sticky$(15)), line$(see%,15),      ch(79),~
               at (20,02), fac(sticky$(16)), line$(see%,16),      ch(79),~
               at (21,02), fac(sticky$(17)), line$(see%,17),      ch(79),~
               at (22,02), fac(sticky$(18)), line$(see%,18),      ch(79),~
               at (23,02), fac(sticky$(19)), line$(see%,19),      ch(79),~
               at (24,02), fac(sticky$(20)), line$(see%,20),      ch(79),~
                                                                         ~
               keys(hex(0001020305090e0f10)),                            ~
               key (keyhit%)

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42120

L43000: REM *************************************************************~
            *        P R I N T   M O D E   I N P U T   S C R E E N      *~
            *                                                           *~
            * GETS RANGE OF ACCOUNT NUMBERS TO PRINT.                   *~
            *************************************************************

            line1$ = " "
            line1$ = "Print" & setdescr$ & " General Ledger Accounts"
            str(line1$,40) = "Archive Year: " & arcyear$
            str(line1$,65) = "Today: " & date$
            line2$ = " "
            if prevaccount$ <> " " then                                  ~
                line2$ = "Last Account Managed: " & prevaccount$
            str(line2$,62%) = " GLDSPLY: " & str(cms2v$,,8%)
            printmessage$ = "Enter ranges, then press (RETURN) to print r~
        ~eport."

            accept                                                       ~
               at (01,02), fac(hex(8c)), line1$                 , ch(79),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Date Range from:",                           ~
               at (06,22), fac(hex(81)), starting$              , ch(10),~
               at (06,37), "thru",                                       ~
               at (06,42), fac(hex(81)), ending$                , ch(10),~
               at (08,02), "Account Range from:",                        ~
               at (08,22), fac(hex(81)), firstaccount$          , ch(12),~
               at (08,35), "thru",                                       ~
               at (08,40), fac(hex(81)), lastaccount$           , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)), printmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return",                                 ~
                                                                         ~
               keys(hex(00010d0f10)), key (keyhit%)

               if keyhit% <> 13 then L43400
                  call "MANUAL" ("GLDSPLY")
                  return

L43400:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

L44000: REM *************************************************************~
            *        P R I N T   M O D E   I N P U T   S C R E E N      *~
            *                                                           *~
            * GETS RANGE OF ACCOUNT NUMBERS TO PRINT.                   *~
            *************************************************************

            line1$ = " "
            line1$ = "Print" & setdescr$ & " General Ledger Accounts"
            str(line1$,40) = "Archive Year: " & arcyear$
            str(line1$,65) = "Today: " & date$
            line2$ = " "
            if prevaccount$ <> " " then                                  ~
                line2$ = "Last Account Managed: " & prevaccount$
            str(line2$,62%) = " GLDSPLY: " & str(cms2v$,,8%)
            printmessage$="Enter range, then press (RETURN) to continue."

            accept                                                       ~
               at (01,02), fac(hex(8c)), line1$                 , ch(79),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Date Range from:",                           ~
               at (06,35), fac(hex(81)), starting$              , ch(10),~
               at (06,46), "thru",                                       ~
               at (06,51), fac(hex(81)), ending$                , ch(10),~
               at (08,02), "Specific Module ID",                         ~
               at (08,35), fac(hex(81)), tmod$                  , ch(02),~
               at (09,02), "Specific User ID",                           ~
               at (09,35), fac(hex(81)), tusr$                  , ch(03),~
               at (10,02), "Debits, Credits, or Both (D,C,B)",           ~
               at (10,35), fac(hex(81)), tdc$                   , ch(01),~
               at (11,02), "Only amounts xx (GE, LE, EQ) to",            ~
               at (11,15), fac(hex(81)), ttyp$                  , ch(02),~
               at (11,35), fac(hex(81)), tamt$                  , ch(10),~
                                                                         ~
               at (14,02), "Please note: accept defaults to see all trans~
        ~actions, quickly.  Change date",                                 ~
               at (15,02), "             range to quickly see only those ~
        ~within a specific time period.",                                 ~
               at (16,02), "             Changing other selection fields ~
        ~can result in long search times",                                ~
               at (17,02), "             if your database is large, and f~
        ~ew transactions meet the criteria.",                             ~
                                                                         ~
               at (21,02), fac(hex(a4)), printmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return",                                 ~
                                                                         ~
               keys(hex(00010d0f10)), key (keyhit%)

               if keyhit% <> 13 then L44530
                  call "MANUAL" ("GLDSPLY")
                  return

L44530:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            * T E S T   D A T A   F O R   S I N G L E   A C C O U N T S *~
            *                                                           *~
            * TESTS FOR ACCOUNT ON FILE.                                *~
            *************************************************************

        deffn'150(f%)
            errormsg$ = " "
            on f% goto L50090, L50240
L50090:     set = 1 : main% = 1% : detl% = 2% /* Defaults */
            if dual_books$ <> "Y" then return
            call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ = " " then goto L50140
                errormsg$ = "G/L System code must be '1' (Statutory)"   &~
                    " or '2' (Local Authority)"
                return
L50140:     setdescr$ = "Statutory"
            if set = 1 then goto L50190
                setdescr$ = "Local Authority"
                main% = 11% : detl% = 12%
L50190:     sethdr$ = setdescr$
            call "FMTTITLE" (sethdr$, "G/L SYSTEM", 12%)
            return

L50240:     f1%(main%) = -999%
            call "GETCODE" (#main%,account$,prtdescr$,1%,0,f1%(main%))
            if f1%(main%) = 0                                            ~
                then errormsg$ = "Account Not On File:" & account$
            return

L51000: REM *************************************************************~
            *     T E S T   D A T A   F O R   R A N G E   P R I N T     *~
            *                                                           *~
            * TESTS DATA FOR RANGE PRINT--WON'T LET USER HAVE THE THING *~
            * WITH LAST KEY < FIRST KEY--POINTLESS.  ALSO NORMALIZES    *~
            * KEYS FOR SINGLE AND ALL OPTIONS.                          *~
            *************************************************************

             errormsg$ = " "
             REM HANDLE DATE FIRST
                if starting$ <> "ALL" then L51130
                     startdate$ = "19010101"
                     enddate$   = "20991231"
                     call "DATFMTC" (enddate$)
		     goto L51131
L51130:         startdate$ = starting$
                call "DATEOKC" (starting$, u3%, errormsg$)
                      if errormsg$ <> " " then return
L51131:         call "DATEOKC" (startdate$, u3%, errormsg$)
                      if errormsg$ <> " " then return
                if ending$ <> " " then L51160
                     enddate$   = "20991231"
                     call "DATFMTC" (enddate$)
		     goto L51161
L51160:         enddate$ = ending$
                call "DATEOKC" (ending$, u3%, errormsg$)
                      if errormsg$ <> " " then return
L51161:         call "DATEOKC" (enddate$, u3%, errormsg$)
                      if errormsg$ <> " " then return
                call "DATUFMTC" (enddate$)
                call "DATUFMTC" (startdate$)
                if startdate$ > enddate$ then  L51450

             REM HANDLES CASE FOR "ALL" CUSTOMERS
                 if firstaccount$ <> "ALL" then L51290
                    init(hex(00)) firstacct$
                    init(hex(ff)) lastacct$
                    return
L51290: REM HANDLES CASE FOR SINGLE CUSTOMER
            if set = 1                                                   ~
                then call "GLVALID" (firstaccount$, firstacct$,errormsg$)~
                else call "GLVALD2" (firstaccount$, firstacct$,errormsg$)
            if errormsg$ = " " then L51340
            errormsg$ = errormsg$ & ": " & firstaccount$
            return
L51340:     if lastaccount$ <> " " then L51370
            lastacct$ = firstacct$
            goto L51430

L51370: REM HANDLES CASE FOR A RANGE OF CUSTOMERS.
            if set = 1                                                   ~
                then call "GLVALID" (lastaccount$, lastacct$, errormsg$) ~
                else call "GLVALD2" (lastaccount$, lastacct$, errormsg$)
            if errormsg$ = " " then L51420
            errormsg$ = errormsg$ & ": " & lastaccount$
            return
L51420:     if lastacct$ < firstacct$ then L51450
L51430:             str(firstacct$) = str(firstacct$) addc all(hex(ff))
                    return
L51450:      REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                 errormsg$ = "Invalid range. Please respecify."
                 return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND DISPLAYS A       *~
            * MESSAGE (ONLY IN FOREGROUND) WHILE LINKING TO THE NEXT    *~
            * PROGRAM.                                                  *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end