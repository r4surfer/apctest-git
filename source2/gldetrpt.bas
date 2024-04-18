        REM *************************************************************~
            *                                                           *~
            *   GGG   L      DDDD   EEEEE  TTTTT  RRRR   PPPP   TTTTT   *~
            *  G      L      D   D  E        T    R   R  P   P    T     *~
            *  G GGG  L      D   D  EEEE     T    RRRR   PPPP     T     *~
            *  G   G  L      D   D  E        T    R   R  P        T     *~
            *   GGG   LLLLL  DDDD   EEEEE    T    R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLDETRPT - DISPLAYS GENERAL LEDGER ACCOUNTS, LETS US      *~
            *            PRINT SUMMARIES AND DETAILS FOR THEM.  WILL    *~
            *            PRINT IN BACKGROUND TOO.                       *~
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
            * 08/27/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/09/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 10/26/88 ! Fixed Break logic @ 14000 to run quicker ! JDH *~
            *          ! Branch Errors in Input. Thanks to KAB    !     *~
            * 06/05/89 ! Merged code for CMS2 and CMS-I.          ! MJB *~
            *          !  - Includes Group Code Range Input       !     *~
            *          !  - Corrected Ref 2 Range test.           !     *~
            * 10/27/89 ! Added L/A files & processing for report  ! MJB *~
            *          !  by group codes.                         !     *~
            * 11/20/89 ! Fixed file% variables and GLFMT/GLFMT2.  ! JDH *~
            * 12/04/90 ! Changed Date Range Validation            ! MJB *~
            * 03/08/91 ! Changed Call to JNLINFO for GL Batches   ! RAC *~
            * 06/03/91 ! PRR 11923- Subtotals not printing in some! JBK *~
            *          !  cases.  Subtotal option not on page 0.  !     *~
            * 12/09/91 ! Added the ability to print the GL Detail ! SID *~
            *          !  off of the archived file.               !     *~
            * 12/15/93 ! PRR 13062. Corrected sort/subtotal.      ! JDH *~
            *          ! PRR 11187. Can now enter ALL in Seq field!     *~
            * 08/31/95 ! PRR 13485. No error if journal not found.! JDH *~
            *          ! PRR 13266. Cursor position on enabled fld!     *~
            *          ! PRR 13183. Allow background printing.    !     *~
            *          ! Allow display rather than report.        !     *~
	    * 06/21/96 ! Changes for the year 2000                ! DXL *~
	    * 04/04/97 ! Change PUTPARM call for NT compatibility.! LDJ *~
            *************************************************************

        dim account$16,                  /* ACCOUNT NUMBER TO DISPLAY  */~
            arcyear$7,                   /* Archived Year Litteral     */~
            back$1,                      /* Background Printing?       */~
            compname$60,                 /* COMPANY NAME TO PRINT      */~
            choice$4,                    /* PICKYEAR return choice     */~
            credit$13,                   /* LINE AMOUNT TO PRINT       */~
            dates$(32)8,                 /* FISCAL DATE STRUCTURE      */~
            debit$13,                    /* LINE AMOUNT TO PRINT       */~
            descr$36,                    /* DESCRIPTION                */~
            description$32,              /* ACCOUNT DESCRIPTION        */~
            diskkey$50,                  /* READ KEY                   */~
            dual_books$1,                /* Dual books in effect?      */~
            exitmsg$(3)16,               /* Exit/Print Messages        */~
            fileid$4,                    /* File ID for PICKYEAR       */~
            firstaccount$12,             /* FIRST G/L ACCT IN RANGE PRT*/~
            firstgroup$6,                /* First Group Code           */~
            firstjnlid$3,                /* FIRST JOURNL ID IN RANGE   */~
            firstmod$3,                  /* FIRST MODULE NO IN RANGE   */~
            firstpstseq$10,              /* FIRST POST SEQ NO IN RANGE */~
            gldeprname$8,                /* Current GLDETAIL File Name */~
            groupcode$6,                 /* Group Code For Print       */~
            groupdescr$30,               /* Group Desc For Print       */~
            inpmessage$79,               /* Screen field prompt        */~
            jnlid$3,                     /* JOURNAL ID                 */~
            lastaccount$12,              /* LAST ACCOUNT IN RANGE PRINT*/~
            lastgroup$6,                 /* Last Group Code            */~
            lastjnlid$3,                 /* LAST JOUNRAL ID IN RANGE   */~
            lastmod$2,                   /* LAST MODULE NO IN RANGE    */~
            lastpstseq$10,               /* LAST POST SEQ NO IN RANGE  */~
            lfac$(20)1,                  /* FIELD DISPLAY FACS         */~
            message$78,                  /* INFORMATION MESSAGE        */~
            modno$2,                     /* MODULE NUMBER              */~
            moduleno$20,                 /* MODULE ID                  */~
            nextmsg$18,                  /* DISPLAY PF FOR SUBTOTAL OPT*/~
            newreadkey$50,               /* NEW KEY FOR PLOW ROUTINE   */~
            oldreadkey$50,               /* KEY FOR PLOW ROUTINE       */~
            option$(4)15,                /* SECOND SCREEN OPTION DISPLY*/~
            p1%(2),                      /* DATA SELECTION FOR REFENCE */~
            p2%(2),                      /* DATA SELECTION FOR REFENCE */~
            plowkey$50,                  /* KEY FOR PLOW ROUTINE       */~
            pf5lit$20,                   /* PF5 Screen Literal         */~
            prgmid$79,                   /* PROGRAM ID DISPLAY         */~
            prtdate$8,                   /* DATE CREATED               */~
            prtposted$8,                 /* DATE POSTED (FORMATTED)    */~
            prttotaldescr$40,            /* SUMMARY LINE DESCRIPTION   */~
            prtuserid$3,                 /* USERID OF DETAIL CREATOR   */~
            rangeref$(4)34,              /* REFERENCE RANGES           */~
            rangerf1$(4)34,              /* REFERENCE RANGES DISPLAY   */~
            readkey$6,                   /* Group File Read Key        */~
            ref1$30,                     /* REFERENCE 1 TEXT           */~
            ref2$34,                     /* REFERENCE 2 TEXT           */~
            ref$(2)34,                   /* REFERENCE FOR PRINT        */~
            rptid$6,                     /* Report ID                  */~
            savegrp$6,                   /* Save Group Code            */~
            set$1, setdescr$30, sethdr$60, /* Set of books code, etc.  */~
            setmsg$26,                   /* Screen message for SET     */~
            sortflag$(4)1,               /* SORT INDICATORS            */~
            startdate$10,                /* DATE FOR START OF DETAIL   */~
            starting$10,                 /* DATE FOR INPUT SCREENS     */~
            subtotop$(22)24,             /* PRINT INFO FOR SUBTOTAL OPT*/~
            enddate$10,                  /* DATE FOR END OF DETAIL     */~
            ending$10,                   /* DATE FOR INPUT SCREENS     */~
            titlj$40,                    /* JOURNAL TITLE              */~
            oldaccount$16,               /* LAST PRT ACCOUNT NUMBER    */~
            userid$3,                    /* Current User Id            */~
            wref$(2)34,                  /* WORKFILE REFERENCES        */~
            wmodno$2,                    /* WORKFILE MODULE NUMBER     */~
            wjnlid$3                     /* WORKFILE JOURNAL ID        */~

        dim blankline$79,                /* UNDERLINE FOR INPUT.       */~
            edtmessage$79,               /* "TO MODIFY..." MESSAGE TEXT*/~
            errormsg$79,                 /* ERROR MESSAGE              */~
            cursor%(2),                  /* CURSOR LOC FOR EDITING     */~
            i$(24)80                     /* SCREEN IMAGE FOR JUNK      */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            fs%(64)                      /* FILE STATUS FLAGS          */~

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
            * #03 ! SYSFILE2 ! SYSTEM INFORMATION (MONTHS OPEN LIST)    *~
            * #04 ! WORK1    ! WORKFILE FOR GL DETAIL SORT BY MODULE    *~
            * #05 ! WORK2    ! WORKFILE FOR GL DETAIL SORT BY ACCOUNT   *~
            * # 7 ! FRGRPMAS ! G/L Account Group Master                 *~
            * #17 ! FRGRPMA2 ! G/L Grouping Codes File for local auth.  *~
            * # 8 ! FRGRPLIN ! G/L Account Group Lines                  *~
            * #18 ! FRGRPLI2 ! G/L Grouping Codes Detail for local auth *~
            * #11 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #12 ! GLDETAL2 ! G. L. detail records for local authority *~
            *************************************************************

            select #01, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1,                                      ~
                        keylen = 9

            select #02, "GLDETAIL",      /* GENERAL LEDGER DETAILS     */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select  #03, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #04, "WORK1",        /* WORK FILE FOR MODULE SORT  */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 32,                                    ~
                        keypos = 1, keylen = 13

            select  #05, "WORK2",        /* WORK FILE FOR ACCOUNT SORT */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 87,                                    ~
                        keypos = 1, keylen = 87

            select #7,  "FRGRPMAS",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 1, keylen = 6,                          ~
                        alt key 1, keypos = 7, keylen = 30, dup

            select #8,  "FRGRPLIN",                                      ~
                        varc, indexed, recsize = 60,                     ~
                        keypos = 1, keylen = 22,                         ~
                        alt key 1, keypos = 7, keylen = 16, dup

            select #11, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #12, "GLDETAL2",                                      ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos = 1,    keylen = 26

            select #17,  "FRGRPMA2", varc, indexed, recsize = 100,       ~
                        keypos = 1, keylen = 6,                          ~
                        alt key 1,  keypos = 7, keylen = 30, dup

            select #18,  "FRGRPLI2", varc, indexed, recsize = 60,        ~
                        keypos = 1, keylen = 22,                         ~
                        alt key 1,  keypos = 7, keylen = 16, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#02, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#03, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 0%, rslt$( 8))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#03, "SWITCHS.GL", f1%(3))
                if f1%(3) = 0% then goto L09000
            get #03 using L02760, dual_books$
L02760:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
                call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
                call "OPENCHCK" (#17, fs%(17), f2%(17), 0%, rslt$(17))
                call "OPENCHCK" (#18, fs%(18), f2%(18), 0%, rslt$(18))

L09000: REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * INITIALIZES CONTROL VARIABLES.                            *~
            *************************************************************

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                " to Desired Value & Press (RETURN)."
            call "EXTRACT" addr("ID", userid$, "TT", back$)
            date$ = date : call "DATEFMT" (date$)
            call "COMPNAME" (12%, compname$, 0%)
            rptid$ = "G/L003"
            arcyear$ = "Current"
            str(prgmid$,62) = "GLDETRPT: " & str(cms2v$,1,8)
            option$(2) = "2. JOURNAL"
            option$(1) = "2. REFERENCE 2"
            option$(4) = "3. POSTING SEQ"
            option$(3) = "3. REFERENCE 1"
        REM READ IN THE FISCAL DATE STRUCTURE OUT OF SYSFILE2.
                call "READ100" (#03, "FISCAL DATES", f1%(3))
                  if f1%(3) <> 0 then L09160
            if back$ = "B" then L65000
L09120:     err% = 0%
            call "ASKUSER" (err%, "ERROR ON SETTING DATE RANGE",         ~
                            "Review your fiscal date structure",         ~
                           "Press PF-16 to Acknowledge and Exit Program",~
                           " ")
            if err% <> 16% then L09120
            goto L65000

L09160:          get #03, using L09170, periods%, monthopen%, dates$()
L09170:                  FMT XX(20), BI(2), XX(136), BI(2), 32*CH(8)
                 if monthopen% > 15 then L09210
                 p%=16 : if monthopen% = 12 and periods% = 12 then p%=17
                 str(dates$(),(monthopen%+p%)*8+1)=" "
L09210:          if periods% = 13 then L09260
                 copy str(dates$(), 14*8+1) to str(dates$(), 13*8+1)
                 copy str(dates$(), 27*8+1) to str(dates$(), 26*8+1)
                 dates$(31) = " " : dates$(30) = " "

L09260:        subtotop$( 1) = "Customer code"
               subtotop$( 2) = "Customer code/Document ID"
               subtotop$( 3) = "Customer code/Invoice No."
               subtotop$( 4) = "Customer code/Part No."
               subtotop$( 5) = "Vendor code"
               subtotop$( 6) = "Vendor code/Document ID"
               subtotop$( 7) = "Vendor code/Invoice or PO No."
               subtotop$( 8) = "Vendor code/Part No."
               subtotop$( 9) = "Part No."
               subtotop$(10) = "Part No./Store ID"
               subtotop$(11) = "Part No./Store ID & Lot ID"
               subtotop$(12) = "Part No./Job No."
               subtotop$(13) = "Job No."
               subtotop$(14) = "Job No./Part"
               subtotop$(15) = "Job No./Employee code"
               subtotop$(16) = "Employee Code"
               subtotop$(17) = "Employee Code/Deduction code"
               subtotop$(18) = "Employee Code/Earning code"
               subtotop$(19) = "Employee code/Job No."
               subtotop$(20) = "Dedcution code/Employee code"
               subtotop$(21) = "Earning code/Employee code"

            if dual_books$<>"Y" then lo_fieldnr%=2% else lo_fieldnr%=1%
            if dual_books$= "Y" then setmsg$="G/L System to use (1 or 2)"
            if back$ = "B" then printing_in_background

L10000: REM *************************************************************~
            *     P R I N T   A   R A N G E   O F   A C C O U N T S     *~
            *                                                           *~
            * PRINTS A RANGE OF GENERAL LEDGER ACCOUNTS.  NOTE THE CODE *~
            * TO SKIP OUT AND PRINT ALL IN BACKGROUND MODE...           *~
            *************************************************************
L10060:
            init(" ") firstaccount$, lastaccount$, errormsg$, enddate$,  ~
                firstmod$, lastmod$, firstjnlid$, lastjnlid$, back$,     ~
                rangeref$(), firstpstseq$, sortby$, rangerf1$(),         ~
                lastpstseq$, sortflag$(), starting$, ending$, subtotal$, ~
                set$, setdescr$, sethdr$, firstgroup$, lastgroup$
                pageno%, bigdebits, bigcredits, prtdebits, prtcredits,   ~
                    bigprtdebits, bigprtcredits, subdebits, subcredits,  ~
                    sortdebits, sortcredits, sub1debits, sub1credits,    ~
                    totaldebits, totalcredits, debit, credit, subtotal=0
                prtdebits$, prtcredits$ = " "
                subtotal2$ = "1"
                printline% = 100 : s% = 1% : bygr% = 0%
                set = 1 : main% = 1% : detl% = 2% /* Defaults */
                          grpm% = 7% : grpl% = 8% /* Defaults */
                edit% = 0%
            for fieldnr% = lo_fieldnr% to 9%
L10163:         gosub'101(fieldnr%)
                      if enabled% = 0 then L10210
L10170:         gosub'201(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 14 then select_archive_year
                  if keyhit%  = 16 then L65000
                  if keyhit% <>  0 then L10170
L10210:         gosub'151(fieldnr%)   /* NORMALIZE KEYS AFTER TEST*/
                  if errormsg$ <> " " then L10170
                  if bygr% = 1% then L11000
                next fieldnr%


            for fieldnr% = 1 to 2
                gosub'102(fieldnr%)
                      if enabled% = 0 then L10280
L10240:         gosub'202(fieldnr%)
                  if keyhit% = 1 then gosub startover
                  if keyhit% <> 0 then L10240
L10280:         gosub'152(fieldnr%)
                  if errormsg$ <> " " then L10240
                next fieldnr%

            if sortby$ <> "2" or subtotal$ = "1" then L11000

            for fieldnr% = 1 to 1
                gosub'103(fieldnr%)
                      if enabled% = 0 then L10370
L10340:         gosub'203(fieldnr%)
                  if keyhit% = 1 then gosub startover
                  if keyhit% <> 0 then L10340
L10370:         gosub'153(fieldnr%)
                  if errormsg$ <> " " then L10340
                next fieldnr%

L11000: REM *************************************************************~
            *     P R I N T   A   R A N G E   O F   A C C O U N T S     *~
            *                                                           *~
            * PRINTS A RANGE OF GENERAL LEDGER ACCOUNTS.  NOTE THE CODE *~
            * TO SKIP OUT AND PRINT ALL IN BACKGROUND MODE...           *~
            *************************************************************

        edtpg1
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
L11200:     gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 and bygr% = 0% then edtpg2
                  if keyhit%  = 14 then L12000
                  if keyhit%  = 16 then L12000
                  if keyhit%  = 32 then L12000
                  if keyhit% <>  0 then L11200
            fieldnr% = cursor%(1) - 5
            if fieldnr% < lo_fieldnr% or fieldnr% > 11% then L11200
            if bygr% = 1% and fieldnr% > 3% then edtpg1
            if bygr% = 0% and fieldnr% = 3% then edtpg1
            if fieldnr% < 9% then L11282
            if fieldnr% > 9% then L11268
            fieldnr% = 8%
            goto L11320
L11268:     fieldnr% = 9%

L11282: REM IF FIELDNR% <> 3% THEN 11320
                edit% = 1%
                gosub'101(fieldnr%)
                edtmessage$ = inpmessage$
            gosub'101(fieldnr%)
                  if enabled% = 0 then L11360
L11320:     gosub'211(fieldnr%)
                  if keyhit% = 1 then gosub startover
                  if keyhit <> 0 then L11320
L11360:     gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11320
                  if bygr% <> 9% then L11380
                      fieldnr% = 4%
                      bygr% = 0%
                      goto L10163
L11380:     goto edtpg1

        edtpg2
L11450:         gosub'212(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       edtpg1
                  if keyhit%  =  5 and sortby$ = "2" and subtotal$ <> "1"~
                                   then edtpg3
                  if keyhit%  = 14 then L12000
                  if keyhit%  = 16 then L12000
                  if keyhit%  = 32 then L12000
                  if keyhit% <>  0 then L11450
            fieldnr% = cursor%(1) - 7
            if fieldnr% < 1 or fieldnr% > 1 then L11450
            if cursor%(2) > 52% then fieldnr% = 2% else fieldnr% = 1%

L11610:         gosub'212(fieldnr%)
                  if keyhit% = 1 then gosub startover
                  if keyhit <> 0 then L11610
                gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11610
                goto L11450

        edtpg3
L11750:         gosub'213(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       edtpg2
                  if keyhit%  = 14 then L12000
                  if keyhit%  = 16 then L12000
                  if keyhit%  = 32 then L12000
                  if keyhit% <>  0 then L11450
            fieldnr% = cursor%(1) - 5%
            if fieldnr% <> 1% then L11750

L11830:         gosub'213(fieldnr%)
L11840:           if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11840
                gosub'153(fieldnr%)
                  if errormsg$ <> " " then L11830
                goto edtpg3

L12000: REM *************************************************************~
            *            M A I N   P R O C E S S I N G                  *~
            *-----------------------------------------------------------*~
            * Initialize Print Options And Start Record Selection       *~
            *************************************************************

            if keyhit% = 32% then set_up_the_report /* For Background */

start_report
            convert set to set$, pic (#)
            time$ = " "  :  call "TIME" (time$)
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            if keyhit% = 14% then call "SET" addr("PM", "K")
            if bygr% = 1% then group_report
            gosub L12610

        REM Check For Any Sort Options - If There Are, Go Somewhere
            if str(sortflag$(),2, 3) <> " " then L16000

            call "SHOSTAT" ("Printing General Ledger Detail Report")

            init (hex(00)) diskkey$
            str(diskkey$,1,9) = firstaccount$

L12190: REM Do Plow Routine, Printing Whatever.
L12200:     call "READ102" (#main%, diskkey$, f1%(main%))
                if f1%(main%) = 0 then L12380
            get #main%, using L12230, account$
L12230:         FMT CH(9)
            if account$ > lastaccount$ then L12380
                diskkey$ = account$ & " "
                init (hex(00)) newreadkey$
                str(newreadkey$,1,16)= str(account$,,16)
                str(newreadkey$,17,6)= str(startdate$,,6)
                call "PLOWNEXT" (#detl%, newreadkey$, 16%, f1%(detl%))
                     if f1%(detl%) = 0 then L12200
                     if str(newreadkey$,17,6) > enddate$ then L12200
                subflag% = 0%
                gosub'250(account$,startdate$)
                bigdebits = bigdebits + totaldebits
                bigcredits = bigcredits + totalcredits
                goto L12190

L12380: REM Close The Printer Before The Next Print
            prttotaldescr$ = "** GRAND TOTALS **"
            gosub'80(bigdebits, bigcredits)
            if printline% + 2% > 58 then gosub heading
            printline% = printline% + 2%
            print skip( 1)
        common_end
            print using L12450
            if keyhit% = 14% then call "GETPRTNM" addr(file$, lib$, vol$)
L12450: %                                                  *****  E N D  ~
        ~ O F   R E P O R T  *****
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            if back$ = "B" then L12490
            if keyhit% <> 14% then L10000

            REM Display printed file...
            close ws
            call "PUTPARM" addr("E", "INPUT   ",4%,                      ~
                     "FILE    ", file$, 8%, "LIBRARY ", lib$, 8%,        ~
                 "VOLUME  ", vol$, 6%, "ACCESS  ", "PRINT ", 6%, "@",ret%)
            call"LINK"addr("DISPLAY ","S"," "," ",0%," "," ",0%, "N", 0%,~
			   ret%)
            call "SCRATCH" addr("F", file$, lib$, vol$, " ", " ", 0%)
            goto L10000

L12490:     message$ = "rptReport GLDETRPT in background: completed."
            call "TIME" (str(message$,44,8))
            goto tell_user

        deffn'80(bigprtdebits, bigprtcredits)
            if bigprtdebits = 0 and bigprtcredits = 0 then return
            if printline% > 54% then gosub heading
            print skip(1)
            print using L55290, prttotaldescr$,bigprtdebits,bigprtcredits
            print skip(1)
            print using L55060
            printline% = printline% + 4%
            return

L12610: REM *************************************************************~
            *       S E T   D A T A   F O R   R A N G E   P R I N T     *~
            *-----------------------------------------------------------*~
            * Sets Data For Range Print-- Normalizes Keys For Single    *~
            * And All Options.                                          *~
            *************************************************************

            gosub print_page_zero
            gosub L13350    /* Go Initialize Sort Options */

        REM Handle Date First
            if starting$ <> "ALL" then L12740
                starting$ = "19010101"
                ending$   = "20991231"
L12740:     startdate$ = starting$
            enddate$ = ending$
            call "DATFMTC" (enddate$)
            call "DATFMTC" (startdate$)
            call "DATUFMTC" (enddate$)
            call "DATUFMTC" (startdate$)

        REM Handles Case For "All" Accounts
            if firstaccount$ <> "ALL" then L12840
                init(hex(00)) firstaccount$
                init(hex(ff)) lastaccount$
                goto L12910

L12840: REM Handles Case For Single Account
            if lastaccount$ <> " " then L12870
                lastaccount$ = firstaccount$
                if set = 1 then L12870
                call "GLUNFM2" (firstaccount$)
                call "GLUNFM2" (lastaccount$)
                goto L12890
L12870:         call "GLUNFMT" (firstaccount$)
                call "GLUNFMT" (lastaccount$)

L12890: REM Handles Case For A Range Of Accounts
            str(firstaccount$,1,9) = addc all(hex(ff))

L12910: REM Handles Case For "All" Modules
            if firstmod$ <> "ALL" then L12950
                firstmod$ = "00"
                lastmod$ = "99"

L12950: REM Handles Case For A Single Module
            if lastmod$ <> " " then L12980
                lastmod$ = firstmod$

L12980: REM Handles Case For "All" Journals
            if firstjnlid$ <> "ALL" then L13030
                init (hex(00)) firstjnlid$
                init (hex(ff)) lastjnlid$
                goto L13060

L13030: REM Handles Case For Single Journal
            if lastjnlid$ <> " " then L13060
                lastjnlid$ = firstjnlid$

L13060: REM Handles Case For "All" Posting Sequence Number
            if firstpstseq$ <> "ALL" then L13110
                firstpstseq% = -1
                lastpstseq% = 999999999
                goto L13130
L13110:     if lastpstseq$ <> " " then L13130
                lastpstseq% = firstpstseq%

L13130: REM Handles Case For "All" Reference 2
            str(rangeref$(p2%),p2%(1),p2%(2)) = rangerf1$(2%)
            str(rangeref$(p2%+2%),p2%(1),p2%(2)) = rangerf1$(4%)
            if rangerf1$(2%) <> "ALL" then L13200
                init (hex(00)) str(rangeref$(p2%),p2%(1),p2%(2))
                init (hex(ff)) str(rangeref$(p2%+2%),p2%(1),p2%(2))
                goto L13230
L13200:     if rangeref$(4%) <> " " then L13230
                str(rangeref$(p2%+2%),p2%(1),p2%(2)) = rangerf1$(2)

L13230: REM Handles Case For "All" Reference 1
            str(rangeref$(p1%),p1%(1),p1%(2)) = rangerf1$(1%)
            str(rangeref$(p1%+2%),p1%(1),p1%(2)) = rangerf1$(3%)
            if rangerf1$(1%) <> "ALL" then L13300
                init (hex(00)) str(rangeref$(p1%),p1%(1),p1%(2))
                init (hex(ff)) str(rangeref$(p1%+2%),p1%(1),p1%(2))
                goto L13330

L13300: REM Handles Case For A Single Reference 1
            if rangeref$(3) <> " " then return
            str(rangeref$(p1%+2%),p1%(1),p1%(2)) = rangerf1$(1)
L13330:     return

L13350: REM Set Sort And Subtotal Option
            init (" ") sortflag$()
            p1% = 1%  :  p2% = 2%
            p1%(1), p2%(1) = 1%  :  p1%(2) = 30%  :  p2%(2) = 34%
            if sortby$ = "1" then sortflag$(2) = "X"                     ~
                             else sortflag$(1) = "X"
            if subtotal$ = "1" then return
            if subtotal$ = "2" then sortflag$(3) = "X"                   ~
                               else sortflag$(4) = "X"
            if sortby$ = "1" and subtotal$ = "3" then sortflag$(3) = "X"
            if sortby$ = "1" then return

                on subtotal% goto L13700,           /* No Option        */~
                                  L13740,           /* Reference Sub    */~
                                  L13780,           /* Customer         */~
                                  L13830,           /* Customer-Document*/~
                                  L13880,           /* Customer-Invoice */~
                                  L13930,           /* Customer-Part    */~
                                  L13780,           /* Vendor           */~
                                  L13830,           /* Vendor-Document  */~
                                  L13880,           /* Vendor-Invoice   */~
                                  L13930,           /* Vendor-Part      */~
                                  L13980,           /* Part             */~
                                  L14030,           /* Store-Part       */~
                                  L14080,           /* Store Lot-Part   */~
                                  L14130,           /* Part - Job       */~
                                  L14180,           /* Job              */~
                                  L14230,           /* Job - Part       */~
                                  L14280,           /* Job - Employee   */~
                                  L14330,           /* Employee         */~
                                  L14380,           /* Employee-Deduct  */~
                                  L14380,           /* Employee-Earn    */~
                                  L14430,           /* Employee-Job     */~
                                  L14480,           /* Deduct-Employee  */~
                                  L14480            /* Earn  -Employee  */

L13700: REM Set No Additional Subtotalling
            p1% = 4% - a% : p1%(1), p2%(1) = 1% : p1%(2), p2%(2) = 34%
            p2% = a% - 1% : sortflag$(3) = "X"  : sortflag$(4) = " "
            return

L13740: REM Set Subtotal For Both Reference Fields
            gosub L13700
            sortflag$(4) = "X"
            return

L13780: REM Set Subtotal For Customer/Vendor Sort
            p1% = 1% : p1%(1) = 1% : p1%(2) = 9%
            p2% = 1% : p2%(1) = 1% : p2%(2) = 9%
            sortflag$(3) = "X" : sortflag$(4) = " "
            return

L13830: REM Set Subtotal For Customer/Vendor And Document Sort
            p1% = 1% : p1%(1) =  1% : p1%(2) = 9%
            p2% = 1% : p2%(1) = 10% : p2%(2) = 25%
            sortflag$(3), sortflag$(4) = "X"
            return

L13880: REM Set Subtotal For Customer/Vendor And Invoice Sort
            p1% = 1% : p1%(1) =  1% : p1%(2) = 9%
            p2% = 1% : p2%(1) = 10% : p2%(2) = 8%
            sortflag$(3), sortflag$(4) = "X"
            return

L13930: REM Set Subtotal For Customer/Vendor Part Number Sort
            p1% = 1% : p1%(1) = 1% : p1%(2) = 9%
            p2% = 2% : p2%(1) = 1% : p2%(2) = 25%
            sortflag$(3), sortflag$(4) = "X"
            return

L13980: REM Set Subtotal For Part Number Sort
            p1% = 2% : p1%(1) = 1% : p1%(2) = 25%
            p2% = 2% : p2%(1) = 1% : p2%(2) = 25%
            sortflag$(3) = "X" : sortflag$(4) = " "
            return

L14030: REM Set Subtotal For Store And Part Number Sort
            p1% = 2% : p1%(1) = 26% : p1%(2) = 3%
            p2% = 2% : p2%(1) = 1% : p2%(2) = 25%
            sortflag$(3), sortflag$(4) = "X"
            return

L14080: REM Set Subtotal For Store/Lot And Part Number Sort
            p1% = 2% : p1%(1) = 1% : p1%(2) = 25%
            p2% = 2% : p2%(1) = 26% : p2%(2) = 9%
            sortflag$(3), sortflag$(4) = "X"
            return

L14130: REM Set Subtotal For Part/Job No Sort
            p1% = 2% : p1%(1) = 1% : p1%(2) = 25%
            p2% = 1% : p2%(1) = 1% : p2%(2) = 8%
            sortflag$(3), sortflag$(4) = "X"
            return

L14180: REM Set Subtotal For Job No.
            p1% = 1% : p1%(1) = 1% : p1%(2) = 8%
            p2% = 1% : p2%(1) = 1% : p2%(2) = 8%
            sortflag$(3) = "X" : sortflag$(4) = " "
            return

L14230: REM Set Subtotal For Job No./Part No.
            p1% = 1% : p1%(1) = 1% : p1%(2) = 8%
            p2% = 2% : p2%(1) = 1% : p2%(2) = 25%
            sortflag$(3), sortflag$(4) = "X"
            return

L14280: REM Set Subtotal For Job No./Employee
            p1% = 1% : p1%(1) = 1% : p1%(2) = 8%
            p2% = 2% : p2%(1) = 1% : p2%(2) = 12%
            sortflag$(3), sortflag$(4) = "X"
            return

L14330: REM Set Subtotal For Employee Code
            p1% = 2% : p1%(1) = 1% : p1%(2) = 12%
            p2% = 2% : p2%(1) = 1% : p2%(2) = 12%
            sortflag$(3) = "X" : sortflag$(4) = " "
            return

L14380: REM Set Subtotal For Employee Code/Deduction Or Earn Code
            p1% = 2% : p1%(1) = 1% : p1%(2) = 12%
            p2% = 1% : p2%(1) = 1% : p2%(2) = 12%
            sortflag$(3), sortflag$(4) = "X"
            return

L14430: REM Set Subtotal For Employee Code Job No.
            p1% = 2% : p1%(1) = 1% : p1%(2) = 12%
            p2% = 1% : p2%(1) = 1% : p2%(2) = 8%
            sortflag$(3), sortflag$(4) = "X"
            return

L14480: REM Set Subtotal For Deduction Code/Employee Code
            p1% = 1% : p1%(1) = 1% : p1%(2) = 12%
            p2% = 2% : p2%(1) = 1% : p2%(2) = 12%
            sortflag$(3), sortflag$(4) = "X"
            return

        REM *************************************************************~
            *      P R I N T   B Y  A C C O U N T                       *~
            *-----------------------------------------------------------*~
            * Prints Account Range When No Subtotalling Is Requested    *~
            *************************************************************

        deffn'250(account$,startdate$)
            totaldebits,totalcredits=0
            init(hex(00)) oldreadkey$
            str(oldreadkey$,1, 16)= str(account$,,16)
            str(oldreadkey$,17, 6)= str(startdate$,,6)
            call "READ100" (#detl%, oldreadkey$, f1%(detl%))
                if f1%(detl%) = 1% then gosub L30210
                if f1%(detl%) = 1% then L15150
L15140:     gosub L30000
L15150:         if f1%(detl%) = 0 then L15600
                if prtposted$ > enddate$    then L15600
                if modno$ < firstmod$       then L15140
                if modno$ > lastmod$        then L15140
                if jnlid$ < firstjnlid$     then L15140
                if jnlid$ > lastjnlid$      then L15140
                if pstseq% < firstpstseq%   then L15140
                if pstseq% > lastpstseq%    then L15140
                if ref1$   < rangeref$(1)   then L15140
                if ref1$   > rangeref$(3)   then L15140
                if ref2$   < rangeref$(2)   then L15140
                if ref2$   > rangeref$(4)   then L15140

            gosub L15310
            goto L15140

L15310: REM Print Detail Line
            if subflag% = 0% then gosub sub_heading
            call "DATEFMT" (prtposted$)
            call "CONVERT" (debit, 2.2, debit$)
            call "CONVERT" (credit, 2.2, credit$)
            if debit = 0  then debit$ = " "
            if credit = 0 then credit$ = " "
            totaldebits = totaldebits + debit
            totalcredits = totalcredits + credit
            if printline% + 1% <= 59% then L15430
                printline% = 999%
                gosub sub_heading
L15430:     printline% = printline% + 1
            temp$ = account$
            if set = 1 then call "GLFMT" (temp$)                         ~
                       else call "GLFMT2" (temp$)
            if sortflag$(1) <> " "                                       ~
                then print using L55130, prtposted$, modno$, jnlid$,      ~
                           pstseq%, ref1$, ref2$, descr$, debit$, credit$~
                else print using L55170, temp$, prtposted$, modno$,       ~
                           jnlid$, ref1$, ref2$, descr$, debit$, credit$
L15530:     str(wref$(p2%),p2%(1),p2%(2)) = str(ref$(p2%),p2%(1),p2%(2))
L15540:     str(wref$(p1%),p1%(1),p1%(2)) = str(ref$(p1%),p1%(1),p1%(2))
            subflag% = 1%
            return

L15600: REM Entry Point For Calls Using Totaldebits, Totalcredits
            if totaldebits = 0 and totalcredits = 0 then return
            prttotaldescr$ = "** ACCOUNT " & temp$ & " TOTAL **"
            if str(sortflag$(),2,3) <> " " then L16300
            if printline% + 2% > 58 then gosub heading
            printline% = printline% + 2
            print using L55210,prttotaldescr$,totaldebits, totalcredits
            print using L55060
            return

L16000: REM *************************************************************~
            *S O R T  A N D  P R I N T  A  R A N G E  O F  A C C O U N T*~
            *-----------------------------------------------------------*~
            *Opens A Sort File Depending On Sort Option, And Then Prints*~
            *************************************************************

            if sortflag$(1) <> " " then file% = 5 else file% = 4
            call "WORKOPEN" (#file%, "IO", 1000%, f2%(file%))
                if f2%(file%) <> 0 then L17055
            init (hex(00)) oldreadkey$
            str(oldreadkey$,1,9) = firstaccount$
            headflag%, wseq% = 0
            call "SHOSTAT" ("Sorting General Ledger Detail")

        REM Do Plow Routine, Gathering Appropriate Information
L16075:     call "READ102" (#main%, oldreadkey$, f1%(main%))
                if f1%(main%) = 0 then L16300
            get #main%, using L16090, account$
L16090:         FMT CH(9)
            if account$ > lastaccount$ then L16300
                init (hex(00)) oldreadkey$
                str(oldreadkey$,1,16)= str(account$,, 9%)
                str(oldreadkey$,17,6)= str(startdate$,,6)
                call "READ100" (#detl%, oldreadkey$, f1%(detl%))
                    if f1%(detl%) = 1% then gosub L30210
                    if f1%(detl%) = 1% then L16140
L16130:         gosub L30000
            if f1%(detl%) = 0 then L16075
L16140:     if prtposted$ < startdate$  then L16130
            if prtposted$ > enddate$    then L16075
            if modno$ < firstmod$       then L16130
            if modno$ > lastmod$        then L16130
            if jnlid$ < firstjnlid$     then L16130
            if jnlid$ > lastjnlid$      then L16130
            if pstseq% < firstpstseq%   then L16130
            if pstseq% > lastpstseq%    then L16130
            if str(ref$(p1%),p1%(1),p1%(2)) <                            ~
               str(rangeref$(p1%),p1%(1),p1%(2)) then L16130
            if str(ref$(p1%),p1%(1),p1%(2)) >                            ~
               str(rangeref$(p1% + 2%),p1%(1),p1%(2)) then L16130
            if str(ref$(p2%),p2%(1),p2%(2)) <                            ~
               str(rangeref$(p2%),p2%(1),p2%(2)) then L16130
            if str(ref$(p2%),p2%(1),p2%(2)) >                            ~
               str(rangeref$(p2% + 2%),p2%(1),p2%(2)) then L16130

        REM Now Save The Pointer In The Workfile
            if file% = 5% then L16265
            write #4, using L16245, modno$, jnlid$, pstseq%, wseq%,       ~
                            account$, prtposted$, seq%, eod goto L17055
L16245:         FMT CH(2), CH(3), BI(4), BI(4), CH(9), CH(6), BI(4)
            wseq% = wseq% + 1
            goto L16130

L16265:     write #5, using L16285, account$,                             ~
                                   str(ref$(p1%), p1%(1), p1%(2)),       ~
                                   str(ref$(p2%), p2%(1), p2%(2)),       ~
                                   prtposted$, seq%, eod goto L17055
L16285:         FMT CH(9),CH(34),CH(34),CH(6),BI(4)
            goto L16130

L16300: REM Now Print The Workfile
            call "SHOSTAT" ("Printing General Ledger Detail Report")
            init (hex(00)) newreadkey$
            init (hex(20)) modno$, jnlid$,wref$(), oldaccount$
            pstseq% = -1
            if file% = 5% then L16420
            call "READ102" (#4, newreadkey$, f1%(4))
            goto L16345
L16340:     call "READNEXT" (#4, f1%(4))
L16345:         if f1%(4) = 0 then L16705
            get #4 using L16245, wmodno$, wjnlid$, wpstseq%,              ~
                                wseq%, account$, prtposted$, seq%
            if wmodno$ <> modno$ then L16575
            if sortflag$(3) <> " " and wjnlid$ <> jnlid$ then L16550
            if sortflag$(4) <> " " and wpstseq% <> pstseq% then L16530
L16375:     put oldreadkey$, using L16380, account$, prtposted$, seq%
L16380:         FMT CH(16), CH(6), BI(4)
            if headflag% = 0 then gosub sub_heading
            headflag% = 1
            gosub L30100
            if f1%(detl%) = 0 then L17055
            gosub L15310
            goto L16340

L16420:     call "READ102" (#5, newreadkey$, f1%(5))
            goto L16435
L16430:     call "READNEXT" (#5, f1%(5))
L16435:         if f1%(5) = 0 then L16705
            get #5 using L16285,  account$,                               ~
                                 str(ref$(p1%), p1%(1), p1%(2)),         ~
                                 str(ref$(p2%), p2%(1), p2%(2)),         ~
                                 prtposted$, seq%
            if account$ <> oldaccount$ then L16575
            if sortflag$(3) <> " " and str(wref$(p1%),p1%(1),p1%(2))     ~
                            <> str(ref$(p1%),p1%(1),p1%(2)) then L16550
            if sortflag$(4) <> " " and str(wref$(p2%),p2%(1),p2%(2))     ~
                            <> str(ref$(p2%),p2%(1),p2%(2)) then L16530
L16485:     put oldreadkey$, using L16380, account$, prtposted$, seq%
            if headflag% = 0 then gosub sub_heading
            headflag% = 1
            gosub L30100
            if f1%(detl%) = 0 then L17055
                oldaccount$ = account$
                gosub L15310
                goto L16430

L16530: REM Subtotal Second Subtotal
            gosub L16735
            if file% = 4% then L16375 else goto L16485

L16550: REM Subtotal First Subtotal
            gosub L16735
            gosub L16820
            if file% = 4% then L16375 else goto L16485

L16575: REM Subtotal Module/Account
            gosub L16600
            headflag% = 0
            if file% = 4% then L16375 else goto L16485

L16600: REM Actual Total Routine For Module
            gosub L16735
            gosub L16820
            if sortdebits = 0 and sortcredits = 0 then return
            if printline% + 5% > 58 then gosub heading
            printline% = printline% + 5
            if file% = 4 then                                            ~
                         prttotaldescr$ = "** MODULE "&modno$&" TOTAL **"~
                         else prttotaldescr$ = "** ACCOUNT TOTAL **"
            if sortflag$(1) <> " " then L16670
            if str(sortflag$(),3,2) <> " " then                          ~
                     gosub'80(sortdebits, sortcredits)                   ~
                else gosub'60(sortdebits, sortcredits)
            goto L16675
L16670:     gosub'60(sortdebits,sortcredits)
L16675:     bigdebits = bigdebits + sortdebits
            bigcredits = bigcredits + sortcredits
            totaldebits, totalcredits, sub1debits, sub1credits,          ~
                         sortdebits,   sortcredits = 0
            return

L16705: REM Total For Report
            gosub L16600
            call "FILEBGON" (#file%)
            f2%(file%) = 1%
            goto L12380

L16735: REM Subtotal For Subtotal2
            if totaldebits = 0 and totalcredits = 0 then L15530
            if sortflag$(4) = " " then L16795
            if file% = 4 then prttotaldescr$ = "** POSTING TOTAL **"     ~
                         else prttotaldescr$ = "*  " &                   ~
                              str(wref$(p2%), p2%(1), p2%(2))

            if sortflag$(1) <> " "                                       ~
                then gosub'70(totaldebits,totalcredits)                  ~
                else gosub'60(totaldebits,totalcredits)
            if sortflag$(1) <> " " then L16795
                if sortflag$(4) <> " " then headflag% = 0
L16795:     sub1debits = sub1debits + totaldebits
            sub1credits= sub1credits + totalcredits
            totaldebits, totalcredits = 0
            return

L16820: REM Subtotal For Subtotal1
            if sub1debits = 0 and sub1credits = 0 then L15540
            if sortflag$(3) = " " then L16895
            if file% = 4 then                                            ~
                    prttotaldescr$ = "** JOURNAL "& jnlid$ & " TOTAL **" ~
               else prttotaldescr$="** " & str(wref$(p1%),p1%(1),p1%(2))

            if sortflag$(1) <> " " then L16890
            if sortflag$(4) <> " " then gosub'80(sub1debits,sub1credits) ~
                                   else gosub'60(sub1debits,sub1credits)

            if sortflag$(3) <> " " then headflag% = 0
            goto L16895
L16890:     gosub'70(sub1debits,sub1credits)
L16895:     sortdebits = sortdebits + sub1debits
            sortcredits= sortcredits + sub1credits
            sub1debits, sub1credits = 0
            return

        deffn'60(subdebits,subcredits)
            if printline% > 56% then gosub heading
            print using L55210, prttotaldescr$, subdebits, subcredits
            print using L55060
            printline% = printline% + 2%
            return

        deffn'70(subdebits,subcredits)
            if printline% > 56% then gosub heading
            subtotal = subdebits - subcredits
            if subtotal < 0 then L16990
            prtdebits = subtotal : prtcredits = 0
            goto L16995
L16990:     prtdebits = 0 : prtcredits = -1 * subtotal
L16995:     convert prtdebits  to prtdebits$, pic(-#########.##)
            convert prtcredits to prtcredits$, pic(-#########.##)
            if prtdebits = 0 and prtcredits = 0 then L17025
            if prtdebits = 0 then prtdebits$ = " "
            if prtcredits = 0 then prtcredits$ = " "
            if prtdebits = 0 and prtcredits = 0 then prtcredits$ = " "
L17025:     print skip(1)
            print using L55250, prttotaldescr$, prtdebits$, prtcredits$
            print skip(1)
            printline% = printline% + 3%
            return

L17055: REM Error Routine For No Workfile Or Workfile Full
            errormsg$ = "Workfile Not Opened or Workfile Full/Corrupted"
            close #file%
            f2%(file%) = 1
            goto L10060

        group_report
            gosub print_page_zero

*       ** First set up the Date Range
            call "SHOSTAT" ("Generating G/L Detail Report by GROUP CODE")
            if starting$ <> "ALL" then L18080
                starting$ = "19010101"
                ending$   = "20991231"
L18080:     startdate$ = starting$
            enddate$ = ending$
            call "DATFMTC" (enddate$)
            call "DATFMTC" (startdate$)
            call "DATUFMTC" (enddate$)
            call "DATUFMTC" (startdate$)

*       ** Next set up Group Code Range
            if firstgroup$ <> "ALL" then L18160
                init(hex(00)) firstgroup$ : init(hex(ff)) lastgroup$
L18160:     readkey$ = firstgroup$
            call "READ104" (#grpm%, readkey$, f1%(grpm%))
                if f1%(grpm%) = 0% then end_group_report
            goto L18240

        group_loop     /* Group Read Loop Here */
            call "READNEXT" (#grpm%, f1%(grpm%))
                if f1%(grpm%) = 0% then end_group_report

L18240:     get #grpm% using L18250, groupcode$, groupdescr$
L18250:         FMT CH(6), CH(30)
            if groupcode$ > lastgroup$ then end_group_report
            gosub group_header
            init(hex(00)) plowkey$
            str(plowkey$,,6) = str(groupcode$)

        account_loop     /* Group Account Read Loop Here */
            call "PLOWNEXT" (#grpl%, plowkey$, 6%, f1%(grpl%))
                if f1%(grpl%) = 1% then L18360
                   gosub group_sub_total
                   goto group_loop
L18360:     get #grpl% using L18370, account$, description$
L18370:         FMT POS(7), CH(16), CH(30)
            gosub account_header
            totaldebits, totalcredits = 0
            init(hex(00)) oldreadkey$
            str(oldreadkey$, 1, 16) = str(account$,,16)
            str(oldreadkey$, 17, 6) = str(startdate$,,6)

        detail_loop
            call "PLOWNEXT" (#detl%, oldreadkey$, 16%, f1%(detl%))
                if f1%(detl%) = 1% then L18490
L18470:             gosub account_sub_total
                    goto account_loop
L18490:     get #detl%, using L18530, prtposted$, seq%, modno$, debit,    ~
                                 credit, ref1$, ref2$, descr$, jnlid$,   ~
                                 pstseq%, prtuserid$, prtdate$

L18530:         FMT XX(16),                        /* Skip Account     */~
                    CH(6),                         /* Module Post Date */~
                    BI(4),                         /* Seq #            */~
                    CH(2),                         /* Type Code Info   */~
                    2*PD(14,4),                    /* Debits, Credits  */~
                    CH(30),                        /* Ref Text String  */~
                    CH(34),                        /* Ref 2 Text       */~
                    CH(36),                        /* Description      */~
                    CH(3),                         /* Journal Id       */~
                    BI(4),                         /* Post Seq #       */~
                    CH(3),                         /* Userid Of Creator*/~
                    CH(6)                          /* System Date      */
            if prtposted$ > enddate$ then L18470
            ref$(1) = ref1$
            ref$(2) = ref2$

*       ** Now print the detail line and accumulate totals
            call "DATEFMT" (prtposted$)
            call "CONVERT" (debit, 2.2, debit$)
            call "CONVERT" (credit, 2.2, credit$)
            if debit = 0  then debit$ = " "
            if credit = 0 then credit$ = " "
            totaldebits = totaldebits + debit
            totalcredits = totalcredits + credit
            if printline% < 58% then L18800
                gosub heading
                gosub account_header
L18800:     printline% = printline% + 1
            temp$ = account$
            if set = 1 then call "GLFMT" (temp$)                         ~
                       else call "GLFMT2" (temp$)
            print using L55130, prtposted$, modno$, jnlid$, pstseq%,      ~
                               ref1$, ref2$, descr$, debit$, credit$
            goto detail_loop

        group_header
            gosub heading
            print skip(1)
            print using L55410, groupcode$, groupdescr$
            printline% = printline% + 2%
            savegrp$ = groupcode$
            return

        account_header
            call "DESCRIBE" (#main%, account$, description$, 0%,         ~
                                               f1%(main%))
            if f1%(main%) <> 0% then L19010
                 description$ = "** NOT ON FILE! **" : goto L19050
L19010:     get #main%, using L19020, temp$
L19020:         FMT POS(41), CH(1)
            acctstatus$ = "(Active)" : if temp$ = hex(01) or             ~
                          temp$ = hex(02) then acctstatus$ = "(Obsolete)"
L19050:     temp$ = account$
            if set = 1 then call "GLFMT" (temp$)                         ~
                       else call "GLFMT2" (temp$)
            if printline% > 54% then gosub heading
            print skip(1)
            print using L55100, temp$, description$, acctstatus$
            print using L55760
            printline% = printline% + 3%
            return

        account_sub_total
            prttotaldescr$ = "**** ACCOUNT " & temp$ & " TOTAL ****"
            if printline% > 55% then gosub heading
            print using L55210, prttotaldescr$, totaldebits, totalcredits
            print using L55060
            printline% = printline% + 2
            groupdb = groupdb + totaldebits
            groupcr = groupcr + totalcredits
            totaldebits, totalcredits = 0
            return

        group_sub_total
            if printline% > 55% then gosub heading
            prttotaldescr$ = "****** GROUP " & savegrp$ & " TOTAL ******"
            print skip(1)
            print using L55210, prttotaldescr$, groupdb, groupcr
            print using L55060
            printline% = printline% + 3%
            bigdebits = bigdebits + groupdb
            bigcredits = bigcredits + groupcr
            savegrp$ = groupcode$
            groupdb, groupcr = 0
            return

        end_group_report
            prttotaldescr$ = "********** GRAND TOTALS **********"
            if printline% > 53 then gosub heading
            print skip(1)
            print using L55290, prttotaldescr$, bigdebits, bigcredits
            print skip(1)
            print using L55060
            print skip( 1)
            goto common_end

        REM *************************************************************~
            *               E N A B L E  F O R  P A G E  1              *~
            *************************************************************

        deffn'101(fieldnr%)
            enabled% = 1% : inpmessage$ = " "
            on fieldnr% gosub  L20160,              /* G/L system to use*/~
                               L20200,              /* Date Range       */~
                               L20250,              /* Group Range      */~
                               L20300,              /* ACCOUNT RANGE    */~
                               L20350,              /* MODULE RANGE     */~
                               L20400,              /* JOURNAL RANGE    */~
                               L20450,              /* POST SEQ RANGE   */~
                               L20500,              /* REFERENCE 1      */~
                               L20550               /* REFERENCE 2 RANGE*/
            return

L20160: REM Enable code for Statutory or Local Authority G/L set  SET$
            inpmessage$ = "Enter '1' to use Statutory books; '2' to use"&~
                          " Local Authority books"
            return

L20200: REM Enable For First Date
            if edit% <> 1% then starting$ ="ALL"
            inpmessage$ = "Enter range of Dates or 'ALL'"
            return

L20250: REM Enable For Group Range
            inpmessage$ = "Group Disables all other Range and Sort "  &  ~
                          "Selections, Leave Blank to Use Others."
            return

L20300: REM Enable For First Account
            if edit% <> 1% then firstaccount$ = "ALL"
            inpmessage$ = "Enter range of Account Numbers or 'ALL'"
            return

L20350: REM Enable For First Module
            if edit% <> 1% then firstmod$ = "ALL"
            inpmessage$ = "Enter range of Module Names or 'ALL'"
            return

L20400: REM Enable For First Journal
            if edit% <> 1% then firstjnlid$ = "ALL"
            inpmessage$ = "Enter range of Journals or 'ALL'"
            return

L20450: REM Enable For First Posting Sequence
            if edit% <> 1% then firstpstseq$ = "ALL"
            inpmessage$ = "Enter range of Posting Sequences or 'ALL'"
            return

L20500: REM Enable For First Reference 1
            if edit% <> 1% then rangerf1$(1%)  = "ALL"
            inpmessage$ = "Enter range of FIRST References or 'ALL'"
            return

L20550: REM Enable For First Reference 2
            if edit% <> 1% then rangerf1$(2%)  = "ALL"
            inpmessage$ = "Enter range of SECOND References or 'ALL'"
            return

        REM *************************************************************~
            *              E N A B L E S  F O R  S C R E E N  2         *~
            *************************************************************

        deffn'102(fieldnr%)
            enabled% = 1%
            return

        REM *************************************************************~
            *               E N A B L E  F O R  P A G E  1              *~
            *************************************************************

        deffn'103(fieldnr%)
            enabled% = 1%
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives The User The Ability To Start Over When He Wants To *~
            * Else Return To The Menu.  Notice That He Has To Push 2    *~
            * Different Buttons To Start Over--A Little Harder.         *~
            *************************************************************

        startover
            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return
            return clear all
            goto L10000

L30000: REM *************************************************************~
            *        D E T A I L  P L O W  R O U T I N E                *~
            *************************************************************
            test% = 16           /* Test For Normal Plow       */
            goto L30126
L30100:     call "READ100"  (#detl%, oldreadkey$, f1%(detl%))
                if f1%(detl%) = 0 then return
            goto L30210
L30126:     call "PLOWNEXT" (#detl%, oldreadkey$, test%, f1%(detl%))
                if f1%(detl%) = 0 then return
L30210:     get #detl%, using L30294, prtposted$, seq%, modno$, debit,    ~
                        credit, ref1$, ref2$, descr$, jnlid$, pstseq%,   ~
                        prtuserid$, prtdate$
L30294:                     FMT XX(16),            /* SKIP ACCOUNT     */~
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

            ref$(1) = ref1$
            ref$(2) = ref2$
            return

        select_archive_year
          if back$ = "B" then L31030
            fileid$ = "GLDE"
            call "PICKYEAR" (fileid$, choice$)
L31030:       if f2%(02) = 0 then close #02 /* Close Curr. Detail File */
              if choice$ = "CURR" or choice$ = " " then L31050 else L31060
L31050:       gldeprname$ = "GLDETAIL" : goto L31070
L31060:       gldeprname$ = "GLDE" & choice$
L31070:       call "PUTPRNAM" addr(#02, gldeprname$)
              call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
              if choice$ = "CURR" or choice$ = " "                       ~
               then arcyear$ = "Current" else arcyear$ = choice$ & "   "
            if back$ = "B" then return
            goto L10170  /* Start from the Top */

set_up_the_report
*          Setup for background processing
	    if starting$ = "ALL" then write_temp_rec
	    call "DATUFMTC" (starting$)
	    call "DATUFMTC" (ending$)
write_temp_rec
                call "READ101" (#3, "zGLDETRPT." & userid$, f1%(3%))
                put #3, using L32080, "zGLDETRPT." & userid$, choice$,    ~
                    set$, starting$, ending$, firstgroup$, lastgroup$,   ~
                    firstaccount$, lastaccount$, firstmod$, lastmod$,    ~
                    firstjnlid$, lastjnlid$, firstpstseq$, lastpstseq$,  ~
                    rangerf1$(1%), rangerf1$(2%), rangerf1$(3%),         ~
                    rangerf1$(4%), sortby$, subtotal$, subtotal2$
L32080:         FMT CH(20), CH(4), CH(1), 2*CH(8), 2*CH(6), 2*CH(12),    ~
                    CH(3), CH(2), 2*CH(3), 2*CH(10), 4*CH(34), 2*CH(1),  ~
                    CH(2)
                if f1%(3%) = 0% then write #3 else rewrite #3
		close #3
                call "TASKUP" ("ME", 0%)
                goto L65000

        printing_in_background
            message$ = "rptReport GLDETRPT in background: Aborted."
            call "READ101" (#3, "zGLDETRPT." & userid$, f1%(3))
                 if f1%(3) = 0% then tell_user
            get #3, using L32080, temp$, choice$,                         ~
                    set$, starting$, ending$, firstgroup$, lastgroup$,   ~
                    firstaccount$, lastaccount$, firstmod$, lastmod$,    ~
                    firstjnlid$, lastjnlid$, firstpstseq$, lastpstseq$,  ~
                    rangerf1$(1%), rangerf1$(2%), rangerf1$(3%),         ~
                    rangerf1$(4%), sortby$, subtotal$, subtotal2$
	    if starting$ = "ALL" then test_tmp_choice
	    call "DATFMTC" (starting$)
	    call "DATFMTC" (ending$)
test_tmp_choice
            if choice$ = "CURR" or choice$ = " " then L32214
                gosub select_archive_year
L32214:     gosub L50190 /* Set                */
            gosub L50420 /* Group Code         */
            gosub L50820 /* Posting Sequence   */
            gosub L53200 /* Sort Option        */
            gosub L53300 /* Subtotal Option    */
            gosub L54200 /* Subtotal Option- 2 */
            /* Above GOSUBs needed to set other variables */
            delete #3
            goto start_report

        tell_user
            call "MESSAGE"addr("XM",str(userid$)&hex(20),message$,78%,0%)
            goto L65000

        REM *************************************************************~
            *        P R I N T   M O D E   I N P U T   S C R E E N      *~
            *-----------------------------------------------------------*~
            * Gets Range Of Account Numbers To Print.                   *~
            *************************************************************

            deffn'201(fieldnr%)
                  str(prgmid$,1,50) = "Report Range Selection"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  exitmsg$(1%) = "(16)EXIT PROGRAM"
                  on fieldnr% gosub L40226,         /* G/L system to use*/~
                                    L40220,         /* Date Range       */~
                                    L40220,         /* Group Range      */~
                                    L40220,         /* Account Range    */~
                                    L40220,         /* Module Range     */~
                                    L40220,         /* Journal Range    */~
                                    L40220,         /* Seq Range        */~
                                    L40220,         /* Ref 1 Range      */~
                                    L40220          /* Ref 2 Range      */
            goto L40240
                  REM Set Fac's For Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM Set Fac's For Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40226:           REM Set Fac's For Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L40240:     accept                                                       ~
               at (01,02), "Print General Ledger Detail",                ~
               at (01,67), "Date: ",                                     ~
               at (01,40), "Archive Year:",fac(hex(8c)),arcyear$, ch(07),~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), fac(hex(8c)), setmsg$                , ch(26),~
               at (06,30), fac(lfac$( 1)), set$                 , ch(01),~
               at (06,33), fac(hex(8c)),   setdescr$            , ch(30),~
               at (07,02), "Date Range",                                 ~
               at (07,30), fac(lfac$( 2)), starting$            , ch(10),~
               at (07,43), fac(lfac$( 2)), ending$              , ch(10),~
               at (08,02), "Account Group Range",                        ~
               at (08,30), fac(lfac$( 3)), firstgroup$          , ch(06),~
               at (08,43), fac(lfac$( 3)), lastgroup$           , ch(06),~
               at (09,02), "Account Code Range",                         ~
               at (09,30), fac(lfac$( 4)), firstaccount$        , ch(12),~
               at (09,43), fac(lfac$( 4)), lastaccount$         , ch(12),~
               at (10,02), "Module ID Range",                            ~
               at (10,30), fac(lfac$( 5)), firstmod$            , ch(03),~
               at (10,43), fac(lfac$( 5)), lastmod$             , ch(02),~
               at (11,02), "Journal ID Range",                           ~
               at (11,30), fac(lfac$( 6)), firstjnlid$          , ch(03),~
               at (11,43), fac(lfac$( 6)), lastjnlid$           , ch(03),~
               at (12,02), "Posting Sequence Range",                     ~
               at (12,30), fac(lfac$( 7)), firstpstseq$         , ch(10),~
               at (12,43), fac(lfac$( 7)), lastpstseq$          , ch(10),~
               at (13,02), "Reference 1 Range",                          ~
               at (13,30), fac(lfac$( 8)), rangerf1$(1%)        , ch(34),~
               at (14,30), fac(lfac$( 8)), rangerf1$(3%)        , ch(34),~
               at (15,02), "Reference 2 Range",                          ~
               at (15,30), fac(lfac$( 9)), rangerf1$(2%)        , ch(34),~
               at (16,30), fac(lfac$( 9)), rangerf1$(4%)        , ch(34),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (22,02), "(1)Start Over",                              ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,39), "(14)Select Archive Year",                    ~
               at (24,65), fac(hex(8c)), exitmsg$(1%),                   ~
                                                                         ~
               keys(hex(00010d0e0f10)), key (keyhit%)

               if keyhit% <> 13 then L40760
                  call "MANUAL" ("GLDETRPT")
                  return

L40760:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *        P R I N T   M O D E   I N P U T   S C R E E N  2   *~
            *-----------------------------------------------------------*~
            * Gets Sort Information For Report To Print.                *~
            *************************************************************
            deffn'202(fieldnr%)
                  str(prgmid$,1,50) = "Report Sort Option Selection"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  exitmsg$(1%) = " "
                  option1$, option2$ = " "
                  if s% < 1% or s% > 2% then goto L41080
                        option1$ = option$(3%-s%)
                        option2$ = option$(5%-s%)
L41080:           on fieldnr% gosub L41160,         /* MODULE SORT      */~
                                    L41160,         /* ACCOUNT SORT     */~
                                    L41160,         /* JNL/REF 2 TOTAL  */~
                                    L41160          /* SEQ/REF1 TOTOL   */
                  goto L41220
                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L41220:     accept                                                       ~
               at (01,02), "Print General Ledger Report",                ~
               at (01,67), "Date: ",                                     ~
               at (01,40), "Archive Year:",fac(hex(8c)),arcyear$, ch(07),~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,15), "Report Sort",                                ~
               at (07,15), "-----------",                                ~
               at (08,15), "Sort By",                                    ~
               at (08,23), fac(lfac$(1)), sortby$               , ch(01),~
               at (09,17), "1. Module",                                  ~
               at (10,17), "2. Account",                                 ~
               at (06,40), "Report Subtotal",                            ~
               at (07,40), "---------------",                            ~
               at (08,40), "Subtotal By",                                ~
               at (08,53), fac(lfac$(2)), subtotal$             , ch(01),~
               at (09,42), "1. NO Subtotal",                             ~
               at (10,42), fac(hex(8c)), option1$               , ch(15),~
               at (11,42), fac(hex(8c)), option2$               , ch(15),~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  fac(hex(8c)), exitmsg$(1%),                            ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L41580
                  call "MANUAL" ("GLDTLRPT")
                  return

L41580:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *        P R I N T   M O D E   I N P U T   S C R E E N  3   *~
            *-----------------------------------------------------------*~
            * Gets Sort Information For Report To Print.                *~
            *************************************************************
            deffn'203(fieldnr%)
                  str(prgmid$,1,50) = "Report Subtotal Option Selection"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  exitmsg$(1%) = " "
                  if a% = 2% then t% = 3% else t% = 1%
                  on fieldnr% gosub L42190          /* SUBTOTAL 2       */
                  goto L42220
                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L42190:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L42220:     accept                                                       ~
               at (01,02), "Print General Ledger Detail",                ~
               at (01,67), "Date: ",                                     ~
               at (01,40), "Archive Year:",fac(hex(8c)),arcyear$, ch(07),~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Enter Subtotal Options",                              ~
               at (06,30), fac(lfac$( 1)), subtotal2$           , ch(02),~
               at (08,04), " 1. No additional options",                  ~
               at (09,04), " 2. Subtotal by",                            ~
               at (09,20), fac(hex(8c)), str(option$(t%),3)     , ch(15),~
               at (10,04), " 3. Customer code",                          ~
               at (11,04), " 4. Customer code/Document ID",              ~
               at (12,04), " 5. Customer code/Invoice No.",              ~
               at (13,04), " 6. Customer code/Part No.",                 ~
               at (14,04), " 7. Vendor code",                            ~
               at (15,04), " 8. Vendor code/Document ID",                ~
               at (16,04), " 9. Vendor code/Invoice or PO No.",          ~
               at (17,04), "10. Vendor code/Part No.",                   ~
               at (18,04), "11. Part No.",                               ~
               at (19,04), "12. Part No./Store ID",                      ~
               at (08,43), "13. Part No./Store ID & Lot ID",             ~
               at (09,43), "14. Part No./Job No.",                       ~
               at (10,43), "15. Job No.",                                ~
               at (11,43), "16. Job No./Part",                           ~
               at (12,43), "17. Job No./Employee code",                  ~
               at (13,43), "18. Employee Code",                          ~
               at (14,43), "19. Employee Code/Deduction code",           ~
               at (15,43), "20. Employee Code/Earning code",             ~
               at (16,43), "21. Employee code/Job No.",                  ~
               at (17,43), "22. Dedcution code/Employee code",           ~
               at (18,43), "23. Earning code/Employee code",             ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  fac(hex(8c)), exitmsg$(1%),                            ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L42700
                  call "MANUAL" ("GLDTLRPT")
                  return

L42700:        if keyhit% <> 15 then return
                  call "PRNTSCRN"

                  return

        REM *************************************************************~
            *        P R I N T   M O D E   E D I T   S C R E E N        *~
            *                                                           *~
            * GETS RANGE OF ACCOUNT NUMBERS TO PRINT.                   *~
            *************************************************************

            deffn'211(fieldnr%)
                  str(prgmid$,1,50) = "Report Range Selection"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  exitmsg$() = " "
                  if fieldnr% > 0% then L44092
                      exitmsg$(1%) = "(14)Display Rpt "
                      exitmsg$(2%) = "(32)Background  "
                      exitmsg$(3%) = "(16)Print Report"

L44092:           pf5lit$ = "(5)Total Options"
                  if bygr% = 1% then pf5lit$ = " "
                  on fieldnr% gosub L44250,         /* G/L system to use*/~
                                    L44220,         /* Date Range       */~
                                    L40220,         /* Group Range      */~
                                    L44220,         /* Account Range    */~
                                    L44220,         /* Module Range     */~
                                    L44220,         /* Journal Range    */~
                                    L44220,         /* Seq # Range      */~
                                    L44220,         /* First Ref 2      */~
                                    L44220          /* First Ref 1      */
                  goto L44280

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L44220:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L44250:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L44280:     accept                                                       ~
               at (01,02), "Print General Ledger Detail",                ~
               at (01,67), "Date: ",                                     ~
               at (01,40), "Archive Year:",fac(hex(8c)),arcyear$, ch(07),~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), fac(hex(8c)), setmsg$                , ch(26),~
               at (06,30), fac(lfac$( 1)), set$                 , ch(01),~
               at (06,33), fac(hex(8c)),   setdescr$            , ch(30),~
               at (07,02), "Date Range",                                 ~
               at (07,30), fac(lfac$( 2)), starting$            , ch(10),~
               at (07,43), fac(lfac$( 2)), ending$              , ch(10),~
               at (08,02), "Account Group Range",                        ~
               at (08,30), fac(lfac$( 3)), firstgroup$          , ch(06),~
               at (08,43), fac(lfac$( 3)), lastgroup$           , ch(06),~
               at (09,02), "Account Code Range",                         ~
               at (09,30), fac(lfac$( 4)), firstaccount$        , ch(12),~
               at (09,43), fac(lfac$( 4)), lastaccount$         , ch(12),~
               at (10,02), "Module ID Range",                            ~
               at (10,30), fac(lfac$( 5)), firstmod$            , ch(03),~
               at (10,43), fac(lfac$( 5)), lastmod$             , ch(02),~
               at (11,02), "Journal ID Range",                           ~
               at (11,30), fac(lfac$( 6)), firstjnlid$          , ch(03),~
               at (11,43), fac(lfac$( 6)), lastjnlid$           , ch(03),~
               at (12,02), "Posting Sequence Range",                     ~
               at (12,30), fac(lfac$( 7)), firstpstseq$         , ch(10),~
               at (12,43), fac(lfac$( 7)), lastpstseq$          , ch(10),~
               at (13,02), "Reference 1 Range",                          ~
               at (13,30), fac(lfac$( 8)), rangerf1$(1%)        , ch(34),~
               at (14,30), fac(lfac$( 8)), rangerf1$(3%)        , ch(34),~
               at (15,02), "Reference 2 Range",                          ~
               at (15,30), fac(lfac$( 9)), rangerf1$(2%)        , ch(34),~
               at (16,30), fac(lfac$( 9)), rangerf1$(4%)        , ch(34),~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (22,02), "(1)Start Over",                              ~
               at (23,20), fac(hex(8c)), pf5lit$,                        ~
               at (23,48), fac(hex(8c)), exitmsg$(1%),                   ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,48), fac(hex(8c)), exitmsg$(2%),                   ~
               at (24,65), fac(hex(8c)), exitmsg$(3%),                   ~
                                                                         ~
               keys(hex(0001050d0e0f1020)), key (keyhit%)

               if keyhit% <> 13 then L44730
                  call "MANUAL" ("GLDTLRPT")

L44730:        if keyhit% <> 15 then L44760
                  call "PRNTSCRN"

L44760:        close ws
               call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
               return

        REM *************************************************************~
            *        P R I N T   M O D E   E D I T   S C R E E N  2     *~
            *                                                           *~
            * GETS SORT INFORMATION FOR REPORT TO PRINT.                *~
            *************************************************************
            deffn'212(fieldnr%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  str(prgmid$,1,50) = "Report Sort Option Selection"
                  option1$, option2$ = " "
                  if s% < 1% or s% > 2% then goto L44870
                        option1$ = option$(3%-s%)
                        option2$ = option$(5%-s%)
L44870:           exitmsg$() = " "
                  if fieldnr% > 0% then L44882
                      exitmsg$(1%) = "(14)Display Rpt "
                      exitmsg$(2%) = "(32)Background  "
                      exitmsg$(3%) = "(16)Print Report"
L44882:           if sortby$ = "2" then nextmsg$ = "(5)Subtotal Option"  ~
                                  else nextmsg$ = " "
                  on fieldnr% gosub L44980,         /* MODULE SORT      */~
                                    L44980,         /* ACCOUNT SORT     */~
                                    L44980,         /* JNL/REF 2 TOTAL  */~
                                    L44980          /* SEQ/REF1 TOTOL   */
            goto L45040

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L44980:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L45040:     accept                                                       ~
               at (01,02), "Print General Ledger Detail",                ~
               at (01,67), "Date: ",                                     ~
               at (01,40), "Archive Year:",fac(hex(8c)),arcyear$, ch(07),~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,15), "Report Sort",                                ~
               at (07,15), "-----------",                                ~
               at (08,15), "Sort By",                                    ~
               at (08,23), fac(lfac$(1)), sortby$               , ch(01),~
               at (09,17), "1. Module",                                  ~
               at (10,17), "2. Account",                                 ~
               at (06,40), "Report Subtotal",                            ~
               at (07,40), "---------------",                            ~
               at (08,40), "Subtotal By",                                ~
               at (08,53), fac(lfac$(2)), subtotal$             , ch(01),~
               at (09,42), "1. NO Subtotal",                             ~
               at (10,42), fac(hex(8c)), option1$               , ch(15),~
               at (11,42), fac(hex(8c)), option2$               , ch(15),~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20),                                               ~
                  "(4)Range Options",                                    ~
               at (24,20),                                               ~
                  fac(hex(8c)), nextmsg$,                                ~
               at (23,48), fac(hex(8c)), exitmsg$(1%),                   ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,48), fac(hex(8c)), exitmsg$(2%),                   ~
               at (24,65), fac(hex(8c)), exitmsg$(3%),                   ~
                                                                         ~
               keys(hex(000104050d0e0f1020)),                            ~
               key (keyhit%)

               if keyhit% <> 13 then L45400
                  call "MANUAL" ("GLDTLRPT")
                  return

L45400:        if keyhit% <> 15 then L45430
                  call "PRNTSCRN"

L45430:        close ws
               call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
               return

        REM *************************************************************~
            *        P R I N T   M O D E   I N P U T   S C R E E N  3   *~
            *                                                           *~
            * GETS SORT INFORMATION FOR REPORT TO PRINT.                *~
            *************************************************************
            deffn'213(fieldnr%)
                  str(prgmid$,1,50) = "Report Subtotal Option Selection"
                  init(hex(86)) lfac$()
                  exitmsg$() = " "
                  if fieldnr% > 0% then L46080
                      exitmsg$(1%) = "(14)Display Rpt "
                      exitmsg$(2%) = "(32)Background  "
                      exitmsg$(3%) = "(16)Print Report"
L46080:           if a% = 2% then t% = 3% else t% = 1%
                  on fieldnr% gosub L46170          /* SUBTOTAL 2       */
                  goto L46200
                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L46170:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L46200:     accept                                                       ~
               at (01,02), "Print General Ledger Report",                ~
               at (01,67), "Date: ",                                     ~
               at (01,40), "Archive Year:",fac(hex(8c)),arcyear$, ch(07),~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Enter Subtotal Options",                              ~
               at (06,30), fac(lfac$( 1)), subtotal2$           , ch(02),~
               at (08,04), " 1. No additional options",                  ~
               at (09,04), " 2. Subtotal by",                            ~
               at (09,20), fac(hex(8c)), str(option$(t%),3)     , ch(15),~
               at (10,04), " 3. Customer code",                          ~
               at (11,04), " 4. Customer code/Document ID",              ~
               at (12,04), " 5. Customer code/Invoice No.",              ~
               at (13,04), " 6. Customer code/Part No.",                 ~
               at (14,04), " 7. Vendor code",                            ~
               at (15,04), " 8. Vendor code/Document ID",                ~
               at (16,04), " 9. Vendor code/Invoice or PO No.",          ~
               at (17,04), "10. Vendor code/Part No.",                   ~
               at (18,04), "11. Part No.",                               ~
               at (19,04), "12. Part No./Store ID",                      ~
               at (08,43), "13. Part No./Store ID & Lot ID",             ~
               at (09,43), "14. Part No./Job No.",                       ~
               at (10,43), "15. Job No.",                                ~
               at (11,43), "16. Job No./Part",                           ~
               at (12,43), "17. Job No./Employee code",                  ~
               at (13,43), "18. Employee Code",                          ~
               at (14,43), "19. Employee Code/Deduction code",           ~
               at (15,43), "20. Employee Code/Earning code",             ~
               at (16,43), "21. Employee code/Job No.",                  ~
               at (17,43), "22. Dedcution code/Employee code",           ~
               at (18,43), "23. Earning code/Employee code",             ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20),                                               ~
                  "(4)Sort Options",                                     ~
               at (23,48), fac(hex(8c)), exitmsg$(1%),                   ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,48), fac(hex(8c)), exitmsg$(2%),                   ~
               at (24,65), fac(hex(8c)), exitmsg$(3%),                   ~
                                                                         ~
               keys(hex(0001040d0e0f1020)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L46680
                  call "MANUAL" ("GLDTLRPT")
                  return

L46680:        if keyhit% <> 15 then L46710
                  call "PRNTSCRN"
                  return

L46710:        close ws
               call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
               return

        REM *************************************************************~
            * T E S T   D A T A   F O R   S I N G L E   A C C O U N T S *~
            *-----------------------------------------------------------*~
            * Test Data for items on Screen #1                          *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50190,         /* G/L system to use*/~
                                    L50320,         /* DATE RANGE       */~
                                    L50420,         /* Group Range      */~
                                    L50570,         /* ACCOUNT RANGE    */~
                                    L50720,         /* MODULE RANGE     */~
                                    L50770,         /* JOURNAL RANGE    */~
                                    L50820,         /* SEQUENCE RANGE   */~
                                    L50920,         /* REF 1 RANGE      */~
                                    L50970          /* REF 2 RANGE      */
                  return

L50190: REM G/L system to use SET$
            setdescr$, sethdr$ = " " : set = 1 : main% = 1% : detl% = 2%
            grpm% = 7%  :  grpl% = 8%
            call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ <> " " then return
                setdescr$ = "Statutory"
                if set = 1 then goto L50270
                    setdescr$ = "Local Authority"
                    main% = 11% : detl% = 12%
                    grpm% = 17% : grpl% = 18%
L50270:     sethdr$ = setdescr$
            call "FMTTITLE" (sethdr$, "G/L SYSTEM", 12%)
            call "PUTPAREN" (setdescr$)
            return

L50320: REM Test Date Range
            if starting$ = "ALL"  then return
            if ending$ = " " then ending$ = starting$
            call "DATEOKC" (starting$, start%, errormsg$)
                if errormsg$ <> " " then return
            call "DATEOKC" (ending$, end%, errormsg$)
                if errormsg$ <> " " then return
            if end% >= start% then return
                errormsg$ = "Ending Date Must be Before Starting Date"
                return

L50420: REM Test for Group Code Range
            if bygr% = 1% and firstgroup$ = " " then L50520
            bygr% = 0%
            if firstgroup$ = " " then return
            if firstgroup$ = "ALL" then L50490
            if lastgroup$ = " " then lastgroup$ = firstgroup$
            if lastgroup$ < firstgroup$ then errormsg$ = "Invalid Range"
L50490:     bygr% = 1%
            return

L50520:     bygr% = 9%
            lastgroup$ = " "
            return

L50570: REM Test For Account Range
            if firstaccount$ = "ALL" then return
            if set = 1                                                   ~
                then call "GLVALID" (firstaccount$, temp$, errormsg$)    ~
                else call "GLVALD2" (firstaccount$, temp$, errormsg$)
                if errormsg$ <> " " then return
            if lastaccount$ = " " then lastaccount$ = firstaccount$
            if set = 1                                                   ~
                then call "GLVALID" (lastaccount$, temp$, errormsg$)     ~
                else call "GLVALD2" (lastaccount$, temp$, errormsg$)
            if errormsg$ <> " " then return
            if lastaccount$ < firstaccount$ then                         ~
                errormsg$ = "Invalid Range"
            return

L50720: REM Test For Module Range
            if firstmod$ = "ALL" or lastmod$ = " " then return
            if lastmod$ < firstmod$ then errormsg$ = "Invalid Range"
            return

L50770: REM Test For Journal Range
            if firstjnlid$ = "ALL" or lastjnlid$ = " " then return
            if lastjnlid$ < firstjnlid$ then errormsg$ = "Invalid Range"
            return

L50820: REM Test For Posting Sequence Range
            if firstpstseq$ = "ALL" then return
            convert firstpstseq$ to firstpstseq%, data goto L50890
            if lastpstseq$ = " " then return
            convert lastpstseq$ to lastpstseq%, data goto L50890
            if lastpstseq% < firstpstseq% then errormsg$ = "Invalid Range"
            return
L50890:     errormsg$ = "Invalid Posting Sequence Number"
            return

L50920: REM Test For Reference 1 Range
            if rangerf1$(1) = "ALL" or rangerf1$(3) = " " then return
            if rangerf1$(3)< rangerf1$(1) then errormsg$ = "Invalid Range"
            return

L50970: REM Test For Reference 2 Range
            if rangerf1$(2) = "ALL" or rangerf1$(4) = " " then return
            if rangerf1$(4)< rangerf1$(2) then errormsg$ = "Invalid Range"
            return

        REM *************************************************************~
            *      T E S T  D A T A  F O R  S O R T  R E Q U E S T      *~
            *-----------------------------------------------------------*~
            * Tests Sort Option - Allows Only One Option                *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
                on fieldnr% gosub L53200,           /* SORT OPTION      */~
                                  L53300            /* SUBTOTAL OPTION  */

            return

L53200: REM TEST SORT OPTION
            s% = pos("12" = sortby$)
            if s% <> 0% then return
            errormsg$ = "Sort Option Must Be '1' or '2'"
            return

L53300: REM TEST SUBTOTAL OPTION
            a% = pos("123" = subtotal$)
            if a% <> 0% then return
            errormsg$ = "Subtotal Option Must Be '1', '2', or '3'"
            return

        REM *************************************************************~
            *      T E S T  D A T A  F O R  S U B T O T A L             *~
            *                                                           *~
            * TESTS SUBTOTAL OPTION - ALLOWS ONLY ONE OPTION            *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub   L54200              /* SUBTOTAL OPTION   */
            return

L54200: REM TEST SUBTOTAL OPTION
            call "NUMTEST" (subtotal2$, 1,23, errormsg$, 0.0, subtotal)
            subtotal% = subtotal
            return

        REM *************************************************************~
            *           P R I N T  L I S T  R O U T I N E               *~
            *                                                           *~
            * PRINTS JOURNAL ENTRY LIST FROM EITHER THE BUFFER OR MASTER*~
            *************************************************************

L55060: %================================================================~
        ~=================================================================~
        ~===

L55100: %   Account Code: ############  Description: ####################~
        ~################     Status: ##############

L55130: % ######## ## ### -######## ######################### ###########~
        ~############## ####################### ############# ############~
        ~#

L55170: % ######### ######## ## ### ######################### ###########~
        ~############## ####################### ############# ############~
        ~#

L55210: %  ########################################                      ~
        ~                                       -#########.## -#########.#~
        ~#

L55250: %  ########################################                      ~
        ~                                       ############# ############~
        ~#

L55290: %  ########################################                      ~
        ~                                       -#########.## -#########.#~
        ~#

L55330: %  Module Number: ##  Journal ID: ###  Posting Sequence: ########~
        ~##  Title: ########################################

L55360: %  Module Number: ##  Journal ID: ###  Title: ###################~
        ~#####################

L55390: %  Module Number: ##                                             ~

L55410: %  Group Code: ###### - ##############################

        sub_heading

            call "DESCRIBE" (#main%, account$,description$,0%,f1%(main%))
            if f1%(main%) <> 0% then L55480
                 description$ = "** NOT ON FILE! **" : goto L55520
L55480:     get #main%, using L55490, temp$
L55490:     FMT POS(41), CH(1)
            acctstatus$ = "(Active)" : if temp$ = hex(01) or             ~
                          temp$ = hex(02) then acctstatus$ = "(Obsolete)"
L55520:     temp$ = account$
            if set = 1 then call "GLFMT" (temp$)                         ~
                       else call "GLFMT2" (temp$)
            if printline% + 4% > 58 then gosub heading
            printline% = printline% + 4%
            print skip(1)
            if sortflag$(1) <> " " then L55690
            sortflag% = pos(-sortflag$()<>(hex(20)))
            moduleno$ = wmodno$
            ret% = 2%  /* Info Only mode */
            call "JNLINFO"(moduleno$, wjnlid$, u3%, " ", titlj$, " ",    ~
                          #03, f1%(3), ret%)
            if ret% <> 0% then titlj$ = "** Unknown Journal **"
            on sortflag% gosub L55690, L55670, L55650, L55630
            goto L55720
L55630:     print using L55330, wmodno$, wjnlid$, wpstseq%, titlj$
            return
L55650:     print using L55360, wmodno$, wjnlid$, titlj$
            return
L55670:     print using L55390, wmodno$
            return
L55690:     print using L55100, temp$, description$, acctstatus$
            print using L55760
            return
L55720:     print skip(1)
            print using L55760
            return

L55760: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---

L55800: % Date     MD JNL    Seq No Reference 1               Reference 2~
        ~               Description                     Debit         Cred~
        ~it

L55840: % Account   Date     MD JNL Reference 1               Reference 2~
        ~               Description                     Debit         Cred~
        ~it

L55880: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---

        heading
            pageno% = pageno% + 1
L55940:     print page
            print using L55980, date$, time$, compname$,                  ~
                               str(prgmid$,62,8) & "-" & rptid$

L55980: % RUN ######## ########              ############################~
        ~##################################                   ############~
        ~###

L56020: %                                    ############################~
        ~##################################

L56050: % ARCHIVE YEAR: #######                 G E N E R A L  L E D G E ~
        ~R  D E T A I L  L I S T I N G                             PAGE: #~
        ~###

            print using L56050, arcyear$, pageno%
            print using L56020, sethdr$
            print skip (1)
            print using L55060
            if pageno% = 0% then L56180
                if sortflag$(1)<>" " then print using L55800              ~
                                     else print using L55840
            print using L55880
            printline% = 7%
L56180:     return


        print_page_zero
            gosub L55940
            print skip(5)
            print using L56630
            print skip(2)
            if dual_books$ = "Y" then print using L56680,                 ~
                    "G/L System to Use", set$ & "  " & setdescr$, " "

            print using L56680, "Range for Dates", starting$, ending$
            print using L56680, "Range for Group", firstgroup$, lastgroup$
            print using L56680, "Range of Accounts", firstaccount$,       ~
                                lastaccount$
            print using L56680, "Range for Modules", firstmod$, lastmod$
            print using L56680, "Range for Journals", firstjnlid$,        ~
                                lastjnlid$
            print using L56680, "Range for Posting Sequence",firstpstseq$,~
                                lastpstseq$
            print using L56680, "Range For Reference 1", rangerf1$(1%),   ~
                                rangerf1$(3%)
            print using L56680, "Range For Reference 2", rangerf1$(2%),   ~
                                rangerf1$(4%)
            print skip( 1)
            if sortby$ = "1" then print using L56710, "Sort By Module"    ~
                             else print using L56710, "Sort By Account"
            if sortby$ = "1" and subtotal$ = "1" then print using L56730, ~
                                 "None"
            if sortby$ = "1" and subtotal$ <> "1" then print using L56730,~
                                 str(option$((2 * a%) - 2), 4%, 11%)
            if sortby$ = "2" and subtotal$ = "1" then print using L56730, ~
                                 "None"
            if sortby$ = "2" and subtotal$ <> "1" and subtotal% = 1%     ~
                then print using L56730, str(option$((2*a%)-3), 4%, 11%)

            if sortby$ = "2" and subtotal$ <> "1" and subtotal% = 2%     ~
                then print using L56730,                                  ~
                           str(option$((2 * a%) - 3), 4%, 11%) & "/" &   ~
                           str(option$(t%), 4%, 11%)
            if sortby$ = "2" and subtotal$ <> "1" and subtotal% > 2%     ~
                then print using L56730, subtotop$(subtotal% - 2%)
            printline% = 100%
            return

L56630: %                                       R E P O R T  S E L E C T ~
        ~I O N S :

        %                                                                ~

L56680: %                    ##########################  ################~
        ~################# TO ##################################

L56710: %                    SORT OPTION:               #################

L56730: %                    SUBTOTAL OPTION:           #################~
        ~#######

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND DISPLAYS A       *~
            * MESSAGE (ONLY IN FOREGROUND) WHILE LINKING TO THE NEXT    *~
            * PROGRAM.                                                  *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
