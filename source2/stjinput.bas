        REM *************************************************************~
            *                                                           *~
            *   SSS   TTTTT  JJJJJ  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  S        T      J      I    NN  N  P   P  U   U    T     *~
            *   SSS     T      J      I    N N N  PPPP   U   U    T     *~
            *      S    T    J J      I    N  NN  P      U   U    T     *~
            *   SSS     T     J     IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STJINPUT - INPUT STANDARD JOURNAL ENTRIES.                *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 06/04/80 ! ORIGINAL (FROM GNJINPUT)                 ! BCW *~
            * 10/08/80 ! CLEAN UP; NEW TABULAR SCREEN FORMATS     ! TEM *~
            * 11/18/80 ! TEST GL DATE FOR EXISTENCE AND OPENNESS  ! TEM *~
            * 08/11/81 ! AUTOREVERSE=SAVE ON FILE; TABS ON TABULAR! TEM *~
            * 05/26/82 ! ADDED INSTRUCTIONS                       ! ECR *~
            * 11/22/82 ! ADDED MORE INSTRUCTIONS                  ! ECR *~
            * 11/23/82 ! FIXED *BUG* IN INSERTMODE (15230)        ! BEV *~
            * 01/09/84 ! ADDED 'STOP DATE' <U.S.E. MODIFICATION>  ! LDJ *~
            * 07/12/84 ! ADDED OPTION TO EXIT WITHOUT POSTING     ! BLT *~
            * 07/12/84 ! ADDED CHECK BETWEEN SAVEFLAG & REVERSEFLG! BLT *~
            * 07/26/84 ! Forced "Save in main file option" = YES  ! ERN *~
            * 01/23/85 ! CORRECTED *BUG* IN DELETE LINEITEM(15535)! RAC *~
            *          ! ADDED ERRORMSG IF LINEITEM > 100 (30395) ! RAC *~
            *          ! AND ADDED PF(4)PREV IN INSERT MODE(15395)! RAC *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! INTERFACE FILE RECORD SIZE FOR PASSING   !     *~
            *          ! MODULE, JOURNAL, AND POSTING SEQUENCE    !     *~
            * 08/16/85 ! Another cleanup after RAC, I swear that  ! LDJ *~
            *          !   boy can't count, STJBUF2 record length !     *~
            *          !   as written was short, had to add       !     *~
            *          !   filler.                                !     *~
            * 03/18/88 ! Standardized & added PLOWCODE to STJBUFFR! MJB *~
            * 06/06/89 ! Adjusted Line Count Logic                ! MJB *~
            * 10/06/89 ! Changed PF literal at line item input scr! JDH *~
            * 04/20/90 ! Increased size of debit & credit fields. ! JDH *~
            * 05/09/90 ! Added test so that reversing journal can ! JDH *~
            *          !  NOT be posted again if it hasn't been   !     *~
            *          !  reversed.  Flag in header warns us.     !     *~
            * 06/13/91 ! Removed FNX and used ROUND. Added ALLFREE! JDH *~
            * 06/26/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            call "SHOSTAT" ("Opening Files, One Moment Please")

        dim acct$(100)16,                /* ACCOUNT NUMBERS FOR ENTRY  */~
            adj$4,                       /* Adj Flag - Filler - N/A    */~
            blankdate$8,                 /* Blank date for comparison  */~
            blankline$79,                /* FOR UNDERLINING            */~
            credit$(100)12,              /* CREDIT AMOUNTS THIS ENTRY  */~
            credit1$13,                  /* CREDIT TO PRINT            */~
            cursor%(2),                  /* CURSOR LOCATION            */~
            dates$(17)8,                 /* FISCAL PERIODS DATE TABLE  */~
            debit$(100)12,               /* DEBIT AMOUNTS THIS ENTRY   */~
            debit1$13,                   /* DEBIT TO PRINT             */~
            deletemsg$22,                /* DELETE ENTRY MESSAGE       */~
            descr$(100)32,               /* Account Descriptions       */~
            description$36,              /* Description of Entry       */~
            edtcoltran$(2)80,            /* AND ALSO WHICH COLUMN(FIELD*/~
            edtmessage$79,               /* "TO MODIFY DISPLAYED.."TEXT*/~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(20,6)1,                 /* FAC'S FOR TABULAR INPUT    */~
            firstentry$10,               /* FIRST JOURNAL ENTRY TO PRIN*/~
            firstdsply$10,               /* FIRST JOURNAL ENTRY TO PRIN*/~
            gldate$8,                    /* USER'S GL DATE             */~
            hdr$(3)30,                   /* Headers for PLOWCODE       */~
            header$100,                  /* HEADER THIS ENTRY - 1 FIELD*/~
            hdrdate$45,                  /* FORMATTED DATE/TIME        */~
            i$(24)80,                    /* JUNK VARIABLE--HOLDS SCREEN*/~
            inputdate$8,                 /* ARGUMENT FOR 'DATE' CALL   */~
            infomsg$79,                  /* INFORMATIVE MESSAGE        */~
            jnltitle$30,                 /* SUB TITLE OF PRINT LIST    */~
            lastentry$10,                /* LAST JOURNAL ENTRY TO PRINT*/~
            lastdsply$10,                /* LAST JOURNAL ENTRY TO PRINT*/~
            linefac$(10)1,               /* FAC'S FOR LINEAR MODE      */~
            line2$79,                    /* Screen line 2              */~
            name$10,                     /* JOURNAL ENTRY NAME         */~
            nfac$(3)1,                   /* FAC'S FOR PRINT LIST SCRN  */~
            outdate$6,                   /* ARGUMENT FOR 'DATE' CALL   */~
            pagebreak$3,                 /* PAGE BREAK INDICATOR       */~
            pfkeys$(4)17,                /* TABULAR FUNCTION KEY LISTS */~
            plowdescr$60,                /* Description for PLOW       */~
            postedflag$1,                /* Has journal been posted?   */~
            prtdate$8,                   /* PRINT DETAIL RECORD DATE   */~
            range$30,                    /* RANGE PRINT LIST           */~
            readkey$50,                  /* KEY FOR PLOW ROUTINE       */~
            ref1$(100)30,                /* Reference 1                */~
            ref2$(100)34,                /* Reference 2                */~
            reverseflag$3,               /* WHETHER OR NOT TO REVERSE  */~
            ruserid$3,                   /* USERID OF INPUT RECORD     */~
            saveflag$3,                  /* WHETHER OR NOT TO SAVE IT  */~
            stopdate$6,                  /* DATE ENTRY TO BE DELETED   */~
            stpdate$10,                   /* WORKING STOP DATE VARIABLE */~
            stpdate2$8,                  /* DEFAULT " "    " "   ""    */~
            tdate$8,                     /* Temp Date Variable         */~
            title$(4,2)60,               /* TABULAR PF KEY TITLES      */~
            udate$8,                     /* Unformatted text date      */~
            userid$3                     /* USERID OF CURRENT USER     */~

        dim f2%(64),                     /* File status flags          */~
            f1%(64),                     /* Record-on-file flags       */~
            rslt$(64)20                  /* Return code from OPENCHCK  */

            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE (ACCT DESCR'S)  *~
            * # 3 ! SYSFILE2 ! SYSTEM INFORMATION...MONTHS OPEN         *~
            * # 5 ! STJMASTR ! STANDARD JOURNAL MASTER FILE             *~
            * # 6 ! STJLINES ! STANDARD JOURNAL LINE ITEMS FILE         *~
            * # 9 ! STJBUFFR ! STANDARD JOURNAL HEADER BUFFER AREA.     *~
            * #10 ! STJBUF2  ! STANDARD JOURNAL DETAIL BUFFER AREA.     *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1 , keylen = 3

            select  #2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #3, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #5, "STJMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 10

            select  #6, "STJLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 13

            select  #9, "STJBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 7,                          ~
                        alt key 1, keypos= 8, keylen= 7,                 ~
                            key 2, keypos=15, keylen=10

            select #10, "STJBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 13

            call "OPENCHCK" (# 1, 0%, f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, 0%, f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (# 6, 0%, f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (# 9, 0%, f2%( 9), 100%, rslt$( 9))
            call "OPENCHCK" (#10, 0%, f2%(10), 100%, rslt$(10))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES VARIOUS PROMPTS USED IN SCREEN DISPLAYS.      *~
            *************************************************************

            edtmessage$ = "To modify displayed values, position cursor to~
        ~ desired line and press RETURN."

            init(hex(00)) edtcoltran$()
            init(hex(01)) str(edtcoltran$(1),  1, 13)
            init(hex(02)) str(edtcoltran$(1), 14, 34)
            init(hex(03)) str(edtcoltran$(1), 48, 33)
            init(hex(04)) str(edtcoltran$(2),  1, 50)
            init(hex(05)) str(edtcoltran$(2), 51, 14)
            init(hex(06)) str(edtcoltran$(2), 65, 16)

            blankdate$ = " "
	    call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            str(line2$,62) = "STJINPUT: " & str(cms2v$,,8)

        REM Check if GL date one of the months open
            call "READ100" (#1, userid$, f1%(1))
                if f1%(1) = 1 then L09280
L09235:     ask% = 2%
            call "ASKUSER" (ask%, "***** NO G/L DATE AVAILABLE ****",    ~
                            "Cannot find your G/L Date",                 ~
                            "Press PF16 to acknowledge and exit", " ")
            if ask% <> 16% then L09235
            f1%(9) = 0%
            goto L65180

L09280: REM Is the month open
            get #1, using L09300, gldate$
L09300:         FMT XX(21), CH(6)
            call "WHICHMON" (#3, gldate$, thismonth%)
            if 0 < thismonth% and 4 > thismonth% then L09390
L09325:     ask% = 2%
            call "ASKUSER" (ask%, "***** G/L DATE NOT VALID ****",       ~
                            "Your G/L Date is not in a currently open" & ~
                            " month", "Press PF16 to exit this program", ~
                            "and reset your G/L date", " ")
            if ask% <> 16% then L09325
            f1%(9) = 0%
            goto L65180

        REM Get end fiscal year date for default stop date value
L09390:     call "READ100" (#3, "FISCAL DATES", f1%(3))
                if f1%(3) = 1 then L09450
L09405:     ask% = 2%
            call "ASKUSER" (ask%, "**** NO FISCAL CALENDAR ****",        ~
                            "There is no fiscal calendar available",     ~
                            "Press PF16 to Exit this program",           ~
                            "and set up a fiscal calendar")
            if ask% <> 16% then L09405
            f1%(9) = 0%
            goto L65180

L09450: REM Get the month & compute end fiscal date
            get #3, using L09470, dates$()
L09470:         FMT XX(22), 17*CH(08)
            tdate$ = dates$(1)
	    call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$,1%,4%) to year%
            convert year% + 1% to str(inputdate$,1%,4%), pic (0000)
            str(inputdate$,5%,4%) = str(udate$,5%,4%)
            call "DATEFMT" (inputdate$)
	    call "DATUNFMT" (inputdate$)
            year% = - 1
            call "DATE" addr( "G+", inputdate$, year%, outdate$, ret%)
            stpdate2$ = outdate$
            call "DATEFMT" (stpdate2$)
            title$(1,1) = "(1)Start Over(2)Col 1(4)Line Above(13)Instrs(1~
        ~5)Prt Scrn"
            title$(1,2) = "(16)Return   "
            title$(2,1) = "(1)Start Over(2)First(3)Last(4)Prev(5)Next(6)D~
        ~own 7)Up"
            title$(2,2) = "(9)Header(11)Ins(12)Del(13)Instrs(15)Prt Scrn ~
        ~(16)Save Data"
            title$(3,1) = "Supply requested items or 4(Prev) and (ENTER)"
            title$(3,2) = "or (1)to exit insert mode"
            title$(4,1) = "Press RETURN  to DELETE flashing line or (1) t~
        ~o EXIT Delete."

            pfkeys$(1) = hex(000102040d0f10ffffffffffffffffffff)
            pfkeys$(2) = hex(0001020304050607090b0c0d0f10ffffff)
            pfkeys$(3) = hex(0001040d0fffffffffffffffffffffffff)
            pfkeys$(4) = hex(00010d0fffffffffffffffffffffffffff)

        REM *************************************************************~
            *                  I N P U T   H E A D E R                  *~
            *-----------------------------------------------------------*~
            * Inputs header, loads old checks if on file, etc...        *~
            *************************************************************

        inputmode
            init(" ") name$, description$, acct$(), descr$(),            ~
                      debit$(), credit$(), errormsg$, infomsg$,          ~
                      blankline$, reverseflag$, saveflag$, stopdate$,    ~
                      stpdate$, ref1$(), ref2$(), edtmessage$
L10100:     date%, ret%, editmode% = 0
            saveflag$ = "YES"
            prtall% = 1
            total   = 0
            call "ALLFREE"

            for fieldnr% = 1 to 5
                if fieldnr% = 3 then goto L10190
L10130:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  9 then print_master
                      if keyhit%  = 10 then print_buffer
                      if keyhit%  = 11 then delete_entry
                      if keyhit%  = 12 then delete_buffer
                      if keyhit%  = 16  and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then L10130
                gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10130
L10190:         next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *-----------------------------------------------------------*~
            * Inputs line items and tests for validity.                 *~
            *************************************************************

            maxlines%, screenline%, currentline% , line% = 0

L11080:     screenline% = screenline% + 1
            currentline% = currentline% + 1
            if screenline% <= 10 then L11150
               screenline% = 1
               line% = line% + 10
               if line% = 100  then editmode

L11150:     infomsg$ = " "
            for fieldnr% = 1 to 6
                gosub'160(fieldnr%)
                      if enabled% = 0 then L11270
L11190:         gosub'203(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub columnone
                      if keyhit%  =  4 then gosub lineabove
                      if keyhit%  = 13 then call "MANUAL" ("STJINPUT")
                      if keyhit%  = 16 and fieldnr% = 1 then editmode
                      if keyhit% <>  0 then       L11190
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L11190
L11270:         next fieldnr%
            maxlines% = maxlines% + 1

            gosub L29000
            goto  L11080

        REM *************************************************************~
            *       E D I T   H E A D E R   I N F O R M A T I O N       *~
            *                                                           *~
            * EDITS HEADER INFORMATION.                                 *~
            *************************************************************

        editmode
            init(" ") errormsg$, infomsg$, edtmessage$
            editmode% = 1

L12100:     gosub'202(0%)
                  if keyhit% =  1 then gosub startover
                  if keyhit% =  2 then       editlines
                  if keyhit% = 16 then       datasave
                  if keyhit% <> 0 then       L12100
            fieldnr% = max(0, cursor%(1) - 5)
            if fieldnr% = 3 then goto L12100
            if fieldnr% <= 0 or fieldnr% > 5 then L12100

L12180:     gosub'202(fieldnr%)
                  if keyhit% =  1 then gosub startover
                  if keyhit% <> 0 then       L12180
            gosub'150(fieldnr%)
                  if errormsg$ <> " " then L12180
            goto editmode

        REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *-----------------------------------------------------------*~
            * Edits line items, goes and saves data to the file, etc.   *~
            *************************************************************

        editlines
            line%, currentline%, screenline% = 0
            gosub L29000

L13090:     gosub'213(0%)
                  if keyhit%  =  0 then       L13260
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       line%=0
                  if keyhit%  =  3 then       line%=max(0,maxlines%-10)
                  if keyhit%  =  4 then       line%=max(0,line%- 8)
                  if keyhit%  =  5 then       line%=min(line%+ 8,        ~
                                                 max(0,maxlines%-10))
                  if keyhit%  =  6 then       line%=max(0,line%-1)
                  if keyhit%  =  7 then       line%=min(line%+1,         ~
                                                 max(0, maxlines%-10))
                  if keyhit%  =  9 then       editmode
                  if keyhit%  = 11 then gosub insertmode
                  if keyhit%  = 12 then gosub deletemode
                  if keyhit%  = 13 then call "MANUAL" ("STJINPUT")
                  if keyhit%  = 16 then       datasave
                  goto L13090

L13260: REM Now figure out which field he hit.
            screenline% = max(0%,(cursor%(1) - 5%)/2 + 1)
            if screenline%  =  0 then L13090
            currentline% = screenline% + line%
            if currentline% > maxlines% then L13090
            whichrow% = mod((cursor%(1)-4%),2%)
            if whichrow% = 0 then                                        ~
                fieldnr% = val(str(edtcoltran$(2),cursor%(2)))           ~
                else                                                     ~
                fieldnr% = val(str(edtcoltran$(1),cursor%(2)))
            if fieldnr% = 0 then L13090
            if acct$(currentline%) = " " then fieldnr% = 2
            infomsg$ = " "

L13350:     gosub'213(fieldnr%)
                on keyhit% gosub startover
                if keyhit% <> 0 then L13350
                gosub'151(fieldnr%)
                    if errormsg$ <> " " then L13350

            gosub L29000
            goto L13090

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *-----------------------------------------------------------*~
            * Column one key and line above key functions handled here. *~
            *************************************************************

        columnone
            if fieldnr% = 1 then return
            acct$(currentline%), ref1$(currentline%),                    ~
            ref2$(currentline%), descr$(currentline%),                   ~
            debit$(currentline%), credit$(currentline%) = " "
            infomsg$ = " "
            fieldnr% = 1
            return

        lineabove
            if currentline% = 1 and fieldnr% = 2 then                    ~
                ref1$(currentline%) = name$
            if currentline% = 1 and fieldnr% = 4 then                    ~
                descr$(currentline%) = description$
            if currentline% = 1 and fieldnr% = 3 then                    ~
                ref2$(currentline%) = name$
            if currentline% = 1 then return
            on fieldnr% gosub L14220,     /* Account number             */~
                              L14223,     /* Reference 1                */~
                              L14227,     /* Reference 2                */~
                              L14230,     /* Account description        */~
                              L14240,     /* Debit amount this account  */~
                              L14250      /* Credit amount              */
            return

L14220:         acct$  (currentline%) = acct$  (currentline%-1)  : return
L14223:         ref1$  (currentline%) = ref1$  (currentline%-1)  : return
L14227:         ref2$  (currentline%) = ref2$  (currentline%-1)  : return
L14230:         descr$ (currentline%) = descr$ (currentline%-1)  : return
L14240:         debit$ (currentline%) = debit$ (currentline%-1)  : return
L14250:         credit$(currentline%) = credit$(currentline%-1)  : return

L15000: REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Handles insertion of a line item into the invoice.        *~
            *************************************************************

        insertmode
            if maxlines% = 100 then return         /* Array full, can't*/
        REM Otherwise, set CURRENTLINE%, SCREENLINE%, and copy right
            screenline% = max(0,(cursor%(1) - 5)/2)
            if line% + screenline% < maxlines% then L15120
                screenline% = maxlines% - line% /* to ins at end    */
L15120:     if screenline% <> 10 then L15150    /* bottom of page   */
                line% = line% + 1
                screenline% = screenline% - 1
L15150:     currentline%, c% = screenline% + line%

        REM Copy all the elements up one
            if c% >= maxlines% then L15260
                for temp% = maxlines% to c% step -1
                    acct$   (temp%+1) = acct$   (temp%)
                    ref1$   (temp%+1) = ref1$   (temp%)
                    ref2$   (temp%+1) = ref2$   (temp%)
                    descr$  (temp%+1) = descr$  (temp%)
                    debit$  (temp%+1) = debit$  (temp%)
                    credit$ (temp%+1) = credit$  (temp%)
                next temp%

L15260:     screenline% = screenline% + 1
            c%, currentline% = currentline% + 1

            init(" ") acct$(c%), descr$(c%), debit$(c%), credit$(c%),    ~
                      errormsg$, infomsg$, ref1$(c%), ref2$(c%)

        REM Now input the line, enable cancel out option
            infomsg$ = " "
            for fieldnr% = 1 to 6
                gosub'160(fieldnr%)
                    if enabled% = 0 then L15420
L15370:         gosub'223(fieldnr%)
                    if keyhit%  =  1 then L15490     /* end insert*/
                    if keyhit%  =  4 then gosub lineabove
                    if keyhit% <>  0 then L15370
                gosub'151(fieldnr%)
                    if errormsg$ <> " " then L15370
L15420:     next fieldnr%

            maxlines%  = maxlines% + 1
*          CURSOR%(1) = MIN(CURSOR%(1) + 2, 24)
            gosub L29000              /* re # lines, recompute total */
            goto L15000
L15480:
L15490: REM This routine aborts insert mode and destroys screenline%
            c% = currentline%
            if currentline% <= maxlines% then gosub L15620

            temp% = maxlines% + 1
            if temp% > 100 then temp% = 100
            init(" ") acct$(temp%), descr$(temp%), debit$(temp%),        ~
                      credit$(temp%), errormsg$, infomsg$,               ~
                      ref1$(temp%), ref2$(temp%)

            gosub L29000              /* Renumber & retotal entry   */
            if currentline% >= maxlines% and screenline% = 10            ~
                  then line% = max(0%, line%- 1%)
            return

L15620:     for temp% = currentline% to maxlines%
                acct$   (temp%) = acct$   (temp%+1)
                ref1$   (temp%) = ref1$   (temp%+1)
                ref2$   (temp%) = ref2$   (temp%+1)
                descr$  (temp%) = descr$  (temp%+1)
                debit$  (temp%) = debit$  (temp%+1)
                credit$ (temp%) = credit$ (temp%+1)
                next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Deletes a line item.                                      *~
            *************************************************************

        deletemode
            if maxlines% = 0 then return
            screenline% = (cursor%(1) - 5%)/2 + 1
            if screenline% < 1 then return
               currentline% = screenline% + line%
               if currentline% > maxlines% then return

L16130:     gosub'233(screenline%)
                if keyhit%  =  1 then return
                if keyhit% <>  0 then L16130

            c% = currentline%
            if currentline% < maxlines% then gosub L15480
                                         /* Actually delete line @C%   */
            temp% = maxlines%
            init(" ") acct$(temp%), descr$(temp%), debit$(temp%),        ~
                      credit$(temp%), errormsg$, infomsg$, ref1$(temp%), ~
                      ref2$(temp%)
            maxlines% = maxlines% - 1
            gosub L29000                  /* renumber & retotal entry   */
            return


        delete_entry
        REM *************************************************************~
            *  D E L E T E  R E Q E U S T E D  B U F F E R  E N T R Y   *~
            *-----------------------------------------------------------*~
            * Checks to make sure entry to delete is in buffer.         *~
            *************************************************************

            file1% = 5
            file2% = 6
            ky%    = 0
            goto L17150

        delete_buffer

            file1% = 9
            file2% = 10
            ky%    = 2

L17150:     for fieldnr% = 1 to 2
                gosub'054(fieldnr%)
                    if enabled% = 0 then L17250
L17180:         gosub'204(fieldnr%)
                    if keyhit% = 16 and fieldnr% = 2 then L17230
                    if keyhit% =  1 then gosub startover
                    if keyhit% = 16 and fieldnr% = 1 then inputmode
                    if keyhit% <> 0 and fieldnr% = 2 then L17180
L17230:         gosub'154(fieldnr%)
                    if errormsg$ <> " " then L17180
L17250:     next fieldnr%

            if keyhit% = 16 then L17150
            call "REDALT1" (#file1%, name$, ky%, f1%(file1%))
                if f1%(file1%) <> 0 then delete #file1%
            call "DELETE" (#file2%, name$, 10%)
            goto inputmode

        REM *************************************************************~
            *     C H E C K   S U M   A N D   W R I T E   E N T R Y     *~
            *-----------------------------------------------------------*~
            * Checks to make sure that the debits balance the credits   *~
            * if they don't, user gets screen telling how far off we are*~
            *************************************************************

        datasave
        REM First, total up journal entry; if not net 0, error.
            total = 0
            for temp% = 1 to maxlines%
                if acct$(temp%) = " " then L19150
                    convert debit$(temp%) to debit
                    convert credit$(temp%) to credit
                    total = total + debit - credit
L19150:     next temp%
            total = round(total, 2)

            if total = 0 then L19210
                gosub L46000
                goto editmode

L19210: REM Now save the journal to the buffer.
            call "REDALT1" (#9, name$, 2%, f1%(9))
                if f1%(9) <> 0 then delete #9
            call "DELETE" (#10, name$, 10%)
            gosub L31000

        REM Set name of last entry
            lastentry1$ = name$
            goto inputmode

        REM *************************************************************~
            *   I N P U T   E N A B L E   T A B U L A R   F I E L D S   *~
            *-----------------------------------------------------------*~
            * Enable input of each field in the table.                  *~
            *************************************************************

            deffn'160(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20125,         /* Account number   */~
                                    L20145,         /* Ref 1            */~
                                    L20165,         /* Ref 2            */~
                                    L20185,         /* Description      */~
                                    L20205,         /* Debit amount     */~
                                    L20230          /* Credit amount    */
                     return

L20125: REM Enable for account number
            str(infomsg$,1)= "Enter Account Code"
            enabled% = 1
            return
L20145: REM Enable for ref 1
            str(infomsg$,1,24)= "Enter Reference 1"
            ref1$(currentline%) = name$
            enabled% = 1
            return
L20165: REM Enable for ref 2
            str(infomsg$,1,24)= "Enter Reference 2"
            ref2$(currentline%) = name$
            enabled% = 1
            return
L20185: REM Enable for description
            str(infomsg$,1,24)= "Enter Description"
            descr$(currentline%) = description$
            enabled% = 1
            return
L20205: REM Enable for debit amount (enabled only for non-blank acct)
            if acct$(currentline%) = " " then return
            str(infomsg$,1,24)= "Enter Debit Amount"
            enabled% = 1
            return
L20230: REM enable for credit (enabled only for non-blank acct)
            if acct$(currentline%) = " " then return
            if debit <> 0 then gosub'151(fieldnr%)
            if debit <> 0 then L20260
                str(infomsg$,1,24)= "Enter Credit Amount"
                enabled% = 1
L20260:         return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 3 of input. *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L22110,         /* FIRSTENTRY$      */~
                                    L22160,         /* LASTENTRY$       */~
                                    L22190          /* PAGEBREAK$       */
                     return

L22110: REM Default/enable for firstentry$
            if file1% = 9 then edtmessage$ =                             ~
                "Enter Buffer Entries to Print else PF16 to Return"      ~
                          else edtmessage$ =                             ~
                "Enter Master Entries to Print else PF16 to Return"

            firstentry$, lastentry$, lastdsply$, errormsg$ = " "
            firstdsply$ = "ALL"
            prtall% = 0
            pagebreak$ = "NO"
            return

L22160: REM Default/enable for lastentry$
            if prtall% = 1 then enabled% = 0%
            return

L22190: REM Default/enable for pagebreak$
            return

        REM *************************************************************~
            *    I N P U T  E N A B L E  F O R  D E L E T E  E N T R Y  *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for delete entry screen  *~
            *************************************************************

            deffn'054(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L23120,    /* Name$                 */~
                                    L23150     /* Editmode              */
            return

L23120: REM Default/enable for name$
            if file1% = 9 then deletemsg$ = "Buffer Entry to Delete"     ~
                          else deletemsg$ = "Master Entry to Delete"
            edtmessage$ = "Enter Journal Name to DELETE or PF16 for " &  ~
                          "Input Screen"
            return

L23150: REM Default/enable for editmode
            edtmessage$ = "Press ENTER to DELETE or PF16 for Editmode"
            return

L29000: REM *************************************************************~
            *    R E N U M B E R   A N D   R E T O T A L   E N T R Y    *~
            *-----------------------------------------------------------*~
            * Renumbers and retotals the journal entry.                 *~
            *************************************************************

            total = 0
            for temp% = 1 to maxlines%
                if acct$(temp%) = " " then L29130
                   convert debit$(temp%) to debit
                   convert credit$(temp%) to credit
                total = total + debit - credit
L29130:         next temp%
            total = round(total, 2)
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
L29945:     keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29945
                return clear all
                goto inputmode

L30000: REM *************************************************************~
            *        L O A D   E N T R Y   F R O M   B U F F E R        *~
            *-----------------------------------------------------------*~
            * Searches the buffer for the old journal entry name and    *~
            * loads it for us.                                          *~
            *************************************************************

            oldentryonfile% = 0

        REM Search buffer and see if it's on file, return if not.
            call "REDALT0" (#9, name$, 2%, f1%(9))
                if f1%(9) = 0 then L30170
            get #9, using L30130, ruserid$, header$
L30130:         FMT CH(3), XX(11), CH(55)
            if ruserid$ = userid$ then L30140
                errormsg$ = "Journal Entry currently in process by " &   ~
                             "user " & ruserid$
                return
L30140:     file% = 10
            goto L30240

L30170: REM If not in buffer, then check main file.
            call "READ100" (#5, name$, f1%(5))
                if f1%(5) = 0 then return
            get #5, using L30210, header$
L30210:         FMT CH(55)
            file% = 6

L30240: REM Load and format header information.
            if file% = 10 then                                           ~
                call "SHOSTAT" ("Loading Journal Entry from Buffer")     ~
              else                                                       ~
                call "SHOSTAT" ("Loading Journal Entry from Master")

            get header$, using L30560, description$, saveflag$,           ~
                         reverseflag$, stopdate$, postedflag$
            if saveflag$    = "Y" then saveflag$    = "YES"              ~
                                  else saveflag$    = "NO "
            if reverseflag$ = "Y" then reverseflag$ = "YES"              ~
                                  else reverseflag$ = "NO "
            stpdate$=stopdate$ : call "DATEFMT" (stpdate$)
            oldentryonfile% = 1

        REM Plow routine to load data from line items.
            readkey$ = name$
            maxlines% = 0

L30380:     call "PLOWNEXT" (#file%, readkey$, 10%, f1%(file%))
            if f1%(file%) = 0 then return
            if maxlines% = 100 then lineitems_error
            maxlines% = maxlines% + 1
            get #file%, using L30490, acct$(maxlines%), ref1$(maxlines%), ~
                            ref2$(maxlines%), adj$, descr$(maxlines%),   ~
                            debit, credit
            if  acct$(maxlines%) = " " then L30380
                call "GLFMT" (acct$(maxlines%))
                call "CONVERT" (debit, 2.2, debit$ (maxlines%))
                call "CONVERT" (credit, 2.2, credit$(maxlines%))
                goto L30380

L30490:     FMT XX(3),                   /* These two fields are variab*/~
                XX(10),                  /* depending on which file    */~
                CH(9),                   /* Account Number             */~
                CH(30),                  /* Reference 1                */~
                CH(34),                  /* Reference 2                */~
                CH(04),                  /* Adj Flag (N/A)             */~
                CH(32),                  /* Description                */~
                PD(14,4),                /* Debit Amount               */~
                PD(14,4)                 /* Credit Amount              */

L30560:     FMT XX(10),                  /* NAME OF JOURNAL ENTRY      */~
                CH(36),                  /* DESRIPTION OF THIS ENTRY   */~
                CH(1),                   /* SAVE ON MAIN FILE FLAG     */~
                CH(1),                   /* SELF-REVERSING ENTRY FLAG  */~
                CH(6),                   /* DATE TO DELETE J.E.        */~
                CH(1)                    /* Posted flag                */

        lineitems_error
            errormsg$ = "Journal entry contains more than 100 line items"
            init (" ") name$, description$, acct$(), descr$(),           ~
                       debit$(), credit$(), infomsg$, ref1$(), ref2$()
            return clear
            goto L10100

L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *-----------------------------------------------------------*~
            * Write data to file.                                       *~
            *************************************************************

        REM Write header to buffer file.
            call "FMTKEY" (#9, key$, reversekey$)
            write #9, using L31260, key$, reversekey$, name$,             ~
                      description$, saveflag$, reverseflag$, stopdate$,  ~
                      "N", " "

        REM Write line items to file
            if maxlines% = 0 then return
            for temp% = 1 to maxlines%
                convert temp% to seqnr$, pic(###)
                debit, credit = 0
                if acct$(temp%) = " " then L31200
                call "GLUNFMT" (acct$(temp%))
                convert debit$(temp%) to debit
                convert credit$(temp%) to credit
L31200:         write #10, using L31330, name$, seqnr$, acct$(temp%),     ~
                           ref1$(temp%), ref2$(temp%), " ",              ~
                           descr$(temp%), debit, credit, " "
            next temp%
            return

L31260:     FMT CH(7),                   /* BUFFER KEY INFORMATION     */~
                CH(7),                   /* REVERSE KEY INFORMATION    */~
                CH(10),                  /* NAME OF ENTRY              */~
                CH(36),                  /* Description                */~
                CH(1),                   /* SAVE THIS ENTRY ON FILE ?  */~
                CH(1),                   /* AUTO-REVERSING ENTRY FLAG  */~
                CH(6),                   /* DATE TO DELETE THIS J.E.   */~
                CH(1),                   /* Posted flag                */~
                CH(31)                   /* FILLER                     */

L31330:     FMT CH(10),                  /* ENTRY NAME                 */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(9),                   /* ACCOUNT NUMBER             */~
                CH(30),                  /* REF 1                      */~
                CH(34),                  /* REF 2                      */~
                CH(04),                  /* Adjustment Field, N/A      */~
                CH(32),                  /* DESCRIPTION THIS LINE      */~
                PD(14,4),                /* DEBIT AMOUNT               */~
                PD(14,4),                /* CREDIT AMOUNT              */~
                CH(22)                   /* FILLER                     */

        REM *************************************************************~
            *        G E T   J O U R N A L   E N T R Y   N A M E        *~
            *-----------------------------------------------------------*~
            * Gets name of this journal entry name and a description of *~
            * it.  the name is necessary for recall.                    *~
            *************************************************************

            deffn'201(fieldnr%)
               init(hex(8c)) linefac$()
               if lastentry1$ <> " " then                                ~
                  str(line2$,,60) = "Last Entry: " & lastentry1$
                  on fieldnr% gosub L40190,     /* Entry name           */~
                                    L40160,     /* Description          */~
                                    L40190,     /* Save on buffer flag  */~
                                    L40190,     /* Auto reverse flag    */~
                                    L40190      /* Stop date            */
                  goto L40270

L40160:           REM Set FAC's for upper/lower case input
                      linefac$(fieldnr%) = hex(80)
                      return
L40190:           REM Set FAC's for upper case only input
                      linefac$(fieldnr%) = hex(81)
                      if fieldnr% <> 5 then return
                      if stpdate$ = " " or ~
                         stpdate$ = blankdate$ ~
                      then stpdate$ = stpdate2$
                      return
                  REM Set FAC's for numeric only input
                      linefac$(fieldnr%) = hex(82)
                      return

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Standard Journal Entries",                      ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),      date$             , ch(08),~
               at (02,02), fac(hex(ac)),      line2$            , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)),      errormsg$         , ch(79),~
               at (06,02),                                               ~
                  "Journal entry name",                                  ~
               at (06,30), fac(linefac$( 1)), name$             , ch(10),~
               at (07,02),                                               ~
                  "Title of entry",                                      ~
               at (07,30), fac(linefac$( 2)), description$      , ch(36),~
               at (08,02),                                               ~
                  "Save in main file         ",                          ~
               at (08,30), fac(linefac$( 3)), saveflag$         , ch(03),~
               at (09,02),                                               ~
                  "Auto-reverse at month end",                           ~
               at (09,30), fac(linefac$( 4)), reverseflag$      , ch(03),~
               at (10,02), "Date entry to be deleted",                   ~
               at (10,30), fac(linefac$( 5)), stpdate$          , ch(08),~
               at (21,02), fac(hex(a4)),      blankline$        , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20),                                               ~
                  "(9)Print Master",                                     ~
               at (24,19),                                               ~
                  "(10)Print Buffer",                                    ~
               at (23,40),                                               ~
                  "(11)Delete Master",                                   ~
               at (24,40),                                               ~
                  "(12)Delete Buffer",                                   ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(0001090a0b0c0d0f10)),                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40840
                  call "MANUAL" ("STJINPUT")
                  goto L40270

L40840:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *       E D I T   H E A D E R   I N F O R M A T I O N       *~
            *                                                           *~
            * EDITS HEADER INFORMATION FOR THE JOURNAL ENTRY.           *~
            *************************************************************

            deffn'202(fieldnr%)
                  init(hex(8c)) linefac$()
                  if lastentry1$ <> " " then                             ~
                      str(line2$,,60) = "Last Entry: " & lastentry1$
                  on fieldnr% gosub L41170,         /* ENTRY NAME       */~
                                    L41140,         /* DESCRIPTION OF   */~
                                    L41170,         /* SAVE FLAG        */~
                                    L41170,         /* REVERSE FLAG     */~
                                    L41170          /* STOP DATE        */
                  goto L41240

L41140:           REM Set FAC's for upper/lower case input
                      linefac$(fieldnr%) = hex(80)
                      return
L41170:           REM Set FAC's for upper case only input
                      linefac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for numeric only input
                      linefac$(fieldnr%) = hex(82)
                      return

L41240:     accept                                                       ~
               at (01,02),                                               ~
                  "Edit Standard Journal Entries",                       ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),      date$             , ch(08),~
               at (02,02), fac(hex(ac)),      line2$            , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Journal entry name",                                  ~
               at (06,30), fac(linefac$( 1)), name$             , ch(10),~
               at (07,02),                                               ~
                  "Title of entry",                                      ~
               at (07,30), fac(linefac$( 2)), description$      , ch(36),~
               at (08,02),                                               ~
                  "Save in main file         ",                          ~
               at (08,30), fac(linefac$( 3)), saveflag$         , ch(03),~
               at (09,02),                                               ~
                  "Auto-reverse at month end",                           ~
               at (09,30), fac(linefac$( 4)), reverseflag$      , ch(03),~
               at (10,02), "Date entry to be deleted",                   ~
               at (10,30), fac(linefac$( 5)), stpdate$          , ch(08),~
               at (21,02), fac(hex(a4)),      edtmessage$       , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,02),                                               ~
                  "(2)Line Items",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Save Data",                                       ~
                                                                         ~
               keys(hex(0001020d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L41610
                  call "MANUAL" ("STJINPUT")
                  goto L41240

L41610:        if keyhit% <> 15 then L41650
                  call "PRNTSCRN"
                  return

L41650:        REM GET CURSOR LOCATION FOR EDIT FIGURE OUT.
                   close ws
                   call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                   return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   3      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'103(fieldnr%)
                  str(line2$,,60) = " "
                  init(hex(84)) nfac$()
                  on fieldnr% gosub L42170,         /* FIRSTENTRY%      */~
                                    L42170,         /* LASTENTRY%       */~
                                    L42170          /* PAGEBREAK%       */
                     goto L42240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      nfac$(fieldnr%) = hex(80)
                      return
L42170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      nfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      nfac$(fieldnr%) = hex(82)
                      return

L42240:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Standard Journal Entries",                      ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),      date$             , ch(08),~
               at (02,02), fac(hex(ac)),      line2$            , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "First Journal Entry Name",                            ~
               at (06,30), fac(nfac$( 1)), firstdsply$          , ch(10),~
               at (07,02),                                               ~
                  "Ending Journal Entry Name",                           ~
               at (07,30), fac(nfac$( 2)), lastdsply$           , ch(10),~
               at (08,02),                                               ~
                  "Page Break on Journal?",                              ~
               at (08,30), fac(nfac$( 3)), pagebreak$           , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Return      ",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L42600
                  call "MANUAL" ("STJINPUT")
                  goto L42240

L42600:        if keyhit% <> 15 then L42640
                  call "PRNTSCRN"
                  goto L42240

L42640:        if keyhit% <> 16 then return
                  return clear
                  goto inputmode

        REM *************************************************************~
            *           D E L E T E  E N T R Y  S C R E E N             *~
            *                                                           *~
            * HANDLES INPUT OF BUFFER ENTRY TO DELETE FOR INPUT MODE AND*~
            * EDIT MODE                                                 *~
            *************************************************************

            deffn'204(fieldnr%)
                  str(line2$,,60) = " "
                  init(hex(84)) nfac$()
                  on fieldnr% gosub L43170,         /* NAME$            */~
                                    L43170          /* EDITMODE$        */
                     goto L43240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      nfac$(fieldnr%) = hex(80)
                      return
L43170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      nfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      nfac$(fieldnr%) = hex(82)
                      return

L43240:     accept                                                       ~
               at (01,02),                                               ~
                  "DELETE Standard Journal Entry",                       ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), fac(hex(84)), deletemsg$             , ch(22),~
               at (06,30), fac(nfac$( 1)), name$                , ch(10),~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Return",                                          ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L43530
                  call "MANUAL" ("STJINPUT")
                  goto L43240

L43530:        if keyhit% <> 15 then L43570
                  call "PRNTSCRN"
                  goto L43240

L43570:        return

        REM *************************************************************~
            *           T A B U L A R   M O D E   S C R E E N           *~
            *                                                           *~
            * HANDLES THE INPUT OF FIELDS IN TABLE, IN EITHER INPUT,    *~
            * EDIT, INSERT, OR DELETE STYLE.                            *~
            *************************************************************

            deffn'203(fieldnr%)                    /* INPUT MODE       */
                  screen% = 1
                  goto L44130

            deffn'213(fieldnr%)                    /* EDIT MODE        */
                  screen% = 2
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L44135
                  goto L44130

            deffn'223(fieldnr%)                    /* INSERT MODE      */
                  screen% = 3
                  goto L44130

            deffn'233(screenline%)                 /* DELETE MODE      */
                  screen% = 4
                  init(hex(84)) fac$()
                  for temp% = 1 to 6
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L44215

L44130:           init(hex(84)) fac$()
L44135:           on fieldnr% gosub L44180,         /* ACCOUNT NUMBER   */~
                                    L44165,         /* REF 1            */~
                                    L44165,         /* REF 2            */~
                                    L44165,         /* ACCT DESCRIPTION */~
                                    L44195,         /* DEBIT AMOUNT     */~
                                    L44195          /* CREDIT AMOUNT    */
                  goto L44215

L44165:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
L44180:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L44195:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L44215:     accept                                                       ~
               at (01,02), fac(hex(8c)),     title$(screen%,1)  , ch(60),~
               at (02,02), fac(hex(8c)),     title$(screen%,2)  , ch(60),~
               at (03,02), fac(hex(94)),     errormsg$          , ch(60),~
               at (04,02), fac(hex(a4)),     infomsg$           , ch(79),~
                                                                         ~
               at (01,64), "! TOTAL:      "                             ,~
               at (02,64), "!"                                          ,~
               at (03,64), "+----------------"                          ,~
               at (02,66), fac(hex(84)), total,     pic(-###########.##),~
                                                                         ~
               at (05,02), fac(fac$( 1,1)), acct$  (line%+ 1)   , ch(12),~
               at (07,02), fac(fac$( 2,1)), acct$  (line%+ 2)   , ch(12),~
               at (09,02), fac(fac$( 3,1)), acct$  (line%+ 3)   , ch(12),~
               at (11,02), fac(fac$( 4,1)), acct$  (line%+ 4)   , ch(12),~
               at (13,02), fac(fac$( 5,1)), acct$  (line%+ 5)   , ch(12),~
               at (15,02), fac(fac$( 6,1)), acct$  (line%+ 6)   , ch(12),~
               at (17,02), fac(fac$( 7,1)), acct$  (line%+ 7)   , ch(12),~
               at (19,02), fac(fac$( 8,1)), acct$  (line%+ 8)   , ch(12),~
               at (21,02), fac(fac$( 9,1)), acct$  (line%+ 9)   , ch(12),~
               at (23,02), fac(fac$(10,1)), acct$  (line%+10)   , ch(12),~
                                                                         ~
               at (05,15), fac(fac$( 1,2)), ref1$  (line%+ 1)   , ch(30),~
               at (07,15), fac(fac$( 2,2)), ref1$  (line%+ 2)   , ch(30),~
               at (09,15), fac(fac$( 3,2)), ref1$  (line%+ 3)   , ch(30),~
               at (11,15), fac(fac$( 4,2)), ref1$  (line%+ 4)   , ch(30),~
               at (13,15), fac(fac$( 5,2)), ref1$  (line%+ 5)   , ch(30),~
               at (15,15), fac(fac$( 6,2)), ref1$  (line%+ 6)   , ch(30),~
               at (17,15), fac(fac$( 7,2)), ref1$  (line%+ 7)   , ch(30),~
               at (19,15), fac(fac$( 8,2)), ref1$  (line%+ 8)   , ch(30),~
               at (21,15), fac(fac$( 9,2)), ref1$  (line%+ 9)   , ch(30),~
               at (23,15), fac(fac$(10,2)), ref1$  (line%+10)   , ch(30),~
                                                                         ~
               at (05,47), fac(fac$( 1,3)), ref2$  (line%+ 1)   , ch(34),~
               at (07,47), fac(fac$( 2,3)), ref2$  (line%+ 2)   , ch(34),~
               at (09,47), fac(fac$( 3,3)), ref2$  (line%+ 3)   , ch(34),~
               at (11,47), fac(fac$( 4,3)), ref2$  (line%+ 4)   , ch(34),~
               at (13,47), fac(fac$( 5,3)), ref2$  (line%+ 5)   , ch(34),~
               at (15,47), fac(fac$( 6,3)), ref2$  (line%+ 6)   , ch(34),~
               at (17,47), fac(fac$( 7,3)), ref2$  (line%+ 7)   , ch(34),~
               at (19,47), fac(fac$( 8,3)), ref2$  (line%+ 8)   , ch(34),~
               at (21,47), fac(fac$( 9,3)), ref2$  (line%+ 9)   , ch(34),~
               at (23,47), fac(fac$(10,3)), ref2$  (line%+10)   , ch(34),~
                                                                         ~
               at (06,14), fac(fac$( 1,4)), descr$ (line%+ 1)   , ch(32),~
               at (08,14), fac(fac$( 2,4)), descr$ (line%+ 2)   , ch(32),~
               at (10,14), fac(fac$( 3,4)), descr$ (line%+ 3)   , ch(32),~
               at (12,14), fac(fac$( 4,4)), descr$ (line%+ 4)   , ch(32),~
               at (14,14), fac(fac$( 5,4)), descr$ (line%+ 5)   , ch(32),~
               at (16,14), fac(fac$( 6,4)), descr$ (line%+ 6)   , ch(32),~
               at (18,14), fac(fac$( 7,4)), descr$ (line%+ 7)   , ch(32),~
               at (20,14), fac(fac$( 8,4)), descr$ (line%+ 8)   , ch(32),~
               at (22,14), fac(fac$( 9,4)), descr$ (line%+ 9)   , ch(32),~
               at (24,14), fac(fac$(10,4)), descr$ (line%+10)   , ch(32),~
                                                                         ~
               at (06,53), fac(fac$( 1,5)), debit$ (line%+ 1)   , ch(12),~
               at (08,53), fac(fac$( 2,5)), debit$ (line%+ 2)   , ch(12),~
               at (10,53), fac(fac$( 3,5)), debit$ (line%+ 3)   , ch(12),~
               at (12,53), fac(fac$( 4,5)), debit$ (line%+ 4)   , ch(12),~
               at (14,53), fac(fac$( 5,5)), debit$ (line%+ 5)   , ch(12),~
               at (16,53), fac(fac$( 6,5)), debit$ (line%+ 6)   , ch(12),~
               at (18,53), fac(fac$( 7,5)), debit$ (line%+ 7)   , ch(12),~
               at (20,53), fac(fac$( 8,5)), debit$ (line%+ 8)   , ch(12),~
               at (22,53), fac(fac$( 9,5)), debit$ (line%+ 9)   , ch(12),~
               at (24,53), fac(fac$(10,5)), debit$ (line%+10)   , ch(12),~
                                                                         ~
               at (06,69), fac(fac$( 1,6)), credit$(line%+ 1)   , ch(12),~
               at (08,69), fac(fac$( 2,6)), credit$(line%+ 2)   , ch(12),~
               at (10,69), fac(fac$( 3,6)), credit$(line%+ 3)   , ch(12),~
               at (12,69), fac(fac$( 4,6)), credit$(line%+ 4)   , ch(12),~
               at (14,69), fac(fac$( 5,6)), credit$(line%+ 5)   , ch(12),~
               at (16,69), fac(fac$( 6,6)), credit$(line%+ 6)   , ch(12),~
               at (18,69), fac(fac$( 7,6)), credit$(line%+ 7)   , ch(12),~
               at (20,69), fac(fac$( 8,6)), credit$(line%+ 8)   , ch(12),~
               at (22,69), fac(fac$( 9,6)), credit$(line%+ 9)   , ch(12),~
               at (24,69), fac(fac$(10,6)), credit$(line%+10)   , ch(12),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 13 then L44725
                  call "MANUAL" ("STJINPUT")
                  goto L41240

L44725:        if keyhit% <> 15 then L44745
                  call "PRNTSCRN"
                  goto L44215

L44745:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L46000: REM *************************************************************~
            *       E R R O R   S C R E E N   F O R   T O T A L S       *~
            *                                                           *~
            * This screen does the error message in case the total of   *~
            * the journal entry does not balance (in which case it'd    *~
            * throw the general ledger out of balance)                  *~
            *************************************************************

            temp$ = "E R R O R ! !"
               if lastentry1$ <> " " then                                ~
                  str(line2$,,60) = "Last Entry: " & lastentry1$
            accept                                                       ~
               at (01,02), "Create Standard Journal Entries",            ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),      date$             , ch(08),~
               at (02,02), fac(hex(ac)),      line2$            , ch(79),~
                                                                         ~
               at (08,21), "*****************************************",  ~
               at (09,21), "*                                       *",  ~
               at (10,21), "*                                       *",  ~
               at (11,21), "* The total amount of the journal entry *",  ~
               at (12,21), "* is not zero.  press RETURN to return  *",  ~
               at (13,21), "* to EDIT mode and make corrections.    *",  ~
               at (14,21), "*****************************************",  ~
               at (09,35), fac(hex(94)), temp$                  , ch(13),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,02),                                               ~
                  "(RETURN)Return to edit mode",                         ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
                                                                         ~
               keys(hex(000f)),                                          ~
               key (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *              T E S T   H E A D E R   D A T A              *~
            *                                                           *~
            * HEADER DATA TEST, INCLUDING WHETHER OR NOT THE JOURNAL    *~
            * ENTRY NAME IS OUT IN THE BUFFER OR NOT.                   *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L50160,         /* J.E. NAME        */~
                                    L50270,         /* TITLE            */~
                                    L50290,         /* SAVE ON FILE FLAG*/~
                                    L50390,         /* AUTO-REVERSE FLAG*/~
                                    L50460          /* STOP DATE        */
                  return

L50160: REM Test to see that the journal entry is not on file.
            if name$ <> " " then L50190
            plowdescr$ = hex(06) & "Select Journal Entry from MASTER file"
            call "GETCODE" (#5, name$, plowdescr$, 0%, 0.32, f1%(5))
                if f1%(5) <> 0 then L50190
            plowdescr$ = hex(06) & "Select Journal Entry from BUFFER file"
            call "PLOWCODE" (#9, name$, plowdescr$, 3000%, 2.36, f1%(9), ~
                             hdr$(), 0, 25)
                if f1%(9) <> 0 then L50190
                    errormsg$ = "Journal ID CANNOT be blank"
                    return
L50190:     gosub L30000
            if postedflag$ <> "Y" or str(reverseflag$,,1) <> "Y" then    ~
                                                                    L50220
                errormsg$ = "This Reversing Journal has been posted, " & ~
                            "but not reversed. Can't post again."
                return
L50220:     if oldentryonfile% = 0 then return
                errormsg$ = " "
                return clear
                return clear
                goto editmode

L50270: REM Test description
            return

L50290: REM Test for valid save on file flag.
            if saveflag$ = " " then saveflag$ = "YES"
            if pos(saveflag$="Y") <> 0 then saveflag$ = "YES"            ~
                                       else saveflag$ = "NO "
            if saveflag$ = "NO " and reverseflag$ = "YES" then L50350
            return
L50350:     errormsg$ = "No Auto-Reverse Can Be Performed If The Standard~
        ~ Entry Isn't Saved"
            return

L50390: REM Test for valid auto-reverse at end of month flag.
            if reverseflag$ = " " then reverseflag$ = "YES"
            if pos(reverseflag$="Y") <> 0 then reverseflag$ = "YES"      ~
                                          else reverseflag$ = "NO "
            if pos(reverseflag$="Y") <> 0 then saveflag$ = "YES"
            return

L50460: REM Test to see that stop date is valid if present
            stopdate$ = stpdate$
            if stpdate$ = " " or     ~
	       stpdate$ = blankdate$ ~
	       then return
            call "DATEOKC" (stpdate$, date%, errormsg$)
            if errormsg$ > " " then return
            call "DATUFMTC" (stpdate$)
            stopdate$ = stpdate$
            call "DATEFMT" (stpdate$)
            return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M S      *~
            *                                                           *~
            * TESTS DATA FOR THE ACCOUNT ON FILE, AND THAT SORT OF THING*~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  c% = currentline%
                  on fieldnr% gosub L51150,         /* ACCOUNT #        */~
                                    L51210,         /* REFERENCE 1      */~
                                    L51210,         /* REFERENCE 2      */~
                                    L51210,         /* DESCRIPTION      */~
                                    L51230,         /* DEBIT AMOUNT     */~
                                    L51310          /* CREDIT AMOUNT    */
                  return

L51150: REM Test for account on file. return if description only line
            call "GETCODE"(#2, acct$(c%), str(infomsg$,25),1%,0,f1%(2))
            if f1%(2) = 0 then errormsg$ = hex(00)
            return

L51210: REM Test data for description
            return

L51230: REM Test debit account for valid number.
            if debit$(c%) = " " then debit$(c%) = "0"
            call "NUMTEST" (debit$(c%), 0, 9e8, errormsg$, -2.2, debit)
            return

L51310: REM Check out the credit account
            if credit$(c%) = " " then credit$(c%) = "0"
            call "NUMTEST" (credit$(c%), 0, 9e8, errormsg$, -2.2, credit)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 3.                       *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52110,         /* first entry      */~
                                    L52260,         /* last entry       */~
                                    L52380          /* page break       */
                     return

L52110: REM Test data for FIRSTENTRY$
            if firstdsply$ = "ALL" then L52220
                goto L52242
L52220:     firstentry$ = " " : lastdsply$ = "ALL"
            init(hex(ff)) lastentry$
            prtall% = 1
            return
L52242:     firstentry$ = firstdsply$
            return

L52260: REM Test data for LASTENTRY$
            if lastentry$ = " " then lastentry$ = firstentry$
            if lastentry$ = " " then lastdsply$ = firstdsply$
            if lastentry$ >= firstentry$ then L52360
            errormsg$ = "Journal Entry Name must be equal to or greater t~
        ~han First Journal Entry Name"
L52360:     return

L52380: REM Test data for PAGEBREAK$
            if pagebreak$ = "NO" or pagebreak$ = "YES" then L52410
                errormsg$ = "Enter YES or NO for Print New Page"
L52410:         return

        REM *************************************************************~
            *         T E S T  D A T A  F O R  D E L E T E  E N T R Y   *~
            *                                                           *~
            * TESTS ENTRY TO SEE IF IN BUFFER FILE                      *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53120,         /* NAME$            */~
                                    L53210          /* EDITMODE         */
            return

L53120: REM Test for name$ in buffer file
            if name$ = " " then L53175
            call "REDALT0" (#file1%, name$, ky%, f1%(file1%))
                  if f1%(file1%) = 0 then L53175
            if file1% = 5 then L53190
            get #file1% using L53160, ruserid$
L53160:         FMT CH(3)
            if ruserid$ = userid$ then L53190
L53175:     if file1% = 9 then                                           ~
                         errormsg$ = "Journal Entry not in Buffer file"  ~
                else                                                     ~
                         errormsg$ = "Journal Entry not in Master file"
L53190:     return

L53210: REM Test for editmode
            return

        print_master
        REM *************************************************************~
            *           P R I N T  L I S T  R O U T I N E               *~
            *                                                           *~
            * PRINTS JOURNAL ENTRY LIST FROM EITHER THE BUFFER OR MASTER*~
            *************************************************************

            flag1% = 0
            file1% = 5
            file2% = 6
            ky%    = 0
            jnltitle$ = "F R O M  M A S T E R  F I L E"
            goto print_list

        print_buffer
            flag1% = 1
            file1% = 9
            file2% = 10
            ky%    = 2
            jnltitle$ = "F R O M  B U F F E R  F I L E"

        print_list
            for fieldnr% = 1 to  3
                gosub'053(fieldnr%)
                    if enabled% = 0 then L55320
L55270:         gosub'103(fieldnr%)
                    if keyhit% = 1 then gosub startover
                    if keyhit% <> 0 then L55270
                gosub'153(fieldnr%)
                    if errormsg$ <> " " then L55270
L55320:     next fieldnr%

            printline% = 60
            pgflg% = 0%
            print% = 1%
            pageno%= 0%
            call "DATE" addr("HD", hdrdate$)
            select printer (134)
            if flag1% = 0 then prtdate$ = " " else prtdate$=gldate$
            call "DATEFMT" (prtdate$)
            readkey$= " "
            if prtall% = 1 then range$ = "ALL STANDARD JOURNAL ENTRIES"  ~
               else range$ = "From " & firstentry$ & " to " & lastentry$
            call "PLOWALTS" (#file1%,readkey$, ky%,  0%, f1%(file1%))
            goto L55500
L55490:     call "READNEXT" (#file1%, f1%(file1%))
L55500:         if f1%(file1%) = 0 then close printer
                if f1%(file1%) = 0 then inputmode
            readkey$=str(key(#file1%,ky%),1,10) & " "
            if readkey$ < firstentry$ then L55490
            if readkey$ > lastentry$ then close printer
            if readkey$ > lastentry$ then inputmode
            totaldebits, totalcredits = 0
            if flag1% = 0 then get #file1%, using L30210, header$         ~
                          else get #file1%, using L55580, ruserid$,header$
L55580:         FMT CH(3), XX(11), CH(54)
            if flag1% = 1 and ruserid$ <> userid$ then L55490
            get header$, using L30560, description$, saveflag$,           ~
                                      reverseflag$, stopdate$
            if saveflag$    = "Y" then saveflag$    = "Yes"              ~
                                  else saveflag$    = "No "
            if reverseflag$ = "Y" then reverseflag$ = "Yes"              ~
                                  else reverseflag$ = "No "
            stpdate$ = stopdate$  :  call "DATEFMT" (stpdate$)
            if printline% > 54% then gosub heading

            if pagebreak$ = "YES" and pgflg% = 1 then gosub heading
            print using L60080
            print using L60120, readkey$, description$, saveflag$,        ~
                               reverseflag$, stpdate$
            print using L60080
            printline% = printline% + 3

            if printline% > 53% then gosub heading
            gosub sub_heading

L55870:     call "PLOWNEXT" (#file2%, readkey$, 10%, f1%(file2%))
                if f1%(file2%) = 0 then L56020
            get #file2%, using L30490, acct$(print%), ref1$(print%),      ~
                                      ref2$(print%), adj$,               ~
                                      descr$(print%), debit, credit

            if  acct$(print%) = " " then L55870
            call "GLFMT" (acct$(print%))
            prtctl% = 1
            if printline% > 56% then gosub heading
            convert debit  to debit1$,  pic(#########.##)
            convert credit to credit1$, pic(#########.##)
            if debit = 0 then debit1$ = " "
            if credit = 0 then credit1$ = " "
            print using L60160, acct$(print%), prtdate$, ref1$(print%),   ~
                        ref2$(print%), descr$(print%), debit1$, credit1$
            printline% = printline% +1%
            totaldebits = totaldebits + debit
            totalcredits = totalcredits + credit
            goto L55870

            prtctl% = 2
L56020:     if printline% > 54% then gosub heading
            print using L60200
            if totaldebits < 1e9 and totalcredits < 1e9 then L56030
                print using L60270, totaldebits, totalcredits
                goto L56035
L56030:     print using L60240, totaldebits, totalcredits
L56035:     print using L60040
            printline% = printline% + 3
            goto L55490

        sub_heading
            print using L60280
            print using L60320
            print using L60360
            print using L60400
            printline% = printline% + 4
            pgflg% = 1
            return

        heading
            print page
            pageno% = pageno% + 1
            print using L60440, pageno%, hdrdate$
            print using L60480, jnltitle$
            print using L60510, range$
            print using L60040
            if prtctl% = 0 then printline% = 7%
            if prtctl% = 1 then printline% = 4%
            if prtctl% = 2 then printline% = 6%
            prtctl% = 0
            return

        REM *************************************************************~
            *  I M A G E   S T A T E M E N T S                          *~
            *************************************************************

L60040: %================================================================~
        ~=================================================================~
        ~===

L60080: %!                                                               ~
        ~                                                                 ~
        ~  !

L60120: %!  Journal Name: ##########  Description: ######################~
        ~##############  Save: ###  Reverse: ###  Delete Date: ########   ~
        ~  !

L60160: %!############# ######## ######################### ##############~
        ~########### #############################!############!##########~
        ~##!

L60200: %!                                                               ~
        ~                                         !------------!----------~
        ~--!

L60240: %!                                                               ~
        ~                                         !#########.##!#########.~
        ~##!
L60270: %!                       Total Debits ###########.##             ~
        ~Total Credits ###########.##             !    <---    !    <---  ~
        ~  !
L60280: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+

L60320: %!Account Nmbr !  Date  !     Reference 1         !        Refere~
        ~nce 2      !         Description         !    Debit   !   Credit ~
        ~  !

L60360: %+---------------------------------------------------------------~
        ~-----------------------------------------+------------+----------~
        ~--+

L60400: %!                                                               ~
        ~                                         !            !          ~
        ~  !

L60440: %Page ########               S T A N D A R D  J O U R N A L  E N ~
        ~T R Y  L I S T I N G   ##########################################~
        ~###

L60480: %Report: STJINPUT                      ##########################~
        ~####

L60510: %Range: ##############################


L65000: REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Closes all open files, displays message, and sees if any  *~
            * documents are in the buffer for this user, generating an  *~
            * appropraite return code if there are.                     *~
            *************************************************************

        REM Now set ENDFLAG% to see if anything for this user in buffer.
            readkey$ = userid$
            call "PLOWNEXT" (#9, readkey$, 3%, f1%(9))
                if f1%(9) = 0% then L65180
L65120:     f1%(9) = 0%
            call "ASKUSER" (f1%(9), "*** G/L POST OPTION ***",           ~
                            "Press PF1 to POST to G/L    ", "- or -",    ~
                            "      PF2 to NOT POST to G/L")
            if f1%(9) <> 1% and f1%(9) <> 2% then L65120

L65180:     call "SHOSTAT" ("Closing files, One Moment Please")

            end f1%(9)
