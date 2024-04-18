        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      PPPP    OOO    SSS   TTTTT  3333           *~
            *  G      L      P   P  O   O  S        T        3          *~
            *  G GGG  L      PPPP   O   O   SSS     T     333           *~
            *  G   G  L      P      O   O      S    T        3          *~
            *   GGG   LLLLL  P       OOO    SSS     T    3333           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLPOST3  - Posts debits and Credits to the General Ledger *~
            *            Master file.  In addition keeps a detailed     *~
            *            history of the transactions passed through this*~
            *            routine by writing those transactions to the   *~
            *            GLDETAIL file.  (Note;  Type or Module 99 =    *~
            *            Closing Year End transaction).                 *~
            *     **** THIS SUBROUTINE USED TO BE 'GLPOST2' ****        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 06/11/80 ! DEBUG, INTEGRATE INTO SYSTEM.            ! BCW *~
            * 06/03/83 ! DON'T POST ZERO DETAILS                  ! KAB *~
            * 11/09/83 ! REVISED TO CONFORM TO NEW GLMAIN LAYOUT  ! HES *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION   ! RAC *~
            * 07/09/85 ! FINAL CLEANUP & GL CODE FMT CONTROL      ! KEN *~
            *          ! PROGRAM STANDARD IS INTERNAL GL CODE     ! KEN *~
            * 10/15/85 ! BYPASS RELOAD FROM SYSFILE2 AND CALL TO  ! HES *~
            *          ! EXTRACT ON 2nd TIME IN, SKIP PERIOD CALC !     *~
            *          ! LOGIC WHEN POSSIBLE. SHOULD SPEED UP SUB !     *~
            * 10/30/85 ! ADDED USERID TO CALL SYNTAX (2 layer sub)! HES *~
            * 08/20/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 10/04/88 ! Fixed Rounding Inconsistancies.          ! JDH *~
            * 04/18/89 ! Added returncode if suspence acct used   ! RJM *~
            *          !   this returncode is not an error        !     *~
            *          !   It is informational, used if dual books!     *~
            * 01/21/91 ! Remm's out PRINT BELL for usage in SSL   ! MJB *~
            * 05/30/91 ! Reinstated Responding to Obsolete Flag   ! JDH *~
            *          !   of 'X' from GLMAIN which was lost at R6!     *~
            * 03/31/93 ! PRR 12810&11. Corrected error messages.  ! JDH *~
	    * 05/28/96 ! Changes for the year 2000                ! DXL *~
            * 08/09/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "GLPOST3"(saveaccount$,      /* Account to be Updated      */~
                      debit,             /* Debit Amount (0 if credit) */~
                      credit,            /* Credit Amount (0 if debit) */~
                      date$,             /* Date of Module Posting Date*/~
                      anyperiod%,        /* 0 means restricted to 3open*/~
                      modl$,             /* Source Module ID           */~
                      savetext$,         /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                      jnlid$,            /* Journal ID                 */~
                      pstseq%,           /* Posting Sequence Number    */~
                      userid$,           /* Guess                      */~
/*(AWD001)*/          division$,         /* Division Code              */~
                      #1,                /* UFB Address of GLMAIN* file*/~
                      #2,                /* UFB Address of GLDETAIL* fl*/~
                      #3,                /* UFB Address of SYSFILE2    */~
/*(AWD001)*/          #4,                /* UFB Address of GLORTRAN    */~
                      returncode%)       /* Error Code Returned        */~

        REM * Note that file channels #1 & #2 may represent GLMAIN &     ~
               GLDETAIL or GLMAIN2 & GLDETAL2 in an installation having  ~
               local authority books requirements (in which case this    ~
               subroutine is called twice for each transaction). Refer to~
               GLPOST2 for the detailed calling logic.

            dim account$9,               /* Account Number             */~
                adjaccount$9,            /* Adjustment Account Number  */~
                acctfmt$16,              /* Formatted Account Number   */~
                blankdate$8,             /* Blank unfmt date           */~
                clsdate$(2)8,            /* Detail Date for Closing Ent*/~
                credit$14,               /* For error display          */~
                date$6,                  /* Date of Module when posting*/~
                debit$14,                /* For error display          */~
                err$(2)60,               /* Error message array        */~
                mtext$(3)79,             /* Error message array        */~
                filler$112,              /* Record Area Within GLMAIN  */~
		filler2$2,               /* Char filler for date fields*/~
                jnlid$3,                 /* Journal ID                 */~
                monthopen$3,             /* For error display          */~
                saveaccount$9,           /* Saves Passed Account Number*/~
                savedate$6,              /* Date for Detail Key        */~
                savetext$100,            /* Saves Passed Text          */~
                savejnlid$3,             /* Journal Id                 */~
                modl$2,                  /* Module ID                  */~
                text$100,                /* See Calling Argument Descr */~
                ttype$1,                 /* Task type (fore/background)*/~
                userid$3                 /* User generating transaction*/

            dim f1%(3)                   /* Record Status from READs   */

            dim u$(17)8,                 /* Fiscal Dates from SYSFILE2 */~
                bal(17),                 /* Balances of 3 Open Months  */~
                descr$30,                /* Account Description        */~
                accttype$1               /* Account Type A,L,C,R,E     */

            dim division$3               /* division number (AWD001)   */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            if date$ = " " or date$ = blankdate$ then date$ = date
            text$ = savetext$
            account$ = saveaccount$

*             If passed in as a -999, Dual Books error, No Posting!

            if returncode% = -999% then gosub get_fiscal_dates
L09920:     if returncode% = -999% then gosub'99(6%)

            returncode% = 99%            /* ALWAYS ASSUME THE WORST... */

            if modl$ = "99" and anyperiod% = 0% then gosub'99(1%)
            /* Skip Setup Stuff */
            if savedate$ <> " " and savedate$ <> blankdate$ then L12700

        REM *************************************************************~
            * MAKE SURE FILES ARE OPEN...                               *~
            *                                                           *~
            *************************************************************

            if open1% = 0 then call "OPENCHCK" (#1, open1%, 0%, 0%, " ")
                if open1% < 1 then gosub'99(5%)
            if open2% = 0 then call "OPENCHCK" (#2, open2%, 0%, 300%," ")
            if open3% = 0 then call "OPENCHCK" (#3, open3%, 0%, 0%, " ")
/*(AWD001) */
            if open4% = 0 then call "OPENCHCK" (#4, open4%, 0%, 300%, " ")

            call "READ100" (#3, "FISCAL DATES", f1%(3))
                if f1%(3) = 0 then gosub'99(2%)

            get #3, using L12300, p%, u$(), monthopen%, clsdate$(1),  ~
            clsdate$(2), adjaccount$
L12300:     FMT XX(20), BI(2), 17*CH(8), BI(2), XX(96), CH(8),        ~
            CH(8), XX(144), CH(16)
            c% = p% - 11

L12700:     REM Reverse Negative Posting...
                gldebit  = debit
                glcredit = credit
                if gldebit >= 0  then L13300
                   glcredit = -(gldebit) + glcredit   /* +CR = -DB   */
                   gldebit  = 0
L13300:         if glcredit >= 0 then L13700
                   gldebit  = -(glcredit) + gldebit   /* +DB = -CR   */
                   glcredit = 0

L13700:     REM NOW POST AMOUNTS TO G/L MAIN FILE, GETTING DETAIL SEQ #  ~
                IF ACCOUNT DOES NOT EXIST, POST TO ADJACCOUNT$, W/NOTE   ~
                (INVISIBLE TO CALLING PROGRAM)

                if abs(gldebit) < .0001 and abs(glcredit) < .0001        ~
                                                     then exit_stage_left
L14200:         call "READ101" (#1, account$, f1%(1))
                      if f1%(1) = 1 then L15100
L14400:                  if account$ = adjaccount$ then gosub'99(3%)
                            acctfmt$ = account$
                            call "GLFMT" (acctfmt$)
                            str(text$,69,13) =  str(acctfmt$,,12) & ":"
                            account$ = adjaccount$
                               /* Suspence Acct used, inform caller */
                            returncode% = 1000%
                            goto L14200

L15100:         get #1, using L20400, account$, descr$, accttype$, ob%,   ~
                                     seqnr%, filler$, closing, bal()

                if seqnr% > 5000000 then seqnr% = 0%
                if ob% = 2% then L14400  /* Flagged to Force to Suspence */

                REM Figure Out Which Period To Post To...
                if date$ = savedate$ and jnlid$ = savejnlid$ then L17700
                savedate$ = date$ : savejnlid$ = jnlid$
                if modl$ = "99" then L17700
                temp% = 17
                for u3% = 1 to 17
                  if u$(u3%) = " " or u$(u3%) = blankdate$ then L16300
                     if date$ < u$(u3%) then temp% = u3% - 1
                     if date$ < u$(u3%) then L16400
L16300:         next u3%
L16400:         if temp% = 0 then gosub'99(4%)   /* Not in F.Y. */

                if temp% = 13 and p% = 12 then temp% = 12
                if anyperiod% <> 0 then L17700    /* Post Any Period */

                REM Open Periods Only...
                if monthopen% = temp%     then L17700 /*  REMEMBER THE  */
                if monthopen% = temp% + 1 then L17700 /* 13TH BUCKET IS */
                if monthopen% = temp% - 1 then L17700 /* EMPTY IF P%=12 */
                if monthopen% = 12 and p% = 12 and temp% = 14 then L17700
                if monthopen% = 14 and p% = 12 and temp% = 12 then L17700
                    gosub'99(4%)

L17700:         REM Got it, now update GLMAIN...
                gldebit  = round(gldebit,  2)
                glcredit = round(glcredit, 2)
                if modl$ = "99" then closing =                           ~
                             round(closing + gldebit - glcredit, 2) else ~
                bal(temp%) = round(bal(temp%) + gldebit - glcredit, 2)

                rewrite #1, using L20400, account$, descr$, accttype$,    ~
                                 ob%, seqnr% + 1, filler$, closing, bal()

            REM SPECIAL PROCESSING FOR CLOSING DETAIL RECORDS
                if modl$ <> "99" then L19200
                seqnr% = seqnr% + 97
                date$ = clsdate$(c%)

L19200:     REM NOW WRITE G/L DETAIL.  THIS CAN BE UNHOOKED, IF YOU WISH.
                write #2, using L21200,                                   ~
                          account$, date$, seqnr% + 1%, modl$,           ~
                          gldebit, glcredit, text$, jnlid$, pstseq%,     ~
                          userid$, date
/*(AWD001)*/
                gosub write_glortran

        goto L19960

        get_fiscal_dates
            call "READ100" (#3, "FISCAL DATES", f1%(3))
                if f1%(3) = 0 then gosub'99(2%)
            get #3, using L12300, p%, u$(), monthopen%, clsdate$(1),~
                                clsdate$(2), adjaccount$
        goto L09920
L19960:
        exit_stage_left
                if returncode% = 99% then returncode% = 0%
                end

L20400:         FMT CH(9),               /* SKIP ACCOUNT NUMBER        */~
                    CH(30),              /* SKIP ACCOUNT DESCRIPTION   */~
                    CH(1),               /* AND ACCOUNT TYPE           */~
                    BI(1),               /* Obsolete Indicator         */~
                    BI(3),               /* NEXT DETAIL SEQUENCE NUMBER*/~
                    CH(112),             /* PRIOR YEAR DATA            */~
                    PD(14,4),            /* SEQNR, ACCT BALANCES       */~
                    17*PD(14,4)          /* SEQNR, ACCT BALANCES       */~

L21200:         FMT CH(16),              /* ACCOUNT NUMBER             */~
                    CH(6),               /* MODULE DATE POSTED         */~
                    BI(4),               /* SEQUENCE NUMBER STRING     */~
                    CH(2),               /* MODULE POSTED              */~
                    2*PD(14,4),          /* DEBIT, CREDIT AMOUNTS.     */~
                    CH(100),             /* ONE LONG STRING OF TEXT    */~
                                         /* CH(30) - REF #1            */~
                                         /* CH(34) - REF #2            */~
                                         /* CH( 4) - SPC. POST FLAG    */~
                                         /* CH(32) - FREE TEXT DESCR   */~
                    CH(3),               /* JOURNAL ID                 */~
                    BI(4),               /* POSTING SEQUENCE NUMBER    */~
                    CH(3),               /* OPERATOR ID OF CURRENT USER*/~
                    CH(6)                /* SYSTEM DATE WHEN WRITTEN.  */

/* (AWD001) */

GLORTRAN_FMT:        FMT CH(01),         /* TRANSMIT FLAG               */~
                         CH(16),         /* GL ACCOUNT                  */~
                         CH(02),         /* MODULE ID                   */~ 
                         CH(03),         /* JOURNAL ID                  */~ 
                         BI(04),         /* GL POSTING NUMBER           */~
                         BI(04),         /* POST SEQ BY MODULE/JOURNAL  */~
                         CH(03),         /* DIVISION                    */~ 
                         CH(01),         /* TRANSMIT FLAG               */~
                         CH(06),         /* SYSTEM DATE                 */~ 
                         CH(08),         /* SYSTEM TIME                 */~
                         CH(16),         /* GL ACCOUNT                  */~
                         CH(02),         /* MODULE ID                   */~ 
                         CH(03),         /* JOURNAL ID                  */~ 
                         BI(04),         /* GL POSTING NUMBER           */~
                         BI(04),         /* POST SEQ BY MODULE/JOURNAL  */~
                         CH(03),         /* USER ID                     */~
                         CH(16),         /* GL ACCOUNT                  */~
                         CH(06),         /* GL DATE                     */~
                         BI(04),         /* GL POSTING NUMBER           */~
                         PD(14,4),       /* DEBIT AMOUNT                */~
                         PD(14,4),       /* CREDIT AMOUNT               */~ 
                         CH(100),         /* ALL TEXT - GL REF1, GL REF2 */~
                                         /* ADJ FLAG, GL TEXT           */~
                         CH(290)         /* FILLER                      */

/* (AWD001) */

        REM *************************************************************~
            *             W A R N I N G   M E S S A G E                 *~
            *                                                           *~
            * SHOWS THE USER THE ACCOUNT AND AMOUNT WHEN POSTING ABORTS.*~
            *************************************************************

            deffn'99(err%)
            returncode% = 99%
            fdate$ = date$
            call "DATEFMT" (fdate$)
            acctfmt$ = account$
            call "GLFMT" (acctfmt$)
            call "EXTRACT" addr ("TT", ttype$)

            on err% goto L24100, L24500, L25000, L25600, L26000, L26200
L24100:     err$(1) = "Either you are not allow to make closing entries,"
            err$(2) = "or you are generating them from an invalid source."
            goto L26300

L24500:     err$(1) = "The fiscal date structure is invalid.  Please fix"
            err$(2) = "A.S.P., since NO G/L posting is possible until cor~
        ~rected"
            goto L26300

L25000:     err$(1) = "Account number (" & str(text$,69,12)
            err$(1) = err$(1) & ") is not on file,"
            err$(2) = "and the system supense acct (" & account$
            err$(2) = err$(2) & ") isn't either."
            goto L26300

L25600:     err$(1) ="The desired posting date conflicts with the current"
            err$(2) = "periods open/fiscal date structure."
            goto L26300

L26000:     err$(1) ="The General Ledger Master File (GLMAIN) either"
            err$(2) = "doesn't exist, or it can't be opened."
            goto L26300

L26200:     err$(1) ="Dual Books On: The Local Authority Suspense Account"
            err$(2) ="has not been defined. Run DBFLAGS to do so."


L26300:     mtext$(1) =                                                  ~
            "WARNING: G/L posting is NOT possible for the following trans~
        ~action...     "
            str(mtext$(1),79) = hex(0d)
            mtext$(2) = "Account:" & hex(84) & acctfmt$ & hex(8c)
            mtext$(2) = mtext$(2) & " Module:"   & hex(84) & modl$
            mtext$(2) = mtext$(2) & hex(8c)
            mtext$(2) = mtext$(2) & " Journal:"   & hex(84) & jnlid$
            mtext$(2) = mtext$(2) & hex(8c)
            mtext$(2) = mtext$(2) & " User:"     & hex(84) & userid$
            mtext$(2) = mtext$(2) & hex(8c)
            mtext$(2) = mtext$(2) & " Post Date:" & hex(84) & fdate$
            mtext$(2) = mtext$(2) & hex(8c)
            str(mtext$(2),79) = hex(0d)
            mtext$(3) = "Transaction Post Text:" &hex(84)& str(text$,69)
            call "CONVERT" (debit, 2.2, debit$)
            call "CONVERT" (credit, 2.2, credit$)
            if debit$ = "          0.00" then debit$ = " "
            if credit$ = "          0.00" then credit$ = " "
            str(debit$,,1), str(credit$,,1), str(monthopen$,,3) = hex(84)
            convert monthopen% to str(monthopen$,,3), pic(###)

          if ttype$ <> "B" then display_error
            REM Here if running in background...
            tran(mtext$(), hex(200d2084208c))replacing
            select printer (134)
            print page
            print "A T T E N T I O N:   * * SYSTEM ADMINISTRATOR * *"
            print : print
            print mtext$(1) : print mtext$(2) : print mtext$(3)
            print using L29600, debit, credit, monthopen$
L29600:     %DEBIT AMOUNT: #########.##          CREDIT AMOUNT: #########~
        ~.##     PERIOD OPEN ###
            print
            print err$(1) : print err$(2)
            close printer
            goto end_routine

/* (AWD001) */
        write_glortran

            write #4, using GLORTRAN_FMT, "S", account$, modl$, jnlid$,  ~
                              seqnr% + 1%, pstseq%, division$, "S", date,~
                              time, account$, modl$, jnlid$, seqnr% + 1%,~
                              pstseq%, userid$, account$, date$, seqnr% + 1%,~
                              gldebit, glcredit, text$,   " "


        return

        display_error
            call "GETPARM" addr("I ",              /* 'I' OR 'R'       */~
                                "R",               /* 'R'EQ            */~
                                "GL ERROR",        /* PRNAME           */~
                                "@",               /* PF KEY RECEIVER  */~
                                "0001",            /* MESSAGE ID       */~
                                "GLPOST",          /* MESSAGE ISSUER   */~
                                mtext$(),          /* SCREEN HEADER TXT*/~
                                237%,              /* MESSAGE LENGTH   */~
                                "U", "Transaction Amount:",19%, "A", 11%,~
                                "A", 3%,           /* COLUMN           */~
                                "T", "Debit amount:", 13%, "A", 12%,     ~
                                "A", 5%,           /* COLUMN           */~
                                "T", debit$, 14%, "A", 12%,              ~
                                "A", 23%,          /* COLUMN           */~
                                "T", "Credit amount:", 14%, "A", 13%,    ~
                                "A", 5%,           /* COLUMN           */~
                                "T", credit$, 14%, "A", 13%,             ~
                                "A", 23%,          /* COLUMN           */~
                                "T", "G/L Period Open:", 16%, "A", 15%,  ~
                                "A", 5%,           /* COLUMN           */~
                                "T", monthopen$, 3%, "A", 15%,           ~
                                "A", 34%,          /* COLUMN           */~
                                "U", "Error condition:   ",19%, "A", 17%,~
                                "A", 3%,           /* COLUMN           */~
                                "T", err$(1),   60%, "A", 18%,           ~
                                "A", 5%,           /* COLUMN           */~
                                "T", err$(2),   60%, "A", 19%,           ~
                                "A", 5%,           /* COLUMN           */~
                                "U", "NO POSTING HAS OCCURRED.  THIS SCRE~
        ~EN WILL BE PRINTED FOR LATER ANALYSIS.    ", 78%,                ~
                                "A",               /* ROW FLAG         */~
                                22%,               /* ROW              */~
                                "A",               /* COLUMN FLAG      */~
                                03%,               /* COLUMN NUMBER    */~
                                "T", "(16)ACKNOWLEDGE AND CONTINUE", 28%,~
                                "A", 24%, "A", 53%,"P",hex(00010000),"N")

            call "ZPRNTSCR"
        end_routine
            savedate$ = blankdate$    /* Force reload of control stuff */
            end
