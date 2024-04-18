        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   TTTTT   CCC   JJJJJ  U   U  RRRR   N   N  L       *~
            *  S        T    C   C    J    U   U  R   R  NN  N  L       *~
            *   SSS     T    C        J    U   U  RRRR   N N N  L       *~
            *      S    T    C   C  J J    U   U  R   R  N  NN  L       *~
            *   SSS     T     CCC    J      UUU   R   R  N   N  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCJURNL - Program run at standard cost implementation    *~
            *            to revalue fixed standard cost parts           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/08/90 ! Original                                 ! RAC *~
            * 11/11/92 ! PRR 12675  Corrected Report ID.          ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim account$9,                   /* Account number             */~
            acct$9,                      /* Account number unformatted */~
            company$60,                  /* Company or Division name   */~
            date$8,                      /* Run Date                   */~
            gl_post_info$(2)255,         /* G/L Posting Info           */~
            gltext$100,                  /* G/L Posting Text           */~
            jnlid$3,                     /* Journal ID                 */~
            modno$2,                     /* Module ID                  */~
            pacct$12,                    /* Account number formatted   */~
            plowkey$50,                  /* Plowkey key for JBCREDIT   */~
            postdate$6,                  /* User Posting Date          */~
            summary$1,                   /* Journal Summary indicator  */~
            suspense$9,                  /* Suspense Account           */~
            time$8,                      /* Time of Day                */~
            rptid$6,                     /* Journal Report ID          */~
            rpttitle$72,                 /* Journal Title              */~
            userid$3                     /* User ID                    */

        dim credit$13,                   /* CREDIT AMOUNTS THIS ENTRY  */~
            creditsstk$(1500)9,          /* CREDIT ACCOUNT RECAP STACK */~
            creditsstk(1500),            /* PARALLEL STACK FOR AMOUNTS */~
            debit$13,                    /* DEBIT AMOUNTS THIS ENTRY   */~
            debitsstk$(1500)9,           /* DEBIT RECAP STACK          */~
            debitsstk(1500),             /* PARALLEL STACK FOR AMOUNTS */~
            location$2,                  /* LOCATION OF ACCT ON STACK  */~
            rcpacct$(2)16,               /* ACCOUNT NUMBERS FOR RECAP  */~
            rcpacctdescr$(2)30,          /* DESCRIPTIONS FOR RECAP     */~
            rcpamt(2),                   /* RECAP AMOUNTS FOR STUFF    */~
            rcpline%(2),                 /* LINE COUNTER FOR RECAP     */~
            rcpptr%(2)                   /* POINTERS INTO RECAP STACK  */

        dim totaldebits$13,              /* Print Variable             */~
            totalcredits$13              /* Print Variable             */

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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! STCJRNTF ! Standard Cost Revaluation G/L TIF file   *~
            * #05 ! GLMAIN   ! General Ledger CHart Of Accounts File.   *~
            * #06 ! GLDETAIL ! General Ledger Detailed Transaction Hist *~
            * #07 ! USERINFO ! User Posting Dates                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "STCJRNTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 645,                                  ~
                         keypos =  1, keylen = 10                        ~

            select #05, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            select #06, "GLDETAIL",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  26                      ~

            select #07, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
           call "EXTRACT" addr("ID", userid$)
           date$ = date
           call "DATEFMT" (date$)

           rptid$ = "STC010"

*        Get Users Posting Date...
           call "READ100" (#7, userid$, f1%(7))
           if f1%(7) <> 0% then L09160
               call "ASKUSER" (keyhit%, "Sorry",                         ~
                 "You're Not Listed As A Valid User In This Data Base",  ~
                                         " ", "Press (RETURN) To Exit.")
               goto exit_program
L09160:    get #7, using L09170, postdate$
L09170:        FMT POS(22), CH(6)       /* GL POSTING DATE */

*        Validate Users Posting Date...
           call "WHICHMON" (#1, postdate$, temp%)
           if temp% <> 0% then L09270
               call "ASKUSER" (keyhit%, "Sorry",                         ~
                     "Your Posting Date Is Outside The Posting Window",  ~
                                         " ", "Press (RETURN) To Exit.")
               goto exit_program

L09270:    modno$ = "04"
           jnlid$ = "ISC"
           call "JNLINFO" (modno$, jnlid$, pstseq%, summary$, rpttitle$, ~
                           postdate$, #1, f2%(1), returncode%)
           call "FMTTITLE" (rpttitle$, "JOURNAL", 12%)

           call "COMPNAME" (12%, company$, returncode%)

           call "READ100" (#1, "FISCAL DATES", f1%(1))
               if f1%(1) = 0% then L65000
           get #1 using L09380, suspense$
L09380:    FMT POS(417), CH(9)

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

        /* GENERATE_REPORT */
            call "SHOSTAT" ("Printing Report and Posting General Ledger")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("STC010", " ", 0%, 0%)
            totaldebits, totalcredits = 0
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 60% then gosub page_head

*       * Report Generation Logic goes here
            init (hex(00)) plowkey$
            str(plowkey$,1,3) = userid$
L30140:     call "PLOWNEXT" (#2, plowkey$, 3%, f1%(2))
                if f1%(2) = 0% then L40000
            get #2 using L30170, acct$, gltext$, debit, credit,           ~
                gl_post_info$()
            call "READ100" (#5, acct$, f1%(5))
                 if f1%(5) = 0% then acct$ = suspense$
L30170:     FMT POS(11), CH(9), CH(100), PD(14,4), PD(14,4),             ~
                2*CH(255)
            str(gltext$,69,32) = "Standard Cost Set Implementation"
            if debit - credit < 0 then L30220
            debit = debit - credit
            credit = 0
            gosub'162 (acct$, debit)
            totaldebits = totaldebits + debit
            goto L30245
L30220:     credit = credit - debit
            debit = 0
            gosub'163 (acct$, credit)
            totalcredits = totalcredits + credit

L30245:     if summary$ = "Y" then L30280
            gosub post_to_gl
L30280:     lcntr% = lcntr% + 1%
            pacct$ = acct$
            call "GLFMT" (pacct$)
            if lcntr% > 60% then gosub page_head
            call "CONVERT" (debit, 2.2, debit$)
            call "CONVERT" (credit, 2.2, credit$)
            print using L60250, str(gltext$,31,25), str(gltext$,56,3),    ~
               str(gltext$,59,6), str(gltext$,69,),pacct$,debit$,credit$
            goto L30140


        end_report                /* Report Ending Routine */
            init (hex(00)) plowkey$
            str(plowkey$,1,3) = userid$
            call "DELETE" (#2, userid$, 3%)
            print skip(2)
            time$ = " "  :  call "TIME" (time$)
            print using L64981, time$ /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto L65000

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, rptid$
            print using L60110, rpttitle$, pcntr%
            print
            print using L60150
            print using L60170
            print using L60150
            print using L60210

            lcntr% = 8%
            return

        end_journal
             call "CONVERT" (totaldebits, 2.2, totaldebits$)
             call "CONVERT" (totalcredits, 2.2, totalcredits$)
             print using L60150
             print using L60290, totaldebits$, totalcredits$
             print using L60330
             return

L40000: REM *************************************************************~
            *         P R I N T   D A I L Y   R E C A P   I N F O       *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.  *******CENSORED********  WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

            gosub end_journal
            if debitsptr% = 0 and creditsptr% = 0 then end_report
            if summary$ = "Y" then gosub post_s_to_gl

            totaldebits, totalcredits, colsdone% = 0
            mat rcpline% = con
            mat rcpptr% = zer
            rcpacct$() = " "
            gosub L41120        /* SKIP TO TOP OF PAGE.                 */

L40160:     for column% = 1 to 2
                lcntr% = lcntr% + 1%
                if lcntr% > 60% then gosub L41120
                on column% gosub L40230, L40680
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then end_report     /* DONE W/ REPORT   */
                goto L40160

L40230:     REM HANDLES LEFT (DEBITS) COLUMN FOR REPORT
                on rcpline%(1) gosub L40270, L40500, L40540, L40580, L40620
                return

L40270:         REM PRINT CONTENTS OF DEBITS STACK, IF ANY.
                    if debitsptr% = 0 then L40540
                    rcpptr%(1) = rcpptr%(1) + 1
                    rcpamt(1)=debitsstk(rcpptr%(1))
                    rcpacct$(1)=debitsstk$(rcpptr%(1))
                    if rcpacct$(1) <> "OVER FLOW" then L40350
                       rcpacctdescr$(1) = "Rest Of Debits (can't print)"
                       goto L40420
L40350:             REM GET ACCOUNT DESCRIPTION
                        call "READ100" (#5, rcpacct$(1), f1%(5))
                        if f1%(5) = 0                                    ~
                           then rcpacctdescr$(1)="ACCOUNT NOT ON FILE"   ~
                           else get #5, using L40400, rcpacctdescr$(1)
L40400:                                 FMT XX(9), CH(30)
                    call "GLFMT" (rcpacct$(1))
L40420:             print using L41290,rcpacct$(1),rcpacctdescr$(1),      ~
                                         rcpamt(1);
                    totaldebits=totaldebits+debitsstk(rcpptr%(1))
                    totaldebits=round(totaldebits, 2)
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
                           FMT XX(9), CH(30)
L40500:         REM PRINTS SEPARATOR LINE.
                    print using L41280, "*--";
                    rcpline%(1) = 3
                    return
L40540:         REM PRINTS TOTAL LINE
                    print using L41300, totaldebits;
                    rcpline%(1) = 4
                    return
L40580:         REM PRINTS STARS
                    print using L41270, "*";
                    rcpline%(1) = 5
                    return
L40620:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L40680:     REM HANDLES RIGHT HAND COLUMN--CREDITS.
                print tab(68);
                on rcpline%(2) gosub L40720, L40940, L40980, L41020, L41060
                   return
L40720:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L40980
                    rcpptr%(2) = rcpptr%(2) + 1
                    rcpamt(2)=creditsstk(rcpptr%(2))
                    rcpacct$(2)=creditsstk$(rcpptr%(2))
                    if rcpacct$(2) <> "OVER FLOW" then L40800
                       rcpacctdescr$(2) = "Rest Of Credits (can't print)"
                       goto L40860
L40800:             REM PRINT ACCOUNT DESCIPTION.
                        call "READ100" (#5, rcpacct$(2), f1%(5))
                        if f1%(5)=0                                      ~
                           then rcpacctdescr$(2)="ACCOUNT NOT ON FILE"   ~
                           else get #5, using L40930, rcpacctdescr$(2)
                    call "GLFMT" (rcpacct$(2))
L40860:             print using L41290,rcpacct$(2),rcpacctdescr$(2),      ~
                               rcpamt(2);
                    totalcredits=totalcredits+creditsstk(rcpptr%(2))
                    totalcredits=round(totalcredits, 2)
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L40930:             FMT XX(9), CH(30)
L40940:         REM PRINT SEPARATOR LINE
                    print using L41280, "*--";
                    rcpline%(2) = 3
                    return
L40980:         REM PRINT TOTAL CREDITS LINE
                    print using L41310, totalcredits;
                    rcpline%(2) = 4
                    return
L41020:         REM PRINT STARS
                    print using L41270,"*";
                    rcpline%(2) = 5
                    return
L41060:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L41120:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, rptid$
            print using L60110, rpttitle$, pcntr%
            print
            print using L41420
            print
            print using L41460
            print using L41500, "#", "#"
            print using L41540
            lcntr% = 9%
                   return

L41270: %**************************#*************************************
L41280: %#--------------+--------------------------------+--------------*
L41290: %* ############ ! ############################## !-#########.## *
L41300: %*              ! TOTAL DEBITS                   !-#########.## *
L41310: %*              ! TOTAL CREDITS                  !-#########.## *

        %PAGE ##### ########################################  J O U R N A~
        ~ L  R E C A P          ##########################################~
        ~###

        %PAGE ##### ########################################  J O U R N A~
        ~ L  R E C A P          ##########################################~
        ~###
        %                                               ############

L41420: %=========================D E B I T S============================~
        ~   =============================C R E D I T S====================~
        ~==

L41460: %****************************************************************~
        ~   **************************************************************~
        ~**

L41500: %* ACCOUNT #    !     D E S C R I P T I O N      !    AMOUNT    *~
        ~   * ACCOUNT #    !     D E S C R I P T I O N      !    AMOUNT   ~
        ~ *

L41540: %*--------------+--------------------------------+--------------*~
        ~   *--------------+--------------------------------+-------------~
        ~-*

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  if abs(amount) < .001 then return
                  search str(debitsstk$(),1) = account$                  ~
                               to location$ step 9 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L41740  /* PUSH NEW ITEM*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH CELL?  */
                     debitsstk(junk%) = debitsstk(junk%) + amount
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L41740:           REM PUSH NEW ITEM ONTO STACK.
                      if debitsptr% < dim(debitsstk$(),1) then L41800
                         debitsstk$(debitsptr%) = "OVER FLOW"
                         debitsstk(debitsptr%) =                         ~
                         debitsstk(debitsptr%) + amount
                         return
L41800:               debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(account$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  if abs(amount) < .001 then return
                  search str(creditsstk$(),1) = account$                 ~
                               to location$ step 9
                               /* SCAN ONLY WHAT IS USED OF STACK      */
                  if location$ = hex(0000) then L41950  /* IF NO ==> NEW*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH ELEMENT*/
                     creditsstk(junk%) = creditsstk(junk%) + amount
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L41950:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      if creditsptr% < dim(creditsstk$(),1) then L42010
                         creditsstk$(creditsptr%) = "OVER FLOW"
                         creditsstk(creditsptr%) =                       ~
                         creditsstk(creditsptr%) + amount
                         return
L42010:               creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      creditsstk(creditsptr%) = amount
                      return

        REM *************************************************************~
            * POST SUMMARY ONLY TO GL                                   *~
            *************************************************************

        post_s_to_gl

            if summary$ <> "Y" then return
            if debitsptr% <> 0% then L45100
            if creditsptr% = 0% then return

L45100:     gltext$ = " "
            str(gltext$,69,32) = "Standard Cost Set Implementation"

            for p% = 1% to max(creditsptr%, debitsptr%)

                if p% > creditsptr% then L45230
                   acct$  = str(creditsstk$(p%),1,9)
                   debit  = 0
                   credit = creditsstk(p%)
                      gosub post_to_gl

L45230:         if p% > debitsptr% then L45290
                   acct$  = str(debitsstk$(p%),1,9)
                   credit = 0
                   debit  = debitsstk(p%)
                      gosub post_to_gl

L45290:     next p%

            return

        post_to_gl

            call "GLPOST2" (acct$, debit, credit, postdate$, 0%, modno$, ~
                            gltext$, jnlid$, pstseq%, userid$, #5, #6,   ~
                            #1, returncode%, " ", gl_post_info$())
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########             ############################~
        ~#################################                 STCJURNL:######

*       * Header Line 2
L60110: %                               #################################~
        ~#######################################               PAGE: ###

*       * Header line 3
L60150: %+---------------------------+-------+------+--------------------~
        ~---------------+-------------+---------------+---------------+

*       * Column Heading
L60170: %! Part Number               ! Store ! Lot  ! Description        ~
        ~               !   Account   !     Debit     !    Credit     !

*       * Blank Line
L60210: %!                           !       !      !                    ~
        ~               !             !               !               !

*       * Line item
L60250: %! ######################### !  ###  !######!  ##################~
        ~############## ! ########### ! ############# ! ############# !

*       * Journal totals
L60290: %                                                                ~
        ~        Journal Totals = >   ! ############# ! ############# !

*       * Journal totals underline
L60330: %                                                                ~
        ~                             +---------------+---------------+

L64981: %                             * * * * * * * * * *   E N D   O F  ~
        ~R E P O R T (@ ########)  * * * * * * * * * *

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "JNLCLOSE" (modno$, jnlid$, pstseq%, returncode%)
            call "SHOSTAT" ("One Moment Please")
            end
