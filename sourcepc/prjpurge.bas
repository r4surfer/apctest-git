        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   RRRR   JJJJJ  PPPP   U   U  RRRR    GGG   EEEEE   *~
            *  P   P  R   R     J   P   P  U   U  R   R  G      E       *~
            *  PPPP   RRRR      J   PPPP   U   U  RRRR   G GGG  EEEE    *~
            *  P      R  R   J  J   P      U   U  R   R  G   G  E       *~
            *  P      R   R   JJ    P       UUU   R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRJPURGE - Purges details for a project from project(mastr*~
            *            budget mtldr  mtlcm  purch  labor  sales) and  *~
            *            PIP files.  Calls PIPFLAGS to balance the PIP. *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/12/86 ! KENNY (NEW HEADER CREATED)               ! KEN *~
            * 09/12/86 ! HAL  ADDED DELETE OF PIPS AND PIPFLAGS   ! HAL *~
            * 10/09/92 ! Changed OPENFILE mode from 'IO' to       ! FMZ *~
            *          ! 'SHARE' for all data files-Pyrotek request     *~
            * 10/22/92 ! Changed name from 'JOB' to "PRJPURGE"    ! FMZ *~
            * 10/27/92 ! Cleanup. (Why me?) & PRR11543 Add GETCODE! JDH *~
            * 02/01/93 ! PRR 12592 Purge by Range of Project #s.  ! JIM *~
            * 02/01/93 ! Code relating to screens, etc., generally! JIM *~
            * 02/01/93 !   brought up to standard. (Minor rewrite)! JIM *~
            * 02/01/93 ! ALLFREE.                                 ! JIM *~
            * 02/01/93 ! Some integer conversions fixed.          ! JIM *~
            * 08/26/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                /* BLANK LINE FOR INPUT SCREEN*/~
            column$19,                   /* Screen column header       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            datejobclosed$6,             /* Date project closed        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            i$(24)80,                    /* Screen Image               */~
            inkey$19,                    /* KEY FOR PIPIN DELETE       */~
            jobbyebye$(4)8, jobbyebye$8, /* Projects to be purged      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* TOP line on Input Screen   */~
            outkey$48,                   /* KEY FOR PIPOUT DELETE      */~
            part$25,                     /* PART NUMBER                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* USER ID OF PURGER          */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

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
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! JOBMASTR ! WIPME/ JOB COST YOU NASTY MASTER.        *~
            * # 2 ! JOBBUDGT ! WIP/JC BUDGETS FILE                      *~
            * # 3 ! JOBMTLDR ! JOB MATERIAL DIRECT DETAIL FILE          *~
            * # 4 ! JOBMTLCM ! JOB MATERIAL COMMITTMENTS FILE           *~
            * # 5 ! JOBPURCH ! JOB PURCHASES DETAIL FILE                *~
            * # 6 ! JOBLABOR ! JOB LABOR DETAIL FILE                    *~
            * # 7 ! JOBSALES ! JOB TO SALES INVOICING DETAIL FILE       *~
            * # 8 ! USERINFO ! USER INFORMATION FILE                    *~
            * # 9 ! PIPIN    ! PIP IN FILE                              *~
            * #10 ! PIPOUT   ! PIP OUT FILE                             *~
            * #11 ! PIPMASTR ! PIP MASTR FILE                           *~
            * #12 ! SFCUM2   ! Sales Forecast Cumulative                *~
            *************************************************************

            select #1,  "JOBMASTR",                                      ~
                        varc                                             ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8

            select #2,  "JOBBUDGT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos=1 , keylen = 13

            select #3,  "JOBMTLDR"                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 16

            select  #4, "JOBMTLCM"                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos=1, keylen= 16

            select  #5, "JOBPURCH",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos  = 1, keylen = 16                         ~

            select # 6, "JOBLABOR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 16

            select  #7, "JOBSALES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 16

            select # 8, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos  = 1, keylen = 3

            select  #9, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 60,                                    ~
                        keypos =30, keylen = 19

            select #10, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  64,                                   ~
                        keypos  = 1, keylen = 56,                        ~
                        alt key 1, keypos = 20, keylen = 37

            select #11, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26
            select #12,  "SFCUM2",                                       ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 1985,                                ~
                          keypos = 1, keylen = 25

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
            call "OPENFILE" (# 9, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZATION OF CERTAIN NECESSARY SYSTEM VARIABLES.     *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID" , userid$)
            call "READ100" (#8, userid$, f1%(8%))
            if f1%(8%) = 0% then L65000
            date$ = date
            call "DATEFMT" (date$)
            column$ = "From       To"
            str(line2$,62%) = "PRJPURGE: " & str(cms2v$,1%,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            call "ALLFREE"
            init(" ") jobbyebye$(), errormsg$
            blankline$ = "Enter Project Number range, partial, 'FIRST', '~
        ~LAST', 'ALL' or '?' Wildcard."

            for fieldnr% = 1% to 1%
                gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then goto L10190
L10150:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit% =  1% then gosub startover
                      if keyhit% = 16% then goto L65000
                      if keyhit% <> 0% then goto L10150
L10190:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then goto L10150
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            blankline$ = "Press (RETURN) to edit the input range."
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto do_the_purge
                  if keyhit% <>  0% then goto editpg1
            fieldnr% = 1%
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then goto editpg1
L11142:     blankline$ = "Enter Project Number range, partial, 'FIRST', '~
        ~LAST', 'ALL' or '?' Wildcard."
            gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then goto L11142
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then goto L11142
            goto editpg1

        REM *************************************************************~
            *  H E R E ' S   W H E R E   W E   D O   T H E   P U R G E  *~
            *************************************************************

        do_the_purge
            anypurged%, f1%(1%) = 0%                /* Prime the READs */

        do_the_purge_2
            if f1%(1%) = 0%                                              ~
                then call "READ102" (#1, jobbyebye$(3%), f1%(1%))        ~
                else call "READNEXT" (#1, f1%(1%))
            if f1%(1%) = 0% then goto end_of_purge
            get #1 using L12130, jobbyebye$, datejobclosed$
L12130:         FMT CH(8), POS(45), CH(6)
            if jobbyebye$ > jobbyebye$(4%) then goto end_of_purge
            if datejobclosed$ = " " or datejobclosed$ = blankdate$ ~
                                     then goto do_the_purge_2   /* Open */
            if datejobclosed$ > date then goto do_the_purge_2/* Future */
            anypurged% = 1%         /* Indicate 'something was purged' */

            call "SHOSTAT" ("Purging Project #: " & jobbyebye$)

            call "DELETE" (#2, jobbyebye$, 8%)             /* JOBBUDGT */
            call "DELETE" (#3, jobbyebye$, 8%)             /* JOBMTLDR */
            call "DELETE" (#4, jobbyebye$, 8%)             /* JOBMTLCM */
            call "DELETE" (#5, jobbyebye$, 8%)             /* JOBPURCH */
            call "DELETE" (#6, jobbyebye$, 8%)             /* JOBLABOR */
            call "DELETE" (#7, jobbyebye$, 8%)             /* JOBSALES */
            call "DELETE" (#1, jobbyebye$, 8%)             /* JOBMASTR */

*        Delete the PIPIN & PIPOUT records, then adjust/balance the PIP.
            str(inkey$,1%,8%) = "JOB(PR):"
            str(inkey$,9%,8%) = jobbyebye$

L12320:     call "PLOWNXT1" (#9, inkey$, 16%, f1%(9%))        /* PIPIN */
            if f1%(9%) = 0% then L12400
            get #9, using L12350, part$, outdate%, quantity
L12350:         FMT CH(25), BI(4), XX(19), PD(14,4)
            delete #9
            call "PIPFLAGS" (part$, 1%, outdate%, -quantity, #11, #12)
            goto L12320

L12400:     str(outkey$,1%,11%) = "JOB(PROJ): "
            str(outkey$,12%,8%) = jobbyebye$

L12430:     call "PLOWNXT1" (#10, outkey$, 19%, f1%(10%))    /* PIPOUT */
            if f1%(10%) = 0% then goto do_the_purge_2
            get #10, using L12460, part$, outdate%, quantity
L12460:         FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
            delete #10
            call "PIPFLAGS" (part$, 1%, outdate%, quantity, #11, #12)
            goto L12430

        end_of_purge
            if anypurged% <> 0% then goto inputmode
                u3% = 2%                           /* Window at bottom */
                call "ASKUSER" (u3%, "*** NULL SET SELECTED ***",        ~
                     "There were no Projects purged based on your crite"&~
                     "ria.", " ", "Press any PF key to acknowledge and "&~
                     "continue.")
                goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20110          /* Range of Project #s    */
            return

L20110: REM Def/Enable Range of Project #s to Purge JOBBYEBYE$()
            if str(jobbyebye$()) = " " then jobbyebye$(1%) = "ALL"
            return

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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40140          /* Range of Project #s */
              goto L40170

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40170:     accept                                                       ~
               at (01,02), "Purge/Delete Project Records",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,36), fac(hex(ac)),   column$              , ch(19),~
                                                                         ~
               at (07,02), "Range of Project #s to Purge:",              ~
               at (07,36), fac(lfac$(1%)), jobbyebye$(1%)       , ch(08),~
               at (07,47), fac(lfac$(1%)), jobbyebye$(2%)       , ch(08),~
                                                                         ~
               at (09,02), "(Note that only CLOSED projects in the above ~
        ~range will be purged)",                                          ~
               at (21,02), fac(hex(a4)),   blankline$           , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40420
                  call "MANUAL" ("PRJPURGE") : goto L40170

L40420:        if keyhit% <> 15% then L40450
                  call "PRNTSCRN" : goto L40170

L40450:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40600     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return

L40600: if fieldnr% > 0% then L40690  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Do the PURGE"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40690:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100          /* Range of Project #s    */
            return

L50100: REM Test for Range of Project #s to Purge  JOBBYEBYE$()
            call "TESTRNGE" (jobbyebye$(1%), jobbyebye$(2%),             ~
                jobbyebye$(3%), jobbyebye$(4%), errormsg$, #1)
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
