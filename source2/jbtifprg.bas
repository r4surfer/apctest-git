        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  JJJJJ  BBBB   TTTTT  IIIII  FFFFF  PPPP   RRRR    GGG    *~
            *    J    B   B    T      I    F      P   P  R   R  G       *~
            *    J    BBBB     T      I    FFFF   PPPP   RRRR   G GGG   *~
            *  J J    B   B    T      I    F      P      R   R  G   G   *~
            *   J     BBBB     T    IIIII  F      P      R   R   GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTIFPRG - Print/Purge/Count Job Transaction Records      *~
            *            Selections include - Job Range                 *~
            *                               - Date Range                *~
            *                               - print and/or purge        *~
            *                               - some optional detail      *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/30/93 ! Original - REWRITE                       ! KEN *~
            * 08/14/96 ! Changes for the year 2000.               ! DXL *~
            * 04/04/97 ! Change PUTPARN call for NT Compatibility ! LDJ *~
	    PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            ask$(3)80,                   /* Ask User Strings           */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cjob$8, pjob$8,              /* Used in prnt, cnt control  */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            detail$1,                    /*   Print Detail?            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            job$(4)8,                    /* Job Number Range           */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            poption$1,                   /* Purge Option               */~
            pd$1, pm$1,                  /* Print File Info            */~
            pf$8, pl$8, pv$6,            /* Print File Info            */~
            rdate$(4)10,                 /* Transaction Date Range     */~
            rptid$6,                     /* Report Id                  */~
            rpttitle$60,                 /* Report Title               */~
            time$8,                      /* System Time                */~
            trantype$1,                  /*   Type (Posted, Rejected)  */~
            userid$3                     /* Current User Id            */~

        dim                                                              ~
            cost$96,                     /* Costs read in from disk    */~
            costs(12), costs$(12)10,     /* Costs for printing         */~
            emp$12,                      /* Employee number            */~
            erntype$12,                  /* Earnings type              */~
            expgl$12,                    /* Expense GL Account         */~
            func$16,                     /* Function literal           */~
            funcj1$(20)16,               /* Function fm JBPOST1        */~
            funcj2$(20)16,               /* Function fm JBPOST2        */~
            jnl$3,                       /* Journal ID                 */~
            job$8,                       /* Job Number                 */~
            jobdescr$30,                 /* Job Description            */~
            jobpart$25,                  /* Job Part                   */~
            lot$6,                       /* Lot Number                 */~
            mod$2,                       /* Module ID                  */~
            part$25, parthold$25,        /* TPart Number               */~
            pdate$6,                     /* Posting Date               */~
            prpdate$8,                   /* Formatted posting date     */~
            qty$10,                      /* formatted quantity         */~
            rate$10,                     /* formatted rate             */~
            rej$20,                      /* Reject Message             */~
            rwdate$8,                    /* Rework due date            */~
            stat$1,                      /* Status                     */~
            str$3,                       /* Store Number               */~
            tdate$6,                     /* Transaction Date           */~
            typ$(10)15,                  /* Value added type           */~
            prttdate$8,                  /* Formatted trans date       */~
            ttime$8,                     /* Transaction Time           */~
            uid$3,                       /* User ID                    */~
            wc$4,                        /* Work Center                */~
            work1$56,                    /* Work area 1                */~
            work2$40                     /* Work area 2                */

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #01 ! JBTIF    ! SFC Background Posting Transaction Image *~
            * #02 ! JBMASTR2 ! Production job master file               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "JBTIF",                                         ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    9, keylen =  18,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #02, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, f1%(64%))
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " & Press (RETURN).  PF32 to EXIT.  "

            rpttitle$ = "Print/Purge Job Transaction Re" &               ~
                        "cords                         "
            rptid$ = "JB0015"

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "JBTIFPRG: " & str(cms2v$,,8)

            funcj1$( 1) = "KIT COMPONENT  "
            funcj1$( 2) = "MOVE MTL IN    "
            funcj1$( 3) = "KIT COMPLETE   "
            funcj1$( 4) = "KIT SER #'S -JB"
            funcj1$( 5) = "not used       "
            funcj1$( 6) = "not used       "
            funcj1$( 7) = "KIT/RPT COMPL. "
            funcj1$( 8) = "KIT %/RPT CMP  "
            funcj1$( 9) = "KIT/RPT CMP JOB"
            funcj1$(10) = "KIT %/RPT > JOB"
            funcj1$(11) = "CLOSE JOURNAL  "
            funcj1$(12) = "TO JOB BLOCK   "
            funcj1$(20) = "KILL TASK      "

            funcj2$( 1) = "REPORT COMPLETE"
            funcj2$( 2) = "MOVE MTL J - J "
            funcj2$( 3) = "MOVE VAL J - J "
            funcj2$( 4) = "MOVE MTL J - I "
            funcj2$( 5) = "VALUE ADDED    "
            funcj2$( 6) = "SCRAP          "
            funcj2$( 7) = "REWORK         "
            funcj2$( 8) = "ADJ. J - INV.  "
            funcj2$( 9) = "RPT COMP -> JOB"
            funcj2$(10) = "DIR JOB QTY ADJ"
            funcj2$(11) = "CLOSE JOURNAL  "
            funcj2$(12) = "TO JOB BLOCK   "
            funcj2$(20) = "KILL TASK      "

            typ$(1) = "LABOR"
            typ$(2) = "WORK CENTER"
            typ$(3) = "DIRECT"
            typ$(4) = "CLOSING ADJ"
            typ$(5) = "CORE MAT VAL"
            typ$(6) = "CORE CREDITS"
            typ$(7) = "CORE CLSE ADJ"
            typ$(8) = "CORE TO F/G"

            lfac$() = " " : mod_admin% = 0%
            call "CMSMACHK" ("SFC", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then mod_admin% = 1%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       extract_data
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  7% then editpg1
            if fieldnr% = 3%           then    editpg1
            if fieldnr% > 3%           then    fieldnr% = fieldnr% - 1%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120


        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data

*          CALL "SHOSTAT" ("Scanning Job Transactions Log")
            print at(04,02), hex(84) & "Scanning Job Transactions Log"
*        Since we have a trailing ASKUSER status, don't hide the screen.

            trans% = 0% : jobs% = 0%
            cjob$ = all(hex(ff)) : pjob$ = all(hex(ff))

            plowkey$ = job$(3%)

            if pd$ = "N" then L19200
               select printer(134)
               time$ = " "  :  call "TIME" (time$)
               call "SETPRNT" (rptid$, " ", 0%, 0%)
               if pd$ <> "D" then L19170
                  call "EXTRACT" addr("PM", pm$)
                  call "SET" addr("PM", "K")
L19170:        pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
               rpttitle$ = "PRINT JOB TRANSACTION RECORDS (NO PURGE)"
               if poption$ <> "Y" then L19176
               rpttitle$ = "PRINT AND PURGE JOB TRANSACTION RECORDS"
L19176:        call "STRING" addr("CT", rpttitle$, 60%)


L19200:     call "PLOWALTS" (#1, plowkey$, 1%, 0%, f1%(1%))
               if f1%(1%) = 0% then end_report

            get #1 using L19240, plowkey$
L19240:         FMT CH(26)
            if str(plowkey$,1%,8%) <= str(job$(3%)) then L19200
            if str(plowkey$,1%,8%)  > str(job$(4%)) then end_report
            if trantype$ = "A" then L19310
            if trantype$ = "R" and str(plowkey$,9%,1%) <> "X" then L19200
            if trantype$ = "P" and str(plowkey$,9%,1%) <> "O" then L19200
            if trantype$ = "O" and str(plowkey$,9%,1%) <> " " then L19200

L19310:     if str(plowkey$,13%,6%) < str(rdate$(3%)) then L19200
            if str(plowkey$,13%,6%) > str(rdate$(4%)) then L19200

            if str(cjob$,,8%) <> str(plowkey$,1%,8%) then                ~
                                                     jobs% = jobs% + 1%
               cjob$ = str(plowkey$,1%,8%)
            trans% = trans% + 1%

            if pd$ <> "N" then gosub print_report
            if poption$ = "Y" then                                       ~
               call "DELETE" (#1, str(plowkey$,9%,18%), 18%)
            goto L19200

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Job Number             */~
                              L20200,         /* Date Range             */~
                              L20300,         /* Type (Pstd,Rejtd)      */~
                              L20400,         /* Print Option           */~
                              L20500,         /* Print Detail?          */~
                              L20600          /* Purge Option           */
            return
L20100: REM Def/Enable Job Number                  JOB$
            if job$(1%) = " " then job$(1%) = "ALL"
            return

L20200: REM Def/Enable Date Range                  RDATE$
            if rdate$(1%) = " " or rdate$(1%) = blankdate$ ~
                              then rdate$(1%) = "ALL"
            return

L20300: REM Def/Enable   Type (Posted, Rejected)   TRANTYPE$
            if trantype$ = " " then trantype$ = "R"
            return

L20400: REM Def/Enable   Print Option              PD$
            if pd$ = " " then pd$ = "P"
            return

L20500: REM Def/Enable   Print Detail?             DETAIL$
            if detail$ = " " then detail$ = "N"
            if pd$ = "N" then enabled% = 0%
            return

L20600: REM Def/Enable   Purge Option              POPTION$
            if poption$ = " " then poption$ = "N"
            if trantype$ <> "A" then return
               poption$ = "N"
               enabled% = 0%
               return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr% : inpmessage$ = " "
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Job Number Range (or ALL, FIRST, LAST ...).            ",~
         "Enter Date Range (or ALL, FIRST, LAST ...).                  ",~
         "Transaction Type (Posted, Rejected, Open or All). [P, R, O, A: ~
        ~Default = R]"                                                   ,~
         "Print or Display (Neither = Count). [P, D, N: Default = P]"   ,~
         "Print Detail? [A-Additional, C-Cost, B-Both, N-None: Default = ~
        ~N]"                                                             ,~
         "Purge Option (Administrator Only). [Y or N): Default = N]"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      detail$, job$(), poption$, rdate$(), trantype$, pd$

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

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
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        print_report

            get #1 using L35060, job$, stat$, id$, tdate$, ttime$, fnct%, ~
                                uid$, mod$, jnl$, pdate$, part$, str$,   ~
                                lot$, qty, work1$, work2$, cost$, rej$

            if pjob$ = job$ then L30210
               pjob$ = job$
               jobdescr$ = " " : jobpart$ = " "
               if pjob$ = " " then L30190
               call "READ100" (#2, pjob$, f1%(2%))
                  if f1%(2) = 0% then L30190
               get #2 using L30180, jobdescr$, jobpart$
L30180:            FMT POS(9), CH(30), POS(58), CH(25)
L30190:        if lcntr% > 48% then gosub page_head else gosub job_head

L30210:     prttdate$ = tdate$
            prpdate$  = pdate$
            call "DATEFMT" (prttdate$)
            call "DATEFMT" (prpdate$)
            call "CONVERT" (qty, 2.2, qty$)
            fncx% = min(20%, max(1%, fnct%))
            if trantype$ <> "A" then L30280
            if rej$      <> " " then L30280
               if stat$ = " " then rej$ = "OPEN"
               if stat$ = "O" then rej$ = "POSTED SUCCESSFULLY"
               if stat$ = "X" then rej$ = "REJECTED TRANSACTION"

L30280:     if id$ <> "J0" then L30310
               func$ = "HOLD JOB" : goto L30340

L30310:     if id$  = "J1" then func$ = funcj1$(fncx%)                   ~
                           else func$ = funcj2$(fncx%)     /* J2 or J3 */

L30340:     if lcntr% > 54% then gosub page_head

            if id$ <> "J0" then L30470
L30370:        uid$, mod$, jnl$, prpdate$, rej$ = " " /* ????? */

L30390:     print using L60310, id$, fnct%, prttdate$,                    ~
                               str(ttime$,1,2), str(ttime$,3,2),         ~
                               str(ttime$,5,2), str(ttime$,7,2),         ~
                               func$, uid$, mod$, jnl$, prpdate$,        ~
                               " ", " ", " ", " ", rej$
            lcntr% = lcntr% + 1%
            return   /* No Details Here */

L30470:     if id$ <> "J1" then L30630
               if fnct%  = 99% then L30370
               if fnct%  = 11% then L30390
               if fnct%  = 12% then L30390
               if fnct% <>  3% then L30550
                  qty = qty/100 : if qty < .01 then qty = 100
                  call "CONVERT" (qty, 2.2, str(qty$,,9%))
                  str(qty$,10%,1%) = "%"

L30550:     print using L60310, id$, fnct%, prttdate$,                    ~
                               str(ttime$,1,2), str(ttime$,3,2),         ~
                               str(ttime$,5,2), str(ttime$,7,2),         ~
                               func$, uid$, mod$, jnl$, prpdate$,        ~
                               part$, str$, lot$, qty$, rej$
            lcntr% = lcntr% + 1%
            goto L32000

L30630
*        Must be J2 or J3
               if fnct%  = 99% then L30370
               if fnct%  = 11% then L30390
               if fnct%  = 12% then L30390
               if fnct%  =  5% then L30691
               if fnct%  =  7% then L30691
                  goto L30550

L30691:     parthold$ = part$
            get part$ using L30693, typ%
L30693:         FMT BI(1)
            part$ = " "
            put part$ using L30696, typ%, typ$(typ%)
L30696:         %##) ###############

            print using L60310, id$, fnct%, prttdate$,                    ~
                               str(ttime$,1,2), str(ttime$,3,2),         ~
                               str(ttime$,5,2), str(ttime$,7,2),         ~
                               func$, uid$, mod$, jnl$, prpdate$,        ~
                               part$, " ", " ", " ", rej$

            part$ = parthold$
            lcntr% = lcntr% + 1%
            goto L32000

L32000
*       ** Print Cost Details and other detailish info.....

            if detail$ = "N" then return

*        First the costs
            if detail$ = "A" then L32100
               /* Either B or C */

            if cost$ = " " then init (hex(00)) cost$
            get cost$ using L32050, costs()
L32050:         FMT 12*PD(14,4)
            for b% = 1% to 12%
                call "CONVERT" (costs(b%), 4.4, costs$(b%))
            next b%
            print using L60430, costs$( 1), costs$( 2), costs$( 3),       ~
                               costs$( 4), costs$( 5), costs$( 6)
            print using L60450, costs$( 7), costs$( 8), costs$( 9),       ~
                               costs$(10), costs$(11), costs$(12)
            lcntr% = lcntr% + 2%

L32100
*        And now the ancilliary details
            if detail$ = "C" then return
               /* Either A or B */

            if id$ <> "J1" then L32200

            if fnct% <> 1% then L32170
               qty = 0 : convert str(work2$,,10) to qty, data goto L32160
               if qty = 0 then return
               call "CONVERT" (qty, 2.2, qty$)
               print using L60500, qty$
               lcntr% = lcntr% + 1%
L32160:        return

L32170:     if fnct% =  7% then j2_1
            if fnct% =  8% then j2_1
            if fnct% =  9% then j2_9
            if fnct% = 10% then j2_9
            return

L32200
*       ** Details for J2 & J3 (and four J1's)

            if fnct% <> 1% then L32235
        j2_1
               if str(work2$,3%,19%) = " " then                          ~
               print using L60530, str(work2$,1%,1%), str(work2$,2%,1%)   ~
                                          else                           ~
               print using L60535, str(work2$,1%,1%), str(work2$,2%,1%),  ~
                     str(work1$,31%,16%), str(work1$,47%,9%),            ~
                     str(work2$, 3%,16%), str(work2$,19%,3%)
               lcntr% = lcntr% + 1%
               return

L32235:     if fnct% <> 2% then L32260
               print using L60560, str(work1$,1%,8%)
               lcntr% = lcntr% + 1%
               return

L32260:     if fnct% <> 3% then L32330
                get part$ using L32270, typ%, emp$, wc$
L32270:             FMT BI(1), CH(12), CH(4)
                if typ% > 2% then return
                get work1$ using L32290, erntype$, rate
L32290:             FMT CH(12), XX(8), PD(14,4)
                call "CONVERT" (rate, 2.2, rate$)
                print using L60580, typ%, typ$(typ%), emp$, wc$, qty$,    ~
                                   rate$, str(work1$,45%,8%)
                lcntr% = lcntr% + 1%
                return

L32330:     if fnct% <> 4% then L32345
               return

L32345:     if fnct% <> 5% then L32415
                get part$ using L32355, typ%, emp$, wc$
L32355:             FMT BI(1), CH(12), CH(4)
                if typ% > 2% then return
                str(expgl$,1,3) = str$
                str(expgl$,4,6) = lot$
                get work1$ using L32375, erntype$, rate
L32375:             FMT CH(12), XX(8), PD(14,4)
                call "CONVERT" (rate, 2.2, rate$)
                call "GLFMT" (expgl$)
                print using L60600, typ%, typ$(typ%), emp$, wc$, qty$,    ~
                                   rate$, expgl$
                lcntr% = lcntr% + 1%
                return

L32415:     if fnct% <> 6% then L32430
               return

L32430:     if fnct% <> 7% then L32465
               rwdate$ = str(work2$,,8%)
               call "DATEFMT" (rwdate$)
               print using L60570, rwdate$
               lcntr% = lcntr% + 1%
               return

L32465:     if fnct% <> 8% then L32480
               return

L32480:     if fnct% <> 9% then L32510
        j2_9
               print using L60540, str(work2$,1%,1%), str(work2$,2%,1%),  ~
                                  str(work1$,1%,8%)
               lcntr% = lcntr% + 1%
               return

L32510:     if fnct% <> 10% then return
               return

        end_report                        /* Report Ending Routine */
            if pd$ = "N" then L33080
            if pcntr% < 0% then L33050     /* Didn't Print Anything */
               time$ = " "  :  call "TIME" (time$)
               print skip(2)
               print using L60170, time$   /* End of report line */
               call "GETPRTNM" addr(pf$, pl$, pv$)
L33050:        close printer : select ws
               if pd$ <> "D" then L33060
                  call "SET" addr("PM", pm$)
L33060:        call "SETPRNT" (" ", " ", 0%, 1%)
               if pd$ <> "D" then L33080
               if pcntr% < 0% then L33080  /* Didn't Print Anything */
            close ws
            call "PUTPARM" addr("E", "INPUT   ",4%,                      ~
                                               "FILE    ", pf$    ,  8%, ~
                                               "LIBRARY ", pl$    ,  8%, ~
                                               "VOLUME  ", pv$    ,  6%, ~
                                               "ACCESS  ", "PRINT ", 6%, ~
                                               "@",ret%)
            call "LINK" addr("DISPLAY ","S"," "," ",0%," "," ",0%,"N",   ~
                             u3%, ret%)
            call "SCRATCH" addr("F", pf$, pl$, pv$, " ", " ", ret%)
*          GOTO INPUTMODE

L33080:     init(" ") ask$()
            put ask$(1%) using L60210, jobs%
            put ask$(2%) using L60220, trans%
            ask$(3%) = "Press PF1 to return to INPUT, PF8 to EDIT," &    ~
                       " PF16 to EXIT PROGRAM"

L33140:     ask% = 2%
            call "ASKUSER" (ask%, "* * * TASK COMPLETE * * *",           ~
                            ask$(1%), ask$(2%), ask$(3%))
            if ask% = 1%  then inputmode
            if ask% = 8%  then editpg1
            if ask% = 16% then exit_program
               goto L33140

        page_head              /* Page Heading Print Routine */

            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "JBTIFPRG", rptid$
            print using L60110, rpttitle$, pcntr%
            lcntr% = 2%
            if pcntr% = 0% then print_params
        job_head               /* Job Heading Routine        */
            print skip(2)
            print using L60232, job$, jobdescr$, jobpart$
            print
            print using L60250
            print using L60280
            lcntr% = lcntr% + 6%

            return

        print_params           /* Print Page Zero */

L34522:     i% = pos(str(i$()) > hex(7f))
               if i% = 0% then L34550
                  str(i$(),i%,1%) = hex(20)
                  goto L34522

L34550:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"

            goto page_head     /* Again, and this time return */

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: JBTIF                             */~
            CH(8),          /* Production Job Code                     */~
            CH(1),          /* General purpose status indicator        */~
            CH(2),          /* acronymn tells taskup sub what task to s*/~
            XX(1),          /* Priority code.                          */~
            CH(6),          /* system (clock) date from the computer   */~
            CH(8),          /* Time from the system clock              */~
            BI(1),          /* Function Code                           */~
            CH(3),          /* User-ID of specific user                */~
            CH(2),          /* Identifier for an application module    */~
            CH(3),          /* Subsidiary Journal ID                   */~
            XX(4),          /* Posting sequence number within a Module/*/~
            CH(6),          /* Date Posted                             */~
            CH(25),         /* Part code                               */~
            CH(3),          /* Warehouse or Store                      */~
            CH(6),          /* Which lot in inventory                  */~
            PD(14,4),       /* Quantity                                */~
            CH(56),         /* Multiple Use                            */~
            CH(40),         /* Multiple Use                            */~
            CH(96),         /* Cost Breakdown                          */~
            CH(20)          /* Error Text                              */
                            /* CH(51) Unused Space                     */

        REM                 /* FILE - JBTIF  (J0)                      */~
            CH(8),          /* Production Job Code                     */~
            CH(1),          /* blank                                   */~
            CH(2),          /* "J0"                                    */~
            CH(1),          /* blank                                   */~
            CH(6),          /* system (clock) date from the computer   */~
            CH(8),          /* Time from the system clock              */~
            BI(1),          /* Hex(20) - blank                         */~
            CH(3),          /* User-ID of specific user                */~
            CH(120),        /* blank                                   */~
            CH(200)         /* blank                                   */~

        REM                 /* FILE - JBTIF (J1)                       */~
            CH(8),          /* Production Job Code                     */~
            CH(1),          /* Status Indicator - blank, O, X          */~
            CH(2),          /* "J1"                                    */~
            CH(1),          /* Priority code.                          */~
            CH(6),          /* system (clock) date from the computer   */~
            CH(8),          /* Time from the system clock              */~
            BI(1),          /* Fnct - 1, 2, 3, 4, 7, 8, 9, 10, 11      */~
                            /*        7, 9 -> J2/1 & 9, 10 -> J2/9     */~
                            /*        they just wait at J1 for kitting */~
            CH(3),          /* User-ID of specific user                */~
            CH(2),          /* Identifier for an application module    */~
            CH(3),          /* Subsidiary Journal ID                   */~
            BI(4),          /* Posting sequence number within a Module/*/~
            CH(6),          /* Date Posted                             */~
            CH(25),         /* Part code                               */~
            CH(3),          /* Warehouse or Store                      */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            PD(14,4),       /* Quantity (100 * PCT to KIT - Type 3)    */~
            CH(56),         /* Type 1 - PIPOUT Key                     */~
            CH(40),         /* Type 1 - CH(10) Pipout Adjustment Qty   */~
                            /* Type 1, 2, 4, POS(38), BI(3) SERTIF Seq */~
            CH(96),         /* Cost Breakdown                          */~
            CH(20)          /* Error Text                              */
                            /* CH(51) Unused Space                     */

        REM                 /* FILE - JBTIF (J2)                       */~
            CH(8),          /* Production Job Code                     */~
            CH(1),          /* Status Indicator - blank, O, X          */~
            CH(2),          /* "J2"                                    */~
            CH(1),          /* Priority code.                          */~
            CH(6),          /* system (clock) date from the computer   */~
            CH(8),          /* Time from the system clock              */~
            BI(1),          /* Fnct - 1 through 11                     */~
            CH(3),          /* User-ID of specific user                */~
            CH(2),          /* Identifier for an application module    */~
            CH(3),          /* Subsidiary Journal ID                   */~
            BI(4),          /* Posting sequence number within a Module/*/~
            CH(6),          /* Date Posted                             */~
            CH(25),         /* Part code - Except.....                 */~
                            /* Type 3 - Pos( 1), BI( 1) JBVL Type      */~
                            /*          Pos( 2), ch(12) Employee Code  */~
                            /*          Pos(14), ch( 4) Work Center    */~
                            /* Type 5 - Pos( 1), BI( 1) JBVL Type      */~
                            /*          Pos( 2), ch(12) Employee Code  */~
                            /*          Pos(14), ch( 4) Work Center    */~
                            /*          Pos(18), ch( 6) Trans Date     */~
            CH(3),          /* Warehouse or Store                      */~
            CH(6),          /* Which lot in inventory - always used wit*/~
                            /* Type 5 - Store/Lot contains G/L Account */~
            PD(14,4),       /* Quantity                                */~
            CH(56),         /* Type 1 - Pos( 9), ch(22) JBCREDIT Key   */~
                            /*      1 - Pos(31), ch(25) RCV & VEN (PJ) */~
                            /* Type 2 - Pos( 1), ch(8)  To Job Code    */~
                            /*      2 - Pos( 9), ch(22) JBMATER2 Key   */~
                            /*      2 - Pos(55), bi(2)  SERDTL Seq     */~
                            /* Type 3 - Pos( 1), ch(12) Earn Type      */~
                            /*      3 - Pos(13), ch( 4) Labor Class    */~
                            /*      3 - Pos(17), ch( 4) Activity       */~
                            /*      3 - Pos(21), PD( 8) Unit Rate      */~
                            /*      3 - Pos(29), PD( 8) Labor2         */~
                            /*      3 - Pos(37), PD( 8) Overhead       */~
                            /*      3 - Pos(45), ch( 8) From Job code  */~
                            /* Type 4 - Pos( 9), ch(22) JBMATER2 Key   */~
                            /*      4 - Pos(55), bi(2) SERDTL Seq      */~
                            /* Type 5 - Pos( 1), ch(12) Earn Type      */~
                            /*      5 - Pos(13), ch( 4) Labor Class    */~
                            /*      5 - Pos(17), ch( 4) Activity       */~
                            /*      5 - Pos(21), PD( 8) Unit Rate      */~
                            /*      5 - Pos(29), PD( 8) Labor2         */~
                            /*      5 - Pos(37), PD( 8) Overhead       */~
                            /*      5 - Pos(45), ch( 9) Overhead Acct  */~
                            /* Type 9 - Pos( 1), ch( 8) To Job Code    */~
            CH(40),         /* Type 1 - Pos( 1), ch(2) Cost Flags      */~
                            /*      1 - Pos( 3), ch(19) PO & Line (PJ) */~
                            /*      1 - Pos(38), bi(3) SERTIF seq      */~
                            /* Type 3 - JBVLPOST Posting Text          */~
                            /* Type 5 - JBVLPOST Posting Text          */~
                            /* Type 7 - Pos( 1), ch(6) RWK Job End Dte */~
                            /* Type 9 - Pos( 1), ch(2) Cost Flags      */~
                            /*      9 - Pos(38), bi(3) SERTIF seq      */~
            CH(96),         /* Cost Breakdown                          */~
            CH(20)          /* Error Text                              */
                            /* CH(51) Unused Space                     */

        REM                 /* FILE - JBTIF (Function 11, J1 & J2)     */~
            CH(8),          /* Blank                                   */~
            CH(1),          /* Status Indicator - blank, O, X          */~
            CH(2),          /* "J1" or "J2"                            */~
            CH(1),          /* Priority code. Hex(99)                  */~
            CH(6),          /* system (clock) date from the computer   */~
            CH(8),          /* Time from the system clock              */~
            BI(1),          /* Fnct - 11, Close G/L Journal            */~
            CH(3),          /* User-ID of specific user                */~
            CH(2),          /* Identifier for an application module    */~
            CH(3),          /* Subsidiary Journal ID                   */~
            BI(4),          /* Posting sequence number within a Module/*/~
            CH(6),          /* Date Requested                          */~
            CH(34),         /* Blank                                   */~
            PD(14,4),       /* 0 (Zero)                                */~
            CH(212)         /* Blank                                   */~
                            /* CH(51) Unused Space                     */

        REM                 /* FILE - JBTIF (Function 99, J1 & J2)     */~
            CH(8),          /* Blank                                   */~
            CH(1),          /* Status Indicator - blank, O, X          */~
            CH(2),          /* "J1" or "J2"                            */~
            CH(1),          /* Priority code. Hex(01) or Hex(FF)       */~
            CH(6),          /* system (clock) date from the computer   */~
            CH(8),          /* Time from the system clock              */~
            BI(1),          /* Fnct - 99, Task Down                    */~
            CH(8),          /* Blank                                   */~
            BI(4),          /* 0%                                      */~
            CH(40),         /* Blank                                   */~
            PD(14,4),       /* 0 (Zero)                                */~
            CH(212)         /* Blank                                   */~
                            /* CH(51) Unused Space                     */

        REM Last but not least, there is a "J3" which can be decoded as  ~
            J2.  It waits for a J2 task to finish, but cannot itself be  ~
            processed unless and until its 'linked' J2 is successful.    ~
            Then it is found and changed to a J2.

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40095,         /* Job Number        */   ~
                                L40095,         /* Date Range        */   ~
                                L40095,         /* Type (Pstd,Rejtd) */   ~
                                L40095,         /* Print Option      */   ~
                                L40095,         /* Print Detail?     */   ~
                                L40095          /* Purge Option      */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Print/Purge Job Transaction Records",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Job Number",                                 ~
               at (07,30), fac(lfac$( 1)), job$(1%)             , ch(08),~
               at (07,56), fac(lfac$( 1)), job$(2%)             , ch(08),~
                                                                         ~
               at (08,02), "Date Range",                                 ~
               at (08,30), fac(lfac$( 2)), rdate$(1%)           , ch(10),~
               at (08,56), fac(lfac$( 2)), rdate$(2%)           , ch(10),~
                                                                         ~
               at (10,02), "  Type - Posted, Rejected, Open:",           ~
               at (10,35), fac(lfac$( 3)), trantype$            , ch(01),~
                                                                         ~
               at (11,02), "  Print or Display?       Print:",           ~
               at (11,35), fac(lfac$( 4)), pd$                  , ch(01),~
                                                                         ~
               at (12,02), "  Print Detail?          Detail:",           ~
               at (12,35), fac(lfac$( 5)), detail$              , ch(01),~
                                                                         ~
               at (13,02), "  Purge Option?           Purge:",           ~
               at (13,35), fac(lfac$( 6)), poption$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40315
                  call "MANUAL" ("JBTIFPRG") : goto L40110

L40315:        if keyhit% <> 15 then L40330
                  call "PRNTSCRN" : goto L40110

L40330:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40425     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
*          IF FIELDNR% = 1% THEN 40410
*              STR(PF$(3),64)    = " "  :  STR(PFKEYS$,16,1) = HEX(FF)
            if fieldnr% > 1% then L40415
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40415:     return

L40425: if fieldnr% > 0% then L40470  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Process File"
           if pd$  = "P" then                                            ~
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
           if pd$  = "D" then                                            ~
            pf$(3) = "                                        " &        ~
                     "                       (16)Display Rpt "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f102000)
            return
L40470:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50100,         /* Job Number             */~
                              L50200,         /* Date Range             */~
                              L50300,         /* Type (Pstd,Rejtd)      */~
                              L50400,         /* Print Option           */~
                              L50500,         /* Print Detail?          */~
                              L50600          /* Prnt and/or Purge      */
            return
L50100: REM Test for Job Number                   JOB$
            call "TESTRNGE"                                              ~
                  (job$(1%), job$(2%), job$(3%), job$(4%), errormsg$, #2)
            return

L50200: REM Test for Date Range                   RDATE$
            if rdate$(1%)  = "ALL"   then L50206
            if rdate$(1%) <> "FIRST" then L50210
L50206:        rdate$(3%) = all(hex(00)) : d1% = 0%
               goto L50218
L50210:     call "DATEOKC" (rdate$(1%), d1%, errormsg$)
               if errormsg$ <> " " then return
            rdate$(3%) = rdate$(1%) : call "DATUFMTC" (rdate$(3%))

L50218:     if rdate$(1%) <> "ALL" then L50222
               rdate$(2%) = " " : goto L50224
L50222:     if rdate$(2%) <> "LAST" then L50226
L50224:        rdate$(4%) = all(hex(ff)) : d2% = 99999999% : goto L50242
L50226:     if rdate$(2%) <> " " and rdate$(2%) <> blankdate$ then L50234
               if d1% > 0% then L50232
               rdate$(2%) = "LAST" : goto L50224
L50232:     rdate$(2%) = rdate$(1%)
L50234:     call "DATEOKC" (rdate$(2%), d2%, errormsg$)
               if errormsg$ <> " " then return
            rdate$(4%) = rdate$(2%) : call "DATUFMTC" (rdate$(4%))

L50242:     if d1% <= d2% then return
            errormsg$ = "Invalid Receipt Date range."
            return

L50300: REM Test for   Type (Posted, Rejected)    TRANTYPE$
            if pos("AOPR" = trantype$) <> 0% then L50391
            errormsg$ = "Invalid Transaction Type (Status)."
            return
L50391:     if trantype$ = "A" then poption$ = "N"
            return

L50400: REM Test for   Print                      PD$
            if pos("PDN" = str(pd$,1%,1%)) <> 0% then return
               errormsg$ = "Invalid Selection for Print/Display Option."
               return

L50500: REM Test for   Print Detail?              DETAIL$
            if pos("ABCN" = detail$) <> 0% then return
            errormsg$ = "Invalid Selection for Detail Option."
            return

L50600: REM Test for   Purge                      POPTION$
            if mod_admin% = 0% then poption$ = "N"
            if trantype$ = "A" then poption$ = "N"
            if pos("YN" = str(poption$,1%,1%)) <> 0% then return
               errormsg$ = "Invalid Selection for Purge Option."
               return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

L60170: %                                             ********** END OF R~
        ~EPORT @ ######## **********

*       * Ask User Images
L60210: % A total of ######## Jobs were found with eligible transactions.
L60220: % A total of ########## Transactions were included.

*       * Job Header
L60232:   %***** JOB NUMBER: ########  ############################## PAR~
        ~T: #########################

*       * Report Images
L60250:   %ID FCT TRANS DATE & TIME    FUNCTION        USR MD JNL PST DAT~
        ~E PART NUMBER (VAL DESCR)   STORE  LOT   QUANTITY TRANS. MESSAGE

L60280:   %--------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-----
L60310:   %## ### ######## ##:##:##:## ############### ### ## ### #######~
        ~# ######################### ### ###### ########## ###############~
        ~#####

*       * Cost Detail Images
L60430: %         COSTS:  1) ##########   2) ##########   3) ##########  ~
        ~! 4) #########   5) ##########   6) ##########
L60450: %                 7) ##########   8) ##########   9) ##########  ~
        ~!10) #########  11) ##########  12) ##########

*       * Additional Information Images
*       * J1
L60500: %         PIPOUT ADJUSTMENT QUANTITY ########

*       * J2
L60530: %         COMPLETION VALUE METHOD - BOM: #  T/L: #
L60535: %         COMPLETION VALUE METHOD - BOM: #  T/L: #  RCV #########~
        ~####### VENDOR ######### PO ################ LINE ###
L60540: %         COMPLETION VALUE METHOD - BOM: #  T/L: #  COMPLETED TO ~
        ~JOB ########
L60560: %         MOVED TO JOB ########
L60570: %         REWORK PLANNED COMPLETE DATE: ########
L60580: %         ##) ############### EMP: ############ WC: ####  #######~
        ~### HRS @ ##########  $/HR   MOVED FROM JOB: #########
L60600: %         ##) ############### EMP: ############ WC: ####  #######~
        ~### HRS @ ##########  $/HR   EXP ACCOUNT: ############

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
