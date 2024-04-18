        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR   JJJJJ  N   N  L       SSS   BBBB    *~
            *  C   C  O   O  R   R    J    NN  N  L      S      B   B   *~
            *  C      O   O  RRRR     J    N N N  L       SSS   BBBB    *~
            *  C   C  O   O  R   R  J J    N  NN  L          S  B   B   *~
            *   CCC    OOO   R   R   J     N   N  LLLLL   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORJNLSB - Handles interface between application and the  *~
            *            GL TIF.  Functionality incluedes               *~
            *              1) Journal Open                              *~
            *              2) Journal Write                             *~
            *              3) Journal Close                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/25/92 ! Original                                 ! KEN *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "CORJNLSB" (f$, pglseq1%, glseq2%, glseqinc%, caudit%,   ~
                            glcde$, gltwas$, gltcur$, glinfo$, ret%)

        dim /* The Argument List -                                     */~
            f$2,          /* Function Code - JS = Set Up               */~
                          /*               - JW = Write Transaction    */~
                          /*               - JE = Journal End          */~
            glcde$2,      /* Transaction Type Identifier               */~
            gltwas$90,    /* Previous (Reversal) Block                 */~
            gltcur$90,    /* Current Block                             */~
            glinfo$135    /* Additional Data for GL Posting & Audit    */

        REM /* The Argument List Continued -                           */~
            GLSEQ1%       /* 'Data Save' Sequence # (Unusual for       */~
                          /* Calling Program to Modify other than via  */~
                          /* 4th Argument, but . . .                   */~
            GLSEQ2%       /* Relative Transaction # within data save.  */~
                          /* Responsibility of Calling Program!!       */~
            GLSEQINC%     /* GLSEQ1% Increment. Zeroed after Honoring  */~
                          /* If Caller Sets to 1% at top of Data Save  */~
                          /* and Doesn't touch it again, All should be */~
                          /* fine.  I.E. no zero denotes start of a    */~
                          /* logical subset, zero denotes continuation.*/~
            CAUDIT%       /* Caller's Control over Audit File          */~
                          /*     0 - Don't Write                       */~
                          /*     1 - Write Always                      */~
                          /*     2 - Write Only If Significant (GL)    */~
                          /*     3 - Write Only Audit (Purge?)         */~
                          /* If CORFLAGS says no this is meaningless.  */~
            RET%          /* Return Code (what else)                   */~
                          /*    99 - Core Not Installed                */~
                          /*    98 - JS Unsuccessful                   */~
                          /*    97 - ?? Unsuccessful (Not Expected)    */~
                          /*     0 - All Ok Fine.                      */

        dim /* The Local Variables                                     */~
            fn$2,         /* Function Requested                        */~
            gldate$8,     /* Posting Date                              */~
            glhdr$25,     /* Transaction Header                        */~
            gltranx$90,   /* Null Transaction Block                    */~
            gltrn0$90,    /* Former (Reversal) Block                   */~
            gltrn1$90,    /* Current Block                             */~
            gldata$135,   /* Additional Transaction Information        */~
            readkey$100,  /* General Purpose File Key                  */~
            plowkey$100,  /* General Purpose File Key                  */~
            swtchs$200,   /* Hold Switches                             */~
            progid$8,     /* Program Id (Caller)                       */~
            userid$3      /* User Id                                   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #18 ! USERINFO ! User Posting Dates                       *~
            * #19 ! CORJNLTF ! Core Bank G/L Journal TIF                *~
            * #20 ! CORAUDIT ! Core Bank Audit File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #18, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =   3                      ~

            select #19, "CORJNLTF",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  33,                     ~
                        alt key  1, keypos =    4, keylen =  30          ~

            select #20, "CORAUDIT",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  33,                     ~
                        alt key  1, keypos =    4, keylen =  30          ~

            if swtchs% <> 0% then L09000

            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")

            readkey$ = "SWITCHS.COR"
            call "READ100" (#1, readkey$, swtchs%)
               if swtchs% <> 0% then L05080
                  swtchs%  = -1% : goto L09000
L05080:     get #1 using L05090, swtchs$
L05090:         FMT POS(21), CH(200)
            swtchs% = 1%
            if str(swtchs$,83,1) = "Y" then audit% = 1%
            call "OPENCHCK" (#18, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#19, 0%, 0%, 500%, " ")
            if audit% <> 0% then                                         ~
            call "OPENCHCK" (#20, 0%, 0%, 500%, " ")

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            ret% = 99%
            if swtchs% < 1% then exit_program
            fn$ = f$
            ret% = 97%
            if fn$ = "JE" then end_journal
            if fn$ = "JW" then write_journal
            if fn$ <> "JS" then exit_program
               gosub start_journal
               if ret% <> 0% then exit_program
               glinfo$ = glhdr$
               goto exit_program

        REM *************************************************************~
            * S T A R T  J O U R N A L                                  *~
            *-----------------------------------------------------------*~
            * Set Up GLHDR$ After a few Checks                          *~
            *************************************************************

        start_journal:

            if progid$ <> " " then journal_started
            ret% = 98%
            call "EXTRACT" addr("ID", userid$, "CF", progid$)

            plowkey$ = userid$
            call "PLOWNEXT" (#19, plowkey$, 11%, f1%)
               if f1% = 0% then L10330
            readkey$ = "Journal for program " & str(plowkey$,12,8) &     ~
                       " was not properly terminated"
L10170:     ask% = 0%
                call "ASKUSER" (ask%, "* * * WARNING * * *", readkey$,   ~
                                    "Check Journals Carefully!",         ~
                     "Press (RETURN) to continue or PF16 to EXIT Program")
            if ask% = 0% then L10250
            if ask% = 16% then exit_program
               goto L10170

L10250:        init (hex(00)) readkey$
               str(readkey$,,25) = str(userid$,,3) & str(plowkey$,12,22)
               put str(readkey$,30,4) using L10280, 1%
L10280:            FMT BI(4)
               call "DELETE" (#19, readkey$, 33%)
               init (hex(00)) str(plowkey$,12)
               call "DELETE" (#19, plowkey$, 11%)

L10330
*        Check if G/L date one of the months open...
            call "READ100" (#18, userid$, f1%)
            if f1% <> 0% then L10420
                call "ASKUSER" (0%, "* * * ERROR * * *",                 ~
                   "Unable to find User's Module Date.",                 ~
                   "Please correct problem and re-run program",          ~
                   "Press ENTER or any PF Key to Exit Program")
                goto exit_program

L10420
*        Is the month open?
            get #18, using L10440, gldate$
L10440:         FMT XX(21), CH(6)
            call "WHICHPER" (#01, gldate$, thismonth%)
            if thismonth% <> 0% then L10530
                call "ASKUSER" (0%, "* * * ERROR * * *",                 ~
                   "Your G/L Module Date is in a month that is not open",~
                   "Please correct problem and re-run program",          ~
                   "Press ENTER or any PF Key to Exit Program")
                goto exit_program

L10530:     glhdr$ = str(userid$,,3%) &                                  ~
                     str(progid$,,8%) &                                  ~
                     str(gldate$,,6%) & hex(00)

L10570:     call "GETDTTM" addr(str(glhdr$,19%,7%))
            init (hex(00)) plowkey$
            str(plowkey$,,22%) = str(glhdr$,4%,22%)
            call "PLOWALTS" (#19, plowkey$, 1%, 22%, f1%)
               if f1% <> 0% then L10570
            glseq1% = 0%
            gltranx$ = "X": init (hex(00)) str(gltranx$,2%,24%)
        /* Got Header all Set Up, Leave it alone */

        /* Write 'User In Journal' Record         */
            write #19 using L10710, userid$, " ", str(glhdr$,4%,22%),     ~
                                   " ", gltranx$, gltranx$,              ~
                                   " ", " ", " ", date, time, " "

L10710:         FMT CH(3), CH(8), CH(22), CH(2), 2*CH(90), 2*CH(25),     ~
                    CH(30), CH(6), CH(8), CH(41)

        /* Write 'Journal In Use' Record         */
            write #19 using L10790,  glhdr$,     0%,      1%,             ~
                                    " ", gltranx$, gltranx$,             ~
                                    " ", " ", " ", date, time, " "

L10790:         FMT CH(25), 2%*BI(4), CH(2), 2*CH(90), 2*CH(25),         ~
                    CH(30), CH(6), CH(8), CH(41)

        journal_started:

            ret% = 0%
            return

        REM *************************************************************~
            * W R I T E   J O U R N A L                                 *~
            *-----------------------------------------------------------*~
            * Actual Writes to CORJNLTF and CORAUDIT                    *~
            *************************************************************

        write_journal:

            if progid$ <> " " then L11150
               gosub start_journal    /* On the Fly? */
               if ret% <> 0% then exit_program

L11150:     if pglseq1% > 0% then glseq1% = pglseq1%
            glseq1% = glseq1% + glseqinc% : glseqinc% = 0% : ret% = 97%
            pglseq1% = glseq1%
            gltrn0$ = gltwas$ : if gltrn0$ = " " then gltrn0$ = gltranx$
            gltrn1$ = gltcur$ : if gltrn1$ = " " then gltrn1$ = gltranx$
            gldata$ = glinfo$

            if caudit% = 3% then L11420
            if str(gltrn1$,,90) = str(gltrn0$,,90) then L11420
            write #19 using L11340,  glhdr$, glseq1%, glseq2%,            ~
                                    glcde$, gltrn0$, gltrn1$,            ~
                                    str(gldata$,,80%),                   ~
                                    date, time, " "

L11340:         FMT CH(25), 2%*BI(4), CH(2), 2*CH(90), CH(80),           ~
                    CH(6), CH(8), CH(41)

L11420:     if audit%  = 0% then exit_write
            if caudit% = 0% then exit_write
            if caudit% = 1% then L11460
            if caudit% = 3% then L11460
               if caudit% <> 2% then exit_write
                  if str(gltrn1$,,90) = str(gltrn0$,,90) then exit_write

L11460:     write #20 using L11580,  glhdr$, glseq1%, glseq2%,            ~
                                    glcde$, gltrn0$, gltrn1$,            ~
                                    str(gldata$,,80%),                   ~
                                    date, time, str(gldata$,95%)


L11580:         FMT CH(25), 2%*BI(4), CH(2), 2*CH(90), CH(80),           ~
                    CH(6), CH(8), CH(41)

        exit_write:
            ret% = 0%
            goto exit_program

        REM *************************************************************~
            * J O U R N A L   E N D                                     *~
            *-----------------------------------------------------------*~
            * Just Logically Close Journal and Bye.                     *~
            *************************************************************

        end_journal:
            ret% = 0%
            if progid$ = " " then exit_program
        /* Delete 'Journal In Use' Record         */
            init (hex(00)) plowkey$
            str(plowkey$,,25%) = str(glhdr$,,25%)
            call "PLOWNXT1" (#19, plowkey$, 29%, f1%)
               if f1% <> 0% then delete #19
            plowkey$ = userid$
            call "DELETE" (#19, plowkey$, 11%)

            progid$ = " "   /* Just in Case We Re-enter */
            goto exit_program

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program

            end
