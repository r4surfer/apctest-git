        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR   PPPP   TTTTT  PPPP    AAA   RRRR    AAA   M   M   *~
            *  R   R  P   P    T    P   P  A   A  R   R  A   A  MM MM   *~
            *  RRRR   PPPP     T    PPPP   AAAAA  RRRR   AAAAA  M M M   *~
            *  R  R   P        T    P      A   A  R  R   A   A  M   M   *~
            *  R   R  P        T    P      A   A  R   R  A   A  M   A   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RPTPARAM - Allows management of report specific           *~
            *            parameters.                                    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/14/84 ! ORIGINAL                                 ! ERN *~
            * 01/14/86 ! Change to validate SPOOLSYS (no GIGO).   ! LDJ *~
            * 10/20/93 ! Allow Print Mode 'P' (Unix Only)         ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            ask$(15)1,                   /* Prompt User? (Y/N)         */~
            blank$79,                    /* Blank Line                 */~
            cfac$(15)1,                  /* FACs FOR Report IDs (code) */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$(15)30,                /* Report Title               */~
            dfac$(15)1,                  /* Code description Facs      */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            eof$3,                       /* END-OF-FILE FLAG           */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fileclass$(15)1,             /* Default File Protection    */~
            form$(15)3,                  /* Form Number                */~
            hdr0$ 6, hdr1$25, hdr2$ 3,   /* COLUMN HEADINGS            */~
            hdr3$ 4, hdr4$ 3, hdr5$ 2,   /*                            */~
            hdr6$ 3, hdr7$ 2, hdr8$ 8,   /*                            */~
            hdr9$ 6, hdra$ 8,            /*                            */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            lfac$(15,15)1,               /* Parameter FACs             */~
            mode$(15)1,                  /* Print Mode                 */~
            pfdescr$(2,2)79,             /* PF KEY DESCRIPTIONS        */~
            pfkeys$(2)17,                /* PF KEYS (HEX)              */~
            plowkey$50,                  /* PLOW KEY                   */~
            prtclass$(15)1,              /* Print File Class           */~
            ptr$(15)3,                   /* Printer                    */~
            rptid$(15)6,                 /* Report ID                  */~
            spllib$(15)8,                /* Spool Library              */~
            splsys$(15)8,                /* Spool System               */~
            splvol$(15)6,                /* Spool Volume               */~
            spoolsys$8,                  /* Spool System               */~
            version$6                    /* System Version             */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! REPORTS  ! System Report Parameter File             *~
            * #2  ! INTDOC01 ! System Element File                      *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "REPORTS",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 80,                                    ~
                        keypos =    1,  keylen = 6

            select #2,  "INTDOC01",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1000,                                  ~
                        keypos =   25, keylen =  16,                     ~
                        alt key  1, keypos =   17, keylen =  24,         ~
                            key  2, keypos =    1, keylen =  40,         ~
                            key  3, keypos =   41, keylen =  50, dup

        call "SHOSTAT" ("Opening Files, One Moment Please.")
            call "OPENCHCK" (#1,  0%,      f2%(1), 100%, " ")
            call "OPENOLIB" (#2,  "SHARE", f2%(2), rslt$(2), axd$(2))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            call "EXTSPSYS" addr(spoolsys$)
            unix% = -1
            call "EXTRACT" addr("S#", version$)
            convert version$ to unix%, data goto L09060

L09060:     date$  = date  :  call "DATEFMT" (date$)
            blank$ = "Date: " & date$
            str(blank$,62) = "Revision: " & cms2v$

            pfdescr$(1,1) = "(ENTER)EDIT Line    (5)NEXT Screen      " & ~
                            "              (10)MOVE Screen to Entry "
            pfdescr$(2,1) = "(2)FIRST Screen    (13)Instructions  (15" & ~
                            ")Print Screen (31)Print Report (16)EXIT"

            pfdescr$(1,2) = "(ENTER)SAVE DATA                  (8)DEL" & ~
                            "ETE                                    "
            pfdescr$(2,2) = "(1)Start Line Over               (13)Ins" & ~
                            "tructions   (15)Print Screen           "

            pfkeys$(1) = hex(ff02ffff05ffffffff0aff1f0d0e0f1000)
            pfkeys$(2) = hex(01ffffffffffff08ffffffff0dff0fff00)

            gosub load_first

            hdr0$ = "Rpt ID"
            hdr1$ = "     Report Title"
            hdr2$ = "Ask"
            hdr3$ = "Form"
            hdr4$ = "Mde"
            hdr5$ = "PC"
            hdr6$ = "Ptr"
            hdr7$ = "FC"
            hdr8$ = "SpoolLib"
            hdr9$ = "SplVol"
            hdra$ = "SpoolSys"

        REM *************************************************************~
            *        M A I N   P R O G R A M   L O G I C                *~
            *-----------------------------------------------------------*~
            *  Screen controls and branching.                           *~
            *************************************************************

        screen_loop
            errormsg$ = " "
L10070:     gosub'101(0%)      /* Get what is to be done               */
                if keyhit%  =  2 then gosub load_first
                if keyhit%  =  5 then gosub load_next
                if keyhit%  = 10 then gosub load_stated
                if keyhit%  = 16 then       L65000
                if keyhit%  = 31 then       report_printing
                if keyhit% <>  0 then       L10070
            errormsg$ = " "
            l% = cursor%(1) - 5
            if l% <> 1 then goto L10250
            /* Add -or- Change a specifically mentioned report.        */
                plowkey$ = "IOREPORT" & rptid$(1)
                call "PLOWCODE" (#2, plowkey$, descr$(1), 8%, 1.5, f1%(2))
                if f1%(2) = 1% then rptid$(1) = str(plowkey$,9)
                if f1%(2) = 1% then L10210
                    errormsg$ = "Report ID not found in SES."
                    goto L10070
L10210:         gosub load_specific
L10250:     if l% < 1 or l% > last% then goto L10070

L10280:     gosub'101(l%)      /* Get field entries.                   */
                if keyhit%  =  1 then gosub startover
                if keyhit%  =  8 then gosub delete_code
                if keyhit% <>  0 then goto  L10280
            gosub'151(l%)      /* Edit fields                          */
                if errormsg$ <> " "  then goto L10280
                gosub save_data
                goto screen_loop

        REM *************************************************************~
            *             S T A R T   O V E R                           *~
            * --------------------------------------------------------- *~
            *  Abort line modfification. Gives operator a second chance.*~
            *  Note that if code is displayed on screen we must reload  *~
            *  so that it's values revert to what they were before they *~
            *  we modified.                                             *~
            *************************************************************
        startover
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "** START OVER **",                ~
                           "PRESS (1) to RETURN to Display",             ~
                           "PRESS (ENTER) to ABORT changes made to LINE",~
                           " ")
                if keyhit1% = 0% then L29190
                if keyhit1% = 1% then L29220
                goto startover

L29190: /* ENTER */  return clear
                     if l% <>  1 then goto L29202
                          gosub clear_first
                          goto screen_loop
L29202:              rptid$(1) = rptid$(2)
                     gosub load_stated
                     goto screen_loop

L29220: /* PF 1  */  return


        REM *************************************************************~
            *           L O A D   D A T A   R O U T I N E S             *~
            *-----------------------------------------------------------*~
            *   Move a screen's worth of data into display arrays.      *~
            *************************************************************

        load_first        /* Load from top of file                     */
            plowkey$ = hex(00)
            goto load_data

        load_stated       /* Load from operator specified code         */
            if len(rptid$(1)) = 6 then                                   ~
             plowkey$ = str(rptid$(1),,5) &bin(val(str(rptid$(1),6,1))-1)~
                  else  plowkey$ = rptid$(1) & hex(00)
            gosub clear_first
            goto load_data

        load_next         /* Load from last code listed                */
            if eof$ <> "YES" then goto L30180
                errormsg$ = "ALREADY AT END OF FILE."
                return
L30180:     if last% = 1 then goto load_first
            plowkey$ = rptid$(last%) & hex(00)

        load_data
            errormsg$ = " "
            last% = 1
            eof$  = "NO"
            init (" ") rptid$(), descr$(), ask$(), form$(), mode$(),     ~
                       prtclass$(), ptr$(), fileclass$(), spllib$(),     ~
                       splvol$(), splsys$()

          plowloop
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 1 then goto L30300
                eof$ = "YES"
                return
L30300:     last% = last% + 1  :  l% = last%
            gosub get_record   /* Load record and format for display   */
          if l% < 15 then goto plowloop else return


        load_specific     /* Get specific code from file               */
                          /* If new, supply defaults.                  */
            call "READ100" (#1, str(rptid$(l%)), f1%(1))
            if f1%(1) <> 1 then goto L30610
                gosub get_record
                return
L30610:   /* Not on file-- setup with defaults               */
            ask$(l%) = "N"
         return


        get_record   /* Get record from buffer and format for display  */
            get #1 using L30850, rptid$(l%), ask$(l%), form$(l%),         ~
                                mode$(l%), prtclass$(l%), ptr$(l%),      ~
                                fileclass$(l%), spllib$(l%), splvol$(l%),~
                                splsys$(l%)
            descr$(l%) = "* NOT ON FILE *"
            call "READ100" (#2, rptid$(l%), f1%(2))
            if f1%(2) = 1% then get #2 using L30750, descr$(l%)
L30750:         FMT XX(40), CH(30)
        return


L30850:         FMT  CH( 6),             /* Report ID                  */~
                     CH( 1),             /* Ask User?                  */~
                     CH( 3),             /* Form Number                */~
                     CH( 1),             /* Print Mode                 */~
                     CH( 1),             /* Print Class                */~
                     CH( 3),             /* Printer Number             */~
                     CH( 1),             /* Print File Class           */~
                     CH( 8),             /* Spool Library              */~
                     CH( 6),             /* Spool Volume               */~
                     CH( 8),             /* Spool System               */~
                     XX(42)              /* Filler                     */

        REM *************************************************************~
            *           S A V E   D A T A   O N    F I L E              *~
            *-----------------------------------------------------------*~
            *  Update file with code maintained (L%). After save, if    *~
            *  Code was on line 1 we redo screen with that code on top. *~
            *************************************************************
        save_data
            call "READ101" (#1, str(rptid$(l%)), f1%(1))
            put #1 using L31600, rptid$(l%), ask$(l%), form$(l%),         ~
                                mode$(l%), prtclass$(l%), ptr$(l%),      ~
                                fileclass$(l%), spllib$(l%), splvol$(l%),~
                                splsys$(l%), " "
            if f1%(1) = 0 then write #1 else rewrite #1

         /* Now make sure that code added/changed appears on screen.   */
         /* Also accessed by DELETE_CODE routine.                      */
         screen_align
            if l%   <> 1 then goto L31180
            if rptid$(l%) > rptid$(last%) and last% < 15 then L31180
            if rptid$(l%) < rptid$(2) or                                 ~
               rptid$(l%) > rptid$(last%) then goto L31190

L31180:     rptid$(1) = rptid$(2)        /* Keeps the screen the same  */
                goto L31200
L31190:     rptid$(1) = rptid$(l%)       /* Put modified code on top   */
L31200:     gosub load_stated            /* Start display at RPTID$(1) */
          return

L31600:         FMT  CH( 6),             /* Report ID                  */~
                     CH( 1),             /* Ask User?                  */~
                     CH( 3),             /* Form Number                */~
                     CH( 1),             /* Print Mode                 */~
                     CH( 1),             /* Printer Class              */~
                     CH( 3),             /* Printer Number             */~
                     CH( 1),             /* Print File Class           */~
                     CH( 8),             /* Spool Library              */~
                     CH( 6),             /* Spool Volume               */~
                     CH( 8),             /* Spool System               */~
                     CH(42)              /* Filler                     */

        REM *************************************************************~
            *             D E L E T E   C O D E                         *~
            *-----------------------------------------------------------*~
            *  Delete code from file after getting confirmation.        *~
            *************************************************************
        delete_code

L32070:     accept at (02,02), "REPORT PARAMETERS MANAGEMENT",           ~
                   at (04,02), "   **** D E L E T E ****",               ~
                   at (06,02), "  Report ID",                            ~
                   at (06,15), fac(hex(84)), rptid$(l%),                 ~
                   at (07,02), "      Title",                            ~
                   at (07,15), fac(hex(84)), descr$(l%),                 ~
                   at (10,02), "(ENTER) DELETE",                         ~
                   at (12,02), "(PF- 1) RETURN (Do Not Delete)",         ~
                                                                         ~
                   keys(hex(00010f)),                                    ~
                     on hex(00010f)   goto L32240, L32190, L32210

L32190: /*PF01 */  return

L32210: /*PF15 */  call "PRNTSCRN"
                   goto L32070

L32240: /*ENTER*/  call "DELETE" (#1, rptid$(l%), 6%)
                   rptid$(1) = rptid$(l%)
                   gosub screen_align
                   return clear
                   goto screen_loop


        REM *************************************************************~
            *        R E P O R T   P R I N T I N G                      *~
            *-----------------------------------------------------------*~
            *  Print report for either ALL logical files -or- just one. *~
            *************************************************************
        report_printing
            call "SETPRNT" ("SYS001", " ", 0%, 0%)
            call "SHOSTAT" ("Printing Report Parameters List")
            page% = 0%  :  line% = 857%
            select printer(134)
            plowkey$ = hex(00)

        report_loop
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 1% then goto L33210
                if line% > 52% then gosub report_heading
                print " "
                print "    END OF REPORT"
                close printer  :  select ws
                call "SETPRNT" ("SYS001", " ", 0%, 1%)
                gosub load_first
                goto screen_loop

L33210:     if line% > 52% then gosub report_heading
            get #1 using L33260, rptid$(1), ask$(1), form$(1),            ~
                                mode$(1), prtclass$(1), ptr$(1),         ~
                                fileclass$(1), spllib$(1), splvol$(1),   ~
                                splsys$(1)
L33260:         FMT  CH(6), CH(1), CH(3), CH(1), CH(1), CH(3), CH(1),    ~
                     CH(8), CH(6), CH(8)
            descr$(1) = "* NOT ON FILE *"
            call "READ100" (#2, rptid$(1), f1%(2))
            if f1%(2) = 0% then L33340
                get #2 using L33320, descr$(1)
L33320:              FMT XX(40), CH(30)

L33340:     print using L33500, rptid$(1), descr$(1), ask$(1), form$(1),  ~
                               mode$(1), prtclass$(1), ptr$(1),          ~
                               fileclass$(1), spllib$(1), splvol$(1),    ~
                               splsys$(1)
            line% = line% + 1%
            goto report_loop


L33420: %########                            REPORT PARAMETERS LISTING   ~
        ~               SYS-001         PAGE: ###
L33440: %REPORT                                            PRINT  PRINT  ~
        ~PRNTR   FILE    SPOOL    SPOOL   SPOOL
L33460: %  ID            REPORT TITLE           ASK  FORM   MODE  CLASS  ~
        ~NUMBR  CLASS   LIBRARY  VOLUME   SYSTEM
L33480: %------ ------------------------------  ---  ----  -----  -----  ~
        ~-----  -----  --------  ------  --------
L33500: %###### ##############################   #    ###    #      #    ~
        ~ ###     #    ########  ######  ########


        report_heading
            line% = 5%
            page% = page% + 1%
            print page
            print using L33420, date$, page%
            print
            print using L33440
            print using L33460
            print using L33480
          return


        REM *************************************************************~
            *        D A T A   E N T R Y   S C R E E N                  *~
            *-----------------------------------------------------------*~
            *  Combination Display and Manage Screen.  I will not be so *~
            *  presumptious to call it 'super slick' or such -- only    *~
            *  time will reveal its worth.                              *~
            *                                                           *~
            *  There are essentially 2 entry points --                  *~
            *     line =  0 - Get from operator what to do. The code    *~
            *                 field on the 'wild line' is the only      *~
            *                 modifiable field.                         *~
            *     line <> 0 - Get entries for the line.                 *~
            *************************************************************

        deffn'101(l%)
            if l% <> 0 then goto L40260

          /* Case 1. Get what is to be done.                           */
            init (hex(84)) cfac$()  :  cfac$(1) = hex(81) /* RptID Fac */
            init (hex(8c)) dfac$()                        /* Title Fac */
            init (hex(8c)) lfac$()                        /* Line  Fac */
            mode% = 1
            edtmessage$ = "Locate cursor and then press appropriate" &   ~
                          " PF key."
            goto L40440

L40260:   /* Case 2. Modify line L%.                                   */
            if errormsg$ = " " then L40300
                edtmessage$ = "Please correct error as indicated."
                goto L40420
L40300:     init (hex(8c)) cfac$() : cfac$(l%) = hex(a4)
                dfac$(l%)    = hex(84)             /* Report Title     */
                lfac$(l%, 1) = hex(81)             /* Prompt?          */
                lfac$(l%, 2) = hex(82)             /* Form Number      */
                lfac$(l%, 3) = hex(81)             /* Print Mode       */
                lfac$(l%, 4) = hex(81)             /* Print Class      */
                lfac$(l%, 5) = hex(82)             /* Printer Number   */
                lfac$(l%, 6) = hex(81)             /* File Class       */
                lfac$(l%, 7) = hex(81)             /* Spool Library    */
                lfac$(l%, 8) = hex(81)             /* Spool Volume     */
                lfac$(l%, 9) = hex(81)             /* Spool System     */
            edtmessage$ = "Modify field(s) then press (ENTER) to save."
L40420:     mode% = 2%

L40440: accept                                                           ~
          at(01,02), "MANAGE REPORT PARAMETERS FILE",                    ~
          at(02,02), fac(hex(ac)), blank$,                               ~
          at(03,02), fac(hex(94)), errormsg$,                            ~
                                                                         ~
          at(05,02), fac(hex(ac)), hdr0$,                                ~
          at(05,09), fac(hex(ac)), hdr1$,                                ~
          at(05,34), fac(hex(ac)), hdr2$,                                ~
          at(05,38), fac(hex(ac)), hdr3$,                                ~
          at(05,43), fac(hex(ac)), hdr4$,                                ~
          at(05,47), fac(hex(ac)), hdr5$,                                ~
          at(05,50), fac(hex(ac)), hdr6$,                                ~
          at(05,54), fac(hex(ac)), hdr7$,                                ~
          at(05,57), fac(hex(ac)), hdr8$,                                ~
          at(05,66), fac(hex(ac)), hdr9$,                                ~
          at(05,73), fac(hex(ac)), hdra$,                                ~
                                                                         ~
          at(06,02), fac(cfac$( 1)),    rptid$( 1),                      ~
          at(07,02), fac(cfac$( 2)),    rptid$( 2),                      ~
          at(08,02), fac(cfac$( 3)),    rptid$( 3),                      ~
          at(09,02), fac(cfac$( 4)),    rptid$( 4),                      ~
          at(10,02), fac(cfac$( 5)),    rptid$( 5),                      ~
          at(11,02), fac(cfac$( 6)),    rptid$( 6),                      ~
          at(12,02), fac(cfac$( 7)),    rptid$( 7),                      ~
          at(13,02), fac(cfac$( 8)),    rptid$( 8),                      ~
          at(14,02), fac(cfac$( 9)),    rptid$( 9),                      ~
          at(15,02), fac(cfac$(10)),    rptid$(10),                      ~
          at(16,02), fac(cfac$(11)),    rptid$(11),                      ~
          at(17,02), fac(cfac$(12)),    rptid$(12),                      ~
          at(18,02), fac(cfac$(13)),    rptid$(13),                      ~
          at(19,02), fac(cfac$(14)),    rptid$(14),                      ~
          at(20,02), fac(cfac$(15)),    rptid$(15),                      ~
                                                                         ~
          at(06,09), fac(dfac$( 1)),             descr$( 1)     , ch(25),~
          at(07,09), fac(dfac$( 2)),             descr$( 2)     , ch(25),~
          at(08,09), fac(dfac$( 3)),             descr$( 3)     , ch(25),~
          at(09,09), fac(dfac$( 4)),             descr$( 4)     , ch(25),~
          at(10,09), fac(dfac$( 5)),             descr$( 5)     , ch(25),~
          at(11,09), fac(dfac$( 6)),             descr$( 6)     , ch(25),~
          at(12,09), fac(dfac$( 7)),             descr$( 7)     , ch(25),~
          at(13,09), fac(dfac$( 8)),             descr$( 8)     , ch(25),~
          at(14,09), fac(dfac$( 9)),             descr$( 9)     , ch(25),~
          at(15,09), fac(dfac$(10)),             descr$(10)     , ch(25),~
          at(16,09), fac(dfac$(11)),             descr$(11)     , ch(25),~
          at(17,09), fac(dfac$(12)),             descr$(12)     , ch(25),~
          at(18,09), fac(dfac$(13)),             descr$(13)     , ch(25),~
          at(19,09), fac(dfac$(14)),             descr$(14)     , ch(25),~
          at(20,09), fac(dfac$(15)),             descr$(15)     , ch(25),~
                                                                         ~
          at(06,35), fac(lfac$( 1, 1)),          ask$( 1),               ~
          at(07,35), fac(lfac$( 2, 1)),          ask$( 2),               ~
          at(08,35), fac(lfac$( 3, 1)),          ask$( 3),               ~
          at(09,35), fac(lfac$( 4, 1)),          ask$( 4),               ~
          at(10,35), fac(lfac$( 5, 1)),          ask$( 5),               ~
          at(11,35), fac(lfac$( 6, 1)),          ask$( 6),               ~
          at(12,35), fac(lfac$( 7, 1)),          ask$( 7),               ~
          at(13,35), fac(lfac$( 8, 1)),          ask$( 8),               ~
          at(14,35), fac(lfac$( 9, 1)),          ask$( 9),               ~
          at(15,35), fac(lfac$(10, 1)),          ask$(10),               ~
          at(16,35), fac(lfac$(11, 1)),          ask$(11),               ~
          at(17,35), fac(lfac$(12, 1)),          ask$(12),               ~
          at(18,35), fac(lfac$(13, 1)),          ask$(13),               ~
          at(19,35), fac(lfac$(14, 1)),          ask$(14),               ~
          at(20,35), fac(lfac$(15, 1)),          ask$(15),               ~
                                                                         ~
          at(06,38), fac(lfac$( 1, 2)),         form$( 1),               ~
          at(07,38), fac(lfac$( 2, 2)),         form$( 2),               ~
          at(08,38), fac(lfac$( 3, 2)),         form$( 3),               ~
          at(09,38), fac(lfac$( 4, 2)),         form$( 4),               ~
          at(10,38), fac(lfac$( 5, 2)),         form$( 5),               ~
          at(11,38), fac(lfac$( 6, 2)),         form$( 6),               ~
          at(12,38), fac(lfac$( 7, 2)),         form$( 7),               ~
          at(13,38), fac(lfac$( 8, 2)),         form$( 8),               ~
          at(14,38), fac(lfac$( 9, 2)),         form$( 9),               ~
          at(15,38), fac(lfac$(10, 2)),         form$(10),               ~
          at(16,38), fac(lfac$(11, 2)),         form$(11),               ~
          at(17,38), fac(lfac$(12, 2)),         form$(12),               ~
          at(18,38), fac(lfac$(13, 2)),         form$(13),               ~
          at(19,38), fac(lfac$(14, 2)),         form$(14),               ~
          at(20,38), fac(lfac$(15, 2)),         form$(15),               ~
                                                                         ~
          at(06,44), fac(lfac$( 1, 3)),         mode$( 1),               ~
          at(07,44), fac(lfac$( 2, 3)),         mode$( 2),               ~
          at(08,44), fac(lfac$( 3, 3)),         mode$( 3),               ~
          at(09,44), fac(lfac$( 4, 3)),         mode$( 4),               ~
          at(10,44), fac(lfac$( 5, 3)),         mode$( 5),               ~
          at(11,44), fac(lfac$( 6, 3)),         mode$( 6),               ~
          at(12,44), fac(lfac$( 7, 3)),         mode$( 7),               ~
          at(13,44), fac(lfac$( 8, 3)),         mode$( 8),               ~
          at(14,44), fac(lfac$( 9, 3)),         mode$( 9),               ~
          at(15,44), fac(lfac$(10, 3)),         mode$(10),               ~
          at(16,44), fac(lfac$(11, 3)),         mode$(11),               ~
          at(17,44), fac(lfac$(12, 3)),         mode$(12),               ~
          at(18,44), fac(lfac$(13, 3)),         mode$(13),               ~
          at(19,44), fac(lfac$(14, 3)),         mode$(14),               ~
          at(20,44), fac(lfac$(15, 3)),         mode$(15),               ~
                                                                         ~
          at(06,47), fac(lfac$( 1, 4)),     prtclass$( 1),               ~
          at(07,47), fac(lfac$( 2, 4)),     prtclass$( 2),               ~
          at(08,47), fac(lfac$( 3, 4)),     prtclass$( 3),               ~
          at(09,47), fac(lfac$( 4, 4)),     prtclass$( 4),               ~
          at(10,47), fac(lfac$( 5, 4)),     prtclass$( 5),               ~
          at(11,47), fac(lfac$( 6, 4)),     prtclass$( 6),               ~
          at(12,47), fac(lfac$( 7, 4)),     prtclass$( 7),               ~
          at(13,47), fac(lfac$( 8, 4)),     prtclass$( 8),               ~
          at(14,47), fac(lfac$( 9, 4)),     prtclass$( 9),               ~
          at(15,47), fac(lfac$(10, 4)),     prtclass$(10),               ~
          at(16,47), fac(lfac$(11, 4)),     prtclass$(11),               ~
          at(17,47), fac(lfac$(12, 4)),     prtclass$(12),               ~
          at(18,47), fac(lfac$(13, 4)),     prtclass$(13),               ~
          at(19,47), fac(lfac$(14, 4)),     prtclass$(14),               ~
          at(20,47), fac(lfac$(15, 4)),     prtclass$(15),               ~
                                                                         ~
          at(06,50), fac(lfac$( 1, 5)),          ptr$( 1),               ~
          at(07,50), fac(lfac$( 2, 5)),          ptr$( 2),               ~
          at(08,50), fac(lfac$( 3, 5)),          ptr$( 3),               ~
          at(09,50), fac(lfac$( 4, 5)),          ptr$( 4),               ~
          at(10,50), fac(lfac$( 5, 5)),          ptr$( 5),               ~
          at(11,50), fac(lfac$( 6, 5)),          ptr$( 6),               ~
          at(12,50), fac(lfac$( 7, 5)),          ptr$( 7),               ~
          at(13,50), fac(lfac$( 8, 5)),          ptr$( 8),               ~
          at(14,50), fac(lfac$( 9, 5)),          ptr$( 9),               ~
          at(15,50), fac(lfac$(10, 5)),          ptr$(10),               ~
          at(16,50), fac(lfac$(11, 5)),          ptr$(11),               ~
          at(17,50), fac(lfac$(12, 5)),          ptr$(12),               ~
          at(18,50), fac(lfac$(13, 5)),          ptr$(13),               ~
          at(19,50), fac(lfac$(14, 5)),          ptr$(14),               ~
          at(20,50), fac(lfac$(15, 5)),          ptr$(15),               ~
                                                                         ~
          at(06,54), fac(lfac$( 1, 6)),    fileclass$( 1),               ~
          at(07,54), fac(lfac$( 2, 6)),    fileclass$( 2),               ~
          at(08,54), fac(lfac$( 3, 6)),    fileclass$( 3),               ~
          at(09,54), fac(lfac$( 4, 6)),    fileclass$( 4),               ~
          at(10,54), fac(lfac$( 5, 6)),    fileclass$( 5),               ~
          at(11,54), fac(lfac$( 6, 6)),    fileclass$( 6),               ~
          at(12,54), fac(lfac$( 7, 6)),    fileclass$( 7),               ~
          at(13,54), fac(lfac$( 8, 6)),    fileclass$( 8),               ~
          at(14,54), fac(lfac$( 9, 6)),    fileclass$( 9),               ~
          at(15,54), fac(lfac$(10, 6)),    fileclass$(10),               ~
          at(16,54), fac(lfac$(11, 6)),    fileclass$(11),               ~
          at(17,54), fac(lfac$(12, 6)),    fileclass$(12),               ~
          at(18,54), fac(lfac$(13, 6)),    fileclass$(13),               ~
          at(19,54), fac(lfac$(14, 6)),    fileclass$(14),               ~
          at(20,54), fac(lfac$(15, 6)),    fileclass$(15),               ~
                                                                         ~
          at(06,57), fac(lfac$( 1, 7)),       spllib$( 1),               ~
          at(07,57), fac(lfac$( 2, 7)),       spllib$( 2),               ~
          at(08,57), fac(lfac$( 3, 7)),       spllib$( 3),               ~
          at(09,57), fac(lfac$( 4, 7)),       spllib$( 4),               ~
          at(10,57), fac(lfac$( 5, 7)),       spllib$( 5),               ~
          at(11,57), fac(lfac$( 6, 7)),       spllib$( 6),               ~
          at(12,57), fac(lfac$( 7, 7)),       spllib$( 7),               ~
          at(13,57), fac(lfac$( 8, 7)),       spllib$( 8),               ~
          at(14,57), fac(lfac$( 9, 7)),       spllib$( 9),               ~
          at(15,57), fac(lfac$(10, 7)),       spllib$(10),               ~
          at(16,57), fac(lfac$(11, 7)),       spllib$(11),               ~
          at(17,57), fac(lfac$(12, 7)),       spllib$(12),               ~
          at(18,57), fac(lfac$(13, 7)),       spllib$(13),               ~
          at(19,57), fac(lfac$(14, 7)),       spllib$(14),               ~
          at(20,57), fac(lfac$(15, 7)),       spllib$(15),               ~
                                                                         ~
          at(06,66), fac(lfac$( 1, 8)),       splvol$( 1),               ~
          at(07,66), fac(lfac$( 2, 8)),       splvol$( 2),               ~
          at(08,66), fac(lfac$( 3, 8)),       splvol$( 3),               ~
          at(09,66), fac(lfac$( 4, 8)),       splvol$( 4),               ~
          at(10,66), fac(lfac$( 5, 8)),       splvol$( 5),               ~
          at(11,66), fac(lfac$( 6, 8)),       splvol$( 6),               ~
          at(12,66), fac(lfac$( 7, 8)),       splvol$( 7),               ~
          at(13,66), fac(lfac$( 8, 8)),       splvol$( 8),               ~
          at(14,66), fac(lfac$( 9, 8)),       splvol$( 9),               ~
          at(15,66), fac(lfac$(10, 8)),       splvol$(10),               ~
          at(16,66), fac(lfac$(11, 8)),       splvol$(11),               ~
          at(17,66), fac(lfac$(12, 8)),       splvol$(12),               ~
          at(18,66), fac(lfac$(13, 8)),       splvol$(13),               ~
          at(19,66), fac(lfac$(14, 8)),       splvol$(14),               ~
          at(20,66), fac(lfac$(15, 8)),       splvol$(15),               ~
                                                                         ~
          at(06,73), fac(lfac$( 1, 9)),       splsys$( 1),               ~
          at(07,73), fac(lfac$( 2, 9)),       splsys$( 2),               ~
          at(08,73), fac(lfac$( 3, 9)),       splsys$( 3),               ~
          at(09,73), fac(lfac$( 4, 9)),       splsys$( 4),               ~
          at(10,73), fac(lfac$( 5, 9)),       splsys$( 5),               ~
          at(11,73), fac(lfac$( 6, 9)),       splsys$( 6),               ~
          at(12,73), fac(lfac$( 7, 9)),       splsys$( 7),               ~
          at(13,73), fac(lfac$( 8, 9)),       splsys$( 8),               ~
          at(14,73), fac(lfac$( 9, 9)),       splsys$( 9),               ~
          at(15,73), fac(lfac$(10, 9)),       splsys$(10),               ~
          at(16,73), fac(lfac$(11, 9)),       splsys$(11),               ~
          at(17,73), fac(lfac$(12, 9)),       splsys$(12),               ~
          at(18,73), fac(lfac$(13, 9)),       splsys$(13),               ~
          at(19,73), fac(lfac$(14, 9)),       splsys$(14),               ~
          at(20,73), fac(lfac$(15, 9)),       splsys$(15),               ~
                                                                         ~
          at(21,02), fac(hex(a4)), edtmessage$,                          ~
          at(23,02), fac(hex(8c)), pfdescr$(1,mode%),                    ~
          at(24,02), fac(hex(8c)), pfdescr$(2,mode%),                    ~
                                                                         ~
                keys(str(pfkeys$(mode%))),                               ~
                key (keyhit%)

               if keyhit% <> 13 then L42480
                  call "MANUAL" ("FSRSNINP")
                  goto L40440

L42480:        if keyhit% <> 15 then L42520
                  call "PRNTSCRN"
                  goto L40440

L42520:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                                        u3% = u3%

            return

        REM *************************************************************~
            *           E D I T   D A T A                               *~
            *-----------------------------------------------------------*~
            *  Edit data fields for line entered. If in error, set      *~
            *  message and FACs, else just return.                      *~
            *************************************************************
        deffn'151(l%)
            init (hex(8c)) lfac$()
            errormsg$ = " "

*        Test data for PROMPT USER.
            if ask$(l%) = "Y" or ask$(l%) = "N" then L50160
                errormsg$ = "ASK (Prompt User) must be 'Y' or 'N'."
                lfac$(l%, 1) = hex(81)
                return

L50160
*        Test data for FORM NUMBER
            if form$(l%) = " " then L50250
                convert form$(l%) to t%, data goto L50200 : goto L50190
L50190:         if t% >= 0% and t% <= 254% then L50230
L50200:              errormsg$ = "Form Number must be blank or 0-254."
                     lfac$(l%, 2) = hex(82)
                     return
L50230:         convert t% to form$(l%), pic(000)

L50250
*        Test data for PRINT MODE
            if unix% < 0% then L50292
            if pos(" SHKO" = mode$(l%)) > 0% then L50310
                errormsg$ = "Print Mode must be ' ', 'S', 'H', 'K' or 'O'"
                lfac$(l%, 3) = hex(81)
                return
L50292:     if pos(" SHKOP" = mode$(l%)) > 0% then L50310
                errormsg$ = "Print Mode must be"
                errormsg$ = errormsg$ & " ' ', 'S', 'H', 'K', 'O' or 'P'"
                lfac$(l%, 3) = hex(81)
                return

L50310
*        Test data for PRINT CLASS
            if pos(" ABCDEFGHIJKLMNOPQRSTUVWXYZ" = prtclass$(l%)) > 0%   ~
                                                               then L50380
                errormsg$ = "Printer Class must be 'A'-'Z' or blank."
                lfac$(l%, 4) = hex(81)
                return

L50380
*        Test data for PRINTER DEVICE
            if ptr$(l%) = " " then L50470
                convert ptr$(l%) to t%, data goto L50420 : goto L50410
L50410:         if t% >= 0% and t% <= 512% then L50450
L50420:              errormsg$ = "Printer must be blank or 0-512."
                     lfac$(l%, 5) = hex(82)
                     return
L50450:         convert t% to ptr$(l%), pic(000)

L50470
*        Test data for FILE PROTECT CLASS
            if pos(" #@$ABCDEFGHIJKLMNOPQRSTUVWXYZ" = fileclass$(l%)) > 0~
                                                               then L50540
                errormsg$ = "Printer Class must be 'A'-'Z', '#', '@'," & ~
                            " '$' or blank."
                lfac$(l%, 6) = hex(81)
                return

L50540
*        Test data for SPOOL LIBRARY
            gosub'159(spllib$(l%))
            if errormsg$ = " " then L50600
                lfac$(l%, 7) = hex(81)
                return

L50600
*        Test data for SPOOL VOLUME
            gosub'159(splvol$(l%))
            if errormsg$ = " " then L50660
                lfac$(l%, 8) = hex(81)
                return

L50660
*        Test data for SPOOL SYSTEM
*          GOSUB'159(SPLSYS$(L%))
            if splsys$(l%) = " " then return
            call "SETSPSYS" addr(splsys$(l%), ret%)
            if ret% > 0% then errormsg$ = "SpoolSys Not defined in Networ~
        ~k Configuration File"
            call "SETSPSYS" addr(spoolsys$, ret%)
            if errormsg$ = " " then return
                lfac$(l%, 9) = hex(81)
                return


        REM *************************************************************~
            *        M I S C.  S U B - R O U T I N E S                  *~
            *************************************************************

        clear_first       /* Clear array bucket 1                      */
            rptid$(1), descr$(1), ask$(1), form$(1), mode$(1), ptr$(1),  ~
            prtclass$(1), fileclass$(1), spllib$(1), splvol$(1),         ~
            splsys$(1) = " "
        return


        deffn'159(test$)  /* Test Library and Volume names   */
            if test$ = " " then return
              for i% = 1% to len(test$)
                if pos("$@#0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"         ~
                                      = str(test$,i%,1)) = 0% then L55210
              next i%
            return

L55210:         errormsg$ = "Library or Volume Name contains invalid" &  ~
                            " characters: " & test$
                return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            end
