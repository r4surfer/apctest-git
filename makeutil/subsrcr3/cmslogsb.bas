        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   CCC   M   M  SSSS   L       OOO    GGG    SSS   BBBB    *~
            *  C      MM MM  S      L      O   O  G      S      B   B   *~
            *  C      M M M  SSSS   L      O   O  G GGG   SSS   BBBB    *~
            *  C      M   M      S  L      O   O  G   G      S  B   B   *~
            *   CCC   M   M  SSSS   LLLLL   OOO    GGG    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CMSLOGSB - This routine is used to log program access,    *~
            *            Run-Time statistics, etc.  Calling program     *~
            *            must login to this routine before linking to   *~
            *            a program and logout via this routine after    *~
            *            returning from a link.                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/23/85 ! ORIGINAL                                 ! LDJ *~
            * 05/02/88 ! Fixed nasty if OUTLIB = blank            ! LDJ *~
            * 10/21/88 ! Modified to pass CMSLOG library from the ! LDJ *~
            *          ! caller instead of assuming that current  !     *~
            *          ! OUTLIB is still the SES library.         !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "CMSLOGSB" (program$,        /* Link(ed) to Program        */~
                        library$,        /* Link(ed) to Program Library*/~
                        volume$,         /* Link(ed) to Program Volume */~
                        link_type$,      /* Link Type in LINK subroutin*/~
                        logincode$,      /* Logging IN or logging OUT  */~
                        comp%,           /* Link Completion Code       */~
                        return%,         /* Return Code from program or*/~
                                         /*   Link SVC.                */~
                        log_library$)    /* SES Library to Log Entries */

        dim                                                              ~
            first_time$1,                /* First time in routine flag */~
            inlib$8,                     /* User's INLIB or database   */~
            library$8,                   /* Link(ed) to Program Library*/~
            link_type$1,                 /* Link Type in LINK subroutin*/~
            logincode$3,                 /* Logging IN or logging OUT  */~
            log_library$8,               /* Log Activity in Library ...*/~
            program$8,                   /* Link(ed) to Program        */~
            read$1,                      /* File Read Flag             */~
            seslib$8,                    /* SES Library                */~
            sesvol$6,                    /* SES Volume                 */~
            userid$3,                    /* Current User ID            */~
            volume$6,                    /* Link(ed) to Program Volume */~
            vtoc$(1)22,                  /* FIND argument              */~
            write$1                      /* File Write Flag            */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************
            if first_time$ = "N" then L01271
            first_time$ = "N"

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CMSLOG   ! User Activity Log file                   *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "CMSLOG",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  256,                                  ~
                        keypos =   23, keylen =  17

        REM *************************************************************~
            *                  O P E N   F I L E S                      *~
            *-----------------------------------------------------------*~
            *   Save link space by not calling 'OPENOLIB' routine...    *~
            *   Note that OUTLIB must be set to users S.E.S. Lib name.  *~
            *   Note that OUTLIB is passed as LOG_LIBRARY$ by Caller.   *~
            *************************************************************~

            seslib$ = log_library$
            if seslib$ = " " then end
            sesvol$ = "?"
            call "FIND" addr("        ",seslib$,sesvol$,1%,1%,vtoc$())
            sesvol$ = str(vtoc$(1%),,6%)
            if sesvol$ = " " then end
            call "ACCESSCK" addr("CMSLOG  ", seslib$, sesvol$, read$,    ~
                                 write$, return%)
            if return% > 0% then L01180
            if write$ <> "Y" then sesvol$ = " "
            if write$ <> "Y" then end

L01110:         call "PUTNAMES" addr(#1, "CMSLOG  ", seslib$, sesvol$)
                call "WORKOPN2" (#1, "SHARE", 0%, f2%)
        REM     OPEN NOGETPARM #1,                                       ~
                     SHARED,                                             ~
                     FILE    = "CMSLOG  ",                               ~
                     LIBRARY = SESLIB$,                                  ~
                     VOLUME  = SESVOL$
                     goto L01271

L01180:         call "PUTNAMES" addr(#1, "CMSLOG  ", seslib$, sesvol$)
                call "WORKOPN2" (#1, "OUTSP", 2000%, f2%)
        REM     OPEN NOGETPARM #1,                                       ~
                     OUTPUT,                                             ~
                     FILE    = "CMSLOG  ",                               ~
                     LIBRARY = SESLIB$,                                  ~
                     VOLUME  = SESVOL$,                                  ~
                     SPACE   = 2000

                close #1
                goto L01110

L01271:     if seslib$ = " " or sesvol$ = " " then end
            if logincode$ = "OUT" then logout

        REM  LOGIN
            gosub extract_info
            put #1 using L02460 ,                                          ~
            " ",            /* Disk VOLUME name                        */~
            " ",            /* Library Identifier                      */~
            program$,       /* Program Name                            */~
            userid$,        /* user-id of specific user                */~
            date,           /* system (clock) date from the computer   */~
            time,           /* Time from the system clock              */~
            elapse_time%,   /* Elapsed time since logon or 'run' from c*/~
            diskio%,        /* Disk I/O Count (extracted from system)  */~
            otherio%,       /* Other I/O Count (TC, etc extracted from */~
            prog_pagein%,   /* Program Page-In Count                   */~
            prog_pageout%,  /* Program Page-Out Count                  */~
            cpu%,           /* CPU Time Consumed since "run" from Comma*/~
            printio%,       /* Printer I/O Count (extracted from system*/~
            sys_pagein%,    /* System Page-In Count                    */~
            sys_page_out%,  /* System Page-Outs Count                  */~
            tapeio%,        /* Tape I/O Count (extracted from system)  */~
            wsio%,          /* Workstation I/O Count (extracted from sy*/~
            crt%,           /* System Device Number                    */~
            date,           /* System date (second variable)           */~
            time,           /* System time second variable             */~
            elapse_time%,   /* Elapsed time since logon or 'run' from c*/~
            diskio%,        /* Disk I/O Count (extracted from system)  */~
            otherio%,       /* Other I/O Count (TC, etc extracted from */~
            prog_pagein%,   /* Program Page-In Count                   */~
            prog_pageout%,  /* Program Page-Out Count                  */~
            cpu%,           /* CPU Time Consumed since "run" from Comma*/~
            printio%,       /* Printer I/O Count (extracted from system*/~
            sys_pagein%,    /* System Page-In Count                    */~
            sys_page_out%,  /* System Page-Outs Count                  */~
            tapeio%,        /* Tape I/O Count (extracted from system)  */~
            wsio%,          /* Workstation I/O Count (extracted from sy*/~
            link_type$,     /* Link Type in LINK subroutine            */~
            -1%,            /* Completion Code from LINK SVC / USERSUB */~
            -1%,            /* Return Code from Program or Link SVC    */~
            inlib$,         /* User's database library                 */~
            " "             /* Filler For Rest of Record or Internal Sp*/

            write #1
L01710:     end

        logout
            read #1, hold, key = key(#1) , eod goto L01710
            gosub extract_info
            put #1 using L02850 ,                                          ~
            volume$,        /* Disk VOLUME name                        */~
            library$,       /* Library Identifier                      */~
            program$,       /* Program Name                            */~
            date,           /* System date (second variable)           */~
            time,           /* System time second variable             */~
            elapse_time%,   /* Elapsed time since logon or 'run' from c*/~
            diskio%,        /* Disk I/O Count (extracted from system)  */~
            otherio%,       /* Other I/O Count (TC, etc extracted from */~
            prog_pagein%,   /* Program Page-In Count                   */~
            prog_pageout%,  /* Program Page-Out Count                  */~
            cpu%,           /* CPU Time Consumed since "run" from Comma*/~
            printio%,       /* Printer I/O Count (extracted from system*/~
            sys_pagein%,    /* System Page-In Count                    */~
            sys_page_out%,  /* System Page-Outs Count                  */~
            tapeio%,        /* Tape I/O Count (extracted from system)  */~
            wsio%,          /* Workstation I/O Count (extracted from sy*/~
            link_type$,     /* Link Type in LINK subroutine            */~
            comp%,          /* Completion Code from LINK SVC / USERSUB */~
            return%,        /* Return Code from Program or Link SVC    */~
            inlib$,         /* User's database library                 */~
            " "             /* Filler For Rest of Record or Internal Sp*/

            rewrite #1
            end

        extract_info
            call "EXTRACT" addr(                                         ~
            "ID",                                                        ~
            userid$,        /* user-id of specific user                */~
            "IL",                                                        ~
            inlib$,         /* User's database library                 */~
            "E:",                                                        ~
            elapse_time%,   /* Elapsed time since logon or 'run' from c*/~
            "D@",                                                        ~
            diskio%,        /* Disk I/O Count (extracted from system)  */~
            "O@",                                                        ~
            otherio%,       /* Other I/O Count (TC, etc extracted from */~
            "P+",                                                        ~
            prog_pagein%,   /* Program Page-In Count                   */~
            "P-",                                                        ~
            prog_pageout%,  /* Program Page-Out Count                  */~
            "P:",                                                        ~
            cpu%,           /* CPU Time Consumed since "run" from Comma*/~
            "P@",                                                        ~
            printio%,       /* Printer I/O Count (extracted from system*/~
            "S+",                                                        ~
            sys_pagein%,    /* System Page-In Count                    */~
            "S-",                                                        ~
            sys_page_out%,  /* System Page-Outs Count                  */~
            "T@",                                                        ~
            tapeio%,        /* Tape I/O Count (extracted from system)  */~
            "W@",                                                        ~
            wsio%,          /* Workstation I/O Count (extracted from sy*/~
            "W#",                                                        ~
            crt%)           /* SYSTEM DEVICE NUMBER                    */

            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************


L02460: FMT                 /* FILE: CMSLOG                            */~
            CH(6),          /* Disk VOLUME name                        */~
            CH(8),          /* Library Identifier                      */~
            CH(8),          /* Program Name                            */~
            CH(3),          /* user-id of specific user                */~
            CH(6),          /* system (clock) date from the computer   */~
            CH(8),          /* Time from the system clock              */~
            BI(4),          /* Elapsed time since logon or 'run' from c*/~
            BI(4),          /* Disk I/O Count (extracted from system)  */~
            BI(4),          /* Other I/O Count (TC, etc extracted from */~
            BI(4),          /* Program Page-In Count                   */~
            BI(4),          /* Program Page-Out Count                  */~
            BI(4),          /* CPU Time Consumed since "run" from Comma*/~
            BI(4),          /* Printer I/O Count (extracted from system*/~
            BI(4),          /* System Page-In Count                    */~
            BI(4),          /* System Page-Outs Count                  */~
            BI(4),          /* Tape I/O Count (extracted from system)  */~
            BI(4),          /* Workstation I/O Count (extracted from sy*/~
            BI(1),          /* System Device Number                    */~
            CH(6),          /* System date (second variable)           */~
            CH(8),          /* System time second variable             */~
            BI(4),          /* Elapsed time since logon or 'run' from c*/~
            BI(4),          /* Disk I/O Count (extracted from system)  */~
            BI(4),          /* Other I/O Count (TC, etc extracted from */~
            BI(4),          /* Program Page-In Count                   */~
            BI(4),          /* Program Page-Out Count                  */~
            BI(4),          /* CPU Time Consumed since "run" from Comma*/~
            BI(4),          /* Printer I/O Count (extracted from system*/~
            BI(4),          /* System Page-In Count                    */~
            BI(4),          /* System Page-Outs Count                  */~
            BI(4),          /* Tape I/O Count (extracted from system)  */~
            BI(4),          /* Workstation I/O Count (extracted from sy*/~
            CH(1),          /* Link Type in LINK subroutine            */~
            BI(4),          /* Completion Code from LINK SVC / USERSUB */~
            BI(4),          /* Return Code from Program or Link SVC    */~
            CH(8),          /* Database Library (INLIB)                */~
            CH(0097)        /* Filler For Rest of Record or Internal Sp*/~


L02850: FMT                 /* FILE: CMSLOG   -  Logout                */~
            CH(6),          /* Disk VOLUME name                        */~
            CH(8),          /* Library Identifier                      */~
            CH(8),          /* Program Name                            */~
            POS(85),                                                     ~
            CH(6),          /* System date (second variable)           */~
            CH(8),          /* System time second variable             */~
            BI(4),          /* Elapsed time since logon or 'run' from c*/~
            BI(4),          /* Disk I/O Count (extracted from system)  */~
            BI(4),          /* Other I/O Count (TC, etc extracted from */~
            BI(4),          /* Program Page-In Count                   */~
            BI(4),          /* Program Page-Out Count                  */~
            BI(4),          /* CPU Time Consumed since "run" from Comma*/~
            BI(4),          /* Printer I/O Count (extracted from system*/~
            BI(4),          /* System Page-In Count                    */~
            BI(4),          /* System Page-Outs Count                  */~
            BI(4),          /* Tape I/O Count (extracted from system)  */~
            BI(4),          /* Workstation I/O Count (extracted from sy*/~
            CH(1),          /* Link Type in LINK subroutine            */~
            BI(4),          /* Completion Code from LINK SVC / USERSUB */~
            BI(4),          /* Return Code from Program or Link SVC    */~
            CH(8),          /* Database Library (INLIB)                */~
            CH(0097)        /* Filler For Rest of Record or Internal Sp*/~

