        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  TTTTT   AAA    SSS   K   K  BBBB   K   K   GGG   DDDD    *~
            *    T    A   A  S      K  K   B   B  K  K   G      D   D   *~
            *    T    AAAAA   SSS   KKK    BBBB   KKK    G GGG  D   D   *~
            *    T    A   A      S  K  K   B   B  K  K   G   G  D   D   *~
            *    T    A   A   SSS   K   K  BBBB   K   K   GGG   DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TASKBKGD - Free-format driver for TASKUP.                 *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/30/94 ! Original                                 ! KAB *~
            *          ! GETPARM Cloned from CMSRUN               !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            dbs$1,                       /* One / Data Base? [System]  */~
            errs$1,                      /* What to do with errors     */~
            errabort$1,                  /* Abort if error return      */~
            lib$8,                       /* in Library                 */~
            more$1,                      /* More to Submit             */~
            one$1,                       /* Unique?                    */~
            parms$1,                     /* Parameters Passed?         */~
            pfn$1,                       /* Function                   */~
            pid$2,                       /* Background Task Id         */~
            prg$8,                       /* Program/Procedure          */~
            vol$6                        /* on Volume                  */

        REM *************************************************************~
            *        G E T P A R M   C A L L   V A R I A B L E S        *~
            *************************************************************
        dim getparm_type$2,              /* Either 'ID' or 'RD'        */~
            getparm_form$1,              /* Getparm Form               */~
            errormsg$80,                 /* Error Message              */~
            pfkey$1,                     /* Aid Byte Receiver          */~
            message_text$(2)79,          /* Screen Header Text         */~
            spec_type$(10)1,             /* Defines Keylist Types      */~
            keyword$(10)8,               /* Getparm Keywords           */~
            pfmask$4,                    /* Masked Pf Key Bits  (32)   */~
            prname$8,                    /* Prname - Input or Error    */~
                                                                         ~
            search%(1),                  /* Search Target              */~
                                                                         ~
            ret$10,                      /* Last Return Code           */~
            rethx$8                      /* Last Return Code           */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
        REM *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *************************************************************
        init_mode
            message_len% = 158%
            getparm_type$ = "I"
            getparm_form$ = "R"
            prname$ = "TSKBKG01"
            pfkey$ = "@"
            pfmask$ = hex(00010000)
            init ("K")spec_type$()

            keyword$( 1) = "TASKID  "
            keyword$( 2) = "FUNCTION"
            keyword$( 3) = "PROGRAM "
            keyword$( 4) = "LIBRARY "
            keyword$( 5) = "VOLUME  "
            keyword$( 6) = "UNIQUE  "
            keyword$( 7) = "DBSPEC  "
            keyword$( 8) = "ERRDISP "
            keyword$( 9) = "ERRABORT"
            keyword$(10) = "MORE    "

            message_text$(1) =                                           ~
            "***Caelus Management Systems   - Submit Task Utility -   Ver~
        ~sion: " & str(cms2v$,,8%) & "***"
            str(message_text$(1%),79%) = hex(0d)
            message_text$(2%) = hex(0d842020) &                          ~
            "Enter the required information below and press RETURN or PF ~
        ~16 to Exit"

            errormsg$ = " "

            pid$      = " "              /* Background Task Id         */
            pfn$      = "S"              /* Function                   */
            prg$      = " "              /* Program/Procedure          */
            lib$      = " "              /* in Library                 */
            vol$      = " "              /* on Volume                  */
            one$      = "N"              /* Unique?                    */
            dbs$      = "Y"              /* One / Data Base? [System]  */
            errs$     = "P"              /* What to do with errors     */
            errabort$ = "N"              /* Abort if error return      */

            if more$ = " " then                                          ~
            more$     = "N"              /* More to Submit             */

            parms$    = "N"              /* Parameters Passed? (Always)*/

        REM *************************************************************~
            *             Request File to RUN Via GETPARM               *~
            *************************************************************
        input_filename

            call "GETPARM" addr(getparm_type$,     /* 'I' or 'R'       */~
                                getparm_form$,     /* 'R'equired       */~
                                prname$,           /* Prname           */~
                                pfkey$,            /* Pf Key Receiver  */~
                                "0001",            /* Message Id       */~
                                "TSKBKD",          /* Message Issuer   */~
                                message_text$(),   /* Screen Header Txt*/~
                                message_len%,      /* Message Length   */~
                                                   /* Keyowrds & Rcvrs */~
                                spec_type$(1),     /* Task Id          */~
                                keyword$(1),       /*   ""             */~
                                pid$,              /*   ""             */~
                                02%,               /* Field Length     */~
                                "A",               /* Row Flag (Abs)   */~
                                11%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(2),     /* Function         */~
                                keyword$(2),       /*   ""             */~
                                pfn$,              /*   ""             */~
                                01%,               /* Field Length     */~
                                "R",               /* Row Flag (Rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(3),     /* Run Proc/Prog    */~
                                keyword$(3),       /*   ""             */~
                                prg$,              /*   ""             */~
                                08%,               /* Field Length     */~
                                "R",               /* Row Flag (Rel)   */~
                                02%,               /* Row              */~
                                "A",               /* Column Flag (Rel)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(4),     /* in Library       */~
                                keyword$(4),       /*   ""             */~
                                lib$,              /*   ""             */~
                                08%,               /* Field Length     */~
                                "R",               /* Row Flag (rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(5),     /* On Volume        */~
                                keyword$(5),       /*   ""             */~
                                vol$,              /*   ""             */~
                                06%,               /* Field Length     */~
                                "R",               /* Row Flag (rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(6),     /* Unique           */~
                                keyword$(6),       /*   ""             */~
                                one$,              /*   ""             */~
                                01%,               /* Field Length     */~
                                "R",               /* Row Flag (rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(7),     /* DB Specific      */~
                                keyword$(7),       /*   ""             */~
                                dbs$,              /*   ""             */~
                                01%,               /* Field Length     */~
                                "R",               /* Row Flag (rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(8),     /* Error Disposition*/~
                                keyword$(8),       /*   ""             */~
                                errs$,             /*   ""             */~
                                01%,               /* Field Length     */~
                                "R",               /* Row Flag (rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(9),     /* Error Abort?     */~
                                keyword$(9),       /*   ""             */~
                                errabort$,         /*   ""             */~
                                01%,               /* Field Length     */~
                                "R",               /* Row Flag (rel)   */~
                                02%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(10),    /* More to Submit?  */~
                                keyword$(10),      /*   ""             */~
                                more$,             /*   ""             */~
                                01%,               /* Field Length     */~
                                "R",               /* Row Flag (rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                                   /* Text Begins      */~
                                "T",               /* Regular Text     */~
                                "Enter the TASK ID for PROGRAM/PROC.   ",~
                                38%,               /* Text Length      */~
                                "A",               /* Row Flag (Abs)   */~
                                11%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "Specify the Background Control Funtion",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "If 'ZZ', Enter the PROGRAM or PROC.   ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                02%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "   Optionally Specify The RUN LIBRARY ",~
                                29%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "                  And The RUN VOLUME  ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "   Must this Function be Unique?      ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "      If so, for Data Base only?      ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "Error Disposition [Show, None, Prompt]",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "Abort Sequence If Error Occurs?       ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                02%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "Are there More tasks to Submit?       ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                                   /* And the Footer   */~
                                "T",               /* Regular Text     */~
                                ret$,              /* Text             */~
                                10%,               /* Text Length      */~
                                "A",               /* Row Flag (Abs)   */~
                                11%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                70%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                rethx$,            /* Text             */~
                                08%,               /* Text Length      */~
                                "R",               /* Row Flag (Rel)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                72%,               /* Column Number    */~
                                                                         ~
                                "P",               /* Pf Key Mask Spec */~
                                pfmask$,           /* Pf Key Mask      */~
                                "E",               /* Enter Key Allowed*/~
                                "T",               /*Error Message Line*/~
                                str(errormsg$,,78),/* Error Message    */~
                                78%,               /* Text Length      */~
                                "A",               /* Row Flag         */~
                                24%,               /* Row              */~
                                "A",               /* Column Flag      */~
                                03%)               /* Column Number    */~

        REM *************************************************************~
            *                 Edit the Entered Responses                *~
            *************************************************************

            if pfkey$  = "P" then end                   /* PF 16      */
            if pfkey$ <> "@" then input_filename  /* Enter Key Pressed */
            getparm_type$ = "R"          /* In Case Errors Found Below */
            errormsg$ = " "
            init ("K")spec_type$()

*       ** PID$                         /* Background Task Id         */~
            search "MEJ1J2POSOCDTKZZ" = str(pid$) to search%() step 2
            if search%(1%) <> 0% then L12170
               spec_type$(1) = "R"
               errormsg$ = "Enter a valid Process ID"
               errormsg$ = errormsg$ & " (ME, J1, J2, PO, SO, CD, TK, ZZ)"
               goto L12210
L12170:     if search%(1%) <> 1% then L12210
               spec_type$(1) = "R"
               errormsg$ = "This task Cannot be Submitted"

L12210
*       ** PFN$                         /* Function                   */~
            if pos("SCARPDN" = pfn$) <> 0% then L12280
               spec_type$(2) = "R"
               if errormsg$ <> " " then L12280
               errormsg$ = "Enter a valid function code"
               errormsg$ = errormsg$ & " (S, C, A, R, P, D or N)"

L12280:     if pid$ = "ZZ" then L12320
               init (" ") prg$, lib$, vol$, one$, dbs$, errs$
               goto L12610

L12320
*       ** PRG$                         /* Program/Procedure          */~
            if prg$ <> " " then L12380
               spec_type$(3) = "R"
               if errormsg$ <> " " then L12380
               errormsg$ = "Program or Procedure must be entered"

L12380
*       ** LIB$                         /* in Library                 */~

*       ** VOL$                         /* on Volume                  */~

*       ** ONE$                         /* Unique?                    */~
            if pos("YN" = one$) <> 0% then L12480
               spec_type$(6) = "R"
               if errormsg$ <> " " then L12480
               errormsg$ = "Enter Y if multiple copies not allowed (or N)"

L12480
*       ** DBS$                         /* One / Data Base? [System]  */~
            if pos("YN" = dbs$) <> 0% then L12540
               spec_type$(7) = "R"
               if errormsg$ <> " " then L12540
               errormsg$ = "Enter N if only one per system (or Y)"

L12540
*       ** ERRS$                        /* What to do with errors     */~
            if pos("PNS" = errs$) <> 0% then L12610
               spec_type$(8) = "R"
               if errormsg$ <> " " then L12610
               errormsg$ = "Enter 'P' - Prompt, 'S' - Display only,"
               errormsg$ = errormsg$ & " or 'N' for no action"

L12610
*       ** ERRABORT$                    /* Abort if error return      */~
            if pos("YN" = errabort$) <> 0% then L12680
               spec_type$(9) = "R"
               if errormsg$ <> " " then L12680
               errormsg$ = "ENTER 'Y' - Teminate submissions on error"
               errormsg$ = errormsg$ & " or 'N' to continue if more"

L12680
*       ** MORE$                        /* More to Submit             */~
            if pos("YN" = more$) <> 0% then error_exit
               spec_type$(10) = "R"
               if errormsg$ <> " " then error_exit
               errormsg$ = "ENTER 'Y' - If more submissions"
               errormsg$ = errormsg$ & " or 'N' to terminate"

        error_exit
            if errormsg$ = " " then call_taskup
               errormsg$ = hex(b4) & errormsg$
               goto input_filename

        REM *************************************************************~
            * Do the job                                                *~
            *************************************************************

        call_taskup

        REM Set Return Code
            on pos("SCARPDN" = pfn$) goto L14100, L14110, L14120, L14130,    ~
                                          L14140, L14150, L14160
               end 32%
L14100:     ret% =    0% : goto L14180
L14110:     ret% = 9999% : goto L14180
L14120:     ret% = 9998% : goto L14180
L14130:     ret% = 9997% : goto L14180
L14140:     ret% = 8000% : goto L14180
L14150:     ret% = 8001% : goto L14180
L14160:     ret% = 8999% : goto L14180

L14180:     if errs$ = "P" then errs$ = "Y"

            call "TASKUP" (pid$,     /* Passed In only, not modified.  */~
                                     /* Possible values / meanings are:*/~
                                     /* CD = Verify & Submit CDATOCMS  */~
                                     /* J1 = Verify & Submit JBPOST1   */~
                                     /* J2 = Verify & Submit JBPOST2   */~
                                     /* ME = Submit the Calling Program*/~
                                     /* PO = Verify & Submit VBKUPDTE  */~
                                     /* SO = Verify & Submit BCKUPDTE  */~
                                     /* TK = Verify & Submit CDAVSCOM  */~
                                     /* ZZ = Submit the Passed In      */~
                                     /*      Program below, Verifying  */~
                                     /*      to see if already running */~
                                     /*      only if so specified.     */~
                          ret%,      /* IN:  8000 = Set Port Id in     */~
                                     /*             Data base          */~
                                     /*      8001 = Try to Destroy Port*/~
                                     /*             & Tempport (UNIX). */~
                                     /*      8999 = Submit, but don't  */~
                                     /*             wait for confirm.  */~
                                     /*      9997 = Constuct Message   */~
                                     /*             Port for INLIB     */~
                                     /*      9998 = Send ABORT Message */~
                                     /*             to Existing Task.  */~
                                     /*      9999 = Send CANCEL Message*/~
                                     /*             to Existing Task.  */~
                                     /* OUT:   99 = SYSFILE2 not found.*/~
                                     /*        90 = SUBMIT to          */~
                                     /*             Background failed. */~
                                     /*        80 = Submitted task     */~
                                     /*             failed to respond  */~
                                     /*             (send ITM) within  */~
                                     /*             the given tim-out  */~
                                     /*             period.            */~
                                     /*        75 = Unable to SCRATCH  */~
                                     /*             existing PROC or   */~
                                     /*             no WORKLIB avial.  */~
                                     /*        70 = Unable to FIND     */~
                                     /*             the specified      */~
                                     /*             Program to Submit. */~
                                     /*  PORTID%    IF 9997% in, port  */~
                                     /*             ID out. use BIN(,4)*/~
                                     /*             in calling program */~
                                     /*             change back PORT$  */~
                                     /*         0 = Either Task was    */~
                                     /*             Successfully       */~
                                     /*             Submitted -or- is  */~
                                     /*             already running    */~
                                     /*             -or- was signaled  */~
                                     /*             to ABORT or CANCEL.*/~
                          prg$,      /* Name of Program/Proc to Submit.*/~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be both     */~
                                     /* present and non-blank.         */~
                          lib$,      /* Name of Program/Proc Library.  */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                                     /* If present but blank will      */~
                                     /* search for program lib and vol.*/~
                          vol$,      /* Name of Program/Proc Volume.   */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                                     /* If present but blank will      */~
                                     /* search for program lib and vol.*/~
                          dbs$,      /* Is the Program Specific to a   */~
                                     /* Caelus Database (INLIB)? Y/N.  */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                          one$,      /* Must the Program be Unique?    */~
                                     /* Y/N. If Database Specific = Y  */~
                                     /* then only one copy per database*/~
                                     /* may be running.  If Database   */~
                                     /* Specific = N then only one copy*/~
                                     /* can be running at any given    */~
                                     /* time on the entire system.     */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                          errs$,     /* Show Error messages on the     */~
                                     /* Screen? Y/blank = Yes - ASKUSER*/~
                                     /*         S       = Yes - SHOSTAT*/~
                                     /*         N       = NO           */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                          parms$)    /* Create PUTPARM(s) to Satisfy   */~
                                     /* the submitted programs GETPARMS*/~
                                     /* Y or N.                        */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
        /*                PRNAME$(), /* Name of Program GETPARM(s) to  */~
                                     /* be satisfied or PUTPARM'd.     */~
                                     /* Ignored unless ID$ = ZZ and    */~
                                     /* PUTPARM$ = "Y" in which case   */~
                                     /* it is required and must be     */~
                                     /* present.                       */~
                                     /* Number of array elements may be*/~
                                     /* 1 thru 'N' but the element     */~
                                     /* length must be 8.              */~
        /*                PRFIELD$(),/* Name of Program GETPARM FIELDS */~
                                     /* be satisfied or PUTPARM'd.     */~
                                     /* Ignored unless ID$ = ZZ and    */~
                                     /* PUTPARM$ = "Y" in which case   */~
                                     /* it is required and must be     */~
                                     /* present.                       */~
                                     /* This MUST be a 2 dimensional   */~
                                     /* array whose rows correspond to */~
                                     /* the PRNAME$() above and columns*/~
                                     /* the fields for each PRNAME.    */~
                                     /* Contains the GETPARM FIELD name*/~
                                     /* to be satisfied using the      */~
                                     /* PRVALUES$() below.             */~
                                     /* Number of rows must be >= the  */~
                                     /* number of elements in PRNAME$()*/~
                                     /* and columns >= the max number  */~
                                     /* of fields to supply to a PRNAME*/~
                                     /* but the individual element     */~
                                     /* length must be 8.              */~
        /*                PRVALUE$())/* Values for GETPARM FIELDS      */~
                                     /* be satisfied or PUTPARM'd.     */~
                                     /* Ignored unless ID$ = ZZ and    */~
                                     /* PUTPARM$ = "Y" in which case   */~
                                     /* it is required and must be     */~
                                     /* present.                       */~
                                     /* This MUST be a 2 dimensional   */~
                                     /* array of the same dimensions   */~
                                     /* as PRFIELD$() above except that*/~
                                     /* the individual element length  */~
                                     /* must be only as large as the   */~
                                     /* largest field value to putparm.*/

            str(ret$,,4%) = bin(ret%,4)
            hexunpack str(ret$,,4%) to str(rethx$,,8%)
            convert ret% to ret$, pic(##########)

            if ret% = 0% then L15410
            if errabort$ = "Y" then end ret%
L15410:     if more$    <> "Y" then end ret%
               goto init_mode

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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

