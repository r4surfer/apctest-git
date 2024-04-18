        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  DDDD   DDDD    GGG   EEEEE  TTTTT   OOO   PPPP   TTTTT   *~
            *  D   D  D   D  G      E        T    O   O  P   P    T     *~
            *  D   D  D   D  G GG   EEEE     T    O   O  PPPP     T     *~
            *  D   D  D   D  G   G  E        T    O   O  P        T     *~
            *  DDDD   DDDD    GGG   EEEEE    T     OOO   P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DDGETOPT - Subroutine which issues a GETPARM request      *~
            *            asking for options to Control SES behavior     *~
            *            in DCINPUT2 & DCINPUT3.  Values are returned to*~
            *            the calling program via the call arguments.    *~
            *            Editing/Verification is the responsibility of  *~
            *            the calling program.                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/27/87 ! Original                                 ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "DDGETOPT" addr(                                             ~
                       parent$,          /* Parent Structure name or ""*/~
                       parent_enabled$,  /* Parent    Field Enabled?   */~
                       version$,         /* Version # Field or Blank   */~
                       version_enabled$, /* Version # Field Enabled?   */~
                       exit_on_save$,    /* Exit on DATASAVE/STARTOVER?*/~
                       rename_enabled$,  /* Rename Function Enabled?   */~
                       edit_for_cda$,    /* CDA Edits in Force?  YES/NO*/~
                       advanced_enabled$)/* Advanced Functions Enabled?*/

        REM *************************************************************~
            *     L I N K A G E   S E C T I O N   V A R I A B L E S     *~
            *************************************************************
        dim advanced_enabled$3,          /* Advanced Functions Enabled?*/~
            edit_for_cda$3,              /* CDA Edits in Force?  YES/NO*/~
            exit_on_save$3,              /* Exit on DATASAVE/STARTOVER?*/~
            parent$16,                   /* Parent Structure name or ""*/~
            parent_enabled$3,            /* Parent    Field Enabled?   */~
            rename_enabled$3,            /* Rename Function Enabled?   */~
            version$6,                   /* Version # Field or Blank   */~
            version_enabled$3            /* Version # Field Enabled?   */

        REM *************************************************************~
            *        G E T P A R M   C A L L   V A R I A B L E S        *~
            *************************************************************
        dim getparm_type$2,              /* Either 'ID' or 'RD'        */~
            getparm_form$1,              /* GETPARM Form               */~
            errormsg$78,                 /* Error message              */~
            pfkey$1,                     /* AID byte receiver          */~
            message_text$(3)79,          /* Screen header text         */~
            spec_type$(10)1,             /* Defines keylist types      */~
            keyword$(10)8,               /* GETPARM Keywords           */~
            pfmask$4                     /* Masked PF key bits  (32)   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.01.00 07/01/88 General Release R5.01.00        "
        REM *************************************************************
            message_len% = 237%
            getparm_type$ = "ID"
            getparm_form$ = "R"
            pfkey$ = "@"
            pfmask$ = hex(fffd0000)
            init ("K")spec_type$()
            keyword$(1) = "PARENT  "
            keyword$(2) = "PARENOPT"
            keyword$(3) = "VERSION "
            keyword$(4) = "VERSOPT "
            keyword$(5) = "EXITOPT "
            keyword$(6) = "RENAMOPT"
            keyword$(7) = "ADVOPT  "
            keyword$(8) = "CDAOPT  "
            parent$, version$ = " "
            parent_enabled$, version_enabled$ = "YES"
            edit_for_cda$, exit_on_save$ = "NO"
            advanced_enabled$, rename_enabled$ = "YES"
            errormsg$ = " "
            message_text$(1%) =                                          ~
            "This screen is requesting you to supply the Update Options t~
        ~o allow in the"
            str(message_text$(1%),79%) = hex(0d)
            message_text$(2%) =                                          ~
            "SES Utility which you've just Run.                          ~
        ~            "
            str(message_text$(2%),79%) = hex(0d)
            message_text$(3%) =                                          ~
            "Enter the required information below and press RETURN or PF ~
        ~16 to Exit"

        REM *************************************************************~
            *     I N P U T   M O D E   M A I N   R O U T I N E         *~
            *                                                           *~
            *************************************************************

        inputmode

            call "GETPARM" addr(getparm_type$,     /* 'I' or 'R'       */~
                                getparm_form$,     /* 'R'equired       */~
                                "OPTIONS ",        /* Prname           */~
                                pfkey$,            /* PF Key Receiver  */~
                                "0001",            /* Message Id       */~
                                "GETOPT",          /* Message Issuer   */~
                                message_text$(),   /* Screen Header Txt*/~
                                message_len%,      /* Message Length   */~
                                spec_type$(1),     /* SES Parent       */~
                                keyword$(1),       /*   ""             */~
                                parent$,           /*   ""             */~
                                16%,               /*   "" Length      */~
                                "A",               /* Row Flag (Abs)   */~
                                12%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(2),     /* Parent  Enabled? */~
                                keyword$(2),       /*   ""             */~
                                parent_enabled$,   /*   ""             */~
                                03%,               /*   "" Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(3),     /* SES Version      */~
                                keyword$(3),       /*   ""             */~
                                version$,          /*   ""             */~
                                06%,               /*   "" Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(4),     /* Version Enabled? */~
                                keyword$(4),       /*   ""             */~
                                version_enabled$,  /*   ""             */~
                                03%,               /*   "" Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(5),     /* Exit On Save ?   */~
                                keyword$(5),       /*   ""             */~
                                exit_on_save$,     /*   ""             */~
                                03%,               /*   "" Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(6),     /* RENAME Allowed?  */~
                                keyword$(6),       /*   ""             */~
                                rename_enabled$,   /*   ""             */~
                                03%,               /*   "" Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(7),     /* Advanced Enabled?*/~
                                keyword$(7),       /*   ""             */~
                                advanced_enabled$, /*   ""             */~
                                03%,               /*   "" Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(8),     /* Edit for CDA?    */~
                                keyword$(8),       /*   ""             */~
                                edit_for_cda$,     /*   ""             */~
                                03%,               /*   "" Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                "T",               /* Regular Text     */~
                              "Enter the PARENT Structure Name or blank",~
                                40%,               /* Text Length      */~
                                "A",               /* Row Flag (Abs)   */~
                                12%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* REGULAR TEXT     */~
                                "May the Parent Name be Changed?       ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                              "Enter the Parent VERSION NUMBER or blank",~
                                40%,               /* Text Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* REGULAR TEXT     */~
                                "May the Version Number be Changed?    ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* REGULAR TEXT     */~
                            "Exit on Program on Data Save / Start Over?",~
                                42%,               /* Text Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* REGULAR TEXT     */~
                                "RENAME Function to be Enabled?        ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "Advanced Functions to be Enabled?     ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "Edit Restrictions for C.D.A. On?      ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "P",               /* Pf Key Mask Spec */~
                                pfmask$,           /* Pf Key Mask      */~
                                "E",               /* Enter Key Allowed*/~
                                "T",               /*Error Message Line*/~
                                errormsg$,         /* Error Message    */~
                                78%,               /* Text Length      */~
                                "A",               /* Row Flag         */~
                                24%,               /* Row              */~
                                "A",               /* Column Flag      */~
                                03%)               /* Column Number    */~

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            *************************************************************

            if pfkey$  = "P" then call "RETURN" addr(2%) /* PF 16      */
            if pfkey$ <> "@" then inputmode              /* PF 0       */
            getparm_type$ = "R"          /* In Case Errors Found Below */
            errormsg$ = " "
            init("K") spec_type$()
            if parent_enabled$="YES" or parent_enabled$="NO" then L12430
               errormsg$="Parent Allowed to Change must be YES or NO :"  ~
                       & parent_enabled$
               spec_type$(2%) = "R"
L12430:     if version_enabled$="YES" or version_enabled$="NO" then L12470
               errormsg$="Version Allowed to Change must be YES or NO :" ~
                       & version_enabled$
               spec_type$(4%) = "R"
L12470:     if exit_on_save$="YES" or exit_on_save$="NO" then L12510
               errormsg$="Exit on Save / Startover must be YES or NO :"  ~
                       & exit_on_save$
               spec_type$(5%) = "R"
L12510:     if rename_enabled$="YES" or rename_enabled$="NO" then L12540
               errormsg$="RENAME Function Enabled must be YES or NO :"   ~
                       & rename_enabled$
               spec_type$(6%) = "R"
L12540:     if advanced_enabled$="YES" or                                ~
               advanced_enabled$="NO" then L12570
               errormsg$="Advanced Functions Enabled must be YES or NO :"~
                       & advanced_enabled$
               spec_type$(7%) = "R"
L12570:     if edit_for_cda$="YES" or edit_for_cda$="NO" then L12600
               errormsg$="C.D.A. Edits  Enabled must be YES or NO :"     ~
                       & edit_for_cda$
               spec_type$(8%) = "R"
L12600:     if errormsg$ <> " " then errormsg$= hex(94) & errormsg$
            if errormsg$ <> " " then inputmode
            end
