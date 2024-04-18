        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  DDDD   DDDD    GGG   EEEEE  TTTTT  L      IIIII  BBBB    *~
            *  D   D  D   D  G      E        T    L        I    B   B   *~
            *  D   D  D   D  G GG   EEEE     T    L        I    BBBB    *~
            *  D   D  D   D  G   G  E        T    L        I    B   B   *~
            *  DDDD   DDDD    GGG   EEEEE    T    LLLLL  IIIII  BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DDGETLIB - Subroutine which issues a GETPARM request      *~
            *            asking for the default SES Library name.       *~
            *            The GETPARM generated is a "hidden" getparm    *~
            *            whose default values may only be changed by a  *~
            *            procedure.  The default Library name is blank. *~
            *            The Library Description Key Field if non-blank *~
            *            allows the programmer to tell the routine to   *~
            *            create the given SES Library if it does not    *~
            *            already exist.                                 *~
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

        sub "DDGETLIB" addr(                                             ~
                       library$,         /* SES Library Name           */~
                       library_enabled$, /* SES Library Enabled? YES/NO*/~
                       library_descr$,   /* SES Library Name           */~
                       #1)               /* INTDOC01 File              */

        REM *************************************************************~
            *     L I N K A G E   S E C T I O N   V A R I A B L E S     *~
            *************************************************************
        dim library$6,                   /* SES Library Name           */~
            library_descr$50,            /* SES Library Description    */~
            library_enabled$3            /* SES Library Enabled? YES/NO*/

        REM *************************************************************~
            *        G E T P A R M   C A L L   V A R I A B L E S        *~
            *************************************************************
        dim getparm_type$2,              /* Either 'ID' or 'RD'        */~
            getparm_form$1,              /* GETPARM Form               */~
            errormsg$78,                 /* Error message              */~
            pfkey$1,                     /* AID byte receiver          */~
            message_text$(3)79,          /* Screen header text         */~
            spec_type$(03)1,             /* Defines keylist types      */~
            keyword$(03)8,               /* GETPARM Keywords           */~
            pfmask$4                     /* Masked PF key bits  (32)   */

        REM *************************************************************~
            *             I N T D O C 0 1      V A R I A B L E S        *~
            *************************************************************
        dim record$(4)250,               /* INTDOC01 Record Area       */~
            userid$3                     /* Current User ID            */

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
            keyword$(1) = "SESLIB  "
            keyword$(2) = "ENABLED "
            keyword$(3) = "DESCRIPT"
            library$, library_descr$ = " "
            library_enabled$ = "YES"
            errormsg$ = " "
            message_text$(1%) =                                          ~
            "Please supply the S.E.S. Library Name to use in this SES rel~
        ~ated program. "
            str(message_text$(1%),79%) = hex(0d)
            message_text$(2%) =                                          ~
            "If the Description is supplied the Library will be created i~
        ~f neccessary."
            str(message_text$(2%),79%) = hex(0d)
            message_text$(3%) =                                          ~
            "Enter the required information below and press RETURN or PF1~
        ~6 to Exit."

        REM *************************************************************~
            *     I N P U T   M O D E   M A I N   R O U T I N E         *~
            *                                                           *~
            *************************************************************

        inputmode

            call "GETPARM" addr(getparm_type$,     /* 'I' or 'R'       */~
                                getparm_form$,     /* 'R'equired       */~
                                "LIBRARY ",        /* Prname           */~
                                pfkey$,            /* PF Key Receiver  */~
                                "0001",            /* Message Id       */~
                                "GETLIB",          /* Message Issuer   */~
                                message_text$(),   /* Screen Header Txt*/~
                                message_len%,      /* Message Length   */~
                                spec_type$(1),     /* SES Library      */~
                                keyword$(1),       /*   ""             */~
                                library$,          /*   ""             */~
                                06%,               /*   "" Length      */~
                                "A",               /* Row Flag (Abs)   */~
                                12%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(2),     /* Library Enabled? */~
                                keyword$(2),       /*   ""             */~
                                library_enabled$,  /*   ""             */~
                                03%,               /*   "" Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                spec_type$(3),     /* Library Descriptn*/~
                                keyword$(3),       /*   ""             */~
                                library_descr$,    /*   ""             */~
                                50%,               /*   "" Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                02%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                20%,               /* Column Number    */~
                                "C",               /* Field Type       */~
                                "T",               /* REGULAR TEXT     */~
                                "Enter the SES Library Name or blank   ",~
                                38%,               /* TEXT LENGTH      */~
                                "A",               /* ROW FLAG (ABS)   */~
                                12%,               /* ROW              */~
                                "A",               /* COLUMN FLAG (ABS)*/~
                                05%,               /* COLUMN NUMBER    */~
                                "T",               /* REGULAR TEXT     */~
                                "May the SES Library Name be Changed?  ",~
                                38%,               /* Text Length      */~
                                "R",               /* Row Flag (Abs)   */~
                                01%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                05%,               /* Column Number    */~
                                "T",               /* Regular Text     */~
                                "Enter SES Library Description or blank",~
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
            if library_enabled$="YES" or library_enabled$="NO" then L12390
               errormsg$="Library Allowed to Change must be YES or NO :" ~
                        & library_enabled$
               spec_type$(2%) = "R"
L12390:     if library$ = " " then L12600
            readkey$ = library$
            call "READ100" (#1, readkey$, f1%)
            if f1% = 0% then gosub check_for_create_library
L12600:     if errormsg$ <> " " then errormsg$= hex(94) & errormsg$
            if errormsg$ <> " " then inputmode
            end

        check_for_create_library
            call "GETUFBS1" addr(#1, f2%)
            if f2% = 0% then return
            if library_descr$ > " " then create_library
               errormsg$ = "Sorry, but SES Library " & library$ &        ~
                           " was not found!"
               return

        create_library
            call "EXTRACT" addr("ID", userid$)
            str(record$(),17%,24%) = "LIBRARY " & str(library$,,6%)
            str(record$(),41%,50%) = library_descr$
            str(record$(),882%,5%) = all(hex(00))
            str(record$(),937%,15%)= date & date & userid$
            put #1 using L12810, record$()
            write #1, eod goto L12800
L12800:     return
L12810:     FMT 4*CH(250)
