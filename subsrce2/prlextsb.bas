        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP   RRRR   L      EEEEE  X   X  TTTTT   SSS   BBBB    *~
            *  P   P  R   R  L      E       X X     T    S      B   B   *~
            *  PPPP   RRRR   L      EEEE     X      T     SSS   BBBB    *~
            *  P      R   R  L      E       X X     T        S  B   B   *~
            *  P      R   R  LLLLL  EEEEE  X   X    T     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLEXTSB - THIS SUBROUTINE WILL CHECK TO SEE IF THE CMS   *~
            *            PAYROLL MODULE IS INSTALLED AND THEN CHECK TO  *~
            *            SEE IF THE CALLING PROGRAM SHOULD CONTINUE TO  *~
            *            RUN OR SHOULD BE TERMINATED.                   *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/02/92 ! ORIGINAL                                 ! JBK *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "PRLEXTSB" (mod$, ret%)

*        MOD$    = Module calling to check switch (PRL, SFC, or "   ")
*
*        RET%    = 0  - OK, calling program can continue to process
*                  1  - MOD$ was blank, CMS Payroll is installed
*                  2  - MOD$ was blank, CMS Payroll is not installed
*                  99 - Confict, Calling program should not continue

        dim                                                              ~
            askhd$40,                    /* Header for ASKUSER         */~
            askmsg$(3)80,                /* Message line for ASKUSER   */~
            mod$3,                       /* Module of calling program  */~
            prl_installed$1,             /* Switch for payroll install */~
            sysfile2key$20               /* SYSFILE2 read      key     */


        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            call "OPENCHCK" (#1, 0%, f2%(1),   0%, " ")
            if f2%(1) = 0% then L10000
                ret%  = 1%
                goto L65000



L10000: REM *************************************************************~
            * I N T I A L I Z A T I O N                                 *~
            * --------------------------------------------------------- *~
            * Load switches and test calling arguments.                 *~
            *************************************************************

            if mod$ = "PRL" or mod$ = "SFC" or mod$ = " " then L10100
                ret% = 0%
                goto L65000

L10100:     prl_installed$ = "N"  :  ret% = 99%  /* Expect the worst   */

            gosub load_switchs           /* F1%(1) indicates status    */

            if mod$ <> " " then L10190
                if prl_installed$ = "Y" then ret% = 1%   /* PRL Here   */
                if prl_installed$ = "N" then ret% = 2%   /* No PRL     */
                goto L65000

L10190:     if mod$ = "SFC"  then L10600

        REM "Payroll test
             if prl_installed$ = "N" then L10400          /* No PRL     */
                ret% = 0%                /* We have PRL, everything OK */
                goto L65000

L10400: REM "Payroll caller, but no PRL installed
            askhd$ = "NO CMS PERSONNEL/PAYROLL INSTALLED"
            askmsg$(1) = "The CMS Personnel/Payroll System is not "  &   ~
                         "Installed."
            askmsg$(2) = "This Personnel/Payroll Program CANNOT be "  &  ~
                         "Run Until it is Installed."
            goto display_message

L10600: REM "SFC test
             if prl_installed$ = "Y" then L10800         /* PRL is Here */
                ret% = 0%                /* SFC and no PRL, all is OK  */
                goto L65000

L10800: REM "SFC caller, but PRL is installed
            askhd$ = "CMS PERSONNEL/PAYROLL INSTALLED"
            askmsg$(1) = "The CMS Personnel/Payroll System is "  &       ~
                         "Installed."
            askmsg$(2) = "This SFC Program CANNOT be Run While CMS " &   ~
                         "Personnel/Payroll is Installed."
            goto display_message

        display_message
            askmsg$(3) = "- Press Any Key to Continue -"
            keyhit% = 0%
            call "ASKUSER" (keyhit%, askhd$, askmsg$(1), askmsg$(2),     ~
                            askmsg$(3))
            goto L65000


        REM *************************************************************~
            * L O A D   S W I T C H E S                                 *~
            * --------------------------------------------------------- *~
            * Move switches from disc to file buffer area               *~
            *************************************************************
        load_switchs
            sysfile2key$ = "SWITCHS.PRL"
            call "READ100" (#1, sysfile2key$, f1%(1))
                if f1%(1) = 0% then return

            get #1 using L30120, prl_installed$
L30120:         FMT XX(20), CH(1)

            if prl_installed$ <> "Y" then prl_installed$ = "N"
            return

L65000: REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL


            if f2%(1%) = 0 then close #1

            end
