        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y  TTTTT  Y   Y  PPPP   EEEEE          *~
            *  H   H  NN  N  Y   Y    T    Y   Y  P   P  E              *~
            *  HHHHH  N N N   YYY     T     YYY   PPPP   EEEE           *~
            *  H   H  N  NN    Y      T      Y    P      E              *~
            *  H   H  N   N    Y      T      Y    P      EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYTYPE  - RETURNS LITERAL FOR PART TYPE.  SUBROUTINE     *~
            *            TO INSURE CONSISTANCY.                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/21/85 ! ORIGINAL                                 ! HES *~
            * 01/25/96 ! Look up GENCODES Descrs if available.    ! JDH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "HNYTYPE" (type%, typedescr$, paren%)

        dim typedescr$32,                /* TYPE DESCRIPTION           */~
            type$3,                      /* Part Type                  */~
            readkey$99                   /* Misc Read Key              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #20 ! GENCODES ! General Codes File                       *~
            *************************************************************

            select #20, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            if been_here_before% = 1% then L09000
                call "OPENCHCK" (#20, fs%, f2%, 0%, " ") : fs% = f2%
                been_here_before% = 1%

L09000
*        See if there are GENCODES for Inventory Types
            convert type% to type$, pic(00#)
            readkey$ = "PARTTYPE " & type$
            call "DESCRIBE" (#20, readkey$, typedescr$, paren%, f1%)
            if f1% = 1% then end

*        If nothing is there, we'll use the generic descriptions
            typedescr$ = "** Invalid **"
            if type% = 0   then typedescr$ = "Generic          "
            if type% > 0   then typedescr$ = "Nonplanned       "
            if type% > 199 then typedescr$ = "Purchased        "
            if type% > 489 then typedescr$ = "Purchased Tool   "
            if type% > 499 then typedescr$ = "Manufactured     "
            if type% > 789 then typedescr$ = "Manufactured Tool"
            if type% > 799 then typedescr$ = "Manufactured     "
            if type% > 999 then typedescr$ = "** Invalid **    "
            if paren% = 1% then call "PUTPAREN" (typedescr$)

        REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND DISPLAYS A       *~
            * MESSAGE (ONLY IN FOREGROUND) WHILE WE LINK TO THE MENU.   *~
            *************************************************************

            end
