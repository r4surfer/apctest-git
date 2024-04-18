        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII  PPPP   RRRR   M   M   *~
            *  H   H  NN  N  Y   Y  P   P    I    P   P  R   R  MM MM   *~
            *  HHHHH  N N N   YYY   PPPP     I    PPPP   RRRR   M M M   *~
            *  H   H  N  NN    Y    P        I    P      R   R  M   M   *~
            *  H   H  N   N    Y    P      IIIII  P      R   R  M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPIPRM - Maintain Physical Inventory Session Parameters.*~
            *            This program acts as a Stand-Alone Driver to   *~
            *            subroutine HNYPIDEF which sets switches        *~
            *            controlling the behavior of the ticket print,  *~
            *            data entry, & variance posting programs.       *~
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
            * 01/08/86 ! Original                                 ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            description$30,              /* Session Notes / Name       */~
            glvar$1,                     /* Update G/L Variance flag   */~
            hnyvar$1,                    /* Update HNY Variance flag   */~
            part_req$1,                  /* Part Numbers Required flag */~
            print$1,                     /* Print Sheets or Tickets flg*/~
            session_date$8,              /* Planned Count Date         */~
            session_nbr$2                /* Count Session Number       */~


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYPISYS ! Physical Inventory System Session Contro *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.16.01 04/04/86 Physical inventory and miscel  "
        REM *************************************************************
            select #1,  "HNYPISYS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  512,                                  ~
                        keypos =    7, keylen =   2,                     ~
                        alt key  1, keypos =    1, keylen =   8


        enter_parameters

        call "HNYPIDEF" (#1,             /* HNYPISYS File              */~
                        session_nbr$,    /* Count Session Number       */~
                        session_date$,   /* Planned count date         */~
                        description$,    /* Session notes/name         */~
                        part_req$,       /* Part numbers required flag */~
                        print$,          /* Print sheets or tickets ?  */~
                        hnyvar$,         /* Update QTY-ON-HAND Var Flag*/~
                        glvar$,          /* Update G/L Variance Flag   */~
                        keyhit%)         /* PF Key passed back;        */~
                                         /*    0 = save data           */~
                                         /*    1 = start over          */~
                                         /*   16 = exit, no save       */

            if keyhit% =  1% then startover
            if keyhit% = 32% then L65000
            if keyhit% <>16% then enter_parameters
            goto datasave

        REM *************************************************************~
            *                     S T A R T   O V E R                   *~
            *-----------------------------------------------------------*~
            *  Allow user opportunity to cancel & reenter (start over). *~
            *************************************************************
        startover
            call "STARTOVR" (keyhit%)
            if keyhit% = 1% then enter_parameters
            init(" ") session_nbr$, session_date$, description$,         ~
                      part_req$, print$, hnyvar$, glvar$
            goto enter_parameters


        REM *************************************************************~
            *                      D A T A S A V E                      *~
            *-----------------------------------------------------------*~
            *  Replace the Session Parameter Switches for the Session.  *~
            *************************************************************
        datasave
            call "SHOWMSG" ("Saving Parameters for Count Session " &     ~
                             session_nbr$)
            call "READ101" (#1, session_nbr$, f1%)
            if f1% = 0% then call "SHOWMSG" ("PROGRAM/SYSTEM ERROR!")
            if f1% = 0% then L65000
            call "DATUNFMT" (session_date$)
            put #1 using L19110, session_date$, description$, part_req$,  ~
                                print$, hnyvar$, glvar$
L19110:     FMT CH(6), POS(9), CH(30), POS(343), 4*CH(1)
            rewrite #1

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
